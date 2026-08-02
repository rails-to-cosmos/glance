-- | The server, driven as a WAI 'Application'.  No socket is bound: every case
-- here is a request handed straight to the app, so the suite stays free of
-- ports and of the races that come with them.  The websocket route is the one
-- thing an upgrade-less request cannot reach, and the frames it would carry
-- are TestStore's subject.
module TestServe (spec) where

import Control.Monad (filterM, (<=<))
import Data.Aeson ( FromJSON, Value (Bool, Null, Number, Object, String)
                  , eitherDecode, encode, object, parseJSON, (.=) )
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.Char (isDigit)
import Data.List (find, isInfixOf, nub, sort, sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Types ( HeaderName, RequestHeaders, methodDelete, methodPost
                          , renderQuery, statusCode )
import Network.Wai (Application, defaultRequest, requestHeaders, requestMethod)
import Network.Wai.Test ( SRequest (SRequest)
                        , SResponse (simpleBody, simpleHeaders, simpleStatus)
                        , request, runSession, setPath, srequest )
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults ( boolAt, document, field, intAt, listAt, maybeTextAt, membersAt
                    , orgFile, textAt, textsAt, viewDir, withTempDir )

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Data.Org.Edit (snapDigest, snapshotOf)
import Glance.Query ( QueryResult (qrRecords), builtinFilter, loadDir, loadFile
                    , viewJSON )
import Glance.Web ( ServeOptions (..), application, bannerLines, bootstrapWanted
                  , defaultPort, viewTitleFor )
import Glance.Web.Store ( Hub, applyFile, finishLoading, loadStore, newHub
                       , newLoadingHub, publish )

-- Fixtures
--
-- 'viewDir' is the directory TestQuery loads: one sample document and one that
-- is not UTF-8, six headlines between them.

-- | The document those six headlines come from.
sampleFile :: FilePath
sampleFile = viewDir <> "/sample.org"

-- | @sha256sum test\/fixtures\/view\/sample.org@.  Written down rather than
-- computed here: the digest the server hands out is what a client pins its
-- edit to, and an oracle that runs the same code as the server proves nothing
-- about it.
sampleDigest :: T.Text
sampleDigest = "0de46a0cceb1b1b30364c0bba0107e63bbd2c9b504d1e2bf31f29321f1ff2493"

-- | A file whose first headline is materialized, edited and written back.  The
-- id is in the drawer, so it stays the same across the temp directory's name
-- and across the edit.
committable :: T.Text
committable = T.unlines
  [ "#+CATEGORY: notes"
  , "* TODO First :one:"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: first"
  , ":END:"
  , "body of first"
  , "* TODO Second"
  , "tail"
  ]

-- | An assets directory holding a stub renderer.
assetsDir :: FilePath
assetsDir = "test/fixtures/assets"

-- | A path with no renderer under it, and no directory either.
missingAssetsDir :: FilePath
missingAssetsDir = "test/fixtures/assets-not-here"

-- | The renderer the binary compiles in, as it sits in the tree.  The suite
-- runs from the package root, so this is the same path @Glance.Web@'s splice
-- read at build time.
vendoredRenderer :: FilePath
vendoredRenderer = "assets/table-view.js"

served :: FilePath -> ServeOptions
served assets = builtIn { soAssets = Just assets }

-- | What a plain @glance serve@ runs: no @--assets@, so every asset is the
-- binary's own.
builtIn :: ServeOptions
builtIn = ServeOptions { soDir = viewDir, soPort = defaultPort, soAssets = Nothing
                       , soDerived = False }

-- | The app a server with ASSETS runs, over a store loaded the way 'serve'
-- loads one.  A fresh store per request is the suite's convenience; the server
-- keeps one for its lifetime.
app :: FilePath -> IO Application
app assets = appOf (served assets)

-- | The app OPTS runs, over that same store.
appOf :: ServeOptions -> IO Application
appOf opts = application opts <$> (newHub =<< loadStore viewDir)

-- | A server over DIR: the app, and the hub whose store it answers from — the
-- write cases look at that store afterwards to show the route left it alone.
serverOver :: FilePath -> IO (Application, Hub)
serverOver dir = do
  hub <- newHub =<< loadStore dir
  pure (application (served assetsDir) { soDir = dir } hub, hub)

-- | GET PATH from a server configured with ASSETS.
get :: FilePath -> ByteString -> IO SResponse
get assets = getOf (served assets)

-- | GET PATH from a server started without @--assets@.
getBuiltIn :: ByteString -> IO SResponse
getBuiltIn = getOf builtIn

-- | GET PATH from a server running OPTS.
getOf :: ServeOptions -> ByteString -> IO SResponse
getOf opts path = do
  application' <- appOf opts
  getFrom application' path

-- | GET PATH from APPLICATION'.
getFrom :: Application -> ByteString -> IO SResponse
getFrom application' path = getWith application' path []

-- | GET PATH from APPLICATION', sending HEADERS — the conditional and the
-- content-negotiation cases are all one request header apart.
getWith :: Application -> ByteString -> RequestHeaders -> IO SResponse
getWith application' path headers =
  runSession (request (setPath defaultRequest path) { requestHeaders = headers }) application'

-- | POST PAYLOAD to PATH on APPLICATION', as JSON.
postTo :: Application -> ByteString -> BL.ByteString -> IO SResponse
postTo application' path payload = runSession (srequest (SRequest req payload)) application'
  where req = (setPath defaultRequest path)
                { requestMethod  = methodPost
                , requestHeaders = [("Content-Type", "application/json")] }

-- | @\/headline?id=…@ with ID percent-encoded, the way a client builds it: a
-- row id is @FILE#K@ and carries both the slashes a path segment would fight
-- over and the hash a raw URL would read as a fragment.
headlinePath :: T.Text -> ByteString
headlinePath rid = "/headline" <> renderQuery True [("id", Just (TE.encodeUtf8 rid))]

-- | A commit body: the subtree text and the digest it was materialized with.
commitBody :: T.Text -> T.Text -> BL.ByteString
commitBody org digest = encode (object ["org" .= org, "digest" .= digest])

-- | A commit body in the other shape: the parts a lens client edits, and the
-- same digest.  The server composes them back into one subtree, putting its own
-- regions — the hidden properties and the logbook — back beside them.
splitBody :: T.Text -> [[T.Text]] -> T.Text -> BL.ByteString
splitBody body props = planningBody body props []

-- | 'splitBody', also naming the planning entries.
planningBody :: T.Text -> [[T.Text]] -> [[T.Text]] -> T.Text -> BL.ByteString
planningBody body props plan digest = encode (object
  [ "body" .= body, "properties" .= props, "planning" .= plan, "digest" .= digest ])

-- | Run K over a server holding 'committable' with its first headline already
-- materialized: the app, the file on disk, and the materialize response a
-- commit to it has to present back.  Six cases opened with those five lines.
withCommitted :: (Application -> FilePath -> Value -> Assertion) -> Assertion
withCommitted k = withTempDir $ \dir -> do
  path <- orgFile dir "notes.org" committable
  (a, _hub) <- serverOver dir
  v <- getFrom a (headlinePath "first") >>= decoded
  k a path v

-- Assertions

status :: SResponse -> Int
status = statusCode . simpleStatus

header :: HeaderName -> SResponse -> Maybe ByteString
header name r = lookup name (simpleHeaders r)

-- | The @ETag@ R carries.
etagOf :: SResponse -> IO ByteString
etagOf r = maybe (assertFailure "no ETag on the response") pure (header "ETag" r)

-- | WHAT: is TAG the entity tag of a store at generation GEN — a quoted
-- @\<fingerprint\>-g\<n\>@, the fingerprint being sixteen hex digits of the
-- loaded tree's digest?  Written out here rather than taken from the server,
-- since an oracle that formats the tag the way the server formats it agrees
-- with whatever the server does.
assertTreeTag :: String -> Int -> ByteString -> Assertion
assertTreeTag what gen tag = do
  assertBool (what <> ": no tree fingerprint in " <> show tag)
             (BSC.length fingerprint == 16
                && BSC.all (`elem` ("0123456789abcdef" :: String)) fingerprint)
  assertEqual (what <> ": generation") ("-g" <> BSC.pack (show gen) <> "\"") rest
  where (fingerprint, rest) = BSC.splitAt 16 (BSC.drop 1 tag)

-- | TAG with its generation half replaced by N: what the same tree would carry
-- N updates in.
atGeneration :: Int -> ByteString -> ByteString
atGeneration n tag = BSC.takeWhile (/= '-') tag <> "-g" <> BSC.pack (show n) <> "\""

-- | A fingerprint no tree has: the tag half that stands for another daemon's
-- store.
zeroes :: ByteString
zeroes = BSC.replicate 16 '0'

body :: SResponse -> T.Text
body = TE.decodeUtf8 . BL.toStrict . simpleBody

assertContains :: String -> T.Text -> T.Text -> Assertion
assertContains what needle haystack =
  assertBool (what <> ": no " <> show needle <> " in " <> show (T.take 400 haystack))
             (needle `T.isInfixOf` haystack)

-- | WHAT: every one of NEEDLES is in HAYSTACK.
holdsAll :: String -> [T.Text] -> T.Text -> Assertion
holdsAll what needles haystack = mapM_ (\n -> assertContains what n haystack) needles

-- | WHAT: none of NEEDLES is in HAYSTACK.  Each of them names a design the page
-- superseded, so one coming back means two are live at once.
holdsNone :: String -> [T.Text] -> T.Text -> Assertion
holdsNone what needles haystack =
  mapM_ (\n -> assertBool (what <> ": " <> show n <> " survives in the page")
                          (not (n `T.isInfixOf` haystack)))
        needles

-- | R's body as a JSON 'Value', or the decode error as a test failure.
decoded :: SResponse -> IO Value
decoded r = either (\e -> assertFailure ("response JSON: " <> e)) pure
                   (eitherDecode (simpleBody r))

-- | R's @rows@ array.
rowsOf :: SResponse -> IO [Value]
rowsOf r = listAt "rows" =<< decoded r

-- | KEY of V, decoded into whatever the case wants back.
decodedAt :: FromJSON a => T.Text -> Value -> IO a
decodedAt key v = do
  raw <- field key v
  either (\e -> assertFailure (T.unpack key <> ": " <> e)) pure (parseEither parseJSON raw)

-- | KEY of V as pairs of strings — the shape @\/headline@ carries a drawer in.
pairsAt :: T.Text -> Value -> IO [[T.Text]]
pairsAt = decodedAt

-- | V's own field names.  An absent field is an answer here rather than a
-- failure — @sort@ is the one the document order leaves out.
fieldsOf :: Value -> IO [T.Text]
fieldsOf (Object o) = pure (map Key.toText (KM.keys o))
fieldsOf v = assertFailure ("expected an object, got " <> show v)

-- | ROW's @id@, or the whole row when it has none — a failure that reads.
rowId :: Value -> T.Text
rowId row = case row of
  Object o -> case KM.lookup "id" o of
    Just (String i) -> i
    _noId           -> T.pack (show row)
  _notARow -> T.pack (show row)

-- | ROW's @scheduled@ cell, empty when it has none.  The key the view declares
-- its sort on, so a page has to come out of this order.
scheduledOf :: Value -> T.Text
scheduledOf row = case row of
  Object o -> case KM.lookup "cells" o of
    Just (Object cells) -> case KM.lookup "scheduled" cells of
      Just (String s) -> s
      _unscheduled    -> ""
    _noCells -> ""
  _notARow -> ""

-- | The state column's badge values, in palette order.
badgeValues :: Value -> IO [T.Text]
badgeValues view = do
  cols <- listAt "columns" view
  state <- maybe (assertFailure "no state column") pure
                 (find (keyIs "state") cols)
  traverse (textAt "value") =<< listAt "badges" state
  where keyIs k (Object o) = KM.lookup "key" o == Just (String k)
        keyIs _ _notAColumn = False

-- | What sits between OPEN and CLOSE in HAYSTACK, when both are in it.
between :: T.Text -> T.Text -> T.Text -> Maybe T.Text
between open close haystack
  | T.null after = Nothing
  | T.null rest  = Nothing
  | otherwise    = Just inner
  where (_before, after) = T.breakOn open haystack
        (inner, rest)    = T.breakOn close (T.drop (T.length open) after)

-- | The SHA-256 of the bytes PATH holds now.  Taken off the file rather than
-- through a load: the digest a receipt reports is what a client pins its next
-- edit to, and an oracle that re-runs the loader agrees with it whatever the
-- loader digested.
digestOnDisk :: FilePath -> IO T.Text
digestOnDisk path = snapDigest . snapshotOf path <$> document path

-- Spec
--
-- One shell is rendered for the whole group and handed to every case that reads
-- it ('withResource'): the page is a pure function of the options and the
-- store, and thirty-odd cases were each loading the fixture directory again to
-- get the same string back.

spec :: TestTree
spec = withResource (body <$> get assetsDir "/") (const (pure ())) $ \shell ->
  testGroup "Serve"
    [ headlineSpec, bannerSpec, statsSpec, cacheSpec, gzipSpec, querySpec
    , orderSpec, archiveViewSpec
    , bootstrapSpec, materializeSpec, commitSpec, commandSpec, planningSpec, captureSpec
    , configSpec, keywordsSpec, linksSpec, indexingSpec
    , pageSpec shell, keymapSpec shell
    , glueSpec shell, bootSpec shell, liveSpec shell, paletteSpec shell
    , moveSpec shell, markSpec shell
    , commandKeySpec shell, promptKeySpec shell, whichKeySpec shell
    , openKeySpec shell, agendaSpec shell, logSpec shell
    , sheetSpec shell
    , settingsSpec shell
    , touchSpec shell
    , shellFontSpec shell, assetSpec, embeddedSpec, errorSpec ]

-- | One boot of the shell's glue, run: the address bar it opens on, what the
-- server answers as @X-Glance-Total@, the @\/headlines@ URLs that have to
-- follow in order, and the search string the page settles the URL on.
--
-- Reading the glue as text cannot answer this: a call that is written and never
-- reached matches a string search exactly as well as one that runs.  The boot
-- is where that matters most — which query the page opens on, and whether the
-- parity baseline is ever fetched under a filtered one.
data Boot = Boot
  { boLabel  :: String
  , boSearch :: T.Text
  , boTotal  :: Int
  , boKeys   :: T.Text   -- ^ keys pressed over the table once the boot settled.
  , boAsked  :: [T.Text]
  , boUrl    :: T.Text
  }

shellBoots :: [Boot]
shellBoots =
  [ Boot "a bare boot opens on the active view and arms the check"
      "" 500 ""
      -- The default, a page of it; the rest of that answer behind the paint;
      -- and the unfiltered set the parity check needs, which no filtered paint
      -- can supply.
      [ "/headlines?q=state%3A*active*&limit=100"
      , "/headlines?q=state%3A*active*"
      , "/headlines" ]
      "?q=state%3A*active*"

  , Boot "with the whole answer on the first page there is no second fetch"
      "" 1 ""
      [ "/headlines?q=state%3A*active*&limit=100", "/headlines" ]
      "?q=state%3A*active*"

  , Boot "a deep link is asked for as it stands, and arms the check too"
      "?q=tanik&keys=vim" 500 ""
      [ "/headlines?q=tanik&limit=100", "/headlines?q=tanik", "/headlines" ]
      -- Nothing is written over a URL the reader wrote: `keys' included.
      "?q=tanik&keys=vim"

  , Boot "an empty q is a reader asking for everything, and no default lands on it"
      "?q=" 500 ""
      -- Unfiltered from the first fetch, so the paint is its own baseline and
      -- there is nothing to arm.
      [ "/headlines?limit=100", "/headlines" ]
      "?q="

  -- DEL is the applied query's own backspace, and the default is subject to it
  -- like any other token: one press and the whole store is on screen.
  , Boot "DEL over the table strips the default and shows everything"
      "" 500 "Backspace"
      [ "/headlines?q=state%3A*active*&limit=100"
      , "/headlines?q=state%3A*active*"
      , "/headlines"
      , "/headlines" ]
      -- `remember("")' writes `q' PRESENT and empty, which is what tells the
      -- next boot that a reader cleared this rather than never filtered it.
      -- Taking the parameter out instead is what re-injected the default.
      "?q="

  , Boot "and strips a deep link the same way, leaving the rest of the URL"
      "?q=tanik&keys=vim" 500 "Backspace"
      [ "/headlines?q=tanik&limit=100", "/headlines?q=tanik", "/headlines"
      , "/headlines" ]
      "?q=&keys=vim"

  , Boot "with no query to strip it asks for nothing"
      "?q=" 500 "Backspace"
      [ "/headlines?limit=100", "/headlines" ]
      "?q="

  -- `g' applies the tree's own default view, and applies it the way every other
  -- query is applied: into the URL, then asked of the server, then mounted as
  -- the renderer's chips.  It goes through the mount because the chips are the
  -- renderer's and only a mount can be handed a query it did not commit itself.
  , Boot "g applies the tree's default view over a cleared one"
      "?q=" 500 "g"
      [ "/headlines?limit=100", "/headlines"
      -- The boot's two, then the remount's — which is a boot in its own right
      -- and reads the URL `g' has just written.  It arms nothing: the parity
      -- baseline was fetched by the boot and a remount does not throw it away.
      , "/headlines?q=state%3A*active*&limit=100"
      , "/headlines?q=state%3A*active*" ]
      "?q=state%3A*active*"

  -- On a page already showing it, `g' is the same round trip rather than a
  -- no-op: it is a remount, and the URL it lands on is the one it wrote.
  , Boot "and re-applies it over a deep link that narrowed past it"
      "?q=tanik" 500 "g"
      [ "/headlines?q=tanik&limit=100", "/headlines?q=tanik", "/headlines"
      , "/headlines?q=state%3A*active*&limit=100"
      , "/headlines?q=state%3A*active*" ]
      "?q=state%3A*active*"
  ]

-- | The boots above, run where the machine has a node to run them.
bootSpec :: IO T.Text -> TestTree
bootSpec shell = testGroup "Shell boot"
  [ testCase boLabel $ bootOf shell boSearch boTotal boKeys "" $ \answer -> do
      assertEqual (boLabel <> ": the fetches") boAsked =<< textsAt "asked" answer
      assertEqual (boLabel <> ": the URL it settles on") boUrl =<< textAt "url" answer
  | Boot{..} <- shellBoots ]

-- | What happens to a booted page when the socket goes, and what it still holds
-- afterwards.  The distinction the whole group is about is 'lvMounts': a
-- reconnect that rebuilds the mount is the page reloading under the reader,
-- which is what an editor writing a whole tree used to look like from here.
--
-- 'lvActs' is the harness's own script — a close with the reason the server
-- would send, a keystroke into the sheet, a store that moved underneath.
data Live = Live
  { lvLabel  :: String
  , lvSearch :: T.Text
  , lvKeys   :: T.Text
  , lvActs   :: T.Text
  , lvAsked  :: [T.Text]   -- ^ every @\/headlines@ URL, in order.
  , lvTags   :: [T.Text]   -- ^ the @If-None-Match@ values sent with them.
  , lvMounts :: Int        -- ^ how many times the table was mounted; the boot is one.
  , lvSheet  :: T.Text     -- ^ what the sheet holds at the end.
  , lvState  :: T.Text     -- ^ the word the sheet's header carries.
  , lvUrl    :: T.Text
  }

-- | The boot's three fetches, which every case here starts with.
booted :: [T.Text]
booted = [ "/headlines?q=state%3A*active*&limit=100"
         , "/headlines?q=state%3A*active*"
         , "/headlines" ]

-- | The applied query, asked for again — what a reconnect costs when it costs
-- anything.
reasked :: T.Text
reasked = "/headlines?q=state%3A*active*"

shellLives :: [Live]
shellLives =
  [ -- The storm case, and the reason this group exists.  The server abandons a
    -- backlog it cannot deliver and closes with `resync'; the page revalidates,
    -- is told 304, re-attaches, and never touches its mount.
    Live "a dropped backlog costs one revalidation and keeps the mount"
      "" "" "close:resync"
      (booted <> [reasked]) ["\"t0\""] 1 "" "" "?q=state%3A*active*"

    -- The store moved while the socket was down: the same one fetch, and its
    -- rows go into the table standing there.
  , Live "a store that moved refreshes the rows under the same mount"
      "" "" "moved close:resync"
      (booted <> [reasked]) ["\"t0\""] 1 "" "" "?q=state%3A*active*"

    -- The one thing rows cannot carry.  No `view-changed' was sent here — this
    -- is the daemon-restart shape, where the columns moved with no socket open
    -- to say so — and the reconnect finds it by comparing what it fetched.
  , Live "columns that moved rebuild the mount, close reason or none"
      "" "" "recolumn close:resync"
      (booted <> [reasked] <> take 2 booted) ["\"t0\""] 2 "" "" "?q=state%3A*active*"

    -- The killing case: a `view-changed' close mid-edit.  The mount goes, and
    -- the text the reader had not saved comes back with it.
  , Live "view-changed mid-edit rebuilds the mount and keeps the sheet's text"
      "" "Enter" "sheet:hello close:view-changed"
      (booted <> take 2 booted) [] 2 "hello" "synced" "?q=state%3A*active*"

    -- And when the file moved under the open sheet, the restore says so rather
    -- than flushing over it later: the text stands, at `conflict'.
  , Live "a sheet restored over a moved file lands in the conflict flow"
      "" "Enter" "sheet:hello rewritten close:view-changed"
      (booted <> take 2 booted) [] 2 "hello" "conflict" "?q=state%3A*active*"

    -- A cleared filter is a `?q=' in the URL and nothing re-injects the default
    -- over it — which is what the reader saw as the filter resetting itself.
  , Live "a cleared filter stays cleared through a reconnect"
      "" "Backspace" "close:resync"
      (booted <> ["/headlines", "/headlines"]) ["\"t0\""] 1 "" "" "?q="
  ]

-- | The cases above, run where the machine has a node to run them.
liveSpec :: IO T.Text -> TestTree
liveSpec shell = testGroup "Shell reconnect"
  [ testCase lvLabel $ bootOf shell lvSearch 500 lvKeys lvActs $ \answer -> do
      assertEqual (lvLabel <> ": the fetches") lvAsked =<< textsAt "asked" answer
      assertEqual (lvLabel <> ": the tags it revalidated with")
                  lvTags =<< textsAt "tags" answer
      assertEqual (lvLabel <> ": how many times the table was mounted")
                  lvMounts =<< intAt "mounts" answer
      assertEqual (lvLabel <> ": what the sheet holds") lvSheet =<< textAt "sheet" answer
      assertEqual (lvLabel <> ": where the sheet stands") lvState =<< textAt "state" answer
      assertEqual (lvLabel <> ": the URL") lvUrl =<< textAt "url" answer
  | Live{..} <- shellLives ]

-- | A half-typed palette outlives a remount too, and comes back raised.  Its
-- own case: the palette's lifecycle is the renderer's and what this page can
-- see of it is the field and the one call that raises it, so both are what get
-- asserted.  @\/@ raises it here, the same key a reader presses.
paletteSpec :: IO T.Text -> TestTree
paletteSpec shell = testGroup "Shell palette"
  [ testCase "a half-typed palette is raised again after a remount" $
      bootOf shell "" 500 "/" "filter:tan close:view-changed" $ \answer -> do
        assertEqual "mounted twice" 2 =<< intAt "mounts" answer
        -- Once for the key, once for the restore: the shell has no second way
        -- into the palette and does not grow one here.
        assertEqual "raised again" 2 =<< intAt "raises" answer
        assertEqual "with what was typed in it" "tan" =<< textAt "palette" answer
  ]

-- | The buffer-end keys, driven through the presses a reader makes.  Reading
-- the glue cannot answer this: the whole change is what the SECOND press does,
-- and both presses run the same line of source.  The store is nine rows over
-- three pages, which is the smallest set with a page in front, a page behind
-- and an end to stop at.
--
-- 'moveScript' opens every case, so a script reads as the presses alone.
moveSpec :: IO T.Text -> TestTree
moveSpec shell = testGroup "Shell movement"
  [ -- Off the end, `<' is the within-page jump it always was.
    testCase "< takes the page's first row" $
      bootOf shell "" 500 "" (moveScript "press:n press:n press:<") $ \answer -> do
        assertEqual "the row" "r1" =<< textAt "selected" answer
        assertEqual "the page it stayed on" 1 =<< intAt "page" answer
        assertEqual "the echo" "< → first-row" =<< textAt "echo" answer

    -- On it, the same key climbs — and lands on the FIRST row of the page it
    -- turned to, where the renderer's own turn lands on the last.
  , testCase "< on the first row turns back a page and lands on its first row" $
      bootOf shell "" 500 "" (moveScript "press:] press:] press:<") $ \answer -> do
        assertEqual "the row" "r4" =<< textAt "selected" answer
        assertEqual "the page" 2 =<< intAt "page" answer
        assertEqual "the echo names it" "< → first-row (page 2/3)"
          =<< textAt "echo" answer

    -- The chain, to the top and then nowhere: page three's first row, page
    -- two's, page one's, and a fourth press that moves nothing.
  , testCase "and stops on page one's first row" $
      bootOf shell "" 500 "" (moveScript "press:] press:] press:< press:< press:<") $
        \answer -> do
          assertEqual "the row" "r1" =<< textAt "selected" answer
          assertEqual "the page" 1 =<< intAt "page" answer
          -- A stop is the plain echo: nothing moved, so no page is named.
          assertEqual "the echo" "< → first-row" =<< textAt "echo" answer

  , testCase "> takes the page's last row" $
      bootOf shell "" 500 "" (moveScript "press:>") $ \answer -> do
        assertEqual "the row" "r3" =<< textAt "selected" answer
        assertEqual "the page it stayed on" 1 =<< intAt "page" answer
        assertEqual "the echo" "> → last-row" =<< textAt "echo" answer

    -- The asymmetric half: `nextPage' lands on the new page's FIRST row, so
    -- without the follow-up select this answers `r4'.
  , testCase "> on the last row turns a page and lands on its last row" $
      bootOf shell "" 500 "" (moveScript "press:> press:>") $ \answer -> do
        assertEqual "the row" "r6" =<< textAt "selected" answer
        assertEqual "the page" 2 =<< intAt "page" answer
        assertEqual "the echo names it" "> → last-row (page 2/3)"
          =<< textAt "echo" answer

    -- vi's spelling of the same command, walked to the bottom and held there.
  , testCase "G is that key, and the last page's last row is the end of it" $
      bootOf shell "" 500 "" (moveScript "press:G press:G press:G press:G") $
        \answer -> do
          assertEqual "the row" "r9" =<< textAt "selected" answer
          assertEqual "the page" 3 =<< intAt "page" answer
          assertEqual "the echo" "G → last-row" =<< textAt "echo" answer

    -- The column is the renderer's across a turn, and this page hands it back
    -- rather than keeping one: `f' picks column 0 and it survives the climb.
  , testCase "a climb keeps the column the cursor was in" $
      bootOf shell "" 500 "" (moveScript "press:f press:> press:>") $ \answer -> do
        assertEqual "the row" "r6" =<< textAt "selected" answer
        assertEqual "the column" 0 =<< intAt "col" answer

    -- An asset with no pager keeps the half it can do, and says it the same
    -- way: a key that cannot climb still reports the row it took.
  , testCase "an asset without a pager keeps the within-page jump" $
      bootOf shell "" 500 "" (moveScript "press:] pageless press:< press:<") $
        \answer -> do
          assertEqual "the row" "r4" =<< textAt "selected" answer
          assertEqual "the page it could not leave" 2 =<< intAt "page" answer
          assertEqual "the echo" "< → first-row" =<< textAt "echo" answer

    -- With no popup open the table holds the keys, and the corner's chrome is
    -- not a popup.  A `select' that kept the focus would take `n' as its own
    -- type-ahead, and the reader would have to click the table back — so the
    -- press after the switch is the whole assertion.
  , testCase "the corner's theme select gives the keys back to the table" $
      bootOf shell "" 500 "" "theme:dark press:n" $ \answer -> do
        assertEqual "nothing holds the keyboard" "" =<< textAt "holding" answer
        assertEqual "and the key moved the cursor" "r2" =<< textAt "selected" answer
  ]

-- | Nine rows over three pages, then SCRIPT.  Every case here needs a set with
-- pages in it, and the harness's three rows are one page whatever the size.
moveScript :: T.Text -> T.Text
moveScript script = "rows:9 paged:3 " <> script

-- | Marking, driven through the keys a reader presses.  The renderer holds the
-- marks and this page holds the keys, so what is asserted here is the half that
-- is the page's: that @m@ walks as it marks, that @u@ is not a toggle, that the
-- count comes back out of the renderer, and that a table-view.js without the
-- calls is told about rather than crashed into.
markSpec :: IO T.Text -> TestTree
markSpec shell = testGroup "Shell marks"
  [ testCase "the mount asks for them" $
      bootOf shell "" 500 "" "" $
        assertEqual "marks:true reached the renderer" True <=< boolAt "marksOn"

    -- The flag's own hint, drawn by the renderer over the row wearing one: the
    -- keys are this page's, so the wording is too.
  , testCase "and names the keys a flagged row answers to" $
      bootOf shell "" 500 "" "" $
        assertEqual "flagHelp reached the renderer" "d/D archive · u unflag"
          <=< textAt "flagHelp"

    -- The renderer's per-row hint says RET materializes, which the resident key
    -- line under the table already says — and says for every command rather
    -- than for the one.  One place, so the mount turns the other off.
  , testCase "and asks for no per-row hints, the key line saying it once" $
      bootOf shell "" 500 "" "" $
        assertEqual "actionHints:false reached the renderer" False <=< boolAt "hintsOn"

    -- Dired's walk: two presses mark two rows rather than one row twice, and
    -- the count in the echo is the renderer's own.
  , testCase "m marks the row it is on and steps to the next" $
      bootOf shell "" 500 "m m" "" $ \answer -> do
        assertEqual "the rows it marked" ["r1", "r2"] =<< textsAt "marked" answer
        assertEqual "and where it left the cursor" 2 =<< intAt "cursor" answer
        assertEqual "counting as it went" "m → mark-toggle (marked · 2)" =<< textAt "echo" answer

    -- The same key on the same row takes it back off, which is what makes it a
    -- toggle: `m' twice over one row leaves nothing, since the second press is
    -- on the row the first one stepped to.
  , testCase "m on a marked row unmarks it" $
      bootOf shell "" 500 "m" "press:ArrowUp press:m" $ \answer -> do
        assertEqual "nothing marked" [] =<< textsAt "marked" answer
        assertEqual "and it says so" "m → mark-toggle (unmarked · 0)" =<< textAt "echo" answer

    -- `u' only ever takes a mark off.  After `m' the cursor is on an unmarked
    -- row, so a toggle would mark it and the count would read 2.
  , testCase "u never marks a row, it only unmarks one" $
      bootOf shell "" 500 "m u" "" $ \answer -> do
        assertEqual "the first mark stands alone" ["r1"] =<< textsAt "marked" answer
        assertEqual "and the count did not grow" "u → unmark (unmarked · 1)" =<< textAt "echo" answer

  , testCase "U clears every mark at once" $
      bootOf shell "" 500 "m m U" "" $ \answer -> do
        assertEqual "nothing left" [] =<< textsAt "marked" answer
        assertEqual "the echo" "U → unmark-all (all marks and flags cleared)" =<< textAt "echo" answer

    -- `M' is the renderer's call because the SET is the renderer's: a page it is
    -- not showing is marked too, which is the whole reason a shell-side loop
    -- over the visible rows would be the wrong answer.
  , testCase "M marks every row loaded, not the page on show" $
      bootOf shell "" 500 "M" "" $ \answer -> do
        assertEqual "all three" ["r1", "r2", "r3"] =<< textsAt "marked" answer
        assertEqual "counted by the renderer" "M → mark-all (marked · 3)" =<< textAt "echo" answer
        assertEqual "and the cursor stayed where it was" 0 =<< intAt "cursor" answer

    -- dired's flag, in two presses: the first marks the row for archiving and
    -- the second is the confirmation.  One press writes nothing at all.
  , testCase "d flags the row, and a second d archives it" $ do
      bootOf shell "" 500 "d" "" $ \answer -> do
        assertEqual "the row is flagged" ["r1"] =<< textsAt "flagged" answer
        assertEqual "and nothing was written" [] =<< postedOf answer
        assertEqual "the pill says what the next press costs"
                    "d → archive-flag (flagged — d again archives)" =<< textAt "echo" answer
        -- The two sets are the renderer's own and stay apart: flagging a row
        -- leaves the marked set exactly where it was.
        assertEqual "and no mark went on with it" [] =<< textsAt "marked" answer
      -- One flag is a set of one, so the single-row flow is the general one and
      -- reads as it: the second press is `D', and `D' names the set it ran over.
      bootOf shell "" 500 "d d" "" $ \answer -> do
        assertEqual "one flag is a set of one, so the second press takes it"
                    [("archive", ["r1"])] =<< postedOf answer
        assertEqual "and the flag is spent" [] =<< textsAt "flagged" answer
        assertEqual "counted" "d → archive-flag (archived · 1 flagged)" =<< textAt "echo" answer

    -- The flag stays on the ROW rather than following the cursor, so a walk
    -- between the two presses is a walk back before the second one lands.
  , testCase "d on one row and d on another flags both and archives neither" $
      bootOf shell "" 500 "d n d" "" $ \answer -> do
        assertEqual "two rows flagged" ["r1", "r2"] =<< textsAt "flagged" answer
        assertEqual "and nothing written" [] =<< postedOf answer

    -- dired's `dd': the second press is `D', so it takes the WHOLE flagged set
    -- rather than the row under it.  `d n d n d' flags r1, r2 and r3 and leaves
    -- the cursor on r3; the press after that archives all three at once.
  , testCase "the second d archives every flagged row, not just the one under it" $
      bootOf shell "" 500 "d n d n d" "press:d" $ \answer -> do
        assertEqual "all three, in one request"
                    [("archive", ["r1", "r2", "r3"])] =<< postedOf answer
        assertEqual "and no flag is left" [] =<< textsAt "flagged" answer
        assertEqual "named the way D names it" "d → archive-flag (archived · 3 flagged)"
          =<< textAt "echo" answer

    -- The same set, the same request, the same pill: `D' is `d' without the
    -- flagging press in front of it, and there is one implementation.
  , testCase "D on that same set does exactly what the second d does" $
      bootOf shell "" 500 "d n d n d" "press:D" $ \answer -> do
        assertEqual "the same three" [("archive", ["r1", "r2", "r3"])]
          =<< postedOf answer
        assertEqual "the same pill, under its own key" "D → org-glance-overview:delete (archived · 3 flagged)"
          =<< textAt "echo" answer

    -- `d' is in ONCE, and this is why: a HELD key reaching the handler twice
    -- would flag a row and archive it from one press, which is exactly the
    -- confirmation the two-press shape exists to be.
  , testCase "a held d flags and stops there" $
      bootOf shell "" 500 "d" "repeat:d repeat:d repeat:d" $ \answer -> do
        assertEqual "still just flagged" ["r1"] =<< textsAt "flagged" answer
        assertEqual "and the burst wrote nothing" [] =<< postedOf answer

    -- `u' takes the flag off first: it is the more recent thing a reader put on
    -- the row, and the one that would otherwise write a file.
  , testCase "u clears an archive flag before it touches a mark" $ do
      bootOf shell "" 500 "d" "press:ArrowUp press:u" $ \answer -> do
        assertEqual "the flag is off" [] =<< textsAt "flagged" answer
        assertEqual "and it says which" "u → unmark (flag cleared)" =<< textAt "echo" answer
      -- `m' marks r1 and steps; `d' flags r2 where it landed.  `u' on r2 takes
      -- the flag, and `u' back on r1 takes the mark — one key, flag first.
      bootOf shell "" 500 "m d" "press:u press:ArrowUp press:ArrowUp press:u" $ \answer -> do
        assertEqual "the flag went" [] =<< textsAt "flagged" answer
        assertEqual "and the mark after it" [] =<< textsAt "marked" answer

  , testCase "U clears the flags along with the marks" $
      bootOf shell "" 500 "m d" "press:U" $ \answer -> do
        assertEqual "no marks" [] =<< textsAt "marked" answer
        assertEqual "and no flags" [] =<< textsAt "flagged" answer

    -- An asset predating the flag calls is named rather than crashed into, the
    -- same way the mark calls are: the write must never be the fallback.
  , testCase "a table-view.js without the flag calls is named, not crashed into" $
      bootOf shell "" 500 "" "bare press:d" $ \answer -> do
        assertEqual "nothing was written" [] =<< postedOf answer
        assertEqual "and it said why"
                    "d → archive-flag (this table-view.js has no archive flags)"
                    =<< textAt "echo" answer

    -- An asset predating the calls: the key says what is missing rather than
    -- throwing, the same way the pager and the token strip do.  A throw would
    -- fail the harness outright, so what this pins is the wording — and that
    -- `m' left the cursor alone, since a key that cannot do its job must not
    -- half-do it.
  , testCase "a table-view.js without the calls is named, not crashed into" $
      bootOf shell "" 500 "" "bare press:m press:U" $ \answer -> do
        assertEqual "and it did not walk on regardless" 0 =<< intAt "cursor" answer
        assertEqual "the last key said why"
                    "U → unmark-all (this table-view.js has no marks)"
                    =<< textAt "echo" answer
  ]

-- | The two structured commands, driven through the keys a reader presses.
-- What is asserted is this page's half: which rows a command names — the
-- FLAGGED set for archiving and the MARKED one for a state, each falling back
-- to the row at point — what the value palette
-- offers and commits, and what the pill says when the server refuses.  The
-- edits themselves are @TestQuery@'s subject and the route is
-- @POST \/command@'s; nothing here re-states either.
commandKeySpec :: IO T.Text -> TestTree
commandKeySpec shell = testGroup "Shell commands"
  [ testCase "D with nothing flagged archives the row at point" $
      bootOf shell "" 500 "D" "" $ \answer -> do
        assertEqual "one archive, over the selected row"
                    [("archive", ["r1"])] =<< postedOf answer
        assertEqual "and the pill says which" "D → org-glance-overview:delete (archived · row)"
          =<< textAt "echo" answer

    -- The FLAGGED set is what `D' runs over. A flag is a selection made for
    -- archiving; a mark is the generic bulk selection a reader lays down to set
    -- a state over a run of rows, and letting the archive key inherit one would
    -- make every mark a loaded gun.
  , testCase "D archives the flagged set, and leaves the marks where they are" $
      -- `m m' marks r1 and r2 and steps to r3; `d' flags r3.
      bootOf shell "" 500 "m m d" "press:D" $ \answer -> do
        assertEqual "the flagged row, and only it"
                    [("archive", ["r3"])] =<< postedOf answer
        assertEqual "named as the set it was" "D → org-glance-overview:delete (archived · 1 flagged)"
          =<< textAt "echo" answer
        assertEqual "the marks are untouched" ["r1", "r2"] =<< textsAt "marked" answer

    -- The flags are spent, the way a second `d' spends the one it fires over.
    -- They have to be: the renderer keeps a flag whose row a filter is hiding,
    -- so a set left standing would be archived again by the next press and the
    -- row at point would never be reachable again.
  , testCase "D spends the flags it fired over, and the next D is the point row" $
      bootOf shell "" 500 "d" "press:D press:D" $ \answer -> do
        assertEqual "the flagged row, then the row under the cursor"
                    [("archive", ["r1"]), ("archive", ["r1"])] =<< postedOf answer
        assertEqual "nothing flagged is left" [] =<< textsAt "flagged" answer
        assertEqual "and the second press said so" "D → org-glance-overview:delete (archived · row)"
          =<< textAt "echo" answer

  , testCase "and with marks but no flags it is still the row at point" $
      bootOf shell "" 500 "m m D" "" $ \answer -> do
        assertEqual "the row under the cursor, never the marked pair"
                    [("archive", ["r3"])] =<< postedOf answer
        assertEqual "said as the point row" "D → org-glance-overview:delete (archived · row)"
          =<< textAt "echo" answer
        assertEqual "and the marks stand" ["r1", "r2"] =<< textsAt "marked" answer

    -- The other half of that split, unchanged: `set-state' is the command that
    -- DOES read the marked set, so the two selections stay apart on both sides.
  , testCase "set-state still runs over the marked set" $
      bootOf shell "" 500 "m m d" "press:C-c press:C-t press:t" $ \answer -> do
        assertEqual "the marked pair, and not the flagged row"
                    [("set-state", ["r1", "r2"])] =<< postedOf answer
        assertEqual "and the flag is still on, unspent" ["r3"]
          =<< textsAt "flagged" answer

  , testCase "a server that refuses is counted out and logged" $
      bootOf shell "" 500 "" "refuse press:D" $ \answer -> do
        assertEqual "the command still went" 1 . length =<< postedOf answer
        -- The set name gives way to the bare count: "row" over zero rows would
        -- read as a write that landed.
        assertEqual "nothing landed" "D → org-glance-overview:delete (archived · 0)" =<< textAt "echo" answer

    -- C-c C-t is a chord, so this also exercises the prefix path: the first key
    -- opens it and the second completes it, over a table with no field focused.
    -- The letter is the whole gesture: the palette IS the confirmation, so
    -- there is no RET behind it.
  , testCase "C-c C-t raises the palette and a letter commits on its own" $
      bootOf shell "" 500 "C-c C-t" "press:t" $ \answer -> do
        assertEqual "the palette said what it was setting and over how many"
                    "set state · 1 row" =<< textAt "phead" answer
        assertEqual "one command, over the row at point"
                    [("set-state", ["r1"])] =<< postedOf answer
        assertEqual "as the keyword that letter names" [Just "TODO"]
          =<< keywordsOf answer
        assertEqual "the pill names the state" "C-c C-t → org-glance-overview:todo (TODO · 1)"
          =<< textAt "echo" answer
        assertEqual "and the overlay is down" "" =<< textAt "prompt" answer

    -- The reserved-chord rule, and the half no other case can see: `C-t' is in
    -- RESERVED, so a press that opened nothing would be left to the browser.
    -- Completing a bound sequence outranks that, and what says so is the
    -- dispatch claiming BOTH chords.  This is the page's whole guarantee about
    -- the sequence: a browser that owns `Ctrl+T' above the document (Chromium
    -- does) never delivers the second press, and nothing here can reach that.
  , testCase "the completing chord is claimed, reserved or not" $
      bootOf shell "" 500 "C-c C-t" "" $ \answer -> do
        assertEqual "the palette is up" "on" =<< textAt "prompt" answer
        assertEqual "neither chord was left to the browser"
                    ["C-c", "C-t"] =<< textsAt "prevented" answer

    -- RET is nobody's here: it commits in the fallback mode alone, and a reader
    -- who pressed it out of habit gets the palette still standing rather than a
    -- write they did not name.
  , testCase "RET commits nothing in letter mode" $
      bootOf shell "" 500 "C-c C-t" "press:Enter" $ \answer -> do
        assertEqual "no command went" [] =<< postedOf answer
        assertEqual "and the palette is still up" "on" =<< textAt "prompt" answer

    -- `t' raises the palette AND is a letter inside it, and this listener sits
    -- BEHIND the dispatch — so the one press that opened the overlay arrives in
    -- it next.  Two presses, two jobs.
  , testCase "the press that raises the palette is not a key in it" $ do
      bootOf shell "" 500 "" "press:t" $ \answer -> do
        assertEqual "the first press only opened it" [] =<< postedOf answer
        assertEqual "and it is up" "on" =<< textAt "prompt" answer
      bootOf shell "" 500 "" "press:t press:t" $ \answer -> do
        assertEqual "the second is the letter" [("set-state", ["r1"])]
          =<< postedOf answer
        assertEqual "as TODO" [Just "TODO"] =<< keywordsOf answer

    -- The `ONCE' rule, owed by the palette rather than by the map: a HELD `t'
    -- would open and then commit through what it opened.  The dispatch's list
    -- cannot reach that — it governs rows, and the repeat arrives while every
    -- row is already dead.
  , testCase "a held t opens the palette and stops there" $
      bootOf shell "" 500 "" "press:t repeat:t repeat:t" $ \answer -> do
        assertEqual "nothing was written" [] =<< postedOf answer
        assertEqual "and the palette is waiting for a real press" "on"
          =<< textAt "prompt" answer

    -- The exclusivity the letters need: while the palette is up every `table'
    -- row is dead, so `n' moves nothing and `d' — dired's archive flag out
    -- there — is DONE in here.  The gating is `typing()', which the palette
    -- turns on with no field focused at all.
  , testCase "the table's own letters are the palette's while it is up" $
      bootOf shell "" 500 "C-c C-t" "press:n press:d" $ \answer -> do
        assertEqual "the cursor never moved" 0 =<< intAt "cursor" answer
        assertEqual "nothing was flagged" [] =<< textsAt "flagged" answer
        assertEqual "and d set a state" [("set-state", ["r1"])] =<< postedOf answer
        assertEqual "the one it names" [Just "DONE"] =<< keywordsOf answer

  , testCase "the meta entry clears the keyword rather than setting one" $
      bootOf shell "" 500 "C-c C-t" "press:c" $ \answer -> do
        assertEqual "a null keyword" [Nothing] =<< keywordsOf answer
        assertEqual "and the pill says so" "C-c C-t → org-glance-overview:todo (*clear* · 1)"
          =<< textAt "echo" answer

  , testCase "/ falls back to typing, and RET takes what is left" $
      bootOf shell "" 500 "C-c C-t" "press:/ type:done press:Enter" $ \answer -> do
        assertEqual "the narrowed choice" [Just "DONE"] =<< keywordsOf answer
        assertEqual "the pill" "C-c C-t → org-glance-overview:todo (DONE · 1)" =<< textAt "echo" answer

    -- C-n is a reserved chord the map never claims; the palette claims it while
    -- its own field has focus, the way a focused select keeps its arrows.
  , testCase "C-n walks the fallback list, and the arrows do the same" $
      mapM_ (\key -> bootOf shell "" 500 "C-c C-t" ("press:/ press:" <> key)
               (assertEqual (T.unpack key <> ": stepped to the second entry")
                            [Just "READING"] <=< keywordsOf))
            ["C-n press:Enter", "ArrowDown press:Enter"]

    -- One door out of either mode: `/' is entered and never left, so ESC is
    -- what closes the palette wherever a reader is standing in it.
  , testCase "ESC leaves the palette from either mode and writes nothing" $
      mapM_ (\acts -> bootOf shell "" 500 "C-c C-t" acts $ \answer -> do
               assertEqual (T.unpack acts <> ": no command went")
                           [] =<< postedOf answer
               assertEqual "the overlay is down" "" =<< textAt "prompt" answer)
            ["press:Escape", "press:/ press:Escape"]

  , testCase "over a marked set it names the whole set" $
      bootOf shell "" 500 "m m C-c C-t" "press:t" $ \answer -> do
        assertEqual "the rows" [("set-state", ["r1", "r2"])] =<< postedOf answer
        assertEqual "and the title counts them" "set state · 2 rows"
          =<< textAt "phead" answer
  ]

-- | The two keys that collect a LINE rather than pick from a list: @+@ and the
-- reschedule chords.  They raise the same overlay the value palette does, in
-- its text mode — no list, no letters, RET commits what was typed.
--
-- What is pinned here is the page's half: which body each key posts, that both
-- chords are claimed off the browser, and that the log names what landed.  The
-- date grammar is the server's and is @TestQuery@'s subject.
promptKeySpec :: IO T.Text -> TestTree
promptKeySpec shell = testGroup "Shell capture and reschedule"
  [ testCase "+ raises a line to type and RET captures it" $
      bootOf shell "" 500 "+" "type:milk press:Enter" $ \answer -> do
        assertEqual "the palette said what it is for" "capture · a headline for the inbox"
          =<< textAt "phead" answer
        assertEqual "one capture, naming no rows" ["capture"] =<< namesOf answer
        assertEqual "carrying the line as typed" ["milk"] =<< capturedOf answer
        assertEqual "the pill names the file it landed in"
                    "+ → org-glance-overview:capture (captured · /o/inbox.org)"
          =<< textAt "echo" answer
        assertEqual "and the log names the headline"
                    (Just "headline \"milk\" captured into /o/inbox.org")
          =<< lastLog answer
        assertEqual "the overlay is down" "" =<< textAt "prompt" answer

    -- The palette is up with a field in it, so `typing()' is true: every table
    -- row is dead and the keys are the field's.
  , testCase "and ESC leaves it having written nothing" $
      bootOf shell "" 500 "+" "type:milk press:Escape" $ \answer -> do
        assertEqual "no command went" [] =<< namesOf answer
        assertEqual "the overlay is down" "" =<< textAt "prompt" answer

  , testCase "an empty line captures nothing and says so" $
      bootOf shell "" 500 "+" "press:Enter" $ \answer -> do
        assertEqual "no command went" [] =<< namesOf answer
        assertEqual "the pill says why"
                    "+ → org-glance-overview:capture (nothing to capture)"
          =<< textAt "echo" answer

  , testCase "a refused capture is one cmd error line" $
      bootOf shell "" 500 "" "refuse press:+ type:milk press:Enter" $ \answer -> do
        assertEqual "the command still went" ["capture"] =<< namesOf answer
        assertEqual "and the log carries the server's own words"
                    (Just "capture failed: #+GLANCE_CAPTURE_TARGET: /x.org is an absolute path")
          =<< lastLog answer

    -- The chords survive the browser where `C-c C-t' does not, and what the
    -- page owes is the same: both halves claimed off it.
  , testCase "both reschedule chords are claimed, and name the keyword" $
      mapM_ (\(keys, chord, keyword) ->
               bootOf shell "" 500 keys "" $ \answer -> do
                 assertEqual (T.unpack keys <> ": the palette is up") "on"
                   =<< textAt "prompt" answer
                 assertEqual "titled by the keyword and the rows it runs over"
                             (keyword <> " · 1 row") =<< textAt "phead" answer
                 assertEqual "neither chord was left to the browser"
                             ["C-c", chord] =<< textsAt "prevented" answer)
            [("C-c C-s", "C-s", "scheduled"), ("C-c C-d", "C-d", "deadline")]

  , testCase "a date goes to the server as the text that was typed" $
      bootOf shell "" 500 "C-c C-s" "type:+3d press:Enter" $ \answer -> do
        assertEqual "one command, over the row at point"
                    [("set-planning", ["r1"])] =<< postedOf answer
        assertEqual "with the keyword and the date beside it"
                    [("SCHEDULED", Just "+3d")] =<< plannedOf answer
        assertEqual "the pill names what was asked for"
                    "C-c C-s → org-glance-overview:schedule (+3d · 1)"
          =<< textAt "echo" answer
        assertEqual "and the log names the row"
                    (Just "headline \"one\" scheduled +3d") =<< lastLog answer

    -- An empty line is the clear: the entry comes off, and the server drops the
    -- line with it when it was the last one.
  , testCase "an empty line clears the entry" $
      bootOf shell "" 500 "C-c C-d" "press:Enter" $ \answer -> do
        assertEqual "a null date" [("DEADLINE", Nothing)] =<< plannedOf answer
        assertEqual "the pill says which" "C-c C-d → org-glance-overview:deadline (cleared · 1)"
          =<< textAt "echo" answer
        assertEqual "and so does the log"
                    (Just "headline \"one\" deadline cleared") =<< lastLog answer

    -- The marked set, like every other command that names rows.
  , testCase "over a marked set it names the whole set" $
      bootOf shell "" 500 "m m C-c C-s" "type:today press:Enter" $ \answer -> do
        assertEqual "the marked pair" [("set-planning", ["r1", "r2"])] =<< postedOf answer
        assertEqual "and the title counts them" "scheduled · 2 rows"
          =<< textAt "phead" answer
  ]

-- | The command names the page posted, in order — what a capture is read by,
-- since it names no rows for 'postedOf' to report.
namesOf :: Value -> IO [T.Text]
namesOf answer = traverse (textAt "name") =<< listAt "commands" answer

-- | The @args@ object of each posted command, which the three readers below cut
-- their own field out of.
argsOf :: Value -> IO [Value]
argsOf answer = traverse (field "args") =<< listAt "commands" answer

-- | The line each posted capture carried.
capturedOf :: Value -> IO [T.Text]
capturedOf = traverse (textAt "text") <=< argsOf

-- | The keyword and date each posted @set-planning@ carried.
plannedOf :: Value -> IO [(T.Text, Maybe T.Text)]
plannedOf = traverse one <=< argsOf
  where one v = (,) <$> textAt "keyword" v <*> maybeTextAt "date" v

-- | The message on the last line of the event strip, or 'Nothing' where it has
-- none.
lastLog :: Value -> IO (Maybe T.Text)
lastLog answer = fmap (message . cut) . listToMaybe . reverse <$> logOf answer

-- | @o@: what the row points at, followed.
--
-- The gesture is decided by the ANSWER — none refuses, one opens without
-- asking, several raise the palette — so every case here runs the fetch and
-- reads what came of it.  Which links a subtree holds is @TestQuery@'s
-- ("Links") and the route's shape is @linksSpec@'s; this is the keystroke.
openKeySpec :: IO T.Text -> TestTree
openKeySpec shell = testGroup "Shell open"
  [ testCase "o asks about the row at point" $
      bootOf shell "" 500 "o" "" $
        assertEqual "one request, naming the row" ["/links?id=r1"] <=< textsAt "linked"

  , testCase "! is the same command, and reaches it the same way" $
      bootOf shell "" 500 "!" "" $ \answer -> do
        assertEqual "the same request" ["/links?id=r1"] =<< textsAt "linked" answer
        -- Raising a palette is not a landing, so the pill still carries what
        -- `run\' says of the row — the command and its help — the way it does
        -- while the state palette is up.  The landing is the letter.
        assertEqual "under the same name"
                    "! → org-glance-overview:open · follow this row\'s link; several raise the palette"
          =<< textAt "echo" answer

  , testCase "one link opens without asking" $
      bootOf shell "" 500 "" "onelink press:o" $ \answer -> do
        assertEqual "the tab, with the opener cut"
                    [("https://one.example/a", "_blank", "noopener")] =<< openedOf answer
        assertEqual "no palette went up" "" =<< textAt "prompt" answer
        assertEqual "the pill names the command and what it opened"
                    "o → org-glance-overview:open (First reference)"
          =<< textAt "echo" answer
        assertEqual "and the log names the target"
                    (Just "link \"https://one.example/a\" opened") =<< lastLog answer

  , testCase "no link at all is a refusal that names the command" $
      bootOf shell "" 500 "" "nolinks press:o" $ \answer -> do
        assertEqual "nothing opened" [] =<< openedOf answer
        assertEqual "no palette either" "" =<< textAt "prompt" answer
        assertEqual "and the pill says why"
                    "o → org-glance-overview:open (no links)" =<< textAt "echo" answer

    -- Several is the palette: a FLAT list under which-key letters, each entry
    -- described the way the row's own text describes it, with the target beside
    -- it.  No source table — one row points where it points and no scope
    -- classified it — so the layout is the fallback's shape with the letters on.
  , testCase "several raise the palette, one letter each" $
      bootOf shell "" 500 "o" "" $ \answer -> do
        assertEqual "raised" "on" =<< textAt "prompt" answer
        assertEqual "titled by the count" "open · 3 links" =<< textAt "phead" answer
        assertEqual "the entries, in the order the subtree writes them"
          [ ("pe", "", ["f [F]irst reference"],  [])
          , ("pe", "", ["s [S]econd reference"], [])
          , ("pe", "", ["m [m]ailto:t@example.org"], []) ] =<< paletteOf answer
        assertEqual "and the foot says a letter opens rather than sets"
                    "a letter opens it · / to search · ESC leaves"
          =<< textAt "pfoot" answer

    -- The press that raised THIS palette has been dispatched and gone by the
    -- time the answer lands, where `t' is still travelling when its palette
    -- goes up.  So nothing is declined here, and the first letter commits.
  , testCase "a letter opens its link and closes the palette" $
      bootOf shell "" 500 "o" "press:s" $ \answer -> do
        assertEqual "the second one" [("https://two.example/b", "_blank", "noopener")]
          =<< openedOf answer
        assertEqual "the overlay is down" "" =<< textAt "prompt" answer
        assertEqual "the pill names it by its description"
                    "o → org-glance-overview:open (Second reference)"
          =<< textAt "echo" answer

  , testCase "ESC leaves it having opened nothing" $
      bootOf shell "" 500 "o" "press:Escape" $ \answer -> do
        assertEqual "nothing opened" [] =<< openedOf answer
        assertEqual "the overlay is down" "" =<< textAt "prompt" answer

    -- `/' is the established completing-read, and it narrows over the target as
    -- well as the description: a reader who remembers the host and not the
    -- wording has only the one.
  , testCase "/ narrows over the descriptions and the targets alike" $ do
      bootOf shell "" 500 "o" "press:/ type:second" $
        assertEqual "by description"
          [("pe pat", "", ["Second reference"], [])] <=< paletteOf
      bootOf shell "" 500 "o" "press:/ type:one.example" $
        assertEqual "by target, which no description spells"
          [("pe pat", "", ["First reference"], [])] <=< paletteOf
      bootOf shell "" 500 "o" "press:/ type:second press:Enter" $
        assertEqual "and RET opens what is left"
          [("https://two.example/b", "_blank", "noopener")] <=< openedOf

    -- A held key must not be a browser tab per repeat, which is why the command
    -- is on the ONCE list beside the writes.
  , testCase "a held o asks once" $
      bootOf shell "" 500 "o" "repeat:o repeat:o repeat:o" $
        assertEqual "one request" ["/links?id=r1"] <=< textsAt "linked"

  , testCase "a refused answer is one cmd error line and no palette" $
      bootOf shell "" 500 "" "refuse press:o" $ \answer -> do
        assertEqual "nothing opened" [] =<< openedOf answer
        assertEqual "no palette" "" =<< textAt "prompt" answer
        assertEqual "and the log carries the server's own words"
                    (Just "open failed: no headline with id r1") =<< lastLog answer
  ]

-- | Every tab the page opened: the URL, the target name and the window features
-- — @noopener@ being half of what makes following a link safe.
openedOf :: Value -> IO [(T.Text, T.Text, T.Text)]
openedOf answer = traverse one =<< listAt "opened" answer
  where one v = (,,) <$> textAt "url" v <*> textAt "target" v <*> textAt "features" v

-- | @a@: the agenda, which is a canned VIEW rather than a mode.
--
-- One query through the door @g@ uses — into the URL, asked of the server,
-- mounted as the renderer's chips — plus the one thing the default view does
-- not want, which is the scheduled sort insisted on once the rows are up.
agendaSpec :: IO T.Text -> TestTree
agendaSpec shell = testGroup "Shell agenda"
  [ testCase "applies its query the way g applies the tree's default" $
      bootOf shell "?q=" 500 "a" "" $ \answer -> do
        assertEqual "the boot's two, then the remount's"
          [ "/headlines?limit=100", "/headlines"
          , "/headlines?q=state%3A*active*%20-planned%3Anone&limit=100"
          , "/headlines?q=state%3A*active*%20-planned%3Anone" ]
          =<< textsAt "asked" answer
        assertEqual "and the URL it settles on is that query"
                    "?q=state%3A*active*+-planned%3Anone" =<< textAt "url" answer

  , testCase "the rows land in scheduled order, earliest first" $
      bootOf shell "?q=" 500 "a" "" $
        assertEqual "the sort the view is for"
                    (Just ("scheduled", True)) <=< sortOf

  , testCase "and the pill names the command and the count the server answered" $
      bootOf shell "?q=" 3 "a" "" $
        assertEqual "counted by the server, not by the page it painted"
                    "a → org-glance-agenda (agenda · 3 rows)" <=< textAt "echo"

  , testCase "one row is one row" $
      bootOf shell "?q=" 1 "a" "" $
        assertEqual "singular" "a → org-glance-agenda (agenda · 1 row)" <=< textAt "echo"

    -- An asset with no programmatic sort keeps the order the view declares,
    -- which is already this one; the key says what it did either way rather
    -- than throwing on a call that is not there.
  , testCase "an asset without a programmatic sort still applies the view" $
      bootOf shell "?q=" 500 "" "sortless press:a" $ \answer -> do
        assertEqual "no sort was asked for" Nothing =<< sortOf answer
        assertEqual "the query still went"
                    "?q=state%3A*active*+-planned%3Anone" =<< textAt "url" answer

    -- `g' is the way home, and it is the way home from here like anywhere else.
  , testCase "g returns to the tree's default view" $
      bootOf shell "?q=" 500 "a g" "" $
        assertEqual "the last query asked for is the default's"
                    "?q=state%3A*active*" <=< textAt "url"

    -- The landing is armed for ONE boot: a second remount that nobody asked an
    -- agenda of must not re-sort and must not echo a count.
  , testCase "the landing is spent by the boot it was armed for" $
      bootOf shell "?q=" 500 "a" "close:view-changed" $
        assertEqual "the remount behind the close echoed no agenda"
                    "a → org-glance-agenda (agenda · 500 rows)" <=< textAt "echo"

  , testCase "a held a remounts once" $
      bootOf shell "?q=" 500 "a" "repeat:a repeat:a repeat:a" $
        assertEqual "one remount, so one pair of fetches"
          [ "/headlines?limit=100", "/headlines"
          , "/headlines?q=state%3A*active*%20-planned%3Anone&limit=100"
          , "/headlines?q=state%3A*active*%20-planned%3Anone" ] <=< textsAt "asked"
  ]

-- | The sort the agenda asked the renderer for, if any.  Through `field', so a
-- harness that stopped reporting the call at all fails loudly rather than
-- reading as a page that asked for none.
sortOf :: Value -> IO (Maybe (T.Text, Bool))
sortOf answer = field "sorted" answer >>= said
  where said Null     = pure Nothing
        said sorted   = Just <$> ((,) <$> textAt "column" sorted
                                      <*> boolAt "ascending" sorted)

-- | The which-key letters: the assignment, driven as the pure function it is,
-- and the list it draws.  The letters are what a reader learns by heart, so
-- what is pinned is that one cycle always yields the same ones — the rule is
-- order-only and each entry claims the first still-free letter of its OWN
-- spelling.  Which rows a commit names is @commandKeySpec@'s subject.
whichKeySpec :: IO T.Text -> TestTree
whichKeySpec shell = testGroup "Shell which-key"
  [ testCase "the assignment, cycle by cycle" $ mapM_ (assigns shell)
      -- The plain chain: DONE has the d, so DELEGATED falls through to its e.
      [ ( "TODO,DONE,DELEGATED", ["t@0", "d@0", "e@1"] )
      -- A whole tree's, in the order the producer sends it — actives as
      -- declared, then the done-like ones, then the meta.  Nothing is
      -- special-cased: DONE is `o' for the reason DELEGATED is `e'.
      , ( "TODO,NEXT,STARTED,WAITING,DELEGATED,CANCELLED,DONE,*clear*"
        , ["t@0", "n@0", "s@0", "w@0", "d@0", "c@0", "o@1", "l@2"] )
      -- Synthetic, since no real cycle exhausts a letter pool: an entry with
      -- nothing left is UNBOUND rather than stealing one, which is what keeps
      -- the letters above it where they were.
      , ( "ON,NO,NOON", ["o@0", "n@0", "-"] )
      -- The meta is in the pool like any other, and last buys it no privilege:
      -- its stars are not letters, so `c' where nothing took one, and its own
      -- spelling is what it falls through when something did.
      , ( "TODO,DONE,*clear*",    ["t@0", "d@0", "c@1"] )
      , ( "CANCELLED,*clear*",    ["c@0", "l@2"] ) ]

    -- What the reader sees, and why: one row per SOURCE in precedence order,
    -- its keywords in the Active and Inactive cells, each an accent-boxed key
    -- token and the word with the claimed letter BOLD WHERE IT SITS — which is
    -- the whole of the teaching.  The table IS the classify chain: `READING'
    -- under `book' says which scope answered for it.  The meta spans a row of
    -- its own at the foot, in the muted italic every starred value wears.
  , testCase "the table draws one row per source, keywords in their cells" $
      bootOf shell "" 500 "C-c C-t" "" $ \answer -> do
        assertEqual "the header, the sources in order, and the meta last"
          [ ("pr ph", "source",   ["active"],      ["inactive"])
          , ("pr",    "file",     ["l [L]ATER"],   [])
          , ("pr",    "book",     ["r [R]EADING"], ["e R[E]AD"])
          , ("pr",    "built-in", ["t [T]ODO"],    ["d [D]ONE"])
          , ("pr pm", "",         ["c *[c]lear*"], []) ] =<< paletteOf answer
        assertEqual "and the foot names the keys the list cannot draw"
                    "a letter sets it · / to search · ESC leaves"
          =<< textAt "pfoot" answer

    -- The palette resolves for the rows the command would run over, which is
    -- the marked set where there is one — the same rows `overTargets' counts in
    -- the title, asked of the server as one request.
    -- One parameter per id rather than the comma list a caller types by hand:
    -- the fallback row id is a path, and a comma in one would split it on the
    -- other side.
  , testCase "the resolution is asked for the rows the command names" $ do
      bootOf shell "" 500 "C-c C-t" "" $
        assertEqual "the row at point" ["/keywords?ids=r1"] <=< textsAt "resolved"
      bootOf shell "" 500 "m m C-c C-t" "" $
        assertEqual "the marked set, in one request"
                    ["/keywords?ids=r1&ids=r2"] <=< textsAt "resolved"

    -- Two tags, two rows: the order is the server's and this page draws it as
    -- it arrives, which is what makes the table the resolution rather than a
    -- rendering of it.
  , testCase "a set spanning two tags shows both tag sources" $
      bootOf shell "" 500 "" "twotags press:t" $
        assertEqual "book then film, then the built-in cycle"
          [ ("pr ph", "source",   ["active"],       ["inactive"])
          , ("pr",    "book",     ["r [R]EADING"],  ["e R[E]AD"])
          , ("pr",    "film",     ["w [W]ATCHING"], ["a W[A]TCHED"])
          , ("pr",    "built-in", ["t [T]ODO"],     ["d [D]ONE"])
          , ("pr pm", "",         ["c *[c]lear*"],  []) ] <=< paletteOf

    -- The hues are the producer's and travel on the state column; the
    -- resolution names keywords alone, so the palette goes and looks each one
    -- up.  A keyword no badge names carries none, and is drawn all the same.
  , testCase "each keyword wears its own badge colour, where there is one" $
      bootOf shell "" 500 "C-c C-t" "" $
        assertEqual "READING, TODO and DONE have badges; LATER and READ do not"
          [ ("[R]EADING", "#bb9af7"), ("[T]ODO", "#e0af68"), ("[D]ONE", "#73daca") ]
          <=< paletteHues

    -- The overlay goes up on the keypress and the answer fills it, so the guard
    -- that makes the raising press not a letter is unmoved and ESC works from
    -- the moment the key lands.
  , testCase "the palette is up before the resolution is" $
      bootOf shell "" 500 "" "stall press:t" $ \answer -> do
        assertEqual "raised" "on" =<< textAt "prompt" answer
        assertEqual "with a line saying what it is waiting for"
                    [("pnone", "", ["resolving…"], [])] =<< paletteOf answer

    -- The fallback is FLAT — every entry, whichever source it came from, under
    -- no table at all — and drops the token column outright: no letter commits
    -- there, so drawing one would be a lie about what typing it does.  The
    -- cursor is this list's own, and it opens on the first row.
  , testCase "/ flattens the table, drops the letters and names its own keys" $
      bootOf shell "" 500 "C-c C-t" "press:/" $ \answer -> do
        assertEqual "the box says which mode it is in" "narrow"
          =<< textAt "pmode" answer
        assertEqual "the same entries in the same order, with a cursor and no tokens"
          [ ("pe pat", "", ["LATER"],   [])
          , ("pe",     "", ["READING"], [])
          , ("pe",     "", ["READ"],    [])
          , ("pe",     "", ["TODO"],    [])
          , ("pe",     "", ["DONE"],    [])
          , ("pe pm",  "", ["*clear*"], []) ] =<< paletteOf answer
        assertEqual "and the foot names the keys that are live there"
                    "RET sets it · C-n/C-p walks · ESC leaves"
          =<< textAt "pfoot" answer

  , testCase "typing there narrows to what matches" $
      bootOf shell "" 500 "C-c C-t" "press:/ type:ead" $
        assertEqual "the two book keywords hold it, nothing else does"
          [ ("pe pat", "", ["READING"], [])
          , ("pe",     "", ["READ"],    []) ] <=< paletteOf

    -- A resolution that does not arrive takes the overlay down rather than
    -- leaving a palette with nothing in it: there is no state to pick, and the
    -- log is where the reason goes.
  , testCase "a refused resolution closes the palette and says so" $
      bootOf shell "" 500 "" "refuse press:t" $ \answer -> do
        assertEqual "the overlay is down" "" =<< textAt "prompt" answer
        assertEqual "and the log named it"
                    (Just "keywords failed: GET /keywords?ids=<row id>")
          =<< lastLog answer
  ]

-- | The sheet's two panes, driven through the keys a reader presses.  What is
-- asserted here is the half the page owns: what the panel shows, how it is
-- moved over and opened, how it grows, what an emptied key means, what a sync
-- sends, and which of the two shapes the sheet is in.  The cut between the
-- panes is the server's and is @TestQuery@'s subject; nothing here re-states it.
--
-- The panel is modal, so most of these open with @TAB@ into it: @pnav@ is the
-- panel holding the keys, @pat@ is the row its cursor is on, and @focus@ names
-- a field only while a row is open — in nav there is nothing focused at all.
--
-- @Enter@ materializes the first row, which is where every case starts.
sheetSpec :: IO T.Text -> TestTree
sheetSpec shell = testGroup "Shell sheet"
  [ testCase "materialize opens two panes over one subtree" $
      bootOf shell "" 500 "Enter" "" $ \answer -> do
        assertEqual "the textarea holds the body, every region lifted out"
                    "* TODO one\n" =<< textAt "sheet" answer
        -- The three planning rows first, in org's own order and empty where the
        -- headline has no entry, then the drawer in file order.
        assertEqual "the panel holds the planning rows and then the drawer"
                    [ ["SCHEDULED", "<2026-08-01 Sat>"], ["DEADLINE", ""]
                    , ["CLOSED", ""], ["EFFORT", "0:30"] ]
                    =<< pairsAt "props" answer
        -- Read-only, full width under the panes, and never sent back.  The
        -- drawer's INTERIOR alone: the widget being the drawer says what it is,
        -- so the two delimiter lines would be spent saying it twice.
        assertEqual "the logbook is shown, its delimiters left off"
                    "- moved here" =<< textAt "logbook" answer
        assertEqual "and the sheet is in its two-pane shape" "" =<< textAt "shape" answer
        -- The panel is read-only text until it is crossed into: the keys are
        -- the body's, and the cursor is waiting on the first row.
        assertEqual "the keys are in the body pane" False =<< boolAt "pnav" answer
        assertEqual "with the focus in it" "mtext" =<< textAt "focus" answer
        assertEqual "and the panel's cursor at the top" 0 =<< intAt "pat" answer

    -- The row id is the SERVER's: it never reaches this page, so there is no
    -- row to warn about and no note to draw.  The file still has it, which
    -- TestQuery's lens group is what shows.
  , testCase "the identity property never reaches the panel" $
      bootOf shell "" 500 "Enter" "" $ \answer -> do
        rows <- pairsAt "props" answer
        assertEqual "no row names it" [] [ r | r <- rows, take 1 r == ["ORG_GLANCE_ID"] ]

    -- TAB crosses the panes and nothing else, so the panel keeps its cursor:
    -- two stops, and the same key comes back to the row it left.
  , testCase "TAB crosses to the panel and back, and the cursor is remembered" $ do
      bootOf shell "" 500 "Enter" "press:Tab" $ \answer -> do
        assertEqual "the panel has the keys" True =<< boolAt "pnav" answer
        assertEqual "with nothing focused, which is what frees the letters"
                    "" =<< textAt "focus" answer
        assertEqual "and the cursor on its first row" 0 =<< intAt "pat" answer
      bootOf shell "" 500 "Enter" "press:Tab press:n press:Tab" $ \answer -> do
        assertEqual "back in the body" "mtext" =<< textAt "focus" answer
        assertEqual "the panel let go of the keys" False =<< boolAt "pnav" answer
        assertEqual "and kept where it had got to" 1 =<< intAt "pat" answer
      bootOf shell "" 500 "Enter" "press:Tab press:n press:Tab press:Tab" $
        assertEqual "which is where the next crossing lands" 1 <=< intAt "pat"

    -- Two stops make the direction say nothing, so S-TAB is that one toggle
    -- rather than a second walk with an end of its own to fall off.
  , testCase "S-TAB is the same crossing, both ways" $ do
      bootOf shell "" 500 "Enter" "press:S-Tab" $
        assertEqual "into the panel" True <=< boolAt "pnav"
      bootOf shell "" 500 "Enter" "press:Tab press:S-Tab" $
        assertEqual "and out of it" "mtext" <=< textAt "focus"

    -- Nothing is focused in nav, so every printable key is free: both profiles'
    -- movement is bound at once, and the arrows ask for no profile at all.
  , testCase "nav moves on n/p, j/k and the arrows, and stops at the ends" $ do
      bootOf shell "" 500 "Enter" "press:Tab press:n press:n" $ \answer -> do
        assertEqual "two rows down" 2 =<< intAt "pat" answer
        -- The panel holding the keys with nothing focused is a focus of its own
        -- as far as the map is concerned, or these letters would move the table
        -- under the sheet as well.
        assertEqual "and the table's own row did not move" 0 =<< intAt "cursor" answer
      bootOf shell "" 500 "Enter" "press:Tab press:j press:j press:k" $
        assertEqual "vi's pair walks the same rows" 1 <=< intAt "pat"
      bootOf shell "" 500 "Enter" "press:Tab press:ArrowDown press:ArrowDown press:ArrowUp" $
        assertEqual "and so do the arrows" 1 <=< intAt "pat"
      bootOf shell "" 500 "Enter" "press:Tab press:p" $
        assertEqual "the first row is the end of the walk up" 0 <=< intAt "pat"
      bootOf shell "" 500 "Enter" "press:Tab press:n press:n press:n press:n" $
        assertEqual "and the last property the end of the walk down" 3 <=< intAt "pat"

    -- Editing a row that is there is almost always editing its value; a
    -- planning row has no editable key at all, org owning that half of it.
  , testCase "RET opens the row at point, and a planning row opens its value" $ do
      bootOf shell "" 500 "Enter" "press:Tab press:Enter" $
        assertEqual "the value of the planning row at point" "pval:0" <=< textAt "focus"
      bootOf shell "" 500 "Enter" "press:Tab press:n press:n press:n press:Enter" $
        assertEqual "and of the property under them" "pval:3" <=< textAt "focus"

    -- One row, two fields: TAB has nothing else to mean inside an open row, so
    -- the pane crossing is suspended for as long as one is open.
  , testCase "TAB hops the open row's two fields rather than leaving" $ do
      bootOf shell "" 500 "Enter" "press:Tab press:Enter press:Tab" $ \answer -> do
        assertEqual "over to the key" "pkey:0" =<< textAt "focus" answer
        assertEqual "and still in the panel" True =<< boolAt "pnav" answer
      bootOf shell "" 500 "Enter" "press:Tab press:Enter press:Tab press:Tab" $
        assertEqual "and back to the value" "pval:0" <=< textAt "focus"
      bootOf shell "" 500 "Enter" "press:Tab press:Enter press:S-Tab" $
        assertEqual "S-TAB is that same hop" "pkey:0" <=< textAt "focus"

  , testCase "RET commits the open row and goes back to nav" $
      bootOf shell "" 500 "Enter"
             "press:Tab press:n press:n press:n press:Enter pval:3=0:45 press:Enter" $
        \answer -> do
          assertEqual "the row took the text its field was holding"
                      [ ["SCHEDULED", "<2026-08-01 Sat>"], ["DEADLINE", ""]
                      , ["CLOSED", ""], ["EFFORT", "0:45"] ]
                      =<< pairsAt "props" answer
          assertEqual "the fields are gone" "" =<< textAt "focus" answer
          assertEqual "the panel still has the keys" True =<< boolAt "pnav" answer
          assertEqual "and the cursor stayed on the row" 3 =<< intAt "pat" answer

    -- `+' is the add affordance, and the whole of it: keyboard-first means the
    -- key IS the offer, where a row that is always empty was chrome every
    -- reader of the panel had to filter back out.
  , testCase "+ adds a property at the end and opens it" $ do
      bootOf shell "" 500 "Enter" "press:Tab press:+" $ \answer -> do
        assertEqual "an empty row at the end"
                    [ ["SCHEDULED", "<2026-08-01 Sat>"], ["DEADLINE", ""]
                    , ["CLOSED", ""], ["EFFORT", "0:30"], ["", ""] ]
                    =<< pairsAt "props" answer
        assertEqual "with the cursor on it" 4 =<< intAt "pat" answer
        assertEqual "open at its key, which is the thing being typed"
                    "pkey:4" =<< textAt "focus" answer
      bootOf shell "" 500 "Enter" "press:Tab press:+ pkey:4=ADDED press:Enter" $ \answer -> do
        assertEqual "and committing it is a property"
                    [ ["SCHEDULED", "<2026-08-01 Sat>"], ["DEADLINE", ""]
                    , ["CLOSED", ""], ["EFFORT", "0:30"], ["ADDED", ""] ]
                    =<< pairsAt "props" answer
        assertEqual "with nothing grown under it" 4 =<< intAt "pat" answer

    -- ESC over an open row is the ROW's, and puts back the text it was opened
    -- on; only from nav does the key reach the sheet's own ladder.
  , testCase "ESC puts an open row back, and the next one closes the sheet" $ do
      bootOf shell "" 500 "Enter"
             "press:Tab press:n press:n press:n press:Enter pval:3=0:45 press:Escape" $ \answer -> do
        assertEqual "the value it was opened on"
                    [ ["SCHEDULED", "<2026-08-01 Sat>"], ["DEADLINE", ""]
                    , ["CLOSED", ""], ["EFFORT", "0:30"] ]
                    =<< pairsAt "props" answer
        assertEqual "the sheet is still up" "on" =<< textAt "modal" answer
        assertEqual "and back in nav" True =<< boolAt "pnav" answer
        assertEqual "with nothing written" ([] :: [Value]) =<< listAt "writes" answer
      bootOf shell "" 500 "Enter"
             "press:Tab press:n press:n press:n press:Enter press:Escape press:Escape" $
        assertEqual "the second one is the sheet's" "" <=< textAt "modal"

    -- What a sync sends is the committed panel, which is what makes the commit
    -- the thing that means yes.
  , testCase "a sync sends the panes apart, and an empty planning row is not one" $
      bootOf shell "" 500 "Enter"
             ("press:Tab press:n press:n press:n press:Enter pval:3=0:45"
                <> " press:Enter press:C-x press:C-s") $
        \answer -> do
          assertEqual "one write" ["* TODO one\n"] =<< traverse (textAt "body")
                                                   =<< listAt "writes" answer
          -- The identity is the server's and is in neither list; the two empty
          -- planning rows are entries the headline has not got.
          assertEqual "carrying the drawer, edit and all"
                      [[["EFFORT", "0:45"]]]
                      =<< traverse (pairsAt "properties") =<< listAt "writes" answer
          assertEqual "and the planning entries it has"
                      [[["SCHEDULED", "<2026-08-01 Sat>"]]]
                      =<< traverse (pairsAt "planning") =<< listAt "writes" answer
          assertEqual "and it landed" "synced" =<< textAt "state" answer

    -- Emptying every planning row is how the line comes off, which the server
    -- reads as "no planning" rather than as "leave it alone".
  , testCase "an emptied planning row is an entry taken off" $
      bootOf shell "" 500 "Enter"
             "press:Tab press:Enter pval:0= press:Enter press:C-x press:C-s" $
        assertEqual "nothing left to write" [[]]
                    <=< (traverse (pairsAt "planning") <=< listAt "writes")

    -- Emptying a key is how a property is deleted: there is no key to press for
    -- it, and none is owed — the row simply stops naming anything.
  , testCase "an emptied key is a property deleted" $
      bootOf shell "" 500 "Enter"
             ("press:Tab press:n press:n press:n press:Enter pkey:3="
                <> " press:Enter press:C-x press:C-s") $
        assertEqual "the drawer the write asks for" [[]]
                    <=< (traverse (pairsAt "properties") <=< listAt "writes")

    -- C-c ' is org's `edit-special' rhyme.  It re-materializes rather than
    -- converting anything locally, which is what keeps an org parser out of
    -- this page: the raw text it shows is the server's `org', not a join done
    -- here.
  , testCase "C-c ' shows the raw subtree, and again shows the panes" $ do
      bootOf shell "" 500 "Enter" "press:C-c press:'" $ \answer -> do
        assertEqual "the whole subtree, every region spelled out"
                    ("* TODO one\nSCHEDULED: <2026-08-01 Sat>\n:PROPERTIES:\n"
                       <> ":ORG_GLANCE_ID: r1\n:EFFORT: 0:30\n:END:\n"
                       <> ":LOGBOOK:\n- moved here\n:END:\n")
                    =<< textAt "sheet" answer
        assertEqual "the panel is off the sheet" "raw" =<< textAt "shape" answer
        assertEqual "and the logbook strip with it" "" =<< textAt "logbook" answer
        assertEqual "and the pill says which way it went" "C-c ' → org-edit-special (raw org)"
                    =<< textAt "echo" answer
      bootOf shell "" 500 "Enter" "press:C-c press:' press:C-c press:'" $ \answer -> do
        assertEqual "back to the body alone" "* TODO one\n" =<< textAt "sheet" answer
        assertEqual "with the panel back" "" =<< textAt "shape" answer
        assertEqual "the pill" "C-c ' → org-edit-special (properties panel)" =<< textAt "echo" answer

    -- A re-read cannot carry unsaved work, and converting locally would need the
    -- parser this design exists to avoid.  So the toggle is refused, and says
    -- which key would let it through.
  , testCase "a dirty sheet is refused the toggle, in either pane" $ do
      bootOf shell "" 500 "Enter" "sheet:hello press:C-c press:'" $ \answer -> do
        assertEqual "the text stands" "hello" =<< textAt "sheet" answer
        assertEqual "and the shape with it" "" =<< textAt "shape" answer
        assertEqual "named the key" "C-c ' → org-edit-special (sync first — C-x C-s)"
                    =<< textAt "echo" answer
      bootOf shell "" 500 "Enter"
             ("press:Tab press:n press:n press:n press:Enter pval:3=0:45"
                <> " press:Enter press:C-c press:'") $
        \answer -> do
          assertEqual "a committed panel edit is dirty too" "" =<< textAt "shape" answer
          assertEqual "same refusal" "C-c ' → org-edit-special (sync first — C-x C-s)"
                      =<< textAt "echo" answer

    -- The other half of that rule: an edit nobody committed is not one, so the
    -- toggle goes through exactly as it would over a sheet nobody touched.
  , testCase "an open row is not an edit until it is committed" $
      bootOf shell "" 500 "Enter"
             ("press:Tab press:n press:n press:n press:Enter pval:3=0:45"
                <> " press:C-c press:'") $ \answer -> do
        assertEqual "the toggle went through" "raw" =<< textAt "shape" answer
        assertEqual "and said so" "C-c ' → org-edit-special (raw org)" =<< textAt "echo" answer

    -- A remount takes the sheet down and puts it back: both panes, and the work
    -- in either of them.
  , testCase "a remount carries the panel across it" $
      bootOf shell "" 500 "Enter"
             ("press:Tab press:n press:n press:n press:Enter pval:3=0:45"
                <> " press:Enter close:view-changed") $
        \answer -> do
          assertEqual "mounted twice" 2 =<< intAt "mounts" answer
          assertEqual "the panel is back, edit and all"
                      [ ["SCHEDULED", "<2026-08-01 Sat>"], ["DEADLINE", ""]
                      , ["CLOSED", ""], ["EFFORT", "0:45"] ]
                      =<< pairsAt "props" answer
          assertEqual "still dirty against the file, and still synced-looking"
                      "synced" =<< textAt "state" answer

    -- One pane, nothing to cross to: the key goes back to the browser, which is
    -- the whole of what raw mode changes here.
  , testCase "raw mode leaves TAB to the browser" $
      bootOf shell "" 500 "Enter" "press:C-c press:' press:Tab" $ \answer -> do
        assertEqual "the focus stayed in the text" "mtext" =<< textAt "focus" answer
        assertEqual "and the panel never took the keys" False =<< boolAt "pnav" answer
        assertBool "nor the key off the browser"
          . notElem "Tab" =<< textsAt "prevented" answer

    -- Where the cursor was left belongs to the sheet that was open: the next
    -- materialize is a fresh drawer, read-only and at the top of itself.
  , testCase "the panel opens at the top again when the sheet is reopened" $
      bootOf shell "" 500 "Enter" "press:Tab press:n press:Escape press:Enter" $
        \answer -> do
          assertEqual "the cursor is back on the first row" 0 =<< intAt "pat" answer
          assertEqual "and the keys back in the body" False =<< boolAt "pnav" answer
  ]

-- | The settings sheet, driven through the keys a reader presses.  What is
-- asserted is this page's half: that the chord raises it over the layers
-- @\/config@ served, that a box holds one file's @#+TODO:@ lines verbatim, that
-- closing it is the save, and that a pristine one costs no request.  The splice
-- itself is @configSpec@'s subject and the grammar is @TestConfig@'s; nothing
-- here re-states either.
settingsSpec :: IO T.Text -> TestTree
settingsSpec shell = testGroup "Shell settings"
  [ testCase ", opens it over the layers the server serves" $
      bootOf shell "" 500 "," "" $ \answer -> do
        assertEqual "the sheet is up" "on" =<< textAt "settings" answer
        assertEqual "one box per layer, the lines verbatim"
                    ["", "#+TODO: TODO READING | READ"] =<< textsAt "cshown" answer
        assertEqual "the union is previewed" "TODO | DONE" =<< textAt "ceff" answer
        assertEqual "and it opens synced" "synced" =<< textAt "cstate" answer
        assertEqual "with nothing written" ([] :: [Value]) =<< listAt "configWrites" answer

    -- The sheet's own rule, and the reason it has no buttons: the way out is
    -- the save.  Only the layer that moved is written.
  , testCase "ESC syncs the layers that moved and closes" $
      bootOf shell "" 500 "," "ctext:0=#+TODO:_TODO_STARTED_|_DONE press:Escape" $
        \answer -> do
          writes <- listAt "configWrites" answer
          assertEqual "one write, for the layer that moved" 1 (length writes)
          assertEqual "the system layer" "/o/.org-glance/config/system.org"
            =<< textAt "path" (head writes)
          assertEqual "its lines, as typed" ["#+TODO:_TODO_STARTED_|_DONE"]
            =<< textsAt "lines" (head writes)
          -- The empty digest is the pin an absent file carries, handed straight
          -- back: creating the first layer is a write like any other.
          assertEqual "pinned to the digest it was read with" ""
            =<< textAt "digest" (head writes)
          assertEqual "and the sheet is down" "" =<< textAt "settings" answer

  , testCase "a pristine sheet closes without asking the server for anything" $
      bootOf shell "" 500 "," "press:Escape" $ \answer -> do
        assertEqual "no write" ([] :: [Value]) =<< listAt "configWrites" answer
        assertEqual "the sheet is down" "" =<< textAt "settings" answer

    -- The system layer carries two tree-wide fields beside its cycle, and both
    -- ride in that layer's own write: one file, one digest, one splice.
  , testCase "the capture target is a field of the system layer, and rides its write" $
      bootOf shell "" 500 "," "ccap:0=notes/in.org press:Escape" $ \answer -> do
        writes <- listAt "configWrites" answer
        assertEqual "one write, for the layer that moved" 1 (length writes)
        assertEqual "carrying the target" "notes/in.org" =<< textAt "capture" (head writes)
        assertEqual "and the server holds it now" "notes/in.org"
          =<< textAt "servedCapture" answer

  , testCase "and it opens on what the server serves" $
      bootOf shell "" 500 "," "ccap:0=notes/in.org press:C-x press:C-s" $
        assertEqual "the field shows what was typed" "notes/in.org" <=< textAt "ccap"

    -- Two sheets over one page would leave `C-x C-s' and `ESC' guessing which
    -- one they meant.  `typing()' is not what keeps them apart, which is the
    -- point of the case: a click on the open sheet's own header blurs its
    -- textarea, and every `table' row is live again the moment it does.  So the
    -- refusal is stated in `openSettings' rather than left to the focus.
  , testCase "it will not open over the materialize sheet" $
      bootOf shell "" 500 "Enter" "blur press:," $ \answer -> do
        assertEqual "the settings sheet stayed down" "" =<< textAt "settings" answer
        assertEqual "and the subtree is still the one open" "on"
          =<< textAt "modal" answer

  , testCase "C-x C-s syncs mid-edit and leaves the sheet open" $
      bootOf shell "" 500 "," "ctext:1=#+TODO:_A_|_B press:C-x press:C-s" $
        \answer -> do
          assertEqual "one write" 1 . length =<< listAt "configWrites" answer
          assertEqual "the sheet is still up" "on" =<< textAt "settings" answer
          assertEqual "and it is synced again" "synced" =<< textAt "cstate" answer

    -- A file that moved under the sheet is a 409 and the sheet stays open at
    -- `conflict', where C-x C-s overwrites and ESC discards — the materialize
    -- sheet's flow, over config files.
  , testCase "a layer that moved underneath lands at conflict, and ESC discards" $
      bootOf shell "" 500 "," "ctext:1=#+TODO:_A_|_B cmoved press:C-x press:C-s" $
        \answer -> do
          assertEqual "the write was refused" 1 . length =<< listAt "configWrites" answer
          assertEqual "the sheet waits" "conflict" =<< textAt "cstate" answer
          assertEqual "and is still up" "on" =<< textAt "settings" answer
  , testCase "and the second ESC there closes it without writing" $
      bootOf shell "" 500 ","
             "ctext:1=#+TODO:_A_|_B cmoved press:C-x press:C-s press:Escape" $ \answer -> do
        assertEqual "no second write" 1 . length =<< listAt "configWrites" answer
        assertEqual "the sheet is down" "" =<< textAt "settings" answer

    -- The one that matters most here: writing a layer is what moves the
    -- columns, so the close that follows a successful save is `view-changed'.
    -- The sheet is a sibling of `#app' and outlives the remount by where it
    -- sits — asserted rather than assumed, since it is a layout fact.
  , testCase "a view-changed remount leaves the sheet standing" $
      bootOf shell "" 500 "," "ctext:1=#+TODO:_A_|_B close:view-changed" $
        \answer -> do
          assertEqual "the mount was rebuilt" 2 =<< intAt "mounts" answer
          assertEqual "the sheet is still up" "on" =<< textAt "settings" answer
          assertEqual "with the edit still in it"
                      ["", "#+TODO:_A_|_B"] =<< textsAt "cshown" answer
  ]

-- | The event strip, driven through the keys and the acts that write to it.
-- The widget's own contract is what is asserted: the shape of a line, that
-- nothing ever takes one away, that the ring drops from the front, that a
-- repeat is counted rather than repeated, and that a write names the rows it
-- landed on.  Reading the glue cannot answer any of it — every one of these is
-- a fact about what a sequence of calls leaves on screen.
logSpec :: IO T.Text -> TestTree
logSpec shell = testGroup "Shell log"
  [ -- The boot line is an ordinary line: the mount used to clear the strip, so
    -- a page's first second was gone the moment the table arrived.
    testCase "opens on a boot line the mount leaves alone" $
      bootOf shell "" 500 "" "" $ \answer -> do
        strip <- logOf answer
        assertEqual "one line, the boot's" [("info", "boot", "loading …")]
                    (map cut strip)
        assertBool ("a clock opens it: " <> show strip)
                   (all (stamped . stampOf . snd) strip)

    -- Every line, whatever wrote it: a clock, one of the three severities —
    -- spelled in the line and worn as its class, so the colour and the word can
    -- never disagree — and one of the six scopes.
  , testCase "every line is a stamp, a severity and a scope" $
      bootOf shell "" 500 "d q" "offline close:resync" $ \answer -> do
        strip <- logOf answer
        assertBool ("stamped: " <> show strip)
                   (all (stamped . stampOf . snd) strip)
        assertEqual "the severity is the class it wears" []
          [ line | line@(sev, text) <- strip, sevOf text /= sev ]
        assertEqual "out of the three" []
          [ sev | (sev, _text) <- strip, sev `notElem` ["info", "warn", "error"] ]
        assertEqual "and the six scopes" []
          [ scope | (_sev, scope, _msg) <- map cut strip
                  , scope `notElem` ["ws", "sync", "cmd", "filter", "config", "boot"] ]

    -- Five hundred lines, and the OLDEST is what goes: a reader scrolled back
    -- into the strip is reading the recent past, and dropping from the far end
    -- of it is what a ring is for.  Five hundred and one appended over the boot
    -- line takes two off the front.
  , testCase "the ring holds five hundred and drops from the front" $
      bootOf shell "" 500 "" "spam:501" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "capped" 500 (length strip)
        assertEqual "the boot line and `line 0' are what went"
                    ["line 1", "line 2"] [ m | (_s, _c, m) <- take 2 strip ]
        assertEqual "and the newest stands" ["line 500"]
                    [ m | (_s, _c, m) <- drop 499 strip ]

    -- The one mutation an append-only strip allows: a message identical to the
    -- one before it is counted on that line.  A retry loop otherwise fills the
    -- ring with one sentence and takes everything else out of reach.
  , testCase "a repeat is counted on its line rather than written under it" $
      bootOf shell "" 500 "q q q" "" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "the boot line and one more" 2 (length strip)
        assertEqual "counted"
                    [("info", "cmd", "q closes the sheet; there is no window to quit ×3")]
                    (drop 1 strip)

    -- A message that is not the LAST one is a new line, so a repeat interrupted
    -- by anything else starts counting again rather than reaching back.
  , testCase "and only against the line it follows" $
      bootOf shell "" 500 "q d q" "" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "three lines under the boot's" 4 (length strip)
        assertEqual "the last says it once, uncounted"
                    "q closes the sheet; there is no window to quit"
                    (message (last strip))

    -- The connection's two severities, over a daemon that went away: the fetch
    -- that failed is an error and the retry behind it is a warning.
  , testCase "a dead daemon logs the failure and the retry" $
      bootOf shell "" 500 "" "offline close:resync" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "both, in that order"
                    [ ("error", "ws", "load failed: fetch failed")
                    , ("warn", "ws", "disconnected · retrying in 1s") ]
                    (drop 1 strip)

    -- dired's flag, said in words: the pill says what the key did and the strip
    -- says which row it did it to, which is the half that survives the next
    -- keystroke.
  , testCase "d names the row it flagged, and u names it unflagging one" $
      bootOf shell "" 500 "d u" "" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "the row, by its title"
                    [ ("info", "cmd", "headline \"one\" marked for deletion")
                    , ("info", "cmd", "headline \"one\" unmarked for deletion") ]
                    (drop 1 strip)

    -- One line per ROW rather than per request: a set spanning three files can
    -- come back two-thirds applied, so what landed is named row by row.
  , testCase "every archived row is named, one line each" $ do
      bootOf shell "" 500 "d d" "" $ \answer -> do
        strip <- map message . drop 1 . map cut <$> logOf answer
        assertEqual "flagged, then archived"
                    [ "headline \"one\" marked for deletion"
                    , "headline \"one\" archived" ] strip
      bootOf shell "" 500 "d n d n d" "press:d" $ \answer -> do
        strip <- map message . drop 1 . map cut <$> logOf answer
        assertEqual "three flags and three archives"
                    [ "headline \"one\" marked for deletion"
                    , "headline \"two\" marked for deletion"
                    , "headline \"three\" marked for deletion"
                    , "headline \"one\" archived"
                    , "headline \"two\" archived"
                    , "headline \"three\" archived" ] strip

    -- The state a row landed on, and the clear that is not a keyword.
  , testCase "a state that landed names the row and the keyword" $ do
      bootOf shell "" 500 "C-c C-t" "press:t" $ \answer -> do
        strip <- map message . drop 1 . map cut <$> logOf answer
        assertEqual "the keyword it took" ["headline \"one\" → TODO"] strip
      bootOf shell "" 500 "m m C-c C-t" "press:t" $ \answer -> do
        strip <- map message . drop 1 . map cut <$> logOf answer
        assertEqual "both marked rows" [ "headline \"one\" → TODO"
                                       , "headline \"two\" → TODO" ] strip
      bootOf shell "" 500 "C-c C-t" "press:c" $ \answer -> do
        strip <- map message . drop 1 . map cut <$> logOf answer
        assertEqual "the clear is not a keyword"
                    ["headline \"one\" state cleared"] strip

    -- A refusal is the error the pill's count cannot carry: which row, and what
    -- the server said about it.
  , testCase "a refused write is an error line and names no landing" $
      bootOf shell "" 500 "" "refuse press:D" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "the refusal, whole"
                    [("error", "cmd", "r1: a.org changed on disk")] (drop 1 strip)
  ]

-- | The event strip out of a harness answer: the severity class each line
-- wears, and the text it renders.
logOf :: Value -> IO [(T.Text, T.Text)]
logOf answer = traverse one =<< listAt "log" answer
  where one v = (,) <$> textAt "sev" v <*> textAt "text" v

-- | A line as it reads: its severity class, the scope it names, and the message
-- the rest of it is.  The stamp is a clock and is checked by 'stamped'.
cut :: (T.Text, T.Text) -> (T.Text, T.Text, T.Text)
cut (sev, text) = case T.words text of
  (_stamp : _sev : scope : rest) -> (sev, scope, T.unwords rest)
  _shapeless                     -> (sev, "", text)

-- | The message a cut line carries, for the cases that assert only that.
message :: (T.Text, T.Text, T.Text) -> T.Text
message (_sev, _scope, m) = m

-- | The clock a line opens with, and the severity it spells after it.
stampOf, sevOf :: T.Text -> T.Text
stampOf = fromMaybe "" . listToMaybe . T.words
sevOf   = fromMaybe "" . listToMaybe . drop 1 . T.words

-- | Whether T is an @HH:MM:SS@ clock, which is how every line opens.
stamped :: T.Text -> Bool
stamped t = T.length t == 8 && T.index t 2 == ':' && T.index t 5 == ':'
            && T.all (\c -> isDigit c || c == ':') t

-- | The commands the page posted, as the name and the ids each one named.
postedOf :: Value -> IO [(T.Text, [T.Text])]
postedOf answer = traverse one =<< listAt "commands" answer
  where one v = (,) <$> textAt "name" v <*> textsAt "ids" v

-- | The keyword each posted command carried, for the @set-state@ cases.
keywordsOf :: Value -> IO [Maybe T.Text]
keywordsOf = traverse (maybeTextAt "keyword") <=< argsOf

-- | The value palette as it is drawn: per ROW of the resolution table, its
-- classes, the source it names, and the entries in its Active and Inactive
-- cells.  An entry is spelled @KEY WORD@, the word carrying the bolded letter
-- bracketed where it sits; a fallback-mode row is one entry with no token in
-- the active cell.  The hairlines between rows are the rows' own borders, so
-- what reads out of this is the table's shape.
paletteOf :: Value -> IO [(T.Text, T.Text, [T.Text], [T.Text])]
paletteOf answer = traverse one =<< listAt "plist" answer
  where one v = (,,,) <$> textAt "cls" v <*> textAt "source" v
                      <*> spelledAt "active" v <*> spelledAt "inactive" v
        spelledAt key = traverse spelled <=< listAt key
        spelled e = do
          key <- textAt "key" e
          word <- textAt "word" e
          pure (if T.null key then word else key <> " " <> word)

-- | Every badge hue the palette wrote, as the word and the colour, in draw
-- order.  The hues are the producer's and ride on the state column, so what
-- this pins is that the palette goes and finds them for keywords the resolution
-- names without them.
paletteHues :: Value -> IO [(T.Text, T.Text)]
paletteHues answer = do
  rows <- listAt "plist" answer
  entries <- concat <$> traverse halves rows
  filter (not . T.null . snd)
    <$> traverse (\e -> (,) <$> textAt "word" e <*> textAt "color" e) entries
  where halves v = (<>) <$> listAt "active" v <*> listAt "inactive" v

-- | WHAT: the which-key assignment over a comma-separated CYCLE is EXPECTED —
-- one @LETTER\@INDEX@ per entry, and @-@ where an entry claimed nothing.  The
-- rule runs under the harness as the pure function it is, over no page at all.
assigns :: IO T.Text -> (T.Text, [T.Text]) -> Assertion
assigns shell (keywords, expected) =
  bootOf shell "" 500 "" ("assign:" <> keywords)
         (assertEqual (T.unpack keywords) expected <=< textsAt "assigned")

-- | SHELL's glue booted under node on SEARCH, with the server reporting TOTAL
-- matches, KEYS pressed over the table once it settled and ACTS run after
-- those, then CHECK over the harness's whole answer.  A machine with no node
-- runs nothing and passes: the boot is checked wherever there is one, and the
-- glue group still reads the same page as text.
bootOf :: IO T.Text -> T.Text -> Int -> T.Text -> T.Text -> (Value -> Assertion)
       -> Assertion
bootOf shell search total keys acts check = do
  node <- findExecutable "node"
  case node of
    Nothing  -> pure ()
    Just exe -> withTempDir $ \dir -> do
      page <- shell
      glueOf page >>= TIO.writeFile (dir </> "shell.js")
      keysOf page >>= TIO.writeFile (dir </> "keys.json")
      (code, out, err) <- readProcessWithExitCode exe
                            [ harness, dir, T.unpack search, show total
                            , T.unpack keys, T.unpack acts ] ""
      case code of
        ExitSuccess -> check =<<
          either (\e -> assertFailure ("the harness answered: " <> e)) pure
                 (eitherDecode (BL.fromStrict (TE.encodeUtf8 (T.pack out))))
        _failed -> assertFailure ("the boot harness said: " <> err)

-- | The commands a held key delivers once, as the map declares them.  Named
-- rather than spelled twice: two cases read the list, one for the dispatch that
-- honours it and one for the rule that every entry is a bound command.
--
-- The first five write or destroy; the last two do neither and are here because
-- a leaned-on key is ruinous either way — `o' is a browser tab per repeat and
-- `a' a remount per repeat.
onceNames :: [T.Text]
onceNames = [ "filter-drop-token", "unmark-all", "mark-all"
            , "archive-flag", "org-glance-overview:delete"
            , "org-glance-overview:open", "org-glance-agenda" ]

-- | The browser the boot runs in, stubbed down to what it touches.
harness :: FilePath
harness = "test/fixtures/shell-harness.js"

-- | A claim about a page this server serves: strings it must carry, and strings
-- it must not.
data Glue = Glue { glLabel :: String, glHas :: [T.Text], glGone :: [T.Text] }

-- | A claim with nothing to forbid.
glue :: String -> [T.Text] -> Glue
glue label has = Glue label has []

-- | The shell's inline glue, checked as the data it is.  Every row is a case
-- that read the same rendered page and asserted a list of strings into it; as
-- rows they share the render and the two assertion shapes, and adding one is
-- adding a line.
glueSpec :: IO T.Text -> TestTree
glueSpec shell = testGroup "Shell glue"
  [ testCase glLabel $ do
      b <- shell
      holdsAll glLabel glHas b
      holdsNone glLabel glGone b
  | Glue{..} <- shellGlue ]

shellGlue :: [Glue]
shellGlue =
  [ glue "paints a page and loads the rest behind it"
      [ "const PAGE = 100;", "load(`${narrow}limit=${PAGE}`)"
      , "r.headers.get(\"X-Glance-Total\")", "a.total > (a.view.rows || []).length"
      , "if (table && query === asked) paint(b)" ]

  -- The page opens on a view rather than on everything.  It is a query like
  -- any other: in the URL, mounted as a chip, asked of the server — so DEL
  -- takes it off and the whole store is one keystroke away.
  , glue "a bare boot opens on the active view"
      [ "const DEFAULT_QUERY = \"state:*active*\";"
      -- A `q' in the address bar is the reader's own, empty or not.
      , "const bootQuery = () => (params().has(\"q\") ? urlQuery() : DEFAULT_QUERY);"
      , "const asked = (query = bootQuery());"
      -- Injected, then committed: what the page shows and what the address bar
      -- says are the same query from the first paint on.
      , "if (!params().has(\"q\")) remember(asked);"
      , "initialQuery: query," ]

  -- The check compares a filtered answer against an unfiltered one, and this
  -- page can open filtered — a link, or the default view.  A paint under a
  -- query arms nothing, so the baseline is fetched once behind the table.
  , glue "the parity baseline is armed even when the boot was filtered"
      [ "function arm(total) {", "if (!query || all.length) return;"
      , "load(\"\").then((a) => { all = a.view.rows || []; parity(total); })"
      , "arm(a.total); })"
      , "else arm(a.total);" ]

  , glue "hands the filter to the server and aborts stale fetches"
      [ "onFilter: filter", "new AbortController()", "inflight.abort()"
      , "signal: inflight.signal", "load(asking(query))"
      -- One spelling of the query string, so a revalidation cannot be answered
      -- 304 against rows some other question was asked.
      , "const asking = (q) => (q ? `?q=${encodeURIComponent(q)}` : \"\");"
      , "e.name !== \"AbortError\""
      -- The string as typed: the grammar is the server's to parse.
      , "const filter = (q) => commit(q.trim());" ]

  -- A key the columns do not name is a producer virtual key, which is the one
  -- place the renderer's suggestions and the server's parser can be different
  -- versions.  The check reads the rows the page already holds.
  , glue "an empty answer to a virtual key is checked locally"
      [ "function parity(total)", "if (total !== 0 || !query || !all.length) return;"
      , "TableView.parseQuery(query, keys)"
      , "t.key === null && !t.quoted && !t.negated"
      , "filter parity divergence — asset/daemon version skew"
      , "console.warn(note, { query, server: total, local })"
      , "if (!query) all = rows;" ]

  -- Present-and-empty is a reader who took the filter off; absent is a page
  -- nobody has filtered yet, and only that one has the default injected over
  -- it.  Deleting the parameter is what made a cleared view come back filtered
  -- on the next remount, so the write is unconditional.
  , Glue "the applied query lives in the URL, an empty one included"
      [ "history.replaceState(null, \"\", `?${p.toString()}`);"
      , "p.set(\"q\", q);"
      -- `keys' rides in the same query string and has to survive a commit.
      , "new URLSearchParams(location.search)"
      , "const urlQuery = () => params().get(\"q\") || \"\";"
      -- A ?q= in the address bar is applied on load, and DEL strips it token
      -- by token through the renderer, default or not.
      , "const asked = (query = bootQuery());"
      , "table.stripLastToken()", "const left = table.getQuery().trim();"
      , "commit(left);" ]
      ["p.delete(\"q\")"]

  -- `/' asks the renderer to raise its palette instead of reaching for a box on
  -- the page: `openFilter' is mode-agnostic, so the one call covers an asset in
  -- any of them.  The renderer keeps `omnibox' for consumers that want the
  -- control resident; this shell is off it.
  , Glue "the filter is summoned rather than resident"
      [ "palette: true,"
      , "const summons = () => !!table && typeof table.openFilter === \"function\";"
      , "if (summons()) { table.openFilter(); return; }"
      -- An asset predating the call has a resident box; focus that.  The field
      -- is named once, since the fallback, the restore and the stash all want
      -- it and none of them may reach further into the renderer's chrome.
      , "const filterBox = () => document.querySelector(\"#app .tv-filter\");"
      , "const box = filterBox();"
      , "if (box) { box.focus(); box.select(); }"
      -- And the map says what the key does now, which is what the echo pill
      -- prints when it runs.
      , "summon the filter palette" ]
      ["omnibox: true,"]

  -- Marking is the renderer's: it draws the boxes, keys the marks by id and
  -- counts them, so this page holds no set of its own and asks for the count
  -- rather than keeping one.  What is the page's is dired's advance — the key
  -- that marks is the key that walks — and the rule that `u' is not a toggle.
  , Glue "marks are the renderer's, and m/u/U are this page's keys"
      -- What the keys DO is asserted by driving them, in "Shell marks"; the
      -- needles here are the two things behaviour cannot show. First, that the
      -- page asks the renderer for the count rather than deriving one, and
      -- reads its answer for the state a toggle landed in.
      [ "marks: true,"
      , "let on = table.toggleMark(id);"
      , "· ${table.markedCount()}`);"
      -- And that a flagged row's hint is the two keys that answer the flag,
      -- spelled here and drawn there.
      , "flagHelp: \"d/D archive · u unflag\"," ]
      -- And second, that no set, count or membership test is kept on this side.
      -- `getMarked()' is not one: a command asks the renderer which rows are
      -- marked at the moment it runs, which is the opposite of keeping a copy.
      ["let marked", "const marked = new Set", "marks.add", "marks.has"]

  -- The value palette's letters. What they DO is asserted by driving them, in
  -- "Shell which-key"; the needles here are the two things behaviour cannot
  -- show. First, that the rule lives in ONE pure function over the ordered
  -- labels — the display and the dispatch both read its answer, so a letter
  -- drawn and a letter honoured cannot drift.
  , Glue "the which-key letters are one pure function's answer"
      [ "function whichKeys(labels) {"
      , "function letterAt(label, at) {"
      -- Folded into each entry once and IN PLACE, so the table's cells and the
      -- flat list hold the same objects and the drawing and the dispatch read
      -- one field rather than agreeing on a parallel array's indices.
      , "        list[i].key = letterAt(list[i].label, cut);"
      -- A badge hue is written inline, so it has to be told to give way under
      -- the fallback's cursor row — `--g-sel' is a bright yellow in the light
      -- theme, and this is the one declaration on the page that outranks one.
      , "#plist .pat .pw{color:var(--g-fg)!important}"
      -- The claimed letter is drawn by WEIGHT, so it keeps the badge hue the
      -- rest of the word wears and puts no rule through the descenders.
      , "part(word, \"b\", \"\", c.label[c.cut]);"
      , ".pw b{font-weight:700}"
      -- And second, that both modes commit through one call, so the letter and
      -- the fallback's RET are the same delivery.
      , "else if (!e.repeat) takeChoice(hit);"
      , "else if (k === \"RET\") takeChoice(prompting.shown[prompting.at]);" ]
      -- No second copy of the assignment, no confirmation step behind a letter
      -- (the palette IS the confirmation), and no underline left behind.
      ["const LETTERS", "confirm(", ".pw u{", "part(word, \"u\""]

  -- The resolution table's chrome, which behaviour cannot show: the hairline
  -- between two source rows is that row's own top border rather than a divider
  -- element of its own, and the source column wears the muted small lowercase
  -- a tag wears everywhere else on this page.
  , Glue "the palette's hairlines are the table's own borders"
      [ ".pr{display:grid;grid-template-columns:6.5em 1fr 1fr"
      , ".pr+.pr{border-top:1px solid var(--g-border)}"
      , ".ph,.ps{font-size:11px;color:var(--g-mute)}"
      -- `*clear*' spans, since no source declares taking a keyword off.
      , ".pr.pm{grid-template-columns:1fr}" ]
      -- The flat list's divider went with the flat list, and so did the page's
      -- own idea of what the states are: the keywords are the server's answer.
      -- No cell coordinate on an entry either — a cell HOLDS its entries.
      [".psep", "stateChoices", "x.cell ===", "c.at ==="]

  -- The overlay is raised and dissolved by the renderer, whose own input stops
  -- ESC and DEL before this page's document handler sees them.  What keeps the
  -- shell's rows off the palette either way is `typing()': every `table' row is
  -- dead while a field has focus, and the one `any' row — ESC — closes the sheet
  -- and otherwise only blurs whatever is typing.
  , Glue "the palette's lifecycle stays the renderer's"
      [ "const live = (b) => b.scope === \"any\""
      , "|| (b.scope === \"table\" && !typing());"
      , "a.tagName === \"INPUT\" || a.tagName === \"TEXTAREA\""
      , "cancel: () => {"
      , "else if (typing()) document.activeElement.blur();" ]
      ["closeFilter", "tv-veil", "tv-panel"]

  -- With `bootstrap=off' no `set-rows' frame can arrive, so the branch that
  -- would have applied one is gone rather than left unreachable.
  , Glue "opens a socket and applies the streaming ops"
      [ "new WebSocket(", "/ws?bootstrap=off", "table.setRows("
      , "\"upsert-row\"", "table.upsertRow(", "\"delete-row\"", "table.deleteRow("
      -- Under a filter the rows are the server's answer to a query, so a row
      -- frame is re-asked for rather than spliced into them.
      , "setTimeout(fetchRows, 250)" ]
      ["\"set-rows\""]

  -- A close costs rows; only the columns moving costs the mount.  The
  -- reconnect revalidates the applied query against the tag the last answer
  -- carried, re-attaches, and leaves the page — sheet, palette, selection, URL
  -- — exactly where it was.  A dropped backlog under an editor's write storm
  -- arrives here, which is why a storm is a row refresh rather than a reload.
  , Glue "a close is a reconnect, and only view-changed is a remount"
      [ "socket.onclose = (e) => {"
      , "if (e && e.reason === \"view-changed\") remount(); else resync();"
      , "function resync() {"
      , "if (!table) { start(); return; }"
      , "load(asking(asked), etag)"
      , "if (a.view && query === asked) paint(a);"
      , "listen();"
      , "setTimeout(resync,", "Math.min(backoff * 2, 30000)"
      -- The revalidation is this page's, not the browser cache's, so the 304
      -- comes back as the answer it is.
      , "init.headers = { \"if-none-match\": tag }; init.cache = \"no-store\";"
      , "r.status === 304 ? { view: null, total: 0 }"
      , "etag = r.headers.get(\"ETag\") || etag;"
      -- A daemon restarted while the page was away had no socket to send
      -- `view-changed' down, so the columns are checked rather than trusted.
      , "if (a.view && !sameColumns(a.view.columns || [])) { remount(); return; }"
      , "const sameColumns = (next) => JSON.stringify(next) === JSON.stringify(cols);" ]
      -- The old door: every close went through the boot, which re-read the URL
      -- and rebuilt the mount.
      ["socket.onclose = () => {", "setTimeout(start,"]

  -- What a remount takes down goes back up: the palette with what was typed in
  -- it, the sheet with work the reader has not saved — both panes of it, in the
  -- shape it was showing.  The sheet's digest is re-read rather than remembered,
  -- so a file that moved underneath opens the conflict flow instead of being
  -- overwritten by the restore.
  , glue "a real remount carries the sheet and the palette across it"
      [ "function remount(after) { stash(); start(after); }"
      , "function stash() {"
      , "sheet: editing && dirty()"
      , "? { id: editing.id, raw, text: el(\"mtext\").value, props: props(),"
      , "digest: editing.digest }"
      , "palette: typedFilter(),"
      , "return box && document.activeElement === box ? box.value || \"\" : null;"
      , "function restore() {"
      , "if (box) { box.value = was.palette; box.focus(); }"
      , "if (was.sheet) reopen(was.sheet);"
      , "headline(s.id).then((h) => {"
      , "el(\"mtext\").value = s.text;"
      , "if (!s.raw) drawProps(s.props, s.plan);"
      , "if (h.digest !== s.digest) sync(\"conflict\");"
      -- The one place a new table appears, so the one place a restore belongs.
      , "restore();" ]

  -- A cold daemon answers the boot fetch with 503 while it walks the tree; the
  -- page it is answering is this one, so it says so and asks again.
  -- A cold daemon on the boot, and a restarted one under a live page: both
  -- poll through the reconnect, so the page a reader had is still on screen
  -- while the walk runs.
  , glue "shows the indexing state and polls out of it"
      [ "r.status === 503", "{ indexing: b }", "if (e.indexing) return indexing("
      , "indexing … ${b.elapsed}s", "setTimeout(resync, 1000)"
      , "dot(\"wait\")", "#dot.wait{" ]

  , glue "materializes a row and syncs it back"
      [ "\"materialize\"", "/headline?id=${encodeURIComponent(", "<textarea id=\"mtext\""
      , "method: \"POST\"", "flush(editing.digest)", "a.status === 409"
      -- The sheet's exits are keymap rows: ESC closes it, C-x C-s syncs it from
      -- inside the textarea.
      , "keyboard-quit", "C-x C-s" ]

  -- Two panes over one subtree, and the cut between them is the SERVER's: this
  -- page reads `body' and `properties' off the route and hands them back the
  -- same way.  Nothing here looks for a drawer in org text — there is no parser
  -- on this side, and C-c ' re-materializes rather than converting locally.
  , Glue "the sheet is a body pane and a property panel"
      [ "<div id=\"mpanes\">", "<div id=\"mprops\"></div>"
      , "base = raw ? h.org : h.body;"
      , "drawProps(raw ? [] : h.properties || [], raw ? [] : h.planning || []);"
      , "{ body: el(\"mtext\").value, properties: props(), planning: planning() }"
      -- The panel: a row is text until it is opened and fields while it is, `+'
      -- adds one, and the emptied key deletes.
      , "(prows[i].fixed ? \"prow pln\" : \"prow\") + (i === pcur ? \" pat\" : \"\"));"
      , "const e = document.createElement(open ? \"input\" : \"span\");"
      , "function addProperty() {"
      , "else if (k === \"+\") addProperty();"
      -- Trimmed both sides, since the server hands them over trimmed: what the
      -- panel can show is exactly what it can write.
      , "[r.key.trim(), r.val.trim()]"
      , ".filter((p) => p[0] !== \"\");"
      -- The logbook: shown, and out of everything a commit is made of.
      , "function drawLog(text) {"
      , "<pre id=\"mlog\"></pre>"
      -- Display-only: what goes back is the whole drawer, and this page never
      -- sends it at all.
      , ".split(\"\\n\").slice(1, -1).join(\"\\n\")"
      -- The toggle re-reads rather than converting, and refuses a dirty sheet.
      , "if (dirty()) { said(b, \"sync first — C-x C-s\"); return; }"
      , "headline(h.id).then((fresh) => {"
      -- The panel's own keys: TAB crosses the panes and hops an open row's two
      -- fields, nav movement is both spellings of the map's own letters and the
      -- arrows, and RET opens a row and commits it.
      , "const k = keyName(e), crossing = k === \"TAB\" || k === \"S-TAB\";"
      , "else if (k === \"<down>\" || k === \"n\" || k === \"j\") moveCur(1);"
      , "} else if (crossing) leavePanel();"
      , "pnav = true; el(\"mprops\").className = \"on\"; el(\"mtext\").blur();"
      -- Nav holds the keys with nothing focused, so the map has to be told —
      -- the value palette's letter mode is the other thing that does.
      , "return pnav || !!prompting"
      -- The panel stacks under the text when there is no room beside it, which
      -- is a wrap rather than a second breakpoint to keep in step.
      , "#mpanes{flex:1;min-height:0;display:flex;flex-wrap:wrap;gap:10px}"
      , "#sheet.raw #mprops{display:none}"
      -- A planning row's key is org's rather than the author's, and says so.
      , ".pln .pkey{color:var(--g-mute)}"
      -- And the panel reads as the table does: the same cell padding, the same
      -- stripe over the same ground, and the cursor row in the page's own
      -- selection rather than a tint of its own.  `:nth-child' is honest here
      -- where the renderer needs a stamped class — this list is not windowed.
      , ".prow input,.prow span{font:12px/1.5 var(--dk-mono);padding:5px 12px;"
      , "    display:flex;flex-direction:column}"
      , "#mprops .prow:nth-child(even){background:var(--g-surface)}"
      , "#mprops.on .pat{background:var(--g-sel);color:var(--g-fg)}"
      -- One hairline, at the group edge, drawn by the first property row so a
      -- drawer with none draws nothing.
      , ".prow.pln + .prow:not(.pln){border-top:1px solid var(--g-border)}" ]
      -- Field order is the DOM's: the fields are in the order the drawer writes
      -- them and nothing reorders them.  And no parser: the page never goes
      -- looking for a drawer in the text it holds.  Nor a row gap on either
      -- box: the stripe is the separation, and a gap would leave gutters
      -- through it.
      [ "tabindex", ":PROPERTIES:", ":END:"
      , "#mprops{flex:1 1 240px;min-width:0;overflow-y:auto;\n    display:flex;flex-direction:column;gap"
      , ".prow{display:flex;flex-wrap:wrap;gap" ]

  -- The author's Emacs theme in one set of custom properties: white on true
  -- black in the dark variant, black on white in the light one.  The hairline is
  -- the renderer's own `--tv-border', so the page draws one weight of chrome;
  -- danneskjold's own border faces frame instead.
  , Glue "the page wears danneskjold and the sheet wears Hack"
      [ "--g-bg:#FFFFFF;--g-fg:#000000;--g-border:#E3E6EA"
      , "@media (prefers-color-scheme:dark){:root{--g-bg:#000000;--g-fg:#FFFFFF;"
      , "--g-border:#2A2D3D;--g-mute:#A4C2EB;--g-surface:#21252B;--g-sel:#373D4F;"
      , "background:var(--g-bg);color:var(--g-fg)"
      , "#mtext::selection{background:var(--g-sel);color:var(--g-fg)}"
      , "#mnote.conflict,#mnote.error{color:var(--g-bad)}"
      , "border:1px solid var(--g-border)"
      -- The sheet asks for the author's Emacs font by name; the page keeps the
      -- stack it had.
      , "--dk-mono:\"Hack\", var(--glance-mono)"
      , "font:14px/1.5 var(--glance-mono)" ]
      ["--g-border:#BDC3C7", "--g-border:#223959"]

  -- One rule sets both widths, so the strip cannot drift from the table above
  -- it; the hairline, the radius and the surface tint are `.tv-root''s, which is
  -- what makes it read as the same thing.  The frame is resident, so an arriving
  -- event cannot shift the key line under it: the collapse, the hand-reserved
  -- line and the ten-line cap are all superseded designs the flex rule replaced,
  -- and a second limit here could only fight the column.
  , Glue "the log wears the table's container under it"
      [ "#app,#log{width:100%;box-sizing:border-box}"
      , "border:1px solid var(--g-border);border-radius:8px;"
      -- It takes the height the table and the key line leave, and scrolls
      -- inside it rather than at a cap of its own.
      , "background:var(--g-surface);flex:1 1 auto;overflow-y:auto}"
      -- The end of a long message is scrolled to unless the reader has scrolled
      -- up to hold a place.
      , "box.scrollTop + box.clientHeight >= box.scrollHeight - 4"
      , "if (end) box.scrollTop = box.scrollHeight;" ]
      ["#log:empty", "min-height:1.4em", "max-height:10em"]

  -- Connection, sync outcomes, the parity warning and errors: what a reader
  -- could not have seen otherwise.  The row count is the renderer's hint line
  -- and the keys are the resident key line's; the strip repeated both.
  , Glue "the log carries events and nothing the page shows anyway"
      [ "append(\"ws\", \"warn\", `disconnected · retrying in ${Math.round(backoff / 1000)}s`)"
      , "append(\"boot\", \"info\", `indexing … ${b.elapsed}s"
      , "append(\"ws\", \"error\", `load failed: ${e.message}`)"
      , "append(\"sync\", \"info\", \"closed without writing — the file is as it was\")"
      , "filter parity divergence — asset/daemon version skew"
      -- The boot line is a line like any other: the strip opens holding it and
      -- nothing takes it away, so the page's first second is still readable an
      -- hour later.
      , "<div id=\"log\"></div>"
      , "append(\"boot\", \"info\", \"loading …\");"
      ]
      -- The clearing dance is gone with the placeholder it existed to take
      -- away: an append-only strip has no way to say less than it has said.
      [ "const say = () =>", "say();", "getRows().length"
      , "matching ${query}", "${profile} keys"
      , "log(\"\")", "<div id=\"log\">loading …</div>" ]

  -- The strip's own machinery: a stamp, a bounded ring dropping from the front,
  -- and a repeat counted on the line it repeats rather than written under it.
  , glue "the log is a bounded ring of stamped lines"
      [ "const LOGCAP = 500;"
      , "new Date().toTimeString().slice(0, 8)"
      , "while (box.children.length > LOGCAP) box.removeChild(box.children[0]);"
      , "logLast.count.textContent = `×${(logLast.n += 1)}`;"
      -- A message is one line: whatever control characters it carries collapse.
      , "String(message).replace(/[\\x00-\\x1f]+/g, \" \")"
      -- The severity is the one part that changes colour, so a warning is
      -- findable in a screenful of chatter.
      , "#log .warn .lv{color:var(--g-warn)}"
      , "#log .error .lv{color:var(--g-bad)}" ]

  -- `table-view.js' gives its sticky header `z-index:1' and its completion list
  -- `5'; an unnumbered backdrop painted under both.  The page's own corner and
  -- echo stay below the backdrop and dim with everything else.
  , glue "the sheet's backdrop covers the renderer's chrome"
      [ "position:fixed;inset:0;z-index:100;", "position:relative;z-index:101;"
      , "#corner{position:fixed;top:12px;right:14px;z-index:3;"
      , "#echo{position:fixed;right:14px;bottom:12px;z-index:2;" ]

  , glue "the theme is a three-way switch the page honours"
      -- The selector and its three options.
      [ "<label for=\"themesel\">theme:</label>"
      , "<option value=\"auto\">auto</option><option value=\"light\">light</option>"
      , "<option value=\"dark\">dark</option>"
      -- `auto' is the media query; the other two pin the attribute the
      -- renderer's own overrides read.
      , ":root[data-theme=\"light\"]{--g-bg:#FFFFFF"
      , ":root[data-theme=\"dark\"]{--g-bg:#000000"
      , "if (name === \"auto\") delete document.documentElement.dataset.theme;"
      , "else document.documentElement.dataset.theme = name;"
      , "localStorage.getItem(\"glance-theme\")"
      , "localStorage.setItem(\"glance-theme\", v)"
      , "el(\"themesel\").addEventListener(\"change\""
      -- And the head applies it before anything paints.
      , "<script>try{var t=localStorage.getItem(\"glance-theme\");" ]

  , Glue "the dispatch and the echo widget read that blob and no other map"
      [ "<script id=\"keys\" type=\"application/json\">"
      , "JSON.parse(el(\"keys\").textContent)"
      , "MAPS.rows.filter(live)"
      , "HANDLERS[b.handler]" ]
      -- One map: no profile to remember, to ask for, or to offer.
      [ "MAPS.profiles", "MAPS.default", "glance-keys", "keysel", "setProfile" ]

  -- The tree's own default view, embedded by the daemon and applied by `g'
  -- through the ordinary commit path: into the URL, then asked of the server.
  , Glue "the default view is the tree's, and `g' applies it"
      [ "const DEFAULT_QUERY = "
      , "const bootQuery = () => (params().has(\"q\") ? urlQuery() : DEFAULT_QUERY);"
      , "function applyDefault(b) { applyView(b, DEFAULT_QUERY); }"
      , "remember(q);"
      , "remount();" ]
      -- `g' replaced the refresh key outright: one door through the mount.
      [ "function refresh()", "refreshing …", "org-glance-overview:refresh" ]

  -- The second canned view, applied through the same door and differing in one
  -- thing: it has something to do once its rows are up.
  , Glue "`a' is the agenda query through the same door, plus its own sort"
      [ "const AGENDA_QUERY = \"state:*active* -planned:none\";"
      , "applyAgenda: (b) => applyView(b, AGENDA_QUERY, (total) => landedAgenda(b, total)),"
      , "if (sorts()) table.sortBy(\"scheduled\", true);"
      , "said(b, `agenda · ${total} row${total === 1 ? \"\" : \"s\"}`);"
      -- The landing is an ARGUMENT of the boot it belongs to, so a boot that
      -- never lands cannot leave one behind for the next.
      , "function start(after) {"
      , "if (after) after(a.total);" ]
      -- A view rather than a mode: no state saying the agenda is on, no second
      -- sort order to keep in step with the view's own, and no variable this
      -- arms and disarms by hand.
      [ "agendaMode", "let agenda =", "sortKeys", "let landed" ]

  -- `o' follows the row.  The extraction is the server's — the page holds no
  -- org parser — and how many links come back decides the whole gesture.
  , Glue "`o' follows the row's links, and the server is what finds them"
      [ "const linksOf = (id) => getJSON(`/links?id=${encodeURIComponent(id)}`);"
      , "if (!links.length) { said(b, \"no links\"); return; }"
      , "if (links.length === 1) { openLink(b, links[0]); return; }"
      , "ask(`open · ${links.length} links`, (c) => openLink(b, c),"
      , "window.open(link.target, \"_blank\", \"noopener\");"
      , "append(\"cmd\", \"info\", `link ${JSON.stringify(link.target)} opened`);"
      -- The palette is raised behind the answer, so nothing is travelling.
      , "prompting.raising = false;"
      , "a letter opens it · / to search · ESC leaves"
      -- `/' narrows over the description AND the target.
      , "`${c.label} ${c.hint || \"\"}`.toLowerCase().includes(want)" ]
      -- No bracket grammar here: `[[T][D]]' is read where `displayText' is.
      [ "\\\\[\\\\[", "showLinks", "linkAt" ]

  , glue "a binding with no handler names what it is waiting for"
      [ "arrives with daemon commands (M4)" ]

  , glue "the echo widget is mounted, in Emacs wording"
      [ "<div id=\"echo\"", "#echo{position:fixed", "is undefined", "timed out"
      , "Enter: \"RET\"", "Escape: \"ESC\"", "ArrowUp: \"<up>\"" ]

  -- A row step is `selectStep': it carries the column and turns the page at
  -- either end, and `getVisible()' is one page's worth, so arithmetic over it
  -- here would stop dead at a boundary.  The index walk stays as the fallback
  -- for an asset predating the call — which has no pages either.  The selected
  -- row is then marked once, by the renderer's own secondary-highlight
  -- background: the accent stripe this page drew over it is a superseded design
  -- (#26), a second mark for the same fact.
  , Glue "row movement drives the renderer's own selection"
      [ "const steps = () => !!table && typeof table.selectStep === \"function\";"
      , "if (visible().length) table.selectStep(step);"
      -- Which row is on is the renderer's answer too, with the DOM read left as
      -- the fallback for an asset predating that call.
      , "tbody tr.tv-sel", "table.getVisible()", "table.select(id, column())", ".tv-filter"
      , "if (cells()) return table.getSelection().id;" ]
      [ "tr.click()", "scrollIntoView", "rowEls("
      , "box-shadow:inset 2px 0 0 var(--tv-accent)", "tr.tv-sel{box-shadow" ]

  , glue "the set is paged, and the brackets turn one"
      -- One number for the boot's limit and the renderer's page, so the first
      -- paint is exactly page one.
      [ "const PAGE = 100;   // rows in the first paint, and rows to a page"
      , "pageSize: PAGE,"
      , "load(`${narrow}limit=${PAGE}`)"
      -- The turn is the renderer's, and the bracket says where it landed:
      -- `] → next-page (page 3/129)'.
      , "nextPage: (b) => turnPage(b, 1),"
      , "previousPage: (b) => turnPage(b, -1),"
      , "if (step > 0) table.nextPage(); else table.previousPage();"
      , "said(b, `page ${at.page}/${at.pages}`);"
      -- An asset without a pager says so rather than throwing.
      , "typeof table.nextPage === \"function\""
      , "typeof table.pageInfo === \"function\""
      , "this table-view.js has no pager" ]

  -- The buffer ends climb: the page's end row first, and the same key again
  -- turns onto the next page's.  The landing is a select of its own in BOTH
  -- directions, since the renderer arrives at the far end of the page it turned
  -- to — and the column is read back out of the selection the turn kept rather
  -- than carried across in a local of this page's.
  , Glue "the buffer ends are progressive across pages"
      [ "firstRow: (b) => endStop(b, false),"
      , "lastRow: (b) => endStop(b, true),"
      , "const end = (rows) => rows[last ? rows.length - 1 : 0].id;"
      , "if (!pager() || focusedId() !== end(list)) {"
      , "if (!(last ? table.nextPage() : table.previousPage())) { said(b, \"\"); return; }"
      , "if (turned.length) table.select(end(turned), column());" ]
      -- The column stays the renderer's across a turn: no local carries it.
      ["const col = ", "let col = "]

  -- The column is the renderer's to hold: the shell reads it back out of
  -- `getSelection()' every time, which is why it survives a profile switch and
  -- goes when the selection does.  No second copy of it lives here.
  , Glue "cell movement is that selection with a column, and no state here"
      [ "const column = () => (cells() ? table.getSelection().col : null);"
      , "nextColumn: (b) => moveCol(b, 1),"
      , "previousColumn: (b) => moveCol(b, -1),"
      -- A whole-row selection has no column, and either direction lands on the
      -- first one from there.
      , "const at = column(), want = at === null ? 0 : at + step;"
      , "table.select(id, want)"
      -- An asset without cell selection says so rather than throwing.
      , "typeof table.getSelection === \"function\""
      , "this table-view.js has no cell selection"
      -- The row is handed to its handler so the echo can open the same way.
      , "if (handler) handler(b);" ]
      ["let col = ", "selCol", "lastColumn"]

  -- `f → next-column (Headline)', and `f → next-column (at last)' where the walk
  -- ran out of columns.
  , glue "the landing column is echoed by its header, or the edge it stopped at"
      [ "said(b, cols[want].header || cols[want].key);"
      , "said(b, want < 0 ? \"at first\" : \"at last\")"
      , "said(b, \"no row\")"
      -- The headers are the mounted view's, and parity cuts the keys out of the
      -- same list where it needs them.
      , "cols = view.columns || [];"
      , "const keys = cols.map((c) => c.key);" ]

  -- The rules: a finger's 44px, and a word saying what the row is while no chip
  -- has filled it.  The renderer hides an empty row with an inline
  -- `display:none', which `!important' outranks.
  , glue "a coarse pointer taps the chip row to summon the filter"
      [ "@media (pointer:coarse){"
      , "#app .tv-chips{min-height:44px;cursor:pointer}"
      , "#app .tv-chips:empty{display:flex!important;align-items:center}"
      , "content:\"filter …\""
      -- The handler: delegated from #app so it survives a re-mount, and through
      -- the same `focusFilter' the key runs.
      , "el(\"app\").addEventListener(\"click\""
      , "matchMedia(\"(pointer: coarse)\").matches"
      , "if (!coarse()) return;"
      , "t.closest(\".tv-chips\")"
      -- A tap on a chip is that chip's own removal and stays the renderer's.
      , "t.closest(\".tv-chip\")"
      , "focusFilter();" ]

  -- Under 16px, focusing a field zooms the page in and nothing zooms it back
  -- out.  The renderer's own input is the renderer's problem; the sheet's
  -- textarea and its property fields are this page's, and they keep their 12px
  -- everywhere else.  All of them in the one block, which is where every rule
  -- a touch device gets lives — the panes stacking there included.
  , glue "a coarse pointer gets fields iOS will not zoom into"
      [ "#mtext,#pinput,.prow input,.ctext,.cview{font-size:16px}}", "font:12px/1.5 var(--dk-mono)"
      , "#mpanes{flex-direction:column}" ]

  -- The keyboard-first exception, and the second one the page makes: `,'
  -- is the way into settings wherever there are keys, so the gear exists only
  -- where there are none.  It needs no `coarse()' of its own — the rule that
  -- shows it is inside the one block, and an element that is not displayed
  -- cannot be tapped.
  , glue "a coarse pointer gets a gear where the settings chord cannot be typed"
      [ "<button id=\"gear\" title=\"settings\">"
      , "#gear{display:none}"
      , "    #gear{display:inline-block;"
      , "min-width:44px;min-height:44px"
      , "el(\"gear\").addEventListener(\"click\", openSettings);" ]

  , glue "asks for one font stack, everywhere in the page"
      [ "--glance-mono:\"JetBrains Mono\", \"Fira Code\", \"SF Mono\", Menlo, Consolas, monospace"
      -- The renderer injects `.tv-root{font:…}' from its own script, which lands
      -- after this page's style element; the extra selector step wins.
      , "#app .tv-root{font-family:var(--glance-mono)}"
      , "font:14px/1.5 var(--glance-mono)", "font:12px/1.5 var(--dk-mono)"
      -- The sheet asks for the author's Emacs font first and falls back through
      -- the page's own stack, so there is still one list.
      , "--dk-mono:\"Hack\", var(--glance-mono)" ]

  -- The assets directory this shell is rendered against holds no font file, so
  -- the declaration must not be there to point at one.
  , Glue "with no font file to serve, says nothing about one" [] ["@font-face"]
  ]

-- | The window between @bind@ and the end of the startup walk.  The server
-- listens through it, so every route has an answer: the three that read the
-- store say they cannot yet, and the page that says so is served.
indexingSpec :: TestTree
indexingSpec = testGroup "Indexing (bind before load)"
  [ testCase "/headlines is a 503 that says when to come back" $ do
      application' <- indexingApp
      r <- getFrom application' "/headlines"
      assertEqual "status" 503 (status r)
      assertEqual "retry" (Just "1") (header "Retry-After" r)
      assertEqual "content type"
                  (Just "application/json; charset=utf-8") (header "Content-Type" r)
      loading <- decoded r
      assertEqual "loading" (Bool True) =<< field "loading" loading
      -- Seconds, rounded to a tenth: the shell prints them as `indexing … 0.3s',
      -- so a raw double would put fifteen digits on the page.
      elapsed <- field "elapsed" loading
      case elapsed of
        Number n -> do
          assertBool ("elapsed runs backwards: " <> show n) (n >= 0)
          assertEqual "elapsed is not rounded to a tenth"
                      (fromInteger (round (n * 10)) / 10) n
        other -> assertFailure ("expected a number of seconds, got " <> show other)
      -- And no query parameter makes the store readable early.
      q <- getFrom application' "/headlines?q=meeting&limit=10&offset=5"
      assertEqual "with parameters" 503 (status q)

  , testCase "materialize and commit wait for the load too" $ do
      application' <- indexingApp
      r <- getFrom application' (headlinePath "sample.org#0")
      assertEqual "GET /headline" 503 (status r)
      -- A commit before the load would be refused as a headline the file does
      -- have: the 503 is the honest answer, and the retriable one.
      w <- postTo application' (headlinePath "sample.org#0") (commitBody "* x\n" "deadbeef")
      assertEqual "POST /headline" 503 (status w)
      assertEqual "retry" (Just "1") (header "Retry-After" w)

  , testCase "/ws says the same, so a client reconnects rather than mounts" $ do
      application' <- indexingApp
      r <- getFrom application' "/ws"
      assertEqual "status" 503 (status r)

    -- The resolution is the store's — the rows it names and the config they
    -- were parsed under — so serving it early would answer for a row the walk
    -- has not reached with a chain it has not read.
  , testCase "/keywords waits for the store the rows come out of" $ do
      application' <- indexingApp
      r <- getFrom application' "/keywords?ids=sample.org%230"
      assertEqual "status" 503 (status r)
      assertEqual "retry" (Just "1") (header "Retry-After" r)

    -- The layer list comes off the store's own `clDirs' — the config
    -- directories the WALK met — so serving it early would answer with the
    -- fallback guess and hand a client digests for files it had not looked at.
  , testCase "/config waits for the walk, since the layers are what it found" $ do
      application' <- indexingApp
      r <- getFrom application' "/config"
      assertEqual "GET" 503 (status r)
      w <- postTo application' "/config" (configBody "/x.org" [] "")
      assertEqual "POST" 503 (status w)
      assertEqual "retry" (Just "1") (header "Retry-After" w)

  , testCase "the elapsed seconds are the load's age, rounded to a tenth" $ do
      -- The case above pins the shape against an age of microseconds, where
      -- every rounding agrees on 0.0; this one gives the load a real age, so
      -- the tenth is a digit that has to be there and the hundredth one that
      -- must not be.  12.37 s sits inside the [12.35, 12.45) bucket, leaving
      -- the in-process request 80 ms before the answer moves.
      hub <- newLoadingHub . subtract 12.37 =<< getMonotonicTime
      r <- getFrom (application (served assetsDir) hub) "/headlines"
      assertEqual "status" 503 (status r)
      elapsed <- field "elapsed" =<< decoded r
      assertEqual "elapsed" (Number 12.4) elapsed

  , testCase "the shell and its assets are served the whole time" $ do
      application' <- indexingApp
      r <- getFrom application' "/"
      assertEqual "status" 200 (status r)
      assertContains "the shell itself" "TableView.mount" (body r)
      js <- getFrom application' "/table-view.js"
      assertEqual "the renderer" 200 (status js)

  , testCase "the load landing opens the store routes, on the same server" $ do
      hub <- newLoadingHub =<< getMonotonicTime
      let application' = application (served assetsDir) hub
      before <- getFrom application' "/headlines"
      assertEqual "before" 503 (status before)
      finishLoading hub =<< loadStore viewDir
      after <- getFrom application' "/headlines"
      assertEqual "after" 200 (status after)
      -- That the body is the view the load produced is 'headlineSpec''s claim
      -- over the same directory; what is this case's is that the routes opened
      -- onto a loaded store rather than an empty one.
      assertEqual "the rows the walk found" (Just "6") (header "X-Glance-Rows" after)
      -- The tag is the loaded tree's, at the generation a store loaded at
      -- startup starts on.
      etagOf after >>= assertTreeTag "the store the walk landed" 0
  ]

-- | A server whose startup walk has not finished — the state 'Glance.Web.serve'
-- binds its socket in.
indexingApp :: IO Application
indexingApp = application (served assetsDir) <$> (newLoadingHub =<< getMonotonicTime)

-- | @\/headlines@ is the facade's view document — the same 'Value' 'viewJSON'
-- builds from the same directory, so the server adds nothing to the wire.
headlineSpec :: TestTree
headlineSpec = testGroup "GET /headlines"
  [ testCase "is the view JSON for the served directory, rendered from the store" $ do
      r <- get assetsDir "/headlines"
      expected <- viewJSON (viewTitleFor viewDir) . qrRecords <$> loadDir viewDir
      assertEqual "status" 200 (status r)
      got <- decoded r
      assertEqual "view" expected got

  , testCase "is UTF-8 JSON, and says so" $ do
      r <- get assetsDir "/headlines"
      assertEqual "content type"
                  (Just "application/json; charset=utf-8") (header "Content-Type" r)
      assertContains "unicode cell" "Привет мир" (body r)

  , testCase "carries one row per top entry" $ do
      r <- get assetsDir "/headlines"
      rows <- length . qrRecords <$> loadDir viewDir
      assertEqual "fixture rows" 6 rows
      assertEqual "X-Glance-Rows" (Just "6") (header "X-Glance-Rows" r)

  , testCase "serves JSON with the assets directory missing" $ do
      r <- get missingAssetsDir "/headlines"
      assertEqual "status" 200 (status r)
      assertEqual "X-Glance-Rows" (Just "6") (header "X-Glance-Rows" r)
  ]

-- | The load counts ride in headers; the body stays SCHEMA.md's field set.
-- | The startup banner.  Pure, so the one thing worth asserting about it costs
-- no server: the first line names the SUBCOMMAND the operator ran, since the
-- daemon `glance desktop' starts is the same one `glance serve' does and the
-- banner was the only place saying which.
bannerSpec :: TestTree
bannerSpec = testGroup "Startup banner"
  [ testCase "names the subcommand that started it" $ do
      assertEqual "under serve" "glance serve — http://127.0.0.1:7777/"
                  (head (bannerLines "serve" opts True))
      assertEqual "under desktop" "glance desktop — http://127.0.0.1:7777/"
                  (head (bannerLines "desktop" opts True))

    -- The rest of the banner is the same daemon's either way, and a missing
    -- renderer is the one thing that changes a line.
  , testCase "and says the same about the daemon under both" $ do
      assertEqual "every line but the first"
                  (tail (bannerLines "serve" opts True))
                  (tail (bannerLines "desktop" opts True))
      assertBool "a missing renderer is not reported"
                 ("(missing — /headlines only)" `isInfixOf`
                    unlines (bannerLines "serve" opts False))

    -- With no `--assets' there is no directory to name, and nothing that can
    -- go missing: the binary carries the renderer.
  , testCase "and says where the renderer came from" $ do
      assertEqual "under --assets" "  assets:  /a"
                  (bannerLines "serve" opts True !! 2)
      assertEqual "without it" "  assets:  compiled in (--assets serves a directory instead)"
                  (bannerLines "serve" opts { soAssets = Nothing } True !! 2)
  ]
  where opts = ServeOptions { soDir = "/o", soPort = defaultPort
                            , soAssets = Just "/a", soDerived = False }

statsSpec :: TestTree
statsSpec = testGroup "Load stats"
  [ testCase "report what the walk covered" $ do
      r <- get assetsDir "/headlines"
      assertEqual "files" (Just "2") (header "X-Glance-Files" r)
      assertEqual "parse failures" (Just "0") (header "X-Glance-Parse-Failures" r)
      assertEqual "decode failures" (Just "1") (header "X-Glance-Decode-Failures" r)
      assertEqual "read failures" (Just "0") (header "X-Glance-Read-Failures" r)
      assertEqual "id collisions" (Just "0") (header "X-Glance-Id-Collisions" r)

  , testCase "count the rows two files claimed one id for" $ withTempDir $ \dir -> do
      -- The org-glance shape: a canonical store and a mirror of it.  Both are
      -- named here, so the walk's own exclusion is not what is under test.
      let shared = "* TODO one\n:PROPERTIES:\n:ORG_GLANCE_ID: shared-id\n:END:\n"
      _ <- orgFile dir "canonical.org" shared
      _ <- orgFile dir "mirror.org" shared
      (a, _hub) <- serverOver dir
      r <- getFrom a "/headlines"
      assertEqual "one row per id" 1 . length =<< rowsOf r
      assertEqual "and it says how many it chose between"
                  (Just "1") (header "X-Glance-Id-Collisions" r)

  , testCase "leave the view document's field set alone" $ do
      v <- get assetsDir "/headlines" >>= decoded
      case v of
        Object o -> assertEqual "top-level keys"
                                ["actions", "columns", "rows", "sort", "title"]
                                (sort (map Key.toText (KM.keys o)))
        _        -> assertFailure ("expected an object, got " <> show v)
  ]

-- | The @ETag@ is the tree's fingerprint and the store's generation: which
-- documents were loaded, and how far they have moved since.  The watcher moves
-- the generation; a restart moves the fingerprint or leaves it, which is the
-- half a client's cached copy is revalidated against across one.  Every query
-- variant shares the tag — the parameters are in the URL, and an HTTP cache is
-- keyed by URL, so each variant revalidates against the tag it was itself
-- given.
cacheSpec :: TestTree
cacheSpec = testGroup "GET /headlines cache validation"
  [ testCase "carries a tree tag and a generation, and says to revalidate" $ do
      r <- get assetsDir "/headlines"
      etagOf r >>= assertTreeTag "the fixture store" 0
      assertEqual "Cache-Control" (Just "no-cache") (header "Cache-Control" r)

  , testCase "the tag it just gave out is a 304 with no body" $ do
      a <- app assetsDir
      first' <- getFrom a "/headlines"
      let tag = fromMaybe "" (header "ETag" first')
      again <- getWith a "/headlines" [("If-None-Match", tag)]
      assertEqual "status" 304 (status again)
      assertEqual "body" "" (simpleBody again)
      assertEqual "the tag comes back" (Just tag) (header "ETag" again)
      -- Nothing else is owed on a 304, and Content-Type least of all.
      assertEqual "no content type" Nothing (header "Content-Type" again)

  , testCase "a weak tag, or one in a list, still matches" $ do
      a <- app assetsDir
      tag <- etagOf =<< getFrom a "/headlines"
      weak <- getWith a "/headlines" [("If-None-Match", "W/" <> tag)]
      listed <- getWith a "/headlines" [("If-None-Match", "\"" <> zeroes <> "-g9\", " <> tag)]
      assertEqual "weak" 304 (status weak)
      assertEqual "listed" 304 (status listed)

  , testCase "a tag from another generation is the whole document again" $ do
      a <- app assetsDir
      tag <- etagOf =<< getFrom a "/headlines"
      r <- getWith a "/headlines" [("If-None-Match", atGeneration 7 tag)]
      assertEqual "status" 200 (status r)
      assertEqual "X-Glance-Rows" (Just "6") (header "X-Glance-Rows" r)

  , testCase "and so is one from another tree at this very generation" $ do
      -- The restart: a client holding the tag a daemon gave out before it was
      -- restarted over a tree that has changed since.  The generation is back
      -- at zero and says nothing about that, so the fingerprint is the whole of
      -- what refuses the 304 — with the generation alone, both tags read "g0"
      -- and the client keeps a table that is nowhere any more.
      a <- app assetsDir
      tag <- etagOf =<< getFrom a "/headlines"
      let elsewhere = "\"" <> zeroes <> "-g0\""
      assertBool "the fixture tree prints as all zeroes" (tag /= elsewhere)
      stale <- getWith a "/headlines" [("If-None-Match", elsewhere)]
      fresh <- getWith a "/headlines" [("If-None-Match", tag)]
      assertEqual "another tree, same generation" 200 (status stale)
      assertEqual "this tree" 304 (status fresh)

  , testCase "a store the watch moved is a fresh tag" $ withTempDir $ \dir -> do
      path <- orgFile dir "notes.org" committable
      (a, hub) <- serverOver dir
      before <- getFrom a "/headlines"
      let tag = fromMaybe "" (header "ETag" before)
      -- The watch's own step, taken here without a watcher: re-load the file
      -- and publish it, which is the one path that updates the store.
      _ <- orgFile dir "notes.org" (committable <> "* TODO Third\n")
      outcome <- loadFile path
      _ <- publish hub (applyFile path outcome)
      after <- getWith a "/headlines" [("If-None-Match", tag)]
      assertEqual "status" 200 (status after)
      assertBool "the tag moved with the store"
                 (header "ETag" after /= Just tag)
      assertEqual "and the new row is in it" (Just "3") (header "X-Glance-Rows" after)

  , testCase "a re-load that changes nothing leaves the tag where it was" $
      withTempDir $ \dir -> do
        path <- orgFile dir "notes.org" committable
        (a, hub) <- serverOver dir
        before <- getFrom a "/headlines"
        outcome <- loadFile path
        _ <- publish hub (applyFile path outcome)
        after <- getFrom a "/headlines"
        assertEqual "the tag" (header "ETag" before) (header "ETag" after)

  , testCase "one tag serves every variant, which the URL keeps apart" $ do
      a <- app assetsDir
      full <- getFrom a "/headlines"
      paged <- getFrom a "/headlines?limit=2"
      filtered <- getFrom a "/headlines?q=table"
      assertEqual "the paged tag" (header "ETag" full) (header "ETag" paged)
      assertEqual "the filtered tag" (header "ETag" full) (header "ETag" filtered)
      -- The bodies differ, which is what the distinct URLs are for.
      assertBool "one URL's answer served for another"
                 (simpleBody full /= simpleBody paged)
  ]

-- | Compression: on for the text this server sends, off for a body too small
-- to gain by it, and always with the @Vary@ that keeps the two encodings from
-- being confused for each other.
gzipSpec :: TestTree
gzipSpec = testGroup "Compression"
  [ testCase "the view JSON is gzipped for a client that asks" $ do
      a <- app assetsDir
      plain' <- getFrom a "/headlines"
      zipped <- getWith a "/headlines" [("Accept-Encoding", "gzip")]
      assertEqual "status" 200 (status zipped)
      assertEqual "Content-Encoding" (Just "gzip") (header "Content-Encoding" zipped)
      assertBool "compressed to no less than it was"
                 (BL.length (simpleBody zipped) < BL.length (simpleBody plain'))

  , testCase "and left alone for a client that does not" $ do
      r <- get assetsDir "/headlines"
      assertEqual "Content-Encoding" Nothing (header "Content-Encoding" r)
      assertContains "the body is JSON" "\"rows\"" (body r)

  , testCase "every answer varies on the encoding, 304s included" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines"
      notModified <- getWith a "/headlines"
        [("If-None-Match", fromMaybe "" (header "ETag" r))]
      assertEqual "on the 200" (Just "Accept-Encoding") (header "Vary" r)
      assertEqual "on the 304" (Just "Accept-Encoding") (header "Vary" notModified)

  , testCase "a body under the threshold is not worth compressing" $ do
      a <- app assetsDir
      r <- getWith a "/headline" [("Accept-Encoding", "gzip")]
      assertEqual "status" 400 (status r)
      assertBool "the error JSON is small" (BL.length (simpleBody r) < 860)
      assertEqual "Content-Encoding" Nothing (header "Content-Encoding" r)

  , testCase "the renderer is compressed too, though it is a file" $ do
      a <- app assetsDir
      r <- getWith a "/table-view.js" [("Accept-Encoding", "gzip")]
      assertEqual "status" 200 (status r)
      assertEqual "Content-Encoding" (Just "gzip") (header "Content-Encoding" r)
  ]

-- | @q@, @limit@ and @offset@: filter first, then page, and report what the
-- page covers in the header family the load counts already use.
querySpec :: TestTree
querySpec = testGroup "GET /headlines filter and paging"
  [ testCase "no parameters is the whole set, as it always was" $ do
      r <- get assetsDir "/headlines"
      assertEqual "X-Glance-Total" (Just "6") (header "X-Glance-Total" r)
      assertEqual "X-Glance-Has-Next" (Just "false") (header "X-Glance-Has-Next" r)
      assertEqual "rows" 6 . length =<< rowsOf r

  , testCase "q narrows on the row as it displays, case-insensitively" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?q=SHIP%20THE%20TABLE"
      assertEqual "X-Glance-Total" (Just "1") (header "X-Glance-Total" r)
      ids <- map rowId <$> rowsOf r
      assertEqual "the matching row" ["ship-table-view"] ids

  , testCase "q matches a bracket link by its description, not its target" $
      withTempDir $ \dir -> do
        -- What the row shows is what a filter searches, the way the renderer
        -- searches its own cached display text (table-view.js `displayText').
        _ <- orgFile dir "links.org"
               "* TODO Read [[file:table-view/SCHEMA.md][the schema]]\n"
        (a, _hub) <- serverOver dir
        shown <- getFrom a "/headlines?q=the%20schema"
        target <- getFrom a "/headlines?q=SCHEMA.md"
        assertEqual "the description matches" (Just "1") (header "X-Glance-Total" shown)
        assertEqual "the target does not" (Just "0") (header "X-Glance-Total" target)

  , testCase "q matching nothing is an empty page under a 200" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?q=no-such-headline-anywhere"
      assertEqual "status" 200 (status r)
      assertEqual "X-Glance-Total" (Just "0") (header "X-Glance-Total" r)
      assertEqual "rows" 0 . length =<< rowsOf r

  , testCase "q reaches the filter grammar intact" $ do
      a <- app assetsDir
      let total path = fmap TE.decodeUtf8 . header "X-Glance-Total" <$> getFrom a path
      -- This route's subject is transport: TestFilter is the grammar's home and
      -- states every rule over the same fixture.  What is this route's own
      -- contract is that a `?q=' arrives at the parser as it was typed, so three
      -- shapes are enough — a predicate, a negation of one, and org cell text
      -- carrying the separator that must not become one.
      assertEqual "a predicate" (Just "1") =<< total "/headlines?q=state:DONE"
      assertEqual "a negation drops what it matches" (Just "5")
        =<< total "/headlines?q=-state:DONE"
      assertEqual "a tag string stays text" (Just "2") =<< total "/headlines?q=:web:"

    -- The default view over a tree holding one of each: the boot query is
    -- `state:*active*' and the row it exists to show is the one nobody has put
    -- a keyword on, which the active group takes along with the keywords it
    -- names.  TestFilter states the rule; what this asserts is the rule
    -- arriving through the route the shell actually boots on.
  , testCase "the default view carries the entry nobody stated" $
      withTempDir $ \dir -> do
        _ <- orgFile dir "notes.org" (T.unlines
               [ "* TODO Ship it", "* DONE Shipped", "* Jotted and never stated" ])
        (a, _hub) <- serverOver dir
        let titles path = fmap sort . mapM (textAt "title" <=< field "cells")
                            =<< rowsOf =<< getFrom a path
        assertEqual "which query the shell boots on" "state:*active*" builtinFilter
        assertEqual "the active group takes the stateless entry with the keyword"
                    ["Jotted and never stated", "Ship it"]
          =<< titles "/headlines?q=state%3A*active*"
        assertEqual "the inactive group leaves it behind" ["Shipped"]
          =<< titles "/headlines?q=state%3A*inactive*"
        assertEqual "none is that one entry, asked for by name"
                    ["Jotted and never stated"] =<< titles "/headlines?q=state%3Anone"
        assertEqual "so negating the default view drops it too" ["Shipped"]
          =<< titles "/headlines?q=-state%3A*active*"

  , testCase "a filtered OR query pages out of the view's own sort" $ do
      a <- app assetsDir
      whole <- rowsOf =<< getFrom a "/headlines?q=state:active"
      one <- getFrom a "/headlines?q=state:active&limit=2&offset=0"
      two <- getFrom a "/headlines?q=state:active&limit=2&offset=2"
      -- Three keywords in the file's active set, plus the stateless row the
      -- group takes with them (TestFilter, "the stateless row is active").
      assertEqual "the union" 4 (length whole)
      assertEqual "the total is the match count, not the page" (Just "4")
                  (header "X-Glance-Total" one)
      let sorted = map rowId (sortOn scheduledOf whole)
      assertEqual "page one" (take 2 sorted) . map rowId =<< rowsOf one
      assertEqual "page two" (drop 2 sorted) . map rowId =<< rowsOf two
      assertEqual "more follows page one" (Just "true") (header "X-Glance-Has-Next" one)
      assertEqual "nothing follows page two" (Just "false") (header "X-Glance-Has-Next" two)

  , testCase "limit cuts a page out of the view's own sort" $ do
      a <- app assetsDir
      whole <- rowsOf =<< getFrom a "/headlines"
      page <- rowsOf =<< getFrom a "/headlines?limit=3"
      assertEqual "page size" 3 (length page)
      -- The page is the first three by scheduled ascending, which is what the
      -- view declares — not the first three the walk found.
      assertEqual "the sort the view declares"
                  (take 3 (map rowId (sortOn scheduledOf whole)))
                  (map rowId page)

  , testCase "offset walks the pages, and has-next says when to stop" $ do
      a <- app assetsDir
      whole <- map rowId . sortOn scheduledOf <$> (rowsOf =<< getFrom a "/headlines")
      one <- getFrom a "/headlines?limit=4&offset=0"
      two <- getFrom a "/headlines?limit=4&offset=4"
      past <- getFrom a "/headlines?limit=4&offset=6"
      assertEqual "page one" (take 4 whole) . map rowId =<< rowsOf one
      assertEqual "page two" (drop 4 whole) . map rowId =<< rowsOf two
      assertEqual "more follows page one" (Just "true") (header "X-Glance-Has-Next" one)
      assertEqual "nothing follows page two" (Just "false") (header "X-Glance-Has-Next" two)
      assertEqual "past the end is empty" 0 . length =<< rowsOf past
      assertEqual "and says so" (Just "false") (header "X-Glance-Has-Next" past)

  , testCase "the filter runs before the page, so the total is the match count" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?q=e&limit=2&offset=1"
      matched <- length <$> (rowsOf =<< getFrom a "/headlines?q=e")
      assertEqual "the total is what matched" (Just (T.pack (show matched)))
                  (fmap TE.decodeUtf8 (header "X-Glance-Total" r))
      assertBool "the fixture would not exercise the arithmetic" (matched > 3)
      page <- rowsOf r
      assertEqual "the page is a slice of it" 2 (length page)
      assertEqual "and more follows" (Just "true") (header "X-Glance-Has-Next" r)

  , testCase "the state palette is the store's, whatever the page holds" $ do
      a <- app assetsDir
      whole <- badgeValues =<< decoded =<< getFrom a "/headlines"
      page <- badgeValues =<< decoded =<< getFrom a "/headlines?limit=1"
      none <- badgeValues =<< decoded =<< getFrom a "/headlines?q=no-such-headline"
      assertEqual "the paged palette" whole page
      assertEqual "the empty page's palette" whole none
      assertBool "the fixture declares keywords" (length whole > 2)

  , testCase "a limit past the cap is refused, and named" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?limit=20001"
      ok <- getFrom a "/headlines?limit=20000"
      assertEqual "over" 400 (status r)
      assertContains "the cap" "20000" (body r)
      assertEqual "at the cap" 200 (status ok)

  , testCase "a parameter that is not a number is a 400 saying which" $ do
      a <- app assetsDir
      mapM_ (\(path, named) -> do
               r <- getFrom a path
               assertEqual (show path <> " status") 400 (status r)
               assertContains "names the parameter" named (body r))
            [ ("/headlines?limit=lots", "limit")
            , ("/headlines?limit=-1", "limit")
            , ("/headlines?offset=x", "offset")
            , ("/headlines?offset=-3", "offset") ]

  , testCase "a bare parameter reads as an absent one" $ do
      a <- app assetsDir
      r <- getFrom a "/headlines?limit&q"
      assertEqual "status" 200 (status r)
      assertEqual "rows" 6 . length =<< rowsOf r
  ]

-- | @order=document@ — EXPERIMENTAL, and the only thing that reaches it is a
-- typed URL.  It moves both halves of the ordering at once: the rows stay in
-- walk order under a limit, and the view carries no @sort@ field for a renderer
-- to re-apply.  What it orders is top entries, so it is the order the files
-- list them in rather than an outline.
orderSpec :: TestTree
orderSpec = testGroup "GET /headlines?order=document"
  [ testCase "the default still declares the view's sort" $ do
      v <- get assetsDir "/headlines" >>= decoded
      fieldsOf v >>= assertBool "no sort field" . elem "sort"

  , testCase "and so does naming it" $ do
      v <- get assetsDir "/headlines?order=scheduled" >>= decoded
      fieldsOf v >>= assertBool "no sort field" . elem "sort"

  , testCase "document order declares none at all" $ do
      v <- get assetsDir "/headlines?order=document" >>= decoded
      assertEqual "top-level keys" ["actions", "columns", "rows", "title"]
        . sort =<< fieldsOf v

  , testCase "and the page it cuts is walk order, where the default's is sorted" $ do
      a <- app assetsDir
      walk <- map rowId <$> (rowsOf =<< getFrom a "/headlines")
      byDate <- map rowId <$> (rowsOf =<< getFrom a "/headlines?limit=3")
      doc <- map rowId <$> (rowsOf =<< getFrom a "/headlines?order=document&limit=3")
      assertEqual "the walk's first three" (take 3 walk) doc
      -- Without this the case would pass over a fixture whose two orders agree.
      assertBool ("the fixture cannot tell them apart: " <> show byDate)
                 (byDate /= doc)

  , testCase "anything else under order is a 400 naming it" $ do
      a <- app assetsDir
      mapM_ (\path -> do
               r <- getFrom a path
               assertEqual (show path <> " status") 400 (status r)
               assertContains "names the parameter" "order" (body r))
            ["/headlines?order=walk", "/headlines?order=Document", "/headlines?order="]
  ]

-- | @\/ws?bootstrap=off@: the opening @set-rows@ dropped for a client that
-- fetched the rows over HTTP.  Checked on the parser, since the suite binds no
-- socket — the decision is the whole of what the query controls.
bootstrapSpec :: TestTree
bootstrapSpec = testGroup "Socket bootstrap control"
  [ testCase "is wanted by default, and by every query but the one" $
      mapM_ (\path -> assertBool (show path <> " skipped the bootstrap")
                                 (bootstrapWanted path))
            ["/ws", "/ws?", "/ws?keys=vim", "/ws?bootstrap=on", "/ws?bootstrap="]

  , testCase "bootstrap=off drops it, wherever it sits in the query" $
      mapM_ (\path -> assertBool (show path <> " still sent the bootstrap")
                                 (not (bootstrapWanted path)))
            ["/ws?bootstrap=off", "/ws?keys=vim&bootstrap=off", "/ws?bootstrap=off&x=1"]
  ]

-- | @GET \/headline@: one subtree out of the read model, with the coordinates
-- a write back to it needs.
materializeSpec :: TestTree
materializeSpec = testGroup "GET /headline"
  [ testCase "is the raw subtree, the file it came from and its digest" $ do
      (a, _hub) <- serverOver viewDir
      r <- getFrom a (headlinePath "ship-table-view")
      assertEqual "status" 200 (status r)
      assertEqual "content type"
                  (Just "application/json; charset=utf-8") (header "Content-Type" r)
      v <- decoded r
      org <- textAt "org" v
      rid <- textAt "id" v
      file <- textAt "file" v
      digest <- textAt "digest" v
      assertEqual "id" "ship-table-view" rid
      assertEqual "file" (T.pack sampleFile) file
      assertEqual "digest" sampleDigest digest
      assertEqual "org" (T.unlines [ "* NEXT [#A] Ship the table view :web:glance:"
                                   , "SCHEDULED: <2026-08-01 Sat 09:30> DEADLINE: <2026-08-05 Wed>"
                                   , ":PROPERTIES:"
                                   , ":ORG_GLANCE_ID: ship-table-view"
                                   , ":END:" ])
                  org

  , testCase "the org text is exactly the span the response reports" $ do
      (a, _hub) <- serverOver viewDir
      v <- getFrom a (headlinePath "ship-table-view") >>= decoded
      org <- textAt "org" v
      extent <- field "span" v
      start <- intAt "start" extent
      end <- intAt "end" extent
      doc <- document sampleFile
      assertEqual "slice" (T.take (end - start) (T.drop start doc)) org
      -- The extent starts at the stars, so the file's own header belongs to no
      -- subtree and a commit cannot take it with the headline.
      assertEqual "the preamble sits ahead of the first subtree"
                  "#+CATEGORY: sample\n#+TODO: NEXT WAITING | CANCELLED\n\n" (T.take start doc)

    -- Rows are top entries, so a child has no row of its own and materialize is
    -- the whole of how a client reaches one.  What comes back is the outline,
    -- children included — which is the claim the filtering rests on.
  , testCase "a top entry materializes with its children in it" $ withTempDir $ \dir -> do
      let doc = T.unlines [ "* TODO parent", ":PROPERTIES:", ":ORG_GLANCE_ID: top"
                          , ":END:", "** child", "child body", "*** grandchild" ]
      _ <- orgFile dir "tree.org" doc
      (a, _hub) <- serverOver dir
      assertEqual "one row for the file" 1 . length =<< rowsOf =<< getFrom a "/headlines"
      v <- getFrom a (headlinePath "top") >>= decoded
      assertEqual "the whole outline" doc =<< textAt "org" v
      -- A child's drawer is body text here, so the split leaves the descendants
      -- in the pane a client edits.
      assertEqual "and the body keeps them"
                  (T.unlines ["* TODO parent", "** child", "child body", "*** grandchild"])
                  =<< textAt "body" v

    -- A row id with no ORG_GLANCE_ID is FILE#K, so it carries slashes and a
    -- HASH.  The hash is the one that would bite: spelled into a URL raw it
    -- opens a fragment and the id arrives truncated at the first slash-free
    -- half of it.  The query string plus percent-encoding is what makes it a
    -- non-issue, on this side and in the shell (`encodeURIComponent').
  , testCase "an id carrying a hash and slashes round-trips" $ do
      (a, _hub) <- serverOver viewDir
      let rid = T.pack sampleFile <> "#1"
      r <- getFrom a (headlinePath rid)
      assertEqual "status" 200 (status r)
      v <- decoded r
      back <- textAt "id" v
      org <- textAt "org" v
      assertEqual "id" rid back
      assertContains "subtree" "Привет мир" org

    -- The same subtree, split: the drawer lifted out of the text and named
    -- beside it, so a client can edit the two apart without an org parser.  The
    -- whole `org' rides along untouched — the split is an addition, not a
    -- replacement.
  , testCase "the drawer arrives beside the body, lifted out of it" $ do
      (a, _hub) <- serverOver viewDir
      v <- getFrom a (headlinePath "ship-table-view") >>= decoded
      assertEqual "the body is the subtree with every region's lines gone"
                  (T.unlines ["* NEXT [#A] Ship the table view :web:glance:"])
                  =<< textAt "body" v
      -- The one property this drawer holds is the identity, which is the
      -- server's: the pane a client edits is empty and the file still has it.
      assertEqual "and the hidden one is not offered" [] =<< pairsAt "properties" v
      assertEqual "the planning line arrives as entries"
                  [ ["SCHEDULED", "<2026-08-01 Sat 09:30>"]
                  , ["DEADLINE", "<2026-08-05 Wed>"] ]
                  =<< pairsAt "planning" v
      assertEqual "and there is no logbook here" "" =<< textAt "logbook" v
      assertContains "while org is still the whole subtree" ":PROPERTIES:" =<< textAt "org" v

  , testCase "a headline with no drawer is all body and no pairs" $ do
      (a, _hub) <- serverOver viewDir
      v <- getFrom a (headlinePath (T.pack sampleFile <> "#1")) >>= decoded
      assertEqual "the body is the subtree, its planning line lifted out"
                  "* TODO [#B] Привет мир :unicode:\n" =<< textAt "body" v
      assertEqual "with nothing to show beside it" [] =<< pairsAt "properties" v
      assertEqual "and the planning line named apart"
                  [["SCHEDULED", "<2026-08-03 Mon>"]] =<< pairsAt "planning" v

  , testCase "an id no row carries is a 404" $ do
      (a, _hub) <- serverOver viewDir
      r <- getFrom a (headlinePath "no-such-headline")
      assertEqual "status" 404 (status r)
      assertContains "hint" "no headline with id" (body r)

  , testCase "no id at all says what the route wants" $ do
      (a, _hub) <- serverOver viewDir
      r <- getFrom a "/headline"
      assertEqual "status" 400 (status r)
      -- The whole hint: `id=' on its own rides every id-bearing URL the page
      -- builds, so a body naming the method and the parameter is what tells a
      -- client with the wrong URL what to write.
      assertContains "hint" "GET /headline?id=<row id>" (body r)
  ]

-- | @POST \/headline@: the subtree written back, and every way that is refused.
commitSpec :: TestTree
commitSpec = testGroup "POST /headline"
  [ testCase "writes the edited subtree and leaves the rest of the file alone" $
      withCommitted $ \a path v -> do
        org <- textAt "org" v
        digest <- textAt "digest" v
        before <- document path
        extent <- field "span" v
        start <- intAt "start" extent
        end <- intAt "end" extent
        let edited = T.replace "TODO First" "DONE First" org <> "an added line\n"
        r <- postTo a (headlinePath "first") (commitBody edited digest)
        assertEqual "status" 200 (status r)
        after <- document path
        assertEqual "the file is prefix + new subtree + suffix"
                    (T.take start before <> edited <> T.drop end before) after
        assertContains "the edit landed" "* DONE First" after
        assertContains "the next headline is untouched" "* TODO Second\ntail\n" after
        fresh <- textAt "digest" =<< decoded r
        expected <- digestOnDisk path
        assertEqual "the reported digest is the file's" expected fresh

  , testCase "leaves the store alone — the watch is what updates rows" $
      withCommitted $ \a path before -> do
        org <- textAt "org" before
        digest <- textAt "digest" before
        r <- postTo a (headlinePath "first") (commitBody (org <> "a line\n") digest)
        assertEqual "status" 200 (status r)
        -- No watcher runs in this suite, so the store still holds the load it
        -- started with: the route wrote to the file and to nothing else.
        after <- decoded =<< getFrom a (headlinePath "first")
        assertEqual "the store's subtree" (Just org) . Just =<< textAt "org" after
        assertEqual "the store's digest" (Just digest) . Just =<< textAt "digest" after
        onDisk <- digestOnDisk path
        assertBool "the file was not written" (onDisk /= digest)

  , testCase "a file rewritten behind the client is a conflict, and stays as it is" $
      withCommitted $ \a path v -> do
        org <- textAt "org" v
        digest <- textAt "digest" v
        let meddled = committable <> "* TODO Someone else\n"
        TIO.writeFile path meddled
        r <- postTo a (headlinePath "first") (commitBody (org <> "mine\n") digest)
        assertEqual "status" 409 (status r)
        conflict <- decoded r
        reason <- textAt "reason" conflict
        assertEqual "reason" "drift" reason
        assertContains "the message says to materialize again" "materialize" (body r)
        after <- document path
        assertEqual "the file is the meddler's" meddled after

  , testCase "a digest the store no longer holds is a conflict too" $
      withCommitted $ \a path v -> do
        org <- textAt "org" v
        let stale = T.replicate 64 "0"
        r <- postTo a (headlinePath "first") (commitBody org stale)
        assertEqual "status" 409 (status r)
        conflict <- decoded r
        reason <- textAt "reason" conflict
        current <- textAt "digest" conflict
        assertEqual "reason" "stale" reason
        assertEqual "the digest to re-materialize with" (Just current) . Just
          =<< textAt "digest" v
        after <- document path
        assertEqual "untouched" committable after

    -- The other shape of the same write: the body and the drawer named apart,
    -- composed here.  What it buys is exactly the byte rule — the property
    -- nobody touched goes back as the line it came in on.
  , testCase "the split shape writes the same subtree, verbatim where nothing moved" $
      withCommitted $ \a path v -> do
        digest <- textAt "digest" v
        body' <- textAt "body" v
        props <- pairsAt "properties" v
        r <- postTo a (headlinePath "first")
               (splitBody body' (props <> [["EFFORT", "0:30"]]) digest)
        assertEqual "status" 200 (status r)
        after <- document path
        assertEqual "the drawer, with the addition and nothing else re-spelled"
                    (T.unlines [ "* TODO First :one:", ":PROPERTIES:"
                               , ":ORG_GLANCE_ID: first", ":EFFORT: 0:30", ":END:"
                               , "body of first" ])
                    (T.unlines (take 6 (drop 1 (T.lines after))))
        assertContains "and the next headline is untouched" "* TODO Second\ntail\n" after

  , testCase "an emptied properties list takes the drawer away" $
      withCommitted $ \a path v -> do
        digest <- textAt "digest" v
        body' <- textAt "body" v
        r <- postTo a (headlinePath "first") (splitBody body' [] digest)
        assertEqual "status" 200 (status r)
        after <- document path
        -- The identity property is the SERVER's, so emptying the list empties
        -- the client's half of the drawer and leaves that one line standing.
        assertEqual "the subtree is its body and the server's own line"
                    (T.unlines [ "#+CATEGORY: notes", "* TODO First :one:", ":PROPERTIES:"
                               , ":ORG_GLANCE_ID: first", ":END:", "body of first"
                               , "* TODO Second", "tail" ])
                    after

  , testCase "the split shape is drift-locked like the whole one" $
      withCommitted $ \a path v -> do
        body' <- textAt "body" v
        props <- pairsAt "properties" v
        let stale = T.replicate 64 "0"
        r <- postTo a (headlinePath "first") (splitBody body' props stale)
        assertEqual "status" 409 (status r)
        assertEqual "reason" "stale" =<< textAt "reason" =<< decoded r
        assertEqual "untouched" committable =<< document path

  , testCase "and by the file on disk, not only by the store" $
      withCommitted $ \a path v -> do
        digest <- textAt "digest" v
        body' <- textAt "body" v
        props <- pairsAt "properties" v
        let meddled = committable <> "* TODO Someone else\n"
        TIO.writeFile path meddled
        r <- postTo a (headlinePath "first") (splitBody body' props digest)
        assertEqual "status" 409 (status r)
        assertEqual "reason" "drift" =<< textAt "reason" =<< decoded r
        assertEqual "the file is the meddler's" meddled =<< document path

    -- A planning value no timestamp parser would read back is refused BEFORE
    -- the write, and the refusal names the field: letting one through is silent,
    -- since the line stops being a planning line on the next load.
  , testCase "a planning entry that does not reparse is a 409 naming the field" $
      withCommitted $ \a path v -> do
        digest <- textAt "digest" v
        body' <- textAt "body" v
        r <- postTo a (headlinePath "first")
               (planningBody body' [] [["SCHEDULED", "tomorrow"]] digest)
        assertEqual "status" 409 (status r)
        b <- decoded r
        assertEqual "reason" "planning" =<< textAt "reason" b
        assertEqual "which field" "SCHEDULED" =<< textAt "field" b
        assertContains "and what it wanted" "timestamp org would read back"
          =<< textAt "error" b
        -- No digest on this one: nothing about it is a lock, and a client
        -- reading `digest' off a 409 is reading what its next write would pin.
        assertEqual "the fields it carries" ["error", "field", "reason"] =<< fieldsOf b
        assertEqual "untouched" committable =<< document path
        -- A keyword org does not know is refused the same way.
        bad <- postTo a (headlinePath "first")
                 (planningBody body' [] [["WHENEVER", "<2026-08-01 Sat>"]] digest)
        assertEqual "status" 409 (status bad)
        assertEqual "named" "WHENEVER" =<< textAt "field" =<< decoded bad

  , testCase "a body that is not the two fields is a 400" $
      withCommitted $ \a _path _v -> do
        broken <- postTo a (headlinePath "first") "{not json"
        missing <- postTo a (headlinePath "first") (encode (object ["org" .= ("x" :: T.Text)]))
        assertEqual "malformed" 400 (status broken)
        assertEqual "incomplete" 400 (status missing)
        -- The parse error names the missing field, rather than the word
        -- appearing anywhere in a body that also carries the digest itself.
        assertContains "says which" "key \\\"digest\\\" not found" (body missing)

    -- Which of two texts to write is not a thing to guess at, and a `body' with
    -- no `properties' beside it would read as "drop the drawer" — too much to
    -- infer from a field a client forgot to send.
  , testCase "the two shapes are told apart, and neither is half-given" $
      withCommitted $ \a path v -> do
        digest <- textAt "digest" v
        both <- postTo a (headlinePath "first")
                  (encode (object [ "org" .= ("* x\n" :: T.Text), "body" .= ("* x\n" :: T.Text)
                                  , "digest" .= digest ]))
        lonely <- postTo a (headlinePath "first")
                    (encode (object ["body" .= ("* x\n" :: T.Text), "digest" .= digest]))
        neither <- postTo a (headlinePath "first") (encode (object ["digest" .= digest]))
        assertEqual "both shapes at once" 400 (status both)
        assertContains "says so" "not both" (body both)
        assertEqual "a body with no properties" 400 (status lonely)
        assertContains "names the missing field" "properties" (body lonely)
        assertEqual "neither shape" 400 (status neither)
        assertContains "names both" "no \\\"org\\\"" (body neither)
        assertEqual "and nothing was written" committable =<< document path

  , testCase "a body over the cap is refused before it is read" $
      withCommitted $ \a _path _v -> do
        let huge = BL.fromStrict (BS.replicate (1024 * 1024 + 1) 0x78)
        r <- postTo a (headlinePath "first") huge
        assertEqual "status" 413 (status r)
        assertContains "the cap" "body over" (body r)

  , testCase "an id no row carries is a 404, and no id a 400" $
      withCommitted $ \a _path _v -> do
        unknown <- postTo a (headlinePath "no-such-headline") (commitBody "* x\n" "d")
        anonymous <- postTo a "/headline" (commitBody "* x\n" "d")
        assertEqual "unknown id" 404 (status unknown)
        assertEqual "no id" 400 (status anonymous)
        assertContains "the hint" "POST /headline?id=<row id>" (body anonymous)
  ]

-- | The rows a structured command names, and the two files they live in.  Ids
-- are in drawers so they survive both the temp directory's name and every edit
-- made to the text above them.
commandable :: T.Text
commandable = T.unlines
  [ "#+TODO: NEXT WAITING | CANCELLED"
  , "* NEXT First :one:"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: first"
  , ":END:"
  , "* Second"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: second"
  , ":END:"
  ]

-- | A second file, declaring no keywords of its own — so a keyword legal in
-- 'commandable' is illegal here, which is what makes legality per file
-- observable.
elsewhereOrg :: T.Text
elsewhereOrg = T.unlines
  [ "* TODO Third"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: third"
  , ":END:"
  ]

-- | A command as the shell sends one.
command :: T.Text -> [T.Text] -> Value -> BL.ByteString
command name ids args = encode (object ["name" .= name, "ids" .= ids, "args" .= args])

-- | @set-state@'s argument, a keyword or the null that clears one.
keywordArg :: Maybe T.Text -> Value
keywordArg keyword = object ["keyword" .= keyword]

-- | Run K over a server holding both files: the app, the hub whose store it
-- answers from — the write cases look at that store afterwards, and the
-- idempotence case steps it the way the watch would — and the two paths.
withCommandable :: (Application -> Hub -> FilePath -> FilePath -> Assertion) -> Assertion
withCommandable k = withTempDir $ \dir -> do
  here <- orgFile dir "notes.org" commandable
  there <- orgFile dir "other.org" elsewhereOrg
  (a, hub) <- serverOver dir
  k a hub here there

-- | The watch's own step, taken here without a watcher: PATH re-loaded into HUB
-- and published, which is the one path that updates the store.  A command
-- computes its spans and its digest from the store, so a second command over a
-- file the first one wrote needs this in between — exactly as a live daemon
-- does, and refuses with a drift error without it.
watchStep :: Hub -> FilePath -> Assertion
watchStep hub path = do
  outcome <- loadFile path
  _ <- publish hub (applyFile path outcome)
  pure ()

-- | R's results as id and whether the row landed, in the order they arrived.
outcomesOf :: SResponse -> IO [(T.Text, Bool)]
outcomesOf r = do
  results <- listAt "results" =<< decoded r
  traverse (\v -> (,) <$> textAt "id" v <*> boolAt "ok" v) results

-- | The digest R reports for each row that landed.
digestsOf :: SResponse -> IO [T.Text]
digestsOf r = do
  results <- listAt "results" =<< decoded r
  ok <- filterM (boolAt "ok") results
  traverse (textAt "digest") ok

-- | @POST \/command@: the structured writes, per file and per id.
commandSpec :: TestTree
commandSpec = testGroup "POST /command"
  [ testCase "set-state replaces the keyword and moves no other byte" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command" (command "set-state" ["first"] (keywordArg (Just "WAITING")))
        assertEqual "status" 200 (status r)
        assertEqual "the row landed" [("first", True)] =<< outcomesOf r
        after <- document path
        -- Stated as the whole file: everything ahead of the keyword and past it
        -- is the same string it was, by the same assertion as the edit.
        assertEqual "the file is the old one with one word replaced"
                    (T.replace "* NEXT First" "* WAITING First" before) after
        onDisk <- digestOnDisk path
        assertEqual "the digest it reports is the file's" [onDisk] =<< digestsOf r

  , testCase "a keyword where there was none is inserted after the stars" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command" (command "set-state" ["second"] (keywordArg (Just "NEXT")))
        assertEqual "status" 200 (status r)
        after <- document path
        assertEqual "inserted, and nothing else"
                    (T.replace "* Second" "* NEXT Second" before) after

  , testCase "a null keyword takes the word and its space off" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command" (command "set-state" ["first"] (keywordArg Nothing))
        assertEqual "status" 200 (status r)
        assertEqual "the file closed up" (T.replace "* NEXT First" "* First" before)
          =<< document path

    -- Two rows of one file are ONE editFile, and the proof is that the second
    -- one landed at all: a write per row would pin the second to the digest the
    -- first invalidated, and drift.  The shared digest says the same thing.
  , testCase "two rows of one file are one write, and both land" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command"
               (command "set-state" ["first", "second"] (keywordArg (Just "CANCELLED")))
        assertEqual "status" 200 (status r)
        assertEqual "both rows" [("first", True), ("second", True)] =<< outcomesOf r
        digests <- digestsOf r
        after <- document path
        assertEqual "both edits, in one file"
                    (T.replace "* Second" "* CANCELLED Second"
                       (T.replace "* NEXT First" "* CANCELLED First" before))
                    after
        onDisk <- digestOnDisk path
        assertEqual "a digest per row" 2 (length digests)
        assertEqual "one write, so one digest, and it is the file's" [onDisk] (nub digests)

  , testCase "rows in two files are two writes, and each is its own" $
      withCommandable $ \a _hub path other -> do
        r <- postTo a "/command" (command "archive" ["first", "third"] (object []))
        assertEqual "status" 200 (status r)
        assertEqual "both rows" [("first", True), ("third", True)] =<< outcomesOf r
        assertEqual "two files, two digests" 2 . length . nub =<< digestsOf r
        assertContains "the tag joined the list" "* NEXT First :one:ARCHIVE:" =<< document path
        assertContains "and started one" "* TODO Third :ARCHIVE:" =<< document other

    -- No cross-file rollback, and none is possible: the answer says which rows
    -- landed instead.
  , testCase "a file that moved refuses its rows while the others land" $
      withCommandable $ \a _hub path other -> do
        meddled <- (<> "* TODO Someone else\n") <$> document other
        TIO.writeFile other meddled
        r <- postTo a "/command" (command "archive" ["first", "third"] (object []))
        assertEqual "status" 200 (status r)
        assertEqual "one landed, one did not"
                    [("first", True), ("third", False)] =<< outcomesOf r
        assertContains "the untouched file took its edit" ":one:ARCHIVE:" =<< document path
        assertEqual "and the moved one is the meddler's" meddled =<< document other

  , testCase "an id no row carries is refused on its own" $
      withCommandable $ \a _hub path _other -> do
        r <- postTo a "/command" (command "archive" ["nowhere", "first"] (object []))
        assertEqual "status" 200 (status r)
        -- Answered in the order the ids were named, whichever of them was work.
        assertEqual "in the order asked"
                    [("nowhere", False), ("first", True)] =<< outcomesOf r
        assertContains "the real row still landed" ":one:ARCHIVE:" =<< document path

    -- Legality is per file, and a set-state some named row would refuse is
    -- refused whole: half a state change over a marked set is worse than none.
  , testCase "a keyword one named file does not declare refuses the request" $
      withCommandable $ \a _hub path other -> do
        before <- document path
        r <- postTo a "/command"
               (command "set-state" ["first", "third"] (keywordArg (Just "WAITING")))
        assertEqual "status" 400 (status r)
        assertContains "names the keyword" "WAITING" (body r)
        assertEqual "the first file is untouched" before =<< document path
        assertEqual "and so is the second" elsewhereOrg =<< document other

  , testCase "the state column's group values are refused like any other word" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        mapM_ (\meta -> do
                 r <- postTo a "/command"
                        (command "set-state" ["first"] (keywordArg (Just meta)))
                 assertEqual (T.unpack meta <> ": status") 400 (status r))
              ["*active*", "*inactive*"]
        assertEqual "nothing written" before =<< document path

  , testCase "archive is idempotent — the second run changes nothing" $
      withCommandable $ \a hub path _other -> do
        _ <- postTo a "/command" (command "archive" ["first"] (object []))
        once <- document path
        watchStep hub path   -- the store learns what the first command wrote
        again <- postTo a "/command" (command "archive" ["first"] (object []))
        assertEqual "status" 200 (status again)
        assertEqual "the row still landed" [("first", True)] =<< outcomesOf again
        assertEqual "and the file is byte for byte what it was" once =<< document path
        assertEqual "one tag, not two" 1 (T.count "ARCHIVE" once)

  , testCase "a digest the store no longer holds refuses that file's rows" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        let stale = encode (object [ "name" .= ("archive" :: T.Text)
                                   , "ids" .= (["first", "second"] :: [T.Text])
                                   , "digests" .= object ["first" .= T.replicate 64 "0"] ])
        r <- postTo a "/command" stale
        assertEqual "status" 200 (status r)
        -- The digest is per FILE, so a stale pin on one row refuses the file.
        assertEqual "both rows of that file"
                    [("first", False), ("second", False)] =<< outcomesOf r
        assertEqual "untouched" before =<< document path

  , testCase "a digest the store does hold is written as usual" $
      withCommandable $ \a _hub path _other -> do
        held <- textAt "digest" =<< decoded =<< getFrom a (headlinePath "first")
        let pinned = encode (object [ "name" .= ("archive" :: T.Text)
                                    , "ids" .= (["first"] :: [T.Text])
                                    , "digests" .= object ["first" .= held] ])
        r <- postTo a "/command" pinned
        assertEqual "the row landed" [("first", True)] =<< outcomesOf r
        assertContains "and the tag is on it" ":one:ARCHIVE:" =<< document path

  , testCase "leaves the store alone — the watch is what updates rows" $
      withCommandable $ \a _hub path _other -> do
        before <- decoded =<< getFrom a (headlinePath "first")
        r <- postTo a "/command" (command "archive" ["first"] (object []))
        assertEqual "status" 200 (status r)
        after <- decoded =<< getFrom a (headlinePath "first")
        -- No watcher runs in this suite, so the store still holds the load it
        -- started with: the route wrote to the file and to nothing else.
        assertEqual "the store answers exactly what it did" before after
        onDisk <- digestOnDisk path
        pinned <- textAt "digest" before
        assertBool "the file was written" (onDisk /= pinned)

  , testCase "a body that is not a command is a 400, and says what one is" $
      withCommandable $ \a _hub _path _other -> do
        mapM_ (\(what, payload, needle) -> do
                 r <- postTo a "/command" payload
                 assertEqual (what <> ": status") 400 (status r)
                 assertContains what needle (body r))
              [ ("malformed", "{not json", "body")
              , ("no name", encode (object ["ids" .= (["first"] :: [T.Text])]), "body")
              , ("no such command", command "explode" ["first"] (object []), "no such command")
              , ("no rows", command "archive" [] (object []), "names rows")
              , ("set-state with no args"
                , encode (object [ "name" .= ("set-state" :: T.Text)
                                 , "ids" .= (["first"] :: [T.Text]) ])
                , "keyword") ]

  , testCase "one id is spelled id, the way a command on the row at point is" $
      withCommandable $ \a _hub path _other -> do
        r <- postTo a "/command" (encode (object [ "name" .= ("archive" :: T.Text)
                                                 , "id" .= ("first" :: T.Text) ]))
        assertEqual "status" 200 (status r)
        assertEqual "the row landed" [("first", True)] =<< outcomesOf r
        assertContains "written" ":one:ARCHIVE:" =<< document path

  , testCase "an id named twice is written once" $
      withCommandable $ \a _hub path _other -> do
        r <- postTo a "/command" (command "archive" ["first", "first"] (object []))
        assertEqual "one result" [("first", True)] =<< outcomesOf r
        assertEqual "one tag" 1 . T.count "ARCHIVE" =<< document path

  , testCase "a body over the cap is refused before it is read" $
      withCommandable $ \a _hub _path _other -> do
        r <- postTo a "/command" (BL.fromStrict (BS.replicate (1024 * 1024 + 1) 0x78))
        assertEqual "status" 413 (status r)

  , testCase "the route takes POST and nothing else" $
      withCommandable $ \a _hub _path _other -> do
        r <- getFrom a "/command"
        assertEqual "status" 405 (status r)
        assertContains "hint" "/command takes POST" (body r)
  ]

-- | @set-planning@: the reschedule keys' half of the command route.  What is
-- pinned here is the request's shape and the whole-request refusal; the span
-- math itself is @TestQuery@'s "set-planning" group and is not restated.
planningSpec :: TestTree
planningSpec = testGroup "POST /command set-planning"
  [ testCase "a date lands as an active timestamp with the weekday computed" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command"
               (command "set-planning" ["first"] (planningArg "SCHEDULED" (Just "2026-08-05")))
        assertEqual "status" 200 (status r)
        assertEqual "the row landed" [("first", True)] =<< outcomesOf r
        assertEqual "the line went under the title line, and nothing else moved"
                    (T.replace "* NEXT First :one:\n"
                               "* NEXT First :one:\nSCHEDULED: <2026-08-05 Wed>\n" before)
          =<< document path

  , testCase "and a null date takes the entry and its line off" $
      withCommandable $ \a hub path _other -> do
        _ <- postTo a "/command"
               (command "set-planning" ["first"] (planningArg "DEADLINE" (Just "2026-08-05")))
        watchStep hub path
        before <- document path
        assertContains "there is a line to take off" "DEADLINE: <2026-08-05 Wed>" before
        r <- postTo a "/command"
               (command "set-planning" ["first"] (planningArg "DEADLINE" Nothing))
        assertEqual "status" 200 (status r)
        assertEqual "the file is what it was before the first command"
                    (T.replace "DEADLINE: <2026-08-05 Wed>\n" "" before) =<< document path

    -- Two files, two writes, one date: the clock is read once for the request,
    -- so a marked set cannot land on two days.
  , testCase "over rows in two files, each file is its own write" $
      withCommandable $ \a _hub path other -> do
        r <- postTo a "/command"
               (command "set-planning" ["first", "third"] (planningArg "SCHEDULED" (Just "today")))
        assertEqual "status" 200 (status r)
        assertEqual "both rows" [("first", True), ("third", True)] =<< outcomesOf r
        assertEqual "two files, two digests" 2 . length . nub =<< digestsOf r
        here <- document path
        there <- document other
        let dayOf = T.takeWhile (/= '\n') . T.drop 1 . T.dropWhile (/= '<')
        assertEqual "and the same day in both" (dayOf here) (dayOf there)

    -- The whole request, the way an undeclared keyword refuses one: half a
    -- reschedule over a marked set is worse than none of one.
  , testCase "a date no parser reads refuses the request, naming it" $
      withCommandable $ \a _hub path other -> do
        before <- document path
        r <- postTo a "/command"
               (command "set-planning" ["first", "third"]
                        (planningArg "SCHEDULED" (Just "next tuesday")))
        assertEqual "status" 400 (status r)
        assertContains "names the input" "next tuesday" (body r)
        assertEqual "the first file is untouched" before =<< document path
        assertEqual "and so is the second" elsewhereOrg =<< document other

  , testCase "and so does a keyword no key sets" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command"
               (command "set-planning" ["first"] (planningArg "CLOSED" (Just "2026-08-05")))
        assertEqual "status" 400 (status r)
        assertContains "names the keyword" "CLOSED" (body r)
        assertEqual "nothing written" before =<< document path

    -- Absent is not null: one says nothing about the entry and the other asks
    -- for it to come off, and a client that forgot the field is told so.
  , testCase "a request with no date at all is a 400" $
      withCommandable $ \a _hub _path _other -> do
        r <- postTo a "/command"
               (command "set-planning" ["first"] (object ["keyword" .= ("SCHEDULED" :: T.Text)]))
        assertEqual "status" 400 (status r)
        assertContains "asks for one" "date" (body r)

  , testCase "and one with no keyword either" $
      withCommandable $ \a _hub _path _other -> do
        r <- postTo a "/command"
               (command "set-planning" ["first"] (object ["date" .= ("today" :: T.Text)]))
        assertEqual "status" 400 (status r)
        assertContains "asks for one" "keyword" (body r)
  ]

-- | @capture@: the one command that names no row, and the one write whose
-- target comes out of the config rather than out of the request.
captureSpec :: TestTree
captureSpec = testGroup "POST /command capture"
  [ -- The target may not exist: the empty digest is the pin for that, so the
    -- first capture into a tree creates the file and the entry is the whole of
    -- it.
    testCase "creates the target and the entry is the whole file" $
      withCaptureTree Nothing $ \a _hub dir -> do
        r <- postTo a "/command" (capture "TODO Buy milk :errands:")
        assertEqual "status" 200 (status r)
        v <- decoded r
        assertEqual "it says where it wrote" (T.pack (dir </> "inbox.org"))
          =<< textAt "file" v
        assertEqual "and that it did" True =<< boolAt "ok" v
        written <- document (dir </> "inbox.org")
        assertEqual "the entry, with its creation time in a drawer"
                    [ "* TODO Buy milk :errands:", ":PROPERTIES:", ":END:" ]
                    [ l | l <- T.lines written
                        , not (":ORG_GLANCE_CREATION_TIME:" `T.isPrefixOf` l) ]
        assertContains "the stamp is the property's" ":ORG_GLANCE_CREATION_TIME: [" written
        onDisk <- digestOnDisk (dir </> "inbox.org")
        assertEqual "and the digest it reports is the file's" onDisk =<< textAt "digest" v

    -- The stamp is org's inactive form, to the minute, in the server's zone.
  , testCase "the creation time reparses as org's own inactive timestamp" $
      withCaptureTree Nothing $ \a _hub dir -> do
        _ <- postTo a "/command" (capture "read the docs")
        written <- document (dir </> "inbox.org")
        stamp <- maybe (assertFailure ("no stamp in " <> show written)) pure
                       (between ":ORG_GLANCE_CREATION_TIME: " "\n" written)
        assertBool ("inactive and bracketed: " <> show stamp)
                   ("[" `T.isPrefixOf` stamp && "]" `T.isSuffixOf` stamp)
        assertEqual "and the shape org writes" (T.length "[2026-08-01 Sat 09:30]")
                    (T.length stamp)

    -- Appended, so a file that already holds work keeps every byte of it.
  , testCase "a second capture appends and moves no byte of the first" $
      withCaptureTree Nothing $ \a _hub dir -> do
        _ <- postTo a "/command" (capture "first thing")
        before <- document (dir </> "inbox.org")
        _ <- postTo a "/command" (capture "second thing")
        after <- document (dir </> "inbox.org")
        assertBool ("appended: " <> show after) (before `T.isPrefixOf` after)
        assertContains "and the second entry is there" "* second thing" after

  , testCase "the tree's own target is where it goes" $
      withCaptureTree (Just "notes/in.org") $ \a _hub dir -> do
        r <- postTo a "/command" (capture "a note")
        assertEqual "status" 200 (status r)
        assertEqual "the configured file" (T.pack (dir </> "notes/in.org"))
          =<< textAt "file" =<< decoded r
        assertContains "written there" "* a note" =<< document (dir </> "notes/in.org")

    -- Refused where the config is read, so a misconfigured tree says so rather
    -- than writing outside itself.
  , testCase "a target outside the served root is refused, and writes nothing" $
      mapM_ (\target -> withCaptureTree (Just target) $ \a _hub dir -> do
               r <- postTo a "/command" (capture "a note")
               assertEqual (T.unpack target <> ": status") 400 (status r)
               assertContains (T.unpack target) "GLANCE_CAPTURE_TARGET" (body r)
               there <- doesFileExist (dir </> "inbox.org")
               assertBool "and no inbox was written instead" (not there))
            ["/tmp/glance-escape.org", "../escape.org", "inbox.txt"]

    -- The watch is the one thing that updates rows, here as everywhere.
  , testCase "the row arrives over the watch, not out of the route" $
      withCaptureTree Nothing $ \a hub dir -> do
        _ <- postTo a "/command" (capture "TODO Buy milk")
        assertEqual "the store has not moved" 1 . length =<< rowsOf =<< getFrom a "/headlines"
        watchStep hub (dir </> "inbox.org")
        rows <- rowsOf =<< getFrom a "/headlines"
        assertEqual "and now it has" 2 (length rows)
        assertBool ("the captured row is in it: " <> show rows)
                   (any (("Buy milk" `T.isInfixOf`) . T.pack . show) rows)

    -- The entry a capture promises is ONE headline, so the two ways of making
    -- it something else are 400 with nothing written.
  , testCase "an empty line and a multi-line one are refused" $
      withCaptureTree Nothing $ \a _hub dir ->
        mapM_ (\(what, text') -> do
                 r <- postTo a "/command" (capture text')
                 assertEqual (what <> ": status") 400 (status r)
                 there <- doesFileExist (dir </> "inbox.org")
                 assertBool (what <> ": wrote a file anyway") (not there))
              [("empty", ""), ("blank", "   "), ("two lines", "one\n* two")]

  , testCase "and a body with no text at all says what one is" $
      withCaptureTree Nothing $ \a _hub _dir -> do
        r <- postTo a "/command"
               (encode (object ["name" .= ("capture" :: T.Text), "args" .= object []]))
        assertEqual "status" 400 (status r)
        assertContains "names the field" "text" (body r)

    -- It is the one command that needs none, so the rule that every other one
    -- names rows must not reach it.
  , testCase "it names no rows, and is not refused for that" $
      withCaptureTree Nothing $ \a _hub _dir -> do
        r <- postTo a "/command" (capture "no ids here")
        assertEqual "status" 200 (status r)
  ]

-- | A capture as the shell sends one: no ids at all, and one line of org.
capture :: T.Text -> BL.ByteString
capture text' = encode (object [ "name" .= ("capture" :: T.Text)
                               , "args" .= object ["text" .= text'] ])

-- | @set-planning@'s arguments: which keyword, and the date text or the null
-- that takes the entry off.
planningArg :: T.Text -> Maybe T.Text -> Value
planningArg keyword date = object ["keyword" .= keyword, "date" .= date]

-- | Run K over a server holding one document and, where TARGET names one, a
-- system config naming it as the capture target.  The hub comes with it, since
-- what a capture leaves for the WATCH is half of what there is to check.
withCaptureTree :: Maybe T.Text -> (Application -> Hub -> FilePath -> Assertion) -> Assertion
withCaptureTree target k = withTempDir $ \dir -> do
  _ <- orgFile dir "notes.org" "* TODO Already here\n"
  mapM_ (writeSystemConfig dir) target
  (a, hub) <- serverOver dir
  k a hub dir

-- | DIR's system layer, naming TARGET as the tree's capture target.  The path
-- is 'systemAt''s, so no case here spells the config layout a second time.
writeSystemConfig :: FilePath -> T.Text -> IO ()
writeSystemConfig dir target = do
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path ("#+GLANCE_CAPTURE_TARGET: " <> target <> "\n")
  where path = T.unpack (systemAt dir)

-- | The keyword layers, read and written.  @GET@ lists every config file the
-- served tree has — plus the @system.org@ it could have — and @POST@ puts one
-- layer's @#+TODO:@ block back, through the same engine, the same lock and the
-- same atomic rename every other write uses.
configSpec :: TestTree
configSpec = testGroup "GET and POST /config"
  [ testCase "lists each layer with its lines and the digest a write pins" $
      withConfigTree $ \a dir -> do
        r <- getFrom a "/config"
        assertEqual "status" 200 (status r)
        v <- decoded r
        layers <- listAt "layers" v
        assertEqual "system first, then the tag configs by name"
                    [systemAt dir, tagAt dir "book", tagAt dir "film"]
          =<< traverse (textAt "path") layers
        assertEqual "which layer each is" [Nothing, Just "book", Just "film"]
          =<< traverse (maybeTextAt "tag") layers
        assertEqual "the lines, verbatim"
                    [[], ["#+TODO:  TODO READING | READ ABANDONED"], []]
          =<< traverse (textsAt "lines") layers
        -- The union is the store's own palette, so the preview and the badges
        -- a reader is looking at cannot disagree.
        keywords <- field "keywords" v
        assertEqual "active" ["READING", "TODO"] =<< textsAt "active" keywords
        assertEqual "inactive" ["ABANDONED", "READ", "DONE"] =<< textsAt "inactive" keywords

    -- A tree that has never had a system layer still has the place for one, and
    -- the empty digest is what says so: it is the pin an absent file carries,
    -- so the record a reader is handed is the lock a writer presents back.
    -- The default view rides beside the layers because it is a line of one of
    -- them: shown off the same read the digests were taken from, so a sheet
    -- cannot show one file and pin its write to another.
  , testCase "the default view rides beside the layers" $
      withConfigTree $ \a _dir -> do
        v <- decoded =<< getFrom a "/config"
        assertEqual "with no line anywhere, the built-in"
                    "state:*active*" =<< textAt "filter" v

  , testCase "and a system layer naming one is what is served" $
      withConfigTree $ \a dir -> do
        digest <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        r <- postTo a "/config" (viewBody (systemAt dir) [] (Just "tag:work") digest)
        assertEqual "status" 200 (status r)
        assertContains "the line is in the file" "#+GLANCE_DEFAULT_FILTER: tag:work"
          =<< document (T.unpack (systemAt dir))
        v <- decoded =<< getFrom a "/config"
        assertEqual "and the next read says so" "tag:work" =<< textAt "filter" v

  , testCase "an emptied default view takes the line away" $
      withConfigTree $ \a dir -> do
        digest <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        _ <- postTo a "/config" (viewBody (systemAt dir) [] (Just "tag:work") digest)
        fresh <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        r <- postTo a "/config" (viewBody (systemAt dir) [] (Just "") fresh)
        assertEqual "status" 200 (status r)
        after <- document (T.unpack (systemAt dir))
        assertBool ("the line is gone: " <> show after)
                   (not ("GLANCE_DEFAULT_FILTER" `T.isInfixOf` after))
        assertEqual "so the built-in answers again" "state:*active*"
          =<< textAt "filter" =<< decoded =<< getFrom a "/config"

    -- A default view belongs to a TREE rather than to a tag, so a tag layer's
    -- write leaves the line alone whatever it named.
  , testCase "a tag layer cannot set the default view" $
      withConfigTree $ \a dir -> do
        digest <- digestOnDisk (T.unpack (tagAt dir "book"))
        r <- postTo a "/config"
               (viewBody (tagAt dir "book") ["#+TODO: TODO | DONE"] (Just "tag:work") digest)
        assertEqual "status" 200 (status r)
        after <- document (T.unpack (tagAt dir "book"))
        assertBool ("nothing was written: " <> show after)
                   (not ("GLANCE_DEFAULT_FILTER" `T.isInfixOf` after))

    -- The capture target is the second tree-wide line of the same file, and it
    -- travels the same way: read off the layers, written in their write.
  , testCase "the capture target rides beside the layers too" $
      withConfigTree $ \a _dir ->
        assertEqual "with no line anywhere, nothing" ""
          =<< textAt "capture" =<< decoded =<< getFrom a "/config"

  , testCase "and it is written in the system layer's own write" $
      withConfigTree $ \a dir -> do
        digest <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        r <- postTo a "/config" (captureBody (systemAt dir) [] (Just "notes/in.org") digest)
        assertEqual "status" 200 (status r)
        assertContains "the line is in the file" "#+GLANCE_CAPTURE_TARGET: notes/in.org"
          =<< document (T.unpack (systemAt dir))
        assertEqual "and the next read says so" "notes/in.org"
          =<< textAt "capture" =<< decoded =<< getFrom a "/config"

  , testCase "an emptied capture target takes the line away" $
      withConfigTree $ \a dir -> do
        digest <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        _ <- postTo a "/config" (captureBody (systemAt dir) [] (Just "notes/in.org") digest)
        fresh <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        r <- postTo a "/config" (captureBody (systemAt dir) [] (Just "") fresh)
        assertEqual "status" 200 (status r)
        after <- document (T.unpack (systemAt dir))
        assertBool ("the line is gone: " <> show after)
                   (not ("GLANCE_CAPTURE_TARGET" `T.isInfixOf` after))

  , testCase "a tag layer cannot set it either" $
      withConfigTree $ \a dir -> do
        digest <- digestOnDisk (T.unpack (tagAt dir "book"))
        r <- postTo a "/config"
               (captureBody (tagAt dir "book") ["#+TODO: TODO | DONE"] (Just "in.org") digest)
        assertEqual "status" 200 (status r)
        after <- document (T.unpack (tagAt dir "book"))
        assertBool ("nothing was written: " <> show after)
                   (not ("GLANCE_CAPTURE_TARGET" `T.isInfixOf` after))

    -- The page carries it as DEFAULT_QUERY, read off the store at request time.
    -- The store is the read model for everything else the page shows, and the
    -- watch reseeds it when a config file moves, so a live daemon converges the
    -- way it does for the badge palette.
  , testCase "the served page carries the tree's default view" $ do
      withConfigTree $ \a _dir ->
        assertContains "the built-in, where nothing configures one"
                       "const DEFAULT_QUERY = \"state:*active*\"" . body =<< getFrom a "/"
      withTempDir $ \dir -> do
        let config = dir </> ".org-glance" </> "config"
        createDirectoryIfMissing True config
        TIO.writeFile (config </> "system.org")
          "#+TODO: TODO | DONE\n#+GLANCE_DEFAULT_FILTER: tag:work\n"
        _ <- orgFile dir "notes.org" "* TODO x\n"
        (a, _hub) <- serverOver dir
        assertContains "the tree's own" "const DEFAULT_QUERY = \"tag:work\"" . body
          =<< getFrom a "/"

  , testCase "a tree with no system.org lists it anyway, as creatable" $
      withConfigTree $ \a dir -> do
        layers <- listAt "layers" =<< decoded =<< getFrom a "/config"
        digests <- traverse (textAt "digest") layers
        assertEqual "the system layer is not a file yet" "" (head digests)
        assertBool "and the tag config is one" (not (T.null (digests !! 1)))
        assertEqual "which one it would be" (systemAt dir) =<< textAt "path" (head layers)

  , testCase "replaces the block and leaves every other byte alone" $
      withConfigTree $ \a dir -> do
        before <- document (T.unpack (tagAt dir "book"))
        digest <- digestOnDisk (T.unpack (tagAt dir "book"))
        r <- postTo a "/config"
               (configBody (tagAt dir "book") ["#+TODO: TODO READING NEXT | READ"] digest)
        assertEqual "status" 200 (status r)
        after <- document (T.unpack (tagAt dir "book"))
        assertEqual "the pragma line, and nothing else"
                    (T.replace "#+TODO:  TODO READING | READ ABANDONED"
                               "#+TODO: TODO READING NEXT | READ" before)
                    after
        -- The receipt is the file's new digest, so a second write needs no
        -- second read.
        receipt <- textAt "digest" =<< decoded r
        onDisk <- digestOnDisk (T.unpack (tagAt dir "book"))
        assertEqual "the receipt is the file's new digest" onDisk receipt

  , testCase "inserts under the header when the file carries no block" $
      withConfigTree $ \a dir -> do
        let path = T.unpack (tagAt dir "film")
        digest <- digestOnDisk path
        r <- postTo a "/config" (configBody (tagAt dir "film") ["#+TODO: A | B"] digest)
        assertEqual "status" 200 (status r)
        -- After the `#+TITLE:' run the file opens with, which is where org
        -- would have put it, and ahead of everything that is not a header.
        assertEqual "placed under the header"
                    "#+TITLE: Film\n#+TODO: A | B\n\n* %?\n" =<< document path

  , testCase "creates the file, and the directories over it" $
      withConfigTree $ \a dir -> do
        r <- postTo a "/config"
               (configBody (systemAt dir) ["#+TODO: TODO STARTED | DONE"] "")
        assertEqual "status" 200 (status r)
        assertEqual "the whole file is the block"
                    "#+TODO: TODO STARTED | DONE\n" =<< document (T.unpack (systemAt dir))

  , testCase "an empty block takes the layer's line off" $
      withConfigTree $ \a dir -> do
        let path = T.unpack (tagAt dir "book")
        digest <- digestOnDisk path
        r <- postTo a "/config" (configBody (tagAt dir "book") [] digest)
        assertEqual "status" 200 (status r)
        assertEqual "the line is gone and the template is not"
                    "#+TITLE: Book\n\n* Book\n" =<< document path

  , testCase "a digest the file no longer carries is a 409 with nothing written" $
      withConfigTree $ \a dir -> do
        let path = T.unpack (tagAt dir "book")
        before <- document path
        r <- postTo a "/config" (configBody (tagAt dir "book") ["#+TODO: A | B"] "deadbeef")
        assertEqual "status" 409 (status r)
        assertEqual "reason" "drift" =<< textAt "reason" =<< decoded r
        assertEqual "the file is as it was" before =<< document path

    -- The empty digest means "nothing is there", so a file that turned up
    -- meanwhile refuses the way a moved one does rather than being overwritten.
  , testCase "creating over a file that exists is the same refusal" $
      withConfigTree $ \a dir -> do
        r <- postTo a "/config" (configBody (tagAt dir "book") ["#+TODO: A | B"] "")
        assertEqual "status" 409 (status r)
        assertEqual "reason" "drift" =<< textAt "reason" =<< decoded r

  , testCase "refuses lines that are not a #+TODO: block" $
      withConfigTree $ \a dir -> do
        let path = T.unpack (tagAt dir "book")
        before <- document path
        digest <- digestOnDisk path
        mapM_ (\(what, lines') -> do
                 r <- postTo a "/config" (configBody (tagAt dir "book") lines' digest)
                 assertEqual what 400 (status r))
              [ ("a headline is not a pragma", ["* TODO not a pragma"])
              , ("nor is a title", ["#+TITLE: no"])
              , ("a pragma declaring nothing", ["#+TODO:"])
              -- The group meta-values are not keywords, and the parser is
              -- what refuses them: no keyword token holds an asterisk.
              , ("the filter's group names", ["#+TODO: *active* | *inactive*"])
              , ("and one bad line spoils the block", ["#+TODO: A | B", "oops"]) ]
        assertEqual "and nothing was written" before =<< document path

  , testCase "refuses a path that is not one of this tree's layers" $
      withConfigTree $ \a _dir -> do
        r <- postTo a "/config" (configBody "/etc/passwd" ["#+TODO: A | B"] "")
        assertEqual "status" 400 (status r)
        assertContains "says which paths there are" ".org-glance/config/system.org"
          =<< textAt "error" =<< decoded r

  , testCase "and a body that is not a layer write" $
      withConfigTree $ \a _dir -> do
        r <- postTo a "/config" (encode (object ["nope" .= True]))
        assertEqual "status" 400 (status r)

  , testCase "takes GET and POST and nothing else" $
      withConfigTree $ \a _dir -> do
        r <- runSession (request (setPath defaultRequest "/config")
                                   { requestMethod = methodDelete }) a
        assertEqual "status" 405 (status r)
        assertEqual "content type"
                    (Just "application/json; charset=utf-8") (header "Content-Type" r)

    -- The route is a writer like the other two, so it leaves the store alone:
    -- the rows and the palette arrive when the watch has seen the config move.
  , testCase "leaves the store alone — the watch is what reseeds" $
      withConfigTree $ \a dir -> do
        before <- badgeValues =<< decoded =<< getFrom a "/headlines"
        digest <- digestOnDisk (T.unpack (tagAt dir "book"))
        _ <- postTo a "/config"
               (configBody (tagAt dir "book") ["#+TODO: TODO READING NEXT | READ"] digest)
        -- The files say the new thing at once, since @/config@ reads them; the
        -- palette is the store's and cannot move until the watch has run.
        layers <- listAt "layers" =<< decoded =<< getFrom a "/config"
        assertEqual "the file"
                    [[], ["#+TODO: TODO READING NEXT | READ"], []]
          =<< traverse (textsAt "lines") layers
        assertEqual "the badges the table is showing" before
          =<< badgeValues =<< decoded =<< getFrom a "/headlines"
        assertBool "and NEXT is not among them" ("NEXT" `notElem` before)
  ]

-- | A tree laid out the way org-glance lays one out: a tag config with a cycle
-- and a capture template, one with a header and no cycle, no system layer at
-- all, and one ordinary document.
withConfigTree :: (Application -> FilePath -> Assertion) -> Assertion
withConfigTree k = withTempDir $ \dir -> do
  let tags = dir </> ".org-glance" </> "config" </> "tags"
  createDirectoryIfMissing True tags
  TIO.writeFile (tags </> "book.org")
    "#+TITLE: Book\n#+TODO:  TODO READING | READ ABANDONED\n\n* Book\n"
  TIO.writeFile (tags </> "film.org") "#+TITLE: Film\n\n* %?\n"
  _ <- orgFile dir "notes.org" "* READING War and Peace\n"
  (a, _hub) <- serverOver dir
  k a dir

-- | @GET \/keywords@: the classification chain behind the rows a command names,
-- which is what the state palette draws.
--
-- The chain itself is 'Data.Org.Config.classify' and @TestConfig@ is where the
-- rule is tested; what is pinned here is the resolution READ FORWARDS — a
-- keyword under the NEAREST source that declares it and nowhere below it — plus
-- how several rows merge and what the route refuses.
keywordsSpec :: TestTree
keywordsSpec = testGroup "GET /keywords"
  [ testCase "the file's own pragma takes a keyword off every source below it" $
      withLayeredTree $ \a -> do
        r <- getFrom a "/keywords?ids=filed"
        assertEqual "status" 200 (status r)
        -- READING is the file's, book's AND pile's; it belongs to the file
        -- alone, which leaves pile with nothing and so no row.  READ is book's
        -- and the system layer's, so it stays with the nearer of the two.
        -- The union closes the chain: `film''s cycle is recognized here and no
        -- scope this row reaches claims it, which is what makes it settable and
        -- what makes the last row honest about where it came from.
        assertEqual "file, then book, then the system layer, org's own, the union"
          [ ("file",    ["READING"],   [])
          , ("book",    [],            ["READ"])
          , ("system",  ["STARTED"],   [])
          , ("builtin", ["TODO"],      ["DONE"])
          , ("union",   ["WATCHING"],  ["WATCHED"]) ] =<< sourcesOf r
        assertEqual "and nothing was asked for that is not there" [] =<< textsAt "unknown"
          =<< decoded r

  , testCase "the first tag that declares a keyword is the one that keeps it" $
      withLayeredTree $ \a -> do
        -- The same two tags with no file pragma over them: book is named first
        -- on the headline, so READING is book's and pile drops out entirely.
        assertEqual "book, and no pile row at all"
          [ ("book",    ["READING"],  ["READ"])
          , ("system",  ["STARTED"],  [])
          , ("builtin", ["TODO"],     ["DONE"])
          , ("union",   ["WATCHING"], ["WATCHED"]) ]
          =<< sourcesOf =<< getFrom a "/keywords?ids=tagged"

  , testCase "a row no scope speaks for falls through to the recognition union" $
      withLayeredTree $ \a ->
        -- Untagged, in a file that declares nothing: every tag's cycle is still
        -- settable here and the union is the only thing that ever classified
        -- it, which is exactly what the last row of the chain says.
        assertEqual "the system layer, org's own, then everything else"
          [ ("system",  ["STARTED"],            ["READ"])
          , ("builtin", ["TODO"],               ["DONE"])
          , ("union",   ["READING", "WATCHING"], ["WATCHED"]) ]
          =<< sourcesOf =<< getFrom a "/keywords?ids=bare"

    -- The marked set: one answer over every row it holds, and a tag any of them
    -- carries is a source of its own.
  , testCase "two rows under different tags bring both tag sources" $
      withLayeredTree $ \a ->
        assertEqual "book from one, film from the other"
          [ ("book",    ["READING"],  ["READ"])
          , ("film",    ["WATCHING"], ["WATCHED"])
          , ("system",  ["STARTED"],  [])
          , ("builtin", ["TODO"],     ["DONE"]) ]
          =<< sourcesOf =<< getFrom a "/keywords?ids=tagged,filmed"

    -- The merge's one cost, stated: READING is under `book' for the tagged row
    -- alone and under `file' for the one whose file declares it, and the set
    -- answers with the NEARER of the two.  So the table describes the set
    -- rather than any one member of it.
  , testCase "a keyword nearer in one row than another lands in the nearer source" $
      withLayeredTree $ \a ->
        assertEqual "the file's, though one of the two rows reaches it by tag"
          [ ("file",    ["READING"],  [])
          , ("book",    [],           ["READ"])
          , ("system",  ["STARTED"],  [])
          , ("builtin", ["TODO"],     ["DONE"])
          , ("union",   ["WATCHING"], ["WATCHED"]) ]
          =<< sourcesOf =<< getFrom a "/keywords?ids=tagged,filed"

    -- The command route's convention: an id the store has no row for is named
    -- rather than refused, so a marked set that has gone stale still answers
    -- for the rows that are there.
  , testCase "an id the store does not hold is named and left out" $
      withLayeredTree $ \a -> do
        r <- getFrom a "/keywords?ids=nosuch,tagged"
        assertEqual "status" 200 (status r)
        assertEqual "the ones that are gone" ["nosuch"] =<< textsAt "unknown" =<< decoded r
        assertEqual "resolved for the one that is not"
          [ ("book",    ["READING"],  ["READ"])
          , ("system",  ["STARTED"],  [])
          , ("builtin", ["TODO"],     ["DONE"])
          , ("union",   ["WATCHING"], ["WATCHED"]) ] =<< sourcesOf r

  , testCase "every id unknown resolves nothing and still says which" $
      withLayeredTree $ \a -> do
        r <- getFrom a "/keywords?ids=nosuch"
        assertEqual "status" 200 (status r)
        assertEqual "no sources" [] =<< sourcesOf r
        assertEqual "and both halves of why" ["nosuch"] =<< textsAt "unknown" =<< decoded r

    -- Three spellings of one list: the comma form a caller types out, the
    -- repeated parameter the shell writes (an id may hold a comma, and the
    -- split happens after decoding, so percent-encoding cannot save it), and
    -- the singular key `POST /command' also takes.
  , testCase "ids repeat, ids comma-separate, id is one, and none is a 400" $
      withLayeredTree $ \a -> do
        let both = [ ("book",    ["READING"],  ["READ"])
                   , ("film",    ["WATCHING"], ["WATCHED"])
                   , ("system",  ["STARTED"],  [])
                   , ("builtin", ["TODO"],     ["DONE"]) ]
        assertEqual "repeated" both
          =<< sourcesOf =<< getFrom a "/keywords?ids=tagged&ids=filmed"
        assertEqual "and mixed with the comma form" both
          =<< sourcesOf =<< getFrom a "/keywords?ids=tagged&id=filmed"
        assertEqual "the singular spelling answers for one"
          [ ("book",    ["READING"],  ["READ"])
          , ("system",  ["STARTED"],  [])
          , ("builtin", ["TODO"],     ["DONE"])
          , ("union",   ["WATCHING"], ["WATCHED"]) ]
          =<< sourcesOf =<< getFrom a "/keywords?id=tagged"
        r <- getFrom a "/keywords"
        assertEqual "status" 400 (status r)
        assertEqual "naming the parameter" "GET /keywords?ids=<row id>,<row id>"
          =<< textAt "error" =<< decoded r

  , testCase "and it is a read: POST is a 405" $ do
      r <- withLayeredTree (\a -> postTo a "/keywords" "{}")
      assertEqual "status" 405 (status r)

    -- A tree may configure a tag called `system', and the four reserved names
    -- are not taken out of the tag namespace to stop it.  The entries stay
    -- apart — a tag keeps its tag RANK, so it sits above the system layer the
    -- way any other tag does — and the precedence order is what tells the two
    -- rows named alike apart.
  , testCase "a tag spelled like a reserved source keeps its own rank" $
      withTempDir $ \dir -> do
        writeLayers dir [ (Nothing,       "#+TODO: STARTED | SHELVED\n")
                        , (Just "system", "#+TODO: PLANNED | SHELVED\n") ]
        _ <- orgFile dir "a.org" (T.unlines
               [ "* one :system:", ":PROPERTIES:", ":ORG_GLANCE_ID: only", ":END:" ])
        (a, _hub) <- serverOver dir
        assertEqual "the tag first, keeping SHELVED, then the layer it shadows"
          [ ("system",  ["PLANNED"], ["SHELVED"])
          , ("system",  ["STARTED"], [])
          , ("builtin", ["TODO"],    ["DONE"]) ]
          =<< sourcesOf =<< getFrom a "/keywords?ids=only"
  ]

-- | @GET \/links@: where one row points.
--
-- The extraction rule is @TestQuery@'s ("Links"), which drives the pure
-- function; what belongs here is the route — the id it takes, the shape it
-- answers in, and the two refusals it shares with materialize.
linksSpec :: TestTree
linksSpec = testGroup "GET /links"
  [ testCase "is the row's links, in the order its subtree writes them" $
      withLinkTree $ \a -> do
        r <- getFrom a "/links?id=linked"
        assertEqual "status" 200 (status r)
        assertEqual "target and description"
          [ ["https://x.example/a", "the first"]
          , ["https://y.example/b", "https://y.example/b"]
          , ["https://z.example/c", "https://z.example/c"] ]
          =<< linksOf r

  , testCase "an id the store has no row for is a 404, like materialize" $
      withLinkTree $ \a -> do
        r <- getFrom a "/links?id=nosuch"
        assertEqual "status" 404 (status r)
        assertContains "hint" "no headline with id" (body r)

  , testCase "no id at all says what the route wants" $
      withLinkTree $ \a -> do
        r <- getFrom a "/links"
        assertEqual "status" 400 (status r)
        assertEqual "naming the parameter" "GET /links?id=<row id>"
          =<< textAt "error" =<< decoded r

  , testCase "a row with nothing to follow answers with an empty list" $
      withLinkTree $ \a ->
        assertEqual "no links" [] =<< linksOf =<< getFrom a "/links?id=bare"

  , testCase "and it is a read: POST is a 405" $ do
      r <- withLinkTree (\a -> postTo a "/links?id=linked" "{}")
      assertEqual "status" 405 (status r)
  ]

-- | The answer's links as @[target, desc]@ pairs.
linksOf :: SResponse -> IO [[T.Text]]
linksOf r = traverse one =<< listAt "links" =<< decoded r
  where one v = sequence [textAt "target" v, textAt "desc" v]

-- | A tree with one row worth following and one with nothing in it: a bracket
-- link on the title, a bare URL in the body, and one more under a child, so
-- the route's answer shows it read the SUBTREE.
withLinkTree :: (Application -> IO a) -> IO a
withLinkTree k = withTempDir $ \dir -> do
  _ <- orgFile dir "a.org" (T.unlines
         [ "* one [[https://x.example/a][the first]]"
         , ":PROPERTIES:"
         , ":ORG_GLANCE_ID: linked"
         , ":END:"
         , "see https://y.example/b for the rest"
         , "** a child https://z.example/c"
         , "* two"
         , ":PROPERTIES:"
         , ":ORG_GLANCE_ID: bare"
         , ":END:"
         , "nothing to follow here" ])
  (a, _hub) <- serverOver dir
  k a

-- | Each source the answer names, with the keywords it is the nearest to
-- declare.
sourcesOf :: SResponse -> IO [(T.Text, [T.Text], [T.Text])]
sourcesOf r = traverse one =<< listAt "sources" =<< decoded r
  where one v = (,,) <$> textAt "source" v <*> textsAt "active" v <*> textsAt "inactive" v

-- | A tree whose every layer has something to say about the same few keywords,
-- so which one ANSWERS is observable at each rung: a system layer, two tag
-- configs that disagree about @READING@, a third for a tag nothing else names,
-- and four rows reaching the chain at four different depths.
--
-- Polymorphic in what K yields, so the one case that wants the response rather
-- than an assertion needs no second name for the same tree.
withLayeredTree :: (Application -> IO a) -> IO a
withLayeredTree k = withTempDir $ \dir -> do
  writeLayers dir
    [ (Nothing,       "#+TODO: STARTED | READ\n")
    , (Just "book",   "#+TODO: READING | READ\n")
    , (Just "pile",   "#+TODO: | READING\n")
    , (Just "film",   "#+TODO: WATCHING | WATCHED\n") ]
  -- The file declares READING itself, and its row wears both tags that also do.
  _ <- orgFile dir "a.org" (T.unlines
         [ "#+TODO: READING |", "* READING one :book:pile:"
         , ":PROPERTIES:", ":ORG_GLANCE_ID: filed", ":END:" ])
  -- The same two tags with nothing above them, a third tag on its own, and a
  -- row that reaches no scope nearer than the system layer.
  _ <- orgFile dir "b.org" (T.unlines
         [ "* two :book:pile:", ":PROPERTIES:", ":ORG_GLANCE_ID: tagged", ":END:"
         , "* three :film:", ":PROPERTIES:", ":ORG_GLANCE_ID: filmed", ":END:"
         , "* four", ":PROPERTIES:", ":ORG_GLANCE_ID: bare", ":END:" ])
  (a, _hub) <- serverOver dir
  k a

-- | LAYERS written under DIR's config directory: 'Nothing' is @system.org@ and
-- a tag is its file beside it.  The layout is 'systemAt' and 'tagAt', so no
-- case here spells it a second time.
writeLayers :: FilePath -> [(Maybe FilePath, T.Text)] -> IO ()
writeLayers dir layers = do
  createDirectoryIfMissing True (takeDirectory (T.unpack (tagAt dir "any")))
  mapM_ write layers
  where write (tag, text) =
          TIO.writeFile (T.unpack (maybe (systemAt dir) (tagAt dir) tag)) text

systemAt :: FilePath -> T.Text
systemAt dir = T.pack (dir </> ".org-glance" </> "config" </> "system.org")

tagAt :: FilePath -> FilePath -> T.Text
tagAt dir tag = T.pack (dir </> ".org-glance" </> "config" </> "tags" </> tag <> ".org")

-- | A layer write: which file, the lines to put in it, and the digest it was
-- read with.
configBody :: T.Text -> [T.Text] -> T.Text -> BL.ByteString
configBody path lines' = layerBody path lines' Nothing Nothing

-- | 'configBody', also setting the default view.
viewBody :: T.Text -> [T.Text] -> Maybe T.Text -> T.Text -> BL.ByteString
viewBody path lines' want = layerBody path lines' want Nothing

-- | 'configBody', also setting the capture target.
captureBody :: T.Text -> [T.Text] -> Maybe T.Text -> T.Text -> BL.ByteString
captureBody path lines' = layerBody path lines' Nothing

-- | A layer write over all three of its lines.  Absent leaves a line alone; the
-- three ride in one request because they are lines of one file.
layerBody :: T.Text -> [T.Text] -> Maybe T.Text -> Maybe T.Text -> T.Text -> BL.ByteString
layerBody path lines' want target digest = encode (object
  ([ "path" .= path, "lines" .= lines', "digest" .= digest ]
     <> [ "filter" .= f | Just f <- [want] ]
     <> [ "capture" .= c | Just c <- [target] ]))

-- | Archived rows are out of the view unless the query asks for them.  @D@
-- archives rather than deletes, so this is what keeps the default table from
-- growing without bound — and what must never hide the key that reaches them.
archiveViewSpec :: TestTree
archiveViewSpec = testGroup "GET /headlines and the archive"
  [ testCase "an archived row is out of the default answer, and counted" $
      withArchived $ \a -> do
        r <- getFrom a "/headlines"
        assertEqual "the rows that are left" ["plain", "shipped"] . sort . map rowId
          =<< rowsOf r
        assertEqual "X-Glance-Total" (Just "2") (header "X-Glance-Total" r)
        assertEqual "X-Glance-Archived" (Just "1") (header "X-Glance-Archived" r)

  , testCase "the exclusion is exactly what -archive: spells" $
      withArchived $ \a -> do
        implicit <- rowsOf =<< getFrom a "/headlines"
        explicit <- getFrom a "/headlines?q=-archive%3A"
        assertEqual "the same rows" (map rowId implicit) . map rowId =<< rowsOf explicit
        -- A query that says it itself is not one this server also says: the
        -- count is zero because nothing was withheld from it.
        assertEqual "nothing hidden from it" (Just "0")
                    (header "X-Glance-Archived" explicit)

  , testCase "naming the key at all shows them" $
      withArchived $ \a ->
        mapM_ (\(path, wanted) -> do
                 r <- getFrom a path
                 assertEqual (show path <> ": the rows") wanted . sort . map rowId
                   =<< rowsOf r
                 assertEqual (show path <> ": nothing hidden") (Just "0")
                             (header "X-Glance-Archived" r))
              [ ("/headlines?q=archive%3A", ["filed"])
              , ("/headlines?q=archive%3Afiled", ["filed"])
              , ("/headlines?q=state%3ADONE%20archive%3A", ["filed"]) ]

    -- The vocabulary is the WHOLE store's, which is what makes the key reach
    -- what the default hides.  A value no row spells as text is the proof: as
    -- free text `archive:filed' matches nothing, so a match is a predicate.
  , testCase "the key survives the exclusion that hides its rows" $
      withArchived $ \a -> do
        faceted <- getFrom a "/headlines?q=archive%3Afiled"
        text' <- getFrom a "/headlines?q=%22archive%3Afiled%22"
        assertEqual "as a predicate" (Just "1") (header "X-Glance-Total" faceted)
        assertEqual "as free text" (Just "0") (header "X-Glance-Total" text')

  , testCase "a tree with nothing archived says so" $ do
      r <- get assetsDir "/headlines"
      assertEqual "X-Glance-Archived" (Just "0") (header "X-Glance-Archived" r)
      assertEqual "and every row is served" (Just "6") (header "X-Glance-Total" r)

  , testCase "the exclusion runs before the page, like the filter" $
      withArchived $ \a -> do
        r <- getFrom a "/headlines?limit=1"
        assertEqual "the total is what is left after it" (Just "2")
                    (header "X-Glance-Total" r)
        assertEqual "the page" 1 . length =<< rowsOf r
        assertEqual "and more follows" (Just "true") (header "X-Glance-Has-Next" r)
  ]

-- | Run K over a server holding three rows, one of them tagged @ARCHIVE@.
withArchived :: (Application -> Assertion) -> Assertion
withArchived k = withTempDir $ \dir -> do
  _ <- orgFile dir "notes.org" (T.unlines
         [ "* TODO Plain"
         , ":PROPERTIES:"
         , ":ORG_GLANCE_ID: plain"
         , ":END:"
         , "* DONE Shipped :web:"
         , ":PROPERTIES:"
         , ":ORG_GLANCE_ID: shipped"
         , ":END:"
         , "* DONE Filed :web:ARCHIVE:"
         , ":PROPERTIES:"
         , ":ORG_GLANCE_ID: filed"
         , ":END:" ])
  (a, _hub) <- serverOver dir
  k a

-- | @\/@ in both modes: a shell that mounts the renderer, and a page that
-- explains where the renderer went.
pageSpec :: IO T.Text -> TestTree
pageSpec shell = testGroup "GET /"
  [ testCase "with assets, is a shell that fetches and mounts" $ do
      r <- get assetsDir "/"
      assertEqual "status" 200 (status r)
      assertEqual "content type" (Just "text/html; charset=utf-8") (header "Content-Type" r)
      assertContains "renderer" "src=\"table-view.js\"" (body r)
      assertContains "fetch glue" "fetch(`/headlines${params}`" (body r)
      assertContains "mount" "TableView.mount(" (body r)

  , testCase "with assets, the restored query is the renderer's own chips" $ do
      b <- shell
      -- The mount is handed the applied query; the renderer tokenizes it into
      -- committed chips and delivers nothing, since the rows in hand are
      -- already the server's answer to it.
      holdsAll "restore glue"
            [ "initialQuery: query,"
            -- An asset predating the option drops it silently, so the mount
            -- asks whether it took and stuffs the box when it did not.
            , "const holds = (q) => typeof table.getQuery === \"function\""
            , "&& table.getQuery() === q;"
            , "if (query && !holds(query)) showQuery();"
            , "function showQuery() {" ] b
      -- One restoration point: `start' re-fetches and re-mounts for every way
      -- back in, so it does not restore the query a second time itself.
      assertEqual "showQuery is called from the mount alone" 1
                  (T.count "!holds(query)) showQuery();" b)
      assertEqual "showQuery is defined once" 1 (T.count "function showQuery()" b)

  , testCase "with assets, DEL takes the last token off through the renderer" $ do
      b <- shell
      -- The chips are the renderer's, so the strip is too: the shell asks and
      -- then follows, rather than recomposing a query the chips would outlive.
      mapM_ (\needle -> assertContains "DEL glue" needle b)
            [ "table.stripLastToken()", "table.getQuery().trim()"
            , "filterDrop: (b) => {", "said(b, \"no filter\")"
            , "said(b, left ? `filter: ${JSON.stringify(left)}` : \"filter cleared\");"
            -- An asset without the pair says so instead of guessing.
            , "typeof table.stripLastToken === \"function\""
            , "this table-view.js has no filter tokens"
            -- One press, one token: a held DEL claims the key and runs once,
            -- where held movement keeps repeating.  The table is the blob's.
            , "if (!(e.repeat && MAPS.once.indexOf(hit.command) !== -1)) run(hit);" ]
      -- `D' is on the list for a different reason than the other two: it
      -- writes files, so a held key must not be a hundred /command requests.
      onceOf b >>= assertEqual "the commands auto-repeat is off for" onceNames
      -- The guard is per command, so it cannot take auto-repeat off movement.
      assertBool "the repeat guard is blanket rather than per command"
                 (not ("if (e.repeat) return" `T.isInfixOf` b))
      -- Neither of the two designs this replaced survives.
      holdsNone "a superseded filter path" ["glance-filter-history", "function withoutLast"] b

  , testCase "with assets, the sheet is buttonless and syncs on the way out" $ do
      b <- shell
      holdsAll "sheet glue"
            -- Dirty against the materialized original decides everything, and
            -- EITHER pane moving is dirty: a pristine close is no request at all.
            [ "const dirty = () => editing !== null"
            , "&& (el(\"mtext\").value !== base"
            , "|| (!raw && edited() !== baseProps));"
            , "if (!dirty()) { shut(); return; }"
            , "flush(editing.digest).then((ok) => ok && shut());"
            -- The backdrop is the mouse's ESC.
            , "if (e.target === el(\"modal\")) leave()"
            -- The receipt chains: the 200's digest is the next flush's lock, and
            -- both baselines move to what was actually sent.
            , "h.digest = a.body.digest;"
            , "base = raw ? sent.org : sent.body;"
            , "baseProps = raw ? null : JSON.stringify([sent.properties, sent.planning]);"
            -- A conflict keeps the sheet open and names the two keys.
            , "if (a.status === 409 && a.body.reason !== \"planning\") sync(\"conflict\");"
            , "conflict — C-x C-s overwrite · ESC discard"
            , "if (troubled()) {"
            , "append(\"sync\", \"info\", \"closed without writing — the file is as it was\");"
            -- And a tab closing on an edited sheet still owes the file.
            , "addEventListener(\"beforeunload\""
            , "post(editing.id, editing.digest, asked(), { keepalive: true })" ] b
      -- One word carries the sheet's state, `sync' is its only writer, and the
      -- states that wait for a key say which key.  No buttons to reach them with.
      holdsAll "sync status"
            [ "synced: \"synced\"", "syncing: \"syncing…\"", "id=\"mnote\""
            , "error: \"error — C-x C-s retry · ESC discard\" };"
            , "const troubled = () => state === \"conflict\" || state === \"error\";"
            , "const flushing = () => state === \"syncing\";"
            , "function sync(next, message) {", "state = next;" ] b
      -- Nothing else writes the word, so the header cannot disagree with it.
      assertEqual "sync is the only writer" 1 (T.count "      state = next;" b)
      holdsNone "a sheet button"
        [ "id=\"msave\"", "id=\"mcancel\"", "id=\"mredo\"", "id=\"mfoot\"", "Re-materialize" ] b

  , testCase "with assets, the page is one column the viewport tall" $ do
      b <- shell
      holdsAll "column"
            [ "height:100vh;box-sizing:border-box;overflow:hidden;"
            , "padding:34px 24px 24px;display:flex;flex-direction:column;gap:14px}"
            -- The table asks for its height and can give it back; the key line
            -- never gives any of its own up, so a short window squeezes the
            -- table rather than clipping the line.
            , "#app{height:80vh;min-height:0}"
            , "#kbd{flex:none;" ] b
      -- Table, log, key line, in that order — the corner and the pill are
      -- fixed and out of the column, and the sheet is display:none until it
      -- is not.
      let at needle = T.length (fst (T.breakOn needle b))
      assertBool ("app, log, kbd in that order: " <> show (at "id=\"app\"", at "id=\"log\"", at "id=\"kbd\""))
                 (at "id=\"app\"" < at "id=\"log\"" && at "id=\"log\"" < at "id=\"kbd\"")

  , testCase "with assets, the last line is the map, resident" $ do
      b <- shell
      holdsAll "key line"
            [ "<div id=\"kbd\"></div>"
            , "MAPS.rows.find((x) => x.command === command && x.scope === \"table\")"
            -- A staged row has no handler and is no offer.
            , "return b && b.handler ? b.seq : null;"
            , "el(\"kbd\").textContent = MAPS.hints" ] b
      -- Commands, not keys, in the order the line reads them: the table is the
      -- blob's and each spelling comes out of the one map.
      hints <- hintsOf b
      assertEqual "the key line's table"
        [ (["next-row", "previous-row"], "rows")
        , (["next-column", "previous-column"], "cells")
        -- The page pair reads open-then-close, so the line says `[/]' where
        -- the two above it read forward first.
        , (["previous-page", "next-page"], "pages")
        -- The one label carrying a second sentence: without it a reader takes
        -- `<' for a within-page key and never finds out that it climbs.
        , (["first-row", "last-row"], "first/last row, again = page up/down")
        , (["org-glance-overview:materialize"], "materialize")
        -- What a row points AT, beside what it IS: `RET' opens the entry and
        -- `o' follows it out.
        , (["org-glance-overview:open"], "open link")
        -- Four keys, one word: the line says `m/u/U/M mark' the way it says
        -- `n/p rows', since the group is one idea.
        , (["mark-toggle", "unmark", "unmark-all", "mark-all"], "mark")
        -- The structured commands, beside the keys that choose what they run
        -- over.
        , (["org-glance-overview:todo"], "state")
        , (["org-glance-overview:schedule", "org-glance-overview:deadline"]
          , "schedule/deadline")
        -- The one that names no row, so it is beside the others rather than
        -- among the keys that pick a set.
        , (["org-glance-overview:capture"], "capture")
        -- `state' runs over the MARKED set; archiving runs over the FLAGGED
        -- one, and reads as the two steps it is.
        , (["archive-flag"], "flag for archive")
        , (["archive-flag", "org-glance-overview:delete"], "archive flagged")
        , (["filter-rows"], "filter")
        , (["apply-default-filter"], "default view")
        -- The second canned view, next to the one `g' applies: both are a
        -- query, and the line says so by putting them together.
        , (["org-glance-agenda"], "agenda")
        , (["filter-drop-token"], "drop token")
        , (["customize"], "settings")
        , (["quit-window"], "quit")
        ] hints
      -- And every command it names is one the map binds, in the table scope,
      -- with a handler behind it.  A hint for anything else is an empty offer.
      rows <- keymapOf b
      let offered = [ c | (_k, _s, c, Just _h, "table", _help) <- rows ]
      assertEqual "hinted but unbound" []
        [ c | (cs, _label) <- hints, c <- cs, c `notElem` offered ]
      -- No literal key in the line: only the blob knows which key runs what.
      -- Nor does the transient log repeat what the resident line already says.
      holdsNone "the key line spells a key itself"
                ["\"n/p rows", "\"j/k rows", "RET materializes"] b

  , testCase "without assets, explains JSON-only mode" $ do
      r <- get missingAssetsDir "/"
      assertEqual "status" 200 (status r)
      assertEqual "content type" (Just "text/html; charset=utf-8") (header "Content-Type" r)
      assertContains "mode" "JSON-only" (body r)
      assertContains "flag" "--assets" (body r)
      assertContains "endpoint" "/headlines" (body r)
      assertBool "mounts a renderer it has not got"
                 (not ("TableView.mount(" `T.isInfixOf` body r))
  ]

-- | A keymap row as the suite spells it: the keys the dispatch matches, the
-- notation the echo widget shows, the command name, the handler behind it (or
-- none, for a binding no daemon command backs yet), the scope it is live in,
-- and the help line the echo widget adds where the command name is an Emacs
-- name for something narrower.
type Row = ([T.Text], T.Text, T.Text, Maybe T.Text, T.Text, Maybe T.Text)

-- | The shell's keymap, checked as the data it is.  The page carries the map
-- as a JSON blob and its own dispatch parses that blob, so reading it here
-- reads what the browser reads rather than what a grep for handler names would
-- find.
--
-- The expected map is written down rather than imported: the point of the
-- assertion is that the sequences and the org-glance command names are the
-- ones @org-glance-overview-mode-map@ spells, and an oracle taken from the
-- code under test would agree with any of them.
--
-- ONE map.  The movement profiles are gone: @n@\/@j@ both step a row and
-- @f@\/@l@ both step a cell, which is two rows apiece and no selector, no
-- stored choice and no URL parameter.
expectedRows :: [Row]
expectedRows =
  -- The letters lead: the resident key line shows the first row bound to a
  -- command, and `n/p rows' reads better there than `<down>/<up> rows'.
  [ (["n"],          "n",       "next-row",                        Just "nextRow",        "table", Nothing)
  , (["p"],          "p",       "previous-row",                    Just "previousRow",    "table", Nothing)
  , (["j"],          "j",       "next-row",                        Just "nextRow",        "table", Nothing)
  , (["k"],          "k",       "previous-row",                    Just "previousRow",    "table", Nothing)
  , (["<down>"],     "<down>",  "next-row",                        Just "nextRow",        "table", Nothing)
  , (["<up>"],       "<up>",    "previous-row",                    Just "previousRow",    "table", Nothing)
  , (["f"],          "f",       "next-column",                     Just "nextColumn",     "table", rightHelp)
  , (["b"],          "b",       "previous-column",                 Just "previousColumn", "table", leftHelp)
  , (["l"],          "l",       "next-column",                     Just "nextColumn",     "table", rightHelp)
  , (["h"],          "h",       "previous-column",                 Just "previousColumn", "table", leftHelp)
  , (["<"],          "<",       "first-row",                       Just "firstRow",       "table", topHelp)
  , ([">"],          ">",       "last-row",                        Just "lastRow",        "table", endHelp)
  , (["G"],          "G",       "last-row",                        Just "lastRow",        "table", endHelp)
  , (["]"],          "]",       "next-page",                       Just "nextPage",       "table", Nothing)
  , (["["],          "[",       "previous-page",                   Just "previousPage",   "table", Nothing)
  , (["RET"],        "RET",     "org-glance-overview:materialize", Just "materializeRow", "table", Nothing)
  , (["/"],          "/",       "filter-rows",                     Just "focusFilter",    "table",
       Just "summon the filter palette")
  , (["DEL"],        "DEL",     "filter-drop-token",               Just "filterDrop",     "table",
       Just "drop the filter's last token")
  , (["g"],          "g",       "apply-default-filter",            Just "applyDefault",   "table",
       Just "the view this tree opens on")
  , (["m"],          "m",       "mark-toggle",                     Just "markToggle",     "table",
       Just "toggle this row's mark, then step down")
  , (["u"],          "u",       "unmark",                          Just "unmarkRow",      "table",
       Just "take this row's archive flag off, else its mark, then step down")
  , (["U"],          "U",       "unmark-all",                      Just "unmarkAll",      "table",
       Just "every mark and every archive flag off")
  , (["M"],          "M",       "mark-all",                        Just "markAll",        "table",
       Just "mark every row loaded")
  , (["q"],          "q",       "quit-window",                     Just "quitWindow",     "table", Nothing)
  , (["TAB"],        "TAB",     "org-cycle",                       Nothing,               "table", Nothing)
  -- Where the row points, out of its own subtree.  Two spellings of one
  -- command, so one help line.
  , (["o"],          "o",       "org-glance-overview:open",        Just "openLinks",      "table", openHelp)
  , (["!"],          "!",       "org-glance-overview:open",        Just "openLinks",      "table", openHelp)
  -- A canned VIEW rather than a mode: one query, applied the way `g' applies
  -- the tree's default.
  , (["a"],          "a",       "org-glance-agenda",               Just "applyAgenda",    "table",
       Just "the active rows carrying a date, earliest first")
  , (["@"],          "@",       "org-glance-overview:relations",   Nothing,               "table", Nothing)
  -- The one write that names no row: it makes one, in the file the tree's own
  -- @#+GLANCE_CAPTURE_TARGET:@ names.
  , (["+"],          "+",       "org-glance-overview:capture",     Just "capture",        "table",
       Just "a headline for the inbox, typed as org")
  -- dired's flag, in two presses: the first marks the row for archiving and the
  -- second does it.  Plain @d@ is never a write on its own.
  , (["d"],          "d",       "archive-flag",                    Just "archiveFlag",    "table",
       Just "flag for archive; d again archives all flagged")
  -- org-glance's own name for dired's key, and a help line because what it
  -- does here is narrower than the name: the headline is tagged, never removed.
  , (["D"],          "D",       "org-glance-overview:delete",      Just "archiveRows",    "table",
       Just "archive the flagged rows, or the row at point \8212 never a delete")
  , (["t"],          "t",       "org-glance-overview:todo",        Just "setState",       "table",
       Just "set the state of the marked rows, or the row at point")
  , (["C-c", "C-t"], "C-c C-t", "org-glance-overview:todo",        Just "setState",       "table",
       Just "the org spelling, where the browser lets it through")
  -- Both of these survive the browser where @C-c C-t@ does not: @Ctrl+S@ and
  -- @Ctrl+D@ are page default actions rather than chrome shortcuts.
  , (["C-c", "C-s"], "C-c C-s", "org-glance-overview:schedule",    Just "schedulePlan",   "table",
       planHelp)
  , (["C-c", "C-d"], "C-c C-d", "org-glance-overview:deadline",    Just "deadlinePlan",   "table",
       planHelp)
  -- Emacs's own name, since org-glance has no settings command and inventing
  -- one would put a name in this table that no map anywhere carries.
  , ([","],          ",",       "customize",                       Just "openSettings",   "table",
       Just "the keyword cycles and the default view, a config layer at a time")
  , (["C-x", "C-s"], "C-x C-s", "save-buffer",                     Just "save",           "modal",
       Just "sync the sheet now; again to overwrite a conflict")
  , (["C-c", "'"],   "C-c '",   "org-edit-special",                Just "toggleRaw",      "modal",
       Just "the sheet as raw org, or as body and properties; sync an edited one first")
  , (["ESC"],        "ESC",     "keyboard-quit",                   Just "cancel",         "any",
       Just "close the sheet, syncing an edited one; again to discard")
  ]
  where rightHelp = Just "the cell to the right; row movement keeps the column"
        leftHelp  = Just "the cell to the left; from a whole row, the first column"
        topHelp   = Just "first row, again = page up"
        endHelp   = Just "last row, again = page down"
        planHelp  = Just "a date over the marked rows, or the row at point; empty clears it"
        openHelp  = Just "follow this row's link; several raise the palette"

-- | The keymap blob out of SHELL, parsed.  Everything the dispatch reads is in
-- here, so the assertions below are over data rather than over the spelling of
-- a JS literal.
blobOf :: T.Text -> IO Value
blobOf shell = keysOf shell >>= \raw ->
  either (\e -> assertFailure ("keymap JSON: " <> e)) pure
         (eitherDecode (BL.fromStrict (TE.encodeUtf8 raw)))

-- | SHELL's keymap blob as it stands in the page, undecoded — what the glue
-- itself parses out of the document.
keysOf :: T.Text -> IO T.Text
keysOf shell = maybe (assertFailure "no keymap blob in the shell") pure
                     (between "<script id=\"keys\" type=\"application/json\">" "</script>" shell)

-- | The resident key line's table out of SHELL: the commands it names, in the
-- order the line reads them, each with its label.
hintsOf :: T.Text -> IO [([T.Text], T.Text)]
hintsOf shell = traverse one =<< listAt "hints" =<< blobOf shell
  where one v = (,) <$> textsAt "commands" v <*> textAt "label" v

-- | The chords SHELL's blob declares never claimed.
reservedOf :: T.Text -> IO [T.Text]
reservedOf shell = textsAt "reserved" =<< blobOf shell

-- | The commands SHELL's blob declares auto-repeat is off for.
onceOf :: T.Text -> IO [T.Text]
onceOf shell = textsAt "once" =<< blobOf shell

-- | The keymap blob out of SHELL: the one row list the dispatch reads.
keymapOf :: T.Text -> IO [Row]
keymapOf shell = traverse row =<< listAt "rows" =<< blobOf shell
  where
    row v = (,,,,,) <$> textsAt "keys" v <*> textAt "seq" v <*> textAt "command" v
                    <*> maybeTextAt "handler" v <*> textAt "scope" v
                    <*> maybeTextAt "help" v

-- | The shell's inline glue, on its own — what a syntax check is run over.
glueOf :: T.Text -> IO T.Text
glueOf shell = maybe (assertFailure "no inline script in the shell") pure
                     (between "\n  <script>\n" "  </script>" shell)

keymapSpec :: IO T.Text -> TestTree
keymapSpec shell = testGroup "Shell keymap"
  [ testCase "is one JSON blob, in org-glance's own command names" $ do
      rows <- keymapOf =<< shell
      assertEqual "the map, whole" expectedRows rows

  , testCase "there is one map, and the profile machinery is gone with it" $ do
      b <- shell
      holdsNone "a movement profile"
        [ "MAPS.profiles", "MAPS.default", "\"profiles\":", "\"shared\":"
        , "glance-keys", "keysel", "setProfile", "?keys=" ] b
      -- The dispatch and the key line read the one list.
      holdsAll "the one list" ["MAPS.rows.filter(live)", "MAPS.rows.find("] b

    -- The echo speaks the FUNCTION NAME, verbatim.  A rebinding config will
    -- address a command by exactly this string, so a reader who learns one off
    -- the pill has to be able to type it — which rules out the prose spelling
    -- (`> → last row' for `last-row').  Two halves, and together they close it:
    -- every emission puts `${b.command}' in the slot after the arrow, and no
    -- command in the blob carries a space, so the slot cannot hold prose.  What
    -- follows is a bracketed outcome and is prose on purpose.
  , testCase "every echo names the command it ran, verbatim" $ do
      inline <- glueOf =<< shell
      let after = drop 1 (T.splitOn "${b.seq} → " inline)
          slots = [ T.takeWhile (/= '`') s | s <- after ]
      assertBool "no keyed echo at all — the sweep read nothing"
                 (not (null slots))
      assertEqual "an arrow slot that is not the command" []
                  [ s | s <- slots, not ("${b.command}" `T.isPrefixOf` s) ]
      -- The one echo written without a binding in hand names its command too.
      assertContains "ESC's own echo" "ESC → keyboard-quit (row unchanged)" inline
      rows <- keymapOf =<< shell
      assertEqual "a command name that cannot be typed as one" []
                  [ c | (_k, _s, c, _h, _sc, _help) <- rows, " " `T.isInfixOf` c ]

  , testCase "nothing is bound twice, and no sequence hides a longer one" $ do
      rows <- keymapOf =<< shell
      let bound = [ k | (k, _, _, _, _, _) <- rows ]
          twice = [ k | k <- nub bound, length (filter (== k) bound) > 1 ]
          -- A complete sequence that also opens a longer one would match first
          -- and leave the longer one unreachable.
          eaten = [ (k, l) | k <- bound, l <- bound, k /= l, k == take (length k) l ]
      assertEqual "bound twice" [] twice
      assertEqual "swallows a longer sequence" [] eaten
      -- Two spellings of one command is the point of the unified map, so the
      -- pairs are asserted rather than left to the absence of a duplicate.
      assertEqual "row movement has both spellings, the letter first"
        [["n"], ["j"], ["<down>"]]
        [ k | (k, _s, c, _h, _scope, _help) <- rows, c == "next-row" ]
      assertEqual "cell movement has both spellings"
        [["f"], ["l"]]
        [ k | (k, _s, c, _h, _scope, _help) <- rows, c == "next-column" ]

  , testCase "the status corner carries the dot and the theme, in that order" $ do
      b <- shell
      corner <- maybe (assertFailure "no status corner in the shell") pure
                      (between "<div id=\"corner\">" "</div>" b)
      holdsAll "corner" ["id=\"dot\"", "id=\"themesel\""] corner
      assertContains "fixed in the corner" "#corner{position:fixed;top:12px;right:14px" b
      let at needle = T.length (fst (T.breakOn needle corner))
      assertBool ("dot then theme: " <> show corner)
                 (at "id=\"dot\"" < at "id=\"themesel\"")

  , testCase "the view title is the tab's alone, and nothing on the page repeats it" $ do
      b <- shell
      -- The table is the top of the page; a heading repeating the tab title put
      -- the same string on screen twice.
      assertContains "palette" "palette: true," b
      assertBool ("a heading survives in the shell: " <> show (between "<h1>" "</h1>" b))
                 (not ("<h1>" `T.isInfixOf` b))
      -- Written down rather than taken from the code that writes it into the
      -- page: an oracle calling 'viewTitleFor' agrees with whatever it returns.
      assertEqual "the tab title" "test/fixtures/view — glance" (viewTitleFor viewDir)
      assertEqual "the title, once in the document" 1
                  (T.count "test/fixtures/view — glance" b)

  , testCase "the prefix keys are claimed only where they are ours" $ do
      b <- shell
      holdsAll "chord policy"
        -- A selection keeps C-c and C-x as copy and cut; the reserved chords
        -- reach the browser when they abandon a claimed prefix, which is why
        -- neither profile moves on C-n or C-p.
        [ "if (!selecting()) { e.preventDefault();"
        , "if (MAPS.reserved.indexOf(k) === -1) e.preventDefault();" ] b
      reservedOf b >>= assertEqual "the chords never claimed on their own"
        ["C-l", "C-r", "C-t", "C-w", "C-n", "C-p", "<f5>"]
      -- None of them is bound, so the guard is the only thing that decides.
      rows <- keymapOf b
      reserved <- reservedOf b
      assertEqual "a reserved chord is bound" []
        [ k | (k, _s, _c, _h, _scope, _help) <- rows, k `elem` map pure reserved ]

  , testCase "the writes are the commands auto-repeat is off for" $ do
      b <- shell
      -- `d' most of all: a held key that survived here would flag a row and
      -- archive it from ONE press, which is the confirmation the two-press
      -- shape exists to be.
      onceOf b >>= assertEqual "once" onceNames
      rows <- keymapOf b
      once <- onceOf b
      assertEqual "a command is on the once list and unbound" []
        [ c | c <- once, c `notElem` [ x | (_k, _s, x, _h, _scope, _help) <- rows ] ]

  , testCase "the inline glue is JavaScript, where there is a node to say so" $ do
      node <- findExecutable "node"
      case node of
        -- No node on this machine: the syntax of the glue is checked wherever
        -- there is one, and the rest of this group still reads it as text.
        Nothing  -> pure ()
        Just exe -> withTempDir $ \dir -> do
          inline <- glueOf =<< shell
          let path = dir </> "shell.js"
          TIO.writeFile path inline
          (code, _out, err) <- readProcessWithExitCode exe ["--check", path] ""
          assertEqual ("node --check said: " <> err) ExitSuccess code
  ]

-- | What a coarse pointer gets, and what a fine one is spared.  Keys are the
-- interface wherever there are keys; a touch device is the one place they
-- cannot reach, so the filter earns a tap target there and nowhere else.
touchSpec :: IO T.Text -> TestTree
touchSpec shell = testGroup "Touch"
  [ testCase "every page this server serves lays out at the device's own width" $ do
      withAssets <- shell
      bare <- body <$> get missingAssetsDir "/"
      -- Without it a phone lays the page out at 980px and scales it down, so
      -- the table renders at a third of the size it asked for.
      mapM_ (\(what, page') ->
               assertContains what
                 "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
                 page')
            [("the shell", withAssets), ("the JSON-only page", bare)]

  , testCase "a fine pointer sees none of it" $ do
      b <- shell
      -- Everything above is inside the query, and the handler asks the same
      -- query before it runs: with a mouse the page is what it always was.
      let (before, coarse') = T.breakOn "@media (pointer:coarse){" b
      assertBool "no coarse block in the page" (not (T.null coarse'))
      mapM_ (\needle -> assertBool ("a touch rule outside the query: " <> show needle)
                                   (not (needle `T.isInfixOf` before)))
            ["min-height:44px", "#mtext,#pinput{font-size:16px}", "tv-chips:empty"]
      assertEqual "one coarse block, and one gate on it" 1
                  (T.count "@media (pointer:coarse){" b)
  ]

-- | The shell is monospace, and gets there without asking the network for it.
shellFontSpec :: IO T.Text -> TestTree
shellFontSpec shell = testGroup "Shell type"
  [ testCase "a font in the assets directory is declared and served" $
      withTempDir $ \dir -> do
        TIO.writeFile (dir </> "table-view.js") ""
        BS.writeFile (dir </> "JetBrainsMono-Regular.woff2") "wOF2"
        b <- body <$> get dir "/"
        assertContains "declared" "@font-face{font-family:\"JetBrains Mono\"" b
        assertContains "from this server, by name"
                       "src:url(\"JetBrainsMono-Regular.woff2\") format(\"woff2\")" b
        r <- get dir "/JetBrainsMono-Regular.woff2"
        assertEqual "status" 200 (status r)
        assertEqual "content type" (Just "font/woff2") (header "Content-Type" r)

  , testCase "no page this server serves reaches off it" $ do
      withAssets <- shell
      bare <- body <$> get missingAssetsDir "/"
      mapM_ (\(what, page') -> holdsNone (what <> " fetches")
                                         ["http://", "https://", "@import"] page')
            [("the shell", withAssets), ("the JSON-only page", bare)]
  ]

-- | Assets come out of the configured directory, and only from there.
assetSpec :: TestTree
assetSpec = testGroup "Assets"
  [ testCase "the renderer is served as JavaScript" $ do
      r <- get assetsDir "/table-view.js"
      assertEqual "status" 200 (status r)
      assertEqual "content type"
                  (Just "text/javascript; charset=utf-8") (header "Content-Type" r)

  , testCase "a file the assets directory lacks is a 404" $ do
      r <- get assetsDir "/table-view.css"
      assertEqual "status" 404 (status r)

  , testCase "a traversal segment is not a file name" $ do
      r <- get assetsDir "/.."
      assertEqual "status" 404 (status r)
  ]

-- | The renderer the binary carries.  Without @--assets@ there is no directory
-- to find and nothing to find it in: a @glance@ copied anywhere serves the same
-- page, which is what makes @--assets@ a development flag rather than the way
-- the program is normally run.
embeddedSpec :: TestTree
embeddedSpec = testGroup "Embedded renderer"
  [ testCase "with no --assets, /table-view.js is the vendored file byte for byte" $ do
      r <- getBuiltIn "/table-view.js"
      assertEqual "status" 200 (status r)
      vendored <- BS.readFile vendoredRenderer
      assertEqual "the bytes `make sync-renderer' put in the tree"
                  vendored (BL.toStrict (simpleBody r))
      -- Big enough that a truncated or placeholder embed cannot pass the
      -- comparison above by both sides being empty.
      assertBool "and it is a renderer" (BS.length vendored > 100000)

  , testCase "served with the content type and the length a file would be" $ do
      r <- getBuiltIn "/table-view.js"
      assertEqual "content type"
                  (Just "text/javascript; charset=utf-8") (header "Content-Type" r)
      size <- BS.length <$> BS.readFile vendoredRenderer
      assertEqual "Content-Length"
                  (Just (BSC.pack (show size))) (header "Content-Length" r)

  , testCase "and compressed for a client that asks, the way the file was" $ do
      a <- appOf builtIn
      zipped <- getWith a "/table-view.js" [("Accept-Encoding", "gzip")]
      assertEqual "status" 200 (status zipped)
      assertEqual "Content-Encoding" (Just "gzip") (header "Content-Encoding" zipped)
      assertEqual "Vary" (Just "Accept-Encoding") (header "Vary" zipped)

  , testCase "so / is the shell, and the JSON-only page is unreachable" $ do
      b <- body <$> getBuiltIn "/"
      assertContains "renderer" "src=\"table-view.js\"" b
      assertContains "mount" "TableView.mount(" b
      holdsNone "the JSON-only page" ["JSON-only mode"] b

  , testCase "--assets replaces the compiled-in renderer rather than adding to it" $ do
      r <- get assetsDir "/table-view.js"
      stub <- BS.readFile (assetsDir </> "table-view.js")
      vendored <- BS.readFile vendoredRenderer
      assertEqual "the directory's own file" stub (BL.toStrict (simpleBody r))
      assertBool "which is not the compiled-in one" (stub /= vendored)

  , testCase "and an --assets directory without one still gets the JSON-only page" $ do
      b <- body <$> get missingAssetsDir "/"
      assertContains "the JSON-only page" "JSON-only mode" b
      assertContains "the directory it looked in" (T.pack missingAssetsDir) b
      holdsNone "no table" ["TableView.mount("] b

  , testCase "with no --assets the renderer is the only asset there is" $ do
      -- Nothing else is compiled in, so nothing else can be asked for: the
      -- font stays an `--assets' affordance and is not invented here.
      mapM_ (\name -> do r <- getBuiltIn name
                         assertEqual (show name) 404 (status r))
            ["/table-view.css", "/JetBrainsMono-Regular.woff2", "/.."]
      b <- body <$> getBuiltIn "/"
      holdsNone "no @font-face without a file to point at" ["@font-face"] b
  ]

errorSpec :: TestTree
errorSpec = testGroup "Errors"
  [ testCase "an unknown path is a 404" $ do
      r <- get assetsDir "/graph"
      assertEqual "status" 404 (status r)
      assertContains "hint" "/headlines" (body r)

  , testCase "a write to a read route is refused" $ do
      let req = (setPath defaultRequest "/headlines") { requestMethod = methodPost }
      application' <- app assetsDir
      r <- runSession (request req) application'
      assertEqual "status" 405 (status r)
      assertContains "hint" "/headline" (body r)

  , testCase "/headline takes GET and POST, and nothing else" $ do
      let req = (setPath defaultRequest "/headline") { requestMethod = methodDelete }
      application' <- app assetsDir
      r <- runSession (request req) application'
      assertEqual "status" 405 (status r)

  , testCase "/ws without an upgrade says what it wants" $ do
      r <- get assetsDir "/ws"
      assertEqual "status" 400 (status r)
      assertContains "hint" "websocket" (body r)
  ]
