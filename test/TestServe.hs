-- | The server, driven as a WAI 'Application'.  No socket is bound: every case
-- here is a request handed straight to the app, so the suite stays free of
-- ports and of the races that come with them.  The websocket route is the one
-- thing an upgrade-less request cannot reach, and the frames it would carry
-- are TestStore's subject.
module TestServe (spec) where

import Control.Monad (filterM, forM_, (<=<))
import Data.Aeson ( FromJSON, Value (Bool, Null, Number, Object, String)
                  , eitherDecode, encode, object, parseJSON, (.=) )
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.Char (isDigit)
import Data.List (elemIndex, find, isInfixOf, nub, sort, sortOn)
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
import TestDefaults ( boolAt, document, field, intAt, listAt, maybeTextAt
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
sampleDigest = "ba16aa19887a04a410a1f0047b4fcee147818d0c8471e4e1db60f5bc7dfe22dc"

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

-- | KEY of every @POST \/headline@ body the sheet sent, as pairs.  Six cases
-- ask what a sync wrote into one of the two lists.
wroteAt :: T.Text -> Value -> IO [[[T.Text]]]
wroteAt key = traverse (pairsAt key) <=< listAt "writes"

-- | The @SCHEDULED@ stamp the shell harness's fixture headline carries.
sheetStamp :: T.Text
sheetStamp = "<2026-08-01 Sat>"

-- | What the sheet's property panel shows over that fixture: org's three
-- planning rows with @SCHEDULED@ holding SCHED, then PROPS.  Fourteen cases
-- assert some shape of this one drawer, so it is spelled once here.
panelRows :: T.Text -> [[T.Text]] -> [[T.Text]]
panelRows sched props =
  [["SCHEDULED", sched], ["DEADLINE", ""], ["CLOSED", ""]] <> props

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

-- | ROW under the first two keys of the chain the view declares
-- ('Glance.Query.defaultSortChain'): its STATE by palette position, then its
-- title folded.  Every row of this fixture carries a different state, so the
-- leading key settles the order on its own and the tie-breakers behind it never
-- fire; the title is here because the ONE stateless row would otherwise tie
-- with itself.  An empty state sorts past every keyword, which is the nulls
-- rule read for this one key.
--
-- A page has to come out of that order — @\/headlines@ with no @limit@ answers
-- in walk order for the client to sort, so this is what a paged answer is
-- measured against.  An independent oracle rather than a call: it spells the
-- fixture's own palette out ('samplePalette') instead of asking the code under
-- test which order it meant.
sortKeyOf :: Value -> (Int, T.Text)
sortKeyOf row = (statePos (cellOf "state"), T.toCaseFold (cellOf "title"))
  where
    statePos s = fromMaybe (length samplePalette) (elemIndex s samplePalette)
    cellOf key = case row of
      Object o -> case KM.lookup "cells" o of
        Just (Object cells) -> case KM.lookup key cells of
          Just (String s) -> s
          _noCell         -> ""
        _noCells -> ""
      _notARow -> ""

-- | The keywords @test\/fixtures\/view@ declares, in the order its @#+TODO:@
-- line spells them — which is the badge palette, and so the order the state
-- column sorts in.
samplePalette :: [T.Text]
samplePalette = ["NEXT", "TODO", "WAITING", "CANCELLED", "DONE"]

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
    , orderSpec, sortQuerySpec, archiveViewSpec
    , bootstrapSpec, materializeSpec, commitSpec, commandSpec, planningSpec
    , tagCommandSpec, renameCommandSpec, tagsSpec, captureSpec
    , configSpec, keywordsSpec, linksSpec, editLinkSpec, indexingSpec
    , pageSpec shell, keymapSpec shell
    , glueSpec shell, bootSpec shell, liveSpec shell, washSpec shell
    , paletteSpec shell
    , moveSpec shell, sortKeySpec shell, markSpec shell, landingSpec shell
    , commandKeySpec shell, promptKeySpec shell, whichKeySpec shell, tagKeySpec shell
    , openKeySpec shell, agendaSpec shell, drillSpec shell, logSpec shell
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
      -- The boot's two, then the remount's ONE.  A re-application has a whole
      -- table standing, so it asks for the whole answer and swaps on it —
      -- where the boot's page-sized fetch buys a first paint, this one would
      -- buy a complete table replaced by a partial one.  It arms nothing
      -- either: the parity baseline was fetched by the boot and a remount does
      -- not throw it away.
      , "/headlines?q=state%3A*active*" ]
      "?q=state%3A*active*"

  -- On a page already showing it, `g' is the same round trip rather than a
  -- no-op: it is a remount, and the URL it lands on is the one it wrote.
  , Boot "and re-applies it over a deep link that narrowed past it"
      "?q=tanik" 500 "g"
      [ "/headlines?q=tanik&limit=100", "/headlines?q=tanik", "/headlines"
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
    -- The remount asks ONE fetch, not the boot's pair: the table it is
    -- replacing is already whole, so it swaps on the whole answer.
  , Live "columns that moved rebuild the mount, close reason or none"
      "" "" "recolumn close:resync"
      (booted <> [reasked, reasked]) ["\"t0\""] 2 "" "" "?q=state%3A*active*"

    -- The killing case: a `view-changed' close mid-edit.  The mount goes, and
    -- the text the reader had not saved comes back with it.
  , Live "view-changed mid-edit rebuilds the mount and keeps the sheet's text"
      "" "Enter" "sheet:hello close:view-changed"
      (booted <> [reasked]) [] 2 "hello" "synced" "?q=state%3A*active*"

    -- And when the file moved under the open sheet, the restore says so rather
    -- than flushing over it later: the text stands, at `conflict'.
  , Live "a sheet restored over a moved file lands in the conflict flow"
      "" "Enter" "sheet:hello rewritten close:view-changed"
      (booted <> [reasked]) [] 2 "hello" "conflict" "?q=state%3A*active*"

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

-- | THE STALE WASH, and the paint discipline under it.
--
-- Two things stop what is on screen being known to be current — a view being
-- replaced, and a socket that would deliver a change being gone — and they wear
-- one look, carried by one class on the document element.  Each arms on a delay,
-- which is the whole of what keeps it off a page that is working; whoever arms
-- one is who clears it.
--
-- What the harness can see of the CSS is nothing, so the selectors and the
-- exemptions are 'shellGlue''s rows.  What it can see is the class going on and
-- coming off, and the row counts the table was handed on the way, which is where
-- the paint discipline is: a view swaps ON ITS ANSWER, in one mount.
washSpec :: IO T.Text -> TestTree
washSpec shell = testGroup "Shell wash"
  [ -- The bug this group exists for.  `g' over a table that is already whole
    -- used to fetch a PAGE, mount that, and pull the rest in behind it, so a
    -- complete view was replaced by a hundred rows and reflowed a moment later.
    -- One fetch, one mount, and the count the table was handed never drops.
    testCase "g swaps a view in one mount, and never through a partial one" $
      bootOf shell "" 500 "" "rows:150 press:g" $ \answer -> do
        paints <- paintsOf answer
        -- The boot's page and the boot's rest over three rows, the store grown
        -- to a hundred and fifty, then the swap — ONE entry.  A page-sized
        -- fetch here would put a `100' between the last two, which is the
        -- complete table replaced by a partial one.
        assertEqual "the boot's two, then the swap" [3, 3, 150] paints
        assertEqual "the table was built twice" 2 =<< intAt "mounts" answer
        assertBool ("no paint was empty: " <> show paints) (0 `notElem` paints)

    -- A commit REPAINTS rather than remounting, and the answer is ONE
    -- `setRows': the rows standing are the last answer until the next one is in
    -- hand.  `DEL' is the commit this suite can drive — it strips a token and
    -- commits what is left, the same door a palette commit goes through.
  , testCase "a commit that repaints hands over one set of rows" $
      bootOf shell "?q=tanik%20web" 500 "" "rows:150 press:Backspace" $ \answer -> do
        paints <- paintsOf answer
        assertEqual "the boot's two, then the commit's one" [3, 3, 150] paints
        assertEqual "and no remount" 1 =<< intAt "mounts" answer

    -- The grace is the whole of what keeps the wash off a page that is working.
    -- Every answer here is a microtask, so this is the ordinary case: a boot, a
    -- swap and a reconnect, and nothing is ever dimmed.
  , testCase "a page that answers dims nothing at all" $
      bootOf shell "" 500 "" "press:g close:resync" $ \answer -> do
        assertEqual "no transition" [] =<< textsAt "washed" answer
        assertEqual "and nothing left on" False =<< boolAt "stale" answer

    -- A view whose answer is out past the grace: the rows standing are stale
    -- and say so, and the answer takes it back.
  , testCase "a swap out past the grace dims the page, and its answer clears it" $
      bootOf shell "" 500 "" "hang press:g wait:400 deliver" $ \answer -> do
        assertEqual "armed, then cleared" ["on", "off"] =<< textsAt "washed" answer
        assertEqual "nothing left on" False =<< boolAt "stale" answer

    -- The COUNT is what the second half of that is for: `load' aborts the fetch
    -- before it, so an abort and the fetch replacing it overlap, and a boolean
    -- would clear the wash the replacement still wants.  Two swaps under one
    -- hang is exactly that overlap.
  , testCase "an abort hands the wash to the fetch that replaced it" $
      bootOf shell "" 500 "" "hang press:g wait:400 press:g wait:100 deliver" $
        \answer -> do
          assertEqual "one arming and one clearing, no flap"
                      ["on", "off"] =<< textsAt "washed" answer
          assertEqual "nothing left on" False =<< boolAt "stale" answer

    -- The other half of the grace: a reconnect that costs one revalidation is
    -- over long before the socket's delay, so a blip dims nothing.
  , testCase "a socket blip inside its delay dims nothing" $
      bootOf shell "" 500 "" "close:resync wait:500" $ \answer -> do
        assertEqual "no transition" [] =<< textsAt "washed" answer
        assertEqual "and nothing left on" False =<< boolAt "stale" answer

    -- A socket that stays gone is the one a reader can sit in for minutes: the
    -- page goes on showing rows nothing can correct, and the wash is what says
    -- so.  The daemon comes back, the retry behind the backoff finds it, and
    -- the socket that opens is what takes the wash off.
  , testCase "a socket that stays gone dims the page, and the reconnect clears it" $
      bootOf shell "" 500 "" "offline close:x wait:500 online wait:900" $ \answer -> do
        assertEqual "armed, then cleared" ["on", "off"] =<< textsAt "washed" answer
        assertEqual "nothing left on" False =<< boolAt "stale" answer

    -- And it stays on for as long as the socket is gone: the arming is not a
    -- flash that goes by itself.
  , testCase "and stays on while it is still gone" $
      bootOf shell "" 500 "" "offline close:x wait:500" $ \answer -> do
        assertEqual "armed and standing" ["on"] =<< textsAt "washed" answer
        assertEqual "still on" True =<< boolAt "stale" answer

    -- A sheet open over stale rows is stale with them.  The class is the
    -- DOCUMENT's, so it reaches the overlays without this page naming one —
    -- which selectors it reaches them by is `shellGlue''s row.
  , testCase "an open sheet is washed with the rows under it" $
      bootOf shell "" 500 "Enter" "offline close:x wait:500" $ \answer -> do
        assertEqual "the sheet is still up" "on" =<< textAt "modal" answer
        assertEqual "and the page is washed" True =<< boolAt "stale" answer
  ]

-- | Every row count the page handed the TABLE, in order: one per mount and one
-- per @setRows@.  A view arriving in one piece is one entry.
paintsOf :: Value -> IO [Int]
paintsOf answer = traverse count =<< listAt "paints" answer
  where count (Number n) = pure (round n)
        count other = assertFailure ("expected a row count, got " <> show other)

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

    -- The arrows walk BOTH axes, and silently: the key line shows a command's
    -- first binding, so `<right>' sits behind `f' the way `<down>' has always
    -- sat behind `n'.  Same handler, so walking off the last cell is the
    -- LANDING it is for the letters — the renderer reads a column index outside
    -- the table as no column at all — rather than a wall this page invents.
  , testCase "the arrows step the column too, and land off the ends" $ do
      bootOf shell "" 500 "" "press:ArrowRight" $ \answer -> do
        assertEqual "the first column, from the whole-row look" 0 =<< intAt "col" answer
        assertEqual "named by the header over it" "<right> → next-column (state)"
          =<< textAt "echo" answer
      bootOf shell "" 500 "" "press:ArrowRight press:ArrowRight" $
        assertEqual "and the next one" 1 <=< intAt "col"
      -- Two columns, so the third step walks off the end and lands.
      bootOf shell "" 500 "" "press:ArrowRight press:ArrowRight press:ArrowRight" $ \answer -> do
        assertEqual "off the cells" Null =<< field "col" answer
        assertEqual "which the echo says is a landing"
                    "<right> → next-column (row mode)" =<< textAt "echo" answer
      bootOf shell "" 500 "" "press:ArrowLeft" $
        assertEqual "and the other arrow lands on the first column too" 0 <=< intAt "col"

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

  ]

-- | Nine rows over three pages, then SCRIPT.  Every case here needs a set with
-- pages in it, and the harness's three rows are one page whatever the size.
moveScript :: T.Text -> T.Text
moveScript script = "rows:9 paged:3 " <> script

-- | @^@: the order the rows are in, over the column the cell keys picked.
--
-- Three rules, and the renderer decides all three.  WHICH column is the cell
-- selection's, so a whole-row selection is a refusal rather than a guess.
-- WHETHER it sorts is the column's own @sortable@ — the renderer's opt-in,
-- which @sortPromote@ gates but this page still names, so the refusal can
-- speak.  @^@ PROMOTES: the column at point heads the chain ascending (the
-- rest shift down, deduped), and on the column already leading it flips that
-- key alone — composing a chain is pressing over columns in reverse priority
-- order, the web's spelling of table-view.el's @C-u ^@.  The record of what
-- is in force is the handle's own (@getSort@); this page keeps none.
sortKeySpec :: IO T.Text -> TestTree
sortKeySpec shell = testGroup "Shell sort"
  [ testCase "sorts by the column at point: the leader flips in place" $
      bootOf shell "" 500 "f ^" "" $ \answer -> do
        assertEqual "the view's chain opens on state, so the press flips it"
                    (Just ("state", False)) =<< sortOf answer
        assertEqual "and the echo speaks the direction it landed in"
                    "^ → toggle-sort (state ▼)" =<< textAt "echo" answer

  , testCase "a second press flips the leader back, and a third again" $ do
      bootOf shell "" 500 "f ^ ^" "" $ \answer -> do
        assertEqual "the leader flips alone" (Just ("state", True)) =<< sortOf answer
        assertEqual "the echo" "^ → toggle-sort (state ▲)" =<< textAt "echo" answer
      bootOf shell "" 500 "f ^ ^ ^" "" $
        assertEqual "and round again" (Just ("state", False)) <=< sortOf

    -- The column is the renderer's, so a selection that names none is a
    -- question this page cannot answer: it says which key answers it instead of
    -- picking a column on the reader's behalf.
  , testCase "a whole-row selection names no column, and the key says which picks one" $
      bootOf shell "" 500 "^" "" $ \answer -> do
        assertEqual "nothing was asked of the renderer" 0 =<< intAt "sortCalls" answer
        assertEqual "the echo names the key that picks a column"
                    "^ → toggle-sort (no column selected — f/l to pick one)"
                    =<< textAt "echo" answer

    -- `sortable' gates what a READER may reach and `sortBy' ignores it, so a
    -- page driving a reader's key is the only thing that can honour it.
  , testCase "a column that declares no sortable is left alone" $
      bootOf shell "" 500 "f f ^" "" $ \answer -> do
        assertEqual "the column the cursor is in" 1 =<< intAt "col" answer
        assertEqual "nothing was asked of the renderer" 0 =<< intAt "sortCalls" answer
        assertEqual "and the echo names it"
                    "^ → toggle-sort (tag does not sort)" =<< textAt "echo" answer

  , testCase "an asset with no programmatic sort is named, not crashed into" $
      bootOf shell "" 500 "" "sortless press:f press:^" $ \answer -> do
        assertEqual "no sort was asked for" Nothing =<< sortOf answer
        assertEqual "the echo" "^ → toggle-sort (this table-view.js has no sort)"
                    =<< textAt "echo" answer

    -- The renderer keeps its sort keys across a `setRows' — it drops the
    -- derived orders and nothing else — so a reconnect that repaints the rows
    -- lands them in the order the reader put the table in, and this page
    -- re-asserts nothing.  The record survives with it: the next press
    -- continues the cycle rather than starting it over.
  , testCase "a refetch keeps the sort, and nothing re-asserts it" $ do
      bootOf shell "" 500 "f ^" "moved close:resync" $ \answer -> do
        assertEqual "one sort asked for, at the press" 1 =<< intAt "sortCalls" answer
        assertEqual "and it is still the one that was asked for"
                    (Just ("state", False)) =<< sortOf answer
      bootOf shell "" 500 "f ^" "moved close:resync press:^" $ \answer -> do
        assertEqual "the press after it flips the leader it left in force"
                    (Just ("state", True)) =<< sortOf answer
        assertEqual "the echo" "^ → toggle-sort (state ▲)" =<< textAt "echo" answer

    -- A REMOUNT re-reads the chain off the query it mounts under, which now
    -- carries the order: the press after one continues the chain the reader
    -- built rather than starting the declared one over.
  , testCase "a remount re-seeds the chain off the query it mounts under" $
      bootOf shell "" 500 "f ^" "close:view-changed press:f press:^" $ \answer ->
        assertEqual "the leader the query named, flipped back"
                    "^ → toggle-sort (state ▲)" =<< textAt "echo" answer

    -- THE PRESS IS A QUERY EDIT.  The renderer writes the chain into the applied
    -- query and delivers it, so it arrives here as an ordinary commit: the URL
    -- is rewritten and the server is asked for the order it was just told about,
    -- which is what makes page one of a limited answer the right hundred rows.
  , testCase "the press writes the order into the query and asks for it" $
      -- A bare boot opens on the default view, so the press lands beside the
      -- query that was already applied rather than over it.
      bootOf shell "" 500 "f ^" "" $ \answer -> do
        assertEqual "the URL carries the order"
                    "?q=state%3A*active*+sort%3Astate%3Adesc" =<< textAt "url" answer
        assertEqual "and the server was asked for it"
                    (Just "/headlines?q=state%3A*active*%20sort%3Astate%3Adesc")
          . lastOf =<< textsAt "asked" answer

    -- And it composes with a filter rather than replacing it: the sort tokens
    -- are the query's own, so a narrowed view stays narrowed.
  , testCase "the order joins a filter already applied" $
      bootOf shell "?q=state%3ATODO" 500 "f ^" "" $ \answer ->
        assertEqual "the predicate, then the order"
                    "?q=state%3ATODO+sort%3Astate%3Adesc" =<< textAt "url" answer

    -- DEL takes it off like any other token, which is the whole of the way home:
    -- with no sort token the answer comes back in the view's declared order.
  , testCase "DEL takes the order back off" $
      bootOf shell "?q=state%3ATODO" 500 "f ^" "press:Backspace" $ \answer -> do
        assertEqual "the query the strip left" "?q=state%3ATODO"
          =<< textAt "url" answer
        assertEqual "and that is what was asked for"
                    (Just "/headlines?q=state%3ATODO") . lastOf
          =<< textsAt "asked" answer
  ]

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

    -- AN ARCHIVED ROW SPENDS ITS MARK, the way it spends its flag.  A mark
    -- survives a `setRows' and a filter that hides its row — which is what makes
    -- it useful, and what would otherwise leave an archived row marked where no
    -- reader can see it: the count would carry it, `U' would answer about it,
    -- and it would come back marked under `tag:*archive*'.
  , testCase "archiving takes the archived rows' marks with their flags" $ do
      -- `m' marks and STEPS, so the two presses mark r1 and r2 and leave the
      -- cursor past them; `p p' walks back to the row the flag is for.
      bootOf shell "" 500 "m m p p d d" "" $ \answer -> do
        assertEqual "the row that was archived" [("archive", ["r1"])] =<< postedOf answer
        assertEqual "keeps neither its flag" [] =<< textsAt "flagged" answer
        assertEqual "nor its mark, and the other row keeps its own" ["r2"]
          =<< textsAt "marked" answer
      -- `D' with nothing flagged takes the row at point, which the three marks
      -- left on the last one: the marks it did not reach are untouched.
      bootOf shell "" 500 "m m m D" "" $ \answer -> do
        assertEqual "the row at point went" [("archive", ["r3"])] =<< postedOf answer
        assertEqual "and the marks the archive did not reach stand"
                    ["r1", "r2"] =<< textsAt "marked" answer

  , testCase "and an unmarked row costs no mark at all" $
      bootOf shell "" 500 "n m p p d d" "" $ \answer -> do
        assertEqual "the row at point was archived" [("archive", ["r1"])]
          =<< postedOf answer
        assertEqual "the mark on the OTHER row is untouched" ["r2"]
          =<< textsAt "marked" answer

    -- A refused write archived nothing, so it spends nothing either.
  , testCase "a refused archive leaves the mark where it was" $
      bootOf shell "" 500 "" "refuse press:m press:p press:d press:d" $ \answer -> do
        assertEqual "the command went" [("archive", ["r1"])] =<< postedOf answer
        assertEqual "and the mark stands" ["r1"] =<< textsAt "marked" answer

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

-- | Where point ends up after an archive takes its row out of the view.
--
-- The rows leave by one of two doors and both are driven here: an unfiltered
-- client SPLICES the socket's row ops straight in, and a filtered one reads
-- none of them and refetches.  The anchor is worked out at FIRE time, while the
-- view still holds the rows about to go, and lands at whichever door they
-- actually left by.
--
-- The other two landing rules are somebody else's cases and stay there: an
-- applied view lands on row one (@moveSpec@, @drillSpec@) and a pop puts back
-- the row its drill was launched from (@drillSpec@).  The one case here that
-- touches them is the last, which pins that an applied view still lands on row
-- one immediately after an anchor landed somewhere else.
landingSpec :: IO T.Text -> TestTree
landingSpec shell = testGroup "Shell landing"
  [ -- dired's: the row point was standing on goes, and point goes to the one
    -- after it.  Under a filter that means the refetch the frame scheduled,
    -- which is where the rows leave for a filtered reader — and the frame the
    -- server sent was an UPSERT, the row still being the store's.
    testCase "an archived row mid-table lands point on the next surviving row" $
      bootOf shell "" 500 "n d d" "unserved:r2 frame:upsert=r2 wait:300" $ \answer -> do
        assertEqual "the row at point was archived"
                    [("archive", ["r2"])] =<< postedOf answer
        assertEqual "and point moved down one, not back to the top"
                    (Just "r3") =<< maybeTextAt "selected" answer
        assertEqual "which is where the row it was on had been" 1
                    =<< intAt "cursor" answer
        assertEqual "the rows came back over the wire, not off the frame"
                    [] =<< textsAt "spliced" answer

    -- Nothing below point to scan to, so the anchor walks back UP to the
    -- nearest surviving row — the new last row, which is the landing a reader
    -- deleting from the bottom of a buffer expects.  That branch always agrees
    -- with the renderer's own keeping (point is past every survivor, so the
    -- place it stood clamps to the same row), so what this pins is the outcome
    -- rather than which of the two produced it.
  , testCase "archiving the last row lands point on the new last" $
      bootOf shell "" 500 "n n d d" "unserved:r3 frame:upsert=r3 wait:300" $ \answer -> do
        assertEqual "the last row went" [("archive", ["r3"])] =<< postedOf answer
        assertEqual "and point is on the one above it"
                    (Just "r2") =<< maybeTextAt "selected" answer

    -- THE CASE THE RENDERER'S OWN KEEPING GETS WRONG, and the reason the anchor
    -- is taken at fire time at all.  Six rows, `r1' and `r4' flagged, point on
    -- `r4': the next surviving row is `r5', but rows went from ABOVE point too,
    -- so the visual PLACE point stood in — index 3 — is `r6' once they have
    -- gone.  A landing that only knew where point had been would skip a row.
  , testCase "the anchor is the next surviving row, not the place point stood" $
      bootOf shell "" 500 ""
             ("rows:6 press:d press:n press:n press:n press:d press:D"
              <> " unserved:r1,r4 frame:upsert=r1 frame:upsert=r4 wait:300") $ \answer -> do
        assertEqual "both flagged rows, in one request"
                    [("archive", ["r1", "r4"])] =<< postedOf answer
        assertEqual "and the flags are spent" [] =<< textsAt "flagged" answer
        assertEqual "the row under the one that went, not the one two below it"
                    (Just "r5") =<< maybeTextAt "selected" answer

    -- And with point on a row that SURVIVES the set, nothing is owed: no
    -- anchor is armed at all, so it stays exactly where it stood — which is what
    -- "where point was" means when point did not have to move.
  , testCase "a set archived from a surviving row leaves point on that row" $
      bootOf shell "" 500 ""
             ("rows:5 press:n press:d press:n press:n press:d press:p press:D"
              <> " unserved:r2,r4 frame:upsert=r2 frame:upsert=r4 wait:300") $ \answer ->
        assertEqual "the row point was on is still under it"
                    (Just "r3") =<< maybeTextAt "selected" answer

    -- And no anchor is left ARMED behind it either.  The anchor belongs to the
    -- archive that took point's row away, so an archive that took some other
    -- row must leave nothing lying in wait: when point's row later goes for
    -- some unrelated reason, the renderer's own keeping is the whole rule.
  , testCase "and arms nothing for a later removal to land on" $
      bootOf shell "?q=" 500 ""
             ("rows:6 press:d press:n press:n press:n press:d press:p press:D"
              <> " frame:delete=r1,r4 frame:delete=r3") $ \answer ->
        assertEqual "the row that took r3's place, not the archive's own anchor"
                    (Just "r6") =<< maybeTextAt "selected" answer

    -- A page where every row is leaving has nowhere to land, so the anchor is
    -- nothing and the empty view selects nothing — which is what an applied
    -- view with no rows in it already did, and what the renderer does when the
    -- last row goes out from under the cursor.
  , testCase "archiving every row leaves nothing selected" $
      bootOf shell "" 500 "d n d n d"
             ("press:d unserved:r1,r2,r3"
              <> " frame:upsert=r1 frame:upsert=r2 frame:upsert=r3 wait:300") $ \answer -> do
        assertEqual "all three went" [("archive", ["r1", "r2", "r3"])]
          =<< postedOf answer
        assertEqual "and there is no row to be on" Nothing
          =<< maybeTextAt "selected" answer

    -- THE CARVE.  A refetch the watch caused is the view the reader already
    -- had, arriving again because a file moved: it lands nothing of its own, so
    -- somebody else's edit no longer yanks a reader back to row one.  Only the
    -- archive that took the rows away may override where the renderer kept the
    -- cursor, and it says so by arming the anchor.
  , testCase "a watch refetch under a filter leaves point where it was" $
      bootOf shell "" 500 "n n" "frame:upsert=r1 wait:300" $ \answer -> do
        assertEqual "the frame was re-asked for" 3 . length =<< listAt "paints" answer
        assertEqual "and point did not move for it"
                    (Just "r3") =<< maybeTextAt "selected" answer

    -- A refused write moved no row, so the landing it armed goes with the marks
    -- it did not spend: the row point was on is still there.  When it later
    -- goes for some other reason the renderer's own keeping is the whole rule,
    -- which lands on the row that took its PLACE rather than on the one the
    -- archive would have picked.
  , testCase "a refused archive arms no landing" $
      bootOf shell "?q=" 500 ""
             "refuse press:d press:n press:d press:p press:D frame:delete=r1" $ \answer ->
        assertEqual "the row that took r1's place, not the anchor's r3"
                    (Just "r2") =<< maybeTextAt "selected" answer

    -- THE ANCHOR ITSELF VANISHING between the fire and the landing, which is
    -- what the remembered PLACE is for: `r3' is archived from under point and
    -- `r4', the row it was to land on, goes to somebody else's edit first.
    -- `select' answers false for a row the view no longer holds, so the landing
    -- falls through to where the anchor WOULD have been sitting once the
    -- archived rows had gone — index 1 of what is left — rather than to row one.
  , testCase "an anchor the view lost falls back to the place it would have had" $
      bootOf shell "" 500 ""
             ("rows:4 press:n press:d press:d unserved:r2,r3"
              <> " frame:upsert=r2 wait:300") $ \answer -> do
        assertEqual "the row point was on" [("archive", ["r2"])] =<< postedOf answer
        assertEqual "the place, since the row it named is gone too"
                    (Just "r4") =<< maybeTextAt "selected" answer

    -- An archive is an UPSERT on the wire — `Store.streamed` emits a delete
    -- only for an id that left the store, and archiving adds a tag to a row
    -- that stays — so an unfiltered client keeps the row it just archived:
    -- `/headlines` would not have served it, and the socket is not filtered.
    -- Nothing left the view, so point does not move.
  , testCase "an archived row an unfiltered client keeps does not move point" $
      bootOf shell "?q=" 500 "n d d" "frame:upsert=r2" $ \answer -> do
        assertEqual "the row was spliced back in" ["upsert r2"]
          =<< textsAt "spliced" answer
        assertEqual "and point is still on it" (Just "r2")
          =<< maybeTextAt "selected" answer

    -- Which is what the splice door is FOR: it cannot land an archive's anchor,
    -- because the archive's own frames leave every row where it was — what it
    -- does is SPEND it, so the anchor describes one watch step and no more.
    -- Here the rows go later, for somebody else's reason, and the landing is
    -- the renderer's own keeping (the visual place, `r6`) rather than the
    -- archive's anchor (`r5`), which was spent when its own frames arrived.
  , testCase "and its frames spend the anchor rather than landing it" $
      bootOf shell "?q=" 500 ""
             ("rows:6 press:d press:n press:n press:n press:d press:D"
              <> " frame:upsert=r1 frame:upsert=r4 frame:delete=r1,r4") $ \answer -> do
        assertEqual "the frames the archive itself caused, then the removals"
                    [ "upsert r1", "upsert r4", "delete r1", "delete r4" ]
                    =<< textsAt "spliced" answer
        assertEqual "the renderer's place, the anchor having been spent"
                    (Just "r6") =<< maybeTextAt "selected" answer

    -- The carve reaches the WATCH's refetch and nothing else: an applied view
    -- is a new question and still lands on row one, immediately after an anchor
    -- landed somewhere else.
  , testCase "an applied view still lands on row one after an anchor did not" $
      bootOf shell "" 500 "n d d"
             "unserved:r2 frame:upsert=r2 wait:300 press:g" $ \answer ->
        assertEqual "g took the top of its answer" (Just "r1")
          =<< maybeTextAt "selected" answer

    -- An anchor belongs to the VIEW it was taken in, and a mount thrown away
    -- takes it with it.  Reachable because an archive under NO filter leaves
    -- its row on screen — the socket carries an upsert whatever the query — so
    -- the anchor is still armed when `g' rebuilds the table.  Left standing, it
    -- would fire on the next frame and pull the cursor off the row the new view
    -- had just landed it on.
  , testCase "a remount drops an anchor the archive never spent" $
      bootOf shell "?q=" 500 "n d d" "press:g frame:delete=r2 wait:300" $ \answer ->
        assertEqual "where g landed it, not where the old view's anchor pointed"
                    (Just "r1") =<< maybeTextAt "selected" answer

    -- `visible()` is ONE PAGE, so "the row point was on has left the view" is
    -- only answerable about the page the anchor was taken on.  A reader who
    -- turned a page between the write and its watch event would otherwise be
    -- told every row of that page had gone, and be landed on the new page's
    -- row `at`.
  , testCase "an anchor is not landed on a page it was not taken on" $
      bootOf shell "" 500 ""
             ("rows:6 paged:3 press:n press:n press:d press:d press:] press:n"
              <> " unserved:r3 frame:upsert=r3 wait:300") $ \answer -> do
        assertEqual "the row point was on" [("archive", ["r3"])] =<< postedOf answer
        assertEqual "still on the page it walked to" 2 =<< intAt "page" answer
        assertEqual "and on the row it walked to, not the other page's anchor"
                    (Just "r5") =<< maybeTextAt "selected" answer

    -- The third road the same rows can arrive without them: a socket that was
    -- down while the write landed, and a reconnect whose answer is the first
    -- this page has seen since.  `resync` repaints the same view, so it settles
    -- the anchor exactly as the watch's own refetch would have.
  , testCase "a reconnect's repaint lands the anchor too" $
      bootOf shell "" 500 ""
             ("rows:6 press:d press:n press:n press:n press:d press:D"
              <> " unserved:r1,r4 close:resync") $ \answer ->
        assertEqual "the next surviving row, not the renderer's place"
                    (Just "r5") =<< maybeTextAt "selected" answer

    -- And the other door that replaces a view without rebuilding the mount: a
    -- COMMIT.  `^` writes its chain into the query, which is a commit like any
    -- other, so the anchor taken under the query being left goes with it.
  , testCase "and so does a commit, which replaces the view without a remount" $
      bootOf shell "?q=" 500 "n d d"
             "press:f press:^ frame:delete=r2 wait:300" $ \answer ->
        assertEqual "where the commit landed it, not the old view's anchor"
                    (Just "r1") =<< maybeTextAt "selected" answer
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

    -- `*empty*' answers to DEL, which already MEANS take-it-off wherever this
    -- page binds one, and claims no letter — so the a-z pool is the keywords'.
  , testCase "the meta entry clears the keyword rather than setting one" $
      bootOf shell "" 500 "C-c C-t" "press:Backspace" $ \answer -> do
        assertEqual "a null keyword" [Nothing] =<< keywordsOf answer
        assertEqual "and the pill says so" "C-c C-t → org-glance-overview:todo (*empty* · 1)"
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
                            [Just "DONE"] <=< keywordsOf))
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

-- | @:@ — the manage-tags popup, which is the page's FOURTH table-view mount
-- and the only MUTABLE one.
--
-- What is pinned here is the page's half: the mount it raises and the shape of
-- it, the coverage column, the removal gesture, the add flow and the rename.
-- The span math is @TestQuery@'s and the routes are @tagCommandSpec@'s.
tagKeySpec :: IO T.Text -> TestTree
tagKeySpec shell = testGroup "Shell tags"
  [ testCase ": raises a mount over the row's own tags" $
      bootOf shell "" 500 ":" "" $ \answer -> do
        assertEqual "raised" "on" =<< textAt "tagpop" answer
        assertEqual "and no value palette with it" "" =<< textAt "prompt" answer
        assertEqual "titled by what it is over and how many"
                    "tags · 1 row" =<< textAt "thead" answer
        assertEqual "one request, naming the row" ["/tags?ids=r1"]
          =<< textsAt "tagged" answer
        assertEqual "one mount, built on the first raise" 1 =<< intAt "tmounts" answer
        assertEqual "the columns the shell declared" ["title", "on", "rows"]
          =<< traverse (textAt "key") =<< listAt "tcols" answer

    -- A ROW IS A RECORD: the tag, how much of the set carries it, and what the
    -- whole tree has under it.  The third is the server's count and the one
    -- number no arithmetic over the rows in hand recovers.
  , testCase "a row is the tag, its coverage and the tree's count" $
      bootOf shell "" 500 ":" "" $ \answer -> do
        assertEqual "one row per tag" [["web", "all", "40"]] =<< pairsAt "ttags" answer
        assertEqual "the cursor lands on the first" 0 =<< intAt "tat" answer
        assertEqual "and the foot names every key that works"
                    "RET renames · d flags · D removes · + adds · ESC leaves"
          =<< textAt "tfoot" answer

    -- MUTABLE, and stated in the mount: flags for the removal gesture, no marks
    -- (the set this runs over is the TABLE's and was settled before it went up),
    -- no per-row hint and no page.
  , testCase "the mount is mutable: flags on, marks off, no hints, no page" $
      bootOf shell "" 500 ":" "" $ \answer -> do
        assertEqual "marks off" False =<< boolAt "tmarks" answer
        assertEqual "flags on" True =<< boolAt "tflags" answer
        assertEqual "hints off" False =<< boolAt "thints" answer
        assertEqual "no page size, so the whole list is on show" 0
          =<< intAt "tpage" answer
        assertEqual "and the flag's own hint names the two keys that answer it"
                    "d/D remove · u unflag" =<< textAt "tflagHelp" answer

    -- The same rows every other keyed write runs over: the marked set where
    -- there is one, the row at point otherwise.
  , testCase "over a marked set it names the whole set, in one request" $
      bootOf shell "" 500 "m m :" "" $ \answer -> do
        assertEqual "the title counts them" "tags · 2 rows" =<< textAt "thead" answer
        assertEqual "and the resolution is one request"
                    ["/tags?ids=r1&ids=r2"] =<< textsAt "tagged" answer

    -- COVERAGE, which is what the letter palette wrote into a muted aside and
    -- this one gives a column: `all' where the set is level, `k/n' where it is
    -- not.  `partly' leaves the third row without `web'.
  , testCase "a tag part of the set carries says so in its own cell" $
      bootOf shell "" 500 "" "partly press:m press:m press:m press::" $
        assertEqual "two of the three rows" [["web", "2/3", "40"]] <=< pairsAt "ttags"

    -- The popup browses on the same keys the property panel and the link popup
    -- do, which is `rowStep' in one place.
  , testCase "n and p walk it, in both spellings" $ do
      let two = "press:m press:m press:: press:+ type:work press:Enter"
      bootOf shell "" 500 "" two $ \answer -> do
        assertEqual "two tags to walk"
                    [["web", "all", "40"], ["work", "all", "9"]] =<< pairsAt "ttags" answer
        assertEqual "the cursor lands on the one just written" 1 =<< intAt "tat" answer
      bootOf shell "" 500 "" (two <> " press:p") $
        assertEqual "up one" 0 <=< intAt "tat"
      bootOf shell "" 500 "" (two <> " press:k press:j") $
        assertEqual "and back" 1 <=< intAt "tat"

    -- THE DELETION GESTURE, dired's and the page's: `d' flags, `d' again on the
    -- flagged row IS `D', and the removal goes to every target CARRYING the tag.
  , testCase "d flags the tag at point and writes nothing" $
      bootOf shell "" 500 ":" "press:d" $ \answer -> do
        assertEqual "flagged" ["web"] =<< textsAt "tflagged" answer
        assertEqual "nothing written" [] =<< postedOf answer
        assertEqual "and the echo says what a second press does"
                    "d → tag-flag (d again removes)" =<< textAt "echo" answer

  , testCase "a second d removes it from every row carrying it" $
      bootOf shell "" 500 "m m m :" "press:d press:d" $ \answer -> do
        assertEqual "over all three" [("remove-tag", ["r1", "r2", "r3"])]
          =<< postedOf answer
        -- Mounted once and kept, like the panel and the link popup: a write is
        -- a `setRows' over the same instance, never a second mount.
        assertEqual "still one mount" 1 =<< intAt "tmounts" answer
        assertEqual "and a repaint for the raise and for what landed" 2
          =<< intAt "tsets" answer
        assertEqual "as the tag the row named" ["web"] =<< tagsPosted answer
        assertEqual "the pill names what landed"
                    ": → org-agenda-set-tags (untagged :web: · 3)" =<< textAt "echo" answer
        assertEqual "the flag was spent" [] =<< textsAt "tflagged" answer
        assertEqual "and the entry went with it" [] =<< pairsAt "ttags" answer
        assertEqual "leaving the foot naming the one key that still does anything"
                    "nothing tagged here · + adds one · ESC leaves"
          =<< textAt "tfoot" answer

  , testCase "D is the same handler without the flagging press" $
      bootOf shell "" 500 "m m :" "press:D" $ \answer -> do
        assertEqual "both rows" [("remove-tag", ["r1", "r2"])] =<< postedOf answer
        assertEqual "and the popup stands" "on" =<< textAt "tagpop" answer

    -- Several flags are several commands, since a command names ONE tag — each
    -- its own per-file batch of atomic writes — and every one of them is aimed
    -- at the rows carrying THAT tag.  `partly' leaves the third row without
    -- `web', so the two removals name different sets.
  , testCase "D over several flagged tags is one command each, over its own rows" $
      bootOf shell "" 500 ""
             ("partly press:m press:m press:m press:: press:+ type:work press:Enter"
              <> " press:d press:p press:d press:D") $ \answer -> do
        assertEqual "the add, then a removal per flagged tag"
                    [ ("add-tag", ["r1", "r2", "r3"])
                    , ("remove-tag", ["r1", "r2", "r3"])
                    , ("remove-tag", ["r1", "r2"]) ] =<< postedOf answer
        assertEqual "each carrying its own tag" ["work", "work", "web"]
          =<< tagsPosted answer
        assertEqual "and every tag went" [] =<< pairsAt "ttags" answer

  , testCase "u takes a flag off before anything is written" $
      bootOf shell "" 500 ":" "press:d press:u" $ \answer -> do
        assertEqual "no flag left" [] =<< textsAt "tflagged" answer
        assertEqual "nothing written" [] =<< postedOf answer
        assertEqual "and the echo says which" "u → tag-unflag (flag cleared)"
          =<< textAt "echo" answer

    -- A HELD `d' must not flag a tag and remove it from ONE press, which is the
    -- confirmation the two-press shape exists to be.
  , testCase "a held d flags once and never removes" $
      bootOf shell "" 500 ":" "press:d repeat:d repeat:d" $ \answer -> do
        assertEqual "nothing written" [] =<< postedOf answer
        assertEqual "and the flag is still just a flag" ["web"]
          =<< textsAt "tflagged" answer

    -- `+' — the add flow, unchanged: one field over the ADDABLE vocabulary,
    -- which is the tree's tags less the ones every target already carries.
  , testCase "+ raises the field over what can be added" $
      bootOf shell "" 500 ":" "press:+" $ \answer -> do
        assertEqual "the palette is up over the popup" "on" =<< textAt "prompt" answer
        assertEqual "in its typing mode" "narrow" =<< textAt "pmode" answer
        assertEqual "titled by the rows it would write" "add a tag · 1 row"
          =<< textAt "phead" answer
        assertEqual "the tree's tags, less the one every row already has"
          [ ("pe pat", "", ["archive"], [])
          , ("pe",     "", ["book"],    [])
          , ("pe",     "", ["work"],    []) ] =<< paletteOf answer
        assertEqual "and the foot names what RET does there"
                    "RET adds it · C-n/C-p walks · ESC leaves" =<< textAt "pfoot" answer

    -- A tag only SOME of the targets carry stays offered, wearing the coverage
    -- that says who lacks it: adding it levels the set up, which is a write.
  , testCase "a tag some of the set carries is still addable, and says so" $
      bootOf shell "" 500 "" "partly press:m press:m press:m press:: press:+" $
        assertEqual "offered first, wearing its coverage"
          [ ("web", "2/3"), ("archive", ""), ("book", ""), ("work", "") ]
          <=< paletteHints

  , testCase "RET there adds the tag to every row lacking it and stays open" $
      bootOf shell "" 500 "m m :" "press:+ type:work press:Enter" $ \answer -> do
        assertEqual "both rows, since neither carries it"
                    [("add-tag", ["r1", "r2"])] =<< postedOf answer
        assertEqual "as the tag typing settled on" ["work"] =<< tagsPosted answer
        assertEqual "the field is gone" "" =<< textAt "prompt" answer
        assertEqual "the popup stands" "on" =<< textAt "tagpop" answer
        assertEqual "with the new tag beside the old one"
                    [["web", "all", "40"], ["work", "all", "9"]]
          =<< pairsAt "ttags" answer
        assertEqual "the store was not asked again" ["/tags?ids=r1&ids=r2"]
          =<< textsAt "tagged" answer

    -- The field's own RET must not reach the popup underneath it.  The palette
    -- closes as it commits and its listener runs AHEAD of the popup's, so
    -- without the claimed-key guard the same press would land on a popup with
    -- no prompt on it and open the rename over the tag it had just written.
  , testCase "the RET that adds does not open the rename behind it" $
      bootOf shell "" 500 ":" "press:+ type:work press:Enter" $ \answer -> do
        assertEqual "the tag was added" [("add-tag", ["r1"])] =<< postedOf answer
        assertEqual "and no rename opened" False =<< boolAt "trename" answer

  , testCase "and a tag the tree has never held is committable all the same" $
      bootOf shell "" 500 ":" "press:+ type:brandnew press:Enter" $ \answer -> do
        assertEqual "the typed line, folded" ["brandnew"] =<< tagsPosted answer
        assertEqual "over the row at point" [("add-tag", ["r1"])] =<< postedOf answer
        assertEqual "and it joins the list under a count of its own"
                    [["web", "all", "40"], ["brandnew", "all", "1"]]
          =<< pairsAt "ttags" answer

  , testCase "typing a tag every row has writes nothing and says so" $
      bootOf shell "" 500 "m m m :" "press:+ type:web press:Enter" $ \answer -> do
        assertEqual "no command went" [] =<< postedOf answer
        assertEqual "and the pill says why"
                    ": → org-agenda-set-tags (:web: is on every row already)"
          =<< textAt "echo" answer

    -- RET IS THE RENAME, through the property panel's edit model: the tag cell
    -- becomes a field over itself, opened on the text it holds.
  , testCase "RET opens the tag at point over itself" $
      bootOf shell "" 500 ":" "press:Enter" $ \answer -> do
        assertEqual "the overlay is up" True =<< boolAt "trename" answer
        assertEqual "holding the tag it opened on" "web" =<< textAt "tname" answer
        assertEqual "and nothing is written by opening it" [] =<< postedOf answer

  , testCase "and RET again commits it as one rename-tag" $
      bootOf shell "" 500 "m m :" "press:Enter tname:code press:Enter" $ \answer -> do
        assertEqual "one command, over the rows carrying the old name"
                    [("rename-tag", ["r1", "r2"])] =<< postedOf answer
        assertEqual "carrying both ends" [("web", "code")] =<< renamesPosted answer
        assertEqual "the overlay is gone" False =<< boolAt "trename" answer
        assertEqual "the popup stands" "on" =<< textAt "tagpop" answer
        assertEqual "the row is renamed in place, keeping its coverage"
                    [["code", "all", "2"]] =<< pairsAt "ttags" answer
        assertEqual "the pill names what landed"
                    ": → org-agenda-set-tags (renamed :web:→:code: · 2)"
          =<< textAt "echo" answer

  , testCase "the log names every row a rename landed on" $
      bootOf shell "" 500 "m m :" "press:Enter tname:code press:Enter" $ \answer -> do
        lines' <- map (message . cut) <$> logOf answer
        assertEqual "one line per row"
                    ["headline \"one\" retagged web→code", "headline \"two\" retagged web→code"]
                    (drop (length lines' - 2) lines')

    -- The typed name is folded, because presence is, and a name that does not
    -- move costs no round trip.
  , testCase "a rename to the same name writes nothing" $
      mapM_ (\typed -> bootOf shell "" 500 ":" ("press:Enter tname:" <> typed
                                                <> " press:Enter") $ \answer -> do
               assertEqual (T.unpack typed <> ": no command went") []
                 =<< postedOf answer
               assertEqual "and the pill says so"
                           ": → org-agenda-set-tags (unchanged)" =<< textAt "echo" answer)
            ["web", "WEB", ""]

    -- ESC over the overlay is the ROW's, and only from the popup does the key
    -- reach the popup — the ladder the property panel's open row already walks.
  , testCase "ESC leaves the rename a rung at a time" $ do
      bootOf shell "" 500 ":" "press:Enter tname:code press:Escape" $ \answer -> do
        assertEqual "nothing was written" [] =<< postedOf answer
        assertEqual "the overlay is gone" False =<< boolAt "trename" answer
        assertEqual "the popup stands" "on" =<< textAt "tagpop" answer
        assertEqual "and the tag is the tag it was" [["web", "all", "40"]]
          =<< pairsAt "ttags" answer
      bootOf shell "" 500 ":" "press:Enter press:Escape press:Escape" $
        assertEqual "a second ESC closes it" "" <=< textAt "tagpop"

    -- THE TAG A COMMIT RENAMES IS THE TAG THE OVERLAY OPENED OVER.  No key can
    -- move the cursor while the field is up, but a MOUSE CLICK can, and a commit
    -- that re-read the cursor would rename whichever tag the reader landed on
    -- with the name typed for another.  The overlay snapshots at open, and the
    -- property panel's row edit now answers the same way off the same mechanism.
  , testCase "a click under an open rename still renames the tag it opened on" $
      bootOf shell "" 500 ""
             ("press:m press:m press:: press:+ type:work press:Enter"
                <> " press:Enter tname:renamed click:0 press:Enter") $ \answer -> do
        assertEqual "the add, then one rename over the rows carrying it"
                    [("add-tag", ["r1", "r2"]), ("rename-tag", ["r1", "r2"])]
          =<< postedOf answer
        assertEqual "and it names the tag the overlay opened on, not the clicked one"
                    ": → org-agenda-set-tags (renamed :work:→:renamed: · 2)"
          =<< textAt "echo" answer
        assertEqual "so the clicked tag stands and the opened one moved"
                    [["web", "all", "40"], ["renamed", "all", "2"]]
          =<< pairsAt "ttags" answer

  , testCase "ESC from the + field leaves the popup standing" $
      bootOf shell "" 500 ":" "press:+ type:work press:Escape" $ \answer -> do
        assertEqual "nothing was written" [] =<< postedOf answer
        assertEqual "the field is gone" "" =<< textAt "prompt" answer
        assertEqual "and the popup is still up" "on" =<< textAt "tagpop" answer

  , testCase "ESC from the popup closes it, having written nothing" $
      bootOf shell "" 500 ":" "press:Escape" $ \answer -> do
        assertEqual "nothing was written" [] =<< postedOf answer
        assertEqual "the popup is down" "" =<< textAt "tagpop" answer

    -- THE LETTERS ARE GONE.  The state palette keeps them; a tag list is read
    -- rather than committed from memory, so a bare letter here is nobody's.
  , testCase "a letter commits nothing, the which-key list having gone" $
      bootOf shell "" 500 ":" "press:w press:a press:b" $ \answer -> do
        assertEqual "no command went" [] =<< postedOf answer
        assertEqual "no value palette either" "" =<< textAt "prompt" answer
        assertEqual "and the popup is still up" "on" =<< textAt "tagpop" answer

    -- While it is up every `table' row is dead, so the keys the popup does not
    -- claim reach nothing at all.
  , testCase "the table's own keys are inert while the popup is up" $
      bootOf shell "" 500 ":" "press:m press:M press:U press:t" $ \answer -> do
        assertEqual "nothing was marked" [] =<< textsAt "marked" answer
        assertEqual "nothing was flagged in the table" [] =<< textsAt "flagged" answer
        assertEqual "no command was posted" [] =<< namesOf answer
        assertEqual "and no state palette went up" "" =<< textAt "prompt" answer

    -- And the popup's own keys are dead while its `+' field is up, which is
    -- what the listener's `prompting' guard buys: `d' narrows the field rather
    -- than flagging the tag underneath it.
  , testCase "and the popup's own keys are dead under its field" $
      bootOf shell "" 500 ":" "press:+ press:d" $ \answer -> do
        assertEqual "nothing was flagged" [] =<< textsAt "tflagged" answer
        assertEqual "and the field is still up" "narrow" =<< textAt "pmode" answer

  , testCase "an untagged set opens on a popup that says so" $
      bootOf shell "" 500 "" "untagged press::" $ \answer -> do
        assertEqual "the popup is up" "on" =<< textAt "tagpop" answer
        assertEqual "with nothing in it" [] =<< pairsAt "ttags" answer
        assertEqual "and the foot naming the way in"
                    "nothing tagged here · + adds one · ESC leaves"
          =<< textAt "tfoot" answer

  , testCase "a refused resolution raises nothing and says so" $
      bootOf shell "" 500 "" "refuse press::" $ \answer -> do
        assertEqual "no popup" "" =<< textAt "tagpop" answer
        assertEqual "and the log named it" (Just "tags failed: GET /tags?ids=<row id>")
          =<< lastLog answer

  , testCase "and a set the store knows no row of raises none either" $
      bootOf shell "" 500 "" "unknownrows press::" $ \answer -> do
        assertEqual "no popup" "" =<< textAt "tagpop" answer
        assertEqual "the pill says which" ": → org-agenda-set-tags (no such row)"
          =<< textAt "echo" answer

    -- THE LIST REFRESHES FROM THE ANSWER, never from a re-read: `/command' does
    -- not write the store — the watch does, a debounce later — so asking
    -- `/tags' again would report what the files said BEFORE the write.  The
    -- fake store still says every row carries `web' when this reads the list.
  , testCase "the list is what landed, and the store is not asked twice" $
      bootOf shell "" 500 "m m :" "press:d press:d" $ \answer -> do
        assertEqual "the one resolution, and no second" ["/tags?ids=r1&ids=r2"]
          =<< textsAt "tagged" answer
        assertEqual "and the tag is gone from a list nobody re-read" []
          =<< pairsAt "ttags" answer
  ]

-- | The pair each posted @rename-tag@ carried.
renamesPosted :: Value -> IO [(T.Text, T.Text)]
renamesPosted = traverse one <=< argsOf
  where one v = (,) <$> textAt "from" v <*> textAt "to" v

-- | The tag each posted command carried.
tagsPosted :: Value -> IO [T.Text]
tagsPosted = traverse (textAt "tag") <=< argsOf

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
-- asking, several raise the POPUP — so every case here runs the fetch and
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
                    "! → org-glance-overview:open · follow this row\'s link; several list them"
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

    -- Several is the POPUP, and the popup is the page's THIRD table-view mount.
    -- A list of links is a list of RECORDS — a kind, a name, a destination —
    -- and reading it is how a reader picks one, which is the browse gesture a
    -- which-key letter is the wrong shape for.
  , testCase "several raise the popup, which is a table-view mount" $
      bootOf shell "" 500 "o" "" $ \answer -> do
        assertEqual "raised" "on" =<< textAt "popup" answer
        assertEqual "no value palette went up" "" =<< textAt "prompt" answer
        assertEqual "titled by the count" "open · 3 links" =<< textAt "lhead" answer
        assertEqual "one mount, built on the first raise" 1
          =<< intAt "lmounts" answer
        assertEqual "the columns the shell declared" ["type", "title", "url"]
          =<< traverse (textAt "key") =<< listAt "lcols" answer

    -- The rows carry the server's own three answers, in the order the subtree
    -- writes them: the type it derived, the description the entry itself wrote,
    -- and where it points.
  , testCase "the rows are the answer, type and all" $
      bootOf shell "" 500 "o" "" $ \answer -> do
        assertEqual "one row per link"
          [ ["https", "First reference", "https://one.example/a"]
          , ["https", "Second reference", "https://two.example/b"]
          , ["mailto", "mailto:t@example.org", "mailto:t@example.org"] ]
          =<< pairsAt "llinks" answer
        assertEqual "the cursor lands on the first" 0 =<< intAt "lat" answer
        assertEqual "and the foot names the three keys that work"
                    "RET edits · o opens it · ESC leaves" =<< textAt "lfoot" answer

    -- READ-ONLY, and stated in the mount rather than inherited: nothing here
    -- writes, so a mark column, a flag wash and a per-row hint would each be
    -- chrome about a gesture the popup does not have.
  , testCase "the mount is read-only: no marks, no flags, no hints, no page" $
      bootOf shell "" 500 "o" "" $ \answer -> do
        assertEqual "marks off" False =<< boolAt "lmarks" answer
        assertEqual "flags off" False =<< boolAt "lflags" answer
        assertEqual "hints off" False =<< boolAt "lhints" answer
        assertEqual "and no page size, so the whole list is on show" 0
          =<< intAt "lpage" answer

    -- The whole point of `typing()' counting the popup: every `table' row is
    -- dead under it, so the keys that WRITE do nothing at all while a reader is
    -- browsing links.  Asserted over the four that would otherwise cost a file.
  , testCase "the write keys are inert while the popup is up" $
      bootOf shell "" 500 "o" "press:d press:D press:m press:M press:u press:U" $
        \answer -> do
          assertEqual "nothing was flagged, here or in the table" []
            =<< textsAt "lflagged" answer
          assertEqual "nor in the table under it" [] =<< textsAt "flagged" answer
          assertEqual "nothing was marked" [] =<< textsAt "lmarked" answer
          assertEqual "nor there" [] =<< textsAt "marked" answer
          assertEqual "and no command was posted" [] =<< namesOf answer
          assertEqual "the popup is still up" "on" =<< textAt "popup" answer

  , testCase "n and p walk the popup, in both spellings" $ do
      bootOf shell "" 500 "o" "press:n press:n" $
        assertEqual "down twice" 2 <=< intAt "lat"
      bootOf shell "" 500 "o" "press:j press:k" $
        assertEqual "and back" 0 <=< intAt "lat"
      bootOf shell "" 500 "o" "press:ArrowDown" $
        assertEqual "the arrows too" 1 <=< intAt "lat"

    -- `o' is the key that raised this, carried inside: it opens the link the
    -- cursor is on rather than the row's first, and closes.
  , testCase "o opens the link at point and closes the popup" $
      bootOf shell "" 500 "o" "press:n press:o" $ \answer -> do
        assertEqual "the second one" [("https://two.example/b", "_blank", "noopener")]
          =<< openedOf answer
        assertEqual "the popup is down" "" =<< textAt "popup" answer
        assertEqual "the pill names it by its description"
                    "o → org-glance-overview:open (Second reference)"
          =<< textAt "echo" answer

  , testCase "ESC leaves it having opened nothing" $
      bootOf shell "" 500 "o" "press:Escape" $ \answer -> do
        assertEqual "nothing opened" [] =<< openedOf answer
        assertEqual "the popup is down" "" =<< textAt "popup" answer

    -- `RET' EDITS the link at point in place: the row's own title and url cells
    -- become fields over themselves, which is the property panel's edit model
    -- and the third surface to declare a shape for it.  The type cell is the
    -- server's word for the target and never opens.
  , testCase "RET opens the link at point over its own two cells" $
      bootOf shell "" 500 "o" "press:Enter" $ \answer -> do
        assertEqual "the overlay is up" True =<< boolAt "lopen" answer
        assertEqual "holding what the entry calls it" "First reference"
          =<< textAt "ltitle" answer
        assertEqual "and where it points" "https://one.example/a"
          =<< textAt "lurl" answer
        assertEqual "the target takes the focus" "lurl" =<< textAt "focus" answer
        assertEqual "the popup stands under it" "on" =<< textAt "popup" answer
        assertEqual "and nothing is posted by opening one" [] =<< namesOf answer

  , testCase "TAB hops the two fields, and nothing else moves" $ do
      bootOf shell "" 500 "o" "press:Enter press:Tab" $ \answer -> do
        assertEqual "over to the description" "ltitle" =<< textAt "focus" answer
        assertEqual "the overlay is still open" True =<< boolAt "lopen" answer
      bootOf shell "" 500 "o" "press:Enter press:Tab press:Tab" $
        assertEqual "and back" "lurl" <=< textAt "focus"

    -- THE COMMIT is `edit-link' over the SPAN the server handed out, pinned to
    -- the digest that same answer carried: this page holds no bracket grammar
    -- and no offsets of its own, so what it sends back is the range it was
    -- given and the two strings a reader typed.
  , testCase "RET commits the span the server gave, under the digest it came with" $
      bootOf shell "" 500 "o" "press:Enter lurl:https://new.example press:Enter" $
        \answer -> do
          assertEqual "one command, naming the row the popup was raised over"
            [("edit-link", ["r1"])] =<< postedOf answer
          [cmd] <- listAt "commands" answer
          args <- field "args" cmd
          assertEqual "the span it was handed" [10, 48] =<< spanOf args
          assertEqual "the target as typed" "https://new.example"
            =<< textAt "target" args
          assertEqual "and the digest that answer carried" "d0"
            =<< textAt "r1" =<< field "digests" cmd

    -- ABSENT IS NOT NULL, and the field the reader left alone is what says so:
    -- the description field opens on what the link SHOWS, which for a link with
    -- none of its own is its target, so sending that back would spell the
    -- target into a description.
  , testCase "a description nobody moved is not sent at all" $
      bootOf shell "" 500 "o" "press:Enter lurl:https://new.example press:Enter" $
        \answer -> do
          [cmd] <- listAt "commands" answer
          args <- field "args" cmd
          assertEqual "no desc field" ["span", "target"] . sort =<< fieldsOf args

  , testCase "and one the reader emptied is the null that takes it off" $
      bootOf shell "" 500 "o" "press:Enter ltitle: press:Enter" $ \answer -> do
        [cmd] <- listAt "commands" answer
        args <- field "args" cmd
        assertEqual "a null description" Null =<< field "desc" args
        assertEqual "under the target it already had" "https://one.example/a"
          =<< textAt "target" args

  , testCase "a description typed over the old one is sent as it was typed" $
      bootOf shell "" 500 "o" "press:Enter ltitle:renamed press:Enter" $
        \answer -> do
          [cmd] <- listAt "commands" answer
          args <- field "args" cmd
          assertEqual "the text" "renamed" =<< textAt "desc" args

    -- The popup CLOSES on the press, both outcomes alike, which is `o'\''s own
    -- rule — and it has to: the spans it holds describe the file as it was, and
    -- the write has just moved it.  `o' again is one keystroke and comes back
    -- with fresh ones.
  , testCase "the commit closes the popup, and the log names both ends" $
      bootOf shell "" 500 "o" "press:Enter lurl:https://new.example press:Enter" $
        \answer -> do
          assertEqual "the popup is down" "" =<< textAt "popup" answer
          assertEqual "and the overlay with it" False =<< boolAt "lopen" answer
          assertEqual "the pill names what moved"
            "o → org-glance-overview:open (link edited: https://one.example/a → \
            \https://new.example · 1)"
            =<< textAt "echo" answer
          assertEqual "and the log names the row it landed on"
            (Just "headline \"one\" link edited: https://one.example/a → \
                  \https://new.example")
            =<< lastLog answer

  , testCase "a link nobody changed costs no write" $
      bootOf shell "" 500 "o" "press:Enter press:Enter" $ \answer -> do
        assertEqual "nothing posted" [] =<< namesOf answer
        assertEqual "the popup is down all the same" "" =<< textAt "popup" answer
        assertEqual "and the pill says so"
                    "o → org-glance-overview:open (unchanged)" =<< textAt "echo" answer

  , testCase "an emptied target is refused here, since a link points somewhere" $
      bootOf shell "" 500 "o" "press:Enter lurl: press:Enter" $ \answer -> do
        assertEqual "nothing posted" [] =<< namesOf answer
        assertEqual "the pill says why"
          "o → org-glance-overview:open (a link points somewhere)"
          =<< textAt "echo" answer

  , testCase "ESC over an open link puts it back and leaves the popup standing" $
      bootOf shell "" 500 "o" "press:Enter lurl:https://new.example press:Escape" $
        \answer -> do
          assertEqual "the overlay is gone" False =<< boolAt "lopen" answer
          assertEqual "the popup is not" "on" =<< textAt "popup" answer
          assertEqual "nothing was posted" [] =<< namesOf answer
          assertEqual "and the pill says the link stands"
            "ESC → keyboard-quit (link unchanged)" =<< textAt "echo" answer

  , testCase "and a second ESC closes the popup" $
      bootOf shell "" 500 "o" "press:Enter press:Escape press:Escape" $
        assertEqual "down" "" <=< textAt "popup"

    -- THE HAZARD THE SHARED MECHANISM ANSWERS, on the third surface: no KEY can
    -- move the cursor under an open field, but a MOUSE CLICK can, and a commit
    -- that re-read the cursor would send the text typed for one link against
    -- another link's span.  The commit is handed the row the overlay OPENED
    -- over, so the click moves nothing.
  , testCase "a click under an open link cannot redirect the write" $
      bootOf shell "" 500 "o" "press:Enter lurl:https://new.example click:2 press:Enter" $
        \answer -> do
          [cmd] <- listAt "commands" answer
          args <- field "args" cmd
          assertEqual "the span is the one the overlay opened over"
            [10, 48] =<< spanOf args
          assertEqual "and the target is what was typed for it" "https://new.example"
            =<< textAt "target" args

    -- A held key must not be a browser tab per repeat, which is why the command
    -- is on the ONCE list beside the writes.
  , testCase "a held o asks once" $
      bootOf shell "" 500 "o" "repeat:o repeat:o repeat:o" $
        assertEqual "one request" ["/links?id=r1"] <=< textsAt "linked"

  , testCase "a refused answer is one cmd error line and no popup" $
      bootOf shell "" 500 "" "refuse press:o" $ \answer -> do
        assertEqual "nothing opened" [] =<< openedOf answer
        assertEqual "no popup" "" =<< textAt "popup" answer
        assertEqual "and the log carries the server's own words"
                    (Just "open failed: no headline with id r1") =<< lastLog answer

    -- A tab can be pointed at http(s) and at nothing else, and the TYPE is what
    -- says so — the server's own word rather than a regex this page runs over
    -- the target a second time.  Org writes plenty of other link types and
    -- `/links' reports them all, so the COMMIT is where the judgement lands,
    -- which is one function for both paths: the lone link that opens without
    -- asking and the popup row `o' picks.
  , testCase "a single link that is not http(s) opens nothing and says so" $
      bootOf shell "" 500 "" "onemailto press:o" $ \answer -> do
        assertEqual "no tab" [] =<< openedOf answer
        assertEqual "and no popup, since one link never raises one" ""
          =<< textAt "popup" answer
        assertEqual "the pill names the command and the refusal"
                    "o → org-glance-overview:open (link type not implemented)"
          =<< textAt "echo" answer
        assertEqual "and the log warns, naming the target"
                    (Just "link type not implemented: mailto:t@example.org")
          =<< lastLog answer

    -- The popup still LISTS every link the row holds — that is what teaches a
    -- reader what is in the entry — and `o' is where the answer is given.
  , testCase "an o on a non-http row refuses the same way" $
      bootOf shell "" 500 "o" "press:n press:n press:o" $ \answer -> do
        assertEqual "nothing opened" [] =<< openedOf answer
        assertEqual "the popup is down all the same" "" =<< textAt "popup" answer
        assertEqual "the pill says why"
                    "o → org-glance-overview:open (link type not implemented)"
          =<< textAt "echo" answer

  , testCase "and an http row beside it still opens" $
      bootOf shell "" 500 "o" "press:o" $
        assertEqual "the first one" [("https://one.example/a", "_blank", "noopener")]
          <=< openedOf

    -- Every type the server derives, drawn.  The badge column carries whatever
    -- word came back — the six the palette declares hues for and the catch-all
    -- alike — because a type this page has never seen is still a fact about the
    -- link and hiding it would teach less than showing it uncoloured.
  , testCase "every type the server derives reaches the badge cell" $
      bootOf shell "" 500 "" "everytype press:o" $ \answer ->
        assertEqual "one word per row"
          ["https", "http", "glance", "mailto", "id", "file", "other"]
          . map head =<< pairsAt "llinks" answer

    -- One walk down the same popup, `o' on every row: the two followable rows
    -- open the tab that row points at and the five others open none, each
    -- saying so by name.  The steps are the table's own, so what this asserts
    -- is `followable' reading the type — one rule over seven values rather than
    -- two cases and a list of exceptions.
  , testCase "and only the followable ones open a tab" $
      forM_ [ (0 :: Int, "https://a.example", True)
            , (1, "http://b.example",         True)
            , (2, "org-glance-visit:XYZ",     False)
            , (3, "mailto:t@example.org",     False)
            , (4, "id:99",                    False)
            , (5, "file:notes.org",           False)
            , (6, "Some Headline",            False) ] $
        \(at, target, opens) ->
          bootOf shell "" 500 ""
            ("everytype press:o " <> T.replicate at "press:n " <> "press:o") $ \answer -> do
              assertEqual (T.unpack target)
                [(target, "_blank", "noopener") | opens] =<< openedOf answer
              if opens then pure () else
                assertEqual "and the refusal names the target"
                  (Just ("link type not implemented: " <> target)) =<< lastLog answer
  ]

-- | The @span@ an @edit-link@ body carries, as the pair of offsets it is.
spanOf :: Value -> IO [Int]
spanOf args = traverse number =<< listAt "span" args
  where number (Number n) = pure (round n)
        number other = assertFailure ("expected a number in span, got " <> show other)

-- | Every tab the page opened: the URL, the target name and the window features
-- — @noopener@ being half of what makes following a link safe.
openedOf :: Value -> IO [(T.Text, T.Text, T.Text)]
openedOf answer = traverse one =<< listAt "opened" answer
  where one v = (,,) <$> textAt "url" v <*> textAt "target" v <*> textAt "features" v

-- | @a@: the agenda, which is a canned VIEW rather than a mode.
--
-- One query through the door @g@ uses — into the URL, asked of the server,
-- mounted as the renderer's chips — and its ORDER is a token of that query
-- rather than a call behind the answer, so the whole view is one string.
agendaSpec :: IO T.Text -> TestTree
agendaSpec shell = testGroup "Shell agenda"
  [ testCase "applies its query the way g applies the tree's default" $
      bootOf shell "?q=" 500 "a" "" $ \answer -> do
        assertEqual "the boot's two, then the remount's one"
          [ "/headlines?limit=100", "/headlines"
          , "/headlines?q=state%3A*active*%20-planned%3A*empty*%20sort%3Ascheduled" ]
          =<< textsAt "asked" answer
        assertEqual "and the URL it settles on is that query"
                    "?q=state%3A*active*+-planned%3A*empty*+sort%3Ascheduled"
          =<< textAt "url" answer

    -- The order is IN the query, so the server answers page one in it and the
    -- renderer reads the chain off the same string.  Nothing is asked of the
    -- handle: a canned view that had to call for its order could state one the
    -- query it applied did not.
  , testCase "the rows land in scheduled order, and the query is what says so" $
      bootOf shell "?q=" 500 "a" "" $ \answer -> do
        assertEqual "the chain the query named" [("scheduled", True)]
          =<< chainOf answer
        assertEqual "and no sort was asked of the renderer" 0
          =<< intAt "sortCalls" answer

    -- DEL walks out of the order the way it walks out of the filter: the sort
    -- token is the query's last one, so one press takes it off and the answer
    -- comes back in the view's own order.
  , testCase "and DEL takes the order back off, one token like any other" $
      bootOf shell "?q=" 500 "a" "press:Backspace" $ \answer -> do
        assertEqual "the query the strip left"
                    "?q=state%3A*active*+-planned%3A*empty*" =<< textAt "url" answer
        assertEqual "asked for without the order"
                    (Just "/headlines?q=state%3A*active*%20-planned%3A*empty*")
          . lastOf =<< textsAt "asked" answer

  , testCase "and the pill names the command and the count the server answered" $
      bootOf shell "?q=" 3 "a" "" $
        assertEqual "counted by the server, not by the page it painted"
                    "a → org-glance-agenda (agenda · 3 rows)" <=< textAt "echo"

  , testCase "one row is one row" $
      bootOf shell "?q=" 1 "a" "" $
        assertEqual "singular" "a → org-glance-agenda (agenda · 1 row)" <=< textAt "echo"

    -- An asset with no sort calls at all applies the same view: the order is a
    -- token of the query, so there is nothing for this page to ask for and
    -- nothing to feature-detect on the way in.  What an old asset loses is the
    -- ORDER, which the server still answers in.
  , testCase "an asset without a programmatic sort still applies the view" $
      bootOf shell "?q=" 500 "" "sortless press:a" $ \answer -> do
        assertEqual "no sort was asked for" Nothing =<< sortOf answer
        assertEqual "the query still went, order and all"
                    "?q=state%3A*active*+-planned%3A*empty*+sort%3Ascheduled"
          =<< textAt "url" answer

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
        assertEqual "one remount, so one fetch behind the boot's"
          [ "/headlines?limit=100", "/headlines"
          , "/headlines?q=state%3A*active*%20-planned%3A*empty*%20sort%3Ascheduled" ]
          <=< textsAt "asked"
  ]

-- | @\@@: the drill, and the ladder DEL walks back down it.
--
-- One semantic at two grains.  A JUMP pushes a crumb and applies a whole new
-- query; a REFINEMENT edits the query in place and pushes nothing.  DEL undoes
-- whichever is nearest: tokens while the query has any, then one crumb.
--
-- The stack is the RENDERER's — this page keeps no copy — and it crosses a
-- remount through the URL, which is the only channel it has and the reason
-- @stash@\/@restore@ say nothing about it.
drillSpec :: IO T.Text -> TestTree
drillSpec shell = testGroup "Shell drill"
  [ testCase "@ applies a ref view over the row at point and leaves a crumb" $
      bootOf shell "" 500 "@" "" $ \answer -> do
        -- The boot's three, then the drill's PROBE, then the view it applied:
        -- `@' asks whether there is anything to land on before it lands.
        assertEqual "the boot's three, the probe, then the drill's"
          [ "/headlines?q=state%3A*active*&limit=100", "/headlines?q=state%3A*active*"
          , "/headlines", "/headlines?q=ref%3Ar1&limit=1"
          , "/headlines?q=ref%3Ar1" ]
          =<< textsAt "asked" answer
        -- The crumb records where the reader was STANDING, so the label is the
        -- query being left rather than the one being applied.
        assertEqual "one crumb, naming the view it came from"
                    ["state:*active*"] =<< textsAt "crumbs" answer

    -- A drill out of the EMPTY query pushes NOTHING, because "all rows" IS the
    -- empty filter: the ladder's first rung already lands there — strip the
    -- `ref:' token, the query goes empty, and with no trail behind it the key
    -- clears the filter — so a crumb would restore the view DEL reaches anyway.
    -- What goes with it is the remembered row: the walk back lands on the first
    -- row, like every applied view that is not a pop.
  , testCase "@ out of an empty query leaves no crumb, and DEL is still the way back" $
      bootOf shell "?q=" 500 "@" "" $ \answer -> do
        assertEqual "the view is applied all the same"
          [ "/headlines?limit=100", "/headlines", "/headlines?q=ref%3Ar1&limit=1"
          , "/headlines?q=ref%3Ar1" ]
          =<< textsAt "asked" answer
        assertEqual "and the strip carries no chip" [] =<< textsAt "crumbs" answer

    -- Pressed as an ACT, so the drill's remount has landed before the key that
    -- walks back out of it: with no crumb to pop, DEL has only the mounted
    -- query to strip and the old mount's was empty.
  , testCase "and that DEL lands on all rows, first row selected" $
      bootOf shell "?q=" 500 "@" "press:Backspace" $ \answer -> do
        url <- textAt "url" answer
        assertBool ("the filter is cleared rather than popped: " <> T.unpack url)
                   ("?q=&" `T.isPrefixOf` url || url == "?q=")
        assertEqual "named as the clearing it is"
                    "DEL → filter-drop-token (filter cleared)" =<< textAt "echo" answer
        assertEqual "on the first row" (Just "r1") =<< maybeTextAt "selected" answer

    -- ZERO REFERENCES IS NO JUMP.  The drill is PROBED — the same query under
    -- `limit=1', which is a count — and nothing pointing at the row leaves the
    -- table, the filter and the trail exactly where they were: an empty view is
    -- the one landing a reader can read nothing off, and walking back out of it
    -- costs a keystroke to undo a keystroke.
  , testCase "@ onto a row nothing refers to applies no view at all" $
      bootOf shell "" 500 "" "noreferences press:@" $ \answer -> do
        assertEqual "the probe, and nothing behind it"
          [ "/headlines?q=state%3A*active*&limit=100", "/headlines?q=state%3A*active*"
          , "/headlines", "/headlines?q=ref%3Ar1&limit=1" ]
          =<< textsAt "asked" answer
        assertEqual "the view standing is the one the reader was on"
                    "?q=state%3A*active*" =<< textAt "url" answer
        assertEqual "no crumb was pushed" [] =<< textsAt "crumbs" answer
        assertEqual "the pill says why nothing moved"
                    "@ → org-glance-overview:relations (no references to \"one\")"
          =<< textAt "echo" answer
        assertEqual "and the log names the headline"
                    (Just "no references to headline \"one\"") =<< lastLog answer

  , testCase "the pill names the command, the row and the count" $
      bootOf shell "" 3 "@" "" $
        assertEqual "counted by the server"
                    "@ → org-glance-overview:relations (references of \"one\" · 3)"
          <=< textAt "echo"

  , testCase "the trail and its labels ride in the URL beside the query" $
      bootOf shell "" 500 "@" "" $ \answer -> do
        url <- textAt "url" answer
        assertBool ("the ref query is applied: " <> T.unpack url)
                   ("q=ref%3Ar1" `T.isInfixOf` url)
        assertBool ("the trail rides with it: " <> T.unpack url)
                   ("crumbs=" `T.isInfixOf` url)

    -- The ladder's second rung.  The drill left `ref:r1' as the whole query, so
    -- ONE DEL empties it and walks back out — a step out and a step back rather
    -- than a step and a half.
  , testCase "DEL on an emptied query pops the crumb and applies it" $
      bootOf shell "" 500 "@" "press:Backspace" $ \answer -> do
        assertEqual "back on the view the drill left"
                    "?q=state%3A*active*" =<< textAt "url" answer
        assertEqual "and the trail is spent" [] =<< textsAt "crumbs" answer
        assertEqual "the pill names where it landed"
                    "DEL → filter-drop-token (back to state:*active*)"
          =<< textAt "echo" answer

    -- The first rung is unchanged: while the query still has tokens, DEL takes
    -- one off and the trail is not touched.  A REFINEMENT edits the query in
    -- place, so undoing one is a token rather than a crumb — which is the whole
    -- of what makes the two grains one key.
  , testCase "DEL over a refined drill strips a token before it pops" $
      bootOf shell ("?q=ref%3Ar1%20tanik&crumbs=" <> bootedTrail) 500 "Backspace" ""
        $ \answer -> do
        assertEqual "the crumb is still standing" ["everything"]
          =<< textsAt "crumbs" answer
        url <- textAt "url" answer
        assertBool ("the ref token survived the strip: " <> T.unpack url)
                   ("q=ref%3Ar1" `T.isInfixOf` url)

    -- With no trail behind it the key does what it always did, which is the
    -- rung that was there before the ladder had a second one.
  , testCase "DEL with an empty stack clears the filter as it always has" $
      bootOf shell "" 500 "Backspace" "" $ \answer -> do
        assertEqual "the cleared query, present and empty" "?q="
          =<< textAt "url" answer
        assertEqual "the pill says so"
                    "DEL → filter-drop-token (filter cleared)" =<< textAt "echo" answer

  , testCase "g is home and throws the trail away" $
      bootOf shell "" 500 "@" "press:g" $ \answer -> do
        assertEqual "no crumbs left" [] =<< textsAt "crumbs" answer
        assertEqual "and the URL is the default view, with no trail on it"
                    "?q=state%3A*active*" =<< textAt "url" answer

    -- A `view-changed' close rebuilds the mount, and `setView' drops the crumbs
    -- with the world they described.  The URL is what puts them back.
  , testCase "a remount restores the trail and the labels" $
      bootOf shell "" 500 "@" "close:view-changed" $ \answer -> do
        -- The boot, the drill's own remount, and the one the close caused.
        assertEqual "mounted three times" 3 =<< intAt "mounts" answer
        assertEqual "the crumb survived the remount" ["state:*active*"]
          =<< textsAt "crumbs" answer
        assertEqual "and the ref view is still what is applied"
                    "?q=ref%3Ar1" . T.takeWhile (/= '&') =<< textAt "url" answer

    -- And the restored trail is LIVE rather than decorative: DEL walks back
    -- down it after the remount the same way it would have before one.
  , testCase "and the trail a remount put back can still be walked" $
      bootOf shell "" 500 "@" "close:view-changed press:Backspace" $ \answer -> do
        assertEqual "back on the view the drill left"
                    "?q=state%3A*active*" =<< textAt "url" answer
        assertEqual "the trail is spent" [] =<< textsAt "crumbs" answer

  , testCase "a booted trail is restored from the URL and can be walked back" $
      bootOf shell ("?q=ref%3Ar1&crumbs=" <> bootedTrail) 500 "" "" $ \answer ->
        assertEqual "the trail the address bar carried" ["everything"]
          =<< textsAt "crumbs" answer

  , testCase "and DEL walks that booted trail back out" $
      bootOf shell ("?q=ref%3Ar1&crumbs=" <> bootedTrail) 500 "Backspace" "" $ \answer -> do
        assertEqual "landed on the crumb's own query" "?q=" =<< textAt "url" answer
        assertEqual "naming it by its label"
                    "DEL → filter-drop-token (back to everything)" =<< textAt "echo" answer

    -- A crumb remembers the SELECTION it was pushed from, so walking back puts
    -- the cursor where the reader left it rather than at the top of a view they
    -- had moved down into.  It rides BESIDE the trail: the renderer's `crumbOf'
    -- keeps a crumb's label and query and drops everything else, so a selection
    -- put inside one would never come back out of `getCrumbs()'.
  , testCase "a pop puts the cursor back on the row the drill was launched from" $
      bootOf shell "" 500 "n n @" "press:Backspace" $ \answer -> do
        assertEqual "back on the third row" "r3" =<< textAt "selected" answer
        assertEqual "and the trail is spent" [] =<< textsAt "crumbs" answer

  , testCase "and the column it was in, when one was set" $
      bootOf shell "" 500 "n f @" "press:Backspace" $ \answer -> do
        assertEqual "the row" "r2" =<< textAt "selected" answer
        assertEqual "and the cell it was on" 0 =<< intAt "col" answer

    -- Never force a missing id: a row the popped answer no longer holds falls
    -- through to the ordinary landing rather than being selected in absentia.
  , testCase "a remembered row the answer lost falls back to the first row" $
      bootOf shell "" 500 "n n @" "rows:2 press:Backspace" $ \answer -> do
        assertEqual "the store lost r3, so the landing is row one" "r1"
          =<< textAt "selected" answer

  , testCase "the remembered selection rides in the URL with the trail" $
      bootOf shell "" 500 "n @" "" $ \answer -> do
        url <- textAt "url" answer
        assertBool ("the pair is carried: " <> T.unpack url)
                   ("sels" `T.isInfixOf` url)

    -- Every application that is NOT a pop lands on the first row of the answer:
    -- `g' here, and a commit below, which repaints rather than remounting and
    -- would otherwise leave the cursor on a row the answer may not hold.
  , testCase "g lands on the first row rather than where the reader was" $
      bootOf shell "" 500 "n n g" "" $
        assertEqual "row one" "r1" <=< textAt "selected"

    -- A commit REPAINTS rather than remounting, so without the rule the cursor
    -- would sit wherever it was over a set that may not hold that row at all.
    -- `DEL' is the commit this suite can drive: it strips a token and commits
    -- what is left, which is the same door a palette commit goes through.
  , testCase "a commit that repaints lands on the first row too" $
      bootOf shell "?q=tanik%20web" 500 "n n Backspace" "" $ \answer -> do
        assertEqual "row one" "r1" =<< textAt "selected" answer
        assertEqual "and it was a strip rather than a pop" "?q=tanik"
          =<< textAt "url" answer

    -- A held `@' is a remount per repeat, each leaving a crumb behind, which is
    -- why the command is on the ONCE list beside the other view keys.
  , testCase "a held @ drills once" $
      bootOf shell "" 500 "@" "repeat:@ repeat:@" $
        assertEqual "one crumb, not three" ["state:*active*"] <=< textsAt "crumbs"

    -- An asset predating the trail is told so rather than being made to apply a
    -- view the reader would have no way back out of.
  , testCase "an asset with no crumbs refuses the drill and stays put" $
      bootOf shell "" 500 "" "crumbless press:@" $ \answer -> do
        assertEqual "the boot's fetches and no more"
          [ "/headlines?q=state%3A*active*&limit=100", "/headlines?q=state%3A*active*"
          , "/headlines" ] =<< textsAt "asked" answer
        assertEqual "and the pill says which call is missing"
                    "@ → org-glance-overview:relations (this table-view.js has no crumbs)"
          =<< textAt "echo" answer
  ]

-- | A trail as an address bar carries it: one crumb standing for the unfiltered
-- view, and the label the live @ref:@ chip wears.
bootedTrail :: T.Text
bootedTrail = "%7B%22trail%22%3A%5B%7B%22label%22%3A%22everything%22%2C%22query%22%3A%22%22%7D%5D%2C%22labels%22%3A%7B%7D%7D"

-- | The sort the agenda asked the renderer for, if any.  Through `field', so a
-- harness that stopped reporting the call at all fails loudly rather than
-- reading as a page that asked for none.
sortOf :: Value -> IO (Maybe (T.Text, Bool))
sortOf answer = field "sorted" answer >>= said
  where said Null   = pure Nothing
        said sorted = Just <$> orderKeyOf sorted

-- | The CHAIN in force, highest priority first — which the applied query names
-- and no call has to have made.
chainOf :: Value -> IO [(T.Text, Bool)]
chainOf answer = traverse orderKeyOf =<< listAt "chain" answer

-- | One key of a sort chain, wherever it is read from: the wire's @sort@ array,
-- the harness's chain, or the last sort a call asked for.
orderKeyOf :: Value -> IO (T.Text, Bool)
orderKeyOf key = (,) <$> textAt "column" key <*> boolAt "ascending" key

-- | The last of XS, or 'Nothing' where there is none.
lastOf :: [a] -> Maybe a
lastOf = listToMaybe . reverse

-- | The which-key letters: the assignment, driven as the pure function it is,
-- and the list it draws.  The letters are what a reader learns by heart, so
-- what is pinned is that one cycle always yields the same ones — the rule is
-- order-only and each entry claims the first still-free letter of its OWN
-- spelling.  Which rows a commit names is @commandKeySpec@'s subject.
whichKeySpec :: IO T.Text -> TestTree
whichKeySpec shell = testGroup "Shell which-key"
  [ testCase "the assignment, cycle by cycle" $ mapM_ (assigns shell)
      -- The chain as the resolver now draws it: org's pair leads, so TODO takes
      -- `t' and DONE takes `d' whatever a narrower scope declares, and
      -- DELEGATED falls through to its own `e'.  The letters a reader learns
      -- for the two words every tree has are the same in every tree.
      [ ( "TODO,DONE,DELEGATED", ["t@0", "d@0", "e@1"] )
      -- The same three words drawn the other way round, which is what the old
      -- nearest-scope order did to a tree whose file or tag declared DELEGATED:
      -- it claimed `d' and DONE was pushed off it.  Order-only, so the rule did
      -- not have to change for this to stop happening — the chain did.
      , ( "DELEGATED,TODO,DONE", ["d@0", "t@0", "o@1"] )
      -- A whole tree's, in the order the producer sends it — actives as
      -- declared, then the done-like ones.  Nothing is special-cased: DONE is
      -- `o' for the reason DELEGATED is `e'.  `*empty*' is not in it: the meta
      -- answers to DEL and is kept out of the pool by `offer', so this is what a
      -- palette actually hands the rule.
      , ( "TODO,NEXT,STARTED,WAITING,DELEGATED,CANCELLED,DONE"
        , ["t@0", "n@0", "s@0", "w@0", "d@0", "c@0", "o@1"] )
      -- Synthetic, since no real cycle exhausts a letter pool: an entry with
      -- nothing left is UNBOUND rather than stealing one, which is what keeps
      -- the letters above it where they were.
      , ( "ON,NO,NOON", ["o@0", "n@0", "-"] )
      -- The letter `*empty*' used to take is the one a cycle keeps now that the
      -- meta answers to DEL: `CANCELLED' claims `c' outright where it once had
      -- to share the pool with a word spelled `*empty*'.
      , ( "CANCELLED,CLOSED",     ["c@0", "l@1"] ) ]

    -- What the reader sees, and why: one row per SOURCE in precedence order,
    -- widest first, its keywords in the Active and Inactive cells, each an
    -- accent-boxed key token and the word with the claimed letter BOLD WHERE IT
    -- SITS — which is the whole of the teaching.  The table IS the classify
    -- chain: `TODO' under `default' and `READING' under `book' say which scope
    -- answered for each.  Every source is drawn under the NAME it arrives
    -- under, so the page keeps no table of labels.  The meta spans a row of its
    -- own at the foot, in the muted italic every starred value wears.
  , testCase "the table draws one row per source, keywords in their cells" $
      bootOf shell "" 500 "C-c C-t" "" $ \answer -> do
        assertEqual "the header, the sources in order, and the meta last"
          [ ("pr ph", "source",   ["active"],      ["inactive"])
          , ("pr",    "default",  ["[T]ODO"],    ["[D]ONE"])
          , ("pr",    "book",     ["[R]EADING"], ["R[E]AD"])
          , ("pr",    "file",     ["[L]ATER"],   [])
          , ("pr pm", "",         ["DEL *empty*"], []) ] =<< paletteOf answer
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
        assertEqual "the default pair, then book, then film"
          [ ("pr ph", "source",   ["active"],       ["inactive"])
          , ("pr",    "default",  ["[T]ODO"],     ["[D]ONE"])
          , ("pr",    "book",     ["[R]EADING"],  ["R[E]AD"])
          , ("pr",    "film",     ["[W]ATCHING"], ["W[A]TCHED"])
          , ("pr pm", "",         ["DEL *empty*"],  []) ] <=< paletteOf

    -- The hues are the producer's and travel on the state column; the
    -- resolution names keywords alone, so the palette goes and looks each one
    -- up.  A keyword no badge names carries none, and is drawn all the same.
    -- The claimed letter is marked INSIDE the keyword — there is no token
    -- column — and the rule under it takes that state's own badge hue, so the
    -- one thing telling a reader which key commits is drawn in the colour the
    -- word is already wearing.  `*empty*' is the exception and says why: DEL
    -- names no position in a word to be marked at, so that row alone keeps a
    -- token.
  , testCase "the letter is marked in the word, and only *empty* wears a token" $
      bootOf shell "" 500 "C-c C-t" "" $ \answer -> do
        assertEqual "one token in the whole table, on the meta row"
                    ["DEL"] . filter (not . T.null) . map snd
          =<< paletteField "key" answer
        -- The rule takes the badge hue by value, so a keyword the palette
        -- names without one is marked by weight alone.
        assertEqual "and the rule under each letter is that keyword's own hue"
          [ ("[T]ODO", "#e0af68"), ("[D]ONE", "#73daca"), ("[R]EADING", "#bb9af7") ]
                    . filter (not . T.null . snd)
          =<< paletteField "mark" answer

    -- DEL is `*empty*'\''s, and the state palette is the only one carrying such
    -- an entry.  Over the tags popup's own field the press is the field's text
    -- editing and nothing else: it commits nothing, it does not reach the popup
    -- underneath, and the map's own DEL is already dead under `typing()'.
  , testCase "DEL fires nothing in a palette that has no clear" $
      bootOf shell "" 500 ":" "press:+ press:Backspace" $ \answer -> do
        assertEqual "no command went" [] =<< postedOf answer
        assertEqual "the field is still up" "narrow" =<< textAt "pmode" answer
        assertEqual "and the popup under it is untouched" [["web", "all", "40"]]
          =<< pairsAt "ttags" answer

  , testCase "each keyword wears its own badge colour, where there is one" $
      bootOf shell "" 500 "C-c C-t" "" $
        assertEqual "TODO, DONE and READING have badges; LATER and READ do not"
          [ ("[T]ODO", "#e0af68"), ("[D]ONE", "#73daca"), ("[R]EADING", "#bb9af7") ]
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
          [ ("pe pat", "", ["TODO"],    [])
          , ("pe",     "", ["DONE"],    [])
          , ("pe",     "", ["READING"], [])
          , ("pe",     "", ["READ"],    [])
          , ("pe",     "", ["LATER"],   [])
          , ("pe pm",  "", ["*empty*"], []) ] =<< paletteOf answer
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
                    (panelRows sheetStamp [["EFFORT", "0:30"]])
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
                      (panelRows sheetStamp [["EFFORT", "0:45"]])
                      =<< pairsAt "props" answer
          assertEqual "the fields are gone" "" =<< textAt "focus" answer
          assertEqual "the panel still has the keys" True =<< boolAt "pnav" answer
          assertEqual "and the cursor stayed on the row" 3 =<< intAt "pat" answer

    -- THE ROW A COMMIT WRITES IS THE ROW THE OVERLAY OPENED OVER, and the hazard
    -- is the one thing that can move a cursor while a row is open: a MOUSE
    -- CLICK.  No key can — the panel's listener sends every key to the fields
    -- while `pediting()' — so a commit that re-read the cursor would take the
    -- text typed for one row and write it into whichever row the reader landed
    -- on, silently.  The tags popup's rename guarded this from the start and the
    -- panel did not; the shared overlay snapshots at open, so both do.
  , testCase "a click under an open row commits the row that was opened" $
      bootOf shell "" 500 "Enter"
             ("press:Tab press:n press:n press:n press:Enter pval:3=0:45"
                <> " click:0 press:Enter") $ \answer -> do
        assertEqual "the opened row took the text, and the clicked one is untouched"
                    (panelRows sheetStamp [["EFFORT", "0:45"]])
                    =<< pairsAt "props" answer
        assertEqual "the overlay closed" "" =<< textAt "focus" answer

    -- The same hazard from the other side: a click that lands on a row whose
    -- KEY the commit would have rewritten.  The add-row is the case with the
    -- most to lose — its key is the thing being typed — so a redirected commit
    -- would name a property after a planning keyword.
  , testCase "and a click cannot redirect the key an add-row is writing" $
      bootOf shell "" 500 "Enter"
             ("press:Tab press:+ pkey:4=OWNER pval:4=ada"
                <> " click:3 press:Enter") $ \answer ->
        assertEqual "the added row took both fields and EFFORT stands"
                    (panelRows sheetStamp [["EFFORT", "0:30"], ["OWNER", "ada"]])
                    =<< pairsAt "props" answer

    -- `+' is the add affordance, and the whole of it: keyboard-first means the
    -- key IS the offer, where a row that is always empty was chrome every
    -- reader of the panel had to filter back out.
  , testCase "+ adds a property at the end and opens it" $ do
      bootOf shell "" 500 "Enter" "press:Tab press:+" $ \answer -> do
        assertEqual "an empty row at the end"
                    (panelRows sheetStamp [["EFFORT", "0:30"], ["", ""]])
                    =<< pairsAt "props" answer
        assertEqual "with the cursor on it" 4 =<< intAt "pat" answer
        assertEqual "open at its key, which is the thing being typed"
                    "pkey:4" =<< textAt "focus" answer
      bootOf shell "" 500 "Enter" "press:Tab press:+ pkey:4=ADDED press:Enter" $ \answer -> do
        assertEqual "and committing it is a property"
                    (panelRows sheetStamp [["EFFORT", "0:30"], ["ADDED", ""]])
                    =<< pairsAt "props" answer
        assertEqual "with nothing grown under it" 4 =<< intAt "pat" answer

    -- ESC over an open row is the ROW's, and puts back the text it was opened
    -- on; only from nav does the key reach the sheet's own ladder.
  , testCase "ESC puts an open row back, and the next one closes the sheet" $ do
      bootOf shell "" 500 "Enter"
             "press:Tab press:n press:n press:n press:Enter pval:3=0:45 press:Escape" $ \answer -> do
        assertEqual "the value it was opened on"
                    (panelRows sheetStamp [["EFFORT", "0:30"]])
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
                      =<< wroteAt "properties" answer
          assertEqual "and the planning entries it has"
                      [[["SCHEDULED", "<2026-08-01 Sat>"]]]
                      =<< wroteAt "planning" answer
          assertEqual "and it landed" "synced" =<< textAt "state" answer

    -- Emptying every planning row is how the line comes off, which the server
    -- reads as "no planning" rather than as "leave it alone".
  , testCase "an emptied planning row is an entry taken off" $
      bootOf shell "" 500 "Enter"
             "press:Tab press:Enter pval:0= press:Enter press:C-x press:C-s" $
        assertEqual "nothing left to write" [[]]
                    <=< wroteAt "planning"

    -- Emptying a key is how a property is deleted: there is no key to press for
    -- it, and none is owed — the row simply stops naming anything.
  , testCase "an emptied key is a property deleted" $
      bootOf shell "" 500 "Enter"
             ("press:Tab press:n press:n press:n press:Enter pkey:3="
                <> " press:Enter press:C-x press:C-s") $
        assertEqual "the drawer the write asks for" [[]]
                    <=< wroteAt "properties"

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
                      (panelRows sheetStamp [["EFFORT", "0:45"]])
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

    -- ONE FOCUS LANGUAGE, and the state it is drawn from is the panel's own
    -- `on'.  The body pane takes a real focus and the panel holds the keys with
    -- nothing focused at all, so the mark is the FRAME's on both sides — and it
    -- has to leave when the keys do, whichever way they go.
  , testCase "the pane holding the keys wears it, and only while it does" $ do
      bootOf shell "" 500 "Enter" "" $ \answer -> do
        assertEqual "the body pane opens with the keys" "mtext"
          =<< textAt "focus" answer
        assertEqual "so the panel's frame is unmarked" False =<< boolAt "pnav" answer
      bootOf shell "" 500 "Enter" "press:Tab" $ \answer -> do
        assertEqual "crossing marks the panel" True =<< boolAt "pnav" answer
        assertEqual "and takes the focus off the textarea, so its own mark goes"
                    "" =<< textAt "focus" answer
      bootOf shell "" 500 "Enter" "press:Tab press:Tab" $ \answer -> do
        assertEqual "crossing back unmarks it" False =<< boolAt "pnav" answer
        assertEqual "and the body pane has it again" "mtext" =<< textAt "focus" answer
      -- The leak this closes: the sheet used to clear the nav FLAG and leave
      -- the class on, so a panel closed from nav stayed marked behind the
      -- backdrop until the next materialize redrew it.
      bootOf shell "" 500 "Enter" "press:Tab press:Escape" $ \answer -> do
        assertEqual "the sheet is closed" "" =<< textAt "modal" answer
        assertEqual "and the panel's mark went with it" False =<< boolAt "pnav" answer

    -- Where the cursor was left belongs to the sheet that was open: the next
    -- materialize is a fresh drawer, read-only and at the top of itself.
  , testCase "the panel opens at the top again when the sheet is reopened" $
      bootOf shell "" 500 "Enter" "press:Tab press:n press:Escape press:Enter" $
        \answer -> do
          assertEqual "the cursor is back on the first row" 0 =<< intAt "pat" answer
          assertEqual "and the keys back in the body" False =<< boolAt "pnav" answer

    -- THE PANEL IS A MOUNT, and this is what that buys: the rows the reader
    -- moves over are the renderer's rows, the cursor is the renderer's
    -- selection, and this page keeps no copy of either.  The
    -- flag ground is its own opt-in now (flags: true, no mark column drawn),
    -- and the hint line is off, since the key line under the table names every
    -- key once.
  , testCase "the panel is a table-view mount of its own" $
      bootOf shell "" 500 "Enter" "" $ \answer -> do
        assertEqual "one panel mount" 1 =<< intAt "pmounts" answer
        assertEqual "asked for flags alone — the gutter without the checkbox"
                    True =<< boolAt "pflags" answer
        assertEqual "and never for marks (nothing here reads one)"
                    False =<< boolAt "pmarks" answer
        assertEqual "and not for the renderer's own legend" False
                    =<< boolAt "phints" answer
        assertEqual "naming the two keys a flagged row answers to"
                    "d/D delete · u unflag" =<< textAt "pflagHelp" answer
        -- No page size: a drawer is short and every row of it is on screen, so
        -- there is no page for a cursor to fall off the end of.
        assertEqual "and for the whole list at once" 0 =<< intAt "ppage" answer

    -- The table is rebuilt by a remount and the panel is not: it is a sibling of
    -- `#app' like the sheet around it, so what a reopened sheet costs is one
    -- `setRows' rather than a second mount with a second theme listener behind
    -- it.
  , testCase "the panel is mounted once and re-set per sheet" $
      -- ESC closes the sheet and leaves the body pane focused, which is a focus
      -- of its own as far as the map is concerned; the click that takes it off
      -- is what puts the table's own keys back.
      bootOf shell "" 500 "Enter"
             ("press:Escape blur press:Enter press:Escape blur press:Enter"
                <> " close:view-changed") $
        \answer -> do
          assertEqual "the table was rebuilt" 2 =<< intAt "mounts" answer
          assertEqual "the panel never was" 1 =<< intAt "pmounts" answer
          -- Three sheets opened, three drawers handed over: the mount is a view
          -- of the model and a new model is one `setRows'.
          assertEqual "and every drawer arrived through setRows" 3
            =<< intAt "psets" answer

    -- Deletion is the TABLE's gesture over the panel's rows, and the same
    -- renderer state answers it: `d' lays a flag down and the row wears the
    -- wash the mount draws for one.
  , testCase "d flags the row at point rather than deleting it" $
      bootOf shell "" 500 "Enter" "press:Tab press:n press:n press:n press:d" $
        \answer -> do
          assertEqual "the mount is holding the flag" ["P0"]
                      =<< textsAt "pflagged" answer
          assertEqual "and the drawer is untouched"
                      (panelRows sheetStamp [["EFFORT", "0:30"]])
                      =<< pairsAt "props" answer
          assertEqual "the pill says what the second press will do"
                      "d → delete-flag (d again deletes)" =<< textAt "echo" answer
          -- The proof that `pnav' kills the table's own rows: `d' over the table
          -- is `archive-flag', and a second one would post an archive.
          assertEqual "and the table's own d never ran" ([] :: [Value])
                      =<< listAt "commands" answer

    -- `dd' is dired's, and the second press is `D': it takes EVERY flagged row
    -- rather than the one under it, which is what makes the flag the
    -- confirmation.
  , testCase "d again deletes the flagged property, and D is that press alone" $ do
      bootOf shell "" 500 "Enter"
             "press:Tab press:n press:n press:n press:d press:d" $ \answer -> do
        assertEqual "the property is off the panel"
                    (panelRows sheetStamp [])
                    =<< pairsAt "props" answer
        assertEqual "the flag was spent with it" ([] :: [T.Text])
                    =<< textsAt "pflagged" answer
        assertEqual "and the pill named the set" "D → org-delete-property (1 flagged)"
                    =<< textAt "echo" answer
      bootOf shell "" 500 "Enter" "press:Tab press:n press:n press:n press:D" $
        \answer -> do
          assertEqual "D needs no flag: the row at point is the set"
                      (panelRows sheetStamp [])
                      =<< pairsAt "props" answer
          assertEqual "and says so" "D → org-delete-property (row)"
                      =<< textAt "echo" answer

    -- The three planning rows are org's keys rather than the author's, so a
    -- delete CLEARS the entry and the row stands — which is already how an entry
    -- is absent, and how the whole line comes off.
  , testCase "deleting a planning row clears the entry and keeps the row" $
      bootOf shell "" 500 "Enter"
             "press:Tab press:d press:d press:C-x press:C-s" $ \answer -> do
        assertEqual "the row is still there, empty"
                    (panelRows "" [["EFFORT", "0:30"]])
                    =<< pairsAt "props" answer
        assertEqual "and the write carries no planning entry" [[]]
                    =<< wroteAt "planning" answer

    -- `u' is the way back off a flag, and it walks on the way the table's does.
  , testCase "u takes a flag off and steps on" $
      bootOf shell "" 500 "Enter"
             "press:Tab press:n press:n press:n press:d press:u press:D" $ \answer -> do
        assertEqual "nothing was flagged when D ran" ([] :: [T.Text])
                    =<< textsAt "pflagged" answer
        -- `u' stepped off the last row and stayed, so `D' took the row at point:
        -- the property, and not one of org's three.
        assertEqual "so D took the row at point"
                    (panelRows sheetStamp [])
                    =<< pairsAt "props" answer

    -- A held `d' would flag a row and delete it from ONE press, which is the
    -- confirmation the two-press shape exists to be.  The dispatch's own ONCE
    -- list cannot reach a key this listener owns, so the guard is the panel's.
  , testCase "a held d flags once and never deletes what it flagged" $
      bootOf shell "" 500 "Enter"
             "press:Tab press:n press:n press:n press:d repeat:d repeat:d" $
        \answer -> do
        assertEqual "still flagged" ["P0"] =<< textsAt "pflagged" answer
        assertEqual "and still there"
                    (panelRows sheetStamp [["EFFORT", "0:30"]])
                    =<< pairsAt "props" answer

    -- A deletion moves the model, so the sheet is dirty and the way out is a
    -- write — the same rule a committed edit answers to.
  , testCase "a deletion is an edit, and a cancelled one is not" $ do
      bootOf shell "" 500 "Enter"
             "press:Tab press:n press:n press:n press:d press:d press:C-x press:C-s" $
        \answer -> do
          assertEqual "the drawer the write asks for" [[]]
                      =<< wroteAt "properties" answer
          assertEqual "and it landed" "synced" =<< textAt "state" answer
      bootOf shell "" 500 "Enter" "press:Tab press:n press:n press:n press:d press:Escape" $
        \answer -> do
          assertEqual "a flag alone writes nothing" ([] :: [Value])
                      =<< listAt "writes" answer
          assertEqual "and the sheet closed without one" "" =<< textAt "modal" answer

    -- ONE PAIR OF FIELDS, over whichever row is at point.  The mount rewrites
    -- its own rows as it scrolls, so an edit cannot live inside one — it sits
    -- over the panel and is anchored to the row the cursor is on, which is why
    -- opening a second row moves the same overlay rather than growing another.
  , testCase "the edit overlay is one pair of fields over the row at point" $
      bootOf shell "" 500 "Enter"
             ("press:Tab press:Enter press:Escape"
                <> " press:n press:n press:n press:Enter pval:3=0:45 press:Enter") $
        \answer -> do
          assertEqual "the overlay went with the cursor"
                      (panelRows sheetStamp [["EFFORT", "0:45"]])
                      =<< pairsAt "props" answer
          assertEqual "and closed behind it" "" =<< textAt "focus" answer

    -- The hidden properties are not rowed, so they are not flaggable and no
    -- gesture can reach them.  The identity is the case that matters: a key that
    -- deleted it would break the row id every update is keyed off.
  , testCase "nothing hidden is rowed, so nothing hidden is flaggable" $
      bootOf shell "" 500 "Enter" "press:Tab press:n press:n press:n press:D" $
        \answer -> do
          rows <- pairsAt "props" answer
          assertEqual "the identity was never a row"
                      [] [ r | r <- rows, take 1 r == ["ORG_GLANCE_ID"] ]
          assertEqual "and the only property there was the one that went"
                      3 (length rows)
  ]

-- | The settings sheet, driven through the keys a reader presses.  What is
-- asserted is this page's half: that the chord raises it in PANELS over the
-- layers @\/config@ served, that the one box holds the SELECTED file's
-- @#+TODO:@ lines verbatim and that switching layers costs no edit, that the
-- two preference panels apply without touching the server, that closing it is
-- the save, and that a pristine one costs no request.  The splice itself is
-- @configSpec@'s subject and the grammar is @TestConfig@'s; nothing here
-- re-states either.
settingsSpec :: IO T.Text -> TestTree
settingsSpec shell = testGroup "Shell settings"
  [ testCase ", opens it over the layers the server serves" $
      bootOf shell "" 500 "," "" $ \answer -> do
        assertEqual "the sheet is up" "on" =<< textAt "settings" answer
        assertEqual "the first layer's lines, verbatim" "" =<< textAt "cshown" answer
        assertEqual "the union is previewed" "TODO | DONE" =<< textAt "ceff" answer
        assertEqual "and it opens synced" "synced" =<< textAt "cstate" answer
        assertEqual "with nothing written" ([] :: [Value]) =<< listAt "configWrites" answer

    -- ONE SELECT over the layers, system first and then the tags in their own
    -- alphabet.  The server's order is the walk's, so the sheet's is its own —
    -- the fixture serves `film' ahead of `book' precisely so the two differ.
  , testCase "the layers are a select: system first, then the tags in alphabet" $
      bootOf shell "" 500 "," "" $ \answer -> do
        assertEqual "system, then book, then film"
                    ["system", "tag · book", "tag · film"] =<< textsAt "clayers" answer
        assertEqual "opening on the first" "0" =<< textAt "cat" answer
        assertEqual "and the label names the file it is"
                    "system · /o/.org-glance/config/system.org · not created yet"
          =<< textAt "clab" answer

    -- The one box is a VIEW of the selected layer, so picking another swaps what
    -- is in it and nothing else.
  , testCase "picking a layer swaps the box to that file's lines" $
      bootOf shell "" 500 "," "clayer:1" $ \answer -> do
        assertEqual "book's lines" "#+TODO: TODO READING | READ"
          =<< textAt "cshown" answer
        assertEqual "and book's label" "tag · book · /o/.org-glance/config/tags/book.org"
          =<< textAt "clab" answer
        assertEqual "with nothing written" ([] :: [Value]) =<< listAt "configWrites" answer

    -- THE RULE THE STACK OF BOXES USED TO GIVE FOR FREE: an edit belongs to its
    -- layer, and a reader who looks at another one comes back to it.
  , testCase "a switch away and back keeps the edit" $
      bootOf shell "" 500 "," "ctext:#+TODO:_A_|_B clayer:1 clayer:0" $ \answer -> do
        assertEqual "the edit is still there" "#+TODO:_A_|_B" =<< textAt "cshown" answer
        assertEqual "and nothing was written on the way" ([] :: [Value])
          =<< listAt "configWrites" answer

    -- READING A LAYER IS NOT EDITING IT.  Walking the whole select and coming
    -- back is the shape a reader looking for one tag makes, and every layer's
    -- bytes have been through the box by the end of it: nothing may be written,
    -- and what is on screen must be the file's own text down to the spacing.
  , testCase "walking every layer and back writes nothing" $
      bootOf shell "" 500 "," "clayer:1 clayer:2 clayer:0 press:Escape" $ \answer -> do
        assertEqual "no write" ([] :: [Value]) =<< listAt "configWrites" answer
        assertEqual "the sheet is down" "" =<< textAt "settings" answer
  , testCase "and the box shows a layer's lines byte for byte" $
      bootOf shell "" 500 "," "clayer:2 clayer:1" $
        assertEqual "book's line, spacing and bar included"
                    "#+TODO: TODO READING | READ" <=< textAt "cshown"

    -- And every layer edited on the way is written, one drift-locked call per
    -- FILE — which is what the boxes were doing and what one box must not lose.
  , testCase "every layer edited is written, one call each" $
      bootOf shell "" 500 ","
             "ctext:#+TODO:_A_|_B clayer:2 ctext:#+TODO:_C_|_D press:Escape" $
        \answer -> do
          writes <- listAt "configWrites" answer
          assertEqual "two writes, one per file" 2 (length writes)
          paths <- traverse (textAt "path") writes
          assertEqual "the system layer and the one tag layer that moved"
                      [ "/o/.org-glance/config/system.org"
                      , "/o/.org-glance/config/tags/film.org" ] paths
          assertEqual "each carrying its own lines"
                      [["#+TODO:_A_|_B"], ["#+TODO:_C_|_D"]]
            =<< traverse (textsAt "lines") writes

    -- ONE list draws the headers and the order, so a fourth panel is an entry
    -- there rather than a second place that has to hear about it.  The order is
    -- the tab order too: the sheet keeps native tabbing, so the DOM says which
    -- field Tab reaches next.
  , testCase "it is three panels, each under its own header" $
      bootOf shell "" 500 "," "" $
        assertEqual "general, theme, keywords" ["general", "theme", "keywords"]
          <=< textsAt "csecs"

    -- THE LOG KNOB, the general panel's one field that asks no server: it is a
    -- `localStorage' preference like the theme, it applies as it is typed, and
    -- the number lands on the strip itself where the stylesheet's arithmetic
    -- reads it.
  , testCase "the log knob applies as it is typed, and is remembered" $
      bootOf shell "" 500 "," "clog:12" $ \answer -> do
        assertEqual "the cap is on the strip" "12" =<< textAt "logn" answer
        assertEqual "and remembered" "12" =<< textAt "logStored" answer
        assertEqual "the sheet is still up" "on" =<< textAt "settings" answer
        assertEqual "and nothing was written" ([] :: [Value])
          =<< listAt "configWrites" answer

    -- The default is the stylesheet's declared value, so a page nobody has
    -- touched shows seven and stores nothing.
  , testCase "and it opens on seven, with nothing stored" $
      bootOf shell "" 500 "," "" $ \answer -> do
        assertEqual "the boot wrote the default" "7" =<< textAt "logn" answer
        assertEqual "the field is empty" "" =<< textAt "clog" answer
        assertEqual "and the key is not there" "«unset»" =<< textAt "logStored" answer

    -- THE BOOT READS THE PREFERENCE, which no act can reach: every act runs
    -- after the page has already applied it, so the browser has to arrive
    -- remembering one.
  , testCase "a browser that remembers one boots at it" $
      bootWith shell "glance-log=21" "" 500 "" "" $ \answer -> do
        assertEqual "the cap is the stored one" "21" =<< textAt "logn" answer
        assertEqual "and the sheet shows it" "" =<< textAt "clog" answer
  , testCase "and the sheet opens on it" $
      bootWith shell "glance-log=21" "" 500 "," "" $
        assertEqual "the field is the stored value" "21" <=< textAt "clog"

    -- A stored value the band no longer takes — an older build's, a hand-edited
    -- one — falls back rather than being applied.
  , testCase "a stored value outside the band boots at the default" $
      bootWith shell "glance-log=900" "" 500 "" "" $
        assertEqual "the default" "7" <=< textAt "logn"

    -- Emptying it is how a reader asks for the default back, which is why blank
    -- is a value this page takes rather than one it refuses.  What is stored is
    -- NOTHING, since a preference spelling the empty string is a preference.
  , testCase "blanking it restores the default and removes the preference" $
      bootWith shell "glance-log=12" "" 500 "," "clog:" $ \answer -> do
        assertEqual "back to seven" "7" =<< textAt "logn" answer
        assertEqual "with the key gone" "«unset»" =<< textAt "logStored" answer

    -- A value outside the band is DECLINED rather than clamped: the cap a reader
    -- had stands, and the box is redrawn from the preference on the next open.
  , testCase "a value outside the band is declined, and the cap stands" $
      bootOf shell "" 500 "," "clog:12 clog:999" $ \answer -> do
        assertEqual "the cap did not move" "12" =<< textAt "logn" answer
        assertEqual "nor did the storage" "12" =<< textAt "logStored" answer
  , testCase "and so is a value that is no number at all" $
      bootOf shell "" 500 "," "clog:12 clog:tall clog:0 clog:-3 clog:3.5" $ \answer -> do
        assertEqual "the cap did not move" "12" =<< textAt "logn" answer
        assertEqual "nor did the storage" "12" =<< textAt "logStored" answer

    -- Reopening draws the stored preference over whatever was left in the box,
    -- which is what makes a refused value cost nothing past the keystroke.
  , testCase "reopening draws the preference back over a refused value" $
      bootOf shell "" 500 "," "clog:12 clog:999 press:Escape press:," $ \answer -> do
        assertEqual "the field shows the preference" "12" =<< textAt "clog" answer
        assertEqual "and the cap is still it" "12" =<< textAt "logn" answer

    -- The theme is a preference rather than a write: it applies as it is
    -- picked, it is stored, and the sheet it was picked in stays where it is.
  , testCase "the theme panel applies and persists without closing the sheet" $
      bootOf shell "" 500 "," "theme:dark" $ \answer -> do
        assertEqual "stamped on the document element" "dark" =<< textAt "theme" answer
        assertEqual "and remembered" "dark" =<< textAt "themeStored" answer
        assertEqual "the sheet is still up" "on" =<< textAt "settings" answer
        assertEqual "and nothing was written" ([] :: [Value])
          =<< listAt "configWrites" answer

    -- `auto' is the attribute coming OFF rather than a third value written into
    -- it, which is what lets the media query decide again.
  , testCase "and auto takes the attribute back off" $
      bootOf shell "" 500 "," "theme:dark theme:auto" $ \answer -> do
        assertEqual "no attribute" "" =<< textAt "theme" answer
        assertEqual "but the choice is remembered" "auto" =<< textAt "themeStored" answer

    -- The focus rule, both halves.  A `SELECT' inside a popup KEEPS the focus —
    -- the popup is a legitimate holder and the table's keys are dead under it —
    -- and the way the keys come back is closing the popup, which is what a
    -- hand-written `blur()' on a control outside one was standing in for.
  , testCase "the sheet's theme select keeps the keys away from the table" $
      bootOf shell "" 500 "," "theme:dark press:n" $ \answer -> do
        assertEqual "the select holds the keyboard" "SELECT" =<< textAt "holding" answer
        assertEqual "and the table did not move" "r1" =<< textAt "selected" answer
  , testCase "and closing it is what gives them back" $
      bootOf shell "" 500 "," "theme:dark press:Escape press:n" $ \answer -> do
        assertEqual "the sheet is down" "" =<< textAt "settings" answer
        assertEqual "nothing holds the keyboard" "" =<< textAt "holding" answer
        assertEqual "and the key moved the cursor" "r2" =<< textAt "selected" answer

    -- The sheet's own rule, and the reason it has no buttons: the way out is
    -- the save.  Only the layer that moved is written.
  , testCase "ESC syncs the layers that moved and closes" $
      bootOf shell "" 500 "," "ctext:#+TODO:_TODO_STARTED_|_DONE press:Escape" $
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

    -- The general panel's two fields are `system.org''s two tree-wide LINES,
    -- drawn under their own header and posted in that layer's own write: one
    -- file, one digest, one splice, wherever on the sheet they are shown.
  , testCase "the capture target is a general field, and rides the system write" $
      bootOf shell "" 500 "," "ccap:notes/in.org press:Escape" $ \answer -> do
        writes <- listAt "configWrites" answer
        assertEqual "one write, for the layer that moved" 1 (length writes)
        assertEqual "the system layer" "/o/.org-glance/config/system.org"
          =<< textAt "path" (head writes)
        assertEqual "carrying the target" "notes/in.org" =<< textAt "capture" (head writes)
        assertEqual "and the server holds it now" "notes/in.org"
          =<< textAt "servedCapture" answer

  , testCase "and it opens on what the server serves" $
      bootOf shell "" 500 "," "ccap:notes/in.org press:C-x press:C-s" $
        assertEqual "the field shows what was typed" "notes/in.org" <=< textAt "ccap"

    -- The default view is the other one, and it takes the same road: a general
    -- field, the system layer's write.
  , testCase "the default view is the other general field, on the same write" $
      bootOf shell "" 500 "," "cview:tag:work press:Escape" $ \answer -> do
        writes <- listAt "configWrites" answer
        assertEqual "one write" 1 (length writes)
        assertEqual "carrying the view" "tag:work" =<< textAt "filter" (head writes)
        assertEqual "and the server holds it now" "tag:work" =<< textAt "served" answer

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
      bootOf shell "" 500 "," "clayer:1 ctext:#+TODO:_A_|_B press:C-x press:C-s" $
        \answer -> do
          assertEqual "one write" 1 . length =<< listAt "configWrites" answer
          assertEqual "the sheet is still up" "on" =<< textAt "settings" answer
          assertEqual "and it is synced again" "synced" =<< textAt "cstate" answer

    -- A file that moved under the sheet is a 409 and the sheet stays open at
    -- `conflict', where C-x C-s overwrites and ESC discards — the materialize
    -- sheet's flow, over config files.
  , testCase "a layer that moved underneath lands at conflict, and ESC discards" $
      bootOf shell "" 500 "," "clayer:1 ctext:#+TODO:_A_|_B cmoved press:C-x press:C-s" $
        \answer -> do
          assertEqual "the write was refused" 1 . length =<< listAt "configWrites" answer
          assertEqual "the sheet waits" "conflict" =<< textAt "cstate" answer
          assertEqual "and is still up" "on" =<< textAt "settings" answer
  , testCase "and the second ESC there closes it without writing" $
      bootOf shell "" 500 ","
             "clayer:1 ctext:#+TODO:_A_|_B cmoved press:C-x press:C-s press:Escape" $
        \answer -> do
          assertEqual "no second write" 1 . length =<< listAt "configWrites" answer
          assertEqual "the sheet is down" "" =<< textAt "settings" answer

    -- `C-x C-s' SYNCS MID-EDIT, so the reader is still typing while the write is
    -- out — and a flush that landed must leave the box exactly as they left it.
    -- The old stack of boxes could not get this wrong; one box redrawn from the
    -- text the flush snapshotted can.
  , testCase "a sync that lands does not paint over what is being typed" $
      bootOf shell "" 500 ","
             "clayer:1 ctext:#+TODO:_A_|_B chang press:C-x press:C-s\
             \ ctext:#+TODO:_A_|_B_C cdeliver" $ \answer -> do
        assertEqual "one write went out" 1 . length =<< listAt "configWrites" answer
        assertEqual "and the keystrokes behind it stand" "#+TODO:_A_|_B_C"
          =<< textAt "cshown" answer
        -- Still dirty against what was sent, so the way out writes it.
        assertEqual "the sheet is up" "on" =<< textAt "settings" answer

    -- WITH ONE BOX, a refusal has to bring its own layer with it: the sheet
    -- SELECTS the file that was refused and shows the server's words under it,
    -- since a message under a box showing another layer describes a file the
    -- reader cannot see.  The edit was made on `book' and the reader walked on
    -- to `film' before syncing.
  , testCase "a 409 selects the layer it refused and names it" $
      bootOf shell "" 500 ","
             "clayer:1 ctext:#+TODO:_A_|_B clayer:2 cmoved press:C-x press:C-s" $
        \answer -> do
          assertEqual "the sheet came back to book" "1" =<< textAt "cat" answer
          assertEqual "showing the edit that was refused" "#+TODO:_A_|_B"
            =<< textAt "cshown" answer
          assertContains "with the server's own words under it" "changed on disk"
            =<< textAt "clerr" answer
          assertEqual "and the sheet waits" "conflict" =<< textAt "cstate" answer
          -- And the log names it, since one box can show one refusal.
          strip <- logOf answer
          assertBool "the log names the refused layer"
            (any (T.isInfixOf "tags/book.org" . snd) strip)

    -- The label carries the DIGEST, so a layer this sheet just created has to
    -- stop saying it is not there yet — the sheet is still open on it and the
    -- line above the box is the only thing that says whether the file exists.
  , testCase "a layer the sheet creates stops saying it is not there yet" $
      bootOf shell "" 500 "," "" $
        assertEqual "the system layer has no file behind it"
                    "system · /o/.org-glance/config/system.org · not created yet"
          <=< textAt "clab"
  , testCase "and the write is what takes the words off" $
      bootOf shell "" 500 "," "ctext:#+TODO:_A_|_B press:C-x press:C-s" $ \answer -> do
        assertEqual "the label is the path alone"
                    "system · /o/.org-glance/config/system.org" =<< textAt "clab" answer
        assertEqual "and the box was left as it was" "#+TODO:_A_|_B"
          =<< textAt "cshown" answer

    -- A refusal describes a WRITE, so an edit taken back takes its refusal with
    -- it: the layer matches the file again and there is nothing left to explain.
  , testCase "reverting an edit drops the refusal it earned" $
      bootOf shell "" 500 ","
             "ctext:#+TODO:_A_|_B cmoved press:C-x press:C-s ctext: press:C-x press:C-s" $
        \answer -> do
          assertEqual "one write, the refused one" 1 . length
            =<< listAt "configWrites" answer
          assertEqual "the line under the box is gone" "" =<< textAt "clerr" answer
          assertEqual "and the sheet is synced" "synced" =<< textAt "cstate" answer

    -- The one that matters most here: writing a layer is what moves the
    -- columns, so the close that follows a successful save is `view-changed'.
    -- The sheet is a sibling of `#app' and outlives the remount by where it
    -- sits — asserted rather than assumed, since it is a layout fact.
  , testCase "a view-changed remount leaves the sheet standing" $
      bootOf shell "" 500 "," "clayer:1 ctext:#+TODO:_A_|_B close:view-changed" $
        \answer -> do
          assertEqual "the mount was rebuilt" 2 =<< intAt "mounts" answer
          assertEqual "the sheet is still up" "on" =<< textAt "settings" answer
          assertEqual "with the edit still in it" "#+TODO:_A_|_B"
            =<< textAt "cshown" answer
          assertEqual "on the layer it was made in" "1" =<< textAt "cat" answer
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
    -- SPELLED uppercase in the line and WORN lowercase as its class, so the
    -- colour and the word can never disagree — and one of the six scopes.
  , testCase "every line is a stamp, a severity and a scope" $
      bootOf shell "" 500 "d q" "offline close:resync" $ \answer -> do
        strip <- logOf answer
        assertBool ("stamped: " <> show strip)
                   (all (stamped . stampOf . snd) strip)
        assertEqual "the severity is the class it wears, upcased" []
          [ line | line@(sev, text) <- strip, sevOf text /= T.toUpper sev ]
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
      bootOf shell "" 500 "C-c C-t" "press:Backspace" $ \answer -> do
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
paletteHues = fmap (filter (not . T.null . snd)) . paletteField "color"

-- | Every entry the palette drew, as its word and the muted aside beside it —
-- the tag palette's partial count.  Empty where the entry has none, which is
-- what a tag every target already carries looks like.
paletteHints :: Value -> IO [(T.Text, T.Text)]
paletteHints = paletteField "hint"

-- | Every entry the palette drew, as its word and KEY, flattened out of the
-- table in draw order — each row's active cell, then its inactive one.
paletteField :: T.Text -> Value -> IO [(T.Text, T.Text)]
paletteField key answer = do
  rows <- listAt "plist" answer
  entries <- concat <$> traverse halves rows
  traverse (\e -> (,) <$> textAt "word" e <*> textAt key e) entries
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
bootOf shell = bootWith shell ""

-- | 'bootOf' over a browser that already REMEMBERS something: STORE is
-- @KEY=VALUE@ pairs joined by commas, seeded into @localStorage@ ahead of the
-- glue.  A preference the BOOT reads is unreachable from an act, every act
-- running after the page has already applied it.
bootWith :: IO T.Text -> T.Text -> T.Text -> Int -> T.Text -> T.Text
         -> (Value -> Assertion) -> Assertion
bootWith shell store search total keys acts check = do
  node <- findExecutable "node"
  case node of
    Nothing  -> pure ()
    Just exe -> withTempDir $ \dir -> do
      page <- shell
      glueOf page >>= TIO.writeFile (dir </> "shell.js")
      keysOf page >>= TIO.writeFile (dir </> "keys.json")
      (code, out, err) <- readProcessWithExitCode exe
                            [ harness, dir, T.unpack search, show total
                            , T.unpack keys, T.unpack acts, T.unpack store ] ""
      case code of
        ExitSuccess -> check =<<
          either (\e -> assertFailure ("the harness answered: " <> e)) pure
                 (eitherDecode (BL.fromStrict (TE.encodeUtf8 (T.pack out))))
        _failed -> assertFailure ("the boot harness said: " <> err)

-- | The commands a held key delivers once, as the map declares them.  Named
-- rather than spelled twice: two cases read the list, one for the dispatch that
-- honours it and one for the rule that every entry is a bound command.
--
-- The first five write or destroy; the rest do neither and are here because
-- a leaned-on key is ruinous either way — `o' is a browser tab per repeat and
-- `a' a remount per repeat.
onceNames :: [T.Text]
onceNames = [ "filter-drop-token", "unmark-all", "mark-all"
            , "archive-flag", "org-glance-overview:delete"
            , "org-glance-overview:open", "org-glance-agenda"
              -- A held `@' is a remount per repeat, each leaving a crumb behind
              -- for DEL to walk back one at a time.
            , "org-glance-overview:relations"
              -- And a held `^' re-sorts per repeat and lands on whichever
              -- direction the parity of the count leaves it.
            , "toggle-sort" ]

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
      [ "const PAGE = 100;", "swap ? asking(asked) : `${narrow}limit=${PAGE}`"
      , "r.headers.get(\"X-Glance-Total\")"
      , "if (!swap && a.total > (a.view.rows || []).length)"
      , "if (table && query === asked) paint(b)" ]

  -- SWAP ON THE ANSWER.  The two-phase fetch is the BOOT's, where the first
  -- page is the difference between a table and a blank page; a re-application
  -- has a whole table standing and asks for the whole answer, so the rows go
  -- in one mount rather than a page's worth reflowing a moment later.
  , glue "a view already on screen is replaced in one mount"
      [ "const swap = !!table;"
      , "viewing(load(swap ? asking(asked) : `${narrow}limit=${PAGE}`)).then((a) => {"
      , "else arm(a.total);" ]

  -- THE WASH: one state holder, two reasons, one class.  The view reason is
  -- STEPPED, since an abort overlaps the fetch that replaced it; the socket's
  -- is SET, since a connection refused closes without ever having opened.
  , Glue "the wash is one holder over two reasons"
      [ "const WASH = { view: 300, socket: 400 };"
      , "      n: { view: 0, socket: 0 }, at: { view: 0, socket: 0 },"
      , "      step(why, by) { this.want(why, this.n[why] + by); },"
      , "wash.step(\"view\", 1);"
      , "return p.finally(() => wash.step(\"view\", -1));"
      , "backoff = 1000; wash.want(\"socket\", 0);"
      , "wash.want(\"socket\", 1);"
      , "document.documentElement.classList.toggle(\"stale\"," ]
      -- One class, and the page reads it nowhere: the look is the stylesheet's
      -- whole business, so no branch here may ask whether the wash is on.
      [ "classList.contains", "wash.on.view ?", "if (wash.on" ]

  -- What it dims, and what it must not.  The table and the whole modal band go
  -- under it — a sheet open over stale rows is stale with them — and the parts
  -- that EXPLAIN the state stay legible, since dimming the answer along with
  -- the question leaves the page saying nothing.  ONE property: no blur, since
  -- a stale row is still the row, and no `filter' of any kind, since a filter
  -- would make `#app' the containing block for the renderer's own fixed palette
  -- backdrop and clip it inside the table's box.
  , Glue "the wash dims the table and the overlays, and exempts what explains"
      [ "  html.stale #app,html.stale #modal,html.stale #prompt,html.stale #config,"
      , "  html.stale #links,html.stale #tags{opacity:.55}"
      , "  #app,#modal,#prompt,#config,#links,#tags{transition:opacity .18s ease}" ]
      [ "html.stale #log", "html.stale #kbd"
      , "html.stale #echo", "html.stale body", "stale #app{filter", "filter:blur"
      , "filter:saturate", "filter:grayscale" ]

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
      , "        pool[i].key = letterAt(pool[i].label, cut);"
      -- The pool is the entries with no key of their own, so `*empty*' — which
      -- answers to DEL — spends none of it and a wide cycle keeps the letter
      -- the meta used to take.
      , "      const pool = list.filter((c) => !c.fixed);"
      -- A badge hue is written inline, so it has to be told to give way under
      -- the fallback's cursor row — `--g-sel' is a bright yellow in the light
      -- theme, and this is the one declaration on the page that outranks one.
      , "#plist .pat .pw{color:var(--g-fg)!important}"
      -- The claimed letter is marked INSIDE the keyword and nowhere else: bold,
      -- underlined, and the rule in that state's own badge hue — written inline
      -- per entry, since only the entry knows the hue.  The key-token column
      -- that used to carry the letter is gone with it.
      , "const hot = part(word, \"b\", \"\", c.label[c.cut]);"
      , "if (c.color) hot.style.textDecorationColor = c.color;"
      , ".pw b{font-weight:700;text-decoration:underline;"
      , "text-decoration-thickness:2px;text-underline-offset:2px}"
      -- One entry keeps a token, and it is the one whose key names no position
      -- in a word.
      , "if (!prompting.narrow && c.fixed) part(row, \"span\", \"pk\", c.key);"
      -- And second, that both modes commit through one call, so the letter and
      -- the fallback's RET are the same delivery.
      , "else if (!e.repeat) takeChoice(hit);"
      , "else if (k === \"RET\") takeChoice(prompting.shown[prompting.at] || freely());" ]
      -- No second copy of the assignment, no confirmation step behind a letter
      -- (the palette IS the confirmation), no underline as an element of its
      -- own, and no token slot for an entry that claimed nothing.
      ["const LETTERS", "confirm(", ".pw u{", "part(word, \"u\"", ".pk.off{"
      , "\"pk off\""]

  -- The resolution table's chrome, which behaviour cannot show: the hairline
  -- between two source rows is that row's own top border rather than a divider
  -- element of its own, and the source column wears the muted small lowercase
  -- a tag wears everywhere else on this page.
  , Glue "the palette's hairlines are the table's own borders"
      [ ".pr{display:grid;grid-template-columns:6.5em 1fr 1fr"
      , ".pr+.pr{border-top:1px solid var(--g-border)}"
      , ".ph,.ps{font-size:11px;color:var(--g-mute)}"
      -- `*empty*' spans, since no source declares taking a keyword off.
      , ".pr.pm{grid-template-columns:1fr}" ]
      -- The flat list's divider went with the flat list, and so did the page's
      -- own idea of what the states are: the keywords are the server's answer.
      -- No cell coordinate on an entry either — a cell HOLDS its entries.
      [".psep", "stateChoices", "x.cell ===", "c.at ==="]

  -- The tags popup's rules behaviour cannot show.  First, the union is
  -- FIRST-SEEN rather than sorted: an added tag joins at the END, so a commit
  -- moves no row that was already on screen, where an alphabetical insert in
  -- the middle would take one out from under the cursor.
  , Glue "the tag union is first-seen, and the refresh is the answer"
      [ "for (const r of ttargets) for (const t of r.tags)"
      , "if (seen.indexOf(t) === -1) seen.push(t);"
      -- And second, that what the list shows next comes out of the command's
      -- own per-id answer.  It has to: `/command' never writes the store — the
      -- watch does, a debounce later — so a re-read here would answer with what
      -- the files said BEFORE the write.
      , "const landedIds = (results) =>"
      , "new Set((results || []).filter((x) => x.ok).map((x) => x.id));" ]
      -- No sort over the union, and no second resolution behind a commit.
      ["seen.sort(", "tagsOf(over", "tagsOf(prompting", "tagsOf(ttargets"]

  -- The tags popup is a MOUNT, and a mutable one: three columns declared
  -- server-side, the removal gesture's flags asked for, marks refused, and the
  -- rename overlay laid over the tag CELL — which behaviour cannot show,
  -- because the suite's page has no layout for a geometry read to find.
  , Glue "the tags popup is a mutable mount with a rename overlay"
      [ "const TCOLS = "
      , "tmount = TableView.mount(el(\"ttable\"), { columns: TCOLS, rows: [] },"
      , "{ palette: true, marks: false, flags: true, actionHints: false,"
      , "flagHelp: \"d/D remove · u unflag\" });"
      -- The overlay is the SHARED mechanism over one cell: the popup declares a
      -- shape and nothing about the gesture is spelled twice.
      , "box: \"tedit\", pane: \"tpane\", fields: [\"tname\"], cells: [0, 0],"
      , "const renaming = () => !!edit && edit.o === TROW;"
      , "openEdit(TROW, at);"
      -- And the write is ONE command rather than a remove and an add composed,
      -- over the tag the overlay OPENED on rather than the one under the cursor.
      , "renameTag(edit.row, el(\"tname\").value);"
      , "fire(tagging, \"rename-tag\", over.map((r) => r.id), { from, to },"
      , "`retagged ${args.from}→${args.to}`" ]
      -- THE LETTERS ARE GONE from this list, and with them the palette that
      -- stayed open over its own writes.  No second copy of the tag sets here
      -- either: the popup's rows are derived from the targets on every repaint —
      -- and no second copy of the ROWS, a tag being its own row id.  The rename
      -- overlay's own placer, shutter and snapshot are gone into the shared
      -- mechanism, so their names coming back means two implementations are live.
      [ "tagChoices", "tagVocabulary", "tagCommit", "landedTags", "letterMode"
      , "prompting.sticky", "a letter toggles it", "prompting.letters"
      , "trows", "tagRows()", "placeTag", "shutRename", "renamingFrom"
      , "function tflag" ]

  -- ONE EDIT OVERLAY, over three surfaces.  The property panel opens a row's two
  -- fields, the tags popup one cell as a field over itself and the link popup
  -- two; the class, the anchor, the blur and the SNAPSHOT are one
  -- implementation, and a shape says what differs.
  , Glue "the edit overlay is one mechanism the three surfaces declare a shape for"
      [ "function openEdit(o, row) {"
      , "edit = { o, row };"
      , "el(o.box).className = \"on\";"
      , "o.fill(row);"
      , "o.focus(row);"
      -- The anchor reads the mount's published root and the renderer's own
      -- gutter class, for whichever surface is open, and the shape says which
      -- RANGE of the row's own cells the box covers.
      , "const tr = m && m.el.querySelector(\"tbody tr.tv-sel\");"
      , "const tds = o.cells && [...tr.querySelectorAll(\"td:not(.tv-box)\")];"
      , "const from = tds && tds[o.cells[0]], to = tds && tds[o.cells[1]];"
      , "s.width = `${rt.right - l.left}px`;"
      -- The window resize moves whichever overlay is up, and is registered once
      -- rather than per mount.
      , "window.addEventListener(\"resize\", placeEdit);"
      -- THE SNAPSHOT, which is the bug this retired: a commit reads the row the
      -- overlay OPENED over, never the cursor, so a click that moved the cursor
      -- under an open field cannot redirect the write.
      , "const r = edit.row;"
      , "const pediting = () => !!edit && edit.o === PROW;"
      -- SHARING THE STATE MUST NOT SHARE THE SHUTTER.  The tags popup can stand
      -- over an open materialize sheet — clicking the sheet's chrome blurs its
      -- textarea and every `table' row goes live again — so an unscoped shut
      -- would let the sheet's `drawProps'/`shut' cancel an open tag rename. Each
      -- caller names its own shape, which is the isolation the two hand-written
      -- shutters had.
      , "function shutEdit(o) {"
      , "if (!edit || edit.o !== o) return;"
      , "shutEdit(PROW);"
      , "shutEdit(TROW);"
      , "shutEdit(LROW);" ]
      -- The live cursor read the commit used to make, the per-surface copies of
      -- the gesture, and the unscoped shut that would reach across surfaces.
      [ "prows[patAt()]", "function place()", "function shutRename"
      , "shutEdit();" ]

  -- ONE `d'/`D'/`u' GESTURE, likewise: the two-press rule, the feature
  -- detection, the set-or-row choice and the walk after `u' are written once and
  -- each surface names the four words it says them in.
  , Glue "the flag gesture is one implementation over two surfaces"
      [ "function flagKey(k, s) {"
      , "if (k === \"D\" || (k === \"d\" && flags.indexOf(at) !== -1)) {"
      , "echo(`u → ${s.unflag} (flag cleared)`);"
      , "echo(`d → ${s.flag} (d again ${s.again})`);"
      , "echo(`${k} → ${s.none}`);"
      , "none: \"org-delete-property (no row)\","
      , "unflag: \"delete-unflag\", flag: \"delete-flag\", again: \"deletes\","
      , "none: \"org-toggle-tag (no tag)\","
      , "unflag: \"tag-unflag\", flag: \"tag-flag\", again: \"removes\","
      , "flagKey(k, PFLAGS)", "flagKey(k, TFLAGS)" ]
      -- The two hand-written copies it replaced.
      [ "function pflag", "function tflag", "d → delete-flag (d again deletes)"
      , "d → tag-flag (d again removes)" ]

  -- `@' asks before it applies: a row nothing points at leaves the table, the
  -- filter and the trail where they were.  The probe is a COUNT — one row —
  -- since the number is the whole of what it reads.
  , glue "the drill is probed before it is applied"
      [ "load(`${asking(token)}&limit=1`).then((a) => {"
      , "if (!a.total) {"
      , "drill(b, token, name);" ]

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
      -- frame is re-asked for rather than spliced into them — and the refetch
      -- lands the archive's anchor rather than the first row every other
      -- caller of `fetchRows' takes.
      , "setTimeout(() => fetchRows(settled), 250)" ]
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
      , "if (a.view && query === asked) { paint(a); settled(); }"
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
      [ "function remount(after) { leaving = null; stash(); start(after); }"
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
    -- The event strip is the whole of what says so, the status dot having gone
    -- with the corner it sat in.
  , glue "shows the indexing state and polls out of it"
      [ "r.status === 503", "{ indexing: b }", "if (e.indexing) return indexing("
      , "indexing … ${b.elapsed}s", "setTimeout(resync, 1000)" ]

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
      [ "<div id=\"mpanes\">", "<div id=\"mprops\"><div id=\"mptable\"></div>"
      , "base = raw ? h.org : h.body;"
      , "drawProps(raw ? [] : h.properties || [], raw ? [] : h.planning || []);"
      , "{ body: el(\"mtext\").value, properties: props(), planning: planning() }"
      -- THE PANEL IS A MOUNT.  The renderer draws every list this page has, so
      -- there is one implementation of a row, a cursor and a flag rather than
      -- two — and the options say what the sheet asks of it.
      , "pmount = TableView.mount(el(\"mptable\"), { columns: PCOLS, rows: [] }, {"
      , "        flagHelp: \"d/D delete · u unflag\","
      -- The model is this page's and the mount is a view of it: every change
      -- goes back through `setRows'.
      , "      m.setRows(prowsOf());"
      , "prows.map((r) => ({ id: r.id, cells: { key: r.key, value: r.val } }));"
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
      -- The panel's own keys: TAB crosses the panes and hops the open row's two
      -- fields, nav movement is both spellings of the map's own letters and the
      -- arrows, and RET opens a row and commits it.  Movement is the MOUNT's
      -- step, so the cursor a reader moves is the renderer's.
      , "const k = keyName(e), crossing = k === \"TAB\" || k === \"S-TAB\";"
      , "const rowStep = (k) => (k === \"<down>\" || k === \"n\" || k === \"j\" ? 1"
      , "else if (step) stepIn(pmount, step);"
      , "} else if (crossing) leavePanel();"
      , "const pnav = () => el(\"mprops\").className === \"on\";"
      , "el(\"mprops\").className = \"on\"; el(\"mtext\").blur();"
      -- Nav holds the keys with nothing focused, so the map has to be told —
      -- the value palette's letter mode is the other thing that does.
      , "return pnav() || !!prompting"
      -- The panel stacks under the text when there is no room beside it, which
      -- is a wrap rather than a second breakpoint to keep in step.
      , "#mpanes{flex:1;min-height:0;display:flex;flex-wrap:wrap;gap:10px}"
      , "#sheet.raw #mprops{display:none}"
      -- The pane hosts the mount and positions the overlay, and that is the
      -- whole of what it styles: `.tv-root' brings the frame and draws the rows.
      , "#mprops{flex:1 1 240px;min-width:0;min-height:0;position:relative;"
      , "#mptable .tv-root,#ltable .tv-root,#ttable .tv-root{flex:1;min-width:0;"
      -- The open row's fields sit OVER the row, since the mount rewrites its own
      -- rows as it scrolls, and they land on the text they replace.
      , "#pedit,#tedit,#ledit{display:none;position:absolute;background:var(--g-sel)}"
      , "#pedit input,#tedit input,#ledit input{font:13px/1.5 var(--dk-mono);"
      -- A planning row's key is org's rather than the author's, and says so.
      , "#pkey[readonly]{color:var(--g-mute)}"
      -- ONE FOCUS LANGUAGE: whichever pane holds the keys wears the accent on
      -- its own frame.  Declared for both rather than left to the browser,
      -- which can only dress the one that takes a real focus.
      , "#mtext:focus{outline:none;border-color:var(--g-accent)}"
      , "#mprops.on .tv-root{border-color:var(--g-accent)}" ]
      -- No rows of this page's own: the row element, the stripe, the cursor
      -- class and the movement that painted them are the renderer's now, and a
      -- second spelling of any of them is the thing this replaced.  No tab index
      -- either, and no parser — the page never goes looking for a drawer in the
      -- text it holds.
      [ "tabindex", ":PROPERTIES:", ":END:"
      , ".prow", "pcur", "drawRow", "addRow(" ]

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
      -- N of its own line boxes and no more, computed off the rule's own font
      -- size (`em', so it is not restated) and the padding above it rather than
      -- eyeballed.  N is a CUSTOM PROPERTY declared at the default here, so the
      -- arithmetic is in one place and the settings sheet writes a NUMBER onto
      -- the element.
      , "    --g-logn:7;"
      , "max-height:calc(var(--g-logn) * 1.5em + 2 * 6px + 2 * 1px);"
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
      -- A sheet closing on trouble says so under its OWN scope, which is the
      -- one thing the shared ladder takes from the sheet it is closing.
      , "append(s.scope, \"info\", s.closed);"
      , "scope: \"sync\", state: \"synced\","
      , "closed: \"closed without writing — the file is as it was\","
      , "scope: \"config\", state: \"synced\","
      , "closed: \"settings closed — the files are as they were\","
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
  -- `5'; an unnumbered backdrop painted under both.  The page's own echo pill
  -- stays below the backdrop and dims with everything else.  THREE levels now:
  -- the corner held the third and went with it.
  , Glue "the sheet's backdrop covers the renderer's chrome"
      [ "position:fixed;inset:0;z-index:100;", "position:relative;z-index:101;"
      , "#echo{position:fixed;right:14px;bottom:12px;z-index:2;" ]
      [ "z-index:3" ]

  , glue "the theme is a three-way switch the page honours"
      -- The selector and its three options, under the settings sheet's own
      -- theme panel.
      [ "id=\"themesel\""
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

  -- The log's height, the page's SECOND `localStorage' preference and the
  -- general panel's one field that asks no server.  The stylesheet owns the
  -- arithmetic; what the knob writes is the number, onto the element.  A value
  -- outside the band is declined rather than clamped, and blank is how a reader
  -- asks for the default back.
  , glue "the log's height is a stored preference the general panel edits"
      [ "id=\"clog\""
      , "const LOG = { key: \"glance-log\", def: 7, min: 1, max: 50 };"
      , "if (!t) return LOG.def;"
      , "return /^[0-9]+$/.test(t) && +t >= LOG.min && +t <= LOG.max ? +t : null;"
      , "localStorage.getItem(LOG.key)"
      , "localStorage.setItem(LOG.key, v)"
      , "el(\"log\").style.setProperty(\"--g-logn\", String(n));"
      , "setLogLines(logLines(logPref.get()) || LOG.def);"
      -- Applied as it is TYPED, so the field is a knob rather than a form.
      , "el(\"clog\").addEventListener(\"input\""
      , "if (n === null) return;"
      -- And the sheet draws the preference back over a value that was refused.
      , "el(\"clog\").value = logPref.get();"
      -- An EMPTIED field is a preference that is not there.
      , "else localStorage.removeItem(LOG.key); } catch (e)" ]

  -- THE KEYWORDS PANEL IS ONE SELECT AND ONE BOX.  A tree has as many config
  -- files as it has tags, and a stack of boxes was as tall as that number.  The
  -- text lives on the LAYER rather than in the box, which is what makes a switch
  -- free; every door takes the box back to its layer first, and the flush still
  -- posts one drift-locked call per file.
  , Glue "the keyword layers are a select over one box"
      [ "id=\"clayer\"", "<textarea id=\"ctext\" class=\"ctext\""
      , "crows = (b.layers || []).map(layerRow).sort(byLayer);"
      -- System first, then the tags in their own alphabet — the server's order
      -- is the walk's.
      , "const byLayer = (a, b) => (a.tag === null ? 0 : 1) - (b.tag === null ? 0 : 1)"
      , "|| String(a.tag).localeCompare(String(b.tag));"
      , "const takeLayer = () => { if (crows[cat]) crows[cat].text = el(\"ctext\").value; };"
      , "el(\"clayer\").addEventListener(\"change\""
      , "const cdirty = () => (takeLayer(), crows.some(cmoved));"
      , "const cmoved = (r) => r.text !== r.base"
      -- One POST per layer that moved, each awaited, each under its own digest.
      -- A layer with nothing to send drops the refusal it was carrying, since
      -- the edit that earned it has been taken back.
      , "if (!cmoved(r)) { r.err = \"\"; continue; }"
      , "body: JSON.stringify({ path: r.path, lines: sent.split(\"\\n\"),"
      -- A refusal brings its layer with it, since one box shows one file; a
      -- flush that refused nothing redraws what sits AROUND the box and leaves
      -- the box alone, since `C-x C-s' syncs mid-edit and a redraw there would
      -- paint over what is being typed.
      , "if (landed === -1) landed = crows.indexOf(r);"
      , "      if (landed === -1) showAround();"
      , "      else { takeLayer(); showLayer(landed); }"
      -- The label is redrawn too: a layer this sheet just CREATED has a digest
      -- now and must stop saying it is not there yet.
      , "+ (r.digest ? \"\" : \" · not created yet\") : \"\";" ]
      -- No box per layer, and no second copy of the text on an element.
      [ "createElement(\"textarea\")", "r.box.value", "r.note.textContent" ]

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
      , "applyView(b, DEFAULT_QUERY);"
      -- `g' is HOME, so it is not a step on the trail: the crumbs and the
      -- labels naming them go with it, where DEL walks back one rung at a time.
      , "if (crumbing()) table.setCrumbs([]);"
      , "crumbLabels = {};"
      , "remember(q);"
      , "remount();" ]
      -- `g' replaced the refresh key outright: one door through the mount.
      [ "function refresh()", "refreshing …", "org-glance-overview:refresh" ]

  -- The second canned view, applied through the same door and differing in one
  -- thing: it carries its own ORDER, which is a token of the query like any
  -- other rather than a call behind the answer.
  , Glue "`a' is the agenda query through the same door, its own sort included"
      [ "const AGENDA_QUERY = \"state:*active* -planned:*empty* sort:scheduled\";"
      , "applyAgenda: (b) => applyView(b, AGENDA_QUERY, (total) => landedAgenda(b, total)),"
      , "said(b, `agenda · ${rowsWord(total)}`);"
      -- The landing is an ARGUMENT of the boot it belongs to, so a boot that
      -- never lands cannot leave one behind for the next.
      , "function start(after) {"
      , "if (after) after(a.total);" ]
      -- A view rather than a mode: no state saying the agenda is on, no sort
      -- asked for behind the query that already states it, and no variable this
      -- arms and disarms by hand.
      [ "agendaMode", "let agenda =", "sortKeys", "let landed"
      , "sortRows", "table.sortBy(" ]

  -- `o' follows the row.  The extraction is the server's — the page holds no
  -- org parser — and how many links come back decides the whole gesture.
  , Glue "`o' follows the row's links, and the server is what finds them"
      [ "const linksOf = (id) => getJSON(`/links?id=${encodeURIComponent(id)}`);"
      , "if (!links.length) { said(b, \"no links\"); return; }"
      , "if (links.length === 1) { openLink(b, links[0]); return; }"
      , "showLinks(b, id, a);"
      , "window.open(link.target, \"_blank\", \"noopener\");"
      , "append(\"cmd\", \"info\", `link ${JSON.stringify(link.target)} opened`);"
      -- SEVERAL is a table-view MOUNT, and the followable set is the SERVER's
      -- list spliced in rather than a regex this page runs over the target a
      -- second time.  What the mount was given and what the foot says are the
      -- popup cases' business, which read them off behaviour.
      , "lmount = TableView.mount(el(\"ltable\"), { columns: LCOLS, rows: [] },"
      , "const followable = (l) => FOLLOWABLE.indexOf(l.type) !== -1;" ]
      -- No bracket grammar here: `[[T][D]]' is read where `displayText' is.  No
      -- which-key letters either: the popup replaced them, so nothing assigns
      -- one over a link and nothing narrows a link list.
      [ "\\\\[\\\\[", "linkAt("
      , "linkChoices", "a letter opens it", "c.target" ]

  -- And `RET' WRITES one: the link at point becomes two fields over its own
  -- cells, and the commit is `edit-link' over the SPAN the server handed out,
  -- pinned to the digest that answer carried.  The third surface on the shared
  -- overlay, so what is spelled here is the shape and the commit.
  , Glue "the link popup edits in place, over the range the server gave it"
      [ "box: \"ledit\", pane: \"lpane\", fields: [\"ltitle\", \"lurl\"], cells: [1, 2],"
      , "const lediting = () => !!edit && edit.o === LROW;"
      , "openEdit(LROW, at);"
      , "else if (k === \"RET\") commitLink(edit.row);"
      , "const args = { span: link.span, target };"
      -- ABSENT IS NOT NULL: only a description field the reader moved says
      -- anything, and one they emptied is the null that takes it off.
      , "if (typed !== link.desc) args.desc = typed || null;"
      , "fire(b, \"edit-link\", [id], args," ]
      -- No link SPELLED here — the shape is the server's, so this page sends a
      -- target and a description and never a rendered link — and no offsets of
      -- its own: the range came out of the answer and goes back as it came.  No
      -- re-read behind the commit either: `/command' never writes the store, so
      -- asking again here would answer with what the file said BEFORE the write.
      [ "arrives with the link span", "renderLink", "linksOf(lfor"
      , "link.span[0] +", "repaintLinks" ]

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
      , "swap ? asking(asked) : `${narrow}limit=${PAGE}`"
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

  -- `^' sorts by the column the cell selection is standing in, and the press is
  -- a QUERY EDIT: the renderer composes the chain, writes it into the applied
  -- query as `sort:' tokens and delivers it, so the press comes back through
  -- `onFilter' as an ordinary commit — URL, refetch and all.  This page names no
  -- order and remembers none.
  , Glue "`^' promotes the column at point to the chain's head"
      [ "toggleSort: (b) => {"
      -- The column comes out of the renderer's selection like every other key's.
      , "const at = column(), c = at === null ? null : cols[at];"
      , "if (!c) { said(b, \"no column selected — f/l to pick one\"); return; }"
      -- `sortPromote' is where `sortable' is enforced, so the refusal is read
      -- off the call — one gate — and the key SPEAKS it.
      , "if (!table.sortPromote(c.key)) { said(b, `${named} does not sort`); return; }"
      , "const chain = table.getSort() || [], head = chain[0];"
      -- An asset with no promotion says so rather than throwing.
      , "if (!sorts()) { said(b, \"this table-view.js has no sort\"); return; }" ]
      -- No sort record and no sort CALL survive: `sortAt' was the page's copy of
      -- what the handle publishes, and `sortBy' was how a canned view stated an
      -- order the query now carries.  The header marks stay the renderer's
      -- drawing.
      [ "sortAt", "tv-arrow", "sortRows", "table.sortBy(" ]

  -- `f → next-column (Headline)', and `f → next-column (row mode)' where the
  -- walk left the cells.  Walking off an end is a LANDING rather than a wall:
  -- the renderer reads a column index outside the table as no column at all, so
  -- the step is handed over out of range and comes back as the whole-row look.
  -- The column is read back out of `column()' — the renderer's answer decides,
  -- and `want' is only what was asked for.
  , Glue "the landing column is echoed by its header, or the row mode it left for"
      [ "const now = column();"
      , "said(b, now === null ? \"row mode\" : (cols[now].header || cols[now].key));"
      , "said(b, \"no row\")"
      -- The headers are the mounted view's, and parity cuts the keys out of the
      -- same list where it needs them.
      , "cols = view.columns || [];"
      , "const keys = cols.map((c) => c.key);" ]
      -- The clamp this page used to keep, and must not grow back: an edge test
      -- here would swallow the key at a wall the renderer does not have.
      [ "at first", "at last", "want >= cols.length" ]

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
      [ "#mtext,#pinput,#pedit input,#tedit input,#ledit input,"
      , ".ctext,.cview{font-size:16px}}", "font:12px/1.5 var(--dk-mono)"
      , "#mpanes{flex-direction:column}" ]

  -- THE SETTINGS SHEET IS UNREACHABLE ON A TOUCH DEVICE, and it is a KNOWN GAP
  -- rather than an oversight: the gear that opened it lived in the status
  -- corner, and the corner is gone.  Asserted from both sides so the gap cannot
  -- be half-closed by accident — no gear anywhere, and the comment that owns the
  -- question in the one media block.
  , Glue "the settings door a coarse pointer had went with the corner"
      -- The block itself is intact, so what is asserted is a missing DOOR
      -- rather than a missing block.
      [ "  @media (pointer:coarse){", "#app .tv-chips{min-height:44px;cursor:pointer}" ]
      [ "id=\"gear\"", "#gear{", "\9881" ]

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

  , testCase "and /tags waits for the same store" $ do
      application' <- indexingApp
      r <- getFrom application' "/tags?ids=sample.org%230"
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
        assertEqual "the empty meta is that one entry, asked for by name"
                    ["Jotted and never stated"] =<< titles "/headlines?q=state%3A*empty*"
        -- And the bare word is a keyword nobody declared, so it finds nothing.
        assertEqual "where the word it replaced is a value" []
          =<< titles "/headlines?q=state%3Anone"
        assertEqual "so negating the default view drops it too" ["Shipped"]
          =<< titles "/headlines?q=-state%3A*active*"

  , testCase "a filtered OR query pages out of the view's own sort" $ do
      a <- app assetsDir
      whole <- rowsOf =<< getFrom a "/headlines?q=state:*active*"
      one <- getFrom a "/headlines?q=state:*active*&limit=2&offset=0"
      two <- getFrom a "/headlines?q=state:*active*&limit=2&offset=2"
      -- Three keywords in the file's active set, plus the stateless row the
      -- group takes with them (TestFilter, "the stateless row is active").
      assertEqual "the union" 4 (length whole)
      assertEqual "the total is the match count, not the page" (Just "4")
                  (header "X-Glance-Total" one)
      let sorted = map rowId (sortOn sortKeyOf whole)
      assertEqual "page one" (take 2 sorted) . map rowId =<< rowsOf one
      assertEqual "page two" (drop 2 sorted) . map rowId =<< rowsOf two
      assertEqual "more follows page one" (Just "true") (header "X-Glance-Has-Next" one)
      assertEqual "nothing follows page two" (Just "false") (header "X-Glance-Has-Next" two)

  , testCase "limit cuts a page out of the view's own sort" $ do
      a <- app assetsDir
      whole <- rowsOf =<< getFrom a "/headlines"
      page <- rowsOf =<< getFrom a "/headlines?limit=3"
      assertEqual "page size" 3 (length page)
      -- The page is the first three of the chain the view declares, not the
      -- first three the walk found.
      assertEqual "the sort the view declares"
                  (take 3 (map rowId (sortOn sortKeyOf whole)))
                  (map rowId page)

  , testCase "offset walks the pages, and has-next says when to stop" $ do
      a <- app assetsDir
      whole <- map rowId . sortOn sortKeyOf <$> (rowsOf =<< getFrom a "/headlines")
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
      -- The whole fixture under a limit, since its first rows are in the same
      -- order either way and a shorter page cannot tell the two apart.
      byState <- map rowId <$> (rowsOf =<< getFrom a "/headlines?limit=6")
      doc <- map rowId <$> (rowsOf =<< getFrom a "/headlines?order=document&limit=6")
      assertEqual "the walk itself" walk doc
      -- Without this the case would pass over a fixture whose two orders agree.
      assertBool ("the fixture cannot tell them apart: " <> show byState)
                 (byState /= doc)

  , testCase "anything else under order is a 400 naming it" $ do
      a <- app assetsDir
      mapM_ (\path -> do
               r <- getFrom a path
               assertEqual (show path <> " status") 400 (status r)
               assertContains "names the parameter" "order" (body r))
            ["/headlines?order=walk", "/headlines?order=Document", "/headlines?order="]
  ]

-- | The ORDER a query states: @?q=@'s @sort:@ tokens, served AND declared.
--
-- The server sorts, which is what makes a limited answer's page one the right
-- rows: the client asks for a hundred and gets the first hundred of the order
-- it asked for.  And what the view declares is the EFFECTIVE chain — the
-- query's where it names one, the default where it does not — so the renderer
-- and the header ordinals describe the order the rows are actually in.
--
-- The grammar itself is @TestFilter@'s; what is asserted here is the answer.
sortQuerySpec :: TestTree
sortQuerySpec = testGroup "GET /headlines?q=sort:"
  [ testCase "no sort token leaves the default chain, and says nothing about it" $ do
      v <- get assetsDir "/headlines?q=state:*active*" >>= decoded
      assertEqual "the chain declared" [("state", True), ("title", True)
                                       , ("deadline", True), ("scheduled", True)]
        =<< chainDeclaredBy v

  , testCase "a sort token replaces it, and the view declares what it did" $ do
      v <- get assetsDir "/headlines?q=sort:deadline:desc" >>= decoded
      assertEqual "the chain declared" [("deadline", False)] =<< chainDeclaredBy v

  , testCase "and the rows come back in it" $ do
      a <- app assetsDir
      whole <- rowsOf =<< getFrom a "/headlines?q=sort:deadline&limit=6"
      -- Three of the six carry a deadline; the empty cells settle behind them,
      -- outside the direction, and keep walk order among themselves.
      assertEqual "earliest deadline first, the undated behind them"
        [ "ship-table-view", "test/fixtures/view/sample.org#2"
        , "test/fixtures/view/sample.org#1", "test/fixtures/view/sample.org#3"
        , "test/fixtures/view/sample.org#4", "test/fixtures/view/sample.org#5" ]
        (map rowId whole)
      down <- rowsOf =<< getFrom a "/headlines?q=sort:deadline:desc&limit=6"
      assertEqual "reversing the key leaves the empty cells where they were"
        [ "test/fixtures/view/sample.org#2", "ship-table-view"
        , "test/fixtures/view/sample.org#1", "test/fixtures/view/sample.org#3"
        , "test/fixtures/view/sample.org#4", "test/fixtures/view/sample.org#5" ]
        (map rowId down)

    -- The boot's own shape: a page-sized first answer has to be the first page
    -- of the order asked for, or the reader reads the wrong hundred rows.
  , testCase "page one of a limited answer is the first page of that order" $ do
      a <- app assetsDir
      whole <- map rowId <$> (rowsOf =<< getFrom a "/headlines?q=sort:title&limit=6")
      page <- getFrom a "/headlines?q=sort:title&limit=2"
      two <- getFrom a "/headlines?q=sort:title&limit=2&offset=2"
      assertEqual "page one" (take 2 whole) . map rowId =<< rowsOf page
      assertEqual "page two" (take 2 (drop 2 whole)) . map rowId =<< rowsOf two
      assertEqual "the total is the store's, not the page's" (Just "6")
                  (header "X-Glance-Total" page)

  , testCase "the token narrows nothing" $ do
      a <- app assetsDir
      plain <- length <$> (rowsOf =<< getFrom a "/headlines")
      sorted' <- length <$> (rowsOf =<< getFrom a "/headlines?q=sort:title")
      beside <- length <$> (rowsOf =<< getFrom a "/headlines?q=state:*active*")
      also <- length <$> (rowsOf =<< getFrom a "/headlines?q=state:*active* sort:title")
      assertEqual "alone" plain sorted'
      assertEqual "and beside a predicate" beside also

    -- ONE COLUMN, ONE DIRECTION.  A token that is not a chain key is the whole
    -- request's 400 naming it, where a renderer drops the key: the rows a
    -- refused query would have served are the rows it asked for in an order
    -- nobody can give.
  , testCase "a token that is no chain key is a 400 naming it" $ do
      a <- app assetsDir
      mapM_ (\(q, named) -> do
               r <- getFrom a ("/headlines?q=" <> q)
               assertEqual (show q <> " status") 400 (status r)
               assertContains "names the token" named (body r))
        [ ("-sort:title",              "-sort:title")
        , ("sort:title|state",         "sort:title|state")
        , ("sort:nosuchcolumn",        "nosuchcolumn")
        , ("sort:title:sideways",      "sort:title:sideways")
        , ("sort:title sort:title",    "title") ]

  , testCase "and a half-typed one is no refusal at all" $ do
      r <- get assetsDir "/headlines?q=sort:"
      assertEqual "status" 200 (status r)
      assertEqual "rows" 6 . length =<< rowsOf r

    -- @order=@ picks the BASE the query overrides, so the two compose rather
    -- than fighting: document order with a sort token is that token's order.
  , testCase "a sort token outranks order=document" $ do
      v <- get assetsDir "/headlines?order=document&q=sort:title" >>= decoded
      assertEqual "the chain declared" [("title", True)] =<< chainDeclaredBy v
  ]

-- | The chain VIEW declares, highest priority first — the @sort@ array, or none
-- where the view has no such field.
chainDeclaredBy :: Value -> IO [(T.Text, Bool)]
chainDeclaredBy view = do
  fields <- fieldsOf view
  if "sort" `notElem` fields then pure []
    else traverse orderKeyOf =<< listAt "sort" view

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

-- | @add-tag@ and @remove-tag@'s argument.  Flat rather than nullable: a tag
-- comes off through the other command rather than through a null.
tagArg :: T.Text -> Value
tagArg tag = object ["tag" .= tag]

-- | @rename-tag@'s argument, which names both ends.
renameArg :: T.Text -> T.Text -> Value
renameArg from to = object ["from" .= from, "to" .= to]

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

    -- Legality is per ROW's chain, whose nearest scope is its file, and a
    -- set-state some named row would refuse is refused whole: half a state
    -- change over a marked set is worse than none.
  , testCase "a keyword one named row's chain lacks refuses the request" $
      withCommandable $ \a _hub path other -> do
        before <- document path
        r <- postTo a "/command"
               (command "set-state" ["first", "third"] (keywordArg (Just "WAITING")))
        assertEqual "status" 400 (status r)
        assertContains "names the keyword" "WAITING" (body r)
        assertContains "and the row it does not fit" "third" (body r)
        assertEqual "the first file is untouched" before =<< document path
        assertEqual "and so is the second" elsewhereOrg =<< document other

    -- The union's death, from the write side.  `film''s cycle is RECOGNIZED in
    -- every file under this root — that is what keeps the word out of a title —
    -- and no scope an untagged row reaches declares it, so it is not a state
    -- that row may be put into.  It used to pass, on the file's recognized set.
  , testCase "another tag's keyword is refused on a row that does not reach it" $
      withLayeredTree $ \a -> do
        r <- postTo a "/command" (command "set-state" ["bare"] (keywordArg (Just "WATCHING")))
        assertEqual "status" 400 (status r)
        assertContains "names the keyword" "WATCHING" (body r)
        assertContains "and the row" "bare" (body r)

    -- Each row against ITS OWN chain, so a set spanning two tags is refused for
    -- the member the keyword does not fit — and the member it does fit takes it
    -- when asked on its own.  This is the cost of the palette merging several
    -- rows into one table, stated as what a reader sees.
  , testCase "a marked set spanning tags is refused for the row that cannot take it" $
      withLayeredTree $ \a -> do
        r <- postTo a "/command"
               (command "set-state" ["tagged", "filmed"] (keywordArg (Just "READING")))
        assertEqual "status" 400 (status r)
        assertContains "names the row it does not fit" "filmed" (body r)
        ok <- postTo a "/command" (command "set-state" ["tagged"] (keywordArg (Just "READING")))
        assertEqual "and the one it fits, alone" 200 (status ok)
        assertEqual "landed" [("tagged", True)] =<< outcomesOf ok

    -- The regression the tightening had to leave standing: every rung of the
    -- chain still writes.  A tree apiece, since two writes to one file drift in
    -- a suite that runs no watch.
  , testCase "each rung of the chain is settable on a row that reaches it" $
      mapM_ (\(rid, keyword) -> withLayeredTree $ \a -> do
               r <- postTo a "/command" (command "set-state" [rid] (keywordArg (Just keyword)))
               assertEqual (T.unpack (rid <> " -> " <> keyword)) 200 (status r)
               assertEqual "landed" [(rid, True)] =<< outcomesOf r)
            [ ("filed", "READING")   -- the file's own #+TODO:
            , ("tagged", "READING")  -- one of its tags' configs
            , ("bare", "STARTED")    -- system.org
            , ("bare", "TODO") ]     -- org's own cycle

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

-- | @add-tag@ and @remove-tag@: the pair the manage-tags palette commits.
--
-- The span math is @TestQuery@'s ("Commands"), which drives the two pure
-- functions; what belongs here is the route — the batching, the per-id answer,
-- and the refusals that are the request's shape rather than a row's state.
tagCommandSpec :: TestTree
tagCommandSpec = testGroup "POST /command add-tag and remove-tag"
  [ testCase "add-tag joins the run and moves no other byte" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command" (command "add-tag" ["first"] (tagArg "work"))
        assertEqual "status" 200 (status r)
        assertEqual "the row landed" [("first", True)] =<< outcomesOf r
        assertEqual "the file is the old one with one tag more"
                    (T.replace "* NEXT First :one:" "* NEXT First :one:work:" before)
          =<< document path
        onDisk <- digestOnDisk path
        assertEqual "the digest it reports is the file's" [onDisk] =<< digestsOf r

  , testCase "and opens a run on a row that had none" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command" (command "add-tag" ["second"] (tagArg "work"))
        assertEqual "status" 200 (status r)
        assertEqual "the run is the whole edit"
                    (T.replace "* Second" "* Second :work:" before) =<< document path

  , testCase "remove-tag cuts it, and the last one takes the run away" $
      withCommandable $ \a hub path _other -> do
        before <- document path
        _ <- postTo a "/command" (command "remove-tag" ["first"] (tagArg "one"))
        assertEqual "the run went with its last entry"
                    (T.replace "* NEXT First :one:" "* NEXT First" before)
          =<< document path
        -- The store has to catch up before a second command can measure a span
        -- in this file, exactly as it does for a live daemon.
        watchStep hub path
        _ <- postTo a "/command" (command "add-tag" ["first"] (tagArg "work"))
        watchStep hub path
        r <- postTo a "/command" (command "add-tag" ["first"] (tagArg "home"))
        assertEqual "status" 200 (status r)
        watchStep hub path
        assertContains "two entries now" "* NEXT First :work:home:" =<< document path
        _ <- postTo a "/command" (command "remove-tag" ["first"] (tagArg "work"))
        assertContains "and one after the cut" "* NEXT First :home:" =<< document path

    -- Both directions are idempotent, so a palette may commit the same letter
    -- twice without the second press meaning anything.
  , testCase "adding what is there and removing what is not both land, changing nothing" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        added <- postTo a "/command" (command "add-tag" ["first"] (tagArg "one"))
        assertEqual "the row landed" [("first", True)] =<< outcomesOf added
        gone <- postTo a "/command" (command "remove-tag" ["second"] (tagArg "work"))
        assertEqual "and so did the other" [("second", True)] =<< outcomesOf gone
        assertEqual "and the file says what it always said" before =<< document path

    -- Two rows of one file are ONE editFile, and the proof is that the second
    -- landed: a write per row would pin the second to the digest the first
    -- invalidated.
  , testCase "two rows of one file are one write, and both land" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command" (command "add-tag" ["first", "second"] (tagArg "work"))
        assertEqual "both rows" [("first", True), ("second", True)] =<< outcomesOf r
        assertEqual "both edits, in one file"
                    (T.replace "* Second" "* Second :work:"
                       (T.replace "* NEXT First :one:" "* NEXT First :one:work:" before))
          =<< document path
        onDisk <- digestOnDisk path
        assertEqual "one write, so one digest, and it is the file's" [onDisk]
                    . nub =<< digestsOf r

  , testCase "rows in two files are two writes, and each is its own" $
      withCommandable $ \a _hub path other -> do
        r <- postTo a "/command" (command "add-tag" ["first", "third"] (tagArg "work"))
        assertEqual "both rows" [("first", True), ("third", True)] =<< outcomesOf r
        assertEqual "two files, two digests" 2 . length . nub =<< digestsOf r
        assertContains "the tag joined the run" "* NEXT First :one:work:" =<< document path
        assertContains "and opened one" "* TODO Third :work:" =<< document other

    -- The normalize-up half the PALETTE decides: it sends an add to the rows
    -- LACKING the tag, so the route sees a set that is uniform in what it needs.
    -- Sending it the whole set is safe all the same, since the row that has it
    -- costs no edit.
  , testCase "a mixed set is levelled up whichever rows are named" $
      withCommandable $ \a _hub path other -> do
        r <- postTo a "/command" (command "add-tag" ["first", "second", "third"]
                                          (tagArg "one"))
        assertEqual "every row landed"
                    [("first", True), ("second", True), ("third", True)] =<< outcomesOf r
        here <- document path
        assertContains "the row that had it is untouched" "* NEXT First :one:" here
        assertContains "and the one that lacked it has it" "* Second :one:" here
        assertContains "across the file boundary too" "* TODO Third :one:" =<< document other

  , testCase "a tag no parser reads refuses the request, naming it" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command" (command "add-tag" ["first"] (tagArg "50%"))
        assertEqual "status" 400 (status r)
        assertContains "names what it turned down" "50%" (body r)
        assertEqual "and nothing was written" before =<< document path

  , testCase "an empty tag is refused the same way" $
      withCommandable $ \a _hub _path _other -> do
        r <- postTo a "/command" (command "remove-tag" ["first"] (tagArg ""))
        assertEqual "status" 400 (status r)

  , testCase "and a request with no tag at all says what one wants" $
      withCommandable $ \a _hub _path _other -> do
        r <- postTo a "/command" (command "add-tag" ["first"] (object []))
        assertEqual "status" 400 (status r)
        assertContains "asks for one" "tag" (body r)

  , testCase "an id no row carries is refused on its own" $
      withCommandable $ \a _hub path _other -> do
        r <- postTo a "/command" (command "add-tag" ["first", "nosuch"] (tagArg "work"))
        assertEqual "status" 200 (status r)
        assertEqual "one landed, one did not"
                    [("first", True), ("nosuch", False)] =<< outcomesOf r
        assertContains "and the row that is there moved" "* NEXT First :one:work:"
          =<< document path

    -- The route writes the FILE; the watch is what updates rows, so a tag is
    -- reachable through neither the cell nor the vocabulary until the file has
    -- been read again.  Then all three move together: the cell, the row's own
    -- search text, and the virtual filter key the store keeps beside its rows.
  , testCase "the row arrives over the watch, and the vocabulary with it" $
      withCommandable $ \a hub path _other -> do
        _ <- postTo a "/command" (command "add-tag" ["second"] (tagArg "work"))
        assertEqual "the store has not read the file yet" []
          . map rowId =<< rowsOf =<< getFrom a "/headlines?q=work%3A"
        watchStep hub path
        r <- getFrom a "/headlines?q=work%3A"
        assertEqual "the tag is a filter key now, and it reaches the row"
                    ["second"] . map rowId =<< rowsOf r
        assertEqual "the cell carries the run the file grew" [":work:"]
          =<< traverse (cellAt "tag") =<< rowsOf r
        assertEqual "and the row's search text moved with it" ["second"]
          . map rowId =<< rowsOf =<< getFrom a "/headlines?q=work"
  ]

-- | @rename-tag@: the command the tags popup's @RET@ commits.
--
-- The span math is @TestQuery@'s ("rename-tag"); what belongs here is the
-- route — the argument shape, the two walls it puts up, and that a rename over
-- several rows of one file is still ONE atomic write.
renameCommandSpec :: TestTree
renameCommandSpec = testGroup "POST /command rename-tag"
  [ testCase "replaces the entry where it stands, moving no other byte" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command" (command "rename-tag" ["first"] (renameArg "one" "two"))
        assertEqual "status" 200 (status r)
        assertEqual "the row landed" [("first", True)] =<< outcomesOf r
        assertEqual "the file is the old one with that entry renamed"
                    (T.replace "* NEXT First :one:" "* NEXT First :two:" before)
          =<< document path
        onDisk <- digestOnDisk path
        assertEqual "the digest it reports is the file's" [onDisk] =<< digestsOf r

    -- BOTH DIRECTIONS from one edit set: the entry is replaced rather than cut
    -- and re-appended, so a rename and its inverse put the file back byte for
    -- byte — the property a remove-then-add composition cannot have.
  , testCase "and renaming it back puts the file where it was" $
      withCommandable $ \a hub path _other -> do
        before <- document path
        _ <- postTo a "/command" (command "rename-tag" ["first"] (renameArg "one" "two"))
        watchStep hub path
        r <- postTo a "/command" (command "rename-tag" ["first"] (renameArg "two" "one"))
        assertEqual "status" 200 (status r)
        assertEqual "byte for byte" before =<< document path

    -- A row that does not carry the old name costs no edit, which is what makes
    -- the command safe to send over the whole set the popup was raised on.
  , testCase "a row that never carried it lands, changing nothing" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command" (command "rename-tag" ["second"] (renameArg "one" "two"))
        assertEqual "status" 200 (status r)
        assertEqual "the row landed" [("second", True)] =<< outcomesOf r
        assertEqual "and the file says what it always said" before =<< document path

    -- Two rows of one file are ONE editFile, and the proof is that the second
    -- landed: a write per row would pin the second to the digest the first
    -- invalidated.
  , testCase "two rows of one file are one write, and both land" $
      withCommandable $ \a hub path _other -> do
        _ <- postTo a "/command" (command "add-tag" ["first", "second"] (tagArg "work"))
        watchStep hub path
        r <- postTo a "/command" (command "rename-tag" ["first", "second"]
                                          (renameArg "work" "projects"))
        assertEqual "both rows" [("first", True), ("second", True)] =<< outcomesOf r
        here <- document path
        assertContains "the entry moved in place" "* NEXT First :one:projects:" here
        assertContains "and in the row that had only it" "* Second :projects:" here
        onDisk <- digestOnDisk path
        assertEqual "one write, so one digest, and it is the file's" [onDisk]
                    . nub =<< digestsOf r

  , testCase "rows in two files are two writes, and each is its own" $
      withCommandable $ \a hub path other -> do
        _ <- postTo a "/command" (command "add-tag" ["first", "third"] (tagArg "work"))
        watchStep hub path
        watchStep hub other
        r <- postTo a "/command" (command "rename-tag" ["first", "third"]
                                          (renameArg "work" "projects"))
        assertEqual "both rows" [("first", True), ("third", True)] =<< outcomesOf r
        assertEqual "two files, two digests" 2 . length . nub =<< digestsOf r
        assertContains "here" "* NEXT First :one:projects:" =<< document path
        assertContains "and there" "* TODO Third :projects:" =<< document other

    -- The charset wall is the request's, and it stands at BOTH ends: a string
    -- that is not a tag is not a tag for any row.
  , testCase "a name no parser reads refuses the request, naming it" $
      mapM_ (\(from, to, named) ->
               withCommandable $ \a _hub path _other -> do
                 before <- document path
                 r <- postTo a "/command"
                        (command "rename-tag" ["first"] (renameArg from to))
                 assertEqual (T.unpack named <> ": status") 400 (status r)
                 assertContains "names what it turned down" named (body r)
                 assertEqual "and nothing was written" before =<< document path)
            [("one", "50%", "50%"), ("50%", "one", "50%"), ("one", "", "")]

  , testCase "and a request naming only one end says what one wants" $
      mapM_ (\args -> withCommandable $ \a _hub _path _other -> do
               r <- postTo a "/command" (command "rename-tag" ["first"] args)
               assertEqual "status" 400 (status r)
               assertContains "asks for both ends" "from" (body r)
               assertContains "by name" "to" (body r))
            [ object ["from" .= ("one" :: T.Text)]
            , object ["to" .= ("two" :: T.Text)]
            , object [] ]

  , testCase "an id no row carries is refused on its own" $
      withCommandable $ \a _hub path _other -> do
        r <- postTo a "/command" (command "rename-tag" ["first", "nosuch"]
                                          (renameArg "one" "two"))
        assertEqual "status" 200 (status r)
        assertEqual "one landed, one did not"
                    [("first", True), ("nosuch", False)] =<< outcomesOf r
        assertContains "and the row that is there moved" "* NEXT First :two:"
          =<< document path

    -- The route writes the FILE; the watch is what updates rows, so the new
    -- name is a filter key only once the file has been read again.
  , testCase "the row arrives over the watch, under its new name" $
      withCommandable $ \a hub path _other -> do
        _ <- postTo a "/command" (command "rename-tag" ["first"] (renameArg "one" "two"))
        watchStep hub path
        assertEqual "the old name reaches nothing" []
          . map rowId =<< rowsOf =<< getFrom a "/headlines?q=one%3A"
        r <- getFrom a "/headlines?q=two%3A"
        assertEqual "and the new one reaches the row" ["first"] . map rowId =<< rowsOf r
        assertEqual "the cell carries the run the file holds" [":two:"]
          =<< traverse (cellAt "tag") =<< rowsOf r
  ]

-- | ROW's cell under KEY, empty where it has none.
cellAt :: T.Text -> Value -> IO T.Text
cellAt key row = do
  cells <- field "cells" row
  fromMaybe "" <$> maybeTextAt key cells

-- | @GET \/tags@: what the rows a tag command names are tagged with.
--
-- The reading rule is @TestQuery@'s; what belongs here is the route — the
-- shape, the order, the vocabulary beside it, and the two refusals it shares
-- with @\/keywords@.
tagsSpec :: TestTree
tagsSpec = testGroup "GET /tags"
  [ testCase "is a row's own tags, folded, in the order the file spells them" $
      withTaggedTree $ \a -> do
        r <- getFrom a "/tags?ids=both"
        assertEqual "status" 200 (status r)
        assertEqual "the run as it stands, lowercased" [("both", ["web", "work"])]
          =<< tagRowsOf r
        assertEqual "and nothing was asked for that is not there" []
          =<< textsAt "unknown" =<< decoded r

  , testCase "several rows answer in the order they were named" $
      withTaggedTree $ \a -> do
        assertEqual "as asked" [("bare", []), ("both", ["web", "work"])]
          =<< tagRowsOf =<< getFrom a "/tags?ids=bare,both"
        assertEqual "and the other way round"
                    [("both", ["web", "work"]), ("bare", [])]
          =<< tagRowsOf =<< getFrom a "/tags?ids=both,bare"

    -- The whole store's, not the named rows': a completing read has to reach a
    -- tag none of the targets carries, and the rows a page holds are a fraction
    -- of the tree.
  , testCase "the vocabulary is the tree's, whichever row was asked about" $
      withTaggedTree $ \a ->
        assertEqual "every tag in the store, sorted" ["archive", "shelf", "web", "work"]
          =<< textsAt "vocabulary" =<< decoded =<< getFrom a "/tags?ids=bare"

    -- The COUNTS are the tree's rows per tag, which is what the popup's third
    -- column shows.  Rows rather than files, and rows rather than the named
    -- set: the store's own `stTags' counts FILES, so this is a different
    -- question and no arithmetic recovers it.  `web' is on two rows of one
    -- file, `archive' on one row of another, and the fold is the presence
    -- rule's — `:Web:' counts as `web'.
  , testCase "the counts are the tree's rows per tag, folded" $
      withTaggedTree $ \a -> do
        counts <- field "counts" =<< decoded =<< getFrom a "/tags?ids=bare"
        assertEqual "one entry per tag the store holds"
                    ["archive", "shelf", "web", "work"] =<< countedTags counts
        assertEqual "web is on two rows of one file, however it is spelled" 2
          =<< intAt "web" counts
        assertEqual "work on one of them" 1 =<< intAt "work" counts
        assertEqual "shelf on one row of the other file" 1 =<< intAt "shelf" counts
        assertEqual "and the archive tag counts like any other" 1
          =<< intAt "archive" counts

  , testCase "an id the store does not hold is named and left out" $
      withTaggedTree $ \a -> do
        r <- getFrom a "/tags?ids=nosuch,both"
        assertEqual "status" 200 (status r)
        assertEqual "the ones that are gone" ["nosuch"] =<< textsAt "unknown" =<< decoded r
        assertEqual "resolved for the one that is not" [("both", ["web", "work"])]
          =<< tagRowsOf r

  , testCase "ids repeat, ids comma-separate, id is one, and none is a 400" $
      withTaggedTree $ \a -> do
        let both = [("bare", []), ("both", ["web", "work"])]
        assertEqual "repeated" both =<< tagRowsOf =<< getFrom a "/tags?ids=bare&ids=both"
        assertEqual "and mixed with the singular" both
          =<< tagRowsOf =<< getFrom a "/tags?ids=bare&id=both"
        r <- getFrom a "/tags"
        assertEqual "status" 400 (status r)
        assertEqual "naming the parameter" "GET /tags?ids=<row id>,<row id>"
          =<< textAt "error" =<< decoded r

  , testCase "and it is a read: POST is a 405" $ do
      r <- withTaggedTree (\a -> postTo a "/tags" "{}")
      assertEqual "status" 405 (status r)
  ]

-- | Each row the answer names, with the tags it carries.
tagRowsOf :: SResponse -> IO [(T.Text, [T.Text])]
tagRowsOf = traverse one <=< rowsOf
  where one v = (,) <$> textAt "id" v <*> textsAt "tags" v

-- | The tags a counts object names, sorted: JSON object order is nobody's
-- contract, and what each of them counts is read with 'intAt'.
countedTags :: Value -> IO [T.Text]
countedTags = fmap sort . fieldsOf

-- | A tree whose rows disagree about their tags, and which holds one tag no row
-- the palette would resolve for carries — so the vocabulary being the STORE's
-- rather than the answer's is observable.
withTaggedTree :: (Application -> IO a) -> IO a
withTaggedTree k = withTempDir $ \dir -> do
  _ <- orgFile dir "a.org" (T.unlines
         [ "* one :Web:work:", ":PROPERTIES:", ":ORG_GLANCE_ID: both", ":END:"
         , "* two :web:", ":PROPERTIES:", ":ORG_GLANCE_ID: one", ":END:"
         , "* three", ":PROPERTIES:", ":ORG_GLANCE_ID: bare", ":END:" ])
  _ <- orgFile dir "b.org" (T.unlines
         [ "* four :shelf:ARCHIVE:", ":PROPERTIES:", ":ORG_GLANCE_ID: shelved", ":END:" ])
  (a, _hub) <- serverOver dir
  k a

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
-- keyword under the WIDEST source that declares it and nowhere below it — plus
-- how several rows merge and what the route refuses.
keywordsSpec :: TestTree
keywordsSpec = testGroup "GET /keywords"
  [ testCase "the default pair leads and every source below it loses those words" $
      withLayeredTree $ \a -> do
        r <- getFrom a "/keywords?ids=filed"
        assertEqual "status" 200 (status r)
        -- READING is the file's, book's AND pile's; book is the widest of the
        -- three to declare it, which leaves pile and the file with nothing and
        -- so no rows.  READ is the system layer's and book's, and stays with the
        -- WIDER of the two.  The chain ENDS at the file: `film''s cycle is
        -- recognized here, since recognition is a superset, and no scope this
        -- row reaches claims it — so it is neither shown nor settable.
        assertEqual "org's own, then the system layer, then book"
          [ ("default", ["TODO"],      ["DONE"])
          , ("system",  ["STARTED"],   ["READ"])
          , ("book",    ["READING"],   []) ] =<< sourcesOf r
        assertEqual "and nothing was asked for that is not there" [] =<< textsAt "unknown"
          =<< decoded r

    -- The reorder's display consequence, pinned: `filed' declares READING in its
    -- own `#+TODO:' and `tagged' does not, and the two now answer ALIKE — the
    -- word belongs to `book', the widest scope that names it, either way.  Under
    -- the old chain the file's own line pulled it into a `file' row.
  , testCase "a file redeclaring a wider scope's word gets no row of its own" $
      withLayeredTree $ \a -> do
        filed <- sourcesOf =<< getFrom a "/keywords?ids=filed"
        assertEqual "the row whose file declares nothing answers the same" filed
          =<< sourcesOf =<< getFrom a "/keywords?ids=tagged"
        assertEqual "and no source is named for the file at all" []
          [ src | (src, _a, _i) <- filed, src == "file" ]

  , testCase "the first tag that declares a keyword is the one that keeps it" $
      withLayeredTree $ \a -> do
        -- The same two tags with no file pragma over them: book is named first
        -- on the headline, so READING is book's and pile drops out entirely.
        assertEqual "book, and no pile row at all"
          [ ("default", ["TODO"],     ["DONE"])
          , ("system",  ["STARTED"],  ["READ"])
          , ("book",    ["READING"],  []) ]
          =<< sourcesOf =<< getFrom a "/keywords?ids=tagged"

  , testCase "a row no scope speaks for is offered org's own and the system layer" $
      withLayeredTree $ \a ->
        -- Untagged, in a file that declares nothing: the tags' cycles parse
        -- here and no scope this row reaches names one, so the palette stops
        -- where the chain does and neither READING nor WATCHING is on offer.
        assertEqual "org's own and the system layer, and nothing under them"
          [ ("default", ["TODO"],    ["DONE"])
          , ("system",  ["STARTED"], ["READ"]) ]
          =<< sourcesOf =<< getFrom a "/keywords?ids=bare"

    -- The marked set: one answer over every row it holds, and a tag any of them
    -- carries is a source of its own.
  , testCase "two rows under different tags bring both tag sources" $
      withLayeredTree $ \a ->
        assertEqual "book from one, film from the other"
          [ ("default", ["TODO"],      ["DONE"])
          , ("system",  ["STARTED"],   ["READ"])
          , ("book",    ["READING"],   [])
          , ("film",    ["WATCHING"],  ["WATCHED"]) ]
          =<< sourcesOf =<< getFrom a "/keywords?ids=tagged,filmed"

    -- The merge's one cost, stated: WATCHED is `film''s alone and READ is the
    -- system layer's, so a set spanning the two rows shows each word under the
    -- WIDEST source any member reaches it by.  The table describes the set
    -- rather than any one member of it.
  , testCase "a keyword wider in one row than another lands in the wider source" $
      withLayeredTree $ \a ->
        assertEqual "one answer over both rows, widest source first"
          [ ("default", ["TODO"],     ["DONE"])
          , ("system",  ["STARTED"],  ["READ"])
          , ("book",    ["READING"],  []) ]
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
          [ ("default", ["TODO"],     ["DONE"])
          , ("system",  ["STARTED"],  ["READ"])
          , ("book",    ["READING"],  []) ] =<< sourcesOf r

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
        let both = [ ("default", ["TODO"],     ["DONE"])
                   , ("system",  ["STARTED"],  ["READ"])
                   , ("book",    ["READING"],  [])
                   , ("film",    ["WATCHING"], ["WATCHED"]) ]
        assertEqual "repeated" both
          =<< sourcesOf =<< getFrom a "/keywords?ids=tagged&ids=filmed"
        assertEqual "and mixed with the comma form" both
          =<< sourcesOf =<< getFrom a "/keywords?ids=tagged&id=filmed"
        assertEqual "the singular spelling answers for one"
          [ ("default", ["TODO"],     ["DONE"])
          , ("system",  ["STARTED"],  ["READ"])
          , ("book",    ["READING"],  []) ]
          =<< sourcesOf =<< getFrom a "/keywords?id=tagged"
        r <- getFrom a "/keywords"
        assertEqual "status" 400 (status r)
        assertEqual "naming the parameter" "GET /keywords?ids=<row id>,<row id>"
          =<< textAt "error" =<< decoded r

  , testCase "and it is a read: POST is a 405" $ do
      r <- withLayeredTree (\a -> postTo a "/keywords" "{}")
      assertEqual "status" 405 (status r)

    -- A tree may configure a tag called `system', and the three reserved names
    -- are not taken out of the tag namespace to stop it.  The entries stay
    -- apart — a tag keeps its tag RANK, so it sits BELOW the system layer the
    -- way any other tag does — and the precedence order is what tells the two
    -- rows named alike apart.
  , testCase "a tag spelled like a reserved source keeps its own rank" $
      withTempDir $ \dir -> do
        writeLayers dir [ (Nothing,       "#+TODO: STARTED | SHELVED\n")
                        , (Just "system", "#+TODO: PLANNED | SHELVED\n") ]
        _ <- orgFile dir "a.org" (T.unlines
               [ "* one :system:", ":PROPERTIES:", ":ORG_GLANCE_ID: only", ":END:" ])
        (a, _hub) <- serverOver dir
        assertEqual "org's own, then the layer keeping SHELVED, then the tag it shadows"
          [ ("default", ["TODO"],    ["DONE"])
          , ("system",  ["STARTED"], ["SHELVED"])
          , ("system",  ["PLANNED"], []) ]
          =<< sourcesOf =<< getFrom a "/keywords?ids=only"
  ]

-- | @POST \/command edit-link@: the link write the popup's @RET@ commits.
--
-- The span math and the form table are @TestQuery@'s ("edit-link"); what
-- belongs here is the ROUND TRIP a client makes — the range comes out of
-- @GET \/links@ and goes back into @POST \/command@ — and the refusals that are
-- the route's rather than the math's.
editLinkSpec :: TestTree
editLinkSpec = testGroup "POST /command edit-link"
  [ testCase "the range /links handed out is the range the write splices" $
      withLinkable $ \a _hub path -> do
        before <- document path
        (sp, digest) <- pinnedSpan a "first" 0
        r <- postTo a "/command"
               (linkCommand "edit-link" ["first"]
                       (object ["span" .= sp, "target" .= ("https://z.example" :: T.Text)])
                       [("first", digest)])
        assertEqual "status" 200 (status r)
        assertEqual "the row landed" [("first", True)] =<< outcomesOf r
        assertEqual "the file is the old one with one target replaced"
          (T.replace "[[https://a.example][A]]" "[[https://z.example][A]]" before)
          =<< document path
        onDisk <- digestOnDisk path
        assertEqual "the digest it reports is the file's" [onDisk] =<< digestsOf r

    -- The three forms over one row, each through the range the route itself
    -- reported: a bracketed link keeps its description, a description ARRIVING
    -- brackets a plain URL, and a null takes one off.  The table is
    -- @TestQuery@'s; what this pins is that the offsets survive the wire.
  , testCase "a description added, kept and taken off, over the wire" $ do
      withLinkable $ \a _hub path -> do
        before <- document path
        (sp, digest) <- pinnedSpan a "first" 1
        _ <- postTo a "/command"
               (linkCommand "edit-link" ["first"]
                       (object [ "span" .= sp
                               , "target" .= ("https://b.example" :: T.Text)
                               , "desc" .= ("B" :: T.Text) ])
                       [("first", digest)])
        assertEqual "the bracketed bare link took a description"
          (T.replace "[[https://b.example]]" "[[https://b.example][B]]" before)
          =<< document path
      withLinkable $ \a _hub path -> do
        before <- document path
        (sp, digest) <- pinnedSpan a "first" 2
        _ <- postTo a "/command"
               (linkCommand "edit-link" ["first"]
                       (object [ "span" .= sp
                               , "target" .= ("https://d.example" :: T.Text) ])
                       [("first", digest)])
        assertEqual "and the plain URL swapped its target and stayed plain"
          (T.replace "https://c.example" "https://d.example" before) =<< document path
      withLinkable $ \a _hub path -> do
        before <- document path
        (sp, digest) <- pinnedSpan a "first" 0
        _ <- postTo a "/command"
               (linkCommand "edit-link" ["first"]
                       (object [ "span" .= sp
                               , "target" .= ("https://a.example" :: T.Text)
                               , "desc" .= Null ])
                       [("first", digest)])
        assertEqual "a null description leaves a desc-less bracketed link"
          (T.replace "[[https://a.example][A]]" "[[https://a.example]]" before)
          =<< document path

    -- THE PIN.  The spans describe the text the store last read, so a client
    -- holding a digest the store no longer has is refused — per id, since a
    -- digest is per file — rather than splicing a range that has moved.
  , testCase "a span measured against a text the store no longer holds is refused" $
      withLinkable $ \a _hub path -> do
        (sp, _digest) <- pinnedSpan a "first" 0
        before <- document path
        r <- postTo a "/command"
               (linkCommand "edit-link" ["first"]
                       (object ["span" .= sp, "target" .= ("https://z.example" :: T.Text)])
                       [("first", "0000")])
        assertEqual "status" 200 (status r)
        assertEqual "the row did not land" [("first", False)] =<< outcomesOf r
        assertEqual "and nothing was written" before =<< document path

    -- The subtree wall's interesting half: a span that IS in the file, and IS a
    -- link, and belongs to ANOTHER ROW.  The digest is per file, so nothing but
    -- this wall stands between one row's write and a link no reader of that row
    -- was ever shown.
  , testCase "a link belonging to another row of the same file is refused" $
      withLinkable $ \a _hub path -> do
        before <- document path
        (sp, digest) <- pinnedSpan a "second" 0
        r <- postTo a "/command"
               (linkCommand "edit-link" ["first"]
                       (object ["span" .= sp, "target" .= ("https://z.example" :: T.Text)])
                       [("first", digest)])
        assertEqual "status" 400 (status r)
        assertContains "naming the row it does not belong to" "first" (body r)
        assertContains "and the extent that does not hold it" "subtree" (body r)
        assertEqual "nothing was written" before =<< document path

    -- A SPAN NAMES ONE ROW's own text, so the command names one row.  Over two it
    -- would mean a different range in each file, and in one of them very likely
    -- a link the reader never saw.
  , testCase "two ids are refused, since a span names one row's own text" $
      withLinkable $ \a _hub path -> do
        before <- document path
        (sp, digest) <- pinnedSpan a "first" 0
        r <- postTo a "/command"
               (linkCommand "edit-link" ["first", "second"]
                       (object ["span" .= sp, "target" .= ("https://z.example" :: T.Text)])
                       [("first", digest)])
        assertEqual "status" 400 (status r)
        assertContains "naming the command" "edit-link" (body r)
        assertContains "and the rule" "one row" (body r)
        assertEqual "nothing was written" before =<< document path

    -- The refusals, all 400 with the file untouched: a link that points nowhere,
    -- a padded target, a missing range, a range the row does not hold, a range
    -- that is not a link, and a target that would not read back as the link it
    -- claims to be.
  , testCase "every refusal is a 400, and each names what it turned down" $
      mapM_ (\(what, args, named) ->
               withLinkable $ \a _hub path -> do
                 before <- document path
                 (sp, digest) <- pinnedSpan a "first" 0
                 r <- postTo a "/command"
                        (linkCommand "edit-link" ["first"] (args sp) [("first", digest)])
                 assertEqual (what <> ": status") 400 (status r)
                 assertContains what named (body r)
                 assertEqual (what <> ": nothing written") before =<< document path)
        [ ( "an empty target"
          , \sp -> object ["span" .= sp, "target" .= ("" :: T.Text)]
          , "points somewhere" )
        , ( "no span at all"
          , const (object ["target" .= ("https://z.example" :: T.Text)])
          , "span" )
        , ( "a span outside the row"
          , const (object [ "span" .= [9000 :: Int, 9100]
                          , "target" .= ("https://z.example" :: T.Text) ])
          , "subtree" )
        , ( "a span over prose"
          , const (object [ "span" .= [0 :: Int, 4]
                          , "target" .= ("https://z.example" :: T.Text) ])
          , "does not read as one link" )
        , ( "a target that would not read back as a link"
          , \sp -> object ["span" .= sp, "target" .= ("https://a]b" :: T.Text)]
          , "does not read as one link" )
        , ( "a padded target"
          , \sp -> object ["span" .= sp, "target" .= (" https://z.example " :: T.Text)]
          , "leading or trailing space" )
        , ( "a newline in the target"
          , \sp -> object ["span" .= sp, "target" .= ("https://z\n* B" :: T.Text)]
          , "one line" ) ]

    -- The route writes the FILE; the watch is what updates rows.  A link in the
    -- TITLE is a cell, so the edit reaches the table the way every other write
    -- does — and one in the body moves no cell at all, which is the store's own
    -- rule and why the popup re-asks rather than expecting a frame.
  , testCase "a title link reaches the row over the watch" $
      withLinkable $ \a hub path -> do
        (sp, digest) <- pinnedSpan a "first" 0
        _ <- postTo a "/command"
               (linkCommand "edit-link" ["first"]
                       (object [ "span" .= sp
                               , "target" .= ("https://a.example" :: T.Text)
                               , "desc" .= ("Alpha" :: T.Text) ])
                       [("first", digest)])
        watchStep hub path
        r <- getFrom a "/headlines"
        assertEqual "the cell carries the line the file holds"
          ["one [[https://a.example][Alpha]]", "two [[https://e.example][E]]"]
          =<< traverse (cellAt "title") =<< rowsOf r

    -- And the links themselves are re-read: the popup asks again and gets the
    -- new target, its span moved by what the edit cost.
  , testCase "and /links answers with the edited link once the watch has run" $
      withLinkable $ \a hub path -> do
        (sp, digest) <- pinnedSpan a "first" 0
        _ <- postTo a "/command"
               (linkCommand "edit-link" ["first"]
                       (object ["span" .= sp, "target" .= ("https://z.example" :: T.Text)])
                       [("first", digest)])
        watchStep hub path
        assertEqual "the new target, described as it always was"
          [ ["https://z.example", "A", "https"]
          , ["https://b.example", "https://b.example", "https"]
          , ["https://c.example", "https://c.example", "https"] ]
          =<< linksOf =<< getFrom a "/links?id=first"
        text <- document path
        assertEqual "and the spans still cut their own links out of the file"
          [ "[[https://z.example][A]]", "[[https://b.example]]", "https://c.example" ]
          . map (charSpan text) =<< spansOf =<< getFrom a "/links?id=first"
  ]

-- | A row pointing three ways — a described bracket link on the title, a
-- desc-less one in the body and a plain URL beside it — so the form table has
-- one of each to be right about, and a second row with a link of its OWN: the
-- ids rule needs a second row, and the subtree wall needs a span that IS in this
-- file and IS a link and belongs to somebody else.
linkable :: T.Text
linkable = T.unlines
  [ "* one [[https://a.example][A]]"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: first"
  , ":END:"
  , "body [[https://b.example]] and https://c.example here"
  , "* two [[https://e.example][E]]"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: second"
  , ":END:"
  , "nothing else to follow"
  ]

-- | Run K over a server holding 'linkable': the app, the hub whose store it
-- answers from — the watch cases step it the way a live daemon does — and the
-- file.
withLinkable :: (Application -> Hub -> FilePath -> Assertion) -> Assertion
withLinkable k = withTempDir $ \dir -> do
  path <- orgFile dir "notes.org" linkable
  (a, hub) <- serverOver dir
  k a hub path

-- | The span A reports for ROW's link at AT, and the digest that answer carried
-- — which is exactly what the popup holds and sends back.
pinnedSpan :: Application -> ByteString -> Int -> IO (Value, T.Text)
pinnedSpan a rid at = do
  answer <- decoded =<< getFrom a ("/links?id=" <> rid)
  links <- listAt "links" answer
  sp <- field "span" (links !! at)
  (,) sp <$> textAt "digest" answer

-- | A command as the LINK POPUP sends one: 'command' under the digests it was
-- measured against, which the commands naming a property of a row send none of.
linkCommand :: T.Text -> [T.Text] -> Value -> [(T.Text, T.Text)] -> BL.ByteString
linkCommand name ids args digests = encode (object
  [ "name" .= name, "ids" .= ids, "args" .= args
  , "digests" .= object [ Key.fromText rid .= digest | (rid, digest) <- digests ] ])

-- | @GET \/links@: where one row points.
--
-- The extraction rule is @TestQuery@'s ("Links"), which drives the pure
-- function; what belongs here is the route — the id it takes, the shape it
-- answers in, and the two refusals it shares with materialize.
linksSpec :: TestTree
linksSpec = testGroup "GET /links"
  [ testCase "is the row's links, in the order its subtree writes them" $
      withLinkTree $ \a _dir -> do
        r <- getFrom a "/links?id=linked"
        assertEqual "status" 200 (status r)
        assertEqual "target, description and type"
          [ ["https://x.example/a", "the first", "https"]
          , ["https://y.example/b", "https://y.example/b", "https"]
          , ["https://z.example/c", "https://z.example/c", "https"] ]
          =<< linksOf r

    -- The type is the SERVER's word for the target, which is what the popup's
    -- badge column draws and what `o' reads to decide whether a tab can be
    -- pointed anywhere.  The derivation is `TestQuery''s ("Links"); what belongs
    -- here is that the route carries it, over targets no tab can follow.
  , testCase "every link carries its type, followable or not" $
      withLinkTree $ \a _dir ->
        assertEqual "one word per link"
          [ ["mailto:t@example.org", "write", "mailto"]
          , ["org-glance-visit:E1B2", "the other row", "glance"]
          , ["file:notes.org", "notes", "file"]
          , ["Some Headline", "Some Headline", "other"] ]
          =<< linksOf =<< getFrom a "/links?id=typed"

  , testCase "an id the store has no row for is a 404, like materialize" $
      withLinkTree $ \a _dir -> do
        r <- getFrom a "/links?id=nosuch"
        assertEqual "status" 404 (status r)
        assertContains "hint" "no headline with id" (body r)

  , testCase "no id at all says what the route wants" $
      withLinkTree $ \a _dir -> do
        r <- getFrom a "/links"
        assertEqual "status" 400 (status r)
        assertEqual "naming the parameter" "GET /links?id=<row id>"
          =<< textAt "error" =<< decoded r

  , testCase "a row with nothing to follow answers with an empty list" $
      withLinkTree $ \a _dir ->
        assertEqual "no links" [] =<< linksOf =<< getFrom a "/links?id=bare"

    -- EVERY LINK CARRIES ITS SPAN, and the span is into the FILE: it is what
    -- makes the answer writeable, since `edit-link' takes that range back and
    -- splices it.  Asserted by cutting each range out of the file on disk, which
    -- is the claim a span makes and the one a client acts on.
  , testCase "every link carries the file range that spells it" $
      withLinkTree $ \a dir -> do
        r <- getFrom a "/links?id=linked"
        text <- document (dir </> "a.org")
        assertEqual "each range cuts its own link out of the file"
          [ "[[https://x.example/a][the first]]", "https://y.example/b"
          , "https://z.example/c" ]
          . map (charSpan text) =<< spansOf r

    -- And the answer carries the file's DIGEST, which is the lock an edit is
    -- pinned to: the spans describe the text the store last read, so a client
    -- that sends it back is refused rather than spliced blind once the file has
    -- moved.
  , testCase "and the digest those spans were measured against" $
      withLinkTree $ \a dir -> do
        r <- getFrom a "/links?id=linked"
        onDisk <- digestOnDisk (dir </> "a.org")
        assertEqual "the file's own" onDisk =<< textAt "digest" =<< decoded r

  , testCase "and it is a read: POST is a 405" $ do
      r <- withLinkTree (\a _dir -> postTo a "/links?id=linked" "{}")
      assertEqual "status" 405 (status r)
  ]

-- | The answer's links as @[target, desc]@ pairs.
linksOf :: SResponse -> IO [[T.Text]]
linksOf r = traverse one =<< listAt "links" =<< decoded r
  where one v = sequence [textAt "target" v, textAt "desc" v, textAt "type" v]

-- | The half-open char range each of R's links reports.
spansOf :: SResponse -> IO [(Int, Int)]
spansOf r = traverse one =<< listAt "links" =<< decoded r
  where one v = listAt "span" v >>= pair
        pair [Number from, Number to] = pure (round from, round to)
        pair other = assertFailure ("expected a [start, end] span, got " <> show other)

-- | TEXT's half-open char range, which is what a span claims to be.
charSpan :: T.Text -> (Int, Int) -> T.Text
charSpan text (from, to) = T.take (to - from) (T.drop from text)

-- | A tree with one row worth following, one holding a link of every type the
-- popup draws a badge for, and one with nothing in it.  The first has a bracket
-- link on the title, a bare URL in the body and one more under a child, so the
-- route's answer shows it read the SUBTREE.
withLinkTree :: (Application -> FilePath -> IO a) -> IO a
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
         , "nothing to follow here"
         , "* three"
         , ":PROPERTIES:"
         , ":ORG_GLANCE_ID: typed"
         , ":END:"
         , "[[mailto:t@example.org][write]] [[org-glance-visit:E1B2][the other row]]"
         , "[[file:notes.org][notes]] [[Some Headline]]" ])
  (a, _hub) <- serverOver dir
  k a dir

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
        assertEqual "the rows that are left" ["near", "plain", "shipped"] . sort . map rowId
          =<< rowsOf r
        assertEqual "X-Glance-Total" (Just "3") (header "X-Glance-Total" r)
        assertEqual "X-Glance-Archived" (Just "1") (header "X-Glance-Archived" r)

  , testCase "the exclusion is exactly what -tag:*archive* spells" $
      withArchived $ \a -> do
        implicit <- rowsOf =<< getFrom a "/headlines"
        explicit <- getFrom a "/headlines?q=-tag%3A*archive*"
        assertEqual "the same rows" (map rowId implicit) . map rowId =<< rowsOf explicit
        -- A query that says it itself is not one this server also says: the
        -- count is zero because nothing was withheld from it.
        assertEqual "nothing hidden from it" (Just "0")
                    (header "X-Glance-Archived" explicit)

  , testCase "naming the META at all shows them" $
      withArchived $ \a ->
        mapM_ (\(path, wanted) -> do
                 r <- getFrom a path
                 assertEqual (show path <> ": the rows") wanted . sort . map rowId
                   =<< rowsOf r
                 assertEqual (show path <> ": nothing hidden") (Just "0")
                             (header "X-Glance-Archived" r))
              [ ("/headlines?q=tag%3A*archive*", ["filed"])
              , ("/headlines?q=tag%3A*archive*%20filed", ["filed"])
              , ("/headlines?q=state%3ADONE%20tag%3A*archive*", ["filed"]) ]

    -- THE COUPLING IS THE META'S ALONE.  The bare word is an ordinary tag
    -- predicate: it filters, it reveals nothing, and the count says a row was
    -- withheld from it — which is what a tree using `archive' for something of
    -- its own needs, and what makes the two spellings tell apart.  `near'
    -- carries `:archived:', which the column matches by substring.
  , testCase "the plain tag predicate filters without lifting the exclusion" $
      withArchived $ \a -> do
        plain <- getFrom a "/headlines?q=tag%3Aarchive"
        assertEqual "the rows it reaches" ["near"] . map rowId =<< rowsOf plain
        assertEqual "and the archived one it does not" (Just "1")
                    (header "X-Glance-Archived" plain)
        -- Which the meta reaches, over the same word, in the same column.
        meta <- getFrom a "/headlines?q=tag%3A*archive*"
        assertEqual "the meta is the whole tag" ["filed"] . map rowId =<< rowsOf meta

    -- The vocabulary is the WHOLE store's, which is what makes the predicate
    -- reach what the default hides.  A spelling no row carries as text is the
    -- proof: as free text `tag:*archive*' matches nothing, so a match is the
    -- predicate reading the tags cell.
  , testCase "the predicate survives the exclusion that hides its rows" $
      withArchived $ \a -> do
        faceted <- getFrom a "/headlines?q=tag%3A*archive*"
        text' <- getFrom a "/headlines?q=%22tag%3A*archive*%22"
        assertEqual "as a predicate" (Just "1") (header "X-Glance-Total" faceted)
        assertEqual "as free text" (Just "0") (header "X-Glance-Total" text')

  , testCase "a tree with nothing archived says so" $ do
      r <- get assetsDir "/headlines"
      assertEqual "X-Glance-Archived" (Just "0") (header "X-Glance-Archived" r)
      assertEqual "and every row is served" (Just "6") (header "X-Glance-Total" r)

    -- And naming the meta there costs nothing: with the tag nowhere in the
    -- tree there is no exclusion to lift, so the query is the ordinary tags
    -- predicate it spells and answers with the rows carrying that whole tag,
    -- which is none.  The vocabulary is the exclusion's own half of the
    -- question ('Glance.Web.Filter.namesArchive' asks the query's).
  , testCase "and naming the meta against it lifts nothing" $ do
      r <- get assetsDir "/headlines?q=tag%3A*archive*"
      assertEqual "no row carries the tag" (Just "0") (header "X-Glance-Total" r)
      assertEqual "and none was withheld" (Just "0") (header "X-Glance-Archived" r)

  , testCase "the exclusion runs before the page, like the filter" $
      withArchived $ \a -> do
        r <- getFrom a "/headlines?limit=1"
        assertEqual "the total is what is left after it" (Just "3")
                    (header "X-Glance-Total" r)
        assertEqual "the page" 1 . length =<< rowsOf r
        assertEqual "and more follows" (Just "true") (header "X-Glance-Has-Next" r)
  ]

-- | Run K over a server holding four rows: one tagged @ARCHIVE@, and one whose
-- own tag merely HOLDS the word, which is what tells the meta from the plain
-- predicate.
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
         , "* TODO Near miss :archived:"
         , ":PROPERTIES:"
         , ":ORG_GLANCE_ID: near"
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
            -- The close ladder is the SHEET's rather than this sheet's: one
            -- pristine/dirty/troubled rule over whichever of the two is up, and
            -- the subtree sheet's entry is where its own flush is pinned.
            , "if (!s.dirty()) { s.shut(); return; }"
            , "if (s.state !== \"syncing\") s.flush().then((ok) => ok && s.shut());"
            , "flush: () => flush(editing.digest),"
            -- The backdrop is the mouse's ESC.
            , "if (e.target === el(id)) leaveSheet();"
            -- The receipt chains: the 200's digest is the next flush's lock, and
            -- both baselines move to what was actually sent.
            , "h.digest = a.body.digest;"
            , "base = raw ? sent.org : sent.body;"
            , "baseProps = raw ? null : JSON.stringify([sent.properties, sent.planning]);"
            -- A conflict keeps the sheet open and names the two keys.
            , "if (a.status === 409 && a.body.reason !== \"planning\") sync(\"conflict\");"
            , "conflict — C-x C-s overwrite · ESC discard"
            , "if (s.state === \"conflict\" || s.state === \"error\") {"
            , "append(s.scope, \"info\", s.closed);"
            , "closed: \"closed without writing — the file is as it was\","
            -- And a tab closing on an edited sheet still owes the file.
            , "addEventListener(\"beforeunload\""
            , "post(editing.id, editing.digest, asked(), { keepalive: true })" ] b
      -- One word carries a sheet's state, `note' is its only writer, and the
      -- states that wait for a key say which key.  No buttons to reach them
      -- with.  The retry line is one constant: three copies of it were three
      -- chances for the header to say a key that is not bound.
      holdsAll "sync status"
            [ "synced: \"synced\"", "syncing: \"syncing…\"", "id=\"mnote\""
            , "const RETRY = \" — C-x C-s retry · ESC discard\";"
            , "error: \"error\" + RETRY };"
            , "function note(s, next, message) {", "s.state = next;"
            , "const sync = (next, message) => note(subtreeSheet, next, message);" ] b
      -- Nothing else writes the word, so no header can disagree with its own
      -- sheet — one writer over BOTH of them now.
      assertEqual "note is the only writer" 1 (T.count "      s.state = next;" b)
      assertEqual "and the retry line is spelled once" 1
                  (T.count " — C-x C-s retry · ESC discard" b)
      holdsNone "a sheet button"
        [ "id=\"msave\"", "id=\"mcancel\"", "id=\"mredo\"", "id=\"mfoot\"", "Re-materialize" ] b

  , testCase "with assets, the page is one column the viewport tall" $ do
      b <- shell
      holdsAll "column"
            [ "height:100vh;box-sizing:border-box;overflow:hidden;"
            -- One padding, all four sides: the extra top was the fixed corner's
            -- room and nothing floats over the table's top edge now.
            , "padding:24px;display:flex;flex-direction:column;gap:14px}"
            -- The table asks for its height and can give it back; the key line
            -- never gives any of its own up, so a short window squeezes the
            -- table rather than clipping the line.
            , "#app{flex:1 1 auto;min-height:0}"
            , "#kbd{flex:none;" ] b
      -- Table, log, key line, in that order — the pill is fixed and out of the
      -- column, and the sheet is display:none until it is not.
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
        -- Beside the movement group: what it sorts by is the column the cell
        -- keys picked.
        , (["toggle-sort"], "sort")
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
        , (["org-agenda-set-tags"], "tags")
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
        -- The drill, named beside the key that walks back out of it: a reader
        -- shown only the way in has no way home.
        , (["org-glance-overview:relations"], "references")
        , (["filter-drop-token"], "drop token/back")
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
  , (["<right>"],    "<right>", "next-column",                     Just "nextColumn",     "table", rightHelp)
  , (["<left>"],     "<left>",  "previous-column",                 Just "previousColumn", "table", leftHelp)
  , (["<"],          "<",       "first-row",                       Just "firstRow",       "table", topHelp)
  , ([">"],          ">",       "last-row",                        Just "lastRow",        "table", endHelp)
  , (["G"],          "G",       "last-row",                        Just "lastRow",        "table", endHelp)
  , (["]"],          "]",       "next-page",                       Just "nextPage",       "table", Nothing)
  , (["["],          "[",       "previous-page",                   Just "previousPage",   "table", Nothing)
  -- table-view's own key for the same question, in both renderers: `^' sorts by
  -- the column point is in.
  , (["^"],          "^",       "toggle-sort",                     Just "toggleSort",     "table",
       Just "put this column at the head of the order; again reverses it")
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
  -- The drill: the rows pointing AT the one at point, applied as a `ref:' view
  -- with a crumb left behind for DEL to walk back along.
  , (["@"],          "@",       "org-glance-overview:relations",   Just "relations",      "table",
       Just "the rows referring to this one; DEL walks back")
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
  -- The agenda's own key for the same question over there, and the one palette
  -- that stays up: managing tags is several ops where setting a state is one.
  , ([":"],          ":",       "org-agenda-set-tags",             Just "manageTags",     "table",
       Just "add or drop tags over the marked rows, or the row at point")
  -- Both of these survive the browser where @C-c C-t@ does not: @Ctrl+S@ and
  -- @Ctrl+D@ are page default actions rather than chrome shortcuts.
  , (["C-c", "C-s"], "C-c C-s", "org-glance-overview:schedule",    Just "schedulePlan",   "table",
       planHelp)
  , (["C-c", "C-d"], "C-c C-d", "org-glance-overview:deadline",    Just "deadlinePlan",   "table",
       planHelp)
  -- Emacs's own name, since org-glance has no settings command and inventing
  -- one would put a name in this table that no map anywhere carries.
  , ([","],          ",",       "customize",                       Just "openSettings",   "table",
       Just "the settings sheet: general, theme, keyword cycles")
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
        openHelp  = Just "follow this row's link; several list them"

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
      assertEqual "cell movement has all three, the letters first"
        [["f"], ["l"], ["<right>"]]
        [ k | (k, _s, c, _h, _scope, _help) <- rows, c == "next-column" ]

    -- THERE IS NO STATUS CORNER.  What it held is said twice over without it —
    -- the socket's state is the stale wash and the strip's own `ws' lines — and
    -- what it cost was a fixed box, a z-level, a top padding to keep clear of,
    -- and the standing hazard of a control put there: outside a popup, one that
    -- keeps the focus eats `n' and `p' as type-ahead.  Asserted as an ABSENCE,
    -- so the box cannot come back by another name and bring the rule with it.
  , testCase "the page has no status corner, and nothing focusable outside a popup" $ do
      b <- shell
      holdsNone "the shell"
        [ "id=\"corner\"", "#corner", "id=\"dot\"", "#dot", "dot(\"live\")"
        , "dot(\"down\")", "dot(\"wait\")", "id=\"gear\"", "#gear" ] b
      -- Every control the page carries is inside one of the popups, which is
      -- what makes the sheet's one `blur()' on close the whole focus rule.  The
      -- page's own COLUMN — table, log, key line — is what the popups are not,
      -- and it holds nothing a browser will focus.
      column <- maybe (assertFailure "no modal band in the shell") pure
                      (between "<body>" "<div id=\"modal\">" b)
      holdsNone "the page's column"
        ["<select", "<input", "<textarea", "<button", "<a "] column
      -- And what follows the popups: the echo pill, which is a readout, up to
      -- the first script.  Both ends of the markup are swept, so a control can
      -- be added neither above the overlays nor below them.
      after <- maybe (assertFailure "no keymap blob in the shell") pure
                     (between "<div id=\"echo\"" "<script id=\"keys\"" b)
      holdsNone "under the popups"
        ["<select", "<input", "<textarea", "<button", "<a "] after
      -- And the theme lives in the settings sheet, under its own panel.
      sheet <- maybe (assertFailure "no settings sheet in the shell") pure
                     (between "<div id=\"config\">" "<div id=\"echo\"" b)
      holdsAll "the theme panel" ["id=\"ctheme\"", "id=\"themesel\""] sheet
      -- No control gives the keys back on its own change: the sheet does it once
      -- when it closes, so there is no per-control `blur()' left to keep in step.
      holdsNone "the shell" ["e.target.blur();"] b

    -- The panels are a list of headers joined to the markup BY ID, and the join
    -- is the one thing a string can get wrong: a `parts' id the markup does not
    -- carry throws at boot and takes the whole inline script with it, and the
    -- harness cannot see it (its stub answers every id).  So the ids are read
    -- back out of the list the page ships and checked against the page.
  , testCase "every panel body the sections list names is an id the markup carries" $ do
      b <- shell
      let named = concatMap quotedIn (drop 1 (T.splitOn "parts: [" b))
          quotedIn seg = [ q | (i, q) <- zip [0 :: Int ..]
                                             (T.splitOn "\"" (T.takeWhile (/= ']') seg))
                             , odd i ]
      assertBool "the sections list names no panel bodies" (not (null named))
      holdsAll "panel bodies" [ "id=\"" <> i <> "\"" | i <- named ] b

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
