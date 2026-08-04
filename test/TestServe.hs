-- | The server, driven as a WAI 'Application'.  No socket is bound: every case
-- here is a request handed straight to the app, so the suite stays free of
-- ports and of the races that come with them.  The websocket route is the one
-- thing an upgrade-less request cannot reach, and the frames it would carry
-- are TestStore's subject.
module TestServe (spec) where

import Control.Monad (filterM, forM_, (<=<))
import Data.Aeson ( FromJSON, Value (Array, Bool, Null, Number, Object, String)
                  , eitherDecode, encode, object, parseJSON, (.=) )
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.Char (isDigit, isLower)
import Data.Foldable (toList)
import Data.List (elemIndex, find, isInfixOf, nub, sort, sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Types ( HeaderName, RequestHeaders, methodDelete, methodPost
                          , renderQuery )
import Network.Wai (Application, defaultRequest, requestHeaders, requestMethod)
import Network.Wai.Test ( SResponse (simpleBody, simpleHeaders)
                        , request, runSession, setPath )
import System.Directory ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
                        , findExecutable, listDirectory )
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults ( assertContains, boolAt, digestOnDisk, document, field, holdsAll
                    , holdsNone
                    , intAt, listAt, maybeTextAt, orgFile, sparseAt
                    , sparseTextAt, systemFileIn, tagFileIn, writeLayers
                    , tagsDirIn, textAt, textsAt, viewDir, withTempDir )
import TestWire ( assertOk, capture, command, drainNow, keywordArg, ok, postTo
                , serverAt, status )

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Glance.Query ( QueryResult (qrRecords), builtinFilter
                    , linkColumns, loadDir, loadFile, tagColumns, viewJSON )
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

-- | The app OPTS runs, over a store loaded from the directory OPTS names.  It
-- reads @soDir@ rather than 'viewDir' outright: every caller here passes a
-- 'ServeOptions' whose directory IS 'viewDir', so the answer is unmoved, and a
-- caller that points one somewhere else now gets the tree it asked for instead
-- of the fixture quietly answering for it.
appOf :: ServeOptions -> IO Application
appOf opts = application opts <$> (newHub =<< loadStore (soDir opts))

-- | A server over DIR, with this suite's assets.
serverOver :: FilePath -> IO (Application, Hub)
serverOver = serverAt (Just assetsDir)

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

-- | @\/headline?id=…@ with ID percent-encoded, the way a client builds it: a
-- row id is @FILE#K@ and carries both the slashes a path segment would fight
-- over and the hash a raw URL would read as a fragment.
headlinePath :: T.Text -> ByteString
headlinePath rid = "/headline" <> renderQuery True [("id", Just (TE.encodeUtf8 rid))]

-- | @\/headline?id=RID&child=K@: the K-th entry inside RID's subtree, which is
-- the sub-addressing a sheet walks the outline by.
childPath :: T.Text -> Int -> ByteString
childPath rid k = "/headline" <> renderQuery True
  [("id", Just (TE.encodeUtf8 rid)), ("child", Just (BSC.pack (show k)))]

-- | A nested document: a row with two children and a grandchild under the
-- first, so an index has both a level jump and a sibling to be right about.
nestedDoc :: T.Text
nestedDoc = T.unlines
  [ "* TODO parent", ":PROPERTIES:", ":ORG_GLANCE_ID: top", ":END:"
  , "parent body"
  , "** child one"
  , "SCHEDULED: <2026-08-05 Wed>"
  , "one body"
  , "*** grandchild"
  , "** child two"
  ]

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

-- The three fields the harness's answer is read for most.  Each is
-- @assertEqual@ over one 'textAt', and between them they carry a third of this
-- file's shell assertions — the echo widget alone speaks every command's
-- receipt, so it is read once per command the page has.

-- | WHAT: ANSWER's echo widget says SAID.
echoIs :: String -> T.Text -> Value -> Assertion
echoIs what said = assertEqual what said <=< textAt "echo"

-- | WHAT: the URL ANSWER settled on is WANTED.
urlIs :: String -> T.Text -> Value -> Assertion
urlIs what wanted = assertEqual what wanted <=< textAt "url"

-- | WHAT: the row selected in ANSWER is the one with id WANTED.
rowIs :: String -> T.Text -> Value -> Assertion
rowIs what wanted = assertEqual what wanted <=< textAt "selected"

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
-- planning rows with @SCHEDULED@ holding SCHED, then PROPS.  Fifteen cases
-- assert some shape of this one drawer, so it is spelled once here.
panelRows :: T.Text -> [[T.Text]] -> [[T.Text]]
panelRows sched props =
  [["SCHEDULED", sched], ["DEADLINE", ""], ["CLOSED", ""]] <> props

-- | WHAT: ANSWER's property panel is 'panelRows' over SCHED and PROPS.  The
-- three lines those fifteen cases each spelled, as one.
panelIsAt :: String -> T.Text -> [[T.Text]] -> Value -> Assertion
panelIsAt what sched props answer =
  assertEqual what (panelRows sched props) =<< pairsAt "props" answer

-- | 'panelIsAt' under the stamp the harness's fixture headline carries.
panelIs :: String -> [[T.Text]] -> Value -> Assertion
panelIs what = panelIsAt what sheetStamp

-- | The subtree the shell harness serves, in the two shapes @GET \/headline@
-- hands it over in.  It carries one of every kind the document draws — a
-- headline line, a planning entry, a property, two paragraphs and a child — so a
-- case that asserts what a sync WROTE names the whole of it once here.
fixtureOrg, fixtureBody :: T.Text
fixtureOrg = "* TODO one\nSCHEDULED: <2026-08-01 Sat>\n:PROPERTIES:\n"
  <> ":ORG_GLANCE_ID: r1\n:EFFORT: 0:30\n:END:\n:LOGBOOK:\n- moved here\n:END:\n"
  <> "first para\n\nsecond para\n** two\nchild body\n"
fixtureBody = "* TODO one\nfirst para\n\nsecond para\n** two\nchild body\n"

-- | The harness's TABLED body with WAS replaced by NOW — the whole document, so
-- a case asserting a one-line splice is asserting every other byte with it.
tabledAfter :: T.Text -> T.Text -> T.Text
tabledAfter was now = T.replace was now tabledBody

-- | The body the @tabled@ act serves: a lead-in paragraph, a four-line table
-- with a rule among its rows, a two-item list and a closing paragraph.
tabledBody :: T.Text
tabledBody = T.unlines
  [ "* TODO one", "lead in", "| a | b |", "|---+---|", "| 1 | 2 |", "| 3 | 4 |"
  , "", "- alpha", "- beta", "", "tail para", "** two", "child body" ]

-- | The structured document as the sheet DREW it: one entry per element, its
-- KIND and then its parts — a headline line as its four cells, a paragraph as
-- its text.  Read off the draw rather than out of a model, since the draw is
-- what a reader has in front of them.
docOf :: Value -> IO [[T.Text]]
docOf = traverse parts <=< listAt "doc"
  where parts v = mapM text' =<< listOf v
        listOf (Array xs) = pure (toList xs)
        listOf v          = assertFailure ("expected an array, got " <> show v)
        text' (String t)  = pure t
        text' v           = assertFailure ("expected a string, got " <> show v)

-- | The PARTS of every element of KIND in ROWS, each element's own joined by
-- newlines — the texts a stop of that kind is showing.
partsOf :: T.Text -> [[T.Text]] -> [T.Text]
partsOf kind rows = [ T.intercalate "\n" (drop 1 r) | r <- rows, take 1 r == [kind] ]

-- | Where the document's cursor is: the element, and which of its cells — @-1@
-- for the whole-element look, which is what an element with no cells always has.
pointOf :: Value -> IO (Int, Int)
pointOf answer = (,) <$> intAt "dat" answer <*> intAt "dcol" answer

-- | Which of the document's elements wear a deletion flag, by their place in it.
flaggedOf :: Value -> IO [Int]
flaggedOf = flaggedAt "dflagged"

-- | The priority each posted @set-priority@ carried, in the order they went
-- out: the LETTER, or 'Nothing' for the null that takes the token off.
prioritiesOf :: Value -> IO [Maybe T.Text]
prioritiesOf = traverse one <=< argsOf
  where one v = spelled =<< field "priority" v
        spelled Null       = pure Nothing
        spelled (String t) = pure (Just t)
        spelled other      = assertFailure ("expected a priority, got " <> show other)

-- | A list of INDICES the harness reports under KEY.
flaggedAt :: T.Text -> Value -> IO [Int]
flaggedAt key = traverse whole <=< listAt key
  where whole (Number n) = pure (round n)
        whole v          = assertFailure ("expected a number, got " <> show v)

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

-- | The keywords @test\/fixtures\/view@ recognizes, in the order the badge
-- palette carries them — and so the order the state column sorts in.
--
-- The chain, spelled out: org's own pair leads, then the fixture's
-- @#+TODO: NEXT WAITING | CANCELLED@ in the order that line spells it.  This
-- list read @NEXT TODO WAITING | CANCELLED DONE@ while the union was
-- Set-shaped, which is alphabetical rather than declared and is the defect the
-- ordered chain fixed.
samplePalette :: [T.Text]
samplePalette = ["TODO", "NEXT", "WAITING", "DONE", "CANCELLED"]

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
    , blobCaptureSpec, captureViewSpec
    , configSpec, keywordsSpec, linksSpec, editLinkSpec, indexingSpec
    , pageSpec shell, keymapSpec shell, layoutSpec shell
    , glueSpec shell, bootSpec shell, liveSpec shell, washSpec shell
    , paletteSpec shell
    , moveSpec shell, sortKeySpec shell, markSpec shell, landingSpec shell
    , commandKeySpec shell, promptKeySpec shell, whichKeySpec shell
    , cellSpanSpec shell, tagKeySpec shell
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
      urlIs (boLabel <> ": the URL it settles on") boUrl answer
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
      "" "Enter" "press:C-c press:' sheet:hello close:view-changed"
      (booted <> [reasked]) [] 2 "hello" "synced" "?q=state%3A*active*"

    -- And when the file moved under the open sheet, the restore says so rather
    -- than flushing over it later: the text stands, at `conflict'.
  , Live "a sheet restored over a moved file lands in the conflict flow"
      "" "Enter" "press:C-c press:' sheet:hello rewritten close:view-changed"
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
      urlIs (lvLabel <> ": the URL") lvUrl answer
  | Live{..} <- shellLives ]

-- | LABEL's case: ACTS over a boot leave the wash making the transitions
-- WASHED, and standing at STALE.  Six of 'washSpec''s nine cases are that one
-- shape, and each keeps the comment saying which hazard it is.
washes :: IO T.Text -> String -> T.Text -> [T.Text] -> Bool -> TestTree
washes shell label acts washed stale = keyed shell label "" acts $ \answer -> do
  assertEqual "the transitions" washed =<< textsAt "washed" answer
  assertEqual "left on" stale =<< boolAt "stale" answer

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
    keyed shell "g swaps a view in one mount, and never through a partial one"
      "" "rows:150 press:g" $ \answer -> do
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
  , keyedAt shell "?q=tanik%20web" 500 "a commit that repaints hands over one set of rows"
      "" "rows:150 press:Backspace" $ \answer -> do
        paints <- paintsOf answer
        assertEqual "the boot's two, then the commit's one" [3, 3, 150] paints
        assertEqual "and no remount" 1 =<< intAt "mounts" answer

    -- The grace is the whole of what keeps the wash off a page that is working.
    -- Every answer here is a microtask, so this is the ordinary case: a boot, a
    -- swap and a reconnect, and nothing is ever dimmed.
  , washes shell "a page that answers dims nothing at all" "press:g close:resync" [] False

    -- A view whose answer is out past the grace: the rows standing are stale
    -- and say so, and the answer takes it back.
  , washes shell "a swap out past the grace dims the page, and its answer clears it"
           "hang press:g wait:400 deliver" ["on", "off"] False

    -- The COUNT is what the second half of that is for: `load' aborts the fetch
    -- before it, so an abort and the fetch replacing it overlap, and a boolean
    -- would clear the wash the replacement still wants.  Two swaps under one
    -- hang is exactly that overlap.
  , washes shell "an abort hands the wash to the fetch that replaced it"
           "hang press:g wait:400 press:g wait:100 deliver" ["on", "off"] False

    -- The other half of the grace: a reconnect that costs one revalidation is
    -- over long before the socket's delay, so a blip dims nothing.
  , washes shell "a socket blip inside its delay dims nothing" "close:resync wait:500" [] False

    -- A socket that stays gone is the one a reader can sit in for minutes: the
    -- page goes on showing rows nothing can correct, and the wash is what says
    -- so.  The daemon comes back, the retry behind the backoff finds it, and
    -- the socket that opens is what takes the wash off.
  , washes shell "a socket that stays gone dims the page, and the reconnect clears it"
           "offline close:x wait:500 online wait:900" ["on", "off"] False

    -- And it stays on for as long as the socket is gone: the arming is not a
    -- flash that goes by itself.
  , washes shell "and stays on while it is still gone" "offline close:x wait:500" ["on"] True

    -- A sheet open over stale rows is stale with them.  The class is the
    -- DOCUMENT's, so it reaches the overlays without this page naming one —
    -- which selectors it reaches them by is `shellGlue''s row.
  , keyed shell "an open sheet is washed with the rows under it"
      "Enter" "offline close:x wait:500" $ \answer -> do
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
  [ keyed shell "a half-typed palette is raised again after a remount"
      "/" "filter:tan close:view-changed" $ \answer -> do
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
    keyed shell "< takes the page's first row"
      "" (moveScript "press:n press:n press:<") $ \answer -> do
        rowIs "the row" "r1" answer
        assertEqual "the page it stayed on" 1 =<< intAt "page" answer
        echoIs "the echo" "< → first-row" answer

    -- On it, the same key climbs — and lands on the FIRST row of the page it
    -- turned to, where the renderer's own turn lands on the last.
  , keyed shell "< on the first row turns back a page and lands on its first row"
      "" (moveScript "press:] press:] press:<") $ \answer -> do
        rowIs "the row" "r4" answer
        assertEqual "the page" 2 =<< intAt "page" answer
        echoIs "the echo names it" "< → first-row (page 2/3)" answer

    -- The chain, to the top and then nowhere: page three's first row, page
    -- two's, page one's, and a fourth press that moves nothing.
  , keyed shell "and stops on page one's first row"
      "" (moveScript "press:] press:] press:< press:< press:<") $
        \answer -> do
          rowIs "the row" "r1" answer
          assertEqual "the page" 1 =<< intAt "page" answer
          -- A stop is the plain echo: nothing moved, so no page is named.
          echoIs "the echo" "< → first-row" answer

  , keyed shell "> takes the page's last row" "" (moveScript "press:>") $ \answer -> do
        rowIs "the row" "r3" answer
        assertEqual "the page it stayed on" 1 =<< intAt "page" answer
        echoIs "the echo" "> → last-row" answer

    -- The asymmetric half: `nextPage' lands on the new page's FIRST row, so
    -- without the follow-up select this answers `r4'.
  , keyed shell "> on the last row turns a page and lands on its last row"
      "" (moveScript "press:> press:>") $ \answer -> do
        rowIs "the row" "r6" answer
        assertEqual "the page" 2 =<< intAt "page" answer
        echoIs "the echo names it" "> → last-row (page 2/3)" answer

    -- vi's spelling of the same command, walked to the bottom and held there.
  , keyed shell "G is that key, and the last page's last row is the end of it"
      "" (moveScript "press:G press:G press:G press:G") $
        \answer -> do
          rowIs "the row" "r9" answer
          assertEqual "the page" 3 =<< intAt "page" answer
          echoIs "the echo" "G → last-row" answer

    -- The arrows walk BOTH axes, and silently: the key line shows a command's
    -- first binding, so `<right>' sits behind `f' the way `<down>' has always
    -- sat behind `n'.  Same handler, so walking off the last cell is the
    -- LANDING it is for the letters — the renderer reads a column index outside
    -- the table as no column at all — rather than a wall this page invents.
  , testCase "the arrows step the column too, and land off the ends" $ do
      onTable "press:ArrowRight" $ \answer -> do
        assertEqual "the first column, from the whole-row look" 0 =<< intAt "col" answer
        echoIs "named by the header over it" "<right> → next-column (state)" answer
      onTable "press:ArrowRight press:ArrowRight" $
        assertEqual "and the next one" 1 <=< intAt "col"
      -- Two columns, so the third step walks off the end and lands.
      onTable "press:ArrowRight press:ArrowRight press:ArrowRight" $ \answer -> do
        assertEqual "off the cells" Null =<< field "col" answer
        echoIs "which the echo says is a landing" "<right> → next-column (row mode)" answer
      onTable "press:ArrowLeft" $
        assertEqual "and the other arrow lands on the first column too" 0 <=< intAt "col"

    -- The column is the renderer's across a turn, and this page hands it back
    -- rather than keeping one: `f' picks column 0 and it survives the climb.
  , keyed shell "a climb keeps the column the cursor was in"
      "" (moveScript "press:f press:> press:>") $ \answer -> do
        rowIs "the row" "r6" answer
        assertEqual "the column" 0 =<< intAt "col" answer

    -- An asset with no pager keeps the half it can do, and says it the same
    -- way: a key that cannot climb still reports the row it took.
  , keyed shell "an asset without a pager keeps the within-page jump"
      "" (moveScript "press:] pageless press:< press:<") $
        \answer -> do
          rowIs "the row" "r4" answer
          assertEqual "the page it could not leave" 2 =<< intAt "page" answer
          echoIs "the echo" "< → first-row" answer

  ]

-- | Nine rows over three pages, then SCRIPT.  Every case here needs a set with
-- pages in it, and the harness's three rows are one page whatever the size.
  where
    onTable = bootOf shell "" 500 ""

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
  [ keyed shell "sorts by the column at point: the leader flips in place" "f ^" "" $ \answer -> do
        assertEqual "the view's chain opens on state, so the press flips it"
                    (Just ("state", False)) =<< sortOf answer
        echoIs "and the echo speaks the direction it landed in"
          "^ → toggle-sort (state ▼)" answer

  , testCase "a second press flips the leader back, and a third again" $ do
      bootOf shell "" 500 "f ^ ^" "" $ \answer -> do
        assertEqual "the leader flips alone" (Just ("state", True)) =<< sortOf answer
        echoIs "the echo" "^ → toggle-sort (state ▲)" answer
      bootOf shell "" 500 "f ^ ^ ^" "" $
        assertEqual "and round again" (Just ("state", False)) <=< sortOf

    -- The column is the renderer's, so a selection that names none is a
    -- question this page cannot answer: it says which key answers it instead of
    -- picking a column on the reader's behalf.
  , keyed shell "a whole-row selection names no column, and the key says which picks one"
      "^" "" $ \answer -> do
        assertEqual "nothing was asked of the renderer" 0 =<< intAt "sortCalls" answer
        echoIs "the echo names the key that picks a column"
          "^ → toggle-sort (no column selected — f/l to pick one)" answer

    -- `sortable' gates what a READER may reach and `sortBy' ignores it, so a
    -- page driving a reader's key is the only thing that can honour it.
  , keyed shell "a column that declares no sortable is left alone" "f f ^" "" $ \answer -> do
        assertEqual "the column the cursor is in" 1 =<< intAt "col" answer
        assertEqual "nothing was asked of the renderer" 0 =<< intAt "sortCalls" answer
        echoIs "and the echo names it" "^ → toggle-sort (tag does not sort)" answer

  , keyed shell "an asset with no programmatic sort is named, not crashed into"
      "" "sortless press:f press:^" $ \answer -> do
        assertEqual "no sort was asked for" Nothing =<< sortOf answer
        echoIs "the echo" "^ → toggle-sort (this table-view.js has no sort)" answer

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
        echoIs "the echo" "^ → toggle-sort (state ▲)" answer

    -- A REMOUNT re-reads the chain off the query it mounts under, which now
    -- carries the order: the press after one continues the chain the reader
    -- built rather than starting the declared one over.
  , keyed shell "a remount re-seeds the chain off the query it mounts under"
      "f ^" "close:view-changed press:f press:^" $
        echoIs "the leader the query named, flipped back" "^ → toggle-sort (state ▲)"

    -- THE PRESS IS A QUERY EDIT.  The renderer writes the chain into the applied
    -- query and delivers it, so it arrives here as an ordinary commit: the URL
    -- is rewritten and the server is asked for the order it was just told about,
    -- which is what makes page one of a limited answer the right hundred rows.
  , testCase "the press writes the order into the query and asks for it" $
      -- A bare boot opens on the default view, so the press lands beside the
      -- query that was already applied rather than over it.
      bootOf shell "" 500 "f ^" "" $ \answer -> do
        urlIs "the URL carries the order" "?q=state%3A*active*+sort%3Astate%3Adesc" answer
        assertEqual "and the server was asked for it"
                    (Just "/headlines?q=state%3A*active*%20sort%3Astate%3Adesc")
          . lastOf =<< textsAt "asked" answer

    -- And it composes with a filter rather than replacing it: the sort tokens
    -- are the query's own, so a narrowed view stays narrowed.
  , keyedAt shell "?q=state%3ATODO" 500 "the order joins a filter already applied" "f ^" "" $
        urlIs "the predicate, then the order" "?q=state%3ATODO+sort%3Astate%3Adesc"

    -- DEL takes it off like any other token, which is the whole of the way home:
    -- with no sort token the answer comes back in the view's declared order.
  , keyedAt shell "?q=state%3ATODO" 500 "DEL takes the order back off"
      "f ^" "press:Backspace" $ \answer -> do
        urlIs "the query the strip left" "?q=state%3ATODO" answer
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
markSpec shell =
  overBoot shell "" "" $ \plain ->
  testGroup "Shell marks"
  [ -- DEL'S FIRST RUNG: ERASE THE LAST STRUCTURE STANDING, which is the
    -- backspace's own rhyme lifted one more level.  A MARKED SET is a structure
    -- a reader put there, so while there are marks the key takes them off and
    -- stops — the query is not touched, and the next press is the one that
    -- reaches it.
    testCase "DEL clears the marks first, and leaves the query alone" $ do
      bootOf shell "?q=state%3ATODO+web" 500 "m m Backspace" "" $ \answer -> do
        assertEqual "the marks are gone" ([] :: [T.Text]) =<< textsAt "marked" answer
        urlIs "and the query is untouched" "?q=state%3ATODO+web" answer
        -- The pill names `unmark-all', which is the command that RAN: DEL
        -- delegates to `U''s own implementation and says so.
        echoIs "the pill names the command that ran and counts it" "DEL → unmark-all (2)" answer

      -- The SECOND press finds no marks and falls through to the rung it always
      -- had, in silence — a rung with nothing under it does not speak.
    , keyedAt shell "?q=state%3ATODO+web" 500 "and the second DEL drops a token, as it always did"
        "m m Backspace Backspace" "" $ \answer -> do
          urlIs "one token off" "?q=state%3ATODO" answer
          echoIs "and the pill is the filter's again"
            "DEL → filter-drop-token (filter: \"state:TODO\")" answer

      -- FLAGS ARE NOT MARKS and the rung leaves them where they are: a flag is
      -- the archive queue, and a backspace that emptied it would throw away a
      -- set a reader built to write with.
    , keyedAt shell "?q=state%3ATODO+web" 500
        "and the flags stand, being the archive queue rather than a mark"
        "d m m Backspace" "" $ \answer -> do
          assertEqual "the marks went" ([] :: [T.Text]) =<< textsAt "marked" answer
          assertEqual "the flag stayed" ["r1"] =<< textsAt "flagged" answer

      -- With NO marks the ladder is the one it always was, first press included.
    , keyedAt shell "?q=state%3ATODO+web" 500 "with nothing marked the first press is still the filter's"
        "Backspace" "" $ \answer -> do
          urlIs "one token off" "?q=state%3ATODO" answer
          echoIs "and it said so" "DEL → filter-drop-token (filter: \"state:TODO\")" answer

      -- An asset with no marks at all has no rung to run, and the key falls
      -- through without saying anything about marks.
    , keyedAt shell "?q=state%3ATODO+web" 500 "an asset with no marks falls straight through"
        "" "bare press:Backspace" $ \answer -> do
          urlIs "one token off" "?q=state%3ATODO" answer
          echoIs "and the pill never mentioned marks"
            "DEL → filter-drop-token (filter: \"state:TODO\")" answer

  ,  atBoot plain "the mount asks for them" $
        assertEqual "marks:true reached the renderer" True <=< boolAt "marksOn"

    -- The flag's own hint, drawn by the renderer over the row wearing one: the
    -- keys are this page's, so the wording is too.
  , atBoot plain "and names the keys a flagged row answers to" $
        assertEqual "flagHelp reached the renderer" "d/D archive · u unflag"
          <=< textAt "flagHelp"

    -- The renderer's per-row hint says RET materializes, which the resident key
    -- line under the table already says — and says for every command rather
    -- than for the one.  One place, so the mount turns the other off.
  , atBoot plain "and asks for no per-row hints, the key line saying it once" $
        assertEqual "actionHints:false reached the renderer" False <=< boolAt "hintsOn"

    -- Dired's walk: two presses mark two rows rather than one row twice, and
    -- the count in the echo is the renderer's own.
  , keyed shell "m marks the row it is on and steps to the next" "m m" "" $ \answer -> do
        assertEqual "the rows it marked" ["r1", "r2"] =<< textsAt "marked" answer
        assertEqual "and where it left the cursor" 2 =<< intAt "cursor" answer
        echoIs "counting as it went" "m → mark-toggle (marked · 2)" answer

    -- The same key on the same row takes it back off, which is what makes it a
    -- toggle: `m' twice over one row leaves nothing, since the second press is
    -- on the row the first one stepped to.
  , keyed shell "m on a marked row unmarks it" "m" "press:ArrowUp press:m" $ \answer -> do
        assertEqual "nothing marked" [] =<< textsAt "marked" answer
        echoIs "and it says so" "m → mark-toggle (unmarked · 0)" answer

    -- `u' only ever takes a mark off.  After `m' the cursor is on an unmarked
    -- row, so a toggle would mark it and the count would read 2.
  , keyed shell "u never marks a row, it only unmarks one" "m u" "" $ \answer -> do
        assertEqual "the first mark stands alone" ["r1"] =<< textsAt "marked" answer
        echoIs "and the count did not grow" "u → unmark (unmarked · 1)" answer

  , keyed shell "U clears every mark at once" "m m U" "" $ \answer -> do
        assertEqual "nothing left" [] =<< textsAt "marked" answer
        echoIs "the echo" "U → unmark-all (all marks and flags cleared)" answer

    -- `M' is the renderer's call because the SET is the renderer's: a page it is
    -- not showing is marked too, which is the whole reason a shell-side loop
    -- over the visible rows would be the wrong answer.
  , keyed shell "M marks every row loaded, not the page on show" "M" "" $ \answer -> do
        assertEqual "all three" ["r1", "r2", "r3"] =<< textsAt "marked" answer
        echoIs "counted by the renderer" "M → mark-all (marked · 3)" answer
        assertEqual "and the cursor stayed where it was" 0 =<< intAt "cursor" answer

    -- dired's flag, in two presses: the first marks the row for archiving and
    -- the second is the confirmation.  One press writes nothing at all.
  , testCase "d flags the row, and a second d archives it" $ do
      bootOf shell "" 500 "d" "" $ \answer -> do
        assertEqual "the row is flagged" ["r1"] =<< textsAt "flagged" answer
        assertEqual "and nothing was written" [] =<< postedOf answer
        echoIs "the pill says what the next press costs"
          "d → archive-flag (flagged — d again archives)" answer
        -- The two sets are the renderer's own and stay apart: flagging a row
        -- leaves the marked set exactly where it was.
        assertEqual "and no mark went on with it" [] =<< textsAt "marked" answer
      -- One flag is a set of one, so the single-row flow is the general one and
      -- reads as it: the second press is `D', and `D' names the set it ran over.
      bootOf shell "" 500 "d d" "" $ \answer -> do
        assertEqual "one flag is a set of one, so the second press takes it"
                    [("archive", ["r1"])] =<< postedOf answer
        assertEqual "and the flag is spent" [] =<< textsAt "flagged" answer
        echoIs "counted" "d → archive-flag (archived · 1 flagged)" answer

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

  , keyed shell "and an unmarked row costs no mark at all" "n m p p d d" "" $ \answer -> do
        assertEqual "the row at point was archived" [("archive", ["r1"])]
          =<< postedOf answer
        assertEqual "the mark on the OTHER row is untouched" ["r2"]
          =<< textsAt "marked" answer

    -- A refused write archived nothing, so it spends nothing either.
  , keyed shell "a refused archive leaves the mark where it was"
      "" "refuse press:m press:p press:d press:d" $ \answer -> do
        assertEqual "the command went" [("archive", ["r1"])] =<< postedOf answer
        assertEqual "and the mark stands" ["r1"] =<< textsAt "marked" answer

    -- The flag stays on the ROW rather than following the cursor, so a walk
    -- between the two presses is a walk back before the second one lands.
  , keyed shell "d on one row and d on another flags both and archives neither"
      "d n d" "" $ \answer -> do
        assertEqual "two rows flagged" ["r1", "r2"] =<< textsAt "flagged" answer
        assertEqual "and nothing written" [] =<< postedOf answer

    -- dired's `dd': the second press is `D', so it takes the WHOLE flagged set
    -- rather than the row under it.  `d n d n d' flags r1, r2 and r3 and leaves
    -- the cursor on r3; the press after that archives all three at once.
  , keyed shell "the second d archives every flagged row, not just the one under it"
      "d n d n d" "press:d" $ \answer -> do
        assertEqual "all three, in one request"
                    [("archive", ["r1", "r2", "r3"])] =<< postedOf answer
        assertEqual "and no flag is left" [] =<< textsAt "flagged" answer
        echoIs "named the way D names it" "d → archive-flag (archived · 3 flagged)" answer

    -- The same set, the same request, the same pill: `D' is `d' without the
    -- flagging press in front of it, and there is one implementation.
  , keyed shell "D on that same set does exactly what the second d does"
      "d n d n d" "press:D" $ \answer -> do
        assertEqual "the same three" [("archive", ["r1", "r2", "r3"])]
          =<< postedOf answer
        echoIs "the same pill, under its own key"
          "D → org-glance-overview:delete (archived · 3 flagged)" answer

    -- `d' is in ONCE, and this is why: a HELD key reaching the handler twice
    -- would flag a row and archive it from one press, which is exactly the
    -- confirmation the two-press shape exists to be.
  , keyed shell "a held d flags and stops there" "d" "repeat:d repeat:d repeat:d" $ \answer -> do
        assertEqual "still just flagged" ["r1"] =<< textsAt "flagged" answer
        assertEqual "and the burst wrote nothing" [] =<< postedOf answer

    -- `u' takes the flag off first: it is the more recent thing a reader put on
    -- the row, and the one that would otherwise write a file.
  , testCase "u clears an archive flag before it touches a mark" $ do
      bootOf shell "" 500 "d" "press:ArrowUp press:u" $ \answer -> do
        assertEqual "the flag is off" [] =<< textsAt "flagged" answer
        echoIs "and it says which" "u → unmark (flag cleared)" answer
      -- `m' marks r1 and steps; `d' flags r2 where it landed.  `u' on r2 takes
      -- the flag, and `u' back on r1 takes the mark — one key, flag first.
      bootOf shell "" 500 "m d" "press:u press:ArrowUp press:ArrowUp press:u" $ \answer -> do
        assertEqual "the flag went" [] =<< textsAt "flagged" answer
        assertEqual "and the mark after it" [] =<< textsAt "marked" answer

  , keyed shell "U clears the flags along with the marks" "m d" "press:U" $ \answer -> do
        assertEqual "no marks" [] =<< textsAt "marked" answer
        assertEqual "and no flags" [] =<< textsAt "flagged" answer

    -- An asset predating the flag calls is named rather than crashed into, the
    -- same way the mark calls are: the write must never be the fallback.
  , keyed shell "a table-view.js without the flag calls is named, not crashed into"
      "" "bare press:d" $ \answer -> do
        assertEqual "nothing was written" [] =<< postedOf answer
        echoIs "and it said why"
          "d → archive-flag (this table-view.js has no archive flags)" answer

    -- An asset predating the calls: the key says what is missing rather than
    -- throwing, the same way the pager and the token strip do.  A throw would
    -- fail the harness outright, so what this pins is the wording — and that
    -- `m' left the cursor alone, since a key that cannot do its job must not
    -- half-do it.
  , keyed shell "a table-view.js without the calls is named, not crashed into"
      "" "bare press:m press:U" $ \answer -> do
        assertEqual "and it did not walk on regardless" 0 =<< intAt "cursor" answer
        echoIs "the last key said why" "U → unmark-all (this table-view.js has no marks)" answer
  ]

-- | Where point ends up: on the BOOT, and after an archive takes its row out
-- of the view.
--
-- The boot opens the group because it is the landing every other one is
-- measured against — a mount has no cursor of its own, so a page that landed
-- nothing opened with every row key answering @no row@.
--
-- The archive's rows leave by one of two doors and both are driven here: an
-- unfiltered client SPLICES the socket's row ops straight in, and a filtered
-- one reads none of them and refetches.  The anchor is worked out at FIRE time,
-- while the view still holds the rows about to go, and lands at whichever door
-- they actually left by.
--
-- The remaining landing rules are somebody else's cases and stay there: an
-- applied view lands on row one (@moveSpec@, @drillSpec@) and a pop puts back
-- the row its drill was launched from (@drillSpec@).  The one archive case that
-- touches them pins that an applied view still lands on row one immediately
-- after an anchor landed somewhere else.
landingSpec :: IO T.Text -> TestTree
landingSpec shell = testGroup "Shell landing"
  [ -- A BOOT IS AN APPLIED VIEW, so it lands where every applied view lands.
    -- The renderer selects nothing until it is asked to, so the cursor on row
    -- one here is this page's own landing and nothing else.  The total is 500
    -- over a three-row store, so the whole set arrives behind the first page:
    -- the landing is taken on the FIRST paint and the swap behind it keeps it,
    -- which is the one landing per mount.
    keyed shell "a boot lands on row one, like every other applied view" "" "" $ \answer -> do
        assertEqual "the first row of the answer" (Just "r1")
          =<< maybeTextAt "selected" answer
        assertEqual "at the top of the page" 0 =<< intAt "cursor" answer
        assertEqual "and the whole set arrived behind the first page" 2 . length
          =<< listAt "paints" answer

    -- Which is the whole point of it: the first key a reader presses has a row
    -- to work on, with no `n' spent to reach one.
  , keyed shell "so the first key pressed already has a row to work on" "d d" "" $ \answer -> do
        assertEqual "the row the boot landed on" [("archive", ["r1"])]
          =<< postedOf answer
        echoIs "and the pill named the write" "d → archive-flag (archived · 1 flagged)" answer

    -- AND AN EMPTY ANSWER LANDS NOTHING.  `land' selects nothing where there is
    -- no row to select, so the keys that want one say so rather than writing
    -- over a row that is not there.
  , keyedAt shell "" 0 "an empty answer leaves nothing selected, and d says so"
      "d" "" $ \answer -> do
        assertEqual "no row is on" Nothing =<< maybeTextAt "selected" answer
        assertEqual "and the cursor is nowhere" (-1) =<< intAt "cursor" answer
        echoIs "which the key names" "d → archive-flag (no row)" answer
        assertEqual "nothing was flagged" [] =<< textsAt "flagged" answer
        assertEqual "and nothing was written" [] =<< postedOf answer

    -- RET is the other key with a row in its hand, and it names the key that
    -- would pick one rather than opening a sheet over nothing.
  , keyedAt shell "" 0 "and RET says which key would pick one" "Enter" "" $ \answer -> do
        assertEqual "the strip says what to press"
                    (Just "no row focused — n or p picks one") =<< lastLog answer
        assertEqual "and no sheet opened" "" =<< textAt "modal" answer

    -- A LANDING SOMEBODY ASKED FOR OUTRANKS THE BOOT'S.  A pop carries the row
    -- its drill was launched from, and it arrives through `applyView', whose
    -- own landing runs in place of this door's — so the trail's remembered row
    -- is not overwritten by row one on the way past.
  , keyedAt shell ("?q=ref%3Ar1&crumbs="
                    <> bootedSels) 500 "a pop out of a booted trail still lands on the remembered row"
      "Backspace" "" $
        \answer -> do
          assertEqual "the row the drill was launched from" (Just "r3")
            =<< maybeTextAt "selected" answer
          urlIs "over the crumb's own query, which is what was applied" "?q=" answer

    -- dired's: the row point was standing on goes, and point goes to the one
    -- after it.  Under a filter that means the refetch the frame scheduled,
    -- which is where the rows leave for a filtered reader — and the frame the
    -- server sent was an UPSERT, the row still being the store's.
  , keyed shell "an archived row mid-table lands point on the next surviving row"
      "n d d" "unserved:r2 frame:upsert=r2 wait:300" $ \answer -> do
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
  , keyed shell "archiving the last row lands point on the new last"
      "n n d d" "unserved:r3 frame:upsert=r3 wait:300" $ \answer -> do
        assertEqual "the last row went" [("archive", ["r3"])] =<< postedOf answer
        assertEqual "and point is on the one above it"
                    (Just "r2") =<< maybeTextAt "selected" answer

    -- THE CASE THE RENDERER'S OWN KEEPING GETS WRONG, and the reason the anchor
    -- is taken at fire time at all.  Six rows, `r1' and `r4' flagged, point on
    -- `r4': the next surviving row is `r5', but rows went from ABOVE point too,
    -- so the visual PLACE point stood in — index 3 — is `r6' once they have
    -- gone.  A landing that only knew where point had been would skip a row.
  , keyed shell "the anchor is the next surviving row, not the place point stood"
      "" ("rows:6 press:d press:n press:n press:n press:d press:D"
           <> " unserved:r1,r4 frame:upsert=r1 frame:upsert=r4 wait:300") $ \answer -> do
        assertEqual "both flagged rows, in one request"
                    [("archive", ["r1", "r4"])] =<< postedOf answer
        assertEqual "and the flags are spent" [] =<< textsAt "flagged" answer
        assertEqual "the row under the one that went, not the one two below it"
                    (Just "r5") =<< maybeTextAt "selected" answer

    -- And with point on a row that SURVIVES the set, nothing is owed: no
    -- anchor is armed at all, so it stays exactly where it stood — which is what
    -- "where point was" means when point did not have to move.
  , keyed shell "a set archived from a surviving row leaves point on that row"
      "" ("rows:5 press:n press:d press:n press:n press:d press:p press:D"
           <> " unserved:r2,r4 frame:upsert=r2 frame:upsert=r4 wait:300") $
        assertEqual "the row point was on is still under it"
                    (Just "r3") <=< maybeTextAt "selected"

    -- And no anchor is left ARMED behind it either.  The anchor belongs to the
    -- archive that took point's row away, so an archive that took some other
    -- row must leave nothing lying in wait: when point's row later goes for
    -- some unrelated reason, the renderer's own keeping is the whole rule.
  , keyedAt shell "?q=" 500 "and arms nothing for a later removal to land on"
      "" ("rows:6 press:d press:n press:n press:n press:d press:p press:D"
           <> " frame:delete=r1,r4 frame:delete=r3") $
        assertEqual "the row that took r3's place, not the archive's own anchor"
                    (Just "r6") <=< maybeTextAt "selected"

    -- A page where every row is leaving has nowhere to land, so the anchor is
    -- nothing and the empty view selects nothing — which is what an applied
    -- view with no rows in it already did, and what the renderer does when the
    -- last row goes out from under the cursor.
  , keyed shell "archiving every row leaves nothing selected"
      "d n d n d" ("press:d unserved:r1,r2,r3"
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
  , keyed shell "a watch refetch under a filter leaves point where it was"
      "n n" "frame:upsert=r1 wait:300" $ \answer -> do
        assertEqual "the frame was re-asked for" 3 . length =<< listAt "paints" answer
        assertEqual "and point did not move for it"
                    (Just "r3") =<< maybeTextAt "selected" answer

    -- A refused write moved no row, so the landing it armed goes with the marks
    -- it did not spend: the row point was on is still there.  When it later
    -- goes for some other reason the renderer's own keeping is the whole rule,
    -- which lands on the row that took its PLACE rather than on the one the
    -- archive would have picked.
  , keyedAt shell "?q=" 500 "a refused archive arms no landing"
      "" "refuse press:d press:n press:d press:p press:D frame:delete=r1" $
        assertEqual "the row that took r1's place, not the anchor's r3"
                    (Just "r2") <=< maybeTextAt "selected"

    -- THE ANCHOR ITSELF VANISHING between the fire and the landing, which is
    -- what the remembered PLACE is for: `r3' is archived from under point and
    -- `r4', the row it was to land on, goes to somebody else's edit first.
    -- `select' answers false for a row the view no longer holds, so the landing
    -- falls through to where the anchor WOULD have been sitting once the
    -- archived rows had gone — index 1 of what is left — rather than to row one.
  , keyed shell "an anchor the view lost falls back to the place it would have had"
      "" ("rows:4 press:n press:d press:d unserved:r2,r3"
           <> " frame:upsert=r2 wait:300") $ \answer -> do
        assertEqual "the row point was on" [("archive", ["r2"])] =<< postedOf answer
        assertEqual "the place, since the row it named is gone too"
                    (Just "r4") =<< maybeTextAt "selected" answer

    -- An archive is an UPSERT on the wire — `Store.streamed` emits a delete
    -- only for an id that left the store, and archiving adds a tag to a row
    -- that stays — so an unfiltered client keeps the row it just archived:
    -- `/headlines` would not have served it, and the socket is not filtered.
    -- Nothing left the view, so point does not move.
  , keyedAt shell "?q=" 500 "an archived row an unfiltered client keeps does not move point"
      "n d d" "frame:upsert=r2" $ \answer -> do
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
  , keyedAt shell "?q=" 500 "and its frames spend the anchor rather than landing it"
      "" ("rows:6 press:d press:n press:n press:n press:d press:D"
           <> " frame:upsert=r1 frame:upsert=r4 frame:delete=r1,r4") $ \answer -> do
        assertEqual "the frames the archive itself caused, then the removals"
                    [ "upsert r1", "upsert r4", "delete r1", "delete r4" ]
                    =<< textsAt "spliced" answer
        assertEqual "the renderer's place, the anchor having been spent"
                    (Just "r6") =<< maybeTextAt "selected" answer

    -- The carve reaches the WATCH's refetch and nothing else: an applied view
    -- is a new question and still lands on row one, immediately after an anchor
    -- landed somewhere else.
  , keyed shell "an applied view still lands on row one after an anchor did not"
      "n d d" "unserved:r2 frame:upsert=r2 wait:300 press:g" $
        assertEqual "g took the top of its answer" (Just "r1")
          <=< maybeTextAt "selected"

    -- An anchor belongs to the VIEW it was taken in, and a mount thrown away
    -- takes it with it.  Reachable because an archive under NO filter leaves
    -- its row on screen — the socket carries an upsert whatever the query — so
    -- the anchor is still armed when `g' rebuilds the table.  Left standing, it
    -- would fire on the next frame and pull the cursor off the row the new view
    -- had just landed it on.
  , keyedAt shell "?q=" 500 "a remount drops an anchor the archive never spent"
      "n d d" "press:g frame:delete=r2 wait:300" $
        assertEqual "where g landed it, not where the old view's anchor pointed"
                    (Just "r1") <=< maybeTextAt "selected"

    -- `visible()` is ONE PAGE, so "the row point was on has left the view" is
    -- only answerable about the page the anchor was taken on.  A reader who
    -- turned a page between the write and its watch event would otherwise be
    -- told every row of that page had gone, and be landed on the new page's
    -- row `at`.
  , keyed shell "an anchor is not landed on a page it was not taken on"
      "" ("rows:6 paged:3 press:n press:n press:d press:d press:] press:n"
           <> " unserved:r3 frame:upsert=r3 wait:300") $ \answer -> do
        assertEqual "the row point was on" [("archive", ["r3"])] =<< postedOf answer
        assertEqual "still on the page it walked to" 2 =<< intAt "page" answer
        assertEqual "and on the row it walked to, not the other page's anchor"
                    (Just "r5") =<< maybeTextAt "selected" answer

    -- The third road the same rows can arrive without them: a socket that was
    -- down while the write landed, and a reconnect whose answer is the first
    -- this page has seen since.  `resync` repaints the same view, so it settles
    -- the anchor exactly as the watch's own refetch would have.
  , keyed shell "a reconnect's repaint lands the anchor too"
      "" ("rows:6 press:d press:n press:n press:n press:d press:D"
           <> " unserved:r1,r4 close:resync") $
        assertEqual "the next surviving row, not the renderer's place"
                    (Just "r5") <=< maybeTextAt "selected"

    -- And the other door that replaces a view without rebuilding the mount: a
    -- COMMIT.  `^` writes its chain into the query, which is a commit like any
    -- other, so the anchor taken under the query being left goes with it.
  , keyedAt shell "?q=" 500 "and so does a commit, which replaces the view without a remount"
      "n d d" "press:f press:^ frame:delete=r2 wait:300" $
        assertEqual "where the commit landed it, not the old view's anchor"
                    (Just "r1") <=< maybeTextAt "selected"
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
  [ -- ORG'S PRIORITY RING, pressed.  Up runs `none → C → B → A → none' and down
    -- the reverse, and the WRAP IS THROUGH NONE, which is what makes the key
    -- that sets a priority the key that takes it off.
    testCase "S-up cycles the priority, and wraps through none" $ do
      bootOf shell "" 500 "S-ArrowUp" "" $ \answer -> do
        assertEqual "an entry with none takes the lowest"
                    [("set-priority", ["r1"])] =<< postedOf answer
        assertEqual "which is C" [Just "C"] =<< prioritiesOf answer
        echoIs "and the pill names the command and the landing"
          "S-<up> → priority-up ([#C] · 1)" answer
      onTable "priorities:C press:S-ArrowUp" $
        assertEqual "C climbs to B" [Just "B"] <=< prioritiesOf
      onTable "priorities:B press:S-ArrowUp" $
        assertEqual "and B to A" [Just "A"] <=< prioritiesOf
      onTable "priorities:A press:S-ArrowUp" $ \answer -> do
        assertEqual "and A wraps to none" [Nothing] =<< prioritiesOf answer
        echoIs "which the pill spells as the meta it is"
          "S-<up> → priority-up (*empty* · 1)" answer

  , testCase "and S-down runs the same ring the other way" $ do
      bootOf shell "" 500 "S-ArrowDown" "" $
        assertEqual "none wraps to the highest" [Just "A"] <=< prioritiesOf
      onTable "priorities:A press:S-ArrowDown" $
        assertEqual "A falls to B" [Just "B"] <=< prioritiesOf
      onTable "priorities:B press:S-ArrowDown" $
        assertEqual "and B to C" [Just "C"] <=< prioritiesOf
      onTable "priorities:C press:S-ArrowDown" $
        assertEqual "and C to none" [Nothing] <=< prioritiesOf

    -- EACH ROW CYCLES FROM ITS OWN VALUE, which is org's per-entry semantics —
    -- and the one thing a single request cannot carry, `args' being one object
    -- for the call.  So a MIXED marked set is one command per landing value,
    -- each over the rows that land there, and the set stays mixed.
  , keyed shell "a mixed marked set is one command per landing, and stays mixed"
      "" "priorities:A,,C press:m press:m press:m press:S-ArrowUp" $ \answer -> do
        assertEqual "three rows, three landings"
                    [ ("set-priority", ["r1"]), ("set-priority", ["r2"])
                    , ("set-priority", ["r3"]) ] =<< postedOf answer
        assertEqual "A wrapped, none climbed to C, C climbed to B"
                    [Nothing, Just "C", Just "B"] =<< prioritiesOf answer

    -- A set that AGREES is the common press and costs one request.
  , keyed shell "and a set that agrees is one"
      "" "priorities:B,B,B press:m press:m press:m press:S-ArrowUp" $ \answer -> do
        assertEqual "one command over all three"
                    [("set-priority", ["r1", "r2", "r3"])] =<< postedOf answer
        assertEqual "at the one landing" [Just "A"] =<< prioritiesOf answer

  , keyedAt shell "" 0 "with no row the key says so and posts nothing"
      "S-ArrowUp" "" $ \answer -> do
        assertEqual "nothing posted" ([] :: [Value]) =<< listAt "commands" answer
        echoIs "and it said why" "S-<up> → priority-up (no row)" answer

  ,  keyed shell "D with nothing flagged archives the row at point" "D" "" $ \answer -> do
        assertEqual "one archive, over the selected row"
                    [("archive", ["r1"])] =<< postedOf answer
        echoIs "and the pill says which"
          "D → org-glance-overview:delete (archived · row)" answer

    -- The FLAGGED set is what `D' runs over. A flag is a selection made for
    -- archiving; a mark is the generic bulk selection a reader lays down to set
    -- a state over a run of rows, and letting the archive key inherit one would
    -- make every mark a loaded gun.
  , testCase "D archives the flagged set, and leaves the marks where they are" $
      -- `m m' marks r1 and r2 and steps to r3; `d' flags r3.
      bootOf shell "" 500 "m m d" "press:D" $ \answer -> do
        assertEqual "the flagged row, and only it"
                    [("archive", ["r3"])] =<< postedOf answer
        echoIs "named as the set it was"
          "D → org-glance-overview:delete (archived · 1 flagged)" answer
        assertEqual "the marks are untouched" ["r1", "r2"] =<< textsAt "marked" answer

    -- The flags are spent, the way a second `d' spends the one it fires over.
    -- They have to be: the renderer keeps a flag whose row a filter is hiding,
    -- so a set left standing would be archived again by the next press and the
    -- row at point would never be reachable again.
  , keyed shell "D spends the flags it fired over, and the next D is the point row"
      "d" "press:D press:D" $ \answer -> do
        assertEqual "the flagged row, then the row under the cursor"
                    [("archive", ["r1"]), ("archive", ["r1"])] =<< postedOf answer
        assertEqual "nothing flagged is left" [] =<< textsAt "flagged" answer
        echoIs "and the second press said so"
          "D → org-glance-overview:delete (archived · row)" answer

  , keyed shell "and with marks but no flags it is still the row at point"
      "m m D" "" $ \answer -> do
        assertEqual "the row under the cursor, never the marked pair"
                    [("archive", ["r3"])] =<< postedOf answer
        echoIs "said as the point row" "D → org-glance-overview:delete (archived · row)" answer
        assertEqual "and the marks stand" ["r1", "r2"] =<< textsAt "marked" answer

    -- The other half of that split, unchanged: `set-state' is the command that
    -- DOES read the marked set, so the two selections stay apart on both sides.
  , keyed shell "set-state still runs over the marked set"
      "m m d" "press:C-c press:C-t press:t" $ \answer -> do
        assertEqual "the marked pair, and not the flagged row"
                    [("set-state", ["r1", "r2"])] =<< postedOf answer
        assertEqual "and the flag is still on, unspent" ["r3"]
          =<< textsAt "flagged" answer

  , keyed shell "a server that refuses is counted out and logged"
      "" "refuse press:D" $ \answer -> do
        assertEqual "the command still went" 1 . length =<< postedOf answer
        -- The set name gives way to the bare count: "row" over zero rows would
        -- read as a write that landed.
        echoIs "nothing landed" "D → org-glance-overview:delete (archived · 0)" answer

    -- C-c C-t is a chord, so this also exercises the prefix path: the first key
    -- opens it and the second completes it, over a table with no field focused.
    -- The letter is the whole gesture: the palette IS the confirmation, so
    -- there is no RET behind it.
  , keyed shell "C-c C-t raises the palette and a letter commits on its own"
      "C-c C-t" "press:t" $ \answer -> do
        assertEqual "the palette said what it was setting and over how many"
                    "set state · 1 row" =<< textAt "phead" answer
        assertEqual "one command, over the row at point"
                    [("set-state", ["r1"])] =<< postedOf answer
        assertEqual "as the keyword that letter names" [Just "TODO"]
          =<< keywordsOf answer
        echoIs "the pill names the state" "C-c C-t → org-glance-overview:todo (TODO · 1)" answer
        assertEqual "and the overlay is down" "" =<< textAt "prompt" answer

    -- The reserved-chord rule, and the half no other case can see: `C-t' is in
    -- RESERVED, so a press that opened nothing would be left to the browser.
    -- Completing a bound sequence outranks that, and what says so is the
    -- dispatch claiming BOTH chords.  This is the page's whole guarantee about
    -- the sequence: a browser that owns `Ctrl+T' above the document (Chromium
    -- does) never delivers the second press, and nothing here can reach that.
  , keyed shell "the completing chord is claimed, reserved or not" "C-c C-t" "" $ \answer -> do
        assertEqual "the palette is up" "on" =<< textAt "prompt" answer
        assertEqual "neither chord was left to the browser"
                    ["C-c", "C-t"] =<< textsAt "prevented" answer

    -- RET is nobody's here: it commits in the fallback mode alone, and a reader
    -- who pressed it out of habit gets the palette still standing rather than a
    -- write they did not name.
  , keyed shell "RET commits nothing in letter mode" "C-c C-t" "press:Enter" $ \answer -> do
        assertEqual "no command went" [] =<< postedOf answer
        assertEqual "and the palette is still up" "on" =<< textAt "prompt" answer

    -- `t' raises the palette AND is a letter inside it, and this listener sits
    -- BEHIND the dispatch — so the one press that opened the overlay arrives in
    -- it next.  Two presses, two jobs.
  , testCase "the press that raises the palette is not a key in it" $ do
      onTable "press:t" $ \answer -> do
        assertEqual "the first press only opened it" [] =<< postedOf answer
        assertEqual "and it is up" "on" =<< textAt "prompt" answer
      onTable "press:t press:t" $ \answer -> do
        assertEqual "the second is the letter" [("set-state", ["r1"])]
          =<< postedOf answer
        assertEqual "as TODO" [Just "TODO"] =<< keywordsOf answer

    -- The `ONCE' rule, owed by the palette rather than by the map: a HELD `t'
    -- would open and then commit through what it opened.  The dispatch's list
    -- cannot reach that — it governs rows, and the repeat arrives while every
    -- row is already dead.
  , keyed shell "a held t opens the palette and stops there"
      "" "press:t repeat:t repeat:t" $ \answer -> do
        assertEqual "nothing was written" [] =<< postedOf answer
        assertEqual "and the palette is waiting for a real press" "on"
          =<< textAt "prompt" answer

    -- The exclusivity the letters need: while the palette is up every `table'
    -- row is dead, so `n' moves nothing and `d' — dired's archive flag out
    -- there — is DONE in here.  The gating is `typing()', which the palette
    -- turns on with no field focused at all.
  , keyed shell "the table's own letters are the palette's while it is up"
      "C-c C-t" "press:n press:d" $ \answer -> do
        assertEqual "the cursor never moved" 0 =<< intAt "cursor" answer
        assertEqual "nothing was flagged" [] =<< textsAt "flagged" answer
        assertEqual "and d set a state" [("set-state", ["r1"])] =<< postedOf answer
        assertEqual "the one it names" [Just "DONE"] =<< keywordsOf answer

    -- `*empty*' answers to DEL, which already MEANS take-it-off wherever this
    -- page binds one, and claims no letter — so the a-z pool is the keywords'.
  , keyed shell "the meta entry clears the keyword rather than setting one"
      "C-c C-t" "press:Backspace" $ \answer -> do
        assertEqual "a null keyword" [Nothing] =<< keywordsOf answer
        echoIs "and the pill says so" "C-c C-t → org-glance-overview:todo (*empty* · 1)" answer

  , keyed shell "/ falls back to typing, and RET takes what is left"
      "C-c C-t" "press:/ type:done press:Enter" $ \answer -> do
        assertEqual "the narrowed choice" [Just "DONE"] =<< keywordsOf answer
        echoIs "the pill" "C-c C-t → org-glance-overview:todo (DONE · 1)" answer

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

  , keyed shell "over a marked set it names the whole set" "m m C-c C-t" "press:t" $ \answer -> do
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
  where
    onTable = bootOf shell "" 500 ""

tagKeySpec :: IO T.Text -> TestTree
tagKeySpec shell =
  overBoot shell ":" "" $ \tagged ->
  testGroup "Shell tags"
  [ atBoot tagged ": raises a mount over the row's own tags" $ \answer -> do
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
  , atBoot tagged "a row is the tag, its coverage and the tree's count" $ \answer -> do
        assertEqual "one row per tag" [["web", "all", "40"]] =<< pairsAt "ttags" answer
        assertEqual "the cursor lands on the first" 0 =<< intAt "tat" answer
        assertEqual "and the foot names every key that works"
                    "RET renames · d flags · D removes · + adds · ESC leaves"
          =<< textAt "tfoot" answer

    -- MUTABLE, and stated in the mount: flags for the removal gesture, no marks
    -- (the set this runs over is the TABLE's and was settled before it went up),
    -- no per-row hint and no page.
  , atBoot tagged "the mount is mutable: flags on, marks off, no hints, no page" $ \answer -> do
        assertEqual "marks off" False =<< boolAt "tmarks" answer
        assertEqual "flags on" True =<< boolAt "tflags" answer
        assertEqual "hints off" False =<< boolAt "thints" answer
        assertEqual "no page size, so the whole list is on show" 0
          =<< intAt "tpage" answer
        assertEqual "and the flag's own hint names the two keys that answer it"
                    "d/D remove · u unflag" =<< textAt "tflagHelp" answer

    -- The same rows every other keyed write runs over: the marked set where
    -- there is one, the row at point otherwise.
  , keyed shell "over a marked set it names the whole set, in one request"
      "m m :" "" $ \answer -> do
        assertEqual "the title counts them" "tags · 2 rows" =<< textAt "thead" answer
        assertEqual "and the resolution is one request"
                    ["/tags?ids=r1&ids=r2"] =<< textsAt "tagged" answer

    -- COVERAGE, which is what the letter palette wrote into a muted aside and
    -- this one gives a column: `all' where the set is level, `k/n' where it is
    -- not.  `partly' leaves the third row without `web'.
  , keyed shell "a tag part of the set carries says so in its own cell"
      "" "partly press:m press:m press:m press::" $
        assertEqual "two of the three rows" [["web", "2/3", "40"]] <=< pairsAt "ttags"

    -- The popup browses on the same keys the property panel and the link popup
    -- do, which is `rowStep' in one place.
  , testCase "n and p walk it, in both spellings" $ do
      let two = "press:m press:m press:: press:+ type:work press:Enter"
      onTable two $ \answer -> do
        assertEqual "two tags to walk"
                    [["web", "all", "40"], ["work", "all", "9"]] =<< pairsAt "ttags" answer
        assertEqual "the cursor lands on the one just written" 1 =<< intAt "tat" answer
      onTable (two <> " press:p") $
        assertEqual "up one" 0 <=< intAt "tat"
      onTable (two <> " press:k press:j") $
        assertEqual "and back" 1 <=< intAt "tat"

    -- THE DELETION GESTURE, dired's and the page's: `d' flags, `d' again on the
    -- flagged row IS `D', and the removal goes to every target CARRYING the tag.
  , keyed shell "d flags the tag at point and writes nothing" ":" "press:d" $ \answer -> do
        assertEqual "flagged" ["web"] =<< textsAt "tflagged" answer
        assertEqual "nothing written" [] =<< postedOf answer
        echoIs "and the echo says what a second press does"
          "d → tag-flag (d again removes)" answer

  , keyed shell "a second d removes it from every row carrying it"
      "m m m :" "press:d press:d" $ \answer -> do
        assertEqual "over all three" [("remove-tag", ["r1", "r2", "r3"])]
          =<< postedOf answer
        -- Mounted once and kept, like the panel and the link popup: a write is
        -- a `setRows' over the same instance, never a second mount.
        assertEqual "still one mount" 1 =<< intAt "tmounts" answer
        assertEqual "and a repaint for the raise and for what landed" 2
          =<< intAt "tsets" answer
        assertEqual "as the tag the row named" ["web"] =<< tagsPosted answer
        echoIs "the pill names what landed"
          ": → org-agenda-set-tags (untagged :web: · 3)" answer
        assertEqual "the flag was spent" [] =<< textsAt "tflagged" answer
        assertEqual "and the entry went with it" [] =<< pairsAt "ttags" answer
        assertEqual "leaving the foot naming the one key that still does anything"
                    "nothing tagged here · + adds one · ESC leaves"
          =<< textAt "tfoot" answer

  , keyed shell "D is the same handler without the flagging press" "m m :" "press:D" $ \answer -> do
        assertEqual "both rows" [("remove-tag", ["r1", "r2"])] =<< postedOf answer
        assertEqual "and the popup stands" "on" =<< textAt "tagpop" answer

    -- Several flags are several commands, since a command names ONE tag — each
    -- its own per-file batch of atomic writes — and every one of them is aimed
    -- at the rows carrying THAT tag.  `partly' leaves the third row without
    -- `web', so the two removals name different sets.
  , keyed shell "D over several flagged tags is one command each, over its own rows"
      "" ("partly press:m press:m press:m press:: press:+ type:work press:Enter"
           <> " press:d press:p press:d press:D") $ \answer -> do
        assertEqual "the add, then a removal per flagged tag"
                    [ ("add-tag", ["r1", "r2", "r3"])
                    , ("remove-tag", ["r1", "r2", "r3"])
                    , ("remove-tag", ["r1", "r2"]) ] =<< postedOf answer
        assertEqual "each carrying its own tag" ["work", "work", "web"]
          =<< tagsPosted answer
        assertEqual "and every tag went" [] =<< pairsAt "ttags" answer

  , keyed shell "u takes a flag off before anything is written"
      ":" "press:d press:u" $ \answer -> do
        assertEqual "no flag left" [] =<< textsAt "tflagged" answer
        assertEqual "nothing written" [] =<< postedOf answer
        echoIs "and the echo says which" "u → tag-unflag (flag cleared)" answer

    -- A HELD `d' must not flag a tag and remove it from ONE press, which is the
    -- confirmation the two-press shape exists to be.
  , keyed shell "a held d flags once and never removes"
      ":" "press:d repeat:d repeat:d" $ \answer -> do
        assertEqual "nothing written" [] =<< postedOf answer
        assertEqual "and the flag is still just a flag" ["web"]
          =<< textsAt "tflagged" answer

    -- `+' — the add flow, unchanged: one field over the ADDABLE vocabulary,
    -- which is the tree's tags less the ones every target already carries.
  , keyed shell "+ raises the field over what can be added" ":" "press:+" $ \answer -> do
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
  , keyed shell "a tag some of the set carries is still addable, and says so"
      "" "partly press:m press:m press:m press:: press:+" $
        assertEqual "offered first, wearing its coverage"
          [ ("web", "2/3"), ("archive", ""), ("book", ""), ("work", "") ]
          <=< paletteHints

  , keyed shell "RET there adds the tag to every row lacking it and stays open"
      "m m :" "press:+ type:work press:Enter" $ \answer -> do
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
  , keyed shell "the RET that adds does not open the rename behind it"
      ":" "press:+ type:work press:Enter" $ \answer -> do
        assertEqual "the tag was added" [("add-tag", ["r1"])] =<< postedOf answer
        assertEqual "and no rename opened" False =<< boolAt "trename" answer

  , keyed shell "and a tag the tree has never held is committable all the same"
      ":" "press:+ type:brandnew press:Enter" $ \answer -> do
        assertEqual "the typed line, folded" ["brandnew"] =<< tagsPosted answer
        assertEqual "over the row at point" [("add-tag", ["r1"])] =<< postedOf answer
        assertEqual "and it joins the list under a count of its own"
                    [["web", "all", "40"], ["brandnew", "all", "1"]]
          =<< pairsAt "ttags" answer

  , keyed shell "typing a tag every row has writes nothing and says so"
      "m m m :" "press:+ type:web press:Enter" $ \answer -> do
        assertEqual "no command went" [] =<< postedOf answer
        echoIs "and the pill says why"
          ": → org-agenda-set-tags (:web: is on every row already)" answer

    -- RET IS THE RENAME, through the property panel's edit model: the tag cell
    -- becomes a field over itself, opened on the text it holds.
  , keyed shell "RET opens the tag at point over itself" ":" "press:Enter" $ \answer -> do
        assertEqual "the overlay is up" True =<< boolAt "trename" answer
        assertEqual "holding the tag it opened on" "web" =<< textAt "tname" answer
        assertEqual "and nothing is written by opening it" [] =<< postedOf answer

  , keyed shell "and RET again commits it as one rename-tag"
      "m m :" "press:Enter tname:code press:Enter" $ \answer -> do
        assertEqual "one command, over the rows carrying the old name"
                    [("rename-tag", ["r1", "r2"])] =<< postedOf answer
        assertEqual "carrying both ends" [("web", "code")] =<< renamesPosted answer
        assertEqual "the overlay is gone" False =<< boolAt "trename" answer
        assertEqual "the popup stands" "on" =<< textAt "tagpop" answer
        assertEqual "the row is renamed in place, keeping its coverage"
                    [["code", "all", "2"]] =<< pairsAt "ttags" answer
        echoIs "the pill names what landed"
          ": → org-agenda-set-tags (renamed :web:→:code: · 2)" answer

  , keyed shell "the log names every row a rename landed on"
      "m m :" "press:Enter tname:code press:Enter" $ \answer -> do
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
  , keyed shell "a click under an open rename still renames the tag it opened on"
      "" ("press:m press:m press:: press:+ type:work press:Enter"
           <> " press:Enter tname:renamed click:0 press:Enter") $ \answer -> do
        assertEqual "the add, then one rename over the rows carrying it"
                    [("add-tag", ["r1", "r2"]), ("rename-tag", ["r1", "r2"])]
          =<< postedOf answer
        echoIs "and it names the tag the overlay opened on, not the clicked one"
          ": → org-agenda-set-tags (renamed :work:→:renamed: · 2)" answer
        assertEqual "so the clicked tag stands and the opened one moved"
                    [["web", "all", "40"], ["renamed", "all", "2"]]
          =<< pairsAt "ttags" answer

  , keyed shell "ESC from the + field leaves the popup standing"
      ":" "press:+ type:work press:Escape" $ \answer -> do
        assertEqual "nothing was written" [] =<< postedOf answer
        assertEqual "the field is gone" "" =<< textAt "prompt" answer
        assertEqual "and the popup is still up" "on" =<< textAt "tagpop" answer

  , keyed shell "ESC from the popup closes it, having written nothing"
      ":" "press:Escape" $ \answer -> do
        assertEqual "nothing was written" [] =<< postedOf answer
        assertEqual "the popup is down" "" =<< textAt "tagpop" answer

    -- THE LETTERS ARE GONE.  The state palette keeps them; a tag list is read
    -- rather than committed from memory, so a bare letter here is nobody's.
  , keyed shell "a letter commits nothing, the which-key list having gone"
      ":" "press:w press:a press:b" $ \answer -> do
        assertEqual "no command went" [] =<< postedOf answer
        assertEqual "no value palette either" "" =<< textAt "prompt" answer
        assertEqual "and the popup is still up" "on" =<< textAt "tagpop" answer

    -- While it is up every `table' row is dead, so the keys the popup does not
    -- claim reach nothing at all.
  , keyed shell "the table's own keys are inert while the popup is up"
      ":" "press:m press:M press:U press:t" $ \answer -> do
        assertEqual "nothing was marked" [] =<< textsAt "marked" answer
        assertEqual "nothing was flagged in the table" [] =<< textsAt "flagged" answer
        assertEqual "no command was posted" [] =<< namesOf answer
        assertEqual "and no state palette went up" "" =<< textAt "prompt" answer

    -- And the popup's own keys are dead while its `+' field is up, which is
    -- what the listener's `prompting' guard buys: `d' narrows the field rather
    -- than flagging the tag underneath it.
  , keyed shell "and the popup's own keys are dead under its field"
      ":" "press:+ press:d" $ \answer -> do
        assertEqual "nothing was flagged" [] =<< textsAt "tflagged" answer
        assertEqual "and the field is still up" "narrow" =<< textAt "pmode" answer

  , keyed shell "an untagged set opens on a popup that says so"
      "" "untagged press::" $ \answer -> do
        assertEqual "the popup is up" "on" =<< textAt "tagpop" answer
        assertEqual "with nothing in it" [] =<< pairsAt "ttags" answer
        assertEqual "and the foot naming the way in"
                    "nothing tagged here · + adds one · ESC leaves"
          =<< textAt "tfoot" answer

    -- And `RET' over it opens NOTHING and names the command that had no row —
    -- the one guard both browsing popups raise their overlay through, so an
    -- empty list cannot get a field laid over a row that is not there.
  , keyed shell "and RET over an empty one opens nothing and says so"
      "" "untagged press:: press:Enter" $ \answer -> do
        assertEqual "no overlay" False =<< boolAt "trename" answer
        echoIs "the pill names the command" "RET → org-rename-tag (no tag)" answer
        assertEqual "and nothing was written" [] =<< postedOf answer

  , keyed shell "a refused resolution raises nothing and says so"
      "" "refuse press::" $ \answer -> do
        assertEqual "no popup" "" =<< textAt "tagpop" answer
        assertEqual "and the log named it" (Just "tags failed: GET /tags?ids=<row id>")
          =<< lastLog answer

  , keyed shell "and a set the store knows no row of raises none either"
      "" "unknownrows press::" $ \answer -> do
        assertEqual "no popup" "" =<< textAt "tagpop" answer
        echoIs "the pill says which" ": → org-agenda-set-tags (no such row)" answer

    -- THE LIST REFRESHES FROM THE ANSWER, never from a re-read: `/command' does
    -- not write the store — the watch does, a debounce later — so asking
    -- `/tags' again would report what the files said BEFORE the write.  The
    -- fake store still says every row carries `web' when this reads the list.
  , keyed shell "the list is what landed, and the store is not asked twice"
      "m m :" "press:d press:d" $ \answer -> do
        assertEqual "the one resolution, and no second" ["/tags?ids=r1&ids=r2"]
          =<< textsAt "tagged" answer
        assertEqual "and the tag is gone from a list nobody re-read" []
          =<< pairsAt "ttags" answer
  ]

-- | The pair each posted @rename-tag@ carried.
  where
    onTable = bootOf shell "" 500 ""

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
    -- `+' OPENS WITH THE TAG, and `*empty*' leads the list: an immediate RET is
    -- the untagged inbox path exactly as it was before the chain existed.
  [ keyed shell "+ asks which tag first, and *empty* is the inbox"
      "+" "press:Enter type:milk press:Enter" $ \answer -> do
        assertEqual "the vocabulary came off the server" ["/capture"]
          =<< textsAt "capturing" answer
        assertEqual "one capture, naming no rows" ["capture"] =<< namesOf answer
        assertEqual "carrying the line as typed" ["milk"] =<< capturedOf answer
        assertEqual "and no tag with it" [Nothing] =<< taggedOf answer
        echoIs "the pill names the file it landed in"
          "+ → org-glance-overview:capture (captured · /o/inbox.org)" answer
        assertEqual "and the log names the headline"
                    (Just "headline \"milk\" captured into /o/inbox.org")
          =<< lastLog answer
        assertEqual "the overlay is down" "" =<< textAt "prompt" answer

    -- A TAG WITH A TEMPLATE asks that template's own prompts, in the order the
    -- server named them, and the answers ride in `fields'.  This page holds no
    -- template grammar: what it asks is what `/capture?tag=' said to ask.
  , keyed shell "a tag's template asks its prompts, one field at a time"
      "+" "type:book press:Enter type:Herbert press:Enter type:Dune press:Enter"
      $ \answer -> do
        assertEqual "the tag was resolved before the reader was asked anything"
                    ["/capture", "/capture?tag=book"] =<< textsAt "capturing" answer
        assertEqual "one capture" ["capture"] =<< namesOf answer
        assertEqual "the line as typed" ["Dune"] =<< capturedOf answer
        assertEqual "under the tag it was filed with" [Just "book"] =<< taggedOf answer
        assertEqual "and the template's ask answered"
                    [Just "Herbert"] =<< answeredOf "Author" answer
        echoIs "the pill names the tag rather than a file"
          "+ → org-glance-overview:capture (captured · :book:)" answer

    -- A tag NOBODY configured asks nothing: the server answers no prompts and
    -- the chain goes straight to the line.
  , keyed shell "a tag with no template goes straight to the line"
      "+" "type:web press:Enter type:milk press:Enter" $ \answer -> do
        assertEqual "resolved all the same" ["/capture", "/capture?tag=web"]
          =<< textsAt "capturing" answer
        assertEqual "the line as typed" ["milk"] =<< capturedOf answer
        assertEqual "under the tag" [Just "web"] =<< taggedOf answer

    -- ESC ANYWHERE ENDS THE WHOLE CHAIN with nothing sent, and it is the absence
    -- of machinery rather than a rule: a step that is abandoned never calls the
    -- one behind it.
  , keyed shell "ESC at the tag prompt writes nothing"
      "+" "press:Escape" $ \answer -> do
        assertEqual "no command went" [] =<< namesOf answer
        assertEqual "the overlay is down" "" =<< textAt "prompt" answer

  , keyed shell "ESC at a template's own prompt writes nothing"
      "+" "type:book press:Enter type:Herbert press:Escape" $ \answer -> do
        assertEqual "the tag was resolved" ["/capture", "/capture?tag=book"]
          =<< textsAt "capturing" answer
        assertEqual "no command went" [] =<< namesOf answer
        assertEqual "the overlay is down" "" =<< textAt "prompt" answer

  , keyed shell "and ESC at the line leaves it having written nothing"
      "+" "press:Enter type:milk press:Escape" $ \answer -> do
        assertEqual "no command went" [] =<< namesOf answer
        assertEqual "the overlay is down" "" =<< textAt "prompt" answer

  , keyed shell "an empty line captures nothing and says so"
      "+" "press:Enter press:Enter" $ \answer -> do
        assertEqual "no command went" [] =<< namesOf answer
        echoIs "the pill says why" "+ → org-glance-overview:capture (nothing to capture)" answer

  , keyed shell "a refused capture is one cmd error line"
      "" "refuse press:+ press:Enter type:milk press:Enter" $ \answer -> do
        assertEqual "the command still went" ["capture"] =<< namesOf answer
        assertEqual "and the log carries the server's own words"
                    (Just "capture failed: #+GLANCE_CAPTURE_TARGET: /x.org is an absolute path")
          =<< lastLog answer

    -- THE LANDING: the answer names the row the write made, and point goes to it
    -- when the watch delivers it.  `land''s ordinary rule and no second one — a
    -- row the view has not got leaves the cursor where it stands.
  , keyed shell "the captured row is where point lands when it arrives"
      "+" "press:Enter type:milk press:Enter frame:upsert=r3 wait:300" $ \answer ->
        assertEqual "point is on the row the capture made" (Just "r3")
          =<< maybeTextAt "selected" answer

    -- THE WHOLE CHAIN FOR A TAGGED CAPTURE, which is the one the daemon's nudge
    -- unblocked: the blob it writes sits under directories fsnotify never
    -- entered, so before the nudge no frame was coming and this landing had
    -- nothing to land on until a restart.  Every link is asserted here — the
    -- tag resolved, the command posted under it, the answer's id kept, the
    -- frame delivering that very row, and point moving off the boot's row one
    -- onto it.
  , keyed shell "a tagged capture lands point on the blob when the watch delivers it"
      "+" "type:book press:Enter type:Herbert press:Enter type:Dune press:Enter\
          \ frame:upsert=r3 wait:300" $ \answer -> do
        assertEqual "the tag was resolved off the server"
                    ["/capture", "/capture?tag=book"] =<< textsAt "capturing" answer
        assertEqual "one capture, under that tag" [Just "book"] =<< taggedOf answer
        assertEqual "point left the row the boot landed on" (Just "r3")
          =<< maybeTextAt "selected" answer
        assertEqual "which is the third row" 2 =<< intAt "cursor" answer
        -- The boot is the default view, so it is FILTERED: the frame schedules
        -- the refetch rather than splicing, and the landing still holds across
        -- it because `arriving' is spent at whichever of the three doors comes.
        assertEqual "nothing was spliced under the filter" [] =<< textsAt "spliced" answer

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

  , keyed shell "a date goes to the server as the text that was typed"
      "C-c C-s" "type:+3d press:Enter" $ \answer -> do
        assertEqual "one command, over the row at point"
                    [("set-planning", ["r1"])] =<< postedOf answer
        assertEqual "with the keyword and the date beside it"
                    [("SCHEDULED", Just "+3d")] =<< plannedOf answer
        echoIs "the pill names what was asked for"
          "C-c C-s → org-glance-overview:schedule (+3d · 1)" answer
        assertEqual "and the log names the row"
                    (Just "headline \"one\" scheduled +3d") =<< lastLog answer

    -- An empty line is the clear: the entry comes off, and the server drops the
    -- line with it when it was the last one.
  , keyed shell "an empty line clears the entry" "C-c C-d" "press:Enter" $ \answer -> do
        assertEqual "a null date" [("DEADLINE", Nothing)] =<< plannedOf answer
        echoIs "the pill says which"
          "C-c C-d → org-glance-overview:deadline (cleared · 1)" answer
        assertEqual "and so does the log"
                    (Just "headline \"one\" deadline cleared") =<< lastLog answer

    -- The marked set, like every other command that names rows.
  , keyed shell "over a marked set it names the whole set"
      "m m C-c C-s" "type:today press:Enter" $ \answer -> do
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

-- | The tag each posted @capture@ filed under, 'Nothing' for the inbox path —
-- a SPARSE field, absent rather than null where there is none.
taggedOf :: Value -> IO [Maybe T.Text]
taggedOf = traverse (sparseTextAt "tag") <=< argsOf

-- | What each posted command answered for the template prompt NAME, 'Nothing'
-- where its args carried no @fields@ at all or no answer for that one.
answeredOf :: T.Text -> Value -> IO [Maybe T.Text]
answeredOf name = traverse one <=< argsOf
  where one v = maybe (pure Nothing) (sparseTextAt name) =<< sparseAt "fields" v

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
openKeySpec shell =
  overBoot shell "o" "" $ \opened ->
  overBoot shell "o" "press:Enter lurl:https://new.example press:Enter" $ \committed ->
  testGroup "Shell open"
  [ atBoot opened "o asks about the row at point" $
        assertEqual "one request, naming the row" ["/links?id=r1"] <=< textsAt "linked"

  , keyed shell "! is the same command, and reaches it the same way" "!" "" $ \answer -> do
        assertEqual "the same request" ["/links?id=r1"] =<< textsAt "linked" answer
        -- Raising a palette is not a landing, so the pill still carries what
        -- `run\' says of the row — the command and its help — the way it does
        -- while the state palette is up.  The landing is the letter.
        assertEqual "under the same name"
                    ("! → org-glance-overview:open · open links: the row here,"
                       <> " the element in the sheet; several list them")
          =<< textAt "echo" answer

  , keyed shell "one link opens without asking" "" "onelink press:o" $ \answer -> do
        assertEqual "the tab, with the opener cut"
                    [("https://one.example/a", "_blank", "noopener")] =<< openedOf answer
        assertEqual "no palette went up" "" =<< textAt "prompt" answer
        echoIs "the pill names the command and what it opened"
          "o → org-glance-overview:open (First reference)" answer
        assertEqual "and the log names the target"
                    (Just "link \"https://one.example/a\" opened") =<< lastLog answer

  , keyed shell "no link at all is a refusal that names the command"
      "" "nolinks press:o" $ \answer -> do
        assertEqual "nothing opened" [] =<< openedOf answer
        assertEqual "no palette either" "" =<< textAt "prompt" answer
        echoIs "and the pill says why" "o → org-glance-overview:open (no links)" answer

    -- Several is the POPUP, and the popup is the page's THIRD table-view mount.
    -- A list of links is a list of RECORDS — a kind, a name, a destination —
    -- and reading it is how a reader picks one, which is the browse gesture a
    -- which-key letter is the wrong shape for.
  , atBoot opened "several raise the popup, which is a table-view mount" $ \answer -> do
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
  , atBoot opened "the rows are the answer, type and all" $ \answer -> do
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
  , atBoot opened "the mount is read-only: no marks, no flags, no hints, no page" $ \answer -> do
        assertEqual "marks off" False =<< boolAt "lmarks" answer
        assertEqual "flags off" False =<< boolAt "lflags" answer
        assertEqual "hints off" False =<< boolAt "lhints" answer
        assertEqual "and no page size, so the whole list is on show" 0
          =<< intAt "lpage" answer

    -- The whole point of `typing()' counting the popup: every `table' row is
    -- dead under it, so the keys that WRITE do nothing at all while a reader is
    -- browsing links.  Asserted over the four that would otherwise cost a file.
  , keyed shell "the write keys are inert while the popup is up"
      "o" "press:d press:D press:m press:M press:u press:U" $
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
  , keyed shell "o opens the link at point and closes the popup"
      "o" "press:n press:o" $ \answer -> do
        assertEqual "the second one" [("https://two.example/b", "_blank", "noopener")]
          =<< openedOf answer
        assertEqual "the popup is down" "" =<< textAt "popup" answer
        echoIs "the pill names it by its description"
          "o → org-glance-overview:open (Second reference)" answer

  , keyed shell "ESC leaves it having opened nothing" "o" "press:Escape" $ \answer -> do
        assertEqual "nothing opened" [] =<< openedOf answer
        assertEqual "the popup is down" "" =<< textAt "popup" answer

    -- `RET' EDITS the link at point in place: the row's own title and url cells
    -- become fields over themselves, which is the property panel's edit model
    -- and the third surface to declare a shape for it.  The type cell is the
    -- server's word for the target and never opens.
  , keyed shell "RET opens the link at point over its own two cells"
      "o" "press:Enter" $ \answer -> do
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
  , atBoot committed "RET commits the span the server gave, under the digest it came with" $
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
  , atBoot committed "a description nobody moved is not sent at all" $
        \answer -> do
          [cmd] <- listAt "commands" answer
          args <- field "args" cmd
          assertEqual "no desc field" ["span", "target"] . sort =<< fieldsOf args

  , keyed shell "and one the reader emptied is the null that takes it off"
      "o" "press:Enter ltitle: press:Enter" $ \answer -> do
        [cmd] <- listAt "commands" answer
        args <- field "args" cmd
        assertEqual "a null description" Null =<< field "desc" args
        assertEqual "under the target it already had" "https://one.example/a"
          =<< textAt "target" args

  , keyed shell "a description typed over the old one is sent as it was typed"
      "o" "press:Enter ltitle:renamed press:Enter" $
        \answer -> do
          [cmd] <- listAt "commands" answer
          args <- field "args" cmd
          assertEqual "the text" "renamed" =<< textAt "desc" args

    -- The popup CLOSES on the press, both outcomes alike, which is `o'\''s own
    -- rule — and it has to: the spans it holds describe the file as it was, and
    -- the write has just moved it.  `o' again is one keystroke and comes back
    -- with fresh ones.
  , atBoot committed "the commit closes the popup, and the log names both ends" $
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

  , keyed shell "a link nobody changed costs no write" "o" "press:Enter press:Enter" $ \answer -> do
        assertEqual "nothing posted" [] =<< namesOf answer
        assertEqual "the popup is down all the same" "" =<< textAt "popup" answer
        echoIs "and the pill says so" "o → org-glance-overview:open (unchanged)" answer

  , keyed shell "an emptied target is refused here, since a link points somewhere"
      "o" "press:Enter lurl: press:Enter" $ \answer -> do
        assertEqual "nothing posted" [] =<< namesOf answer
        echoIs "the pill says why"
          "o → org-glance-overview:open (a link points somewhere)" answer

  , keyed shell "ESC over an open link puts it back and leaves the popup standing"
      "o" "press:Enter lurl:https://new.example press:Escape" $
        \answer -> do
          assertEqual "the overlay is gone" False =<< boolAt "lopen" answer
          assertEqual "the popup is not" "on" =<< textAt "popup" answer
          assertEqual "nothing was posted" [] =<< namesOf answer
          echoIs "and the pill says the link stands"
            "ESC → keyboard-quit (link unchanged)" answer

  , keyed shell "and a second ESC closes the popup" "o" "press:Enter press:Escape press:Escape" $
        assertEqual "down" "" <=< textAt "popup"

    -- THE HAZARD THE SHARED MECHANISM ANSWERS, on the third surface: no KEY can
    -- move the cursor under an open field, but a MOUSE CLICK can, and a commit
    -- that re-read the cursor would send the text typed for one link against
    -- another link's span.  The commit is handed the row the overlay OPENED
    -- over, so the click moves nothing.
  , keyed shell "a click under an open link cannot redirect the write"
      "o" "press:Enter lurl:https://new.example click:2 press:Enter" $
        \answer -> do
          [cmd] <- listAt "commands" answer
          args <- field "args" cmd
          assertEqual "the span is the one the overlay opened over"
            [10, 48] =<< spanOf args
          assertEqual "and the target is what was typed for it" "https://new.example"
            =<< textAt "target" args

    -- A held key must not be a browser tab per repeat, which is why the command
    -- is on the ONCE list beside the writes.
  , keyed shell "a held o asks once" "o" "repeat:o repeat:o repeat:o" $
        assertEqual "one request" ["/links?id=r1"] <=< textsAt "linked"

  , keyed shell "a refused answer is one cmd error line and no popup"
      "" "refuse press:o" $ \answer -> do
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
  , keyed shell "a single link that is not http(s) opens nothing and says so"
      "" "onemailto press:o" $ \answer -> do
        assertEqual "no tab" [] =<< openedOf answer
        assertEqual "and no popup, since one link never raises one" ""
          =<< textAt "popup" answer
        echoIs "the pill names the command and the refusal"
          "o → org-glance-overview:open (link type not implemented)" answer
        assertEqual "and the log warns, naming the target"
                    (Just "link type not implemented: mailto:t@example.org")
          =<< lastLog answer

    -- The popup still LISTS every link the row holds — that is what teaches a
    -- reader what is in the entry — and `o' is where the answer is given.
  , keyed shell "an o on a non-http row refuses the same way"
      "o" "press:n press:n press:o" $ \answer -> do
        assertEqual "nothing opened" [] =<< openedOf answer
        assertEqual "the popup is down all the same" "" =<< textAt "popup" answer
        echoIs "the pill says why"
          "o → org-glance-overview:open (link type not implemented)" answer

  , keyed shell "and an http row beside it still opens" "o" "press:o" $
        assertEqual "the first one" [("https://one.example/a", "_blank", "noopener")]
          <=< openedOf

    -- Every type the server derives, drawn.  The badge column carries whatever
    -- word came back — the six the palette declares hues for and the catch-all
    -- alike — because a type this page has never seen is still a fact about the
    -- link and hiding it would teach less than showing it uncoloured.
  , keyed shell "every type the server derives reaches the badge cell" "" "everytype press:o" $
        assertEqual "one word per row"
          ["https", "http", "glance", "mailto", "id", "file", "other"]
          . map head <=< pairsAt "llinks"

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
  [ keyedAt shell "?q=" 500 "applies its query the way g applies the tree's default"
      "a" "" $ \answer -> do
        assertEqual "the boot's two, then the remount's one"
          [ "/headlines?limit=100", "/headlines"
          , "/headlines?q=state%3A*active*%20-planned%3A*empty*%20sort%3Ascheduled" ]
          =<< textsAt "asked" answer
        urlIs "and the URL it settles on is that query"
          "?q=state%3A*active*+-planned%3A*empty*+sort%3Ascheduled" answer

    -- The order is IN the query, so the server answers page one in it and the
    -- renderer reads the chain off the same string.  Nothing is asked of the
    -- handle: a canned view that had to call for its order could state one the
    -- query it applied did not.
  , keyedAt shell "?q=" 500 "the rows land in scheduled order, and the query is what says so"
      "a" "" $ \answer -> do
        assertEqual "the chain the query named" [("scheduled", True)]
          =<< chainOf answer
        assertEqual "and no sort was asked of the renderer" 0
          =<< intAt "sortCalls" answer

    -- DEL walks out of the order the way it walks out of the filter: the sort
    -- token is the query's last one, so one press takes it off and the answer
    -- comes back in the view's own order.
  , keyedAt shell "?q=" 500 "and DEL takes the order back off, one token like any other"
      "a" "press:Backspace" $ \answer -> do
        urlIs "the query the strip left" "?q=state%3A*active*+-planned%3A*empty*" answer
        assertEqual "asked for without the order"
                    (Just "/headlines?q=state%3A*active*%20-planned%3A*empty*")
          . lastOf =<< textsAt "asked" answer

  , keyedAt shell "?q=" 3 "and the pill names the command and the count the server answered"
      "a" "" $
        echoIs "counted by the server, not by the page it painted"
          "a → org-glance-agenda (agenda · 3 rows)"

  , keyedAt shell "?q=" 1 "one row is one row" "a" "" $
        echoIs "singular" "a → org-glance-agenda (agenda · 1 row)"

    -- An asset with no sort calls at all applies the same view: the order is a
    -- token of the query, so there is nothing for this page to ask for and
    -- nothing to feature-detect on the way in.  What an old asset loses is the
    -- ORDER, which the server still answers in.
  , keyedAt shell "?q=" 500 "an asset without a programmatic sort still applies the view"
      "" "sortless press:a" $ \answer -> do
        assertEqual "no sort was asked for" Nothing =<< sortOf answer
        urlIs "the query still went, order and all"
          "?q=state%3A*active*+-planned%3A*empty*+sort%3Ascheduled" answer

    -- `g' is the way home, and it is the way home from here like anywhere else.
  , keyedAt shell "?q=" 500 "g returns to the tree's default view" "a g" "" $
        urlIs "the last query asked for is the default's" "?q=state%3A*active*"

    -- The landing is armed for ONE boot: a second remount that nobody asked an
    -- agenda of must not re-sort and must not echo a count.
  , keyedAt shell "?q=" 500 "the landing is spent by the boot it was armed for"
      "a" "close:view-changed" $ \answer -> do
        echoIs "the remount behind the close echoed no agenda"
          "a → org-glance-agenda (agenda · 500 rows)" answer
        -- The echo pill's FINAL text cannot see this regression: it is
        -- last-writer-wins, so an unspent landing re-runs the agenda and writes
        -- the very string above.  Neither can the fetches — the remount
        -- revalidates the APPLIED query either way — nor `sortCalls', the order
        -- being a token of the query rather than a call.  The one trace a
        -- second run leaves is a second WRITE, so the assertion is over the
        -- echo's history.
        wrote <- textsAt "echoes" answer
        assertEqual ("the agenda landed once: " <> show wrote)
                    1 (length (filter ("(agenda · " `T.isInfixOf`) wrote))

  , keyedAt shell "?q=" 500 "a held a remounts once" "a" "repeat:a repeat:a repeat:a" $
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
  [ keyed shell "@ applies a ref view over the row at point and leaves a crumb"
      "@" "" $ \answer -> do
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
  , keyedAt shell "?q=" 500 "@ out of an empty query leaves no crumb, and DEL is still the way back"
      "@" "" $ \answer -> do
        assertEqual "the view is applied all the same"
          [ "/headlines?limit=100", "/headlines", "/headlines?q=ref%3Ar1&limit=1"
          , "/headlines?q=ref%3Ar1" ]
          =<< textsAt "asked" answer
        assertEqual "and the strip carries no chip" [] =<< textsAt "crumbs" answer

    -- Pressed as an ACT, so the drill's remount has landed before the key that
    -- walks back out of it: with no crumb to pop, DEL has only the mounted
    -- query to strip and the old mount's was empty.
  , keyedAt shell "?q=" 500 "and that DEL lands on all rows, first row selected"
      "@" "press:Backspace" $ \answer -> do
        url <- textAt "url" answer
        assertBool ("the filter is cleared rather than popped: " <> T.unpack url)
                   ("?q=&" `T.isPrefixOf` url || url == "?q=")
        echoIs "named as the clearing it is" "DEL → filter-drop-token (filter cleared)" answer
        assertEqual "on the first row" (Just "r1") =<< maybeTextAt "selected" answer

    -- ZERO REFERENCES IS NO JUMP.  The drill is PROBED — the same query under
    -- `limit=1', which is a count — and nothing pointing at the row leaves the
    -- table, the filter and the trail exactly where they were: an empty view is
    -- the one landing a reader can read nothing off, and walking back out of it
    -- costs a keystroke to undo a keystroke.
  , keyed shell "@ onto a row nothing refers to applies no view at all"
      "" "noreferences press:@" $ \answer -> do
        assertEqual "the probe, and nothing behind it"
          [ "/headlines?q=state%3A*active*&limit=100", "/headlines?q=state%3A*active*"
          , "/headlines", "/headlines?q=ref%3Ar1&limit=1" ]
          =<< textsAt "asked" answer
        urlIs "the view standing is the one the reader was on" "?q=state%3A*active*" answer
        assertEqual "no crumb was pushed" [] =<< textsAt "crumbs" answer
        echoIs "the pill says why nothing moved"
          "@ → org-glance-overview:relations (no references to \"one\")" answer
        assertEqual "and the log names the headline"
                    (Just "no references to headline \"one\"") =<< lastLog answer

  , keyedAt shell "" 3 "the pill names the command, the row and the count" "@" "" $
        echoIs "counted by the server"
          "@ → org-glance-overview:relations (references of \"one\" · 3)"

  , keyed shell "the trail and its labels ride in the URL beside the query" "@" "" $ \answer -> do
        url <- textAt "url" answer
        assertBool ("the ref query is applied: " <> T.unpack url)
                   ("q=ref%3Ar1" `T.isInfixOf` url)
        assertBool ("the trail rides with it: " <> T.unpack url)
                   ("crumbs=" `T.isInfixOf` url)

    -- The ladder's second rung.  The drill left `ref:r1' as the whole query, so
    -- ONE DEL empties it and walks back out — a step out and a step back rather
    -- than a step and a half.
  , keyed shell "DEL on an emptied query pops the crumb and applies it"
      "@" "press:Backspace" $ \answer -> do
        urlIs "back on the view the drill left" "?q=state%3A*active*" answer
        assertEqual "and the trail is spent" [] =<< textsAt "crumbs" answer
        echoIs "the pill names where it landed"
          "DEL → filter-drop-token (back to state:*active*)" answer

    -- The first rung is unchanged: while the query still has tokens, DEL takes
    -- one off and the trail is not touched.  A REFINEMENT edits the query in
    -- place, so undoing one is a token rather than a crumb — which is the whole
    -- of what makes the two grains one key.
  , keyedAt shell ("?q=ref%3Ar1%20tanik&crumbs="
                    <> bootedTrail) 500 "DEL over a refined drill strips a token before it pops"
      "Backspace" ""
        $ \answer -> do
        assertEqual "the crumb is still standing" ["everything"]
          =<< textsAt "crumbs" answer
        url <- textAt "url" answer
        assertBool ("the ref token survived the strip: " <> T.unpack url)
                   ("q=ref%3Ar1" `T.isInfixOf` url)

    -- With no trail behind it the key does what it always did, which is the
    -- rung that was there before the ladder had a second one.
  , keyed shell "DEL with an empty stack clears the filter as it always has"
      "Backspace" "" $ \answer -> do
        urlIs "the cleared query, present and empty" "?q=" answer
        echoIs "the pill says so" "DEL → filter-drop-token (filter cleared)" answer

  , keyed shell "g is home and throws the trail away" "@" "press:g" $ \answer -> do
        assertEqual "no crumbs left" [] =<< textsAt "crumbs" answer
        urlIs "and the URL is the default view, with no trail on it"
          "?q=state%3A*active*" answer

    -- A `view-changed' close rebuilds the mount, and `setView' drops the crumbs
    -- with the world they described.  The URL is what puts them back.
  , keyed shell "a remount restores the trail and the labels"
      "@" "close:view-changed" $ \answer -> do
        -- The boot, the drill's own remount, and the one the close caused.
        assertEqual "mounted three times" 3 =<< intAt "mounts" answer
        assertEqual "the crumb survived the remount" ["state:*active*"]
          =<< textsAt "crumbs" answer
        assertEqual "and the ref view is still what is applied"
                    "?q=ref%3Ar1" . T.takeWhile (/= '&') =<< textAt "url" answer

    -- And the restored trail is LIVE rather than decorative: DEL walks back
    -- down it after the remount the same way it would have before one.
  , keyed shell "and the trail a remount put back can still be walked"
      "@" "close:view-changed press:Backspace" $ \answer -> do
        urlIs "back on the view the drill left" "?q=state%3A*active*" answer
        assertEqual "the trail is spent" [] =<< textsAt "crumbs" answer

  , keyedAt shell ("?q=ref%3Ar1&crumbs="
                    <> bootedTrail) 500 "a booted trail is restored from the URL and can be walked back"
      "" "" $
        assertEqual "the trail the address bar carried" ["everything"]
          <=< textsAt "crumbs"

  , keyedAt shell ("?q=ref%3Ar1&crumbs="
                    <> bootedTrail) 500 "and DEL walks that booted trail back out"
      "Backspace" "" $ \answer -> do
        urlIs "landed on the crumb's own query" "?q=" answer
        echoIs "naming it by its label" "DEL → filter-drop-token (back to everything)" answer

    -- A crumb remembers the SELECTION it was pushed from, so walking back puts
    -- the cursor where the reader left it rather than at the top of a view they
    -- had moved down into.  It rides BESIDE the trail: the renderer's `crumbOf'
    -- keeps a crumb's label and query and drops everything else, so a selection
    -- put inside one would never come back out of `getCrumbs()'.
  , keyed shell "a pop puts the cursor back on the row the drill was launched from"
      "n n @" "press:Backspace" $ \answer -> do
        rowIs "back on the third row" "r3" answer
        assertEqual "and the trail is spent" [] =<< textsAt "crumbs" answer

  , keyed shell "and the column it was in, when one was set"
      "n f @" "press:Backspace" $ \answer -> do
        rowIs "the row" "r2" answer
        assertEqual "and the cell it was on" 0 =<< intAt "col" answer

    -- Never force a missing id: a row the popped answer no longer holds falls
    -- through to the ordinary landing rather than being selected in absentia.
  , keyed shell "a remembered row the answer lost falls back to the first row"
      "n n @" "rows:2 press:Backspace" $ \answer -> do
        rowIs "the store lost r3, so the landing is row one" "r1" answer

  , keyed shell "the remembered selection rides in the URL with the trail" "n @" "" $ \answer -> do
        url <- textAt "url" answer
        assertBool ("the pair is carried: " <> T.unpack url)
                   ("sels" `T.isInfixOf` url)

    -- Every application that is NOT a pop lands on the first row of the answer:
    -- `g' here, and a commit below, which repaints rather than remounting and
    -- would otherwise leave the cursor on a row the answer may not hold.
  , keyed shell "g lands on the first row rather than where the reader was" "n n g" "" $
        rowIs "row one" "r1"

    -- A commit REPAINTS rather than remounting, so without the rule the cursor
    -- would sit wherever it was over a set that may not hold that row at all.
    -- `DEL' is the commit this suite can drive: it strips a token and commits
    -- what is left, which is the same door a palette commit goes through.
  , keyedAt shell "?q=tanik%20web" 500 "a commit that repaints lands on the first row too"
      "n n Backspace" "" $ \answer -> do
        rowIs "row one" "r1" answer
        urlIs "and it was a strip rather than a pop" "?q=tanik" answer

    -- A held `@' is a remount per repeat, each leaving a crumb behind, which is
    -- why the command is on the ONCE list beside the other view keys.
  , keyed shell "a held @ drills once" "@" "repeat:@ repeat:@" $
        assertEqual "one crumb, not three" ["state:*active*"] <=< textsAt "crumbs"

    -- An asset predating the trail is told so rather than being made to apply a
    -- view the reader would have no way back out of.
  , keyed shell "an asset with no crumbs refuses the drill and stays put"
      "" "crumbless press:@" $ \answer -> do
        assertEqual "the boot's fetches and no more"
          [ "/headlines?q=state%3A*active*&limit=100", "/headlines?q=state%3A*active*"
          , "/headlines" ] =<< textsAt "asked" answer
        echoIs "and the pill says which call is missing"
          "@ → org-glance-overview:relations (this table-view.js has no crumbs)" answer
  ]

-- | A trail as an address bar carries it: one crumb standing for the unfiltered
-- view, and the label the live @ref:@ chip wears.
bootedTrail :: T.Text
bootedTrail = "%7B%22trail%22%3A%5B%7B%22label%22%3A%22everything%22%2C%22query%22%3A%22%22%7D%5D%2C%22labels%22%3A%7B%7D%7D"

-- | The same trail with the SELECTION the crumb was pushed from beside it,
-- which is what a pop puts back — and what the boot's own landing must not
-- write over.  @landingSpec@ reads it.
bootedSels :: T.Text
bootedSels = "%7B%22trail%22%3A%5B%7B%22label%22%3A%22everything%22%2C%22query%22%3A%22%22%7D%5D%2C%22labels%22%3A%7B%7D%2C%22sels%22%3A%5B%7B%22id%22%3A%22r3%22%2C%22col%22%3Anull%7D%5D%7D"

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
whichKeySpec shell =
  overBoot shell "C-c C-t" "" $ \palette ->
  testGroup "Shell which-key"
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
  , atBoot palette "the table draws one row per source, keywords in their cells" $ \answer -> do
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
  , keyed shell "a set spanning two tags shows both tag sources" "" "twotags press:t" $
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
  , atBoot palette "the letter is marked in the word, and only *empty* wears a token"
      $ \answer -> do
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
    -- DEL ERASES THE LAST STRUCTURE STANDING, and over a popup that is the
    -- popup: neither the link list nor the tag list has an inner ladder, so the
    -- key closes it exactly as ESC does.  The backspace's own rhyme, one surface
    -- in from the table's marks-then-token-then-crumb ladder.
  , testCase "DEL closes a popup that has nothing inside it to erase" $ do
      bootOf shell "" 500 "o" "press:Backspace" $ \answer -> do
        assertEqual "the link popup is gone" "" =<< textAt "popup" answer
        echoIs "and the pill names the function that ran" "DEL → keyboard-quit" answer
      bootOf shell "" 500 ":" "press:Backspace" $ \answer -> do
        assertEqual "and so is the tag popup" "" =<< textAt "tagpop" answer
        echoIs "under the same line" "DEL → keyboard-quit" answer
      -- IN NAV ALONE.  Inside an OPEN edit the key is the field's own erase: the
      -- page declines it, which is what leaves the browser's default standing.
      bootOf shell "" 500 "o" "press:Enter press:Backspace" $ \answer -> do
        assertEqual "the popup stands" "on" =<< textAt "popup" answer
        assertEqual "with its edit still open" True =<< boolAt "lopen" answer
        assertEqual "and the target lost a character"
                    "https://one.example/" =<< textAt "lurl" answer
        assertBool "the key was left to the field"
          . notElem "Backspace" =<< textsAt "prevented" answer
      bootOf shell "" 500 ":" "press:Enter press:Backspace" $ \answer -> do
        assertEqual "the tag popup stands" "on" =<< textAt "tagpop" answer
        assertEqual "with its rename still open" True =<< boolAt "trename" answer
        assertEqual "and the name lost a character" "we" =<< textAt "tname" answer

  , keyed shell "DEL fires nothing in a palette that has no clear"
      ":" "press:+ press:Backspace" $ \answer -> do
        assertEqual "no command went" [] =<< postedOf answer
        assertEqual "the field is still up" "narrow" =<< textAt "pmode" answer
        assertEqual "and the popup under it is untouched" [["web", "all", "40"]]
          =<< pairsAt "ttags" answer

  , atBoot palette "each keyword wears its own badge colour, where there is one" $
        assertEqual "TODO, DONE and READING have badges; LATER and READ do not"
          [ ("[T]ODO", "#e0af68"), ("[D]ONE", "#73daca"), ("[R]EADING", "#bb9af7") ]
          <=< paletteHues

    -- The overlay goes up on the keypress and the answer fills it, so the guard
    -- that makes the raising press not a letter is unmoved and ESC works from
    -- the moment the key lands.
  , keyed shell "the palette is up before the resolution is" "" "stall press:t" $ \answer -> do
        assertEqual "raised" "on" =<< textAt "prompt" answer
        assertEqual "with a line saying what it is waiting for"
                    [("pnone", "", ["resolving…"], [])] =<< paletteOf answer

    -- The fallback is FLAT — every entry, whichever source it came from, under
    -- no table at all — and drops the token column outright: no letter commits
    -- there, so drawing one would be a lie about what typing it does.  The
    -- cursor is this list's own, and it opens on the first row.
  , keyed shell "/ flattens the table, drops the letters and names its own keys"
      "C-c C-t" "press:/" $ \answer -> do
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

  , keyed shell "typing there narrows to what matches" "C-c C-t" "press:/ type:ead" $
        assertEqual "the two book keywords hold it, nothing else does"
          [ ("pe pat", "", ["READING"], [])
          , ("pe",     "", ["READ"],    []) ] <=< paletteOf

    -- A resolution that does not arrive takes the overlay down rather than
    -- leaving a palette with nothing in it: there is no state to pick, and the
    -- log is where the reason goes.
  , keyed shell "a refused resolution closes the palette and says so"
      "" "refuse press:t" $ \answer -> do
        assertEqual "the overlay is down" "" =<< textAt "prompt" answer
        assertEqual "and the log named it"
                    (Just "keywords failed: GET /keywords?ids=<row id>")
          =<< lastLog answer
  ]

-- | The sheet's two panes, driven through the keys a reader presses.  What is
-- asserted here is the half the page owns: what the DOCUMENT draws and how it is
-- walked, what the PANEL shows and how it is opened and grown, what a sync sends,
-- and which of the two shapes the sheet is in.  The cut between the panes is the
-- server's and is @TestQuery@'s subject; nothing here re-states it.
--
-- BOTH panes are modal and neither focuses anything: the document holds the keys
-- when the sheet opens (@dactive@), @TAB@ crosses to the panel (@pnav@), and
-- @focus@ names a field only while a row or an element is open.
--
-- @Enter@ materializes the first row, which is where every case starts.  The
-- fixture subtree has a planning entry, one property, two paragraphs and one
-- child, so every kind the document draws is in it.
sheetSpec :: IO T.Text -> TestTree
sheetSpec shell =
  overBoot shell "Enter" "" $ \sheet ->
  testGroup "Shell sheet"
  [ atBoot sheet "materialize opens two panes over one subtree" $ \answer -> do
        -- The left pane is the subtree's TEXT as its elements: the headline
        -- line, the body's own paragraphs, and the child under it.  Every
        -- headline line opens with its STARS, org-cleaned — every star but the
        -- last drawn as a space, so the root reads `* ' and the child ` * ',
        -- which is `org-hide-leading-stars' with `org-startup-indented'.  A part
        -- the headline has NOT got renders nothing at all: this entry has no
        -- priority and no tags, and there is no placeholder for either.
        assertEqual "the document draws the headline, the paragraphs and the child"
          [ ["head", "* ", "TODO", "one"]
          , ["para", "first para"]
          , ["para", "second para"]
          , ["child", "  * ", "two", ":web:"] ] =<< docOf answer
        assertEqual "with the cursor on the headline and no cell picked yet"
                    (0, -1) =<< pointOf answer
        assertEqual "and the document holding the keys" True =<< boolAt "dactive" answer
        -- THE CRUMB STRIP STANDS: the row alone is one crumb, so the bar is a
        -- place rather than something that appears on the way down.
        assertEqual "the trail is the row, and it is where the reader stands"
                    (["one"], [0]) =<< ((,) <$> textsAt "where" answer
                                            <*> flaggedAt "whereAt" answer)
        assertEqual "the textarea is behind it, empty until C-c '"
                    "" =<< textAt "sheet" answer
        -- The three planning rows first, in org's own order and empty where the
        -- headline has no entry, then the drawer in file order.
        panelIs "the panel holds the planning rows and then the drawer"
                [["EFFORT", "0:30"]] answer
        -- Read-only, full width under the panes, and never sent back.  The
        -- drawer's INTERIOR alone: the widget being the drawer says what it is,
        -- so the two delimiter lines would be spent saying it twice.
        assertEqual "the logbook is shown, its delimiters left off"
                    "- moved here" =<< textAt "logbook" answer
        assertEqual "and the sheet is in its two-pane shape" "" =<< textAt "shape" answer
        -- The panel is read-only text until it is crossed into: the keys are
        -- the document's, and the panel's cursor is waiting on its first row.
        assertEqual "the keys are in the document pane" False =<< boolAt "pnav" answer
        assertEqual "with nothing focused, which is what frees the letters"
                    "" =<< textAt "focus" answer
        assertEqual "and the panel's cursor at the top" 0 =<< intAt "pat" answer

    -- MOVEMENT IS THE TABLE'S LETTERS EXACTLY, over the elements: both
    -- spellings and the vertical arrows walk them, and the walk stops at the
    -- ends rather than wrapping.
  , testCase "the document walks its elements on n/p, j/k and the arrows" $ do
      insheet "press:n press:n" $
        assertEqual "two elements down" (2, -1) <=< pointOf
      insheet "press:j press:j press:k" $
        assertEqual "vi's pair walks the same elements" (1, -1) <=< pointOf
      insheet "press:ArrowDown press:ArrowDown press:ArrowUp" $
        assertEqual "and so do the arrows" (1, -1) <=< pointOf
      insheet "press:p" $
        assertEqual "the headline is the end of the walk up" (0, -1) <=< pointOf
      insheet "press:n press:n press:n press:n" $
        assertEqual "and the child the end of the walk down" (3, -1) <=< pointOf

    -- THE CURSOR CARRIES ITS PANE'S SCROLL, which is the table's own discipline
    -- over the one scroller this page owns.  `#mdoc' clamps at the sheet's
    -- bound, so an element under the fold is reachable by `n' and invisible
    -- without this.
    --
    -- GEOMETRY IS BEYOND THE STUB: nothing in the harness has a layout, so
    -- whether the element WAS out of view cannot be asked and is not asserted.
    -- What is asserted is that the page ASKED — the same caveat, and the same
    -- shape of pin, as the overlay's placing.
  , testCase "the element under point asks its pane's scroller" $ do
      insheet "press:n press:n" $ \answer -> do
        seen <- textsAt "scrolled" answer
        assertEqual "the last ask was made on the element under point"
                    (Just "de d-para dat") (listToMaybe (reverse seen))
        -- `block:"nearest"' IS the scrolloff band as the platform spells it: an
        -- element already in view stays where it is, one past an edge comes
        -- just inside, and the pane never re-centres under a walk.
        assertEqual "and it asked for the band, not a re-centring"
                    (object [ "block" .= ("nearest" :: T.Text) ])
          =<< field "scrollAsked" answer
      -- Every draw carries it, the first one included, so a sheet reopened onto
      -- a cursor left far down lands with it in view rather than at the top.
      insheet "" $ \answer -> do
        seen <- textsAt "scrolled" answer
        assertEqual "the materialize itself asked, on the headline"
                    (Just "de d-head dat") (listToMaybe seen)

    -- CELLS ARE THE TABLE'S TOO, and the stops are the parts that are THERE:
    -- this entry has a state and a title and neither a priority nor tags, so it
    -- is two stops and the absent pair are not walked onto at all.  Walking off
    -- either end lands in the whole-element look rather than bumping, which is
    -- the rule `f'/`b' keep over the table.
  , testCase "f/b, l/h and the horizontal arrows walk the PRESENT cells" $ do
      insheet "press:f" $ \answer -> do
        assertEqual "the first cell" (0, 0) =<< pointOf answer
        echoIs "named by its key" "f → next-column (state)" answer
      -- The second stop is the TITLE, not the priority the entry has not got.
      insheet "press:l press:l" $ \answer -> do
        assertEqual "two across is the title" (0, 1) =<< pointOf answer
        echoIs "the absent priority was no stop" "l → next-column (title)" answer
      insheet "press:l press:l press:l" $
        assertEqual "and there is no third" (0, -1) <=< pointOf
      insheet "press:f press:b" $ \answer -> do
        assertEqual "back off the left end is the whole element" (0, -1)
          =<< pointOf answer
        echoIs "and says so" "b → next-column (element mode)" answer
      insheet "press:ArrowRight press:ArrowRight" $
        assertEqual "the arrows are the same walk" (0, 1) <=< pointOf
      -- A paragraph has no cells at all, so the key says so and moves nothing.
      insheet "press:n press:f" $ \answer -> do
        assertEqual "nothing moved" (1, -1) =<< pointOf answer
        echoIs "and the key said why" "f → next-column (no cells in this element)" answer
      -- And the column goes when the cursor leaves an element that had one.
      insheet "press:f press:n" $
        assertEqual "the cell went with the element" (1, -1) <=< pointOf

    -- A CURSOR IS ONLY DRAWN WHERE THE KEYS ARE, and its POSITION is not: the
    -- wash is CSS gated on the pane that holds them, so what behaviour can say
    -- is which pane wears the gate and that neither cursor MOVED while the other
    -- had the keys.  The gating rules themselves are asserted as text in "Shell
    -- glue" — nothing here has a stylesheet.
  , testCase "each pane's cursor waits where it was while the other has the keys" $ do
      -- Both cursors are somewhere from the moment the sheet opens; only the
      -- document's is drawn.
      insheet "press:n" $ \answer -> do
        assertEqual "the document has the keys" (True, False)
          =<< ((,) <$> boolAt "dactive" answer <*> boolAt "pnav" answer)
        assertEqual "its cursor moved" 1 =<< intAt "dat" answer
        assertEqual "and the panel's is waiting at its top" 0 =<< intAt "pat" answer
      -- Crossing hands the gate over and moves NEITHER position.
      insheet "press:n press:Tab press:n press:n" $ \answer -> do
        assertEqual "the panel has them now" (False, True)
          =<< ((,) <$> boolAt "dactive" answer <*> boolAt "pnav" answer)
        assertEqual "the document's cursor is where it was left" 1
          =<< intAt "dat" answer
        assertEqual "and the panel's has moved under them" 2 =<< intAt "pat" answer
      -- And back: the gate returns to the document and both are still there.
      insheet "press:n press:Tab press:n press:n press:Tab" $
        \answer -> do
          assertEqual "the document has them again" (True, False)
            =<< ((,) <$> boolAt "dactive" answer <*> boolAt "pnav" answer)
          assertEqual "with its cursor untouched" 1 =<< intAt "dat" answer
          assertEqual "and the panel's kept where it got to" 2 =<< intAt "pat" answer

    -- ONE GRID: THE CHILD'S STAR SITS IN THE PARENT'S BODY COLUMN.  That is the
    -- whole of org-indent's arithmetic here — two spaces a level, so the stars
    -- are indented TO the body level rather than beside it — and it is asserted
    -- as the EQUALITY of two numbers this page produces independently: the
    -- column the child's own prefix puts its star at, and the column the
    -- paragraphs under the head are padded to.
  , atBoot sheet "a child's star sits in the parent's body column" $ \answer -> do
        rows <- docOf answer
        body <- textAt "dindent" answer
        let prefix = case [ r | r <- rows, take 1 r == ["child"] ] of
                       ((_kind : p : _rest) : _more) -> p
                       _none                         -> ""
        assertEqual "the child's prefix is two spaces and its star" "  * " prefix
        assertEqual "and its star stands in the column the body starts at"
                    (Just (read (T.unpack body) :: Int)) (T.findIndex (== '*') prefix)

    -- ORG-STARTUP-INDENTED'S OTHER HALF: content sits under the TITLE TEXT
    -- rather than under the stars, so the column is the width of the head's own
    -- star prefix.  The head is the ROOT of its own document whatever entry the
    -- sheet walked into — it always draws `* ' — so the answer is the same two
    -- at every depth; what this pins is that it is DERIVED from `dstars' rather
    -- than a 2 spelled beside it, and the child case is where a hand-written
    -- one would have gone wrong first.
  , testCase "content lines start at the title's column, at either depth" $ do
      insheet "" $
        assertEqual "the row's own document" "2" <=< textAt "dindent"
      insheet "press:n press:n press:n press:Enter" $
        assertEqual "and a child, which is the root of its own" "2"
          <=< textAt "dindent"

    -- NO PLACEHOLDERS, EVER.  An absent part renders nothing in every state —
    -- at rest, with the element under point, and with the cursor in a cell
    -- beside it — so what a reader sees is the entry as org spells it and the
    -- only thing that marks structure is the cursor.  Setting an absent part is
    -- `t' and `:', below, rather than a cell that has to be drawn to be reached.
  , testCase "an absent part renders nothing, in every state" $
      mapM_ (\(what, keys) ->
               insheet keys $ \answer -> do
                 rows <- docOf answer
                 assertEqual (what <> ": the headline line, and nothing it lacks")
                             ["head", "* ", "TODO", "one"] (head rows)
                 assertEqual (what <> ": nor the child's")
                             ["child", "  * ", "two", ":web:"] (last rows))
            [ ("at rest", ""), ("on the element", "press:p")
            , ("in the state cell", "press:f"), ("in the title cell", "press:f press:f")
            , ("on the child", "press:n press:n press:n") ]

    -- RET IS BY KIND, and a CHILD is the one that moves the sheet: it
    -- re-materializes into that entry, which is the same route under the index
    -- the server handed over.  The breadcrumb is what says where it landed.
  , testCase "RET on a child materializes into it, and DEL climbs back" $ do
      insheet "press:n press:n press:n press:Enter" $ \answer -> do
        -- The child is the ROOT of its own document, so its stars read `* ' and
        -- the depth a reader sees is relative to the entry they are looking at.
        assertEqual "the child's own document"
          [ ["head", "* ", "two", ":web:"]
          , ["para", "child body"] ] =<< docOf answer
        assertEqual "the trail gained a crumb" ["one", "two"] =<< textsAt "where" answer
        assertEqual "and the last one is where the reader stands" [1]
          =<< flaggedAt "whereAt" answer
        echoIs "and the pill names what it opened"
          "RET → org-glance-overview:materialize (two)" answer
      bootOf shell "" 500 "Enter"
             "press:n press:n press:n press:Enter press:Backspace" $ \answer -> do
        assertEqual "back at the row, one crumb again" ["one"] =<< textsAt "where" answer
        assertEqual "with the cursor on the child it came out of" (3, -1)
          =<< pointOf answer
        echoIs "and the pill names the climb" "DEL → org-glance-overview:up (one)" answer

    -- At the top there is nothing above the row, so DEL is the sheet's door —
    -- and the table's own DEL must not also fire, or the filter under the sheet
    -- would lose a token to the same press.
  , keyed shell "DEL at the top closes the sheet and nothing else"
      "Enter" "press:Backspace" $ \answer -> do
        assertEqual "the sheet is closed" "" =<< textAt "modal" answer
        urlIs "and the applied query is where it was" "?q=state%3A*active*" answer

    -- A PARAGRAPH opens as text, and its commit is `C-x C-s': RET is a newline
    -- inside one, so `save-buffer' over the open edit is the only commit it has.
    -- What goes back is the BODY with that block's own lines spliced over, every
    -- other byte where it was.
  , testCase "RET opens a paragraph as text, and C-x C-s writes it" $ do
      insheet "press:n press:Enter" $ \answer -> do
        assertEqual "the block is open" True =<< boolAt "dparaopen" answer
        assertEqual "with its text in the field" "first para" =<< textAt "dtext" answer
        assertEqual "and the focus in it" "dtext" =<< textAt "focus" answer
      bootOf shell "" 500 "Enter"
             "press:n press:Enter dpara:rewritten press:C-x press:C-s" $ \answer -> do
        assertEqual "one write, aimed at the row"
                    ["r1"] =<< textsAt "wroteAt" answer
        assertEqual "the body with that block replaced and nothing else"
                    ["* TODO one\nrewritten\n\nsecond para\n** two\nchild body\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer
        assertEqual "and the sheet is synced" "synced" =<< textAt "state" answer

    -- THE GRAIN IS A WALK, not a mode.  A list and a `#+begin_'/`#+end_' block
    -- each take TWO KINDS OF STOP over the same bytes, laid out in document
    -- order as `[whole, item1..itemN]' and inline among everything else — so
    -- `n' from above meets the whole thing and then walks into it, and `p' from
    -- below walks the items and meets the whole on the way out.
    --
    -- WHICH IS THE WHOLE MECHANISM.  There is no descend key and no ascend key,
    -- no mode to be in and none to leave: `p' is `n' read backwards because the
    -- sequence is one sequence.  RET stays pure edit at either grain, DEL stays
    -- the sheet's own ladder, and `d' flags whatever the stop is.
  , testCase "a list and a block are the whole thing, then their parts" $ do
      onTable "grain press:Enter" $ \answer -> do
        assertEqual "the walk, kind by kind"
                    [ "head", "para", "comp:list", "item", "item", "item"
                    , "comp:quote", "item", "item", "para", "child" ]
          =<< map head <$> docOf answer
        assertEqual "and the grain of each stop"
                    [ "element", "element", "composite", "leaf", "leaf", "leaf"
                    , "composite", "leaf", "leaf", "element", "element" ]
          =<< textsAt "dgrains" answer
        -- Each leaf hangs under the composite it was drawn inside, which is
        -- what makes the two grains one range rather than two.
        assertEqual "and who it hangs under" [-1, -1, -1, 2, 2, 2, -1, 6, 6, -1, -1]
          =<< flaggedAt "downers" answer

    -- AND `p' IS THAT SEQUENCE READ BACKWARDS, which is the whole reason the
    -- composite sits AHEAD of its leaves rather than after them: coming down,
    -- the reader meets the block and then walks into it; coming up, they walk
    -- the parts and meet the whole on the way OUT.  One order serves both, so
    -- neither direction needs a rule of its own.
  , testCase "p walks the parts, then the whole, on the way out" $ do
      let down9 = "press:n press:n press:n press:n press:n press:n press:n press:n press:n"
      onTable ("grain press:Enter " <> down9 <> " press:p") $
        assertEqual "up from the tail paragraph is the block's LAST part" (8, -1)
          <=< pointOf
      bootOf shell "" 500 ""
             ("grain press:Enter " <> down9 <> " press:p press:p press:p") $
        assertEqual "and three up is the block itself" (6, -1) <=< pointOf
      -- Which is exactly what `n' saw coming the other way, in reverse.
      bootOf shell "" 500 ""
             "grain press:Enter press:n press:n press:n press:n press:n press:n" $
        assertEqual "six down is the block, before its parts" (6, -1) <=< pointOf

    -- AND AN ORG TABLE IS THAT SAME SHAPE, which is the whole of what it is: a
    -- run of `|' lines is ONE COARSE STOP and then its rows, drawn inline in the
    -- one sequence, so `n' from above meets the table and walks into it and `p'
    -- from below walks the rows and meets it on the way out.  No cell grain and
    -- no column awareness: 101 of the corpus's 6337 files hold table rows, so
    -- the case for a second walk to teach is not there.
    --
    -- A LINE IS A LEAF, the `|---+---|' RULE included: a line is a line, and
    -- editing or deleting one is the same act whichever kind of row it is.
  , keyed shell "a table is one stop, then its rows" "" "tabled press:Enter" $ \answer -> do
        assertEqual "the walk, kind by kind, over a MIXED body"
                    [ "head", "para", "comp:table", "item", "item", "item", "item"
                    , "comp:list", "item", "item", "para", "child" ]
          =<< map head <$> docOf answer
        assertEqual "and the grain of each stop"
                    [ "element", "element", "composite", "leaf", "leaf", "leaf"
                    , "leaf", "composite", "leaf", "leaf", "element", "element" ]
          =<< textsAt "dgrains" answer
        -- Each row hangs under the table it was drawn inside, the way a list's
        -- items hang under theirs.
        assertEqual "and who each row hangs under"
                    [-1, -1, -1, 2, 2, 2, 2, -1, 7, 7, -1, -1]
          =<< flaggedAt "downers" answer
        -- The rule is a stop like any other, and it shows what the file says.
        assertEqual "the four rows, the rule among them"
                    [["| a | b |"], ["|---+---|"], ["| 1 | 2 |"], ["| 3 | 4 |"]]
          . map (drop 1) . take 4 . drop 3 =<< docOf answer

  , testCase "the walk crosses a table in both directions" $ do
      onTable "tabled press:Enter press:n press:n" $
        assertEqual "n from the lead-in meets the WHOLE table first" (2, -1)
          <=< pointOf
      onTable "tabled press:Enter press:n press:n press:n" $
        assertEqual "and then walks into its first row" (3, -1) <=< pointOf
      let down7 = T.unwords (replicate 7 "press:n")
      onTable ("tabled press:Enter " <> down7 <> " press:p") $
        assertEqual "up from the list is the table's LAST row" (6, -1) <=< pointOf
      bootOf shell "" 500 ""
             ("tabled press:Enter " <> down7 <> " " <> T.unwords (replicate 5 "press:p")) $
        assertEqual "and five up is the table itself" (2, -1) <=< pointOf

    -- A ROW EDIT IS A LINE SPLICE, which is the decompose/recompose property one
    -- grain in: the row remembers the line it came out of, so what goes back is
    -- the body with that ONE line replaced and every other byte where it was —
    -- the rule and the rows around it included.
  , testCase "editing a table row splices that line and nothing else" $ do
      bootOf shell "" 500 ""
             ("tabled press:Enter " <> T.unwords (replicate 5 "press:n")
                <> " press:Enter dpara:~9~9~ press:C-x press:C-s") $ \answer ->
        assertEqual "the body with that row replaced and nothing else"
                    [tabledAfter "| 1 | 2 |" "|9|9|"]
          =<< traverse (textAt "body") =<< listAt "writes" answer
      -- AND THE RULE IS EDITABLE THE SAME WAY: it is a leaf, so RET opens it and
      -- the commit puts its own line back.
      bootOf shell "" 500 ""
             ("tabled press:Enter " <> T.unwords (replicate 4 "press:n")
                <> " press:Enter dpara:~-+-~ press:C-x press:C-s") $ \answer ->
        assertEqual "the rule replaced, and the rows around it untouched"
                    [tabledAfter "|---+---|" "|-+-|"]
          =<< traverse (textAt "body") =<< listAt "writes" answer
      -- RET on the WHOLE table opens the whole block, which is the composite's
      -- own rule and the reason both grains are stops.
      onTable "tabled press:Enter press:n press:n press:Enter" $
        assertEqual "the block whole, rule and all"
                    "| a | b |\n|---+---|\n| 1 | 2 |\n| 3 | 4 |" <=< textAt "dtext"

    -- ORG LINKS RENDER, and the rule is ORG'S OWN DISPLAY-VS-SOURCE MODEL: what
    -- is SHOWN is the description — `[[T][D]]' shows `D', `[[T]]' shows `T', a
    -- bare URL shows itself — and what `RET' opens is the RAW org, brackets and
    -- all.  The display never becomes the source, so an edit is always over
    -- what the file says.
    --
    -- NO SECOND PARSER.  The shown text is the server's `desc' verbatim and the
    -- range is the server's `span': one scan, in `Glance.Query', and this page
    -- does arithmetic on the answer.  Which is why a bare URL is drawn too —
    -- it is in the same answer — and why one URL written twice is drawn twice,
    -- each where it stands rather than wherever a search first found it.
  , keyed shell "a paragraph shows its links' descriptions, in link ink"
      "" "linky press:Enter" $ \answer -> do
        segs <- pairsAt "dsegs" answer
        -- `[[T][D]]' shows D and `[[T]]' shows T, each in its place, with the
        -- text between them left exactly as written.
        assertEqual "the paragraph, cut into text and links"
          [ "dt:see ", "dl:alpha", "dt: and ", "dl:https://b.example/", "dt: here" ]
          (segs !! 1)
        -- A BARE URL is a link too, coming back in the same answer.  Written
        -- TWICE it is marked ONCE: `/links' keeps the FIRST spelling of a
        -- target and no other (`Glance.Query.orgLinks', so that `o' offers one
        -- destination one letter), and this draw follows the SPANS it was
        -- given rather than searching the text for what it just drew — so the
        -- second occurrence is the text it is.  A consequence of the scan's
        -- rule, visible here because the display is downstream of it.
        assertEqual "the first spelling is marked, the second reads as text"
          [ "dt:bare ", "dl:https://c.example/", "dt: then https://c.example/ twice" ]
          (segs !! 2)
        -- What the element READS as, once the pieces are put together: the
        -- brackets are gone from the DISPLAY.  Where they are still there is
        -- the file, and the next case is what opens it.
        assertEqual "and reads as the descriptions"
          ["para", "see alpha and https://b.example/ here"] . (!! 1) =<< docOf answer
      -- What RET opens, spelled out: the display is not the source.
  , keyed shell "RET opens the raw org, not what was shown"
      "" "linky press:Enter press:n press:Enter" $
        assertEqual "brackets and all"
          "see [[https://a.example/][alpha]] and [[https://b.example/]] here"
          <=< textAt "dtext"

    -- THE TITLE CELL RENDERS THE SAME WAY, its links being in the same answer.
    -- The server sends where the cell starts (`titleAt') because only it has
    -- that sub-span; everything else is the same arithmetic.
  , keyed shell "the headline's title cell shows its link too"
      "" "linky press:Enter" $ \answer -> do
        segs <- pairsAt "dsegs" answer
        assertEqual "the title, cut the same way"
                    ["dt:one ", "dl:the title link"] (head segs)

    -- A LINK IS NOT A STOP and binds no mouse: `o' is the opener, here as over
    -- the table, so the marks say "there is a reference in this text" and
    -- nothing more.  The walk is the same walk it was.
  , keyed shell "links are drawn, and are no stop" "" "linky press:Enter press:n" $ \answer -> do
        assertEqual "one step down is the paragraph, links and all" (1, -1)
          =<< pointOf answer
        assertEqual "nothing was opened by drawing them" [] =<< openedOf answer

    -- `o' SCOPES TO THE STOP, whatever the grain: the span it asks over is the
    -- element's own line range, so one item's links and the whole list's are
    -- two different questions asked with one key.  The range is worked out on
    -- this side — every lifted region sits above the paragraphs, so the body's
    -- lines and the file's differ by one constant — which is why a new grain
    -- needed no new server field.
  , testCase "o asks over the stop the cursor is on" $ do
      -- At the first ITEM there is one link inside it, so `o' opens it outright.
      bootOf shell "" 500 ""
             "grain grainlinks press:Enter press:n press:n press:n press:o" $
        \answer -> do
          assertEqual "the item's own link, opened"
                      [("https://alpha.example/", "_blank", "noopener")]
            =<< openedOf answer
          assertEqual "and no popup was needed" "" =<< textAt "popup" answer
      -- At the WHOLE LIST both are inside, so the same key raises the popup —
      -- one gesture, and the stop is what decides which answer it has.
      bootOf shell "" 500 ""
             "grain grainlinks press:Enter press:n press:n press:o" $ \answer -> do
        assertEqual "nothing opened outright" [] =<< openedOf answer
        assertEqual "both links are listed" ["in alpha", "in beta"]
          =<< map (!! 1) <$> pairsAt "llinks" answer
      -- And at a stop with no link under it, the honest refusal.
      onTable "grain grainlinks press:Enter press:n press:o" $
        -- The pill names the COMMAND, and the sequence is the keymap row's own
        -- spelling of it rather than the key that was pressed.
        \answer -> assertEqual "the lead-in reaches neither"
                               "RET → org-glance-overview:open (no links)"
                     =<< textAt "echo" answer

    -- ONE BLANK LINE STAYS IN A LIST    -- ONE BLANK LINE STAYS IN A LIST, which is org's rule and the corpus's:
    -- 1173 item pairs are separated by exactly one.  `beta' after a blank is
    -- the SAME list's second item rather than a second list, and the deeper
    -- `- nested' rides inside `alpha' rather than taking a stop — v1's grain.
  , keyed shell "a blank line and a nested item stay inside their list"
      "" "grain press:Enter press:n press:n press:n" $ \answer -> do
        assertEqual "three items, and the first carries what hangs under it"
                    ["- alpha\n  more alpha\n  - nested", "- beta", "- gamma"]
          =<< partsOf "item" . take 6 <$> docOf answer
        assertEqual "the cursor is on the first of them" (3, -1) =<< pointOf answer

    -- WHAT NO LEAF CLAIMS IS STILL DRAWN, and drawn inert: a block's own
    -- `#+begin_'/`#+end_' lines are inside the composite and belong to no
    -- paragraph in it, so they are on screen exactly once and nothing lands on
    -- them.  The lens's one-owner-per-byte rule, one grain down.
  , keyed shell "a block's delimiters are drawn, and are no stop"
      "" "grain press:Enter" $ \answer -> do
        rows <- docOf answer
        assertEqual "the composite shows the delimiters and nothing else"
                    ["#+begin_quote\n\n#+end_quote"] (partsOf "comp:quote" rows)
        assertEqual "and its paragraphs are the stops inside it"
                    ["quoted one", "quoted two"] (partsOf "item" (drop 6 rows))

    -- RET IS PURE EDIT AT EITHER GRAIN: a leaf opens its own lines, a composite
    -- opens the whole block's, and each commit splices exactly the range its
    -- stop covers.
  , testCase "RET edits a leaf's own lines, and splices only those" $ do
      onTable "grain press:Enter press:n press:n press:n press:Enter" $
        \answer -> assertEqual "the item, as it stands"
                               "- alpha\n  more alpha\n  - nested"
                     =<< textAt "dtext" answer
      bootOf shell "" 500 ""
             ("grain press:Enter press:n press:n press:n press:Enter dpara:-_ALPHA"
              <> " press:C-x press:C-s") $ \answer -> do
        body <- traverse (textAt "body") =<< listAt "writes" answer
        assertEqual "the item's lines, and every other byte where it was"
          [ "* TODO one\nlead in\n- ALPHA\n\n- beta\n- gamma\n\n#+begin_quote\n"
            <> "quoted one\n\nquoted two\n#+end_quote\n\ntail para\n** two\nchild body\n" ]
          body
  , testCase "RET at the whole list edits the whole list" $ do
      onTable "grain press:Enter press:n press:n press:Enter" $
        \answer -> assertEqual "every line the composite covers"
                               "- alpha\n  more alpha\n  - nested\n\n- beta\n- gamma"
                     =<< textAt "dtext" answer
      bootOf shell "" 500 ""
             ("grain press:Enter press:n press:n press:Enter dpara:-_one|-_two"
              <> " press:C-x press:C-s") $ \answer -> do
        body <- traverse (textAt "body") =<< listAt "writes" answer
        assertEqual "the list's whole range replaced, and nothing beyond it"
          [ "* TODO one\nlead in\n- one\n- two\n\n#+begin_quote\n"
            <> "quoted one\n\nquoted two\n#+end_quote\n\ntail para\n** two\nchild body\n" ]
          body

    -- `d' FLAGS WHATEVER THE STOP IS, which is the same rule at both grains and
    -- is why the grain needed no key of its own: the reader is already standing
    -- on the thing they mean.
  , testCase "d flags one item, or the whole list" $ do
      onTable "grain press:Enter press:n press:n press:n press:d" $
        assertEqual "the item alone" [3] <=< flaggedOf
      onTable "grain press:Enter press:n press:n press:d" $
        assertEqual "or the composite alone" [2] <=< flaggedOf
      -- And the delete splices the range the flag was laid on.
      onTable "grain press:Enter press:n press:n press:d press:d" $
        \answer -> do
          body <- traverse (textAt "body") =<< listAt "writes" answer
          assertEqual "the whole list is gone, the rest untouched"
            [ "* TODO one\nlead in\n#+begin_quote\nquoted one\n\nquoted two\n"
              <> "#+end_quote\n\ntail para\n** two\nchild body\n" ] body

    -- ESC over an open element is the ELEMENT's and puts back what it held; the
    -- next one reaches the sheet's own ladder.
  , testCase "ESC puts an open paragraph back, and the next one closes the sheet" $ do
      insheet "press:n press:Enter dpara:rewritten press:Escape" $
        \answer -> do
          assertEqual "the overlay is gone" False =<< boolAt "dparaopen" answer
          assertEqual "the sheet is still up" "on" =<< textAt "modal" answer
          assertEqual "with nothing written" ([] :: [Value]) =<< listAt "writes" answer
          echoIs "and it said so" "ESC → keyboard-quit (element unchanged)" answer
      insheet "press:n press:Enter press:Escape press:Escape" $
        assertEqual "the second one is the sheet's" "" <=< textAt "modal"

    -- THE DELETION GESTURE IS KIND-AWARE, and over the document it is the
    -- paragraphs: `d' flags, a second `d' takes every flagged block out of the
    -- body, and the write is one splice.
  , testCase "d flags a paragraph and d again splices it out of the body" $ do
      insheet "press:n press:d" $ \answer -> do
        assertEqual "the block wears the flag" [1] =<< flaggedOf answer
        assertEqual "and nothing is written yet" ([] :: [Value])
          =<< listAt "writes" answer
        echoIs "the pill says what the second press will do"
          "d → delete-flag (d again deletes)" answer
      insheet "press:n press:d press:d" $ \answer -> do
        assertEqual "the body with the block and its blank line gone"
                    ["* TODO one\nsecond para\n** two\nchild body\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer
        echoIs "and the pill counted the set" "D → org-delete-element (1 flagged taken)" answer
      -- A HELD `d' must not flag and delete from one press, which is the
      -- confirmation the two-press shape exists to be.
      insheet "press:n press:d repeat:d" $ \answer -> do
        assertEqual "the flag is still there" [1] =<< flaggedOf answer
        assertEqual "and nothing was written" ([] :: [Value]) =<< listAt "writes" answer

    -- A HEADLINE is refused: deleting an entry is not what this sheet is for,
    -- and there is no command behind it.  It says so rather than doing nothing.
  , keyed shell "a headline is not deleted from the document, and says so"
      "Enter" "press:n press:n press:n press:D" $ \answer -> do
        assertEqual "nothing written" ([] :: [Value]) =<< listAt "writes" answer
        assertEqual "the log says why"
          (Just "a headline is not deleted from the sheet — this writes elements only")
          =<< lastLog answer

    -- THE HEADLINE'S CELLS ARE THE ROW'S, and a row is what `/command'
    -- addresses: the state cell raises the page's own value palette over THIS
    -- row, and the tags cell raises the tags popup over it.
  , keyed shell "RET on the state cell raises the palette over this row"
      "Enter" "press:f press:Enter" $ \answer -> do
        assertEqual "the palette is up" "on" =<< textAt "prompt" answer
        assertEqual "named for the entry" "set state · one" =<< textAt "phead" answer
        assertEqual "resolved for the row the sheet is on"
                    ["/keywords?ids=r1"] =<< textsAt "resolved" answer

  , keyed shell "and a letter there is one set-state over it"
      "Enter" "press:f press:Enter press:d" $
        assertEqual "the command it posted"
          [("set-state", ["r1"])] <=< postedOf

    -- `t' AND `:' WORK AT THE ELEMENT, which is what makes an ABSENT part
    -- settable: this entry carries no tags, so there is no tags cell to walk
    -- onto, and the question is asked of the headline instead.  No cell point is
    -- needed and none is read.
  , testCase "t and : fire from the element, whatever the cell point" $ do
      insheet "press:t" $ \answer -> do
        assertEqual "the palette is up" "on" =<< textAt "prompt" answer
        assertEqual "over the row the sheet is on" ["/keywords?ids=r1"]
          =<< textsAt "resolved" answer
      insheet "press::" $ \answer -> do
        assertEqual "the popup is up" "on" =<< textAt "tagpop" answer
        assertEqual "named for the entry" "tags · one" =<< textAt "thead" answer
      -- With a cell under point they mean the same thing: the element is what
      -- they name, and the column is not read.
      insheet "press:f press:t" $
        assertEqual "a cell point changes nothing" "on" <=< textAt "prompt"
      -- And from a paragraph they say which line takes them.
      insheet "press:n press:t" $ \answer -> do
        assertEqual "nothing raised" "" =<< textAt "prompt" answer
        echoIs "and it said where to stand" "the headline line takes this — n/p to it" answer

    -- A POPUP RAISED FROM THE DOCUMENT GETS THE KEYS, and the document does not
    -- keep them.  The sheet is the FLOOR of the surface stack — everything here
    -- can be raised over it — so its listener declines while anything above it
    -- is up.  Without that the document eats the very letter the palette was
    -- raised to read: it ran its own binding for it AND claimed the press, so
    -- the popup behind it saw a key already handled.
  , keyed shell "a palette raised from the document has the letters, and it alone"
      "Enter" "press:t press:d" $ \answer -> do
        assertEqual "the letter committed" [("set-state", ["r1"])] =<< postedOf answer
        -- `d' is the document's delete-flag key.  If the document had still been
        -- listening it would have flagged the element under point on the way.
        assertEqual "and flagged nothing on the way" ([] :: [Int])
          =<< flaggedOf answer

  , keyed shell "and the tags popup raised from it takes its own d, not the document's"
      "Enter" "press:: press:d" $ \answer -> do
        assertEqual "the popup is up" "on" =<< textAt "tagpop" answer
        -- `d' is the flag key on BOTH surfaces, which is what makes it the
        -- sharpest press to test with: it landed on the popup's tag.
        assertEqual "the tag wears the flag" ["web"] =<< textsAt "tflagged" answer
        assertEqual "and no element of the document does" ([] :: [Int])
          =<< flaggedOf answer

    -- THE SAME RULE FROM THE TABLE, which is the regression the reorder owes:
    -- with no sheet open the floor is not up at all and nothing changes.
  , keyed shell "and a palette raised from the TABLE still has them" "t" "press:d" $ \answer -> do
        assertEqual "the letter committed" [("set-state", ["r1"])] =<< postedOf answer
        assertEqual "the sheet never opened" "" =<< textAt "modal" answer

    -- THE TITLE IS A CELL A READER EDITS AS TEXT, so it opens in the shared
    -- overlay and commits `set-title' — a span splice over the title's own
    -- characters rather than a rewrite of the subtree around it.
  , testCase "RET on the title cell opens it, and RET commits set-title" $ do
      insheet "press:f press:f press:Enter" $ \answer -> do
        assertEqual "the overlay is open" True =<< boolAt "dopen" answer
        assertEqual "the key names the cell" "title" =<< textAt "dkey" answer
        assertEqual "and the value is the title" "one" =<< textAt "dval" answer
        assertEqual "with the focus on the value" "dval" =<< textAt "focus" answer
      bootOf shell "" 500 "Enter"
             "press:f press:f press:Enter dval:renamed press:Enter" $ \answer -> do
        assertEqual "one set-title over this row"
                    [("set-title", ["r1"])] =<< postedOf answer
        assertEqual "and the log named both ends"
                    (Just "headline \"one\" retitled \"renamed\"") =<< lastLog answer
        assertEqual "nothing went through the lens" ([] :: [Value])
          =<< listAt "writes" answer

    -- TWO KEYS COMMIT AN OPEN ELEMENT, and org's is one of them: `C-c C-c' is
    -- `org-ctrl-c-ctrl-c', its own "do the thing here", and here the thing is
    -- whatever element is open — the paragraph's textarea and the two-field
    -- overlay alike.  `C-x C-s' keeps the half that is a BUFFER's: with nothing
    -- open it flushes the sheet and on a conflict it overwrites, which is why
    -- the two are not one row under two spellings.
    --
    -- `Ctrl+C' reaches the page — it is a page default action rather than a
    -- chrome shortcut — and COPY is untouched, prefix opening being guarded by
    -- `selecting()': with anything selected the first press is the browser's,
    -- which is exactly when a reader means to copy.
  , testCase "C-c C-c commits the open element, where C-x C-s does" $ do
      -- The paragraph, both ways, writing the same body.
      let wrote acts = insheet acts $ \answer ->
            assertEqual "the block replaced and nothing else"
                        ["* TODO one\nrewritten\n\nsecond para\n** two\nchild body\n"]
              =<< traverse (textAt "body") =<< listAt "writes" answer
      wrote "press:n press:Enter dpara:rewritten press:C-x press:C-s"
      wrote "press:n press:Enter dpara:rewritten press:C-c press:C-c"
      -- And the overlay, likewise: the same command over the same row.
      bootOf shell "" 500 "Enter"
             "press:f press:f press:Enter dval:renamed press:C-c press:C-c" $
        \answer -> do
          assertEqual "one set-title over this row"
                      [("set-title", ["r1"])] =<< postedOf answer
          assertEqual "the overlay is closed" False =<< boolAt "dopen" answer
      -- THE ECHO NAMES THE COMMAND THAT RAN, so the two keys are told apart by
      -- what they say as well as by what they are.
      bootOf shell "" 500 "Enter"
             "press:n press:Enter press:C-c press:C-c" $
        echoIs "org's own name, on an element nothing changed in"
          "C-c C-c → org-ctrl-c-ctrl-c (paragraph unchanged)"
      insheet "press:n press:Enter press:C-x press:C-s" $
        echoIs "and the buffer's name where that key ran"
          "C-x C-s → save-buffer (paragraph unchanged)"
      -- With NOTHING open it is not the sheet's flush: that is `save-buffer''s
      -- half, and this key stops where the element does.
      insheet "press:C-c press:C-c" $ \answer -> do
        assertEqual "nothing was written" ([] :: [Value]) =<< listAt "writes" answer
        echoIs "and it said so" "C-c C-c → org-ctrl-c-ctrl-c (nothing open here)" answer

    -- EVERY COMMIT RE-READS THE ENTRY IT WROTE, so the model the reader is
    -- looking at is the SERVER's reading of what landed rather than this page's
    -- guess at it — and it re-reads the entry the sheet is standing on rather
    -- than the row.
  , testCase "a commit re-materializes the entry it wrote" $ do
      bootOf shell "" 500 "Enter"
             "press:n press:Enter dpara:rewritten press:C-x press:C-s" $
        assertEqual "opened once, and read again on the answer"
                    ["r1", "r1"] <=< textsAt "readAt"
      bootOf shell "" 500 "Enter"
             ("press:n press:n press:n press:Enter press:n press:Enter"
                <> " dpara:reworded press:C-x press:C-s") $
        assertEqual "the row, the child, and the child again"
                    ["r1", "r1#0", "r1#0"] <=< textsAt "readAt"

    -- A `/command' NEVER WRITES THE STORE — the watch does, a debounce later —
    -- so a cell edit made from this sheet leaves it holding what the file said
    -- before.  The frame naming this row is when there is something fresher to
    -- read, which is the same channel the table's own rows arrive by.
  , testCase "a socket frame naming this row re-reads the sheet" $ do
      insheet "frame:upsert=r1" $
        assertEqual "opened once, then re-read on the frame"
                    ["r1", "r1"] <=< textsAt "readAt"
      -- Not while an edit is open: a re-read would pull the model out from
      -- under the fields the reader is typing into.
      insheet "press:n press:Enter frame:upsert=r1" $
        assertEqual "left alone under an open element" ["r1"] <=< textsAt "readAt"
      -- Nor over an open PANEL row, nor over drawer work the reader has
      -- committed to the model and not yet flushed: `reload' rebuilds `prows'
      -- and re-pins `baseProps', so a re-read there throws the edit away under a
      -- `synced' header — and the reader's own `t' or `S-<up>' from inside the
      -- sheet is what CAUSES the frame, so it is the ordinary case rather than a
      -- race.
      insheet "press:Tab press:Enter frame:upsert=r1" $
        assertEqual "left alone under an open panel row" ["r1"] <=< textsAt "readAt"
      bootOf shell "" 500 "Enter"
             "press:Tab press:Enter pval:0=tomorrow press:Enter frame:upsert=r1" $
        \answer -> do
          assertEqual "and over a drawer edit nobody has flushed"
                      ["r1"] =<< textsAt "readAt" answer
          assertBool "with the edit still on screen"
            . elem ["SCHEDULED", "tomorrow"] =<< pairsAt "props" answer
      -- And a frame for some other row says nothing about this sheet.
      insheet "frame:upsert=r2" $
        assertEqual "another row is not this one" ["r1"] <=< textsAt "readAt"

    -- THE RING REACHES THE DOCUMENT, over the entry the sheet is standing on:
    -- the same command and the same wrap, read off the ANSWER's own cells rather
    -- than off a table row the page may not be showing.
  , testCase "S-up cycles the priority of the entry the sheet is on" $ do
      insheet "press:S-ArrowUp" $ \answer -> do
        assertEqual "one command over this row"
                    [("set-priority", ["r1"])] =<< postedOf answer
        assertEqual "the fixture entry has none, so it lands on C"
                    [Just "C"] =<< prioritiesOf answer
        echoIs "and the pill names the key that ran it" "S-<up> → priority-up ([#C] · 1)" answer
      -- Refused on a child, for its cells' own reason: no row id to name.
      bootOf shell "" 500 "Enter"
             "press:n press:n press:n press:Enter press:S-ArrowUp" $ \answer -> do
        assertEqual "nothing posted" ([] :: [Value]) =<< listAt "commands" answer
        echoIs "and it said which key climbs out"
          "a child is not settable yet — DEL opens its parent" answer

    -- AND A HELD ONE ASKS ONCE.  This listener runs AHEAD of the dispatch and
    -- claims what it takes, so the map's own ONCE list can never reach a key of
    -- its own: without a guard here a leaned-on `S-<up>' was one `/command' per
    -- repeat, each measured against a cell the answer before it had already
    -- moved — a burst of 409s off one press.  Movement keeps its repeat, which
    -- is how a reader crosses the pane.
  , testCase "a held S-up cycles once" $ do
      bootOf shell "" 500 "Enter"
             "press:S-ArrowUp repeat:S-ArrowUp repeat:S-ArrowUp" $ \answer -> do
        assertEqual "one command, however long the key is held"
                    [("set-priority", ["r1"])] =<< postedOf answer
      insheet "press:n repeat:n repeat:n" $
        assertEqual "and a held movement key still walks" 3 <=< intAt "dat"

    -- RET ON THE PRIORITY CELL STILL REFUSES, and now for a reason rather than
    -- for want of one: a ring of three is pressed, not picked from a list, so
    -- there is no popup a cell could raise that the two keys do not already
    -- answer faster.
  , keyed shell "and RET on the priority cell still has no popup to raise"
      "" "priorities:A press:Enter press:f press:f press:Enter" $
        \answer -> do
          assertEqual "nothing posted" ([] :: [Value]) =<< listAt "commands" answer
          echoIs "and the pill says the keys that do it"
            "RET → priority cycles on S-<up>/S-<down>" answer

    -- A CHILD'S cells are read-only in v1: a child has no row id, so no
    -- `/command' can address it, and the echo says which key reaches the entry
    -- that owns them.
  , testCase "a child's cells are not settable yet, and the echo says so" $ do
      bootOf shell "" 500 "Enter"
             "press:n press:n press:n press:Enter press:f press:Enter" $ \answer -> do
        assertEqual "nothing posted" ([] :: [Value]) =<< listAt "commands" answer
        echoIs "and the pill named the way out"
          "RET → a child's title is not settable yet — DEL opens its parent" answer
      -- And the element keys are refused there for the same reason: a child has
      -- no row id, so no `/command' can name it.
      insheet "press:n press:n press:n press:Enter press:t" $
        \answer -> do
          assertEqual "nothing raised" "" =<< textAt "prompt" answer
          echoIs "and it said which key climbs out"
            "a child is not settable yet — DEL opens its parent" answer

    -- A CHILD'S OWN PARTS are editable, through the lens that materialized it:
    -- the write is aimed at that entry's extent rather than at the row's.
  , keyed shell "a child's paragraph writes the child's own extent"
      "Enter" ("press:n press:n press:n press:Enter press:n press:Enter"
                <> " dpara:reworded press:C-x press:C-s") $ \answer -> do
        assertEqual "aimed at the entry, not the row" ["r1#0"]
          =<< textsAt "wroteAt" answer
        assertEqual "carrying the child's own body"
                    ["** two :web:\nreworded\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer

    -- The row id is the SERVER's: it never reaches this page, so there is no
    -- row to warn about and no note to draw.  The file still has it, which
    -- TestQuery's lens group is what shows.
  , atBoot sheet "the identity property never reaches the panel" $ \answer -> do
        rows <- pairsAt "props" answer
        assertEqual "no row names it" [] [ r | r <- rows, take 1 r == ["ORG_GLANCE_ID"] ]

    -- TAB crosses the panes and nothing else, so the panel keeps its cursor:
    -- two stops, and the same key comes back to the row it left.
  , testCase "TAB crosses to the panel and back, and the cursor is remembered" $ do
      insheet "press:Tab" $ \answer -> do
        assertEqual "the panel has the keys" True =<< boolAt "pnav" answer
        assertEqual "with nothing focused, which is what frees the letters"
                    "" =<< textAt "focus" answer
        assertEqual "and the cursor on its first row" 0 =<< intAt "pat" answer
      insheet "press:Tab press:n press:Tab" $ \answer -> do
        assertEqual "back in the document" True =<< boolAt "dactive" answer
        assertEqual "the panel let go of the keys" False =<< boolAt "pnav" answer
        assertEqual "and kept where it had got to" 1 =<< intAt "pat" answer
      insheet "press:Tab press:n press:Tab press:Tab" $
        assertEqual "which is where the next crossing lands" 1 <=< intAt "pat"

    -- Two stops make the direction say nothing, so S-TAB is that one toggle
    -- rather than a second walk with an end of its own to fall off.
  , testCase "S-TAB is the same crossing, both ways" $ do
      insheet "press:S-Tab" $
        assertEqual "into the panel" True <=< boolAt "pnav"
      insheet "press:Tab press:S-Tab" $
        assertEqual "and out of it" True <=< boolAt "dactive"

    -- Nothing is focused in nav, so every printable key is free: both profiles'
    -- movement is bound at once, and the arrows ask for no profile at all.
  , testCase "nav moves on n/p, j/k and the arrows, and stops at the ends" $ do
      insheet "press:Tab press:n press:n" $ \answer -> do
        assertEqual "two rows down" 2 =<< intAt "pat" answer
        -- The panel holding the keys with nothing focused is a focus of its own
        -- as far as the map is concerned, or these letters would move the table
        -- under the sheet as well.
        assertEqual "and the table's own row did not move" 0 =<< intAt "cursor" answer
      insheet "press:Tab press:j press:j press:k" $
        assertEqual "vi's pair walks the same rows" 1 <=< intAt "pat"
      insheet "press:Tab press:ArrowDown press:ArrowDown press:ArrowUp" $
        assertEqual "and so do the arrows" 1 <=< intAt "pat"
      insheet "press:Tab press:p" $
        assertEqual "the first row is the end of the walk up" 0 <=< intAt "pat"
      insheet "press:Tab press:n press:n press:n press:n" $
        assertEqual "and the last property the end of the walk down" 3 <=< intAt "pat"

    -- Editing a row that is there is almost always editing its value; a
    -- planning row has no editable key at all, org owning that half of it.
  , testCase "RET opens the row at point, and a planning row opens its value" $ do
      insheet "press:Tab press:Enter" $
        assertEqual "the value of the planning row at point" "pval:0" <=< textAt "focus"
      insheet "press:Tab press:n press:n press:n press:Enter" $
        assertEqual "and of the property under them" "pval:3" <=< textAt "focus"

    -- One row, two fields: TAB has nothing else to mean inside an open row, so
    -- the pane crossing is suspended for as long as one is open.
  , testCase "TAB hops the open row's two fields rather than leaving" $ do
      insheet "press:Tab press:Enter press:Tab" $ \answer -> do
        assertEqual "over to the key" "pkey:0" =<< textAt "focus" answer
        assertEqual "and still in the panel" True =<< boolAt "pnav" answer
      insheet "press:Tab press:Enter press:Tab press:Tab" $
        assertEqual "and back to the value" "pval:0" <=< textAt "focus"
      insheet "press:Tab press:Enter press:S-Tab" $
        assertEqual "S-TAB is that same hop" "pkey:0" <=< textAt "focus"

  , keyed shell "RET commits the open row and goes back to nav"
      "Enter" "press:Tab press:n press:n press:n press:Enter pval:3=0:45 press:Enter" $
        \answer -> do
          panelIs "the row took the text its field was holding" [["EFFORT", "0:45"]] answer
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
  , keyed shell "a click under an open row commits the row that was opened"
      "Enter" ("press:Tab press:n press:n press:n press:Enter pval:3=0:45"
                <> " click:0 press:Enter") $ \answer -> do
        panelIs "the opened row took the text, and the clicked one is untouched"
                [["EFFORT", "0:45"]] answer
        assertEqual "the overlay closed" "" =<< textAt "focus" answer

    -- The same hazard from the other side: a click that lands on a row whose
    -- KEY the commit would have rewritten.  The add-row is the case with the
    -- most to lose — its key is the thing being typed — so a redirected commit
    -- would name a property after a planning keyword.
  , keyed shell "and a click cannot redirect the key an add-row is writing"
      "Enter" ("press:Tab press:+ pkey:4=OWNER pval:4=ada" <> " click:3 press:Enter") $
        panelIs "the added row took both fields and EFFORT stands"
                [["EFFORT", "0:30"], ["OWNER", "ada"]]

    -- `+' is the add affordance, and the whole of it: keyboard-first means the
    -- key IS the offer, where a row that is always empty was chrome every
    -- reader of the panel had to filter back out.
  , testCase "+ adds a property at the end and opens it" $ do
      insheet "press:Tab press:+" $ \answer -> do
        panelIs "an empty row at the end" [["EFFORT", "0:30"], ["", ""]] answer
        assertEqual "with the cursor on it" 4 =<< intAt "pat" answer
        assertEqual "open at its key, which is the thing being typed"
                    "pkey:4" =<< textAt "focus" answer
      insheet "press:Tab press:+ pkey:4=ADDED press:Enter" $ \answer -> do
        panelIs "and committing it is a property" [["EFFORT", "0:30"], ["ADDED", ""]] answer
        assertEqual "with nothing grown under it" 4 =<< intAt "pat" answer

    -- ESC over an open row is the ROW's, and puts back the text it was opened
    -- on; only from nav does the key reach the sheet's own ladder.
  , testCase "ESC puts an open row back, and the next one closes the sheet" $ do
      bootOf shell "" 500 "Enter"
             "press:Tab press:n press:n press:n press:Enter pval:3=0:45 press:Escape" $ \answer -> do
        panelIs "the value it was opened on" [["EFFORT", "0:30"]] answer
        assertEqual "the sheet is still up" "on" =<< textAt "modal" answer
        assertEqual "and back in nav" True =<< boolAt "pnav" answer
        assertEqual "with nothing written" ([] :: [Value]) =<< listAt "writes" answer
      bootOf shell "" 500 "Enter"
             "press:Tab press:n press:n press:n press:Enter press:Escape press:Escape" $
        assertEqual "the second one is the sheet's" "" <=< textAt "modal"

    -- What a sync sends is the committed panel, which is what makes the commit
    -- the thing that means yes.
  , keyed shell "a sync sends the panes apart, and an empty planning row is not one"
      "Enter" ("press:Tab press:n press:n press:n press:Enter pval:3=0:45"
                <> " press:Enter press:C-x press:C-s") $
        \answer -> do
          -- The BODY goes back whole, every byte of it: the panel moved and the
          -- document did not, so nothing in the text was touched.
          assertEqual "one write" [fixtureBody] =<< traverse (textAt "body")
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
  , keyed shell "an emptied planning row is an entry taken off"
      "Enter" "press:Tab press:Enter pval:0= press:Enter press:C-x press:C-s" $
        assertEqual "nothing left to write" [[]]
                    <=< wroteAt "planning"

    -- Emptying a key is how a property is deleted: there is no key to press for
    -- it, and none is owed — the row simply stops naming anything.
  , keyed shell "an emptied key is a property deleted"
      "Enter" ("press:Tab press:n press:n press:n press:Enter pkey:3="
                <> " press:Enter press:C-x press:C-s") $
        assertEqual "the drawer the write asks for" [[]]
                    <=< wroteAt "properties"

    -- C-c ' is org's `edit-special' rhyme.  It re-materializes rather than
    -- converting anything locally, which is what keeps an org parser out of
    -- this page: the raw text it shows is the server's `org', not a join done
    -- here.
  , testCase "C-c ' shows the raw subtree, and again shows the panes" $ do
      insheet "press:C-c press:'" $ \answer -> do
        assertEqual "the whole subtree, every region spelled out"
                    fixtureOrg =<< textAt "sheet" answer
        assertEqual "the panel is off the sheet" "raw" =<< textAt "shape" answer
        assertEqual "and the logbook strip with it" "" =<< textAt "logbook" answer
        echoIs "and the pill says which way it went" "C-c ' → org-edit-special (raw org)" answer
      insheet "press:C-c press:' press:C-c press:'" $ \answer -> do
        assertEqual "back to the document, and the textarea empty behind it"
                    "" =<< textAt "sheet" answer
        assertEqual "with both panes back" "" =<< textAt "shape" answer
        echoIs "the pill" "C-c ' → org-edit-special (structured document)" answer

    -- A re-read cannot carry unsaved work, and converting locally would need the
    -- parser this design exists to avoid.  So the toggle is refused, and says
    -- which key would let it through.
  , testCase "a dirty sheet is refused the toggle, in either pane" $ do
      insheet "press:C-c press:' sheet:hello press:C-c press:'" $
        \answer -> do
          assertEqual "the text stands" "hello" =<< textAt "sheet" answer
          assertEqual "and the shape with it" "raw" =<< textAt "shape" answer
          echoIs "named the key" "C-c ' → org-edit-special (sync first — C-x C-s)" answer
      bootOf shell "" 500 "Enter"
             ("press:Tab press:n press:n press:n press:Enter pval:3=0:45"
                <> " press:Enter press:C-c press:'") $
        \answer -> do
          assertEqual "a committed panel edit is dirty too" "" =<< textAt "shape" answer
          echoIs "same refusal" "C-c ' → org-edit-special (sync first — C-x C-s)" answer

    -- The other half of that rule: an edit nobody committed is not one, so the
    -- toggle goes through exactly as it would over a sheet nobody touched.
  , keyed shell "an open row is not an edit until it is committed"
      "Enter" ("press:Tab press:n press:n press:n press:Enter pval:3=0:45"
                <> " press:C-c press:'") $ \answer -> do
        assertEqual "the toggle went through" "raw" =<< textAt "shape" answer
        echoIs "and said so" "C-c ' → org-edit-special (raw org)" answer

    -- A remount takes the sheet down and puts it back: both panes, and the work
    -- in either of them.
  , keyed shell "a remount carries the panel across it"
      "Enter" ("press:Tab press:n press:n press:n press:Enter pval:3=0:45"
                <> " press:Enter close:view-changed") $
        \answer -> do
          assertEqual "mounted twice" 2 =<< intAt "mounts" answer
          panelIs "the panel is back, edit and all" [["EFFORT", "0:45"]] answer
          assertEqual "still dirty against the file, and still synced-looking"
                      "synced" =<< textAt "state" answer

    -- One pane, nothing to cross to: the key goes back to the browser, which is
    -- the whole of what raw mode changes here.
  , keyed shell "raw mode leaves TAB to the browser"
      "Enter" "press:C-c press:' press:Tab" $ \answer -> do
        assertEqual "the focus stayed in the text" "mtext" =<< textAt "focus" answer
        assertEqual "and the panel never took the keys" False =<< boolAt "pnav" answer
        assertBool "nor the key off the browser"
          . notElem "Tab" =<< textsAt "prevented" answer

    -- AND A BLURRED RAW SHEET STILL HOLDS THEM.  Clicking the sheet's own
    -- chrome takes the focus off its textarea without closing anything, and a
    -- surface that stopped counting there left every `table' row live under an
    -- open sheet — `d' among them, which flags the row BEHIND it for archiving.
    -- The sheet is a surface whenever it is up, in either shape.
  , testCase "a raw sheet keeps the keys with its textarea blurred" $ do
      insheet "press:C-c press:' blur press:d" $ \answer -> do
        assertEqual "nothing focused" "" =<< textAt "focus" answer
        assertEqual "and no row flagged behind the sheet"
                    ([] :: [T.Text]) =<< textsAt "flagged" answer
      -- What that costs is `q', which is scope `table': with a sheet up it is
      -- dead, so the sheet's doors are ESC and the backdrop.
      insheet "press:C-c press:' blur press:q" $
        assertEqual "and the sheet is still up" "on" <=< textAt "modal"

    -- ONE FOCUS LANGUAGE ACROSS BOTH PANES, and NEITHER focuses anything: each
    -- holds the keys with nothing focused, which is what leaves every printable
    -- key free to be movement and a command.  So the mark is the FRAME's on both
    -- sides — one class each — and it has to leave when the keys do, whichever
    -- way they go.
  , testCase "the pane holding the keys wears it, and only while it does" $ do
      insheet "" $ \answer -> do
        assertEqual "the document opens with the keys" True
          =<< boolAt "dactive" answer
        assertEqual "so the panel's frame is unmarked" False =<< boolAt "pnav" answer
        assertEqual "and nothing is focused at all" "" =<< textAt "focus" answer
      insheet "press:Tab" $ \answer -> do
        assertEqual "crossing marks the panel" True =<< boolAt "pnav" answer
        assertEqual "and unmarks the document" False =<< boolAt "dactive" answer
        assertEqual "with nothing focused either way" "" =<< textAt "focus" answer
      insheet "press:Tab press:Tab" $ \answer -> do
        assertEqual "crossing back unmarks the panel" False =<< boolAt "pnav" answer
        assertEqual "and the document has it again" True =<< boolAt "dactive" answer
      -- The leak this closes: the sheet used to clear the nav FLAG and leave
      -- the class on, so a panel closed from nav stayed marked behind the
      -- backdrop until the next materialize redrew it.
      insheet "press:Tab press:Escape" $ \answer -> do
        assertEqual "the sheet is closed" "" =<< textAt "modal" answer
        assertEqual "and both marks went with it" (False, False)
          =<< ((,) <$> boolAt "pnav" answer <*> boolAt "dactive" answer)

    -- Where the cursor was left belongs to the sheet that was open: the next
    -- materialize is a fresh drawer, read-only and at the top of itself.
  , keyed shell "the panel opens at the top again when the sheet is reopened"
      "Enter" "press:Tab press:n press:Escape press:Enter" $
        \answer -> do
          assertEqual "the cursor is back on the first row" 0 =<< intAt "pat" answer
          assertEqual "and the keys back in the body" False =<< boolAt "pnav" answer

    -- THE PANEL IS A MOUNT, and this is what that buys: the rows the reader
    -- moves over are the renderer's rows, the cursor is the renderer's
    -- selection, and this page keeps no copy of either.  The
    -- flag ground is its own opt-in now (flags: true, no mark column drawn),
    -- and the hint line is off, since the key line under the table names every
    -- key once.
  , atBoot sheet "the panel is a table-view mount of its own" $ \answer -> do
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
  , keyed shell "d flags the row at point rather than deleting it"
      "Enter" "press:Tab press:n press:n press:n press:d" $
        \answer -> do
          assertEqual "the mount is holding the flag" ["P0"]
                      =<< textsAt "pflagged" answer
          panelIs "and the drawer is untouched" [["EFFORT", "0:30"]] answer
          echoIs "the pill says what the second press will do"
            "d → delete-flag (d again deletes)" answer
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
        panelIs "the property is off the panel" [] answer
        assertEqual "the flag was spent with it" ([] :: [T.Text])
                    =<< textsAt "pflagged" answer
        echoIs "and the pill named the set" "D → org-delete-property (1 flagged)" answer
      insheet "press:Tab press:n press:n press:n press:D" $
        \answer -> do
          panelIs "D needs no flag: the row at point is the set" [] answer
          echoIs "and says so" "D → org-delete-property (row)" answer

    -- The three planning rows are org's keys rather than the author's, so a
    -- delete CLEARS the entry and the row stands — which is already how an entry
    -- is absent, and how the whole line comes off.
  , keyed shell "deleting a planning row clears the entry and keeps the row"
      "Enter" "press:Tab press:d press:d press:C-x press:C-s" $ \answer -> do
        panelIsAt "the row is still there, empty" "" [["EFFORT", "0:30"]] answer
        assertEqual "and the write carries no planning entry" [[]]
                    =<< wroteAt "planning" answer

    -- `u' is the way back off a flag, and it walks on the way the table's does.
  , keyed shell "u takes a flag off and steps on"
      "Enter" "press:Tab press:n press:n press:n press:d press:u press:D" $ \answer -> do
        assertEqual "nothing was flagged when D ran" ([] :: [T.Text])
                    =<< textsAt "pflagged" answer
        -- `u' stepped off the last row and stayed, so `D' took the row at point:
        -- the property, and not one of org's three.
        panelIs "so D took the row at point" [] answer

    -- A held `d' would flag a row and delete it from ONE press, which is the
    -- confirmation the two-press shape exists to be.  The dispatch's own ONCE
    -- list cannot reach a key this listener owns, so the guard is the panel's.
  , keyed shell "a held d flags once and never deletes what it flagged"
      "Enter" "press:Tab press:n press:n press:n press:d repeat:d repeat:d" $
        \answer -> do
        assertEqual "still flagged" ["P0"] =<< textsAt "pflagged" answer
        panelIs "and still there" [["EFFORT", "0:30"]] answer

    -- A deletion moves the model, so the sheet is dirty and the way out is a
    -- write — the same rule a committed edit answers to.
  , testCase "a deletion is an edit, and a cancelled one is not" $ do
      bootOf shell "" 500 "Enter"
             "press:Tab press:n press:n press:n press:d press:d press:C-x press:C-s" $
        \answer -> do
          assertEqual "the drawer the write asks for" [[]]
                      =<< wroteAt "properties" answer
          assertEqual "and it landed" "synced" =<< textAt "state" answer
      insheet "press:Tab press:n press:n press:n press:d press:Escape" $
        \answer -> do
          assertEqual "a flag alone writes nothing" ([] :: [Value])
                      =<< listAt "writes" answer
          assertEqual "and the sheet closed without one" "" =<< textAt "modal" answer

    -- ONE PAIR OF FIELDS, over whichever row is at point.  The mount rewrites
    -- its own rows as it scrolls, so an edit cannot live inside one — it sits
    -- over the panel and is anchored to the row the cursor is on, which is why
    -- opening a second row moves the same overlay rather than growing another.
  , keyed shell "the edit overlay is one pair of fields over the row at point"
      "Enter" ("press:Tab press:Enter press:Escape"
                <> " press:n press:n press:n press:Enter pval:3=0:45 press:Enter") $
        \answer -> do
          panelIs "the overlay went with the cursor" [["EFFORT", "0:45"]] answer
          assertEqual "and closed behind it" "" =<< textAt "focus" answer

    -- The hidden properties are not rowed, so they are not flaggable and no
    -- gesture can reach them.  The identity is the case that matters: a key that
    -- deleted it would break the row id every update is keyed off.
  , keyed shell "nothing hidden is rowed, so nothing hidden is flaggable"
      "Enter" "press:Tab press:n press:n press:n press:D" $
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
  where
    insheet = bootOf shell "" 500 "Enter"
    onTable = bootOf shell "" 500 ""

settingsSpec :: IO T.Text -> TestTree
settingsSpec shell =
  overBoot shell "," "" $ \settings ->
  testGroup "Shell settings"
  [ atBoot settings ", opens it over the layers the server serves" $ \answer -> do
        assertEqual "the sheet is up" "on" =<< textAt "settings" answer
        assertEqual "the first layer's lines, verbatim" "" =<< textAt "cshown" answer
        assertEqual "the union is previewed" "TODO | DONE" =<< textAt "ceff" answer
        assertEqual "and it opens synced" "synced" =<< textAt "cstate" answer
        assertEqual "with nothing written" ([] :: [Value]) =<< listAt "configWrites" answer

    -- ONE SELECT over the layers, system first and then the tags in their own
    -- alphabet.  The server's order is the walk's, so the sheet's is its own —
    -- the fixture serves `film' ahead of `book' precisely so the two differ.
  , atBoot settings "the layers are a select: system first, then the tags in alphabet"
      $ \answer -> do
        assertEqual "system, then book, then film"
                    ["system", "tag:book", "tag:film"] =<< textsAt "clayers" answer
        assertEqual "opening on the first" "0" =<< textAt "cat" answer
        assertEqual "and the label names the file it is"
                    "system · /o/.org-glance/config/system.org · not created yet"
          =<< textAt "clab" answer

    -- The one box is a VIEW of the selected layer, so picking another swaps what
    -- is in it and nothing else.
  , keyed shell "picking a layer swaps the box to that file's lines" "," "clayer:1" $ \answer -> do
        assertEqual "book's lines" "#+TODO: TODO READING | READ"
          =<< textAt "cshown" answer
        assertEqual "and book's label" "tag:book · /o/.org-glance/config/tags/book.org"
          =<< textAt "clab" answer
        assertEqual "with nothing written" ([] :: [Value]) =<< listAt "configWrites" answer

    -- THE RULE THE STACK OF BOXES USED TO GIVE FOR FREE: an edit belongs to its
    -- layer, and a reader who looks at another one comes back to it.
  , keyed shell "a switch away and back keeps the edit"
      "," "ctext:#+TODO:_A_|_B clayer:1 clayer:0" $ \answer -> do
        assertEqual "the edit is still there" "#+TODO:_A_|_B" =<< textAt "cshown" answer
        assertEqual "and nothing was written on the way" ([] :: [Value])
          =<< listAt "configWrites" answer

    -- READING A LAYER IS NOT EDITING IT.  Walking the whole select and coming
    -- back is the shape a reader looking for one tag makes, and every layer's
    -- bytes have been through the box by the end of it: nothing may be written,
    -- and what is on screen must be the file's own text down to the spacing.
  , keyed shell "walking every layer and back writes nothing"
      "," "clayer:1 clayer:2 clayer:0 press:Escape" $ \answer -> do
        assertEqual "no write" ([] :: [Value]) =<< listAt "configWrites" answer
        assertEqual "the sheet is down" "" =<< textAt "settings" answer
  , keyed shell "and the box shows a layer's lines byte for byte" "," "clayer:2 clayer:1" $
        assertEqual "book's line, spacing and bar included"
                    "#+TODO: TODO READING | READ" <=< textAt "cshown"

    -- And every layer edited on the way is written, one drift-locked call per
    -- FILE — which is what the boxes were doing and what one box must not lose.
  , keyed shell "every layer edited is written, one call each"
      "," "ctext:#+TODO:_A_|_B clayer:2 ctext:#+TODO:_C_|_D press:Escape" $
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
  , atBoot settings "it is three panels, each under its own header" $
        assertEqual "general, theme, keywords" ["general", "theme", "keywords"]
          <=< textsAt "csecs"

    -- THE LOG KNOB, the general panel's one field that asks no server: it is a
    -- `localStorage' preference like the theme, it applies as it is typed, and
    -- the number lands on the strip itself where the stylesheet's arithmetic
    -- reads it.
  , keyed shell "the log knob applies as it is typed, and is remembered"
      "," "clog:12" $ \answer -> do
        assertEqual "the cap is on the strip" "12" =<< textAt "logn" answer
        assertEqual "and remembered" "12" =<< textAt "logStored" answer
        assertEqual "the sheet is still up" "on" =<< textAt "settings" answer
        assertEqual "and nothing was written" ([] :: [Value])
          =<< listAt "configWrites" answer

    -- The default is the stylesheet's declared value, so a page nobody has
    -- touched shows seven and stores nothing.
  , atBoot settings "and it opens on seven, with nothing stored" $ \answer -> do
        assertEqual "the boot wrote the default" "7" =<< textAt "logn" answer
        assertEqual "the field is empty" "" =<< textAt "clog" answer
        assertEqual "and the key is not there" "«unset»" =<< textAt "logStored" answer

    -- THE BOOT READS THE PREFERENCE, which no act can reach: every act runs
    -- after the page has already applied it, so the browser has to arrive
    -- remembering one.
  , keyedWith shell "glance-log=21" "" 500 "a browser that remembers one boots at it"
      "" "" $ \answer -> do
        assertEqual "the cap is the stored one" "21" =<< textAt "logn" answer
        assertEqual "and the sheet shows it" "" =<< textAt "clog" answer
  , keyedWith shell "glance-log=21" "" 500 "and the sheet opens on it" "," "" $
        assertEqual "the field is the stored value" "21" <=< textAt "clog"

    -- A stored value the band no longer takes — an older build's, a hand-edited
    -- one — falls back rather than being applied.
  , keyedWith shell "glance-log=900" "" 500 "a stored value outside the band boots at the default"
      "" "" $
        assertEqual "the default" "7" <=< textAt "logn"

    -- Emptying it is how a reader asks for the default back, which is why blank
    -- is a value this page takes rather than one it refuses.  What is stored is
    -- NOTHING, since a preference spelling the empty string is a preference.
  , keyedWith shell "glance-log=12" "" 500 "blanking it restores the default and removes the preference"
      "," "clog:" $ \answer -> do
        assertEqual "back to seven" "7" =<< textAt "logn" answer
        assertEqual "with the key gone" "«unset»" =<< textAt "logStored" answer

    -- A value outside the band is DECLINED rather than clamped: the cap a reader
    -- had stands, and the box is redrawn from the preference on the next open.
  , keyed shell "a value outside the band is declined, and the cap stands"
      "," "clog:12 clog:999" $ \answer -> do
        assertEqual "the cap did not move" "12" =<< textAt "logn" answer
        assertEqual "nor did the storage" "12" =<< textAt "logStored" answer
  , keyed shell "and so is a value that is no number at all"
      "," "clog:12 clog:tall clog:0 clog:-3 clog:3.5" $ \answer -> do
        assertEqual "the cap did not move" "12" =<< textAt "logn" answer
        assertEqual "nor did the storage" "12" =<< textAt "logStored" answer

    -- Reopening draws the stored preference over whatever was left in the box,
    -- which is what makes a refused value cost nothing past the keystroke.
  , keyed shell "reopening draws the preference back over a refused value"
      "," "clog:12 clog:999 press:Escape press:," $ \answer -> do
        assertEqual "the field shows the preference" "12" =<< textAt "clog" answer
        assertEqual "and the cap is still it" "12" =<< textAt "logn" answer

    -- The theme is a preference rather than a write: it applies as it is
    -- picked, it is stored, and the sheet it was picked in stays where it is.
  , keyed shell "the theme panel applies and persists without closing the sheet"
      "," "theme:dark" $ \answer -> do
        assertEqual "stamped on the document element" "dark" =<< textAt "theme" answer
        assertEqual "and remembered" "dark" =<< textAt "themeStored" answer
        assertEqual "the sheet is still up" "on" =<< textAt "settings" answer
        assertEqual "and nothing was written" ([] :: [Value])
          =<< listAt "configWrites" answer

    -- `auto' is the attribute coming OFF rather than a third value written into
    -- it, which is what lets the media query decide again.
  , keyed shell "and auto takes the attribute back off" "," "theme:dark theme:auto" $ \answer -> do
        assertEqual "no attribute" "" =<< textAt "theme" answer
        assertEqual "but the choice is remembered" "auto" =<< textAt "themeStored" answer

    -- The focus rule, both halves.  A `SELECT' inside a popup KEEPS the focus —
    -- the popup is a legitimate holder and the table's keys are dead under it —
    -- and the way the keys come back is closing the popup, which is what a
    -- hand-written `blur()' on a control outside one was standing in for.
  , keyed shell "the sheet's theme select keeps the keys away from the table"
      "," "theme:dark press:n" $ \answer -> do
        assertEqual "the select holds the keyboard" "SELECT" =<< textAt "holding" answer
        rowIs "and the table did not move" "r1" answer
  , keyed shell "and closing it is what gives them back"
      "," "theme:dark press:Escape press:n" $ \answer -> do
        assertEqual "the sheet is down" "" =<< textAt "settings" answer
        assertEqual "nothing holds the keyboard" "" =<< textAt "holding" answer
        rowIs "and the key moved the cursor" "r2" answer

    -- The sheet's own rule, and the reason it has no buttons: the way out is
    -- the save.  Only the layer that moved is written.
  , keyed shell "ESC syncs the layers that moved and closes"
      "," "ctext:#+TODO:_TODO_STARTED_|_DONE press:Escape" $
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

  , keyed shell "a pristine sheet closes without asking the server for anything"
      "," "press:Escape" $ \answer -> do
        assertEqual "no write" ([] :: [Value]) =<< listAt "configWrites" answer
        assertEqual "the sheet is down" "" =<< textAt "settings" answer

    -- The general panel's two fields are `system.org''s two tree-wide LINES,
    -- drawn under their own header and posted in that layer's own write: one
    -- file, one digest, one splice, wherever on the sheet they are shown.
  , keyed shell "the capture target is a general field, and rides the system write"
      "," "ccap:notes/in.org press:Escape" $ \answer -> do
        writes <- listAt "configWrites" answer
        assertEqual "one write, for the layer that moved" 1 (length writes)
        assertEqual "the system layer" "/o/.org-glance/config/system.org"
          =<< textAt "path" (head writes)
        assertEqual "carrying the target" "notes/in.org" =<< textAt "capture" (head writes)
        assertEqual "and the server holds it now" "notes/in.org"
          =<< textAt "servedCapture" answer

  , keyed shell "and it opens on what the server serves"
      "," "ccap:notes/in.org press:C-x press:C-s" $
        assertEqual "the field shows what was typed" "notes/in.org" <=< textAt "ccap"

    -- The default view is the other one, and it takes the same road: a general
    -- field, the system layer's write.
  , keyed shell "the default view is the other general field, on the same write"
      "," "cview:tag:work press:Escape" $ \answer -> do
        writes <- listAt "configWrites" answer
        assertEqual "one write" 1 (length writes)
        assertEqual "carrying the view" "tag:work" =<< textAt "filter" (head writes)
        assertEqual "and the server holds it now" "tag:work" =<< textAt "served" answer

    -- Two sheets over one page would leave `C-x C-s' and `ESC' guessing which
    -- one they meant.  `typing()' is not what keeps them apart, which is the
    -- point of the case: a click on the open sheet's own header blurs its
    -- textarea, and every `table' row is live again the moment it does.  So the
    -- refusal is stated in `openSettings' rather than left to the focus.
  , keyed shell "it will not open over the materialize sheet" "Enter" "blur press:," $ \answer -> do
        assertEqual "the settings sheet stayed down" "" =<< textAt "settings" answer
        assertEqual "and the subtree is still the one open" "on"
          =<< textAt "modal" answer

    -- AND IT IS A SURFACE WHILE IT STANDS, which is the half `openSettings''s
    -- refusal cannot cover.  The sheet opens with a field focused, so `typing()'
    -- used to catch it by the FOCUS — and a click on the sheet's own chrome
    -- takes that focus away without closing anything, which left every `table'
    -- row live under an open settings sheet, `d' among them: one press flags the
    -- row behind it and the next archives it.  The sheet is one entry in
    -- `SURFACES' now, so it is up whether or not anything in it is focused.
  , keyed shell "the settings sheet holds the keys with its fields blurred"
      "," "blur press:d" $ \answer -> do
        assertEqual "the sheet is up" "on" =<< textAt "settings" answer
        assertEqual "with nothing focused" "" =<< textAt "focus" answer
        assertEqual "and no row flagged behind it"
                    ([] :: [T.Text]) =<< textsAt "flagged" answer
        assertEqual "nor anything written" [] =<< postedOf answer

  , keyed shell "C-x C-s syncs mid-edit and leaves the sheet open"
      "," "clayer:1 ctext:#+TODO:_A_|_B press:C-x press:C-s" $
        \answer -> do
          assertEqual "one write" 1 . length =<< listAt "configWrites" answer
          assertEqual "the sheet is still up" "on" =<< textAt "settings" answer
          assertEqual "and it is synced again" "synced" =<< textAt "cstate" answer

    -- A file that moved under the sheet is a 409 and the sheet stays open at
    -- `conflict', where C-x C-s overwrites and ESC discards — the materialize
    -- sheet's flow, over config files.
  , keyed shell "a layer that moved underneath lands at conflict, and ESC discards"
      "," "clayer:1 ctext:#+TODO:_A_|_B cmoved press:C-x press:C-s" $
        \answer -> do
          assertEqual "the write was refused" 1 . length =<< listAt "configWrites" answer
          assertEqual "the sheet waits" "conflict" =<< textAt "cstate" answer
          assertEqual "and is still up" "on" =<< textAt "settings" answer
  , keyed shell "and the second ESC there closes it without writing"
      "," "clayer:1 ctext:#+TODO:_A_|_B cmoved press:C-x press:C-s press:Escape" $
        \answer -> do
          assertEqual "no second write" 1 . length =<< listAt "configWrites" answer
          assertEqual "the sheet is down" "" =<< textAt "settings" answer

    -- `C-x C-s' SYNCS MID-EDIT, so the reader is still typing while the write is
    -- out — and a flush that landed must leave the box exactly as they left it.
    -- The old stack of boxes could not get this wrong; one box redrawn from the
    -- text the flush snapshotted can.
  , keyed shell "a sync that lands does not paint over what is being typed"
      "," "clayer:1 ctext:#+TODO:_A_|_B chang press:C-x press:C-s\
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
  , keyed shell "a 409 selects the layer it refused and names it"
      "," "clayer:1 ctext:#+TODO:_A_|_B clayer:2 cmoved press:C-x press:C-s" $
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
  , atBoot settings "a layer the sheet creates stops saying it is not there yet" $
        assertEqual "the system layer has no file behind it"
                    "system · /o/.org-glance/config/system.org · not created yet"
          <=< textAt "clab"
  , keyed shell "and the write is what takes the words off"
      "," "ctext:#+TODO:_A_|_B press:C-x press:C-s" $ \answer -> do
        assertEqual "the label is the path alone"
                    "system · /o/.org-glance/config/system.org" =<< textAt "clab" answer
        assertEqual "and the box was left as it was" "#+TODO:_A_|_B"
          =<< textAt "cshown" answer

    -- A refusal describes a WRITE, so an edit taken back takes its refusal with
    -- it: the layer matches the file again and there is nothing left to explain.
  , keyed shell "reverting an edit drops the refusal it earned"
      "," "ctext:#+TODO:_A_|_B cmoved press:C-x press:C-s ctext: press:C-x press:C-s" $
        \answer -> do
          assertEqual "one write, the refused one" 1 . length
            =<< listAt "configWrites" answer
          assertEqual "the line under the box is gone" "" =<< textAt "clerr" answer
          assertEqual "and the sheet is synced" "synced" =<< textAt "cstate" answer

    -- The one that matters most here: writing a layer is what moves the
    -- columns, so the close that follows a successful save is `view-changed'.
    -- The sheet is a sibling of `#app' and outlives the remount by where it
    -- sits — asserted rather than assumed, since it is a layout fact.
  , keyed shell "a view-changed remount leaves the sheet standing"
      "," "clayer:1 ctext:#+TODO:_A_|_B close:view-changed" $
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
    keyed shell "opens on a boot line the mount leaves alone" "" "" $ \answer -> do
        strip <- logOf answer
        assertEqual "one line, the boot's" [("info", "boot", "loading …")]
                    (map cut strip)
        assertBool ("a clock opens it: " <> show strip)
                   (all (stamped . stampOf . snd) strip)

    -- Every line, whatever wrote it: a clock, one of the three severities —
    -- SPELLED uppercase in the line and WORN lowercase as its class, so the
    -- colour and the word can never disagree — and one of the six scopes.
  , keyed shell "every line is a stamp, a severity and a scope"
      "d q" "offline close:resync" $ \answer -> do
        strip <- logOf answer
        -- Every assertion below is quantified over the strip, so an EMPTY one
        -- passes all four.  The acts above write a `cmd', a `ws' and a boot
        -- line, and the sweeps elsewhere in this file all guard the same way:
        -- a gate that found nothing to check is not a gate that passed.
        assertBool "the acts wrote lines to sweep" (length strip >= 3)
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
  , keyed shell "the ring holds five hundred and drops from the front" "" "spam:501" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "capped" 500 (length strip)
        assertEqual "the boot line and `line 0' are what went"
                    ["line 1", "line 2"] [ m | (_s, _c, m) <- take 2 strip ]
        assertEqual "and the newest stands" ["line 500"]
                    [ m | (_s, _c, m) <- drop 499 strip ]

    -- The one mutation an append-only strip allows: a message identical to the
    -- one before it is counted on that line.  A retry loop otherwise fills the
    -- ring with one sentence and takes everything else out of reach.
  , keyed shell "a repeat is counted on its line rather than written under it"
      "q q q" "" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "the boot line and one more" 2 (length strip)
        assertEqual "counted"
                    [("info", "cmd", "q closes the sheet; there is no window to quit ×3")]
                    (drop 1 strip)

    -- A message that is not the LAST one is a new line, so a repeat interrupted
    -- by anything else starts counting again rather than reaching back.
  , keyed shell "and only against the line it follows" "q d q" "" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "three lines under the boot's" 4 (length strip)
        assertEqual "the last says it once, uncounted"
                    "q closes the sheet; there is no window to quit"
                    (message (last strip))

    -- The connection's two severities, over a daemon that went away: the fetch
    -- that failed is an error and the retry behind it is a warning.
  , keyed shell "a dead daemon logs the failure and the retry"
      "" "offline close:resync" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "both, in that order"
                    [ ("error", "ws", "load failed: fetch failed")
                    , ("warn", "ws", "disconnected · retrying in 1s") ]
                    (drop 1 strip)

    -- dired's flag, said in words: the pill says what the key did and the strip
    -- says which row it did it to, which is the half that survives the next
    -- keystroke.
  , keyed shell "d names the row it flagged, and u names it unflagging one" "d u" "" $ \answer -> do
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
  , keyed shell "a refused write is an error line and names no landing"
      "" "refuse press:D" $ \answer -> do
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

-- | An edit overlay's cell resolution, likewise driven as the pure function it
-- is: the KEYS its shape names, resolved against COLUMNS, come out as
-- @\"FROM,TO\"@ — or @«none»@ where one of them names no column there.
resolves :: IO T.Text -> ([T.Text], [T.Text], T.Text) -> Assertion
resolves shell (keys, cols, expected) =
  bootOf shell "" 500 ""
         ("cells:" <> T.intercalate "," keys <> "@" <> T.intercalate "," cols)
         (assertEqual (show keys <> " over " <> show cols) (Just expected)
            <=< maybeTextAt "span")

-- | The KEYS of the columns a popup's shape is resolved against, out of the
-- SERVER's own declaration — so what the cases below check is the real list
-- rather than a copy of it here.
columnKeys :: [Value] -> IO [T.Text]
columnKeys = traverse (textAt "key")

-- | WHERE AN EDIT OVERLAY LANDS, resolved BY KEY.  A shape names the columns it
-- covers and the resolution turns them into the indices the placement reads, so
-- a column list that moves takes the box with it — where the shape used to
-- carry a positional pair and reordering those columns put the box over the
-- wrong cells, greenly.  The rule is pure and order-only, which is what lets it
-- be checked against the server's own declaration under the harness, over no
-- page at all.
cellSpanSpec :: IO T.Text -> TestTree
cellSpanSpec shell = testGroup "Shell cell resolution"
  [ testCase "the two popups' own shapes, against the columns the server declares" $ do
      links <- columnKeys linkColumns
      tags <- columnKeys tagColumns
      -- The link popup edits the description and the target; `type' is derived
      -- and leads the list, which is exactly why the pair is not 0,1.
      resolves shell (["title", "url"], links, "1,2")
      -- And the tags popup edits the tag cell alone, a run of one.
      resolves shell (["title"], tags, "0,0")

  , testCase "an unknown key resolves to nothing, so the placement is a no-op" $ do
      links <- columnKeys linkColumns
      resolves shell (["title", "nosuchcolumn"], links, "«none»")
      resolves shell (["nosuchcolumn"], links, "«none»")
      -- Naming no column at all is the same answer: there is nothing to cover.
      resolves shell ([], links, "«none»")

    -- The run is drawn from one EDGE to the other, so it is the columns' order
    -- rather than the shape's: a shape spelling its keys the other way round
    -- means the same two cells and gets the same box.
  , testCase "the run follows the columns' order, whatever order the shape spelled" $ do
      links <- columnKeys linkColumns
      resolves shell (["url", "title"], links, "1,2")
      resolves shell (["type", "url"], links, "0,2")
  ]

-- | SHELL's glue booted under node on SEARCH, with the server reporting TOTAL
-- matches, KEYS pressed over the table once it settled and ACTS run after
-- those, then CHECK over the harness's whole answer.  A machine with no node
-- runs nothing and passes: the boot is checked wherever there is one, and the
-- glue group still reads the same page as text.
bootOf :: IO T.Text -> T.Text -> Int -> T.Text -> T.Text -> (Value -> Assertion)
       -> Assertion
bootOf shell = bootWith shell ""

-- | LABEL's case over a boot of the default page — no search, 500 matches —
-- with KEYS pressed and ACTS run.  The page the great majority of these cases
-- want, so the two lines that spelled it are one.
keyed :: IO T.Text -> String -> T.Text -> T.Text -> (Value -> Assertion) -> TestTree
keyed shell label keys acts = testCase label . bootOf shell "" 500 keys acts

-- | 'keyed' over a page the boot asked SEARCH for, with the server reporting
-- TOTAL matches.
keyedAt :: IO T.Text -> T.Text -> Int -> String -> T.Text -> T.Text
        -> (Value -> Assertion) -> TestTree
keyedAt shell search total label keys acts =
  testCase label . bootOf shell search total keys acts

-- | 'keyedAt' over a browser that already remembers STORE.
keyedWith :: IO T.Text -> T.Text -> T.Text -> Int -> String -> T.Text -> T.Text
          -> (Value -> Assertion) -> TestTree
keyedWith shell store search total label keys acts =
  testCase label . bootWith shell store search total keys acts

-- | 'bootOf' over a browser that already REMEMBERS something: STORE is
-- @KEY=VALUE@ pairs joined by commas, seeded into @localStorage@ ahead of the
-- glue.  A preference the BOOT reads is unreachable from an act, every act
-- running after the page has already applied it.
bootWith :: IO T.Text -> T.Text -> T.Text -> Int -> T.Text -> T.Text
         -> (Value -> Assertion) -> Assertion
bootWith shell store search total keys acts check =
  reading check =<< bootedPage shell store search total keys acts

-- | The harness's answer to one boot, without a check over it: 'Nothing' where
-- there is no node, and the harness's own complaint as a 'Left'.  Named apart
-- from 'bootWith' so 'overBoot' can acquire ONE answer for a run of cases that
-- drive the same page — the boot is a temp directory, two writes and a node
-- process, and four cases reading four fields of one page paid for four of them.
bootedPage :: IO T.Text -> T.Text -> T.Text -> Int -> T.Text -> T.Text
           -> IO (Maybe (Either String Value))
bootedPage shell store search total keys acts = do
  node <- findExecutable "node"
  case node of
    -- SAY SO.  331 of this file's cases route through here, and a machine with
    -- no node ran every one of them green having asserted nothing at all.
    -- 'TestDefaults.withCorpusSample' answers the same silence the same way.
    Nothing  -> Nothing <$ hPutStrLn stderr "\nSKIPPED - node is not on PATH: shell boot"
    Just exe -> withTempDir $ \dir -> do
      page <- shell
      glueOf page >>= TIO.writeFile (dir </> "shell.js")
      keysOf page >>= TIO.writeFile (dir </> "keys.json")
      (code, out, err) <- readProcessWithExitCode exe
                            [ harness, dir, T.unpack search, show total
                            , T.unpack keys, T.unpack acts, T.unpack store ] ""
      pure . Just $ case code of
        ExitSuccess -> either (\e -> Left ("the harness answered: " <> e)) Right
                              (eitherDecode (BL.fromStrict (TE.encodeUtf8 (T.pack out))))
        _failed     -> Left ("the boot harness said: " <> err)

-- | Run CHECK over a 'bootedPage' answer: nothing to check where there was no
-- node, and the harness's complaint as the failure.
reading :: (Value -> Assertion) -> Maybe (Either String Value) -> Assertion
reading check = maybe (pure ()) (either assertFailure check)

-- | Run K under ONE boot of the default page with KEYS and ACTS, acquired once
-- for every 'atBoot' case in the tree K builds and released after the last.
-- Wraps a group rather than nesting one inside it, so no case is renamed and
-- none is merged into another: what goes is the repeated node process.
overBoot :: IO T.Text -> T.Text -> T.Text
         -> (IO (Maybe (Either String Value)) -> TestTree) -> TestTree
overBoot shell keys acts =
  withResource (bootedPage shell "" "" 500 keys acts) (const (pure ()))

-- | LABEL's case over the page an enclosing 'overBoot' booted.
atBoot :: IO (Maybe (Either String Value)) -> String -> (Value -> Assertion) -> TestTree
atBoot page label check = testCase label (reading check =<< page)

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
              -- A held priority key would walk the ring round and land wherever
              -- the repeat count left it, which is the reversing key's problem
              -- one ring wider.
            , "priority-up", "priority-down"
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
  ([ testCase glLabel $ do
       b <- shell
       holdsAll glLabel glHas b
       holdsNone glLabel glGone b
   | Glue{..} <- shellGlue ]
   <> [ groundSweep shell, tierSweep shell, gridSweep shell, editIndentSweep shell
      , scrollSweep shell, containSweep shell, logColumnSweep shell ])

-- | THE EDIT BOX IS THE BLOCK, WEARING A DIFFERENT GROUND.  `RET' over a
-- paragraph must move NOTHING: the textarea takes the block's font, its line
-- height, all four paddings — the grid inset and the title-column indent among
-- them — its full width and no margin, and draws no border and no outline.  The
-- ground and the caret are the whole of the signal.
--
-- Asserted as RELATIONS over the declarations rather than as copied strings: the
-- box must READ the block's own expressions (`font:inherit', `--g-doc-pad',
-- the indent `calc'), since a figure restated here is exactly the drift this
-- exists to catch — a literal @13px\/1.5@ against the pane's
-- @--g-doc-fs@\/@--g-doc-lh@ put every line after the first on another rhythm.
-- A CSS sweep cannot measure geometry; it pins what the page DECLARES.
editIndentSweep :: IO T.Text -> TestTree
editIndentSweep shell = testCase "the paragraph's edit box is the block it covers" $ do
  page <- shell
  para <- need "the paragraph's indent"
               (between "  .d-para,.d-comp{margin:.5em 0;\n    padding-left:" "}" page)
  box <- need "the edit box's rule" (between "  #dpara textarea{" "}" page)
  assertBool ("the box is indented by what the block is: " <> T.unpack box)
             (("padding-left:" <> para) `T.isInfixOf` box)
  -- The block's own type, read rather than restated.
  assertBool ("the box takes the block's type: " <> T.unpack box)
             ("font:inherit" `T.isInfixOf` box)
  -- The block's own grid inset, on the axes the indent does not name, and the
  -- block's own wrapping, so a long token breaks in the same place.
  assertBool ("the box takes the grid inset: " <> T.unpack box)
             ("padding:1px var(--g-doc-pad)" `T.isInfixOf` box)
  assertBool ("the box takes the block's wrap: " <> T.unpack box)
             ("overflow-wrap:anywhere" `T.isInfixOf` box)
  -- And nothing of its own that would move the text or draw a line — a floor of
  -- its own included, since `placeEdit' sizes the box off the block.
  mapM_ (\decl -> assertBool ("the box declares " <> T.unpack decl <> ": " <> T.unpack box)
                             (decl `T.isInfixOf` box))
        ["width:100%", "margin:0", "border:none", "resize:none"]
  assertEqual "a figure the box restates instead of reading" []
              [ n | n <- ["13px", "12px", "1.5 var", "padding:1px 6px", "min-height:2em"]
                  , n `T.isInfixOf` box ]
  -- REGISTRATION, which the declarations above cannot give: the overlay is
  -- absolutely positioned against the pane's PADDING box while the text it
  -- covers sits inside the CONTENT box, so it has to answer for the pane's own
  -- horizontal inset — and `placeEdit' has to take the pane's border and its
  -- scroll offset back out of the vertical.  A bare delta put the box 10px left
  -- and 1px high over `#mdoc', and walked it further on every scroll.
  span' <- need "the document overlays' span" (between "  #dedit,#dpara{" "}" page)
  assertEqual "the document overlays span the pane's content box"
              "left:var(--g-doc-padx);right:var(--g-doc-padx)" span'
  assertBool "the pane's inset is one name, read by both"
             ("padding:var(--g-doc-pady) var(--g-doc-padx)" `T.isInfixOf` page)
  assertBool "the placement takes the pane's border and scroll back out"
             ("a.top - b.top - pane.clientTop + pane.scrollTop" `T.isInfixOf` page)
  -- FOCUS DRAWS NO LINE.  The three CELL overlays keep their underline; the
  -- document's box is read as text and must not grow one.
  focus <- need "the box's focus rule" (between "  #dpara textarea:focus{" "}" page)
  assertEqual "a line the document box would grow on focus" []
              [ n | n <- ["border-bottom-color", "border-bottom:"], n `T.isInfixOf` focus ]
  -- THE GROUND IS THE SIGNAL, and it is one the block is not already wearing:
  -- the edit opens on the document CURSOR, which is `--g-sel' already.
  ground <- need "the box's ground" (between "  #dpara{" "}" page)
  assertEqual "the edit ground is the page's input surface"
              "background:var(--g-surface)" ground
  where need what = maybe (assertFailure ("no " <> what <> " in the page")) pure

-- | THE LOG'S SEVERITY AND SCOPE ARE COLUMNS, each as wide as its own longest
-- word, so every message in the strip starts at one x position.
--
-- Derived rather than copied: the words are gathered off the page's OWN
-- @append@ calls and the longest of each is measured in characters, so a scope
-- longer than @config@ — or a severity beyond @error@ — fails here rather than
-- quietly wrapping the strip.  The vocabulary is not a list in the code, and
-- this is what stands in for one.
logColumnSweep :: IO T.Text -> TestTree
logColumnSweep shell = testCase "the log's severity and scope are columns" $ do
  page <- shell
  let calls  = [ (scope, sev)
               | rest <- drop 1 (T.splitOn "append(\"" page)
               , (scope, more) <- [T.breakOn "\"" rest]
               , Just after <- [T.stripPrefix "\", \"" more]
               , (sev, _end) <- [T.breakOn "\"" after]
               , not (T.null scope), T.all isLower scope
               , not (T.null sev), T.all isLower sev ]
      widest = maximum . map T.length
  assertBool "the page makes log lines at all" (length calls > 4)
  assertEqual "the severity column is as wide as the longest severity"
              5 (widest (map snd calls))
  assertEqual "and the scope column as wide as the longest scope"
              6 (widest (map fst calls))
  mapM_ (\needle -> assertContains "the column is declared" needle page)
    [ "  #log .lv,#log .lc{display:inline-block}"
    , "  #log .lv{width:5ch}", "  #log .lc{width:6ch}" ]

-- | ONE @scrollIntoView@, AND IT IS THE DOCUMENT'S.  The call is forbidden over
-- the TABLE's rows, which belong to the renderer — it owns their scroller,
-- their page and their selection, so reaching into them that way is this page
-- working around an interface it already has.  The document's rows are the
-- SHELL's, drawn into a scroller this page declares, so the call is ordinary
-- there and the movement code needs it.
--
-- The distinction is kept by COUNTING rather than by wording: one occurrence,
-- and it is the document cursor's.  A second has nowhere legitimate to be, so
-- it would have to be a reach into something this page does not own.
scrollSweep :: IO T.Text -> TestTree
scrollSweep shell = testCase "the one scrollIntoView is the document's own" $ do
  page <- shell
  code <- glueOf page
  assertEqual "exactly one call site" 1 (T.count "scrollIntoView(" code)
  assertEqual "named twice: the call, and the detect that guards it" 2
              (T.count "scrollIntoView" code)
  assertContains "and it is the document cursor's"
    "        row.scrollIntoView({ block: \"nearest\" });" page
  -- THE BAND IS CSS, which is what keeps that one call one call.  `nearest'
  -- honours `scroll-margin', so the scrolloff is declared on the elements and
  -- the movement code measures nothing.
  assertContains "the band rides the elements" "  .de{scroll-margin-block:var(--g-doc-off);" page
  -- And it is counted in the pane's OWN lines rather than in pixels, off the
  -- two numbers the pane is set in — the same relation-not-copy discipline the
  -- star gutter and the body indent are held to.
  assertContains "three of the pane's lines"
    "    --g-doc-off:calc(3 * var(--g-doc-fs) * var(--g-doc-lh));" page
  assertContains "and the pane is set in those same two"
    "    font:var(--g-doc-fs)/var(--g-doc-lh) var(--dk-mono);" page

-- | A POPUP CLAMPS AT ITS BOUND AND SCROLLS INSIDE IT.  The bound is one figure
-- and every tier reads it; what makes it hold is the CHAIN from the bounded box
-- down to whichever element owns each scroll, and a chain is exactly what a
-- stray declaration breaks silently — the box keeps its @max-height@ and the
-- content paints straight past it.
--
-- Two links are pinned by their absence as much as their presence:
--
-- * The panes ROW carries @overflow:hidden@.  @flex:1;min-height:0@ lets the
--   row be SIZED by the box; it does not stop the row's own content painting
--   past that size.  Under @flex-wrap@ the flex LINE is content-sized and
--   @align-items:stretch@ stretches the panes to the LINE rather than to the
--   box, so a tall subtree grew the line and the line escaped — with the panes'
--   own @overflow@ never coming into it, their heights never having been
--   bounded.
--
-- * NO PANE CARRIES A FLOOR.  A @min-height@ on a flex child is a refusal to
--   shrink, which is the classic way a bounded box is pushed open from inside.
--   The measure is the TIER's (@.pop-sheet@), where it is one number.
containSweep :: IO T.Text -> TestTree
containSweep shell = testCase "every popup clamps, and scrolls inside" $ do
  page <- shell
  -- THE BOUND IS CAPPED. A viewport tall enough makes the arithmetic exceed
  -- what a reader can take in, so 90vh is the ceiling whatever it works out to.
  assertContains "the bound caps at 90vh"
    "    --g-pop-max:min(90vh," page
  -- RULE-SCOPED, the way `tierSweep' and `groundSweep' read their rules: each
  -- declaration is looked for inside the BODY of the selector that owes it.  A
  -- flat `isInfixOf' over the page cannot say which rule answered, and one of
  -- these pairs could not be anchored at all — `#mdoc' and `#mprops' open with
  -- the same three declarations, so the document pane's needle was satisfied by
  -- the panel and the document was never asserted at all.
  --
  -- WHAT IT SWEPT IS ASSERTED FIRST, so a renamed or regrouped selector fails
  -- loudly rather than passing over nothing.
  --
  -- EVERY rule the selector appears in, not the first: a box's declarations are
  -- split between the card rule every working surface SHARES and the short one
  -- saying what only it wants, and the question is what the selector ends up
  -- declaring.
  let swept = [ (sel, bodies) | (sel, _decls) <- clamps
              , let bodies = rulesIn sel page, not (null bodies) ]
  assertEqual "the sweep found a rule for every selector it names"
              (map fst clamps) (map fst swept)
  mapM_ (\((sel, decls), (_sel, bodies)) ->
           mapM_ (\d -> assertBool
                    (T.unpack (sel <> " no longer declares " <> d <> ": "
                                 <> T.intercalate " | " bodies))
                    (any (d `T.isInfixOf`) bodies))
                 decls)
        (zip clamps swept)
  -- AND NEITHER HAS THE BOX.  A fixed height is the whole tier now, so a floor
  -- anywhere would be a second opinion about the same measure.
  assertEqual "a floor under the working box, whose height is fixed" []
              [ line | line <- T.lines page, ".pop-sheet{" `T.isInfixOf` line
                     , "min-height" `T.isInfixOf` line ]
  assertEqual "and no pane declares a viewport floor at all" []
    [ line | line <- T.lines page
           , any (`T.isInfixOf` line) ["#mtext{", "#mdoc{", "#mprops{", "#mpanes{"]
           , "min-height:" `T.isInfixOf` line
           , not ("min-height:0" `T.isInfixOf` line) ]
  where
    -- Every box and pane that has to CONTAIN what it holds, and the
    -- declarations that make it do so: a flex child shrinks to its parent only
    -- with the floor taken off (@min-height:0@), and the scroll then belongs to
    -- whichever element the content is inside.
    clamps =
      [ -- The panes row: sized by the box, and CONTAINING what it is sized to.
        ("#mpanes", ["flex:1", "min-height:0", "overflow:hidden"])
        -- The raw pane has no floor of its own — it is the one that had.
      , ("#mtext", ["min-height:0"])
        -- The document pane is its own scroller, and can shrink to be one.
      , ("#mdoc", ["min-height:0", "overflow:auto"])
        -- The panel clamps and hands its scroll to the mount inside it.
      , ("#mprops", ["min-height:0", "overflow:hidden"])
      , ("#mptable", ["flex:1", "min-height:0"])
        -- The link and tag popups, the same arrangement one tier up.
      , ("#tpane", ["min-height:0", "overflow:hidden"])
      , ("#ltable", ["min-height:0", "overflow:hidden"])
        -- The settings sheet and the palette list scroll in their own right.
      , ("#cbox", ["overflow-y:auto"])
      , ("#plist", ["max-height:40vh", "overflow-y:auto"])
        -- And the logbook strip, which is bounded rather than shrinkable.
      , ("#mlog", ["flex:0 0 auto", "max-height:22vh", "overflow:auto"])
      ]

-- | ONE GRID, ONE BASE: the head's star gutter and a paragraph's indent are the
-- SAME arithmetic rather than the same glyph count, so a bold or a fallback face
-- with a different advance cannot move one without moving the other.
--
-- Asserted as the relation between the two declarations rather than as two
-- copied strings: the paragraph's padding is the base plus the gutter, and the
-- gutter is what the head's prefix is given outright.
gridSweep :: IO T.Text -> TestTree
gridSweep shell = testCase "the star gutter and the body indent are one arithmetic" $ do
  page <- shell
  gutter <- need "the head's star gutter" (between "  .d-head .ds{width:calc(" ")}" page)
  para <- need "the paragraph's indent"
                (between "  .d-para,.d-comp{margin:.5em 0;" "}" page)
  base <- need "the document's base padding" (between "--g-doc-pad:" ";" page)
  assertEqual "the paragraph is padded by the base plus the gutter"
              ("padding-left:calc(var(--g-doc-pad) + " <> gutter <> ")")
              (T.strip (T.replace "\n" "" (T.replace "  " "" para)))
  -- And the element every one of them sits in is inset by that same base, so
  -- the two are counted from one place.
  assertContains "the base is the element's own inset"
                 "    padding:1px var(--g-doc-pad);" page
  assertBool ("the base is a length: " <> T.unpack base) (not (T.null base))
  where need what = maybe (assertFailure ("no " <> what <> " in the page")) pure

-- | POPUP SIZE IS A TIER, and this is what makes the rule enforceable rather
-- than stated: every box wears exactly one of the three, and NO rule naming a
-- box declares a width or a height.  Swept, so a popup that grew a size of its
-- own fails here rather than being noticed by a reader with two windows open.
--
-- The sweep asserts what it swept first — every box found, every tier defined —
-- so a renamed id or a dropped tier fails loudly rather than passing over
-- nothing.
tierSweep :: IO T.Text -> TestTree
tierSweep shell = testCase "every popup wears one size tier, and declares none" $ do
  page <- shell
  -- Each box, and the tier it is dressed in.
  mapM_ (\(box, tier) ->
           assertContains "the box wears its tier"
                          ("id=\"" <> box <> "\" class=\"" <> tier <> "\"") page)
        tiers
  -- The two are defined, once, and nothing else is — `pop-wide' included, which
  -- was the third until fixing its height made its definition `pop-sheet''s
  -- character for character.
  mapM_ (\tier -> assertContains "the tier is defined" ("." <> tier <> "{") page)
        (nub (map snd tiers))
  assertEqual "no tier beyond the two the list names" []
              [ t | t <- ["pop-wide", "pop-fullscreen", "pop-compact", "pop-eighty"]
                  , ("." <> t <> "{") `T.isInfixOf` page ]
  -- And no rule naming a BOX declares a size.  Read out of the rule's own body,
  -- so a `min-height' inside a pane it holds is nobody's business here.
  --
  -- WHAT IT SWEPT IS ASSERTED FIRST, which is the half this was missing: the
  -- selectors are GROUPED in the page, a literal `#pbox{' matched none of them,
  -- and the `Just body <-' guard dropped three of the five boxes with no
  -- complaint at all.  A box with no rule to read is now a failure rather than a
  -- silent pass over nothing.
  let swept = [ (box, body) | (box, _tier) <- tiers, Just body <- [ruleIn ("#" <> box) page] ]
  assertEqual "the sweep found a rule for every box it names"
              (map fst tiers) (map fst swept)
  assertEqual "a box that declares its own size" []
              [ (box, prop)
              | (box, body) <- swept
              , prop <- ["width:", "height:"]
              , prop `T.isInfixOf` body ]
  -- ONE TOP LINE, and every backdrop reads it.  A shallow palette and a tall
  -- sheet open with their top borders on the same rule, so raising one after
  -- another does not walk the reader's eye down the window; growth is downward
  -- and every tier's ceiling is what the anchor leaves, so no box runs off the
  -- bottom.  The four per-backdrop anchors this replaced are asserted GONE.
  assertContains "the anchor is declared once" "--g-pop-top:5vh;" page
  assertContains "and what it leaves is derived from it"
                 "--g-pop-max:min(90vh," page
  -- SYMMETRIC: the foot margin is the head's, derived from the anchor rather
  -- than spelled as a second figure — a tall box stopping short of the bottom by
  -- a different amount reads as one that ran out of room.
  assertContains "and the bound is the anchor twice over"
                 "calc(100vh - 2 * var(--g-pop-top))" page
  assertEqual "no second figure under the box" []
              [ n | n <- ["100vh - var(--g-pop-top) - var(--g-pop-pad)"]
                  , n `T.isInfixOf` page ]
  assertContains "every backdrop anchors its top, and none centres"
                 "padding-top:var(--g-pop-top);" page
  assertEqual "a backdrop that centres, or anchors at a line of its own" []
              [ n | n <- ["align-items:center;justify-content:center"
                        , "padding-top:15vh", "padding-top:12vh", "padding-top:8vh" ]
                  , n `T.isInfixOf` page ]
  -- And every tier's own bounds are measured against it rather than against the
  -- window, which is what keeps the growth inside the room the anchor left.
  mapM_ (\needle -> assertContains "a tier bounded by the anchor's room" needle page)
        [ ".pop-band{width:min(560px,100%);max-height:var(--g-pop-max)}"
        , ".pop-sheet{width:min(80vw,100%);height:var(--g-pop-max)}" ]
  where
    -- FOUR OF THE FIVE ARE WORKING SURFACES and wear the one tier for it: the
    -- materialize sheet, the link and tag popups, and the settings sheet.  The
    -- state palette is the odd one — a list of single words, which is what the
    -- band is.  `pop-wide' stood between them until its height went fixed and
    -- its definition became `pop-sheet''s outright.
    tiers = [ ("pbox", "pop-band"), ("lbox", "pop-sheet"), ("tbox", "pop-sheet")
            , ("sheet", "pop-sheet"), ("cbox", "pop-sheet") ]

-- | EVERY SELECTION IN THE DOCUMENT IS A GROUND.  Swept rather than listed: the
-- rules are cut out of the rendered page by their selectors and each body is
-- asserted to name a background and nothing that draws a LINE — no underline, no
-- border, no outline.  A locator that adds a line to a row of text moves the
-- text, and a document is read as text; the table's own crosshair is two grounds
-- for the same reason.
--
-- The sweep asserts what it swept first, so a selector renamed out from under it
-- fails loudly rather than passing over nothing.
groundSweep :: IO T.Text -> TestTree
groundSweep shell = testCase "every document selection is a ground, never a line" $ do
  page <- shell
  let bodies = [ (sel, body) | sel <- selectors, Just body <- [ruleIn sel page] ]
  assertEqual "the sweep found every rule it names"
              (length selectors) (length bodies)
  mapM_ (\(sel, body) -> do
           assertBool (T.unpack sel <> " draws no ground: " <> T.unpack body)
                      ("background" `T.isInfixOf` body)
           mapM_ (\line -> assertBool
                    (T.unpack sel <> " draws a " <> T.unpack line <> ": " <> T.unpack body)
                    (not (line `T.isInfixOf` body)))
                 ["underline", "outline", "border", "text-decoration", "box-shadow"])
        bodies
  where
    -- The four states a document element or one of its cells can be in.
    selectors = [".de.dat", ".de.dfl", ".de.dat.dfl", ".dc.don"]

-- | The body of the first rule whose SELECTOR LIST names SEL, or 'Nothing' when
-- no rule does.
--
-- GROUPED SELECTORS ARE THE POINT.  A literal @"#pbox{"@ match found NOTHING
-- where the page writes @#pbox,#lbox,#tbox{...}@, and both sweeps read this
-- through a @Just body <-@ pattern guard, so the box dropped out in SILENCE:
-- three of the five boxes went unswept while the group's own comment claimed it
-- asserted what it swept.  SEL counts as named when the character after it
-- opens the body or ends a list entry, and only while no @}@ has intervened,
-- which is what keeps a mention inside some other rule's body from answering
-- for it.  One definition, since the two sweeps had a copy each.
ruleIn :: T.Text -> T.Text -> Maybe T.Text
ruleIn sel = listToMaybe . rulesIn sel

-- | The body of EVERY rule in PAGE whose selector list names SEL, in source
-- order.
rulesIn :: T.Text -> T.Text -> [T.Text]
rulesIn sel page
  | T.null rest         = []
  | opens && inSelector = T.takeWhile (/= '}') (T.drop 1 body) : rulesIn sel after
  | otherwise           = rulesIn sel after
  where
    rest         = snd (T.breakOn sel page)
    after        = T.drop (T.length sel) rest
    opens        = maybe False (\(c, _more) -> c == '{' || c == ',') (T.uncons after)
    (list, body) = T.breakOn "{" after
    inSelector   = not ("}" `T.isInfixOf` list)

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
      , "const summons = () => can(table, \"openFilter\");"
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
      , "tmount = mountOnce(\"ttable\", TCOLS,"
      , "{ palette: true, marks: false, flags: true, actionHints: false,"
      , "flagHelp: \"d/D remove · u unflag\" },"
      -- The overlay is the SHARED mechanism over one cell: the popup declares a
      -- shape and nothing about the gesture is spelled twice.
      , "cells: [\"title\"], cols: TCOLS,"
      , "const renaming = () => !!edit && edit.o === TROW;"
      -- Raised over the tag at point, through the guard both browsing popups
      -- open their overlay by: a row or the refusal, never a box over nothing.
      , "openOver(TROW, tagAt(), \"org-rename-tag (no tag)\")"
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
  , Glue "the edit overlay is one mechanism the four surfaces declare a shape for"
      [ "function openEdit(o, row) {"
      , "edit = { o, row };"
      , "el(o.box).className = \"on\";"
      , "o.fill(row);"
      , "o.focus(row);"
      -- The anchor is the SHAPE's: a mount names its published root and the
      -- renderer's own selected row, and the structured document — which is no
      -- mount — names the element under point.  One reader either way.
      , "const anchorOf = (o) => {"
      , "return m ? m.el.querySelector(\"tbody tr.tv-sel\") : null;"
      , "const tr = anchorOf(o);"
      -- And the shape names BY KEY which of the row's own cells the box covers,
      -- resolved against the column list the server declared, so a column that
      -- moves takes the box with it.
      , "const span = o.cells && cellSpan(o.cells, o.cols);"
      , "const tds = span && [...tr.querySelectorAll(\"td:not(.tv-box)\")];"
      , "const from = tds && tds[span[0]], to = tds && tds[span[1]];"
      , "s.width = `${rt.right - l.left}px`;"
      -- The window resize moves whichever overlay is up, and is registered once
      -- rather than per mount.
      , "window.addEventListener(\"resize\", placeEdit);"
      -- THE SNAPSHOT, which is the bug this retired: a commit reads the row the
      -- overlay OPENED over, never the cursor, so a click that moved the cursor
      -- under an open field cannot redirect the write.
      , "const r = edit.row;"
      , "const dediting = () => !!edit && edit.o === DROW;"
      -- SHARING THE STATE MUST NOT SHARE THE SHUTTER.  The tags popup can stand
      -- over an open materialize sheet — clicking the sheet's chrome blurs its
      -- textarea and every `table' row goes live again — so an unscoped shut
      -- would let the sheet's `fill'/`shut' cancel an open tag rename. Each
      -- caller names its own shape, which is the isolation the two hand-written
      -- shutters had, and ESC names it through the one sentence every surface
      -- words the event with.
      , "function shutEdit(o) {"
      , "if (!edit || edit.o !== o) return;"
      , "for (const o of shapes) shutEdit(o);"
      , "cancelEdit(\"element\", DROW, DPARA)"
      , "cancelEdit(\"row\", PROW)"
      , "cancelEdit(\"tag\", TROW)"
      , "cancelEdit(\"link\", LROW)" ]
      -- The live cursor read the commit used to make, the per-surface copies of
      -- the gesture, and the unscoped shut that would reach across surfaces.
      [ "drows[docAt()]", "function place()", "function shutRename"
      , "shutEdit();" ]

  -- THE DOCUMENT'S OWN RULES, as the data they are.  What is DRIVEN is in
  -- "Shell sheet"; what is read here is the three things behaviour cannot show
  -- from the outside: that the body is cut at the child that owns it, that the
  -- dispatch stands aside for a key this listener has already claimed, and that
  -- the cursor carries a grain nothing spends yet.
  , Glue "the document is elements, cut where the outline under it begins"
      -- ONE OWNER PER BYTE, one level down: the lens hands over the whole
      -- subtree's body, so the paragraphs stop at `ownLines' and the children
      -- are drawn from the entries the server named.  Without the cut the same
      -- lines would be a paragraph AND the child that owns them.
      [ "function blocksIn(lines, own) {"
      , "const own = h.ownLines === undefined ? dlines.length : h.ownLines;"
      , "for (const b of blocksIn(dlines, own))"
      -- The commit is a SPLICE: each paragraph remembers the line range it came
      -- out of, so what goes back is the body with those lines replaced and
      -- every other byte where it was.
      , "function bodyText(drop) {"
      , "out.splice(p.from, p.to - p.from, ...p.text.split(\"\\n\"));"
      -- DEL IS UP, and at the top it is the sheet's door.
      , "if (editing.child === null) { leaveSheet(); return; }"
      , "reread(up === null ? undefined : up, (h, fresh) => {"
      -- A KEY THIS LISTENER CLAIMED IS NOT THE MAP'S.  `DEL' closes the sheet
      -- from here, and without this the table's own `DEL' would strip a filter
      -- token off the view underneath on the same press.
      , "if (e.defaultPrevented) return;"
      -- The cursor's GRAIN: one element today, and the field is what a future
      -- expand-region moves rather than every reader of the cursor learning
      -- about it twice.
      , "let drows = [], dat = 0, dcol = null, dgrain = \"element\";"
      , "dgrain = dcol === null ? \"element\" : \"cell\";"
      -- A HEADLINE LINE IS LAID OUT AS ORG LAYS ONE OUT: the two headline kinds
      -- are flex rows, the title takes the room the line has left, and the tags
      -- are flushed to the far edge (`org-tags-column').  A paragraph beside
      -- them is flowing text and takes none of it.
      -- A CURSOR IS ONLY DRAWN WHERE THE KEYS ARE: both panes' cursor washes
      -- are gated on the pane holding them, and the panel's costs two rules
      -- because the wash it suppresses is the RENDERER's and the stripe under
      -- it has to be put back.  A FLAG is not a cursor and keeps its ground
      -- either way — it is a queue, and it has to read from the other pane.
      , "#mdoc.on .de.dat{"
      , "#mdoc.on .dc.don{"
      , "#mprops:not(.on) .tv-table tbody tr.tv-sel{background:transparent}"
      , "#mprops:not(.on) .tv-table tbody tr.tv-sel.tv-alt{background:var(--tv-alt)}"
      , ".d-head,.d-child{display:flex;align-items:baseline}"
      , ".dc-title{flex:1 1 auto;min-width:0}"
      , "margin-left:auto;margin-right:0}"
      -- And content sits under the TITLE TEXT, which is the other half of
      -- `org-startup-indented'.  PADDING rather than a margin — a margin would
      -- shrink the element's box and take the selection wash off the left of the
      -- line — and the width is written onto the pane as a NUMBER, with the
      -- arithmetic in the stylesheet, the way the log's cap is.
      , "el(\"mdoc\").style.setProperty(\"--g-doc-indent\","
      , "String(dstars(docLevel()).length));"
      , "padding-left:calc(var(--g-doc-pad) + var(--g-doc-indent, 2) * 1ch)}" ]
      -- The document is not a mount and never asks the renderer to draw it.
      [ "TableView.mount(el(\"mdoc\")", "TableView.mount(el(\"dlist\")" ]

  -- ONE `d'/`D'/`u' GESTURE, likewise, and over FOUR surfaces now: the
  -- two-press rule, the feature detection, the set-or-row choice, the spending
  -- of the flags and the walk after `u' are written once, and each surface names
  -- the phrases it says them in, the mount they live on, what "take these"
  -- means and what it logs.  The document's `mount' is not a renderer's at all
  -- — four calls over a Set of element ids — which is what says the gesture asks
  -- a mount for four things and never what kind of mount it is.
  , Glue "the flag gesture is one implementation over four surfaces"
      [ "function flagKey(k, s, say) {"
      , "if (k === \"D\" || (k === \"d\" && flags.indexOf(at) !== -1)) {"
      , "if (can(m, \"clearFlags\")) m.clearFlags();"
      , "say(s.unflag);", "say(s.flag);", "say(s.none);", "say(s.missing);"
      , "none: \"org-delete-element (no element)\","
      , "unflag: \"delete-unflag (flag cleared)\","
      , "none: \"org-toggle-tag (no tag)\","
      , "unflag: \"tag-unflag (flag cleared)\","
      -- And the table's own shape, which is a function of the BINDING because
      -- `said' spells the binding's command name: one gesture, two names.
      , "const XFLAGS = (b) => ({"
      , "flag: \"flagged — d again archives\","
      , "flagPress(k, e, DFLAGS)", "flagPress(k, e, TFLAGS)"
      , "archiveFlag: (b) => flagKey(\"d\", XFLAGS(b), (what) => said(b, what)),"
      , "archiveRows: (b) => flagKey(\"D\", XFLAGS(b), (what) => said(b, what)),"
      , "flagKey(\"u\", XFLAGS(b), (what) => said(b, what)); return; }" ]
      -- The three hand-written copies it replaced: the panel's, the popup's, and
      -- the table's — which was an `archiveFlag' of its own, a fork inside
      -- `archive' choosing between the flagged set and the row at point, and a
      -- flag branch inside `mark'.
      [ "function dflag", "function tflag", "d → delete-flag (d again deletes)"
      , "d → tag-flag (d again removes)"
      , "if (isFlagged(id)) { archive(b); return; }"
      , "said(b, \"flagged — d again archives\")"
      , "const flags = flagging() ? table.getFlagged() : [];"
      , "archiveRows: archive," ]

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
      [ "function remount(after) { leaving = arriving = null; stash(); start(after); }"
      , "function stash() {"
      -- A structured sheet is never dirty — every element commits on its own —
      -- so what a remount would lose is where the reader was STANDING and
      -- whatever an open edit is holding, and both ride across.
      , "sheet: editing"
      , "? { id: editing.id, child: editing.child, raw,"
      , "at: drows[dat] ? drows[dat].id : null, col: dcol,"
      , "open: openEditState(), digest: editing.digest }"
      , "palette: typedFilter(),"
      , "return box && document.activeElement === box ? box.value || \"\" : null;"
      , "function restore() {"
      , "if (box) { box.value = was.palette; box.focus(); }"
      , "if (was.sheet) reopen(was.sheet);"
      , "headline(s.id, s.child).then((h) => {"
      , "el(\"mtext\").value = s.text;"
      , "if (s.open) reopenEdit(s.open);"
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
  , Glue "the sheet is a structured document and a property panel"
      [ "<div id=\"mpanes\">", "<div id=\"mdoc\"><div id=\"dlist\"></div>"
      , "<div id=\"mprops\"><div id=\"mptable\"></div>"
      , "base = raw ? h.org : \"\";"
      , "if (raw) { drows = []; dlines = []; drawDoc(); } else docFrom(h);"
      , "drawProps(raw ? [] : h.properties || [], raw ? [] : h.planning || []);"
      , "{ body: bodyText(), properties: props(), planning: planning() }"
      -- THE DOCUMENT IS NOT A MOUNT, and that is the doctrine line: the
      -- renderer's list widget draws a list of RECORDS, one shape per row, and
      -- this is a list of KINDS.  The model is `drows' and the whole view is one
      -- draw.
      , "drows.push({ id: \"H\", kind: \"head\", cells: cellsOf(h.cells) });"
      , "kind: \"para\","
      , "drows.push({ id: `C${c.index}`, kind: \"child\", index: c.index,"
      , "function drawDoc() {"
      -- THE PANEL IS ONE, and for the same reason read the other way: a drawer
      -- is a list of records.  The model is this page's and the mount is a view
      -- of it, every change going back through `setRows'.
      , "pmount = mountOnce(\"mptable\", PCOLS, {"
      , "        flagHelp: \"d/D delete · u unflag\","
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
      , "reread(editing.child, (_h, fresh) => {"
      -- The panel's own keys: TAB crosses the panes and hops the open row's two
      -- fields, nav movement is both spellings of the map's own letters and the
      -- arrows, and RET opens a row and commits it.  Movement is the MOUNT's
      -- step, so the cursor a reader moves is the renderer's.
      , "const k = keyName(e), crossing = k === \"TAB\" || k === \"S-TAB\";"
      , "const rowStep = (k) => (k === \"<down>\" || k === \"n\" || k === \"j\" ? 1"
      , "else if (rowStep(k)) stepIn(pmount, rowStep(k));"
      , "      } else if (pnav()) {"
      , "const pnav = () => el(\"mprops\").className === \"on\";"
      , "el(\"mprops\").className = \"on\"; el(\"mdoc\").className = \"\";"
      -- Nav holds the keys with nothing focused, so the map has to be told.  It
      -- is the FIRST of the modal surfaces, its listener registering ahead of
      -- the dispatch, and `typing()' reads that one list rather than naming any
      -- of them.
      , "{ name: \"sheet\", up: docHolds, edit: sheetOpen, shut: cancelSheetEdit },"
      , "return SURFACES.some((s) => s.up())"
      -- The panel stacks under the text when there is no room beside it, which
      -- is a wrap rather than a second breakpoint to keep in step.
      , "#mpanes{flex:1;min-height:0;overflow:hidden;"
      , "#sheet.raw #mprops{display:none}"
      -- The pane hosts the mount and positions the overlay, and that is the
      -- whole of what it styles: `.tv-root' brings the frame and draws the rows.
      , "#mprops{flex:1 1 240px;min-width:0;min-height:0;position:relative;"
      , "#mptable .tv-root,#ltable .tv-root,#ttable .tv-root{flex:1;min-width:0;"
      -- The open row's fields sit OVER the row, since the mount rewrites its own
      -- rows as it scrolls, and they land on the text they replace.
      , "#dedit,#dpara,#pedit,#tedit,#ledit{display:none;position:absolute;"
      , "#dedit input,#pedit input,#tedit input,#ledit input,#dpara textarea{"
      -- A planning row's key is org's rather than the author's, and says so.
      , "#pkey[readonly]{color:var(--g-mute)}"
      -- ONE FOCUS LANGUAGE: whichever pane holds the keys wears the accent on
      -- its own frame.  Declared for both rather than left to the browser,
      -- which can only dress the one that takes a real focus.
      , "#mtext:focus{outline:none;border-color:var(--g-accent)}"
      , "#mprops.on .tv-root,#mdoc.on{border-color:var(--g-accent)}" ]
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
      , "font:14px/1.5 var(--glance-mono)"
      -- THE LINK INK IS THE RENDERER'S, hand-copied per theme the way the
      -- hairline is: `--tv-link' is declared on `.tv-root' rather than on the
      -- document element, so a live `var()' read resolves to nothing in a pane
      -- beside the mount.
      , "--g-link:#30739B;", "--g-link:#7CC9F8;" ]
      -- ALIASED, NOT RESPELLED: every use reads the name.  A hex at a use site
      -- is what makes a renderer change N edits instead of one, so the two
      -- values may appear only where the palette declares them.
      [ "--g-border:#BDC3C7", "--g-border:#223959"
      , "color:#30739B", "color:#7CC9F8", "text-decoration:underline;color:#" ]

  -- One rule sets both widths, so the strip cannot drift from the table above
  -- it; the hairline, the radius and the surface tint are `.tv-root''s, which is
  -- what makes it read as the same thing.
  --
  -- ITS HEIGHT IS STATIC: N line boxes whatever it is holding, so the table
  -- above it never resizes under a reader's cursor because a write logged a
  -- line, and a quiet page reads the same as a busy one.  The collapse, the
  -- hand-reserved line, the ten-line cap and the flexible strip that grew to its
  -- content are all superseded designs.
  , Glue "the log wears the table's container under it, at a static height"
      [ "#app,#log{width:100%;box-sizing:border-box}"
      , "border:1px solid var(--g-border);border-radius:8px;"
      -- The table is the flexible one and takes the whole of the rest.
      , "#app{flex:1 1 auto;min-height:0}"
      , "background:var(--g-surface);flex:none;overflow-y:auto}"
      -- N of its own line boxes exactly, computed off the rule's own font
      -- size (`em', so it is not restated) and the padding above it rather than
      -- eyeballed.  N is a CUSTOM PROPERTY declared at the default here, so the
      -- arithmetic is in one place and the settings sheet writes a NUMBER onto
      -- the element.
      , "    --g-logn:7;"
      , "height:calc(var(--g-logn) * 1.5em + 2 * 6px + 2 * 1px);"
      -- The end of a long message is scrolled to unless the reader has scrolled
      -- up to hold a place.
      , "box.scrollTop + box.clientHeight >= box.scrollHeight - 4"
      , "if (end) box.scrollTop = box.scrollHeight;" ]
      [ "#log:empty", "min-height:1.4em", "max-height:10em"
      , "max-height:calc(var(--g-logn)" ]

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
      , "const themed = pref(\"glance-theme\", \"auto\");"
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
      , "const logPref = pref(LOG.key, \"\");"
      , "localStorage.setItem(key, v)"
      , "el(\"log\").style.setProperty(\"--g-logn\", String(n));"
      , "setLogLines(logLines(logPref.get()) || LOG.def);"
      -- Applied as it is TYPED, so the field is a knob rather than a form.
      , "el(\"clog\").addEventListener(\"input\""
      , "if (n === null) return;"
      -- And the sheet draws the preference back over a value that was refused.
      , "el(\"clog\").value = logPref.get();"
      -- An EMPTIED field is a preference that is not there.
      , "else localStorage.removeItem(key); } catch (e)" ]

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
      , "crows[cat].text = el(\"ctext\").value;"
      , "el(\"clayer\").addEventListener(\"change\""
      , "const cdirty = () => (takeLayer(), crows.some(cmoved));"
      , "const cmoved = (r) => r.text !== r.base || r.tpl !== r.tplBase"
      -- The layer's SECOND box: the capture template, a region of the same file
      -- riding in the same write, kept on the layer the way its cycle is.
      , "<textarea id=\"ctpl\" class=\"ctext\""
      , "crows[cat].tpl = el(\"ctpl\").value;"
      , "tpl: layer.template || \"\", tplBase: layer.template || \"\","
      , "...(tpl !== r.tplBase ? { template: tpl } : {}),"
      -- One POST per layer that moved, each awaited, each under its own digest.
      -- A layer with nothing to send drops the refusal it was carrying, since
      -- the edit that earned it has been taken back.
      , "if (!cmoved(r)) { r.err = \"\"; continue; }"
      , "postJSON(\"/config\","
      , "{ path: r.path, lines: sent.split(\"\\n\"),"
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
      , "lmount = mountOnce(\"ltable\", LCOLS,"
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
      [ "box: \"ledit\", pane: \"lpane\", fields: [\"ltitle\", \"lurl\"],"
      -- BY KEY, against the column list the server declared: reordering those
      -- columns takes the box with them, where a positional pair had nothing
      -- tying it to the list it indexed.
      , "cells: [\"title\", \"url\"], cols: LCOLS,"
      , "const lediting = () => !!edit && edit.o === LROW;"
      , "openOver(LROW, pointedRow(), \"org-insert-link (no link)\")"
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

  -- ONE ENVELOPE PER VERB.  The routes that read a value share the unwrap that
  -- throws the server's own error, and the routes that write share the POST's
  -- method, header and encoding — so what a refusal looks like and what a body
  -- is sent as are each decided once.  `/config' assembles its own body inline
  -- and is the one that does.
  , Glue "the JSON verbs are written once"
      [ "const unwrap = (r) => r.json().then((b) => {"
      , "const getJSON = (url) => fetch(url).then(unwrap);"
      , "const postJSON = (url, body, extra) =>"
      , "headers: { \"content-type\": \"application/json\" },"
      , "const outcome = (r) => r.json().then((b) => ({ status: r.status, body: b }));"
      , "postJSON(at(id, child), { ...asked, digest }, extra);"
      , "const postCommand = (body) => postJSON(\"/command\", body).then(unwrap);" ]
      -- And no hand-rolled envelope left but `/config\''s, which assembles its
      -- own body inline.
      [ "method: \"POST\",\n  , \"        headers:" ]

  -- THE SUBTREE WRITE'S ANSWER, once: a 200 re-pins the digest and hands the
  -- caller its line, and under that is one ladder for a moved file, a refused
  -- planning entry and a request that never landed.  `commitDoc' is
  -- `commitDocWith' with the body rebuilt out of the model.
  , glue "one ladder answers every subtree write"
      [ "function landed(h, onOk) {"
      , "const commitDoc = (what, drop) =>"
      , "commitDocWith(bodyText(drop), () => { if (what) echo(`RET → ${what}`); });"
      , "function commitDocWith(body, say) {"
      , ".then((a) => { if (editing === h && landed(h, say)(a)) reload(); })"
      , ".then(landed(h, () => {" ]

  -- THE SHARED READINGS: a mount's cursor as an id, guarded once for the three
  -- surfaces that ask; the TAB hop off the shape's own field list; and the log
  -- verb as a table beside the route's names rather than a ladder inside the
  -- one shared write path.
  , Glue "the page reads a cursor, a hop and a verb in one place each"
      [ "const selectedId = (mount) =>"
      , "(can(mount, \"getSelection\") ? (mount.getSelection() || {}).id : null) || null;"
      , "const patAt = () => prows.findIndex((r) => r.id === selectedId(pmount));"
      , "const at = selectedId(lmount);"
      , "const at = selectedId(tmount);"
      , "function hop() {"
      , "const at = ids.findIndex((id) => el(id) === document.activeElement);"
      , "const VERBED = {"
      , "const verbed = (name, args, verb) => (VERBED[name] || stated)(args, verb);"
      , "const what = verbed(name, args, verb);"
      -- Exclusivity is walked off the one list rather than restated by hand.
      , "for (const s of SURFACES) if (s.momentary && s.up()) s.off();" ]
      -- The hand-written copies these replaced.
      [ "pmount.getSelection().id", "(lmount.getSelection() || {}).id"
      , "(tmount.getSelection() || {}).id"
      , "document.activeElement === el(\"pkey\") ? el(\"pval\")"
      , "document.activeElement === el(\"ltitle\") ? el(\"lurl\")"
      , "name === \"edit-link\" ? verb"
      , "if (linking()) shutLinks();" ]

  -- ONE LISTENER SHAPE FOR THE TWO BROWSING POPUPS, and the guard that was one
  -- popup's is now both's: a key another listener has already CLAIMED is nobody
  -- else's.  The `e.repeat' guard stays in the chain that owns it.
  , Glue "the two browsing popups share one listener"
      [ "function popupKeys(name, mount, o) {"
      , "if (momentary() !== name || e.defaultPrevented) return;"
      , "popupKeys(\"links\", () => lmount, {"
      , "popupKeys(\"tags\", () => tmount, {"
      , "flagPress(k, e, TFLAGS)" ]
      [ "if (momentary() !== \"links\") return;"
      , "if (momentary() !== \"tags\" || e.defaultPrevented) return;" ]

  -- THE FOLLOW GESTURE AND THE ASKING, each written once: `o' at the row's
  -- grain and at the element's are one function over different sets, and the
  -- two keys that ask before writing differ only in where the rows come from.
  , glue "the follow gesture and the two askers are one each"
      [ "function followLinks(b, id, a, links) {"
      , "linksOf(id).then((a) => followLinks(b, id, a, a.links || []))"
      , "followLinks(b, editing.id, { ...a, links }, links);"
      , "function askState(b, ids, title) {"
      , "function askTags(b, ids, title) {"
      , "const docTargets = (b, label, k) =>"
      , "k(b, [editing.id], `${label} · ${docTitle()}`);"
      , "setState: (b) => overTargets(b, \"set state\", askState),"
      , "manageTags: (b) => overTargets(b, \"tags\", askTags),"
      , "docTargets(docBinding(\"org-glance-overview:todo\"), \"set state\", askState);"
      , "docTargets(docBinding(\"org-agenda-set-tags\"), \"tags\", askTags);"
      -- And the raise both palette doors take.
      , "function raise(title, state, value, cls, foot) {" ]

  -- THE DOCUMENT'S OWN ARITHMETIC.  Spans are CHAR offsets, so the pane counts
  -- characters rather than UTF-16 units; the overlay is anchored to the element
  -- the DRAW marked rather than to the `dat'-th child of the list, which a
  -- composite's nested leaves make a different element; and the element ordinal
  -- is the BUILD's, spent by a loop rather than kept in module scope.
  , Glue "the document counts characters and anchors what it drew"
      [ "const chars = (s) => Array.from(String(s));"
      , "const clen = (s) => chars(s).length;"
      , "const cslice = (s, a, b) => chars(s).slice(a, b).join(\"\");"
      , "const bodyShift = () => clen(editing.org || \"\") - clen(editing.body || \"\");"
      , "dlines.slice(0, line).reduce((n, l) => n + clen(l), 0) + line;"
      , "for (const l of linksIn([at, at + n], links)) {"
      , "part(into, \"span\", \"dt\", cslice(text, cut, a));"
      , "const docElAt = () => dcursor;"
      -- And the cell at point is READ rather than assumed: a stash put back over
      -- a headline that has since lost one names a column that is not there.
      , "const c = dcol === null ? null : shown(r)[dcol];"
      , "let owner = null, seq = 0;"
      , "const id = `B${seq++}`;" ]
      -- The UTF-16 readings and the two positional reaches they replaced.
      [ "(editing.org || \"\").length", "n + l.length, 0"
      , "text.slice(cut, a)", "at + text.length"
      , "el(\"dlist\").children || [])[dat]", "let dseq = 0" ]

  -- THE BOX'S TIER SURVIVES A RAISE.  `#pbox' carries its size tier as a class
  -- and the mode is a second one, so the mode is TOGGLED — a wholesale write
  -- dropped the tier on the first raise, silently, since only a live page is a
  -- size.  The markup still ships it, which `tierSweep' is what asserts.
  , Glue "raising the palette keeps the box's tier"
      [ "el(\"pbox\").classList.toggle(\"narrow\", cls === \"narrow\");" ]
      [ "el(\"pbox\").className = cls;" ]

  -- EVERY VEIL IS A DOOR.  The two sheets leave through their own ladder and
  -- the two momentary popups are answered and gone, so what a backdrop click
  -- does differs by surface — but every backdrop has one.
  , glue "the momentary veils are backdrops too"
      [ "for (const id of [\"modal\", \"config\"])"
      , "if (e.target === el(id)) leaveSheet();"
      , "for (const [id, off] of [[\"links\", shutLinks], [\"tags\", shutTags]])"
      , "if (e.target === el(id)) off();" ]

  -- ONE COMMAND AT A TIME where a press makes several: rows sharing a FILE are
  -- written under ONE drift lock, so two requests fired together are each
  -- measured against a digest the other is moving.
  , Glue "a press that makes several commands sends them in turn"
      [ "async function cyclePriority(b, step) {"
      , "await fire(b, \"set-priority\", over, { priority: key || null },"
      , "async function removeTags(list) {"
      , "for (const tag of list)"
        -- Guarded, so a refusal on one tag does not abandon the tags behind it:
        -- `fire' throws on a whole-request refusal and the flags are already
        -- spent by the time this runs.
      , "await Promise.resolve(untag(tag)).catch(failed(tagging, \"remove-tag\"));" ]
      [ "for (const tag of list) untag(tag);" ]

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
      [ "const steps = () => can(table, \"selectStep\");"
      , "if (visible().length) table.selectStep(step);"
      -- Which row is on is the renderer's answer too, with the DOM read left as
      -- the fallback for an asset predating that call.
      , "tbody tr.tv-sel", "table.getVisible()", "table.select(id, column())", ".tv-filter"
      , "if (cells()) return table.getSelection().id;" ]
      [ "tr.click()", "rowEls("
      , "box-shadow:inset 2px 0 0 var(--tv-accent)", "tr.tv-sel{box-shadow" ]

  -- `scrollIntoView' WAS on that forbidden list outright, and the document pane
  -- took it off; `scrollSweep' below is where the rule went.

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
      , "can(table, \"nextPage\")"
      , "can(table, \"pageInfo\")"
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
      , "can(table, \"getSelection\")"
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
      [ "#mtext,#pinput,#dedit input,#pedit input,#tedit input,#ledit input,"
      , "#dpara textarea,"
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
      r <- ok =<< getFrom application' "/"
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
      r <- ok =<< get missingAssetsDir "/headlines"
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
      assertBool "a missing renderer is reported"
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
      r <- ok =<< getWith a "/headlines" [("If-None-Match", atGeneration 7 tag)]
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
      after <- ok =<< getWith a "/headlines" [("If-None-Match", tag)]
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
      zipped <- ok =<< getWith a "/headlines" [("Accept-Encoding", "gzip")]
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
      r <- ok =<< getWith a "/table-view.js" [("Accept-Encoding", "gzip")]
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
      r <- ok =<< getFrom a "/headlines?q=no-such-headline-anywhere"
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
      r <- ok =<< getFrom a "/headlines?limit&q"
      assertEqual "rows" 6 . length =<< rowsOf r
  ]

-- | Document order, which is @?q=sort:*none*@ — a QUERY TOKEN rather than a
-- parameter of its own, and a starred meta like @*active*@ and @*archive*@.  It
-- moves both halves of the ordering at once: the rows stay in walk order under a
-- limit, and the view carries no @sort@ field for a renderer to re-apply.  What
-- it orders is top entries, so it is the order the files list them in rather
-- than an outline.
--
-- @?order=@ was the older spelling and is GONE.  It is refused rather than
-- ignored, which is the whole reason it was ever spelled out: a parameter this
-- server no longer reads would otherwise serve the default order and look
-- exactly like a working request.
orderSpec :: TestTree
orderSpec = testGroup "GET /headlines?q=sort:*none*"
  [ testCase "the default still declares the view's sort" $ do
      v <- get assetsDir "/headlines" >>= decoded
      fieldsOf v >>= assertBool "no sort field" . elem "sort"

  , testCase "document order declares none at all" $ do
      v <- get assetsDir "/headlines?q=sort:*none*" >>= decoded
      assertEqual "top-level keys" ["actions", "columns", "rows", "title"]
        . sort =<< fieldsOf v

  , testCase "and the page it cuts is walk order, where the default's is sorted" $ do
      a <- app assetsDir
      walk <- map rowId <$> (rowsOf =<< getFrom a "/headlines")
      -- The whole fixture under a limit, since its first rows are in the same
      -- order either way and a shorter page cannot tell the two apart.
      byState <- map rowId <$> (rowsOf =<< getFrom a "/headlines?limit=6")
      doc <- map rowId <$> (rowsOf =<< getFrom a "/headlines?q=sort:*none*&limit=6")
      assertEqual "the walk itself" walk doc
      -- Without this the case would pass over a fixture whose two orders agree.
      assertBool ("the fixture cannot tell them apart: " <> show byState)
                 (byState /= doc)

    -- The empty chain admits no companions: a key beside it is two orders in one
    -- query, and a reader who wrote both meant one of them.
  , testCase "a sort key beside it is a 400 naming the meta" $ do
      a <- app assetsDir
      mapM_ (\path -> do
               r <- getFrom a path
               assertEqual (show path <> " status") 400 (status r)
               assertContains "names the meta" "*none*" (body r))
            [ "/headlines?q=sort:*none*%20sort:title"
            , "/headlines?q=sort:title%20sort:*none*"
            , "/headlines?q=sort:*none*:desc" ]

    -- The retired parameter, in every spelling that used to work and one that
    -- never did: all of them 400, and the refusal names what replaced it.
  , testCase "order= is gone, and the refusal names its replacement" $ do
      a <- app assetsDir
      mapM_ (\path -> do
               r <- getFrom a path
               assertEqual (show path <> " status") 400 (status r)
               assertContains "names the parameter" "order=" (body r)
               assertContains "and its replacement" "sort:*none*" (body r))
            [ "/headlines?order=document", "/headlines?order=scheduled"
            , "/headlines?order=walk", "/headlines?order=" ]
      -- A parameter with no value reads as absent, here as everywhere: `?order'
      -- asks for no order and is not a request for the retired one.
      bare <- getFrom a "/headlines?order"
      assertEqual "a bare parameter is an absent one" 200 (status bare)
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

    -- The arrow form is SUGAR, so what is asserted is that the answer is the
    -- answer to the spelling it is sugar for — rows and declaration both.
  , testCase "an arrow-chained token is the tokens it is sugar for" $ do
      a <- app assetsDir
      let asked q = do r <- getFrom a ("/headlines?q=" <> q)
                       v <- decoded r
                       (,) <$> (map rowId <$> rowsOf r) <*> chainDeclaredBy v
      chained <- asked "sort:deadline:desc-%3Etitle"
      assertEqual "the two spellings answer alike" chained
        =<< asked "sort:deadline:desc%20sort:title"
      assertEqual "and the chain declared is the chain named"
                  [("deadline", False), ("title", True)] (snd chained)

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
          -- A SEGMENT is refused the way the token it is one of would be, and
          -- the whole token as written is what comes back.
        , ("sort:title-%3Enosuchcolumn", "nosuchcolumn") ]

  , testCase "and a half-typed one is no refusal at all" $ do
      r <- ok =<< get assetsDir "/headlines?q=sort:"
      assertEqual "rows" 6 . length =<< rowsOf r
      half <- ok =<< get assetsDir "/headlines?q=sort:title-%3E"
      assertEqual "a half-typed segment either" 6 . length =<< rowsOf half

    -- The empty chain is a sort token like any other, so it is refused for the
    -- same reason a column named twice is: two orders in one query.
  , testCase "and it cannot state two orders at once" $ do
      r <- get assetsDir "/headlines?q=sort:title%20sort:*none*"
      assertEqual "status" 400 (status r)
      assertContains "names the meta" "*none*" (body r)
      mid <- get assetsDir "/headlines?q=sort:title-%3E*none*"
      assertEqual "mid-chain is the same refusal" 400 (status mid)
      assertContains "and names the meta" "*none*" (body mid)
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
      r <- ok =<< getFrom a (headlinePath "ship-table-view")
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

    -- SUB-ADDRESSING.  A child has no row of its own, so the ROW's id plus an
    -- INDEX is the whole of how one is named — document order over the subtree,
    -- which is what makes a grandchild one number away from the row rather than
    -- a path a client has to walk.
  , testCase "the row names the entries hanging under it, and how to reach them"
      $ withTempDir $ \dir -> do
      _ <- orgFile dir "tree.org" nestedDoc
      (a, _hub) <- serverOver dir
      v <- getFrom a (headlinePath "top") >>= decoded
      assertEqual "standing on the row itself" Null =<< field "child" v
      assertEqual "with nothing above it" Null =<< field "parent" v
      assertEqual "the trail is the row alone" ["parent"] =<< textsAt "path" v
      -- The DIRECT children, each with the index `?child=' names it by: the
      -- grandchild hangs under the first and is not one of these.
      assertEqual "its own two children, by index" [0, 2]
        =<< traverse (intAt "index") =<< listAt "children" v
      assertEqual "and their cells" ["child one", "child two"]
        =<< traverse (textAt "title") =<< listAt "children" v
      assertEqual "the levels org spells" [2, 2]
        =<< traverse (intAt "level") =<< listAt "children" v

  , testCase "a child materializes as its own subtree, under the file's digest"
      $ withTempDir $ \dir -> do
      _ <- orgFile dir "tree.org" nestedDoc
      (a, _hub) <- serverOver dir
      row <- getFrom a (headlinePath "top") >>= decoded
      v <- getFrom a (childPath "top" 0) >>= decoded
      assertEqual "status" 200 . status =<< getFrom a (childPath "top" 0)
      assertEqual "the entry's own extent, its grandchild in it"
                  (T.unlines ["** child one", "SCHEDULED: <2026-08-05 Wed>"
                             , "one body", "*** grandchild"])
        =<< textAt "org" v
      assertEqual "its own planning line, lifted out"
                  [["SCHEDULED", "<2026-08-05 Wed>"]] =<< pairsAt "planning" v
      assertEqual "its own cells" "child one" =<< textAt "title" =<< field "cells" v
      -- The id and the digest are the ROW's: one file, one lock, whichever
      -- entry the sheet is standing on.
      assertEqual "the row's id" "top" =<< textAt "id" v
      rowDigest <- textAt "digest" row
      assertEqual "and the file's digest" rowDigest =<< textAt "digest" v
      assertEqual "the trail says where it is" ["parent", "child one"]
        =<< textsAt "path" v
      assertEqual "and the way back out is the row" Null =<< field "parent" v
      assertEqual "with its own child under it, by index" [1]
        =<< traverse (intAt "index") =<< listAt "children" v

  , testCase "and the grandchild climbs back to the child, not to the row"
      $ withTempDir $ \dir -> do
      _ <- orgFile dir "tree.org" nestedDoc
      (a, _hub) <- serverOver dir
      v <- getFrom a (childPath "top" 1) >>= decoded
      assertEqual "the entry" "*** grandchild\n" =<< textAt "org" v
      assertEqual "which child it hangs under" (Number 0) =<< field "parent" v
      assertEqual "the whole trail" ["parent", "child one", "grandchild"]
        =<< textsAt "path" v

    -- The body a client edits stops where the outline under it begins, or the
    -- same bytes would be drawn twice — once as this entry's last paragraph and
    -- once as the child that owns them.
  , testCase "ownLines is where the entry's own body stops" $ withTempDir $ \dir -> do
      _ <- orgFile dir "tree.org" nestedDoc
      (a, _hub) <- serverOver dir
      v <- getFrom a (headlinePath "top") >>= decoded
      body <- textAt "body" v
      own <- intAt "ownLines" v
      assertEqual "the stars and the one paragraph under them" 2 own
      assertEqual "which are these lines" ["* TODO parent", "parent body"]
                  (take own (T.lines body))
      child <- getFrom a (childPath "top" 0) >>= decoded
      assertEqual "and the child's own stops at ITS child" 2
        =<< intAt "ownLines" child

  , testCase "a child index the subtree has no entry for is a 404"
      $ withTempDir $ \dir -> do
      _ <- orgFile dir "tree.org" nestedDoc
      (a, _hub) <- serverOver dir
      r <- getFrom a (childPath "top" 9)
      assertEqual "status" 404 (status r)
      assertContains "names what it holds" "holds 3" =<< textAt "error" =<< decoded r

    -- A number that is not one is a 400 rather than a quiet fall back to the
    -- row: a mistyped index that served the parent would look exactly like a
    -- working request, and a write pinned to it would splice the wrong subtree.
  , testCase "and a child that is not a number is a 400" $ withTempDir $ \dir -> do
      _ <- orgFile dir "tree.org" nestedDoc
      (a, _hub) <- serverOver dir
      r <- getFrom a ("/headline" <> renderQuery True
                        [("id", Just "top"), ("child", Just "x")])
      assertEqual "status" 400 (status r)
      assertContains "says what one is" "whole number" =<< textAt "error" =<< decoded r

    -- A row id with no ORG_GLANCE_ID is FILE#K, so it carries slashes and a
    -- HASH.  The hash is the one that would bite: spelled into a URL raw it
    -- opens a fragment and the id arrives truncated at the first slash-free
    -- half of it.  The query string plus percent-encoding is what makes it a
    -- non-issue, on this side and in the shell (`encodeURIComponent').
  , testCase "an id carrying a hash and slashes round-trips" $ do
      (a, _hub) <- serverOver viewDir
      let rid = T.pack sampleFile <> "#1"
      r <- ok =<< getFrom a (headlinePath rid)
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
        r <- ok =<< postTo a (headlinePath "first") (commitBody edited digest)
        after <- document path
        assertEqual "the file is prefix + new subtree + suffix"
                    (T.take start before <> edited <> T.drop end before) after
        assertContains "the edit landed" "* DONE First" after
        assertContains "the next headline is untouched" "* TODO Second\ntail\n" after
        fresh <- textAt "digest" =<< decoded r
        expected <- digestOnDisk path
        assertEqual "the reported digest is the file's" expected fresh

    -- A CHILD IS WRITTEN THE WAY THE ROW IS: the same route under a `child=',
    -- splicing that entry's OWN extent and pinning the same file digest.  What
    -- the assertion is about is the extent — everything ahead of the child and
    -- everything past it is the string it was.
  , testCase "a child commit splices the child's extent alone" $ withTempDir $ \dir -> do
      path <- orgFile dir "tree.org" nestedDoc
      (a, _hub) <- serverOver dir
      v <- getFrom a (childPath "top" 0) >>= decoded
      org <- textAt "org" v
      digest <- textAt "digest" v
      before <- document path
      let edited = T.replace "one body" "one body, rewritten" org
      r <- ok =<< postTo a (childPath "top" 0) (commitBody edited digest)
      after <- document path
      assertEqual "the file is prefix + the child's new text + suffix"
                  (T.replace org edited before) after
      assertContains "the row's own headline is untouched" "* TODO parent\n" after
      assertContains "and so is the sibling behind it" "** child two\n" after
      expected <- digestOnDisk path
      assertEqual "the digest it reports is the file's"
                  expected =<< textAt "digest" =<< decoded r

    -- The lens over a child is the lens: the parts go back the same way, and the
    -- server's own regions are put back beside them.
  , testCase "and its parts recompose into the same extent" $ withTempDir $ \dir -> do
      path <- orgFile dir "tree.org" nestedDoc
      (a, _hub) <- serverOver dir
      v <- getFrom a (childPath "top" 0) >>= decoded
      before <- document path
      body <- textAt "body" v
      digest <- textAt "digest" v
      props <- pairsAt "properties" v
      plan <- pairsAt "planning" v
      assertOk =<< postTo a (childPath "top" 0)
             (encode (object [ "body" .= body, "digest" .= digest
                             , "properties" .= props, "planning" .= plan ]))
      assertEqual "a round trip nobody edited is the file it was"
                  before =<< document path

  , testCase "a commit aimed at a child that is not there is a 404" $
      withTempDir $ \dir -> do
        _ <- orgFile dir "tree.org" nestedDoc
        (a, _hub) <- serverOver dir
        v <- getFrom a (headlinePath "top") >>= decoded
        digest <- textAt "digest" v
        before <- document (dir </> "tree.org")
        r <- postTo a (childPath "top" 9) (commitBody "** nope\n" digest)
        assertEqual "status" 404 (status r)
        -- The NAME promises the commit did not land, and a status alone says
        -- only that the answer was refused.  Every sibling refusal here asserts
        -- the file too.
        assertEqual "and nothing was written" before =<< document (dir </> "tree.org")

  , testCase "leaves the store alone — the watch is what updates rows" $
      withCommitted $ \a path before -> do
        org <- textAt "org" before
        digest <- textAt "digest" before
        assertOk =<< postTo a (headlinePath "first") (commitBody (org <> "a line\n") digest)
        -- No watcher runs in this suite, so the store still holds the load it
        -- started with: the route wrote to the file and to nothing else.
        after <- decoded =<< getFrom a (headlinePath "first")
        assertEqual "the store's subtree" (Just org) . Just =<< textAt "org" after
        assertEqual "the store's digest" (Just digest) . Just =<< textAt "digest" after
        onDisk <- digestOnDisk path
        assertBool "but the file was written" (onDisk /= digest)

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
        assertOk =<< postTo a (headlinePath "first")
               (splitBody body' (props <> [["EFFORT", "0:30"]]) digest)
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
        assertOk =<< postTo a (headlinePath "first") (splitBody body' [] digest)
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
      withCommitted $ \a path _v -> do
        let huge = BL.fromStrict (BS.replicate (1024 * 1024 + 1) 0x78)
        r <- postTo a (headlinePath "first") huge
        assertEqual "status" 413 (status r)
        assertContains "the cap" "body over" (body r)
        -- BEFORE IT IS READ is the claim, and a status cannot carry it: the
        -- file standing untouched is what says the body never reached a write.
        assertEqual "and nothing was written" committable =<< document path

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

-- | @add-tag@ and @remove-tag@'s argument.  Flat rather than nullable: a tag
-- comes off through the other command rather than through a null.
tagArg :: T.Text -> Value
tagArg tag = object ["tag" .= tag]

-- | @set-title@'s argument.  Flat for @tagArg@'s reason: a headline with no
-- title is a blank entry and no longer a row, so there is nothing to clear.
titleArg :: T.Text -> Value
titleArg title = object ["title" .= title]

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
        r <- ok =<< postTo a "/command" (command "set-state" ["first"] (keywordArg (Just "WAITING")))
        assertEqual "the row landed" [("first", True)] =<< outcomesOf r
        after <- document path
        -- Stated as the whole file: everything ahead of the keyword and past it
        -- is the same string it was, by the same assertion as the edit.
        assertEqual "the file is the old one with one word replaced"
                    (T.replace "* NEXT First" "* WAITING First" before) after
        onDisk <- digestOnDisk path
        assertEqual "the digest it reports is the file's" [onDisk] =<< digestsOf r

    -- SET-TITLE, which is the one CELL a reader edits as text.  The span is the
    -- title's own, so what the assertion is really about is what it did NOT
    -- touch: the keyword in front of it and the tag run behind it.
  , testCase "set-title replaces the title and nothing around it" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- ok =<< postTo a "/command" (command "set-title" ["first"] (titleArg "Renamed"))
        assertEqual "the row landed" [("first", True)] =<< outcomesOf r
        after <- document path
        assertEqual "one title replaced, the rest of the file untouched"
                    (T.replace "* NEXT First" "* NEXT Renamed" before) after
        onDisk <- digestOnDisk path
        assertEqual "the digest it reports is the file's" [onDisk] =<< digestsOf r

  , testCase "and refuses a title org would not read back as one" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        mapM_ (\(what, title) -> do
                 r <- postTo a "/command" (command "set-title" ["first"] (titleArg title))
                 assertEqual (what <> ": status") 400 (status r)
                 assertEqual (what <> ": nothing written") before =<< document path)
              [("empty", ""), ("blank", "   "), ("two lines", "one\ntwo")]

  , testCase "and a request with no title at all is a 400" $
      withCommandable $ \a _hub _path _other -> do
        r <- postTo a "/command" (command "set-title" ["first"] (object []))
        assertEqual "status" 400 (status r)
        assertContains "names the field" "\"title\"" =<< textAt "error" =<< decoded r

  , testCase "a keyword where there was none is inserted after the stars" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        assertOk =<< postTo a "/command" (command "set-state" ["second"] (keywordArg (Just "NEXT")))
        after <- document path
        assertEqual "inserted, and nothing else"
                    (T.replace "* Second" "* NEXT Second" before) after

  , testCase "a null keyword takes the word and its space off" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        assertOk =<< postTo a "/command" (command "set-state" ["first"] (keywordArg Nothing))
        assertEqual "the file closed up" (T.replace "* NEXT First" "* First" before)
          =<< document path

    -- Two rows of one file are ONE editFile, and the proof is that the second
    -- one landed at all: a write per row would pin the second to the digest the
    -- first invalidated, and drift.  The shared digest says the same thing.
  , testCase "two rows of one file are one write, and both land" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- ok =<< postTo a "/command"
               (command "set-state" ["first", "second"] (keywordArg (Just "CANCELLED")))
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
        r <- ok =<< postTo a "/command" (command "archive" ["first", "third"] (object []))
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
        r <- ok =<< postTo a "/command" (command "archive" ["first", "third"] (object []))
        assertEqual "one landed, one did not"
                    [("first", True), ("third", False)] =<< outcomesOf r
        assertContains "the untouched file took its edit" ":one:ARCHIVE:" =<< document path
        assertEqual "and the moved one is the meddler's" meddled =<< document other

  , testCase "an id no row carries is refused on its own" $
      withCommandable $ \a _hub path _other -> do
        r <- ok =<< postTo a "/command" (command "archive" ["nowhere", "first"] (object []))
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
        again <- ok =<< postTo a "/command" (command "archive" ["first"] (object []))
        assertEqual "the row still landed" [("first", True)] =<< outcomesOf again
        assertEqual "and the file is byte for byte what it was" once =<< document path
        assertEqual "one tag, not two" 1 (T.count "ARCHIVE" once)

  , testCase "a digest the store no longer holds refuses that file's rows" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        let stale = encode (object [ "name" .= ("archive" :: T.Text)
                                   , "ids" .= (["first", "second"] :: [T.Text])
                                   , "digests" .= object ["first" .= T.replicate 64 "0"] ])
        r <- ok =<< postTo a "/command" stale
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
        assertOk =<< postTo a "/command" (command "archive" ["first"] (object []))
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
        r <- ok =<< postTo a "/command" (encode (object [ "name" .= ("archive" :: T.Text)
                                                 , "id" .= ("first" :: T.Text) ]))
        assertEqual "the row landed" [("first", True)] =<< outcomesOf r
        assertContains "written" ":one:ARCHIVE:" =<< document path

  , testCase "an id named twice is written once" $
      withCommandable $ \a _hub path _other -> do
        r <- postTo a "/command" (command "archive" ["first", "first"] (object []))
        assertEqual "one result" [("first", True)] =<< outcomesOf r
        assertEqual "one tag" 1 . T.count "ARCHIVE" =<< document path

  , testCase "a body over the cap is refused before it is read" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command" (BL.fromStrict (BS.replicate (1024 * 1024 + 1) 0x78))
        assertEqual "status" 413 (status r)
        -- The cap outranks every other refusal, so nothing downstream of it
        -- ran; the file is where that is readable.
        assertEqual "and no row moved" before =<< document path

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
        r <- ok =<< postTo a "/command"
               (command "set-planning" ["first"] (planningArg "SCHEDULED" (Just "2026-08-05")))
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
        assertOk =<< postTo a "/command"
               (command "set-planning" ["first"] (planningArg "DEADLINE" Nothing))
        assertEqual "the file is what it was before the first command"
                    (T.replace "DEADLINE: <2026-08-05 Wed>\n" "" before) =<< document path

    -- Two files, two writes, one date: the clock is read once for the request,
    -- so a marked set cannot land on two days.
  , testCase "over rows in two files, each file is its own write" $
      withCommandable $ \a _hub path other -> do
        r <- ok =<< postTo a "/command"
               (command "set-planning" ["first", "third"] (planningArg "SCHEDULED" (Just "today")))
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
        r <- ok =<< postTo a "/command" (command "add-tag" ["first"] (tagArg "work"))
        assertEqual "the row landed" [("first", True)] =<< outcomesOf r
        assertEqual "the file is the old one with one tag more"
                    (T.replace "* NEXT First :one:" "* NEXT First :one:work:" before)
          =<< document path
        onDisk <- digestOnDisk path
        assertEqual "the digest it reports is the file's" [onDisk] =<< digestsOf r

  , testCase "and opens a run on a row that had none" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        assertOk =<< postTo a "/command" (command "add-tag" ["second"] (tagArg "work"))
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
        assertOk =<< postTo a "/command" (command "add-tag" ["first"] (tagArg "home"))
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
        r <- postTo a "/command" (command "add-tag" ["first"] (tagArg "a.b"))
        assertEqual "status" 400 (status r)
        assertContains "names what it turned down" "a.b" (body r)
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
        r <- ok =<< postTo a "/command" (command "add-tag" ["first", "nosuch"] (tagArg "work"))
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
        r <- ok =<< postTo a "/command" (command "rename-tag" ["first"] (renameArg "one" "two"))
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
        assertOk =<< postTo a "/command" (command "rename-tag" ["first"] (renameArg "two" "one"))
        assertEqual "byte for byte" before =<< document path

    -- A row that does not carry the old name costs no edit, which is what makes
    -- the command safe to send over the whole set the popup was raised on.
  , testCase "a row that never carried it lands, changing nothing" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- ok =<< postTo a "/command" (command "rename-tag" ["second"] (renameArg "one" "two"))
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
            [("one", "a.b", "a.b"), ("a.b", "one", "a.b"), ("one", "", "")]

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
        r <- ok =<< postTo a "/command" (command "rename-tag" ["first", "nosuch"]
                                          (renameArg "one" "two"))
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
        r <- ok =<< getFrom a "/tags?ids=both"
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
        r <- ok =<< getFrom a "/tags?ids=nosuch,both"
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
        r <- ok =<< postTo a "/command" (capture "TODO Buy milk :errands:")
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
        r <- ok =<< postTo a "/command" (capture "a note")
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
        assertOk =<< postTo a "/command" (capture "no ids here")

    -- THE ID THE ANSWER CARRIES is where point lands when the watch delivers
    -- the row, so it has to be the id the next load spells: the target file's
    -- path and the ordinal behind the rows the store already holds.
  , testCase "the answer names the row the capture made" $
      withCaptureTree Nothing $ \a hub dir -> do
        r <- ok =<< postTo a "/command" (capture "TODO Buy milk")
        assertEqual "the file's own path and the next ordinal"
                    (T.pack (dir </> "inbox.org") <> "#0") =<< textAt "id" =<< decoded r
        watchStep hub (dir </> "inbox.org")
        rows <- rowsOf =<< getFrom a "/headlines"
        assertBool ("the store spells the same id: " <> show (map rowId rows))
                   ((T.pack (dir </> "inbox.org") <> "#0") `elem` map rowId rows)

    -- The inbox CREATES too, where the target is not there yet, so it queues
    -- its path by the same rule the blob does.  The directory over it is the
    -- served root and is watched, so the nudge is the redundant one of the two
    -- — and that is the point: it is one rule for both shapes rather than a
    -- special case for the one that needed it.
  , testCase "a capture that creates its target delivers the row itself" $
      withCaptureTree Nothing $ \a hub dir -> do
        rid <- textAt "id" =<< decoded =<< ok =<< postTo a "/command" (capture "TODO Buy milk")
        drainNow dir hub
        rows <- rowsOf =<< getFrom a "/headlines"
        assertBool ("the captured row is there: " <> show (map rowId rows))
                   (rid `elem` map rowId rows)
  ]

-- | A TAGGED capture: the blob in the store, org-glance's own citizen.
--
-- What is pinned here is the whole path from the request to the two files it
-- leaves — the sharded blob and the @EXTERNAL.jsonl@ line naming it — plus the
-- refusals, which are decided before a byte is written.
blobCaptureSpec :: TestTree
blobCaptureSpec = testGroup "POST /command capture, under a tag"
  [ testCase "writes a blob at org-glance's own sharded path" $
      withStoreTree $ \a _hub dir -> do
        v <- decoded =<< ok =<< postTo a "/command" dune
        ident <- textAt "id" v
        path <- textAt "file" v
        assertEqual "sharded by the id's first two characters"
                    (T.pack (dir </> ".org-glance/data") <> "/" <> T.take 2 ident
                       <> "/" <> T.drop 2 ident <> "/data.org")
                    path
        assertEqual "the id is a UUID" [8, 4, 4, 4, 12] (map T.length (T.splitOn "-" ident))
        written <- document (T.unpack path)
        assertContains "the tag is on the headline" ("* Dune :book:") written
        assertContains "and the id is the drawer's" (":ORG_GLANCE_ID: " <> ident) written
        assertContains "beside the creation time" ":ORG_GLANCE_CREATION_TIME: [" written

    -- THE ROW ARRIVES LIVE, and this case names no path to make it.  A blob's
    -- `<shard>/<rest>/' pair is one `createDirectoryIfMissing True', which
    -- fsnotify arms without traversing into, so no event is ever coming for it:
    -- the daemon queues the path itself at write time and the drain loop reads
    -- the QUEUE.  `drain' is handed the directory and the hub and nothing else,
    -- so this passes only because the capture put its own blob there.
  , testCase "and the row arrives with no event behind it" $
      withStoreTree $ \a hub dir -> do
        ident <- textAt "id" =<< decoded =<< ok =<< postTo a "/command" dune
        drainNow dir hub
        rows <- rowsOf =<< getFrom a "/headlines"
        assertBool ("the blob is a row: " <> show (map rowId rows))
                   (ident `elem` map rowId rows)

    -- AND SO DOES EVERY WRITE AFTER IT, which "the writes that CREATE" got
    -- wrong: the shard is unwatched for the life of the daemon, so a state set
    -- on the row the capture just made was written correctly to the file and
    -- never reached the table.  Every write route leaves through
    -- `Glance.Web.Watch.writeSpans', so the rule is the daemon queueing every
    -- path it writes rather than a list of the ones that create.
  , testCase "and so does a later write to that same blob" $
      withStoreTree $ \a hub dir -> do
        ident <- textAt "id" =<< decoded =<< ok =<< postTo a "/command" dune
        drainNow dir hub
        assertOk =<< postTo a "/command"
                       (command "set-state" [ident] (keywordArg (Just "READING")))
        drainNow dir hub
        rows <- rowsOf =<< getFrom a "/headlines"
        state <- traverse (cellAt "state") [ r | r <- rows, rowId r == ident ]
        assertEqual "the table caught up with the file" ["READING"] state

    -- The note rides the write door every other write leaves through, so a
    -- capture costs no rule of its own: blob first, line second.
  , testCase "and one EXTERNAL.jsonl line naming it" $
      withStoreTree $ \a _hub dir -> do
        ident <- textAt "id" =<< decoded =<< ok =<< postTo a "/command" dune
        noted <- document (dir </> ".org-glance/meta/EXTERNAL.jsonl")
        assertEqual "one line" 1 (length (T.lines noted))
        assertContains "naming the blob's own id" ("{\"id\":\"" <> ident <> "\"") noted

    -- AND SO DOES A MATERIALIZE COMMIT, which is the fifth write site and the
    -- one no case pinned: `POST /headline' leaves through the same door, so a
    -- subtree rewritten inside a shard fsnotify never entered still reaches the
    -- table.  `drain' is handed the directory and the hub and names no path.
  , testCase "and so does a materialize commit into that shard" $
      withStoreTree $ \a hub dir -> do
        ident <- textAt "id" =<< decoded =<< ok =<< postTo a "/command" dune
        drainNow dir hub
        before <- decoded =<< ok =<< getFrom a (headlinePath ident)
        org <- textAt "org" before
        digest <- textAt "digest" before
        assertOk =<< postTo a (headlinePath ident)
                       (commitBody (T.replace "* Dune" "* READING Dune" org) digest)
        drainNow dir hub
        rows <- rowsOf =<< getFrom a "/headlines"
        state <- traverse (cellAt "state") [ r | r <- rows, rowId r == ident ]
        assertEqual "the table caught up with the commit" ["READING"] state

    -- The tag's TEMPLATE is what a blob is shaped by, and the answers ride in
    -- `fields'.
  , testCase "the tag's template is expanded, prompts and all" $
      withStoreTree $ \a _hub _dir -> do
        v <- decoded =<< ok =<< postTo a "/command"
                                  (captureAs "book" [("Author", "Herbert")] "Dune")
        written <- document . T.unpack =<< textAt "file" v
        assertContains "the point took the line" "Dune" written
        assertContains "the ask took its answer" ":AUTHOR: Herbert" written
        assertContains "and the template's own child came with it" "*** Notes" written

  , testCase "a tag no layer configures takes the bare template" $
      withStoreTree $ \a _hub _dir -> do
        v <- decoded =<< ok =<< postTo a "/command" (captureAs "web" [] "a link")
        written <- document . T.unpack =<< textAt "file" v
        assertEqual "one entry and its drawer, and nothing else"
                    [ "* a link :web:", ":PROPERTIES:", ":END:" ]
                    [ l | l <- T.lines written, not (":ORG_GLANCE_" `T.isPrefixOf` l) ]

    -- THREE REFUSALS, each of them the whole request's and each of them ahead of
    -- any write.
  , testCase "an unanswered prompt is a 400 naming it, and writes nothing" $
      withStoreTree $ \a _hub dir -> do
        r <- postTo a "/command" (captureAs "book" [] "Dune")
        assertEqual "status" 400 (status r)
        assertContains "naming the prompt" "Author" (body r)
        assertEqual "and no blob was written" [] =<< blobsIn dir

  , testCase "a template with no %? is a 400 naming what it lacks" $
      withStoreTree $ \a _hub dir -> do
        TIO.writeFile (tagFileIn dir "film") "#+TITLE: Film\n\n* nothing here\n"
        r <- postTo a "/command" (captureAs "film" [] "Alien")
        assertEqual "status" 400 (status r)
        assertContains "naming the code" "%?" (body r)
        assertEqual "and no blob was written" [] =<< blobsIn dir

    -- THE ONE-HEADLINE WALL REACHES THE TAGGED PATH.  Both the line and every
    -- `fields' answer are spliced into the same document, so a newline in either
    -- lands a column-1 star the parser reads as a second entry — and a blob
    -- holds ONE entry, the headline org-glance keys it by.
  , testCase "a captured line carrying a newline is a 400, and writes nothing" $
      withStoreTree $ \a _hub dir -> do
        r <- postTo a "/command" (captureAs "book" [("Author", "Herbert")] "a\n* b")
        assertEqual "status" 400 (status r)
        assertContains "naming the shape" "one headline" (body r)
        assertEqual "and no blob was written" [] =<< blobsIn dir

  , testCase "and so is an answer carrying one, named by its prompt" $
      withStoreTree $ \a _hub dir -> do
        r <- postTo a "/command" (captureAs "book" [("Author", "H\n* b")] "Dune")
        assertEqual "status" 400 (status r)
        assertContains "naming the field" "Author" (body r)
        assertEqual "and no blob was written" [] =<< blobsIn dir

  , testCase "an answer stripped to nothing is refused too" $
      withStoreTree $ \a _hub dir -> do
        r <- postTo a "/command" (captureAs "book" [("Author", "   ")] "Dune")
        assertEqual "status" 400 (status r)
        assertContains "naming the field" "Author" (body r)
        assertEqual "and no blob was written" [] =<< blobsIn dir

  , testCase "a tag that is not one is refused with the request's shape" $
      withStoreTree $ \a _hub dir -> do
        r <- postTo a "/command" (captureAs "not a tag" [] "x")
        assertEqual "status" 400 (status r)
        assertEqual "and no blob was written" [] =<< blobsIn dir

    -- A tree with no store is not made into one by asking: those directories
    -- are org-glance's and a daemon that made them would be deciding for it.
  , testCase "a tree with no store refuses a tagged capture, naming it" $
      withCaptureTree Nothing $ \a _hub dir -> do
        r <- postTo a "/command" (captureAs "book" [] "Dune")
        assertEqual "status" 400 (status r)
        assertContains "naming the directory" ".org-glance" (body r)
        assertEqual "and no blob was written" [] =<< blobsIn dir

    -- The untagged path is untouched by all of it, which is the whole point of
    -- the tag being optional.
  , testCase "and with no tag it is still the inbox, bare" $
      withStoreTree $ \a _hub dir -> do
        v <- decoded =<< ok =<< postTo a "/command" (capture "TODO Buy milk")
        assertEqual "the tree's inbox" (T.pack (dir </> "inbox.org")) =<< textAt "file" v
        assertEqual "no blob at all" [] =<< blobsIn dir
        written <- document (dir </> "inbox.org")
        assertEqual "the entry the bare path has always written"
                    [ "* TODO Buy milk", ":PROPERTIES:", ":END:" ]
                    [ l | l <- T.lines written, not (":ORG_GLANCE_" `T.isPrefixOf` l) ]
  ]

-- | @GET \/capture@: what a capture will ask for before it asks it.
captureViewSpec :: TestTree
captureViewSpec = testGroup "GET /capture"
  [ testCase "a tag's template names its prompts, in template order" $
      withStoreTree $ \a _hub _dir -> do
        v <- decoded =<< ok =<< getFrom a "/capture?tag=book"
        assertEqual "there is one" True =<< boolAt "template" v
        assertEqual "and this is what it asks" ["Author"] =<< textsAt "prompts" v

  , testCase "a tag with no layer has no template and asks nothing" $
      withStoreTree $ \a _hub _dir -> do
        v <- decoded =<< ok =<< getFrom a "/capture?tag=web"
        assertEqual "none" False =<< boolAt "template" v
        assertEqual "and nothing to ask" [] =<< textsAt "prompts" v

    -- With no tag it is the untagged path's own shape: the inbox capture stays
    -- bare, so there is nothing to resolve and the answer says so.
  , testCase "with no tag at all it is the bare shape" $
      withStoreTree $ \a _hub _dir -> do
        v <- decoded =<< ok =<< getFrom a "/capture"
        assertEqual "no template" False =<< boolAt "template" v
        assertEqual "no prompts" [] =<< textsAt "prompts" v

    -- The vocabulary is what the tag prompt completes over, and it is the
    -- TREE's rather than any row's — a capture names no rows to ask about.
  , testCase "the tag vocabulary is the tree's" $
      withStoreTree $ \a _hub _dir ->
        assertEqual "every tag the store holds" ["book"]
          =<< textsAt "tags" =<< decoded =<< getFrom a "/capture"

    -- ONE spelling of the expansion subset: what this serves is what expands and
    -- what the settings box completes over.
  , testCase "the codes are the expansion subset, each with its meaning" $
      withStoreTree $ \a _hub _dir -> do
        codes <- listAt "codes" =<< decoded =<< getFrom a "/capture"
        assertEqual "the four v1 knows" ["%?", "%U", "%T", "%^{PROMPT}"]
          =<< traverse (textAt "code") codes
        assertBool "and each says what it does"
          . all (not . T.null) =<< traverse (textAt "means") codes

  , testCase "and it is a read: POST is a 405" $
      withStoreTree $ \a _hub _dir -> do
        r <- postTo a "/capture" "{}"
        assertEqual "status" 405 (status r)
  ]

-- | Run K over a server whose tree keeps an org-glance store and a @book@ layer
-- carrying a capture template with an ask in it.
withStoreTree :: (Application -> Hub -> FilePath -> Assertion) -> Assertion
withStoreTree k = withTempDir $ \dir -> do
  writeLayers dir
    [ ( Just "book"
      , "#+TITLE: Book\n#+TODO: TODO READING | READ\n\n\
        \* %?\n:PROPERTIES:\n:AUTHOR: %^{Author}\n:END:\n*** Notes\n" ) ]
  createDirectoryIfMissing True (dir </> ".org-glance" </> "data")
  _ <- orgFile dir "notes.org" "* TODO Already here :book:\n"
  (a, hub) <- serverOver dir
  k a hub dir

-- | Every file under DIR's store, which is how a refusal is checked to have
-- written nothing.  Spelled out rather than taken off the walk: the walk is the
-- subject of other cases here, and an oracle derived from it would agree with
-- any change to it.
blobsIn :: FilePath -> IO [FilePath]
blobsIn dir = under (dir </> ".org-glance" </> "data")
  where
    under at = do
      isDir <- doesDirectoryExist at
      if not isDir then pure [ at | at /= dir </> ".org-glance" </> "data" ] else
        concat <$> (mapM (under . (at </>)) . sort =<< listDirectory at)

-- | The fixture's own tagged capture, with its template's one ask answered.
dune :: BL.ByteString
dune = captureAs "book" [("Author", "Herbert")] "Dune"

-- | A capture as the shell sends a TAGGED one: the tag, the answers its
-- template asked for, and the line.
captureAs :: T.Text -> [(T.Text, T.Text)] -> T.Text -> BL.ByteString
captureAs tag answers text' = encode (object
  [ "name" .= ("capture" :: T.Text)
  , "args" .= object ([ "text" .= text', "tag" .= tag ]
                        <> [ "fields" .= object [ Key.fromText k .= v | (k, v) <- answers ]
                           | not (null answers) ]) ])

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
        r <- ok =<< getFrom a "/config"
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
        -- a reader is looking at cannot disagree — including the ORDER, which
        -- is org's own pair and then the line above spelled left to right.
        keywords <- field "keywords" v
        assertEqual "active" ["TODO", "READING"] =<< textsAt "active" keywords
        assertEqual "inactive" ["DONE", "READ", "ABANDONED"] =<< textsAt "inactive" keywords

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
        assertOk =<< postTo a "/config" (viewBody (systemAt dir) [] (Just "tag:work") digest)
        assertContains "the line is in the file" "#+GLANCE_DEFAULT_FILTER: tag:work"
          =<< document (T.unpack (systemAt dir))
        v <- decoded =<< getFrom a "/config"
        assertEqual "and the next read says so" "tag:work" =<< textAt "filter" v

    -- THE FIRST CONFIG DIRECTORY IN A TREE THAT HAD NONE, which was a known gap
    -- and is now the same door a capture uses.  `.org-glance/config/' is two
    -- directories minted at once, and fsnotify arms a new directory without
    -- traversing into it, so nothing was ever going to deliver that write; the
    -- route queues the path it wrote and the drain loop reads the QUEUE, which
    -- is why this case names no path to `drain'.  A config path settles as a
    -- RESEED, so what moves is the whole tree's classification.
  , testCase "the first config layer in a tree reseeds it with no event behind it" $
      withTempDir $ \dir -> do
        _ <- orgFile dir "a.org" "* STARTED refactor\n"
        (a, hub) <- serverOver dir
        let stateCells = traverse (cellAt "state") <=< rowsOf <=< getFrom a
        assertEqual "before, the word is title text" [""]
          =<< stateCells "/headlines"
        assertOk =<< postTo a "/config"
                       (configBody (systemAt dir) ["#+TODO: TODO STARTED | DONE"] "")
        drainNow dir hub
        assertEqual "after, it is a state" ["STARTED"] =<< stateCells "/headlines"
        assertEqual "and the palette moved with it" ["TODO", "STARTED", "DONE"]
          =<< badgeValues =<< decoded =<< getFrom a "/headlines"

  , testCase "an emptied default view takes the line away" $
      withConfigTree $ \a dir -> do
        digest <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        _ <- postTo a "/config" (viewBody (systemAt dir) [] (Just "tag:work") digest)
        fresh <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        assertOk =<< postTo a "/config" (viewBody (systemAt dir) [] (Just "") fresh)
        after <- document (T.unpack (systemAt dir))
        assertBool ("the line is gone: " <> show after)
                   (not ("GLANCE_DEFAULT_FILTER" `T.isInfixOf` after))
        assertEqual "so the built-in answers again" "state:*active*"
          =<< textAt "filter" =<< decoded =<< getFrom a "/config"

    -- THE CAPTURE TEMPLATE is a REGION of the same file, so it is served beside
    -- the lines and written in the same drift-locked call: one file, one digest.
    -- Every layer may carry one, which is what tells it from the two tree-wide
    -- lines beside it.
  , testCase "each layer's capture template is served verbatim" $
      withConfigTree $ \a _dir -> do
        layers <- listAt "layers" =<< decoded =<< getFrom a "/config"
        assertEqual "the first heading of each, to the end of the file"
                    ["", "* Book", "* %?"] =<< traverse (textAt "template") layers

  , testCase "and written back in the same call as the block" $
      withConfigTree $ \a dir -> do
        digest <- digestOnDisk (T.unpack (tagAt dir "book"))
        assertOk =<< postTo a "/config"
          (templateBody (tagAt dir "book") ["#+TODO: TODO READING | READ ABANDONED"]
                        Nothing Nothing (Just "* %?\n:PROPERTIES:\n:AUTHOR: %^{Author}\n:END:")
                        digest)
        after <- document (T.unpack (tagAt dir "book"))
        assertEqual "the pragmas above it keep their bytes, and the template moved"
                    "#+TITLE: Book\n#+TODO: TODO READING | READ ABANDONED\n\n\
                    \* %?\n:PROPERTIES:\n:AUTHOR: %^{Author}\n:END:\n"
                    after
        assertEqual "and the read answers with what was written"
                    "* %?\n:PROPERTIES:\n:AUTHOR: %^{Author}\n:END:"
          =<< textAt "template" . (!! 1) =<< listAt "layers" =<< decoded =<< getFrom a "/config"

  , testCase "an empty template takes the heading away" $
      withConfigTree $ \a dir -> do
        digest <- digestOnDisk (T.unpack (tagAt dir "book"))
        assertOk =<< postTo a "/config"
          (templateBody (tagAt dir "book") ["#+TODO: TODO READING | READ ABANDONED"]
                        Nothing Nothing (Just "") digest)
        assertEqual "the pragmas survive alone"
                    "#+TITLE: Book\n#+TODO: TODO READING | READ ABANDONED\n\n"
          =<< document (T.unpack (tagAt dir "book"))

    -- ONE WALL, and it is what keeps a blob's first headline the entry
    -- org-glance keys it by.
  , testCase "a template that is not one top entry is a 400 that writes nothing" $
      withConfigTree $ \a dir -> do
        before <- document (T.unpack (tagAt dir "book"))
        digest <- digestOnDisk (T.unpack (tagAt dir "book"))
        r <- postTo a "/config"
               (templateBody (tagAt dir "book") ["#+TODO: TODO READING | READ ABANDONED"]
                             Nothing Nothing (Just "** %?") digest)
        assertEqual "status" 400 (status r)
        assertContains "naming the rule" "top entry" (body r)
        assertEqual "and the file is untouched" before
          =<< document (T.unpack (tagAt dir "book"))

    -- A default view belongs to a TREE rather than to a tag, so a tag layer's
    -- write leaves the line alone whatever it named.
  , testCase "a tag layer cannot set the default view" $
      withConfigTree $ \a dir -> do
        digest <- digestOnDisk (T.unpack (tagAt dir "book"))
        assertOk =<< postTo a "/config"
               (viewBody (tagAt dir "book") ["#+TODO: TODO | DONE"] (Just "tag:work") digest)
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
        assertOk =<< postTo a "/config" (captureBody (systemAt dir) [] (Just "notes/in.org") digest)
        assertContains "the line is in the file" "#+GLANCE_CAPTURE_TARGET: notes/in.org"
          =<< document (T.unpack (systemAt dir))
        assertEqual "and the next read says so" "notes/in.org"
          =<< textAt "capture" =<< decoded =<< getFrom a "/config"

  , testCase "an emptied capture target takes the line away" $
      withConfigTree $ \a dir -> do
        digest <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        _ <- postTo a "/config" (captureBody (systemAt dir) [] (Just "notes/in.org") digest)
        fresh <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        assertOk =<< postTo a "/config" (captureBody (systemAt dir) [] (Just "") fresh)
        after <- document (T.unpack (systemAt dir))
        assertBool ("the line is gone: " <> show after)
                   (not ("GLANCE_CAPTURE_TARGET" `T.isInfixOf` after))

  , testCase "a tag layer cannot set it either" $
      withConfigTree $ \a dir -> do
        digest <- digestOnDisk (T.unpack (tagAt dir "book"))
        assertOk =<< postTo a "/config"
               (captureBody (tagAt dir "book") ["#+TODO: TODO | DONE"] (Just "in.org") digest)
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
        let system = systemFileIn dir
        createDirectoryIfMissing True (takeDirectory system)
        TIO.writeFile system
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
        r <- ok =<< postTo a "/config"
               (configBody (tagAt dir "book") ["#+TODO: TODO READING NEXT | READ"] digest)
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
        assertOk =<< postTo a "/config" (configBody (tagAt dir "film") ["#+TODO: A | B"] digest)
        -- After the `#+TITLE:' run the file opens with, which is where org
        -- would have put it, and ahead of everything that is not a header.
        assertEqual "placed under the header"
                    "#+TITLE: Film\n#+TODO: A | B\n\n* %?\n" =<< document path

  , testCase "creates the file, and the directories over it" $
      withConfigTree $ \a dir -> do
        assertOk =<< postTo a "/config"
               (configBody (systemAt dir) ["#+TODO: TODO STARTED | DONE"] "")
        assertEqual "the whole file is the block"
                    "#+TODO: TODO STARTED | DONE\n" =<< document (T.unpack (systemAt dir))

  , testCase "an empty block takes the layer's line off" $
      withConfigTree $ \a dir -> do
        let path = T.unpack (tagAt dir "book")
        digest <- digestOnDisk path
        assertOk =<< postTo a "/config" (configBody (tagAt dir "book") [] digest)
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
              -- ONE ROW, because WHAT a block may say is `configEdits'' rule and
              -- `TestConfig' enumerates it there; what this route owes is that
              -- the refusal is a 400 and that nothing was written.
              [ ("a headline is not a pragma", ["* TODO not a pragma"]) ]
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
  let tags = tagsDirIn dir
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
        r <- ok =<< getFrom a "/keywords?ids=filed"
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
        r <- ok =<< getFrom a "/keywords?ids=nosuch,tagged"
        assertEqual "the ones that are gone" ["nosuch"] =<< textsAt "unknown" =<< decoded r
        assertEqual "resolved for the one that is not"
          [ ("default", ["TODO"],     ["DONE"])
          , ("system",  ["STARTED"],  ["READ"])
          , ("book",    ["READING"],  []) ] =<< sourcesOf r

  , testCase "every id unknown resolves nothing and still says which" $
      withLayeredTree $ \a -> do
        r <- ok =<< getFrom a "/keywords?ids=nosuch"
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

    -- The SOURCES are the chain's order and the WORDS INSIDE ONE are its
    -- layer's own, left to right off the `#+TODO:' line.  Both cells here are
    -- spelled against the alphabet on purpose: this answer is what the state
    -- palette draws and what its letters are assigned over, so a sorted one
    -- would move a reader's keys every time a tree added a word.
  , testCase "a source's keywords arrive in the order its line spells them" $
      withTempDir $ \dir -> do
        writeLayers dir
          [ (Nothing,     "#+TODO: STARTED PENDING DELEGATED | CANCELLED ABANDONED\n")
          , (Just "book", "#+TODO: READING SHELVED | READ\n") ]
        _ <- orgFile dir "a.org" (T.unlines
               [ "* one :book:", ":PROPERTIES:", ":ORG_GLANCE_ID: only", ":END:" ])
        (a, _hub) <- serverOver dir
        assertEqual "each cell as its layer wrote it, never sorted"
          [ ("default", ["TODO"],                            ["DONE"])
          , ("system",  ["STARTED", "PENDING", "DELEGATED"], ["CANCELLED", "ABANDONED"])
          , ("book",    ["READING", "SHELVED"],              ["READ"]) ]
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
        r <- ok =<< postTo a "/command"
               (linkCommand "edit-link" ["first"]
                       (object ["span" .= sp, "target" .= ("https://z.example" :: T.Text)])
                       [("first", digest)])
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
        r <- ok =<< postTo a "/command"
               (linkCommand "edit-link" ["first"]
                       (object ["span" .= sp, "target" .= ("https://z.example" :: T.Text)])
                       [("first", "0000")])
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

    -- THE ROW COUNT IS THE COARSEST THING WRONG with the request, so it is what
    -- the refusal names: a caller that named three rows has misunderstood the
    -- command, and telling it about a missing span instead would answer the
    -- smaller question.  It is `csArgs' asking, the same function the span and
    -- the target go through — there is no separate ids rule above it.
  , testCase "and the count outranks everything else its args owe" $
      withLinkable $ \a _hub _path -> do
        r <- postTo a "/command"
               (linkCommand "edit-link" ["first", "second"] (object []) [])
        assertEqual "status" 400 (status r)
        assertContains "the count outranks the missing span" "one row" (body r)

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
        r <- ok =<< getFrom a "/links?id=linked"
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
-- | DIR's system layer and its layer for TAG.  Read off the library's own
-- layout ('Data.Org.Config.configDirIn'), so a layout that moves takes these
-- fixtures with it rather than leaving them building a tree the server no
-- longer reads as config.
--
-- The cost is that fixture and route now travel together, so no case reached
-- through here can fail on a layout move.  What catches one is the LITERAL
-- spelling in @\"says which paths there are\"@ above, and @TestConfig@'s own:
-- this group has ONE pin on the layout rather than a case each.
systemAt :: FilePath -> T.Text
systemAt = T.pack . systemFileIn

tagAt :: FilePath -> FilePath -> T.Text
tagAt dir tag = T.pack (tagFileIn dir tag)

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
layerBody path lines' want target = templateBody path lines' want target Nothing

-- | 'layerBody' also naming the layer's CAPTURE TEMPLATE, which is a region of
-- the same file and rides in the same drift-locked write.
templateBody :: T.Text -> [T.Text] -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text
             -> T.Text -> BL.ByteString
templateBody path lines' want target template digest = encode (object
  ([ "path" .= path, "lines" .= lines', "digest" .= digest ]
     <> [ "filter" .= f | Just f <- [want] ]
     <> [ "capture" .= c | Just c <- [target] ]
     <> [ "template" .= t | Just t <- [template] ]))

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
      r <- ok =<< get assetsDir "/"
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
            , "const holds = (q) => can(table, \"getQuery\")"
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
            , "can(table, \"stripLastToken\")"
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
            , "&& (raw ? el(\"mtext\").value !== base : edited() !== baseProps);"
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
            , "base = raw ? sent.org : base;"
            , "baseProps = raw ? null : JSON.stringify([sent.properties, sent.planning]);"
            -- A conflict keeps the sheet open and names the two keys.
            , "if (a.status === 409 && a.body.reason !== \"planning\") sync(\"conflict\");"
            , "conflict — C-x C-s overwrite · ESC discard"
            , "if (s.state === \"conflict\" || s.state === \"error\") {"
            , "append(s.scope, \"info\", s.closed);"
            , "closed: \"closed without writing — the file is as it was\","
            -- And a tab closing on an edited sheet still owes the file.
            , "addEventListener(\"beforeunload\""
            , "post(editing.id, editing.digest, asked(), { keepalive: true }, editing.child)" ] b
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
      assertBool ("app, log, kbd in that order: "
                   <> show (at "id=\"app\"", at "id=\"log\"", at "id=\"kbd\""))
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
        , (["priority-up", "priority-down"], "priority")
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
        , (["filter-drop-token"], "unmark/drop token/back")
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
      r <- ok =<< get missingAssetsDir "/"
      assertEqual "content type" (Just "text/html; charset=utf-8") (header "Content-Type" r)
      assertContains "mode" "JSON-only mode" (body r)
      assertContains "the directory it looked in" (T.pack missingAssetsDir) (body r)
      assertContains "flag" "--assets" (body r)
      assertContains "endpoint" "/headlines" (body r)
      assertBool "mounts no renderer it has not got"
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
       Just "unmark all, else drop the filter's last token")
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
    -- Org's own priority keys, and they CYCLE: a ring of three plus none, so a
    -- press is the answer where a palette would be a list of three to read.
  , (["S-<up>"],     "S-<up>",  "priority-up",                     Just "priorityUp",     "table",
       Just "cycle the priority of the marked rows, or the row at point")
  , (["S-<down>"],   "S-<down>", "priority-down",                  Just "priorityDown",   "table",
       Just "cycle the priority of the marked rows, or the row at point")
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
  , (["C-c", "C-c"], "C-c C-c", "org-ctrl-c-ctrl-c",               Just "commitEdit",     "modal",
       Just "commit the element being edited")
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
        openHelp  = Just "open links: the row here, the element in the sheet; several list them"

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
      -- It is written ONCE for the four surfaces, so the slot after the arrow is
      -- the literal command and what varies is the bracketed outcome behind it.
      assertContains "ESC's own echo"
                     "ESC → keyboard-quit (${what} unchanged)" inline
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

-- | A LETTER IS A PHYSICAL KEY, so a keyboard writing another alphabet drives
-- this page.  The rule lives in @keyName@ alone — the one function every
-- listener names a press through — and the split it draws is the whole of it:
-- @KeyA@..@KeyZ@ answer as the letter that key sits at, and everything else is
-- the CHARACTER @e.key@ reported, punctuation included.
--
-- The presses here carry both halves the way a browser delivers them: @т%KeyN@
-- is ЙЦУКЕН's @т@ on the key a Latin layout writes @n@ on.  Every OTHER case in
-- this file presses a character with no @code@ at all, which is the fallback
-- half — a browser that sends none, and the suite's own events.
layoutSpec :: IO T.Text -> TestTree
layoutSpec shell = testGroup "Shell layout"
  [ -- The complaint this answers: a reader with the Cyrillic layout up pressed
    -- `n' and the table sat there, `т' being no binding of anything.
    keyed shell "a Cyrillic press moves on the key the letter sits at"
      "т%KeyN т%KeyN" "" $ \answer -> do
        rowIs "two rows down" "r3" answer
        -- The pill speaks the BINDING's own spelling, which is what says the
        -- press resolved to the map rather than to the character.
        echoIs "under the map's own name for the key" "n → next-row" answer

  , keyed shell "and both movement dialects are the keys they sit at"
      "о%KeyJ о%KeyJ л%KeyK" "" $ \answer -> do
        rowIs "down twice on vim's pair, back up once" "r2" answer
        echoIs "the last press" "k → previous-row" answer

    -- SHIFT IS THE UPPERCASE BINDING rather than an `S-' modifier, which is
    -- what keeps `d' and `D' the two rows they are: one flags and the other
    -- writes, and a layout must not be able to collapse them into each other.
  , testCase "shift picks the uppercase binding, and the lowercase one stays its own" $ do
      bootOf shell "" 500 "в%KeyD" "" $ \answer -> do
        assertEqual "the row is flagged and nothing is written" [] =<< postedOf answer
        assertEqual "the flag" ["r1"] =<< textsAt "flagged" answer
        echoIs "dired's first press" "d → archive-flag (flagged — d again archives)" answer
      bootOf shell "" 500 "S-В%KeyD" "" $ \answer -> do
        assertEqual "the shifted half archives the row at point"
                    [("archive", ["r1"])] =<< postedOf answer
        echoIs "and names the command it ran"
          "D → org-glance-overview:delete (archived · row)" answer

    -- PUNCTUATION IS THE CHARACTER.  `:' is Shift+Semicolon on a US layout and
    -- Shift+Digit6 on the Russian one — there is no position to bind — so the
    -- character is the honest answer and the key reaches the tag palette from
    -- either.
  , keyed shell "punctuation answers to the character, whatever key it sits on"
      "S-:%Digit6" "" $ \answer -> do
        assertEqual "the tag popup is up" "on" =<< textAt "tagpop" answer
        assertEqual "over the row at point" "tags · 1 row" =<< textAt "thead" answer

    -- A chord's second key is a letter, so it comes through the same door: the
    -- reserved-chord rule is unmoved and both presses are still claimed.
  , keyed shell "a chord completes on the physical key too" "C-c C-е%KeyT" "" $ \answer -> do
        assertEqual "the palette is up" "on" =<< textAt "prompt" answer
        assertEqual "resolved for the row the command names"
                    ["/keywords?ids=r1"] =<< textsAt "resolved" answer
        assertEqual "and neither chord was left to the browser"
                    ["C-c", "C-е%KeyT"] =<< textsAt "prevented" answer

    -- The which-key letters are `keyName''s too, and the pool is a-z by
    -- construction — so a Cyrillic press arrives already spelled in the
    -- alphabet the palette assigned from, and the letter commits.
  , keyed shell "a palette letter commits from a Cyrillic press"
      "t" "press:е%KeyT" $ \answer -> do
        assertEqual "one set-state over the row at point"
                    [("set-state", ["r1"])] =<< postedOf answer
        assertEqual "as the keyword that letter names" [Just "TODO"] =<< keywordsOf answer

    -- A FIELD KEEPS ITS CHARACTERS.  The dispatch runs outside `typing()' and
    -- the fallback field claims arrows and RET alone, so a letter over one is
    -- neither a command nor a commit — it is text, and the page leaves the
    -- press to it.
  , keyed shell "a focused field is left the character it was sent"
      "t /" "press:т%KeyN" $ \answer -> do
        assertEqual "the palette is in its typing mode" "narrow" =<< textAt "pmode" answer
        assertEqual "nothing was committed" [] =<< postedOf answer
        rowIs "and the table under it never moved" "r1" answer
        assertBool "the key was left to the field"
          . notElem "т%KeyN" =<< textsAt "prevented" answer

    -- The other half of the split, pressed: a `code' the rule does not read.
  , keyed shell "a press carrying no code at all is the character it always was"
      "n j" "" $ rowIs "two rows down" "r3"

    -- The split is one function's, and this is what says so: the RAW event
    -- fields are read inside `keyName' and nowhere else, so the dispatch, the
    -- sheet, the value palette and the popups cannot answer the question their
    -- own way — they name a press or they have no name for it.  Asserted as an
    -- absence over the glue with that function cut out of it, which no count of
    -- readers could say.
  , testCase "the raw event is read in one place, and every listener inherits it" $ do
      inline <- glueOf =<< shell
      named <- maybe (assertFailure "no keyName in the glue") pure
                     (between "function keyName(e) {" "\n    }" inline)
      holdsAll "the letter rule" ["const LETTER = /^Key([A-Z])$/;"] inline
      holdsAll "both halves of the split, in keyName" ["e.code", "e.key"] named
      holdsNone "the glue outside keyName"
        ["e.code", "e.key"] (T.replace named "" inline)
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
      -- EACH NEEDLE IS WITNESSED INSIDE THE BLOCK FIRST.  An absence over a
      -- string the page cannot hold is a test that can never fail, which is what
      -- the 16px one had become: the field roll grew and `#mtext,#pinput{' with
      -- it, so the needle named a rule that no longer exists anywhere.
      mapM_ (\needle -> do
               assertBool ("the query does not carry it: " <> show needle)
                          (needle `T.isInfixOf` coarse')
               assertBool ("a touch rule outside the query: " <> show needle)
                          (not (needle `T.isInfixOf` before)))
            ["min-height:44px", ".ctext,.cview{font-size:16px}", "tv-chips:empty"]
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
        r <- ok =<< get dir "/JetBrainsMono-Regular.woff2"
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
      r <- ok =<< get assetsDir "/table-view.js"
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
      r <- ok =<< getBuiltIn "/table-view.js"
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
      zipped <- ok =<< getWith a "/table-view.js" [("Accept-Encoding", "gzip")]
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
