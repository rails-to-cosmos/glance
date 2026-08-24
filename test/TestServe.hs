-- | The server, driven as a WAI 'Application'.  No socket is bound.
module TestServe (spec) where

import Control.Monad (filterM, forM_, unless, when, (<=<))
import Data.Aeson ( FromJSON, Value (Array, Bool, Null, Number, Object, String)
                  , eitherDecode, encode, object, parseJSON, toJSON, (.=) )
import Data.Aeson.Types (Pair, parseEither)
import Data.ByteString (ByteString)
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace)
import Data.Foldable (toList)
import Data.List (elemIndex, find, isInfixOf, nub, sort, sortOn)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.Time (fromGregorian, toGregorian)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Types ( HeaderName, RequestHeaders, methodDelete, methodPost
                          , renderQuery )
import Network.Wai (Application, defaultRequest, requestHeaders, requestMethod)
import Network.Wai.Test ( SResponse (simpleBody, simpleHeaders)
                        , request, runSession, setPath )
import System.Directory ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
                        , findExecutable, getTemporaryDirectory, listDirectory
                        , removeDirectoryRecursive )
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (getProcessID)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults ( assertContains, boolAt, committable, dateCorpus, dateCorpusPath
                    , digestOnDisk, document, field, holdsAll
                    , holdsNone
                    , columnKeysOf, columnOf, intAt, listAt, maybeTextAt, orgFile, sparseAt
                    , sparseTextAt, systemFileIn, tagFileIn, writeLayers
                    , tagsDirIn, textAt, textsAt, viewDir, viewText, withTempDir )
import TestWire ( assertOk, capture, command, drainNow, keywordArg, ok, postTo
                , serverAt, status )

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Glance.Query ( ConfigSetting (csName), QueryResult (qrRecords)
                    , blobPathIn, builtinFilter, configSettings
                    , linkColumns, loadDir, loadFile, prioritySlots, stateSlots
                    , storeRootIn, tagColumns, todoLines
                    , trashPathFor, viewJSON )
import Glance.Web ( ServeOptions (..), application, bannerLines, bootstrapWanted
                  , defaultPort, viewTitleFor )
import Glance.Web.Page.Popups (Popup (..), Tier (..), popups, tierClass)
import Glance.Web.Base (gluePartFiles, today)
import Glance.Web.Commands (commandNames)
import Glance.Web.Theme (Theme (..), themes)
import Glance.Web.Store ( Hub, applyFile, finishLoading, loadStore, newHub
                       , newLoadingHub, publish )

import qualified Glance.Web.Routes as Routes

sampleFile :: FilePath
sampleFile = viewDir <> "/sample.org"

-- | @sha256sum test\/fixtures\/view\/sample.org@ — an INDEPENDENT ORACLE, never computed here.
sampleDigest :: T.Text
sampleDigest = "ba16aa19887a04a410a1f0047b4fcee147818d0c8471e4e1db60f5bc7dfe22dc"

assetsDir :: FilePath
assetsDir = "test/fixtures/assets"

missingAssetsDir :: FilePath
missingAssetsDir = "test/fixtures/assets-not-here"

vendoredRenderer :: FilePath
vendoredRenderer = "assets/table-view.js"

served :: FilePath -> ServeOptions
served assets = builtIn { soAssets = Just assets }

builtIn :: ServeOptions
builtIn = ServeOptions { soDir = viewDir, soPort = defaultPort, soAssets = Nothing
                       , soDerived = False }

app :: FilePath -> IO Application
app assets = appOf (served assets)

-- | The app OPTS runs, over a store loaded from the directory OPTS names.
appOf :: ServeOptions -> IO Application
appOf opts = application opts <$> (newHub =<< loadStore (soDir opts))

serverOver :: FilePath -> IO (Application, Hub)
serverOver = serverAt (Just assetsDir)

get :: FilePath -> ByteString -> IO SResponse
get assets = getOf (served assets)

getBuiltIn :: ByteString -> IO SResponse
getBuiltIn = getOf builtIn

getOf :: ServeOptions -> ByteString -> IO SResponse
getOf opts path = do
  application' <- appOf opts
  getFrom application' path

getFrom :: Application -> ByteString -> IO SResponse
getFrom application' path = getWith application' path []

-- | Every PATH is the whole request's 400, and the body NAMES what it turned down.
refuses400 :: Application -> String -> [(ByteString, T.Text)] -> Assertion
refuses400 a what = mapM_ $ \(path, named) -> do
  r <- getFrom a path
  assertEqual (show path <> " status") 400 (status r)
  assertContains what named (body r)

-- | A 405 is settled ahead of any tree, so every read route answers one over the same app.
-- | THE @?ids=@ GRAMMAR at one read route: an id nothing carries is NAMED rather than refused, the parameter repeats, a comma splits it, @id@ is its singular, and none at all is a 400.  TREE is the fixture, READ' what the door answers with, KNOWN one of the tree's ids beside what it alone resolves to, and the triple two ids beside what the pair resolves to.
idsParamCases :: (Eq b, Show b)
              => ((Application -> Assertion) -> Assertion) -> ByteString
              -> (SResponse -> IO b) -> (T.Text, b) -> (T.Text, T.Text, b) -> [TestTree]
idsParamCases tree route read' (known, alone) (one, two, both) =
  [ testCase "an id the store does not hold is named and left out" $
      tree $ \a -> do
        r <- ok =<< getFrom a (route <> "?ids=nosuch," <> enc known)
        assertEqual "the ones that are gone" ["nosuch"] =<< textsAt "unknown" =<< decoded r
        assertEqual "resolved for the one that is not" alone =<< read' r

  , testCase "ids repeat, ids comma-separate, id is one, and none is a 400" $
      tree $ \a -> do
        assertEqual "repeated" both
          =<< read' =<< getFrom a (route <> "?ids=" <> enc one <> "&ids=" <> enc two)
        assertEqual "and mixed with the singular" both
          =<< read' =<< getFrom a (route <> "?ids=" <> enc one <> "&id=" <> enc two)
        assertEqual "the singular spelling answers for one" alone
          =<< read' =<< getFrom a (route <> "?id=" <> enc known)
        r <- getFrom a route
        assertEqual "status" 400 (status r)
        assertEqual "naming the parameter"
                    ("GET " <> TE.decodeUtf8 route <> "?ids=<row id>,<row id>")
          =<< textAt "error" =<< decoded r
  ]
  where enc = TE.encodeUtf8

postIs405 :: ByteString -> TestTree
postIs405 path = testCase "and it is a read: POST is a 405" $ do
  a <- app assetsDir
  r <- postTo a path "{}"
  assertEqual "status" 405 (status r)

getWith :: Application -> ByteString -> RequestHeaders -> IO SResponse
getWith application' path headers =
  runSession (request (setPath defaultRequest path) { requestHeaders = headers }) application'

-- | @\/headline?id=…@ percent-encoded: a row id is @FILE#K@, slashes and hash included.
headlinePath :: T.Text -> ByteString
headlinePath rid = "/headline" <> renderQuery True [("id", Just (TE.encodeUtf8 rid))]

childPath :: T.Text -> Int -> ByteString
childPath rid k = "/headline" <> renderQuery True
  [("id", Just (TE.encodeUtf8 rid)), ("child", Just (BSC.pack (show k)))]

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

commitBody :: T.Text -> T.Text -> BL.ByteString
commitBody org digest = encode (object ["org" .= org, "digest" .= digest])

-- | The lens-shaped commit body; the server puts its own regions back beside the parts.
splitBody :: T.Text -> [[T.Text]] -> T.Text -> BL.ByteString
splitBody body props = planningBody body props []

planningBody :: T.Text -> [[T.Text]] -> [[T.Text]] -> T.Text -> BL.ByteString
planningBody body props plan digest = encode (object
  [ "body" .= body, "properties" .= props, "planning" .= plan, "digest" .= digest ])

-- | A server over 'committable' with its first headline materialized, handed to K with the file, the answer, and the three fields a commit is composed of: the DIGEST, the BODY and the PROPERTIES.
withCommitted :: (Application -> FilePath -> Value -> T.Text -> T.Text -> [[T.Text]] -> Assertion)
              -> Assertion
withCommitted k = withTempDir $ \dir -> do
  path <- orgFile dir "notes.org" committable
  (a, _hub) <- serverOver dir
  v <- getFrom a (headlinePath "first") >>= decoded
  digest <- textAt "digest" v
  body' <- textAt "body" v
  props <- pairsAt "properties" v
  k a path v digest body' props

-- | A server over 'nestedDoc' alone: the tree the child routes address.
withNested :: (Application -> FilePath -> Assertion) -> Assertion
withNested k = withTempDir $ \dir -> do
  path <- orgFile dir "tree.org" nestedDoc
  (a, _hub) <- serverOver dir
  k a path

header :: HeaderName -> SResponse -> Maybe ByteString
header name r = lookup name (simpleHeaders r)

etagOf :: SResponse -> IO ByteString
etagOf r = maybe (assertFailure "no ETag on the response") pure (header "ETag" r)

-- | WHAT: is TAG a store's tag at generation GEN, on some day?  Spelled out
-- here rather than taken from the server.  The DAY is read for its SHAPE alone:
-- the server's own would be this test's wall clock, and the law that two days
-- are two tags is a unit's ("the same store on two days is two tags").
assertTreeTag :: String -> Int -> ByteString -> Assertion
assertTreeTag what gen tag = do
  assertBool (what <> ": no tree fingerprint in " <> show tag)
             (BSC.length fingerprint == 16
                && BSC.all (`elem` ("0123456789abcdef" :: String)) fingerprint)
  assertEqual (what <> ": generation") ("-g" <> BSC.pack (show gen)) generation
  assertBool (what <> ": no day in " <> show tag)
             (BSC.length day == 10
                && BSC.all (`elem` ("0123456789-" :: String)) day)
  where (fingerprint, rest) = BSC.splitAt 16 (BSC.drop 1 tag)
        (generation, dated) = BSC.breakSubstring dayMark rest
        day = BSC.takeWhile (/= '"') (BSC.drop (BSC.length dayMark) dated)

-- | @-d@ — how the day is joined onto a tag, spelled once for both readers.
dayMark :: ByteString
dayMark = "-d"

-- | TAG at generation N, its tree fingerprint and its DAY kept: what a client
-- holding a tag from an older generation of this very tree sends back.
atGeneration :: Int -> ByteString -> ByteString
atGeneration n tag = BSC.takeWhile (/= '-') tag <> "-g" <> BSC.pack (show n)
                       <> snd (BSC.breakSubstring dayMark tag)

zeroes :: ByteString
zeroes = BSC.replicate 16 '0'

body :: SResponse -> T.Text
body = TE.decodeUtf8 . BL.toStrict . simpleBody


echoIs :: String -> T.Text -> Value -> Assertion
echoIs what said = assertEqual what said <=< textAt "echo"

urlIs :: String -> T.Text -> Value -> Assertion
urlIs what wanted = assertEqual what wanted <=< textAt "url"

rowIs :: String -> T.Text -> Value -> Assertion
rowIs what wanted = assertEqual what wanted <=< textAt "selected"

decoded :: SResponse -> IO Value
decoded r = either (\e -> assertFailure ("response JSON: " <> e)) pure
                   (eitherDecode (simpleBody r))

rowsOf :: SResponse -> IO [Value]
rowsOf r = listAt "rows" =<< decoded r

decodedAt :: FromJSON a => T.Text -> Value -> IO a
decodedAt key v = do
  raw <- field key v
  either (\e -> assertFailure (T.unpack key <> ": " <> e)) pure (parseEither parseJSON raw)

pairsAt :: T.Text -> Value -> IO [[T.Text]]
pairsAt = decodedAt

-- | The offers KEY drew: each word with the hint beside it, which names where
-- taking that offer would land.  READ TOGETHER, off the one draw.
offersIn :: T.Text -> Value -> IO [(T.Text, T.Text)]
offersIn key = traverse one <=< listAt key
  where one v = (,) <$> textAt "word" v <*> textAt "hint" v

-- | The pair box's offers.
offersOf :: Value -> IO [(T.Text, T.Text)]
offersOf = offersIn "doffers"

wroteAt :: T.Text -> Value -> IO [[[T.Text]]]
wroteAt key = traverse (pairsAt key) <=< listAt "writes"

sheetStamp :: T.Text
sheetStamp = "<2026-08-01 Sat>"

-- | The two entries the harness's @planned@ act adds beside it: a second
-- SETTABLE word, and org's third word whose wall reparses rather than resolves.
deadStamp, closedStamp :: T.Text
deadStamp = "<2026-09-09 Wed>"
closedStamp = "[2026-08-02 Sun]"

-- | The default subtree as the pane draws it: the headline, the lifted header —
-- the planning line, then the drawer FOLDED — the body, and the child with its
-- block.  A child folds too but is no drawer, so it wears no `d-drawer' class.
-- EVERY PANE CLOSES ON THE TAIL: one synthesized empty paragraph past the end.
fixtureDoc :: [[T.Text]]
fixtureDoc =
  [ ["head", "* ", "TODO", "one"]
  , ["meta", "SCHEDULED: " <> sheetStamp]
  , ["comp:properties:drawer", ":PROPERTIES: \8230"]
  , ["para", "first para"]
  , ["para", "second para"]
  , ["child", "  * ", "two", ":web:"]
  , ["para", "child body"]
  , ["para:tail", ""] ]

-- | The shell's two header mirrors, in one reading.
headerIs :: String -> [[T.Text]] -> [[T.Text]] -> Value -> Assertion
headerIs what props plan answer =
  assertEqual what (props, plan)
    =<< ((,) <$> pairsAt "dprops" answer <*> pairsAt "dplan" answer)

fixtureOrg, fixtureBody :: T.Text
fixtureOrg = "* TODO one\nSCHEDULED: <2026-08-01 Sat>\n:PROPERTIES:\n"
  <> ":ORG_GLANCE_ID: r1\n:EFFORT: 0:30\n:END:\n:LOGBOOK:\n- moved here\n:END:\n"
  <> "first para\n\nsecond para\n** two\nchild body\n"
fixtureBody = "* TODO one\nfirst para\n\nsecond para\n** two\nchild body\n"

-- | 'tabledBody' with WAS replaced by NOW — the whole document, so every other byte is asserted with it.
tabledAfter :: T.Text -> T.Text -> T.Text
tabledAfter was now = T.replace was now tabledBody

tabledBody :: T.Text
tabledBody = T.unlines
  [ "* TODO one", "lead in", "| a | b |", "|---+---|", "| 1 | 2 |", "| 3 | 4 |"
  , "", "- alpha", "- beta", "", "tail para", "** two", "child body" ]

-- | The structured document as the sheet DREW it, read off the draw rather than out of a model.
docOf :: Value -> IO [[T.Text]]
docOf = traverse parts <=< listAt "doc"
  where parts v = mapM text' =<< listOf v
        listOf (Array xs) = pure (toList xs)
        listOf v          = assertFailure ("expected an array, got " <> show v)
        text' (String t)  = pure t
        text' v           = assertFailure ("expected a string, got " <> show v)

partsOf :: T.Text -> [[T.Text]] -> [T.Text]
partsOf kind rows = [ T.intercalate "\n" (drop 1 r) | r <- rows, take 1 r == [kind] ]

-- | Which element the document's cursor is on.
pointOf :: Value -> IO Int
pointOf = intAt "dat"

flaggedOf :: Value -> IO [Int]
flaggedOf = flaggedAt "dflagged"

-- | Each posted @set-priority@'s letter, 'Nothing' for the null that takes the token off.
prioritiesOf :: Value -> IO [Maybe T.Text]
prioritiesOf = traverse one <=< argsOf
  where one v = spelled =<< field "priority" v
        spelled Null       = pure Nothing
        spelled (String t) = pure (Just t)
        spelled other      = assertFailure ("expected a priority, got " <> show other)

flaggedAt :: T.Text -> Value -> IO [Int]
flaggedAt key = traverse whole <=< listAt key
  where whole (Number n) = pure (round n)
        whole v          = assertFailure ("expected a number, got " <> show v)

-- | V's own field names; an absent field is an answer here rather than a failure.
fieldsOf :: Value -> IO [T.Text]
fieldsOf (Object o) = pure (map Key.toText (KM.keys o))
fieldsOf v = assertFailure ("expected an object, got " <> show v)

rowId :: Value -> T.Text
rowId row = case row of
  Object o -> case KM.lookup "id" o of
    Just (String i) -> i
    _noId           -> T.pack (show row)
  _notARow -> T.pack (show row)

-- | ROW by the view's first two sort keys — an INDEPENDENT ORACLE rather than a call.
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

-- | @test\/fixtures\/view@'s keywords in palette order: org's pair, then the file's @#+TODO:@ line.
samplePalette :: [T.Text]
samplePalette = ["TODO", "NEXT", "WAITING", "DONE", "CANCELLED"]

badgeValues :: Value -> IO [T.Text]
badgeValues view = do
  cols <- listAt "columns" view
  state <- maybe (assertFailure "no state column") pure
                 (find (keyIs "state") cols)
  traverse (textAt "value") =<< listAt "badges" state
  where keyIs k (Object o) = KM.lookup "key" o == Just (String k)
        keyIs _ _notAColumn = False

between :: T.Text -> T.Text -> T.Text -> Maybe T.Text
between open close haystack
  | T.null after = Nothing
  | T.null rest  = Nothing
  | otherwise    = Just inner
  where (_before, after) = T.breakOn open haystack
        (inner, rest)    = T.breakOn close (T.drop (T.length open) after)

-- | THE FIXTURE THE WHOLE SUITE BOOTS OUT OF, acquired once: the page PLUS the
-- script it names, so a text sweep reads one universe, and on disk the five
-- files every boot hands the harness.  Written HERE rather than per boot: they
-- do not vary (`elm.js' alone is 285KB) and the harness only READS them, while
-- what does vary from boot to boot is argv, which never touches the disk.
bootFixture :: IO T.Text
bootFixture = do
  page <- (<>) <$> (body <$> get assetsDir "/")
               <*> (stripGlueComments <$> glueSource)
  dir <- bootDir
  createDirectoryIfMissing True dir
  page <$ writeFixtureTo dir page

dropBootFixture :: T.Text -> IO ()
dropBootFixture _page = do
  dir <- bootDir
  there <- doesDirectoryExist dir
  when there (removeDirectoryRecursive dir)

-- | Where those five files live.  NAMED OFF THE PROCESS so the fixture and every
-- boot compute the same directory, rather than threading it through the dozen
-- helpers a case reaches a boot through.
bootDir :: IO FilePath
bootDir = do
  base <- getTemporaryDirectory
  pid  <- getProcessID
  pure (base </> ("glance-shell-" <> show pid))

writeFixtureTo :: FilePath -> T.Text -> IO ()
writeFixtureTo dir page = do
  glueOf page >>= TIO.writeFile (dir </> "shell.js")
  elmOf page >>= TIO.writeFile (dir </> "elm.js")
  keysOf page >>= TIO.writeFile (dir </> "keys.json")
  cfgOf page >>= TIO.writeFile (dir </> "cfg.json")
  -- THE MARKUP, so the harness DERIVES which ids are fields rather than
  -- keeping a second list by hand: one forgotten row there had `typing()'
  -- read a div and answer that the keys belonged to the table.
  TIO.writeFile (dir </> "page.html") page

spec :: TestTree
spec = withResource bootFixture dropBootFixture $ \shell ->
  testGroup "Serve"
    [ headlineSpec, bannerSpec, statsSpec, cacheSpec, gzipSpec, querySpec
    , orderSpec, sortQuerySpec, columnsQuerySpec, archiveViewSpec
    , bootstrapSpec, materializeSpec, commitSpec, commandSpec, planningSpec
    , tagCommandSpec, deleteCommandSpec, renameCommandSpec, tagsSpec, captureSpec
    , propertiesSpec, blobCaptureSpec, captureViewSpec
    , configSpec, keywordsSpec, linksSpec, referSpec, editLinkSpec, indexingSpec
    , pageSpec shell, keymapSpec shell, layoutSpec shell
    , glueSpec shell, bootSpec shell, liveSpec shell, washSpec shell
    , paletteSpec shell
    , moveSpec shell, sortKeySpec shell, markSpec shell, landingSpec shell
    , commandKeySpec shell, promptKeySpec shell, whichKeySpec shell
    , cellSpanSpec shell, tagKeySpec shell
    , openKeySpec shell, narrowSpec shell, agendaSpec shell, drillSpec shell
    , logSpec shell
    , sheetSpec shell
    , dateWidgetSpec shell
    , settingsSpec shell
    , touchSpec shell
    , shellFontSpec shell, assetSpec, embeddedSpec, errorSpec ]

-- | One boot of the shell's glue, RUN: a call written and never reached matches a text search too.
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
      [ "/headlines?limit=100", "/headlines" ]
      "?q="

  , Boot "DEL over the table strips the default and shows everything"
      "" 500 "Backspace"
      [ "/headlines?q=state%3A*active*&limit=100"
      , "/headlines?q=state%3A*active*"
      , "/headlines"
      , "/headlines" ]
      -- `remember("")' writes `q' PRESENT and empty: a reader who cleared, rather than never filtered.
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

  , Boot "g applies the tree's default view over a cleared one"
      "?q=" 500 "g"
      [ "/headlines?limit=100", "/headlines"
      -- The boot's two, then the remount's ONE: a re-application swaps on the whole answer.
      , "/headlines?q=state%3A*active*" ]
      "?q=state%3A*active*"

  , Boot "and re-applies it over a deep link that narrowed past it"
      "?q=tanik" 500 "g"
      [ "/headlines?q=tanik&limit=100", "/headlines?q=tanik", "/headlines"
      , "/headlines?q=state%3A*active*" ]
      "?q=state%3A*active*"
  ]

bootSpec :: IO T.Text -> TestTree
bootSpec shell = testGroup "Shell boot"
  ([ testCase boLabel $ bootOf shell boSearch boTotal boKeys "" $ \answer -> do
       assertEqual (boLabel <> ": the fetches") boAsked =<< textsAt "asked" answer
       urlIs (boLabel <> ": the URL it settles on") boUrl answer
   | Boot{..} <- shellBoots ]
   <> [ domSpec shell ])

-- | THE HARNESS'S OWN DOM, ASSERTED BEFORE ANYTHING IS READ THROUGH IT.
domSpec :: IO T.Text -> TestTree
domSpec shell = overBoot shell "" "" $ \booted ->
  atBoot booted "the harness's own DOM answers the selectors the page writes" $
    \answer -> do
      dom <- field "dom" answer
      assertEqual "every row of the tree" 3 =<< intAt "rows" dom
      assertEqual "the descendant chain finds the selected row alone" "c1"
        =<< textAt "sel" dom
      assertEqual "`:not' takes the gutter cell out of the run" ["c0", "c1", "c2"]
        =<< textsAt "gutterless" dom
      assertEqual "an alternation is the union" 3 =<< intAt "list" dom
      -- The tree carries a DECOY outside any `tbody', so the chain is answered by its ancestors.
      assertEqual "and the class alone reaches the decoy too" 2
        =<< intAt "decoyed" dom
      assertEqual "`closest' climbs to the root it is under" True
        =<< boolAt "closest" dom
      assertEqual "`matches' answers about the element alone" True
        =<< boolAt "matches" dom
      assertEqual "a tree nobody attached stays detached" True
        =<< boolAt "detached" dom
      assertEqual "an attribute step picks the slot its keyword names"
                  "<2026-08-01 Sat>" =<< textAt "slot" dom
      assertEqual "and no other keyword answers to it" 0 =<< intAt "otherSlot" dom
      assertEqual "and the subtree's text is every text node in order"
                  "decoyc0c1c2<2026-08-01 Sat>" =<< textAt "text" dom

-- | What a booted page holds after the socket goes; 'lvMounts' is the distinction.
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

booted :: [T.Text]
booted = [ "/headlines?q=state%3A*active*&limit=100"
         , "/headlines?q=state%3A*active*"
         , "/headlines" ]

reasked :: T.Text
reasked = "/headlines?q=state%3A*active*"

shellLives :: [Live]
shellLives =
  [ -- The storm case, and the reason this group exists.  The server abandons a
    -- backlog it cannot deliver and closes with `resync'; the page revalidates and keeps its mount.
    Live "a dropped backlog costs one revalidation and keeps the mount"
      "" "" "close:resync"
      (booted <> [reasked]) ["\"t0\""] 1 "" "" "?q=state%3A*active*"

  , Live "a store that moved refreshes the rows under the same mount"
      "" "" "moved close:resync"
      (booted <> [reasked]) ["\"t0\""] 1 "" "" "?q=state%3A*active*"

    -- No `view-changed' here: the daemon-restart shape, the columns moved with no socket to say so.
  , Live "columns that moved rebuild the mount, close reason or none"
      "" "" "recolumn close:resync"
      (booted <> [reasked, reasked]) ["\"t0\""] 2 "" "" "?q=state%3A*active*"

  , Live "view-changed mid-edit rebuilds the mount and keeps the sheet's text"
      "" "Enter" "press:C-c press:' sheet:hello close:view-changed"
      (booted <> [reasked]) [] 2 "hello" "synced"
      "?q=state%3A*active*&page=sheet&row=r1"

  , Live "a sheet restored over a moved file lands in the conflict flow"
      "" "Enter" "press:C-c press:' sheet:hello rewritten close:view-changed"
      (booted <> [reasked]) [] 2 "hello" "conflict"
      "?q=state%3A*active*&page=sheet&row=r1"

  , Live "a cleared filter stays cleared through a reconnect"
      "" "Backspace" "close:resync"
      (booted <> ["/headlines", "/headlines"]) ["\"t0\""] 1 "" "" "?q="
  ]

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

washes :: IO T.Text -> String -> T.Text -> [T.Text] -> Bool -> TestTree
washes shell label acts washed stale = keyed shell label "" acts $ \answer -> do
  assertEqual "the transitions" washed =<< textsAt "washed" answer
  assertEqual "left on" stale =<< boolAt "stale" answer

-- | THE STALE WASH: one class on the document element, armed on a delay; whoever arms one clears it.
washSpec :: IO T.Text -> TestTree
washSpec shell = testGroup "Shell wash"
  [ -- The bug this group exists for.  `g' over a table that is already whole
    -- used to fetch a PAGE and mount that, replacing a complete view with a hundred rows.
    keyed shell "g swaps a view in one mount, and never through a partial one"
      "" "rows:150 press:g" $ \answer -> do
        paints <- paintsOf answer
        -- A page-sized fetch here would put a `100' between the last two.
        assertEqual "the boot's two, then the swap" [3, 3, 150] paints
        assertEqual "the table was built twice" 2 =<< intAt "mounts" answer
        assertBool ("no paint was empty: " <> show paints) (0 `notElem` paints)

  , keyedAt shell "?q=tanik%20web" 500 "a commit that repaints hands over one set of rows"
      "" "rows:150 press:Backspace" $ \answer -> do
        paints <- paintsOf answer
        assertEqual "the boot's two, then the commit's one" [3, 3, 150] paints
        assertEqual "and no remount" 1 =<< intAt "mounts" answer

  , washes shell "a page that answers dims nothing at all" "press:g close:resync" [] False

  , washes shell "a swap out past the grace dims the page, and its answer clears it"
           "hang press:g wait:400 deliver" ["on", "off"] False

    -- The COUNT: an abort and the fetch replacing it overlap, so a boolean would clear a wash still wanted.
  , washes shell "an abort hands the wash to the fetch that replaced it"
           "hang press:g wait:400 press:g wait:100 deliver" ["on", "off"] False

  , washes shell "a socket blip inside its delay dims nothing" "close:resync wait:500" [] False

  , washes shell "a socket that stays gone dims the page, and the reconnect clears it"
           "offline close:x wait:500 online until:stale=off" ["on", "off"] False

  , washes shell "and stays on while it is still gone" "offline close:x wait:500" ["on"] True

  , keyed shell "an open sheet is washed with the rows under it"
      "Enter" "offline close:x wait:500" $ \answer -> do
        assertEqual "the sheet is still up" "on" =<< textAt "modal" answer
        assertEqual "and the page is washed" True =<< boolAt "stale" answer
  ]

paintsOf :: Value -> IO [Int]
paintsOf answer = traverse count =<< listAt "paints" answer
  where count (Number n) = pure (round n)
        count other = assertFailure ("expected a row count, got " <> show other)

paletteSpec :: IO T.Text -> TestTree
paletteSpec shell = testGroup "Shell palette"
  [ keyed shell "a half-typed palette is raised again after a remount"
      "/" "filter:tan close:view-changed" $ \answer -> do
        assertEqual "mounted twice" 2 =<< intAt "mounts" answer
        assertEqual "raised again" 2 =<< intAt "raises" answer
        assertEqual "with what was typed in it" "tan" =<< textAt "palette" answer
        -- The re-raise is the common door: the stash carries the TEXT, and the
        -- reader who wants the whole grammar back presses `.' over it.
        assertEqual "both raises are the filter half" ["narrow", "narrow"]
                    =<< textsAt "doors" answer

  -- ONE BOX, TWO DOORS, and the option is the only thing that tells them apart.
  , keyed shell "`/' opens the filter half" "/" "" $ \answer -> do
        assertEqual "raised once" 1 =<< intAt "raises" answer
        assertEqual "through the narrow door" ["narrow"] =<< textsAt "doors" answer
  , keyed shell "`.' opens the whole expression" "." "" $ \answer -> do
        assertEqual "raised once" 1 =<< intAt "raises" answer
        assertEqual "through the whole door" ["whole"] =<< textsAt "doors" answer

  -- WHAT `/' REFUSES IS SPOKEN, NOT SWALLOWED: the renderer keeps the token in
  -- the box and hands the page its spelling; the page names the other door.
  , keyed shell "a shaping token refused at `/' names the door it belongs to"
      "" "shaping:sort:title" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "the refusal, whole"
                    [("info", "filter", "sort: autocomplete restricted, this key belongs to #'compose (kbd \".\")")]
                    (drop 1 strip)
        echoIs "and the pill says the same" "sort: autocomplete restricted, this key belongs to #'compose (kbd \".\")" answer
        -- Refused is not applied: nothing new is asked for, and the box keeps it.
        assertEqual "the token still standing" "sort:title" =<< textAt "palette" answer
  , keyed shell "and the door is named off the refused token's own key"
      "" "shaping:+columns:State,Title" $ \answer -> do
        strip <- map (message . cut) <$> logOf answer
        assertEqual "the sign is off and the key leads"
                    ["columns: autocomplete restricted, this key belongs to #'compose (kbd \".\")"] (drop 1 strip)
  ]

-- | The buffer-end keys as presses: the change is what the SECOND press does.
moveSpec :: IO T.Text -> TestTree
moveSpec shell = testGroup "Shell movement"
  ([ keyed shell label "" (moveScript script) $ \answer -> do
       rowIs "the row" row answer
       assertEqual pageWhat page =<< intAt "page" answer
       echoIs echoWhat echo answer
   | (label, script, row, (pageWhat, page), (echoWhat, echo)) <- ends ]
   <>
  [ testCase "the arrows step the column too, and land off the ends" $ do
      onTable shell "press:ArrowRight" $ \answer -> do
        assertEqual "the first column, from the whole-row look" 0 =<< intAt "col" answer
        echoIs "named by the header over it" "<right> → next-column (state)" answer
      onTable shell "press:ArrowRight press:ArrowRight" $
        assertEqual "and the next one" 1 <=< intAt "col"
      onTable shell "press:ArrowRight press:ArrowRight press:ArrowRight" $ \answer -> do
        assertEqual "off the cells" Null =<< field "col" answer
        echoIs "which the echo says is a landing" "<right> → next-column (row mode)" answer
      -- BACKWARD OUT OF A WHOLE ROW IS A NO-OP: there is no cell to its left,
      -- and landing on the first column made the two directions one press.
      onTable shell "press:ArrowLeft" $ \answer -> do
        assertEqual "a whole row stays whole" Null =<< field "col" answer
        echoIs "and the echo names no landing" "<left> → previous-column" answer
      onTable shell "press:ArrowRight press:ArrowLeft" $
        assertEqual "while a cell still steps back out" Null <=< field "col"

  , keyed shell "a climb keeps the column the cursor was in"
      "" (moveScript "press:f press:> press:>") $ \answer -> do
        rowIs "the row" "r6" answer
        assertEqual "the column" 0 =<< intAt "col" answer

  , keyed shell "an asset without a pager keeps the within-page jump"
      "" (moveScript "press:] pageless press:< press:<") $
        \answer -> do
          rowIs "the row" "r4" answer
          assertEqual "the page it could not leave" 2 =<< intAt "page" answer
          echoIs "the echo" "< → first-row" answer

  ])

  where
    -- Off the end, `<' and `>' are the within-page jumps they always were; on the
    -- end row each turns a page and lands on that page's own end.
    ends =
      [ ( "< takes the page's first row", "press:n press:n press:<"
        , "r1", ("the page it stayed on", 1), ("the echo", "< → first-row") )
      , ( "< on the first row turns back a page and lands on its first row"
        , "press:] press:] press:<", "r4", ("the page", 2)
        , ("the echo names it", "< → first-row (page 2/3)") )
      , ( "and stops on page one's first row"
        , "press:] press:] press:< press:< press:<", "r1", ("the page", 1)
        , ("the echo", "< → first-row") )
      , ( "> takes the page's last row", "press:>"
        , "r3", ("the page it stayed on", 1), ("the echo", "> → last-row") )
        -- `nextPage' lands on the new page's FIRST row, so without the follow-up select this answers `r4'.
      , ( "> on the last row turns a page and lands on its last row"
        , "press:> press:>", "r6", ("the page", 2)
        , ("the echo names it", "> → last-row (page 2/3)") )
      , ( "G is that key, and the last page's last row is the end of it"
        , "press:G press:G press:G press:G", "r9", ("the page", 3)
        , ("the echo", "G → last-row") ) ]

moveScript :: T.Text -> T.Text
moveScript script = "rows:9 paged:3 " <> script

-- | @^@ PROMOTES: the column at point heads the chain, and on the leading one it flips that key.
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

  , keyed shell "a whole-row selection names no column, and the key says which picks one"
      "^" "" $ \answer -> do
        assertEqual "nothing was asked of the renderer" 0 =<< intAt "sortCalls" answer
        echoIs "the echo names the key that picks a column"
          "^ → toggle-sort (no column selected — f/l to pick one)" answer

    -- `sortable' gates what a READER may reach and `sortBy' ignores it.
  , keyed shell "a column that declares no sortable is left alone" "f f ^" "" $ \answer -> do
        assertEqual "the column the cursor is in" 1 =<< intAt "col" answer
        assertEqual "nothing was asked of the renderer" 0 =<< intAt "sortCalls" answer
        echoIs "and the echo names it" "^ → toggle-sort (tag does not sort)" answer

  , keyed shell "an asset with no programmatic sort is named, not crashed into"
      "" "sortless press:f press:^" $ \answer -> do
        assertEqual "no sort was asked for" Nothing =<< sortOf answer
        echoIs "the echo" "^ → toggle-sort (this table-view.js has no sort)" answer

  , testCase "a refetch keeps the sort, and nothing re-asserts it" $ do
      bootOf shell "" 500 "f ^" "moved close:resync" $ \answer -> do
        assertEqual "one sort asked for, at the press" 1 =<< intAt "sortCalls" answer
        assertEqual "and it is still the one that was asked for"
                    (Just ("state", False)) =<< sortOf answer
      bootOf shell "" 500 "f ^" "moved close:resync press:^" $ \answer -> do
        assertEqual "the press after it flips the leader it left in force"
                    (Just ("state", True)) =<< sortOf answer
        echoIs "the echo" "^ → toggle-sort (state ▲)" answer

  , keyed shell "a remount re-seeds the chain off the query it mounts under"
      "f ^" "close:view-changed press:f press:^" $
        echoIs "the leader the query named, flipped back" "^ → toggle-sort (state ▲)"

    -- THE PRESS IS A QUERY EDIT: the renderer writes the chain into the applied query and delivers it.
  , testCase "the press writes the order into the query and asks for it" $
      bootOf shell "" 500 "f ^" "" $ \answer -> do
        urlIs "the URL carries the order" "?q=state%3A*active*+sort%3Astate%3Adesc" answer
        assertEqual "and the server was asked for it"
                    (Just "/headlines?q=state%3A*active*%20sort%3Astate%3Adesc")
          . lastOf =<< textsAt "asked" answer

  , keyedAt shell "?q=state%3ATODO" 500 "the order joins a filter already applied" "f ^" "" $
        urlIs "the predicate, then the order" "?q=state%3ATODO+sort%3Astate%3Adesc"

  , keyedAt shell "?q=state%3ATODO" 500 "DEL takes the order back off"
      "f ^" "press:Backspace" $ \answer -> do
        urlIs "the query the strip left" "?q=state%3ATODO" answer
        assertEqual "and that is what was asked for"
                    (Just "/headlines?q=state%3ATODO") . lastOf
          =<< textsAt "asked" answer
  ]

-- | Marking through the keys a reader presses: the renderer holds the marks, this page the keys.
markSpec :: IO T.Text -> TestTree
markSpec shell =
  overBoot shell "" "" $ \plain ->
  testGroup "Shell marks"
  [ -- DEL'S FIRST RUNG: ERASE THE LAST STRUCTURE STANDING, which is the
    -- backspace's own rhyme; a MARKED SET is one, so the key takes the marks off and stops.
    testCase "DEL clears the marks first, and leaves the query alone" $ do
      bootOf shell "?q=state%3ATODO+web" 500 "m m Backspace" "" $ \answer -> do
        assertEqual "the marks are gone" ([] :: [T.Text]) =<< textsAt "marked" answer
        urlIs "and the query is untouched" "?q=state%3ATODO+web" answer
        echoIs "the pill names the command that ran and counts it" "DEL → unmark-all (2)" answer

      -- The SECOND press falls through to the rung it always had, in silence.
    , keyedAt shell "?q=state%3ATODO+web" 500 "and the second DEL drops a token, as it always did"
        "m m Backspace Backspace" "" $ \answer -> do
          urlIs "one token off" "?q=state%3ATODO" answer
          echoIs "and the pill is the filter's again"
            "DEL → filter-drop-token (filter: \"state:TODO\")" answer

      -- FLAGS ARE NOT MARKS: a flag is the archive queue, and the rung leaves it where it is.
    , keyedAt shell "?q=state%3ATODO+web" 500
        "and the flags stand, being the archive queue rather than a mark"
        "d m m Backspace" "" $ \answer -> do
          assertEqual "the marks went" ([] :: [T.Text]) =<< textsAt "marked" answer
          assertEqual "the flag stayed" ["r1"] =<< textsAt "flagged" answer

    , keyedAt shell "?q=state%3ATODO+web" 500 "with nothing marked the first press is still the filter's"
        "Backspace" "" $ \answer -> do
          urlIs "one token off" "?q=state%3ATODO" answer
          echoIs "and it said so" "DEL → filter-drop-token (filter: \"state:TODO\")" answer

    , keyedAt shell "?q=state%3ATODO+web" 500 "an asset with no marks falls straight through"
        "" "bare press:Backspace" $ \answer -> do
          urlIs "one token off" "?q=state%3ATODO" answer
          echoIs "and the pill never mentioned marks"
            "DEL → filter-drop-token (filter: \"state:TODO\")" answer

  ,  atBoot plain "the mount asks for them" $
        assertEqual "marks:true reached the renderer" True <=< boolAt "marksOn"

  , atBoot plain "and names the keys a flagged row answers to" $
        assertEqual "flagHelp reached the renderer" "d/D archive · u unflag"
          <=< textAt "flagHelp"

    -- One place, so the mount turns the renderer's per-row hint off.
  , atBoot plain "and asks for no per-row hints, the key line saying it once" $
        assertEqual "actionHints:false reached the renderer" False <=< boolAt "hintsOn"

  , keyed shell "m marks the row it is on and steps to the next" "m m" "" $ \answer -> do
        assertEqual "the rows it marked" ["r1", "r2"] =<< textsAt "marked" answer
        assertEqual "and where it left the cursor" 2 =<< intAt "cursor" answer
        echoIs "counting as it went" "m → mark-toggle (marked · 2)" answer

  , keyed shell "m on a marked row unmarks it" "m" "press:ArrowUp press:m" $ \answer -> do
        assertEqual "nothing marked" [] =<< textsAt "marked" answer
        echoIs "and it says so" "m → mark-toggle (unmarked · 0)" answer

    -- After `m' the cursor is on an unmarked row, so a toggle would mark it and the count read 2.
  , keyed shell "u never marks a row, it only unmarks one" "m u" "" $ \answer -> do
        assertEqual "the first mark stands alone" ["r1"] =<< textsAt "marked" answer
        echoIs "and the count did not grow" "u → unmark (unmarked · 1)" answer

  , keyed shell "U clears every mark at once" "m m U" "" $ \answer -> do
        assertEqual "nothing left" [] =<< textsAt "marked" answer
        echoIs "the echo" "U → unmark-all (all marks and flags cleared)" answer

    -- `M' is the renderer's call because the SET is: a page it is not showing is marked too.
  , keyed shell "M marks every row loaded, not the page on show" "M" "" $ \answer -> do
        assertEqual "all three" ["r1", "r2", "r3"] =<< textsAt "marked" answer
        echoIs "counted by the renderer" "M → mark-all (marked · 3)" answer
        assertEqual "and the cursor stayed where it was" 0 =<< intAt "cursor" answer

    -- `markAll' only ADDS, so a count that did not move says every row already carried one.
  , keyed shell "and M again takes them all off" "M" "press:M" $ \answer -> do
        assertEqual "nothing marked" [] =<< textsAt "marked" answer
        echoIs "and it says which way it went" "M → mark-all (unmarked · 3)" answer
  , keyed shell "M over a partly marked set marks the rest rather than clearing"
      "m" "press:M" $ \answer -> do
        assertEqual "all three" ["r1", "r2", "r3"] =<< textsAt "marked" answer
        echoIs "" "M → mark-all (marked · 3)" answer
  , keyed shell "and a third press marks again, so the pair is a toggle"
      "M" "press:M press:M" $ \answer -> do
        assertEqual "all three" ["r1", "r2", "r3"] =<< textsAt "marked" answer
        echoIs "" "M → mark-all (marked · 3)" answer

  , testCase "d flags the row, and a second d archives it" $ do
      bootOf shell "" 500 "d" "" $ \answer -> do
        assertEqual "the row is flagged" ["r1"] =<< textsAt "flagged" answer
        assertEqual "and nothing was written" [] =<< postedOf answer
        echoIs "the pill says what the next press costs"
          "d → archive-flag (flagged — d again archives)" answer
        assertEqual "and no mark went on with it" [] =<< textsAt "marked" answer
      -- One flag is a set of one, so the single-row flow is the general one.
      bootOf shell "" 500 "d d" "" $ \answer -> do
        assertEqual "one flag is a set of one, so the second press takes it"
                    [("archive", ["r1"])] =<< postedOf answer
        assertEqual "and the flag is spent" [] =<< textsAt "flagged" answer
        echoIs "counted" "d → archive-flag (archived · 1 flagged)" answer

    -- AN ARCHIVED ROW SPENDS ITS MARK, the way it spends its flag.
  , testCase "archiving takes the archived rows' marks with their flags" $ do
      -- `m' marks and STEPS, so `p p' walks back to the row the flag is for.
      bootOf shell "" 500 "m m p p d d" "" $ \answer -> do
        assertEqual "the row that was archived" [("archive", ["r1"])] =<< postedOf answer
        assertEqual "keeps neither its flag" [] =<< textsAt "flagged" answer
        assertEqual "nor its mark, and the other row keeps its own" ["r2"]
          =<< textsAt "marked" answer
      bootOf shell "" 500 "m m m D" "" $ \answer -> do
        assertEqual "the row at point went" [("archive", ["r3"])] =<< postedOf answer
        assertEqual "and the marks the archive did not reach stand"
                    ["r1", "r2"] =<< textsAt "marked" answer

  , keyed shell "and an unmarked row costs no mark at all" "n m p p d d" "" $ \answer -> do
        assertEqual "the row at point was archived" [("archive", ["r1"])]
          =<< postedOf answer
        assertEqual "the mark on the OTHER row is untouched" ["r2"]
          =<< textsAt "marked" answer

  , keyed shell "a refused archive leaves the mark where it was"
      "" "refuse press:m press:p press:d press:d" $ \answer -> do
        assertEqual "the command went" [("archive", ["r1"])] =<< postedOf answer
        assertEqual "and the mark stands" ["r1"] =<< textsAt "marked" answer

    -- The flag stays on the ROW rather than following the cursor.
  , keyed shell "d on one row and d on another flags both and archives neither"
      "d n d" "" $ \answer -> do
        assertEqual "two rows flagged" ["r1", "r2"] =<< textsAt "flagged" answer
        assertEqual "and nothing written" [] =<< postedOf answer

    -- dired's `dd': the second press is `D' and takes the WHOLE flagged set.
  , keyed shell "the second d archives every flagged row, not just the one under it"
      "d n d n d" "press:d" $ \answer -> do
        assertEqual "all three, in one request"
                    [("archive", ["r1", "r2", "r3"])] =<< postedOf answer
        assertEqual "and no flag is left" [] =<< textsAt "flagged" answer
        echoIs "named the way D names it" "d → archive-flag (archived · 3 flagged)" answer

  , keyed shell "D on that same set does exactly what the second d does"
      "d n d n d" "press:D" $ \answer -> do
        assertEqual "the same three" [("archive", ["r1", "r2", "r3"])]
          =<< postedOf answer
        echoIs "the same pill, under its own key"
          "D → org-glance-overview:delete (archived · 3 flagged)" answer

    -- `d' is in ONCE: a HELD key reaching the handler twice would flag a row and archive it from one press.
  , keyed shell "a held d flags and stops there" "d" "repeat:d repeat:d repeat:d" $ \answer -> do
        assertEqual "still just flagged" ["r1"] =<< textsAt "flagged" answer
        assertEqual "and the burst wrote nothing" [] =<< postedOf answer

    -- `u' takes the flag off first: it is the more recent thing a reader put on the row.
  , testCase "u clears an archive flag before it touches a mark" $ do
      bootOf shell "" 500 "d" "press:ArrowUp press:u" $ \answer -> do
        assertEqual "the flag is off" [] =<< textsAt "flagged" answer
        echoIs "and it says which" "u → unmark (flag cleared)" answer
      bootOf shell "" 500 "m d" "press:u press:ArrowUp press:ArrowUp press:u" $ \answer -> do
        assertEqual "the flag went" [] =<< textsAt "flagged" answer
        assertEqual "and the mark after it" [] =<< textsAt "marked" answer

  , keyed shell "U clears the flags along with the marks" "m d" "press:U" $ \answer -> do
        assertEqual "no marks" [] =<< textsAt "marked" answer
        assertEqual "and no flags" [] =<< textsAt "flagged" answer

  , keyed shell "a table-view.js without the flag calls is named, not crashed into"
      "" "bare press:d" $ \answer -> do
        assertEqual "nothing was written" [] =<< postedOf answer
        echoIs "and it said why"
          "d → archive-flag (this table-view.js has no archive flags)" answer

    -- A throw would fail the harness outright, so what this pins is the wording.
  , keyed shell "a table-view.js without the calls is named, not crashed into"
      "" "bare press:m press:U" $ \answer -> do
        assertEqual "and it did not walk on regardless" 0 =<< intAt "cursor" answer
        echoIs "the last key said why" "U → unmark-all (this table-view.js has no marks)" answer
  ]

-- | Where point ends up: on the BOOT, and after an archive takes its row out of the view.
landingSpec :: IO T.Text -> TestTree
landingSpec shell = testGroup "Shell landing"
  [ -- A BOOT IS AN APPLIED VIEW, so it lands where every applied view lands.
    -- The renderer selects nothing until asked, so row one here is this page's own landing.
    keyed shell "a boot lands on row one, like every other applied view" "" "" $ \answer -> do
        assertEqual "the first row of the answer" (Just "r1")
          =<< maybeTextAt "selected" answer
        assertEqual "at the top of the page" 0 =<< intAt "cursor" answer
        assertEqual "and the whole set arrived behind the first page" 2 . length
          =<< listAt "paints" answer

  , keyed shell "so the first key pressed already has a row to work on" "d d" "" $ \answer -> do
        assertEqual "the row the boot landed on" [("archive", ["r1"])]
          =<< postedOf answer
        echoIs "and the pill named the write" "d → archive-flag (archived · 1 flagged)" answer

  , keyedAt shell "" 0 "an empty answer leaves nothing selected, and d says so"
      "d" "" $ \answer -> do
        assertEqual "no row is on" Nothing =<< maybeTextAt "selected" answer
        assertEqual "and the cursor is nowhere" (-1) =<< intAt "cursor" answer
        echoIs "which the key names" "d → archive-flag (no row)" answer
        assertEqual "nothing was flagged" [] =<< textsAt "flagged" answer
        assertEqual "and nothing was written" [] =<< postedOf answer

  , keyedAt shell "" 0 "and RET says which key would pick one" "Enter" "" $ \answer -> do
        assertEqual "the strip says what to press"
                    (Just "no row focused — n or p picks one") =<< lastLog answer
        assertEqual "and no sheet opened" "" =<< textAt "modal" answer

    -- A LANDING SOMEBODY ASKED FOR OUTRANKS THE BOOT'S.
  , keyedAt shell ("?q=ref%3Ar1&crumbs="
                    <> bootedSels) 500 "a pop out of a booted trail still lands on the remembered row"
      "Backspace" "" $
        \answer -> do
          assertEqual "the row the drill was launched from" (Just "r3")
            =<< maybeTextAt "selected" answer
          urlIs "over the crumb's own query, which is what was applied" "?q=" answer

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

    -- That branch always agrees with the renderer's own keeping, so what this pins is the outcome.
  , keyed shell "archiving the last row lands point on the new last"
      "n n d d" "unserved:r3 frame:upsert=r3 wait:300" $ \answer -> do
        assertEqual "the last row went" [("archive", ["r3"])] =<< postedOf answer
        assertEqual "and point is on the one above it"
                    (Just "r2") =<< maybeTextAt "selected" answer

    -- THE CASE THE RENDERER'S OWN KEEPING GETS WRONG, and the reason the anchor is taken at fire time.
  , keyed shell "the anchor is the next surviving row, not the place point stood"
      "" ("rows:6 press:d press:n press:n press:n press:d press:D"
           <> " unserved:r1,r4 frame:upsert=r1 frame:upsert=r4 wait:300") $ \answer -> do
        assertEqual "both flagged rows, in one request"
                    [("archive", ["r1", "r4"])] =<< postedOf answer
        assertEqual "and the flags are spent" [] =<< textsAt "flagged" answer
        assertEqual "the row under the one that went, not the one two below it"
                    (Just "r5") =<< maybeTextAt "selected" answer

  , keyed shell "a set archived from a surviving row leaves point on that row"
      "" ("rows:5 press:n press:d press:n press:n press:d press:p press:D"
           <> " unserved:r2,r4 frame:upsert=r2 frame:upsert=r4 wait:300") $
        assertEqual "the row point was on is still under it"
                    (Just "r3") <=< maybeTextAt "selected"

    -- And no anchor is left ARMED behind it: an anchor belongs to the archive that took point's row.
  , keyedAt shell "?q=" 500 "and arms nothing for a later removal to land on"
      "" ("rows:6 press:d press:n press:n press:n press:d press:p press:D"
           <> " frame:delete=r1,r4 frame:delete=r3") $
        assertEqual "the row that took r3's place, not the archive's own anchor"
                    (Just "r6") <=< maybeTextAt "selected"

  , keyed shell "archiving every row leaves nothing selected"
      "d n d n d" ("press:d unserved:r1,r2,r3"
                    <> " frame:upsert=r1 frame:upsert=r2 frame:upsert=r3 wait:300") $ \answer -> do
        assertEqual "all three went" [("archive", ["r1", "r2", "r3"])]
          =<< postedOf answer
        assertEqual "and there is no row to be on" Nothing
          =<< maybeTextAt "selected" answer

    -- THE CARVE: a refetch the watch caused lands nothing of its own.
  , keyed shell "a watch refetch under a filter leaves point where it was"
      "n n" "frame:upsert=r1 wait:300" $ \answer -> do
        assertEqual "the frame was re-asked for" 3 . length =<< listAt "paints" answer
        assertEqual "and point did not move for it"
                    (Just "r3") =<< maybeTextAt "selected" answer

  , keyedAt shell "?q=" 500 "a refused archive arms no landing"
      "" "refuse press:d press:n press:d press:p press:D frame:delete=r1" $
        assertEqual "the row that took r1's place, not the anchor's r3"
                    (Just "r2") <=< maybeTextAt "selected"

    -- THE ANCHOR ITSELF VANISHING between the fire and the landing, which is what the remembered PLACE is for.
  , keyed shell "an anchor the view lost falls back to the place it would have had"
      "" ("rows:4 press:n press:d press:d unserved:r2,r3"
           <> " frame:upsert=r2 wait:300") $ \answer -> do
        assertEqual "the row point was on" [("archive", ["r2"])] =<< postedOf answer
        assertEqual "the place, since the row it named is gone too"
                    (Just "r4") =<< maybeTextAt "selected" answer

    -- An archive is an UPSERT on the wire, so an unfiltered client keeps the row it just archived.
  , keyedAt shell "?q=" 500 "an archived row an unfiltered client keeps does not move point"
      "n d d" "frame:upsert=r2" $ \answer -> do
        assertEqual "the row was spliced back in" ["upsert r2"]
          =<< textsAt "spliced" answer
        assertEqual "and point is still on it" (Just "r2")
          =<< maybeTextAt "selected" answer

    -- The splice door SPENDS the anchor rather than landing it, so it describes ONE watch step.
  , keyedAt shell "?q=" 500 "and its frames spend the anchor rather than landing it"
      "" ("rows:6 press:d press:n press:n press:n press:d press:D"
           <> " frame:upsert=r1 frame:upsert=r4 frame:delete=r1,r4") $ \answer -> do
        assertEqual "the frames the archive itself caused, then the removals"
                    [ "upsert r1", "upsert r4", "delete r1", "delete r4" ]
                    =<< textsAt "spliced" answer
        assertEqual "the renderer's place, the anchor having been spent"
                    (Just "r6") =<< maybeTextAt "selected" answer

  , keyed shell "an applied view lands by its own rule after an anchor did not"
      "n d d" "unserved:r2 frame:upsert=r2 wait:300 press:g" $
        assertEqual "g kept the row it was on" (Just "r3")
          <=< maybeTextAt "selected"

    -- An anchor belongs to the VIEW it was taken in, and a mount thrown away takes it with it.
  , keyedAt shell "?q=" 500 "a remount drops an anchor the archive never spent"
      "n d d" "press:g frame:delete=r2 wait:300" $
        assertEqual "where g landed it, never where the old view's anchor pointed"
                    (Just "r3") <=< maybeTextAt "selected"

    -- `visible()' is ONE PAGE, so the question is only answerable about the page the anchor was taken on.
  , keyed shell "an anchor is not landed on a page it was not taken on"
      "" ("rows:6 paged:3 press:n press:n press:d press:d press:] press:n"
           <> " unserved:r3 frame:upsert=r3 wait:300") $ \answer -> do
        assertEqual "the row point was on" [("archive", ["r3"])] =<< postedOf answer
        assertEqual "still on the page it walked to" 2 =<< intAt "page" answer
        assertEqual "and on the row it walked to, not the other page's anchor"
                    (Just "r5") =<< maybeTextAt "selected" answer

  , keyed shell "a reconnect's repaint lands the anchor too"
      "" ("rows:6 press:d press:n press:n press:n press:d press:D"
           <> " unserved:r1,r4 close:resync") $
        assertEqual "the next surviving row, not the renderer's place"
                    (Just "r5") <=< maybeTextAt "selected"

  , keyedAt shell "?q=" 500 "and so does a commit, which replaces the view without a remount"
      "n d d" "press:f press:^ frame:delete=r2 wait:300" $
        assertEqual "where the commit landed it, not the old view's anchor"
                    (Just "r1") <=< maybeTextAt "selected"
  ]

-- | The two structured commands as keys: which rows each names, what the palette commits, what the pill says.
commandKeySpec :: IO T.Text -> TestTree
commandKeySpec shell = testGroup "Shell commands"
  [ -- ORG'S PRIORITY RING, pressed.  Up runs `none → C → B → A → none' and down
    -- the reverse, and the WRAP IS THROUGH NONE, which makes the key that sets a priority the key that takes it off.
    testCase "S-up cycles the priority, and wraps through none" $ do
      bootOf shell "" 500 "S-ArrowUp" "" $ \answer -> do
        assertEqual "an entry with none takes the lowest"
                    [("set-priority", ["r1"])] =<< postedOf answer
        assertEqual "which is C" [Just "C"] =<< prioritiesOf answer
        echoIs "and the pill names the command and the landing"
          "S-<up> → priority-up ([#C] · 1)" answer
      onTable shell "priorities:C press:S-ArrowUp" $
        assertEqual "C climbs to B" [Just "B"] <=< prioritiesOf
      onTable shell "priorities:B press:S-ArrowUp" $
        assertEqual "and B to A" [Just "A"] <=< prioritiesOf
      onTable shell "priorities:A press:S-ArrowUp" $ \answer -> do
        assertEqual "and A wraps to none" [Nothing] =<< prioritiesOf answer
        echoIs "which the pill spells as the meta it is"
          "S-<up> → priority-up (*empty* · 1)" answer

  , testCase "and S-down runs the same ring the other way" $ do
      bootOf shell "" 500 "S-ArrowDown" "" $
        assertEqual "none wraps to the highest" [Just "A"] <=< prioritiesOf
      onTable shell "priorities:A press:S-ArrowDown" $
        assertEqual "A falls to B" [Just "B"] <=< prioritiesOf
      onTable shell "priorities:B press:S-ArrowDown" $
        assertEqual "and B to C" [Just "C"] <=< prioritiesOf
      onTable shell "priorities:C press:S-ArrowDown" $
        assertEqual "and C to none" [Nothing] <=< prioritiesOf

    -- EACH ROW CYCLES FROM ITS OWN VALUE, so a MIXED set is one command per LANDING value.
  , keyed shell "a mixed marked set is one command per landing, and stays mixed"
      "" "priorities:A,,C press:m press:m press:m press:S-ArrowUp" $ \answer -> do
        assertEqual "three rows, three landings"
                    [ ("set-priority", ["r1"]), ("set-priority", ["r2"])
                    , ("set-priority", ["r3"]) ] =<< postedOf answer
        assertEqual "A wrapped, none climbed to C, C climbed to B"
                    [Nothing, Just "C", Just "B"] =<< prioritiesOf answer

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

    -- The FLAGGED set is what `D' runs over: letting the archive key inherit a mark would make every mark a loaded gun.
  , testCase "D archives the flagged set, and leaves the marks where they are" $
      bootOf shell "" 500 "m m d" "press:D" $ \answer -> do
        assertEqual "the flagged row, and only it"
                    [("archive", ["r3"])] =<< postedOf answer
        echoIs "named as the set it was"
          "D → org-glance-overview:delete (archived · 1 flagged)" answer
        assertEqual "the marks are untouched" ["r1", "r2"] =<< textsAt "marked" answer

    -- The flags are spent: the renderer keeps a flag whose row a filter is hiding.
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

  , keyed shell "set-state still runs over the marked set"
      "m m d" "press:C-c press:C-t press:t" $ \answer -> do
        assertEqual "the marked pair, and not the flagged row"
                    [("set-state", ["r1", "r2"])] =<< postedOf answer
        assertEqual "and the flag is still on, unspent" ["r3"]
          =<< textsAt "flagged" answer

  , keyed shell "a server that refuses is counted out and logged"
      "" "refuse press:D" $ \answer -> do
        assertEqual "the command still went" 1 . length =<< postedOf answer
        -- The set name gives way to the bare count: "row" over zero rows would read as a write that landed.
        echoIs "nothing landed" "D → org-glance-overview:delete (archived · 0)" answer

    -- The letter is the whole gesture: the palette IS the confirmation, so there is no RET behind it.
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

    -- Completing a bound sequence outranks RESERVED, and the dispatch claiming BOTH chords is what says so.
  , keyed shell "the completing chord is claimed, reserved or not" "C-c C-t" "" $ \answer -> do
        assertEqual "the palette is up" "on" =<< textAt "prompt" answer
        assertEqual "neither chord was left to the browser"
                    ["C-c", "C-t"] =<< textsAt "prevented" answer

  , keyed shell "RET commits nothing in letter mode" "C-c C-t" "press:Enter" $ \answer -> do
        assertEqual "no command went" [] =<< postedOf answer
        assertEqual "and the palette is still up" "on" =<< textAt "prompt" answer

    -- This listener sits BEHIND the dispatch, so the press that opened the overlay arrives in it next.
  , testCase "the press that raises the palette is not a key in it" $ do
      onTable shell "press:t" $ \answer -> do
        assertEqual "the first press only opened it" [] =<< postedOf answer
        assertEqual "and it is up" "on" =<< textAt "prompt" answer
      onTable shell "press:t press:t" $ \answer -> do
        assertEqual "the second is the letter" [("set-state", ["r1"])]
          =<< postedOf answer
        assertEqual "as TODO" [Just "TODO"] =<< keywordsOf answer

    -- The `ONCE' rule is the palette's: a HELD `t' would open and then commit through what it opened.
  , keyed shell "a held t opens the palette and stops there"
      "" "press:t repeat:t repeat:t" $ \answer -> do
        assertEqual "nothing was written" [] =<< postedOf answer
        assertEqual "and the palette is waiting for a real press" "on"
          =<< textAt "prompt" answer

    -- While the palette is up every `table' row is dead; the gating is `typing()', with no field focused.
  , keyed shell "the table's own letters are the palette's while it is up"
      "C-c C-t" "press:n press:d" $ \answer -> do
        assertEqual "the cursor never moved" 0 =<< intAt "cursor" answer
        assertEqual "nothing was flagged" [] =<< textsAt "flagged" answer
        assertEqual "and d set a state" [("set-state", ["r1"])] =<< postedOf answer
        assertEqual "the one it names" [Just "DONE"] =<< keywordsOf answer

    -- `*empty*' answers to DEL and claims no letter, so the a-z pool is the keywords'.
  , keyed shell "the meta entry clears the keyword rather than setting one"
      "C-c C-t" "press:Backspace" $ \answer -> do
        assertEqual "a null keyword" [Nothing] =<< keywordsOf answer
        echoIs "and the pill says so" "C-c C-t → org-glance-overview:todo (*empty* · 1)" answer

  , keyed shell "/ falls back to typing, and RET takes what is left"
      "C-c C-t" "press:/ type:done press:Enter" $ \answer -> do
        assertEqual "the narrowed choice" [Just "DONE"] =<< keywordsOf answer
        echoIs "the pill" "C-c C-t → org-glance-overview:todo (DONE · 1)" answer

    -- C-n is a reserved chord the map never claims; the palette claims it while its own field has focus.
  , testCase "C-n walks the fallback list, and the arrows do the same" $
      mapM_ (\key -> bootOf shell "" 500 "C-c C-t" ("press:/ press:" <> key)
               (assertEqual (T.unpack key <> ": stepped to the second entry")
                            [Just "DONE"] <=< keywordsOf))
            ["C-n press:Enter", "ArrowDown press:Enter"]

    -- `/' is entered and never left, so ESC is the one door out of either mode.
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

-- | @:@ — the manage-tags popup, the page's FOURTH table-view mount and the only MUTABLE one.
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
        assertEqual "one list, built on the first raise" 1 =<< intAt "tmounts" answer
        assertEqual "under the headers the shell declared" ["Tag", "On", "Rows"]
          =<< textsAt "tcols" answer

    -- The tree's count is the server's and the one number no arithmetic over the rows in hand recovers.
  , atBoot tagged "a row is the tag, its coverage and the tree's count" $ \answer -> do
        assertEqual "one row per tag" [["web", "all", "40"]] =<< pairsAt "ttags" answer
        assertEqual "the cursor lands on the first" 0 =<< intAt "tat" answer
        assertEqual "and the foot names every key that works"
                    "RET renames · d flags · D removes · + adds · / narrows · ESC leaves"
          =<< textAt "tfoot" answer

    -- MUTABLE, and the flag gesture says so: no mark column and no page.
  , atBoot tagged "the list is mutable: it flags, and says which keys do it" $ \answer -> do
        assertEqual "nothing flagged before a key says so" []
          =<< textsAt "tflagged" answer
        assertEqual "and the flag's own hint names the two keys that answer it"
                    "d/D remove · u unflag" =<< textAt "tflagHelp" answer

  , keyed shell "over a marked set it names the whole set, in one request"
      "m m :" "" $ \answer -> do
        assertEqual "the title counts them" "tags · 2 rows" =<< textAt "thead" answer
        assertEqual "and the resolution is one request"
                    ["/tags?ids=r1&ids=r2"] =<< textsAt "tagged" answer

    -- COVERAGE: `all' where the set is level, `k/n' where it is not.
  , keyed shell "a tag part of the set carries says so in its own cell"
      "" "partly press:m press:m press:m press::" $
        assertEqual "two of the three rows" [["web", "2/3", "40"]] <=< pairsAt "ttags"

  , testCase "n and p walk it, in both spellings" $ do
      let two = "press:m press:m press:: press:+ type:work press:Enter"
      onTable shell two $ \answer -> do
        assertEqual "two tags to walk"
                    [["web", "all", "40"], ["work", "all", "9"]] =<< pairsAt "ttags" answer
        assertEqual "the cursor lands on the one just written" 1 =<< intAt "tat" answer
      onTable shell (two <> " press:p") $
        assertEqual "up one" 0 <=< intAt "tat"
      onTable shell (two <> " press:k press:j") $
        assertEqual "and back" 1 <=< intAt "tat"

  , keyed shell "d flags the tag at point and writes nothing" ":" "press:d" $ \answer -> do
        assertEqual "flagged" ["web"] =<< textsAt "tflagged" answer
        assertEqual "nothing written" [] =<< postedOf answer
        echoIs "and the echo says what a second press does"
          "d → tag-flag (d again removes)" answer

  , keyed shell "a second d removes it from every row carrying it"
      "m m m :" "press:d press:d" $ \answer -> do
        assertEqual "over all three" [("remove-tag", ["r1", "r2", "r3"])]
          =<< postedOf answer
        -- Mounted once and kept: a write is a `setRows' over the same instance.
        assertEqual "still one list" 1 =<< intAt "tmounts" answer
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

  , keyed shell "D over an archived row asks for the word before it deletes"
      "" "archived:r1 press:D" $ \answer -> do
        assertEqual "nothing posted on the press alone" [] =<< namesOf answer
        assertEqual "the question is up" "on" =<< textAt "prompt" answer
        assertContains "naming what it will do" "delete" =<< textAt "phead" answer
        -- THE WORD IS SHOWN UPPERCASE, a wall reading as one, and MATCHED FOLDED.
        assertContains "and the word to type is spelled as the wall it is"
                       "type DELETE and RET" =<< textAt "pfoot" answer

  , keyed shell "and either spelling of it gets past"
      "" "archived:r1 press:D type:DELETE press:Enter" $ \answer ->
        assertEqual "the shouted one" [("delete", ["r1"])] =<< postedOf answer

  , keyed shell "and the word is what sends it"
      "" "archived:r1 press:D type:delete press:Enter" $ \answer -> do
        assertEqual "one delete, over the row at point"
                    [("delete", ["r1"])] =<< postedOf answer
        assertEqual "and the question is gone" "" =<< textAt "prompt" answer

  , keyed shell "and anything else writes nothing"
      "" "archived:r1 press:D type:yes press:Enter" $ \answer -> do
        assertEqual "nothing posted" [] =<< namesOf answer
        echoIs "and it says so" "D → org-glance-overview:delete (not deleted)" answer

    -- A MIXED SET ARCHIVES, one step for the whole set; FLAGGED, never marked.
  , keyed shell "a set only partly archived is archived, not deleted"
      "" "archived:r1 press:d press:n press:d press:D" $ \answer -> do
        assertEqual "archive, over both" [("archive", ["r1", "r2"])] =<< postedOf answer
        assertEqual "and nothing was asked" "" =<< textAt "prompt" answer

  , keyed shell "a wholly archived set is asked for once"
      "" "archived:r1,r2 press:d press:n press:d press:D type:delete press:Enter" $ \answer -> do
        assertEqual "one delete over both" [("delete", ["r1", "r2"])] =<< postedOf answer

    -- `x' IS `dired-do-flagged-delete': the FLAGS alone, never the row at point, and it asks first.
  , keyed shell "x takes the flags and asks, naming the count"
      "" "press:d press:n press:d press:x" $ \answer -> do
        assertEqual "nothing posted on the press alone" [] =<< namesOf answer
        assertEqual "the question is up" "on" =<< textAt "prompt" answer
        assertEqual "naming the act and how many" "archive · 2 flagged"
          =<< textAt "phead" answer

  , keyed shell "and the word sends it over exactly those rows"
      "" "press:d press:n press:d press:x type:yes press:Enter" $ \answer -> do
        assertEqual "one archive, over both flagged rows"
                    [("archive", ["r1", "r2"])] =<< postedOf answer
        assertEqual "and the flags are spent" [] =<< textsAt "flagged" answer

  , keyed shell "and anything else leaves them standing"
      "" "press:d press:n press:d press:x type:no press:Enter" $ \answer -> do
        assertEqual "nothing posted" [] =<< namesOf answer
        echoIs "and it says so"
          "x → dired-do-flagged-delete (left standing)" answer
        assertEqual "the flags are where they were" ["r1", "r2"]
          =<< textsAt "flagged" answer

  , keyed shell "x with nothing flagged writes nothing and says dired's words"
      "" "press:x" $ \answer -> do
        assertEqual "nothing posted" [] =<< namesOf answer
        assertEqual "and nothing asked" "" =<< textAt "prompt" answer
        echoIs "" "x → dired-do-flagged-delete (no deletions requested)" answer

    -- ONE QUESTION, WEIGHTED TO THE ACT: a wholly archived set asks for the stronger word instead.
  , keyed shell "x over a wholly archived set asks for the delete word, once"
      "" "archived:r1,r2 press:d press:n press:d press:x" $ \answer -> do
        assertEqual "nothing posted yet" [] =<< namesOf answer
        assertContains "the question is the delete one" "delete"
          =<< textAt "phead" answer
  , keyed shell "and that word deletes, with no second question"
      "" "archived:r1,r2 press:d press:n press:d press:x type:delete press:Enter" $
        \answer -> do
          assertEqual "one delete over both" [("delete", ["r1", "r2"])]
            =<< postedOf answer
          assertEqual "and nothing is left asking" "" =<< textAt "prompt" answer

  , keyed shell "D is the same handler without the flagging press" "m m :" "press:D" $ \answer -> do
        assertEqual "both rows" [("remove-tag", ["r1", "r2"])] =<< postedOf answer
        assertEqual "and the popup stands" "on" =<< textAt "tagpop" answer

    -- A command names ONE tag, so several flags are several commands, each over the rows carrying that tag.
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

  , keyed shell "a held d flags once and never removes"
      ":" "press:d repeat:d repeat:d" $ \answer -> do
        assertEqual "nothing written" [] =<< postedOf answer
        assertEqual "and the flag is still just a flag" ["web"]
          =<< textsAt "tflagged" answer

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

    -- Without the claimed-key guard the RET that adds would open the rename over the tag it had just written.
  , keyed shell "the RET that adds does not open the rename behind it"
      ":" "press:+ type:work press:Enter" $ \answer -> do
        assertEqual "the tag was added" [("add-tag", ["r1"])] =<< postedOf answer
        assertEqual "and no rename opened" False =<< boolAt "trename" answer

    -- THE TYPED VALUE IS ALWAYS AN OFFER where the vocabulary is open: it is
    -- DRAWN, leading, hinted as itself, so RET commits it by taking the entry
    -- point rests on rather than through a door of its own — AGENTS.hs.
  , keyed shell "the typed line is drawn as an entry of its own, leading"
      ":" "press:+ type:brandnew" $ \answer -> do
        assertEqual "it alone, no tag folding to it"
          [ ("pe pat", "", ["brandnew"], []) ] =<< paletteOf answer
        assertEqual "wearing the hint that says what it is"
                    [("brandnew", "new")] =<< paletteHints answer

  , keyed shell "and a tag the tree has never held is committable all the same"
      ":" "press:+ type:brandnew press:Enter" $ \answer -> do
        assertEqual "the typed line, folded" ["brandnew"] =<< tagsPosted answer
        assertEqual "over the row at point" [("add-tag", ["r1"])] =<< postedOf answer
        assertEqual "and it joins the list under a count of its own"
                    [["web", "all", "40"], ["brandnew", "all", "1"]]
          =<< pairsAt "ttags" answer

    -- THE BUG THE LAW EXISTS FOR: the field held ONE match, point sat on it, and
    -- RET wrote the tree's word over the reader's — `shelf' typed against a tree
    -- holding `bookshelf' added `bookshelf'.
  , keyed shell "the typed line leads the tag it reads as a prefix of"
      ":" "press:+ type:boo" $ \answer -> do
        assertEqual "what was typed first, the tag holding it under"
          [ ("pe pat", "", ["boo"],  [])
          , ("pe",     "", ["book"], []) ] =<< paletteOf answer
        assertEqual "and only the typed one calls itself new"
                    [("boo", "new"), ("book", "")] =<< paletteHints answer

  , keyed shell "so RET writes that word, the tag beside it untouched"
      ":" "press:+ type:boo press:Enter" $ \answer -> do
        assertEqual "over the row at point" [("add-tag", ["r1"])] =<< postedOf answer
        assertEqual "carrying what was typed" ["boo"] =<< tagsPosted answer
        assertEqual "and the popup lists it beside the row's own"
                    [["web", "all", "40"], ["boo", "all", "1"]]
          =<< pairsAt "ttags" answer

  , keyed shell "and the match is one C-n away, which is how it is taken"
      ":" "press:+ type:boo press:C-n press:Enter" $
        assertEqual "the tag walked onto" ["book"] <=< tagsPosted

    -- ONE ENTRY AND NEVER TWO: a typed value folding to an entry coincides with it.
  , keyed shell "a typed value that folds to a tag coincides with it"
      ":" "press:+ type:BOOK" $ \answer -> do
        assertEqual "the tag alone, drawn once" [ ("pe pat", "", ["book"], []) ]
          =<< paletteOf answer
        assertEqual "wearing its own hint rather than the new one"
                    [("book", "")] =<< paletteHints answer

  , keyed shell "and several matches keep the typed line at their head"
      ":" "press:+ type:o" $
        assertEqual "the literal, then every tag holding the letter"
          [("o", "new"), ("book", ""), ("work", "")] <=< paletteHints

  , keyed shell "typing a tag every row has writes nothing and says so"
      "m m m :" "press:+ type:web press:Enter" $ \answer -> do
        assertEqual "no command went" [] =<< postedOf answer
        echoIs "and the pill says why"
          ": → org-agenda-set-tags (:web: is on every row already)" answer

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

    -- The typed name is folded, because presence is, and a name that does not move costs no round trip.
  , testCase "a rename to the same name writes nothing" $
      mapM_ (\typed -> bootOf shell "" 500 ":" ("press:Enter tname:" <> typed
                                                <> " press:Enter") $ \answer -> do
               assertEqual (T.unpack typed <> ": no command went") []
                 =<< postedOf answer
               assertEqual "and the pill says so"
                           ": → org-agenda-set-tags (unchanged)" =<< textAt "echo" answer)
            ["web", "WEB", ""]

  , testCase "ESC leaves the rename a rung at a time" $ do
      bootOf shell "" 500 ":" "press:Enter tname:code press:Escape" $ \answer -> do
        assertEqual "nothing was written" [] =<< postedOf answer
        assertEqual "the overlay is gone" False =<< boolAt "trename" answer
        assertEqual "the popup stands" "on" =<< textAt "tagpop" answer
        assertEqual "and the tag is the tag it was" [["web", "all", "40"]]
          =<< pairsAt "ttags" answer
      bootOf shell "" 500 ":" "press:Enter press:Escape press:Escape" $
        assertEqual "a second ESC closes it" "" <=< textAt "tagpop"

    -- THE TAG A COMMIT RENAMES IS THE TAG THE OVERLAY OPENED OVER: no key moves the cursor under a field, a MOUSE CLICK does.
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

    -- THE LETTERS ARE GONE: a tag list is read rather than committed from memory.
  , keyed shell "a letter commits nothing, the which-key list having gone"
      ":" "press:w press:a press:b" $ \answer -> do
        assertEqual "no command went" [] =<< postedOf answer
        assertEqual "no value palette either" "" =<< textAt "prompt" answer
        assertEqual "and the popup is still up" "on" =<< textAt "tagpop" answer

  , keyed shell "the table's own keys are inert while the popup is up"
      ":" "press:m press:M press:U press:t" $ \answer -> do
        assertEqual "nothing was marked" [] =<< textsAt "marked" answer
        assertEqual "nothing was flagged in the table" [] =<< textsAt "flagged" answer
        assertEqual "no command was posted" [] =<< namesOf answer
        assertEqual "and no state palette went up" "" =<< textAt "prompt" answer

    -- The popup's own keys are dead under its `+' field, which is the listener's `prompting' guard.
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

    -- THE LIST REFRESHES FROM THE ANSWER: `/command' does not write the store, so `/tags' would report the pre-write files.
  , keyed shell "the list is what landed, and the store is not asked twice"
      "m m :" "press:d press:d" $ \answer -> do
        assertEqual "the one resolution, and no second" ["/tags?ids=r1&ids=r2"]
          =<< textsAt "tagged" answer
        assertEqual "and the tag is gone from a list nobody re-read" []
          =<< pairsAt "ttags" answer
  ]

renamesPosted :: Value -> IO [(T.Text, T.Text)]
renamesPosted = traverse one <=< argsOf
  where one v = (,) <$> textAt "from" v <*> textAt "to" v

tagsPosted :: Value -> IO [T.Text]
tagsPosted = traverse (textAt "tag") <=< argsOf

-- | The two keys that collect a LINE rather than pick from a list: @+@ and the reschedule chords.
promptKeySpec :: IO T.Text -> TestTree
promptKeySpec shell = testGroup "Shell capture and reschedule"
    -- `+' RAISES ONE FORM, whole: tag field, the template's grown fields, the line.
  [ keyed shell "+ is one form, and an empty tag is the inbox"
      "+" "press:Enter ktext:milk press:Enter" $ \answer -> do
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
        assertEqual "the form is down on the 200" "" =<< textAt "capture" answer

    -- This page holds no template grammar: what it asks is what `/capture?tag=' said to ask.
  , keyed shell "a tag's template grows its fields in place"
      "+" "ktag:book press:Enter kf:Herbert press:Enter ktext:Dune press:Enter"
      $ \answer -> do
        assertEqual "the tag was resolved when it settled"
                    ["/capture", "/capture?tag=book"] =<< textsAt "capturing" answer
        assertEqual "one capture" ["capture"] =<< namesOf answer
        assertEqual "the line as typed" ["Dune"] =<< capturedOf answer
        assertEqual "under the tag it was filed with" [Just "book"] =<< taggedOf answer
        assertEqual "and the template's ask answered"
                    [Just "Herbert"] =<< answeredOf "Author" answer
        echoIs "the pill names the tag rather than a file"
          "+ → org-glance-overview:capture (captured · :book:)" answer

    -- THE VIEW'S OWN TAG IS THE DEFAULT, and a SUGGESTION: backspacing to the inbox is one key.
  , keyedAt shell "?q=tag%3Abook" 500 "the capture form opens on the filter's tag"
      "" "press:+" $ \answer -> do
        assertEqual "the field carries it" "book" =<< textAt "ktag" answer
        assertEqual "and it was resolved without a keystroke"
                    ["/capture", "/capture?tag=book"] =<< textsAt "capturing" answer

  , keyedAt shell "?q=-tag%3Abook%20tag%3Aa%7Cb%20tag%3A*archive*" 500
      "a negated, alternated or starred tag seeds nothing"
      "" "press:+" $ \answer -> do
        assertEqual "the field is empty" "" =<< textAt "ktag" answer
        assertEqual "and nothing was resolved" ["/capture"]
          =<< textsAt "capturing" answer

  , keyed shell "a tag with no template goes straight to the line"
      "+" "ktag:web press:Enter ktext:milk press:Enter" $ \answer -> do
        assertEqual "resolved all the same" ["/capture", "/capture?tag=web"]
          =<< textsAt "capturing" answer
        assertEqual "the line as typed" ["milk"] =<< capturedOf answer
        assertEqual "under the tag" [Just "web"] =<< taggedOf answer

    -- ESC ANYWHERE CLOSES THE FORM: one surface, one door out, the keymap's own `cancel'.
  , keyed shell "ESC at the tag field writes nothing"
      "+" "press:Escape" $ \answer -> do
        assertEqual "no command went" [] =<< namesOf answer
        assertEqual "the form is down" "" =<< textAt "capture" answer

  , keyed shell "ESC at a grown field writes nothing"
      "+" "ktag:book press:Enter kf:Herbert press:Escape" $ \answer -> do
        assertEqual "the tag was resolved" ["/capture", "/capture?tag=book"]
          =<< textsAt "capturing" answer
        assertEqual "no command went" [] =<< namesOf answer
        assertEqual "the form is down" "" =<< textAt "capture" answer

  , keyed shell "and ESC at the line leaves it having written nothing"
      "+" "press:Enter ktext:milk press:Escape" $ \answer -> do
        assertEqual "no command went" [] =<< namesOf answer
        assertEqual "the form is down" "" =<< textAt "capture" answer

    -- The form STAYS on a refusal, everything typed kept; the 200 alone closes it.
  , keyed shell "an empty line captures nothing and says so"
      "+" "press:Enter press:Enter" $ \answer -> do
        assertEqual "no command went" [] =<< namesOf answer
        echoIs "the pill says why" "+ → org-glance-overview:capture (nothing to capture)" answer
        assertEqual "and the form is still up" "on" =<< textAt "capture" answer

  , keyed shell "a refused capture is one cmd error line, and the form stays"
      "" "refuse press:+ press:Enter ktext:milk press:Enter" $ \answer -> do
        assertEqual "the command still went" ["capture"] =<< namesOf answer
        assertEqual "and the log carries the server's own words"
                    (Just "capture failed: inbox.org changed on disk")
          =<< lastLog answer
        assertEqual "everything typed is still there" "on" =<< textAt "capture" answer

  , keyed shell "the captured row is where point lands when it arrives"
      "+" "press:Enter ktext:milk press:Enter frame:upsert=r3 wait:300" $ \answer ->
        assertEqual "point is on the row the capture made" (Just "r3")
          =<< maybeTextAt "selected" answer

    -- THE WHOLE FORM FOR A TAGGED CAPTURE: the blob sits under directories fsnotify never entered, so the nudge is what delivers it.
  , keyed shell "a tagged capture lands point on the blob when the watch delivers it"
      "+" "ktag:book press:Enter kf:Herbert press:Enter ktext:Dune press:Enter\
          \ frame:upsert=r3 wait:300" $ \answer -> do
        assertEqual "the tag was resolved off the server"
                    ["/capture", "/capture?tag=book"] =<< textsAt "capturing" answer
        assertEqual "one capture, under that tag" [Just "book"] =<< taggedOf answer
        assertEqual "point left the row the boot landed on" (Just "r3")
          =<< maybeTextAt "selected" answer
        assertEqual "which is the third row" 2 =<< intAt "cursor" answer
        assertEqual "nothing was spliced under the filter" [] =<< textsAt "spliced" answer

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

  , keyed shell "an empty line clears the entry" "C-c C-d" "press:Enter" $ \answer -> do
        assertEqual "a null date" [("DEADLINE", Nothing)] =<< plannedOf answer
        echoIs "the pill says which"
          "C-c C-d → org-glance-overview:deadline (cleared · 1)" answer
        assertEqual "and so does the log"
                    (Just "headline \"one\" deadline cleared") =<< lastLog answer

  , keyed shell "over a marked set it names the whole set"
      "m m C-c C-s" "type:today press:Enter" $ \answer -> do
        assertEqual "the marked pair" [("set-planning", ["r1", "r2"])] =<< postedOf answer
        assertEqual "and the title counts them" "scheduled · 2 rows"
          =<< textAt "phead" answer
  ]

namesOf :: Value -> IO [T.Text]
namesOf answer = traverse (textAt "name") =<< listAt "commands" answer

argsOf :: Value -> IO [Value]
argsOf answer = traverse (field "args") =<< listAt "commands" answer

capturedOf :: Value -> IO [T.Text]
capturedOf = traverse (textAt "text") <=< argsOf

-- | The tag each posted @capture@ filed under, 'Nothing' for the inbox — a SPARSE field, absent rather than null.
taggedOf :: Value -> IO [Maybe T.Text]
taggedOf = traverse (sparseTextAt "tag") <=< argsOf

answeredOf :: T.Text -> Value -> IO [Maybe T.Text]
answeredOf name = traverse one <=< argsOf
  where one v = maybe (pure Nothing) (sparseTextAt name) =<< sparseAt "fields" v

plannedOf :: Value -> IO [(T.Text, Maybe T.Text)]
plannedOf = traverse one <=< argsOf
  where one v = (,) <$> textAt "keyword" v <*> maybeTextAt "date" v

lastLog :: Value -> IO (Maybe T.Text)
lastLog answer = fmap (message . cut) . listToMaybe . reverse <$> logOf answer

-- | @o@: what the row points at, followed — the ANSWER decides the gesture.
openKeySpec :: IO T.Text -> TestTree
openKeySpec shell =
  overBoot shell "o" "" $ \opened ->
  overBoot shell "o" "press:e lurl:https://new.example press:Enter" $ \committed ->
  testGroup "Shell open"
  [ atBoot opened "o asks about the row at point" $
        assertEqual "one request, naming the row" ["/links?id=r1"] <=< textsAt "linked"

  , keyed shell "! is the same command, and reaches it the same way" "!" "" $ \answer -> do
        assertEqual "the same request" ["/links?id=r1"] =<< textsAt "linked" answer
        -- Raising a palette is not a landing, so the pill still carries what `run' says of the row.
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

    -- Several is the POPUP: a list of links is a list of RECORDS, which a which-key letter is the wrong shape for.
  , atBoot opened "several raise the popup, which is a table-view mount" $ \answer -> do
        assertEqual "raised" "on" =<< textAt "popup" answer
        assertEqual "no value palette went up" "" =<< textAt "prompt" answer
        assertEqual "titled by the count" "open · 3 links" =<< textAt "lhead" answer
        assertEqual "one list, built on the first raise" 1
          =<< intAt "lmounts" answer
        assertEqual "under the headers the shell declared" ["Type", "Title", "Target"]
          =<< textsAt "lcols" answer

  , atBoot opened "the rows are the answer, type and all" $ \answer -> do
        assertEqual "one row per link"
          [ ["https", "First reference", "https://one.example/a"]
          , ["https", "Second reference", "https://two.example/b"]
          , ["mailto", "mailto:t@example.org", "mailto:t@example.org"] ]
          =<< pairsAt "llinks" answer
        assertEqual "the cursor lands on the first" 0 =<< intAt "lat" answer
        assertEqual "and the foot names the four keys that work"
                    "RET opens it · e edits · / narrows · ESC leaves" =<< textAt "lfoot" answer

    -- READ-ONLY: nothing here writes, so there is no flag hint to draw.
  , atBoot opened "the list is read-only: it names no flag keys" $ \answer ->
        assertEqual "no flag hint under it" "" =<< textAt "lflagHelp" answer

    -- The whole point of `typing()' counting the popup: every `table' row is dead under it.
  , keyed shell "the write keys are inert while the popup is up"
      "o" "press:d press:D press:m press:M press:u press:U" $
        \answer -> do
          assertEqual "nothing was flagged, here or in the table" []
            =<< textsAt "lflagged" answer
          assertEqual "nor in the table under it" [] =<< textsAt "flagged" answer
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

    -- RET IS THE POPUP'S OWN NAME SPOKEN: the list is `open', so its default
    -- press opens, and `o' stays the same key it is everywhere else.
  , testCase "RET and o both open the link at point and close the popup" $ do
      bootOf shell "" 500 "o" "press:n press:o" $ \answer -> do
        assertEqual "the second one" [("https://two.example/b", "_blank", "noopener")]
          =<< openedOf answer
        assertEqual "the popup is down" "" =<< textAt "popup" answer
        echoIs "the pill names it by its description"
          "o → org-glance-overview:open (Second reference)" answer
      bootOf shell "" 500 "o" "press:n press:Enter" $ \answer -> do
        assertEqual "RET opens the very same tab"
          [("https://two.example/b", "_blank", "noopener")] =<< openedOf answer
        assertEqual "and takes the popup down with it" "" =<< textAt "popup" answer
        assertEqual "with no edit overlay behind it" False =<< boolAt "lopen" answer
        -- THE PILL WEARS THE OPENER'S BINDING: the popup answers under the
        -- binding that raised it, so both keys land the one line.
        echoIs "under the same command"
          "o → org-glance-overview:open (Second reference)" answer

  , keyed shell "ESC leaves it having opened nothing" "o" "press:Escape" $ \answer -> do
        assertEqual "nothing opened" [] =<< openedOf answer
        assertEqual "the popup is down" "" =<< textAt "popup" answer

    -- `e' EDITS the link at point in place, on the property panel's edit model.
  , keyed shell "e opens the link at point over its own two cells"
      "o" "press:e" $ \answer -> do
        assertEqual "the overlay is up" True =<< boolAt "lopen" answer
        assertEqual "holding what the entry calls it" "First reference"
          =<< textAt "ltitle" answer
        assertEqual "and where it points" "https://one.example/a"
          =<< textAt "lurl" answer
        assertEqual "the target takes the focus" "lurl" =<< textAt "focus" answer
        assertEqual "the popup stands under it" "on" =<< textAt "popup" answer
        assertEqual "and nothing is posted by opening one" [] =<< namesOf answer

  , testCase "TAB hops the two fields, and nothing else moves" $ do
      bootOf shell "" 500 "o" "press:e press:Tab" $ \answer -> do
        assertEqual "over to the description" "ltitle" =<< textAt "focus" answer
        assertEqual "the overlay is still open" True =<< boolAt "lopen" answer
      bootOf shell "" 500 "o" "press:e press:Tab press:Tab" $
        assertEqual "and back" "lurl" <=< textAt "focus"

    -- This page holds no bracket grammar and no offsets of its own: it sends back the range it was given.
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

    -- ABSENT IS NOT NULL: the description field opens on what the link SHOWS, which for a desc-less link is its target.
  , atBoot committed "a description nobody moved is not sent at all" $
        \answer -> do
          [cmd] <- listAt "commands" answer
          args <- field "args" cmd
          assertEqual "no desc field" ["span", "target"] . sort =<< fieldsOf args

  , keyed shell "and one the reader emptied is the null that takes it off"
      "o" "press:e ltitle: press:Enter" $ \answer -> do
        [cmd] <- listAt "commands" answer
        args <- field "args" cmd
        assertEqual "a null description" Null =<< field "desc" args
        assertEqual "under the target it already had" "https://one.example/a"
          =<< textAt "target" args

  , keyed shell "a description typed over the old one is sent as it was typed"
      "o" "press:e ltitle:renamed press:Enter" $
        \answer -> do
          [cmd] <- listAt "commands" answer
          args <- field "args" cmd
          assertEqual "the text" "renamed" =<< textAt "desc" args

    -- The popup CLOSES on the press, both outcomes alike: the spans describe a file the write has just moved.
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

  , keyed shell "a link nobody changed costs no write" "o" "press:e press:Enter" $ \answer -> do
        assertEqual "nothing posted" [] =<< namesOf answer
        assertEqual "the popup is down all the same" "" =<< textAt "popup" answer
        echoIs "and the pill says so" "o → org-glance-overview:open (unchanged)" answer

  , keyed shell "an emptied target is refused here, since a link points somewhere"
      "o" "press:e lurl: press:Enter" $ \answer -> do
        assertEqual "nothing posted" [] =<< namesOf answer
        echoIs "the pill says why"
          "o → org-glance-overview:open (a link points somewhere)" answer

  , keyed shell "ESC over an open link puts it back and leaves the popup standing"
      "o" "press:e lurl:https://new.example press:Escape" $
        \answer -> do
          assertEqual "the overlay is gone" False =<< boolAt "lopen" answer
          assertEqual "the popup is not" "on" =<< textAt "popup" answer
          assertEqual "nothing was posted" [] =<< namesOf answer
          echoIs "and the pill says the link stands"
            "ESC → keyboard-quit (link unchanged)" answer

  , keyed shell "and a second ESC closes the popup" "o" "press:e press:Escape press:Escape" $
        assertEqual "down" "" <=< textAt "popup"

    -- No KEY can move the cursor under an open field, but a MOUSE CLICK can.
  , keyed shell "a click under an open link cannot redirect the write"
      "o" "press:e lurl:https://new.example click:2 press:Enter" $
        \answer -> do
          [cmd] <- listAt "commands" answer
          args <- field "args" cmd
          assertEqual "the span is the one the overlay opened over"
            [10, 48] =<< spanOf args
          assertEqual "and the target is what was typed for it" "https://new.example"
            =<< textAt "target" args

    -- A held key must not be a browser tab per repeat, which is why the command is on the ONCE list.
  , keyed shell "a held o asks once" "o" "repeat:o repeat:o repeat:o" $
        assertEqual "one request" ["/links?id=r1"] <=< textsAt "linked"

  , keyed shell "a refused answer is one cmd error line and no popup"
      "" "refuse press:o" $ \answer -> do
        assertEqual "nothing opened" [] =<< openedOf answer
        assertEqual "no popup" "" =<< textAt "popup" answer
        assertEqual "and the log carries the server's own words"
                    (Just "open failed: no headline with id r1") =<< lastLog answer

    -- A tab can be pointed at http(s) and nothing else, and the TYPE says so — the server's own word.
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

  , keyed shell "an o on a non-http row refuses the same way"
      "o" "press:n press:n press:o" $ \answer -> do
        assertEqual "nothing opened" [] =<< openedOf answer
        assertEqual "the popup is down all the same" "" =<< textAt "popup" answer
        echoIs "the pill says why"
          "o → org-glance-overview:open (link type not implemented)" answer

  , keyed shell "and an http row beside it still opens" "o" "press:o" $
        assertEqual "the first one" [("https://one.example/a", "_blank", "noopener")]
          <=< openedOf

    -- A type this page has never seen is still a fact about the link, so it is drawn uncoloured.
  , keyed shell "every type the server derives reaches the badge cell" "" "everytype press:o" $
        assertEqual "one word per row"
          ["https", "http", "glance", "mailto", "id", "file", "other"]
          . map head <=< pairsAt "llinks"

    -- A MATERIAL TYPE POINTS AT A HEADLINE, a followable one at a place a tab
    -- can go: opening a material link MATERIALIZES that entry, the scheme and
    -- any `?kind=' cut off the target.  Three outcomes, and every row says
    -- which it took.
  , testCase "only the followable ones open a tab, and a material one materializes" $
      forM_ [ ( 0 :: Int, "https://a.example", True, False
              , "link \"https://a.example\" opened" )
            , (1, "http://b.example", True, False, "link \"http://b.example\" opened")
            , (2, "org-glance-visit:XYZ", False, True, "materialized \"XYZ\"")
            , ( 3, "mailto:t@example.org", False, False
              , "link type not implemented: mailto:t@example.org" )
            , (4, "id:99", False, False, "link type not implemented: id:99")
            , ( 5, "file:notes.org", False, False
              , "link type not implemented: file:notes.org" )
            , ( 6, "Some Headline", False, False
              , "link type not implemented: Some Headline" ) ] $
        \(at, target, opens, opensSheet, logged) ->
          bootOf shell "" 500 ""
            ("everytype press:o " <> T.replicate at "press:n " <> "press:o") $ \answer -> do
              assertEqual (T.unpack target <> ": the tab")
                [(target, "_blank", "noopener") | opens] =<< openedOf answer
              assertEqual (T.unpack target <> ": the sheet")
                (if opensSheet then "on" else "") =<< textAt "modal" answer
              assertEqual (T.unpack target <> ": the entry it read")
                ["r1" | opensSheet] =<< textsAt "readAt" answer
              assertEqual (T.unpack target <> ": the log line")
                (Just logged) =<< lastLog answer
  ]

-- | @\/@: the narrow every small list takes — ONE PROGRAM, FOUR MOUNTS.
narrowSpec :: IO T.Text -> TestTree
narrowSpec shell =
  overBoot shell "o" "press:/" $ \opened ->
  overBoot shell "o" "press:/ narrow:second" $ \narrowed ->
  testGroup "Shell narrow"
  [ atBoot opened "/ opens a field over the list and takes the keys" $ \answer -> do
        assertEqual "the link popup's list carries it, holding nothing yet"
                    [["ltable", ""]] =<< pairsAt "narrows" answer
        assertEqual "and it is what the letters go to now"
                    "narrow:ltable" =<< textAt "focus" answer
        assertEqual "nothing is narrowed away by an empty one" 3
          . length =<< pairsAt "llinks" answer
        echoIs "the pill names the table's own command, one list in"
               "/ → filter-rows" answer

    -- SUBSTRING, CASE-FOLDED, over the cells the list DRAWS, and no grammar of any kind.
  , atBoot narrowed "it narrows to the rows the text is in, folded" $ \answer -> do
        assertEqual "the one whose title spells it"
          [["https", "Second reference", "https://two.example/b"]]
          =<< pairsAt "llinks" answer
        assertEqual "and the cursor is on it" 0 =<< intAt "lat" answer

  , keyed shell "the popup's own keys are suspended under the field"
      "o" "press:/ narrow:second press:o press:d press:q" $ \answer -> do
        assertEqual "no tab was opened" [] =<< openedOf answer
        assertEqual "nothing was flagged" [] =<< textsAt "lflagged" answer
        assertEqual "the popup is still up" "on" =<< textAt "popup" answer
        assertEqual "and the narrow is still the reader's"
                    [["ltable", "second"]] =<< pairsAt "narrows" answer

  , keyed shell "RET leaves the field with the narrow standing"
      "o" "press:/ narrow:second press:Enter" $ \answer -> do
        assertEqual "the field has given the keys back" "" =<< textAt "focus" answer
        assertEqual "the rows stay narrowed"
                    [["ltable", "second"]] =<< pairsAt "narrows" answer
        echoIs "and the pill counts what is left"
               "RET → filter-rows (1 of 3)" answer

  , keyed shell "and the keys come back to the rows that survived"
      "o" "press:/ narrow:second press:Enter press:o" $ \answer ->
        assertEqual "`o' opens the match rather than the row `/' was pressed on"
          [("https://two.example/b", "_blank", "noopener")] =<< openedOf answer

    -- ESC IS A LADDER: the narrow is a rung under the popup.
  , keyed shell "ESC clears the narrow and leaves the popup up"
      "o" "press:/ narrow:second press:Escape" $ \answer -> do
        assertEqual "no field is left" [] =<< pairsAt "narrows" answer
        assertEqual "every row is back" 3 . length =<< pairsAt "llinks" answer
        assertEqual "the popup stands" "on" =<< textAt "popup" answer
        echoIs "and the pill says what was cleared"
               "ESC → keyboard-quit (narrow cleared)" answer

  , keyed shell "and the press after it steps out"
      "o" "press:/ narrow:second press:Escape press:Escape" $
        assertEqual "the popup is down" "" <=< textAt "popup"

  , keyed shell "DEL clears the narrow before it closes the popup"
      "o" "press:/ narrow:second press:Enter press:Backspace" $ \answer -> do
        assertEqual "the narrow went" [] =<< pairsAt "narrows" answer
        assertEqual "the popup stands" "on" =<< textAt "popup" answer

    -- Under the FIELD it is the field's own erase, the one place the ladder does not reach.
  , keyed shell "DEL inside the field erases rather than steps out"
      "o" "press:/ narrow:second press:Backspace" $ \answer -> do
        assertEqual "the popup is untouched" "on" =<< textAt "popup" answer
        assertEqual "and the field still holds the keys"
                    "narrow:ltable" =<< textAt "focus" answer

    -- A NARROW BELONGS TO THE QUESTION IT WAS TYPED OVER.
  , keyed shell "a popup that closes takes its narrow with it"
      "o" "press:/ narrow:second press:Enter press:q press:o" $ \answer -> do
        assertEqual "the field is gone" [] =<< pairsAt "narrows" answer
        assertEqual "and the list came back whole" 3 . length
          =<< pairsAt "llinks" answer

  , keyed shell "the states table narrows too, over the cells it draws"
      "," "ctab:ui press:/ narrow:read" $ \answer -> do
        assertEqual "the two states spelling it, in the layer's own order"
                    ["tag:book|READING|active|", "tag:book|READ|inactive|"]
          =<< textsAt "chues" answer
        assertEqual "and it is the states table's own field"
                    [["cstates", "read"]] =<< pairsAt "narrows" answer

    -- A NARROW MATCHING NOTHING has no cursor to offer, and the surface says so.
  , keyed shell "a narrow that matches nothing leaves no row to act on"
      "" "press:: press:/ narrow:zzz press:Enter press:d" $ \answer -> do
        assertEqual "no rows are drawn" [] =<< pairsAt "ttags" answer
        assertEqual "nothing was flagged" [] =<< textsAt "tflagged" answer
        echoIs "and the key says there is no tag under it"
               "d → org-toggle-tag (no tag)" answer
  ]

spanOf :: Value -> IO [Int]
spanOf args = traverse number =<< listAt "span" args
  where number (Number n) = pure (round n)
        number other = assertFailure ("expected a number in span, got " <> show other)

openedOf :: Value -> IO [(T.Text, T.Text, T.Text)]
openedOf answer = traverse one =<< listAt "opened" answer
  where one v = (,,) <$> textAt "url" v <*> textAt "target" v <*> textAt "features" v

-- | @a@: the agenda, a canned VIEW rather than a mode, applied through the door @g@ uses.
agendaSpec :: IO T.Text -> TestTree
agendaSpec shell = testGroup "Shell agenda"
  [ keyedAt shell "?q=" 500 "applies its query the way g applies the tree's default"
      "A" "" $ \answer -> do
        assertEqual "the boot's two, then the remount's one"
          [ "/headlines?limit=100", "/headlines"
          , "/headlines?q=state%3A*active*%20-planned%3A*empty*%20sort%3Ascheduled" ]
          =<< textsAt "asked" answer
        urlIs "and the URL it settles on is that query"
          "?q=state%3A*active*+-planned%3A*empty*+sort%3Ascheduled" answer

    -- The order is IN the query, so nothing is asked of the handle: a call could state an order the query did not.
  , keyedAt shell "?q=" 500 "the rows land in scheduled order, and the query is what says so"
      "A" "" $ \answer -> do
        assertEqual "the chain the query named" [("scheduled", True)]
          =<< chainOf answer
        assertEqual "and no sort was asked of the renderer" 0
          =<< intAt "sortCalls" answer

  , keyedAt shell "?q=" 500 "and DEL takes the order back off, one token like any other"
      "A" "press:Backspace" $ \answer -> do
        urlIs "the query the strip left" "?q=state%3A*active*+-planned%3A*empty*" answer
        assertEqual "asked for without the order"
                    (Just "/headlines?q=state%3A*active*%20-planned%3A*empty*")
          . lastOf =<< textsAt "asked" answer

  , keyedAt shell "?q=" 3 "and the pill names the command and the count the server answered"
      "A" "" $
        echoIs "counted by the server, not by the page it painted"
          "A → org-glance-agenda (agenda · 3 rows)"

  , keyedAt shell "?q=" 1 "one row is one row" "A" "" $
        echoIs "singular" "A → org-glance-agenda (agenda · 1 row)"

  , keyedAt shell "?q=" 500 "an asset without a programmatic sort still applies the view"
      "" "sortless press:A" $ \answer -> do
        assertEqual "no sort was asked for" Nothing =<< sortOf answer
        urlIs "the query still went, order and all"
          "?q=state%3A*active*+-planned%3A*empty*+sort%3Ascheduled" answer

  , keyedAt shell "?q=" 500 "g returns to the tree's default view" "a g" "" $
        urlIs "the last query asked for is the default's" "?q=state%3A*active*"

    -- The landing is armed for ONE boot: a second remount must not re-sort or echo a count.
  , keyedAt shell "?q=" 500 "the landing is spent by the boot it was armed for"
      "A" "close:view-changed" $ \answer -> do
        echoIs "the remount behind the close echoed no agenda"
          "A → org-glance-agenda (agenda · 500 rows)" answer
        -- The pill is last-writer-wins, so the one trace a second run leaves is a second WRITE.
        wrote <- textsAt "echoes" answer
        assertEqual ("the agenda landed once: " <> show wrote)
                    1 (length (filter ("(agenda · " `T.isInfixOf`) wrote))

  , keyedAt shell "?q=" 500 "a held A remounts once" "A" "repeat:A repeat:A repeat:A" $
        assertEqual "one remount, so one fetch behind the boot's"
          [ "/headlines?limit=100", "/headlines"
          , "/headlines?q=state%3A*active*%20-planned%3A*empty*%20sort%3Ascheduled" ]
          <=< textsAt "asked"
  ]

-- | @\@@: the drill, and the ladder DEL walks back down it.  The stack is the RENDERER's.
drillSpec :: IO T.Text -> TestTree
drillSpec shell = testGroup "Shell drill"
  [ keyed shell "@ applies a ref view over the row at point and leaves a crumb"
      "@" "" $ \answer -> do
        assertEqual "the boot's three, the probe, then the drill's"
          [ "/headlines?q=state%3A*active*&limit=100", "/headlines?q=state%3A*active*"
          , "/headlines", "/headlines?q=ref%3Ar1&limit=1"
          , "/headlines?q=ref%3Ar1" ]
          =<< textsAt "asked" answer
        -- The crumb records where the reader was STANDING, so the label is the query being left.
        assertEqual "one crumb, naming the view it came from"
                    ["state:*active*"] =<< textsAt "crumbs" answer

    -- A drill out of the EMPTY query pushes NOTHING: "all rows" IS the empty filter, which DEL already reaches.
  , keyedAt shell "?q=" 500 "@ out of an empty query leaves no crumb, and DEL is still the way back"
      "@" "" $ \answer -> do
        assertEqual "the view is applied all the same"
          [ "/headlines?limit=100", "/headlines", "/headlines?q=ref%3Ar1&limit=1"
          , "/headlines?q=ref%3Ar1" ]
          =<< textsAt "asked" answer
        assertEqual "and the strip carries no chip" [] =<< textsAt "crumbs" answer

  , keyedAt shell "?q=" 500 "and that DEL lands on all rows, first row selected"
      "@" "press:Backspace" $ \answer -> do
        url <- textAt "url" answer
        assertBool ("the filter is cleared rather than popped: " <> T.unpack url)
                   ("?q=&" `T.isPrefixOf` url || url == "?q=")
        echoIs "named as the clearing it is" "DEL → filter-drop-token (filter cleared)" answer
        assertEqual "on the first row" (Just "r1") =<< maybeTextAt "selected" answer

    -- ZERO REFERENCES IS NO JUMP: the drill is PROBED under `limit=1', and an empty view is unreadable.
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

  , keyed shell "DEL on an emptied query pops the crumb and applies it"
      "@" "press:Backspace" $ \answer -> do
        urlIs "back on the view the drill left" "?q=state%3A*active*" answer
        assertEqual "and the trail is spent" [] =<< textsAt "crumbs" answer
        echoIs "the pill names where it landed"
          "DEL → filter-drop-token (back to state:*active*)" answer

  , keyedAt shell ("?q=ref%3Ar1%20tanik&crumbs="
                    <> bootedTrail) 500 "DEL over a refined drill strips a token before it pops"
      "Backspace" ""
        $ \answer -> do
        assertEqual "the crumb is still standing" ["everything"]
          =<< textsAt "crumbs" answer
        url <- textAt "url" answer
        assertBool ("the ref token survived the strip: " <> T.unpack url)
                   ("q=ref%3Ar1" `T.isInfixOf` url)

  , keyed shell "DEL with an empty stack clears the filter as it always has"
      "Backspace" "" $ \answer -> do
        urlIs "the cleared query, present and empty" "?q=" answer
        echoIs "the pill says so" "DEL → filter-drop-token (filter cleared)" answer

  , keyed shell "g is home and throws the trail away" "@" "press:g" $ \answer -> do
        assertEqual "no crumbs left" [] =<< textsAt "crumbs" answer
        urlIs "and the URL is the default view, with no trail on it"
          "?q=state%3A*active*" answer

    -- `setView' drops the crumbs with the world they described; the URL is what puts them back.
  , keyed shell "a remount restores the trail and the labels"
      "@" "close:view-changed" $ \answer -> do
        assertEqual "mounted three times" 3 =<< intAt "mounts" answer
        assertEqual "the crumb survived the remount" ["state:*active*"]
          =<< textsAt "crumbs" answer
        assertEqual "and the ref view is still what is applied"
                    "?q=ref%3Ar1" . T.takeWhile (/= '&') =<< textAt "url" answer

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

    -- A selection rides BESIDE the trail: the renderer's `crumbOf' keeps a label and a query and drops the rest.
  , keyed shell "a pop puts the cursor back on the row the drill was launched from"
      "n n @" "press:Backspace" $ \answer -> do
        rowIs "back on the third row" "r3" answer
        assertEqual "and the trail is spent" [] =<< textsAt "crumbs" answer

  , keyed shell "and the column it was in, when one was set"
      "n f @" "press:Backspace" $ \answer -> do
        rowIs "the row" "r2" answer
        assertEqual "and the cell it was on" 0 =<< intAt "col" answer

    -- Never force a missing id: a row the popped answer no longer holds falls through to the ordinary landing.
  , keyed shell "a remembered row the answer lost falls back to the first row"
      "n n @" "rows:2 press:Backspace" $ \answer -> do
        rowIs "the store lost r3, so the landing is row one" "r1" answer

  , keyed shell "the remembered selection rides in the URL with the trail" "n @" "" $ \answer -> do
        url <- textAt "url" answer
        assertBool ("the pair is carried: " <> T.unpack url)
                   ("sels" `T.isInfixOf` url)

    -- THE TAGS COLUMN OFFERS NO META: the ARCHIVE VIEW is the door, and the PREDICATE is untouched.
  , testCase "the tags column declares no metas, the archive view being the door" $ do
      v <- get assetsDir "/headlines" >>= decoded
      cols <- listAt "columns" v
      tagCol <- filterM (fmap (== "tag") . textAt "key") cols
      case tagCol of
        (c:_) -> assertEqual "no values declared" Nothing =<< sparseAt "values" c
        []    -> assertFailure "no tag column"

    -- THE VOCABULARY IS THE SERVER'S: the view JSON declares every saved view with the query it holds NOW.
  , testCase "the view JSON declares the saved views view: completes from" $ do
      v <- get assetsDir "/headlines" >>= decoded
      views <- listAt "views" v
      assertEqual "the registry's own order" ["default", "agenda", "archive"]
        =<< traverse (textAt "name") views
      assertEqual "each with the query it holds"
                  [ "state:*active*", "state:*active* -planned:*empty* sort:scheduled"
                  , "tag:*archive*" ]
        =<< traverse (textAt "query") views

    -- `view:NAME' IS A MACRO: the token never survives into the applied query.
  , keyed shell "view:agenda applies the agenda, and leaves no token behind"
      "" "commit:view:agenda" $ \answer -> do
        urlIs "the agenda's own query, not the token that asked for it"
              "?q=state%3A*active*+-planned%3A*empty*+sort%3Ascheduled" answer
        echoIs "the token names the command it stands for"
               "view:agenda \8594 org-glance-agenda (agenda \183 500 rows)" answer
  , keyed shell "view:default is home, crumbs and all" "n @" "commit:view:default" $
        \answer -> do
          urlIs "the tree's default view" "?q=state%3A*active*" answer
          assertEqual "and the trail it threw away" [] =<< textsAt "crumbs" answer
  , keyed shell "a view nobody carries stays the text it is" "" "commit:view:nope" $
        urlIs "committed as written" "?q=view%3Anope"

  , keyed shell "g keeps point where its answer still holds the row" "n n g" "" $
        rowIs "the row the reader was on" "r3"

    -- A commit REPAINTS rather than remounting, so without the rule the cursor sits over a set that may not hold its row.
  , keyedAt shell "?q=tanik%20web" 500 "a commit that repaints lands on the first row too"
      "n n Backspace" "" $ \answer -> do
        rowIs "row one" "r1" answer
        urlIs "and it was a strip rather than a pop" "?q=tanik" answer

    -- A held `@' is a remount per repeat, each leaving a crumb behind: hence the ONCE list.
  , keyed shell "a held @ drills once" "@" "repeat:@ repeat:@" $
        assertEqual "one crumb, not three" ["state:*active*"] <=< textsAt "crumbs"

  , keyed shell "an asset with no crumbs refuses the drill and stays put"
      "" "crumbless press:@" $ \answer -> do
        assertEqual "the boot's fetches and no more"
          [ "/headlines?q=state%3A*active*&limit=100", "/headlines?q=state%3A*active*"
          , "/headlines" ] =<< textsAt "asked" answer
        echoIs "and the pill says which call is missing"
          "@ → org-glance-overview:relations (this table-view.js has no crumbs)" answer
  ]

bootedTrail :: T.Text
bootedTrail = "%7B%22trail%22%3A%5B%7B%22label%22%3A%22everything%22%2C%22query%22%3A%22%22%7D%5D%2C%22labels%22%3A%7B%7D%7D"

bootedSels :: T.Text
bootedSels = "%7B%22trail%22%3A%5B%7B%22label%22%3A%22everything%22%2C%22query%22%3A%22%22%7D%5D%2C%22labels%22%3A%7B%7D%2C%22sels%22%3A%5B%7B%22id%22%3A%22r3%22%2C%22col%22%3Anull%7D%5D%7D"

-- | The sort the agenda asked for.  Through `field', so a harness that stopped reporting it fails loudly.
sortOf :: Value -> IO (Maybe (T.Text, Bool))
sortOf answer = field "sorted" answer >>= said
  where said Null   = pure Nothing
        said sorted = Just <$> orderKeyOf sorted

chainOf :: Value -> IO [(T.Text, Bool)]
chainOf answer = traverse orderKeyOf =<< listAt "chain" answer

orderKeyOf :: Value -> IO (T.Text, Bool)
orderKeyOf key = (,) <$> textAt "column" key <*> boolAt "ascending" key

lastOf :: [a] -> Maybe a
lastOf = listToMaybe . reverse

-- | The which-key letters, driven as the pure function they are: order-only, first still-free letter of the entry's own spelling.
whichKeySpec :: IO T.Text -> TestTree
whichKeySpec shell =
  overBoot shell "C-c C-t" "" $ \palette ->
  testGroup "Shell which-key"
  [ testCase "the assignment, cycle by cycle" $ mapM_ (assigns shell)
      -- org's pair leads, so TODO takes `t' and DONE takes `d' in every tree.
      [ ( "TODO,DONE,DELEGATED", ["t@0", "d@0", "e@1"] )
      , ( "DELEGATED,TODO,DONE", ["d@0", "t@0", "o@1"] )
      -- `*empty*' is not in the pool: the meta answers to DEL and `offer' keeps it out.
      , ( "TODO,NEXT,STARTED,WAITING,DELEGATED,CANCELLED,DONE"
        , ["t@0", "n@0", "s@0", "w@0", "d@0", "c@0", "o@1"] )
      -- Synthetic, since no real cycle exhausts a letter pool: an entry with nothing left is UNBOUND.
      , ( "ON,NO,NOON", ["o@0", "n@0", "-"] )
      , ( "CANCELLED,CLOSED",     ["c@0", "l@1"] ) ]

    -- One row per SOURCE in precedence order: the table IS the classify chain, under the NAME each arrives under.
  , atBoot palette "the table draws one row per source, keywords in their cells" $ \answer -> do
        assertEqual "the header, the sources in order, and the meta last"
          [ ("pr ph", "source",   ["active"],      ["inactive"])
          , ("pr",    "default",  ["[T]ODO"],    ["[D]ONE"])
          , ("pr",    "book",     ["[R]EADING"], ["R[E]AD"])
          , ("pr",    "file",     ["[L]ATER"],   [])
          , ("pr pm", "",         ["DEL *empty*"], []) ] =<< paletteOf answer
        assertEqual "and the foot names the keys the list cannot draw"
                    "a letter sets it · + adds one · / to search · ESC leaves"
          =<< textAt "pfoot" answer

    -- `+' ASKS FOR A STATE THE STORE HAS NOT GOT.  It is DECLARED in a config
    -- layer first, and only then set: `set-state' refuses a keyword outside the
    -- row's own chain, which is the wall this walks through rather than around.
  , keyed shell "+ raises the mint form, and the palette stands behind it"
      "t" "press:+" $ \answer -> do
        assertEqual "the form is up" "on" =<< textAt "mint" answer
        assertEqual "over a palette that did not go" "on" =<< textAt "prompt" answer
        assertEqual "the namespaces: the tree, then the tag layers it has"
                    ["system", "tag:film", "tag:book"] =<< textsAt "nspaces" answer

    -- The rows on screen are the rows that filter chose, so its tags are the
    -- namespaces in play and the first of them is what the form opens on.
  , keyedAt shell "?q=tag%3Abook" 500 "a tag: filter puts its tag in the select, and first"
      "t" "press:+" $ \answer -> do
        assertEqual "the query's tag leads, and the tree's own follow"
                    ["system", "tag:book", "tag:film"] =<< textsAt "nspaces" answer
        assertEqual "opened on the tag the filter named, active, with no hue"
                    ["tag:book", "", "active", "", ""] =<< textsAt "nfields" answer

    -- `tagOf' lowercases a layer's basename into its tag, so `Book' and `book' are
    -- ONE layer; offering both would mint the same file twice under two names.
  , keyedAt shell "?q=tag%3ABook" 500 "a tag named in another case folds onto the layer it is"
      "t" "press:+" $ \answer -> do
        assertEqual "one entry for it, folded, and no second spelling"
                    ["system", "tag:book", "tag:film"] =<< textsAt "nspaces" answer

  , keyed shell "the minted state is declared in its layer, then set on the rows"
      "t" "press:+ nfields:tag:book/HANDED/active// press:Enter" $ \answer -> do
        wrote <- oneConfigWrite answer
        assertEqual "into that layer's own file"
                    "/o/.org-glance/config/tags/book.org" =<< textAt "path" wrote
        assertEqual "the cycle carries it now"
                    ["#+TODO: TODO READING HANDED | READ"] =<< textsAt "lines" wrote
        assertEqual "and the state landed on the row" ["set-state"]
          =<< traverse (textAt "name") =<< listAt "commands" answer
        assertEqual "the form went with it" "" =<< textAt "mint" answer

    -- The tag has no file, so the write is what brings the layer into being.
  , keyed shell "a namespace with no layer file yet is minted by the write"
      "t" "press:+ nfields:tag:cinema/QUEUED/inactive// press:Enter" $ \answer -> do
        wrote <- oneConfigWrite answer
        assertEqual "under the tree's own tags directory"
                    "/o/.org-glance/config/tags/cinema.org" =<< textAt "path" wrote
        assertEqual "declaring the state on the done side of the bar"
                    ["#+TODO:  | QUEUED"] =<< textsAt "lines" wrote
        assertEqual "with the empty digest, which is what a write reads as create"
                    "" =<< textAt "digest" wrote

    -- A COLOUR IS THE SYSTEM LAYER'S, so a state minted under a tag moves two files.
  , keyed shell "a hue per theme rides a second write, to the system layer"
      "t" "press:+ nfields:tag:book/HANDED/active/#7B1FA2/#D0A0FF press:Enter"
      $ \answer -> do
        writes <- listAt "configWrites" answer
        assertEqual "two writes: the cycle, then the colours" 2 (length writes)
        assertEqual "the second at the system layer"
                    "/o/.org-glance/config/system.org" =<< textAt "path" (writes !! 1)
        assertEqual "carrying a line's worth per theme"
                    [["light", "HANDED", "#7B1FA2"], ["dark", "HANDED", "#D0A0FF"]]
          =<< coloursOf (writes !! 1)

  , keyed shell "ESC leaves the mint and hands the palette back"
      "t" "press:+ press:Escape" $ \answer -> do
        assertEqual "the form went" "" =<< textAt "mint" answer
        assertEqual "the palette did not" "on" =<< textAt "prompt" answer
        assertEqual "and nothing was written" ([] :: [Value])
          =<< listAt "configWrites" answer

  , keyed shell "a word org cannot read back is refused before the write"
      "t" "press:+ nfields:system/IN-PROGRESS/active// press:Enter" $ \answer -> do
        assertEqual "nothing went" ([] :: [Value]) =<< listAt "configWrites" answer
        assertEqual "the form stands, for the word to be fixed" "on"
          =<< textAt "mint" answer

    -- One parameter per id rather than a comma list: the fallback row id is a path.
  , testCase "the resolution is asked for the rows the command names" $ do
      bootOf shell "" 500 "C-c C-t" "" $
        assertEqual "the row at point" ["/keywords?ids=r1"] <=< textsAt "resolved"
      bootOf shell "" 500 "m m C-c C-t" "" $
        assertEqual "the marked set, in one request"
                    ["/keywords?ids=r1&ids=r2"] <=< textsAt "resolved"

  , keyed shell "a set spanning two tags shows both tag sources" "" "twotags press:t" $
        assertEqual "the default pair, then book, then film"
          [ ("pr ph", "source",   ["active"],       ["inactive"])
          , ("pr",    "default",  ["[T]ODO"],     ["[D]ONE"])
          , ("pr",    "book",     ["[R]EADING"],  ["R[E]AD"])
          , ("pr",    "film",     ["[W]ATCHING"], ["W[A]TCHED"])
          , ("pr pm", "",         ["DEL *empty*"],  []) ] <=< paletteOf

    -- The letter is marked INSIDE the keyword in that state's own badge hue; `*empty*' alone keeps a token.
  , atBoot palette "the letter is marked in the word, and only *empty* wears a token"
      $ \answer -> do
        assertEqual "one token in the whole table, on the meta row"
                    ["DEL"] . filter (not . T.null) . map snd
          =<< paletteField "key" answer
        assertEqual "and the rule under each letter is that keyword's own hue"
          [ ("[T]ODO", "#e0af68"), ("[D]ONE", "#73daca"), ("[R]EADING", "#bb9af7") ]
                    . filter (not . T.null . snd)
          =<< paletteField "mark" answer

    -- DEL ERASES THE LAST STRUCTURE STANDING, and over a popup with no inner ladder that is the popup.
  , testCase "DEL closes a popup that has nothing inside it to erase" $ do
      bootOf shell "" 500 "o" "press:Backspace" $ \answer -> do
        assertEqual "the link popup is gone" "" =<< textAt "popup" answer
        echoIs "and the pill names the function that ran" "DEL → keyboard-quit" answer
      bootOf shell "" 500 ":" "press:Backspace" $ \answer -> do
        assertEqual "and so is the tag popup" "" =<< textAt "tagpop" answer
        echoIs "under the same line" "DEL → keyboard-quit" answer
      -- IN NAV ALONE: inside an OPEN edit the key is the field's own erase and the page declines it.
      bootOf shell "" 500 "o" "press:e press:Backspace" $ \answer -> do
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

    -- The overlay goes up on the keypress and the answer fills it, so ESC works from the moment the key lands.
  , keyed shell "the palette is up before the resolution is" "" "stall press:t" $ \answer -> do
        assertEqual "raised" "on" =<< textAt "prompt" answer
        assertEqual "with a line saying what it is waiting for"
                    [("pnone", "", ["resolving…"], [])] =<< paletteOf answer

    -- The fallback is FLAT and drops the token column: no letter commits there, so drawing one would lie.
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

  , keyed shell "a refused resolution closes the palette and says so"
      "" "refuse press:t" $ \answer -> do
        assertEqual "the overlay is down" "" =<< textAt "prompt" answer
        assertEqual "and the log named it"
                    (Just "keywords failed: GET /keywords?ids=<row id>")
          =<< lastLog answer
  ]

-- | The sheet's two panes as keys: what the DOCUMENT draws and how it is walked, what the PANEL shows, what a sync sends.
sheetSpec :: IO T.Text -> TestTree
sheetSpec shell =
  overBoot shell "Enter" "" $ \sheet ->
  testGroup "Shell sheet"
  [ -- A BADGE CELL WEARS THE COLUMN'S OWN HUE, which is the THEME's: the wire
    -- carries a SLOT (`var(--g-state-a0)') rather than a colour, so a theme switched client-side moves the pane with it.
    atBoot sheet "the headline's badge cells wear the theme's own hues" $ \answer -> do
        assertEqual "the declared hue, and nothing for an absent priority"
                    ["#e0af68", ""] =<< textsAt "dhues" answer

    -- The pane is an Elm program mounted INSIDE `#dlist', so emptying that element takes the program's node with it.
  , keyed shell "the sheet closed and opened again still draws its document"
      "Enter" "press:Escape press:Enter" $ \answer -> do
        assertEqual "the document is there the second time"
          fixtureDoc
          =<< pairsAt "doc" answer
        assertEqual "and the lifted header with it"
          ([["EFFORT", "0:30"]], [["SCHEDULED", sheetStamp]])
          =<< ((,) <$> pairsAt "dprops" answer <*> pairsAt "dplan" answer)

  , atBoot sheet "materialize opens two panes over one subtree" $ \answer -> do
        -- Every headline line opens with its STARS, org-cleaned: `org-hide-leading-stars' with `org-startup-indented'.
        assertEqual "the document draws the headline, the lifted header, the paragraphs and the child whole"
          fixtureDoc =<< docOf answer
        assertEqual "with the cursor on the headline and no cell picked yet"
                    0 =<< pointOf answer
        assertEqual "and the document holding the keys" True =<< boolAt "dactive" answer
        assertEqual "the trail is the row, and it is where the reader stands"
                    (["one"], [0]) =<< ((,) <$> textsAt "where" answer
                                            <*> flaggedAt "whereAt" answer)
        assertEqual "the textarea is behind it, empty until C-c '"
                    "" =<< textAt "sheet" answer
        headerIs "the mirrors carry the lifted header the fill sent in"
                 [["EFFORT", "0:30"]] [["SCHEDULED", sheetStamp]] answer
        -- The drawer's INTERIOR alone: the widget being the drawer says what it is.
        assertEqual "the logbook is shown, its delimiters left off"
                    "- moved here" =<< textAt "logbook" answer
        assertEqual "and the sheet is in its two-pane shape" "" =<< textAt "shape" answer
        assertEqual "with nothing focused, which is what frees the letters"
                    "" =<< textAt "focus" answer

  , testCase "the document walks its elements on n/p, j/k and the arrows" $ do
      -- `f' FIRST, into the body the walks below measure.
      insheet shell "press:f press:n" $
        assertEqual "two stops down, the planning line and the drawer" 2 <=< pointOf
      insheet shell "press:f press:j press:j press:k" $
        assertEqual "vi's pair walks the same elements" 2 <=< pointOf
      insheet shell "press:f press:ArrowDown press:ArrowDown press:ArrowUp" $
        assertEqual "and so do the arrows" 2 <=< pointOf
      insheet shell "press:p" $
        assertEqual "the headline is the end of the walk up" 0 <=< pointOf
      -- THE WALK SKIPS OWNED ROWS: the child's own block is reached with f.
      insheet shell "press:f press:n press:n press:n press:n" $
        assertEqual "four stops down is the child, its block skipped" 5 <=< pointOf
      -- THE TAIL CLOSES THE WALK: the synthesized empty row past everything.
      insheet shell "press:f press:n press:n press:n press:n press:n" $
        assertEqual "and the tail the end of the walk down" 7 <=< pointOf

    -- THE ROOT READS INTO ITS OWN CONTENTS: `n' from the entry's line is the
    -- reader's step.  A CHILD headline walks headlines -- org's own
    -- next/previous-visible-heading -- and its contents are behind `f'.
  , testCase "the root steps into its contents, and a child walks headlines" $ do
      insheet shell "press:n" $
        assertEqual "the entry's first content row" 1 <=< pointOf
      insheet shell "press:n press:p" $
        assertEqual "and p climbs back to the sheet's own line" 0 <=< pointOf
      insheet shell "press:f" $
        assertEqual "f enters the same body" 1 <=< pointOf
      insheet shell "press:n press:n press:n press:n press:n press:p" $
        assertEqual "p from the child crosses the contents whole" 0 <=< pointOf
      insheet shell "press:n press:n press:n press:n press:n press:n" $
        assertEqual "and n past the child lands on the tail" 7 <=< pointOf
      insheet shell "press:n press:n press:n press:n press:n press:n press:n" $
        assertEqual "nothing stands past the tail, so the walk stays" 7 <=< pointOf

    -- `p' IS HEADLINE-SIZED PAST A BODY'S EDGE: inside a body it is the shelf's
    -- own element step (`j/k' above), and where the shelf has nothing above --
    -- the body's first element, the TAIL past every subtree -- it lands on the
    -- NEAREST VISIBLE HEADLINE, org's own previous-visible-heading.
  , testCase "p climbs to a headline past its body's edge" $ do
      insheet shell (ontoChild <> " press:f press:p") $
        assertEqual "a body's first element climbs to its own headline" 5 <=< pointOf
      insheet shell (ontoChild <> " press:n press:p") $
        assertEqual "and the tail is one press from the last headline" 5 <=< pointOf

    -- GEOMETRY IS BEYOND THE STUB, so what is asserted is that the page ASKED.
  , testCase "the element under point asks its pane's scroller" $ do
      insheet shell "press:f press:n press:n press:n" $ \answer -> do
        seen <- textsAt "scrolled" answer
        assertEqual "the last ask was made on the element under point"
                    (Just "de d-para dat lvl-top") (listToMaybe (reverse seen))
        -- `block:"nearest"' IS the scrolloff band as the platform spells it.
        assertEqual "and it asked for the band, not a re-centring"
                    (object [ "block" .= ("nearest" :: T.Text) ])
          =<< field "scrollAsked" answer
      insheet shell "" $ \answer -> do
        seen <- textsAt "scrolled" answer
        assertEqual "the materialize itself asked, on the headline"
                    (Just "de d-head dat lvl-top") (listToMaybe seen)

    -- THE HEADLINE IS ONE STOP: its parts have their own keys, so `f' does not
    -- walk into the line -- it enters the CONTENTS, everything being under them.
  , testCase "f on the headline enters the body, and a paragraph has nothing finer" $ do
      insheet shell "press:f" $ \answer -> do
        assertEqual "the first row of the body" 1 =<< pointOf answer
        echoIs "and the key named what it entered" "f → grain-finer (the body)" answer
      insheet shell "press:f press:n press:n press:f" $ \answer -> do
        assertEqual "a paragraph does not move" 3 =<< pointOf answer
        echoIs "and says why" "f → grain-finer (nothing finer here)" answer

    -- Asserted as the EQUALITY of two numbers this page produces independently.
  , atBoot sheet "a child's star sits in the parent's body column" $ \answer -> do
        rows <- docOf answer
        body <- textAt "dindent" answer
        let prefix = case [ r | r <- rows, take 1 r == ["child"] ] of
                       ((_kind : p : _rest) : _more) -> p
                       _none                         -> ""
        assertEqual "the child's prefix is two spaces and its star" "  * " prefix
        assertEqual "and its star stands in the column the body starts at"
                    (Just (read (T.unpack body) :: Int)) (T.findIndex (== '*') prefix)

    -- What this pins is that the indent is DERIVED from `dstars' rather than a 2 spelled beside it.
  , testCase "content lines start at the title's column, at either depth" $ do
      insheet shell "" $
        assertEqual "the row's own document" "2" <=< textAt "dindent"
      insheet shell (ontoChild <> " press:Enter") $
        assertEqual "and a child, which is the root of its own" "2"
          <=< textAt "dindent"

    -- NO PLACEHOLDERS, EVER: an absent part renders nothing in every state.
  , testCase "an absent part renders nothing, in every state" $
      mapM_ (\(what, keys) ->
               insheet shell keys $ \answer -> do
                 rows <- docOf answer
                 assertEqual (what <> ": the headline line, and nothing it lacks")
                             ["head", "* ", "TODO", "one"] (head rows)
                 assertEqual (what <> ": nor the child's")
                             ["child", "  * ", "two", ":web:"] (rows !! 5))
            [ ("at rest", ""), ("on the element", "press:p")
            , ("on the child", ontoChild) ]

    -- RET IS BY KIND, and a CHILD re-materializes into that entry under the index the server handed over.
  , testCase "RET on a child materializes into it, and DEL climbs back" $ do
      insheet shell (ontoChild <> " press:Enter") $ \answer -> do
        -- The drawer is drawn even with no pairs: `+' needs a place to land.
        assertEqual "the child's own document"
          [ ["head", "* ", "two", ":web:"]
          , ["comp:properties:drawer", ":PROPERTIES: \8230"]
          , ["para", "child body"]
          , ["para:tail", ""] ] =<< docOf answer
        assertEqual "the trail gained a crumb" ["one", "two"] =<< textsAt "where" answer
        assertEqual "and the last one is where the reader stands" [1]
          =<< flaggedAt "whereAt" answer
        echoIs "and the pill names what it opened"
          "RET → org-glance-overview:materialize (two)" answer
      insheet shell
             (ontoChild <> " press:Enter press:Backspace") $ \answer -> do
        assertEqual "back at the row, one crumb again" ["one"] =<< textsAt "where" answer
        assertEqual "with the cursor on the child it came out of" 5
          =<< pointOf answer
        echoIs "and the pill names the climb" "DEL → org-glance-overview:up (one)" answer

    -- The table's own DEL must not also fire, or the filter under the sheet would lose a token to the same press.
  , keyed shell "DEL at the top closes the sheet and nothing else"
      "Enter" "press:Backspace" $ \answer -> do
        assertEqual "the sheet is closed" "" =<< textAt "modal" answer
        urlIs "and the applied query is where it was" "?q=state%3A*active*" answer

    -- `RET' COMMITS an open paragraph — org's `C-c C-c' under another name; `S-RET' adds a sibling and `M-RET' is the newline.
  , testCase "q closes the sheet, and is a letter inside an open edit" $ do
      -- `quit-window' ONE WINDOW IN: free here because the document pane holds the keys with NOTHING focused.
      insheet shell "press:q" $ \answer -> do
        assertEqual "the sheet is shut" "" =<< textAt "modal" answer
        echoIs "named as the command it is" "q → quit-window" answer
      insheet shell "press:f press:n press:n press:Enter press:q" $ \answer -> do
        assertEqual "the sheet stands" "on" =<< textAt "modal" answer
        assertEqual "and the edit with it" True =<< boolAt "dparaopen" answer
      insheet shell "press:Enter press:q" $ \answer ->
        assertEqual "the sheet stands over an open title" "on" =<< textAt "modal" answer

  , testCase "RET commits the open paragraph and M-RET is the newline" $ do
      insheet shell "press:f press:n press:n press:Enter dpara:rewritten press:Enter" $
        \answer -> do
          assertEqual "the body with that block replaced"
            ["* TODO one\nrewritten\n\nsecond para\n** two\nchild body\n"]
            =<< traverse (textAt "body") =<< listAt "writes" answer
          assertEqual "and the edit is shut" False =<< boolAt "dparaopen" answer
      -- `S-RET' COMMITS THE SAME BYTES; the sibling it asks for is what the key adds.
      insheet shell "press:f press:n press:n press:Enter dpara:rewritten press:S-Enter" $
        \answer ->
          assertEqual "the same body, under the key that asks for another"
            ["* TODO one\nrewritten\n\nsecond para\n** two\nchild body\n"]
            =<< traverse (textAt "body") =<< listAt "writes" answer
      insheet shell "press:f press:n press:n press:Enter dpara:one press:M-Enter" $
        \answer -> do
          assertEqual "nothing was written" [] =<< textsAt "wroteAt" answer
          assertEqual "the edit is still open" True =<< boolAt "dparaopen" answer
          assertEqual "with a newline at the caret" "one\n" =<< textAt "dtext" answer

    -- THE BOX GROWS WITH WHAT IS TYPED, to a cap: the shell writes the count, the stylesheet owns the arithmetic.
  , testCase "the paragraph box stands as tall as what is in it, up to ten" $ do
      mapM_ (\(keys, what, rows) ->
               insheet shell keys (assertEqual what rows <=< textAt "dprows"))
        [ ("press:f press:n press:n press:Enter", "one line to open with", "1")
        , ("press:f press:n press:n press:Enter dpara:one|two|three", "three where three were typed", "3")
          -- The META key splices at the caret rather than going through the field's own event.
        , ( "press:f press:n press:n press:Enter dpara:one press:M-Enter"
          , "and M-RET grows it by the line it just made", "2" )
        , ( "press:f press:n press:n press:Enter dpara:"
              <> T.intercalate "|" (map (T.pack . show) [1 :: Int .. 14])
          , "capped, so the document under it stays readable", "10" )
        , ( "press:f press:n press:n press:+ dpara:one|two|three|four"
          , "an added paragraph grows the same way", "4" )
          -- The room goes back to ZERO rather than to one: the field's metrics differ from the pane's row.
        , ( "press:f press:n press:n press:Enter dpara:one|two|three press:Escape"
          , "no floor at all once the edit is gone", "0" )
        , ( "press:f press:n press:n press:Enter dpara:one|two|three press:Enter"
          , "and a commit gives it back too", "0" )
          -- A sheet that never opened an edit writes no number, and the STYLESHEET's own `0' covers it.
        , ("", "a sheet with nothing open never wrote one", "") ]

  , testCase "RET opens a paragraph as text, and C-x C-s writes it" $ do
      insheet shell "press:f press:n press:n press:Enter" $ \answer -> do
        assertEqual "the block is open" True =<< boolAt "dparaopen" answer
        assertEqual "with its text in the field" "first para" =<< textAt "dtext" answer
        assertEqual "and the focus in it" "dtext" =<< textAt "focus" answer
      insheet shell
             "press:f press:n press:n press:Enter dpara:rewritten press:C-x press:C-s" $ \answer -> do
        assertEqual "one write, aimed at the row"
                    ["r1"] =<< textsAt "wroteAt" answer
        assertEqual "the body with that block replaced and nothing else"
                    ["* TODO one\nrewritten\n\nsecond para\n** two\nchild body\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer
        assertEqual "and the sheet is synced" "synced" =<< textAt "state" answer

    -- The overlay opens EMPTY and nothing moves until `RET', so `ESC' is a no-op by construction.
  , testCase "+ opens an empty paragraph and writes nothing yet" $
      insheet shell "press:f press:n press:n press:+" $ \answer -> do
        assertEqual "the overlay is up" True =<< boolAt "dparaopen" answer
        assertEqual "and EMPTY, where RET opens with the text" ""
          =<< textAt "dtext" answer
        assertEqual "with the focus in it" "dtext" =<< textAt "focus" answer
        assertEqual "and nothing written" [] =<< textsAt "wroteAt" answer
        echoIs "the echo names where it would land"
               "+ \8594 org-insert-element (after this paragraph)" answer

    -- THE PARAGRAPH IS DRAWN BEFORE IT IS WRITTEN; the row is zero-width, which `bodyText' passes over.
  , testCase "+ draws the empty paragraph, and point goes to it" $
      insheet shell "press:f press:n press:n press:+" $ \answer -> do
        assertEqual "a line of its own, under the one point stood on"
                    [ "head", "meta", "comp:properties:drawer"
                    , "para", "draft:para", "para", "child", "para", "para:tail" ]
          =<< map head <$> docOf answer
        assertEqual "holding nothing" [""] . partsOf "draft:para" =<< docOf answer
        assertEqual "and the cursor is on it" 4 =<< intAt "dat" answer

    -- AND AN ITEM IS DRAWN AS THE ITEM IT WILL BE: the row wears the LEAD.
  , testCase "+ on an item draws the item it will be" $ do
      onTable shell (intoRun <> " press:+") $ \answer -> do
        assertEqual "drawn STRICTLY BELOW the stop, never at the run's bottom"
                    [ "head", "meta", "comp:properties:drawer", "para", "comp:list"
                    , "item", "item", "draft:item"
                    , "item", "item", "comp:quote", "item", "item", "para", "child", "para"
                    , "para:tail" ]
          =<< map head <$> docOf answer
        assertEqual "wearing the stop's own bullet" ["- "]
          . partsOf "draft:item" =<< docOf answer
        assertEqual "and the cursor is on it" 7 =<< intAt "dat" answer
        echoIs "the echo names the level, not the structure"
               "+ \8594 org-insert-element (an item at this level)" answer

    -- A CONTINUATION LANDS UNDER THE ITEM'S OWN TEXT: org reads one by its indent.
  , testCase "M-RET inside an item carries the marker's width onto the next line" $ do
      onTable shell (intoRun <> " press:Enter press:M-Enter") $
        \answer -> do
          -- The caret opens at the head of the box, so the newline lands there and
          -- the spaces the marker occupies come with it.
          box <- textAt "dtext" answer
          assertEqual "the newline carried the marker's width"
                      "\n  - alpha\n  more alpha" box
      -- AND A PARAGRAPH TAKES NONE: there is no marker to sit under.
      insheet shell "press:f press:n press:n press:Enter press:M-Enter" $
        assertEqual "a paragraph's newline carried an indent" "\nfirst para"
          <=< textAt "dtext"

    -- TAB WALKS THE RUNGS AN ITEM MAY SIT ON, and it is a TOGGLE: the walk comes
    -- back where it started, so it is undone from the keyboard alone.
  , testCase "TAB in an open item walks its levels and comes back" $ do
      onTable shell (intoRun <> " press:+ press:Tab") $ \answer -> do
        assertEqual "the box holds the item one level in" "  - "
          =<< textAt "dtext" answer
        echoIs "and the echo names the rung"
               "TAB \8594 org-metaright (one level in)" answer
      onTable shell (intoRun <> " press:+ press:Tab press:Tab") $
        \answer -> do
          assertEqual "and around again to where it opened" "- "
            =<< textAt "dtext" answer
          echoIs "which the echo says too"
                 "TAB \8594 org-metaright (back where it was)" answer

    -- AND THE RUNG IS WHAT IS WRITTEN: `+' then TAB is how a child is made, and
    -- the marker carries the indent it was walked to.
  , testCase "an item TABbed in is written under the one above it" $
      onTable shell (intoRun <> " press:+ press:Tab press:Enter") $
        \answer -> do
          wrote <- traverse (textAt "body") =<< listAt "writes" answer
          assertBool ("the deeper marker was written: " <> show wrote)
                     (any (T.isInfixOf "\n  - \n") wrote)

  , testCase "TAB has no rung to take where there is no list item" $
      insheet shell "press:f press:n press:n press:Enter press:Tab" $ \answer -> do
        echoIs "the paragraph is not one" "TAB \8594 org-metaright (not a list item)" answer
        assertEqual "and nothing moved" "first para" =<< textAt "dtext" answer

  , testCase "and ESC leaves behind what it found, point included" $
      insheet shell "press:f press:n press:n press:+ dpara:typed press:Escape" $ \answer -> do
        assertEqual "the drawn row goes with the box"
                    [ "head", "meta", "comp:properties:drawer"
                    , "para", "para", "child", "para", "para:tail" ]
          =<< map head <$> docOf answer
        assertEqual "and point is back on the stop it was pressed from"
                    3 =<< intAt "dat" answer

  , testCase "RET writes it in under the paragraph point stood on" $
      insheet shell "press:f press:n press:n press:+ dpara:added press:Enter" $ \answer -> do
        assertEqual "one write, aimed at the row" ["r1"] =<< textsAt "wroteAt" answer
        assertEqual "the paragraph joined and nothing else"
          ["* TODO one\nfirst para\n\nadded\n\nsecond para\n** two\nchild body\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer
        assertEqual "the overlay is shut" False =<< boolAt "dparaopen" answer
        assertEqual "the pane shows it where it was drawn"
                    ["added"] . partsOf "draft:para" =<< docOf answer
        assertEqual "with the cursor still on it" 4 =<< intAt "dat" answer
        echoIs "" "RET \8594 org-ctrl-c-ctrl-c (paragraph added)" answer

    -- THE SEPARATOR IS DECIDED rather than spelled: a fixed "\n\n" reads the prose back as one paragraph.
  , testCase "+ under the last paragraph keeps a blank before the child" $
      insheet shell "press:f press:n press:n press:n press:+ dpara:added press:Enter" $ \answer ->
        assertEqual ""
          ["* TODO one\nfirst para\n\nsecond para\n\nadded\n\n** two\nchild body\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer

  , testCase "+ on the headline line leads the body" $ do
      insheet shell "press:+" $ \answer ->
        echoIs "the echo says so before a byte moves"
               "+ \8594 org-insert-element (at the top)" answer
      insheet shell "press:+ dpara:opener press:Enter" $ \answer ->
        assertEqual "and the paragraph goes in ahead of the first"
          ["* TODO one\nopener\n\nfirst para\n\nsecond para\n** two\nchild body\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer

    -- `+' ADDS A SIBLING OF THE STOP, and THE GRAIN IS THE SELECTOR.
  , testCase "+ inside a list adds an item at the list's bottom" $
      onTable shell (intoRun <> " press:+ dpara:-_note press:Enter") $
        \answer ->
          assertEqual "under alpha and the run nested INSIDE it, never past gamma"
            [ "* TODO one\nlead in\n- alpha\n  more alpha\n  - nested\n- note\n\n"
              <> "- beta\n- gamma\n\n#+begin_quote\nquoted one\n\nquoted two\n"
              <> "#+end_quote\n\ntail para\n** two\nchild body\n" ]
            =<< traverse (textAt "body") =<< listAt "writes" answer

  , testCase "+ on a nested item joins the NESTED run, at its own indent" $ do
      onTable shell (intoNestedRun <> " press:+") $ \answer -> do
        assertEqual "drawn under the nested item, inside alpha"
                    [ "head", "meta", "comp:properties:drawer", "para", "comp:list"
                    , "item", "item", "draft:item"
                    , "item", "item", "comp:quote", "item", "item", "para", "child", "para"
                    , "para:tail" ]
          =<< map head <$> docOf answer
        assertEqual "wearing the nested indent" ["  - "]
          . partsOf "draft:item" =<< docOf answer
        assertEqual "and the cursor is on it" 7 =<< intAt "dat" answer
        -- EVERY BYTE ON SCREEN EXACTLY ONCE; `downers' is the reading that sees it, where the flat `.de' walk cannot.
        assertEqual "the leaves past it are still the composite's own"
                    [-1, -1, -1, -1, -1, 4, 5, 5, 4, 4, -1, 10, 10, -1, -1, -1, -1]
          =<< flaggedAt "downers" answer
      onTable shell (intoNestedRun <> " press:+ dpara:__-_note press:Enter") $ \answer ->
        assertEqual "two spaces in, above the blank the outer run keeps"
          [ "* TODO one\nlead in\n- alpha\n  more alpha\n  - nested\n  - note\n\n"
            <> "- beta\n- gamma\n\n#+begin_quote\nquoted one\n\nquoted two\n"
            <> "#+end_quote\n\ntail para\n** two\nchild body\n" ]
          =<< traverse (textAt "body") =<< listAt "writes" answer

  , testCase "the composite still lands a paragraph past the whole list" $ do
      onTable shell "grain press:Enter press:f press:n press:n press:n press:+ dpara:note press:Enter" $
        \answer ->
          assertEqual "past the last item, never between two"
            [ "* TODO one\nlead in\n- alpha\n  more alpha\n  - nested\n\n- beta\n- gamma\n\n"
              <> "note\n\n#+begin_quote\nquoted one\n\nquoted two\n#+end_quote\n\n"
              <> "tail para\n** two\nchild body\n" ]
            =<< traverse (textAt "body") =<< listAt "writes" answer
      onTable shell "grain press:Enter press:f press:n press:n press:n press:+" $
        echoIs "and the echo is the structure's, as it was"
               "+ \8594 org-insert-element (after the list)"

    -- A CHECKBOX COMES ALONG EMPTY, org's own `org-insert-item': the box is part of what the line OPENS with.
  , testCase "a checkbox item's new sibling comes along boxed and empty" $
      onTable shell (intoChecky <> " press:+ dpara:-_[_]_epsilon press:Enter") $
        \answer ->
          assertEqual "an EMPTY box, whatever the stop's own state"
            [ "* TODO one\n- [ ] alpha\n- [ ] epsilon\n- [X] beta\n- [-] gamma\n"
              <> "- delta\n** two\nchild body\n" ]
            =<< traverse (textAt "body") =<< listAt "writes" answer

    -- AN ITEM'S TOKEN IS ON SCREEN WHILE IT IS TYPED: the box is laid over the drawn row exactly and opaquely.
  , testCase "the box opens wearing the token the row was drawn with" $ do
      onTable shell (intoRun <> " press:+") $ \answer -> do
        assertEqual "the bullet is in the field, not only under it" "- "
          =<< textAt "dtext" answer
        assertEqual "and the row still wears it too" ["- "]
          . partsOf "draft:item" =<< docOf answer
      onTable shell (intoChecky <> " press:+") $
        assertEqual "a checkbox list opens its box boxed" "- [ ] "
          <=< textAt "dtext"
      onTable shell "grain press:Enter press:f press:n press:n press:+" $
        assertEqual "and a PARAGRAPH opens empty, owing no token" ""
          <=< textAt "dtext"

    -- A marker is a LEAD everywhere but a TABLE, whose row closes with a pipe — point at its end types a THIRD column.
  , testCase "point stands inside a seeded table row, past a seeded lead" $ do
      onTable shell "tabled press:Enter press:f press:n press:n press:n press:f press:Enter press:S-Enter" $
        \answer -> do
          assertEqual "the row is drawn at the table's own widths" "|   |   |"
            =<< textAt "dtext" answer
          assertEqual "with point one space into its first cell" 2
            =<< intAt "dcaret" answer
      onTable shell (intoRun <> " press:+") $
        assertEqual "where a bullet is a lead and point follows it" 2
          <=< intAt "dcaret"

    -- AND THE LEAD GOES BACK OFF ON THE WAY OUT.
  , testCase "what the reader adds is what the wire carries" $ do
      onTable shell (intoChecky <> " press:+ dpara:-_[_]_epsilon press:Enter") $
        \answer ->
          assertEqual "typed after the token"
            [ "* TODO one\n- [ ] alpha\n- [ ] epsilon\n- [X] beta\n- [-] gamma\n"
              <> "- delta\n** two\nchild body\n" ]
            =<< traverse (textAt "body") =<< listAt "writes" answer
      -- AND A BOX HOLDING NOTHING BUT ITS OWN TOKEN IS NO ITEM.
      onTable shell (intoChecky <> " press:+ dpara:-_[_]_ press:Enter") $
        \answer -> do
          assertEqual "nothing written" ([] :: [Value]) =<< listAt "writes" answer
          echoIs "" "RET \8594 org-ctrl-c-ctrl-c (nothing added)" answer

    -- EVERY COMMAND THAT NAMES ROWS OWES A LOG PHRASE, and the join between the two tables is checked rather than kept by hand.
  , testCase "every command that names rows spells its own log phrase" $ do
      page <- shell
      let phraseless = ["capture"]   -- makes a row rather than naming one
          owed = filter (`notElem` phraseless) commandNames
          quoted n = "\"" <> n <> "\":"
          spelled n = quoted n `T.isInfixOf` page
                        || (n <> ":") `T.isInfixOf` page
      assertBool ("too few commands swept: " <> show (length owed))
                 (length owed >= 8)
      assertEqual "a command whose rows would log another command's phrase" []
                  [ n | n <- owed, not (spelled n) ]

    -- WHAT THE BOX HOLDS IS WHAT IS WRITTEN: a prepended lead gave the reader both.
  , testCase "a token the reader edits is the token that is written" $ do
      onTable shell (intoChecky <> " press:+ dpara:-_DONE_ship_it press:Enter") $
        \answer ->
          assertEqual "their line, and no second token in front of it"
            [ "* TODO one\n- [ ] alpha\n- DONE ship it\n- [X] beta\n- [-] gamma\n"
              <> "- delta\n** two\nchild body\n" ]
            =<< traverse (textAt "body") =<< listAt "writes" answer
      onTable shell (intoRun <> " press:+") $
        assertEqual "the plain run's own token, drawn before a key is struck"
          "- " <=< textAt "dtext"
      onTable shell (intoRun <> " press:+ dpara:-_note press:Enter") $
        \answer ->
          assertEqual "and written as the item it was drawn as"
            [ "* TODO one\nlead in\n- alpha\n  more alpha\n  - nested\n- note\n\n"
              <> "- beta\n- gamma\n\n#+begin_quote\nquoted one\n\nquoted two\n"
              <> "#+end_quote\n\ntail para\n** two\nchild body\n" ]
            =<< traverse (textAt "body") =<< listAt "writes" answer

    -- `+' with no box open names no line, so the region's interior is reachable from `S-RET' alone.
  , testCase "a table's line keeps the composite's landing" $
      onTable shell "tabled press:Enter press:f press:n press:n press:n press:f press:+ dpara:note press:Enter" $
        \answer ->
          assertEqual "a pipe row is no prefix, so the paragraph goes past the table"
            [ "* TODO one\nlead in\n| a | b |\n|---+---|\n| 1 | 2 |\n| 3 | 4 |\n\n"
              <> "note\n\n- alpha\n- beta\n\ntail para\n** two\nchild body\n" ]
            =<< traverse (textAt "body") =<< listAt "writes" answer

  , testCase "+ inside a block lands under #+end_, never in the source" $ do
      onTable shell ("grain press:Enter press:f press:n press:n press:n press:n press:f press:+"
               <> " dpara:note press:Enter") $ \answer ->
        assertEqual "the quote is byte for byte what it was"
          [ "* TODO one\nlead in\n- alpha\n  more alpha\n  - nested\n\n- beta\n- gamma\n\n"
            <> "#+begin_quote\nquoted one\n\nquoted two\n#+end_quote\n\nnote\n\n"
            <> "tail para\n** two\nchild body\n" ]
          =<< traverse (textAt "body") =<< listAt "writes" answer
      onTable shell "grain press:Enter press:f press:n press:n press:n press:n press:f press:+" $
        echoIs "named by the block's own word"
               "+ \8594 org-insert-element (after the quote)"

  , testCase "+ over a child refuses and names the door" $
      insheet shell (ontoChild <> " press:+") $ \answer -> do
        assertEqual "no overlay" False =<< boolAt "dparaopen" answer
        assertEqual "nothing written" [] =<< textsAt "wroteAt" answer
        echoIs ""
          "+ \8594 org-insert-element (a child's body is its own \8212 RET opens it)" answer

  , testCase "an empty + adds nothing, and ESC undoes nothing" $ do
      insheet shell "press:f press:n press:n press:+ press:Enter" $ \answer -> do
        assertEqual "nothing written" [] =<< textsAt "wroteAt" answer
        echoIs "" "RET \8594 org-ctrl-c-ctrl-c (nothing added)" answer
      insheet shell "press:f press:n press:n press:+ dpara:__ press:Enter" $ \answer ->
        assertEqual "nor whitespace" [] =<< textsAt "wroteAt" answer
      insheet shell "press:f press:n press:n press:+ dpara:typed press:Escape" $ \answer -> do
        assertEqual "nothing written" [] =<< textsAt "wroteAt" answer
        assertEqual "and the pane is the document it was"
                    ["first para", "second para", "child body"]
          . partsOf "para" =<< docOf answer
        echoIs "" "ESC \8594 keyboard-quit (element unchanged)" answer

    -- The wall reads the TEXTAREA, which holds what the reader TYPED, and the lead never reaches it.
  , testCase "an empty + on an item writes no bare bullet" $
      onTable shell (intoRun <> " press:+ press:Enter") $
        \answer -> do
          assertEqual "nothing written" [] =<< textsAt "wroteAt" answer
          echoIs "" "RET \8594 org-ctrl-c-ctrl-c (nothing added)" answer

    -- MOVEMENT IS TWO AXES: `n'/`p' step SIBLINGS at the cursor's grain, `f'/`b' move the grain itself.
  , testCase "a list and a block are the whole thing, then their parts" $ do
      onTable shell "grain press:Enter" $ \answer -> do
        assertEqual "the walk, kind by kind, the lifted header leading it"
                    [ "head", "meta", "comp:properties:drawer", "para", "comp:list"
                    , "item", "item", "item", "item"
                    , "comp:quote", "item", "item", "para", "child", "para", "para:tail" ]
          =<< map head <$> docOf answer
        assertEqual "and the grain of each stop"
                    [ "element", "element", "composite", "element", "composite"
                    , "leaf", "leaf", "leaf", "leaf"
                    , "composite", "leaf", "leaf", "element", "element", "element"
                    , "element" ]
          =<< textsAt "dgrains" answer
        assertEqual "and who it hangs under"
                    [-1, -1, -1, -1, -1, 4, 5, 4, 4, -1, 9, 9, -1, -1, -1, -1]
          =<< flaggedAt "downers" answer

  , testCase "n skims the composites whole, and p is the skim reversed" $ do
      onTable shell "grain press:Enter press:f press:n press:n press:n press:n" $
        assertEqual "five down crosses the list whole to the quote" 9
          <=< pointOf
      onTable shell ("grain press:Enter press:f press:n press:n press:n"
               <> " press:n press:n press:n") $
        assertEqual "seven down is the tail child, the document skimmed" 13
          <=< pointOf
      onTable shell
             "grain press:Enter press:f press:n press:n press:n press:n press:p" $
        assertEqual "and p steps back over the list without entering it" 4
          <=< pointOf

    -- ONE ROW WALK, EVERY DIALECT: `C-n'/`C-p' ALIAS `n'/`p' in `rowStep', the
    -- page's one spelling of the walk, so the doc pane inherits the emacs habit
    -- with every other consumer of it.  ASSERTED AGAINST `n'/`p' rather than
    -- against a number of its own, or the two could part without a red run.
  , testCase "C-n and C-p walk the rows exactly as n and p do" $ do
      onTable shell
             "grain press:Enter press:f press:C-n press:C-n press:C-n press:C-n" $
        assertEqual "five down crosses the list whole to the quote" 9
          <=< pointOf
      onTable shell ("grain press:Enter press:f press:C-n press:C-n press:C-n"
                     <> " press:C-n press:C-p") $
        assertEqual "and back over the list without entering it" 4
          <=< pointOf
      onTable shell (intoRun <> " press:C-n press:C-n") $ \answer -> do
        assertEqual "inside a run the chord walks the leaves" 8 =<< pointOf answer
        -- ECHO PARITY: the step says nothing of its own under either spelling,
        -- so what stands is still the `f' that entered the run.
        echoIs "and adds no line of its own" "f → grain-finer (list 1/3)" answer
      onTable shell (intoRun <> " press:n press:n") $
        echoIs "which is exactly what n leaves standing" "f → grain-finer (list 1/3)"
      onTable shell (intoRun <> " press:C-p") $
        assertEqual "and C-p clamps at the first as p does" 5 <=< pointOf

    -- At the finest and at the floor the keys refuse with an echo; going OUT of the sheet stays DEL's.
  , testCase "f enters a composite's leaves, n/p walk them, b re-selects the whole" $ do
      onTable shell intoRun $ \answer -> do
        assertEqual "f lands on the first item" 5 =<< pointOf answer
        echoIs "and says where it is" "f → grain-finer (list 1/3)" answer
      onTable shell (intoRun <> " press:n press:n") $
        assertEqual "n walks the items" 8 <=< pointOf
      onTable shell (intoRun <> " press:n press:n press:n") $
        assertEqual "and clamps at the last rather than leaving the run" 8
          <=< pointOf
      onTable shell (intoRun <> " press:p") $
        assertEqual "p clamps at the first the same way" 5 <=< pointOf
      -- The walk steps past a sibling's descendants coming back exactly as it steps past its own going forward.
      onTable shell (intoRun <> " press:n press:p") $
        assertEqual "p from beta crosses the nested run to alpha" 5
          <=< pointOf
      onTable shell (intoRun <> " press:n press:b") $ \answer -> do
        assertEqual "b is the whole list again, from any item" 4
          =<< pointOf answer
        echoIs "named by its kind" "b → grain-broader (list)" answer
      onTable shell intoNestedRun $ \answer -> do
        assertEqual "the nested item is one rung down" 6 =<< pointOf answer
        echoIs "counted under its parent" "f → grain-finer (item 1/1)" answer
      onTable shell (intoNestedRun <> " press:n") $
        assertEqual "a run of one clamps at once" 6 <=< pointOf
      onTable shell (intoNestedRun <> " press:b") $ \answer -> do
        assertEqual "b climbs to the item" 5 =<< pointOf answer
        echoIs "named as one" "b → grain-broader (item)" answer
      onTable shell (intoRun <> " press:n press:f") $ \answer -> do
        assertEqual "nothing finer than a childless leaf" 7 =<< pointOf answer
        echoIs "and the key says so" "f → grain-finer (at the finest)" answer
      onTable shell "grain press:Enter press:b" $ \answer -> do
        assertEqual "the entry's own line is the floor" 0 =<< pointOf answer
        echoIs "b never closes" "b → grain-broader (the whole entry)" answer

    -- REVERSED EXPAND-REGION: `b' out of an element goes to THE ENTRY'S OWN LINE.
  , testCase "b out of an element marks the whole headline" $ do
      onTable shell "grain press:Enter press:f press:n press:n press:b" $ \answer -> do
        assertEqual "up from the lead paragraph" 0 =<< pointOf answer
        echoIs "" "b → grain-broader (the headline)" answer
      onTable shell (intoRun <> " press:b press:b") $ \answer ->
        assertEqual "the item, its list, then the entry" 0 =<< pointOf answer

    -- THREE DIALECTS, ONE AXIS: `l'/`h' and the horizontal arrows are ALIASES of `f'/`b'.
  , testCase "l/h and the horizontal arrows are f/b" $ do
      onTable shell "grain press:Enter press:f press:n press:n press:n press:l" $ \answer -> do
        assertEqual "l dives like f" 5 =<< pointOf answer
        echoIs "and speaks as the key pressed" "l → grain-finer (list 1/3)" answer
      onTable shell "grain press:Enter press:f press:n press:n press:n press:ArrowRight" $
        assertEqual "and so does the right arrow" 5 <=< pointOf
      onTable shell "grain press:Enter press:f press:h" $ \answer -> do
        assertEqual "h climbs like b" 0 =<< pointOf answer
        echoIs "" "h → grain-broader (the headline)" answer
      onTable shell "grain press:Enter press:f press:ArrowLeft" $
        assertEqual "and so does the left arrow" 0 <=< pointOf

    -- AN ORG TABLE IS THAT SAME SHAPE: one coarse stop, then its rows.  A LINE IS A LEAF, the `|---+---|' rule included.
  , keyed shell "a table is one stop, then its rows" "" "tabled press:Enter" $ \answer -> do
        assertEqual "the walk, kind by kind, over a MIXED body"
                    [ "head", "meta", "comp:properties:drawer", "para", "comp:table"
                    , "item", "item", "item", "item"
                    , "comp:list", "item", "item", "para", "child", "para", "para:tail" ]
          =<< map head <$> docOf answer
        assertEqual "and the grain of each stop"
                    [ "element", "element", "composite", "element", "composite"
                    , "leaf", "leaf", "leaf", "leaf"
                    , "composite", "leaf", "leaf", "element", "element", "element"
                    , "element" ]
          =<< textsAt "dgrains" answer
        assertEqual "and who each row hangs under"
                    [-1, -1, -1, -1, -1, 4, 4, 4, 4, -1, 9, 9, -1, -1, -1, -1]
          =<< flaggedAt "downers" answer
        assertEqual "the four rows, the rule among them"
                    [["| a | b |"], ["|---+---|"], ["| 1 | 2 |"], ["| 3 | 4 |"]]
          . map (drop 1) . take 4 . drop 5 =<< docOf answer

  , testCase "the table is one stop, and f walks its rows" $ do
      onTable shell "tabled press:Enter press:f press:n press:n press:n" $
        assertEqual "n from the lead-in meets the WHOLE table" 4
          <=< pointOf
      onTable shell "tabled press:Enter press:f press:n press:n press:n press:n" $
        assertEqual "and the next n crosses it whole to the list" 9
          <=< pointOf
      onTable shell "tabled press:Enter press:f press:n press:n press:n press:f" $
        assertEqual "f enters the first row" 5 <=< pointOf
      onTable shell "tabled press:Enter press:f press:n press:n press:n press:f press:n press:n press:n" $
        assertEqual "n walks the rows, the rule among them" 8 <=< pointOf
      onTable shell "tabled press:Enter press:f press:n press:n press:n press:f press:n press:b" $
        assertEqual "and b is the table whole again" 4 <=< pointOf

    -- A ROW EDIT IS A LINE SPLICE: the row remembers the line it came out of.
  , testCase "editing a table row splices that line and nothing else" $ do
      onTable shell
             ("tabled press:Enter press:f press:n press:n press:n press:f press:n press:n"
                <> " press:Enter dpara:~9~9~ press:C-x press:C-s") $ \answer ->
        assertEqual "the body with that row replaced and nothing else"
                    [tabledAfter "| 1 | 2 |" "|9|9|"]
          =<< traverse (textAt "body") =<< listAt "writes" answer
      onTable shell
             ("tabled press:Enter press:f press:n press:n press:n press:f press:n"
                <> " press:Enter dpara:~-+-~ press:C-x press:C-s") $ \answer ->
        assertEqual "the rule replaced, and the rows around it untouched"
                    [tabledAfter "|---+---|" "|-+-|"]
          =<< traverse (textAt "body") =<< listAt "writes" answer
      onTable shell "tabled press:Enter press:f press:n press:n press:n press:Enter" $
        assertEqual "the block whole, rule and all"
                    "| a | b |\n|---+---|\n| 1 | 2 |\n| 3 | 4 |" <=< textAt "dtext"

    -- ORG'S CHECKBOX on the stop under point, `[-]' checking the way org checks it.
  , testCase "SPC toggles a checkbox item and writes the box alone" $ do
      onTable shell
             (intoChecky <> " press:Space") $ \answer -> do
        assertEqual "the box checked, every other byte where it was"
                    ["* TODO one\n- [X] alpha\n- [X] beta\n- [-] gamma\n- delta\n** two\nchild body\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer
        echoIs "and the echo names org's command"
               "SPC → org-toggle-checkbox ([X])" answer
      onTable shell
             (intoChecky <> " press:n press:Space") $ \answer -> do
        assertEqual "a checked box clears"
                    ["* TODO one\n- [ ] alpha\n- [ ] beta\n- [-] gamma\n- delta\n** two\nchild body\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer
        echoIs "and says so" "SPC → org-toggle-checkbox ([ ])" answer
      onTable shell
             (intoChecky <> " press:n press:n press:Space") $ \answer ->
        assertEqual "the partial state checks, org's own rule"
                    ["* TODO one\n- [ ] alpha\n- [X] beta\n- [X] gamma\n- delta\n** two\nchild body\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer

    -- THE STORE LAGS THE WRITE IT ANSWERS FOR, so the reload a 200 fires DROPS any answer that is not the write's own receipt.
  , testCase "the toggle survives its own reload: the stale store answer is dropped" $ do
      onTable shell
             (intoChecky <> " press:Space") $ \answer -> do
        assertEqual "the box is flipped ON SCREEN as well as in the file"
                    ["- [X] alpha"]
          =<< (take 1 . partsOf "item" <$> docOf answer)
        assertEqual "and the sheet is synced, never conflict"
                    "synced" =<< textAt "state" answer
      onTable shell
             (intoChecky <> " press:Space press:Space") $ \answer -> do
        writes <- listAt "writes" answer
        assertEqual "two writes, the box back off"
                    [ "* TODO one\n- [X] alpha\n- [X] beta\n- [-] gamma\n- delta\n** two\nchild body\n"
                    , "* TODO one\n- [ ] alpha\n- [X] beta\n- [-] gamma\n- delta\n** two\nchild body\n" ]
          =<< traverse (textAt "body") writes
        assertEqual "the second under the first's receipt, not the store's stale pin"
                    ["d0", "w1"] =<< traverse (textAt "digest") writes
        assertEqual "and still synced" "synced" =<< textAt "state" answer

    -- AND A CELL EDIT RE-PINS OFF ITS OWN ANSWER: the sheet takes the digest off the command's 200.
  , testCase "a command from the sheet re-pins the digest its answer carries" $
      insheet shell ("press:Enter dtin:renamed press:Enter"
                 <> " press:f press:n press:n press:Enter dpara:rewritten press:C-x press:C-s") $
        \answer -> do
          assertEqual "the subtree write rides the command's receipt"
                      ["d1"] =<< traverse (textAt "digest") =<< listAt "writes" answer
          assertEqual "and lands synced, never conflict"
                      "synced" =<< textAt "state" answer

  , testCase "a paragraph commit keeps the pane's text over the stale re-read" $
      insheet shell "press:f press:n press:n press:Enter dpara:rewritten press:C-x press:C-s" $ \answer -> do
        assertEqual "the pane holds what was written"
                    ["rewritten"]
          =<< (take 1 . partsOf "para" <$> docOf answer)
        assertEqual "under the write's own receipt" "synced" =<< textAt "state" answer

  , testCase "SPC off a checkbox refuses, and C-c C-c is the same toggle" $ do
      onTable shell
             (intoChecky <> " press:n press:n press:n press:Space") $ \answer -> do
        assertEqual "a bare item takes no write" ([] :: [Value])
          =<< listAt "writes" answer
        echoIs "and the echo says why"
               "SPC → org-toggle-checkbox (no checkbox here)" answer
      onTable shell
             (intoChecky <> " press:C-c press:C-c") $ \answer -> do
        assertEqual "org's own key runs the same toggle"
                    ["* TODO one\n- [X] alpha\n- [X] beta\n- [-] gamma\n- delta\n** two\nchild body\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer
        echoIs "under its own name"
               "C-c C-c → org-ctrl-c-ctrl-c ([X])" answer

    -- ORG'S DISPLAY-VS-SOURCE MODEL, and NO SECOND PARSER: the shown text is the server's `desc' and the range its `span'.
  , keyed shell "a paragraph shows its links' descriptions, in link ink"
      "" "linky press:Enter" $ \answer -> do
        segs <- pairsAt "dsegs" answer
        assertEqual "the paragraph, cut into text and links"
          [ "dt:see ", "dl:alpha", "dt: and ", "dl:https://b.example/", "dt: here" ]
          (segs !! 3)
        -- `/links' dedups by the (target, shown) pair, so a bare URL written twice under one look is marked ONCE.
        assertEqual "the first spelling is marked, the second reads as text"
          [ "dt:bare ", "dl:https://c.example/", "dt: then https://c.example/ twice" ]
          (segs !! 4)
        assertEqual "and reads as the descriptions"
          ["para", "see alpha and https://b.example/ here"] . (!! 3) =<< docOf answer
  , keyed shell "RET opens the raw org, not what was shown"
      "" "linky press:Enter press:f press:n press:n press:Enter" $
        assertEqual "brackets and all"
          "see [[https://a.example/][alpha]] and [[https://b.example/]] here"
          <=< textAt "dtext"

    -- THE LINKS RIDE THE MATERIALIZE: compact from the FIRST frame, with no second fetch to bridge.
  , keyed shell "the links ride the materialize: compact on every fill, no second fetch"
      "" ("linky press:Enter " <> ontoChild <> " press:Enter"
            <> " press:Backspace") $ \answer -> do
        segs <- pairsAt "dsegs" answer
        assertEqual "the paragraph reads compact after the round trip"
          [ "dt:see ", "dl:alpha", "dt: and ", "dl:https://b.example/", "dt: here" ]
          (segs !! 3)
        assertEqual "and the title cell kept its mark"
                    ["dt:one ", "dl:the title link"] (head segs)
        assertEqual "and the sheet never asked /links" ([] :: [T.Text])
          =<< textsAt "linked" answer

    -- The server sends where the cell starts (`titleAt') because only it has that sub-span.
  , keyed shell "the headline's title cell shows its link too"
      "" "linky press:Enter" $ \answer -> do
        segs <- pairsAt "dsegs" answer
        assertEqual "the title, cut the same way"
                    ["dt:one ", "dl:the title link"] (head segs)
        -- EXACTLY ONE path writes the cell, and what it must never carry is the source spelling.
        raw <- textAt "dtitleraw" answer
        assertEqual "the cell reads as the display alone"
                    "one the title link" raw

    -- A LINK IS NOT A STOP and binds no mouse: `o' is the opener.
  , keyed shell "links are drawn, and are no stop" "" "linky press:Enter press:f press:n press:n" $ \answer -> do
        assertEqual "three stops down is the paragraph, links and all" 3
          =<< pointOf answer
        assertEqual "nothing was opened by drawing them" [] =<< openedOf answer

    -- `o' OPENS A ROW'S REACH: a headline answers for the whole subtree
    -- under it -- the root's reach the entry -- where its own SPAN is one line.
  , keyed shell "o on the root gathers the whole entry's links" ""
      "linky press:Enter press:o" $ \answer -> do
        assertEqual "the popup, over every link the subtree holds" "on"
          =<< textAt "popup" answer
        assertEqual "titled by the count" "open · 4 links" =<< textAt "lhead" answer

  , keyed shell "o on a child answers for the child's own reach" ""
      ("linky press:Enter " <> ontoChild <> " press:o") $ \answer -> do
        assertEqual "no popup over an empty reach" "" =<< textAt "popup" answer
        echoIs "the child's subtree holds no links, and the door says so"
          "RET → org-glance-overview:open (no links)" answer

    -- `o' SCOPES TO THE STOP: every lifted region sits above the paragraphs, so body lines and file lines differ by one constant.
  , testCase "o asks over the stop the cursor is on" $ do
      onTable shell
             "grain grainlinks press:Enter press:f press:n press:n press:n press:f press:o" $
        \answer -> do
          assertEqual "the item's own link, opened"
                      [("https://alpha.example/", "_blank", "noopener")]
            =<< openedOf answer
          assertEqual "and no popup was needed" "" =<< textAt "popup" answer
      onTable shell
             "grain grainlinks press:Enter press:f press:n press:n press:n press:o" $ \answer -> do
        assertEqual "nothing opened outright" [] =<< openedOf answer
        assertEqual "both links are listed" ["in alpha", "in beta"]
          =<< map (!! 1) <$> pairsAt "llinks" answer
      onTable shell "grain grainlinks press:Enter press:f press:n press:n press:o" $
        -- The pill names the COMMAND, and the sequence is the keymap row's own spelling of it.
        \answer -> assertEqual "the lead-in reaches neither"
                               "RET → org-glance-overview:open (no links)"
                     =<< textAt "echo" answer

    -- ONE BLANK LINE STAYS IN A LIST, which is org's rule and the corpus's.
  , keyed shell "a blank line and a nested item stay inside their list"
      "" intoRun $ \answer -> do
        assertEqual "four stops: alpha's head, the nested run, beta, gamma"
                    ["- alpha\n  more alpha", "  - nested", "- beta", "- gamma"]
          =<< partsOf "item" . take 9 <$> docOf answer
        assertEqual "the cursor is on the first of them" 5 =<< pointOf answer

    -- WHAT NO LEAF CLAIMS IS STILL DRAWN, and drawn inert: the lens's one-owner-per-byte rule, one grain down.
  , keyed shell "a block's delimiters are drawn, and are no stop"
      "" "grain press:Enter" $ \answer -> do
        rows <- docOf answer
        assertEqual "the composite shows the delimiters and nothing else"
                    ["#+begin_quote\n\n#+end_quote"] (partsOf "comp:quote" rows)
        assertEqual "and its paragraphs are the stops inside it"
                    ["quoted one", "quoted two"] (partsOf "item" (drop 9 rows))

    -- RET IS PURE EDIT AT EITHER GRAIN, and each commit splices exactly the range its stop covers.
  , testCase "RET edits a leaf's own lines, and splices only those" $ do
      onTable shell (intoRun <> " press:Enter") $
        \answer -> assertEqual "the item's OWN lines, the nested one being its own stop"
                               "- alpha\n  more alpha"
                     =<< textAt "dtext" answer
      onTable shell
             (intoRun <> " press:Enter dpara:-_ALPHA press:C-x press:C-s") $ \answer -> do
        body <- traverse (textAt "body") =<< listAt "writes" answer
        -- THE NESTED ITEM SURVIVES ITS PARENT'S EDIT: it is a stop of its own,
        -- so the parent's commit replaces the parent's lines and no more.
        assertEqual "the item's lines, and every other byte where it was"
          [ "* TODO one\nlead in\n- ALPHA\n  - nested\n\n- beta\n- gamma\n\n#+begin_quote\n"
            <> "quoted one\n\nquoted two\n#+end_quote\n\ntail para\n** two\nchild body\n" ]
          body
  , testCase "RET at the whole list edits the whole list" $ do
      onTable shell "grain press:Enter press:f press:n press:n press:n press:Enter" $
        \answer -> assertEqual "every line the composite covers"
                               "- alpha\n  more alpha\n  - nested\n\n- beta\n- gamma"
                     =<< textAt "dtext" answer
      onTable shell
             ("grain press:Enter press:f press:n press:n press:n press:Enter dpara:-_one|-_two"
              <> " press:C-x press:C-s") $ \answer -> do
        body <- traverse (textAt "body") =<< listAt "writes" answer
        assertEqual "the list's whole range replaced, and nothing beyond it"
          [ "* TODO one\nlead in\n- one\n- two\n\n#+begin_quote\n"
            <> "quoted one\n\nquoted two\n#+end_quote\n\ntail para\n** two\nchild body\n" ]
          body

    -- `d' FLAGS WHATEVER THE STOP IS, which is why the grain needed no key of its own.
  , testCase "d flags one item, or the whole list" $ do
      onTable shell (intoRun <> " press:d") $
        assertEqual "the item alone" [5] <=< flaggedOf
      onTable shell "grain press:Enter press:f press:n press:n press:n press:d" $
        assertEqual "or the composite alone" [4] <=< flaggedOf
      onTable shell "grain press:Enter press:f press:n press:n press:n press:d press:d" $
        \answer -> do
          body <- traverse (textAt "body") =<< listAt "writes" answer
          assertEqual "the whole list is gone, the rest untouched"
            [ "* TODO one\nlead in\n#+begin_quote\nquoted one\n\nquoted two\n"
              <> "#+end_quote\n\ntail para\n** two\nchild body\n" ] body

    -- ESC over an open element is the ELEMENT's; the next one reaches the sheet's own ladder.
  , testCase "ESC puts an open paragraph back, and the next one closes the sheet" $ do
      insheet shell "press:f press:n press:n press:Enter dpara:rewritten press:Escape" $
        \answer -> do
          assertEqual "the overlay is gone" False =<< boolAt "dparaopen" answer
          assertEqual "the sheet is still up" "on" =<< textAt "modal" answer
          assertEqual "with nothing written" ([] :: [Value]) =<< listAt "writes" answer
          echoIs "and it said so" "ESC → keyboard-quit (element unchanged)" answer
      insheet shell "press:f press:n press:n press:Enter press:Escape press:Escape" $
        assertEqual "the second one is the sheet's" "" <=< textAt "modal"

  , testCase "d flags a paragraph and d again splices it out of the body" $ do
      insheet shell "press:f press:n press:n press:d" $ \answer -> do
        assertEqual "the block wears the flag" [3] =<< flaggedOf answer
        assertEqual "and nothing is written yet" ([] :: [Value])
          =<< listAt "writes" answer
        echoIs "the pill says what the second press will do"
          "d → delete-flag (d again deletes)" answer
      insheet shell "press:f press:n press:n press:d press:d" $ \answer -> do
        assertEqual "the body with the block and its blank line gone"
                    ["* TODO one\nsecond para\n** two\nchild body\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer
        echoIs "and the pill counted the set" "D → org-delete-element (1 flagged taken)" answer
      -- A HELD `d' must not flag and delete from one press.
      insheet shell "press:f press:n press:n press:d repeat:d" $ \answer -> do
        assertEqual "the flag is still there" [3] =<< flaggedOf answer
        assertEqual "and nothing was written" ([] :: [Value]) =<< listAt "writes" answer

    -- AND `x' IS THE SAME GESTURE HERE: `flagPress' is the one door for the sheet's four surfaces.
  , testCase "x over the document asks before it splices" $ do
      insheet shell "press:f press:n press:n press:d press:x" $ \answer -> do
        assertEqual "nothing written on the press alone" ([] :: [Value])
          =<< listAt "writes" answer
        assertEqual "the question is up" "on" =<< textAt "prompt" answer
        assertEqual "naming the act and how many" "delete · 1 flagged"
          =<< textAt "phead" answer
      insheet shell "press:f press:n press:n press:d press:x type:yes press:Enter" $ \answer ->
        assertEqual "and the word splices it out"
                    ["* TODO one\nsecond para\n** two\nchild body\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer
      insheet shell "press:f press:n press:n press:d press:x type:no press:Enter" $ \answer -> do
        assertEqual "anything else writes nothing" ([] :: [Value])
          =<< listAt "writes" answer
        assertEqual "and the flag stands" [3] =<< flaggedOf answer
      insheet shell "press:f press:x" $ \answer -> do
        assertEqual "nothing flagged is nothing to do" ([] :: [Value])
          =<< listAt "writes" answer
        echoIs "" "x → dired-do-flagged-delete (no deletions requested)" answer

  , keyed shell "a headline is not deleted from the document, and says so"
      "Enter" (ontoChild <> " press:D") $ \answer -> do
        assertEqual "nothing written" ([] :: [Value]) =<< listAt "writes" answer
        assertEqual "the log says why"
          (Just "a headline is not deleted from the sheet — this writes elements only")
          =<< lastLog answer

    -- `t' AND `:' WORK AT THE ELEMENT, which is what makes an ABSENT part settable.
  , testCase "t and : fire from the element, which is where the headline is one stop" $ do
      insheet shell "press:t" $ \answer -> do
        assertEqual "the palette is up" "on" =<< textAt "prompt" answer
        assertEqual "over the row the sheet is on" ["/keywords?ids=r1"]
          =<< textsAt "resolved" answer
      insheet shell "press::" $ \answer -> do
        assertEqual "the popup is up" "on" =<< textAt "tagpop" answer
        assertEqual "named for the entry" "tags · one" =<< textAt "thead" answer
      insheet shell "press:f press:t" $ \answer -> do
        assertEqual "nothing raised" "" =<< textAt "prompt" answer
        echoIs "and it said where to stand" "the headline line takes this — n/p to it" answer

    -- The sheet is the FLOOR of the surface stack, so its listener declines while anything above it is up.
  , keyed shell "a palette raised from the document has the letters, and it alone"
      "Enter" "press:t press:d" $ \answer -> do
        assertEqual "the letter committed" [("set-state", ["r1"])] =<< postedOf answer
        assertEqual "and flagged nothing on the way" ([] :: [Int])
          =<< flaggedOf answer

  , keyed shell "and the tags popup raised from it takes its own d, not the document's"
      "Enter" "press:: press:d" $ \answer -> do
        assertEqual "the popup is up" "on" =<< textAt "tagpop" answer
        assertEqual "the tag wears the flag" ["web"] =<< textsAt "tflagged" answer
        assertEqual "and no element of the document does" ([] :: [Int])
          =<< flaggedOf answer

  , keyed shell "and a palette raised from the TABLE still has them" "t" "press:d" $ \answer -> do
        assertEqual "the letter committed" [("set-state", ["r1"])] =<< postedOf answer
        assertEqual "the sheet never opened" "" =<< textAt "modal" answer

    -- THE TITLE EDITS IN PLACE AND THE HEADLINE KEEPS ITS DRESS: one field over the title text alone.
  , testCase "RET on the headline line itself opens the title" $ do
      insheet shell "press:Enter" $ \answer -> do
        assertEqual "the overlay is open" True =<< boolAt "dopen" answer
        assertEqual "and holds the title" "one" =<< textAt "dtin" answer
        assertEqual "with the focus in it" "dtin" =<< textAt "focus" answer
      insheet shell
             "press:Enter dtin:renamed press:Enter" $ \answer -> do
        assertEqual "one set-title over this row"
                    [("set-title", ["r1"])] =<< postedOf answer
        assertEqual "and the log named both ends"
                    (Just "headline \"one\" retitled \"renamed\"") =<< lastLog answer
        assertEqual "nothing went through the lens" ([] :: [Value])
          =<< listAt "writes" answer

    -- TWO KEYS COMMIT AN OPEN ELEMENT: `C-c C-c' stops where the element does, `C-x C-s' keeps the BUFFER's half.
  , testCase "C-c C-c commits the open element, where C-x C-s does" $ do
      insheet shell "press:f press:n press:n press:Enter dpara:rewritten press:C-c press:C-c" $
        \answer ->
          assertEqual "the block replaced and nothing else"
                      ["* TODO one\nrewritten\n\nsecond para\n** two\nchild body\n"]
            =<< traverse (textAt "body") =<< listAt "writes" answer
      insheet shell
             "press:Enter dtin:renamed press:C-c press:C-c" $
        \answer -> do
          assertEqual "one set-title over this row"
                      [("set-title", ["r1"])] =<< postedOf answer
          assertEqual "the overlay is closed" False =<< boolAt "dopen" answer
      insheet shell
             "press:f press:n press:n press:Enter press:C-c press:C-c" $
        echoIs "org's own name, on an element nothing changed in"
          "C-c C-c → org-ctrl-c-ctrl-c (paragraph unchanged)"
      insheet shell "press:f press:n press:n press:Enter press:C-x press:C-s" $
        echoIs "and the buffer's name where that key ran"
          "C-x C-s → save-buffer (paragraph unchanged)"
      insheet shell "press:C-c press:C-c" $ \answer -> do
        assertEqual "nothing was written" ([] :: [Value]) =<< listAt "writes" answer
        echoIs "and it said so" "C-c C-c → org-ctrl-c-ctrl-c (nothing open here)" answer

    -- EVERY COMMIT RE-READS THE ENTRY IT WROTE, and the entry the sheet stands on rather than the row.
  , testCase "a commit re-materializes the entry it wrote" $ do
      insheet shell
             "press:f press:n press:n press:Enter dpara:rewritten press:C-x press:C-s" $
        assertEqual "opened once, and read again on the answer"
                    ["r1", "r1"] <=< textsAt "readAt"
      insheet shell
             (ontoChild <> " press:Enter press:f press:n press:Enter"
                <> " dpara:reworded press:C-x press:C-s") $
        assertEqual "the row, the child, and the child again"
                    ["r1", "r1#0", "r1#0"] <=< textsAt "readAt"

    -- A STATE SET FROM THE SHEET LANDS ON SCREEN: the write and the re-read both happen off one press.
  , testCase "a state set from the sheet writes and re-reads the entry" $
      insheet shell "press:t press:t frame:upsert=r1" $ \answer -> do
        assertEqual "one set-state over this row"
                    [("set-state", ["r1"])] =<< postedOf answer
        assertEqual "opened once, then re-read when the watch says so"
                    ["r1", "r1"] =<< textsAt "readAt" answer
        assertEqual "and the palette is gone" "" =<< textAt "prompt" answer

    -- A `/command' NEVER WRITES THE STORE — the watch does, a debounce later.
  , testCase "a socket frame naming this row re-reads the sheet" $ do
      insheet shell "frame:upsert=r1" $
        assertEqual "opened once, then re-read on the frame"
                    ["r1", "r1"] <=< textsAt "readAt"
      -- Not while an edit is open: a re-read would pull the model out from under the fields.
      insheet shell "press:f press:n press:n press:Enter frame:upsert=r1" $
        assertEqual "left alone under an open element" ["r1"] <=< textsAt "readAt"
      -- Nor over an open PAIR line: it is the same overlay, over a synthesized row.
      insheet shell "press:f press:n press:f press:Enter frame:upsert=r1" $
        assertEqual "left alone under an open pair line" ["r1"] <=< textsAt "readAt"
      -- A COMMITTED pair edit wrote at once; until the store catches its receipt
      -- up the sheet is DIRTY, and the frame is left to the write's own retry.
      insheet shell
             ("press:f press:n press:f press:Enter dpara::EFFORT:_0:45"
                <> " press:Enter frame:upsert=r1") $
        \answer -> do
          assertEqual "the open, then the write's own re-read and no third"
                      ["r1", "r1"] =<< textsAt "readAt" answer
          assertBool "with the edit still on screen"
            . elem ["meta", ":EFFORT: 0:45"] =<< pairsAt "doc" answer
      insheet shell "frame:upsert=r2" $
        assertEqual "another row is not this one" ["r1"] <=< textsAt "readAt"

    -- THE RING REACHES THE DOCUMENT, read off the ANSWER's own cells rather than a table row.
  , testCase "S-up cycles the priority of the entry the sheet is on" $ do
      insheet shell "press:S-ArrowUp" $ \answer -> do
        assertEqual "one command over this row"
                    [("set-priority", ["r1"])] =<< postedOf answer
        assertEqual "the fixture entry has none, so it lands on C"
                    [Just "C"] =<< prioritiesOf answer
        echoIs "and the pill names the key that ran it" "S-<up> → priority-up ([#C] · 1)" answer
      insheet shell
             (ontoChild <> " press:Enter press:S-ArrowUp") $ \answer -> do
        assertEqual "nothing posted" ([] :: [Value]) =<< listAt "commands" answer
        echoIs "and it said which key climbs out"
          "a child is not settable yet — DEL opens its parent" answer

    -- This listener runs AHEAD of the dispatch, so the map's ONCE list can never reach a key of its own.
  , testCase "a held S-up cycles once" $ do
      insheet shell
             "press:S-ArrowUp repeat:S-ArrowUp repeat:S-ArrowUp" $ \answer -> do
        assertEqual "one command, however long the key is held"
                    [("set-priority", ["r1"])] =<< postedOf answer
      insheet shell "press:f repeat:n repeat:n" $
        assertEqual "and a held movement key still walks" 3 <=< intAt "dat"

    -- A CHILD IS READ-ONLY: no row id, so no `/command' can address it.
  , testCase "a child is not settable yet, and the echo says so" $ do
      insheet shell
             (ontoChild <> " press:Enter press:Enter") $ \answer -> do
        assertEqual "nothing posted" ([] :: [Value]) =<< listAt "commands" answer
        echoIs "and the pill named the way out"
          "RET → a child's title is not settable yet — DEL opens its parent" answer
      insheet shell (ontoChild <> " press:Enter press:t") $
        \answer -> do
          assertEqual "nothing raised" "" =<< textAt "prompt" answer
          echoIs "and it said which key climbs out"
            "a child is not settable yet — DEL opens its parent" answer

    -- A CHILD'S OWN PARTS are editable through the lens that materialized it, at that entry's extent.
  , keyed shell "a child's paragraph writes the child's own extent"
      "Enter" (ontoChild <> " press:Enter press:f press:n press:Enter"
                <> " dpara:reworded press:C-x press:C-s") $ \answer -> do
        assertEqual "aimed at the entry, not the row" ["r1#0"]
          =<< textsAt "wroteAt" answer
        assertEqual "carrying the child's own body"
                    ["** two :web:\nreworded\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer

  , atBoot sheet "the identity property never reaches the pane" $ \answer -> do
        rows <- pairsAt "dprops" answer
        assertEqual "no pair names it" [] [ r | r <- rows, take 1 r == ["ORG_GLANCE_ID"] ]
        drawn <- docOf answer
        assertEqual "and no drawn line spells it" []
                    [ r | r <- drawn, any ("ORG_GLANCE_ID" `T.isInfixOf`) r ]

    -- TAB FOLDS, as it does in org: the drawer starts as one line, and the model says which way it went.
  , testCase "TAB folds and opens the drawer, and f steps into a folded one" $ do
      insheet shell "press:f press:n press:Tab" $ \answer -> do
        assertEqual "the drawer opened, its pairs drawn as leaves"
                    [ ["comp:properties:drawer", ":PROPERTIES:", ":END:"]
                    , ["meta", ":EFFORT: 0:30"] ]
          =<< take 2 . drop 2 <$> docOf answer
        echoIs "named org's own way" "TAB → org-cycle (properties open)" answer
      insheet shell "press:f press:n press:Tab press:Tab" $ \answer -> do
        assertEqual "TAB again is the one folded line, org's ellipsis on it"
                    ["comp:properties:drawer", ":PROPERTIES: \8230"]
          =<< (!! 2) <$> docOf answer
        echoIs "" "TAB → org-cycle (properties folded)" answer
      insheet shell "press:f press:n press:f" $ \answer -> do
        assertEqual "f into the folded drawer opens it and lands on the pair" 3
          =<< pointOf answer
        echoIs "counted like any composite" "f → grain-finer (properties 1/1)" answer
      -- FROM INSIDE, TAB reaches the nearest foldable owner: the drawer folds over point.
      insheet shell "press:f press:n press:f press:Tab" $ \answer -> do
        assertEqual "folded from the pair, point on the frame" 2 =<< pointOf answer
        echoIs "" "TAB → org-cycle (properties folded)" answer
      insheet shell "press:Tab" $
        echoIs "nowhere foldable says so" "TAB → nothing folds here"

  , testCase "RET on a pair opens its own line as text" $ do
      insheet shell "press:f press:n press:f press:Enter" $ \answer -> do
        assertEqual "a pair opens as its `:KEY: value' line"
                    ":EFFORT: 0:30" =<< textAt "dtext" answer
        assertEqual "over the pair's own row" 3 =<< intAt "dat" answer
      insheet shell "press:f press:n press:Enter" $ \answer -> do
        assertEqual "the drawer's own line is its frame, so RET declines"
                    False =<< boolAt "dparaopen" answer
        assertEqual "and the fold is left where it stood, folded"
                    ["comp:properties:drawer", ":PROPERTIES: \8230"]
          =<< (!! 2) <$> docOf answer
        echoIs "and names the two doors" "RET → f reaches the rows inside — TAB folds" answer

    -- THE PLANNING LINE IS A LINE OF ENTRIES, and the frame's rule reaches it:
    -- what RET edits is an entry inside, so over the WHOLE line RET is reserved
    -- and says where the entries are.  The one door that could clear every entry
    -- at a stroke went with it; the widget clears them one at a time.
  , testCase "RET on the whole planning line is inert, and names the way in" $
      insheet shell "press:f press:Enter" $ \answer -> do
        assertEqual "no text block over it" False =<< boolAt "dparaopen" answer
        assertEqual "and no widget either" False =<< boolAt "ddateopen" answer
        assertEqual "nothing written" ([] :: [Value]) =<< listAt "writes" answer
        echoIs "and it names the grain below it"
               "RET → f reaches the entries — RET on one edits it" answer

    -- THE PLANNING LINE'S FINER GRAIN IS THE ENTRIES IT DRAWS.  It holds no rows,
    -- so the walk inside it is over the line's OWN LIST, org's order, counted the
    -- way a run's leaves are counted; point never leaves the row, the ENTRY being
    -- the sub-row grain.  `b' steps back along it and off the FIRST entry to the
    -- whole line, where the row grain answers again; the LAST entry clamps in the
    -- house's own word.
  , testCase "f walks the planning line's entries, and b comes back out" $ do
      -- ONE WALK IN AND BACK OUT, read off the ECHO HISTORY: the pill is
      -- last-writer-wins, so the words it was SET to are the steps in order.
      onTable shell ("planned press:Enter press:f press:f press:f press:f press:f"
                     <> " press:b press:b press:b") $ \answer -> do
        steps <- filter ("grain-" `T.isInfixOf`) <$> textsAt "echoes" answer
        assertEqual "every step of the walk, in the order it was taken"
                    [ "f → grain-finer (the body)"
                    , "f → grain-finer (SCHEDULED 1/3)"
                      -- Org's own order, its third word a stop like any other.
                    , "f → grain-finer (DEADLINE 2/3)"
                    , "f → grain-finer (CLOSED 3/3)"
                      -- The last one clamps, spoken and standing still.
                    , "f → grain-finer (at the finest)"
                    , "b → grain-broader (DEADLINE 2/3)"
                    , "b → grain-broader (SCHEDULED 1/3)"
                      -- And off the first entry to the whole line.
                    , "b → grain-broader (the planning line)" ]
                    steps
        -- POINT NEVER LEFT THE ROW: the entries are a sub-row grain, so the line
        -- the walk came back out to is the row it walked inside all along.
        assertEqual "which is the step the row grain then answers" 1 =<< pointOf answer
      -- A ROW STEP LEAVES THE ENTRIES: the sub-row grain is this line's own and
      -- does not ride to another row, so `b' after one is the row's, not an entry's.
      onTable shell "planned press:Enter press:f press:f press:n press:p press:b" $
        echoIs "the entry did not ride the row step"
               "b → grain-broader (the headline)"

    -- A COMMITTED PAIR EDIT IS A WRITE: the cargo rides the port, body and lists together.
  , keyed shell "a committed pair edit writes at once, the whole header on it"
      "Enter" "press:f press:n press:f press:Enter dpara::EFFORT:_0:45 press:Enter" $
        \answer -> do
          assertEqual "one write" [fixtureBody] =<< traverse (textAt "body")
                                                =<< listAt "writes" answer
          assertEqual "carrying the drawer, edit and all"
                      [[["EFFORT", "0:45"]]]
                      =<< wroteAt "properties" answer
          assertEqual "and the planning entries it has"
                      [[["SCHEDULED", sheetStamp]]]
                      =<< wroteAt "planning" answer
          assertEqual "and it landed" "synced" =<< textAt "state" answer
          headerIs "the mirrors moved with it"
                   [["EFFORT", "0:45"]] [["SCHEDULED", sheetStamp]] answer
          assertEqual "the pane draws the edit" ["meta", ":EFFORT: 0:45"]
            =<< (!! 3) <$> docOf answer
          assertEqual "the fields are gone" "" =<< textAt "focus" answer
          assertEqual "and the cursor stayed on the pair" 3 =<< intAt "dat" answer

    -- A TYPO NEVER WRITES PROSE INTO THE DRAWER: a line opening no `:KEY:' is refused.
  , keyed shell "a pair edited to no `:KEY: value' line is refused"
      "Enter" "press:f press:n press:f press:Enter dpara:nonsense press:Enter" $
        \answer -> do
          assertEqual "nothing written" ([] :: [Value]) =<< listAt "writes" answer
          assertEqual "and the pair stands as it was" ["meta", ":EFFORT: 0:30"]
            =<< (!! 3) <$> docOf answer

    -- `+' IS THE ADD AFFORDANCE AND THE WHOLE OF IT: the pair is typed INLINE, in
    -- a row drawn where it will stand, dressed as the line it will become.
  , testCase "+ on a meta row types the pair inline, and it lands whole" $ do
      insheet shell "press:f press:+" $ \answer -> do
        assertEqual "the two fields are up" True =<< boolAt "dpairopen" answer
        assertEqual "and no modal asked for either half" "" =<< textAt "prompt" answer
        assertEqual "the key half holds the focus" "dkey" =<< textAt "focus" answer
        assertEqual "over a row drawn at the drawer's end, which opened for it"
                    ["draft:meta", ""] =<< (!! 4) <$> docOf answer
        assertEqual "and point is on that row" 4 =<< intAt "dat" answer
        assertEqual "nothing is written while it is typed" ([] :: [Value])
          =<< listAt "writes" answer
        echoIs "the echo carries the foot a chromeless box has no room for"
               ("+ → org-set-property (a key, then its value"
                  <> " — RET applies · ESC cancels)") answer
      -- `:' HANDS THE KEY OVER TO ITS VALUE, org's own muscle, and RET applies.
      insheet shell "press:f press:+ dkey:OWNER press:: dval:ada press:Enter" $
        \answer -> do
          assertEqual "the pair arrived whole and the write followed at once"
                      [[["EFFORT", "0:30"], ["OWNER", "ada"]]]
            =<< wroteAt "properties" answer
          assertEqual "the drawer opened on the new pair" ["meta", ":OWNER: ada"]
            =<< (!! 4) <$> docOf answer
          assertEqual "with the cursor on it" 4 =<< intAt "dat" answer
          assertEqual "and the fields went with the draft row" False
            =<< boolAt "dpairopen" answer
          echoIs "and the echo speaks the line"
                 "RET → org-set-property (:OWNER: ada)" answer
      -- TAB CARRIES THE FORM the way RET does, out of either half.
      insheet shell "press:f press:+ dkey:OWNER press:Tab dval:ada press:Tab" $
        \answer -> do
          assertEqual "the same pair, the same write"
                      [[["EFFORT", "0:30"], ["OWNER", "ada"]]]
            =<< wroteAt "properties" answer
          echoIs "" "TAB → org-set-property (:OWNER: ada)" answer

      -- EVERY REFUSAL IS THE BOX'S OWN AND IT STAYS UP BEHIND EACH, so what was
      -- typed is still there to be fixed.  RET in the KEY hands over, so an
      -- empty pair is refused at the value, which is where the apply is.
      insheet shell "press:f press:+ press:Enter press:Enter" $ \answer -> do
        assertEqual "an empty key writes nothing" ([] :: [Value])
          =<< listAt "writes" answer
        assertEqual "and the box stands" True =<< boolAt "dpairopen" answer
        echoIs "" "RET → org-set-property (a key is required)" answer
      insheet shell "press:f press:+ dkey:OWNER press:: press:Enter" $ \answer -> do
        assertEqual "an empty value writes nothing either" ([] :: [Value])
          =<< listAt "writes" answer
        assertEqual "and the key typed is there to finish" "OWNER"
          =<< textAt "dkey" answer
        echoIs "" "RET → org-set-property (a value is required)" answer
      -- THE DRAWER'S OWN FRAME WORDS ARE NO KEY: written as one, `:END:' would
      -- close the drawer here and everything under it would fall out of it.
      insheet shell "press:f press:+ dkey:END press:: dval:x press:Enter" $
        \answer -> do
          assertEqual "a reserved key writes nothing" ([] :: [Value])
            =<< listAt "writes" answer
          assertEqual "and the box stands" True =<< boolAt "dpairopen" answer
          echoIs "" ("RET → org-set-property (:END: frames the drawer"
                       <> " — writing it would end the drawer here)") answer
      -- AND THE STORE'S OWN PAIR IS NEVER DRAWN, so typing one would forge the
      -- identity the headline is found and linked by.
      insheet shell "press:f press:+ dkey:ORG_GLANCE_ID press:: dval:r2 press:Enter" $
        \answer -> do
          assertEqual "a hidden key writes nothing" ([] :: [Value])
            =<< listAt "writes" answer
          assertEqual "and the box stands" True =<< boolAt "dpairopen" answer
          echoIs "" ("RET → org-set-property (:ORG_GLANCE_ID: is the store's own"
                       <> " — this would forge the headline's identity)") answer

      -- THE ESCAPE IS FROM THE EDIT: the box goes, the drawn row with it, and
      -- point is back on the stop `+' was pressed over.
      insheet shell "press:f press:+ dkey:OWNER press:: dval:ada press:Escape" $
        \answer -> do
          assertEqual "nothing written" ([] :: [Value]) =<< listAt "writes" answer
          assertEqual "the fields are gone" False =<< boolAt "dpairopen" answer
          assertEqual "the drawn row with them" ["para", "first para"]
            =<< (!! 4) <$> docOf answer
          assertEqual "and the drawer holds the pairs it held"
                      ["meta", ":EFFORT: 0:30"] =<< (!! 3) <$> docOf answer
          assertEqual "point back on the row the key was pressed over" 1
            =<< intAt "dat" answer
          echoIs "" "ESC → keyboard-quit (the drawer unchanged)" answer

    -- A KEY THAT FOLDS TO ONE OF ORG'S THREE PLANNING WORDS IS NO PROPERTY: it
    -- is a planning entry wearing a property's clothes, and the box routes it
    -- to the planning line, upcased, with the drawer never seeing it.
  , testCase "a pair keyed for planning is written to the planning line" $ do
      insheet shell
             "press:f press:+ dkey:SCHEDULED press:: dval:<2026-09-01_Tue> press:Enter" $
        \answer -> do
          assertEqual "the entry replaced on the planning line"
                      [[["SCHEDULED", "<2026-09-01 Tue>"]]] =<< wroteAt "planning" answer
          assertEqual "and the drawer written exactly as it stood"
                      [[["EFFORT", "0:30"]]] =<< wroteAt "properties" answer
          assertEqual "the two meta rows the pane is left with"
                      ["SCHEDULED: <2026-09-01 Tue>", ":EFFORT: 0:30"]
            . partsOf "meta" =<< docOf answer
          assertEqual "with the cursor on the line it landed on" 1 =<< intAt "dat" answer
          assertEqual "and the fields went with the draft row" False
            =<< boolAt "dpairopen" answer
          echoIs "the echo names the entry and where it went"
                 ("RET → org-set-property (SCHEDULED: <2026-09-01 Tue>"
                    <> " — the planning line)") answer
      -- THE CASE IS THE TYPIST'S and the write's is org's, so a key case-folds
      -- to the word and lands upcased.  A word the line does not spell is ADDED.
      insheet shell
             "press:f press:+ dkey:deadline press:: dval:<2026-09-05_Sat> press:Enter" $
        \answer -> do
          assertEqual "the line's own entry kept, the fresh one at its end"
                      [[["SCHEDULED", sheetStamp], ["DEADLINE", "<2026-09-05 Sat>"]]]
            =<< wroteAt "planning" answer
          assertEqual "the drawer untouched" [[["EFFORT", "0:30"]]]
            =<< wroteAt "properties" answer
          assertEqual "and the line spells both"
                      ["SCHEDULED: " <> sheetStamp <> " DEADLINE: <2026-09-05 Sat>"
                      , ":EFFORT: 0:30" ]
            . partsOf "meta" =<< docOf answer
          echoIs "" ("RET → org-set-property (DEADLINE: <2026-09-05 Sat>"
                       <> " — the planning line)") answer
      -- CLOSED IS ONE OF THE THREE, and THE BRACKET KIND IS THE TYPIST'S: the
      -- wall asks whether org reads the value back, never which brackets it wears.
      insheet shell
             ("press:f press:+ dkey:Closed press::"
                <> " dval:[2026-09-02_Wed_18:30] press:Enter") $
        \answer -> do
          assertEqual "the inactive stamp the typist spelled"
                      [[["SCHEDULED", sheetStamp], ["CLOSED", "[2026-09-02 Wed 18:30]"]]]
            =<< wroteAt "planning" answer
          assertEqual "and no pair joined the drawer" [[["EFFORT", "0:30"]]]
            =<< wroteAt "properties" answer
      -- A RANGE IS ONE VALUE, and org's own `--' joins it.
      insheet shell
             ("press:f press:+ dkey:SCHEDULED press::"
                <> " dval:<2026-09-01_Tue>--<2026-09-05_Sat> press:Enter") $
        assertEqual "the range written whole"
                    [[["SCHEDULED", "<2026-09-01 Tue>--<2026-09-05 Sat>"]]]
          <=< wroteAt "planning"
      -- AND THE `--7d' INSIDE A STAMP IS A WARNING COOKIE rather than that join:
      -- a stamp ends at its own bracket, so the wall never has to guess.
      insheet shell
             ("press:f press:+ dkey:DEADLINE press::"
                <> " dval:<2026-09-05_Sat_.+2d_--7d> press:Enter") $
        assertEqual "org's own cookies ride the value"
                    [[["SCHEDULED", sheetStamp], ["DEADLINE", "<2026-09-05 Sat .+2d --7d>"]]]
          <=< wroteAt "planning"
      -- EACH FIELD IS A DECIMAL RUN to the parser, so a date spelled without its
      -- leading zeroes IS a date: the box may not refuse a value the server takes.
      insheet shell
             "press:f press:+ dkey:SCHEDULED press:: dval:<2026-8-1_Sat> press:Enter" $
        \answer -> do
          assertEqual "the single-digit month and day written as they were typed"
                      [[["SCHEDULED", "<2026-8-1 Sat>"]]] =<< wroteAt "planning" answer
          echoIs "" ("RET → org-set-property (SCHEDULED: <2026-8-1 Sat>"
                       <> " — the planning line)") answer

    -- THE PLANNING WALL IS THE SERVER'S OWN, ECHOED WHERE THE BOX STILL STANDS:
    -- the write would come back with these very words and nothing left on screen
    -- to fix them in.  The two words this server SETS take everything a
    -- date-owed field takes, so the box asks the READER rather than the stamp
    -- regex — a phrase refused here that the wall would have accepted is that
    -- same failure one storey down, the other way up.
  , testCase "a planning value the date grammar refuses is refused inline" $ do
      insheet shell "press:f press:+ dkey:SCHEDULED press:: dval:soon press:Enter" $
        \answer -> do
          assertEqual "nothing written" ([] :: [Value]) =<< listAt "writes" answer
          assertEqual "and the box stands" True =<< boolAt "dpairopen" answer
          assertEqual "with both halves there to be fixed" ("SCHEDULED", "soon")
            =<< ((,) <$> textAt "dkey" answer <*> textAt "dval" answer)
          headerIs "and the two lists are the bytes they were"
                   [["EFFORT", "0:30"]] [["SCHEDULED", sheetStamp]] answer
          echoIs "" ("RET → org-set-property (SCHEDULED is not a date"
                       <> " org would read back)") answer
      -- …AND WHAT THE GRAMMAR TAKES, THE BOX TAKES, RAW: the server resolves the
      -- phrase once against its own clock, so a bare word and a bare ISO date
      -- travel as they were typed and come back as bytes org writes.
      insheet shell "press:f press:+ dkey:SCHEDULED press:: dval:tomorrow press:Enter" $
        \answer ->
          assertEqual "the phrase on the planning line, for the wall to resolve"
                      [[["SCHEDULED", "tomorrow"]]] =<< wroteAt "planning" answer
      insheet shell "press:f press:+ dkey:DEADLINE press:: dval:2026-09-05 press:Enter" $
        \answer ->
          assertEqual "and beside the entry the line already had"
                      [[["SCHEDULED", sheetStamp], ["DEADLINE", "2026-09-05"]]]
            =<< wroteAt "planning" answer
      -- BOTH HALVES OF A RANGE WEAR ONE BRACKET: the parser takes the pair's
      -- OPENING bracket again after the `--', so a mixed range reparses as
      -- nothing and the box has to refuse it where the box still stands.
      insheet shell
             ("press:f press:+ dkey:SCHEDULED press::"
                <> " dval:<2026-09-01_Tue>--[2026-09-05_Sat] press:Enter") $
        \answer -> do
          assertEqual "nothing written" ([] :: [Value]) =<< listAt "writes" answer
          assertEqual "and the box stands" True =<< boolAt "dpairopen" answer
          assertEqual "with what was typed there to be fixed"
                      "<2026-09-01 Tue>--[2026-09-05 Sat]" =<< textAt "dval" answer
          echoIs "" ("RET → org-set-property (SCHEDULED is not a date"
                       <> " org would read back)") answer
      -- CLOSED IS NOT SETTABLE: it opens no widget, and its value takes the
      -- plain stamp wall.
      insheet shell "press:f press:+ dkey:CLOSED press:: dval:tomorrow press:Enter" $
        \answer -> do
          assertEqual "nothing written" ([] :: [Value]) =<< listAt "writes" answer
          echoIs "" ("RET → org-set-property (CLOSED is not a timestamp"
                       <> " org would read back)") answer
      -- THE EMPTY HALF IS STILL THE EMPTY HALF'S REFUSAL: a planning key with
      -- no value never reaches the stamp wall.
      insheet shell "press:f press:+ dkey:CLOSED press:: press:Enter" $
        echoIs "" "RET → org-set-property (a value is required)"

    -- A `:SCHEDULED:' PAIR STANDING IN A DRAWER IS ONE ANOTHER WRITER MINTED —
    -- the parser never puts one there — so RET over it MIGRATES: the drawer
    -- entry off and the planning entry set, both lists on the one write.
  , keyed shell "a drawer pair keyed for planning migrates on RET" ""
      ("mistyped press:Enter press:f press:n press:f press:n"
         <> " press:Enter press:Enter") $ \answer -> do
        assertEqual "one write" 1 . length =<< listAt "writes" answer
        assertEqual "the drawer entry gone" [[["EFFORT", "0:30"]]]
          =<< wroteAt "properties" answer
        assertEqual "and the planning entry set in that same one"
                    [[["SCHEDULED", "<2026-09-01 Tue>"]]] =<< wroteAt "planning" answer
        assertEqual "the two meta rows the pane is left with"
                    ["SCHEDULED: <2026-09-01 Tue>", ":EFFORT: 0:30"]
          . partsOf "meta" =<< docOf answer
        assertEqual "with the cursor on the line it moved to" 1 =<< intAt "dat" answer
        echoIs "and the model's own word for where it went"
               ("RET → org-ctrl-c-ctrl-c (SCHEDULED: <2026-09-01 Tue>"
                  <> " — moved to the planning line)") answer

    -- ONE WALL, BOTH DOORS.  The drawer's own door routes on the KEY, so it owes
    -- the VALUE the same reading the pair box gives it — asked while the line's
    -- box still stands, since a write refused with the box shut leaves nothing on
    -- screen to fix.
  , keyed shell "the drawer's door meets the planning value's wall too" ""
      ("mistypedbad press:Enter press:f press:n press:f press:n"
         <> " press:Enter press:Enter") $ \answer -> do
        assertEqual "nothing written" ([] :: [Value]) =<< listAt "writes" answer
        assertEqual "and the line's own box stands" True =<< boolAt "dparaopen" answer
        assertEqual "holding the line there to be fixed" ":SCHEDULED: soon"
          =<< textAt "dtext" answer
        headerIs "with the two lists the bytes they were"
                 [["EFFORT", "0:30"], ["SCHEDULED", "soon"]]
                 [["SCHEDULED", sheetStamp]] answer
        echoIs "" ("RET → org-ctrl-c-ctrl-c (SCHEDULED is not a date"
                     <> " org would read back)") answer

    -- THE THREE ARE OFFERED BESIDE THE TREE'S OWN KEYS, hinted so the reroute
    -- is visible before it happens, and UPCASED since that is what they become.
  , testCase "the key half offers org's planning words, hinted" $ do
      insheet shell "press:f press:+" $ \answer -> do
        assertEqual "the tree's keys by how often it writes them, the three last,\
                    \ and only the three saying where they land"
                    [ ("EFFORT", ""), ("OWNER", ""), ("URL", "")
                    , ("SCHEDULED", "planning"), ("DEADLINE", "planning")
                    , ("CLOSED", "planning") ]
          =<< offersOf answer
      -- WHAT IS TYPED FILTERS BOTH, the way this page filters everywhere, and
      -- THE TYPED LINE LEADS what it leaves standing — AGENTS.hs.
      insheet shell "press:f press:+ dkey:sch" $ \answer -> do
        assertEqual "what was typed, then the one word that folds to it,\
                    \ each hinted for where it lands"
                    [("sch", "new"), ("SCHEDULED", "planning")] =<< offersOf answer
        assertEqual "and point on the typed one, so `:' hands `sch' over" 0
          =<< intAt "dofferat" answer
      -- ACCEPTING ONE FLOWS THE NORMAL `:'/TAB ADVANCE, and the write routes.
      -- The offer is one `C-n' under the typed line, which is how it is reached.
      insheet shell
             ("press:f press:+ dkey:sch press:C-n press::"
                <> " dval:<2026-09-01_Tue> press:Enter") $
        \answer -> do
          assertEqual "the offer taken, upcased, and routed"
                      [[["SCHEDULED", "<2026-09-01 Tue>"]]] =<< wroteAt "planning" answer
          assertEqual "the drawer untouched" [[["EFFORT", "0:30"]]]
            =<< wroteAt "properties" answer
      -- AND WITHOUT THAT WALK THE PARTIAL KEY IS THE KEY: `sch' is a property
      -- the tree has never held, so it lands in the drawer as it was spelled.
      insheet shell
             "press:f press:+ dkey:sch press:: dval:soon press:Enter" $
        \answer -> do
          assertEqual "the drawer takes the word typed"
                      [[["EFFORT", "0:30"], ["sch", "soon"]]]
            =<< wroteAt "properties" answer
          assertEqual "and the planning line stands as the fixture spells it"
                      [[["SCHEDULED", "<2026-08-01 Sat>"]]]
            =<< wroteAt "planning" answer
      -- THE THREE ARE `CFG.planning', not the tree's: a server with no
      -- `/properties' door still offers them.
      insheet shell "novocab press:f press:+" $ \answer -> do
        assertEqual "the tree has nothing to add, and each says where it lands"
                    [ ("SCHEDULED", "planning"), ("DEADLINE", "planning")
                    , ("CLOSED", "planning") ]
          =<< offersOf answer

    -- THE VALUE HALF IS AN OPEN VOCABULARY TOO, and the typed line leading is
    -- also what gives point a way BACK to it: the walk clamps at the head of the
    -- list, so a head that was the tree's word left the reader's unreachable.
  , testCase "the value half leads with the typed line, and the walk returns to it" $ do
      insheet shell "press:f press:+ dkey:OWNER press:: dval:ad" $ \answer -> do
        assertEqual "what was typed, then the value the tree spells under the key,\
                    \ the typed one alone calling itself new"
                    [("ad", "new"), ("ada", "")] =<< offersOf answer
      insheet shell "press:f press:+ dkey:OWNER press:: dval:ad press:Enter" $
        \answer ->
          assertEqual "so RET writes the word typed"
                      [[["EFFORT", "0:30"], ["OWNER", "ad"]]]
            =<< wroteAt "properties" answer
      -- Taking a VALUE offer is DRY: it fills the field, and the apply stays the
      -- reader's own next press.
      insheet shell
             "press:f press:+ dkey:OWNER press:: dval:ad press:C-n press:Enter press:Enter" $
        \answer ->
          assertEqual "walking onto the offer still completes to it"
                      [[["EFFORT", "0:30"], ["OWNER", "ada"]]]
            =<< wroteAt "properties" answer
      insheet shell
             ("press:f press:+ dkey:OWNER press:: dval:ad"
                <> " press:C-n press:C-p press:Enter") $
        \answer ->
          assertEqual "and walking back lands on the typed line again"
                      [[["EFFORT", "0:30"], ["OWNER", "ad"]]]
            =<< wroteAt "properties" answer
      -- A VALUE FOLDING TO ONE THE TREE SPELLS COINCIDES WITH IT: one row drawn.
      insheet shell "press:f press:+ dkey:OWNER press:: dval:ADA" $ \answer -> do
        assertEqual "the tree's spelling alone, carrying no new hint"
                    [("ada", "")] =<< offersOf answer

    -- THE COINCIDENCE IS ASKED OF THE WHOLE VOCABULARY, never of what the cap
    -- left standing: a key the tree really spells that RANKS UNDER THE CAP is
    -- still that key, so it leads the offers unhinted rather than drawing itself
    -- `new' — and `:' hands over the word that was typed.
  , testCase "a real key ranking under the offer cap coincides with itself" $ do
      insheet shell "deepvocab press:f press:+ dkey:AREA" $ \answer -> do
        assertEqual "the typed key leads, wearing no hint, the cap spent on the rest"
                    [ ("AREA", ""), ("AREA_CODE", ""), ("AREA_NAME", "")
                    , ("AREA_SIZE", ""), ("SUBAREA", ""), ("AREA_ID", "")
                    , ("AREA_TAG", "") ]
          =<< offersOf answer
        assertEqual "with point on it" 0 =<< intAt "dofferat" answer
      insheet shell "deepvocab press:f press:+ dkey:AREA press:: dval:north press:Enter" $
        \answer ->
          assertEqual "so the pair is written under the key that was typed"
                      [[["EFFORT", "0:30"], ["AREA", "north"]]]
            =<< wroteAt "properties" answer

  , testCase "ESC puts an open pair back, and the next one closes the sheet" $ do
      insheet shell
             "press:f press:n press:f press:Enter dpara:junk press:Escape" $ \answer -> do
        assertEqual "the pair as it was" ["meta", ":EFFORT: 0:30"]
          =<< (!! 3) <$> docOf answer
        assertEqual "the sheet is still up" "on" =<< textAt "modal" answer
        assertEqual "with nothing written" ([] :: [Value]) =<< listAt "writes" answer
      insheet shell
             "press:f press:n press:f press:Enter press:Escape press:Escape" $
        assertEqual "the second one is the sheet's" "" <=< textAt "modal"

    -- C-c ' RE-MATERIALIZES rather than converting locally, which keeps an org parser out of this page.
  , testCase "C-c ' shows the raw subtree, and again shows the panes" $ do
      insheet shell "press:C-c press:'" $ \answer -> do
        assertEqual "the whole subtree, every region spelled out"
                    fixtureOrg =<< textAt "sheet" answer
        assertEqual "the doc pane is off the sheet" "raw" =<< textAt "shape" answer
        assertEqual "and the logbook strip with it" "" =<< textAt "logbook" answer
        echoIs "and the pill says which way it went" "C-c ' → org-edit-special (raw org)" answer
      insheet shell "press:C-c press:' press:C-c press:'" $ \answer -> do
        assertEqual "back to the document, and the textarea empty behind it"
                    "" =<< textAt "sheet" answer
        assertEqual "with both panes back" "" =<< textAt "shape" answer
        echoIs "the pill" "C-c ' → org-edit-special (structured document)" answer

  , testCase "a dirty sheet is refused the toggle, in either pane" $ do
      insheet shell "press:C-c press:' sheet:hello press:C-c press:'" $
        \answer -> do
          assertEqual "the text stands" "hello" =<< textAt "sheet" answer
          assertEqual "and the shape with it" "raw" =<< textAt "shape" answer
          echoIs "named the key" "C-c ' → org-edit-special (sync first — C-x C-s)" answer
      -- The write LANDED but the store still answers the old digest: until the
      -- re-read catches the receipt up, the sheet is dirty and the toggle waits.
      insheet shell
             ("press:f press:n press:f press:Enter dpara::EFFORT:_0:45"
                <> " press:Enter press:C-c press:'") $
        \answer -> do
          assertEqual "a header edit the store has not caught up with is dirty too"
                      "" =<< textAt "shape" answer
          echoIs "same refusal" "C-c ' → org-edit-special (sync first — C-x C-s)" answer

    -- An edit nobody committed is not one.
  , keyed shell "an open pair line is not an edit until it is committed"
      "Enter" ("press:f press:n press:f press:Enter dpara::EFFORT:_0:45"
                <> " press:C-c press:'") $ \answer -> do
        assertEqual "the toggle went through" "raw" =<< textAt "shape" answer
        echoIs "and said so" "C-c ' → org-edit-special (raw org)" answer

  , keyed shell "a remount carries the sheet's edited header across it"
      "Enter" ("press:f press:n press:f press:Enter dpara::EFFORT:_0:45"
                <> " press:Enter close:view-changed") $
        \answer -> do
          assertEqual "mounted twice" 2 =<< intAt "mounts" answer
          headerIs "the stash carried the lists, edit and all"
                   [["EFFORT", "0:45"]] [["SCHEDULED", sheetStamp]] answer
          assertEqual "the drawer reopened on the pair" ["meta", ":EFFORT: 0:45"]
            =<< (!! 3) <$> docOf answer
          -- The reopened sheet re-reads its digest, and the store still lags the
          -- write's receipt: what a remount cannot hide is said as CONFLICT.
          assertEqual "and the lag is worn openly"
                      "conflict" =<< textAt "state" answer

  , keyed shell "raw mode leaves TAB to the browser"
      "Enter" "press:C-c press:' press:Tab" $ \answer -> do
        assertEqual "the focus stayed in the text" "mtext" =<< textAt "focus" answer
        assertBool "and the key was left off the browser"
          . notElem "Tab" =<< textsAt "prevented" answer

    -- A blurred raw sheet is still a SURFACE: a click on its chrome takes the focus off without closing anything.
  , testCase "a raw sheet keeps the keys with its textarea blurred" $ do
      insheet shell "press:C-c press:' blur press:d" $ \answer -> do
        assertEqual "nothing focused" "" =<< textAt "focus" answer
        assertEqual "and no row flagged behind the sheet"
                    ([] :: [T.Text]) =<< textsAt "flagged" answer
      insheet shell "press:C-c press:' blur press:q" $
        assertEqual "and the sheet is still up" "on" <=< textAt "modal"

    -- ONE FOCUS LANGUAGE, and the pane focuses nothing: the mark is the FRAME's, and it leaves when the keys do.
  , testCase "the pane holding the keys wears it, and only while it does" $ do
      insheet shell "" $ \answer -> do
        assertEqual "the document opens with the keys" True
          =<< boolAt "dactive" answer
        assertEqual "and nothing is focused at all" "" =<< textAt "focus" answer
      insheet shell "press:Escape" $ \answer -> do
        assertEqual "the sheet is closed" "" =<< textAt "modal" answer
        assertEqual "and the mark went with it" False =<< boolAt "dactive" answer

    -- The FOLDS are reseeded per fill: what the reader opened does not outlive the sheet.
  , keyed shell "the drawer starts folded again when the sheet is reopened"
      "Enter" "press:f press:n press:f press:Escape press:Enter" $
        \answer -> do
          assertEqual "one folded line again" ["comp:properties:drawer", ":PROPERTIES: \8230"]
            =<< (!! 2) <$> docOf answer
          assertEqual "and the cursor back on the headline" 0 =<< intAt "dat" answer

  , keyed shell "d flags a meta row rather than deleting it"
      "Enter" "press:f press:d" $
        \answer -> do
          assertEqual "the planning line wears the flag" [1] =<< flaggedOf answer
          headerIs "and the lists are untouched"
                   [["EFFORT", "0:30"]] [["SCHEDULED", sheetStamp]] answer
          echoIs "the pill says what the second press will do"
            "d → delete-flag (d again deletes)" answer
          -- `d' over the table is `archive-flag', so this is the proof that the sheet holds the keys.
          assertEqual "and the table's own d never ran" ([] :: [Value])
                      =<< listAt "commands" answer

    -- A DELETED PAIR LEAVES THROUGH THE LISTS, never the splice: the write follows at once.
  , testCase "d again deletes the pair through the lists, and D is that press alone" $ do
      insheet shell
             "press:f press:n press:f press:d press:d" $ \answer -> do
        assertEqual "the drawer the write asks for" [[]]
                    =<< wroteAt "properties" answer
        assertEqual "the flag was spent with it" ([] :: [Int])
                    =<< flaggedOf answer
        echoIs "and the pill counted the set" "D → org-delete-element (1 flagged taken)" answer
      insheet shell "press:f press:n press:f press:D" $
        \answer -> do
          assertEqual "D needs no flag: the row at point is the set" [[]]
            =<< wroteAt "properties" answer
          echoIs "and says so" "D → org-delete-element (row taken)" answer

    -- The row is SYNTHESIZED off the list, so clearing the entries takes the line with them.
  , keyed shell "deleting the planning line clears its entries, and the row goes with them"
      "Enter" "press:f press:d press:d" $ \answer -> do
        assertEqual "the write carries no planning entry" [[]]
                    =<< wroteAt "planning" answer
        assertEqual "and no planning line is drawn"
                    [] . partsOf "meta" =<< docOf answer

  , keyed shell "u takes a flag off and steps on"
      "Enter" "press:f press:d press:u press:D" $ \answer -> do
        echoIs "nothing was flagged when D ran, so it took the row it stepped to"
               "D → org-delete-element (row taken)" answer
        assertEqual "which was the drawer: every pair went through the lists"
                    [[]] =<< wroteAt "properties" answer
        assertEqual "and the planning line stands" [[["SCHEDULED", sheetStamp]]]
                    =<< wroteAt "planning" answer

    -- The dispatch's own ONCE list cannot reach a key this listener owns, so the guard is the sheet's.
  , keyed shell "a held d flags once and never deletes what it flagged"
      "Enter" "press:f press:d repeat:d repeat:d" $
        \answer -> do
        assertEqual "still flagged" [1] =<< flaggedOf answer
        assertEqual "and nothing written" ([] :: [Value]) =<< listAt "writes" answer

  , testCase "a deletion writes at once, and a flag alone writes nothing" $ do
      insheet shell
             "press:f press:n press:f press:d press:d" $
        \answer ->
          assertEqual "and it landed" "synced" =<< textAt "state" answer
      insheet shell "press:f press:d press:Escape" $
        \answer -> do
          assertEqual "a flag alone writes nothing" ([] :: [Value])
                      =<< listAt "writes" answer
          assertEqual "and the sheet closed without one" "" =<< textAt "modal" answer
  ]

-- | THE DATE WIDGET in the material document: the field in the planning value's
-- own slot, the resolver's preview riding after it as GHOST, and the RAW text
-- going to the server at the commit.  Driven through the shell harness, which
-- runs the page's own glue.
--
-- THE CLOCK IS PINNED where an answer would otherwise move with the calendar the
-- suite runs on: `dateon:' is the corpus's own reference day, 2026-08-22 (Sat).
dateWidgetSpec :: IO T.Text -> TestTree
dateWidgetSpec shell = testGroup "Shell date widget"
  [ -- THE ENTRY COMES UP WHOLLY SELECTED, org-read-date's own default: one
    -- keystroke replaces the value that stands, and a bare RET recommits it.
    testCase "C-c C-s opens over the value that stands, wholly selected" $ do
      insheet shell (pinned <> " press:C-c press:C-s") $ \answer -> do
        assertEqual "the widget is up" True =<< boolAt "ddateopen" answer
        assertEqual "and no modal was raised for it" "" =<< textAt "prompt" answer
        assertEqual "the field holds the entry that stands"
                    sheetStamp =<< textAt "dwhen" answer
        assertEqual "the field holds the keys" "dwhen" =<< textAt "focus" answer
        assertEqual "and the whole of it is selected"
                    [0, T.length sheetStamp, T.length sheetStamp]
          =<< intsAt "dwhensel" answer
        -- THE GHOST IS SILENT AT ENTRY: the value that stands IS its own
        -- resolution, org's own spelling, so there is nothing to add.
        assertEqual "the ghost says nothing over a value that is its own answer"
                    "" =<< textAt "dghost" answer
        assertEqual "and nothing is written while it stands open" ([] :: [Value])
          =<< listAt "writes" answer
        assertEqual "nor asked of the server" ([] :: [Value])
          =<< listAt "commands" answer
      -- A BARE RET TAKES THE DEFAULT, byte for byte -- the whole point of the
      -- selection: the reader who meant "that one" presses one key.
      insheet shell (pinned <> " press:C-c press:C-s press:Enter") $ \answer -> do
        assertEqual "the same bytes back to the server"
                    [("SCHEDULED", Just sheetStamp)] =<< plannedOf answer
        assertEqual "the widget shut behind it" False =<< boolAt "ddateopen" answer

    -- ONE LINE: what was typed, and the resolution riding after it in the mute
    -- ink.  THREE STATES AND NO FOURTH.
  , testCase "the ghost previews, refuses, and keeps quiet" $ do
      insheet shell (pinned <> " press:C-c press:C-s dwhen:18_aug") $ \answer -> do
        assertEqual "what was typed stands in the field" "18 aug"
          =<< textAt "dwhen" answer
        assertEqual "and the resolution rides after it"
                    " \8594 <2026-08-18 Tue>" =<< textAt "dghost" answer
        assertEqual "in the mute ink, not the marked one" False
          =<< boolAt "dghostbad" answer
        assertEqual "and nothing is asked of the server for a preview"
                    ([] :: [Value]) =<< listAt "commands" answer
      -- A TERM STILL BEING WRITTEN IS NO MISTAKE: `18 a' is a month halfway
      -- typed, and a refusal flashed at every keystroke is one nobody reads.
      insheet shell (pinned <> " press:C-c press:C-s dwhen:18_a") $ \answer -> do
        assertEqual "the ghost is dark over a half-typed month" ""
          =<< textAt "dghost" answer
        assertEqual "and wears no refusal" False =<< boolAt "dghostbad" answer
      -- A HARD REFUSAL SPEAKS, in the refusal's own ink and the corpus's own
      -- word: no further character rescues a day that is not on the calendar.
      insheet shell (pinned <> " press:C-c press:C-s dwhen:31_february") $ \answer -> do
        assertEqual "the short word, which is all a trailing ghost has room for"
                    " \10007 not a date" =<< textAt "dghost" answer
        assertEqual "wearing the refusal's ink" True =<< boolAt "dghostbad" answer
      -- AN INVERTED RANGE GETS THE SECOND WORD: "not a date" reads oddly of a
      -- phrase naming two perfectly good days in the wrong order.
      insheet shell (pinned <> " press:C-c press:C-s dwhen:from_30_dec_to_2_jan") $
        \answer ->
          assertEqual "the inversion is spelled apart"
                      " \10007 ends before it starts" =<< textAt "dghost" answer
      -- AND IT FALLS SILENT WHERE THE RESOLUTION IS WHAT WAS TYPED: drawing the
      -- same string twice on one line is the duplication the shape is against.
      insheet shell (pinned <> " press:C-c press:C-s dwhen:<2026-08-05_Mon>") $
        \answer -> do
          assertEqual "org's own spelling stands as written"
                      "<2026-08-05 Mon>" =<< textAt "dwhen" answer
          assertEqual "and the ghost adds nothing to it" ""
            =<< textAt "dghost" answer
      -- …AND THE ONE PLACE THE WIDGET MUST NOT KNOW BETTER: that day is a
      -- Wednesday, and org's own bracket goes through VERBATIM, wrong weekday
      -- and all (`test/TestQuery.hs:1791' pins the wall's half of it).  A pass
      -- that made the renderer uniform would silently respell this.
      insheet shell
              (pinned <> " press:C-c press:C-s dwhen:<2026-08-05_Mon> press:Enter") $
        \answer ->
          assertEqual "the bytes the reader typed, not the bytes a calendar says"
                      [("SCHEDULED", Just "<2026-08-05 Mon>")] =<< plannedOf answer

    -- WHAT TRAVELS IS WHAT WAS TYPED.  The ghost resolved for ink; the server
    -- resolves for bytes, once, against its own clock.
  , keyed shell "RET sends the raw phrase, never the ghost's own reading"
      "Enter" (pinned <> " press:C-c press:C-s dwhen:18_aug press:Enter") $ \answer -> do
        assertEqual "one command, over the row the sheet is open on"
                    [("set-planning", ["r1"])] =<< postedOf answer
        assertEqual "carrying the phrase, not the stamp the ghost drew"
                    [("SCHEDULED", Just "18 aug")] =<< plannedOf answer
        echoIs "the pill names the commit's own key"
          "RET \8594 org-glance-overview:schedule (18 aug \183 1)" answer

    -- THE SHIPPED FOOT'S OWN PROMISE, kept verbatim: clearing is the widget's
    -- law and not the grammar's, so it never asks whether nothing is a date.
  , keyed shell "an emptied field clears the entry"
      "Enter" (pinned <> " press:C-c press:C-s dclear press:Enter") $ \answer -> do
        assertEqual "a null date" [("SCHEDULED", Nothing)] =<< plannedOf answer
        echoIs "and the pill says which"
          "RET \8594 org-glance-overview:schedule (cleared \183 1)" answer

    -- A MATERIALIZED CHILD HAS NO ROW ID, so the door changes and the wall does
    -- not.  `set-planning' addresses ROWS: fired over a child it names the row
    -- the sheet was opened on, and the entry lands on the ROOT headline.  The
    -- child rides the COMMIT door instead -- the very one the pair box's
    -- planning-routed pair rides -- aimed by `?child=' at the entry, carrying
    -- the RAW phrase for the same `settledPlanning' wall to resolve.  WHAT THE
    -- SUBTREE'S BYTES BECOME is the browser's to prove; here it is the TARGET
    -- and the CARGO, which is what went wrong.
  , testCase "the widget over a materialized child writes the CHILD, not the row" $ do
      insheet shell (ontoChild <> " press:Enter press:C-c press:C-s"
                     <> " dwhen:18_aug press:Enter") $ \answer -> do
        assertEqual "no command is asked at all, which is what named the ROW"
                    ([] :: [Value]) =<< listAt "commands" answer
        assertEqual "the write is aimed at the entry" ["r1#0"]
          =<< textsAt "wroteAt" answer
        assertEqual "carrying the raw phrase on the child's own planning list"
                    [[["SCHEDULED", "18 aug"]]] =<< wroteAt "planning" answer
        assertEqual "beside the child's own body, so no byte of the row rides along"
                    ["** two :web:\nchild body\n"]
          =<< traverse (textAt "body") =<< listAt "writes" answer
        echoIs "and the pill names the commit's own key"
               "RET \8594 org-glance-overview:schedule (18 aug)" answer
      -- AND THE CLEAR RIDES THE SAME DOOR: an empty value is how org takes an
      -- entry off, which the drawer's both-halves rule would refuse -- that rule
      -- is the DRAWER's, and the planning line is asked above it.
      insheet shell (ontoChild <> " press:Enter press:C-c press:C-s"
                     <> " dclear press:Enter") $ \answer -> do
        assertEqual "nothing asked" ([] :: [Value]) =<< listAt "commands" answer
        assertEqual "one write, at the entry" ["r1#0"] =<< textsAt "wroteAt" answer
        assertEqual "with the entry off the list" [[]] =<< wroteAt "planning" answer
        echoIs "and the pill says which"
               "RET \8594 org-glance-overview:schedule (cleared)" answer

    -- ORG SCHEDULES THE ENTRY AT POINT.  On a CHILD row the keys materialize it
    -- first -- the move `RET' makes over that row -- and the widget opens over
    -- the child's own planning, so the commit above lands where the reader was
    -- pointing.  THE SUMMON RIDES THE REREAD'S CONTINUATION: the fill is what
    -- the box reads its slot out of, and a summon beside it would open over the
    -- document it just left.
  , testCase "C-c C-s on a child row materializes it and opens over ITS planning" $ do
      insheet shell (ontoChild <> " press:C-c press:C-s") $ \answer -> do
        assertEqual "the sheet is the child's own" ["one", "two"]
          =<< textsAt "where" answer
        assertEqual "read at the entry, which is the materialize" ["r1", "r1#0"]
          =<< textsAt "readAt" answer
        assertEqual "the widget is up" True =<< boolAt "ddateopen" answer
        assertEqual "over a slot this child has not got, so it opens empty" ""
          =<< textAt "dwhen" answer
        assertEqual "and the keyword is ghosted onto the CHILD's line"
                    ["SCHEDULED: "] . partsOf "meta" =<< docOf answer
        assertEqual "nothing written by opening it" ([] :: [Value])
          =<< listAt "writes" answer
        echoIs "and the pill is the widget's own foot"
               "C-c C-s \8594 org-glance-overview:schedule \
               \(RET sets it \183 empty clears it \183 ESC leaves)" answer
      -- AND THE OTHER KEY THE SAME WAY, since one summon serves both words.
      insheet shell (ontoChild <> " press:C-c press:C-d dwhen:18_aug press:Enter") $
        \answer -> do
          assertEqual "the write is the child's" ["r1#0"] =<< textsAt "wroteAt" answer
          assertEqual "under the key that summoned it"
                      [[["DEADLINE", "18 aug"]]] =<< wroteAt "planning" answer
      -- DEL IS UNTOUCHED: the climb out of a child is the parent, summon or no.
      insheet shell (ontoChild <> " press:C-c press:C-s press:Escape press:Backspace") $
        \answer -> do
          assertEqual "back at the row, one crumb again" ["one"]
            =<< textsAt "where" answer
          assertEqual "and nothing was written on the way" ([] :: [Value])
            =<< listAt "writes" answer

    -- THE OTHER DOOR ONTO THE SAME BOX: `f' walks into the planning line and
    -- `RET' over an ENTRY raises the widget keyed by that entry -- one box, one
    -- wall, and no key of its own to learn.
  , testCase "RET over an entry raises the widget the summon keys raise" $ do
      onTable shell ("planned " <> pinned
                     <> " press:Enter press:f press:f press:f press:Enter") $ \answer -> do
        assertEqual "the widget is up" True =<< boolAt "ddateopen" answer
        assertEqual "over the entry the walk stood in, not the line's first"
                    deadStamp =<< textAt "dwhen" answer
        assertEqual "the field holds the keys" "dwhen" =<< textAt "focus" answer
        assertEqual "and the whole of it is selected"
                    [0, T.length deadStamp, T.length deadStamp]
          =<< intsAt "dwhensel" answer
        assertEqual "the ghost adds nothing to a value that is its own answer"
                    "" =<< textAt "dghost" answer
        echoIs "and the pill names that entry's own command"
               "RET \8594 org-glance-overview:deadline \
               \(RET sets it \183 empty clears it \183 ESC leaves)" answer
      onTable shell ("planned " <> pinned
                     <> " press:Enter press:f press:f press:f press:Enter"
                     <> " dwhen:18_aug press:Enter") $ \answer -> do
        assertEqual "and it commits through the widget's own door"
                    [("DEADLINE", Just "18 aug")] =<< plannedOf answer
        assertEqual "the widget shut behind it" False =<< boolAt "ddateopen" answer

    -- ORG'S THIRD WORD IS NOT SET, IT IS REPARSED: the server takes a CLOSED
    -- value back only if it reads as a timestamp, so the widget over that entry
    -- reads the same wall -- and English is widened to it on neither side.
  , testCase "the widget over CLOSED reads its own wall, and no English" $ do
      let closedRun = "planned " <> pinned <> " press:Enter"
                      <> " press:f press:f press:f press:f press:Enter"
      onTable shell closedRun $ \answer -> do
        assertEqual "the box opens over it like any other entry" True
          =<< boolAt "ddateopen" answer
        assertEqual "holding org's own spelling" closedStamp
          =<< textAt "dwhen" answer
        assertEqual "and the ghost adds nothing to it" "" =<< textAt "dghost" answer
      -- THE GRAMMAR IS NOT THIS WALL'S: a phrase the settable words resolve is
      -- a hard refusal here, there is no vocabulary to offer either, and RET over
      -- it commits nothing.
      onTable shell (closedRun <> " dwhen:18_aug press:Enter") $ \answer -> do
        assertEqual "the short word a trailing ghost has room for"
                    " \10007 not a timestamp" =<< textAt "dghost" answer
        assertEqual "wearing the refusal's ink" True =<< boolAt "dghostbad" answer
        assertEqual "and nothing is proposed that this wall would refuse"
                    [] =<< widgetOffers answer
        assertEqual "nothing was asked" ([] :: [Value]) =<< listAt "commands" answer
        assertEqual "the box stands, with what was typed still in it" True
          =<< boolAt "ddateopen" answer
        echoIs "and the refusal is the CLOSED wall's own sentence"
               "RET \8594 org-add-planning-info \
               \(CLOSED is not a timestamp org would read back)" answer
      -- A BRACKET STILL OPEN IS STILL BEING TYPED: no refusal flashes on the way
      -- through org's own spelling.
      onTable shell (closedRun <> " dwhen:[2026-09-01") $ \answer -> do
        assertEqual "the ghost is dark over an unclosed bracket" ""
          =<< textAt "dghost" answer
        assertEqual "and wears no refusal" False =<< boolAt "dghostbad" answer
      -- AND THE COMMIT DOOR IS THE ONE EVERY OTHER ENTRY USES, carrying the raw
      -- text: what the wall reads back is what the reader typed.
      onTable shell (closedRun <> " dwhen:[2026-09-01_Tue] press:Enter") $ \answer -> do
        assertEqual "one command, over the row the sheet is open on"
                    [("set-planning", ["r1"])] =<< postedOf answer
        assertEqual "carrying the bracket as it was typed"
                    [("CLOSED", Just "[2026-09-01 Tue]")] =<< plannedOf answer
      -- THE SHIFTED ARROWS MOVE THE DAY AND NEVER THE SPELLING: a step that
      -- wrote a bare ISO back would leave the field holding what RET refuses.
      onTable shell (closedRun <> " press:S-ArrowRight") $ \answer -> do
        assertEqual "a day forward, still a bracket of the kind that stood"
                    "[2026-08-03 Mon]" =<< textAt "dwhen" answer
        assertEqual "so the ghost has nothing to add" "" =<< textAt "dghost" answer

    -- A ROW WITH NO SUCH ENTRY HAS NO SLOT TO STAND IN, so the summon draws one
    -- -- and the draft joins NO list, which is what keeps a half-typed date off
    -- the disk the moment the sheet is left.
  , testCase "C-c C-d draws the line it needs, and the draft joins no list" $
      insheet shell (pinned <> " press:C-c press:C-d") $ \answer -> do
        assertEqual "the keyword is ghosted onto the planning line"
                    ["SCHEDULED: " <> sheetStamp <> " DEADLINE: "]
          . partsOf "meta" =<< docOf answer
        assertEqual "the list a flush writes is the list it was"
                    [["SCHEDULED", sheetStamp]] =<< pairsAt "dplan" answer
        assertEqual "and the field opens empty over it" "" =<< textAt "dwhen" answer
        assertEqual "with point on the line the widget stands in" 1
          =<< intAt "dat" answer
        -- THE OPEN CYCLE IS NOT OVER WHEN THE BOX GOES UP: drawing the slot
        -- sends a port message and the model comes back a macrotask later with
        -- a redraw behind it, which the entry must survive.
        assertEqual "the field still holds the keys after the redraw" "dwhen"
          =<< textAt "focus" answer
        assertEqual "and the entry's selection with it" [0, 0, 0]
          =<< intsAt "dwhensel" answer

    -- THE ESCAPE IS FROM THE EDIT, and the sheet comes back byte for byte --
    -- including the planning line's own ABSENCE where the summon drew it in.
  , testCase "ESC takes the widget and the keyword it ghosted in" $
      insheet shell
              (pinned <> " press:C-c press:C-d dwhen:18_aug press:Escape") $ \answer -> do
        assertEqual "the widget is gone" False =<< boolAt "ddateopen" answer
        assertEqual "and the document is the one it opened over"
                    fixtureDoc =<< docOf answer
        assertEqual "point back on the stop the key was pressed over" 0
          =<< intAt "dat" answer
        assertEqual "nothing written" ([] :: [Value]) =<< listAt "writes" answer
        assertEqual "and nothing asked" ([] :: [Value]) =<< listAt "commands" answer
        echoIs "" "ESC \8594 keyboard-quit (the planning line unchanged)" answer

    -- THE OTHER SUMMON KEY SWITCHES THE STANDING WIDGET rather than refusing it:
    -- a reader who pressed the wrong word presses the other one, and the box
    -- that stands leaves by the very door ESC opens -- byte-identical restore,
    -- the ghosted keyword going back out with it -- before the asked word's box
    -- opens.  THE ENTRY-SELECTION IS WHAT MADE THE REFUSAL INVISIBLE: over a
    -- wholly selected field `C-c' copied instead of prefixing, so the second
    -- chord died at the dispatch and never reached this door at all.
  , testCase "C-c C-s over the open DEADLINE widget switches to SCHEDULED" $ do
      insheet shell (pinned <> " press:C-c press:C-d press:C-c press:C-s") $
        \answer -> do
          -- The RET that opened the sheet leads; what matters after it is that
          -- the SECOND chord was claimed at all, which is the bug's own shape.
          assertEqual "both chords were claimed, the second one included"
                      ["Enter", "C-c", "C-d", "C-c", "C-s"]
            =<< textsAt "prevented" answer
          assertEqual "the widget stands" True =<< boolAt "ddateopen" answer
          assertEqual "over the SCHEDULED value the row already carries"
                      sheetStamp =<< textAt "dwhen" answer
          assertEqual "and the DEADLINE the first chord ghosted in went with it"
                      ["SCHEDULED: " <> sheetStamp] . partsOf "meta" =<< docOf answer
          assertEqual "the list a flush writes is the list it was"
                      [["SCHEDULED", sheetStamp]] =<< pairsAt "dplan" answer
          assertEqual "nothing written on the way" ([] :: [Value])
            =<< listAt "writes" answer
          assertEqual "and nothing asked" ([] :: [Value])
            =<< listAt "commands" answer
          echoIs "the pill is the second summon's own foot"
                 "C-c C-s \8594 org-glance-overview:schedule \
                 \(RET sets it \183 empty clears it \183 ESC leaves)" answer
      -- THE SAME KEY IS A RE-SUMMON, harmless and not special-cased: the box
      -- comes back over the same slot, wholly selected as an open leaves it.
      insheet shell (pinned <> " press:C-c press:C-s press:C-c press:C-s") $
        \answer -> do
          assertEqual "the widget stands" True =<< boolAt "ddateopen" answer
          assertEqual "over the same value" sheetStamp =<< textAt "dwhen" answer
          assertEqual "wholly selected again"
                      [0, T.length sheetStamp, T.length sheetStamp]
            =<< intsAt "dwhensel" answer
          assertEqual "and the document is the one it opened over"
                      fixtureDoc =<< docOf answer
      -- AND THE RE-SUMMON OF A DRAWN ONE IS THE HARD HALF: the cancel takes the
      -- ghosted keyword back and the summon draws it again, so a second line or
      -- a point left on a row that no longer exists is what would go wrong.
      insheet shell (pinned <> " press:C-c press:C-d press:C-c press:C-d") $
        \answer -> do
          assertEqual "the widget stands" True =<< boolAt "ddateopen" answer
          assertEqual "over an empty slot, as the first summon left it" ""
            =<< textAt "dwhen" answer
          assertEqual "the keyword is ghosted in ONCE, not twice"
                      ["SCHEDULED: " <> sheetStamp <> " DEADLINE: "]
            . partsOf "meta" =<< docOf answer
          assertEqual "with point on the line the widget stands in" 1
            =<< intAt "dat" answer
          assertEqual "and the list a flush writes is still the list it was"
                      [["SCHEDULED", sheetStamp]] =<< pairsAt "dplan" answer

    -- THE EXEMPTION IS THE VIRGIN WIDGET'S ALONE.  Once the reader has TYPED,
    -- a selection they MADE over the field is live again and `C-c' is the copy
    -- it always was -- the browser's own law, which this page bends for the
    -- box's own selection and for nothing else.
  , testCase "a selection the reader made keeps C-c a copy" $
      insheet shell (pinned <> " press:C-c press:C-d dwhen:18_aug dselect"
                            <> " press:C-c press:C-s") $ \answer -> do
        -- The sheet's own RET leads; the tail is one chord where the case above
        -- has two, and that ABSENCE is the whole assertion.
        assertEqual "the first chord was claimed and the second one was not"
                    ["Enter", "C-c", "C-d"] =<< textsAt "prevented" answer
        assertEqual "so the DEADLINE box is the one still standing" True
          =<< boolAt "ddateopen" answer
        assertEqual "holding what was typed into it" "18 aug"
          =<< textAt "dwhen" answer
        assertEqual "with the keyword it was summoned for still drawn"
                    ["SCHEDULED: " <> sheetStamp <> " DEADLINE: "]
          . partsOf "meta" =<< docOf answer

    -- AND THE REFUSAL THE OTHER EDITS KEEP NOW SPEAKS, which the dead chord hid:
    -- a pair box holds a line nobody has decided about, so the summon names the
    -- two keys out of it rather than throwing the line away.
  , testCase "an open pair box refuses the summon, and says so" $
      insheet shell "press:f press:+ press:C-c press:C-s" $ \answer -> do
        assertEqual "the pair box stands" True =<< boolAt "dpairopen" answer
        assertEqual "and no widget went up beside it" False
          =<< boolAt "ddateopen" answer
        echoIs "the guard's own sentence, at last audible"
               "C-c C-s \8594 org-glance-overview:schedule \
               \(an edit is open \8212 RET writes it, ESC leaves)" answer

    -- ORG-READ-DATE'S OWN WALK IN ITS OWN MINIBUFFER: the plain arrows belong to
    -- the caret, so the shifted ones carry the day and the week.
  , testCase "the shifted arrows adjust in place, and the ghost follows" $ do
      insheet shell (pinned <> " press:C-c press:C-s press:S-ArrowRight") $ \answer -> do
        assertEqual "a day forward, written into the field"
                    "2026-08-02" =<< textAt "dwhen" answer
        assertEqual "and the ghost is the day it now names"
                    " \8594 <2026-08-02 Sun>" =<< textAt "dghost" answer
      insheet shell (pinned <> " press:C-c press:C-s press:S-ArrowLeft") $ \answer ->
        assertEqual "and back" "2026-07-31" =<< textAt "dwhen" answer
      insheet shell (pinned <> " press:C-c press:C-s press:S-ArrowDown") $ \answer ->
        assertEqual "a week down" "2026-08-08" =<< textAt "dwhen" answer
      insheet shell (pinned <> " press:C-c press:C-s press:S-ArrowUp") $ \answer ->
        assertEqual "and a week up" "2026-07-25" =<< textAt "dwhen" answer
      -- A YEAR UNDER 100 WALKS ONE DAY AND NOT NINETEEN CENTURIES: `Date.UTC'
      -- reads 0..99 as 1900+y, and the arrows ran their arithmetic through it.
      -- TWICE, because the step WRITES ITS ANSWER BACK into the field and the
      -- next press must read that answer: the bare ISO's year is any digit run
      -- at both doors, or the walk stops dead after one step.
      insheet shell (pinned <> " press:C-c press:C-s dwhen:0099-01-01"
                            <> " press:S-ArrowRight") $ \answer -> do
        assertEqual "a day forward off a small year" "99-01-02"
          =<< textAt "dwhen" answer
        assertEqual "and the ghost is the wall's own stamp for it"
                    " \8594 <99-01-02 Fri>" =<< textAt "dghost" answer
      insheet shell (pinned <> " press:C-c press:C-s dwhen:0099-01-01"
                            <> " press:S-ArrowRight press:S-ArrowRight") $ \answer ->
        assertEqual "and the walk goes on from what it wrote" "99-01-03"
          =<< textAt "dwhen" answer

    -- OFFERS STAND AT FRESH AND UNFINISHED POSITIONS AND NOWHERE ELSE, and a
    -- DATE offer resolves -- so the hint column is the offer's own preview.
  , testCase "the offers resolve, and a finished term carries none" $ do
      insheet shell (pinned <> " press:C-c press:C-s dwhen:18_a") $ \answer -> do
        assertEqual "the reader's own line leads, then the months it could be"
                    [ ("18 a", "new")
                    , ("18 april", "<2026-04-18 Sat>")
                    , ("18 august", "<2026-08-18 Tue>") ]
          =<< widgetOffers answer
        assertEqual "with point on the line the reader typed" 0
          =<< intAt "dwofferat" answer
      insheet shell (pinned <> " press:C-c press:C-s dwhen:18_august") $ \answer ->
        assertEqual "a term that reads as a whole date carries none"
                    [] =<< widgetOffers answer
      -- POINT STANDS ON THE READER'S OWN LINE, which is nothing to take: `RET'
      -- there is the phrase as it was spelled, and here that phrase is refused.
      insheet shell (pinned <> " press:C-c press:C-s dwhen:18_a press:Enter") $
        \answer -> do
          assertEqual "the typed line is not swapped for the word it prefixes"
                      "18 a" =<< textAt "dwhen" answer
          assertEqual "and nothing was asked" ([] :: [Value])
            =<< listAt "commands" answer
      -- RET IS DRY OVER AN OFFER THE WALK LANDED ON, and FINAL over the value
      -- taking it left standing: two presses, and the first writes nothing.
      insheet shell
              (pinned <> " press:C-c press:C-s dwhen:18_a press:C-n press:Enter") $
        \answer -> do
          assertEqual "the offer under point is taken" "18 april"
            =<< textAt "dwhen" answer
          assertEqual "and nothing else happened" ([] :: [Value])
            =<< listAt "commands" answer
          assertEqual "the widget stands" True =<< boolAt "ddateopen" answer
      insheet shell
              (pinned <> " press:C-c press:C-s dwhen:18_a press:C-n press:Enter press:Enter") $
        \answer ->
          assertEqual "and the same key over the finished term applies"
                      [("SCHEDULED", Just "18 april")] =<< plannedOf answer

    -- ONE WIDGET, BOTH DOORS: the pair box's value half, where its key routes.
  , testCase "the pair box's value half wears the same ghost" $ do
      insheet shell
              (pinned <> " press:f press:+ dkey:SCHEDULED press:: dval:18_aug") $
        \answer -> do
          assertEqual "the value half previews what it will land"
                      " \8594 <2026-08-18 Tue>" =<< textAt "dvghost" answer
          assertEqual "and a finished value carries no offers" [] =<< offersOf answer
      -- ITS OFFERS ARE DATES, hinted with what they RESOLVE TO -- the one thing
      -- a date vocabulary can do that the tree's property vocabulary cannot.
      insheet shell
              (pinned <> " press:f press:+ dkey:SCHEDULED press:: dval:18_a") $
        \answer ->
          assertEqual "the reader's own line, then the months it could be"
                      [ ("18 a", "new")
                      , ("18 april", "<2026-04-18 Sat>")
                      , ("18 august", "<2026-08-18 Tue>") ]
            =<< offersOf answer
      -- A KEY THAT ROUTES NOWHERE OWES NO DATE AND CARRIES NO GHOST.
      insheet shell (pinned <> " press:f press:+ dkey:OWNER press:: dval:18_aug") $
        \answer ->
          assertEqual "the drawer's own pair is a value like any other"
                      "" =<< textAt "dvghost" answer
      -- AND THE PHRASE REACHES THE WALL AS IT WAS TYPED: the box's own wall is
      -- the grammar's, or a phrase the server would accept is refused here.
      insheet shell
              (pinned <> " press:f press:+ dkey:SCHEDULED press:: dval:18_aug press:Enter") $
        \answer ->
          assertEqual "the raw phrase on the planning line"
                      [[["SCHEDULED", "18 aug"]]] =<< wroteAt "planning" answer

    -- THE DRIFT PIN'S CLIENT HALF.  The server's wall is asserted against this
    -- same file in `TestQuery'; here the PANE's ghost resolver is driven over
    -- every vector of it, so the two cannot part on one phrase without a red run.
  , testCase ("the ghost reads " <> dateCorpusPath <> " exactly as the wall does") $ do
      corpus <- dateCorpus
      assertBool ("the corpus carries the proposal's rows: " <> show (length corpus))
                 (length corpus >= 66)
      answer <- bootedPage shell "" "" 500 "Enter"
                  (T.unwords [ "date:" <> day <> "/" <> spelled typed
                             | (typed, day, _owed) <- corpus ])
      reading (\a -> assertEqual "the pane's reading, vector by vector"
                                 (map answerFor corpus) =<< textsAt "dateReads" a)
              answer

    -- ZERO RED FRAMES ON THE WAY IN.  The corpus above judges each phrase
    -- FINISHED; this walks every prefix of one, because a refusal that flashes
    -- mid-word and is gone by the last keystroke is invisible to a vector and
    -- plain to the reader typing it.  EVERY ACCEPTED VECTOR of the same shared
    -- file, so an arm the ghost cannot reach on the way in cannot hide behind a
    -- hand-kept list that never grew the phrase.
  , testCase "an accepted phrase draws no refusal on the way in" $ do
      corpus <- dateCorpus
      answer <- bootedPage shell "" "" 500 "Enter"
                  (T.unwords (map typing (nub [ typed | (typed, _day, Right _) <- corpus ])))
      reading (\a -> assertEqual "the prefixes the ghost answered in red"
                                 allowedFlashes =<< textsAt "dateFlashes" a)
              answer
  ]
  where
    -- THE CORPUS'S OWN REFERENCE DAY, spelled once: every act asking for a day
    -- asks for this one, or an answer moves with the calendar the suite runs on.
    refDay = "2026-08-22"
    pinned = "dateon:" <> refDay
    typing p = "dtyping:" <> refDay <> "/" <> spelled p
    -- ONE ALLOWANCE, and it PINS rather than excuses: a phrase the corpus
    -- REFUSES may be a proper prefix of one it accepts, and there the ghost is
    -- right to speak.  `from 30 dec 2026 to 2 jan' is the corpus's own inverted
    -- interval -- the year behind it is what un-inverts it -- so it flashes
    -- twice on the way into `... 2 jan 2027': once as typed, once with the
    -- space that starts the year.  Anything else here is a reading the pane
    -- owes and does not have.
    allowedFlashes = map inverted [ "from 30 dec 2026 to 2 jan"
                                  , "from 30 dec 2026 to 2 jan " ]
    inverted prefix = prefix <> " \8658 \10007 ends before it starts"
    -- The harness's own notation for an act's argument: `_' is a space, `~' a
    -- literal underscore -- so a vector carrying either survives the split.
    spelled = T.replace " " "_" . T.replace "_" "~"
    answerFor (_typed, _day, Right stamp) = stamp
    answerFor (_typed, _day, Left word)   = "\10007 " <> word

-- | The date widget's own offers as drawn, word and resolved hint together.
widgetOffers :: Value -> IO [(T.Text, T.Text)]
widgetOffers = offersIn "dwoffers"

intsAt :: T.Text -> Value -> IO [Int]
intsAt = decodedAt

-- | The settings sheet as keys: PANELS over the layers @\/config@ served, one box holding the SELECTED file's lines.
settingsSpec :: IO T.Text -> TestTree
settingsSpec shell =
  overBoot shell "," "" $ \settings ->
  testGroup "Shell settings"
  [ atBoot settings ", opens it over the layers the server serves" $ \answer -> do
        assertEqual "the sheet is up" "on" =<< textAt "settings" answer
        assertEqual "the first layer's lines, verbatim" "#+TODO: TODO | DONE"
          =<< textAt "cshown" answer
        assertEqual "the union is previewed" "TODO | DONE" =<< textAt "ceff" answer
        assertEqual "and it opens synced" "synced" =<< textAt "cstate" answer
        assertEqual "with nothing written" ([] :: [Value]) =<< listAt "configWrites" answer

    -- The server's order is the walk's, so the sheet's is its own: the fixture serves `film' ahead of `book'.
  , atBoot settings "the layers are a select: system first, then the tags in alphabet"
      $ \answer -> do
        assertEqual "system, then book, then film"
                    ["system", "tag:book", "tag:film"] =<< textsAt "clayers" answer
        assertEqual "opening on the first" "0" =<< textAt "cat" answer
        assertEqual "and the label names the file it is"
                    "system · /o/.org-glance/config/system.org · not created yet"
          =<< textAt "clab" answer

  , keyed shell "picking a layer swaps the box to that file's lines" "," "clayer:1" $ \answer -> do
        assertEqual "book's lines" "#+TODO: TODO READING | READ"
          =<< textAt "cshown" answer
        assertEqual "and book's label" "tag:book · /o/.org-glance/config/tags/book.org"
          =<< textAt "clab" answer
        assertEqual "with nothing written" ([] :: [Value]) =<< listAt "configWrites" answer

  , keyed shell "a switch away and back keeps the edit"
      "," "ctext:#+TODO:_A_|_B clayer:1 clayer:0" $ \answer -> do
        assertEqual "the edit is still there" "#+TODO:_A_|_B" =<< textAt "cshown" answer
        assertEqual "and nothing was written on the way" ([] :: [Value])
          =<< listAt "configWrites" answer

    -- READING A LAYER IS NOT EDITING IT: every layer's bytes go through the box and nothing may be written.
  , keyed shell "walking every layer and back writes nothing"
      "," "clayer:1 clayer:2 clayer:0 press:Escape" $ \answer -> do
        assertEqual "no write" ([] :: [Value]) =<< listAt "configWrites" answer
        assertEqual "the sheet is down" "" =<< textAt "settings" answer
  , keyed shell "and the box shows a layer's lines byte for byte" "," "clayer:2 clayer:1" $
        assertEqual "book's line, spacing and bar included"
                    "#+TODO: TODO READING | READ" <=< textAt "cshown"

    -- Every layer edited on the way is written, one drift-locked call per FILE.
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

    -- ONE list draws the tabs and the order, and a panel that is not showing is out of the flow with its fields.
  , atBoot settings "it is two panels, each named by its own tab" $
        assertEqual "ui, keywords" ["ui", "keywords"]
          <=< textsAt "csecs"

  , atBoot settings "and the sheet opens on the first of them" $
        assertEqual "ui" "ui" <=< textAt "ctab"

    -- THE THEME SELECT IS THE REGISTRY'S: a DERIVED oracle, so a hard-coded option here fails.
  , testCase "the theme select is one option per theme this build carries" $ do
      page <- shell
      holdsAll "auto leads, then the registry in its own order"
        (  ["<option value=\"auto\">auto</option>"]
        <> [ "<option value=\"" <> thId t <> "\">" <> thLabel t <> "</option>"
           | t <- themes ]) page

    -- EVERY POPUP HAS A URL: `?page=NAME' beside `q', with the panel as the FRAGMENT.
  , keyed shell "the settings sheet says so in the URL, panel and all"
      "," "ctab:ui" $ \answer -> do
        urlIs "the surface, and the panel it is showing"
              "?q=state%3A*active*&page=config#ui" answer

  , keyed shell "and closing it takes the parameter off"
      "," "press:Escape" $ \answer ->
        urlIs "the query alone again" "?q=state%3A*active*" answer

    -- ONE WRITER means one door at each end: a raise that wrote `?page=' and no close left it standing.
  , keyed shell "the capture form says so in the URL"
      "+" "" $ \answer ->
        urlIs "the surface" "?q=state%3A*active*&page=capture" answer

  , keyed shell "and closing the capture form takes the parameter off"
      "+" "press:Escape" $ \answer ->
        urlIs "the query alone again" "?q=state%3A*active*" answer

  , keyed shell "a tab shows its own panel and no other"
      "," "ctab:ui" $ \answer ->
        assertEqual "the ui panel" "ui" =<< textAt "ctab" answer

  , keyed shell "TAB walks the panels and wraps"
      "," "press:Tab" $ \answer ->
        assertEqual "one on from ui" "keywords" =<< textAt "ctab" answer
  , keyed shell "and S-TAB walks back, wrapping the other way"
      "," "press:S-Tab" $ \answer ->
        assertEqual "the last panel" "keywords" =<< textAt "ctab" answer
  , keyed shell "two presses come home"
      "," "press:Tab press:Tab" $ \answer ->
        assertEqual "ui again" "ui" =<< textAt "ctab" answer

    -- WHICH theme the hues describe is DERIVED from the reader's own pick; the table is by LAYER, then cycle order.
  , keyed shell "the states table is every keyword the tree knows, by layer"
      "," "ctab:ui" $ \answer -> do
        -- A word TWO layers declare is TWO rows: a state belongs to a file.
        assertEqual "every layer's cycle, in its own order" ["system|TODO|active|", "system|DONE|inactive|"
                    , "tag:book|TODO|active|", "tag:book|READING|active|"
                    , "tag:book|READ|inactive|", "tag:film|WATCHING|active|"
                    , "tag:film|WATCHED|inactive|"]
          =<< textsAt "chues" answer

    -- A colour is the TREE's, so it lands in `system.org''s line whatever layer the state belongs to.
  , keyed shell "RET edits a state's colour, and it rides the system write"
      "," "ctab:ui sat:TODO press:Enter sfields://#7B1FA2 press:Enter press:Escape"
      $ \answer -> do
        assertEqual "carrying the one hue, flat"
                    [["light", "TODO", "#7B1FA2"]]
          =<< coloursOf =<< oneConfigWrite answer

  , keyed shell "and the colour column follows the theme on screen"
      "," "ctab:ui sat:TODO press:Enter sfields://#7B1FA2 press:Enter theme:dark"
      $ \answer -> do
        assertEqual "dark names no hue of its own yet" ["system|TODO|active|", "system|DONE|inactive|"
                    , "tag:book|TODO|active|", "tag:book|READING|active|"
                    , "tag:book|READ|inactive|", "tag:film|WATCHING|active|"
                    , "tag:film|WATCHED|inactive|"]
          =<< textsAt "chues" answer
        assertEqual "and nothing was written on the way" ([] :: [Value])
          =<< listAt "configWrites" answer

    -- ONE FIELD PER THEME: the colour config is keyed by theme, so a form with one
    -- field would edit whichever theme was on and leave the other on a palette slot.
  , keyed shell "a state's hue is asked for once per theme, and both are written"
      "," "ctab:ui sat:TODO press:Enter sfields://#7B1FA2/#D0A0FF press:Enter press:Escape"
      $ \answer -> do
        assertEqual "carrying a line's worth per theme"
                    [["light", "TODO", "#7B1FA2"], ["dark", "TODO", "#D0A0FF"]]
          =<< coloursOf =<< oneConfigWrite answer

  , keyed shell "and the form reads back both, whichever theme is on"
      "," "ctab:ui sat:TODO press:Enter sfields://#7B1FA2/#D0A0FF press:Enter theme:dark sat:TODO press:Enter"
      $ \answer ->
        assertEqual "the two hues, beside the name and the group"
                    ["TODO", "active", "#7B1FA2", "#D0A0FF"] =<< textsAt "sfields" answer

  , keyed shell "+ adds a state to its layer's cycle"
      "," "ctab:ui sat:TODO press:+ sfields:WAITING/active/ press:Enter press:Escape"
      $ \answer -> do
        assertEqual "the cycle carries it now" ["#+TODO: TODO WAITING | DONE"]
          =<< textsAt "lines" =<< oneConfigWrite answer

  , keyed shell "dd removes a state from its layer's cycle"
      "," "ctab:ui sat:TODO press:d press:d press:Escape" $ \answer -> do
        assertEqual "the cycle is short one keyword" ["#+TODO:  | DONE"]
          =<< textsAt "lines" =<< oneConfigWrite answer

  , keyed shell "an untouched states table rides no write"
      "," "ctab:ui press:Escape" $ \answer ->
        assertEqual "pristine, so nothing went" ([] :: [Value])
          =<< listAt "configWrites" answer

  , atBoot settings "and it opens on seven, with nothing stored" $ \answer -> do
        assertEqual "the boot wrote the default" "7" =<< textAt "logn" answer
        assertEqual "and the key is not there" "«unset»" =<< textAt "logStored" answer

    -- THE BOOT READS THE PREFERENCE, which no act can reach: the browser has to arrive remembering one.
  , keyedWith shell "glance-log=21" "" 500 "a browser that remembers one boots at it"
      "" "" $ \answer -> do
        assertEqual "the cap is the stored one" "21" =<< textAt "logn" answer

  , keyedWith shell "glance-log=900" "" 500 "a stored value outside the band boots at the default"
      "" "" $
        assertEqual "the default" "7" <=< textAt "logn"

  , keyed shell "the theme panel applies and persists without closing the sheet"
      "," "theme:dark" $ \answer -> do
        assertEqual "stamped on the document element" "dark" =<< textAt "theme" answer
        assertEqual "and remembered" "dark" =<< textAt "themeStored" answer
        assertEqual "the sheet is still up" "on" =<< textAt "settings" answer
        assertEqual "and nothing was written" ([] :: [Value])
          =<< listAt "configWrites" answer

    -- `auto' is the attribute coming OFF rather than a third value written into it.
  , keyed shell "and auto takes the attribute back off" "," "theme:dark theme:auto" $ \answer -> do
        assertEqual "no attribute" "" =<< textAt "theme" answer
        assertEqual "but the choice is remembered" "auto" =<< textAt "themeStored" answer

    -- THE ZOOM IS THE WINDOW'S, and a browser tab already owns these three keys.
    -- `run' is reached only past a `preventDefault', so declining is something
    -- only `live' can do: the rows are dead where no window stands.
  , keyed shell "with no window behind the page the zoom keys are left alone"
      "C-+ C-- C-0" "" $ \answer -> do
        assertEqual "nothing was posted" ([] :: [T.Text]) =<< textsAt "zoomed" answer
        assertBool "a key the browser owns was claimed"
          . all (`notElem` ["C-+", "C--", "C-0"]) =<< textsAt "prevented" answer
        assertEqual "and no level was stored" "«unset»" =<< textAt "zoomStored" answer

  , keyed shell "and the settings row says whose the zoom is"
      "," "" $
        assertEqual "the browser's, and the keys that reach it"
                    "the browser's own · C-+ / C-- / C-0 reach it directly"
          <=< textAt "czoom"

    -- ONE POST AT BOOT, since the window opens at its own level and the page is
    -- the only side that remembers one.
  , keyedIn shell "native" "" "a window behind the page is worn at boot"
      "" "" $ \answer -> do
        assertEqual "the default, said once" ["1"] =<< textsAt "zoomed" answer
        assertEqual "and nothing stored, since it is the default" "«unset»"
          =<< textAt "zoomStored" answer

  , keyedIn shell "native" "glance-zoom=150" "a remembered level is worn at boot"
      "" "" $
        assertEqual "the stored level, as a level" ["1.5"] <=< textsAt "zoomed"

    -- The band is the SERVER's; a stored value outside it is CLAMPED where the
    -- log's height is declined, because a press at the ceiling is still a press.
  , keyedIn shell "native" "glance-zoom=900" "a remembered level outside the band lands on the edge"
      "" "" $ \answer -> do
        assertEqual "the ceiling" ["3"] =<< textsAt "zoomed" answer
        assertEqual "and written back inside it" "300" =<< textAt "zoomStored" answer

  , keyedIn shell "native" "" "C-+ steps a tenth up, says the level and remembers it"
      "C-+" "" $ \answer -> do
        assertEqual "the boot's, then the press's" ["1", "1.1"]
          =<< textsAt "zoomed" answer
        echoIs "named as the command it is" "C-+ → text-scale-increase (110%)" answer
        assertEqual "remembered as a whole percent" "110" =<< textAt "zoomStored" answer

    -- The step is the level's own tenth, so the ladder compounds the way a browser's does.
  , keyedIn shell "native" "" "and again from where it left off" "C-+ C-+" "" $ \answer -> do
        assertEqual "110, then 121" ["1", "1.1", "1.21"] =<< textsAt "zoomed" answer
        echoIs "" "C-+ → text-scale-increase (121%)" answer

    -- `+' WANTS THE SHIFT on most layouts, which is why the unshifted key is bound too.
  , keyedIn shell "native" "" "C-= is the same command" "C-=" "" $ \answer -> do
        assertEqual "one step up" ["1", "1.1"] =<< textsAt "zoomed" answer
        echoIs "" "C-= → text-scale-increase (110%)" answer

  , keyedIn shell "native" "" "C-- steps a tenth down" "C--" "" $ \answer -> do
        assertEqual "the boot's, then the press's" ["1", "0.91"]
          =<< textsAt "zoomed" answer
        echoIs "" "C-- → text-scale-decrease (91%)" answer

  , keyedIn shell "native" "glance-zoom=290" "a press at the ceiling stays at the ceiling"
      "C-+ C-+" "" $ \answer -> do
        assertEqual "held at 300" ["2.9", "3", "3"] =<< textsAt "zoomed" answer
        echoIs "" "C-+ → text-scale-increase (300%)" answer

    -- Blank REMOVES the key, the log height's own reading of "back to the default".
  , keyedIn shell "native" "glance-zoom=150" "C-0 puts it back, and forgets the level"
      "C-0" "" $ \answer -> do
        assertEqual "the boot's, then 100%" ["1.5", "1"] =<< textsAt "zoomed" answer
        echoIs "" "C-0 → text-scale-set (100%)" answer
        assertEqual "the key is gone" "«unset»" =<< textAt "zoomStored" answer

  , keyedIn shell "native" "glance-zoom=150" "and the settings row reads the level back"
      "C-+" "press:," $
        assertEqual "the level, and the keys that move it"
                    "165% · C-+ / C-- / C-0" <=< textAt "czoom"

    -- A `SELECT' inside a popup KEEPS the focus, and closing the popup is how the keys come back.
  , keyed shell "the sheet's theme select keeps the keys away from the table"
      "," "theme:dark press:n" $ \answer -> do
        assertEqual "the select holds the keyboard" "SELECT" =<< textAt "holding" answer
        rowIs "and the table did not move" "r1" answer
  , keyed shell "and closing it is what gives them back"
      "," "theme:dark press:Escape press:n" $ \answer -> do
        assertEqual "the sheet is down" "" =<< textAt "settings" answer
        assertEqual "nothing holds the keyboard" "" =<< textAt "holding" answer
        rowIs "and the key moved the cursor" "r2" answer

    -- The way out is the save, and only the layer that moved is written.
  , keyed shell "ESC syncs the layers that moved and closes"
      "," "ctext:#+TODO:_TODO_STARTED_|_DONE press:Escape" $
        \answer -> do
          wrote <- oneConfigWrite answer
          assertEqual "the system layer" "/o/.org-glance/config/system.org"
            =<< textAt "path" wrote
          assertEqual "its lines, as typed" ["#+TODO:_TODO_STARTED_|_DONE"]
            =<< textsAt "lines" wrote
          -- The empty digest is the pin an absent file carries, handed straight back.
          assertEqual "pinned to the digest it was read with" ""
            =<< textAt "digest" wrote
          assertEqual "and the sheet is down" "" =<< textAt "settings" answer

  , keyed shell "a pristine sheet closes without asking the server for anything"
      "," "press:Escape" $ \answer -> do
        assertEqual "no write" ([] :: [Value]) =<< listAt "configWrites" answer
        assertEqual "the sheet is down" "" =<< textAt "settings" answer

    -- `P' IS THE PIN, and it ASKS WHICH SAVED VIEW the applied query becomes.  Nothing is written by the raise.
  , keyed shell "P asks which saved view the applied query becomes"
      "" "press:P" $ \answer -> do
        assertEqual "the palette is up" "on" =<< textAt "prompt" answer
        assertEqual "naming what is being pinned" "pin · state:*active*"
          =<< textAt "phead" answer
        assertEqual "the registry in order, what each holds, then the reset flag"
                    [ ("[d]efault", "state:*active*")
                    , ("[a]genda", "state:*active* -planned:*empty* sort:scheduled")
                    , ("a[r]chive", "tag:*archive*")
                    , ("reset", "off · put a view's built-in back") ]
          =<< paletteHints answer
        assertEqual "and the question wrote nothing" ([] :: [Value])
          =<< listAt "configWrites" answer

  , keyed shell "and a letter pins the query into that view"
      "" "press:P press:d" $ \answer -> do
        wrote <- oneConfigWrite answer
        assertEqual "at the system path" "/o/.org-glance/config/system.org"
          =<< textAt "path" wrote
        assertEqual "carrying the applied query" "state:*active*"
          =<< wroteView "default" wrote
        assertEqual "and the server holds it now" "state:*active*"
          =<< textAt "served" answer
        echoIs "the pill names the view it landed in"
          "P → set-saved-view (default · state:*active*)" answer
        assertEqual "and the badge is on" True =<< boolAt "pinned" answer

  , keyed shell "ESC over the question pins nothing"
      "" "press:P press:Escape" $ \answer -> do
        assertEqual "nothing written" ([] :: [Value]) =<< listAt "configWrites" answer
        assertEqual "and the palette is down" "" =<< textAt "prompt" answer

    -- A palette no entry of which claims DEL is a surface with no inner ladder, so the backspace steps out.
  , keyed shell "DEL steps out of the question the way it leaves a popup"
      "" "press:P press:Backspace" $ \answer -> do
        assertEqual "nothing written" ([] :: [Value]) =<< listAt "configWrites" answer
        assertEqual "and the palette is down" "" =<< textAt "prompt" answer
        echoIs "and said so" "DEL → keyboard-quit" answer

  , keyed shell "DEL over the state palette still commits *empty*"
      "" "press:t press:Backspace" $ \answer -> do
        assertEqual "the keyword came off" [Nothing] =<< keywordsOf answer
        assertEqual "and the palette is down" "" =<< textAt "prompt" answer

    -- AND `-' IS A FLAG over that list, magit's own shape: armed, a letter puts that view's BUILT-IN back.
  , keyedAt shell "?q=tag%3Awork" 500 "- arms the reset, and a letter puts the built-in back"
      "" "press:P press:d press:P press:- press:d" $ \answer -> do
        writes <- listAt "configWrites" answer
        assertEqual "the pin, then the reset" 2 (length writes)
        assertEqual "the reset writes the empty query" ""
          =<< wroteView "default" (writes !! 1)
        assertEqual "and the server is back on the built-in" "state:*active*"
          =<< textAt "served" answer
        echoIs "the pill says which half ran, and what landed"
          "P → set-saved-view (default reset · state:*active*)" answer

  , keyed shell "- toggles, and the list under it does not move"
      "" "press:P press:-" $ \answer -> do
        assertEqual "the question says what a letter will do now"
                    "reset · which view" =<< textAt "phead" answer
        assertEqual "the views stand, and the rung says it is on"
                    [ ("[d]efault", "state:*active*")
                    , ("[a]genda", "state:*active* -planned:*empty* sort:scheduled")
                    , ("a[r]chive", "tag:*archive*")
                    , ("reset", "on · a letter puts the built-in back") ]
          =<< paletteHints answer
        assertEqual "and nothing written by the flag" ([] :: [Value])
          =<< listAt "configWrites" answer

  , keyed shell "and a second - puts the pin back"
      "" "press:P press:- press:-" $ \answer -> do
        assertEqual "the pin question again" "pin · state:*active*"
          =<< textAt "phead" answer
        assertEqual "nothing written" ([] :: [Value]) =<< listAt "configWrites" answer

    -- AND THE FLAG DIES WITH THE QUESTION IT WAS SET ON: a commit closes the palette.
  , keyedAt shell "?q=tag%3Awork" 500 "the flag is off again on the next raise"
      "" "press:P press:- press:d press:P" $ \answer ->
        assertEqual "the pin question, with the flag spent"
                    "pin · tag:work" =<< textAt "phead" answer

  , keyed shell "/ falls back to the completing read over the same list"
      "" "press:P press:/ type:agen press:Enter" $ \answer -> do
        assertEqual "into the view the typing left" "state:*active*"
          =<< wroteView "agenda" =<< oneConfigWrite answer

    -- EVERY REGISTRY ENTRY TAKES THE PIN, and the write names that view ALONE.
  , keyedAt shell "?q=tag%3Awork" 500 "another letter pins it into the agenda"
      "" "press:P press:a" $ \answer -> do
        wrote <- oneConfigWrite answer
        assertEqual "carrying the agenda alone" "tag:work"
          =<< wroteView "agenda" wrote
        assertEqual "the default view is not named" Nothing
          =<< (field "views" wrote >>= sparseTextAt "default")
        assertEqual "and the server holds the agenda now" "tag:work"
          =<< textAt "servedAgenda" answer
        assertEqual "with the default where it was" "state:*active*"
          =<< textAt "served" answer
        echoIs "and the pill names the agenda"
          "P → set-saved-view (agenda · tag:work)" answer

    -- The FIRST `a' is the pin palette's own which-key letter for the agenda view rather than this binding.
  , keyedAt shell "?q=tag%3Awork" 500 "A applies the agenda the pin just wrote"
      "" "press:P press:a press:g press:A" $ \answer ->
        urlIs "the freshly pinned agenda, not the built-in"
              "?q=tag%3Awork" answer

    -- THE SORT RIDES THE PIN, and `g' applies the LIVE pinned query rather than the boot-baked constant.
  , keyedAt shell "?q=tag%3Awork%20sort%3Adeadline" 500
      "the pinned view keeps its sort, and g applies it live"
      "" "press:P press:d press:g" $ \answer -> do
        writes <- listAt "configWrites" answer
        assertEqual "the write carries the order too" "tag:work sort:deadline"
          =<< wroteView "default" (head writes)
        urlIs "g applied the freshly pinned view, sort and all"
          "?q=tag%3Awork+sort%3Adeadline" answer
        assertEqual "and the badge held" True =<< boolAt "pinned" answer

    -- The query is the ONE carrier of a view — filter, order, column set — and nothing here knows a token from a token.
  , keyedAt shell "?q=tag%3Awork%20columns%3Astate%2Ctitle%20sort%3Adeadline" 500
      "the pinned view keeps its columns too, and g applies the whole view"
      "" "press:P press:d press:g" $ \answer -> do
        writes <- listAt "configWrites" answer
        assertEqual "the write carries filter, columns and order"
                    "tag:work columns:state,title sort:deadline"
          =<< wroteView "default" (head writes)
        urlIs "g applied the freshly pinned view whole"
          "?q=tag%3Awork+columns%3Astate%2Ctitle+sort%3Adeadline" answer
        assertEqual "and the badge held" True =<< boolAt "pinned" answer

    -- THE BADGE IS A BOOLEAN OVER THE APPLIED VIEW, off the moment the query diverges.
  , keyedAt shell "?q=tag%3Awork" 500 "the badge follows the applied view"
      "" "press:P press:d press:Backspace" $ \answer ->
        assertEqual "a diverged query takes the badge off" False
          =<< boolAt "pinned" answer

    -- `q' IS THE OTHER DOOR OUT OF A BROWSING POPUP, dired's own; the value palette keeps its letters.
  , keyed shell "q closes a browsing popup, and the palette keeps its letters"
      "" "press:o press:q" $ \answer -> do
        assertEqual "the link popup is gone" "" =<< textAt "popup" answer
        echoIs "and said so" "q → keyboard-quit" answer
  , keyed shell "q in the state palette is a letter, not a door"
      "" "press:t press:q" $ \answer ->
        assertBool "the palette stands" . not . T.null =<< textAt "prompt" answer

    -- A CLICK has no keydown behind it for the raising guard to spend, so this door clears it by hand.
  , keyed shell "the pin button asks the same question, and the next letter answers"
      "" "pinclick press:d" $ \answer -> do
        assertEqual "carrying the applied query" "state:*active*"
          =<< wroteView "default" =<< oneConfigWrite answer
        assertEqual "the badge is on" True =<< boolAt "pinned" answer
        assertEqual "and the echo names the command"
                    "pin → set-saved-view (default · state:*active*)"
          =<< textAt "echo" answer

    -- `typing()' is not what keeps the two sheets apart, so the refusal is stated in `openSettings'.
  , keyed shell "it will not open over the materialize sheet" "Enter" "blur press:," $ \answer -> do
        assertEqual "the settings sheet stayed down" "" =<< textAt "settings" answer
        assertEqual "and the subtree is still the one open" "on"
          =<< textAt "modal" answer

    -- AND IT IS A SURFACE WHILE IT STANDS, whether or not anything in it is focused.
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

    -- `C-x C-s' SYNCS MID-EDIT, so a flush that landed must leave the box exactly as the reader left it.
  , keyed shell "a sync that lands does not paint over what is being typed"
      "," "clayer:1 ctext:#+TODO:_A_|_B chang press:C-x press:C-s\
          \ ctext:#+TODO:_A_|_B_C cdeliver" $ \answer -> do
        assertEqual "one write went out" 1 . length =<< listAt "configWrites" answer
        assertEqual "and the keystrokes behind it stand" "#+TODO:_A_|_B_C"
          =<< textAt "cshown" answer
        assertEqual "the sheet is up" "on" =<< textAt "settings" answer

    -- WITH ONE BOX, a refusal SELECTS the file it refused: a message under another layer describes a file the reader cannot see.
  , keyed shell "a 409 selects the layer it refused and names it"
      "," "clayer:1 ctext:#+TODO:_A_|_B clayer:2 cmoved press:C-x press:C-s" $
        \answer -> do
          assertEqual "the sheet came back to book" "1" =<< textAt "cat" answer
          assertEqual "showing the edit that was refused" "#+TODO:_A_|_B"
            =<< textAt "cshown" answer
          assertContains "with the server's own words under it" "changed on disk"
            =<< textAt "clerr" answer
          assertEqual "and the sheet waits" "conflict" =<< textAt "cstate" answer
          strip <- logOf answer
          assertBool "the log names the refused layer"
            (any (T.isInfixOf "tags/book.org" . snd) strip)

    -- The label carries the DIGEST, so a layer this sheet just created must stop saying it is not there yet.
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

  , keyed shell "reverting an edit drops the refusal it earned"
      "," "ctext:#+TODO:_A_|_B cmoved press:C-x press:C-s\
           \ crevert press:C-x press:C-s" $
        \answer -> do
          assertEqual "one write, the refused one" 1 . length
            =<< listAt "configWrites" answer
          assertEqual "the line under the box is gone" "" =<< textAt "clerr" answer
          assertEqual "and the sheet is synced" "synced" =<< textAt "cstate" answer

    -- The sheet is a sibling of `#app' and outlives the remount by where it sits — a layout fact.
  , keyed shell "a view-changed remount leaves the sheet standing"
      "," "clayer:1 ctext:#+TODO:_A_|_B close:view-changed" $
        \answer -> do
          assertEqual "the mount was rebuilt" 2 =<< intAt "mounts" answer
          assertEqual "the sheet is still up" "on" =<< textAt "settings" answer
          assertEqual "with the edit still in it" "#+TODO:_A_|_B"
            =<< textAt "cshown" answer
          assertEqual "on the layer it was made in" "1" =<< textAt "cat" answer
  ]

-- | The event strip: the shape of a line, the ring, the counted repeat, and a write naming the rows it landed on.
logSpec :: IO T.Text -> TestTree
logSpec shell = testGroup "Shell log"
  [ -- The boot line is an ordinary line: the mount used to clear the strip, so
    -- a page's first second was gone the moment the table arrived.
    keyed shell "opens on a boot line the mount leaves alone" "" "" $ \answer -> do
        strip <- logOf answer
        assertEqual "one line, the boot's, wearing the view"
                    [("info", "boot", "loading … view: state:*active*")]
                    (map cut strip)
        assertBool ("a clock opens it: " <> show strip)
                   (all (stamped . stampOf . snd) strip)

    -- The severity is SPELLED uppercase in the line and WORN lowercase as its class.
  , keyed shell "every line is a stamp, a severity and a scope"
      "d q" "offline close:resync" $ \answer -> do
        strip <- logOf answer
        -- Every assertion below is quantified over the strip, so an EMPTY one would pass all four.
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

    -- Five hundred lines, and the OLDEST is what goes.
  , keyed shell "the ring holds five hundred and drops from the front" "" "spam:501" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "capped" 500 (length strip)
        assertEqual "the boot line and `line 0' are what went"
                    ["line 1", "line 2"] [ m | (_s, _c, m) <- take 2 strip ]
        assertEqual "and the newest stands" ["line 500"]
                    [ m | (_s, _c, m) <- drop 499 strip ]

    -- The one mutation an append-only strip allows: a message identical to the one before is counted on that line.
  , keyed shell "a repeat is counted on its line rather than written under it"
      "q q q" "" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "the boot line and one more" 2 (length strip)
        assertEqual "counted"
                    [("info", "cmd", "q quits the native window; a browser tab closes itself ×3")]
                    (drop 1 strip)

  , keyed shell "and only against the line it follows" "q d q" "" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "three lines under the boot's" 4 (length strip)
        assertEqual "the last says it once, uncounted"
                    "q quits the native window; a browser tab closes itself"
                    (message (last strip))

  , keyed shell "a dead daemon logs the failure and the retry"
      "" "offline close:resync" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "both, in that order"
                    [ ("error", "ws", "load failed: fetch failed")
                    , ("warn", "ws", "disconnected · retrying in 1s") ]
                    (drop 1 strip)

  , keyed shell "d names the row it flagged, and u names it unflagging one" "d u" "" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "the row, by its title"
                    [ ("info", "cmd", "headline \"one\" marked for deletion")
                    , ("info", "cmd", "headline \"one\" unmarked for deletion") ]
                    (drop 1 strip)

    -- One line per ROW rather than per request: a set spanning three files can come back two-thirds applied.
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

  , keyed shell "a refused write is an error line and names no landing"
      "" "refuse press:D" $ \answer -> do
        strip <- map cut <$> logOf answer
        assertEqual "the refusal, whole"
                    [("error", "cmd", "r1: a.org changed on disk")] (drop 1 strip)
  ]

logOf :: Value -> IO [(T.Text, T.Text)]
logOf answer = traverse one =<< listAt "log" answer
  where one v = (,) <$> textAt "sev" v <*> textAt "text" v

cut :: (T.Text, T.Text) -> (T.Text, T.Text, T.Text)
cut (sev, text) = case T.words text of
  (_stamp : _sev : scope : rest) -> (sev, scope, T.unwords rest)
  _shapeless                     -> (sev, "", text)

message :: (T.Text, T.Text, T.Text) -> T.Text
message (_sev, _scope, m) = m

stampOf, sevOf :: T.Text -> T.Text
stampOf = fromMaybe "" . listToMaybe . T.words
sevOf   = fromMaybe "" . listToMaybe . drop 1 . T.words

stamped :: T.Text -> Bool
stamped t = T.length t == 8 && T.index t 2 == ':' && T.index t 5 == ':'
            && T.all (\c -> isDigit c || c == ':') t

postedOf :: Value -> IO [(T.Text, [T.Text])]
postedOf answer = traverse one =<< listAt "commands" answer
  where one v = (,) <$> textAt "name" v <*> textsAt "ids" v

keywordsOf :: Value -> IO [Maybe T.Text]
keywordsOf = traverse (maybeTextAt "keyword") <=< argsOf

-- | The value palette as drawn: per ROW, its classes, the source it names, and the Active and Inactive entries.
paletteOf :: Value -> IO [(T.Text, T.Text, [T.Text], [T.Text])]
paletteOf answer = traverse one =<< listAt "plist" answer
  where one v = (,,,) <$> textAt "cls" v <*> textAt "source" v
                      <*> spelledAt "active" v <*> spelledAt "inactive" v
        spelledAt key = traverse spelled <=< listAt key
        spelled e = do
          key <- textAt "key" e
          word <- textAt "word" e
          pure (if T.null key then word else key <> " " <> word)

-- | Every badge hue the palette wrote.  The hues are the producer's and ride on the state column.
paletteHues :: Value -> IO [(T.Text, T.Text)]
paletteHues = fmap (filter (not . T.null . snd)) . paletteField "color"

paletteHints :: Value -> IO [(T.Text, T.Text)]
paletteHints = paletteField "hint"

paletteField :: T.Text -> Value -> IO [(T.Text, T.Text)]
paletteField key answer = do
  rows <- listAt "plist" answer
  entries <- concat <$> traverse halves rows
  traverse (\e -> (,) <$> textAt "word" e <*> textAt key e) entries
  where halves v = (<>) <$> listAt "active" v <*> listAt "inactive" v

-- | WHAT: the which-key assignment over CYCLE is EXPECTED, run as the pure function it is over no page at all.
assigns :: IO T.Text -> (T.Text, [T.Text]) -> Assertion
assigns shell (keywords, expected) =
  bootOf shell "" 500 "" ("assign:" <> keywords)
         (assertEqual (T.unpack keywords) expected <=< textsAt "assigned")

resolves :: IO T.Text -> ([T.Text], [T.Text], T.Text) -> Assertion
resolves shell (keys, cols, expected) =
  bootOf shell "" 500 ""
         ("cells:" <> T.intercalate "," keys <> "@" <> T.intercalate "," cols)
         (assertEqual (show keys <> " over " <> show cols) (Just expected)
            <=< maybeTextAt "span")

-- | The KEYS a popup's shape is resolved against, out of the SERVER's own declaration rather than a copy.
columnKeys :: [Value] -> IO [T.Text]
columnKeys = traverse (textAt "key")

-- | WHERE AN EDIT OVERLAY LANDS, resolved BY KEY: pure and order-only, so a column list that moves takes the box with it.
cellSpanSpec :: IO T.Text -> TestTree
cellSpanSpec shell = testGroup "Shell cell resolution"
  [ testCase "the two popups' own shapes, against the columns the server declares" $ do
      links <- columnKeys linkColumns
      tags <- columnKeys tagColumns
      -- `type' is derived and leads the list, which is exactly why the pair is not 0,1.
      resolves shell (["title", "url"], links, "1,2")
      resolves shell (["title"], tags, "0,0")

  , testCase "an unknown key resolves to nothing, so the placement is a no-op" $ do
      links <- columnKeys linkColumns
      resolves shell (["title", "nosuchcolumn"], links, "«none»")
      resolves shell (["nosuchcolumn"], links, "«none»")
      resolves shell ([], links, "«none»")

    -- The run is drawn EDGE to EDGE, so it is the columns' order rather than the shape's.
  , testCase "the run follows the columns' order, whatever order the shape spelled" $ do
      links <- columnKeys linkColumns
      resolves shell (["url", "title"], links, "1,2")
      resolves shell (["type", "url"], links, "0,2")
  ]

-- | SHELL's glue booted under node.  A machine with no node runs nothing and passes.
bootOf :: IO T.Text -> T.Text -> Int -> T.Text -> T.Text -> (Value -> Assertion)
       -> Assertion
bootOf shell = bootWith shell ""

keyed :: IO T.Text -> String -> T.Text -> T.Text -> (Value -> Assertion) -> TestTree
keyed shell label keys acts = testCase label . bootOf shell "" 500 keys acts

keyedAt :: IO T.Text -> T.Text -> Int -> String -> T.Text -> T.Text
        -> (Value -> Assertion) -> TestTree
keyedAt shell search total label keys acts =
  testCase label . bootOf shell search total keys acts

keyedWith :: IO T.Text -> T.Text -> T.Text -> Int -> String -> T.Text -> T.Text
          -> (Value -> Assertion) -> TestTree
keyedWith shell store search total label keys acts =
  testCase label . bootWith shell store search total keys acts

-- | 'keyed' with a WINDOW behind the page, and whatever it already remembers.
keyedIn :: IO T.Text -> T.Text -> T.Text -> String -> T.Text -> T.Text
        -> (Value -> Assertion) -> TestTree
keyedIn shell hosting store label keys acts check =
  testCase label (reading check =<< bootedIn hosting shell store "" 500 keys acts)

-- | The two doors a key run opens through: the TABLE, and the row's SHEET.
onTable :: IO T.Text -> T.Text -> (Value -> Assertion) -> Assertion
onTable shell = bootOf shell "" 500 ""

insheet :: IO T.Text -> T.Text -> (Value -> Assertion) -> Assertion
insheet shell = bootOf shell "" 500 "Enter"

-- | The three key scripts that put point INSIDE a list: the grain tree's outer run, its nested run, and the checkbox tree's run.  Each opens the sheet, enters the body with `f' and walks down to the list and steps into it.
intoRun, intoNestedRun, intoChecky :: T.Text
intoRun       = "grain press:Enter press:f press:n press:n press:n press:f"
intoNestedRun = intoRun <> " press:f"
intoChecky    = "checky press:Enter press:f press:n press:n press:f"

-- | THE ROOT STEPS INTO ITS CONTENTS, so the fixture's child is five `n' down:
-- the planning line, the drawer and the two paragraphs, then the child itself.
ontoChild :: T.Text
ontoChild = "press:n press:n press:n press:n press:n"

-- | 'bootOf' over a browser that already REMEMBERS something: a preference the BOOT reads is unreachable from an act.
bootWith :: IO T.Text -> T.Text -> T.Text -> Int -> T.Text -> T.Text
         -> (Value -> Assertion) -> Assertion
bootWith shell store search total keys acts check =
  reading check =<< bootedPage shell store search total keys acts

-- | The harness's answer to one boot, named apart so 'overBoot' can acquire ONE answer for a run of cases.
bootedPage :: IO T.Text -> T.Text -> T.Text -> Int -> T.Text -> T.Text
           -> IO (Maybe (Either String Value))
bootedPage = bootedIn ""

-- | 'bootedPage' with a WINDOW behind the page: @native@ stands the script-
-- message bridge up before the glue evaluates.  A boot fact like the stored
-- preference, and argv for the same reason — no act arrives early enough.
bootedIn :: T.Text -> IO T.Text -> T.Text -> T.Text -> Int -> T.Text -> T.Text
         -> IO (Maybe (Either String Value))
bootedIn hosting shell store search total keys acts = do
  node <- findExecutable "node"
  case node of
    -- SAY SO: a machine with no node ran every case green having asserted nothing at all.
    Nothing  -> Nothing <$ hPutStrLn stderr "\nSKIPPED - node is not on PATH: shell boot"
    Just exe -> do
      -- THE ONE FIXTURE, READ-ONLY: 'bootFixture' wrote it before the first
      -- case ran, and the harness only reads out of it.  A boot reached from
      -- outside that resource writes it itself rather than failing obscurely.
      dir <- bootDir
      written <- doesFileExist (dir </> "shell.js")
      unless written $ do
        createDirectoryIfMissing True dir
        writeFixtureTo dir =<< shell
      (code, out, err) <- readProcessWithExitCode exe
                            [ harness, dir, T.unpack search, show total
                            , T.unpack keys, T.unpack acts, T.unpack store
                            , T.unpack hosting ] ""
      pure . Just $ case code of
        ExitSuccess -> either (\e -> Left ("the harness answered: " <> e)) Right
                              (eitherDecode (BL.fromStrict (TE.encodeUtf8 (T.pack out))))
        _failed     -> Left ("the boot harness said: " <> err)

reading :: (Value -> Assertion) -> Maybe (Either String Value) -> Assertion
reading check = maybe (pure ()) (either assertFailure check)

-- | Run K under ONE boot.  Wraps a group rather than nesting one inside it, so no case is renamed or merged.
overBoot :: IO T.Text -> T.Text -> T.Text
         -> (IO (Maybe (Either String Value)) -> TestTree) -> TestTree
overBoot shell keys acts =
  withResource (bootedPage shell "" "" 500 keys acts) (const (pure ()))

atBoot :: IO (Maybe (Either String Value)) -> String -> (Value -> Assertion) -> TestTree
atBoot page label check = testCase label (reading check =<< page)

-- | The commands a held key delivers once, as the map declares them.  Named rather than spelled twice.
onceNames :: [T.Text]
onceNames = [ "filter-drop-token", "unmark-all", "mark-all"
            , "archive-flag", "org-glance-overview:delete"
              -- A held `x' re-raises its question over a page whose RET commits a write.
            , "dired-do-flagged-delete"
              -- A held pin re-raises its question over a page whose letters commit a config write.
            , "set-saved-view"
            , "org-glance-overview:open", "org-glance-agenda"
              -- A held `@' is a remount per repeat, each leaving a crumb behind.
            , "org-glance-overview:relations"
              -- A held priority key walks the ring round and lands wherever the repeat count leaves it.
            , "priority-up", "priority-down"
              -- And a held `^' re-sorts per repeat, landing on whichever direction the parity leaves.
            , "toggle-sort" ]

harness :: FilePath
harness = "test/fixtures/shell-harness.js"

-- | A claim about a page this server serves: strings it must carry, and strings it must not.
data Glue = Glue { glLabel :: String, glHas :: [T.Text], glGone :: [T.Text] }

glue :: String -> [T.Text] -> Glue
glue label has = Glue label has []

glueSpec :: IO T.Text -> TestTree
glueSpec shell = testGroup "Shell glue"
  ([ testCase glLabel $ do
       -- The fixture is page-plus-script, one universe, so a row may pin either side.
       b <- shell
       holdsAll glLabel glHas b
       holdsNone glLabel glGone b
   | Glue{..} <- shellGlue ]
   <> [ groundSweep shell, tierSweep shell, gridSweep shell, editIndentSweep shell
      , scrollSweep shell, containSweep shell, logColumnSweep shell
      , paletteSweep shell ])

-- | WHAT A SWEEP READS OUT OF THE SERVED PAGE, or the case fails naming what it wanted.
need :: String -> Maybe a -> IO a
need what = maybe (assertFailure ("no " <> what <> " in the page")) pure

-- | The paragraph rule's opening, spelled ONCE: two sweeps read the indent out of it, and a re-spelling breaks both.
paraIndent :: T.Text
paraIndent = "  .d-para,.d-comp,.d-meta{margin:7px 0;"

-- | THE EDIT BOX IS THE BLOCK, WEARING A DIFFERENT GROUND.  Asserted as RELATIONS over the declarations rather than as copied strings.
editIndentSweep :: IO T.Text -> TestTree
editIndentSweep shell = testCase "the paragraph's edit box is the block it covers" $ do
  page <- shell
  para <- need "the paragraph's indent"
               (between (paraIndent <> "\n    padding-left:") "}" page)
  box <- need "the edit box's rule" (between "  #dpara textarea{" "}" page)
  assertBool ("the box is indented by what the block is: " <> T.unpack box)
             (("padding-left:" <> para) `T.isInfixOf` box)
  assertBool ("the box takes the block's type: " <> T.unpack box)
             ("font:inherit" `T.isInfixOf` box)
  assertBool ("the box takes the grid inset: " <> T.unpack box)
             ("padding:1px var(--g-doc-pad)" `T.isInfixOf` box)
  assertBool ("the box takes the block's wrap: " <> T.unpack box)
             ("overflow-wrap:anywhere" `T.isInfixOf` box)
  mapM_ (\decl -> assertBool ("the box declares " <> T.unpack decl <> ": " <> T.unpack box)
                             (decl `T.isInfixOf` box))
        ["width:100%", "margin:0", "border:none", "resize:none"]
  assertEqual "a figure the box restates instead of reading" []
              [ n | n <- ["13px", "12px", "1.5 var", "padding:1px 6px", "min-height:2em"]
                  , n `T.isInfixOf` box ]
  -- REGISTRATION: the overlay is positioned against the pane's PADDING box while the text it covers sits in the CONTENT box.
  -- The pair's box takes the same fallback: both are laid over a whole row.
  span' <- need "the paragraph overlay's span" (between "  #dpara,#dpair{" "}" page)
  assertEqual "the paragraph overlay spans the pane's content box"
              "left:var(--g-doc-padx);right:var(--g-doc-padx)" span'
  -- AND THE EDIT IS INLINE: what grows is the BLOCK, and the cap is spelled in the shell and nowhere else.
  block <- need "the block's floor" (between "  .de.dat{" "}" page)
  assertBool ("the block's floor is the line count it was handed: " <> T.unpack block)
             ("min-height:calc(var(--g-doc-rows, 0)" `T.isInfixOf` block)
  assertEqual "a metric the floor restates instead of reading" []
              [ n | n <- ["13px", "1.5 "], n `T.isInfixOf` block ]
  assertBool "the pane's inset is one name, read by both"
             ("padding:var(--g-doc-pady) var(--g-doc-padx)" `T.isInfixOf` page)
  assertBool "the placement takes the pane's border and scroll back out"
             ("a.top - b.top - pane.clientTop + pane.scrollTop" `T.isInfixOf` page)
  -- FOCUS DRAWS NO LINE: the document's box is read as text and must not grow one.
  focus <- need "the box's focus rule"
                (between ("  #dpara textarea:focus,#dtin:focus,#dpair input:focus,"
                            <> "#ddate input:focus{") "}" page)
  assertEqual "a line the document box would grow on focus" []
              [ n | n <- ["border-bottom-color", "border-bottom:"], n `T.isInfixOf` focus ]
  -- THE GROUND IS THE SIGNAL, and one the block is not already wearing.  THE DATE
  -- WIDGET TAKES IT BY JOINING THE LIST: it stands INSIDE the cursor row, and
  -- `--g-sel' is spent on both that row's wash and every field's text selection,
  -- so a widget with no ground of its own would select its entry in exactly the
  -- colour already behind it.
  ground <- need "the box's ground" (between "  #dpara,#dpair,#ddate,#dtitle{" "}" page)
  assertEqual "the edit ground is the page's input surface"
              "background:var(--g-surface)" ground
  -- …and the row lifts its own wash while one stands, so the two golds are never
  -- on one line.
  assertContains "the row at point drops its wash under an open widget"
    "  #mdoc.on.tight .de.dat{background-color:transparent}" page

-- | THE LOG'S SEVERITY AND SCOPE ARE COLUMNS, derived off the page's OWN @append@ calls rather than copied.
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

-- | ONE @scrollIntoView@, AND IT IS THE DOCUMENT'S — kept by COUNTING rather than by wording.
scrollSweep :: IO T.Text -> TestTree
scrollSweep shell = testCase "the one scrollIntoView is the document's own" $ do
  page <- shell
  code <- glueOf page
  assertEqual "exactly one call site" 1 (T.count "scrollIntoView(" code)
  assertEqual "named twice: the call, and the detect that guards it" 2
              (T.count "scrollIntoView" code)
  assertContains "and it is the document cursor's"
    "        row.scrollIntoView({ block: \"nearest\" });" page
  -- THE BAND IS CSS: `nearest' honours `scroll-margin', so the movement code measures nothing.
  assertContains "the band rides the elements" "  .de{scroll-margin-block:var(--g-doc-off);" page
  assertContains "three of the pane's lines"
    "    --g-doc-off:calc(3 * var(--g-doc-lh));" page
  assertContains "and the pane is set in those same two"
    "    font:var(--g-doc-fs)/var(--g-doc-lh) var(--dk-mono);" page

-- | A POPUP CLAMPS AT ITS BOUND AND SCROLLS INSIDE IT, and the CHAIN is what a stray declaration breaks silently.
-- Two links pinned by absence: the panes row's @overflow:hidden@, and NO PANE CARRYING A FLOOR.
containSweep :: IO T.Text -> TestTree
containSweep shell = testCase "every popup clamps, and scrolls inside" $ do
  page <- shell
  -- THE BOUND IS CAPPED: 90vh is the ceiling whatever the arithmetic works out to.
  assertContains "the bound caps at 90vh"
    "    --g-pop-max:min(90vh," page
  -- RULE-SCOPED and asserted first: a flat `isInfixOf' cannot say which rule answered, and EVERY rule the selector appears in is read.
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
  assertEqual "a floor under the working box, whose height is fixed" []
              [ line | line <- T.lines page, ".pop-sheet{" `T.isInfixOf` line
                     , "min-height" `T.isInfixOf` line ]
  assertEqual "and no pane declares a viewport floor at all" []
    [ line | line <- T.lines page
           , any (`T.isInfixOf` line) ["#mtext{", "#mdoc{", "#mpanes{"]
           , "min-height:" `T.isInfixOf` line
           , not ("min-height:0" `T.isInfixOf` line) ]
  where
    -- A flex child shrinks to its parent only with the floor taken off (@min-height:0@).
    clamps =
      [ -- The panes row: sized by the box, and CONTAINING what it is sized to.
        ("#mpanes", ["flex:1", "min-height:0", "overflow:hidden"])
      , ("#mtext", ["min-height:0"])
      , ("#mdoc", ["min-height:0", "overflow:auto"])
      , ("#tpane", ["min-height:0", "overflow:hidden"])
      , ("#ltable", ["min-height:0", "overflow:hidden"])
      , ("#cbox", ["overflow-y:auto"])
      , ("#plist", ["max-height:40vh", "overflow-y:auto"])
      , ("#mlog", ["flex:0 0 auto", "max-height:22vh", "overflow:auto"])
      ]

-- | ONE GRID, ONE BASE, asserted as the relation between the two declarations rather than as two copied strings.
gridSweep :: IO T.Text -> TestTree
gridSweep shell = testCase "the star gutter and the body indent are one arithmetic" $ do
  page <- shell
  gutter <- need "the head's star gutter" (between "  .d-head .ds{width:calc(" ")}" page)
  para <- need "the paragraph's indent" (between paraIndent "}" page)
  base <- need "the document's base padding" (between "--g-doc-pad:" ";" page)
  assertEqual "the paragraph is padded by the base plus the gutter"
              ("padding-left:calc(var(--g-doc-pad) + " <> gutter <> ")")
              (T.strip (T.replace "\n" "" (T.replace "  " "" para)))
  assertContains "the base is the element's own inset"
                 "    padding:1px var(--g-doc-pad);" page
  assertBool ("the base is a length: " <> T.unpack base) (not (T.null base))

-- | POPUP SIZE IS A TIER: every box wears one of the three and no box rule declares a size.
-- | ONE PALETTE, TWO NAMESPACES, DERIVED: the values are read out of the served page and COMPARED.
paletteSweep :: IO T.Text -> TestTree
paletteSweep shell = testCase "one palette, two namespaces, every theme" $ do
  page <- shell
  let valuesOf name =
        [ T.dropWhileEnd (== ';') rest
        | line <- T.lines page
        , Just rest <- [T.stripPrefix (name <> ":") (T.strip line)] ]
  -- The count is asserted first: a palette that stopped being emitted would read as agreement between two empty lists.
  assertEqual "every theme declares the selection" 4 (length (valuesOf "--g-sel"))
  mapM_ (\(g, tv) ->
           assertEqual (T.unpack (g <> " is " <> tv))
                       (valuesOf g) (valuesOf tv))
        [ ("--g-bg", "--tv-bg"), ("--g-fg", "--tv-fg")
        , ("--g-surface", "--tv-alt"), ("--g-mute", "--tv-muted")
        , ("--g-border", "--tv-border"), ("--g-accent", "--tv-accent")
        , ("--g-sel", "--tv-sel"), ("--g-link", "--tv-link")
        , ("--g-col", "--tv-col"), ("--g-cell-wash", "--tv-cell-wash")
        , ("--g-bad", "--tv-flag"), ("--g-flag-wash", "--tv-flag-wash") ]
  -- A BADGE HUE IS THE THEME'S, so the wire carries a SLOT and the slots the served ROWS name are read off the view document.
  view <- get assetsDir "/headlines" >>= decoded
  cols <- listAt "columns" view
  named <- concat <$> mapM (\c -> do
             held <- sparseAt "badges" c
             case held of
               Nothing -> pure []
               Just _  -> mapM (textAt "color") =<< listAt "badges" c) cols
  assertBool "the badges name slots at all" (not (null named))
  -- Each colour is `var(--g-<value>, var(--g-<slot>))', and what is asserted is the FALLBACK.
  mapM_ (\colour -> do
           assertBool (T.unpack (colour <> " opens a fallback chain"))
                      ("var(--g-" `T.isPrefixOf` colour && ", var(--g-" `T.isInfixOf` colour)
           let slot = T.dropEnd 2 (snd (T.breakOnEnd ", var(" colour))
           assertBool (T.unpack (slot <> " is declared by every theme"))
                      (length (T.breakOnAll (slot <> ":") page) >= 4))
        named
  -- The slot COUNT is the wire's, so a theme's own list is cycled to fill it.
  mapM_ (\(token, n) ->
           assertEqual (T.unpack (token <> " is declared once per theme"))
                       (4 * n)
                       (sum [ length (T.breakOnAll (token <> T.pack (show i) <> ":") page)
                            | i <- [0 .. n - 1] ]))
        [ ("--g-state-a", stateSlots), ("--g-state-i", stateSlots)
        , ("--g-priority-", prioritySlots) ]
  -- The renderer's palette blocks carry no specificity (`:where'), so these ordinary rules win.
  renderer <- TIO.readFile "assets/table-view.js"
  mapM_ (\needle -> assertContains "the renderer's palette is a default" needle renderer)
        [ ":where(.tv-root){"
        , ":where(:root[data-theme=\"dark\"] .tv-root){"
        , ":where(:root[data-theme=\"light\"] .tv-root){" ]

tierSweep :: IO T.Text -> TestTree
tierSweep shell = testCase "every popup wears one size tier, and declares none" $ do
  page <- shell
  mapM_ (\(box, tier) ->
           assertContains "the box wears its tier"
                          ("id=\"" <> box <> "\" class=\"" <> tier <> "\"") page)
        tiers
  mapM_ (\tier -> assertContains "the tier is defined" ("." <> tier <> "{") page)
        (nub (map snd tiers))
  assertEqual "no tier beyond the two the list names" []
              [ t | t <- ["pop-wide", "pop-fullscreen", "pop-compact", "pop-eighty"]
                  , ("." <> t <> "{") `T.isInfixOf` page ]
  -- WHAT IT SWEPT IS ASSERTED FIRST: a box with no rule to read is a failure rather than a silent pass.
  let swept = [ (box, body) | (box, _tier) <- tiers, Just body <- [ruleIn ("#" <> box) page] ]
  assertEqual "the sweep found a rule for every box it names"
              (map fst tiers) (map fst swept)
  assertEqual "a box that declares its own size" []
              [ (box, prop)
              | (box, body) <- swept
              , prop <- ["width:", "height:"]
              , prop `T.isInfixOf` body ]
  -- ONE TOP LINE, and every backdrop reads it, so growth is downward and no box runs off the bottom.
  assertContains "the anchor is declared once" "--g-pop-top:5vh;" page
  assertContains "and what it leaves is derived from it"
                 "--g-pop-max:min(90vh," page
  -- SYMMETRIC: the foot margin is the head's, derived from the anchor rather than spelled as a second figure.
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
  mapM_ (\needle -> assertContains "a tier bounded by the anchor's room" needle page)
        [ ".pop-band{width:min(560px,100%);max-height:var(--g-pop-max)}"
        , ".pop-sheet{width:min(80vw,100%);height:var(--g-pop-max)}" ]
  where
    -- The tag manager wears the BAND: three short columns are narrower than the palette.
    -- EVERY TIERED BOX THE REGISTRY NAMES, so a surface added there is swept by
    -- itself; the hand-written five left `#kbox' and `#nbox' unlooked at.
    tiers = [ (puBox p, tierClass (puTier p)) | p <- popups, puTier p /= Untiered ]

-- | THE CURSOR IS THE GROUND THE TABLE'S CURSOR WEARS and a FLAG IS A MARK, swept rather than listed.
groundSweep :: IO T.Text -> TestTree
groundSweep shell = testCase "the cursor grounds its own line, a flag marks it, and neither draws a line" $ do
  page <- shell
  -- EVERY SELECTION IS A GROUND: no underline, outline or border on either.
  let bodies = [ (sel, body) | sel <- selectors, Just body <- [ruleIn sel page] ]
  mapM_ (\(sel, body) ->
           mapM_ (\decl -> assertBool
                    (T.unpack sel <> " draws a " <> T.unpack decl <> ": " <> T.unpack body)
                    (not (decl `T.isInfixOf` body)))
                 ["underline", "outline", "border", "text-decoration"])
        bodies
  -- A FLAG CARRIES NO GROUND, its mark being the branch it takes.
  mapM_ (\body -> assertBool ("the flag grounds its row: " <> T.unpack body)
                            (not ("background" `T.isInfixOf` body)))
        [ body | Just body <- [ruleIn ".de.dfl" page] ]
  -- THE CURSOR GROUNDS ITS OWN LINE, and a row drawn INSIDE it takes the page's back:
  -- a nested item is drawn inside its parent, so the ground would run the subtree.
  cursor <- need "cursor ground" (ruleIn "#mdoc.on .de.dat" page)
  assertBool ("the cursor grounds " <> T.unpack cursor)
             ("background-color:var(--g-sel)" `T.isInfixOf` cursor)
  kid <- maybe (assertFailure "nothing gives the page's ground back to a nested row") pure
               (ruleIn "#mdoc.on .de.dat:not(.d-comp) .de" page)
  assertBool ("a row inside point grounds " <> T.unpack kid)
             ("background-color:var(--g-bg)" `T.isInfixOf` kid)
  -- THE MARK IS THE ROW'S OWN COLUMN: thin over what the row carries, bold over the
  -- line it owns, and a flag says the same in red.
  -- ANY RULE THE SELECTOR OWNS, since a row at point takes both a ground and an ink.
  mapM_ (\(sel, ink) -> do
           let bodies' = rulesIn sel page
           assertBool ("no " <> T.unpack sel <> " rule in the page") (not (null bodies'))
           assertBool (T.unpack sel <> " paints no " <> T.unpack ink <> ": "
                         <> T.unpack (T.intercalate " | " bodies'))
                      (any (ink `T.isInfixOf`) bodies'))
        marks
  where
    selectors = [".de.dat", ".de.dfl"]
    -- A LIT BAR IS A STEP SHORT OF THE PAGE'S INK, so it leads the eye without
    -- competing with the words beside it: `--g-mark' is what every lit bar takes.
    marks = [ ("#mdoc", "--g-mark:color-mix(in srgb, var(--g-fg) 68%, var(--g-bg))")
            , ("#mdoc.on .de.dat", "var(--g-mark)")
            , ("#mdoc.on .de.dat .de", "var(--g-mark)")
            , (".de.dfl", "var(--g-bad)")
            , (".de", "var(--g-point-off)")
            , ("#mdoc.on .up", "var(--g-mark)")
            , ("#mdoc.on .sib", "var(--g-mark)")
            , (".blk.sp-0", "var(--g-mark)")
            , ("#mdoc.on .focus .de", "var(--g-point-off)")
            , (".d-list .d-item::before", "var(--ink)") ]

-- | The body of the first rule whose SELECTOR LIST names SEL.  GROUPED SELECTORS ARE THE POINT: a literal @"#pbox{"@ matched none.
ruleIn :: T.Text -> T.Text -> Maybe T.Text
ruleIn sel = listToMaybe . rulesIn sel

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

  -- SWAP ON THE ANSWER: the two-phase fetch is the BOOT's, and a re-application asks for the whole answer.
  , glue "a view already on screen is replaced in one mount"
      [ "const swap = !!table;"
      , "viewing(load(swap ? asking(asked) : `${narrow}limit=${PAGE}`)).then((a) => {"
      , "else arm(a.total);" ]

  -- THE WASH: the view reason is STEPPED, since an abort overlaps its replacement; the socket's is SET.
  , Glue "the wash is one holder over two reasons"
      [ "const WASH = { view: 300, socket: 400 };"
      , "      n: { view: 0, socket: 0 }, at: { view: 0, socket: 0 },"
      , "      step(why, by) { this.want(why, this.n[why] + by); },"
      , "wash.step(\"view\", 1);"
      , "return p.finally(() => wash.step(\"view\", -1));"
      , "backoff = 1000; wash.want(\"socket\", 0);"
      , "wash.want(\"socket\", 1);"
      , "document.documentElement.classList.toggle(\"stale\"," ]
      -- The page reads the class nowhere: the look is the stylesheet's whole business.
      [ "classList.contains", "wash.on.view ?", "if (wash.on" ]

  -- ONE property: no blur, and no `filter' of any kind — a filter would make `#app' the containing block for the fixed palette backdrop.
  , Glue "the wash dims the table and the overlays, and exempts what explains"
      -- EVERY VEILED SURFACE, in the order `Popups.popups' names them: the list is
      -- joined from the registry, so a surface added there joins the wash by itself.
      [ "html.stale #app,html.stale #modal,html.stale #prompt,html.stale #config,"
          <> "html.stale #links,html.stale #tags,html.stale #capture,"
          <> "html.stale #mint{opacity:.55}"
      , "#app,#modal,#prompt,#config,#links,#tags,#capture,#mint"
          <> "{transition:opacity .18s ease}" ]
      [ "html.stale #log", "html.stale #kbd"
      , "html.stale #echo", "html.stale body", "stale #app{filter", "filter:blur"
      , "filter:saturate", "filter:grayscale" ]

  , glue "a bare boot opens on the active view"
      [ "let bootedOn = savedQuery(\"default\");"
      -- A `q' in the address bar is the reader's own, empty or not.
      , "      const q = params().has(\"q\") ? urlQuery() : bootedOn;"
      , "const asked = (query = bootQuery());"
      , "if (!params().has(\"q\")) remember(asked);"
      , "initialQuery: query," ]

  -- A BLIND BOOT ADOPTS THE TREE'S DEFAULT once the walk lands: `remember' has
  -- written `q' back by then, so the guard reads the door's own URL, pinned
  -- before boot wrote anything -- and the applied config says itself aloud.
  , glue "a blind boot adopts the tree's default view and says its config"
      [ "const bootHadQ = params().has(\"q\");"
      , "if (now === was || bootHadQ || query !== was) return;"
      , "logConfig(cfg);"
      , "append(\"boot\", \"info\", `config layer ${l.path}`"
      , "if (v.query) append(\"boot\", \"info\", `view ${v.id}: ${v.query}`);" ]

  -- A paint under a query arms nothing, so the parity baseline is fetched once behind the table.
  , glue "the parity baseline is armed even when the boot was filtered"
      [ "function arm(total) {", "if (!query || all.length) return;"
      , "load(\"\").then((a) => { all = a.view.rows || []; parity(total); })"
      , "arm(a.total); })"
      , "else arm(a.total);" ]

  , glue "hands the filter to the server and aborts stale fetches"
      [ "onFilter: filter", "new AbortController()", "inflight.abort()"
      , "signal: inflight.signal", "load(asking(query))"
      -- One spelling of the query string, so a revalidation cannot be answered 304 against other rows.
      , "const asking = (q) => (q ? `?q=${encodeURIComponent(q)}` : \"\");"
      , "e.name !== \"AbortError\""
      , "      if (named) { applyNamed(named); return; }" ]

  , glue "an empty answer to a virtual key is checked locally"
      [ "function parity(total)", "if (total !== 0 || !query || !all.length) return;"
      , "TableView.parseQuery(query, keys)"
      , "t.key === null && !t.quoted && !t.negated && !t.added"
      -- An added token widens its own axis, so a `+' is the sign rather than a
      -- key the asset dropped: a correct zero is no skew.  The current asset
      -- says so with `added'; the leading `+' reads the same off a stale one.
      , "!t.value.startsWith(\"+\")"
      , "filter parity divergence — asset/daemon version skew"
      , "console.warn(note, { query, server: total, local })"
      , "if (!query) all = rows;" ]

  -- Present-and-empty is a reader who took the filter off; absent is a page nobody has filtered, and only that gets the default.
  , Glue "the applied query lives in the URL, an empty one included"
      [ "history.replaceState(null, \"\", `?${p.toString()}${location.hash || \"\"}`);"
      , "p.set(\"q\", q);"
      , "new URLSearchParams(location.search)"
      , "const urlQuery = () => params().get(\"q\") || \"\";"
      , "const asked = (query = bootQuery());"
      , "table.stripLastToken()", "const left = table.getQuery().trim();"
      , "commit(left);" ]
      ["p.delete(\"q\")"]

  -- `openFilter' is mode-agnostic, so the one call covers an asset in any of them.
  -- `omnibox' is the PICKER's, whose filter cannot summon a centred overlay over
  -- the caret it hangs at.  The main table's box is summoned onto the CHIP
  -- STRIP's own row, which `filterDock: "strip"' below pins — so there is no
  -- must-not-appear half.
  , glue "the filter is summoned rather than resident"
      [ "filterDock: \"strip\","
      , "const summons = () => can(table, \"openFilter\");"
      , "if (summons()) { table.openFilter(door); return; }"
      -- The field is named once, since the fallback, the restore and the stash all want it.
      , "(document.querySelector(\"#app .tv-filter\"));"
      , "const box = filterBox();"
      , "if (box) selectWhole(box);"
      , "summon the filter box onto the chip strip" ]

  -- TWO DOORS ONTO ONE QUERY, and the page opens them through ONE raise: the
  -- narrow flag is the session's, so `/' hands the option over on every press
  -- and `.' hands none.  What the doors DO is the renderer's and is driven in
  -- the browser suite; the needles here are the wiring that reaches it.
  , Glue "`/' opens the filter half and `.' the whole expression"
      [ "const focusFilter = () => raiseFilter({ narrow: true });"
      , "const focusQuery = () => raiseFilter();"
      -- The command the keymap names, in the map the dispatch reads handlers out of.
      , "applyDefault, pinView, relations, focusFilter, focusQuery, toggleRaw, openSettings,"
      -- The refusal reaches the page as a mount option, and only the MAIN table's.
      , "onRefused: refused,"
      , "String(spelling || \"\").replace(/^[-+]/, \"\").split(/[:=]/)[0];"
      , "`${shapingKey(spelling)}: autocomplete restricted, this key belongs to #'compose (kbd \".\")`"
      , "append(\"filter\", \"info\", note);" ]
      -- No second query and no splitter here: the renderer refuses the token and
      -- this page is told which one, so the glue holds no list of shaping keys.
      [ "const SHAPING", "shapingKeys", "narrowQuery", "filterHalf" ]

  -- Marking is the renderer's: this page holds no set and asks for the count rather than keeping one.
  , Glue "marks are the renderer's, and m/u/U are this page's keys"
      -- What the keys DO is asserted by driving them; the needles here are what behaviour cannot show.
      [ "marks: true,"
      , "let on = table.toggleMark(id);"
      , "· ${table.markedCount()}`);"
      , "flagHelp: \"d/D archive · u unflag\"," ]
      -- `getMarked()' is not a copy: a command asks the renderer which rows are marked at the moment it runs.
      ["let marked", "const marked = new Set", "marks.add", "marks.has"]

  -- The rule lives in ONE pure function, so a letter drawn and a letter honoured cannot drift.
  , Glue "the which-key letters are one pure function's answer"
      [ "function whichKeys(labels) {"
      , "function letterAt(label, at) {"
      -- Folded into each entry once and IN PLACE, so the drawing and the dispatch read one field.
      , "        pool[i].key = letterAt(pool[i].label, cut);"
      -- The pool is the entries with no key of their own, so `*empty*' spends none of it.
      , "      const pool = list.filter((c) => !c.fixed);"
      -- A badge hue is written inline, so it has to be told to give way under the fallback's cursor row.
      , "#plist .pat .pw{color:var(--g-fg)!important}"
      -- The claimed letter is marked INSIDE the keyword and nowhere else, in that state's own badge hue.
      , "const hot = part(word, \"b\", \"\", c.label[c.cut]);"
      , "if (c.color) hot.style.textDecorationColor = c.color;"
      , ".pw b{font-weight:700;text-decoration:underline;"
      , "text-decoration-thickness:2px;text-underline-offset:2px}"
      , "if (!prompting.narrow && c.fixed) part(row, \"span\", \"pk\", c.key);"
      -- Both modes commit through one call, so the letter and the fallback's RET are the same delivery.
      , "else if (!repeating(e)) takeChoice(hit);"
      , "else if (k === \"RET\") { const now = promptNow(); takeChoice(now.shown[now.at]); }" ]
      ["const LETTERS", "confirm(", ".pw u{", "part(word, \"u\"", ".pk.off{"
      , "\"pk off\""]

    -- WHAT BEHAVIOUR CANNOT SHOW HERE: which call declares which vocabulary.
    -- The palette's RET has ONE source now — the shown list — so the typed line
    -- reaches a commit by being drawn, and the closed callers draw none.
  , Glue "an open field draws the typed line, and openness is spelled at the call"
      [ "const NEW_HINT = \"new\";"
      , "mine.open = vocabulary === \"open\";"
      -- THE FOLD-EQUALITY TEST IS ONE PREDICATE both widgets read, and it is
      -- asked of the WHOLE vocabulary the filter left rather than of what a cap
      -- drew: a word the tree really spells coincides with its entry however it
      -- ranks, so it never draws itself `new'.
      , "const leadTyped = (typed, words) => {"
      , "return !!want && !words.some((w) => String(w).toLowerCase() === want);"
      -- The palette's leading literal, and the pair box's, one rule twice drawn.
      , "const literal = prompting.open && leadTyped(typed, shown.map((c) => c.label));"
      , "? [{ label: typed, tag: typed, hint: NEW_HINT }].concat(shown) : shown;"
      , "const minted = leadTyped(typed, words);"
      , "return (minted ? [{ word: typed, hint: NEW_HINT }]"
      -- The three call sites, each saying which vocabulary it opened over.
      , "addable(), \"RET adds it · C-n/C-p walks · ESC leaves\", addTag, \"open\");"
      , "}, \"open\");"
      , "(c) => insertCode(at, to, String(c.tag || \"\")), \"closed\");" ]
      -- The free-text back door that committed a value no entry ever drew.
      ["freely", "prompting.wider &&", "|| { tag:"]

  -- The hairline between two source rows is that row's own top border rather than a divider element.
  , Glue "the palette's hairlines are the table's own borders"
      [ "#plist.ptable{display:grid;"
      , ".ptable>.pr{display:grid;grid-template-columns:subgrid;grid-column:1/-1}"
      , ".pr+.pr{border-top:1px solid var(--g-border)}"
      , ".ph,.ps{font-size:11px;color:var(--g-mute)}"
      , ".pr.pm{grid-template-columns:1fr}" ]
      [".psep", "stateChoices", "x.cell ===", "c.at ==="]

  -- The tag union is FIRST-SEEN: an alphabetical insert would take a row out from under the cursor.
  , Glue "the tag union is first-seen, and the refresh is the answer"
      [ "for (const r of ttargets) for (const t of r.tags)"
      , "if (seen.indexOf(t) === -1) seen.push(t);"
      -- `/command' never writes the store, so a re-read here would answer with what the files said BEFORE the write.
      , "const landedIds = (results) =>"
      , "new Set((results || []).filter((x) => x.ok).map((x) => x.id));" ]
      ["seen.sort(", "tagsOf(over", "tagsOf(prompting", "tagsOf(ttargets"]

  -- The tags popup is a MOUNT and a mutable one, with the rename overlay laid over the tag CELL.
  , Glue "the tags popup is a mutable mount with a rename overlay"
      [ "const TCOLS = "
      , "tmount = listing(\"ttable\", TCOLS, \"d/D remove · u unflag\", \"tpane\");"
      , "const managing = () => !!tagging;"
      , "cells: [\"title\"], cols: TCOLS,"
      , "const renaming = () => { const e = editNow(); return !!e && e.o === TROW; };"
      , "openOver(TROW, tagAt(), \"org-rename-tag (no tag)\")"
      -- The write is ONE command over the tag the overlay OPENED on rather than the one under the cursor.
      , "renameTag(edit.row, el(\"tname\").value);"
      , "fire(tagging, \"rename-tag\", over.map((r) => r.id), { from, to },"
      , "`retagged ${args.from}→${args.to}`" ]
      -- The names below coming back means two implementations are live.
      [ "tagChoices", "tagVocabulary", "tagCommit", "landedTags", "letterMode"
      , "prompting.sticky", "a letter toggles it", "prompting.letters"
      , "trows", "tagRows()", "placeTag", "shutRename", "renamingFrom"
      , "function tflag" ]

  -- ONE EDIT OVERLAY: the class, the anchor, the blur and the SNAPSHOT are one
  -- implementation, and a shape declares its differences from it and no more.
  -- The doc pane declares FOUR of the seven -- title, paragraph, pair, date.
  , Glue "the edit overlay is one mechanism, seven shapes over four surfaces"
      [ "function openEdit(o, row) {"
      , "edit = { o, row };"
      , "el(o.box).className = \"on\";"
      , "o.fill(row);"
      , "o.focus(row);"
      -- The anchor is the SHAPE's: a mount names its root and selected row, the document names the element under point.
      , "const anchorOf = (o) => {"
      , "return m ? m.el.querySelector(\"tbody tr.tv-sel\") : null;"
      , "const tr = anchorOf(o);"
      -- The shape names BY KEY, resolved against the column list the server declared, so a column that moves takes the box.
      , "const span = o.cells && cellSpan(o.cells, o.cols);"
      , "const tds = span && [...tr.querySelectorAll(\"td:not(.tv-box)\")];"
      , "const from = tds && tds[span[0]], to = tds && tds[span[1]];"
      , "s.width = `${rt.right - l.left}px`;"
      -- The window resize is registered once rather than per mount.
      , "window.addEventListener(\"resize\", placeEdit);"
      -- THE SNAPSHOT: a commit reads the row the overlay OPENED over, never the cursor.
      , "const r = edit.row;"
      -- The seven, each named by the predicate or the commit that asks for it.
      , "const dediting = () => !!edit && edit.o === DTITLE;"
      , "const dparaing = () => !!edit && edit.o === DPARA;"
      , "const dpairing = () => !!edit && edit.o === DPAIR;"
      , "const ddating = () => !!edit && edit.o === DDATE;"
      , "const sediting = () => !!edit && edit.o === SROW;"
      -- SHARING THE STATE MUST NOT SHARE THE SHUTTER: an unscoped shut would cancel another surface's open edit.
      , "function shutEdit(o) {"
      , "if (!edit || edit.o !== o) return;"
      , "for (const o of shapes) shutEdit(o);"
      , "cancelEdit(when ? \"the planning line\" : pair ? \"the drawer\" : \"element\","
      , "cancelEdit(\"tag\", TROW)"
      , "cancelEdit(\"link\", LROW)" ]
      [ "drows[docAt()]", "function place()", "function shutRename"
      , "shutEdit();" ]

  -- What is read here is the three things behaviour cannot show from the outside.
  , Glue "the document is elements, cut where the outline under it begins"
      -- ONE OWNER PER BYTE: without the cut the same lines are a paragraph AND the child that owns them.
      [ "own: h.ownLines === undefined ? body.split(\"\\n\").length : h.ownLines,"
      -- The body a write sends is the ANSWER to the edit rather than a reconstruction on this side.
      , "const editPara = (r, text, say) => {"
      , "dcommit = say;"
      -- DEL IS UP, and at the top it is the sheet's door.
      , "if (editing.child === null) { leaveSheet(); return; }"
      , "reread(up === null ? undefined : up, (h, fresh) => {"
      -- A KEY THIS LISTENER CLAIMED IS NOT THE MAP'S, or the table's own `DEL' would strip a token on the same press.
      , "if (e.defaultPrevented) return;"
      , "let drows = [], dat = 0;"
      , "dflags = now.flags; dbody = now.body;"
      -- THE LIFTED HEADER RIDES THE SAME PUSH: the mirrors are the write's lists.
      , "dprops = now.properties; dplan = now.planning;"
      , ".d-list .d-item::before{top:0;bottom:0;width:1px;"
      , ".d-head,.d-child{display:flex;align-items:baseline;font-weight:600}"
      , ".dc-title{flex:1 1 auto;min-width:0}"
      , "margin-left:auto;margin-right:0}"
      -- PADDING rather than a margin: a margin would take the selection wash off the left of the line.
      , "el(\"mdoc\").style.setProperty(\"--g-doc-indent\","
      , "el(\"mdoc\").style.setProperty(\"--g-doc-indent\", String(\"* \".length));"
      , "padding-left:calc(var(--g-doc-pad) + var(--g-doc-indent, 2) * 1ch)}" ]
      -- The document is not a mount and never asks the renderer to draw it.
      [ "TableView.mount(el(\"mdoc\")", "TableView.mount(el(\"dlist\")" ]

  -- ONE `d'/`D'/`u' GESTURE over FOUR surfaces, each naming its phrases, its mount and what "take these" means.
  , Glue "the flag gesture is one implementation over four surfaces"
      [ "function flagKey(k, s, say) {"
      , "if (k === \"D\" || (k === \"d\" && flags.indexOf(at) !== -1)) {"
      , "if (can(m, \"clearFlags\")) m.clearFlags();"
      , "say(s.unflag);", "say(s.flag);", "say(s.none);", "say(s.missing);"
      , "none: \"org-delete-element (no element)\","
      , "unflag: \"delete-unflag (flag cleared)\","
      , "none: \"org-toggle-tag (no tag)\","
      , "unflag: \"tag-unflag (flag cleared)\","
      , "const XFLAGS = (b) => ({"
      , "flag: \"flagged — d again archives\","
      , "flagPress(k, e, DFLAGS)", "flagPress(k, e, TFLAGS)"
      , "archiveFlag: (b) => flagKey(\"d\", XFLAGS(b), (what) => said(b, what)),"
      , "archiveRows: (b) => flagKey(\"D\", XFLAGS(b), (what) => said(b, what)),"
      , "flagKey(\"u\", XFLAGS(b), (what) => said(b, what)); return; }" ]
      [ "function dflag", "function tflag", "d → delete-flag (d again deletes)"
      , "d → tag-flag (d again removes)"
      , "if (isFlagged(id)) { archive(b); return; }"
      , "said(b, \"flagged — d again archives\")"
      , "const flags = flagging() ? table.getFlagged() : [];"
      , "archiveRows: archive," ]

  -- The drill is probed with a COUNT — one row — since the number is the whole of what it reads.
  , glue "the drill is probed before it is applied"
      [ "load(`${asking(token)}&limit=1`).then((a) => {"
      , "if (!a.total) {"
      , "drill(b, token, name);" ]

  -- `typing()' is what keeps the shell's rows off the palette: every `table' row is dead while a field has focus.
  , Glue "the palette's lifecycle stays the renderer's"
      [ "const live = (b) => b.scope === \"any\""
      , "|| (b.scope === \"table\" && !typing())"
      -- The window's own scope closes the list, and is the ONE row that reads
      -- something other than the surfaces: is there a window behind this page.
      , "|| (b.scope === \"window\" && !!hosted(\"zoom\"));"
      , "a.tagName === \"INPUT\" || a.tagName === \"TEXTAREA\""
      , "cancel: () => {"
      , "else if (typing()) active().blur();" ]
      -- The CLASS, not the token: `--tv-veil' is the renderer's theming API and this page may not reach the element.
      ["closeFilter", ".tv-veil", ".tv-panel"]

  -- With `bootstrap=off' no `set-rows' frame can arrive, so the branch that applied one is gone.
  , Glue "opens a socket and applies the streaming ops"
      [ "new WebSocket(", "/ws?bootstrap=off", "table.setRows("
      , "\"upsert-row\"", "table.upsertRow(", "\"delete-row\"", "table.deleteRow("
      -- Under a filter a row frame is re-asked for rather than spliced, and the refetch lands the archive's anchor.
      , "setTimeout(() => fetchRows(settled), 250)" ]
      ["\"set-rows\""]

  -- A close costs rows; only the columns moving costs the mount.
  , Glue "a close is a reconnect, and only view-changed is a remount"
      [ "socket.onclose = (e) => {"
      , "if (e && e.reason === \"view-changed\") remount(); else resync();"
      , "function resync() {"
      , "if (!table) { start(); return; }"
      , "load(asking(asked), etag)"
      , "if (a.view && query === asked) { paint(a); settled(); }"
      , "listen();"
      , "setTimeout(resync,", "Math.min(backoff * 2, 30000)"
      -- The revalidation is this page's rather than the browser cache's, so the 304 comes back as the answer it is.
      , "init.headers = { \"if-none-match\": tag }; init.cache = \"no-store\";"
      , "r.status === 304 ? { view: null, total: 0 }"
      , "etag = r.headers.get(\"ETag\") || etag;"
      -- A daemon restarted while the page was away had no socket to send `view-changed' down.
      , "if (a.view && !sameColumns(a.view.columns || [])) { remount(); return; }"
      , "const sameColumns = (next) => JSON.stringify(next) === JSON.stringify(cols);" ]
      ["socket.onclose = () => {", "setTimeout(start,"]

  -- The sheet's digest is re-read rather than remembered, so a file that moved opens the conflict flow.
  , glue "a real remount carries the sheet and the palette across it"
      [ "function remount(after) { leaving = arriving = null; stash(); start(after); }"
      , "function stash() {"
      -- A structured sheet is never dirty — every element commits on its own.
      , "sheet: editing"
      , "? { id: editing.id, child: editing.child, raw,"
      , "at: docCursor().at,"
      , "open: openEditState(), digest: editing.digest }"
      , "palette: typedFilter(),"
      , "return box && active() === box ? box.value || \"\" : null;"
      , "function restore() {"
      , "if (box) { box.value = was.palette; box.focus(); }"
      , "if (was.sheet) reopen(was.sheet);"
      , "headline(s.id, s.child).then((h) => {"
      , "el(\"mtext\").value = s.text;"
      , "if (s.open) reopenEdit(s.open);"
      , "if (h.digest !== s.digest) sync(\"conflict\");"
      , "restore();" ]

  -- A cold daemon answers 503 while it walks; the boot and the reconnect both poll out of it.
  , glue "shows the indexing state and polls out of it"
      [ "r.status === 503", "{ indexing: b }", "if (e.indexing) return indexing("
      , "indexing … ${b.elapsed}s", "setTimeout(resync, 1000)" ]

  , glue "materializes a row and syncs it back"
      [ "\"materialize\"", "/headline?id=${encodeURIComponent(", "<textarea id=\"mtext\""
      , "method: \"POST\"", "flush(editing.digest)", "a.status === 409"
      , "keyboard-quit", "C-x C-s" ]

  -- ONE STRUCTURED PANE over the subtree, and the cut is the SERVER's: there is no parser on this side.
  , Glue "the sheet's one structured pane holds the lifted header"
      [ "<div id=\"mpanes\">", "<div id=\"mdoc\"><div id=\"dlist\"></div>"
      , "base = raw ? h.org : \"\";"
      , "docFill(h, raw);"
      -- THE HEADER RIDES THE FILL: planning and the drawer go into Elm as LISTS, and their rows are synthesized there.
      , "props: h.properties || [],"
      , "plan: h.planning || [],"
      , "planKeys: PLANNING,"
      -- THE DOCUMENT IS NOT A MOUNT: the renderer's list widget draws RECORDS and this is a list of KINDS.
      , "dport = Elm.Doc.init({ node: part(el(\"dlist\"), \"div\", \"\") }).ports;"
      , "drows = now.rows; dat = now.at;"
      -- CHILDREN ARE DRAWN WHOLE: every descendant, with its headline's line in body coordinates.
      , "kids: (h.children || []).map((c) =>"
      , "({ index: c.index, level: c.level, line: c.line,"
      -- TAB FOLDS, as it does in org: the model says whether anything did.
      , "once(() => dsay(k, { kind: \"tab\" }));"
      -- A drawer's own line is its frame; what RET edits is a pair inside.
      , "if (r.fold) { echo(\"RET → f reaches the rows inside — TAB folds\"); return; }"
      -- THE SAME RULE ONE GRAIN FINER: the planning line is a line of ENTRIES,
      -- and the entry point stands in rides the mirror by its KEYWORD, asked as a
      -- CAPABILITY so the shell names no row by id.
      , "if (r.entries) { planEnter(); return; }"
      , "dplankey = now.planKey || null;"
      , "planHere(docBinding(planCommand(dplankey)), dplankey);"
      -- `+' IN THE DRAWER TYPES THE PAIR IN PLACE: a row is drawn where the pair
      -- will stand, the two fields cover it, and `:' hands the key to its value.
      , "dsend({ kind: \"draftpair\" });"
      , "openEdit(DPAIR, { id: r.id, add: true, today: dateNow() });"
      , "if (onKey) { hop(); pairMoved(); return; }"
      , "dsend({ kind: \"addprop\", key, value });"
      -- THE COMMIT CARRIES ITS OWN CARGO: body and lists together, off the port.
      , "{ body: cargo.body, properties: cargo.properties, planning: cargo.planning },"
      , ": { body: dbody, properties: dprops, planning: dplan };"
      -- DIRTY IS THE LISTS AGAINST THEIR BASE; the body's own edits commit element by element.
      , "const edited = () => stamp(dprops, dplan);"
      , "const stamp = (props, plan) => JSON.stringify([props || [], plan || []]);"
      , "&& (raw ? el(\"mtext\").value !== base : edited() !== baseProps);"
      -- A deleted pair leaves through the LISTS, counted beside the body's own.
      , "if (answer.refused)"
      , "function drawLog(text) {"
      , "<pre id=\"mlog\"></pre>"
      -- Display-only: what goes back is the whole drawer, and this page never sends it.
      , ".split(\"\\n\").slice(1, -1).join(\"\\n\")"
      , "if (dirty()) { said(b, \"sync first — C-x C-s\"); return; }"
      , "reread(editing.child, (_h, fresh) => {"
      -- The doc pane holds the keys with NOTHING focused, so the map has to be told; it is the FIRST of the modal surfaces.
      , "{ name: \"sheet\", up: docHolds, edit: sheetOpen, shut: cancelSheetEdit,"
      , "return SURFACES.some((s) => s.up())"
      , "#mpanes{flex:1;min-height:0;overflow:hidden;"
      -- The open element's fields sit OVER the row; the document's box takes `font:inherit' so an edit renders in the PANE's line box.
      , "#dtitle,#dpara,#dpair,#ddate,#sedit,#tedit,#ledit{display:none;"
      , "#sedit input,#tedit input,#ledit input{"
      -- ONE FOCUS LANGUAGE: the browser can only dress the one pane that takes a real focus.
      , "#mtext:focus{outline:none;border-color:var(--g-accent)}"
      , "#mdoc.on{border-color:var(--g-accent)}" ]
      -- The property PANEL is gone whole: no second pane, no mount, no fields of
      -- its own -- and the two modal prompts `+' raised are gone with it.
      [ "tabindex", ".prow", "pcur", "drawRow", "addRow("
      , "mprops", "mptable", "pmount", "PCOLS", "drawProps", "pnav"
      , "pedit", "pkey", "pval", "props()", "planning()"
      , "enterPanel", "leavePanel", "PFLAGS"
      , "askText(\"property key\"", "value for :${key}:" ]

  , Glue "the page wears the default theme and the sheet wears Hack"
      [ "    --g-bg:#FFFFFF;", "    --g-fg:#000000;", "    --g-border:#E3E6EA;"
      , "  @media (prefers-color-scheme:dark){"
      , "      --g-bg:#000000;", "      --g-fg:#FFFFFF;", "      --g-sel:#373D4F;"
      , "background:var(--g-bg);color:var(--g-fg)"
      , "#mtext::selection{background:var(--g-sel);color:var(--g-fg)}"
      , "#mnote.conflict,#mnote.error{color:var(--g-bad)}"
      , "border:1px solid var(--g-border)"
      , "--dk-mono:\"Hack\", var(--glance-mono)"
      -- `--tv-link' is declared on `.tv-root', so a live `var()' read resolves to nothing in a pane beside the mount.
      , "--g-link:#30739B;", "--g-link:#7CC9F8;" ]
      -- ALIASED, NOT RESPELLED: a hex at a use site makes a renderer change N edits instead of one.
      [ "--g-border:#BDC3C7", "--g-border:#223959"
      , "color:#30739B", "color:#7CC9F8", "text-decoration:underline;color:#" ]

  -- One rule sets both widths.  ITS HEIGHT IS STATIC: N line boxes whatever it is holding.
  , Glue "the log wears the table's container under it, at a static height"
      [ "#app,#log{width:100%;box-sizing:border-box}"
      , "border:1px solid var(--g-border);border-radius:8px;"
      , "#app{flex:1 1 auto;min-height:0}"
      , "background:var(--g-surface);flex:none;overflow-y:auto}"
      -- N is a CUSTOM PROPERTY declared at the default, so the arithmetic is in one place.
      , "    --g-logn:7;"
      , "height:calc(var(--g-logn) * 1.5em + 2 * 6px + 2 * 1px);"
      , "box.scrollTop + box.clientHeight >= box.scrollHeight - 4"
      , "if (end) box.scrollTop = box.scrollHeight;" ]
      [ "#log:empty", "min-height:1.4em", "max-height:10em"
      , "max-height:calc(var(--g-logn)" ]

  , Glue "the log carries events and nothing the page shows anyway"
      [ "append(\"ws\", \"warn\", `disconnected · retrying in ${Math.round(backoff / 1000)}s`)"
      , "append(\"boot\", \"info\", `indexing … ${b.elapsed}s"
      , "append(\"ws\", \"error\", `load failed: ${e.message}`)"
      , "append(s.scope, \"info\", s.closed);"
      , "scope: \"sync\", state: \"synced\","
      , "closed: \"closed without writing — the file is as it was\","
      , "scope: \"config\", state: \"synced\","
      , "closed: \"settings closed — the files are as they were\","
      , "filter parity divergence — asset/daemon version skew"
      , "<div id=\"log\"></div>"
      , "`loading … ${opening ? `view: ${opening}` : \"all rows\"}`);"
      ]
      [ "const say = () =>", "say();", "getRows().length"
      , "matching ${query}", "${profile} keys"
      , "log(\"\")", "<div id=\"log\">loading …</div>" ]

  , glue "the log is a bounded ring of stamped lines"
      [ "const LOGCAP = 500;"
      , "new Date().toTimeString().slice(0, 8)"
      , "while (box.children.length > LOGCAP) box.removeChild(box.children[0]);"
      , "logLast.count.textContent = `×${(logLast.n += 1)}`;"
      , "String(message).replace(/[\\x00-\\x1f]+/g, \" \")"
      , "#log .warn .lv{color:var(--g-warn)}"
      , "#log .error .lv{color:var(--g-bad)}" ]

  -- `table-view.js' gives its sticky header `z-index:1' and its completion list `5'; an unnumbered backdrop painted under both.
  , Glue "the sheet's backdrop covers the renderer's chrome"
      [ "position:fixed;inset:0;z-index:100;", "position:relative;z-index:101;"
      , "#echo{position:fixed;right:14px;bottom:12px;z-index:2;" ]
      [ "z-index:3" ]

  , glue "the theme is a three-way switch the page honours"
      [ "id=\"themesel\""
      , "<option value=\"auto\">auto</option><option value=\"light\">light</option>"
      , "<option value=\"dark\">dark</option>"
      , ":root[data-theme=\"light\"]{", ":root[data-theme=\"dark\"]{"
      , ":root[data-theme=\"light\"] .tv-root{"
      , ":root[data-theme=\"dark\"] .tv-root{"
      , "if (name === \"auto\") delete document.documentElement.dataset.theme;"
      , "else document.documentElement.dataset.theme = name;"
      , "const themed = pref(\"glance-theme\", \"auto\");"
      , "el(\"themesel\").addEventListener(\"change\""
      , "<script>try{var v=localStorage.getItem(\"glance-theme\");" ]

  -- A value outside the band is declined rather than clamped, and blank is how a reader asks for the default back.
  , glue "the log's height is a stored preference no field reaches"
      [ "const LOG = CFG.log;"
      , "\"def\":7", "\"min\":1", "\"max\":50", "\"key\":\"glance-log\""
      , "if (!t) return LOG.def;"
      , "return /^[0-9]+$/.test(t) && +t >= LOG.min && +t <= LOG.max ? +t : null;"
      , "const logPref = pref(LOG.key, \"\");"
      , "localStorage.setItem(key, v)"
      , "el(\"log\").style.setProperty(\"--g-logn\", String(n));"
      , "setLogLines(logLines(logPref.get()) || LOG.def);"
      , "else localStorage.removeItem(key); } catch (e)" ]

  -- THE SECOND KNOB OF THAT SHAPE, and the one the keys move: the band is the
  -- SERVER's, so the page's clamp and the window's are one figure read twice.
  -- CSS zoom is absent on purpose — it would put the panes' measured rects out
  -- against the styles drawn from them.
  , Glue "the window's zoom is a stored percentage the keys move"
      [ "const ZOOM = CFG.zoom;"
      , "\"def\":100", "\"min\":50", "\"max\":300", "\"key\":\"glance-zoom\""
      , "\"step\":1.1"
      , "const zoomPref = pref(ZOOM.key, \"\");"
      , "const zoomBand = (n) => Math.max(ZOOM.min, Math.min(ZOOM.max, Math.round(n)));"
      , "return /^[0-9]+$/.test(t) ? zoomBand(+t) : ZOOM.def;"
      , "zoomPref.set(zoomAt === ZOOM.def ? \"\" : String(zoomAt));"
      , "if (door) door.postMessage(String(zoomAt / 100));"
      , "wearZoom(step > 0 ? zoomAt * ZOOM.step : zoomAt / ZOOM.step)"
      -- The stored level is worn at boot, and only where there is a window to wear it.
      , "if (hosted(\"zoom\")) wearZoom(zoomAt); else showZoom();"
      -- ONE SPELLING OF THE WINDOW TEST, and `q' goes through it too.
      , "const hosted = (name) =>"
      , "(window.webkit && window.webkit.messageHandlers"
      , "&& window.webkit.messageHandlers[name]) || null;"
      , "const host = hosted(\"quit\");"
      -- The row reads the level back and names the keys the MAP spells.
      , "id=\"czoom\"", ".cval{font:12px/1.5 var(--dk-mono)}"
      , "`${zoomAt}% · ${zoomKeys()}`"
      , "`the browser's own · ${zoomKeys()} reach it directly`"
      , "textScaleIncrease: (b) => said(b, `${zoomedBy(1)}%`)," ]
      -- CSS zoom in any spelling, and the window test spelled inline.
      [ "style.zoom", "transform:scale(", "window.webkit.messageHandlers.quit" ]

  -- THE KEYWORDS PANEL IS ONE SELECT AND ONE BOX: the text lives on the LAYER, which is what makes a switch free.
  , Glue "the keyword layers are a select over one box"
      [ "id=\"clayer\"", "<textarea id=\"ctext\" class=\"ctext\""
      , "crows = (b.layers || []).map(layerRow).sort(byLayer);"
      , "const byLayer = (a, b) => (a.tag === null ? 0 : 1) - (b.tag === null ? 0 : 1)"
      , "|| String(a.tag).localeCompare(String(b.tag));"
      , "crows[cat].text = el(\"ctext\").value;"
      , "el(\"clayer\").addEventListener(\"change\""
      , "const cdirty = () => (takeLayer(), crows.some(cmoved));"
      -- ONE LIST for every setting the sheet writes beside the cycle: `cmoved' and the flush fold `CFIELDS'.
      , "const cmoved = (r) => r.text !== r.base || cfmoved(r).length > 0;"
      , "const cfmoved = (r) => CFIELDS.filter((f) => f.on(r) && f.now(r) !== f.was(r));"
      , "<textarea id=\"ctpl\" class=\"ctext\""
      , "crows[cat].tpl = el(\"ctpl\").value;"
      , "tpl: layer.template || \"\", tplBase: layer.template || \"\","
      , "{ key: \"template\", on: () => true,"
      -- One POST per layer that moved, each under its own digest.
      , "if (!cmoved(r)) { r.err = \"\"; continue; }"
      , "postJSON(\"/config\", body)"
      , "for (const m of moved) body[m.f.key] = m.body;"
      -- A flush that refused nothing leaves the box alone: `C-x C-s' syncs mid-edit and a redraw would paint over the typing.
      , "if (landed === -1) landed = crows.indexOf(r);"
      , "      if (landed === -1) showAround();"
      , "      else { takeLayer(); showLayer(landed); }"
      , "+ (r.digest ? \"\" : \" · not created yet\") : \"\";" ]
      [ "createElement(\"textarea\")", "r.box.value", "r.note.textContent" ]

  , Glue "the dispatch and the echo widget read that blob and no other map"
      [ "<script id=\"keys\" type=\"application/json\">"
      , "JSON.parse(el(\"keys\").textContent)"
      , "MAPS.rows.filter(live)"
      , "HANDLERS[b.handler]" ]
      [ "MAPS.profiles", "MAPS.default", "glance-keys", "keysel", "setProfile" ]

  -- `g' reads the LIVE default, so a fresh pin is applied without a page reload.
  , Glue "the default view is the tree's, and `g' applies it"
      [ "const savedQuery = (id) => saved[id] || \"\";"
      , "applyView(b, savedQuery(\"default\"), undefined, here);"
      -- `g' is HOME rather than a step on the trail: the crumbs and their labels go with it.
      , "if (crumbing()) table.setCrumbs([]);"
      , "crumbLabels = {};"
      , "remember(q);"
      , "remount();" ]
      [ "function refresh()", "refreshing …", "org-glance-overview:refresh" ]

  -- The second canned view carries its own ORDER, which is a token of the query rather than a call behind the answer.
  , Glue "`a' is the agenda query through the same door, its own sort included"
      [ "    seedViews(CFG.views);"
      , "applyAgenda: (b) => applyView(b, savedQuery(\"agenda\"), (total) => landedAgenda(b, total)),"
      , "said(b, `agenda · ${rowsWord(total)}`);"
      -- The landing is an ARGUMENT of the boot it belongs to, so a boot that never lands leaves none behind.
      , "function start(after) {"
      , "if (after) after(a.total);" ]
      [ "agendaMode", "let agenda =", "sortKeys", "let landed"
      , "sortRows", "table.sortBy(" ]

  -- `o' follows the row: the extraction is the server's, and how many links come back decides the gesture.
  , Glue "`o' follows the row's links, and the server is what finds them"
      [ "const linksOf = (id) => getJSON(`/links?id=${encodeURIComponent(id)}`);"
      , "if (!links.length) { said(b, \"no links\"); return; }"
      , "if (links.length === 1) { openLink(b, links[0]); return; }"
      , "showLinks(b, id, a);"
      , "window.open(link.target, \"_blank\", \"noopener\");"
      , "append(\"cmd\", \"info\", `link ${JSON.stringify(link.target)} opened`);"
      , "lmount = listing(\"ltable\", LCOLS, \"\", \"lpane\");"
      , "const followable = (l) => FOLLOWABLE.indexOf(l.type) !== -1;" ]
      [ "\\\\[\\\\[", "linkAt("
      , "linkChoices", "a letter opens it", "c.target" ]

  -- `RET' WRITES one: `edit-link' over the SPAN the server handed out, pinned to the digest that answer carried.
  , Glue "the link popup edits in place, over the range the server gave it"
      [ "box: \"ledit\", pane: \"lpane\", fields: [\"ltitle\", \"lurl\"],"
      -- BY KEY against the column list the server declared, so reordering those columns takes the box with them.
      , "cells: [\"title\", \"url\"], cols: LCOLS,"
      , "const lediting = () => { const e = editNow(); return !!e && e.o === LROW; };"
      , "openOver(LROW, pointedRow(), \"org-insert-link (no link)\")"
      , "else if (k === \"RET\") commitLink(edit.row);"
      , "const args = { span: link.span, target };"
      -- ABSENT IS NOT NULL: only a description field the reader moved says anything.
      , "if (typed !== link.desc) args.desc = typed || null;"
      , "fire(b, \"edit-link\", [id], args," ]
      -- No offsets of its own and no re-read behind the commit: `/command' never writes the store.
      [ "arrives with the link span", "renderLink", "linksOf(lfor"
      , "link.span[0] +", "repaintLinks" ]

  , glue "a binding with no handler names what it is waiting for"
      [ "arrives with daemon commands (M4)" ]

  -- ONE ENVELOPE PER VERB: what a refusal looks like and what a body is sent as are each decided once.
  , Glue "the JSON verbs are written once"
      [ "const unwrap = (r) => r.json().then((b) => {"
      , "const getJSON = (url, extra) => fetch(url, extra).then(unwrap);"
      , "const postJSON = (url, body, extra) =>"
      , "headers: { \"content-type\": \"application/json\" },"
      , "const outcome = (r) => r.json().then((b) => ({ status: r.status, body: b }));"
      , "postJSON(at(id, child), { ...asked, digest }, extra);"
      , "const postCommand = (body) => postJSON(\"/command\", body).then(unwrap);" ]
      -- The must-not-appear half was a botched edit: a Haskell list separator
      -- had leaked INTO the needle, so no page could ever carry it.  What is
      -- left above pins the one spelling.
      []

  -- THE SUBTREE WRITE'S ANSWER, once: a 200 re-pins the digest, and under it is one ladder for every refusal.
  , glue "one ladder answers every subtree write"
      [ "function landed(h, onOk) {"
      , "const commitDoc = (cargo) => {"
      , "dport.docBody.subscribe(commitDoc);"
      , "function commitDocWith(cargo, say) {"
      , ".then((a) => { if (editing === h && landed(h, say)(a)) reload(); })"
      , ".then(landed(h, () => {" ]

  -- THE SHARED READINGS: a mount's cursor as an id, the TAB hop off the shape's field list, and the log verb as a table.
  , Glue "the page reads a cursor, a hop and a verb in one place each"
      [ "const selectedId = (mount) =>"
      , "(can(mount, \"getSelection\") ? (mount.getSelection() || {}).id : null) || null;"
      , "const at = selectedId(lmount);"
      , "const at = selectedId(tmount);"
      , "function hop() {"
      , "const at = ids.findIndex((id) => el(id) === active());"
      , "const VERBED = {"
      , "const verbed = (name, args, verb) => (VERBED[name] || ((_args, v) => v))(args, verb);"
      , "const what = verbed(name, args, verb);"
      , "for (const s of SURFACES) if (s.momentary && s.up()) s.off();" ]
      [ "pmount.getSelection().id", "(lmount.getSelection() || {}).id"
      , "(tmount.getSelection() || {}).id"
      , "active() === el(\"pkey\") ? el(\"pval\")"
      , "active() === el(\"ltitle\") ? el(\"lurl\")"
      , "name === \"edit-link\" ? verb"
      , "if (linking()) shutLinks();" ]

  -- ONE LISTENER SHAPE FOR THE TWO BROWSING POPUPS: a key another listener has already CLAIMED is nobody else's.
  , Glue "the two browsing popups share one listener"
      [ "function popupKeys(name, mount, o) {"
      , "if (momentary() !== name || e.defaultPrevented) return;"
      , "popupKeys(\"links\", linkMount, {"
      , "popupKeys(\"tags\", tagMount, {"
      , "flagPress(k, e, TFLAGS)" ]
      [ "if (momentary() !== \"links\") return;"
      , "if (momentary() !== \"tags\" || e.defaultPrevented) return;" ]

  , glue "the follow gesture and the two askers are one each"
      [ "function followLinks(b, id, a, links) {"
      , "linksOf(id).then((a) => followLinks(b, id, a, a.links || []))"
      , "followLinks(b, editing.id, { digest: editing.digest, links }, links);"
      , "function askState(b, ids, title) {"
      , "function askTags(b, ids, title) {"
      , "const docTargets = (b, label, k) =>"
      , "k(b, [entryNow().id], `${label} · ${docTitle()}`);"
      , "setState: (b) => overTargets(b, \"set state\", askState),"
      , "manageTags: (b) => overTargets(b, \"tags\", askTags),"
      , "docTargets(docBinding(\"org-glance-overview:todo\"), \"set state\", askState);"
      , "docTargets(docBinding(\"org-agenda-set-tags\"), \"tags\", askTags);"
      , "function raise(title, state, cls, foot) {" ]

  -- Spans are CHAR offsets, so the pane counts characters rather than UTF-16 units.
  , Glue "the document counts characters and anchors what it drew"
      [ "const clen = (s) => Array.from(String(s)).length;"
      , "const bodyShift = (h) => clen(h.org || \"\") - clen(h.body || \"\");"
      , "const linksIn = (at, links) => (links || dlinks).filter((l) =>"
      -- The anchor is READ off what was drawn: `.dat' is not `#dlist''s `dat'-th child, a composite drawing its leaves inside itself.
      , "const docElAt = () => el(\"dlist\").querySelector(\".dat\");"
      , "const spanOf = (r) => (r && r.span) || null;" ]
      [ "(editing.org || \"\").length", "n + l.length, 0"
      , "text.slice(cut, a)", "at + text.length"
      , "el(\"dlist\").children || [])[dat]", "let dseq = 0" ]

  -- THE BOX'S TIER SURVIVES A RAISE: the mode is TOGGLED, where a wholesale write dropped the tier silently.
  , Glue "raising the palette keeps the box's tier"
      [ "el(\"pbox\").classList.toggle(\"narrow\", cls === \"narrow\");" ]
      [ "el(\"pbox\").className = cls;" ]

  -- EVERY VEIL IS A DOOR, and what a backdrop click does differs by surface.
  , glue "the momentary veils are backdrops too"
      [ "for (const id of [\"modal\", \"config\"])"
      , "if (e.target === el(id)) leaveSheet();"
      , "const backdrops = [[\"links\", () => shutLinks()], [\"tags\", () => shutTags()]];"
      , "if (e.target === el(id)) off();" ]

  -- ONE COMMAND AT A TIME: rows sharing a FILE are written under ONE drift lock.
  , Glue "a press that makes several commands sends them in turn"
      [ "async function cyclePriority(b, step) {"
      , "await fire(b, \"set-priority\", over, { priority: key || null },"
      , "async function removeTags(list) {"
      , "for (const tag of list)"
        -- Guarded, so a refusal on one tag does not abandon the tags behind it.
      , "await Promise.resolve(untag(tag)).catch(failed(tagging, \"remove-tag\"));" ]
      [ "for (const tag of list) untag(tag);" ]

  , glue "the echo widget is mounted, in Emacs wording"
      [ "<div id=\"echo\"", "#echo{position:fixed", "is undefined", "timed out"
      , "Enter: \"RET\"", "Escape: \"ESC\"", "ArrowUp: \"<up>\"" ]

  -- A row step is `selectStep': `getVisible()' is one page's worth, so arithmetic over it here would stop at a boundary.
  , Glue "row movement drives the renderer's own selection"
      [ "const steps = () => can(table, \"selectStep\");"
      , "if (visible().length) table.selectStep(step);"
      , "tbody tr.tv-sel", "table.getVisible()", "table.select(id, column())", ".tv-filter"
      , "if (cells()) return table.getSelection().id;" ]
      [ "tr.click()", "rowEls("
      , "box-shadow:inset 2px 0 0 var(--tv-accent)", "tr.tv-sel{box-shadow" ]

  -- `scrollIntoView' WAS on that forbidden list outright; `scrollSweep' below is where the rule went.

  , glue "the set is paged, and the brackets turn one"
      -- One number for the boot's limit and the renderer's page, so the first paint is exactly page one.
      [ "const PAGE = 100;   // rows in the first paint, and rows to a page"
      , "pageSize: PAGE,"
      , "nextPage: (b) => turnPage(b, 1),"
      , "previousPage: (b) => turnPage(b, -1),"
      , "if (step > 0) table.nextPage(); else table.previousPage();"
      , "said(b, `page ${at.page}/${at.pages}`);"
      , "wants(b, \"pager\", \"nextPage\", \"pageInfo\")" ]

  -- The buffer ends climb, and the landing is a select of its own in BOTH directions.
  , Glue "the buffer ends are progressive across pages"
      [ "firstRow: (b) => endStop(b, false),"
      , "lastRow: (b) => endStop(b, true),"
      , "const end = (rows) => rows[last ? rows.length - 1 : 0].id;"
      , "if (!pager() || focusedId() !== end(list)) {"
      , "if (!(last ? table.nextPage() : table.previousPage())) { said(b, \"\"); return; }"
      , "if (turned.length) table.select(end(turned), column());" ]
      ["const col = ", "let col = "]

  -- ONE CAPABILITY DOOR: the needles pin the CALL rather than the words, so the spelling changes here alone.
  , Glue "the capability door is one question and one refusal sentence"
      [ "const can = (mount, ...names) =>"
      , "names.every((n) => typeof mount[n] === \"function\")"
      , "const lacks = (what) => `this table-view.js has no ${what}`;"
      , "const wants = (b, what, ...names) =>"
      , "can(table, ...names) || (said(b, lacks(what)), false);" ]
      [ "said(b, \"this table-view.js has no"
      , "const strips = ", "const sorts = " ]

  -- The column is the renderer's to hold: the shell reads it back out of `getSelection()' and keeps no copy.
  , Glue "cell movement is that selection with a column, and no state here"
      [ "const column = () => (cells() ? table.getSelection().col : null);"
      , "nextColumn: (b) => moveCol(b, 1),"
      , "previousColumn: (b) => moveCol(b, -1),"
      , "const want = at === null ? 0 : at + step;"
      -- A whole row has no cell to its LEFT, so back out of one is a no-op.
      , "if (at === null && step < 0) { said(b, \"\"); return; }"
      , "table.select(id, want)"
      , "can(table, \"getSelection\")"
      , "wants(b, \"cell selection\", \"getSelection\")"
      , "if (handler) handler(b);" ]
      ["let col = ", "selCol", "lastColumn"]

  -- `^' IS A QUERY EDIT: the renderer writes the chain into the applied query, so the press arrives as an ordinary commit.
  , Glue "`^' promotes the column at point to the chain's head"
      [ "toggleSort: (b) => {"
      , "const at = column(), c = at === null ? null : cols[at];"
      , "if (!c) { said(b, \"no column selected — f/l to pick one\"); return; }"
      -- `sortPromote' is where `sortable' is enforced, so the refusal is read off the call and the key SPEAKS it.
      , "if (!table.sortPromote(c.key)) { said(b, `${named} does not sort`); return; }"
      , "const chain = table.getSort() || [], head = chain[0];"
      , "wants(b, \"sort\", \"sortPromote\")" ]
      [ "sortAt", "tv-arrow", "sortRows", "table.sortBy(" ]

  -- Walking off an end is a LANDING rather than a wall: the renderer reads an out-of-range column as no column.
  , Glue "the landing column is echoed by its header, or the row mode it left for"
      [ "const now = column();"
      , "said(b, now === null ? \"row mode\" : (cols[now].header || cols[now].key));"
      , "said(b, \"no row\")"
      , "cols = view.columns || [];"
      , "const keys = cols.map((c) => c.key);" ]
      -- The clamp this page used to keep, and must not grow back: it swallowed the key at a wall the renderer does not have.
      [ "at first", "at last", "want >= cols.length" ]

  -- The renderer hides an empty chip row with an inline `display:none', which `!important' outranks.
  , glue "a coarse pointer taps the chip row to summon the filter"
      [ "@media (pointer:coarse){"
      , "#app .tv-chips{min-height:44px;cursor:pointer}"
      , "#app .tv-chips:empty{display:flex!important;align-items:center}"
      , "content:\"filter …\""
      -- Delegated from #app so it survives a re-mount, through the same `focusFilter' the key runs.
      , "el(\"app\").addEventListener(\"click\""
      , "matchMedia(\"(pointer: coarse)\").matches"
      , "if (!coarse()) return;"
      , "t.closest(\".tv-chips\")"
      , "t.closest(\".tv-chip\")"
      , "focusFilter();" ]

  -- Under 16px, focusing a field zooms the page in and nothing zooms it back out.
  , glue "a coarse pointer gets fields iOS will not zoom into"
      [ "#mtext,#pinput,#dtin,#dpair input,"
      , "#sedit input,#tedit input,#ledit input,"
      , "#dpara textarea,"
      -- The DOCKED box is a field on this page's own row, so it takes the guard with them.
      , "#ktext,#app .tv-filter,"
      , ".ctext,.cview{font-size:16px}}"
      , "#mpanes{flex-direction:column}" ]

  -- These four boxes write ORG into the user's own files, so a remembered value, a
  -- capitalised first letter or a "corrected" quote would be a silent edit.
  , glue "the four document boxes decline every offer the browser makes"
      [ "<input id=\"dtin\" spellcheck=\"false\" autocomplete=\"off\""
          <> " autocapitalize=\"off\" autocorrect=\"off\">"
      , "<textarea id=\"dtext\" spellcheck=\"false\" autocomplete=\"off\""
          <> " autocapitalize=\"off\" autocorrect=\"off\"></textarea>"
      , "<input id=\"dkey\" spellcheck=\"false\" autocomplete=\"off\""
          <> " autocapitalize=\"off\" autocorrect=\"off\">"
      , "<input id=\"dval\" spellcheck=\"false\" autocomplete=\"off\""
          <> " autocapitalize=\"off\" autocorrect=\"off\">" ]

  -- THE SETTINGS SHEET IS UNREACHABLE ON A TOUCH DEVICE, a KNOWN GAP asserted from both sides.
  , Glue "the settings door a coarse pointer had went with the corner"
      [ "  @media (pointer:coarse){" ]
      [ "id=\"gear\"", "#gear{", "\9881" ]

    -- THE PLATFORM PAINTS THE `<select>', and a page that never declares its
    -- scheme gets the LIGHT control palette whatever its own colours are — with
    -- `color' inherited over it that is white on white, which is what the native
    -- WebKitGTK window drew.  It rides the palette blocks, so every theme says it.
  , glue "every palette block declares the scheme the platform paints controls in"
      [ "  :root{\n    color-scheme:light;"
      , "  @media (prefers-color-scheme:dark){\n    :root{\n      color-scheme:dark;"
      , "  :root[data-theme=\"light\"]{\n    color-scheme:light;"
      , "  :root[data-theme=\"dark\"]{\n    color-scheme:dark;" ]

  , glue "asks for one font stack, everywhere in the page"
      [ "--glance-mono:\"JetBrains Mono\", \"Fira Code\", \"SF Mono\", Menlo, Consolas, monospace"
      -- The renderer injects `.tv-root{font:…}' after this page's style element, so the extra selector step wins.
      , "#app .tv-root{font-family:var(--glance-mono)}"
      , "font:14px/1.5 var(--glance-mono)", "font:12px/1.5 var(--dk-mono)" ]

  -- The assets directory holds no font file, so the declaration must not be there to point at one.
  , Glue "with no font file to serve, says nothing about one" [] ["@font-face"]
  ]

-- | The window between @bind@ and the end of the startup walk: every route has an answer through it.
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
      -- Seconds, rounded to a tenth: a raw double would put fifteen digits on the page.
      elapsed <- field "elapsed" loading
      case elapsed of
        Number n -> do
          assertBool ("elapsed runs backwards: " <> show n) (n >= 0)
          assertEqual "elapsed is not rounded to a tenth"
                      (fromInteger (round (n * 10)) / 10) n
        other -> assertFailure ("expected a number of seconds, got " <> show other)
      q <- getFrom application' "/headlines?q=meeting&limit=10&offset=5"
      assertEqual "with parameters" 503 (status q)

  , testCase "materialize and commit wait for the load too" $ do
      application' <- indexingApp
      r <- getFrom application' (headlinePath "sample.org#0")
      assertEqual "GET /headline" 503 (status r)
      -- The 503 is the honest answer, and the retriable one.
      w <- postTo application' (headlinePath "sample.org#0") (commitBody "* x\n" "deadbeef")
      assertEqual "POST /headline" 503 (status w)
      assertEqual "retry" (Just "1") (header "Retry-After" w)

  , testCase "/ws says the same, so a client reconnects rather than mounts" $ do
      application' <- indexingApp
      r <- getFrom application' "/ws"
      assertEqual "status" 503 (status r)

    -- The resolution is the store's, so serving it early would answer for a row the walk has not reached.
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

    -- The layer list comes off the store's own `clDirs' — the config directories the WALK met.
  , testCase "/config waits for the walk, since the layers are what it found" $ do
      application' <- indexingApp
      r <- getFrom application' "/config"
      assertEqual "GET" 503 (status r)
      w <- postTo application' "/config" (configBody "/x.org" [] "")
      assertEqual "POST" 503 (status w)
      assertEqual "retry" (Just "1") (header "Retry-After" w)

  , testCase "the elapsed seconds are the load's age, rounded to a tenth" $ do
      -- 12.37 s sits inside the [12.35, 12.45) bucket, leaving the in-process request 80 ms before the answer moves.
      hub <- newLoadingHub . subtract 12.37 =<< getMonotonicTime
      r <- getFrom (application (served assetsDir) hub) "/headlines"
      assertEqual "status" 503 (status r)
      elapsed <- field "elapsed" =<< decoded r
      assertEqual "elapsed" (Number 12.4) elapsed

  , testCase "the shell and its assets are served the whole time" $ do
      application' <- indexingApp
      r <- ok =<< getFrom application' "/"
      assertContains "the shell names its script" "src=\"glue.js\"" (body r)
      js <- getFrom application' "/table-view.js"
      assertEqual "the renderer" 200 (status js)
      gl <- getFrom application' "/glue.js"
      assertEqual "the glue" 200 (status gl)
      assertContains "and it is the shell itself" "TableView.mount" (body gl)

  , testCase "the load landing opens the store routes, on the same server" $ do
      hub <- newLoadingHub =<< getMonotonicTime
      let application' = application (served assetsDir) hub
      before <- getFrom application' "/headlines"
      assertEqual "before" 503 (status before)
      finishLoading hub =<< loadStore viewDir
      after <- getFrom application' "/headlines"
      assertEqual "after" 200 (status after)
      assertEqual "the rows the walk found" (Just "6") (header "X-Glance-Rows" after)
      etagOf after >>= assertTreeTag "the store the walk landed" 0
  ]

indexingApp :: IO Application
indexingApp = application (served assetsDir) <$> (newLoadingHub =<< getMonotonicTime)

-- | @\/headlines@ is the facade's view document — the same 'Value' 'viewJSON' builds, so the server adds nothing to the wire.
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

-- | The startup banner.  Pure, so the one thing worth asserting costs no server: the first line names the SUBCOMMAND.
bannerSpec :: TestTree
bannerSpec = testGroup "Startup banner"
  [ testCase "names the subcommand that started it" $ do
      assertEqual "under serve" "glance serve — http://127.0.0.1:7777/"
                  (head (bannerLines "serve" opts True))
      assertEqual "under desktop" "glance desktop — http://127.0.0.1:7777/"
                  (head (bannerLines "desktop" opts True))

  , testCase "and says the same about the daemon under both" $ do
      assertEqual "every line but the first"
                  (tail (bannerLines "serve" opts True))
                  (tail (bannerLines "desktop" opts True))
      assertBool "a missing renderer is reported"
                 ("(missing — /headlines only)" `isInfixOf`
                    unlines (bannerLines "serve" opts False))

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
      -- Both are named here, so the walk's own exclusion is not what is under test.
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
                                ["actions", "columns", "rows", "sort", "title", "views"]
                                (sort (map Key.toText (KM.keys o)))
        _        -> assertFailure ("expected an object, got " <> show v)
  ]

-- | The @ETag@ is the tree's fingerprint, the store's generation and the day the
-- request was answered on, and every query variant shares it.
cacheSpec :: TestTree
cacheSpec = testGroup "GET /headlines cache validation"
  [ testCase "carries a tree tag, a generation and a day, and says to revalidate" $ do
      r <- get assetsDir "/headlines"
      etagOf r >>= assertTreeTag "the fixture store" 0
      assertEqual "Cache-Control" (Just "no-cache") (header "Cache-Control" r)

    -- THE DAY RIDES IN THE TAG WHATEVER THE QUERY SPELLS: `*today*' resolves
    -- per request, so a store nothing touched across midnight must revalidate
    -- rather than answer 304 with yesterday's rows.  The days are INJECTED, so
    -- the law is a unit's and not the wall clock's.
  , testCase "the same store on two days is two tags" $ do
      st <- loadStore viewDir
      let tagOn y m d = Routes.etagOf (fromGregorian y m d) st
      assertBool "midnight is a fresh tag"
                 (tagOn 2026 8 21 /= tagOn 2026 8 22)
      assertEqual "and one day is one tag" (tagOn 2026 8 22) (tagOn 2026 8 22)
      assertTreeTag "the injected day" 0 (tagOn 2026 8 22)

  , testCase "the tag it just gave out is a 304 with no body" $ do
      a <- app assetsDir
      first' <- getFrom a "/headlines"
      let tag = fromMaybe "" (header "ETag" first')
      again <- getWith a "/headlines" [("If-None-Match", tag)]
      assertEqual "status" 304 (status again)
      assertEqual "body" "" (simpleBody again)
      assertEqual "the tag comes back" (Just tag) (header "ETag" again)
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
      -- Across a restart the generation is back at zero, so the fingerprint is the whole of what refuses the 304.
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
      -- The watch's own step, taken here without a watcher: re-load the file and publish it.
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
      assertBool "one URL's answer served for another"
                 (simpleBody full /= simpleBody paged)
  ]

-- | Compression: off for a body too small to gain by it, and always with the @Vary@ that keeps the encodings apart.
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
      -- This route's subject is transport; TestFilter is the grammar's home and states every rule.
      assertEqual "a predicate" (Just "1") =<< total "/headlines?q=state:DONE"
      assertEqual "a negation drops what it matches" (Just "5")
        =<< total "/headlines?q=-state:DONE"
      assertEqual "a tag string stays text" (Just "2") =<< total "/headlines?q=:web:"
      -- A bare plus in a query string is a space, so the sign travels as %2B.
      -- Read as free text the token would answer with none of the six rows.
      assertEqual "an added token widens its own axis" (Just "2")
        =<< total "/headlines?q=state%3ATODO%20%2Bstate%3ADONE"
      -- A COMPARISON travels as value text: `<' is %3C and `=' is %3D, and the
      -- fixture dates one row before that deadline and one after it.  The
      -- undated rows ride out under the sign and not under the operator, which
      -- is the empty-cell law arriving over the wire.
      assertEqual "a comparison narrows to the compared rows" (Just "1")
        =<< total "/headlines?q=deadline%3A%3C2026-08-10"
      assertEqual "and its negation keeps the rows that have no date" (Just "5")
        =<< total "/headlines?q=-deadline%3A%3C2026-08-10"
      assertEqual "the range spells the same closed interval" (Just "1")
        =<< total "/headlines?q=deadline%3A2026-08-04..2026-08-06"
      -- A SHIFT TRAVELS AS VALUE TEXT, and the value's own plus rides %2B like
      -- the token's sign: `deadline:2026-08-01+4d' resolves to the fifth, which
      -- one fixture row carries, and both ends of a range take one.
      assertEqual "a shifted literal resolves over the wire" (Just "1")
        =<< total "/headlines?q=deadline%3A2026-08-01%2B4d"
      assertEqual "and a shifted range end names the same interval" (Just "1")
        =<< total "/headlines?q=deadline%3A2026-08-01%2B3d..2026-08-01%2B5d"
      -- The quoted spaced spelling of the same value, quotes and spaces encoded.
      assertEqual "the quoted spaced spelling is that one query" (Just "1")
        =<< total "/headlines?q=deadline%3A%222026-08-01%20%2B%204%20days%22"

    -- THE REFERENCE AXES OVER THE WIRE, one drive each direction and one kind
    -- test.  This route's subject is transport; TestFilter draws the graph and
    -- states every rule.  `?' travels as %3F and `*' as itself.
  , testCase "both reference directions reach the filter, kind and all" $
      withTempDir $ \dir -> do
        _ <- orgFile dir "edges.org" (T.unlines
               [ "* Anchor"
               , ":PROPERTIES:"
               , ":ORG_GLANCE_ID: anchor"
               , ":END:"
               , "cites [[glance:target?kind=cites][T]]"
               , "* Target"
               , ":PROPERTIES:"
               , ":ORG_GLANCE_ID: target"
               , ":END:"
               , "* Bystander" ])
        (a, _hub) <- serverOver dir
        let titles path = fmap sort . mapM (textAt "title" <=< field "cells")
                            =<< rowsOf =<< getFrom a path
        assertEqual "ref: serves the row pointing at the target" ["Anchor"]
          =<< titles "/headlines?q=ref%3Atarget"
        assertEqual "from: serves the row the anchor points at" ["Target"]
          =<< titles "/headlines?q=from%3Aanchor"
        assertEqual "the kind test narrows to the edge's own kind" ["Anchor"]
          =<< titles "/headlines?q=ref%3Atarget%3Fkind%3Dcites"
        assertEqual "and a kind no edge carries serves nothing" []
          =<< titles "/headlines?q=from%3Aanchor%3Fkind%3Dblocked-by"
        assertEqual "the starred anchor is the union over the slot" ["Anchor"]
          =<< titles "/headlines?q=ref%3A*any*"
        assertEqual "read from the other end" ["Target"]
          =<< titles "/headlines?q=from%3A*any*"

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
        assertEqual "where the word it replaced is a value" []
          =<< titles "/headlines?q=state%3Anone"
        assertEqual "so negating the default view drops it too" ["Shipped"]
          =<< titles "/headlines?q=-state%3A*active*"

  , testCase "a filtered OR query pages out of the view's own sort" $ do
      a <- app assetsDir
      whole <- rowsOf =<< getFrom a "/headlines?q=state:*active*"
      one <- getFrom a "/headlines?q=state:*active*&limit=2&offset=0"
      two <- getFrom a "/headlines?q=state:*active*&limit=2&offset=2"
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
      refuses400 a "names the parameter"
        [ ("/headlines?limit=lots", "limit"), ("/headlines?limit=-1", "limit")
        , ("/headlines?offset=x", "offset"), ("/headlines?offset=-3", "offset") ]

  , testCase "a bare parameter reads as an absent one" $ do
      a <- app assetsDir
      r <- ok =<< getFrom a "/headlines?limit&q"
      assertEqual "rows" 6 . length =<< rowsOf r
  ]

-- | Document order is @?q=sort:*none*@, a QUERY TOKEN and a starred meta.  @?order=@ is GONE and refused rather than ignored.
orderSpec :: TestTree
orderSpec = testGroup "GET /headlines?q=sort:*none*"
  [ testCase "the default still declares the view's sort" $ do
      v <- get assetsDir "/headlines" >>= decoded
      fieldsOf v >>= assertBool "no sort field" . elem "sort"

  , testCase "document order declares none at all" $ do
      v <- get assetsDir "/headlines?q=sort:*none*" >>= decoded
      assertEqual "top-level keys" ["actions", "columns", "rows", "title", "views"]
        . sort =<< fieldsOf v

  , testCase "and the page it cuts is walk order, where the default's is sorted" $ do
      a <- app assetsDir
      walk <- map rowId <$> (rowsOf =<< getFrom a "/headlines")
      byState <- map rowId <$> (rowsOf =<< getFrom a "/headlines?limit=6")
      doc <- map rowId <$> (rowsOf =<< getFrom a "/headlines?q=sort:*none*&limit=6")
      assertEqual "the walk itself" walk doc
      -- Without this the case would pass over a fixture whose two orders agree.
      assertBool ("the fixture cannot tell them apart: " <> show byState)
                 (byState /= doc)

    -- The empty chain admits no companions: a reader who wrote both meant one of them.
  , testCase "a sort key beside it is a 400 naming the meta" $ do
      a <- app assetsDir
      refuses400 a "names the meta"
        [ ("/headlines?q=sort:*none*%20sort:title", "*none*")
        , ("/headlines?q=sort:title%20sort:*none*", "*none*")
        , ("/headlines?q=sort:*none*:desc", "*none*") ]

  , testCase "order= is gone, and the refusal names its replacement" $ do
      a <- app assetsDir
      mapM_ (\path -> do
               r <- getFrom a path
               assertEqual (show path <> " status") 400 (status r)
               assertContains "names the parameter" "order=" (body r)
               assertContains "and its replacement" "sort:*none*" (body r))
            [ "/headlines?order=document", "/headlines?order=scheduled"
            , "/headlines?order=walk", "/headlines?order=" ]
      -- A parameter with no value reads as absent, here as everywhere.
      bare <- getFrom a "/headlines?order"
      assertEqual "a bare parameter is an absent one" 200 (status bare)
  ]

-- | The ORDER a query states, served AND declared: what the view declares is the EFFECTIVE chain.
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

    -- The arrow form is SUGAR, so the answer is the answer to the spelling it is sugar for.
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
      -- The empty cells settle behind, outside the direction, keeping walk order among themselves.
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

    -- A page-sized first answer has to be the first page of the order asked for.
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

    -- ONE COLUMN, ONE DIRECTION: a token that is no chain key is the whole request's 400, where a renderer drops the key.
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
          -- A SEGMENT is refused the way its token would be, and the whole token as written comes back.
        , ("sort:title-%3Enosuchcolumn", "nosuchcolumn") ]

  , testCase "and a half-typed one is no refusal at all" $ do
      r <- ok =<< get assetsDir "/headlines?q=sort:"
      assertEqual "rows" 6 . length =<< rowsOf r
      half <- ok =<< get assetsDir "/headlines?q=sort:title-%3E"
      assertEqual "a half-typed segment either" 6 . length =<< rowsOf half

  , testCase "and it cannot state two orders at once" $ do
      r <- get assetsDir "/headlines?q=sort:title%20sort:*none*"
      assertEqual "status" 400 (status r)
      assertContains "names the meta" "*none*" (body r)
      mid <- get assetsDir "/headlines?q=sort:title-%3E*none*"
      assertEqual "mid-chain is the same refusal" 400 (status mid)
      assertContains "and names the meta" "*none*" (body mid)
  ]

chainDeclaredBy :: Value -> IO [(T.Text, Bool)]
chainDeclaredBy view = do
  fields <- fieldsOf view
  if "sort" `notElem` fields then pure []
    else traverse orderKeyOf =<< listAt "sort" view

-- | @\/ws?bootstrap=off@, checked on the parser: the suite binds no socket, and the decision is the whole of what the query controls.
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
      -- The extent starts at the stars, so the file's own header belongs to no subtree.
      assertEqual "the preamble sits ahead of the first subtree"
                  "#+CATEGORY: sample\n#+TODO: NEXT WAITING | CANCELLED\n\n" (T.take start doc)

    -- Rows are top entries, so materialize is the whole of how a client reaches a child.
  , testCase "a top entry materializes with its children in it" $ withTempDir $ \dir -> do
      let doc = T.unlines [ "* TODO parent", ":PROPERTIES:", ":ORG_GLANCE_ID: top"
                          , ":END:", "** child", "child body", "*** grandchild" ]
      _ <- orgFile dir "tree.org" doc
      (a, _hub) <- serverOver dir
      assertEqual "one row for the file" 1 . length =<< rowsOf =<< getFrom a "/headlines"
      v <- getFrom a (headlinePath "top") >>= decoded
      assertEqual "the whole outline" doc =<< textAt "org" v
      -- A child's drawer is body text here, so the split leaves the descendants in the pane a client edits.
      assertEqual "and the body keeps them"
                  (T.unlines ["* TODO parent", "** child", "child body", "*** grandchild"])
                  =<< textAt "body" v

    -- SUB-ADDRESSING: the ROW's id plus an INDEX in document order over the subtree.
  , testCase "the row names the entries hanging under it, and how to reach them"
      $ withNested $ \a _path -> do
      v <- getFrom a (headlinePath "top") >>= decoded
      assertEqual "standing on the row itself" Null =<< field "child" v
      assertEqual "with nothing above it" Null =<< field "parent" v
      assertEqual "the trail is the row alone" ["parent"] =<< textsAt "path" v
      -- EVERY descendant, document order: the pane draws whole subtrees.
      assertEqual "every entry under it, by index" [0, 1, 2]
        =<< traverse (intAt "index") =<< listAt "children" v
      assertEqual "and their cells" ["child one", "grandchild", "child two"]
        =<< traverse (textAt "title") =<< listAt "children" v
      assertEqual "the levels org spells" [2, 3, 2]
        =<< traverse (intAt "level") =<< listAt "children" v
      -- WHERE EACH STANDS in the lifted body: its headline's line, so the pane
      -- can hang the entry's blocks under it without a parser of its own.
      assertEqual "each headline's line, in body coordinates" [2, 5, 6]
        =<< traverse (intAt "line") =<< listAt "children" v

  , testCase "a child materializes as its own subtree, under the file's digest"
      $ withNested $ \a _path -> do
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
      -- The id and the digest are the ROW's: one file, one lock.
      assertEqual "the row's id" "top" =<< textAt "id" v
      rowDigest <- textAt "digest" row
      assertEqual "and the file's digest" rowDigest =<< textAt "digest" v
      assertEqual "the trail says where it is" ["parent", "child one"]
        =<< textsAt "path" v
      assertEqual "and the way back out is the row" Null =<< field "parent" v
      assertEqual "with its own child under it, by index" [1]
        =<< traverse (intAt "index") =<< listAt "children" v

  , testCase "and the grandchild climbs back to the child, not to the row"
      $ withNested $ \a _path -> do
      v <- getFrom a (childPath "top" 1) >>= decoded
      assertEqual "the entry" "*** grandchild\n" =<< textAt "org" v
      assertEqual "which child it hangs under" (Number 0) =<< field "parent" v
      assertEqual "the whole trail" ["parent", "child one", "grandchild"]
        =<< textsAt "path" v

    -- The body stops where the outline under it begins, or the same bytes would be drawn twice.
  , testCase "ownLines is where the entry's own body stops" $ withNested $ \a _path -> do
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
      $ withNested $ \a _path -> do
      r <- getFrom a (childPath "top" 9)
      assertEqual "status" 404 (status r)
      assertContains "names what it holds" "holds 3" =<< textAt "error" =<< decoded r

    -- A mistyped index that served the parent would look exactly like a working request.
  , testCase "and a child that is not a number is a 400" $ withNested $ \a _path -> do
      r <- getFrom a ("/headline" <> renderQuery True
                        [("id", Just "top"), ("child", Just "x")])
      assertEqual "status" 400 (status r)
      assertContains "says what one is" "whole number" =<< textAt "error" =<< decoded r

    -- A row id is FILE#K, so it carries slashes and a HASH; the query string plus percent-encoding is what makes it a non-issue.
  , testCase "an id carrying a hash and slashes round-trips" $ do
      (a, _hub) <- serverOver viewDir
      let rid = T.pack sampleFile <> "#1"
      r <- ok =<< getFrom a (headlinePath rid)
      v <- decoded r
      back <- textAt "id" v
      org <- textAt "org" v
      assertEqual "id" rid back
      assertContains "subtree" "Привет мир" org

    -- The whole `org' rides along untouched — the split is an addition.
  , testCase "the drawer arrives beside the body, lifted out of it" $ do
      (a, _hub) <- serverOver viewDir
      v <- getFrom a (headlinePath "ship-table-view") >>= decoded
      assertEqual "the body is the subtree with every region's lines gone"
                  (T.unlines ["* NEXT [#A] Ship the table view :web:glance:"])
                  =<< textAt "body" v
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
      assertContains "hint" "GET /headline?id=<row id>" (body r)
  ]

commitSpec :: TestTree
commitSpec = testGroup "POST /headline"
  [ testCase "writes the edited subtree and leaves the rest of the file alone" $
      withCommitted $ \a path v digest _body _props -> do
        org <- textAt "org" v
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

    -- A WRITE SPELLS NO TRAILING SPACE, asserted where the bytes land rather than on the text a route composes.
  , testCase "a committed subtree lands with its line ends trimmed" $
      withCommitted $ \a path _v digest body' props -> do
        before <- document path
        let edited = T.replace "body of first" "body of first  \ntyped over  \t" body'
        assertOk =<< postTo a (headlinePath "first") (splitBody edited props digest)
        after <- document path
        assertContains "the line the reader typed is trimmed" "\nbody of first\n" after
        assertContains "and so is the one behind it" "\ntyped over\n" after
        assertContains "the hidden property is where it was"
                       ":PROPERTIES:\n:ORG_GLANCE_ID: first\n:END:\n" after
        assertBool "no line of the file trails"
                   (not (any (\l -> l /= T.stripEnd l) (T.lines after)))
        assertEqual "the file is otherwise the one it was"
                    before (T.replace "typed over\n" "" after)

    -- A CHILD IS WRITTEN THE WAY THE ROW IS: the same route under a `child=', splicing that entry's OWN extent.
  , testCase "a child commit splices the child's extent alone" $ withNested $ \a path -> do
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

  , testCase "and its parts recompose into the same extent" $ withNested $ \a path -> do
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
      withNested $ \a path -> do
        v <- getFrom a (headlinePath "top") >>= decoded
        digest <- textAt "digest" v
        before <- document path
        r <- postTo a (childPath "top" 9) (commitBody "** nope\n" digest)
        assertEqual "status" 404 (status r)
        -- The NAME promises the commit did not land, and every sibling refusal here asserts the file too.
        assertEqual "and nothing was written" before =<< document path

  , testCase "leaves the store alone — the watch is what updates rows" $
      withCommitted $ \a path before digest _body _props -> do
        org <- textAt "org" before
        assertOk =<< postTo a (headlinePath "first") (commitBody (org <> "a line\n") digest)
        after <- decoded =<< getFrom a (headlinePath "first")
        assertEqual "the store's subtree" (Just org) . Just =<< textAt "org" after
        assertEqual "the store's digest" (Just digest) . Just =<< textAt "digest" after
        onDisk <- digestOnDisk path
        assertBool "but the file was written" (onDisk /= digest)

  , testCase "a file rewritten behind the client is a conflict, and stays as it is" $
      withCommitted $ \a path v digest _body _props -> do
        org <- textAt "org" v
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
      withCommitted $ \a path v _digest _body _props -> do
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

    -- The split shape buys exactly the byte rule: a property nobody touched goes back as the line it came in on.
  , testCase "the split shape writes the same subtree, verbatim where nothing moved" $
      withCommitted $ \a path _v digest body' props -> do
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
      withCommitted $ \a path _v digest body' _props -> do
        assertOk =<< postTo a (headlinePath "first") (splitBody body' [] digest)
        after <- document path
        -- The identity property is the SERVER's, so emptying the list leaves that one line standing.
        assertEqual "the subtree is its body and the server's own line"
                    (T.unlines [ "#+CATEGORY: notes", "* TODO First :one:", ":PROPERTIES:"
                               , ":ORG_GLANCE_ID: first", ":END:", "body of first"
                               , "* TODO Second", "tail" ])
                    after

  , testCase "the split shape is drift-locked like the whole one" $
      withCommitted $ \a path _v _digest body' props -> do
        let stale = T.replicate 64 "0"
        r <- postTo a (headlinePath "first") (splitBody body' props stale)
        assertEqual "status" 409 (status r)
        assertEqual "reason" "stale" =<< textAt "reason" =<< decoded r
        assertEqual "untouched" committable =<< document path

  , testCase "and by the file on disk as well as by the store" $
      withCommitted $ \a path _v digest body' props -> do
        let meddled = committable <> "* TODO Someone else\n"
        TIO.writeFile path meddled
        r <- postTo a (headlinePath "first") (splitBody body' props digest)
        assertEqual "status" 409 (status r)
        assertEqual "reason" "drift" =<< textAt "reason" =<< decoded r
        assertEqual "the file is the meddler's" meddled =<< document path

    -- A planning value NO reading takes is refused BEFORE the write, naming the
    -- field.  The wall's own grammar is 'planningTimestamp' (TestQuery holds the
    -- corpus); what this pins is that a phrase outside it never lands.
  , testCase "a planning entry that no date reading takes is a 409 naming the field" $
      withCommitted $ \a path _v digest body' _props -> do
        r <- postTo a (headlinePath "first")
               (planningBody body' [] [["SCHEDULED", "soon"]] digest)
        assertEqual "status" 409 (status r)
        b <- decoded r
        assertEqual "reason" "planning" =<< textAt "reason" b
        assertEqual "which field" "SCHEDULED" =<< textAt "field" b
        -- THE WALL'S OWN SENTENCE, carried through: the 409 says what the date
        -- grammar says rather than a second spelling of it.
        assertContains "and what it wanted, in the reader's own words"
                       "soon is not a date: spell it" =<< textAt "error" b
        assertContains "naming the forms it would have taken" "18 aug"
          =<< textAt "error" b
        -- No digest on this one: nothing about it is a lock.
        assertEqual "the fields it carries" ["error", "field", "reason"] =<< fieldsOf b
        assertEqual "untouched" committable =<< document path
        -- AN UNKNOWN KEY IS A KEY REFUSAL: 'unplanned''s own sentence, so the two
        -- write doors refuse it alike.
        bad <- postTo a (headlinePath "first")
                 (planningBody body' [] [["WHENEVER", "<2026-08-01 Sat>"]] digest)
        assertEqual "status" 409 (status bad)
        badly <- decoded bad
        assertEqual "named" "WHENEVER" =<< textAt "field" badly
        assertContains "the keyword wall's own words, not the value wall's"
          "WHENEVER is not a planning keyword; this server writes SCHEDULED and DEADLINE and CLOSED"
          =<< textAt "error" badly

    -- THE WALL IS ALSO THE TRANSFORM.  The pane spells no org: it sends the raw
    -- typed text and the server writes the bytes org itself would.  The year is
    -- SPELLED here so the assertion does not move with the calendar.
  , testCase "an English date on the planning line is REWRITTEN to org's own spelling" $
      withCommitted $ \a path _v digest body' _props -> do
        assertOk =<< postTo a (headlinePath "first")
               (planningBody body' [] [["SCHEDULED", "18 aug 2027"]] digest)
        assertContains "the stamp the server computed, weekday and all"
                       "SCHEDULED: <2027-08-18 Wed>" =<< document path

  , testCase "and an English interval as org's own -- pair" $
      withCommitted $ \a path _v digest body' _props -> do
        assertOk =<< postTo a (headlinePath "first")
               (planningBody body' [] [["DEADLINE", "from 18 to 19 august 2027"]] digest)
        assertContains "both ends, each weekday computed"
                       "DEADLINE: <2027-08-18 Wed>--<2027-08-19 Thu>" =<< document path

    -- The year defaults to the SERVER'S OWN CLOCK, read once per request, so the
    -- expectation is computed off that same clock rather than written down.
  , testCase "a year-less phrase takes the server's own year, flat" $
      withCommitted $ \a path _v digest body' _props -> do
        (year, _month, _day) <- toGregorian <$> today
        assertOk =<< postTo a (headlinePath "first")
               (planningBody body' [] [["SCHEDULED", "18 aug"]] digest)
        assertContains "that August, in the clock's year"
                       ("SCHEDULED: <" <> T.pack (show year) <> "-08-18 ")
          =<< document path

    -- An interval naming two good days in the wrong order takes the SAME column
    -- as a phrase that never parsed: no new refusal machinery on this wall.
  , testCase "an inverted interval is the same 409 as any other refusal" $
      withCommitted $ \a path _v digest body' _props -> do
        r <- postTo a (headlinePath "first")
               (planningBody body' [] [["SCHEDULED", "from 30 dec to 2 jan"]] digest)
        assertEqual "status" 409 (status r)
        assertEqual "which field" "SCHEDULED" =<< textAt "field" =<< decoded r
        -- ITS OWN WORDS: the 409 carries the inversion rather than flattening it.
        assertContains "the inversion's own sentence" "ends before it starts"
          =<< textAt "error" =<< decoded r
        assertEqual "untouched" committable =<< document path

    -- CLOSED IS ORG'S OWN BOOKKEEPING and takes REPARSE alone: the widget's
    -- grammar is SCHEDULED's and DEADLINE's.
  , testCase "CLOSED takes org's own spelling and no phrase beside it" $
      withCommitted $ \a path _v digest body' _props -> do
        r <- postTo a (headlinePath "first")
               (planningBody body' [] [["CLOSED", "18 aug 2027"]] digest)
        assertEqual "status" 409 (status r)
        assertEqual "which field" "CLOSED" =<< textAt "field" =<< decoded r
        -- AND IN REPARSE'S OWN WORDS, where the settable keys answer in the date
        -- grammar's: `timestamp' is what this reading actually wants.
        assertContains "the reparse wall's sentence" "is not a timestamp org would read back"
          =<< textAt "error" =<< decoded r
        assertEqual "untouched" committable =<< document path
        assertOk =<< postTo a (headlinePath "first")
               (planningBody body' [] [["CLOSED", "[2026-08-01 Sat]"]] digest)
        assertContains "org's own, verbatim" "CLOSED: [2026-08-01 Sat]" =<< document path

    -- THE RAW HALF TRANSFORMS NOTHING: `org' is a whole document the client
    -- typed, and rewriting bytes inside it would be the server editing a buffer.
  , testCase "the raw half is left exactly as it was typed" $
      withCommitted $ \a path v digest _body _props -> do
        org <- textAt "org" v
        assertOk =<< postTo a (headlinePath "first")
               (commitBody (T.replace "* TODO First :one:\n"
                                      "* TODO First :one:\nSCHEDULED: 18 aug 2027\n" org)
                           digest)
        assertContains "the phrase stands where the client put it"
                       "SCHEDULED: 18 aug 2027" =<< document path

  , testCase "a body that is not the two fields is a 400" $
      withCommitted $ \a _path _v _digest _body _props -> do
        broken <- postTo a (headlinePath "first") "{not json"
        missing <- postTo a (headlinePath "first") (encode (object ["org" .= ("x" :: T.Text)]))
        assertEqual "malformed" 400 (status broken)
        assertEqual "incomplete" 400 (status missing)
        assertContains "says which" "key \\\"digest\\\" not found" (body missing)

    -- A `body' with no `properties' beside it would read as "drop the drawer" — too much to infer.
  , testCase "the two shapes are told apart, and neither is half-given" $
      withCommitted $ \a path _v digest _body _props -> do
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
      withCommitted $ \a path _v _digest _body _props -> do
        let huge = BL.fromStrict (BS.replicate (1024 * 1024 + 1) 0x78)
        r <- postTo a (headlinePath "first") huge
        assertEqual "status" 413 (status r)
        assertContains "the cap" "body over" (body r)
        -- BEFORE IT IS READ is the claim, and a status cannot carry it: the untouched file is what says so.
        assertEqual "and nothing was written" committable =<< document path

  , testCase "an id no row carries is a 404, and no id a 400" $
      withCommitted $ \a _path _v _digest _body _props -> do
        unknown <- postTo a (headlinePath "no-such-headline") (commitBody "* x\n" "d")
        anonymous <- postTo a "/headline" (commitBody "* x\n" "d")
        assertEqual "unknown id" 404 (status unknown)
        assertEqual "no id" 400 (status anonymous)
        assertContains "the hint" "POST /headline?id=<row id>" (body anonymous)
  ]

-- | The rows a structured command names.  Ids are in drawers, so they survive the temp directory's name and every edit.
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

-- | A second file declaring no keywords of its own, which is what makes legality per file observable.
elsewhereOrg :: T.Text
elsewhereOrg = T.unlines
  [ "* TODO Third"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: third"
  , ":END:"
  ]

-- | @add-tag@ and @remove-tag@'s argument.  Flat rather than nullable: a tag comes off through the other command.
tagArg :: T.Text -> Value
tagArg tag = object ["tag" .= tag]

-- | @set-title@'s argument.  Flat for @tagArg@'s reason: a headline with no title is a blank entry.
titleArg :: T.Text -> Value
titleArg title = object ["title" .= title]

renameArg :: T.Text -> T.Text -> Value
renameArg from to = object ["from" .= from, "to" .= to]

withCommandable :: (Application -> Hub -> FilePath -> FilePath -> Assertion) -> Assertion
withCommandable k = withTempDir $ \dir -> do
  here <- orgFile dir "notes.org" commandable
  there <- orgFile dir "other.org" elsewhereOrg
  (a, hub) <- serverOver dir
  k a hub here there

-- | The watch's own step, taken here without a watcher: a command computes its spans and digest from the store.
watchStep :: Hub -> FilePath -> Assertion
watchStep hub path = do
  outcome <- loadFile path
  _ <- publish hub (applyFile path outcome)
  pure ()

outcomesOf :: SResponse -> IO [(T.Text, Bool)]
outcomesOf r = do
  results <- listAt "results" =<< decoded r
  traverse (\v -> (,) <$> textAt "id" v <*> boolAt "ok" v) results

errorOf :: SResponse -> IO T.Text
errorOf r = do
  results <- listAt "results" =<< decoded r
  bad <- filterM (fmap not . boolAt "ok") results
  T.unwords <$> traverse (textAt "error") bad

digestsOf :: SResponse -> IO [T.Text]
digestsOf r = do
  results <- listAt "results" =<< decoded r
  ok <- filterM (boolAt "ok") results
  traverse (textAt "digest") ok

-- | A tree the door under test needs nothing written to first.
asIs :: Application -> Hub -> FilePath -> Assertion
asIs _a _hub _path = pure ()

asIsIn :: Application -> Hub -> FilePath -> FilePath -> Assertion
asIsIn _a _hub _path _other = pure ()

-- | ROWS OF ONE FILE RIDE ONE WRITE, at whichever door: ARRANGE readies the tree, CMD names two rows of the same file, and SAYS reads the file before and after.
oneFileOneWrite :: (Application -> Hub -> FilePath -> Assertion) -> BL.ByteString
                -> (T.Text -> T.Text -> Assertion) -> TestTree
oneFileOneWrite arrange cmd says =
  testCase "two rows of one file are one write, and both land" $
    withCommandable $ \a hub path _other -> do
      arrange a hub path
      before <- document path
      r <- ok =<< postTo a "/command" cmd
      assertEqual "both rows" [("first", True), ("second", True)] =<< outcomesOf r
      digests <- digestsOf r
      says before =<< document path
      onDisk <- digestOnDisk path
      assertEqual "a digest per row" 2 (length digests)
      assertEqual "one write, so one digest, and it is the file's" [onDisk] (nub digests)

-- | AND ROWS IN TWO FILES RIDE A WRITE EACH: ARRANGE readies both, CMD names one row of each, and each file carries its own needle after.
twoFilesTwoWrites :: (Application -> Hub -> FilePath -> FilePath -> Assertion) -> BL.ByteString
                  -> (String, T.Text) -> (String, T.Text) -> TestTree
twoFilesTwoWrites arrange cmd (hereSays, here) (thereSays, there) =
  testCase "rows in two files are two writes, and each is its own" $
    withCommandable $ \a hub path other -> do
      arrange a hub path other
      r <- ok =<< postTo a "/command" cmd
      assertEqual "both rows" [("first", True), ("third", True)] =<< outcomesOf r
      assertEqual "two files, two digests" 2 . length . nub =<< digestsOf r
      assertContains hereSays here =<< document path
      assertContains thereSays there =<< document other

-- | ONE ID NOTHING CARRIES, beside one that lands: the same contract at three doors.
loneMissingId :: BL.ByteString -> (String, [(T.Text, Bool)]) -> (String, T.Text) -> TestTree
loneMissingId cmd (order, outcomes) (moved, named) =
  testCase "an id no row carries is refused on its own" $
    withCommandable $ \a _hub path _other -> do
      r <- ok =<< postTo a "/command" cmd
      assertEqual order outcomes =<< outcomesOf r
      assertContains moved named =<< document path

commandSpec :: TestTree
commandSpec = testGroup "POST /command"
  [ testCase "set-state replaces the keyword and moves no other byte" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- ok =<< postTo a "/command" (command "set-state" ["first"] (keywordArg (Just "WAITING")))
        assertEqual "the row landed" [("first", True)] =<< outcomesOf r
        after <- document path
        assertEqual "the file is the old one with one word replaced"
                    (T.replace "* NEXT First" "* WAITING First" before) after
        onDisk <- digestOnDisk path
        assertEqual "the digest it reports is the file's" [onDisk] =<< digestsOf r

    -- The span is the title's own, so the assertion is about what it did NOT touch.
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

    -- Two rows of one file are ONE editFile: a write per row would pin the second to the digest the first invalidated.
  , oneFileOneWrite asIs
      (command "set-state" ["first", "second"] (keywordArg (Just "CANCELLED")))
      (\before after -> assertEqual "both edits, in one file"
         (T.replace "* Second" "* CANCELLED Second"
            (T.replace "* NEXT First" "* CANCELLED First" before)) after)

  , twoFilesTwoWrites asIsIn (command "archive" ["first", "third"] (object []))
      ("the tag joined the list", "* NEXT First :one:ARCHIVE:")
      ("and started one", "* TODO Third :ARCHIVE:")

    -- No cross-file rollback, and none is possible: the answer says which rows landed instead.
  , testCase "a file that moved refuses its rows while the others land" $
      withCommandable $ \a _hub path other -> do
        meddled <- (<> "* TODO Someone else\n") <$> document other
        TIO.writeFile other meddled
        r <- ok =<< postTo a "/command" (command "archive" ["first", "third"] (object []))
        assertEqual "one landed, one did not"
                    [("first", True), ("third", False)] =<< outcomesOf r
        assertContains "the untouched file took its edit" ":one:ARCHIVE:" =<< document path
        assertEqual "and the moved one is the meddler's" meddled =<< document other

  , loneMissingId (command "archive" ["nowhere", "first"] (object []))
      ("in the order asked", [("nowhere", False), ("first", True)])
      ("the real row still landed", ":one:ARCHIVE:")

    -- Legality is per ROW's chain, and half a state change over a marked set is worse than none.
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

    -- `film''s cycle is RECOGNIZED under this root, and no scope an untagged row reaches declares it.
  , testCase "another tag's keyword is refused on a row that does not reach it" $
      withLayeredTree $ \a -> do
        r <- postTo a "/command" (command "set-state" ["bare"] (keywordArg (Just "WATCHING")))
        assertEqual "status" 400 (status r)
        assertContains "names the keyword" "WATCHING" (body r)
        assertContains "and the row" "bare" (body r)

    -- Each row against ITS OWN chain, which is the cost of the palette merging several rows into one table.
  , testCase "a marked set spanning tags is refused for the row that cannot take it" $
      withLayeredTree $ \a -> do
        r <- postTo a "/command"
               (command "set-state" ["tagged", "filmed"] (keywordArg (Just "READING")))
        assertEqual "status" 400 (status r)
        assertContains "names the row it does not fit" "filmed" (body r)
        ok <- postTo a "/command" (command "set-state" ["tagged"] (keywordArg (Just "READING")))
        assertEqual "and the one it fits, alone" 200 (status ok)
        assertEqual "landed" [("tagged", True)] =<< outcomesOf ok

    -- A tree apiece, since two writes to one file drift in a suite that runs no watch.
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
        -- The cap outranks every other refusal, so nothing downstream of it ran.
        assertEqual "and no row moved" before =<< document path

  , testCase "the route takes POST and nothing else" $
      withCommandable $ \a _hub _path _other -> do
        r <- getFrom a "/command"
        assertEqual "status" 405 (status r)
        assertContains "hint" "/command takes POST" (body r)

    -- THE WRITE HINT NAMES EVERY WRITE ROUTE, being derived from the route
    -- table: a hand-written sentence had missed /config, which takes POST too.
    -- Asked of a path that takes no POST at all, so the hint is what answers.
  , testCase "a refused method names every route that does write" $
      withCommandable $ \a _hub _path _other -> do
        r <- postTo a "/headlines" (encode (object []))
        assertEqual "status" 405 (status r)
        forM_ ["/headline", "/command", "/config"] $ \route ->
          assertContains ("the hint omits " <> T.unpack route)
                         ("POST " <> route) (body r)
  ]

-- | @set-planning@: the request's shape and the whole-request refusal; the span math is @TestQuery@'s.
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

    -- The clock is read once for the request, so a marked set cannot land on two days.
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

  , testCase "a date no parser reads refuses the request, naming it" $
      withCommandable $ \a _hub path other -> do
        before <- document path
        r <- postTo a "/command"
               (command "set-planning" ["first", "third"]
                        (planningArg "SCHEDULED" (Just "next tuesday")))
        assertEqual "status" 400 (status r)
        assertContains "names the input" "next tuesday" (body r)
        assertContains "and the English forms it does take" "18 aug" (body r)
        assertEqual "the first file is untouched" before =<< document path
        assertEqual "and so is the second" elsewhereOrg =<< document other

    -- ONE PARSER, BOTH DOORS: `set-planning''s date argument and the planning
    -- line's own wall read the same grammar, so a phrase the pane may type is a
    -- phrase this command takes.
  , testCase "an English date lands here too, weekday computed" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        assertOk =<< postTo a "/command"
               (command "set-planning" ["first"] (planningArg "SCHEDULED" (Just "18 aug 2027")))
        assertEqual "the stamp the server computed"
                    (T.replace "* NEXT First :one:\n"
                               "* NEXT First :one:\nSCHEDULED: <2027-08-18 Wed>\n" before)
          =<< document path

  , testCase "and an English interval as org's own -- pair" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        assertOk =<< postTo a "/command"
               (command "set-planning" ["first"]
                        (planningArg "DEADLINE" (Just "from 18 to 19 august 2027")))
        assertEqual "both ends, each weekday computed"
                    (T.replace "* NEXT First :one:\n"
                               ("* NEXT First :one:\nDEADLINE: <2027-08-18 Wed>"
                                  <> "--<2027-08-19 Thu>\n") before)
          =<< document path

    -- The degenerate pair COLLAPSES, so the two spellings of one day agree.
  , testCase "a same-day interval collapses to the single stamp" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        assertOk =<< postTo a "/command"
               (command "set-planning" ["first"]
                        (planningArg "SCHEDULED" (Just "from 18 to 18 august 2027")))
        assertEqual "one stamp, not two"
                    (T.replace "* NEXT First :one:\n"
                               "* NEXT First :one:\nSCHEDULED: <2027-08-18 Wed>\n" before)
          =<< document path

    -- "Not a date" reads oddly of a phrase naming two perfectly good ones.
  , testCase "an inverted interval is refused in its own words" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command"
               (command "set-planning" ["first"]
                        (planningArg "SCHEDULED" (Just "from 30 dec to 2 jan")))
        assertEqual "status" 400 (status r)
        assertContains "names the input" "from 30 dec to 2 jan" (body r)
        assertContains "says which way it runs" "ends before it starts" (body r)
        assertContains "and names the remedy" "spell a year" (body r)
        assertEqual "nothing written" before =<< document path

    -- 'fromGregorianValid' is the wall, and a day it declines never reaches disk.
  , testCase "a day the calendar has not got is refused, naming it" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command"
               (command "set-planning" ["first"] (planningArg "SCHEDULED" (Just "31 feb")))
        assertEqual "status" 400 (status r)
        assertContains "names the input" "31 feb" (body r)
        assertEqual "nothing written" before =<< document path

    -- AN UNKNOWN KEY OUTRANKS EVERY VALUE: the keyword picks which wall the
    -- date meets, so a word naming no planning entry is refused before the
    -- date is read at all -- a perfectly good date does not rescue it.
  , testCase "and so does a keyword that names no planning entry" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- postTo a "/command"
               (command "set-planning" ["first"] (planningArg "TIMESTAMP" (Just "2026-08-05")))
        assertEqual "status" 400 (status r)
        assertContains "names the keyword" "TIMESTAMP" (body r)
        assertContains "and the three it writes" "SCHEDULED and DEADLINE and CLOSED" (body r)
        assertEqual "nothing written" before =<< document path

    -- ORG'S THIRD WORD IS NOT COMPOSED FOR, IT IS REPARSED.  The widget over
    -- CLOSED commits through this door with the RAW text it was handed, so the
    -- server's wall here and the client's own are one wall by construction:
    -- what reads back lands verbatim, English is refused in reparse's words,
    -- and a null date clears the entry as it does for the other two.
  , testCase "CLOSED lands verbatim, takes no English, and clears" $
      withCommandable $ \a hub path _other -> do
        before <- document path
        assertOk =<< postTo a "/command"
               (command "set-planning" ["first"]
                        (planningArg "CLOSED" (Just "[2026-09-01 Tue]")))
        assertEqual "org's own bracket, byte for byte, on the planning line"
                    (T.replace "* NEXT First :one:\n"
                               "* NEXT First :one:\nCLOSED: [2026-09-01 Tue]\n" before)
          =<< document path
        watchStep hub path
        standing <- document path
        r <- postTo a "/command"
               (command "set-planning" ["first"] (planningArg "CLOSED" (Just "18 aug 2027")))
        assertEqual "a phrase the other two resolve is refused here" 400 (status r)
        assertContains "in the reparse wall's own sentence"
                       "CLOSED is not a timestamp org would read back" (body r)
        assertEqual "and nothing moved" standing =<< document path
        assertOk =<< postTo a "/command"
               (command "set-planning" ["first"] (planningArg "CLOSED" Nothing))
        assertEqual "a null date takes the entry and its line off" before
          =<< document path

    -- Absent is not null: one says nothing about the entry and the other asks for it to come off.
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

-- | @add-tag@ and @remove-tag@: the route — the batching, the per-id answer, and the request-shape refusals.
tagCommandSpec :: TestTree
deleteCommandSpec :: TestTree
deleteCommandSpec = testGroup "POST /command delete"
  [ testCase "an archived blob is gzipped into the trash and leaves the tree" $
      withDeletable $ \a root archived' _live _shared -> do
        r <- ok =<< postTo a "/command" (command "delete" ["gone"] (object []))
        assertEqual "the row landed" [("gone", True)] =<< outcomesOf r
        assertEqual "and the blob is out of the live tree" False
          =<< doesFileExist archived'
        let kept = fromJust (trashPathFor root archived')
        assertEqual "the trash holds it" True =<< doesFileExist kept
        assertEqual "byte for byte" archivedBlob . TE.decodeUtf8 . BL.toStrict
          . GZip.decompress =<< BL.readFile kept

    -- The record org-glance holds points at bytes that are now in the trash, so the line says DROP.
  , testCase "and it leaves one tombstone naming the row" $
      withDeletable $ \a root _archived _live _shared -> do
        assertOk =<< postTo a "/command" (command "delete" ["gone"] (object []))
        noted <- noteLinesIn root
        assertEqual "one line" 1 (length noted)
        assertContains "naming the row" "\"id\":\"gone\"" (head noted)
        assertContains "and saying it is gone" ",\"tombstone\":true}" (head noted)

    -- ARCHIVING IS THE STEP BEFORE THIS ONE, and the wall is the SERVER's as much as the shell's.
  , testCase "a row that is not archived is refused, and stands" $
      withDeletable $ \a root _archived live _shared -> do
        r <- ok =<< postTo a "/command" (command "delete" ["here"] (object []))
        assertEqual "refused" [("here", False)] =<< outcomesOf r
        assertContains "naming the step it owes" "not archived" =<< errorOf r
        assertEqual "and the blob stands" True =<< doesFileExist live
        assertEqual "so nothing is noted" [] =<< noteLinesIn root

    -- A SHARED ORG FILE IS MANY ROWS' DOCUMENT, and moving it would take the others with it.
  , testCase "an archived row in a shared file is refused, and stands" $
      withDeletable $ \a root _archived _live shared -> do
        r <- ok =<< postTo a "/command" (command "delete" ["shared"] (object []))
        assertEqual "refused" [("shared", False)] =<< outcomesOf r
        assertContains "naming what it deletes" "blob" =<< errorOf r
        assertEqual "and the file stands" True =<< doesFileExist shared
        assertEqual "so nothing is noted" [] =<< noteLinesIn root

  , testCase "an id the store does not hold is refused like any other" $
      withDeletable $ \a root _archived _live _shared -> do
        r <- ok =<< postTo a "/command" (command "delete" ["nope"] (object []))
        assertEqual "refused" [("nope", False)] =<< outcomesOf r
        assertEqual "so nothing is noted" [] =<< noteLinesIn root

    -- It NAMES ROWS, and the id wall reads the NAME rather than "has edits".
  , testCase "and it owes ids" $
      withDeletable $ \a _root _archived _live _shared -> do
        r <- postTo a "/command" (encode (object ["name" .= ("delete" :: T.Text)]))
        assertEqual "400" 400 (status r)
        assertContains "asks for them" "names rows" (body r)
  ]

-- | The lines ROOT's store holds in @meta\/EXTERNAL.jsonl@.  THE FORMAT IS @TestExternal@'s SUBJECT.
noteLinesIn :: FilePath -> IO [T.Text]
noteLinesIn root = do
  there <- doesFileExist note
  if there then T.lines <$> document note else pure []
  where note = storeRootIn root </> "meta" </> "EXTERNAL.jsonl"

-- | A tree with the three shapes @delete@ tells apart.  K is handed the ROOT, which is what every trash function takes.
withDeletable :: (Application -> FilePath -> FilePath -> FilePath -> FilePath -> Assertion)
              -> Assertion
withDeletable k = withTempDir $ \dir -> do
  let store = storeRootIn dir
      blob ident text = do
        let path = blobPathIn store ident
        createDirectoryIfMissing True (takeDirectory path)
        TIO.writeFile path text
        pure path
  archived' <- blob "a7deadbeef" archivedBlob
  live <- blob "b8cafebabe" liveBlob
  shared <- orgFile dir "shared.org" sharedOrg
  (a, _hub) <- serverOver dir
  k a dir archived' live shared

archivedBlob, liveBlob, sharedOrg :: T.Text
archivedBlob = "* DONE Gone :archive:\n:PROPERTIES:\n:ORG_GLANCE_ID: gone\n:END:\n"
liveBlob = "* TODO Here\n:PROPERTIES:\n:ORG_GLANCE_ID: here\n:END:\n"
sharedOrg = "* DONE Shared :archive:\n:PROPERTIES:\n:ORG_GLANCE_ID: shared\n:END:\n\
            \* TODO Neighbour\n:PROPERTIES:\n:ORG_GLANCE_ID: neighbour\n:END:\n"

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
        -- The store has to catch up before a second command can measure a span in this file.
        watchStep hub path
        _ <- postTo a "/command" (command "add-tag" ["first"] (tagArg "work"))
        watchStep hub path
        assertOk =<< postTo a "/command" (command "add-tag" ["first"] (tagArg "home"))
        watchStep hub path
        assertContains "two entries now" "* NEXT First :work:home:" =<< document path
        _ <- postTo a "/command" (command "remove-tag" ["first"] (tagArg "work"))
        assertContains "and one after the cut" "* NEXT First :home:" =<< document path

    -- Both directions are idempotent, so a palette may commit the same letter twice.
  , testCase "adding what is there and removing what is not both land, changing nothing" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        added <- postTo a "/command" (command "add-tag" ["first"] (tagArg "one"))
        assertEqual "the row landed" [("first", True)] =<< outcomesOf added
        gone <- postTo a "/command" (command "remove-tag" ["second"] (tagArg "work"))
        assertEqual "and so did the other" [("second", True)] =<< outcomesOf gone
        assertEqual "and the file says what it always said" before =<< document path

  , oneFileOneWrite asIs (command "add-tag" ["first", "second"] (tagArg "work"))
      (\before after -> assertEqual "both edits, in one file"
         (T.replace "* Second" "* Second :work:"
            (T.replace "* NEXT First :one:" "* NEXT First :one:work:" before)) after)

  , twoFilesTwoWrites asIsIn (command "add-tag" ["first", "third"] (tagArg "work"))
      ("the tag joined the run", "* NEXT First :one:work:")
      ("and opened one", "* TODO Third :work:")

    -- The PALETTE normalizes up; sending the whole set is safe, since the row that has it costs no edit.
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

  , loneMissingId (command "add-tag" ["first", "nosuch"] (tagArg "work"))
      ("one landed, one did not", [("first", True), ("nosuch", False)])
      ("and the row that is there moved", "* NEXT First :one:work:")

    -- The route writes the FILE; the watch is what updates rows, and then all three move together.
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

-- | @rename-tag@: the argument shape, the two walls, and one atomic write over several rows of one file.
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

    -- BOTH DIRECTIONS from one edit set, which a remove-then-add composition cannot have.
  , testCase "and renaming it back puts the file where it was" $
      withCommandable $ \a hub path _other -> do
        before <- document path
        _ <- postTo a "/command" (command "rename-tag" ["first"] (renameArg "one" "two"))
        watchStep hub path
        assertOk =<< postTo a "/command" (command "rename-tag" ["first"] (renameArg "two" "one"))
        assertEqual "byte for byte" before =<< document path

    -- A row that does not carry the old name costs no edit.
  , testCase "a row that never carried it lands, changing nothing" $
      withCommandable $ \a _hub path _other -> do
        before <- document path
        r <- ok =<< postTo a "/command" (command "rename-tag" ["second"] (renameArg "one" "two"))
        assertEqual "the row landed" [("second", True)] =<< outcomesOf r
        assertEqual "and the file says what it always said" before =<< document path

  , oneFileOneWrite
      (\a hub path -> do
         _ <- postTo a "/command" (command "add-tag" ["first", "second"] (tagArg "work"))
         watchStep hub path)
      (command "rename-tag" ["first", "second"] (renameArg "work" "projects"))
      (\_before after -> do
         assertContains "the entry moved in place" "* NEXT First :one:projects:" after
         assertContains "and in the row that had only it" "* Second :projects:" after)

  , twoFilesTwoWrites
      (\a hub path other -> do
         _ <- postTo a "/command" (command "add-tag" ["first", "third"] (tagArg "work"))
         watchStep hub path
         watchStep hub other)
      (command "rename-tag" ["first", "third"] (renameArg "work" "projects"))
      ("here", "* NEXT First :one:projects:")
      ("and there", "* TODO Third :projects:")

    -- The charset wall is the request's and stands at BOTH ends.
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

  , loneMissingId (command "rename-tag" ["first", "nosuch"] (renameArg "one" "two"))
      ("one landed, one did not", [("first", True), ("nosuch", False)])
      ("and the row that is there moved", "* NEXT First :two:")

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

cellAt :: T.Text -> Value -> IO T.Text
cellAt key row = do
  cells <- field "cells" row
  fromMaybe "" <$> maybeTextAt key cells

-- | @GET \/tags@: the route — the shape, the order, the vocabulary beside it, and the refusals it shares with @\/keywords@.
tagsSpec :: TestTree
tagsSpec = testGroup "GET /tags" $
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

    -- The whole store's, not the named rows': a completing read has to reach a tag none of the targets carries.
  , testCase "the vocabulary is the tree's, whichever row was asked about" $
      withTaggedTree $ \a ->
        assertEqual "every tag in the store, sorted" ["archive", "shelf", "web", "work"]
          =<< textsAt "vocabulary" =<< decoded =<< getFrom a "/tags?ids=bare"

    -- The COUNTS are ROWS per tag: `stTags' counts FILES, so no arithmetic recovers this.
  , testCase "the counts are the tree's rows per tag, folded" $
      withTaggedTree $ \a -> do
        counts <- field "counts" =<< decoded =<< getFrom a "/tags?ids=bare"
        assertEqual "one entry per tag the store holds"
                    ["archive", "shelf", "web", "work"] =<< countedNames counts
        assertEqual "web is on two rows of one file, however it is spelled" 2
          =<< intAt "web" counts
        assertEqual "work on one of them" 1 =<< intAt "work" counts
        assertEqual "shelf on one row of the other file" 1 =<< intAt "shelf" counts
        assertEqual "and the archive tag counts like any other" 1
          =<< intAt "archive" counts

  ]
  <> idsParamCases withTaggedTree "/tags" tagRowsOf
       ("both", [("both", ["web", "work"])])
       ("bare", "both", [("bare", []), ("both", ["web", "work"])])
  <> [ postIs405 "/tags" ]

tagRowsOf :: SResponse -> IO [(T.Text, [T.Text])]
tagRowsOf = traverse one <=< rowsOf
  where one v = (,) <$> textAt "id" v <*> textsAt "tags" v

-- | The names a counts object spells, sorted: JSON object order is nobody's contract.
countedNames :: Value -> IO [T.Text]
countedNames = fmap sort . fieldsOf

-- | A tree holding one tag no resolved row carries, so the vocabulary being the STORE's is observable.
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

-- | @GET \/properties@: the drawer vocabulary, which names no row, so the whole store answers.
propertiesSpec :: TestTree
propertiesSpec = testGroup "GET /properties"
  [ -- THE HIDDEN KEYS ARE NO VOCABULARY: both sit in every drawer of the
    -- fixture, and completing one would write it.
    testCase "the keys are the tree's own, counted in rows" $
      withDrawerTree $ \a -> do
        keys <- field "keys" =<< decoded =<< ok =<< getFrom a "/properties"
        assertEqual "one entry per key a row spells, the server's own left out"
                    ["Author", "Genre", "Rating"] =<< countedNames keys
        assertEqual "Genre is on two rows of one file and one of the other" 3
          =<< intAt "Genre" keys
        assertEqual "Rating on the one row that spells it" 1 =<< intAt "Rating" keys

  , testCase "and every value under the key it was spelled with" $
      withDrawerTree $ \a -> do
        values <- field "values" =<< decoded =<< ok =<< getFrom a "/properties"
        genre <- field "Genre" values
        assertEqual "both spellings the tree holds" ["heist", "noir"]
          =<< countedNames genre
        assertEqual "noir on the two rows of one file" 2 =<< intAt "noir" genre
        assertEqual "heist on the one row of the other" 1 =<< intAt "heist" genre
        assertEqual "and a key on one row answers that one value" 1
          =<< intAt "Leonard" =<< field "Author" values

    -- A tree with nothing to complete from is a tree, not a miss: the drawer's
    -- input opens on it and offers nothing.
  , testCase "a tree whose drawers hold nothing answers empty objects" $
      withTempDir $ \dir -> do
        _ <- orgFile dir "bare.org" "* one\n"
        (a, _hub) <- serverOver dir
        v <- decoded =<< ok =<< getFrom a "/properties"
        assertEqual "no keys" [] =<< countedNames =<< field "keys" v
        assertEqual "and no values" [] =<< countedNames =<< field "values" v

    -- ORG'S PLANNING WORDS ARE NO PROPERTY VOCABULARY: this route walks
    -- DRAWERS, and the parser lifts planning off the headline before one is
    -- read.  The pair box offers the three out of `CFG.planning' instead.
  , testCase "a planned tree offers no planning word" $
      withTempDir $ \dir -> do
        _ <- orgFile dir "plan.org" (T.unlines
               [ "* one", "SCHEDULED: <2026-09-01 Tue> DEADLINE: <2026-09-05 Sat>"
               , ":PROPERTIES:", ":Genre: noir", ":END:", "* two"
               , "CLOSED: [2026-09-02 Wed 18:30]" ])
        (a, _hub) <- serverOver dir
        v <- decoded =<< ok =<< getFrom a "/properties"
        assertEqual "the drawer's own key, and nothing the planning lines spell"
                    ["Genre"] =<< countedNames =<< field "keys" v

    -- AND WHERE ANOTHER WRITER MINTED ONE INTO A DRAWER the route counts it,
    -- since that is literally where it stands: the pair the pane MIGRATES.
  , testCase "a `:SCHEDULED:' another writer put in a drawer is drawer vocabulary" $
      withTempDir $ \dir -> do
        _ <- orgFile dir "stray.org" (T.unlines
               [ "* one", ":PROPERTIES:", ":SCHEDULED: <2026-09-01 Tue>", ":END:" ])
        (a, _hub) <- serverOver dir
        keys <- field "keys" =<< decoded =<< ok =<< getFrom a "/properties"
        assertEqual "the pair as the file spells it" ["SCHEDULED"]
          =<< countedNames keys

  , postIs405 "/properties"
  ]

-- | A tree spelling one key across two files, one key on a single row, and the server's own pair in every drawer.
withDrawerTree :: (Application -> IO a) -> IO a
withDrawerTree k = withTempDir $ \dir -> do
  _ <- orgFile dir "a.org" (T.unlines
         [ "* one", ":PROPERTIES:", ":ORG_GLANCE_ID: one"
         , ":ORG_GLANCE_CREATION_TIME: [2026-08-20 Thu]"
         , ":Genre: noir", ":Rating: 5", ":END:"
         , "* two", ":PROPERTIES:", ":ORG_GLANCE_ID: two", ":Genre: noir", ":END:" ])
  _ <- orgFile dir "b.org" (T.unlines
         [ "* three", ":PROPERTIES:", ":ORG_GLANCE_ID: three"
         , ":Genre: heist", ":Author: Leonard", ":END:" ])
  (a, _hub) <- serverOver dir
  k a

-- | @capture@: the one command that names no row, and the one write whose target comes out of the config.
captureSpec :: TestTree
captureSpec = testGroup "POST /command capture"
  [ -- The target may not exist, and the empty digest is the pin for that.
    testCase "creates the target and the entry is the whole file" $
      withCaptureTree $ \a _hub dir -> do
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

  , testCase "the creation time reparses as org's own inactive timestamp" $
      withCaptureTree $ \a _hub dir -> do
        _ <- postTo a "/command" (capture "read the docs")
        written <- document (dir </> "inbox.org")
        stamp <- maybe (assertFailure ("no stamp in " <> show written)) pure
                       (between ":ORG_GLANCE_CREATION_TIME: " "\n" written)
        assertBool ("inactive and bracketed: " <> show stamp)
                   ("[" `T.isPrefixOf` stamp && "]" `T.isSuffixOf` stamp)
        assertEqual "and the shape org writes" (T.length "[2026-08-01 Sat 09:30]")
                    (T.length stamp)

  , testCase "a second capture appends and moves no byte of the first" $
      withCaptureTree $ \a _hub dir -> do
        _ <- postTo a "/command" (capture "first thing")
        before <- document (dir </> "inbox.org")
        _ <- postTo a "/command" (capture "second thing")
        after <- document (dir </> "inbox.org")
        assertBool ("appended: " <> show after) (before `T.isPrefixOf` after)
        assertContains "and the second entry is there" "* second thing" after

  , testCase "the row arrives over the watch, not out of the route" $
      withCaptureTree $ \a hub dir -> do
        _ <- postTo a "/command" (capture "TODO Buy milk")
        assertEqual "the store has not moved" 1 . length =<< rowsOf =<< getFrom a "/headlines"
        watchStep hub (dir </> "inbox.org")
        rows <- rowsOf =<< getFrom a "/headlines"
        assertEqual "and now it has" 2 (length rows)
        assertBool ("the captured row is in it: " <> show rows)
                   (any (("Buy milk" `T.isInfixOf`) . T.pack . show) rows)

    -- The entry a capture promises is ONE headline, so the two ways of making it something else are 400.
  , testCase "an empty line and a multi-line one are refused" $
      withCaptureTree $ \a _hub dir ->
        mapM_ (\(what, text') -> do
                 r <- postTo a "/command" (capture text')
                 assertEqual (what <> ": status") 400 (status r)
                 there <- doesFileExist (dir </> "inbox.org")
                 assertBool (what <> ": wrote a file anyway") (not there))
              [("empty", ""), ("blank", "   "), ("two lines", "one\n* two")]

  , testCase "and a body with no text at all says what one is" $
      withCaptureTree $ \a _hub _dir -> do
        r <- postTo a "/command"
               (encode (object ["name" .= ("capture" :: T.Text), "args" .= object []]))
        assertEqual "status" 400 (status r)
        assertContains "names the field" "text" (body r)

  , testCase "it names no rows, and is not refused for that" $
      withCaptureTree $ \a _hub _dir -> do
        assertOk =<< postTo a "/command" (capture "no ids here")

    -- THE ID THE ANSWER CARRIES has to be the id the next load spells.
  , testCase "the answer names the row the capture made" $
      withCaptureTree $ \a hub dir -> do
        r <- ok =<< postTo a "/command" (capture "TODO Buy milk")
        assertEqual "the file's own path and the next ordinal"
                    (T.pack (dir </> "inbox.org") <> "#0") =<< textAt "id" =<< decoded r
        watchStep hub (dir </> "inbox.org")
        rows <- rowsOf =<< getFrom a "/headlines"
        assertBool ("the store spells the same id: " <> show (map rowId rows))
                   ((T.pack (dir </> "inbox.org") <> "#0") `elem` map rowId rows)

    -- One rule for both shapes rather than a special case for the one that needed it.
  , testCase "a capture that creates its target delivers the row itself" $
      withCaptureTree $ \a hub dir -> do
        rid <- textAt "id" =<< decoded =<< ok =<< postTo a "/command" (capture "TODO Buy milk")
        drainNow dir hub
        rows <- rowsOf =<< getFrom a "/headlines"
        assertBool ("the captured row is there: " <> show (map rowId rows))
                   (rid `elem` map rowId rows)
  ]

-- | A TAGGED capture: the whole path from the request to the sharded blob and the @EXTERNAL.jsonl@ line naming it.
blobCaptureSpec :: TestTree
blobCaptureSpec = testGroup "POST /command capture, under a tag"
  ([ testCase "writes a blob at org-glance's own sharded path" $
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

    -- fsnotify arms a blob's fresh shard without traversing into it, so the daemon queues the path itself at write time.
  , testCase "and the row arrives with no event behind it" $
      withStoreTree $ \a hub dir -> do
        ident <- textAt "id" =<< decoded =<< ok =<< postTo a "/command" dune
        drainNow dir hub
        rows <- rowsOf =<< getFrom a "/headlines"
        assertBool ("the blob is a row: " <> show (map rowId rows))
                   (ident `elem` map rowId rows)

    -- AND SO DOES EVERY WRITE AFTER IT: the shard is unwatched for the daemon's life.
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

    -- The note rides the write door every other write leaves through: blob first, line second.
  , testCase "and one EXTERNAL.jsonl line naming it" $
      withStoreTree $ \a _hub dir -> do
        ident <- textAt "id" =<< decoded =<< ok =<< postTo a "/command" dune
        noted <- document (dir </> ".org-glance/meta/EXTERNAL.jsonl")
        assertEqual "one line" 1 (length (T.lines noted))
        assertContains "naming the blob's own id" ("{\"id\":\"" <> ident <> "\"") noted

    -- AND SO DOES A MATERIALIZE COMMIT, the fifth write site, through the same door.
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
  ]
   <> map refusedCapture blobRefusals
   <>
  [ testCase "and with no tag it is still the inbox, bare" $
      withStoreTree $ \a _hub dir -> do
        v <- decoded =<< ok =<< postTo a "/command" (capture "TODO Buy milk")
        assertEqual "the tree's inbox" (T.pack (dir </> "inbox.org")) =<< textAt "file" v
        assertEqual "no blob at all" [] =<< blobsIn dir
        written <- document (dir </> "inbox.org")
        assertEqual "the entry the bare path has always written"
                    [ "* TODO Buy milk", ":PROPERTIES:", ":END:" ]
                    [ l | l <- T.lines written, not (":ORG_GLANCE_" `T.isPrefixOf` l) ]
  ])

-- | A capture the route turns down: the tree it is posted at, the payload, and
-- what the body must name.  Every one of them is refused ahead of any write.
data Refused = Refused
  { rfLabel :: String
  , rfTree  :: (Application -> Hub -> FilePath -> Assertion) -> Assertion
  , rfPost  :: BL.ByteString
  , rfNames :: Maybe (String, T.Text)
  }

blobRefusals :: [Refused]
blobRefusals =
  [ Refused "an unanswered prompt is a 400 naming it, and writes nothing"
      withStoreTree (captureAs "book" [] "Dune") (Just ("naming the prompt", "Author"))

  , Refused "a template with no %? is a 400 naming what it lacks"
      (\k -> withStoreTree $ \a hub dir -> do
         TIO.writeFile (tagFileIn dir "film") "#+TITLE: Film\n\n* nothing here\n"
         k a hub dir)
      (captureAs "film" [] "Alien") (Just ("naming the code", "%?"))

    -- THE ONE-HEADLINE WALL REACHES THE TAGGED PATH: a newline lands a column-1 star the parser reads as a second entry.
  , Refused "a captured line carrying a newline is a 400, and writes nothing"
      withStoreTree (captureAs "book" [("Author", "Herbert")] "a\n* b")
      (Just ("naming the shape", "one headline"))

  , Refused "and so is an answer carrying one, named by its prompt"
      withStoreTree (captureAs "book" [("Author", "H\n* b")] "Dune")
      (Just ("naming the field", "Author"))

  , Refused "an answer stripped to nothing is refused too"
      withStoreTree (captureAs "book" [("Author", "   ")] "Dune")
      (Just ("naming the field", "Author"))

  , Refused "a tag that is not one is refused with the request's shape"
      withStoreTree (captureAs "not a tag" [] "x") Nothing

    -- A tree with no store is not made into one by asking.
  , Refused "a tree with no store refuses a tagged capture, naming it"
      withCaptureTree (captureAs "book" [] "Dune")
      (Just ("naming the directory", ".org-glance"))
  ]

refusedCapture :: Refused -> TestTree
refusedCapture Refused{..} = testCase rfLabel $
  rfTree $ \a _hub dir -> do
    r <- postTo a "/command" rfPost
    assertEqual "status" 400 (status r)
    mapM_ (\(what, named) -> assertContains what named (body r)) rfNames
    assertEqual "and no blob was written" [] =<< blobsIn dir

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

  , testCase "with no tag at all it is the bare shape" $
      withStoreTree $ \a _hub _dir -> do
        v <- decoded =<< ok =<< getFrom a "/capture"
        assertEqual "no template" False =<< boolAt "template" v
        assertEqual "no prompts" [] =<< textsAt "prompts" v

    -- The vocabulary is the TREE's rather than any row's — a capture names no rows to ask about.
  , testCase "the tag vocabulary is the tree's" $
      withStoreTree $ \a _hub _dir ->
        assertEqual "every tag the store holds" ["book"]
          =<< textsAt "tags" =<< decoded =<< getFrom a "/capture"

    -- ONE spelling of the expansion subset: what this serves is what expands.
  , testCase "the codes are the expansion subset, each with its meaning" $
      withStoreTree $ \a _hub _dir -> do
        codes <- listAt "codes" =<< decoded =<< getFrom a "/capture"
        assertEqual "the four v1 knows" ["%?", "%U", "%T", "%^{PROMPT}"]
          =<< traverse (textAt "code") codes
        assertBool "and each says what it does"
          . all (not . T.null) =<< traverse (textAt "means") codes

  , postIs405 "/capture"
  ]

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

-- | Every file under DIR's store.  Spelled out rather than taken off the walk, which an oracle would then agree with.
blobsIn :: FilePath -> IO [FilePath]
blobsIn dir = under (dir </> ".org-glance" </> "data")
  where
    under at = do
      isDir <- doesDirectoryExist at
      if not isDir then pure [ at | at /= dir </> ".org-glance" </> "data" ] else
        concat <$> (mapM (under . (at </>)) . sort =<< listDirectory at)

dune :: BL.ByteString
dune = captureAs "book" [("Author", "Herbert")] "Dune"

captureAs :: T.Text -> [(T.Text, T.Text)] -> T.Text -> BL.ByteString
captureAs tag answers text' = encode (object
  [ "name" .= ("capture" :: T.Text)
  , "args" .= object ([ "text" .= text', "tag" .= tag ]
                        <> [ "fields" .= object [ Key.fromText k .= v | (k, v) <- answers ]
                           | not (null answers) ]) ])

planningArg :: T.Text -> Maybe T.Text -> Value
planningArg keyword date = object ["keyword" .= keyword, "date" .= date]

withCaptureTree :: (Application -> Hub -> FilePath -> Assertion) -> Assertion
withCaptureTree k = withTempDir $ \dir -> do
  _ <- orgFile dir "notes.org" "* TODO Already here\n"
  (a, hub) <- serverOver dir
  k a hub dir

-- | The keyword layers, read and written: @GET@ lists every config file the tree has, plus the @system.org@ it could have.
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
        -- The union is the store's own palette, ORDER included, so the preview and the badges cannot disagree.
        keywords <- field "keywords" v
        assertEqual "active" ["TODO", "READING"] =<< textsAt "active" keywords
        assertEqual "inactive" ["DONE", "READ", "ABANDONED"] =<< textsAt "inactive" keywords

    -- The empty digest is the pin an absent file carries, so the record a reader is handed is the lock a writer presents back.
  , testCase "the default view rides beside the layers" $
      withConfigTree $ \a _dir -> do
        v <- decoded =<< getFrom a "/config"
        assertEqual "with no line anywhere, the built-in"
                    "state:*active*" =<< viewText "default" v

  , testCase "and a system layer naming one is what is served" $
      withConfigTree $ \a dir -> do
        digest <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        assertOk =<< postTo a "/config" (viewBody (systemAt dir) [] (Just "tag:work") digest)
        assertContains "the line is in the file" "#+GLANCE_DEFAULT_FILTER: tag:work"
          =<< document (T.unpack (systemAt dir))
        v <- decoded =<< getFrom a "/config"
        assertEqual "and the next read says so" "tag:work" =<< viewText "default" v

    -- Naming one view leaves the other's line where it was, which is what makes the sheet's per-view write honest.
  , testCase "the agenda view is a line of its own" $
      withConfigTree $ \a dir -> do
        v <- decoded =<< getFrom a "/config"
        assertEqual "with no line anywhere, the built-in"
                    "state:*active* -planned:*empty* sort:scheduled"
          =<< viewText "agenda" v
        digest <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        assertOk =<< postTo a "/config" (encode (object
          [ "path" .= systemAt dir, "digest" .= digest
          , "views" .= object ["agenda" .= ("tag:home sort:deadline" :: T.Text)] ]))
        after <- document (T.unpack (systemAt dir))
        assertContains "the line is in the file"
          "#+GLANCE_AGENDA_FILTER: tag:home sort:deadline" after
        fresh <- decoded =<< getFrom a "/config"
        assertEqual "and it is what is served" "tag:home sort:deadline"
          =<< viewText "agenda" fresh
        assertEqual "the default view is untouched" "state:*active*"
          =<< viewText "default" fresh

    -- THE TREE'S STATE HUES ride the same write and travel FLAT in both directions, so no client iterates keys.
  , testCase "state colours ride the same write and come back served" $
      withConfigTree $ \a dir -> do
        v <- decoded =<< getFrom a "/config"
        assertEqual "a build carrying two themes" ["light", "dark"]
          =<< textsAt "themes" v
        assertEqual "and no tree hue until one is written" ([] :: [Value])
          =<< listAt "colors" v
        digest <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        assertOk =<< postTo a "/config" (encode (object
          [ "path" .= systemAt dir, "digest" .= digest
          , "colors" .= [ object [ "theme" .= ("light" :: T.Text)
                                 , "keyword" .= ("TODO" :: T.Text)
                                 , "hue" .= ("#7B1FA2" :: T.Text) ]
                        , object [ "theme" .= ("light" :: T.Text)
                                 , "keyword" .= ("DONE" :: T.Text)
                                 , "hue" .= ("#00695C" :: T.Text) ] ] ]))
        after <- document (T.unpack (systemAt dir))
        assertContains "one line for the theme, the pairs in the order given"
          "#+GLANCE_STATE_COLORS: light TODO=#7B1FA2 DONE=#00695C" after
        assertEqual "served back flat" ["#7B1FA2", "#00695C"]
          =<< (traverse (textAt "hue") =<< listAt "colors" =<< decoded =<< getFrom a "/config")

  , testCase "a view no build carries is a 400 naming it" $
      withConfigTree $ \a dir -> do
        digest <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        answer <- postTo a "/config" (encode (object
          [ "path" .= systemAt dir, "digest" .= digest
          , "views" .= object ["weekly" .= ("tag:home" :: T.Text)] ]))
        assertEqual "refused" 400 (status answer)
        assertContains "naming the view and what this build has"
          "no view is called weekly" =<< textAt "error" =<< decoded answer

    -- THE PIN'S OWN SHAPE: no `lines' key at all — the block stands untouched and the filter line joins.
  , testCase "a write with no lines key leaves the block and pins the filter" $
      withConfigTree $ \a dir -> do
        digest <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        assertOk =<< postTo a "/config"
          (configBody (systemAt dir) ["#+TODO: TODO | DONE"] digest)
        before <- document (T.unpack (systemAt dir))
        fresh <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        assertOk =<< postTo a "/config" (encode (object
          [ "path" .= systemAt dir, "digest" .= fresh
          , "views" .= object
              [ "default" .= ("state:*active* columns:state,title sort:state->priority->title" :: T.Text) ] ]))
        after <- document (T.unpack (systemAt dir))
        assertContains "the pinned line — filter, columns and sort chain whole"
          "#+GLANCE_DEFAULT_FILTER: state:*active* columns:state,title sort:state->priority->title" after
        assertEqual "and every #+TODO: byte where it was"
          (todoLines before) (todoLines after)
        assertEqual "served on the next read"
          "state:*active* columns:state,title sort:state->priority->title"
          =<< viewText "default" =<< decoded =<< getFrom a "/config"

    -- A TAG LAYER IS MINTED BY BEING WRITTEN TO: `filesIn' can only list what is
    -- there, so without this a state can never be added to a tag that has no file.
  , testCase "a write to a tag with no layer file mints one" $
      withConfigTree $ \a dir -> do
        listed <- traverse (textAt "path") =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        assertBool ("film.org was already listed: " <> show listed)
                   (tagAt dir "cinema" `notElem` listed)
        assertOk =<< postTo a "/config"
          (configBody (tagAt dir "cinema") ["#+TODO: QUEUED | SEEN"] "")
        assertContains "the minted layer declares what it was written"
          "#+TODO: QUEUED | SEEN" =<< document (tagFileIn dir "cinema")
        fresh <- listAt "layers" =<< decoded =<< getFrom a "/config"
        paths <- traverse (textAt "path") fresh
        assertBool ("the minted layer is not served back: " <> show paths)
                   (tagAt dir "cinema" `elem` paths)
        again <- textAt "digest" . head
                   =<< filterM (fmap (== tagAt dir "cinema") . textAt "path") fresh
        assertOk =<< postTo a "/config"
          (configBody (tagAt dir "cinema") ["#+TODO: QUEUED WATCHING | SEEN"] again)
        assertContains "and a second write appends to the file it minted"
          "#+TODO: QUEUED WATCHING | SEEN" =<< document (tagFileIn dir "cinema")

  , testCase "a path outside the tree's own tags directory is still refused" $
      withConfigTree $ \a dir -> do
        answer <- postTo a "/config"
          (configBody (T.pack (dir </> "elsewhere.org")) ["#+TODO: A | B"] "")
        assertEqual "refused" 400 (status answer)
        assertContains "naming the layers this tree has"
          "no config layer at" =<< textAt "error" =<< decoded answer
        gone <- doesFileExist (dir </> "elsewhere.org")
        assertBool "and it wrote nothing there" (not gone)

    -- A malformed word makes `todoPragmas' yield NOTHING, so without the wall the
    -- writer is told the block came to nothing rather than which word did it.
  , testCase "a state org cannot read back is refused by name" $
      withConfigTree $ \a dir -> do
        digest <- textAt "digest" . head =<< listAt "layers" =<< decoded =<< getFrom a "/config"
        answer <- postTo a "/config"
          (configBody (systemAt dir) ["#+TODO: TODO IN-PROGRESS | DONE"] digest)
        assertEqual "refused" 400 (status answer)
        assertContains "naming the word rather than the block"
          "IN-PROGRESS is not a TODO state" =<< textAt "error" =<< decoded answer
        wrote <- doesFileExist (systemFileIn dir)
        assertBool "and a refused write minted no layer" (not wrote)

    -- `.org-glance/config/' is two directories minted at once, which fsnotify arms without entering; a config path settles as a RESEED.
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
          =<< viewText "default" =<< decoded =<< getFrom a "/config"

    -- THE CAPTURE TEMPLATE is a REGION of the same file: one file, one digest, and every layer may carry one.
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

    -- ONE WALL, keeping a blob's first headline the entry org-glance keys it by.
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

    -- A TREE-WIDE SETTING BELONGS TO A TREE rather than to a tag, and the route takes that off the LAYER it looked up.
  , testCase "a tag layer's write reaches no tree-wide setting" $
      withConfigTree $ \a dir -> do
        assertEqual "the body names every setting the registry carries"
                    (sort (map csName configSettings)) (sort (map fst everySetting))
        digest <- digestOnDisk (T.unpack (tagAt dir "book"))
        assertOk =<< postTo a "/config" (encode (object
          ([ "path" .= tagAt dir "book", "digest" .= digest
           , "lines" .= (["#+TODO: TODO | DONE"] :: [T.Text]) ]
             <> [ Key.fromText k .= v | (k, v) <- everySetting ])))
        after <- document (T.unpack (tagAt dir "book"))
        -- Every tree-wide member is a `#+GLANCE_' line, so the claim is over the family rather than three spellings.
        assertBool ("a tree-wide line reached a tag layer: " <> show after)
                   (not ("#+GLANCE_" `T.isInfixOf` after))
        -- And the mask is not a blanket one, or the claim above would hold for a route that wrote nothing.
        assertContains "while the template, which every layer owns, landed"
                       "* %?" after

  , testCase "the served page carries the tree's default view" $ do
      withConfigTree $ \a _dir ->
        assertContains "the built-in, where nothing configures one"
                       "\"views\":[{\"id\":\"default\",\"query\":\"state:*active*\"}"
                       . body =<< getFrom a "/"
      withTempDir $ \dir -> do
        let system = systemFileIn dir
        createDirectoryIfMissing True (takeDirectory system)
        TIO.writeFile system
          "#+TODO: TODO | DONE\n#+GLANCE_DEFAULT_FILTER: tag:work\n"
        _ <- orgFile dir "notes.org" "* TODO x\n"
        (a, _hub) <- serverOver dir
        assertContains "the tree's own" "\"query\":\"tag:work\"" . body
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
        -- The receipt is the file's new digest, so a second write needs no second read.
        receipt <- textAt "digest" =<< decoded r
        onDisk <- digestOnDisk (T.unpack (tagAt dir "book"))
        assertEqual "the receipt is the file's new digest" onDisk receipt

  , testCase "inserts under the header when the file carries no block" $
      withConfigTree $ \a dir -> do
        let path = T.unpack (tagAt dir "film")
        digest <- digestOnDisk path
        assertOk =<< postTo a "/config" (configBody (tagAt dir "film") ["#+TODO: A | B"] digest)
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

    -- The empty digest means "nothing is there", so a file that turned up meanwhile refuses the way a moved one does.
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
              -- ONE ROW, because WHAT a block may say is `configEdits'' rule and `TestConfig' enumerates it there.
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

    -- The route is a writer like the other two, so it leaves the store alone.
  , testCase "leaves the store alone — the watch is what reseeds" $
      withConfigTree $ \a dir -> do
        before <- badgeValues =<< decoded =<< getFrom a "/headlines"
        digest <- digestOnDisk (T.unpack (tagAt dir "book"))
        _ <- postTo a "/config"
               (configBody (tagAt dir "book") ["#+TODO: TODO READING NEXT | READ"] digest)
        layers <- listAt "layers" =<< decoded =<< getFrom a "/config"
        assertEqual "the file"
                    [[], ["#+TODO: TODO READING NEXT | READ"], []]
          =<< traverse (textsAt "lines") layers
        assertEqual "the badges the table is showing" before
          =<< badgeValues =<< decoded =<< getFrom a "/headlines"
        assertBool "and NEXT is not among them" ("NEXT" `notElem` before)
  ]

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

-- | @GET \/keywords@: the resolution READ FORWARDS — a keyword under the WIDEST source that declares it and nowhere below.
keywordsSpec :: TestTree
keywordsSpec = testGroup "GET /keywords"
  ([ testCase "the default pair leads and every source below it loses those words" $
      withLayeredTree $ \a -> do
        r <- ok =<< getFrom a "/keywords?ids=filed"
        -- The chain ENDS at the file: `film''s cycle is recognized here and no scope this row reaches claims it.
        assertEqual "org's own, then the system layer, then book" orgSystemBook
          =<< sourcesOf r
        assertEqual "and nothing was asked for that is not there" [] =<< textsAt "unknown"
          =<< decoded r

    -- The word belongs to the widest scope that names it.
  , testCase "a file redeclaring a wider scope's word gets no row of its own" $
      withLayeredTree $ \a -> do
        filed <- sourcesOf =<< getFrom a "/keywords?ids=filed"
        assertEqual "the row whose file declares nothing answers the same" filed
          =<< sourcesOf =<< getFrom a "/keywords?ids=tagged"
        assertEqual "and no source is named for the file at all" []
          [ src | (src, _a, _i) <- filed, src == "file" ]
   ]
   <> [ testCase label $ withLayeredTree $ \a ->
          assertEqual what want =<< sourcesOf =<< getFrom a path
      | (label, path, what, want) <- resolved ]
    -- An id the store has no row for is named rather than refused, so a stale marked set still answers.
   <> idsParamCases withLayeredTree "/keywords" sourcesOf
        ("tagged", orgSystemBook)
        ("tagged", "filmed", orgSystemBook <> [("film", ["WATCHING"], ["WATCHED"])])
   <>
  [ testCase "every id unknown resolves nothing and still says which" $
      withLayeredTree $ \a -> do
        r <- ok =<< getFrom a "/keywords?ids=nosuch"
        assertEqual "no sources" [] =<< sourcesOf r
        assertEqual "and both halves of why" ["nosuch"] =<< textsAt "unknown" =<< decoded r

  , postIs405 "/keywords"

    -- A tag keeps its tag RANK, so a tag spelled `system' sits BELOW the system layer.
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

    -- Both cells are spelled against the alphabet on purpose: the palette's letters are assigned over this order.
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
  ])

  where
    resolved =
      [ ( "the first tag that declares a keyword is the one that keeps it"
        , "/keywords?ids=tagged", "book, and no pile row at all", orgSystemBook )
      , ( "a row no scope speaks for is offered org's own and the system layer"
        , "/keywords?ids=bare", "org's own and the system layer, and nothing under them"
        , [("default", ["TODO"], ["DONE"]), ("system", ["STARTED"], ["READ"])] )
      , ( "two rows under different tags bring both tag sources"
        , "/keywords?ids=tagged,filmed", "book from one, film from the other"
        , orgSystemBook <> [("film", ["WATCHING"], ["WATCHED"])] )
        -- The table describes the SET rather than any one member of it.
      , ( "a keyword wider in one row than another lands in the wider source"
        , "/keywords?ids=tagged,filed", "one answer over both rows, widest source first"
        , orgSystemBook ) ]

-- | org's own pair, then the system layer, then book: what a layered row resolves through.
orgSystemBook :: [(T.Text, [T.Text], [T.Text])]
orgSystemBook = [ ("default", ["TODO"],     ["DONE"])
                , ("system",  ["STARTED"],  ["READ"])
                , ("book",    ["READING"],  []) ]

-- | @POST \/command edit-link@: the ROUND TRIP a client makes, and the refusals that are the route's rather than the math's.
editLinkSpec :: TestTree
editLinkSpec = testGroup "POST /command edit-link"
  [ testCase "the range /links handed out is the range the write splices" $
      withLinkable $ \a _hub path -> do
        before <- document path
        r <- ok =<< postEditLink a "first" 0 ["target" .= ("https://z.example" :: T.Text)]
        assertEqual "the row landed" [("first", True)] =<< outcomesOf r
        assertEqual "the file is the old one with one target replaced"
          (T.replace "[[https://a.example][A]]" "[[https://z.example][A]]" before)
          =<< document path
        onDisk <- digestOnDisk path
        assertEqual "the digest it reports is the file's" [onDisk] =<< digestsOf r

    -- Each form goes through the range the route itself reported, so what this pins is that the offsets survive the wire.
  , testCase "a description added, kept and taken off, over the wire" $ do
      withLinkable $ \a _hub path -> do
        before <- document path
        _ <- postEditLink a "first" 1 [ "target" .= ("https://b.example" :: T.Text)
                                      , "desc" .= ("B" :: T.Text) ]
        assertEqual "the bracketed bare link took a description"
          (T.replace "[[https://b.example]]" "[[https://b.example][B]]" before)
          =<< document path
      withLinkable $ \a _hub path -> do
        before <- document path
        _ <- postEditLink a "first" 2 ["target" .= ("https://d.example" :: T.Text)]
        assertEqual "and the plain URL swapped its target and stayed plain"
          (T.replace "https://c.example" "https://d.example" before) =<< document path
      withLinkable $ \a _hub path -> do
        before <- document path
        _ <- postEditLink a "first" 0 [ "target" .= ("https://a.example" :: T.Text)
                                      , "desc" .= Null ]
        assertEqual "a null description leaves a desc-less bracketed link"
          (T.replace "[[https://a.example][A]]" "[[https://a.example]]" before)
          =<< document path

    -- THE PIN: a digest the store no longer has is refused per id, since a digest is per file.
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

    -- The digest is per file, so nothing but the subtree wall stands between one row's write and another row's link.
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

    -- A SPAN NAMES ONE ROW's own text, so the command names one row.
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

    -- THE ROW COUNT IS THE COARSEST THING WRONG, and it is `csArgs' asking — there is no separate ids rule above it.
  , testCase "and the count outranks everything else its args owe" $
      withLinkable $ \a _hub _path -> do
        r <- postTo a "/command"
               (linkCommand "edit-link" ["first", "second"] (object []) [])
        assertEqual "status" 400 (status r)
        assertContains "the count outranks the missing span" "one row" (body r)

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

    -- A link in the TITLE is a cell; one in the body moves no cell at all, which is why the popup re-asks.
  , testCase "a title link reaches the row over the watch" $
      withLinkable $ \a hub path -> do
        _ <- postEditLink a "first" 0 [ "target" .= ("https://a.example" :: T.Text)
                                      , "desc" .= ("Alpha" :: T.Text) ]
        watchStep hub path
        r <- getFrom a "/headlines"
        assertEqual "the cell carries the line the file holds"
          ["one [[https://a.example][Alpha]]", "two [[https://e.example][E]]"]
          =<< traverse (cellAt "title") =<< rowsOf r

  , testCase "and /links answers with the edited link once the watch has run" $
      withLinkable $ \a hub path -> do
        _ <- postEditLink a "first" 0 ["target" .= ("https://z.example" :: T.Text)]
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

-- | A row pointing three ways, and a second row with a link of its OWN: the ids rule and the subtree wall each need one.
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

withLinkable :: (Application -> Hub -> FilePath -> Assertion) -> Assertion
withLinkable k = withTempDir $ \dir -> do
  path <- orgFile dir "notes.org" linkable
  (a, hub) <- serverOver dir
  k a hub path

-- | The span A reports for ROW's link at AT, and the digest that answer carried — what the popup holds and sends back.
pinnedSpan :: Application -> ByteString -> Int -> IO (Value, T.Text)
pinnedSpan a rid at = do
  answer <- decoded =<< getFrom a ("/links?id=" <> rid)
  links <- listAt "links" answer
  sp <- field "span" (links !! at)
  (,) sp <$> textAt "digest" answer

-- | A command as the LINK POPUP sends one: under the digests it was measured against.
linkCommand :: T.Text -> [T.Text] -> Value -> [(T.Text, T.Text)] -> BL.ByteString
linkCommand name ids args digests = encode (object
  [ "name" .= name, "ids" .= ids, "args" .= args
  , "digests" .= object [ Key.fromText rid .= digest | (rid, digest) <- digests ] ])

-- | The WHOLE round trip the popup makes for ROW's link at AT: the span and digest A just reported, sent back as @edit-link@ with ARGS beside the span.
postEditLink :: Application -> T.Text -> Int -> [Pair] -> IO SResponse
postEditLink a rid at args = do
  (sp, digest) <- pinnedSpan a (TE.encodeUtf8 rid) at
  postTo a "/command" (linkCommand "edit-link" [rid] (object (("span" .= sp) : args))
                                   [(rid, digest)])

-- | The picker's door on to the SAME pipeline @GET \/headlines@ answers with.  What
-- is asked here is only what the picker adds: the two cuts, and that the shape
-- did not fork.
referSpec :: TestTree
referSpec = testGroup "GET /refer"
  [ testCase "answers the view /headlines answers, so one mount reads both" $
      withReferTree $ \a -> do
        table <- ok =<< getFrom a "/headlines?q=refer-a"
        pick  <- ok =<< getFrom a "/refer?q=refer-a"
        cols  <- listAt "columns" =<< decoded table
        assertEqual "the columns are one table's" cols
          =<< listAt "columns" =<< decoded pick
        wanted <- referIds table
        assertEqual "and the rows the query names are the same rows" wanted
          =<< referIds pick

  , testCase "a row with no ORG_GLANCE_ID is not offered: it cannot be linked to" $
      withReferTree $ \a -> do
        table <- referIds =<< ok =<< getFrom a "/headlines"
        pick  <- referIds =<< ok =<< getFrom a "/refer"
        assertBool ("the table did not carry the unaddressable row: " <> show table)
                   (any ("#" `T.isInfixOf`) table)
        assertEqual "every offered row is addressable" [] (filter ("#" `T.isInfixOf`) pick)

  , testCase "a row is not its own reference" $
      withReferTree $ \a -> do
        with    <- referIds =<< ok =<< getFrom a "/refer"
        without <- referIds =<< ok =<< getFrom a "/refer?row=refer-a"
        assertBool "the fixture never offered it" ("refer-a" `elem` with)
        assertEqual "the row it stands on is gone"
                    (filter (/= "refer-a") with) without

    -- THE KINDS THE TREE ALREADY USES, counted in ROWS the way `/tags' counts
    -- them: free text is how a kind is minted, so an established spelling has to
    -- be tellable from a typo made once.
  , testCase "the kinds the tree uses come back counted, commonest first" $
      withReferTree $ \a -> do
        answer <- decoded =<< ok =<< getFrom a "/refer"
        kinds <- traverse (\v -> (,) <$> textAt "kind" v <*> intAt "rows" v)
                   =<< listAt "kinds" answer
        assertEqual "cites is on two rows, refutes on one" [("cites", 2), ("refutes", 1)] kinds

  , testCase "a plain mention declares no kind, so none is counted for it" $
      withReferTree $ \a -> do
        answer <- decoded =<< ok =<< getFrom a "/refer?q=cites%20two"
        kinds <- traverse (textAt "kind") =<< listAt "kinds" answer
        assertEqual "the one row asked for makes one typed edge" ["cites"] kinds

    -- ONE SLUG ACROSS TWO PROGRAMS, and the SERVER is where it is spelled: the
    -- page sends what was typed and writes back what org-glance would have.
  , testCase "a typed kind comes back canonical, and nothing comes back for none" $
      withReferTree $ \a -> do
        let slugOf q = sparseTextAt "kind" =<< decoded =<< ok =<< getFrom a q
        assertEqual "downcased, trimmed, its spaces folded"
                    (Just "roasted-by") =<< slugOf "/refer?kind=Roasted%20By"
        assertEqual "already canonical, unchanged"
                    (Just "roasted-by") =<< slugOf "/refer?kind=roasted-by"
        assertEqual "no kind asked for, none answered" Nothing =<< slugOf "/refer"
        assertEqual "a kind of pure space declares nothing"
                    Nothing =<< slugOf "/refer?kind=%20"

  , testCase "the query narrows it exactly as it narrows the table" $
      withReferTree $ \a ->
        assertEqual "state:*active* over the addressable rows" ["refer-a"]
          =<< referIds =<< ok =<< getFrom a "/refer?q=state%3A*active*"
  ]

referIds :: SResponse -> IO [T.Text]
referIds r = traverse (textAt "id") =<< rowsOf r

-- | Two addressable rows and one without an id, which is the case the picker cuts.
withReferTree :: (Application -> Assertion) -> Assertion
withReferTree k = withTempDir $ \dir -> do
  _ <- orgFile dir "refer.org" $ T.unlines
    [ "* TODO refer-me alpha"
    , ":PROPERTIES:"
    , ":ORG_GLANCE_ID: refer-a"
    , ":END:"
    , "* DONE refer-me beta"
    , ":PROPERTIES:"
    , ":ORG_GLANCE_ID: refer-b"
    , ":END:"
    , "* TODO refer-me with no id at all"
      -- TYPED EDGES, so the kind vocabulary has something to fold: one kind on
      -- two rows, one on one, and a plain mention that declares none.
    , "* DONE refer-me cites two"
    , ":PROPERTIES:"
    , ":ORG_GLANCE_ID: refer-c"
    , ":END:"
    , "sees [[glance:refer-a?kind=cites][alpha]] and [[glance:refer-b][beta]]"
    , "* DONE refer-me cites and refutes"
    , ":PROPERTIES:"
    , ":ORG_GLANCE_ID: refer-d"
    , ":END:"
    , "sees [[glance:refer-a?kind=cites][alpha]] and [[glance:refer-b?kind=refutes][beta]]"
    ]
  (a, _hub) <- serverOver dir
  k a

-- | @GET \/links@: the route — the id it takes, the shape it answers in, and the refusals it shares with materialize.
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

    -- The type is the SERVER's word for the target; the derivation is `TestQuery''s.
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

    -- EVERY LINK CARRIES ITS SPAN, into the FILE: asserted by cutting each range out of the file on disk.
  , testCase "every link carries the file range that spells it" $
      withLinkTree $ \a dir -> do
        r <- getFrom a "/links?id=linked"
        text <- document (dir </> "a.org")
        assertEqual "each range cuts its own link out of the file"
          [ "[[https://x.example/a][the first]]", "https://y.example/b"
          , "https://z.example/c" ]
          . map (charSpan text) =<< spansOf r

    -- And the file's DIGEST, which is the lock an edit is pinned to.
  , testCase "and the digest those spans were measured against" $
      withLinkTree $ \a dir -> do
        r <- getFrom a "/links?id=linked"
        onDisk <- digestOnDisk (dir </> "a.org")
        assertEqual "the file's own" onDisk =<< textAt "digest" =<< decoded r

  , postIs405 "/links?id=linked"
  ]

linksOf :: SResponse -> IO [[T.Text]]
linksOf r = traverse one =<< listAt "links" =<< decoded r
  where one v = sequence [textAt "target" v, textAt "desc" v, textAt "type" v]

spansOf :: SResponse -> IO [(Int, Int)]
spansOf r = traverse one =<< listAt "links" =<< decoded r
  where one v = listAt "span" v >>= pair
        pair [Number from, Number to] = pure (round from, round to)
        pair other = assertFailure ("expected a [start, end] span, got " <> show other)

charSpan :: T.Text -> (Int, Int) -> T.Text
charSpan text (from, to) = T.take (to - from) (T.drop from text)

-- | The first row has a link under a child, so the route's answer shows it read the SUBTREE.
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

sourcesOf :: SResponse -> IO [(T.Text, [T.Text], [T.Text])]
sourcesOf r = traverse one =<< listAt "sources" =<< decoded r
  where one v = (,,) <$> textAt "source" v <*> textsAt "active" v <*> textsAt "inactive" v

-- | A tree whose every layer says something about the same keywords, so which one ANSWERS is observable at each rung.
withLayeredTree :: (Application -> IO a) -> IO a
withLayeredTree k = withTempDir $ \dir -> do
  writeLayers dir
    [ (Nothing,       "#+TODO: STARTED | READ\n")
    , (Just "book",   "#+TODO: READING | READ\n")
    , (Just "pile",   "#+TODO: | READING\n")
    , (Just "film",   "#+TODO: WATCHING | WATCHED\n") ]
  _ <- orgFile dir "a.org" (T.unlines
         [ "#+TODO: READING |", "* READING one :book:pile:"
         , ":PROPERTIES:", ":ORG_GLANCE_ID: filed", ":END:" ])
  _ <- orgFile dir "b.org" (T.unlines
         [ "* two :book:pile:", ":PROPERTIES:", ":ORG_GLANCE_ID: tagged", ":END:"
         , "* three :film:", ":PROPERTIES:", ":ORG_GLANCE_ID: filmed", ":END:"
         , "* four", ":PROPERTIES:", ":ORG_GLANCE_ID: bare", ":END:" ])
  (a, _hub) <- serverOver dir
  k a

-- | DIR's system layer and its layer for TAG, off the library's own layout ('Data.Org.Config.configDirIn').
systemAt :: FilePath -> T.Text
systemAt = T.pack . systemFileIn

tagAt :: FilePath -> FilePath -> T.Text
tagAt dir tag = T.pack (tagFileIn dir tag)

configBody :: T.Text -> [T.Text] -> T.Text -> BL.ByteString
configBody path lines' = layerBody path lines' Nothing Nothing

viewBody :: T.Text -> [T.Text] -> Maybe T.Text -> T.Text -> BL.ByteString
viewBody path lines' want = layerBody path lines' want Nothing

-- | The query a captured @POST \/config@ body names for view ID: the WRITE's shape, an OBJECT keyed by id.
wroteView :: T.Text -> Value -> IO T.Text
wroteView vid v = field "views" v >>= textAt vid

-- | The ONE config write ANSWER carried, a failure at any other count.
oneConfigWrite :: Value -> IO Value
oneConfigWrite answer = do
  writes <- listAt "configWrites" answer
  case writes of
    [one]  -> pure one
    others -> assertFailure ("expected one config write, got " <> show (length others))

-- | A write's hues, flat: theme, keyword and hue per line.
coloursOf :: Value -> IO [[T.Text]]
coloursOf v = traverse (\h -> traverse (`textAt` h) ["theme", "keyword", "hue"])
                =<< listAt "colors" v

-- | A write naming EVERY setting 'configSettings' carries.  The names are asserted against the registry where this is used.
everySetting :: [(T.Text, Value)]
everySetting =
  [ ("views",    object ["default" .= ("tag:work" :: T.Text)])
  , ("colors",   toJSON [ object [ "theme" .= ("light" :: T.Text)
                                 , "keyword" .= ("TODO" :: T.Text)
                                 , "hue" .= ("#7B1FA2" :: T.Text) ] ])
  , ("template", toJSON ("* %?" :: T.Text))
  ]

-- | A layer write over all three of its lines; the three ride in one request because they are lines of one file.
layerBody :: T.Text -> [T.Text] -> Maybe T.Text -> Maybe T.Text -> T.Text -> BL.ByteString
layerBody path lines' want target = templateBody path lines' want target Nothing

templateBody :: T.Text -> [T.Text] -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text
             -> T.Text -> BL.ByteString
templateBody path lines' want target template digest = encode (object
  ([ "path" .= path, "lines" .= lines', "digest" .= digest ]
     <> [ "views" .= object ["default" .= f] | Just f <- [want] ]
     <> [ "capture" .= c | Just c <- [target] ]
     <> [ "template" .= t | Just t <- [template] ]))

-- | @?q=columns:@ shapes the COLUMN SET the answer declares and fills.  The grammar's own cases are @TestFilter@'s.
columnsQuerySpec :: TestTree
columnsQuerySpec = testGroup "GET /headlines?q=columns:"
  [ testCase "no columns token serves the default six" $ do
      v <- get assetsDir "/headlines?q=state:*active*" >>= decoded
      assertEqual "the default view"
                  ["state", "priority", "title", "scheduled", "deadline", "tag"]
        =<< columnKeysOf v

  , testCase "a columns token picks the set, in written order" $ do
      v <- get assetsDir "/headlines?q=columns:State,Title,Tags" >>= decoded
      assertEqual "keys and headers resolve case-insensitively"
                  ["state", "title", "tag"] =<< columnKeysOf v
      row <- head <$> listAt "rows" v
      cells <- field "cells" row
      case cells of
        Object o -> assertEqual "the cells are keyed by the picked set"
                                ["state", "tag", "title"]
                                (sort (map Key.toText (KM.keys o)))
        _        -> assertFailure ("expected a cells object, got " <> show cells)

  , testCase "the minimal set is Title, injected in front when unnamed" $ do
      v <- get assetsDir "/headlines?q=columns:state" >>= decoded
      assertEqual "title joined first" ["title", "state"] =<< columnKeysOf v
      named <- get assetsDir "/headlines?q=columns:tags,title,state" >>= decoded
      assertEqual "named, it stays where it was put"
                  ["tag", "title", "state"] =<< columnKeysOf named

  , testCase "an empty list falls back to the default" $ do
      v <- get assetsDir "/headlines?q=columns:" >>= decoded
      assertEqual "the default view"
                  ["state", "priority", "title", "scheduled", "deadline", "tag"]
        =<< columnKeysOf v

  , testCase "an unknown name is a custom column reading the property drawer" $ do
      v <- get assetsDir "/headlines?q=columns:ORG_GLANCE_ID" >>= decoded
      cols <- listAt "columns" v
      pairs <- mapM (\c -> (,) <$> textAt "key" c <*> textAt "header" c) cols
      assertEqual "title's floor, then folded key under verbatim header"
                  [("title", "Title"), ("org_glance_id", "ORG_GLANCE_ID")] pairs
      rows <- listAt "rows" v
      ids <- mapM (maybeTextAt "org_glance_id" <=< field "cells") rows
      assertBool "the fixture id is a cell now"
                 ("ship-table-view" `elem` [ i | Just i <- ids ])

  , testCase "closed is the planning line's own timestamp" $
      withTempDir $ \dir -> do
        _ <- orgFile dir "notes.org" $ T.unlines
          [ "* DONE finished task"
          , "CLOSED: [2026-08-01 Sat 10:30]"
          , "* TODO open task" ]
        (a, _hub) <- serverOver dir
        rows <- rowsOf =<< getFrom a "/headlines?q=columns:title,Closed"
        stamps <- mapM (maybeTextAt "closed" <=< field "cells") rows
        assertEqual "the stamp verbatim, and null where there is none"
                    [Just "[2026-08-01 Sat 10:30]", Nothing] stamps

  , testCase "a negation and an alternation are the whole request's 400" $ do
      bad <- get assetsDir "/headlines?q=-columns:state"
      assertEqual "status" 400 (status bad)
      assertContains "naming the token" "-columns:state" (body bad)
      alt <- get assetsDir "/headlines?q=columns:a%7Cb"
      assertEqual "status" 400 (status alt)
      assertContains "naming the token" "columns:a|b" (body alt)

    -- The badge palette rides the KEY, so a picked state column still carries it.
  , testCase "a picked state column keeps its badges" $ do
      v <- get assetsDir "/headlines?q=columns:state,title" >>= decoded
      col <- columnOf "state" v
      badges <- listAt "badges" col
      assertBool "the palette is there" (not (null badges))
  ]

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

    -- THE COUPLING IS THE META'S ALONE: the bare word is an ordinary tag predicate and reveals nothing.
  , testCase "the plain tag predicate filters without lifting the exclusion" $
      withArchived $ \a -> do
        plain <- getFrom a "/headlines?q=tag%3Aarchive"
        assertEqual "the rows it reaches" ["near"] . map rowId =<< rowsOf plain
        assertEqual "and the archived one it does not" (Just "1")
                    (header "X-Glance-Archived" plain)
        meta <- getFrom a "/headlines?q=tag%3A*archive*"
        assertEqual "the meta is the whole tag" ["filed"] . map rowId =<< rowsOf meta

    -- As free text `tag:*archive*' matches nothing, so a match is the predicate reading the tags cell.
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

-- | Four rows, one tagged @ARCHIVE@ and one whose own tag merely HOLDS the word — which tells the meta from the predicate.
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

pageSpec :: IO T.Text -> TestTree
pageSpec shell = testGroup "GET /"
  [ testCase "with assets, is a shell that fetches and mounts" $ do
      r <- ok =<< get assetsDir "/"
      assertEqual "content type" (Just "text/html; charset=utf-8") (header "Content-Type" r)
      assertContains "renderer" "src=\"table-view.js\"" (body r)
      assertContains "the shell's script" "src=\"glue.js\"" (body r)
      g <- ok =<< get assetsDir "/glue.js"
      assertContains "fetch glue" "fetch(`/headlines${params}`" (body g)
      assertContains "mount" "TableView.mount(" (body g)

  , testCase "with assets, the restored query is the renderer's own chips" $ do
      b <- shell
      holdsAll "restore glue"
            [ "initialQuery: query,"
            -- An asset predating the option drops it silently, so the mount asks whether it took.
            , "const holds = (q) => can(table, \"getQuery\")"
            , "&& table.getQuery() === q;"
            , "if (query && !holds(query)) showQuery();"
            , "function showQuery() {" ] b
      -- One restoration point: `start' re-fetches and re-mounts for every way back in.
      assertEqual "showQuery is called from the mount alone" 1
                  (T.count "!holds(query)) showQuery();" b)
      assertEqual "showQuery is defined once" 1 (T.count "function showQuery()" b)

  , testCase "with assets, DEL takes the last token off through the renderer" $ do
      b <- shell
      -- The chips are the renderer's, so the strip is too: the shell asks and then follows.
      mapM_ (\needle -> assertContains "DEL glue" needle b)
            [ "table.stripLastToken()", "table.getQuery().trim()"
            , "filterDrop: (b) => {", "said(b, \"no filter\")"
            , "said(b, left ? `filter: ${JSON.stringify(left)}` : \"filter cleared\");"
            , "wants(b, \"filter tokens\", \"stripLastToken\", \"getQuery\")"
            , "wants(b, \"filter tokens\", \"stripLastToken\", \"getQuery\")"
            -- One press, one token: a held DEL claims the key and runs once, where held movement keeps repeating.
            , "if (!(repeating(e) && MAPS.once.indexOf(hit.command) !== -1)) run(hit);" ]
      onceOf b >>= assertEqual "the commands auto-repeat is off for" onceNames
      -- The guard is per command, so it cannot take auto-repeat off movement.
      assertBool "the repeat guard is blanket rather than per command"
                 (not ("if (e.repeat) return" `T.isInfixOf` b))
      holdsNone "a superseded filter path" ["glance-filter-history", "function withoutLast"] b

  , testCase "with assets, the sheet is buttonless and syncs on the way out" $ do
      b <- shell
      holdsAll "sheet glue"
            -- EITHER pane moving is dirty, and a pristine close is no request at all.
            [ "const dirty = () => editing !== null"
            , "&& (raw ? el(\"mtext\").value !== base : edited() !== baseProps);"
            -- The close ladder is the SHEET's rather than this sheet's: one rule over whichever of the two is up.
            , "if (!s.dirty()) { s.shut(); return; }"
            , "if (s.state !== \"syncing\") s.flush().then((ok) => ok && s.shut());"
            , "flush: () => flush(editing.digest),"
            -- The backdrop is the mouse's ESC.
            , "if (e.target === el(id)) leaveSheet();"
            -- The receipt chains: the 200's digest is the next flush's lock.
            , "h.digest = a.body.digest;"
            , "base = raw ? sent.org : base;"
            , "baseProps = raw ? null : stamp(sent.properties, sent.planning);"
            , "if (a.status === 409 && a.body.reason !== \"planning\") sync(\"conflict\");"
            , "conflict — C-x C-s overwrite · ESC discard"
            , "if (s.state === \"conflict\" || s.state === \"error\") {"
            , "append(s.scope, \"info\", s.closed);"
            , "closed: \"closed without writing — the file is as it was\","
            , "addEventListener(\"beforeunload\""
            , "post(editing.id, editing.digest, asked(), { keepalive: true }, editing.child)" ] b
      -- One word carries a sheet's state, `note' is its only writer, and the retry line is one constant.
      holdsAll "sync status"
            [ "synced: \"synced\"", "syncing: \"syncing…\"", "id=\"mnote\""
            , "const RETRY = \" — C-x C-s retry · ESC discard\";"
            , "error: \"error\" + RETRY };"
            , "function note(s, next, message) {", "s.state = next;"
            , "const sync = (next, message) => note(subtreeSheet, next, message);" ] b
      assertEqual "note is the only writer" 1 (T.count "      s.state = next;" b)
      assertEqual "and the retry line is spelled once" 1
                  (T.count " — C-x C-s retry · ESC discard" b)
      holdsNone "a sheet button"
        [ "id=\"msave\"", "id=\"mcancel\"", "id=\"mredo\"", "id=\"mfoot\"", "Re-materialize" ] b

  , testCase "with assets, the page is one column the viewport tall" $ do
      b <- shell
      holdsAll "column"
            [ "height:100vh;box-sizing:border-box;overflow:hidden;"
            , "padding:24px;display:flex;flex-direction:column;gap:14px}"
            -- The key line never gives its height up, so a short window squeezes the table rather than clipping the line.
            , "#app{flex:1 1 auto;min-height:0}"
            , "#kbd{flex:none;" ] b
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
      -- Commands, not keys, in the order the line reads them: each spelling comes out of the one map.
      hints <- hintsOf b
      assertEqual "the key line's table"
        [ (["next-row", "previous-row"], "rows")
        , (["next-column", "previous-column"], "cells")
        , (["previous-page", "next-page"], "pages")
        -- Without the second sentence a reader takes `<' for a within-page key and never finds out that it climbs.
        , (["first-row", "last-row"], "first/last row, again = page up/down")
        , (["toggle-sort"], "sort")
        , (["org-glance-overview:materialize"], "materialize")
        , (["org-glance-overview:open"], "open link")
        , (["mark-toggle", "unmark", "unmark-all", "mark-all"], "mark")
        , (["org-glance-overview:todo"], "state")
        , (["priority-up", "priority-down"], "priority")
        , (["org-agenda-set-tags"], "tags")
        , (["org-glance-overview:schedule", "org-glance-overview:deadline"]
          , "schedule/deadline")
        , (["org-glance-overview:capture"], "capture")
        -- `state' runs over the MARKED set; archiving runs over the FLAGGED one.
        , (["archive-flag"], "flag for archive")
        , (["archive-flag", "org-glance-overview:delete"], "archive flagged")
        , (["filter-rows"], "filter")
        -- The line names both doors, or the whole grammar is a key nobody finds.
        , (["compose-query"], "whole query")
        , (["apply-default-filter"], "default view")
        , (["org-glance-agenda"], "agenda")
        -- The drill, named beside the key that walks back out of it: a reader shown only the way in has no way home.
        , (["org-glance-overview:relations"], "references")
        , (["filter-drop-token"], "unmark/drop token/back")
        , (["customize"], "settings")
        , (["quit-window"], "quit")
        ] hints
      -- Every command it names is one the map binds, in the table scope, with a handler behind it.
      rows <- keymapOf b
      let offered = [ c | (_k, _s, c, Just _h, "table", _help) <- rows ]
      assertEqual "hinted but unbound" []
        [ c | (cs, _label) <- hints, c <- cs, c `notElem` offered ]
      -- No literal key in the line: only the blob knows which key runs what.
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

-- | A keymap row: the keys the dispatch matches, the notation the echo shows, the command, the handler, the scope, the help.
type Row = ([T.Text], T.Text, T.Text, Maybe T.Text, T.Text, Maybe T.Text)

-- | The shell's keymap as the data it is.  The expected map is written down rather than imported: an oracle from the code would agree with anything.
expectedRows :: [Row]
expectedRows =
  -- The letters lead: the resident key line shows the first row bound to a command.
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
  , (["^"],          "^",       "toggle-sort",                     Just "toggleSort",     "table",
       Just "put this column at the head of the order; again reverses it")
  , (["RET"],        "RET",     "org-glance-overview:materialize", Just "materializeRow", "table", Nothing)
  , (["/"],          "/",       "filter-rows",                     Just "focusFilter",    "table",
       Just "summon the filter box onto the chip strip")
  -- The other door onto the one query: `/' edits the filter half, `.' the whole.
  , (["."],          ".",       "compose-query",                   Just "focusQuery",     "table",
       Just "the whole expression: filters, sort: and columns: together")
  , (["DEL"],        "DEL",     "filter-drop-token",               Just "filterDrop",     "table",
       Just "unmark all, else drop the filter's last token")
  , (["g"],          "g",       "apply-default-filter",            Just "applyDefault",   "table",
       Just "the view this tree opens on")
  , (["P"],          "P",       "set-saved-view",                  Just "pinView",        "table",
       Just "pin the applied view, into whichever saved view answers")
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
  -- Two spellings of one command, so one help line.
  , (["o"],          "o",       "org-glance-overview:open",        Just "openLinks",      "table", openHelp)
  , (["!"],          "!",       "org-glance-overview:open",        Just "openLinks",      "table", openHelp)
  , (["A"],          "A",       "org-glance-agenda",               Just "applyAgenda",    "table",
       Just "the active rows carrying a date, earliest first")
  , (["@"],          "@",       "org-glance-overview:relations",   Just "relations",      "table",
       Just "the rows referring to this one; DEL walks back")
  , (["+"],          "+",       "org-glance-overview:capture",     Just "capture",        "table",
       Just "a headline for the inbox, typed as org")
  , (["d"],          "d",       "archive-flag",                    Just "archiveFlag",    "table",
       Just "flag for archive; d again archives all flagged")
  -- org-glance's own name for dired's key, with a help line because the name covers TWO steps.
  , (["D"],          "D",       "org-glance-overview:delete",      Just "archiveRows",    "table",
       Just "archive the flagged; an already-archived row deletes, on a typed word")
    -- dired's OTHER half of the pair: `x' takes the FLAGS alone and asks first, naming the count.
  , (["x"],          "x",       "dired-do-flagged-delete",         Just "flaggedDelete",  "table",
       Just "act on the flagged rows, after asking; d flags, D is the quick one")
    -- Org's own priority keys, and they CYCLE: a ring of three plus none, so a press is the answer.
  , (["S-<up>"],     "S-<up>",  "priority-up",                     Just "priorityUp",     "table",
       Just "cycle the priority of the marked rows, or the row at point")
  , (["S-<down>"],   "S-<down>", "priority-down",                  Just "priorityDown",   "table",
       Just "cycle the priority of the marked rows, or the row at point")
  , (["t"],          "t",       "org-glance-overview:todo",        Just "setState",       "table",
       Just "set the state of the marked rows, or the row at point")
  , (["C-c", "C-t"], "C-c C-t", "org-glance-overview:todo",        Just "setState",       "table",
       Just "the org spelling, where the browser lets it through")
  -- The one palette that stays up: managing tags is several ops where setting a state is one.
  , ([":"],          ":",       "org-agenda-set-tags",             Just "manageTags",     "table",
       Just "add or drop tags over the marked rows, or the row at point")
  -- Both of these survive the browser where @C-c C-t@ does not: they are page default actions.
  , (["C-c", "C-s"], "C-c C-s", "org-glance-overview:schedule",    Just "schedulePlan",   "table",
       planHelp)
  , (["C-c", "C-d"], "C-c C-d", "org-glance-overview:deadline",    Just "deadlinePlan",   "table",
       planHelp)
  -- Emacs's own name, since org-glance has no settings command and inventing one would put a name in this table no map carries.
  , ([","],          ",",       "customize",                       Just "openSettings",   "table",
       Just "the settings sheet: general, theme, keyword cycles")
  , (["@"],          "@",       "org-glance-material:refer",       Just "refer",          "modal",
       Just "link a headline into the prose; at a word boundary, so an address stays text")
  -- ONE COMMAND, TWO SURFACES, `@'-fashion: in the MATERIAL DOCUMENT the pair
  -- raises the date widget over the row's own slot.  Listed behind the table's
  -- own rows, so the resident key line still shows the command's FIRST row.
  , (["C-c", "C-s"], "C-c C-s", "org-glance-overview:schedule",    Just "scheduleHere",   "modal",
       hereHelp)
  , (["C-c", "C-d"], "C-c C-d", "org-glance-overview:deadline",    Just "deadlineHere",   "modal",
       hereHelp)
  , (["C-x", "C-s"], "C-x C-s", "save-buffer",                     Just "save",           "modal",
       Just "sync the sheet now; again to overwrite a conflict")
  , (["C-c", "C-c"], "C-c C-c", "org-ctrl-c-ctrl-c",               Just "commitEdit",     "modal",
       Just "commit the element being edited")
  , (["C-c", "'"],   "C-c '",   "org-edit-special",                Just "toggleRaw",      "modal",
       Just "the sheet as raw org, or as body and properties; sync an edited one first")
  -- THE WINDOW'S OWN SCOPE, and the only rows in it: live where a window stands
  -- behind the page and dead in a browser tab, which is the one way these three
  -- reach the browser's own zoom — `run' is past a `preventDefault'.
  , (["C-+"],        "C-+",     "text-scale-increase",             Just "textScaleIncrease", "window",
       zoomHelp)
  -- `+' wants the shift on most layouts, so the unshifted key is bound too — which is what a browser reads as zoom-in for the same reason.
  , (["C-="],        "C-=",     "text-scale-increase",             Just "textScaleIncrease", "window",
       zoomHelp)
  , (["C--"],        "C--",     "text-scale-decrease",             Just "textScaleDecrease", "window",
       zoomHelp)
  , (["C-0"],        "C-0",     "text-scale-set",                  Just "textScaleSet",      "window",
       Just "back to 100%")
  , (["ESC"],        "ESC",     "keyboard-quit",                   Just "cancel",         "any",
       Just "close the sheet, syncing an edited one; again to discard")
  ]
  where zoomHelp  = Just "the window's own zoom, a tenth of itself at a time"
        rightHelp = Just "the cell to the right; row movement keeps the column"
        leftHelp  = Just "the cell to the left; a whole row has none"
        topHelp   = Just "first row, again = page up"
        endHelp   = Just "last row, again = page down"
        planHelp  = Just "a date over the marked rows, or the row at point; empty clears it"
        hereHelp  = Just "a date in this row's own slot, resolved as you type; empty clears it"
        openHelp  = Just "open links: the row here, the element in the sheet; several list them"

blobOf :: T.Text -> IO Value
blobOf shell = keysOf shell >>= \raw ->
  either (\e -> assertFailure ("keymap JSON: " <> e)) pure
         (eitherDecode (BL.fromStrict (TE.encodeUtf8 raw)))

keysOf :: T.Text -> IO T.Text
keysOf shell = maybe (assertFailure "no keymap blob in the shell") pure
                     (between "<script id=\"keys\" type=\"application/json\">" "</script>" shell)

hintsOf :: T.Text -> IO [([T.Text], T.Text)]
hintsOf shell = traverse one =<< listAt "hints" =<< blobOf shell
  where one v = (,) <$> textsAt "commands" v <*> textAt "label" v

reservedOf :: T.Text -> IO [T.Text]
reservedOf shell = textsAt "reserved" =<< blobOf shell

onceOf :: T.Text -> IO [T.Text]
onceOf shell = textsAt "once" =<< blobOf shell

keymapOf :: T.Text -> IO [Row]
keymapOf shell = traverse row =<< listAt "rows" =<< blobOf shell
  where
    row v = (,,,,,) <$> textsAt "keys" v <*> textAt "seq" v <*> textAt "command" v
                    <*> maybeTextAt "handler" v <*> textAt "scope" v
                    <*> maybeTextAt "help" v

stripGlueComments :: T.Text -> T.Text
stripGlueComments =
  T.unlines . filter (not . T.isPrefixOf "//" . T.stripStart) . T.lines

-- | The identifiers at key position in the object literal @NAME = {@ opens,
-- shorthand members counted.  The walk steps over strings and trailing
-- comments, since either may carry an unbalanced brace, and counts parens
-- because a comma inside an argument list opens no member.
objectKeys :: T.Text -> T.Text -> [T.Text]
objectKeys name src = go (1 :: Int) (0 :: Int) True (T.drop 1 body) []
  where
    body = snd (T.breakOn "{" (snd (T.breakOn (name <> " = {") src)))
    go braces parens fresh t acc = case T.uncons t of
      Nothing -> reverse acc
      Just (c, rest)
        | c == '}' && braces == 1 -> reverse acc
        | c == '/', "/" `T.isPrefixOf` rest -> go braces parens fresh (T.dropWhile (/= '\n') rest) acc
        | c `elem` ("\"'`" :: String) -> go braces parens False (past c rest) acc
        | c == '}'                -> go (braces - 1) parens False rest acc
        | c == '{'                -> go (braces + 1) parens False rest acc
        | c == ')' || c == ']'    -> go braces (parens - 1) False rest acc
        | c == '(' || c == '['    -> go braces (parens + 1) False rest acc
        | c == ','                -> go braces parens (braces == 1 && parens == 0) rest acc
        | isSpace c               -> go braces parens fresh rest acc
        | fresh && braces == 1 && parens == 0 && (isAlpha c || c == '_') ->
            let (ident, after) = T.span (\x -> isAlphaNum x || x == '_') t
            in go braces parens False after (ident : acc)
        | otherwise               -> go braces parens False rest acc
    -- What follows the closing QUOTE, escapes honoured.
    past q t = case T.uncons t of
      Nothing            -> t
      Just ('\\', after) -> past q (T.drop 1 after)
      Just (c, after) | c == q    -> after
                      | otherwise -> past q after

glueOf :: T.Text -> IO T.Text
glueOf shell = do
  assertBool "the page names glue.js" ("src=\"glue.js\"" `T.isInfixOf` shell)
  stripGlueComments <$> glueSource

elmOf :: T.Text -> IO T.Text
elmOf shell = do
  assertBool "the page names elm.js" ("src=\"elm.js\"" `T.isInfixOf` shell)
  TIO.readFile "assets/elm.js"

glueSource :: IO T.Text
glueSource = T.concat <$> mapM (TIO.readFile . ("frontend/glue" </>)) gluePartFiles

cfgOf :: T.Text -> IO T.Text
cfgOf shell = maybe (assertFailure "no cfg blob in the shell") pure
                    (between "<script id=\"cfg\" type=\"application/json\">"
                             "</script>" shell)

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
      holdsAll "the one list" ["MAPS.rows.filter(live)", "MAPS.rows.find("] b

    -- The echo speaks the FUNCTION NAME, verbatim: a rebinding config addresses a command by exactly this string.
  , testCase "every echo names the command it ran, verbatim" $ do
      inline <- glueOf =<< shell
      let after = drop 1 (T.splitOn "${b.seq} → " inline)
          slots = [ T.takeWhile (/= '`') s | s <- after ]
      assertBool "no keyed echo at all — the sweep read nothing"
                 (not (null slots))
      assertEqual "an arrow slot that is not the command" []
                  [ s | s <- slots, not ("${b.command}" `T.isPrefixOf` s) ]
      -- The one echo written without a binding in hand names its command too, once for the four surfaces.
      assertContains "ESC's own echo"
                     "ESC → keyboard-quit (${what} unchanged)" inline
      rows <- keymapOf =<< shell
      assertEqual "a command name that cannot be typed as one" []
                  [ c | (_k, _s, c, _h, _sc, _help) <- rows, " " `T.isInfixOf` c ]

  , testCase "nothing is bound twice, and no sequence hides a longer one" $ do
      rows <- keymapOf =<< shell
      -- WITHIN A SCOPE.  `live' gates by scope, so one sequence may mean two
      -- things on two surfaces and never both at once: `@' READS the edges from
      -- the table and WRITES one from the sheet, which is org-glance's own
      -- split.  `any' is live everywhere, so it clashes with every scope.
      let bound = [ (k, sc) | (k, _, _, _, sc, _) <- rows ]
          shares a b = a == b || a == "any" || b == "any"
          twice = nub [ k | ((k, a), i) <- zip bound [0 :: Int ..]
                          , ((l, b), j) <- zip bound [0 ..]
                          , i < j, k == l, shares a b ]
          -- A complete sequence that also opens a longer one would match first and leave the longer unreachable.
          eaten = [ (k, l) | (k, a) <- bound, (l, b) <- bound
                           , k /= l, k == take (length k) l, shares a b ]
      assertEqual "bound twice in one scope" [] twice
      assertEqual "swallows a longer sequence" [] eaten
      -- The split is the point, so it is asserted rather than left to an absence.
      assertEqual "@ is the read on the table and the write on the sheet"
        [(["@"], "table"), (["@"], "modal")]
        [ (k, sc) | (k, _s, _c, _h, sc, _help) <- rows, k == ["@"] ]
      -- Two spellings of one command is the point, so the pairs are asserted rather than left to an absence.
      assertEqual "row movement has both spellings, the letter first"
        [["n"], ["j"], ["<down>"]]
        [ k | (k, _s, c, _h, _scope, _help) <- rows, c == "next-row" ]
      assertEqual "cell movement has all three, the letters first"
        [["f"], ["l"], ["<right>"]]
        [ k | (k, _s, c, _h, _scope, _help) <- rows, c == "next-column" ]

    -- THERE IS NO STATUS CORNER, asserted as an ABSENCE so the box cannot come back by another name.
  , testCase "the page has no status corner, and nothing focusable outside a popup" $ do
      b <- shell
      holdsNone "the shell"
        [ "id=\"corner\"", "#corner", "id=\"dot\"", "#dot", "dot(\"live\")"
        , "dot(\"down\")", "dot(\"wait\")", "id=\"gear\"", "#gear" ] b
      -- The page's own COLUMN — table, log, key line — is what the popups are not, and it holds nothing focusable.
      column <- maybe (assertFailure "no modal band in the shell") pure
                      (between "<body>" "<div id=\"modal\">" b)
      holdsNone "the page's column"
        ["<select", "<input", "<textarea", "<button", "<a "] column
      -- Both ends of the markup are swept, so a control can be added neither above the overlays nor below them.
      after <- maybe (assertFailure "no keymap blob in the shell") pure
                     (between "<div id=\"echo\"" "<script id=\"keys\"" b)
      holdsNone "under the popups"
        ["<select", "<input", "<textarea", "<button", "<a "] after
      sheet <- maybe (assertFailure "no settings sheet in the shell") pure
                     (between "<div id=\"config\">" "<div id=\"echo\"" b)
      holdsAll "the theme panel" ["id=\"ctheme\"", "id=\"themesel\""] sheet
      holdsNone "the shell" ["e.target.blur();"] b

    -- A `parts' id the markup does not carry throws at boot and takes the inline script with it, and the harness cannot see it.
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
      assertBool ("a heading survives in the shell: " <> show (between "<h1>" "</h1>" b))
                 (not ("<h1>" `T.isInfixOf` b))
      -- Written down rather than taken from the code: an oracle calling 'viewTitleFor' agrees with whatever it returns.
      assertEqual "the tab title" "test/fixtures/view — glance" (viewTitleFor viewDir)
      assertEqual "the title, once in the document" 1
                  (T.count "test/fixtures/view — glance" b)

  , testCase "the prefix keys are claimed only where they are ours" $ do
      b <- shell
      holdsAll "chord policy"
        -- A selection keeps C-c and C-x as copy and cut; the reserved chords reach the browser when they abandon a prefix.
        [ "if (!selecting()) { e.preventDefault();"
        , "if (MAPS.reserved.indexOf(k) === -1) e.preventDefault();" ] b
      reservedOf b >>= assertEqual "the chords never claimed on their own"
        ["C-l", "C-r", "C-t", "C-u", "C-w", "C-n", "C-p", "<f5>"]
      rows <- keymapOf b
      reserved <- reservedOf b
      assertEqual "a reserved chord is bound" []
        [ k | (k, _s, _c, _h, _scope, _help) <- rows, k `elem` map pure reserved ]

    -- REPEAT IS DERIVED, never just read: WebKitGTK's auto-repeat arrives with `repeat' unset.
  , keyedAt shell "?q=tag%3Awork%20state%3ATODO" 500
      "a held DEL strips one token, even when the event lies"
      "" "stuck:Backspace stuck:Backspace" $ \answer ->
        urlIs "one token gone, one standing" "?q=tag%3Awork" answer

  , keyed shell "a held d cannot flag and archive in one press"
      "" "stuck:d stuck:d" $ \answer -> do
        assertEqual "the row is flagged and nothing more" ["r1"]
          =<< textsAt "flagged" answer
        assertEqual "no archive went" [] =<< namesOf answer

  , keyedAt shell "?q=tag%3Awork%20state%3ATODO" 500
      "released and pressed again is two honest presses"
      "" "press:Backspace press:Backspace" $ \answer ->
        urlIs "both tokens gone" "?q=" answer

    -- A JOIN THE COMPILER CANNOT SEE: the keymap names a shell function and the
    -- shell answers by string equality, so a typo is bound, documented, drawn on
    -- the key line, echoed — and dead, printing the same M4 line a deliberately
    -- unhandled row prints.  Nothing else tells the two apart.
  , testCase "a binding names a handler the shell carries" $ do
      b <- shell
      rows <- keymapOf b
      handlers <- objectKeys "HANDLERS" <$> glueOf b
      assertBool ("the sweep found handlers: " <> show (length handlers))
                 (length handlers >= 20)
      assertEqual "bound to a handler the glue does not define" []
        [ h | (_k, _s, _c, Just h, _scope, _help) <- rows, h `notElem` handlers ]

  , testCase "the writes are the commands auto-repeat is off for" $ do
      b <- shell
      onceOf b >>= assertEqual "once" onceNames
      rows <- keymapOf b
      once <- onceOf b
      assertEqual "a command is on the once list and unbound" []
        [ c | c <- once, c `notElem` [ x | (_k, _s, x, _h, _scope, _help) <- rows ] ]

  , testCase "the inline glue is JavaScript, where there is a node to say so" $ do
      node <- findExecutable "node"
      case node of
        -- The syntax of the glue is checked wherever there is a node, and the skip SAYS so.
        Nothing  -> hPutStrLn stderr "\nSKIPPED - node is not on PATH: glue syntax"
        Just exe -> withTempDir $ \dir -> do
          inline <- glueOf =<< shell
          let path = dir </> "shell.js"
          TIO.writeFile path inline
          (code, _out, err) <- readProcessWithExitCode exe ["--check", path] ""
          assertEqual ("node --check said: " <> err) ExitSuccess code
  ]

-- | A LETTER IS A PHYSICAL KEY: @KeyA@..@KeyZ@ answer as the letter that key sits at, and everything else is @e.key@'s character.
layoutSpec :: IO T.Text -> TestTree
layoutSpec shell = testGroup "Shell layout"
  [ -- The complaint this answers: a reader with the Cyrillic layout up pressed
    -- `n' and the table sat there, `т' being no binding of anything.
    keyed shell "a Cyrillic press moves on the key the letter sits at"
      "т%KeyN т%KeyN" "" $ \answer -> do
        rowIs "two rows down" "r3" answer
        -- The pill speaks the BINDING's own spelling, which says the press resolved to the map.
        echoIs "under the map's own name for the key" "n → next-row" answer

  , keyed shell "and both movement dialects are the keys they sit at"
      "о%KeyJ о%KeyJ л%KeyK" "" $ \answer -> do
        rowIs "down twice on vim's pair, back up once" "r2" answer
        echoIs "the last press" "k → previous-row" answer

    -- SHIFT IS THE UPPERCASE BINDING rather than an `S-' modifier, which keeps `d' and `D' two rows.
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

    -- PUNCTUATION IS THE CHARACTER: there is no position to bind, `:' sitting on a different key per layout.
  , keyed shell "punctuation answers to the character, whatever key it sits on"
      "S-:%Digit6" "" $ \answer -> do
        assertEqual "the tag popup is up" "on" =<< textAt "tagpop" answer
        assertEqual "over the row at point" "tags · 1 row" =<< textAt "thead" answer

  , keyed shell "a chord completes on the physical key too" "C-c C-е%KeyT" "" $ \answer -> do
        assertEqual "the palette is up" "on" =<< textAt "prompt" answer
        assertEqual "resolved for the row the command names"
                    ["/keywords?ids=r1"] =<< textsAt "resolved" answer
        assertEqual "and neither chord was left to the browser"
                    ["C-c", "C-е%KeyT"] =<< textsAt "prevented" answer

  , keyed shell "a palette letter commits from a Cyrillic press"
      "t" "press:е%KeyT" $ \answer -> do
        assertEqual "one set-state over the row at point"
                    [("set-state", ["r1"])] =<< postedOf answer
        assertEqual "as the keyword that letter names" [Just "TODO"] =<< keywordsOf answer

    -- A FIELD KEEPS ITS CHARACTERS: the dispatch runs outside `typing()' and the fallback field claims arrows and RET alone.
  , keyed shell "a focused field is left the character it was sent"
      "t /" "press:т%KeyN" $ \answer -> do
        assertEqual "the palette is in its typing mode" "narrow" =<< textAt "pmode" answer
        assertEqual "nothing was committed" [] =<< postedOf answer
        rowIs "and the table under it never moved" "r1" answer
        assertBool "the key was left to the field"
          . notElem "т%KeyN" =<< textsAt "prevented" answer

  , keyed shell "a press carrying no code at all is the character it always was"
      "n j" "" $ rowIs "two rows down" "r3"

    -- The RAW event fields are read inside `keyName' and nowhere else, asserted as an absence over the glue with it cut out.
  , testCase "the raw event is read in one place, and every listener inherits it" $ do
      inline <- glueOf =<< shell
      named <- maybe (assertFailure "no keyName in the glue") pure
                     (between "function keyName(e) {" "\n    }" inline)
      -- `keyToken' is keyName's one sibling, keyed by the PHYSICAL key: a shift released mid-hold changes `e.key' and not the key.
      token <- maybe (assertFailure "no keyToken in the glue") pure
                     (between "const keyToken = (e) =>" ";" inline)
      holdsAll "the letter rule" ["const LETTER = /^Key([A-Z])$/;"] inline
      holdsAll "both halves of the split, in keyName" ["e.code", "e.key"] named
      holdsAll "and in the token, code first" ["e.code", "e.key"] token
      holdsNone "the glue outside the two"
        ["e.code", "e.key"] (T.replace token "" (T.replace named "" inline))
  ]

-- | What a coarse pointer gets: a touch device is the one place keys cannot reach.
touchSpec :: IO T.Text -> TestTree
touchSpec shell = testGroup "Touch"
  [ testCase "every page this server serves lays out at the device's own width" $ do
      withAssets <- shell
      bare <- body <$> get missingAssetsDir "/"
      -- Without it a phone lays the page out at 980px and scales it down.
      mapM_ (\(what, page') ->
               assertContains what
                 "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
                 page')
            [("the shell", withAssets), ("the JSON-only page", bare)]

  , testCase "a fine pointer sees none of it" $ do
      b <- shell
      let (before, coarse') = T.breakOn "@media (pointer:coarse){" b
      assertBool "no coarse block in the page" (not (T.null coarse'))
      -- EACH NEEDLE IS WITNESSED INSIDE THE BLOCK FIRST: an absence over a string the page cannot hold can never fail.
      mapM_ (\needle -> do
               assertBool ("the query does not carry it: " <> show needle)
                          (needle `T.isInfixOf` coarse')
               assertBool ("a touch rule outside the query: " <> show needle)
                          (not (needle `T.isInfixOf` before)))
            ["min-height:44px", ".ctext,.cview{font-size:16px}", "tv-chips:empty"]
      assertEqual "one coarse block, and one gate on it" 1
                  (T.count "@media (pointer:coarse){" b)
  ]

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

-- | The renderer the binary carries, which is what makes @--assets@ a development flag rather than the normal way to run.
embeddedSpec :: TestTree
embeddedSpec = testGroup "Embedded renderer"
  [ testCase "with no --assets, /table-view.js is the vendored file byte for byte" $ do
      r <- ok =<< getBuiltIn "/table-view.js"
      vendored <- BS.readFile vendoredRenderer
      assertEqual "the bytes `make sync-renderer' put in the tree"
                  vendored (BL.toStrict (simpleBody r))
      -- Big enough that a truncated or placeholder embed cannot pass by both sides being empty.
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
      assertContains "the shell's script" "src=\"glue.js\"" b
      g <- body <$> getBuiltIn "/glue.js"
      assertContains "mount" "TableView.mount(" g
      holdsNone "the JSON-only page" ["JSON-only mode"] b

  , testCase "--assets replaces the compiled-in renderer rather than adding to it" $ do
      r <- get assetsDir "/table-view.js"
      stub <- BS.readFile (assetsDir </> "table-view.js")
      vendored <- BS.readFile vendoredRenderer
      assertEqual "the directory's own file" stub (BL.toStrict (simpleBody r))
      assertBool "which is not the compiled-in one" (stub /= vendored)

  , testCase "with no --assets the renderer is the only asset there is" $ do
      -- Nothing else is compiled in: the font stays an `--assets' affordance and is not invented here.
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
