-- | What more than one test module needs: fixture builders, the temp-directory scaffolding the on-disk cases run in, the JSON accessors every wire assertion reads a response through, and the one corpus gate.
module TestDefaults ( assertContains
                    , assertParts
                    , at
                    , bare
                    , bareParse
                    , boolAt
                    , buildSources
                    , columnKeysOf
                    , columnOf
                    , committable
                    , compactTs
                    , digestOnDisk
                    , document
                    , entry
                    , entryAs
                    , field
                    , headlinesOf
                    , holdsAll
                    , holdsNone
                    , initialState
                    , intAt
                    , listAt
                    , maybeTextAt
                    , namesIn
                    , refusedNaming
                    , sparseTextAt
                    , sparseAt
                    , on
                    , orgFile
                    , parsedIn
                    , plainTs
                    , presentSpans
                    , proposalDirs
                    , proposalsByStatus
                    , propertyKeys
                    , recordsOf
                    , rewrite
                    , sourcesUnder
                    , spansOf
                    , systemFileIn
                    , tagFileIn
                    , tagsDirIn
                    , testProperty
                    , testPropertyWith
                    , writeLayers
                    , textAt
                    , valueAfter
                    , viewText
                    , textsAt
                    , titled
                    , viewDir
                    , withCategory
                    , withCorpusSample
                    , withDoc
                    , withDocDir
                    , withGlanceBinary
                    , withHeadline
                    , withHeadlineIn
                    , withId
                    , withStoreOf
                    , withTempDir
                    , withTempDirNamed
                    , withTodo
                    ) where

import Control.Monad (filterM, forM)
import Control.Exception (IOException, finally, throwIO, try)
import Data.Aeson (Value (Bool, Null, Number, Object, String), parseJSON)
import Data.Aeson.Types (parseEither)
import Data.List (isPrefixOf, sort)
import Data.Maybe (fromMaybe, listToMaybe)
-- 'headlinesOf' is hidden and spelled again below ON PURPOSE: the suite's copy is an INDEPENDENT ORACLE for the span groups that read it.
import Data.Org hiding (headlinesOf)
import Data.Org.Config (configDirIn, configPaths)
import Data.Org.Walk (findOrgFiles, foundFiles)
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Data.Unique (hashUnique, newUnique)
import System.Directory ( createDirectory, createDirectoryIfMissing, doesDirectoryExist
                        , doesFileExist, getTemporaryDirectory, listDirectory
                        , removeDirectoryRecursive )
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isAlreadyExistsError)
import System.Posix.Process (getProcessID)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)
import Test.QuickCheck.Random (mkQCGen)
import Text.Read (readMaybe)

import qualified Test.QuickCheck as QC

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Glance.Query (HeadlineRecord, QueryResult (qrRecords), digestOfText, loadDir, loadFile)
import Glance.Web.Store (Frame, Store, applyFile, loadStore)

-- Time

strptime :: Text -> UTCTime
strptime t = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack t)

-- | The moment T names, as a source that spelled a time of day.
at :: Text -> TsMoment
at t = TsMoment (strptime t) True

-- | The moment T names, as a date-only source.
on :: Text -> TsMoment
on t = TsMoment (strptime t) False

plainTs :: TimestampStatus -> TsMoment -> Timestamp
plainTs status moment = Timestamp { tsStatus = status
                                  , tsInterval = Nothing
                                  , tsWarning = Nothing
                                  , tsStart = moment
                                  , tsEnd = Nothing
                                  , tsCompactRange = False }

compactTs :: TimestampStatus -> TsMoment -> TsMoment -> Timestamp
compactTs status start end = (plainTs status start) { tsEnd = Just end
                                                    , tsCompactRange = True }

-- Context

initialState :: Context
initialState = defaultContext

withCategory :: Context -> Text -> Context
withCategory ctx category = setCategory category ctx

withTodo :: Context -> ([Text], [Text]) -> Context
withTodo ctx (actives, inactives) =
  setTodo (Set.fromList actives) (Set.fromList inactives) ctx

-- Documents

-- | The suite's sample directory, named here because the facade, the filter and the server all answer questions about this one tree.
viewDir :: FilePath
viewDir = "test/fixtures/view"

withId :: Text -> Text -> Text
withId name' ident = T.intercalate "\n"
  [ "* " <> name'
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: " <> ident
  , ":END:" ]

entryAs :: Text -> Text -> Text
entryAs ident title' = T.unlines
  [ "* " <> title'
  , "  :PROPERTIES:"
  , "  :ORG_GLANCE_ID: " <> ident
  , "  :END:" ]

-- | 'entryAs' for the common fixture: a @TODO NAME@ entry whose id is NAME, so a frame naming an id names the headline it came from.
entry :: Text -> Text
entry name' = entryAs name' ("TODO " <> name')

titled :: Text -> Headline
titled t = defaultHeadline { title = Title [OrgLineToken (Token t)] }

-- | A file whose first headline is materialized, edited and written back.
committable :: Text
committable = T.unlines
  [ "#+CATEGORY: notes"
  , "* TODO First :one:"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: first"
  , ":END:"
  , "body of first"
  , "* TODO Second"
  , "tail" ]

-- Files

-- | Run ACT over a directory of its own, removed afterwards whatever happens.  LABEL, the process id and a unique name it; 'createDirectory' retries a name already taken, so two runs never share one.
withTempDirNamed :: String -> (FilePath -> IO a) -> IO a
withTempDirNamed label act = do
  base <- getTemporaryDirectory
  pid <- getProcessID
  dir <- create base pid (16 :: Int)
  act dir `finally` removeDirectoryRecursive dir
  where
    create base pid tries = do
      unique <- hashUnique <$> newUnique
      let dir = base </> ("glance-" <> label <> "-" <> show pid <> "-" <> show unique)
      made <- try (createDirectory dir)
      case made of
        Right ()                                    -> pure dir
        Left e | isAlreadyExistsError e, tries > 0  -> create base pid (tries - 1)
               | otherwise                          -> throwIO (e :: IOException)

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withTempDirNamed "test"

orgFile :: FilePath -> FilePath -> Text -> IO FilePath
orgFile dir name text = path <$ TIO.writeFile path text
  where path = dir </> name

document :: FilePath -> IO Text
document path = TE.decodeUtf8 <$> BS.readFile path

-- | PATH's records, or its load failure as a test failure.  A file that loads with no rows fails too: an empty oracle makes the comparison it feeds vacuous.
recordsOf :: FilePath -> IO [HeadlineRecord]
recordsOf path = loadFile path >>= either (assertFailure . whyNot) rows
  where whyNot failure = "expected " <> path <> " to load, got " <> show failure
        rows [] = assertFailure ("expected " <> path <> " to load with rows, got none")
        rows rs = pure rs

-- | Run K over the records DOC alone loads to, written to NAME in LABEL's own directory.  A document that loads with no rows reaches K as an empty list.
withDoc :: String -> FilePath -> Text -> ([HeadlineRecord] -> IO a) -> IO a
withDoc label name doc k = withTempDirNamed label $ \dir -> do
  path <- orgFile dir name doc
  loadFile path >>= either (assertFailure . show) k

-- | 'withDoc' read through 'loadDir': the same one-file tree, with the id resolution a directory load runs over it.
withDocDir :: String -> FilePath -> Text -> ([HeadlineRecord] -> IO a) -> IO a
withDocDir label name doc k = withTempDirNamed label $ \dir -> do
  _ <- orgFile dir name doc
  k . qrRecords =<< loadDir dir

-- The live store

-- | Run K over a store loaded from a directory holding FILES, handed the directory, the FIRST path and the store.
withStoreOf :: [(FilePath, Text)] -> (FilePath -> FilePath -> Store -> IO a) -> IO a
withStoreOf files k = withTempDir $ \dir -> do
  paths <- mapM (uncurry (orgFile dir)) files
  case paths of
    path : _ -> k dir path =<< loadStore dir
    []       -> assertFailure "withStoreOf: a store of no files says nothing"

-- | Rewrite PATH with TEXT and fold the re-read into STORE: the watch's whole step.
rewrite :: FilePath -> Text -> Store -> IO (Store, [Frame])
rewrite path text store = do
  TIO.writeFile path text
  applyFile path <$> loadFile path <*> pure store

-- Config layout, read off 'configDirIn'/'configPaths', so a layout the library moves takes every fixture with it.

systemFileIn :: FilePath -> FilePath
systemFileIn = fst . configPaths . configDirIn

tagsDirIn :: FilePath -> FilePath
tagsDirIn = snd . configPaths . configDirIn

tagFileIn :: FilePath -> FilePath -> FilePath
tagFileIn dir tag = tagsDirIn dir </> tag <> ".org"

writeLayers :: FilePath -> [(Maybe FilePath, Text)] -> IO ()
writeLayers dir layers = do
  createDirectoryIfMissing True (tagsDirIn dir)
  mapM_ (\(tag, text) -> TIO.writeFile (maybe (systemFileIn dir) (tagFileIn dir) tag) text)
        layers

-- Corpus

-- | Run K over a sample of the org files under @GLANCE_CORPUS@, and answer how many things it checked.  An unset variable SAYS SO on stderr; a variable naming a missing directory, and a run that samples nothing, are loud failures.
withCorpusSample :: String -> ([FilePath] -> IO Int) -> Assertion
withCorpusSample label k = do
  asked <- lookupEnv "GLANCE_CORPUS"
  there <- maybe (pure False) doesDirectoryExist asked
  case asked of
    Just root | there -> do
      found <- findOrgFiles [root]
      checked <- k (sample (sort (foundFiles found)))
      assertBool (label <> ": nothing sampled under " <> root) (checked > 0)
    Just root -> assertFailure ("GLANCE_CORPUS names no directory: " <> root)
    -- The newline starts the line at a column of its own: tasty's progress output is on stdout and the two share a terminal.
    Nothing   -> hPutStrLn stderr ("\nSKIPPED — GLANCE_CORPUS is unset: " <> label)
  where
    sample paths = every (max 1 (length paths `div` 64)) paths
    every n = map snd . filter ((== 0) . (`mod` n) . fst) . zip [0 :: Int ..]

-- Properties.  No @tasty-quickcheck@: it is the one package this suite would have had to FETCH.

-- | The seed every property run starts from unless @GLANCE_QC_SEED@ names another.  FIXED, so a red run replays from the commit alone.
propertySeed :: Int
propertySeed = 20260811

-- | Run PROP as one HUnit case over COUNT cases.  A failure prints the seed, the reason and the SHRUNK counterexample.
testPropertyWith :: QC.Testable p => Int -> String -> p -> TestTree
testPropertyWith count name p = testCase name $ do
  seed <- maybe (pure propertySeed) readSeed =<< lookupEnv "GLANCE_QC_SEED"
  outcome <- QC.quickCheckWithResult (args seed) p
  case outcome of
    QC.Success{} -> pure ()
    QC.Failure{ QC.reason = why, QC.failingTestCase = shrunk } ->
      assertFailure (unlines (replay seed : why : shrunk))
    other -> assertFailure (replay seed <> "\n" <> QC.output other)
  where
    args seed = QC.stdArgs { QC.replay = Just (mkQCGen seed, 0)
                           , QC.maxSuccess = count
                           , QC.maxShrinks = 400
                           , QC.chatty = False }
    replay seed = "replay: GLANCE_QC_SEED=" <> show seed
    readSeed asked = maybe (assertFailure ("GLANCE_QC_SEED is not a number: " <> asked))
                           pure (readMaybe asked)

testProperty :: QC.Testable p => String -> p -> TestTree
testProperty = testPropertyWith 200

-- Text assertions

-- | WHAT: NEEDLE is somewhere in HAYSTACK.  The haystack is truncated in the message, a served page being tens of thousands of characters wide.
assertContains :: String -> Text -> Text -> Assertion
assertContains what needle haystack =
  assertBool (what <> ": no " <> show needle <> " in " <> show (T.take 2000 haystack))
             (needle `T.isInfixOf` haystack)

holdsAll :: String -> [Text] -> Text -> Assertion
holdsAll what needles haystack = mapM_ (\n -> assertContains what n haystack) needles

-- | WHAT: none of NEEDLES is in HAYSTACK.  Each of them names a design the page superseded, so one coming back means two are live at once.
holdsNone :: String -> [Text] -> Text -> Assertion
holdsNone what needles haystack =
  mapM_ (\n -> assertBool (what <> ": " <> show n <> " survives in the page")
                          (not (n `T.isInfixOf` haystack)))
        needles

-- JSON accessors, each failing the test with what it found instead

field :: Text -> Value -> IO Value
field k (Object o) = maybe (assertFailure ("missing key " <> show k))
                           pure (KM.lookup (Key.fromText k) o)
field k v = assertFailure ("expected an object with " <> show k <> ", got " <> show v)

textAt :: Text -> Value -> IO Text
textAt k v = field k v >>= string
  where string (String t) = pure t
        string other = assertFailure ("expected a string at " <> show k
                                        <> ", got " <> show other)

viewText :: Text -> Value -> IO Text
viewText vid v = do
  entries <- listAt "views" v
  named <- filterM (fmap (== vid) . textAt "id") entries
  case named of
    (e:_) -> textAt "query" e
    []    -> assertFailure ("no view called " <> show vid <> " in " <> show v)

intAt :: Text -> Value -> IO Int
intAt k v = field k v >>= number
  where number (Number n) = pure (round n)
        number other = assertFailure ("expected a number at " <> show k
                                        <> ", got " <> show other)

boolAt :: Text -> Value -> IO Bool
boolAt k v = field k v >>= flag
  where flag (Bool b) = pure b
        flag other = assertFailure ("expected a boolean at " <> show k
                                      <> ", got " <> show other)

-- | V's value at KEY, 'Nothing' where V carries no such key at all — the accessor for a SPARSE field, where 'field' fails on the absence and 'maybeTextAt' reads a null.
sparseAt :: Text -> Value -> IO (Maybe Value)
sparseAt k (Object o) = pure (KM.lookup (Key.fromText k) o)
sparseAt k v = assertFailure ("expected an object with " <> show k <> ", got " <> show v)

maybeTextAt :: Text -> Value -> IO (Maybe Text)
maybeTextAt k v = field k v >>= stringOrNull k

-- | The string at KEY of V, 'Nothing' where the key is ABSENT as well as where it is null.
sparseTextAt :: Text -> Value -> IO (Maybe Text)
sparseTextAt k v = maybe (pure Nothing) (stringOrNull k) =<< sparseAt k v

stringOrNull :: Text -> Value -> IO (Maybe Text)
stringOrNull _k Null = pure Nothing
stringOrNull _k (String t) = pure (Just t)
stringOrNull k other = assertFailure ("expected a string or null at " <> show k
                                        <> ", got " <> show other)

refusedNaming :: Show a => String -> [Text] -> Either Text a -> Assertion
refusedNaming what words' outcome = case outcome of
  Right landed -> assertFailure (what <> ": was read as " <> show landed)
  Left why     -> mapM_ (\w -> assertBool (what <> ": " <> T.unpack why)
                                          (w `T.isInfixOf` why)) words'

listAt :: Text -> Value -> IO [Value]
listAt k v = field k v >>= read'
  where read' x = either (\e -> assertFailure ("array at " <> show k <> ": " <> e)) pure
                         (parseEither parseJSON x)

-- | The SHA-256 of the bytes PATH holds now, taken off the FILE so the oracle agrees with a receipt whatever the loader digested.
digestOnDisk :: FilePath -> IO Text
digestOnDisk path = digestOfText <$> document path

textsAt :: Text -> Value -> IO [Text]
textsAt k v = field k v >>= read'
  where read' x = either (\e -> assertFailure ("strings at " <> show k <> ": " <> e)) pure
                         (parseEither parseJSON x)

columnKeysOf :: Value -> IO [Text]
columnKeysOf v = listAt "columns" v >>= mapM (textAt "key")

columnOf :: Text -> Value -> IO Value
columnOf key v = do
  cols <- listAt "columns" v
  keys <- mapM (textAt "key") cols
  case [c | (c, k) <- zip cols keys, k == key] of
    [c] -> pure c
    cs  -> assertFailure ("expected one " <> show key <> " column, got " <> show (length cs))

-- Parsing

bare :: [Spanned Element] -> [Element]
bare = map (stripSpans . valueOf)

bareParse :: Context -> Text -> [Element]
bareParse ctx input = case orgParse ctx input of
  (elems, _ctx, _err) -> bare elems

headlinesOf :: [Spanned Element] -> [Headline]
headlinesOf elems = [h | e <- elems, EHeadline h <- [valueOf e]]

withHeadlineIn :: Context -> Text -> (Headline -> Assertion) -> Assertion
withHeadlineIn ctx input k = case bareParse ctx input of
  [EHeadline h] -> k h
  es -> assertFailure ("expected one headline in " <> show input <> ", got: " <> show es)

withHeadline :: Text -> (Headline -> Assertion) -> Assertion
withHeadline = withHeadlineIn defaultContext

parsedIn :: String -> Text -> IO ([Spanned Element], Context)
parsedIn label input = case orgParse defaultContext input of
  (elems, ctx, Nothing) -> pure (elems, ctx)
  (_, _, Just _err)     -> assertFailure (label <> ": parse error in " <> show input)

-- Headline spans

presentSpans :: Headline -> [(String, Span)]
presentSpans h = [(T.unpack label, s) | (label, Just s, _ok) <- headlineSpanParts h]

spansOf :: Headline -> [(String, Span)]
spansOf h = ("hsFull", hsFull (spans h)) : presentSpans h

propertyKeys :: Headline -> [Text]
propertyKeys h = case properties h of
  Properties ps -> [k | Property (Keyword k) _ <- ps]

assertParts :: (String -> String) -> Text -> Headline -> Assertion
assertParts say doc h = sequence_
  [ assertBool (say (T.unpack part <> " sliced " <> show slice)) (ok slice)
  | (part, Just sp, ok) <- headlineSpanParts h
  , let slice = sliceSpan doc sp ]

-- Sources.  The sweeps 'TestSpec' and 'TestSelfContained' both run over the tree.

-- | Every @.hs@ file under DIR, at any depth.
sourcesUnder :: FilePath -> IO [FilePath]
sourcesUnder dir = do
  entries <- map (dir </>) <$> listDirectory dir
  files <- filterM doesFileExist entries
  nested <- mapM sourcesUnder =<< filterM doesDirectoryExist entries
  pure (filter ((== ".hs") . takeExtension) files <> concat nested)

-- | Every Haskell file this package builds from.  The vendored GTK bindings are out: upstream's, and unbuilt unless @-f native-window@ is.
buildSources :: IO [FilePath]
buildSources =
  concat <$> mapM sourcesUnder ["src", "src-query", "src-web", "src-desktop-native", "app"]

-- | The lines of PATH carrying NEEDLE, each with its file and number.
namesIn :: Text -> FilePath -> IO [String]
namesIn needle path = report . T.lines <$> TIO.readFile path
  where
    report ls = [ path <> ":" <> show n <> ": " <> T.unpack (T.strip l)
                | (n, l) <- zip [1 :: Int ..] ls, needle `T.isInfixOf` l ]

-- The built CLI

-- | @$GLANCE_BIN@, else the binary cabal's default layout holds for the version
-- being built.  'Nothing' is a SKIP the caller must say out loud: a
-- @--builddir@ run finds none, and a case that quietly passes without the
-- binary has asserted nothing.
glanceBinary :: IO (Maybe FilePath)
glanceBinary = do
  named <- lookupEnv "GLANCE_BIN"
  case named of
    Just path -> pure (Just path)
    -- THE VERSION IS A STEP OF THE PATH and a tree keeps every version it has
    -- ever built, so a bare @*@ there hands back whichever cabal made first —
    -- for weeks, a binary four cuts old.
    Nothing   -> do
      v <- cabalVersion
      listToMaybe <$> globPath "dist-newstyle/build"
        ["*", "*", "glance-" <> T.unpack v, "x", "glance", "build", "glance", "glance"]

-- | K over the built binary, or a skip naming WHAT went unasserted.
withGlanceBinary :: String -> (FilePath -> Assertion) -> Assertion
withGlanceBinary what k = glanceBinary >>=
  maybe (hPutStrLn stderr ("\nSKIPPED - no glance binary built: " <> what)) k

-- | The version @glance.cabal@ names, empty when there is none to read.
cabalVersion :: IO Text
cabalVersion = fromMaybe "" . valueAfter "version:" <$> TIO.readFile "glance.cabal"

-- | The value NAME leads on the first line carrying it, stripped.
valueAfter :: Text -> Text -> Maybe Text
valueAfter name body = listToMaybe
  [ T.strip rest
  | l <- T.lines body, Just rest <- [T.stripPrefix name (T.stripStart l)] ]

-- | The paths under ROOT matching STEPS, @*@ any one directory — cabal's layout.
globPath :: FilePath -> [String] -> IO [FilePath]
globPath root [] = do
  there <- doesFileExist root
  pure [ root | there ]
globPath root ("*" : rest) = do
  isDir <- doesDirectoryExist root
  if not isDir then pure [] else do
    entries <- listDirectory root
    concat <$> mapM (\e -> globPath (root </> e) rest) entries
globPath root (step : rest) = globPath (root </> step) rest


-- | The status directories under @docs/proposals@, sidecars dropped.
proposalDirs :: IO [FilePath]
proposalDirs = filter (not . proposalSidecar) <$> listDirectory "docs/proposals"

-- | Every proposal, as (status directory, file name).
proposalsByStatus :: IO [(FilePath, FilePath)]
proposalsByStatus = do
  statuses <- proposalDirs
  fmap concat . forM statuses $ \st ->
    map ((,) st) . filter (not . proposalSidecar)
      <$> listDirectory ("docs/proposals" </> st)

-- | An editor's lock or autosave beside the proposals.
proposalSidecar :: FilePath -> Bool
proposalSidecar n = (".#" `isPrefixOf` n) || ("#" `isPrefixOf` n && "#" `isPrefixOf` reverse n)
