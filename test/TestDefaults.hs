-- | What more than one test module needs: fixture builders, the temp-directory
-- scaffolding the on-disk cases run in, the JSON accessors every wire assertion
-- reads a response through, and the one corpus gate.
--
-- The JSON accessors live here rather than beside their callers because four
-- modules were reading the same shapes four ways; each fails the test with what
-- it found instead, which is the whole reason not to use a partial pattern.
module TestDefaults ( assertContains
                    , assertParts
                    , at
                    , bare
                    , bareParse
                    , boolAt
                    , columnKeysOf
                    , columnOf
                    , compactTs
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
                    , membersAt
                    , on
                    , orgFile
                    , parsedIn
                    , plainTs
                    , presentSpans
                    , propertyKeys
                    , recordsOf
                    , spansOf
                    , systemFileIn
                    , tagFileIn
                    , tagsDirIn
                    , textAt
                    , textsAt
                    , titled
                    , viewDir
                    , withCategory
                    , withCorpusSample
                    , withDoc
                    , withDocDir
                    , withHeadline
                    , withHeadlineIn
                    , withId
                    , withTempDir
                    , withTempDirNamed
                    , withTodo
                    ) where

import Control.Exception (IOException, finally, throwIO, try)
import Data.Aeson (Value (Bool, Null, Number, Object, String), parseJSON)
import Data.Aeson.Types (parseEither)
import Data.List (sort)
-- 'headlinesOf' is hidden and spelled again below ON PURPOSE: the suite's copy
-- is an INDEPENDENT ORACLE for the span groups that read it, and one derived
-- from the library would agree with any change to it.
import Data.Org hiding (headlinesOf)
import Data.Org.Config (configDirIn, configPaths)
import Data.Org.Walk (findOrgFiles, foundFiles)
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import Data.Unique (hashUnique, newUnique)
import System.Directory ( createDirectory, doesDirectoryExist, getTemporaryDirectory
                        , removeDirectoryRecursive )
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isAlreadyExistsError)
import System.Posix.Process (getProcessID)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Glance.Query (HeadlineRecord, QueryResult (qrRecords), loadDir, loadFile)

-- Time

-- | The UTC time T spells as "%Y-%m-%d %H:%M:%S".
strptime :: Text -> UTCTime
strptime t = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack t)

-- | The moment T names, as a source that spelled a time of day.
at :: Text -> TsMoment
at t = TsMoment (strptime t) True

-- | The moment T names, as a date-only source.
on :: Text -> TsMoment
on t = TsMoment (strptime t) False

-- | A timestamp of STATUS at MOMENT: no repeater, no range end.
plainTs :: TimestampStatus -> TsMoment -> Timestamp
plainTs status moment = Timestamp { tsStatus = status
                                  , tsInterval = Nothing
                                  , tsStart = moment
                                  , tsEnd = Nothing
                                  , tsCompactRange = False }

-- | A compact same-day range of STATUS: START through END in one bracket pair.
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

-- | The suite's sample directory: one sample document and one that is not
-- UTF-8, six headlines between them.  The facade, the filter and the server all
-- answer questions about this one tree, so they all name it here.
viewDir :: FilePath
viewDir = "test/fixtures/view"

-- | A headline titled NAME carrying IDENT in an ORG_GLANCE_ID drawer.
withId :: Text -> Text -> Text
withId name' ident = T.intercalate "\n"
  [ "* " <> name'
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: " <> ident
  , ":END:" ]

-- | A whole entry: the title line TITLE, an indented drawer carrying IDENT, and
-- the newline that ends it — the shape the store's fixtures are files of.
entryAs :: Text -> Text -> Text
entryAs ident title' = T.unlines
  [ "* " <> title'
  , "  :PROPERTIES:"
  , "  :ORG_GLANCE_ID: " <> ident
  , "  :END:" ]

-- | 'entryAs' for the common fixture: a @TODO NAME@ entry whose id is NAME, so
-- a frame naming an id names the headline it came from.
entry :: Text -> Text
entry name' = entryAs name' ("TODO " <> name')

-- | A default headline whose title is the single token T.
titled :: Text -> Headline
titled t = defaultHeadline { title = Title [OrgLineToken (Token t)] }

-- Files

-- | Run ACT over a directory of its own, removed afterwards whatever happens.
-- A test whose subject is files on disk — the store, the watch, the write path
-- — writes real ones; the suite depends on @directory@ already and gains
-- nothing from a temp-file package for this.
--
-- LABEL names the directory, so one a killed run left behind says which group
-- made it.  The name also carries the process id and a per-process unique, and
-- the directory is created with 'createDirectory' rather than
-- 'createDirectoryIfMissing': a name already taken is retried under a fresh one
-- rather than shared.  Two suites running at once, or a stale directory from a
-- run that died before its @finally@, would otherwise write into each other's
-- fixtures and fail somewhere else entirely.
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

-- | 'withTempDirNamed' under the suite's own label.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withTempDirNamed "test"

-- | Write TEXT to DIR/NAME and yield the path.
orgFile :: FilePath -> FilePath -> Text -> IO FilePath
orgFile dir name text = path <$ TIO.writeFile path text
  where path = dir </> name

-- | PATH's text, decoded the way the loader decodes it.
document :: FilePath -> IO Text
document path = TE.decodeUtf8 <$> BS.readFile path

-- | PATH's records, or its load failure as a test failure.  The watcher's own
-- read.  A file that loads with no rows fails too: every caller uses the result
-- as an oracle, and an empty one makes the comparison it feeds vacuous.
recordsOf :: FilePath -> IO [HeadlineRecord]
recordsOf path = loadFile path >>= either (assertFailure . whyNot) rows
  where whyNot failure = "expected " <> path <> " to load, got " <> show failure
        rows [] = assertFailure ("expected " <> path <> " to load with rows, got none")
        rows rs = pure rs

-- | Run K over the records DOC alone loads to, written to NAME in a directory
-- of its own so the load path is the ordinary one.  LABEL names that directory,
-- so leftovers say which group made them.  A document that loads with no rows
-- reaches K as an empty list, which is an answer here rather than a failure.
withDoc :: String -> FilePath -> Text -> ([HeadlineRecord] -> IO a) -> IO a
withDoc label name doc k = withTempDirNamed label $ \dir -> do
  path <- orgFile dir name doc
  loadFile path >>= either (assertFailure . show) k

-- | 'withDoc' read through 'loadDir' rather than 'loadFile': the same one-file
-- tree, with the id resolution a directory load runs over it.
withDocDir :: String -> FilePath -> Text -> ([HeadlineRecord] -> IO a) -> IO a
withDocDir label name doc k = withTempDirNamed label $ \dir -> do
  _ <- orgFile dir name doc
  k . qrRecords =<< loadDir dir

-- Config layout
--
-- Read off 'configDirIn'/'configPaths' rather than spelled again, so a layout
-- the library moves takes every fixture with it.

-- | DIR's system layer.
systemFileIn :: FilePath -> FilePath
systemFileIn = fst . configPaths . configDirIn

-- | DIR's per-tag layer directory.
tagsDirIn :: FilePath -> FilePath
tagsDirIn = snd . configPaths . configDirIn

-- | DIR's layer for TAG.
tagFileIn :: FilePath -> FilePath -> FilePath
tagFileIn dir tag = tagsDirIn dir </> tag <> ".org"

-- Corpus

-- | Run K over a sample of the org files under @GLANCE_CORPUS@, and answer how
-- many things it checked.  LABEL names the check.
--
-- The gate is env-gated because the walk is what costs — seconds over a real
-- tree, against hundredths for the rest of the suite.  What it must not be is
-- silent: with the variable unset the group passed and read as green, so every
-- claim resting on it was unverified on any machine that had not set it.  It
-- now says on stderr that it was skipped, which is the cheapest form of the
-- truth.  A variable naming a directory that is not there stays a loud failure,
-- and a run that samples nothing is one too — a gate that found nothing to
-- check is not a gate that passed.
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
    -- The newline is so the line starts at a column of its own: tasty's own
    -- progress output is on stdout and the two share a terminal.
    Nothing   -> hPutStrLn stderr ("\nSKIPPED — GLANCE_CORPUS is unset: " <> label)
  where
    sample paths = every (max 1 (length paths `div` 64)) paths
    every n = map snd . filter ((== 0) . (`mod` n) . fst) . zip [0 :: Int ..]

-- Text assertions

-- | WHAT: NEEDLE is somewhere in HAYSTACK.
--
-- The haystack is truncated in the message: the served pages this reads are
-- tens of thousands of characters wide, and a failure that prints one buries
-- itself.  The cap is wide enough for a whole subtree, which is the other kind
-- of haystack here — cutting those at 400 hid the end of what a recompose
-- wrote, which is the half a failure is usually about.
assertContains :: String -> Text -> Text -> Assertion
assertContains what needle haystack =
  assertBool (what <> ": no " <> show needle <> " in " <> show (T.take 2000 haystack))
             (needle `T.isInfixOf` haystack)

-- | WHAT: every one of NEEDLES is in HAYSTACK.
holdsAll :: String -> [Text] -> Text -> Assertion
holdsAll what needles haystack = mapM_ (\n -> assertContains what n haystack) needles

-- | WHAT: none of NEEDLES is in HAYSTACK.  Each of them names a design the page
-- superseded, so one coming back means two are live at once.
holdsNone :: String -> [Text] -> Text -> Assertion
holdsNone what needles haystack =
  mapM_ (\n -> assertBool (what <> ": " <> show n <> " survives in the page")
                          (not (n `T.isInfixOf` haystack)))
        needles

-- JSON accessors, each failing the test with what it found instead

-- | V's value at KEY.
field :: Text -> Value -> IO Value
field k (Object o) = maybe (assertFailure ("missing key " <> show k))
                           pure (KM.lookup (Key.fromText k) o)
field k v = assertFailure ("expected an object with " <> show k <> ", got " <> show v)

-- | The string at KEY of V.
textAt :: Text -> Value -> IO Text
textAt k v = field k v >>= string
  where string (String t) = pure t
        string other = assertFailure ("expected a string at " <> show k
                                        <> ", got " <> show other)

-- | The number at KEY of V, as an offset.
intAt :: Text -> Value -> IO Int
intAt k v = field k v >>= number
  where number (Number n) = pure (round n)
        number other = assertFailure ("expected a number at " <> show k
                                        <> ", got " <> show other)

-- | The boolean at KEY of V.
boolAt :: Text -> Value -> IO Bool
boolAt k v = field k v >>= flag
  where flag (Bool b) = pure b
        flag other = assertFailure ("expected a boolean at " <> show k
                                      <> ", got " <> show other)

-- | The string at KEY of V, where the key is there and its value may be null.
maybeTextAt :: Text -> Value -> IO (Maybe Text)
maybeTextAt k v = field k v >>= string
  where string Null = pure Nothing
        string (String t) = pure (Just t)
        string other = assertFailure ("expected a string or null at " <> show k
                                        <> ", got " <> show other)

-- | The array at KEY of V.
listAt :: Text -> Value -> IO [Value]
listAt k v = field k v >>= read'
  where read' x = either (\e -> assertFailure ("array at " <> show k <> ": " <> e)) pure
                         (parseEither parseJSON x)

-- | The array of strings at KEY of V.
textsAt :: Text -> Value -> IO [Text]
textsAt k v = field k v >>= read'
  where read' x = either (\e -> assertFailure ("strings at " <> show k <> ": " <> e)) pure
                         (parseEither parseJSON x)

-- | The object at KEY of V, as its members by name.
membersAt :: Text -> Value -> IO [(Text, Value)]
membersAt k v = field k v >>= members
  where members (Object o) = pure [ (Key.toText name', x) | (name', x) <- KM.toList o ]
        members other = assertFailure ("expected an object at " <> show k
                                         <> ", got " <> show other)

-- | V's column keys, in view order.
columnKeysOf :: Value -> IO [Text]
columnKeysOf v = listAt "columns" v >>= mapM (textAt "key")

-- | The column of V keyed KEY.
columnOf :: Text -> Value -> IO Value
columnOf key v = do
  cols <- listAt "columns" v
  keys <- mapM (textAt "key") cols
  case [c | (c, k) <- zip cols keys, k == key] of
    [c] -> pure c
    cs  -> assertFailure ("expected one " <> show key <> " column, got " <> show (length cs))

-- Parsing

-- | Drop the spans ELEMS carry, for span-insensitive comparison.
bare :: [Spanned Element] -> [Element]
bare = map (stripSpans . valueOf)

-- | The elements INPUT parses to in CTX, spans dropped.
bareParse :: Context -> Text -> [Element]
bareParse ctx input = case orgParse ctx input of
  (elems, _ctx, _err) -> bare elems

-- | The headlines among ELEMS, in source order.
headlinesOf :: [Spanned Element] -> [Headline]
headlinesOf elems = [h | e <- elems, EHeadline h <- [valueOf e]]

-- | Run K on the one headline INPUT parses to in CTX; fail otherwise.
withHeadlineIn :: Context -> Text -> (Headline -> Assertion) -> Assertion
withHeadlineIn ctx input k = case bareParse ctx input of
  [EHeadline h] -> k h
  es -> assertFailure ("expected one headline in " <> show input <> ", got: " <> show es)

-- | 'withHeadlineIn', starting from 'defaultContext'.
withHeadline :: Text -> (Headline -> Assertion) -> Assertion
withHeadline = withHeadlineIn defaultContext

-- | The elements and final context INPUT parses to; a parse error is LABEL's
-- failure.
parsedIn :: String -> Text -> IO ([Spanned Element], Context)
parsedIn label input = case orgParse defaultContext input of
  (elems, ctx, Nothing) -> pure (elems, ctx)
  (_, _, Just _err)     -> assertFailure (label <> ": parse error in " <> show input)

-- Headline spans

-- | The sub-spans H actually carries, in source order.
presentSpans :: Headline -> [(String, Span)]
presentSpans h = [(T.unpack label, s) | (label, Just s, _ok) <- headlineSpanParts h]

-- | H's labelled spans: 'hsFull' and every sub-span it carries.
spansOf :: Headline -> [(String, Span)]
spansOf h = ("hsFull", hsFull (spans h)) : presentSpans h

-- | The property keys H declares, in source order.
propertyKeys :: Headline -> [Text]
propertyKeys h = case properties h of
  Properties ps -> [k | Property (Keyword k) _ <- ps]

-- | Every sub-span H carries slices back to the component it stands for.  SAY
-- decorates the failure, the two callers naming a headline two ways.
assertParts :: (String -> String) -> Text -> Headline -> Assertion
assertParts say doc h = sequence_
  [ assertBool (say (T.unpack part <> " sliced " <> show slice)) (ok slice)
  | (part, Just sp, ok) <- headlineSpanParts h
  , let slice = sliceSpan doc sp ]
