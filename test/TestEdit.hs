module TestEdit (spec) where

import Control.Exception (IOException, try)
import Data.Bits ((.&.))
import Data.List (sort, sortOn)
-- The suite's own 'headlinesOf' is the oracle here; see 'TestDefaults'.
import Data.Org hiding (headlinesOf)
import Data.Org.Edit
import Data.Text (Text)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
import System.Posix.Types (FileMode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults ( assertParts, headlinesOf, parsedIn, spansOf, withCorpusSample
                    , withTempDirNamed )

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- Fixtures

-- | The replacement every span-exactness check splices in: absent from org
-- syntax, and a different length from anything it replaces, so a splice that
-- shifts by the wrong amount cannot come out right by accident.
marker :: Text
marker = "⟦MARK⟧"

plannedDoc :: Text
plannedDoc = T.intercalate "\n"
  [ "#+TODO: TODO NEXT | DONE CANCELLED"
  , "* TODO [#A] Write the engine :work:eng:"
  , "SCHEDULED: <2024-01-15 Mon 10:30> DEADLINE: <2024-01-20 Sat>"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: engine-1"
  , ":END:"
  , "body text here"
  , "** NEXT Привет мир :тег:"
  , "* DONE Old thing"
  ]

outOfOrderDoc :: Text
outOfOrderDoc = T.intercalate "\n"
  [ "* TODO Task :x:"
  , "CLOSED: [2024-01-02 Tue 10:00] DEADLINE: <2024-01-03 Wed> SCHEDULED: <2024-01-01 Mon>"
  , "** DONE [#C] Second :y:z:"
  ]

drawerDoc :: Text
drawerDoc = T.intercalate "\n"
  [ "* Task"
  , "DEADLINE: <2024-02-01 Thu>"
  , ":PROPERTIES:"
  , ":K: v"
  , ":END:"
  , "* Задача :работа:"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: ид-1"
  , ":END:"
  ]

fixtures :: [(String, Text)]
fixtures = [ ("planning, drawer and unicode", plannedDoc)
           , ("planning keywords out of order", outOfOrderDoc)
           , ("drawers and a unicode title", drawerDoc)
           ]

-- | Documents the generated multi-edit cases run over: ascii, two scripts of
-- unicode, and one carrying newlines and a drawer.
randomDocs :: [(String, Text)]
randomDocs = [ ("ascii", "the quick brown fox jumps over the lazy dog")
             , ("unicode", "Привет мир, こんにちは мир")
             , ("lines", plannedDoc)
             ]

-- Helpers

-- | Fail with LABEL when E is a 'Left', else yield its value.
expectRight :: Show e => String -> Either e a -> IO a
expectRight label = either (\e -> assertFailure (label <> ": " <> show e)) pure

-- | The elements INPUT parses to, failing when it does not parse.
parsed :: String -> Text -> IO [Spanned Element]
parsed label = fmap fst . parsedIn label

-- | The span of the first @TODO@ keyword in DOC.  Read out of the parse rather
-- than written down: an offset typed into a test is one the fixture can drift
-- away from.
todoSpan :: Text -> IO Span
todoSpan doc = do
  elems <- parsed "fixture" doc
  case [ s | h <- headlinesOf elems
           , Just s <- [hsTodo (spans h)]
           , sliceSpan doc s == "TODO" ] of
    (s:_) -> pure s
    []    -> assertFailure "the fixture carries no TODO keyword"

-- | The exactly-the-span property: replacing SP with 'marker' changes SP and
-- nothing else.  Every character ahead of the span survives at its own offset,
-- every character past it survives shifted by the length difference, and the
-- spliced region is the marker itself.  This is S8's surgical exit bar stated
-- on the text, ahead of any command that will have to meet it as a one-hunk
-- diff.
assertExactSplice :: String -> Text -> Span -> Assertion
assertExactSplice label doc sp = do
  out <- expectRight label (applyEdits doc [Edit sp marker])
  assertEqual (label <> ": prefix") (T.take s doc) (T.take s out)
  assertEqual (label <> ": spliced region") marker (T.take (T.length marker) (T.drop s out))
  assertEqual (label <> ": suffix") (T.drop e doc) (T.drop (s + T.length marker) out)
  assertEqual (label <> ": length") (T.length doc - (e - s) + T.length marker) (T.length out)
  where s = spanStart sp
        e = spanEnd sp

-- | Apply EDITS one at a time, the last of them first — 'foldr' over the sorted
-- list runs right to left — so the offsets ahead of each are still the original
-- document's.  Quadratic and obviously correct: the oracle 'applyEdits' is
-- checked against.
referenceApply :: Text -> [Edit] -> Text
referenceApply doc edits = foldr (flip splice1) doc (sortOn key edits)
  where key e = (spanStart (editSpan e), spanEnd (editSpan e))
        splice1 t (Edit (Span s e) new) = T.take s t <> new <> T.drop e t

-- | A linear congruential generator (Numerical Recipes' constants): the same
-- cases on every run, without a property-testing dependency.
lcg :: Int -> Int
lcg s = (1664525 * s + 1013904223) `mod` 2147483648

-- | Non-overlapping edits over a document of LEN characters, drawn from SEED.
-- Gaps of zero produce touching edits and widths of zero produce insertions,
-- both of which are legal and both of which the oracle has to agree on.
randomEdits :: Int -> Int -> [Edit]
randomEdits seed len = go (lcg seed) 0
  where
    go s at
      | start >= len = []
      | otherwise    = Edit (Span start end) new : go (lcg s'') end
      where start = at + s `mod` 5
            s'    = lcg s
            end   = min len (start + s' `mod` 4)
            s''   = lcg s'
            new   = replacements !! (s'' `mod` length replacements)
    replacements = ["", "X", "мир, ", "a much longer replacement", "\n"]

-- | Put DOC at PATH as UTF-8, whatever the locale says.
writeDoc :: FilePath -> Text -> IO ()
writeDoc path = BS.writeFile path . TE.encodeUtf8

-- | Assert PATH holds exactly BYTES.
assertBytes :: String -> BS.ByteString -> FilePath -> Assertion
assertBytes label want path = do
  found <- BS.readFile path
  assertEqual (label <> ": bytes at " <> path) want found

-- Spec

spec :: TestTree
spec = testGroup "Edit"
  [ testGroup "Single splice" (map spliceCase spliceCases)
  , testGroup "Validation" validationSpec
  , testGroup "Multi-edit" multiEditSpec
  , testGroup "Exactly the span"
      [ testCase name (mapM_ (headlineSplices name doc) . headlinesOf =<< parsed name doc)
      | (name, doc) <- fixtures ]
  , testGroup "Round-trip with the parser"
      [ toggleCase "same-length keyword" ("DONE", False)
      , toggleCase "longer keyword"      ("CANCELLED", False)
      ]
  , testGroup "Digests" digestSpec
  , testGroup "Files" fileSpec
  , testGroup "Corpus" [corpusCase]
  ]

-- Single splice

-- | A document, one edit, and what the edit must make of it.
data SpliceCase = SpliceCase { scName :: String
                             , scDoc  :: Text
                             , scSpan :: Span
                             , scNew  :: Text
                             , scWant :: Text
                             }

spliceCases :: [SpliceCase]
spliceCases =
  [ SpliceCase "replace a word"           "hello world" (Span 6 11) "there"  "hello there"
  , SpliceCase "insert at the start"      "hello world" (Span 0 0)  "oh, "   "oh, hello world"
  , SpliceCase "insert at the end"        "hello world" (Span 11 11) "!"     "hello world!"
  , SpliceCase "delete a range"           "hello world" (Span 5 11) ""       "hello"
  , SpliceCase "replace the whole thing"  "hello world" (Span 0 11) "gone"   "gone"
  , SpliceCase "empty document"           ""            (Span 0 0)  "new"    "new"
  , SpliceCase "empty replacement of all" "abc"         (Span 0 3)  ""       ""
  , SpliceCase "cyrillic tail"
      "Привет мир" (Span 7 10) "world"
      "Привет world"
  , SpliceCase "cyrillic head"
      "Привет мир" (Span 0 6) "Hello"
      "Hello мир"
  , SpliceCase "keyword of a unicode headline"
      "* TODO Привет мир :тег:" (Span 2 6) "DONE"
      "* DONE Привет мир :тег:"
  ]

-- | The result is the expected text, is the prefix and suffix of the source
-- around the replacement, and has the length the arithmetic predicts.
spliceCase :: SpliceCase -> TestTree
spliceCase SpliceCase{..} = testCase scName $ do
  out <- expectRight scName (applyEdits scDoc [Edit scSpan scNew])
  assertEqual "result" scWant out
  assertEqual "by construction"
              (T.take (spanStart scSpan) scDoc <> scNew <> T.drop (spanEnd scSpan) scDoc)
              out
  assertEqual "length"
              (T.length scDoc - (spanEnd scSpan - spanStart scSpan) + T.length scNew)
              (T.length out)

-- Validation

validationSpec :: [TestTree]
validationSpec =
  [ testCase "no edits is the identity" $
      assertEqual "unchanged" (Right doc) (applyEdits doc [])

  , testCase "an end past the document is out of bounds" $
      assertEqual "error" (Left (OutOfBounds (Span 0 99) 11))
                  (applyEdits doc [Edit (Span 0 99) "x"])

  , testCase "a negative start is out of bounds" $
      assertEqual "error" (Left (OutOfBounds (Span (-1) 3) 11))
                  (applyEdits doc [Edit (Span (-1) 3) "x"])

  , testCase "a span at the very end is in bounds" $
      assertEqual "result" (Right "hello world!") (applyEdits doc [Edit (Span 11 11) "!"])

  , testCase "a start past its end is backwards" $
      assertEqual "error" (Left (Backwards (Span 5 2)))
                  (applyEdits doc [Edit (Span 5 2) "x"])

  , testCase "bounds are checked before the pair check" $
      assertEqual "error" (Left (OutOfBounds (Span 20 30) 11))
                  (applyEdits doc [Edit (Span 0 3) "x", Edit (Span 20 30) "y"])

  , testCase "overlapping edits are rejected" $
      assertEqual "error" (Left (Overlap (Span 0 6) (Span 4 9)))
                  (applyEdits doc [Edit (Span 0 6) "x", Edit (Span 4 9) "y"])

  , testCase "the overlap is the same pair whichever order it arrives in" $
      assertEqual "error" (Left (Overlap (Span 0 6) (Span 4 9)))
                  (applyEdits doc [Edit (Span 4 9) "y", Edit (Span 0 6) "x"])

  , testCase "an edit inside another is rejected" $
      assertEqual "error" (Left (Overlap (Span 0 11) (Span 2 4)))
                  (applyEdits doc [Edit (Span 0 11) "x", Edit (Span 2 4) "y"])

  , testCase "touching edits are accepted" $
      assertEqual "result" (Right "HELLO WORLD")
                  (applyEdits doc [Edit (Span 0 5) "HELLO", Edit (Span 5 11) " WORLD"])

  , testCase "two insertions at one offset apply in list order" $
      assertEqual "result" (Right "hello <one><two>world")
                  (applyEdits doc [Edit (Span 6 6) "<one>", Edit (Span 6 6) "<two>"])

  , testCase "an insertion at the start of a replacement is not an overlap" $
      assertEqual "result" (Right "hello <in>WORLD")
                  (applyEdits doc [Edit (Span 6 11) "WORLD", Edit (Span 6 6) "<in>"])
  ]
  where doc = "hello world"

-- Multi-edit

multiEditSpec :: [TestTree]
multiEditSpec =
  [ testCase "disjoint edits commute" $ do
      let a = Edit (Span 0 5) "HELLO"
          b = Edit (Span 6 11) "WORLD"
      together <- expectRight "together" (applyEdits doc [a, b])
      reversed <- expectRight "reversed" (applyEdits doc [b, a])
      stepwise <- expectRight "b then a" (applyEdits doc [b] >>= \t -> applyEdits t [a])
      assertEqual "either order" together reversed
      assertEqual "one at a time" together stepwise
      assertEqual "result" "HELLO WORLD" together

  , testCase "a deletion and an insertion in one batch" $
      assertEqual "result" (Right "hello!")
                  (applyEdits doc [Edit (Span 5 11) "", Edit (Span 11 11) "!"])
  ] ++
  [ testCase (name <> ", seed " <> show seed) $ do
      let edits = randomEdits seed (T.length text)
      out <- expectRight "generated" (applyEdits text edits)
      assertEqual "reference" (referenceApply text edits) out
      assertEqual "length" (T.length text + sum (map growth edits)) (T.length out)
  | (name, text) <- randomDocs
  , seed <- [1 .. 12 :: Int]
  ]
  where doc = "hello world"
        growth (Edit (Span s e) new) = T.length new - (e - s)

-- Exactly the span

-- | Every span H carries — 'hsFull' and each sub-span — splices exactly.
headlineSplices :: String -> Text -> Headline -> Assertion
headlineSplices name doc h =
  sequence_ [ assertExactSplice (name <> ", " <> part) doc sp | (part, sp) <- spansOf h ]

-- Round-trip with the parser

-- | Replacing a headline's TODO span re-parses to the same document with that
-- one keyword changed: the elements are equal modulo spans and that field, and
-- every span of the re-parse still slices to its own component, which is what
-- the shifted offsets after a longer keyword would break.
toggleCase :: String -> (Text, Bool) -> TestTree
toggleCase label (keyword, isActive) = testCase label $ do
  elems <- parsed label plannedDoc
  sp <- todoSpan plannedDoc
  edited <- expectRight label (applyEdits plannedDoc [Edit sp keyword])
  elems' <- parsed (label <> ", re-parse") edited
  assertEqual "elements" (map (swapTodo . stripSpans . valueOf) elems)
                         (map (stripSpans . valueOf) elems')
  case headlinesOf elems' of
    (h:_) -> do
      assertEqual "the keyword changed" (Just keyword) (name <$> todo h)
      assertEqual "the keyword slices back" (Just keyword)
                  (sliceSpan edited <$> hsTodo (spans h))
    []    -> assertFailure "the re-parse lost the headlines"
  mapM_ (assertParts (\m -> label <> ", re-parse: " <> m) edited) (headlinesOf elems')
  where swapTodo (EHeadline h) | (name <$> todo h) == Just "TODO" =
          EHeadline h { todo = Just (Todo keyword isActive) }
        swapTodo e = e

-- Digests

digestSpec :: [TestTree]
digestSpec =
  [ testCase "the empty document" $
      assertEqual "sha256" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                  (snapDigest (snapshotOf "x.org" ""))

  , testCase "a known vector" $
      assertEqual "sha256" "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
                  (snapDigest (snapshotOf "x.org" "abc"))

  , testCase "the digest is of the utf-8 bytes" $
      assertEqual "sha256" (snapDigest (snapshotOf "x.org" "мир"))
                  (snapDigest (snapshotOf "y.org" "мир"))

  , testCase "a snapshot of the text is a snapshot of the file" $
      withTempDirNamed "digest" $ \dir -> do
        let path = dir </> "notes.org"
        writeDoc path plannedDoc
        taken <- takeSnapshot path
        assertEqual "digest" (Right (snapshotOf path plannedDoc)) taken
  ]

-- Files

fileSpec :: [TestTree]
fileSpec =
  [ testCase "the edited document lands, and the receipt describes it" $
      withTempDirNamed "write" $ \dir -> do
        let path = dir </> "notes.org"
        writeDoc path plannedDoc
        toggle <- toDone
        snap <- expectRight "snapshot" =<< takeSnapshot path
        want <- expectRight "apply" (applyEdits plannedDoc [toggle])
        receipt <- expectRight "edit" =<< editFile snap [toggle]
        assertBytes "written" (TE.encodeUtf8 want) path
        assertEqual "receipt text" want (receiptText receipt)
        assertEqual "receipt snapshot" (snapshotOf path want) (receiptSnapshot receipt)
        fresh <- takeSnapshot path
        assertEqual "a fresh snapshot agrees" (Right (receiptSnapshot receipt)) fresh

  , testCase "a receipt chains the next edit without a re-read" $
      withTempDirNamed "chain" $ \dir -> do
        let path = dir </> "notes.org"
        writeDoc path plannedDoc
        toggle <- toDone
        snap <- expectRight "snapshot" =<< takeSnapshot path
        first' <- expectRight "first edit" =<< editFile snap [toggle]
        second' <- expectRight "second edit"
                     =<< editFile (receiptSnapshot first') [Edit (Span 0 0) "# top\n"]
        assertBytes "written" (TE.encodeUtf8 (receiptText second')) path
        assertEqual "the edits stacked" ("# top\n" <> receiptText first') (receiptText second')

  , testCase "a file written behind the snapshot drifts, and stays as it is" $
      withTempDirNamed "drift" $ \dir -> do
        let path = dir </> "notes.org"
            meddled = plannedDoc <> "\n* Someone else\n"
        writeDoc path plannedDoc
        toggle <- toDone
        snap <- expectRight "snapshot" =<< takeSnapshot path
        writeDoc path meddled
        outcome <- editFile snap [toggle]
        assertEqual "drift"
                    (Left (Drift path (snapDigest snap) (snapDigest (snapshotOf path meddled))))
                    (fmap receiptText outcome)
        assertBytes "untouched" (TE.encodeUtf8 meddled) path

  , testCase "a rejected batch writes nothing" $
      withTempDirNamed "reject" $ \dir -> do
        let path = dir </> "notes.org"
        writeDoc path plannedDoc
        snap <- expectRight "snapshot" =<< takeSnapshot path
        outcome <- editFile snap [Edit (Span 0 4) "x", Edit (Span 2 6) "y"]
        assertEqual "rejected" (Left (Rejected (Overlap (Span 0 4) (Span 2 6))))
                    (fmap receiptText outcome)
        assertBytes "untouched" (TE.encodeUtf8 plannedDoc) path

  , testCase "the temp file leaves nothing behind and the mode survives" $
      withTempDirNamed "atomic" $ \dir -> do
        let path = dir </> "notes.org"
        writeDoc path plannedDoc
        setFileMode path private
        toggle <- toDone
        snap <- expectRight "snapshot" =<< takeSnapshot path
        _ <- expectRight "edit" =<< editFile snap [toggle]
        left <- listDirectory dir
        assertEqual "directory contents" ["notes.org"] (sort left)
        mode <- accessMode path
        assertEqual "mode" private mode

  , testCase "a missing file reads as a failure, twice over" $
      withTempDirNamed "missing" $ \dir -> do
        let path = dir </> "gone.org"
        taken <- takeSnapshot path
        assertBool "takeSnapshot" (isReadFailure taken)
        outcome <- editFile (Snapshot path "whatever") []
        assertBool "editFile" (isReadFailure (fmap receiptText outcome))

  , testCase "bytes that are not utf-8 snapshot but do not edit" $
      withTempDirNamed "decode" $ \dir -> do
        let path = dir </> "binary.org"
            bytes = BS.pack [0x2a, 0x20, 0xff, 0xfe, 0x0a]
        BS.writeFile path bytes
        snap <- expectRight "snapshot" =<< takeSnapshot path
        outcome <- editFile snap [Edit (Span 0 1) "x"]
        assertEqual "decode failure" (Left (DecodeFailed path)) (fmap receiptText outcome)
        assertBytes "untouched" bytes path
  ]
  where
    private = 0o600 :: FileMode
    accessMode path = (.&. 0o777) . fileMode <$> getFileStatus path
    toDone = flip Edit "DONE" <$> todoSpan plannedDoc
    isReadFailure (Left (ReadFailed _ _)) = True
    isReadFailure _                       = False

-- Corpus

-- | Sampled real files: every span of every sampled headline splices exactly,
-- against the text the file actually holds.  A failure here is a parser span
-- bug rather than an engine one — a span that does not slice back cleanly is a
-- span write-back must not be handed.  Read-only, and provably so: each file is
-- snapshotted before and after the pure splices and the two must agree.
--
-- Behind @GLANCE_CORPUS@, which names the root to walk:
-- @GLANCE_CORPUS=~/sync cabal test@.  The walk is what costs — 12.8 s over
-- 690k entries for 6308 files, against 0.03 s for the rest of this suite — so
-- it stays a command of its own, the way @glance scan@ does.  Unset, it says on
-- stderr that it was skipped: a green run is not evidence for the corpus half.
corpusCase :: TestTree
corpusCase = testCase "sampled headlines splice exactly (GLANCE_CORPUS=<root>)" $
  withCorpusSample "the splice canary" $ \paths -> do
    sampled <- collect 50 paths
    mapM_ check sampled
    pure (length sampled)
  where
    -- Every span the headline carries, which is every span a command could
    -- splice: the keyword a toggle rewrites, the timestamp a reschedule
    -- replaces, and the rest of them.
    check (path, doc, hs) = do
      before <- takeSnapshot path
      sequence_ [ assertExactSplice (path <> ", " <> part) doc sp
                | h <- hs, (part, sp) <- spansOf h ]
      after <- takeSnapshot path
      assertEqual ("the check wrote to " <> path) before after

-- | Up to WANT headlines from PATHS, with the text they came from, skipping
-- files that do not read, decode or parse.
collect :: Int -> [FilePath] -> IO [(FilePath, Text, [Headline])]
collect want = go 0
  where
    go _seen [] = pure []
    go seen (path:rest)
      | seen >= want = pure []
      | otherwise = do
          loaded <- loadDoc path
          case loaded of
            Just (doc, hs) | not (null hs) ->
              ((path, doc, hs) :) <$> go (seen + length hs) rest
            _skipped -> go seen rest

-- | PATH's text and headlines, or nothing when it does not load.  Reading is
-- all this test suite ever does to the corpus.
loadDoc :: FilePath -> IO (Maybe (Text, [Headline]))
loadDoc path = do
  raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  pure $ case raw of
    Left _err -> Nothing
    Right bytes -> case TE.decodeUtf8' bytes of
      Left _err -> Nothing
      Right doc -> case orgParse defaultContext doc of
        (_elems, _ctx, Just _err) -> Nothing
        (elems, _ctx, Nothing)    -> Just (doc, headlinesOf elems)
