-- | Subtree extents: what @\/headline@ hands a client to edit.
--
-- The unit under test is a slice of raw text, so every expectation here is
-- spelled as the text itself rather than as an offset — an offset typed into a
-- test is one the fixture drifts away from, and the slice is what a browser
-- would show.  The invariant group then states the geometry the write path
-- rests on: subtrees nest, siblings do not overlap, each covers its own
-- headline, and the last one runs to the end of the file.
--
-- This module names the parser as well as the facade.  'hrSubtree' is a facade
-- value, but the headline extent it must contain ('hsFull') is the parser's,
-- and the containment claim is only worth asserting against the real one.
module TestSubtree (spec) where

import Control.Exception (IOException, try)
import Data.List (sort)
import Data.Text (Text)
import System.Directory (doesDirectoryExist)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Org ( Headline, Indent (Indent), Span (..), hsFull, indent, spans )
import Data.Org.Walk (findOrgFiles, foundFiles)
import Glance.Query ( HeadlineRecord (hrHeadline, hrSubtree), loadFile, subtreeText )

-- Fixtures

fixtureDir :: FilePath
fixtureDir = "test/fixtures/subtree"

-- | The fixture named NAME, as a path.
fixture :: FilePath -> FilePath
fixture name = fixtureDir <> "/" <> name

-- Helpers

-- | PATH's records, or its load failure as a test failure.
records :: FilePath -> IO [HeadlineRecord]
records path = loadFile path >>= either whyNot pure
  where whyNot failure = assertFailure ("expected " <> path <> " to load, got " <> show failure)

-- | PATH's text, decoded the way the loader decodes it.
document :: FilePath -> IO Text
document path = TE.decodeUtf8 <$> BS.readFile path

-- | The subtree of every headline in the fixture named NAME, as text.
subtreesOf :: FilePath -> IO [Text]
subtreesOf name = map subtreeText <$> records (fixture name)

-- | H's outline level.
levelOf :: Headline -> Int
levelOf h = case indent h of Indent n -> n

-- | R's headline extent — where the stars start and the last parsed component
-- of the headline itself ends.
fullOf :: HeadlineRecord -> Span
fullOf = hsFull . spans . hrHeadline

-- | The geometry every load must produce, stated over one file's RECORDS
-- against its text DOC.  LABEL names the file in the failure.
assertGeometry :: String -> Text -> [HeadlineRecord] -> Assertion
assertGeometry label doc recs = do
  mapM_ coversItsHeadline recs
  mapM_ nestOrMiss (pairs recs)
  mapM_ leavesNoGap (zip recs (drop 1 recs))
  case recs of
    [] -> pure ()
    _  -> assertEqual (label <> ": the last subtree ends at the end of the file")
                      (T.length doc) (spanEnd (hrSubtree (last recs)))
  where
    at r = label <> ", subtree " <> show (hrSubtree r)

    -- A subtree starts at its own stars and runs at least to the end of the
    -- headline line: it is the headline plus what hangs off it, never less.
    coversItsHeadline r = do
      assertEqual (at r <> ": starts at the headline")
                  (spanStart (fullOf r)) (spanStart (hrSubtree r))
      assertBool (at r <> ": ends before its own headline does")
                 (spanEnd (hrSubtree r) >= spanEnd (fullOf r))
      assertBool (at r <> ": does not fit the document")
                 (spanEnd (hrSubtree r) <= T.length doc)

    -- Any two subtrees are disjoint or one holds the other whole: an outline
    -- has no crossing extents, and a deeper headline sits inside the one above
    -- it while two at one level cannot share a character.
    nestOrMiss (a, b) = assertBool
      (label <> ": " <> show (hrSubtree a) <> " and " <> show (hrSubtree b) <> " cross")
      (disjoint || inside b a || inside a b)
      where disjoint = spanEnd (hrSubtree a) <= spanStart (hrSubtree b)
                    || spanEnd (hrSubtree b) <= spanStart (hrSubtree a)
            inside x y = spanStart (hrSubtree y) <= spanStart (hrSubtree x)
                      && spanEnd (hrSubtree x) <= spanEnd (hrSubtree y)

    -- The next headline in the file starts inside the current subtree (it is a
    -- descendant) or exactly where it ends (it closes it).  A gap would be
    -- text belonging to no subtree at all.
    leavesNoGap (a, b) = do
      assertBool (label <> ": headlines out of order") (start b >= start a)
      assertBool (label <> ": a gap between " <> show (hrSubtree a) <> " and " <> show (hrSubtree b))
                 (start b <= spanEnd (hrSubtree a))
      assertBool (label <> ": a deeper headline outside its parent")
                 (levelOf (hrHeadline b) <= levelOf (hrHeadline a)
                   || spanEnd (hrSubtree b) <= spanEnd (hrSubtree a))
      where start = spanStart . hrSubtree

pairs :: [a] -> [(a, a)]
pairs xs = [(a, b) | (i, a) <- zip [0 :: Int ..] xs, (j, b) <- zip [0 ..] xs, i < j]

-- Spec

spec :: TestTree
spec = testGroup "Subtree" [extentSpec, invariantSpec, corpusSpec]

-- | What the slice actually is, file by file.
extentSpec :: TestTree
extentSpec = testGroup "Extent"
  [ testCase "a headline runs to the next one at its level" $
      subtreesOf "flat.org" >>= assertEqual "subtrees"
        [ "* One\nbody of one\n"
        , "* Two\n"
        , "* Three\nlast line\n"
        ]

  , testCase "a parent carries its children, and each child carries its own" $
      subtreesOf "nested.org" >>= assertEqual "subtrees"
        [ "* Parent\nparent body\n** Child A\na body\n*** Grandchild\ndeep body\n** Child B\n"
        , "** Child A\na body\n*** Grandchild\ndeep body\n"
        , "*** Grandchild\ndeep body\n"
        , "** Child B\n"
        , "* Next top\ntail\n"
        ]

  , testCase "a deeper headline ends at the shallower one that follows it" $ do
      subtrees <- subtreesOf "nested.org"
      assertEqual "the grandchild stops at Child B" ["*** Grandchild\ndeep body\n"]
                  (take 1 (drop 2 subtrees))

  , testCase "the last headline runs to the end of the file" $ do
      doc <- document (fixture "flat.org")
      recs <- records (fixture "flat.org")
      assertEqual "end" (T.length doc) (spanEnd (hrSubtree (last recs)))
      assertBool "the slice is the file's tail" (subtreeText (last recs) `T.isSuffixOf` doc)

  , testCase "blank lines before the next headline belong to the subtree above" $
      subtreesOf "blanks.org" >>= assertEqual "subtrees"
        [ "* First\n\nbody\n\n\n"
        , "* Second\n"
        ]

  , testCase "a file with one headline is one subtree, the whole document" $ do
      subtrees <- subtreesOf "single.org"
      doc <- document (fixture "single.org")
      assertEqual "subtrees" [doc] subtrees

  , testCase "offsets are characters, so unicode slices whole" $ do
      subtrees <- subtreesOf "unicode.org"
      doc <- document (fixture "unicode.org")
      recs <- records (fixture "unicode.org")
      assertEqual "subtrees"
        [ "* Привет мир\nтело письма\n** Дочь :тег:\nвложенное\n"
        , "** Дочь :тег:\nвложенное\n"
        , "* Последний\n"
        ]
        subtrees
      -- 61 characters over 105 bytes: an offset counted in bytes would run
      -- past the end of this document, and one measured in bytes and sliced in
      -- characters would cut the fixture's tail off.
      assertEqual "the last subtree ends at the character length"
                  (T.length doc) (spanEnd (hrSubtree (last recs)))
      assertEqual "characters" 61 (T.length doc)
      assertEqual "bytes" 105 (BS.length (TE.encodeUtf8 doc))
  ]

-- | The geometry over every fixture, stated once and checked on all of them.
invariantSpec :: TestTree
invariantSpec = testGroup "Invariants"
  [ testCase name $ do
      doc <- document (fixture name)
      recs <- records (fixture name)
      assertBool (name <> " has no headlines") (not (null recs))
      assertGeometry name doc recs
  | name <- ["flat.org", "nested.org", "blanks.org", "single.org", "unicode.org"]
  ]

-- | The same geometry over sampled real files.  Read-only — the whole test is
-- a load and a set of comparisons, and no path here writes.
--
-- Behind @GLANCE_CORPUS@, which names the root to walk, the way TestEdit's
-- canary is: @GLANCE_CORPUS=~\/sync cabal test@.  The walk is what costs.
corpusSpec :: TestTree
corpusSpec = testGroup "Corpus"
  [ testCase "sampled files nest, tile and end at EOF (GLANCE_CORPUS=<root>)" $ do
      asked <- lookupEnv "GLANCE_CORPUS"
      there <- maybe (pure False) doesDirectoryExist asked
      case asked of
        Just root | there -> do
          found <- findOrgFiles [root]
          let paths = sample (sort (foundFiles found))
          checked <- sum <$> mapM checkFile paths
          assertBool ("no headline sampled under " <> root) (checked > 0)
        Just root -> assertFailure ("GLANCE_CORPUS names no directory: " <> root)
        Nothing   -> pure ()
  ]
  where
    sample paths = every (max 1 (length paths `div` 64)) paths
    every k = map snd . filter ((== 0) . (`mod` k) . fst) . zip [0 :: Int ..]

    -- A file that does not read, decode or parse contributes no records and no
    -- claim; the corpus has some of each and they are TestEdit's subject.
    checkFile path = do
      loaded <- loadFile path
      raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
      case (loaded, TE.decodeUtf8' <$> raw) of
        (Right recs, Right (Right doc)) -> length recs <$ assertGeometry path doc recs
        _unusable                       -> pure 0
