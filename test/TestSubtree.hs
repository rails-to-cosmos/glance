-- | Subtree extents: what @\/headline@ hands a client to edit.
--
-- The unit under test is a slice of raw text, so every expectation here is
-- spelled as the text itself rather than as an offset — an offset typed into a
-- test is one the fixture drifts away from, and the slice is what a browser
-- would show.  The invariant group then states the geometry the write path
-- rests on: one extent per top entry, each covering its own headline and every
-- descendant, consecutive extents meeting exactly, and the last one running to
-- the end of the file.
--
-- This module names the parser as well as the facade.  'hrSubtree' is a facade
-- value, but the headline extent it must contain ('hsFull') is the parser's,
-- and the containment claim is only worth asserting against the real one.
module TestSubtree (spec) where

import Control.Exception (IOException, try)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import TestDefaults (document, recordsOf, withCorpusSample)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Org ( Headline, Indent (Indent), Span (..), hsFull, indent, spans )
import Glance.Query ( HeadlineRecord (hrHeadline, hrSubtree), loadFile, subtreeText )

-- Fixtures

fixtureDir :: FilePath
fixtureDir = "test/fixtures/subtree"

-- | The fixture named NAME, as a path.
fixture :: FilePath -> FilePath
fixture name = fixtureDir <> "/" <> name

-- Helpers

-- | The subtree of every record the fixture named NAME loads, as text.
subtreesOf :: FilePath -> IO [Text]
subtreesOf name = map subtreeText <$> recordsOf (fixture name)

-- | H's outline level.
levelOf :: Headline -> Int
levelOf h = case indent h of Indent n -> n

-- | R's headline extent — where the stars start and the last parsed component
-- of the headline itself ends.
fullOf :: HeadlineRecord -> Span
fullOf = hsFull . spans . hrHeadline

-- | The geometry every load must produce, stated over one file's RECORDS
-- against its text DOC.  LABEL names the file in the failure.
--
-- Records are top entries, so the extents TILE rather than nest: each is one
-- level-one headline and everything under it, and the next one begins exactly
-- where it ends.  The nesting is inside a single extent, which is what a
-- materialize hands over whole.
assertGeometry :: String -> Text -> [HeadlineRecord] -> Assertion
assertGeometry label doc recs = do
  mapM_ isATopEntry recs
  mapM_ coversItsHeadline recs
  mapM_ abuts (zip recs (drop 1 recs))
  case recs of
    [] -> pure ()
    _  -> assertEqual (label <> ": the last subtree ends at the end of the file")
                      (T.length doc) (spanEnd (hrSubtree (last recs)))
  where
    at r = label <> ", subtree " <> show (hrSubtree r)

    -- One star, no ancestor.  A deeper headline is inside somebody's extent
    -- rather than beside it, so its presence here would mean two extents
    -- covering one byte.
    isATopEntry r = assertEqual (at r <> ": not a top entry") 1 (levelOf (hrHeadline r))

    -- A subtree starts at its own stars and runs at least to the end of the
    -- headline line: it is the headline plus what hangs off it, never less.
    coversItsHeadline r = do
      assertEqual (at r <> ": starts at the headline")
                  (spanStart (fullOf r)) (spanStart (hrSubtree r))
      assertBool (at r <> ": ends before its own headline does")
                 (spanEnd (hrSubtree r) >= spanEnd (fullOf r))
      assertBool (at r <> ": does not fit the document")
                 (spanEnd (hrSubtree r) <= T.length doc)

    -- Consecutive extents meet exactly: no gap, which would be text belonging
    -- to no entry, and no overlap, which would be text belonging to two.
    abuts (a, b) = assertEqual
      (label <> ": " <> show (hrSubtree a) <> " and " <> show (hrSubtree b) <> " do not meet")
      (spanEnd (hrSubtree a)) (spanStart (hrSubtree b))

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

    -- Rows are top entries, so this file's five headlines are two extents —
    -- and the first of them is what carries every child, which is what a
    -- materialize of it hands back.
  , testCase "a top entry carries its whole outline, children and all" $
      subtreesOf "nested.org" >>= assertEqual "subtrees"
        [ "* Parent\nparent body\n** Child A\na body\n*** Grandchild\ndeep body\n** Child B\n"
        , "* Next top\ntail\n"
        ]

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
      recs <- recordsOf (fixture "unicode.org")
      assertEqual "subtrees"
        [ "* Привет мир\nтело письма\n** Дочь :тег:\nвложенное\n"
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
      recs <- recordsOf (fixture name)
      assertGeometry name doc recs
  | name <- ["flat.org", "nested.org", "blanks.org", "single.org", "unicode.org"]
  ]

-- | The same geometry over sampled real files.  Read-only — the whole test is
-- a load and a set of comparisons, and no path here writes.
--
-- Behind @GLANCE_CORPUS@, which names the root to walk, the way TestEdit's
-- canary is: @GLANCE_CORPUS=~\/sync cabal test@.  The walk is what costs.  Unset,
-- it says on stderr that it was skipped rather than passing quietly.
corpusSpec :: TestTree
corpusSpec = testGroup "Corpus"
  [ testCase "sampled files nest, tile and end at EOF (GLANCE_CORPUS=<root>)" $
      withCorpusSample "the subtree geometry" (fmap sum . mapM checkFile)
  ]
  where
    -- A file that does not read, decode or parse contributes no records and no
    -- claim; the corpus has some of each and they are TestEdit's subject.
    checkFile path = do
      loaded <- loadFile path
      raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
      case (loaded, TE.decodeUtf8' <$> raw) of
        (Right recs, Right (Right doc)) -> length recs <$ assertGeometry path doc recs
        _unusable                       -> pure 0
