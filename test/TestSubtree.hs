-- | Subtree extents: what @\/headline@ hands a client to edit.  Expectations
-- are spelled as the slice TEXT rather than as offsets, which drift from the
-- fixture; 'hsFull' is the parser's, so containment is worth only the real one.
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


fixtureDir :: FilePath
fixtureDir = "test/fixtures/subtree"

fixture :: FilePath -> FilePath
fixture name = fixtureDir <> "/" <> name


subtreesOf :: FilePath -> IO [Text]
subtreesOf name = map subtreeText <$> recordsOf (fixture name)

levelOf :: Headline -> Int
levelOf h = case indent h of Indent n -> n

fullOf :: HeadlineRecord -> Span
fullOf = hsFull . spans . hrHeadline

-- | Extents TILE rather than nest: one per top entry, meeting exactly.
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

    isATopEntry r = assertEqual (at r <> ": not a top entry") 1 (levelOf (hrHeadline r))

    coversItsHeadline r = do
      assertEqual (at r <> ": starts at the headline")
                  (spanStart (fullOf r)) (spanStart (hrSubtree r))
      assertBool (at r <> ": ends before its own headline does")
                 (spanEnd (hrSubtree r) >= spanEnd (fullOf r))
      assertBool (at r <> ": does not fit the document")
                 (spanEnd (hrSubtree r) <= T.length doc)

    abuts (a, b) = assertEqual
      (label <> ": " <> show (hrSubtree a) <> " and " <> show (hrSubtree b) <> " do not meet")
      (spanEnd (hrSubtree a)) (spanStart (hrSubtree b))


spec :: TestTree
spec = testGroup "Subtree" [extentSpec, invariantSpec, corpusSpec]

extentSpec :: TestTree
extentSpec = testGroup "Extent"
  [ testCase "a headline runs to the next one at its level" $
      subtreesOf "flat.org" >>= assertEqual "subtrees"
        [ "* One\nbody of one\n"
        , "* Two\n"
        , "* Three\nlast line\n"
        ]

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
      -- 61 characters over 105 bytes: a byte offset runs past the end.
      assertEqual "characters" 61 (T.length doc)
      assertEqual "bytes" 105 (BS.length (TE.encodeUtf8 doc))
  ]

invariantSpec :: TestTree
invariantSpec = testGroup "Invariants"
  [ testCase name $ do
      doc <- document (fixture name)
      recs <- recordsOf (fixture name)
      assertGeometry name doc recs
  | name <- ["flat.org", "nested.org", "blanks.org", "single.org", "unicode.org"]
  ]

-- | The same geometry over sampled real files, behind @GLANCE_CORPUS@.
corpusSpec :: TestTree
corpusSpec = testGroup "Corpus"
  [ testCase "sampled files nest, tile and end at EOF (GLANCE_CORPUS=<root>)" $
      withCorpusSample "the subtree geometry" (fmap sum . mapM checkFile)
  ]
  where
    -- A file that does not read, decode or parse contributes no claim.
    checkFile path = do
      loaded <- loadFile path
      raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
      case (loaded, TE.decodeUtf8' <$> raw) of
        (Right recs, Right (Right doc)) -> length recs <$ assertGeometry path doc recs
        _unusable                       -> pure 0
