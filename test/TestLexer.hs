module TestLexer (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Data.Org (orgParse)
import qualified Data.Org as Org
import Text.Megaparsec (errorBundlePretty)

data TestCase = TestCase !Text ![Text] !Result

data Result = Result ![Org.Element] (Maybe Text)
  deriving (Eq, Show)

testCases :: [TestCase]
testCases =
  [ expectSuccess "Single token"         ["a"]                 ["a"]
  , expectSuccess "Multiple tokens"      ["a", "b"]            ["a", "b"]
  , expectSuccess "Skip spaces"          [" "]                 []
  , expectSuccess "Skip leading spaces"  [" ", "a", "b"]       ["a", "b"]
  , expectSuccess "Skip trailing spaces" ["a", "b", " "]       ["a", "b"]
  , expectSuccess "Skip multiple spaces" ["a", " ", " ", "b"]  ["a", "b"]
  ]

expectSuccess :: Text -> [Text] -> [Text] -> TestCase
expectSuccess description inputLines result = TestCase description inputLines (Result (map token result) Nothing)

token :: Text -> Org.Element
token a = Org.Element (Org.Token a)

spec :: TestTree
spec = testGroup "Lexer" assertMany
  where assertMany = map assert testCases
        assert tc@(TestCase description _ expected) = testCase (T.unpack description)
          $ assertEqual [] expected
          $ result tc
        result (TestCase _ input _) = case orgParse mempty inputLines of
          (tokens, context, Nothing) -> Result tokens Nothing
          (tokens, context, Just err) -> Result tokens (Just $ T.replace "\n" " " $ T.pack $ errorBundlePretty err)
          where inputLines = T.intercalate "\n" input
