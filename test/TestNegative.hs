module TestNegative (spec) where

import Data.Org
import qualified Data.Org as Org
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)

parsesAs :: Text -> (Element -> Bool) -> Bool
parsesAs input predicate = case orgParse defaultContext input of
  (e:_, _, _) -> predicate (valueOf e)
  _           -> False

isToken :: Element -> Bool
isToken (EToken _) = True
isToken _ = False

isHeadline :: Element -> Bool
isHeadline (EHeadline _) = True
isHeadline _ = False

hasError :: Text -> Bool
hasError input = case orgParse defaultContext input of
  (_, _, Just _) -> True
  _              -> False

elementCount :: Text -> Int
elementCount input = case orgParse defaultContext input of
  (elems, _, _) -> length elems

spec :: TestTree
spec = testGroup "Negative / Edge cases"
  [ testGroup "Graceful degradation"
    [ testCase "Lone star parses as headline with empty title" $
        assertBool "Should be a headline" (parsesAs "*" isHeadline)

    , testCase "Incomplete tags become part of title" $ do
        let (elems, _, _) = orgParse defaultContext "* Hello :incomplete"
        case map valueOf elems of
          [EHeadline h] ->
            assertEqual "Incomplete tags should be in title"
              (Title [OrgLineToken (Token "Hello"), OrgLineToken (Token ":incomplete")])
              (title h)
          _ -> assertBool "Expected single headline" False

    , testCase "Unknown pragma keyword becomes generic pragma" $ do
        let (elems, _, _) = orgParse defaultContext "#+FOOBAR: some value"
        case map valueOf elems of
          [EPragma (Pragma (Keyword "FOOBAR") _)] -> return ()
          _ -> assertBool ("Expected generic pragma, got: " <> show elems) False

    , testCase "Drawer is not a property block" $
        assertBool "Standalone drawer should be token" (parsesAs ":DRAWER:" isToken)

    , testCase "Property-like text outside headline is a token" $
        assertBool "Should be token" (parsesAs ":KEY: value" isToken)
    ]

  , testGroup "Empty / whitespace inputs"
    [ testCase "Empty string" $
        assertEqual "Should produce no elements" 0 (elementCount "")

    , testCase "Only spaces" $
        assertEqual "Should produce no elements" 0 (elementCount "   ")

    , testCase "Only newlines" $
        assertEqual "Should produce no elements" 0 (elementCount "\n\n\n")

    , testCase "Mixed whitespace" $
        assertEqual "Should produce no elements" 0 (elementCount "  \n  \n  ")
    ]

  , testGroup "Headline edge cases"
    [ testCase "Headline with only stars and space is valid headline" $ do
        let (elems, _, _) = orgParse defaultContext "* "
        case map valueOf elems of
          [EHeadline h] -> assertEqual "Title should be empty" (Title []) (title h)
          _ -> assertBool ("Expected headline with empty title, got: " <> show elems) False

    , testCase "Very deep nesting" $ do
        let (elems, _, _) = orgParse defaultContext "********** Deep"
        case map valueOf elems of
          [EHeadline h] -> assertEqual "Should have indent 10" (Indent 10) (indent h)
          _ -> assertBool "Expected headline" False

    , testCase "Headline without title text after TODO" $ do
        let (elems, _, _) = orgParse defaultContext "* TODO"
        case map valueOf elems of
          [EHeadline h] -> do
            assertEqual "Should have TODO" (Just (Todo "TODO" True)) (todo h)
            assertEqual "Title should be empty" (Title []) (title h)
          _ -> assertBool ("Expected headline, got: " <> show elems) False

    , testCase "Priority without TODO" $ do
        let (elems, _, _) = orgParse defaultContext "* [#A] Hello"
        case map valueOf elems of
          [EHeadline h] -> do
            assertEqual "No TODO" Nothing (todo h)
            assertEqual "Has priority" (Just (Priority 'A')) (priority h)
          _ -> assertBool "Expected headline" False

    , testCase "Multiple headlines in sequence" $ do
        let input = T.intercalate "\n" ["* First", "** Second", "*** Third"]
        assertEqual "Should parse 3 headlines" 3 (elementCount input)
    ]

  , testGroup "Headlines are anchored to column 1"
    [ testCase "Mid-line emphasis is not a headline" $ do
        let (elems, _, _) = orgParse defaultContext "word *done* word"
        assertEqual "Should be three tokens"
          [EToken (Token "word"), EToken (Token "*done*"), EToken (Token "word")]
          (map valueOf elems)

    , testCase "Mid-line emphasized keyword is not a headline" $ do
        let (elems, _, _) = orgParse defaultContext "see *TODO* below"
        assertBool "No headline expected" (not (any (isHeadline . valueOf) elems))

    , testCase "Emphasis on a later body line is not a headline" $ do
        let (elems, _, _) = orgParse defaultContext (T.intercalate "\n" ["* Task", "body *TODO* text"])
        assertEqual "Only the real headline" 1 (length (filter (isHeadline . valueOf) elems))

    , testCase "Stars after a newline still open a headline" $ do
        let (elems, _, _) = orgParse defaultContext "body\n* Task"
        assertEqual "One headline" 1 (length (filter (isHeadline . valueOf) elems))

    , testCase "Indented stars are not a headline" $
        assertBool "Should be token" (parsesAs "  * Task" isToken)
    ]

  , testGroup "Timestamp edge cases"
    [ testCase "Invalid month falls back to token" $
        assertBool "Should not parse as timestamp" (parsesAs "<2024-13-01>" isToken)

    , testCase "Invalid day falls back to token" $
        assertBool "Should not parse as timestamp" (parsesAs "<2024-01-32>" isToken)

    , testCase "Unclosed active timestamp" $
        assertBool "Should be token" (parsesAs "<2024-01-01" isToken)
    ]

  , testGroup "Pragma edge cases"
    [ testCase "Pragma with no value" $ do
        let (elems, _, _) = orgParse defaultContext "#+TODO: "
        case map valueOf elems of
          [EPragma (PTodo _ _)] -> return ()
          _ -> assertBool ("Expected TODO pragma, got: " <> show elems) False

    , testCase "Hash without plus is a token" $
        assertBool "Should be token" (parsesAs "#notapragma" isToken)
    ]
  ]
