module TestNegative (spec) where

import Data.Maybe (isJust, isNothing)
import Data.Org
import qualified Data.Set as Set
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import TestDefaults

parsesAs :: Text -> (Element -> Bool) -> Bool
parsesAs input predicate = case bareParse defaultContext input of
  (e:_) -> predicate e
  _     -> False

isToken :: Element -> Bool
isToken (EToken _) = True
isToken _ = False

isHeadline :: Element -> Bool
isHeadline (EHeadline _) = True
isHeadline _ = False

headlineCount :: Text -> Int
headlineCount = length . filter isHeadline . bareParse defaultContext

-- | INPUT must parse cleanly and yield nothing at all.
assertNoElements :: (String, Text) -> Assertion
assertNoElements (desc, input) = case orgParse defaultContext input of
  (elems, _ctx, err) -> do
    assertBool (desc <> ": unexpected parse error") (isNothing err)
    assertEqual (desc <> ": elements") [] (bare elems)

spec :: TestTree
spec = testGroup "Negative / Edge cases"
  [ testGroup "Graceful degradation"
    [ testCase "Lone star parses as headline with empty title" $
        assertBool "Should be a headline" (parsesAs "*" isHeadline)

    , testCase "Incomplete tags become part of title" $
        withHeadline "* Hello :incomplete" $ \h ->
          assertEqual "Incomplete tags should be in title"
            (Title [OrgLineToken "Hello", OrgLineToken ":incomplete"])
            (title h)

    , testCase "Unknown pragma keyword becomes generic pragma" $
        assertEqual "elements"
          [EPragma (Pragma (Keyword "FOOBAR") (OrgLine [OrgLineToken "some", OrgLineToken "value"]))]
          (bareParse defaultContext "#+FOOBAR: some value")

    , testCase "Property-like text outside headline is a token" $
        assertBool "Should be token" (parsesAs ":KEY: value" isToken)
    ]

  , testCase "Whitespace-only input yields nothing" $
      mapM_ assertNoElements [ ("Only spaces",     "   ")
                             , ("Only newlines",   "\n\n\n")
                             , ("Mixed whitespace", "  \n  \n  ") ]

  , testGroup "Parse failures"
    [ testCase "Mismatched range brackets fail the whole document" $
        case orgParse defaultContext "[2023-07-15 Sat 15:54]--<2023-07-15 Sat 17:10>" of
          (elems, ctx, err) -> do
            assertBool "expected a parse error" (isJust err)
            assertEqual "no elements on error" [] (bare elems)
            assertEqual "context untouched on error" defaultContext ctx
    ]

  , testGroup "Headline edge cases"
    [ testCase "Headline with only stars and space is valid headline" $
        withHeadline "* " $ \h -> assertEqual "Title should be empty" (Title []) (title h)

    , testCase "Very deep nesting" $
        withHeadline "********** Deep" $ \h ->
          assertEqual "Should have indent 10" (Indent 10) (indent h)

    , testCase "Headline without title text after TODO" $
        withHeadline "* TODO" $ \h -> do
          assertEqual "Should have TODO" (Just (Todo "TODO" True)) (todo h)
          assertEqual "Title should be empty" (Title []) (title h)

    , testCase "Priority without TODO" $
        withHeadline "* [#A] Hello" $ \h -> do
          assertEqual "No TODO" Nothing (todo h)
          assertEqual "Has priority" (Just (Priority 'A')) (priority h)

    , testCase "Multiple headlines in sequence" $
        assertEqual "elements"
          [ EHeadline (titled "First")
          , EHeadline (titled "Second") { indent = Indent 2 }
          , EHeadline (titled "Third") { indent = Indent 3 } ]
          (bareParse defaultContext "* First\n** Second\n*** Third")
    ]

  , testGroup "Headlines are anchored to column 1"
    [ testCase "Mid-line emphasis is not a headline" $
        assertEqual "Should be three tokens"
          [EToken "word", EToken "*done*", EToken "word"]
          (bareParse defaultContext "word *done* word")

    , testCase "Mid-line emphasized keyword is not a headline" $
        assertEqual "No headline expected" 0 (headlineCount "see *TODO* below")

    , testCase "Emphasis on a later body line is not a headline" $
        assertEqual "Only the real headline" 1 (headlineCount "* Task\nbody *TODO* text")

    , testCase "Stars after a newline still open a headline" $
        assertEqual "One headline" 1 (headlineCount "body\n* Task")

    , testCase "Indented stars are not a headline" $
        assertBool "Should be token" (parsesAs "  * Task" isToken)
    ]

  , testGroup "Timestamp edge cases"
    [ testCase "Invalid month falls back to token" $
        assertBool "Should not parse as timestamp" (parsesAs "<2024-13-01>" isToken)

    , testCase "Invalid day falls back to token" $
        assertBool "Should not parse as timestamp" (parsesAs "<2024-01-32>" isToken)

    , testCase "Invalid hour falls back to token" $
        assertBool "Should not parse as timestamp" (parsesAs "<2024-01-01 Mon 25:00>" isToken)

    , testCase "Unclosed active timestamp" $
        assertBool "Should be token" (parsesAs "<2024-01-01" isToken)
    ]

  , testGroup "Pragma edge cases"
    [ testCase "Pragma with no value" $
        case orgParse defaultContext "#+TODO: " of
          (elems, ctx, _err) -> do
            assertEqual "elements" [EPragma (PTodo Set.empty Set.empty)] (bare elems)
            assertEqual "active keywords unchanged"
                        (todoActive defaultContext) (todoActive ctx)
            assertEqual "inactive keywords unchanged"
                        (todoInactive defaultContext) (todoInactive ctx)

    , testCase "Hash without plus is a token" $
        assertBool "Should be token" (parsesAs "#notapragma" isToken)
    ]
  ]
