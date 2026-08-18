module TestNegative (spec) where

import Data.Maybe (isJust, isNothing)
import Data.Org
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import TestDefaults
import qualified TextShow as TS

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

-- | INPUT's first element is a token, however the run is spelled.
tokenCase :: (String, String, Text) -> TestTree
tokenCase (desc, msg, input) = testCase desc $ assertBool msg (parsesAs input isToken)

-- | INPUT holds WANT headlines: the column-1 rule, counted.
countCase :: (String, String, Text, Int) -> TestTree
countCase (desc, msg, input, want) =
  testCase desc $ assertEqual msg want (headlineCount input)

-- | Where a run of stars opens a headline, and where it is ink in a line.
anchorCases :: [(String, String, Text, Int)]
anchorCases =
  [ ("Mid-line emphasized keyword is not a headline", "No headline expected", "see *TODO* below", 0)
  , ( "Emphasis on a later body line is not a headline", "Only the real headline"
    , "* Task\nbody *TODO* text", 1 )
  , ("Stars after a newline still open a headline", "One headline", "body\n* Task", 1)
    -- Column 1 is necessary and not sufficient: org wants whitespace after the
    -- stars too, so a body line opening with emphasis is text wherever it sits.
  , ("Emphasis at column 1 is not a headline", "No headline expected", "*Passport requirements*", 0)
  , ( "Emphasis at column 1 under a headline stays body text", "Only the real headline"
    , "* Task\n*TODO* [[link][do it]]", 1 )
  , ("A star run with no space after it is text", "No headline expected", "**bold** claim", 0)
  ]

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

    , tokenCase ("Property-like text outside headline is a token", "Should be token", ":KEY: value")
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

    -- The stars consume horizontal space alone, so an empty title ends at the
    -- line it was written on rather than running on into the next headline.
    , testCase "An empty headline does not swallow the one after it" $
        assertEqual "elements"
          [ EHeadline (titled "First")
          , EHeadline defaultHeadline
          , EHeadline (titled "Third") ]
          (bareParse defaultContext "* First\n\n* \n\n* Third")

    , testCase "Multiple headlines in sequence" $
        assertEqual "elements"
          [ EHeadline (titled "First")
          , EHeadline (titled "Second") { indent = Indent 2 }
          , EHeadline (titled "Third") { indent = Indent 3 } ]
          (bareParse defaultContext "* First\n** Second\n*** Third")
    ]

  , testGroup "Headlines are anchored to column 1" $
    [ testCase "Mid-line emphasis is not a headline" $
        assertEqual "Should be three tokens"
          [EToken "word", EToken "*done*", EToken "word"]
          (bareParse defaultContext "word *done* word")

    , tokenCase ("Indented stars are not a headline", "Should be token", "  * Task")
    ] ++ map countCase anchorCases

  , testGroup "Timestamp edge cases" (map tokenCase
    [ ("Invalid month falls back to token", "Should not parse as timestamp", "<2024-13-01>")
    , ("Invalid day falls back to token", "Should not parse as timestamp", "<2024-01-32>")
    , ( "Invalid hour falls back to token", "Should not parse as timestamp"
      , "<2024-01-01 Mon 25:00>" )
    , ("Unclosed active timestamp", "Should be token", "<2024-01-01")
    ])

  , testGroup "Pragma edge cases"
    [ testCase "Pragma with no value" $
        case orgParse defaultContext "#+TODO: " of
          (elems, ctx, _err) -> do
            assertEqual "elements" [EPragma (PTodo [] [])] (bare elems)
            assertEqual "active keywords unchanged"
                        (todoActive defaultContext) (todoActive ctx)
            assertEqual "inactive keywords unchanged"
                        (todoInactive defaultContext) (todoInactive ctx)

    , tokenCase ("Hash without plus is a token", "Should be token", "#notapragma")
    ]

    -- A property KEY is org's own charset — any run without whitespace or a
    -- colon — wider than the TODO-keyword wall on purpose: :TELE2: and :ЖКХ:
    -- both live in the corpus and used to fail the drawer mid-parse.
  , testGroup "Property key charset"
    [ testCase "Digits and non-Latin keys keep the drawer whole" $
        withHeadline "* Tanik\n:PROPERTIES:\n:TELE2: +7 999\n\
                     \:ЖКХ: +7 495\n:ORG_GLANCE_ID: x1\n:END:\n" $ \h -> do
          let keysOf = propertyKeys h
          assertBool "digit key read" ("TELE2" `elem` keysOf)
          assertBool "non-Latin key read, uppercased like every key"
                     ("ЖКХ" `elem` keysOf)
          assertBool "the id BEHIND them survives" ("ORG_GLANCE_ID" `elem` keysOf)
    ]

    -- The weekday slot is display-only — every render recomputes the word — so
    -- the parser reads any run of letters there and drops it.  Exactly three
    -- letters made it English-only and cost ~/sync's Dutch blobs their ids.
  , testGroup "Timestamp weekday charset"
    [ testCase "A foreign weekday keeps the planning line and the drawer whole" $
        withHeadline "* Task\nCLOSED: [2025-12-04 do 22:34]\n:PROPERTIES:\n\
                     \:ORG_GLANCE_ID: x1\n:END:\n" $ \h -> do
          assertBool "the CLOSED entry attached" (isJust (closed h))
          assertBool "the id BEHIND the stamp survives"
                     ("ORG_GLANCE_ID" `elem` propertyKeys h)

      -- Every spelling ~/sync writes, plus the two of the same seven it does not.
    , testCase "Every weekday spelling the corpus writes" $
        sequence_ [ withHeadline ("* Task\nCLOSED: [2025-12-04 " <> wd <> " 22:34]") $ \h ->
                      assertBool (T.unpack wd <> " read") (isJust (closed h))
                  | wd <- ["ma", "di", "wo", "do", "vr", "za", "zo"] ]

    , testCase "A weekday is letters of any length in any script" $
        sequence_ [ withHeadline ("* Task\nSCHEDULED: <2024-01-15 " <> wd <> ">") $ \h ->
                      assertBool (T.unpack wd <> " read") (isJust (schedule h))
                  | wd <- ["M", "Mon", "Monday", "понедельник", "月曜日"] ]

      -- Dropping the word keeps the recompute rule true: a Dutch stamp comes back
      -- out in org's own English, so only a span splice carries the source spelling.
    , testCase "A foreign weekday re-renders recomputed, compact range and all" $
        sequence_
          [ case bareParse defaultContext input of
              [ETimestamp ts] -> assertEqual (T.unpack input) expected (TS.showt ts)
              other -> assertBool (T.unpack input <> ": one timestamp expected, got "
                                   <> show (length other)) False
          | (input, expected) <- [ ("[2025-12-04 do 22:34]",       "[2025-12-04 Thu 22:34]")
                                 , ("[2025-11-02 zo 21:59-22:00]", "[2025-11-02 Sun 21:59-22:00]") ] ]
    ]
  ]
