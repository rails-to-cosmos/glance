module TestNegative (spec) where

import Data.Maybe (isJust, isNothing)
import Data.Org
import qualified Data.Set as Set
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

    -- Column 1 is necessary and not sufficient: org wants whitespace after the
    -- stars too, so a body line opening with emphasis is text wherever it sits.
    , testCase "Emphasis at column 1 is not a headline" $
        assertEqual "No headline expected" 0 (headlineCount "*Passport requirements*")

    , testCase "Emphasis at column 1 under a headline stays body text" $
        assertEqual "Only the real headline" 1
          (headlineCount "* Task\n*TODO* [[link][do it]]")

    , testCase "A star run with no space after it is text" $
        assertEqual "No headline expected" 0 (headlineCount "**bold** claim")
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

    -- A property KEY is org's own charset — any run without whitespace or a
    -- colon — which is wider than the TODO-keyword wall on purpose: a digit
    -- (:TELE2:) and a non-Latin key (:ЖКХ:) both live in the corpus, and
    -- either one used to fail the drawer mid-parse, taking every later
    -- property with it.
  , testGroup "Property key charset"
    [ testCase "Digits and non-Latin keys keep the drawer whole" $
        case orgParse defaultContext
               "* Tanik\n:PROPERTIES:\n:TELE2: +7 999\n:ЖКХ: +7 495\n:ORG_GLANCE_ID: x1\n:END:\n" of
          (elems, _ctx, err) | [EHeadline h] <- bare elems -> do
            assertEqual "no parse error" Nothing err
            let Properties ps = properties h
                keysOf = [ k | Property (Keyword k) _ <- ps ]
            assertBool "digit key read" ("TELE2" `elem` keysOf)
            assertBool "non-Latin key read, uppercased like every key"
                       ("ЖКХ" `elem` keysOf)
            assertBool "the id BEHIND them survives"
                       ("ORG_GLANCE_ID" `elem` keysOf)
          (other, _, _) -> assertBool ("one headline expected: " <> show (length (bare other))) False
    ]

    -- The weekday slot is display-only — every render recomputes the word from
    -- the date — so the parser reads any run of letters there and drops it.
    -- Exactly three letters made the slot English-only, and ~/sync writes Dutch:
    -- a two-letter abbreviation failed the timestamp, which failed the planning
    -- line, which left the drawer no longer next and took the headline's
    -- properties whole.  28 of the corpus's blobs lost their id that way.
  , testGroup "Timestamp weekday charset"
    [ testCase "A foreign weekday keeps the planning line and the drawer whole" $
        case orgParse defaultContext
               "* Task\nCLOSED: [2025-12-04 do 22:34]\n:PROPERTIES:\n:ORG_GLANCE_ID: x1\n:END:\n" of
          (elems, _ctx, err) | [EHeadline h] <- bare elems -> do
            assertEqual "no parse error" Nothing err
            assertBool "the CLOSED entry attached" (isJust (closed h))
            let Properties ps = properties h
            assertBool "the id BEHIND the stamp survives"
                       ("ORG_GLANCE_ID" `elem` [k | Property (Keyword k) _ <- ps])
          (other, _, _) -> assertBool ("one headline expected: " <> show (length (bare other))) False

      -- Every spelling ~/sync writes, plus the two the locale has that it does
      -- not: the census found ma, do, zo, vr and za, and di and wo belong to
      -- the same seven.
    , testCase "Every weekday spelling the corpus writes" $
        sequence_ [ withHeadline ("* Task\nCLOSED: [2025-12-04 " <> wd <> " 22:34]") $ \h ->
                      assertBool (T.unpack wd <> " read") (isJust (closed h))
                  | wd <- ["ma", "di", "wo", "do", "vr", "za", "zo"] ]

    , testCase "A weekday is letters of any length in any script" $
        sequence_ [ withHeadline ("* Task\nSCHEDULED: <2024-01-15 " <> wd <> ">") $ \h ->
                      assertBool (T.unpack wd <> " read") (isJust (schedule h))
                  | wd <- ["M", "Mon", "Monday", "понедельник", "月曜日"] ]

      -- Dropping the word is what keeps the recompute rule true: a Dutch stamp
      -- comes back out in org's own English, so only a span splice carries the
      -- source spelling anywhere.  The compact range spelling is untouched by
      -- the slot ahead of it.
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
