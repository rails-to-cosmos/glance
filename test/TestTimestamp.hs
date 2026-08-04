module TestTimestamp (spec) where

import Data.Org
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestDefaults

parseTimestamp :: Text -> Maybe Timestamp
parseTimestamp input = case orgParse defaultContext input of
  (Spanned _ (ETimestamp ts) : _, _, _) -> Just ts
  _                                     -> Nothing

-- | MOMENT repeating by INTERVAL, as an active timestamp.
repeating :: TsMoment -> TimestampRepeaterInterval -> Timestamp
repeating moment interval = (plainTs TimestampActive moment) { tsInterval = Just interval }

-- | One row per source that must parse to a timestamp and nothing more: LABEL,
-- INPUT, and what INPUT must parse to.  A case asserting something else — a
-- projection out of the parse, a render — stays a case of its own below.
parseCases :: [(String, Text, Maybe Timestamp)]
parseCases =
  [ ( "Active date with time", "<2024-01-15 Mon 10:30>"
    , Just (plainTs TimestampActive (at "2024-01-15 10:30:00")) )

  , ( "Active date with time and seconds", "<2024-01-15 Mon 10:30:45>"
    , Just (plainTs TimestampActive (at "2024-01-15 10:30:45")) )

  , ( "Inactive date only", "[2024-06-15]"
    , Just (plainTs TimestampInactive (on "2024-06-15 00:00:00")) )

  , ( "Inactive date with weekday", "[2024-06-15 Sat]"
    , Just (plainTs TimestampInactive (on "2024-06-15 00:00:00")) )

  , ( "Inactive date with time", "[2024-06-15 Sat 14:00]"
    , Just (plainTs TimestampInactive (at "2024-06-15 14:00:00")) )

  , ( "Active range", "<2024-01-15 Mon>--<2024-01-19 Fri>"
    , Just (plainTs TimestampActive (on "2024-01-15 00:00:00"))
             { tsEnd = Just (on "2024-01-19 00:00:00") } )

  , ( "Compact range, both times land on the start's day"
    , "<2024-01-15 Mon 10:30-11:30>"
    , Just (compactTs TimestampActive (at "2024-01-15 10:30:00")
                                      (at "2024-01-15 11:30:00")) )

  , ( "Compact range in inactive brackets", "[2021-11-09 Tue 17:30-18:30]"
    , Just (compactTs TimestampInactive (at "2021-11-09 17:30:00")
                                        (at "2021-11-09 18:30:00")) )

  , ( "Compact range with seconds on both ends", "<2024-01-15 Mon 10:30:15-11:45:30>"
    , Just (compactTs TimestampActive (at "2024-01-15 10:30:15")
                                      (at "2024-01-15 11:45:30")) )

  , ( "A repeater follows the compact range", "<2024-01-15 Mon 10:30-11:30 +1w>"
    , Just (compactTs TimestampActive (at "2024-01-15 10:30:00")
                                      (at "2024-01-15 11:30:00"))
             { tsInterval = Just (TimestampRepeaterInterval Restart 1 Weeks TRSPlus) } )

    -- '-' opens both a range end and a warning cookie; only the time's colon
    -- separates them, so "-1d" backtracks out of the range and stays a cookie
    -- whether or not a space precedes it.  org's grammar: a lone "-1d" is the
    -- agenda warning, never a repeater.
  , ( "A warning cookie is not a range end", "<2024-01-15 Mon 10:30-1d>"
    , Just ((plainTs TimestampActive (at "2024-01-15 10:30:00"))
              { tsWarning = Just (TimestampWarningInterval False 1 Days) }) )

    -- org's canonical two-cookie stamp: repeater and warning side by side —
    -- the second cookie used to block the closing bracket, failing the whole
    -- timestamp and, behind a planning keyword, demoting the line to body.
  , ( "Repeater and warning together (test-org-element/timestamp-parser)"
    , "<2024-01-15 Mon +1m -3d>"
    , Just ((repeating (on "2024-01-15 00:00:00")
                       (TimestampRepeaterInterval Restart 1 Months TRSPlus))
              { tsWarning = Just (TimestampWarningInterval False 3 Days) }) )

  , ( "First-only delay (test-org-element/timestamp-parser)"
    , "<2024-01-15 Mon .+2d --7d>"
    , Just ((repeating (on "2024-01-15 00:00:00")
                       (TimestampRepeaterInterval Cumulative 2 Days TRSPlus))
              { tsWarning = Just (TimestampWarningInterval True 7 Days) }) )

    -- org-element reads the cookies in either order; the conventional
    -- repeater-then-warning is what a re-render spells.
  , ( "Warning written before the repeater", "<2024-01-15 Mon -3d +1m>"
    , Just ((repeating (on "2024-01-15 00:00:00")
                       (TimestampRepeaterInterval Restart 1 Months TRSPlus))
              { tsWarning = Just (TimestampWarningInterval False 3 Days) }) )

  , ( "Weekly restart repeater", "<2024-01-01 +1w>"
    , Just (repeating (on "2024-01-01 00:00:00")
                      (TimestampRepeaterInterval Restart 1 Weeks TRSPlus)) )

  , ( "Daily cumulative repeater", "<2024-01-01 .+3d>"
    , Just (repeating (on "2024-01-01 00:00:00")
                      (TimestampRepeaterInterval Cumulative 3 Days TRSPlus)) )

  , ( "Monthly catch-up repeater", "<2024-01-01 ++1m>"
    , Just (repeating (on "2024-01-01 00:00:00")
                      (TimestampRepeaterInterval CatchUp 1 Months TRSPlus)) )

  , ( "Yearly repeater", "<2024-01-01 +1y>"
    , Just (repeating (on "2024-01-01 00:00:00")
                      (TimestampRepeaterInterval Restart 1 Years TRSPlus)) )

  , ( "Repeater with weekday and time", "<2024-03-15 Fri 09:00 +2w>"
    , Just (repeating (at "2024-03-15 09:00:00")
                      (TimestampRepeaterInterval Restart 2 Weeks TRSPlus)) )
  ]

-- | Check that INPUT parses to EXPECTED, as the case named LABEL.
parseCase :: (String, Text, Maybe Timestamp) -> TestTree
parseCase (label, input, expected) =
  testCase label (assertEqual "" expected (parseTimestamp input))

spec :: TestTree
spec = testGroup "Timestamp"
  [ testGroup "Parses" (map parseCase parseCases)

  , testGroup "Ranges"
    -- A half not followed by a matching "--[" leaves tsEnd unset; the
    -- mismatched-bracket document itself fails to parse (see TestNegative).
    [ testCase "A lone half is not a range" $
        assertEqual "" (Just Nothing) (tsEnd <$> parseTimestamp "[2023-07-15 Sat 15:54] tail")

    , testCase "The -- spelling is kept, not folded into the compact one" $
        assertEqual "" (Just False)
          (tsCompactRange <$> parseTimestamp "[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]")
    ]

  , testGroup "Timestamp in headline title"
    [ testCase "Active timestamp in title" $
        withHeadline "* Meeting <2024-01-15 Mon 10:00>" $ \h ->
          assertBool "Title should contain timestamp" (hasTimestamp (title h))

    , testCase "Inactive timestamp in title" $
        withHeadline "* Created [2024-01-15 Mon 10:00]" $ \h ->
          assertBool "Title should contain timestamp" (hasTimestamp (title h))

    ]
  ]
  where
    hasTimestamp (Title xs) = any isTimestampElem xs
    isTimestampElem (OrgLineTimestamp _) = True
    isTimestampElem _ = False
