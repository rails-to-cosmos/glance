module TestTimestamp (spec) where

import Data.Org
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestDefaults
import qualified TextShow as TS

parseTimestamp :: Text -> Maybe Timestamp
parseTimestamp input = case orgParse defaultContext input of
  (Spanned _ (ETimestamp ts) : _, _, _) -> Just ts
  _                                     -> Nothing

-- | MOMENT repeating by INTERVAL, as an active timestamp.
repeating :: TsMoment -> TimestampRepeaterInterval -> Timestamp
repeating moment interval = (plainTs TimestampActive moment) { tsInterval = Just interval }

spec :: TestTree
spec = testGroup "Timestamp"
  [ testGroup "Active timestamps"
    [ testCase "Date with time" $
        assertEqual "" (Just (plainTs TimestampActive (at "2024-01-15 10:30:00")))
                       (parseTimestamp "<2024-01-15 Mon 10:30>")

    , testCase "Date with time and seconds" $
        assertEqual "" (Just (plainTs TimestampActive (at "2024-01-15 10:30:45")))
                       (parseTimestamp "<2024-01-15 Mon 10:30:45>")
    ]

  , testGroup "Inactive timestamps"
    [ testCase "Date only" $
        assertEqual "" (Just (plainTs TimestampInactive (on "2024-06-15 00:00:00")))
                       (parseTimestamp "[2024-06-15]")

    , testCase "Date with weekday" $
        assertEqual "" (Just (plainTs TimestampInactive (on "2024-06-15 00:00:00")))
                       (parseTimestamp "[2024-06-15 Sat]")

    , testCase "Date with time" $
        assertEqual "" (Just (plainTs TimestampInactive (at "2024-06-15 14:00:00")))
                       (parseTimestamp "[2024-06-15 Sat 14:00]")
    ]

  , testGroup "Ranges"
    [ testCase "Active range" $
        assertEqual "" (Just (plainTs TimestampActive (on "2024-01-15 00:00:00"))
                               { tsEnd = Just (on "2024-01-19 00:00:00") })
                       (parseTimestamp "<2024-01-15 Mon>--<2024-01-19 Fri>")

    , testCase "Range renders both halves" $
        assertEqual "" (Just "[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]")
                       (fmap TS.showt (parseTimestamp "[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]"))

    -- A half not followed by a matching "--[" leaves tsEnd unset; the
    -- mismatched-bracket document itself fails to parse (see TestNegative).
    , testCase "A lone half is not a range" $
        assertEqual "" (Just Nothing) (tsEnd <$> parseTimestamp "[2023-07-15 Sat 15:54] tail")
    ]

  , testGroup "Repeater intervals"
    [ testCase "Weekly restart repeater" $
        assertEqual "" (Just (repeating (on "2024-01-01 00:00:00")
                                        (TimestampRepeaterInterval Restart 1 Weeks TRSPlus)))
                       (parseTimestamp "<2024-01-01 +1w>")

    , testCase "Daily cumulative repeater" $
        assertEqual "" (Just (repeating (on "2024-01-01 00:00:00")
                                        (TimestampRepeaterInterval Cumulative 3 Days TRSPlus)))
                       (parseTimestamp "<2024-01-01 .+3d>")

    , testCase "Monthly catch-up repeater" $
        assertEqual "" (Just (repeating (on "2024-01-01 00:00:00")
                                        (TimestampRepeaterInterval CatchUp 1 Months TRSPlus)))
                       (parseTimestamp "<2024-01-01 ++1m>")

    , testCase "Yearly repeater" $
        assertEqual "" (Just (repeating (on "2024-01-01 00:00:00")
                                        (TimestampRepeaterInterval Restart 1 Years TRSPlus)))
                       (parseTimestamp "<2024-01-01 +1y>")

    , testCase "Repeater with weekday and time" $
        assertEqual "" (Just (repeating (at "2024-03-15 09:00:00")
                                        (TimestampRepeaterInterval Restart 2 Weeks TRSPlus)))
                       (parseTimestamp "<2024-03-15 Fri 09:00 +2w>")
    ]

  , testGroup "Timestamp in headline title"
    [ testCase "Active timestamp in title" $
        withHeadline "* Meeting <2024-01-15 Mon 10:00>" $ \h ->
          assertBool "Title should contain timestamp" (hasTimestamp (title h))

    , testCase "Inactive timestamp in title" $
        withHeadline "* Created [2024-01-15 Mon 10:00]" $ \h ->
          assertBool "Title should contain timestamp" (hasTimestamp (title h))

    , testCase "Date-only timestamp in title renders without a time" $
        withHeadline "* Due <2026-07-08 Wed>" $ \h ->
          assertEqual "" "* Due <2026-07-08 Wed>" (TS.showt h)
    ]
  ]
  where
    hasTimestamp (Title xs) = any isTimestampElem xs
    isTimestampElem (OrgLineTimestamp _) = True
    isTimestampElem _ = False
