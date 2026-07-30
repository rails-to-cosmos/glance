module TestTimestamp (spec) where

import Data.Org
import qualified Data.Org as Org
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool)
import qualified TextShow as TS

strptime :: Text -> UTCTime
strptime t = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack t) :: UTCTime

-- | The moment T names, as a source that spelled a time of day.
at :: Text -> TsMoment
at t = TsMoment (strptime t) True

-- | The moment T names, as a date-only source.
on :: Text -> TsMoment
on t = TsMoment (strptime t) False

parseTimestamp :: Text -> Maybe Timestamp
parseTimestamp input = case orgParse mempty input of
  (Spanned _ (ETimestamp ts) : _, _, _) -> Just ts
  _                                     -> Nothing

parseFails :: Text -> Bool
parseFails input = case orgParse mempty input of
  (Spanned _ (ETimestamp _) : _, _, _) -> False
  _                                    -> True

spec :: TestTree
spec = testGroup "Timestamp"
  [ testGroup "Active timestamps"
    [ testCase "Date only" $
        assertEqual "" (Just ts { tsStatus = TimestampActive, tsStart = on "2024-01-01 00:00:00" })
                       (parseTimestamp "<2024-01-01>")

    , testCase "Date with weekday" $
        assertEqual "" (Just ts { tsStatus = TimestampActive, tsStart = on "2024-01-01 00:00:00" })
                       (parseTimestamp "<2024-01-01 Mon>")

    , testCase "Date with time" $
        assertEqual "" (Just ts { tsStatus = TimestampActive, tsStart = at "2024-01-15 10:30:00" })
                       (parseTimestamp "<2024-01-15 Mon 10:30>")

    , testCase "Date with time and seconds" $
        assertEqual "" (Just ts { tsStatus = TimestampActive, tsStart = at "2024-01-15 10:30:45" })
                       (parseTimestamp "<2024-01-15 Mon 10:30:45>")
    ]

  , testGroup "Inactive timestamps"
    [ testCase "Date only" $
        assertEqual "" (Just ts { tsStatus = TimestampInactive, tsStart = on "2024-06-15 00:00:00" })
                       (parseTimestamp "[2024-06-15]")

    , testCase "Date with weekday" $
        assertEqual "" (Just ts { tsStatus = TimestampInactive, tsStart = on "2024-06-15 00:00:00" })
                       (parseTimestamp "[2024-06-15 Sat]")

    , testCase "Date with time" $
        assertEqual "" (Just ts { tsStatus = TimestampInactive, tsStart = at "2024-06-15 14:00:00" })
                       (parseTimestamp "[2024-06-15 Sat 14:00]")
    ]

  , testGroup "Ranges"
    [ testCase "Inactive range" $
        assertEqual "" (Just ts { tsStatus = TimestampInactive
                                , tsStart = at "2023-07-15 15:54:00"
                                , tsEnd = Just (at "2023-07-15 17:10:00") })
                       (parseTimestamp "[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]")

    , testCase "Active range" $
        assertEqual "" (Just ts { tsStatus = TimestampActive
                                , tsStart = on "2024-01-15 00:00:00"
                                , tsEnd = Just (on "2024-01-19 00:00:00") })
                       (parseTimestamp "<2024-01-15 Mon>--<2024-01-19 Fri>")

    , testCase "Range renders both halves" $
        assertEqual "" (Just "[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]")
                       (fmap TS.showt (parseTimestamp "[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]"))

    , testCase "Mismatched brackets do not form a range" $
        assertBool "Halves must share a bracket kind"
                   (parseFails "[2023-07-15 Sat 15:54]--<2023-07-15 Sat 17:10>")

    , testCase "CLOCK line parses as elements" $ do
        let input = T.intercalate "\n"
              [ "* Task"
              , "CLOCK: [2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10] =>  1:16" ]
        case orgParse mempty input of
          (_, _, Just _err) -> assertBool "CLOCK line should parse" False
          (elems, _, Nothing) -> do
            let ranges = [ (s, t) | Spanned s (ETimestamp t) <- elems, isJust (tsEnd t) ]
            assertEqual "one range" 1 (length ranges)
            assertEqual "range slices back"
                        ["[2023-07-15 Sat 15:54]--[2023-07-15 Sat 17:10]"]
                        [ sliceSpan input s | (s, _) <- ranges ]
    ]

  , testGroup "Repeater intervals"
    [ testCase "Weekly restart repeater" $
        assertEqual "" (Just ts { tsStatus = TimestampActive
                                , tsStart = on "2024-01-01 00:00:00"
                                , tsInterval = Just (TimestampRepeaterInterval Restart 1 Weeks TRSPlus) })
                       (parseTimestamp "<2024-01-01 +1w>")

    , testCase "Daily cumulative repeater" $
        assertEqual "" (Just ts { tsStatus = TimestampActive
                                , tsStart = on "2024-01-01 00:00:00"
                                , tsInterval = Just (TimestampRepeaterInterval Cumulative 3 Days TRSPlus) })
                       (parseTimestamp "<2024-01-01 .+3d>")

    , testCase "Monthly catch-up repeater" $
        assertEqual "" (Just ts { tsStatus = TimestampActive
                                , tsStart = on "2024-01-01 00:00:00"
                                , tsInterval = Just (TimestampRepeaterInterval CatchUp 1 Months TRSPlus) })
                       (parseTimestamp "<2024-01-01 ++1m>")

    , testCase "Yearly repeater" $
        assertEqual "" (Just ts { tsStatus = TimestampActive
                                , tsStart = on "2024-01-01 00:00:00"
                                , tsInterval = Just (TimestampRepeaterInterval Restart 1 Years TRSPlus) })
                       (parseTimestamp "<2024-01-01 +1y>")

    , testCase "Repeater with weekday and time" $
        assertEqual "" (Just ts { tsStatus = TimestampActive
                                , tsStart = at "2024-03-15 09:00:00"
                                , tsInterval = Just (TimestampRepeaterInterval Restart 2 Weeks TRSPlus) })
                       (parseTimestamp "<2024-03-15 Fri 09:00 +2w>")
    ]

  , testGroup "Rendering"
    [ testCase "Date-only keeps its date" $
        assertEqual "" (Just "<2026-07-08 Wed>")
                       (fmap TS.showt (parseTimestamp "<2026-07-08 Wed>"))

    , testCase "Time of day survives" $
        assertEqual "" (Just "<2024-01-15 Mon 10:30>")
                       (fmap TS.showt (parseTimestamp "<2024-01-15 Mon 10:30>"))

    , testCase "Midnight spelled out stays spelled out" $
        assertEqual "" (Just "[2024-01-01 Mon 00:00]")
                       (fmap TS.showt (parseTimestamp "[2024-01-01 Mon 00:00]"))
    ]

  , testGroup "Timestamp in headline title"
    [ testCase "Active timestamp in title" $ do
        let (elems, _, _) = orgParse mempty "* Meeting <2024-01-15 Mon 10:00>"
        case map valueOf elems of
          [EHeadline h] -> assertBool "Title should contain timestamp"
            (case title h of
               Title xs -> any isTimestampElem xs)
          _ -> assertBool "Expected single headline" False

    , testCase "Inactive timestamp in title" $ do
        let (elems, _, _) = orgParse mempty "* Created [2024-01-15 Mon 10:00]"
        case map valueOf elems of
          [EHeadline h] -> assertBool "Title should contain timestamp"
            (case title h of
               Title xs -> any isTimestampElem xs)
          _ -> assertBool "Expected single headline" False

    , testCase "Date-only timestamp in title renders without a time" $ do
        let (elems, _, _) = orgParse mempty "* Due <2026-07-08 Wed>"
        case map valueOf elems of
          [EHeadline h] -> assertEqual "" "* Due <2026-07-08 Wed>" (TS.showt h)
          _ -> assertBool "Expected single headline" False
    ]

  , testGroup "Invalid timestamps"
    [ testCase "Invalid month" $
        assertBool "Should not parse as timestamp" (parseFails "<2024-13-01>")

    , testCase "Invalid day" $
        assertBool "Should not parse as timestamp" (parseFails "<2024-01-32>")

    , testCase "Invalid hour" $
        assertBool "Should not parse as timestamp" (parseFails "<2024-01-01 Mon 25:00>")
    ]
  ]
  where
    ts = Timestamp { tsStatus = TimestampActive
                   , tsInterval = Nothing
                   , tsStart = on "2000-01-01 00:00:00"
                   , tsEnd = Nothing }
    isTimestampElem (OrgLineTimestamp _) = True
    isTimestampElem _ = False
