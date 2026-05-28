module TestTimestamp (spec) where

import Data.Org
import qualified Data.Org as Org
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool)

strptime :: Text -> UTCTime
strptime t = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack t) :: UTCTime

parseTimestamp :: Text -> Maybe Timestamp
parseTimestamp input = case orgParse mempty input of
  (ETimestamp ts : _, _, _) -> Just ts
  _                         -> Nothing

parseFails :: Text -> Bool
parseFails input = case orgParse mempty input of
  (ETimestamp _ : _, _, _) -> False
  _                        -> True

spec :: TestTree
spec = testGroup "Timestamp"
  [ testGroup "Active timestamps"
    [ testCase "Date only" $
        assertEqual "" (Just ts { tsStatus = TimestampActive, tsTime = strptime "2024-01-01 00:00:00" })
                       (parseTimestamp "<2024-01-01>")

    , testCase "Date with weekday" $
        assertEqual "" (Just ts { tsStatus = TimestampActive, tsTime = strptime "2024-01-01 00:00:00" })
                       (parseTimestamp "<2024-01-01 Mon>")

    , testCase "Date with time" $
        assertEqual "" (Just ts { tsStatus = TimestampActive, tsTime = strptime "2024-01-15 10:30:00" })
                       (parseTimestamp "<2024-01-15 Mon 10:30>")

    , testCase "Date with time and seconds" $
        assertEqual "" (Just ts { tsStatus = TimestampActive, tsTime = strptime "2024-01-15 10:30:45" })
                       (parseTimestamp "<2024-01-15 Mon 10:30:45>")
    ]

  , testGroup "Inactive timestamps"
    [ testCase "Date only" $
        assertEqual "" (Just ts { tsStatus = TimestampInactive, tsTime = strptime "2024-06-15 00:00:00" })
                       (parseTimestamp "[2024-06-15]")

    , testCase "Date with weekday" $
        assertEqual "" (Just ts { tsStatus = TimestampInactive, tsTime = strptime "2024-06-15 00:00:00" })
                       (parseTimestamp "[2024-06-15 Sat]")

    , testCase "Date with time" $
        assertEqual "" (Just ts { tsStatus = TimestampInactive, tsTime = strptime "2024-06-15 14:00:00" })
                       (parseTimestamp "[2024-06-15 Sat 14:00]")
    ]

  , testGroup "Repeater intervals"
    [ testCase "Weekly restart repeater" $
        assertEqual "" (Just ts { tsStatus = TimestampActive
                                , tsTime = strptime "2024-01-01 00:00:00"
                                , tsInterval = Just (TimestampRepeaterInterval Restart 1 Weeks TRSPlus) })
                       (parseTimestamp "<2024-01-01 +1w>")

    , testCase "Daily cumulative repeater" $
        assertEqual "" (Just ts { tsStatus = TimestampActive
                                , tsTime = strptime "2024-01-01 00:00:00"
                                , tsInterval = Just (TimestampRepeaterInterval Cumulative 3 Days TRSPlus) })
                       (parseTimestamp "<2024-01-01 .+3d>")

    , testCase "Monthly catch-up repeater" $
        assertEqual "" (Just ts { tsStatus = TimestampActive
                                , tsTime = strptime "2024-01-01 00:00:00"
                                , tsInterval = Just (TimestampRepeaterInterval CatchUp 1 Months TRSPlus) })
                       (parseTimestamp "<2024-01-01 ++1m>")

    , testCase "Yearly repeater" $
        assertEqual "" (Just ts { tsStatus = TimestampActive
                                , tsTime = strptime "2024-01-01 00:00:00"
                                , tsInterval = Just (TimestampRepeaterInterval Restart 1 Years TRSPlus) })
                       (parseTimestamp "<2024-01-01 +1y>")

    , testCase "Repeater with weekday and time" $
        assertEqual "" (Just ts { tsStatus = TimestampActive
                                , tsTime = strptime "2024-03-15 09:00:00"
                                , tsInterval = Just (TimestampRepeaterInterval Restart 2 Weeks TRSPlus) })
                       (parseTimestamp "<2024-03-15 Fri 09:00 +2w>")
    ]

  , testGroup "Timestamp in headline title"
    [ testCase "Active timestamp in title" $ do
        let (elems, _, _) = orgParse mempty "* Meeting <2024-01-15 Mon 10:00>"
        case elems of
          [EHeadline h] -> assertBool "Title should contain timestamp"
            (case title h of
               Title xs -> any isTimestampElem xs)
          _ -> assertBool "Expected single headline" False

    , testCase "Inactive timestamp in title" $ do
        let (elems, _, _) = orgParse mempty "* Created [2024-01-15 Mon 10:00]"
        case elems of
          [EHeadline h] -> assertBool "Title should contain timestamp"
            (case title h of
               Title xs -> any isTimestampElem xs)
          _ -> assertBool "Expected single headline" False
    ]

  , testGroup "Invalid timestamps"
    [ testCase "Invalid month" $
        assertBool "Should not parse as timestamp" (parseFails "<2024-13-01>")

    , testCase "Invalid day" $
        assertBool "Should not parse as timestamp" (parseFails "<2024-01-32>")
    ]
  ]
  where
    ts = Timestamp { tsStatus = TimestampActive, tsInterval = Nothing, tsTime = strptime "2000-01-01 00:00:00" }
    isTimestampElem (OrgLineTimestamp _) = True
    isTimestampElem _ = False
