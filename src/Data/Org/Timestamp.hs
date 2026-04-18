{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Org.Timestamp where

import Control.Monad (void, guard)
import Control.Monad.State (lift)
import Data.Maybe (fromMaybe)
import Data.Org.Base
import Data.Org.Context
import Data.Text (Text)
import Text.Megaparsec ((<|>))
import Text.Megaparsec.Char (char, space)
import TextShow (TextShow, showt, showb, fromText)

import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL

data Timestamp = Timestamp { status :: !TimestampStatus
                           , interval :: !(Maybe TimestampRepeaterInterval)
                           , time :: !Time.UTCTime
                           } deriving (Show, Eq)

instance Ord Timestamp where
  compare a b = compare (time a) (time b)

instance Identity Timestamp where
  identity = Just . showt

instance TextShow Timestamp where
  showb ts = openBracket
    <> fromText timeText
    <> fromText repeaterSeparator
    <> fromText repeaterText
    <> closeBracket

    where openBracket = case status ts of
            TimestampActive -> "<"
            TimestampInactive -> "["
          closeBracket = case status ts of
            TimestampActive -> ">"
            TimestampInactive -> "]"
          timeText = format (time ts)
          repeaterTypeText = case interval ts of
            Nothing -> ""
            Just TimestampRepeaterInterval { repeaterType = Restart } -> ""
            Just TimestampRepeaterInterval { repeaterType = Cumulative } -> "."
            Just TimestampRepeaterInterval { repeaterType = CatchUp } -> "+"
          repeaterSignText = case interval ts of
            Nothing -> ""
            Just TimestampRepeaterInterval { repeaterSign = TRSPlus } -> "+"
            Just TimestampRepeaterInterval { repeaterSign = TRSMinus } -> "-"
          repeaterUnitText = case interval ts of
            Nothing -> ""
            Just TimestampRepeaterInterval { repeaterUnit = Days } -> "d"
            Just TimestampRepeaterInterval { repeaterUnit = Weeks } -> "w"
            Just TimestampRepeaterInterval { repeaterUnit = Months } -> "m"
            Just TimestampRepeaterInterval { repeaterUnit = Years } -> "y"
          repeaterValText = case interval ts of
            Nothing -> ""
            Just TimestampRepeaterInterval { repeaterValue = val } -> showt val
          repeaterText = repeaterTypeText <> repeaterSignText <> repeaterValText <> repeaterUnitText
          repeaterSeparator = case repeaterText of
            "" -> ""
            _repeater -> " "

instance Display Timestamp where
  display = showt

instance Parse Context Timestamp where
  parse = do
    status <- parse
    (Day day) <- parse <* space
    maybeWeekday <- MP.optional (parse :: StatefulParser Context Weekday)
    _ <- space
    maybeTime <- MP.optional parse
    _ <- space
    interval <- MP.optional . MP.try $ parse <* space

    void $ char $ case status of
      TimestampActive -> '>'
      TimestampInactive -> ']'

    let time = case maybeTime of
          Just (TimeOfDay t) -> Time.UTCTime day (Time.timeOfDayToTime t)
          Nothing -> Time.UTCTime day (Time.timeOfDayToTime (Time.TimeOfDay 0 0 0))


    return (Timestamp {..})

data TimestampStatus = TimestampActive | TimestampInactive
  deriving (Show, Eq)

instance Parse Context TimestampStatus where
  parse = do
    controlChar <- MPC.char '<' <|> MPC.char '['
    case controlChar of
      '<' -> return TimestampActive
      '[' -> return TimestampInactive
      _ -> return TimestampInactive

data TimestampRepeaterInterval = TimestampRepeaterInterval
  { repeaterType :: !TimestampRepeaterType
  , repeaterValue :: !Int
  , repeaterUnit :: !TimestampUnit
  , repeaterSign :: !TimestampRepeaterSign
  } deriving (Show, Eq)

instance Parse Context TimestampRepeaterInterval where
  parse = do
    repType <- MP.optional . MP.try $ MP.oneOf ['.', '+']
    repSign <- MP.optional . MP.try $ MP.oneOf ['+', '-']
    repValue <- MPL.decimal
    repUnit <- MP.oneOf ['d', 'w', 'm', 'y']

    return TimestampRepeaterInterval {
      repeaterValue = repValue,
      repeaterType = case repType of
                       Just '.' -> Cumulative
                       Just '+' | repSign == Just '+' -> CatchUp
                       _type -> Restart,
      repeaterUnit = case repUnit of
                       'd'   -> Days
                       'w'   -> Weeks
                       'm'   -> Months
                       'y'   -> Years
                       _unit -> Days,
      repeaterSign = case repSign of
                       Just '-' -> TRSMinus
                       _sign -> TRSPlus
      }


data TimestampRepeaterSign = TRSPlus | TRSMinus
  deriving (Show, Eq)

data TimestampRepeaterType = CatchUp | Restart | Cumulative
  deriving (Show, Eq)

data TimestampUnit = Days | Weeks | Months | Years
  deriving (Show, Eq)

newtype Day = Day Time.Day

instance Parse Context Day where
  parse = do
    let sep = '-'
    year <- MPL.decimal <* MPC.char sep
    month <- MPL.decimal <* MPC.char sep
    day <- MPL.decimal <* MPC.space
    guard (month >= 1 && month <= 12) <|> fail "Month out of range"
    guard (day >= 1 && day <= 31) <|> fail "Day out of range"
    return $ Day $ Time.fromGregorian year month day

newtype TimeOfDay = TimeOfDay Time.TimeOfDay

instance Parse Context TimeOfDay where
  parse = do
    let sep = ':'
    tsHour <- MP.optional . MP.try $ MPL.decimal <* MPC.char sep
    tsMinute <- MP.optional . MP.try $ MPL.decimal
    tsSecond <- MP.optional . MP.try $ MPC.char sep *> MPL.decimal <* MPC.space
    let h = (fromMaybe 0 tsHour)
    let m = (fromMaybe 0 tsMinute)
    let s = (fromMaybe 0 tsSecond)
    return $ TimeOfDay (Time.TimeOfDay h m s)

newtype Weekday = Weekday Text

instance Parse Context Weekday where
  parse = do
    weekday <- MP.count 3 MPC.letterChar
    _ <- MPC.space
    return $ Weekday (T.pack weekday)

format :: Time.UTCTime -> Text
format ts = T.pack (Time.formatTime Time.defaultTimeLocale timeFormat ts)
  where timeFormat = "%Y-%m-%d %a %H:%M:%S"
