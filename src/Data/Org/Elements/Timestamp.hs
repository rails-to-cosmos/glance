module Data.Org.Elements.Timestamp ( Timestamp (..)
                                   , TimestampStatus (..)
                                   , TimestampRepeaterInterval (..)
                                   , TimestampRepeaterType (..) ) where

import Data.Org.Parse

import Data.Org.Identity (Identity)
import Data.Org.Identity qualified as Identity

import Data.Text (Text, pack)
import Data.Time qualified as Time
import Data.Maybe (fromMaybe)

import Control.Monad.State qualified as State

import TextShow (TextShow)
import TextShow qualified as TS

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

import Control.Monad (guard, void)

data Timestamp = Timestamp
  { timestampStatus :: !TimestampStatus
  , timestampInterval :: !(Maybe TimestampRepeaterInterval)
  , timestampTime :: !Time.UTCTime
  } deriving (Show, Eq)

data TimestampStatus = TimestampActive | TimestampInactive
  deriving (Show, Eq)

data TimestampRepeaterInterval = TimestampRepeaterInterval
  { repeaterType :: !TimestampRepeaterType
  , repeaterValue :: !Int
  , repeaterUnit :: !TimestampUnit
  , repeaterSign :: !TimestampRepeaterSign
  } deriving (Show, Eq)

data TimestampRepeaterSign = TRSPlus | TRSMinus
  deriving (Show, Eq)

data TimestampRepeaterType = CatchUp | Restart | Cumulative
  deriving (Show, Eq)

data TimestampUnit = Days | Weeks | Months | Years
  deriving (Show, Eq)

instance TextShow Timestamp where
  showb ts =
    openBracket
    <> TS.fromText timeText
    <> TS.fromText repeaterSeparator
    <> TS.fromText repeaterText
    <> closeBracket

    where
      openBracket = case timestampStatus ts of
        TimestampActive -> "<"
        TimestampInactive -> "["
      closeBracket = case timestampStatus ts of
        TimestampActive -> ">"
        TimestampInactive -> "]"
      timeText = formatTimestamp (timestampTime ts)
      repeaterTypeText = case timestampInterval ts of
        Nothing -> ""
        Just TimestampRepeaterInterval { repeaterType = Restart } -> ""
        Just TimestampRepeaterInterval { repeaterType = Cumulative } -> "."
        Just TimestampRepeaterInterval { repeaterType = CatchUp } -> "+"
      repeaterSignText = case timestampInterval ts of
        Nothing -> ""
        Just TimestampRepeaterInterval { repeaterSign = TRSPlus } -> "+"
        Just TimestampRepeaterInterval { repeaterSign = TRSMinus } -> "-"
      repeaterUnitText = case timestampInterval ts of
        Nothing -> ""
        Just TimestampRepeaterInterval { repeaterUnit = Days } -> "d"
        Just TimestampRepeaterInterval { repeaterUnit = Weeks } -> "w"
        Just TimestampRepeaterInterval { repeaterUnit = Months } -> "m"
        Just TimestampRepeaterInterval { repeaterUnit = Years } -> "y"
      repeaterValText = case timestampInterval ts of
        Nothing -> ""
        Just TimestampRepeaterInterval { repeaterValue = val } -> TS.showt val
      repeaterText = repeaterTypeText <> repeaterSignText <> repeaterValText <> repeaterUnitText
      repeaterSeparator = case repeaterText of
        "" -> ""
        _repeater -> " "

instance Parse Timestamp where
  parse = do
    timestampStatus' <- State.lift timestampStatusParser
    tsDay' <- State.lift timestampDayParser <* space
    _tsWeekday' <- optional $ State.lift timestampWeekdayParser <* space
    timestampTime' <- optional $ State.lift timestampTimeParser <* space
    timestampIntervaleaterInterval' <- optional . try $ State.lift timestampRepeaterParser <* space
    void $ char $ case timestampStatus' of
      TimestampActive -> '>'
      TimestampInactive -> ']'

    return Timestamp
      { timestampStatus = timestampStatus'
      , timestampInterval = timestampIntervaleaterInterval'
      , timestampTime = case timestampTime' of
                   Just t -> Time.UTCTime tsDay' (Time.timeOfDayToTime t)
                   Nothing -> Time.UTCTime tsDay' (Time.timeOfDayToTime (Time.TimeOfDay 0 0 0)) }

formatTimestamp :: Time.UTCTime -> Text
formatTimestamp ts = pack (Time.formatTime Time.defaultTimeLocale timeFormat ts)
  where timeFormat = if (seconds::Integer) `mod` 60 == 0
                     then "%Y-%m-%d %a %H:%M"
                     else "%Y-%m-%d %a %H:%M:%S"
        seconds = floor $ Time.utctDayTime ts

timestampCtrl :: StatelessParser Char
timestampCtrl = char '<' <|> char '['

timestampStatusParser :: StatelessParser TimestampStatus
timestampStatusParser = do
  ctrl <- timestampCtrl
  case ctrl of
    '<' -> return TimestampActive
    '[' -> return TimestampInactive
    _ctrl -> return TimestampInactive

timestampDayParser :: StatelessParser Time.Day
timestampDayParser = do
  let sep = '-'
  year <- decimal <* char sep
  month <- decimal <* char sep
  day <- decimal <* space
  guard (month >= 1 && month <= 12) <|> fail "Month out of range"
  guard (day >= 1 && day <= 31) <|> fail "Day out of range"
  return (Time.fromGregorian year month day)

timestampTimeParser :: StatelessParser Time.TimeOfDay
timestampTimeParser = do
  let sep = ':'
  tsHour <- optional . try $ decimal <* char sep
  tsMinute <- optional . try $ decimal
  tsSecond <- optional . try $ char sep *> decimal <* space
  return (Time.TimeOfDay
          (fromMaybe 0 tsHour)
          (fromMaybe 0 tsMinute)
          (fromMaybe 0 tsSecond))

timestampWeekdayParser :: StatelessParser Text
timestampWeekdayParser = do
  weekday <- count 3 letterChar
  space
  return (pack weekday)

timestampRepeaterParser :: StatelessParser TimestampRepeaterInterval
timestampRepeaterParser = do
  repType <- optional . try $ oneOf ['.', '+']
  repSign <- optional . try $ oneOf ['+', '-']
  repValue <- decimal
  repUnit <- oneOf ['d', 'w', 'm', 'y']

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
