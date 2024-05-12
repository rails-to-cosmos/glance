module Data.Org.Timestamp
  ( Timestamp (..)
  , timestampCtrl
  , TimestampStatus (..)
  , TimestampRepeaterInterval (..)
  , TimestampRepeaterType (..)
  ) where

import Data.Org.Element

import Data.Text (Text, pack)
import Data.Time qualified as Time
import Data.Maybe (fromMaybe)

import Control.Monad.State qualified as State

import TextShow

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

import Control.Monad (guard, void)

data Timestamp = Timestamp
  { tsStatus :: !TimestampStatus
  , tsRep :: !(Maybe TimestampRepeaterInterval)
  , tsTime :: !Time.UTCTime
  } deriving (Show, Eq)

data TimestampStatus = TsActive | TsInactive
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
    <> fromText timeText
    <> fromText repeaterSeparator
    <> fromText repeaterText
    <> closeBracket

    where
      openBracket = case tsStatus ts of
        TsActive -> "<"
        TsInactive -> "["
      closeBracket = case tsStatus ts of
        TsActive -> ">"
        TsInactive -> "]"
      timeText = formatTimestamp (tsTime ts)
      repeaterTypeText = case tsRep ts of
        Nothing -> ""
        Just TimestampRepeaterInterval { repeaterType = Restart } -> ""
        Just TimestampRepeaterInterval { repeaterType = Cumulative } -> "."
        Just TimestampRepeaterInterval { repeaterType = CatchUp } -> "+"
      repeaterSignText = case tsRep ts of
        Nothing -> ""
        Just TimestampRepeaterInterval { repeaterSign = TRSPlus } -> "+"
        Just TimestampRepeaterInterval { repeaterSign = TRSMinus } -> "-"
      repeaterUnitText = case tsRep ts of
        Nothing -> ""
        Just TimestampRepeaterInterval { repeaterUnit = Days } -> "d"
        Just TimestampRepeaterInterval { repeaterUnit = Weeks } -> "w"
        Just TimestampRepeaterInterval { repeaterUnit = Months } -> "m"
        Just TimestampRepeaterInterval { repeaterUnit = Years } -> "y"
      repeaterValText = case tsRep ts of
        Nothing -> ""
        Just TimestampRepeaterInterval { repeaterValue = val } -> showt val
      repeaterText = repeaterTypeText <> repeaterSignText <> repeaterValText <> repeaterUnitText
      repeaterSeparator = case repeaterText of
        "" -> ""
        _repeater -> " "

instance OrgElement Timestamp where
  parser = do
    tsStatus' <- State.lift timestampStatusParser
    tsDay' <- State.lift timestampDayParser
    space
    void $ optional $ State.lift timestampWeekdayParser -- weekday
    space
    tsTime' <- optional . try $ State.lift timestampTimeParser
    space
    tsRepeaterInterval' <- optional . try $ State.lift timestampRepeaterParser
    space
    void $ char $ case tsStatus' of
      TsActive -> '>'
      TsInactive -> ']'

    return Timestamp
      { tsStatus = tsStatus'
      , tsRep = tsRepeaterInterval'
      , tsTime = case tsTime' of
                   Just t -> Time.UTCTime tsDay' (Time.timeOfDayToTime t)
                   Nothing -> Time.UTCTime tsDay' (Time.timeOfDayToTime (Time.TimeOfDay 0 0 0)) }

formatTimestamp :: Time.UTCTime -> Text
formatTimestamp ts = pack (Time.formatTime Time.defaultTimeLocale timeFormat ts)
  where timeFormat = if (seconds::Integer) `mod` 60 == 0
                     then "%Y-%m-%d %a %H:%M"
                     else "%Y-%m-%d %a %H:%M:%S"
        seconds = floor $ Time.utctDayTime ts

timestampCtrl :: Parser Char
timestampCtrl = char '<' <|> char '['

timestampStatusParser :: Parser TimestampStatus
timestampStatusParser = do
  ctrl <- timestampCtrl
  case ctrl of
    '<' -> return TsActive
    '[' -> return TsInactive
    _ctrl -> return TsInactive

timestampDayParser :: Parser Time.Day
timestampDayParser = do
  let sep = '-'
  year <- decimal <* char sep
  month <- decimal <* char sep
  day <- decimal <* space
  guard (month >= 1 && month <= 12) <|> fail "Month out of range"
  guard (day >= 1 && day <= 31) <|> fail "Day out of range"
  return (Time.fromGregorian year month day)

timestampTimeParser :: Parser Time.TimeOfDay
timestampTimeParser = do
  let sep = ':'
  tsHour <- optional . try $ decimal <* char sep
  tsMinute <- optional . try $ decimal
  tsSecond <- optional . try $ char sep *> decimal <* space
  return (Time.TimeOfDay
          (fromMaybe 0 tsHour)
          (fromMaybe 0 tsMinute)
          (fromMaybe 0 tsSecond))

timestampWeekdayParser :: Parser Text
timestampWeekdayParser = do
  weekday <- count 3 letterChar
  space
  return (pack weekday)

timestampRepeaterParser :: Parser TimestampRepeaterInterval
timestampRepeaterParser = do
  type' <- optional . try $ oneOf ['.', '+']
  sign' <- optional . try $ oneOf ['+', '-']

  val' <- decimal
  unit' <- oneOf ['d', 'w', 'm', 'y']

  return TimestampRepeaterInterval {
    repeaterType = case type' of
        Nothing -> Restart
        Just '.' -> Cumulative
        Just '+' | sign' == Just '+' -> CatchUp
        _type -> Restart,
    repeaterUnit = case unit' of
        'd' -> Days
        'w' -> Weeks
        'm' -> Months
        'y' -> Years
        _   -> Days,
    repeaterValue = val',
    repeaterSign = case sign' of
        Just '-' -> TRSMinus
        _sign -> TRSPlus
    }
