module Data.Org.Timestamp ( Ts (..)
                          , TsStatus (..)
                          , TsRepeaterInterval (..)
                          , TsRepeaterType (..) ) where

import Data.Org.Base qualified as Org

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

data Ts = Ts
  { tsStatus :: !TsStatus
  , tsRep :: !(Maybe TsRepeaterInterval)
  , tsTime :: !Time.UTCTime
  } deriving (Show, Eq)

data TsStatus = TsActive | TsInactive
  deriving (Show, Eq)

data TsRepeaterInterval = TsRepeaterInterval
  { repeaterType :: !TsRepeaterType
  , repeaterValue :: !Int
  , repeaterUnit :: !TsUnit
  , repeaterSign :: !TsRepeaterSign
  } deriving (Show, Eq)

data TsRepeaterSign = TRSPlus | TRSMinus
  deriving (Show, Eq)

data TsRepeaterType = CatchUp | Restart | Cumulative
  deriving (Show, Eq)

data TsUnit = Days | Weeks | Months | Years
  deriving (Show, Eq)

instance TextShow Ts where
  showb ts =
    openBracket
    <> TS.fromText timeText
    <> TS.fromText repeaterSep
    <> TS.fromText repeaterText
    <> closeBracket

    where
      openBracket = case tsStatus ts of
        TsActive -> "<"
        TsInactive -> "["
      closeBracket = case tsStatus ts of
        TsActive -> ">"
        TsInactive -> "]"
      timeText = formatTs (tsTime ts)
      repeaterTypeText = case tsRep ts of
        Nothing -> ""
        Just TsRepeaterInterval { repeaterType = Restart } -> ""
        Just TsRepeaterInterval { repeaterType = Cumulative } -> "."
        Just TsRepeaterInterval { repeaterType = CatchUp } -> "+"
      repeaterSignText = case tsRep ts of
        Nothing -> ""
        Just TsRepeaterInterval { repeaterSign = TRSPlus } -> "+"
        Just TsRepeaterInterval { repeaterSign = TRSMinus } -> "-"
      repeaterUnitText = case tsRep ts of
        Nothing -> ""
        Just TsRepeaterInterval { repeaterUnit = Days } -> "d"
        Just TsRepeaterInterval { repeaterUnit = Weeks } -> "w"
        Just TsRepeaterInterval { repeaterUnit = Months } -> "m"
        Just TsRepeaterInterval { repeaterUnit = Years } -> "y"
      repeaterValText = case tsRep ts of
        Nothing -> ""
        Just TsRepeaterInterval { repeaterValue = val } -> TS.showt val
      repeaterText = repeaterTypeText <> repeaterSignText <> repeaterValText <> repeaterUnitText
      repeaterSep = case repeaterText of
        "" -> ""
        _repeater -> " "

instance Org.Base Ts where
  parser = do
    tsStatus' <- State.lift timestampStatusParser
    tsDay' <- State.lift timestampDayParser <* space
    _tsWeekday' <- optional $ State.lift timestampWeekdayParser <* space
    tsTime' <- optional $ State.lift timestampTimeParser <* space
    tsRepeaterInterval' <- optional . try $ State.lift timestampRepeaterParser <* space
    void $ char $ case tsStatus' of
      TsActive -> '>'
      TsInactive -> ']'

    return Ts
      { tsStatus = tsStatus'
      , tsRep = tsRepeaterInterval'
      , tsTime = case tsTime' of
                   Just t -> Time.UTCTime tsDay' (Time.timeOfDayToTime t)
                   Nothing -> Time.UTCTime tsDay' (Time.timeOfDayToTime (Time.TimeOfDay 0 0 0)) }

formatTs :: Time.UTCTime -> Text
formatTs ts = pack (Time.formatTime Time.defaultTimeLocale timeFormat ts)
  where timeFormat = if (seconds::Integer) `mod` 60 == 0
                     then "%Y-%m-%d %a %H:%M"
                     else "%Y-%m-%d %a %H:%M:%S"
        seconds = floor $ Time.utctDayTime ts

timestampCtrl :: Org.StatelessParser Char
timestampCtrl = char '<' <|> char '['

timestampStatusParser :: Org.StatelessParser TsStatus
timestampStatusParser = do
  ctrl <- timestampCtrl
  case ctrl of
    '<' -> return TsActive
    '[' -> return TsInactive
    _ctrl -> return TsInactive

timestampDayParser :: Org.StatelessParser Time.Day
timestampDayParser = do
  let sep = '-'
  year <- decimal <* char sep
  month <- decimal <* char sep
  day <- decimal <* space
  guard (month >= 1 && month <= 12) <|> fail "Month out of range"
  guard (day >= 1 && day <= 31) <|> fail "Day out of range"
  return (Time.fromGregorian year month day)

timestampTimeParser :: Org.StatelessParser Time.TimeOfDay
timestampTimeParser = do
  let sep = ':'
  tsHour <- optional . try $ decimal <* char sep
  tsMinute <- optional . try $ decimal
  tsSecond <- optional . try $ char sep *> decimal <* space
  return (Time.TimeOfDay
          (fromMaybe 0 tsHour)
          (fromMaybe 0 tsMinute)
          (fromMaybe 0 tsSecond))

timestampWeekdayParser :: Org.StatelessParser Text
timestampWeekdayParser = do
  weekday <- count 3 letterChar
  space
  return (pack weekday)

timestampRepeaterParser :: Org.StatelessParser TsRepeaterInterval
timestampRepeaterParser = do
  type' <- optional . try $ oneOf ['.', '+']
  sign' <- optional . try $ oneOf ['+', '-']

  val' <- decimal
  unit' <- oneOf ['d', 'w', 'm', 'y']

  return TsRepeaterInterval {
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
