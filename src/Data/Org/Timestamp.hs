{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Timestamp ( OrgTimestamp (..)
                          , timestampCtrl
                          , OrgTimestampType (..)
                          , OrgTimestampStatus (..)
                          , OrgTimestampRepeaterInterval (..)
                          , OrgTimestampRepeaterType (..)
  ) where

import Data.Org.Element
import Data.Org.Context

import Data.Text (Text, pack)
import Data.Time qualified as Time
import Data.Maybe (fromMaybe)

import TextShow

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

import Control.Monad (guard, void)

data OrgTimestamp = OrgTimestamp
  { tsType :: OrgTimestampType
  , tsStatus :: OrgTimestampStatus
  , tsRep :: Maybe OrgTimestampRepeaterInterval
  , tsTime :: Time.UTCTime
  } deriving (Show, Eq)

data OrgTimestampType = TsScheduled | TsDeadline | TsClosed | TsArbitrary
  deriving (Show, Eq)

data OrgTimestampStatus = TsActive | TsInactive
  deriving (Show, Eq)

data OrgTimestampRepeaterInterval =
  OrgTimestampRepeaterInterval
  { repeaterType :: OrgTimestampRepeaterType
  , repeaterValue :: Int
  , repeaterUnit :: OrgTimestampUnit
  , repeaterSign :: OrgTimestampRepeaterSign
  } deriving (Show, Eq)

data OrgTimestampRepeaterSign = TRSPlus | TRSMinus
  deriving (Show, Eq)

data OrgTimestampRepeaterType = CatchUp | Restart | Cumulative
  deriving (Show, Eq)

data OrgTimestampUnit = Days | Weeks | Months | Years
  deriving (Show, Eq)

instance TextShow OrgTimestamp where
  showb ts =
    typeText
    <> openBracket
    <> fromText timeText
    <> fromText repeaterSeparator
    <> fromText repeaterText
    <> closeBracket

    where
      typeText = case tsType ts of
        TsScheduled -> "SCHEDULED: "
        TsDeadline -> "DEADLINE: "
        TsClosed -> "CLOSED: "
        TsArbitrary -> ""
      openBracket = case tsStatus ts of
        TsActive -> "<"
        TsInactive -> "["
      closeBracket = case tsStatus ts of
        TsActive -> ">"
        TsInactive -> "]"
      timeText = formatTimestamp (tsTime ts)
      repeaterTypeText = case tsRep ts of
        Nothing -> ""
        Just OrgTimestampRepeaterInterval { repeaterType = Restart } -> ""
        Just OrgTimestampRepeaterInterval { repeaterType = Cumulative } -> "."
        Just OrgTimestampRepeaterInterval { repeaterType = CatchUp } -> "+"
      repeaterSignText = case tsRep ts of
        Nothing -> ""
        Just OrgTimestampRepeaterInterval { repeaterSign = TRSPlus } -> "+"
        Just OrgTimestampRepeaterInterval { repeaterSign = TRSMinus } -> "-"
      repeaterUnitText = case tsRep ts of
        Nothing -> ""
        Just OrgTimestampRepeaterInterval { repeaterUnit = Days } -> "d"
        Just OrgTimestampRepeaterInterval { repeaterUnit = Weeks } -> "w"
        Just OrgTimestampRepeaterInterval { repeaterUnit = Months } -> "m"
        Just OrgTimestampRepeaterInterval { repeaterUnit = Years } -> "y"
      repeaterValText = case tsRep ts of
        Nothing -> ""
        Just OrgTimestampRepeaterInterval { repeaterValue = val } -> showt val
      repeaterText = repeaterTypeText <> repeaterSignText <> repeaterValText <> repeaterUnitText
      repeaterSeparator = case repeaterText of
        "" -> ""
        _ -> " "

instance OrgElement OrgTimestamp where

  parser _ = do
    tsType' <- optional . try $ timestampTypeParser
    tsStatus' <- timestampStatusParser
    tsDay' <- timestampDayParser
    space
    void $ optional timestampWeekdayParser -- weekday
    space
    tsTime' <- optional . try $ timestampTimeParser
    space
    tsRepeaterInterval' <- optional . try $ timestampRepeaterParser
    void $ char $ case tsStatus' of
      TsActive -> '>'
      TsInactive -> ']'

    return $ OrgTimestamp
      { tsType = fromMaybe TsArbitrary tsType'
      , tsStatus = tsStatus'
      , tsRep = tsRepeaterInterval'
      , tsTime = case tsTime' of
                   Just t -> Time.UTCTime tsDay' (Time.timeOfDayToTime t)
                   Nothing -> Time.UTCTime tsDay' (Time.timeOfDayToTime (Time.TimeOfDay 0 0 0))
      }

  modifyState (OrgTimestamp {tsTime = t}) ctx = ctx -- {metaTime = metaTime ctx ++ [t]}

formatTimestamp :: Time.UTCTime -> Text
formatTimestamp ts = pack (Time.formatTime Time.defaultTimeLocale timeFormat ts)
  where timeFormat = if (seconds::Integer) `mod` 60 == 0
                     then "%Y-%m-%d %a %H:%M"
                     else "%Y-%m-%d %a %H:%M:%S"
        seconds = floor $ Time.utctDayTime ts

timestampCtrl :: Parser Char
timestampCtrl = char '<' <|> char '['

timestampStatusParser :: Parser OrgTimestampStatus
timestampStatusParser = do
  ctrl <- timestampCtrl
  case ctrl of
    '<' -> return TsActive
    '[' -> return TsInactive
    _ -> return TsInactive

timestampTypeParser :: Parser OrgTimestampType
timestampTypeParser = do
  ctrl <- manyTill anySingle (lookAhead $ void timestampCtrl <|> space1 <|> eof)
  space
  case ctrl of
    "SCHEDULED:" -> return TsScheduled
    "DEADLINE:" -> return TsDeadline
    "CLOSED:" -> return TsClosed
    _ -> fail "Expected control timestamp text (either SCHEDULED:, DEADLINE: or CLOSED:), but got nothing"

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
  space1
  return (pack weekday)

timestampRepeaterParser :: Parser OrgTimestampRepeaterInterval
timestampRepeaterParser = do
  type' <- optional . try $ oneOf ['.', '+']
  sign' <- optional . try $ oneOf ['+', '-']

  val' <- decimal
  unit' <- oneOf ['d', 'w', 'm', 'y']

  return OrgTimestampRepeaterInterval {
    repeaterType = case type' of
        Nothing -> Restart
        Just '.' -> Cumulative
        Just '+' | sign' == Just '+' -> CatchUp
        _ -> Restart,
    repeaterUnit = case unit' of
        'd' -> Days
        'w' -> Weeks
        'm' -> Months
        'y' -> Years
        _   -> Days,
    repeaterValue = val',
    repeaterSign = case sign' of
        Just '-' -> TRSMinus
        _ -> TRSPlus
    }
