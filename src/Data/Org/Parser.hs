module Data.Org.Parser ( Parse (..)
                       , StatefulParser
                       , StatelessParser
                       , OrgParser
                       , OrgParserResult
                       , orgParse
                       ) where

import Control.Monad (void, guard)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Maybe (fromMaybe)
import Data.Org.Types
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Void (Void)
import Text.Megaparsec (lookAhead, try, choice, eof, manyTill, takeWhile1P, (<|>), many, ParseErrorBundle)
import qualified Text.Megaparsec as MP
import Text.Megaparsec (option, optional, some)
import Text.Megaparsec.Char (eol, space1, space, char)
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified TextShow as TS

-- Parser types

type OrgParser = Context -> Text -> OrgParserResult
type OrgParserResult = ([Element], Context, Maybe (ParseErrorBundle Text Void))
type StatefulParser a = StateT Context StatelessParser a
type StatelessParser = MP.Parsec Void Text

class Parse a where
  parse :: StatefulParser a

-- Public API

orgParse :: OrgParser
orgParse st cmd = case MP.parse sfParser "" cmd of
  Right (elems, finalSt) -> (elems, finalSt, Nothing)
  Left err               -> ([], st, Just err)
  where sfParser = parser.runStateT st
        parser = do
          _ <- space
          elems <- parse `MP.sepEndBy` space1
          _ <- space <* eof
          return elems

-- Helpers

via :: (Parse a) => (a -> b) -> StatefulParser b
via el = try (el <$> parse)

parseContainer :: (Parse element) => ([element] -> container) -> StatefulParser container
parseContainer con = do
    let stop = void eol <|> eof
    con <$> manyTill (MPC.hspace *> parse) (lookAhead stop)

parseContainerUntil :: (Parse element)
            => ([element] -> container)
            -> StatefulParser end
            -> StatefulParser container
parseContainerUntil con endParser = do
    let stop = void eol <|> void (try endParser) <|> eof
    elems <- manyTill (MPC.hspace *> parse) (lookAhead (try stop))
    return $ con elems

-- Parse instances

instance Parse Element where
  parse = choice
    [ try (EHeadline <$> (parse :: StatefulParser Headline))
    , try (EPragma <$> (parse :: StatefulParser Pragma))
    , try (ETimestamp <$> (parse :: StatefulParser Timestamp))
    , EToken <$> (parse :: StatefulParser Token)
    ]

instance Parse Headline where
  parse = do
    indent' <- parse
    todo' <- optional $ try parse
    priority' <- optional $ try parse
    title' <- parse
    tags' <- option mempty $ try parse
    properties' <- option mempty $ try parse

    let headline = Headline { indent = indent'
                            , todo = todo'
                            , priority = priority'
                            , title = title'
                            , tags = tags'
                            , properties = properties'
                            , schedule = Nothing
                            , deadline = Nothing
                            , refs = []
                            , hashRefs = []
                            }

    State.modify $ registerHeadline headline

    return headline

instance Parse Indent where
  parse = do
    stars <- MP.some (MPC.char '*') <* MPC.space
    return $ Indent (length stars)

instance Parse Keyword where
  parse = do
    let keyword = some (MP.satisfy (\c -> isAlpha c || c == '_'))
    Keyword . T.toUpper . T.pack <$> keyword

instance Parse Pragma where
  parse = do
    key@(Keyword kText) <- MPC.string "#+" *> parse <* MPC.char ':' <* MPC.space

    case kText of
      "CATEGORY" -> do
        cat <- parse :: StatefulParser OrgLine
        State.modify $ setCategory (TS.showt cat)
        return $ PCategory cat

      "TODO" -> do
        let todoKw = do
              Keyword k <- parse
              void $ optional (MPC.char '(' *> MP.takeWhileP Nothing (/= ')') *> MPC.char ')')
              return k

        active   <- todoKw `MP.sepEndBy` MPC.hspace1
        inactive <- MP.option [] $ do
                      void $ MPC.char '|' <* MPC.hspace
                      todoKw `MP.sepEndBy` MPC.hspace1

        let (sActive, sInactive) = (Set.fromList active, Set.fromList inactive)
        State.modify (setTodo sActive sInactive)
        return $ PTodo sActive sInactive

      _ -> do
        val <- parse :: StatefulParser OrgLine
        return $ Pragma key val

instance Parse Priority where
  parse = do
    p <- MPC.char '[' *> MPC.char '#' *> MPC.letterChar <* MPC.char ']' <* MPC.space
    return (Priority p)

instance Parse Property where
  parse = do
    keyword <- MPC.char ':' *> (parse :: StatefulParser Keyword) <* MPC.char ':' <* MPC.space
    guard $ not (reserved keyword)
    value <- parse :: StatefulParser OrgLine

    case keyword of
      Keyword "CATEGORY" -> State.modify $ setCategory $ TS.showt value
      _keyword -> State.modify id

    return $ Property keyword value
    where reserved :: Keyword -> Bool
          reserved (Keyword k) = k `elem` ["PROPERTIES", "END"]

instance Parse Properties where
  parse = do
    _ <- MPC.eol
    _ <- MPC.hspace *> MPC.string ":PROPERTIES:" <* MPC.eol
    ps <- MP.manyTill (MPC.hspace *> (parse :: StatefulParser Property) <* MPC.eol) (MPC.hspace *> MPC.string ":END:")
    return (Properties ps)

instance Parse OrgLineElement where
  parse = via OrgLineTimestamp
    <|> (OrgLineToken <$> parse)

instance Parse OrgLine where
  parse = parseContainer OrgLine

instance Parse Tags where
  parse = do
    _ <- MPC.hspace1
    _ <- char ':'
    Tags <$> many tag
    where tag = takeWhile1P (Just "tag") isTagChar <* char ':'
          isTagChar c = isAlphaNum c || c == '_' || c == '-' || c == '@' || c == '#'

instance Parse Timestamp where
  parse = do
    tsStatus <- State.lift tsStatusParser
    tsDay <- State.lift tsDayParser <* MPC.space
    _tsWeekday' <- MP.optional $ State.lift tsWeekdayParser <* MPC.space
    tsTime' <- MP.optional $ State.lift tsTimeParser <* MPC.space
    tsInterval <- MP.optional . MP.try $ State.lift tsRepeaterParser <* MPC.space

    void $ MPC.char $ case tsStatus of
      TimestampActive -> '>'
      TimestampInactive -> ']'

    let tsTime = case tsTime' of
          Just t -> Time.UTCTime tsDay (Time.timeOfDayToTime t)
          Nothing -> Time.UTCTime tsDay (Time.timeOfDayToTime (Time.TimeOfDay 0 0 0))

    return (Timestamp {..})

instance Parse Title where
  parse = parseContainerUntil Title stop
    where stop = parse :: StatefulParser Tags

instance Parse Todo where
  parse = do
    ctx <- State.get
    Keyword result <- (parse :: StatefulParser Keyword) <* MPC.space
    guard $ inTodo result ctx
    return Todo { name = result
                , active = result `elem` todoActive ctx
                }

instance Parse Token where
  parse = Token <$> takeWhile1P (Just "token") (not . isSpace)

-- Timestamp sub-parsers

tsStatusParser :: StatelessParser TimestampStatus
tsStatusParser = do
  ctrl <- MPC.char '<' <|> MPC.char '['
  case ctrl of
    '<' -> return TimestampActive
    '[' -> return TimestampInactive
    _   -> return TimestampInactive

tsDayParser :: StatelessParser Time.Day
tsDayParser = do
  let sep = '-'
  year <- MPL.decimal <* MPC.char sep
  month <- MPL.decimal <* MPC.char sep
  day <- MPL.decimal <* MPC.space
  guard (month >= 1 && month <= 12) <|> fail "Month out of range"
  guard (day >= 1 && day <= 31) <|> fail "Day out of range"
  return (Time.fromGregorian year month day)

tsTimeParser :: StatelessParser Time.TimeOfDay
tsTimeParser = do
  let sep = ':'
  tsHour <- MP.optional . MP.try $ MPL.decimal <* MPC.char sep
  tsMinute <- MP.optional . MP.try $ MPL.decimal
  tsSecond <- MP.optional . MP.try $ MPC.char sep *> MPL.decimal <* MPC.space
  return (Time.TimeOfDay
          (fromMaybe 0 tsHour)
          (fromMaybe 0 tsMinute)
          (fromMaybe 0 tsSecond))

tsWeekdayParser :: StatelessParser Text
tsWeekdayParser = do
  weekday <- MP.count 3 MPC.letterChar
  MPC.space
  return (T.pack weekday)

tsRepeaterParser :: StatelessParser TimestampRepeaterInterval
tsRepeaterParser = do
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
