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
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Org.Types
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Void (Void)
import Text.Megaparsec (lookAhead, try, choice, eof, manyTill, takeWhile1P, (<|>), many, option, optional, some, ParseErrorBundle)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (eol, space, char)
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified TextShow as TS

-- Parser types

type OrgParser = Context -> Text -> OrgParserResult
type OrgParserResult = ([Spanned Element], Context, Maybe (ParseErrorBundle Text Void))
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
          bol <- startsLine <$> MP.takeWhileP (Just "white space") isSpace
          elems <- elementsP bol
          _ <- space <* eof
          return elems

-- Helpers

-- | Whitespace-separated elements, as 'MP.sepEndBy' would collect them.  BOL
-- says whether the next element starts a line; each separator recomputes it.
elementsP :: Bool -> StatefulParser [Spanned Element]
elementsP bol = option [] $ do
  el <- spannedP (elementP bol)
  rest <- option [] $ do
    gap <- takeWhile1P (Just "white space") isSpace
    elementsP (startsLine gap)
  return (el : rest)

-- | True when GAP leaves the parser at the start of a line.  An empty gap
-- means nothing was skipped, which only happens at offset 0.
startsLine :: Text -> Bool
startsLine gap = T.null gap || T.last gap == '\n'

-- | Pair what P parses with the span it consumed.  Offsets come from
-- 'MP.getOffset': character indices into the parsed text, half-open [start, end).
spannedP :: StatefulParser a -> StatefulParser (Spanned a)
spannedP p = do
  s <- MP.getOffset
  x <- p
  e <- MP.getOffset
  pure (Spanned (Span s e) x)

-- | Span from the first to the last of SPANS; Nothing when empty.
spanRange :: [Span] -> Maybe Span
spanRange = foldr step Nothing
  where step s Nothing     = Just s
        step s (Just rest) = Just (Span (spanStart s) (spanEnd rest))

via :: (Parse a) => (a -> b) -> StatefulParser b
via el = try (el <$> parse)

-- | Parse elements up to the end of the line.  Trailing horizontal space
-- terminates the container and stays unconsumed.
parseContainer :: (Parse element) => ([element] -> container) -> StatefulParser container
parseContainer con = do
    let stop = MPC.hspace *> (void eol <|> eof)
    con <$> manyTill (MPC.hspace *> parse) (lookAhead (try stop))

-- | Like 'parseContainer' but stopping at ENDPARSER, also yielding the span
-- from the first to the last element.  ENDPARSER comes first: it may claim the
-- horizontal space the end-of-line branch would otherwise swallow.
spannedContainerUntil :: (Parse element)
                      => ([element] -> container)
                      -> StatefulParser end
                      -> StatefulParser (Maybe Span, container)
spannedContainerUntil con endParser = do
    let stop = void (try endParser) <|> (MPC.hspace *> (void eol <|> eof))
    elems <- manyTill (MPC.hspace *> spannedP parse) (lookAhead (try stop))
    return (spanRange (map spanOf elems), con (map valueOf elems))

-- Parse instances

instance Parse Element where
  parse = elementP True

-- | Parse one element.  A headline is only tried when BOL: org anchors its
-- stars to column 1, so mid-line "*emphasis*" stays plain text.
elementP :: Bool -> StatefulParser Element
elementP bol = choice $
     [ try (EHeadline <$> (parse :: StatefulParser Headline)) | bol ]
  ++ [ try (EPragma <$> (parse :: StatefulParser Pragma))
     , try (ETimestamp <$> (parse :: StatefulParser Timestamp))
     , EToken <$> (parse :: StatefulParser Token)
     ]

instance Parse Headline where
  parse = do
    start <- MP.getOffset
    indent' <- indentP
    todo' <- optional $ try todoP
    priority' <- optional $ try priorityP
    (titleSpan, title') <- titleP
    (tagsSpan, tags') <- option (Nothing, mempty) $ try tagsP
    properties' <- optional $ try propertiesP

    let propsSpan = spanOf <$> properties'
        present = catMaybes [ propsSpan
                            , tagsSpan
                            , titleSpan
                            , spanOf <$> priority'
                            , spanOf <$> todo'
                            ]

        headline = Headline { indent = valueOf indent'
                            , todo = valueOf <$> todo'
                            , priority = valueOf <$> priority'
                            , title = title'
                            , tags = tags'
                            , properties = maybe mempty valueOf properties'
                            , schedule = Nothing
                            , deadline = Nothing
                            , refs = []
                            , hashRefs = []
                            , spans = HeadlineSpans
                                { hsFull = Span start (maximum (spanEnd (spanOf indent') : map spanEnd present))
                                , hsTodo = spanOf <$> todo'
                                , hsPriority = spanOf <$> priority'
                                , hsTitle = titleSpan
                                , hsTags = tagsSpan
                                , hsProperties = propsSpan
                                }
                            }

    State.modify $ registerHeadline headline

    return headline

instance Parse Indent where
  parse = valueOf <$> indentP

-- | Parse the stars, spanning them alone; the trailing space is still consumed.
indentP :: StatefulParser (Spanned Indent)
indentP = do
  s <- MP.getOffset
  stars <- MP.some (MPC.char '*')
  e <- MP.getOffset
  _ <- MPC.space
  return $ Spanned (Span s e) (Indent (length stars))

instance Parse Keyword where
  parse = Keyword . T.toUpper <$> keywordTextP

-- | Parse a bare keyword word, preserving the casing the source used.
keywordTextP :: StatefulParser Text
keywordTextP = T.pack <$> some (MP.satisfy (\c -> isAlpha c || c == '_'))

instance Parse Pragma where
  parse = do
    key@(Keyword kText) <- MPC.string "#+" *> parse <* MPC.char ':' <* MPC.space

    case kText of
      "CATEGORY" -> do
        cat <- parse :: StatefulParser OrgLine
        State.modify $ setCategory (TS.showt cat)
        return $ PCategory cat

      "TODO" -> do
        -- Keywords register as written: org matches them case-sensitively.
        let todoKw = keywordTextP
                     <* optional (MPC.char '(' *> MP.takeWhileP Nothing (/= ')') *> MPC.char ')')

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
  parse = valueOf <$> priorityP

-- | Parse "[#X]", spanning it alone; the trailing space is still consumed.
priorityP :: StatefulParser (Spanned Priority)
priorityP = do
  s <- MP.getOffset
  p <- MPC.char '[' *> MPC.char '#' *> MPC.letterChar <* MPC.char ']'
  e <- MP.getOffset
  _ <- MPC.space
  return $ Spanned (Span s e) (Priority p)

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
  parse = valueOf <$> propertiesP

-- | Parse a property drawer; the span starts at the drawer line, past the
-- leading eol, and ends right after ":END:".
propertiesP :: StatefulParser (Spanned Properties)
propertiesP = do
  _ <- MPC.eol
  s <- MP.getOffset
  _ <- MPC.hspace *> MPC.string ":PROPERTIES:" <* MPC.hspace <* MPC.eol
  ps <- MP.manyTill (MPC.hspace *> (parse :: StatefulParser Property) <* MPC.hspace <* MPC.eol)
                    (try (MPC.hspace *> MPC.string ":END:"))
  e <- MP.getOffset
  return $ Spanned (Span s e) (Properties ps)

instance Parse OrgLineElement where
  parse = via OrgLineTimestamp
    <|> (OrgLineToken <$> parse)

instance Parse OrgLine where
  parse = parseContainer OrgLine

instance Parse Tags where
  parse = snd <$> tagsP

-- | Parse ":a:b:", spanning the first through the last colon.  The span is
-- Nothing when no tag followed the opening colon.
tagsP :: StatefulParser (Maybe Span, Tags)
tagsP = do
    _ <- MPC.hspace1
    s <- MP.getOffset
    _ <- char ':'
    ts <- many tag
    e <- MP.getOffset
    return (if null ts then Nothing else Just (Span s e), Tags ts)
    where tag = takeWhile1P (Just "tag") isTagChar <* char ':'
          isTagChar c = isAlphaNum c || c == '_' || c == '-' || c == '@' || c == '#'

instance Parse Timestamp where
  parse = State.lift tsParser

instance Parse Title where
  parse = snd <$> titleP

-- | Parse a title, spanning its first through its last element.
titleP :: StatefulParser (Maybe Span, Title)
titleP = spannedContainerUntil Title stop
  where stop = parse :: StatefulParser Tags

instance Parse Todo where
  parse = valueOf <$> todoP

-- | Parse a todo keyword, spanning it alone; the trailing space is still
-- consumed.  The keyword must match a registered one exactly, case included.
todoP :: StatefulParser (Spanned Todo)
todoP = do
  ctx <- State.get
  s <- MP.getOffset
  result <- keywordTextP
  e <- MP.getOffset
  _ <- MPC.space
  guard $ inTodo result ctx
  return $ Spanned (Span s e) Todo { name = result
                                   , active = result `elem` todoActive ctx
                                   }

instance Parse Token where
  parse = Token <$> takeWhile1P (Just "token") (not . isSpace)

-- Timestamp sub-parsers

-- | Parse a timestamp, optionally a "start--end" range.  Both ends must use
-- the bracket kind the start opened with.
tsParser :: StatelessParser Timestamp
tsParser = do
  tsStatus <- tsStatusParser
  (tsStart, tsInterval) <- tsBodyParser tsStatus
  tsEnd <- MP.optional . MP.try $ do
    _ <- MPC.string "--" *> MPC.char (fst (tsBrackets tsStatus))
    fst <$> tsBodyParser tsStatus
  return (Timestamp {..})

-- | The brackets STATUS is written with: opening and closing.
tsBrackets :: TimestampStatus -> (Char, Char)
tsBrackets TimestampActive = ('<', '>')
tsBrackets TimestampInactive = ('[', ']')

tsStatusParser :: StatelessParser TimestampStatus
tsStatusParser = (TimestampActive <$ MPC.char '<')
             <|> (TimestampInactive <$ MPC.char '[')

-- | Parse one bracketed moment of STATUS, from the day through the closing
-- bracket, together with the repeater it carries.
tsBodyParser :: TimestampStatus -> StatelessParser (TsMoment, Maybe TimestampRepeaterInterval)
tsBodyParser status = do
  day <- tsDayParser <* MPC.space
  _weekday <- MP.optional (MP.try tsWeekdayParser) <* MPC.space
  time <- MP.optional (MP.try tsTimeParser) <* MPC.space
  interval <- MP.optional . MP.try $ tsRepeaterParser <* MPC.space
  void $ MPC.char (snd (tsBrackets status))
  let moment = TsMoment { tsmTime = Time.UTCTime day (Time.timeOfDayToTime (fromMaybe midnight time))
                        , tsmHasTime = isJust time
                        }
  return (moment, interval)
  where midnight = Time.TimeOfDay 0 0 0

tsDayParser :: StatelessParser Time.Day
tsDayParser = do
  let sep = '-'
  year <- MPL.decimal <* MPC.char sep
  month <- MPL.decimal <* MPC.char sep
  day <- MPL.decimal <* MPC.space
  guard (month >= 1 && month <= 12) <|> fail "Month out of range"
  guard (day >= 1 && day <= 31) <|> fail "Day out of range"
  return (Time.fromGregorian year month day)

-- | Parse a time of day, "HH:MM" with optional ":SS".
tsTimeParser :: StatelessParser Time.TimeOfDay
tsTimeParser = do
  let sep = ':'
  tsHour <- MPL.decimal <* MPC.char sep
  tsMinute <- MPL.decimal
  tsSecond <- MP.option 0 $ MPC.char sep *> MPL.decimal
  guard (tsHour <= 23 && tsMinute <= 59 && tsSecond < 60) <|> fail "Time out of range"
  return (Time.TimeOfDay tsHour tsMinute (fromInteger tsSecond))

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
