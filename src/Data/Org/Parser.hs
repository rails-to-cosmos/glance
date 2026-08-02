module Data.Org.Parser ( Parse (..)
                       , StatefulParser
                       , StatelessParser
                       , OrgParser
                       , OrgParserResult
                       , isTagChar
                       , orgParse
                       ) where

import Control.Monad (void, guard, when)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust)
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

-- | Like 'spannedP', also consuming the whitespace trailing P.  The span still
-- covers P alone.
lexemeP :: StatefulParser a -> StatefulParser (Spanned a)
lexemeP p = spannedP p <* MPC.space

-- | Span from the first to the last of SPANS; Nothing when empty.  Forced at
-- every step: a thunk chain here would outlive the document it points into.
spanRange :: [Span] -> Maybe Span
spanRange = foldl' (\acc sp -> Just $! maybe sp (<> sp) acc) Nothing

-- | Parse elements until ENDPARSER or the end of the line, yielding the span
-- from the first to the last one.  ENDPARSER comes first: it may claim the
-- horizontal space the end-of-line branch would otherwise swallow.  Trailing
-- horizontal space terminates the container and stays unconsumed.
spannedContainerUntil :: (Parse element)
                      => ([element] -> container)
                      -> StatefulParser end
                      -> StatefulParser (Maybe Span, container)
spannedContainerUntil con endParser = do
    let stop = void (try endParser) <|> (MPC.hspace *> (void eol <|> eof))
    elems <- manyTill (MPC.hspace *> spannedP parse) (lookAhead (try stop))
    return (spanRange (map spanOf elems), con (map valueOf elems))

-- Parse instances

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
    indent' <- indentP
    todo' <- optional $ try todoP
    priority' <- optional $ try priorityP
    (titleSpan, title') <- titleP
    (tagsSpan, tags') <- option (Nothing, mempty) $ try tagsP
    planning' <- option noPlanning $ try planningP
    properties' <- optional $ try propertiesP

    let headline = Headline { indent = valueOf indent'
                            , todo = valueOf <$> todo'
                            , priority = valueOf <$> priority'
                            , title = title'
                            , tags = tags'
                            , properties = maybe mempty valueOf properties'
                            , schedule = valueOf <$> plScheduled planning'
                            , deadline = valueOf <$> plDeadline planning'
                            , closed = valueOf <$> plClosed planning'
                            , spans = HeadlineSpans
                                { hsStars = spanOf indent'
                                , hsTodo = spanOf <$> todo'
                                , hsPriority = spanOf <$> priority'
                                , hsTitle = titleSpan
                                , hsTags = tagsSpan
                                , hsSchedule = spanOf <$> plScheduled planning'
                                , hsDeadline = spanOf <$> plDeadline planning'
                                , hsClosed = spanOf <$> plClosed planning'
                                , hsProperties = spanOf <$> properties'
                                }
                            }

    State.modify $ registerHeadline headline

    return headline

-- | Parse the stars, spanning them alone; the horizontal space after them is
-- consumed, and something has to end the run.
--
-- Org's own rule — @org-outline-regexp@ is @\\*+ @ — so a star run opens a
-- headline only when whitespace follows it.  @*bold*@ opening a body line is
-- emphasis, and reading it as a headline put body text in the table as a row of
-- its own.  The end of the line ends the run too, which leaves a bare star run
-- the empty headline this has always read it as.
--
-- Horizontal space alone, never 'MPC.space': eating the newline let a headline
-- with an empty title run on into the next line and take it — stars included —
-- as its own title, so @* @ above @* Delta@ parsed as ONE headline titled
-- @* Delta@.
indentP :: StatefulParser (Spanned Indent)
indentP = spannedP (Indent . length <$> MP.some (MPC.char '*'))
          <* (void MPC.hspace1 <|> lookAhead (void eol <|> eof))

instance Parse Keyword where
  parse = Keyword . T.toUpper <$> keywordTextP

-- | Parse a bare keyword word, preserving the casing the source used.
keywordTextP :: StatefulParser Text
keywordTextP = T.pack <$> some (MP.satisfy (\c -> isAlpha c || c == '_'))

instance Parse Pragma where
  parse = do
    key@(Keyword kText) <- MPC.string "#+" *> parse <* MPC.char ':' <* MPC.space

    case kText of
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
        when (kText == "CATEGORY") $ State.modify $ setCategory $ TS.showt val
        return $ Pragma key val

-- | The keywords a planning line may carry.
data PlanningKeyword = PlanScheduled | PlanDeadline | PlanClosed
  deriving (Enum, Bounded)

-- | The text org writes KW with, colon included.  Uppercase only: emacs never
-- writes another casing, and folding would swallow prose starting with
-- "closed:".
planningText :: PlanningKeyword -> Text
planningText PlanScheduled = "SCHEDULED:"
planningText PlanDeadline = "DEADLINE:"
planningText PlanClosed = "CLOSED:"

-- | What a planning line yielded: at most one timestamp per keyword.
data Planning = Planning { plScheduled :: !(Maybe (Spanned Timestamp))
                         , plDeadline  :: !(Maybe (Spanned Timestamp))
                         , plClosed    :: !(Maybe (Spanned Timestamp))
                         }

noPlanning :: Planning
noPlanning = Planning Nothing Nothing Nothing

-- | Parse the planning line: the one line right after a headline's title line,
-- holding SCHEDULED:, DEADLINE: and CLOSED: in any order.  A keyword repeated
-- on the line keeps its last timestamp, which is how org reads one.  Each span
-- covers the timestamp alone, so rescheduling replaces exactly that text.
--
-- The leading eol is consumed and the line's own eol is left to 'propertiesP',
-- so a drawer follows either spelling.  A failed entry backtracks over the
-- horizontal space it skipped, leaving it to separate whatever trails the last
-- timestamp: the top loop requires whitespace between elements, and eating it
-- here would fail the whole document.
planningP :: StatefulParser Planning
planningP = foldl' assign noPlanning <$> (MPC.eol *> some (try entryP))
  where entryP = do
          kw <- MPC.hspace *> choice [k <$ MPC.string (planningText k) | k <- [minBound ..]]
          ts <- MPC.hspace1 *> spannedP (parse :: StatefulParser Timestamp)
          return (kw, ts)

        assign pl (PlanScheduled, ts) = pl { plScheduled = Just ts }
        assign pl (PlanDeadline, ts) = pl { plDeadline = Just ts }
        assign pl (PlanClosed, ts) = pl { plClosed = Just ts }

-- | Parse "[#X]", spanning it alone; the trailing space is still consumed.
priorityP :: StatefulParser (Spanned Priority)
priorityP = lexemeP (Priority <$> (MPC.char '[' *> MPC.char '#' *> MPC.letterChar <* MPC.char ']'))

instance Parse Property where
  parse = do
    keyword <- MPC.char ':' *> propertyKeyP <* MPC.char ':' <* MPC.space
    guard $ not (reserved keyword)
    value <- parse :: StatefulParser OrgLine
    when (keyword == Keyword "CATEGORY") $ State.modify $ setCategory $ TS.showt value
    return $ Property keyword value
    where reserved :: Keyword -> Bool
          reserved (Keyword k) = k `elem` ["PROPERTIES", "END"]

-- | A property KEY is org's own rule — any run without whitespace or a
-- colon (`:TELE2:`, `:ЖКХ:`, `:FOO-BAR:`) — uppercased like every pragma
-- key.  Deliberately WIDER than 'keywordTextP': the TODO-keyword charset
-- (letters and underscores) is the wall that keeps a starred meta
-- undeclarable, and it must not widen with this one.
propertyKeyP :: StatefulParser Keyword
propertyKeyP = Keyword . T.toUpper . T.pack
           <$> some (MP.satisfy (\c -> not (isSpace c) && c /= ':'))

-- | Parse a property drawer; the span starts at the drawer line, past the
-- leading eol, and ends right after ":END:".
propertiesP :: StatefulParser (Spanned Properties)
propertiesP = MPC.eol *> spannedP drawer
  where drawer = do
          _ <- MPC.hspace *> MPC.string ":PROPERTIES:" <* MPC.hspace <* MPC.eol
          ps <- MP.manyTill (MPC.hspace *> (parse :: StatefulParser Property) <* MPC.hspace <* MPC.eol)
                            (try (MPC.hspace *> MPC.string ":END:"))
          return (Properties ps)

instance Parse OrgLineElement where
  parse = try (OrgLineTimestamp <$> parse)
      <|> (OrgLineToken <$> parse)

instance Parse OrgLine where
  parse = snd <$> spannedContainerUntil OrgLine (MP.empty :: StatefulParser ())

instance Parse Tags where
  parse = snd <$> tagsP

-- | Parse ":a:b:", spanning the first through the last colon.  The span is
-- Nothing when no tag followed the opening colon.
tagsP :: StatefulParser (Maybe Span, Tags)
tagsP = do
    _ <- MPC.hspace1
    Spanned sp ts <- spannedP (char ':' *> many tag)
    return (if null ts then Nothing else Just sp, Tags ts)
    where tag = takeWhile1P (Just "tag") isTagChar <* char ':'

-- | Is C a character an org tag is spelled with?  A non-empty run of these
-- between two colons is a tag and nothing else is, so this predicate is the
-- whole grammar of one — which is why it is exported: a command layer WRITING a
-- tag has to write something this parser reads back, and re-spelling the set
-- there would be a second grammar to keep in step ('Glance.Query.tagText').
--
-- Org's own @org-tag-re@ is @[[:alnum:]_@@#%]+@, so this set differs from it in
-- two characters: @-@ is here and not there, @%@ is there and not here.  The
-- divergence predates the write path and the corpus depends on the first half
-- of it.
isTagChar :: Char -> Bool
isTagChar c = isAlphaNum c || c == '_' || c == '-' || c == '@' || c == '#'

instance Parse Timestamp where
  parse = State.lift tsParser

-- | Parse a title, spanning its first through its last element.
titleP :: StatefulParser (Maybe Span, Title)
titleP = spannedContainerUntil Title tagsP

-- | Parse a todo keyword, spanning it alone; the trailing space is still
-- consumed.  The keyword must match a registered one exactly, case included.
todoP :: StatefulParser (Spanned Todo)
todoP = do
  ctx <- State.get
  Spanned sp result <- lexemeP keywordTextP
  guard $ inTodo result ctx
  return $ Spanned sp Todo { name = result
                           , active = result `elem` todoActive ctx
                           }

instance Parse Token where
  parse = Token <$> takeWhile1P (Just "token") (not . isSpace)

-- Timestamp sub-parsers

-- | Parse a timestamp, optionally a range.  Org spells a range either as
-- "start--end" over two bracket pairs, both of the kind the start opened with,
-- or compactly as one pair holding "HH:MM-HH:MM" on a single day;
-- 'tsCompactRange' records which the source used.  A "--" half wins when some
-- source carries both, the model having room for one end.
tsParser :: StatelessParser Timestamp
tsParser = do
  tsStatus <- tsStatusParser
  (tsStart, compactEnd, tsInterval) <- tsBodyParser tsStatus
  dashEnd <- MP.optional . MP.try $ do
    _ <- MPC.string "--" *> MPC.char (fst (tsBrackets tsStatus))
    (moment, _compact, _interval) <- tsBodyParser tsStatus
    return moment
  let (tsEnd, tsCompactRange) = case dashEnd of
        Just moment -> (Just moment, False)
        Nothing     -> (compactEnd, isJust compactEnd)
  return (Timestamp {..})

tsStatusParser :: StatelessParser TimestampStatus
tsStatusParser = (TimestampActive <$ MPC.char '<')
             <|> (TimestampInactive <$ MPC.char '[')

-- | Parse one bracketed moment of STATUS, from the day through the closing
-- bracket: the moment, the compact range end it may carry, and its repeater.
tsBodyParser :: TimestampStatus -> StatelessParser (TsMoment, Maybe TsMoment, Maybe TimestampRepeaterInterval)
tsBodyParser status = do
  day <- tsDayParser <* MPC.space
  void $ MP.optional (MP.try tsWeekdayParser) <* MPC.space
  time <- MP.optional (MP.try tsTimeParser)
  -- A range end and a repeater both open with '-', so the end time is tried
  -- first and only its colon tells them apart: "-1d" gets through 'MPL.decimal'
  -- and fails at the missing ':', backtracking whole and leaving the repeater
  -- its text.  No space may sit around the '-' — org writes none, and allowing
  -- it would let " -1d" read as a range end.
  endTime <- if isJust time
             then MP.optional (MP.try (MPC.char '-' *> tsTimeParser))
             else return Nothing
  MPC.space
  interval <- MP.optional . MP.try $ tsRepeaterParser <* MPC.space
  void $ MPC.char (snd (tsBrackets status))
  let atTime hasTime t = TsMoment { tsmTime = Time.UTCTime day (Time.timeOfDayToTime t)
                                  , tsmHasTime = hasTime }
  return ( atTime (isJust time) (fromMaybe midnight time)
         , atTime True <$> endTime
         , interval )
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

-- | Skip the weekday: a run of letters in any script, of any length.  The slot
-- is display-only — every render recomputes the weekday from the date — so its
-- spelling is read and dropped, and a locale's word is as good as org's own.
-- ~/sync writes Dutch @do@, @ma@, @zo@, @vr@ and @za@ beside English @Mon@, and
-- exactly three letters lost the whole planning line to them.  Letters alone:
-- a repeater opens with @.@, @+@, @-@ or a digit and a time with a digit, so
-- one letter is what holds @.+3d@ out of this slot.  The trailing dot French
-- and Catalan abbreviate with (@lun.@) is outside the rule and outside the
-- corpus; admitting it needs a guard against @.+3d@ that nothing here yet owes.
tsWeekdayParser :: StatelessParser ()
tsWeekdayParser = void (takeWhile1P (Just "weekday") isAlpha) <* MPC.space

-- | Parse a repeater such as ".+3d", spelled with the characters
-- 'typeChar', 'signChar' and 'unitChar' name.
tsRepeaterParser :: StatelessParser TimestampRepeaterInterval
tsRepeaterParser = do
  repType <- MP.optional . MP.try $ choice [t <$ char c | t <- [minBound ..], Just c <- [typeChar t]]
  repSign <- MP.optional . MP.try $ choice [s <$ char (signChar s) | s <- [minBound ..]]
  repValue <- MPL.decimal
  repUnit <- choice [u <$ char (unitChar u) | u <- [minBound ..]]

  return TimestampRepeaterInterval {
    repeaterValue = repValue,
    repeaterType = case (repType, repSign) of
                     (Just Cumulative, _)         -> Cumulative
                     (Just CatchUp, Just TRSPlus) -> CatchUp
                     _type                        -> Restart,
    repeaterUnit = repUnit,
    repeaterSign = fromMaybe TRSPlus repSign
    }
