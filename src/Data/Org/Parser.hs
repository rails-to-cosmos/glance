module Data.Org.Parser ( Parse (..)
                       , isKeywordChar
                       , isTagChar
                       , orgParse
                       ) where

import Control.Monad (void, guard, when)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust, listToMaybe)
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


type OrgParser = Context -> Text -> OrgParserResult
type OrgParserResult = ([Spanned Element], Context, Maybe (ParseErrorBundle Text Void))
type StatefulParser a = StateT Context StatelessParser a
type StatelessParser = MP.Parsec Void Text

class Parse a where
  parse :: StatefulParser a


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


elementsP :: Bool -> StatefulParser [Spanned Element]
elementsP bol = option [] $ do
  el <- spannedP (elementP bol)
  rest <- option [] (spacedP <|> abuttingP el)
  return (el : rest)
  where
    spacedP = do
      gap <- takeWhile1P (Just "white space") isSpace
      elementsP (startsLine gap)

    -- A timestamp closes on its OWN bracket, so prose abuts it with nothing
    -- between: @[2026-08-03 Mon].@ is a timestamp and the token @.@.  ONLY a
    -- timestamp: a token already ran to the next space, and a HEADLINE that
    -- stops mid-line is corrupt org (@test/fixtures/broken/broken.org@), which
    -- must stay a parse failure.  NEVER at BOL — an abutting run is mid-line,
    -- so no @*@ behind a timestamp opens a headline.
    abuttingP (Spanned _ (ETimestamp _)) = elementsP False
    abuttingP _                          = MP.empty

startsLine :: Text -> Bool
startsLine gap = T.null gap || T.last gap == '\n'

spannedP :: StatefulParser a -> StatefulParser (Spanned a)
spannedP p = do
  s <- MP.getOffset
  x <- p
  e <- MP.getOffset
  pure (Spanned (Span s e) x)

-- | P, spanning it alone, then the horizontal space behind it.  NEVER
-- 'MPC.space': a part ENDING ITS LINE would eat the newline and the line under.
lexemeP :: StatefulParser a -> StatefulParser (Spanned a)
lexemeP p = spannedP p <* MPC.hspace

-- | Forced at every step: a thunk chain would outlive the document it points into.
spanRange :: [Span] -> Maybe Span
spanRange = foldl' (\acc sp -> Just $! maybe sp (<> sp) acc) Nothing

-- | ENDPARSER comes FIRST: it may claim the hspace the eol branch would swallow.
spannedContainerUntil :: (Parse element)
                      => ([element] -> container)
                      -> StatefulParser end
                      -> StatefulParser (Maybe Span, container)
spannedContainerUntil con endParser = do
    let stop = void (try endParser) <|> (MPC.hspace *> (void eol <|> eof))
    elems <- manyTill (MPC.hspace *> spannedP parse) (lookAhead (try stop))
    return (spanRange (map spanOf elems), con (map valueOf elems))


-- | One element.  A headline is tried only when BOL: org anchors stars to column 1.
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

-- | Org's @org-outline-regexp@ is @\\*+ @, so the run must END — hspace or eol
-- — else @*bold*@ opens a row.  Never 'MPC.space': an empty title would run on.
indentP :: StatefulParser (Spanned Indent)
indentP = spannedP (Indent . length <$> MP.some (MPC.char '*'))
          <* (void MPC.hspace1 <|> lookAhead (void eol <|> eof))

instance Parse Keyword where
  parse = Keyword . T.toUpper <$> keywordTextP

keywordTextP :: StatefulParser Text
keywordTextP = T.pack <$> some (MP.satisfy isKeywordChar)

-- | Exported so a command layer writes what this reads; org's own @#+TODO:@ word.
isKeywordChar :: Char -> Bool
isKeywordChar c = isAlpha c || c == '_'

instance Parse Pragma where
  parse = do
    key@(Keyword kText) <- MPC.string "#+" *> parse <* MPC.char ':' <* MPC.space

    case kText of
      -- The two older spellings configure the same cycle; a re-render says @#+TODO:@.
      k | k `elem` ["TODO", "SEQ_TODO", "TYP_TODO"] -> do
        -- Keywords register as written: org matches them case-sensitively.
        let todoKw = keywordTextP
                     <* optional (MPC.char '(' *> MP.takeWhileP Nothing (/= ')') *> MPC.char ')')

        active   <- todoKw `MP.sepEndBy` MPC.hspace1
        inactive <- MP.option [] $ do
                      void $ MPC.char '|' <* MPC.hspace
                      todoKw `MP.sepEndBy` MPC.hspace1

        -- Sets answer recognition; the LISTS keep the order a palette draws in.
        State.modify (setTodo (Set.fromList active) (Set.fromList inactive))
        return $ PTodo active inactive

      _ -> do
        val <- parse :: StatefulParser OrgLine
        when (kText == "CATEGORY") $ State.modify $ setCategory $ TS.showt val
        return $ Pragma key val

data PlanningKeyword = PlanScheduled | PlanDeadline | PlanClosed
  deriving (Enum, Bounded)

-- | The text org writes KW with.  Uppercase only: folding would swallow prose.
planningText :: PlanningKeyword -> Text
planningText PlanScheduled = "SCHEDULED:"
planningText PlanDeadline = "DEADLINE:"
planningText PlanClosed = "CLOSED:"

data Planning = Planning { plScheduled :: !(Maybe (Spanned Timestamp))
                         , plDeadline  :: !(Maybe (Spanned Timestamp))
                         , plClosed    :: !(Maybe (Spanned Timestamp))
                         }

noPlanning :: Planning
noPlanning = Planning Nothing Nothing Nothing

-- | The one line after the title line, last-wins per keyword.  A failed entry
-- backtracks over the hspace it skipped: the top loop needs it between elements.
planningP :: StatefulParser Planning
planningP = foldl' assign noPlanning <$> (MPC.hspace *> MPC.eol *> some (try entryP))
  where entryP = do
          kw <- MPC.hspace *> choice [k <$ MPC.string (planningText k) | k <- [minBound ..]]
          ts <- MPC.hspace1 *> spannedP (parse :: StatefulParser Timestamp)
          return (kw, ts)

        assign pl (PlanScheduled, ts) = pl { plScheduled = Just ts }
        assign pl (PlanDeadline, ts) = pl { plDeadline = Just ts }
        assign pl (PlanClosed, ts) = pl { plClosed = Just ts }

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

-- | Org's own rule; WIDER than 'keywordTextP', which walls off a starred meta.
propertyKeyP :: StatefulParser Keyword
propertyKeyP = Keyword . T.toUpper . T.pack
           <$> some (MP.satisfy (\c -> not (isSpace c) && c /= ':'))

propertiesP :: StatefulParser (Spanned Properties)
propertiesP = MPC.hspace *> MPC.eol *> spannedP drawer
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

tagsP :: StatefulParser (Maybe Span, Tags)
tagsP = do
    _ <- MPC.hspace1
    Spanned sp ts <- spannedP (char ':' *> many tag)
    return (if null ts then Nothing else Just sp, Tags ts)
    where tag = takeWhile1P (Just "tag") isTagChar <* char ':'

-- | Exported so a command layer writes what this reads; @org-tag-re@ plus @-@ and @%@.
isTagChar :: Char -> Bool
isTagChar c = isAlphaNum c || c == '_' || c == '-' || c == '@' || c == '#' || c == '%'

instance Parse Timestamp where
  parse = State.lift tsParser

titleP :: StatefulParser (Maybe Span, Title)
titleP = spannedContainerUntil Title tagsP

-- | A todo keyword, spanned alone; it must match a registered one, case included.
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


-- | @<a>--<b>@ or the compact @<date wd 10:30-11:30>@; 'tsCompactRange' records which.
tsParser :: StatelessParser Timestamp
tsParser = do
  tsStatus <- tsStatusParser
  (tsStart, compactEnd, tsInterval, tsWarning) <- tsBodyParser tsStatus
  dashEnd <- MP.optional . MP.try $ do
    _ <- MPC.string "--" *> MPC.char (fst (tsBrackets tsStatus))
    (moment, _compact, _interval, _warning) <- tsBodyParser tsStatus
    return moment
  let (tsEnd, tsCompactRange) = case dashEnd of
        Just moment -> (Just moment, False)
        Nothing     -> (compactEnd, isJust compactEnd)
  return (Timestamp {..})

tsStatusParser :: StatelessParser TimestampStatus
tsStatusParser = (TimestampActive <$ MPC.char '<')
             <|> (TimestampInactive <$ MPC.char '[')

tsBodyParser :: TimestampStatus
             -> StatelessParser (TsMoment, Maybe TsMoment, Maybe TimestampRepeaterInterval, Maybe TimestampWarningInterval)
tsBodyParser status = do
  day <- tsDayParser <* MPC.space
  void $ MP.optional (MP.try tsWeekdayParser) <* MPC.space
  time <- MP.optional (MP.try tsTimeParser)
  -- A range end and a warning both open with '-', so the end time is tried first
  -- and only its colon tells them apart.  No space around the '-'; org writes none.
  endTime <- if isJust time
             then MP.optional (MP.try (MPC.char '-' *> tsTimeParser))
             else return Nothing
  MPC.space
  cookies <- MP.many (tsCookieParser <* MPC.space)
  void $ MPC.char (snd (tsBrackets status))
  let atTime hasTime t = TsMoment { tsmTime = Time.UTCTime day (Time.timeOfDayToTime t)
                                  , tsmHasTime = hasTime }
  return ( atTime (isJust time) (fromMaybe midnight time)
         , atTime True <$> endTime
         , listToMaybe [ r | CookieRepeat r <- cookies ]
         , listToMaybe [ w | CookieWarn w <- cookies ] )
  where midnight = Time.TimeOfDay 0 0 0

-- | The warning arm is tried FIRST, which re-homes a lone @-3d@ to org's cookie.
data TsCookie = CookieRepeat !TimestampRepeaterInterval
              | CookieWarn !TimestampWarningInterval

tsCookieParser :: StatelessParser TsCookie
tsCookieParser = (CookieWarn <$> MP.try tsWarningParser)
             <|> (CookieRepeat <$> MP.try tsRepeaterParser)

tsWarningParser :: StatelessParser TimestampWarningInterval
tsWarningParser = do
  void $ MPC.char '-'
  firstOnly <- isJust <$> MP.optional (MPC.char '-')
  value <- MPL.decimal
  unit <- byChar (Just . unitChar)
  return TimestampWarningInterval { warningFirstOnly = firstOnly
                                  , warningValue = value
                                  , warningUnit = unit }

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
  tsHour <- MPL.decimal <* MPC.char sep
  tsMinute <- MPL.decimal
  tsSecond <- MP.option 0 $ MPC.char sep *> MPL.decimal
  guard (tsHour <= 23 && tsMinute <= 59 && tsSecond < 60) <|> fail "Time out of range"
  return (Time.TimeOfDay tsHour tsMinute (fromInteger tsSecond))

-- | Display-only.  LETTERS alone: a repeater opens with @.@, @+@, @-@ or a digit.
tsWeekdayParser :: StatelessParser ()
tsWeekdayParser = void (takeWhile1P (Just "weekday") isAlpha) <* MPC.space

byChar :: (Bounded a, Enum a) => (a -> Maybe Char) -> StatelessParser a
byChar spell = choice [ v <$ char c | v <- [minBound ..], Just c <- [spell v] ]

tsRepeaterParser :: StatelessParser TimestampRepeaterInterval
tsRepeaterParser = do
  repType <- MP.optional (MP.try (byChar typeChar))
  repSign <- MP.optional (MP.try (byChar (Just . signChar)))
  repValue <- MPL.decimal
  repUnit <- byChar (Just . unitChar)

  return TimestampRepeaterInterval {
    repeaterValue = repValue,
    -- TOTAL over the kind: a wildcard would rewrite a new one to `Restart'
    -- unread.  `+' is both `CatchUp''s prefix and the SIGN.
    repeaterType = case repType of
                     Nothing         -> Restart
                     Just Restart    -> Restart
                     Just Cumulative -> Cumulative
                     Just CatchUp    -> maybe Restart (const CatchUp) repSign,
    repeaterUnit = repUnit,
    repeaterSign = fromMaybe TRSPlus repSign
    }
