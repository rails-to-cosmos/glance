{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Org.Types ( Context (..)
                      , Display (..)
                      , Element (..)
                      , Headline (..)
                      , HeadlineSpans (..)
                      , Indent (..)
                      , Keyword (..)
                      , OrgLine (..)
                      , OrgLineElement (..)
                      , Pragma (..)
                      , Priority (..)
                      , Properties (..)
                      , Property (..)
                      , Span (..)
                      , Spanned (..)
                      , Tags (..)
                      , Timestamp (..)
                      , TimestampRepeaterInterval (..)
                      , TimestampWarningInterval (..)
                      , TimestampRepeaterSign (..)
                      , TimestampRepeaterType (..)
                      , TimestampStatus (..)
                      , TimestampUnit (..)
                      , Title (..)
                      , Todo (..)
                      , Token (..)
                      , TsMoment (..)
                      , archiveTag
                      , defaultContext
                      , defaultHeadline
                      , firstHeadlineOf
                      , headlineSpanParts
                      , headlinesOf
                      , hsFull
                      , headlineIdProperty
                      , identity
                      , inTodo
                      , levelOf
                      , registerHeadline
                      , resolveHeadline
                      , setCategory
                      , setTodo
                      , shiftSpan
                      , signChar
                      , sliceSpan
                      , spanFaults
                      , spelled
                      , stripSpans
                      , tsBrackets
                      , typeChar
                      , unitChar
                      ) where

import Data.List (find, foldl', intersperse, nub, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Time as Time
import TextShow (TextShow, fromText, showt, showb, showbSpace, Builder)
import qualified TextShow as TS

headlineIdProperty :: Text
headlineIdProperty = "ORG_GLANCE_ID"

-- | The tag org gives an archived headline, as org spells it.  Here rather than
-- beside its readers because it is ORG's name and three layers ask for it: the
-- @archive@ command writes it ('Glance.Query.archiveEdits'), the filter hides
-- what wears it, and the scan's index comparison reads a blob's archive flag
-- off it ('Data.Org.Index').
archiveTag :: Text
archiveTag = "ARCHIVE"

-- Typeclasses

class Display a where
  display :: a -> Text

-- Span / Spanned

-- | Half-open character span [start, end) into the text given to 'orgParse'.
data Span = Span { spanStart :: !Int, spanEnd :: !Int }
  deriving (Show, Eq)

-- | Cover from the first span's start through the second span's end.
instance Semigroup Span where
  Span s _ <> Span _ e = Span s e

-- | Value with the source span it was parsed from.
data Spanned a = Spanned { spanOf :: !Span, valueOf :: !a }
  deriving (Show, Eq)

instance TextShow a => TextShow (Spanned a) where
  showb = showb . valueOf

instance Display a => Display (Spanned a) where
  display = display . valueOf

-- | Extract the text SPAN covers out of T.
sliceSpan :: Text -> Span -> Text
sliceSpan t (Span s e) = T.take (e - s) (T.drop s t)

-- | SP moved BY characters along the text it was measured in — negative to
-- read a document's span in a slice's own offsets.  Beside 'sliceSpan' because
-- it is the other half of moving between two coordinate systems, and because
-- three callers spelled the addition out for themselves.
shiftSpan :: Int -> Span -> Span
shiftSpan by (Span s e) = Span (s + by) (e + by)

-- | Label every way SP is malformed against a document of LEN characters.
spanFaults :: Int -> Span -> [Text]
spanFaults len sp = concat
  [ [ "negative-start"  | spanStart sp < 0 ]
  , [ "start-after-end" | spanStart sp > spanEnd sp ]
  , [ "end-past-eof"    | spanEnd sp > len ]
  ]

-- Context

data Context = Context { todoActive :: !(Set Text)
                       , todoInactive :: !(Set Text)
                       , metaCategory :: !Text
                       , ias :: !(Map Text Headline)
                       } deriving (Show, Eq)

instance Display Context where
  display Context{..} = T.unlines
    [ "Context"
    , "  Category:       " <> metaCategory
    , "  Active Todos:   " <> fset todoActive
    , "  Inactive Todos: " <> fset todoInactive
    , "  Headlines:      " <> showt (Map.size ias) <> " items"
    ]
    where fset :: Set Text -> Text
          fset s
            | Set.null s = "{}"
            | otherwise  = "{ " <> T.intercalate ", " (Set.toList s) <> " }"

-- | The context a parse starts from: org's built-in TODO keywords and nothing
-- else.  Contexts compose only by threading them through 'orgParse'.
defaultContext :: Context
defaultContext = Context { todoActive = Set.fromList ["TODO"]
                         , todoInactive = Set.fromList ["DONE"]
                         , metaCategory = mempty
                         , ias = Map.empty
                         }

setCategory :: Text -> Context -> Context
setCategory category ctx = ctx { metaCategory = category }

registerHeadline :: Headline -> Context -> Context
registerHeadline headline ctx = case identity headline of
  Nothing -> ctx
  Just k  -> ctx { ias = Map.insert k headline (ias ctx) }

inTodo :: Text -> Context -> Bool
inTodo todo ctx = todo `Set.member` (todoActive ctx <> todoInactive ctx)

setTodo :: Set Text -> Set Text -> Context -> Context
setTodo active inactive Context{..} =
  Context{..} { todoActive = todoActive <> active
              , todoInactive = todoInactive <> inactive }

-- Element

data Element = EHeadline Headline
             | EPragma Pragma
             | ETimestamp Timestamp
             | EToken Token
  deriving (Show, Eq)

instance Display Element where
  display (EHeadline a) = display a
  display (EPragma a) = display a
  display (ETimestamp a) = display a
  display (EToken a) = display a

instance TextShow Element where
  showb (EHeadline a) = TS.showb a
  showb (EPragma a) = TS.showb a
  showb (ETimestamp a) = TS.showb a
  showb (EToken a) = TS.showb a

-- | Reset the spans an element carries, for span-insensitive comparison.
--
-- Every constructor is spelled out and there is NO catch-all, which is what
-- makes the obligation enforceable: the suite reads elements through
-- @bare = map (stripSpans . valueOf)@, so a fifth constructor carrying spans
-- that fell through unstripped would turn ~150 span-insensitive assertions
-- span-sensitive in silence.  Under @-Wall@ the compiler now asks for the arm.
stripSpans :: Element -> Element
stripSpans (EHeadline a) = EHeadline a { spans = emptyHeadlineSpans }
stripSpans e@(EPragma _) = e
stripSpans e@(ETimestamp _) = e
stripSpans e@(EToken _) = e

-- | The headlines among ELEMS, in document order.  One reading of the
-- comprehension four callers wrote out — the scan, the index, the loader and
-- the view — so a second 'Element' constructor carrying a headline extends this
-- rather than four sites that would silently disagree.
headlinesOf :: [Spanned Element] -> [Headline]
headlinesOf elems = [ h | EHeadline h <- map valueOf elems ]

-- | The FIRST headline among ELEMS, which is the entry a blob holds.  First
-- rather than first-with-an-id: a child carrying an id of its own is not the
-- document's, and the two readers of this — the write note and the blob
-- composer — must agree about which headline a stored entry IS.
firstHeadlineOf :: [Spanned Element] -> Maybe Headline
firstHeadlineOf = listToMaybe . headlinesOf

-- Headline

data Headline = Headline { indent     :: !Indent
                         , todo       :: !(Maybe Todo)
                         , priority   :: !(Maybe Priority)
                         , title      :: !Title
                         , tags       :: !Tags
                         , schedule   :: !(Maybe Timestamp)
                         , deadline   :: !(Maybe Timestamp)
                         , closed     :: !(Maybe Timestamp)
                         , properties :: !Properties
                         , spans      :: !HeadlineSpans
                         } deriving (Show, Eq)

-- | Spans of a headline's mutable parts.  Sub-spans are tight: no surrounding
-- whitespace.  The extent of the whole headline is 'hsFull', computed off these
-- rather than stored beside them.
data HeadlineSpans = HeadlineSpans
  { hsStars      :: !Span          -- ^ the stars alone, where the headline begins.
  , hsTodo       :: !(Maybe Span)  -- ^ keyword text exactly, e.g. "TODO".
  , hsPriority   :: !(Maybe Span)  -- ^ "[#A]" exactly.
  , hsTitle      :: !(Maybe Span)  -- ^ first to last title element; Nothing when title empty.
  , hsTags       :: !(Maybe Span)  -- ^ ":a:b:" exactly; Nothing when no tags.
  , hsSchedule   :: !(Maybe Span)  -- ^ the SCHEDULED: timestamp alone, keyword excluded.
  , hsDeadline   :: !(Maybe Span)  -- ^ the DEADLINE: timestamp alone, keyword excluded.
  , hsClosed     :: !(Maybe Span)  -- ^ the CLOSED: timestamp alone, keyword excluded.
  , hsProperties :: !(Maybe Span)  -- ^ line start of ":PROPERTIES:" through end of ":END:".
  } deriving (Show, Eq)

emptyHeadlineSpans :: HeadlineSpans
emptyHeadlineSpans = HeadlineSpans { hsStars      = Span 0 0
                                   , hsTodo       = Nothing
                                   , hsPriority   = Nothing
                                   , hsTitle      = Nothing
                                   , hsTags       = Nothing
                                   , hsSchedule   = Nothing
                                   , hsDeadline   = Nothing
                                   , hsClosed     = Nothing
                                   , hsProperties = Nothing
                                   }

-- | A headline's eight labelled sub-spans, as a key rather than a string.  One
-- name per component, so the ordering below and the slice predicates of
-- 'headlineSpanParts' are two total functions of ONE enumeration: a component
-- named by the order and missed by the dispatch is a compile error where it
-- used to be a silent fall-through.
data SpanPart = SpTodo | SpPriority | SpTitle | SpTags
              | SpSchedule | SpDeadline | SpClosed | SpProperties
  deriving (Eq, Ord, Show)

-- | What the corpus audit and the suite call PART.
spanPartLabel :: SpanPart -> Text
spanPartLabel SpTodo       = "hsTodo"
spanPartLabel SpPriority   = "hsPriority"
spanPartLabel SpTitle      = "hsTitle"
spanPartLabel SpTags       = "hsTags"
spanPartLabel SpSchedule   = "hsSchedule"
spanPartLabel SpDeadline   = "hsDeadline"
spanPartLabel SpClosed     = "hsClosed"
spanPartLabel SpProperties = "hsProperties"

-- | HS's keyed sub-spans in source order.  The three planning entries sort
-- by offset: org writes SCHEDULED:, DEADLINE: and CLOSED: in any order on the
-- planning line, and every consumer — the overlap check, 'hsFull', the ordering
-- assertion — reads this one list as source order.
spanParts :: HeadlineSpans -> [(SpanPart, Maybe Span)]
spanParts hs = before ++ sortOn (fmap spanStart . snd) planning ++ after
  where before   = [ (SpTodo, hsTodo hs), (SpPriority, hsPriority hs)
                   , (SpTitle, hsTitle hs), (SpTags, hsTags hs) ]
        planning = [ (SpSchedule, hsSchedule hs), (SpDeadline, hsDeadline hs)
                   , (SpClosed, hsClosed hs) ]
        after    = [ (SpProperties, hsProperties hs) ]

-- | The extent of the headline itself: the stars through the end of the last
-- component present, and never the whitespace after it.  Derived rather than
-- stored, so it cannot come to disagree with 'spanParts' about which order the
-- components run in — the ordering invariant is the structure here.
hsFull :: HeadlineSpans -> Span
hsFull hs = foldl' (<>) (hsStars hs) [ sp | (_part, Just sp) <- spanParts hs ]

-- | H's labelled sub-spans in source order, each paired with the predicate its
-- slice out of the source must satisfy.  Single source of the span spec: the
-- order is 'spanParts'\' and the tests are 'slices'\', both total over
-- 'SpanPart', so neither table can name a component the other has forgotten.
headlineSpanParts :: Headline -> [(Text, Maybe Span, Text -> Bool)]
headlineSpanParts h = [ (spanPartLabel p, sp, slices p) | (p, sp) <- spanParts (spans h) ]
  where slices SpTodo       = (== maybe "" name (todo h))
        slices SpPriority   = (== maybe "" showt (priority h))
        slices SpTitle      = \t -> T.words t == T.words (showt (title h))
        slices SpTags       = (== showt (tags h))
        slices SpSchedule   = timestampSlice (schedule h)
        slices SpDeadline   = timestampSlice (deadline h)
        slices SpClosed     = timestampSlice (closed h)
        slices SpProperties = drawer
        drawer t = ":PROPERTIES:" `T.isPrefixOf` stripped && ":END:" `T.isSuffixOf` stripped
          where stripped = T.strip t

-- | Whether a slice can be the source spelling of TS: the brackets 'tsStatus'
-- names, around a non-empty body.  The check stays structural for two reasons.
-- Rendering recomputes the weekday, so a source stamp carrying a stale one —
-- the corpus has them — never equals its own render.  And this module cannot
-- reparse, since the parser imports it; TestSpans, which can, carries the
-- exact reparse-equality assertion for these three spans.
timestampSlice :: Maybe Timestamp -> Text -> Bool
timestampSlice Nothing = T.null
timestampSlice (Just ts) = \t -> T.length t > 2 && T.head t == open && T.last t == close
  where (open, close) = tsBrackets (tsStatus ts)

defaultHeadline :: Headline
defaultHeadline = Headline { indent     = Indent 1
                           , todo       = Nothing
                           , priority   = Nothing
                           , title      = Title []
                           , tags       = Tags []
                           , schedule   = Nothing
                           , deadline   = Nothing
                           , closed     = Nothing
                           , properties = mempty
                           , spans      = emptyHeadlineSpans
                           }

resolveHeadline :: Headline -> Headline -> Headline
resolveHeadline h1 h2 = case (schedule h1, schedule h2) of
    (Just t1, Just t2) | t1 > t2 -> h1
    _                            -> h2

-- | H's own identifier: the ORG_GLANCE_ID property, when it carries one.
identity :: Headline -> Maybe Text
identity = getProperty headlineIdProperty . properties

instance Display Headline where
  display h@Headline{..} =
    T.unlines $ [ "Headline"
                , kv "Indent"     (showt indent)
                , kv "Title"      (showt title)
                , kv "Todo"       (formatMaybe todo)
                , kv "Tags"       (showt tags)
                , kv "Priority"   (formatMaybe priority)
                , kv "Schedule"   (formatMaybe schedule)
                , kv "Deadline"   (formatMaybe deadline)
                , kv "Closed"     (formatMaybe closed)
                , kv "ID"         (formatMaybe (identity h))
                , "  Properties:"
                ]
    ++ formatProps properties
    where kv :: Text -> Text -> Text
          kv k v = "  " <> T.justifyLeft 12 ' ' (k <> ":") <> v

          formatMaybe :: (TextShow a) => Maybe a -> Text
          formatMaybe Nothing  = "_"
          formatMaybe (Just x) = showt x

          formatProps :: Properties -> [Text]
          formatProps (Properties []) = ["    (none)"]
          formatProps (Properties ps) = [ "    " <> showt (key p) <> " = " <> showt (val p) | p <- ps ]

instance TextShow Headline where
  showb Headline{..} =
       showb indent
    <> showbSpace
    <> maybe mempty spaced todo
    <> maybe mempty spaced priority
    <> showb title
    <> if tags == mempty then mempty else showbSpace
    <> showb tags
    where spaced :: TextShow a => a -> Builder
          spaced = (<> showbSpace) . showb

-- Indent

newtype Indent = Indent Int
  deriving stock (Show, Eq)

instance Semigroup Indent where
  (<>) (Indent a) (Indent b) = Indent (a + b)

instance Monoid Indent where
  mempty = Indent 1

instance TextShow Indent where
  showb (Indent n) = TS.fromText (T.replicate n "*")

-- | H's outline level: the number of stars it opens with.  The unwrapping three
-- callers spelled by hand, including the @level == 1@ that decides a row.
levelOf :: Headline -> Int
levelOf h = case indent h of Indent n -> n

-- Keyword

newtype Keyword = Keyword Text
  deriving stock (Show, Eq)

instance TextShow Keyword where
  showb (Keyword k) = TS.fromText k

-- Pragma

-- | A pragma line.  @#+TODO:@ has a constructor of its own because its keywords
-- are read rather than shown: the two halves of the bar are what a tree
-- configures.
--
-- Both halves are LISTS, in the order the line spells them, because that order
-- is the org file's whole say over how the state column sorts and how a palette
-- draws — a @Set@ here re-sorted every tree's cycle alphabetically before
-- anything downstream could read it.  A word repeated on one line arrives
-- twice; deduplicating is 'Data.Org.Config.mergeKeywords'\'s, one rule at the
-- layer that merges layers.  RECOGNITION is unaffected: the parser folds these
-- into 'Context'\'s sets, which is where a headline's keyword is matched.
data Pragma = Pragma !Keyword !OrgLine
            | PTodo ![Text] ![Text]
  deriving (Show, Eq)

instance TextShow Pragma where
  showb (Pragma k v) = "#+" <> TS.showb k <> ": " <> TS.showb v
  showb (PTodo active inactive) = "#+TODO:" <> TS.showbSpace <> TS.fromText (T.unwords active) <> " | " <> TS.fromText (T.unwords inactive)

instance Display Pragma where
  display = showt

-- Priority

newtype Priority = Priority Char
  deriving stock (Show, Eq)

instance TextShow Priority where
  showb (Priority p) = "[#" <> B.singleton p <> "]"

-- Property / Properties

data Property = Property { key :: !Keyword, val :: !OrgLine }
  deriving (Show, Eq)

instance TextShow Property where
  showb (Property {..}) = ":" <> TS.showb key <> ": " <> TS.showb val

newtype Properties = Properties [Property]
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

instance TextShow Properties where
  showb (Properties ps) = ":PROPERTIES:\n" <> TS.showb ps <> ":END:\n"

-- | The value of property K in PROPS, rendered as org spelled it.
getProperty :: Text -> Properties -> Maybe Text
getProperty k (Properties props) = TS.showt . val <$> find ((== Keyword k) . key) props

-- OrgLineElement / OrgLine

data OrgLineElement = OrgLineToken !Token
                    | OrgLineTimestamp !Timestamp
  deriving (Show, Eq)

instance TextShow OrgLineElement where
  showb (OrgLineToken t) = TS.showb t
  showb (OrgLineTimestamp t) = TS.showb t

-- | Render ELEMENTS separated by single spaces.
showbSpaced :: [OrgLineElement] -> Builder
showbSpaced = mconcat . intersperse showbSpace . map showb

newtype OrgLine = OrgLine [OrgLineElement]
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

instance TextShow OrgLine where
  showb (OrgLine xs) = showbSpaced xs

-- Tags

type Tag = Text

newtype Tags = Tags [Tag]
  deriving stock (Show, Eq)

instance TextShow Tags where
  showb (Tags []) = fromText ""
  showb (Tags ts) = fromText $ ":" <> T.intercalate ":" ts <> ":"

instance Semigroup Tags where
  (<>) (Tags a) (Tags b) = Tags (nub (a <> b))

instance Monoid Tags where
  mempty = Tags mempty

-- Timestamp

-- | One end of a timestamp: the moment, and whether the source spelled a time
-- of day.  Date-only timestamps hold midnight with 'tsmHasTime' unset.
data TsMoment = TsMoment { tsmTime :: !Time.UTCTime
                         , tsmHasTime :: !Bool
                         } deriving (Show, Eq)

-- | A timestamp; 'tsEnd' set makes it a range.  Both ends share the bracket
-- kind 'tsStatus' names.  Org spells a range two ways — "<a>--<b>", which
-- CLOCK lines always use, and the compact same-day "<date wd 10:30-11:30>" —
-- and 'tsCompactRange' preserves the source's choice, since re-rendering the
-- other spelling is a spurious hunk.
data Timestamp = Timestamp { tsStatus :: !TimestampStatus
                           , tsInterval :: !(Maybe TimestampRepeaterInterval)
                           , tsWarning :: !(Maybe TimestampWarningInterval)
                           , tsStart :: !TsMoment
                           , tsEnd :: !(Maybe TsMoment)
                           , tsCompactRange :: !Bool
                           } deriving (Show, Eq)

instance Ord Timestamp where
  compare a b = compare (tsmTime (tsStart a)) (tsmTime (tsStart b))

instance TextShow Timestamp where
  showb ts = case tsEnd ts of
    Just end | compactly end -> bracketed (tsFormat (tsStart ts) <> "-" <> tsTimeOnly end <> cookieText)
    Just end                 -> bracketed (tsFormat (tsStart ts) <> cookieText)
                             <> "--" <> bracketed (tsFormat end)
    Nothing                  -> bracketed (tsFormat (tsStart ts) <> cookieText)
    where bracketed body = fromText (T.cons open (T.snoc body close))
          (open, close) = tsBrackets (tsStatus ts)
          -- Repeater then warning — org's conventional order.  A source that
          -- spelled them the other way round re-renders conventionally, which
          -- is inside TextShow's documented lossiness; the span channel keeps
          -- the source spelling.
          cookieText = maybe "" ((" " <>) . repeaterFormat) (tsInterval ts)
                    <> maybe "" ((" " <>) . warningFormat) (tsWarning ts)
          -- Both ends carrying a time on one day is what the compact spelling
          -- can express; guarded rather than assumed, so a hand-built
          -- timestamp cannot render its end date away.
          compactly end = tsCompactRange ts
                       && tsmHasTime (tsStart ts) && tsmHasTime end
                       && Time.utctDay (tsmTime (tsStart ts)) == Time.utctDay (tsmTime end)

instance Display Timestamp where
  display = showt

data TimestampStatus = TimestampActive | TimestampInactive
  deriving (Show, Eq)

-- | The brackets STATUS is written with: opening and closing.
tsBrackets :: TimestampStatus -> (Char, Char)
tsBrackets TimestampActive = ('<', '>')
tsBrackets TimestampInactive = ('[', ']')

data TimestampRepeaterInterval = TimestampRepeaterInterval
  { repeaterType :: !TimestampRepeaterType
  , repeaterValue :: !Int
  , repeaterUnit :: !TimestampUnit
  , repeaterSign :: !TimestampRepeaterSign
  } deriving (Show, Eq)

data TimestampRepeaterSign = TRSPlus | TRSMinus
  deriving (Show, Eq, Enum, Bounded)

-- | The character org writes SIGN with.
signChar :: TimestampRepeaterSign -> Char
signChar TRSPlus = '+'
signChar TRSMinus = '-'

data TimestampRepeaterType = CatchUp | Restart | Cumulative
  deriving (Show, Eq, Enum, Bounded)

-- | The character prefixing TYPE; 'Restart' is spelled by its absence.
typeChar :: TimestampRepeaterType -> Maybe Char
typeChar CatchUp = Just '+'
typeChar Restart = Nothing
typeChar Cumulative = Just '.'

-- | An agenda warning or delay cookie: @-3d@ moves the entry's agenda lead
-- time, and the doubled @--3d@ applies to the first occurrence alone.  org's
-- own grammar — a lone @-3d@ IS this cookie, never a repeater, and 'tsWarning'
-- is where it lives.
data TimestampWarningInterval = TimestampWarningInterval
  { warningFirstOnly :: !Bool  -- ^ the @--@ spelling.
  , warningValue :: !Int
  , warningUnit :: !TimestampUnit
  } deriving (Show, Eq)

-- | Render W the way org writes it, e.g. @-3d@ or @--7d@.
warningFormat :: TimestampWarningInterval -> Text
warningFormat TimestampWarningInterval{..} =
  (if warningFirstOnly then "--" else "-")
    <> showt warningValue
    <> T.singleton (unitChar warningUnit)

data TimestampUnit = Days | Weeks | Months | Years
  deriving (Show, Eq, Enum, Bounded)

-- | The letter org writes UNIT with.
unitChar :: TimestampUnit -> Char
unitChar Days = 'd'
unitChar Weeks = 'w'
unitChar Months = 'm'
unitChar Years = 'y'

-- | T under FMT in the default locale.  The one spelling of
-- @formatTime defaultTimeLocale@, since every stamp this codebase writes — org's
-- own @%Y-%m-%d %a@, a time of day, the index's ISO instant — is that call under
-- a different format string.
spelled :: Time.FormatTime t => String -> t -> Text
spelled fmt = T.pack . Time.formatTime Time.defaultTimeLocale fmt

-- | The time-of-day format M needs: seconds only when it carries them.
tsTimeFormat :: TsMoment -> String
tsTimeFormat (TsMoment time _hasTime)
  | seconds `mod` 60 == 0 = "%H:%M"
  | otherwise             = "%H:%M:%S"
  where seconds = floor (Time.utctDayTime time) :: Integer

-- | Render M as org writes it: date, recomputed weekday, and a time of day
-- only when the source carried one.
tsFormat :: TsMoment -> Text
tsFormat m@(TsMoment time hasTime) = spelled fmt time
  where fmt | hasTime   = "%Y-%m-%d %a " <> tsTimeFormat m
            | otherwise = "%Y-%m-%d %a"

-- | Render M's time of day alone: the tail of the compact range spelling.
tsTimeOnly :: TsMoment -> Text
tsTimeOnly m@(TsMoment time _hasTime) = spelled (tsTimeFormat m) time

-- | Render INTERVAL the way org writes a repeater, e.g. ".+3d".
repeaterFormat :: TimestampRepeaterInterval -> Text
repeaterFormat TimestampRepeaterInterval{..} =
  T.pack (maybeToList (typeChar repeaterType) <> [signChar repeaterSign])
    <> showt repeaterValue
    <> T.singleton (unitChar repeaterUnit)

-- Title

newtype Title = Title [OrgLineElement]
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

instance TextShow Title where
  showb (Title xs) = showbSpaced xs

-- Todo

-- | A headline's TODO keyword as the parser read it.
--
-- NAME is authoritative and verbatim.  ACTIVE is DECIDED AT PARSE TIME against
-- whatever context the parse was seeded with, and NOTHING downstream reads it
-- as the answer: classification is 'Data.Org.Config.classify', nearest scope
-- wins, and the served @active@ flag comes from there.  The field survives
-- because the parse-time reading is what keeps a keyword out of a title, and it
-- is part of 'Todo''s 'Eq', which the suite compares on.  Treat it as a
-- by-product of recognition rather than as a classification.
data Todo = Todo { name :: Text, active :: Bool }
  deriving (Show, Eq)

instance TextShow Todo where
  showb a = TS.fromText (name a)

-- Token

newtype Token = Token Text
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

instance IsString Token where
  fromString s = Token (T.pack s)

instance TextShow Token where
  showb (Token a) = TS.fromText a

instance Display Token where
  display = showt
