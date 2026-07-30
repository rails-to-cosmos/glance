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
                      , TimestampRepeaterSign (..)
                      , TimestampRepeaterType (..)
                      , TimestampStatus (..)
                      , TimestampUnit (..)
                      , Title (..)
                      , Todo (..)
                      , Token (..)
                      , TsMoment (..)
                      , defaultContext
                      , defaultHeadline
                      , emptyHeadlineSpans
                      , getProperty
                      , headlineSpanParts
                      , identity
                      , inTodo
                      , registerHeadline
                      , resolveHeadline
                      , setCategory
                      , setTodo
                      , signChar
                      , sliceSpan
                      , spanFaults
                      , stripSpans
                      , tsBrackets
                      , typeChar
                      , unitChar
                      ) where

import Data.List (find, intersperse, nub, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
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
stripSpans :: Element -> Element
stripSpans (EHeadline a) = EHeadline a { spans = emptyHeadlineSpans }
stripSpans e = e

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

-- | Spans of a headline's mutable parts.  'hsFull' covers stars through the
-- last parsed component.  Sub-spans are tight: no surrounding whitespace.
data HeadlineSpans = HeadlineSpans
  { hsFull       :: !Span
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
emptyHeadlineSpans = HeadlineSpans { hsFull       = Span 0 0
                                   , hsTodo       = Nothing
                                   , hsPriority   = Nothing
                                   , hsTitle      = Nothing
                                   , hsTags       = Nothing
                                   , hsSchedule   = Nothing
                                   , hsDeadline   = Nothing
                                   , hsClosed     = Nothing
                                   , hsProperties = Nothing
                                   }

-- | H's labelled sub-spans in source order, each paired with the predicate its
-- slice out of the source must satisfy.  Single source of the span spec.  The
-- three planning entries sort by offset: org writes SCHEDULED:, DEADLINE: and
-- CLOSED: in any order on the planning line, and every consumer — the overlap
-- check, the 'hsFull' fold, the ordering assertion — reads this list as source
-- order.
headlineSpanParts :: Headline -> [(Text, Maybe Span, Text -> Bool)]
headlineSpanParts h =
     [ ("hsTodo",     hsTodo hs,     (== maybe "" name (todo h)))
     , ("hsPriority", hsPriority hs, (== maybe "" showt (priority h)))
     , ("hsTitle",    hsTitle hs,    \t -> T.words t == T.words (showt (title h)))
     , ("hsTags",     hsTags hs,     (== showt (tags h)))
     ]
  ++ sortOn (\(_label, sp, _ok) -> spanStart <$> sp) planning
  ++ [ ("hsProperties", hsProperties hs, drawer) ]
  where hs = spans h
        planning = [ ("hsSchedule", hsSchedule hs, timestampSlice (schedule h))
                   , ("hsDeadline", hsDeadline hs, timestampSlice (deadline h))
                   , ("hsClosed",   hsClosed hs,   timestampSlice (closed h))
                   ]
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

-- Keyword

newtype Keyword = Keyword Text
  deriving stock (Show, Eq)

instance TextShow Keyword where
  showb (Keyword k) = TS.fromText k

-- Pragma

data Pragma = Pragma !Keyword !OrgLine
            | PTodo !(Set Text) !(Set Text)
  deriving (Show, Eq)

instance TextShow Pragma where
  showb (Pragma k v) = "#+" <> TS.showb k <> ": " <> TS.showb v
  showb (PTodo active inactive) = "#+TODO:" <> TS.showbSpace <> TS.fromText (T.unwords (Set.toList active)) <> " | " <> TS.fromText (T.unwords (Set.toList inactive))

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

-- | A timestamp; 'tsEnd' set makes it the "start--end" range org writes on
-- CLOCK lines.  Both ends share the bracket kind 'tsStatus' names.
data Timestamp = Timestamp { tsStatus :: !TimestampStatus
                           , tsInterval :: !(Maybe TimestampRepeaterInterval)
                           , tsStart :: !TsMoment
                           , tsEnd :: !(Maybe TsMoment)
                           } deriving (Show, Eq)

instance Ord Timestamp where
  compare a b = compare (tsmTime (tsStart a)) (tsmTime (tsStart b))

instance TextShow Timestamp where
  showb ts = bracketed (tsFormat (tsStart ts) <> repeaterText)
          <> maybe mempty (\end -> "--" <> bracketed (tsFormat end)) (tsEnd ts)
    where bracketed body = fromText (T.cons open (T.snoc body close))
          (open, close) = tsBrackets (tsStatus ts)
          repeaterText = maybe "" ((" " <>) . repeaterFormat) (tsInterval ts)

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

data TimestampUnit = Days | Weeks | Months | Years
  deriving (Show, Eq, Enum, Bounded)

-- | The letter org writes UNIT with.
unitChar :: TimestampUnit -> Char
unitChar Days = 'd'
unitChar Weeks = 'w'
unitChar Months = 'm'
unitChar Years = 'y'

-- | Render M as org writes it: date, recomputed weekday, and a time of day
-- only when the source carried one.
tsFormat :: TsMoment -> Text
tsFormat (TsMoment time hasTime) = T.pack (Time.formatTime Time.defaultTimeLocale timeFormat time)
  where timeFormat | not hasTime           = "%Y-%m-%d %a"
                   | seconds `mod` 60 == 0 = "%Y-%m-%d %a %H:%M"
                   | otherwise             = "%Y-%m-%d %a %H:%M:%S"
        seconds = floor (Time.utctDayTime time) :: Integer

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
