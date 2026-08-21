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
                      , orgIdentity
                      , inTodo
                      , levelOf
                      , registerHeadline
                      , repeaterFormat
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
                      , addUnit
                      , relativeForms
                      , unitChar
                      , unitOf
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

-- | The tag org gives an archived headline; three layers ask for ORG's name.
archiveTag :: Text
archiveTag = "ARCHIVE"


class Display a where
  display :: a -> Text


-- | Half-open character span [start, end) into the text given to 'orgParse'.
data Span = Span { spanStart :: !Int, spanEnd :: !Int }
  deriving (Show, Eq)

-- | Cover from the first span's start through the second span's end.
instance Semigroup Span where
  Span s _ <> Span _ e = Span s e

data Spanned a = Spanned { spanOf :: !Span, valueOf :: !a }
  deriving (Show, Eq)

instance TextShow a => TextShow (Spanned a) where
  showb = showb . valueOf

instance Display a => Display (Spanned a) where
  display = display . valueOf

sliceSpan :: Text -> Span -> Text
sliceSpan t (Span s e) = T.take (e - s) (T.drop s t)

-- | SP moved BY characters; negative reads a document span in a slice's offsets.
shiftSpan :: Int -> Span -> Span
shiftSpan by (Span s e) = Span (s + by) (e + by)

-- | Label every way SP is malformed against a document of LEN characters.
spanFaults :: Int -> Span -> [Text]
spanFaults len sp = concat
  [ [ "negative-start"  | spanStart sp < 0 ]
  , [ "start-after-end" | spanStart sp > spanEnd sp ]
  , [ "end-past-eof"    | spanEnd sp > len ]
  ]


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

-- | The context a parse starts from: org's built-in TODO keywords and nothing else.
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

-- | Reset the spans an element carries, for span-insensitive comparison.  NO
-- catch-all: a fifth constructor carrying spans must extend this (AGENTS.hs).
stripSpans :: Element -> Element
stripSpans (EHeadline a) = EHeadline a { spans = emptyHeadlineSpans }
stripSpans e@(EPragma _) = e
stripSpans e@(ETimestamp _) = e
stripSpans e@(EToken _) = e

-- | The headlines among ELEMS, in document order.
headlinesOf :: [Spanned Element] -> [Headline]
headlinesOf elems = [ h | EHeadline h <- map valueOf elems ]

-- | The FIRST headline among ELEMS — the entry a blob holds, never a child's.
firstHeadlineOf :: [Spanned Element] -> Maybe Headline
firstHeadlineOf = listToMaybe . headlinesOf


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

-- | Spans of a headline's mutable parts, tight; the extent is 'hsFull', derived.
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

-- | A headline's eight sub-spans as a key, so order and dispatch stay total.
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

-- | HS's keyed sub-spans in SOURCE order; the three planning entries sort by offset.
spanParts :: HeadlineSpans -> [(SpanPart, Maybe Span)]
spanParts hs = before ++ sortOn (fmap spanStart . snd) planning ++ after
  where before   = [ (SpTodo, hsTodo hs), (SpPriority, hsPriority hs)
                   , (SpTitle, hsTitle hs), (SpTags, hsTags hs) ]
        planning = [ (SpSchedule, hsSchedule hs), (SpDeadline, hsDeadline hs)
                   , (SpClosed, hsClosed hs) ]
        after    = [ (SpProperties, hsProperties hs) ]

-- | The stars through the LAST component present, never the whitespace after it.
hsFull :: HeadlineSpans -> Span
hsFull hs = foldl' (<>) (hsStars hs) [ sp | (_part, Just sp) <- spanParts hs ]

-- | H's labelled sub-spans in source order, each with the predicate its slice satisfies.
headlineSpanParts :: Headline -> [(Text, Maybe Span, Text -> Bool)]
headlineSpanParts h = [ (spanPartLabel p, sp, slices p) | (p, sp) <- spanParts (spans h) ]
  where slices SpTodo       = (== maybe "" name (todo h))
        slices SpPriority   = (== maybe "" showt (priority h))
        slices SpTitle      = \t -> stamped t == stamped (showt (title h))
        slices SpTags       = (== showt (tags h))
        slices SpSchedule   = timestampSlice (schedule h)
        slices SpDeadline   = timestampSlice (deadline h)
        slices SpClosed     = timestampSlice (closed h)
        slices SpProperties = drawer
        drawer t = ":PROPERTIES:" `T.isPrefixOf` stripped && ":END:" `T.isSuffixOf` stripped
          where stripped = T.strip t

-- | T's words with every BRACKETED RUN collapsed to its own opener: 'TextShow'
-- recomputes a weekday, so a title CARRYING a timestamp fails a word-for-word
-- test over a slice that is exactly right.
stamped :: Text -> [Text]
stamped = go . T.words
  where
    go [] = []
    go (w:ws)
      | Just close <- closer w
      , not (close `T.isSuffixOf` w) = T.take 1 w : go (drop 1 (dropWhile (not . T.isSuffixOf close) ws))
      | Just _ <- closer w           = T.take 1 w : go ws
      | otherwise                    = w : go ws
    closer w = case T.uncons w of
      Just ('[', _) -> Just "]"
      Just ('<', _) -> Just ">"
      _             -> Nothing

-- | Can a slice be TS's source spelling?  Structural: a render recomputes the weekday.
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

-- | H's org-id: the @:ID:@ property, org's own namespace beside 'identity'.
orgIdentity :: Headline -> Maybe Text
orgIdentity = getProperty "ID" . properties

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


newtype Indent = Indent Int
  deriving stock (Show, Eq)

instance Semigroup Indent where
  (<>) (Indent a) (Indent b) = Indent (a + b)

instance Monoid Indent where
  mempty = Indent 1

instance TextShow Indent where
  showb (Indent n) = TS.fromText (T.replicate n "*")

-- | H's outline level: the number of stars it opens with.
levelOf :: Headline -> Int
levelOf h = case indent h of Indent n -> n


newtype Keyword = Keyword Text
  deriving stock (Show, Eq)

instance TextShow Keyword where
  showb (Keyword k) = TS.fromText k


-- | A pragma line.  @#+TODO:@ keeps both halves as LISTS in line order — that
-- order is the tree's whole say over how a state column sorts and a palette draws.
data Pragma = Pragma !Keyword !OrgLine
            | PTodo ![Text] ![Text]
  deriving (Show, Eq)

instance TextShow Pragma where
  showb (Pragma k v) = "#+" <> TS.showb k <> ": " <> TS.showb v
  showb (PTodo active inactive) = "#+TODO:" <> TS.showbSpace <> TS.fromText (T.unwords active) <> " | " <> TS.fromText (T.unwords inactive)

instance Display Pragma where
  display = showt


newtype Priority = Priority Char
  deriving stock (Show, Eq)

instance TextShow Priority where
  showb (Priority p) = "[#" <> B.singleton p <> "]"


data Property = Property { key :: !Keyword, val :: !OrgLine }
  deriving (Show, Eq)

instance TextShow Property where
  showb (Property {..}) = ":" <> TS.showb key <> ": " <> TS.showb val

newtype Properties = Properties [Property]
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

instance TextShow Properties where
  showb (Properties ps) = ":PROPERTIES:\n" <> TS.showb ps <> ":END:\n"

getProperty :: Text -> Properties -> Maybe Text
getProperty k (Properties props) = TS.showt . val <$> find ((== Keyword k) . key) props


data OrgLineElement = OrgLineToken !Token
                    | OrgLineTimestamp !Timestamp
  deriving (Show, Eq)

instance TextShow OrgLineElement where
  showb (OrgLineToken t) = TS.showb t
  showb (OrgLineTimestamp t) = TS.showb t

showbSpaced :: [OrgLineElement] -> Builder
showbSpaced = mconcat . intersperse showbSpace . map showb

newtype OrgLine = OrgLine [OrgLineElement]
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

instance TextShow OrgLine where
  showb (OrgLine xs) = showbSpaced xs


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


-- | One end of a timestamp; a date-only stamp holds midnight, 'tsmHasTime' unset.
data TsMoment = TsMoment { tsmTime :: !Time.UTCTime
                         , tsmHasTime :: !Bool
                         } deriving (Show, Eq)

-- | A timestamp; 'tsEnd' set makes it a range, both ends sharing 'tsStatus'\'s
-- brackets.  'tsCompactRange' preserves which spelling the source used.
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
          -- Repeater then warning — org's conventional order.
          cookieText = maybe "" ((" " <>) . repeaterFormat) (tsInterval ts)
                    <> maybe "" ((" " <>) . warningFormat) (tsWarning ts)
          -- Guarded, so a hand-built timestamp cannot render its end date away.
          compactly end = tsCompactRange ts
                       && tsmHasTime (tsStart ts) && tsmHasTime end
                       && Time.utctDay (tsmTime (tsStart ts)) == Time.utctDay (tsmTime end)

instance Display Timestamp where
  display = showt

data TimestampStatus = TimestampActive | TimestampInactive
  deriving (Show, Eq)

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

-- | An agenda warning\/delay cookie: a lone @-3d@ IS this, never a repeater.
data TimestampWarningInterval = TimestampWarningInterval
  { warningFirstOnly :: !Bool  -- ^ the @--@ spelling.
  , warningValue :: !Int
  , warningUnit :: !TimestampUnit
  } deriving (Show, Eq)

warningFormat :: TimestampWarningInterval -> Text
warningFormat TimestampWarningInterval{..} =
  (if warningFirstOnly then "--" else "-")
    <> showt warningValue
    <> T.singleton (unitChar warningUnit)

data TimestampUnit = Days | Weeks | Months | Years
  deriving (Show, Eq, Enum, Bounded)

unitChar :: TimestampUnit -> Char
unitChar Days = 'd'
unitChar Weeks = 'w'
unitChar Months = 'm'
unitChar Years = 'y'

-- | The unit LETTER spells, 'unitChar' read backwards -- org's whole charset, so
-- the relative-date reader and the parser answer over one list.
unitOf :: Char -> Maybe TimestampUnit
unitOf c = find ((== c) . unitChar) [minBound ..]

-- | DAY moved N of UNIT on, org's own calendar arithmetic.
addUnit :: TimestampUnit -> Integer -> Time.Day -> Time.Day
addUnit Days   n = Time.addDays n
addUnit Weeks  n = Time.addDays (7 * n)
addUnit Months n = Time.addGregorianMonthsClip n
addUnit Years  n = Time.addGregorianYearsClip n

-- | The relative forms a reader may spell, derived so a new unit is offered.
relativeForms :: Text
relativeForms = T.intercalate ", " [ "+1" <> T.singleton (unitChar u) | u <- [minBound ..] ]

-- | T under FMT in the default locale — the one @formatTime@ spelling.
spelled :: Time.FormatTime t => String -> t -> Text
spelled fmt = T.pack . Time.formatTime Time.defaultTimeLocale fmt

-- | The time-of-day format M needs: seconds only when it carries them.
tsTimeFormat :: TsMoment -> String
tsTimeFormat (TsMoment time _hasTime)
  | seconds `mod` 60 == 0 = "%H:%M"
  | otherwise             = "%H:%M:%S"
  where seconds = floor (Time.utctDayTime time) :: Integer

-- | Render M as org writes it, the weekday recomputed from the date.
tsFormat :: TsMoment -> Text
tsFormat m@(TsMoment time hasTime) = spelled fmt time
  where fmt | hasTime   = "%Y-%m-%d %a " <> tsTimeFormat m
            | otherwise = "%Y-%m-%d %a"

tsTimeOnly :: TsMoment -> Text
tsTimeOnly m@(TsMoment time _hasTime) = spelled (tsTimeFormat m) time

repeaterFormat :: TimestampRepeaterInterval -> Text
repeaterFormat TimestampRepeaterInterval{..} =
  T.pack (maybeToList (typeChar repeaterType) <> [signChar repeaterSign])
    <> showt repeaterValue
    <> T.singleton (unitChar repeaterUnit)


newtype Title = Title [OrgLineElement]
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

instance TextShow Title where
  showb (Title xs) = showbSpaced xs


-- | A headline's TODO keyword as the parser read it.  NAME is authoritative;
-- ACTIVE is a by-product of recognition — 'Data.Org.Config.classify' decides.
data Todo = Todo { name :: Text, active :: Bool }
  deriving (Show, Eq)

instance TextShow Todo where
  showb a = TS.fromText (name a)


newtype Token = Token Text
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

instance IsString Token where
  fromString s = Token (T.pack s)

instance TextShow Token where
  showb (Token a) = TS.fromText a

instance Display Token where
  display = showt
