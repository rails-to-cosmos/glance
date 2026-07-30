module Data.Org.Types ( Context (..)
                      , Display (..)
                      , Element (..)
                      , Hashable (..)
                      , HashID (..)
                      , Headline (..)
                      , HeadlineID
                      , HeadlineSpans (..)
                      , IAS (..)
                      , Identity (..)
                      , Indent (..)
                      , Keyword (..)
                      , OrgLine (..)
                      , OrgLineElement (..)
                      , Pragma (..)
                      , Priority (..)
                      , Properties (..)
                      , Property (..)
                      , Ref (..)
                      , RefKind (..)
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
                      , defaultHeadline
                      , emptyHeadlineSpans
                      , getProperty
                      , getTodo
                      , headlineIdProperty
                      , inTodo
                      , registerHeadline
                      , resolveHeadline
                      , setCategory
                      , setTodo
                      , sliceSpan
                      , stripSpans
                      , tsFormat
                      ) where

import qualified Crypto.Hash as Crypto
import Data.List (nub, find, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Time as Time
import TextShow (TextShow, fromText, showt, showb, showbSpace, Builder)
import qualified TextShow as TS

headlineIdProperty :: Text
headlineIdProperty = "ORG_GLANCE_ID"

-- Typeclasses

class Display a where
  display :: a -> Text

class Identity a where
  identity :: a -> Maybe Text

class Hashable a where
  hash :: a -> HashID

-- Span / Spanned

-- | Half-open character span [start, end) into the text given to 'orgParse'.
data Span = Span { spanStart :: !Int, spanEnd :: !Int }
  deriving (Show, Eq)

-- | Value with the source span it was parsed from.
data Spanned a = Spanned { spanOf :: !Span, valueOf :: !a }
  deriving (Show, Eq)

instance TextShow a => TextShow (Spanned a) where
  showb = showb . valueOf

instance Display a => Display (Spanned a) where
  display = display . valueOf

instance Identity a => Identity (Spanned a) where
  identity = identity . valueOf

-- | Extract the text SPAN covers out of T.
sliceSpan :: Text -> Span -> Text
sliceSpan t (Span s e) = T.take (e - s) (T.drop s t)

-- HashID / IAS

newtype HashID = HashID (Crypto.Digest Crypto.SHA256)
  deriving (Show, Eq)

instance TextShow HashID where
  showb (HashID digest) = TS.fromString (show digest)

type HeadlineID = Text

newtype IAS = IAS (Map HeadlineID Headline)
  deriving (Show, Eq)

instance Semigroup IAS where
  (IAS m1) <> (IAS m2) = IAS $ Map.unionWith (\_ new -> new) m1 m2

instance Monoid IAS where
  mempty = IAS Map.empty

-- Context

data Context = Context { todoActive :: !(Set Text)
                       , todoInactive :: !(Set Text)
                       , metaCategory :: !Text
                       , ias :: !IAS
                       } deriving (Show, Eq)

instance Display Context where
  display Context{ias = IAS m, ..} = T.unlines
    [ "Context"
    , "  Category:       " <> metaCategory
    , "  Active Todos:   " <> fset todoActive
    , "  Inactive Todos: " <> fset todoInactive
    , "  Headlines:      " <> showt (Map.size m) <> " items"
    ]
    where fset :: Set Text -> Text
          fset s
            | Set.null s = "{}"
            | otherwise  = "{ " <> T.intercalate ", " (Set.toList s) <> " }"

instance Semigroup Context where
  (<>) a b = Context { todoActive = todoActive a <> todoActive b
                     , todoInactive = todoInactive a <> todoInactive b
                     , metaCategory = metaCategory a <> metaCategory b
                     , ias = ias a <> ias b
                     }

instance Monoid Context where
  mempty = Context { todoActive = Set.fromList ["TODO"]
                   , todoInactive = Set.fromList ["DONE"]
                   , metaCategory = mempty
                   , ias = mempty
                   }

setCategory :: Text -> Context -> Context
setCategory category ctx = ctx { metaCategory = category }

registerHeadline :: Headline -> Context -> Context
registerHeadline headline ctx@Context{ias = IAS m} =
  case identity headline of
    Nothing -> ctx
    Just k -> ctx { ias = IAS (Map.insert k headline m) }

inTodo :: Text -> Context -> Bool
inTodo todo ctx = todo `elem` getTodo ctx

getTodo :: Context -> Set Text
getTodo ctx = todoActive ctx <> todoInactive ctx

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

instance Identity Element where
  identity (EHeadline a) = identity a
  identity (EPragma a) = identity a
  identity (ETimestamp a) = identity a
  identity (EToken a) = identity a

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

-- RefKind / Ref

data RefKind
    = CHILD_OF
    | PARENT_OF
    | NEXT_SIBLING_OF
    | PREV_SIBLING_OF
    | BLOCKS
    | BLOCKED_BY
    | RELATED_TO
    | CITES
    | CustomRef Text
    deriving (Show, Eq, Ord)

instance TextShow RefKind where
  showb (CustomRef t) = fromText t
  showb x             = fromString (show x)

instance IsString RefKind where
  fromString s = case s of
    "child of"        -> CHILD_OF
    "parent of"       -> PARENT_OF
    "next sibling of" -> NEXT_SIBLING_OF
    "prev sibling of" -> PREV_SIBLING_OF
    "blocks"          -> BLOCKS
    "blocked by"      -> BLOCKED_BY
    "related to"      -> RELATED_TO
    "cites"           -> CITES
    _                 -> CustomRef (T.pack s)

data Ref = Ref { kind :: RefKind
               , headlineId :: HeadlineID
               }
  deriving (Show, Eq)

instance TextShow Ref where
  showb Ref {..} = TS.showb kind <> TS.showb headlineId

-- Headline

data Headline = Headline { indent     :: !Indent
                         , todo       :: !(Maybe Todo)
                         , priority   :: !(Maybe Priority)
                         , title      :: !Title
                         , tags       :: !Tags
                         , schedule   :: !(Maybe Timestamp)
                         , deadline   :: !(Maybe Timestamp)
                         , properties :: !Properties
                         , refs       :: ![Ref]
                         , hashRefs   :: ![HashID]
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
  , hsProperties :: !(Maybe Span)  -- ^ line start of ":PROPERTIES:" through end of ":END:".
  } deriving (Show, Eq)

emptyHeadlineSpans :: HeadlineSpans
emptyHeadlineSpans = HeadlineSpans (Span 0 0) Nothing Nothing Nothing Nothing Nothing

defaultHeadline :: Headline
defaultHeadline = Headline { indent     = Indent 1
                           , todo       = Nothing
                           , priority   = Nothing
                           , title      = Title []
                           , tags       = Tags []
                           , schedule   = Nothing
                           , deadline   = Nothing
                           , properties = mempty
                           , refs       = mempty
                           , hashRefs   = mempty
                           , spans      = emptyHeadlineSpans
                           }

resolveHeadline :: Headline -> Headline -> Headline
resolveHeadline h1 h2 = case (schedule h1, schedule h2) of
    (Just t1, Just t2) | t1 > t2 -> h1
    _                            -> h2

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
                , kv "ID"         (formatMaybe (identity h))
                , kv "Hash"       (showt (hash h))
                , "  Properties:"
                ]
    ++ formatProps properties
    ++ [ "  Refs:" ] ++ formatList refs
    ++ [ "  HashRefs:" ] ++ formatList hashRefs
    where kv :: Text -> Text -> Text
          kv k v = "  " <> T.justifyLeft 12 ' ' (k <> ":") <> v

          formatMaybe :: (TextShow a) => Maybe a -> Text
          formatMaybe Nothing  = "_"
          formatMaybe (Just x) = showt x

          formatList :: (TextShow a) => [a] -> [Text]
          formatList [] = ["    (empty)"]
          formatList xs = fmap (\x -> "    - " <> showt x) xs

          formatProps :: Properties -> [Text]
          formatProps (Properties []) = ["    (none)"]
          formatProps (Properties ps) = [ "    " <> showt (key p) <> " = " <> showt (val p) | p <- ps ]

instance Identity Headline where
  identity Headline {..} = getProperty headlineIdProperty properties

instance Hashable Headline where
  hash Headline {..} = HashID $ Crypto.hash $ TE.encodeUtf8 $ TS.showt title

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
  deriving (Show, Eq)

instance Semigroup Indent where
  (<>) (Indent a) (Indent b) = Indent (a + b)

instance Monoid Indent where
  mempty = Indent 1

instance TextShow Indent where
  showb (Indent n) = TS.fromText (T.replicate n "*")

-- Keyword

newtype Keyword = Keyword Text
  deriving (Show, Eq)

instance TextShow Keyword where
  showb (Keyword k) = TS.fromText k

-- Pragma

data Pragma = Pragma !Keyword !OrgLine
            | PTodo !(Set Text) !(Set Text)
            | PCategory !OrgLine
  deriving (Show, Eq)

instance Identity Pragma where
  identity (Pragma k v) = Just $ T.intercalate "-" [showt k, showt v]
  identity (PTodo active inactive) = Just $ T.intercalate "-" (sort (Set.toList (active <> inactive)))
  identity (PCategory category) = Just $ showt category

instance TextShow Pragma where
  showb (Pragma k v) = "#+" <> TS.showb k <> ": " <> TS.showb v
  showb (PTodo active inactive) = "#+TODO:" <> TS.showbSpace <> TS.fromText (T.unwords (Set.toList active)) <> " | " <> TS.fromText (T.unwords (Set.toList inactive))
  showb (PCategory category) = "#+CATEGORY:" <> TS.showbSpace <> TS.showb category

instance Display Pragma where
  display = showt

-- Priority

newtype Priority = Priority Char
  deriving (Show, Eq)

instance TextShow Priority where
  showb (Priority p) = "[#" <> B.singleton p <> "]"

-- Property / Properties

data Property = Property { key :: !Keyword, val :: !OrgLine }
  deriving (Show, Eq)

instance TextShow Property where
  showb (Property {..}) = ":" <> TS.showb key <> ": " <> TS.showb val

newtype Properties = Properties [Property]
  deriving (Show, Eq)

instance Semigroup Properties where
  (<>) (Properties a) (Properties b) = Properties (a <> b)

instance Monoid Properties where
  mempty = Properties []

instance TextShow Properties where
  showb (Properties ps) = ":PROPERTIES:\n" <> TS.showb ps <> ":END:\n"

getProperty :: Text -> Properties -> Maybe Text
getProperty k (Properties props) = case find (\p -> key p == Keyword k) props of
    Nothing -> Nothing
    Just (Property _ v) -> Just (TS.showt v)

-- OrgLineElement / OrgLine

data OrgLineElement = OrgLineToken !Token
                    | OrgLineTimestamp !Timestamp
  deriving (Show, Eq)

instance TextShow OrgLineElement where
  showb (OrgLineToken t) = TS.showb t
  showb (OrgLineTimestamp t) = TS.showb t

newtype OrgLine = OrgLine [OrgLineElement]
  deriving (Show, Eq)

instance Semigroup OrgLine where
  (<>) (OrgLine a) (OrgLine b) = OrgLine (a <> b)

instance Monoid OrgLine where
  mempty = OrgLine []

instance TextShow OrgLine where
  showb (OrgLine []) = ""
  showb (OrgLine [x]) = TS.showb x
  showb (OrgLine (x:xs)) = TS.showb x <> " " <> TS.showb (OrgLine xs)

-- Tags

type Tag = Text

newtype Tags = Tags [Tag]
  deriving (Show, Eq)

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

instance Identity Timestamp where
  identity = Just . TS.showt

instance TextShow Timestamp where
  showb ts = bracketed (tsFormat (tsStart ts) <> repeaterText)
          <> maybe mempty (\end -> "--" <> bracketed (tsFormat end)) (tsEnd ts)
    where bracketed body = fromText (openBracket <> body <> closeBracket)
          (openBracket, closeBracket) = case tsStatus ts of
            TimestampActive -> ("<", ">")
            TimestampInactive -> ("[", "]")
          repeaterText = maybe "" ((" " <>) . repeaterFormat) (tsInterval ts)

instance Display Timestamp where
  display = showt

data TimestampStatus = TimestampActive | TimestampInactive
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
  typeText <> signText <> showt repeaterValue <> unitText
  where typeText = case repeaterType of
          Restart    -> ""
          Cumulative -> "."
          CatchUp    -> "+"
        signText = case repeaterSign of
          TRSPlus  -> "+"
          TRSMinus -> "-"
        unitText = case repeaterUnit of
          Days   -> "d"
          Weeks  -> "w"
          Months -> "m"
          Years  -> "y"

-- Title

newtype Title = Title [OrgLineElement]
  deriving (Show, Eq)

instance Semigroup Title where
  (<>) (Title a) (Title b) = Title (a <> b)

instance Monoid Title where
  mempty = Title []

instance TextShow Title where
  showb (Title []) = ""
  showb (Title [x]) = showb x
  showb (Title (x:xs)) = showb x <> " " <> TS.showb (Title xs)

-- Todo

data Todo = Todo { name :: Text, active :: Bool }
  deriving (Show, Eq)

instance TextShow Todo where
  showb a = TS.fromText (name a)

-- Token

newtype Token = Token Text
  deriving (Show, Eq)

instance IsString Token where
  fromString s = Token (T.pack s)

instance Semigroup Token where
  (<>) (Token a) (Token b) = Token (a <> b)

instance Monoid Token where
  mempty = Token (mempty :: Text)

instance Identity Token where
  identity = Just . TS.showt

instance TextShow Token where
  showb (Token a) = TS.fromText a

instance Display Token where
  display = showt
