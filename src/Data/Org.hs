module Data.Org ( Context (..)
                , Element (..)
                , Headline (..)
                , Identity
                , Indent (..)
                , Keyword (..)
                , Pragma (..)
                , Priority (..)
                , Properties (..)
                , Property (..)
                , Sentence (..)
                , SentenceElement (..)
                , Separator (..)
                , Tags (..)
                , Timestamp (..)
                , TimestampStatus (..)
                , Title (..)
                , TitleElement (..)
                , Todo (..)
                , Token (..)
                , setCategory
                , inTodo
                , getTodo
                , setTodo
                , orgParse
                , orgParseM) where

import Control.Monad (void, guard)
import Control.Monad.State ( StateT, lift, runStateT )
import Control.Monad.State qualified as State
import Data.Char (isAlpha)
import Data.List (nub, find, sort)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString(..))
import Data.Text (Text, pack, toUpper, intercalate, replicate)
import Data.Text qualified as Text
import Data.Text.Lazy.Builder ()
import Data.Text.Lazy.Builder qualified as B
import Data.Time qualified as Time
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable
import Data.Void (Void)
import Text.Megaparsec (lookAhead, try, choice, eof, manyTill, takeWhile1P, (<|>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (eol, space1, space, char)
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as MPL
import TextShow (TextShow, fromText)
import TextShow qualified
import UnliftIO ()

import Prelude hiding (unwords, concat, replicate, concatMap)

orgParse :: Context -> Text -> ([Element], Context)
orgParse st cmd = case MP.parse (runStateT (MP.manyTill parse MP.eof) st) "" cmd of
  Right v -> v
  Left err  -> ([], st)  -- GToken (Token (pack (PS.errorBundlePretty err)))

orgParseM :: Text -> ([Element], Context)
orgParseM = orgParse mempty

-- tag :: StatelessParser Text
-- tag = MP.takeWhile1P (Just "tag character") (`elem` keyword) <* MPC.char ':'

-- keyword :: [Char]
-- keyword = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_"

class Identity a where
  identity :: a -> Text

class Parse a where
  parse :: StatefulParser a

data Context = Context { todoActive :: !(Set Text)
                       , todoInactive :: !(Set Text)
                       , metaCategory :: !Text
                       } deriving (Show, Eq, Typeable)

instance Semigroup Context where
  (<>) a b = Context { todoActive = todoActive a <> todoActive b
                     , todoInactive = todoInactive a <> todoInactive b
                     , metaCategory = metaCategory a <> metaCategory b
                     -- , metaTime = metaTime a <> metaTime b
                     }

instance Monoid Context where
  mempty = Context { todoActive = Set.fromList ["TODO"]
                   , todoInactive = Set.fromList ["DONE"]
                   , metaCategory = mempty
                   -- , metaTime = mempty :: [UTCTime]
                   -- , metaStack = EmptyStack
                   }

setCategory :: Text -> Context -> Context
setCategory category ctx = ctx { metaCategory = category }

inTodo :: Text -> Context -> Bool
inTodo todo ctx = todo `elem` getTodo ctx

getTodo :: Context -> Set Text
getTodo ctx = todoActive ctx <> todoInactive ctx

setTodo :: Set Text -> Set Text -> Context -> Context
setTodo active inactive Context{..} =
  Context{..} { todoActive = todoActive <> active
              , todoInactive = todoInactive <> inactive }

type StatelessParser = MP.Parsec Void Text

type StatefulParser a = StateT Context StatelessParser a

data Element where Element :: ( Show a
                              , TextShow a
                              , Typeable a
                              , Eq a
                              , Parse a
                              , Identity a) => a -> Element

instance Identity Element where
  identity (Element a) = identity a

instance Show Element where
  show (Element a) = show a

instance Eq Element where
  (Element x) == (Element y) = case Typeable.cast y of
    Just y' -> x == y'
    Nothing -> False

instance Parse Element where
  parse = choice [ try (Element <$> (parse :: StatefulParser Separator))
                 , try (Element <$> (parse :: StatefulParser Headline))
                 , try (Element <$> (parse :: StatefulParser Pragma))
                 , try (Element <$> (parse :: StatefulParser Timestamp))
                 , Element <$> (parse :: StatefulParser Token) ]

instance TextShow Element where
  showb (Element a) = TextShow.showb a

data Headline = Headline { indent :: !Indent
                         , todo :: !(Maybe Todo)
                         , priority :: !(Maybe Priority)
                         , title :: !Title
                         , schedule :: !(Maybe Timestamp)
                         , deadline :: !(Maybe Timestamp)
                         , properties :: !Properties
                         } deriving (Show, Eq)

instance Identity Headline where
  identity Headline {..} = case getProperty "ID" properties of
    Nothing -> TextShow.showt title
    Just (Property _ v) -> TextShow.showt v

instance TextShow Headline where
  showb Headline {..} =
    showSpaced indent
    <> foldMap showSpaced todo
    <> foldMap showSpaced priority
    <> TextShow.showb title
    where showSpaced :: TextShow a => a -> TextShow.Builder
          showSpaced = (<> TextShow.showbSpace) . TextShow.showb

instance Parse Headline where
  parse = do
    indent' <- parse :: StatefulParser Indent
    todo' <- MP.optional (MP.try parse :: StatefulParser Todo)
    priority' <- MP.optional (MP.try parse :: StatefulParser Priority)
    title' <- parse :: StatefulParser Title
    -- schedule' <- optional $ try (string "SCHEDULED:" *> space *> (parse :: StatefulParser Timestamp))
    -- deadline' <- optional $ try (string "DEADLINE:" *> space *> (parse :: StatefulParser Timestamp))
    properties' <- MP.option (mempty :: Properties) (MP.try (MPC.eol *> parse :: StatefulParser Properties))

    _newline <- parse :: StatefulParser Separator

    -- _id <- State.lift (liftIO (randomIO :: IO UUID))

    let headline = Headline { indent = indent'
                            , todo = todo'
                            , priority = priority'
                            , title = title'
                            , schedule = Nothing -- schedule'
                            , deadline = Nothing -- deadline'
                            , properties = properties' }

    return headline

newtype Indent = Indent Int
  deriving (Show, Eq)

instance Semigroup Indent where
  (<>) (Indent a) (Indent b) = Indent (a + b)

instance Monoid Indent where
  mempty = Indent 1

instance Parse Indent where
  parse = do
    stars <- MP.some (MPC.char '*') <* MPC.space
    return $ Indent (length stars)

instance TextShow Indent where
  showb (Indent indent) = TextShow.fromText (replicate indent "*")

newtype Keyword = Keyword Text
  deriving (Show, Eq)

instance TextShow Keyword where
  showb (Keyword k) = TextShow.fromText k

instance Parse Keyword where
  parse = do
    let keyword = MP.some (MP.satisfy (\c -> isAlpha c || c == '_'))
    Keyword . toUpper . pack <$> keyword

data Pragma = Pragma !Keyword !Text
            | PTodo !(Set.Set Text) !(Set.Set Text)
            | PCategory !Sentence
  deriving (Show, Eq)

instance Identity Pragma where
  identity (Pragma keyword text) = intercalate "-" [TextShow.showt keyword, text]
  identity (PTodo active inactive) = intercalate "-" (sort (Set.toList (active <> inactive)))
  identity (PCategory category) = TextShow.showt category

instance Parse Pragma where
  parse = do
    let keyword = parse :: StatefulParser Keyword
        todoList = MP.some (todo <* MPC.space)
        doneList = MP.option [] (MPC.char '|' *> MPC.space *> todoList)
        todoShort = pack <$> MP.between (MPC.char '(') (MPC.char ')') (MP.many (MP.noneOf ['(', ')', '\n']))
        todo = do
          Keyword result <- keyword <* MP.skipMany todoShort
          return result

    key <- MPC.string "#+" *> keyword <* MPC.string ":" <* MPC.space
    case key of
      Keyword "CATEGORY" -> do
        category <- parse :: StatefulParser Sentence
        State.modify $ setCategory (TextShow.showt category)
        return $ PCategory category
      Keyword "TODO" -> do
        pragmaActive <- Set.fromList <$> todoList
        pragmaInactive <- Set.fromList <$> doneList

        State.modify (setTodo pragmaActive pragmaInactive)

        return $ PTodo pragmaActive pragmaInactive
      _keyword -> do
        Token value <- parse :: StatefulParser Token
        return $ Pragma key value

instance TextShow Pragma where
  showb (Pragma k v) = "#+" <> TextShow.showb k <> ": " <> TextShow.fromText v
  showb (PTodo active inactive) = "#+TODO:" <> TextShow.showbSpace <> TextShow.fromText (Text.unwords (Set.toList active)) <> " | " <> TextShow.fromText (Text.unwords (Set.toList inactive))
  showb (PCategory category) = "#+CATEGORY:" <> TextShow.showbSpace <> TextShow.showb category

newtype Priority = Priority Char
  deriving (Show, Eq)

instance TextShow Priority where
  showb (Priority priority) = "[#" <> B.singleton priority <> "]" <> TextShow.showbSpace

instance Parse Priority where
  parse = do
    priority <- MPC.char '[' *> MPC.char '#' *> MPC.letterChar <* MPC.char ']' <* MPC.space
    return (Priority priority)

data Property = Property { key :: !Keyword, val :: !Sentence }
  deriving (Show, Eq)

instance Parse Property where
  parse = do
    keyword <- MPC.char ':' *> (parse :: StatefulParser Keyword) <* MPC.char ':' <* MPC.space
    guard $ not (reserved keyword)
    value <- parse :: StatefulParser Sentence

    case keyword of
      Keyword "CATEGORY" -> State.modify $ setCategory $ TextShow.showt value
      _keyword -> State.modify id

    return $ Property keyword value
    where reserved :: Keyword -> Bool
          reserved (Keyword k) = k `elem` ["PROPERTIES", "END"]

instance TextShow Property where
  showb (Property {..}) = ":" <> TextShow.showb key <> ": " <> TextShow.showb val

newtype Properties = Properties [Property]
  deriving (Show, Eq)

instance Semigroup Properties where
  (<>) (Properties a) (Properties b) = Properties (a <> b)

instance Monoid Properties where
  mempty = Properties []

instance Parse Properties where
  parse = do
    _ <- MPC.string ":PROPERTIES:" <* MPC.eol
    properties <- MP.manyTill ((parse :: StatefulParser Property) <* MPC.eol) (MPC.string ":END:")
    return (Properties properties)

instance TextShow Properties where
  showb (Properties ps) = ":PROPERTIES:\n" <> TextShow.showb ps <> ":END:\n"

getProperty :: Text -> Properties -> Maybe Property
getProperty k (Properties props) = find (\p -> key p == Keyword k) props

data SentenceElement = ElToken !Token
                     | ElSeparator !Separator
                     | ElTimestamp !Timestamp
  deriving (Show, Eq)

instance TextShow SentenceElement where
  showb (ElToken t) = TextShow.showb t
  showb (ElSeparator t) = TextShow.showb t
  showb (ElTimestamp t) = TextShow.showb t

instance Parse SentenceElement where
  parse = parseTry ElSeparator
    <|> parseTry ElTimestamp
    <|> (ElToken <$> parse)
    where parseTry el = try (el <$> parse)

newtype Sentence = Sentence [SentenceElement]
  deriving (Show, Eq)

instance Monoid Sentence where
  mempty = Sentence []

instance Semigroup Sentence where
  (<>) (Sentence a) (Sentence b) = Sentence (a <> b)

instance TextShow Sentence where
  showb (Sentence []) = ""
  showb (Sentence (x:xs)) = TextShow.showb x <> TextShow.showb (Sentence xs)

instance Parse Sentence where
  parse = do
    let stop = choice [ void eol, eof ]
    elems <- manyTill (parse :: StatefulParser SentenceElement) (lookAhead stop)
    return (Sentence elems)

data Separator = SPC | EOL | EOF
  deriving (Show, Eq)

instance Identity Separator where
  identity SPC = "SPC"
  identity EOL = "EOL"
  identity EOF = "EOF"

instance Parse Separator where
  parse = choice [ EOF <$ eof
                 , EOL <$ eol
                 , SPC <$ space1 <* space ]

instance TextShow Separator where
  showb SPC = " "
  showb EOL = "\n"
  showb EOF = ""

type Tag = Text

newtype Tags = Tags [Tag]
  deriving (Show, Eq)

instance TextShow Tags where
  showb (Tags []) = fromText ""
  showb (Tags tags) = fromText ":" <> fromText (intercalate ":" tags) <> fromText ":"

instance Semigroup Tags where
  (<>) (Tags a) (Tags b) = Tags (nub a <> b)

instance Monoid Tags where
  mempty = Tags []

instance Parse Tags where
  parse = do
    let stop = lookAhead $ try $ choice [void eol, eof]
    Tags <$> lift (char ':' *> manyTill tag stop)
    where tag :: StatelessParser Text
          tag = takeWhile1P (Just "tag character") (`elem` keyword) <* char ':'
          keyword :: [Char]
          keyword = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_"

-- Timestamp

data Timestamp = Timestamp { tsStatus :: !TimestampStatus
                           , tsInterval :: !(Maybe TimestampRepeaterInterval)
                           , tsTime :: !Time.UTCTime
                           } deriving (Show, Eq)

instance Identity Timestamp where
  identity = TextShow.showt

instance TextShow Timestamp where
  showb ts = openBracket
    <> TextShow.fromText timeText
    <> TextShow.fromText repeaterSeparator
    <> TextShow.fromText repeaterText
    <> closeBracket

    where
      openBracket = case tsStatus ts of
        TimestampActive -> "<"
        TimestampInactive -> "["
      closeBracket = case tsStatus ts of
        TimestampActive -> ">"
        TimestampInactive -> "]"
      timeText = tsFormat (tsTime ts)
      repeaterTypeText = case tsInterval ts of
        Nothing -> ""
        Just TimestampRepeaterInterval { repeaterType = Restart } -> ""
        Just TimestampRepeaterInterval { repeaterType = Cumulative } -> "."
        Just TimestampRepeaterInterval { repeaterType = CatchUp } -> "+"
      repeaterSignText = case tsInterval ts of
        Nothing -> ""
        Just TimestampRepeaterInterval { repeaterSign = TRSPlus } -> "+"
        Just TimestampRepeaterInterval { repeaterSign = TRSMinus } -> "-"
      repeaterUnitText = case tsInterval ts of
        Nothing -> ""
        Just TimestampRepeaterInterval { repeaterUnit = Days } -> "d"
        Just TimestampRepeaterInterval { repeaterUnit = Weeks } -> "w"
        Just TimestampRepeaterInterval { repeaterUnit = Months } -> "m"
        Just TimestampRepeaterInterval { repeaterUnit = Years } -> "y"
      repeaterValText = case tsInterval ts of
        Nothing -> ""
        Just TimestampRepeaterInterval { repeaterValue = val } -> TextShow.showt val
      repeaterText = repeaterTypeText <> repeaterSignText <> repeaterValText <> repeaterUnitText
      repeaterSeparator = case repeaterText of
        "" -> ""
        _repeater -> " "

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

tsFormat :: Time.UTCTime -> Text
tsFormat ts = pack (Time.formatTime Time.defaultTimeLocale timeFormat ts)
  where timeFormat = if (seconds::Integer) `mod` 60 == 0
                     then "%Y-%m-%d %a %H:%M"
                     else "%Y-%m-%d %a %H:%M:%S"
        seconds = floor $ Time.utctDayTime ts

tsCtrl :: StatelessParser Char
tsCtrl = MPC.char '<' <|> MPC.char '['

tsStatusParser :: StatelessParser TimestampStatus
tsStatusParser = do
  ctrl <- tsCtrl
  case ctrl of
    '<' -> return TimestampActive
    '[' -> return TimestampInactive
    _ctrl -> return TimestampInactive

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
  return (pack weekday)

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

-- Title

data TitleElement where
  TitleElement :: (Show a, TextShow a, Typeable a, Eq a, Parse a) => a -> TitleElement

instance Show TitleElement where
  show (TitleElement a) = show a

instance TextShow TitleElement where
  showb (TitleElement a) = TextShow.showb a

instance Eq TitleElement where
    (TitleElement a) == (TitleElement b) = case Typeable.cast b of
        Just b' -> a == b'
        Nothing -> False

instance Parse TitleElement where
  parse = MP.choice [ MP.try (TitleElement <$> (parse :: StatefulParser Separator))
                    , MP.try (TitleElement <$> (parse :: StatefulParser Timestamp))
                    , MP.try (TitleElement <$> (parse :: StatefulParser Tags))
                    , TitleElement <$> (parse :: StatefulParser Token) ]

newtype Title = Title [TitleElement]
  deriving (Show, Eq)

instance Semigroup Title where
  (<>) (Title a) (Title b) = Title (a <> b)

instance Monoid Title where
  mempty = Title []

instance TextShow Title where
  showb (Title []) = ""
  showb (Title (x:xs)) = TextShow.showb x <> TextShow.showb (Title xs)

instance Parse Title where
  parse = do
    let stop = MP.choice [ void MPC.eol, MP.eof ]
    elems <- MP.manyTill (parse :: StatefulParser TitleElement) (MP.lookAhead stop)
    return (Title elems)


-- Todo

data Todo = Todo { name :: Text, active :: Bool }
  deriving (Show, Eq)

instance Parse Todo where
  parse = do
    ctx <- State.get
    Keyword result <- (parse :: StatefulParser Keyword) <* MPC.space
    guard $ inTodo result ctx
    return Todo { name = result
                , active = result `elem` todoActive ctx
                }

instance TextShow Todo where
  showb a = TextShow.fromText (name a)


-- Token

newtype Token = Token Text
  deriving (Show, Eq)

instance IsString Token where
  fromString s = Token (pack s)

instance Semigroup Token where
  (<>) (Token a) (Token b) = Token (a <> b)

instance Monoid Token where
  mempty = Token (mempty :: Text)

instance Identity Token where
  identity (Token token) = TextShow.showt token

instance TextShow Token where
  showb (Token a) = TextShow.fromText a

instance Parse Token where
  parse = do
    let stop = MP.lookAhead (MP.choice [MPC.space1, void MPC.eol, MP.eof])
        word = MP.manyTill MP.anySingle stop
    Token <$> fmap pack word
