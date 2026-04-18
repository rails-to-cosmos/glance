module Data.OrgElement where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Set (Set)
import Data.Time (UTCTime)

import Data.Org.Timestamp (Timestamp)

import qualified TextShow as TS
import TextShow (TextShow)

-- Token

-- newtype Token = Token Text
--   deriving (Show, Eq)

-- newtype Keyword = Keyword Text
--   deriving (Show, Eq)

-- newtype Indent = Indent Int
--   deriving (Show, Eq)

-- data Todo = Todo { name :: Text, active :: Bool }
--   deriving (Show, Eq)

-- newtype Priority = Priority Char
--   deriving (Show, Eq)

-- data OrgLineElement = OrgLineToken !Token
--                     | OrgLineTimestamp !Timestamp
--   deriving (Show, Eq)

-- instance Parse OrgLineElement where
--   parse = via OrgLineTimestamp
--     <|> (OrgLineToken <$> parse)

-- instance TextShow OrgLineElement where
--   showb (OrgLineToken t) = TS.showb t
--   showb (OrgLineTimestamp t) = TS.showb t

-- newtype OrgLine = OrgLine [OrgLineElement]
--   deriving (Show, Eq)

-- instance Semigroup OrgLine where
--   (<>) (OrgLine a) (OrgLine b) = OrgLine (a <> b)

-- instance Monoid OrgLine where
--   mempty = OrgLine []

-- instance TextShow OrgLine where
--   showb (OrgLine []) = ""
--   showb (OrgLine (x:xs)) = TS.showb x <> TS.showb (OrgLine xs)

-- instance Parse OrgLine where
--   parse = parseContainer OrgLine

-- newtype Title = Title [OrgLineElement]
--   deriving (Show, Eq)

-- instance Semigroup Title where
--   (<>) (Title a) (Title b) = Title (a <> b)

-- instance Monoid Title where
--   mempty = Title []

-- instance TextShow Title where
--   showb (Title xs) = foldMap showb xs

-- instance Parse Title where
--   parse = parseContainerUntil Title stop
--     where stop = parse :: StatefulParser Tags

-- type Tag = Text

-- data Element = Headline { indent     :: !Indent
--                         , todo       :: !(Maybe Todo)
--                         , priority   :: !(Maybe Priority)
--                         , title      :: !Title
--                         , tags       :: ![Tag]
--                         , schedule   :: !(Maybe Timestamp)
--                         , deadline   :: !(Maybe Timestamp)
--                         , properties :: !Properties
--                         , refs       :: ![Ref]
--                         , hashRefs   :: ![HashID]
--                         }
--              | Pragma !Keyword !Text
--              | PragmaTodo !(Set Text) !(Set Text)
--              | PragmaCategory !Keyword
--              | Token Text
--   deriving (Show, Eq)

-- -- instance TextShow Element where
-- --   showb (Element a) = TS.showb a

-- -- instance IsString Token where
-- --   fromString s = Token (T.pack s)

-- -- instance Semigroup Token where
-- --   (<>) (Token a) (Token b) = Token (a <> b)

-- -- instance Monoid Token where
-- --   mempty = Token (mempty :: Text)

-- -- instance Identity Token where
-- --   identity = Just . TS.showt

-- -- instance TextShow Token where
-- --   showb (Token a) = TS.fromText a

-- -- instance Display Token where
-- --   display = showt

-- -- instance Parse Token where
-- --   parse = Token <$> takeWhile1P (Just "token") (not . isSpace)
