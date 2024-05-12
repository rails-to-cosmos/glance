module Data.Org.Title ( Title (..), TitleElement (..) ) where

import Control.Monad

import Data.Org.Element
import Data.Org.Lexeme
import Data.Org.Tags
import Data.Org.Timestamp
import Data.Org.Separator

import TextShow

import Text.Megaparsec
import Text.Megaparsec.Char

import Prelude hiding (concat)

newtype Title = Title [TitleElement]
  deriving (Show, Eq)

data TitleElement = TitleText !Lexeme
                  | TitleTags !Tags
                  | TitleTimestamp !Timestamp
                  | TitleSeparator !Separator
  deriving (Show, Eq)

instance TextShow TitleElement where
  showb (TitleText (Lexeme x)) = fromText x
  showb (TitleTags x) = showb x
  showb (TitleTimestamp x) = showb x
  showb (TitleSeparator x) = showb x

instance Semigroup Title where
  (<>) (Title lhs) (Title rhs) = Title (lhs <> rhs)

instance Monoid Title where
  mempty = Title []

instance TextShow Title where
  showb (Title []) = ""
  showb (Title (x:xs)) = showb x <> showb (Title xs)

instance OrgElement Title where
  parser = do
    let stopParsers = choice [ void eol, eof ]
        elemParsers = choice [ TitleSeparator <$> try parser
                             , TitleTimestamp <$> try parser
                             , TitleTags <$> try parser
                             , TitleText <$> parser ]

    elems <- manyTill elemParsers (lookAhead stopParsers)

    return (Title elems)
