module Data.Org.Title ( Title (..), TitleElement (..) ) where

import Control.Monad

import Data.Org.Element
import Data.Org.Token
import Data.Org.Tags
import Data.Org.Timestamp
import Data.Org.Separator

import TextShow

import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char

import Prelude hiding (concat)

newtype Title = Title [TitleElement]
  deriving (Show, Eq)

data TitleElement = TText !Token
                  | TitleTags !Tags
                  | TitleTimestamp !Timestamp
                  | TSep !Separator
  deriving (Show, Eq)

instance TextShow TitleElement where
  showb (TText (Token x)) = fromText x
  showb (TitleTags x) = showb x
  showb (TitleTimestamp x) = showb x
  showb (TSep x) = showb x

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
        elemParsers = choice [ TSep <$> try parser
                             , TitleTimestamp <$> try parser
                             , TitleTags <$> try parser
                             , TText <$> parser ]

    elems <- manyTill elemParsers (lookAhead stopParsers)

    return (Title elems)
