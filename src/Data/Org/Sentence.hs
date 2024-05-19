module Data.Org.Sentence (Sentence(..), SentenceElement (..)) where

import Data.Org.Token
import Data.Org.Timestamp
import Data.Org.Separator
import Data.Org.Element

import TextShow (TextShow, showb)
import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad (void)

data SentenceElement = STk !Tk
                     | STs !Ts
                     | SSep !Sep
  deriving (Show, Eq)

instance TextShow SentenceElement where
  showb (STk x) = showb x
  showb (STs x) = showb x
  showb (SSep x) = showb x

newtype Sentence = Sentence [SentenceElement]
  deriving (Show, Eq)

instance Monoid Sentence where
  mempty = Sentence []

instance Semigroup Sentence where
  (<>) (Sentence a) (Sentence b) = Sentence (a <> b)

instance TextShow Sentence where
  showb (Sentence []) = ""
  showb (Sentence (x:xs)) = showb x <> showb (Sentence xs)

instance Org Sentence where
  parser = do
    let stopParsers = choice [ void eol, eof ]
        elemParsers = choice [ SSep <$> try parser
                             , STs <$> try parser
                             , STk <$> parser ]

    elems <- manyTill elemParsers (lookAhead stopParsers)

    return (Sentence elems)
