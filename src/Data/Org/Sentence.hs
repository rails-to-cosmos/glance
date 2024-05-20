module Data.Org.Sentence (Sentence(..), SentenceElement (..)) where

import Data.Org.Token
import Data.Org.Timestamp
import Data.Org.Separator
import Data.Org.Base qualified as Org

import TextShow (TextShow)
import TextShow qualified as TS

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad (void)

data SentenceElement = STk !Tk
                     | STs !Ts
                     | SSep !Sep
  deriving (Show, Eq)

instance TextShow SentenceElement where
  showb (STk x) = TS.showb x
  showb (STs x) = TS.showb x
  showb (SSep x) = TS.showb x

newtype Sentence = Sentence [SentenceElement]
  deriving (Show, Eq)

instance Monoid Sentence where
  mempty = Sentence []

instance Semigroup Sentence where
  (<>) (Sentence a) (Sentence b) = Sentence (a <> b)

instance TextShow Sentence where
  showb (Sentence []) = ""
  showb (Sentence (x:xs)) = TS.showb x <> TS.showb (Sentence xs)

instance Org.Parse Sentence where
  parser = do
    let stopParsers = choice [ void eol, eof ]
        elemParsers = choice [ SSep <$> try Org.parser
                             , STs <$> try Org.parser
                             , STk <$> Org.parser ]

    elems <- manyTill elemParsers (lookAhead stopParsers)

    return (Sentence elems)
