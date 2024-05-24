module Data.Org.Sentence (Sentence(..), SentenceElement (..)) where

import Control.Monad (void)

import Data.Org.Parse
import Data.Org.Separator
import Data.Org.Timestamp
import Data.Org.Token

import TextShow (TextShow)
import TextShow qualified as TS

import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char

data SentenceElement = SToken !Token
                     | STimestamp !Timestamp
                     | SSeparator !Separator
  deriving (Show, Eq)

instance TextShow SentenceElement where
  showb (SToken x) = TS.showb x
  showb (STimestamp x) = TS.showb x
  showb (SSeparator x) = TS.showb x

newtype Sentence = Sentence [SentenceElement]
  deriving (Show, Eq)

instance Monoid Sentence where
  mempty = Sentence []

instance Semigroup Sentence where
  (<>) (Sentence a) (Sentence b) = Sentence (a <> b)

instance TextShow Sentence where
  showb (Sentence []) = ""
  showb (Sentence (x:xs)) = TS.showb x <> TS.showb (Sentence xs)

instance Parse Sentence where
  parser = do
    let stopParsers = choice [ void eol, eof ]
        elemParsers = choice [ SSeparator <$> try parser
                             , STimestamp <$> try parser
                             , SToken <$> parser ]

    elems <- manyTill elemParsers (lookAhead stopParsers)

    return (Sentence elems)
