module Data.Org.Elements.Sentence (Sentence(..), SentenceElement (..)) where

import Control.Monad (void)

import Data.Org.Parse
import Data.Org.Elements.Separator
import Data.Org.Elements.Timestamp
import Data.Org.Elements.Token

import TextShow (TextShow)
import TextShow qualified as TS

import Text.Megaparsec qualified as MPS
import Text.Megaparsec.Char (eol)

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
  parse = do
    let stopParsers = MPS.choice [ void eol, MPS.eof ]
        elemParsers = MPS.choice [ SSeparator <$> MPS.try parse
                                 , STimestamp <$> MPS.try parse
                                 , SToken <$> parse ]

    elems <- MPS.manyTill elemParsers (MPS.lookAhead stopParsers)

    return (Sentence elems)
