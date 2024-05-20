module Data.Org.Title ( Title (..)
                      , TitleElement (..) ) where

import Control.Monad

import Data.Org.Parse
import Data.Org.Token
import Data.Org.Tags
import Data.Org.Timestamp
import Data.Org.Separator

import TextShow (TextShow)
import TextShow qualified as TS

import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char

import Prelude hiding (concat)

newtype Title = Title [TitleElement]
  deriving (Show, Eq)

data TitleElement = TText !Token
                  | TTags !Tags
                  | TTs !Ts
                  | TSep !Sep
  deriving (Show, Eq)

instance TextShow TitleElement where
  showb (TText (Token x)) = TS.fromText x
  showb (TTags x) = TS.showb x
  showb (TTs x) = TS.showb x
  showb (TSep x) = TS.showb x

instance Semigroup Title where
  (<>) (Title lhs) (Title rhs) = Title (lhs <> rhs)

instance Monoid Title where
  mempty = Title []

instance TextShow Title where
  showb (Title []) = ""
  showb (Title (x:xs)) = TS.showb x <> TS.showb (Title xs)

instance Parse Title where
  parser = do
    let stopParsers = choice [ void eol, eof ]
        elemParsers = choice [ TSep <$> try parser
                             , TTs <$> try parser
                             , TTags <$> try parser
                             , TText <$> parser ]

    elems <- manyTill elemParsers (lookAhead stopParsers)

    return (Title elems)
