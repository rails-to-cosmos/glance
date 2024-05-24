{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Org.Title ( Title (..)
                      , TitleElement (..) ) where

import Control.Monad

import Data.Org.Parse
import Data.Org.Separator
import Data.Org.Tags
import Data.Org.Timestamp
import Data.Org.Token
import Data.Typeable

import TextShow (TextShow)
import TextShow qualified as TS

import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char

import Prelude hiding (concat)

data TitleElement where
  TitleElement :: Parse a => a -> TitleElement

instance Show TitleElement where
  show (TitleElement a) = show a

instance TextShow TitleElement where
  showb (TitleElement a) = TS.showb a

instance Eq TitleElement where
    (TitleElement x) == (TitleElement y) = case cast y of
        Just y' -> x == y'
        Nothing -> False

instance Parse TitleElement where
  parser = choice [ try (TitleElement <$> (parser :: StatefulParser Separator))
                  , try (TitleElement <$> (parser :: StatefulParser Timestamp))
                  , try (TitleElement <$> (parser :: StatefulParser Tags))
                  , TitleElement <$> (parser :: StatefulParser Token) ]

newtype Title = Title [TitleElement]
  deriving (Show, Eq)

instance Semigroup Title where
  (<>) (Title lhs) (Title rhs) = Title (lhs <> rhs)

instance Monoid Title where
  mempty = Title []

instance TextShow Title where
  showb (Title []) = ""
  showb (Title (x:xs)) = TS.showb x <> TS.showb (Title xs)

instance Parse Title where
  parser = do
    let stop = choice [ void eol, eof ]

    elems <- manyTill (parser :: StatefulParser TitleElement) (lookAhead stop)

    return (Title elems)
