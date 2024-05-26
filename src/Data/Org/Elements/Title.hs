{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Org.Elements.Title ( Title (..)
                               , TitleElement (..) ) where

import Control.Monad

import Data.Org.Parser
import Data.Org.Elements.Separator
import Data.Org.Elements.Tags
import Data.Org.Elements.Timestamp
import Data.Org.Elements.Token

import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable

import TextShow (TextShow)
import TextShow qualified

import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char

import Prelude hiding (concat)

data TitleElement where
  TitleElement :: (Show a, TextShow a, Typeable a, Eq a, Parseable a) => a -> TitleElement

instance Show TitleElement where
  show (TitleElement a) = show a

instance TextShow TitleElement where
  showb (TitleElement a) = TextShow.showb a

instance Eq TitleElement where
    (TitleElement x) == (TitleElement y) = case Typeable.cast y of
        Just y' -> x == y'
        Nothing -> False

instance Parseable TitleElement where
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
  showb (Title (x:xs)) = TextShow.showb x <> TextShow.showb (Title xs)

instance Parseable Title where
  parser = do
    let stop = choice [ void eol, eof ]

    elems <- manyTill (parser :: StatefulParser TitleElement) (lookAhead stop)

    return (Title elems)
