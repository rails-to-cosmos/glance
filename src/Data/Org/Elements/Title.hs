{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Org.Elements.Title (Title (..), Element (..)) where

import Control.Monad

import Data.Org.Parse
import Data.Org.Elements.Separator
import Data.Org.Elements.Tags
import Data.Org.Elements.Timestamp
import Data.Org.Elements.Token

import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable

import TextShow (TextShow)
import TextShow qualified

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

import Prelude hiding (concat)

data Element where
  Element :: (Show a, TextShow a, Typeable a, Eq a, Parse a) => a -> Element

instance Show Element where
  show (Element a) = show a

instance TextShow Element where
  showb (Element a) = TextShow.showb a

instance Eq Element where
    (Element a) == (Element b) = case Typeable.cast b of
        Just b' -> a == b'
        Nothing -> False

instance Parse Element where
  parse = MP.choice [ MP.try (Element <$> (parse :: StatefulParser Separator))
                    , MP.try (Element <$> (parse :: StatefulParser Timestamp))
                    , MP.try (Element <$> (parse :: StatefulParser Tags))
                    , Element <$> (parse :: StatefulParser Token) ]

newtype Title = Title [Element]
  deriving (Show, Eq)

instance Semigroup Title where
  (<>) (Title lhs) (Title rhs) = Title (lhs <> rhs)

instance Monoid Title where
  mempty = Title []

instance TextShow Title where
  showb (Title []) = ""
  showb (Title (x:xs)) = TextShow.showb x <> TextShow.showb (Title xs)

instance Parse Title where
  parse = do
    let stop = MP.choice [ void MPC.eol, MP.eof ]
    elems <- MP.manyTill (parse :: StatefulParser Element) (MP.lookAhead stop)
    return (Title elems)
