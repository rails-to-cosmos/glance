{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Org.Elements.Title ( Title (..)
                               , TitleElement (..) ) where

import Control.Monad
import Data.Org.Elements.Separator
import Data.Org.Elements.Tags
import Data.Org.Elements.Timestamp
import Data.Org.Elements.Token
import Data.Org.Parser
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable
import Prelude hiding (concat)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import TextShow (TextShow)
import TextShow qualified

data TitleElement where
  TitleElement :: (Show a, TextShow a, Typeable a, Eq a, Parse a) => a -> TitleElement

instance Show TitleElement where
  show (TitleElement a) = show a

instance TextShow TitleElement where
  showb (TitleElement a) = TextShow.showb a

instance Eq TitleElement where
    (TitleElement x) == (TitleElement y) = case Typeable.cast y of
        Just y' -> x == y'
        Nothing -> False

instance Parse TitleElement where
  parse = MP.choice [ MP.try (TitleElement <$> (parse :: StatefulParser Separator))
                    , MP.try (TitleElement <$> (parse :: StatefulParser Timestamp))
                    , MP.try (TitleElement <$> (parse :: StatefulParser Tags))
                    , TitleElement <$> (parse :: StatefulParser Token) ]

newtype Title = Title [TitleElement]
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

    elems <- MP.manyTill (parse :: StatefulParser TitleElement) (MP.lookAhead stop)

    return (Title elems)
