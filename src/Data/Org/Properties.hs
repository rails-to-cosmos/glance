module Data.Org.Properties (Properties (..)) where

import Data.Org.Parse
import Data.Org.Property

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow (TextShow)
import TextShow qualified as TS

newtype Properties = Properties [Property]
  deriving (Show, Eq)

instance Semigroup Properties where
  (<>) (Properties lhs) (Properties rhs) = Properties (lhs <> rhs)

instance Monoid Properties where
  mempty = Properties []

instance Parse Properties where
  parser = do
    _ <- string ":PROPERTIES:" <* eol
    properties <- manyTill ((parser :: StatefulParser Property) <* eol) (string ":END:")
    return (Properties properties)

instance TextShow Properties where
  showb (Properties ps) = ":PROPERTIES:\n" <> TS.showb ps <> ":END:\n"
