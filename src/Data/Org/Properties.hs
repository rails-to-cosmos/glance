module Data.Org.Properties (Properties (..)) where

import Data.Org.Element
import Data.Org.Property

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow

newtype Properties = Properties [Property]
  deriving (Show, Eq)

instance Semigroup Properties where
  (<>) (Properties lhs) (Properties rhs) = Properties (lhs <> rhs)

instance Monoid Properties where
  mempty = Properties []

instance Org Properties where
  parser = do
    _ <- string ":PROPERTIES:" <* eol
    properties <- manyTill ((parser :: OrgParser Property) <* eol) (string ":END:")
    return (Properties properties)

instance TextShow Properties where
  showb (Properties ps) = ":PROPERTIES:\n" <> showb ps <> ":END:\n"
