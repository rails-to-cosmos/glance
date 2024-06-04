module Data.Org.Elements.Properties (Properties (..)) where

import Data.Org.Parse
import Data.Org.Elements.Property

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

import TextShow (TextShow)
import TextShow qualified as TS

newtype Properties = Properties [Property]
  deriving (Show, Eq)

instance Semigroup Properties where
  (<>) (Properties a) (Properties b) = Properties (a <> b)

instance Monoid Properties where
  mempty = Properties []

instance Parse Properties where
  parse = do
    _ <- MPC.string ":PROPERTIES:" <* MPC.eol
    properties <- MP.manyTill ((parse :: StatefulParser Property) <* MPC.eol) (MPC.string ":END:")
    return (Properties properties)

instance TextShow Properties where
  showb (Properties ps) = ":PROPERTIES:\n" <> TS.showb ps <> ":END:\n"
