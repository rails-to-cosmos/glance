module Data.Org.Elements.Properties (Properties (..), Property(..), find) where

import Data.Text (Text)
import Data.List qualified as List

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

import TextShow (TextShow)
import TextShow qualified as TS

import Data.Org.Elements.Keyword qualified as Keyword
import Data.Org.Elements.Property (Property)
import Data.Org.Elements.Property qualified as Property
import Data.Org.Types

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

find :: Text -> Properties -> Maybe Property
find k (Properties props) = List.find (\p -> Property.key p == Keyword.fromText k) props
