module Data.Org.Elements.Indent (Indent (..)) where

import Data.Org.Types

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Data.Text qualified as Text

import TextShow (TextShow)
import TextShow qualified as TS

newtype Indent = Indent Int
  deriving (Show, Eq)

instance Semigroup Indent where
  (<>) (Indent a) (Indent b) = Indent (a + b)

instance Monoid Indent where
  mempty = Indent 1

instance Parse Indent where
  parse = do
    stars <- MP.some (MPC.char '*') <* MPC.space
    return $ Indent (length stars)

instance TextShow Indent where
  showb (Indent indent) = TS.fromText (Text.replicate indent "*")
