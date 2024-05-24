module Data.Org.Indent (Indent (..)) where

import Data.Org.Parse

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text qualified as T

import TextShow (TextShow)
import TextShow qualified as TS

newtype Indent = Indent Int
  deriving (Show, Eq)

instance Semigroup Indent where
  (<>) (Indent lhs) (Indent rhs) = Indent (lhs + rhs)

instance Monoid Indent where
  mempty = Indent 1

instance Parse Indent where
  parser = do
    stars <- some (char '*') <* space
    return $ Indent (length stars)

instance TextShow Indent where
  showb (Indent indent) = TS.fromText (T.replicate indent "*")
