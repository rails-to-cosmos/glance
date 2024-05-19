module Data.Org.Indent (Indent (..)) where

import Data.Org.Element

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text qualified as Text
import TextShow qualified

newtype Indent = Indent Int
  deriving (Show, Eq)

instance Semigroup Indent where
  (<>) (Indent lhs) (Indent rhs) = Indent (lhs + rhs)

instance Monoid Indent where
  mempty = Indent 1

instance Org Indent where
  parser = do
    stars <- some (char '*') <* space
    return $ Indent (length stars)

instance TextShow.TextShow Indent where
  showb (Indent indent) = TextShow.fromText (Text.replicate indent "*")
