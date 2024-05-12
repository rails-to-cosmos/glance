module Data.Org.Priority (Priority (..)) where

import Data.Char (ord)
import Data.Org.Element

import Text.Megaparsec
import Text.Megaparsec.Char

newtype Priority = Priority (Maybe Char)
  deriving (Show, Eq)

instance Semigroup Priority where
  (<>) (Priority lhs) (Priority rhs) = Priority (minByOrd lhs rhs)
    where
      minByOrd Nothing b = b
      minByOrd a Nothing = a
      minByOrd (Just a) (Just b)
        | ord a <= ord b = Just a
        | otherwise     = Just b

instance Monoid Priority where
  mempty = Priority Nothing

instance OrgElement Priority where
  parser = do
    priority <- optional (char '[' *> char '#' *> letterChar <* char ']' <* space)
    return (Priority priority)
