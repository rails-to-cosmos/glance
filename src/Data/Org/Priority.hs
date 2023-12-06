module Data.Org.Priority (OrgPriority (..)) where

import Data.Char (ord)
import Data.Org.Element

import Text.Megaparsec
import Text.Megaparsec.Char

newtype OrgPriority = OrgPriority (Maybe Char)
  deriving (Show, Eq)

instance Semigroup OrgPriority where
  (<>) (OrgPriority lhs) (OrgPriority rhs) = OrgPriority (minByOrd lhs rhs)
    where
      minByOrd Nothing b = b
      minByOrd a Nothing = a
      minByOrd (Just a) (Just b)
        | ord a <= ord b = Just a
        | otherwise     = Just b

instance Monoid OrgPriority where
  mempty = OrgPriority Nothing

instance OrgElement OrgPriority where
  parser _ = do
    priority <- optional . try $ char '[' *> char '#' *> letterChar <* char ']' <* space
    return $ OrgPriority priority

  modifyState _ ctx = ctx
  -- modifyState (OrgPriority p') ctx = ctx {headline = h'}
  --   where h = headline ctx
  --         h' = h {priority = OrgPriority p'}
