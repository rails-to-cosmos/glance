module Data.Org.Indent (OrgIndent (..)) where

import Data.Org.Element
import Data.Org.Context

import Text.Megaparsec
import Text.Megaparsec.Char

newtype OrgIndent = OrgIndent Int
  deriving (Show, Eq)

instance Semigroup OrgIndent where
  (<>) (OrgIndent lhs) (OrgIndent rhs) = OrgIndent (lhs + rhs)

instance Monoid OrgIndent where
  mempty = OrgIndent 1

instance OrgElement OrgIndent where
  parser _ = do
    stars <- some (char '*')
    return $ OrgIndent $ length stars

  modifyState _ ctx = ctx
