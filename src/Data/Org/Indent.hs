{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Indent (OrgIndent (..)) where

import Data.Org.Base
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
  type StateType OrgIndent = OrgContext

  parser _ = do
    stars <- some (char '*') <* space
    return $ OrgIndent $ length stars

  modifyState _ ctx = ctx
