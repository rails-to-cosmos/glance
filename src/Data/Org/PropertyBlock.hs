{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Org.PropertyBlock (OrgPropertyBlock (..)) where

import Data.Org.Base
import Data.Org.Context
import Data.Org.Property

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad

import TextShow

newtype OrgPropertyBlock = OrgPropertyBlock [OrgProperty]
  deriving (Show, Eq)

instance Semigroup OrgPropertyBlock where
  (<>) (OrgPropertyBlock lhs) (OrgPropertyBlock rhs) = OrgPropertyBlock (lhs <> rhs)

instance Monoid OrgPropertyBlock where
  mempty = OrgPropertyBlock []

instance OrgElement OrgPropertyBlock where
  type StateType OrgPropertyBlock = OrgContext

  parser ctx = do
    void (string ":PROPERTIES:")
    void (many newline)
    properties <- manyTill ((parser ctx :: Parser OrgProperty) <* many newline) (string ":END:")

    return (OrgPropertyBlock properties)

  modifyState (OrgPropertyBlock properties) ctx = foldl (flip modifyState) ctx properties

instance TextShow OrgPropertyBlock where
  showb (OrgPropertyBlock ps) = ":PROPERTIES:\n" <> showb ps <> ":END:\n"
