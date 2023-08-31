{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Property (OrgProperty (..)) where

import Data.Org.Base
import Data.Org.Context
import Data.Org.Keyword
import Data.Org.PlainText
import Data.Text (Text)

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow

import Control.Monad

import Prelude hiding (unwords, concat, replicate, concatMap)

data OrgProperty = OrgProperty OrgKeyword Text
  deriving (Show, Eq)

reservedKeywords :: [Text]
reservedKeywords = ["PROPERTIES", "END"]

reservedPropertyKeyword :: OrgKeyword -> Bool
reservedPropertyKeyword (OrgKeyword k) = k `elem` reservedKeywords

instance OrgElement OrgProperty where
  type StateType OrgProperty = OrgContext

  parser ctx = do
    k <- between (char ':') (char ':') (parser ctx :: Parser OrgKeyword)
    space
    guard $ not (reservedPropertyKeyword k)
    PlainText v <- parser ctx :: Parser PlainText
    return $ OrgProperty k v

  modifier (OrgProperty (OrgKeyword "CATEGORY") category) ctx = ctx {metaCategory = category}
  modifier _ ctx = ctx

instance TextShow OrgProperty where
  showb (OrgProperty k v) = ":" <> showb k <> ": " <> fromText v
