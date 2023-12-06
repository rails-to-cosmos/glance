{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Property (OrgProperty (..)) where

import Data.Org.Element
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

propertyStackKeywords :: [Text]
propertyStackKeywords = ["PROPERTIES", "END"]

isPropertyStackKeyword :: OrgKeyword -> Bool
isPropertyStackKeyword (OrgKeyword k) = k `elem` propertyStackKeywords

instance OrgElement OrgProperty where

  parser ctx = do
    k <- between (char ':') (char ':') (parser ctx :: Parser OrgKeyword)
    space
    guard $ not (isPropertyStackKeyword k)
    PlainText v <- parser ctx :: Parser PlainText
    return $ OrgProperty k v

  modifyState (OrgProperty (OrgKeyword "CATEGORY") category) ctx = ctx {metaCategory = category}
  modifyState _ ctx = ctx

instance TextShow OrgProperty where
  showb (OrgProperty k v) = ":" <> showb k <> ": " <> fromText v
