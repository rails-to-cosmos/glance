{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Property (OrgProperty (..)) where

import Data.Org.Element
import Data.Org.Context
import Data.Org.Keyword
import Data.Org.PlainText
import Data.Text (Text)

import qualified Control.Monad.State as State

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
  parser = do
    key <- between (char ':') (char ':') (parser :: OrgParser OrgKeyword)
    space
    guard $ not (isPropertyStackKeyword key)

    PlainText value <- parser :: OrgParser PlainText

    case key of
      OrgKeyword "CATEGORY" -> State.modify (\ctx -> ctx {metaCategory = value})
      _ -> State.modify id

    return $ OrgProperty key value

instance TextShow OrgProperty where
  showb (OrgProperty k v) = ":" <> showb k <> ": " <> fromText v
