{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Tags (OrgTags (..), tagCtrl) where

import Data.Text (Text, intercalate)
import Data.List (nub)

import Data.Org.Element
import Data.Char

import TextShow (TextShow, fromText, showb)
import Text.Megaparsec
import Text.Megaparsec.Char

import Prelude hiding (unwords, concat, replicate, concatMap)

newtype OrgTags = OrgTags [Text]
  deriving (Show, Eq)

instance TextShow OrgTags where
  showb (OrgTags []) = fromText ""
  showb (OrgTags tags) = fromText ":" <> fromText (intercalate ":" tags) <> fromText ":"

instance Semigroup OrgTags where
  (<>) (OrgTags lhs) (OrgTags rhs) = OrgTags (nub lhs <> rhs)

instance Monoid OrgTags where
  mempty = OrgTags []

tag :: Parser Text
tag = takeWhile1P (Just "tag character") (`elem` keyword) <* tagCtrl

keyword :: [Char]
keyword = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_"

tagCtrl :: Parser Char
tagCtrl = char ':'

instance OrgElement OrgTags where
  parser _ = OrgTags <$> (tagCtrl *> many tag)

  modifyState _ ctx = ctx
