{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Pragma (OrgPragma (..)) where

import Data.Org.Base
import Data.Org.Context
import Data.Org.Keyword
import Data.Org.PlainText
import Data.Text (Text, pack, unwords)
import Data.List (nub)

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow

import Control.Monad

import Prelude hiding (unwords, concat, replicate, concatMap)

data OrgPragma = OrgPragma OrgKeyword Text
               | OrgTodoPragma [Text] [Text]
               | OrgCategoryPragma Text
  deriving (Show, Eq)

instance OrgElement OrgPragma where
  type StateType OrgPragma = OrgContext

  parser ctx = do
    let keyword = parser ctx :: Parser OrgKeyword
        plaintext = parser ctx :: Parser PlainText
        whitespaces = skipMany (char ' ')
        todoList = some (todo <* whitespaces)
        todoShort = pack <$> between (char '(') (char ')') (many (noneOf ['(', ')', '\n']))
        todo = do
          OrgKeyword result <- keyword <* skipMany todoShort
          return result

    kw <- string "#+" *> keyword <* string ":" <* whitespaces
    case kw of
      OrgKeyword "CATEGORY" -> do
        PlainText category <- plaintext
        return $ OrgCategoryPragma category
      OrgKeyword "TODO" -> do
        active <- todoList
        inactive <- char '|' *> whitespaces *> option [] todoList
        return $ OrgTodoPragma active inactive
      _ -> do
        PlainText v <- plaintext
        return $ OrgPragma kw v

  modifier (OrgCategoryPragma category) ctx = ctx {metaCategory = category}
  modifier (OrgTodoPragma active inactive) ctx = ctx {metaTodo = newTodo}
    where newTodo = ( nub $ fst (metaTodo ctx) ++ active, nub $ snd (metaTodo ctx) ++ inactive )
  modifier _ ctx = ctx

instance TextShow OrgPragma where
  showb (OrgPragma k v) = "#+" <> showb k <> ": " <> fromText v
  showb (OrgTodoPragma active inactive) = "#+TODO:" <> showbSpace <> fromText (unwords active) <> " | " <> fromText (unwords inactive)
  showb (OrgCategoryPragma category) = "#+CATEGORY:" <> showbSpace <> fromText category
