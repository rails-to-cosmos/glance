{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Pragma (OrgPragma (..)) where

import Data.Org.Element
import Data.Org.Context
import Data.Org.Keyword
import Data.Org.PlainText
import Data.Text (Text, pack, unwords)
import Data.List (nub)

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow

import Control.Monad
import qualified Control.Monad.State as State
import Prelude hiding (unwords, concat, replicate, concatMap)

data OrgPragma = OrgPragma OrgKeyword Text
               | OrgTodoPragma [Text] [Text]
               | OrgCategoryPragma Text
  deriving (Show, Eq)

instance OrgElement OrgPragma where
  parser = do
    let keyword = parser :: OrgParser OrgKeyword
        todoList = some (todo <* space)
        todoShort = pack <$> between (char '(') (char ')') (many (noneOf ['(', ')', '\n']))
        todo = do
          OrgKeyword result <- keyword <* skipMany todoShort
          return result

    key <- string "#+" *> keyword <* string ":" <* space
    case key of
      OrgKeyword "CATEGORY" -> do
        PlainText category <- parser :: OrgParser PlainText
        State.modify (\ctx -> ctx {metaCategory = category})
        return $ OrgCategoryPragma category
      OrgKeyword "TODO" -> do
        active <- todoList
        inactive <- option [] (char '|' *> space *> todoList)
        State.modify (\ctx -> ctx {metaTodo = (nub (fst (metaTodo ctx) ++ active), nub (snd (metaTodo ctx) ++ inactive))})
        return $ OrgTodoPragma active inactive
      _ -> do
        PlainText value <- parser :: OrgParser PlainText
        return $ OrgPragma key value

instance TextShow OrgPragma where
  showb (OrgPragma k v) = "#+" <> showb k <> ": " <> fromText v
  showb (OrgTodoPragma active inactive) = "#+TODO:" <> showbSpace <> fromText (unwords active) <> " | " <> fromText (unwords inactive)
  showb (OrgCategoryPragma category) = "#+CATEGORY:" <> showbSpace <> fromText category
