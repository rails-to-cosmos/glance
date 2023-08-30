{-# LANGUAGE TypeFamilies #-}

module Data.Org.Todo (OrgTodo (..)) where

import Control.Monad (guard)

import Data.Text (Text)
import Data.Org.Base
import Data.Org.Context
import Data.Org.Keyword
import Text.Megaparsec

newtype OrgTodo = OrgTodo (Maybe Text)
  deriving (Show, Eq)

instance Semigroup OrgTodo where
  (<>) (OrgTodo lhs) (OrgTodo rhs) = OrgTodo (lhs <> rhs)

instance Monoid OrgTodo where
  mempty = OrgTodo Nothing

instance OrgElement OrgTodo where
  type StateType OrgTodo = OrgContext

  parser ctx = OrgTodo <$> optional (try $ todo ctx)

  modifier _ ctx = ctx

todo :: OrgContext -> Parser Text
todo ctx = do
  OrgKeyword result <- (parser ctx :: Parser OrgKeyword)
  guard $ result `elem` registeredTodoStates ctx
  return result
