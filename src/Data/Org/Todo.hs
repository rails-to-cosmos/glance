module Data.Org.Todo (OrgTodo (..)) where

import Control.Monad (guard)

import Data.Text (Text)
import Data.Org.Element
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
  parser ctx = OrgTodo <$> optional (try $ todo ctx)

  modifyState _ ctx = ctx

todo :: OrgContext -> Parser Text
todo ctx = do
  OrgKeyword result <- (parser ctx :: Parser OrgKeyword)
  guard $ result `elem` allTodoStates ctx
  return result
