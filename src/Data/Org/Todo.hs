module Data.Org.Todo (OrgTodo (..)) where

import Control.Monad (guard)

import Data.Text (Text)
import Data.Org.Element
import Data.Org.Context
import Data.Org.Keyword
import Text.Megaparsec
import qualified Control.Monad.State as State

newtype OrgTodo = OrgTodo (Maybe Text)
  deriving (Show, Eq)

instance Semigroup OrgTodo where
  (<>) (OrgTodo lhs) (OrgTodo rhs) = OrgTodo (lhs <> rhs)

instance Monoid OrgTodo where
  mempty = OrgTodo Nothing

instance OrgElement OrgTodo where
  parser = OrgTodo <$> optional (try todo)

todo :: OrgParser Text
todo = do
  ctx <- State.get
  OrgKeyword result <- (parser :: OrgParser OrgKeyword)
  guard $ result `elem` allTodoStates ctx
  return result
