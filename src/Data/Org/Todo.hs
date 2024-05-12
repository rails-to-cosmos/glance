module Data.Org.Todo (Todo (..)) where

import Data.Text (Text)
import Data.Org.Element
import Data.Org.Context
import Data.Org.Keyword

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad
import Control.Monad.State qualified as State

newtype Todo = Todo (Maybe Text)
  deriving (Show, Eq)

instance Semigroup Todo where
  (<>) (Todo lhs) (Todo rhs) = Todo (lhs <> rhs)

instance Monoid Todo where
  mempty = Todo Nothing

instance OrgElement Todo where
  parser = Todo <$> optional (try todo)

todo :: OrgParser Text
todo = do
  ctx <- State.get
  Keyword result <- (parser :: OrgParser Keyword) <* space
  guard $ result `elem` (metaTodoActive ctx <> metaTodoInactive ctx)
  return result
