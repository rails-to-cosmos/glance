module Data.Org.Todo (Todo (..)) where

import Data.Text (Text)
import Data.Org.Element
import Data.Org.Context
import Data.Org.Keyword

import TextShow qualified as TS
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

instance Org Todo where
  parser = Todo <$> optional (try todo)

instance TS.TextShow Todo where
  showb (Todo Nothing) = TS.showbSpace
  showb (Todo (Just state)) = TS.showbSpace <> TS.fromText state <> TS.showbSpace

todo :: OrgParser Text
todo = do
  ctx <- State.get
  Keyword result <- (parser :: OrgParser Keyword) <* space
  guard $ result `elem` (metaTodoActive ctx <> metaTodoInactive ctx)
  return result
