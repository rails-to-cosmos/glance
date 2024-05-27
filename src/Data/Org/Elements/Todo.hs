module Data.Org.Elements.Todo (Todo (..)) where

import Data.Org.State
import Data.Text (Text)
import Data.Org.Parse
import Data.Org.Elements.Keyword

import TextShow (TextShow)
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

instance Parse Todo where
  parser = Todo <$> optional (try todo)

instance TextShow Todo where
  showb (Todo Nothing) = TS.showbSpace
  showb (Todo (Just state)) = TS.showbSpace <> TS.fromText state <> TS.showbSpace

todo :: StatefulParser Text
todo = do
  ctx <- State.get
  Keyword result <- (parser :: StatefulParser Keyword) <* space
  guard $ inTodo result ctx
  return result
