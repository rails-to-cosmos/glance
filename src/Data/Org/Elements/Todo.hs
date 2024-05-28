module Data.Org.Elements.Todo (Todo (..)) where

import Data.Org.State
import Data.Text (Text)
import Data.Org.Parse
import Data.Org.Elements.Keyword

import TextShow (TextShow)
import TextShow qualified

import Text.Megaparsec qualified as MPS
import Text.Megaparsec.Char qualified as MPSChar

import Control.Monad
import Control.Monad.State qualified as State

newtype Todo = Todo (Maybe Text)
  deriving (Show, Eq)

instance Semigroup Todo where
  (<>) (Todo a) (Todo b) = Todo (a <> b)

instance Monoid Todo where
  mempty = Todo Nothing

instance Parse Todo where
  parse = Todo <$> MPS.optional (MPS.try todo)

instance TextShow Todo where
  showb (Todo Nothing) = TextShow.showbSpace
  showb (Todo (Just state)) = TextShow.showbSpace <> TextShow.fromText state <> TextShow.showbSpace

todo :: StatefulParser Text
todo = do
  ctx <- State.get
  Keyword result <- (parse :: StatefulParser Keyword) <* MPSChar.space
  guard $ inTodo result ctx
  return result
