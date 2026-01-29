module Data.Org.Elements.Todo (Todo (..)) where

import Data.Org.Context
import Data.Text (Text)
import Data.Org.Parser
import Data.Org.Elements.Keyword

import TextShow (TextShow)
import TextShow qualified

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

import Control.Monad
import Control.Monad.State qualified as State

data Todo = Todo { name :: Text, active :: Bool }
  deriving (Show, Eq)

instance Parse Todo where
  parse = do
    ctx <- State.get
    Keyword result <- (parse :: StatefulParser Keyword) <* MPC.space
    guard $ inTodo result ctx
    return Todo { name = result
                , active = result `elem` todoActive ctx
                }

instance TextShow Todo where
  showb a = TextShow.fromText (name a)
