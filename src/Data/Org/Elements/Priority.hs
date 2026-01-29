module Data.Org.Elements.Priority (Priority (..)) where

import Data.Org.Types

import Text.Megaparsec.Char qualified as MPC
import Data.Text.Lazy.Builder qualified as B

import TextShow (TextShow)
import TextShow qualified as TS

newtype Priority = Priority Char
  deriving (Show, Eq)

instance TextShow Priority where
  showb (Priority priority) = "[#" <> B.singleton priority <> "]" <> TS.showbSpace

instance Parse Priority where
  parse = do
    priority <- MPC.char '[' *> MPC.char '#' *> MPC.letterChar <* MPC.char ']' <* MPC.space
    return (Priority priority)
