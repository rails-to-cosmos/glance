module Data.Org.Elements.Priority (Priority (..)) where

import Data.Char (ord)
import Data.Org.Parser

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Data.Text.Lazy.Builder qualified as B

import TextShow (TextShow)
import TextShow qualified as TS

newtype Priority = Priority (Maybe Char)
  deriving (Show, Eq)

instance Semigroup Priority where
  (<>) (Priority a) (Priority b) = Priority (minByOrd a b)

instance Monoid Priority where
  mempty = Priority Nothing

instance TextShow Priority where
  showb (Priority Nothing) = TS.fromText ""
  showb (Priority (Just priority)) = "[#" <> B.singleton priority <> "]" <> TS.showbSpace

instance Parse Priority where
  parse = do
    priority <- MP.optional (MPC.char '[' *> MPC.char '#' *> MPC.letterChar <* MPC.char ']' <* MPC.space)
    return (Priority priority)

minByOrd :: Maybe Char -> Maybe Char -> Maybe Char
minByOrd Nothing b = b
minByOrd a Nothing = a
minByOrd (Just a) (Just b)
  | ord a <= ord b = Just a
  | otherwise     = Just b
