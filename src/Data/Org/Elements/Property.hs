module Data.Org.Elements.Property (Property (..)) where

import Data.Org.Context
import Data.Org.Parser
import Data.Org.Elements.Keyword
import Data.Org.Elements.Sentence

import Text.Megaparsec.Char qualified as MPC

import TextShow (TextShow)
import TextShow qualified

import Control.Monad
import Control.Monad.State qualified as State

import Prelude hiding (unwords, concat, replicate, concatMap)

data Property = Property { key :: !Keyword
                         , val :: !Sentence }
  deriving (Show, Eq)

reserved :: Keyword -> Bool
reserved (Keyword k) = k `elem` ["PROPERTIES", "END"]

instance Parse Property where
  parse = do
    keyword <- MPC.char ':' *> (parse :: StatefulParser Keyword) <* MPC.char ':' <* MPC.space

    guard $ not (reserved keyword)

    value <- parse :: StatefulParser Sentence

    case keyword of
      Keyword "CATEGORY" -> State.modify $ setCategory $ TextShow.showt value
      _keyword -> State.modify id

    return $ Property keyword value

instance TextShow Property where
  showb (Property {..}) = ":" <> TextShow.showb key <> ": " <> TextShow.showb val
