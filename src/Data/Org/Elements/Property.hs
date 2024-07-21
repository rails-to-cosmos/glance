module Data.Org.Elements.Property (Property (..)) where

import Data.Org.State
import Data.Org.Parser
import Data.Org.Elements.Keyword
import Data.Org.Elements.Sentence
import Data.Text (Text)

import Text.Megaparsec.Char qualified as MPC

import TextShow (TextShow)
import TextShow qualified

import Control.Monad
import Control.Monad.State qualified as State

import Prelude hiding (unwords, concat, replicate, concatMap)

data Property = Property !Keyword !Sentence
  deriving (Show, Eq)

reservedKeywords :: [Text]
reservedKeywords = ["PROPERTIES", "END"]

isPropertyStackKeyword :: Keyword -> Bool
isPropertyStackKeyword (Keyword k) = k `elem` reservedKeywords

instance Parse Property where
  parse = do
    keyword <- MPC.char ':' *> (parse :: StatefulParser Keyword) <* MPC.char ':' <* MPC.space

    guard $ not (isPropertyStackKeyword keyword)

    value <- parse :: StatefulParser Sentence

    case keyword of
      Keyword "CATEGORY" -> State.modify (setCategory (TextShow.showt value))
      _keyword -> State.modify id

    return $ Property keyword value

instance TextShow Property where
  showb (Property k v) = ":" <> TextShow.showb k <> ": " <> TextShow.showb v
