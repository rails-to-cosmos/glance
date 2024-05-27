module Data.Org.Elements.Property (Property (..)) where

import Data.Org.State
import Data.Org.Parser
import Data.Org.Elements.Keyword
import Data.Org.Elements.Sentence
import Data.Text (Text)

import Text.Megaparsec.Char

import TextShow (TextShow)
import TextShow qualified as TS

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
  parser = do
    keyword <- char ':' *> (parser :: StatefulParser Keyword) <* char ':' <* space

    guard $ not (isPropertyStackKeyword keyword)

    value <- parser :: StatefulParser Sentence

    case keyword of
      Keyword "CATEGORY" -> State.modify (setCategory (TS.showt value))
      _keyword -> State.modify id

    return $ Property keyword value

instance TextShow Property where
  showb (Property k v) = ":" <> TS.showb k <> ": " <> TS.showb v
