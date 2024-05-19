module Data.Org.Property (Property (..)) where

import Data.Org.Element
import Data.Org.Keyword
import Data.Org.Sentence
import Data.Org.Context (metaCategory)
import Data.Text (Text)

import Text.Megaparsec.Char
import TextShow (TextShow, showt, showb)

import Control.Monad
import Control.Monad.State qualified as State

import Prelude hiding (unwords, concat, replicate, concatMap)

data Property = Property !Keyword !Sentence
  deriving (Show, Eq)

reservedKeywords :: [Text]
reservedKeywords = ["PROPERTIES", "END"]

isPropertyStackKeyword :: Keyword -> Bool
isPropertyStackKeyword (Keyword k) = k `elem` reservedKeywords

instance Org Property where
  parser = do
    keyword <- char ':' *> (parser :: OrgParser Keyword) <* char ':' <* space

    guard $ not (isPropertyStackKeyword keyword)

    value <- parser :: OrgParser Sentence

    case keyword of
      Keyword "CATEGORY" -> State.modify (\ctx -> ctx {metaCategory = showt value})
      _keyword -> State.modify id

    return $ Property keyword value

instance TextShow Property where
  showb (Property k v) = ":" <> showb k <> ": " <> showb v
