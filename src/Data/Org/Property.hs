module Data.Org.Property (Property (..)) where

import Data.Org.Element
import Data.Org.Keyword
import Data.Org.Sentence
import Data.Text (Text)

import Text.Megaparsec.Char
import TextShow

import Control.Monad

import Prelude hiding (unwords, concat, replicate, concatMap)

data Property = Property !Keyword !Sentence
  deriving (Show, Eq)

reservedKeywords :: [Text]
reservedKeywords = ["PROPERTIES", "END"]

isPropertyStackKeyword :: Keyword -> Bool
isPropertyStackKeyword (Keyword k) = k `elem` reservedKeywords

instance OrgElement Property where
  parser = do
    keyword <- char ':' *> (parser :: OrgParser Keyword) <* char ':'
    space
    guard $ not (isPropertyStackKeyword keyword)

    value <- parser :: OrgParser Sentence

    -- case keyword of
    --   Keyword "CATEGORY" -> State.modify (\ctx -> ctx {metaCategory = value})
    --   _keyword -> State.modify id

    return $ Property keyword value

instance TextShow Property where
  showb (Property k v) = ":" <> showb k <> ": " <> showb v
