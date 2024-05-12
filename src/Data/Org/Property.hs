module Data.Org.Property (OrgProperty (..)) where

import Data.Org.Element
import Data.Org.Context
import Data.Org.Keyword
import Data.Org.PlainText
import Data.Text (Text)

import Control.Monad.State qualified as State

import Text.Megaparsec.Char
import TextShow

import Control.Monad

import Prelude hiding (unwords, concat, replicate, concatMap)

data OrgProperty = OrgProperty !OrgKeyword !Text
  deriving (Show, Eq)

propertyStackKeywords :: [Text]
propertyStackKeywords = ["PROPERTIES", "END"]

isPropertyStackKeyword :: OrgKeyword -> Bool
isPropertyStackKeyword (OrgKeyword k) = k `elem` propertyStackKeywords

instance OrgElement OrgProperty where
  parser = do
    keyword <- char ':' *> (parser :: OrgParser OrgKeyword) <* char ':'
    space
    guard $ not (isPropertyStackKeyword keyword)

    PlainText value <- parser :: OrgParser PlainText

    case keyword of
      OrgKeyword "CATEGORY" -> State.modify (\ctx -> ctx {metaCategory = value})
      _keyword -> State.modify id

    return $ OrgProperty keyword value

instance TextShow OrgProperty where
  showb (OrgProperty k v) = ":" <> showb k <> ": " <> fromText v
