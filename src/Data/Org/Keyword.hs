module Data.Org.Keyword (OrgKeyword (..)) where

import Data.Org.Element
import Data.Text (Text, pack, toUpper)
import Data.Char (isAlpha)

import Text.Megaparsec

import TextShow (TextShow, fromText, showb)

import Prelude hiding (unwords, concat, replicate, concatMap)

newtype OrgKeyword = OrgKeyword Text
  deriving (Show, Eq)

instance TextShow OrgKeyword where
  showb (OrgKeyword k) = fromText k

instance OrgElement OrgKeyword where
  parser = do
    let keyword = some (satisfy (\c -> isAlpha c || c == '_'))
    OrgKeyword . toUpper . pack <$> keyword
