module Data.Org.Elements.Keyword (Keyword (..), fromText) where

import Data.Org.Types

import Data.Text (Text, pack, toUpper)
import Data.Char (isAlpha)

import Text.Megaparsec qualified as MP

import TextShow (TextShow)
import TextShow qualified as TS

import Prelude hiding (unwords, concat, replicate, concatMap)

newtype Keyword = Keyword Text
  deriving (Show, Eq)

instance TextShow Keyword where
  showb (Keyword k) = TS.fromText k

instance Parse Keyword where
  parse = do
    let keyword = MP.some (MP.satisfy (\c -> isAlpha c || c == '_'))
    Keyword . toUpper . pack <$> keyword

fromText :: Text -> Keyword
fromText = Keyword
