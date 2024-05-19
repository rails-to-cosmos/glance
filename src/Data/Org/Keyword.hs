module Data.Org.Keyword (Keyword (..)) where

import Data.Org.Base qualified as Org
import Data.Text (Text, pack, toUpper)
import Data.Char (isAlpha)

import Text.Megaparsec

import TextShow (TextShow)
import TextShow qualified as TS

import Prelude hiding (unwords, concat, replicate, concatMap)

newtype Keyword = Keyword Text
  deriving (Show, Eq)

instance TextShow Keyword where
  showb (Keyword k) = TS.fromText k

instance Org.Base Keyword where
  parser = do
    let keyword = some (satisfy (\c -> isAlpha c || c == '_'))
    Keyword . toUpper . pack <$> keyword
