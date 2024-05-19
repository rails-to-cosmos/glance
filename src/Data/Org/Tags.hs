module Data.Org.Tags (Tags (..)) where

import Data.Text (Text, intercalate)
import Data.List (nub)

import Data.Org.Element
import Data.Char

import TextShow (TextShow, fromText, showb)
import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad
import Control.Monad.State qualified as State

import Prelude hiding (unwords, concat, replicate, concatMap)

newtype Tags = Tags [Text]
  deriving (Show, Eq)

instance TextShow Tags where
  showb (Tags []) = fromText ""
  showb (Tags tags) = fromText ":" <> fromText (intercalate ":" tags) <> fromText ":"

instance Semigroup Tags where
  (<>) (Tags lhs) (Tags rhs) = Tags (nub lhs <> rhs)

instance Monoid Tags where
  mempty = Tags []

tag :: Parser Text
tag = takeWhile1P (Just "tag character") (`elem` keyword) <* char ':'

keyword :: [Char]
keyword = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_"

instance Org Tags where
  parser = do
    let stop = lookAhead (try (choice [void eol, eof]))
    Tags <$> State.lift (char ':' *> manyTill tag stop)
