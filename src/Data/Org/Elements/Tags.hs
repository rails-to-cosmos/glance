module Data.Org.Elements.Tags (Tags (..)) where

import Data.Text (Text, intercalate)
import Data.List (nub)

import Data.Org.Parse
import Data.Char

import TextShow (TextShow)
import TextShow qualified as TS

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad
import Control.Monad.State qualified as S

import Prelude hiding (unwords, concat, replicate, concatMap)

newtype Tags = Tags [Text]
  deriving (Show, Eq)

instance TextShow Tags where
  showb (Tags []) = TS.fromText ""
  showb (Tags tags) = TS.fromText ":" <> TS.fromText (intercalate ":" tags) <> TS.fromText ":"

instance Semigroup Tags where
  (<>) (Tags lhs) (Tags rhs) = Tags (nub lhs <> rhs)

instance Monoid Tags where
  mempty = Tags []

tag :: StatelessParser Text
tag = takeWhile1P (Just "tag character") (`elem` keyword) <* char ':'

keyword :: [Char]
keyword = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_"

instance Parse Tags where
  parser = do
    let stop = lookAhead (try (choice [void eol, eof]))
    Tags <$> S.lift (char ':' *> manyTill tag stop)
