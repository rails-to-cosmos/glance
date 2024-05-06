module Data.Org.Tags (OrgTags (..)) where

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

newtype OrgTags = OrgTags [Text]
  deriving (Show, Eq)

instance TextShow OrgTags where
  showb (OrgTags []) = fromText ""
  showb (OrgTags tags) = fromText ":" <> fromText (intercalate ":" tags) <> fromText ":"

instance Semigroup OrgTags where
  (<>) (OrgTags lhs) (OrgTags rhs) = OrgTags (nub lhs <> rhs)

instance Monoid OrgTags where
  mempty = OrgTags []

tag :: Parser Text
tag = takeWhile1P (Just "tag character") (`elem` keyword) <* char ':'

keyword :: [Char]
keyword = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_"

instance OrgElement OrgTags where
  parser = do
    let stop = lookAhead (try (choice [void eol, eof]))
    OrgTags <$> State.lift (char ':' *> manyTill tag stop)
