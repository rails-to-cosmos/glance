module Data.Org.Elements.Tags (Tags (..)) where

import Data.Text (Text, intercalate)
import Data.List (nub)

import Data.Org.Types
import Data.Char

import TextShow (TextShow)
import TextShow qualified as TS

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

import Control.Monad
import Control.Monad.State qualified as S

import Prelude hiding (unwords, concat, replicate, concatMap)

newtype Tags = Tags [Text]
  deriving (Show, Eq)

instance TextShow Tags where
  showb (Tags []) = TS.fromText ""
  showb (Tags tags) = TS.fromText ":" <> TS.fromText (intercalate ":" tags) <> TS.fromText ":"

instance Semigroup Tags where
  (<>) (Tags a) (Tags b) = Tags (nub a <> b)

instance Monoid Tags where
  mempty = Tags []

tag :: StatelessParser Text
tag = MP.takeWhile1P (Just "tag character") (`elem` keyword) <* MPC.char ':'

keyword :: [Char]
keyword = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_"

instance Parse Tags where
  parse = do
    let stop = MP.lookAhead (MP.try (MP.choice [void MPC.eol, MP.eof]))
    Tags <$> S.lift (MPC.char ':' *> MP.manyTill tag stop)
