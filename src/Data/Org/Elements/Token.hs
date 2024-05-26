module Data.Org.Elements.Token (Token(..)) where

import Data.Text (Text, pack)
import Data.Org.Parser

import TextShow (TextShow)
import TextShow qualified as TS

import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char (space1, eol)

import Control.Monad (void)

newtype Token = Token Text
  deriving (Show, Eq)

instance Semigroup Token where
  (<>) (Token a) (Token b) = Token (a <> b)

instance Monoid Token where
  mempty = Token (mempty :: Text)

instance TextShow Token where
  showb (Token a) = TS.fromText a

instance Parseable Token where
  parser = do
    let stop = lookAhead (choice [space1, void eol, eof])
        word = manyTill anySingle stop
    Token <$> fmap pack word
