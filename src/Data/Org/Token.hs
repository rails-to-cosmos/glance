module Data.Org.Token (Token(..)) where

import Data.Text (Text, pack)
import Data.Org.Element

import TextShow (TextShow, fromText, showb)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char

import Control.Monad (void)

newtype Token = Token Text
  deriving (Show, Eq)

instance Semigroup Token where
  (<>) (Token a) (Token b) = Token (a <> b)

instance Monoid Token where
  mempty = Token (mempty :: Text)

instance TextShow Token where
  showb (Token a) = fromText a

instance OrgElement Token where
  parser = do
    let stop = lookAhead (choice [space1, void eol, eof])
        word = manyTill anySingle stop
    Token <$> fmap pack word
