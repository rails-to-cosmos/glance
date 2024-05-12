module Data.Org.Lexeme (Lexeme(..)) where

import           Data.Text (Text, pack)
import           Data.Org.Element

import           TextShow (TextShow, fromText, showb)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Control.Monad (void)

newtype Lexeme = Lexeme Text
  deriving (Show, Eq)

instance Semigroup Lexeme where
  (<>) (Lexeme a) (Lexeme b) = Lexeme (a <> b)

instance Monoid Lexeme where
  mempty = Lexeme (mempty :: Text)

instance TextShow Lexeme where
  showb (Lexeme a) = fromText a

instance OrgElement Lexeme where
  parser = do
    let stop = lookAhead (choice [space1, void eol, eof])
        word = manyTill anySingle stop
    Lexeme <$> fmap pack word
