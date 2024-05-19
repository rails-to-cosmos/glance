module Data.Org.Token (Tk(..)) where

import Data.Text (Text, pack)
import Data.Org.Base qualified as Org

import TextShow (TextShow)
import TextShow qualified as TS

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad (void)

newtype Tk = Tk Text
  deriving (Show, Eq)

instance Semigroup Tk where
  (<>) (Tk a) (Tk b) = Tk (a <> b)

instance Monoid Tk where
  mempty = Tk (mempty :: Text)

instance TextShow Tk where
  showb (Tk a) = TS.fromText a

instance Org.Base Tk where
  parser = do
    let stop = lookAhead (choice [space1, void eol, eof])
        word = manyTill anySingle stop
    Tk <$> fmap pack word
