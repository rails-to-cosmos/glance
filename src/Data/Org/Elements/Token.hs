module Data.Org.Elements.Token (Token(..)) where

import Data.Text (Text, pack)
import Data.Org.Parse
import Data.Org.Identity

import TextShow (TextShow)
import TextShow qualified

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

import Control.Monad (void)

newtype Token = Token Text
  deriving (Show, Eq)

instance Semigroup Token where
  (<>) (Token a) (Token b) = Token (a <> b)

instance Monoid Token where
  mempty = Token (mempty :: Text)

instance Identity Token where
  id (Token token) = TextShow.showt token

instance TextShow Token where
  showb (Token a) = TextShow.fromText a

instance Parse Token where
  parse = do
    let stop = MP.lookAhead (MP.choice [MPC.space1, void MPC.eol, MP.eof])
        word = MP.manyTill MP.anySingle stop
    Token <$> fmap pack word
