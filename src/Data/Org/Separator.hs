module Data.Org.Separator (Separator (..)) where

import Data.Org.Parse

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow (TextShow)
import TextShow qualified as TS

data Separator = SPC | EOL | EOF
  deriving (Show, Eq)

instance Parse Separator where
  parser = choice
    [ EOF <$ eof
    , EOL <$ eol
    , SPC <$ space1 <* space ]

instance TextShow Separator where
  showb SPC = " "
  showb EOL = "\n"
  showb EOF = ""
