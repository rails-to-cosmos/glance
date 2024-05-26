module Data.Org.Elements.Separator (Separator (..)) where

import Data.Org.Parser

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow (TextShow)
import TextShow qualified as TS

data Separator = SPC | EOL | EOF
  deriving (Show, Eq)

instance Parseable Separator where
  parser = choice
    [ EOF <$ eof
    , EOL <$ eol
    , SPC <$ space1 <* space ]

instance TextShow Separator where
  showb SPC = " "
  showb EOL = "\n"
  showb EOF = ""
