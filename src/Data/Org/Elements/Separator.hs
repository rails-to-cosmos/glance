module Data.Org.Elements.Separator (Separator (..)) where

import Data.Org.Parse
import Data.Org.Identity (Identity)
import Data.Org.Identity qualified as Identity

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow (TextShow)
import TextShow qualified as TS

data Separator = SPC | EOL | EOF
  deriving (Show, Eq)

instance Identity Separator where
  id SPC = "SPC"
  id EOL = "EOL"
  id EOF = "EOF"

instance Parse Separator where
  parser = choice
    [ EOF <$ eof
    , EOL <$ eol
    , SPC <$ space1 <* space ]

instance TextShow Separator where
  showb SPC = " "
  showb EOL = "\n"
  showb EOF = ""
