module Data.Org.Separator (Separator (..)) where

import Data.Org.Element

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow

data Separator = SPC | EOL | EOF
  deriving (Show, Eq)

instance OrgElement Separator where
  parser = choice
    [ EOF <$ eof
    , EOL <$ eol
    , SPC <$ space1 <* space ]

instance TextShow Separator where
  showb SPC = " "
  showb EOL = "\n"
  showb EOF = ""
