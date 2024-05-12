module Data.Org.Separator (Sep (..)) where

import Data.Org.Element

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow

data Sep = SPC | EOL | EOF
  deriving (Show, Eq)

instance OrgElement Sep where
  parser = choice
    [ EOF <$ eof
    , EOL <$ eol
    , SPC <$ space1 <* space ]

instance TextShow Sep where
  showb SPC = " "
  showb EOL = "\n"
  showb EOF = ""
