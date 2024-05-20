module Data.Org.Separator (Sep (..)) where

import Data.Org.Base qualified as Org

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow (TextShow)
import TextShow qualified as TS

data Sep = SPC | EOL | EOF
  deriving (Show, Eq)

instance Org.Parse Sep where
  parser = choice
    [ EOF <$ eof
    , EOL <$ eol
    , SPC <$ space1 <* space ]

instance TextShow Sep where
  showb SPC = " "
  showb EOL = "\n"
  showb EOF = ""
