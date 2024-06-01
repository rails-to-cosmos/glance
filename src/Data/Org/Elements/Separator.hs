module Data.Org.Elements.Separator (Separator (..)) where

import Data.Org.Parse
import Data.Org.Identity (Identity)
import Data.Org.Identity qualified as Identity

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC

import TextShow (TextShow)
import TextShow qualified as TS

data Separator = SPC | EOL | EOF
  deriving (Show, Eq)

instance Identity Separator where
  identity SPC = "SPC"
  identity EOL = "EOL"
  identity EOF = "EOF"

instance Parse Separator where
  parse = MP.choice [ EOF <$ MP.eof
                    , EOL <$ MPC.eol
                    , SPC <$ MPC.space1 <* MPC.space ]

instance TextShow Separator where
  showb SPC = " "
  showb EOL = "\n"
  showb EOF = ""
