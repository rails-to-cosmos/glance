{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Separator (OrgSeparator (..)) where

import Data.Org.Element

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow

data OrgSeparator = SPC | EOL | EOF
  deriving (Show, Eq)

instance OrgElement OrgSeparator where
  parser = choice
    [ SPC <$ space1 <* space
    , EOL <$ eol
    , EOF <$ eof
    ]

instance TextShow OrgSeparator where
  showb SPC = " "
  showb EOL = "\n"
  showb EOF = ""
