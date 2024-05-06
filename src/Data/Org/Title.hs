{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Title (OrgTitle (..)) where

import Control.Monad

import Data.Org.Element
import Data.Org.PlainText
import Data.Org.Tags
import Data.Org.Timestamp
import Data.Org.Separator

import TextShow (TextShow, showb)

import Text.Megaparsec
import Text.Megaparsec.Char

import Prelude hiding (concat)

newtype OrgTitle = OrgTitle [OrgTitleElement]
  deriving (Show, Eq)

data OrgTitleElement = OrgTitleText !PlainText
                     | OrgTitleTags !OrgTags
                     | OrgTitleTimestamp !OrgTimestamp
                     | OrgTitleSeparator !OrgSeparator
  deriving (Show, Eq)

instance TextShow OrgTitleElement where
  showb (OrgTitleText x) = showb x
  showb (OrgTitleTags x) = showb x
  showb (OrgTitleTimestamp x) = showb x
  showb (OrgTitleSeparator x) = showb x

instance Semigroup OrgTitle where
  (<>) (OrgTitle lhs) (OrgTitle rhs) = OrgTitle (lhs <> rhs)

instance Monoid OrgTitle where
  mempty = OrgTitle []

instance TextShow OrgTitle where
  showb (OrgTitle elems) = showb elems

instance OrgElement OrgTitle where
  parser = do
    let stopParsers = choice [ void (parser :: OrgParser OrgTags)
                             , void eol
                             , eof ]

        elemParsers = choice [ OrgTitleSeparator <$> try parser
                             , OrgTitleTimestamp <$> try parser
                             , OrgTitleText <$> try parser ]

    elems <- manyTill elemParsers (lookAhead stopParsers)

    return $ OrgTitle elems
