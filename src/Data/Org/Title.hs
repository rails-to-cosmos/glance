module Data.Org.Title ( OrgTitle (..)
                      , OrgTitleElement (..)) where

import Control.Monad

import Data.Org.Element
import Data.Org.PlainText
import Data.Org.Tags
import Data.Org.Timestamp
import Data.Org.Separator

import TextShow

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
  showb (OrgTitleText (PlainText x)) = fromText x
  showb (OrgTitleTags x) = showb x
  showb (OrgTitleTimestamp x) = showb x
  showb (OrgTitleSeparator x) = showb x

instance Semigroup OrgTitle where
  (<>) (OrgTitle lhs) (OrgTitle rhs) = OrgTitle (lhs <> rhs)

instance Monoid OrgTitle where
  mempty = OrgTitle []

instance TextShow OrgTitle where
  showb (OrgTitle []) = ""
  showb (OrgTitle (x:xs)) = showb x <> showb (OrgTitle xs)

instance OrgElement OrgTitle where
  parser = do
    let stopParsers = choice [ void eol, eof ]
        elemParsers = choice [ OrgTitleSeparator <$> try parser
                             , OrgTitleTimestamp <$> try parser
                             , OrgTitleTags <$> try parser
                             , OrgTitleText <$> try parser ]

    elems <- manyTill elemParsers stopParsers

    return (OrgTitle elems)
