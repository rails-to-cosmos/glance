{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Title (OrgTitle (..), OrgTitleElement (..)) where

import Control.Monad
import Data.Text (Text, pack, strip)

import Data.Org.Element
import Data.Org.PlainText
import Data.Org.Tags
import Data.Org.Timestamp

import TextShow (TextShow, showb, fromText)

import Text.Megaparsec
import Text.Megaparsec.Char

import Prelude hiding (concat)

newtype OrgTitle = OrgTitle Text
  deriving (Show, Eq)

data OrgTitleElement = OrgTitleTags OrgTags
                     | OrgTitleTimestamp OrgTimestamp
                     | OrgTitleText PlainText
  deriving (Show, Eq)

instance TextShow OrgTitleElement where
  showb (OrgTitleTags t) = showb t
  showb (OrgTitleTimestamp t) = showb t
  showb (OrgTitleText t) = showb t

instance Semigroup OrgTitle where
  (<>) (OrgTitle lhs) (OrgTitle rhs) = OrgTitle (lhs <> rhs)

instance Monoid OrgTitle where
  mempty = OrgTitle ""

instance TextShow OrgTitle where
  showb (OrgTitle x) = fromText x

instance OrgElement OrgTitle where
  parser = do
    let end = lookAhead $ choice [ try (void (parser :: OrgParser OrgTags))
                                 , void eol
                                 , eof
                                 ]

    elements <- manyTill anySingle end

    return $ OrgTitle $ strip $ pack elements
