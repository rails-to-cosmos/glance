{-# LANGUAGE TypeFamilies #-}

module Data.Org.Title (OrgTitle (..)) where

import Control.Monad
import Data.Text (concat)

import Data.Org.Base
import Data.Org.Context
import Data.Org.PlainText
import Data.Org.Tags
import Data.Org.Timestamp

import TextShow (TextShow, showb, showt, fromText)

import Text.Megaparsec
import Text.Megaparsec.Char

import Prelude hiding (concat)

newtype OrgTitle = OrgTitle [OrgTitleElement]
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
  mempty = OrgTitle []

instance TextShow OrgTitle where
  showb (OrgTitle xs) = fromText (concat (map showt xs))

instance OrgElement OrgTitle where
  type StateType OrgTitle = OrgContext

  parser ctx = do
    let stop = lookAhead $ void eol <|> eof
    elements <- manyTill ( choice
                         [ OrgTitleTags <$> (try (parser ctx) :: Parser OrgTags)
                         , OrgTitleTimestamp <$> (try (parser ctx) :: Parser OrgTimestamp)
                         , OrgTitleText <$> (parser ctx :: Parser PlainText)
                         ]
                         ) stop
    return $ OrgTitle elements

  modifier (OrgTitle ((OrgTitleTags x) : xs)) ctx = modifier (OrgTitle xs) (modifier x ctx)
  modifier (OrgTitle ((OrgTitleTimestamp x) : xs)) ctx = modifier (OrgTitle xs) (modifier x ctx)
  modifier (OrgTitle ((OrgTitleText x) : xs)) ctx = modifier (OrgTitle xs) (modifier x ctx)
  modifier (OrgTitle []) ctx = ctx
