{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Data.Org.Generic (OrgGeneric (..)) where

import Data.Org.Base
import Data.Org.Context
import Data.Org.Tags
import Data.Org.Timestamp
import Data.Org.PlainText
import Data.Org.Headline
import Data.Org.Property
import Data.Org.Pragma
import TextShow (TextShow, showb)

import Text.Megaparsec
import Text.Megaparsec.Char

newtype OrgGeneric = OrgGeneric [OrgGenericElement]
  deriving (Show, Eq)

data OrgGenericElement = OrgGenericHeadline OrgHeadline
                       | OrgGenericPragma OrgPragma
                       | OrgGenericProperty OrgProperty
                       | OrgGenericTags OrgTags
                       | OrgGenericTimestamp OrgTimestamp
                       | OrgGenericText PlainText
  deriving (Show, Eq)

instance TextShow OrgGenericElement where
  showb = \case
    OrgGenericTags t         -> showb t
    OrgGenericTimestamp t    -> showb t
    OrgGenericText t         -> showb t
    OrgGenericPragma t       -> showb t
    OrgGenericProperty t     -> showb t
    OrgGenericHeadline t     -> showb t

instance OrgElement OrgGeneric where
  type StateType OrgGeneric = OrgContext

  parser ctx = do
    let elements = [ OrgGenericTags      <$> (try (parser ctx) :: Parser OrgTags)
                   , OrgGenericTimestamp <$> (try (parser ctx) :: Parser OrgTimestamp)
                   , OrgGenericHeadline  <$> (try (parser ctx) :: Parser OrgHeadline)
                   , OrgGenericProperty  <$> (try (parser ctx) :: Parser OrgProperty)
                   , OrgGenericPragma    <$> (try (parser ctx) :: Parser OrgPragma)
                   , OrgGenericText      <$> (parser ctx       :: Parser PlainText)
                   ]
    OrgGeneric <$> choice elements `sepEndBy` eol

  modifier og ctx = case og of
    OrgGeneric (e:xs) -> modifier (OrgGeneric xs) newCtx
      where newCtx = case e of
              OrgGenericTags x       -> modifier x ctx
              OrgGenericTimestamp x  -> modifier x ctx
              OrgGenericText x       -> modifier x ctx
              OrgGenericPragma x     -> modifier x ctx
              OrgGenericProperty x   -> modifier x ctx
              OrgGenericHeadline x   -> modifier x ctx
    OrgGeneric [] -> ctx
