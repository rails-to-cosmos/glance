{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Data.Org.Generic (OrgGeneric (..)) where

import Data.Org.Base
import Data.Org.Context
import Data.Org.Tags
import Data.Org.Timestamp
import Data.Org.PlainText
import Data.Org.Headline
import Data.Org.PropertyBlock
import Data.Org.Pragma
import TextShow (TextShow, showb)

import Text.Megaparsec
import Text.Megaparsec.Char

newtype OrgGeneric = OrgGeneric [OrgGenericElement]
  deriving (Show, Eq)

data OrgGenericElement = OrgGenericHeadline OrgHeadline
                       | OrgGenericPragma OrgPragma
                       | OrgGenericPropertyBlock OrgPropertyBlock
                       | OrgGenericTags OrgTags
                       | OrgGenericTimestamp OrgTimestamp
                       | OrgGenericText PlainText
  deriving (Show, Eq)

instance TextShow OrgGenericElement where
  showb = \case
    OrgGenericTags t          -> showb t
    OrgGenericTimestamp t     -> showb t
    OrgGenericText t          -> showb t
    OrgGenericPragma t        -> showb t
    OrgGenericPropertyBlock t -> showb t
    OrgGenericHeadline t      -> showb t

instance OrgElement OrgGeneric where
  type StateType OrgGeneric = OrgContext

  parser ctx = do
    let elements = [ OrgGenericHeadline  <$> (try (parser ctx) :: Parser OrgHeadline)
                   , OrgGenericPropertyBlock <$> (try (parser ctx) :: Parser OrgPropertyBlock)
                   , OrgGenericPragma    <$> (try (parser ctx) :: Parser OrgPragma)
                   , OrgGenericTimestamp <$> (try (parser ctx) :: Parser OrgTimestamp)
                   , OrgGenericTags      <$> (try (parser ctx) :: Parser OrgTags)
                   , OrgGenericText      <$> (parser ctx       :: Parser PlainText)
                   ]
    OrgGeneric <$> choice elements `sepEndBy` eol

  modifier (OrgGeneric (e : xs)) ctx = modifier (OrgGeneric xs) newCtx
      where newCtx = case e of
              OrgGenericTags x       -> modifier x ctx
              OrgGenericTimestamp x  -> modifier x ctx
              OrgGenericText x       -> modifier x ctx
              OrgGenericPragma x     -> modifier x ctx
              OrgGenericPropertyBlock x -> modifier x ctx
              OrgGenericHeadline x   -> modifier x ctx

  modifier (OrgGeneric []) ctx = ctx
