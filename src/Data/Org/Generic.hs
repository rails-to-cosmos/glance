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
    let elements = [ OrgGenericHeadline      <$> (try (parser ctx) :: Parser OrgHeadline)
                   , OrgGenericPropertyBlock <$> (try (parser ctx) :: Parser OrgPropertyBlock)
                   , OrgGenericPragma        <$> (try (parser ctx) :: Parser OrgPragma)
                   , OrgGenericTimestamp     <$> (try (parser ctx) :: Parser OrgTimestamp)
                   , OrgGenericTags          <$> (try (parser ctx) :: Parser OrgTags)
                   , OrgGenericText          <$> (parser ctx       :: Parser PlainText)
                   ]
    OrgGeneric <$> choice elements `sepEndBy` eol

  modifyState (OrgGeneric (e : xs)) ctx = modifyState (OrgGeneric xs) ctx'
      where ctx' = case e of
              OrgGenericTags x          -> modifyState x ctx
              OrgGenericTimestamp x     -> modifyState x ctx
              OrgGenericText x          -> modifyState x ctx
              OrgGenericPragma x        -> modifyState x ctx
              OrgGenericPropertyBlock x -> modifyState x ctx
              OrgGenericHeadline x      -> modifyState x ctx

  modifyState (OrgGeneric []) ctx = ctx
