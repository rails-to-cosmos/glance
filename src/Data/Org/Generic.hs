{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Data.Org.Generic (OrgGenericElement (..)) where

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

instance OrgElement OrgGenericElement where
  type StateType OrgGenericElement = OrgContext

  parser ctx = do
    let elements = [ OrgGenericHeadline      <$> (try (parser ctx) :: Parser OrgHeadline)
                   , OrgGenericPropertyBlock <$> (try (parser ctx) :: Parser OrgPropertyBlock)
                   , OrgGenericPragma        <$> (try (parser ctx) :: Parser OrgPragma)
                   , OrgGenericTimestamp     <$> (try (parser ctx) :: Parser OrgTimestamp)
                   , OrgGenericTags          <$> (try (parser ctx) :: Parser OrgTags)
                   , OrgGenericText          <$> (parser ctx       :: Parser PlainText)
                   ]
    choice elements

  modifyState (OrgGenericTags x) ctx = modifyState x ctx
  modifyState (OrgGenericTimestamp x) ctx = modifyState x ctx
  modifyState (OrgGenericText x) ctx = modifyState x ctx
  modifyState (OrgGenericPragma x) ctx = modifyState x ctx
  modifyState (OrgGenericPropertyBlock x) ctx = modifyState x ctx
  modifyState (OrgGenericHeadline x) ctx = modifyState x ctx
