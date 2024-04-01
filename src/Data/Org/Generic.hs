{-# LANGUAGE LambdaCase #-}

module Data.Org.Generic (OrgGenericElement (..)) where

import Data.Org.Element
import Data.Org.Headline
import Data.Org.PlainText
import Data.Org.Pragma
import Data.Org.PropertyBlock
import Data.Org.Tags
import Data.Org.Timestamp
import Text.Megaparsec
import TextShow (TextShow, showb)

data OrgGenericElement = OrgGenericHeadline OrgHeadline
                       | OrgGenericPragma OrgPragma
                       | OrgGenericPropertyBlock OrgPropertyBlock
                       | OrgGenericTags OrgTags
                       | OrgGenericTimestamp OrgTimestamp
                       | OrgGenericText PlainText
                       deriving (Show, Eq)

instance TextShow OrgGenericElement where
  showb = \case
    OrgGenericTags t -> showb t
    OrgGenericTimestamp t -> showb t
    OrgGenericText t -> showb t
    OrgGenericPragma t -> showb t
    OrgGenericPropertyBlock t -> showb t
    OrgGenericHeadline t -> showb t

instance OrgElement OrgGenericElement where
  parser = choice [ OrgGenericHeadline <$> try parser
                  , OrgGenericPragma <$> try parser
                  , OrgGenericTimestamp <$> try parser
                  , OrgGenericText <$> parser
                  ]
