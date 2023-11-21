{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Tags (OrgTags (..), tagCtrl) where

import Data.Text (Text, intercalate, pack)
import Data.List (nub)

import Data.Org.Base
import Data.Org.Context

import TextShow (TextShow, fromText, showb)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Prelude hiding (unwords, concat, replicate, concatMap)

newtype OrgTags = OrgTags [Text]
  deriving (Show, Eq)

instance TextShow OrgTags where
  showb (OrgTags []) = fromText ""
  showb (OrgTags tags) = fromText ":" <> fromText (intercalate ":" tags) <> fromText ":"

instance Semigroup OrgTags where
  (<>) (OrgTags lhs) (OrgTags rhs) = OrgTags (nub lhs <> rhs)

instance Monoid OrgTags where
  mempty = OrgTags []

instance OrgElement OrgTags where
  type StateType OrgTags = OrgContext

  parser _ = do
    void tagCtrl
    OrgTags <$> manyTill tagParser eof

  modifyState _ ctx = ctx

  -- modifyState xs ctx = ctx {headline = h'}
  --   where h = headline ctx
  --         h' = h {tags = tags h <> xs}

-- modifyState :: OrgElement -> OrgContext -> OrgContext
-- modifyState (ETags tags') ctx = ctx {headline = h'}
--   where h = headline ctx
--         t' = tags h
--         h' = h {tags = L.nub (t' ++ tags')}
-- modifyState _ ctx = ctx

tagCtrl :: Parser Char
tagCtrl = char ':'

tagParser :: Parser Text
tagParser = do
  tag <- manyTill (anySingleBut ' ') (void tagCtrl)
  return $ pack tag
