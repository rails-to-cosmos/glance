{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Headline (OrgHeadline (..)) where

import Data.Text (replicate)

import Data.Org.Base
import Data.Org.Context
import Data.Org.Indent
import Data.Org.Todo
import Data.Org.Priority
import Data.Org.Title
import Data.Org.Tags
import Data.Org.PropertyBlock

import Control.Monad

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow

import Prelude hiding (replicate)

data OrgHeadline = OrgHeadline
  { indent :: OrgIndent,
    todo :: OrgTodo,
    priority :: OrgPriority,
    title :: OrgTitle,
    tags :: OrgTags,
    properties :: OrgPropertyBlock
  } deriving (Show, Eq)

instance Semigroup OrgHeadline where
  (<>) lhs rhs = OrgHeadline
    { indent = indent lhs <> indent rhs
    , todo = todo lhs <> todo rhs
    , priority = priority lhs <> priority rhs
    , title = title lhs <> title rhs
    , tags = tags lhs <> tags rhs
    , properties = properties lhs <> properties rhs
    }

instance Monoid OrgHeadline where
  mempty = OrgHeadline
    { indent = mempty :: OrgIndent
    , todo = mempty :: OrgTodo
    , priority = mempty :: OrgPriority
    , title = mempty :: OrgTitle
    , tags = mempty :: OrgTags
    , properties = mempty :: OrgPropertyBlock
    }

instance TextShow OrgHeadline where
    showb h = showb (replicate i "*")
      <> showb (title h)
      <> showbSpace
      <> showb (tags h)
      where OrgIndent i = indent h

instance OrgElement OrgHeadline where
  type StateType OrgHeadline = OrgContext

  parser ctx = do
    i <- parser ctx :: Parser OrgIndent
    t <- parser ctx :: Parser OrgTodo
    p <- parser ctx :: Parser OrgPriority
    s <- parser ctx :: Parser OrgTitle

    void (many eol)

    -- properties <- parser ctx :: Parser OrgPropertyBlock
    -- properties <- option (mempty :: OrgPropertyBlock) (many newline *> (parser ctx :: Parser OrgPropertyBlock))

    return OrgHeadline { indent = i
                       , todo = t
                       , priority = p
                       , title = s
                       , tags = mempty :: OrgTags
                       , properties = mempty :: OrgPropertyBlock
                       }

  modifyState (OrgHeadline {title=t}) = modifyState t
