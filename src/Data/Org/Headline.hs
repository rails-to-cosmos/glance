{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Headline (OrgHeadline (..)) where

import Control.Monad
import Data.Org.Element
import Data.Org.Indent
import Data.Org.Priority
import Data.Org.PropertyBlock
import Data.Org.Tags
import Data.Org.Title
import Data.Org.Todo
import Data.Text (replicate)
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
  }
  deriving (Show, Eq)

instance Semigroup OrgHeadline where
  (<>) lhs rhs =
    OrgHeadline
      { indent = indent lhs <> indent rhs,
        todo = todo lhs <> todo rhs,
        priority = priority lhs <> priority rhs,
        title = title lhs <> title rhs,
        tags = tags lhs <> tags rhs,
        properties = properties lhs <> properties rhs
      }

instance Monoid OrgHeadline where
  mempty =
    OrgHeadline
      { indent = mempty :: OrgIndent,
        todo = mempty :: OrgTodo,
        priority = mempty :: OrgPriority,
        title = mempty :: OrgTitle,
        tags = mempty :: OrgTags,
        properties = mempty :: OrgPropertyBlock
      }

instance TextShow OrgHeadline where
  showb headline =
    fromText (replicate i "*")
      <> showbSpace
      <> showb (title headline)
      <> tagString
    where
      OrgIndent i = indent headline
      tagString = case tags headline of
        OrgTags [] -> fromText ""
        t -> showbSpace <> showb t

instance OrgElement OrgHeadline where
  parser ctx = do
    indent' <- parser ctx :: Parser OrgIndent
    void space

    todo' <- parser ctx :: Parser OrgTodo
    void space

    priority' <- parser ctx :: Parser OrgPriority
    void space

    title' <- parser ctx :: Parser OrgTitle
    tags' <- option (mempty :: OrgTags) (parser ctx :: Parser OrgTags)

    properties <- option (mempty :: OrgPropertyBlock) (newline *> (parser ctx :: Parser OrgPropertyBlock))

    return
      OrgHeadline
        { indent = indent',
          todo = todo',
          priority = priority',
          title = title',
          tags = tags',
          properties = properties
        }

  modifyState (OrgHeadline { title = title, properties = properties }) =
    modifyState properties . modifyState title
