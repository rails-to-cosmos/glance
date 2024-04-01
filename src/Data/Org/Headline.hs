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

data OrgHeadline = OrgHeadline { indent :: OrgIndent
                               , todo :: OrgTodo
                               , priority :: OrgPriority
                               , title :: OrgTitle
                               , tags :: OrgTags
                               , properties :: OrgPropertyBlock
                               } deriving (Show, Eq)

instance Semigroup OrgHeadline where
  (<>) lhs rhs = OrgHeadline { indent = indent lhs <> indent rhs
                             , todo = todo lhs <> todo rhs
                             , priority = priority lhs <> priority rhs
                             , title = title lhs <> title rhs
                             , tags = tags lhs <> tags rhs
                             , properties = properties lhs <> properties rhs
                             }

instance Monoid OrgHeadline where
  mempty = OrgHeadline { indent = mempty :: OrgIndent
                       , todo = mempty :: OrgTodo
                       , priority = mempty :: OrgPriority
                       , title = mempty :: OrgTitle
                       , tags = mempty :: OrgTags
                       , properties = mempty :: OrgPropertyBlock
                       }

instance TextShow OrgHeadline where
  showb headline = fromText (replicate i "*") <> showbSpace <> showb (title headline) <> tagString
    where OrgIndent i = indent headline
          tagString = case tags headline of
            OrgTags [] -> fromText ""
            t -> showbSpace <> showb t

instance OrgElement OrgHeadline where
  parser = do
    indent' <- parser :: OrgParser OrgIndent
    todo' <- option (mempty :: OrgTodo) (parser :: OrgParser OrgTodo)
    priority' <- option (mempty :: OrgPriority) (parser :: OrgParser OrgPriority)
    title' <- option (mempty :: OrgTitle) (parser :: OrgParser OrgTitle)
    tags' <- option (mempty :: OrgTags) (parser :: OrgParser OrgTags)
    void eol <|> eof
    properties' <- option (mempty :: OrgPropertyBlock) (try parser :: OrgParser OrgPropertyBlock)

    return OrgHeadline { indent = indent'
                       , todo = todo'
                       , priority = priority'
                       , title = title'
                       , tags = tags'
                       , properties = properties'
                       }
