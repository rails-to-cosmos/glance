module Data.Org.Headline (OrgHeadline (..)) where

import Control.Monad
import Data.Org.Element
import Data.Org.Indent
import Data.Org.Priority
import Data.Org.PropertyBlock
import Data.Org.Title
import Data.Org.Todo
import Data.Org.Timestamp
import Data.Text (replicate)
import Text.Megaparsec
import TextShow
import Prelude hiding (replicate)

data OrgHeadline = OrgHeadline { indent :: !OrgIndent
                               , todo :: !OrgTodo
                               , priority :: !OrgPriority
                               , title :: !OrgTitle
                               , schedule :: !(Maybe OrgTimestamp)
                               , deadline :: !(Maybe OrgTimestamp)
                               , properties :: !OrgPropertyBlock
                               } deriving (Show, Eq)

instance Semigroup OrgHeadline where
  (<>) lhs rhs = OrgHeadline { indent = indent lhs <> indent rhs
                             , todo = todo lhs <> todo rhs
                             , priority = priority lhs <> priority rhs
                             , title = title lhs <> title rhs
                             , schedule = Nothing
                             , deadline = Nothing
                             , properties = properties lhs <> properties rhs }

instance Monoid OrgHeadline where
  mempty = OrgHeadline { indent = mempty :: OrgIndent
                       , todo = mempty :: OrgTodo
                       , priority = mempty :: OrgPriority
                       , title = mempty :: OrgTitle
                       , schedule = Nothing
                       , deadline = Nothing
                       , properties = mempty :: OrgPropertyBlock }

instance TextShow OrgHeadline where
  showb headline = fromText (replicate i "*") <> showbSpace <> showb (title headline)
    where OrgIndent i = indent headline

instance OrgElement OrgHeadline where
  parser = do
    indent' <- parser :: OrgParser OrgIndent
    todo' <- option (mempty :: OrgTodo) (parser :: OrgParser OrgTodo)
    priority' <- option (mempty :: OrgPriority) (parser :: OrgParser OrgPriority)
    title' <- parser :: OrgParser OrgTitle
    -- schedule' <- optional $ try (string "SCHEDULED:" *> space *> (parser :: OrgParser OrgTimestamp))
    -- deadline' <- optional $ try (string "DEADLINE:" *> space *> (parser :: OrgParser OrgTimestamp))
    properties' <- option (mempty :: OrgPropertyBlock) (try parser :: OrgParser OrgPropertyBlock)

    return OrgHeadline { indent = indent'
                       , todo = todo'
                       , priority = priority'
                       , title = title'
                       , schedule = Nothing -- schedule'
                       , deadline = Nothing -- deadline'
                       , properties = properties'
                       }
