module Data.Org.Headline (Headline (..)) where

import Control.Monad
import Data.Org.Element
import Data.Org.Indent
import Data.Org.Priority
import Data.Org.Properties
import Data.Org.Title
import Data.Org.Todo
import Data.Org.Timestamp
import Data.Text (replicate)
import Text.Megaparsec
import Text.Megaparsec.Char
import TextShow
import Prelude hiding (replicate)

data Headline = Headline { indent :: !Indent
                         , todo :: !Todo
                         , priority :: !Priority
                         , title :: !Title
                         , schedule :: !(Maybe Timestamp)
                         , deadline :: !(Maybe Timestamp)
                         , properties :: !Properties
                         } deriving (Show, Eq)

instance Semigroup Headline where
  (<>) lhs rhs = Headline { indent = indent lhs <> indent rhs
                          , todo = todo lhs <> todo rhs
                          , priority = priority lhs <> priority rhs
                          , title = title lhs <> title rhs
                          , schedule = Nothing
                          , deadline = Nothing
                          , properties = properties lhs <> properties rhs }

instance Monoid Headline where
  mempty = Headline { indent = mempty :: Indent
                    , todo = mempty :: Todo
                    , priority = mempty :: Priority
                    , title = mempty :: Title
                    , schedule = Nothing
                    , deadline = Nothing
                    , properties = mempty :: Properties }

instance TextShow Headline where
  showb headline = fromText (replicate i "*") <> showbSpace <> showb (title headline)
    where Indent i = indent headline

instance OrgElement Headline where
  parser = do
    indent' <- parser :: OrgParser Indent
    todo' <- option (mempty :: Todo) (parser :: OrgParser Todo)
    priority' <- option (mempty :: Priority) (parser :: OrgParser Priority)
    title' <- parser :: OrgParser Title
    -- schedule' <- optional $ try (string "SCHEDULED:" *> space *> (parser :: OrgParser Timestamp))
    -- deadline' <- optional $ try (string "DEADLINE:" *> space *> (parser :: OrgParser Timestamp))
    properties' <- option (mempty :: Properties) (try (eol *> parser :: OrgParser Properties))

    return Headline { indent = indent'
                    , todo = todo'
                    , priority = priority'
                    , title = title'
                    , schedule = Nothing -- schedule'
                    , deadline = Nothing -- deadline'
                    , properties = properties' }
