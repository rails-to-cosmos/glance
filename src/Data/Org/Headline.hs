module Data.Org.Headline (Headline (..)) where

import Data.Org.Base qualified as Org
import Data.Org.Indent
import Data.Org.Priority
import Data.Org.Properties
import Data.Org.Title
import Data.Org.Todo
import Data.Org.Timestamp
import Data.Org.Separator
import Data.Text qualified as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import TextShow qualified

data Headline = Headline { indent :: !Indent
                         , todo :: !Todo
                         , priority :: !Priority
                         , title :: !Title
                         , schedule :: !(Maybe Ts)
                         , deadline :: !(Maybe Ts)
                         , properties :: !Properties
                         } deriving (Show, Eq)

instance Semigroup Headline where
  (<>) a b = Headline { indent = indent a <> indent b
                      , todo = todo a <> todo b
                      , priority = priority a <> priority b
                      , title = title a <> title b
                      , schedule = Nothing
                      , deadline = Nothing
                      , properties = properties a <> properties b }

instance Monoid Headline where
  mempty = Headline { indent = mempty :: Indent
                    , todo = mempty :: Todo
                    , priority = mempty :: Priority
                    , title = mempty :: Title
                    , schedule = Nothing
                    , deadline = Nothing
                    , properties = mempty :: Properties }

instance TextShow.TextShow Headline where
  showb headline = TextShow.showb (indent headline)
    <> TextShow.showb (todo headline)
    <> TextShow.showb (priority headline)
    <> TextShow.showb (title headline)

instance Org.Base Headline where
  parser = do
    indent' <- Org.parser :: Org.OrgParser Indent
    todo' <- option (mempty :: Todo) (Org.parser :: Org.OrgParser Todo)
    priority' <- option (mempty :: Priority) (Org.parser :: Org.OrgParser Priority)
    title' <- Org.parser :: Org.OrgParser Title
    -- schedule' <- optional $ try (string "SCHEDULED:" *> space *> (Org.parser :: Org.OrgParser Ts))
    -- deadline' <- optional $ try (string "DEADLINE:" *> space *> (Org.parser :: Org.OrgParser Ts))
    properties' <- option (mempty :: Properties) (try (eol *> Org.parser :: Org.OrgParser Properties))

    _ <- Org.parser :: Org.OrgParser Sep

    return Headline { indent = indent'
                    , todo = todo'
                    , priority = priority'
                    , title = title'
                    , schedule = Nothing -- schedule'
                    , deadline = Nothing -- deadline'
                    , properties = properties' }
