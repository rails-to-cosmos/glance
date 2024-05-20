module Data.Org.Headline (Headline (..)) where

import Data.Org.Parse
import Data.Org.Indent
import Data.Org.Priority
import Data.Org.Properties
import Data.Org.Title
import Data.Org.Todo
import Data.Org.Timestamp
import Data.Org.Separator
import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow (TextShow)
import TextShow qualified as TS

data Headline = Headline { indent :: !Indent
                         , todo :: !Todo
                         , priority :: !Priority
                         , title :: !Title
                         , schedule :: !(Maybe Timestamp)
                         , deadline :: !(Maybe Timestamp)
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

instance TextShow Headline where
  showb headline = TS.showb (indent headline)
    <> TS.showb (todo headline)
    <> TS.showb (priority headline)
    <> TS.showb (title headline)

instance Parse Headline where
  parser = do
    indent' <- parser :: StatefulParser Indent
    todo' <- option (mempty :: Todo) (parser :: StatefulParser Todo)
    priority' <- option (mempty :: Priority) (parser :: StatefulParser Priority)
    title' <- parser :: StatefulParser Title
    -- schedule' <- optional $ try (string "SCHEDULED:" *> space *> (parser :: StatefulParser Timestamp))
    -- deadline' <- optional $ try (string "DEADLINE:" *> space *> (parser :: StatefulParser Timestamp))
    properties' <- option (mempty :: Properties) (try (eol *> parser :: StatefulParser Properties))

    _ <- parser :: StatefulParser Separator

    return Headline { indent = indent'
                    , todo = todo'
                    , priority = priority'
                    , title = title'
                    , schedule = Nothing -- schedule'
                    , deadline = Nothing -- deadline'
                    , properties = properties' }
