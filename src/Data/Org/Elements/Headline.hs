module Data.Org.Elements.Headline (Headline (..)) where

import Data.Org.Parse
import Data.Org.Elements.Indent
import Data.Org.Elements.Priority
import Data.Org.Elements.Properties
import Data.Org.Elements.Title
import Data.Org.Elements.Todo
import Data.Org.Elements.Timestamp
import Data.Org.Elements.Separator

import Data.Graph.Inductive qualified as Graph

import Text.Megaparsec
import Text.Megaparsec.Char

import TextShow (TextShow)
import TextShow qualified as TS

import Control.Monad.State qualified as State

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

    -- ctx <- State.get
    -- State.modify $ addNode

    let headline = Headline { indent = indent'
                            , todo = todo'
                            , priority = priority'
                            , title = title'
                            , schedule = Nothing -- schedule'
                            , deadline = Nothing -- deadline'
                            , properties = properties' }

    return headline
