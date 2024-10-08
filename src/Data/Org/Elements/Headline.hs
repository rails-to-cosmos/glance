module Data.Org.Elements.Headline (Headline (..)) where

import Data.Org.Elements.Indent
import Data.Org.Elements.Priority
import Data.Org.Elements.Properties (Property(..), Properties)
import Data.Org.Elements.Properties qualified as Properties
import Data.Org.Elements.Separator
import Data.Org.Elements.Timestamp
import Data.Org.Elements.Title
import Data.Org.Elements.Todo
import Data.Org.Identity
import Data.Org.Parser

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import TextShow (TextShow)
import TextShow qualified

data Headline = Headline { indent :: !Indent
                         , todo :: !Todo
                         , priority :: !Priority
                         , title :: !Title
                         , schedule :: !(Maybe Timestamp)
                         , deadline :: !(Maybe Timestamp)
                         , properties :: !Properties
                         } deriving (Show, Eq)

instance Identity Headline where
  identity Headline {..} = case Properties.find "GLANCE_ID" properties of
    Nothing -> TextShow.showt title
    Just (Property _k v) -> TextShow.showt v

instance Semigroup Headline where
  (<>) a b = Headline { indent = indent a <> indent b
                      , todo = todo a <> todo b
                      , priority = priority a <> priority b
                      , title = title a <> title b
                      , schedule = Nothing
                      , deadline = Nothing
                      , properties = properties a <> properties b }

instance Monoid Headline where
  mempty = Headline { indent = mempty
                    , todo = mempty
                    , priority = mempty
                    , title = mempty
                    , schedule = Nothing
                    , deadline = Nothing
                    , properties = mempty }

instance TextShow Headline where
  showb a = TextShow.showb (indent a)
    <> TextShow.showb (todo a)
    <> TextShow.showb (priority a)
    <> TextShow.showb (title a)

instance Parse Headline where
  parse = do
    indent' <- parse :: StatefulParser Indent
    todo' <- MP.option (mempty :: Todo) (parse :: StatefulParser Todo)
    priority' <- MP.option (mempty :: Priority) (parse :: StatefulParser Priority)
    title' <- parse :: StatefulParser Title
    -- schedule' <- optional $ try (string "SCHEDULED:" *> space *> (parse :: StatefulParser Timestamp))
    -- deadline' <- optional $ try (string "DEADLINE:" *> space *> (parse :: StatefulParser Timestamp))
    properties' <- MP.option (mempty :: Properties) (MP.try (MPC.eol *> parse :: StatefulParser Properties))

    _newline <- parse :: StatefulParser Separator

    -- _id <- State.lift (liftIO (randomIO :: IO UUID))

    let headline = Headline { indent = indent'
                            , todo = todo'
                            , priority = priority'
                            , title = title'
                            , schedule = Nothing -- schedule'
                            , deadline = Nothing -- deadline'
                            , properties = properties' }

    return headline
