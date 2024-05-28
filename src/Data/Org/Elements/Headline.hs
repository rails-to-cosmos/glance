module Data.Org.Elements.Headline (Headline (..)) where

import Data.Org.Identity
import Data.Org.Elements.Indent
import Data.Org.Elements.Priority
import Data.Org.Elements.Properties
import Data.Org.Elements.Separator
import Data.Org.Elements.Timestamp
import Data.Org.Elements.Title
import Data.Org.Elements.Todo
import Data.Org.Parse
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import TextShow (TextShow)
import TextShow qualified

data Headline = Headline { id :: !Text
                         , indent :: !Indent
                         , todo :: !Todo
                         , priority :: !Priority
                         , title :: !Title
                         , schedule :: !(Maybe Timestamp)
                         , deadline :: !(Maybe Timestamp)
                         , properties :: !Properties
                         } deriving (Show, Eq)

instance Identity Headline where
  id = Headline.id

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
  showb headline = TextShow.showb (indent headline)
    <> TextShow.showb (todo headline)
    <> TextShow.showb (priority headline)
    <> TextShow.showb (title headline)

instance Parse Headline where
  parse = do
    indent' <- parse :: StatefulParser Indent
    todo' <- MP.option (mempty :: Todo) (parse :: StatefulParser Todo)
    priority' <- MP.option (mempty :: Priority) (parse :: StatefulParser Priority)
    title' <- parse :: StatefulParser Title
    -- schedule' <- optional $ try (string "SCHEDULED:" *> space *> (parse :: StatefulParser Timestamp))
    -- deadline' <- optional $ try (string "DEADLINE:" *> space *> (parse :: StatefulParser Timestamp))
    properties' <- MP.option (mempty :: Properties) (MP.try (MPC.eol *> parse :: StatefulParser Properties))

    _ <- parse :: StatefulParser Separator

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
