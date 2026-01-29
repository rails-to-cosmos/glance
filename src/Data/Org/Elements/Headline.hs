module Data.Org.Elements.Headline (Headline (..)) where

import Data.Org.Elements.Indent
import Data.Org.Elements.Priority
import Data.Org.Elements.Properties (Property(..), Properties)
import Data.Org.Elements.Properties qualified as Properties
import Data.Org.Elements.Separator
import Data.Org.Elements.Timestamp
import Data.Org.Elements.Title
import Data.Org.Elements.Todo
import Data.Org.Types

import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import TextShow (TextShow)
import TextShow qualified

data Headline = Headline { indent :: !Indent
                         , todo :: !(Maybe Todo)
                         , priority :: !(Maybe Priority)
                         , title :: !Title
                         , schedule :: !(Maybe Timestamp)
                         , deadline :: !(Maybe Timestamp)
                         , properties :: !Properties
                         } deriving (Show, Eq)

instance Identity Headline where
  identity Headline {..} = case Properties.find "ID" properties of
    Nothing -> TextShow.showt title
    Just (Property _ v) -> TextShow.showt v

instance TextShow Headline where
  showb Headline {..} =
    showSpaced indent
    <> foldMap showSpaced todo
    <> foldMap showSpaced priority
    <> TextShow.showb title
    where showSpaced :: TextShow a => a -> TextShow.Builder
          showSpaced = (<> TextShow.showbSpace) . TextShow.showb

instance Parse Headline where
  parse = do
    indent' <- parse :: StatefulParser Indent
    todo' <- MP.optional (MP.try parse :: StatefulParser Todo)
    priority' <- MP.optional (MP.try parse :: StatefulParser Priority)
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
