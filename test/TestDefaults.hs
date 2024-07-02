module TestDefaults (initialState, defaultHeadline, withCategory, withTodo) where

import Data.Set qualified as Set

import Data.Text (Text)
import Data.Org qualified as Org

defaultHeadline :: Org.Headline
defaultHeadline = mempty :: Org.Headline

initialState :: Org.State
initialState = Org.State (mempty :: Org.Context)

withCategory :: Org.State -> Text -> Org.State
withCategory ctx category = Org.setCategory category ctx

withTodo :: Org.State -> ([Text], [Text]) -> Org.State
withTodo ctx (active, inactive) = Org.setTodo (Set.fromList active) (Set.fromList inactive) ctx
