module TestDefaults (initialState, defaultHeadline, withCategory, withTodo) where

import Data.Set qualified as Set

import Data.Text (Text)
import Data.Org qualified as Org

defaultHeadline :: Org.Headline
defaultHeadline = mempty

initialState :: Org.Context
initialState = mempty

withCategory :: Org.Context -> Text -> Org.Context
withCategory ctx category = Org.setCategory category ctx

withTodo :: Org.Context -> ([Text], [Text]) -> Org.Context
withTodo ctx (active, inactive) = Org.setTodo (Set.fromList active) (Set.fromList inactive) ctx
