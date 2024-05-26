module TestDefaults (defaultContext, defaultHeadline, withCategory, withTodo) where

import Data.Set qualified as Set

import Data.Text (Text)
import Data.Org qualified as Org

defaultHeadline :: Org.Headline
defaultHeadline = mempty :: Org.Headline

defaultContext :: Org.Ctx
defaultContext = Org.Ctx (mempty :: Org.Context)

withCategory :: Org.Ctx -> Text -> Org.Ctx
withCategory ctx category = Org.categoryUpdate category ctx

withTodo :: Org.Ctx -> ([Text], [Text]) -> Org.Ctx
withTodo ctx (active, inactive) = Org.todoUpdate (Set.fromList active) (Set.fromList inactive) ctx
