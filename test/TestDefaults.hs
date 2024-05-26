module TestDefaults (defaultContext, defaultHeadline, withCategory, withTodo) where

import Data.Set qualified as Set

import Data.Text (Text)
import Data.Org qualified as Org

defaultHeadline :: Org.Headline
defaultHeadline = mempty :: Org.Headline

defaultContext :: Org.St
defaultContext = Org.St (mempty :: Org.Context)

withCategory :: Org.St -> Text -> Org.St
withCategory ctx category = Org.categoryUpdate category ctx

withTodo :: Org.St -> ([Text], [Text]) -> Org.St
withTodo ctx (active, inactive) = Org.todoUpdate (Set.fromList active) (Set.fromList inactive) ctx
