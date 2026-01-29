module Data.Org.Context (Context (..),
                         setCategory,
                         inTodo, getTodo, setTodo) where

import Data.Org.Types
import Data.Set (Set)
import Data.Text (Text)

setCategory :: Text -> Context -> Context
setCategory category ctx = ctx { metaCategory = category }

inTodo :: Text -> Context -> Bool
inTodo todo ctx = todo `elem` getTodo ctx

getTodo :: Context -> Set Text
getTodo ctx = todoActive ctx <> todoInactive ctx

setTodo :: Set Text -> Set Text -> Context -> Context
setTodo active inactive Context{..} =
  Context{..} { todoActive = todoActive <> active
              , todoInactive = todoInactive <> inactive }
