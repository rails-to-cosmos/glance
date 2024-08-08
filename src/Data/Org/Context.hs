module Data.Org.Context (Context (..),
                         setCategory,
                         inTodo, getTodo, setTodo) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Typeable

data Context = Context { metaTodoActive :: !(Set Text)
                       , metaTodoInactive :: !(Set Text)
                       , metaCategory :: !Text
                       -- , metaTime :: [UTCTime]
                       } deriving (Show, Eq, Typeable)

instance Semigroup Context where
  (<>) a b = Context { metaTodoActive = metaTodoActive a <> metaTodoActive b
                     , metaTodoInactive = metaTodoInactive a <> metaTodoInactive b
                     , metaCategory = metaCategory a <> metaCategory b
                     -- , metaTime = metaTime a <> metaTime b
                     }

instance Monoid Context where
  mempty = Context { metaTodoActive = Set.fromList ["TODO"]
                   , metaTodoInactive = Set.fromList ["DONE"]
                   , metaCategory = mempty
                   -- , metaTime = mempty :: [UTCTime]
                   -- , metaStack = EmptyStack
                   }

setCategory :: Text -> Context -> Context
setCategory category ctx = ctx { metaCategory = category }

inTodo :: Text -> Context -> Bool
inTodo todo ctx = todo `elem` getTodo ctx

getTodo :: Context -> Set Text
getTodo ctx = metaTodoActive ctx <> metaTodoInactive ctx

setTodo :: Set Text -> Set Text -> Context -> Context
setTodo active inactive Context{..} = Context{..} { metaTodoActive = metaTodoActive <> active
                                                  , metaTodoInactive = metaTodoInactive <> inactive }
