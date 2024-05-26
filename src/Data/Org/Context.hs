module Data.Org.Context (Context (..)) where

import Data.Typeable
import Data.Org.MutableState
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import Data.Graph.Inductive qualified as Graph

type NodeLabel = Text
type EdgeLabel = Text

-- newtype OrgGraph = OrgGraph (Graph.Gr NodeLabel EdgeLabel)
--   deriving (Show, Eq)

-- instance Monoid OrgGraph where
--   mempty = OrgGraph Graph.empty

-- instance Semigroup OrgGraph where
--   (<>) lhs _rhs = lhs  -- TODO implement semigroup

data Context = Context { metaTodoActive :: !(Set Text)
                       , metaTodoInactive :: !(Set Text)
                       , metaCategory :: !Text
                       -- , metaGraph :: OrgGraph
                       -- , metaTime :: [UTCTime]
                       -- , metaStack :: OrgStack
                       } deriving (Show, Eq, Typeable)

instance Mut Context where
  categoryUpdate category ctx = ctx { metaCategory = category }

  todoAll ctx = metaTodoActive ctx <> metaTodoInactive ctx
  todoElem todo ctx = todo `elem` todoAll ctx
  todoUpdate active inactive ctx = ctx { metaTodoActive = metaTodoActive ctx <> active
                                       , metaTodoInactive = metaTodoInactive ctx <> inactive }

instance Semigroup Context where
  (<>) lhs rhs = Context { metaTodoActive = metaTodoActive lhs <> metaTodoActive rhs
                         , metaTodoInactive = metaTodoInactive lhs <> metaTodoInactive rhs
                         , metaCategory = metaCategory lhs <> metaCategory rhs
                         -- , metaGraph = metaGraph lhs <> metaGraph rhs
                         -- , metaTime = metaTime lhs <> metaTime rhs
                         }

instance Monoid Context where
  mempty = Context { metaTodoActive = Set.fromList ["TODO"]
                   , metaTodoInactive = Set.fromList ["DONE"]
                   , metaCategory = mempty :: Text
                   -- , metaGraph = mempty :: OrgGraph
                   -- , metaTime = mempty :: [UTCTime]
                   -- , metaStack = EmptyStack
                   }
