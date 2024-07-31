module Data.Org.Context (Context (..)) where

import Data.Graph.Inductive qualified as G
import Data.Org.Elements.Base qualified as Org
import Data.Org.State
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Typeable

-- type Graph = G.Gr
-- type Node = Org.Element
-- type Edge = Text

-- newtype OrgGraph = OrgGraph (Graph Node Edge)
--   deriving (Show, Eq)

-- instance Monoid OrgGraph where
--   mempty = OrgGraph G.empty

-- instance Semigroup OrgGraph where
--   (<>) lhs _rhs = lhs  -- TODO implement semigroup

data Context = Context { metaTodoActive :: !(Set Text)
                       , metaTodoInactive :: !(Set Text)
                       , metaCategory :: !Text
                       -- , metaTime :: [UTCTime]
                       -- , metaStack :: OrgStack
                       } deriving (Show, Eq, Typeable)

instance Mut Context where
  setCategory category ctx = ctx { metaCategory = category }

  getTodo ctx = metaTodoActive ctx <> metaTodoInactive ctx
  inTodo todo ctx = todo `elem` getTodo ctx
  setTodo active inactive ctx = ctx { metaTodoActive = metaTodoActive ctx <> active
                                    , metaTodoInactive = metaTodoInactive ctx <> inactive }

instance Semigroup Context where
  (<>) lhs rhs = Context { metaTodoActive = metaTodoActive lhs <> metaTodoActive rhs
                         , metaTodoInactive = metaTodoInactive lhs <> metaTodoInactive rhs
                         , metaCategory = metaCategory lhs <> metaCategory rhs
                         -- , metaTime = metaTime lhs <> metaTime rhs
                         }

instance Monoid Context where
  mempty = Context { metaTodoActive = Set.fromList ["TODO"]
                   , metaTodoInactive = Set.fromList ["DONE"]
                   , metaCategory = mempty
                   -- , metaTime = mempty :: [UTCTime]
                   -- , metaStack = EmptyStack
                   }
