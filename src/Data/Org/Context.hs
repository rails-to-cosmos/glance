module Data.Org.Context (Context (..)) where

import Data.Org.Elements.Base qualified as Org
import Data.Org.State
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Typeable

-- import Data.Graph.Inductive qualified as G

-- type Graph = G.Gr
-- type Node = Org.Element
-- type Edge = Text

-- newtype OrgGraph = OrgGraph (Graph Node Edge)
--   deriving (Show, Eq)

-- instance Monoid OrgGraph where
--   mempty = OrgGraph G.empty

-- instance Semigroup OrgGraph where
--   (<>) a _b = a  -- TODO implement semigroup

data Context = Context { metaTodoActive :: !(Set Text)
                       , metaTodoInactive :: !(Set Text)
                       , metaCategory :: !Text
                       -- , metaGraph :: OrgGraph
                       -- , metaTime :: [UTCTime]
                       -- , metaStack :: OrgStack
                       } deriving (Show, Eq, Typeable)

instance Mutable Context where
  setCategory category ctx = ctx { metaCategory = category }

  getTodo ctx = metaTodoActive ctx <> metaTodoInactive ctx
  inTodo todo ctx = todo `elem` getTodo ctx
  setTodo active inactive ctx = ctx { metaTodoActive = metaTodoActive ctx <> active
                                       , metaTodoInactive = metaTodoInactive ctx <> inactive }

instance Semigroup Context where
  (<>) a b = Context { metaTodoActive = metaTodoActive a <> metaTodoActive b
                         , metaTodoInactive = metaTodoInactive a <> metaTodoInactive b
                         , metaCategory = metaCategory a <> metaCategory b
                         -- , metaGraph = metaGraph a <> metaGraph b
                         -- , metaTime = metaTime a <> metaTime b
                         }

instance Monoid Context where
  mempty = Context { metaTodoActive = Set.fromList ["TODO"]
                   , metaTodoInactive = Set.fromList ["DONE"]
                   , metaCategory = mempty
                   -- , metaGraph = mempty
                   -- , metaTime = mempty :: [UTCTime]
                   -- , metaStack = EmptyStack
                   }
