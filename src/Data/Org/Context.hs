module Data.Org.Context (Context (..),
                         setCategory,
                         inTodo, getTodo, setTodo,
                         addNode) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Typeable

import Data.Graph.Inductive qualified as G

newtype OrgGraph = OrgGraph (G.Gr () ())
  deriving (Show, Eq)

instance Monoid OrgGraph where
  mempty = OrgGraph G.empty

instance Semigroup OrgGraph where
  (<>) a _b = a  -- TODO implement semigroup

data Context = Context { metaTodoActive :: !(Set Text)
                       , metaTodoInactive :: !(Set Text)
                       , metaCategory :: !Text
                       , metaGraph :: OrgGraph
                       -- , metaTime :: [UTCTime]
                       } deriving (Show, Eq, Typeable)

instance Semigroup Context where
  (<>) a b = Context { metaTodoActive = metaTodoActive a <> metaTodoActive b
                     , metaTodoInactive = metaTodoInactive a <> metaTodoInactive b
                     , metaCategory = metaCategory a <> metaCategory b
                     , metaGraph = metaGraph a <> metaGraph b
                     -- , metaTime = metaTime a <> metaTime b
                     }

instance Monoid Context where
  mempty = Context { metaTodoActive = Set.fromList ["TODO"]
                   , metaTodoInactive = Set.fromList ["DONE"]
                   , metaCategory = mempty
                   , metaGraph = mempty
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

addNode :: Int -> Context -> Context
addNode node context@Context{ metaGraph = OrgGraph gr  } = context { metaGraph = OrgGraph (G.insNode (node, ()) gr) }
