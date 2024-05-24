module Data.Org.Context (Context (..)) where

import Data.Set qualified as Set
import Data.Text (Text)

-- newtype HeadlineId = HeadlineId Int
--   deriving (Show, Eq)

-- data Role = Parent | Child | Custom Text
--   deriving (Show, Eq)

-- data Relation = Relation Role HeadlineId
--   deriving (Show, Eq)

-- data OrgStack = OrgDrawer [Text] | OrgBabel [Text] | EmptyStack
--   deriving (Show, Eq)

data Context = Context { metaTodoActive :: !(Set.Set Text)
                             , metaTodoInactive :: !(Set.Set Text)
                             , metaCategory :: !Text
                             -- , metaTime :: [UTCTime]
                             -- , metaStack :: OrgStack
                             } deriving (Show, Eq)

instance Semigroup Context where
  (<>) lhs rhs = Context
    { metaTodoActive = metaTodoActive lhs <> metaTodoActive rhs
    , metaTodoInactive = metaTodoInactive lhs <> metaTodoInactive rhs
    , metaCategory = metaCategory lhs <> metaCategory rhs
    -- , metaTime = metaTime lhs <> metaTime rhs
    }

instance Monoid Context where
  mempty = Context { metaTodoActive = Set.fromList ["TODO"]
                      , metaTodoInactive = Set.fromList ["DONE"]
                      , metaCategory = mempty :: Text
                      -- , metaTime = mempty :: [UTCTime]
                      -- , metaStack = EmptyStack
                      }
