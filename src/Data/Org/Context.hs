module Data.Org.Context (OrgContext (..)) where

import qualified Data.Set as Set
import Data.Text (Text)

-- newtype HeadlineId = HeadlineId Int
--   deriving (Show, Eq)

-- data Role = Parent | Child | Custom Text
--   deriving (Show, Eq)

-- data Relation = Relation Role HeadlineId
--   deriving (Show, Eq)

-- data OrgStack = OrgDrawer [Text] | OrgBabel [Text] | EmptyStack
--   deriving (Show, Eq)

data OrgContext = OrgContext { metaTodoActive :: !(Set.Set Text)
                             , metaTodoInactive :: !(Set.Set Text)
                             , metaCategory :: !Text
                             -- , metaTime :: [UTCTime]
                             -- , metaStack :: OrgStack
                             } deriving (Show, Eq)

instance Semigroup OrgContext where
  (<>) lhs rhs = OrgContext
    { metaTodoActive = metaTodoActive lhs <> metaTodoActive rhs
    , metaTodoInactive = metaTodoInactive lhs <> metaTodoInactive rhs
    , metaCategory = metaCategory lhs <> metaCategory rhs
    -- , metaTime = metaTime lhs <> metaTime rhs
    }

instance Monoid OrgContext where
  mempty = OrgContext { metaTodoActive = Set.fromList ["TODO"]
                      , metaTodoInactive = Set.fromList ["DONE"]
                      , metaCategory = mempty :: Text
                      -- , metaTime = mempty :: [UTCTime]
                      -- , metaStack = EmptyStack
                      }
