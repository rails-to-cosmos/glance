{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Context (OrgContext (..), allTodoStates) where

import Data.Text (Text)

-- newtype HeadlineId = HeadlineId Int
--   deriving (Show, Eq)

-- data Role = Parent | Child | Custom Text
--   deriving (Show, Eq)

-- data Relation = Relation Role HeadlineId
--   deriving (Show, Eq)

-- data OrgStack = OrgDrawer [Text] | OrgBabel [Text] | EmptyStack
--   deriving (Show, Eq)

data OrgContext = OrgContext { metaTodo :: !([Text], [Text])
                             , metaCategory :: !Text
                             -- , metaTime :: [UTCTime]
                             -- , metaStack :: OrgStack
                             } deriving (Show, Eq)

instance Semigroup OrgContext where
  (<>) lhs rhs = OrgContext
    { metaTodo = ( active lhs <> active rhs
                 , inactive lhs <> inactive rhs
                 )
    , metaCategory = metaCategory lhs <> metaCategory rhs
    -- , metaTime = metaTime lhs <> metaTime rhs
    }

instance Monoid OrgContext where
  mempty = OrgContext { metaTodo = (["TODO"], ["DONE"])
                      , metaCategory = mempty :: Text
                      -- , metaTime = mempty :: [UTCTime]
                      -- , metaStack = EmptyStack
                      }

allTodoStates :: OrgContext -> [Text]
allTodoStates ctx = active ctx ++ inactive ctx

active :: OrgContext -> [Text]
active ctx = fst (metaTodo ctx)

inactive :: OrgContext -> [Text]
inactive ctx = snd (metaTodo ctx)
