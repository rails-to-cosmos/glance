{-# LANGUAGE OverloadedStrings #-}

module Data.Org.Context (OrgContext (..), OrgStack(..), registeredTodoStates) where

import Data.Text (Text)
import Data.Time (UTCTime)

newtype HeadlineId = HeadlineId Int
  deriving (Show, Eq)

data Role = Parent | Child | Custom Text
  deriving (Show, Eq)

data Relation = Relation Role HeadlineId
  deriving (Show, Eq)

data OrgStack = OrgDrawer [Text] | OrgBabel [Text] | EmptyStack
  deriving (Show, Eq)

data OrgContext = OrgContext
  { metaTodo :: ([Text], [Text])
  , metaCategory :: Text
  , metaTime :: [UTCTime]
  , metaRelations :: [Relation]
  , metaStack :: OrgStack
  } deriving (Show, Eq)

instance Semigroup OrgContext where
  (<>) lhs rhs = OrgContext
    { metaTodo =
        ( fst (metaTodo lhs) <> fst (metaTodo rhs)
        , snd (metaTodo lhs) <> snd (metaTodo rhs)
        )
    , metaCategory = metaCategory lhs <> metaCategory rhs
    , metaTime = metaTime lhs <> metaTime rhs
    , metaRelations = metaRelations lhs <> metaRelations rhs
    }

instance Monoid OrgContext where
  mempty = OrgContext
    { metaTodo = (["TODO"], ["DONE"])
    , metaCategory = mempty :: Text
    , metaTime = mempty :: [UTCTime]
    , metaRelations = mempty :: [Relation]
    , metaStack = EmptyStack
    }

registeredTodoStates :: OrgContext -> [Text]
registeredTodoStates context = active ++ inactive
  where active = fst $ metaTodo context
        inactive = snd $ metaTodo context
