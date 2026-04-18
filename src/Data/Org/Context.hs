module Data.Org.Context where

import Data.Set (Set)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Context = Context { todoActive :: !(Set Text)
                       , todoInactive :: !(Set Text)
                       , metaCategory :: !Text
                       -- , identityAddressableStorage :: IdentityAddressableStorage
                       } deriving (Show, Eq)
