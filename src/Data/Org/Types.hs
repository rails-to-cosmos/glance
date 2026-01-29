module Data.Org.Types where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Typeable
import Data.Void (Void)

import Text.Megaparsec qualified as MP
import Control.Monad.State (StateT)

data Context = Context { todoActive :: !(Set Text)
                       , todoInactive :: !(Set Text)
                       , metaCategory :: !Text
                       -- , metaTime :: [UTCTime]
                       } deriving (Show, Eq, Typeable)

instance Semigroup Context where
  (<>) a b = Context { todoActive = todoActive a <> todoActive b
                     , todoInactive = todoInactive a <> todoInactive b
                     , metaCategory = metaCategory a <> metaCategory b
                     -- , metaTime = metaTime a <> metaTime b
                     }

instance Monoid Context where
  mempty = Context { todoActive = Set.fromList ["TODO"]
                   , todoInactive = Set.fromList ["DONE"]
                   , metaCategory = mempty
                   -- , metaTime = mempty :: [UTCTime]
                   -- , metaStack = EmptyStack
                   }

class Identity a where
  identity :: a -> Text

type StatelessParser = MP.Parsec Void Text
type StatefulParser a = StateT Context StatelessParser a

class Parse a where
  parse :: StatefulParser a
