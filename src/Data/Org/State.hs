module Data.Org.State (State (..), Mutable (..)) where

import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable

class Mutable s where  -- TODO lens
  setCategory :: Text -> s -> s
  setTodo :: Set Text -> Set Text -> s -> s
  getTodo :: s -> Set Text
  inTodo :: Text -> s -> Bool
  addNode :: Text -> Text -> s -> s

data State where
  State :: (Eq s, Show s, Mutable s, Typeable s) => s -> State

instance Mutable State where
  setCategory category (State s) = State (setCategory category s)
  setTodo active inactive (State s) = State (setTodo active inactive s)
  getTodo (State s) = getTodo s
  inTodo todo (State s) = todo `elem` getTodo s
  addNode node edge (State s) = State (addNode node edge s)

instance Show State where
  show (State s) = show s

instance Eq State where
  (State a) == (State b) = case Typeable.cast b of
    Just b' -> a == b'
    Nothing -> False
