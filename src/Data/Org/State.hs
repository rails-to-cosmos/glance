{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Org.State (St (..), Mut (..)) where

import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable

class Mut s where
  setCategory :: Text -> s -> s
  setTodo :: Set Text -> Set Text -> s -> s
  getTodo :: s -> Set Text
  inTodo :: Text -> s -> Bool

data St where
  St :: (Eq s, Show s, Mut s, Typeable s) => s -> St

instance Mut St where
  setCategory category (St s) = St (setCategory category s)
  setTodo active inactive (St s) = St (setTodo active inactive s)
  getTodo (St s) = getTodo s
  inTodo todo (St s) = todo `elem` getTodo s

instance Show St where
  show (St s) = show s

instance Eq St where
  (St a) == (St b) = case Typeable.cast b of
    Just b' -> a == b'
    Nothing -> False
