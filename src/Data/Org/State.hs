{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Org.State (St (..), Mut (..)) where

import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable
import Data.Text (Text)
import Data.Set (Set)

class Mut s where
  categoryUpdate :: Text -> s -> s
  todoUpdate :: Set Text -> Set Text -> s -> s
  todoAll :: s -> Set Text
  todoElem :: Text -> s -> Bool

data St where
  St :: (Eq s, Show s, Mut s, Typeable s) => s -> St

instance Mut St where
  categoryUpdate category (St s) = St (categoryUpdate category s)
  todoUpdate active inactive (St s) = St (todoUpdate active inactive s)
  todoAll (St s) = todoAll s
  todoElem todo (St s) = todo `elem` todoAll s

instance Show St where
  show (St s) = show s

instance Eq St where
    (St a) == (St b) = case Typeable.cast b of
        Just b' -> a == b'
        Nothing -> False
