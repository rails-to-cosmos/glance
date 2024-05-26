{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Org.State (Ctx (..), Mut (..)) where

import Data.Typeable (Typeable)
import Data.Text (Text)
import Data.Set (Set)

class Mut s where
  categoryUpdate :: Text -> s -> s
  todoUpdate :: Set Text -> Set Text -> s -> s
  todoAll :: s -> Set Text
  todoElem :: Text -> s -> Bool

data Ctx where
  Ctx :: (Eq s, Show s, Mut s, Typeable s) => s -> Ctx

instance Mut Ctx where
  categoryUpdate category (Ctx s) = Ctx (categoryUpdate category s)
  todoUpdate active inactive (Ctx s) = Ctx (todoUpdate active inactive s)
  todoAll (Ctx s) = todoAll s
  todoElem todo (Ctx s) = todo `elem` todoAll s

instance Show Ctx where
  show (Ctx s) = show s

instance Eq Ctx where
    (Ctx a) == (Ctx b) = case cast b of
        Just b' -> a == b'
        Nothing -> False
