module Data.Org.Identity (Identity (..)) where

import Data.Text (Text)

class Identity a where
  id :: a -> Text
