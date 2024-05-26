module Data.Org.Identifiable (Identifiable (..)) where

import Data.Text (Text)

class Identifiable a where
  id :: a -> Text
