module Data.Config (Config (..)) where

import qualified System.Console.Haskeline as Haskeline

newtype Config where
  Config :: {haskelineSettings :: Haskeline.Settings IO} -> Config
