module Data.Config (Config (..)) where

import System.Console.Haskeline qualified as Haskeline

newtype Config where
  Config :: {haskelineSettings :: Haskeline.Settings IO} -> Config
