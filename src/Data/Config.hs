module Data.Config (Config (..)) where

import Data.Text qualified as Text
import System.Console.Haskeline qualified as Haskeline

data Config = Config { haskelineSettings :: Haskeline.Settings IO
                     , dbConnectionString :: Text.Text
                     , dbPoolSize :: Int }
