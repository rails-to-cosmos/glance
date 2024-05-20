module Data.Config (Config (..)) where

import Data.Text qualified as T
import System.Console.Haskeline qualified as Haskeline

data Config = Config { haskelineSettings :: Haskeline.Settings IO
                     , dbConnectionString :: T.Text
                     , dbPoolSize :: Int }
