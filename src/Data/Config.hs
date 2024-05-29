module Data.Config (Config (..)) where

import Data.Text (Text)
import System.Console.Haskeline qualified as Haskeline

data Config = Config { haskelineSettings :: Haskeline.Settings IO
                     , dbConnectionString :: Text
                     , dbPoolSize :: Int }
