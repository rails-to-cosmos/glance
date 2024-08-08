module Data.Org.Parser ( StatelessParser
                       , StatefulParser
                       , Parse(..) ) where

import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec qualified as MP

import Control.Monad.State (StateT)

import Data.Org.Context

type StatelessParser = MP.Parsec Void Text
type StatefulParser a = StateT Context StatelessParser a

class Parse a where
  parse :: StatefulParser a
