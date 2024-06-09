module Data.Org.Parse ( StatelessParser,
                        StatefulParser,
                        Parse(..) ) where

import Data.Org.State qualified as Org.State
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec qualified as MP
import Control.Monad.State (StateT)

type StatelessParser = MP.Parsec Void Text
type StatefulParser a = StateT Org.State.State StatelessParser a

class Parse a where
  parse :: StatefulParser a
