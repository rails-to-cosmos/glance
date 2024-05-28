module Data.Org.Parse ( StatelessParser,
                        StatefulParser,
                        Parse(..) ) where

import Data.Org.State (St)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec qualified as MP
import Control.Monad.State (StateT)

type StatelessParser = MP.Parsec Void Text
type StatefulParserBase s a = StateT s StatelessParser a
type StatefulParser a = StatefulParserBase St a

class Parse a where
  parse :: StatefulParser a
