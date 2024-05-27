module Data.Org.Parse ( StatelessParser,
                        StatefulParser,
                        Parse(..) ) where

import Data.Org.State (St)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Control.Monad.State (StateT)

type StatelessParser = Parsec Void Text
type StatefulParserBase s a = StateT s StatelessParser a
type StatefulParser a = StatefulParserBase St a

class Parse a where
  parser :: StatefulParser a
