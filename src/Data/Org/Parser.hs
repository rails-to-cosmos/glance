module Data.Org.Parser ( StatelessParser,
                        StatefulParser,
                        Parseable(..) ) where

import Data.Org.State (St)
import Data.Typeable (Typeable)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import TextShow (TextShow)
import Control.Monad.State (StateT)

type StatelessParser = Parsec Void Text
type StatefulParserBase s a = StateT s StatelessParser a
type StatefulParser a = StatefulParserBase St a

class (Show a, TextShow a, Typeable a, Eq a) => Parseable a where
  parser :: StatefulParser a
