module Data.Org.Parse ( StatelessParser,
                        StatefulParser,
                        Parse(parser) ) where

import Data.Typeable (Typeable)
import Data.Text (Text)
import Data.Void (Void)
import Data.Org.Context (Context)
import Text.Megaparsec (Parsec)
import TextShow (TextShow)
import Control.Monad.State (StateT)

type StatelessParser = Parsec Void Text
type StatefulParserBase s a = StateT s StatelessParser a
type StatefulParser a = StatefulParserBase Context a

class (Show a, TextShow a, Typeable a, Eq a) => Parse a where
  parser :: StatefulParser a
