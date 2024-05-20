module Data.Org.Base ( StatelessParser,
                       StatefulParser,
                       Parse(..) ) where

import Data.Typeable (Typeable)
import Data.Text (Text)
import Data.Void (Void)
import Data.Org.Context (OrgContext)
import Text.Megaparsec (Parsec)
import TextShow (TextShow)
import Control.Monad.State (StateT)

type StatelessParser = Parsec Void Text
type StatefulParserBase s a = StateT s StatelessParser a
type StatefulParser a = StatefulParserBase OrgContext a

class (Show a, TextShow a, Typeable a, Eq a) => Parse a where
  parser :: StatefulParser a
