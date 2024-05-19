module Data.Org.Base (StatelessParser, StatefulParser, Base(..)) where

import Data.Typeable
import Data.Text (Text)
import Data.Void (Void)
import Data.Org.Context (OrgContext)
import Text.Megaparsec (Parsec)
import TextShow (TextShow)
import Control.Monad.State qualified as State

type StatelessParser = Parsec Void Text
type StatefulParserBase s a = State.StateT s StatelessParser a
type StatefulParser a = StatefulParserBase OrgContext a

class (Show a, TextShow a, Typeable a, Eq a) => Base a where
  parser :: StatefulParser a
