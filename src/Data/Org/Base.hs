module Data.Org.Base (Parser, OrgParser, Base(..)) where

import Data.Typeable
import Data.Text (Text)
import Data.Void (Void)
import Data.Org.Context (OrgContext)
import Text.Megaparsec (Parsec)
import TextShow qualified
import Control.Monad.State qualified as State

type Parser = Parsec Void Text
type StatefulParser s a = State.StateT s Parser a
type OrgParser a = StatefulParser OrgContext a

class (Show a, TextShow.TextShow a, Typeable a, Eq a) => Base a where
  parser :: OrgParser a
