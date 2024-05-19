module Data.Org.Element (Parser, OrgParser, Org(..)) where

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

class (Show a, TextShow.TextShow a, Typeable a, Eq a) => Org a where
  parser :: OrgParser a
