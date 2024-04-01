module Data.Org.Element (Parser, OrgParser, OrgElement(..)) where

import Data.Text (Text)
import Data.Void (Void)
import Data.Org.Context (OrgContext)
import Text.Megaparsec (Parsec)
import qualified Control.Monad.State as State

type Parser = Parsec Void Text
type StatefulParser s a = State.StateT s Parser a
type OrgParser a = StatefulParser OrgContext a

class OrgElement a where
  parser :: OrgParser a
