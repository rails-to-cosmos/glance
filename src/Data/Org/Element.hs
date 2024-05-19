module Data.Org.Element (Parser, OrgParser, OrgElement(..)) where

import Data.Text (Text)
import Data.Void (Void)
import Data.Org.Context (OrgContext)
import Text.Megaparsec (Parsec)
import TextShow qualified
import Control.Monad.State qualified as State

type Parser = Parsec Void Text
type StatefulParser s a = State.StateT s Parser a
type OrgParser a = StatefulParser OrgContext a

class (Show a, Eq a, TextShow.TextShow a) => OrgElement a where
  parser :: OrgParser a
