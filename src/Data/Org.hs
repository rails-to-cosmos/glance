module Data.Org
  ( OrgElement (..)
  , OrgContext (..)
  , OrgGenericElement (..)
  , OrgHeadline (..)
  , OrgIndent (..)
  , OrgKeyword (..)
  , PlainText (..)
  , OrgPriority (..)
  , OrgProperty (..)
  , OrgPropertyBlock (..)
  , OrgTags (..)
  , OrgTimestamp (..)
  , OrgTimestampStatus (..)
  , OrgTitle (..)
  , OrgTitleElement (..)
  , OrgTodo (..)
  , OrgPragma (..)
  , OrgSeparator (..)
  , parse
  ) where

import Data.Org.Element
import Data.Org.Context
import Data.Org.Generic
import Data.Org.Headline
import Data.Org.Indent
import Data.Org.Keyword
import Data.Org.PlainText
import Data.Org.Pragma
import Data.Org.Priority
import Data.Org.Property
import Data.Org.PropertyBlock
import Data.Org.Tags
import Data.Org.Timestamp
import Data.Org.Title
import Data.Org.Todo
import Data.Org.Separator

import Control.Monad.State (runStateT)
import Data.Org.Element qualified as OrgElement
import Data.Text (Text, pack)
import Data.Text.Lazy.Builder ()
import Text.Megaparsec qualified as PS
import UnliftIO ()

parse :: OrgContext -> Text -> ([OrgGenericElement], OrgContext)
parse ctx cmd = case PS.parse (runStateT (PS.manyTill OrgElement.parser PS.eof) ctx) "" cmd of
  Right val -> val
  Left err  -> ([OrgGenericText (PlainText (pack (PS.errorBundlePretty err)))], ctx)
