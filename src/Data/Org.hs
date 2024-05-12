module Data.Org
  ( OrgElement (..)
  , OrgContext (..)
  , GElement (..)
  , Headline (..)
  , Indent (..)
  , Keyword (..)
  , Token (..)
  , Priority (..)
  , Property (..)
  , Properties (..)
  , Tags (..)
  , Timestamp (..)
  , TimestampStatus (..)
  , Title (..)
  , TitleElement (..)
  , Todo (..)
  , Pragma (..)
  , Separator (..)
  , Sentence (..)
  , SentenceElement (..)
  , parse
  , mparse ) where

import Data.Org.Element
import Data.Org.Context
import Data.Org.Generic
import Data.Org.Headline
import Data.Org.Indent
import Data.Org.Keyword
import Data.Org.Token
import Data.Org.Pragma
import Data.Org.Priority
import Data.Org.Property
import Data.Org.Properties
import Data.Org.Tags
import Data.Org.Timestamp
import Data.Org.Title
import Data.Org.Todo
import Data.Org.Separator
import Data.Org.Sentence

import Control.Monad.State (runStateT)
import Data.Org.Element qualified as OrgElement
import Data.Text (Text, pack)
import Data.Text.Lazy.Builder ()
import Text.Megaparsec qualified as PS
import UnliftIO ()

parse :: OrgContext -> Text -> ([GElement], OrgContext)
parse ctx cmd = case PS.parse (runStateT (PS.manyTill OrgElement.parser PS.eof) ctx) "" cmd of
  Right val -> val
  Left err  -> ([GText (Token (pack (PS.errorBundlePretty err)))], ctx)

mparse :: Text -> ([GElement], OrgContext)
mparse = parse (mempty :: OrgContext)
