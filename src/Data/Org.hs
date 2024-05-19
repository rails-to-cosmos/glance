module Data.Org
  ( Org (..)
  , OrgContext (..)
  , OrgElement (..)
  , Headline (..)
  , Indent (..)
  , Keyword (..)
  , Tk (..)
  , Priority (..)
  , Property (..)
  , Properties (..)
  , Tags (..)
  , Ts (..)
  , TsStatus (..)
  , Title (..)
  , TitleElement (..)
  , Todo (..)
  , Pragma (..)
  , Sep (..)
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
import Data.Org.Element qualified as Org
import Data.Text (Text, pack)
import Data.Text.Lazy.Builder ()
import Text.Megaparsec qualified as MPS
import UnliftIO ()

parse :: OrgContext -> Text -> ([OrgElement], OrgContext)
parse ctx cmd = case MPS.parse (runStateT (MPS.manyTill Org.parser MPS.eof) ctx) "" cmd of
  Right val -> val
  Left err  -> ([], ctx)  -- GTk (Tk (pack (PS.errorBundlePretty err)))

mparse :: Text -> ([OrgElement], OrgContext)
mparse = parse (mempty :: OrgContext)
