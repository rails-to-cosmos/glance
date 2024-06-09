module Data.Org ( Context (..)
                , OrgState.State (..)
                , OrgState.Mutable (..)
                , Element (..)
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
                , Todo (..)
                , Pragma (..)
                , Separator (..)
                , Sentence (..)
                , parse
                , mparse
                , sparse ) where

import Data.Org.Parse qualified as OrgParse
import Data.Org.State qualified as OrgState
import Data.Org.Context

import Data.Org.Elements.Base
import Data.Org.Elements.Headline
import Data.Org.Elements.Indent
import Data.Org.Elements.Keyword
import Data.Org.Elements.Pragma
import Data.Org.Elements.Priority
import Data.Org.Elements.Properties
import Data.Org.Elements.Sentence (Sentence(..))
import Data.Org.Elements.Separator
import Data.Org.Elements.Tags
import Data.Org.Elements.Timestamp
import Data.Org.Elements.Title (Title(..))
import Data.Org.Elements.Todo
import Data.Org.Elements.Token

import Control.Monad.State (runStateT)
import Data.Text (Text)
import Data.Text.Lazy.Builder ()
import Text.Megaparsec qualified as MP
import UnliftIO ()

parse :: OrgState.State -> Text -> ([Element], OrgState.State)
parse ctx cmd = case MP.parse (runStateT (MP.manyTill OrgParse.parse MP.eof) ctx) "" cmd of
  Right v -> v
  Left err  -> ([], ctx)  -- GToken (Token (pack (PS.errorBundlePretty err)))

mparse :: Text -> ([Element], OrgState.State)
mparse = parse (OrgState.State (mempty :: Context))

sparse :: Text -> [Element]
sparse a = case mparse a of
  (elems, _state) -> elems
