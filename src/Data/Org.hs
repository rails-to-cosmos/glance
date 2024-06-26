module Data.Org ( Context (..)
                , St (..)
                , Mut (..)
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
                , TitleElement (..)
                , Todo (..)
                , Pragma (..)
                , Separator (..)
                , Sentence (..)
                , SentenceElement (..)
                , parse
                , mparse ) where

import Data.Org.Parse qualified as OrgParse
import Data.Org.State
import Data.Org.Context

import Data.Org.Elements.Base
import Data.Org.Elements.Headline
import Data.Org.Elements.Indent
import Data.Org.Elements.Keyword
import Data.Org.Elements.Pragma
import Data.Org.Elements.Priority
import Data.Org.Elements.Properties
import Data.Org.Elements.Property
import Data.Org.Elements.Sentence
import Data.Org.Elements.Separator
import Data.Org.Elements.Tags
import Data.Org.Elements.Timestamp
import Data.Org.Elements.Title
import Data.Org.Elements.Todo
import Data.Org.Elements.Token

import Control.Monad.State (runStateT)
import Data.Text (Text)
import Data.Text.Lazy.Builder ()
import Text.Megaparsec qualified as MP
import UnliftIO ()

parse :: St -> Text -> ([Element], St)
parse ctx cmd = case MP.parse (runStateT (MP.manyTill OrgParse.parse MP.eof) ctx) "" cmd of
  Right val -> val
  Left err  -> ([], ctx)  -- GToken (Token (pack (PS.errorBundlePretty err)))

mparse :: Text -> ([Element], St)
mparse = parse (St (mempty :: Context))
