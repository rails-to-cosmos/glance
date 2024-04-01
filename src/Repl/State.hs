{-# LANGUAGE ImportQualifiedPost #-}

module Repl.State (parseOrgElements) where

import Control.Monad.State (runStateT)
import Data.Org.Context (OrgContext)
import Data.Org.Element qualified as OrgElement
import Data.Org.Generic
import Data.Org.PlainText
import Data.Text (Text, pack)
import Data.Text.Lazy.Builder ()
import Text.Megaparsec
import UnliftIO ()

parseOrgElements :: OrgContext -> Text -> ([OrgGenericElement], OrgContext)
parseOrgElements ctx cmd = case parse (runStateT (manyTill OrgElement.parser eof) ctx) "" cmd of
  Right val -> val
  Left err  -> ([OrgGenericText (PlainText (pack (errorBundlePretty err)))], ctx)
