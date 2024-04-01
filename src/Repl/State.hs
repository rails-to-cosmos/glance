{-# LANGUAGE ImportQualifiedPost #-}

module Repl.State (applyCommand) where

import           Control.Monad.State (runStateT)
import           Data.Void (Void)
import           Data.Text (Text, pack)
import           Data.Text.Lazy.Builder ()
import           UnliftIO ()
import Data.Org.Element qualified as OrgElement
import           Data.Org.Context (OrgContext)
import           Data.Org.Generic
import           Data.Org.PlainText
import           Text.Megaparsec

parse' :: OrgContext -> Text -> Either (ParseErrorBundle Text Void) ([OrgGenericElement], OrgContext)
parse' ctx = parse (runStateT (manyTill OrgElement.parser eof) ctx) ""

applyCommand :: OrgContext -> Text -> ([OrgGenericElement], OrgContext)
applyCommand ctx cmd = case parse' ctx cmd of
  Right val -> val
  Left err  -> ([OrgGenericText (PlainText (pack (errorBundlePretty err)))], ctx)
