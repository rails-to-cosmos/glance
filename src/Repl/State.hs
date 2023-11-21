{-# LANGUAGE OverloadedStrings #-}

module Repl.State (applyCommand, parseStateful) where

import Control.Monad.State (runStateT)
import Data.Void (Void)
import Data.Text (Text, pack)
import Data.Text.Lazy.Builder ()
import UnliftIO ()

import Data.Org.Base (Parser, OrgElement(apply))
import Data.Org.Context (OrgContext)
import Data.Org.Generic
import Data.Org.PlainText

import Text.Megaparsec (parse, ParseErrorBundle, errorBundlePretty)

parseStateful :: OrgContext -> Text -> Either (ParseErrorBundle Text Void) (OrgGenericElement, OrgContext)
parseStateful ctx = parse parser ""
  where parser = runStateT apply ctx :: Parser (OrgGenericElement, OrgContext)

applyCommand :: OrgContext -> Text -> (OrgGenericElement, OrgContext)
applyCommand ctx cmd = case parseStateful ctx cmd of
  Right (els, ctx') -> (els, ctx')
  Left err -> (OrgGenericText (PlainText (pack (errorBundlePretty err))), ctx)
