{-# LANGUAGE OverloadedStrings #-}

module Repl.State (applyCommand, parseStateful) where

import Control.Monad.State (runStateT)
import Data.Void (Void)
import Data.Text (Text)
import Data.Text.Lazy.Builder ()
import UnliftIO ()

import Data.Org.Base (Parser, OrgElement(apply))
import Data.Org.Context (OrgContext)
import Data.Org.Generic (OrgGeneric)

import Text.Megaparsec (parse, ParseErrorBundle)

parseStateful :: OrgContext -> Text -> Either (ParseErrorBundle Text Void) (OrgGeneric, OrgContext)
parseStateful ctx = parse parser ""
  where parser = runStateT apply ctx :: Parser (OrgGeneric, OrgContext)

applyCommand :: OrgContext -> Text -> (Maybe OrgGeneric, OrgContext)
applyCommand ctx cmd = case parseStateful ctx cmd of
  Right (el, ctx') -> (Just el, ctx')
  Left _ -> (Nothing, ctx)
