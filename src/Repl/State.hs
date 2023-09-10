{-# LANGUAGE OverloadedStrings #-}

module Repl.State (applyCommand) where

import Control.Monad.State (runStateT)
import Data.Text (Text)
import Data.Text.Lazy.Builder ()
import UnliftIO ()

import Data.Org.Base
import Data.Org.Context
import Data.Org.Generic

import Text.Megaparsec

generic :: OrgContext -> Parser (OrgGeneric, OrgContext)
generic ctx = runStateT apply ctx :: Parser (OrgGeneric, OrgContext)

applyCommand :: OrgContext -> Text -> OrgContext
applyCommand ctx cmd = case parse (generic ctx) "" cmd of
  Right (_, c) -> c {metaStack = EmptyStack}
  Left _ -> ctx
