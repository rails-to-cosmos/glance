{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Org.Context
import Repl.Org
import Repl.State

main :: IO ()
main = runRepl "mydatabase.sqlite" 10 (mempty::OrgContext) applyCommand
