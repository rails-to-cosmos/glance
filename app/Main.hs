{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment
import System.Exit

import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.ByteString as BS
import qualified Data.Text as Text

import Data.Org.Context
import Repl.Org
import Repl.State

main :: IO ()
main = getArgs >>= parse >>= putStr . tac

tac :: String -> String
tac  = unlines . reverse . lines

parse :: [String] -> IO a

parse [] = do
  repl
  exitSuccess

parse ["--help"] = do
  version
  usage
  exitSuccess

parse ["--version"] = do
  version
  exitSuccess

parse ("--context":filename:_) = do
  content <- Text.pack . BSChar8.unpack <$> BS.readFile filename

  let context = mempty :: OrgContext
      (elements, context') = parseOrgElements context content

  print elements
  runRepl "mydatabase.sqlite" 10 context' parseOrgElements
  exitSuccess

parse (x:xs) = do
  putStrLn ("Unknown argument skipped: " ++ x)
  parse xs

repl :: IO ()
repl = runRepl "mydatabase.sqlite" 10 (mempty::OrgContext) parseOrgElements

usage :: IO ()
usage   = putStrLn "Usage: glance [--repl | --help | --version | --file [filename] ]"

version :: IO ()
version = putStrLn "Haskell glance 0.1.0.0"
