module Main (main) where

import System.Environment
import System.Exit

import Data.ByteString.Char8 qualified as BSChar8
import Data.ByteString qualified as BS
import Data.Text qualified as Text

import Data.Org qualified as Org
import Data.Org.Context
import Repl.Org

main :: IO ()
main = getArgs >>= parse

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
      (elements, context') = Org.parse context content

  print elements
  runRepl "mydatabase.sqlite" 10 context' Org.parse
  exitSuccess

parse (x:xs) = do
  putStrLn ("Unknown argument skipped: " ++ x)
  parse xs

repl :: IO ()
repl = runRepl "mydatabase.sqlite" 10 (mempty::OrgContext) Org.parse

usage :: IO ()
usage   = putStrLn "Usage: glance [--repl | --help | --version | --file [filename] ]"

version :: IO ()
version = putStrLn "Haskell glance 0.1.0.0"
