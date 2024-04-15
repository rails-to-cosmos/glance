{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment
import System.Exit

import Data.Org.Context
import Repl.Org
import Repl.State

main :: IO ()
main = getArgs >>= parse >>= putStr . tac

tac :: String -> String
tac  = unlines . reverse . lines

parse :: [String] -> IO a
parse ["--help"] = usage >> exit
parse ["--version"] = version >> exit
parse ["--repl"] = repl >> exit
parse []     = usage >> exit
parse (x : _) = parse [x]

repl :: IO ()
repl = runRepl "mydatabase.sqlite" 10 (mempty::OrgContext) parseOrgElements

usage :: IO ()
usage   = putStrLn "Usage: glance [--repl | --help | --version] [file ..]"

version :: IO ()
version = putStrLn "Haskell glance 0.1.0.0"

exit :: IO a
exit    = exitSuccess

die :: IO ()
die = exitFailure
