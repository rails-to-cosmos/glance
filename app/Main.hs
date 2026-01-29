module Main (main) where

import System.Environment
import System.Exit

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSChar8
import Data.Config (Config (..))
import Data.Org qualified as Org

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO

import Repl.Org

import System.Directory
import System.FilePath
import System.Console.Haskeline qualified as Haskeline

defaultConfig :: IO Config
defaultConfig = do
  homeDir <- getHomeDirectory

  let configDir = homeDir </> ".config" </> "glance"
      historyFile = Just (configDir </> ".history")
      haskelineSettings = Haskeline.defaultSettings { Haskeline.autoAddHistory = True
                                                    , Haskeline.historyFile = historyFile }

  createDirectoryIfMissing True configDir

  return Config {..}

main :: IO ()
main = do
  getArgs >>= parse

greetings :: [[Text]] -> IO ()
greetings messages = do
  TIO.putStrLn "---"
  TIO.putStrLn "Hello, fellow hacker!\n"
  let _lines = map (Text.intercalate " ") messages
  mapM_ TIO.putStrLn _lines
  TIO.putStrLn "---"
  TIO.putStrLn ""

parse :: [String] -> IO a

parse [] = do
  config <- defaultConfig
  repl config mempty
  exitSuccess

parse (filename:_) = do
  config <- defaultConfig
  content <- Text.pack . BSChar8.unpack <$> BS.readFile filename

  let (_elements, context) = Org.mparse content

  greetings [["Additional context provided:", Text.pack filename]]

  runRepl config context Org.parse
  exitSuccess

repl :: Config -> Org.Context -> IO ()
repl config context = do
  greetings []
  runRepl config context Org.parse

-- parse (x:xs) = do
--   putStrLn ("Unknown argument skipped: " ++ x)
--   parse xs

-- parse ["--help"] = do
--   version
--   usage
--   exitSuccess

-- usage :: IO ()
-- usage   = putStrLn "Usage: glance [filename]"

-- version :: IO ()
-- version = putStrLn "Haskell glance 0.1.0.0"
