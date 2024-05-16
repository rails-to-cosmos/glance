module Main (main) where

import System.Environment
import System.Exit

import Data.ByteString.Char8 qualified as BSChar8
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Org qualified as Org
import Data.Config qualified as Config
import Data.Org.Context
import Repl.Org

import TextShow

import System.Directory
import System.FilePath
import System.Console.Haskeline qualified as Haskeline

defaultConfig :: IO Config.Config
defaultConfig = do
  homeDir <- getHomeDirectory

  let configDir = homeDir </> ".config" </> "glance"
      historyFile = Just (configDir </> ".history")
      dbFile = configDir </> "db.sqlite"

      haskelineSettings = Haskeline.defaultSettings { Haskeline.autoAddHistory = True
                                                    , Haskeline.historyFile = historyFile }

  createDirectoryIfMissing True configDir

  return Config.Config { Config.haskelineSettings = haskelineSettings
                       , Config.dbConnectionString = Text.pack dbFile
                       , Config.dbPoolSize = 10 }

defaultContext :: OrgContext
defaultContext = mempty

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO a

parse [] = do
  config <- defaultConfig
  repl config defaultContext
  exitSuccess

parse (filename:_) = do
  config <- defaultConfig
  content <- Text.pack . BSChar8.unpack <$> BS.readFile filename

  let (elements, context) = Org.parse defaultContext content

  print elements
  repl config context
  exitSuccess

repl :: Config.Config -> OrgContext -> IO ()
repl config context = do
  printT ("Hello there, fellow hacker!" :: Text.Text)
  printT (Text.intercalate " " ["I'll use meta db located in", Config.dbConnectionString config])

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
