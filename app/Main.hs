module Main (main) where

import System.Environment
import System.Exit

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import Data.Org (defaultContext, orgParse)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Text.Megaparsec (errorBundlePretty)

import Repl.Org
import Scan (runScan)

import System.Directory
import System.FilePath
import qualified System.Console.Haskeline as Haskeline

-- | Haskeline settings backed by a history file under ~/.config/glance.
replSettings :: IO (Haskeline.Settings IO)
replSettings = do
  homeDir <- getHomeDirectory

  let configDir = homeDir </> ".config" </> "glance"

  createDirectoryIfMissing True configDir

  return Haskeline.defaultSettings { Haskeline.autoAddHistory = True
                                   , Haskeline.historyFile = Just (configDir </> ".history") }

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
  settings <- replSettings
  greetings []
  runRepl settings defaultContext
  exitSuccess

parse ("scan":dirs) = do
  runScan (if null dirs then ["."] else dirs)
  exitSuccess

parse (filename:_) = do
  settings <- replSettings
  content <- Text.pack . BSChar8.unpack <$> BS.readFile filename

  let (_elements, context, maybeErr) = orgParse defaultContext content

  greetings [ ["Additional context provided:", Text.pack filename]]

  case maybeErr of
    Just err -> TIO.putStrLn $ Text.pack (errorBundlePretty err)
    Nothing  -> pure ()

  runRepl settings context
  exitSuccess
