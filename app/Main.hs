module Main (main) where

import Data.List (isPrefixOf)
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

import Glance.Web (ServeOptions (..), defaultAssetsDir, defaultPort, serve)

import System.Directory
import System.FilePath
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
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

parse ("serve":args) = case serveOptions args of
  Left err -> do
    hPutStrLn stderr ("glance serve: " <> err)
    hPutStrLn stderr serveUsage
    exitFailure
  Right opts -> do
    serve opts
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

serveUsage :: String
serveUsage = "usage: glance serve --dir DIR [--port N (default "
          <> show defaultPort <> ")] [--assets PATH]"

-- | ARGS as serve options, or what is wrong with them.  Hand-rolled: three
-- flags do not earn an option-parsing dependency.
serveOptions :: [String] -> Either String ServeOptions
serveOptions = go (ServeOptions "" defaultPort defaultAssetsDir)
  where
    go opts [] | null (soDir opts) = Left "--dir is required"
               | otherwise         = Right opts
    go opts ("--dir":dir:rest)     = go opts { soDir = dir } rest
    go opts ("--assets":path:rest) = go opts { soAssets = path } rest
    go opts ("--port":port:rest)   = case readMaybe port of
      Just n | n > 0 && n < 65536 -> go opts { soPort = n } rest
      _                           -> Left ("not a port number: " <> port)
    go _ [flag] | "--" `isPrefixOf` flag = Left (flag <> " needs a value")
    go _ (arg:_)                   = Left ("unknown argument: " <> arg)
