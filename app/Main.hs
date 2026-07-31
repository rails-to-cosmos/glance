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

import Glance.Desktop (DesktopOptions (..), desktop)
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

parse ("serve":args) = run "serve" serveUsage serve (serveOptions args)

parse ("desktop":args) = run "desktop" desktopUsage desktop (desktopOptions args)

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

-- | OPTIONS through ACT, or NAME's complaint and USAGE on stderr.
run :: String -> String -> (a -> IO ()) -> Either String a -> IO b
run name usage act parsed = case parsed of
  Left err -> do
    hPutStrLn stderr ("glance " <> name <> ": " <> err)
    hPutStrLn stderr usage
    exitFailure
  Right opts -> do
    act opts
    exitSuccess

serveUsage :: String
serveUsage = "usage: glance serve --dir DIR [--port N (default "
          <> show defaultPort <> ")] [--assets PATH]"

desktopUsage :: String
desktopUsage = "usage: glance desktop --dir DIR [--port N (default "
            <> show defaultPort <> ")] [--assets PATH] [--browser CMD] [--dry-run]"

-- | ARGS as desktop options, or what is wrong with them.  Hand-rolled: five
-- flags do not earn an option-parsing dependency.
desktopOptions :: [String] -> Either String DesktopOptions
desktopOptions = go (DesktopOptions (ServeOptions "" defaultPort defaultAssetsDir) Nothing False)
  where
    go opts [] | null (soDir (doServe opts)) = Left "--dir is required"
               | otherwise                   = Right opts
    go opts ("--dir":dir:rest)      = serving opts (\s -> s { soDir = dir }) rest
    go opts ("--assets":path:rest)  = serving opts (\s -> s { soAssets = path }) rest
    go opts ("--browser":cmd:rest)  = go opts { doBrowser = Just cmd } rest
    go opts ("--dry-run":rest)      = go opts { doDryRun = True } rest
    go opts ("--port":port:rest)    = case readMaybe port of
      Just n | n > 0 && n < 65536  -> serving opts (\s -> s { soPort = n }) rest
      _                            -> Left ("not a port number: " <> port)
    go _ [flag] | "--" `isPrefixOf` flag = Left (flag <> " needs a value")
    go _ (arg:_)                    = Left ("unknown argument: " <> arg)
    serving opts f = go opts { doServe = f (doServe opts) }

-- | ARGS as serve options: the desktop parser, minus the two flags that only
-- mean something with a window in front of the server.  One flag table for the
-- two commands, and a rejection that names the command the flag belongs to.
serveOptions :: [String] -> Either String ServeOptions
serveOptions args = do
  opts <- desktopOptions args
  case (doBrowser opts, doDryRun opts) of
    (Just _, _) -> Left "--browser is a desktop flag; serve opens no window"
    (_, True)   -> Left "--dry-run is a desktop flag; serve opens no window"
    _serving    -> Right (doServe opts)
