module Main (main) where

import Data.List (isPrefixOf)
import System.Environment
import System.Exit

import Data.Org (Context, defaultContext, orgParse)
import Data.Org.Edit (readDocument)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Text.Megaparsec (errorBundlePretty)

import Repl.Org
import Scan (runScan)

import Data.Org.Walk (WalkOptions (..), defaultWalk)
import Glance.Desktop (DesktopOptions (..))
import Glance.Desktop.Native (desktopWith)
import Glance.Desktop.WebKit (nativeAvailable, nativeWindow)
import Glance.Web (ServeOptions (..), defaultPort, serve)

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

-- | The REPL banner, with MESSAGES under it.
greetings :: [Text] -> IO ()
greetings messages = do
  TIO.putStrLn "---"
  TIO.putStrLn "Hello, fellow hacker!\n"
  mapM_ TIO.putStrLn messages
  TIO.putStrLn "---"
  TIO.putStrLn ""

parse :: [String] -> IO a

parse [] = repl [] [] defaultContext

parse ("scan":args) = do
  let derived = "--include-derived" `elem` args
      dirs = filter (/= "--include-derived") args
  runScan (WalkOptions derived) (if null dirs then ["."] else dirs)
  exitSuccess

parse ("serve":args) = run "serve" serveUsage serve (serveOptions args)

parse ("desktop":args) = run "desktop" desktopUsage runDesktop (desktopOptions args)

parse (filename:_) = do
  -- 'readDocument' rather than a decode of this REPL's own: every other reader
  -- in the codebase takes UTF-8 through it, and the latin-1 round trip this
  -- replaces gave a non-ASCII file mojibake titles and latin-1 offsets where
  -- the parser's spans are characters.
  content <- readDocument filename

  case content of
    Nothing -> do
      -- 'readDocument' answers 'Nothing' for a file that is not there, one
      -- that cannot be read, and one whose bytes are not UTF-8, so the sentence
      -- names all three rather than claiming the first.
      hPutStrLn stderr ("glance: cannot read " <> filename <> " as UTF-8 org")
      exitFailure
    Just (text, _digest) ->
      repl ["Additional context provided: " <> Text.pack filename]
           [ Text.pack (errorBundlePretty err) | Just err <- [maybeErr] ]
           context
      where (_elements, context, maybeErr) = orgParse defaultContext text

-- | Greet with MESSAGES, print NOTES under the banner and hand the terminal to
-- the REPL under CONTEXT.  The four steps both entry points take, in the one
-- order they take them.
repl :: [Text] -> [Text] -> Context -> IO a
repl messages notes context = do
  settings <- replSettings
  greetings messages
  mapM_ TIO.putStrLn notes
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
          <> show defaultPort <> ")] [--assets PATH] [--include-derived]"

desktopUsage :: String
desktopUsage = "usage: glance desktop --dir DIR [--port N (default "
            <> show defaultPort
            <> ")] [--assets PATH] [--browser CMD] [--dry-run] [--keep-serving]"
            <> " [--include-derived]"

-- | One desktop session: this build's own window when it has one and nothing
-- names a browser, and stage 1's app-mode browser otherwise.
runDesktop :: Desktop -> IO ()
runDesktop d = desktopWith nativeAvailable nativeWindow (dKeepServing d) (dWindow d)

-- | Everything @glance desktop@ takes: the window options, plus the flag that
-- means something only when the window is this process's own.
data Desktop = Desktop
  { dWindow      :: !DesktopOptions  -- ^ the daemon and the two stage-1 window flags.
  , dKeepServing :: !Bool            -- ^ @--keep-serving@: outlive the window.
  }

-- | What DESKTOP serves.
serveOf :: Desktop -> ServeOptions
serveOf = doServe . dWindow

-- | ARGS as desktop options, or what is wrong with them.  Hand-rolled: six
-- flags do not earn an option-parsing dependency.
desktopOptions :: [String] -> Either String Desktop
desktopOptions = go (Desktop (DesktopOptions bare Nothing False) False)
  where
    bare = ServeOptions "" defaultPort Nothing (woIncludeDerived defaultWalk)
    go d [] | null (soDir (serveOf d)) = Left "--dir is required"
            | otherwise                = Right d
    go d ("--dir":dir:rest)         = serving d (\s -> s { soDir = dir }) rest
    go d ("--assets":path:rest)     = serving d (\s -> s { soAssets = Just path }) rest
    go d ("--include-derived":rest) = serving d (\s -> s { soDerived = True }) rest
    go d ("--browser":cmd:rest)     = window d (\w -> w { doBrowser = Just cmd }) rest
    go d ("--dry-run":rest)         = window d (\w -> w { doDryRun = True }) rest
    go d ("--keep-serving":rest)    = go d { dKeepServing = True } rest
    go d ("--port":port:rest)       = case readMaybe port of
      Just n | n > 0 && n < 65536   -> serving d (\s -> s { soPort = n }) rest
      _                             -> Left ("not a port number: " <> port)
    go _ [flag] | "--" `isPrefixOf` flag = Left (flag <> " needs a value")
    go _ (arg:_)                    = Left ("unknown argument: " <> arg)
    window d f = go d { dWindow = f (dWindow d) }
    serving d f = window d (\w -> w { doServe = f (doServe w) })

-- | ARGS as serve options: the desktop parser, minus the three flags that only
-- mean something with a window in front of the server.  One flag table for the
-- two commands, and a rejection that names the command the flag belongs to.
serveOptions :: [String] -> Either String ServeOptions
serveOptions args = do
  d <- desktopOptions args
  case (doBrowser (dWindow d), doDryRun (dWindow d), dKeepServing d) of
    (Just _, _, _) -> Left "--browser is a desktop flag; serve opens no window"
    (_, True, _)   -> Left "--dry-run is a desktop flag; serve opens no window"
    (_, _, True)   -> Left "--keep-serving is a desktop flag; serve opens no window"
    _serving       -> Right (serveOf d)
