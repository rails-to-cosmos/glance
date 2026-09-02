{-# LANGUAGE CPP #-}

-- | @glance desktop@ with a window this process owns (AGENTS.hs).  The engine
-- arrives as a plain @URL -> IO ()@, so this compiles in both flag states.
module Glance.Desktop.Native
  ( desktopWith
  , nativeDryRunLines
  , nativeEngine
  , nativeTitle
  , nativeWindowLine
  , prefersNative
  , runNative
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception ( AsyncException (ThreadKilled), SomeException
                         , displayException, finally, fromException, try )
import Control.Monad (unless, void)
import Data.List (isPrefixOf)
import Data.Maybe (isJust, isNothing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

import Glance.Desktop (DesktopOptions (..), desktop, desktopURL, dryRunLines)
import Glance.Web.Watch (say)
import Glance.Web (ServeOptions (soDir, soPort), serveAs)

nativeTitle :: FilePath -> String
nativeTitle dir = "glance — " <> dir

prefersNative :: Bool -> Maybe String -> Maybe String -> Bool
prefersNative available env flag = available && isNothing (env <|> flag)

-- | The window engine THIS os builds: WebKitGTK on Linux, WKWebView on macOS.
-- One source for the report line and the test that reads it.
nativeEngine :: String
#ifdef darwin_HOST_OS
nativeEngine = "WKWebView"
#else
nativeEngine = "WebKitGTK"
#endif

nativeWindowLine :: String -> String
nativeWindowLine title = "  window:  native window (" <> nativeEngine <> ") — " <> title

-- | The browser path's report with its window line swapped, so shared lines
-- cannot drift.
nativeDryRunLines :: String -> String -> [String]
nativeDryRunLines title url = map swap (dryRunLines Nothing url)
  where swap line | "  window:" `isPrefixOf` line = nativeWindowLine title
                  | otherwise                     = line

-- | WINDOW blocks until the window closes; a fake in the suite.
desktopWith :: Bool -> (String -> String -> IO ()) -> Bool -> DesktopOptions -> IO ()
desktopWith available window keep opts = do
  env <- lookupEnv "GLANCE_BROWSER"
  case (prefersNative available env (doBrowser opts), doDryRun opts) of
    (False, _)   -> desktop opts
    (True, True) -> mapM_ putStrLn (nativeDryRunLines title url)
    (True, _run) -> runNative (serveAs "desktop" (doServe opts))
                              (announcing title (window title)) keep url
  where
    url   = desktopURL (soPort (doServe opts))
    title = nativeTitle (soDir (doServe opts))

announcing :: String -> (String -> IO ()) -> String -> IO ()
announcing title open at = say [nativeWindowLine title] >> open at

-- | The daemon runs on a thread and the window on this one: GTK requires the
-- main thread and warp does not.
runNative :: (IO () -> IO ()) -> (String -> IO ()) -> Bool -> String -> IO ()
runNative daemon window keep url = do
  gate <- newEmptyMVar     -- True once listening, False if the daemon stopped first.
  stopped <- newEmptyMVar
  server <- forkIO (reporting (daemon (void (tryPutMVar gate True)))
                      `finally` (void (tryPutMVar gate False) >> void (tryPutMVar stopped ())))
  listening <- takeMVar gate
  unless listening (exitWith (ExitFailure 1))
  opened <- try (window url)
  case opened of
    Left err -> do
      say [ "  window:  native window failed: " <> displayException (err :: SomeException)
          , "  the daemon is still serving — open " <> url <> " yourself, or C-c to stop it." ]
      takeMVar stopped
    Right () | keep -> takeMVar stopped
             | otherwise -> do
                 say ["  window closed — stopping the daemon."]
                 killThread server

-- | ACT on a thread of its own, where GHC's own report names no source.
reporting :: IO () -> IO ()
reporting act = do
  outcome <- (try act :: IO (Either SomeException ()))
  case outcome of
    Right () -> pure ()
    Left err -> unless (expected err)
                  (hPutStrLn stderr ("glance desktop: the daemon stopped: "
                                       <> displayException err))
  where
    expected err = isJust (fromException err :: Maybe ExitCode)
                || fromException err == Just ThreadKilled
