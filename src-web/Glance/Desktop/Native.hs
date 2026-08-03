-- | @glance desktop@ with a window this process owns.
--
-- Stage 2 of the proposal's desktop shell (rev 4), and the half of it that has
-- no GTK in it: which window to open, when to open it, and what closing it
-- means.  The engine is 'Glance.Desktop.WebKit', built only under the
-- @native-window@ flag and reached here as a plain @URL -> IO ()@ the CLI hands
-- over — so this module compiles, and is tested, in both flag states.
--
-- Two rules decide the window.  Explicit beats native: @$GLANCE_BROWSER@ or
-- @--browser@ names a browser and gets one, in every build.  Otherwise a build
-- that has a window of its own uses it, and one that does not runs stage 1's
-- browser ladder ('Glance.Desktop.desktop').
--
-- The window is the app.  Stage 1 opens a browser window and closing it is
-- closing a tab; here the window is this process, so closing it stops the
-- daemon.  @--keep-serving@ puts stage 1's semantics back for a session that
-- wants the server to outlive the window.
module Glance.Desktop.Native
  ( desktopWith
  , nativeDryRunLines
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

-- | What the window over DIR is called.  The directory is the session: two
-- daemons over two trees are two windows, and the title is what tells them
-- apart in a window list.
nativeTitle :: FilePath -> String
nativeTitle dir = "glance — " <> dir

-- | Whether to open our own window, given whether this build HAS one and the
-- @$GLANCE_BROWSER@ ENV and @--browser@ FLAG that would name another.
--
-- Naming a browser is asking for it.  The ladder's own order — environment,
-- then flag — is 'Glance.Desktop.resolveBrowser''s and stays there; all this
-- asks is whether either spoke.
prefersNative :: Bool -> Maybe String -> Maybe String -> Bool
prefersNative available env flag = available && isNothing (env <|> flag)

-- | How the native window reads where the browser ladder prints its command.
nativeWindowLine :: String -> String
nativeWindowLine title = "  window:  native window (WebKitGTK) — " <> title

-- | What @--dry-run@ prints for a native window titled TITLE at URL.
--
-- The browser path's report with its window line swapped, so the two lines the
-- two paths share cannot drift: @--dry-run@ answers what this machine
-- resolves to, and it answers it the same way whichever window won.
nativeDryRunLines :: String -> String -> [String]
nativeDryRunLines title url = map swap (dryRunLines Nothing url)
  where swap line | "  window:" `isPrefixOf` line = nativeWindowLine title
                  | otherwise                     = line

-- | Run OPTS, preferring the window WINDOW opens when this build HAS one, and
-- keeping the daemon past the window when KEEP.
--
-- WINDOW takes a title and a URL and blocks until the window closes; it is
-- 'Glance.Desktop.WebKit.nativeWindow' in the CLI and a fake in the suite.
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

-- | OPEN with its window line printed first, under TITLE.  The banner owes the
-- operator a line saying which window opened, and it owes the line
-- @--dry-run@ promised; printing here rather than before 'runNative' puts it
-- where stage 1 puts its own, after the banner and once the socket is up.
announcing :: String -> (String -> IO ()) -> String -> IO ()
announcing title open at = say [nativeWindowLine title] >> open at

-- | Serve on DAEMON with WINDOW in front of it at URL, keeping the daemon past
-- the window when KEEP.
--
-- DAEMON takes the callback to run once the socket is listening and blocks;
-- WINDOW takes the URL and blocks until the window closes.  The window is
-- opened at the socket rather than at the loaded store, for the reason stage 1
-- opens a browser there: the page that says @indexing@ is worth more than a
-- window that appears once the walk is over.
--
-- The daemon runs on a thread and the window on this one, because GTK requires
-- the main thread and warp does not.  A daemon that stops before it listens —
-- a missing directory, a taken port — has already said why, and this exits
-- rather than waiting for a socket that is not coming.
--
-- A window that never opened leaves the daemon serving.  Closing one that did
-- stops it, unless KEEP: the window is the app, and a window that failed to
-- build is a window failure, which has never taken this daemon down.
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

-- | ACT with whatever ended it named rather than dumped.  It runs on a thread
-- of its own, where GHC's own report names no source and where the two ways it
-- ends here — @die@ and this module's own 'killThread' — have both been
-- accounted for already.
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
