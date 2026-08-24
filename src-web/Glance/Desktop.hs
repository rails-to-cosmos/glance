-- | @glance desktop@: the daemon with a window in front of it.  No window failure ever fails the daemon.
module Glance.Desktop
  ( DesktopOptions (..)
  , browserCandidates
  , desktop
  , desktopURL
  , dryRunLines
  , openWindow
  , resolveBrowser
  , windowLine
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (void)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (doesFileExist, findExecutablesInDirectories, getPermissions, executable)
import System.Environment (lookupEnv)
import System.FilePath (getSearchPath, isPathSeparator)
import System.Process (ProcessHandle, createProcess, new_session, proc, waitForProcess)

import Glance.Web (ServeOptions (soPort), serveAs)
import Glance.Web.Watch (say)

data DesktopOptions = DesktopOptions
  { doServe   :: !ServeOptions      -- ^ the daemon, exactly as @serve@ takes it.
  , doBrowser :: !(Maybe String)    -- ^ @--browser@: the command to open the window with.
  , doDryRun  :: !Bool              -- ^ @--dry-run@: print the resolved command and stop.
  } deriving (Eq, Show)

desktopURL :: Int -> String
desktopURL port = "http://127.0.0.1:" <> show port <> "/"

-- | The browsers tried, in order, when nothing names one; each takes @--app=URL@.  Firefox dropped @--app@ and is absent.
browserCandidates :: [FilePath]
browserCandidates =
  [ "chromium", "chromium-browser", "google-chrome-stable", "google-chrome"
  , "brave", "vivaldi" ]

-- | The command that opens URL, given @$GLANCE_BROWSER@ as ENV, @--browser@ as FLAG and PATH as DIRS.  'Nothing' means no window can be opened here at all.
-- A NAMED command is run as given with the URL appended — @xdg-open@ names the
-- default browser's own tab; only the AUTO hunt dresses its candidates in
-- @--app@, since those are the chromium family the good window is tied to.
resolveBrowser :: Maybe String -> Maybe String -> [FilePath] -> String
               -> IO (Maybe (FilePath, [String]))
resolveBrowser env flag dirs url = case env <|> flag of
  Just named -> Just . plain . fromMaybe named <$> onPath dirs named
  Nothing    -> firstJust (map candidate browserCandidates <> [opener])
  where
    appMode exe    = (exe, ["--app=" <> url])
    plain exe      = (exe, [url])
    candidate name = fmap appMode <$> onPath dirs name
    opener         = fmap plain <$> onPath dirs "xdg-open"

-- | NAME as something to run, looked for in DIRS.  A name carrying a separator is a path already.
onPath :: [FilePath] -> String -> IO (Maybe FilePath)
onPath dirs name
  | any isPathSeparator name = do
      there <- doesFileExist name
      runnable <- if there then executable <$> getPermissions name else pure False
      pure (if runnable then Just name else Nothing)
  | otherwise = listToMaybe <$> findExecutablesInDirectories dirs name

firstJust :: [IO (Maybe a)] -> IO (Maybe a)
firstJust []           = pure Nothing
firstJust (act : rest) = act >>= maybe (firstJust rest) (pure . Just)

-- | How a resolved command reads in the banner and in @--dry-run@ alike.
windowLine :: Maybe (FilePath, [String]) -> String
windowLine (Just (exe, args)) = "  window:  " <> unwords (exe : args)
windowLine Nothing = "  window:  none — no browser on PATH and no xdg-open; open the URL yourself"

dryRunLines :: Maybe (FilePath, [String]) -> String -> [String]
dryRunLines cmd url =
  [ "glance desktop — " <> url, windowLine cmd, "  dry run — nothing started." ]

-- | Open the window CMD describes, or say why there is none.  Every failure is reported and swallowed.
openWindow :: Maybe (FilePath, [String]) -> IO ()
openWindow cmd = do
  say [windowLine cmd]
  case cmd of
    Nothing -> pure ()
    Just (exe, args) -> do
      spawned <- try (spawn exe args)
      case spawned of
        Right handle -> void (forkIO (void (waitForProcess handle)))
        Left err -> say [ "  window:  failed to start " <> exe <> ": "
                            <> displayException (err :: SomeException) ]

-- | EXE ARGS in a session of its own, so a @Ctrl-C@ meant for the daemon does not reach the window.
spawn :: FilePath -> [String] -> IO ProcessHandle
spawn exe args = do
  (_in, _out, _err, handle) <- createProcess (proc exe args) { new_session = True }
  pure handle

-- | Run OPTS: resolve the window, then serve until killed.  @--dry-run@ stops before anything is bound.
desktop :: DesktopOptions -> IO ()
desktop opts = do
  env <- lookupEnv "GLANCE_BROWSER"
  dirs <- getSearchPath
  cmd <- resolveBrowser env (doBrowser opts) dirs url
  if doDryRun opts
    then mapM_ putStrLn (dryRunLines cmd url)
    else serveAs "desktop" (doServe opts) (openWindow cmd)
  where url = desktopURL (soPort (doServe opts))
