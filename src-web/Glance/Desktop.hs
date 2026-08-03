-- | @glance desktop@: the daemon with a window in front of it.
--
-- Stage 1 of the proposal's desktop shell (rev 4) and all of it that is code
-- here: the architecture is unchanged — daemon, bridge, web UI — and the window
-- is one more client of the same loopback server.  What it adds over @serve@ is
-- an app-mode browser window, opened the moment the socket is listening, so
-- there is no browser chrome, no address bar and no tab to find.
--
-- The window is opened at the socket rather than at the loaded store.  That is
-- the whole point of 'Glance.Web.serveAs': a cold daemon spends seconds
-- walking the tree, and the page that says so is worth more than a window that
-- appears once the wait is over.
--
-- Nothing here can fail the daemon.  A browser that is not installed, a
-- @--browser@ that is not on the path, an @xdg-open@ that is missing — each
-- prints what happened and leaves the server running, because the org files are
-- served whether or not a window found them.
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

-- | What one desktop session runs: a server, and the window in front of it.
data DesktopOptions = DesktopOptions
  { doServe   :: !ServeOptions      -- ^ the daemon, exactly as @serve@ takes it.
  , doBrowser :: !(Maybe String)    -- ^ @--browser@: the command to open the window with.
  , doDryRun  :: !Bool              -- ^ @--dry-run@: print the resolved command and stop.
  } deriving (Eq, Show)

-- | Where the window points: this server, on the loopback address it binds.
desktopURL :: Int -> String
desktopURL port = "http://127.0.0.1:" <> show port <> "/"

-- | The browsers tried, in order, when nothing names one.  Chromium first
-- because it is the one this is developed against; every name here takes
-- @--app=URL@ and opens a chrome-less window on it, which is the whole trick.
-- Firefox is absent on purpose: it dropped @--app@ and its @--kiosk@ is a
-- different thing.
browserCandidates :: [FilePath]
browserCandidates =
  [ "chromium", "chromium-browser", "google-chrome-stable", "google-chrome"
  , "brave", "vivaldi" ]

-- | The command that opens URL, given @$GLANCE_BROWSER@ as ENV, @--browser@ as
-- FLAG, and PATH as the directories to look in.  'Nothing' means no window can
-- be opened here at all.
--
-- Order: the environment, then the flag, then 'browserCandidates', then
-- @xdg-open@.  The environment leads so that a machine whose one browser is not
-- on this list can be set up once, in a shell profile, and have every
-- invocation obey it — including the ones a launcher makes, which carry no
-- flags to override.
--
-- A named browser is taken at its word.  A bare name is looked up on the path
-- list and used as given when the lookup fails, so the spawn reports the name
-- the operator wrote rather than this silently falling through to something
-- else; a name with a separator in it is a path and is used as it stands.  It
-- also gets @--app=@, which is the contract: @GLANCE_BROWSER@ names a
-- chromium-family browser, and something else there opens a plain window at
-- best.
--
-- @xdg-open@ is the fallback and gets the URL alone — a tab in whatever browser
-- the desktop prefers, which is a worse window and a working one.
resolveBrowser :: Maybe String -> Maybe String -> [FilePath] -> String
               -> IO (Maybe (FilePath, [String]))
resolveBrowser env flag dirs url = case env <|> flag of
  Just named -> Just . appMode . fromMaybe named <$> onPath dirs named
  Nothing    -> firstJust (map candidate browserCandidates <> [opener])
  where
    appMode exe    = (exe, ["--app=" <> url])
    candidate name = fmap appMode <$> onPath dirs name
    opener         = fmap (\exe -> (exe, [url])) <$> onPath dirs "xdg-open"

-- | NAME as something to run, looked for in DIRS.  A name carrying a separator
-- is a path already and only has to exist and be executable.
onPath :: [FilePath] -> String -> IO (Maybe FilePath)
onPath dirs name
  | any isPathSeparator name = do
      there <- doesFileExist name
      runnable <- if there then executable <$> getPermissions name else pure False
      pure (if runnable then Just name else Nothing)
  | otherwise = listToMaybe <$> findExecutablesInDirectories dirs name

-- | The first of ACTS to answer, or 'Nothing' when none does.  What makes the
-- browser ladder one list: every rung is a lookup that may come up empty, and
-- @xdg-open@ is the last of them rather than a case standing beside them.
firstJust :: [IO (Maybe a)] -> IO (Maybe a)
firstJust []           = pure Nothing
firstJust (act : rest) = act >>= maybe (firstJust rest) (pure . Just)

-- | How a resolved command reads in the banner and in @--dry-run@ alike, so
-- the probe prints the line the run would.
windowLine :: Maybe (FilePath, [String]) -> String
windowLine (Just (exe, args)) = "  window:  " <> unwords (exe : args)
windowLine Nothing = "  window:  none — no browser on PATH and no xdg-open; open the URL yourself"

-- | What @--dry-run@ prints for CMD at URL: the command line it would run, and
-- that it ran nothing.
dryRunLines :: Maybe (FilePath, [String]) -> String -> [String]
dryRunLines cmd url =
  [ "glance desktop — " <> url, windowLine cmd, "  dry run — nothing started." ]

-- | Open the window CMD describes, or say why there is none.  Detached and
-- never waited on: the window outlives nothing here, and closing it leaves the
-- daemon serving.  Every failure is reported and swallowed — a daemon that dies
-- because a browser is missing is worse at serving org files than one that says
-- so and keeps going.
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

-- | EXE ARGS in a session of its own, so a @Ctrl-C@ meant for the daemon does
-- not reach the window through the terminal's process group.  'openWindow'
-- waits on the handle in a thread of its own, for the one reason a launcher
-- has to: a browser that exits leaves no zombie behind.
spawn :: FilePath -> [String] -> IO ProcessHandle
spawn exe args = do
  (_in, _out, _err, handle) <- createProcess (proc exe args) { new_session = True }
  pure handle

-- | Run OPTS: resolve the window, then serve until killed with the window
-- opened as soon as the socket is listening.
--
-- @--dry-run@ stops after the resolution and before anything is bound — it is a
-- probe of what would be run, and serving is @serve@'s job.  So a port stays
-- free across it, which is the check a test can make of a launcher without a
-- launcher's side effects.
desktop :: DesktopOptions -> IO ()
desktop opts = do
  env <- lookupEnv "GLANCE_BROWSER"
  dirs <- getSearchPath
  cmd <- resolveBrowser env (doBrowser opts) dirs url
  if doDryRun opts
    then mapM_ putStrLn (dryRunLines cmd url)
    -- Named for the subcommand, so the banner says which of the two started:
    -- the daemon is the same one `serve' runs.
    else serveAs "desktop" (doServe opts) (openWindow cmd)
  where url = desktopURL (soPort (doServe opts))
