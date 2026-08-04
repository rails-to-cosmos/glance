-- | @glance desktop@: which window opens, with which arguments, and what
-- happens when there is none.
--
-- No window is ever opened here, native or borrowed.  Discovery runs over a
-- temp directory of fake executables handed to 'resolveBrowser' as its path
-- list — the parameter exists for exactly this, since @setEnv \"PATH\"@ would
-- change it for the whole test process — the one case that really launches
-- something launches a shell script that writes its arguments to a file, and
-- the native flow runs a fake daemon under a fake window, so what is under
-- test there is the handoff between the three threads rather than GTK.
module TestDesktop (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Exception (finally, throwIO, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory ( doesDirectoryExist, doesFileExist, getPermissions
                        , listDirectory, setOwnerExecutable, setPermissions )
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hClose, hFlush, stdout, withFile)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults (withTempDir)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Glance.Desktop ( browserCandidates, desktopURL, dryRunLines, openWindow
                      , resolveBrowser, windowLine )
import Glance.Desktop.Native ( nativeDryRunLines, nativeTitle, nativeWindowLine
                             , prefersNative, runNative )

spec :: TestTree
spec = testGroup "Desktop" [discoverySpec, spawnSpec, dryRunSpec, nativeSpec, flowSpec]

-- Fixtures

-- | The URL a window is opened on, for a server on this port.
url :: String
url = desktopURL 7797

-- | EXE as a browser opening this test's URL in app mode.
appMode :: FilePath -> String
appMode exe = exe <> " --app=" <> url

-- | NAME as an executable in DIR, running BODY.  A browser that writes down
-- how it was called is enough of one for a launcher test.
fakeExecutable :: FilePath -> String -> T.Text -> IO FilePath
fakeExecutable dir name body = do
  TIO.writeFile path (T.unlines ["#!/bin/sh", body])
  perms <- getPermissions path
  setPermissions path (setOwnerExecutable True perms)
  pure path
  where path = dir </> name

-- | NAMES as executables in DIR that do nothing, the way a browser this test
-- only ever resolves does.
fakeBrowsers :: FilePath -> [String] -> IO ()
fakeBrowsers dir = mapM_ (\name -> fakeExecutable dir name "exit 0")

-- | What 'resolveBrowser' resolves to, as one string — the command line as
-- 'windowLine' would print it, minus the label.
resolved :: Maybe String -> Maybe String -> [FilePath] -> IO (Maybe String)
resolved envVar flag dirs =
  fmap (\(exe, args) -> unwords (exe : args)) <$> resolveBrowser envVar flag dirs url

-- Discovery

-- | One row of the discovery table: the case's name, what its assertion claims,
-- the executables the path directory holds, @$GLANCE_BROWSER@, @--browser@, and
-- the command line that must come out of that directory.
type Resolution =
  (String, String, [String], Maybe String, Maybe String, FilePath -> String)

-- | The discovery cases that differ in nothing but their answer.
resolutions :: [Resolution]
resolutions =
  [ ( "takes the first candidate the path holds, in app mode"
    , "chromium leads the list", ["google-chrome", "chromium", "xdg-open"]
    , Nothing, Nothing, \dir -> appMode (dir </> "chromium") )
  , ( "falls through the candidate list in order"
    , "brave before vivaldi", ["vivaldi", "brave", "xdg-open"]
    , Nothing, Nothing, \dir -> appMode (dir </> "brave") )
  , ( "--browser beats the candidates"
    , "the flag wins", ["chromium", "mybrowser", "xdg-open"]
    , Nothing, Just "mybrowser", \dir -> appMode (dir </> "mybrowser") )
  , ( "GLANCE_BROWSER beats --browser and the candidates"
    , "the environment wins", ["chromium", "mybrowser", "envbrowser", "xdg-open"]
    , Just "envbrowser", Just "mybrowser", \dir -> appMode (dir </> "envbrowser") )
    -- Falling back to chromium here would silently run something other than
    -- what was asked for; the spawn says the name that was written.
  , ( "a named browser the path lacks is still what gets run"
    , "the name as given", ["chromium", "xdg-open"]
    , Nothing, Just "nosuchbrowser", const (appMode "nosuchbrowser") )
  , ( "with no browser at all, xdg-open opens a plain tab"
    , "the URL alone — xdg-open takes no --app", ["xdg-open"]
    , Nothing, Nothing, \dir -> dir </> "xdg-open" <> " " <> url )
  ]

-- | ROW as a case: a temp directory holding the executables it names, and one
-- answer out of 'resolveBrowser' over it.
discovery :: Resolution -> TestTree
discovery (what, says, names, envVar, flag, wants) =
  testCase what $ withTempDir $ \dir -> do
    fakeBrowsers dir names
    got <- resolved envVar flag [dir]
    assertEqual says (Just (wants dir)) got

-- | 'resolutions' as cases, then the four that stand outside the table: the
-- candidate list itself, a browser named by path, and two that resolve nothing.
discoverySpec :: TestTree
discoverySpec = testGroup "Browser discovery" $ map discovery resolutions <>
  [ testCase "every candidate is a browser that takes --app" $ do
      assertEqual "the list, in order"
        [ "chromium", "chromium-browser", "google-chrome-stable", "google-chrome"
        , "brave", "vivaldi" ] browserCandidates
      -- Firefox dropped --app; a name here that ignores it would open a
      -- full-chrome window and call it a desktop shell.
      assertBool "firefox is not on it" ("firefox" `notElem` browserCandidates)

  , testCase "a named browser with a path is taken as it stands" $
      withTempDir $ \dir -> do
        exe <- fakeExecutable dir "elsewhere" "exit 0"
        got <- resolved Nothing (Just exe) []
        assertEqual "no path list is consulted" (Just (appMode exe)) got

  , testCase "with nothing on the path there is no window, and no failure" $
      withTempDir $ \dir -> do
        got <- resolveBrowser Nothing Nothing [dir] url
        assertEqual "nothing resolved" Nothing got
        assertBool "and the operator is told what to do"
                   ("open the URL yourself" `isInfixOf` windowLine Nothing)

  , testCase "a directory that is not on the path holds no browser" $
      withTempDir $ \dir -> do
        fakeBrowsers dir ["chromium"]
        got <- resolveBrowser Nothing Nothing [] url
        assertEqual "an empty path list finds nothing" Nothing got
  ]

-- Spawning

spawnSpec :: TestTree
spawnSpec = withResource (newMVar ()) (const (pure ())) $ \lock ->
  testGroup "Opening the window"
    [ testCase "the resolved browser is run, in app mode, on the loopback URL" $
        withTempDir $ \dir -> do
          let argv = dir </> "argv"
          _ <- fakeExecutable dir "chromium"
                 (T.pack ("printf '%s\\n' \"$@\" > " <> show argv))
          cmd <- resolveBrowser Nothing Nothing [dir] url
          openWindow cmd
          written <- waitForFile argv
          assertEqual "the arguments the window was opened with"
                      ["--app=" <> T.pack url] (T.lines written)

    , testCase "a browser that cannot be started leaves the daemon alone" $
        withTempDir $ \dir -> do
          -- No spawn is possible: the path names nothing.  'openWindow' returns
          -- normally, which is what keeps `serveAs' serving — and returning is
          -- all it does, so what it printed is the only evidence it ran at all.
          let missing = dir </> "not-installed"
          said <- capturedStdout lock (dir </> "said")
                                 (openWindow (Just (missing, ["--app=" <> url])))
          assertBool ("no failure reported in " <> show said)
                     (("failed to start " <> missing) `isInfixOf` T.unpack said)
          started <- doesFileExist (dir </> "argv")
          assertBool "something was started after all" (not started)

    , testCase "with no window to open, there is nothing to run" $
        withTempDir $ \dir -> do
          said <- capturedStdout lock (dir </> "said") (openWindow Nothing)
          assertBool ("no line saying there is no window: " <> show said)
                     ("open the URL yourself" `isInfixOf` T.unpack said)
          started <- doesFileExist (dir </> "argv")
          assertBool "something was started after all" (not started)
    ]

-- | Whatever ACT writes to stdout, kept at PATH and answered as text.
-- 'openWindow' reports through stdout and returns @()@ whatever happens, so
-- catching what it printed is the only way to assert that it said anything.
--
-- Serialized on LOCK: the redirection is process-wide, so two of these at once
-- would each catch the other's output and lose their own.
capturedStdout :: IO (MVar ()) -> FilePath -> IO () -> IO T.Text
capturedStdout lock path act = do
  held <- lock
  withMVar held $ \() -> do
    hFlush stdout
    saved <- hDuplicate stdout
    withFile path WriteMode $ \sink -> do
      hDuplicateTo sink stdout
      act `finally` (hFlush stdout >> hDuplicateTo saved stdout >> hClose saved)
  TIO.readFile path

-- | The contents of PATH once something has written it, or a failure after two
-- seconds.  The browser is spawned and not waited on — a launcher that waits
-- for the window is a launcher that never serves — so the test does the
-- waiting the launcher refuses to.
waitForFile :: FilePath -> IO T.Text
waitForFile path = waitUntil ("the file " <> path) (doesFileExist path) >> TIO.readFile path

-- Dry run

dryRunSpec :: TestTree
dryRunSpec = testGroup "--dry-run"
  [ testCase "prints the command it would run, and the URL it would open" $ do
      let lines' = dryRunLines (Just ("/usr/bin/chromium", ["--app=" <> url])) url
      assertEqual "three lines"
        [ "glance desktop — " <> url
        , "  window:  /usr/bin/chromium --app=" <> url
        , "  dry run — nothing started." ] lines'

  , testCase "the binary resolves a browser off the path and starts nothing" $
      withBuiltBinary ["chromium", "xdg-open"] (const []) $ \dir out -> do
        -- The process returned at all, which is the assertion about the
        -- socket: a run that bound 7797 would still be serving on it.
        mapM_ (\needle -> assertBool ("dry run: no " <> show needle <> " in " <> out)
                                     (needle `isInfixOf` out))
              [ "glance desktop — " <> desktopURL 7797, "nothing started" ]
        -- A build with a window of its own answers with that one, and one
        -- without has to find chromium.  Both are honest answers to the
        -- question --dry-run asks, and both start nothing.
        assertBool ("dry run resolved no window at all: " <> out)
                   (  "native window" `isInfixOf` out
                   || appMode (dir </> "chromium") `isInfixOf` out )

  , testCase "a named browser beats whatever window the build has of its own" $
      withBuiltBinary ["chromium", "envbrowser", "xdg-open"]
                      (\d -> [("GLANCE_BROWSER", d </> "envbrowser")]) $ \dir out -> do
        assertBool ("dry run: the named browser is not in " <> out)
                   (appMode (dir </> "envbrowser") `isInfixOf` out)
        assertBool ("dry run: a native window won anyway: " <> out)
                   (not ("native window" `isInfixOf` out))
  ]

-- | Run K over a directory holding NAMES as the whole @PATH@ and over what
-- @glance desktop --dry-run@ printed there.  EXTRA of that directory goes ahead
-- of the environment 'pathOnly' hands the child.
--
-- A checkout with nothing built skips the case: @cabal build all@ makes that
-- unreachable, and a suite run against a bare tree still passes.
withBuiltBinary :: [String] -> (FilePath -> [(String, String)])
                -> (FilePath -> String -> Assertion) -> Assertion
withBuiltBinary names extra k = do
  built <- glanceBinary
  case built of
    Nothing  -> pure ()
    Just exe -> withTempDir $ \dir -> do
      fakeBrowsers dir names
      controlled <- pathOnly dir
      k dir =<< probe exe (extra dir <> controlled)

-- | What EXE prints for @desktop --dry-run@ under ENVIRONMENT.  A failure
-- names the exit and what went to stderr, since the assertions past this read
-- stdout alone.
probe :: FilePath -> [(String, String)] -> IO String
probe exe environment = do
  (code, out, err) <- readCreateProcessWithExitCode
    (proc exe ["desktop", "--dir", "test/fixtures/view", "--port", "7797", "--dry-run"])
      { env = Just environment } ""
  assertEqual ("glance desktop --dry-run said: " <> err) ExitSuccess code
  pure out

-- | This environment with DIR as the whole @PATH@ and no @GLANCE_BROWSER@ in
-- it: the child's browser discovery is the test's to control, and the rest of
-- the environment — the locale the banner's em dash needs, above all — is the
-- one a terminal would hand it.
pathOnly :: FilePath -> IO [(String, String)]
pathOnly dir = do
  inherited <- getEnvironment
  pure (("PATH", dir) : [ kv | kv@(name, _) <- inherited, name `notElem` controlled ])
  where controlled = ["PATH", "GLANCE_BROWSER"]

-- | The @glance@ binary this checkout builds, if it is built.  @$GLANCE_BIN@
-- overrides; otherwise the one place cabal puts it.
glanceBinary :: IO (Maybe FilePath)
glanceBinary = do
  named <- lookupEnv "GLANCE_BIN"
  case named of
    Just path -> pure (Just path)
    Nothing   -> listToMaybe <$> globPath "dist-newstyle/build"
                   ["*", "*", "*", "x", "glance", "build", "glance", "glance"]

-- Native window: which one is preferred, and what it says

nativeSpec :: TestTree
nativeSpec = testGroup "Preferring the window this build owns"
  [ testCase "the resolution table, with the native path in it" $
      assertEqual "available, $GLANCE_BROWSER, --browser -> native?"
        [ True, False, False, False, False, False, False, False ]
        [ prefersNative have env flag
        | have <- [True, False]
        , env  <- [Nothing, Just "envbrowser"]
        , flag <- [Nothing, Just "mybrowser"] ]

  , testCase "naming a browser is asking for one, whatever the build has" $ do
      assertBool "GLANCE_BROWSER left the native window preferred"
                 (not (prefersNative True (Just "envbrowser") Nothing))
      assertBool "--browser left the native window preferred"
                 (not (prefersNative True Nothing (Just "mybrowser")))

  , testCase "the title names the directory the session is over" $
      assertEqual "one window per tree, told apart in a window list"
                  "glance — /home/x/org" (nativeTitle "/home/x/org")

  , testCase "--dry-run says native window where it says the browser command" $ do
      let title = nativeTitle "/home/x/org"
      assertEqual "three lines, the middle one this build's window"
        [ "glance desktop — " <> url
        , "  window:  native window (WebKitGTK) — " <> title
        , "  dry run — nothing started." ] (nativeDryRunLines title url)
      -- The two paths answer the same question, so the lines that are not
      -- about which window opened have to be the same lines.
      let browser = dryRunLines (Just ("/usr/bin/chromium", ["--app=" <> url])) url
      assertEqual "the URL line and the closing line are the browser path's"
        (drop 2 browser) (drop 2 (nativeDryRunLines title url))
      assertEqual "and so is the first" (take 1 browser) (take 1 (nativeDryRunLines title url))

  , testCase "the window line carries the title, and says which engine" $ do
      let said = nativeWindowLine "glance — /home/x/org"
      mapM_ (\needle -> assertBool (show needle <> " missing from " <> said)
                                   (needle `isInfixOf` said))
            ["native window", "WebKitGTK", "/home/x/org"]
  ]

-- Running the daemon under a window this process owns

-- 'runNative' reports what it did by printing, the way the rest of this
-- launcher does, and these cases let it: capturing stdout here would mean
-- holding a process-wide redirection for the length of a wait, and swallowing
-- whatever the rest of the suite printed into it.
flowSpec :: TestTree
flowSpec = testGroup "The window in front of the daemon"
  [ testCase "the window opens once the socket is listening, and not before" $
      session $ \log' held stopped -> do
        runNative (fakeDaemon log' held stopped) (window log') False url
        done <- readIORef log'
        assertEqual "the socket first" ["listening", "window on " <> url] done

  , testCase "closing the window stops the daemon" $
      session $ \log' held stopped -> do
        runNative (fakeDaemon log' held stopped) (window log') False url
        waitUntil "the daemon to stop" (readIORef stopped)

  , testCase "--keep-serving leaves the daemon running past the window" $
      session $ \log' held stopped -> do
        _ <- forkIO (runNative (fakeDaemon log' held stopped) (window log') True url)
        waitUntil "the window to close" (opened log')
        left <- stillServing stopped
        assertBool "the window took the daemon with it anyway" left
        putMVar held ()
        waitUntil "the daemon to stop when it is the daemon that stops"
                  (readIORef stopped)

  , testCase "a window that never opened leaves the daemon serving" $
      session $ \log' held stopped -> do
        let broken _at = throwIO (userError "no display")
        _ <- forkIO (runNative (fakeDaemon log' held stopped) broken False url)
        waitUntil "the daemon to listen" (elem "listening" <$> readIORef log')
        -- A window that failed to build is a window failure, and no window
        -- failure has ever taken this daemon down.
        left <- stillServing stopped
        assertBool "a window that failed stopped the daemon" left
        putMVar held ()

  , testCase "a daemon that stops before it listens takes the process with it" $ do
      let quiet _ready = pure ()
          never _at = assertFailure "a window opened over a daemon that never listened"
      code <- try (runNative quiet never False url)
      assertEqual "exits rather than waiting for a socket that is not coming"
                  (Left (ExitFailure 1)) code
  ]

-- | ACT over a fresh log, a release and a stopped flag — one desktop session's
-- worth of state, since every case here needs all three.
session :: (IORef [String] -> MVar () -> IORef Bool -> IO ()) -> IO ()
session act = do
  log' <- newIORef []
  held <- newEmptyMVar
  stopped <- newIORef False
  act log' held stopped

-- | A daemon that says so in LOG, calls back once it is listening, serves until
-- HELD is filled or it is killed, and sets STOPPED on its way out.  What warp
-- does here, minus the socket: it blocks, and it says when it is up.
fakeDaemon :: IORef [String] -> MVar () -> IORef Bool -> IO () -> IO ()
fakeDaemon log' held stopped ready =
  (note log' "listening" >> ready >> takeMVar held)
    `finally` writeIORef stopped True

-- | A window on AT that closes as soon as it opens, noting it in LOG.
window :: IORef [String] -> String -> IO ()
window log' at = note log' ("window on " <> at)

-- | Add LINE to LOG, in the order the lines were added.
note :: IORef [String] -> String -> IO ()
note log' line = atomicModifyIORef' log' (\ls -> (ls <> [line], ()))

-- | Whether LOG has a window in it yet.
opened :: IORef [String] -> IO Bool
opened log' = any ("window on " `isInfixOf`) <$> readIORef log'

-- | Wait for CHECK, or fail after two seconds naming WHAT.  Three threads hand
-- off here and none is asked to announce that it is finished.
waitUntil :: String -> IO Bool -> IO ()
waitUntil what check = go (200 :: Int)
  where
    go 0 = assertFailure ("timed out waiting for " <> what)
    go n = do
      ok <- check
      if ok then pure () else threadDelay 10000 >> go (n - 1)

-- | Whether STOPPED is still unset a beat later.  The claim is that nothing
-- happens to the daemon, and waiting is the only way to make it.
stillServing :: IORef Bool -> IO Bool
stillServing stopped = threadDelay 200000 >> (not <$> readIORef stopped)

-- | The paths under ROOT matching STEPS, where @*@ is any one directory.  A
-- three-line glob, for the three wildcards in cabal's build layout.
globPath :: FilePath -> [String] -> IO [FilePath]
globPath root [] = do
  there <- doesFileExist root
  pure [ root | there ]
globPath root ("*" : rest) = do
  isDir <- doesDirectoryExist root
  if not isDir then pure [] else do
    entries <- listDirectory root
    concat <$> mapM (\e -> globPath (root </> e) rest) entries
globPath root (step : rest) = globPath (root </> step) rest
