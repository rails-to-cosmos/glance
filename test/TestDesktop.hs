-- | @glance desktop@: which window opens, with which arguments, and when there
-- is none.  No window is opened here — 'resolveBrowser' takes its path list as
-- a parameter, since @setEnv \"PATH\"@ would change it for the whole process.
module TestDesktop (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Exception (finally, throwIO, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (doesFileExist, getPermissions, setOwnerExecutable, setPermissions)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hClose, hFlush, stdout, withFile)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults (withGlanceBinary, withTempDir)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Glance.Desktop ( browserCandidates, desktopURL, dryRunLines, openWindow
                      , resolveBrowser, windowLine )
import Glance.Desktop.Native ( nativeDryRunLines, nativeEngine, nativeTitle
                             , nativeWindowLine, prefersNative, runNative )
import Glance.Desktop.WebKit (zoomAsked)
import Glance.Web.Base (zoomMax, zoomMin)

spec :: TestTree
spec = testGroup "Desktop"
  [discoverySpec, spawnSpec, dryRunSpec, nativeSpec, zoomSpec, flowSpec]


url :: String
url = desktopURL 7797

appMode :: FilePath -> String
appMode exe = exe <> " --app=" <> url

fakeExecutable :: FilePath -> String -> T.Text -> IO FilePath
fakeExecutable dir name body = do
  TIO.writeFile path (T.unlines ["#!/bin/sh", body])
  perms <- getPermissions path
  setPermissions path (setOwnerExecutable True perms)
  pure path
  where path = dir </> name

fakeBrowsers :: FilePath -> [String] -> IO ()
fakeBrowsers dir = mapM_ (\name -> fakeExecutable dir name "exit 0")

resolved :: Maybe String -> Maybe String -> [FilePath] -> IO (Maybe String)
resolved envVar flag dirs =
  fmap (\(exe, args) -> unwords (exe : args)) <$> resolveBrowser envVar flag dirs url


-- | Case name, claim, path executables, @$GLANCE_BROWSER@, @--browser@, answer.
type Resolution =
  (String, String, [String], Maybe String, Maybe String, FilePath -> String)

resolutions :: [Resolution]
resolutions =
  [ ( "takes the first candidate the path holds, in app mode"
    , "chromium leads the list", ["google-chrome", "chromium", "xdg-open"]
    , Nothing, Nothing, \dir -> appMode (dir </> "chromium") )
  , ( "falls through the candidate list in order"
    , "brave before vivaldi", ["vivaldi", "brave", "xdg-open"]
    , Nothing, Nothing, \dir -> appMode (dir </> "brave") )
    -- A NAMED command takes the URL plain: `--app' is chromium's alone, and
    -- forcing it on breaks every other opener, xdg-open first.
  , ( "--browser beats the candidates, and runs as given"
    , "the flag wins, URL plain", ["chromium", "mybrowser", "xdg-open"]
    , Nothing, Just "mybrowser", \dir -> dir </> "mybrowser" <> " " <> url )
  , ( "GLANCE_BROWSER beats --browser and the candidates"
    , "the environment wins, URL plain"
    , ["chromium", "mybrowser", "envbrowser", "xdg-open"]
    , Just "envbrowser", Just "mybrowser"
    , \dir -> dir </> "envbrowser" <> " " <> url )
    -- Falling back to chromium would silently run something other than asked for.
  , ( "a named browser the path lacks is still what gets run"
    , "the name as given", ["chromium", "xdg-open"]
    , Nothing, Just "nosuchbrowser", const ("nosuchbrowser " <> url) )
  , ( "with no browser at all, xdg-open opens a plain tab"
    , "the URL alone — xdg-open takes no --app", ["xdg-open"]
    , Nothing, Nothing, \dir -> dir </> "xdg-open" <> " " <> url )
  ]

discovery :: Resolution -> TestTree
discovery (what, says, names, envVar, flag, wants) =
  testCase what $ withTempDir $ \dir -> do
    fakeBrowsers dir names
    got <- resolved envVar flag [dir]
    assertEqual says (Just (wants dir)) got

discoverySpec :: TestTree
discoverySpec = testGroup "Browser discovery" $ map discovery resolutions <>
  [ testCase "every candidate is a browser that takes --app" $ do
      assertEqual "the list, in order"
        [ "chromium", "chromium-browser", "google-chrome-stable", "google-chrome"
        , "brave", "vivaldi" ] browserCandidates
      -- Firefox dropped --app; a name here that ignores it opens full chrome.
      assertBool "firefox is not on it" ("firefox" `notElem` browserCandidates)

  , testCase "a named browser with a path is taken as it stands" $
      withTempDir $ \dir -> do
        exe <- fakeExecutable dir "elsewhere" "exit 0"
        got <- resolved Nothing (Just exe) []
        assertEqual "no path list is consulted, URL plain"
                    (Just (exe <> " " <> url)) got

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
          -- normally, so what it printed is the only evidence it ran at all.
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

-- | Whatever ACT writes to stdout, kept at PATH.  Serialized on LOCK: the
-- redirection is process-wide, so two at once would each catch the other's.
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

-- | PATH's contents once something has written it.  The browser is spawned and
-- not waited on, so the test does the waiting the launcher refuses to.
waitForFile :: FilePath -> IO T.Text
waitForFile path = waitUntil ("the file " <> path) (doesFileExist path) >> TIO.readFile path


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
        -- The process returned at all: a run that bound 7797 would still serve.
        mapM_ (\needle -> assertBool ("dry run: no " <> show needle <> " in " <> out)
                                     (needle `isInfixOf` out))
              [ "glance desktop — " <> desktopURL 7797, "nothing started" ]
        -- A native window and chromium are both honest answers to --dry-run.
        assertBool ("dry run resolved no window at all: " <> out)
                   (  "native window" `isInfixOf` out
                   || appMode (dir </> "chromium") `isInfixOf` out )

  , testCase "a named browser beats whatever window the build has of its own" $
      withBuiltBinary ["chromium", "envbrowser", "xdg-open"]
                      (\d -> [("GLANCE_BROWSER", d </> "envbrowser")]) $ \dir out -> do
        assertBool ("dry run: the named browser is not in " <> out)
                   ((dir </> "envbrowser" <> " " <> url) `isInfixOf` out)
        assertBool ("dry run: a native window won anyway: " <> out)
                   (not ("native window" `isInfixOf` out))
  ]

-- | Run K over a directory holding NAMES as the whole @PATH@ and over what
-- @glance desktop --dry-run@ printed there; EXTRA leads that environment.
withBuiltBinary :: [String] -> (FilePath -> [(String, String)])
                -> (FilePath -> String -> Assertion) -> Assertion
withBuiltBinary names extra k =
  withGlanceBinary "desktop probe" $ \exe -> withTempDir $ \dir -> do
    fakeBrowsers dir names
    controlled <- pathOnly dir
    k dir =<< probe exe (extra dir <> controlled)

-- | A failure names the exit and stderr, since the assertions read stdout alone.
probe :: FilePath -> [(String, String)] -> IO String
probe exe environment = do
  (code, out, err) <- readCreateProcessWithExitCode
    (proc exe ["desktop", "--dir", "test/fixtures/view", "--port", "7797", "--dry-run"])
      { env = Just environment } ""
  assertEqual ("glance desktop --dry-run said: " <> err) ExitSuccess code
  pure out

-- | DIR as the whole @PATH@ and no @GLANCE_BROWSER@; the rest is inherited, the
-- locale the banner's em dash needs above all.
pathOnly :: FilePath -> IO [(String, String)]
pathOnly dir = do
  inherited <- getEnvironment
  pure (("PATH", dir) : [ kv | kv@(name, _) <- inherited, name `notElem` controlled ])
  where controlled = ["PATH", "GLANCE_BROWSER"]


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
        , "  window:  native window (" <> nativeEngine <> ") — " <> title
        , "  dry run — nothing started." ] (nativeDryRunLines title url)
      -- The lines that are not about which window opened must be the same lines.
      let browser = dryRunLines (Just ("/usr/bin/chromium", ["--app=" <> url])) url
      assertEqual "the URL line and the closing line are the browser path's"
        (drop 2 browser) (drop 2 (nativeDryRunLines title url))
      assertEqual "and so is the first" (take 1 browser) (take 1 (nativeDryRunLines title url))

  , testCase "the window line carries the title, and says which engine" $ do
      let said = nativeWindowLine "glance — /home/x/org"
      mapM_ (\needle -> assertBool (show needle <> " missing from " <> said)
                                   (needle `isInfixOf` said))
            ["native window", nativeEngine, "/home/x/org"]
  ]


-- | THE ZOOM BRIDGE.  The GTK half is behind the @native-window@ flag and
-- unreachable from an unflagged suite, so THE WIRING is checked as source —
-- what the file SAYS, which is where that drift would be.  'zoomAsked' sits
-- OUTSIDE the @#ifdef@ and is asked as a function, its band handed in the way
-- the app hands it.
zoomSpec :: TestTree
zoomSpec = testGroup "The zoom the page asks for"
  [ testCase "the page's door is registered beside quit's, and named as one" $ do
      src <- webKitSource
      mapM_ (\needle -> assertBool (show needle <> " missing from WebKit.hs")
                                   (needle `T.isInfixOf` src))
            [ "zoomName = T.pack \"zoom\""
            , "WK.userContentManagerRegisterScriptMessageHandler ucm zoomName"
            , "WK.onUserContentManagerScriptMessageReceived ucm (Just zoomName)"
            -- The level is WORN BY THE VIEW; a CSS zoom would put the panes'
            -- measured rects out against the styles drawn from them.
            , "WK.webViewSetZoomLevel view" ]

    -- THE ONE BAND, TWO UNITS: the page holds whole percentages and the window a
    -- level, and the band is HANDED DOWN ('Main.runDesktop' passes it) rather
    -- than spelled a second time, so there is nothing left to drift.
  , testCase "the level the page names is held inside the page's own band" $ do
      let band = (zoomMin, zoomMax)
          low  = fromIntegral zoomMin / 100 :: Double
          high = fromIntegral zoomMax / 100 :: Double
      assertEqual "under the floor, the floor" (Just low) (zoomAsked band (show (low / 2)))
      assertEqual "over the ceiling, the ceiling" (Just high) (zoomAsked band (show (high * 2)))
      assertEqual "and each edge is itself worn" (Just low, Just high)
                  (zoomAsked band (show low), zoomAsked band (show high))
      assertEqual "a level inside the band stands as it is"
                  (Just 1.25) (zoomAsked band "1.25")
      -- `Read Double' takes NaN and Infinity, and no window wears either; a
      -- message naming no number at all is dropped the same way.
      mapM_ (\said -> assertEqual (show said <> " is no level a window wears")
                                  Nothing (zoomAsked band said))
            ["NaN", "Infinity", "-Infinity", "1e400", "", "wide", "1.2.3", "100%"]
  ]

webKitSource :: IO T.Text
webKitSource = TIO.readFile "src-desktop-native/Glance/Desktop/WebKit.hs"


-- 'runNative' reports by printing and these cases let it: capturing stdout here
-- would hold a process-wide redirection over a wait and swallow the suite's.
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
        -- A window that failed to build has never taken this daemon down.
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

session :: (IORef [String] -> MVar () -> IORef Bool -> IO ()) -> IO ()
session act = do
  log' <- newIORef []
  held <- newEmptyMVar
  stopped <- newIORef False
  act log' held stopped

-- | What warp does here, minus the socket: it blocks, and it says when it is up.
fakeDaemon :: IORef [String] -> MVar () -> IORef Bool -> IO () -> IO ()
fakeDaemon log' held stopped ready =
  (note log' "listening" >> ready >> takeMVar held)
    `finally` writeIORef stopped True

window :: IORef [String] -> String -> IO ()
window log' at = note log' ("window on " <> at)

note :: IORef [String] -> String -> IO ()
note log' line = atomicModifyIORef' log' (\ls -> (ls <> [line], ()))

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

stillServing :: IORef Bool -> IO Bool
stillServing stopped = threadDelay 200000 >> (not <$> readIORef stopped)

