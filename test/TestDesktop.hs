-- | @glance desktop@: which browser opens the window, with which arguments,
-- and what happens when there is none.
--
-- No window is ever opened here.  Discovery runs over a temp directory of fake
-- executables handed to 'resolveBrowser' as its path list — the parameter
-- exists for exactly this, since @setEnv \"PATH\"@ would change it for the whole
-- test process — and the one case that really launches something launches a
-- shell script that writes its arguments to a file.
module TestDesktop (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (finally)
import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory ( doesDirectoryExist, doesFileExist, getPermissions
                        , listDirectory, setOwnerExecutable, setPermissions )
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hClose, hFlush, stdout, withFile)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestDefaults (withTempDir)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Glance.Desktop ( browserCandidates, desktopURL, dryRunLines, openWindow
                      , resolveBrowser, windowLine )

spec :: TestTree
spec = testGroup "Desktop" [discoverySpec, spawnSpec, dryRunSpec]

-- Fixtures

-- | The URL a window is opened on, for a server on this port.
url :: String
url = desktopURL 7797

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

discoverySpec :: TestTree
discoverySpec = testGroup "Browser discovery"
  [ testCase "takes the first candidate the path holds, in app mode" $
      withTempDir $ \dir -> do
        fakeBrowsers dir ["google-chrome", "chromium", "xdg-open"]
        got <- resolved Nothing Nothing [dir]
        assertEqual "chromium leads the list"
                    (Just (dir </> "chromium" <> " --app=" <> url)) got

  , testCase "falls through the candidate list in order" $
      withTempDir $ \dir -> do
        fakeBrowsers dir ["vivaldi", "brave", "xdg-open"]
        got <- resolved Nothing Nothing [dir]
        assertEqual "brave before vivaldi"
                    (Just (dir </> "brave" <> " --app=" <> url)) got

  , testCase "every candidate is a browser that takes --app" $ do
      assertEqual "the list, in order"
        [ "chromium", "chromium-browser", "google-chrome-stable", "google-chrome"
        , "brave", "vivaldi" ] browserCandidates
      -- Firefox dropped --app; a name here that ignores it would open a
      -- full-chrome window and call it a desktop shell.
      assertBool "firefox is not on it" ("firefox" `notElem` browserCandidates)

  , testCase "--browser beats the candidates" $
      withTempDir $ \dir -> do
        fakeBrowsers dir ["chromium", "mybrowser", "xdg-open"]
        got <- resolved Nothing (Just "mybrowser") [dir]
        assertEqual "the flag wins"
                    (Just (dir </> "mybrowser" <> " --app=" <> url)) got

  , testCase "GLANCE_BROWSER beats --browser and the candidates" $
      withTempDir $ \dir -> do
        fakeBrowsers dir ["chromium", "mybrowser", "envbrowser", "xdg-open"]
        got <- resolved (Just "envbrowser") (Just "mybrowser") [dir]
        assertEqual "the environment wins"
                    (Just (dir </> "envbrowser" <> " --app=" <> url)) got

  , testCase "a named browser with a path is taken as it stands" $
      withTempDir $ \dir -> do
        exe <- fakeExecutable dir "elsewhere" "exit 0"
        got <- resolved Nothing (Just exe) []
        assertEqual "no path list is consulted" (Just (exe <> " --app=" <> url)) got

  , testCase "a named browser the path lacks is still what gets run" $
      withTempDir $ \dir -> do
        fakeBrowsers dir ["chromium", "xdg-open"]
        got <- resolved Nothing (Just "nosuchbrowser") [dir]
        -- Falling back to chromium here would silently run something other
        -- than what was asked for; the spawn says the name that was written.
        assertEqual "the name as given" (Just ("nosuchbrowser --app=" <> url)) got

  , testCase "with no browser at all, xdg-open opens a plain tab" $
      withTempDir $ \dir -> do
        fakeBrowsers dir ["xdg-open"]
        got <- resolved Nothing Nothing [dir]
        assertEqual "the URL alone — xdg-open takes no --app"
                    (Just (dir </> "xdg-open" <> " " <> url)) got

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
          -- normally, which is what keeps `serveWith' serving — and returning is
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
waitForFile path = go (200 :: Int)
  where
    go 0 = TIO.readFile path  -- fails with the read error, which names the path
    go n = do
      there <- doesFileExist path
      if there then TIO.readFile path else threadDelay 10000 >> go (n - 1)

-- Dry run

dryRunSpec :: TestTree
dryRunSpec = testGroup "--dry-run"
  [ testCase "prints the command it would run, and the URL it would open" $ do
      let lines' = dryRunLines (Just ("/usr/bin/chromium", ["--app=" <> url])) url
      assertEqual "three lines"
        [ "glance desktop — " <> url
        , "  window:  /usr/bin/chromium --app=" <> url
        , "  dry run — nothing started." ] lines'

  , testCase "the binary resolves a browser off the path and starts nothing" $ do
      built <- glanceBinary
      case built of
        -- Nothing built here: `cabal build all' makes this case unreachable,
        -- and a suite run against a bare checkout still passes.
        Nothing  -> pure ()
        Just exe -> withTempDir $ \dir -> do
          fakeBrowsers dir ["chromium", "xdg-open"]
          controlled <- pathOnly dir
          (code, out, err) <- readCreateProcessWithExitCode
            (proc exe ["desktop", "--dir", "test/fixtures/view", "--port", "7797", "--dry-run"])
              { env = Just controlled } ""
          assertEqual ("glance desktop --dry-run said: " <> err) ExitSuccess code
          -- The process returned at all, which is the assertion about the
          -- socket: a run that bound 7797 would still be serving on it.
          mapM_ (\needle -> assertBool ("dry run: no " <> show needle <> " in " <> out)
                                       (needle `isInfixOf` out))
                [ "glance desktop — " <> desktopURL 7797
                , dir </> "chromium" <> " --app=" <> desktopURL 7797
                , "nothing started" ]
  ]

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
