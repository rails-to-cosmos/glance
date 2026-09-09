-- | What the built binary answers when asked how to run it.
module TestCli (spec) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import TestDefaults (withGlanceBinary)

spec :: TestTree
spec = testGroup "The CLI's own help"
    -- A BARE `glance' IS ONE OF THE SPELLINGS; the REPL it used to open is `glance repl' now.
  [ testCase "every spelling of the ask prints the usage and exits 0" $
      withGlanceBinary "CLI help" $ \exe ->
        forM_ [[], ["--help"], ["-h"], ["help"]] $ \args -> do
          out <- helping exe args
          assertBool (unwords args <> " printed no usage: " <> out)
                     ("usage: glance" `isInfixOf` out)

    -- ASKING OUTRANKS RUNNING: no `--dir' is given, so a help that reached the parser would exit 1.
  , testCase "a command's own help is its block alone, and names its own flags" $
      withGlanceBinary "CLI help" $ \exe ->
        forM_ commands $ \(name, flag) -> do
          out <- helping exe [name, "--help"]
          assertEqual ("glance " <> name <> " --help printed the wrong blocks")
            [name] [ c | (c, _) <- commands, ("usage: glance " <> c) `isInfixOf` out ]
          assertBool ("glance " <> name <> " --help never names " <> flag)
                     (flag `isInfixOf` out)

    -- THE STDIO TRANSPORT END TO END: a request a line, its response a line, a
    -- notification answered with nothing, and EOF a clean exit.
  , testCase "glance mcp answers JSON-RPC over stdin, the notification silent" $
      withGlanceBinary "mcp stdio" $ \exe -> do
        let msgs = unlines
              [ "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}"
              , "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
              , "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\",\"params\":{}}" ]
        (code, out, _err) <-
          readProcessWithExitCode exe ["mcp", "--dir", "test/fixtures/subtree"] msgs
        assertEqual "glance mcp exits clean at EOF" ExitSuccess code
        assertEqual "one reply a request, the notification silent" 2 (length (lines out))
        assertBool ("the initialize names the server: " <> out)
                   ("\"name\":\"glance\"" `isInfixOf` out)
        assertBool ("tools/list carries the catalog: " <> out)
                   ("\"tools\"" `isInfixOf` out)
  ]
  where commands = [ ("serve", "--dir"), ("mcp", "--dir"), ("desktop", "--browser")
                   , ("doctor", "--include-derived"), ("repl", "FILE")
                   , ("backfill-created", "--dry-run") ]

helping :: FilePath -> [String] -> IO String
helping exe args = do
  (code, out, err) <- readProcessWithExitCode exe args ""
  assertEqual ("glance " <> unwords args <> " said: " <> err) ExitSuccess code
  assertEqual ("glance " <> unwords args <> " complained") "" err
  pure out
