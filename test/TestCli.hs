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
  ]
  where commands = [ ("serve", "--dir"), ("desktop", "--browser")
                   , ("scan", "--include-derived"), ("repl", "FILE") ]

helping :: FilePath -> [String] -> IO String
helping exe args = do
  (code, out, err) <- readProcessWithExitCode exe args ""
  assertEqual ("glance " <> unwords args <> " said: " <> err) ExitSuccess code
  assertEqual ("glance " <> unwords args <> " complained") "" err
  pure out
