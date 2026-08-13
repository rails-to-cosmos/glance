-- | The WASM probe: the core run INSIDE a WASI runtime over a real tree — walk,
-- parse, count (docs/proposals/2026-08-05-native-ports.draft.md, host 4).
-- Serial on purpose: WASI has one thread.
module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Glance.Query ( HeadlineRecord (hrTitle), defaultWalk
                    , loadDirFilesSerially )

main :: IO ()
main = do
  args <- getArgs
  root <- case args of
    [dir] -> pure dir
    _bad  -> die "usage: glance-wasm-probe DIR"
  (files, dirErrs) <- loadDirFilesSerially defaultWalk root
  let rows = concat [ rs | (_path, Right rs) <- files ]
      failed = length [ () | (_path, Left _e) <- files ]
  TIO.putStrLn (T.pack ("files:   " <> show (length files)))
  TIO.putStrLn (T.pack ("rows:    " <> show (length rows)))
  TIO.putStrLn (T.pack ("failed:  " <> show failed))
  TIO.putStrLn (T.pack ("direrrs: " <> show dirErrs))
  case rows of
    (r : _more) -> TIO.putStrLn ("first:   " <> hrTitle r)
    []          -> TIO.putStrLn "first:   (no rows)"
