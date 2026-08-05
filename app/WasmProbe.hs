-- | The WASM probe: the core run INSIDE a WASI runtime over a real tree —
-- walk, parse, count — which is the engine half of the daemon-in-the-page
-- milestone (docs/proposal-native-ports.md, host 4) proven end to end before
-- any JSFFI design.
--
-- Serial on purpose: WASI has one thread, and 'loadDirFilesSerially' is the
-- pool-free spelling of the same load the daemon runs.  The output is the
-- scan's vocabulary — files, rows, and the first row's title as a smoke of
-- the parse — so a reader can eyeball it against @glance scan@ over the same
-- tree.
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
