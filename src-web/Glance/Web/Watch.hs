-- | The file watch: inotify events in, store updates and frames out.
--
-- Three rules the loop keeps.  A changed file is re-parsed on its own, from
-- 'Glance.Query.defaultContext' by way of 'Glance.Query.loadFile' — per-file
-- context is a parser invariant, and a long-lived shared context would let one
-- file's @#+TODO:@ line reach another's headlines.  Events are debounced per
-- path, because an editor writes a file in a flurry of syscalls and each one
-- would otherwise cost a parse.  And a file that fails to parse keeps the rows
-- it had: 'Glance.Query.orgParse' is all-or-nothing, so a save caught mid-write
-- looks exactly like a file whose headlines all vanished, and dropping them
-- would empty the table until the next keystroke.
module Glance.Web.Watch
  ( debounceDelay
  , due
  , isWatchable
  , reload
  , watched
  , watchOrgTree
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Monad (forever, unless)
import Data.Map.Strict (Map)
import GHC.Clock (getMonotonicTime)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)

import qualified Data.Map.Strict as Map
import qualified System.FSNotify as FS

import Glance.Query (LoadFailure (..), WalkOptions (..), derivedPath, documentPath, loadFile)
import Glance.Web.Store ( Frame (..), Hub, applyFile, dropFile, publish )

-- | How long a path must stay quiet before it is re-parsed, in seconds.  An
-- editor's save is several events (truncate, write, rename, chmod) inside a few
-- milliseconds; 100 ms collapses them into one parse and is invisible against
-- the 1 s watch-to-render budget.
debounceDelay :: Double
debounceDelay = 0.1

-- | How often the drain loop looks for work.  Small enough that the delay a
-- path waits is 'debounceDelay' and not much more.
tick :: Int
tick = 25000

-- | Watch DIR and fold every org edit under it into HUB, over the tree OPTS
-- asks for.  Blocks forever: the manager lives as long as this call, so run it
-- in its own thread.
watchOrgTree :: WalkOptions -> FilePath -> Hub -> IO ()
watchOrgTree opts dir hub = do
  pending <- newTVarIO Map.empty
  FS.withManager $ \mgr -> do
    _stop <- FS.watchTree mgr dir (watched opts . FS.eventPath) (note pending)
    forever $ do
      threadDelay tick
      now <- getMonotonicTime
      paths <- atomically $ do
        (ripe, rest) <- due debounceDelay now <$> readTVar pending
        writeTVar pending rest
        pure ripe
      mapM_ (reload hub) paths
  where note pending event = do
          now <- getMonotonicTime
          atomically (modifyTVar' pending (Map.insert (FS.eventPath event) now))

-- | Is PATH one this watch reads, under OPTS?  What 'isWatchable' keeps, minus
-- what the walk declined to enter: a file the store was never given must not
-- arrive by way of an inotify event, or an org-glance mirror would appear in
-- the table the moment it was rewritten.
watched :: WalkOptions -> FilePath -> Bool
watched opts path = isWatchable path
                 && (woIncludeDerived opts || not (derivedPath path))

-- | Is PATH one this watch cares about?  A document by the walk's own rule
-- ('Glance.Query.documentPath'): an org file by extension, minus the two
-- sidecars Emacs writes beside a buffer — @.#name.org@, a lock symlink that
-- usually dangles, and @#name.org#@, an auto-save.  The walk keeps the same
-- rule, so the set this filter accepts is the set the store was given.
isWatchable :: FilePath -> Bool
isWatchable = documentPath

-- | The paths in PENDING last touched at least DELAY seconds before NOW, and
-- what is left pending.  Pure, and the whole of the debounce: a path that keeps
-- receiving events keeps being deferred, and is parsed once the writes stop.
-- The clock is 'GHC.Clock.getMonotonicTime', so a system clock stepping
-- backwards cannot leave a path pending forever.
due :: Double -> Double -> Map FilePath Double -> ([FilePath], Map FilePath Double)
due delay now pending = (Map.keys ripe, rest)
  where (ripe, rest) = Map.partition ((>= delay) . (now -)) pending

-- | Re-read PATH into HUB and stream what changed.  A path that no longer
-- exists is a deletion, whatever event brought us here: a rename arrives as an
-- add, a remove or both depending on how the editor saves, and asking the
-- filesystem is the answer that holds for all of them.
reload :: Hub -> FilePath -> IO ()
reload hub path = do
  started <- getMonotonicTime
  exists <- doesFileExist path
  outcome <- if exists then Just <$> loadFile path else pure Nothing
  frames <- publish hub (maybe (dropFile path) (applyFile path) outcome)
  finished <- getMonotonicTime
  report path outcome frames (finished - started)

-- | One line per event, when there was anything to say.  The elapsed time is
-- the re-parse metric the persistence gate is decided on
-- (docs/plan-org-console-web.md), so it is on every line that did work.
report :: FilePath -> Maybe (Either LoadFailure [a]) -> [Frame] -> Double -> IO ()
report path outcome frames elapsed = unless (null note && null frames) $ do
  putStrLn ("glance watch: " <> path <> " " <> summary <> " " <> millis elapsed)
  hFlush stdout
  where
    summary | not (null note)           = note
            | ViewChanged `elem` frames = "keywords changed — clients reconnect"
            | otherwise                 = show (length ups) <> " upsert, "
                                       <> show (length frames - length ups) <> " delete"
    ups  = [ () | UpsertRow _ <- frames ]
    note = case outcome of
      Just (Left ReadFailed)   -> "unreadable — rows kept"
      Just (Left DecodeFailed) -> "not UTF-8 — rows kept"
      Just (Left ParseFailed)  -> "parse failed — rows kept"
      _loaded                  -> ""

millis :: Double -> String
millis seconds = "(" <> show (round (seconds * 1000) :: Int) <> " ms)"
