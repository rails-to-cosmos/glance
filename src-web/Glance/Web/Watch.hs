-- | The file watch: inotify events in, store updates and frames out.
--
-- Four rules the loop keeps.  A changed file is re-parsed on its own, under the
-- store's own config layers by way of 'Glance.Query.loadFileWith' — that seed
-- is a constant of the load, so no file's @#+TODO:@ line reaches another's
-- headlines and a re-read lands where the full walk left it.  Events are
-- debounced per path, because an editor writes a file in a flurry of syscalls
-- and each one would otherwise cost a parse.  A file that fails to parse keeps
-- the rows it had: 'Glance.Query.orgParse' is all-or-nothing, so a save caught
-- mid-write looks exactly like a file whose headlines all vanished, and
-- dropping them would empty the table until the next keystroke.
--
-- And a @.org-glance\/config@ file is the one event that is not about its own
-- path: it changes what every OTHER file parses, so the answer is a reseed —
-- the config re-read and the whole tree re-walked ('reseed').  Expensive, and
-- the only correct answer to a keyword that has just started existing.
--
-- Not every path this loop answers arrives from inotify.  fsnotify arms a newly
-- created directory without traversing INTO it, so a file under one is unwatched
-- for as long as the daemon runs and no event for it is ever coming.  Every
-- write route therefore leaves through 'writeSpans', which queues the path it
-- just wrote; the queue is the one inotify fills, and 'nudge' is its only door.
module Glance.Web.Watch
  ( debounceDelay
  , drain
  , due
  , isWatchable
  , nudge
  , say
  , settle
  , watched
  , watchOrgTree
  , writeSpans
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM ( atomically, modifyTVar', readTVar, readTVarIO
                              , writeTVar )
import Control.Monad (forever, unless, when)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)

import qualified Data.Map.Strict as Map
import qualified System.FSNotify as FS

import Glance.Query ( LoadFailure (..), Span, WalkOptions (..), WriteFailure
                    , configPath, derivedPath, documentPath, loadFileWith
                    , replaceSpans )
import Glance.Web.Store ( Frame (..), Hub (hubPending, hubStore), Store (stConfig)
                        , applyFile, dropFile, loadStoreWith, publish, reseeded )

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
watchOrgTree opts dir hub =
  FS.withManager $ \mgr -> do
    _stop <- FS.watchTree mgr dir (watched opts . FS.eventPath) note
    forever (threadDelay tick >> drain opts debounceDelay dir hub)
  where note = nudge opts hub . FS.eventPath

-- | One turn of the drain loop: whatever in HUB's queue has been quiet for
-- DELAY, taken out of it and settled.
--
-- Taking and settling are two steps and the take is the transaction, so a path
-- that arrives while 'settle' is running waits for the next turn rather than
-- being lost — which is what lets a nudge be dropped in from a request thread
-- while the loop is mid-parse.  Answering the queue rather than a caller's list
-- is also what a test needs to check that the daemon put a path there itself.
--
-- A turn with nothing ripe writes nothing.  The loop takes 40 turns a second
-- and request threads now write the same TVar, so an unconditional 'writeTVar'
-- of the map it just read would dirty it 40 times a second and make a
-- concurrent 'nudge' retry for no reason.
drain :: WalkOptions -> Double -> FilePath -> Hub -> IO ()
drain opts delay dir hub = do
  now <- getMonotonicTime
  paths <- atomically $ do
    (ripe, rest) <- due delay now <$> readTVar (hubPending hub)
    unless (null ripe) (writeTVar (hubPending hub) rest)
    pure ripe
  settle opts dir hub paths

-- | Queue PATH for re-reading, as if an event had arrived for it.
--
-- The one door into the queue, which is what makes the rule total: inotify's
-- own handler comes through here too, so a path is filtered by 'watched' on the
-- way in whoever put it there and a nudge can no more smuggle a derived mirror
-- into the table than an event can.  A path the walk declines is dropped in
-- silence, which is the same answer the fsnotify predicate gives it.
--
-- What it buys is the path nothing is watching.  fsnotify arms a newly created
-- directory but does not traverse into it, so a blob under
-- @data\/\<shard>\/\<rest>\/@ raises no event ever — not for the capture that
-- made it and not for any write after.  The daemon knows the path at write
-- time, so it says so ('writeSpans'), and the row arrives the way every other
-- row does.
--
-- The store is untouched here.  This drops a path in a map; 'settle' on the
-- drain loop is still the only thing that loads a file or publishes a frame, so
-- the watch stays the sole updater of the store and the debounce still holds —
-- a real event landing after a nudge overwrites the timestamp and the pair
-- costs one parse.
nudge :: WalkOptions -> Hub -> FilePath -> IO ()
nudge opts hub path = when (watched opts path) $ do
  now <- getMonotonicTime
  atomically (modifyTVar' (hubPending hub) (Map.insert path now))

-- | EDITS spliced into PATH under DIGEST, and PATH queued where that worked.
--
-- THE ONE DOOR EVERY WRITE ROUTE LEAVES THROUGH, which is the whole rule: the
-- daemon queues every path it writes.  Stating it as "the writes that CREATE"
-- was a census rather than a rule and it was wrong — being unwatched is a
-- property of the PATH and it outlives the write that caused it.  A blob's
-- @data\/\<shard>\/\<rest>\/@ is minted by one @createDirectoryIfMissing True@
-- and fsnotify never enters it, so the capture arrived and every LATER write to
-- that row — a state, a tag, an archive, a materialize commit — was written
-- correctly and never delivered.
--
-- Nudging a watched file costs nothing.  The queue is keyed by path, so the
-- nudge coalesces with the inotify events landing microseconds behind
-- it and the pair is one parse; 'nudge' drops what the walk declines, so a path
-- the store was never given cannot arrive this way either.  What it buys is a
-- rule with no list to keep in step with the routes.
--
-- Rides the success branch, so a drift or a refusal queues nothing — there is
-- nothing new to read — and the path is spelled ONCE, where a caller pairing
-- 'replaceSpans' with a nudge of its own could name two different files.
writeSpans :: WalkOptions -> Hub -> FilePath -> Text -> [(Span, Text)]
           -> IO (Either WriteFailure Text)
writeSpans opts hub path digest edits = do
  written <- replaceSpans path digest edits
  either (const (pure ())) (const (nudge opts hub path)) written
  pure written

-- | The ripe PATHS folded into HUB: one reseed when any of them is a config
-- file, one re-read each when none is.
--
-- The two do not mix.  A reseed re-walks the whole tree, so it already covers
-- every ordinary path that ripened beside it, and running both would parse
-- those files twice and stream them twice.  One window's worth of config edits
-- is likewise one reseed, which is what makes saving a directory of tag
-- configs cost what saving one costs.
settle :: WalkOptions -> FilePath -> Hub -> [FilePath] -> IO ()
settle opts dir hub paths
  | any configPath paths = reseed opts dir hub paths
  | otherwise            = mapM_ (reload hub) paths

-- | Is PATH one this watch reads, under OPTS?  What 'isWatchable' keeps, minus
-- what the walk declined to enter: a file the store was never given must not
-- arrive by way of an inotify event, or an org-glance mirror would appear in
-- the table the moment it was rewritten.
--
-- The config is the one exception, and a deliberate one: those files are never
-- rows, so no walk ever handed them over, and they are watched precisely
-- because a change to one changes the files that ARE rows.  'settle' is what
-- keeps the exception from meaning "a config file is a row after all".
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
  cfg <- stConfig <$> readTVarIO (hubStore hub)
  exists <- doesFileExist path
  outcome <- if exists then Just <$> loadFileWith cfg path else pure Nothing
  frames <- publish hub (maybe (dropFile path) (applyFile path) outcome)
  finished <- getMonotonicTime
  report path outcome frames (finished - started)

-- | DIR re-read from the top into HUB, because one of PATHS is a config file.
--
-- The whole load again: the config layers, the walk, every parse.  There is no
-- cheaper honest answer — a keyword the config has just gained is recognized in
-- files that have not changed and cannot be found without re-reading them — and
-- 'Glance.Web.Store.reseeded' then diffs the two stores so a client is told
-- what actually moved rather than being handed the tree again.
--
-- The fresh store is built OUTSIDE the publishing transaction, so the seconds
-- it takes are seconds the daemon keeps serving the store it already has.  What
-- that costs is a window: a file written during the walk is read by it or by
-- the event that follows, and either way the last write wins.
reseed :: WalkOptions -> FilePath -> Hub -> [FilePath] -> IO ()
reseed opts dir hub paths = do
  started <- getMonotonicTime
  fresh <- loadStoreWith opts dir
  frames <- publish hub (reseeded fresh)
  finished <- getMonotonicTime
  say [ "glance watch: " <> unwords (map show paths) <> " config reseed — "
          <> frameSummary frames <> " " <> millis (finished - started) ]

-- | One line per event, when there was anything to say.  The elapsed time is
-- the re-parse metric the persistence gate is decided on
-- (docs/plan-org-console-web.md), so it is on every line that did work.
report :: FilePath -> Maybe (Either LoadFailure [a]) -> [Frame] -> Double -> IO ()
report path outcome frames elapsed = unless (null note && null frames) $
  say [ "glance watch: " <> path <> " " <> summary <> " " <> millis elapsed ]
  where
    summary | null note = frameSummary frames
            | otherwise = note
    note = case outcome of
      Just (Left ReadFailed)   -> "unreadable — rows kept"
      Just (Left DecodeFailed) -> "not UTF-8 — rows kept"
      Just (Left ParseFailed)  -> "parse failed — rows kept"
      _loaded                  -> ""

-- | What FRAMES did, for the operator's log — one wording for the per-file line
-- and the reseed's.
--
-- A moved palette is the whole answer: it closes every socket, and the store
-- REPLACES a step's rows with it rather than sending both, so there is nothing
-- else in the list to report.  Otherwise each op is counted as itself rather
-- than by subtracting the upserts from the length, which would call any frame
-- that is neither one a delete.
frameSummary :: [Frame] -> String
frameSummary frames
  | ViewChanged `elem` frames = "keywords changed — clients reconnect"
  | otherwise = count [ () | UpsertRow _ <- frames ] <> " upsert, "
             <> count [ () | DeleteRow _ <- frames ] <> " delete"
  where count = show . length

-- | LINES to stdout, flushed.  Redirected stdout is block-buffered and every
-- caller here then blocks — in the drain loop, in warp, in a window — so
-- without the flush the log gets its lines when the run is over.
--
-- ONE spelling for every module of @glance-web@ that prints and then blocks —
-- 'Glance.Web' for the startup banner, 'Glance.Desktop' and
-- 'Glance.Desktop.Native' for theirs, this one for the event log.  Each of them
-- sits above this module, so there is no cycle to close.
say :: [String] -> IO ()
say ls = mapM_ putStrLn ls >> hFlush stdout

millis :: Double -> String
millis seconds = "(" <> show (round (seconds * 1000) :: Int) <> " ms)"
