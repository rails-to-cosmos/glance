-- | The file watch: inotify events in, store updates and frames out.
-- Debounce, config reseed and the nudge rule: AGENTS.hs.
module Glance.Web.Watch
  ( debounceDelay
  , drain
  , due
  , isWatchable
  , nudge
  , reload
  , say
  , settle
  , watched
  , watchOrgTree
  , writeSpans
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Concurrent.STM (atomically, modifyTVar', readTVar, readTVarIO, writeTVar)
import Control.Exception (IOException, try)
import Control.Monad (forever, unless, when)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Tuple (swap)
import GHC.Clock (getMonotonicTime)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory)
import System.IO (hFlush, stdout)

import qualified Data.Map.Strict as Map
import qualified System.FSNotify as FS

import Glance.Query ( LoadFailure (..), Span, TailCursor, WalkOptions (..)
                    , WriteFailure, blobPathIn, configPath, derivedPath
                    , documentPath, loadFileWith, replaceSpans, segmentEnd
                    , segmentIn, storeRootIn, tailFrom, tailedFile )
import Glance.Web.Git (autoSyncPoke)
import Glance.Web.Store ( CloseReason (ViewChanged), Frame (..)
                        , Hub (hubAutoSync, hubPending, hubStore)
                        , RowOp (..), Store (stConfig)
                        , applyFile, dropFile, loadStoreWith, publish, reseeded )

-- | How long a path must stay quiet before it is re-parsed, in seconds.
debounceDelay :: Double
debounceDelay = 0.1

tick :: Int
tick = 25000

-- | Watch DIR into HUB until killed, SEEN placing the WAL tail's cursor.  One
-- tree watch carries both channels: a document event is nudged, one of the WAL's
-- own files is tailed.
watchOrgTree :: WalkOptions -> FilePath -> Hub -> TailCursor -> IO ()
watchOrgTree opts dir hub seen =
  FS.withManager $ \mgr -> do
    wal <- newWal dir seen (nudge opts hub)
    let note event
          | tailedFile path  = tailWal wal
          | minted wal event = armWal mgr wal
          | otherwise        = nudge opts hub path
          where path = FS.eventPath event
    _stop <- FS.watchTree mgr dir (heard opts wal) note
    forever (threadDelay tick >> drain opts debounceDelay dir hub)

-- | The events this watch takes: the documents it reads, the WAL files it tails,
-- and the minting of the directory that WAL lives in.
heard :: WalkOptions -> Wal -> FS.Event -> Bool
heard opts wal event =
  watched opts path || tailedFile path || minted wal event
  where path = FS.eventPath event

-- | One turn of the drain loop: HUB's ripe paths taken out IN the transaction.
drain :: WalkOptions -> Double -> FilePath -> Hub -> IO ()
drain opts delay dir hub = do
  now <- getMonotonicTime
  paths <- atomically $ do
    (ripe, rest) <- due delay now <$> readTVar (hubPending hub)
    unless (null ripe) (writeTVar (hubPending hub) rest)
    pure ripe
  settle opts dir hub paths

-- | Queue PATH for re-reading.  THE ONE DOOR into the queue, filtered by 'watched'.
nudge :: WalkOptions -> Hub -> FilePath -> IO ()
nudge opts hub path = when (watched opts path) $ do
  now <- getMonotonicTime
  atomically (modifyTVar' (hubPending hub) (Map.insert path now))

-- | EDITS spliced into PATH under DIGEST, and PATH nudged on the SUCCESS branch.
writeSpans :: WalkOptions -> Hub -> FilePath -> Text -> [(Span, Text)]
           -> IO (Either WriteFailure Text)
writeSpans opts hub path digest edits = do
  written <- replaceSpans path digest edits
  either (const (pure ())) (const landed) written
  pure written
  where
    -- On the success branch only: nudge the re-read, then poke Model B (a no-op
    -- unless auto-sync is on and armed).  Off the request path; never blocks the write.
    landed = nudge opts hub path >> (readTVarIO (hubAutoSync hub) >>= mapM_ autoSyncPoke)

-- | The ripe PATHS folded into HUB; a config file among them makes it a reseed.
settle :: WalkOptions -> FilePath -> Hub -> [FilePath] -> IO ()
settle opts dir hub paths
  | any configPath paths = reseed opts dir hub paths
  | otherwise            = mapM_ (reload hub) paths

-- | Is PATH one this watch reads, under OPTS?  A config file is the exception.
watched :: WalkOptions -> FilePath -> Bool
watched opts path = isWatchable path
                 && (woIncludeDerived opts || not (derivedPath path))

isWatchable :: FilePath -> Bool
isWatchable = documentPath

-- | PENDING split at DELAY seconds before NOW, on a monotonic clock.
due :: Double -> Double -> Map FilePath Double -> ([FilePath], Map FilePath Double)
due delay now pending = (Map.keys ripe, rest)
  where (ripe, rest) = Map.partition ((>= delay) . (now -)) pending

-- | Re-read PATH into HUB; a path that no longer exists is a deletion.
reload :: Hub -> FilePath -> IO ()
reload hub path = do
  started <- getMonotonicTime
  cfg <- stConfig <$> readTVarIO (hubStore hub)
  exists <- doesFileExist path
  outcome <- if exists then Just <$> loadFileWith cfg path else pure Nothing
  frames <- publish hub (maybe (dropFile path) (applyFile path) outcome)
  finished <- getMonotonicTime
  report path outcome frames (finished - started)

reseed :: WalkOptions -> FilePath -> Hub -> [FilePath] -> IO ()
reseed opts dir hub paths = do
  started <- getMonotonicTime
  fresh <- loadStoreWith opts dir
  frames <- publish hub (reseeded fresh)
  finished <- getMonotonicTime
  say [ "glance watch: " <> unwords (map show paths) <> " config reseed — "
          <> frameSummary frames <> " " <> millis (finished - started) ]

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

frameSummary :: [Frame] -> String
frameSummary frames
  | Close ViewChanged `elem` frames = "keywords changed — clients reconnect"
  | otherwise = count [ () | Op (UpsertRow _) <- frames ] <> " upsert, "
             <> count [ () | Op (DeleteRow _) <- frames ] <> " delete"
  where count = show . length

-- | LINES to stdout, FLUSHED: every caller here then blocks on a buffered handle.
say :: [String] -> IO ()
say ls = mapM_ putStrLn ls >> hFlush stdout

millis :: Double -> String
millis seconds = "(" <> show (round (seconds * 1000) :: Int) <> " ms)"

-- ORG-GLANCE'S WAL, TAILED: what the tree watch cannot hear, and why the peer's
-- own write-ahead index is the channel that can — AGENTS.hs's store notes.

-- | A tail over one served root's WAL.
data Wal = Wal
  { walSeg   :: !FilePath             -- ^ the open segment, derived once.
  , walStore :: !FilePath             -- ^ the @.org-glance@ a record's blob hangs off.
  , walSeen  :: !(MVar TailCursor)    -- ^ how far into the segment the tail has read.
  , walNudge :: !(FilePath -> IO ())  -- ^ the one queue door.
  }

-- | A tail over ROOT's WAL, its cursor at SEEN and each record's blob handed to NUDGE'.
newWal :: FilePath -> TailCursor -> (FilePath -> IO ()) -> IO Wal
newWal root seen nudge' =
  (\cursor -> Wal (segmentIn root) (storeRootIn root) cursor nudge') <$> newMVar seen

-- | Is EVENT the minting of a directory WAL's segment hangs under — the store,
-- or the @meta@ in it?  A tree watch arms a new directory without traversing
-- into it, so a store org-glance mints under a running daemon is reached here
-- and nowhere else.
minted :: Wal -> FS.Event -> Bool
minted wal (FS.Added path _ FS.IsDirectory) = path `elem` [takeDirectory meta, meta]
  where meta = walMeta wal
minted _ _notADirectoryArriving = False

-- | WAL's @meta@ watched, its cursor placed, then read.  hinotify merges a
-- second watch on a directory already armed, and throws where the directory it
-- is handed went away again.
armWal :: FS.WatchManager -> Wal -> IO ()
armWal mgr wal =
  try (FS.watchDir mgr (walMeta wal) (tailedFile . FS.eventPath) (const (tailWal wal)))
    >>= either gone (const (place >> tailWal wal))
  where
    -- An unplaced cursor seeds at the segment's end; a placed one stands, so a
    -- second arming rewinds nothing.
    place = modifyMVar_ (walSeen wal) (maybe (segmentEnd (walSeg wal)) (pure . Just))
    gone :: IOException -> IO ()
    gone _ = pure ()

-- | The records appended past WAL's cursor, each one's blob NUDGED — a
-- tombstone's too, the reload finding the blob gone and dropping the row.  No
-- other field of a record is read: the blob's parse is the row's truth.  The
-- cursor moves under the MVar, so an arming and a callback cannot interleave.
tailWal :: Wal -> IO ()
tailWal wal = modifyMVar (walSeen wal) step >>= mapM_ (walNudge wal . blobPathIn (walStore wal))
  where step seen = swap <$> tailFrom (walSeg wal) seen

walMeta :: Wal -> FilePath
walMeta = takeDirectory . walSeg
