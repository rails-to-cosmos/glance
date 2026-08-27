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
import Glance.Web.Store ( CloseReason (ViewChanged), Frame (..), Hub (hubPending, hubStore)
                        , RowOp (..), Store (stConfig)
                        , applyFile, dropFile, loadStoreWith, publish, reseeded )

-- | How long a path must stay quiet before it is re-parsed, in seconds.
debounceDelay :: Double
debounceDelay = 0.1

tick :: Int
tick = 25000

watchOrgTree :: WalkOptions -> FilePath -> Hub -> IO ()
watchOrgTree opts dir hub =
  FS.withManager $ \mgr -> do
    _stop <- FS.watchTree mgr dir (watched opts . FS.eventPath) note
    forever (threadDelay tick >> drain opts debounceDelay dir hub)
  where note = nudge opts hub . FS.eventPath

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
  either (const (pure ())) (const (nudge opts hub path)) written
  pure written

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
