-- | The in-memory projection of the org tree, and the sockets watching it.
--
-- S3 parsed the whole directory per request; at 14.6 s over 6308 files that is
-- a page load you wait for.  S5 parses it once at startup, keeps the result
-- here, and re-parses one file per edit.  Org files stay the single source of
-- truth — this is a projection, thrown away with the process, and every row in
-- it came out of a file the watcher can re-read.
--
-- The store is keyed by path so that 'Data.Map.Strict.elems' is walk order:
-- 'Glance.Query.loadDir' sorts the paths it found and a 'Data.Map.Strict.Map'
-- keeps them sorted, so 'storeResult' reproduces the load it stands in for row
-- for row.  Each entry keeps its file's rows and how its last load went; a
-- parse failure keeps the rows and records the failure, because 'orgParse' is
-- all-or-nothing and a half-written file that fails to parse says nothing
-- about the headlines that were in it a moment ago.
--
-- Frames are SCHEMA.md's streaming ops.  'ViewChanged' is the one thing that
-- is not: the columns carry the TODO-keyword palette, SCHEMA.md has no op for
-- a column change, and inventing one would put this producer outside the
-- contract.  The socket closes instead and the client re-fetches the view it
-- already knows how to mount.
module Glance.Web.Store
  ( -- * The store
    Store (..)
  , FileEntry (..)
  , emptyStore
  , loadStore
  , storeHeadline
  , storeRecords
  , storeResult
  , storeKeywords
  , applyFile
  , dropFile
    -- * Frames
  , Frame (..)
  , frameJSON
  , frameText
  , bootstrapFrame
    -- * The hub
  , Hub (hubStore)
  , Client
  , clientCapacity
  , newHub
  , subscribe
  , unsubscribe
  , nextFrame
  , publish
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue, newTBQueue, readTBQueue, writeTBQueue)
import Control.Monad ((<=<))
import Data.Aeson (Value, encode, object, (.=))
import Data.List (foldl', nub)
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Glance.Query ( HeadlineRecord (hrId, hrKeywords), LoadFailure (..)
                    , QueryResult (..), TodoKeywords, loadDirFiles
                    , mergeKeywords, rowJSON )

-- The store

-- | One directory, parsed.  The dir-error count is the startup walk's: the
-- watcher re-reads files rather than re-walking, so a directory that becomes
-- unlistable later is not noticed until a restart.
data Store = Store
  { stFiles   :: !(Map FilePath FileEntry)  -- ^ path-keyed, hence walk-ordered.
  , stIds     :: !(Map Text Int)            -- ^ row id → how many files carry it.
  , stDirErrs :: !Int                       -- ^ directories the startup walk could not list.
  , stGen     :: !Int                       -- ^ update counter; see 'guarded'.
  }

-- | What the store holds for one file: the rows it contributes, and how its
-- last load went.  The two are independent — a file that stopped parsing keeps
-- the rows of its last good parse and is counted as a failure at the same time.
data FileEntry = FileEntry
  { feRecords :: ![HeadlineRecord]      -- ^ rows in file order, from the last good parse.
  , feFailure :: !(Maybe LoadFailure)   -- ^ how the last load ended.
  }

emptyStore :: Store
emptyStore = Store Map.empty Map.empty 0 0

-- | DIR walked and parsed into a store: the same files 'Glance.Query.loadDir'
-- visits, with the per-file breakdown kept instead of folded into counts.
loadStore :: FilePath -> IO Store
loadStore dir = do
  (files, dirErrs) <- loadDirFiles dir
  pure $! foldl' seed (emptyStore { stDirErrs = dirErrs }) files
  where seed st (path, outcome) = fst (putFile path outcome st)

-- | Every row the store holds, in walk order.
storeRecords :: Store -> [HeadlineRecord]
storeRecords = concatMap feRecords . Map.elems . stFiles

-- | The store as the load result it stands in for: same rows in the same
-- order, same counts.  @GET \/headlines@ renders this, so the served document
-- is the one a fresh 'Glance.Query.loadDir' would have produced.
storeResult :: Store -> QueryResult
storeResult st = QueryResult
  { qrRecords        = storeRecords st
  , qrFiles          = length entries
  , qrParseFailures  = failures ParseFailed
  , qrDecodeFailures = failures DecodeFailed
  , qrReadFailures   = stDirErrs st + failures ReadFailed
  }
  where entries    = Map.elems (stFiles st)
        failures f = length (filter ((== Just f) . feFailure) entries)

-- | The record ST holds under ID, or 'Nothing'.  What @\/headline@ materializes
-- from: the row the table shows, with the subtree extent and the digest of the
-- text that extent was measured in, which is what makes a write back to it
-- drift-checkable.
--
-- A scan of the store rather than an index: 'stIds' counts ids to decide
-- deletions and holds no records, and an index that held them would be a
-- second structure to keep in step with 'stFiles' on every reload.  The scan
-- is ~2.4 ms over the 13359-row ~/sync store, which is most of a materialize
-- request and none of a user's attention; it is the lever if @\/headline@ ever
-- lands in a loop.  Two files declaring one @ORG_GLANCE_ID@ share a row and the
-- later one in walk order wins, which is how 'rowsById' resolves the same
-- collision on the wire.
storeHeadline :: Text -> Store -> Maybe HeadlineRecord
storeHeadline rid = foldl' pick Nothing . storeRecords
  where pick found r | hrId r == rid = Just r
                     | otherwise     = found

-- | The palette the store's columns carry.  One record per file is enough:
-- every row of a file shares its keyword sets and 'mergeKeywords' deduplicates,
-- so this is the same answer as merging all of them, at one merge per file.
storeKeywords :: Store -> TodoKeywords
storeKeywords = mergeKeywords . mapMaybe (fmap hrKeywords . listToMaybe . feRecords)
              . Map.elems . stFiles

-- Updates

-- | PATH re-loaded into the store, and the frames the change implies.  A
-- failure keeps the file's rows and streams nothing — nothing about them is
-- known to have changed.
applyFile :: FilePath -> Either LoadFailure [HeadlineRecord] -> Store -> (Store, [Frame])
applyFile path outcome = guarded path (putFile path outcome)

-- | PATH gone from the store: every row only it carried goes with it.
dropFile :: FilePath -> Store -> (Store, [Frame])
dropFile path = guarded path (removeFile path)

-- | STEP, with the columns watched and the generation moved.  The palette can
-- only move when the file STEP touched changes what it declares, every other
-- file's contribution being untouched and the merge being a function of them;
-- that check is a lookup, and the full merge runs only when it fires.
--
-- The generation is what @\/headlines@ spells as an @ETag@, so it has to move
-- whenever a response would: when rows changed (there are frames, 'ViewChanged'
-- among them) or when the file's load outcome did, which is a stats header
-- moving with no row to show for it.  A watch event over a file nothing wrote
-- leaves it alone, so an idle tree revalidates to 304 forever.
guarded :: FilePath -> (Store -> (Store, [Frame])) -> Store -> (Store, [Frame])
guarded path step st = (if moved then next { stGen = stGen next + 1 } else next, out)
  where
    (next, frames) = step st
    -- '&&' short-circuits, so the full merge runs only when the touched file's
    -- own declaration moved.
    palette  = declared st /= declared next && storeKeywords st /= storeKeywords next
    out      = if palette then [ViewChanged] else frames
    moved    = not (null out) || outcome st /= outcome next
    declared = fmap hrKeywords . (listToMaybe . feRecords <=< Map.lookup path) . stFiles
    outcome  = fmap feFailure . Map.lookup path . stFiles

-- | PATH's outcome written into the store.  New rows and changed ones become
-- upserts, in file order; rows no file carries any more become deletes.
-- Upserts lead, so a client applying the batch never shows fewer rows than the
-- file has.
putFile :: FilePath -> Either LoadFailure [HeadlineRecord] -> Store -> (Store, [Frame])
putFile path outcome st = case outcome of
  Left failure -> (st { stFiles = Map.insert path (FileEntry old (Just failure)) files }, [])
  Right new    -> (next new, frames new)
  where
    files   = stFiles st
    old     = maybe [] feRecords (Map.lookup path files)
    oldRows = rowsById old
    next new = st { stFiles = Map.insert path (FileEntry new Nothing) files
                  , stIds   = reindex (idsOf old) (idsOf new) (stIds st) }
    frames new = upserts <> gone (idsOf old) (idsOf new) (next new)
      where rows    = rowsById new
            upserts = [ UpsertRow row
                      | i <- nub (map hrId new)
                      , Just row <- [Map.lookup i rows]
                      , Map.lookup i oldRows /= Just row ]

-- | PATH's entry removed.  A file the store never held is not an error: the
-- watcher reports every deletion it sees, including of files the walk skipped.
removeFile :: FilePath -> Store -> (Store, [Frame])
removeFile path st = case Map.lookup path (stFiles st) of
  Nothing    -> (st, [])
  Just entry -> (next, gone ids Set.empty next)
    where ids  = idsOf (feRecords entry)
          next = st { stFiles = Map.delete path (stFiles st)
                    , stIds   = reindex ids Set.empty (stIds st) }

-- | Delete frames for the ids in OLD that NEW dropped and no file in ST still
-- carries.  Two files declaring one @ORG_GLANCE_ID@ share a row, and the
-- second of them to lose it is the one that deletes it.
gone :: Set Text -> Set Text -> Store -> [Frame]
gone old new st =
  [ DeleteRow i | i <- Set.toList (Set.difference old new), Map.notMember i (stIds st) ]

-- | The id index with OLD's ids released and NEW's claimed.
reindex :: Set Text -> Set Text -> Map Text Int -> Map Text Int
reindex old new = flip (Set.foldl' claim) new . flip (Set.foldl' release) old
  where release ids i = Map.update (\n -> if n <= 1 then Nothing else Just (n - 1)) i ids
        claim   ids i = Map.insertWith (+) i 1 ids

-- | RECORDS keyed by row id, last one winning — which is how a renderer keying
-- updates off @id@ resolves a duplicate too.
rowsById :: [HeadlineRecord] -> Map Text Value
rowsById records = Map.fromList [ (hrId r, rowJSON r) | r <- records ]

idsOf :: [HeadlineRecord] -> Set Text
idsOf = Set.fromList . map hrId

-- Frames

-- | What a live client receives.  The first three are SCHEMA.md's streaming
-- ops; 'ViewChanged' is a close, since a column change has no op.
data Frame
  = SetRows ![Value]   -- ^ every row, as sent on connect.
  | UpsertRow !Value   -- ^ one row, added or replaced by @id@.
  | DeleteRow !Text    -- ^ one row's @id@, dropped.
  | ViewChanged        -- ^ the columns moved; reconnect and re-fetch the view.
  deriving (Eq, Show)

-- | F as the wire object, or 'Nothing' for 'ViewChanged' — which travels as a
-- close reason rather than as a message.
frameJSON :: Frame -> Maybe Value
frameJSON frame = case frame of
  SetRows rows -> Just (object [ "op" .= ("set-rows" :: Text),   "rows" .= rows ])
  UpsertRow r  -> Just (object [ "op" .= ("upsert-row" :: Text), "row"  .= r ])
  DeleteRow i  -> Just (object [ "op" .= ("delete-row" :: Text), "id"   .= i ])
  ViewChanged  -> Nothing

-- | F encoded, ready for the socket.
frameText :: Frame -> Maybe BL.ByteString
frameText = fmap encode . frameJSON

-- | The frame a socket opens with: the whole store as @set-rows@.  A client
-- fetches @\/headlines@ for the columns and the sort, then applies this, so a
-- row that changed between the fetch and the socket cannot be missed — the
-- snapshot is taken in the transaction that subscribes.
bootstrapFrame :: Store -> Frame
bootstrapFrame = SetRows . map rowJSON . storeRecords

-- The hub

-- | The live store and its sockets.
data Hub = Hub
  { hubStore   :: !(TVar Store)
  , hubClients :: !(TVar (Map Int Client))
  , hubNextId  :: !(TVar Int)
  }

-- | One socket's mailbox.  Bounded on purpose: the watcher hands frames to
-- every client from the transaction that updates the store, and a browser that
-- has stopped reading must not be able to hold that transaction up.  A full
-- mailbox drops its client, the socket closes, and the client resyncs when it
-- reconnects — losing a slow reader's frames is recoverable, stalling the
-- watcher is not.
data Client = Client
  { clQueue   :: !(TBQueue Frame)
  , clDropped :: !(TVar Bool)
  }

-- | How many frames a client may fall behind before it is dropped.  One edit
-- is a handful of frames, so this is many seconds of lag on a live view and
-- one bootstrap on a stalled one.
clientCapacity :: Natural
clientCapacity = 256

newHub :: Store -> IO Hub
newHub st = Hub <$> newTVarIO st <*> newTVarIO Map.empty <*> newTVarIO 0

-- | Register a client and take its bootstrap snapshot in one transaction, so
-- no update can land between the two.  Yields the registration id
-- 'unsubscribe' wants.
subscribe :: Hub -> STM (Int, Client, Frame)
subscribe hub = do
  cid <- readTVar (hubNextId hub)
  writeTVar (hubNextId hub) (cid + 1)
  client <- Client <$> newTBQueue clientCapacity <*> newTVar False
  modifyTVar' (hubClients hub) (Map.insert cid client)
  boot <- bootstrapFrame <$> readTVar (hubStore hub)
  pure (cid, client, boot)

unsubscribe :: Hub -> Int -> IO ()
unsubscribe hub cid = atomically (modifyTVar' (hubClients hub) (Map.delete cid))

-- | C's next frame, or 'Nothing' once it has been dropped.  The dropped flag
-- is read first and inside the transaction, so a client dropped while blocked
-- here wakes and stops rather than draining a mailbox nobody is reading.
nextFrame :: Client -> STM (Maybe Frame)
nextFrame c = do
  dropped <- readTVar (clDropped c)
  if dropped then pure Nothing else Just <$> readTBQueue (clQueue c)

-- | Apply STEP to the store and post its frames to every client, in one
-- transaction.  A client that cannot take them is dropped and the transaction
-- goes through regardless.
-- Returns the frames, for the caller's log.
publish :: Hub -> (Store -> (Store, [Frame])) -> IO [Frame]
publish hub step = atomically $ do
  (st, frames) <- step <$> readTVar (hubStore hub)
  writeTVar (hubStore hub) st
  clients <- readTVar (hubClients hub)
  slow <- mapM (post frames) (Map.toList clients)
  mapM_ (cut clients) (concat slow)
  pure frames
  where
    post frames (cid, c) = do
      room <- writeAll c frames
      pure [ cid | not room ]
    -- The full check is what keeps this from blocking: 'writeTBQueue' on a
    -- full mailbox would retry the whole transaction, which is the watcher
    -- waiting on a browser.
    writeAll _ []       = pure True
    writeAll c (f : fs) = do
      full <- isFullTBQueue (clQueue c)
      if full then pure False else writeTBQueue (clQueue c) f >> writeAll c fs
    cut clients cid = do
      mapM_ (\c -> writeTVar (clDropped c) True) (Map.lookup cid clients)
      modifyTVar' (hubClients hub) (Map.delete cid)
