-- | The in-memory projection of the org tree, and the sockets watching it.
--
-- Keyed by PATH so 'Data.Map.Strict.elems' is walk order.  A parse failure
-- KEEPS the file's rows: 'orgParse' is all-or-nothing.
module Glance.Web.Store
  ( -- * The store
    Store (..)
  , emptyStore
  , loadStore
  , loadStoreWith
  , storeDocument
  , recordsUnder
  , headlinesIn
  , storeRecords
  , storeResult
  , storeKeywords
  , storeTags
  , layersFor
  , applyFile
  , dropFile
  , reseeded
    -- * Frames
  , Frame (..)
  , RowOp (..)
  , CloseReason (..)
  , closeReason
  , frameJSON
  , frameText
  , bootstrapFrame
    -- * The hub
  , Hub (hubStore, hubLoad, hubPending)
  , LoadState (..)
  , Client
  , clientCapacity
  , newHub
  , newLoadingHub
  , finishLoading
  , subscribe
  , unsubscribe
  , nextFrame
  , publish
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVar, newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue, newTBQueue, readTBQueue, writeTBQueue)
import Control.Monad (filterM, (<=<))
import Data.Aeson (Value, encode, object, (.=))
import Data.Either (partitionEithers)
import Data.List (foldl', nub)
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Glance.Query ( ConfigLayerFile, ConfigLayers (clPrint)
                    , HeadlineRecord (hrDigest, hrDoc, hrId, hrKeywords, hrTags)
                    , LoadFailure (..)
                    , QueryResult (..), TodoKeywords, WalkOptions, configDirsIn
                    , defaultWalk
                    , digestOfText, loadDirWithConfig, mergeKeywords, noConfig
                    , noKeywords, readConfigLayers, recognizedKeywords, resolveIds
                    , rowJSON, tagsOfCell )


data Store = Store
  { stFiles   :: !(Map FilePath FileEntry)  -- ^ path-keyed, hence walk-ordered.
  , stTags    :: !(Map Text Int)            -- ^ org tag → how many files carry it; see 'storeTags'.
  , stDirErrs :: !Int                       -- ^ directories the startup walk could not list.
  , stGen     :: !Int                       -- ^ update counter; see 'guarded'.
  , stPrint   :: !Text                      -- ^ which tree this is; see 'fingerprintOf'.
  , stConfig  :: !ConfigLayers              -- ^ the keyword config every file here was parsed under.
  }

data FileEntry = FileEntry
  { feRecords :: ![HeadlineRecord]      -- ^ rows in file order, from the last good parse.
  , feFailure :: !(Maybe LoadFailure)   -- ^ how the last load ended.
  }

emptyStore :: Store
emptyStore = Store Map.empty Map.empty 0 0 "" noConfig

loadStore :: FilePath -> IO Store
loadStore = loadStoreWith defaultWalk

-- | 'loadStore' over the tree OPTS asks for.  The config is read first and
-- kept, so a row arriving by inotify is the row the walk would have produced.
loadStoreWith :: WalkOptions -> FilePath -> IO Store
loadStoreWith opts dir = do
  (cfg, files, dirErrs) <- loadDirWithConfig opts dir
  let loaded = foldl' seed (emptyStore { stDirErrs = dirErrs, stConfig = cfg }) files
  pure $! loaded { stPrint = fingerprintOf loaded }
  where seed st (path, outcome) = putFile path outcome st

-- | What tree ST is: one digest over the config and every file's path and load
-- digest.  The half of the @ETag@ that survives a restart; 'stGen' starts at 0.
fingerprintOf :: Store -> Text
fingerprintOf st =
  digestOfText (T.unlines (("config\t" <> clPrint (stConfig st))
                            : [ T.pack path <> "\t" <> stamp entry
                              | (path, entry) <- Map.toAscList (stFiles st) ]))
  where stamp = maybe "" hrDigest . listToMaybe . feRecords

storeRows :: Store -> [HeadlineRecord]
storeRows = concatMap feRecords . Map.elems . stFiles

storeRecords :: Store -> [HeadlineRecord]
storeRecords = fst . resolveIds . storeRows

storeResult :: Store -> QueryResult
storeResult st = QueryResult
  { qrRecords        = rows
  , qrFiles          = length entries
  , qrParseFailures  = failures ParseFailed
  , qrDecodeFailures = failures DecodeFailed
  , qrReadFailures   = stDirErrs st + failures ReadFailed
  , qrIdCollisions   = clashes
  }
  where entries          = Map.elems (stFiles st)
        (rows, clashes)  = resolveIds (storeRows st)
        failures f       = length (filter ((== Just f) . feFailure) entries)

-- | IDS looked up over rows ALREADY RESOLVED.  It takes the rows rather than
-- the store, so every route resolves once at its own door ('storeRecords').
headlinesIn :: [HeadlineRecord] -> [Text] -> ([HeadlineRecord], [Text])
headlinesIn resolved ids =
  partitionEithers [ maybe (Right rid) Left (Map.lookup rid held) | rid <- ids ]
  where wanted = Set.fromList ids
        held   = Map.fromList [ (hrId r, r) | r <- resolved
                                            , Set.member (hrId r) wanted ]

storeDocument :: FilePath -> Store -> Maybe (Text, Text)
storeDocument path st = (\r -> (hrDoc r, hrDigest r)) <$> listToMaybe (recordsUnder path st)

-- | Every org tag the store's rows carry, sorted; counted per FILE, not row.
storeTags :: Store -> [Text]
storeTags = Map.keys . stTags

layersFor :: FilePath -> Store -> IO [ConfigLayerFile]
layersFor root st = readConfigLayers (configDirsIn root (stConfig st))

-- | The store's palette.  The config LEADS, which is what pins the order.
storeKeywords :: Store -> TodoKeywords
storeKeywords st = mergeKeywords (recognizedKeywords (stConfig st) noKeywords : perFile)
  where perFile = mapMaybe (fmap hrKeywords . listToMaybe . feRecords)
                           (Map.elems (stFiles st))


applyFile :: FilePath -> Either LoadFailure [HeadlineRecord] -> Store -> (Store, [Frame])
applyFile path outcome = guarded path (streamed path (putFile path outcome))

dropFile :: FilePath -> Store -> (Store, [Frame])
dropFile path = guarded path (streamed path (removeFile path))

-- | ST replaced by FRESH, and the frames that costs — the ordinary ops over
-- every id on both sides.  The generation is ST's: FRESH itself carries zero.
reseeded :: Store -> Store -> (Store, [Frame])
reseeded fresh st = installed st fresh (outcomes st /= outcomes fresh) out
  where
    before   = rowsById st
    after    = rowsById fresh
    everyId  = Set.toAscList (Map.keysSet before <> Map.keysSet after)
    out      = if storeKeywords st /= storeKeywords fresh then [Close ViewChanged]
                                                          else rowFrames everyId before after
    outcomes = Map.map feFailure . stFiles

rowsById :: Store -> Map Text Value
rowsById st = Map.fromList [ (hrId r, rowJSON r) | r <- storeRecords st ]

-- | The ops IDS owe between two served views.  UPSERTS LEAD, so a client
-- applying the batch in order never shows fewer rows than the store has.
rowFrames :: [Text] -> Map Text Value -> Map Text Value -> [Frame]
rowFrames ids before after =
  [ Op (UpsertRow row) | i <- ids, Just row <- [Map.lookup i after]
                            , Map.lookup i before /= Just row ]
    <> [ Op (DeleteRow i) | i <- ids, Map.notMember i after ]

-- | NEXT installed over ST, the generation stepped where the view moved.  ONE
-- rule for both writers, and the counter is ST's so 'reseeded' carries it over.
installed :: Store -> Store -> Bool -> [Frame] -> (Store, [Frame])
installed st next outcomes out = (next { stGen = stGen st + bump }, out)
  where bump = if null out && not outcomes then 0 else 1

guarded :: FilePath -> (Store -> (Store, [Frame])) -> Store -> (Store, [Frame])
guarded path step st = installed st next (outcome st /= outcome next) out
  where
    (next, frames) = step st
    palette  = declared st /= declared next && storeKeywords st /= storeKeywords next
    out      = if palette then [Close ViewChanged] else frames
    declared = fmap hrKeywords . (listToMaybe . feRecords <=< Map.lookup path) . stFiles
    outcome  = fmap feFailure . Map.lookup path . stFiles

-- | UPDATE applied, and the frames the ids under PATH owe.  Both sides are
-- read through the store's id resolution, so a streamed row is a served row.
streamed :: FilePath -> (Store -> Store) -> Store -> (Store, [Frame])
streamed path update st = (next, rowFrames touched before after)
  where
    next     = update st
    touched  = nub (idsUnder next <> idsUnder st)
    idsUnder = map hrId . recordsUnder path
    before   = resolvedRows touched st
    after    = resolvedRows touched next

resolvedRows :: [Text] -> Store -> Map Text Value
resolvedRows ids st = Map.fromList [ (hrId r, rowJSON r) | r <- fst (resolveIds carrying) ]
  where wanted   = Set.fromList ids
        carrying = [ r | r <- storeRows st, Set.member (hrId r) wanted ]

recordsUnder :: FilePath -> Store -> [HeadlineRecord]
recordsUnder path = maybe [] feRecords . Map.lookup path . stFiles

putFile :: FilePath -> Either LoadFailure [HeadlineRecord] -> Store -> Store
putFile path outcome st = case outcome of
  Left failure -> st { stFiles = Map.insert path (FileEntry old (Just failure)) files }
  Right new    -> st { stFiles = Map.insert path (FileEntry new Nothing) files
                     , stTags  = stepIndex tagsOf old new (stTags st) }
  where files = stFiles st
        old   = recordsUnder path st

removeFile :: FilePath -> Store -> Store
removeFile path st = st { stFiles = Map.delete path (stFiles st)
                        , stTags  = stepIndex tagsOf (recordsUnder path st) [] (stTags st) }

stepIndex :: Ord k => ([HeadlineRecord] -> Set k)
          -> [HeadlineRecord] -> [HeadlineRecord] -> Map k Int -> Map k Int
stepIndex proj old new ix = Set.foldl' claim (Set.foldl' release ix (proj old)) (proj new)
  where release m k = Map.update (\n -> if n <= 1 then Nothing else Just (n - 1)) k m
        claim   m k = Map.insertWith (+) k 1 m

tagsOf :: [HeadlineRecord] -> Set Text
tagsOf = Set.fromList . concatMap (tagsOfCell . hrTags)


-- | What a live client receives.  A ROW OP travels as a message; a CLOSE
-- travels as a reason, and 'guarded' REPLACES a step's ops with one — rows
-- built against a view that has already moved are rows a client draws wrong.
--
-- TWO CONSTRUCTORS rather than a row op that happens to encode as no JSON: the
-- distinction was carried by 'frameJSON' answering 'Nothing', which is a
-- convention a reader has to know rather than a thing the compiler checks.
data Frame
  = Op !RowOp             -- ^ a row op, as JSON.
  | Close !CloseReason    -- ^ the socket is to be closed, and why.
  deriving (Eq, Show)

-- | The row operations SCHEMA.md's socket carries, and the whole of them.
data RowOp
  = SetRows ![Value]   -- ^ every row, as sent on connect.
  | UpsertRow !Value   -- ^ one row, added or replaced by @id@.
  | DeleteRow !Text    -- ^ one row's @id@, dropped.
  deriving (Eq, Show)

-- | THE WHOLE VOCABULARY OF A SERVER-INITIATED CLOSE.  'Bounded' and 'Enum' so
-- the list is generated where two hand-typed strings used to sit, and a third
-- reason joins by being a constructor.
data CloseReason
  = ViewChanged   -- ^ the columns moved; reconnect and re-fetch the view.
  | Resync        -- ^ the mailbox filled; the backlog is gone, so re-ask.
  deriving (Eq, Show, Enum, Bounded)

-- | The word a close reason travels as.
closeReason :: CloseReason -> Text
closeReason ViewChanged = "view-changed"
closeReason Resync      = "resync"

frameJSON :: Frame -> Maybe Value
frameJSON frame = case frame of
  Op (SetRows rows) -> Just (object [ "op" .= ("set-rows" :: Text),   "rows" .= rows ])
  Op (UpsertRow r)  -> Just (object [ "op" .= ("upsert-row" :: Text), "row"  .= r ])
  Op (DeleteRow i)  -> Just (object [ "op" .= ("delete-row" :: Text), "id"   .= i ])
  Close _           -> Nothing

frameText :: Frame -> Maybe BL.ByteString
frameText = fmap encode . frameJSON

-- | The frame a socket opens with, snapshotted in the subscribing transaction.
bootstrapFrame :: Store -> Frame
bootstrapFrame = Op . SetRows . map rowJSON . storeRecords


-- | The live store, its sockets, and the paths waiting to be re-read.
-- 'hubPending' is filled by inotify AND by every write route on its way out.
data Hub = Hub
  { hubStore   :: !(TVar Store)
  , hubClients :: !(TVar (Map Int Client))
  , hubNextId  :: !(TVar Int)
  , hubLoad    :: !(TVar LoadState)
  , hubPending :: !(TVar (Map FilePath Double))  -- ^ path → when it was last touched, monotonic.
  }

data LoadState
  = Loading !Double  -- ^ walking and parsing since this monotonic second.
  | Loaded           -- ^ the store is the directory.
  deriving (Eq, Show)

-- | One socket's mailbox, bounded on purpose: a browser that stopped reading
-- must not hold the watcher's transaction up.  A full one closes the socket.
data Client = Client
  { clQueue   :: !(TBQueue Frame)
  , clDropped :: !(TVar Bool)
  }

clientCapacity :: Natural
clientCapacity = 1024

newHub :: Store -> IO Hub
newHub st = hubOver st Loaded

newLoadingHub :: Double -> IO Hub
newLoadingHub started = hubOver emptyStore (Loading started)

hubOver :: Store -> LoadState -> IO Hub
hubOver st load =
  Hub <$> newTVarIO st <*> newTVarIO Map.empty <*> newTVarIO 0 <*> newTVarIO load
      <*> newTVarIO Map.empty

-- | Install ST and open the store routes, in ONE transaction.
finishLoading :: Hub -> Store -> IO ()
finishLoading hub st = atomically $ do
  writeTVar (hubStore hub) st
  writeTVar (hubLoad hub) Loaded

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

-- | C's next frame, or 'Nothing' once dropped; the flag is read first.
nextFrame :: Client -> STM (Maybe Frame)
nextFrame c = do
  dropped <- readTVar (clDropped c)
  if dropped then pure Nothing else Just <$> readTBQueue (clQueue c)

-- | Apply STEP and post its frames to every client, in one transaction.  A
-- client that cannot take them loses its backlog and its registration.
publish :: Hub -> (Store -> (Store, [Frame])) -> IO [Frame]
publish hub step = atomically $ do
  (st, frames) <- step <$> readTVar (hubStore hub)
  writeTVar (hubStore hub) st
  clients <- readTVar (hubClients hub)
  slow <- filterM (fmap not . flip writeAll frames . snd) (Map.toList clients)
  mapM_ cut slow
  pure frames
  where
    -- A full 'writeTBQueue' would retry the whole transaction.
    writeAll _ []       = pure True
    writeAll c (f : fs) = do
      full <- isFullTBQueue (clQueue c)
      if full then pure False else writeTBQueue (clQueue c) f >> writeAll c fs
    cut (cid, client) = do
      writeTVar (clDropped client) True
      modifyTVar' (hubClients hub) (Map.delete cid)
