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
-- That one parse takes seconds over a real tree, and the server listens ahead
-- of it: a hub starts in 'Loading' over an empty store, the walk runs in its
-- own thread, and 'finishLoading' swaps the result in.  The routes that read
-- the store answer 503 until then ('Glance.Web'), so a browser is served the
-- indexing page rather than a refused connection.
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
  , loadStore
  , loadStoreWith
  , storeDocument
  , headlinesIn
  , storeRecords
  , storeResult
  , storeKeywords
  , storeTags
  , applyFile
  , dropFile
  , reseeded
    -- * Frames
  , Frame (..)
  , frameJSON
  , frameText
  , bootstrapFrame
    -- * The hub
  , Hub (hubStore, hubLoad)
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

import Glance.Query ( ConfigLayers (clPrint)
                    , HeadlineRecord (hrDigest, hrDoc, hrId, hrKeywords, hrTags)
                    , LoadFailure (..)
                    , QueryResult (..), TodoKeywords, WalkOptions, defaultWalk
                    , digestOfText, loadDirWithConfig, mergeKeywords, noConfig
                    , noKeywords, recognizedKeywords, resolveIds, rowJSON
                    , tagsOfCell )

-- The store

-- | One directory, parsed.  The dir-error count is the startup walk's: the
-- watcher re-reads files rather than re-walking, so a directory that becomes
-- unlistable later is not noticed until a restart.
data Store = Store
  { stFiles   :: !(Map FilePath FileEntry)  -- ^ path-keyed, hence walk-ordered.
  , stTags    :: !(Map Text Int)            -- ^ org tag → how many files carry it; see 'storeTags'.
  , stDirErrs :: !Int                       -- ^ directories the startup walk could not list.
  , stGen     :: !Int                       -- ^ update counter; see 'guarded'.
  , stPrint   :: !Text                      -- ^ which tree this is; see 'fingerprintOf'.
  , stConfig  :: !ConfigLayers              -- ^ the keyword config every file here was parsed under.
  }

-- | What the store holds for one file: the rows it contributes, and how its
-- last load went.  The two are independent — a file that stopped parsing keeps
-- the rows of its last good parse and is counted as a failure at the same time.
data FileEntry = FileEntry
  { feRecords :: ![HeadlineRecord]      -- ^ rows in file order, from the last good parse.
  , feFailure :: !(Maybe LoadFailure)   -- ^ how the last load ended.
  }

emptyStore :: Store
emptyStore = Store Map.empty Map.empty 0 0 "" noConfig

-- | DIR walked and parsed into a store: the same files 'Glance.Query.loadDir'
-- visits, with the per-file breakdown kept instead of folded into counts.
loadStore :: FilePath -> IO Store
loadStore = loadStoreWith defaultWalk

-- | 'loadStore' over the tree OPTS asks for.  The config is read first and kept
-- ('stConfig'): every file is parsed under it, and the watch re-reads one file
-- under the same layers, so a row that arrives by inotify is the row the walk
-- would have produced.  The fingerprint is taken last, over the finished store:
-- it says which tree this is, and the walk is the one moment the answer is
-- known to be the directory's.
loadStoreWith :: WalkOptions -> FilePath -> IO Store
loadStoreWith opts dir = do
  (cfg, files, dirErrs) <- loadDirWithConfig opts dir
  let loaded = foldl' seed (emptyStore { stDirErrs = dirErrs, stConfig = cfg }) files
  pure $! loaded { stPrint = fingerprintOf loaded }
  where seed st (path, outcome) = putFile path outcome st

-- | What tree ST is: one digest over the config it was parsed under and then
-- over every file's path and the digest of the bytes it was parsed from
-- ('Glance.Query.hrDigest', pinned at load).  Files fold in path order, so the
-- value describes the tree rather than the walk that found it, and two loads of
-- identical trees print identically.
--
-- The config is in it because it decides what the files MEAN: the same bytes
-- read under a config that has since gained a keyword are different rows, and
-- across a restart the generation — zero in every process — has nothing to say
-- about that.
--
-- This is the half of the @ETag@ that survives a restart.  'stGen' starts at
-- zero in every process, so a tag of the generation alone tells a client
-- holding @\"g0\"@ from a daemon that has since been restarted over a rewritten
-- tree that nothing has changed.  Pairing the two answers both questions: the
-- fingerprint says which tree, the generation says how far it has moved since
-- it was loaded.
--
-- A file that contributed no rows — empty, or a load that failed — has no
-- digest of its own and stands as its path alone.  It contributes no rows to a
-- response either, so nothing a client can see hides behind the tag.
fingerprintOf :: Store -> Text
fingerprintOf st =
  digestOfText (T.unlines (("config\t" <> clPrint (stConfig st))
                            : [ T.pack path <> "\t" <> stamp entry
                              | (path, entry) <- Map.toAscList (stFiles st) ]))
  where stamp = maybe "" hrDigest . listToMaybe . feRecords

-- | Every row the store holds, in walk order, before the id resolution — what
-- the files say, duplicates and all.
storeRows :: Store -> [HeadlineRecord]
storeRows = concatMap feRecords . Map.elems . stFiles

-- | Every row the store serves, in walk order: one per id
-- ('Glance.Query.resolveIds').
storeRecords :: Store -> [HeadlineRecord]
storeRecords = fst . resolveIds . storeRows

-- | The store as the load result it stands in for: same rows in the same
-- order, same counts.  @GET \/headlines@ renders this, so the served document
-- is the one a fresh 'Glance.Query.loadDir' would have produced — the id
-- resolution included, which is why both go through the same function.
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

-- | IDS looked up over rows ALREADY RESOLVED: the rows among them the ids name,
-- in the order the ids were named, and the ids nothing holds.
--
-- It takes the ROWS rather than the store, and that is the whole of the rule
-- this module enforces about @/headline@, @/links@, @/keywords@, @/tags@ and
-- @POST \/command@: every route resolves ONCE, at its own door
-- ('storeRecords'), and hands the list on.  'storeRecords' resolves the WHOLE
-- store each time it is named — ~28 ms over the 10435-row @~\/sync@ store,
-- measured on 2026-08-03 as the difference between @\/tags@ resolving once and
-- twice (medians 45.3 ms and 73.7 ms of 15 requests), against a whole
-- @GET \/headline@ of ~29 ms — so a store-taking lookup let a route resolving
-- two folds pay twice, and a fold over one per id let a marked set of a hundred
-- rows spend SECONDS on one answer.  Neither is reachable from a function that
-- cannot see the store.
--
-- A scan rather than an index: an index by id would be a second structure to
-- keep in step with 'stFiles' on every reload.  It runs over the RESOLVED rows,
-- so materializing an id two files claim opens the one the table is showing.
headlinesIn :: [HeadlineRecord] -> [Text] -> ([HeadlineRecord], [Text])
headlinesIn resolved ids =
  partitionEithers [ maybe (Right rid) Left (Map.lookup rid held) | rid <- ids ]
  where wanted = Set.fromList ids
        held   = Map.fromList [ (hrId r, r) | r <- resolved
                                            , Set.member (hrId r) wanted ]

-- | The text ST holds for PATH and the digest it was parsed from, or 'Nothing'
-- where it holds no rows for it — a file the walk never met, one that failed to
-- load, one with no top entry in it.
--
-- The pair is one file's own: every row of a file shares both, so the first row
-- answers for all of them.  It is what a write measuring an offset in a file the
-- store already read pins itself to, the way materialize pins a subtree — and a
-- caller that gets 'Nothing' owes itself a read ('Glance.Query.currentDocument')
-- rather than a guess.
storeDocument :: FilePath -> Store -> Maybe (Text, Text)
storeDocument path st = (\r -> (hrDoc r, hrDigest r)) <$> listToMaybe (recordsUnder path st)

-- | Every org tag the store's rows carry, sorted.  The producer half of
-- SCHEMA.md's virtual filter keys: each of these is a filter key of its own, so
-- @contact:tanik@ narrows to rows tagged @contact@ that also match the text.
--
-- Kept as a count per tag beside the rows rather than folded out of them per
-- request: the vocabulary is asked for on every @\/headlines@ and the rows are
-- ten thousand of them.  It moves only when a file's rows do, which is exactly
-- when 'guarded' moves the generation the @ETag@ spells, so a client's cached
-- answer can never be one the old vocabulary produced.
storeTags :: Store -> [Text]
storeTags = Map.keys . stTags

-- | The palette the store's columns carry: the config chain's keywords, then
-- whatever the files add.
--
-- One record per file is enough for the second half: every row of a file shares
-- its keyword sets and 'mergeKeywords' deduplicates, so this is the same answer
-- as merging all of them, at one merge per file.  The config leads for two
-- reasons.  It pins the ORDER — palette order is sort priority (SCHEMA.md), and
-- taking it off whichever file sorts first would let a new file at the top of
-- the tree reshuffle the badges.  And it is the only thing left when the files
-- are gone: an empty tree under a config still has the states its author
-- configured, where deriving the palette from rows would answer that a
-- configured keyword does not exist.
--
-- The head is 'recognizedKeywords' over a file declaring nothing, which is the
-- very function each row's own palette starts with — so a tree's cycle sorts the
-- same whether the answer came off the store or off one file's rows, and the
-- order is the org files' spelling in 'Data.Org.Config.keywordScopes'
-- precedence.  It is also why org's own pair is in an empty tree's palette:
-- @TODO@ and @DONE@ are recognized under every root whatever a config says, so a
-- palette that dropped them when the last file went described a tree the parser
-- does not have.
storeKeywords :: Store -> TodoKeywords
storeKeywords st = mergeKeywords (recognizedKeywords (stConfig st) noKeywords : perFile)
  where perFile = mapMaybe (fmap hrKeywords . listToMaybe . feRecords)
                           (Map.elems (stFiles st))

-- Updates

-- | PATH re-loaded into the store, and the frames the change implies.  A
-- failure keeps the file's rows and streams nothing — nothing about them is
-- known to have changed.
applyFile :: FilePath -> Either LoadFailure [HeadlineRecord] -> Store -> (Store, [Frame])
applyFile path outcome = guarded path (streamed path (putFile path outcome))

-- | PATH gone from the store: every row only it carried goes with it.
dropFile :: FilePath -> Store -> (Store, [Frame])
dropFile path = guarded path (streamed path (removeFile path))

-- | ST replaced by FRESH, and the frames that costs.
--
-- The one update that is not about a file.  A @.org-glance\/config@ edit moves
-- what every OTHER file's parse RECOGNIZES, so no per-file step can express it:
-- a word that was the first token of a title in four thousand documents is a
-- state in all of them a moment later.  The watch answers by re-walking and
-- re-parsing the tree ('Glance.Web.Watch.reseed') and handing the result here.
--
-- The diff is over every id on both sides rather than one file's, which is the
-- expensive half and the correct one; the frames are then the ordinary ops, so
-- a client that missed the reason still ends up holding the rows the server
-- has.  A moved palette REPLACES them with 'ViewChanged', exactly as 'guarded'
-- does and for the same reason — rows built against a palette that is already
-- gone are rows a client draws wrong.
--
-- The generation is 'installed's, which both writers of it go through.  What is
-- this one's own is the store it installs: FRESH was loaded from scratch and
-- carries a counter of zero, so ST's is what goes on.
reseeded :: Store -> Store -> (Store, [Frame])
reseeded fresh st = installed st fresh (outcomes st /= outcomes fresh) out
  where
    before   = rowsById st
    after    = rowsById fresh
    everyId  = Set.toAscList (Map.keysSet before <> Map.keysSet after)
    out      = if storeKeywords st /= storeKeywords fresh then [ViewChanged]
                                                          else rowFrames everyId before after
    outcomes = Map.map feFailure . stFiles

-- | Every row ST serves, by id — the resolved view, which is what a frame
-- carries and what a diff of two stores has to compare.
rowsById :: Store -> Map Text Value
rowsById st = Map.fromList [ (hrId r, rowJSON r) | r <- storeRecords st ]

-- | The ops IDS owe, given the rows a store served BEFORE a step and the rows it
-- serves AFTER it: an id whose row arrived or changed is an upsert, one the
-- store no longer serves is a delete, and one neither side holds costs nothing.
--
-- UPSERTS LEAD, and both callers rest on it: a client applying the batch in
-- order never shows fewer rows than the store has, where a delete arriving first
-- would empty a row on screen and fill it again.
--
-- IDS is the whole of what the two callers differ by — 'streamed' passes the ids
-- one file touched, 'reseeded' every id on either side — so one diff and one
-- ordering rule stand under both.
rowFrames :: [Text] -> Map Text Value -> Map Text Value -> [Frame]
rowFrames ids before after =
  [ UpsertRow row | i <- ids, Just row <- [Map.lookup i after]
                            , Map.lookup i before /= Just row ]
    <> [ DeleteRow i | i <- ids, Map.notMember i after ]

-- | NEXT installed over ST with OUT to send, the generation stepped where the
-- view moved: OUT carries something, or OUTCOMES says a file's load outcome did.
--
-- ONE rule, for the two writers of the counter.  The generation is what
-- @\/headlines@ spells as an @ETag@, so it has to move whenever a response
-- would: when rows changed (there are frames, 'ViewChanged' among them) or when
-- a file's load outcome did, which is a stats header moving with no row to show
-- for it.  A watch event over a file nothing wrote leaves it alone, so an idle
-- tree revalidates to 304 forever.
--
-- The counter is ST's rather than NEXT's, because 'reseeded' installs a store
-- loaded from scratch whose own counter is zero and a client revalidating across
-- a reseed must never be handed a tag it has already seen.  The fingerprint is
-- not one of the conditions: it moves itself, so a config edit that changes no
-- keyword rewrites bytes it covers, the @ETag@ already differs, and the
-- generation has nothing to add.
installed :: Store -> Store -> Bool -> [Frame] -> (Store, [Frame])
installed st next outcomes out = (next { stGen = stGen st + bump }, out)
  where bump = if null out && not outcomes then 0 else 1

-- | STEP, with the columns watched and the generation moved ('installed').  The
-- palette can only move when the file STEP touched changes what it declares,
-- every other file's contribution being untouched and the merge being a function
-- of them; that check is a lookup, and the full merge runs only when it fires.
guarded :: FilePath -> (Store -> (Store, [Frame])) -> Store -> (Store, [Frame])
guarded path step st = installed st next (outcome st /= outcome next) out
  where
    (next, frames) = step st
    -- '&&' short-circuits, so the full merge runs only when the touched file's
    -- own declaration moved.
    palette  = declared st /= declared next && storeKeywords st /= storeKeywords next
    out      = if palette then [ViewChanged] else frames
    declared = fmap hrKeywords . (listToMaybe . feRecords <=< Map.lookup path) . stFiles
    outcome  = fmap feFailure . Map.lookup path . stFiles

-- | UPDATE applied to the store, and the frames it owes for the ids under PATH.
-- Both sides are read through the store's own id resolution, so a streamed row
-- is the row @\/headlines@ would serve.  The diff itself is 'rowFrames', over
-- the touched ids in file order.
--
-- Resolving here is the whole point.  Where two files claim one
-- @ORG_GLANCE_ID@, an edit to the LOSING file streams the winner — which is to
-- say nothing at all, the winner being unmoved — rather than painting the
-- loser's cells over a row every other reader is shown differently; and a
-- winner that goes away re-points its id at the row behind it rather than
-- leaving a stale one until the client reconnects.
--
-- Cost: one pass over the store's rows per side, keeping only the ids the step
-- touched, and it is CHEAPER than a route's own lookup rather than the same
-- order of work — 'resolvedRows' filters to the touched ids BEFORE it resolves,
-- so the full-store resolution that dominates a request is never paid here.  A
-- scan and a resolution of a handful of rows, per watch event rather than per request:
-- measured at 5–6 ms for the whole step over a 14000-row store with a client
-- attached, where the parse alone is 4 ms.  It buys the one thing an incremental
-- view could not otherwise have: agreement with every other reader.
streamed :: FilePath -> (Store -> Store) -> Store -> (Store, [Frame])
streamed path update st = (next, rowFrames touched before after)
  where
    next     = update st
    touched  = nub (idsUnder next <> idsUnder st)
    idsUnder = map hrId . recordsUnder path
    before   = resolvedRows touched st
    after    = resolvedRows touched next

-- | The row each of IDS resolves to in ST: the store's own resolution
-- ('Glance.Query.resolveIds') over the rows carrying them, which is the call
-- every served answer goes through.  Two rules come with using it here rather
-- than keying one file's records by id.  Between files, a @.org-glance\/data\/@
-- path wins and walk order breaks the rest; within one file, two headlines
-- sharing an id leave the FIRST standing — the incumbent, since a file cannot
-- outrank itself.  Both directions are the served view's, so a streamed row and
-- a fetched one cannot contradict each other.
resolvedRows :: [Text] -> Store -> Map Text Value
resolvedRows ids st = Map.fromList [ (hrId r, rowJSON r) | r <- fst (resolveIds carrying) ]
  where wanted   = Set.fromList ids
        carrying = [ r | r <- storeRows st, Set.member (hrId r) wanted ]

-- | The rows ST holds for PATH, or none.
recordsUnder :: FilePath -> Store -> [HeadlineRecord]
recordsUnder path = maybe [] feRecords . Map.lookup path . stFiles

-- | PATH's outcome written into the store.  A failure keeps the rows the file's
-- last good parse produced and records it beside them: 'Glance.Query.orgParse'
-- is all-or-nothing, so a save caught mid-write says nothing about the
-- headlines that were there a moment ago.
putFile :: FilePath -> Either LoadFailure [HeadlineRecord] -> Store -> Store
putFile path outcome st = case outcome of
  Left failure -> st { stFiles = Map.insert path (FileEntry old (Just failure)) files }
  Right new    -> st { stFiles = Map.insert path (FileEntry new Nothing) files
                     , stTags  = stepIndex tagsOf old new (stTags st) }
  where files = stFiles st
        old   = recordsUnder path st

-- | PATH's entry removed.  A file the store never held is not an error: the
-- watcher reports every deletion it sees, including of files the walk skipped.
removeFile :: FilePath -> Store -> Store
removeFile path st = st { stFiles = Map.delete path (stFiles st)
                        , stTags  = stepIndex tagsOf (recordsUnder path st) [] (stTags st) }

-- | One index with what OLD's records claimed released and what NEW's claim
-- taken, PROJ being what a file contributes to it.
stepIndex :: Ord k => ([HeadlineRecord] -> Set k)
          -> [HeadlineRecord] -> [HeadlineRecord] -> Map k Int -> Map k Int
stepIndex proj old new ix = Set.foldl' claim (Set.foldl' release ix (proj old)) (proj new)
  where release m k = Map.update (\n -> if n <= 1 then Nothing else Just (n - 1)) k m
        claim   m k = Map.insertWith (+) k 1 m

-- | The distinct tags RECORDS carry, deduplicated per file so 'stepIndex'
-- counts files rather than rows.
tagsOf :: [HeadlineRecord] -> Set Text
tagsOf = Set.fromList . concatMap (tagsOfCell . hrTags)

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

-- | The live store and its sockets.  'hubLoad' says whether the store is the
-- directory yet: the server binds its socket before the walk runs, so every
-- route that reads the store has to be able to answer that it cannot.
data Hub = Hub
  { hubStore   :: !(TVar Store)
  , hubClients :: !(TVar (Map Int Client))
  , hubNextId  :: !(TVar Int)
  , hubLoad    :: !(TVar LoadState)
  }

-- | How far the startup load got.  'Loading' carries the monotonic time it
-- started, which is the only thing the 503 has to say about how long it has
-- been going: the walk hands its files over in one batch, so there is no
-- per-file count to report and inventing one would mean rewriting the walk.
data LoadState
  = Loading !Double  -- ^ walking and parsing since this monotonic second.
  | Loaded           -- ^ the store is the directory.
  deriving (Eq, Show)

-- | One socket's mailbox.  Bounded on purpose: the watcher hands frames to
-- every client from the transaction that updates the store, and a browser that
-- has stopped reading must not be able to hold that transaction up.  A full
-- mailbox abandons the backlog and closes the socket, which the client answers
-- by re-asking for rows ('Glance.Web.Routes.pump' names that close @resync@) —
-- losing a slow reader's frames is recoverable, stalling the watcher is not.
--
-- 'clDropped' is read before the queue in 'nextFrame', so the backlog behind a
-- full mailbox is never delivered: the queue goes with the 'Client' the moment
-- 'publish' unregisters it, and draining it here would only cost the
-- transaction a pass over the whole mailbox to throw the result away.
data Client = Client
  { clQueue   :: !(TBQueue Frame)
  , clDropped :: !(TVar Bool)
  }

-- | How many frames a client may fall behind before its backlog is abandoned.
-- One edit is a handful of frames, so this is many seconds of lag on a live
-- view and one bootstrap on a stalled one.
--
-- Sized for the burst that motivated it: 'publish' coalesces WITHIN a step, so
-- one file's save is one transaction and a handful of frames, but an editor
-- writing a tree is a step per file and nothing coalesces across them.  1024
-- covers a few hundred files back to back; past that the resync is the cheaper
-- answer anyway, since one @\/headlines@ carries what any longer backlog would
-- have said.
clientCapacity :: Natural
clientCapacity = 1024

-- | A hub over ST, ready to serve it.
newHub :: Store -> IO Hub
newHub st = hubOver st Loaded

-- | A hub with no store yet, loading since STARTED.  What 'Glance.Web.serve'
-- binds its socket over: 'finishLoading' installs the walk's result when it
-- lands, and until then every store route answers 503.
newLoadingHub :: Double -> IO Hub
newLoadingHub started = hubOver emptyStore (Loading started)

hubOver :: Store -> LoadState -> IO Hub
hubOver st load =
  Hub <$> newTVarIO st <*> newTVarIO Map.empty <*> newTVarIO 0 <*> newTVarIO load

-- | Install ST as HUB's store and open the store routes.  One transaction, so
-- no request sees the new store still described as loading.  Nothing is
-- published: a client cannot have subscribed while the socket answered 503.
finishLoading :: Hub -> Store -> IO ()
finishLoading hub st = atomically $ do
  writeTVar (hubStore hub) st
  writeTVar (hubLoad hub) Loaded

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
-- transaction.  A client that cannot take them loses its backlog and its
-- registration, and the transaction goes through regardless.
-- Returns the frames, for the caller's log.
publish :: Hub -> (Store -> (Store, [Frame])) -> IO [Frame]
publish hub step = atomically $ do
  (st, frames) <- step <$> readTVar (hubStore hub)
  writeTVar (hubStore hub) st
  clients <- readTVar (hubClients hub)
  slow <- filterM (fmap not . flip writeAll frames . snd) (Map.toList clients)
  mapM_ cut slow
  pure frames
  where
    -- The full check is what keeps this from blocking: 'writeTBQueue' on a
    -- full mailbox would retry the whole transaction, which is the watcher
    -- waiting on a browser.
    writeAll _ []       = pure True
    writeAll c (f : fs) = do
      full <- isFullTBQueue (clQueue c)
      if full then pure False else writeTBQueue (clQueue c) f >> writeAll c fs
    cut (cid, client) = do
      writeTVar (clDropped client) True
      modifyTVar' (hubClients hub) (Map.delete cid)
