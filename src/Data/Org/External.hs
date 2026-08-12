-- | The note this daemon leaves when it writes or deletes one of org-glance's
-- blobs: @\<store\>\/.org-glance\/meta\/EXTERNAL.jsonl@.
--
-- WHY THERE IS ONE.  A blob is the canonical document and org-glance's index is
-- its projection, derived and written by Emacs alone.  This daemon edits the
-- blob and not the index, so every browser edit leaves the index a record
-- behind — the drift the instrument counts.  Here the two sides meet: the
-- writer names the ids it moved and says of each whether the blob is still
-- there, and @M-x org-glance-graph:refresh-external@ re-derives a record for
-- every id whose blob survived, tombstones the rest, and takes those lines off.
--
-- THE CONTRACT, frozen.  One JSON object per line, newline-terminated.  A write
-- is two fields in this order:
--
-- > {"id":"e3b0c442-…","at":"2026-08-03T04:21:07Z"}
--
-- and a DELETE is those two plus a third, in this order:
--
-- > {"id":"e3b0c442-…","at":"2026-08-03T04:21:07Z","tombstone":true}
--
-- @id@ is the blob's FIRST headline's @ORG_GLANCE_ID@, read the way
-- 'Data.Org.Index.blobEntryOf' reads it, so a line names the record it speaks
-- for; @at@ is the server clock in UTC, and nothing acts on it.  APPEND ONLY:
-- this side never truncates, never rewrites, and touches no file under @meta@
-- but its own two ('externalFile' and 'completionsFile').
--
-- @tombstone@ appears on a delete alone and its value is only JSON @true@.
-- There is no @"tombstone":false@ — absence IS the plain line, so each fact has
-- one spelling, and it is the spelling the READER of this file takes as a
-- delete: org-glance's @(eq t …)@ test, the stricter of the two readers these
-- repos carry ('Data.Org.Index.recordOf' reads the WAL's own key with
-- @truthy@).  The word is org-glance's own: its WAL record spells
-- @:tombstone t@ and 'Data.Org.Index' already reads it, so a second vocabulary
-- would be a second thing to keep in step.
--
-- COMPATIBILITY BOTH WAYS, which is why the delete is a third field rather than
-- a new @op@ vocabulary.  A NEW glance against an OLD org-glance degrades to
-- exactly today's behaviour: @--read-external@ reads @id@ alone and ignores
-- what it does not know, so the id is read, the blob it names is gone, and the
-- line is skipped as "no stored blob" and dropped.  An OLD glance against a NEW
-- org-glance never writes the field, so nothing changes.
--
-- THE CRASH RULE.  The reader appends every re-derived record BEFORE shortening
-- this file, so a crash between the two costs a repeated refresh and nothing
-- else — re-deriving from an unmoved blob appends an equal record and the fold
-- keeps the latest per id.  Idempotent by construction, which is what lets the
-- two steps be unsynchronised; the reader drops exactly the prefix it read, so
-- a line appended mid-refresh survives.
--
-- WHAT IS NOT PROMISED.  A plain line is a HINT that a blob moved; a tombstone
-- says the blob is GONE, so the record is to be dropped rather than re-derived
-- — the one line here that is not a re-derivation hint.  The append is best
-- effort and a failure is swallowed either way: the blob is already written, or
-- already in the trash, and the answer the caller is about to send describes
-- THAT act.  A lost line costs drift the instrument reports; a lost write note
-- the next edit of the same id repairs, and a lost tombstone leaves the record
-- live, which is the state before any of this existed.
module Data.Org.External ( Completion (..)
                         , blobIdOf
                         , completionLine
                         , completionsFile
                         , completionsPathOf
                         , externalFile
                         , externalLine
                         , externalPathOf
                         , noteCompletion
                         , noteExternalDelete
                         , noteExternalWrite
                         , tombstoneLine
                         ) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (void)
import Data.Aeson (encode)
import Data.Text (Text)
import Foreign.Ptr (castPtr)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath (takeDirectory, (</>))
import System.Posix.IO ( OpenFileFlags (append, creat), OpenMode (WriteOnly)
                       , closeFd, defaultFileFlags, fdWriteBuf, openFd )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Time as Time

import Data.Org (defaultContext, firstHeadlineOf, identity, orgParse, spelled)
import Data.Org.Blob (metaDirIn, storeRootIn)
import Data.Org.Walk (isBlob, orgGlanceRoot)

-- | The one file this repo writes under a store's @meta@ directory.
externalFile :: FilePath
externalFile = "EXTERNAL.jsonl"

-- | Where a write to PATH, or its deletion, is to be noted — or 'Nothing' where
-- neither is to be noted at all.
--
-- Purely textual over the path, which is what lets a delete ask it after
-- 'Data.Org.Trash.trashBlob' has taken the bytes away.
--
-- A blob and nothing else.  'Data.Org.Walk.isBlob' is @data.org@ inside the
-- canonical store, which is the one file per entry org-glance keys by
-- @ORG_GLANCE_ID@ — an ordinary document under @data\/@ has no record to
-- refresh, and a config or an overview is not the store's content at all.  The
-- store is the @.org-glance@ directory the blob sits under, so a tree holding
-- several stores notes each write in its own.
externalPathOf :: FilePath -> Maybe FilePath
externalPathOf path
  | not (isBlob path) = Nothing
  | otherwise = (\store -> metaDirIn store </> externalFile) <$> orgGlanceRoot path

-- | The second file this repo writes there: one line per COMPLETION of a
-- repeating entry.
completionsFile :: FilePath
completionsFile = "COMPLETIONS.jsonl"

-- | Where ROOT's completions are recorded, or 'Nothing' where the tree keeps no
-- store.  A tree with no @.org-glance@ repeats org-natively and records
-- nothing: no daemon makes a store directory it was not given.
completionsPathOf :: FilePath -> IO (Maybe FilePath)
completionsPathOf root = do
  there <- doesDirectoryExist store
  pure (if there then Just (metaDirIn store </> completionsFile) else Nothing)
  where store = storeRootIn root

-- | The line saying IDENT was completed AT into STATE, its next occurrence
-- SHIFTED.  'externalLine''s shape one file over: hand-assembled so the field
-- order is the contract's, values through the encoder where escaping happens.
-- | One repeat, as the ledger records it.  A RECORD rather than three
-- positional 'Text's: all three have the same type, a caller swapping two would
-- compile, and this file is derived, so nothing downstream would catch it.
data Completion = Completion
  { coIdent   :: !Text  -- ^ the entry's @ORG_GLANCE_ID@, the ledger's key.
  , coState   :: !Text  -- ^ the keyword it landed on.
  , coShifted :: !Text  -- ^ its next occurrence, cookie and all.
  } deriving (Eq, Show)

completionLine :: Completion -> Time.UTCTime -> BS.ByteString
completionLine c at = BL.toStrict
  (  "{\"id\":" <> encode (coIdent c)
  <> ",\"at\":" <> encode (spelled "%Y-%m-%dT%H:%M:%SZ" at)
  <> ",\"state\":" <> encode (coState c)
  <> ",\"shifted\":" <> encode (coShifted c) <> "}\n" )

-- | Record C under ROOT.  A no-op for a tree with no store; whether the entry
-- HAS an id is decided where the id is looked up, so there is no second gate.
--
-- THE LEDGER IS DERIVED, NEVER TRUTH: the org file already carries the shifted
-- stamp and the reset keyword, so every IO failure here is swallowed for
-- 'noteExternalWrite''s reason — the write has landed either way.
noteCompletion :: FilePath -> Completion -> IO ()
noteCompletion root c =
  completionsPathOf root >>= mapM_ (\note -> appendNote note (completionLine c))

-- | The frozen two fields naming IDENT AT, with EXTRA behind them.
-- Hand-assembled rather than encoded from an object, so the field ORDER is the
-- contract's; only the values go through the JSON encoder, which is where the
-- escaping has to happen.
--
-- ONE SPELLING OF THE PREFIX, which is what makes the delete's line the write's
-- line plus a field rather than a second copy free to drift from it.
noteLine :: [BL.ByteString] -> Text -> Time.UTCTime -> BS.ByteString
noteLine extra ident at = BL.toStrict
  ("{\"id\":" <> encode ident <> ",\"at\":" <> encode (stamp at) <> mconcat extra <> "}\n")
  where stamp = spelled "%Y-%m-%dT%H:%M:%SZ"

-- | The line saying IDENT's blob moved AT, and its record is to be re-derived.
externalLine :: Text -> Time.UTCTime -> BS.ByteString
externalLine = noteLine []

-- | The line saying IDENT's blob went AT, and its record is to be dropped.
--
-- @true@ is a LITERAL rather than an encoded value: it is the field's only
-- legal spelling, so nothing here can emit the @false@ the contract does not
-- have.
tombstoneLine :: Text -> Time.UTCTime -> BS.ByteString
tombstoneLine = noteLine [",\"tombstone\":true"]

-- | The @ORG_GLANCE_ID@ of DOC's first headline, which is the entry a blob
-- holds.  FIRST rather than first-with-an-id, for 'Data.Org.Index.blobEntryOf's
-- reason: a child carrying an id of its own is not the blob's.
--
-- Seeded from 'defaultContext' like every other parse here.  A keyword only a
-- tag config declares folds into the title under that seed, which costs this
-- nothing: the id is a property, and a headline whose keyword went unrecognised
-- still carries its drawer.  A document no parse reads yields no elements and
-- therefore no id — the same silence as a blob whose entry claims none.
blobIdOf :: Text -> Maybe Text
blobIdOf doc = firstHeadlineOf elems >>= identity
  where (elems, _ctx, _err) = orgParse defaultContext doc

-- | Note that PATH now holds WRITTEN.  A no-op unless PATH is a blob whose
-- first headline claims an id.
--
-- A caller writing several rows of one blob has made ONE
-- 'Data.Org.Edit.editFile' call and gets ONE line: the id names the entry
-- rather than the edit.
--
-- Every IO failure here is swallowed.  The blob is already renamed into place
-- by the time this runs, so nothing this function can do makes the write not
-- have happened, and failing the caller's answer over a hint would report a
-- write that landed as a write that did not.
noteExternalWrite :: FilePath -> Text -> IO ()
noteExternalWrite = noteBlob externalLine

-- | Note that PATH's blob is gone, DELETED being the document it held.
--
-- DELETED is the document as it stood BEFORE the move: 'Data.Org.Trash.trashBlob'
-- takes the whole blob directory out of the live tree, so afterwards there is
-- nothing left to read an id from.
--
-- NO ID, NO LINE, which is 'noteExternalWrite''s own rule and has the same
-- consequence read the other way: a blob claiming no @ORG_GLANCE_ID@ names no
-- record, so its record stays live — the state before any of this existed.
noteExternalDelete :: FilePath -> Text -> IO ()
noteExternalDelete = noteBlob tombstoneLine

-- | Append the line RENDER spells for DOC's entry to PATH's store, or nothing.
--
-- TWO GATES AND ONE PLACE FOR BOTH: the path must be a blob under a store, and
-- its first headline must claim an id.  A write and a delete differ in the line
-- alone, so neither can come to answer a gate the other does not.
noteBlob :: (Text -> Time.UTCTime -> BS.ByteString) -> FilePath -> Text -> IO ()
noteBlob render path doc = case (externalPathOf path, blobIdOf doc) of
  (Just note, Just ident) -> appendNote note (render ident)
  _noBlobOrNoId           -> pure ()

-- | Put the line RENDER spells at the end of NOTE, best effort, under one clock
-- read.  The org file is renamed into place by the time this runs, so nothing
-- here can make the write not have happened and no failure reaches the caller's
-- answer — the rule both ledgers keep, written once.
appendNote :: FilePath -> (Time.UTCTime -> BS.ByteString) -> IO ()
appendNote note render = do
  now <- Time.getCurrentTime
  void (try (write now) :: IO (Either IOException ()))
  where write at = do createDirectoryIfMissing True (takeDirectory note)
                      appendLine note (render at)

-- | Put LINE at the end of PATH, creating PATH where there is none.
--
-- @O_APPEND@ and ONE @write(2)@, rather than a 'System.IO.Handle' in
-- 'System.IO.AppendMode': GHC's append handle remembers the offset it opened
-- at, so two daemons — or two threads of one, which is what a marked set across
-- files is — write over each other's lines and the file ends up shorter than
-- the number of writes.  Under @O_APPEND@ the kernel re-seeks to the end inside
-- each write, so a line lands whole and after every line already there.
--
-- Nothing here truncates, seeks or re-reads: what is in the file is the
-- reader's, and this side only ever grows it.
appendLine :: FilePath -> BS.ByteString -> IO ()
appendLine path line = bracket (openFd path WriteOnly flags) closeFd $ \fd ->
  BU.unsafeUseAsCStringLen line $ \(bytes, len) ->
    void (fdWriteBuf fd (castPtr bytes) (fromIntegral len))
  where flags = defaultFileFlags { append = True, creat = Just 0o666 }
