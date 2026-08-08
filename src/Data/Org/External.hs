-- | The note this daemon leaves when it writes one of org-glance's blobs:
-- @\<store\>\/.org-glance\/meta\/EXTERNAL.jsonl@.
--
-- WHY THERE IS ONE.  A blob is the canonical document and org-glance's index is
-- its projection, derived and written by Emacs alone.  This daemon edits the
-- blob and not the index, so every browser edit leaves the index a record
-- behind — the drift the instrument counts.  Here the two sides meet: the
-- writer names the ids it moved, and @M-x org-glance-graph:refresh-external@
-- re-derives a record for each and takes those lines off.
--
-- THE CONTRACT, frozen.  One JSON object per line, newline-terminated, two
-- fields in this order:
--
-- > {"id":"e3b0c442-…","at":"2026-08-03T04:21:07Z"}
--
-- @id@ is the written blob's FIRST headline's @ORG_GLANCE_ID@, read the way
-- 'Data.Org.Index.blobEntryOf' reads it, so a line names the record that will
-- replace it; @at@ is the server clock in UTC, and nothing acts on it.  APPEND
-- ONLY: this side never truncates, never rewrites, and touches no file under
-- @meta@ but its own two ('externalFile' and 'completionsFile').
--
-- THE CRASH RULE.  The reader appends every re-derived record BEFORE shortening
-- this file, so a crash between the two costs a repeated refresh and nothing
-- else — re-deriving from an unmoved blob appends an equal record and the fold
-- keeps the latest per id.  Idempotent by construction, which is what lets the
-- two steps be unsynchronised; the reader drops exactly the prefix it read, so
-- a line appended mid-refresh survives.
--
-- WHAT IS NOT PROMISED.  A line is a HINT that a blob moved.  The append is
-- best effort and a failure is swallowed: the blob is already on disk and the
-- answer the caller is about to send describes THAT write.  A lost line costs
-- drift the instrument reports and the next edit of the same id repairs.
module Data.Org.External ( Completion (..)
                         , blobIdOf
                         , completionLine
                         , completionsFile
                         , completionsPathOf
                         , externalFile
                         , externalLine
                         , externalPathOf
                         , noteCompletion
                         , noteExternalWrite
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

-- | Where a write to PATH is to be noted, or 'Nothing' where it is not to be
-- noted at all.
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

-- | The line naming IDENT, written AT.  Hand-assembled rather than encoded from
-- an object, so the field ORDER is the contract's; only the values go through
-- the JSON encoder, which is where the escaping has to happen.
externalLine :: Text -> Time.UTCTime -> BS.ByteString
externalLine ident at =
  BL.toStrict ("{\"id\":" <> encode ident <> ",\"at\":" <> encode (stamp at) <> "}\n")
  where stamp = spelled "%Y-%m-%dT%H:%M:%SZ"

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
noteExternalWrite path written = case (externalPathOf path, blobIdOf written) of
  (Just note, Just ident) -> appendNote note (externalLine ident)
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
