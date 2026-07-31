-- | The write-back engine: replace character spans of a document and put the
-- result back on disk atomically.  Protocol-independent, and content-agnostic
-- by design — the replacement text comes from the caller and nothing here
-- consults 'TextShow'.  Spans are the lossless channel (docs/invariants.md,
-- Render), so a command layer that re-renders instead of splicing turns every
-- edit into a whole-file hunk.
--
-- Offsets are the parser's: half-open character spans into the very 'Text'
-- 'Data.Org.Parser.orgParse' was given.  Bytes would splice mid-codepoint on
-- the first unicode title.
--
-- The write is optimistic.  A 'Snapshot' pins the SHA-256 of the bytes the
-- caller's parse came from; 'editFile' re-reads, re-digests, and refuses on any
-- difference, so an edit computed against a stale document never lands and the
-- file stays untouched.  The replacement goes through a temp file in the
-- target's own directory — rename is atomic within one filesystem, so a
-- concurrent reader sees the old bytes or the new ones and never a half-written
-- file.  The temp name ends in @.glance-tmp@, which is not @.org@: a watcher
-- walking the directory ignores it.
--
-- What is preserved: the file's permission bits, copied onto the temp file
-- before the rename.  What is not: owner, group, timestamps, extended
-- attributes and hard links — the rename installs a new inode, so full metadata
-- preservation is out of scope for this engine.  Durability stops at the file:
-- the data is @fsync@ed before the rename, the containing directory is not.
module Data.Org.Edit ( Edit (..)
                     , EditError (..)
                     , EditIOError (..)
                     , EditReceipt (..)
                     , Snapshot (..)
                     , applyEdits
                     , digestOf
                     , editFile
                     , snapshotOf
                     , takeSnapshot
                     ) where

import Control.Exception (IOException, bracketOnError, try)
import Control.Monad (unless, void)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Crypto.Hash (Digest, SHA256, hash)
import Data.List (sortOn)
import Data.Text (Text)
import System.Directory (copyPermissions, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hClose, hFlush, openBinaryTempFile)
import System.Posix.IO (closeFd, handleToFd)
import System.Posix.Unistd (fileSynchronise)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Org.Types (Span (..))
import Data.Org.Walk (errText)

-- Edits

-- | Replace the half-open character span 'editSpan' with 'editText'.  A
-- zero-width span inserts; empty text deletes.
data Edit = Edit { editSpan :: !Span, editText :: !Text }
  deriving (Eq, Show)

-- | Why a batch of edits does not apply to a document.
data EditError
  = OutOfBounds !Span !Int  -- ^ the span, and the length of the document it does not fit.
  | Backwards !Span         -- ^ 'spanStart' past 'spanEnd'.
  | Overlap !Span !Span     -- ^ the earlier span, and the one starting inside it.
  deriving (Eq, Show)

-- | DOC with EDITS applied, every offset read against DOC itself.
--
-- Edits may arrive in any order and must be pairwise non-overlapping; touching
-- is fine, an edit may start where the previous one ends.  Application order is
-- by span, start then end, so two insertions at one offset land in list order
-- and the result never depends on how the caller sorted the batch.  One pass:
-- the cost is the document's length plus the replacements', whatever the number
-- of edits.
applyEdits :: Text -> [Edit] -> Either EditError Text
applyEdits doc edits = do
  mapM_ (checkSpan (T.length doc) . editSpan) edits
  ordered <- disjoint (sortOn key edits)
  pure (T.concat (splice 0 doc ordered))
  where key e = (spanStart (editSpan e), spanEnd (editSpan e))

-- | Reject SP against a document of LEN characters.
checkSpan :: Int -> Span -> Either EditError ()
checkSpan len sp
  | spanStart sp < 0 || spanEnd sp > len = Left (OutOfBounds sp len)
  | spanStart sp > spanEnd sp            = Left (Backwards sp)
  | otherwise                            = Right ()

-- | EDITS unchanged when no two of them overlap, else the offending pair.
-- Expects them sorted by span.
disjoint :: [Edit] -> Either EditError [Edit]
disjoint edits = mapM_ check (zip edits (drop 1 edits)) >> pure edits
  where check (a, b)
          | spanEnd (editSpan a) <= spanStart (editSpan b) = Right ()
          | otherwise = Left (Overlap (editSpan a) (editSpan b))

-- | The pieces of the edited document, left to right: REST is the source still
-- ahead of the cursor and AT is the offset it starts at, so each edit costs
-- only the distance it advances.
splice :: Int -> Text -> [Edit] -> [Text]
splice _at rest [] = [rest]
splice at rest (Edit (Span s e) new : es) = kept : new : splice e dropped es
  where (kept, from) = T.splitAt (s - at) rest
        dropped      = T.drop (e - s) from

-- Snapshots

-- | A file as it stood when its document was read: the path, and the SHA-256 of
-- its bytes in lowercase hex.  'editFile' writes only while the file still
-- digests to this.
data Snapshot = Snapshot { snapPath :: !FilePath, snapDigest :: !Text }
  deriving (Eq, Show)

-- | What an 'editFile' wrote: the file's new 'Snapshot', digested from the
-- bytes it put down, and the text they spell.  A caller chains a second edit
-- off the receipt without re-reading, and re-parses the new text for the spans
-- the next edit needs.
data EditReceipt = EditReceipt
  { receiptSnapshot :: !Snapshot  -- ^ the file as written.
  , receiptText     :: !Text      -- ^ the document as written.
  } deriving (Eq, Show)

-- | Why an 'editFile' or 'takeSnapshot' did not go through.  Every one of them
-- leaves the target byte-identical.
data EditIOError
  = ReadFailed !FilePath !Text   -- ^ path, and the first line of the IO error.
  | DecodeFailed !FilePath       -- ^ the bytes on disk are not valid UTF-8.
  | Drift !FilePath !Text !Text  -- ^ path, the snapshot's digest, the digest found.
  | Rejected !EditError          -- ^ the edits do not apply to the current text.
  | WriteFailed !FilePath !Text  -- ^ the temp write, the permission copy or the rename failed.
  deriving (Eq, Show)

-- | PATH's snapshot as it stands right now, or why it could not be read.  The
-- digest is of the bytes, so a file that is not valid UTF-8 still snapshots and
-- fails later, at the edit.
takeSnapshot :: FilePath -> IO (Either EditIOError Snapshot)
takeSnapshot path = fmap (Snapshot path . digestOf) <$> readBytes path

-- | The snapshot PATH has while it holds DOC.  For the load path, which decoded
-- the bytes it parsed and need not read them twice; equal to a 'takeSnapshot'
-- of the same file whenever DOC is what it holds.
snapshotOf :: FilePath -> Text -> Snapshot
snapshotOf path doc = Snapshot path (digestOf (TE.encodeUtf8 doc))

-- | The SHA-256 of BYTES, lowercase hex — the digest a 'Snapshot' pins.
-- Exported for a loader holding the bytes it parsed: it pins the document it
-- computed its spans against without reading the file a second time, which is
-- the only way the offsets and the digest are guaranteed to describe one text.
digestOf :: BS.ByteString -> Text
digestOf bytes = T.pack (show (hash bytes :: Digest SHA256))

-- Writing

-- | Apply EDITS to the file SNAP was taken of and write the result back.
--
-- The file is re-read and re-digested first: a digest other than the
-- snapshot's means someone else wrote in the meantime, and the answer is
-- 'Drift' with the file untouched.  Nothing is written until the whole batch
-- applies, so a rejected edit leaves no partial state either.  A batch that
-- changes nothing, an empty one included, still rewrites the file with the same
-- bytes — the engine writes what it is told to.
editFile :: Snapshot -> [Edit] -> IO (Either EditIOError EditReceipt)
editFile snap edits = runExceptT $ do
  bytes <- ExceptT (readBytes path)
  let found = digestOf bytes
  unless (found == snapDigest snap) $ throwError (Drift path (snapDigest snap) found)
  doc <- either (const (throwError (DecodeFailed path))) pure (TE.decodeUtf8' bytes)
  edited <- either (throwError . Rejected) pure (applyEdits doc edits)
  let written = TE.encodeUtf8 edited
  ExceptT (writeAtomically path written)
  pure (EditReceipt (Snapshot path (digestOf written)) edited)
  where path = snapPath snap

-- | PATH's bytes, read strictly.
readBytes :: FilePath -> IO (Either EditIOError BS.ByteString)
readBytes path = report <$> (try (BS.readFile path) :: IO (Either IOException BS.ByteString))
  where report = either (Left . ReadFailed path . errText) Right

-- | Put BYTES at PATH without PATH ever holding anything else: a temp file in
-- PATH's own directory, flushed and synced, its permissions copied from PATH,
-- renamed over it.  Same directory because rename is atomic within a
-- filesystem and fails across one.  A failure anywhere removes the temp file.
writeAtomically :: FilePath -> BS.ByteString -> IO (Either EditIOError ())
writeAtomically path bytes = report <$> attempt
  where
    attempt = try (bracketOnError open discard write) :: IO (Either IOException ())
    open = openBinaryTempFile (takeDirectory path) (takeFileName path <> ".glance-tmp")
    write (tmp, h) = do
      BS.hPut h bytes
      hFlush h
      fd <- handleToFd h  -- flushes and closes the handle, keeping the fd.
      fileSynchronise fd
      closeFd fd
      copyPermissions path tmp
      renameFile tmp path
    discard (tmp, h) = do
      ignoring (hClose h)  -- a no-op once 'handleToFd' has run.
      ignoring (removeFile tmp)
    ignoring act = void (try act :: IO (Either IOException ()))
    report = either (Left . WriteFailed path . errText) Right
