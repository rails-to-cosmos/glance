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
--
-- AND SYMLINKS ARE NOT PRESERVED EITHER, which is the one that can surprise.
-- @rename(2)@ replaces the destination NAME, so writing through a symlinked
-- @.org@ file leaves a regular file where the link was and the real file
-- untouched: the table then serves the copy for ever and the original never
-- moves.  @copyPermissions@ above DOES follow the link, so the write looks
-- correct all the way through.  The walk keeps symlinked documents on purpose
-- ('Data.Org.Walk.visit'), so this is reachable; resolving the target before
-- the write is a POLICY decision (whose permissions, whose directory for the
-- temp file, what a link out of the tree means) and is deliberately not taken
-- here.
module Data.Org.Edit ( Edit (..)
                     , EditError (..)
                     , EditIOError (..)
                     , EditReceipt (..)
                     , Snapshot (..)
                     , applyEdits
                     , digestOf
                     , editFile
                     , lineSpansIn
                     , linesWith
                     , readBytes
                     , readDocument
                     , snapshotOf
                     , takeSnapshot
                     ) where

import Control.Exception (IOException, bracketOnError, try)
import Control.Monad (unless, void, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Crypto.Hash (Digest, SHA256, hash)
import Data.List (sortOn)
import Data.Text (Text)
import System.Directory ( copyPermissions, createDirectoryIfMissing, doesFileExist
                        , removeFile, renameFile )
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hClose, hFlush, openBinaryTempFile)
import System.Posix.IO (closeFd, handleToFd)
import System.Posix.Unistd (fileSynchronise)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Org.Types (Span (..), spanFaults)
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
-- by span, start then end, so two edits over DISTINCT spans give the same
-- result however the caller sorted the batch.  Two INSERTIONS at one offset are
-- the exception and the one the sort is stable for: they land in LIST order, so
-- a caller writing two pragma blocks into a file that had neither gets them in
-- the order it named them, and the outcome of such a pair is fixed by
-- construction rather than by the offsets.  One pass: the cost is the
-- document's length plus the replacements', whatever the number of edits.
applyEdits :: Text -> [Edit] -> Either EditError Text
applyEdits doc edits = do
  mapM_ (checkSpan (T.length doc) . editSpan) edits
  ordered <- disjoint (sortOn key edits)
  pure (T.concat (splice 0 doc ordered))
  where key e = (spanStart (editSpan e), spanEnd (editSpan e))

-- | Reject SP against a document of LEN characters.
--
-- The faults are 'Data.Org.Types.spanFaults'\'s — ONE enumeration of what makes a
-- span malformed, shared with the corpus audit, so a fault added there is
-- refused by this engine rather than written.  Only the backwards span has an
-- error of its own; every other fault is out of bounds, which is what a new one
-- reads as until it is given a constructor.  The backwards arm reads that
-- fault's LABEL, so renaming it there quietly downgrades a backwards span to
-- out-of-bounds -- @TestEdit@\'s @Backwards (Span 5 2)@ case is what stands
-- between that and silence.
checkSpan :: Int -> Span -> Either EditError ()
checkSpan len sp = case spanFaults len sp of
  []     -> Right ()
  faults | faults == ["start-after-end"] -> Left (Backwards sp)
         | otherwise                     -> Left (OutOfBounds sp len)

-- | T split into lines, each carrying the newline that ends it.  The last line
-- carries one only where T does, so @T.concat . linesWith@ is @id@.
linesWith :: Text -> [Text]
linesWith t
  | T.null t  = []
  | otherwise = case T.breakOn "\n" t of
      (line, rest) | T.null rest -> [line]
                   | otherwise   -> (line <> "\n") : linesWith (T.drop 1 rest)

-- | T's lines, each with the char span covering it and the newline that ends
-- it.  A final line with no newline still gets a span, ending at the document.
--
-- Here rather than beside either caller because both of them compute WHOLE-LINE
-- span edits — the settings write and the subtree lens — and this module already
-- owns the char-span arithmetic they hand the spans to.  Two spellings agreed by
-- accident: a CRLF or last-line fix in one never reached the other.
lineSpansIn :: Text -> [(Span, Text)]
lineSpansIn t = go 0 (linesWith t)
  where go _at []      = []
        go at (l : ls) = (Span at (at + T.length l), l) : go (at + T.length l) ls

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
takeSnapshot path =
  either (Left . ReadFailed path) (Right . Snapshot path . digestOf) <$> readBytes path

-- | PATH's text and the digest of the bytes it was decoded from, or 'Nothing'
-- where there is nothing readable there.
--
-- ONE read answers both, which is the point: a caller measuring offsets in the
-- text and pinning its write to the digest needs them to describe the same
-- bytes, and two reads do not promise that.  It lives beside 'takeSnapshot'
-- because this is where the pin is defined — including the convention a caller
-- reads 'Nothing' as, which 'currentText' spells as the empty digest and treats
-- as "nothing is there, create it".
--
-- An unreadable file and an undecodable one answer alike.  That is safe rather
-- than lossy for the creating caller: pinning the empty digest against a file
-- that IS there makes 'editFile' re-read, digest what it finds, and refuse as
-- 'Drift'.
readDocument :: FilePath -> IO (Maybe (Text, Text))
readDocument path = do
  raw <- readBytes path
  pure $ case raw of
    Left _err    -> Nothing
    Right bytes  -> either (const Nothing) (\doc -> Just (doc, digestOf bytes))
                           (TE.decodeUtf8' bytes)

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
  doc <- ExceptT (currentText snap)
  edited <- either (throwError . Rejected) pure (applyEdits doc edits)
  let written = TE.encodeUtf8 edited
  ExceptT (writeAtomically path written)
  pure (EditReceipt (Snapshot path (digestOf written)) edited)
  where path = snapPath snap

-- | The document SNAP's edits are measured against, or why there is none.
--
-- Ordinarily the file's own text, refused unless it still digests to the pin.
-- The EMPTY digest is the pin that says NOTHING IS THERE: the document is then
-- the empty one, and the write below creates the file and the directories over
-- it.  A file that turned up before this ran is 'Drift' carrying the digest it
-- holds, which is the same refusal a moved one gets and means the same thing to
-- a caller — what you were editing is not what is there.
--
-- So creation is not a second write path with a lock of its own; it is this one
-- under the pin an absent file has.  A missing file under a real digest stays
-- 'ReadFailed', since a caller holding one believed there was something to read.
--
-- The check is at the START of the write, not at the rename, and @rename(2)@
-- has no exclusive form — so a file created inside that window is replaced
-- rather than refused.  That is the ordinary lock's window too (the drift check
-- reads before the rename likewise) and closing it would mean a different
-- syscall than the one that makes the write atomic.
currentText :: Snapshot -> IO (Either EditIOError Text)
currentText snap = do
  there <- doesFileExist path
  if not there && T.null (snapDigest snap) then pure (Right "") else runExceptT $ do
    bytes <- ExceptT (either (Left . ReadFailed path) Right <$> readBytes path)
    let found = digestOf bytes
    unless (found == snapDigest snap) $ throwError (Drift path (snapDigest snap) found)
    either (const (throwError (DecodeFailed path))) pure (TE.decodeUtf8' bytes)
  where path = snapPath snap

-- | PATH's bytes, read strictly, or the first line of why they could not be
-- had.
--
-- Exported because every reader in this codebase wants the same three things —
-- one strict read, 'IOException' caught, the reason as text — and each hand
-- rolling it grew its own answer to what an unreadable file is.  The reason
-- comes back as TEXT rather than as an 'EditIOError' so a caller with a failure
-- type of its own is not made to unwrap this one; the two callers here wrap it
-- back into 'ReadFailed' at the point they already know the path.  A caller
-- that also wants the text decoded wants 'readDocument' instead.
readBytes :: FilePath -> IO (Either Text BS.ByteString)
readBytes path = report <$> (try (BS.readFile path) :: IO (Either IOException BS.ByteString))
  where report = either (Left . errText) Right

-- | Put BYTES at PATH without PATH ever holding anything else: a temp file in
-- PATH's own directory, flushed and synced, its permissions copied from PATH,
-- renamed over it.  Same directory because rename is atomic within a
-- filesystem and fails across one.  A failure anywhere removes the temp file.
--
-- The two steps that turn on PATH already existing are conditional, so this
-- serves the creating write too: the directory is made where there is none, and
-- there are no permissions to copy from a file that is not there — the new one
-- takes the umask, which is what any other creator would give it.
writeAtomically :: FilePath -> BS.ByteString -> IO (Either EditIOError ())
writeAtomically path bytes = report <$> attempt
  where
    attempt = try (bracketOnError open discard write) :: IO (Either IOException ())
    open = do createDirectoryIfMissing True (takeDirectory path)
              openBinaryTempFile (takeDirectory path) (takeFileName path <> ".glance-tmp")
    write (tmp, h) = do
      BS.hPut h bytes
      hFlush h
      fd <- handleToFd h  -- flushes and closes the handle, keeping the fd.
      fileSynchronise fd
      closeFd fd
      there <- doesFileExist path
      when there (copyPermissions path tmp)
      renameFile tmp path
    discard (tmp, h) = do
      ignoring (hClose h)  -- a no-op once 'handleToFd' has run.
      ignoring (removeFile tmp)
    ignoring act = void (try act :: IO (Either IOException ()))
    report = either (Left . WriteFailed path . errText) Right
