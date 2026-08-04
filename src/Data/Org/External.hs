-- | The note this daemon leaves when it writes one of org-glance's stored
-- blobs: @\<store\>\/.org-glance\/meta\/EXTERNAL.jsonl@.
--
-- WHY THERE IS ONE.  A blob is the canonical document and org-glance's
-- write-ahead index is its projection ('Data.Org.Index'), derived by Emacs and
-- written by Emacs alone.  This daemon edits the blob and does not write the
-- index, so every browser edit leaves the index one record behind — which is
-- exactly what the drift instrument counts, and what the corpus reports as rows
-- disagreeing.  This file is where the two sides meet: the writer names the ids
-- it moved and nothing else; @M-x org-glance-graph:refresh-external@ re-derives
-- a record for each of them and takes those lines off the file.
--
-- THE CONTRACT, frozen.  One JSON object per line, terminated by a newline, two
-- fields in this order:
--
-- > {"id":"e3b0c442-…","at":"2026-08-03T04:21:07Z"}
--
-- @id@ is the @ORG_GLANCE_ID@ of the written blob's FIRST headline — the entry
-- org-glance stored there, read the way 'Data.Org.Index.blobEntryOf' reads it,
-- so a line names the record refreshing it will replace.  @at@ is the server
-- clock in UTC at second resolution, and nothing acts on it.  The file is
-- created with its directories where there is none, and it is only ever
-- APPENDED to: this side never truncates, never rewrites, and never touches
-- another file under @meta@.
--
-- THE CRASH RULE.  The reader appends every re-derived record AFTER which it
-- shortens this file, so a crash between the two costs a repeated refresh and
-- nothing else: re-deriving a record from a blob that has not moved appends a
-- record equal to the one already there, and the fold keeps the latest per id
-- either way.  Idempotent by construction, which is what lets the two steps be
-- unsynchronised.  The reader drops exactly the prefix it read, so a line
-- appended here mid-refresh survives to the next one.
--
-- WHAT IS NOT PROMISED.  A line is a HINT that a blob moved.  The append is
-- best effort and a failure to write it is swallowed ('noteExternalWrite'),
-- because the blob is already on disk and the answer the caller is about to
-- send describes THAT write.  A lost line costs drift the instrument reports
-- and the next edit of the same id repairs.
module Data.Org.External ( blobIdOf
                         , externalFile
                         , externalLine
                         , externalPathOf
                         , noteExternalWrite
                         ) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (void)
import Data.Aeson (encode)
import Data.Text (Text)
import Foreign.Ptr (castPtr)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.Posix.IO ( OpenFileFlags (append, creat), OpenMode (WriteOnly)
                       , closeFd, defaultFileFlags, fdWriteBuf, openFd )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Time as Time

import Data.Org (defaultContext, firstHeadlineOf, identity, orgParse, spelled)
import Data.Org.Blob (metaDirIn)
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
  (Just note, Just ident) -> do
    now <- Time.getCurrentTime
    swallowing $ do
      createDirectoryIfMissing True (takeDirectory note)
      appendLine note (externalLine ident now)
  _noBlobOrNoId -> pure ()
  where swallowing act = void (try act :: IO (Either IOException ()))

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
