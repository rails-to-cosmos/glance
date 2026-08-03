-- | org-glance's write-ahead index, read only, and the drift instrument that
-- compares it with what this parser reads out of the same store's blobs.
--
-- WHAT IS ON DISK.  @org-glance@ keeps a metadata projection of every entry
-- under @.org-glance\/meta\/@ as an append-only log of JSON records, one per
-- line.  @headlines.jsonl@ is the OPEN segment; once it grows past a size bound
-- it is sealed by an atomic rename into an immutable @seg-NNNNNNNNNN.jsonl@,
-- and a one-line @MANIFEST@ — @{"version":2,"segments":[…]}@ — is rewritten to
-- list the live sealed segments oldest-first.  That rename is the sole commit
-- point, so a segment on disk the MANIFEST does not name is invisible.
--
-- THE FOLD, which is @org-glance-graph--latest-records@ and @--ensure-cache@
-- (@src\/data\/org-glance-graph.el@) read forwards: the live segments oldest to
-- newest with the open one LAST, every non-empty line parsed as one record, the
-- LATEST record per @id@ superseding every earlier one, and an id whose latest
-- record carries @tombstone@ dropped from the live set.  Only the open segment
-- may end in a torn line — a crash tears the last append and nothing else — so
-- a parse failure there is legitimate and one anywhere else is corruption.  The
-- elisp re-signals on corruption; a read-only instrument counts it instead
-- ('ifMalformed') and carries on, since refusing to report the other six
-- thousand rows helps nobody.
--
-- THE COMPARISON.  Each live record is matched by @id@ against the blob
-- org-glance stored for it at @.org-glance\/data\/\<2\>\/\<rest\>\/data.org@,
-- as THIS parser reads it ('BlobEntry').  Two terms are compared: the TODO
-- keyword, always; and the archive flag only where the record carries it, since
-- @archived@ joined the record schema later and every record written before it
-- has no such key at all ('irArchived' is 'Nothing' there).  An id on one side
-- and not the other is counted rather than compared.
--
-- Nothing here writes, creates, seals or repairs anything.
module Data.Org.Index ( BlobEntry (..)
                      , IndexDrift (..)
                      , IndexFold (..)
                      , IndexRecord (..)
                      , blobEntryOf
                      , driftOf
                      , foldSegments
                      , indexReportLines
                      , manifestFile
                      , metaDir
                      , openSegment
                      , segmentNames
                      ) where

import Data.Aeson (Value (Array, Bool, Object, String), decodeStrict')
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe)
import Data.Text (Text)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified TextShow as TS

-- | The directory under a store's @.org-glance@ that holds the index — the
-- segments, the MANIFEST, and the one file this repo writes there
-- ('Data.Org.External').  Named here because this module is what the layout is
-- documented in; the scan and the notifier both read it rather than spelling it.
metaDir :: FilePath
metaDir = "meta"

-- | The open append segment: read last, because it holds the newest records.
openSegment :: FilePath
openSegment = "headlines.jsonl"

-- | The file naming the live sealed segments.
manifestFile :: FilePath
manifestFile = "MANIFEST"

-- | How many disagreeing ids the report names.
driftSamples :: Int
driftSamples = 10

-- Records

-- | One live row of the index, in the two terms a blob can disagree with.  The
-- record carries fourteen more fields; none of them is compared, so none of
-- them is kept.
data IndexRecord = IndexRecord
  { irId       :: !Text          -- ^ the @ORG_GLANCE_ID@ the record is keyed by.
  , irState    :: !Text          -- ^ TODO keyword verbatim; empty when the record names none.
  , irArchived :: !(Maybe Bool)  -- ^ 'Nothing' on a record written before the field existed.
  } deriving (Eq, Show)

-- | What this parser read at one blob: the file's FIRST headline, which is the
-- entry org-glance stored there.  A blob holds ONE entry, so first rather than
-- level-one (six of ~\/sync's blobs open at level two) and first rather than
-- first-with-an-id — a CHILD carrying an id of its own is not the blob's, and
-- reaching past a headline whose drawer this parser lost would compare that
-- child's keyword against the parent's record.
data BlobEntry = BlobEntry
  { beId       :: !Text
  , beState    :: !Text      -- ^ TODO keyword verbatim; empty when the headline has none.
  , beArchived :: !Bool      -- ^ does the headline wear org's @ARCHIVE@ tag?
  , beFile     :: !FilePath  -- ^ the blob, as walked.
  } deriving (Eq, Show)

-- | The blob at PATH out of HEADLINES — the file's headlines in FILE order,
-- each as its @ORG_GLANCE_ID@, its TODO keyword and whether it is archived.
-- 'Nothing' when the file has no headline, or when the first one claims no id.
blobEntryOf :: FilePath -> [(Maybe Text, Text, Bool)] -> Maybe BlobEntry
blobEntryOf path headlines = do
  (ident, state, arch) <- listToMaybe headlines
  i <- ident
  pure (BlobEntry i state arch path)

-- Folding the log

-- | The live record set of one store, and what the read crossed to get it.
data IndexFold = IndexFold
  { ifRecords    :: !(Map Text IndexRecord)  -- ^ id to its latest record, tombstoned ids removed.
  , ifRead       :: !Int  -- ^ records parsed across every live segment.
  , ifTombstones :: !Int  -- ^ ids whose LATEST record deletes them.
  , ifMalformed  :: !Int  -- ^ lines no record could be read out of; see the module note.
  } deriving (Eq, Show)

-- | The segment file NAMES to read, in fold order, given the MANIFEST's bytes
-- (or 'Nothing' where there is no MANIFEST): its sealed segments oldest-first,
-- then 'openSegment'.  A MANIFEST that does not parse reads as the empty set,
-- the way the elisp's @condition-case@ leaves it.
--
-- A listed name is kept only when it spells @seg-\<digits\>.jsonl@ —
-- org-glance's own segment pattern.  That is the format's rule, and it doubles
-- as the path guard: a hand-edited MANIFEST cannot name @..\/..\/secrets@ and
-- have a caller open it.
segmentNames :: Maybe BC.ByteString -> [FilePath]
segmentNames manifest = [ T.unpack n | n <- listed, sealedName n ] ++ [openSegment]
  where
    listed = case manifest >>= decodeStrict' of
      Just (Object o) | Just (Array vs) <- KM.lookup (Key.fromText "segments") o ->
        [ n | String n <- toList vs ]
      _noneListed -> []

-- | Is NAME one of org-glance's sealed segments, @seg-\<digits\>.jsonl@?
sealedName :: Text -> Bool
sealedName name = case T.stripPrefix "seg-" name >>= T.stripSuffix ".jsonl" of
  Just digits -> not (T.null digits) && T.all isDigit digits
  Nothing     -> False

-- | The fold in progress: the latest record per id — 'Nothing' where that
-- record is a tombstone, so one map answers both questions — beside the counts.
-- Strict fields, so a segment's worth of lines leaves no thunk chain behind it.
data Tally = Tally !(Map Text (Maybe IndexRecord)) !Int !Int

-- | Fold SEGMENTS — each as (is it the open segment, its bytes), oldest first
-- and the open one last — to the live record set.
--
-- Bytes rather than 'Text': aeson decodes UTF-8 itself and rejects what is not,
-- so a segment holding an invalid byte costs the LINE it sits on rather than a
-- decode of the whole file, and lands in 'ifMalformed' where it belongs.
foldSegments :: [(Bool, BC.ByteString)] -> IndexFold
foldSegments = summarise . foldl' segment (Tally Map.empty 0 0)
  where
    segment acc (open, bytes) = foldl' line acc (marked (splitLines bytes))
      where
        -- The one forgivable failure: the open segment's final line, when the
        -- file does not end in a newline, is an append a crash cut in half.
        torn = open && not (BC.null bytes) && BC.last bytes /= '\n'
        marked ls = let n = length ls
                    in zip [ torn && k == n | k <- [1 :: Int ..] ] ls

    line (Tally seen total bad) (forgiven, bytes)
      | Just (i, rec) <- recordOf bytes = Tally (Map.insert i rec seen) (total + 1) bad
      | forgiven                        = Tally seen total bad
      | otherwise                       = Tally seen total (bad + 1)

    summarise (Tally seen total bad) = IndexFold
      { ifRecords    = Map.mapMaybe id seen
      , ifRead       = total
      , ifTombstones = length [ () | Nothing <- Map.elems seen ]
      , ifMalformed  = bad
      }

-- | LINE's id and record, the record being 'Nothing' when it is a tombstone —
-- so inserting under the id settles supersession and deletion in one step.
-- 'Nothing' overall when the line is not a record at all.
recordOf :: BC.ByteString -> Maybe (Text, Maybe IndexRecord)
recordOf bytes = case decodeStrict' bytes of
  Just (Object o) | Just (String i) <- get o "id" -> Just (i, live o i)
  _notARecord -> Nothing
  where
    get o k = KM.lookup (Key.fromText k) o
    live o i
      | maybe False truthy (get o "tombstone") = Nothing
      | otherwise = Just IndexRecord
          { irId = i
          , irState = case get o "state" of
              Just (String s) -> s
              _absentOrNil    -> ""
          , irArchived = flagOf <$> get o "archived"
          }

-- | Elisp's @(eq t VALUE)@, which is how org-glance decodes a boolean field:
-- only JSON @true@ is true, and the empty object it writes for @nil@ is not.
flagOf :: Value -> Bool
flagOf (Bool b) = b
flagOf _other = False

-- | Is VALUE one elisp would call non-nil?  @json-parse-string@ reads @{}@ back
-- as @nil@, which is how org-glance spells an unset flag, so the empty object
-- is the one object that is false.
truthy :: Value -> Bool
truthy (Bool b) = b
truthy (Object o) = not (KM.null o)
truthy (String s) = not (T.null s)
truthy _other = True

-- | BYTES split on newlines, empty lines dropped the way the elisp scanner
-- drops them.
splitLines :: BC.ByteString -> [BC.ByteString]
splitLines = filter (not . BC.null) . BC.split '\n'

-- Comparing

-- | One store's comparison.
data IndexDrift = IndexDrift
  { dfStore      :: !FilePath  -- ^ the @.org-glance@ directory the index belongs to.
  , dfFold       :: !IndexFold
  , dfBlobs      :: !Int       -- ^ blobs the walk parsed under that store.
  , dfIdless     :: !Int       -- ^ of those, how many carried no id to match by.
  , dfRows       :: !Int       -- ^ ids disagreeing in EITHER term.
  , dfState      :: !Int       -- ^ ids whose TODO keyword disagrees.
  , dfArchived   :: !Int       -- ^ ids whose archive flag disagrees, of those the record states.
  , dfUnindexed  :: !Int       -- ^ blobs no live record names.
  , dfRecordless :: !Int       -- ^ live records with no blob.
  , dfSamples    :: ![Text]    -- ^ up to 'driftSamples' disagreements, id-ordered.
  } deriving (Eq, Show)

-- | Compare STORE's folded index against the BLOBS the walk parsed under it —
-- each as the file and the entry read out of it, 'Nothing' where no headline in
-- it claimed an id.
--
-- An idless blob is neither indexed nor unindexed: there is nothing to match it
-- by, so it is counted ('dfIdless') and the record that named it stays in
-- 'dfRecordless'.  It means this parser read the file and org-glance's id was
-- not in what it read — a `#+TODO:` a seed does not carry, a timestamp it
-- refuses, a drawer its headline lost — so the count is the instrument turned
-- on itself, and it is why 'dfRecordless' is not read as deletion lag.
--
-- Blobs are keyed by id with the first in walk order winning, which is
-- 'Glance.Query.resolveIds' rule for a tie inside one store; a second blob
-- claiming an id is an org-glance bug, and it lands here as one unindexed blob
-- rather than as a resolution.
driftOf :: FilePath -> IndexFold -> [(FilePath, Maybe BlobEntry)] -> IndexDrift
driftOf store folded blobs = IndexDrift
  { dfStore      = store
  , dfFold       = folded
  , dfBlobs      = length blobs
  , dfIdless     = length [ () | (_, Nothing) <- blobs ]
  , dfRows       = length disagreeing
  , dfState      = length [ () | (_, s, _) <- disagreeing, not (T.null s) ]
  , dfArchived   = length [ () | (_, _, a) <- disagreeing, not (T.null a) ]
  , dfUnindexed  = Map.size (Map.difference byId (ifRecords folded))
  , dfRecordless = Map.size (Map.difference (ifRecords folded) byId)
  , dfSamples    = take driftSamples (concatMap sample disagreeing)
  }
  where
    byId = Map.fromListWith (\_new old -> old) [ (beId b, b) | (_, Just b) <- blobs ]
    disagreeing = [ (i, state, arch)
                  | (i, rec) <- Map.toAscList (ifRecords folded)
                  , Just entry <- [Map.lookup i byId]
                  , let state = stateNote rec entry
                  , let arch = archiveNote rec entry
                  , not (T.null state && T.null arch) ]
    sample (i, state, arch) = [ i <> ": " <> note | note <- [state, arch], not (T.null note) ]

-- | How REC and BLOB disagree about the TODO keyword, or empty when they do
-- not.  A record naming no keyword and a headline carrying none both read @""@.
stateNote :: IndexRecord -> BlobEntry -> Text
stateNote rec blob
  | irState rec == beState blob = ""
  | otherwise = "state wal=" <> shown (irState rec) <> " blob=" <> shown (beState blob)
  where shown t = if T.null t then "none" else t

-- | How REC and BLOB disagree about the archive flag, or empty when they agree
-- or when the record predates the field.
archiveNote :: IndexRecord -> BlobEntry -> Text
archiveNote rec blob = case irArchived rec of
  Just flag | flag /= beArchived blob ->
    "archived wal=" <> yesNo flag <> " blob=" <> yesNo (beArchived blob)
  _agreesOrAbsent -> ""
  where yesNo b = if b then "true" else "false"

-- Reporting

-- | DRIFT as the scan prints it: the headline line a reader takes the verdict
-- off, three rows of counts, then the sampled disagreements.
indexReportLines :: IndexDrift -> [Text]
indexReportLines d =
  [ "org-glance index: " <> num (dfRows d) <> " rows disagree ("
      <> num (dfState d) <> " state, " <> num (dfArchived d) <> " archived)"
  , field "store" (T.pack (dfStore d))
  , field "records" (T.intercalate ", "
      [ num (ifRead folded) <> " read"
      , num (Map.size (ifRecords folded)) <> " live"
      , num (ifTombstones folded) <> " tombstones"
      , num (ifMalformed folded) <> " malformed" ])
  , field "blobs" (num (dfBlobs d) <> " parsed, " <> num (dfIdless d) <> " carrying no id")
  , field "unmatched" (num (dfUnindexed d) <> " unindexed blobs, "
                         <> num (dfRecordless d) <> " records without blobs")
  ] ++ map ("  " <>) (dfSamples d)
  where
    folded = dfFold d
    field label value = "  " <> T.justifyLeft 11 ' ' label <> value
    num :: Int -> Text
    num = TS.showt
