-- | org-glance's write-ahead index, read only, and the drift instrument over
-- it.  The fold is @org-glance-graph--latest-records@ read forwards.
module Data.Org.Index ( BlobEntry (..)
                      , IndexDrift (..)
                      , IndexFold (..)
                      , IndexRecord (..)
                      , TailCursor
                      , blobEntryOf
                      , driftOf
                      , foldSegments
                      , indexReportLines
                      , manifestFile
                      , metaDir
                      , openSegment
                      , segmentEnd
                      , segmentNames
                      , tailFrom
                      , tailIds
                      , tailedFile
                      , withFd
                      ) where

import Control.Exception (IOException, bracket, try)
import Data.Aeson (Value (Array, Bool, Object, String), decodeStrict')
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.List (foldl', sort)
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import System.FilePath (takeFileName)
import System.IO (SeekMode (AbsoluteSeek))
import System.Posix.Files (deviceID, fileID, fileSize, getFdStatus)
import System.Posix.IO ( OpenFileFlags, OpenMode (ReadOnly), closeFd
                       , defaultFileFlags, fdSeek, openFd )
import System.Posix.IO.ByteString (fdRead)
import System.Posix.Types (ByteCount, DeviceID, Fd, FileID)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified TextShow as TS

metaDir :: FilePath
metaDir = "meta"

openSegment :: FilePath
openSegment = "headlines.jsonl"

manifestFile :: FilePath
manifestFile = "MANIFEST"

driftSamples :: Int
driftSamples = 10


data IndexRecord = IndexRecord
  { irId       :: !Text          -- ^ the @ORG_GLANCE_ID@ the record is keyed by.
  , irState    :: !Text          -- ^ TODO keyword verbatim; empty when the record names none.
  , irArchived :: !(Maybe Bool)  -- ^ 'Nothing' on a record written before the field existed.
  } deriving (Eq, Show)

-- | What this parser read at one blob: the file's FIRST headline, never a child's.
data BlobEntry = BlobEntry
  { beId       :: !Text
  , beState    :: !Text      -- ^ TODO keyword verbatim; empty when the headline has none.
  , beArchived :: !Bool      -- ^ does the headline wear org's @ARCHIVE@ tag?
  , beSalvaged :: !Bool      -- ^ was the id read off a drawer the parse refused ('Data.Org.Types.salvagedIdentity')?
  , beFile     :: !FilePath  -- ^ the blob, as walked.
  } deriving (Eq, Show)

-- | PATH's entry off HEADLINES — id, state, archived, salvaged, as the scan
-- spells each — the FIRST of them being the blob's.
blobEntryOf :: FilePath -> [(Maybe Text, Text, Bool, Bool)] -> Maybe BlobEntry
blobEntryOf path headlines = do
  (ident, state, arch, salvaged) <- listToMaybe headlines
  i <- ident
  pure (BlobEntry i state arch salvaged path)


data IndexFold = IndexFold
  { ifRecords    :: !(Map Text IndexRecord)  -- ^ id to its latest record, tombstoned ids removed.
  , ifRead       :: !Int  -- ^ records parsed across every live segment.
  , ifTombstones :: !Int  -- ^ ids whose LATEST record deletes them.
  , ifMalformed  :: !Int  -- ^ lines no record could be read out of; see the module note.
  } deriving (Eq, Show)

-- | The segment file NAMES to read in fold order, given the MANIFEST's bytes.
-- A name opens only when it spells @seg-\<digits\>.jsonl@, which is the path guard.
segmentNames :: Maybe BC.ByteString -> [FilePath]
segmentNames manifest = [ T.unpack n | n <- listed, sealedName n ] ++ [openSegment]
  where
    listed = case manifest >>= decodeStrict' of
      Just (Object o) | Just (Array vs) <- KM.lookup (Key.fromText "segments") o ->
        [ n | String n <- toList vs ]
      _noneListed -> []

sealedName :: Text -> Bool
sealedName name = case T.stripPrefix "seg-" name >>= T.stripSuffix ".jsonl" of
  Just digits -> not (T.null digits) && T.all isDigit digits
  Nothing     -> False

-- | The fold in progress: 'Nothing' where an id's latest record is a tombstone.
data Tally = Tally !(Map Text (Maybe IndexRecord)) !Int !Int

-- | Fold SEGMENTS — (is it the open one, its bytes), oldest first — to the live
-- set.  Bytes rather than 'Text': an invalid byte costs the LINE it sits on.
foldSegments :: [(Bool, BC.ByteString)] -> IndexFold
foldSegments = summarise . foldl' segment (Tally Map.empty 0 0)
  where
    segment acc (open, bytes) = foldl' line acc (marked (splitLines bytes))
      where
        -- The one forgivable failure: a crash cut the open segment's last append.
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

-- | LINE's id and record, 'Nothing' for a tombstone so one insert settles both.
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

-- | Elisp's @(eq t VALUE)@: only JSON @true@ is true, so the @{}@ for @nil@ is not.
flagOf :: Value -> Bool
flagOf (Bool b) = b
flagOf _other = False

-- | Is VALUE one elisp would call non-nil?  @{}@ is the one object that is false.
truthy :: Value -> Bool
truthy (Bool b) = b
truthy (Object o) = not (KM.null o)
truthy (String s) = not (T.null s)
truthy _other = True

splitLines :: BC.ByteString -> [BC.ByteString]
splitLines = filter (not . BC.null) . BC.split '\n'

-- | Does PATH name a file the tail listens to — the open segment, or the
-- MANIFEST a seal is committed by?  The seal itself is read off the inode.
tailedFile :: FilePath -> Bool
tailedFile path = takeFileName path `elem` [openSegment, manifestFile]

-- | The ids BYTES appends, and how many of its bytes those lines spend.  A torn
-- tail waits for its newline, which is 'foldSegments'' policy at the open
-- segment's end; a tombstone names its id like any other record, and a line no
-- record reads out of names none.
tailIds :: BC.ByteString -> ([Text], Int)
tailIds bytes = (mapMaybe (fmap fst . recordOf) (splitLines whole), BC.length whole)
  where whole = maybe BC.empty (\i -> BC.take (i + 1) bytes) (BC.elemIndexEnd '\n' bytes)

-- | WHICH FILE a tail was reading and how far into it.  The file carries the
-- seal — org-glance renames the segment away and touches a fresh one under the
-- name, and two segments may be the same size.
data Place = Place !DeviceID !FileID !Integer
  deriving (Eq, Show)

-- | 'Nothing' until a read has placed it.
type TailCursor = Maybe Place

-- | The cursor at SEGMENT's END, which is where a tail seeded at boot starts:
-- the walk has read every blob the bytes behind it name, so none is replayed.
-- 'Nothing' where the segment cannot be opened, there being nothing to skip.
segmentEnd :: FilePath -> IO TailCursor
segmentEnd segment =
  either unread Just <$> try (withFd segment ReadOnly defaultFileFlags placeOf)
  where
    unread :: IOException -> TailCursor
    unread _ = Nothing

-- | FD's device, inode and size off ONE stat: the seal and the end together.
placeOf :: Fd -> IO Place
placeOf fd = shape <$> getFdStatus fd
  where shape st = Place (deviceID st) (fileID st) (toInteger (fileSize st))

-- | The ids appended to SEGMENT past CURSOR, and where the cursor stands after.
-- A SEALED segment starts the read over at 0; a segment that cannot be read
-- leaves the cursor where it was.
tailFrom :: FilePath -> TailCursor -> IO ([Text], TailCursor)
tailFrom segment cursor =
  either unread landed <$> try (withFd segment ReadOnly defaultFileFlags grab)
  where
    grab fd = do
      Place dev ino end <- placeOf fd
      let from = if sealed dev ino end then 0 else maybe 0 offsetOf cursor
      _ <- fdSeek fd AbsoluteSeek (fromInteger from)
      (,) (Place dev ino from) <$> slurp fd (fromInteger (end - from))
    -- A fresh file under the name, one shorter than the offset, or a cursor no
    -- read has placed: each starts at the top.
    sealed dev ino end =
      maybe True (\(Place d i off) -> (d, i) /= (dev, ino) || end < off) cursor
    offsetOf (Place _ _ off) = off
    landed (Place dev ino from, bytes) =
      let (idents, spent) = tailIds bytes
      in (idents, Just (Place dev ino (from + toInteger spent)))
    unread :: IOException -> ([Text], TailCursor)
    unread _ = ([], cursor)

-- | K over PATH's descriptor, opened with MODE and FLAGS and closed however K
-- leaves.  NO GHC HANDLE LOCK is taken on the way: a handle locks its file
-- against every other handle in the process, and these are the PEER's files —
-- no reader of one may refuse org-glance its own write.
withFd :: FilePath -> OpenMode -> OpenFileFlags -> (Fd -> IO a) -> IO a
withFd path mode flags = bracket (openFd path mode flags) closeFd

-- | OWED bytes off FD, a short read looped over.
slurp :: Fd -> ByteCount -> IO BC.ByteString
slurp fd owed
  | owed <= 0 = pure BC.empty
  | otherwise = do
      chunk <- fdRead fd owed
      if BC.null chunk
        then pure chunk
        else (chunk <>) <$> slurp fd (owed - fromIntegral (BC.length chunk))


data IndexDrift = IndexDrift
  { dfStore       :: !FilePath    -- ^ the @.org-glance@ directory the index belongs to.
  , dfFold        :: !IndexFold
  , dfBlobs       :: !Int         -- ^ blobs the walk parsed under that store.
  , dfIdlessPaths :: ![FilePath]  -- ^ of those, the ones carrying no id to match by, path-ordered.
  , dfBrokenPaths :: ![FilePath]  -- ^ blobs whose id was read off a drawer the parse refused, path-ordered.
  , dfRows        :: !Int         -- ^ ids disagreeing in EITHER term.
  , dfState       :: !Int         -- ^ ids whose TODO keyword disagrees.
  , dfArchived    :: !Int         -- ^ ids whose archive flag disagrees, of those the record states.
  , dfUnindexed   :: !Int         -- ^ blobs no live record names.
  , dfRecordless  :: !Int         -- ^ live records with no blob.
  , dfSamples     :: ![Text]      -- ^ up to 'driftSamples' disagreements, id-ordered.
  } deriving (Eq, Show)

-- | Compare STORE's folded index against the BLOBS the walk parsed under it.
-- An idless blob is named ('dfIdlessPaths'), which keeps 'dfRecordless' honest.
driftOf :: FilePath -> IndexFold -> [(FilePath, Maybe BlobEntry)] -> IndexDrift
driftOf store folded blobs = IndexDrift
  { dfStore       = store
  , dfFold        = folded
  , dfBlobs       = length blobs
  , dfIdlessPaths = sort idless
  , dfBrokenPaths = sort broken
  , dfRows        = length disagreeing
  , dfState       = length [ () | (_, s, _) <- disagreeing, not (T.null s) ]
  , dfArchived    = length [ () | (_, _, a) <- disagreeing, not (T.null a) ]
  , dfUnindexed   = Map.size (Map.difference byId (ifRecords folded))
  , dfRecordless  = Map.size (Map.difference (ifRecords folded) byId)
  , dfSamples     = take driftSamples (concatMap sample disagreeing)
  }
  where
    idless = [ p | (p, Nothing) <- blobs ]
    broken = [ p | (p, Just b) <- blobs, beSalvaged b ]
    byId = Map.fromListWith (\_new old -> old) [ (beId b, b) | (_, Just b) <- blobs ]
    disagreeing = [ (i, state, arch)
                  | (i, rec) <- Map.toAscList (ifRecords folded)
                  , Just entry <- [Map.lookup i byId]
                  , let state = stateNote rec entry
                  , let arch = archiveNote rec entry
                  , not (T.null state && T.null arch) ]
    sample (i, state, arch) = [ i <> ": " <> note | note <- [state, arch], not (T.null note) ]

disagreement :: Eq a => Text -> (a -> Text) -> a -> a -> Text
disagreement field shown wal blob
  | wal == blob = ""
  | otherwise   = field <> " wal=" <> shown wal <> " blob=" <> shown blob

stateNote :: IndexRecord -> BlobEntry -> Text
stateNote rec blob = disagreement "state" shown (irState rec) (beState blob)
  where shown t = if T.null t then "none" else t

archiveNote :: IndexRecord -> BlobEntry -> Text
archiveNote rec blob =
  maybe "" (\flag -> disagreement "archived" yesNo flag (beArchived blob)) (irArchived rec)
  where yesNo b = if b then "true" else "false"


-- | DRIFT as the scan prints it: the verdict line, the rows of counts, each
-- count that names files followed by up to 'driftSamples' of them, then the samples.
indexReportLines :: IndexDrift -> [Text]
indexReportLines d = concat
  [ [ "org-glance index: " <> num (dfRows d) <> " rows disagree ("
        <> num (dfState d) <> " state, " <> num (dfArchived d) <> " archived)"
    , field "store" (T.pack (dfStore d))
    , field "records" (T.intercalate ", "
        [ num (ifRead folded) <> " read"
        , num (Map.size (ifRecords folded)) <> " live"
        , num (ifTombstones folded) <> " tombstones"
        , num (ifMalformed folded) <> " malformed" ])
    , field "blobs" (num (dfBlobs d) <> " parsed, "
                       <> num (length (dfIdlessPaths d)) <> " carrying no id") ]
  , listed (dfIdlessPaths d)
  , naming "drawers" (num (length broken) <> " with a drawer the parse refused, "
                        <> "the id read off the raw lines") broken
  , [ field "unmatched" (num (dfUnindexed d) <> " unindexed blobs, "
                            <> num (dfRecordless d) <> " records without blobs") ]
  , map ("  " <>) (dfSamples d)
  ]
  where
    folded = dfFold d
    broken = dfBrokenPaths d
    field label value = "  " <> T.justifyLeft 11 ' ' label <> value
    sampled = take driftSamples
    -- A count that names files prints WITH them; neither prints where there are none.
    naming label value paths = [ field label value | not (null paths) ] ++ listed paths
    listed paths = [ "    " <> T.pack p | p <- sampled paths ]
    num :: Int -> Text
    num = TS.showt
