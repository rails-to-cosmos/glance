-- | org-glance's write-ahead index, read only, and the drift instrument over
-- it.  The fold is @org-glance-graph--latest-records@ read forwards.
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
  , beFile     :: !FilePath  -- ^ the blob, as walked.
  } deriving (Eq, Show)

blobEntryOf :: FilePath -> [(Maybe Text, Text, Bool)] -> Maybe BlobEntry
blobEntryOf path headlines = do
  (ident, state, arch) <- listToMaybe headlines
  i <- ident
  pure (BlobEntry i state arch path)


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

-- | Compare STORE's folded index against the BLOBS the walk parsed under it.
-- An idless blob is counted ('dfIdless'), which keeps 'dfRecordless' honest.
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


-- | DRIFT as the scan prints it: the verdict line, three rows of counts, samples.
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
