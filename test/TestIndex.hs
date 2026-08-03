-- | The drift instrument: how org-glance's write-ahead index is folded, and
-- what the scan says when the fold and the blobs disagree.
--
-- The fold's cases run over a real store written to a temp directory — a
-- MANIFEST, a sealed segment and an open one — because the fold's whole
-- content is WHICH FILES in WHICH ORDER, and a hand-built list of segments
-- would test the part that was never in doubt.  The comparison's cases build
-- their blob entries directly: the scan derives one from a headline it already
-- parsed, and putting a parser in the middle of these would only be able to
-- fail for reasons 'TestParser' owns.
module TestIndex (spec) where

import Control.Monad (filterM)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestDefaults (withTempDirNamed)

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Data.Org.Index ( BlobEntry (..), IndexDrift (..), IndexFold (..)
                      , IndexRecord (..), blobEntryOf, driftOf, foldSegments
                      , indexReportLines, manifestFile, openSegment, segmentNames )

-- Fixtures

-- | One JSON record line, as org-glance writes it: the two fields this reads,
-- and the empty objects it spells @nil@ with everywhere else.
record :: Text -> Text -> Text
record ident state =
  "{\"id\":\"" <> ident <> "\",\"state\":\"" <> state
    <> "\",\"title\":\"t\",\"tags\":[],\"hash\":\"h\",\"schedule\":{},\"deadline\":{}}"

-- | A record carrying the @archived@ field, which joined the schema late.
archivedRecord :: Text -> Text -> Bool -> Text
archivedRecord ident state flag =
  "{\"id\":\"" <> ident <> "\",\"state\":\"" <> state <> "\",\"archived\":"
    <> (if flag then "true" else "{}") <> "}"

-- | A deletion, which is what org-glance appends instead of rewriting a log.
tombstone :: Text -> Text
tombstone ident = "{\"id\":\"" <> ident <> "\",\"tombstone\":true}"

-- | Write a store's meta directory under DIR: SEALED as @seg-NNN.jsonl@ files
-- named by a MANIFEST, and OPEN as @headlines.jsonl@.  Answer the meta path.
--
-- OPEN is written verbatim, so a caller can leave its last line unterminated
-- and get the torn append a crash leaves behind.
metaStore :: FilePath -> [[Text]] -> Text -> IO FilePath
metaStore dir sealed open = do
  createDirectoryIfMissing True meta
  names <- sequence [ segment i lines' | (i, lines') <- zip [1 :: Int ..] sealed ]
  writeFile (meta </> manifestFile) (manifest names)
  writeFile (meta </> openSegment) (T.unpack open)
  pure meta
  where
    meta = dir </> ".org-glance" </> "meta"
    segment i lines' = name <$ writeFile (meta </> name) (T.unpack (T.unlines lines'))
      where name = "seg-" <> pad (show i) <> ".jsonl"
            pad s = replicate (10 - length s) '0' <> s
    manifest names =
      "{\"version\":2,\"segments\":["
        <> mconcat [ sep <> "\"" <> n <> "\"" | (sep, n) <- zip ("" : repeat ",") names ]
        <> "]}\n"

-- | Fold META the way the scan does: the MANIFEST's segments then the open one,
-- each read as bytes, the open one flagged so its torn tail is forgiven.
foldStore :: FilePath -> IO IndexFold
foldStore meta = do
  manifest <- readBytes (meta </> manifestFile)
  names <- filterM (doesFileExist . (meta </>)) (segmentNames (Just manifest))
  foldSegments <$> mapM (\n -> (,) (n == openSegment) <$> readBytes (meta </> n)) names

readBytes :: FilePath -> IO BC.ByteString
readBytes = BC.readFile

-- | A blob entry as the scan builds one, paired with the file it came from.
blob :: Text -> Text -> Bool -> (FilePath, Maybe BlobEntry)
blob ident state arch = (path, Just (BlobEntry ident state arch path))
  where path = "/store/data/" <> T.unpack ident <> "/data.org"

-- | A blob this parser read no id out of, which is a fourth answer beside
-- indexed, unindexed and recordless.
idless :: FilePath -> (FilePath, Maybe BlobEntry)
idless path = (path, Nothing)

-- | The drift between a fold of RECORDS and BLOBS, under a fixed store name.
drift :: [IndexRecord] -> [(FilePath, Maybe BlobEntry)] -> IndexDrift
drift records = driftOf "/store" folded
  where folded = IndexFold (Map.fromList [ (irId r, r) | r <- records ]) (length records) 0 0

-- The fold

foldSpec :: TestTree
foldSpec = testGroup "Folding the write-ahead log"
  [ testCase "a later record supersedes an earlier one, whatever segment it is in" $
      withStore [[record "a" "TODO", record "b" "TODO"]]
                (T.unlines [record "a" "DONE"]) $ \folded -> do
        assertEqual "a" (Just "DONE") (stateOf "a" folded)
        assertEqual "b" (Just "TODO") (stateOf "b" folded)
        assertEqual "records read" 3 (ifRead folded)

    -- The open segment is read LAST, so it wins over every sealed one however
    -- the MANIFEST is ordered.  Reverse the two and the table shows what the
    -- store held before the last edit.
  , testCase "the open segment is newer than every sealed one" $
      withStore [[record "a" "DONE"]] (T.unlines [record "a" "STARTED"]) $ \folded ->
        assertEqual "a" (Just "STARTED") (stateOf "a" folded)

  , testCase "sealed segments fold oldest-first, in MANIFEST order" $
      withStore [[record "a" "TODO"], [record "a" "PENDING"]] "" $ \folded ->
        assertEqual "a" (Just "PENDING") (stateOf "a" folded)

  , testCase "a tombstone takes its id out of the live set and is counted" $
      withStore [[record "a" "TODO", record "b" "TODO"]]
                (T.unlines [tombstone "a"]) $ \folded -> do
        assertEqual "live ids" ["b"] (Map.keys (ifRecords folded))
        assertEqual "tombstones" 1 (ifTombstones folded)
        assertEqual "records read" 3 (ifRead folded)

    -- The log is append-only, so a re-add is an ordinary later record.
  , testCase "a record after a tombstone brings the id back" $
      withStore [[record "a" "TODO"]]
                (T.unlines [tombstone "a", record "a" "DONE"]) $ \folded -> do
        assertEqual "a" (Just "DONE") (stateOf "a" folded)
        assertEqual "tombstones" 0 (ifTombstones folded)

    -- A crash can only tear the LAST append, and only in the open segment.
  , testCase "the open segment's torn final line is forgiven" $
      withStore [[record "a" "TODO"]] (record "b" "DONE" <> "\n{\"id\":\"c\",\"sta") $
        \folded -> do
          assertEqual "live ids" ["a", "b"] (Map.keys (ifRecords folded))
          assertEqual "malformed" 0 (ifMalformed folded)

  , testCase "a broken line anywhere else is counted as malformed" $
      withStore [[record "a" "TODO", "{\"id\":\"b\",\"sta"]] "" $ \folded -> do
        assertEqual "live ids" ["a"] (Map.keys (ifRecords folded))
        assertEqual "malformed" 1 (ifMalformed folded)

    -- Elisp writes `nil' as `{}', so only JSON true is a set flag -- the decode
    -- is `(eq t VALUE)'.  A record with no `archived' key at all predates the
    -- field and states nothing, which is a third answer rather than false.
  , testCase "the archive flag is true, false, or unstated" $
      withStore [] (T.unlines [ archivedRecord "yes" "" True
                              , archivedRecord "no" "" False
                              , record "old" "" ]) $ \folded -> do
        assertEqual "yes" (Just (Just True)) (archivedOf "yes" folded)
        assertEqual "no" (Just (Just False)) (archivedOf "no" folded)
        assertEqual "old" (Just Nothing) (archivedOf "old" folded)

  , testCase "a segment the MANIFEST does not name is invisible" $
      withTempDirNamed "index" $ \dir -> do
        meta <- metaStore dir [[record "a" "TODO"]] ""
        writeFile (meta </> "seg-0000000099.jsonl") (T.unpack (record "orphan" "TODO") <> "\n")
        folded <- foldStore meta
        assertEqual "live ids" ["a"] (Map.keys (ifRecords folded))

    -- The MANIFEST is the whole commit protocol, so a name it lists that is not
    -- a sealed segment is not a file to open.
  , testCase "the MANIFEST can only name seg-<digits>.jsonl" $ do
      assertEqual "traversal refused" [openSegment]
                  (segmentNames (Just "{\"segments\":[\"../../secrets\"]}"))
      assertEqual "sealed kept" ["seg-0000000013.jsonl", openSegment]
                  (segmentNames (Just "{\"segments\":[\"seg-0000000013.jsonl\"]}"))
      assertEqual "no manifest" [openSegment] (segmentNames Nothing)
      assertEqual "unparseable manifest" [openSegment] (segmentNames (Just "{oops"))
  ]
  where
    withStore sealed open k = withTempDirNamed "index" $ \dir ->
      k =<< foldStore =<< metaStore dir sealed open
    stateOf i folded = irState <$> Map.lookup i (ifRecords folded)
    archivedOf i folded = irArchived <$> Map.lookup i (ifRecords folded)

-- The comparison

driftSpec :: TestTree
driftSpec = testGroup "Index against blobs"
  [ testCase "a store that agrees disagrees about nothing" $ do
      let d = drift [rec "a" "DONE" Nothing, rec "b" "" Nothing]
                    [blob "a" "DONE" False, blob "b" "" False]
      assertEqual "rows" 0 (dfRows d)
      assertEqual "samples" [] (dfSamples d)

  , testCase "a keyword the blob spells differently is state drift" $ do
      let d = drift [rec "a" "DONE" Nothing] [blob "a" "TODO" False]
      assertEqual "rows" 1 (dfRows d)
      assertEqual "state" 1 (dfState d)
      assertEqual "archived" 0 (dfArchived d)
      assertEqual "sample" ["a: state wal=DONE blob=TODO"] (dfSamples d)

    -- Both sides spell "no keyword" as the empty string, so the sample has to
    -- name the absence rather than print nothing at all.
  , testCase "an absent keyword on either side reads as none" $ do
      let d = drift [rec "a" "" Nothing] [blob "a" "TODO" False]
      assertEqual "sample" ["a: state wal=none blob=TODO"] (dfSamples d)
      let e = drift [rec "b" "DONE" Nothing] [blob "b" "" False]
      assertEqual "sample" ["b: state wal=DONE blob=none"] (dfSamples e)

  , testCase "an archive flag the blob's tags contradict is archived drift" $ do
      let d = drift [rec "a" "" (Just False)] [blob "a" "" True]
      assertEqual "rows" 1 (dfRows d)
      assertEqual "state" 0 (dfState d)
      assertEqual "archived" 1 (dfArchived d)
      assertEqual "sample" ["a: archived wal=false blob=true"] (dfSamples d)

    -- The field joined the schema late, so a record from before it says
    -- nothing about archiving and cannot be wrong about it.
  , testCase "a record that predates the field is never archive drift" $ do
      let d = drift [rec "a" "" Nothing] [blob "a" "" True]
      assertEqual "rows" 0 (dfRows d)
      assertEqual "archived" 0 (dfArchived d)

    -- One row, two disagreements: the row is counted once and both are named.
  , testCase "a row can disagree in both terms at once" $ do
      let d = drift [rec "a" "DONE" (Just True)] [blob "a" "TODO" False]
      assertEqual "rows" 1 (dfRows d)
      assertEqual "state" 1 (dfState d)
      assertEqual "archived" 1 (dfArchived d)
      assertEqual "samples" [ "a: state wal=DONE blob=TODO"
                            , "a: archived wal=true blob=false" ] (dfSamples d)

  , testCase "a blob no record names is unindexed" $ do
      let d = drift [rec "a" "" Nothing] [blob "a" "" False, blob "loose" "" False]
      assertEqual "unindexed" 1 (dfUnindexed d)
      assertEqual "recordless" 0 (dfRecordless d)
      assertEqual "rows" 0 (dfRows d)

  , testCase "a record with no blob is recordless and never compared" $ do
      let d = drift [rec "a" "" Nothing, rec "gone" "DONE" Nothing] [blob "a" "" False]
      assertEqual "recordless" 1 (dfRecordless d)
      assertEqual "unindexed" 0 (dfUnindexed d)
      assertEqual "rows" 0 (dfRows d)

    -- The instrument turned on itself: a blob this parser read but found no id
    -- in cannot be matched at all, so it is counted on its own and the record
    -- naming it stays recordless.  Reported, because otherwise a parser gap
    -- reads as org-glance indexing something that is not there.
  , testCase "a blob carrying no id is counted apart and matches nothing" $ do
      let d = drift [rec "a" "" Nothing, rec "b" "" Nothing]
                    [blob "a" "" False, idless "/store/data/b/data.org"]
      assertEqual "blobs" 2 (dfBlobs d)
      assertEqual "idless" 1 (dfIdless d)
      assertEqual "unindexed" 0 (dfUnindexed d)
      assertEqual "recordless" 1 (dfRecordless d)
      assertEqual "rows" 0 (dfRows d)

  , testCase "the sample list is capped at ten" $ do
      let ids = [ T.pack (show n) | n <- [100 .. 199 :: Int] ]
          d = drift [ rec i "DONE" Nothing | i <- ids ] [ blob i "TODO" False | i <- ids ]
      assertEqual "rows" 100 (dfRows d)
      assertEqual "samples" 10 (length (dfSamples d))
  ]
  where rec = IndexRecord

-- The blob's own entry

blobSpec :: TestTree
blobSpec = testGroup "What a blob says"
  [ -- A blob holds one entry, and it is the file's first headline: six of
    -- ~/sync's blobs open at level two, so a depth test would lose them.
    testCase "the first headline is the entry, whatever depth it opens at" $
      assertEqual "id" (Just "wanted")
                  (beId <$> blobEntryOf "b.org"
                     [(Just "wanted", "DONE", True), (Just "child", "", False)])

  , testCase "its state and archive flag come off that headline" $ do
      let entry = blobEntryOf "b.org" [(Just "i", "STARTED", True)]
      assertEqual "state" (Just "STARTED") (beState <$> entry)
      assertEqual "archived" (Just True) (beArchived <$> entry)
      assertEqual "file" (Just "b.org") (beFile <$> entry)

    -- Never a CHILD's id.  A blob whose own drawer this parser lost would
    -- otherwise be compared under its child's id, against a record describing
    -- the parent -- a disagreement invented by the instrument.
  , testCase "a child's id is not the blob's" $
      assertEqual "entry" Nothing
                  (beId <$> blobEntryOf "b.org"
                     [(Nothing, "TODO", False), (Just "child", "DONE", False)])

  , testCase "a file with no headline at all is no entry" $
      assertEqual "entry" Nothing (beId <$> blobEntryOf "b.org" [])
  ]

-- The report

reportSpec :: TestTree
reportSpec = testGroup "The scan's index report"
  [ testCase "the headline line carries the three counts" $ do
      let d = drift [rec "a" "DONE" (Just True), rec "b" "TODO" Nothing]
                    [blob "a" "TODO" False, blob "b" "TODO" False]
      assertEqual "headline"
                  "org-glance index: 1 rows disagree (1 state, 1 archived)"
                  (head (indexReportLines d))

  , testCase "the count rows name the store, the fold and what went unmatched" $ do
      let d = drift [rec "a" "" Nothing, rec "gone" "" Nothing]
                    [blob "a" "" False, blob "loose" "" False, idless "/store/data/x/data.org"]
          ls = indexReportLines d
      hasLine ls "store" "/store"
      hasLine ls "records" "2 read, 2 live, 0 tombstones, 0 malformed"
      hasLine ls "blobs" "3 parsed, 1 carrying no id"
      hasLine ls "unmatched" "1 unindexed blobs, 1 records without blobs"

  , testCase "an agreeing store reports itself and samples nothing" $ do
      let ls = indexReportLines (drift [rec "a" "" Nothing] [blob "a" "" False])
      assertEqual "lines" 5 (length ls)
      assertEqual "headline"
                  "org-glance index: 0 rows disagree (0 state, 0 archived)" (head ls)
  ]
  where
    rec = IndexRecord
    hasLine ls label want =
      assertBool (T.unpack label <> " row missing " <> T.unpack want <> " in " <> show ls)
                 (any (\l -> label `T.isInfixOf` l && want `T.isInfixOf` l) ls)

spec :: TestTree
spec = testGroup "Index" [foldSpec, driftSpec, blobSpec, reportSpec]
