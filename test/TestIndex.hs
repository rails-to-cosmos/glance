-- | The drift instrument: how org-glance's write-ahead index is folded, and
-- what the scan says when the fold and the blobs disagree.  The fold's cases
-- run over a real store, its whole content being WHICH FILES in WHICH ORDER.
module TestIndex (spec) where

import Control.Monad (filterM)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (takeDirectory, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestDefaults (withTempDirNamed)

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Org.Doctor (Corpus (..), Totals (..), scanCorpus)
import Data.Org.Index ( BlobEntry (..), IndexDrift (..), IndexFold (..)
                      , IndexRecord (..), blobEntryOf, driftOf, foldSegments
                      , indexReportLines, manifestFile, openSegment, segmentEnd
                      , segmentNames, tailFrom, tailIds, tailedFile )
import Data.Org.Walk (defaultWalk)
import Data.Org.Blob (blobPathIn, metaIn, storeRootIn)


-- | One JSON record line as org-glance writes it, @nil@ spelled @{}@.
record :: Text -> Text -> Text
record ident state =
  "{\"id\":\"" <> ident <> "\",\"state\":\"" <> state
    <> "\",\"title\":\"t\",\"tags\":[],\"hash\":\"h\",\"schedule\":{},\"deadline\":{}}"

-- | A record carrying the @archived@ field, which joined the schema late.
archivedRecord :: Text -> Text -> Bool -> Text
archivedRecord ident state flag =
  "{\"id\":\"" <> ident <> "\",\"state\":\"" <> state <> "\",\"archived\":"
    <> (if flag then "true" else "{}") <> "}"

tombstone :: Text -> Text
tombstone ident = "{\"id\":\"" <> ident <> "\",\"tombstone\":true}"

-- | Write a store's meta directory under DIR and answer the meta path.  OPEN
-- is written verbatim, so a caller can leave the torn tail a crash makes.
metaStore :: FilePath -> [[Text]] -> Text -> IO FilePath
metaStore dir sealed open = do
  createDirectoryIfMissing True meta
  names <- sequence [ segment i lines' | (i, lines') <- zip [1 :: Int ..] sealed ]
  writeFile (meta </> manifestFile) (manifest names)
  writeFile (meta </> openSegment) (T.unpack open)
  pure meta
  where
    meta = metaIn dir
    segment i lines' = name <$ writeFile (meta </> name) (T.unpack (T.unlines lines'))
      where name = "seg-" <> pad (show i) <> ".jsonl"
            pad s = replicate (10 - length s) '0' <> s
    manifest names =
      "{\"version\":2,\"segments\":["
        <> mconcat [ sep <> "\"" <> n <> "\"" | (sep, n) <- zip ("" : repeat ",") names ]
        <> "]}\n"

-- | Fold META the way the scan does, the open segment last and forgiven.
foldStore :: FilePath -> IO IndexFold
foldStore meta = do
  manifest <- readBytes (meta </> manifestFile)
  names <- filterM (doesFileExist . (meta </>)) (segmentNames (Just manifest))
  foldSegments <$> mapM (\n -> (,) (n == openSegment) <$> readBytes (meta </> n)) names

readBytes :: FilePath -> IO BC.ByteString
readBytes = BC.readFile

blob :: Text -> Text -> Bool -> (FilePath, Maybe BlobEntry)
blob ident state arch = (path, Just (BlobEntry ident state arch False path))
  where path = "/store/data/" <> T.unpack ident <> "/data.org"

-- | A blob whose id was read off a drawer the parse refused.
broken :: Text -> FilePath -> (FilePath, Maybe BlobEntry)
broken ident path = (path, Just (BlobEntry ident "" False True path))

-- | A blob no id was read out of: a fourth answer beside indexed and unindexed.
idless :: FilePath -> (FilePath, Maybe BlobEntry)
idless path = (path, Nothing)

-- | A headline as 'Data.Org.Doctor.indexTerms' spells it, its drawer parse-read.
term :: Maybe Text -> Text -> Bool -> (Maybe Text, Text, Bool, Bool)
term ident state arch = (ident, state, arch, False)

-- | Write DOC as the one blob of a store under DIR, then scan DIR as the doctor
-- does and answer the entry read out of it.
scannedBlob :: FilePath -> Text -> IO (Maybe BlobEntry)
scannedBlob dir doc = do
  createDirectoryIfMissing True blobDir
  TIO.writeFile (blobDir </> "data.org") doc
  entries <- reverse . tBlobs . coTotals <$> scanCorpus defaultWalk [dir]
  pure (case entries of (_, entry) : _ -> entry; [] -> Nothing)
  where blobDir = takeDirectory (blobPathIn (storeRootIn dir) "fe6180a2-ac42")

drift :: [IndexRecord] -> [(FilePath, Maybe BlobEntry)] -> IndexDrift
drift records = driftOf "/store" folded
  where folded = IndexFold (Map.fromList [ (irId r, r) | r <- records ]) (length records) 0 0


foldSpec :: TestTree
foldSpec = testGroup "Folding the write-ahead log"
  [ testCase "a later record supersedes an earlier one, whatever segment it is in" $
      withStore [[record "a" "TODO", record "b" "TODO"]]
                (T.unlines [record "a" "DONE"]) $ \folded -> do
        assertEqual "a" (Just "DONE") (stateOf "a" folded)
        assertEqual "b" (Just "TODO") (stateOf "b" folded)
        assertEqual "records read" 3 (ifRead folded)

    -- The open segment is read LAST, whatever order the MANIFEST is in.
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

    -- Elisp writes `nil' as `{}', so only JSON true is a set flag; an absent
    -- key predates the field and states nothing, a third answer.
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

    -- The MANIFEST is the commit protocol: a name that is not a sealed
    -- segment is not a file to open.
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

    -- Both sides spell "no keyword" empty, so the sample names the absence.
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

    -- A record from before the field says nothing and cannot be wrong.
  , testCase "a record that predates the field is never archive drift" $ do
      let d = drift [rec "a" "" Nothing] [blob "a" "" True]
      assertEqual "rows" 0 (dfRows d)
      assertEqual "archived" 0 (dfArchived d)

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

    -- The instrument turned on itself: an idless blob matches nothing, so a
    -- parser gap cannot read as index lag.
  , testCase "a blob carrying no id is counted apart and matches nothing" $ do
      let d = drift [rec "a" "" Nothing, rec "b" "" Nothing]
                    [blob "a" "" False, idless "/store/data/b/data.org"]
      assertEqual "blobs" 2 (dfBlobs d)
      assertEqual "idless" 1 (length (dfIdlessPaths d))
      assertEqual "unindexed" 0 (dfUnindexed d)
      assertEqual "recordless" 1 (dfRecordless d)
      assertEqual "rows" 0 (dfRows d)

    -- The count alone named none of them, so the file could not be found.
  , testCase "the idless and the broken-drawer blobs are named by path" $ do
      let d = drift [rec "a" "" Nothing]
                    [ blob "a" "" False, idless "/store/data/z/data.org"
                    , idless "/store/data/y/data.org", broken "r" "/store/data/x/data.org" ]
      assertEqual "idless" 2 (length (dfIdlessPaths d))
      assertEqual "idless paths, path-ordered"
                  ["/store/data/y/data.org", "/store/data/z/data.org"] (dfIdlessPaths d)
      assertEqual "broken paths" ["/store/data/x/data.org"] (dfBrokenPaths d)

    -- The drift holds every one of them; the REPORT is where the cap lives.
  , testCase "the named paths are capped where the samples are" $ do
      let d = drift [] [ idless ("/store/data/" <> show n <> "/data.org") | n <- [1 .. 30 :: Int] ]
          named = [ l | l <- indexReportLines d, "/store/data/" `T.isInfixOf` l ]
      assertEqual "idless paths held" 30 (length (dfIdlessPaths d))
      assertEqual "paths printed" 10 (length named)

  , testCase "the sample list is capped at ten" $ do
      let ids = [ T.pack (show n) | n <- [100 .. 199 :: Int] ]
          d = drift [ rec i "DONE" Nothing | i <- ids ] [ blob i "TODO" False | i <- ids ]
      assertEqual "rows" 100 (dfRows d)
      assertEqual "samples" 10 (length (dfSamples d))
  ]
  where rec = IndexRecord


blobSpec :: TestTree
blobSpec = testGroup "What a blob says"
  [ -- A blob holds one entry, and it is the file's first headline: six of
    -- ~/sync's blobs that open at level two, so a depth test would lose them.
    testCase "the first headline is the entry, whatever depth it opens at" $
      assertEqual "id" (Just "wanted")
                  (beId <$> blobEntryOf "b.org"
                     [term (Just "wanted") "DONE" True, term (Just "child") "" False])

  , testCase "its state and archive flag come off that headline" $ do
      let entry = blobEntryOf "b.org" [term (Just "i") "STARTED" True]
      assertEqual "state" (Just "STARTED") (beState <$> entry)
      assertEqual "archived" (Just True) (beArchived <$> entry)
      assertEqual "file" (Just "b.org") (beFile <$> entry)

    -- Never a CHILD's id, or a blob whose drawer this parser lost would be
    -- compared against a record describing its parent.
  , testCase "a child's id is not the blob's" $
      assertEqual "entry" Nothing
                  (beId <$> blobEntryOf "b.org"
                     [term Nothing "TODO" False, term (Just "child") "DONE" False])

  , testCase "a file with no headline at all is no entry" $
      assertEqual "entry" Nothing (beId <$> blobEntryOf "b.org" [])

  , testCase "the salvage mark comes off that headline too" $ do
      let entry = blobEntryOf "b.org" [(Just "raw", "TODO", False, True)]
      assertEqual "id" (Just "raw") (beId <$> entry)
      assertEqual "salvaged" (Just True) (beSalvaged <$> entry)

    -- END TO END over the real scan, which is what pins the tail arithmetic:
    -- a closer that lost its colon costs the parse the properties whole, and the
    -- id line is still bytes in the file.
  , testCase "a broken drawer's own id is the entry, marked as salvaged" $
      withTempDirNamed "scan-broken-drawer" $ \dir -> do
        entry <- scannedBlob dir
          "* TODO Broken :work:\n:PROPERTIES:\n:ORG_GLANCE_ID: u-1\n:END\nbody\n"
        assertEqual "id" (Just "u-1") (beId <$> entry)
        assertEqual "salvaged" (Just True) (beSalvaged <$> entry)
        assertEqual "state" (Just "TODO") (beState <$> entry)

    -- THE SALVAGED RUN IS THE DRAWER'S OWN: it opens whatever the indent, stops
    -- at either closer, and @:ENDORSED:@ is a property line rather than one.
  , testCase "the salvaged run opens at the indent and stops at the closer" $
      mapM_ (\(what, doc, want) ->
               withTempDirNamed "scan-drawer-run" $ \dir ->
                 assertEqual what want . fmap beId =<< scannedBlob dir doc)
        [ ( "an indent leaves the drawer a drawer"
          , "* TODO one\n  :PROPERTIES:\n  :ORG_GLANCE_ID: u-2\n  :END\nbody\n"
          , Just "u-2" )
        , ( "an id below the broken closer is no longer the drawer's"
          , "* TODO one\n:PROPERTIES:\n:CATEGORY: c\n:END\n:ORG_GLANCE_ID: past\nbody\n"
          , Nothing )
        , ( "nor below the whole one"
          , "* TODO one\n:PROPERTIES: stray\n:CATEGORY: c\n:END:\n:ORG_GLANCE_ID: past\nbody\n"
          , Nothing )
        , ( ":ENDORSED: is a property line and closes nothing"
          , "* TODO one\n:PROPERTIES:\n:ENDORSED: x\n:ORG_GLANCE_ID: u-3\n:END\nbody\n"
          , Just "u-3" ) ]

  , testCase "a drawer the parse read is taken at its word" $
      withTempDirNamed "scan-read-drawer" $ \dir -> do
        entry <- scannedBlob dir
          "* TODO Whole\n:PROPERTIES:\n:CATEGORY: c\n:END:\nbody\n"
        assertEqual "the path form went to a blob whose drawer parsed" Nothing entry
  ]


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

  , testCase "the idless and broken blobs are listed under their counts" $ do
      let ls = indexReportLines
                 (drift [] [ idless "/store/data/y/data.org"
                           , broken "r" "/store/data/x/data.org" ])
      assertBool ("the idless blob went unnamed in " <> show ls)
                 ("    /store/data/y/data.org" `elem` ls)
      hasLine ls "drawers" "1 with a drawer the parse refused"
      assertBool ("the broken-drawer blob went unnamed in " <> show ls)
                 ("    /store/data/x/data.org" `elem` ls)

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

-- | What the daemon's tail reads off bytes appended to the open segment: the
-- ids it hands the queue, and how many bytes it may then step over.
tailSpec :: TestTree
tailSpec = testGroup "Tailing the open segment"
  [ testCase "every complete line names its id, tombstones with the rest" $ do
      let ls = [record "a" "TODO", tombstone "b"]
          (idents, spent) = tailIds (bytes ls)
      assertEqual "ids" ["a", "b"] idents
      assertEqual "the whole of it is spent" (BC.length (bytes ls)) spent

    -- The one forgivable failure is a crash that cut the last append.  A tail
    -- WAITS on it instead: the bytes are still owed their newline.
  , testCase "a torn tail is left for the next read" $ do
      let ls = [record "a" "TODO"]
          torn = bytes ls <> BC.pack (T.unpack (record "b" "TODO"))
      assertEqual "the complete line alone" ["a"] (fst (tailIds torn))
      assertEqual "and the torn one is not stepped over"
                  (BC.length (bytes ls)) (snd (tailIds torn))

  , testCase "bytes with no newline in them at all name nothing and spend nothing" $
      assertEqual "nothing" ([], 0) (tailIds (BC.pack (T.unpack (record "a" "TODO"))))

  , testCase "a line no record reads out of names no id and is stepped over" $ do
      let ls = ["{oops", record "a" "TODO"]
          (idents, spent) = tailIds (bytes ls)
      assertEqual "ids" ["a"] idents
      assertEqual "both lines spent" (BC.length (bytes ls)) spent

  , testCase "only what is past the cursor is read, and it is read once" $
      withSegment $ \seg -> do
        TIO.writeFile seg (T.unlines [record "a" "TODO"])
        (first, at1) <- tailFrom seg Nothing
        assertEqual "the line" ["a"] first
        assertEqual "and the cursor stands at the segment's end" at1 =<< segmentEnd seg
        assertEqual "and nothing is owed twice" [] . fst =<< tailFrom seg at1
        TIO.appendFile seg (T.unlines [record "b" "TODO"])
        assertEqual "the append alone" ["b"] . fst =<< tailFrom seg at1

    -- Two segments may be the same LENGTH, so the size cannot carry the seal:
    -- a fresh file under the name is what starts the read over.
  , testCase "a segment sealed away and reopened at the same size starts over" $
      withSegment $ \seg -> do
        TIO.writeFile seg (T.unlines [record "a" "TODO"])
        (_first, at1) <- tailFrom seg Nothing
        renameFile seg (takeDirectory seg </> "seg-0000000001.jsonl")
        TIO.writeFile seg (T.unlines [record "b" "TODO"])
        assertEqual "the new file whole" ["b"] . fst =<< tailFrom seg at1

  , testCase "the two files a tail listens to, and no other" $ do
      assertBool "the open segment" (tailedFile ("/s/.org-glance/meta" </> openSegment))
      assertBool "the MANIFEST" (tailedFile ("/s/.org-glance/meta" </> manifestFile))
      assertBool "a sealed segment is folded at boot, never tailed"
                 (not (tailedFile "/s/.org-glance/meta/seg-0000000001.jsonl"))
      assertBool "and the notes this daemon writes are its own"
                 (not (tailedFile "/s/.org-glance/meta/EXTERNAL.jsonl"))
  ]
  where
    bytes ls = BC.pack (T.unpack (T.unlines ls))
    withSegment k = withTempDirNamed "index" (k . (</> openSegment))


spec :: TestTree
spec = testGroup "Index" [foldSpec, tailSpec, driftSpec, blobSpec, reportSpec]
