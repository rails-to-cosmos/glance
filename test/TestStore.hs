-- | The live store: what one file's edit turns into on the wire.  The frames a
-- socket would carry are 'applyFile''s, tested where they are computed.
module TestStore (spec) where

import Control.Concurrent.STM (STM, atomically, orElse, readTVarIO)
import Control.Monad (replicateM_)
import Data.Aeson (Value (Object, String))
import Data.Maybe (listToMaybe)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import TestDefaults
import TestWire (drainNow)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Glance.Query ( HeadlineRecord (hrDigest, hrFile, hrId, hrLinked, hrTitle)
                    , IdCollision (..)
                    , LoadFailure (..), QueryResult (..), TodoKeywords (..)
                    , WalkOptions (..), defaultWalk, loadDir, loadDirWith, loadFile
                    , noConfig, replaceSpans, rowJSON, setStateEdits, subtreeText )
import Glance.Web.Store ( Client, CloseReason (..), Frame (..), Hub (hubPending, hubStore)
                        , RowOp (..)
                        , Store (stGen, stPrint), applyFile, bootstrapFrame
                        , clientCapacity, dropFile, emptyStore, frameJSON, loadStore
                        , loadStoreWith, newHub, nextFrame, publish, storeKeywords
                        , storeRecords, storeResult, storeTags, subscribe )
import Glance.Web.Watch (debounceDelay, due, isWatchable, nudge, watched)

-- Scaffolding

-- | Run K over a store of FILES, handed the directory, the FIRST path and the store.
withStoreOf :: [(FilePath, T.Text)] -> (FilePath -> FilePath -> Store -> IO a) -> IO a
withStoreOf files k = withTempDir $ \dir -> do
  paths <- mapM (uncurry (orgFile dir)) files
  case paths of
    path : _ -> k dir path =<< loadStore dir
    []       -> assertFailure "withStoreOf: a store of no files says nothing"

-- | Rewrite PATH with TEXT and fold the re-read into STORE: the watch's whole step.
rewrite :: FilePath -> T.Text -> Store -> IO (Store, [Frame])
rewrite path text store = do
  TIO.writeFile path text
  applyFile path <$> loadFile path <*> pure store

-- | The one record STORE holds, LABEL naming the moment in the failure.
oneRecord :: String -> Store -> IO HeadlineRecord
oneRecord label store = case storeRecords store of
  [r]  -> pure r
  recs -> assertFailure (label <> ": expected one record, got " <> show (map hrId recs))

-- | The rows STORE holds for PATH: its own answer, narrowed to one file.
rowsUnder :: FilePath -> Store -> [HeadlineRecord]
rowsUnder path store = [ r | r <- storeRecords store, hrFile r == path ]

-- | Ids the frames touch, upserts and deletes apart.
upsertIds, deleteIds :: [Frame] -> [T.Text]
upsertIds frames = [ i | Op (UpsertRow row) <- frames, Just i <- [stringAt "id" row] ]
deleteIds frames = [ i | Op (DeleteRow i) <- frames ]

-- | The title cell of every row the frames upsert.
upsertTitles :: [Frame] -> [T.Text]
upsertTitles frames = [ t | Op (UpsertRow row) <- frames, Just t <- [cellIn "title" row] ]

-- | KEY's cell of a row object, when it holds a string there.
cellIn :: T.Text -> Value -> Maybe T.Text
cellIn key (Object o) = KM.lookup "cells" o >>= stringAt key
cellIn _key _row      = Nothing

-- | A row object's whole @cells@ object.
cellsOf :: Value -> Maybe Value
cellsOf (Object o) = KM.lookup "cells" o
cellsOf _row       = Nothing

-- | A store step that changes nothing and streams FRAMES.
streaming :: [Frame] -> Store -> (Store, [Frame])
streaming frames st = (st, frames)

-- | V's KEY, when V is an object holding a string there.
stringAt :: T.Text -> Value -> Maybe T.Text
stringAt key (Object o) = case KM.lookup (Key.fromText key) o of
  Just (String s) -> Just s
  _other          -> Nothing
stringAt _key _v = Nothing

-- Spec

spec :: TestTree
spec = testGroup "Store"
  [ diffSpec, failureSpec, generationSpec, fingerprintSpec, keywordSpec, tagSpec
  , derivedSpec, sidecarSpec, sharedSpec, bootstrapSpec, hubSpec, debounceSpec
  , nudgeSpec ]

-- | Emacs's sidecars, which the walk and the watch have to refuse together.
sidecarSpec :: TestTree
sidecarSpec = testGroup "Editor sidecars"
  [ testCase "a dangling lock symlink is walked over, not read" $ withTempDir $ \dir -> do
      _ <- orgFile dir "notes.org" "* TODO one\n"
      -- What Emacs leaves beside an open buffer: a dangling lock and an auto-save.
      createSymbolicLink "dmitry@host.4242:1750000000" (dir </> ".#notes.org")
      _ <- orgFile dir "#notes.org#" "* TODO an auto-save\n"
      qr <- loadDir dir
      assertEqual "files" 1 (qrFiles qr)
      assertEqual "rows" 1 (length (qrRecords qr))
      assertEqual "read failures" 0 (qrReadFailures qr)
      st <- loadStore dir
      assertEqual "the store agrees" 0 (qrReadFailures (storeResult st))
      assertEqual "and holds the one document" 1 (qrFiles (storeResult st))

  , testCase "and the watch refuses exactly what the walk did" $ withTempDir $ \dir -> do
      _ <- orgFile dir "notes.org" "* TODO one\n"
      createSymbolicLink "dmitry@host.4242:1750000000" (dir </> ".#notes.org")
      _ <- orgFile dir "#notes.org#" "* TODO an auto-save\n"
      qr <- loadDir dir
      -- One rule, so a file cannot be loaded and unwatchable, or watched and unloaded.
      mapM_ ((\p -> assertBool (p <> ": walked and not watched") (isWatchable p)) . hrFile)
            (qrRecords qr)
      mapM_ (\name -> assertBool (name <> ": watched though not walked")
                                 (not (isWatchable (dir </> name))))
            [".#notes.org", "#notes.org#"]
  ]

-- | What the wire carries where two rows claim one id, resolved as answers are.
sharedSpec :: TestTree
sharedSpec = testGroup "Shared id"
  [ testCase "an edit to the losing file streams nothing over the winner"
      $ withShared $ \_pa pb store -> do
      -- Neither path is canonical, so walk order gives a.org the id.
      (next, frames) <- rewrite pb (entryAs "shared" "TODO from b, edited") store
      assertEqual "frames" [] frames
      assertEqual "the served row is still the winner's" ["from a"]
                  (map hrTitle (storeRecords next))

  , testCase "an edit to the winning file streams the winner's new cells"
      $ withShared $ \pa _pb store -> do
      (next, frames) <- rewrite pa (entryAs "shared" "TODO from a, edited") store
      assertEqual "upserts" ["shared"] (upsertIds frames)
      assertEqual "the cells" ["from a, edited"] (upsertTitles frames)
      assertEqual "which is what is served" (map hrTitle (storeRecords next))
                  (upsertTitles frames)

  , testCase "the winning file going away re-points the id at the loser's row"
      $ withShared $ \pa _pb store -> do
      removeFile pa
      let (next, frames) = dropFile pa store
      -- The id is still carried, so it is one upsert rather than a deletion.
      assertEqual "deletes" [] (deleteIds frames)
      assertEqual "upserts" ["shared"] (upsertIds frames)
      assertEqual "the cells" ["from b"] (upsertTitles frames)
      assertEqual "which is what is served" ["from b"] (map hrTitle (storeRecords next))

  , testCase "two headlines of one file sharing an id are one row, the first"
      $ withStoreOf [("a.org", entryAs "dup" "TODO alpha" <> entryAs "dup" "TODO omega")]
      $ \_dir path store -> do
      -- Within one file no path outranks the other, so the incumbent stands.
      assertEqual "one row for the id" ["alpha"] (map hrTitle (storeRecords store))
      (next, frames) <- rewrite path
        (entryAs "dup" "TODO alpha edited" <> entryAs "dup" "TODO omega edited") store
      assertEqual "the streamed row is the served row"
                  [ Op (UpsertRow (rowJSON r)) | r <- storeRecords next ] frames
      assertEqual "which is the first of the two" ["alpha edited"]
                  (map hrTitle (storeRecords next))
  ]

-- | Two files claiming @shared@; a.org wins it on walk order.
withShared :: (FilePath -> FilePath -> Store -> IO a) -> IO a
withShared k =
  withStoreOf [ ("a.org", entryAs "shared" "TODO from a")
              , ("b.org", entryAs "shared" "TODO from b") ] $ \dir pa store -> do
    assertEqual "one row for the id" ["from a"] (map hrTitle (storeRecords store))
    k pa (dir </> "b.org") store

-- | The other half of the @ETag@: which tree the store was loaded from.
fingerprintSpec :: TestTree
fingerprintSpec = testGroup "Fingerprint"
  [ testCase "two loads of one unchanged tree print the same"
      $ withStoreOf [("a.org", "* TODO one\n"), ("b.org", "* NEXT two\n")]
      $ \dir _path first' -> do
      second' <- loadStore dir
      assertBool "no fingerprint at all" (not (T.null (stPrint first')))
      assertEqual "fingerprint" (stPrint first') (stPrint second')

  , testCase "a byte of difference prints differently"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \dir path before -> do
      _ <- orgFile dir "a.org" "* TODO one!\n"
      after <- loadStore dir
      assertBool ("one fingerprint for two trees: " <> path)
                 (stPrint before /= stPrint after)

  , testCase "and so does a file renamed under the same content"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \dir path before -> do
      -- An id-less row id is FILE#K, so the path IS part of the answer.
      _ <- orgFile dir "b.org" "* TODO one\n"
      removeFile path
      after <- loadStore dir
      assertBool "renamed, same fingerprint" (stPrint before /= stPrint after)

  , testCase "and so does the same tree served from another root"
      $ withTempDir $ \one -> withTempDir $ \two -> do
      mapM_ (\dir -> orgFile dir "a.org" "* TODO one\n") [one, two]
      first' <- loadStore one
      second' <- loadStore two
      assertBool "two roots, one fingerprint" (stPrint first' /= stPrint second')

  , testCase "an edit moves the generation and leaves the fingerprint"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \_dir path store -> do
      -- The pair is the tag; a print per edit would cost a fold over every file.
      (next, frames) <- rewrite path "* TODO one\n* TODO two\n" store
      assertBool "a new headline is a row change" (not (null frames))
      assertBool "generation stuck" (stGen next > stGen store)
      assertEqual "fingerprint" (stPrint store) (stPrint next)
  ]

-- | The update counter @GET \/headlines@ spells as an @ETag@: it moves whenever
-- a response would, and stays put whenever none would.
generationSpec :: TestTree
generationSpec = testGroup "Generation"
  [ testCase "a file nothing wrote leaves it where it was" $
      let doc = "* TODO one\n" in
      withStoreOf [("a.org", doc)] $ \_dir path store -> do
      (same, frames) <- rewrite path doc store
      assertEqual "frames" [] frames
      assertEqual "generation" (stGen store) (stGen same)

  , testCase "a changed row moves it"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \_dir path store -> do
      (next, frames) <- rewrite path "* TODO one\n* TODO two\n" store
      assertBool "a new headline is a row change" (not (null frames))
      assertBool ("generation stuck at " <> show (stGen next)) (stGen next > stGen store)

  , testCase "so does a load outcome, with no row to show for it"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \_dir path store -> do
      -- The load counts ride the same tag as the rows, so an outcome invalidates too.
      let (broken, frames) = applyFile path (Left ParseFailed) store
      assertEqual "frames" [] frames
      assertEqual "rows kept" (map hrId (storeRecords store)) (map hrId (storeRecords broken))
      assertEqual "parse failures" 1 (qrParseFailures (storeResult broken))
      assertBool ("generation stuck at " <> show (stGen broken)) (stGen broken > stGen store)

  , testCase "and so does the same file parsing again"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \_dir path store -> do
      let (broken, _f) = applyFile path (Left ParseFailed) store
      let (fixed, frames) = applyFile path (Right (storeRecords store)) broken
      assertEqual "frames" [] frames
      assertEqual "parse failures cleared" 0 (qrParseFailures (storeResult fixed))
      assertBool ("generation stuck at " <> show (stGen fixed)) (stGen fixed > stGen broken)
  ]

-- | The tag vocabulary, kept beside the rows so a query costs no fold over them.
tagSpec :: TestTree
tagSpec = testGroup "Tag vocabulary"
  [ testCase "is every distinct tag the loaded rows carry" $
      let doc = "* TODO one :web:glance:\n* NEXT two :web:\n* DONE three\n" in
      withStoreOf [("a.org", doc)] $ \_dir _path st ->
      assertEqual "tags" ["glance", "web"] (storeTags st)

  , testCase "a re-read file adds its new tags and drops the ones it lost"
      $ withStoreOf [("a.org", "* TODO one :web:\n")] $ \_dir path st -> do
      assertEqual "before" ["web"] (storeTags st)
      (next, _frames) <- rewrite path "* TODO one :inbox:\n" st
      assertEqual "after" ["inbox"] (storeTags next)

  , testCase "a tag two files carry survives one of them going"
      $ withStoreOf [("a.org", "* TODO one :web:\n"), ("b.org", "* TODO two :web:\n")]
      $ \_dir pa st -> do
      let (gone, _frames) = dropFile pa st
      assertEqual "still declared" ["web"] (storeTags gone)

  , testCase "the vocabulary moves only where the generation does"
      $ withStoreOf [("a.org", "* TODO one :web:\n")] $ \_dir path st -> do
      (same, _f) <- rewrite path "* TODO one :web:\n" st
      assertEqual "no rewrite, no move" (storeTags st) (storeTags same)
      (next, frames) <- rewrite path "* TODO one :web:inbox:\n" st
      assertBool "a tag change is a row change" (not (null frames))
      assertEqual "and the vocabulary followed" ["inbox", "web"] (storeTags next)
  ]

-- | org-glance's derived mirrors: walked past, their ids resolved canonically.
derivedSpec :: TestTree
derivedSpec = testGroup "Derived mirrors"
  [ testCase "the mirror directories are not walked, and data is"
      $ withMirrorTree $ \dir -> do
      qr <- loadDir dir
      assertEqual "files" 2 (qrFiles qr)
      assertEqual "rows" 2 (length (qrRecords qr))
      assertEqual "no collision to resolve" [] (qrIdCollisions qr)
      assertBool "a mirror row was served"
                 (not (any (("overviews" `elem`) . splitOn . hrFile) (qrRecords qr)))

  , testCase "--include-derived walks them, and the canonical row wins the id"
      $ withMirrorTree $ \dir -> do
      qr <- loadDirWith (WalkOptions True) dir
      assertEqual "files" 4 (qrFiles qr)
      -- Four files, four headlines, but two of them claim one id.
      assertEqual "rows" 3 (length (qrRecords qr))
      assertEqual "collisions" 1 (length (qrIdCollisions qr))
      c <- maybe (assertFailure "no collision to inspect") pure
                 (listToMaybe (qrIdCollisions qr))
      assertEqual "the id" "shared-id" (icId c)
      assertBool ("kept the canonical file: " <> icKept c)
                 ("data" `elem` splitOn (icKept c))
      assertBool ("dropped the mirror: " <> icDropped c)
                 ("overviews" `elem` splitOn (icDropped c))

  , testCase "a mirror named as a root is still a mirror"
      $ withMirrorTree $ \dir -> do
      -- The exclusion is a property of the PATH rather than of the descent.
      let meta = dir </> ".org-glance" </> "meta"
      qr <- loadDirWith defaultWalk meta
      assertEqual "files" 0 (qrFiles qr)
      assertEqual "rows" 0 (length (qrRecords qr))
      opened <- loadDirWith (WalkOptions True) meta
      assertEqual "files with --include-derived" 1 (qrFiles opened)
      assertEqual "rows with --include-derived" 1 (length (qrRecords opened))

  , testCase "the store resolves it the same way the load does"
      $ withMirrorTree $ \dir -> do
      qr <- loadDir dir
      st <- loadStore dir
      assertEqual "the default store skips them" 2 (length (storeRecords st))
      assertEqual "one row per id" (map hrId (qrRecords qr)) (map hrId (storeRecords st))

  , testCase "and resolves the shared id, where there is one to resolve"
      $ withMirrorTree $ \dir -> do
      qr <- loadDirWith (WalkOptions True) dir
      st <- loadStoreWith (WalkOptions True) dir
      assertEqual "rows" 3 (length (storeRecords st))
      assertEqual "one row per id" (map hrId (qrRecords qr)) (map hrId (storeRecords st))
      assertEqual "out of the same files" (map hrFile (qrRecords qr))
                  (map hrFile (storeRecords st))

  , testCase "a watch event under a mirror is not one this store reads" $ do
      let mirror = "/o/.org-glance/overviews/c1f3/overview.org"
          canonical = "/o/.org-glance/data/ed/ucation/data.org"
      assertBool "the mirror is watchable as a file" (isWatchable mirror)
      assertBool "and still not watched" (not (watched defaultWalk mirror))
      assertBool "the canonical store is" (watched defaultWalk canonical)
      assertBool "a plain file is" (watched defaultWalk "/o/notes.org")
      assertBool "--include-derived takes the mirror too"
                 (watched (WalkOptions True) mirror)
      assertBool "a lock file is not a document"
                 (not (watched (WalkOptions True) "/o/.org-glance/overviews/.#a.org"))
  ]
  where splitOn = foldr step [[]]
          where step '/' acc = [] : acc
                step c (seg : rest) = (c : seg) : rest
                step _ [] = []

-- | A tree shaped like org-glance's, two mirrors repeating one headline's id.
withMirrorTree :: (FilePath -> IO a) -> IO a
withMirrorTree k = withTempDir $ \dir -> do
  let shared = "* TODO Курс :study:\n:PROPERTIES:\n:ORG_GLANCE_ID: shared-id\n:END:\n"
  _ <- orgFile dir "notes.org" "* TODO a plain note\n"
  createDirectoryIfMissing True (dir </> ".org-glance" </> "data" </> "ed")
  createDirectoryIfMissing True (dir </> ".org-glance" </> "overviews" </> "c1f3")
  createDirectoryIfMissing True (dir </> ".org-glance" </> "meta")
  _ <- orgFile (dir </> ".org-glance" </> "data" </> "ed") "data.org" shared
  _ <- orgFile (dir </> ".org-glance" </> "overviews" </> "c1f3") "overview.org" shared
  _ <- orgFile (dir </> ".org-glance" </> "meta") "agenda.org" "* TODO an agenda render\n"
  k dir

-- | One file re-read, and the frames the difference implies.
diffSpec :: TestTree
diffSpec = testGroup "File diff"
  [ testCase "a file that did not change streams nothing" $
      let doc = "#+CATEGORY: notes\n* TODO one\n* NEXT two :tag:\n" in
      withStoreOf [("a.org", doc)] $ \_dir path store -> do
      -- Over an empty store the claim below is met by an empty diff of nothing.
      assertEqual "the fixture's rows" 2 (length (storeRecords store))
      assertEqual "frames" [] . snd =<< rewrite path doc store

    -- A child is no row, so an edit under one moves no cell; the file is still
    -- re-read, so a materialize after it pins the digest the file now has.
  , testCase "an edit under a child streams nothing and still refreshes the entry" $
      withStoreOf [("a.org", tree "first\n")] $ \_dir path store -> do
      was <- oneRecord "before" store
      (next, frames) <- rewrite path (tree "second\n") store
      assertEqual "frames" [] frames
      assertEqual "the generation stays put, so the ETag does" (stGen store) (stGen next)
      now <- oneRecord "after" next
      assertEqual "the row is the row it was" (rowJSON was) (rowJSON now)
      assertBool "the digest did not follow the file"
                 (hrDigest was /= hrDigest now)
      assertBool ("the subtree is stale: " <> show (subtreeText now))
                 ("second" `T.isInfixOf` subtreeText now)

    -- `linked' is a row FIELD off the whole subtree, so a child moves it with no cell.
  , testCase "unless that edit gives the subtree its first link" $
      withStoreOf [("a.org", tree "first\n")] $ \_dir path store -> do
        was <- oneRecord "before" store
        (next, frames) <- rewrite path (tree "see https://x.example\n") store
        assertEqual "upserts" ["one"] (upsertIds frames)
        assertBool "the generation moves with the frame" (stGen next > stGen store)
        now <- oneRecord "after" next
        assertEqual "the row was not linked" False (hrLinked was)
        assertEqual "and now is" True (hrLinked now)
        assertEqual "while every cell stayed where it was"
                    (cellsOf (rowJSON was)) (cellsOf (rowJSON now))

  , testCase "a new headline is one upsert"
      $ withStoreOf [("a.org", entry "one")] $ \_dir path store -> do
      (next, frames) <- rewrite path (entry "one" <> entry "two") store
      assertEqual "upserts" ["two"] (upsertIds frames)
      assertEqual "deletes" [] (deleteIds frames)
      assertEqual "rows" ["one", "two"] (map hrId (storeRecords next))

  , testCase "an edited title keeps the id the file gave it"
      $ withStoreOf [("a.org", entryAs "one" "TODO first")] $ \_dir path store -> do
      (_next, frames) <- rewrite path (entryAs "one" "DONE first") store
      assertEqual "upserts" ["one"] (upsertIds frames)
      assertEqual "deletes" [] (deleteIds frames)
      expected <- recordsOf path
      assertEqual "row" [Op (UpsertRow (rowJSON r)) | r <- expected] frames

  , testCase "a removed headline is one delete"
      $ withStoreOf [("a.org", entry "one" <> entry "two")] $ \_dir path store -> do
      (next, frames) <- rewrite path (entry "one") store
      assertEqual "upserts" [] (upsertIds frames)
      assertEqual "deletes" ["two"] (deleteIds frames)
      assertEqual "rows" ["one"] (map hrId (storeRecords next))

  -- An id-less row is FILE#K over the TOP ENTRIES, so bytes above it are no part of it.
  , testCase "text inserted above an id-less row leaves its id alone"
      $ withStoreOf [("a.org", twoEntries)] $ \_dir path store -> do
      let before = map hrId (storeRecords store)
      (next, frames) <- rewrite path "#+TITLE: notes\n* TODO one\n* TODO two\n" store
      assertEqual "the ids stand" before (map hrId (storeRecords next))
      assertEqual "so nothing is deleted" [] (deleteIds frames)
      assertEqual "and nothing is reinserted" [] (upsertIds frames)

  , testCase "and an edit to one row is that row alone"
      $ withStoreOf [("a.org", twoEntries)] $ \_dir path store -> do
      let before = map hrId (storeRecords store)
      (next, frames) <- rewrite path "* TODO one\n  a body line\n* DONE two\n" store
      assertEqual "the ids stand" before (map hrId (storeRecords next))
      assertEqual "one upsert, the row that moved" (drop 1 before) (upsertIds frames)
      assertEqual "no deletes" [] (deleteIds frames)

  -- What renumbers a file is its TOP ENTRIES moving, and a swap moves two.
  , testCase "swapping two id-less entries renumbers both"
      $ withStoreOf [("a.org", twoEntries)] $ \_dir path store -> do
      let before = map hrId (storeRecords store)
      (next, frames) <- rewrite path "* TODO two\n* TODO one\n" store
      assertEqual "the ids are the same set" before (map hrId (storeRecords next))
      assertEqual "and both rows are re-sent under them" before (upsertIds frames)
      assertEqual "nothing is deleted" [] (deleteIds frames)
      assertEqual "the row at #0 is the other headline"
                  ["two", "one"] (map hrTitle (storeRecords next))

  , testCase "a new first entry renumbers the rows behind it"
      $ withStoreOf [("a.org", twoEntries)] $ \_dir path store -> do
      let before = map hrId (storeRecords store)
      (next, frames) <- rewrite path "* TODO zero\n* TODO one\n* TODO two\n" store
      let after = map hrId (storeRecords next)
      assertEqual "the old ids all survive, and #2 joins them"
                  (before <> [T.pack path <> "#2"]) after
      assertEqual "every row is (re)sent" after (upsertIds frames)
      assertEqual "and none is deleted" [] (deleteIds frames)
      assertEqual "each id names the headline one place later"
                  ["zero", "one", "two"] (map hrTitle (storeRecords next))

  , testCase "an entry appended after them all costs one upsert"
      $ withStoreOf [("a.org", twoEntries)] $ \_dir path store -> do
      (next, frames) <- rewrite path "* TODO one\n* TODO two\n* TODO three\n" store
      assertEqual "the rows" 3 (length (storeRecords next))
      assertEqual "the new row alone" [T.pack path <> "#2"] (upsertIds frames)
      assertEqual "no deletes" [] (deleteIds frames)

    -- Clearing the keyword off a title-less entry leaves no column at all, so the
    -- ENTRY stays in the file and the ROW leaves the table.  b.org keeps the palette.
  , testCase "clearing the last keyword off a title-less row deletes the row"
      $ withStoreOf [ ("a.org", "* TODO\n")
                    , ("b.org", "* TODO another file, another keyword\n") ]
      $ \_dir path store -> do
      was <- case rowsUnder path store of
        [r] -> pure r
        rs  -> assertFailure ("expected one row under a.org, got " <> show (map hrId rs))
      edits <- either (assertFailure . T.unpack) pure (setStateEdits noConfig Nothing was)
      _ <- either (assertFailure . show) pure
             =<< replaceSpans path (hrDigest was) edits
      left <- TIO.readFile path
      assertEqual "the file keeps the entry" "* \n" left
      (next, frames) <- applyFile path <$> loadFile path <*> pure store
      assertEqual "deletes" [hrId was] (deleteIds frames)
      assertEqual "upserts" [] (upsertIds frames)
      assertEqual "and a.org keeps no row" [] (map hrId (rowsUnder path next))

  , testCase "and the rows behind a blanked entry renumber"
      $ withStoreOf [("a.org", twoEntries)] $ \_dir path store -> do
      let before = map hrId (storeRecords store)
      (next, frames) <- rewrite path "* \n* TODO two\n" store
      assertEqual "the row at #0 is the other headline"
                  ["two"] (map hrTitle (storeRecords next))
      assertEqual "so #0 is re-sent" (take 1 before) (upsertIds frames)
      assertEqual "and #1 is deleted" (drop 1 before) (deleteIds frames)

  , testCase "a deleted file drops the rows it carried"
      $ withStoreOf [("a.org", entry "one" <> entry "two"), ("b.org", entry "three")]
      $ \_dir path store -> do
      removeFile path
      let (next, frames) = dropFile path store
      assertEqual "deletes" ["one", "two"] (deleteIds frames)
      assertEqual "upserts" [] (upsertIds frames)
      assertEqual "rows left" ["three"] (map hrId (storeRecords next))

  , testCase "a row two files carry outlives the first of them to drop it"
      $ withStoreOf [ ("a.org", entryAs "shared" "TODO from a")
                    , ("b.org", entryAs "shared" "TODO from b") ]
      $ \dir pa store -> do
      -- The row is served while one file still provides it; the SECOND to lose it deletes.
      assertEqual "one row for the id" ["shared"] (map hrId (storeRecords store))
      (half, frames) <- rewrite pa "* TODO something else\n" store
      assertEqual "deletes" [] (deleteIds frames)
      assertBool "the row is still served" ("shared" `elem` map hrId (storeRecords half))
      (none, later) <- rewrite (dir </> "b.org") "* TODO nothing shared\n" half
      assertEqual "deletes" ["shared"] (deleteIds later)
      assertBool "and gone" ("shared" `notElem` map hrId (storeRecords none))

  , testCase "a file the store never held is not a deletion"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \dir _path store -> do
      assertEqual "the fixture's rows" 1 (length (storeRecords store))
      let (_next, frames) = dropFile (dir </> "gone.org") store
      assertEqual "frames" [] frames

  , testCase "a created file is upserts and no deletes"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \dir _path store -> do
      (next, frames) <- rewrite (dir </> "b.org") "* TODO two\n* TODO three\n" store
      assertEqual "upserts" 2 (length (upsertIds frames))
      assertEqual "deletes" [] (deleteIds frames)
      assertEqual "rows" 3 (length (storeRecords next))

  , testCase "the store still equals the load it stands in for"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \dir _path store -> do
      (next, _frames) <- rewrite (dir </> "b.org") "#+CATEGORY: notes\n* NEXT two\n" store
      loaded <- loadDir dir
      assertEqual "rows" (map hrId (qrRecords loaded)) (map hrId (storeRecords next))
      assertEqual "files" (qrFiles loaded) (qrFiles (storeResult next))
  ]
  where twoEntries = "* TODO one\n* TODO two\n"
        -- A parent, its child and BODY under the child.
        tree body = entryAs "one" "TODO parent" <> "** child\n" <> body

-- | A file that stops loading keeps the rows it had: 'orgParse' is all-or-nothing,
-- so a save caught mid-write looks like a file whose headlines all vanished.
failureSpec :: TestTree
failureSpec = testGroup "Load failure"
  [ testCase "a parse failure keeps the file's rows and streams nothing"
      $ withStoreOf [("a.org", "* TODO one\n* TODO two\n")] $ \dir path store -> do
      _ <- orgFile dir "a.org" "* A title with a :: double colon\n"
      fresh <- loadFile path
      assertEqual "load" (Left ParseFailed) (fmap (map hrId) fresh)
      let (next, frames) = applyFile path fresh store
      assertEqual "frames" [] frames
      assertEqual "the rows it is keeping" 2 (length (storeRecords store))
      assertEqual "rows kept" (map hrId (storeRecords store)) (map hrId (storeRecords next))
      assertEqual "parse failures" 1 (qrParseFailures (storeResult next))
      assertEqual "files" 1 (qrFiles (storeResult next))

  , testCase "an unreadable file keeps its rows too"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \_dir path store -> do
      let (next, frames) = applyFile path (Left ReadFailed) store
      assertEqual "frames" [] frames
      assertEqual "rows kept" 1 (length (storeRecords next))
      assertEqual "read failures" 1 (qrReadFailures (storeResult next))

  , testCase "a file that parses again streams the difference"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \_dir path store -> do
      let (broken, _f) = applyFile path (Left ParseFailed) store
      (next, frames) <- rewrite path "* TODO one\n* TODO two\n" broken
      assertEqual "upserts" 1 (length (upsertIds frames))
      assertEqual "parse failures cleared" 0 (qrParseFailures (storeResult next))
  ]

-- | The palette rides the columns, which no row op streams: the socket closes.
keywordSpec :: TestTree
keywordSpec = testGroup "Keyword palette"
  [ testCase "a new keyword signals a view change"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \_dir path store -> do
      assertEqual "before" (TodoKeywords ["TODO"] ["DONE"]) (storeKeywords store)
      (next, frames) <- rewrite path "#+TODO: TODO WAITING | DONE\n* WAITING one\n" store
      assertEqual "frames" [Close ViewChanged] frames
      assertEqual "after" (TodoKeywords ["TODO", "WAITING"] ["DONE"]) (storeKeywords next)

  , testCase "a keyword another file still declares is not a view change" $
      let declared = "#+TODO: TODO WAITING | DONE\n* WAITING one\n" in
      withStoreOf [("a.org", declared), ("b.org", declared)] $ \_dir path store -> do
      (next, frames) <- rewrite path "* TODO one\n" store
      assertBool ("view change in " <> show frames) (Close ViewChanged `notElem` frames)
      assertEqual "palette" (storeKeywords store) (storeKeywords next)

    -- Down to org's own pair: TODO and DONE are recognized under every root.
  , testCase "the last file declaring a keyword takes it with it"
      $ withStoreOf [("a.org", "#+TODO: TODO WAITING | DONE\n* WAITING one\n")]
      $ \_dir path store -> do
      removeFile path
      let (next, frames) = dropFile path store
      assertEqual "frames" [Close ViewChanged] frames
      assertEqual "palette" (TodoKeywords ["TODO"] ["DONE"]) (storeKeywords next)
  ]

-- | What a socket sees before anything changes.
bootstrapSpec :: TestTree
bootstrapSpec = testGroup "Bootstrap"
  [ testCase "is a set-rows carrying every row the store holds"
      $ withStoreOf [("a.org", "* TODO one\n* NEXT two\n"), ("b.org", "* DONE three\n")]
      $ \_dir _path store ->
      case bootstrapFrame store of
        Op (SetRows rows) -> do
          assertEqual "rows" (map rowJSON (storeRecords store)) rows
          assertEqual "count" 3 (length rows)
        other -> assertFailure ("expected set-rows, got " <> show other)

  , testCase "encodes as SCHEMA.md's op names"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \_dir path store -> do
      rows <- map rowJSON <$> recordsOf path
      assertEqual "set-rows" (Just "set-rows") (opOf (bootstrapFrame store))
      assertEqual "upsert-row" [Just "upsert-row"] (map (opOf . Op . UpsertRow) rows)
      assertEqual "delete-row" (Just "delete-row") (opOf (Op (DeleteRow "x")))
      assertEqual "a view change is no op at all" Nothing (frameJSON (Close ViewChanged))

  , testCase "a subscriber's bootstrap is the store at subscription"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \dir path store -> do
      hub <- newHub store
      _ <- orgFile dir "a.org" "* TODO one\n* TODO two\n"
      fresh <- loadFile path
      _ <- publish hub (applyFile path fresh)
      (_cid, _client, boot) <- atomically (subscribe hub)
      case boot of
        Op (SetRows rows) -> assertEqual "rows" 2 (length rows)
        other        -> assertFailure ("expected set-rows, got " <> show other)
  ]
  where opOf frame = frameJSON frame >>= stringAt "op"

-- | Delivery, and what happens to a client that stops reading.
hubSpec :: TestTree
hubSpec = testGroup "Hub"
  [ testCase "a subscriber receives what is published, in order" $ withTempDir $ \dir -> do
      (path, hub, client) <- subscribed dir
      _ <- orgFile dir "a.org" "* TODO one\n* TODO two\n"
      fresh <- loadFile path
      frames <- publish hub (applyFile path fresh)
      -- With nothing published the comparison below is met by delivering nothing.
      assertEqual "the upsert the new headline owes" 1 (length frames)
      delivered <- mapM (const (atomically (nextFrame client))) frames
      assertEqual "delivered" (map Just frames) delivered

  , testCase "a client that stops reading is dropped and publishing goes on" $ withTempDir $ \dir -> do
      (_path, hub, client) <- subscribed dir
      _ <- publish hub (streaming (replicate (fromIntegral clientCapacity + 1) (Op (DeleteRow "x"))))
      next <- atomically (nextFrame client)
      assertEqual "dropped" Nothing next
      after <- publish hub (streaming [Op (DeleteRow "y")])
      assertEqual "published anyway" [Op (DeleteRow "y")] after

  -- The SIZE is not asserted; the burst is, since a number here would only restate it.
  , testCase "a burst four times the old mailbox is still delivered" $ withTempDir $ \dir -> do
      (_path, hub, client) <- subscribed dir
      _ <- publish hub (streaming (replicate 1024 (Op (DeleteRow "x"))))
      assertEqual "still live" (Just (Op (DeleteRow "x"))) =<< atomically (nextFrame client)

  -- The overflow that matters is across STEPS: `publish' coalesces inside one.
  , testCase "a burst of steps overflows, and the resubscribe is the whole store"
      $ withTempDir $ \dir -> do
      (path, hub, client) <- subscribed dir
      replicateM_ (fromIntegral clientCapacity + 1)
                  (publish hub (streaming [Op (DeleteRow "x")]))
      assertEqual "the backlog is abandoned" Nothing =<< atomically (nextFrame client)
      _ <- orgFile dir "a.org" "* TODO one\n* TODO two\n"
      _ <- publish hub . applyFile path =<< loadFile path
      (_cid', _client', boot) <- atomically (subscribe hub)
      case boot of
        Op (SetRows rows) -> assertEqual "the resync carries both rows" 2 (length rows)
        other        -> assertFailure ("expected set-rows, got " <> show other)
  ]
  where subscribed dir = do
          path <- orgFile dir "a.org" "* TODO one\n"
          hub <- newHub =<< loadStore dir
          (_cid, client, _boot) <- atomically (subscribe hub)
          pure (path, hub, client)

-- | The debounce, the one part of the watch with a clock in it (monotonic seconds).
debounceSpec :: TestTree
debounceSpec = testGroup "Debounce"
  [ testCase "a path still being written waits" $ do
      let pending = Map.fromList [("a.org", 0), ("b.org", 0.05)]
      assertEqual "due at 0.09" ([], pending) (due debounceDelay 0.09 pending)

  , testCase "a path that went quiet comes due, the others stay" $ do
      let pending = Map.fromList [("a.org", 0), ("b.org", 0.5)]
      assertEqual "due at 0.2"
                  (["a.org"], Map.fromList [("b.org", 0.5)])
                  (due debounceDelay 0.2 pending)

  , testCase "the delay is exactly the boundary" $ do
      let pending = Map.fromList [("a.org", 0)]
      assertEqual "due at the delay" (["a.org"], Map.empty) (due debounceDelay 0.1 pending)

  , testCase "org files are watched and the editor's sidecars are not" $ do
      mapM_ (assertBool "should be watched" . isWatchable)
            ["/o/notes.org", "/o/NOTES.ORG", "notes.org"]
      mapM_ (assertBool "should be ignored" . not . isWatchable)
            ["/o/notes.txt", "/o/notes.org~", "/o/.#notes.org", "/o/#notes.org#", "/o/org"]

    -- BOTH sidecar shapes are exact, and only the `#' half is load-bearing here.
  , testCase "a hash-prefixed org file is a document of its own" $
      mapM_ (assertBool "should be watched" . isWatchable)
            ["/o/#inbox.org", "/o/#notes.org", "#one.org"]
  ]

-- | The nudge: a path queued by the daemon rather than by inotify, since fsnotify
-- arms a new directory without traversing into it.  What is checked is the DOOR.
nudgeSpec :: TestTree
nudgeSpec = testGroup "Nudge"
  [ testCase "a nudged path joins the queue the way an event does" $
      assertEqual "one path waiting" ["/o/notes.org"] =<< queued ["/o/notes.org"]

    -- THE PREDICATE IS THE DOOR'S, so a nudged mirror cannot reach the table.
  , testCase "a path the walk declines is nudged into nothing" $
      assertEqual "nothing waiting" [] =<< queued
        [ "/o/notes.txt", "/o/.#notes.org", "/o/#notes.org#"
        , "/o/.org-glance/overviews/c1f3/overview.org"
        , "/o/.org-glance/meta/agenda.org" ]

    -- The two a write reaches through unwatched directories: a blob and a config layer.
  , testCase "a blob and a config layer both get through" $
      assertEqual "both waiting"
                  [ "/o/.org-glance/config/system.org"
                  , "/o/.org-glance/data/ac/2ede/data.org" ]
        =<< queued [ "/o/.org-glance/data/ac/2ede/data.org"
                   , "/o/.org-glance/config/system.org" ]

    -- THE DEBOUNCE STILL HOLDS: the map is keyed by path, so the pair costs ONE parse.
  , testCase "a nudge and an event for one path are one load" $ do
      pending <- queue ["/o/notes.org", "/o/notes.org"]
      assertEqual "one entry, not two" ["/o/notes.org"] (Map.keys pending)
      assertEqual "and it comes due once" ["/o/notes.org"]
                  (fst (due 0 (maximum (Map.elems pending)) pending))

    -- Loading and streaming are the drain loop's, which keeps the watch the sole updater.
  , testCase "nudging writes no store and streams nothing" $
      withStoreOf [("a.org", "* TODO one\n")] $ \_dir path store -> do
      hub <- newHub store
      (_cid, client, _boot) <- atomically (subscribe hub)
      TIO.writeFile path "* TODO one renamed\n"
      nudge defaultWalk hub path
      next <- readTVarIO (hubStore hub)
      assertEqual "the store still says what it loaded"
                  ["one"] (map hrTitle (storeRecords next))
      assertBool "and the generation has not moved" (stGen next == stGen store)
      assertEqual "nothing on the wire" Nothing =<< atomically (tryFrame client)

    -- The drain names no path: it reads the QUEUE, so this passes only via the nudge.
  , testCase "the drain loop turns a nudge into the load" $
      withStoreOf [("a.org", "* TODO one\n")] $ \dir path store -> do
      hub <- newHub store
      TIO.writeFile path "* TODO one renamed\n"
      nudge defaultWalk hub path
      drainNow dir hub
      assertEqual "the row moved" ["one renamed"] . map hrTitle . storeRecords
        =<< readTVarIO (hubStore hub)
      assertEqual "and the queue is spent" [] . Map.keys
        =<< readTVarIO (hubPending hub)

  , testCase "a nudged path that fails to load keeps its rows and streams nothing" $
      withStoreOf [("a.org", "* TODO one\n")] $ \dir path store -> do
      hub <- newHub store
      (_cid, client, _boot) <- atomically (subscribe hub)
      TIO.writeFile path "* A title with a :: double colon\n"
      nudge defaultWalk hub path
      drainNow dir hub
      next <- readTVarIO (hubStore hub)
      assertEqual "the rows it had" ["one"] (map hrTitle (storeRecords next))
      assertBool "the generation moved on the outcome" (stGen next > stGen store)
      assertEqual "and no row op behind it" Nothing =<< atomically (tryFrame client)
  ]
  where
    -- PATHS nudged into a hub over no files, and what is waiting afterwards.
    queue paths = do
      hub <- newHub emptyStore
      mapM_ (nudge defaultWalk hub) paths
      readTVarIO (hubPending hub)
    queued = fmap Map.keys . queue

-- | C's next frame if there is one, without blocking.
tryFrame :: Client -> STM (Maybe Frame)
tryFrame c = nextFrame c `orElse` pure Nothing
