-- | The live store: what one file's edit turns into on the wire.
--
-- Every case here drives the pure store functions and the hub's STM directly.
-- No port is bound and no websocket is spoken: the frames a socket would carry
-- are the frames 'applyFile' returns, and testing them where they are computed
-- keeps the suite free of the timing that comes with sockets.  The one thing
-- with a clock in it, the debounce, is a pure function over a map.
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
import TestDefaults (entry, entryAs, orgFile, recordsOf, withTempDir)

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
import Glance.Web.Store ( Client, Frame (..), Hub (hubPending, hubStore)
                        , Store (stGen, stPrint), applyFile, bootstrapFrame
                        , clientCapacity, dropFile, emptyStore, frameJSON, loadStore
                        , loadStoreWith, newHub, nextFrame, publish, storeKeywords
                        , storeRecords, storeResult, storeTags, subscribe )
import Glance.Web.Watch (debounceDelay, drain, due, isWatchable, nudge, watched)

-- Scaffolding
--
-- The store's whole subject is files changing, so every case here writes real
-- ones into a directory of its own ('withTempDir') and re-reads them the way
-- the watcher does.

-- | Run K over a store loaded from a directory holding FILES, handing it the
-- directory, the FIRST file's path and the store.  The load runs once the whole
-- list is written, so a case wanting anything else on disk first — a symlink, a
-- second root — keeps its own 'withTempDir'.
withStoreOf :: [(FilePath, T.Text)] -> (FilePath -> FilePath -> Store -> IO a) -> IO a
withStoreOf files k = withTempDir $ \dir -> do
  paths <- mapM (uncurry (orgFile dir)) files
  case paths of
    path : _ -> k dir path =<< loadStore dir
    []       -> assertFailure "withStoreOf: a store of no files says nothing"

-- | Rewrite PATH with TEXT and fold the re-read into STORE: the watch's whole
-- step, which is a write, one file's parse and one diff.
rewrite :: FilePath -> T.Text -> Store -> IO (Store, [Frame])
rewrite path text store = do
  TIO.writeFile path text
  applyFile path <$> loadFile path <*> pure store

-- | The one record STORE holds, LABEL naming the moment in the failure.  A
-- store holding any other number is a fixture that stopped saying what it meant.
oneRecord :: String -> Store -> IO HeadlineRecord
oneRecord label store = case storeRecords store of
  [r]  -> pure r
  recs -> assertFailure (label <> ": expected one record, got " <> show (map hrId recs))

-- | The rows STORE holds for PATH: its own answer, narrowed to one file.
rowsUnder :: FilePath -> Store -> [HeadlineRecord]
rowsUnder path store = [ r | r <- storeRecords store, hrFile r == path ]

-- | Ids the frames touch, upserts and deletes apart.
upsertIds, deleteIds :: [Frame] -> [T.Text]
upsertIds frames = [ i | UpsertRow row <- frames, Just i <- [stringAt "id" row] ]
deleteIds frames = [ i | DeleteRow i <- frames ]

-- | The title cell of every row the frames upsert.  Which id a frame names says
-- nothing about which file's row it carries, and that is the whole subject of
-- the shared-id group: two files claiming one id stream under the same id and
-- differ in the cells.
upsertTitles :: [Frame] -> [T.Text]
upsertTitles frames = [ t | UpsertRow row <- frames, Just t <- [cellIn "title" row] ]

-- | KEY's cell of a row object, when it holds a string there.
cellIn :: T.Text -> Value -> Maybe T.Text
cellIn key (Object o) = KM.lookup "cells" o >>= stringAt key
cellIn _key _row      = Nothing

-- | A row object's whole @cells@ object: what a test compares when the question
-- is whether the CELLS moved, the row fields beside them being another matter.
cellsOf :: Value -> Maybe Value
cellsOf (Object o) = KM.lookup "cells" o
cellsOf _row       = Nothing

-- | A store step that changes nothing and streams FRAMES: delivery under test
-- with the diff out of the way.
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
-- The lock is the one that costs: it dangles, its extension is @.org@, and a
-- watch that filters the path out can never clear the read failure a walk that
-- keeps it books.
sidecarSpec :: TestTree
sidecarSpec = testGroup "Editor sidecars"
  [ testCase "a dangling lock symlink is walked over, not read" $ withTempDir $ \dir -> do
      _ <- orgFile dir "notes.org" "* TODO one\n"
      -- What Emacs leaves beside an open buffer: a symlink to
      -- `user@host.pid:boot' pointing at nothing, and an auto-save copy.
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
      -- Everything the walk kept is a path an event for which is re-read, and
      -- nothing it passed over is: one rule, so a file cannot be loaded and
      -- unwatchable (a stale row nothing refreshes) or watched and unloaded (a
      -- row the walk never granted).
      mapM_ ((\p -> assertBool (p <> ": walked and not watched") (isWatchable p)) . hrFile)
            (qrRecords qr)
      mapM_ (\name -> assertBool (name <> ": watched though not walked")
                                 (not (isWatchable (dir </> name))))
            [".#notes.org", "#notes.org#"]
  ]

-- | What the wire carries where two rows claim one id.  Every served answer is
-- resolved ('Glance.Query.resolveIds'), so the frames have to be resolved too:
-- a client watching the tree must not be shown a row a refresh contradicts.
sharedSpec :: TestTree
sharedSpec = testGroup "Shared id"
  [ testCase "an edit to the losing file streams nothing over the winner"
      $ withShared $ \_pa pb store -> do
      -- Neither path is canonical, so walk order decides and a.org holds the
      -- id.  b.org's rows are invisible: editing one changes no answer this
      -- server gives, and the wire says exactly that.
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
      -- The id is still carried, so it is not a deletion: it is the same row
      -- id showing a different file's cells, which is one upsert.
      assertEqual "deletes" [] (deleteIds frames)
      assertEqual "upserts" ["shared"] (upsertIds frames)
      assertEqual "the cells" ["from b"] (upsertTitles frames)
      assertEqual "which is what is served" ["from b"] (map hrTitle (storeRecords next))

  , testCase "two headlines of one file sharing an id are one row, the first"
      $ withStoreOf [("a.org", entryAs "dup" "TODO alpha" <> entryAs "dup" "TODO omega")]
      $ \_dir path store -> do
      -- Within one file no path outranks the other, so the incumbent stands —
      -- the same rule, and the reason the stream cannot keep the last while
      -- the table keeps the first.
      assertEqual "one row for the id" ["alpha"] (map hrTitle (storeRecords store))
      (next, frames) <- rewrite path
        (entryAs "dup" "TODO alpha edited" <> entryAs "dup" "TODO omega edited") store
      assertEqual "the streamed row is the served row"
                  [ UpsertRow (rowJSON r) | r <- storeRecords next ] frames
      assertEqual "which is the first of the two" ["alpha edited"]
                  (map hrTitle (storeRecords next))
  ]

-- | Two files claiming @shared@, and the store over them.  a.org wins it on
-- walk order; K is handed both paths and that store.
withShared :: (FilePath -> FilePath -> Store -> IO a) -> IO a
withShared k =
  withStoreOf [ ("a.org", entryAs "shared" "TODO from a")
              , ("b.org", entryAs "shared" "TODO from b") ] $ \dir pa store -> do
    assertEqual "one row for the id" ["from a"] (map hrTitle (storeRecords store))
    k pa (dir </> "b.org") store

-- | The other half of the @ETag@: which tree the store was loaded from.  The
-- generation counts changes inside one process and starts at zero in the next
-- one, so this is what a client's cached copy is revalidated against across a
-- restart.
fingerprintSpec :: TestTree
fingerprintSpec = testGroup "Fingerprint"
  [ testCase "two loads of one unchanged tree print the same"
      $ withStoreOf [("a.org", "* TODO one\n"), ("b.org", "* NEXT two\n")]
      $ \dir _path first' -> do
      -- Which is the restart: same directory, same bytes, a second process.
      -- The generation is back at zero either way, so this is the whole of what
      -- keeps that client's 304 honest.
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
      -- An id-less headline's row id is FILE#K, so the path IS part of the
      -- answer: two trees of the same bytes under different names serve
      -- different ids and must not share a tag.
      _ <- orgFile dir "b.org" "* TODO one\n"
      removeFile path
      after <- loadStore dir
      assertBool "renamed, same fingerprint" (stPrint before /= stPrint after)

  , testCase "and so does the same tree served from another root"
      $ withTempDir $ \one -> withTempDir $ \two -> do
      -- Same reason, one level up: the paths the walk builds are the paths the
      -- ids carry, so `--dir a' and `--dir b' are different documents even
      -- byte for byte.  A daemon restarted onto another directory on the same
      -- port cannot 304 a client into the old one.
      mapM_ (\dir -> orgFile dir "a.org" "* TODO one\n") [one, two]
      first' <- loadStore one
      second' <- loadStore two
      assertBool "two roots, one fingerprint" (stPrint first' /= stPrint second')

  , testCase "an edit moves the generation and leaves the fingerprint"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \_dir path store -> do
      -- The pair is the tag: the fingerprint says which tree was loaded, the
      -- generation how far it has moved since.  Recomputing the fingerprint per
      -- edit would say the same thing twice and cost a fold over every file.
      (next, frames) <- rewrite path "* TODO one\n* TODO two\n" store
      assertBool "a new headline is a row change" (not (null frames))
      assertBool "generation stuck" (stGen next > stGen store)
      assertEqual "fingerprint" (stPrint store) (stPrint next)
  ]

-- | The update counter @GET \/headlines@ spells as an @ETag@.  It has to move
-- whenever a response would, and stay put whenever none would: an idle tree
-- revalidates to 304 forever, and a tree that changed never serves a client its
-- stale copy.
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
      -- The load counts ride on the same tag as the rows, so a file that
      -- stopped parsing has to invalidate a cached answer even though the rows
      -- it kept are the very ones that answer carried.
      let (broken, frames) = applyFile path (Left ParseFailed) store
      assertEqual "frames" [] frames
      assertEqual "rows kept" (map hrId (storeRecords store)) (map hrId (storeRecords broken))
      assertEqual "parse failures" 1 (qrParseFailures (storeResult broken))
      assertBool ("generation stuck at " <> show (stGen broken)) (stGen broken > stGen store)

  , testCase "and so does the same file parsing again"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \_dir path store -> do
      let (broken, _f) = applyFile path (Left ParseFailed) store
      -- The very rows it had, so the recovery is the outcome and nothing else.
      let (fixed, frames) = applyFile path (Right (storeRecords store)) broken
      assertEqual "frames" [] frames
      assertEqual "parse failures cleared" 0 (qrParseFailures (storeResult fixed))
      assertBool ("generation stuck at " <> show (stGen fixed)) (stGen fixed > stGen broken)
  ]

-- | The tag vocabulary: SCHEMA.md's virtual filter keys, kept beside the rows
-- so a query costs no fold over them.
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
      -- The ETag is the generation, so a query answered under an old tag must
      -- not be a query the old vocabulary could not have parsed.
      (same, _f) <- rewrite path "* TODO one :web:\n" st
      assertEqual "no rewrite, no move" (storeTags st) (storeTags same)
      (next, frames) <- rewrite path "* TODO one :web:inbox:\n" st
      assertBool "a tag change is a row change" (not (null frames))
      assertEqual "and the vocabulary followed" ["inbox", "web"] (storeTags next)
  ]

-- | org-glance's derived mirrors: walked past, and the ids they duplicate
-- resolved to the canonical file rather than to whichever came last.
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
      -- The exclusion is a property of the path, not of the descent: naming
      -- `meta' reaches the files in it without passing the directory check that
      -- would have turned the walk back, and they are derived all the same.
      let meta = dir </> ".org-glance" </> "meta"
      qr <- loadDirWith defaultWalk meta
      assertEqual "files" 0 (qrFiles qr)
      assertEqual "rows" 0 (length (qrRecords qr))
      -- The fixture does hold one, and --include-derived is what takes it.
      opened <- loadDirWith (WalkOptions True) meta
      assertEqual "files with --include-derived" 1 (qrFiles opened)
      assertEqual "rows with --include-derived" 1 (length (qrRecords opened))

  , testCase "the store resolves it the same way the load does"
      $ withMirrorTree $ \dir -> do
      qr <- loadDir dir
      st <- loadStore dir
      assertEqual "the default store skips them" 2 (length (storeRecords st))
      -- The store is the load it stands in for, id resolution included: the
      -- rows the walk kept, in the order it kept them.
      assertEqual "one row per id" (map hrId (qrRecords qr)) (map hrId (storeRecords st))

  , testCase "and resolves the shared id, where there is one to resolve"
      $ withMirrorTree $ \dir -> do
      -- The comparison above runs over a tree with no duplicate to drop, so it
      -- holds whether the store resolves ids or not.  With the mirrors walked,
      -- two files claim `shared-id' and resolution is the difference between
      -- three rows and four.
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
      -- The sidecar rule still applies inside an included mirror.
      assertBool "a lock file is not a document"
                 (not (watched (WalkOptions True) "/o/.org-glance/overviews/.#a.org"))
  ]
  where splitOn = foldr step [[]]
          where step '/' acc = [] : acc
                step c (seg : rest) = (c : seg) : rest
                step _ [] = []

-- | A tree shaped like org-glance's: a plain note, the canonical store under
-- @.org-glance\/data@, and the two mirror directories repeating one of its
-- headlines under the same id.
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
      -- Over an empty store the claim below is met by an empty diff of nothing,
      -- so the rows the fixture put there are what makes it one.
      assertEqual "the fixture's rows" 2 (length (storeRecords store))
      assertEqual "frames" [] . snd =<< rewrite path doc store

    -- Rows being top entries, a child is not one and an edit under one moves no
    -- cell.  The step still runs — the file is re-read and the entry replaced —
    -- so a materialize after it is pinned to the digest the file now has; what
    -- it does not do is send a frame or move the generation, since neither the
    -- rows nor the stats a response carries have changed.  This is the whole
    -- decision, and both halves of it are load-bearing.
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

    -- The one exception to the rule above, and it is the rule rather than a
    -- hole in it: `linked' is a row FIELD read off the whole subtree, so a
    -- child that grows the entry's first link changes the row JSON where no
    -- cell moved.  A client that draws the mark has to be told.
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
      assertEqual "row" [UpsertRow (rowJSON r) | r <- expected] frames

  , testCase "a removed headline is one delete"
      $ withStoreOf [("a.org", entry "one" <> entry "two")] $ \_dir path store -> do
      (next, frames) <- rewrite path (entry "one") store
      assertEqual "upserts" [] (upsertIds frames)
      assertEqual "deletes" ["two"] (deleteIds frames)
      assertEqual "rows" ["one"] (map hrId (storeRecords next))

  -- An id-less row is FILE#K, K its place among the file's top entries, so
  -- BYTES above it are not part of its name: a preamble, a longer title, a body
  -- that grew.  This case used to pin the opposite — the offset moved, every row
  -- of the file was renamed, and the store shipped a delete plus an insert for
  -- each because it could not tell a rename from one.
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

  -- The churn the ordinal cannot absorb, pinned so the trade is written down:
  -- what renumbers a file is its TOP ENTRIES moving, and a swap moves two.  The
  -- ids are the same two strings before and after, so the wire carries the cells
  -- trading places rather than a delete and an insert — the row at #0 is now the
  -- other headline.
  , testCase "swapping two id-less entries renumbers both"
      $ withStoreOf [("a.org", twoEntries)] $ \_dir path store -> do
      let before = map hrId (storeRecords store)
      (next, frames) <- rewrite path "* TODO two\n* TODO one\n" store
      assertEqual "the ids are the same set" before (map hrId (storeRecords next))
      assertEqual "and both rows are re-sent under them" before (upsertIds frames)
      assertEqual "nothing is deleted" [] (deleteIds frames)
      assertEqual "the row at #0 is the other headline"
                  ["two", "one"] (map hrTitle (storeRecords next))

    -- An entry inserted AHEAD of the others is the honest cost: every ordinal
    -- behind it moves up one, so the last id is new and every earlier one now
    -- names a different headline.  A file whose entries only ever grow at the
    -- END costs nothing at all.
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

    -- The row rule end to end.  Clearing the keyword off a title-less entry
    -- leaves a headline with nothing in any column, so the ENTRY stays in the
    -- file and the ROW leaves the table.  The command writes the file and the
    -- watch is what tells the browser: the route never updates the store, so
    -- the delete arrives here or not at all.
    --
    -- The second file is what makes the delete visible.  A file whose last row
    -- goes takes its keyword contribution with it, and the only file in a tree
    -- taking TODO with it is a moved palette, which 'guarded' answers with
    -- 'ViewChanged' INSTEAD of the rows.  b.org keeps the palette still so the
    -- subject here is the row.
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

    -- And the cost of that, which is the renumbering: an entry going blank is a
    -- removal as far as the ordinal is concerned, so #0 now names the headline
    -- that was #1 and #1 is gone.  The same wire shape a swap produces.
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
      -- One id, two files declaring it: the row is served the whole time one of
      -- them still provides it, and the second to lose it is the one that
      -- deletes it.  A delete sent early takes a row off the table that the
      -- other file is still standing behind.
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
  -- The id-stability cases below all rewrite THIS file, so what each of them
  -- varies is the rewrite alone and the base cannot drift between them.
  where twoEntries = "* TODO one\n* TODO two\n"
        -- A parent, its child and BODY under the child: the base the two cases
        -- about an edit below a top entry vary.
        tree body = entryAs "one" "TODO parent" <> "** child\n" <> body

-- | A file that stops loading keeps the rows it had.  'orgParse' is
-- all-or-nothing, so a save caught mid-write is indistinguishable from a file
-- whose headlines all vanished; keeping them is the conservative read, and the
-- count is how an operator finds out.
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

-- | The columns carry the keyword palette, and SCHEMA.md's streaming ops carry
-- rows alone.  A change to the palette therefore cannot be streamed: the
-- socket closes and the client re-fetches the view.
keywordSpec :: TestTree
keywordSpec = testGroup "Keyword palette"
  [ testCase "a new keyword signals a view change"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \_dir path store -> do
      assertEqual "before" (TodoKeywords ["TODO"] ["DONE"]) (storeKeywords store)
      (next, frames) <- rewrite path "#+TODO: TODO WAITING | DONE\n* WAITING one\n" store
      assertEqual "frames" [ViewChanged] frames
      assertEqual "after" (TodoKeywords ["TODO", "WAITING"] ["DONE"]) (storeKeywords next)

  , testCase "a keyword another file still declares is not a view change" $
      let declared = "#+TODO: TODO WAITING | DONE\n* WAITING one\n" in
      withStoreOf [("a.org", declared), ("b.org", declared)] $ \_dir path store -> do
      (next, frames) <- rewrite path "* TODO one\n" store
      assertBool ("view change in " <> show frames) (ViewChanged `notElem` frames)
      assertEqual "palette" (storeKeywords store) (storeKeywords next)

    -- Down to org's own pair rather than to nothing: TODO and DONE are
    -- recognized under every root whatever a tree configures, so the palette an
    -- empty store carries is the chain's first scope and not the empty list.
  , testCase "the last file declaring a keyword takes it with it"
      $ withStoreOf [("a.org", "#+TODO: TODO WAITING | DONE\n* WAITING one\n")]
      $ \_dir path store -> do
      removeFile path
      let (next, frames) = dropFile path store
      assertEqual "frames" [ViewChanged] frames
      assertEqual "palette" (TodoKeywords ["TODO"] ["DONE"]) (storeKeywords next)
  ]

-- | What a socket sees before anything changes.
bootstrapSpec :: TestTree
bootstrapSpec = testGroup "Bootstrap"
  [ testCase "is a set-rows carrying every row the store holds"
      $ withStoreOf [("a.org", "* TODO one\n* NEXT two\n"), ("b.org", "* DONE three\n")]
      $ \_dir _path store ->
      case bootstrapFrame store of
        SetRows rows -> do
          assertEqual "rows" (map rowJSON (storeRecords store)) rows
          assertEqual "count" 3 (length rows)
        other -> assertFailure ("expected set-rows, got " <> show other)

  , testCase "encodes as SCHEMA.md's op names"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \_dir path store -> do
      rows <- map rowJSON <$> recordsOf path
      assertEqual "set-rows" (Just "set-rows") (opOf (bootstrapFrame store))
      assertEqual "upsert-row" [Just "upsert-row"] (map (opOf . UpsertRow) rows)
      assertEqual "delete-row" (Just "delete-row") (opOf (DeleteRow "x"))
      assertEqual "a view change is no op at all" Nothing (frameJSON ViewChanged)

  , testCase "a subscriber's bootstrap is the store at subscription"
      $ withStoreOf [("a.org", "* TODO one\n")] $ \dir path store -> do
      hub <- newHub store
      _ <- orgFile dir "a.org" "* TODO one\n* TODO two\n"
      fresh <- loadFile path
      _ <- publish hub (applyFile path fresh)
      (_cid, _client, boot) <- atomically (subscribe hub)
      case boot of
        SetRows rows -> assertEqual "rows" 2 (length rows)
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
      -- With nothing published the comparison below is met by delivering
      -- nothing, so the one frame the added headline owes is pinned first.
      assertEqual "the upsert the new headline owes" 1 (length frames)
      delivered <- mapM (const (atomically (nextFrame client))) frames
      assertEqual "delivered" (map Just frames) delivered

  , testCase "a client that stops reading is dropped and publishing goes on" $ withTempDir $ \dir -> do
      (_path, hub, client) <- subscribed dir
      _ <- publish hub (streaming (replicate (fromIntegral clientCapacity + 1) (DeleteRow "x")))
      next <- atomically (nextFrame client)
      assertEqual "dropped" Nothing next
      -- And the store keeps taking updates with the dead client still around.
      after <- publish hub (streaming [DeleteRow "y"])
      assertEqual "published anyway" [DeleteRow "y"] after

  -- The mailbox was 256 and an editor writing a directory overran it in a
  -- second, which cost the page its socket every time.  The size is not
  -- asserted — a burst four times the old one is, since that is what the raise
  -- is for and what a number here would only restate.
  , testCase "a burst four times the old mailbox is still delivered" $ withTempDir $ \dir -> do
      (_path, hub, client) <- subscribed dir
      _ <- publish hub (streaming (replicate 1024 (DeleteRow "x")))
      assertEqual "still live" (Just (DeleteRow "x")) =<< atomically (nextFrame client)

  -- The overflow that matters is across STEPS, not inside one: `publish'
  -- coalesces a file's save into one transaction, and a bulk write is a
  -- transaction per file with nothing coalescing between them.  What the
  -- client gets back is not the backlog — that is gone — but the store as it
  -- stands, in the one frame a resubscribe opens with.
  , testCase "a burst of steps overflows, and the resubscribe is the whole store"
      $ withTempDir $ \dir -> do
      (path, hub, client) <- subscribed dir
      replicateM_ (fromIntegral clientCapacity + 1)
                  (publish hub (streaming [DeleteRow "x"]))
      assertEqual "the backlog is abandoned" Nothing =<< atomically (nextFrame client)
      -- And an edit that landed while it was away is in what it comes back to.
      _ <- orgFile dir "a.org" "* TODO one\n* TODO two\n"
      _ <- publish hub . applyFile path =<< loadFile path
      (_cid', _client', boot) <- atomically (subscribe hub)
      case boot of
        SetRows rows -> assertEqual "the resync carries both rows" 2 (length rows)
        other        -> assertFailure ("expected set-rows, got " <> show other)
  ]
  -- One file, a hub over it and a subscriber on the hub: what every case in
  -- this group opens with, and what none of them is about.
  where subscribed dir = do
          path <- orgFile dir "a.org" "* TODO one\n"
          hub <- newHub =<< loadStore dir
          (_cid, client, _boot) <- atomically (subscribe hub)
          pure (path, hub, client)

-- | The debounce, which is the one part of the watch with a clock in it.  Both
-- sides of it are monotonic seconds, so an entry here is the second a path was
-- last touched and the delay is the wait in the same unit.
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

    -- BOTH sidecar shapes are exact, and only one of them is load-bearing.
    -- `#notes.org#' is refused by its extension before the sidecar rule is
    -- asked, so the `#' rule stands or falls on a name that IS an org file: a
    -- hand-written `#inbox.org' is a document and must be walked and watched.
    -- A leading `#' alone made every such file silently invisible.
  , testCase "a hash-prefixed org file is a document of its own" $
      -- The sidecar rule matches TWO EXACT SHAPES.  `#notes.org#' is refused by
      -- its extension before the rule is asked, so the `#' half stands or falls
      -- on a name that IS an org file: a hand-written `#inbox.org'.  A bare
      -- leading `#' made every such file silently invisible to the walk and the
      -- watch.  The two sidecar shapes are asserted in the case above.
      mapM_ (assertBool "should be watched" . isWatchable)
            ["/o/#inbox.org", "/o/#notes.org", "#one.org"]
  ]

-- | The nudge: a path put into the watch's queue by the daemon rather than by
-- inotify, because fsnotify arms a newly created directory without traversing
-- into it and no event is coming for what a write just made.
--
-- What is checked here is the DOOR — that it filters like an event, coalesces
-- like an event and touches no store.  That every write route leaves through
-- it is 'TestServe'.
nudgeSpec :: TestTree
nudgeSpec = testGroup "Nudge"
  [ testCase "a nudged path joins the queue the way an event does" $
      assertEqual "one path waiting" ["/o/notes.org"] =<< queued ["/o/notes.org"]

    -- THE PREDICATE IS THE DOOR'S, not the caller's.  A route that nudged a
    -- mirror would otherwise put in the table exactly what the walk declined,
    -- which is the one thing the watch filter exists to stop.
  , testCase "a path the walk declines is nudged into nothing" $
      assertEqual "nothing waiting" [] =<< queued
        [ "/o/notes.txt", "/o/.#notes.org", "/o/#notes.org#"
        , "/o/.org-glance/overviews/c1f3/overview.org"
        , "/o/.org-glance/meta/agenda.org" ]

    -- The canonical store and the config are the two a write reaches through
    -- unwatched directories: a blob is a row and a config layer is a reseed.
  , testCase "a blob and a config layer both get through" $
      assertEqual "both waiting"
                  [ "/o/.org-glance/config/system.org"
                  , "/o/.org-glance/data/ac/2ede/data.org" ]
        =<< queued [ "/o/.org-glance/data/ac/2ede/data.org"
                   , "/o/.org-glance/config/system.org" ]

    -- THE DEBOUNCE STILL HOLDS.  The map is keyed by path, so a real event
    -- landing behind a nudge overwrites the timestamp rather than adding a
    -- second entry, and the pair costs ONE parse.  This is the whole reason the
    -- door is the debounce map rather than a queue of its own.
  , testCase "a nudge and an event for one path are one load" $ do
      pending <- queue ["/o/notes.org", "/o/notes.org"]
      assertEqual "one entry, not two" ["/o/notes.org"] (Map.keys pending)
      assertEqual "and it comes due once" ["/o/notes.org"]
                  (fst (due 0 (maximum (Map.elems pending)) pending))

    -- A nudge is a note in a map.  Loading and streaming are the drain loop's,
    -- which is what keeps the watch the sole updater of the store even though a
    -- request thread can now reach the queue.
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

    -- The drain is where it becomes a load, and it names no path: what it reads
    -- is the queue, so this passes only because the nudge put the path there.
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

    -- A nudged path is a path like any other once it is in the queue, so a file
    -- that will not parse keeps its rows and says nothing, exactly as a saved
    -- one does.  Nothing about the door makes a bad load louder.
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
    -- A path in the queue need be no path anything has read, so these cases
    -- touch no disk at all.
    queue paths = do
      hub <- newHub emptyStore
      mapM_ (nudge defaultWalk hub) paths
      readTVarIO (hubPending hub)
    queued = fmap Map.keys . queue

-- | One turn of the drain loop over DIR's HUB with the debounce out of the way:
-- everything queued is ripe, which is what a test wants and what the 25 ms poll
-- would otherwise make it sleep for.
drainNow :: FilePath -> Hub -> IO ()
drainNow = drain defaultWalk 0

-- | C's next frame if there is one already, without blocking on a socket that
-- was never going to be written to.
tryFrame :: Client -> STM (Maybe Frame)
tryFrame c = nextFrame c `orElse` pure Nothing
