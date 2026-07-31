-- | The live store: what one file's edit turns into on the wire.
--
-- Every case here drives the pure store functions and the hub's STM directly.
-- No port is bound and no websocket is spoken: the frames a socket would carry
-- are the frames 'applyFile' returns, and testing them where they are computed
-- keeps the suite free of the timing that comes with sockets.  The one thing
-- with a clock in it, the debounce, is a pure function over a map.
module TestStore (spec) where

import Control.Concurrent.STM (atomically)
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

import Glance.Query ( HeadlineRecord (hrFile, hrId, hrTitle), IdCollision (..)
                    , LoadFailure (..), QueryResult (..), TodoKeywords (..)
                    , WalkOptions (..), defaultWalk, loadDir, loadDirWith, loadFile
                    , rowJSON )
import Glance.Web.Store ( Frame (..), Store (stGen, stPrint), applyFile, bootstrapFrame
                        , clientCapacity, dropFile, frameJSON, loadStore
                        , loadStoreWith, newHub, nextFrame, publish, storeKeywords
                        , storeRecords, storeResult, storeTags, subscribe )
import Glance.Web.Watch (debounceDelay, due, isWatchable, watched)

-- Scaffolding
--
-- The store's whole subject is files changing, so every case here writes real
-- ones into a directory of its own ('withTempDir') and re-reads them the way
-- the watcher does.

-- | Rewrite PATH with TEXT and fold the re-read into STORE: the watch's whole
-- step, which is a write, one file's parse and one diff.
rewrite :: FilePath -> T.Text -> Store -> IO (Store, [Frame])
rewrite path text store = do
  TIO.writeFile path text
  applyFile path <$> loadFile path <*> pure store

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
  , derivedSpec, sidecarSpec, sharedSpec, bootstrapSpec, hubSpec, debounceSpec ]

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
      $ withTempDir $ \dir -> do
      -- Within one file no path outranks the other, so the incumbent stands —
      -- the same rule, and the reason the stream cannot keep the last while
      -- the table keeps the first.
      path <- orgFile dir "a.org" (entryAs "dup" "TODO alpha" <> entryAs "dup" "TODO omega")
      store <- loadStore dir
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
withShared k = withTempDir $ \dir -> do
  pa <- orgFile dir "a.org" (entryAs "shared" "TODO from a")
  pb <- orgFile dir "b.org" (entryAs "shared" "TODO from b")
  store <- loadStore dir
  assertEqual "one row for the id" ["from a"] (map hrTitle (storeRecords store))
  k pa pb store

-- | The other half of the @ETag@: which tree the store was loaded from.  The
-- generation counts changes inside one process and starts at zero in the next
-- one, so this is what a client's cached copy is revalidated against across a
-- restart.
fingerprintSpec :: TestTree
fingerprintSpec = testGroup "Fingerprint"
  [ testCase "two loads of one unchanged tree print the same" $ withTempDir $ \dir -> do
      -- Which is the restart: same directory, same bytes, a second process.
      -- The generation is back at zero either way, so this is the whole of what
      -- keeps that client's 304 honest.
      _ <- orgFile dir "a.org" "* TODO one\n"
      _ <- orgFile dir "b.org" "* NEXT two\n"
      first' <- loadStore dir
      second' <- loadStore dir
      assertBool "no fingerprint at all" (not (T.null (stPrint first')))
      assertEqual "fingerprint" (stPrint first') (stPrint second')

  , testCase "a byte of difference prints differently" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" "* TODO one\n"
      before <- loadStore dir
      _ <- orgFile dir "a.org" "* TODO one!\n"
      after <- loadStore dir
      assertBool ("one fingerprint for two trees: " <> path)
                 (stPrint before /= stPrint after)

  , testCase "and so does a file renamed under the same content"
      $ withTempDir $ \dir -> do
      -- An id-less headline's row id is FILE:START, so the path IS part of the
      -- answer: two trees of the same bytes under different names serve
      -- different ids and must not share a tag.
      path <- orgFile dir "a.org" "* TODO one\n"
      before <- loadStore dir
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
      $ withTempDir $ \dir -> do
      -- The pair is the tag: the fingerprint says which tree was loaded, the
      -- generation how far it has moved since.  Recomputing the fingerprint per
      -- edit would say the same thing twice and cost a fold over every file.
      path <- orgFile dir "a.org" "* TODO one\n"
      store <- loadStore dir
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
  [ testCase "a file nothing wrote leaves it where it was" $ withTempDir $ \dir -> do
      let doc = "* TODO one\n"
      path <- orgFile dir "a.org" doc
      store <- loadStore dir
      (same, frames) <- rewrite path doc store
      assertEqual "frames" [] frames
      assertEqual "generation" (stGen store) (stGen same)

  , testCase "a changed row moves it" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" "* TODO one\n"
      store <- loadStore dir
      (next, frames) <- rewrite path "* TODO one\n* TODO two\n" store
      assertBool "a new headline is a row change" (not (null frames))
      assertBool ("generation stuck at " <> show (stGen next)) (stGen next > stGen store)

  , testCase "so does a load outcome, with no row to show for it" $ withTempDir $ \dir -> do
      -- The load counts ride on the same tag as the rows, so a file that
      -- stopped parsing has to invalidate a cached answer even though the rows
      -- it kept are the very ones that answer carried.
      path <- orgFile dir "a.org" "* TODO one\n"
      store <- loadStore dir
      let (broken, frames) = applyFile path (Left ParseFailed) store
      assertEqual "frames" [] frames
      assertEqual "rows kept" (map hrId (storeRecords store)) (map hrId (storeRecords broken))
      assertEqual "parse failures" 1 (qrParseFailures (storeResult broken))
      assertBool ("generation stuck at " <> show (stGen broken)) (stGen broken > stGen store)

  , testCase "and so does the same file parsing again" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" "* TODO one\n"
      store <- loadStore dir
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
  [ testCase "is every distinct tag the loaded rows carry" $ withTempDir $ \dir -> do
      _ <- orgFile dir "a.org" "* TODO one :web:glance:\n* NEXT two :web:\n* DONE three\n"
      st <- loadStore dir
      assertEqual "tags" ["glance", "web"] (storeTags st)

  , testCase "a re-read file adds its new tags and drops the ones it lost"
      $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" "* TODO one :web:\n"
      st <- loadStore dir
      assertEqual "before" ["web"] (storeTags st)
      (next, _frames) <- rewrite path "* TODO one :inbox:\n" st
      assertEqual "after" ["inbox"] (storeTags next)

  , testCase "a tag two files carry survives one of them going"
      $ withTempDir $ \dir -> do
      pa <- orgFile dir "a.org" "* TODO one :web:\n"
      _ <- orgFile dir "b.org" "* TODO two :web:\n"
      st <- loadStore dir
      let (gone, _frames) = dropFile pa st
      assertEqual "still declared" ["web"] (storeTags gone)

  , testCase "the vocabulary moves only where the generation does"
      $ withTempDir $ \dir -> do
      -- The ETag is the generation, so a query answered under an old tag must
      -- not be a query the old vocabulary could not have parsed.
      path <- orgFile dir "a.org" "* TODO one :web:\n"
      st <- loadStore dir
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
  [ testCase "a file that did not change streams nothing" $ withTempDir $ \dir -> do
      let doc = "#+CATEGORY: notes\n* TODO one\n* NEXT two :tag:\n"
      path <- orgFile dir "a.org" doc
      store <- loadStore dir
      -- Over an empty store the claim below is met by an empty diff of nothing,
      -- so the rows the fixture put there are what makes it one.
      assertEqual "the fixture's rows" 2 (length (storeRecords store))
      assertEqual "frames" [] . snd =<< rewrite path doc store

  , testCase "a new headline is one upsert" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" (entry "one")
      store <- loadStore dir
      (next, frames) <- rewrite path (entry "one" <> entry "two") store
      assertEqual "upserts" ["two"] (upsertIds frames)
      assertEqual "deletes" [] (deleteIds frames)
      assertEqual "rows" ["one", "two"] (map hrId (storeRecords next))

  , testCase "an edited title keeps the id the file gave it" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" (entryAs "one" "TODO first")
      store <- loadStore dir
      (_next, frames) <- rewrite path (entryAs "one" "DONE first") store
      assertEqual "upserts" ["one"] (upsertIds frames)
      assertEqual "deletes" [] (deleteIds frames)
      expected <- recordsOf path
      assertEqual "row" [UpsertRow (rowJSON r) | r <- expected] frames

  , testCase "a removed headline is one delete" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" (entry "one" <> entry "two")
      store <- loadStore dir
      (next, frames) <- rewrite path (entry "one") store
      assertEqual "upserts" [] (upsertIds frames)
      assertEqual "deletes" ["two"] (deleteIds frames)
      assertEqual "rows" ["one"] (map hrId (storeRecords next))

  -- Documented churn: without an ORG_GLANCE_ID a row is FILE:START, so text
  -- inserted above a headline renames it.  The store cannot tell that from a
  -- deletion and an insertion, and says so on the wire.  S8's write-back is
  -- where a stable id for an unmarked headline would have to come from.
  , testCase "an id-less headline that moves is a delete and an insert" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" "* TODO one\n* TODO two\n"
      store <- loadStore dir
      let before = map hrId (storeRecords store)
      (next, frames) <- rewrite path "#+TITLE: notes\n* TODO one\n* TODO two\n" store
      let after = map hrId (storeRecords next)
      assertEqual "every row is reinserted" after (upsertIds frames)
      assertEqual "every old row is dropped" before (deleteIds frames)
      assertBool "ids overlap" (not (any (`elem` after) before))

  , testCase "a deleted file drops the rows it carried" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" (entry "one" <> entry "two")
      _ <- orgFile dir "b.org" (entry "three")
      store <- loadStore dir
      removeFile path
      let (next, frames) = dropFile path store
      assertEqual "deletes" ["one", "two"] (deleteIds frames)
      assertEqual "upserts" [] (upsertIds frames)
      assertEqual "rows left" ["three"] (map hrId (storeRecords next))

  , testCase "a row two files carry outlives the first of them to drop it"
      $ withTempDir $ \dir -> do
      -- One id, two files declaring it: the row is served the whole time one of
      -- them still provides it, and the second to lose it is the one that
      -- deletes it.  A delete sent early takes a row off the table that the
      -- other file is still standing behind.
      pa <- orgFile dir "a.org" (entryAs "shared" "TODO from a")
      pb <- orgFile dir "b.org" (entryAs "shared" "TODO from b")
      store <- loadStore dir
      assertEqual "one row for the id" ["shared"] (map hrId (storeRecords store))
      (half, frames) <- rewrite pa "* TODO something else\n" store
      assertEqual "deletes" [] (deleteIds frames)
      assertBool "the row is still served" ("shared" `elem` map hrId (storeRecords half))
      (none, later) <- rewrite pb "* TODO nothing shared\n" half
      assertEqual "deletes" ["shared"] (deleteIds later)
      assertBool "and gone" ("shared" `notElem` map hrId (storeRecords none))

  , testCase "a file the store never held is not a deletion" $ withTempDir $ \dir -> do
      _ <- orgFile dir "a.org" "* TODO one\n"
      store <- loadStore dir
      assertEqual "the fixture's rows" 1 (length (storeRecords store))
      let (_next, frames) = dropFile (dir </> "gone.org") store
      assertEqual "frames" [] frames

  , testCase "a created file is upserts and no deletes" $ withTempDir $ \dir -> do
      _ <- orgFile dir "a.org" "* TODO one\n"
      store <- loadStore dir
      (next, frames) <- rewrite (dir </> "b.org") "* TODO two\n* TODO three\n" store
      assertEqual "upserts" 2 (length (upsertIds frames))
      assertEqual "deletes" [] (deleteIds frames)
      assertEqual "rows" 3 (length (storeRecords next))

  , testCase "the store still equals the load it stands in for" $ withTempDir $ \dir -> do
      _ <- orgFile dir "a.org" "* TODO one\n"
      store <- loadStore dir
      (next, _frames) <- rewrite (dir </> "b.org") "#+CATEGORY: notes\n* NEXT two\n" store
      loaded <- loadDir dir
      assertEqual "rows" (map hrId (qrRecords loaded)) (map hrId (storeRecords next))
      assertEqual "files" (qrFiles loaded) (qrFiles (storeResult next))
  ]

-- | A file that stops loading keeps the rows it had.  'orgParse' is
-- all-or-nothing, so a save caught mid-write is indistinguishable from a file
-- whose headlines all vanished; keeping them is the conservative read, and the
-- count is how an operator finds out.
failureSpec :: TestTree
failureSpec = testGroup "Load failure"
  [ testCase "a parse failure keeps the file's rows and streams nothing" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" "* TODO one\n* TODO two\n"
      store <- loadStore dir
      _ <- orgFile dir "a.org" "* A title with a :: double colon\n"
      fresh <- loadFile path
      assertEqual "load" (Left ParseFailed) (fmap (map hrId) fresh)
      let (next, frames) = applyFile path fresh store
      assertEqual "frames" [] frames
      assertEqual "the rows it is keeping" 2 (length (storeRecords store))
      assertEqual "rows kept" (map hrId (storeRecords store)) (map hrId (storeRecords next))
      assertEqual "parse failures" 1 (qrParseFailures (storeResult next))
      assertEqual "files" 1 (qrFiles (storeResult next))

  , testCase "an unreadable file keeps its rows too" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" "* TODO one\n"
      store <- loadStore dir
      let (next, frames) = applyFile path (Left ReadFailed) store
      assertEqual "frames" [] frames
      assertEqual "rows kept" 1 (length (storeRecords next))
      assertEqual "read failures" 1 (qrReadFailures (storeResult next))

  , testCase "a file that parses again streams the difference" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" "* TODO one\n"
      store <- loadStore dir
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
  [ testCase "a new keyword signals a view change" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" "* TODO one\n"
      store <- loadStore dir
      assertEqual "before" (TodoKeywords ["TODO"] ["DONE"]) (storeKeywords store)
      (next, frames) <- rewrite path "#+TODO: TODO WAITING | DONE\n* WAITING one\n" store
      assertEqual "frames" [ViewChanged] frames
      assertEqual "after" (TodoKeywords ["TODO", "WAITING"] ["DONE"]) (storeKeywords next)

  , testCase "a keyword another file still declares is not a view change" $ withTempDir $ \dir -> do
      let declared = "#+TODO: TODO WAITING | DONE\n* WAITING one\n"
      path <- orgFile dir "a.org" declared
      _ <- orgFile dir "b.org" declared
      store <- loadStore dir
      (next, frames) <- rewrite path "* TODO one\n" store
      assertBool ("view change in " <> show frames) (ViewChanged `notElem` frames)
      assertEqual "palette" (storeKeywords store) (storeKeywords next)

  , testCase "the last file declaring a keyword takes it with it" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" "#+TODO: TODO WAITING | DONE\n* WAITING one\n"
      store <- loadStore dir
      removeFile path
      let (next, frames) = dropFile path store
      assertEqual "frames" [ViewChanged] frames
      assertEqual "palette" (TodoKeywords [] []) (storeKeywords next)
  ]

-- | What a socket sees before anything changes.
bootstrapSpec :: TestTree
bootstrapSpec = testGroup "Bootstrap"
  [ testCase "is a set-rows carrying every row the store holds" $ withTempDir $ \dir -> do
      _ <- orgFile dir "a.org" "* TODO one\n* NEXT two\n"
      _ <- orgFile dir "b.org" "* DONE three\n"
      store <- loadStore dir
      case bootstrapFrame store of
        SetRows rows -> do
          assertEqual "rows" (map rowJSON (storeRecords store)) rows
          assertEqual "count" 3 (length rows)
        other -> assertFailure ("expected set-rows, got " <> show other)

  , testCase "encodes as SCHEMA.md's op names" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" "* TODO one\n"
      rows <- map rowJSON <$> recordsOf path
      store <- loadStore dir
      assertEqual "set-rows" (Just "set-rows") (opOf (bootstrapFrame store))
      assertEqual "upsert-row" [Just "upsert-row"] (map (opOf . UpsertRow) rows)
      assertEqual "delete-row" (Just "delete-row") (opOf (DeleteRow "x"))
      assertEqual "a view change is no op at all" Nothing (frameJSON ViewChanged)

  , testCase "a subscriber's bootstrap is the store at subscription" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" "* TODO one\n"
      hub <- newHub =<< loadStore dir
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
      path <- orgFile dir "a.org" "* TODO one\n"
      hub <- newHub =<< loadStore dir
      (_cid, client, _boot) <- atomically (subscribe hub)
      _ <- orgFile dir "a.org" "* TODO one\n* TODO two\n"
      fresh <- loadFile path
      frames <- publish hub (applyFile path fresh)
      -- With nothing published the comparison below is met by delivering
      -- nothing, so the one frame the added headline owes is pinned first.
      assertEqual "the upsert the new headline owes" 1 (length frames)
      delivered <- mapM (const (atomically (nextFrame client))) frames
      assertEqual "delivered" (map Just frames) delivered

  , testCase "a client that stops reading is dropped and publishing goes on" $ withTempDir $ \dir -> do
      _ <- orgFile dir "a.org" "* TODO one\n"
      hub <- newHub =<< loadStore dir
      (_cid, client, _boot) <- atomically (subscribe hub)
      _ <- publish hub (streaming (replicate (fromIntegral clientCapacity + 1) (DeleteRow "x")))
      next <- atomically (nextFrame client)
      assertEqual "dropped" Nothing next
      -- And the store keeps taking updates with the dead client still around.
      after <- publish hub (streaming [DeleteRow "y"])
      assertEqual "published anyway" [DeleteRow "y"] after
  ]

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
  ]
