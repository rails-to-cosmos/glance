-- | The live store: what one file's edit turns into on the wire.
--
-- Every case here drives the pure store functions and the hub's STM directly.
-- No port is bound and no websocket is spoken: the frames a socket would carry
-- are the frames 'applyFile' returns, and testing them where they are computed
-- keeps the suite free of the timing that comes with sockets.  The one thing
-- with a clock in it, the debounce, is a pure function over a map.
module TestStore (spec) where

import Control.Concurrent.STM (atomically)
import Control.Exception (finally)
import Data.Aeson (Value (Object, String))
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, secondsToDiffTime)
import Data.Unique (hashUnique, newUnique)
import System.Directory ( createDirectoryIfMissing, getTemporaryDirectory
                        , removeDirectoryRecursive, removeFile )
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Glance.Query ( HeadlineRecord (hrId), LoadFailure (..), QueryResult (..)
                    , TodoKeywords (..), loadDir, loadFile, rowJSON )
import Glance.Web.Store ( Frame (..), Store, applyFile, bootstrapFrame
                        , clientCapacity, dropFile, frameJSON, loadStore, newHub
                        , nextFrame, publish, storeKeywords, storeRecords
                        , storeResult, subscribe )
import Glance.Web.Watch (debounceDelay, due, isWatchable)

-- Scaffolding

-- | Run ACT over a directory of its own, removed afterwards whatever happens.
-- The store's whole subject is files changing, so every case here writes real
-- ones and re-reads them the way the watcher does.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir act = do
  base <- getTemporaryDirectory
  unique <- hashUnique <$> newUnique
  let dir = base </> ("glance-store-" <> show unique)
  createDirectoryIfMissing True dir
  act dir `finally` removeDirectoryRecursive dir

-- | Write TEXT to DIR/NAME and yield the path.
orgFile :: FilePath -> FilePath -> T.Text -> IO FilePath
orgFile dir name text = path <$ TIO.writeFile path text
  where path = dir </> name

-- | PATH's records, or the failure as a test failure.  The watcher's own read.
recordsOf :: FilePath -> IO [HeadlineRecord]
recordsOf path = loadFile path >>= either (assertFailure . whyNot) pure
  where whyNot f = "expected " <> path <> " to load, got " <> show f

-- | Ids the frames touch, upserts and deletes apart.
upsertIds, deleteIds :: [Frame] -> [T.Text]
upsertIds frames = [ i | UpsertRow row <- frames, Just i <- [stringAt "id" row] ]
deleteIds frames = [ i | DeleteRow i <- frames ]

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
  [ diffSpec, failureSpec, keywordSpec, bootstrapSpec, hubSpec, debounceSpec ]

-- | One file re-read, and the frames the difference implies.
diffSpec :: TestTree
diffSpec = testGroup "File diff"
  [ testCase "a file that did not change streams nothing" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" "#+CATEGORY: notes\n* TODO one\n* NEXT two :tag:\n"
      store <- loadStore dir
      fresh <- loadFile path
      assertEqual "frames" [] (snd (applyFile path fresh store))

  , testCase "a new headline is one upsert" $ withTempDir $ \dir -> do
      path <- orgFile dir "a.org" "* TODO one\n  :PROPERTIES:\n  :ORG_GLANCE_ID: one\n  :END:\n"
      store <- loadStore dir
      _ <- orgFile dir "a.org" ("* TODO one\n  :PROPERTIES:\n  :ORG_GLANCE_ID: one\n  :END:\n"
                             <> "* TODO two\n  :PROPERTIES:\n  :ORG_GLANCE_ID: two\n  :END:\n")
      fresh <- loadFile path
      let (next, frames) = applyFile path fresh store
      assertEqual "upserts" ["two"] (upsertIds frames)
      assertEqual "deletes" [] (deleteIds frames)
      assertEqual "rows" ["one", "two"] (map hrId (storeRecords next))

  , testCase "an edited title keeps the id the file gave it" $ withTempDir $ \dir -> do
      let stated s = "* " <> s <> "\n  :PROPERTIES:\n  :ORG_GLANCE_ID: one\n  :END:\n"
      path <- orgFile dir "a.org" (stated "TODO first")
      store <- loadStore dir
      _ <- orgFile dir "a.org" (stated "DONE first")
      fresh <- loadFile path
      let (_next, frames) = applyFile path fresh store
      assertEqual "upserts" ["one"] (upsertIds frames)
      assertEqual "deletes" [] (deleteIds frames)
      expected <- recordsOf path
      assertEqual "row" [UpsertRow (rowJSON r) | r <- expected] frames

  , testCase "a removed headline is one delete" $ withTempDir $ \dir -> do
      let entry i = "* TODO " <> i <> "\n  :PROPERTIES:\n  :ORG_GLANCE_ID: " <> i <> "\n  :END:\n"
      path <- orgFile dir "a.org" (entry "one" <> entry "two")
      store <- loadStore dir
      _ <- orgFile dir "a.org" (entry "one")
      fresh <- loadFile path
      let (next, frames) = applyFile path fresh store
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
      _ <- orgFile dir "a.org" "#+TITLE: notes\n* TODO one\n* TODO two\n"
      fresh <- loadFile path
      let (next, frames) = applyFile path fresh store
          after = map hrId (storeRecords next)
      assertEqual "every row is reinserted" after (upsertIds frames)
      assertEqual "every old row is dropped" before (deleteIds frames)
      assertBool "ids overlap" (not (any (`elem` after) before))

  , testCase "a deleted file drops the rows it carried" $ withTempDir $ \dir -> do
      let entry i = "* TODO " <> i <> "\n  :PROPERTIES:\n  :ORG_GLANCE_ID: " <> i <> "\n  :END:\n"
      path <- orgFile dir "a.org" (entry "one" <> entry "two")
      _ <- orgFile dir "b.org" (entry "three")
      store <- loadStore dir
      removeFile path
      let (next, frames) = dropFile path store
      assertEqual "deletes" ["one", "two"] (deleteIds frames)
      assertEqual "upserts" [] (upsertIds frames)
      assertEqual "rows left" ["three"] (map hrId (storeRecords next))

  , testCase "a file the store never held is not a deletion" $ withTempDir $ \dir -> do
      _ <- orgFile dir "a.org" "* TODO one\n"
      store <- loadStore dir
      let (_next, frames) = dropFile (dir </> "gone.org") store
      assertEqual "frames" [] frames

  , testCase "a created file is upserts and no deletes" $ withTempDir $ \dir -> do
      _ <- orgFile dir "a.org" "* TODO one\n"
      store <- loadStore dir
      path <- orgFile dir "b.org" "* TODO two\n* TODO three\n"
      fresh <- loadFile path
      let (next, frames) = applyFile path fresh store
      assertEqual "upserts" 2 (length (upsertIds frames))
      assertEqual "deletes" [] (deleteIds frames)
      assertEqual "rows" 3 (length (storeRecords next))

  , testCase "the store still equals the load it stands in for" $ withTempDir $ \dir -> do
      _ <- orgFile dir "a.org" "* TODO one\n"
      store <- loadStore dir
      path <- orgFile dir "b.org" "#+CATEGORY: notes\n* NEXT two\n"
      fresh <- loadFile path
      let (next, _frames) = applyFile path fresh store
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
      _ <- orgFile dir "a.org" "* TODO one\n* TODO two\n"
      fresh <- loadFile path
      let (next, frames) = applyFile path fresh broken
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
      _ <- orgFile dir "a.org" "#+TODO: TODO WAITING | DONE\n* WAITING one\n"
      fresh <- loadFile path
      let (next, frames) = applyFile path fresh store
      assertEqual "frames" [ViewChanged] frames
      assertEqual "after" (TodoKeywords ["TODO", "WAITING"] ["DONE"]) (storeKeywords next)

  , testCase "a keyword another file still declares is not a view change" $ withTempDir $ \dir -> do
      let declared = "#+TODO: TODO WAITING | DONE\n* WAITING one\n"
      path <- orgFile dir "a.org" declared
      _ <- orgFile dir "b.org" declared
      store <- loadStore dir
      _ <- orgFile dir "a.org" "* TODO one\n"
      fresh <- loadFile path
      let (next, frames) = applyFile path fresh store
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

-- | The debounce, which is the one part of the watch with a clock in it.
debounceSpec :: TestTree
debounceSpec = testGroup "Debounce"
  [ testCase "a path still being written waits" $ do
      let pending = Map.fromList [("a.org", at 0), ("b.org", at 0.05)]
      assertEqual "due at 0.09" ([], pending) (due debounceDelay (at 0.09) pending)

  , testCase "a path that went quiet comes due, the others stay" $ do
      let pending = Map.fromList [("a.org", at 0), ("b.org", at 0.5)]
      assertEqual "due at 0.2"
                  (["a.org"], Map.fromList [("b.org", at 0.5)])
                  (due debounceDelay (at 0.2) pending)

  , testCase "the delay is exactly the boundary" $ do
      let pending = Map.fromList [("a.org", at 0)]
      assertEqual "due at the delay" (["a.org"], Map.empty) (due debounceDelay (at 0.1) pending)

  , testCase "org files are watched and the editor's sidecars are not" $ do
      mapM_ (assertBool "should be watched" . isWatchable)
            ["/o/notes.org", "/o/NOTES.ORG", "notes.org"]
      mapM_ (assertBool "should be ignored" . not . isWatchable)
            ["/o/notes.txt", "/o/notes.org~", "/o/.#notes.org", "/o/#notes.org#", "/o/org"]
  ]
  where at :: Double -> UTCTime
        at offset = addUTCTime (realToFrac offset)
                               (UTCTime (fromGregorian 2026 7 31) (secondsToDiffTime 0))
