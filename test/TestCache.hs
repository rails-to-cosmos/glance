module TestCache (spec) where

import Control.Exception (bracket)
import Data.List (sort)
import Data.Text (Text)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Glance.Query (HeadlineRecord (..), Ref (..))
import Glance.Web.Cache
  ( Cache, backlinks, cacheApplyFile, cacheCounts, cacheDropFile, cacheFill
  , closeCache, neighbors, openCache )
import Glance.Web.Store (loadStore, storeRecords)
import TestDefaults (orgFile, withTempDir)


-- | A two-file tree: a links to b, so there is exactly one edge to find.
seedTree :: FilePath -> IO ()
seedTree dir = do
  _ <- orgFile dir "a.org"
        "* TODO alpha\n:PROPERTIES:\n:ORG_GLANCE_ID: id-a\n:END:\nsee [[id:id-b][beta]]\n"
  _ <- orgFile dir "b.org"
        "* NEXT beta\n:PROPERTIES:\n:ORG_GLANCE_ID: id-b\n:END:\n"
  pure ()

-- | The edges the records carry, as (src id, target) pairs — the cache's own
-- edge rows, derived from the records so the test never guesses normalization.
edgesOf :: [HeadlineRecord] -> [(Text, Text)]
edgesOf rs = [ (hrId r, refTarget ref) | r <- rs, ref <- hrLinks r ]

rowsIn :: FilePath -> [HeadlineRecord] -> Int
rowsIn path rs = length [ r | r <- rs, hrFile r == path ]

withCache :: FilePath -> (Cache -> IO a) -> IO a
withCache dir = bracket (openCache (dir </> "cache.sqlite")) closeCache

spec :: TestTree
spec = testGroup "Cache"
  [ testCase "fill caches every row the store holds" $ withTempDir $ \dir -> do
      seedTree dir
      records <- storeRecords <$> loadStore dir
      withCache dir $ \c -> do
        cacheFill c records
        (h, e) <- cacheCounts c
        assertEqual "one cached headline per store row" (length records) h
        assertEqual "one edge per link the records carry" (length (edgesOf records)) e

  , testCase "every link round-trips as a backlink and a neighbour" $ withTempDir $ \dir -> do
      seedTree dir
      records <- storeRecords <$> loadStore dir
      withCache dir $ \c -> do
        cacheFill c records
        assertBool "the tree has at least one edge to test" (not (null (edgesOf records)))
        mapM_ (\(src, dst) -> do
                 backs <- backlinks c dst
                 assertBool (show src <> " is a backlink of " <> show dst) (src `elem` backs)
                 near <- neighbors c src
                 assertBool (show dst <> " is a neighbour of " <> show src) (dst `elem` near))
              (edgesOf records)

  , testCase "applyFile replaces just that file's rows and edges" $ withTempDir $ \dir -> do
      seedTree dir
      records <- storeRecords <$> loadStore dir
      withCache dir $ \c -> do
        cacheFill c records
        -- Rewrite a.org with no link, reload, and re-apply only that file.
        _ <- orgFile dir "a.org" "* TODO alpha\n:PROPERTIES:\n:ORG_GLANCE_ID: id-a\n:END:\n"
        records' <- storeRecords <$> loadStore dir
        let aPath = dir </> "a.org"
        cacheApplyFile c aPath [ r | r <- records', hrFile r == aPath ]
        (h, e) <- cacheCounts c
        assertEqual "still one row per document" (length records') h
        assertEqual "a's edge is gone" 0 e

  , testCase "dropFile removes a file's rows and edges" $ withTempDir $ \dir -> do
      seedTree dir
      records <- storeRecords <$> loadStore dir
      withCache dir $ \c -> do
        cacheFill c records
        let aPath = dir </> "a.org"
        cacheDropFile c aPath
        (h, _) <- cacheCounts c
        assertEqual "a's rows are gone, b's remain" (length records - rowsIn aPath records) h
        backs <- backlinks c "id-b"
        assertEqual "a's backlink to b is gone with it" [] (sort backs)
  ]
