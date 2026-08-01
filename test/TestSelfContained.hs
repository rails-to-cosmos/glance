-- | Repo hygiene rather than behaviour: nothing this package builds names a
-- path outside the repository, and the one file it vendors has a way to be
-- refreshed.  Both were bought at a price — the renderer was read at run time
-- out of one machine's home directory until 2026-08-02, so a correct build
-- served a table-less page on every other machine — and neither is visible to a
-- test that drives the server, which is why they live in a module of their own
-- rather than beside the routes.
module TestSelfContained (spec) where

import Control.Monad (filterM)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

spec :: TestTree
spec = testGroup "Self-containment"
  [ testCase "no absolute home path anywhere in the sources" $ do
      files <- haskellSources
      -- A sweep over nothing passes, so it says what it swept first: the module
      -- the path used to live in, and a package's worth of files around it.
      assertBool "the sweep missed the module that carried the path"
                 ("src-web/Glance/Web.hs" `elem` files)
      assertBool ("too few sources swept: " <> show (length files)) (length files >= 12)
      hits <- concat <$> mapM homePaths files
      assertEqual "sources naming an absolute home directory" [] hits

    -- A vendored file with no way to refresh it is a fork, so the loop that
    -- ends in `assets/table-view.js' has to stay written down somewhere the
    -- next reader runs into.
  , testCase "the vendored renderer has a target that refreshes it" $ do
      makefile <- TIO.readFile "Makefile"
      mapM_ (holds makefile)
            ["sync-renderer:", "../table-view/web/table-view.js", "assets/table-view.js"]
  ]

-- | WHAT is somewhere in HAYSTACK.
holds :: T.Text -> T.Text -> Assertion
holds haystack what =
  assertBool ("the Makefile does not mention " <> show what) (what `T.isInfixOf` haystack)

-- | Every Haskell file this package builds from.  The vendored GTK bindings are
-- out: they are upstream's, and are not built unless @-f native-window@ is.
haskellSources :: IO [FilePath]
haskellSources =
  concat <$> mapM under ["src", "src-query", "src-web", "src-desktop-native", "app"]
  where
    under dir = do
      entries <- map (dir </>) <$> listDirectory dir
      files <- filterM doesFileExist entries
      nested <- mapM under =<< filterM doesDirectoryExist entries
      pure (filter ((== ".hs") . takeExtension) files <> concat nested)

-- | The lines of PATH naming an absolute home directory, each with its file and
-- number, so a failure says where to look.
homePaths :: FilePath -> IO [String]
homePaths path = report . T.lines <$> TIO.readFile path
  where
    report ls = [ path <> ":" <> show n <> ": " <> T.unpack (T.strip l)
                | (n, l) <- zip [(1 :: Int) ..] ls, "/home/" `T.isInfixOf` l ]
