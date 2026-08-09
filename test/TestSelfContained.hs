-- | Repo hygiene rather than behaviour: nothing this package builds names a
-- path outside the repository, and the one file it vendors has a way to be
-- refreshed.  Both were bought at a price — the renderer was read at run time
-- out of one machine's home directory until 2026-08-02, so a correct build
-- served a table-less page on every other machine — and neither is visible to a
-- test that drives the server, which is why they live in a module of their own
-- rather than beside the routes.
--
-- A ROUTE RESOLVING THE STORE TWICE was the third rule here and is gone.  It
-- gave the right answer and cost double, which is exactly why no test that
-- drove the server could see it: @\/tags@ owes two folds over the rows, and
-- every 'Glance.Web.Store.storeRecords' is a whole id resolution (~28 ms over
-- the 10435-row @~\/sync@ tree, 2026-08-03).  A grep over @tagsView@'s own
-- source lines guarded it until 2026-08-03, when the design took the hazard
-- away instead: 'Glance.Web.Store' offers nothing that takes a 'Store' and
-- answers about an id, so a second resolution has no spelling left.  The rule
-- is that module's export list now, and a test reading source text for it would
-- be re-asserting the type checker.
module TestSelfContained (spec) where

import Control.Monad (filterM, forM_)
import Data.List (isPrefixOf, (\\))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Glance.Web.Base (gluePartFiles)
import TestDefaults (holdsAll)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | The parts converted to step C, each with the shell state it must not name.
-- The renderer handle and the shell's own view state are what a widget would
-- reach for around its arguments, and what a port to another language could
-- not take with it.
wrappedWidgets :: [(FilePath, [T.Text])]
wrappedWidgets =
  [ ("05-keys.js", [ "table.", "cols", "query", "editing", "prompting"
                   , "SURFACES", "MAPS", "socket" ]) ]

-- | PART with its comment-only lines out, so a name in prose is not a reach.
glueCode :: FilePath -> IO T.Text
glueCode part = strip <$> TIO.readFile ("assets/glue" </> part)
  where strip = T.unlines . filter (not . T.isPrefixOf "//" . T.stripStart) . T.lines

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

    -- ONE SOURCE FOR THE SHELL.  The parts are what the build reads and what a
    -- served directory concatenates, so a whole `glue.js' beside them would be
    -- a second copy to keep in step.
  , testCase "the shell's parts are the whole of the shell" $ do
      assertBool "the part list is empty" (length gluePartFiles >= 2)
      missing <- filterM (fmap not . doesFileExist . ("assets/glue" </>)) gluePartFiles
      assertEqual "parts the build names and the repo lacks" [] missing
      found <- filter ((== ".js") . takeExtension) <$> listDirectory "assets/glue"
      assertEqual "a part on disk the build never reads" [] (found \\ gluePartFiles)
      stray <- doesFileExist "assets/glue.js"
      assertBool "assets/glue.js is back — the parts are the source" (not stray)

    -- A WIDGET REACHES ONLY WHAT IT WAS HANDED — and JS does nothing to hold
    -- that, since the parts share one script scope and an IIFE still sees every
    -- name around it.  The argument list DOCUMENTS the boundary; this keeps it.
    --
    -- A MUST-NOT-APPEAR LIST rather than an allowlist, the idiom this suite
    -- already uses for renderer internals: an allowlist over a shared scope
    -- cannot tell a local `t' from a foreign one without a parser, where a
    -- denylist over the names that MATTER is exact.  What matters is the state
    -- a widget must not reach around its arguments for.
    --
    -- `docs/proposal-widget-files.md' step C.  A part joins by being wrapped
    -- and listed here, one widget at a time.
  , testCase "a wrapped widget reaches around its arguments for nothing" $
      forM_ wrappedWidgets $ \(part, forbidden) -> do
        body <- glueCode part
        let reached = [ name | name <- forbidden, name `T.isInfixOf` body ]
        assertEqual (part <> " reaches past its argument list") [] reached

    -- And the wrapper says what it takes, so the list above and the code agree.
  , testCase "a wrapped widget declares its dependencies in its header" $
      forM_ wrappedWidgets $ \(part, _forbidden) -> do
        body <- glueCode part
        assertBool (part <> " is not wrapped: no `((deps) => {' header")
                   ("= ((" `T.isInfixOf` body && "})(" `T.isInfixOf` body)

    -- A vendored file with no way to refresh it is a fork, so the loop that
    -- ends in `assets/table-view.js' has to stay written down somewhere the
    -- next reader runs into.
  , testCase "the vendored renderer has a target that refreshes it" $ do
      makefile <- TIO.readFile "Makefile"
      holdsAll "the Makefile no longer refreshes the vendored renderer"
               ["sync-renderer:", "../table-view/web/table-view.js", "assets/table-view.js"]
               makefile

    -- THE WRITE DOOR IS ONE FUNCTION.  Every write this daemon makes leaves
    -- through 'Glance.Web.Watch.writeSpans', which queues the path it just wrote
    -- — a blob's shard is created and never watched, so a route splicing through
    -- 'Glance.Query.replaceSpans' itself would write the file correctly and
    -- deliver nothing until a restart.  Nothing in the types says so and the
    -- import is one line, which is why the rule is swept for rather than relied
    -- on.  Comments are exempt: four of them name the function to explain it.
  , testCase "replaceSpans is spliced through the watch and nowhere else" $ do
      files <- filter ("src-web/" `isPrefixOf`) <$> haskellSources
      assertBool ("too few web sources swept: " <> show (length files)) (length files >= 12)
      inWatch <- calls "src-web/Glance/Web/Watch.hs"
      assertBool "the sweep missed the one legitimate call" (not (null inWatch))
      hits <- concat <$> mapM calls [ f | f <- files, f /= "src-web/Glance/Web/Watch.hs" ]
      assertEqual "web modules splicing outside the write door" [] hits
  ]

-- | The lines of PATH that CALL @replaceSpans@ — every mention of it that is
-- not a comment — each with its file and number.
calls :: FilePath -> IO [String]
calls path = report . T.lines <$> TIO.readFile path
  where
    report ls = [ path <> ":" <> show n <> ": " <> T.unpack stripped
                | (n, l) <- zip [(1 :: Int) ..] ls
                , let stripped = T.strip l
                , "replaceSpans" `T.isInfixOf` stripped
                , not ("--" `T.isPrefixOf` stripped) ]

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
