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

import Control.Monad (filterM, forM, forM_)
import Data.List (isPrefixOf, (\\))
import Data.Maybe (listToMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropExtension, takeExtension, (</>))
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
                   , "SURFACES", "MAPS", "socket" ])
    -- The popups take `edit' as the ACCESSOR `editNow', so the `let' itself is
    -- on the list; `!!edit' and `edit.o' name it where `openEdit' and
    -- `lediting' merely contain the letters.
  , ("40-popups.js", [ "table.", "query", "prompting", "SURFACES", "MAPS"
                     , "socket", "!!edit", "edit.o", "drows", "crows" ])
    -- The capture form takes the query, the columns, the entry on show and the
    -- landing anchor as accessors, so the bindings themselves are on its list.
  , ("30-capture.js", [ "can(table", "table.get", "SURFACES", "MAPS", "socket"
                      , "crows", "drows", "editing.", "arriving =" ]) ]

-- | The widget\'s BODY: what sits inside the wrapper, with the call site that
-- SUPPLIES the dependencies left out. The call site names the very bindings the
-- widget may not reach — that is what it is for — so checking the whole file
-- would report every accessor it was handed.
widgetBody :: T.Text -> T.Text
widgetBody = T.unlines . takeWhile (not . T.isPrefixOf "    return {") . T.lines

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

    -- A PROPOSAL'S NAME TELLS ITS STATUS, and the name is the SECOND place that
    -- fact is written — so it is CHECKED rather than kept in step by hand, which
    -- is the failure this repo keeps finding in its own documents.  The status
    -- line LEADS with the token so the comparison is a string equality and no
    -- table of prose spellings sits between the two.  The sweep says what it
    -- swept, since an empty docs directory would otherwise pass.
  , testCase "every proposal's name is the status it declares" $ do
      names <- filter ("proposal-" `isPrefixOf`) <$> listDirectory "docs"
      assertBool ("too few proposals swept: " <> show (length names)) (length names >= 20)
      wrong <- fmap concat . forM names $ \name -> do
        body <- TIO.readFile ("docs" </> name)
        let declared = case T.lines body of
              ls -> listToMaybe
                      [ T.takeWhile (/= ' ') rest
                      | l <- ls, Just rest <- [T.stripPrefix "**Status:** " l] ]
            named = case reverse (T.splitOn "." (T.pack (dropExtension name))) of
              (st:_) -> Just st
              []     -> Nothing
        pure [ (name, declared, named) | declared /= named ]
      assertEqual "a proposal whose name and status disagree" [] wrong

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
    -- `docs/proposal-widget-files.partial.md' step C.  A part joins by being wrapped
    -- and listed here, one widget at a time.
  , testCase "a wrapped widget reaches around its arguments for nothing" $
      forM_ wrappedWidgets $ \(part, forbidden) -> do
        body <- widgetBody <$> glueCode part
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
    -- THE COMMITTED ELM IS WHAT SHIPS, and nothing here rebuilds it: the suite
    -- reads `assets/elm.js' off disk, so a `make elm' that has stopped working
    -- goes green.  It bit once already — test dependencies added by hand left
    -- the indirect list short and elm refused the whole set.  What CAN be
    -- asserted offline is that the asset carries every program the target
    -- names, and that each of those sources is on disk; a module renamed,
    -- added or dropped without a rebuild fails here.
  , testCase "the committed Elm carries every program the build names" $ do
      makefile <- TIO.readFile "Makefile"
      let target = T.takeWhile (/= '\n')
                 . snd . T.breakOn "npx --yes elm make" $ makefile
          mains = [ T.drop 4 (T.dropEnd 4 w)
                  | w <- T.words target, "src/" `T.isPrefixOf` w, ".elm" `T.isSuffixOf` w ]
      assertBool ("no Elm sources named in the target: " <> show target)
                 (length mains >= 2)
      built <- TIO.readFile "assets/elm.js"
      forM_ mains $ \m -> do
        there <- doesFileExist ("assets/elm/src" </> T.unpack m <> ".elm")
        assertBool ("the target names src/" <> T.unpack m <> ".elm, which is not there") there
        assertBool ("assets/elm.js carries no " <> T.unpack m
                      <> " — `make elm' has not been run since it was named")
                   (("'" <> m <> "':") `T.isInfixOf` built)

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
