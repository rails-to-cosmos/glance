-- | Repo hygiene, and what no test that drives the server can see.
module TestSelfContained (spec) where

import Control.Monad (filterM, forM, forM_)
import Data.List (isPrefixOf, nub, (\\))
import Data.Maybe (listToMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropExtension, takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Glance.Web.Base (gluePartFiles)
import TestDefaults (holdsAll, valueAfter)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

wrappedWidgets :: [(FilePath, [T.Text])]
wrappedWidgets =
  [ ("05-keys.js", [ "table.", "cols", "query", "editing", "prompting"
                   , "SURFACES", "MAPS", "socket" ])
    -- `!!edit' and `edit.o' name the `let'; `openEdit' merely holds the letters.
  , ("40-popups.js", [ "table.", "query", "prompting", "SURFACES", "MAPS"
                     , "socket", "!!edit", "edit.o", "drows", "crows" ])
  , ("30-capture.js", [ "can(table", "table.get", "SURFACES", "MAPS", "socket"
                      , "crows", "drows", "editing.", "arriving =" ]) ]

-- | Inside the wrapper alone — the call site names the very forbidden bindings.
widgetBody :: T.Text -> T.Text
widgetBody = T.unlines . takeWhile (not . T.isPrefixOf "    return {") . T.lines

declares :: T.Text -> (T.Text -> T.Text) -> T.Text -> Maybe T.Text
declares marker cut body = listToMaybe
  [ cut (T.drop (T.length marker) rest)
  | l <- T.lines body, let rest = snd (T.breakOn marker l), not (T.null rest) ]

word, day :: T.Text -> T.Text
word = T.takeWhile (/= ' ')
day  = T.take 10

-- | The members of every @…Ports@ interface: a name whose type opens a brace,
-- which is what an Elm port compiles to.  A member may span lines, so the
-- interface is what bounds the read.
portsIn :: T.Text -> [T.Text]
portsIn = go False . map T.strip . T.lines
  where
    go _ [] = []
    go inside (l:ls)
      | "interface " `T.isPrefixOf` l = go ("Ports {" `T.isSuffixOf` l) ls
      | inside && l == "}"            = go False ls
      | inside, (name, rest) <- T.breakOn ":" l
      , "{" `T.isPrefixOf` T.strip (T.drop 1 rest) = T.strip name : go inside ls
      | otherwise                     = go inside ls

-- | PART with its comment-only lines out, so a name in prose is not a reach.
glueCode :: FilePath -> IO T.Text
glueCode part = strip <$> TIO.readFile ("frontend/glue" </> part)
  where strip = T.unlines . filter (not . T.isPrefixOf "//" . T.stripStart) . T.lines

spec :: TestTree
spec = testGroup "Self-containment"
  [ testCase "no absolute home path anywhere in the sources" $ do
      files <- haskellSources
      -- A sweep over nothing passes, so it says what it swept first.
      assertBool "the sweep missed the module that carried the path"
                 ("src-web/Glance/Web.hs" `elem` files)
      assertBool ("too few sources swept: " <> show (length files)) (length files >= 12)
      hits <- concat <$> mapM homePaths files
      assertEqual "sources naming an absolute home directory" [] hits

    -- tsc reports clean over whatever it is handed, so its own list is checked.
  , testCase "the type checker reads the parts the build does" $ do
      conf <- TIO.readFile "frontend/jsconfig.json"
      assertBool "the part list is empty" (length gluePartFiles >= 2)
      assertEqual "a part the build reads and jsconfig.json does not" []
                  [ part | part <- gluePartFiles
                         , not (T.pack ("glue/" <> part) `T.isInfixOf` conf) ]

    -- The NAME is the second place each fact is written, so both are checked.
  , testCase "every proposal's name is the date and status it declares" $ do
      let sidecar n = (("#" `isPrefixOf` n) && ("#" `isPrefixOf` reverse n))
                        || (".#" `isPrefixOf` n)
      names <- filter (not . sidecar) <$> listDirectory "docs/proposals"
      assertBool ("too few proposals swept: " <> show (length names)) (length names >= 35)
      wrong <- fmap concat . forM names $ \name -> do
        body <- TIO.readFile ("docs/proposals" </> name)
        let stem = T.pack (dropExtension name)
            namedStatus = case reverse (T.splitOn "." stem) of
              (st:_) -> Just st
              []     -> Nothing
            declaredStatus = declares "**Status:** " word body
            declaredDate = declares "**Date:** " day body
        pure $ [ (name, "status" :: T.Text, declaredStatus, namedStatus)
               | declaredStatus /= namedStatus ]
            <> [ (name, "date", declaredDate, Just (day stem))
               | declaredDate /= Just (day stem) ]
      assertEqual "a proposal whose name and header disagree" [] wrong

    -- Both decoders end `_ -> D.succeed Ignore', so a mis-spelt kind is dropped
    -- without a word.  A UNION on purpose: `flagPort' serves whichever program
    -- holds its rows, so four kinds are legitimately sent to both.
  , testCase "every port kind the shell sends is one an Elm program decodes" $ do
      doc <- kindsIn "frontend/elm/src/Doc.elm"
      list <- kindsIn "frontend/elm/src/Listing.elm"
      sent <- concat <$> mapM (sendsIn . ("frontend/glue" </>)) gluePartFiles
      -- NUB BOTH SIDES: `\\\\' drops ONE occurrence per element, and four kinds are
      -- decoded by both programs.
      let decoded = nub (doc <> list)
      -- A sweep over nothing passes, so it says what it swept first.
      assertBool ("too few kinds swept: " <> show (length decoded, length sent))
                 (length decoded >= 10 && length sent >= 15)
      assertEqual "a kind the shell sends and no program decodes" []
                  (nub sent \\ decoded)
      assertEqual "a decoder branch nothing sends" []
                  (decoded \\ nub sent)

  , testCase "the shell's parts are the whole of the shell" $ do
      assertBool "the part list is empty" (length gluePartFiles >= 2)
      missing <- filterM (fmap not . doesFileExist . ("frontend/glue" </>)) gluePartFiles
      assertEqual "parts the build names and the repo lacks" [] missing
      found <- filter ((== ".js") . takeExtension) <$> listDirectory "frontend/glue"
      assertEqual "a part on disk the build never reads" [] (found \\ gluePartFiles)
      stray <- doesFileExist "frontend/glue.js"
      assertBool "frontend/glue.js is back — the parts are the source" (not stray)

    -- A MUST-NOT-APPEAR LIST: an allowlist over a shared script scope cannot
    -- tell a local `t' from a foreign one without a parser.  AGENTS.hs, step C.
  , testCase "a wrapped widget reaches around its arguments for nothing" $
      forM_ wrappedWidgets $ \(part, forbidden) -> do
        body <- widgetBody <$> glueCode part
        let reached = [ name | name <- forbidden, name `T.isInfixOf` body ]
        assertEqual (part <> " reaches past its argument list") [] reached

  , testCase "a wrapped widget declares its dependencies in its header" $
      forM_ wrappedWidgets $ \(part, _forbidden) -> do
        body <- glueCode part
        assertBool (part <> " is not wrapped: no `((deps) => {' header")
                   ("= ((" `T.isInfixOf` body && "})(" `T.isInfixOf` body)

    -- NOTHING HERE REBUILDS THE ELM, so a broken `make elm' ships green.  What
    -- is assertable offline: the asset carries every program the target names.
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
        there <- doesFileExist ("frontend/elm/src" </> T.unpack m <> ".elm")
        assertBool ("the target names src/" <> T.unpack m <> ".elm, which is not there") there
        assertBool ("assets/elm.js carries no " <> T.unpack m
                      <> " — `make elm' has not been run since it was named")
                   (("'" <> m <> "':") `T.isInfixOf` built)

    -- `tsc' READS ONE SIDE.  It catches the glue calling a port Elm lacks; a
    -- port DECLARED here and absent there typechecks and dies at boot with
    -- `undefined.subscribe is not a function', so the two lists are joined.
  , testCase "the ports the glue is typed against are the ports Elm declares" $ do
      declared <- portsIn <$> TIO.readFile "frontend/glue.d.ts"
      elmSrc <- mapM (TIO.readFile . ("frontend/elm/src" </>)) ["Doc.elm", "Listing.elm"]
      let exposed = [ T.takeWhile (/= ' ') (T.strip rest)
                    | l <- concatMap T.lines elmSrc
                    , Just rest <- [T.stripPrefix "port " l]
                    , not ("module" `T.isPrefixOf` T.strip rest) ]
      assertBool ("too few ports swept: " <> show declared) (length declared >= 5)
      assertEqual "a port glue.d.ts declares and no Elm module has"
        [] [ T.unpack p | p <- declared, p `notElem` exposed ]
      assertEqual "a port Elm exposes and glue.d.ts never types"
        [] [ T.unpack p | p <- exposed, p `notElem` declared ]

  , testCase "the vendored renderer has a target that refreshes it" $ do
      makefile <- TIO.readFile "Makefile"
      holdsAll "the Makefile no longer refreshes the vendored renderer"
               ["sync-renderer:", "../table-view/web/table-view.js", "assets/table-view.js"]
               makefile

    -- The splice door is 'Glance.Web.Watch.writeSpans'; comments are exempt.
    -- @delete@ MOVES a blob and is outside this sweep — see AGENTS.hs.
  , testCase "replaceSpans is spliced through the watch and nowhere else" $ do
      files <- filter ("src-web/" `isPrefixOf`) <$> haskellSources
      assertBool ("too few web sources swept: " <> show (length files)) (length files >= 12)
      inWatch <- calls "src-web/Glance/Web/Watch.hs"
      assertBool "the sweep missed the one legitimate call" (not (null inWatch))
      hits <- concat <$> mapM calls [ f | f <- files, f /= "src-web/Glance/Web/Watch.hs" ]
      assertEqual "web modules splicing outside the write door" [] hits

    -- ONE VERSION, FOUR PLACES, and a cut moves them together.  The cabal file
    -- is the truth; the rest are copies, and a copy nobody compares drifts.
  , testCase "the version agrees wherever it is written" $ do
      cabalV <- valueAfter "version:" <$> TIO.readFile "glance.cabal"
      readmeV <- between "*Version:* =" "=" <$> TIO.readFile "README.org"
      logV <- newestRelease <$> TIO.readFile "CHANGELOG.md"
      elmV <- between "\"elm-version\": \"" "\"" <$> TIO.readFile "frontend/elm/elm.json"
      assertBool "glance.cabal names no version" (maybe False (not . T.null) cabalV)
      assertEqual "README.org against glance.cabal" cabalV readmeV
      assertEqual "CHANGELOG.md's newest release against glance.cabal" cabalV logV
      -- 0.19.1 is a hard refusal, so the pin is asserted rather than compared.
      assertEqual "frontend/elm/elm.json's compiler pin" (Just "0.19.2") elmV
  ]

-- | What sits between OPEN and the next CLOSE, first occurrence.
between :: T.Text -> T.Text -> T.Text -> Maybe T.Text
between open close body = case T.breakOn open body of
  (_, rest) | not (T.null rest) ->
    Just (fst (T.breakOn close (T.drop (T.length open) rest)))
  _noneOfIt -> Nothing

-- | The newest @## X - DATE@ heading, X alone.  @## Unreleased@ names no
-- version and carries no separator, so it is skipped rather than filtered.
newestRelease :: T.Text -> Maybe T.Text
newestRelease body = listToMaybe
  [ T.strip v
  | l <- T.lines body, Just rest <- [T.stripPrefix "## " l]
  , let v = fst (T.breakOn " - " rest), v /= rest ]

calls :: FilePath -> IO [String]
calls path = report . T.lines <$> TIO.readFile path
  where
    report ls = [ path <> ":" <> show n <> ": " <> T.unpack stripped
                | (n, l) <- zip [(1 :: Int) ..] ls
                , let stripped = T.strip l
                , "replaceSpans" `T.isInfixOf` stripped
                , not ("--" `T.isPrefixOf` stripped) ]

haskellSources :: IO [FilePath]
haskellSources =
  concat <$> mapM under ["src", "src-query", "src-web", "src-desktop-native", "app"]
  where
    under dir = do
      entries <- map (dir </>) <$> listDirectory dir
      files <- filterM doesFileExist entries
      nested <- mapM under =<< filterM doesDirectoryExist entries
      pure (filter ((== ".hs") . takeExtension) files <> concat nested)

homePaths :: FilePath -> IO [String]
homePaths path = report . T.lines <$> TIO.readFile path
  where
    report ls = [ path <> ":" <> show n <> ": " <> T.unpack (T.strip l)
                | (n, l) <- zip [(1 :: Int) ..] ls, "/home/" `T.isInfixOf` l ]

-- | The kind strings a program's decoder answers to: a @case@ arm over the wire's own
--   word, @"step" ->@.
kindsIn :: FilePath -> IO [T.Text]
kindsIn path = branches <$> TIO.readFile path
  where
    branches body =
      [ T.takeWhile (/= '"') (T.drop 1 line)
      | raw <- T.lines body
      , let line = T.strip raw
      , "\"" `T.isPrefixOf` line
      , "\" ->" `T.isSuffixOf` line ]

-- | The kind strings a glue part sends: @kind: "step"@.
sendsIn :: FilePath -> IO [T.Text]
sendsIn path = quoted "kind: \"" <$> TIO.readFile path

-- | Every word OPENER introduces, up to its closing quote.
quoted :: T.Text -> T.Text -> [T.Text]
quoted opener body =
  [ T.takeWhile (/= '"') rest | rest <- drop 1 (T.splitOn opener body) ]
