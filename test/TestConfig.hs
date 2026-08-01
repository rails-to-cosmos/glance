-- | The keyword configuration layer: what @.org-glance\/config@ makes the
-- parser recognize, and how a row's active-ness is classified once it has.
--
-- The two questions are tested apart on purpose, because they answer
-- differently.  Recognition is a UNION over every layer and reaches every file
-- under the root; classification is NEAREST SCOPE and reaches one headline.  A
-- change that collapses them passes half of this module and fails the other.
module TestConfig (spec) where

import Control.Monad ((<=<))
import Control.Concurrent.STM (readTVarIO)
import Data.Bifunctor (first)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults (orgFile, withTempDirNamed)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Org.Config ( TodoKeywords (..), classify, configDirIn, configPaths
                       , noKeywords, todoPragmas )
import Data.Org.Edit (Edit (Edit), applyEdits)
import Glance.Query ( ConfigLayerFile (..), ConfigLayers (..), HeadlineRecord (..)
                    , QueryResult (..), WalkOptions (..), builtinFilter, configEdits
                    , configPath, defaultFilter, defaultFilterOf, defaultWalk, loadDir
                    , loadDirFilesSerially, loadDirWith, loadDirWithConfig, loadFile
                    , readConfigLayers, todoLines )
import Glance.Web.Store ( Frame (..), Hub (hubStore), Store (stConfig, stGen, stPrint)
                        , loadStore, newHub, reseeded, storeKeywords, storeRecords )
import Glance.Web.Watch (settle, watched)

spec :: TestTree
spec = testGroup "Config"
  [ discoverySpec, recognitionSpec, classificationSpec, paletteSpec
  , reloadSpec, writeSpec, absenceSpec, paritySpec ]

-- Fixtures

-- | The tag config the live tree carries, verbatim: org-glance writes a title,
-- a keyword cycle and a capture template, and the template is the reason the
-- pragma lines are read on their own.
bookConfig :: Text
bookConfig = T.unlines
  [ "#+TITLE: Book"
  , "#+TODO:  TODO READING | READ ABANDONED"
  , ""
  , "* Book"
  , "*** Notes"
  , "    %?" ]

-- | A tag config that does not parse, for the same reason two of the three in
-- ~/sync do not: a hyphen inside a COMMENTED @#+TODO:@.  Its real pragma has to
-- survive that.
commentedConfig :: Text
commentedConfig = T.unlines
  [ "# Config for the `film' tag (the file name is the tag)."
  , "#   #+TODO:     a per-tag todo cycle, e.g. `TODO DOING | DONE'"
  , "#+TODO: TODO WATCHING | WATCHED"
  , ""
  , "* %?" ]

-- | A tree laid out the way org-glance lays one out, under a root of its own.
-- SYSTEM is @config\/system.org@ when given, TAGS are @config\/tags\/NAME@, and
-- DOCS are ordinary documents at the root.  STORE is the directory the config
-- hangs off, which is the root itself unless a case wants it nested.
withTreeUnder :: FilePath -> Maybe Text -> [(FilePath, Text)] -> [(FilePath, Text)]
              -> (FilePath -> IO a) -> IO a
withTreeUnder store system tags docs k = withTempDirNamed "config" $ \dir -> do
  let (systemFile, tagsDir) = configPaths (configDirIn (dir </> store))
  createDirectoryIfMissing True tagsDir
  mapM_ (TIO.writeFile systemFile) system
  mapM_ (\(n, t) -> TIO.writeFile (tagsDir </> n) t) tags
  mapM_ (\(n, t) -> TIO.writeFile (dir </> n) t) docs
  k dir

-- | 'withTreeUnder' with the config directly under the root.
withTree :: Maybe Text -> [(FilePath, Text)] -> [(FilePath, Text)]
         -> (FilePath -> IO a) -> IO a
withTree = withTreeUnder "."

-- | DIR's config and its rows, in walk order.
loaded :: FilePath -> IO (ConfigLayers, [HeadlineRecord])
loaded dir = do
  (cfg, files, _dirErrs) <- loadDirWithConfig defaultWalk dir
  pure (cfg, concat [ rs | (_path, Right rs) <- files ])

-- | Each row as its state and whether that state is active here — the pair the
-- whole classification chain exists to answer.
states :: [HeadlineRecord] -> [(Maybe Text, Maybe Bool)]
states = map (\r -> (hrState r, hrActive r))

-- | Row titles, which is where a keyword lands when it is NOT recognized.
titles :: [HeadlineRecord] -> [Text]
titles = map hrTitle

-- | The rows DOCS make under the tree SYSTEM and TAGS configure.
withRows :: Maybe Text -> [(FilePath, Text)] -> [(FilePath, Text)]
         -> ([HeadlineRecord] -> Assertion) -> Assertion
withRows system tags docs k = withTree system tags docs (k . snd <=< loaded)

-- Discovery

-- | Where the config is found, and what finding it costs the table.
discoverySpec :: TestTree
discoverySpec = testGroup "Discovery"
  [ testCase "config files are read and are never rows" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n")
               [("book.org", bookConfig)]
               [("notes.org", "* TODO a note\n")] $ \dir -> do
      qr <- loadDir dir
      assertEqual "files" 1 (qrFiles qr)
      assertEqual "rows" 1 (length (qrRecords qr))
      assertEqual "the row is the note" ["a note"] (titles (qrRecords qr))
      -- The capture template in book.org holds two headlines; neither is a row.
      assertBool "a config file reached the table"
                 (all ((== (dir </> "notes.org")) . hrFile) (qrRecords qr))

  , testCase "and --include-derived does not reach them either" $
      withTree Nothing [("book.org", bookConfig)] [("notes.org", "* TODO a note\n")] $ \dir -> do
      qr <- loadDirWith (WalkOptions True) dir
      assertEqual "files" 1 (qrFiles qr)
      assertEqual "rows" 1 (length (qrRecords qr))

  , testCase "the store hangs off wherever it sits, not off the root" $
      -- ~/sync's own shape: the walk root is the tree and the org-glance store
      -- is one directory down.  Discovery follows the walk, so it finds it.
      withTreeUnder "views" Nothing [("book.org", bookConfig)]
                    [("notes.org", "* READING War and Peace\n")] $ \dir -> do
      (cfg, rows) <- loaded dir
      assertEqual "seed" (TodoKeywords ["READING", "TODO"] ["ABANDONED", "READ"]) (clSeed cfg)
      assertEqual "the nested config still seeded the parse"
                  [(Just "READING", Just True)] (states rows)

  , testCase "a config that does not parse still declares its keywords" $
      withRows Nothing [("film.org", commentedConfig)] [("a.org", "* WATCHING Alien\n")] $ \rows ->
      assertEqual "the real pragma survived the commented one"
                  [(Just "WATCHING", Just True)] (states rows)

  , testCase "the pragma lines are what is read, comments and all" $ do
      assertEqual "the live tag config"
                  (TodoKeywords ["READING", "TODO"] ["ABANDONED", "READ"])
                  (todoPragmas bookConfig)
      assertEqual "a commented pragma is not one"
                  (TodoKeywords ["TODO", "WATCHING"] ["WATCHED"])
                  (todoPragmas commentedConfig)
      assertEqual "either casing, org takes both"
                  (TodoKeywords ["NEXT"] ["GONE"]) (todoPragmas "#+todo: NEXT | GONE\n")
      assertEqual "fast-access keys come off"
                  (TodoKeywords ["NEXT"] ["GONE"]) (todoPragmas "#+TODO: NEXT(n) | GONE(g)\n")
      assertEqual "a file with no pragma declares nothing"
                  noKeywords (todoPragmas "* TODO a note\n")

  , testCase "a config path is watched, and is not a document the walk gave" $ do
      let system = "/o/.org-glance/config/system.org"
          tag    = "/o/.org-glance/config/tags/book.org"
      assertBool "system.org is a config path" (configPath system)
      assertBool "a tag config is one too" (configPath tag)
      assertBool "an ordinary file is not" (not (configPath "/o/notes.org"))
      assertBool "and neither is the store" (not (configPath "/o/.org-glance/data/x.org"))
      -- Watched on purpose: the walk never handed it over, and a change to it
      -- changes every file that WAS handed over.
      assertBool "the watch takes system.org" (watched defaultWalk system)
      assertBool "and the tag config" (watched defaultWalk tag)
      -- The sidecar rule still applies: the live tags directory holds one.
      assertBool "an autosave beside a tag config is not watched"
                 (not (watched defaultWalk "/o/.org-glance/config/tags/#book.org#"))
  ]

-- Recognition

-- | A keyword any layer names is a keyword in every file under the root.  This
-- is the half that has to be a SUPERSET: the same headline reading as a state
-- in one file and as a title in the next is the bug the layer exists to close.
recognitionSpec :: TestTree
recognitionSpec = testGroup "Recognition"
  [ testCase "a keyword only a tag config names parses as a state elsewhere" $
      withRows Nothing [("book.org", bookConfig)]
               -- No `:book:' tag, no `#+TODO:' of its own: nothing in this file
               -- says READING is a keyword except the config.
               [("a.org", "* READING War and Peace\n")] $ \rows -> do
      assertEqual "state" [(Just "READING", Just True)] (states rows)
      assertEqual "and the title kept the rest" ["War and Peace"] (titles rows)

  , testCase "and without the config the same word is the title's first" $
      withRows Nothing [] [("a.org", "* READING War and Peace\n")] $ \rows -> do
      assertEqual "state" [(Nothing, Nothing)] (states rows)
      assertEqual "title" ["READING War and Peace"] (titles rows)

  , testCase "the STARTED class: a system keyword used bare in a data file" $
      -- The corpus case this landed for.  64 headlines under ~/sync open with
      -- STARTED and three files in the whole tree carry a `#+TODO:' line, so
      -- without a layer above the file the word is title text everywhere.
      withRows (Just "#+TODO: TODO STARTED | DONE CANCELLED\n") []
               [("a.org", "* STARTED refactor the walk\n")] $ \rows -> do
      assertEqual "recognized and classified" [(Just "STARTED", Just True)] (states rows)
      assertEqual "title" ["refactor the walk"] (titles rows)

  , testCase "recognition is the union of every layer, not the nearest one" $
      withRows (Just "#+TODO: STARTED |\n")
               [("book.org", bookConfig), ("film.org", commentedConfig)]
               [ ("a.org", "* STARTED one\n")
               , ("b.org", "* ABANDONED two\n")
               , ("c.org", "* WATCHED three\n") ] $ \rows ->
      assertEqual "each layer's keyword reaches each file"
                  [(Just "STARTED", Just True), (Just "ABANDONED", Just False)
                  ,(Just "WATCHED", Just False)]
                  (states rows)

  , testCase "a file's own pragma still adds on top, and only below itself" $
      withRows Nothing [("book.org", bookConfig)]
               [("a.org", "* LATER one\n#+TODO: LATER |\n* LATER two\n")] $ \rows -> do
      -- Append-only and positional: the pragma reaches what follows it.
      assertEqual "before the pragma, after it"
                  [(Nothing, Nothing), (Just "LATER", Just True)] (states rows)
      assertEqual "titles" ["LATER one", "two"] (titles rows)

  , testCase "the recognized set a row carries is the seed plus its own" $
      withRows Nothing [("book.org", bookConfig)] [("a.org", "#+TODO: LATER |\n* LATER one\n")] $
        \rows -> assertEqual "hrKeywords"
                   [TodoKeywords ["LATER", "READING", "TODO"] ["ABANDONED", "DONE", "READ"]]
                   (map hrKeywords rows)
  ]

-- Classification

-- | Whether a recognized keyword is active is answered by the NEAREST scope:
-- the file's own @#+TODO:@, then the headline's tags in order, then the system
-- layer, then org's TODO\/DONE, then the union.
classificationSpec :: TestTree
classificationSpec = testGroup "Classification"
  [ testCase "the file's own pragma outranks the tag config" $
      withRows Nothing [("book.org", bookConfig)]
               -- book.org calls READING active; this file calls it done-like,
               -- and the headline wears the tag that would have said otherwise.
               [("a.org", "#+TODO: | READING\n* READING done with it :book:\n")] $ \rows ->
      assertEqual "the file wins" [(Just "READING", Just False)] (states rows)

  , testCase "the tag config outranks the system layer" $
      withRows (Just "#+TODO: | READING\n") [("book.org", bookConfig)]
               [ ("a.org", "* READING tagged :book:\n")
               , ("b.org", "* READING untagged\n") ] $ \rows ->
      -- Tagged: book.org answers, active.  Untagged: nothing nearer than the
      -- system layer has anything to say, and it says done-like.
      assertEqual "tag then system"
                  [(Just "READING", Just True), (Just "READING", Just False)] (states rows)

  , testCase "the system layer outranks org's own TODO and DONE" $
      withRows (Just "#+TODO: | TODO\n") [] [("a.org", "* TODO not work here\n")] $ \rows ->
      assertEqual "system wins over the builtin" [(Just "TODO", Just False)] (states rows)

  , testCase "and with no layer at all the builtin is what answers" $
      withRows Nothing [] [("a.org", "* TODO one\n* DONE two\n")] $ \rows ->
      assertEqual "TODO active, DONE not"
                  [(Just "TODO", Just True), (Just "DONE", Just False)] (states rows)

  , testCase "the first tag with anything to say about the keyword wins" $ do
      let reading  = "#+TODO: TODO READING |\n"          -- active
          shelved  = "#+TODO: TODO | READING\n"          -- done-like
      withRows Nothing [("book.org", reading), ("pile.org", shelved)]
               [ ("a.org", "* READING one :book:pile:\n")
               , ("b.org", "* READING two :pile:book:\n")
                 -- A first tag that says nothing about the keyword does not
                 -- claim it: the next tag that does is the one that answers.
               , ("c.org", "* READING three :nosuch:pile:\n") ] $ \rows ->
        assertEqual "tag order decides"
                    [ (Just "READING", Just True), (Just "READING", Just False)
                    , (Just "READING", Just False) ]
                    (states rows)

  , testCase "a keyword no scope here claims falls back to the union" $
      -- ABANDONED is book.org's and this headline is not a book.  It is still
      -- recognized, and the only thing that has ever classified it is the layer
      -- that named it.
      withRows Nothing [("book.org", bookConfig)] [("a.org", "* ABANDONED a plan\n")] $ \rows ->
      assertEqual "done-like, as book.org has it" [(Just "ABANDONED", Just False)] (states rows)

  , testCase "a row with no keyword is in neither group" $
      withRows (Just "#+TODO: TODO STARTED | DONE\n") [] [("a.org", "* just a heading\n")] $
        \rows -> assertEqual "neither" [(Nothing, Nothing)] (states rows)

  , testCase "the resolver is the rule, and it is total" $ do
      -- The chain, exercised where the fixtures cannot reach: a keyword in no
      -- layer whatsoever still has to come back with an answer.
      let cfg = ConfigLayers { clSystem = TodoKeywords [] ["TODO"]
                             , clTags   = [("book", TodoKeywords ["READING"] [])]
                             , clSeed   = TodoKeywords ["READING"] ["TODO"]
                             , clFilter = Nothing
                             , clPrint  = ""
                             , clDirs   = [] }
          file = TodoKeywords [] ["READING"]
      assertEqual "file first" False (classify cfg file ["book"] "READING")
      assertEqual "then the tag" True (classify cfg noKeywords ["book"] "READING")
      assertEqual "then the system" False (classify cfg noKeywords [] "TODO")
      assertEqual "then the builtin" False (classify cfg noKeywords [] "DONE")
      assertEqual "then the union" True (classify cfg noKeywords [] "READING")
      assertEqual "and a word nothing names is active" True (classify cfg noKeywords [] "NOPE")
  ]

-- Palette

-- | The badge palette is the union with the config leading, which is what
-- makes its order — and so the state column's sort priority — independent of
-- which file the walk reached first.
paletteSpec :: TestTree
paletteSpec = testGroup "Palette"
  [ testCase "the config's keywords lead and the files add to them" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n") [("book.org", bookConfig)]
               [("a.org", "#+TODO: LATER |\n* LATER one\n")] $ \dir -> do
      store <- loadStore dir
      assertEqual "system, then tags, then whatever a file adds"
                  (TodoKeywords ["STARTED", "TODO", "READING", "LATER"]
                                ["DONE", "ABANDONED", "READ"])
                  (storeKeywords store)

  , testCase "and a tree with no rows still has the states it configures" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n") [] [] $ \dir -> do
      store <- loadStore dir
      assertEqual "rows" [] (map hrId (storeRecords store))
      assertEqual "badges all the same"
                  (TodoKeywords ["STARTED", "TODO"] ["DONE"]) (storeKeywords store)

  , testCase "with no config the palette is the files', as it always was" $
      withTree Nothing [] [("a.org", "#+TODO: TODO WAITING | DONE\n* WAITING one\n")] $ \dir -> do
      store <- loadStore dir
      assertEqual "palette" (TodoKeywords ["TODO", "WAITING"] ["DONE"]) (storeKeywords store)
  ]

-- Reload

-- | A config edit is the one watch event that is not about its own path: it
-- moves what every other file RECOGNIZES, so the answer is a reseed.
reloadSpec :: TestTree
reloadSpec = testGroup "Reload"
  [ testCase "an edited config reseeds the store and reparses the rows" $
      withTree (Just "#+TODO: TODO | DONE\n") [] [("a.org", "* STARTED refactor\n")] $ \dir -> do
      store <- loadStore dir
      assertEqual "before, the word is title text"
                  [(Nothing, Nothing)] (states (storeRecords store))
      hub <- newHub store
      let systemFile = fst (configPaths (configDirIn dir))
      TIO.writeFile systemFile "#+TODO: TODO STARTED | DONE\n"
      settle defaultWalk dir hub [systemFile]
      next <- readTVarIO (hubStore hub)
      assertEqual "after, it is a state"
                  [(Just "STARTED", Just True)] (states (storeRecords next))
      assertEqual "the config the store carries moved"
                  (TodoKeywords ["STARTED", "TODO"] ["DONE"]) (clSeed (stConfig next))
      assertBool "the generation moved with it" (stGen next > stGen store)

  , testCase "and the palette move closes the socket rather than streaming rows" $
      withTree (Just "#+TODO: TODO | DONE\n") [] [("a.org", "* STARTED refactor\n")] $ \dir -> do
      store <- loadStore dir
      let systemFile = fst (configPaths (configDirIn dir))
      TIO.writeFile systemFile "#+TODO: TODO STARTED | DONE\n"
      (next, frames) <- afterEdit store dir
      assertEqual "one close, no rows behind it" [ViewChanged] frames
      assertBool "the generation moved" (stGen next == stGen store + 1)

  , testCase "a config edit that moves no keyword moves no rows" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n") [] [("a.org", "* STARTED refactor\n")] $
        \dir -> do
      store <- loadStore dir
      let systemFile = fst (configPaths (configDirIn dir))
      -- A title added above the pragma: the file moved, the keywords did not.
      TIO.writeFile systemFile "#+TITLE: States\n#+TODO: TODO STARTED | DONE\n"
      (next, frames) <- afterEdit store dir
      assertEqual "nothing to say" [] frames
      assertBool "and the generation stayed put" (stGen next == stGen store)
      -- The tag still moves, because the fingerprint covers the config: the
      -- bytes deciding what these rows MEAN are not the same bytes any more.
      assertBool "the fingerprint moved on its own" (stPrint next /= stPrint store)

  , testCase "a data edit under a reseed streams rows rather than a close" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n") [] [("a.org", "* STARTED one\n")] $ \dir -> do
      store <- loadStore dir
      let systemFile = fst (configPaths (configDirIn dir))
      -- The keywords are unchanged, so the palette holds and the ordinary ops
      -- are what a client gets — which is the branch ViewChanged replaces.
      TIO.writeFile systemFile "#+TITLE: States\n#+TODO: TODO STARTED | DONE\n"
      TIO.writeFile (dir </> "a.org") "* STARTED one renamed\n"
      (next, frames) <- afterEdit store dir
      assertEqual "one upsert" 1 (length [ () | UpsertRow _ <- frames ])
      assertEqual "no deletes" 0 (length [ () | DeleteRow _ <- frames ])
      assertBool "the generation moved" (stGen next == stGen store + 1)

  , testCase "a config deleted takes its keywords with it" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n") [] [("a.org", "* STARTED refactor\n")] $
        \dir -> do
      store <- loadStore dir
      hub <- newHub store
      let systemFile = fst (configPaths (configDirIn dir))
      removeFile systemFile
      settle defaultWalk dir hub [systemFile]
      next <- readTVarIO (hubStore hub)
      assertEqual "back to title text" [(Nothing, Nothing)] (states (storeRecords next))
      assertEqual "and to org's own two"
                  (TodoKeywords ["TODO"] ["DONE"]) (storeKeywords next)

  , testCase "an ordinary file in the same window is reseeded, not re-read twice" $
      withTree (Just "#+TODO: TODO | DONE\n") [] [("a.org", "* STARTED one\n")] $ \dir -> do
      store <- loadStore dir
      hub <- newHub store
      let systemFile = fst (configPaths (configDirIn dir))
      TIO.writeFile systemFile "#+TODO: TODO STARTED | DONE\n"
      _ <- orgFile dir "b.org" "* STARTED two\n"
      -- Both paths ripen together; the reseed covers the pair.
      settle defaultWalk dir hub [dir </> "b.org", systemFile]
      next <- readTVarIO (hubStore hub)
      assertEqual "both files, both recognized"
                  [(Just "STARTED", Just True), (Just "STARTED", Just True)]
                  (states (storeRecords next))

  , testCase "an ordinary edit with no config among them is still one file" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n") [] [("a.org", "* STARTED one\n")] $ \dir -> do
      store <- loadStore dir
      hub <- newHub store
      TIO.writeFile (dir </> "a.org") "* STARTED one renamed\n"
      settle defaultWalk dir hub [dir </> "a.org"]
      next <- readTVarIO (hubStore hub)
      assertEqual "re-read under the store's own config"
                  [(Just "STARTED", Just True)] (states (storeRecords next))
      assertEqual "title" ["one renamed"] (titles (storeRecords next))
  ]

-- | DIR loaded again and diffed against BEFORE — the pure half of what the
-- watch does with a config event, which is where the frames are readable.
afterEdit :: Store -> FilePath -> IO (Store, [Frame])
afterEdit before dir = (`reseeded` before) <$> loadStore dir

-- Writing a layer

-- | A config file is edited the way every other file is: the @#+TODO:@ lines
-- are located as spans and spliced, so a capture template around them is bytes
-- the write never names.  The grammar of what may go in is checked ahead of
-- the write, since a layer that parses as nothing looks configured and does
-- nothing.
writeSpec :: TestTree
writeSpec = testGroup "Writing a layer"
  [ testCase "the lines a layer declares are the ones it is edited by" $ do
      assertEqual "the live tag config's, verbatim"
                  ["#+TODO:  TODO READING | READ ABANDONED"] (todoLines bookConfig)
      -- The commented one opens with `#', not `#+', so it is not a line here
      -- either — the same rule that keeps it out of 'todoPragmas'.
      assertEqual "a commented pragma is not one"
                  ["#+TODO: TODO WATCHING | WATCHED"] (todoLines commentedConfig)
      assertEqual "a file with none" [] (todoLines "* TODO a note\n")

  , testCase "a block replaces the one that is there and nothing around it" $
      assertEqual "only the pragma line moved"
                  (Right "#+TITLE: Book\n#+TODO: TODO NEXT | DONE\n\n* Book\n*** Notes\n    %?\n")
                  (spliced bookConfig ["#+TODO: TODO NEXT | DONE"])

  , testCase "a file spelling its cycle twice comes back spelling it once" $
      -- The first line's offset is kept and every later one goes: a block is
      -- what the sheet edits, so what it writes is the whole of the file's.
      assertEqual "one line, where the first one was"
                  (Right "#+TITLE: X\n#+TODO: A | B\ntail\n")
                  (spliced "#+TITLE: X\n#+TODO: A |\n#+TODO: | B\ntail\n" ["#+TODO: A | B"])

  , testCase "a file with no block takes one under the header it opens with" $ do
      assertEqual "after the #+ run, ahead of the content"
                  (Right "#+TITLE: Film\n#+TODO: A | B\n\n* %?\n")
                  (spliced "#+TITLE: Film\n\n* %?\n" ["#+TODO: A | B"])
      assertEqual "at the top when it opens with content"
                  (Right "#+TODO: A | B\n* %?\n") (spliced "* %?\n" ["#+TODO: A | B"])
      -- The shape a create takes: the block is the whole file.
      assertEqual "and a file that is not there yet is all block"
                  (Right "#+TODO: A | B\n") (spliced "" ["#+TODO: A | B"])
      -- The one insertion point that is not a line start: a document of nothing
      -- but header, not closed with a newline.  Appended bare, the block would
      -- land on the end of a live line and be no pragma at all.
      assertEqual "a header with no newline gets one first"
                  (Right "#+TITLE: X\n#+TODO: A | B\n")
                  (spliced "#+TITLE: X" ["#+TODO: A | B"])

  , testCase "an empty block deletes the lines, and is the no-op without them" $ do
      assertEqual "the template survives the deletion"
                  (Right "#+TITLE: Book\n\n* Book\n*** Notes\n    %?\n")
                  (spliced bookConfig [])
      assertEqual "blank lines are not lines" (Right "#+TITLE: X\n")
                  (spliced "#+TITLE: X\n" ["", "  "])

  , testCase "what a layer may say, and what it may not" $ do
      mapM_ (\(what, lines') ->
               assertBool what (either (const True) (const False)
                                       (configEdits bookConfig lines' Nothing)))
            [ ("a headline is not a pragma", ["* TODO not a pragma"])
            , ("nor is a title", ["#+TITLE: no"])
            , ("a pragma declaring nothing", ["#+TODO:"])
            , ("one bad line spoils the block", ["#+TODO: A | B", "oops"])
              -- The pragma test is a PREFIX test, so an entry smuggling a
              -- newline would pass it and write everything behind that newline
              -- into the file unread.  One line per line is the whole of what
              -- makes this a #+TODO:-only splice.
            , ("a line carrying a newline of its own", ["#+TODO: A | B\n* not a pragma"])
              -- The parser's keyword token is letters and underscores, so the
              -- filter's group names cannot be declared as keywords at all —
              -- the same wall `setStateEdits' puts up from the other side.
            , ("the filter's group names", ["#+TODO: *active* | *inactive*"])
            , ("and a starred word beside real ones", ["#+TODO: TODO *x* | DONE"]) ]
      assertBool "a cycle with fast-access keys is a block"
                 (either (const False) (const True)
                         (configEdits bookConfig ["#+TODO: TODO(t) | DONE(d)"] Nothing))

    -- The default view is a line of `system.org', read by the same reader and
    -- written by the same splice.  Absent means the built-in, which is what
    -- keeps a tree that has never been configured opening on its unfinished
    -- work rather than on nothing.
  , testCase "the default view is a line of the system layer" $ do
      assertEqual "read off the file"
                  (Just "tag:work") (defaultFilterOf "#+GLANCE_DEFAULT_FILTER: tag:work\n")
      assertEqual "folded, the way org reads a pragma key"
                  (Just "tag:work") (defaultFilterOf "#+glance_default_filter: tag:work\n")
      assertEqual "a file with no line names none" Nothing (defaultFilterOf bookConfig)
      -- A LAST-line rule: a reader scrolling the file reads the one at the
      -- bottom, and so does this.
      assertEqual "the last one wins" (Just "b")
                  (defaultFilterOf "#+GLANCE_DEFAULT_FILTER: a\n#+GLANCE_DEFAULT_FILTER: b\n")
      -- A line naming nothing is a query naming nothing, which is the whole
      -- store; only an ABSENT line falls back.
      assertEqual "and a line with nothing on it is the empty query"
                  (Just "") (defaultFilterOf "#+GLANCE_DEFAULT_FILTER:\n")

  , testCase "with no line anywhere the built-in is what answers" $
      withTree Nothing [] [("a.org", "* TODO x\n")] $ \dir -> do
        (cfg, _rows) <- loaded dir
        assertEqual "nothing configured" Nothing (clFilter cfg)
        assertEqual "so the tree opens on the active group" builtinFilter (defaultFilter cfg)

  , testCase "and the system layer's line is what the tree opens on" $
      withTree (Just "#+TODO: TODO | DONE\n#+GLANCE_DEFAULT_FILTER: tag:work\n")
               [] [("a.org", "* TODO x\n")] $ \dir -> do
        (cfg, _rows) <- loaded dir
        assertEqual "read at load" (Just "tag:work") (clFilter cfg)
        assertEqual "and it is what answers" "tag:work" (defaultFilter cfg)

    -- One file, one write, one lock: the cycle and the default view are lines of
    -- the same document, so they ride in one splice under one digest.
  , testCase "the default view is written by the same splice as the cycle" $ do
      assertEqual "written under the header, beside the block"
                  (Right "#+TITLE: X\n#+TODO: A | B\n#+GLANCE_DEFAULT_FILTER: tag:work\n")
                  (splicedWith "#+TITLE: X\n" ["#+TODO: A | B"] (Just "tag:work"))
      assertEqual "an existing line is replaced where it stands"
                  (Right "#+GLANCE_DEFAULT_FILTER: tag:home\n#+TODO: A | B\ntail\n")
                  (splicedWith "#+GLANCE_DEFAULT_FILTER: tag:work\n#+TODO: A | B\ntail\n"
                               ["#+TODO: A | B"] (Just "tag:home"))
      assertEqual "an empty one takes the line away, which is the built-in back"
                  (Right "#+TODO: A | B\ntail\n")
                  (splicedWith "#+GLANCE_DEFAULT_FILTER: tag:work\n#+TODO: A | B\ntail\n"
                               ["#+TODO: A | B"] (Just ""))
      -- Absent is not empty: a tag layer's write names no filter at all, and the
      -- system layer's line is none of its business.
      assertEqual "and naming none leaves the line exactly as it is"
                  (Right "#+GLANCE_DEFAULT_FILTER: tag:work\n#+TODO: A | B\ntail\n")
                  (splicedWith "#+GLANCE_DEFAULT_FILTER: tag:work\n#+TODO: A | B\ntail\n"
                               ["#+TODO: A | B"] Nothing)
      -- Both pragmas missing insert at one offset, which the engine resolves in
      -- list order rather than refusing.
      assertEqual "a file with neither takes both, cycle first"
                  (Right "#+TODO: A | B\n#+GLANCE_DEFAULT_FILTER: tag:work\n* %?\n")
                  (splicedWith "* %?\n" ["#+TODO: A | B"] (Just "tag:work"))

  , testCase "the layers are read as files, absent ones included" $
      withTree Nothing [("book.org", bookConfig)] [] $ \dir -> do
      files <- readConfigLayers [configDirIn dir]
      assertEqual "the system file, then the tag configs"
                  [Nothing, Just "book"] (map lfTag files)
      assertEqual "an absent file has no digest and no text"
                  [True, False] (map (T.null . lfDigest) files)
      assertEqual "and the one that is there holds what it holds"
                  [[], ["#+TODO:  TODO READING | READ ABANDONED"]]
                  (map (todoLines . lfText) files)

    -- The whole loop, without the socket: a write lands, the watch sees a
    -- config path and reseeds, and the palette a client would be handed moves.
  , testCase "a layer written by hand reseeds the tree the watch is holding" $
      withTree Nothing [] [("a.org", "* STARTED refactor\n")] $ \dir -> do
      store <- loadStore dir
      assertEqual "before, the word is title text"
                  [(Nothing, Nothing)] (states (storeRecords store))
      hub <- newHub store
      let systemFile = fst (configPaths (configDirIn dir))
      written <- either (assertFailure . T.unpack) pure
                        (spliced "" ["#+TODO: TODO STARTED | DONE"])
      TIO.writeFile systemFile written
      settle defaultWalk dir hub [systemFile]
      next <- readTVarIO (hubStore hub)
      assertEqual "after, it is a state"
                  [(Just "STARTED", Just True)] (states (storeRecords next))
      assertEqual "and the palette carries it"
                  (TodoKeywords ["STARTED", "TODO"] ["DONE"]) (storeKeywords next)
  ]

-- | DOC with LINES as its @#+TODO:@ block, spliced the way the route splices
-- it: 'configEdits' for the spans and the write engine's own 'applyEdits' for
-- the result, so what is asserted is the document a write would leave behind.
spliced :: Text -> [Text] -> Either Text Text
spliced doc lines' = splicedWith doc lines' Nothing

-- | 'spliced', also setting the default view to WANT.
splicedWith :: Text -> [Text] -> Maybe Text -> Either Text Text
splicedWith doc lines' want = do
  edits <- configEdits doc lines' want
  first (T.pack . show) (applyEdits doc [ Edit sp new | (sp, new) <- edits ])

-- Absence

-- | With no config anywhere, every answer has to be the one this repo gave
-- before the layer existed.
absenceSpec :: TestTree
absenceSpec = testGroup "No config"
  [ testCase "a tree without one loads exactly as loadFile does" $
      withTempDirNamed "config" $ \dir -> do
      path <- orgFile dir "a.org" "#+TODO: TODO WAITING | DONE\n* WAITING one :x:\n* DONE two\n"
      (cfg, rows) <- loaded dir
      assertEqual "no layers" (TodoKeywords [] []) (clSeed cfg)
      assertEqual "nothing to fingerprint" "" (clPrint cfg)
      bare <- loadFile path >>= either (assertFailure . show) pure
      assertEqual "row for row" (map shape bare) (map shape rows)

  , testCase "and its rows classify by the file and the builtin alone" $
      withTempDirNamed "config" $ \dir -> do
      _ <- orgFile dir "a.org" "#+TODO: NEXT | GONE\n* NEXT one\n* GONE two\n* TODO three\n"
      (_cfg, rows) <- loaded dir
      assertEqual "the file's two, then org's own"
                  [ (Just "NEXT", Just True), (Just "GONE", Just False)
                  , (Just "TODO", Just True) ]
                  (states rows)
  ]

-- | The fields a row is compared by: everything the wire carries off it.
shape :: HeadlineRecord -> (FilePath, Text, Maybe Text, Text, Text, Maybe Bool, TodoKeywords)
shape r = (hrFile r, hrId r, hrState r, hrTitle r, hrTags r, hrActive r, hrKeywords r)

-- Parity

-- | The pool and the serial loop see one config, and see it the same way.
paritySpec :: TestTree
paritySpec = testGroup "Parity"
  [ testCase "a serial load of a configured tree equals a parallel one" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n") [("book.org", bookConfig)]
               [ ("a.org", "* STARTED one\n")
               , ("b.org", "* READING two :book:\n")
               , ("c.org", "* ABANDONED three\n")
               , ("d.org", "#+TODO: LATER |\n* LATER four\n") ] $ \dir -> do
      (serial, _errs) <- loadDirFilesSerially defaultWalk dir
      (_cfg, parallel, _dirErrs) <- loadDirWithConfig defaultWalk dir
      assertEqual "paths" (map fst serial) (map fst parallel)
      assertEqual "rows"
                  [ map shape rs | (_p, Right rs) <- serial ]
                  [ map shape rs | (_p, Right rs) <- parallel ]
      -- And what the tree actually reads as, so the equality above is not two
      -- copies of a wrong answer.
      let rows = concat [ rs | (_p, Right rs) <- parallel ]
      assertEqual "states"
                  [ (Just "STARTED", Just True), (Just "READING", Just True)
                  , (Just "ABANDONED", Just False), (Just "LATER", Just True) ]
                  (states rows)
  ]
