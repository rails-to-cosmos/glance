-- | The keyword configuration layer: what @.org-glance\/config@ makes the
-- parser recognize, and how a row's active-ness is classified once it has.
--
-- The two questions are tested apart on purpose, because they answer
-- differently.  Recognition is a UNION over every layer and reaches every file
-- under the root; classification is WIDEST SCOPE and reaches one headline.  A
-- change that collapses them passes half of this module and fails the other.
module TestConfig (spec) where

import Control.Monad ((<=<))
import Control.Concurrent.STM (readTVarIO)
import Data.Bifunctor (first)
import Data.List (sort)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults (orgFile, refusedNaming, systemFileIn, tagsDirIn, withTempDirNamed)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Org.Config ( TodoKeywords (..), classify, configDirIn, noKeywords
                       , todoPragmas )
import Data.Org.Edit (Edit (Edit), applyEdits)
import Glance.Query ( ConfigLayerFile (..), ConfigLayers (..), ConfigParts (..)
                    , SavedView (..), savedView, savedViews, viewOf
                    , HeadlineRecord (..)
                    , QueryResult (..), WalkOptions (..), builtinFilter
                    , captureTargetIn, captureTargetOf, configEdits
                    , configPath, defaultFilter, defaultSortChain
                    , defaultWalk, loadDir
                    , loadDirFilesSerially, loadDirWith, loadDirWithConfig, loadFile
                    , noConfig, noParts, readConfigLayers, sortedForViewWith
                    , todoLines )
import Glance.Web.Store ( CloseReason (..), Frame (..), Hub (hubStore), RowOp (..)
                        , Store (stConfig, stGen, stPrint)
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
  let root = dir </> store
      tagsDir = tagsDirIn root
  createDirectoryIfMissing True tagsDir
  mapM_ (TIO.writeFile (systemFileIn root)) system
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
      assertEqual "seed" (TodoKeywords ["TODO", "READING"] ["READ", "ABANDONED"]) (clSeed cfg)
      assertEqual "the nested config still seeded the parse"
                  [(Just "READING", Just True)] (states rows)

  , testCase "a config that does not parse still declares its keywords" $
      withRows Nothing [("film.org", commentedConfig)] [("a.org", "* WATCHING Alien\n")] $ \rows ->
      assertEqual "the real pragma survived the commented one"
                  [(Just "WATCHING", Just True)] (states rows)

  , testCase "the pragma lines are what is read, comments and all" $ do
      assertEqual "the live tag config"
                  (TodoKeywords ["TODO", "READING"] ["READ", "ABANDONED"])
                  (todoPragmas bookConfig)
      assertEqual "a commented pragma is not one"
                  (TodoKeywords ["TODO", "WATCHING"] ["WATCHED"])
                  (todoPragmas commentedConfig)
      assertEqual "and the keywords keep the order the line spells them in"
                  (TodoKeywords ["WAITING", "TODO"] ["CANCELLED", "DONE"])
                  (todoPragmas "#+TODO: WAITING TODO | CANCELLED DONE\n")
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

    -- Recognition reaches every file; classification does not follow it there.
    -- ABANDONED and WATCHED parse as states in files that carry neither tag,
    -- which is the whole point of the seed — and no scope those untagged rows
    -- reach declares either, so both take the unclassified fallback.  What is
    -- pinned here is the first half: the word is a STATE rather than the first
    -- word of a title.
  , testCase "recognition is the union of every layer, and classification is not" $
      withRows (Just "#+TODO: STARTED |\n")
               [("book.org", bookConfig), ("film.org", commentedConfig)]
               [ ("a.org", "* STARTED one\n")
               , ("b.org", "* ABANDONED two\n")
               , ("c.org", "* WATCHED three\n") ] $ \rows -> do
      assertEqual "each layer's keyword reaches each file"
                  [(Just "STARTED", Just True), (Just "ABANDONED", Just True)
                  ,(Just "WATCHED", Just True)]
                  (states rows)
      assertEqual "and none of them landed in a title" ["one", "two", "three"] (titles rows)

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
                   [TodoKeywords ["TODO", "READING", "LATER"] ["DONE", "READ", "ABANDONED"]]
                   (map hrKeywords rows)

    -- And a redeclaration is still RECOGNIZED once the union is ordered: the
    -- file puts READING after the bar where book.org puts it before, and the
    -- word stays in the union — in the ACTIVE half, where its first declaration
    -- put it, since 'mergeKeywords' resolves a disagreement that way.  So the
    -- headline is a state rather than the first word of a title.  Which bucket
    -- the ROW lands in is 'classify''s separate question, answered here by the
    -- file, this row carrying no tag that would reach book.org's opinion.
  , testCase "a shadowed redeclaration is still in the union, in its first place" $
      withRows Nothing [("book.org", bookConfig)]
               [("a.org", "#+TODO: LATER | READING\n* READING one\n")] $ \rows -> do
      assertEqual "hrKeywords"
                  [TodoKeywords ["TODO", "READING", "LATER"] ["DONE", "READ", "ABANDONED"]]
                  (map hrKeywords rows)
      assertEqual "and the row is a state, on the file's own reading of it"
                  [(Just "READING", Just False)] (states rows)
  ]

-- Classification

-- | Whether a recognized keyword is active is answered by the WIDEST scope:
-- org's TODO\/DONE, then the system layer, then the headline's tags in order,
-- then the file's own @#+TODO:@.  The union answers nowhere.
classificationSpec :: TestTree
classificationSpec = testGroup "Classification"
  [ testCase "the tag config outranks the file's own pragma" $
      withRows Nothing [("book.org", bookConfig)]
               -- book.org calls READING active; this file calls it done-like,
               -- and the headline wears the tag.  The tag is the wider scope, so
               -- the file's private opinion about a shared word is not applied.
               [("a.org", "#+TODO: | READING\n* READING done with it :book:\n")] $ \rows ->
      assertEqual "the tag wins" [(Just "READING", Just True)] (states rows)

  , testCase "the system layer outranks the tag config" $
      withRows (Just "#+TODO: | READING\n") [("book.org", bookConfig)]
               [ ("a.org", "* READING tagged :book:\n")
               , ("b.org", "* READING untagged\n") ] $ \rows ->
      -- The tree said READING is done-like, so it is done-like in both rows:
      -- carrying the `book' tag no longer buys a row a different answer about a
      -- word the layer above it already settled.
      assertEqual "the system layer, tagged or not"
                  [(Just "READING", Just False), (Just "READING", Just False)] (states rows)

  , testCase "org's own TODO and DONE outrank every layer under them" $
      -- The system layer puts TODO after the bar; org's pair is the widest
      -- scope and answers first, so TODO is work here as it is everywhere.
      withRows (Just "#+TODO: | TODO\n") [] [("a.org", "* TODO still work here\n")] $ \rows ->
      assertEqual "the default pair wins" [(Just "TODO", Just True)] (states rows)

  , testCase "and with no layer at all the default pair is what answers" $
      withRows Nothing [] [("a.org", "* TODO one\n* DONE two\n")] $ \rows ->
      assertEqual "TODO active, DONE not"
                  [(Just "TODO", Just True), (Just "DONE", Just False)] (states rows)

    -- The reorder's cost, pinned where a reader meets it: a file redeclaring a
    -- word a wider scope already settled keeps the word RECOGNIZED and loses
    -- the redefinition.  Under the old nearest-scope chain both of these
    -- classified the file's way.
  , testCase "a file redeclaring a wider scope's word does not move its rows" $
      withRows (Just "#+TODO: STARTED | SHELVED\n") []
               [ ("a.org", "#+TODO: | TODO\n* TODO org's answer stands\n")
               , ("b.org", "#+TODO: SHELVED |\n* SHELVED the tree's answer stands\n") ] $
        \rows ->
      assertEqual "the default pair and then the system layer"
                  [(Just "TODO", Just True), (Just "SHELVED", Just False)] (states rows)

  , testCase "and the word it redeclares still parses as a state" $
      withRows (Just "#+TODO: STARTED | SHELVED\n") []
               [("b.org", "#+TODO: SHELVED |\n* SHELVED the tree's answer stands\n")] $ \rows ->
      assertEqual "the keyword did not land in the title"
                  ["the tree's answer stands"] (titles rows)

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

  , testCase "a keyword no scope here claims is recognized and unclassified" $
      -- ABANDONED is book.org's and this headline is not a book.  It is still
      -- recognized — the seed is what keeps it out of the title — and the layer
      -- that named it is not a scope of this row, so nothing here calls it
      -- done-like and it takes the fallback.
      withRows Nothing [("book.org", bookConfig)] [("a.org", "* ABANDONED a plan\n")] $ \rows -> do
      assertEqual "a state, and active by default"
                  [(Just "ABANDONED", Just True)] (states rows)
      assertEqual "the word did not land in the title" ["a plan"] (titles rows)

  , testCase "and it is classified once the row carries the tag that names it" $
      withRows Nothing [("book.org", bookConfig)]
               [("a.org", "* ABANDONED a plan :book:\n")] $ \rows ->
      assertEqual "done-like, as book.org has it" [(Just "ABANDONED", Just False)] (states rows)

  , testCase "a row with no keyword is in neither group" $
      withRows (Just "#+TODO: TODO STARTED | DONE\n") [] [("a.org", "* just a heading\n")] $
        \rows -> assertEqual "neither" [(Nothing, Nothing)] (states rows)

  , testCase "the resolver is the rule, and it is total" $ do
      -- The chain, exercised where the fixtures cannot reach: every rung is the
      -- winner over a narrower one that disagrees, and a keyword in no layer
      -- whatsoever still has to come back with an answer.
      let cfg = ConfigLayers { clSystem  = TodoKeywords [] ["TODO", "STARTED"]
                             , clTags    = [("book", TodoKeywords ["READING"] [])]
                             , clSeed    = TodoKeywords ["READING", "STARTED"] ["TODO"]
                             , clViews   = []
                             , clCapture = Nothing
                             , clStateColors = []
                             , clPrint   = ""
                             , clDirs    = [] }
          file = TodoKeywords ["STARTED"] ["READING"]
      assertEqual "the default pair first" True (classify cfg file ["book"] "TODO")
      assertEqual "then the system layer" False (classify cfg file ["book"] "STARTED")
      assertEqual "then the tag" True (classify cfg file ["book"] "READING")
      assertEqual "and last the file" False (classify cfg file [] "READING")
      -- And no fifth scope: the seed names READING and the chain stops at the
      -- file, so a row reaching neither takes the fallback rather than the
      -- layer that happened to declare it.
      assertEqual "the seed is not a scope" True (classify cfg noKeywords [] "READING")
      assertEqual "and a word nothing names is active" True (classify cfg noKeywords [] "NOPE")
  ]

-- Palette

-- | The badge palette is the union with the config leading, which is what
-- makes its order — and so the state column's sort priority — independent of
-- which file the walk reached first.
--
-- ORDER IS THE ORG FILES'.  Every list here is 'keywordScopes' precedence by
-- segment — org's own pair, @system.org@, the tag configs by name, then
-- whatever a file adds — and each layer's own left-to-right spelling inside its
-- segment.  A repeat keeps its FIRST place, so a word two layers name sorts
-- where the wider one put it.
paletteSpec :: TestTree
paletteSpec = testGroup "Palette"
  [ testCase "the config's keywords lead and the files add to them" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n") [("book.org", bookConfig)]
               [("a.org", "#+TODO: LATER |\n* LATER one\n")] $ \dir -> do
      store <- loadStore dir
      -- system spells TODO STARTED | DONE, book.org TODO READING | READ
      -- ABANDONED, a.org LATER — and TODO is org's own, so it leads whoever
      -- names it.
      assertEqual "system, then tags, then whatever a file adds"
                  (TodoKeywords ["TODO", "STARTED", "READING", "LATER"]
                                ["DONE", "READ", "ABANDONED"])
                  (storeKeywords store)

  , testCase "and reordering one #+TODO: line reorders the palette" $ do
      -- One tree spelled twice, the two cycles differing only in their order.
      -- The palette follows the line, which is the whole claim: the org file is
      -- the state column's comparator config.
      let spelled cycle' k =
            withTree (Just cycle') [] [("a.org", "* STARTED one\n")] (k <=< loadStore)
      spelled "#+TODO: STARTED WAITING | CANCELLED DONE\n" $
        assertEqual "as the line spells it"
                    (TodoKeywords ["TODO", "STARTED", "WAITING"] ["DONE", "CANCELLED"])
          . storeKeywords
      spelled "#+TODO: WAITING STARTED | DONE CANCELLED\n" $
        assertEqual "and as it spells it the other way"
                    (TodoKeywords ["TODO", "WAITING", "STARTED"] ["DONE", "CANCELLED"])
          . storeKeywords

  , testCase "and a tree with no rows still has the states it configures" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n") [] [] $ \dir -> do
      store <- loadStore dir
      assertEqual "rows" [] (map hrId (storeRecords store))
      assertEqual "badges all the same"
                  (TodoKeywords ["TODO", "STARTED"] ["DONE"]) (storeKeywords store)

  , testCase "with no config the palette is the files', as it always was" $
      withTree Nothing [] [("a.org", "#+TODO: TODO WAITING | DONE\n* WAITING one\n")] $ \dir -> do
      store <- loadStore dir
      assertEqual "palette" (TodoKeywords ["TODO", "WAITING"] ["DONE"]) (storeKeywords store)
  ]

-- Reload

-- | A config edit is the one watch event that reaches past its own path: it
-- moves what every other file RECOGNIZES, so the answer is a reseed.
reloadSpec :: TestTree
reloadSpec = testGroup "Reload"
  [ testCase "an edited config reseeds the store and reparses the rows" $
      withTree (Just "#+TODO: TODO | DONE\n") [] [("a.org", "* STARTED refactor\n")] $ \dir -> do
      store <- loadStore dir
      assertEqual "before, the word is title text"
                  [(Nothing, Nothing)] (states (storeRecords store))
      hub <- newHub store
      let systemFile = systemFileIn dir
      TIO.writeFile systemFile "#+TODO: TODO STARTED | DONE\n"
      settle defaultWalk dir hub [systemFile]
      next <- readTVarIO (hubStore hub)
      assertEqual "after, it is a state"
                  [(Just "STARTED", Just True)] (states (storeRecords next))
      assertEqual "the config the store carries moved"
                  (TodoKeywords ["TODO", "STARTED"] ["DONE"]) (clSeed (stConfig next))
      assertBool "the generation moved with it" (stGen next > stGen store)

  , testCase "and the palette move closes the socket rather than streaming rows" $
      withTree (Just "#+TODO: TODO | DONE\n") [] [("a.org", "* STARTED refactor\n")] $ \dir -> do
      store <- loadStore dir
      let systemFile = systemFileIn dir
      TIO.writeFile systemFile "#+TODO: TODO STARTED | DONE\n"
      (next, frames) <- afterEdit store dir
      assertEqual "one close, no rows behind it" [Close ViewChanged] frames
      assertBool "the generation moved" (stGen next == stGen store + 1)

  , testCase "a config edit that moves no keyword moves no rows" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n") [] [("a.org", "* STARTED refactor\n")] $
        \dir -> do
      store <- loadStore dir
      let systemFile = systemFileIn dir
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
      let systemFile = systemFileIn dir
      -- The keywords are unchanged, so the palette holds and the ordinary ops
      -- are what a client gets — which is the branch ViewChanged replaces.
      TIO.writeFile systemFile "#+TITLE: States\n#+TODO: TODO STARTED | DONE\n"
      TIO.writeFile (dir </> "a.org") "* STARTED one renamed\n"
      (next, frames) <- afterEdit store dir
      assertEqual "one upsert" 1 (length [ () | Op (UpsertRow _) <- frames ])
      assertEqual "no deletes" 0 (length [ () | Op (DeleteRow _) <- frames ])
      assertBool "the generation moved" (stGen next == stGen store + 1)

  , testCase "a config deleted takes its keywords with it" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n") [] [("a.org", "* STARTED refactor\n")] $
        \dir -> do
      store <- loadStore dir
      hub <- newHub store
      let systemFile = systemFileIn dir
      removeFile systemFile
      settle defaultWalk dir hub [systemFile]
      next <- readTVarIO (hubStore hub)
      assertEqual "back to title text" [(Nothing, Nothing)] (states (storeRecords next))
      assertEqual "and to org's own two"
                  (TodoKeywords ["TODO"] ["DONE"]) (storeKeywords next)

    -- A CONFIG IN THE WINDOW IS A RESEED, and the unannounced file is what
    -- observes it: `c.org' is written and never named to `settle', so it can
    -- only be in the store if the step RE-WALKED the tree.  Asserting the two
    -- announced files alone would pass under two ordinary per-file re-reads,
    -- which is the mechanism this case exists to tell apart.
  , testCase "a config among the paths reseeds the whole tree, once" $
      withTree (Just "#+TODO: TODO | DONE\n") [] [("a.org", "* STARTED one\n")] $ \dir -> do
      store <- loadStore dir
      hub <- newHub store
      let systemFile = systemFileIn dir
      TIO.writeFile systemFile "#+TODO: TODO STARTED | DONE\n"
      _ <- orgFile dir "b.org" "* STARTED two\n"
      _ <- orgFile dir "c.org" "* STARTED unannounced\n"
      -- Both announced paths ripen together; the reseed covers the pair.
      settle defaultWalk dir hub [dir </> "b.org", systemFile]
      next <- readTVarIO (hubStore hub)
      assertEqual "every file, every keyword recognized"
                  (replicate 3 (Just "STARTED", Just True)) (states (storeRecords next))
      assertEqual "the unannounced file arrived on the re-walk"
                  ["one", "two", "unannounced"] (sort (titles (storeRecords next)))

    -- The converse, observed the same way: with no config among the paths the
    -- step touches the ONE file it was told about, so a file that appeared on
    -- disk without an event stays out until something announces it.
  , testCase "an ordinary edit with no config among them is still one file" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n") [] [("a.org", "* STARTED one\n")] $ \dir -> do
      store <- loadStore dir
      hub <- newHub store
      TIO.writeFile (dir </> "a.org") "* STARTED one renamed\n"
      _ <- orgFile dir "c.org" "* STARTED unannounced\n"
      settle defaultWalk dir hub [dir </> "a.org"]
      next <- readTVarIO (hubStore hub)
      assertEqual "re-read under the store's own config"
                  [(Just "STARTED", Just True)] (states (storeRecords next))
      -- One row, so `c.org' never arrived: a step with no config among its
      -- paths touches the files it was told about and re-walks nothing.
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

    -- ABSENT lines leave the block standing — the optional regions' own rule,
    -- and what a pin rides: the filter line alone, no cycle restated.  The
    -- pin shipped against a server that still REQUIRED the field, every shell
    -- test driving a harness stub — this is the server-side case that was
    -- missing.
  , testCase "absent lines write the filter alone, the cycle untouched" $ do
      assertEqual "the filter line joins, every other byte where it was"
        (Right ("#+TITLE: Book\n#+TODO:  TODO READING | READ ABANDONED\n"
                <> "#+GLANCE_DEFAULT_FILTER: state:*active* sort:state->title\n"
                <> "\n* Book\n*** Notes\n    %?\n"))
        (do edits <- configEdits bookConfig Nothing
                       noParts { cpViews = [("default", "state:*active* sort:state->title")] }
            first (T.pack . show)
                  (applyEdits bookConfig [ Edit sp new | (sp, new) <- edits ]))
      assertEqual "and absent everything is no edit at all"
        (Right []) (configEdits bookConfig Nothing noParts)

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

    -- A config file's own line ending, for the block AND for the opening a
    -- header with no newline owes: an LF block spliced into a CRLF file left
    -- one file speaking two conventions, with the line the reader just typed
    -- the odd one out.
  , testCase "a CRLF layer keeps its own line endings" $ do
      assertEqual "the block replaced in place"
                  (Right "#+TITLE: Book\r\n#+TODO: A | B\r\n\r\n* Book\r\n")
                  (spliced "#+TITLE: Book\r\n#+TODO: TODO | DONE\r\n\r\n* Book\r\n"
                           ["#+TODO: A | B"])
      assertEqual "and inserted where there was none"
                  (Right "#+TITLE: Book\r\n#+TODO: A | B\r\n\r\n* Book\r\n")
                  (spliced "#+TITLE: Book\r\n\r\n* Book\r\n" ["#+TODO: A | B"])
      assertEqual "the opening a live last line owes is the file's too"
                  (Right "#+TITLE: X\r\n#+TITLE: Y\r\n#+TODO: A | B\r\n")
                  (spliced "#+TITLE: X\r\n#+TITLE: Y" ["#+TODO: A | B"])

  , testCase "what a layer may say, and what it may not" $ do
      mapM_ (\(what, lines') ->
               assertBool what (either (const True) (const False)
                                       (configEdits bookConfig (Just lines') noParts)))
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
                         (configEdits bookConfig (Just ["#+TODO: TODO(t) | DONE(d)"]) noParts))

    -- The two tree-wide lines of `system.org'.  One reader finds either
    -- ('lastPragmaValue') and one splice writes either ('pragmaLineEdits'), so
    -- the two cases over 'treePragmas' are one claim about the pair rather than
    -- two blocks that have to be kept in step.  Absent means the built-in,
    -- which is what keeps a tree that has never been configured opening on its
    -- unfinished work and capturing into its own inbox.
  , testCase "the tree-wide lines are read off the system layer" $
      mapM_ (\(key, value, other, rd, _wr) -> do
               let says what = T.unpack key <> ": " <> what
               assertEqual (says "read off the file")
                           (Just value) (rd (pragmaLine key value))
               assertEqual (says "folded, the way org reads a pragma key")
                           (Just value) (rd (pragmaLine (T.toLower key) value))
               assertEqual (says "a file with no line names none") Nothing (rd bookConfig)
               -- A LAST-line rule: a reader scrolling the file reads the one at
               -- the bottom, and so does this.
               assertEqual (says "the last one wins") (Just other)
                           (rd (pragmaLine key value <> pragmaLine key other)))
            treePragmas

    -- A line naming nothing is a query naming nothing, which is the whole
    -- store; only an ABSENT line falls back.
  , testCase "and a default view line with nothing on it is the empty query" $
      assertEqual "the empty query"
                  (Just "") (viewOf defaultSaved "#+GLANCE_DEFAULT_FILTER:\n")

  , testCase "with no line anywhere the built-in is what answers" $
      withTree Nothing [] [("a.org", "* TODO x\n")] $ \dir -> do
        (cfg, _rows) <- loaded dir
        assertEqual "nothing configured" [] (clViews cfg)
        assertEqual "so the tree opens on the active group" builtinFilter (defaultFilter cfg)

  , testCase "and the system layer's line is what the tree opens on" $
      withTree (Just "#+TODO: TODO | DONE\n#+GLANCE_DEFAULT_FILTER: tag:work\n")
               [] [("a.org", "* TODO x\n")] $ \dir -> do
        (cfg, _rows) <- loaded dir
        assertEqual "read at load" (Just "tag:work") (lookup "default" (clViews cfg))
        assertEqual "and it is what answers" "tag:work" (defaultFilter cfg)

    -- Where a capture lands is decided HERE, when the config is read, and not
    -- when a `+' arrives: a tree misconfigured in January says so at startup.
  , testCase "the capture target resolves against the served root, or is refused" $ do
      assertEqual "with no line, the tree's own inbox"
                  (Right "/o/inbox.org") (captureTargetIn "/o" noConfig)
      assertEqual "named, resolved against the root"
                  (Right "/o/notes/in.org") (captureTargetIn "/o" (naming "notes/in.org"))
      assertEqual "and an empty line is the default again"
                  (Right "/o/inbox.org") (captureTargetIn "/o" (naming "  "))
      -- Three refusals, all textual the way every other path rule here is.
      mapM_ (\(what, target, needle) ->
               refusedNaming what [needle] (captureTargetIn "/o" (naming target)))
            [ ("an absolute path", "/etc/passwd.org", "absolute")
            , ("a path climbing out", "../elsewhere.org", "outside")
            , ("one deeper down", "notes/../../out.org", "outside")
            -- A file the walk would not collect is a capture that vanishes: the
            -- entry is written and no watch ever delivers a row for it.  All
            -- THREE of the walk's predicates, since an org file under
            -- `.org-glance' is exactly the case an extension test would bless.
            , ("a name the walk skips", "inbox.txt", "walks")
            , ("one of Emacs's sidecars", ".#inbox.org", "walks")
            , ("the config the walk reads by path", ".org-glance/config/system.org", "walks")
            , ("and a derived mirror", ".org-glance/overviews/inbox.org", "walks") ]

  , testCase "and a tree that names one loads it" $
      withTree (Just "#+TODO: TODO | DONE\n#+GLANCE_CAPTURE_TARGET: notes/in.org\n")
               [] [("a.org", "* TODO x\n")] $ \dir -> do
        (cfg, _rows) <- loaded dir
        assertEqual "read at load" (Just "notes/in.org") (clCapture cfg)
        assertEqual "and it is what a capture would write to"
                    (Right (dir </> "notes/in.org")) (captureTargetIn dir cfg)

    -- One file, one write, one lock: the cycle and both tree-wide lines are
    -- lines of the same document, so they ride in one splice under one digest.
  , testCase "the tree-wide lines are written by the same splice as the cycle" $
      mapM_ (\(key, value, other, _rd, wr) -> do
               let says what = T.unpack key <> ": " <> what
                   block = ["#+TODO: A | B"]
                   held = pragmaLine key value <> "#+TODO: A | B\ntail\n"
               assertEqual (says "written under the header, beside the block")
                           (Right ("#+TITLE: X\n#+TODO: A | B\n" <> pragmaLine key value))
                           (wr "#+TITLE: X\n" block (Just value))
               assertEqual (says "an existing line is replaced where it stands")
                           (Right (pragmaLine key other <> "#+TODO: A | B\ntail\n"))
                           (wr held block (Just other))
               -- An empty value takes the line away, which is that setting's
               -- built-in back: the active group, and the tree's own inbox.
               assertEqual (says "an empty one takes the line away")
                           (Right "#+TODO: A | B\ntail\n") (wr held block (Just ""))
               -- Absent and empty differ: a tag layer's write names neither
               -- line, and the system layer's are none of its business.
               assertEqual (says "and naming none leaves the line exactly as it is")
                           (Right held) (wr held block Nothing))
            treePragmas

    -- Pragmas a file lacks are inserted at ONE offset, which the engine
    -- resolves in list order rather than refusing.
  , testCase "a file with none of them takes them in the order they are named" $ do
      assertEqual "the cycle, then the default view"
                  (Right "#+TODO: A | B\n#+GLANCE_DEFAULT_FILTER: tag:work\n* %?\n")
                  (splicedWith "* %?\n" ["#+TODO: A | B"] (Just "tag:work"))
      assertEqual "and all three where all three are named"
                  (Right "#+TODO: A | B\n#+GLANCE_DEFAULT_FILTER: tag:work\n\
                         \#+GLANCE_CAPTURE_TARGET: in.org\n* %?\n")
                  (splicing "* %?\n" ["#+TODO: A | B"] (Just "tag:work") (Just "in.org"))

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
      let systemFile = systemFileIn dir
      written <- either (assertFailure . T.unpack) pure
                        (spliced "" ["#+TODO: TODO STARTED | DONE"])
      TIO.writeFile systemFile written
      settle defaultWalk dir hub [systemFile]
      next <- readTVarIO (hubStore hub)
      assertEqual "after, it is a state"
                  [(Just "STARTED", Just True)] (states (storeRecords next))
      assertEqual "and the palette carries it"
                  (TodoKeywords ["TODO", "STARTED"] ["DONE"]) (storeKeywords next)

    -- THE ORG FILE IS THE COMPARATOR CONFIG, end to end.  One tree, two writes
    -- of one cycle differing only in the order it spells its two states: the
    -- route's own splice lands it, the watch reseeds, the palette follows the
    -- line and the table follows the palette.  The titles run the OTHER way in
    -- both halves, so a sort settling on them would answer the same twice.
  , testCase "a reordered cycle reorders the table" $
      withTree Nothing [] [("a.org", "* STARTED beta\n* WAITING alpha\n")] $ \dir -> do
      hub <- newHub =<< loadStore dir
      let systemFile = systemFileIn dir
          reseedWith line = do
            written <- either (assertFailure . T.unpack) pure (spliced "" [line])
            TIO.writeFile systemFile written
            settle defaultWalk dir hub [systemFile]
            st <- readTVarIO (hubStore hub)
            let palette = storeKeywords st
            pure ( palette
                 , map hrTitle
                       (sortedForViewWith palette defaultSortChain (storeRecords st)) )
      assertEqual "the cycle as written, and the rows behind it"
                  ( TodoKeywords ["TODO", "STARTED", "WAITING"] ["DONE"]
                  , ["beta", "alpha"] )
        =<< reseedWith "#+TODO: STARTED WAITING | DONE"
      assertEqual "and spelled the other way round, both swap"
                  ( TodoKeywords ["TODO", "WAITING", "STARTED"] ["DONE"]
                  , ["alpha", "beta"] )
        =<< reseedWith "#+TODO: WAITING STARTED | DONE"
  ]

-- | DOC with LINES as its @#+TODO:@ block, spliced the way the route splices
-- it: 'configEdits' for the spans and the write engine's own 'applyEdits' for
-- the result, so what is asserted is the document a write would leave behind.
spliced :: Text -> [Text] -> Either Text Text
spliced doc lines' = splicedWith doc lines' Nothing

-- | 'spliced', also setting the default view to WANT.
splicedWith :: Text -> [Text] -> Maybe Text -> Either Text Text
splicedWith doc lines' want = splicing doc lines' want Nothing

-- | A config naming TARGET as its capture target and nothing else.
naming :: Text -> ConfigLayers
naming target = noConfig { clCapture = Just target }

-- | 'spliced', also setting the capture target to TARGET.
splicedCapture :: Text -> [Text] -> Maybe Text -> Either Text Text
splicedCapture doc lines' = splicing doc lines' Nothing

-- | 'spliced' over both of the system layer's tree-wide lines.
splicing :: Text -> [Text] -> Maybe Text -> Maybe Text -> Either Text Text
splicing doc lines' want target = do
  edits <- configEdits doc (Just lines')
             noParts { cpViews = maybe [] (\q -> [("default", q)]) want, cpCapture = target }
  first (T.pack . show) (applyEdits doc [ Edit sp new | (sp, new) <- edits ])

-- | The system layer's two tree-wide lines: each spelled key, two values of the
-- shape that line takes, the reader that finds it and the splice that writes
-- it.  The keys are spelled here rather than read off the library, so a renamed
-- pragma is a failure rather than a rename the suite follows.
treePragmas :: [( Text, Text, Text, Text -> Maybe Text
                , Text -> [Text] -> Maybe Text -> Either Text Text )]
treePragmas =
  [ ("#+GLANCE_DEFAULT_FILTER", "tag:work", "tag:home", viewOf defaultSaved, splicedWith)
  , ("#+GLANCE_CAPTURE_TARGET", "a.org", "b.org", captureTargetOf, splicedCapture) ]

-- | The default view's registry entry, which every build carries.
defaultSaved :: SavedView
defaultSaved = case savedView "default" of
  Just v  -> v
  Nothing -> error "TestConfig: no default view in savedViews"

-- | The line KEY spells VALUE on, newline and all.
pragmaLine :: Text -> Text -> Text
pragmaLine key value = key <> ": " <> value <> "\n"

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

  , testCase "and its rows classify by the default pair and the file alone" $
      withTempDirNamed "config" $ \dir -> do
      _ <- orgFile dir "a.org" "#+TODO: NEXT | GONE\n* NEXT one\n* GONE two\n* TODO three\n"
      (_cfg, rows) <- loaded dir
      assertEqual "the file's two, and org's own over them"
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
      -- c.org's ABANDONED is book.org's word on an untagged row: recognized
      -- through the seed and claimed by no scope this headline reaches, so it
      -- takes the unclassified fallback rather than book.org's opinion.
      let rows = concat [ rs | (_p, Right rs) <- parallel ]
      assertEqual "states"
                  [ (Just "STARTED", Just True), (Just "READING", Just True)
                  , (Just "ABANDONED", Just True), (Just "LATER", Just True) ]
                  (states rows)
  ]
