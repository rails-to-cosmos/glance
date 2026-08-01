-- | The keyword configuration layer: what @.org-glance\/config@ makes the
-- parser recognize, and how a row's active-ness is classified once it has.
--
-- The two questions are tested apart on purpose, because they answer
-- differently.  Recognition is a UNION over every layer and reaches every file
-- under the root; classification is NEAREST SCOPE and reaches one headline.  A
-- change that collapses them passes half of this module and fails the other.
module TestConfig (spec) where

import Control.Concurrent.STM (readTVarIO)
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
import Glance.Query ( ConfigLayers (..), HeadlineRecord (..), QueryResult (..)
                    , WalkOptions (..), configPath, defaultWalk, loadDir
                    , loadDirFilesSerially, loadDirWith, loadDirWithConfig
                    , loadFile )
import Glance.Web.Store ( Frame (..), Hub (hubStore), Store (stConfig, stGen, stPrint)
                        , loadStore, newHub, reseeded, storeKeywords, storeRecords )
import Glance.Web.Watch (settle, watched)

spec :: TestTree
spec = testGroup "Config"
  [ discoverySpec, recognitionSpec, classificationSpec, paletteSpec
  , reloadSpec, absenceSpec, paritySpec ]

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
withRows system tags docs k = withTree system tags docs (\dir -> loaded dir >>= k . snd)

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
                             , clPrint  = "" }
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
