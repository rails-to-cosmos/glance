-- | The keyword configuration layer: what @.org-glance\/config@ makes the
-- parser recognize, and how a row's active-ness is classified once it has.
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
                    , ConfigSetting (..), SettingScope (..), configSettings
                    , TreeSettings (..), noTreeSettings, treeSettings
                    , Span
                    , SavedView (..), savedView, savedViews, viewOf
                    , HeadlineRecord (..)
                    , QueryResult (..), WalkOptions (..), builtinFilter
                    , captureTargetIn, configEdits
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

-- | The tag config the live tree carries verbatim: title, cycle, template.
bookConfig :: Text
bookConfig = T.unlines
  [ "#+TITLE: Book"
  , "#+TODO:  TODO READING | READ ABANDONED"
  , ""
  , "* Book"
  , "*** Notes"
  , "    %?" ]

-- | A config that does not parse: a hyphen inside a COMMENTED @#+TODO:@.
commentedConfig :: Text
commentedConfig = T.unlines
  [ "# Config for the `film' tag (the file name is the tag)."
  , "#   #+TODO:     a per-tag todo cycle, e.g. `TODO DOING | DONE'"
  , "#+TODO: TODO WATCHING | WATCHED"
  , ""
  , "* %?" ]

-- | SYSTEM is @config\/system.org@, TAGS @config\/tags\/NAME@, DOCS root documents.
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

withTree :: Maybe Text -> [(FilePath, Text)] -> [(FilePath, Text)]
         -> (FilePath -> IO a) -> IO a
withTree = withTreeUnder "."

loaded :: FilePath -> IO (ConfigLayers, [HeadlineRecord])
loaded dir = do
  (cfg, files, _dirErrs) <- loadDirWithConfig defaultWalk dir
  pure (cfg, concat [ rs | (_path, Right rs) <- files ])

states :: [HeadlineRecord] -> [(Maybe Text, Maybe Bool)]
states = map (\r -> (hrState r, hrActive r))

-- | Row titles, which is where a keyword lands when it is NOT recognized.
titles :: [HeadlineRecord] -> [Text]
titles = map hrTitle

withRows :: Maybe Text -> [(FilePath, Text)] -> [(FilePath, Text)]
         -> ([HeadlineRecord] -> Assertion) -> Assertion
withRows system tags docs k = withTree system tags docs (k . snd <=< loaded)

-- Discovery

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
      assertBool "a config file reached the table"
                 (all ((== (dir </> "notes.org")) . hrFile) (qrRecords qr))

  , testCase "and --include-derived does not reach them either" $
      withTree Nothing [("book.org", bookConfig)] [("notes.org", "* TODO a note\n")] $ \dir -> do
      qr <- loadDirWith (WalkOptions True) dir
      assertEqual "files" 1 (qrFiles qr)
      assertEqual "rows" 1 (length (qrRecords qr))

  , testCase "the store hangs off wherever it sits, not off the root" $
      -- ~/sync's own shape: the store is one directory down, and discovery follows the walk.
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
      -- Watched on purpose: a change to it changes every file that WAS handed over.
      assertBool "the watch takes system.org" (watched defaultWalk system)
      assertBool "and the tag config" (watched defaultWalk tag)
      assertBool "an autosave beside a tag config is not watched"
                 (not (watched defaultWalk "/o/.org-glance/config/tags/#book.org#"))
  ]

-- Recognition

-- | A SUPERSET: one headline reading as a state here and a title there is the bug.
recognitionSpec :: TestTree
recognitionSpec = testGroup "Recognition"
  [ testCase "a keyword only a tag config names parses as a state elsewhere" $
      withRows Nothing [("book.org", bookConfig)]
               -- Nothing in this file says READING is a keyword except the config.
               [("a.org", "* READING War and Peace\n")] $ \rows -> do
      assertEqual "state" [(Just "READING", Just True)] (states rows)
      assertEqual "and the title kept the rest" ["War and Peace"] (titles rows)

  , testCase "and without the config the same word is the title's first" $
      withRows Nothing [] [("a.org", "* READING War and Peace\n")] $ \rows -> do
      assertEqual "state" [(Nothing, Nothing)] (states rows)
      assertEqual "title" ["READING War and Peace"] (titles rows)

  , testCase "the STARTED class: a system keyword used bare in a data file" $
      -- The corpus case: with no layer above the file the word is title text everywhere.
      withRows (Just "#+TODO: TODO STARTED | DONE CANCELLED\n") []
               [("a.org", "* STARTED refactor the walk\n")] $ \rows -> do
      assertEqual "recognized and classified" [(Just "STARTED", Just True)] (states rows)
      assertEqual "title" ["refactor the walk"] (titles rows)

    -- Recognition reaches every file; classification does not follow it there.
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
      assertEqual "before the pragma, after it"
                  [(Nothing, Nothing), (Just "LATER", Just True)] (states rows)
      assertEqual "titles" ["LATER one", "two"] (titles rows)

  , testCase "the recognized set a row carries is the seed plus its own" $
      withRows Nothing [("book.org", bookConfig)] [("a.org", "#+TODO: LATER |\n* LATER one\n")] $
        \rows -> assertEqual "hrKeywords"
                   [TodoKeywords ["TODO", "READING", "LATER"] ["DONE", "READ", "ABANDONED"]]
                   (map hrKeywords rows)

    -- A redeclaration stays RECOGNIZED, in the half its FIRST declaration put it.
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

-- | WIDEST scope answers: org's pair, system, the row's tags, then the file.
classificationSpec :: TestTree
classificationSpec = testGroup "Classification"
  [ testCase "the tag config outranks the file's own pragma" $
      withRows Nothing [("book.org", bookConfig)]
               -- The tag is the wider scope, so the file's own opinion does not apply.
               [("a.org", "#+TODO: | READING\n* READING done with it :book:\n")] $ \rows ->
      assertEqual "the tag wins" [(Just "READING", Just True)] (states rows)

  , testCase "the system layer outranks the tag config" $
      withRows (Just "#+TODO: | READING\n") [("book.org", bookConfig)]
               [ ("a.org", "* READING tagged :book:\n")
               , ("b.org", "* READING untagged\n") ] $ \rows ->
      -- The tree settled READING, so carrying `book' buys the row no other answer.
      assertEqual "the system layer, tagged or not"
                  [(Just "READING", Just False), (Just "READING", Just False)] (states rows)

  , testCase "org's own TODO and DONE outrank every layer under them" $
      withRows (Just "#+TODO: | TODO\n") [] [("a.org", "* TODO still work here\n")] $ \rows ->
      assertEqual "the default pair wins" [(Just "TODO", Just True)] (states rows)

  , testCase "and with no layer at all the default pair is what answers" $
      withRows Nothing [] [("a.org", "* TODO one\n* DONE two\n")] $ \rows ->
      assertEqual "TODO active, DONE not"
                  [(Just "TODO", Just True), (Just "DONE", Just False)] (states rows)

    -- A file redeclaring a word a wider scope settled loses the redefinition.
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
                 -- A first tag saying nothing about the keyword does not claim it.
               , ("c.org", "* READING three :nosuch:pile:\n") ] $ \rows ->
        assertEqual "tag order decides"
                    [ (Just "READING", Just True), (Just "READING", Just False)
                    , (Just "READING", Just False) ]
                    (states rows)

  , testCase "a keyword no scope here claims is recognized and unclassified" $
      -- Recognized through the seed, claimed by no scope this row reaches: the fallback.
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
      -- Every rung wins over a narrower disagreement; an unnamed keyword still answers.
      let cfg = ConfigLayers { clSystem  = TodoKeywords [] ["TODO", "STARTED"]
                             , clTags    = [("book", TodoKeywords ["READING"] [])]
                             , clSeed    = TodoKeywords ["READING", "STARTED"] ["TODO"]
                             , clTree    = noTreeSettings
                             , clPrint   = ""
                             , clDirs    = [] }
          file = TodoKeywords ["STARTED"] ["READING"]
      assertEqual "the default pair first" True (classify cfg file ["book"] "TODO")
      assertEqual "then the system layer" False (classify cfg file ["book"] "STARTED")
      assertEqual "then the tag" True (classify cfg file ["book"] "READING")
      assertEqual "and last the file" False (classify cfg file [] "READING")
      -- And no fifth scope: the chain stops at the file.
      assertEqual "the seed is not a scope" True (classify cfg noKeywords [] "READING")
      assertEqual "and a word nothing names is active" True (classify cfg noKeywords [] "NOPE")
  ]

-- Palette

-- | ORDER IS THE ORG FILES': 'keywordScopes' precedence by segment, each layer's
-- own left-to-right spelling inside it, a repeat keeping its FIRST place.
paletteSpec :: TestTree
paletteSpec = testGroup "Palette"
  [ testCase "the config's keywords lead and the files add to them" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n") [("book.org", bookConfig)]
               [("a.org", "#+TODO: LATER |\n* LATER one\n")] $ \dir -> do
      store <- loadStore dir
      assertEqual "system, then tags, then whatever a file adds"
                  (TodoKeywords ["TODO", "STARTED", "READING", "LATER"]
                                ["DONE", "READ", "ABANDONED"])
                  (storeKeywords store)

  , testCase "and reordering one #+TODO: line reorders the palette" $ do
      -- One tree spelled twice: the org file is the state column's comparator config.
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

-- | A config edit moves what every other file RECOGNIZES, so the answer is a reseed.
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
      TIO.writeFile systemFile "#+TITLE: States\n#+TODO: TODO STARTED | DONE\n"
      (next, frames) <- afterEdit store dir
      assertEqual "nothing to say" [] frames
      assertBool "and the generation stayed put" (stGen next == stGen store)
      -- The fingerprint covers the config: the bytes deciding what these rows MEAN moved.
      assertBool "the fingerprint moved on its own" (stPrint next /= stPrint store)

  , testCase "a data edit under a reseed streams rows rather than a close" $
      withTree (Just "#+TODO: TODO STARTED | DONE\n") [] [("a.org", "* STARTED one\n")] $ \dir -> do
      store <- loadStore dir
      let systemFile = systemFileIn dir
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

    -- A RESEED, observed through `c.org': written and never announced, so it can
    -- only be in the store if the step RE-WALKED the tree.
  , testCase "a config among the paths reseeds the whole tree, once" $
      withTree (Just "#+TODO: TODO | DONE\n") [] [("a.org", "* STARTED one\n")] $ \dir -> do
      store <- loadStore dir
      hub <- newHub store
      let systemFile = systemFileIn dir
      TIO.writeFile systemFile "#+TODO: TODO STARTED | DONE\n"
      _ <- orgFile dir "b.org" "* STARTED two\n"
      _ <- orgFile dir "c.org" "* STARTED unannounced\n"
      settle defaultWalk dir hub [dir </> "b.org", systemFile]
      next <- readTVarIO (hubStore hub)
      assertEqual "every file, every keyword recognized"
                  (replicate 3 (Just "STARTED", Just True)) (states (storeRecords next))
      assertEqual "the unannounced file arrived on the re-walk"
                  ["one", "two", "unannounced"] (sort (titles (storeRecords next)))

    -- The converse: with no config among the paths, an unannounced file stays out.
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
      assertEqual "title" ["one renamed"] (titles (storeRecords next))
  ]

-- | DIR loaded again and diffed against BEFORE: the pure half of a config event.
afterEdit :: Store -> FilePath -> IO (Store, [Frame])
afterEdit before dir = (`reseeded` before) <$> loadStore dir

-- Writing a layer

-- | The @#+TODO:@ lines are spliced as spans, so a template around them is untouched.
writeSpec :: TestTree
writeSpec = testGroup "Writing a layer"
  [ testCase "the lines a layer declares are the ones it is edited by" $ do
      assertEqual "the live tag config's, verbatim"
                  ["#+TODO:  TODO READING | READ ABANDONED"] (todoLines bookConfig)
      -- The commented one opens with `#' where a pragma opens with `#+', so it is no line.
      assertEqual "a commented pragma is not one"
                  ["#+TODO: TODO WATCHING | WATCHED"] (todoLines commentedConfig)
      assertEqual "a file with none" [] (todoLines "* TODO a note\n")

  , testCase "a block replaces the one that is there and nothing around it" $
      assertEqual "only the pragma line moved"
                  (Right "#+TITLE: Book\n#+TODO: TODO NEXT | DONE\n\n* Book\n*** Notes\n    %?\n")
                  (spliced bookConfig ["#+TODO: TODO NEXT | DONE"])

    -- ABSENT lines leave the block standing, which is what a pin rides on.
  , testCase "absent lines write the filter alone, the cycle untouched" $ do
      assertEqual "the filter line joins, every other byte where it was"
        (Right ("#+TITLE: Book\n#+TODO:  TODO READING | READ ABANDONED\n"
                <> "#+GLANCE_DEFAULT_FILTER: state:*active* sort:state->title\n"
                <> "\n* Book\n*** Notes\n    %?\n"))
        (do edits <- configEdits (systemLayer bookConfig) Nothing
                       noParts { cpViews = [("default", "state:*active* sort:state->title")] }
            first (T.pack . show)
                  (applyEdits bookConfig [ Edit sp new | (sp, new) <- edits ]))
      assertEqual "and absent everything is no edit at all"
        (Right []) (configEdits (systemLayer bookConfig) Nothing noParts)

  , testCase "a file spelling its cycle twice comes back spelling it once" $
      assertEqual "one line, where the first one was"
                  (Right "#+TITLE: X\n#+TODO: A | B\ntail\n")
                  (spliced "#+TITLE: X\n#+TODO: A |\n#+TODO: | B\ntail\n" ["#+TODO: A | B"])

  , testCase "a file with no block takes one under the header it opens with" $ do
      assertEqual "after the #+ run, ahead of the content"
                  (Right "#+TITLE: Film\n#+TODO: A | B\n\n* %?\n")
                  (spliced "#+TITLE: Film\n\n* %?\n" ["#+TODO: A | B"])
      assertEqual "at the top when it opens with content"
                  (Right "#+TODO: A | B\n* %?\n") (spliced "* %?\n" ["#+TODO: A | B"])
      assertEqual "and a file that is not there yet is all block"
                  (Right "#+TODO: A | B\n") (spliced "" ["#+TODO: A | B"])
      -- A header with no closing newline: appended bare, the block would land on a live line.
      assertEqual "a header with no newline gets one first"
                  (Right "#+TITLE: X\n#+TODO: A | B\n")
                  (spliced "#+TITLE: X" ["#+TODO: A | B"])

  , testCase "an empty block deletes the lines, and is the no-op without them" $ do
      assertEqual "the template survives the deletion"
                  (Right "#+TITLE: Book\n\n* Book\n*** Notes\n    %?\n")
                  (spliced bookConfig [])
      assertEqual "blank lines are not lines" (Right "#+TITLE: X\n")
                  (spliced "#+TITLE: X\n" ["", "  "])

    -- The file's own line ending; an LF block in a CRLF file mixes conventions.
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
                                       (configEdits (systemLayer bookConfig) (Just lines') noParts)))
            [ ("a headline is not a pragma", ["* TODO not a pragma"])
            , ("nor is a title", ["#+TITLE: no"])
            , ("a pragma declaring nothing", ["#+TODO:"])
            , ("one bad line spoils the block", ["#+TODO: A | B", "oops"])
              -- A PREFIX test, so a smuggled newline would write everything behind it unread.
            , ("a line carrying a newline of its own", ["#+TODO: A | B\n* not a pragma"])
              -- Keyword tokens are letters and underscores, so a group name cannot be one.
            , ("the filter's group names", ["#+TODO: *active* | *inactive*"])
            , ("and a starred word beside real ones", ["#+TODO: TODO *x* | DONE"]) ]
      assertBool "a cycle with fast-access keys is a block"
                 (either (const False) (const True)
                         (configEdits (systemLayer bookConfig) (Just ["#+TODO: TODO(t) | DONE(d)"]) noParts))

    -- One reader finds either line and one splice writes either; absent is the built-in.
  , testCase "the tree-wide lines are read off the system layer" $
      mapM_ (\(key, value, other, rd, _wr) -> do
               let says what = T.unpack key <> ": " <> what
               assertEqual (says "read off the file")
                           (Just value) (rd (pragmaLine key value))
               assertEqual (says "folded, the way org reads a pragma key")
                           (Just value) (rd (pragmaLine (T.toLower key) value))
               assertEqual (says "a file with no line names none") Nothing (rd bookConfig)
               assertEqual (says "the last one wins") (Just other)
                           (rd (pragmaLine key value <> pragmaLine key other)))
            treePragmas

    -- A line naming nothing is a query naming nothing; only an ABSENT line falls back.
  , testCase "and a default view line with nothing on it is the empty query" $
      assertEqual "the empty query"
                  (Just "") (viewOf defaultSaved "#+GLANCE_DEFAULT_FILTER:\n")

  , testCase "with no line anywhere the built-in is what answers" $
      withTree Nothing [] [("a.org", "* TODO x\n")] $ \dir -> do
        (cfg, _rows) <- loaded dir
        assertEqual "nothing configured" [] (tsViews (clTree cfg))
        assertEqual "so the tree opens on the active group" builtinFilter (defaultFilter cfg)

  , testCase "and the system layer's line is what the tree opens on" $
      withTree (Just "#+TODO: TODO | DONE\n#+GLANCE_DEFAULT_FILTER: tag:work\n")
               [] [("a.org", "* TODO x\n")] $ \dir -> do
        (cfg, _rows) <- loaded dir
        assertEqual "read at load" (Just "tag:work") (lookup "default" (tsViews (clTree cfg)))
        assertEqual "and it is what answers" "tag:work" (defaultFilter cfg)

    -- ONE ENTRY POINT PER TREE: no layer names it, so there is no refusal to make.
  , testCase "the capture target is the tree's own inbox, under the served root" $ do
      assertEqual "the served root's own" "/o/inbox.org" (captureTargetIn "/o")
      assertEqual "and it follows the root" "/other/inbox.org" (captureTargetIn "/other")

  , testCase "a tree still naming a capture target captures into the inbox anyway" $
      withTree (Just "#+TODO: TODO | DONE\n#+GLANCE_CAPTURE_TARGET: notes/in.org\n")
               [] [("a.org", "* TODO x\n")] $ \dir -> do
        _ <- loaded dir
        assertEqual "the pragma is inert" (dir </> "inbox.org") (captureTargetIn dir)

    -- One file, one write, one lock: cycle and both tree-wide lines ride one splice.
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
               assertEqual (says "an empty one takes the line away")
                           (Right "#+TODO: A | B\ntail\n") (wr held block (Just ""))
               -- Absent and empty differ: a tag layer's write names neither line.
               assertEqual (says "and naming none leaves the line exactly as it is")
                           (Right held) (wr held block Nothing))
            treePragmas

  , testCase "a file with none of them takes them in the order they are named" $ do
      assertEqual "the cycle, then the default view"
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

    -- The whole loop without the socket: write, config path, reseed, palette moves.
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

    -- THE ORG FILE IS THE COMPARATOR CONFIG; the titles run the OTHER way in both halves.
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

    -- THE SWEEP ASSERTS WHAT IT SWEPT: a row `everyPart' misses passes vacuously.
  , testCase "the write shape names every setting the registry carries" $
      mapM_ (\s -> assertBool (T.unpack (csName s) <> " is named by no test write")
                              (not (null (partEdits s bookConfig))))
            configSettings

    -- THE SCOPE HAZARD: a tree-wide setting off the mask let a TAG layer set it.
  , testCase "a tag layer's write reaches no tree-wide setting" $ do
      let after = written (tagLayer bookConfig)
      mapM_ (\s -> do
               let says = T.unpack (csName s)
                   lines' = addedLines bookConfig (partEdits s bookConfig)
               assertBool (says <> " writes nothing, so the case proves nothing")
                          (not (null lines'))
               mapM_ (\l -> assertBool (says <> " reached a tag layer: " <> show l)
                                       (l `notElem` T.lines after)) lines')
            [ s | s <- configSettings, csScope s == TreeWide ]

    -- The mask is no blanket: what a tag layer OWNS still lands.
  , testCase "and every per-layer setting still lands in one" $ do
      let after = written (tagLayer bookConfig)
      mapM_ (\s -> mapM_ (\l -> assertBool (T.unpack (csName s) <> " was masked: " <> show l)
                                           (l `elem` T.lines after))
                         (addedLines bookConfig (partEdits s bookConfig)))
            [ s | s <- configSettings, csScope s == PerLayer ]

  , testCase "and the system layer reaches all of them" $ do
      let after = written (systemLayer bookConfig)
      mapM_ (\s -> mapM_ (\l -> assertBool (T.unpack (csName s) <> " never landed: " <> show l)
                                           (l `elem` T.lines after))
                         (addedLines bookConfig (partEdits s bookConfig)))
            configSettings

    -- ONE FOLD, TWO CONSUMERS: the load caches it; `GET /config' recomputes it.
  , testCase "the load and the settings route read the tree through one fold" $
      withTree (Just "#+TODO: TODO | DONE\n\
                     \#+GLANCE_DEFAULT_FILTER: tag:work\n\
                     \#+GLANCE_CAPTURE_TARGET: notes/in.org\n\
                     \#+GLANCE_STATE_COLORS: light TODO=#7B1FA2\n")
               [] [("a.org", "* TODO x\n")] $ \dir -> do
        (cfg, _rows) <- loaded dir
        files <- readConfigLayers [configDirIn dir]
        assertEqual "what the store holds is what a fresh read answers"
                    (treeSettings files) (clTree cfg)
        assertEqual "and every member of it came back"
                    ( [("default", "tag:work")]
                    , [("light", [("TODO", "#7B1FA2")])] )
                    ( tsViews (clTree cfg), tsColors (clTree cfg) )
  ]

-- | A write naming EVERY setting 'configSettings' carries.
everyPart :: ConfigParts
everyPart = ConfigParts { cpViews    = [("default", "tag:work")]
                        , cpColors   = Just [("light", [("TODO", "#7B1FA2")])]
                        , cpTemplate = Just "* %?" }

-- | DOC as the tree's @system.org@ and as one tag's config: the scope 'configEdits' reads.
systemLayer, tagLayer :: Text -> ConfigLayerFile
systemLayer doc = ConfigLayerFile "system.org" Nothing "d" doc
tagLayer doc = ConfigLayerFile "tags/book.org" (Just "book") "d" doc

partEdits :: ConfigSetting -> Text -> [(Span, Text)]
partEdits s doc = either (const []) id (csEdits s doc everyPart)

written :: ConfigLayerFile -> Text
written layer = either (\why -> error ("TestConfig: " <> T.unpack why)) id $ do
  edits <- configEdits layer Nothing everyPart
  first (T.pack . show) (applyEdits doc [ Edit sp new | (sp, new) <- edits ])
  where doc = lfText layer

addedLines :: Text -> [(Span, Text)] -> [Text]
addedLines doc edits = case applyEdits doc [ Edit sp new | (sp, new) <- edits ] of
  Left why    -> error ("TestConfig: " <> show why)
  Right after -> [ l | l <- T.lines after, l `notElem` T.lines doc ]

-- | DOC with LINES as its @#+TODO:@ block, spliced the way the route splices it.
spliced :: Text -> [Text] -> Either Text Text
spliced doc lines' = splicedWith doc lines' Nothing

splicedWith :: Text -> [Text] -> Maybe Text -> Either Text Text
splicedWith doc lines' want = splicing doc lines' want

splicing :: Text -> [Text] -> Maybe Text -> Either Text Text
splicing doc lines' want = do
  edits <- configEdits (systemLayer doc) (Just lines')
             noParts { cpViews = maybe [] (\q -> [("default", q)]) want }
  first (T.pack . show) (applyEdits doc [ Edit sp new | (sp, new) <- edits ])

-- | The keys are spelled here, never read off the library: a rename must fail.
treePragmas :: [( Text, Text, Text, Text -> Maybe Text
                , Text -> [Text] -> Maybe Text -> Either Text Text )]
treePragmas =
  [ ("#+GLANCE_DEFAULT_FILTER", "tag:work", "tag:home", viewOf defaultSaved, splicedWith) ]

defaultSaved :: SavedView
defaultSaved = case savedView "default" of
  Just v  -> v
  Nothing -> error "TestConfig: no default view in savedViews"

pragmaLine :: Text -> Text -> Text
pragmaLine key value = key <> ": " <> value <> "\n"

-- Absence

-- | With no config anywhere, every answer is the one this repo gave before.
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
      -- And what the tree actually reads as, so the equality is no shared wrong answer.
      let rows = concat [ rs | (_p, Right rs) <- parallel ]
      assertEqual "states"
                  [ (Just "STARTED", Just True), (Just "READING", Just True)
                  , (Just "ABANDONED", Just True), (Just "LATER", Just True) ]
                  (states rows)
  ]
