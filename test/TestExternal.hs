-- | The note this daemon leaves org-glance when it writes a stored blob:
-- @\<store\>\/.org-glance\/meta\/EXTERNAL.jsonl@.
--
-- Two layers, and the split is deliberate.  The DOOR cases go through
-- 'Glance.Query.replaceSpans', the one function every write in this program
-- leaves through, so they state the rule once for all four of its callers.  The
-- ROUTE cases then drive the real HTTP surface — a structured command, a
-- materialize commit, a capture — to show that those callers really are that
-- function's callers, and that the door is therefore where the rule can live.
module TestExternal (spec) where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad (forM_)
import Data.Aeson (encode, object, (.=))
import Data.ByteString (ByteString)
import Data.List (sort)
import Data.Text (Text)
import Network.HTTP.Types (renderQuery)
import Network.Wai (Application)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults (digestOnDisk, document, entryAs, withTempDirNamed)
import TestWire (assertOk, capture, command, keywordArg, postTo, serverAt, status)

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Time as Time

import Data.Org.External ( Completion (..), blobIdOf, completionLine, completionsFile
                         , completionsPathOf, externalFile, externalLine
                         , externalPathOf, noteCompletion )
import Data.Maybe (fromJust)
import Data.Org.Index (metaDir)
import Data.Org.Trash (trashBlob, trashPathFor)
import Data.Org.Walk (isDerived)

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL
import Glance.Query (Span (Span), WriteFailure, blobPathIn, replaceSpans, storeRootIn)

-- Fixtures

-- | An entry as org-glance stores one: a level-one headline in STATE whose
-- drawer names the id IDENT the index keys its record by, over a line of body.
entry :: Text -> Text -> Text
entry ident state = entryAs ident (state <> " Entry " <> ident) <> "body\n"

-- | Write ID's blob under DIR's store and answer its path.  The layout is the
-- LIBRARY's ('Glance.Query.blobPathIn' over 'storeRootIn'), so a fixture and the
-- writer it stands in for shard an id the same way.
blobIn :: FilePath -> Text -> Text -> IO FilePath
blobIn dir ident text = do
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path text
  pure path
  where path = blobPathIn (storeRootIn dir) ident

-- | A temp directory to stand a store up in.
withStore :: (FilePath -> Assertion) -> Assertion
withStore = withTempDirNamed "external"

-- | DIR's notification file, whether or not it is there.
notePath :: FilePath -> FilePath
notePath dir = dir </> ".org-glance" </> metaDir </> externalFile

-- | The lines DIR's notification file holds, none when there is no file.
noteLines :: FilePath -> IO [ByteString]
noteLines dir = do
  there <- doesFileExist (notePath dir)
  if there then BC.lines <$> BC.readFile (notePath dir) else pure []

-- | The @id@ of each line the notification file holds, in file order.
--
-- Read by prefix rather than by a JSON decode, which is the point: the field
-- ORDER is part of the frozen contract, so a reader here that accepted any
-- order would not be reading the contract at all.  A line that does not open
-- @{"id":"@ fails the case by yielding a name no assertion expects.
notedIds :: FilePath -> IO [Text]
notedIds dir = map idOf <$> noteLines dir
  where idOf line = maybe ("MALFORMED: " <> TE.decodeUtf8 line) (T.takeWhile (/= '"'))
                          (T.stripPrefix "{\"id\":\"" (TE.decodeUtf8 line))

-- | The span NEEDLE occupies in TEXT, which is how these cases name an edit.
spanOf :: Text -> Text -> Span
spanOf text needle = Span at (at + T.length needle)
  where at = T.length (fst (T.breakOn needle text))

-- | Replace FROM with TO in PATH through the write door, failing the case when
-- the write does not land.
splice :: FilePath -> Text -> Text -> Assertion
splice path from to = do
  text <- document path
  digest <- digestOnDisk path
  landed =<< replaceSpans path digest [(spanOf text from, to)]

-- | Fail unless a write landed.
landed :: Either WriteFailure Text -> Assertion
landed = either (assertFailure . ("the write was refused: " <>) . show) (const (pure ()))

-- The door

doorSpec :: TestTree
doorSpec = testGroup "The write door"
  [ testCase "a blob write appends one line naming its entry" $
      withStore $ \dir -> do
        path <- blobIn dir "abcdef" (entry "abcdef" "TODO")
        splice path "TODO" "DONE"
        assertEqual "one line, naming the blob's id" ["abcdef"] =<< notedIds dir

    -- The id is the BLOB's rather than the edit's: an edit under a child moves
    -- the entry org-glance keyed the file by, and that is the record a refresh
    -- replaces.
  , testCase "an edit under a child is noted under the entry's id" $
      withStore $ \dir -> do
        path <- blobIn dir "abcdef"
                  (entry "abcdef" "TODO" <> T.unlines
                     [ "** TODO Child", ":PROPERTIES:", ":ORG_GLANCE_ID: kid", ":END:" ])
        splice path "TODO Child" "DONE Child"
        assertEqual "the entry's id" ["abcdef"] =<< notedIds dir

    -- A command over several rows of one file is ONE `editFile', so it is one
    -- line: the id names the entry, not the edit.
  , testCase "one write of several spans is one line" $
      withStore $ \dir -> do
        path <- blobIn dir "abcdef" (entry "abcdef" "TODO" <> "** TODO Child\n")
        text <- document path
        digest <- digestOnDisk path
        landed =<< replaceSpans path digest
                     [ (spanOf text "TODO Entry", "DONE Entry")
                     , (spanOf text "TODO Child", "DONE Child") ]
        assertEqual "one line for one write" ["abcdef"] =<< notedIds dir

    -- Each case below is a file this daemon legitimately writes and must not
    -- note, and they are the four shapes that reach the door.
  , testCase "an ordinary document outside the store is not noted" $
      withStore $ \dir -> do
        let path = dir </> "notes.org"
        TIO.writeFile path (entry "abcdef" "TODO")
        splice path "TODO" "DONE"
        assertEqual "no lines" [] =<< noteLines dir
        assertBool "and no file made" . not =<< doesFileExist (notePath dir)

  , testCase "a config layer under the store is not noted" $
      withStore $ \dir -> do
        let path = dir </> ".org-glance" </> "config" </> "system.org"
        createDirectoryIfMissing True (takeDirectory path)
        TIO.writeFile path "#+TODO: TODO | DONE\n"
        splice path "TODO |" "TODO NEXT |"
        assertEqual "nothing noted" [] =<< noteLines dir

    -- Inside `data/' and still not the blob: `data.org' is the one file
    -- org-glance keys an entry by, so another document beside it has no record
    -- to refresh.
  , testCase "another org file in a blob's own directory is not noted" $
      withStore $ \dir -> do
        blob <- blobIn dir "abcdef" (entry "abcdef" "TODO")
        let path = takeDirectory blob </> "notes.org"
        TIO.writeFile path (entry "abcdef" "TODO")
        splice path "TODO" "DONE"
        assertEqual "nothing noted" [] =<< noteLines dir

  , testCase "a blob whose entry claims no id is skipped in silence" $
      withStore $ \dir -> do
        path <- blobIn dir "abcdef" "* TODO Anonymous\n"
        splice path "TODO" "DONE"
        assertEqual "nothing noted" [] =<< noteLines dir

    -- The refusal happens before anything is renamed into place, so there is no
    -- write to note.
  , testCase "a write that drifts notes nothing" $
      withStore $ \dir -> do
        path <- blobIn dir "abcdef" (entry "abcdef" "TODO")
        text <- document path
        outcome <- replaceSpans path "notthedigest" [(spanOf text "TODO", "DONE")]
        assertBool "refused" (either (const True) (const False) outcome)
        assertEqual "nothing noted" [] =<< noteLines dir

  , testCase "the file and the meta directory are created where there are none" $
      withStore $ \dir -> do
        path <- blobIn dir "abcdef" (entry "abcdef" "TODO")
        assertBool "nothing there yet" . not =<< doesFileExist (notePath dir)
        splice path "TODO" "DONE"
        assertBool "created" =<< doesFileExist (notePath dir)
  ]

-- The line

formatSpec :: TestTree
formatSpec = testGroup "The line, as frozen"
  [ testCase "two fields, id then at, newline-terminated" $
      assertEqual "golden"
                  "{\"id\":\"abcdef\",\"at\":\"2026-08-03T04:21:07Z\"}\n"
                  (externalLine "abcdef" (stamp "2026-08-03T04:21:07"))

    -- The values go through the JSON encoder and the keys do not, which is what
    -- fixes the order without leaving an id unescaped.
  , testCase "an id carrying JSON metacharacters is escaped" $
      assertEqual "escaped"
                  "{\"id\":\"a\\\"b\\\\c\",\"at\":\"2026-08-03T04:21:07Z\"}\n"
                  (externalLine "a\"b\\c" (stamp "2026-08-03T04:21:07"))

  , testCase "the stamp is UTC at second resolution, whatever the clock carries" $
      assertEqual "truncated"
                  "{\"id\":\"i\",\"at\":\"2026-08-03T04:21:07Z\"}\n"
                  (externalLine "i" (Time.addUTCTime 0.75 (stamp "2026-08-03T04:21:07")))

  , testCase "the entry is the first headline, and a child's id is not it" $ do
      assertEqual "first" (Just "abcdef") (blobIdOf (entry "abcdef" "TODO"))
      assertEqual "not the child's" Nothing
                  (blobIdOf ("* Anonymous\n" <> entry "kid" "TODO"))
      assertEqual "no headline" Nothing (blobIdOf "just text\n")

    -- The parse is seeded from `defaultContext', so a keyword only a tag config
    -- declares folds into the title.  The id is a property and does not care,
    -- which is what lets one seed serve every store.
  , testCase "an unrecognised keyword costs the id nothing" $
      assertEqual "still found" (Just "abcdef") (blobIdOf (entry "abcdef" "READING"))
  ]
  where stamp = Time.parseTimeOrError True Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

-- Where a write is noted

pathSpec :: TestTree
pathSpec = testGroup "Where a write is noted"
  [ testCase "beside that store's own index" $
      assertEqual "path" (Just ("/home/me/sync/.org-glance/meta/" <> externalFile))
                  (externalPathOf "/home/me/sync/.org-glance/data/ab/cdef/data.org")

    -- org-glance falls back to `data/<id>' for an id of two characters or
    -- fewer, so the shard is no part of the rule.
  , testCase "an unsharded blob lands in the same place" $
      assertEqual "path" (Just ("/s/.org-glance/meta/" <> externalFile))
                  (externalPathOf "/s/.org-glance/data/ab/data.org")

  , testCase "a nested store notes its own writes, not the outer store's" $
      assertEqual "the innermost one"
                  (Just ("/a/.org-glance/data/x/y/.org-glance/meta/" <> externalFile))
                  (externalPathOf "/a/.org-glance/data/x/y/.org-glance/data/ab/cd/data.org")

  , testCase "and nowhere at all for a file that is not a blob" $
      forM_ [ ("no store above it", "/home/me/sync/notes.org")
            , ("a config layer", "/s/.org-glance/config/system.org")
            , ("an overview mirror", "/s/.org-glance/overviews/all/overview.org")
            , ("a blob's history", "/s/.org-glance/data/ab/cd/occurrences/2026.org")
            , ("a file beside the blob", "/s/.org-glance/data/ab/cd/notes.org")
            ] $ \(what, path) -> assertEqual what Nothing (externalPathOf path)
  ]

-- Append-only

appendSpec :: TestTree
appendSpec = testGroup "Append-only"
  [ testCase "each write adds a line and moves none of the ones before it" $
      withStore $ \dir -> do
        path <- blobIn dir "abcdef" (entry "abcdef" "TODO")
        forM_ (zip ["DONE", "NEXT", "TODO"] [1 :: Int ..]) $ \(state, n) -> do
          text <- document path
          splice path (T.takeWhile (/= ' ') (T.drop 2 text)) state
          assertEqual ("lines after write " <> show n) n . length =<< noteLines dir
        assertEqual "one per write, same id" (replicate 3 "abcdef") =<< notedIds dir

    -- Nothing already in the file is ever rewritten, so a reader's leftovers
    -- and a hand-written line both survive a write.
  , testCase "a line already in the file survives" $
      withStore $ \dir -> do
        createDirectoryIfMissing True (takeDirectory (notePath dir))
        BC.writeFile (notePath dir) "{\"id\":\"earlier\",\"at\":\"2026-01-01T00:00:00Z\"}\n"
        path <- blobIn dir "abcdef" (entry "abcdef" "TODO")
        splice path "TODO" "DONE"
        assertEqual "the old line kept its place" ["earlier", "abcdef"] =<< notedIds dir

    -- The other files under `meta' are org-glance's write-ahead log, and this
    -- daemon must never be the reason a record moved.
  , testCase "no other file under meta is touched" $
      withStore $ \dir -> do
        let wal = takeDirectory (notePath dir) </> "headlines.jsonl"
        createDirectoryIfMissing True (takeDirectory wal)
        BC.writeFile wal "{\"id\":\"abcdef\",\"state\":\"TODO\"}\n"
        path <- blobIn dir "abcdef" (entry "abcdef" "TODO")
        splice path "TODO" "DONE"
        assertEqual "the log is the log" "{\"id\":\"abcdef\",\"state\":\"TODO\"}\n"
          =<< BC.readFile wal

    -- One open and one write per call, in append mode, so lines interleave
    -- whole: a torn one would hand the reader a record it cannot parse and the
    -- refresh would stop on it.
  , testCase "concurrent writes land as whole lines" $
      withStore $ \dir -> do
        paths <- traverse (\i -> blobIn dir (ident i) (entry (ident i) "TODO")) [1 .. 8]
        mapConcurrently_ (\p -> splice p "TODO" "DONE") paths
        ls <- noteLines dir
        assertEqual "a line each" 8 (length ls)
        assertEqual "every id, once" (sort (map ident [1 .. 8])) . sort =<< notedIds dir
  ]
  where ident i = T.pack ("id" <> show (i :: Int) <> "0000")

-- The routes that write

routeSpec :: TestTree
routeSpec = testGroup "The routes that write"
  [ testCase "POST /command notes the blob it wrote" $
      withStore $ \dir -> do
        _ <- blobIn dir "abcdef" (entry "abcdef" "TODO")
        a <- serverOver dir
        assertOk =<< postTo a "/command"
                       (command "set-state" ["abcdef"] (keywordArg (Just "DONE")))
        assertEqual "noted" ["abcdef"] =<< notedIds dir

  , testCase "POST /headline notes the blob it wrote" $
      withStore $ \dir -> do
        path <- blobIn dir "abcdef" (entry "abcdef" "TODO")
        a <- serverOver dir
        digest <- digestOnDisk path
        r <- postTo a ("/headline" <> renderQuery True [("id", Just "abcdef")])
               (encode (object [ "org" .= entry "abcdef" "DONE", "digest" .= digest ]))
        assertEqual "status" 200 (status r)
        assertEqual "noted" ["abcdef"] =<< notedIds dir

    -- A capture writes the tree's inbox, which is an ordinary document: the one
    -- write route that has no blob to name.
  , testCase "a capture notes nothing" $
      withStore $ \dir -> do
        _ <- blobIn dir "abcdef" (entry "abcdef" "TODO")
        a <- serverOver dir
        assertOk =<< postTo a "/command" (capture "Fresh")
        assertEqual "nothing noted" [] =<< noteLines dir
  ]

-- | A server over DIR, loaded the way @glance serve@ loads one.  No @--assets@:
-- nothing here asks for a page.
serverOver :: FilePath -> IO Application
serverOver dir = fst <$> serverAt Nothing dir

spec :: TestTree
spec = testGroup "External"
  [doorSpec, formatSpec, pathSpec, appendSpec, routeSpec, completionSpec, trashSpec]

-- | DELETION IS A MOVE, never an unlink.  A blob is the canonical document and
-- the index is its projection, so the one destructive command this daemon has
-- takes the bytes out of the live tree and keeps them: compressed, under
-- @trash\/@, with the shard the id is spelled by carried over.
trashSpec :: TestTree
trashSpec = testGroup "Trash"
  [ testCase "a blob's trash path keeps the shard the id is spelled by" $
      assertEqual "under trash, gzipped"
        (Just (storeRootIn "/t" </> "trash" </> "a7" </> "92f0" </> "data.org.gz"))
        (trashPathFor "/t" (blobPathIn (storeRootIn "/t") "a792f0"))

    -- Only a blob: a row in a shared org file is many rows' document, and
    -- moving it would take the others with it.
  , testCase "and nothing else has one" $
      assertEqual "not a blob" Nothing (trashPathFor "/t" "/t/inbox.org")

  , testCase "the bytes are kept, and the live tree loses them" $
      withTempDirNamed "trash-move" $ \root -> do
        let blob = blobPathIn (storeRootIn root) "a792f0"
        createDirectoryIfMissing True (takeDirectory blob)
        TIO.writeFile blob "* TODO one :archive:\n"
        put <- trashBlob root blob
        dest <- either (assertFailure . T.unpack) pure put
        assertEqual "the original is out of the tree" False =<< doesFileExist blob
        assertEqual "and the trash holds it" True =<< doesFileExist dest
        kept <- GZip.decompress <$> BL.readFile dest
        assertEqual "byte for byte" "* TODO one :archive:\n" kept

    -- A SECOND DELETION OF ONE ID is the first one's bytes being asked for
    -- again: refused, and what is already kept is what stays.
  , testCase "a trash that already holds the id keeps what it has" $
      withTempDirNamed "trash-twice" $ \root -> do
        let blob = blobPathIn (storeRootIn root) "a792f0"
            write t = do createDirectoryIfMissing True (takeDirectory blob)
                         TIO.writeFile blob t
        write "first\n"
        _ <- trashBlob root blob
        write "second\n"
        again <- trashBlob root blob
        case again of
          Right _  -> assertFailure "a second deletion overwrote the first"
          Left why -> assertBool ("names the trash: " <> T.unpack why)
                                 ("trash" `T.isInfixOf` why)
        assertEqual "the second is still in the tree" True =<< doesFileExist blob
        kept <- GZip.decompress <$> BL.readFile
                  (fromJust (trashPathFor root blob))
        assertEqual "and the first is what is kept" "first\n" kept

    -- THE TRASH IS NOT WALKED, and gets that from the denylist rather than from
    -- the extension: a `.org' put there is still declined.
  , testCase "nothing under trash is walked" $
      assertEqual "declined like every other derived name" True
        (isDerived (storeRootIn "/t" </> "trash" </> "a7" </> "92f0" </> "data.org"))
  ]

-- | THE SECOND LEDGER, and the first one keyed by something other than a blob:
-- one line per completion of a repeating entry, under the SERVED root's own
-- store.  Derived, never truth — the org file already carries the shifted stamp
-- and the reset keyword, so a tree with no store repeats and records nothing.
completionSpec :: TestTree
completionSpec = testGroup "Completions"
  [ testCase "four fields, in the order the contract froze" $
      assertEqual "golden"
        "{\"id\":\"abcdef\",\"at\":\"2026-08-03T04:21:07Z\",\"state\":\"TODO\",\
        \\"shifted\":\"<2026-08-15 Sat +1w>\"}\n"
        (completionLine (Completion "abcdef" "TODO" "<2026-08-15 Sat +1w>")
                        (stamp "2026-08-03T04:21:07"))

  , testCase "the values are escaped and the keys are not" $
      assertEqual "escaped"
        "{\"id\":\"a\\\"b\",\"at\":\"2026-08-03T04:21:07Z\",\"state\":\"A\\\\B\",\
        \\"shifted\":\"x\"}\n"
        (completionLine (Completion "a\"b" "A\\B" "x") (stamp "2026-08-03T04:21:07"))

    -- A tree with no `.org-glance' keeps org's own behaviour and no ledger: no
    -- daemon makes a store directory it was not given.
  , testCase "a tree with no store has nowhere to record, and none is made" $
      withTempDirNamed "no-store" $ \dir -> do
        assertEqual "nothing to write to" Nothing =<< completionsPathOf dir
        noteCompletion dir (Completion "i" "TODO" "<2026-08-15 Sat +1w>")
        assertEqual "and none was created" False
          =<< doesFileExist (dir </> ".org-glance" </> "meta" </> completionsFile)

  , testCase "a tree with one records under its own meta directory" $
      withTempDirNamed "store" $ \dir -> do
        createDirectoryIfMissing True (dir </> ".org-glance" </> "meta")
        let note = dir </> ".org-glance" </> "meta" </> completionsFile
        assertEqual "the path it answers with" (Just note) =<< completionsPathOf dir
        noteCompletion dir (Completion "i" "TODO" "<2026-08-15 Sat +1w>")
        noteCompletion dir (Completion "j" "NEXT" "<2026-08-16 Sun +1d>")
        -- APPEND-ONLY: the second line joins the first rather than replacing it.
        lines' <- T.lines <$> document note
        assertEqual "one line per completion" 2 (length lines')
        assertBool "the first is still there" ("\"id\":\"i\"" `T.isInfixOf` head lines')
        assertBool "and the second followed it" ("\"id\":\"j\"" `T.isInfixOf` last lines')


    -- END TO END: a repeating row completed through the write route.  ONE write
    -- moves the stamp and resets the keyword, and the ledger line rides its
    -- success -- so the file stays org's own and the history sits beside it.
  , testCase "completing a repeating row shifts it, resets it and records it" $
      withStore $ \dir -> do
        -- The planning line is the ONE line after the title and BEFORE any
        -- drawer, so the fixture is spelled out rather than built from
        -- `entryAs', which puts the drawer straight under the headline.
        let repeating = "* TODO Water the plants\n\
                        \SCHEDULED: <2020-01-06 Mon +1w>\n\
                        \  :PROPERTIES:\n\
                        \  :ORG_GLANCE_ID: abcdef\n\
                        \  :END:\n"
        path <- blobIn dir "abcdef" repeating
        a <- serverOver dir
        assertOk =<< postTo a "/command"
                       (command "set-state" ["abcdef"] (keywordArg (Just "DONE")))
        after <- document path
        assertBool ("reset, not closed: " <> show after) ("* TODO Water" `T.isInfixOf` after)
        assertBool ("the cookie is kept: " <> show after) ("+1w>" `T.isInfixOf` after)
        assertBool ("and the stamp moved: " <> show after)
                   (not ("<2020-01-06" `T.isInfixOf` after))
        recorded <- T.lines <$> document (dir </> ".org-glance" </> "meta"
                                              </> completionsFile)
        assertEqual "one completion" 1 (length recorded)
        assertBool ("names the row: " <> show recorded)
                   ("\"id\":\"abcdef\"" `T.isInfixOf` head recorded)
        assertBool ("and the state it reset to: " <> show recorded)
                   ("\"state\":\"TODO\"" `T.isInfixOf` head recorded)

    -- A row with no repeater takes the plain path and records nothing, so the
    -- ledger describes repeats and only repeats.
  , testCase "an ordinary state change records no completion" $
      withStore $ \dir -> do
        _ <- blobIn dir "abcdef" (entry "abcdef" "TODO")
        a <- serverOver dir
        assertOk =<< postTo a "/command"
                       (command "set-state" ["abcdef"] (keywordArg (Just "DONE")))
        assertEqual "no ledger for a plain close" False
          =<< doesFileExist (dir </> ".org-glance" </> "meta" </> completionsFile)
  ]
  where stamp = Time.parseTimeOrError True Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
