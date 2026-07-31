-- | The facade under test.  Everything here goes through 'Glance.Query': the
-- module imports no parser internals, so a wire shape that needs one fails to
-- compile instead of failing a renderer.
module TestQuery (spec) where

import Control.Concurrent (getNumCapabilities, rtsSupportsBoundThreads)
import Control.Monad (forM_, replicateM)
import Data.Aeson (Value (Bool, Object, String), eitherDecodeFileStrict')
import Data.Char (isDigit)
import Data.List (foldl', nub, sort)
import Data.Text (Text)
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults ( columnKeysOf, columnOf, entryAs, field, intAt, listAt
                    , orgFile, textAt, viewDir, withTempDirNamed )

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Glance.Query ( HeadlineParts (..), HeadlineRecord (..), LoadFailure (..)
                    , QueryResult (..), Span (..), archiveEdits, archived, defaultWalk
                    , displayText, headlineParts, loadDir, loadDirFilesSerially
                    , loadDirFilesWith, loadFile, matchesSearch, recomposedSubtree
                    , setStateEdits, subtreeText, viewJSON )

-- Fixtures

-- | One file the parser rejects, kept out of 'viewDir' so the golden stays put.
brokenDir :: FilePath
brokenDir = "test/fixtures/broken"

goldenPath :: FilePath
goldenPath = "test/fixtures/sample-view.json"

viewTitle :: Text
viewTitle = "Sample — glance"

-- | Run K over the sample directory's records.
withRecords :: ([HeadlineRecord] -> Assertion) -> Assertion
withRecords k = loadDir viewDir >>= k . qrRecords

-- | Run K over the sample directory's view.
withView :: (Value -> Assertion) -> Assertion
withView k = withRecords (k . viewJSON viewTitle)

-- | Run K over the view DOC alone makes, written into a file of its own so the
-- load path is the ordinary one.
withViewOf :: Text -> (Value -> Assertion) -> Assertion
withViewOf doc k = withTempDirNamed "view" $ \dir -> do
  path <- orgFile dir "tree.org" doc
  loadFile path >>= either (assertFailure . show) (k . viewJSON viewTitle)

-- | An outline with a level at every depth the guides have a case for: a root
-- with a child and a grandchild, a second child under the same root, and a
-- second root.  The golden's fixture is flat, so this is where the ladder is.
nested :: Text
nested = T.unlines
  [ "* one", "** two", "*** three", "** four", "* five" ]

-- JSON accessors this module alone needs; the rest come from 'TestDefaults'.

text :: Value -> IO Text
text (String t) = pure t
text v = assertFailure ("expected a string, got " <> show v)

keysOf :: Value -> IO [Text]
keysOf (Object o) = pure (map Key.toText (KM.keys o))
keysOf v = assertFailure ("expected an object, got " <> show v)

-- | The value at KEY of every element of the array at ARR of V.
each :: Text -> Text -> Value -> IO [Value]
each arr k v = listAt arr v >>= mapM (field k)

-- | KEY of V as a boolean, or 'Nothing' where V does not carry it — an
-- optional flag, so its absence is an answer rather than a failure.
maybeBoolAt :: Text -> Value -> IO (Maybe Bool)
maybeBoolAt key (Object o) = case KM.lookup (Key.fromText key) o of
  Nothing       -> pure Nothing
  Just (Bool b) -> pure (Just b)
  Just other    -> assertFailure ("expected a boolean at " <> show key
                                    <> ", got " <> show other)
maybeBoolAt key v = assertFailure ("expected an object with " <> show key
                                     <> ", got " <> show v)

-- Spec

spec :: TestTree
spec = testGroup "Query"
  [ loadSpec, parallelSpec, cellSpec, searchSpec, viewSpec, schemaSpec, commandSpec
  , lensSpec ]

-- | The properties lens: a subtree split into body and drawer, and put back.
--
-- One rule under all of it — every byte of a subtree has one owner.  So the
-- assertions are about bytes rather than about shapes: what the body keeps, what
-- a property that nobody touched is written back as, and that decompose followed
-- by recompose is the identity on the file.
lensSpec :: TestTree
lensSpec = testGroup "Properties lens"
  [ testGroup "decompose"
    [ testCase "a drawer leaves the body and comes back as pairs" $
        withParts drawered $ \r -> do
          assertEqual "the body is the subtree without the headline's drawer lines"
                      (T.unlines [ "* TODO First :one:", "body line", "** Child"
                                 , ":PROPERTIES:", ":ORG_GLANCE_ID: kid", ":END:"
                                 , "child body" ])
                      (hpBody (headlineParts r))
          assertEqual "the pairs, in file order"
                      [("ORG_GLANCE_ID", "first"), ("EFFORT", "0:30")]
                      (hpProperties (headlineParts r))

    , testCase "a headline with no drawer is its whole subtree and no pairs" $
        withParts (T.unlines ["* TODO Bare", "body line"]) $ \r -> do
          assertEqual "the body is the subtree" (subtreeText r) (hpBody (headlineParts r))
          assertEqual "and there is nothing to show" [] (hpProperties (headlineParts r))

    , testCase "the planning line stays in the body, ahead of where the drawer was" $
        withParts planned $ \r ->
          assertEqual "body"
                      (T.unlines [ "* TODO Timed"
                                 , "SCHEDULED: <2026-08-01 Sat 09:30> DEADLINE: <2026-08-05 Wed>"
                                 , "after" ])
                      (hpBody (headlineParts r))

      -- The lens is over ONE headline: a child's drawer belongs to the child's
      -- own lens and is body text here, byte for byte.
    , testCase "a child's drawer stays in the body untouched" $
        withParts drawered $ \r -> do
          let parts = headlineParts r
          assertContains "the child keeps its own drawer, whole"
                         ":PROPERTIES:\n:ORG_GLANCE_ID: kid\n:END:\n" (hpBody parts)
          assertEqual "and it is no part of this headline's pairs"
                      ["ORG_GLANCE_ID", "EFFORT"] (map fst (hpProperties parts))

    , testCase "unicode is cut by characters, not bytes" $
        withParts unicoded $ \r -> do
          assertEqual "the body keeps its text"
                      (T.unlines ["* TODO Привет мир :unicode:", "тело письма"])
                      (hpBody (headlineParts r))
          assertEqual "and the value is the file's"
                      [("ORG_GLANCE_ID", "привет"), ("CATEGORY", "письма")]
                      (hpProperties (headlineParts r))

      -- The drawer's own spelling is the drawer's business: the pairs a client
      -- sees are stripped, and the file keeps whatever it wrote.
    , testCase "odd spacing is stripped out of the pairs and left in the file" $
        withParts oddly $ \r ->
          assertEqual "the pairs as a panel would show them"
                      [("A", "one"), ("B", ""), ("C", "three")]
                      (hpProperties (headlineParts r))
    ]

  , testGroup "recompose"
    [ testCase "decompose then recompose is the subtree, byte for byte" $
        mapM_ roundTrips [drawered, planned, unicoded, oddly, indented, crlf
                         , T.unlines ["* TODO Bare", "body"]
                         , "* Ends at the drawer\n:PROPERTIES:\n:A: 1\n:END:" ]

    , testCase "a property nobody touched keeps its own line, odd spacing and all" $
        withParts oddly $ \r -> do
          let parts = headlineParts r
              back = recomposedSubtree r (hpBody parts) (hpProperties parts)
          assertContains "the crooked line is the file's own" ":A:one" back
          assertContains "and the empty one too" ":B:\n" back
          assertContains "and the padded one" ":C:   three   \n" back

    , testCase "an edited property is rendered canonically, under the drawer's indent" $
        withParts indented $ \r -> do
          let parts = headlineParts r
              back = recomposedSubtree r (hpBody parts) [("A", "moved"), ("B", "2")]
          assertContains "the edited one is canonical, indented like its neighbours"
                         "  :A: moved\n" back
          assertContains "the untouched one is verbatim" "  :B:  2\n" back

    , testCase "an added property joins the drawer where the client put it" $
        withParts drawered $ \r -> do
          let parts = headlineParts r
              back = recomposedSubtree r (hpBody parts)
                       (hpProperties parts <> [("ADDED", "yes")])
          assertEqual "the drawer, in order"
                      [":PROPERTIES:", ":ORG_GLANCE_ID: first", ":EFFORT: 0:30"
                      , ":ADDED: yes", ":END:"]
                      (drawerOf back)

    , testCase "a dropped property is simply not written" $
        withParts drawered $ \r -> do
          let back = recomposedSubtree r (hpBody (headlineParts r)) [("EFFORT", "0:30")]
          assertEqual "what is left" [":PROPERTIES:", ":EFFORT: 0:30", ":END:"] (drawerOf back)

    , testCase "an empty list takes the drawer away" $
        withParts drawered $ \r -> do
          let parts = headlineParts r
              back = recomposedSubtree r (hpBody parts) []
          assertEqual "the body alone" (hpBody parts) back
          assertBool "and the headline's own drawer is gone with it"
                     (not (":EFFORT:" `T.isInfixOf` back))

    , testCase "a drawer for a headline that never had one goes after the title line" $
        withParts (T.unlines ["* TODO Bare", "body line"]) $ \r ->
          assertEqual "written where org writes one"
                      (T.unlines [ "* TODO Bare", ":PROPERTIES:", ":NEW: 1", ":END:"
                                 , "body line" ])
                      (recomposedSubtree r (subtreeText r) [("NEW", "1")])

    , testCase "and after the planning line when there is one" $
        withParts (T.unlines ["* TODO Timed", "SCHEDULED: <2026-08-01 Sat 09:30>", "after"]) $ \r ->
          assertEqual "the planning line keeps its place"
                      (T.unlines [ "* TODO Timed", "SCHEDULED: <2026-08-01 Sat 09:30>"
                                 , ":PROPERTIES:", ":NEW: 1", ":END:", "after" ])
                      (recomposedSubtree r (subtreeText r) [("NEW", "1")])

      -- The drawer's line is counted from the top of the subtree, which is the
      -- one place a client cannot have moved it from: the lines above it are the
      -- headline's own and the planning line.
    , testCase "an edit further down the body leaves the drawer where it was" $
        withParts drawered $ \r -> do
          let parts = headlineParts r
              edited = hpBody parts <> "one more line\n"
              back = recomposedSubtree r edited (hpProperties parts)
          assertEqual "the drawer still opens the line under the headline"
                      ":PROPERTIES:" (T.lines back !! 1)
          assertContains "and the addition landed" "one more line\n" back

    , testCase "a body shorter than the drawer's line takes it at the end" $
        withParts drawered $ \r ->
          assertEqual "appended, and terminated"
                      "* only\n:PROPERTIES:\n:A: 1\n:END:\n"
                      (recomposedSubtree r "* only" [("A", "1")])
    ]
  ]
  where
    roundTrips doc = withParts doc $ \r -> do
      let parts = headlineParts r
      assertEqual ("round trip of " <> show doc)
                  (subtreeText r) (recomposedSubtree r (hpBody parts) (hpProperties parts))

-- | The drawer TEXT holds, line by line and stripped — what a drawer says,
-- where the byte-level cases say how it is written.
drawerOf :: Text -> [Text]
drawerOf text' = takeWhile (/= ":END:") opened <> [":END:"]
  where opened = dropWhile (/= ":PROPERTIES:") (map T.strip (T.lines text'))

-- | WHAT must be somewhere in TEXT.
assertContains :: String -> Text -> Text -> Assertion
assertContains what needle text' =
  assertBool (what <> ": " <> show needle <> " is not in " <> show text')
             (needle `T.isInfixOf` text')

-- | Run K over the FIRST record DOC loads to, which is the headline every case
-- here is about.
withParts :: Text -> (HeadlineRecord -> Assertion) -> Assertion
withParts doc k = withTempDirNamed "lens" $ \dir -> do
  path <- orgFile dir "lens.org" doc
  loadFile path >>= either (assertFailure . show) first'
  where first' rs = case rs of
          (r : _rest) -> k r
          []          -> assertFailure "the fixture loaded no headlines"

-- | A headline with a drawer, a body, and a child carrying a drawer of its own.
drawered :: Text
drawered = T.unlines
  [ "* TODO First :one:"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: first"
  , ":EFFORT: 0:30"
  , ":END:"
  , "body line"
  , "** Child"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: kid"
  , ":END:"
  , "child body" ]

-- | A headline whose planning line sits between the title and the drawer.
planned :: Text
planned = T.unlines
  [ "* TODO Timed"
  , "SCHEDULED: <2026-08-01 Sat 09:30> DEADLINE: <2026-08-05 Wed>"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: timed"
  , ":END:"
  , "after" ]

unicoded :: Text
unicoded = T.unlines
  [ "* TODO Привет мир :unicode:"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: привет"
  , ":CATEGORY: письма"
  , ":END:"
  , "тело письма" ]

-- | Spacing org never writes and a file can still hold: no space after the
-- colon, a valueless key, and a padded value.
oddly :: Text
oddly = T.unlines
  [ "* TODO Odd", ":PROPERTIES:", ":A:one", ":B:", ":C:   three   ", ":END:", "body" ]

-- | The indentation org used to write drawers under, which a rendered line has
-- to match rather than replace.
indented :: Text
indented = T.unlines
  [ "* TODO Indented", "  :PROPERTIES:", "  :A: 1", "  :B:  2", "  :END:", "body" ]

crlf :: Text
crlf = T.intercalate "\r\n"
  [ "* TODO Windows", ":PROPERTIES:", ":A: 1", ":END:", "body", "" ]

-- | The pool answers what one thread answered.
--
-- The load reads its files on a pool ('Data.Org.Walk.mapFilesConcurrently')
-- and 'loadDirFilesSerially' is the same load with the pool taken out, so the
-- two are comparable directly — and everything else the library says about a
-- directory is a fold of that pair, which is why asserting it here covers the
-- rows, the counts and the id resolution at once.
--
-- The fixture is deliberately wider than any pool: forty documents, so work is
-- handed out rather than taken by one worker, plus one file of each failure
-- kind so a bucket cannot be compared on the happy path alone.
parallelSpec :: TestTree
parallelSpec = testGroup "Parallel load"
  [ testCase "the suite runs on the threaded runtime" $ do
      -- A non-threaded runtime has one capability whatever @-N@ says, and the
      -- pool silently degrades to a serial loop: every assertion below would
      -- still pass and none of them would be about parallelism.
      assertBool "-threaded" rtsSupportsBoundThreads
      caps <- getNumCapabilities
      assertBool ("capabilities: " <> show caps) (caps >= 1)

  , testCase "record for record, the pool load is the serial load" $ withCorpus $ \dir -> do
      (parallel, parErrs) <- loadDirFilesWith defaultWalk dir
      (serial, serErrs) <- loadDirFilesSerially defaultWalk dir
      assertEqual "unlistable directories" serErrs parErrs
      assertEqual "paths, in order" (map fst serial) (map fst parallel)
      assertEqual "outcomes, record for record"
                  (map (outcomeShape . snd) serial)
                  (map (outcomeShape . snd) parallel)

  , testCase "and the failures bucket the same way, in the same order" $ withCorpus $ \dir -> do
      (parallel, _) <- loadDirFilesWith defaultWalk dir
      (serial, _) <- loadDirFilesSerially defaultWalk dir
      -- Order-independent counts first — a bucket is a count in the wire
      -- headers — then the listing, which is deterministic by path sort.
      forM_ [ReadFailed, DecodeFailed, ParseFailed] $ \kind ->
        assertEqual ("count of " <> show kind)
                    (length (failuresOf kind serial)) (length (failuresOf kind parallel))
      assertEqual "the failing paths, in order" (failures serial) (failures parallel)
      assertEqual "one of each kind, so the comparison is not vacuous"
                  [1, 1, 1]
                  [ length (failuresOf kind parallel)
                  | kind <- [ReadFailed, DecodeFailed, ParseFailed] ]

  , testCase "a tree narrower than the pool loads whole" $ withTempDirNamed "narrow" $ \dir -> do
      -- The chunking edge: fewer files than there are workers, so most of them
      -- find the queue already empty.  One file is the file watch's own shape
      -- and skips the pool outright; zero files must not hang or fabricate a row.
      empty <- loadDirFilesWith defaultWalk dir
      assertEqual "no files at all" ([], 0) (shapes empty)
      _ <- orgFile dir "one.org" (entryAs "solo" "TODO solo")
      poolEqualsSerial "one file" 1 dir
      forM_ ["b.org", "c.org"] $ \name ->
        orgFile dir name (entryAs (T.pack name) ("TODO " <> T.pack name))
      poolEqualsSerial "three files" 3 dir

  , testCase "the sequence is the same on every run, ids resolved and all" $
      withCorpus $ \dir -> do
        -- Determinism where completion order could reach an answer:
        -- 'resolveIds' is first-wins over the sequence, and the corpus carries
        -- two files claiming one id with neither of them canonical, so the
        -- winner is decided by path sort alone.  A pool that reassembled by
        -- completion order would hand the id to whichever thread finished first.
        runs <- replicateM 5 (loadDir dir)
        assertEqual "one row order" 1 (length (nub (map (map hrId . qrRecords) runs)))
        assertEqual "one set of counts" 1
                    (length (nub [ (qrFiles r, qrParseFailures r, qrDecodeFailures r
                                   , qrReadFailures r) | r <- runs ]))
        let kept = [ (hrId r, hrFile r) | r <- qrRecords (head runs), hrId r == "shared" ]
        assertEqual "the shared id went to the file that sorts first"
                    [("shared", dir </> "a-claims-shared.org")] kept
        assertEqual "and it collides exactly once" 1
                    (length (qrIdCollisions (head runs)))
  ]

-- | DIR loaded both ways under WHAT: the pool's answer is the serial one,
-- record for record, and it carries FILES files.
poolEqualsSerial :: String -> Int -> FilePath -> Assertion
poolEqualsSerial what files dir = do
  parallel <- shapes <$> loadDirFilesWith defaultWalk dir
  serial <- shapes <$> loadDirFilesSerially defaultWalk dir
  assertEqual what serial parallel
  assertEqual (what <> ": all loaded") files (length (fst parallel))

-- | A tree wider than any pool: forty documents, two files claiming one id
-- between them, and one file of each failure kind — a parse failure, bytes that
-- are not UTF-8, and a dangling symlink the walk keeps and the read refuses.
withCorpus :: (FilePath -> IO a) -> IO a
withCorpus act = withTempDirNamed "parallel" $ \dir -> do
  forM_ [1 .. 40 :: Int] $ \i ->
    let name = "doc-" <> pad i in
    orgFile dir (name <> ".org")
            (entryAs (T.pack name) ("TODO " <> T.pack name) <> entryAs (T.pack (name <> "-b")) "DONE second")
  forM_ ["a-claims-shared.org", "z-claims-shared.org"] $ \name ->
    orgFile dir name (entryAs "shared" ("TODO from " <> T.pack name))
  _ <- orgFile dir "unparseable.org" "* A title with a :: double colon\n"
  BS.writeFile (dir </> "bad-utf8.org") (BS.pack [0x2a, 0x20, 0xff, 0xfe, 0x0a])
  createSymbolicLink "nowhere-at-all" (dir </> "dangling.org")
  act dir
  where pad i = let s = show i in replicate (2 - length s) '0' <> s

-- | R as the strings a comparison reads it by: every cell the wire carries, the
-- file it came from, and the extent and digest the write path pins to it.  The
-- parsed headline stays out — the facade keeps its type private, and the cells
-- and the extent are what a caller can see of it anyway.
shapeOf :: HeadlineRecord -> [Text]
shapeOf r = map T.pack
  [ hrFile r, show (hrId r), show (hrCategory r), show (hrDigest r)
  , show (hrSubtree r), show (hrKeywords r), show (hrState r), show (hrPriority r)
  , show (hrTitle r), show (hrTags r), show (hrScheduled r), show (hrDeadline r)
  , show (hrSearch r), show (T.length (hrDoc r)) ]

outcomeShape :: Either LoadFailure [HeadlineRecord] -> Either LoadFailure [[Text]]
outcomeShape = fmap (map shapeOf)

-- | A per-file load as the pair a test compares: the shaped outcomes and the
-- count of directories the walk could not list.
shapes :: ([(FilePath, Either LoadFailure [HeadlineRecord])], Int)
       -> ([(FilePath, Either LoadFailure [[Text]])], Int)
shapes (files, dirErrs) = ([ (path, outcomeShape o) | (path, o) <- files ], dirErrs)

-- | The files of FILES that failed, in the order they were loaded.
failures :: [(FilePath, Either LoadFailure [HeadlineRecord])] -> [(FilePath, LoadFailure)]
failures files = [ (path, why) | (path, Left why) <- files ]

failuresOf :: LoadFailure -> [(FilePath, Either LoadFailure [HeadlineRecord])] -> [FilePath]
failuresOf kind files = [ path | (path, why) <- failures files, why == kind ]

-- | What a load reports about the files behind it.
loadSpec :: TestTree
loadSpec = testGroup "Load"
  [ testCase "walks the .org files and skips what it cannot decode" $ do
      r <- loadDir viewDir
      assertEqual "files" 2 (qrFiles r)
      assertEqual "records" 6 (length (qrRecords r))
      assertEqual "decode failures" 1 (qrDecodeFailures r)
      assertEqual "parse failures" 0 (qrParseFailures r)
      assertEqual "read failures" 0 (qrReadFailures r)

  , testCase "an unparseable file is counted and contributes no rows" $ do
      r <- loadDir brokenDir
      assertEqual "files" 1 (qrFiles r)
      assertEqual "parse failures" 1 (qrParseFailures r)
      assertEqual "records" 0 (length (qrRecords r))

  , testCase "records carry the file's category" $ withRecords $ \recs ->
      assertEqual "categories" (replicate 6 "sample") (map hrCategory recs)
  ]

-- | The search text a filter runs over, and the display semantics it mirrors.
--
-- The expected strings are written down rather than taken from the renderer,
-- because agreeing with it is the whole point: @table-view.js@'s @displayText@
-- shows a bracket link by its description and squashes every run of control
-- characters to one space, and a server-side filter that did anything else
-- would answer a query differently from the same query typed into a renderer
-- holding its own rows.
searchSpec :: TestTree
searchSpec = testGroup "Search text"
  [ testCase "a bracket link shows its description" $ do
      assertEqual "described" "table-view" (displayText "[[https://x/y][table-view]]")
      assertEqual "bare" "https://x/y" (displayText "[[https://x/y]]")
      assertEqual "empty description" "file:a.org" (displayText "[[file:a.org][]]")

  , testCase "text around a link is kept, and several links resolve" $
      assertEqual "interleaved" "see readme and notes."
                  (displayText "see [[file:R.md][readme]] and [[file:N.org][notes]].")

  , testCase "an unclosed link is left as it is" $ do
      assertEqual "no closing bracket" "[[oops" (displayText "[[oops")
      assertEqual "not a link" "[[a]x]" (displayText "[[a]x]")

  , testCase "a run of control characters is one space" $ do
      assertEqual "newlines" "a b" (displayText "a\n\n\tb")
      -- The trailing run is the one the collapse above does not reach: it ends
      -- the string rather than separating two words, and it still leaves a space.
      assertEqual "trailing" "a " (displayText "a\n")

  , testCase "the row's search text is its cells, lowercased" $ withRecords $ \recs -> do
      let first' = head recs
      assertEqual "the whole row, cell by cell"
                  "next\SUBa\SUBship the table view\SUB:web:glance:\SUB2026-08-01 09:30\SUB2026-08-05"
                  (T.replace "\US" "\SUB" (hrSearch first'))

  , testCase "a query matches case-insensitively, trimmed, and never across cells" $
      withRecords $ \recs -> do
        let matching q = length (filter (matchesSearch q) recs)
        assertEqual "case" 1 (matching "SHIP THE TABLE")
        assertEqual "trimmed" 1 (matching "  ship the table  ")
        -- One row each, stated apart: a sum of two counts is met by 2 + 0.
        assertEqual "unicode, cyrillic mid-title" 1 (matching "печатник")
        assertEqual "unicode, cyrillic title" 1 (matching "Привет")
        assertEqual "an empty query is every row" 6 (matching "")
        assertEqual "blank is empty too" 6 (matching "   ")
        -- The cells are joined by a character no cell can hold, so the end of
        -- one and the start of the next never read as one string.
        assertEqual "across the join" 0 (matching "next a")
  ]

-- | Cells are cut from the source, and dates are spelled the way the wire
-- wants them rather than the way org does.
cellSpec :: TestTree
cellSpec = testGroup "Cells"
  [ testCase "titles and tags come from the source, unicode included" $ withRecords $ \recs -> do
      assertEqual "titles"
                  [ "Ship the table view", "Привет мир", "Reply from the печатник"
                  , "Plain headline without a state", "Drop the old renderer"
                  , "Read the schema" ]
                  (map hrTitle recs)
      assertEqual "tags"
                  [":web:glance:", ":unicode:", "", "", ":cleanup:", ":web:"]
                  (map hrTags recs)

  , testCase "states are the keywords verbatim, custom ones too" $ withRecords $ \recs ->
      assertEqual "states"
                  [Just "NEXT", Just "TODO", Just "WAITING", Nothing, Just "CANCELLED", Just "DONE"]
                  (map hrState recs)

  , testCase "priorities are the letter alone" $ withRecords $ \recs ->
      assertEqual "priorities"
                  [Just "A", Just "B", Nothing, Nothing, Just "C", Nothing]
                  (map hrPriority recs)

  , testCase "dates are ISO, with a time only when the source spelled one" $ withRecords $ \recs -> do
      assertEqual "scheduled"
                  [Just "2026-08-01 09:30", Just "2026-08-03", Nothing, Nothing, Nothing, Nothing]
                  (map hrScheduled recs)
      assertEqual "deadline"
                  [Just "2026-08-05", Nothing, Just "2026-08-10 17:00", Nothing, Nothing, Nothing]
                  (map hrDeadline recs)

  , testCase "an ORG_GLANCE_ID is the row id" $ withRecords $ \recs ->
      assertEqual "id" ["ship-table-view"] (map hrId (take 1 recs))

  , testCase "without one the row id is FILE:START" $ withRecords $ \recs ->
      case drop 1 recs of
        (r : _) -> do
          let (prefix, offset) = T.breakOnEnd ":" (hrId r)
          assertEqual "file prefix" (T.pack (hrFile r) <> ":") prefix
          assertBool ("offset in " <> show (hrId r))
                     (not (T.null offset) && T.all isDigit offset)
        [] -> assertFailure "expected more than one record"
  ]

-- | The view document itself.  The golden pins every value in it, so what is
-- left to state separately is the one thing a regenerated golden would carry
-- along without anyone noticing: the column order five other places index by.
viewSpec :: TestTree
viewSpec = testGroup "View"
  [ testCase "matches test/fixtures/sample-view.json" $ do
      decoded <- eitherDecodeFileStrict' goldenPath
      case decoded of
        Left err       -> assertFailure ("golden JSON: " <> err)
        Right expected -> withView (assertEqual "view" expected)

  , testCase "columns are the headline view's, in order" $ withView $ \v -> do
      keys <- columnKeysOf v
      assertEqual "column keys"
        ["state", "priority", "title", "tag", "scheduled", "deadline"] keys

  -- SCHEMA.md's experimental `depth': a renderer hint about where a row sits
  -- in the outline, 0-based, so the golden's six top-level rows are all zero
  -- and the ladder below is where the counting is checked.
  , testCase "every row carries its outline depth, counted from zero" $
      withViewOf nested $ \v -> do
        rows <- listAt "rows" v
        depths <- mapM (intAt "depth") rows
        assertEqual "depths" [0, 1, 2, 1, 0] depths

  , testCase "and it is a field of the row, never a cell" $
      withViewOf nested $ \v -> do
        cols <- columnKeysOf v
        rows <- listAt "rows" v
        assertBool "depth is a column" ("depth" `notElem` cols)
        cells <- mapM (\r -> field "cells" r >>= keysOf) rows
        assertBool (show cells <> " names depth") (all ("depth" `notElem`) cells)
  ]

-- | Shapes SCHEMA.md requires of any producer.
schemaSpec :: TestTree
schemaSpec = testGroup "Schema conformance"
  [ testCase "every cell key is a column key" $ withView $ \v -> do
      cols <- columnKeysOf v
      rows <- listAt "rows" v
      -- Over no rows the claim below is met by saying nothing, so the fixture's
      -- own count is what makes it one.
      assertEqual "the fixture's rows" 6 (length rows)
      mapM_ (\r -> do
                ks <- field "cells" r >>= keysOf
                assertBool (show ks <> " outside " <> show cols)
                           (all (`elem` cols) ks))
            rows

  , testCase "every row has an id" $ withView $ \v -> do
      ids <- each "rows" "id" v >>= mapM text
      assertEqual "the fixture's rows" 6 (length ids)
      assertBool ("blank id in " <> show ids) (not (any T.null ids))

  , testCase "the badge column carries a palette" $ withView $ \v -> do
      state <- columnOf "state" v
      kind <- field "type" state >>= text
      badges <- listAt "badges" state
      assertEqual "type" "badge" kind
      assertBool "badges are empty" (not (null badges))

  , testCase "and the two group values a filter can name" $ withView $ \v -> do
      -- Vocabulary rather than cell text: no row's state cell holds either, so
      -- they travel as `values' beside the badges, and a renderer completing
      -- the column offers the keywords and these.
      state <- columnOf "state" v
      values <- listAt "values" state >>= mapM text
      assertEqual "values" ["*active*", "*inactive*"] values

  , testCase "the multi-valued column says so, and it is the only one"
      $ withView $ \v -> do
      -- Declared rather than sampled: the renderer decides arity from up to 40
      -- non-empty cells, and a page with fewer than two tagged rows finds no
      -- multi-valued column at all — where `tag:a tag:b' would OR while this
      -- producer ANDs.  The declaration is what settles it.
      cols <- listAt "columns" v
      keys <- mapM (textAt "key") cols
      multi <- mapM (maybeBoolAt "multi") cols
      assertEqual "the columns declaring multi" ["tag"]
                  [ k | (k, Just True) <- zip keys multi ]

  , testCase "the sort column is one of the columns" $ withView $ \v -> do
      cols <- columnKeysOf v
      key <- field "sort" v >>= field "column" >>= text
      assertBool (show key <> " outside " <> show cols) (key `elem` cols)

  , testCase "the actions are SCHEMA.md's key/command/label objects" $ withView $ \v -> do
      keys <- each "actions" "key" v >>= mapM text
      commands <- each "actions" "command" v >>= mapM text
      labels <- each "actions" "label" v >>= mapM text
      fields <- listAt "actions" v >>= mapM keysOf
      assertEqual "keys" ["RET"] keys
      assertEqual "commands" ["materialize"] commands
      assertEqual "labels" ["Materialize"] labels
      assertEqual "fields" [["command", "key", "label"]] (map sort fields)
  ]

-- Commands
--
-- The span math the structured commands run on.  It lives in the facade
-- because 'Data.Org.HeadlineSpans' does not leave the private sublibrary, and
-- it is asserted here for the same reason: this module imports no parser
-- internals, so what these cases see is what the daemon sees.
--
-- Every case splices the edits itself rather than through
-- 'Data.Org.Edit.applyEdits' — an oracle that shares the engine would agree
-- with a wrong offset — and then asserts the WHOLE document, so the bytes
-- around the edit are checked by the same assertion as the edit.

-- | DOC with EDITS applied, right to left so an earlier offset is never moved
-- by a later splice.  The suite's own splice: three lines, no engine.
splice :: Text -> [(Span, Text)] -> Text
splice = foldl' one
  where one doc (Span s e, new) = T.take s doc <> new <> T.drop e doc

-- | Run K over the one record DOC parses to, written into a file of its own so
-- the load path is the ordinary one.
withRecord :: Text -> (HeadlineRecord -> Assertion) -> Assertion
withRecord doc k = withTempDirNamed "command" $ \dir -> do
  path <- orgFile dir "one.org" doc
  loadFile path >>= either (assertFailure . show) one
  where one [r] = k r
        one rs  = assertFailure ("expected one headline, got " <> show (length rs))

-- | WHAT: DOC with @set-state KEYWORD@ applied to its one headline is WANTED.
setStateIs :: String -> Text -> Maybe Text -> Text -> Assertion
setStateIs what doc keyword wanted = withRecord doc $ \r ->
  case setStateEdits keyword r of
    Left why    -> assertFailure (what <> ": refused: " <> T.unpack why)
    Right edits -> assertEqual what wanted (splice doc edits)

-- | WHAT: DOC with @archive@ applied to its one headline is WANTED.
archiveIs :: String -> Text -> Text -> Assertion
archiveIs what doc wanted =
  withRecord doc (assertEqual what wanted . splice doc . archiveEdits)

-- | A document declaring keywords past org's own two, so the legality check
-- has something to be right about.
keyworded :: Text -> Text
keyworded rest = "#+TODO: NEXT WAITING | CANCELLED\n" <> rest

commandSpec :: TestTree
commandSpec = testGroup "Commands"
  [ testGroup "set-state"
    [ testCase "over a keyword, replaces exactly that word" $
        setStateIs "replaced" (keyworded "* NEXT [#A] Ship it :web:\n") (Just "WAITING")
                              (keyworded "* WAITING [#A] Ship it :web:\n")

      -- The insertion point is the stars', which is the one offset a headline
      -- always has: a priority, a title and tags are each optional.
    , testCase "with no keyword, inserts one right after the stars" $
        setStateIs "inserted" "* [#B] Plain :tag:\n" (Just "TODO")
                              "* TODO [#B] Plain :tag:\n"

    , testCase "into a headline that is stars and nothing else" $
        setStateIs "bare" "*\n" (Just "TODO") "* TODO\n"

      -- The space behind the keyword goes with it, so the title closes up
      -- rather than starting a column late.
    , testCase "a null keyword takes the word and the space behind it" $
        setStateIs "cleared" (keyworded "* NEXT Ship it :web:\n") Nothing
                             (keyworded "* Ship it :web:\n")

    , testCase "and the whole run of it, however wide" $
        setStateIs "cleared wide" (keyworded "*   NEXT   Ship it\n") Nothing
                                  (keyworded "*   Ship it\n")

      -- Horizontal only: a keyword at the end of its line keeps the newline
      -- that ends it, or the headline would swallow the line below.
    , testCase "a keyword ending its line keeps the newline" $
        setStateIs "cleared at eol" (keyworded "* NEXT\n* NEXT Second\n") Nothing
                                    (keyworded "* \n* NEXT Second\n")

    , testCase "clearing a headline that has no keyword costs no edit" $
        withRecord "* Plain\n" $ \r ->
          assertEqual "no edits" (Right []) (setStateEdits Nothing r)

      -- Per file, because `#+TODO:' is: the same word is a keyword in one
      -- document and the first word of a title in the next.
    , testCase "a keyword the file does not declare is refused, by name" $
        withRecord "* TODO Plain\n" $ \r ->
          case setStateEdits (Just "WAITING") r of
            Right edits -> assertFailure ("expected a refusal, got " <> show edits)
            Left why -> do
              assertBool ("names the keyword: " <> T.unpack why) ("WAITING" `T.isInfixOf` why)
              assertBool ("names what is declared: " <> T.unpack why)
                         ("TODO" `T.isInfixOf` why && "DONE" `T.isInfixOf` why)

    , testCase "the same keyword is legal once the file declares it" $
        setStateIs "declared" (keyworded "* TODO Plain\n") (Just "WAITING")
                              (keyworded "* WAITING Plain\n")

      -- The state column ships these two as filter vocabulary beside its
      -- badges.  No file declares one, so no file can be put into one.
    , testCase "the state column's group meta-values are not keywords" $
        withRecord (keyworded "* NEXT Plain\n") $ \r ->
          mapM_ (\meta -> case setStateEdits (Just meta) r of
                   Right edits -> assertFailure (T.unpack meta <> ": " <> show edits)
                   Left _why   -> pure ())
                ["*active*", "*inactive*", "active"]
    ]

  , testGroup "archive"
    [ testCase "goes inside the tag list, ahead of its closing colon" $
        archiveIs "tagged" "* TODO Ship it :web:glance:\n"
                           "* TODO Ship it :web:glance:ARCHIVE:\n"

    , testCase "with no tags, is appended to the title line" $
        archiveIs "untagged" "* TODO Ship it\n" "* TODO Ship it :ARCHIVE:\n"

      -- `hsFull' ends at the LAST part in span order, which here is a timestamp
      -- on the next line and a drawer two lines below that.  Appending there
      -- would put the tag inside the drawer.
    , testCase "past a planning line and a drawer, still on the title line" $
        archiveIs "planned" (T.unlines
                    [ "* TODO Ship it"
                    , "SCHEDULED: <2026-08-01 Sat>"
                    , ":PROPERTIES:"
                    , ":ORG_GLANCE_ID: ship"
                    , ":END:" ])
                  (T.unlines
                    [ "* TODO Ship it :ARCHIVE:"
                    , "SCHEDULED: <2026-08-01 Sat>"
                    , ":PROPERTIES:"
                    , ":ORG_GLANCE_ID: ship"
                    , ":END:" ])

    , testCase "onto a headline with no title either" $
        archiveIs "titleless" "* TODO\n" "* TODO :ARCHIVE:\n"

    , testCase "a row already carrying the tag costs no edit" $
        withRecord "* TODO Ship it :web:ARCHIVE:\n" $ \r -> do
          assertBool "reads as archived" (archived r)
          assertEqual "no edits" [] (archiveEdits r)

      -- The tag is matched the way the filter matches one, which folds case.
    , testCase "however the file spells the tag" $
        withRecord "* TODO Ship it :archive:\n" $ \r ->
          assertEqual "no edits" [] (archiveEdits r)

    , testCase "and an untagged row does not read as archived" $
        withRecord "* TODO Ship it :web:\n"
                   (assertBool "not archived" . not . archived)
    ]
  ]
