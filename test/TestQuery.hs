-- | The facade under test.  Everything here goes through 'Glance.Query': the
-- module imports no parser internals, so a wire shape that needs one fails to
-- compile instead of failing a renderer.
module TestQuery (spec) where

import Data.Aeson (Value (Bool, Object, String), eitherDecodeFileStrict')
import Data.Char (isDigit)
import Data.List (sort)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults (columnKeysOf, columnOf, field, listAt, textAt, viewDir)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T

import Glance.Query ( HeadlineRecord (..), QueryResult (..), displayText, loadDir
                    , matchesSearch, viewJSON )

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
spec = testGroup "Query" [loadSpec, cellSpec, searchSpec, viewSpec, schemaSpec]

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
