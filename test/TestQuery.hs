-- | The facade under test: everything goes through 'Glance.Query', so a wire
-- shape needing a parser internal fails to compile.
module TestQuery (spec) where

import Control.Concurrent (rtsSupportsBoundThreads)
import Control.Monad (forM_, replicateM, (<=<))
import Data.Aeson (Value (Bool, Object, String), eitherDecodeFileStrict', object, (.=))
import Data.Either (fromRight, isRight)
import Data.List (foldl', nub, sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Posix.Files (createSymbolicLink)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults ( assertContains, columnKeysOf, columnOf, dateCorpus, dateCorpusPath
                    , entryAs, field, listAt
                    , orgFile, refusedNaming, textAt, viewDir, withDoc
                    , withTempDirNamed )

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Time as Time

import Glance.Query ( ConfigLayerFile (..), ConfigLayers (..), HeadlineParts (..)
                    , refTarget
                    , HeadlineRecord (..)
                    , LinkShape (..), LoadFailure (..), OrgLink (..)
                    , QueryResult (..), Span (..), SubtreeEntry (..)
                    , TodoKeywords (..), addTagEdits
                    , archiveEdits, archived, shiftRepeat
                    , BlobSeed (..), blobDocument, blobPathIn
                    , captureCodes, captureEdits, captureStamp, captureTemplateEdits
                    , captureTemplateIn, captureTemplateOf
                    , dayOf, isoDay, monthWords, shiftDay, shiftIn
                    , defaultWalk, derivedPath, documentPath
                    , displayText, editLinkEdits, expandTemplate
                    , headlineParts, hiddenProperties
                    , keywordSources, loadDir
                    , loadDirFilesSerially, loadDirFilesWith, matchesSearch
                    , mintBlobId, noConfig, orgLinks
                    , planningTimestamp, readsAsTimestamp, recomposedSubtree
                    , linkColumns, linkShown, linkType, removeTagEdits, renameTagEdits
                    , priorityText, rowJSON, setPlanningEdits, setPriorityEdits
                    , setStateEdits, setTitleEdits
                    , settableStates, sortedForView, sortedForViewWith, sortedTagsCell
                    , storeRootIn
                    , subtreeEntries, subtreeEntryAt, subtreeLinks, subtreeText
                    , tagText, tagged, templatePrompts, titleText, uuidFrom, viewJSON )

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

-- | Run K over the records DOC alone makes.
withRecordsOf :: Text -> ([HeadlineRecord] -> Assertion) -> Assertion
withRecordsOf = withDoc "view" "tree.org"

-- | Run K over the view DOC alone makes.
withViewOf :: Text -> (Value -> Assertion) -> Assertion
withViewOf doc k = withRecordsOf doc (k . viewJSON viewTitle)

-- | An outline with a level at every depth; two rows come out of it.
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

boolOf :: Value -> IO Bool
boolOf (Bool b) = pure b
boolOf v = assertFailure ("expected a boolean, got " <> show v)

-- | L as the DISPLAY rule's pair: where it points and what it shows.
linkPair :: OrgLink -> (Text, Text)
linkPair l = (olTarget l, linkShown l)

-- | The links TEXT holds, as those pairs.
shown :: Text -> [(Text, Text)]
shown = map linkPair . orgLinks

-- | The links TEXT holds, each cut back out of TEXT by its own span.
spelled :: Text -> [Text]
spelled text' = [ cut text' (olSpan l) | l <- orgLinks text' ]

-- | The half-open char span SP of TEXT.
cut :: Text -> Span -> Text
cut text' sp = T.take (spanEnd sp - spanStart sp) (T.drop (spanStart sp) text')

-- | The value at KEY of every element of the array at ARR of V.
each :: Text -> Text -> Value -> IO [Value]
each arr k v = listAt arr v >>= mapM (field k)

-- | KEY of V as a boolean, 'Nothing' where V does not carry it.
maybeBoolAt :: Text -> Value -> IO (Maybe Bool)
maybeBoolAt key (Object o) = case KM.lookup (Key.fromText key) o of
  Nothing       -> pure Nothing
  Just (Bool b) -> pure (Just b)
  Just other    -> assertFailure ("expected a boolean at " <> show key
                                    <> ", got " <> show other)
maybeBoolAt key v = assertFailure ("expected an object with " <> show key
                                     <> ", got " <> show v)

spec :: TestTree
spec = testGroup "Query"
  [ loadSpec, walkSpec, levelSpec, blankSpec, parallelSpec, cellSpec, searchSpec
  , linkSpec
  , viewSpec, schemaSpec, commandSpec, lensSpec, entrySpec, captureSpec
  , repeatSpec ]

-- | ORG'S REPEATER, shifted.  The shape is TEXTUAL: only the dates move.
repeatSpec :: TestTree
repeatSpec = testGroup "Repeats"
  [ testCase "the three cookies differ in where the count starts" $ do
      shifts "<2026-08-08 Sat +1w>" "<2026-08-15 Sat +1w>"
      shifts "<2026-07-11 Sat +1w>" "<2026-07-18 Sat +1w>"
      shifts "<2026-07-11 Sat ++1w>" "<2026-08-15 Sat ++1w>"
      shifts "<2026-07-11 Sat .+1w>" "<2026-08-15 Sat .+1w>"

  , testCase "the weekday is recomputed and everything else rides through" $ do
      shifts "<2026-08-08 Sat 09:30 +2d -3d>" "<2026-08-10 Mon 09:30 +2d -3d>"
      shifts "[2026-08-08 Sat +1d]" "[2026-08-09 Sun +1d]"

    -- ORG DOES NOT PAD, so a fixed-width date window eats the space behind the day.
  , testCase "a date org did not pad still moves, weekday and all" $ do
      shifts "<2026-08-8 Sat +2d>" "<2026-08-10 Mon +2d>"
      shifts "<2026-8-8 Sat +2d>" "<2026-08-10 Mon +2d>"

  , testCase "a range moves both halves" $
      shifts "<2026-08-08 Sat +1w>--<2026-08-09 Sun>"
             "<2026-08-15 Sat +1w>--<2026-08-16 Sun>"

  , testCase "the month cookie clips rather than overflowing" $
      shifts "<2026-01-31 Sat +1m>" "<2026-02-28 Sat +1m>"

  , testCase "an entry with no cookie does not repeat" $ do
      assertEqual "a plain stamp" Nothing (shiftRepeat probeToday "<2026-08-08 Sat>")
      assertEqual "a warning cookie is no repeater" Nothing
                  (shiftRepeat probeToday "<2026-08-08 Sat -3d>")
      assertEqual "and text that is no timestamp at all" Nothing
                  (shiftRepeat probeToday "next tuesday")
  ]
  where
    shifts from to = assertEqual (T.unpack from) (Just to) (shiftRepeat probeToday from)

-- | The day every repeat case is worked out against.
probeToday :: Time.Day
probeToday = Time.fromGregorian 2026 8 8

-- | Where a row points: the pure function under @GET \/links@, on the DISPLAY rule.
linkSpec :: TestTree
linkSpec = testGroup "Links" $
  [ testCase "a bracket link is its target and what it shows" $ do
      assertEqual "described" [("https://x/y", "table-view")]
                  (shown "[[https://x/y][table-view]]")
      assertEqual "bare" [("https://x/y", "https://x/y")] (shown "[[https://x/y]]")
      assertEqual "empty description" [("file:a.org", "file:a.org")]
                  (shown "[[file:a.org][]]")

  , testCase "the description is what displayText would show" $ do
      let one = "[[https://x/y][table-view]]"
      assertEqual "one parser, two questions" (displayText one) (snd (head (shown one)))

  , testCase "several links come back in the order they are written" $
      assertEqual "in order" [("file:R.md", "readme"), ("file:N.org", "notes")]
                  (shown "see [[file:R.md][readme]] and [[file:N.org][notes]].")

  , testCase "a bare URL is its own description" $ do
      assertEqual "https" [("https://x.org/a", "https://x.org/a")]
                  (shown "read https://x.org/a today")
      assertEqual "http" [("http://x.org", "http://x.org")] (shown "http://x.org")
      assertEqual "mailto" [("mailto:t@x.org", "mailto:t@x.org")]
                  (shown "write to mailto:t@x.org")

  , testCase "and the punctuation a sentence leaves behind is not part of it" $ do
      assertEqual "full stop" [("https://x.org/a", "https://x.org/a")]
                  (shown "see https://x.org/a.")
      assertEqual "parens" [("https://x.org/a", "https://x.org/a")]
                  (shown "(https://x.org/a)")
      assertEqual "angles" [("https://x.org/a", "https://x.org/a")]
                  (shown "<https://x.org/a>")

  , testCase "a scheme inside a word is not a link" $ do
      assertEqual "glued" [] (shown "xhttps://x.org")
      assertEqual "no scheme at all" [] (shown "x.org and ftp://x.org")

  , testCase "a bracket link's own target is not also a bare URL" $
      assertEqual "counted once" [("https://x.org/a", "the page")]
                  (shown "[[https://x.org/a][the page]]")

  , testCase "text that never closes a link holds no link" $ do
      assertEqual "unclosed" [] (shown "[[oops")
      assertEqual "not a link" [] (shown "[[a]x]")
      assertEqual "and the one after it survives"
                  [("https://x.org", "https://x.org")] (shown "[[oops https://x.org")

  , testCase "a row with nothing to follow has no links" $
      assertEqual "none" [] (shown "* TODO plain headline\nwith a body\n")

    -- EVERY LINK CARRIES ITS SPAN, asserted by cutting the link back out of the text.
  , testCase "a link spans exactly the characters that spell it" $ do
      let text = "see [[file:R.md][readme]] and https://x.org/a."
      assertEqual "the bracket link whole, and the bare URL without the full stop"
        ["[[file:R.md][readme]]", "https://x.org/a"] (spelled text)
      assertEqual "the spans are where the words are"
        [Span 4 25, Span 30 45] (map olSpan (orgLinks text))

  , testCase "the shape a link is spelled in comes back with it" $
      assertEqual "bracketed with a description, bracketed without, and bare"
        [Bracketed (Just "readme"), Bracketed Nothing, Bracketed (Just ""), Bare]
        (map olShape (orgLinks
           "[[file:a][readme]] [[file:b]] [[file:c][]] https://x.org"))

    -- The dedup key is the pair a reader can SEE; the target alone swallowed spellings.
  , testCase "one entry per (target, shown) pair, keeping the first spelling" $ do
      assertEqual "distinct descriptions are distinct entries"
        [("https://x.org", "first"), ("https://x.org", "second")]
        (shown "[[https://x.org][first]] and [[https://x.org][second]]")
      assertEqual "the same description dedups to the first span"
        ["[[https://x.org][x]]"]
        (spelled "[[https://x.org][x]] and [[https://x.org][x]]")

  , testCase "one command under two descriptions serves both (the elisp pair)" $ do
      let cmd = "elisp:(browse-url (string-trim (shell-command-to-string \"~/t/cluster-url alpha grafana\")))"
          text = "[[" <> cmd <> "][pnl]]\n[[" <> cmd <> "][alpha:grafana]]"
      assertEqual "both spellings listed, in order"
        [(cmd, "pnl"), (cmd, "alpha:grafana")] (shown text)

  , testCase "the subtree is what is read, body and children included" $
      withRecordsOf (T.unlines
        [ "* parent [[https://a.example][A]]"
        , "body [[https://b.example][B]]"
        , "** child https://c.example" ]) $ \recs ->
        assertEqual "the whole extent"
          [[ ("https://a.example", "A"), ("https://b.example", "B")
           , ("https://c.example", "https://c.example") ]]
          (map (map linkPair . subtreeLinks) recs)

    -- Spans are shifted by where the subtree slice starts, so 'Data.Org.Edit' can splice.
  , testCase "a row's link spans are offsets into the document it was read from" $
      withRecordsOf (T.unlines
        [ "* first", "nothing here", "* second [[https://a.example][A]]"
        , "body https://b.example" ]) $ \recs ->
        assertEqual "each cut out of the file itself"
          [[], ["[[https://a.example][A]]", "https://b.example"]]
          [ [ cut (hrDoc r) (olSpan l) | l <- subtreeLinks r ] | r <- recs ]

    -- A URL is no reference, which keeps 'hrLinks' small enough for every record.
  , testCase "a row's links are the references its subtree carries" $
      withRecordsOf (T.unlines
        [ "* parent [[org-glance-visit:alpha][A]]"
        , "body https://b.example and [[org-glance-overview:tag][a tag]]"
        , "** child [[org-glance-open:beta][B]]" ]) $ \recs ->
        assertEqual "the references alone" [["alpha", "beta"]]
                    (map (map refTarget . hrLinks) recs)

    -- 'hrLinked' is the WIDER question: is there anywhere to go, which @o@ follows.
  , testCase "a row whose only link is a URL is linked and references nothing" $
      withRecordsOf "* plain\nsee https://x.example for the rest\n" $ \recs -> do
        assertEqual "linked" [True] (map hrLinked recs)
        assertEqual "and pointing at no row" [[]] (map hrLinks recs)

  , testCase "a reference is a link too, so a referencing row is linked" $
      withRecordsOf "* plain\n[[org-glance-visit:alpha][A]]\n" $ \recs ->
        assertEqual "both" [(True, ["alpha"])]
                    (map (\r -> (hrLinked r, map refTarget (hrLinks r))) recs)

  , testCase "and a row with nothing to follow is not linked" $
      withRecordsOf "* plain\njust prose, no link in it\n" $ \recs ->
        assertEqual "nowhere to go" [False] (map hrLinked recs)
  ] ++
  [ testCase name $ sequence_ [ assertEqual msg want (map linkType targets)
                              | (msg, targets, want) <- checks ]
  | (name, checks) <- linkTypeCases ] ++
    -- A badge naming a type nothing produces would be a colour no cell ever wears.
  [ testCase "every declared badge value is a type linkType can produce" $ do
      declared <- traverse (textAt "value")
              =<< listAt "badges" =<< columnOf "type" (object ["columns" .= linkColumns])
      assertEqual "the six the corpus spells"
        ["https", "http", "glance", "mailto", "id", "file"] declared
      assertEqual "and each is what its own scheme derives to" declared
        (map (\t -> linkType (if t == "glance" then "org-glance-visit:x" else t <> ":x"))
             declared)
  ]

-- | Each case's name, then every assertion it makes: the message, the targets,
-- and the types 'linkType' derives from them.
linkTypeCases :: [(String, [(String, [Text], [Text])])]
linkTypeCases =
    -- The type is the target's PREFIX folded, the @org-glance-@ family one word.
  [ ( "a link's type is its scheme, folded"
    , [ ( "the six the corpus spells"
        , [ "https://x.example/a", "http://x.example", "mailto:t@x.org", "id:E1B2"
          , "file:notes.org" ]
        , ["https", "http", "mailto", "id", "file"] )
      , ( "and every org-glance protocol is one word"
        , [ "org-glance-visit:E1", "org-glance-open:E1", "org-glance-material:E1"
          , "org-glance-overview:book" ]
        , ["glance", "glance", "glance", "glance"] ) ] )
  , ( "a scheme this has never seen travels under its own name"
    , [ ( "the word itself", ["ftp://x.example", "doi:10.1/2", "gopher://x", "denote:2026"]
        , ["ftp", "doi", "gopher", "denote"] ) ] )
  , ( "the scheme is folded, so a shouted URL is still followable"
    , [ ("lowercased", ["HTTPS://X.EXAMPLE", "MailTo:t@x.org"], ["https", "mailto"]) ] )
    -- One catch-all: no colon at all, and a word before it that is not scheme-SHAPED.
  , ( "a target with no scheme is other, internal links included"
    , [ ( "nothing to read"
        , [ "Some Headline", "*Some Headline", "./notes.org", "/etc/hosts", "2026:review"
          , ":leading", "a b:c", "" ]
        , replicate 8 "other" ) ] )
    -- A registry of known schemes is deliberately not the rule: an unlisted one would read as prose.
  , ( "and a scheme-shaped word in prose is taken at its word"
    , [ ("the word before the colon", ["Meeting: notes", "TODO:tomorrow"], ["meeting", "todo"]) ] )
  ]

-- | The subtree lens: every byte of a subtree has one owner, so the assertions
-- are about bytes.  The hidden properties and the logbook are the SERVER's, and
-- the cases are generic over 'hiddenProperties'.
lensSpec :: TestTree
lensSpec = testGroup "Subtree lens"
  [ testGroup "decompose"
    [ testCase "a drawer leaves the body and comes back as pairs" $
        withParts drawered $ \r -> do
          assertEqual "the body is the subtree without the headline's drawer lines"
                      (T.unlines [ "* TODO First :one:", "body line", "** Child"
                                 , ":PROPERTIES:", ":ORG_GLANCE_ID: kid", ":END:"
                                 , "child body" ])
                      (hpBody (headlineParts r))
          assertEqual "the pairs, in file order, the server's own left out"
                      [("EFFORT", "0:30")]
                      (hpProperties (headlineParts r))

    , testCase "a headline with no drawer is its whole subtree and no pairs" $
        withParts (T.unlines ["* TODO Bare", "body line"]) $ \r -> do
          assertEqual "the body is the subtree" (subtreeText r) (hpBody (headlineParts r))
          assertEqual "and there is nothing to show" [] (hpProperties (headlineParts r))

      -- The identity property is the row id, so the server keeps it back and writes it in itself.
    , testCase "a hidden property is in neither pane, whatever the file says" $
        withParts drawered $ \r -> do
          let parts = headlineParts r
          assertEqual "no hidden key is offered" []
            [ key | (key, _v) <- hpProperties parts, key `elem` hiddenProperties ]
          assertBool "and its line is in no pane either"
                     (not (":ORG_GLANCE_ID: first" `T.isInfixOf` hpBody parts))

    , testCase "the planning line is its own region, out of the body" $
        withParts planned $ \r -> do
          assertEqual "body"
                      (T.unlines ["* TODO Timed", "after"])
                      (hpBody (headlineParts r))
          assertEqual "and the entries, in the order the line writes them"
                      [ ("SCHEDULED", "<2026-08-01 Sat 09:30>")
                      , ("DEADLINE", "<2026-08-05 Wed>") ]
                      (hpPlanning (headlineParts r))

    , testCase "a headline with no planning has no planning entries" $
        withParts drawered $ \r ->
          assertEqual "none" [] (hpPlanning (headlineParts r))

      -- The logbook is located textually: the LOGBOOK drawer past the title, ahead of the first child.
    , testCase "the logbook is a region of its own, verbatim" $
        withParts logged $ \r -> do
          let parts = headlineParts r
          assertEqual "the drawer, whole"
                      ":LOGBOOK:\nCLOCK: [2026-08-01 Sat 09:00]--[2026-08-01 Sat 09:30]\n:END:\n"
                      (hpLogbook parts)
          assertBool "and out of the body"
                     (not ("CLOCK:" `T.isInfixOf` hpBody parts))
          assertEqual "and no part of the properties" [("EFFORT", "0:30")]
                      (hpProperties parts)

    , testCase "a child's logbook is the child's, and stays body text" $
        withParts childLogged $ \r -> do
          let parts = headlineParts r
          assertEqual "this headline has none" "" (hpLogbook parts)
          assertContains "the child keeps its own" ":LOGBOOK:\nCLOCK: kid\n:END:\n"
                         (hpBody parts)

      -- The lens is over ONE headline: a child's drawer is body text here.
    , testCase "a child's drawer stays in the body untouched" $
        withParts drawered $ \r -> do
          let parts = headlineParts r
          assertContains "the child keeps its own drawer, whole"
                         ":PROPERTIES:\n:ORG_GLANCE_ID: kid\n:END:\n" (hpBody parts)
          assertEqual "and it is no part of this headline's pairs"
                      ["EFFORT"] (map fst (hpProperties parts))

    , testCase "unicode is cut by characters, not bytes" $
        withParts unicoded $ \r -> do
          assertEqual "the body keeps its text"
                      (T.unlines ["* TODO Привет мир :unicode:", "тело письма"])
                      (hpBody (headlineParts r))
          assertEqual "and the value is the file's"
                      [("CATEGORY", "письма")]
                      (hpProperties (headlineParts r))

    , testCase "odd spacing is stripped out of the pairs and left in the file" $
        withParts oddly $ \r ->
          assertEqual "the pairs as a panel would show them"
                      [("A", "one"), ("B", ""), ("C", "three")]
                      (hpProperties (headlineParts r))
    ]

  , testGroup "recompose"
      -- BYTE FOR BYTE UP TO THE LINE END, which is all a write spells differently.
    [ testCase "decompose then recompose is the subtree, its line ends trimmed" $
        mapM_ roundTrips [ drawered, planned, unicoded, oddly, indented, crlf
                         , logged, childLogged, permuted, stamped
                         , T.unlines ["* TODO Bare", "body"]
                         , "* Ends at the drawer\n:PROPERTIES:\n:A: 1\n:END:" ]

      -- The CRLF trap: a strip taking the carriage return spells a file's endings two ways.
    , testCase "no line a recompose writes ends in horizontal space" $
        mapM_ trimsEveryLine
          [ "* TODO Trails   \n:PROPERTIES:\n:A: one \t\n:END:\nbody \nmore\t\n"
          , "* TODO Trails  \r\n:PROPERTIES:\r\n:A: one \r\n:END:\r\nbody \r\n"
          , logged ]

      -- Horizontal space INSIDE a line is content and is not reachable from the end.
    , testCase "horizontal space inside a line is left alone" $
        withParts (T.unlines ["* TODO Kept", "| a | b |", "#+begin_src", "    indented", "#+end_src"]) $ \r ->
          assertEqual "the interior spacing is the file's"
                      (subtreeText r) (recomposedSubtree r (headlineParts r))

    , testCase "a permuted planning line comes back in its own order" $
        withParts permuted $ \r ->
          assertContains "the file's own order"
                         "CLOSED: [2026-07-30 Thu] SCHEDULED: <2026-08-01 Sat>"
                         (recomposedSubtree r (headlineParts r))

    , testCase "a property nobody touched keeps its own line, odd spacing and all" $
        withParts oddly $ \r -> do
          let parts = headlineParts r
              back = recomposedSubtree r parts
          assertContains "the crooked line is the file's own" ":A:one" back
          assertContains "and the empty one too" ":B:\n" back
          assertContains "and the padded one keeps the pad it opens with"
                         ":C:   three\n" back
          assertBool "and loses the one it closed with"
                     (not (":C:   three   " `T.isInfixOf` back))

    , testCase "an edited property is rendered canonically, under the drawer's indent" $
        withParts indented $ \r -> do
          let parts = headlineParts r
              back = recomposedSubtree r parts { hpProperties = [("A", "moved"), ("B", "2")] }
          assertContains "the edited one is canonical, indented like its neighbours"
                         "  :A: moved\n" back
          assertContains "the untouched one is verbatim" "  :B:  2\n" back

    , testCase "an added property joins the drawer where the client put it" $
        withParts drawered $ \r -> do
          let parts = headlineParts r
              back = recomposedSubtree r parts { hpProperties = hpProperties parts <> [("ADDED", "yes")] }
          assertEqual "the drawer, in order"
                      [":PROPERTIES:", ":ORG_GLANCE_ID: first", ":EFFORT: 0:30"
                      , ":ADDED: yes", ":END:"]
                      (drawerOf back)

    , testCase "a dropped property is simply not written" $
        withParts drawered $ \r -> do
          let back = recomposedSubtree r (headlineParts r) { hpProperties = [] }
          assertEqual "the server's own line is what is left"
                      [":PROPERTIES:", ":ORG_GLANCE_ID: first", ":END:"] (drawerOf back)

      -- A hidden property is the server's, so an empty list empties the client's half alone.
    , testCase "a hidden property survives a sync that never mentioned it" $
        withParts drawered $ \r -> do
          let back = recomposedSubtree r (headlineParts r) { hpProperties = [] }
          assertContains "verbatim" ":ORG_GLANCE_ID: first\n" back
          assertBool "and the edited half is gone"
                     (not (":EFFORT:" `T.isInfixOf` back))

      -- More than one entry, so "hidden" is the LIST rather than one key's case.
    , testCase "every hidden key survives, at the line it sat on" $
        withParts stamped $ \r -> do
          let parts = headlineParts r
          assertEqual "neither is offered" [] [ k | (k, _v) <- hpProperties parts
                                                  , k `elem` hiddenProperties ]
          assertEqual "and both are woven back where they were"
                      [ ":PROPERTIES:", ":ORG_GLANCE_ID: kept"
                      , ":ORG_GLANCE_CREATION_TIME: [2026-08-01 Sat 09:30]", ":END:" ]
                      (drawerOf (recomposedSubtree r parts { hpProperties = [] }))

    , testCase "a client naming a hidden key does not move it" $
        withParts drawered $ \r -> do
          let back = recomposedSubtree r (headlineParts r)
                       { hpProperties = [("ORG_GLANCE_ID", "hijacked")] }
          assertContains "the file's own value stands" ":ORG_GLANCE_ID: first\n" back
          assertBool "and the client's is nowhere"
                     (not ("hijacked" `T.isInfixOf` back))

    , testCase "an empty list takes the drawer away when nothing is hidden" $
        withParts oddly $ \r -> do
          let parts = headlineParts r
              back = recomposedSubtree r parts { hpProperties = [] }
          assertEqual "the body alone" (hpBody parts) back
          assertBool "and the drawer is gone with it"
                     (not (":PROPERTIES:" `T.isInfixOf` back))

    , testCase "a drawer for a headline that never had one goes after the title line" $
        withParts (T.unlines ["* TODO Bare", "body line"]) $ \r ->
          assertEqual "written where org writes one"
                      (T.unlines [ "* TODO Bare", ":PROPERTIES:", ":NEW: 1", ":END:"
                                 , "body line" ])
                      (recomposedSubtree r (headlineParts r) { hpProperties = [("NEW", "1")] })

    , testCase "and after the planning line when there is one" $
        withParts (T.unlines ["* TODO Timed", "SCHEDULED: <2026-08-01 Sat 09:30>", "after"]) $ \r ->
          assertEqual "the planning line keeps its place"
                      (T.unlines [ "* TODO Timed", "SCHEDULED: <2026-08-01 Sat 09:30>"
                                 , ":PROPERTIES:", ":NEW: 1", ":END:", "after" ])
                      (recomposedSubtree r (headlineParts r) { hpProperties = [("NEW", "1")] })

    , testCase "an edit further down the body leaves the drawer where it was" $
        withParts drawered $ \r -> do
          let parts = headlineParts r
              back = recomposedSubtree r parts { hpBody = hpBody parts <> "one more line\n" }
          assertEqual "the drawer still opens the line under the headline"
                      ":PROPERTIES:" (T.lines back !! 1)
          assertContains "and the addition landed" "one more line\n" back

    , testCase "a body shorter than the drawer's line takes it at the end" $
        withParts oddly $ \r ->
          assertEqual "appended, and terminated"
                      "* only\n:PROPERTIES:\n:A: 1\n:END:\n"
                      (recomposedSubtree r (headlineParts r)
                         { hpBody = "* only", hpProperties = [("A", "1")] })
    ]

  , testGroup "planning"
    [ testCase "an untouched entry keeps its own text, where it was" $
        withParts planned $ \r -> do
          let parts = headlineParts r
              back  = recomposedSubtree r parts
          assertEqual "the line, as the file wrote it"
                      "SCHEDULED: <2026-08-01 Sat 09:30> DEADLINE: <2026-08-05 Wed>"
                      (T.lines back !! 1)

    , testCase "an edited entry is canonical and the untouched one is not" $
        withParts planned $ \r -> do
          let back = recomposedSubtree r (headlineParts r)
                       { hpPlanning = [ ("DEADLINE", "<2026-08-05 Wed>")
                                      , ("SCHEDULED", "<2026-09-09 Wed>") ] }
          assertEqual "untouched first, in its own place; the edit rendered"
                      "DEADLINE: <2026-08-05 Wed> SCHEDULED: <2026-09-09 Wed>"
                      (T.lines back !! 1)

    , testCase "an entry added to a headline that had none opens the line" $
        withParts (T.unlines ["* TODO Bare", "body line"]) $ \r ->
          assertEqual "written where org writes one"
                      (T.unlines [ "* TODO Bare", "DEADLINE: <2026-08-05 Wed>", "body line" ])
                      (recomposedSubtree r (headlineParts r)
                         { hpPlanning = [("DEADLINE", "<2026-08-05 Wed>")] })

    , testCase "an added entry lands in org's order behind the ones already there" $
        withParts planned $ \r -> do
          let parts = headlineParts r
              back  = recomposedSubtree r parts
                        { hpPlanning = hpPlanning parts <> [("CLOSED", "[2026-08-06 Thu]")] }
          assertEqual "appended, rendered"
                      ("SCHEDULED: <2026-08-01 Sat 09:30> DEADLINE: <2026-08-05 Wed>"
                         <> " CLOSED: [2026-08-06 Thu]")
                      (T.lines back !! 1)

    , testCase "clearing every entry takes the line with it" $
        withParts planned $ \r -> do
          let back = recomposedSubtree r (headlineParts r) { hpPlanning = [] }
          assertBool "no planning line is left"
                     (not ("SCHEDULED:" `T.isInfixOf` back))
          assertEqual "and the drawer moved up under the title"
                      ":PROPERTIES:" (T.lines back !! 1)

    , testCase "a planning line added beside a new drawer takes the line above it" $
        withParts (T.unlines ["* TODO Bare", "body line"]) $ \r ->
          assertEqual "planning, then the drawer, then the body"
                      (T.unlines [ "* TODO Bare", "SCHEDULED: <2026-08-01 Sat>"
                                 , ":PROPERTIES:", ":NEW: 1", ":END:", "body line" ])
                      (recomposedSubtree r (headlineParts r)
                         { hpPlanning = [("SCHEDULED", "<2026-08-01 Sat>")]
                         , hpProperties = [("NEW", "1")] })

    , testCase "what a timestamp has to be to be written at all" $ do
        assertBool "an active stamp" (readsAsTimestamp "<2026-08-01 Sat>")
        assertBool "an inactive one" (readsAsTimestamp "[2026-08-01 Sat 09:00]")
        assertBool "a range" (readsAsTimestamp "<2026-08-01 Sat>--<2026-08-05 Wed>")
        assertBool "space around it is stripped" (readsAsTimestamp "  <2026-08-01 Sat>  ")
        mapM_ (\bad -> assertBool ("refused: " <> show bad) (not (readsAsTimestamp bad)))
              [ "", "tomorrow", "2026-08-01"
              , "<2026-08-01 Sat>\nSCHEDULED: <2026-08-02 Sun>" ]
    ]

  , testGroup "logbook"
    [ testCase "the logbook goes back verbatim, whatever the commit says" $
        withParts logged $ \r -> do
          let back = recomposedSubtree r (headlineParts r) { hpLogbook = "ignored" }
          assertContains "the file's own drawer"
                         ":LOGBOOK:\nCLOCK: [2026-08-01 Sat 09:00]--[2026-08-01 Sat 09:30]\n:END:\n"
                         back
          assertBool "and nothing a client sent" (not ("ignored" `T.isInfixOf` back))

    , testCase "a headline with none does not grow one" $
        withParts drawered $ \r ->
          assertBool "no drawer appeared"
            (not (":LOGBOOK:" `T.isInfixOf`
                    recomposedSubtree r (headlineParts r) { hpLogbook = ":LOGBOOK:\n:END:\n" }))

    , testCase "an emptied body still keeps the server's own regions" $
        withParts logged $ \r -> do
          let back = recomposedSubtree r (headlineParts r)
                       { hpBody = "* TODO Logged\n", hpProperties = [] }
          assertContains "the logbook stands" ":LOGBOOK:" back
          assertContains "and the hidden property with it" ":ORG_GLANCE_ID: logged" back
    ]
  ]
  where
    roundTrips doc = withParts doc $ \r -> do
      let parts = headlineParts r
      assertEqual ("round trip of " <> show doc)
                  (asWritten (subtreeText r)) (recomposedSubtree r parts)

    trimsEveryLine doc = withParts doc $ \r -> do
      let back = recomposedSubtree r (headlineParts r)
      assertEqual ("no line of " <> show doc <> " trails")
                  [] [ l | l <- T.splitOn "\n" back, l /= asWritten l ]
      assertEqual "and the line endings are the file's"
                  (T.count "\r\n" (subtreeText r)) (T.count "\r\n" back)

-- | TEXT as a write spells it: each line's trailing horizontal run off, its
-- terminator kept.  An INDEPENDENT spelling of what 'recomposedSubtree'
-- enforces rather than the export of it.
asWritten :: Text -> Text
asWritten = T.intercalate "\n" . map line . T.splitOn "\n"
  where line l = case T.stripSuffix "\r" l of
          Just body -> T.dropWhileEnd (`elem` (" \t" :: String)) body <> "\r"
          Nothing   -> T.dropWhileEnd (`elem` (" \t" :: String)) l

-- | The drawer TEXT holds, line by line and stripped.
drawerOf :: Text -> [Text]
drawerOf text' = takeWhile (/= ":END:") opened <> [":END:"]
  where opened = dropWhile (/= ":PROPERTIES:") (map T.strip (T.lines text'))

-- | Run K over the FIRST record DOC loads to.
withParts :: Text -> (HeadlineRecord -> Assertion) -> Assertion
withParts doc k = withDoc "lens" "lens.org" doc first'
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

-- | What a capture leaves behind: the server's id and creation time around a client pair.
stamped :: Text
stamped = T.unlines
  [ "* TODO Buy milk :errands:"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: kept"
  , ":EFFORT: 0:05"
  , ":ORG_GLANCE_CREATION_TIME: [2026-08-01 Sat 09:30]"
  , ":END:" ]

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

-- | Spacing org never writes and a file can still hold.
oddly :: Text
oddly = T.unlines
  [ "* TODO Odd", ":PROPERTIES:", ":A:one", ":B:", ":C:   three   ", ":END:", "body" ]

-- | A headline carrying a logbook drawer beside its properties.
logged :: Text
logged = T.unlines
  [ "* TODO Logged"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: logged"
  , ":EFFORT: 0:30"
  , ":END:"
  , ":LOGBOOK:"
  , "CLOCK: [2026-08-01 Sat 09:00]--[2026-08-01 Sat 09:30]"
  , ":END:"
  , "body line" ]

-- | A logbook belonging to the CHILD, so this headline's lens reads it as body.
childLogged :: Text
childLogged = T.unlines
  [ "* TODO Parent"
  , "body line"
  , "** Child"
  , ":LOGBOOK:"
  , "CLOCK: kid"
  , ":END:"
  , "child body" ]

-- | The three planning keywords out of org's own order.
permuted :: Text
permuted = T.unlines
  [ "* TODO Permuted"
  , "CLOSED: [2026-07-30 Thu] SCHEDULED: <2026-08-01 Sat>"
  , "body" ]

-- | The indentation org wrote drawers under, which a rendered line matches.
indented :: Text
indented = T.unlines
  [ "* TODO Indented", "  :PROPERTIES:", "  :A: 1", "  :B:  2", "  :END:", "body" ]

crlf :: Text
crlf = T.intercalate "\r\n"
  [ "* TODO Windows", ":PROPERTIES:", ":A: 1", ":END:", "body", "" ]

-- | The pool answers what one thread answered: 'loadDirFilesSerially' is the
-- same load with the pool taken out.  The fixture is wider than any pool, plus
-- one file of each failure kind.
parallelSpec :: TestTree
parallelSpec = testGroup "Parallel load"
  [ testCase "the suite runs on the threaded runtime" $ do
      -- A non-threaded runtime has one capability whatever @-N@ says, and every assertion below still passes.
      assertBool "-threaded" rtsSupportsBoundThreads

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
      forM_ [ReadFailed, DecodeFailed, ParseFailed] $ \kind ->
        assertEqual ("count of " <> show kind)
                    (length (failuresOf kind serial)) (length (failuresOf kind parallel))
      assertEqual "the failing paths, in order" (failures serial) (failures parallel)
      assertEqual "one of each kind, so the comparison is not vacuous"
                  [1, 1, 1]
                  [ length (failuresOf kind parallel)
                  | kind <- [ReadFailed, DecodeFailed, ParseFailed] ]

  , testCase "a tree narrower than the pool loads whole" $ withTempDirNamed "narrow" $ \dir -> do
      -- Fewer files than workers, and one file skips the pool outright.
      empty <- loadDirFilesWith defaultWalk dir
      assertEqual "no files at all" ([], 0) (shapes empty)
      _ <- orgFile dir "one.org" (entryAs "solo" "TODO solo")
      poolEqualsSerial "one file" 1 dir
      forM_ ["b.org", "c.org"] $ \name ->
        orgFile dir name (entryAs (T.pack name) ("TODO " <> T.pack name))
      poolEqualsSerial "three files" 3 dir

  , testCase "the sequence is the same on every run, ids resolved and all" $
      withCorpus $ \dir -> do
        -- 'resolveIds' is first-wins, so reassembling by completion order would move the winner.
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

-- | DIR loaded both ways under WHAT, record for record, carrying FILES files.
poolEqualsSerial :: String -> Int -> FilePath -> Assertion
poolEqualsSerial what files dir = do
  parallel <- shapes <$> loadDirFilesWith defaultWalk dir
  serial <- shapes <$> loadDirFilesSerially defaultWalk dir
  assertEqual what serial parallel
  assertEqual (what <> ": all loaded") files (length (fst parallel))

-- | A tree wider than any pool: forty documents, one shared id, one file per failure kind.
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

-- | R as the strings a comparison reads it by; the parsed headline stays out.
shapeOf :: HeadlineRecord -> [Text]
shapeOf r = map T.pack
  [ hrFile r, show (hrId r), show (hrCategory r), show (hrDigest r)
  , show (hrSubtree r), show (hrKeywords r), show (hrState r), show (hrPriority r)
  , show (hrTitle r), show (hrTags r), show (hrScheduled r), show (hrDeadline r)
  , show (hrSearch r), show (hrLinks r), show (hrLinked r)
  , show (T.length (hrDoc r)) ]

outcomeShape :: Either LoadFailure [HeadlineRecord] -> Either LoadFailure [[Text]]
outcomeShape = fmap (map shapeOf)

-- | A per-file load as the pair a test compares: shaped outcomes and unlistable directories.
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

-- | What the walk crosses and what it declines.  Asserted as the sorted file
-- list: a tree entered twice through a link and a file quietly dropped look
-- alike in a total.  The links point OUTSIDE the walked root for that reason.
walkSpec :: TestTree
walkSpec = testGroup "Walk"
  [ testCase "the symlink matrix, as the files walked and what they loaded to" $
      withSymlinkTree $ \tree files -> do
        let outcomes = [ (tree </> "dangling.org", Left ReadFailed)
                       , (tree </> "linked.org", Right ["four"])
                       , (tree </> "notes.org", Right ["one"])
                       , (tree </> "realdir.org" </> "deep.org", Right ["three"])
                       , (tree </> "under" </> "inner.org", Right ["two"]) ]
        assertEqual "files walked" (map fst outcomes) (map fst files)
        assertEqual "and what each loaded to" outcomes
                    [ (path, map hrTitle <$> outcome) | (path, outcome) <- files ]

  , testCase "a blob is walked and its occurrence history is not" $
      withStoreTree $ \store files ->
        assertEqual "files walked" [store </> "data" </> "ab" </> "cd" </> "data.org"]
                    (map fst files)

    -- The walk and the watch read ONE predicate, so no uncollected file arrives by inotify.
  , testCase "and the watch declines it through the same predicate" $
      withStoreTree $ \store _files -> do
        let occurrence = store </> "data" </> "ab" </> "cd" </> "occurrences"
                               </> "2026-08-02.org"
        assertBool "the snapshot is a document by name" (documentPath occurrence)
        assertBool "and derived, so the watch drops it" (derivedPath occurrence)
        assertBool "while the blob beside it is neither"
                   (not (derivedPath (store </> "data" </> "ab" </> "cd" </> "data.org")))
  ]

-- | Run ACT over an org-glance store: the blob and its history share one @ORG_GLANCE_ID@.
withStoreTree :: (FilePath -> [(FilePath, Either LoadFailure [HeadlineRecord])] -> Assertion)
              -> Assertion
withStoreTree act = withTempDirNamed "store" $ \root -> do
  let store = root </> ".org-glance"
      entry = store </> "data" </> "ab" </> "cd"
  mapM_ (createDirectoryIfMissing True)
        [entry </> "occurrences", store </> "overviews"]
  _ <- orgFile entry "data.org" (withProperty "* DONE live entry\n")
  _ <- orgFile (entry </> "occurrences") "2026-08-02.org" (withProperty "* DONE a repetition\n")
  _ <- orgFile (store </> "overviews") "task.org" (withProperty "* DONE the mirror\n")
  act store . fst =<< loadDirFilesWith defaultWalk root
  where withProperty headline =
          headline <> ":PROPERTIES:\n:ORG_GLANCE_ID: abcd\n:END:\n"

-- | Run ACT over a walked root; every link points into a directory the walk is never given.
withSymlinkTree :: (FilePath -> [(FilePath, Either LoadFailure [HeadlineRecord])] -> Assertion)
                -> Assertion
withSymlinkTree act = withTempDirNamed "walk" $ \root -> do
  let tree = root </> "tree"
      away = root </> "away"
  mapM_ (createDirectoryIfMissing True)
        [tree </> "under", tree </> "realdir.org", away </> "elsewhere"]
  _ <- orgFile tree "notes.org" "* TODO one\n"
  _ <- orgFile (tree </> "under") "inner.org" "* TODO two\n"
  _ <- orgFile (tree </> "realdir.org") "deep.org" "* TODO three\n"
  _ <- orgFile tree "plain.txt" "not a document\n"
  _ <- orgFile away "target.org" "* TODO four\n"
  _ <- orgFile (away </> "elsewhere") "unreachable.org" "* TODO five\n"
  createSymbolicLink (away </> "target.org") (tree </> "linked.org")
  createSymbolicLink (away </> "elsewhere") (tree </> "dirlink")
  createSymbolicLink (away </> "elsewhere") (tree </> "dirlink.org")
  createSymbolicLink "nowhere-at-all" (tree </> "dangling.org")
  createSymbolicLink "dmitry@host.4242:1750000000" (tree </> ".#notes.org")
  act tree . fst =<< loadDirFilesWith defaultWalk tree

-- | Which headlines become rows: one per level-one headline, the rest materialized.
levelSpec :: TestTree
levelSpec = testGroup "Top entries"
  [ testCase "a nested outline is one record per level-one headline" $
      withRecordsOf nested $ \recs ->
        assertEqual "titles" ["one", "five"] (map hrTitle recs)

  , testCase "and each record's subtree still holds the children" $
      withRecordsOf nested $ \recs ->
        assertEqual "subtrees"
                    ["* one\n** two\n*** three\n** four\n", "* five\n"]
                    (map subtreeText recs)

    -- The rule is the star count: a file opening at level two has no top entry.
  , testCase "a file that never reaches level one contributes no rows" $
      withRecordsOf (T.unlines ["** two", "*** three"]) $ \recs ->
        assertEqual "rows" [] (map hrTitle recs)

    -- An id on a deeper headline is neither a row id nor a collision.
  , testCase "an ORG_GLANCE_ID under a child is not a row id" $
      withRecordsOf (T.unlines [ "* parent", "** child", ":PROPERTIES:"
                               , ":ORG_GLANCE_ID: kid", ":END:" ]) $ \recs -> do
        assertEqual "titles" ["parent"] (map hrTitle recs)
        assertBool ("kid is a row id: " <> show (map hrId recs))
                   ("kid" `notElem` map hrId recs)
  ]

-- | A top entry with none of the six columns is no row: the file keeps it, the table skips it.
blankSpec :: TestTree
blankSpec = testGroup "Blank entries"
  [ testCase "an entry with nothing to show is no row" $
      mapM_ (\(what, doc) -> withRecordsOf doc $ \recs ->
                assertEqual what [] (map hrId recs))
            [ ("stars and a space", "* \n")
            , ("stars alone",       "*\n") ]

  , testCase "one filled column is enough, and it is the one that was filled" $
      mapM_ (\(want, doc) -> withRecordsOf doc $ \recs ->
                assertEqual (T.unpack want) [[want]] (map filledColumns recs))
            [ ("state",     "* TODO\n")
            , ("priority",  "* [#A]\n")
            , ("title",     "* a title\n")
            , ("scheduled", "* \nSCHEDULED: <2026-08-01 Sat>\n")
            , ("deadline",  "* \nDEADLINE: <2026-08-01 Sat>\n") ]

    -- Org spells tags after a title, so a headline of nothing but colons is a TITLE of colons.
  , testCase "a headline of nothing but tags is a title of colons" $
      withRecordsOf "* :work:\n" $ \recs -> do
        assertEqual "columns" [["title"]] (map filledColumns recs)
        assertEqual "the colons are the title" [":work:"] (map hrTitle recs)

  , testCase "and tags proper keep a row the title already kept" $
      withRecordsOf "* a title :work:\n" $ \recs ->
        assertEqual "columns" [["title", "tag"]] (map filledColumns recs)

    -- A blank entry has no row id, so an @ORG_GLANCE_ID@ on one addresses nothing.
  , testCase "nothing outside the six columns rescues an entry" $
      mapM_ (\(what, doc) -> withRecordsOf doc $ \recs ->
                assertEqual what [] (map hrId recs))
            [ ("closed",   "* \nCLOSED: [2026-08-01 Sat]\n")
            , ("drawer",   "* \n:PROPERTIES:\n:ORG_GLANCE_ID: solo\n:END:\n")
            , ("body",     "* \nsome text under it\n")
            , ("children", "* \n** TODO a child\n") ]

    -- The ordinal numbers EMITTED rows, so a blank entry spends none.
  , testCase "a blank entry spends no ordinal" $
      withRecordsOf (T.unlines ["* one", "* ", "* two"]) $ \recs -> do
        assertEqual "titles" ["one", "two"] (map hrTitle recs)
        assertEqual "ids" [ T.pack (hrFile r) <> k | (r, k) <- zip recs ["#0", "#1"] ]
                    (map hrId recs)

  , testCase "so no row the loader emits has six empty cells" $ withRecords $ \recs -> do
      assertEqual "the fixture's rows" 6 (length recs)
      assertEqual "blank rows" [] [ hrId r | r <- recs, null (filledColumns r) ]
  ]

-- | The column keys R fills, in column order.
filledColumns :: HeadlineRecord -> [Text]
filledColumns r =
  [ key | (key, cell) <- zip ["state", "priority", "title", "scheduled", "deadline", "tag"]
                             [ opt (hrState r), opt (hrPriority r), hrTitle r
                             , opt (hrScheduled r), opt (hrDeadline r), hrTags r ]
        , not (T.null cell) ]
  where opt = fromMaybe ""

-- | The search text a filter runs over.  The expected strings are written down
-- rather than taken from the renderer: agreeing with @table-view.js@'s
-- @displayText@ is the whole point.
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
      assertEqual "trailing" "a " (displayText "a\n")

    -- The tags field is the CELL's, so the haystack carries the sorted spelling.
  , testCase "the row's search text is its cells, lowercased" $ withRecords $ \recs -> do
      let first' = head recs
      assertEqual "the whole row, cell by cell"
                  "next\SUB[#a]\SUBship the table view\SUB2026-08-01 09:30\SUB2026-08-05\SUB:glance:web:"
                  (T.replace "\US" "\SUB" (hrSearch first'))

  , testCase "a query matches case-insensitively, trimmed, and never across cells" $
      withRecords $ \recs -> do
        let matching q = length (filter (matchesSearch q) recs)
        assertEqual "case" 1 (matching "SHIP THE TABLE")
        assertEqual "trimmed" 1 (matching "  ship the table  ")
        assertEqual "unicode, cyrillic mid-title" 1 (matching "печатник")
        assertEqual "unicode, cyrillic title" 1 (matching "Привет")
        assertEqual "an empty query is every row" 6 (matching "")
        assertEqual "blank is empty too" 6 (matching "   ")
        -- The cells are joined by a character no cell can hold.
        assertEqual "across the join" 0 (matching "next a")

    -- The visible cost of rows being top entries: a word only a child carries reaches nothing.
  , testCase "a word only a child carries matches nothing" $
      withRecordsOf (T.unlines ["* parent", "** subterranean child"]) $ \recs -> do
        assertEqual "the entry is a row" 1 (length (filter (matchesSearch "parent") recs))
        assertEqual "the child is not" 0 (length (filter (matchesSearch "subterranean") recs))
        assertBool "though its subtree still spells it"
                   (all (T.isInfixOf "subterranean" . subtreeText) recs)
  ]

-- | Cells are cut from the source, dates spelled the way the wire wants them.
cellSpec :: TestTree
cellSpec = testGroup "Cells"
  [ testCase "titles and tags come from the source, unicode included" $ withRecords $ \recs -> do
      assertEqual "titles"
                  [ "Ship the table view", "Привет мир", "Reply from the печатник"
                  , "Plain headline without a state", "Drop the old renderer"
                  , "Read the schema" ]
                  (map hrTitle recs)
      -- The FIELD is the file's own order, which 'classify' reads.
      assertEqual "tags"
                  [":web:glance:", ":unicode:", "", "", ":cleanup:", ":web:"]
                  (map hrTags recs)

    -- The CELL sorts, case-folded, and the field it is cut from does not.
  , testCase "the tags CELL sorts, case-folded, where the field keeps the file" $ do
      assertEqual "the sample row's own cell"
                  ":glance:web:" (sortedTagsCell ":web:glance:")
      assertEqual "folded, so a capital does not sort ahead of every lowercase"
                  ":admin:Work:" (sortedTagsCell ":Work:admin:")
      assertEqual "and stable under a fold tie"
                  ":a:Work:work:" (sortedTagsCell ":Work:work:a:")
      assertEqual "an untagged cell is handed straight back" "" (sortedTagsCell "")
      assertEqual "one tag is already sorted" ":web:" (sortedTagsCell ":web:")

  , testCase "the column is what carries the sort, and the row's JSON with it" $
      withRecords $ \recs -> do
        cells <- traverse (field "cells" . rowJSON) recs
        assertEqual "the cell the table draws"
                    [":glance:web:", ":unicode:", "", "", ":cleanup:", ":web:"]
          =<< traverse (textAt "tag") cells
        assertEqual "and the field it was cut from, untouched"
                    ":web:glance:" (hrTags (head recs))

  , testCase "states are the keywords verbatim, custom ones too" $ withRecords $ \recs ->
      assertEqual "states"
                  [Just "NEXT", Just "TODO", Just "WAITING", Nothing, Just "CANCELLED", Just "DONE"]
                  (map hrState recs)

  , testCase "priorities are org's own bracketed spelling" $ withRecords $ \recs ->
      assertEqual "priorities"
                  [Just "[#A]", Just "[#B]", Nothing, Nothing, Just "[#C]", Nothing]
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

    -- K is a position in the FILE's top entries, whatever the rows around it are called.
  , testCase "without one the row id is FILE#K, K the entry's place in the file" $
      withRecords $ \recs ->
        assertEqual "ids" (map (\k -> T.pack (viewDir </> "sample.org") <> "#" <> k)
                               ["1", "2", "3", "4", "5"])
                    (map hrId (drop 1 recs))

  , testCase "and K counts entries: a child spends no ordinal" $
      withRecordsOf (T.unlines ["* one", "** a child", "*** and another", "* two"]) $ \recs -> do
        assertEqual "titles" ["one", "two"] (map hrTitle recs)
        assertEqual "ids" [ T.pack (hrFile r) <> k | (r, k) <- zip recs ["#0", "#1"] ]
                    (map hrId recs)
  ]

-- | The view document; the golden pins every value, so the column order is stated apart.
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
        ["state", "priority", "title", "scheduled", "deadline", "tag"] keys

  -- Stated as the whole list, so a column added without @sortable@ fails here.
  , testCase "every column opts into sorting" $ withView $ \v -> do
      keys <- columnKeysOf v
      flags <- listAt "columns" v >>= mapM (maybeBoolAt "sortable")
      assertEqual "one sortable per column" (map (const (Just True)) keys) flags

  -- A row is an id and its cells; nothing says where it sits among the others.
  , testCase "no row carries a depth, as a field or as a cell" $
      withViewOf nested $ \v -> do
        cols <- columnKeysOf v
        rows <- listAt "rows" v
        -- Over no rows the sweeps below are met by saying nothing.
        assertEqual "the fixture's rows" 2 (length rows)
        assertBool "depth is a column" ("depth" `notElem` cols)
        fields <- mapM keysOf rows
        assertBool (show fields <> " names depth") (all ("depth" `notElem`) fields)
        cells <- mapM (keysOf <=< field "cells") rows
        assertBool (show cells <> " names depth") (all ("depth" `notElem`) cells)
  ]

-- | Shapes SCHEMA.md requires of any producer.
schemaSpec :: TestTree
schemaSpec = testGroup "Schema conformance"
  [ testCase "every cell key is a column key" $ withView $ \v -> do
      cols <- columnKeysOf v
      rows <- listAt "rows" v
      -- Over no rows the claim below is met by saying nothing.
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

    -- An ADDITIVE row field (@table-view\/SCHEMA.md@): @true@ or absent, never @false@.
  , testCase "a linked row says so, and a bare one says nothing at all" $
      withViewOf (T.unlines [ "* linked", "see https://x.example", "* bare" ]) $ \v -> do
        rows <- listAt "rows" v
        assertEqual "the fixture's rows" 2 (length rows)
        assertEqual "true, then absent" [Just True, Nothing]
                    =<< mapM (maybeBoolAt "linked") rows

  , testCase "and it is a row field rather than a cell of its own" $
      withViewOf "* linked\nsee https://x.example\n" $ \v -> do
        cols <- columnKeysOf v
        cells <- each "rows" "cells" v >>= mapM keysOf
        assertEqual "the fixture's rows" 1 (length cells)
        assertBool "linked is a column" ("linked" `notElem` cols)
        assertBool (show cells <> " names linked") (all ("linked" `notElem`) cells)

  , testCase "the badge column carries a palette" $ withView $ \v -> do
      state <- columnOf "state" v
      kind <- field "type" state >>= text
      badges <- listAt "badges" state
      assertEqual "type" "badge" kind
      assertBool "badges are empty" (not (null badges))

  , testCase "and the two group values a filter can name" $ withView $ \v -> do
      -- Vocabulary rather than cell text: no row's state cell holds either.
      state <- columnOf "state" v
      values <- listAt "values" state >>= mapM text
      assertEqual "values" ["*active*", "*inactive*"] values

  , testCase "the multi-valued column says so, and it is the only one"
      $ withView $ \v -> do
      -- Declared rather than sampled: a page with under two tagged rows finds no list column.
      cols <- listAt "columns" v
      keys <- mapM (textAt "key") cols
      multi <- mapM (maybeBoolAt "multi") cols
      assertEqual "the columns declaring multi" ["tag"]
                  [ k | (k, Just True) <- zip keys multi ]

  , testCase "the declared sort is a chain of the view's own columns" $
      withView $ \v -> do
        cols <- columnKeysOf v
        keys <- each "sort" "column" v >>= mapM text
        assertEqual "the default chain"
                    ["state", "title", "deadline", "scheduled"] keys
        assertBool (show keys <> " outside " <> show cols) (all (`elem` cols) keys)
        ascs <- each "sort" "ascending" v >>= mapM boolOf
        assertEqual "every key ascends" (map (const True) keys) ascs

  , testCase "the rows are served in the order the chain declares" $ do
      -- The declaration and the arrangement are one list read twice.
      let doc = T.unlines [ "* TODO beta", "* echo", "* DONE alpha"
                          , "* TODO Alpha", "* delta" ]
      withRecordsOf doc $ \records ->
        assertEqual "state by palette order, then title folded"
                    ["TODO Alpha", "TODO beta", "DONE alpha", "delta", "echo"]
                    [ maybe "" (<> " ") (hrState r) <> hrTitle r
                    | r <- sortedForView records ]

  , testCase "an empty cell sorts to the end of its own key" $
      -- Nulls are settled per key and outside the direction.
      withRecordsOf (T.unlines ["* same", "* TODO same", "* DONE same"]) $
        \records ->
          assertEqual "the stateless row last"
                      [Just "TODO", Just "DONE", Nothing]
                      (map hrState (sortedForView records))

    -- The comparator reads THROUGH org's brackets, the rule the filter matches by.
  , testCase "priority orders by its letter, through the brackets" $
      withRecordsOf (T.unlines [ "* [#C] gamma", "* [#A] alpha", "* beta", "* [#B] delta" ]) $
        \records ->
          assertEqual "A, B, C, then the row with none"
                      [Just "[#A]", Just "[#B]", Just "[#C]", Nothing]
                      (map hrPriority (sortedForViewWith (TodoKeywords [] []) [("priority", True)] records))

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

-- Commands: the span math the structured commands run on.  Every case splices
-- the edits itself rather than through 'Data.Org.Edit.applyEdits' — an oracle
-- sharing the engine would agree with a wrong offset — and asserts the WHOLE
-- document.

-- | DOC with EDITS applied, right to left so an earlier offset is never moved.
splice :: Text -> [(Span, Text)] -> Text
splice doc edits = foldl' one doc (sortOn (negate . spanStart . fst) edits)
  where one text (Span s e, new) = T.take s text <> new <> T.drop e text

-- | Run K over the one record DOC parses to.
withRecord :: Text -> (HeadlineRecord -> Assertion) -> Assertion
withRecord doc k = withDoc "command" "one.org" doc one
  where one [r] = k r
        one rs  = assertFailure ("expected one headline, got " <> show (length rs))

-- | Run K over the FIRST of DOC's records, for a case about the headline BELOW.
withFirstRecord :: Text -> (HeadlineRecord -> Assertion) -> Assertion
withFirstRecord doc k = withDoc "command" "one.org" doc one
  where one (r:_) = k r
        one []    = assertFailure "expected a headline, got none"

-- | WHAT: DOC with @set-state KEYWORD@ on its one headline is WANTED, under 'noConfig'.
setStateIs :: String -> Text -> Maybe Text -> Text -> Assertion
setStateIs what doc keyword = triedEditsAre what doc (setStateEdits noConfig keyword)

-- | @set-state KEYWORD@ on R under 'layered' is refused, naming keyword, row and WORDS.
refusalNames :: HeadlineRecord -> Text -> [Text] -> Assertion
refusalNames r keyword words' = case setStateEdits layered (Just keyword) r of
  Right edits -> assertFailure ("expected a refusal, got " <> show edits)
  Left why    -> mapM_ (\w -> assertContains "names" w why) (keyword : hrId r : words')

-- | @set-state@ on R under 'layered' takes each of WORDS.
accepts :: HeadlineRecord -> [Text] -> Assertion
accepts r = mapM_ (\w -> either (assertFailure . ((T.unpack w <> ": ") <>) . T.unpack)
                                (const (pure ()))
                                (setStateEdits layered (Just w) r))

-- | A config with a cycle per tag and one in @system.org@.
layered :: ConfigLayers
layered = noConfig
  { clSystem = TodoKeywords ["STARTED"] []
  , clTags   = [ ("book", TodoKeywords ["READING"] ["READ"])
               , ("film", TodoKeywords ["WATCHING"] ["WATCHED"]) ]
  , clSeed   = TodoKeywords ["STARTED", "READING", "WATCHING"] ["READ", "WATCHED"] }

-- | WHAT: DOC with EDITS applied to its one headline is WANTED, asserted whole.
editsAre :: String -> Text -> (HeadlineRecord -> [(Span, Text)]) -> Text -> Assertion
editsAre what doc edits wanted =
  withRecord doc (assertEqual what wanted . splice doc . edits)

-- | 'editsAre' for the commands whose span math can REFUSE.
triedEditsAre :: String -> Text -> (HeadlineRecord -> Either Text [(Span, Text)])
              -> Text -> Assertion
triedEditsAre what doc edits wanted = withRecord doc $ \r ->
  case edits r of
    Left why     -> assertFailure (what <> ": refused: " <> T.unpack why)
    Right splices -> assertEqual what wanted (splice doc splices)

-- | WHAT: DOC with @archive@ applied to its one headline is WANTED.
archiveIs :: String -> Text -> Text -> Assertion
archiveIs what doc = editsAre what doc archiveEdits

-- | WHAT: DOC with @add-tag TAG@ applied to its one headline is WANTED.
addTagIs :: String -> Text -> Text -> Text -> Assertion
addTagIs what doc tag = editsAre what doc (addTagEdits tag)

-- | WHAT: DOC with @remove-tag TAG@ applied to its one headline is WANTED.
removeTagIs :: String -> Text -> Text -> Text -> Assertion
removeTagIs what doc tag = editsAre what doc (removeTagEdits tag)

-- | WHAT: DOC with its FIRST link retargeted to TARGET under DESC is WANTED.
editLinkIs :: String -> Text -> Text -> Maybe (Maybe Text) -> Text -> Assertion
editLinkIs what doc target desc = triedEditsAre what doc $ \r ->
  case subtreeLinks r of
    []      -> Left "the document holds no link"
    (l : _) -> editLinkEdits (olSpan l) target desc r

-- | WHAT: DOC with @rename-tag FROM TO@ applied to its one headline is WANTED.
renameTagIs :: String -> Text -> Text -> Text -> Text -> Assertion
renameTagIs what doc from to = editsAre what doc (renameTagEdits from to)

-- | Is TEXT a tag, as 'tagText' answers?  The charset is the parser's own.
tagIs :: (Text, Bool) -> Assertion
tagIs (text, wanted) = assertEqual (show text) wanted (isRight (tagText text))

-- | A document declaring keywords past org's own two.
keyworded :: Text -> Text
keyworded rest = "#+TODO: NEXT WAITING | CANCELLED\n" <> rest

-- | TITLE with a planning line and a drawer under it: the lines a title-line edit must leave alone.
plannedEntry :: Text -> Text
plannedEntry title' = T.unlines
  [ title'
  , "SCHEDULED: <2026-08-01 Sat>"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: ship"
  , ":END:" ]

commandSpec :: TestTree
commandSpec = testGroup "Commands"
  [ testGroup "set-state"
    [ testCase "over a keyword, replaces exactly that word" $
        setStateIs "replaced" (keyworded "* NEXT [#A] Ship it :web:\n") (Just "WAITING")
                              (keyworded "* WAITING [#A] Ship it :web:\n")

    , testCase "with no keyword, inserts one right after the stars" $
        setStateIs "inserted" "* [#B] Plain :tag:\n" (Just "TODO")
                              "* TODO [#B] Plain :tag:\n"

      -- Stars and nothing else is no row at all ('blankEntry').
    , testCase "into a headline whose only content is a priority" $
        setStateIs "bare" "* [#B]\n" (Just "TODO") "* TODO [#B]\n"

    , testCase "a null keyword takes the word and the space behind it" $
        setStateIs "cleared" (keyworded "* NEXT Ship it :web:\n") Nothing
                             (keyworded "* Ship it :web:\n")

    , testCase "and the whole run of it, however wide" $
        setStateIs "cleared wide" (keyworded "*   NEXT   Ship it\n") Nothing
                                  (keyworded "*   Ship it\n")

      -- Without the newline the headline swallows the line below.
    , testCase "a keyword ending its line keeps the newline" $
        let doc = keyworded "* NEXT\n* NEXT Second\n" in
        withFirstRecord doc $ \r ->
          case setStateEdits noConfig Nothing r of
            Left why      -> assertFailure ("cleared at eol: refused: " <> T.unpack why)
            Right splices -> assertEqual "cleared at eol"
                               (keyworded "* \n* NEXT Second\n") (splice doc splices)
    , testCase "and the line under it is a headline of its own" $
        withDoc "command" "one.org" (keyworded "* NEXT\n* NEXT Second\n") $ \rs ->
          assertEqual "two rows" 2 (length rs)

    , testCase "clearing a headline that has no keyword costs no edit" $
        withRecord "* Plain\n" $ \r ->
          assertEqual "no edits" (Right []) (setStateEdits noConfig Nothing r)

      -- Per CHAIN, and the file is its nearest scope.
    , testCase "a keyword the chain does not declare is refused, by name" $
        withRecord "* TODO Plain\n" $ \r ->
          refusalNames r "WAITING" ["TODO", "DONE"]

    , testCase "the same keyword is legal once the file declares it" $
        setStateIs "declared" (keyworded "* TODO Plain\n") (Just "WAITING")
                              (keyworded "* WAITING Plain\n")

    , testCase "each scope of the chain is settable on a row that reaches it" $
        withRecord (keyworded "* TODO Plain :book:\n") $ \r ->
          accepts r ["NEXT", "READING", "STARTED", "DONE"]

      -- @film@'s cycle parses here — the seed carries it — and no scope this row reaches declares it.
    , testCase "another tag's keyword is refused on a row that does not carry it" $
        withRecord "* TODO Plain\n" $ \r ->
          refusalNames r "WATCHING" ["STARTED"]

    , testCase "and refused on a row carrying a different tag" $
        withRecord "* TODO Plain :book:\n" $ \r ->
          refusalNames r "WATCHING" ["READING"]

      -- 'settableStates' IS 'keywordSources' flattened, so this guards the derivation.
    , testCase "everything the palette shows for a row is settable on it" $
        withRecord (keyworded "* NEXT Plain :book:\n") $ \r -> do
          let shown = [ w | (_source, kw) <- keywordSources layered [r]
                          , w <- tkActive kw <> tkInactive kw ]
          assertEqual "every rung of this row's chain is on offer"
                      [ "TODO", "DONE"                   -- org's own
                      , "STARTED"                        -- system.org's
                      , "READING", "READ"                -- its `book' tag's
                      , "NEXT", "WAITING", "CANCELLED" ] -- the file's own
                      shown
          accepts r shown

    , testCase "and the reorder moved which source shows a word, never the set" $
        withRecord (keyworded "* NEXT Plain :book:\n") $ \r ->
          assertEqual "the same eight words the nearest-scope chain offered"
                      (sort [ "NEXT", "WAITING", "CANCELLED", "READING", "READ"
                            , "STARTED", "TODO", "DONE" ])
                      (sort (settableStates layered r))

    , testCase "the state column's group meta-values are not keywords" $
        withRecord (keyworded "* NEXT Plain\n") $ \r ->
          mapM_ (\meta -> case setStateEdits noConfig (Just meta) r of
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

      -- 'hsFull' ends at the LAST part in span order: appending there lands inside the drawer.
    , testCase "past a planning line and a drawer, still on the title line" $
        archiveIs "planned" (plannedEntry "* TODO Ship it")
                            (plannedEntry "* TODO Ship it :ARCHIVE:")

    , testCase "onto a headline with no title either" $
        archiveIs "titleless" "* TODO\n" "* TODO :ARCHIVE:\n"

    , testCase "a row already carrying the tag costs no edit" $
        withRecord "* TODO Ship it :web:ARCHIVE:\n" $ \r -> do
          assertBool "reads as archived" (archived r)
          assertEqual "no edits" [] (archiveEdits r)

    , testCase "however the file spells the tag" $
        withRecord "* TODO Ship it :archive:\n" $ \r ->
          assertEqual "no edits" [] (archiveEdits r)

    , testCase "and an untagged row does not read as archived" $
        withRecord "* TODO Ship it :web:\n"
                   (assertBool "not archived" . not . archived)

      -- Archiving IS adding one tag, so there is no second insertion rule to drift.
    , testCase "archive is add-tag at org's own name" $
        mapM_ (\doc -> withRecord doc $ \r ->
                 assertEqual (T.unpack doc) (addTagEdits "ARCHIVE" r) (archiveEdits r))
              [ "* TODO Ship it :web:\n", "* TODO Ship it\n"
              , "* TODO Ship it :ARCHIVE:\n", "* TODO\n" ]
    ]

    -- Both are idempotent from opposite sides, which lets a bulk toggle be pressed at.
  , testGroup "add-tag"
    [ -- The written run is the FILE's, in the file's own order: this row's CELL
      -- reads @:glance:web:@ and the splice still lands after @glance:@.
      testCase "joins the run, ahead of its closing colon" $
        addTagIs "into the run" "* TODO Ship it :web:glance:\n" "work"
                                "* TODO Ship it :web:glance:work:\n"

      -- The file's offsets decide which bytes come out; an alphabetical write would not.
    , testCase "and a removal cuts the file's entry, not the cell's" $
        removeTagIs "the first of the file's run" "* TODO Ship it :web:glance:\n"
                    "web" "* TODO Ship it :glance:\n"

    , testCase "with no run, opens one at the end of the title line" $
        addTagIs "creating" "* TODO Ship it\n" "work" "* TODO Ship it :work:\n"

    , testCase "past a planning line, still on the title line" $
        addTagIs "planned" (T.unlines [ "* TODO Ship it"
                                      , "SCHEDULED: <2026-08-01 Sat>" ])
                           "work"
                           (T.unlines [ "* TODO Ship it :work:"
                                      , "SCHEDULED: <2026-08-01 Sat>" ])

    , testCase "a row already carrying it costs no edit" $
        withRecord "* TODO Ship it :web:work:\n" $ \r -> do
          assertBool "reads as tagged" (tagged "work" r)
          assertEqual "no edits" [] (addTagEdits "work" r)

    , testCase "however the file spells it" $
        withRecord "* TODO Ship it :Work:\n" $ \r -> do
          assertBool "reads as tagged" (tagged "work" r)
          assertEqual "no edits" [] (addTagEdits "work" r)
    ]

  , testGroup "remove-tag"
    [ testCase "cuts the entry and the colon that closes it" $
        removeTagIs "from the middle" "* TODO Ship it :web:glance:work:\n" "glance"
                                      "* TODO Ship it :web:work:\n"

    , testCase "the last entry takes the run and its space with it" $
        removeTagIs "emptying" "* TODO Ship it :work:\n" "work" "* TODO Ship it\n"

    , testCase "and a wider gap comes off whole" $
        removeTagIs "spaced" "* TODO Ship it    :work:\n" "work" "* TODO Ship it\n"

    , testCase "emptying it leaves the lines under the headline alone" $
        removeTagIs "planned" (plannedEntry "* TODO Ship it :work:")
                              "work"
                              (plannedEntry "* TODO Ship it")

    , testCase "a row that never had it costs no edit" $
        withRecord "* TODO Ship it :web:\n" $ \r -> do
          assertBool "not tagged" (not (tagged "work" r))
          assertEqual "no edits" [] (removeTagEdits "work" r)

    , testCase "and a row with no run at all costs none either" $
        withRecord "* TODO Ship it\n" $ \r ->
          assertEqual "no edits" [] (removeTagEdits "work" r)

      -- Folded, and EVERY entry spelling it, so a file spelling one tag twice comes out clean.
    , testCase "however the file spells it, and however often" $ do
        removeTagIs "folded" "* TODO Ship it :Work:\n" "work" "* TODO Ship it\n"
        removeTagIs "twice" "* TODO Ship it :work:web:Work:\n" "work"
                            "* TODO Ship it :web:\n"

    , testCase "removing what was just added puts the file back" $ do
        let doc = "* TODO Ship it :web:\n"
        withRecord doc $ \r -> do
          let added = splice doc (addTagEdits "work" r)
          assertEqual "added" "* TODO Ship it :web:work:\n" added
          withRecord added $ \r' ->
            assertEqual "and back" doc (splice added (removeTagEdits "work" r'))
    ]

    -- A command rather than a composition: the edit sets touch, and 'applyEdits' rejects only overlap.
  , testGroup "rename-tag"
    [ testCase "replaces the entry where it stands, colon and all left alone" $ do
        renameTagIs "in the middle" "* TODO Ship it :web:glance:work:\n"
                    "glance" "code" "* TODO Ship it :web:code:work:\n"
        renameTagIs "at the head" "* TODO Ship it :web:glance:\n"
                    "web" "code" "* TODO Ship it :code:glance:\n"
        renameTagIs "alone" "* TODO Ship it :work:\n"
                    "work" "projects" "* TODO Ship it :projects:\n"

      -- The run's ORDER is untouched, so the popup's union does not reshuffle under the cursor.
    , testCase "and the run's order is what it was" $
        renameTagIs "kept" "* TODO Ship it :a:b:c:\n" "a" "z"
                           "* TODO Ship it :z:b:c:\n"

    , testCase "the lines under the headline are untouched" $
        renameTagIs "planned" (plannedEntry "* TODO Ship it :work:")
                              "work" "projects"
                              (plannedEntry "* TODO Ship it :projects:")

    , testCase "a row that does not carry the old name costs no edit" $
        withRecord "* TODO Ship it :web:\n" $ \r ->
          assertEqual "no edits" [] (renameTagEdits "work" "projects" r)

    , testCase "and a row with no run at all costs none either" $
        withRecord "* TODO Ship it\n" $ \r ->
          assertEqual "no edits" [] (renameTagEdits "work" "projects" r)

    , testCase "the old name is matched folded, and the new one written as given" $ do
        renameTagIs "folded" "* TODO Ship it :Work:\n" "work" "projects"
                             "* TODO Ship it :projects:\n"
        renameTagIs "respelled" "* TODO Ship it :Work:\n" "work" "work"
                                "* TODO Ship it :work:\n"

      -- ONE TAG ONCE: 'removeTagEdits' cuts every entry that spells its tag.
    , testCase "a tag spelled twice comes out spelled once" $
        renameTagIs "deduplicated" "* TODO Ship it :work:web:Work:\n" "work" "projects"
                                   "* TODO Ship it :projects:web:\n"

      -- Where the row ALREADY carries the new name the rename is a removal.
    , testCase "a row already carrying the new name loses the old one instead" $ do
        renameTagIs "merged" "* TODO Ship it :web:work:\n" "web" "work"
                             "* TODO Ship it :work:\n"
        renameTagIs "merged the other way" "* TODO Ship it :web:work:\n" "work" "web"
                                           "* TODO Ship it :web:\n"

    , testCase "and the run and its separator stand" $
        renameTagIs "run kept" "* TODO Ship it  :a:b:\n" "a" "b"
                               "* TODO Ship it  :b:\n"

      -- The removal ends where the addition inserts, and the anchor was measured BEFORE it.
    , testCase "the composition it replaces writes the tag onto the title" $ do
        let doc = "* TODO Ship it :work:\n"
        withRecord doc $ \r -> do
          assertEqual "the removal takes the run, and the addition lands past it"
                      "* TODO Ship itprojects:\n"
                      (splice doc (removeTagEdits "work" r <> addTagEdits "projects" r))
          assertEqual "where the one command spells the file" "* TODO Ship it :projects:\n"
                      (splice doc (renameTagEdits "work" "projects" r))
    ]

    -- @edit-link@: the one command whose args name a row's own BYTES, spanned by the scan.
  , testGroup "edit-link"
    [ -- THE FORM TABLE.  A bracketed link stays bracketed and a plain URL stays
      -- plain; a description ARRIVING is the one thing that changes a shape.
      testCase "the shape is preserved, and only an arriving description moves it" $
        mapM_ (\(what, wrote, target, desc, wanted) ->
                 editLinkIs what ("* one " <> wrote <> "\n") target desc
                            ("* one " <> wanted <> "\n"))
          [ ( "described, target alone", "[[https://a.example][A]]"
            , "https://b.example", Nothing, "[[https://b.example][A]]" )
          , ( "described, description replaced", "[[https://a.example][A]]"
            , "https://b.example", Just (Just "B"), "[[https://b.example][B]]" )
          , ( "described, description off", "[[https://a.example][A]]"
            , "https://b.example", Just Nothing, "[[https://b.example]]" )
          , ( "bracketed bare, target alone", "[[https://a.example]]"
            , "https://b.example", Nothing, "[[https://b.example]]" )
          , ( "bracketed bare, description added", "[[https://a.example]]"
            , "https://b.example", Just (Just "B"), "[[https://b.example][B]]" )
          , ( "bracketed bare, description off", "[[https://a.example]]"
            , "https://b.example", Just Nothing, "[[https://b.example]]" )
          , ( "plain URL, target alone", "https://a.example"
            , "https://b.example", Nothing, "https://b.example" )
          , ( "plain URL, description added", "https://a.example"
            , "https://b.example", Just (Just "B"), "[[https://b.example][B]]" )
          , ( "plain URL, description off", "https://a.example"
            , "https://b.example", Just Nothing, "https://b.example" ) ]

      -- ABSENT IS NOT NULL (@.:!@ rather than @.:?@): silence leaves the author's description.
    , testCase "a description that shows nothing is the null spelled another way" $ do
        editLinkIs "empty string" "* one [[https://a.example][A]]\n"
                   "https://b.example" (Just (Just "")) "* one [[https://b.example]]\n"
        editLinkIs "spaces" "* one [[https://a.example][A]]\n"
                   "https://b.example" (Just (Just "  ")) "* one [[https://b.example]]\n"
        editLinkIs "an empty section nobody touched stands"
                   "* one [[https://a.example][]]\n"
                   "https://b.example" Nothing "* one [[https://b.example][]]\n"
        -- The emptiness test strips and the VALUE is written verbatim: content is nobody's to trim.
        editLinkIs "a description with content keeps its spacing"
                   "* one [[https://a.example][A]]\n"
                   "https://b.example" (Just (Just " spaced "))
                   "* one [[https://b.example][ spaced ]]\n"

    , testCase "the link is the only thing that moves" $
        editLinkIs "the prose either side stands"
                   "* one\nsee [[https://a.example][A]] and stop.\n"
                   "https://b.example" (Just (Just "B"))
                   "* one\nsee [[https://b.example][B]] and stop.\n"

      -- A link in a CHILD is the row's, since a row's links are its subtree's.
    , testCase "a link under a child is the row's to edit" $
        editLinkIs "the child's line moves"
                   "* one\n** child [[https://a.example][A]]\n"
                   "https://b.example" Nothing
                   "* one\n** child [[https://b.example][A]]\n"

      -- THE FIRST WALL: the span sits inside the ROW's subtree and covers one link edge to edge.
    , testCase "a span that does not cover exactly one link is refused" $
        withRecord "* one\nsee [[https://a.example][A]] and https://b.example\n" $ \r -> do
          let refused what sp = case editLinkEdits sp "https://c.example" Nothing r of
                Right edits -> assertFailure (what <> ": expected a refusal, got "
                                                <> show edits)
                Left why    -> assertContains what "does not read as one link" why
          refused "one character short" (Span 10 33)
          refused "one character long" (Span 10 35)
          refused "prose" (Span 6 9)
          refused "two links at once" (Span 10 52)

    , testCase "a span outside the row's subtree is refused, naming both" $
        withRecord "* one [[https://a.example][A]]\n" $ \r ->
          case editLinkEdits (Span 900 950) "https://c.example" Nothing r of
            Right edits -> assertFailure ("expected a refusal, got " <> show edits)
            Left why    -> do
              assertContains "names the span" "[900,950)" why
              assertContains "and the row" (hrId r) why

      -- THE SECOND WALL: the write engine is content-agnostic by law, so this layer owes the reparse.
    , testCase "a replacement that would not read as one link is refused" $
        mapM_ (\(what, wrote, target) ->
                 withRecord ("* one " <> wrote <> "\n") $ \r ->
                   case subtreeLinks r of
                     (l : _) -> case editLinkEdits (olSpan l) target Nothing r of
                       Right edits -> assertFailure (what <> ": expected a refusal, got "
                                                       <> show edits)
                       Left why -> assertContains what "does not read as one link" why
                     [] -> assertFailure (what <> ": no link to edit"))
          [ ("a bracket in the target", "[[https://a.example][A]]", "https://a]b")
          , ("a bare link swapped for a path", "https://a.example", "file:notes.org")
          , ("a bare link given a space", "https://a.example", "https://a b") ]

      -- REPARSING ALONE IS NOT THE WALL: @a][b@ renders one link the request never named.
    , testCase "a target that reparses as another link is refused, naming both" $
        withRecord "* one [[https://a.example]]\n" $ \r ->
          case subtreeLinks r of
            (l : _) -> case editLinkEdits (olSpan l) "https://a][b" Nothing r of
              Right edits -> assertFailure ("expected a refusal, got " <> show edits)
              Left why -> do
                assertContains "names what would have been written" "[[https://a][b]]" why
                assertContains "and what it was asked to point at" "https://a][b" why
            [] -> assertFailure "no link to edit"

      -- A NEWLINE reparses as itself and lands a column-1 star the ORG parser reads as a headline.
    , testCase "a newline in either half is refused before anything is written" $
        withRecord "* one [[https://a.example][A]]\n" $ \r ->
          case subtreeLinks r of
            (l : _) -> mapM_ (\(what, target, desc) ->
                                case editLinkEdits (olSpan l) target desc r of
                                  Right edits -> assertFailure (what <> ": expected a"
                                                   <> " refusal, got " <> show edits)
                                  Left why -> assertContains what "one line" why)
                             [ ("in the target", "https://a\n* B", Nothing)
                             , ("in the description", "https://a.example",
                                Just (Just "A\n* B")) ]
            [] -> assertFailure "no link to edit"
    ]

    -- The PARSER's own charset: a character @tagsP@ declines takes the run down into title text.
  , testGroup "the tags add-tag and remove-tag take"
    [ testCase "each spelling, and whether it is one" $ mapM_ tagIs
        [ ("work", True), ("WORK", True), ("work_2", True), ("a-b", True)
        , ("@home", True), ("c#", True), ("2026", True)
        -- Org's own set carries @%@; the hyphen is the one divergence, kept for the wild corpus.
        , ("50%", True)
        , ("", False), ("two words", False), (":work:", False), ("a.b", False) ]

    , testCase "a refusal names what it turned down" $
        case tagText "a.b" of
          Right kept -> assertFailure ("expected a refusal, got " <> show kept)
          Left why   -> assertContains "names the input" "a.b" why
    ]

    -- A value that does not REPARSE is refused: the planning line would be body text.
  , testGroup "the dates set-planning takes"
    [ testCase "each spelling, and what it renders as" $ mapM_ reads'
        -- Org's own form, taken exactly as written once it reparses.
        [ ("<2026-08-05 Wed>",      "<2026-08-05 Wed>")
        , ("<2026-08-05 Wed 09:30>", "<2026-08-05 Wed 09:30>")
        , ("<2026-08-05 Wed +1w>",  "<2026-08-05 Wed +1w>")
        , ("<2026-08-05 Mon>",      "<2026-08-05 Mon>")
        , ("[2026-08-05 Wed]",      "[2026-08-05 Wed]")
        , ("2026-08-05",            "<2026-08-05 Wed>")
        , ("2026-08-05 09:30",      "<2026-08-05 Wed 09:30>")
        , ("2026-08-05 9:05",       "<2026-08-05 Wed 09:05>")
        , ("  2026-08-05  ",        "<2026-08-05 Wed>")
        , ("today",                 "<2026-08-01 Sat>")
        , ("TODAY",                 "<2026-08-01 Sat>")
        , ("tomorrow",              "<2026-08-02 Sun>")
        , ("+0d",                   "<2026-08-01 Sat>")
        , ("+3d",                   "<2026-08-04 Tue>")
        , ("+2w",                   "<2026-08-15 Sat>")
        , ("+1m",                   "<2026-09-01 Tue>")
        -- ORG'S WHOLE CHARSET: the parser reads four units, so this reader takes four.
        , ("+1y",                   "<2027-08-01 Sun>")
        -- ONE SHIFT GRAMMAR ('shiftIn', the filter's own compile): both signs,
        -- any base the field reads alone, month steps CLIPPED.
        , ("-3d",                   "<2026-07-29 Wed>")
        , ("today+3d",              "<2026-08-04 Tue>")
        , ("tomorrow+1w",           "<2026-08-09 Sun>")
        , ("*today*-7d",            "<2026-07-25 Sat>")
        , ("2026-08-01+1d",         "<2026-08-02 Sun>")
        , ("2026-01-31+1m",         "<2026-02-28 Sat>") ]

      -- Only an already-bracketed value is reparsed, so what the rest RENDER is asserted here.
    , testCase "and everything it computes reads back as a timestamp" $
        mapM_ (\text' -> case planningTimestamp today text' of
                 Left why    -> assertFailure (T.unpack text' <> " refused: " <> T.unpack why)
                 Right stamp -> assertBool (T.unpack stamp <> " does not reparse")
                                           (readsAsTimestamp stamp))
              [ "today", "tomorrow", "+3d", "+2w", "+1m", "+1y"
              , "2026-08-05", "2026-08-05 09:30", "2026-08-05 9:05" ]

      -- Derived off 'TimestampUnit', so a unit the parser gains is offered rather than missing.
    , testCase "the refusal names every relative form there is" $
        case planningTimestamp today "next tuesday" of
          Right stamp -> assertFailure ("accepted: " <> T.unpack stamp)
          Left why    -> mapM_ (\form -> assertBool (T.unpack (form <> " unnamed: " <> why))
                                                    (form `T.isInfixOf` why))
                               ["+1d", "+1w", "+1m", "+1y"]

    , testCase "and everything else is refused, by name" $ mapM_ refuses
        [ "", "   ", "next tuesday", "05/08/2026", "2026-13-01", "+3", "+3x"
        , "today+3", "18 aug+1d", "2026-8-1+1d"
        , "<not a date>", "<2026-08-05 Wed", "2026-08-05 25:00" ]

      -- THE PIN ON THE ONE SHIFT GRAMMAR: the wall reads shifts through the very
      -- 'shiftIn' the filter compiles with (spec-pinned in TestSpec via Filter's
      -- re-export), so a shifted literal resolves at the wall to the day
      -- 'shiftDay' serves off its base, clip rule and all.
    , testCase "a shifted literal lands on the day the filter's compile serves" $
        sequence_
          [ case shiftIn lit of
              Nothing -> assertFailure (T.unpack lit <> " spells no shift")
              Just (base, n, u) -> do
                b <- maybe (assertFailure (T.unpack lit <> ": unreadable base")) pure
                           (baseOf base)
                d <- maybe (assertFailure (T.unpack lit <> ": unshiftable")) pure
                           (shiftDay u n b)
                assertEqual (T.unpack lit)
                            (planningTimestamp today (isoDay d))
                            (planningTimestamp today lit)
          | lit <- [ "+3d", "-3d", "+03d", "*today*+30d", "*today*-7d"
                   , "today+3d", "tomorrow+1w", "2026-08-01+1d", "2026-01-31+1m" ] ]
    ]

    -- THE SHARED CORPUS IS THE DRIFT PIN between the two resolvers
    -- ('dateCorpusPath'): the SERVER'S answer is the truth the pane redraws
    -- from, and every vector here is owed an answer by both halves.
  , testGroup "the English dates a date-owed field takes"
    [ testCase ("every vector in " <> dateCorpusPath <> ", resolved or refused") $ do
        vectors <- dateVectors
        -- The proposal's own table, whole: 48 single dates and 18 intervals.
        assertBool ("the corpus carries the proposal's rows: " <> show (length vectors))
                   (length vectors >= 66)
        mapM_ resolvesAs vectors

      -- "Not a date" reads oddly of a phrase naming two perfectly good ones, so
      -- the inversion gets its own wording AND names the remedy.
    , testCase "an inverted interval names the inversion and its remedy" $
        refusedNaming "an inverted interval" ["ends before it starts", "spell a year"]
                      (planningTimestamp today "from 30 dec to 2 jan")

      -- Only an already-bracketed value is reparsed, so what the English forms
      -- RENDER is asserted to read back here.
    , testCase "and everything English computes reads back as a timestamp" $
        mapM_ (\text' -> case planningTimestamp today text' of
                 Left why    -> assertFailure (T.unpack text' <> " refused: " <> T.unpack why)
                 Right stamp -> assertBool (T.unpack stamp <> " does not reparse")
                                           (readsAsTimestamp stamp))
              [ "18 aug", "18 august 2027", "from 18 to 19 aug"
              , "from 30 dec 2026 to 2 jan 2027", "from 18 to 18 aug" ]

      -- ONE ANSWER PER MEANING: the collapse is what makes the grammar's two
      -- spellings of one day agree, so neither is a fixed point the other lacks.
    , testCase "the degenerate interval and the bare date land on the same bytes" $
        assertEqual "one day, two spellings"
                    (planningTimestamp today "18 aug")
                    (planningTimestamp today "from 18 to 18 aug")

      -- RET, RET, RET on one field yields one answer: the second reading meets
      -- the bracketed branch and hands the bytes straight back.
    , testCase "and a stamp it computes is a fixed point under a second reading" $
        mapM_ (\text' -> case planningTimestamp today text' of
                 Left why    -> assertFailure (T.unpack text' <> " refused: " <> T.unpack why)
                 Right stamp -> assertEqual (T.unpack text' <> ", read twice") (Right stamp)
                                            (planningTimestamp today stamp))
              [ "18 aug", "from 18 to 19 aug", "today", "+3d", "2026-08-05" ]

      -- The table is the ONLY language-bearing datum, so it is stated whole here.
    , testCase "the month table is twelve months, the short form and the full" $ do
        assertEqual "twelve months" [1 .. 12] (nub (sort (map snd monthWords)))
        assertEqual "may is ONE entry, its two forms coinciding"
                    [("may", 5)] (filter ((== 5) . snd) monthWords)
        assertEqual "and nothing outside it reads" [Nothing, Nothing, Nothing]
                    (map (`lookup` monthWords) ["sept", "aug.", "augustus"])
    ]

    -- THE BRACKET IS THE ASK FOR AN ACTIVITY, and nothing else: what is inside
    -- is the very grammar the bare field reads, so the roster and the shift and
    -- the English forms all ride in unchanged.  The corpus above carries the
    -- vectors; these are the LAWS the arm turns on.
  , testGroup "the brackets a date-owed field takes"
    [ testCase "a bracket picks the activity, and the whole grammar rides inside" $
        mapM_ reads'
          [ ("[today]",              "[2026-08-01 Sat]")
          , ("<today>",              "<2026-08-01 Sat>")
          , ("[tomorrow]",           "[2026-08-02 Sun]")
          , ("[today+3d]",           "[2026-08-04 Tue]")
          , ("<tomorrow-1w>",        "<2026-07-26 Sun>")
          , ("[+3d]",                "[2026-08-04 Tue]")
          , ("[*today*]",            "[2026-08-01 Sat]")
          , ("[18 aug]",             "[2026-08-18 Tue]")
          , ("[18 august 2027]",     "[2027-08-18 Wed]")
          , ("[ today ]",            "[2026-08-01 Sat]")
          -- ONE BRACKET KIND, BOTH HALVES: 'orgRange' takes the pair, so the
          -- interval needs no grammar of its own to wear the reader's choice.
          , ("[from 18 to 19 aug]",  "[2026-08-18 Tue]--[2026-08-19 Wed]")
          , ("<from 18 to 19 aug>",  "<2026-08-18 Tue>--<2026-08-19 Wed>") ]

      -- ACTIVE IS WHAT THE BARE FIELD ALREADY ANSWERS, so the active bracket
      -- adds nothing to it -- byte for byte, or the two spellings are two laws.
    , testCase "and the active bracket is the bare phrase's own bytes" $
        mapM_ (\phrase -> assertEqual (T.unpack phrase <> ", bracketed")
                                      (planningTimestamp today phrase)
                                      (planningTimestamp today ("<" <> phrase <> ">")))
              ["today", "tomorrow", "+3d", "today+3d", "18 aug", "18 august 2027"
              , "from 18 to 19 aug", "from 18 to 18 aug"]

      -- VERBATIM PRECEDENCE HOLDS EXACTLY: a bracket org itself reads back is
      -- never resolved for, wrong weekday and all (the law pinned above), and
      -- org reads one back with no weekday at all -- so neither grows one here.
    , testCase "org's own spelling still outranks the reading" $
        mapM_ (\stamp -> assertEqual (T.unpack stamp <> ", untouched")
                                     (Right stamp) (planningTimestamp today stamp))
              [ "[2026-08-05 Mon]", "<2026-08-05 Mon>", "[2026-08-18]"
              , "[2026-08-05 Wed 09:30]", "[2026-08-01 Sat]--[2026-08-05 Wed]" ]

      -- A BRACKET THAT READS NOTHING KEEPS THE REFUSAL IT ALREADY HAD: the arm
      -- widens what is taken and never what is said about the rest.
    , testCase "and a bracket no reading takes is refused as it was" $ mapM_ refuses
        [ "[foo]", "[]", "<>", "[   ]", "[today>", "<today]", "[18 aug"
        , "[31 feb]", "[18 aug]--[19 aug]", "<18 aug>--<19 aug>" ]

      -- The one refusal a READING spends a word of its own on travels inside the
      -- brackets with the grammar that spends it.
    , testCase "an inverted bracketed interval names the inversion and its remedy" $
        refusedNaming "a bracketed inversion" ["ends before it starts", "spell a year"]
                      (planningTimestamp today "[from 30 dec to 2 jan]")

      -- RET, RET, RET on one field yields one answer here too: what the arm
      -- writes is org's own spelling, so the second reading meets the verbatim
      -- branch and hands the same bytes back.
    , testCase "what it computes reads back, and is a fixed point" $
        mapM_ (\phrase -> case planningTimestamp today phrase of
                 Left why    -> assertFailure (T.unpack phrase <> " refused: " <> T.unpack why)
                 Right stamp -> do
                   assertBool (T.unpack stamp <> " does not reparse") (readsAsTimestamp stamp)
                   assertEqual (T.unpack phrase <> ", read twice") (Right stamp)
                               (planningTimestamp today stamp))
              [ "[today]", "<today>", "[18 aug]", "[18 august 2027]"
              , "[from 18 to 19 aug]", "[today+3d]", "[+2w]" ]
    ]

  , testGroup "set-planning"
    [ -- A reschedule is the timestamp's own span: the keyword, the spacing and
      -- every other entry on the line stay byte-identical.
      testCase "over an entry, replaces exactly that timestamp" $
        planningIs "moved" "SCHEDULED" (Just "<2026-08-09 Sun>")
          (T.unlines ["* TODO Ship it", "SCHEDULED: <2026-08-01 Sat> DEADLINE: <2026-08-05 Wed>"])
          (T.unlines ["* TODO Ship it", "SCHEDULED: <2026-08-09 Sun> DEADLINE: <2026-08-05 Wed>"])

    , testCase "with no line at all, one goes under the title line" $
        planningIs "grown" "SCHEDULED" (Just "<2026-08-09 Sun>")
          (T.unlines ["* TODO Ship it :web:", "body"])
          (T.unlines ["* TODO Ship it :web:", "SCHEDULED: <2026-08-09 Sun>", "body"])

      -- Under the TITLE line rather than at 'hsFull''s end, which here is the drawer's @:END:@.
    , testCase "and it goes above the drawer, not into it" $
        planningIs "over a drawer" "DEADLINE" (Just "<2026-08-09 Sun>")
          (T.unlines ["* TODO Ship it", ":PROPERTIES:", ":A: 1", ":END:"])
          (T.unlines ["* TODO Ship it", "DEADLINE: <2026-08-09 Sun>", ":PROPERTIES:"
                     , ":A: 1", ":END:"])

    , testCase "beside an entry the line already has, it joins the end" $
        planningIs "joined" "DEADLINE" (Just "<2026-08-09 Sun>")
          (T.unlines ["* TODO Ship it", "SCHEDULED: <2026-08-01 Sat>"])
          (T.unlines ["* TODO Ship it", "SCHEDULED: <2026-08-01 Sat> DEADLINE: <2026-08-09 Sun>"])

    , testCase "clearing the first of two closes the line up" $
        planningIs "cleared first" "SCHEDULED" Nothing
          (T.unlines ["* TODO Ship it", "SCHEDULED: <2026-08-01 Sat> DEADLINE: <2026-08-05 Wed>"])
          (T.unlines ["* TODO Ship it", "DEADLINE: <2026-08-05 Wed>"])

      -- The last entry has no trailing run to take, so the LEADING one goes; both would glue.
    , testCase "and clearing the last of two takes the space in front of it" $
        planningIs "cleared last" "DEADLINE" Nothing
          (T.unlines ["* TODO Ship it", "SCHEDULED: <2026-08-01 Sat> DEADLINE: <2026-08-05 Wed>"])
          (T.unlines ["* TODO Ship it", "SCHEDULED: <2026-08-01 Sat>"])

    , testCase "clearing the middle of three leaves its neighbours apart" $
        planningIs "cleared middle" "DEADLINE" Nothing
          (T.unlines [ "* TODO Ship it"
                     , "SCHEDULED: <2026-08-01 Sat> DEADLINE: <2026-08-05 Wed> \
                       \CLOSED: [2026-07-30 Thu]" ])
          (T.unlines [ "* TODO Ship it"
                     , "SCHEDULED: <2026-08-01 Sat> CLOSED: [2026-07-30 Thu]" ])

    , testCase "clearing the only entry takes the line with it" $
        planningIs "line dropped" "SCHEDULED" Nothing
          (T.unlines ["* TODO Ship it", "SCHEDULED: <2026-08-01 Sat>", "body"])
          (T.unlines ["* TODO Ship it", "body"])

    , testCase "but a CLOSED beside it keeps the line standing" $
        planningIs "closed stays" "SCHEDULED" Nothing
          (T.unlines ["* TODO Ship it", "CLOSED: [2026-07-30 Thu] SCHEDULED: <2026-08-01 Sat>"])
          (T.unlines ["* TODO Ship it", "CLOSED: [2026-07-30 Thu]"])

    , testCase "clearing an entry the headline never had costs no edit" $
        withRecord "* TODO Plain\n" $ \r ->
          assertEqual "no edits" (Right []) (setPlanningEdits "DEADLINE" Nothing r)

      -- ORG'S THREE, AND THE CASE IS ORG'S TOO: the span math composes
      -- @KEYWORD: STAMP@ into the line, so a word naming no entry may not reach
      -- it.  What each key's VALUE has to read as is the write door's wall, not
      -- this one's -- @CLOSED@ takes a bracket here as the other two do.
    , testCase "a keyword that names no planning entry is refused, by name" $
        withRecord "* TODO Plain\n" $ \r ->
          mapM_ (\keyword -> case setPlanningEdits keyword (Just "<2026-08-05 Wed>") r of
                   Right edits -> assertFailure (T.unpack keyword <> ": " <> show edits)
                   Left why -> assertBool (T.unpack why) (keyword `T.isInfixOf` why))
                ["scheduled", "TIMESTAMP", "CLOCK"]

    , testCase "and org's third word composes like the other two" $
        planningIs "closed set" "CLOSED" (Just "[2026-08-05 Wed]")
          (T.unlines ["* TODO Ship it"])
          (T.unlines ["* TODO Ship it", "CLOSED: [2026-08-05 Wed]"])
    ]

  , testGroup "capture"
    [ -- The insertion is at the END, so a file that already holds work keeps
      -- every byte of it exactly where it was.
      testCase "appends a top entry with its creation time in a drawer" $
        assertEqual "the entry, and the file under it"
          (Right (T.unlines [ "* TODO old", "* TODO Buy milk :errands:", ":PROPERTIES:"
                            , ":ORG_GLANCE_CREATION_TIME: [2026-08-01 Sat 09:30]", ":END:" ]))
          (captured "* TODO old\n" "TODO Buy milk :errands:")

    , testCase "into a file that is not there yet, the entry is the file" $
        assertEqual "no leading blank"
          (Right (T.unlines [ "* read the docs", ":PROPERTIES:"
                            , ":ORG_GLANCE_CREATION_TIME: [2026-08-01 Sat 09:30]", ":END:" ]))
          (captured "" "read the docs")

      -- Appended bare to a file with no closing newline, the stars would be no headline at all.
    , testCase "a file not closed with a newline gets one first" $
        assertEqual "the newline is the first thing written"
          (Right (T.unlines [ "tail", "* note", ":PROPERTIES:"
                            , ":ORG_GLANCE_CREATION_TIME: [2026-08-01 Sat 09:30]", ":END:" ]))
          (captured "tail" "note")

    , testCase "the text is stripped and is otherwise raw org" $
        assertContains "written as spelled" "* [#A] TODO ship :web:\n"
          (fromRight "" (captured "" "  [#A] TODO ship :web:  "))

    , testCase "into a CRLF file, the entry is CRLF too" $
        assertEqual "every line the target's own ending"
          (Right (T.intercalate "\r\n"
                    [ "* old", "* note", ":PROPERTIES:"
                    , ":ORG_GLANCE_CREATION_TIME: [2026-08-01 Sat 09:30]", ":END:", "" ]))
          (captured "* old\r\n" "note")

    , testCase "an empty line and a multi-line one are refused" $
        mapM_ (\(what, text') -> case captured "" text' of
                 Right doc -> assertFailure (what <> ": wrote " <> show doc)
                 Left _why -> pure ())
              [ ("empty", ""), ("blank", "   ")
              , ("two headlines", "one\n* two"), ("a body line", "one\nbody") ]

      -- Org's INACTIVE form: a creation time is a record rather than agenda work.
    , testCase "the stamp is org's inactive timestamp, to the minute" $
        assertEqual "as org-glance's own store spells it"
                    "[2026-08-01 Sat 09:30]" (captureStamp stampedAt)

    , testCase "and org reads the stamp back" $
        assertBool "the creation stamp reparses" (readsAsTimestamp (captureStamp stampedAt))
    ]

    -- ORG'S PRIORITY TOKEN, which a key CYCLES; every case asserts the whole document.
  , testGroup "set-priority"
    [ testCase "over a token already there, replaces exactly it" $
        setPriorityIs "replaced" (keyworded "* NEXT [#A] Ship it :web:\n") (Just "B")
                                 (keyworded "* NEXT [#B] Ship it :web:\n")

    , testCase "with none, inserts behind the keyword" $
        setPriorityIs "after the keyword" (keyworded "* NEXT Ship it\n") (Just "A")
                                          (keyworded "* NEXT [#A] Ship it\n")

    , testCase "and behind the stars where there is no keyword" $
        setPriorityIs "after the stars" "* Ship it\n" (Just "C") "* [#C] Ship it\n"

    , testCase "a null takes the token and the space behind it" $
        setPriorityIs "cleared" (keyworded "* NEXT [#A] Ship it\n") Nothing
                                (keyworded "* NEXT Ship it\n")

    , testCase "and the whole run of it, however wide" $
        setPriorityIs "cleared wide" "* [#A]   Ship it\n" Nothing "* Ship it\n"

    , testCase "clearing a headline that carries none costs no edit" $
        withRecord "* Plain\n" $ \r ->
          assertEqual "no edits" (Right []) (setPriorityEdits Nothing r)

    , testCase "the letter is uppercased and stripped" $
        setPriorityIs "folded" "* Plain\n" (Just "  b  ") "* [#B] Plain\n"

    , testCase "anything that is not one letter is refused, by name" $
        mapM_ (\(text', wanted) ->
                 assertEqual (show text') wanted (isRight (priorityText text')))
              [ ("A", True), ("c", True), ("D", True), ("", False)
              , ("AB", False), ("1", False), ("[#A]", False) ]
    ]

    -- The one CELL a reader edits as text; every case asserts the whole document.
  , testGroup "set-title"
    [ testCase "replaces exactly the title, between the keyword and the tags" $
        setTitleIs "replaced" (keyworded "* NEXT [#A] Ship it :web:\n") "Ship it now"
                              (keyworded "* NEXT [#A] Ship it now :web:\n")

    , testCase "over a bare title" $
        setTitleIs "bare" "* Plain\n" "Renamed" "* Renamed\n"

      -- 'titleLineEnd' cannot serve: its answer includes the TAGS, so a title would read as tag text.
    , testCase "a headline with no title grows one behind its priority" $
        setTitleIs "after the priority" "* TODO [#B]\n" "Ship it"
                                        "* TODO [#B] Ship it\n"

    , testCase "and behind its keyword where it has no priority" $
        setTitleIs "after the keyword" "* TODO\n" "Ship it" "* TODO Ship it\n"

      -- With neither in front the separator is the run org already writes after the stars.
    , testCase "and past the stars' own space where it has neither" $
        setTitleIs "after the stars" "* \nSCHEDULED: <2026-08-05 Wed>\n" "Ship it"
                                     "* Ship it\nSCHEDULED: <2026-08-05 Wed>\n"

      -- The parser hands @* TODO :web:@ its colons as the TITLE, so the tags branch is unreachable.
    , testCase "a run of colons with no title in front of it IS the title" $
        setTitleIs "the colons were the title" "* TODO :web:\n" "Ship it"
                                               "* TODO Ship it\n"

    , testCase "the text is stripped" $
        setTitleIs "stripped" "* Plain\n" "   Renamed  " "* Renamed\n"

      -- BY NAME: a non-empty refusal says only that something refused, so each rule names itself.
    , testCase "an empty title and a multi-line one are refused, by name" $
        withRecord "* Plain\n" $ \r ->
          mapM_ (\(what, text', named) -> case setTitleEdits text' r of
                   Right edits -> assertFailure (what <> ": " <> show edits)
                   Left why    -> assertBool (what <> ": " <> T.unpack why)
                                             (named `T.isInfixOf` why))
                [ ("empty", "", "needs a title")
                , ("blank", "   ", "needs a title")
                , ("two lines", "one\ntwo", "is one line") ]

    , testCase "the wall is titleText, and the route reads the same one" $
        mapM_ (\(text', wanted) ->
                 assertEqual (show text') wanted (isRight (titleText text')))
              [ ("Ship it", True), ("  padded  ", True), ("", False)
              , ("   ", False), ("one\ntwo", False) ]
    ]
  ]

-- | WHAT: DOC with @set-planning KEYWORD STAMP@ on its one headline is WANTED.
planningIs :: String -> Text -> Maybe Text -> Text -> Text -> Assertion
planningIs what keyword stamp doc = triedEditsAre what doc (setPlanningEdits keyword stamp)

-- | The day every relative date is worked out from and every capture stamped with.
today :: Time.Day
today = Time.fromGregorian 2026 8 1

stampedAt :: Time.ZonedTime
stampedAt = Time.ZonedTime (Time.LocalTime today (Time.TimeOfDay 9 30 0)) Time.utc

-- | WHAT: TEXT reads as the timestamp WANTED, against 'today'.
reads' :: (Text, Text) -> Assertion
reads' (text', wanted) =
  assertEqual (T.unpack text') (Right wanted) (planningTimestamp today text')

-- | WHAT: TEXT is no date at all, and the refusal says so by naming it.
refuses :: Text -> Assertion
refuses text' = case planningTimestamp today text' of
  Right stamp -> assertFailure (show text' <> ": read as " <> T.unpack stamp)
  Left why    -> assertBool (T.unpack why) ("is not a date" `T.isInfixOf` why)

-- | The day a shift BASE names, restated for the pin.
baseOf :: Text -> Maybe Time.Day
baseOf b | T.null b || b `elem` ["today", "*today*"] = Just today
         | b == "tomorrow"                           = Just (Time.addDays 1 today)
         | otherwise                                 = dayOf b

-- | 'dateCorpus' with its reference column read as a day: the shared loader
-- carries the ISO text, and the wall under test here is asked in 'Time.Day'.
dateVectors :: IO [(Text, Time.Day, Either Text Text)]
dateVectors = mapM asked =<< dateCorpus
  where
    asked (typed, iso, owed) =
      maybe (assertFailure ("not an ISO day: " <> T.unpack iso))
            (\day -> pure (typed, day, owed))
            (dayOf iso)

-- | One corpus vector, asserted through the wall every date-owed surface shares.
resolvesAs :: (Text, Time.Day, Either Text Text) -> Assertion
resolvesAs (typed, day, Right stamp) =
  assertEqual (T.unpack typed) (Right stamp) (planningTimestamp day typed)
resolvesAs (typed, day, Left word) =
  refusedNaming (show typed) [word] (planningTimestamp day typed)

-- | DOC with TEXT captured into it, at 'stampedAt'.
captured :: Text -> Text -> Either Text Text
captured doc text' = splice doc <$> captureEdits doc (captureStamp stampedAt) text'

-- | WHAT: DOC with @set-priority LETTER@ applied to its one headline is WANTED.
setPriorityIs :: String -> Text -> Maybe Text -> Text -> Assertion
setPriorityIs what doc letter = triedEditsAre what doc (setPriorityEdits letter)

-- | WHAT: DOC with @set-title TITLE@ applied to its one headline is WANTED.
setTitleIs :: String -> Text -> Text -> Text -> Assertion
setTitleIs what doc title = triedEditsAre what doc (setTitleEdits title)

-- Subtree entries: which headlines are inside a row's subtree, how they are
-- numbered, what each hangs under and where each extent runs — all out of a
-- re-parse, asserted to agree with the load.

-- | An outline with a level jump in it, so the parent rule is more than one level up.
deep :: Text
deep = T.unlines
  [ "* TODO one :top:"
  , "body of one"
  , "** two"
  , "*** three"
  , "body of three"
  , "** four"
  , "* five"
  ]

-- | Run K over the FIRST row of DOC and the entries inside it.
withEntries :: Text -> (HeadlineRecord -> [SubtreeEntry] -> Assertion) -> Assertion
withEntries doc k = withRecordsOf doc $ \records -> case records of
  (r : _rest) -> k r (subtreeEntries noConfig r)
  []          -> assertFailure "expected at least one row"

entrySpec :: TestTree
entrySpec = testGroup "Subtree entries"
  [ testCase "every headline inside the subtree, in document order" $
      withEntries deep $ \_r entries ->
        assertEqual "the row's own is not among them"
                    ["two", "three", "four"] (map (hrTitle . seRecord) entries)

  , testCase "each one's level, as org spells it" $
      withEntries deep $ \_r entries ->
        assertEqual "the stars counted" [2, 3, 2] (map seLevel entries)

    -- The nearest SHALLOWER entry, which is what a level jump needs.
  , testCase "each one's parent is the nearest shallower entry" $
      withEntries deep $ \_r entries ->
        assertEqual "-1 is the row itself" [-1, 0, -1] (map seParent entries)

  , testCase "each one's extent is its own subtree" $
      withEntries deep $ \_r entries ->
        assertEqual "two carries three, three carries its body, four is one line"
          [ "** two\n*** three\nbody of three\n"
          , "*** three\nbody of three\n"
          , "** four\n" ]
          (map (subtreeText . seRecord) entries)

    -- The lens over a child is the lens: three regions out of the child's own extent.
  , testCase "a child materializes through the same lens the row does" $
      withEntries (T.unlines [ "* one", "** two", "SCHEDULED: <2026-08-05 Wed>"
                             , ":PROPERTIES:", ":EFFORT: 0:30", ":END:"
                             , "body of two" ]) $ \_r entries ->
        case entries of
          (e : _rest) -> do
            let parts = headlineParts (seRecord e)
            assertEqual "the body, both regions lifted out"
                        "** two\nbody of two\n" (hpBody parts)
            assertEqual "the drawer" [("EFFORT", "0:30")] (hpProperties parts)
            assertEqual "and the planning line"
                        [("SCHEDULED", "<2026-08-05 Wed>")] (hpPlanning parts)
          [] -> assertFailure "expected one entry"

  , testCase "and decompose then recompose is the identity on it" $
      withEntries deep $ \_r entries ->
        mapM_ (\e -> let rec' = seRecord e
                     in assertEqual (T.unpack (hrTitle rec'))
                                    (subtreeText rec')
                                    (recomposedSubtree rec' (headlineParts rec')))
              entries

    -- The digest is the FILE's: one file, one lock, whichever entry the sheet is on.
  , testCase "a child pins the file's own digest" $
      withEntries deep $ \r entries ->
        assertEqual "the row's" (replicate (length entries) (hrDigest r))
                    (map (hrDigest . seRecord) entries)

  , testCase "a child's id is the row's with its index behind it" $
      withEntries deep $ \r entries ->
        assertEqual "row/K" [ hrId r <> "/" <> T.pack (show k) | k <- [0 :: Int, 1, 2] ]
                    (map (hrId . seRecord) entries)

  , testCase "the index is what addresses one, and it is bounds-checked" $
      withEntries deep $ \_r entries -> do
        assertEqual "in range" (Just "three")
                    (hrTitle . seRecord <$> subtreeEntryAt entries 1)
        assertEqual "past the end" Nothing (hrTitle . seRecord <$> subtreeEntryAt entries 3)
        assertEqual "and below it" Nothing (hrTitle . seRecord <$> subtreeEntryAt entries (-1))

  , testCase "a row with no children has no entries" $
      withEntries "* one\nbody\n" $ \_r entries ->
        assertEqual "none" 0 (length entries)

  , testCase "a child's cells are read the way a row's are" $
      withEntries (keyworded (T.unlines ["* one", "** NEXT [#B] two :web:x:"])) $
        \_r entries -> case entries of
          (e : _rest) -> do
            let rec' = seRecord e
            assertEqual "state" (Just "NEXT") (hrState rec')
            assertEqual "priority" (Just "[#B]") (hrPriority rec')
            assertEqual "title" "two" (hrTitle rec')
            assertEqual "tags" ":web:x:" (hrTags rec')
          [] -> assertFailure "expected one entry"
  ]

-- | CAPTURE: the template grammar, the blob a tagged capture composes, the store layout.
captureSpec :: TestTree
captureSpec = testGroup "Capture"
  [ testGroup "The expansion subset"
      [ testCase "%? is where the typed line lands" $
          assertEqual "the point" (Right "* milk") (expanded [] "milk" "* %?")

      , testCase "%U and %T are the two brackets of one moment" $ do
          assertEqual "inactive" (Right "* x [2026-08-04 Tue 09:30]")
                      (expanded [] "x" "* %? %U")
          assertEqual "active" (Right "* x <2026-08-04 Tue 09:30>")
                      (expanded [] "x" "* %? %T")

      , testCase "a template spelling one twice stamps one moment" $
          assertEqual "one clock read per request"
                      (Right "* x [2026-08-04 Tue 09:30] [2026-08-04 Tue 09:30]")
                      (expanded [] "x" "* %? %U %U")

      , testCase "%^{PROMPT} takes its answer out of the fields" $
          assertEqual "the answer, verbatim" (Right "* x\n:AUTHOR: Frank Herbert")
                      (expanded [("Author", "Frank Herbert")] "x" "* %?\n:AUTHOR: %^{Author}")

      , testCase "a prompt spelled twice is one ask and two fills" $ do
          assertEqual "asked once" ["Author"] (templatePrompts "* %? %^{Author} %^{Author}")
          assertEqual "filled twice" (Right "* x a a")
                      (expanded [("Author", "a")] "x" "* %? %^{Author} %^{Author}")

        -- EVERYTHING ELSE COPIES THROUGH, so a template using an unknown code stays readable.
      , testCase "a code outside the subset is written as it stands" $
          mapM_ (\template ->
                   assertEqual (T.unpack template) (Right ("* x" <> T.drop 4 template))
                               (expanded [] "x" template))
                [ "* %?%a", "* %?%^g", "* %?%^{unclosed", "* %?%%", "* %?%" ]

      , testCase "and a % that opens nothing is a %" $
          assertEqual "trailing" (Right "* x %") (expanded [] "x" "* %? %")

        -- THE LIST AND THE GRAMMAR ARE TWO SPELLINGS: the scan never consults 'captureCodes'.
      , testCase "every advertised code is one the scan expands" $
          mapM_ (\(code, _means) ->
                   assertBool (T.unpack code <> " copied through as itself")
                              (either (const False) (not . T.isInfixOf code)
                                      (expanded [("PROMPT", "answered")] "typed"
                                                ("* %?" <> code))))
                captureCodes
      ]

  , testGroup "What a template cannot do"
      [ testCase "a template with no %? is refused, naming what it lacks" $
          assertEqual "nowhere for the line to go"
                      (Left "this capture template has no %?, so there is nowhere for the text to go")
                      (expanded [] "milk" "* nothing here")

      , testCase "an ask nobody answered is refused, naming the prompt" $
          assertBool "the prompt is named"
                     (either (T.isInfixOf "Author") (const False)
                             (expanded [] "x" "* %? %^{Author}"))

      , testCase "one unanswered ask refuses the whole expansion" $
          assertBool "refused"
                     (either (const True) (const False)
                             (expanded [("A", "a")] "x" "* %? %^{A} %^{B}"))

      , testCase "and a template that expands to no headline stores nothing" $
          assertBool "no entry to key by an id"
                     (either (T.isInfixOf "no headline") (const False)
                             (blobDocument (BlobSeed "book" "i" "[s]") "not a headline"))
      ]

  , testGroup "Where a template lives"
      [ testCase "the first heading of a layer file, to the end of it" $
          assertEqual "everything under the heading is the template"
                      (Just "* Book\n*** Notes\n    %?")
                      (captureTemplateOf bookLayer)

      , testCase "a file with no heading has none" $
          assertEqual "pragmas alone" Nothing (captureTemplateOf "#+TITLE: Book\n#+TODO: A | B\n")

        -- The tag's own layer first, the system layer's next, and nothing after that.
      , testCase "the tag's layer beats the system layer" $ do
          assertEqual "the tag's own" (Just "* Book\n*** Notes\n    %?")
                      (captureTemplateIn "book" layers)
          assertEqual "and the system layer answers for a tag with no template"
                      (Just "* %? %U") (captureTemplateIn "film" layers)
          assertEqual "as it does for a tag with no layer at all"
                      (Just "* %? %U") (captureTemplateIn "nosuch" layers)

      , testCase "a tag is resolved folded, like every other tag here" $
          assertEqual "Book is book" (captureTemplateIn "book" layers)
                      (captureTemplateIn "BOOK" layers)
      ]

  , testGroup "Editing a template"
        -- The file's own last newline is OUTSIDE the extent, so a replacement leaves it.
      [ testCase "a template already there is replaced where it stands" $
          assertEqual "the pragmas above it keep their bytes"
                      (Right "#+TITLE: Book\n#+TODO: TODO | DONE\n\n* %? %U\n")
                      (templated bookLayer "* %? %U")

      , testCase "a file with none takes it at the end" $
          assertEqual "appended" (Right "#+TITLE: Book\n* %?\n")
                      (templated "#+TITLE: Book\n" "* %?")

      , testCase "an empty value takes the heading and everything under it" $
          assertEqual "the pragmas survive" (Right "#+TITLE: Book\n#+TODO: TODO | DONE\n\n")
                      (templated bookLayer "")

      , testCase "and clearing a file that has none costs no edit" $
          assertEqual "no edit" (Right []) (captureTemplateEdits "#+TITLE: Book\n" "")

      , testCase "a template that is not one top entry is refused" $
          mapM_ (\want -> assertBool (T.unpack want)
                            (either (const True) (const False)
                                    (captureTemplateEdits bookLayer want)))
                ["body text", "** %?", "*%?", "  * %?"]

      , testCase "read then written back leaves the file alone" $
          assertEqual "byte for byte" (Right bookLayer)
                      (templated bookLayer (fromMaybe "" (captureTemplateOf bookLayer)))
      ]

  , testGroup "The blob a tagged capture composes" $
      [ testCase "the tag joins the headline and the drawer carries the id" $
          assertEqual "org-glance's own two properties"
                      (Right (T.unlines [ "* milk :book:"
                                        , ":PROPERTIES:"
                                        , ":ORG_GLANCE_ID: i-1"
                                        , ":ORG_GLANCE_CREATION_TIME: [2026-08-04 Tue 09:30]"
                                        , ":END:" ]))
                      (blobDocument (BlobSeed "book" "i-1" "[2026-08-04 Tue 09:30]") "* milk")
      ] ++
      [ testCase name $ assertEqual msg (Right (T.unlines want)) (blobDocument seed template)
      | (name, msg, template, want) <- blobCases ] ++
      [ testCase "and one wearing others joins the run's end" $
          assertBool "appended to the run"
                     (either (const False) (T.isInfixOf "* milk :web:book:")
                             (blobDocument seed "* milk :web:"))

      , testCase "the template's children are the entry's" $
          assertBool "the child survives"
                     (either (const False) (T.isInfixOf "*** Notes")
                             (blobDocument seed "* Book\n*** Notes\n    milk"))

      , testCase "the document ends in a newline" $
          assertBool "an org file's last line is ended"
                     (either (const False) (T.isSuffixOf "\n")
                             (blobDocument seed "* milk"))
      ]

  , testGroup "Where a blob sits"
      [ testCase "sharded by the id's first two characters, verbatim" $
          assertEqual "org-glance's own layout"
                      "/o/.org-glance/data/04/a14d10-41c1-4a3d/data.org"
                      (blobPathIn "/o/.org-glance" "04a14d10-41c1-4a3d")

        -- NOT FOLDED: an id is an opaque string wherever it is read.
      , testCase "the shard is not folded" $
          assertEqual "Password- shards under Pa"
                      "/o/.org-glance/data/Pa/ssword-1/data.org"
                      (blobPathIn "/o/.org-glance" "Password-1")

      , testCase "an id of two characters or fewer is not sharded" $ do
          assertEqual "two" "/s/data/ab/data.org" (blobPathIn "/s" "ab")
          assertEqual "three" "/s/data/ab/c/data.org" (blobPathIn "/s" "abc")

      , testCase "the store root is the served root's own" $
          assertEqual "one tree, one store" "/o/.org-glance" (storeRootIn "/o")

        -- A blob path is one this walk COLLECTS, which is what makes the row arrive.
      , testCase "a composed path is a blob the walk keeps" $
          assertBool "walked and not derived"
                     (documentPath (blobPathIn (storeRootIn "/o") "abcdef")
                        && not (derivedPath (blobPathIn (storeRootIn "/o") "abcdef")))
      ]

  , testGroup "The id it is keyed by"
      [ testCase "a version-4 UUID, lowercase, 8-4-4-4-12" $
          assertEqual "the shape org-id-uuid writes"
                      "00010203-0405-4607-8809-0a0b0c0d0e0f"
                      (uuidFrom (BS.pack [0 .. 15]))

      , testCase "the version and variant nibbles are stamped whatever the bytes" $ do
          assertEqual "all ones" "ffffffff-ffff-4fff-bfff-ffffffffffff"
                      (uuidFrom (BS.replicate 16 0xff))
          assertEqual "all zeros" "00000000-0000-4000-8000-000000000000"
                      (uuidFrom (BS.replicate 16 0))

      , testCase "and a short string is padded rather than answering short" $
          assertEqual "36 characters either way" 36 (T.length (uuidFrom BS.empty))

      , testCase "a minted id is one of those" $ do
          ident <- mintBlobId
          assertEqual "36 characters" 36 (T.length ident)
          assertEqual "four hyphens" [8, 4, 4, 4, 12] (map T.length (T.splitOn "-" ident))
          assertBool "and no two are the same"
            . (\ids -> length (nub ids) == 8) =<< replicateM 8 mintBlobId
      ]
  ]
  where
    -- ONE clock for every expansion case, so a stamp is an assertion rather than a target.
    noon = Time.ZonedTime (Time.LocalTime (Time.fromGregorian 2026 8 4)
                                          (Time.TimeOfDay 9 30 0))
                          Time.utc
    expanded answers text = expandTemplate noon answers text
    -- The module's own splice: an oracle sharing the write engine would agree with a wrong offset.
    templated doc want = splice doc <$> captureTemplateEdits doc want
    bookLayer = "#+TITLE: Book\n#+TODO: TODO | DONE\n\n* Book\n*** Notes\n    %?\n"
    -- ONE seed for every blob case, so the tag and the id are the constants they are.
    seed = BlobSeed "book" "i-1" "[s]"
    layers =
      [ ConfigLayerFile "/o/.org-glance/config/system.org" Nothing "s" "#+TITLE: X\n* %? %U\n"
      , ConfigLayerFile "/o/.org-glance/config/tags/book.org" (Just "book") "b" bookLayer
      , ConfigLayerFile "/o/.org-glance/config/tags/film.org" (Just "film") "f" "#+TODO: A | B\n"
      ]

-- | A capture template, and the blob composed from it under the tagged seed:
-- the tag on the headline, org-glance's own two properties in the drawer.
blobCases :: [(String, String, Text, [Text])]
blobCases =
    -- A template carrying a drawer keeps it; the two properties join it.
  [ ( "a drawer the template wrote is joined rather than doubled", "one drawer"
    , "* milk\n:PROPERTIES:\n:AUTHOR: X\n:END:\n"
    , [ "* milk :book:", ":PROPERTIES:", ":ORG_GLANCE_ID: i-1"
      , ":ORG_GLANCE_CREATION_TIME: [s]", ":AUTHOR: X", ":END:" ] )
  , ( "a headline already wearing the tag costs no edit", "the run keeps its bytes"
    , "* milk :book:web:"
    , [ "* milk :book:web:", ":PROPERTIES:", ":ORG_GLANCE_ID: i-1"
      , ":ORG_GLANCE_CREATION_TIME: [s]", ":END:" ] )

    -- Spliced between the headline and its @SCHEDULED:@, the planning line becomes body text.
  , ( "a template with a planning line keeps it under the title"
    , "planning first, drawer second", "* milk\nSCHEDULED: <2026-08-10 Mon>\n"
    , [ "* milk :book:", "SCHEDULED: <2026-08-10 Mon>", ":PROPERTIES:"
      , ":ORG_GLANCE_ID: i-1", ":ORG_GLANCE_CREATION_TIME: [s]", ":END:" ] )

    -- A blob is composed whole, so no untouched bytes weigh the trailing-space rule.
  , ( "a template's trailing runs do not reach the file"
    , "every line ends at its last word", "* milk  \na note \t\n    indented survives  \n"
    , [ "* milk :book:", ":PROPERTIES:", ":ORG_GLANCE_ID: i-1"
      , ":ORG_GLANCE_CREATION_TIME: [s]", ":END:", "a note", "    indented survives" ] )
  ]
