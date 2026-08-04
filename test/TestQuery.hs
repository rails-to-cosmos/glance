-- | The facade under test.  Everything here goes through 'Glance.Query': the
-- module imports no parser internals, so a wire shape that needs one fails to
-- compile instead of failing a renderer.
module TestQuery (spec) where

import Control.Concurrent (getNumCapabilities, rtsSupportsBoundThreads)
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
import TestDefaults ( assertContains, columnKeysOf, columnOf, entryAs, field, listAt
                    , orgFile, textAt, viewDir, withDoc, withTempDirNamed )

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Time as Time

import Glance.Query ( ConfigLayerFile (..), ConfigLayers (..), HeadlineParts (..)
                    , HeadlineRecord (..)
                    , LinkShape (..), LoadFailure (..), OrgLink (..)
                    , QueryResult (..), Span (..), SubtreeEntry (..)
                    , TodoKeywords (..), addTagEdits
                    , archiveEdits, archived
                    , BlobSeed (..), blobDocument, blobPathIn
                    , captureCodes, captureEdits, captureStamp, captureTemplateEdits
                    , captureTemplateIn, captureTemplateOf
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

-- | Run K over the records DOC alone makes.
withRecordsOf :: Text -> ([HeadlineRecord] -> Assertion) -> Assertion
withRecordsOf = withDoc "view" "tree.org"

-- | Run K over the view DOC alone makes.
withViewOf :: Text -> (Value -> Assertion) -> Assertion
withViewOf doc k = withRecordsOf doc (k . viewJSON viewTitle)

-- | An outline with a level at every depth: a root with a child and a
-- grandchild, a second child under the same root, and a second root.  Two rows
-- come out of it, the golden's fixture being flat.
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

-- | L as the pair the DISPLAY rule answers with: where it points and what it
-- shows.  The span is the other half of a link and has cases of its own, so a
-- case about the grammar reads through this and says nothing about offsets.
linkPair :: OrgLink -> (Text, Text)
linkPair l = (olTarget l, linkShown l)

-- | The links TEXT holds, as those pairs.
shown :: Text -> [(Text, Text)]
shown = map linkPair . orgLinks

-- | The links TEXT holds, each cut back out of TEXT by its own span — which is
-- the whole claim a span makes.
spelled :: Text -> [Text]
spelled text' = [ cut text' (olSpan l) | l <- orgLinks text' ]

-- | The half-open char span SP of TEXT.
cut :: Text -> Span -> Text
cut text' sp = T.take (spanEnd sp - spanStart sp) (T.drop (spanStart sp) text')

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
  [ loadSpec, walkSpec, levelSpec, blankSpec, parallelSpec, cellSpec, searchSpec
  , linkSpec
  , viewSpec, schemaSpec, commandSpec, lensSpec, entrySpec, captureSpec ]

-- | Where a row points: what @GET \/links@ serves, as the pure function under
-- it.
--
-- The rule is the DISPLAY rule, so the two halves are stated together — what
-- 'displayText' shows for a link is what a link's description is here, and the
-- one parser answers both.  Everything else is the plain-URL half, which the
-- display rule never had to have an opinion about.
linkSpec :: TestTree
linkSpec = testGroup "Links"
  [ testCase "a bracket link is its target and what it shows" $ do
      assertEqual "described" [("https://x/y", "table-view")]
                  (shown "[[https://x/y][table-view]]")
      -- The two spellings with no description of their own fall back to the
      -- target, which is exactly what the table shows for them.
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
      -- And the scan carries on past it: an unclosed `[[' is two characters
      -- skipped rather than the end of the text.
      assertEqual "and the one after it survives"
                  [("https://x.org", "https://x.org")] (shown "[[oops https://x.org")

  , testCase "a row with nothing to follow has no links" $
      assertEqual "none" [] (shown "* TODO plain headline\nwith a body\n")

    -- EVERY LINK CARRIES ITS SPAN, which is the half-open CHAR range it occupies
    -- in the text scanned — what `/links' hands out and what `edit-link' splices
    -- back.  Read as a slice here, so what is asserted is that the range cuts
    -- the link out of the very text it was found in.
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

    -- A target spelled twice is ONE entry, and the entry is the FIRST spelling:
    -- its description and its SPAN.  So an edit made through a deduplicated link
    -- edits the first one, and the others go on pointing where they did.
  , testCase "one entry per target, keeping the first description and its span" $ do
      assertEqual "deduped" [("https://x.org", "first")]
                  (shown "[[https://x.org][first]] and [[https://x.org][second]]")
      assertEqual "the span is the first spelling's"
        ["[[https://x.org][first]]"]
        (spelled "[[https://x.org][first]] and [[https://x.org][second]]")

  , testCase "the subtree is what is read, body and children included" $
      withRecordsOf (T.unlines
        [ "* parent [[https://a.example][A]]"
        , "body [[https://b.example][B]]"
        , "** child https://c.example" ]) $ \recs ->
        assertEqual "the whole extent"
          [[ ("https://a.example", "A"), ("https://b.example", "B")
           , ("https://c.example", "https://c.example") ]]
          (map (map linkPair . subtreeLinks) recs)

    -- A ROW'S SPANS ARE THE DOCUMENT'S.  The scan runs over the subtree slice
    -- and every span is shifted by where that slice starts, so the range is one
    -- `Data.Org.Edit' can splice — asserted by cutting the link back out of the
    -- whole file rather than out of the subtree.
  , testCase "a row's link spans are offsets into the document it was read from" $
      withRecordsOf (T.unlines
        [ "* first", "nothing here", "* second [[https://a.example][A]]"
        , "body https://b.example" ]) $ \recs ->
        assertEqual "each cut out of the file itself"
          [[], ["[[https://a.example][A]]", "https://b.example"]]
          [ [ cut (hrDoc r) (olSpan l) | l <- subtreeLinks r ] | r <- recs ]

    -- `hrLinks' is the same subtree read for a narrower question: which ROWS it
    -- points at.  A URL is not one of them, which is what keeps the field small
    -- enough to carry on every record — the corpus writes 4.5k row references
    -- against 4.1k `file:'/`http' links, and only the first kind is kept.
  , testCase "a row's links are the references its subtree carries" $
      withRecordsOf (T.unlines
        [ "* parent [[org-glance-visit:alpha][A]]"
        , "body https://b.example and [[org-glance-overview:tag][a tag]]"
        , "** child [[org-glance-open:beta][B]]" ]) $ \recs ->
        -- The child's reference is the parent's, the URL and the overview link
        -- are nobody's, and the two id protocols answer alike.
        assertEqual "the references alone" [["alpha", "beta"]] (map hrLinks recs)

    -- 'hrLinked' is the WIDER question the same scan answers: is there anywhere
    -- to go from this row, which is what @o@ follows and what the title's
    -- underline says.  A URL is a link and no reference at all, so the two
    -- fields disagree on most linked rows — ~/sync at 2026-08-02 carries 4976
    -- linked rows and 1824 referencing ones.
  , testCase "a row whose only link is a URL is linked and references nothing" $
      withRecordsOf "* plain\nsee https://x.example for the rest\n" $ \recs -> do
        assertEqual "linked" [True] (map hrLinked recs)
        assertEqual "and pointing at no row" [[]] (map hrLinks recs)

  , testCase "a reference is a link too, so a referencing row is linked" $
      withRecordsOf "* plain\n[[org-glance-visit:alpha][A]]\n" $ \recs ->
        assertEqual "both" [(True, ["alpha"])] (map (\r -> (hrLinked r, hrLinks r)) recs)

  , testCase "and a row with nothing to follow is not linked" $
      withRecordsOf "* plain\njust prose, no link in it\n" $ \recs ->
        assertEqual "nowhere to go" [False] (map hrLinked recs)

    -- THE TYPE, which is one rule read off the target's PREFIX: the scheme,
    -- lowercased, with the whole `org-glance-' family folded into one word.  The
    -- six the popup declares badges for are the ones the corpus spells; none of
    -- them is named in the function, and that is the point — they fall out of
    -- the scheme.
  , testCase "a link's type is its scheme, folded" $ do
      let types = map linkType
      assertEqual "the six the corpus spells"
        ["https", "http", "mailto", "id", "file"]
        (types [ "https://x.example/a", "http://x.example", "mailto:t@x.org"
               , "id:E1B2", "file:notes.org" ])
      -- Every org-glance protocol is ONE type.  The four that name a row and the
      -- two that name a tag or a keyword are the same KIND of destination, and
      -- which of them points at a row is `refPrefixes'' different question.
      assertEqual "and every org-glance protocol is one word"
        ["glance", "glance", "glance", "glance"]
        (types [ "org-glance-visit:E1", "org-glance-open:E1"
               , "org-glance-material:E1", "org-glance-overview:book" ])

  , testCase "a scheme this has never seen travels under its own name" $
      assertEqual "the word itself" ["ftp", "doi", "gopher", "denote"]
        (map linkType ["ftp://x.example", "doi:10.1/2", "gopher://x", "denote:2026"])

    -- The case of a scheme is not part of it, and org files spell one either
    -- way.  Folded here so the popup's badge and `followable' both answer about
    -- the same word.
  , testCase "the scheme is folded, so a shouted URL is still followable" $
      assertEqual "lowercased" ["https", "mailto"]
                  (map linkType ["HTTPS://X.EXAMPLE", "MailTo:t@x.org"])

    -- ONE catch-all, reached two ways: no colon at all, and a word before the
    -- colon that is not scheme-SHAPED — a leading digit, an empty word, a space
    -- in it — RFC 3986 opening a scheme with a letter.
  , testCase "a target with no scheme is other, internal links included" $
      assertEqual "nothing to read"
        (replicate 8 "other")
        (map linkType [ "Some Headline", "*Some Headline", "./notes.org", "/etc/hosts"
                      , "2026:review", ":leading", "a b:c", "" ])

    -- The honest cost of reading the PREFIX and nothing else.  Org's internal
    -- links name a place inside the tree rather than a protocol and a relative
    -- path says nothing about being a file, so both are `other' above — and a
    -- scheme-SHAPED word before a colon IS the type here, whether or not the
    -- author meant a protocol.  A registry of known schemes is the rule this
    -- deliberately is not: an unlisted scheme would then read as prose.
  , testCase "and a scheme-shaped word in prose is taken at its word" $
      assertEqual "the word before the colon" ["meeting", "todo"]
                  (map linkType ["Meeting: notes", "TODO:tomorrow"])

    -- The VOCABULARY and the DERIVER agree: every value the popup's badge column
    -- declares a hue for is a word `linkType' can actually answer with.  A badge
    -- naming a type nothing produces would be a colour no cell ever wears.
  , testCase "every declared badge value is a type linkType can produce" $ do
      declared <- traverse (textAt "value")
              =<< listAt "badges" =<< columnOf "type" (object ["columns" .= linkColumns])
      assertEqual "the six the corpus spells"
        ["https", "http", "glance", "mailto", "id", "file"] declared
      assertEqual "and each is what its own scheme derives to" declared
        (map (\t -> linkType (if t == "glance" then "org-glance-visit:x" else t <> ":x"))
             declared)
  ]

-- | The subtree lens: a subtree split into the parts a client edits and the
-- parts the server keeps, and put back.
--
-- One rule under all of it — every byte of a subtree has one owner.  So the
-- assertions are about bytes rather than about shapes: what the body keeps, what
-- a part that nobody touched is written back as, and that decompose followed
-- by recompose is the identity on the file.
--
-- Three regions come out and four things go back in.  The hidden properties and
-- the logbook are the SERVER's, and the cases below are generic over
-- 'hiddenProperties' rather than spelling @ORG_GLANCE_ID@ into an assertion.
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

      -- The identity property is not a pair a client may edit: it is the row id
      -- the table keys its updates off, so the server keeps it out of what it
      -- hands over and puts it back itself.
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

      -- The logbook is located textually rather than parsed: it is the drawer
      -- named LOGBOOK sitting past the title line and ahead of the first child.
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

      -- The lens is over ONE headline: a child's drawer belongs to the child's
      -- own lens and is body text here, byte for byte.
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
        mapM_ roundTrips [ drawered, planned, unicoded, oddly, indented, crlf
                         , logged, childLogged, permuted, stamped
                         , T.unlines ["* TODO Bare", "body"]
                         , "* Ends at the drawer\n:PROPERTIES:\n:A: 1\n:END:" ]

      -- The three keywords permute freely on their line, so a round trip that
      -- reordered them would be a spurious hunk on every scheduled headline.
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
          assertContains "and the padded one" ":C:   three   \n" back

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

      -- A hidden property survives a client that never mentioned it, in its own
      -- place and byte for byte: it is the server's, so an empty list empties
      -- the client's half and nothing else.
    , testCase "a hidden property survives a sync that never mentioned it" $
        withParts drawered $ \r -> do
          let back = recomposedSubtree r (headlineParts r) { hpProperties = [] }
          assertContains "verbatim" ":ORG_GLANCE_ID: first\n" back
          assertBool "and the edited half is gone"
                     (not (":EFFORT:" `T.isInfixOf` back))

      -- The list has more than one entry in it, so "hidden" is the list rather
      -- than one key's special case: a captured row's creation time is kept the
      -- same way its id is, and both come back at the indices they sat on.
    , testCase "every hidden key survives, at the line it sat on" $
        withParts stamped $ \r -> do
          let parts = headlineParts r
          assertEqual "neither is offered" [] [ k | (k, _v) <- hpProperties parts
                                                  , k `elem` hiddenProperties ]
          assertEqual "and both are woven back where they were"
                      [ ":PROPERTIES:", ":ORG_GLANCE_ID: kept"
                      , ":ORG_GLANCE_CREATION_TIME: [2026-08-01 Sat 09:30]", ":END:" ]
                      (drawerOf (recomposedSubtree r parts { hpProperties = [] }))

      -- And a client that sends one anyway writes nothing.
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

      -- The drawer's line is counted from the top of the subtree, which is the
      -- one place a client cannot have moved it from: the lines above it are the
      -- headline's own and the planning line.
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

      -- A drawer for a headline that had no planning goes under the title; add
      -- a planning entry in the same commit and the two cannot both be line one.
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
              -- A second line would be a second line, and a planning line is one.
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
                  (subtreeText r) (recomposedSubtree r parts)

-- | The drawer TEXT holds, line by line and stripped — what a drawer says,
-- where the byte-level cases say how it is written.
drawerOf :: Text -> [Text]
drawerOf text' = takeWhile (/= ":END:") opened <> [":END:"]
  where opened = dropWhile (/= ":PROPERTIES:") (map T.strip (T.lines text'))

-- | Run K over the FIRST record DOC loads to, which is the headline every case
-- here is about.
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

-- | What a capture leaves behind: the row id and the creation time it was
-- written with, both of them the server's, with one pair of the client's
-- between them.
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

-- | Spacing org never writes and a file can still hold: no space after the
-- colon, a valueless key, and a padded value.
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

-- | A logbook belonging to the CHILD: past the first child's stars, so it is
-- body text as far as this headline's lens is concerned.
childLogged :: Text
childLogged = T.unlines
  [ "* TODO Parent"
  , "body line"
  , "** Child"
  , ":LOGBOOK:"
  , "CLOCK: kid"
  , ":END:"
  , "child body" ]

-- | The three planning keywords out of org's own order, which a file may write
-- and a round trip must not tidy.
permuted :: Text
permuted = T.unlines
  [ "* TODO Permuted"
  , "CLOSED: [2026-07-30 Thu] SCHEDULED: <2026-08-01 Sat>"
  , "body" ]

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
  , show (hrSearch r), show (hrLinks r), show (hrLinked r)
  , show (T.length (hrDoc r)) ]

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

-- | What the walk crosses and what it declines, over a tree carrying every
-- shape at once.  One @lstat@ classifies an entry ('Data.Org.Walk'), and a
-- symlink pays a second stat to classify its TARGET, so the four answers are:
-- a symlinked DIRECTORY is never followed, a symlinked FILE is walked like a
-- real one, a link whose target is missing is walked and fails on the read, and
-- Emacs's lock is refused by NAME before either stat is asked.
--
-- Asserted as the sorted file list rather than as a count, because the two ways
-- this breaks look alike in a total: a tree entered twice through a link adds
-- files, and a file quietly dropped removes one.  The links point OUTSIDE the
-- walked root for the same reason — a followed one shows up as a path that
-- could not have been reached any other way.
-- The paths are asserted first and on their own, because they are the half a
-- reader can act on: a matrix failure reads as one missing or one extra path
-- long before it reads as an outcome list.  The dangling link is then the one
-- the walk keeps on purpose and the read refuses — a genuine .org symlink its
-- author broke is a real file — while Emacs's lock is the case that must never
-- get that far, and does not, never becoming a path at all.
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

    -- The walk and the watch read ONE predicate, so a file no walk collected
    -- can never arrive by inotify and splice a row of history into the table.
  , testCase "and the watch declines it through the same predicate" $
      withStoreTree $ \store _files -> do
        let occurrence = store </> "data" </> "ab" </> "cd" </> "occurrences"
                               </> "2026-08-02.org"
        assertBool "the snapshot is a document by name" (documentPath occurrence)
        assertBool "and derived, so the watch drops it" (derivedPath occurrence)
        assertBool "while the blob beside it is neither"
                   (not (derivedPath (store </> "data" </> "ab" </> "cd" </> "data.org")))
  ]

-- | Run ACT over an org-glance store and the files a load of its tree turned
-- up.  The blob and its history carry the SAME @ORG_GLANCE_ID@, which is the
-- hazard: both sit inside the canonical store, so before the rule the pair tied
-- and walk order decided which one a table showed and a command wrote to.
--
-- The overview mirror is written beside them so the case covers the whole
-- denylist rather than its new clause alone.
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

-- | Run ACT over a walked root and the files a load of it turned up.  Every
-- link points into a sibling directory the walk is never given, so a followed
-- one is a path in the answer rather than a duplicate of one.
--
-- Two names carry their own case.  @realdir.org@ is a real DIRECTORY spelled
-- like a document, so the type decides and the walk enters it; @dirlink.org@ is
-- a symlink to a directory spelled the same way, so the name alone would keep
-- it and the target's type is what refuses it.
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

-- | Which headlines become rows.  The table is a list of top entries: one row
-- per level-one headline, and everything under one reachable by materializing
-- it rather than by a row of its own.
--
-- The consequences are the cases, because each of them is a thing a reader can
-- notice and none of them is an oversight: a child's words leave the search
-- index, a child's @ORG_GLANCE_ID@ stops addressing anything, and a file whose
-- outline never reaches level one contributes nothing at all.
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

    -- The rule is the star count rather than "shallowest headline in the
    -- file": a file that opens at level two has no top entry to show, and
    -- answers the way a file with no headlines does.
  , testCase "a file that never reaches level one contributes no rows" $
      withRecordsOf (T.unlines ["** two", "*** three"]) $ \recs ->
        assertEqual "rows" [] (map hrTitle recs)

    -- Intended, and the reason it is pinned: an id on a deeper headline names
    -- nothing the table can address, so it is neither a row id nor a collision.
  , testCase "an ORG_GLANCE_ID under a child is not a row id" $
      withRecordsOf (T.unlines [ "* parent", "** child", ":PROPERTIES:"
                               , ":ORG_GLANCE_ID: kid", ":END:" ]) $ \recs -> do
        assertEqual "titles" ["parent"] (map hrTitle recs)
        assertBool ("kid is a row id: " <> show (map hrId recs))
                   ("kid" `notElem` map hrId recs)
  ]

-- | The other half of the row rule: a top entry with nothing in any of the six
-- columns is not a row.  The file keeps it — org is the source of truth — and
-- the table skips it, so what used to be a line of six empty cells is now no
-- line.
--
-- The cases are the boundary from both sides.  One filled column is enough, and
-- nothing the table has no column for rescues an entry: a @CLOSED:@ stamp, a
-- drawer, a body, children.  The two costs are pinned rather than described —
-- a blank entry has no id, so no command can address it, and the ordinal counts
-- rows rather than entries, so an entry going blank renumbers the ones behind
-- it.
blankSpec :: TestTree
blankSpec = testGroup "Blank entries"
  [ testCase "an entry with nothing to show is no row" $
      mapM_ (\(what, doc) -> withRecordsOf doc $ \recs ->
                assertEqual what [] (map hrId recs))
            [ ("stars and a space", "* \n")
            , ("stars alone",       "*\n") ]

    -- One case per column, and the assertion names the column: a row whose
    -- state is filled and whose other five are empty is what "the todo alone
    -- keeps it" means.
  , testCase "one filled column is enough, and it is the one that was filled" $
      mapM_ (\(want, doc) -> withRecordsOf doc $ \recs ->
                assertEqual (T.unpack want) [[want]] (map filledColumns recs))
            [ ("state",     "* TODO\n")
            , ("priority",  "* [#A]\n")
            , ("title",     "* a title\n")
            , ("scheduled", "* \nSCHEDULED: <2026-08-01 Sat>\n")
            , ("deadline",  "* \nDEADLINE: <2026-08-01 Sat>\n") ]

    -- The tags clause of the rule never fires alone, and this is why: org
    -- spells tags after a title, so the parser reads a headline that is nothing
    -- but colons as a TITLE of colons.  Either way the entry shows something
    -- and stays a row.
  , testCase "a headline of nothing but tags is a title of colons" $
      withRecordsOf "* :work:\n" $ \recs -> do
        assertEqual "columns" [["title"]] (map filledColumns recs)
        assertEqual "the colons are the title" [":work:"] (map hrTitle recs)

  , testCase "and tags proper keep a row the title already kept" $
      withRecordsOf "* a title :work:\n" $ \recs ->
        assertEqual "columns" [["title", "tag"]] (map filledColumns recs)

    -- What a row shows is what a column can carry, so everything else leaves
    -- the entry blank however much of it there is.  The drawer case costs the
    -- most: a blank entry has no row id, so an ORG_GLANCE_ID on one addresses
    -- nothing and no command can reach it.
  , testCase "nothing outside the six columns rescues an entry" $
      mapM_ (\(what, doc) -> withRecordsOf doc $ \recs ->
                assertEqual what [] (map hrId recs))
            [ ("closed",   "* \nCLOSED: [2026-08-01 Sat]\n")
            , ("drawer",   "* \n:PROPERTIES:\n:ORG_GLANCE_ID: solo\n:END:\n")
            , ("body",     "* \nsome text under it\n")
            , ("children", "* \n** TODO a child\n") ]

    -- The ordinal numbers EMITTED rows ('rowId'), so a blank entry spends none
    -- and every K behind it is one lower than the entry count would give.  Same
    -- class as the reorder churn: an entry going blank is a removal, and
    -- clearing the last keyword off a title-less row is how a reader gets there.
  , testCase "a blank entry spends no ordinal" $
      withRecordsOf (T.unlines ["* one", "* ", "* two"]) $ \recs -> do
        assertEqual "titles" ["one", "two"] (map hrTitle recs)
        assertEqual "ids" [ T.pack (hrFile r) <> k | (r, k) <- zip recs ["#0", "#1"] ]
                    (map hrId recs)

    -- The rule stated over the records rather than over the headlines it is
    -- computed from: the two layers agree, and this is what that agreement
    -- looks like from the outside.
  , testCase "so no row the loader emits has six empty cells" $ withRecords $ \recs -> do
      assertEqual "the fixture's rows" 6 (length recs)
      assertEqual "blank rows" [] [ hrId r | r <- recs, null (filledColumns r) ]
  ]

-- | The column keys R fills, in column order.  Six cells, and a row exists
-- because at least one of them is not empty.
filledColumns :: HeadlineRecord -> [Text]
filledColumns r =
  [ key | (key, cell) <- zip ["state", "priority", "title", "scheduled", "deadline", "tag"]
                             [ opt (hrState r), opt (hrPriority r), hrTitle r
                             , opt (hrScheduled r), opt (hrDeadline r), hrTags r ]
        , not (T.null cell) ]
  where opt = fromMaybe ""

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

    -- The tags field is the CELL's, so the haystack carries the sorted spelling
    -- and not the file's `:web:glance:'.  There is no third answer to keep in
    -- step: `searchTextOf' joins `viewCells', which reads the column accessors.
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
        -- One row each, stated apart: a sum of two counts is met by 2 + 0.
        assertEqual "unicode, cyrillic mid-title" 1 (matching "печатник")
        assertEqual "unicode, cyrillic title" 1 (matching "Привет")
        assertEqual "an empty query is every row" 6 (matching "")
        assertEqual "blank is empty too" 6 (matching "   ")
        -- The cells are joined by a character no cell can hold, so the end of
        -- one and the start of the next never read as one string.
        assertEqual "across the join" 0 (matching "next a")

    -- INTENDED, and pinned because it is the visible cost of rows being top
    -- entries: the index is built out of the cells of the rows that exist, so a
    -- word only a child carries reaches nothing.  What surfaces the child is
    -- materializing the entry it belongs to.
  , testCase "a word only a child carries matches nothing" $
      withRecordsOf (T.unlines ["* parent", "** subterranean child"]) $ \recs -> do
        assertEqual "the entry is a row" 1 (length (filter (matchesSearch "parent") recs))
        assertEqual "the child is not" 0 (length (filter (matchesSearch "subterranean") recs))
        assertBool "though its subtree still spells it"
                   (all (T.isInfixOf "subterranean" . subtreeText) recs)
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
      -- The FIELD is the file's own order.  It is what `classify' reads, and
      -- there the order decides which tag's config governs the row.
      assertEqual "tags"
                  [":web:glance:", ":unicode:", "", "", ":cleanup:", ":web:"]
                  (map hrTags recs)

    -- A reader scanning a column reads it as a list, and a list whose order is
    -- the author's typing order is a list they have to scan whole.  So the CELL
    -- sorts, case-folded, and the field it is cut from does not.
  , testCase "the tags CELL sorts, case-folded, where the field keeps the file" $ do
      assertEqual "the sample row's own cell"
                  ":glance:web:" (sortedTagsCell ":web:glance:")
      assertEqual "folded, so a capital does not sort ahead of every lowercase"
                  ":admin:Work:" (sortedTagsCell ":Work:admin:")
      -- Stable, so two spellings folding alike keep the order the file put them
      -- in and the cell is a function of the file rather than of a tie-break.
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

    -- FILE#K, K counted over the file's TOP ENTRIES: the sample's first row
    -- carries an ORG_GLANCE_ID and the rest do not, so the ordinals run 1..5
    -- with 0 spent on the entry that did not need it.  Numbering the entries
    -- rather than the ids is what keeps a K meaningful — it is a position in
    -- the file, whatever the rows around it are called.
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
        ["state", "priority", "title", "scheduled", "deadline", "tag"] keys

  -- SCHEMA.md makes `sortable' opt-in and this producer opts every column in:
  -- order means something in all six, and the flag is what a READER's `^' or a
  -- header click reads before it will sort one.  Stated as the whole list, so a
  -- column added without it fails here rather than arriving unsortable.
  , testCase "every column opts into sorting" $ withView $ \v -> do
      keys <- columnKeysOf v
      flags <- listAt "columns" v >>= mapM (maybeBoolAt "sortable")
      assertEqual "one sortable per column" (map (const (Just True)) keys) flags

  -- The renderer stopped drawing an outline, so the producer stopped
  -- describing one: a row is an id and its cells, and nothing says where it
  -- sits among the others.  Asked of a fixture that HAS an outline, the
  -- golden's being flat.
  , testCase "no row carries a depth, as a field or as a cell" $
      withViewOf nested $ \v -> do
        cols <- columnKeysOf v
        rows <- listAt "rows" v
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

    -- An ADDITIVE row field (@table-view/SCHEMA.md@): a renderer that never
    -- learns it renders as it always did, which is what SPARSE buys — @true@ or
    -- absent, never @false@, so a row with nowhere to go is the row it was
    -- before the field existed.
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
        assertBool "linked is a column" ("linked" `notElem` cols)
        assertBool (show cells <> " names linked") (all ("linked" `notElem`) cells)

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
      -- Declared rather than sampled: the renderer decides which column holds a
      -- LIST from up to 40 non-empty cells, and a page with fewer than two
      -- tagged rows finds none at all — where `tag:*archive*' would be a
      -- literal matching nothing while this producer reads it as the whole tag.
      -- The declaration is what settles it.
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
        assertEqual "no column is named twice" (length keys) (length (nub keys))
        ascs <- each "sort" "ascending" v >>= mapM boolOf
        assertEqual "every key ascends" (map (const True) keys) ascs

  , testCase "the rows are served in the order the chain declares" $ do
      -- The declaration and the arrangement are one list read twice; a page cut
      -- out of a different order than the one declared is a different set of
      -- rows than the table would have put there.
      let doc = T.unlines [ "* TODO beta", "* echo", "* DONE alpha"
                          , "* TODO Alpha", "* delta" ]
      withRecordsOf doc $ \records ->
        -- State by palette order (org's cycle: TODO before DONE, the stateless
        -- row behind both), then the title folded inside each state.
        assertEqual "state by palette order, then title folded"
                    ["TODO Alpha", "TODO beta", "DONE alpha", "delta", "echo"]
                    [ maybe "" (<> " ") (hrState r) <> hrTitle r
                    | r <- sortedForView records ]

  , testCase "an empty cell sorts to the end of its own key" $
      -- Nulls are settled per key and outside the direction, so a row with no
      -- state is behind every row that has one where the titles tie.
      withRecordsOf (T.unlines ["* same", "* TODO same", "* DONE same"]) $
        \records ->
          assertEqual "the stateless row last"
                      [Just "TODO", Just "DONE", Nothing]
                      (map hrState (sortedForView records))

    -- THE CELL WEARS ORG'S BRACKETS AND THE COMPARATOR READS THROUGH THEM, which
    -- is the same rule the filter matches by.  The bracketed text happens to
    -- order the same way today, so what this pins is the READING: a comparator
    -- over `[#A]' as text is one spelling away from ordering by punctuation.
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
-- by a later splice.  The suite's own splice: four lines, no engine.
--
-- The order is taken rather than assumed, since 'Data.Org.Edit.applyEdits'
-- sorts too and a command handing its edits back ascending — @remove-tag@ over
-- a tag a file spells twice — is not a caller mistake this oracle may punish.
splice :: Text -> [(Span, Text)] -> Text
splice doc edits = foldl' one doc (sortOn (negate . spanStart . fst) edits)
  where one text (Span s e, new) = T.take s text <> new <> T.drop e text

-- | Run K over the one record DOC parses to.
withRecord :: Text -> (HeadlineRecord -> Assertion) -> Assertion
withRecord doc k = withDoc "command" "one.org" doc one
  where one [r] = k r
        one rs  = assertFailure ("expected one headline, got " <> show (length rs))

-- | WHAT: DOC with @set-state KEYWORD@ applied to its one headline is WANTED.
-- Under 'noConfig', so the chain the legality check reads is the file's own
-- @#+TODO:@ over org's built-in cycle and nothing else.
setStateIs :: String -> Text -> Maybe Text -> Text -> Assertion
setStateIs what doc keyword = triedEditsAre what doc (setStateEdits noConfig keyword)

-- | @set-state KEYWORD@ on R under 'layered' is refused, and the refusal spells
-- the keyword it turned down, the row it turned it down for, and every word of
-- WORDS — enough of the chain to say what the row could have taken instead.
refusalNames :: HeadlineRecord -> Text -> [Text] -> Assertion
refusalNames r keyword words' = case setStateEdits layered (Just keyword) r of
  Right edits -> assertFailure ("expected a refusal, got " <> show edits)
  Left why    -> mapM_ (\w -> assertContains "names" w why) (keyword : hrId r : words')

-- | @set-state@ on R under 'layered' takes each of WORDS.  The accepting half
-- of 'refusalNames', and the keyword is its own label.
accepts :: HeadlineRecord -> [Text] -> Assertion
accepts r = mapM_ (\w -> either (assertFailure . ((T.unpack w <> ": ") <>) . T.unpack)
                                (const (pure ()))
                                (setStateEdits layered (Just w) r))

-- | A config with a cycle per tag and one in @system.org@, so the legality
-- check has a chain longer than one file to be right about.
layered :: ConfigLayers
layered = noConfig
  { clSystem = TodoKeywords ["STARTED"] []
  , clTags   = [ ("book", TodoKeywords ["READING"] ["READ"])
               , ("film", TodoKeywords ["WATCHING"] ["WATCHED"]) ]
    -- Recognition unions both tags, which is exactly what no longer makes
    -- either of them settable anywhere.
  , clSeed   = TodoKeywords ["STARTED", "READING", "WATCHING"] ["READ", "WATCHED"] }

-- | WHAT: DOC with EDITS applied to its one headline is WANTED.  The whole
-- document is asserted, so the bytes around the edit are checked by the same
-- assertion as the edit.
editsAre :: String -> Text -> (HeadlineRecord -> [(Span, Text)]) -> Text -> Assertion
editsAre what doc edits wanted =
  withRecord doc (assertEqual what wanted . splice doc . edits)

-- | 'editsAre' for the commands whose span math can REFUSE: the refusal fails
-- the case naming it, so a caller writes the landing and nothing else.
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
-- The span is the scan's own, which is where a client's comes from.
editLinkIs :: String -> Text -> Text -> Maybe (Maybe Text) -> Text -> Assertion
editLinkIs what doc target desc = triedEditsAre what doc $ \r ->
  case subtreeLinks r of
    []      -> Left "the document holds no link"
    (l : _) -> editLinkEdits (olSpan l) target desc r

-- | WHAT: DOC with @rename-tag FROM TO@ applied to its one headline is WANTED.
renameTagIs :: String -> Text -> Text -> Text -> Text -> Assertion
renameTagIs what doc from to = editsAre what doc (renameTagEdits from to)

-- | Is TEXT a tag, as 'tagText' answers?  The predicate is the parser's own
-- charset, so what this pins is the pair agreeing rather than a second list.
tagIs :: (Text, Bool) -> Assertion
tagIs (text, wanted) = assertEqual (show text) wanted (isRight (tagText text))

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

      -- The smallest headline a command can reach.  Stars and nothing else is
      -- no row at all ('blankEntry'), so it carries no id and nothing addresses
      -- it; one shown attribute is what it takes, and the insertion point is
      -- still the stars'.
    , testCase "into a headline whose only content is a priority" $
        setStateIs "bare" "* [#B]\n" (Just "TODO") "* TODO [#B]\n"

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
          assertEqual "no edits" (Right []) (setStateEdits noConfig Nothing r)

      -- Per CHAIN, and the file is its nearest scope: the same word is a
      -- keyword in one document and the first word of a title in the next.
    , testCase "a keyword the chain does not declare is refused, by name" $
        withRecord "* TODO Plain\n" $ \r ->
          refusalNames r "WAITING" ["TODO", "DONE"]

    , testCase "the same keyword is legal once the file declares it" $
        setStateIs "declared" (keyworded "* TODO Plain\n") (Just "WAITING")
                              (keyworded "* WAITING Plain\n")

      -- Every rung of the chain is settable, which is the regression the
      -- tightening had to leave standing: the file's own #+TODO:, its tag's
      -- config, the system layer, org's own cycle.
    , testCase "each scope of the chain is settable on a row that reaches it" $
        withRecord (keyworded "* TODO Plain :book:\n") $ \r ->
          accepts r ["NEXT", "READING", "STARTED", "DONE"]

      -- The union's death, from the settability side: `film''s cycle parses
      -- here — the seed carries it — and no scope this row reaches declares it,
      -- so it is not a state this row may be put into.
    , testCase "another tag's keyword is refused on a row that does not carry it" $
        withRecord "* TODO Plain\n" $ \r ->
          refusalNames r "WATCHING" ["STARTED"]

    , testCase "and refused on a row carrying a different tag" $
        withRecord "* TODO Plain :book:\n" $ \r ->
          refusalNames r "WATCHING" ["READING"]

      -- The offer and the wall are one chain, and since `settableStates' IS
      -- `keywordSources' flattened they agree by construction — so this is the
      -- regression guard for that derivation rather than a property test.
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

      -- What the chain's ORDER costs a write: nothing.  The scopes were
      -- reordered widest-first and the offer is their union either way, so the
      -- words this row may be put into are the words it could be put into
      -- before — only the source each is shown under moved.
    , testCase "and the reorder moved which source shows a word, never the set" $
        withRecord (keyworded "* NEXT Plain :book:\n") $ \r ->
          assertEqual "the same eight words the nearest-scope chain offered"
                      (sort [ "NEXT", "WAITING", "CANCELLED", "READING", "READ"
                            , "STARTED", "TODO", "DONE" ])
                      (sort (settableStates layered r))

      -- The state column ships these two as filter vocabulary beside its
      -- badges.  No file declares one, so no file can be put into one.
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

      -- Archiving IS adding one tag, so the two are one function and there is
      -- no second insertion rule that could drift out of step with this one.
    , testCase "archive is add-tag at org's own name" $
        mapM_ (\doc -> withRecord doc $ \r ->
                 assertEqual (T.unpack doc) (addTagEdits "ARCHIVE" r) (archiveEdits r))
              [ "* TODO Ship it :web:\n", "* TODO Ship it\n"
              , "* TODO Ship it :ARCHIVE:\n", "* TODO\n" ]
    ]

    -- The pair the tag palette commits.  Both are idempotent, from opposite
    -- sides — adding what is there and removing what is not each cost no edit —
    -- which is what lets a bulk toggle be pressed at.
  , testGroup "add-tag"
    [ -- The written run is the FILE's, in the file's own order: this row's CELL
      -- reads `:glance:web:' and the splice still lands after `glance:', because
      -- the edit is measured in the span and the sort is the column's alone.
      testCase "joins the run, ahead of its closing colon" $
        addTagIs "into the run" "* TODO Ship it :web:glance:\n" "work"
                                "* TODO Ship it :web:glance:work:\n"

      -- The other side of the same rule, and the one an alphabetical write would
      -- get wrong in a way no cell could show: `glance' is the FIRST entry of
      -- the sorted cell and the SECOND of the file, and it is the file's offsets
      -- that decide which bytes come out.
    , testCase "and a removal cuts the file's entry, not the cell's" $
        removeTagIs "the first of the file's run" "* TODO Ship it :web:glance:\n"
                    "web" "* TODO Ship it :glance:\n"

    , testCase "with no run, opens one at the end of the title line" $
        addTagIs "creating" "* TODO Ship it\n" "work" "* TODO Ship it :work:\n"

      -- `hsFull' ends at the LAST part in span order, which here is a timestamp
      -- on the next line: appending there would put the tag on the wrong one.
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

      -- Presence folds, the way the filter's vocabulary does, so the palette
      -- never offers to write a second spelling of a tag the row has.
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

      -- The run is the only thing on the title line past the title, so the
      -- horizontal space in front of it is the separator and comes off too.
    , testCase "and a wider gap comes off whole" $
        removeTagIs "spaced" "* TODO Ship it    :work:\n" "work" "* TODO Ship it\n"

    , testCase "emptying it leaves the lines under the headline alone" $
        removeTagIs "planned" (T.unlines [ "* TODO Ship it :work:"
                                         , "SCHEDULED: <2026-08-01 Sat>"
                                         , ":PROPERTIES:"
                                         , ":ORG_GLANCE_ID: ship"
                                         , ":END:" ])
                              "work"
                              (T.unlines [ "* TODO Ship it"
                                         , "SCHEDULED: <2026-08-01 Sat>"
                                         , ":PROPERTIES:"
                                         , ":ORG_GLANCE_ID: ship"
                                         , ":END:" ])

    , testCase "a row that never had it costs no edit" $
        withRecord "* TODO Ship it :web:\n" $ \r -> do
          assertBool "not tagged" (not (tagged "work" r))
          assertEqual "no edits" [] (removeTagEdits "work" r)

    , testCase "and a row with no run at all costs none either" $
        withRecord "* TODO Ship it\n" $ \r ->
          assertEqual "no edits" [] (removeTagEdits "work" r)

      -- Folded, and EVERY entry spelling it, so what "removed" means is that
      -- the row does not answer to 'tagged' afterwards — which a file spelling
      -- one tag twice would otherwise break.
    , testCase "however the file spells it, and however often" $ do
        removeTagIs "folded" "* TODO Ship it :Work:\n" "work" "* TODO Ship it\n"
        removeTagIs "twice" "* TODO Ship it :work:web:Work:\n" "work"
                            "* TODO Ship it :web:\n"

      -- Add then remove is the identity on the bytes, which is the property a
      -- toggle rests on: a mis-press costs a write and no text.
    , testCase "removing what was just added puts the file back" $ do
        let doc = "* TODO Ship it :web:\n"
        withRecord doc $ \r -> do
          let added = splice doc (addTagEdits "work" r)
          assertEqual "added" "* TODO Ship it :web:work:\n" added
          withRecord added $ \r' ->
            assertEqual "and back" doc (splice added (removeTagEdits "work" r'))
    ]

    -- The third tag command, and the one that is a command rather than a
    -- composition: a remove and an add spliced together APPLY — the spans touch
    -- and 'applyEdits' rejects only overlap — and what they write is the tag on
    -- the title, or the entry moved to the end of the run.
  , testGroup "rename-tag"
    [ testCase "replaces the entry where it stands, colon and all left alone" $ do
        renameTagIs "in the middle" "* TODO Ship it :web:glance:work:\n"
                    "glance" "code" "* TODO Ship it :web:code:work:\n"
        renameTagIs "at the head" "* TODO Ship it :web:glance:\n"
                    "web" "code" "* TODO Ship it :code:glance:\n"
        renameTagIs "alone" "* TODO Ship it :work:\n"
                    "work" "projects" "* TODO Ship it :projects:\n"

      -- The property the popup's cursor rests on and a composition cannot have:
      -- the run's ORDER is untouched, so the union it draws does not reshuffle
      -- under the reader's hands.
    , testCase "and the run's order is what it was" $
        renameTagIs "kept" "* TODO Ship it :a:b:c:\n" "a" "z"
                           "* TODO Ship it :z:b:c:\n"

      -- Every other byte of the document, which is the write-back invariant
      -- read at this layer.
    , testCase "the lines under the headline are untouched" $
        renameTagIs "planned" (T.unlines [ "* TODO Ship it :work:"
                                         , "SCHEDULED: <2026-08-01 Sat>"
                                         , ":PROPERTIES:"
                                         , ":ORG_GLANCE_ID: ship"
                                         , ":END:" ])
                              "work" "projects"
                              (T.unlines [ "* TODO Ship it :projects:"
                                         , "SCHEDULED: <2026-08-01 Sat>"
                                         , ":PROPERTIES:"
                                         , ":ORG_GLANCE_ID: ship"
                                         , ":END:" ])

    , testCase "a row that does not carry the old name costs no edit" $
        withRecord "* TODO Ship it :web:\n" $ \r ->
          assertEqual "no edits" [] (renameTagEdits "work" "projects" r)

    , testCase "and a row with no run at all costs none either" $
        withRecord "* TODO Ship it\n" $ \r ->
          assertEqual "no edits" [] (renameTagEdits "work" "projects" r)

      -- Matching FOLDS, the way presence does, so a change of SPELLING is a
      -- rename like any other.
    , testCase "the old name is matched folded, and the new one written as given" $ do
        renameTagIs "folded" "* TODO Ship it :Work:\n" "work" "projects"
                             "* TODO Ship it :projects:\n"
        renameTagIs "respelled" "* TODO Ship it :Work:\n" "work" "work"
                                "* TODO Ship it :work:\n"

      -- ONE TAG ONCE, which is what 'removeTagEdits' keeps by cutting every
      -- entry that spells its tag: the first becomes the new name and the rest
      -- go, so a file spelling one tag twice comes out clean.
    , testCase "a tag spelled twice comes out spelled once" $
        renameTagIs "deduplicated" "* TODO Ship it :work:web:Work:\n" "work" "projects"
                                   "* TODO Ship it :projects:web:\n"

      -- And where the row ALREADY carries the new name, the rename is a
      -- removal: writing it would leave the run holding it twice.  The entry
      -- that survives is the one the file already had, in its own place.
    , testCase "a row already carrying the new name loses the old one instead" $ do
        renameTagIs "merged" "* TODO Ship it :web:work:\n" "web" "work"
                             "* TODO Ship it :work:\n"
        renameTagIs "merged the other way" "* TODO Ship it :web:work:\n" "work" "web"
                                           "* TODO Ship it :web:\n"

      -- Which never empties the run: the entry carrying the new name is one of
      -- the ones left standing, so there is no whole-run branch to reach.
    , testCase "and the run and its separator stand" $
        renameTagIs "run kept" "* TODO Ship it  :a:b:\n" "a" "b"
                               "* TODO Ship it  :b:\n"

      -- The composition this replaces, spelled out.  The two edit sets apply
      -- together — the removal ends exactly where the addition inserts, and
      -- touching spans are what 'Data.Org.Edit.applyEdits' allows ('TestEdit',
      -- "touching edits are accepted") — and the addition's anchor was measured
      -- BEFORE the removal, so the tag lands flush against the title the removal
      -- just closed up to.
    , testCase "the composition it replaces writes the tag onto the title" $ do
        let doc = "* TODO Ship it :work:\n"
        withRecord doc $ \r -> do
          assertEqual "the removal takes the run, and the addition lands past it"
                      "* TODO Ship itprojects:\n"
                      (splice doc (removeTagEdits "work" r <> addTagEdits "projects" r))
          assertEqual "where the one command spells the file" "* TODO Ship it :projects:\n"
                      (splice doc (renameTagEdits "work" "projects" r))
    ]

    -- @edit-link@: the one command whose args name a row's own BYTES.  The span
    -- comes out of the scan here, which is where a client's comes from — the
    -- popup edits the ranges `/links' handed it — so what these cases drive is
    -- the round trip, read and written by one grammar.
  , testGroup "edit-link"
    [ -- THE FORM TABLE.  A bracketed link stays bracketed and a plain URL stays
      -- plain, so an entry keeps the way its author wrote it; a description
      -- ARRIVING is the one thing that changes a shape, since a plain URL has
      -- nowhere to write one.
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
            -- The one shape change: a description has nowhere to live in a
            -- plain URL, so one arriving brackets it.
          , ( "plain URL, description added", "https://a.example"
            , "https://b.example", Just (Just "B"), "[[https://b.example][B]]" )
          , ( "plain URL, description off", "https://a.example"
            , "https://b.example", Just Nothing, "https://b.example" ) ]

      -- ABSENT IS NOT NULL, which is the `args' discipline this route turns on
      -- (`.:!' rather than `.:?'): a request that says nothing about the
      -- description leaves the author's, and a null takes it off.  A
      -- description that SHOWS nothing is the null spelled another way, since
      -- `[[T][]]' shows its target — and an untouched empty section is still
      -- the author's bytes, so it stands.
    , testCase "a description that shows nothing is the null spelled another way" $ do
        editLinkIs "empty string" "* one [[https://a.example][A]]\n"
                   "https://b.example" (Just (Just "")) "* one [[https://b.example]]\n"
        editLinkIs "spaces" "* one [[https://a.example][A]]\n"
                   "https://b.example" (Just (Just "  ")) "* one [[https://b.example]]\n"
        editLinkIs "an empty section nobody touched stands"
                   "* one [[https://a.example][]]\n"
                   "https://b.example" Nothing "* one [[https://b.example][]]\n"
        -- The emptiness test strips and the VALUE is written verbatim, which is
        -- the target's own rule: neither is content, and content is nobody's to
        -- trim.
        editLinkIs "a description with content keeps its spacing"
                   "* one [[https://a.example][A]]\n"
                   "https://b.example" (Just (Just " spaced "))
                   "* one [[https://b.example][ spaced ]]\n"

      -- A description is written as it was given, trimmed of the whitespace a
      -- field leaves behind — the bytes around the link are the author's and
      -- this moves none of them.
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

      -- THE FIRST WALL: the span has to sit inside the ROW's own subtree and
      -- cover exactly one link, edge to edge.  A span a character short of the
      -- real one is refused rather than spliced into the middle of a link.
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

      -- THE SECOND WALL: the replacement has to READ BACK as THE LINK IT CLAIMS
      -- TO BE.  The write engine is content-agnostic by law, so this is the
      -- layer that owes the check — a target that would spell something else on
      -- the next load is refused rather than written.
    , testCase "a replacement that would not read as one link is refused" $
        mapM_ (\(what, wrote, target) ->
                 withRecord ("* one " <> wrote <> "\n") $ \r ->
                   case subtreeLinks r of
                     (l : _) -> case editLinkEdits (olSpan l) target Nothing r of
                       Right edits -> assertFailure (what <> ": expected a refusal, got "
                                                       <> show edits)
                       Left why -> assertContains what "does not read as one link" why
                     [] -> assertFailure (what <> ": no link to edit"))
          -- A bracket in the target closes the link early.
          [ ("a bracket in the target", "[[https://a.example][A]]", "https://a]b")
          -- A plain URL keeps its shape, and a target no plain-link scheme reads
          -- would be prose on the next load.
          , ("a bare link swapped for a path", "https://a.example", "file:notes.org")
          , ("a bare link given a space", "https://a.example", "https://a b") ]

      -- REPARSING ALONE IS NOT THE WALL, and this is the case that says why: a
      -- target spelling `a][b' renders a link that IS one link — pointing at
      -- `a', described `b', neither of them what the request named.  The check
      -- compares the reparse against what was ASKED for, so the grammar cannot
      -- be escaped by spelling it.
    , testCase "a target that reparses as another link is refused, naming both" $
        withRecord "* one [[https://a.example]]\n" $ \r ->
          case subtreeLinks r of
            (l : _) -> case editLinkEdits (olSpan l) "https://a][b" Nothing r of
              Right edits -> assertFailure ("expected a refusal, got " <> show edits)
              Left why -> do
                assertContains "names what would have been written" "[[https://a][b]]" why
                assertContains "and what it was asked to point at" "https://a][b" why
            [] -> assertFailure "no link to edit"

      -- A NEWLINE is the one thing reparsing cannot catch: this scanner has no
      -- line rule, so the link reads back as itself — and lands a column-1 star
      -- in the file, which the ORG parser reads as a new headline.  Refused in
      -- both halves, since neither spans lines in org.
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

    -- The wall both tag commands put up, and it is the PARSER's own charset:
    -- what this server writes has to reparse here, and a tag carrying a
    -- character `tagsP' declines takes the whole run down into title text.
  , testGroup "the tags add-tag and remove-tag take"
    [ testCase "each spelling, and whether it is one" $ mapM_ tagIs
        [ ("work", True), ("WORK", True), ("work_2", True), ("a-b", True)
        , ("@home", True), ("c#", True), ("2026", True)
        -- Org's own set carries `%' and this parser's does not; the parser's is
        -- the one that binds, since it is what reads the write back.
        , ("50%", False)
        , ("", False), ("two words", False), (":work:", False), ("a.b", False) ]

    , testCase "a refusal names what it turned down" $
        case tagText "50%" of
          Right kept -> assertFailure ("expected a refusal, got " <> show kept)
          Left why   -> assertContains "names the input" "50%" why
    ]

    -- The date a key collects, worked out against a fixed today so the answers
    -- can be written down.  What is pinned is that a value which does not
    -- REPARSE is refused rather than written: a planning line that stops being
    -- one is body text on the next load, and the entry the author set is gone.
  , testGroup "the dates set-planning takes"
    [ testCase "each spelling, and what it renders as" $ mapM_ reads'
        -- Org's own, taken exactly as written once it reparses — so a repeater
        -- and a range survive rather than being canonicalized away.
        [ ("<2026-08-05 Wed>",      "<2026-08-05 Wed>")
        , ("<2026-08-05 Wed 09:30>", "<2026-08-05 Wed 09:30>")
        , ("<2026-08-05 Wed +1w>",  "<2026-08-05 Wed +1w>")
        -- A wrong weekday in the file's own form stands: the value is the
        -- author's, and reparsing is the whole of the bar.
        , ("<2026-08-05 Mon>",      "<2026-08-05 Mon>")
        -- BOTH of org's openers are org's own form, so an inactive stamp is
        -- kept verbatim the way an active one is.
        , ("[2026-08-05 Wed]",      "[2026-08-05 Wed]")
        -- ISO, with the weekday computed rather than typed.
        , ("2026-08-05",            "<2026-08-05 Wed>")
        , ("2026-08-05 09:30",      "<2026-08-05 Wed 09:30>")
        , ("2026-08-05 9:05",       "<2026-08-05 Wed 09:05>")
        , ("  2026-08-05  ",        "<2026-08-05 Wed>")
        -- Relative to the day the request was made, once for the whole request.
        , ("today",                 "<2026-08-01 Sat>")
        , ("TODAY",                 "<2026-08-01 Sat>")
        , ("tomorrow",              "<2026-08-02 Sun>")
        , ("+0d",                   "<2026-08-01 Sat>")
        , ("+3d",                   "<2026-08-04 Tue>")
        , ("+2w",                   "<2026-08-15 Sat>")
        , ("+1m",                   "<2026-09-01 Tue>") ]

      -- THE COMPUTED BRANCH CHECKS NOTHING OF ITS OWN.  Only an already-
      -- bracketed value is reparsed before it is kept, so what the other three
      -- spellings RENDER is asserted here rather than trusted: a stamp org does
      -- not read back turns the planning line into body text on the next load,
      -- and the entry the author set is gone with it.
    , testCase "and everything it computes reads back as a timestamp" $
        mapM_ (\text' -> case planningTimestamp today text' of
                 Left why    -> assertFailure (T.unpack text' <> " refused: " <> T.unpack why)
                 Right stamp -> assertBool (T.unpack stamp <> " does not reparse")
                                           (readsAsTimestamp stamp))
              [ "today", "tomorrow", "+3d", "+2w", "+1m"
              , "2026-08-05", "2026-08-05 09:30", "2026-08-05 9:05" ]

    , testCase "and everything else is refused, by name" $ mapM_ refuses
        [ "", "   ", "next tuesday", "05/08/2026", "2026-13-01", "+3", "+3x", "-3d"
        -- A bracketed value that does not reparse is refused like any other:
        -- what the brackets buy is being taken verbatim, not being trusted.
        , "<not a date>", "<2026-08-05 Wed", "2026-08-05 25:00" ]
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

      -- Under the TITLE line rather than at `hsFull''s end, which for a
      -- drawered headline is its `:END:' two lines further down.
    , testCase "and it goes above the drawer, not into it" $
        planningIs "over a drawer" "DEADLINE" (Just "<2026-08-09 Sun>")
          (T.unlines ["* TODO Ship it", ":PROPERTIES:", ":A: 1", ":END:"])
          (T.unlines ["* TODO Ship it", "DEADLINE: <2026-08-09 Sun>", ":PROPERTIES:"
                     , ":A: 1", ":END:"])

      -- An added entry joins the END of the line, behind whatever it already
      -- carries — the lens's own rule for an entry that moved.
    , testCase "beside an entry the line already has, it joins the end" $
        planningIs "joined" "DEADLINE" (Just "<2026-08-09 Sun>")
          (T.unlines ["* TODO Ship it", "SCHEDULED: <2026-08-01 Sat>"])
          (T.unlines ["* TODO Ship it", "SCHEDULED: <2026-08-01 Sat> DEADLINE: <2026-08-09 Sun>"])

      -- Clearing takes the space that separated the entry with it, so the line
      -- closes up rather than keeping a gap where an entry was.
    , testCase "clearing the first of two closes the line up" $
        planningIs "cleared first" "SCHEDULED" Nothing
          (T.unlines ["* TODO Ship it", "SCHEDULED: <2026-08-01 Sat> DEADLINE: <2026-08-05 Wed>"])
          (T.unlines ["* TODO Ship it", "DEADLINE: <2026-08-05 Wed>"])

      -- The last entry on a line has no trailing run to take, so the LEADING
      -- one goes instead — take both and the neighbours would be glued.
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

      -- The lens's rule: a planning line with no entries left is not one, so
      -- the whole line goes rather than an empty keyword being left behind.
    , testCase "clearing the only entry takes the line with it" $
        planningIs "line dropped" "SCHEDULED" Nothing
          (T.unlines ["* TODO Ship it", "SCHEDULED: <2026-08-01 Sat>", "body"])
          (T.unlines ["* TODO Ship it", "body"])

      -- CLOSED is an entry for that purpose even though no key sets one.
    , testCase "but a CLOSED beside it keeps the line standing" $
        planningIs "closed stays" "SCHEDULED" Nothing
          (T.unlines ["* TODO Ship it", "CLOSED: [2026-07-30 Thu] SCHEDULED: <2026-08-01 Sat>"])
          (T.unlines ["* TODO Ship it", "CLOSED: [2026-07-30 Thu]"])

    , testCase "clearing an entry the headline never had costs no edit" $
        withRecord "* TODO Plain\n" $ \r ->
          assertEqual "no edits" (Right []) (setPlanningEdits "DEADLINE" Nothing r)

      -- Only the two a key sets.  CLOSED is org's own bookkeeping, and a
      -- keyword org never reads is not one at all.
    , testCase "a keyword no key sets is refused, by name" $
        withRecord "* TODO Plain\n" $ \r ->
          mapM_ (\keyword -> case setPlanningEdits keyword (Just "<2026-08-05 Wed>") r of
                   Right edits -> assertFailure (T.unpack keyword <> ": " <> show edits)
                   Left why -> assertBool (T.unpack why) (keyword `T.isInfixOf` why))
                ["CLOSED", "scheduled", "TIMESTAMP"]
    ]

  , testGroup "capture"
    [ -- The insertion is at the END, so a file that already holds work keeps
      -- every byte of it exactly where it was.
      testCase "appends a top entry with its creation time in a drawer" $
        assertEqual "the entry, and the file under it"
          (Right (T.unlines [ "* TODO old", "* TODO Buy milk :errands:", ":PROPERTIES:"
                            , ":ORG_GLANCE_CREATION_TIME: [2026-08-01 Sat 09:30]", ":END:" ]))
          (captured "* TODO old\n" "TODO Buy milk :errands:")

      -- A target that is not there yet is the empty document, and the entry is
      -- the whole file: creation is the ordinary write under the empty pin.
    , testCase "into a file that is not there yet, the entry is the file" $
        assertEqual "no leading blank"
          (Right (T.unlines [ "* read the docs", ":PROPERTIES:"
                            , ":ORG_GLANCE_CREATION_TIME: [2026-08-01 Sat 09:30]", ":END:" ]))
          (captured "" "read the docs")

      -- Appended bare to a file whose last line has no newline, the stars would
      -- land on the end of a live line and be no headline at all.
    , testCase "a file not closed with a newline gets one first" $
        assertEqual "the newline is the first thing written"
          (Right (T.unlines [ "tail", "* note", ":PROPERTIES:"
                            , ":ORG_GLANCE_CREATION_TIME: [2026-08-01 Sat 09:30]", ":END:" ]))
          (captured "tail" "note")

    , testCase "the text is stripped and is otherwise raw org" $
        assertContains "written as spelled" "* [#A] TODO ship :web:\n"
          (fromRight "" (captured "" "  [#A] TODO ship :web:  "))

      -- The entry's lines end the way the target's own do, so a capture into a
      -- CRLF file leaves one rather than a file with two kinds of line in it.
    , testCase "into a CRLF file, the entry is CRLF too" $
        assertEqual "every line the target's own ending"
          (Right (T.intercalate "\r\n"
                    [ "* old", "* note", ":PROPERTIES:"
                    , ":ORG_GLANCE_CREATION_TIME: [2026-08-01 Sat 09:30]", ":END:", "" ]))
          (captured "* old\r\n" "note")

      -- The entry this command promises is ONE headline, so the two ways of
      -- making it something else are refused rather than written.
    , testCase "an empty line and a multi-line one are refused" $
        mapM_ (\(what, text') -> case captured "" text' of
                 Right doc -> assertFailure (what <> ": wrote " <> show doc)
                 Left _why -> pure ())
              [ ("empty", ""), ("blank", "   ")
              , ("two headlines", "one\n* two"), ("a body line", "one\nbody") ]

      -- The stamp is org's INACTIVE form: a creation time is a record of when a
      -- row was written rather than something to turn up on an agenda.
    , testCase "the stamp is org's inactive timestamp, to the minute" $
        assertEqual "as org-glance's own store spells it"
                    "[2026-08-01 Sat 09:30]" (captureStamp stampedAt)

      -- And it is a stamp ORG READS.  Nothing else asks: the capture renders
      -- this straight into the drawer, so a bracket org does not know would be
      -- written and only noticed by the next load.
    , testCase "and org reads the stamp back" $
        assertBool "the creation stamp reparses" (readsAsTimestamp (captureStamp stampedAt))
    ]

    -- ORG'S PRIORITY TOKEN, which a key CYCLES rather than a reader types.  The
    -- three shapes are `set-state''s three read one part along, and every case
    -- asserts the whole document: what the edit must NOT touch is the keyword in
    -- front of it and the title behind it.
  , testGroup "set-priority"
    [ testCase "over a token already there, replaces exactly it" $
        setPriorityIs "replaced" (keyworded "* NEXT [#A] Ship it :web:\n") (Just "B")
                                 (keyworded "* NEXT [#B] Ship it :web:\n")

      -- Org writes `* TODO [#A] Title', so the token follows the state.
    , testCase "with none, inserts behind the keyword" $
        setPriorityIs "after the keyword" (keyworded "* NEXT Ship it\n") (Just "A")
                                          (keyworded "* NEXT [#A] Ship it\n")

    , testCase "and behind the stars where there is no keyword" $
        setPriorityIs "after the stars" "* Ship it\n" (Just "C") "* [#C] Ship it\n"

      -- The horizontal run goes with it, so the title closes up.
    , testCase "a null takes the token and the space behind it" $
        setPriorityIs "cleared" (keyworded "* NEXT [#A] Ship it\n") Nothing
                                (keyworded "* NEXT Ship it\n")

    , testCase "and the whole run of it, however wide" $
        setPriorityIs "cleared wide" "* [#A]   Ship it\n" Nothing "* Ship it\n"

      -- Which is what lets the ring's wrap THROUGH NONE be pressed twice.
    , testCase "clearing a headline that carries none costs no edit" $
        withRecord "* Plain\n" $ \r ->
          assertEqual "no edits" (Right []) (setPriorityEdits Nothing r)

    , testCase "the letter is uppercased and stripped" $
        setPriorityIs "folded" "* Plain\n" (Just "  b  ") "* [#B] Plain\n"

      -- One ASCII letter, and the CYCLE is the reader's: a tree using `D' is
      -- writable here and simply carries no badge.
    , testCase "anything that is not one letter is refused, by name" $
        mapM_ (\(text', wanted) ->
                 assertEqual (show text') wanted (isRight (priorityText text')))
              [ ("A", True), ("c", True), ("D", True), ("", False)
              , ("AB", False), ("1", False), ("[#A]", False) ]
    ]

    -- The one CELL a reader edits as text.  The span is the title's own, so
    -- every case here asserts the whole document: what the edit must NOT touch
    -- is the keyword in front of it and the tag run behind it.
  , testGroup "set-title"
    [ testCase "replaces exactly the title, between the keyword and the tags" $
        setTitleIs "replaced" (keyworded "* NEXT [#A] Ship it :web:\n") "Ship it now"
                              (keyworded "* NEXT [#A] Ship it now :web:\n")

    , testCase "over a bare title" $
        setTitleIs "bare" "* Plain\n" "Renamed" "* Renamed\n"

      -- The insertion goes behind the last part org writes AHEAD of a title, so
      -- a headline that has none grows one where org would have written it.
      -- 'titleLineEnd' cannot serve: its answer includes the TAGS, and a title
      -- written past a run would be read back as tag text on the next load.
    , testCase "a headline with no title grows one behind its priority" $
        setTitleIs "after the priority" "* TODO [#B]\n" "Ship it"
                                        "* TODO [#B] Ship it\n"

    , testCase "and behind its keyword where it has no priority" $
        setTitleIs "after the keyword" "* TODO\n" "Ship it" "* TODO Ship it\n"

      -- With neither in front of it the separator is the run org already writes
      -- after the stars, so the title goes PAST it rather than growing a second
      -- space.  The one shape that reaches this and is still a row: a headline
      -- whose only content is a planning entry.
    , testCase "and past the stars' own space where it has neither" $
        setTitleIs "after the stars" "* \nSCHEDULED: <2026-08-05 Wed>\n" "Ship it"
                                     "* Ship it\nSCHEDULED: <2026-08-05 Wed>\n"

      -- A titleless headline carrying TAGS is not a shape org writes: the parser
      -- hands `* TODO :web:' its colons as the TITLE, which is the same rule
      -- 'blankEntry' rests on.  So the tags branch is unreachable from this side
      -- as well, and the case below is what it looks like instead.
    , testCase "a run of colons with no title in front of it IS the title" $
        setTitleIs "the colons were the title" "* TODO :web:\n" "Ship it"
                                               "* TODO Ship it\n"

    , testCase "the text is stripped" $
        setTitleIs "stripped" "* Plain\n" "   Renamed  " "* Renamed\n"

      -- Two rules and no third: a headline with no title is a blank entry and
      -- no longer a row, and a second line is not part of this one.
      -- BY NAME means the message says which rule refused, and a non-empty
      -- string says only that something did: every refusal in this module
      -- passed that, so the claim in the case's own name went unchecked.  The
      -- two rules name themselves — a title is needed, and it is one line.
    , testCase "an empty title and a multi-line one are refused, by name" $
        withRecord "* Plain\n" $ \r ->
          mapM_ (\(what, text', named) -> case setTitleEdits text' r of
                   Right edits -> assertFailure (what <> ": " <> show edits)
                   Left why    -> assertBool (what <> ": " <> T.unpack why)
                                             (named `T.isInfixOf` why))
                [ ("empty", "", "needs a title")
                , ("blank", "   ", "needs a title")
                , ("two lines", "one\ntwo", "is one line") ]

      -- The wall is one function, so what the route refuses ahead of the write
      -- and what the span math refuses are the same answer.
    , testCase "the wall is titleText, and the route reads the same one" $
        mapM_ (\(text', wanted) ->
                 assertEqual (show text') wanted (isRight (titleText text')))
              [ ("Ship it", True), ("  padded  ", True), ("", False)
              , ("   ", False), ("one\ntwo", False) ]
    ]
  ]

-- | WHAT: DOC with @set-planning KEYWORD STAMP@ applied to its one headline is
-- WANTED.
planningIs :: String -> Text -> Maybe Text -> Text -> Text -> Assertion
planningIs what keyword stamp doc = triedEditsAre what doc (setPlanningEdits keyword stamp)

-- | The day every relative date here is worked out from, and the moment every
-- capture is stamped with: a Saturday, so @+1m@ lands on a different weekday and
-- the computed one is doing work.
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

-- | DOC with TEXT captured into it, at 'stampedAt'.
captured :: Text -> Text -> Either Text Text
captured doc text' = splice doc <$> captureEdits doc (captureStamp stampedAt) text'

-- | WHAT: DOC with @set-priority LETTER@ applied to its one headline is WANTED.
setPriorityIs :: String -> Text -> Maybe Text -> Text -> Assertion
setPriorityIs what doc letter = triedEditsAre what doc (setPriorityEdits letter)

-- | WHAT: DOC with @set-title TITLE@ applied to its one headline is WANTED.
setTitleIs :: String -> Text -> Text -> Text -> Assertion
setTitleIs what doc title = triedEditsAre what doc (setTitleEdits title)

-- Subtree entries
--
-- The sub-addressing @?child=K@ rests on: which headlines are inside a row's
-- subtree, in what order they are numbered, what each one hangs under, and where
-- each one's own extent runs.  A row keeps only its own headline, so all four
-- come out of a re-parse, and what is asserted here is that the re-parse agrees
-- with the load — the extents are org's outline rule over the whole document,
-- the same one the rows themselves are cut by.

-- | An outline with a level jump in it, which is what makes the parent rule
-- more than "one level up": @four@ hangs under @one@ across the gap that
-- @three@ leaves, and the second root is outside the first's subtree entirely.
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

    -- The second root is a row of its own, so it is nobody's descendant.
  , testCase "and nothing past it" $
      withEntries deep $ \_r entries ->
        assertBool "five is not inside one"
                   ("five" `notElem` map (hrTitle . seRecord) entries)

  , testCase "each one's level, as org spells it" $
      withEntries deep $ \_r entries ->
        assertEqual "the stars counted" [2, 3, 2] (map seLevel entries)

    -- The nearest SHALLOWER entry, which is what a level jump needs: `four'
    -- hangs under the row across the gap `three' left open.
  , testCase "each one's parent is the nearest shallower entry" $
      withEntries deep $ \_r entries ->
        assertEqual "-1 is the row itself" [-1, 0, -1] (map seParent entries)

    -- The extent is org's outline rule, so a child's slice covers its own
    -- descendants and stops at the next headline at its level or shallower.
  , testCase "each one's extent is its own subtree" $
      withEntries deep $ \_r entries ->
        assertEqual "two carries three, three carries its body, four is one line"
          [ "** two\n*** three\nbody of three\n"
          , "*** three\nbody of three\n"
          , "** four\n" ]
          (map (subtreeText . seRecord) entries)

    -- The lens over a child is the lens: the same three regions, cut out of the
    -- child's own extent.
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

    -- Decompose then recompose is the identity on a CHILD too, which is what
    -- makes a child commit a splice of its own extent rather than a rewrite.
  , testCase "and decompose then recompose is the identity on it" $
      withEntries deep $ \_r entries ->
        mapM_ (\e -> let rec' = seRecord e
                     in assertEqual (T.unpack (hrTitle rec'))
                                    (subtreeText rec')
                                    (recomposedSubtree rec' (headlineParts rec')))
              entries

    -- The digest is the FILE's, so a child's write is pinned to the same lock
    -- the row's is: one file, one digest, whichever entry the sheet is on.
  , testCase "a child pins the file's own digest" $
      withEntries deep $ \r entries ->
        assertEqual "the row's" (replicate (length entries) (hrDigest r))
                    (map (hrDigest . seRecord) entries)

    -- The id exists to be readable in a refusal; nothing resolves one.
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

    -- The cells are the loader's: a child carries a keyword the file declares,
    -- and the same tag reading a row's does.
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

-- | CAPTURE: the template grammar, the blob a tagged capture composes, and the
-- store layout it writes to.
--
-- The expansion subset is the whole of what this repo reads out of
-- org-capture's language, so every code it names has a case and so does the
-- rule for everything it does not.
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

        -- The weekday is COMPUTED, like every other stamp this library writes,
        -- and the two differ in their brackets and in nothing else.
      , testCase "a template spelling one twice stamps one moment" $
          assertEqual "one clock read per request"
                      (Right "* x [2026-08-04 Tue 09:30] [2026-08-04 Tue 09:30]")
                      (expanded [] "x" "* %? %U %U")

      , testCase "%^{PROMPT} takes its answer out of the fields" $
          assertEqual "the answer, verbatim" (Right "* x\n:AUTHOR: Frank Herbert")
                      (expanded [("Author", "Frank Herbert")] "x" "* %?\n:AUTHOR: %^{Author}")

        -- One question, both places filled: a prompt spelled twice is asked once
        -- and answered everywhere it stands.
      , testCase "a prompt spelled twice is one ask and two fills" $ do
          assertEqual "asked once" ["Author"] (templatePrompts "* %? %^{Author} %^{Author}")
          assertEqual "filled twice" (Right "* x a a")
                      (expanded [("Author", "a")] "x" "* %? %^{Author} %^{Author}")

        -- EVERYTHING ELSE COPIES THROUGH, which is the rule that keeps a
        -- template using a code this server has never heard of readable rather
        -- than silently emptied.
      , testCase "a code outside the subset is written as it stands" $
          mapM_ (\template ->
                   assertEqual (T.unpack template) (Right ("* x" <> T.drop 4 template))
                               (expanded [] "x" template))
                [ "* %?%a", "* %?%^g", "* %?%^{unclosed", "* %?%%", "* %?%" ]

      , testCase "and a % that opens nothing is a %" $
          assertEqual "trailing" (Right "* x %") (expanded [] "x" "* %? %")

        -- THE LIST AND THE GRAMMAR ARE TWO SPELLINGS.  'captureCodes' is what
        -- @GET /capture@ serves and the settings box completes over; the scan
        -- spells the same four out as a case and never consults the list.  So
        -- every code the list advertises is put through the scan here: one the
        -- list gained and the scan did not would copy through as itself, which
        -- is an expansion offered to a reader and written literally.
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

        -- The refusal is the WHOLE request's: half an entry with a hole in it is
        -- worse than none of one.
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

        -- The tag's own layer first, the system layer's next, and nothing after
        -- that: the same chain the keywords beside it are resolved by.
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
        -- The file's own last newline is OUTSIDE the extent, so a replacement
        -- leaves it: what the sheet edits is the template, never the byte that
        -- ends the file.
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

        -- ONE WALL, and it is what keeps a blob's first headline the entry
        -- org-glance keys it by.
      , testCase "a template that is not one top entry is refused" $
          mapM_ (\want -> assertBool (T.unpack want)
                            (either (const True) (const False)
                                    (captureTemplateEdits bookLayer want)))
                ["body text", "** %?", "*%?", "  * %?"]

        -- A ROUND TRIP: what the settings sheet is shown is what it can write
        -- back, byte for byte.
      , testCase "read then written back leaves the file alone" $
          assertEqual "byte for byte" (Right bookLayer)
                      (templated bookLayer (fromMaybe "" (captureTemplateOf bookLayer)))
      ]

  , testGroup "The blob a tagged capture composes"
      [ testCase "the tag joins the headline and the drawer carries the id" $
          assertEqual "org-glance's own two properties"
                      (Right (T.unlines [ "* milk :book:"
                                        , ":PROPERTIES:"
                                        , ":ORG_GLANCE_ID: i-1"
                                        , ":ORG_GLANCE_CREATION_TIME: [2026-08-04 Tue 09:30]"
                                        , ":END:" ]))
                      (blobDocument (BlobSeed "book" "i-1" "[2026-08-04 Tue 09:30]") "* milk")

        -- A template carrying a drawer of its own keeps it, and the two
        -- properties join it rather than opening a second one.
      , testCase "a drawer the template wrote is joined rather than doubled" $
          assertEqual "one drawer"
                      (Right (T.unlines [ "* milk :book:"
                                        , ":PROPERTIES:"
                                        , ":ORG_GLANCE_ID: i-1"
                                        , ":ORG_GLANCE_CREATION_TIME: [s]"
                                        , ":AUTHOR: X"
                                        , ":END:" ]))
                      (blobDocument (BlobSeed "book" "i-1" "[s]")
                                    "* milk\n:PROPERTIES:\n:AUTHOR: X\n:END:\n")

      , testCase "a headline already wearing the tag costs no edit" $
          assertEqual "the run keeps its bytes"
                      (Right (T.unlines [ "* milk :book:web:"
                                        , ":PROPERTIES:"
                                        , ":ORG_GLANCE_ID: i-1"
                                        , ":ORG_GLANCE_CREATION_TIME: [s]"
                                        , ":END:" ]))
                      (blobDocument (BlobSeed "book" "i-1" "[s]") "* milk :book:web:")

      , testCase "and one wearing others joins the run's end" $
          assertBool "appended to the run"
                     (either (const False) (T.isInfixOf "* milk :web:book:")
                             (blobDocument (BlobSeed "book" "i-1" "[s]") "* milk :web:"))

        -- The template's own children ride along: a blob is the whole entry.
      , testCase "the template's children are the entry's" $
          assertBool "the child survives"
                     (either (const False) (T.isInfixOf "*** Notes")
                             (blobDocument (BlobSeed "book" "i-1" "[s]") "* Book\n*** Notes\n    milk"))

        -- THE DRAWER GOES UNDER THE PLANNING LINE, which is where org puts one:
        -- spliced between the headline and its `SCHEDULED:' the planning line
        -- stops being the line after the title and is read as body text.
      , testCase "a template with a planning line keeps it under the title" $
          assertEqual "planning first, drawer second"
                      (Right (T.unlines [ "* milk :book:"
                                        , "SCHEDULED: <2026-08-10 Mon>"
                                        , ":PROPERTIES:"
                                        , ":ORG_GLANCE_ID: i-1"
                                        , ":ORG_GLANCE_CREATION_TIME: [s]"
                                        , ":END:" ]))
                      (blobDocument (BlobSeed "book" "i-1" "[s]")
                                    "* milk\nSCHEDULED: <2026-08-10 Mon>\n")

      , testCase "the document ends in a newline" $
          assertBool "an org file's last line is ended"
                     (either (const False) (T.isSuffixOf "\n")
                             (blobDocument (BlobSeed "book" "i-1" "[s]") "* milk"))
      ]

  , testGroup "Where a blob sits"
      [ testCase "sharded by the id's first two characters, verbatim" $
          assertEqual "org-glance's own layout"
                      "/o/.org-glance/data/04/a14d10-41c1-4a3d/data.org"
                      (blobPathIn "/o/.org-glance" "04a14d10-41c1-4a3d")

        -- NOT FOLDED: org-glance's own store carries `Pa', `Pe' and `al' shards
        -- side by side, an id being an opaque string wherever it is read.
      , testCase "the shard is not folded" $
          assertEqual "Password- shards under Pa"
                      "/o/.org-glance/data/Pa/ssword-1/data.org"
                      (blobPathIn "/o/.org-glance" "Password-1")

      , testCase "an id of two characters or fewer is not sharded" $ do
          assertEqual "two" "/s/data/ab/data.org" (blobPathIn "/s" "ab")
          assertEqual "three" "/s/data/ab/c/data.org" (blobPathIn "/s" "abc")

      , testCase "the store root is the served root's own" $
          assertEqual "one tree, one store" "/o/.org-glance" (storeRootIn "/o")

        -- A blob path is one this walk COLLECTS and this note-taker names, which
        -- is what makes the row arrive and the EXTERNAL line get written.
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
    -- ONE clock for every expansion case, so a stamp is an assertion rather than
    -- a moving target.
    noon = Time.ZonedTime (Time.LocalTime (Time.fromGregorian 2026 8 4)
                                          (Time.TimeOfDay 9 30 0))
                          Time.utc
    expanded answers text = expandTemplate noon answers text
    -- The module's own splice, which every other command case here asserts
    -- through: an oracle that shared the write engine would agree with a wrong
    -- offset.
    templated doc want = splice doc <$> captureTemplateEdits doc want
    bookLayer = "#+TITLE: Book\n#+TODO: TODO | DONE\n\n* Book\n*** Notes\n    %?\n"
    layers =
      [ ConfigLayerFile "/o/.org-glance/config/system.org" Nothing "s" "#+TITLE: X\n* %? %U\n"
      , ConfigLayerFile "/o/.org-glance/config/tags/book.org" (Just "book") "b" bookLayer
      , ConfigLayerFile "/o/.org-glance/config/tags/film.org" (Just "film") "f" "#+TODO: A | B\n"
      ]
