-- | The filter query language: the tokenizer as a pure function, and the
-- semantics over a loaded fixture.
--
-- The grammar is @table-view\/SCHEMA.md@'s ("Filter query") and the renderer
-- implements it too, so what is asserted here is the contract rather than this
-- port of it: the expected tokens and the expected matches are written down,
-- not derived from the code under test.
module TestFilter (spec) where

import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults (columnKeysOf, orgFile, viewDir, withTempDir)

import qualified Data.Text as T

import Glance.Query ( HeadlineRecord (..), QueryResult (qrRecords), displayText
                    , loadDir, matchesSearch, refTargetOf, refTargets, tagsOfCell
                    , viewJSON )
import Glance.Web.Filter ( Term (..), Token (..), archiveKey, cellAt
                         , emptyEnv, filterKeys, matchesFilter, namesArchive
                         , parseFilter, plannedKey, refKey, scanQuery, storeEnv
                         , tagsKey )

-- Fixtures
--
-- 'viewDir' is the suite's sample directory: six headlines, five states between
-- them, one of them stateless, and a @#+TODO:@ line that puts three keywords in
-- the active set and two in the done-like one.

-- | The titles of the fixture's rows, in walk order — what a match is reported
-- as, since four of the six rows have only their offset for an id.
data Row = Ship | Privet | Reply | Plain | Drop | Schema
  deriving (Bounded, Enum, Eq, Ord, Show)

titleOf :: Row -> Text
titleOf Ship   = "Ship the table view"
titleOf Privet = "Привет мир"
titleOf Reply  = "Reply from the печатник"
titleOf Plain  = "Plain headline without a state"
titleOf Drop   = "Drop the old renderer"
titleOf Schema = "Read the schema"

-- | The tags the fixture's rows carry, derived here the way the store derives
-- its own list.  No predicate reads it any more — an org tag is not a key — and
-- what it is still for is saying so ('tagsSpec').
vocabularyOf :: [HeadlineRecord] -> [Text]
vocabularyOf = sort . nub . concatMap (tagsOfCell . hrTags)

-- | The rows Q matches, in walk order.
matching :: Text -> IO [Row]
matching q = do
  records <- qrRecords <$> loadDir viewDir
  let hit = [ hrTitle r | r <- records, matchesFilter (storeEnv records) q r ]
  mapM (named records) hit
  where
    named records t = case [ row | row <- [minBound ..], titleOf row == t ] of
      [row]  -> pure row
      _other -> assertFailure ("the fixture moved: no row named " <> show t
                                 <> " among " <> show (map hrTitle records))

-- | Q matches exactly ROWS, and nothing else.
matches :: Text -> [Row] -> Assertion
matches q rows = assertEqual (T.unpack q) rows =<< matching q

spec :: TestTree
spec = testGroup "Filter"
  [ tokenSpec, predicateSpec, tagsSpec, plannedSpec, archiveSpec, shapeSpec
  , degenerateSpec
  , targetSpec, refSpec
  , layoutSpec ]

-- References: extraction
--
-- The forms are the ones ~/sync spells, counted over the walked corpus at
-- 2026-08-02 and written down in 'Glance.Query.refPrefixes'.  These cases are
-- that census in miniature: what the corpus HAS is matched, and the two
-- org-glance protocols that name something other than a row are turned away.

-- | One link target normalized, or refused.
targetSpec :: TestTree
targetSpec = testGroup "Reference targets"
  [ testCase "the id-bearing protocols are stripped, case preserved" $
      mapM_ (\(raw, want) -> assertEqual (T.unpack raw) (Just want) (refTargetOf raw))
        [ ("org-glance-visit:task-spbm-1-2-3-0",    "task-spbm-1-2-3-0")
        , ("org-glance-open:Pets-20210816-eee5a4",  "Pets-20210816-eee5a4")
        , ("org-glance-material:contact-25053-3",   "contact-25053-3")
        -- Org's own, which the corpus does not use at all: in the list because
        -- it is org's own, and this is the case that says so.
        , ("id:9f8e7d6c",                           "9f8e7d6c")
        -- The case is the id's: a fold here would put `Password-…' out of reach.
        , ("org-glance-visit:Password-20210516-d9", "Password-20210516-d9") ]

  , testCase "the two title forms lose their star and keep their text" $ do
      assertEqual "starred" (Just "Hacking the renderer")
                  (refTargetOf "*Hacking the renderer")
      assertEqual "bare" (Just "Highlights") (refTargetOf "Highlights")

  , testCase "a protocol that names something other than a row is refused" $
      mapM_ (\raw -> assertEqual (T.unpack raw) Nothing (refTargetOf raw))
        -- `org-glance-overview:' names a TAG and `org-glance-state:' a keyword:
        -- 2726 and 880 links in the walked corpus, and between them not one
        -- target that is an ORG_GLANCE_ID.
        [ "org-glance-overview:bookmark", "org-glance-state:STARTED"
        , "file:notes.org", "https://x.example/a", "mailto:a@b.example"
        -- A bare target holding a slash is a path, which is org's implicit file
        -- link rather than a headline.
        , "docs/plan.org", "" ]

  , testCase "a subtree's targets are deduplicated and keep their order" $ do
      let text' = T.unlines
            [ "* one [[org-glance-visit:alpha][A]]"
            , "body [[org-glance-overview:tag][skipped]] and [[*Beta]]"
            , "** child [[org-glance-open:alpha][A again]]"
            , "trailing https://x.example/z" ]
      -- `alpha' arrives twice under two protocols and is kept once; the
      -- overview link and the bare URL are not references at all.
      assertEqual "targets" ["alpha", "Beta"] (refTargets text')

  , testCase "a subtree with nothing to point at yields no targets" $
      assertEqual "none" [] (refTargets "* plain\njust prose, and https://x.example\n")

    -- KNOWN LIMIT, inherited rather than introduced.  A link written INSIDE
    -- another link's description defeats the scanner twice over: the OUTER link
    -- fails to close (its description breaks at the inner link's first `]', and
    -- what follows is `][' rather than `]]'), and the rescan that follows picks
    -- the inner one up one bracket late, so its target arrives spelled `[org-…'
    -- and is refused for the leading bracket.  Neither end is a reference.
    --
    -- org-glance's own "Referred from" footer writes exactly this shape, and it
    -- is the whole of what `ref:' misses on the corpus: for the most-referenced
    -- contact in ~/sync it costs 2 files of 128 (2026-08-02, the other 126
    -- answered, 2 of them archived).  It is the `/links' grammar's own rule —
    -- `orgLinks' reports the same bracketed target — and it is reused rather
    -- than worked around, since a second scanner here would be a second grammar
    -- to keep in step with SCHEMA.md's link rule.
  , testCase "a reference nested in another link's description is not found" $
      assertEqual "neither the outer nor the inner" []
        (refTargets "- Referred from [[org-glance-visit:Meeting-1][\
                    \[[org-glance-visit:Contact-2][Wrike]] Goals]] on [2021-10-08 Fri]")
  ]

-- References: resolution
--
-- `ref:ROWID' is the one predicate a row cannot answer alone — it needs the
-- store, to learn how a link may SPELL the row named.  The fixture is three
-- rows: a target carrying an id, a referrer reaching it by that id, a referrer
-- reaching a second target by its title, and the target's own self-link.

-- | K over the fixture's records: a target carrying an id, a referrer reaching
-- it by that id, a referrer reaching a second target by its title, the target's
-- own self-link, and a row pointing nowhere.
withRefTree :: ([HeadlineRecord] -> IO a) -> IO a
withRefTree k = withTempDir $ \dir -> do
  _ <- orgFile dir "a.org" (T.unlines
         [ "* Target"
         , ":PROPERTIES:"
         , ":ORG_GLANCE_ID: alpha"
         , ":END:"
         -- The target links to ITSELF, which org-glance's own materialize
         -- footer writes: the rule says this must not make it its own referrer.
         , "see [[org-glance-visit:alpha][myself]]"
         , "* By id"
         , "points at [[org-glance-visit:alpha][the target]]"
         , "* By title"
         , "points at [[*Second]] instead"
         , "* Second"
         , "* Neither"
         , "no links here" ])
  k . qrRecords =<< loadDir dir

-- | The rows of the fixture that Q matches, by title, in walk order.
refMatching :: Text -> IO [Text]
refMatching q = withRefTree $ \records ->
  pure [ hrTitle r | r <- records, matchesFilter (storeEnv records) q r ]

-- | The id of the row titled NAME.  Looked up rather than spelled: a row with
-- no @ORG_GLANCE_ID@ falls back to @PATH#K@, and the path is a temp directory's.
idOf :: Text -> [HeadlineRecord] -> IO Text
idOf name records = case [ hrId r | r <- records, hrTitle r == name ] of
  [one]  -> pure one
  _other -> assertFailure ("the fixture moved: no row titled " <> show name)

refSpec :: TestTree
refSpec = testGroup "References"
  [ testCase "the key is spelled once, and it is not a column" $ do
      assertEqual "the key" "ref" refKey
      assertBool "and no column carries it" (refKey `notElem` filterKeys)
      -- Named by the grammar, the way `planned' is: the store decides what it
      -- RESOLVES to, never whether it parses.
      assertEqual "a predicate wherever it is written"
                  [Term False (Just "ref") "alpha"] (parsed "ref:alpha")

  , testCase "an id link makes the row that carries it a reference" $
      assertEqual "by id" ["By id"] =<< refMatching "ref:alpha"

  , testCase "a row is not its own reference" $ do
      -- `Target' links to itself and is the row being asked about, so the one
      -- answer is the OTHER row — a list of referrers that always holds the row
      -- you came from holds one useless entry.
      hit <- refMatching "ref:alpha"
      assertBool "the target is not in its own answer" ("Target" `notElem` hit)

  , testCase "a title link resolves against the target's title" $
      -- `Second' carries no ORG_GLANCE_ID, so its row id is the @PATH#K@
      -- fallback — which no file can hold a link to — and the only spelling
      -- that reaches it is its title.
      withRefTree $ \records -> do
        rid <- idOf "Second" records
        assertEqual "by title" ["By title"]
          [ hrTitle r | r <- records, matchesFilter (storeEnv records) ("ref:" <> rid) r ]

  , testCase "an id no row claims matches nothing, and does not fail" $
      -- A filter rather than a command: an unresolvable id narrows to the empty
      -- table the way `tag:nosuchtag' does, and nothing 400s.  This is what a
      -- stale `ref:' in a bookmarked URL lands on.
      assertEqual "unknown" [] =<< refMatching "ref:no-such-row"

  , testCase "the value keeps its case, alone among the predicates" $ do
      -- Every other predicate folds; a row id is exact-string, and the corpus
      -- carries ids spelled with capitals.
      assertEqual "as written" ["By id"] =<< refMatching "ref:alpha"
      assertEqual "folded differently" [] =<< refMatching "ref:ALPHA"

  , testCase "a half-typed ref narrows nothing" $ do
      all' <- refMatching ""
      assertEqual "ref: with no value" all' =<< refMatching "ref:"

  , testCase "a negated ref is every row that does not point there" $ do
      hit <- refMatching "-ref:alpha"
      assertBool "the referrer is gone" ("By id" `notElem` hit)
      assertBool "and the target is still here" ("Target" `elem` hit)

  , testCase "two refs AND, the way a list-valued key does" $
      -- `ref' reads a LIST — the targets a subtree points at — so naming two is
      -- a row pointing at both, and no row here points at both targets.
      withRefTree $ \records -> do
        rid <- idOf "Second" records
        assertEqual "both" []
          [ hrTitle r | r <- records
          , matchesFilter (storeEnv records) ("ref:alpha ref:" <> rid) r ]

  , testCase "without a store behind it a ref resolves to nothing" $ do
      -- `emptyEnv' is what a caller holding no rows answers with: the term
      -- still parses as a predicate, and matches no row.
      records <- qrRecords <$> loadDir viewDir
      assertEqual "no rows" []
        [ hrTitle r | r <- records, matchesFilter emptyEnv "ref:alpha" r ]
  ]

-- | @planned@: the virtual key over the two date columns together.
--
-- Decidable from a row's own cells, which is what makes it a key both sides of
-- the wire can carry — no keyword set, no vocabulary and no clock.  The
-- fixture has one row with both dates, one with a schedule, one with a
-- deadline, one with a @CLOSED:@ stamp and no column at all, and two with
-- nothing, so every branch has a row of its own.
plannedSpec :: TestTree
plannedSpec = testGroup "Planned"
  [ testCase "the key is spelled once, and it is not a column" $ do
      assertEqual "the key" "planned" plannedKey
      assertBool "and no column carries it" (plannedKey `notElem` filterKeys)
      -- Named by the grammar rather than by the tree: it stands over the
      -- columns, and nothing a tree carries can add a key or take one away.
      assertEqual "a predicate with nothing loaded"
                  [Term False (Just "planned") "none"] (parsed "planned:none")

  , testCase "a row is planned when either date cell holds anything" $ do
      matches "-planned:none" [Ship, Privet, Reply]
      -- Ship carries both, Privet a schedule, Reply a deadline.  Drop's
      -- `CLOSED:' is neither column, so it is not a plan.
      matches "planned:none" [Plain, Drop, Schema]

  , testCase "and neither date column alone answers the same question" $ do
      matches "-scheduled:none" [Ship, Privet]
      matches "-deadline:none" [Ship, Reply]

  , testCase "a value is the date prefix, asked of both cells at once" $ do
      matches "planned:2026-08" [Ship, Privet, Reply]
      -- The month a schedule falls in, and the month a deadline falls in.
      matches "planned:2026-08-0" [Ship, Privet]
      matches "planned:2026-08-10" [Reply]
      -- Prefix, like the columns it stands over: no substring out of the middle.
      matches "planned:03" []

  , testCase "an empty value narrows nothing, as every key's does" $ do
      every <- matching ""
      matches "planned:" every

  , testCase "two of them are either, the way the date columns are" $
      matches "planned:2026-08-03 planned:2026-08-10" [Privet, Reply]

  , testCase "negation composes with everything else" $ do
      -- The agenda's own query: the active rows carrying a date.
      matches "state:*active* -planned:none" [Ship, Privet, Reply]
      matches "state:*inactive* -planned:none" []
      matches "-planned:2026-08" [Plain, Drop, Schema]

  , testCase "a tree tagged :planned: cannot take the key away" $
      -- There is nothing left to shadow: a tag is not a key, so the only
      -- reading of the token is the date one.
      assertEqual "still the date key" [Term False (Just "planned") "none"]
                  (parsed "planned:none")
  ]

-- | Which queries turn the served view's archive exclusion off
-- ('Glance.Web.Filter.namesArchive').  The exclusion itself is
-- @\/headlines@'s (@TestServe@); what belongs here is the reading of the
-- query, since it is the grammar answering.
archiveSpec :: TestTree
archiveSpec = testGroup "Archive key"
  [ testCase "is an ordinary value of the tag column, folded like every cell" $ do
      assertEqual "the value" "archive" archiveKey
      assertEqual "and the key it is named under" "tag" tagsKey

  , testCase "every spelling of the predicate counts as naming it" $
      mapM_ (\q -> assertBool (show q <> " did not read as naming the tag")
                             (namesArchive [archiveKey] q))
            [ "tag:archive", "-tag:archive", "state:DONE tag:archive"
            , "tag=archive", "tag:\"archive\"", "tag:ARCHIVE" ]

  , testCase "and a query that says nothing about it does not" $
      mapM_ (\q -> assertBool (show q <> " read as naming the tag")
                             (not (namesArchive [archiveKey] q)))
            -- Free text is not a predicate, quoted text never is, another
            -- column is another cell, and the bare tag key is gone: with tags
            -- out of the grammar, `archive:draft' is text like any other.
            [ "", "archive", "\"tag:archive\"", "archive:", "archive:draft"
            , "state:DONE", "title:archive"
            -- WHOLE value, where the predicate itself is a substring of the
            -- cell: `tag:arch' finds an archived row and leaves the exclusion
            -- on, so it answers empty rather than half-answering.
            , "tag:arch", "tag:archived" ]

    -- With nothing archived in the tree the word names no tag of it, and this
    -- is False — sound, since there is nothing for the exclusion to hide.
  , testCase "with the tag nowhere in the tree, naming it counts for nothing" $
      assertBool "read as naming a tag against an empty vocabulary"
                 (not (namesArchive [] "tag:archive"))
  ]

-- Tags are not keys

-- | An org tag names no filter key.  @tag:course@ is the one spelling, and the
-- facet-then-search a tag tree gives an org user is the two tokens
-- @tag:course text@ — a predicate reads one cell, free text reads the row.  The
-- fixture's tags are @cleanup@, @glance@, @unicode@ and @web@, and none of them
-- is a key.
tagsSpec :: TestTree
tagsSpec = testGroup "Tags are not keys"
  [ testCase "a tag key is free text, colon and all" $ do
      assertEqual "a tag of the tree" [Term False Nothing "web:ship"] (parsed "web:ship")
      assertEqual "and one it does not carry" [Term False Nothing "contact:tanik"]
                                              (parsed "contact:tanik")
      -- What the facet used to answer, and what the text now does.
      matches "web:schema" []
      matches "web:ship" []
      matches "contact:tanik" []

  , testCase "and matched as the text it is, org's own colons included" $
      -- A coincidence worth writing down: org spells a tags cell `:web:', so
      -- the free text `web:' is inside the cell of every row carrying the tag.
      -- The facet is gone all the same — a value after it no longer scopes.
      matches "web:" [Ship, Schema]

  , testCase "tag: is the one spelling, and it is the column's" $ do
      matches "tag:web" [Ship, Schema]
      matches "tag:glance" [Ship]
      matches "tag:unicode" [Privet]
      matches "tag:none" [Reply, Plain]

  , testCase "the facet and the search are two tokens now" $ do
      -- `web:schema' was one; `tag:web schema' is what it meant, and reads the
      -- same rows.  The text is the whole row, so it searches every cell.
      matches "tag:web schema" [Schema]
      matches "tag:web ship" [Ship]
      matches "tag:web 2026-08-01" [Ship]
      matches "tag:cleanup renderer" [Drop]
      matches "tag:web renderer" []

  , testCase "which costs the whole-tag reading, the column being a substring" $ do
      -- `glan:' was no tag and matched nothing; `tag:glan' is a substring of
      -- `:web:glance:' and matches.  The wider reading is the tag column's own,
      -- and it is the reading the renderer has for that column too.
      matches "glan:" []
      matches "tag:glan" [Ship]

  , testCase "the tags a tree carries change no answer at all" $ do
      -- The one divergence this removal was for: the keys a query could name
      -- were the loaded rows' tags on one side of the wire and the whole
      -- store's on the other.  Now neither side has any, and the environment a
      -- query is matched in cannot carry a tag list to disagree over — it is
      -- the rows alone ('storeEnv'), which is what makes this structural rather
      -- than a pair of answers that happen to agree.
      records <- qrRecords <$> loadDir viewDir
      assertEqual "the fixture's tags" ["cleanup", "glance", "unicode", "web"]
                  (vocabularyOf records)
      -- Every one of those tags, spelled as the key it used to be, resolves to
      -- free text — including the two a column shares no name with.
      assertEqual "no tag is a key"
        [ [Term False Nothing (t <> ":x")] | t <- vocabularyOf records ]
        [ parsed (t <> ":x") | t <- vocabularyOf records ]

  , testCase "and a column can no longer be shadowed by one" $ do
      -- A file tagged `:title:' could once have taken the column's key away.
      assertEqual "the column" [Term False (Just "title") "x"] (parsed "title:x")
      assertEqual "the tag, as text" [Term False Nothing "glance:x"] (parsed "glance:x")
  ]

-- Tokenizer

-- | What the scanner cuts out of a query, and what the tokens resolve to
-- against the view's columns.  Org text is the trap this grammar is shaped
-- around: a tag string and a verbatim run both carry the separator characters,
-- and neither is a predicate.
tokenSpec :: TestTree
tokenSpec = testGroup "Tokens"
  [ testCase "a bare word is free text" $
      assertEqual "tokens" [Token False False "tanik"] (scanQuery "tanik")

  , testCase "tokens separate on whitespace and on &" $
      assertEqual "tokens"
        [Token False False "a", Token False False "b", Token False False "c"]
        (scanQuery "a b&c")

  , testCase "runs of separators collapse, and the ends are trimmed" $
      assertEqual "tokens" [Token False False "a", Token False False "b"]
        (scanQuery "  a \t&& b\n")

  , testCase "an empty query has no tokens" $ do
      assertEqual "empty" [] (scanQuery "")
      assertEqual "blank" [] (scanQuery "  & ")

  , testCase "a quoted token keeps its spaces and drops its quotes" $
      assertEqual "tokens" [Token False True "the table"] (scanQuery "\"the table\"")

  , testCase "an unclosed quote runs to the end, so typing one loses nothing" $
      assertEqual "tokens" [Token False True "the tab"] (scanQuery "\"the tab")

  , testCase "a leading - negates, and a - inside a word does not" $ do
      assertEqual "negated" [Token True False "web"] (scanQuery "-web")
      assertEqual "hyphenated" [Token False False "no-such-row"] (scanQuery "no-such-row")
      assertEqual "negated quote" [Token True True "the table"] (scanQuery "-\"the table\"")

  , testCase "org tag text is not a predicate" $ do
      assertEqual ":work: stays text" [Term False Nothing ":work:"] (parsed ":work:")
      assertEqual "=code= stays text" [Term False Nothing "=code="] (parsed "=code=")

  , testCase "key:value is a predicate only for a column of the view" $ do
      assertEqual "a column" [Term False (Just "state") "TODO"] (parsed "state:TODO")
      assertEqual "not a column" [Term False Nothing "note:later"] (parsed "note:later")
      assertEqual "a URL is text" [Term False Nothing "http://example.org"]
                                  (parsed "http://example.org")

  , testCase "= is an alias for :" $
      assertEqual "term" [Term False (Just "state") "active"] (parsed "state=active")

  , testCase "the first separator splits, so a value may carry more" $
      assertEqual "term" [Term False (Just "title") "a:b"] (parsed "title:a:b")

  , testCase "a token that opens with a quote is free text, predicate or not" $
      assertEqual "term" [Term False Nothing "state:TODO"] (parsed "\"state:TODO\"")

  , testCase "a predicate's value may be quoted" $
      assertEqual "term" [Term False (Just "tag") "two words"] (parsed "tag:\"two words\"")

  , testCase "negation carries the whole token, either form" $
      assertEqual "terms" [Term True (Just "state") "DONE", Term True Nothing "web"]
                          (parsed "-state:DONE -web")

  , testCase "the keys are the view's own column keys" $ do
      view <- viewJSON "t" . qrRecords <$> loadDir viewDir
      keys <- columnKeysOf view
      assertEqual "columns" keys filterKeys
  ]

-- | Q parsed: the tokenizer's own subject, where the only thing that makes a
-- predicate is a column, @planned@ or @ref@.
parsed :: Text -> [Term]
parsed = parseFilter

-- Field predicates

-- | One group per column type SCHEMA.md names, plus the two meta-values this
-- producer adds and the @none@ every type shares.
predicateSpec :: TestTree
predicateSpec = testGroup "Predicates"
  [ testCase "state is a whole value, case-insensitively" $ do
      matches "state:TODO" [Privet]
      matches "state:todo" [Privet]
      matches "state:DONE" [Schema]
      -- Whole value, so a prefix of a keyword is not one of them.
      matches "state:TOD" []

  , testCase "state:active and state:inactive are the file's keyword groups" $ do
      -- #+TODO: NEXT WAITING | CANCELLED, over the seeded TODO/DONE.  The
      -- stateless row rides with the active ones; see below.
      matches "state:active" [Ship, Privet, Reply, Plain]
      matches "state:inactive" [Drop, Schema]

  , testCase "the stateless row is active, and it is not inactive" $ do
      -- No scope classifies a headline that carries no keyword, so it is in
      -- neither group — and `*active*' takes it anyway, since an entry nobody
      -- has stated is live work and the default view is what would otherwise
      -- hide it.  `*inactive*' does not: an entry nobody marked done is not
      -- done, so the two groups do not partition the column.
      matches "state:none" [Plain]
      matches "state:*active*" [Ship, Privet, Reply, Plain]
      matches "state:*inactive*" [Drop, Schema]
      -- Which makes `none' a subset of `*active*' rather than a third group,
      -- and makes the negation drop the empty cell along with the keywords.
      matches "-state:*active*" [Drop, Schema]
      matches "-state:*inactive*" [Ship, Privet, Reply, Plain]

  , testCase "and answer to org-glance's starred spelling of the same groups" $ do
      -- `*active*' is what org-glance calls the group and what the view offers
      -- for completion, so it is the canonical spelling; the bare one above
      -- stays an alias.
      matches "state:*active*" [Ship, Privet, Reply, Plain]
      matches "state:*inactive*" [Drop, Schema]
      matches "state:*ACTIVE*" [Ship, Privet, Reply, Plain]
      -- Stars are not a glob: they come off these two values and nothing else,
      -- so a starred keyword is the literal badge text, which no cell holds.
      matches "state:*TODO*" []
      matches "state:*none*" []
      -- One matched pair, so a half-starred value is literal too.
      matches "state:*active" []
      matches "state:active*" []

  , testCase "priority is the letter, case-insensitively" $ do
      matches "priority:A" [Ship]
      matches "priority:a" [Ship]
      matches "priority:c" [Drop]
      matches "priority:none" [Reply, Plain, Schema]

  , testCase "title and tag are substrings of the cell, case-insensitively" $ do
      matches "title:the" [Ship, Reply, Drop, Schema]
      matches "title:SHIP" [Ship]
      matches "title:привет" [Privet]
      matches "tag:web" [Ship, Schema]
      matches "tag:none" [Reply, Plain]
      -- A predicate reads one cell: the word is in another row's title.
      matches "tag:renderer" []

  , testCase "a title predicate sees the cell as it displays" $ do
      matches "title:schema" [Schema]
      -- The scheduled cell holds this, and the title does not.
      matches "title:2026" []

  , testCase "dates match by prefix, so a month is a month" $ do
      matches "scheduled:2026-08" [Ship, Privet]
      matches "scheduled:2026-08-03" [Privet]
      matches "deadline:2026" [Ship, Reply]
      matches "scheduled:none" [Reply, Plain, Drop, Schema]
      -- Prefix, so the day is not matched out of the middle of the cell.
      matches "scheduled:03" []

  , testCase "a value with nothing typed narrows nothing" $ do
      every <- matching ""
      matches "state:" every
      matches "scheduled:" every
      -- Which is the state the suggestion list exists to serve.
      matches "state: title:the" [Ship, Reply, Drop, Schema]

  , testCase "a negated predicate fails the row it matches" $ do
      matches "-state:DONE" [Ship, Privet, Reply, Plain, Drop]
      matches "-state:none" [Ship, Privet, Reply, Drop, Schema]
      matches "-priority:none" [Ship, Privet, Drop]
  ]

-- AND/OR shape

-- | Same-key predicates OR, everything else ANDs — the faceted-filter reading
-- SCHEMA.md pins, and the reason a second @state:@ widens a query rather than
-- emptying it.
shapeSpec :: TestTree
shapeSpec = testGroup "Shape"
  [ testCase "predicates sharing a single-valued key are either of them" $ do
      matches "state:TODO" [Privet]
      matches "state:DONE" [Schema]
      matches "state:TODO state:DONE" [Privet, Schema]
      matches "state:TODO state:DONE state:NEXT" [Ship, Privet, Schema]
      -- Which is the only reading that answers anything: one cell, one value.
      matches "priority:a priority:c" [Ship, Drop]

  , testCase "predicates sharing a multi-valued key are all of them" $ do
      -- The tags cell is a list, so two of them intersect where two states
      -- unite.  A union here would answer [Ship, Schema].
      matches "tag:web" [Ship, Schema]
      matches "tag:glance" [Ship]
      matches "tag:web tag:glance" [Ship]
      matches "tag:web tag:unicode" []
      web <- matching "tag:web"
      glance <- matching "tag:glance"
      both <- matching "tag:web tag:glance"
      assertBool "the intersection is no bigger than either side"
                 (length both <= min (length web) (length glance))

  , testCase "the two arities meet in one query" $ do
      matches "state:TODO state:DONE tag:web" [Schema]
      matches "state:NEXT state:DONE tag:web tag:glance" [Ship]
      matches "state:NEXT state:DONE tag:web tag:cleanup" []

  , testCase "distinct keys and free text and together" $ do
      matches "state:active scheduled:2026-08" [Ship, Privet]
      matches "state:active scheduled:2026-08 ship" [Ship]
      matches "state:TODO state:DONE schema" [Schema]

  , testCase "negations and regardless, so two of them are neither" $ do
      matches "-state:TODO -state:DONE" [Ship, Reply, Plain, Drop]
      matches "state:active -priority:none" [Ship, Privet]

  , testCase "free text is the whole row, in any cell" $ do
      matches "2026-08-05" [Ship]        -- a deadline
      matches ":web:" [Ship, Schema]     -- a tag string, matched as the text it is
      matches "cancelled" [Drop]         -- a state

  , testCase "free-text tokens and together, in any order" $ do
      -- The renderer's rule: each token is its own substring of the row, so
      -- words out of order still match where the old single substring would not.
      matches "ship view" [Ship]
      matches "view ship" [Ship]
      matches "\"the table\"" [Ship]
      matches "\"table the\"" []

  , testCase "a negated free-text token drops the rows holding it" $ do
      matches "-the" [Privet, Plain]
      matches "state:active -the" [Privet, Plain]

  , testCase "an empty query is every row" $ do
      every <- matching ""
      assertEqual "the whole fixture" 6 (length every)
      matches "   " every
  ]

-- Degenerate case

-- | With no predicate in it a query is the substring search it always was, and
-- the two paths agree row by row.
degenerateSpec :: TestTree
degenerateSpec = testGroup "Plain text"
  [ testCase "one word answers exactly what matchesSearch answers" $ do
      records <- qrRecords <$> loadDir viewDir
      let same q = [ (hrTitle r, matchesSearch q r, matchesFilter (storeEnv records) q r)
                   | r <- records ]
          wrong q = [ row | row@(_t, a, b) <- same q, a /= b ]
      mapM_ (\q -> assertEqual (T.unpack q) [] (wrong q))
            [ "", "  ", "ship", "SHIP", "e", "no-such-headline-anywhere"
            , "привет", ":web:", "=code=", "2026-08", "the schema" ]

  , testCase "an unknown key stays a substring of the whole row" $ do
      records <- qrRecords <$> loadDir viewDir
      let q = "note:later"
      assertEqual "no row carries it" []
        [ hrTitle r | r <- records, matchesFilter (storeEnv records) q r ]
      assertBool "and it is not read as a predicate"
                 (all ((== Nothing) . tmKey) (parseFilter q))
  ]

-- Haystack layout

-- | A predicate reads one field of the row's search text, so the field order
-- has to be the column order and the field contents have to be the display
-- cells the free-text search already agrees with.
layoutSpec :: TestTree
layoutSpec = testGroup "Search text layout"
  [ testCase "field i of the search text is column i as it displays" $ do
      records <- qrRecords <$> loadDir viewDir
      mapM_ (\r -> mapM_ (check r) (zip [0 ..] (cellsOf r))) records

  , testCase "a cell past the last column is empty rather than the last one" $ do
      records <- qrRecords <$> loadDir viewDir
      mapM_ (assertEqual "field 6" "" . cellAt (length filterKeys) . hrSearch) records
  ]
  where
    cellsOf r = [ unset (hrState r), unset (hrPriority r), hrTitle r
                , hrTags r, unset (hrScheduled r), unset (hrDeadline r) ]
    unset = fromMaybe ""
    check r (i, cell) =
      assertEqual (T.unpack (hrTitle r) <> " field " <> show (i :: Int))
                  (T.toLower (displayText cell)) (cellAt i (hrSearch r))
