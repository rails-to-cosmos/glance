-- | The filter query language: the tokenizer as a pure function, and the
-- semantics over a loaded fixture.
--
-- The grammar is @table-view\/SCHEMA.md@'s ("Filter query") and the renderer
-- implements it too, so what is asserted here is the contract rather than this
-- port of it: the expected tokens and the expected matches are written down,
-- not derived from the code under test.
module TestFilter (spec) where

import Control.Monad (unless)
import Data.List (nub, sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults (columnKeysOf, field, maybeTextAt, refusedNaming, viewDir, withDocDir)

import qualified Data.Text as T

import Glance.Query ( HeadlineRecord (..), QueryResult (qrRecords), defaultSortChain
                    , displayText
                    , loadDir, matchesSearch, refTargetOf, refTargets, resolveColumns
                    , rowJSON
                    , tagsOfCell, viewJSON )
import Glance.Web.Columns (columnNamesIn)
import Glance.Web.Filter ( Term (..), Token (..), alternatives, archiveKey
                         , archiveMeta, cellAt, columnsKey, emptyEnv, emptyMeta, filterKeys
                         , matchesFilter, metaOf, namesArchive, parseFilter
                         , plannedKey, refKey, scanQuery, sortKey, storeEnv
                         , tagsKey )
import Glance.Web.Sort (sortChainIn)

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
  [ tokenSpec, predicateSpec, tagsSpec, plannedSpec, sortSpec, columnsSpec
  , archiveSpec, metaSpec
  , shapeSpec, alternationSpec
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
withRefTree = withDocDir "test" "a.org" (T.unlines
  [ "* Target"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: alpha"
  , ":END:"
  -- The target links to ITSELF, which org-glance's own materialize footer
  -- writes: the rule says this must not make it its own referrer.
  , "see [[org-glance-visit:alpha][myself]]"
  , "* By id"
  , "points at [[org-glance-visit:alpha][the target]]"
  , "* By title"
  , "points at [[*Second]] instead"
  , "* Second"
  , "* Neither"
  , "no links here" ])

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

  , testCase "two refs AND like any two tokens, and either is one token" $
      -- Every token narrows, so naming two is a row pointing at both — which no
      -- row here does — and the union is the alternation.
      withRefTree $ \records -> do
        rid <- idOf "Second" records
        let hit q = [ hrTitle r | r <- records, matchesFilter (storeEnv records) q r ]
        assertEqual "both" [] (hit ("ref:alpha ref:" <> rid))
        assertEqual "either" ["By id", "By title"] (hit ("ref:alpha|" <> rid))

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
                  [Term False (Just "planned") "*empty*"] (parsed "planned:*empty*")

  , testCase "a row is planned when either date cell holds anything" $ do
      matches "-planned:*empty*" [Ship, Privet, Reply]
      -- Ship carries both, Privet a schedule, Reply a deadline.  Drop's
      -- `CLOSED:' is neither column, so it is not a plan.
      matches "planned:*empty*" [Plain, Drop, Schema]

  , testCase "and neither date column alone answers the same question" $ do
      matches "-scheduled:*empty*" [Ship, Privet]
      matches "-deadline:*empty*" [Ship, Reply]

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

  , testCase "two of them AND like any two tokens, and either is one token" $ do
      -- Privet's schedule and Reply's deadline, which no row carries both of.
      matches "planned:2026-08-03 planned:2026-08-10" []
      matches "planned:2026-08-03|2026-08-10" [Privet, Reply]
      -- Ship carries a schedule AND a deadline, so it is the row two tokens
      -- can answer — the key reads both cells, and a row may fill each.
      matches "planned:2026-08-01 planned:2026-08-05" [Ship]

  , testCase "negation composes with everything else" $ do
      -- The agenda's own query: the active rows carrying a date.
      matches "state:*active* -planned:*empty*" [Ship, Privet, Reply]
      matches "state:*inactive* -planned:*empty*" []
      matches "-planned:2026-08" [Plain, Drop, Schema]

    -- NO TREE CAN TAKE THE KEY AWAY, and the reason is structural rather than
    -- scenario-shaped: key resolution is 'fieldOf' over a CLOSED list, so a tag
    -- named `planned' is not a key and has nothing to shadow.  What is asserted
    -- is that closure — every key a query can name, enumerated — since a
    -- resolution assertion alone repeats the case above it, and a comparison of
    -- two environments would pass whatever the tree carried ('storeEnv' answers
    -- `ref:' and nothing else, so it holds no tag for a tree to reach).
  , testCase "no tree can take the key away, the keys being a closed list" $ do
      assertEqual "still the date key" [Term False (Just "planned") "*empty*"]
                  (parsed "planned:*empty*")
      records <- qrRecords <$> loadDir viewDir
      -- The fixture DOES carry tags, and not one of them is a key: every token
      -- `TAG:x' over them comes back as free text.
      let carried = vocabularyOf records
      assertBool "the fixture carries tags to offer" (length carried >= 2)
      assertEqual "and no tag the tree carries is a key" []
                  [ tag | tag <- carried
                        , Term _neg key _v <- parsed (tag <> ":x")
                        , key /= Nothing ]
      -- The whole vocabulary a query may name, so a key added or lost fails
      -- here rather than being noticed by a reader typing one.
      assertEqual "the keys are exactly the columns plus the four the grammar owns"
                  (sort (filterKeys <> [plannedKey, refKey, sortKey]))
                  (sort [ k | k <- filterKeys <> [plannedKey, refKey, sortKey]
                            , Term _n (Just k') _v <- parsed (k <> ":x"), k' == k ])
  ]

-- | The ORDER token: @sort:COL@, @sort:COL:desc@.
--
-- Two halves, and they meet in the query: it NARROWS NOTHING here, whatever it
-- names, and what it says about the order is 'Glance.Web.Sort.sortChainIn's
-- answer to the same string.  The cases are SCHEMA.md's and the parity vectors'
-- (@fixtures\/parity\/sort-tokens.json@) — what is asserted is the contract
-- rather than this port of it.  Where the chain lands the ROWS is
-- @\/headlines@' (@TestServe@).
sortSpec :: TestTree
sortSpec = testGroup "Sort tokens"
  [ testCase "the key is spelled once, and it is not a column" $ do
      assertEqual "the key" "sort" sortKey
      assertBool "and no column carries it" (sortKey `notElem` filterKeys)
      assertEqual "a token names it like any other key"
                  [Term False (Just "sort") "deadline"] (parsed "sort:deadline")

  , testCase "it narrows nothing, whatever it names" $ do
      every <- matching ""
      mapM_ (`matches` every)
        [ "sort:deadline", "sort:deadline:desc", "sort:state sort:title"
        , "sort:state->title", "sort:state->title:desc", "sort:deadline->"
        , "sort:", "sort:nosuchcolumn", "sort:deadline:sideways" ]

  , testCase "and never as free text, which is what would narrow" $ do
      -- The letters are in no row, so a token read as text would empty the
      -- table — which is exactly what a producer without the key would do.
      matches "sort:title" =<< matching ""
      assertEqual "the token resolved to the key"
                  [Term False (Just "sort") "title"] (parsed "sort:title")

  , testCase "beside a predicate it leaves the narrowing to it" $ do
      matches "state:*inactive* sort:title" [Drop, Schema]
      matches "sort:title state:*inactive*" [Drop, Schema]

  , testCase "a quoted token is free text, here as everywhere" $ do
      matches "\"sort:title\"" []
      assertEqual "free text" [Term False Nothing "sort:title"] (parsed "\"sort:title\"")

  -- The chain the same query states.  Written order is precedence, repeats
  -- compose, and a query naming none leaves the view's declared chain standing.
  , testCase "a query naming no sort key leaves the declared chain standing" $ do
      assertEqual "the default" (Right defaultSortChain)
                  (sortChainIn "state:TODO tag:web")
      assertEqual "and an empty query is a query naming none"
                  (Right defaultSortChain) (sortChainIn "")

  , testCase "a sort token replaces it, whole" $ do
      assertEqual "one key" (Right [("deadline", True)])
                  (sortChainIn "sort:deadline")
      assertEqual "the default is gone rather than behind it"
                  (Right [("deadline", True)])
                  (sortChainIn "state:TODO sort:deadline")

  , testCase "the direction is the token's second half" $ do
      assertEqual "desc" (Right [("deadline", False)])
                  (sortChainIn "sort:deadline:desc")
      assertEqual "asc spells the default" (Right [("deadline", True)])
                  (sortChainIn "sort:deadline:asc")
      assertEqual "and the word is folded" (Right [("deadline", False)])
                  (sortChainIn "sort:deadline:DESC")
      -- A trailing colon is the direction half-typed, and an unspelled
      -- direction ascends — the renderer's own reading of the same token.
      assertEqual "a trailing colon is no direction at all"
                  (Right [("deadline", True)])
                  (sortChainIn "sort:deadline:")

  , testCase "written order is precedence, and repeats compose" $ do
      assertEqual "two keys" (Right [("state", True), ("deadline", False)])
                  (sortChainIn "sort:state sort:deadline:desc")
      assertEqual "the other way round"
                  (Right [("deadline", False), ("state", True)])
                  (sortChainIn "sort:deadline:desc sort:state")
      -- The tokens need not be adjacent: a chain is the sort tokens in the
      -- order the query spells them, whatever stands between them.
      assertEqual "with predicates between them"
                  (Right [("state", True), ("title", True)])
                  (sortChainIn "sort:state tag:web sort:title")

  -- @->@ chains a token's columns, and it is the tokens said once.  SUGAR, so
  -- every case here is asserted against the spelling it is sugar FOR: what is
  -- pinned is that the two are one query rather than that either is right.
  , testCase "an arrow chains one token's columns" $
      mapM_ (\(chained, spelled') ->
               assertEqual (T.unpack chained <> " vs " <> T.unpack spelled')
                           (sortChainIn spelled') (sortChainIn chained))
        [ ("sort:state->title",            "sort:state sort:title")
        , ("sort:state->title:desc",       "sort:state sort:title:desc")
        , ("sort:state:desc->title",       "sort:state:desc sort:title")
        , ("sort:state->title->deadline",  "sort:state sort:title sort:deadline")
          -- The two spellings mix, and precedence reads straight through.
        , ("sort:state sort:title->deadline",
           "sort:state sort:title sort:deadline")
        , ("sort:state->title tag:web sort:deadline",
           "sort:state sort:title tag:web sort:deadline")
          -- A segment half typed is the `key:' rule, like the token it is one of.
        , ("sort:state->",                 "sort:state sort:")
        , ("sort:->state",                 "sort: sort:state")
        , ("sort:->",                      "sort: sort:") ]

  , testCase "and the chain it names is the chain, arrow or no arrow" $ do
      assertEqual "three keys"
                  (Right [("state", True), ("title", False), ("deadline", True)])
                  (sortChainIn "sort:state->title:desc->deadline")
      assertEqual "a half-typed segment orders nothing"
                  (Right [("state", True)]) (sortChainIn "sort:state->")

  , testCase "a half-typed key orders nothing and refuses nothing" $ do
      assertEqual "the key: rule" (Right []) (sortChainIn "sort:")
      assertEqual "and so does a half-typed segment" (Right []) (sortChainIn "sort:->")

  -- `*none*' is the query's whole vocabulary for document order, and it
  -- replaced `?order=document'.  It is a STARRED META like `*active*' and
  -- `*archive*': no column is called it and no cell can hold it.
  , testCase "sort:*none* is the empty chain" $ do
      assertEqual "alone" (Right []) (sortChainIn "sort:*none*")
      assertEqual "beside predicates, which narrow as they always do"
                  (Right []) (sortChainIn "state:TODO sort:*none* tag:web")
      -- The half-typed token names nothing either way, so it is no companion.
      assertEqual "and beside the half-typed token"
                  (Right []) (sortChainIn "sort: sort:*none*")

  , testCase "and it admits no companion that orders anything" $
      mapM_ (\q -> refusedNaming (T.unpack q) ["*none*"] (sortChainIn q))
        [ "sort:*none* sort:title"
        , "sort:title sort:*none*"
        , "sort:*none* sort:*none*"
          -- The empty chain has no key in it to reverse.
        , "sort:*none*:desc"
          -- A SEGMENT is a companion like any other, so the same pair written
          -- once is the same refusal.
        , "sort:*none*->title"
        , "sort:title->*none*"
        , "sort:*none*->*none*"
        , "sort:*none*:desc->title" ]

  , testCase "one column, one direction: everything else is refused by name" $
      mapM_ (\(q, named) -> refusedNaming (T.unpack q) [named] (sortChainIn q))
        [ ("-sort:title",             "-sort:title")
        , ("sort:title|state",        "sort:title|state")
        , ("sort:nosuchcolumn",       "nosuchcolumn")
        , ("sort:title:sideways",     "sort:title:sideways")
          -- Each refusal again as ONE token, the whole of it named back.  The
          -- negation covers every segment, being written before the key.
        , ("-sort:title->state",      "-sort:title->state")
        , ("sort:title|state->deadline", "sort:title|state->deadline")
        , ("sort:state->nosuchcolumn", "nosuchcolumn")
        , ("sort:nosuchcolumn->state", "nosuchcolumn")
        , ("sort:state->title:sideways", "sort:state->title:sideways") ]

    -- A REPEATED column is no refusal on either side: the first spelling wins
    -- and the later key drops — the renderer's own rule, and SCHEMA.md's.  A
    -- duplicate names an order the chain already has, so nothing a reader
    -- could have meant is lost.  First-wins spans the segments and the token
    -- boundaries alike.
  , testCase "a repeated column folds to its first spelling" $
      mapM_ (\(q, want) ->
               case sortChainIn q of
                 Left why    -> assertFailure (T.unpack q <> " refused: " <> T.unpack why)
                 Right chain -> assertEqual (T.unpack q) want chain)
        [ ("sort:title->title",                 [("title", True)])
        , ("sort:title:desc->title",            [("title", False)])
        , ("sort:title sort:title",             [("title", True)])
        , ("sort:title sort:title:desc",        [("title", True)])
        , ("sort:title->state sort:title:desc", [("title", True), ("state", True)])
        , ("sort:title sort:state->title",      [("title", True), ("state", True)]) ]

  , testCase "a refusal is the whole query's, wherever the token sits" $ do
      assertBool "the good key does not rescue the bad one"
                 (either (const True) (const False)
                         (sortChainIn "sort:title -sort:state"))
      assertBool "nor the good segment the bad one beside it"
                 (either (const True) (const False)
                         (sortChainIn "sort:title->nosuchcolumn"))
  ]

-- | Which queries turn the served view's archive exclusion off
-- ('Glance.Web.Filter.namesArchive').  The exclusion itself is
-- @\/headlines@'s (@TestServe@); what belongs here is the reading of the
-- query, since it is the grammar answering.
-- Columns tokens
--
-- The COLUMN SET half of the view grammar ('Glance.Web.Columns'), the sort
-- token's twin: what it never does is narrow, and what it names is
-- 'columnNamesIn''s answer to the same string.  Where the picked set lands on
-- the wire is @\/headlines@' (@TestServe@); 'resolveColumns' is the name
-- resolution both share.
columnsSpec :: TestTree
columnsSpec = testGroup "Columns tokens"
  [ testCase "the key is spelled once, and it is not a column" $ do
      assertEqual "the key" "columns" columnsKey
      assertBool "and no column carries it" (columnsKey `notElem` filterKeys)
      assertEqual "a token names it like any other key"
                  [Term False (Just "columns") "State,Title"]
                  (parsed "columns:State,Title")

  , testCase "it narrows nothing, whatever it names" $ do
      every <- matching ""
      mapM_ (`matches` every)
        [ "columns:state", "columns:State,Title,Tags", "columns:"
        , "columns:nosuchcolumn", "columns:a,,b", "-columns:state" ]

  , testCase "beside a predicate it leaves the narrowing to it" $ do
      matches "state:*inactive* columns:title" [Drop, Schema]
      matches "columns:title state:*inactive*" [Drop, Schema]

  , testCase "a quoted token is free text, here as everywhere" $ do
      matches "\"columns:title\"" []
      assertEqual "free text"
                  [Term False Nothing "columns:title"] (parsed "\"columns:title\"")

  -- The names the same query states.
  , testCase "a query naming no columns token names no set" $ do
      assertEqual "none" (Right Nothing) (columnNamesIn "state:TODO tag:web")
      assertEqual "and an empty query is a query naming none"
                  (Right Nothing) (columnNamesIn "")

  , testCase "names arrive in written order, repeats composing" $ do
      assertEqual "one token" (Right (Just ["State", "Title", "Tags"]))
                  (columnNamesIn "columns:State,Title,Tags")
      assertEqual "two tokens compose" (Right (Just ["state", "title"]))
                  (columnNamesIn "columns:state columns:title")

  , testCase "a name named twice keeps its first place, case-insensitively" $
      assertEqual "state once, first spelling"
                  (Right (Just ["state", "Title"]))
                  (columnNamesIn "columns:state,STATE,Title")

  , testCase "an empty list falls back to the default" $ do
      assertEqual "columns: names no set" (Right Nothing) (columnNamesIn "columns:")
      assertEqual "and neither do empty names" (Right Nothing)
                  (columnNamesIn "columns:,")
      assertEqual "empty names between real ones drop"
                  (Right (Just ["a", "b"])) (columnNamesIn "columns:a,,b")

  , testCase "a negation and an alternation are refused, naming the token" $ do
      refusedNaming "negated" ["negated", "-columns:state"]
                    (columnNamesIn "-columns:state")
      refusedNaming "alternation" ["commas", "columns:a|b"]
                    (columnNamesIn "columns:a|b")

  -- Resolution: what a name becomes.  The cell functions are opaque, so the
  -- cases read the three describable fields.
  , testCase "names resolve case-insensitively, headers included" $ do
      assertEqual "keys and headers both name a column"
                  [("state", "State"), ("title", "Title"), ("tag", "Tags")]
                  (described (resolveColumns ["state", "Title", "Tags"]))
      assertEqual "org's own glyph finds the priority column"
                  [("title", "Title"), ("priority", "#")]
                  (described (resolveColumns ["#"]))

  , testCase "an unknown name is a custom column, header as written" $
      assertEqual "folded key, verbatim header"
                  [("title", "Title"), ("org_glance_id", "ORG_GLANCE_ID")]
                  (described (resolveColumns ["ORG_GLANCE_ID"]))

  , testCase "the minimal set is Title: unnamed, it joins in front" $ do
      assertEqual "injected first" [("title", "Title"), ("state", "State")]
                  (described (resolveColumns ["state"]))
      assertEqual "named, it stays where it was put"
                  [("tag", "Tags"), ("title", "Title"), ("state", "State")]
                  (described (resolveColumns ["tags", "title", "state"]))
  ]

-- | The describable half of a resolved column: its key and its header.
described :: [(Text, Text, Text, HeadlineRecord -> Maybe Text)] -> [(Text, Text)]
described cols = [ (key, header) | (key, header, _kind, _cell) <- cols ]

archiveSpec :: TestTree
archiveSpec = testGroup "Archive key"
  [ testCase "the tag, the meta that names it, and the column both sit under" $ do
      assertEqual "the value" "archive" archiveKey
      assertEqual "the meta" "*archive*" archiveMeta
      assertEqual "and the key it is named under" "tag" tagsKey

  , testCase "every spelling of the META counts as naming it" $
      mapM_ (\q -> assertBool (show q <> " did not read as naming the tag")
                             (namesArchive q))
            [ "tag:*archive*", "-tag:*archive*", "state:DONE tag:*archive*"
            , "tag=*archive*", "tag:\"*archive*\"", "tag:*ARCHIVE*"
            -- An ALTERNATIVE names it too: the reader has asked for archived
            -- rows, and the exclusion would answer a different question.
            , "tag:*archive*|web", "tag:web|*archive*", "-tag:web|*archive*" ]

  , testCase "and a query that says nothing about it does not" $
      mapM_ (\q -> assertBool (show q <> " read as naming the tag")
                             (not (namesArchive q)))
            -- Free text is not a predicate, quoted text never is, another
            -- column is another cell, and the bare tag key is gone: with tags
            -- out of the grammar, `archive:draft' is text like any other.
            [ "", "*archive*", "\"tag:*archive*\"", "archive:", "archive:draft"
            , "state:DONE", "title:*archive*"
            -- THE STARRED SPELLING ALONE.  `tag:archive' is the ordinary
            -- substring predicate every other tag gets, so a tree using the
            -- word for something of its own filters on it and the rows the
            -- default view hides stay hidden.
            , "tag:archive", "-tag:archive", "tag:arch", "tag:archived" ]
  ]

-- The starred family
--
-- `*word*' marks a value with semantics of its own, and a bare word marks
-- nothing — which is the property this fixture is built to catch: it spells
-- both reserved words as ordinary org, a state called @NONE@ and tags called
-- @none@, @archive@ and @archived@, and every one of them has to be reachable
-- as itself.

-- | Run K over a tree that uses the reserved words as its own vocabulary.
withMetaTree :: ([HeadlineRecord] -> IO a) -> IO a
withMetaTree = withDocDir "test" "a.org" (T.unlines
  [ "#+TODO: NONE | ARCHIVE"
  , "* NONE Filed away :web:archive:"
  , "* NONE Not filed :archived:"
  , "* ARCHIVE A state spelled like the tag :none:"
  , "* Nothing stated" ])

-- | The rows of that tree Q matches, by title, in walk order.
metaMatching :: Text -> IO [Text]
metaMatching q = withMetaTree $ \records ->
  pure [ hrTitle r | r <- records, matchesFilter (storeEnv records) q r ]

metaSpec :: TestTree
metaSpec = testGroup "Starred metas"
  [ testCase "the empty meta is the empty cell, on every column key" $ do
      assertEqual "the spelling" "*empty*" emptyMeta
      assertEqual "state" ["Nothing stated"] =<< metaMatching "state:*empty*"
      assertEqual "tag" ["Nothing stated"] =<< metaMatching "tag:*empty*"
      assertEqual "priority" 4 . length =<< metaMatching "priority:*empty*"
      assertEqual "scheduled" 4 . length =<< metaMatching "scheduled:*empty*"
      assertEqual "deadline" 4 . length =<< metaMatching "deadline:*empty*"
      assertEqual "title" [] =<< metaMatching "title:*empty*"
      assertEqual "planned" 4 . length =<< metaMatching "planned:*empty*"

  , testCase "and the bare word it replaced is a value like any other" $ do
      -- The shadow this cost: `none' was every key's word for the empty cell,
      -- so a cell reading `none' could not be asked for.  Both spellings are
      -- reachable now, and they answer different rows.
      assertEqual "a state spelled NONE"
                  ["Filed away", "Not filed"] =<< metaMatching "state:none"
      assertEqual "a tag spelled none"
                  ["A state spelled like the tag"] =<< metaMatching "tag:none"

  , testCase "a starred word on the tags column is the whole tag" $ do
      assertEqual "the tag itself" ["Filed away"] =<< metaMatching "tag:*archive*"
      -- Where the bare word is the substring the column matches by, which is
      -- the reading a tag key used to have and the one `tag:' has now.
      assertEqual "the substring beside it"
                  ["Filed away", "Not filed"] =<< metaMatching "tag:archive"
      assertEqual "negated, the near miss survives"
                  ["Not filed", "A state spelled like the tag", "Nothing stated"]
        =<< metaMatching "-tag:*archive*"

  , testCase "and two of them AND, the way two tokens do" $ do
      assertEqual "both tags" ["Filed away"] =<< metaMatching "tag:*web* tag:*archive*"
      assertEqual "one it lacks" [] =<< metaMatching "tag:*none* tag:*archive*"
      -- Either of them is the one token, metas alternating like any values.
      assertEqual "either tag" ["Filed away", "A state spelled like the tag"]
        =<< metaMatching "tag:*none*|*archive*"

  , testCase "what a meta IS: one matched pair with a word inside it" $
      -- The rule every branch above reads, spelled once ('metaOf'): the stars
      -- have to match and there has to be something between them, which is
      -- @table-view.js@'s @META@ regex and its @starless@ in one answer.
      mapM_ (\(value, want) -> assertEqual (T.unpack value) want (metaOf value))
            [ ("*empty*", Just "empty"), ("*archive*", Just "archive")
            , ("empty", Nothing), ("*empty", Nothing), ("empty*", Nothing)
            , ("**", Nothing), ("*", Nothing), ("", Nothing) ]

  , testCase "a starred word anywhere else is the literal it spells" $ do
      -- The tags column is the only multi-valued one, so nothing else reads a
      -- starred value as an entry; and the state column's own metas are the
      -- two groups and nothing more.
      assertEqual "a keyword" [] =<< metaMatching "state:*NONE*"
      assertEqual "a title word" [] =<< metaMatching "title:*filed*"
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
      matches "tag:*empty*" [Reply, Plain]

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
      -- the cell and matches.  The wider reading is the tag column's own, and it
      -- is the reading the renderer has for that column too.
      matches "glan:" []
      matches "tag:glan" [Ship]

    -- The CELL sorts and the FILE does not, so the one row carrying two tags
    -- spells them `:web:glance:' and reads `:glance:web:'.  Nothing a query can
    -- ask depends on which: a predicate is a substring of ONE tag or membership
    -- of the whole list, and the row is reached under either name and under both
    -- at once.
  , testCase "a predicate is order-independent, whichever way the row spells it" $ do
      matches "tag:web" [Ship, Schema]
      matches "tag:glance" [Ship]
      matches "tag:web tag:glance" [Ship]
      matches "tag:glance tag:web" [Ship]
      -- The whole-tag meta reads the cell as a LIST, which has no order in it.
      matches "tag:*empty*" [Reply, Plain]
      -- And the free-text half sees the cell as the table draws it: sorted, so
      -- the two tags read across the join in that order and no other.
      matches "glance:web" [Ship]
      matches "web:glance" []

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
      assertEqual "term" [Term False (Just "state") "*active*"] (parsed "state=*active*")

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
      -- The whole-value arm folds org's priority decoration on both sides,
      -- because the renderer's badge matching does (per column TYPE, never
      -- per key).  A query nobody writes, closed for parity's sake.
      matches "state:[#TODO]" [Privet]

  , testCase "the two group metas are the file's keyword groups" $ do
      -- #+TODO: NEXT WAITING | CANCELLED, over the seeded TODO/DONE.  The
      -- stateless row rides with the active ones; see below.
      matches "state:*active*" [Ship, Privet, Reply, Plain]
      matches "state:*inactive*" [Drop, Schema]
      -- The stars are the whole of what makes a meta, so the bare words are
      -- keyword text: no file here declares one, and nothing matches.
      matches "state:active" []
      matches "state:inactive" []

  , testCase "the stateless row is active, and it is not inactive" $ do
      -- No scope classifies a headline that carries no keyword, so it is in
      -- neither group — and `*active*' takes it anyway, since an entry nobody
      -- has stated is live work and the default view is what would otherwise
      -- hide it.  `*inactive*' does not: an entry nobody marked done is not
      -- done, so the two groups do not partition the column.
      matches "state:*empty*" [Plain]
      matches "state:*active*" [Ship, Privet, Reply, Plain]
      matches "state:*inactive*" [Drop, Schema]
      -- Which makes `*empty*' a subset of `*active*' rather than a third group,
      -- and makes the negation drop the empty cell along with the keywords.
      matches "-state:*active*" [Drop, Schema]
      matches "-state:*inactive*" [Ship, Privet, Reply, Plain]

  , testCase "a meta is folded like every value, and is no glob" $ do
      matches "state:*ACTIVE*" [Ship, Privet, Reply, Plain]
      -- The metas are a fixed vocabulary rather than a pattern: a starred
      -- keyword is the literal badge text `*todo*', which no cell holds.
      matches "state:*TODO*" []
      matches "state:*none*" []
      -- One matched pair, so a half-starred value is literal too.
      matches "state:*active" []
      matches "state:active*" []

    -- The CELL wears org's own `[#A]' and the match reads THROUGH the brackets,
    -- on both sides: display wears the decoration and matching reads through it.
    -- So the letter a reader types and the cell they are looking at are one
    -- query, and neither spelling is the privileged one.
  , testCase "priority is the letter, case-insensitively and through the brackets" $ do
      matches "priority:A" [Ship]
      matches "priority:a" [Ship]
      matches "priority:c" [Drop]
      matches "priority:[#A]" [Ship]
      matches "priority:[#a]" [Ship]
      -- Still EXACT past the fold: a letter is one character, so nothing here
      -- is a substring test and a half-spelled bracket matches nothing.
      matches "priority:[#" []
      matches "priority:AB" []
      matches "priority:*empty*" [Reply, Plain, Schema]

  , testCase "title and tag are substrings of the cell, case-insensitively" $ do
      matches "title:the" [Ship, Reply, Drop, Schema]
      matches "title:SHIP" [Ship]
      matches "title:привет" [Privet]
      matches "tag:web" [Ship, Schema]
      matches "tag:*empty*" [Reply, Plain]
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
      matches "scheduled:*empty*" [Reply, Plain, Drop, Schema]
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
      matches "-state:*empty*" [Ship, Privet, Reply, Drop, Schema]
      matches "-priority:*empty*" [Ship, Privet, Drop]
  ]

-- AND/OR shape

-- | One combination rule: TOKENS AND, ALTERNATIVES OR ('alternationSpec' is the
-- OR half).  Every token narrows, whether or not another token names its key,
-- so a second @state:@ empties a query where it once widened one.
shapeSpec :: TestTree
shapeSpec = testGroup "Shape"
  [ testCase "every token narrows, whether or not another names its key" $ do
      matches "state:TODO" [Privet]
      matches "state:DONE" [Schema]
      -- A cell holding ONE value cannot hold two, so this is no row at all.
      -- The either it used to mean is one token now: `state:TODO|DONE'.
      matches "state:TODO state:DONE" []
      matches "state:TODO state:DONE state:NEXT" []
      matches "priority:a priority:c" []
      -- The same value twice is that value, which is what makes this a rule
      -- about tokens rather than a rule about two DIFFERENT values.
      matches "state:TODO state:TODO" [Privet]

  , testCase "and a cell holding a list can meet all of them" $ do
      -- The tags cell is a list, so two tokens intersect where two states
      -- cannot.  The answer is the one it always had, under a rule the state
      -- column now obeys too.
      matches "tag:web" [Ship, Schema]
      matches "tag:glance" [Ship]
      matches "tag:web tag:glance" [Ship]
      matches "tag:web tag:unicode" []
      web <- matching "tag:web"
      glance <- matching "tag:glance"
      both <- matching "tag:web tag:glance"
      assertBool "the intersection is no bigger than either side"
                 (length both <= min (length web) (length glance))

  , testCase "an alternation and a token meet in one query" $ do
      matches "state:TODO|DONE tag:web" [Schema]
      matches "state:NEXT|DONE tag:web tag:glance" [Ship]
      matches "state:NEXT|DONE tag:web tag:cleanup" []

  , testCase "distinct keys and free text and together" $ do
      matches "state:*active* scheduled:2026-08" [Ship, Privet]
      matches "state:*active* scheduled:2026-08 ship" [Ship]
      matches "state:TODO|DONE schema" [Schema]

  , testCase "negations and regardless, so two of them are neither" $ do
      matches "-state:TODO -state:DONE" [Ship, Reply, Plain, Drop]
      matches "state:*active* -priority:*empty*" [Ship, Privet]

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
      matches "state:*active* -the" [Privet, Plain]

  , testCase "an empty query is every row" $ do
      every <- matching ""
      assertEqual "the whole fixture" 6 (length every)
      matches "   " every
  ]

-- Alternation

-- | @key:A|B@ — the OR half of the one combination rule.  A predicate's VALUE
-- splits on @|@ and each alternative is read as that key's own value, so the
-- bar means the same thing on every key and every kind of value.
alternationSpec :: TestTree
alternationSpec = testGroup "Alternation"
  [ testCase "what a value splits into, and what an empty alternative costs" $
      mapM_ (\(value, want) -> assertEqual (T.unpack value) want (alternatives value))
            -- An empty alternative is dropped, so every half-typed form is the
            -- value without it.
            [ ("a|b", ["a", "b"]), ("a", ["a"])
            , ("a|", ["a"]), ("|a", ["a"]), ("a||b", ["a", "b"])
            -- And a value that is bars alone is left with none, which is the
            -- `key:' rule: a predicate with nothing to narrow by.
            , ("", []), ("|", []), ("||", []) ]

  , testCase "each alternative is read as that key's own value" $ do
      -- A badge is whole-value, a priority is exact, a title and a tag are
      -- substrings of their cell.
      matches "state:TODO|DONE" [Privet, Schema]
      matches "priority:A|C" [Ship, Drop]
      matches "title:ship|schema" [Ship, Schema]
      matches "tag:glance|unicode" [Ship, Privet]
      -- A date is a prefix, on its column and over both columns at once.
      matches "scheduled:2026-08-01|2026-08-03" [Ship, Privet]
      matches "planned:2026-08-03|2026-08-10" [Privet, Reply]
      -- The semantics stay the key's: a badge is no substring, not even as one
      -- alternative of two.
      matches "state:TOD|DON" []

  , testCase "a meta joins the alternatives like any other value" $ do
      matches "state:*empty*|DONE" [Plain, Schema]
      matches "state:*active*|DONE" [Ship, Privet, Reply, Plain, Schema]
      matches "tag:*empty*|glance" [Ship, Reply, Plain]

  , testCase "a negation covers the whole token" $ do
      matches "-state:TODO|DONE" [Ship, Reply, Plain, Drop]
      -- Neither alternative — which De Morgan makes the two negations too, and
      -- that agreement is the reading worth pinning.
      matches "-tag:web|glance" [Privet, Reply, Plain, Drop]
      matches "-tag:web -tag:glance" [Privet, Reply, Plain, Drop]

  , testCase "an empty alternative narrows nothing and costs nothing" $ do
      matches "state:TODO|" [Privet]
      matches "state:|TODO" [Privet]
      matches "state:TODO||DONE" [Privet, Schema]
      every <- matching ""
      matches "state:|" every
      matches "state:||" every
      matches "tag:||" every

  , testCase "the bar is a predicate's, so free text is the text it spells" $ do
      -- No row's display text holds a bar, so both free-text spellings are
      -- empty where the predicate beside them answers two rows.
      matches "title:ship|schema" [Ship, Schema]
      matches "ship|schema" []
      matches "\"ship|schema\"" []
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
      let hit q = [ hrTitle r | r <- records, matchesFilter (storeEnv records) q r ]
          q = "note:later"
      assertEqual "no row carries it" [] (hit q)
      assertBool "and it is not read as a predicate"
                 (all ((== Nothing) . tmKey) (parseFilter q))
      -- THE POSITIVE HALF, which an empty answer cannot give: a token whose key
      -- nothing declares over text a row DOES spell.  A schedule cell carries a
      -- time, so `09:30' is key `09' — no key at all — over text the Ship row
      -- holds, and it matches that row as the free text it is.  It answers row
      -- for row with the same string QUOTED, which is free text by the grammar.
      assertBool ("an unknown key over text a row carries finds it: "
                    <> show (hit "09:30"))
                 (not (null (hit "09:30")))
      assertEqual "the whole token is the needle, colon included"
                  (hit "\"09:30\"") (hit "09:30")
      -- And the KEY is part of the needle rather than dropped: a reading that
      -- searched the value alone would answer this with most of the fixture.
      assertEqual "the value alone is not what was searched for" [] (hit "nosuchkey:e")
      assertBool "though the value alone matches plenty" (length (hit "e") >= 3)
  ]

-- Haystack layout

-- | A predicate reads one field of the row's search text, so the field order
-- has to be the column order and the field contents have to be the display
-- cells the free-text search already agrees with.
layoutSpec :: TestTree
layoutSpec = testGroup "Search text layout"
  [ -- The hand-written oracle: six cells named here, in the order this suite
    -- says they go in.  It is a real oracle now that the search text is built
    -- out of 'Glance.Query.viewColumns' — the list it is compared against is
    -- the only copy of the layout that is not derived from that table.
    testCase "field i of the search text is column i as it displays" $ do
      records <- qrRecords <$> loadDir viewDir
      mapM_ (\r -> mapM_ (check r) (zip [0 ..] (cellsOf r))) records

  , testCase "a cell past the last column is empty rather than the last one" $ do
      records <- qrRecords <$> loadDir viewDir
      mapM_ (assertEqual "field 6" "" . cellAt (length filterKeys) . hrSearch) records

    -- THE APPEND.  The oracle above names its six cells by hand; this one is
    -- quantified over 'filterKeys', cell text and all, so a SEVENTH column
    -- appended to 'viewColumns' is covered the day it lands: whatever a row
    -- shows under a key, the predicate spelling that key finds the row it was
    -- read off, and finds no row whose own cell for that key is empty.  What
    -- made the append the hole is that the search text used to be a positional
    -- list written out beside the record, which a seventh column left six
    -- fields long — every predicate past it then reading the wrong field.
    --
    -- The values come off the wire rather than out of the haystack, which is
    -- what makes this an end-to-end claim rather than a restatement of how the
    -- haystack was cut.  They are quoted, since a cell holds spaces; the
    -- fixture's cells carry no quote and no bar, the scanner's other two
    -- characters, and the scanner is 'tokenSpec''s subject anyway.
  , testCase "every column is reachable by the key it declares" $ do
      records <- qrRecords <$> loadDir viewDir
      sequence_ [ reachable records r key | r <- records, key <- filterKeys ]
  ]
  where
    -- The tags entry is the CELL rather than the field: `hrTags' is the file's
    -- order and the column sorts it, which is the one place the two differ and
    -- the one place this oracle has to say so out loud.  Sorted HERE rather than
    -- through `sortedTagsCell', so the list stays an independent oracle: calling
    -- the producer's own function would make this a mirror of the code it is
    -- meant to check.
    cellsOf r = [ unset (hrState r), unset (hrPriority r), hrTitle r
                , unset (hrScheduled r), unset (hrDeadline r)
                , sortedTags (hrTags r) ]
    sortedTags cell = case sortOn T.toCaseFold (filter (not . T.null) (T.splitOn ":" cell)) of
      []   -> cell
      tags -> ":" <> T.intercalate ":" tags <> ":"
    unset = fromMaybe ""
    check r (i, cell) =
      assertEqual (T.unpack (hrTitle r) <> " field " <> show (i :: Int))
                  (T.toLower (displayText cell)) (cellAt i (hrSearch r))
    -- The cell R shows under KEY, as the row's own JSON carries it.
    cellUnder key r = fmap (fromMaybe "") . maybeTextAt key =<< field "cells" (rowJSON r)
    reachable records r key = do
      cell <- cellUnder key r
      let value = T.toLower (displayText cell)
          q     = key <> ":\"" <> value <> "\""
          asked = T.unpack (hrTitle r <> " — " <> q)
      unless (T.null value) $ do
        assertBool (asked <> ": the row its own cell came from does not match")
                   (matchesFilter emptyEnv q r)
        answered <- traverse (cellUnder key) (filter (matchesFilter emptyEnv q) records)
        assertBool (asked <> ": a row with an empty cell there answered it")
                   (not (any T.null answered))
