-- | The filter query language: the tokenizer as a pure function, and the
-- semantics over a loaded fixture.  The grammar is @table-view\/SCHEMA.md@'s.
module TestFilter (spec) where

import Control.Monad (unless)
import Data.List (nub, sort, sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults ( columnKeysOf, field, maybeTextAt, orgFile, refusedNaming, viewDir
                    , withDocDir, withTempDir )

import qualified Data.Text as T

import Glance.Query ( HeadlineRecord (..), QueryResult (qrRecords), defaultSortChain
                    , activeMeta, dayWords, displayText, inactiveMeta, metaWord, metas
                    , Ref (..), RefVia (..), loadDir, matchesSearch, refTargetOf, refTargets
                    , resolveColumns
                    , rowJSON
                    , tagsOfCell, viewJSON )
import Glance.Web.Columns (columnNamesIn)
import Glance.Web.Filter ( FilterEnv, Sign (..), Term (..), Token (..), alternatives, anyMeta
                         , archiveKey
                         , archiveMeta, cellAt, columnsKey, emptyEnv, emptyMeta, filterKeys
                         , fromKey
                         , matchesFilter, metaOf, namesArchive, onDay, parseFilter
                         , plannedKey, refKey, scanQuery, sortKey, storeEnv
                         , substringKey
                         , tagsKey, todayMeta, viewAddedIn )
import Glance.Web.Sort (noOrder, sortChainIn)

-- 'viewDir': six headlines, five states between them, one of them stateless.

-- | The fixture's rows, in walk order; four of six have only an offset for an id.
data Row = Ship | Privet | Reply | Plain | Drop | Schema
  deriving (Bounded, Enum, Eq, Ord, Show)

titleOf :: Row -> Text
titleOf Ship   = "Ship the table view"
titleOf Privet = "Привет мир"
titleOf Reply  = "Reply from the печатник"
titleOf Plain  = "Plain headline without a state"
titleOf Drop   = "Drop the old renderer"
titleOf Schema = "Read the schema"

-- | The tags the fixture's rows carry, derived as the store derives its own.
vocabularyOf :: [HeadlineRecord] -> [Text]
vocabularyOf = sort . nub . concatMap (tagsOfCell . hrTags)

-- | The rows of RECORDS that Q matches, by title, in walk order.
titlesMatching :: Text -> [HeadlineRecord] -> [Text]
titlesMatching = titlesMatchingIn id

-- | The rows of RECORDS that Q matches with the store's env shaped by K, by
-- title, in walk order.  ONE MATCHER under every reader here.
titlesMatchingIn :: (FilterEnv -> FilterEnv) -> Text -> [HeadlineRecord] -> [Text]
titlesMatchingIn k q records =
  [ hrTitle r | r <- records, matchesFilter (k (storeEnv records)) q r ]

-- | The rows Q matches, in walk order.
matching :: Text -> IO [Row]
matching = matchingIn id

-- | The rows Q matches with the store's env shaped by K, in walk order.
matchingIn :: (FilterEnv -> FilterEnv) -> Text -> IO [Row]
matchingIn k q = do
  records <- qrRecords <$> loadDir viewDir
  mapM (named records) (titlesMatchingIn k q records)
  where
    named records t = case [ row | row <- [minBound ..], titleOf row == t ] of
      [row]  -> pure row
      _other -> assertFailure ("the fixture moved: no row named " <> show t
                                 <> " among " <> show (map hrTitle records))

-- | Q matches exactly ROWS, and nothing else.
matches :: Text -> [Row] -> Assertion
matches q rows = assertEqual (T.unpack q) rows =<< matching q

-- | THE DAY @*today*@ RESOLVES TO IS INJECTED, never read off the wall clock:
-- every case below names its own day, so the suite answers the same in a year.
matchesOn :: Day -> Text -> [Row] -> Assertion
matchesOn day q rows =
  assertEqual (show day <> " " <> T.unpack q) rows =<< matchingIn (onDay day) q

spec :: TestTree
spec = testGroup "Filter"
  [ tokenSpec, predicateSpec, tagsSpec, plannedSpec, substringSpec, sortSpec
  , columnsSpec
  , comparisonSpec, rangeSpec, todaySpec, shiftSpec
  , archiveSpec, metaSpec, foldSpec
  , shapeSpec, alternationSpec, addedSpec
  , degenerateSpec
  , targetSpec, refSpec, fromSpec
  , layoutSpec ]


-- | A reference with no kind on it — what a plain mention resolves to.
mention :: Text -> Ref
mention t = Ref t Nothing ViaRow

-- | An @id:@ reference with no kind: org-id's namespace.
orgMention :: Text -> Ref
orgMention t = Ref t Nothing ViaOrgId

-- | One link target normalized, or refused.
targetSpec :: TestTree
targetSpec = testGroup "Reference targets"
  [ testCase "the id-bearing protocols are stripped, case preserved" $
      mapM_ (\(raw, want) -> assertEqual (T.unpack raw) (Just want) (refTargetOf raw))
        [ ("org-glance-visit:task-spbm-1-2-3-0",    mention "task-spbm-1-2-3-0")
        , ("org-glance-open:Pets-20210816-eee5a4",  mention "Pets-20210816-eee5a4")
        , ("org-glance-material:contact-25053-3",   mention "contact-25053-3")
        -- Org's own protocol, org-id's own NAMESPACE: it names `:ID:'.
        , ("id:9f8e7d6c",                           orgMention "9f8e7d6c")
        -- The case is the id's: a fold here would put `Password-…' out of reach.
        , ("org-glance-visit:Password-20210516-d9", mention "Password-20210516-d9") ]

    -- THE PEER SPELLS A KIND ON THE EDGE — org-glance's `--edge->link-path'
    -- appends `?kind=SLUG' — and the id alone names the row.  The kind is the
    -- EDGE's own and is kept BESIDE the id rather than dropped with the `?'.
  , testCase "a kind rides off the id and is kept beside it" $ do
      mapM_ (\(raw, want) -> assertEqual (T.unpack raw) (Just want) (refTargetOf raw))
        [ ("org-glance-material:contact-25053-3?kind=author",
           Ref "contact-25053-3" (Just "author") ViaRow)
        , ("org-glance-visit:task-spbm-1-2-3-0?kind=blocked-by",
           Ref "task-spbm-1-2-3-0" (Just "blocked-by") ViaRow)
          -- AN EMPTY KIND IS NO KIND: `?kind=' names nothing to declare.
        , ("id:9f8e7d6c?kind=", orgMention "9f8e7d6c")
          -- Only the peer's own key is a kind; anything else it writes is not.
        , ("id:9f8e7d6c?other=x", orgMention "9f8e7d6c")
        , ("id:9f8e7d6c?other=x&kind=cites", Ref "9f8e7d6c" (Just "cites") ViaOrgId) ]
      assertEqual "a kind with no id before it names nothing"
                  Nothing (refTargetOf "org-glance-material:?kind=author")
      -- A TITLE IS TEXT, so its question mark is its own — which is the whole
      -- reason the strip above is guarded to the PROTOCOL branch.  The fixture
      -- carries the `?': without one this case cannot fail.
      assertEqual "a starred title keeps its question mark"
                  (Just (mention "Why not?")) (refTargetOf "*Why not?")
      assertEqual "and a bare one does too"
                  (Just (mention "Why not?")) (refTargetOf "Why not?")

  , testCase "the two title forms lose their star and keep their text" $ do
      assertEqual "starred" (Just (mention "Hacking the renderer"))
                  (refTargetOf "*Hacking the renderer")
      assertEqual "bare" (Just (mention "Highlights")) (refTargetOf "Highlights")

  , testCase "a protocol that names something other than a row is refused" $
      mapM_ (\raw -> assertEqual (T.unpack raw) Nothing (refTargetOf raw))
        -- `org-glance-overview:' names a TAG and `org-glance-state:' a keyword.
        [ "org-glance-overview:bookmark", "org-glance-state:STARTED"
        , "file:notes.org", "https://x.example/a", "mailto:a@b.example"
        , "docs/plan.org", "" ]

  , testCase "a subtree's targets are deduplicated and keep their order" $ do
      let text' = T.unlines
            [ "* one [[org-glance-visit:alpha][A]]"
            , "body [[org-glance-overview:tag][skipped]] and [[*Beta]]"
            , "** child [[org-glance-open:alpha][A again]]"
            , "trailing https://x.example/z" ]
      assertEqual "targets" [mention "alpha", mention "Beta"] (refTargets text')

    -- ONE GRAMMAR ACROSS TWO PROGRAMS: the peer slugs a kind on encode AND on
    -- read (`org-glance--kind-slug', src/data/org-glance-utils.el:183-187, its
    -- own "invariant 13"), so glance reads what the peer would have read.
  , testCase "a kind is canonicalized the way the peer canonicalizes it" $ do
      mapM_ (\(raw, want) -> assertEqual (T.unpack raw) (Just want) (refTargetOf raw))
        [ ("glance:a?kind=Roasted By",   Ref "a" (Just "roasted-by") ViaRow)
        , ("glance:a?kind=ROASTED-BY",   Ref "a" (Just "roasted-by") ViaRow)
        , ("glance:a?kind=roasted-by",   Ref "a" (Just "roasted-by") ViaRow)
          -- Trimmed and collapsed, so a typed kind and a written one are one.
        , ("glance:a?kind=  blocked   by  ", Ref "a" (Just "blocked-by") ViaRow)
          -- Whitespace ALONE is no kind, the way an empty one is none.
        , ("glance:a?kind=%20", Ref "a" (Just "%20") ViaRow) ]
      assertEqual "a kind of pure space declares nothing"
                  (Just (mention "a")) (refTargetOf "glance:a?kind= ")

    -- DEDUP IS ON THE PAIR, the peer's own rule: one row, two kinds, two edges.
  , testCase "two typed edges to one row are two references" $ do
      let text' = T.unlines
            [ "* one [[glance:alpha?kind=cites][A]]"
            , "again [[glance:alpha?kind=refutes][A]]"
            , "and plainly [[glance:alpha][A]]"
              -- Slugged first, so this is the SAME edge as the first one.
            , "and again [[glance:alpha?kind=Cites][A]]" ]
      assertEqual "the pair is the key"
        [ Ref "alpha" (Just "cites") ViaRow, Ref "alpha" (Just "refutes") ViaRow
        , mention "alpha" ]
        (refTargets text')

  , testCase "a subtree with nothing to point at yields no targets" $
      assertEqual "none" [] (refTargets "* plain\njust prose, and https://x.example\n")

    -- KNOWN LIMIT the `/links' grammar owns: a link written INSIDE another
    -- link's description defeats the scanner at both ends.
  , testCase "a reference nested in another link's description is not found" $
      assertEqual "neither the outer nor the inner" []
        (refTargets "- Referred from [[org-glance-visit:Meeting-1][\
                    \[[org-glance-visit:Contact-2][Wrike]] Goals]] on [2021-10-08 Fri]")
  ]


-- | K over a fixture: a target with an id, two referrers, a self-link, a bare row.
withRefTree :: ([HeadlineRecord] -> IO a) -> IO a
withRefTree = withDocDir "test" "a.org" (T.unlines
  [ "* Target"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: alpha"
  , ":END:"
  -- The target links to ITSELF, which org-glance's materialize footer writes.
  , "see [[org-glance-visit:alpha][myself]]"
  , "* By id"
  , "points at [[org-glance-visit:alpha][the target]]"
  , "* By title"
  , "points at [[*Second]] instead"
  , "* Second"
  , "* Org row"
  , ":PROPERTIES:"
  , ":ID: beadfeed-0000"
  , ":END:"
  , "* By org id"
  , "points at [[id:beadfeed-0000]]"
    -- `alpha' is a row's ORG_GLANCE_ID and no row's `:ID:', so this `id:'
    -- link names NOTHING — the namespaces never cross.
  , "* Crossed"
  , "points at [[id:alpha]]"
  , "* Neither"
  , "no links here" ])

-- | The rows of the fixture that Q matches, by title, in walk order.
refMatching :: Text -> IO [Text]
refMatching q = withRefTree (pure . titlesMatching q)

-- | The id of the row titled NAME; the @PATH#K@ fallback names a temp directory.
idOf :: Text -> [HeadlineRecord] -> IO Text
idOf name records = case [ hrId r | r <- records, hrTitle r == name ] of
  [one]  -> pure one
  _other -> assertFailure ("the fixture moved: no row titled " <> show name)

refSpec :: TestTree
refSpec = testGroup "References"
  [ virtualKeyCase "ref" refKey "alpha"

  , testCase "an id link makes the row that carries it a reference" $
      -- `Crossed' spells [[id:alpha]]; under the old ORG_GLANCE_ID conflation
      -- it would ride into this answer beside `By id'.
      assertEqual "by id" ["By id"] =<< refMatching "ref:alpha"

  , testCase "a row is not its own reference" $ do
      hit <- refMatching "ref:alpha"
      assertBool "the target is not in its own answer" ("Target" `notElem` hit)

  , testCase "a title link resolves against the target's title" $
      -- `Second' carries no ORG_GLANCE_ID, so its title is the only spelling.
      withRefTree $ \records -> do
        rid <- idOf "Second" records
        assertEqual "by title" ["By title"] (titlesMatching ("ref:" <> rid) records)

  , testCase "an org-id link resolves over the ID property, org-id's own" $
      withRefTree $ \records -> do
        rid <- idOf "Org row" records
        assertEqual "by :ID:" ["By org id"] (titlesMatching ("ref:" <> rid) records)

  , testCase "an id no row claims matches nothing, and does not fail" $
      assertEqual "unknown" [] =<< refMatching "ref:no-such-row"

  , testCase "the value keeps its case, alone among the predicates" $ do
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
      withRefTree $ \records -> do
        rid <- idOf "Second" records
        let hit q = titlesMatching q records
        assertEqual "both" [] (hit ("ref:alpha ref:" <> rid))
        assertEqual "either" ["By id", "By title"] (hit ("ref:alpha|" <> rid))

  , testCase "without a store behind it a ref resolves to nothing" $ do
      records <- qrRecords <$> loadDir viewDir
      assertEqual "no rows" []
        [ hrTitle r | r <- records, matchesFilter emptyEnv "ref:alpha" r ]
  ]


-- | THE GRAPH THE TWO DIRECTIONS ARE READ ON.  AN ARROW IS THE EDGE ITS AUTHOR
-- WROTE, so @Alpha --> Bee@ is a link in Alpha's own subtree; the word on it is
-- the kind the edge declares, and a bare arrow declares none.  Read down the
-- left column for @from:@ and up the right for @ref:@.
--
-- > FROM         EDGE                TO           how the link names it
-- > ─────────────────────────────────────────────────────────────────────
-- > Alpha   ──── blocked-by ─────▶   Bee          glance:bee
-- > Alpha   ──── (none) ─────────▶   Alpha        glance:alpha    SELF
-- > Alpha   ──── (none) ─────────▶   Why not?     [[*Why not?]]   by TITLE
-- > Bee     ──── cites ──────────▶   Cee          glance:cee
-- > Cee     ──── (none) ─────────▶   Echo         id:feedface-…   by :ID:
-- > Cee     ──── cites ──────────▶   Alpha        glance:alpha
-- > Delta   ──── Blocked-By ─────▶   Bee          glance:bee      the SLUG folds
-- > Delta   ──── (none) ─────────▶   ✗            glance:nosuchrow
-- > Dangler ──── (none) ─────────▶   ✗            glance:nosuchrow
-- > Selfy   ──── (none) ─────────▶   Selfy        glance:selfy    SELF
-- > Odd     ──── (none) ─────────▶   Bee          glance:bee
-- > Even    ──── (none) ─────────▶   Cee          glance:cee
-- > Crossed      no edge either way
--
-- So @Alpha -> Bee -> Cee -> Alpha@ is a cycle of typed edges, and every row
-- earns its place: @Echo@ answers in org-id's namespace alone where @Crossed@
-- carries that same string as an @ORG_GLANCE_ID@, so the two namespaces are
-- shown never to cross; @Selfy@ and @Alpha@ carry the materialize footer's
-- self-link; @Dangler@ points at nothing that exists; and @Odd@ holds a @?@
-- inside its id where @Even@ holds the id a wrong cut would leave behind.
withEdgeTree :: ([HeadlineRecord] -> IO a) -> IO a
withEdgeTree = withDocDir "edges" "a.org" (T.unlines
  [ "* Alpha"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: alpha"
  , ":END:"
  , "blocks [[glance:bee?kind=blocked-by][Bee]]"
    -- The materialize footer's own link, which no answer may serve.
  , "see [[glance:alpha][myself]]"
  , "and [[*Why not?]]"
  , "* Bee"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: bee"
  , ":END:"
  , "cites [[glance:cee?kind=cites][Cee]]"
  , "* Cee"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: cee"
  , ":END:"
    -- org-id's namespace: it names Echo's `:ID:' and never Crossed's own id.
  , "names [[id:feedface-0001]]"
  , "cites [[glance:alpha?kind=cites][Alpha]]"
  , "* Delta"
    -- The KIND slugs on the EDGE too, so this edge and Alpha's are one kind.
  , "blocks [[glance:bee?kind=Blocked-By][Bee]]"
  , "and [[glance:nosuchrow][nothing]]"
  , "* Echo"
  , ":PROPERTIES:"
  , ":ID: feedface-0001"
  , ":END:"
  , "* Why not?"
  , "* Crossed"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: feedface-0001"
  , ":END:"
  , "* Dangler"
  , "points at [[glance:nosuchrow][nothing]]"
  , "* Selfy"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: selfy"
  , ":END:"
  , "see [[glance:selfy][myself]]"
  , "* Odd"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: odd?id"
  , ":END:"
  , "points at [[glance:bee][Bee]]"
  , "* Even"
  , ":PROPERTIES:"
  , ":ORG_GLANCE_ID: odd"
  , ":END:"
  , "points at [[glance:cee][Cee]]" ])

-- | The rows of the drawn graph that Q matches, by title, in walk order.
edgeMatching :: Text -> IO [Text]
edgeMatching q = withEdgeTree (pure . titlesMatching q)

-- | Q matches exactly TITLES over the drawn graph.
edges :: Text -> [Text] -> Assertion
edges q titles = assertEqual (T.unpack q) titles =<< edgeMatching q

fromSpec :: TestTree
fromSpec = testGroup "The reverse reference"
  [ virtualKeyCase "from" fromKey "alpha"

    -- THE ASYMMETRY, read off the drawn graph: `ref:' walks the arrows into a
    -- row and `from:' the arrows out of it, so the two answers share no row.
  , testCase "ref walks the arrows in and from the arrows out" $ do
      edges "ref:bee"  ["Alpha", "Delta", "Odd"]
      edges "from:bee" ["Cee"]
      edges "ref:cee"  ["Bee", "Even"]
      edges "from:cee" ["Alpha", "Echo"]

    -- THE DUALITY the two keys stand on: `from:T' serves R where `ref:R' serves T.
  , testCase "from:T serves R exactly where ref:R serves T" $ do
      edges "from:alpha" ["Bee", "Why not?"]
      edges "ref:bee"    ["Alpha", "Delta", "Odd"]
      -- Alpha is in `ref:bee''s answer, so Bee is in `from:alpha''s, and back.
      edges "ref:alpha"  ["Cee"]
      edges "from:cee"   ["Alpha", "Echo"]

  , testCase "an anchor no row claims serves nothing either way" $ do
      edges "ref:no-such-row"  []
      edges "from:no-such-row" []

  , testCase "a row is not its own target in either direction" $ do
      -- Selfy's ONE link is the materialize footer's, pointing at itself.
      edges "ref:selfy"  []
      edges "from:selfy" []
      -- And Alpha, which carries a self-link beside its real ones.
      assertBool "Alpha rode into its own ref answer"
        . notElem "Alpha" =<< edgeMatching "ref:alpha"
      assertBool "Alpha rode into its own from answer"
        . notElem "Alpha" =<< edgeMatching "from:alpha"

    -- BOTH NAMESPACES, EACH DIRECTION, and they never cross: Cee's `[[id:…]]'
    -- names Echo's `:ID:' property, where Crossed carries the same string as an
    -- ORG_GLANCE_ID and answers nothing.
  , testCase "each namespace answers its own protocol, both ways" $
      withEdgeTree $ \records -> do
        echo <- idOf "Echo" records
        quest <- idOf "Why not?" records
        let hit q = titlesMatching q records
        assertEqual "an org-id link, pointed at" ["Cee"] (hit ("ref:" <> echo))
        assertEqual "and read from the other end" ["Alpha", "Echo"] (hit "from:cee")
        assertEqual "ORG_GLANCE_ID is no `:ID:'" [] (hit "ref:feedface-0001")
        assertEqual "a title link, pointed at" ["Alpha"] (hit ("ref:" <> quest))
        assertEqual "and read from the other end" ["Bee", "Why not?"] (hit "from:alpha")
        assertEqual "nothing points at Delta" [] . hit . ("ref:" <>) =<< idOf "Delta" records

  , testCase "the bare forms are kind-blind, as they always were" $ do
      -- Bee's edge into Cee carries a kind and Even's does not; the bare form
      -- serves both, which is the shipped law untouched.
      edges "ref:cee"  ["Bee", "Even"]
      edges "from:cee" ["Alpha", "Echo"]

  , testCase "a kind test narrows to the edges carrying that kind" $ do
      edges "ref:bee?kind=blocked-by" ["Alpha", "Delta"]
      edges "ref:bee?kind=cites"      []
      edges "ref:cee?kind=cites"      ["Bee"]
      edges "from:alpha?kind=blocked-by" ["Bee"]
      edges "from:alpha?kind=cites"      []
      edges "from:cee?kind=cites"        ["Alpha"]

    -- ONE SLUG ACROSS TWO PROGRAMS, and across the two sides of one query: the
    -- EDGE slugs (Delta writes `Blocked-By') and so does the TOKEN.  The quoted
    -- spelling is the one that may carry spaces, the scanner cutting on one.
  , testCase "the kind slugs on the token the way it slugs on the edge" $ do
      edges "ref:bee?kind=BLOCKED-BY"      ["Alpha", "Delta"]
      edges "ref:\"bee?kind=Blocked By\""  ["Alpha", "Delta"]
      edges "from:\"alpha?kind=  BLOCKED   BY  \"" ["Bee"]
      edges "ref:bee?kind=no-such-kind"    []

    -- Only the peer's own key cuts, and `kindIn' reads it out of a `&' list.
  , testCase "a query string declaring no kind leaves the value whole" $ do
      edges "ref:cee?other=x"                  []
      edges "ref:cee?kind="                    []
      edges "ref:\"cee?other=x&kind=cites\""   ["Bee"]

    -- THE TEXT `?' STAYS TEXT: the cut is taken only where a kind comes out of
    -- it, so an id carrying one is left whole.  Had it been cut, the anchor
    -- would be `odd', which Even claims, and the answer would be Cee.
  , testCase "a question mark declaring no kind stays inside the id" $ do
      edges "from:odd?id" ["Bee"]
      edges "from:odd"    ["Cee"]

  , testCase "the starred anchor is the union over the slot" $ do
      edges "ref:*any*"  ["Alpha", "Bee", "Cee", "Delta", "Odd", "Even"]
      edges "from:*any*" ["Alpha", "Bee", "Cee", "Echo", "Why not?"]
      -- Every row it serves is served by SOME named anchor, which is what the
      -- union means: the rows outside it point at nothing that exists (Dangler),
      -- point only at themselves (Selfy) or point at nothing at all.
      edges "-ref:*any*"  ["Echo", "Why not?", "Crossed", "Dangler", "Selfy"]
      edges "-from:*any*" ["Delta", "Crossed", "Dangler", "Selfy", "Odd", "Even"]

  , testCase "the starred anchor takes a kind like a named one" $ do
      edges "ref:*any*?kind=blocked-by" ["Alpha", "Delta"]
      edges "from:*any*?kind=cites"     ["Alpha", "Cee"]
      edges "ref:*any*?kind=no-such-kind" []

    -- THE VALUE IS NOT FOLDED on these keys, so a shouted meta is an id like
    -- any other and no row claims it; a half-starred word is no meta either.
  , testCase "the anchor keeps its case, the starred word included" $ do
      edges "ref:*ANY*"  []
      edges "from:*ANY*" []
      edges "ref:*any"   []
      edges "ref:any*"   []

  , testCase "a half-typed reverse ref narrows nothing" $ do
      every <- edgeMatching ""
      edges "from:" every

  , testCase "the alternatives OR and two tokens on one key AND" $ do
      edges "from:alpha|cee" ["Alpha", "Bee", "Echo", "Why not?"]
      edges "from:alpha from:cee" []
      edges "ref:bee|cee" ["Alpha", "Bee", "Delta", "Odd", "Even"]

    -- TWO AXES, and this is what says so: on ONE axis the added token would OR
    -- against the plain one and serve `Why not?' and `Even' too.
  , testCase "ref and from stand as two axes, so an added token widens one" $ do
      edges "ref:cee +from:alpha" ["Bee"]
      edges "ref:cee|alpha" ["Bee", "Cee", "Even"]
      edges "ref:cee +ref:alpha" ["Bee", "Cee", "Even"]

  , testCase "the two axes AND, and token order carries nothing" $ do
      edges "ref:cee from:alpha" ["Bee"]
      edges "from:alpha ref:cee" ["Bee"]
      edges "ref:*any* from:*any*" ["Alpha", "Bee", "Cee"]
      edges "from:*any* ref:*any*" ["Alpha", "Bee", "Cee"]
      edges "ref:bee tag:nosuchtag" []

    -- AN EDGE CROSSES FILES, which is why neither direction and neither map is
    -- a per-file fact: the target's spellings live in one file and the link in
    -- another, so the whole store is what resolves an edge.
  , testCase "an edge across two files resolves both ways" $
      withTempDir $ \dir -> do
        _ <- orgFile dir "here.org" "* Here\npoints at [[glance:there][There]]\n"
        _ <- orgFile dir "there.org" (T.unlines
               [ "* There", ":PROPERTIES:", ":ORG_GLANCE_ID: there", ":END:" ])
        records <- qrRecords <$> loadDir dir
        let hit q = titlesMatching q records
        assertEqual "the fixture no longer loads two rows" 2 (length records)
        assertEqual "pointed at from the other file" ["Here"] (hit "ref:there")
        assertEqual "and read from the other end" ["There"] (hit "from:*any*")
        assertEqual "the pointing row is the one with an edge" ["Here"] (hit "ref:*any*")

  , testCase "without a store behind it neither direction resolves" $
      withEdgeTree $ \records ->
        mapM_ (\q -> assertEqual (T.unpack q) []
                       [ hrTitle r | r <- records, matchesFilter emptyEnv q r ])
              ["ref:bee", "from:bee", "ref:*any*", "from:*any*"]
  ]

-- | @planned@: the virtual key over both date columns, decidable from a row's cells.
plannedSpec :: TestTree
plannedSpec = testGroup "Planned"
  [ virtualKeyCase "planned" plannedKey "*empty*"

  , testCase "a row is planned when either date cell holds anything" $ do
      matches "-planned:*empty*" [Ship, Privet, Reply]
      -- Ship carries both, Privet a schedule, Reply a deadline, Drop a `CLOSED:'.
      matches "planned:*empty*" [Plain, Drop, Schema]

  , testCase "and neither date column alone answers the same question" $ do
      matches "-scheduled:*empty*" [Ship, Privet]
      matches "-deadline:*empty*" [Ship, Reply]

  , testCase "a value is the date prefix, asked of both cells at once" $ do
      matches "planned:2026-08" [Ship, Privet, Reply]
      matches "planned:2026-08-0" [Ship, Privet]
      matches "planned:2026-08-10" [Reply]
      matches "planned:03" []

  , testCase "an empty value narrows nothing, as every key's does" $ do
      every <- matching ""
      matches "planned:" every

  , testCase "two of them AND like any two tokens, and either is one token" $ do
      matches "planned:2026-08-03 planned:2026-08-10" []
      matches "planned:2026-08-03|2026-08-10" [Privet, Reply]
      matches "planned:2026-08-01 planned:2026-08-05" [Ship]

  , testCase "negation composes with everything else" $ do
      matches "state:*active* -planned:*empty*" [Ship, Privet, Reply]
      matches "state:*inactive* -planned:*empty*" []
      matches "-planned:2026-08" [Plain, Drop, Schema]

    -- NO TREE CAN TAKE THE KEY AWAY: key resolution is over a CLOSED list.
  , testCase "no tree can take the key away, the keys being a closed list" $ do
      assertEqual "still the date key" [Term Unsigned (Just "planned") "*empty*"]
                  (parsed "planned:*empty*")
      records <- qrRecords <$> loadDir viewDir
      let carried = vocabularyOf records
      assertBool "the fixture carries tags to offer" (length carried >= 2)
      assertEqual "and no tag the tree carries is a key" []
                  [ tag | tag <- carried
                        , Term _sign key _v <- parsed (tag <> ":x")
                        , key /= Nothing ]
      -- The columns and the keys named above, spelled out here and derived
      -- nowhere, so a key ADDED or LOST is what goes red: a comparison of two
      -- derived lists moves on both sides and catches neither.
      -- THE PRICE OF A KEY ADDED, paid here: `from:X' was a free-text needle
      -- before the reverse reference took the word, and the list is what says
      -- which words the grammar has claimed.
      assertEqual "the keys, spelled rather than derived"
                  [ "deadline", "from", "planned", "priority", "ref", "scheduled"
                  , "sort", "state", "substring", "tag", "title" ]
                  (sort (filterKeys <> grammarKeys))
      -- And every one of them resolves to itself, which is what makes it a key.
      assertEqual "the keys are exactly the columns plus the ones the grammar owns"
                  (sort (filterKeys <> grammarKeys))
                  (sort [ k | k <- filterKeys <> grammarKeys
                            , Term _sign (Just k') _v <- parsed (k <> ":x"), k' == k ])
  ]
  where grammarKeys = [plannedKey, refKey, fromKey, sortKey, substringKey]

-- | The rows whose two date cells are both empty: what every comparison and
-- every range must leave outside, byte order putting @""@ below every literal.
undated :: [Row]
undated = [Reply, Plain, Drop, Schema]

-- | V as a value the scanner hands over whole, spaces and colons included.
quotedValue :: Text -> Text
quotedValue v = "\"" <> v <> "\""

-- | @key:OP D@ — the four operators, the granularity law, and what the sign,
-- the bar and the axis do around an atom that happens to be a comparison.
comparisonSpec :: TestTree
comparisonSpec = testGroup "Comparisons"
  [ testCase "the four operators at day granularity" $ do
      matches "scheduled:<2026-08-03" [Ship]
      matches "scheduled:<=2026-08-03" [Ship, Privet]
      matches "scheduled:>=2026-08-03" [Privet]
      matches "scheduled:>2026-08-03" []
      matches "deadline:>=2026-08-10" [Reply]
      matches "deadline:>2026-08-10" []
      matches "deadline:<2026-08-05" []
      matches "deadline:<=2026-08-05" [Ship]

    -- THE GRANULARITY LAW IN ONE ROW: Ship is scheduled at 09:30 on the first,
    -- so it is INSIDE `<=' that day and OUTSIDE `>' it — the inclusive cutting
    -- at the literal's last instant and the strict one at the same place.
  , testCase "the inclusives cut at the literal's last instant" $ do
      matches "scheduled:<=2026-08-01" [Ship]
      matches "scheduled:>2026-08-01" [Privet]
      matches "scheduled:>=2026-08-01" [Ship, Privet]
      matches "scheduled:<2026-08-01" []

  , testCase "and the same four at month and year granularity" $ do
      matches "scheduled:<2026-09" [Ship, Privet]
      matches "scheduled:<=2026-08" [Ship, Privet]
      matches "scheduled:>=2026-08" [Ship, Privet]
      matches "scheduled:>2026-08" []
      matches "scheduled:<2026-08" []
      matches "scheduled:>2026-07" [Ship, Privet]
      matches "deadline:<=2026-07" []
      matches "deadline:>2026" []
      matches "deadline:>=2026" [Ship, Reply]
      matches "deadline:<=2026" [Ship, Reply]
      matches "deadline:<2026" []
      -- A prefix is a literal whatever it cuts: the first nine days of a month.
      matches "scheduled:<=2026-08-0" [Ship, Privet]
      matches "scheduled:>2026-08-0" []

    -- LAW 3: the comparison forms and the prefix form are ONE reading of one
    -- literal, so the bare value is the closed interval its two ends spell.
  , testCase "the bare form is the closed interval: k:D is k:>=D and k:<=D" $ do
      records <- qrRecords <$> loadDir viewDir
      sequence_
        [ assertEqual (T.unpack (key <> ":" <> d))
            (titlesMatching (key <> ":" <> quotedValue d) records)
            (titlesMatching (key <> ":" <> quotedValue (">=" <> d) <> " "
                               <> key <> ":" <> quotedValue ("<=" <> d)) records)
        | key <- ["scheduled", "deadline"]
        , d   <- [ "2026", "2026-08", "2026-08-0", "2026-08-01", "2026-08-05"
                 , "2026-08-01 09", "2026-08-10 17:00", "2027", "03" ] ]

    -- LAW 5: `""' is below every literal in byte order, so an unguarded `<'
    -- would serve every row the tree never dated.
  , testCase "the empty cell is outside every comparison and every range" $ do
      mapM_ (\v -> do
               hit <- matching ("scheduled:" <> v)
               assertEqual ("scheduled:" <> T.unpack v <> " served an undated row")
                           [] [ row | row <- hit, row `elem` undated ])
            [ "<2026-09", "<=2026-09", ">2020", ">=2020", "<9", ">0"
            , "2020..2030", "0..9" ]
      matches "scheduled:<9" [Ship, Privet]
      matches "scheduled:2020..2030" [Ship, Privet]
      matches "planned:<2026-09" [Ship, Privet, Reply]

    -- LAW 6: the four operators do not pair off under the sign, and the surface
    -- must never rewrite one into another.
  , testCase "negation is no mirror: the undated rows part the two" $ do
      mirror  <- matching "scheduled:>=2026-08-03"
      negated <- matching "-scheduled:<2026-08-03"
      assertEqual "the comparison" [Privet] mirror
      assertEqual "and the sign, which keeps the undated rows"
                  (Privet : undated) negated
      assertBool "the two answers must never be simplified into each other"
                 (mirror /= negated)

  , testCase "an operator with no literal narrows nothing, and negated empties" $ do
      every <- matching ""
      mapM_ (`matches` every)
            [ "scheduled:>", "scheduled:>=", "scheduled:<", "scheduled:<="
            , "planned:>=", "deadline:<" ]
      matches "-scheduled:>=" []
      matches "-planned:<" []
      matches "scheduled:2026-08-03 scheduled:>" [Privet]
      matches "scheduled:2026-08-03|>" [Privet]

  , testCase "a literal naming no date matches no row, and narrows all the same" $ do
      matches "scheduled:>banana" []
      matches "scheduled:<banana" []
      matches "scheduled:>*empty*" []
      matches "scheduled:banana..zebra" []
      -- It is an ATOM rather than a half-typed token, so the sign inverts it.
      every <- matching ""
      matches "-scheduled:<banana" every

    -- CONSERVATIVITY: the operator is read at one position on one set of keys.
  , testCase "the operator is read on the timestamp keys and nowhere else" $ do
      matches "title:>the" []
      matches "title:<a>" []
      matches "state:>A" []
      matches "priority:>A" []
      matches "tag:<web" []
      matches "substring:>ship" []
      matches ">2026-08" []
      matches "\"scheduled:>=2026-08\"" []

  , testCase "the separator's alias and a mid-value quote spell the same token" $ do
      matches "scheduled:>=2026-08-03" [Privet]
      matches "scheduled=>=2026-08-03" [Privet]
      matches "scheduled:\">=2026-08-03\"" [Privet]
      -- With no separator the key would be `scheduled>', which resolves to nothing.
      assertEqual "no separator, no key"
                  [Term Unsigned Nothing "scheduled>=2026-08-03"]
                  (parsed "scheduled>=2026-08-03")
      matches "scheduled>=2026-08-03" []

  , testCase "alternatives split first, so each carries its own operator" $ do
      matches "scheduled:<2026-08-02|>=2026-08-03" [Ship, Privet]
      matches "scheduled:>=2026-08-03|2026-08-01" [Ship, Privet]
      matches "deadline:<2026-08-06|>2026-09" [Ship]
      matches "scheduled:<2026-08-01|>2026-08-03" []
      -- A bar is never a range: neither half reaches the other's rows.
      matches "scheduled:2026-08-01|2026-08-03" [Ship, Privet]

    -- LAW 7: only the atomic predicate gains cases; the axis law is quoted.
  , testCase "a comparison is one more atom, so sign and axis are untouched" $ do
      matches "scheduled:<2026-08-02 +scheduled:>=2026-08-03" [Ship, Privet]
      every <- matching ""
      matches "scheduled:<2026-09 +scheduled:*empty*" every
      matches "scheduled:>=2026-08 deadline:<2026-08-06" [Ship]
      matches "state:*active* scheduled:<2026-08-03" [Ship]

    -- The oracle is the CELL, read off the record rather than off the matcher:
    -- a bare value answers the prefix reading it answered before the operators.
  , testCase "a bare value is the prefix reading it was, byte for byte" $ do
      records <- qrRecords <$> loadDir viewDir
      let shown pick r = T.toLower (displayText (fromMaybe "" (pick r)))
      sequence_
        [ assertEqual (T.unpack (key <> ":" <> v))
            [ hrTitle r | r <- records, v `T.isPrefixOf` shown pick r ]
            (titlesMatching (key <> ":" <> quotedValue v) records)
        | (key, pick) <- [("scheduled", hrScheduled), ("deadline", hrDeadline)]
        , v <- [ "", "2026", "2026-08", "2026-08-0", "2026-08-01"
               , "2026-08-01 09:30", "2026-08-05", "2026-08-10", "03", "banana"
               , "2027" ] ]
  ]

-- | Law 9's counterexample: one row whose two date cells straddle every interval
-- between them, which is what two tokens on @planned@ cannot tell from a hit.
withSpanTree :: ([HeadlineRecord] -> IO a) -> IO a
withSpanTree = withDocDir "test" "a.org" (T.unlines
  [ "* Straddles"
  , "SCHEDULED: <2027-01-01 Fri> DEADLINE: <2020-01-01 Wed>"
  , "* Inside"
  , "SCHEDULED: <2026-08-15 Sat>"
  , "* Outside"
  , "SCHEDULED: <2025-01-01 Wed>" ])

-- | @A..B@ — sugar for two tokens on a single-cell key, and the ONE reading two
-- tokens have no spelling for on a key that names several.
rangeSpec :: TestTree
rangeSpec = testGroup "Ranges"
  [ testCase "on a single-cell key A..B is the two tokens it is sugar for" $ do
      records <- qrRecords <$> loadDir viewDir
      sequence_
        [ assertEqual (T.unpack (key <> ":" <> lo <> ".." <> hi))
            (titlesMatching (key <> ":>=" <> lo <> " " <> key <> ":<=" <> hi) records)
            (titlesMatching (key <> ":" <> lo <> ".." <> hi) records)
        | key      <- ["scheduled", "deadline"]
        , (lo, hi) <- [ ("2026-08-01", "2026-08-03"), ("2026-08-04", "2026-08-06")
                      , ("2026-08", "2026-08"), ("2026", "2026")
                      , ("2026-08-15", "2026-10-08"), ("2027", "2028") ] ]

  , testCase "and it names a range no prefix names" $ do
      matches "deadline:2026-08-04..2026-08-06" [Ship]
      matches "scheduled:2026-08-01..2026-08-03" [Ship, Privet]
      matches "deadline:2026-08-06..2026-08-09" []
      matches "planned:2026-08-04..2026-08-06" [Ship]

  , testCase "a range end with nothing behind it is half-typed" $ do
      every <- matching ""
      mapM_ (`matches` every)
            [ "scheduled:..", "scheduled:2026-08..", "scheduled:..2026-08"
            , "planned:.." ]
      matches "-scheduled:2026-08.." []

    -- LAW 9, and it must go RED if the range is ever desugared into two tokens.
  , testCase "on planned it says the one thing two tokens cannot" $
      withSpanTree $ \records -> do
        let hit q = titlesMatching q records
        assertEqual "the two tokens ask the axis twice, and the straddler passes"
                    ["Straddles", "Inside"]
                    (hit "planned:>=2026-08-01 planned:<=2026-08-31")
        assertEqual "where the range asks ONE cell to lie inside it"
                    ["Inside"] (hit "planned:2026-08-01..2026-08-31")
        -- Naming the cell is the workaround, and it answers the range's answer.
        assertEqual "the named cell" ["Inside"]
                    (hit "scheduled:>=2026-08-01 scheduled:<=2026-08-31")
  ]

-- | THE DAY WORDS — the date values resolved off the env's day and never off
-- the wall clock, so every case here answers the same in a year.
todaySpec :: TestTree
todaySpec = testGroup "The day words"
  [ testCase "bare, a word is the prefix reading of the day it names" $ do
      matchesOn (day 2026 8 1) "scheduled:today" [Ship]
      matchesOn (day 2026 8 3) "scheduled:today" [Privet]
      matchesOn (day 2026 8 5) "deadline:today" [Ship]
      matchesOn (day 2026 8 10) "deadline:today" [Reply]
      matchesOn (day 2026 8 2) "planned:today" []
      matchesOn (day 2026 8 3) "planned:today" [Privet]
      -- @tomorrow@ rides the SAME reader, one day over ('Glance.Query.dayNamed').
      matchesOn (day 2026 7 31) "scheduled:tomorrow" [Ship]
      matchesOn (day 2026 8 2) "scheduled:tomorrow" [Privet]
      matchesOn (day 2026 8 9) "deadline:tomorrow" [Reply]
      -- Folded like every other value.
      matchesOn (day 2026 8 3) "scheduled:TODAY" [Privet]
      matchesOn (day 2026 8 2) "scheduled:Tomorrow" [Privet]
      -- A NEAR-MISS IS NO WORD: the roster's spellings alone read off the clock,
      -- and every other literal matches no row the way @state:TOD@ matches none.
      matchesOn (day 2026 8 3) "scheduled:todayy" []
      matchesOn (day 2026 8 3) "scheduled:tod" []
      matchesOn (day 2026 8 3) "scheduled:yesterday" []
      matchesOn (day 2026 8 3) "scheduled:*today" []

  , testCase "behind an operator, and at either end of a range" $ do
      matchesOn (day 2026 8 3) "scheduled:<today" [Ship]
      matchesOn (day 2026 8 3) "scheduled:>=today" [Privet]
      matchesOn (day 2026 8 3) "scheduled:<=today" [Ship, Privet]
      matchesOn (day 2026 8 1) "scheduled:>today" [Privet]
      matchesOn (day 2026 8 1) "scheduled:today..2026-08-03" [Ship, Privet]
      matchesOn (day 2026 8 3) "scheduled:2026-08-01..today" [Ship, Privet]
      matchesOn (day 2026 8 3) "planned:today..today" [Privet]
      -- BOTH ENDS A WORD, and the two words in one range.
      matchesOn (day 2026 8 1) "scheduled:today..tomorrow" [Ship]
      matchesOn (day 2026 8 2) "scheduled:>=tomorrow" [Privet]
      -- The agenda case: everything planned up to and including today.
      matchesOn (day 2026 8 5) "-planned:*empty* planned:<=today" [Ship, Privet]

    -- READ-COMPAT: the old spelling is taken wherever the bare word is, and a
    -- range may mix the two -- so a stored view half-rewritten still answers.
  , testCase "*today* is today's old spelling and reads the same everywhere" $ do
      matchesOn (day 2026 8 3) "scheduled:*today*" [Privet]
      matchesOn (day 2026 8 3) "scheduled:*TODAY*" [Privet]
      matchesOn (day 2026 8 3) "scheduled:<=*today*" [Ship, Privet]
      matchesOn (day 2026 8 3) "planned:*today*..*today*" [Privet]
      matchesOn (day 2026 8 1) "scheduled:*today*..2026-08-03" [Ship, Privet]
      matchesOn (day 2026 8 1) "scheduled:today..*today*+2d" [Ship, Privet]
      matchesOn (day 2026 8 1) "scheduled:*today*..today+2d" [Ship, Privet]
      assertEqual "the word the family spells" "*today*" todayMeta

  , testCase "with no clock behind it a word names no day" $ do
      matches "scheduled:today" []
      matches "scheduled:tomorrow" []
      matches "scheduled:*today*" []
      matches "scheduled:>=today" []
      matches "planned:today..2026-12-31" []
      -- It is an ATOM all the same, so its sign inverts into every row.
      every <- matching ""
      matches "-scheduled:today" every
      matches "-scheduled:*today*" every

    -- ONE ROSTER, and the reader the planning wall goes through is the one the
    -- filter reads: a word the roster names is a word BOTH surfaces take.
  , testCase "the roster is the words the one base reader answers" $ do
      assertEqual "the day words, in the order the roster declares them"
                  [("today", 0), ("tomorrow", 1), (todayMeta, 0)] dayWords
      assertEqual "the old spelling rides last, being the one nothing offers"
                  (Just todayMeta) (fmap fst (listToMaybe (reverse dayWords)))
  ]
  where day = fromGregorian

-- | A tree standing on the days a CLIPPED shift lands on, and on the ones an
-- unclipped shift would overshoot to.
withClipTree :: ([HeadlineRecord] -> IO a) -> IO a
withClipTree = withDocDir "test" "a.org" (T.unlines
  [ "* Feb 28 2026"
  , "SCHEDULED: <2026-02-28 Sat>"
  , "* Mar 3 2026"
  , "SCHEDULED: <2026-03-03 Tue>"
  , "* Feb 29 2024"
  , "SCHEDULED: <2024-02-29 Thu>"
  , "* Feb 28 2025"
  , "SCHEDULED: <2025-02-28 Fri>"
  , "* Mar 1 2025"
  , "SCHEDULED: <2025-03-01 Sat>" ])

-- | @BASE(+|-)N UNIT@ — a date value carrying a SHIFT.  It resolves AT COMPILE
-- to a plain day literal, so every law above then applies to it untouched; the
-- day it resolves against is injected, so every case here answers the same in a
-- year.
shiftSpec :: TestTree
shiftSpec = testGroup "Shifted date values"
  [ testCase "each of org's own units, at day granularity" $ do
      matchesOn (day 2026 7 31) "scheduled:today+1d" [Ship]
      matchesOn (day 2026 8 2)  "scheduled:today-1d" [Ship]
      matchesOn (day 2026 7 25) "scheduled:today+1w" [Ship]
      matchesOn (day 2026 8 10) "scheduled:today-1w" [Privet]
      matchesOn (day 2026 7 1)  "scheduled:today+1m" [Ship]
      matchesOn (day 2026 9 3)  "scheduled:today-1m" [Privet]
      matchesOn (day 2025 8 1)  "scheduled:today+1y" [Ship]
      matchesOn (day 2027 8 10) "deadline:today-1y" [Reply]
      -- EVERY WORD IS A BASE, `tomorrow' being one day further along.
      matchesOn (day 2026 7 30) "scheduled:tomorrow+1d" [Ship]
      matchesOn (day 2026 8 4)  "scheduled:tomorrow-2d" [Privet]
      -- And the old spelling moves exactly as the bare word does.
      matchesOn (day 2026 7 31) "scheduled:*today*+1d" [Ship]
      matchesOn (day 2026 8 10) "scheduled:*today*-1w" [Privet]
      -- A SPELLED DAY IS A BASE TOO, and it needs no clock behind it.
      matches "scheduled:2026-07-31+1d" [Ship]
      matches "deadline:2026-08-12-2d" [Reply]
      matches "planned:2026-07-04+4w" [Ship]
      -- A count of zero is a decimal run, and the base stands still under it.
      matches "scheduled:2026-08-03+0d" [Privet]

  , testCase "a week is seven days, whichever way it is spelled" $ do
      records <- qrRecords <$> loadDir viewDir
      sequence_
        [ assertEqual (T.unpack (key <> ": " <> weeks <> " and " <> days))
            (titlesMatching (key <> ":" <> days) records)
            (titlesMatching (key <> ":" <> weeks) records)
        | key <- ["scheduled", "deadline", "planned"]
        , (weeks, days) <- [ ("2026-08-15-2w", "2026-08-15-14d")
                           , (">=2026-07-25+1w", ">=2026-07-25+7d")
                           , ("2026-07-20..2026-07-20+3w", "2026-07-20..2026-07-20+21d") ] ]
      -- With teeth: the pair above names rows rather than agreeing on none.
      matches "scheduled:2026-08-15-2w" [Ship]

    -- ORG'S OWN CALENDAR ARITHMETIC, which CLIPS a month and a year to the
    -- target month's last day rather than rolling over into the next.
  , testCase "a month and a year are clipped, never rolled over" $
      withClipTree $ \records -> do
        let hit q = titlesMatching q records
        assertEqual "the tree carries the days a rollover would land on"
                    ["Mar 3 2026", "Mar 1 2025"] (hit "scheduled:2026-03-03|2025-03-01")
        assertEqual "Jan 31 + 1m is February's last, and 2026 is no leap year"
                    ["Feb 28 2026"] (hit "scheduled:2026-01-31+1m")
        assertEqual "the same base in a leap year reaches the 29th"
                    ["Feb 29 2024"] (hit "scheduled:2024-01-31+1m")
        assertEqual "Feb 29 + 1y clips to the 28th"
                    ["Feb 28 2025"] (hit "scheduled:2024-02-29+1y")
        assertEqual "the clip LOSES the day, so a year on from it is the 28th again"
                    ["Feb 28 2026"] (hit "scheduled:2025-02-28+1y")

  , testCase "both ends of a range take a shift" $ do
      matchesOn (day 2026 8 3) "scheduled:today-2d..today+2d" [Ship, Privet]
      matchesOn (day 2026 8 1) "scheduled:today..today+2d" [Ship, Privet]
      matchesOn (day 2026 7 31) "scheduled:tomorrow..tomorrow+2d" [Ship, Privet]
      matches "scheduled:2026-07-31+1d..2026-08-05-2d" [Ship, Privet]
      matches "deadline:2026-08-01+3d..2026-08-01+5d" [Ship]
      -- THE 30-DAY AGENDA IN ONE TOKEN, which is what the ends are for.
      matchesOn (day 2026 8 1) "-planned:*empty* planned:today..today+30d"
                [Ship, Privet, Reply]
      -- And the old spelling serves it at either end, mixed ends included.
      matchesOn (day 2026 8 1) "-planned:*empty* planned:*today*..*today*+30d"
                [Ship, Privet, Reply]
      matchesOn (day 2026 8 1) "-planned:*empty* planned:*today*..today+30d"
                [Ship, Privet, Reply]

    -- The shift is a SPELLING of a day literal, so the granularity cuts, the
    -- empty cell's exclusion and the no-mirror law all read it as one.
  , testCase "a resolved shift is a day literal, and every law then applies" $ do
      matchesOn (day 2026 8 5) "scheduled:<today-2d"  [Ship]
      matchesOn (day 2026 8 5) "scheduled:<=today-2d" [Ship, Privet]
      matchesOn (day 2026 8 5) "scheduled:>=today-2d" [Privet]
      matchesOn (day 2026 8 5) "scheduled:>today-2d"  []
      hit <- matchingIn (onDay (day 2026 8 5)) "scheduled:<today+10y"
      assertEqual "the empty cell stays outside a shifted comparison" []
                  [ row | row <- hit, row `elem` undated ]
      mirror  <- matchingIn (onDay (day 2026 8 5)) "scheduled:>=today-2d"
      negated <- matchingIn (onDay (day 2026 8 5)) "-scheduled:<today-2d"
      assertBool "and negation is no mirror over one either" (mirror /= negated)
      -- Alternatives split above the literal, so each arm carries its own shift.
      matchesOn (day 2026 8 3) "scheduled:today-2d|today" [Ship, Privet]
      matchesOn (day 2026 8 3) "scheduled:today-2d|*today*" [Ship, Privet]

    -- ONE PARSER: the quoted form is the one that may carry spaces, and it
    -- folds onto the compact spelling before any form is read.
  , testCase "the quoted spaced spelling and the compact one are one query" $ do
      records <- qrRecords <$> loadDir viewDir
      sequence_
        [ assertEqual (T.unpack (spaced <> " and " <> compact))
            (titlesMatchingIn (onDay (day 2026 8 3)) ("scheduled:" <> compact) records)
            (titlesMatchingIn (onDay (day 2026 8 3)) ("scheduled:" <> quotedValue spaced)
                              records)
        | (spaced, compact) <-
            [ ("<= today + 30 days", "<=today+30d")
            , ("today - 2 days .. today + 2 days", "today-2d..today+2d")
            , ("today .. today + 30 days", "today..today+30d")
            , ("tomorrow + 1 week", "tomorrow+1w")
            , ("2026-08-01 + 1 week", "2026-08-01+1w")
            , (">= 2026-08-01 + 1 month", ">=2026-08-01+1m")
            , ("today + 1 year", "today+1y")
            , ("<= *today* + 30 days", "<=*today*+30d")
            , ("+ 30 days", "+30d")
            , ("2026-07-31 + 1 day", "2026-07-31+1d") ] ]
      -- The long word is CASE-FOLDED like every other value.
      matchesOn (day 2026 7 4) "scheduled:\"TODAY + 30 DAYS\"" [Privet]
      matchesOn (day 2026 7 4) "scheduled:\"*TODAY* + 30 DAYS\"" [Privet]
      -- And the one space that survives is the timed stamp's own.
      matches "scheduled:\"2026-08-01 09:30\"" [Ship]

    -- THE BARE SHIFT IS TODAY-RELATIVE, decided off the planning grammar's own
    -- precedent (`set-planning' reads a bare `+3d' that way, docs/commands.md);
    -- consistency is the tiebreaker, and this case is the pin.
  , testCase "a bare shift is the request's own day moved" $ do
      records <- qrRecords <$> loadDir viewDir
      matchesOn (day 2026 7 4) "scheduled:+30d" [Privet]
      matchesOn (day 2026 8 10) "scheduled:-1w" [Privet]
      matchesOn (day 2026 8 3) "scheduled:<=+2d" [Ship, Privet]
      sequence_
        [ assertEqual (T.unpack bare)
            (titlesMatchingIn (onDay (day 2026 7 4)) ("scheduled:" <> todayMeta <> bare)
                              records)
            (titlesMatchingIn (onDay (day 2026 7 4)) ("scheduled:" <> bare) records)
        | bare <- ["+30d", "-1w", "+1m", "-2y", "+0d"] ]
      -- And the bare shift means the bare WORD's own shift, spelling for spelling.
      sequence_
        [ assertEqual (T.unpack bare)
            (titlesMatchingIn (onDay (day 2026 7 4)) ("scheduled:today" <> bare) records)
            (titlesMatchingIn (onDay (day 2026 7 4)) ("scheduled:" <> bare) records)
        | bare <- ["+30d", "-1w", "+1m", "-2y", "+0d"] ]

    -- THE TOKEN'S SIGN IS ITS FIRST CHARACTER and the value's own sign sits
    -- inside the value: `scanQuery' stops at the first, so the two never meet.
  , testCase "the token's sign and the shift's sign are read apart" $ do
      assertEqual "the added token, and the shift it carries"
                  [Term Add (Just "scheduled") "+30d"] (parsed "+scheduled:+30d")
      assertEqual "and the negated one" [Term Neg (Just "scheduled") "-1w"]
                  (parsed "-scheduled:-1w")
      -- A lone added token is the plain one, shift and all.
      matchesOn (day 2026 7 4) "+scheduled:+30d" [Privet]
      matchesOn (day 2026 7 4) "-scheduled:+30d" [Ship, Reply, Plain, Drop, Schema]
      matchesOn (day 2026 8 10) "+scheduled:-1w" [Privet]
      -- The added token joins its own axis, each arm carrying its own shift.
      matchesOn (day 2026 8 2) "scheduled:today-1d +scheduled:today+1d" [Ship, Privet]

  , testCase "with no clock behind it a shifted value names no day" $ do
      matches "scheduled:today+30d" []
      matches "scheduled:tomorrow+30d" []
      matches "scheduled:*today*+30d" []
      matches "scheduled:+30d" []
      matches "scheduled:<=today-1w" []
      matches "planned:today..today+30d" []
      -- It is an ATOM all the same, so its sign inverts into every row.
      every <- matching ""
      matches "-scheduled:today+30d" every
      matches "-scheduled:+30d" every

    -- A BASE NAMING NO DAY leaves the whole value naming none: it matches no
    -- row the way `state:TOD' matches none, and narrows all the same.
  , testCase "a base that is no day matches no row, and narrows all the same" $ do
      every <- matching ""
      mapM_ (`matches` [])
            [ "scheduled:2026-08+1d", "scheduled:banana+1d"
            , "scheduled:\"2026-08-01 09:30+1d\"", "scheduled:*empty*+1d"
            , "scheduled:>2026+1y" ]
      matches "-scheduled:2026-08+1d" every

    -- THE HALF-TYPED FAMILY'S OWN LAW, and the shift joins it: an unsigned or
    -- added token narrows nothing, a negated one empties the table.
  , testCase "a shift with no unit behind it is half-typed" $ do
      every <- matching ""
      mapM_ (`matches` every)
            [ "scheduled:today+", "scheduled:today+30", "scheduled:+"
            , "scheduled:+30", "scheduled:2026-08-01+", "planned:today+7"
            , "scheduled:>=today+", "scheduled:today..today+30"
            , "scheduled:tomorrow+", "scheduled:*today*+" ]
      matches "-scheduled:today+30" []
      matches "-planned:today+" []
      matchesOn (day 2026 8 3) "scheduled:today scheduled:today+" [Privet]

    -- THE PLUS FAMILY ALONE is half-typed: `-' is ISO's own separator, so a
    -- rule reading the incomplete minus would read `2026-08-03' as `2026-08'
    -- moved `03' of no unit.  An incomplete minus stays the literal it was.
  , testCase "an incomplete minus is a literal, never a half-typed shift" $ do
      every <- matching ""
      matches "scheduled:today-" []
      matches "scheduled:today-7" []
      matches "-scheduled:today-7" every
      matches "scheduled:*today*-7" []
      matches "scheduled:2026-08-03" [Privet]
      matches "scheduled:2026-08-0" [Ship, Privet]

    -- CONSERVATIVITY: the oracle is the CELL, read off the record rather than
    -- off the matcher, so every value that composed before answers what it did.
  , testCase "every value that composed before answers byte for byte" $ do
      records <- qrRecords <$> loadDir viewDir
      let shown pick r = T.toLower (displayText (fromMaybe "" (pick r)))
      sequence_
        [ assertEqual (T.unpack (key <> ":" <> v))
            [ hrTitle r | r <- records, v `T.isPrefixOf` shown pick r ]
            (titlesMatchingIn (onDay (day 2026 8 3)) (key <> ":" <> quotedValue v) records)
        | (key, pick) <- [("scheduled", hrScheduled), ("deadline", hrDeadline)]
        -- `today' and `tomorrow' are OUT of this corpus: they are the day
        -- words now and name a day rather than the text they spell, which is
        -- the one reading this rename moved.  Their near-misses stay in.
        , v <- [ "2026", "2026-08", "2026-08-0", "2026-08-01", "2026-08-03"
               , "2026-08-01 09:30", "2026-08-05", "2026-08-10", "2026-08-1"
               , "03", "banana", "todayy", "tod", "monday", "2027", "2026-8-1" ] ]
      -- THE NEW GROUND WAS DEAD: no cell a tree writes carries a `+', so every
      -- shift-shaped value served zero rows before and its negation served all.
      assertBool "an ISO cell carries no plus"
                 (not (any (T.isInfixOf "+" . hrSearch) records))
  ]
  where day = fromGregorian

-- | FREE TEXT UNDER A KEY: @substring:V@ is what @V@ alone means, one matcher for both.
substringSpec :: TestTree
substringSpec = testGroup "Substring"
  [ virtualKeyCase "substring" substringKey "ship"

  , testCase "it finds exactly what the bare word finds" $
      mapM_ (\word -> do
              bare <- matching word
              keyed <- matching (substringKey <> ":" <> word)
              assertEqual ("substring:" <> T.unpack word) bare keyed)
            ["ship", "renderer", "2026-08", "zzz", ""]

  , testCase "and the machinery a predicate has comes with it" $ do
      matches "-substring:ship" [Privet, Reply, Plain, Drop, Schema]
      matches "substring:ship|renderer" [Ship, Drop]
      everything <- matching ""
      assertEqual "a key with no value narrows nothing" everything
        =<< matching "substring:"

  , testCase "a quoted value may spell what a bare word cannot" $
      -- The point of the key: a leading `-' negates a bare word, and is text here.
      assertEqual "the hyphen is text here"
                  [Term Unsigned (Just "substring") "-x"] (parsed "substring:\"-x\"")
  ]

-- | The ORDER token @sort:COL[:desc]@: it narrows nothing; the chain is 'sortChainIn's.
sortSpec :: TestTree
sortSpec = testGroup "Sort tokens" $
  [ virtualKeyCase "sort" sortKey "deadline" ]
  <> viewTokenCases "sort"
       [ "sort:deadline", "sort:deadline:desc", "sort:state sort:title"
       , "sort:state->title", "sort:state->title:desc", "sort:deadline->"
       , "sort:", "sort:nosuchcolumn", "sort:deadline:sideways" ]
  <>
  [ testCase "and never as free text, which is what would narrow" $ do
      -- The letters are in no row, so a token read as text would empty the table.
      matches "sort:title" =<< matching ""
      assertEqual "the token resolved to the key"
                  [Term Unsigned (Just "sort") "title"] (parsed "sort:title")

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
      assertEqual "a trailing colon is no direction at all"
                  (Right [("deadline", True)])
                  (sortChainIn "sort:deadline:")

  , testCase "written order is precedence, and repeats compose" $ do
      assertEqual "two keys" (Right [("state", True), ("deadline", False)])
                  (sortChainIn "sort:state sort:deadline:desc")
      assertEqual "the other way round"
                  (Right [("deadline", False), ("state", True)])
                  (sortChainIn "sort:deadline:desc sort:state")
      assertEqual "with predicates between them"
                  (Right [("state", True), ("title", True)])
                  (sortChainIn "sort:state tag:web sort:title")

  -- @->@ is SUGAR: each case is asserted against the spelling it is sugar FOR.
  , testCase "an arrow chains one token's columns" $
      mapM_ (\(chained, spelled') ->
               assertEqual (T.unpack chained <> " vs " <> T.unpack spelled')
                           (sortChainIn spelled') (sortChainIn chained))
        [ ("sort:state->title",            "sort:state sort:title")
        , ("sort:state->title:desc",       "sort:state sort:title:desc")
        , ("sort:state:desc->title",       "sort:state:desc sort:title")
        , ("sort:state->title->deadline",  "sort:state sort:title sort:deadline")
        , ("sort:state sort:title->deadline",
           "sort:state sort:title sort:deadline")
        , ("sort:state->title tag:web sort:deadline",
           "sort:state sort:title tag:web sort:deadline")
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

  -- `*none*' is a STARRED META: no column is called it and no cell can hold it.
  , testCase "sort:*none* is the empty chain" $ do
      assertEqual "alone" (Right []) (sortChainIn "sort:*none*")
      assertEqual "beside predicates, which narrow as they always do"
                  (Right []) (sortChainIn "state:TODO sort:*none* tag:web")
      assertEqual "and beside the half-typed token"
                  (Right []) (sortChainIn "sort: sort:*none*")

  , testCase "and it admits no companion that orders anything" $
      mapM_ (\q -> refusedNaming (T.unpack q) ["*none*"] (sortChainIn q))
        [ "sort:*none* sort:title"
        , "sort:title sort:*none*"
        , "sort:*none* sort:*none*"
        , "sort:*none*:desc"
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
          -- The negation covers every segment, being written before the key.
        , ("-sort:title->state",      "-sort:title->state")
        , ("sort:title|state->deadline", "sort:title|state->deadline")
        , ("sort:state->nosuchcolumn", "nosuchcolumn")
        , ("sort:nosuchcolumn->state", "nosuchcolumn")
        , ("sort:state->title:sideways", "sort:state->title:sideways") ]

    -- A REPEATED column is no refusal: first-wins dedup is the renderer's rule too.
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

-- | The COLUMN SET half of the view grammar, the sort token's twin: it narrows nothing.
columnsSpec :: TestTree
columnsSpec = testGroup "Columns tokens" $
  [ virtualKeyCase "columns" columnsKey "State,Title" ]
  <> viewTokenCases "columns"
       [ "columns:state", "columns:State,Title,Tags", "columns:"
       , "columns:nosuchcolumn", "columns:a,,b", "-columns:state" ]
  <>
  [ testCase "a query naming no columns token names no set" $ do
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

  -- Resolution: the cell functions are opaque, so the cases read three fields.
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
            -- An ALTERNATIVE names it too: archived rows have been asked for.
            , "tag:*archive*|web", "tag:web|*archive*", "-tag:web|*archive*" ]

  , testCase "and a query that says nothing about it does not" $
      mapM_ (\q -> assertBool (show q <> " read as naming the tag")
                             (not (namesArchive q)))
            -- With tags out of the grammar, `archive:draft' is text like any other.
            [ "", "*archive*", "\"tag:*archive*\"", "archive:", "archive:draft"
            , "state:DONE", "title:*archive*"
            -- THE STARRED SPELLING ALONE: `tag:archive' is the ordinary substring.
            , "tag:archive", "-tag:archive", "tag:arch", "tag:archived" ]
  ]

-- The starred family: this fixture spells both reserved words as ordinary org.

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
metaMatching q = withMetaTree (pure . titlesMatching q)

-- | TWO RULES AGENTS.hs STATES AND NOTHING ASKED, each named by a surviving mutant.
foldSpec :: TestTree
foldSpec = testGroup "The folds and the lone hyphen"
  -- A BARE `-' IS A NEGATED EMPTY TERM, and an empty term matches everything.
  [ testCase "a lone hyphen empties the result set" $ do
      assertEqual "the token it parses to" [(Neg, "")]
                  [ (tmSign t, tmValue t) | t <- parseFilter "-" ]
      assertEqual "and it matches no row" [] =<< matching "-"

  , testCase "where a bare word matches on its text" $
      assertEqual "the token wears no sign" [(Unsigned, "x")]
                  [ (tmSign t, tmValue t) | t <- parseFilter "x" ]

    -- VALUES ARE FOLDED ON BOTH SIDES; the cell side folds at load.
  , testCase "free text is folded, whatever case it is written in" $ do
      lower <- matching "todo"
      upper <- matching "TODO"
      assertEqual "the two spellings answer alike" upper lower
      assertBool "and they answer at all" (not (null lower))

  , testCase "and its own key spells the same rule" $ do
      bare <- matching "TODO"
      keyed <- matching "substring:TODO"
      assertEqual "`substring:VALUE' is what VALUE alone means" bare keyed

    -- A KEY is case-SENSITIVE on both sides, so a shouted one is free text.
  , testCase "a key shouted is no key, and reads as the text it is" $ do
      keyed <- matching "state:TODO"
      shouted <- matching "STATE:TODO"
      assertBool "the key answers" (not (null keyed))
      assertEqual "and the shouted one is free text nothing carries" [] shouted
  ]

metaSpec :: TestTree
metaSpec = testGroup "Starred metas"
  -- THE ROSTER IS THE FAMILY: the type and the constants have to name one set.
  [ testCase "the roster is every starred word the code spells" $ do
      assertEqual "the family, in the order the type declares it"
        [activeMeta, inactiveMeta, emptyMeta, archiveMeta, noOrder, todayMeta, anyMeta]
        (map metaWord metas)
      mapM_ (\m -> assertBool (show m <> " is not a starred word")
                              (maybe False (not . T.null) (metaOf (metaWord m))))
            metas

  , testCase "the empty meta is the empty cell, on every column key" $ do
      assertEqual "the spelling" "*empty*" emptyMeta
      assertEqual "state" ["Nothing stated"] =<< metaMatching "state:*empty*"
      assertEqual "tag" ["Nothing stated"] =<< metaMatching "tag:*empty*"
      assertEqual "priority" 4 . length =<< metaMatching "priority:*empty*"
      assertEqual "scheduled" 4 . length =<< metaMatching "scheduled:*empty*"
      assertEqual "deadline" 4 . length =<< metaMatching "deadline:*empty*"
      assertEqual "title" [] =<< metaMatching "title:*empty*"
      assertEqual "planned" 4 . length =<< metaMatching "planned:*empty*"

  , testCase "and the bare word it replaced is a value like any other" $ do
      -- The shadow this cost: `none' was every key's word for the empty cell.
      assertEqual "a state spelled NONE"
                  ["Filed away", "Not filed"] =<< metaMatching "state:none"
      assertEqual "a tag spelled none"
                  ["A state spelled like the tag"] =<< metaMatching "tag:none"

  , testCase "a starred word on the tags column is the whole tag" $ do
      assertEqual "the tag itself" ["Filed away"] =<< metaMatching "tag:*archive*"
      assertEqual "the substring beside it"
                  ["Filed away", "Not filed"] =<< metaMatching "tag:archive"
      assertEqual "negated, the near miss survives"
                  ["Not filed", "A state spelled like the tag", "Nothing stated"]
        =<< metaMatching "-tag:*archive*"

  , testCase "and two of them AND, the way two tokens do" $ do
      assertEqual "both tags" ["Filed away"] =<< metaMatching "tag:*web* tag:*archive*"
      assertEqual "one it lacks" [] =<< metaMatching "tag:*none* tag:*archive*"
      assertEqual "either tag" ["Filed away", "A state spelled like the tag"]
        =<< metaMatching "tag:*none*|*archive*"

  , testCase "what a meta IS: one matched pair with a word inside it" $
      -- The rule every branch above reads, spelled once ('metaOf').
      mapM_ (\(value, want) -> assertEqual (T.unpack value) want (metaOf value))
            [ ("*empty*", Just "empty"), ("*archive*", Just "archive")
            , ("empty", Nothing), ("*empty", Nothing), ("empty*", Nothing)
            , ("**", Nothing), ("*", Nothing), ("", Nothing) ]

  , testCase "a starred word anywhere else is the literal it spells" $ do
      -- The tags column is the only multi-valued one, so nothing else reads a star.
      assertEqual "a keyword" [] =<< metaMatching "state:*NONE*"
      assertEqual "a title word" [] =<< metaMatching "title:*filed*"
      -- THE ANCHOR'S OWN WORD IS THE ANCHOR'S: on a column it is a literal.
      assertEqual "the anchor on a state" [] =<< metaMatching "state:*any*"
      assertEqual "and on a tag" [] =<< metaMatching "tag:*any*"
  ]


-- | An org tag names no filter key; @tag:course text@ is the facet-then-search.
tagsSpec :: TestTree
tagsSpec = testGroup "Tags are not keys"
  [ testCase "a tag key is free text, colon and all" $ do
      assertEqual "a tag of the tree" [Term Unsigned Nothing "web:ship"]
                  (parsed "web:ship")
      assertEqual "and one it does not carry" [Term Unsigned Nothing "contact:tanik"]
                                              (parsed "contact:tanik")
      matches "web:schema" []
      matches "web:ship" []
      matches "contact:tanik" []

  , testCase "and matched as the text it is, org's own colons included" $
      -- org spells a tags cell `:web:', so the free text `web:' is inside it.
      matches "web:" [Ship, Schema]

  , testCase "tag: is the one spelling, and it is the column's" $ do
      matches "tag:web" [Ship, Schema]
      matches "tag:glance" [Ship]
      matches "tag:unicode" [Privet]
      matches "tag:*empty*" [Reply, Plain]

  , testCase "the facet and the search are two tokens now" $ do
      matches "tag:web schema" [Schema]
      matches "tag:web ship" [Ship]
      matches "tag:web 2026-08-01" [Ship]
      matches "tag:cleanup renderer" [Drop]
      matches "tag:web renderer" []

  , testCase "which costs the whole-tag reading, the column being a substring" $ do
      -- `tag:glan' is a substring of the cell, the tag column's own reading.
      matches "glan:" []
      matches "tag:glan" [Ship]

    -- The CELL sorts and the FILE does not, and no query can depend on which.
  , testCase "a predicate is order-independent, whichever way the row spells it" $ do
      matches "tag:web" [Ship, Schema]
      matches "tag:glance" [Ship]
      matches "tag:web tag:glance" [Ship]
      matches "tag:glance tag:web" [Ship]
      matches "tag:*empty*" [Reply, Plain]
      -- The free-text half sees the cell as the table draws it: sorted.
      matches "glance:web" [Ship]
      matches "web:glance" []

  , testCase "the tags a tree carries change no answer at all" $ do
      -- The environment a query is matched in is the rows alone: no tag list.
      records <- qrRecords <$> loadDir viewDir
      assertEqual "the fixture's tags" ["cleanup", "glance", "unicode", "web"]
                  (vocabularyOf records)
      assertEqual "no tag is a key"
        [ [Term Unsigned Nothing (t <> ":x")] | t <- vocabularyOf records ]
        [ parsed (t <> ":x") | t <- vocabularyOf records ]

  , testCase "and a column can no longer be shadowed by one" $ do
      -- A file tagged `:title:' could once have taken the column's key away.
      assertEqual "the column" [Term Unsigned (Just "title") "x"] (parsed "title:x")
      assertEqual "the tag, as text" [Term Unsigned Nothing "glance:x"] (parsed "glance:x")
  ]


-- | What the scanner cuts out of a query, and what the tokens resolve to.
tokenSpec :: TestTree
tokenSpec = testGroup "Tokens"
  [ testCase "a bare word is free text" $
      assertEqual "tokens" [Token Unsigned False "tanik"] (scanQuery "tanik")

  , testCase "tokens separate on whitespace and on &" $
      assertEqual "tokens"
        [ Token Unsigned False "a", Token Unsigned False "b"
        , Token Unsigned False "c" ]
        (scanQuery "a b&c")

  , testCase "runs of separators collapse, and the ends are trimmed" $
      assertEqual "tokens" [Token Unsigned False "a", Token Unsigned False "b"]
        (scanQuery "  a \t&& b\n")

  , testCase "an empty query has no tokens" $ do
      assertEqual "empty" [] (scanQuery "")
      assertEqual "blank" [] (scanQuery "  & ")

  , testCase "a quoted token keeps its spaces and drops its quotes" $
      assertEqual "tokens" [Token Unsigned True "the table"] (scanQuery "\"the table\"")

  , testCase "an unclosed quote runs to the end, so typing one loses nothing" $
      assertEqual "tokens" [Token Unsigned True "the tab"] (scanQuery "\"the tab")

  , testCase "a leading - negates, and a - inside a word does not" $ do
      assertEqual "negated" [Token Neg False "web"] (scanQuery "-web")
      assertEqual "hyphenated" [Token Unsigned False "no-such-row"]
                  (scanQuery "no-such-row")
      assertEqual "negated quote" [Token Neg True "the table"]
                  (scanQuery "-\"the table\"")

  , testCase "org tag text is not a predicate" $ do
      assertEqual ":work: stays text" [Term Unsigned Nothing ":work:"] (parsed ":work:")
      assertEqual "=code= stays text" [Term Unsigned Nothing "=code="] (parsed "=code=")

  , testCase "key:value is a predicate only for a column of the view" $ do
      assertEqual "a column" [Term Unsigned (Just "state") "TODO"] (parsed "state:TODO")
      assertEqual "not a column" [Term Unsigned Nothing "note:later"] (parsed "note:later")
      assertEqual "a URL is text" [Term Unsigned Nothing "http://example.org"]
                                  (parsed "http://example.org")

  , testCase "= is an alias for :" $
      assertEqual "term" [Term Unsigned (Just "state") "*active*"]
                  (parsed "state=*active*")

  , testCase "the first separator splits, so a value may carry more" $
      assertEqual "term" [Term Unsigned (Just "title") "a:b"] (parsed "title:a:b")

  , testCase "a token that opens with a quote is free text, predicate or not" $
      assertEqual "term" [Term Unsigned Nothing "state:TODO"] (parsed "\"state:TODO\"")

  , testCase "a predicate's value may be quoted" $
      assertEqual "term" [Term Unsigned (Just "tag") "two words"]
                  (parsed "tag:\"two words\"")

  , testCase "negation carries the whole token, either form" $
      assertEqual "terms" [ Term Neg (Just "state") "DONE"
                          , Term Neg Nothing "web" ]
                  (parsed "-state:DONE -web")

  , testCase "a leading + adds, and a + inside a word does not" $ do
      assertEqual "added" [Token Add False "web"] (scanQuery "+web")
      assertEqual "free text" [Term Add Nothing "web"] (parsed "+web")
      assertEqual "plussed" [Token Unsigned False "a+b"] (scanQuery "a+b")
      assertEqual "added quote" [Token Add True "the table"]
                  (scanQuery "+\"the table\"")

  , testCase "a + carries the whole token, predicate or free text" $ do
      assertEqual "a predicate" [Term Add (Just "state") "DONE"]
                  (parsed "+state:DONE")
      assertEqual "and a lone + is an added empty term"
                  [Term Add Nothing ""] (parsed "+")

    -- TWO SIGNS FAIL THE SHAPE: `seen' guards both branches, so the second one
    -- lands in the body and the resolver's usual fallthrough reads free text.
  , testCase "a second sign is body text, and the first one stands" $ do
      assertEqual "+- scans" [Token Add False "-x"] (scanQuery "+-x")
      assertEqual "+- resolves" [Term Add Nothing "-x"] (parsed "+-x")
      assertEqual "-+ scans" [Token Neg False "+x"] (scanQuery "-+x")
      assertEqual "-+ resolves" [Term Neg Nothing "+x"] (parsed "-+x")

  , testCase "a quoted value may spell what a bare added word cannot" $
      assertEqual "the hyphen is text here"
                  [Term Add (Just "substring") "-x"]
                  (parsed "+substring:\"-x\"")

  , testCase "a quote swallows the +, so the token is free text sign and all" $
      assertEqual "term" [Term Unsigned Nothing "+state:x"]
                  (parsed "\"+state:x\"")

  , testCase "a key nobody resolves keeps the +, its body reading as text" $
      assertEqual "term" [Term Add Nothing "STATE:x"]
                  (parsed "+STATE:x")

  , testCase "the keys are the view's own column keys" $ do
      view <- viewJSON "t" . qrRecords <$> loadDir viewDir
      keys <- columnKeysOf view
      assertEqual "columns" keys filterKeys
  ]

-- | Q parsed: only a column, @planned@ or @ref@ makes a predicate.
parsed :: Text -> [Term]
parsed = parseFilter

-- | A key the GRAMMAR owns rather than a column: SPELLING is what KEY holds, no
-- column carries it, and @KEY:VALUE@ resolves to it wherever it is written.
virtualKeyCase :: Text -> Text -> Text -> TestTree
virtualKeyCase spelling key value =
  testCase "the key is spelled once, and it is not a column" $ do
    assertEqual "the key" spelling key
    assertBool "and no column carries it" (key `notElem` filterKeys)
    assertEqual "a token names it like any other key"
                [Term Unsigned (Just key) value] (parsed (key <> ":" <> value))

-- | The three laws a VIEW token obeys, one group's worth: every one of TOKENS narrows nothing, a predicate beside @KEY:title@ does all the narrowing, and quoted it is free text like any word.
viewTokenCases :: Text -> [Text] -> [TestTree]
viewTokenCases key tokens =
  [ testCase "it narrows nothing, whatever it names" $ do
      every <- matching ""
      mapM_ (`matches` every) tokens

  , testCase "beside a predicate it leaves the narrowing to it" $ do
      matches ("state:*inactive* " <> token) [Drop, Schema]
      matches (token <> " state:*inactive*") [Drop, Schema]

  , testCase "a quoted token is free text, here as everywhere" $ do
      matches quoted' []
      assertEqual "free text" [Term Unsigned Nothing token] (parsed quoted')
  ]
  where token  = key <> ":title"
        quoted' = "\"" <> token <> "\""


-- | One group per column type SCHEMA.md names, plus this producer's metas.
predicateSpec :: TestTree
predicateSpec = testGroup "Predicates"
  [ testCase "state is a whole value, case-insensitively" $ do
      matches "state:TODO" [Privet]
      matches "state:todo" [Privet]
      matches "state:DONE" [Schema]
      matches "state:TOD" []
      -- The whole-value arm folds org's priority decoration, as the renderer does.
      matches "state:[#TODO]" [Privet]

  , testCase "the two group metas are the file's keyword groups" $ do
      -- #+TODO: NEXT WAITING | CANCELLED, over the seeded TODO/DONE.
      matches "state:*active*" [Ship, Privet, Reply, Plain]
      matches "state:*inactive*" [Drop, Schema]
      matches "state:active" []
      matches "state:inactive" []

  , testCase "the stateless row is active, and it is not inactive" $ do
      -- No scope classifies a keywordless headline, and `*active*' takes it anyway.
      matches "state:*empty*" [Plain]
      matches "state:*active*" [Ship, Privet, Reply, Plain]
      matches "state:*inactive*" [Drop, Schema]
      -- Which makes `*empty*' a subset of `*active*' and no third group.
      matches "-state:*active*" [Drop, Schema]
      matches "-state:*inactive*" [Ship, Privet, Reply, Plain]

  , testCase "a meta is folded like every value, and is no glob" $ do
      matches "state:*ACTIVE*" [Ship, Privet, Reply, Plain]
      matches "state:*TODO*" []
      matches "state:*none*" []
      matches "state:*active" []
      matches "state:active*" []

    -- The CELL wears org's `[#A]' and the match reads THROUGH the brackets.
  , testCase "priority is the letter, case-insensitively and through the brackets" $ do
      matches "priority:A" [Ship]
      matches "priority:a" [Ship]
      matches "priority:c" [Drop]
      matches "priority:[#A]" [Ship]
      matches "priority:[#a]" [Ship]
      -- Still EXACT past the fold: a letter is one character.
      matches "priority:[#" []
      matches "priority:AB" []
      matches "priority:*empty*" [Reply, Plain, Schema]

  , testCase "title and tag are substrings of the cell, case-insensitively" $ do
      matches "title:the" [Ship, Reply, Drop, Schema]
      matches "title:SHIP" [Ship]
      matches "title:привет" [Privet]
      matches "tag:web" [Ship, Schema]
      matches "tag:*empty*" [Reply, Plain]
      matches "tag:renderer" []

  , testCase "a title predicate sees the cell as it displays" $ do
      matches "title:schema" [Schema]
      matches "title:2026" []

  , testCase "dates match by prefix, so a month is a month" $ do
      matches "scheduled:2026-08" [Ship, Privet]
      matches "scheduled:2026-08-03" [Privet]
      matches "deadline:2026" [Ship, Reply]
      matches "scheduled:*empty*" [Reply, Plain, Drop, Schema]
      matches "scheduled:03" []

  , testCase "a value with nothing typed narrows nothing" $ do
      every <- matching ""
      matches "state:" every
      matches "scheduled:" every
      matches "state: title:the" [Ship, Reply, Drop, Schema]

  , testCase "a negated predicate fails the row it matches" $ do
      matches "-state:DONE" [Ship, Privet, Reply, Plain, Drop]
      matches "-state:*empty*" [Ship, Privet, Reply, Drop, Schema]
      matches "-priority:*empty*" [Ship, Privet, Drop]
  ]


-- | One combination rule: TOKENS AND, ALTERNATIVES OR.  Every token narrows.
shapeSpec :: TestTree
shapeSpec = testGroup "Shape"
  [ testCase "every token narrows, whether or not another names its key" $ do
      matches "state:TODO" [Privet]
      matches "state:DONE" [Schema]
      -- A cell holding ONE value cannot hold two; the either is `state:TODO|DONE'.
      matches "state:TODO state:DONE" []
      matches "state:TODO state:DONE state:NEXT" []
      matches "priority:a priority:c" []
      matches "state:TODO state:TODO" [Privet]

  , testCase "and a cell holding a list can meet all of them" $ do
      -- The tags cell is a list, so two tokens intersect where two states cannot.
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
      -- Each token is its own substring of the row, so words out of order match.
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


-- | @key:A|B@ — the OR half: a VALUE splits on @|@, uniformly over every key.
alternationSpec :: TestTree
alternationSpec = testGroup "Alternation"
  [ testCase "what a value splits into, and what an empty alternative costs" $
      mapM_ (\(value, want) -> assertEqual (T.unpack value) want (alternatives value))
            [ ("a|b", ["a", "b"]), ("a", ["a"])
            , ("a|", ["a"]), ("|a", ["a"]), ("a||b", ["a", "b"])
            , ("", []), ("|", []), ("||", []) ]

  , testCase "each alternative is read as that key's own value" $ do
      matches "state:TODO|DONE" [Privet, Schema]
      matches "priority:A|C" [Ship, Drop]
      matches "title:ship|schema" [Ship, Schema]
      matches "tag:glance|unicode" [Ship, Privet]
      matches "scheduled:2026-08-01|2026-08-03" [Ship, Privet]
      matches "planned:2026-08-03|2026-08-10" [Privet, Reply]
      -- The semantics stay the key's: a badge is no substring.
      matches "state:TOD|DON" []

  , testCase "a meta joins the alternatives like any other value" $ do
      matches "state:*empty*|DONE" [Plain, Schema]
      matches "state:*active*|DONE" [Ship, Privet, Reply, Plain, Schema]
      matches "tag:*empty*|glance" [Ship, Reply, Plain]

  , testCase "a negation covers the whole token" $ do
      matches "-state:TODO|DONE" [Ship, Reply, Plain, Drop]
      -- De Morgan makes the two negations one answer, which is what is pinned.
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
      matches "title:ship|schema" [Ship, Schema]
      matches "ship|schema" []
      matches "\"ship|schema\"" []
  ]


-- | @+key:value@ — the token that joins its key's OWN axis as an alternative:
-- within one axis the plain tokens AND as they always did and the added ones OR
-- against that conjunction, and the axes still AND with each other.
addedSpec :: TestTree
addedSpec = testGroup "Added tokens"
  [ testCase "a lone added token is the plain one it spells" $ do
      web <- matching "tag:web"
      assertEqual "the axis holds nothing else, so it is the atom alone"
                  web =<< matching "+tag:web"
      matches "+priority:[#B]" [Privet]

  , testCase "beside a plain token it widens that axis and leaves the rest" $ do
      -- The ask itself: the A rows and the B rows, every other filter standing.
      matches "priority:A +priority:B" [Ship, Privet]
      matches "tag:web priority:A +priority:B" [Ship]
      matches "tag:unicode priority:A +priority:B" [Privet]
      matches "title:the priority:A +priority:C" [Ship, Drop]

  , testCase "grouping is by key, so the tokens answer alike in any order" $
      mapM_ (\q -> assertEqual (T.unpack q) [Ship] =<< matching q)
            [ "priority:A tag:web +priority:B"
            , "priority:A +priority:B tag:web"
            , "+priority:B tag:web priority:A" ]

  , testCase "two plain tokens conjoin under the added one" $ do
      -- (web AND glance) OR unicode, which neither half alone answers.
      matches "tag:web tag:glance" [Ship]
      matches "tag:unicode" [Privet]
      matches "tag:web tag:glance +tag:unicode" [Ship, Privet]

  , testCase "a negation stays inside the conjunction half" $ do
      -- `-k:v +k:w' is "not v, or w", and the tautology serves every row.
      matches "-state:*active* +state:TODO" [Privet, Drop, Schema]
      every <- matching ""
      matches "-state:TODO +state:TODO" every

  , testCase "an added token repeated changes nothing" $ do
      matches "priority:A +priority:B +priority:B" [Ship, Privet]
      matches "+tag:web +tag:web" [Ship, Schema]

  , testCase "on a bare axis an added token spells the alternation" $ do
      alt <- matching "state:TODO|DONE"
      assertEqual "k:v1|v2 and k:v1 +k:v2 answer alike" alt
        =<< matching "state:TODO +state:DONE"

  , testCase "and beside another plain token the two part" $
      -- `u AND (v1 OR v2)', where the added form is `(u AND v1) OR v2' — the
      -- conjoin case above pins that other half.
      matches "tag:web tag:glance|unicode" [Ship]

  , testCase "alternatives ride along, and so do the metas" $ do
      matches "+state:DONE|CANCELLED" [Drop, Schema]
      matches "state:TODO +state:DONE|CANCELLED" [Privet, Drop, Schema]
      matches "state:TODO +state:*inactive*" [Privet, Drop, Schema]
      matches "priority:A +priority:*empty*" [Ship, Reply, Plain, Schema]

  , testCase "free text and substring share one axis, so two words are either" $ do
      matches "ship schema" []
      matches "ship +schema" [Ship, Schema]
      matches "+substring:ship +substring:schema" [Ship, Schema]

  , testCase "the virtual keys take it the way the columns do" $ do
      matches "planned:2026-08-01 +planned:2026-08-10" [Ship, Reply]
      matches "planned:*empty* +planned:2026-08-03" [Privet, Plain, Drop, Schema]
      withRefTree $ \records -> do
        rid <- idOf "Second" records
        let hit q = titlesMatching q records
        assertEqual "either target" ["By id", "By title"]
                    (hit ("ref:alpha +ref:" <> rid))
        assertEqual "which is the alternation's own answer" ["By id", "By title"]
                    (hit ("ref:alpha|" <> rid))

    -- A TOKEN NAMING NO ATOM ADDS NOTHING AND ESTABLISHES NO AXIS: taken as an
    -- axis of its own it would empty the table, and a half-typed token never
    -- does that.  The lone hyphen keeps its own law and still empties the
    -- table, which `foldSpec' pins as the other half of the asymmetry.
  , testCase "a token with nothing typed narrows nothing, added or not" $ do
      every <- matching ""
      matches "state:TODO +state:" [Privet]
      matches "tag:web +state:" [Ship, Schema]
      matches "+state:" every
      matches "+state:|" every
      matches "+" every
      -- THE FLOOD the drop prevents: left standing, `state:' is a match-all in
      -- the conjunction half and every row rides out on the added token's OR.
      matches "state: +state:DONE" [Schema]

  , testCase "an added tag token names the archive like any other spelling" $
      assertBool "+tag:*archive* did not read as naming the tag"
                 (namesArchive "+tag:*archive*")

  , testCase "the three view keys refuse a + the way they refuse a -" $ do
      refusedNaming "sort" ["added", "+sort:title"] (sortChainIn "+sort:title")
      refusedNaming "columns" ["added", "+columns:State"]
                    (columnNamesIn "+columns:State")
      refusedNaming "view" ["added", "+view:agenda"] (viewAddedIn "+view:agenda")

  , testCase "and a query that adds no view token is left as it stands" $ do
      assertEqual "plain" (Right ()) (viewAddedIn "view:agenda")
      -- `-view:NAME' is dropped rather than refused, and stays dropped.
      assertEqual "negated" (Right ()) (viewAddedIn "-view:agenda")
      assertEqual "empty" (Right ()) (viewAddedIn "")
      assertEqual "another key's +" (Right ()) (viewAddedIn "+state:DONE")
  ]


-- | With no predicate in it a query is the substring search it always was.
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
      let hit q = titlesMatching q records
          q = "note:later"
      assertEqual "no row carries it" [] (hit q)
      assertBool "and it is not read as a predicate"
                 (all ((== Nothing) . tmKey) (parseFilter q))
      -- THE POSITIVE HALF: `09:30' is key `09' — no key — over text Ship holds.
      assertBool ("an unknown key over text a row carries finds it: "
                    <> show (hit "09:30"))
                 (not (null (hit "09:30")))
      assertEqual "the whole token is the needle, colon included"
                  (hit "\"09:30\"") (hit "09:30")
      -- The KEY is part of the needle; searching the value alone answers most rows.
      assertEqual "the value alone is not what was searched for" [] (hit "nosuchkey:e")
      assertBool "though the value alone matches plenty" (length (hit "e") >= 3)
  ]


-- | A predicate reads one field of the search text, so field order is column order.
layoutSpec :: TestTree
layoutSpec = testGroup "Search text layout"
  [ -- The hand-written oracle: six cells named here, in the order this suite
    -- says they go in — an INDEPENDENT oracle, the layout's only underived copy.
    testCase "field i of the search text is column i as it displays" $ do
      records <- qrRecords <$> loadDir viewDir
      mapM_ (\r -> mapM_ (check r) (zip [0 ..] (cellsOf r))) records

  , testCase "a cell past the last column is empty rather than the last one" $ do
      records <- qrRecords <$> loadDir viewDir
      mapM_ (assertEqual "field 6" "" . cellAt (length filterKeys) . hrSearch) records

    -- THE APPEND: quantified over 'filterKeys', and the values come off the WIRE.
  , testCase "every column is reachable by the key it declares" $ do
      records <- qrRecords <$> loadDir viewDir
      sequence_ [ reachable records r key | r <- records, key <- filterKeys ]
  ]
  where
    -- Sorted HERE by hand, so this list stays an independent oracle.
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
