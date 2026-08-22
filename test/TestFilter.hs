-- | The filter query language: the tokenizer as a pure function, and the
-- semantics over a loaded fixture.  The grammar is @table-view\/SCHEMA.md@'s.
module TestFilter (spec) where

import Control.Monad (unless)
import Data.List (nub, sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import TestDefaults (columnKeysOf, field, maybeTextAt, refusedNaming, viewDir, withDocDir)

import qualified Data.Text as T

import Glance.Query ( HeadlineRecord (..), QueryResult (qrRecords), defaultSortChain
                    , activeMeta, displayText, inactiveMeta, metaWord, metas
                    , Ref (..), RefVia (..), loadDir, matchesSearch, refTargetOf, refTargets
                    , resolveColumns
                    , rowJSON
                    , tagsOfCell, viewJSON )
import Glance.Web.Columns (columnNamesIn)
import Glance.Web.Filter ( FilterEnv, Sign (..), Term (..), Token (..), alternatives, archiveKey
                         , archiveMeta, cellAt, columnsKey, emptyEnv, emptyMeta, filterKeys
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
  , comparisonSpec, rangeSpec, todaySpec
  , archiveSpec, metaSpec, foldSpec
  , shapeSpec, alternationSpec, addedSpec
  , degenerateSpec
  , targetSpec, refSpec
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
      assertEqual "the keys, spelled rather than derived"
                  [ "deadline", "planned", "priority", "ref", "scheduled"
                  , "sort", "state", "substring", "tag", "title" ]
                  (sort (filterKeys <> grammarKeys))
      -- And every one of them resolves to itself, which is what makes it a key.
      assertEqual "the keys are exactly the columns plus the ones the grammar owns"
                  (sort (filterKeys <> grammarKeys))
                  (sort [ k | k <- filterKeys <> grammarKeys
                            , Term _sign (Just k') _v <- parsed (k <> ":x"), k' == k ])
  ]
  where grammarKeys = [plannedKey, refKey, sortKey, substringKey]

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

-- | @*today*@ — the starred family's DATE VALUE, resolved off the env's day and
-- never off the wall clock, so every case here answers the same in a year.
todaySpec :: TestTree
todaySpec = testGroup "The *today* value"
  [ testCase "bare, it is the prefix reading of the request's own day" $ do
      matchesOn (day 2026 8 1) "scheduled:*today*" [Ship]
      matchesOn (day 2026 8 3) "scheduled:*today*" [Privet]
      matchesOn (day 2026 8 5) "deadline:*today*" [Ship]
      matchesOn (day 2026 8 10) "deadline:*today*" [Reply]
      matchesOn (day 2026 8 2) "planned:*today*" []
      matchesOn (day 2026 8 3) "planned:*today*" [Privet]
      -- Folded like every other value, and no glob: the whole starred word.
      matchesOn (day 2026 8 3) "scheduled:*TODAY*" [Privet]
      matchesOn (day 2026 8 3) "scheduled:today" []
      matchesOn (day 2026 8 3) "scheduled:*today" []

  , testCase "behind an operator, and at either end of a range" $ do
      matchesOn (day 2026 8 3) "scheduled:<*today*" [Ship]
      matchesOn (day 2026 8 3) "scheduled:>=*today*" [Privet]
      matchesOn (day 2026 8 3) "scheduled:<=*today*" [Ship, Privet]
      matchesOn (day 2026 8 1) "scheduled:>*today*" [Privet]
      matchesOn (day 2026 8 1) "scheduled:*today*..2026-08-03" [Ship, Privet]
      matchesOn (day 2026 8 3) "scheduled:2026-08-01..*today*" [Ship, Privet]
      matchesOn (day 2026 8 3) "planned:*today*..*today*" [Privet]
      -- The agenda case: everything planned up to and including today.
      matchesOn (day 2026 8 5) "-planned:*empty* planned:<=*today*" [Ship, Privet]

  , testCase "with no clock behind it the word names no day" $ do
      matches "scheduled:*today*" []
      matches "scheduled:>=*today*" []
      matches "planned:*today*..2026-12-31" []
      -- It is an ATOM all the same, so its sign inverts into every row.
      every <- matching ""
      matches "-scheduled:*today*" every
      assertEqual "the word the family spells" "*today*" todayMeta
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
        [activeMeta, inactiveMeta, emptyMeta, archiveMeta, noOrder, todayMeta]
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
