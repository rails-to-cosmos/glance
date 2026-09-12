# Proposal — `ref:` and `from:` are one field carrying a direction

**Status:** proposed · **Date:** 2026-09-12 · **Origin:** `/generalizer`, the
variant-cost sweep over the filter language after the edge index landed.

## Pattern

One edge relation is read from two ends, and every layer between the store and
the predicate spells the two ends twice.

**Four closure fields where one index would do.** `FilterEnv`
(`src-web/Glance/Web/Filter.hs:324-330`) carries

```haskell
  { feSenders :: Maybe Text -> Text -> Set.Set Text            -- ref:ANCHOR
  , feTargets :: Maybe Text -> Text -> Set.Set Text            -- from:ANCHOR
  , feRefAny  :: Maybe Text -> HeadlineRecord -> Bool          -- ref:*any*
  , feFromAny :: Maybe Text -> HeadlineRecord -> Bool          -- from:*any*
  , feToday   :: Maybe Day
  }
```

and all four are one-liners over a single `EdgeIndex` in `storeEnv`
(`Filter.hs:348-355`), each differing only in `edgesInto` vs `edgesOutOf` and
`edFrom` vs `edTo`. `emptyEnv` (`Filter.hs:335-338`) then spells four
do-nothings: `FilterEnv noIds noIds noEdge noEdge Nothing`.

**Paired equations down the whole file.** `data Field = Col !Int | Planned |
Ref | From | Order | Whole` (`Filter.hs:376`), and after it every total case
carries the pair twice:

- `fieldOf` `:380-381` — `key == refKey` / `key == fromKey`
- `fieldCells` `:389-390` — both `[]`
- `narrows` `:400-401` — both `True`
- `valueFor` `:463-464` — both `tmValue`
- `keyTest` `:494-495` — the two `edgeTest` calls

**The pairing nothing checks.** `edgeTest` (`Filter.hs:507-515`) takes the two
accessors as separate arguments:

```haskell
edgeTest :: FilterEnv
         -> (FilterEnv -> Maybe Text -> Text -> Set.Set Text)
         -> (FilterEnv -> Maybe Text -> HeadlineRecord -> Bool)
         -> Text -> HeadlineRecord -> Bool
```

so `edgeTest env feSenders feFromAny value` typechecks. It would answer
`ref:ID` off the senders index and `ref:*any*` off the reverse one — a query
whose anchored form and starred form read opposite directions, with no compiler
complaint and nothing red. The docstring at `:492-493` already says what the
type does not enforce: *"THE TWO ENDS OF ONE EDGE … through ONE reader, so no
wall of theirs can come apart between them."*

A third direction is not on the horizon, so the payoff here is the wrong-pairing
class of defect and the ~30 lines the pairs cost, rather than a variant count.

## Files

`src-web/Glance/Web/Filter.hs:324-355`, `:376-401`, `:463-464`, `:492-515`;
`src-query/Glance/Query.hs` (`EdgeIndex`, `edgesInto`/`edgesOutOf`,
`edFrom`/`edTo`); `AGENTS.hs:2285-2317` (the model's `FRef`/`FFrom`);
`test/TestFilter.hs`.

## Proposed change

**The direction becomes a value the field carries.**

```haskell
-- | WHICH END of the one edge relation a reference key reads.  @Senders@ is
-- @ref:@ (the rows pointing AT the anchor), @Targets@ is @from:@ (the rows it
-- points at).
data Dir = Senders | Targets deriving (Eq, Show, Enum, Bounded)

data Field = Col !Int | Planned | Ref !Dir | Order | Whole deriving Eq
```

**The env carries the index itself.**

```haskell
data FilterEnv = FilterEnv
  { feEdges :: EdgeIndex      -- ^ the store's one resolved relation; the empty index where no store was reached.
  , feToday :: Maybe Day
  }

-- | No store and no clock: an index with no rows, which serves NO ROW in either
-- direction and under either anchor — what a locally-filtered path answers.
emptyEnv :: FilterEnv
emptyEnv = FilterEnv (edgeIndex []) Nothing

storeEnv :: EdgeIndex -> FilterEnv
storeEnv ix = FilterEnv ix Nothing
```

`edgeIndex []` is three empty maps, and `edgesOutOf`/`edgesInto` already answer
`Set.empty` off a missing key, so the four do-nothing closures `emptyEnv` spells
today become a value. The lazy binding the current docstring buys
(`Filter.hs:340-347` — the index is forced at most once per request, never at
all for a query naming no reference key) survives: the field is non-strict, so
`storeEnv ix` still stores the thunk the caller handed it.

**One reader, the direction its argument.**

```haskell
-- | One reference atom, DIR saying which end of the relation it reads.  @*any*@
-- is the union over the slot; every other anchor is a row id, and one no row
-- claims matches nothing.  COMPILED ONCE PER PREDICATE.
edgeTest :: FilterEnv -> Dir -> Text -> HeadlineRecord -> Bool
edgeTest env dir value
  | anchor == anyMeta = \r -> any keeps (starred dir ix (hrId r))
  | otherwise         = \r -> Set.member (hrId r) ids
  where
    (anchor, kind) = anchorIn value
    ix    = feEdges env
    keeps = carriesKind kind . edKind
    ids   = Set.map (endOf dir) (Set.filter keeps (anchored dir ix anchor))
    -- THE TWO ENDS, named once each: a crossed pair is now unspellable.
    anchored Senders = edgesInto  ; anchored Targets = edgesOutOf
    starred  Senders = edgesOutOf ; starred  Targets = edgesInto
    endOf    Senders = edFrom     ; endOf    Targets = edTo
```

The anchored set, the starred test and the kind filter are each derived once, and the direction cannot be crossed because there is one
`dir` in scope. `keyTest` (`:494-495`) becomes one equation:
`keyTest env _key (Ref dir) value = edgeTest env dir value`. The self-edge cut
and the "a link naming no row is no edge" cut stay where they are, at
`resolvedEdges` (`docs/invariants.md`, *"one edge relation, resolved once per
store version"*).

**The two-axis law survives.** `Filter.hs:371-373` says *"`ref` and `from`
STAND AS TWO: one edge read from its two ends, never one predicate, so every
axis law reads them exactly as it reads `tag` beside `state`"* — and
`AGENTS.hs:2422` (`readsAs`) groups tokens by `Field` equality. `Ref Senders /=
Ref Targets` under the derived `Eq`, so the grouping is unchanged: two `ref:`
tokens still AND within one axis and `ref:A from:B` still crosses two.
`AGENTS.hs:2285`'s model keeps `FRef` and `FFrom` as the spelling of those two
axes; nothing there needs to move.

## LOC estimate

Added ~25 (`Dir`, the rewritten `edgeTest`, the direction-keyed helpers).
Removed now ~40 (three `FilterEnv` fields and their docstrings, `storeEnv`'s
four bodies, `emptyEnv`'s two helpers, five paired equations collapsing to one
each, `edgeTest`'s two accessor parameters). **Per future edge-shaped key
(`refkind:`, a transitive `reaches:`): one `Dir`-like value and one equation
instead of two closure fields, two `storeEnv` lines and five paired equations —
roughly 14 lines down to 3.**

## Risk

- **API**: `Filter.hs` internals only. `FilterEnv` is built by `storeEnv`,
  `emptyEnv` and `onDay` (`:365`); every construction site is in this module.
  `Field` is not exported for construction.
- **Wire**: none. `refKey`/`fromKey` are the same two query words, the same
  refusals, the same answers.
- **On-disk**: none.
- **Test baselines**: `TestFilter.hs`'s reference group and `TestQuery.hs`'s
  *"the reverse index answers ref: over every namespace"* assert answers, not
  shapes, so they stand. `TestServe.hs`'s edge cases likewise. The one thing to
  land first is a case that the anchored and starred forms of ONE key read the
  same direction — the defect the current signature permits and nothing covers.

## Existing precedent

- `Asked` / `Asks` (`Commands.hs:113-122`): the door resolves a VALUE and puts
  it on the record, rather than handing the row a closure to call. The same move
  one layer up.
- `LinkPlace` (`Query.hs:1770-1782`) and `CommandKind` (`Commands.hs:131-138`):
  a closed word whose constructor carries what the branch needs.
- `docs/invariants.md`, *"one edge relation, resolved once per store version"* —
  the cut this proposal keeps in one place, and the reason there is only one
  index for `Dir` to index into.
