# Proposal — the starred metas, as a list

**Status:** done — DONE 2026-08-10 · **Date:** 2026-08-04

`docs/design-rhymes.md:66-69` names a family: "**Stars mean meta.** `*active*`,
`*inactive*`, `*empty*`, `*archive*`, `*none*` — a starred word is reserved
semantics in every context, never a literal."  It is the only family doctrine
names that has no list anywhere in the code, and the drift has already happened
in prose: the one comment that tries to enumerate it
(`src-web/Glance/Web/Sort.hs:41`) names `*clear*`, which exists nowhere, and
omits `*empty*` and `*none*`, which do.  `CLAUDE.md` carries the same stale
member.

**Re-validated and implemented 2026-08-10.**  Two premises had already
self-healed: the stale `*clear*` comment is gone from both `Sort.hs` and
`CLAUDE.md`, and `tagValues` went with the `view:archive` work, so
`*archive*` is now `Filter`'s and derived from `archiveTag`.  The
structural half held — five words, four constants, three modules, no
owner — and is what was implemented.  `Meta`/`metaWord`/`metas`/`starred`
live in `Glance.Query`; the four constants stayed as aliases, so no call
site moved.  The payoff is the suite case rather than the type: it
asserts the five constants ARE `map metaWord metas`, which a sixth
constructor added to the type alone fails (verified by mutation).
## Where the five members live

| Meta | Producer | Renderer |
|---|---|---|
| `*active*` | `src-query/Glance/Query.hs:2880` (`stateValues`), evaluated at `src-web/Glance/Web/Filter.hs:528` | `table-view.js:793` (`ACTIVE_META`), matched `:2107` |
| `*inactive*` | `Query.hs:2880`, `Filter.hs:529` | absent; falls to the literal arm and finds nothing |
| `*empty*` | `Filter.hs:253` (`emptyMeta`), `Glue.hs:2980` (`EMPTY`) | `table-view.js:803`, `:2070` |
| `*archive*` | `Filter.hs:246` (off `archiveTag`), `Query.hs:2889` (`tagValues`) | absent as a literal; caught generically `:2093-2096` |
| `*none*` | `src-web/Glance/Web/Sort.hs:69` (`noOrder`) | `table-view.js:846`, `:911` |

Four constants in three modules plus a JS `const`, with no module that owns the
concept.  Test coverage follows the scatter: `*active*` appears 78 times across
`test/`, `*empty*` 53, `*archive*` 32, `*none*` 32, `*inactive*` 13.  The owning
group `TestFilter.hs:574` (`metaSpec`) is hand-written per member; the one
quantified case (`:615-620`) iterates eight hand-listed strings and asserts the
SHAPE rule — that a starred word is a starred word — rather than that any
particular starred word means anything.

## What is already closed

The undeclarability half is genuinely enforced, by two charset walls.
`keywordTextP` admits letters and `_`, so a starred word cannot parse into a
`#+TODO:` keyword, and `setStateEdits` refuses any word a file's chain does not
declare (`Query.hs:2021-2023`).  `tagText`'s charset (`Query.hs:1942`), reached
through `wantsTag` (`Commands.hs:220-222`), does the same for tags.  A starred
literal cannot be smuggled in as data.

What is open is the other half: which starred words exist, and what each means.

## What member six costs

A constant (1 line), an evaluation arm in whichever of four places it belongs
(`Filter.hs:474`, `:504`, `:528-530`, or `Sort.hs:131`), a `values` declaration
so completion offers it (`Query.hs:2879` or `:2888`), the doctrine list, the
`SCHEMA.md:376-380` divergence note, and the shape roster at
`TestFilter.hs:615-620` — five to thirteen lines across six files in two repos,
with nothing checking that the six agree.  Renderer cost is zero for a
producer-only meta, since `declaredMetas` (`table-view.js:967-969`) picks up
whatever the column declares.

The cross-side meanings have already diverged: the renderer's `*active*`
(`table-view.js:2107`) matches the empty cell alone, where the producer's
(`Filter.hs:528`) matches `hrActive == Just True` OR the empty cell.  That is
`SCHEMA.md`'s blessed direction — the renderer may be narrower — and nothing
checks the direction, so the opposite skew would ship the same way.

## Proposed change

One roster in `Glance.Query`, which `Glance.Web.Filter` and `Glance.Web.Sort`
both already import.

```haskell
-- | The reserved metas, whole.  A starred word is never a literal keyword,
-- never a tag and never a cell value; the charset walls ('keywordTextP',
-- 'tagText') make one undeclarable, and this list is the other half — what a
-- completion offers, what a refusal names, and what a suite iterates.
data Meta = MActive | MInactive | MEmpty | MArchive | MNone
  deriving (Eq, Show, Enum, Bounded)

-- | WORD as it is spelled, stars and all.
metaWord :: Meta -> Text
metaWord MActive   = "*active*"
metaWord MInactive = "*inactive*"
metaWord MEmpty    = "*empty*"
metaWord MArchive  = "*archive*"
metaWord MNone     = "*none*"

-- | Every meta, for a roster that cannot go stale.
metas :: [Meta]
metas = [minBound .. maxBound]
```

`stateValues` (`Query.hs:2880`) and `tagValues` (`:2889`) become
`map metaWord` over a filter of `metas`; `Filter.hs:246`/`:253` and `Sort.hs:69`
read `metaWord` rather than spelling the stars; `metaSpec`
(`TestFilter.hs:574`) becomes `mapM_` over `metas`, so member six arrives with
its shape case already written.  The stale comment at `Sort.hs:41` is replaced
by a reference to the type, and `CLAUDE.md`'s `*clear*` goes with it.

## The one thing this does NOT buy

`Meta` cannot make the two repos agree — there is no schema revision mechanism
between them (`docs/invariants.md:1997-2006` records this as **none**), and the
`Filter` ↔ `table-view.js` port is deliberately term for term.  What it buys is
that the producer's side has a place to be complete, so the SCHEMA note and the
doctrine list can be written against a symbol rather than against memory.  A
cross-repo tripwire would be a separate decision.

## LOC

Added ~16 (the type, `metaWord`, `metas`).  Removed ~8 (four scattered constants
and their haddock, the stale comment).  Saved per future meta: a hand-audit of
six files becomes one constructor the compiler names at every `case` — including
`metaSpec`, which gains the member's shape case for free.

## Risk

No wire change: every meta is spelled exactly as it is spelled today, and
`stateValues`/`tagValues` emit the same strings in the same order.  `Glance.Query`
gains two exported names.  The renderer is untouched.  The main care is that
`Filter.hs:246` derives `*archive*` from `archiveTag` rather than spelling it, so
that derivation should stay and `metaWord MArchive` should agree with it — worth
one assertion.

## Existing precedent

`Data.Org.Config.keywordScopes` (`Config.hs:601-606`) — one list, two readers,
three answers, with the source saying so at `:591-595`.
`Glance.Query.linkTypes` (`:766-775`) is the closer rhyme: "Spelled once and read
three ways … adding one is one edit rather than three that no test ties
together."  The starred metas are that sentence's counterexample.
