# Proposal — run the parity vectors this repo already has a sibling for

**Status:** proposed · **Date:** 2026-08-15 · **Origin:** the user, asking
whether server-side rendering would pay.  It would not, for a loopback
single-user app answering `/headlines` in 0.09 s — but the duplication the
question was pointing at is real, and this is the cheap half of fixing it.
**Supersedes** item 7 of
[../partial/2026-08-13-oracles-that-cannot-fail.md](../partial/2026-08-13-oracles-that-cannot-fail.md),
which named this and left it as a decision.

## The gap

The filter grammar is implemented twice: `Glance.Web.Filter.matchesFilter`
server-side (`Routes.hs:192`) and again inside the 5173-line vendored
`assets/table-view.js`, which carries its own `getQuery`/`setQuery`/`onFilter`.
`AGENTS.hs:2510` states the cost and tags it `Unguarded`:

> Nothing versions the agreement with table-view.js — no handshake, no schema
> version — so every gap above is silent by construction and **both sides are
> kept term for term by hand.**

`make sync-renderer` only *reports* a diff (`Makefile:104-108`); it gates
nothing.  A renderer bump with one changed term makes the two halves disagree
about which rows match, in one session, with no test anywhere that can fail.

`AGENTS.hs:2560` says the vectors *do* run — `"fixtures/parity/filter-query.json
and sort-tokens.json run the shared half of the grammar over the browser
renderer" [Browser]`.  That is true of the **sibling repo's** harness.  From
glance's side nothing reads them, so the `[Browser]` proof does not cover the
Haskell half at all.

## What the sibling already has, measured

`../table-view/fixtures/parity/` holds a manifest and six vector files, and the
manifest **already anticipates a third harness**:

> One manifest, two harnesses … A vector file declares a capability; a harness
> runs the capabilities listed for it below and **fails on any it does not
> implement, so the manifest cannot quietly claim one that is missing.**

```json
"harnesses": {
  "web/perf-driver.js":  ["sort", "render", "query", "query-sort"],
  "table-view-test.el":  ["sort", "render"]
}
```

Adding `"glance/test": ["query"]` is the shape it was built for.

## The measurement that makes this cheap

**`filter-query.json`'s column keys are exactly glance's `filterKeys`.**

| vectors | glance's `viewColumns` (`Query.hs:1799-1806`) |
|---|---|
| `deadline, priority, scheduled, state, tag, title` | `state, priority, title, scheduled, deadline, tag` |

Same six, no remainder.  So **all 67 query cases map onto a `HeadlineRecord`
without inventing a column**, and 26 of them carry their own view, which is
four fixture row-sets rather than one.

The sort file splits, and the split is informative rather than awkward:
**58 `sort-tokens` cases, 41 of which name `score`** — a custom numeric column.
`Glance.Web.Sort.sortChainIn` refuses any key outside `filterKeys`, so those 41
exercise a capability glance deliberately lacks.  **17 run over shared keys.**

So the honest declaration is `["query"]` now, and `"query-sort"` only if the 17
are split out upstream or the harness filters by view.

## Proposed change

A `paritySpec` group in `test/TestFilter.hs`:

1. Read `../table-view/fixtures/parity/manifest.json`; find the entry for this
   harness.  **Absent sibling checkout ⇒ skip loudly**, the way
   `withCorpusSample` and `make interop` already do — `hPutStrLn stderr
   "\nSKIPPED - no ../table-view checkout: parity vectors"`.
2. For each vector file the manifest lists for this harness, read its `views`
   and `cases`.
3. Build `[HeadlineRecord]` from a view's rows: each `cells` key is one of the
   six, `tag` splits on `:`, `id` becomes `hrId`.  A view naming a key outside
   `filterKeys` is a **failure, not a skip** — that is the manifest's own rule
   about not quietly claiming a capability.
4. `assertEqual (caseName) (expect.ids) [ hrId r | r <- rows, matchesFilter (storeEnv rows) q r ]`.
5. **Anti-vacuity**, the house idiom: `assertBool "too few vectors run"
   (length cases >= 60)`.  A manifest that parses to nothing must go red rather
   than green.

Then retag `AGENTS.hs:2510` from `[Unguarded]`, and correct `:2560` — it
currently claims coverage the Haskell side does not have.

## Files

`test/TestFilter.hs` (the group), `test/TestDefaults.hs` (the skip helper, beside
`glanceBinary`'s), `AGENTS.hs` (two notes), `glance.cabal` (nothing — `aeson`
and `directory` are already test deps), and one line in
`../table-view/fixtures/parity/manifest.json` declaring the harness.

## LOC

Added ~90 in `test/TestFilter.hs` plus ~8 shared.  Removed 0.  **What it buys is
125 vectors' worth of agreement that today rests on hand-copying**, and it is
the first thing in this repo by which a parity vector can fail.

## Risk

Test-only.  No production module changes, no wire field moves, no org bytes, no
gesture.  Two ways it can go wrong, both loud by construction: the manifest
parses to nothing (caught by the floor), or a view names a column glance lacks
(a failure by design).

**It may well go red on first run.**  That is the point — `AGENTS.hs:2535`
already records one live divergence (`scheduled:` substring-matching under two
dated rows).  A red first run turns an unguarded hand-agreement into either a
fix or a written-down, tested divergence.

## What this deliberately does not do

Delete the client-side filter.  That is the *other* answer to the same
duplication — make every query go to `/headlines?q=`, one grammar, one
implementation — and it costs a round trip per query commit.  Worth measuring
against a keystroke-debounced filter over localhost, and worth deciding after
this proposal has said how far apart the two implementations actually are.
Ordering matters: measure the disagreement before spending a surface to remove
it.
