# Proposal — one door for the renderer's optional capabilities

**Status:** DONE 2026-08-10 · **Date:** 2026-08-05 · **Source:** generalizer round 3
(hand-run) over the pin/composer/ladder day

**Re-validated 2026-08-10:** HOLDS, and had GROWN — the widget split took
the refusal sentences from 8 to 12, three of them the identical
`"…has no delete flags"` in three surfaces.  Implemented by making `can'
VARIADIC (all 16 existing call sites unchanged), which collapses the four
conjunction aliases; `lacks'/`wants' give the sentence one home.
## Pattern

The shell feature-detects every optional renderer capability — the documented
graceful-floor rule — and the day's four new capabilities (`onPin`,
`setPinned`, `setQuery`, `getRows`) each grew the same three artifacts by
hand, as every earlier one did:

1. **The guard**: 23 `can(handle, "name")` call sites in `Glue.hs`, most as
   guard-plus-call pairs (`if (can(table, "setPinned"))
   table.setPinned(...)`).
2. **The predicate alias**: `crumbing()` (`:435`), `strips()` (`:557`),
   `pager()` (`:2356`), `cells()` (`:2418`), `marking()` (`:2438`) — one
   `can`-conjunction each, named for its feature.
3. **The refusal sentence**: eight hand-spelled forks of ONE template —
   `"this table-view.js has no delete flags"` (`:2135`, `:3831`),
   `no pager` (`:2373`), `no cell selection` (`:2421`), `no marks` (`:2474`,
   `:4652`), `no archive flags` (`:2741`), `no crumbs` (`:4349`) — plus
   `no filter tokens` near `strips`.

Marginal cost of capability N+1 today: 2–4 guard sites, possibly an alias,
possibly a refusal string copied from a neighbour — and nothing ties the
sentence's spelling to the others.

## Proposed change

One capability door, three small helpers in `Glue.hs` beside `can`:

```js
// Call NAME on H when the asset carries it; undefined otherwise.
const on = (h, name, ...args) => (can(h, name) ? h[name](...args) : undefined);
// Whether the asset carries every NAME — the predicate aliases become calls.
const has = (...names) => names.every((n) => can(table, n));
// The one refusal sentence, spelled once: false (and spoken) when the
// asset lacks any NAME, so a handler reads `if (!wants(b, LABEL, ...))`.
const wants = (b, label, ...names) =>
  has(...names) || (said(b, `this table-view.js has no ${label}`), false);
```

- Silent-optional sites (`setPinned`, `setRows` handoffs, `keepInView`'s
  typeof) become `on(table, "setPinned", ...)` one-liners.
- The five aliases become `has("nextPage", "pageInfo")` etc. at their use
  sites, or stay as named consts defined through `has`.
- The eight refusal sites read `if (!wants(b, "crumbs", "pushCrumb",
  "popCrumb", ...)) return;` — the sentence has one home, so the next
  capability's refusal cannot drift in spelling.
- `flagKey`'s `missing:` strings (two spellings of the flags refusal) read
  the same template.

## Files

`src-web/Glance/Web/Page/Glue.hs` (single module); `test/TestServe.hs` glue
needles that pin the refusal sentences re-aim at the template call.

## LOC estimate

+8 (the three helpers) / −25 to −30 immediately (guard pairs, alias bodies,
sentence forks) / **−2 to −4 per future capability**, and the refusal
spelling becomes un-forkable.

## Risk

None on the wire; page-internal. The `bare`/`pageless`/`sortless`/
`crumbless` harness modes exercise every refusal path, so a mis-fold fails
loudly. Test baselines: the literal refusal needles move to one spelling —
which is the point.

## Existing precedent

`can` itself (one `typeof` spelling, apply-batch); `flagKey`'s shape record
(per-surface phrases declared, spoken by one caller); `rowsWord` (one
pluralisation for every set-naming surface).
