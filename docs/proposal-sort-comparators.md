# Proposal — configurable sort comparators

**Status:** brainstorm, not scheduled · **Date:** 2026-08-02

How should a reader configure *how* a column compares, not just *whether* it
sorts?  The driving example: `state` must order semantically — `TODO`,
`DELEGATED`, `DONE` — never alphabetically.

## What already exists

More of this is built than it first appears:

- **SCHEMA's `values` array is a declared order.** A column carrying
  `values: [...]` sorts by position in that list; both renderers honor it
  (el's `--value-order`, js's `valueOrder`).  The mechanism for "not
  alphanumeric" is already the contract.
- **`state` already uses it.** The server emits the state column's `values`
  from the badge palette, whose order is the keyword-cycle declaration order.
  So today, `state` sorts `TODO < DELEGATED < DONE` exactly when the
  `#+TODO:` line spells them in that order — **the org file is already the
  comparator config.**
- **Per-key null placement** exists in el's direction spellings
  (`asc-nulls-first`…) and the arrange rule (nulls settle outside direction).
- **`compare` kinds** (`string` / `numeric` / `version`) are per-column
  SCHEMA fields.

## The gap

1. No way to *override* a column's order without changing what produces it
   (reordering the `#+TODO:` line changes the palette and the cycle, not just
   the sort).
2. No comparator choice for text columns (codepoint vs locale collation — a
   live el/js divergence already documented).
3. No per-token comparator in the sort grammar (`sort:state` always means the
   column's one comparator).
4. Tags: alphabetical is the only order; frequency or pinned-first are
   imaginable.

## Options

**A. Implicit semantic orders (recommended default, zero new config).**
Each column's natural order is derived from what the tree already declares:
`state` = cycle declaration order (active block order, then inactive);
`priority` = letter order; dates = chronological; `title`/`tags` = folded
text.  This is nearly the status quo — the work is documenting it as the
rule and fixing any column that drifts from its natural order.

**B. Per-column override pragma (the escape hatch).**
`#+GLANCE_SORT_STATE: DELEGATED TODO DONE` in `system.org` — a value-order
override, spliced by the same pragma machinery as the default filter and
capture target, editable in the settings sheet.  Values it names come first
in that order; unnamed values follow in the column's natural order.  Wire:
the server just emits the overridden `values` — **no renderer work at all**,
the contract already obeys `values`.

**C. Comparator naming in the sort token (grammar growth, later).**
`sort:state:cycle` vs `sort:state:alpha`, `sort:title:locale` — the third
`:`-segment names a comparator.  Composes with the existing `:desc`
(`sort:state:alpha:desc`).  Cost: grammar surface, parity, completion; value:
per-query comparator switching, which nobody has asked for yet.  Park it.

**D. Full comparator language.**  Rejected: deterministic data (an ordered
value list) covers every real case named so far; a comparator *language*
is C with extra steps and a security surface.

## Recommendation

A now (document + verify the natural orders), B when the first real
override need appears (the machinery is one pragma + one settings row —
every piece exists), C parked until a per-query case shows up, D never.

Nulls and locale stay separate decisions: null placement could join B's
pragma (`... :nulls first`), and title collation is a one-bit config that
should follow whichever way the el/js divergence gets arbitrated.
