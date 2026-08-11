# Proposal — configurable sort comparators

**Status:** partial — A delivered (#67, 2026-08-04); B/C/D still brainstorm ·
**Date:** 2026-08-02, revised 2026-08-04

How should a reader configure *how* a column compares, beyond *whether* it
sorts?  The driving example: `state` must order semantically — `TODO`,
`DELEGATED`, `DONE` — never alphabetically.

## What already exists

More of this is built than it first appears:

- **SCHEMA's `values` array is a declared order.** A column carrying
  `values: [...]` sorts by position in that list; both renderers honor it
  (el's `--value-order`, js's `valueOrder`).  The mechanism for "not
  alphanumeric" is already the contract.
- **`state` already uses it.** The server sorts the state column by position in
  the badge palette, whose order is the keyword-cycle declaration order. So
  `state` sorts `TODO < DELEGATED < DONE` exactly when the `#+TODO:` line spells
  them in that order — **the org file is the comparator config.**

  That sentence was written as though it were already true and was not: three
  `Data.Set` hops between the `#+TODO:` line and the palette
  (`PTodo`'s `Set.fromList`, `declaredKeywords`' `Set.toAscList`, `hrKeywords`
  off the parse's ending context) alphabetized every tree's cycle before a
  comparator could read it, so `state` sorted `DELEGATED < DONE < TODO` whatever
  the line said. #67 (2026-08-04) made the keyword lists ordered end to end and
  the claim true; see docs/invariants.md, "A keyword list is ORDERED".
- **Per-key null placement** exists in el's direction spellings
  (`asc-nulls-first`…) and the arrange rule (nulls settle outside direction).
- **`compare` kinds** (`string` / `numeric` / `version`) are per-column
  SCHEMA fields.

## The gap

1. No way to *override* a column's order without changing what produces it
   (reordering the `#+TODO:` line changes the palette and the cycle along with
   the sort).
2. No comparator choice for text columns (codepoint vs locale collation — a
   live el/js divergence already documented).
3. No per-token comparator in the sort grammar (`sort:state` always means the
   column's one comparator).
4. Tags: alphabetical is the only order; frequency or pinned-first are
   imaginable.

## Options

**A. Implicit semantic orders (recommended default, zero new config).**
DELIVERED, #67.  Each column's natural order is derived from what the tree
already declares: `state` = cycle declaration order (active block order, then
inactive); `priority` = letter order; dates = chronological; `title`/`tags` =
folded text.  The one column that drifted was `state`, and it drifted all the
way — the fix was ordering the keyword lists through the whole presentation
chain rather than documenting a rule the code already followed.

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

A is done.  B when the first real override need appears (the machinery is one
pragma + one settings row — every piece exists), C parked until a per-query
case shows up, D never.

B's cost fell out of A: reordering a `#+TODO:` line now IS the state override,
editable in the settings sheet already, at the price of moving the cycle
itself.  `#+GLANCE_SORT_STATE:` buys only the case where a reader wants the
sort and the cycle to disagree, which nobody has asked for.

Nulls and locale stay separate decisions: null placement could join B's
pragma (`... :nulls first`), and title collation is a one-bit config that
should follow whichever way the el/js divergence gets arbitrated.
