# Proposal — grain navigation in the material document

**Status:** done — delivered 2026-08-04, as specified (both flagged calls taken:
`f` at the finest refuses with an echo; `b` at the floor is a no-op, never a
close) · **Origin:** user design session

## The habit, stated once

The table already teaches it: `n`/`p` walk the COARSE axis (rows), `f`/`b`
walk the FINE one (a row's cells). The document takes the same pair of axes:
`n`/`p` walk SIBLINGS at the current grain, `f`/`b` move the GRAIN itself —
finer and broader. One habit, two surfaces; org inherits what the table
taught the hands.

## What changes

Today the grains are STOPS IN ONE WALK: `n` from above meets a list whole and
then walks into its items, one sequence read in both directions
(docs/design-rhymes.md, "reverse-expand-region"). The change splits the walk
into the two axes:

- **`n`/`p` never dive.** At the element grain they step composite-to-sibling:
  a list is ONE stop however many items it holds, a table one stop over its
  rows. Holding `n` skims the document at reading grain — glancing over
  contents with no item-by-item highlighting.
- **`f` goes one grain finer.** On a composite (list, block, table) it enters
  the leaf grain: the first leaf selects, and `n`/`p` now walk THAT
  composite's leaves — items, lines, rows — clamped to it. On a headline line
  it enters the CELL grain, exactly the cells `f` walks today. On a leaf or a
  paragraph there is nothing finer: refused with an echo, the way movement
  refuses elsewhere.
- **`b` goes one grain broader.** From leaf grain the whole composite
  re-selects (the level the reader came from); from cell grain the whole
  headline line; at element grain it is a no-op with an echo. `b` = broader is
  the mnemonic gift.
- **`RET`/`DEL` keep their meanings unchanged** at whatever the selection is —
  a leaf opens its own lines, a composite the whole block, `DEL` stays the
  sheet's ladder. Movement never changes context; the grain axis is still
  movement.
- **`d`/`D`/`u` flag whatever the stop is**, as today — the finer grain just
  makes a leaf the stop.

## Cells fold into the same story

The headline's cells ARE its finer grain, so the old `f`/`b` cell walk is a
special case of the new rule rather than an exception: `f` enters cell grain
(first present cell), and *within* a grain the horizontal pair `l`/`h` (and
the horizontal arrows) step cell-to-cell, walking off either end into the
whole-line look — which is `b`'s broadening spelled by exhaustion, kept for
the hands that learned it. `n`/`p` at cell grain step to the neighbouring
ELEMENT (coarse axis unchanged), carrying the grain where the neighbour has
one — the table's own rule (a column survives a row step).

Open question, flagged rather than decided: whether `f` pressed at the LAST
cell should keep today's walk-off-the-end behavior or refuse (the new model
says a second `f` would mean "finer still", which does not exist). Proposal:
refuse with echo — walk-off stays `l`'s.

## Model and implementation shape

`drows` already lays composites out as `[whole, leaf1..leafN]` inline, and
the cursor already carries a reserved `dgrain` ("a future expand-region moves
it" — this is that future). The change is confined to the navigation
predicate and the two grain keys:

- Selection = element index + grain word (`element` | `leaf` | `cell`), plus
  the owning composite's index while at `leaf`.
- `n`/`p` filter their candidate rows by grain: at `element`, leaf rows are
  skipped; at `leaf`, candidates clamp to the owner's leaves.
- `f`/`b` move the grain word; drawing, `keepInView`, flags, and the commit
  paths read the selection exactly as they do now.
- The echo speaks it: `f → grain-finer (item 1/7)`, `b → grain-broader
  (list)`, `n → next-element (table)`.

No server change, no wire change, no new DOM — the draw already renders
leaves inside composites and what no leaf claims stays inert.

## What it costs

- The one-walk sequence — v1's "no descend key, no ascend key" — is retired,
  and design-rhymes.md's paragraph moves from "grains are stops in one walk"
  to the two-axis rule. The doctrine's deeper line (movement never changes
  context) is untouched; this is a re-partition of movement, and the change
  buys the skim.
- TestServe's material movement group re-pins: the cases that walk into a
  list under `n` become grain-key cases.
- The key line's material hint gains the grain pair.

## Verdict sought

Approve the axis split as specified (with the flagged `f`-at-last-cell
choice), and it lands as one Glue + tests + docs change.
