# Bug — the cell / column-name edit box paints behind the mounted table

**Status:** fixed · **Reported:** 2026-09-02 (live use) · **Surface:** the
material doc's org-table cell edit and column rename — the overlay box (`#dpara`,
laid over the `td`/`th`).

## The symptom

Renaming a column (or editing a cell), no text input is visible: the reader
types blind and `RET` applies. The box IS open (`#dpara.on`) and positioned over
the header cell, but it is hidden BEHIND the table.

## The mechanism

The doc's edit overlays — `#dtitle`, `#dpara`, `#dpair`, `#ddate` — are
`position: absolute` with NO `z-index` (`assets/page.css`). The mounted
`table-view` sets stacking of its own inside the same pane: a `position: sticky`
header (`z-index: 1`) and a summoned filter dock (`z-index: 91`)
(`assets/table-view.js`). Where the pane's stacking context lets those share a
level with the box, the table paints OVER the `z-index: auto` overlay, so the
box lands behind the header cell it covers.

Not reproduced in the headless browser harness (the probe's page did not create
the stacking context that orders the table above the box), so this is pinned by
the fix rather than a failing case — the box carries a `z-index` that out-ranks
the widget's internals regardless of context.

## The fix

`z-index: 100` on the shared edit-box rule, above the widget's sticky header and
filter dock. The overlay reliably paints over the table it covers.

A deeper fix is proposed separately: give `table-view` NATIVE cell editing (an
input IN the cell), which retires the overlay for the table entirely — see the
table-view cell-edit proposal.
