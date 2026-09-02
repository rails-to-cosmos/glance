# Bug — a table cell loses its selection after RET commits the edit

**Status:** fixed · **Reported:** 2026-09-02 (live use, right after the
table-mount feature landed) · **Surface:** the material doc's org-table cell
edit (`RET` on a cell, `EditCell` → write → reload).

## The symptom

Editing a table cell and pressing `RET` to commit: after the write round-trips,
the cell is no longer selected. Point should stay on the SAME cell — its row AND
its column — the way it stood before the edit. Instead the column is lost.

## The mechanism

A cell edit writes through `composed`; the server persists, the watch reloads,
and the pane gets a fresh `fill`. `applyFill` (`frontend/elm/src/Doc.elm:872`)
swaps in the served subtree — the `fresh` model, whose `col` is `empty`'s
`Nothing` — and brings point back to the row it stood on
(`fillLanding` → `placeOf fresh id`). But it carries only `shut` and `hideDone`
across the rescan, NOT `col`: so the point row survives while the column resets
to `Nothing`, and `tableSelSync` (`frontend/glue/20-sheet.js`) selects the whole
row rather than the cell. `planAt`/`ephemHead` ride the same gap.

Evidence: `applyFill` carries `shut`/`hideDone` only (`Doc.elm:884`, `:892`);
`col` starts `Nothing` in `empty` and is never re-seeded from `model`; `settled`
already validates `col` (holds it only on a table body leaf, `Doc.elm:1099`), so
the carry is safe to hand it.

## The fix

Carry `col` (and `ephemHead`) from the outgoing `model` into the `fresh` one in
`applyFill`; `settled`/`reveal` then drop it where the landed row is no longer a
table body cell. A browser case pins it: edit a cell, commit, and assert the
widget's selection is still that row and column.
