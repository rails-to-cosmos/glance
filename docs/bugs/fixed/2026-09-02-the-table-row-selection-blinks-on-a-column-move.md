# Bug — the table's row selection blinks on every column move

**Status:** fixed · **Reported:** 2026-09-02 (live use) · **Surface:** the
material doc's org table, walking cells with `f`/`b` (a column move within one
row).

## The symptom

Moving between cells of a row — `f`/`b` changing the selected column — the row
selection wash flickers on every step. The main table-view, walked by its own
keys, never blinks: a cursor move repaints the bands, it does not rebuild the
table.

## The mechanism

Elm's `view` runs on every state change, and `glanceTable` builds a fresh
`view` property each time (`tableView` re-encodes columns and rows into a new
`Json.Value`, `frontend/elm/src/Doc.elm`). Elm's virtual DOM sees a new property
value and re-sets it, so the custom element's `set view`
(`frontend/glue/20-sheet.js`) fires on EVERY render — including a bare column
move, which changes `col` (point) but not the table's own data. `set view`
calls `this._tv.setView(v)`, which rebuilds all rows; the rebuild clears the
selection and `tableSelSync` re-selects it a frame later — the blink.

The table DATA is identical across a column move; only the point moved, and the
point is not in the view.

## The fix

The custom element guards the rebuild: it remembers the encoded view and skips
`setView` when the new one is byte-identical, so a column move drives only
`tv.select` (smooth) and `setView` fires only on a real content change (an edit,
an add, a delete). A browser case pins it: tag a rendered row, move `f`, and
assert the same element survives — the table was not rebuilt.
