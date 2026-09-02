# Bug — `d` on a selected table column flags the row, not the column

**Status:** fixed · **Reported:** 2026-09-02 (live use, right after the
table-mount feature landed) · **Surface:** the material doc's org table, a cell
selected (a column at point) — the delete key `d`.

## The symptom

With a table CELL selected — a column at point — pressing `d` flags the whole
ROW for deletion, the dired-style gesture every element wears. When a column is
selected the delete should apply to the COLUMN: the cell struck from every row
and the header, the way `+` on a cell adds a column. `d` on a whole row (no
column) still deletes the row.

## The mechanism

`d` routes straight to `flagPress` → `flagKey(DFLAGS)`
(`frontend/glue/20-sheet.js`), which flags the row at point by id — it has no
notion of the within-row column axis the feature added (`dcol`). So the affordance
the walk already draws — a whole-row wash vs a cell crossing, which `+` reads to
choose row-add vs column-add — is ignored by delete: `d` always means the row.

`+` already branches on `dcol` (whole row → `addrow`, cell → `addcol`,
`20-sheet.js` `insertHere`); delete has no matching branch, and there is no
column-delete message on the Elm side (`AddCol`/`insertCol` have no `DelCol`
twin).

## The fix

Give delete the same `dcol` branch `+` has: on a cell, `d` deletes the COLUMN
through a new `DelCol` Elm message (`removeCol` striking the column from every
row's line, hlines left as separators, the last column refused); on a whole row
it flags the row as before. A browser case pins it: select a cell, press `d`,
and assert the column count dropped while the rows stand.
