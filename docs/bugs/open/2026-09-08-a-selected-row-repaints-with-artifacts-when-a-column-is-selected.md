# Bug — a selected row repaints with artifacts when a column is selected

**Status:** open · **Filed:** 2026-09-08 · **Re-confirmed:** 2026-09-09
(`assets/table-view.js` untouched, the diagnosed lines still present) ·
**Tracked:** [docs/tasks.org](../../tasks.org) · **Surface:** the table-view
widget (`assets/table-view.js`), both the main table and the material-doc table
mount — the same widget, so the bug is consistent across both.

## Symptom

With a COLUMN selected, moving the row selection with `n`/`p` repaints the
selected row through a visible artifact — the row does not crossfade cleanly the
way it does when no column is selected. Row-only selection moves (no column)
are smooth.

## Steps to reproduce

1. Open any table (the main table-view, or a material-doc entry that mounts one).
2. Select a column (click a cell, or navigate into a cell so `getSelection().col`
   is a real index rather than `null`).
3. Move the row selection with `n`/`p`.
4. Watch the crossing cell (selected row × selected column): it flashes an
   off color mid-move rather than crossfading. With no column selected, the same
   `n`/`p` move is smooth.

## Root cause

The selection is drawn as three stacked grounds on two different elements:

- the ROW band is a background on the `<tr>` (`tv-sel`) —
  `assets/table-view.js:3064` (`rowClasses`), painted `.tv-table tbody tr`;
- the COLUMN band and the CROSSING cell are TRANSLUCENT backgrounds on the
  `<td>` (`tv-colsel`, `tv-cell-sel`) — `assets/table-view.js:3078` (`cellClasses`),
  painted `.tv-table tbody td.tv-colsel` / `td.tv-cell-sel`
  (`assets/table-view.js:1817`–`1824`), each a `color-mix(... , transparent)`
  wash that composites OVER the row band beneath it.

Every `tbody tr` and `tbody td` transitions `background-color .08s ease-out`
(`assets/table-view.js:1788`). `stampSelection` (`assets/table-view.js:3339`)
re-stamps classes without a rebuild, so the grounds crossfade.

On an `n`/`p` move with a column selected, the crossing cell changes on BOTH
layers at once: the `<tr>` row wash fades out/in AND the `<td>` cell wash
transitions `tv-cell-sel`⇄`tv-colsel`. Because the top `<td>` layer is
translucent, the two independent eased transitions on the two stacked elements
do not compose to a single clean crossfade — the cell passes through an
incorrect intermediate color. A row-only move (`selCol === null`) touches only
the `<tr>` layer, so there is one animating ground and no mis-composition.

## Fix direction

Drive the crossing cell through ONE animating ground rather than two stacked
translucent transitions — e.g. compose the row+column washes opaquely on the
cell (the header already does this: `th.tv-colsel` mixes into the page ground
rather than laying a film over it, `assets/table-view.js:1817`), or suppress the
`td` transition on the crossing cell during a pure row move so it snaps under a
crossfading row.

The widget's source is the sibling repo `../table-view/web/table-view.js`; the
fix lands there and is vendored into `assets/table-view.js` by `make
sync-renderer`. A browser case under `test/browser/` reproduces the setup
(column selected, `n`/`p`), though the artifact itself is a transient paint that
a resting-state DOM assertion will not catch — a screenshot diff across the move
is the reliable oracle.
