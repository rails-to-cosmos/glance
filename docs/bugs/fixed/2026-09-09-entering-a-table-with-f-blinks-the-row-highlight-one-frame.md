# Bug — entering a table with `f` blinks the row highlight for one frame

**Status:** fixed · **Filed:** 2026-09-09 · **Fixed:** 2026-09-09 · **Surface:**
the material-doc table mount — the doc pane (`frontend/glue/20-sheet.js`) driving
the table-view widget (`assets/table-view.js`).

## Fixed (2026-09-09)

`paintSelection` now stamps the selection SYNCHRONOUSLY
(`assets/table-view.js`, one added `stampSelection()` call) — the row wears
`tv-sel` the same tick the selection moves, so the doc's whole-table wash and the
widget's row wash hand off within one frame. The re-stamp only toggles classes on
the rendered window (no scroll, no rebuild), so the frame loop still owns the
window and the ease. Regression test: `the widget stamps a selection in the same
tick, no f-enter blink` (`test/browser/cases.mjs`) — red before, green after,
full suite 97/97.

**Fixed in the VENDORED copy only.** `assets/table-view.js` is ahead of the
source `../table-view/web/table-view.js` (the source lacks the in-cell editor),
so `make sync-renderer` would regress the widget and was NOT run — the fix must
be back-ported to the source when it is reconciled (docs/tasks.org).

## Symptom

In the material doc, with a table HIGHLIGHTED (point on the whole-table stop),
pressing `f` to enter the table blinks the row highlight once — it drops for a
frame before the first row's highlight appears. It should hand off with no blink.

## Steps to reproduce

1. Materialize an entry that mounts a table (a `comp:table`).
2. Move point onto the table stop — the whole table is washed by the doc's
   cursor.
3. Press `f` to enter the table.
4. The highlight blinks: one frame with neither the whole-table wash nor the
   row wash, then the first row lights.

## Root cause

Two owners draw the highlight, and the handoff on `f` has a one-frame gap:

- While point is on the table stop, the DOC washes the table — the comp row is
  `.de.dat`, so the pane's cursor rule paints it — and the widget shows no row
  (`tableSelSync` sets `gt-nosel`, `frontend/glue/20-sheet.js:814`).
- On `f`, point moves to a body row. `tableSelSync` runs SYNCHRONOUSLY: it drops
  the doc wash (`gt-nosel` off, `20-sheet.js:815`) and calls `tv.select(id, col)`
  (`20-sheet.js:829-830`).
- But the widget stamps `tv-sel` a frame LATER: `select` → `selectRow`
  (`assets/table-view.js:3458`) → `schedule()` (`:3480`), and `tv-sel` is stamped
  only when the frame loop runs `stampSelection` (`tick`, `:3514`). The glue
  already notes it: "The renderer stamps `tv-sel' a frame later"
  (`20-sheet.js:628`).

So for one frame the doc wash is gone and the widget's row wash is not yet
stamped — the highlight blinks. (A pure crossfade would overlap the two; here
they miss by a frame.)

## Fix direction

Make the two highlights hand off within the same frame. Either:

- **Widget (cross-repo, cleaner):** in `selectRow`, stamp the SELECTION
  synchronously (`stampSelection()` touches only classes, no scroll/window), and
  keep `schedule()` for the window/ease the one-frame loop owns
  (`assets/table-view.js:3483` explains why window/ease stay in the loop). Source
  is `../table-view/web/table-view.js`, vendored by `make sync-renderer`.
- **Glue (in-repo):** hold the doc's whole-table wash across the frame the widget
  needs to stamp — drop `gt-nosel` one frame later, so the doc wash bridges the
  gap.

Repro test: the artifact is a one-frame paint, so a resting-state DOM assertion
won't catch it. The existing case "a column move does not rebuild the table (no
selection blink)" (`test/browser/cases.mjs`) is the template for a "no blink"
oracle — mirror its approach for the whole-table→`f`→row transition.
