# Bug — the title box sits on the baseline when the title is empty

**Status:** fixed · **Reported:** 2026-08-25 (live use: the capture doc's bare
draft) · **Surface:** the title edit (`#dtitle`/`#dtin`) over any headline
whose title is empty — the capture draft is where every reader meets it.

## The symptom

Editing an EMPTY headline title, the input appears slightly below its line.
Once a title is written, the next edit is aligned. Empty alone misplaces it.

## The mechanism

`.d-head` lays its cells with `align-items:baseline`
(`src-web/Glance/Web/Page/Style.hs:314`). An empty `.dc-title` holds one
empty TEXT NODE (`viewCells` → `drawText ""` → `[ text "" ]`), so it has no
text baseline; flexbox then synthesizes one from the item's zero-height box
and aligns THAT to the row's real baseline — the cell's rect lands at the
baseline's y, height 0. `placeEdit`'s tight mode
(`frontend/glue/20-sheet.js`) copies the anchor rect verbatim
(`s.top = a.top - …; s.height = a.height`), so `#dtitle` inherits the drop.

Measured on the bare capture draft (browser probe, since removed):

```
EMPTY: row t=148.5 h=23 · .ds t=149.5 h=21 · .dc-title t=164.5 h=0
       #dtitle t=164.5 h=0 (style.top 47.5px) · #dtin t=154
FULL:  .dc-title t=149.5 h=21 · #dtitle t=149.5 h=21 (style.top 32.5px)
       #dtin t=149.5
```

15px = the 13px font's ascent inside the 21px line: the baseline.

## The fix (same day)

The measurement, never the layout: `placeEdit`'s tight mode takes the
VERTICAL from the anchor's row — `tr.closest(".de")`, padding-compensated —
and keeps the horizontal from the cell, where flex places even an empty box
truly (`frontend/glue/20-sheet.js`). A table was considered and rejected:
table-cells aligned by baseline synthesize an empty cell's baseline the same
way, and dropping baseline alignment un-seats the 11px tag run against the
13px title. Pinned in the empty-title browser case: the box's `y`/`h` ride
beside its x/w asserts, red on the old glue ("it dropped to the baseline").
