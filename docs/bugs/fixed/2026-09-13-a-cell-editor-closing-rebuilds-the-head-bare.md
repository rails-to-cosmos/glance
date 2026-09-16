# Bug — a cell editor closing rebuilds the head bare, and the table goes six equal columns

**Status:** fixed — the head owns the colgroup and re-applies the widths ·
**Reported:** 2026-09-13 (`spikes/2026-09-13-frozen-columns`, §5.1; the shot is
`0-walk.png`) · **Surface:** `assets/table-view.js`, `closeCellEditor` /
`renderHead` / `applyWidths`

## Symptom

Every column of the table jumps to the same width the moment an in-cell editor
closes: one `TAB` out of a draft cell, or one `ESC`. The six columns of the
default view — a 4ch state badge, a 1ch priority, a long title, two dates and a
tag run — all become 217px, and the title that was carrying the slack comes in
to the same width as the priority. The next repaint that measures anything puts
them back, so it reads as a flicker rather than as a state.

## Reproduce

Two presses from the keyboard, over the shipped page:

1. `RET` on a standing row (or `+` and then `TAB`, which is the common path).
2. `ESC`.

Measured: `[47.4, 217.1, 217.1, 217.1, 217.1, 217.1, 217.1]` against the fitted
`[47.4, 95.6, 72.2, 548, 125.4, 125.4, 336]`, and every `<col>` carrying
`style.width === ""` where it had carried `calc(40ch + 24px)`.

## Root cause

`closeCellEditor` ran two renders in this order — `assets/table-view.js:4021`,
`:4022` before the fix:

```js
      renderRows(true);
      renderHead();
```

`renderRows` ENDS in `applyWidths()` (`:3272`), which writes the fitted widths
onto the `<col>` nodes. `renderHead` then opens with `colgroup.innerHTML = ""`
(`:3070`) and builds a fresh `<col>` per column carrying no width at all. Under
`table-layout: fixed` (`:1666`) a colgroup with no widths is authoritative:
the engine divides the table equally among the columns. The widths were written
and then thrown away, one render apart.

The head is called there because a HEADER editor may have renamed a column, so
the order alone is not the fault — the ownership is. The colgroup is the head's
node, and the head was returning it empty.

## Fix

`renderHead` ends with `applyWidths()`: the head builds the colgroup and puts
the standing widths back on it before it returns, so no caller has to know that
rebuilding a head un-sizes a table.

- `assets/table-view.js` — `renderHead` ends `applyWidths();  // the colgroup is
  new; the widths are not on it`, with the reason in its docstring.

This lands with the frozen-columns change (a column is fitted once per view),
where it matters more than it did: the widths are no longer re-measured on the
next paint, so a colgroup left bare would have STAYED bare rather than flickering
back.

## Test

`test/browser/cases.mjs` — *TAB out of a draft cell leaves every column where it
was*: records every `th`'s px and every `<col>`'s written style, opens a draft,
`TAB`s out of the title cell and `ESC`s out of the draft, and asserts both
readings are identical to the fitted ones. It asserts the SPREAD too — a fitted
table's columns differ, a bare colgroup's do not — so the six-equal symptom
cannot come back wearing different numbers.
