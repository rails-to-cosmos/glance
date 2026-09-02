# Bug — the column-name edit box is a thin sliver over a blank header cell

**Status:** fixed · **Reported:** 2026-09-02 (live use) · **Surface:** the
material doc's org-table column-name edit over a BLANK header cell — a
headerless table's ephemeral header, or a headed table's not-yet-named column.

## The symptom

Naming a column, no text input is visible: the box opens over the header cell
but is a thin sliver, so the reader types blind and only sees the result once
`RET` applies. A header cell that ALREADY holds a name (e.g. "Street") edits in
a full-height, clearly-bordered box; a BLANK one does not.

## The mechanism

The column-name box (`DHEAD`, block-mode) takes its height from the header
cell's own rect (`placeEdit`: `s.height = a.height`, `frontend/glue/20-sheet.js`).
A blank header cell — the ephemeral ghost header, or a column added but unnamed
— holds only an empty line box, so its `th` is ~10px tall (measured: box height
10.5px against the doc's 21px line), and the box inherits that sliver. A `th`
with text is a full line, which is why a named column edits fine.

Evidence: measured on `drv-noheader`'s ephemeral header — `th` rect height 11px,
`#dpara` height 10.5px, versus 32px over a named `th`.

## The fix

Give a header cell a real height — `height: var(--g-doc-lh)` on
`#mdoc glance-table thead th`, which a table cell treats as a MINIMUM, so a
named cell is unaffected and a blank one stands a full line. The box, measuring
that `th`, then opens full-height. A browser case pins it: open the name box on
a headerless table's ephemeral header and assert it stands at least a line tall.
