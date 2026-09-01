# Spike — an org table in the material doc is a table-view mount

**Date:** 2026-09-01 · **For:**
[an org table in the material doc is a table-view mount](../../docs/proposals/proposed/2026-08-26-an-org-table-is-a-table-view-mount.md)
(proposal #1). Sibling proposal, not spiked here:
[the planning line opens as a table](../../docs/proposals/proposed/2026-08-24-the-planning-line-opens-as-a-table.md).

**Open `index.html`.** Click into the pane, then walk it:
<kbd>n</kbd>/<kbd>p</kbd> rows, <kbd>f</kbd>/<kbd>b</kbd> cells; <kbd>f</kbd> on
the table block enters the ROW, <kbd>f</kbd> again a cell, <kbd>b</kbd> unwinds
cell → row → out (and off a row clears its highlight). <kbd>+</kbd> adds a row
(on a row) or a column (on a cell). <kbd>RET</kbd> edits a cell's RAW text;
<kbd>p</kbd> above row 1 climbs to a column's header and <kbd>RET</kbd> names it.
The **second** table has NO header line — <kbd>p</kbd> summons an ephemeral one.
Click cells, links and headers with the mouse. The **point** and **event** lines
read the round-trip back; **theme** judges both palettes.

## What it proves

The proposal's boundary: **Elm keeps the rows, a custom element keeps the DOM.**
The doc holds one `<glance-table>` host; its `connectedCallback` parses the org
table text and mounts the vendored `assets/table-view.js` — the REAL renderer,
loaded from the repo — and Elm would render only that host, never its children.
The table is one composite block in flow, between a headline's paragraph and a
paragraph after it, so its height and the prose flowing past are visible.

Both directions, driven headlessly by `probe.mjs` (CDP) and asserted:

- **Elm → widget.** The doc walk drives `tv.select(id, col)`. `n`/`p`/`f`/`b`
  after entering land on `table r1 c2`, and `tv.getSelection()` agrees
  (`{id:"r0", col:1}`). Both axes.
- **Widget → Elm.** A mouse click on a cell reports back: the host reads
  `tv.getSelection()` and bubbles a `glance-cell` event the glue would forward
  as a `SelectCell` port message. A click on row 4 cell 1 reports exactly that.
- **Links free.** A `[[glance:cc33][church end]]` cell renders as a `tv-link`;
  clicking it fires `tableview-link` — the `o` door — with the target.
- **Sort is presentation.** A header click reorders the draw; the spike reads
  `getSort()` back and writes nothing.

## Adding rows and columns — the `col = null` affordance

A UX decision made against the running spike. `+` adds — but a row or a column,
and the reader should never have to remember which key does which:

- **On a ROW** (`col = null` — no column selected) `+` adds a **row** after it.
- **On a CELL** (`col` an index) `+` adds a **column** after it.

This falls out of a state table-view ALREADY models and ALREADY draws
distinctly: a whole-row selection (`select(id, null)`) is one wash across the
row; a cell selection (`select(id, col)`) is two bands crossing at the cell. So
what `+` will do is legible from the selection itself — the row wash says "row",
the crossing says "column" — with no mode indicator to invent. The screenshots
show the two: the row-wash (dark) reads *+ adds a row*, the crossing (light)
reads *+ adds a column after*.

It refines the proposal's walk. The proposal has `f` land on "row 1 cell 1"; to
make the no-column state reachable, **`f` lands on the ROW first** (whole,
`col = null`), and `f` again drills into cell 1. `b` unwinds cell → row → out.
`n`/`p` change rows and keep whichever of the two the point is in.

The write each `+` implies, named so the build weighs them:

- **A row add is a one-line splice** — a new `| … |` line, one new `item` leaf,
  the door the proposal already describes for a cell edit.
- **A column add rewrites every row's line** — a cell inserted in each data row
  plus the header and the hline. Lawful (all inside the subtree) but NOT the
  proposal's "writes that line alone"; it is the one mutation here that touches
  every row's bytes, and the build should treat aligned whole-table writing as
  its home rather than a later opt-in.

In the spike a row add is a cheap `applyDelta` insert and a column add rebuilds
through `setView` (columns are not delta-editable) — the same split the port
faces: an inserted row is an incremental draw, an inserted column redraws.

## Editing a cell and a column name

Two edits, one box — `placeEdit`'s shipped cells-mode overlay, in the spike a
bare input laid over the cell's rect (outside the renderer's DOM, so a redraw
under it does not wipe it).

- **`RET` on a cell edits its RAW text.** The box opens on `[[glance:aa11][see
  the plot]]`, not the rendered "see the plot" — the link is editable and the
  page never parses org. Commit rewrites THAT row's line alone (`tableCells` →
  `|`-join, one cell replaced), the proposal's one-line door.
- **A column name is the header cell.** `p` above row 1 climbs to the header of
  the SAME column (`n` comes back down, `p` above it leaves the table); `RET`
  there names the column. A rename rewrites the header line alone.
- **A headerless table shows NO header until you climb to it.** `p` above row 1
  summons an EPHEMERAL header — ghosted, dashed — that exists only while the
  point is on it; `RET` naming a column MATERIALIZES it (writes the header line
  and its hline) and the header becomes real and permanent. Walk away without
  naming and nothing was added. A blank column never shows its key: table-view
  falls back to `col.key` for an empty header (`k0`…), so an unnamed column
  carries a space and reads as unnamed.

Settled with the user against the running spike: header reached by `p` above row
1 (org's own top line); the box shows RAW org for markup cells; a headerless
table's header is EPHEMERAL until a column is named (no promote/refuse step);
`b` off a row clears the row highlight (the whole table, no row picked).

## What it surfaced for the port — the renderer's share

Two vendored (`../table-view`) changes the proposal already names, sharpened by
building against the real handle:

1. **No native selection event.** table-view sets the cell on a click
   (`setSelected`) but emits only `tableview-action` and `tableview-link` — no
   `tableview-select`. The spike works around it by reading `getSelection()` in
   a `click` listener on the host; the clean port wants an **`onSelect(id, col)`
   callback** in the mount options. Name it beside the embedded flag.
2. **Embedded-in-flow is mostly free; the FURNITURE is the flag.** A plain
   (non-`inline`) mount already fills to content — `.tv-root{max-height:100%}`
   over an auto-height host draws every row with no scroll of its own — so
   "every row in flow" needs no upstream change. What the `embedded`/`window:0`
   option is actually FOR is dropping the renderer's page furniture: the title,
   the persistent filter box, the `N rows` hint, and the card frame. The spike
   previews that by hiding `.tv-bar`/`.tv-hint` and stripping `.tv-root`'s frame
   in CSS; the real switch makes it the mount's default and summons the filter
   only when asked.
3. **No public deselect.** The handle has `select(id, col)` but no
   `select(null)` / `clearSelection()`, so the whole-table state (or a table left
   behind) cannot be told to show no washed row. The spike masks the selection
   grounds with CSS — which beats the renderer's own rAF repaint — but the clean
   port wants a deselect on the handle.
4. **A blank header shows the column key.** `renderHead` draws
   `col.header || col.key`, so an empty header leaks `k0`… . The port must give
   an unnamed column a real blank (a space, or an explicit empty-allowed header)
   — for a table with no header line, or a freshly added column.

## Out of the spike, deferred to the build

The real edit box is `placeEdit`'s cells-mode overlay anchored over the `td`
(the spike uses a bare input) plus the one-line splice and the header-line
write; the `m.col` model in Elm; header-vs-hline detection past the simple case;
`#+TBLFM` drawn inert; aligned whole-table writing for a column add. The spike
is the boundary, the two axes, and the edit/add DOORS; the write path is the
proposal's, built failing-test-first.

Throwaway. The real host node is an Elm-rendered `<glance-table>`, its
`(row, column)` an attribute, its moves a port message the glue forwards.
