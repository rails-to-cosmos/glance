# Proposal — an org table in the material doc is a table-view mount

**Status:** proposed · **Date:** 2026-08-26 · **Origin:** user — *"material
doc: can we display org tables using table-view widget?"*

## The law in one line

An org table is drawn by the renderer the main table is drawn by, inside
the block it occupies, and the doc's walk, splice and mirror stay Elm's:
the widget is the DRAW of a composite stop, never a second model.

## Today

A table is one composite stop (`comp:table`) over `item` leaves, one per
row (`Scan.Table`, "a table is one stop, then its rows"). The draw aligns
it cosmetically — `Body.tableRow` pads every column to its widest cell,
rules dropped — `RET` on a row edits its raw `| a | b |` line, and the
splice writes that line alone ("editing a table row splices that line and
nothing else"). Cells are not stops: `f`/`b` have nothing finer than the
row, sorting is unthinkable, and a link in a cell is text.

## What the widget brings, and what already exists for it

- **The two axes the reader knows.** In the main table `n`/`p` walk rows
  and `f`/`b` cells (design-rhymes, "the table teaches both axes"); an org
  table inside the doc gets the same dialect for free.
- **Cell edit.** `placeEdit`'s `cells` mode already lays a box over a
  `td` span for the main table; a cell of an org table is the same box.
- **Sort, narrow, links.** Presentation-only sort, the `/` narrowing the
  popups already use inside a mount, `tv-link` cells with `o`.
- **Precedent.** The refer picker mounts table-view INSIDE the sheet
  (`60-refer.js`, `TableView.mount(el("rmount"), view, { inline: true …})`)
  — the renderer's compact mode; the links and tags popups are mounts with
  edit overlays. One look, one key language.

## The model: an org table as a view

- **Columns**: the first row when an hline follows it (org's own header
  convention); else `c1 … cn`. All `text`, `sortable` (presentation only).
- **Rows**: the data rows; id `<row-id>` of the `item` leaf (positional,
  like block ids), cells keyed by column. `tableCells` is the splitter
  already.
- **Hlines**: group boundaries. Drawn as a separator if the renderer has
  one, else dropped from the view; the file keeps them (they are not rows
  and never were stops).
- **`#+TBLFM`** and formulas: outside the region; drawn inert (`dg`), never
  through the widget.
- **Cell text**: org markup as the doc draws it (`drawText` — links become
  link cells).

## The boundary: Elm keeps the rows, a custom element keeps the DOM

Elm's model is untouched: the composite and its `item` leaves stay the
walk's stops, so folds, `n`/`p`, the splice, `bodyText` and the harness
mirror (`.dat`, `docAtNow`) all stand. What changes is the composite's
DRAW: instead of aligned lines, Elm renders one host node —
`<glance-table>`, a custom element carrying the view as a property and the
point (row, column) as attributes. The element mounts table-view in its
`connectedCallback`, refreshes on attribute change, unmounts on disconnect;
Elm never renders children into it, so its virtual DOM never fights the
mount. The repo's first custom element; the standard Elm–JS interop for a
widget that owns its subtree. Selection flows both ways: Elm's point
selects the widget's row/cell; the widget's own moves (a click, the
renderer's keys) report through a custom event the glue forwards as a port
message (`Select id` today; a `SelectCell` twin).

## Keys

- Outside: unchanged. The table is one stop; `f` enters it.
- Inside: `n`/`p` rows — the `item` stops, Elm's walk as is; **`f`/`b`
  cells** — point gains a column (`m.col`, cleared on leaving the table);
  `b` at column 0 climbs to the composite (today's broader).
- `RET` edits the CELL: the cells-mode box over the `td`, one line. Commit
  rebuilds the row's line from its cells with the one cell replaced
  (`tableCells` → `|`-join) and writes it through today's door — that line
  alone. The written line is RAGGED where the cell grew; the draw
  re-aligns as it does now, and org aligns the file on its next `TAB` in
  Emacs. Writing the whole table aligned (other rows' bytes, inside the
  subtree, lawful) is a later opt-in.
- Sort and `/` narrow inside the mount are presentation: nothing writes;
  ESC clears; leaving the table resets both.

## Out, named

Formula evaluation and `#+TBLFM`, column attributes, hline editing,
spreadsheet recalculation — displayed inert. A cell spanning lines does
not exist in org.

## Alternatives

- **An Elm-native `<table>`**: cheaper boundary; loses sort, narrowing, the
  link cells and the edit box, and grows a second table dialect in Elm —
  rejected, it breaks the one-table-language rhyme the widget exists for.
- **A mount outside Elm's tree, positioned over the block** (the picker's
  way): dodges the custom element; a table must sit in flow, take its
  height and scroll with the doc — rejected.

## The renderer's share

`inline` mode caps its window and summons a filter onto a strip. An
embedded table wants every row in flow, its own filter only when asked, no
scroll of its own — likely one option on `mount` (`window: 0` / an
`embedded` flag) in the vendored table-view; a sibling-repo change, named
here so the port carries it.

## Oracle

- TestServe (stub mounts are counted): a body with a table mounts one
  table-view; the kinds walk is unchanged (`comp:table`, `item` …); `f`
  from the composite lands on row 1 cell 1; `RET` on a cell writes that
  line alone, every other byte identical; a link cell opens with `o`.
- Browser: the mount sits inside the composite's block and scrolls with
  the doc; `f`/`b` cross cells and `b` climbs out; the edit box covers the
  `td`; a sort reorders the draw and writes nothing.

## LOC, roughly

+150 Elm (host node, column model, `m.col`), +120 glue (the element,
event→port, cell commit), +40 Style; the renderer option upstream. The
aligned-line draw (`tableRow`) survives as the fallback where no renderer
is mounted — the stub, a page served without the asset.

Inert until reviewed.
