# Bug — a continuation typed into a wrapped list item is out of sight

**Status:** fixed · **Reported:** 2026-08-18 · **Browser:** Chromium
· **Surface:** the sheet's doc pane, `M-RET` in an open item edit
· **Fixed in:** `frontend/glue/20-sheet.js`

## Symptom

`M-RET` inside an item whose own line WRAPS adds the newline and the box does
not grow, so the line just added sits under the pane's own text: the reader
types into a row they cannot see. On an item that fits one line the same key
grows the box as it should, which is why this went unseen.

It shows up in the suite as a case that fails about half its runs:

```
not ok 32 — a continuation lands under the item's own text, checkbox and all
     waited 8000ms for the box to grow by the line M-RET added (last: false)
```

## Steps to reproduce

```
make browser-check ONLY="a continuation lands under"   # repeat; ~2 in 4 were red
```

By hand: open the sheet over `drv-plan`, walk into the list to
`- [ ] кириллическая задача…` — the item long enough to wrap in the pane —
press `RET` to edit it and then `M-RET`. The caret goes to a second line the
box has no room for.

## Evidence

- The floor the box stands on is the ROW's: `placeEdit` sets the box's height
  from the row it covers (`frontend/glue/20-sheet.js`, `s.height = a.height`),
  and the row's own floor is `min-height:calc(var(--g-doc-rows, 0) * lh)`
  (`src-web/Glance/Web/Page/Style.hs`).
- `--g-doc-rows` counted org's NEWLINES alone —
  `el("dtext").value.split("\n").length`. A wrapped one-line item already
  stands two rows tall, so a floor of 2 changed nothing when the value gained
  its second line.
- Measured over the wrapped item, before the fix — the box never moves:

```
before M-RET  lines 1  rows 3  boxH 63  rowH 63
after  M-RET  lines 2  rows 3  boxH 63  rowH 63
```

  and after it:

```
before M-RET  lines 1  rows 2  boxH 42  rowH 42
after  M-RET  lines 2  rows 3  boxH 63  rowH 63
```

- The case that should have caught it walked with unguarded presses — it read
  the cursor immediately after each `n`, and a key is answered a turn later, so
  it usually stepped PAST the wrapped item and onto `- [X] выполненная…`, which
  fits one line and grows correctly. That is why it was intermittently green.

## Why not the obvious measure

Reading `el("dtext").scrollHeight` directly does not work: `scrollHeight` never
reports less than the element's own height, and the field is stretched to the
box, which is stretched to the row. The first reading inflates the count and
every later one repeats it — a ratchet that reports three rows for a two-row
item and then never changes. The field is collapsed (`flex:none; height:0`) for
the measurement and put back, and the textarea's own padding comes off the
reading.

## The fix

`sizeDocEdit` takes the rows the text OCCUPIES — org's newlines as the floor,
the wrapped height where that is larger, still capped at `DOCROWS`. The case
now waits for the cursor to move between presses and for the box to be placed
over its row before it measures, so it lands on the wrapped item every time and
measures a settled baseline.

A page with no layout measures nothing, so the reading is guarded and org's own
newlines stand alone there — the node shell harness models no layout and has no
`getComputedStyle`, which is the same guard `placeEdit` already carries.

Measured: **2 red in 4 isolated runs before, 0 in 4 after**, plus clean full
runs. `BREAK=cont-floor` (`#mdoc .de.dat{min-height:0}`) takes the room away
again and turns the case red, so the case is known to catch it.
