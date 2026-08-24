# Proposal — the planning line opens as a table

**Status:** proposed · **Date:** 2026-08-24 · **Origin:** user — *"`RET` on the
WHOLE planning line summons the table-view editing popup — key/value columns,
one row per SCHEDULED / DEADLINE / CLOSED with their standing values — and `RET`
on a row in that table opens the datetime ghost widget IN PLACE in the table's
value cell."* Raised against the round landing now, which walks the line's
entries on `f`/`b` and leaves `RET` on the line itself deliberately inert.

## The law in one line

**`RET` on the whole planning line raises the line's own RECORD as a
two-column table — one row per SCHEDULED, DEADLINE and CLOSED, each showing the
value it stands at — and `RET` on a row opens the date widget IN the value
cell.** No second widget, no second grammar, no second wall: the table's value
cell is **the widget's THIRD door**, after the planning slot and the pair box.

*Line numbers into `frontend/glue/20-sheet.js` and `frontend/elm/src/Doc.elm`
are read against the working tree of 2026-08-24, where the entry walk is
landing; the function and constant names beside them are what survives a
drift.*

## The line is a record with three fields

The planning line is the one place in a headline whose vocabulary is CLOSED and
KNOWN: `planningKeywords = ["SCHEDULED", "DEADLINE", "CLOSED"]`
(`src-query/Glance/Query.hs:884`), *"the planning line's whole vocabulary"*
(`AGENTS.hs:112`). The server does not hand the pane a line; it hands it a
LIST — `plan : List ( String, String )` (`frontend/elm/src/Doc.elm:88`), lifted
beside the body and carried by every write (`Routes.hs:540` `settledPlanning`).
The LINE is derived, `planningText` joining the pairs back into org's spelling
(`frontend/elm/src/Body.elm:320`).

So the pane already holds the record. What it draws is org's own single line,
because that is what org writes — and every other keyed thing in this shell is
drawn as a table of key and value: the properties drawer, the tags popup
(`Mount "ttable" ["title","on","rows"]`), the config states table
(`AGENTS.hs:4698`). The planning line is the one record with no such view.

Three things follow, and they are the motivation:

1. **A table shows the fields the line has NOT got.** The entry walk reaches
   what is written (`planFiner`, `frontend/elm/src/Doc.elm:332`); the summon
   keys draw a keyword in to stand in (`redraftPlan`,
   `frontend/glue/20-sheet.js:1548`). Neither answers *what could this row
   carry* in one look. Three rows always drawn does.
2. **The key is free and it is the obvious one.** `RET` on the line is inert by
   design as of this round, and says so.
3. **The editor is already built.** The widget, its ghost, its offers, its
   shifted-arrow walk and its wall all shipped
   ([a date is read where a date is owed](2026-08-22-a-date-is-read-where-a-date-is-owed.md),
   the *As delivered* section). This proposal spends them at a third door.

## What ships this round, and the key it leaves free

The round landing now gives the planning row a walk INSIDE it: `planAt` is an
index into the entries the pane draws (`Doc.elm:73`), `f` takes the first entry
and then each next one (`planFiner`, `:332`), `b` steps back and off the first
one to the whole line (`planBroader`, `:357`), the entry at point wears the
sheet's one word for *here* (`"dpv dat"`, `:1801`), and the keyword — never an
index — rides to the shell as `planKey` (`planKeyAt` `:949`, `stateJSON`
`:1150`, mirrored at `frontend/glue/20-sheet.js:12`).

`RET` lands on that axis: over an entry it raises the very widget `C-c C-s`
raises (`planEnter`, `20-sheet.js:104`), keyed by the entry the walk stands in.
Over the WHOLE line it is inert and names the walk — *"`RET` → `f` reaches the
entries — `RET` on one edits it"*. That echo is the door this proposal fills.

**What it replaces is worth stating.** Until this round, `RET` on the planning
row opened the raw line in the paragraph box like any other meta row
(`20-sheet.js:97`), read back by `Body.readPlanning` (`Doc.elm`, the meta arm).
Taking that door off is what makes the line's key free — and this proposal
keeps it off. See *What stays refused*.

## The popup

### What it draws

Two columns and three rows, in org's own order:

| key | value |
|-----|-------|
| `SCHEDULED` | `<2026-08-18 Tue>` |
| `DEADLINE` | `<2026-08-25 Tue>` |
| `CLOSED` | |

- **The rows are `CFG.planning`'s, always all three** (`Glue.hs:16`, the
  server's own list carried rather than respelled). A field the line has not got
  is a row with an empty value, which is already how an entry is absent
  (`AGENTS.hs:4168`).
- **The key column wears the drawer's reserved-token ink** — `org-special-keyword`
  by another name, the dress `viewPlanning` already spends
  (`span [ class "dk" ]`, `Doc.elm:1795`; `.dk{color:var(--tok)}`,
  `Style.hs:296`).
- **The value column is LAST and the table FILLS.** This one is geometry rather
  than dress: the box the widget opens in is laid over the value cell, and the
  ghost rides one space after the last character typed, capped at 46ch
  (`GHOST_CAP`, `20-sheet.js:725`). A last, filling column gives the box the
  pane's whole remaining width — the same reason the slot door runs `tight` to
  the row's edge, *"so the ghost has line to ride on"* (`20-sheet.js:484`).
- **No sort marks, no narrow, no chips.** It is a record of three fields.

### What mounts it — two machineries, weighed

`assets/table-view.js` is vendored from `../table-view` (`AGENTS.hs:5568`,
`BVendored` at `:5701`), and the shell reaches its work by TWO different roads.
Naming which one this popup takes is the whole of the mounting question.

**(a) `listing()` — the Elm `Listing` program wearing the renderer's classes.
RECOMMENDED.** The links popup, the tags popup and the config states table are
one Elm program, one instance per surface (`20-sheet.js:1634`;
*"THE SHELL'S SMALL LISTS ARE ONE ELM PROGRAM"*), and its markup is
*"the renderer's class for class, because the served stylesheet is written
against it"* (`frontend/elm/src/Listing.elm:1-7`) — `tv-sel` on the cursor row,
`tv-box` on the marking cell, `tv-table` around the lot. The mount takes
columns, a hint and a PANE (`listing(host, cols, hint, pane)`), and registers
the pane's scroll against `placeEdit` itself (`20-sheet.js:1652`).

**(b) `TableView.mount(el, view, { inline: true })` — the vendored renderer.**
The `@` picker is the shell's one true call (`frontend/glue/60-refer.js:158`),
*"THE PICKER IS THE TABLE, SHRUNK"* (`AGENTS.hs:5035`) — a table-view mount in
the renderer's own `inline` mode over `GET /refer`, which is `/headlines`' own
pipeline. It arrives with columns, badge hues, the cursor, the filter grammar,
its suggestions and DEL already built.

#### CALL: the planning table is a `listing()`, and the relations picker is the precedent it declines

What (b) buys is a filter grammar over a SERVED VIEW. This table has three
fixed rows, no server behind them, and nothing to filter — the picker's whole
apparatus would be furniture switched off. It costs, besides: the vendored
handle is checked verb by verb at runtime because nothing versions the
agreement with it (`can(tv, ...REFER_VERBS)`, `60-refer.js:169`; *"Nothing
versions the agreement with table-view.js — no handshake, no schema version"*,
`AGENTS.hs:2966`), and a second consumer of that unversioned contract is a
second thing to keep term for term by hand.

What (a) buys is that **`placeEdit`'s cell anchoring already works over it** —
see the next section — and that the popup joins a shape four surfaces already
wear. The renderer's dress comes along either way, because `Listing` draws it.

**The precedent to follow is the LINKS popup, whole:** `tableFrame` in the
markup (`src-web/Glance/Web/Page.hs:74`), `listing("ltable", LCOLS, "", "lpane")`
at first raise (`frontend/glue/40-popups.js:13`), and `LROW` — an edit shape
declaring `cells` and `cols` (`:54-57`) so its box is laid over the selected
row's own cells. The planning table is that, one column narrower and with the
date widget where `LROW`'s two inputs stand.

### What the surface costs

The last two surfaces landed at **18 files** (`mint`) and **24 files** (`refer`)
— measured in
[the popup surface trail joins the one list](2026-08-20-generalize-popup-surfaces.md),
which is this proposal's obvious dependency and would take the number to ~4.
At today's shape, the rows are:

| where | what |
|---|---|
| `Page/Popups.hs:37` | `Popup "planning" "w" "wbox" Sheet True True everyPart` |
| `Page.hs:158` | one `tableFrame "planning" "w"` call, the widget's box as its overlay |
| `Query.hs:562` | `planColumns` beside `linkColumns`/`tagColumns` |
| `Page/Glue.hs:25` | `"pcols" .= planColumns`, carried rather than respelled |
| `Style.hs:493` | `#wpane` joins `#tpane,#lpane` |
| `20-sheet.js:1634` | one `listing("wtable", PCOLS, "", "wpane")` |
| `70-shell.js:17` | one `SURFACES` row — momentary, `off`, `edit`, `shut`, `rowed` |
| `AGENTS.hs:3905`, `:4698` | one `Surface` row, one `Mount` row |
| — | one keydown listener spelling the `momentary()` guard by hand, the cost [one key for a widget](2026-08-15-one-key-for-a-widget.md) names |

The `Sheet` tier rather than `Band`: the value column must hold a stamp and its
ghost side by side.

## The widget in the cell — the third door

### `placeEdit` already has a cells mode, and it is shipped

```js
const span = o.cells && cellSpan(o.cells, o.cols);
const tds  = span && [...tr.querySelectorAll("td:not(.tv-box)")];
```
(`frontend/glue/20-sheet.js:371-373`, the anchor row from
`m.el.querySelector("tbody tr.tv-sel")` at `:360-364`, the left/width from the
first and last named cell's rects at `:406-408`.)

`cellSpan` (`:424`) takes the min and max of the named keys' column indices, so
**a one-cell span is a span** — `cells: ["value"]` gives `[i, i]` and the box is
laid over exactly that `<td>`. Nothing in the geometry needs writing. The tags
popup already runs a one-cell edit this way (`cells: ["title"]`,
`40-popups.js:212`).

### What the door needs beyond that

1. **The box must stand inside the popup's pane.** The overlay boxes are
   `position:absolute` (`Style.hs:320`) and `placeEdit` measures against
   `o.pane`'s padding box (`20-sheet.js:381-383`). `#ddate` lives in `#mdoc`
   (`Page.hs:46`), and `#mdoc` and `#wpane` are both `position:relative`
   (`Style.hs:125-126`, `:493`), so the box works in either — in ONE at a time.
2. **The value column last and filling**, per the section above.
3. **The late paint is already handled.** `openEdit` measures a frame after the
   fill *"the renderer stamps `tv-sel` a frame later"* (`:337`), and the whole-value
   selection survives the redraw because it is re-asserted while the widget is
   VIRGIN (`reselectDate`, `:778`, called off every port push at `:37`). The
   table door is the same kind of late paint the drawn-in planning line already
   was, so this law is spent rather than extended.
4. **Scroll re-placement is free**: `listing()` binds the pane's scroll to
   `placeEdit` in the capture phase (`:1652`).

#### CALL: one box, moved — no second widget

Two ways to have the widget in two panes:

- **A second box** (`#wdate`, `#wghost`, `#woffer`). The ink code is already
  parameterized by element id — `drawGhost(fieldId, ghostId, …)` (`:733`),
  `paintOffers(boxId, …)`, and the menus are records naming their box
  (`dmenu`/`wmenu`, `:607-608`) — so a second instance is mechanically cheap and
  is exactly the kind of cheap that drifts. Two fields, two grammars' worth of
  keys, two `::selection` rules to keep in step.
- **The one box, reparented at summon** — `el("wpane").appendChild(el("ddate"))`
  ahead of `o.fill` and `o.focus`, whose order `openEdit` already fixes
  (`:328-340`). The offers list rides along, hanging off the box at `top:100%`
  (`Style.hs:347`). **RECOMMENDED**: one widget, one ghost, one wall, three
  doors — which is the promise the shipped delivery already makes for two of
  them (*"one widget, both doors"*, `20-sheet.js:530`).

The shape record is what differs per door, over one box id:

```js
const DDATE = { box: "ddate", pane: "mdoc",  anchor: dPlanAt, tight: true,  … };
const WDATE = { box: "ddate", pane: "wpane", mount: () => wmount,
                cells: ["value"], cols: PCOLS, … };
```

`ddating()` widens to membership over the two (`:502`), so every key over the
widget stays one handler; `WDATE` joins `DOCEDITS` (`:506`) so a reload still
refuses to land over an open edit, and the popup's `SURFACES` row declares it as
the surface's `edit`/`shut` rung.

## ONE CLOCK READ PER SUMMON, extended per popup

The widget stamps the day once when it is summoned and reads that stamp for the
ghost, the offers and the wall above the commit (`planHere`, `:796`;
`editDay = () => (edit && edit.row.today) || dateNow()`, `:513`). The reason is
stated where the field was first built: a walk that crosses midnight must not
answer two days for one phrase while the reader is looking at it
(`docs/spikes/2026-08-23-date-widget/rig.js:1148`).

**The table extends the law one surface out: the POPUP takes the stamp, and a
widget opened in one of its cells inherits it rather than reading the clock
again.** A reader who raises the table at 23:59 and walks from SCHEDULED to
DEADLINE at 00:01 must not get two days in one popup, and the three rows are one
reading of one record. The rule generalizes cleanly: **the stamp belongs to the
outermost door that opened, and an inner door inherits it.**

A popup left standing across midnight keeps yesterday's stamp, and the remedy is
the same one the widget has today — `ESC` and raise it again. The stamp is INK
either way: the commit sends the raw typed text and the server resolves it once
against the request's own clock (`Routes.hs:500`, *"ONE CLOCK READ PER REQUEST,
above the row"*), which is the two-resolver law and the reason a drifted ghost
costs a redraw rather than a wrong byte (`AGENTS.hs:5019`).

## Per-key routing: no wall moves

The walls stay exactly where they are, per KEY, and the table asks the same
carried list the widget asks:

- **SCHEDULED and DEADLINE take the transform.** `plannedEntry`
  (`src-web/Glance/Web/Routes.hs:554`) sends a settable key through
  `planningTimestamp` and answers the bytes org itself would write (`:556-558`).
  The widget's commit is `set-planning` with the RAW typed text (`commitDate`,
  `20-sheet.js:837`), and the pane redraws off the server's answer.
- **CLOSED takes reparse alone.** `readsAsTimestamp value` (`Routes.hs:559`) —
  org's own bookkeeping, refused a `set-planning` write outright
  (`settableKeywords = filter (/= "CLOSED") planningKeywords`, `Query.hs:1661`,
  the 400 naming the two at `:1666`; `settablePlan Closed = False`,
  `AGENTS.hs:3422`).
- **The client spells neither list twice.** `DATED = CFG.settable`
  (`20-sheet.js:938`) is the server's own two carried in the config blob
  (`Page/Glue.hs:20`), and it is already what decides whether a value half owes a
  date at all (`valueOwesDate`, `:534`).

#### CALL: the CLOSED row is a readout, and RET on it is refused in words

The standing law is on the record: CLOSED *"is the one planning word the date
widget never opens over and the one whose value keeps the plain stamp wall"*
(`20-sheet.js:934-937`). The table shows the field — a record's editor that
hides a field it will not edit is worse than one that shows it and says why —
and `RET` on that row says what `set-planning` would have said.

The alternative, weighed: **the widget over CLOSED, committing through the split
write.** It is reachable — a key that folds to one of org's three is routed to
the planning line by the pair box today (`pairGoes`, `AGENTS.hs:4787`), and that
cargo meets `plannedEntry`'s reparse arm. What it costs is the ghost. The ghost
previews `planningTimestamp`, and CLOSED never gets that reading, so the field
would offer `18 aug` under a wall that refuses it — a refusal arriving after the
box has shut, which is the whole complaint the widget exists to answer. Making
it honest means a ghost that is off for one row and on for two, plus a second
commit door under one box. Refused for now; if a later round wants CLOSED
typed, the two changes are named here and the ghost stays off.

**The entry walk meets this same seam this round**: `planCommand` already names
a door for CLOSED (`20-sheet.js:787-789`), and `planEnter` hands any walked
entry to `planHere`. One answer should serve both doors, and it is this one.

## ESC, which costs nothing new

`ESC` is three rungs per surface, innermost first — the edit, the narrow, the
surface (`70-shell.js:180`; modelled as `escAt`, `AGENTS.hs:3926`). The popup
fills them in the order the reader expects:

- **`ESC` in the widget returns to the table.** The surface's `edit`/`shut` pair
  cancels the input whole and byte-identical (`cancelEdit`, `20-sheet.js:356`),
  the table stands, the cursor is on the row it was on. Nothing was written, so
  nothing redraws.
- **`ESC` in the table returns to the line.** The surface's `off` shuts the
  popup and point is on the planning row it was raised from — the row the key
  was pressed over, which the widget's summon already keeps as `back`, read
  BEFORE anything moves point (`:804`).
- **The narrow rung is declared absent.** Three rows want no `/`. A declared
  absence rather than an oversight, the shape
  [one key for a widget](2026-08-15-one-key-for-a-widget.md) asks for.

Two things the popup does NOT need: the drafted keyword (`DraftPlan` /
`UndraftPlan`, `Doc.elm:479`) is the SLOT door's alone — a widget standing in
the value's place needs the place to exist, and a widget in a table cell stands
in the popup — and `sole()` closes no non-momentary surface, so the sheet stands
behind the popup exactly as it stands behind the `@` picker.

## Interactions — four doors onto three entries, one widget

The walk and the popup are two doors onto the same three entries, and they
compose:

| door | reaches | ships |
|---|---|---|
| `C-c C-s` / `C-c C-d` | one named keyword, the line drawn in if absent | shipped (`planHere`, `20-sheet.js:796`) |
| `f` to an entry, then `RET` | the entries the line HAS | this round (`planFiner` `Doc.elm:332`, `planEnter` `20-sheet.js:104`) |
| `RET` on the whole line | all three fields at once, absence included | **this proposal** |
| the drawer's pair box, key `SCHEDULED` | the same entry by routing | shipped (`AGENTS.hs:4787`) |

All four end in one box, one grammar, one wall, one clock read. The walk is the
cheap door — no surface, no ESC rung, no mount — and stays the fast path for a
line that already carries what the reader wants to change. The table is the
door for *what does this row say about time*, and the only one that shows a
field the line has not got.

Untouched, and named so a reviewer can check it: `d d` on the planning line
still CLEARS the line through the lists (`AGENTS.hs:4767`); the table pane's own
`C-c C-s` over marked rows still asks through the shipped prompt and reaches the
same grammar at the same door (`schedulePlan`, `70-shell.js:166` → `planRows`,
`30-capture.js:168`); the raw half (`mtext`)
transforms nothing and is edited as a document (`Routes.hs:538`).

## What stays refused

- **No inline editing of the raw planning line.** The paragraph box over the
  planning row goes with this round and does not come back. A line whose bytes
  the reader wants to type is the raw half's job, and the raw half is a document
  the client hands back whole.
- **No fourth grammar.** The table's value cells read what `planningTimestamp`
  reads, previewed by the one client resolver whose only output is ink, and the
  two stay drift-pinned over `test/fixtures/english-dates.json`
  (`AGENTS.hs:5019`). The popup adds no spelling and no offer the widget does
  not already make.
- **No second widget, no second ghost, no second offers list.** Per the call
  above.
- **No CLOSED write through the widget.** Per the call above.
- **No narrow, no sort, no column config, no marking.** A record is not a query.
- **No second consumer of the unversioned vendored contract.** The renderer's
  dress is reused; its mount API is not.
- **No new keyword.** The rows are `CFG.planning`, three, from the server.

## Implementation sketch

- **`src-query/Glance/Query.hs`** — `planColumns :: [Value]`, two entries,
  beside `linkColumns` (`:562`).
- **`src-web/Glance/Web/Page/Glue.hs`** — one member, `"pcols"`.
- **`src-web/Glance/Web/Page/Popups.hs`** — one `Popup` row (`:37`).
- **`src-web/Glance/Web/Page.hs`** — one `tableFrame` call; the widget's box
  moves out of `#mdoc`'s literal markup only if the reparent is done at boot
  rather than at summon (it should not be — see the call).
- **`src-web/Glance/Web/Page/Style.hs`** — `#wpane` joins the pane rule
  (`:493`); the key column's ink is the drawer's, already declared.
- **`frontend/glue/`** — the mount, the `SURFACES` row, one keydown listener,
  the `WDATE` shape, `ddating()` widened, `docEnter`'s inert arm replaced by the
  raise. ~90 lines added, the `planEnter` echo replaced.
- **`AGENTS.hs`** — one `Surface` row, one `Mount` row, one `Note` for the
  per-popup clock stamp and one for the CLOSED readout.
- **Tests.** `TestServe` takes the markup needles and the `Spec.surfaces`
  parity; `test/browser` takes four cases — the table raised off the line with
  three rows and the standing values in them, the widget opened in the value
  cell and laid over that cell's rect, `ESC` walking widget → table → line, and
  `RET` on CLOSED refused with the popup still up.

## Phases

1. **The surface** — the popup, the mount, the three rows drawn from `dplan`,
   `RET` on a row echoing what it would do. Shippable and readable on its own;
   the readout is most of the value.
2. **The widget in the cell** — `WDATE`, the reparent, the commit path already
   built. This is where `placeEdit` is exercised over a one-cell span.
3. **The clock stamp per popup**, if phase 2 did not carry it — one field on the
   raise, and the one a reviewer should read against `editDay`.

## Alternatives considered

- **A form of three labelled fields rather than a table.** Rejected: the shell
  has one small-list program and four surfaces wearing it, and a bespoke form is
  a fifth dress with its own cursor, its own key handling and no `placeEdit`
  anchoring. The record IS a table; the drawer proves it.
- **Editing the line's text in a wider box.** Rejected: it is the door this
  round removed, and it makes the page spell org (`AGENTS.hs`, *"The page holds
  no org parser and must not grow one"*).
- **`TableView.mount` in `inline` mode.** Weighed above and declined for three
  fixed rows; the picker stays the one consumer.
- **Dropping the entry walk once the table lands.** Rejected: the walk is the
  cheap door and costs no surface. Two doors onto three entries is the shape
  `C-c C-s` and the pair box already have.
- **Drawing only the entries the line HAS.** Rejected: it makes the popup a
  slower spelling of the walk. Showing the absent field IS the readout.
- **A fourth planning keyword behind the same table** (`CLOCK:`). Never
  considered: `CLOCK:` is no constructor in the model (`AGENTS.hs:112`), and a
  table over a list is not a reason to widen the list.

## Overlap, stated

- [a date is read where a date is owed](2026-08-22-a-date-is-read-where-a-date-is-owed.md)
  ships the widget, the grammar, the two-resolver law and the wall this
  proposal spends. It leaves CLOSED and date-shaped properties proposed; the
  CLOSED call here is that document's question answered for ONE door and no
  more.
- [the popup surface trail joins the one list](2026-08-20-generalize-popup-surfaces.md)
  is the dependency that turns this surface's cost from a dozen files into four.
  Neither blocks the other; landing that one first makes this one cheap.
- [one key for a widget](2026-08-15-one-key-for-a-widget.md) names the
  per-surface keydown listener and the three-registry naming this popup pays
  once more. It is the reason the `SURFACES`, `Popup` and `Mount` rows are
  listed as three separate costs above.

Inert until reviewed.
