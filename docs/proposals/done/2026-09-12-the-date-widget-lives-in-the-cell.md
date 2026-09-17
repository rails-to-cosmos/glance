# The date widget lives in the cell

**Status:** done · 2026-09-12 · **Date:** 2026-09-12 · **Origin:** user — the date widget
leaves the material document's planning line and opens in the table's SCHEDULED
and DEADLINE cells, for a landed row and for a draft alike.

*The widget as it stands is the material doc's (`20-sheet.js:754` `DDATE`,
README.org:134). Its grammar was measured in
[`spikes/2026-08-23-date-widget/`](../../../spikes/2026-08-23-date-widget/README.md)
and is drift-pinned over `test/fixtures/english-dates.json`. Where the ghost is
drawn was measured in
[`spikes/2026-09-12-date-cell/`](../../../spikes/2026-09-12-date-cell/README.md)
and the pick is that rig's **C** — a one-line strip under the edited row
(`c-strip-under-row.html`), carrying the phrase AND its resolution whole. This
composes with
[`2026-09-12-capture-is-a-row-in-the-table.md`](2026-09-12-capture-is-a-row-in-the-table.md),
whose draft row gains two stops, and does not supersede
[`2026-08-22-a-date-is-read-where-a-date-is-owed.md`](../proposed/2026-08-22-a-date-is-read-where-a-date-is-owed.md),
whose grammar is what moves.*

## What stands

The grammar is right and is untouched. `readsDate` (`20-sheet.js:1304`) reads
ISO, `today`/`tomorrow`, `+3d` in every unit org spells, English (`18 aug`,
`from 18 to 19 august`) and org's own bracket, and a bracket around any of them
picks the stamp's activity. `dateGhost` (`:1394`) has three states and no
fourth — silent, a resolution, a refusal — and `dateWriting` (`:1341`) is what
keeps a half-typed month from being told off. The whole block is pinned vector
for vector against the server's own wall over one shared corpus: the server at
`TestQuery.hs:1978`, the glue at `TestServe.hs:5326`, both reading
`test/fixtures/english-dates.json`.

The server door is right and is untouched. `set-planning` (`Commands.hs:173`)
is `AsksDate`: the keyword is refused ahead of the value (`wantsPlanning`,
`:204`), `unplanned` names the wall (`Query.hs:1897`), a null date takes the
entry off, and `plannedValue` (`Query.hs:1907`) is the one place SCHEDULED and
DEADLINE meet `planningTimestamp` while CLOSED reparses alone. `capture`'s
widened cargo already carries `planning` and meets that same wall through
`plannedEntry` (`Commands.hs:434`, `Query.hs:1918`) — **the draft's dates cost
the server nothing**.

What is wrong is where the widget stands. A date is a CELL the table already
draws (`viewColumns`, `Query.hs:2497`), and the only way to change one is to
materialize the row, walk the doc pane to its planning line, and press
`C-c C-s` there — or to answer a modal prompt (`planRows`, `30-palette.js:44`)
that has no cell to stand in, no ghost and no step keys. The table shows the
fact and cannot edit it.

## The law

**A date is edited where it is drawn.** With point on a SCHEDULED or DEADLINE
cell, `RET` opens the date editor IN the cell and the ghost in a ONE-LINE STRIP
under that row. The typed phrase, `S-<arrows>` stepping ±1d/±7d, `RET`
committing and `ESC` leaving are the shipped widget's, moved. `TAB` walks the
draft's date cells in column order, and a draft's date rides out in `capture`'s
`planning`.

**The cell holds the answer; the strip holds the reading.** A date column is
118px with a 94px text run, and the ghost is 137px — 266px over a range (spike,
"The cell, measured first"). Every horizontal placement bids against the column
measure and loses, so the answer stops using the row's horizontal space: a `<tr>`
one line tall, spliced under the edited row and destroyed with the edit. It
costs **21px of downward motion, once, reversible**, and the edited row and
everything above it hold still.

Six laws hold it:

1. **ONE GRAMMAR, ONE FILE.** `readsDate` and everything under it leave
   `20-sheet.js` for a shared part. Two readers of one phrase is the drift the
   corpus pin exists to catch, and a second copy would pass it while disagreeing
   with the first on the day it was asked against.
2. **THE GHOST IS INK; THE PHRASE IS WHAT TRAVELS.** `editDay()`
   (`20-sheet.js:770`) is stamped when the editor opens, and the ghost, the
   offers and the walls above the commit all read that one day — for INK alone
   (`dateNow`, `20-sheet.js:1322`). **What goes down the wire is the bytes that
   stand in the field**, and the SERVER resolves them once per request against
   its own clock. This is the shipped law, moved into the cell unchanged:
   `commitDate` sends `typed` verbatim (`20-sheet.js:1004`), `planRows` sends
   `date || null` (`30-palette.js:48`), `resolveAsked` spends the request's one
   `today` on `plannedValue` (`Commands.hs:349`–`:353`), and three cases name it
   — `RET sends the raw phrase, never the ghost's own reading`
   (`TestServe.hs:4883`), `RET sends a bracketed phrase raw too` (`:4876`), `a
   date goes to the server as the text that was typed` (`:2046`). **All three
   stay green and untouched**, and the cell is a fourth caller of the same road.
3. **THE CELL REPAINTS AS THE SERVER SPELLS IT.** The wire's date cell is ISO
   (`isoStamp`, `Query.hs:1342`; `hrScheduled`, `:512`), so a commit that
   optimistically paints `<2026-09-15 Tue>` into the cell paints a spelling no
   settle will agree with — and the phrase the reader typed is not a date cell
   at all. **The editor does not optimistically paint the stamp; the settle
   brings the ISO day.**
4. **AN EMPTY DATE COMMITS `null`.** Never `""`. An empty date cell is `""` and
   sits outside every comparison (`docs/invariants.md:290`), and `""` down the
   wire meets `planningTimestamp`'s `T.null want` refusal (`Query.hs:1964`) as a
   400 rather than a clear.
5. **THE STRIP IS SILENT ON THE CELL'S OWN DAY** (`ENTRY-GHOST`, spike finding
   2). The doc pane opens on the planning line's own `<2026-09-15 Tue>`, so
   `dateGhost` has nothing to add and says nothing. **A cell opens on
   `2026-09-15`** — a different spelling of the same day — so the shipped
   `dateGhost` would speak `→ <2026-09-15 Tue>` before a key is pressed, drawing
   the widget's widest thing at rest. The strip stays quiet while the phrase
   resolves to the day the cell already holds, which is the pane's own rule read
   against the cell's own spelling rather than a new one.
6. **THE GRAMMAR IS THE SHIPPED GRAMMAR** (spike finding 8). `englishDate`
   (`20-sheet.js:1252`) reads `day month [year]` and `from … to …`, and has no
   weekday words: **`next fri` refuses in the cell exactly as it refuses in the
   pane.** Weekday words are owed by the SERVER's reader as much as the client's
   and would be a vector in `test/fixtures/english-dates.json` plus a change to
   both halves — they belong to the English-dates proposal, and no placement
   decision may grow a reading behind them.

The client's day is spent on ink and on the WALL ABOVE THE COMMIT, which is what
makes the ghost worth drawing: `dateKey` (`20-sheet.js:995`) refuses a phrase
`readsWhen` cannot read and posts nothing, so the server's own refusal is the
backstop rather than the reader's first news. A phrase both readers take is
resolved by ONE of them, and it is the server.

## The stages

Each stage's oracle is written first and red before the change, per
[CLAUDE.md](../../../CLAUDE.md).

### 1. The grammar moves to a shared part

`frontend/glue/15-dates.js` — the date grammar, the ghost, the offers and the
step, lifted out of `20-sheet.js:1088`–`:1402` (`DATE` through `dateGhost`) plus
`dateStep`/`dateStepped` (`:1021`–`:1031`). **No call site changes.** The glue
parts are concatenated into ONE scope — `00-core.js`, `20-sheet.js` and
`35-draft.js` are bare fragments, and `glueSource` (`TestServe.hs:11829`) folds
them in `gluePartFiles` order — so a declaration moved between two of them is
reached exactly as it was. `15` sorts before `20`, which is all the ordering
this owes.

**The new part is a BARE FRAGMENT and not a wrapped widget** (`05-keys.js`,
`30-palette.js`). The harness REASSIGNS `dateNow` to pin the reader's clock
(`shell-harness.js:1443` `dateon:`); a `var dateNow = Dates.dateNow` alias would
leave the closure calling its own binding, and the pinned-clock act would go
quietly dead with every date case still green. `TestSelfContained.wrappedWidgets`
(`:16`) therefore gains no row.

`STAMP` stays reachable to `pairRefused` (`20-sheet.js:1421`) and `leadTyped` /
`NEW_HINT` stay `00-core.js`'s — one scope, both directions.

- **Files.** New `frontend/glue/15-dates.js`, `frontend/glue/20-sheet.js`
  (−330), `Base.hs:117`–`:127` (`gluePartFiles`, the ONE list that orders the
  shell), `AGENTS.hs:6090` (the spec's copy, diffed at `TestSpec.hs:1776`),
  `frontend/jsconfig.json:30`–`:40`.
- **Oracle.** `TestSpec.hs:1773` `the shell is ten parts, folded in their
  numbered order` is red on a part list of eleven and is renamed with its
  subject; `TestSelfContained`'s jsconfig diff is red until the fourth roster
  agrees. The corpus pins (`TestServe.hs:5326`, `:5344`) and `dateWidgetSpec`
  (`:4782`) stay green untouched — that they do is the proof the move is pure.
- **Estimate.** −330/+340 glue, +4 roster lines, one renamed case. Six files.

### 2. The widget learns editable keys and the strip

Two mount options and one caret rule in `assets/table-view.js`.

**`editableKeys: string[]`** — the column keys a STANDING row's cells may be
edited in. `editCell` (`:4084`) gates on `!producerRow(id) && !columnEditable(col)`
today, and the column's own `editable` flag (`:4025`) is the wrong shape: the
server emits no `editable` on any column (`columnsFor`, `Query.hs:2557`), and
setting it would also make the header editable (`editHeader`, `:4131`) and open
a dead editor wherever the producer has no verb. A LIST rather than
`editable(row, key)`: the answer is read during paint for the dress and the
cursor, once per cell per row per repaint, and a predicate would cross into the
producer 600 times a WAL tick. A producer's own row stays editable whole.

**`onCellInput(e, cell) => string|null`** — asked on every `input` in an open
cell, and once at the open. The answer is drawn by the widget as a **strip: a
`<tr class="tv-strip">` with one `colspan`ed cell, spliced directly under the
edited row, at the table's full width.** `null` or `""` draws no strip at all
(law 5's silence at entry is the producer answering nothing). The strip is
destroyed when the editor closes, on `RET` and on `ESC` alike, and the rows
below come back to the pixel.

**The strip, and no `td` slot.** The spike measured every horizontal answer
against the column: the ghost is 137px and a date cell's text run is 94px, so
in-cell cuts it by 81px at rest and the neighbour cell — which is the OTHER date
column at the same 118px — cuts it by 43px (spike, A and B). The strip draws it
whole in 1340px, and echoes the PHRASE too, which scrolls out of a 94px input in
every placement (finding 7). One `<tr>`: no negotiation against the column
measure, and no conditional on which columns the view draws.

**The refusal draws in the same strip.** `✗ not a date` is what `dateGhost`
answers for a phrase that will not read (`20-sheet.js:1394`), so it is the same
string through the same seam wearing the refusal's ink. This is a different slot
from the row-level `refused` (`noteOf`/`noteCell`, `:3161`–`:3174`), which rides
the last column a row carries no cell for: a landed row fills every column, so
`noteCell` would draw the note over a fact and take it off screen.

**The caret rule.** `holdEditor` (`:4107`) folds an open input's value into its
row and puts the caret back, for a `producerRow` alone. A standing row's open
editor must survive a repaint the same way, and must NEVER write the typed
phrase into `r.cells` — a landed row's cells are the store's. So the held value
rides in the handle, the row is left as it was, and the strip is re-spliced with
the editor it belongs to.

- **Files.** `assets/table-view.js` (+52/−6: the two options, the strip's splice
  and destroy, the hold), the typedefs at `:51`–`:56`, `:91`–`:118`, `:130`, the
  widget's own sheet (`tv-strip` beside `tv-producer`/`tv-refused` at `:1807`,
  20px on the surface ground with the accent edge down its leading cell),
  `docs/tasks.org:339` (the upstream list — the branch forks the vendored copy,
  and `make sync-renderer` clobbers it silently). That list also carries a stale
  `hint` row for a `Row` field the capture-row branch has since dropped; it is
  corrected in the same pass.
- **This is general and goes upstream.** "The keys a standing row's reader may
  edit" and "a line the producer draws under the row being edited" name no
  glance concept, and the strip is the widget's to place because only the widget
  knows the row's width and where the row is.
- **Oracle.** Browser: `a date cell opens an editor and a title cell does not`;
  `the strip under the row shows the ghost whole, and ESC takes it away`
  (asserting the strip's text is uncut and that every row below is back to the
  pixel it began on); `a headlines answer arriving under an open date cell
  leaves the caret where it stood`.
- **Estimate.** +52/−6 widget, +14 typedef, +12 widget CSS, +8 tasks.org. Two
  files.

### 3. The real row's date editor

`frontend/glue/36-date-cell.js` — the in-cell editor both a landed row and a
draft use. `00-core.js:211` passes ONE `onCellKey: cellKey`, which takes a date
key on any row and hands every other cell to `draftKey` (`35-draft.js:153`).

**The column cursor is shipped; no cursor is built here.** The spike's finding
12 records "the cell cursor does not exist" and that is wrong about the app: the
widget carries `state.selCol`, answers it at `getSelection().col`
(`table-view.js:5349`), dresses it `tv-colsel`/`tv-cell-sel` (`:3208`–`:3210`),
and `f`/`b` with their `l`/`h` and arrow aliases are bound to
`next-column`/`previous-column` in the `table` scope (`Keymap.hs:43`–`:45`),
handled by `moveCol` (`00-core.js:493`, `70-shell.js:122`–`:123`). **So `RET`
over a date cell already has a referent**, and the spike's third seam is not
owed. What the rig built for itself, the app had.

`RET` on a SCHEDULED or DEADLINE cell opens it, stamping `editDay()` at that
moment; the cell's ISO value is what the input opens on, wholly selected, org's
own default — **and the strip stays dark**, per law 5, until the phrase names a
day other than the one the cell already holds. Every keystroke after that
redraws it through `onCellInput`.
`S-<right>`/`S-<left>`/`S-<down>`/`S-<up>` are `dateStep` (`15-dates.js`) over
`dateStepped`, which writes the stepped stamp INTO THE FIELD (`dateAdjust`,
`20-sheet.js:1032`–`:1040`) — the reader sees it and it travels because it is
what stands there, which is law 2 and no exception to it.

`RET` commits `fire(b, "set-planning", [id], {keyword, date: typed || null})`
(`20-sheet.js:1893`) — **the field's own bytes**, an emptied field committing
`null`. `ESC` closes, the strip goes, and the cell is the cell it was. An
unreadable phrase — `next fri` among them, per law 6 — refuses IN PLACE: the
editor stands, **the strip wears `✗ …` in the refusal's ink**, the word is
`readsWhen`'s own (`r.why`, `20-sheet.js:995`–`:1000`), no command is posted and
no row is touched. This is the empty-title wall generalised without amendment
(spike finding 10): something the reader can fix, standing where they can fix
it.

**`commitDate` and `planRows` are untouched.** The cell is a fourth caller of
the road they already take, so `set-planning`'s argument keeps ONE spelling
across four surfaces rather than gaining a second — which is what
`docs/invariants.md:271` asks of a fact several readers agree on.

**`RET` becomes column-sensitive in the table**, which is what this costs.
`RET` is `org-glance-overview:materialize` in the `table` scope
(`Keymap.hs:54`), so a reader standing on a date column no longer materializes
from `RET` and presses `b`/`h`/`<left>` first (`Keymap.hs:43`). The table
already carries a column-sensitive key: `^` (`:52`) reads `state.selCol` to pick
the column it sorts by.

The doc pane keeps its `#ddate` box, its offers menu and its placement whole
(`DDATE`, `20-sheet.js:754`). What the cell borrows is the grammar and the
keys — the pane's surface stays the pane's.

- **Files.** New `frontend/glue/36-date-cell.js` (+150), `00-core.js:211` (+6),
  `Base.hs`, `AGENTS.hs`, `frontend/jsconfig.json` (the fourth roster row). No
  change to `20-sheet.js`'s commit road and none to `30-palette.js`.
- **Oracle.** Browser: `RET on a SCHEDULED cell opens the date editor and sends
  the phrase`; `an unreadable date refuses in the cell`; `S-<right> steps the
  ghost a day`; `ESC leaves the cell's date as it was`; `an emptied date cell
  clears the entry`; `the committed cell waits for the settle rather than
  painting a stamp`. Haskell: `TestServe.hs:2046`, `:4876`, `:4883` and `:4804`
  all stay green untouched — that they do is the proof the cell took the road
  and did not open one.
- **Estimate.** +150 new, +6 glue, +60 cases. Five files.

### 4. The draft's date stops

`DRAFT_CELLS` (`35-draft.js:136`) gains `scheduled` and `deadline`.
`draftWalk` (`:141`) already filters `cols` in header order, so **the two stops
land in column order with no walk of their own** — `title → state → priority →
scheduled → deadline → tag` is whatever the view draws. The date cells open the
stage-3 editor; every other cell keeps the draft's plain input.

`RET` inside a draft's date cell commits THE CAPTURE, not the date: a draft has
no id and no span for a per-cell verb to name, which is the capture-row law's
own refusal. The open cell's bytes are folded into the row first, as every other
cell's are.

`draftArgs` (`:207`) gains `planning: [["SCHEDULED", "<typed>"], ...]` — **the
phrase, per law 2**, the two entries in keyword order, each present only where
its cell holds something. **Empty is no line**: an absent key writes no planning
line, and `capturedEntry` composes what it is given (`Commands.hs:430`–`:444`).

**`capture`'s planning road already resolves through the same wall as
`set-planning`, and no server change is owed.** Checked: `capture` is
`AsksNothing` (`Commands.hs:160`), so it takes no `askStamp` — but
`capturedEntry` resolves the planning itself at `Commands.hs:434`,
`traverse (first snd . plannedEntry (Time.localDay (Time.zonedTimeToLocalTime
now)))` over `agPlanning args`, and `plannedEntry` (`Query.hs:1918`) is
`plannedValue` (`:1907`) with the key named in the refusal — the very function
`set-planning`'s `AsksDate` arm calls (`Commands.hs:353`). So a draft's `+3d`,
`18 aug` or `today` is read by one grammar against one day, the key outranks the
value, and a value no timestamp parser reads back is refused rather than
written. `now` is ONE `getZonedTime` per request, taken before the entry is
composed (`Commands.hs:377`, `:397`) and shared with the creation stamp
(`captureStamp now`), so the batch cannot span midnight.

*The one wrinkle, stated and not fixed here:* that read is spelled
`Time.localDay (Time.zonedTimeToLocalTime now)` rather than `Base.today`
(`Base.hs:78`–`:82`), which is the same expression under a second name where
`docs/invariants.md:137` asks for one spelling. It is still one read per
request, and it must be a `ZonedTime` because the creation drawer wants the time
of day. A `todayIn :: ZonedTime -> Day` in `Base` would collapse the two; it
belongs to that invariant's own sweep.

The draft's own ghost refuses ahead of the wire, so the commit door's 400 stays
as strict as it is for every other caller including MCP `capture` (`Mcp.hs:201`).

- **Files.** `35-draft.js` (+18), `docs/capture.md:43`, `:56` (a row DOES carry
  a planning line now; the "undated tail" sentence and the "no planning line"
  clause both go), `AGENTS.hs:3960`–`:3968` (the draft notes) and `:5424`–
  `:5432` (the refusal).
- **Oracle.** Browser: `the draft's SCHEDULED stop lands the row where the sort
  puts it` — F's premise, which the capture-row spike measured as a 5-row /
  145px jump precisely because a capture had no date. Haskell: the seeding cases
  stay green (a day is still no fact the filter lends, per that proposal's
  refusal).
- **Estimate.** +18 glue, ±20 docs. Four files.

### 5. The keymap, the docs and the cases

**`C-c C-s` / `C-c C-d` split on the marks.** With rows MARKED they keep
today's bulk prompt (`planRows`, `30-palette.js:44`). With NOTHING marked they
open the cell editor on the row at point, in that keyword's own column.

Why that line and no other:

- **`targets()` already splits exactly there** (`00-core.js:533`–`:537`):
  marked, else the row at point. The rows the key takes do not change, so
  `takesRows`'s `Marked` for both commands (`AGENTS.hs:4210`–`:4211`) stays
  true — it says WHICH ROWS, and only the surface differs.
- **A set of rows has no cell to stand in.** That is the whole reason the prompt
  exists, and the same reason the pane has a handler of its own for one command
  (`scheduleHere`, `Keymap.hs:114` — "one command, two handlers").
- **One row then has ONE date surface.** `C-c C-s` and `RET` on the cell are the
  same act, so the ghost, the step keys and the refusal live in one place rather
  than two that must be kept agreeing.

`schedulePlan`/`deadlinePlan` (`70-shell.js:176`–`:177`) carry the branch. No
binding is added, no scope is added, and `Keymap.hs:100`–`:103` and
`AGENTS.hs:4097`–`:4098` are unchanged.

- **Docs.** `README.org:134`–`:142` (the `C-c C-s` paragraph gains the table
  half, the strip and the marked/unmarked split; the key table at `:28` stands),
  `docs/capture.md:43`, `:56`, `:175`, `:195` (the draft's `planning`),
  `docs/query.md` untouched — no filter grammar moves. `AGENTS.hs` gains two
  notes: the date grammar is the shared part's, and the reading rides a strip
  under the row because the cell is ISO (ten characters) while the ghost spells
  org's stamp.
- **Cases.** Browser (`test/browser/cases.mjs`), the six of stage 3 and the one
  of stage 4, plus `C-c C-s with rows marked raises the prompt, with none opens
  the cell`. Shell suite: the ghost vectors re-pointed at `15-dates.js`'s
  declarations — which is a path change and no vector change, the corpus being
  the corpus.
- **The fixture is shipped.** `test/browser/tree/plans.org:5` `drv-plan` carries
  `SCHEDULED: <2026-08-18 Tue>` and a DEADLINE for the step and the ESC cases;
  `test/browser/tree/unset.org:11` `drv-unset-one` is scheduled and undated for
  the clear case; `dates.org`'s three `:datefix:` rows are the doc pane's and
  stay its. **No new fixture is needed.**
- **Estimate.** +12 glue, ±120 docs, +180 cases. Seven files.

## What this touches in the invariants

- **One clock read per request** (`docs/invariants.md:137`) — untouched, and the
  reason the phrase travels. The server reads `today` once before any row
  (`Commands.hs:349`) and `AsksDate` resolves once for the whole request, so a
  marked set cannot span midnight; `capture` reads its own `ZonedTime` once
  (`:377`, `:397`) and resolves through the same `plannedValue`. Resolving on
  the client would move that authority to a clock the server has never read.
  The client's `editDay()` (`20-sheet.js:770`) is a SECOND read for INK alone,
  pinned at open so the ghost and the offers cannot disagree with each other
  mid-edit.
- **The empty cell sits outside every date comparison** (`:290`) — the reason
  law 4 exists. The editor commits `null` and never `""`; `Filter.hs:644` and
  `table-view.js:729` keep their guard, and `*empty*` stays the one name for
  those rows.
- **A fact several readers agree on is spelled in ONE list** (`:271`) —
  `gluePartFiles` gains two rows across four rosters (`Base.hs:117`,
  `AGENTS.hs:6090`, `frontend/jsconfig.json:30`, and the count in
  `TestSpec.hs:1773`); re-spelling any of them at a fifth site is the failure
  that entry records. It is also why the cell joins `set-planning`'s existing
  argument road rather than opening a second one beside it.
- **One door** (`:19`) — untouched: a date still leaves through `set-planning`
  and `writeSpans`, and no write path is added.
- **The client issues one drift-locked write per file, awaited** (`:308`) —
  untouched: `fire` posts one command and awaits it, as `planRows` does today.

## Refused

- **A native date picker.** `<input type="date">` reads no `+3d`, no `18 aug`,
  no `from 18 to 19 august` and no bracket, so it cannot spell what the filter
  and the planning wall both already take; it draws a second calendar the reader
  did not ask for; and it has no slot for a ghost. The grammar is the feature.
- **A second parser.** One grammar, one file, one corpus pin. A cell-sized
  reader that "only needs ISO and `+Nd`" is the drift `TestServe.hs:5326` and
  `TestQuery.hs:1978` exist to catch, and it would pass them both by never
  being driven.
- **Resolving on the client.** The ghost is ink. Sending
  `readsDate(typed, editDay()).stamp` would hand the day to a clock the server
  has never read, split `set-planning`'s argument into two spellings across four
  surfaces, and cost three cases their subject (`TestServe.hs:2046`, `:4876`,
  `:4883`). The phrase travels; the server resolves it once per request.
- **The ghost inside the cell** (spike, A). 94px of text run against a 137px
  ghost, 266px over a range: the answer is cut by 81px on an untouched cell and
  the phrase scrolls out of the input besides. A stays on the shelf as the thing
  to build if the ghost's SPELLING is ever shortened — `2026-08-18 Tue` at 100px
  comes within 6px — and that is a decision about the ghost, made on its own.
- **The ghost in the neighbour cell** (spike, B). A date column's neighbour is
  the other date column at the same 118px, so the cut is 43px rather than none;
  it only looks right where the neighbour happens to be Tags at 190px, and a
  placement that is correct in one column order and wrong in another has to be
  explained every time the view changes.
- **Weekday words.** `next fri` refuses, per law 6. Growing `englishDate` to
  make a placement work would leave the server's own reader behind and the
  corpus unpinned.
- **An offers menu in the cell.** `dateOffers` paints into `#dwoffer` through
  the shared `CompletionMenus` widget, a box the pane owns and a cell has no
  room for. The cell gets the strip. The pane keeps its menu.
- **A cell cursor.** The table has one already (`getSelection().col`,
  `f`/`b` and their aliases, `Keymap.hs:43`–`:45`); building a second is the
  spike's one correctable claim.
- **A per-cell write from the draft.** The draft's dates accumulate and ride out
  in the one `capture` that mints the blob. A draft has no id and no span.
- **Optimistic repaint of the committed cell.** The wire's cell is ISO
  (`Query.hs:1342`), the file's value is org's stamp, and painting either from
  the client is a third spelling. The settle brings the cell.
- **A new binding.** `RET` is the table's own key and `C-c C-s`/`C-c C-d` are
  org's; `Keymap.hs` and `AGENTS.hs`'s `bindings` gain no row.

## Oracles, in one list

| stage | the case that is red first |
|---|---|
| 1 | `the shell is ten parts, folded in their numbered order` (TestSpec), red on eleven; the jsconfig diff (TestSelfContained) |
| 2 | `a date cell opens an editor and a title cell does not`; `the strip under the row shows the ghost whole, and ESC takes it away` |
| 3 | `RET on a SCHEDULED cell opens the date editor and sends the phrase`; `the strip is dark on the cell's own day`; `an unreadable date refuses in the cell`; `next fri refuses like every other phrase the grammar lacks`; `S-<right> steps the ghost a day`; `ESC leaves the cell's date as it was`; `the committed cell waits for the settle rather than painting a stamp` |
| 4 | `the draft's SCHEDULED stop lands the row where the sort puts it` |
| 5 | `C-c C-s with rows marked raises the prompt, with none opens the cell`; the ghost vectors re-pointed at `15-dates.js` |

## Open questions

**Where the ghost lives is answered.** The spike drew A, B and C and measured
each against the column; C — the one-line strip under the edited row — is the
only one that can draw the answer, the only one that can still show a long
phrase, and its whole cost is 21px of downward motion that `ESC` returns to the
pixel. It is the law above and stage 2's seam.

What is left open is smaller and belongs to later work rather than to this
design:

- **The title column as the fill column.** Finding 5's other half: the title
  column always has slack, so a ghost drawn at its right edge would have
  hundreds of pixels and move nothing. It puts the answer three columns from the
  question, which is why the strip is shipped first; it is the variant to build
  if 21px turns out to bother anyone.
- **The ghost's spelling.** `→ <2026-08-18 Tue>` is 137px because the arrow says
  *resolves to* and the brackets say *active*, both of which are org's. Dropping
  them to 100px brings the in-cell placement within 6px of fitting. That is a
  decision about what the ghost SAYS, owed on its own and never smuggled in as a
  layout fix.
- **Weekday words.** `next fri` reads nowhere today. Adding it is a corpus
  vector plus both readers, per law 6, and belongs to the English-dates
  proposal's phase 2.

## Landed

**done · 2026-09-12.** Five stages, each oracle red before its change.

- **1. The grammar moves to a shared part.** `frontend/glue/15-dates.js:1`–`:335`
  carries `DATE` through `dateGhost` plus `dateStep`/`dateStepped`, lifted whole
  out of `20-sheet.js` (−328). No call site changed: the parts are one scope and
  `15` folds before `20`. The four rosters agree — `Base.hs:120`,
  `AGENTS.hs:6135`, `frontend/jsconfig.json:32`, and `TestSpec.hs:1773`'s own
  title, now *twelve parts*. The corpus pins stayed green untouched, which is the
  proof the move was pure.
- **2. The widget learns editable keys and the strip.** `editableKeys` reaches
  the cell gate through `keyEditable` (`assets/table-view.js:4128`, gated at
  `:4135`); `onCellInput` is asked at the open and on every keystroke and its
  answer is spliced as `tr.tv-strip` (`:4173`–`:4183`), dropped by `dropStrip`
  (`:4154`) on `RET` and `ESC` alike. The strip is no row — `standing`
  (`:3194`), the order, the marks and the selection never see it. The caret rule
  is `holdEditor` (`:4201`), which now holds a STANDING row's editor without
  writing the typed line into `r.cells`. Typedefs at `:55`, `:101`–`:130`; the
  sheet at `:1828`–`:1847`.
- **3. The real row's date editor.** `frontend/glue/36-date-cell.js:1`–`:153`:
  `DATE_CELLS` (`:12`), `dateCellDay` (`:27`), `dateCellNote` (`:53`),
  `dateCellKey` (`:70`), `dateCellStep` (`:85`), `commitDateCell` (`:102`),
  `openDateCell` (`:124`), `planKey` (`:140`), `openPlanCell` (`:148`). The
  shipped mount gains `onCellKey`, `editableKeys` and `onCellInput`
  (`frontend/glue/00-core.js:211`–`:216`). `commitDate` and `planRows` are
  untouched, so `set-planning`'s argument keeps ONE spelling across four
  surfaces.
- **4. The draft's date stops.** `DRAFT_CELLS` (`frontend/glue/35-draft.js:137`)
  gains `scheduled` and `deadline`; `draftWalk` (`:142`) filters `cols`, so the
  two stops land in the header's own order with no walk of their own; `draftArgs`
  (`:209`–`:220`) carries `planning`, one entry per date cell that holds
  something, THE PHRASE and never the stamp. No server change was owed —
  `plannedEntry` (`Query.hs:1918`) already resolves `capture`'s planning against
  the request's one clock read.
- **5. The keymap, the docs and the cases.** `README.org:83` (the key table's
  `RET`), `:86` (the two sequences), `:103` (the draft's date stops, and the
  dropped hint), `:146` (the cell, the strip, the step keys and the split);
  `CHANGELOG.md`'s Unreleased; `docs/capture.md:13`–`:68`, `:158`;
  `docs/commands.md:122`–`:142` (the key table gains the cell, the two split
  sequences, the strip and the draft's `planning`);
  `AGENTS.hs:5358`, `:5409`, `:5427`, `:5446`; `docs/invariants.md:303` (*the
  phrase travels; the ghost is ink*). Eight browser cases, `cases.mjs:5698`–
  `:6058`, 108 → 116.

### Departures

- **The producer owns the strip's silence.** Law 5 asks the strip to stay dark
  on the cell's own day. The widget draws whatever `onCellInput` answers and
  `null`/`""` draws nothing, so the silence is `dateCellNote`'s
  (`36-date-cell.js:53`) and the widget learned no date rule — which is what
  made the seam general enough to go upstream.
- **The `RET` seam is the handler, never a binding.** The design reads as though
  `RET` itself became column-sensitive; what shipped is
  `HANDLERS.materializeRow` (`frontend/glue/70-shell.js:139`), which reads the
  shipped column cursor and materializes only where the column draws no date —
  the way `^` reads `state.selCol`. `Keymap.hs` and `AGENTS.hs`'s `bindings`
  gained no row, as asked.
- **The draft's POST is not drivable in the shell rig.** The rig's table stub
  opens no in-cell `<input>` (`test/fixtures/shell-harness.js`), so `onCellKey`
  — the one dispatch a key inside a cell reaches — is never called there. The
  cell's own POST body is read in the browser instead (`RET on a SCHEDULED cell
  opens the date editor and sends the phrase`, `cases.mjs:5698`), and a
  glue-source pin stands in for the shell.
- **The ghost vectors needed no re-pointing.** Stage 5 expected the shell suite's
  date vectors to follow the declarations into `15-dates.js`. They read
  `glueSource` (`TestServe.hs:11825`) — every part concatenated in
  `gluePartFiles` order — so no case spells a path and the move cost them
  nothing.
- **`planningHelp` stands as it was** (`Keymap.hs:157`, pinned at
  `TestServe.hs:11748`). `C-c C-s` still takes "the marked rows, or the row at
  point": `takesRows` is unchanged and only the surface splits, so the help is
  still true.
- **2026-09-12 review: the strip lost to the pane's own box as an overlay —
  offers included.** Seen live, the shipped cell had no completion: `dateOffers`
  paints into `#dwoffer`, a box the pane owns, and the design refused it a cell
  ("An offers menu in the cell", above). One widget with a hole in it is two
  widgets. What replaced it is that very box, laid over the cell as an
  OVERLAY — so the cell's placement negotiates against no column measure either,
  which was the strip's whole argument, and the reading, the offers, the step
  keys and the walls are one code path. Where the overlay goes was then measured
  in [`spikes/2026-09-12-date-overlay/`](../../../spikes/2026-09-12-date-overlay/README.md),
  whose **B** is the pick: the box stands IN THE CELL'S OWN PLACE — same top,
  same left, same height — and grows right, the ghost running on over the
  neighbour as a tail; only the OFFERS turn over at the window's foot, the box
  itself never moving because it is the cell. `#ddate` moved out of `#mdoc` to
  the page's root and is placed against the viewport (`Page.hs:54`,
  `page.css:752`, `placeEdit`'s `fixed`/`over` branches); the door is
  `openDateBox` (`20-sheet.js:1080`). `editableKeys`, `onCellInput`,
  `refreshStrip` and `tr.tv-strip` are gone from the widget — none had been
  upstreamed — and it gained `cellRect(id, col)` and `closeEditor()` instead.
  The draft's refusal moved from the strip to the echo pill. Laws 1–4 and 6
  stand unchanged; **law 5 is retired**: the cell opens on ISO and the shipped
  ghost speaks over it, which is the pane's own reading applied without an
  exception rather than a silence rule of its own.
