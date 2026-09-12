# Capture is a row in the table

**Status:** proposed · **Date:** 2026-09-12 · **Origin:** user — capture loses
its popup and its sheet; `+` types a row into the table already on screen.

*The standing flow is [../../capture.md](../../capture.md). The shapes were
measured in [`spikes/2026-09-12-capture-in-table/`](../../../spikes/2026-09-12-capture-in-table/README.md)
and the pick is that rig's **F** — `{ at: "point", settle: "now", tab: true }`
(`f-at-point-on-tab.html:61`): B's placement, B's settle, E's `TAB`. This
supersedes [`2026-08-24-the-capture-doc-is-the-material-doc.md`](2026-08-24-the-capture-doc-is-the-material-doc.md)
for the capture FORM and leaves the capture COMMAND alone.*

## What stands

`+` raises a tag field seeded from the filter (`30-capture.js:24`, `:27`); the
settled tag fetches `GET /capture` and the material-doc sheet opens over the
served draft (`30-capture.js:111`, `20-sheet.js:2182`); `C-c C-c` commits it
through `POST /command capture` (`20-sheet.js:1623`).

The command behind that door is right and is untouched. It answers
`{ok, file, digest, id}` (`Commands.hs:473`, `AGENTS.hs:3751`) with `id` the row
the cursor lands on; a tag mints a blob and no tag appends to the inbox
(`Commands.hs:369`–`:392`); every key meets its own wall, the state through the
destination's own cycle (`stated`, `Commands.hs:454`). The widened cargo road
`{title, state, priority, tags, planning, properties, body}` already composes
without touching a template (`capturedEntry`, `Commands.hs:430`–`:444`), which
is why "templates ignored" costs the server nothing.

What is wrong is the surfaces. The table can already say everything a capture
says — a state, a priority, a tag run, a title — and capture asks for them on
two surfaces of its own, one of which is a whole document editor.

## The law

**A capture is a row.** `+` splices a DRAFT ROW into the table below the row at
point, seeded from the standing filter, with the title cell's editor open.
`TAB` walks the editor to the next editable cell and `S-TAB` back, wrapping;
`RET` from any cell commits; `ESC` from any cell drops the draft whole.

Three laws hold it, and each is a rung in the suite:

1. **A draft always carries an open editor.** The open input takes every key it
   sees (`table-view.js:3917`, `e.stopPropagation()`), so `n` in a draft types
   an `n`. An editor-less draft row would be a row with no id, no span and no
   file that the movement keys could stand on.
2. **No other row can reach a draft.** It is never marked (`markAll`,
   `table-view.js:3429`), never selected (`selectStep`, `:3695`), and its
   reserved id never reaches `targets()` (`00-core.js:526`) or a `/command`.
3. **`ESC` leaves the rows byte-identical.** No file was written, so nothing is
   put back; the draft is spliced out and the count is the count it was.

## The seeding rule

The filter's POSITIVE, PINNED atoms fill the draft. The rule is shipped already
and moves rather than being written: `pinned` (`30-capture.js:48`) — a negation,
a `+` widening, an alternation and a `*meta*` each describe a SET of rows;
`soleValue` (`:58`) — a scalar named twice describes a union and lends nothing;
`filteredTags` (`:53`) — every positive `tag:`, in order.

| the filter pins | the draft wears |
|---|---|
| one `state:` | that keyword, if the destination's cycle has it |
| one `priority:` | that letter, `[#B]` folded to `B` (`:78`) |
| the first `tag:` | the destination: the blob's layer |
| every later `tag:` | the draft's own tag cell |
| anything else | nothing |

**The destination is said in the row** as `→ book` or `→ inbox`, drawn in the
SCHEDULED cell — the one column a capture never fills. The title cell cannot
hold it: `openCellEditor` empties the cell it opens in (`table-view.js:3910`),
so a hint drawn beside the title is wiped the moment the editor arrives. This
also buys back the 98px the spike measured the hint costing the title cell.

**A state the destination's cycle lacks is dropped, and the hint says so.** `+`
asks the cycle door (stage 1) at the same moment it draws the row; the answer
either confirms the seeded state or clears it, and the hint reads
`→ book · NEXT dropped`. The wire never carries the refused state, so
`stated`'s 400 (`Commands.hs:456`) stays exactly as strict as it is for every
other caller — including the MCP `capture` tool (`Mcp.hs:201`).

## The stages

Each stage's oracle is written first and red before the change, per
[CLAUDE.md](../../../CLAUDE.md).

### 1. The read door narrows to the cycle

`GET /capture?tag=NAME` stops answering a draft document and answers
`{"cycle": [...]}` — `draftKeywords` (`Query.hs:1523`) and nothing else. The
page needs the destination's cycle before the reader types; it needs no expanded
template, no cells, no `%?` point and no tag vocabulary.

- **Files.** `Routes.hs:815`–`:838` (`captureView`, cut to a read and a
  `draftKeywords`), `:842`–`:896` (`draftJSON`, `draftCells`, `draftTagsCell`,
  `inheritedIn`, `inheritedTags` — all dead), `Query.hs:2207`–`:2222`
  (`draftTemplate`), `:2327`–`:2400` (`draftPointLine`, `Inherited`,
  `draftSeeded`) and their exports at `:82`–`:89`. `draftRecord` stays: the
  commit road reads back through it (`Query.hs:2412`).
- **The answer already carries the landed id.** `captured`
  (`Commands.hs:473`–`:475`) spells `["ok","file","digest","id"]` and
  `AGENTS.hs:3751` pins it. No write-path change is owed here.
- **Oracle.** `test/TestServe.hs:10460` `captureViewSpec` shrinks from thirteen
  cases to three: `the cycle is the tag's own, in the shape /keywords answers
  in` (`:10495`, kept), `with no tag the cycle is the default one alone`
  (`:10505`, kept), and a new `the answer is the cycle and nothing else`. The
  state-wall refusal at `:10399` keeps its provenance by naming the same door.
- **Estimate.** −190/+55 Haskell; −120/+30 in the suite. Four files.

### 2. The draft row, and paint

A phantom entry in the rows the producer hands the widget, re-spliced on every
paint. `paint()` (`00-core.js:243`) calls `setRows` on every `/headlines`
answer — a WAL tick, a poll, a filter change — and `setRows` resets
`state.rows` and repaints (`table-view.js:5046`), so a draft left out of the
splice is erased. Suppressing the paint instead would leave a stale table under
a live draft.

The widget learns ONE row field, `draft: true`, with four readings: it is
dressed (`rowClasses`, `table-view.js:3064`), never marked (`markAll`, `:3429`),
never stepped onto (`selectStep`, `:3695`), and its cells are the only editable
cells in the table. A per-COLUMN `editable` flag cannot carry this: the main
table mounts with no editable column and no `onEdit` at all
(`00-core.js:196`–`:213`), so opting the columns in would open a dead editor on
every real row's double-click (`table-view.js:3941`).

- **Files.** New `frontend/glue/35-draft.js` (the seeding rule moved out of
  `30-capture.js:39`–`:87`, the phantom, the hint), `00-core.js:243` (splice on
  paint), `Base.hs:117`–`:127` (`gluePartFiles` is the one list),
  `frontend/jsconfig.json:33`, `assets/table-view.js` (+14), `assets/page.css`
  (the accent edge, the `draft` badge, the dashed rule, ghost ink).
- **Three channels say "draft", since colour alone would fail** (the accent
  edge, the badge, the ghost ink under a dashed rule —
  [`2026-08-17-hue-is-never-the-only-channel.md`](2026-08-17-hue-is-never-the-only-channel.md)).
  The seeded cells are literally what the row below wears: the filter pinned
  both.
- **Local filtering cannot eat the draft.** The renderer sets `state.filter`
  only where the producer owns no filtering (`table-view.js:4985`), and the
  shell passes `onFilter`, so `ordered()` (`:2806`) narrows nothing.
- **Oracle.** Browser: `+ splices a draft row below the row at point, with its
  title cell open`; `a headlines answer arriving under a draft leaves it
  standing`. Shell suite: the four seeding filters.
- **Estimate.** +200/−45 across six files.

### 3. The walk — the seam table-view owes

`openCellEditor` reads `Enter` and `Tab` as the same commit and `Escape` as a
bare close (`table-view.js:3915`–`:3920`), and it stops propagation, so no key
inside an open cell reaches the shell's dispatch (`70-shell.js:257`) — which
would decline it anyway, `typing()` being true (`:82`). **A draft's keys cannot
be bound in `Keymap.hs`.** They belong to the editor.

The seam is one mount option, `onCellKey(e, {id, col, value})`, called at the
head of that keydown; a `true` answer means the glue took the key. Over a draft
the glue takes `TAB`/`S-TAB` (walk, wrapping title → state → priority → tags),
`RET` (commit) and `ESC` (drop the whole draft). Every other row keeps the
shipped reading, which costs nothing today because no other row is editable.

The walk accumulates and posts nothing: the closing cell's value is written
into the phantom row's cells, then the next cell opens. `closeCellEditor` calls
`renderRows(true)` (`table-view.js:3892`), so the value must be in the row
before the redraw or the redraw shows it gone.

- **Files.** `assets/table-view.js` (+12, the hook and its typedef at `:82`),
  `35-draft.js` (+60).
- **This is the vendored copy.** `docs/tasks.org:31` records the reconcile with
  `../table-view/web/table-view.js`, whose in-cell editor arrived from upstream.
  `onCellKey` and the `draft` row field are both general — a producer-owned row
  with producer-owned keys — and should go upstream rather than fork.
- **Keymap.** `TAB` stays `org-cycle` in the `table` scope (`Keymap.hs:75`) and
  `+` stays `org-glance-overview:capture` (`:82`); `ESC` stays `keyboard-quit`
  in `any` (`:141`). No binding is added, and no new scope: the draft is not a
  surface. `AGENTS.hs:4084` `bindings` therefore needs no row either — and the
  registry is undiffed against `Keymap.hs`
  ([`docs/bugs/open/2026-09-12-the-spec-counts-two-registries-nothing-walks.md`](../../bugs/open/2026-09-12-the-spec-counts-two-registries-nothing-walks.md)),
  so the draft's keys are stated in `docs/capture.md`'s flow and in the row's
  own footer, rather than in a registry nothing walks.
- **Oracle.** Browser: `TAB walks the draft's cells and wraps, S-TAB walks them
  back`; `n and p in an open draft cell type rather than walk`; `a landed row
  opens no cell editor, and the draft is the only row that does`.
- **Estimate.** +72/−0, two files.

### 4. The commit, and where the row lands

`RET` from any cell folds the open editor's value in and posts
`{name: "capture", args}` — the same args `commitCapture` builds today
(`20-sheet.js:1629`–`:1635`), minus body, properties and planning, which a row
does not have. `settle: "now"`: the commit asks for the server's order at once,
the draft is spliced out, and the fresh row arrives where `sort:` puts it.

Point follows it with no new concept: `arriving = a.id` (`20-sheet.js:1638`) is
spent by `arrived()` (`:2099`–`:2104`) on the next settle, landing point on the
id when it becomes visible. The jump is accepted — a capture has no SCHEDULED,
so the undated tail is where it belongs whatever row it was typed beside (the
spike measured 5 rows / 145px in a seven-row fixture).

- **Files.** `35-draft.js` (+40), `20-sheet.js:1619`–`:1644` (`commitCapture`
  moves out).
- **Oracle.** Browser: `RET from any cell captures, and point follows the row
  the server places`.
- **Estimate.** +40/−26, two files.

### 5. The empty title refuses in place

`RET` on an empty title keeps the draft standing, returns the editor to the
title cell, and writes the refusal into the hint beside the row; the next
keystroke clears it. The words are the shipped ones — `nothing to capture`
(`20-sheet.js:1628`, `:1524`) — and the dress is the spike's: the accent edge
and the dashed rule turn `--g-warn` (`Theme.hs:62`).

The draft STAYS, which is where this parts from the spike: `rig.js` drops the
row with the refusal, and a row that cannot commit is a row the reader would
then have to retype.

- **Files.** `35-draft.js` (+20), `assets/page.css` (+10).
- **Oracle.** Browser: `RET on an empty title keeps the draft and says what it
  wants`, asserting the row count is unmoved and the editor is back on the title.
- **Estimate.** +30/−0, two files.

### 6. The deletions

The popup, the draft sheet, and everything that existed to carry them.

| what goes | where |
|---|---|
| the tag field and its list | `30-capture.js:11`–`:37`, `:88`–`:135`, `Page.hs:89`–`:95` (`#ktag`, `#klist`) |
| `captureShape` (`GET /capture` as a draft read) | `30-capture.js:412`–`:420` |
| the draft's state and tags popups | `30-capture.js:167`–`:207` (`draftIn`, the two branches) |
| the sheet over a draft | `20-sheet.js:2177`–`:2218` (`showDraft`, `draftOf`, `captureWhere`) |
| the draft's commit and its plan settle | `20-sheet.js:1600`–`:1644` |
| ~20 `capturing()` branches | `20-sheet.js:39`, `:189`, `:230`–`:249`, `:286`, `:1049`, `:1522`, `:1556`–`:1590`, `:1916`–`:1926`, `:1977`, `:2024`, `:2060`, `:2255` |
| the capture surface and its `?page=capture` | `70-shell.js:27`–`:28`, `:178` (the `+` handler re-points) |
| the popup registry row | `Page/Popups.hs:45`, `AGENTS.hs:4210`, `:4665` |

`30-capture.js` keeps the value palette, the link door and `planRows`, so it is
renamed `30-palette.js` in the one list that orders the shell
(`Base.hs:117`–`:127`, `frontend/jsconfig.json:33`,
`test/TestSelfContained.hs:23`).

Spec and docs move with it: `AGENTS.hs:3738`–`:3745` (the capture prose),
`:3764`–`:3774` (`CaptureDoor`/`refusalAt` — with no draft door, `NoPlaceholder`
and `TemplateNoHeadline` are spoken at the commit door alone), `:3786`–`:3791`
(`captureRead` becomes `["cycle"]`), `:3989`–`:4001` (the four draft notes),
`docs/capture.md:17`–`:101` (The flow, What the filter lends) and `:150`–`:173`
(the read door), `README.org:100`–`:107`, `CHANGELOG.md`.

- **Estimate.** −330 glue, −45 Haskell, ±150 docs. Eleven files.

### 7. The cases

**Browser** (`test/browser/cases.mjs`) — every law here is a paint fault a
model-reading test cannot see. Retired: `a tagged capture opens as a document,
and C-c C-c mints the blob it drew` (`:4507`), `the inbox jot is + RET the line
RET, and lands the bytes it always did` (`:4690`), `ESC over a draft leaves
nothing behind, on screen or on disk` (`:4736`), the popup half of `an empty
title still has a slot, and its edit stands tight in it` (`:4864`–`:4890`; its
real-doc half at `:4892` stands), and the helpers `captureForm` (`:91`) and
`draftOver` (`:99`). Added:

1. `+ splices a draft row below the row at point, with its title cell open`
2. `the draft wears what the filter pins, and says where it lands`
3. `TAB walks the draft's cells and wraps, S-TAB walks them back`
4. `RET from any cell captures, and point follows the row the server places`
5. `RET on an empty title keeps the draft and says what it wants`
6. `ESC drops the draft and leaves the rows byte for byte`
7. `a headlines answer arriving under a draft leaves it standing`
8. `n and p in an open draft cell type rather than walk`
9. `a state the destination's cycle lacks is dropped, and the hint says so`
10. `a landed row opens no cell editor, and the draft is the only row that does`

**No new fixture is needed.** `test/browser/tree/.org-glance/config/tags/book.org`
is the tree's one tag layer and carries `#+TODO: TODO READING | READ`, so
`tag:book` reaches `→ book` and drives case 9 from a `state:NEXT` filter, while
a tagless filter reaches `→ inbox` — the inbox is created by the capture itself
(`docs/capture.md:25`).

**Haskell.** `captureViewSpec` per stage 1. `promptKeySpec` ("Shell capture and
reschedule", `test/TestServe.hs:2029`) is rewritten over the draft row: its
`GET /capture` stub and `ktag:` driver step go from
`test/fixtures/shell-harness.js:403`–`:427`, `:1472`–`:1480`, and four pure
cases pin the seeding rule against the spike's four filters. Two settings cases
die with the surface: `the capture form says so in the URL` (`:5606`) and `and
closing the capture form takes the parameter off` (`:5610`). The glue-source
pins at `:6821`, `:6823` (`#capture` in the wash lists), `:7065`, `:7067`
(`bareCapture`) and `:7691` (`#ktag`'s 16px guard) lose their subjects.

The capture COMMAND's suites are untouched: `captureSpec` (`:10068`),
`blobCaptureSpec` (`:10170`) and `blobRefusals` (`:10356`) all stay green,
including `a state outside the capture's own cycle is refused, naming the cycle`
(`:10399`) — the wall this design keeps rather than weakens.

## What this touches in the invariants

- **One door** (`docs/invariants.md:19`) — untouched: the capture still leaves
  through `writeSpans` and the new gesture adds no write path.
- **The empty digest is the create pin** (`:33`) — untouched: the blob is still
  written under `""`, which is what makes the capture a create.
- **A fact several readers agree on is spelled in ONE list** (`:271`) —
  `gluePartFiles`, `popups` and the `Surface` roster all lose the capture row
  and gain the draft part; re-spelling any of them at a second site is the
  failure that entry records.
- No entry forbids a new user-visible gesture, and no rule anywhere in the tree
  forbids a native dialog — neither guard exists to break.

## Refused

- **Templates.** A tagged capture's `#+TODO:` cycle still walls the state, and
  its template seeds nothing. The server already ignores it on the cargo road
  (`Commands.hs:430`), so nothing is torn out to achieve this.
- **The tag field.** The destination is read off the filter and stated in the
  row. This removes the reader's veto over an irreversible choice — `→ book`
  mints a blob under that layer, `→ inbox` appends to `inbox.org` — and the
  answer to that is the tags cell being one `TAB` away.
- **A per-cell write before the commit.** The draft's cells accumulate; the
  whole capture goes out at one `RET` through the one command that mints a blob.
  A draft has no id and no span for a per-cell verb to name.
- **A native dialog, and any second surface.** D — the form strip above the
  table — measured as the only shape with no motion at all, and it costs a
  surface of its own plus a width-mirroring loop between two DOM trees
  (spike, D). It stays on the shelf.
- **A filter-lent planning day.** `inherited` lends `scheduled:`/`deadline:`
  today (`30-capture.js:82`–`:85`); the seeding rule here is the three atoms the
  decision names, and the SCHEDULED cell carries the destination hint instead.
- **A draft that outlives `ESC`.** No autosave, no draft store; a capture is
  committed or it never was.

## Oracles, in one list

| stage | the case that is red first |
|---|---|
| 1 | `the answer is the cycle and nothing else` (TestServe) |
| 2 | `+ splices a draft row below the row at point, with its title cell open`; `a headlines answer arriving under a draft leaves it standing` |
| 3 | `TAB walks the draft's cells and wraps, S-TAB walks them back`; `n and p in an open draft cell type rather than walk` |
| 4 | `RET from any cell captures, and point follows the row the server places` |
| 5 | `RET on an empty title keeps the draft and says what it wants` |
| 6 | `ESC drops the draft and leaves the rows byte for byte`; the suite going green with the deleted cases gone |
| 7 | the ten browser cases and the four seeding cases, whole |

## Open questions

- **Where the hint goes when SCHEDULED is narrow.** The draft's hint rides the
  SCHEDULED cell at 118px. `→ book · NEXT dropped` may not fit; the fallback is
  a per-row `note` the widget draws in a reserved right-hand slot, which is a
  second vendored concept and is why it is not the first answer.
- **Whether `+` waits for the cycle.** The row draws at once and the cycle
  answer lands behind it. A `RET` faster than the round trip posts the seeded
  state and can meet `stated`'s 400; the refusal then lands in the hint and the
  draft stands with the state cleared, which is stage 5's machinery reused. The
  alternative — holding the editor until the answer arrives — costs the jot its
  19 keys' worth of immediacy.
