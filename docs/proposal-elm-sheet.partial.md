# Proposal — the materialize sheet in Elm

**Status:** partial — steps 1–4 DONE (both panes are Elm); step 5 not taken · **Date:** 2026-08-09 · **Origin:** user, asking whether
the split glue is ready for Elm · **Depends on:**
`docs/proposal-widget-files.partial.md`, whose step B split the shell and whose step C
made the sheet one component

## A correction this plan is built on

Earlier notes in that proposal said the sheet has ZERO synchronous renderer
calls. That was measured on `10-document.js` ALONE, before the merge, and
repeated about the merged file. **`20-sheet.js` made 28 synchronous accesses into
table-view handles**, and that is the exact property deciding whether Elm's
ports can carry it. Split by whose handle:

| handle | accesses | what it is |
| --- | --- | --- |
| `table.*` | 20 | the MAIN table — `select`, `getSelection`, `getMarked`, `getFlagged`, `markedCount`, `toggleMark`, `selectStep`, `nextPage`/`previousPage`, `pageInfo` |
| `m.*` in `flagKey` | 4 | POLYMORPHIC — whichever mount the surface's shape names |
| `pmount` | 4 | the PROPERTY PANEL's own mount — `el`, `setRows`, `select`, `clearFlags` |

The three are different problems and the plan treats them separately.

## What the port would take

`assets/glue/20-sheet.js`, 1111 lines after step 1. It owns `editing`, `raw`, `base`,
`baseProps` and the document model (`drows`, `dat`, `dcol`, `dgrain`,
`dparent`, `dlines`, `dcursor`, `dflags`, `dlinks`). Thirteen DOM ids:
`modal`, `sheet`, `mdoc`, `dlist`, `mprops`, `mtext`, `dtext`, `dtin`, `pkey`,
`pval`, `mwhere`, `mfile`, `mlog`. Its wire surface is small — four
`GET /headline`, three `POST /headline`, five `/command` through `fire`.

It is already model-view-update shaped: `drows`/`prows` are the model,
`drawDoc`/`repaint` the view, the key handlers the update.

## The three problems, in the order they must be solved

### 1. Table machinery is living in the sheet, and it is not the sheet's

Twelve table-level definitions arrived with the panel and stayed:
`flagKey`, `mark`, `marking`, `move`, `turnPage`, `endStop`, `targets`,
`focusedId`, `titleOf`, `pager`, `pageNow`, `sorts`. They account for
**all 20 `table.*` calls**. None of them is about the sheet; they are the
TABLE's marks, flags, movement and paging, sharing a file with the sheet
because step B cut by line count.

**Move them out first**, into `00-core` beside the other floor helpers, and
`flagKey` with them — the gesture is generic over a surface's shape and belongs
on the floor beside `flagsOn`.

**DONE.** 114 lines moved, the line multiset proving a pure move, suite green
at 1735. The sheet's `table.*` accesses went 20 → 1 and its polymorphic 4 went
with `flagKey`, leaving **five**: `unmark`'s single `table.toggleMark`, which
sits with `fire` because `fire` re-pins `editing.digest`, and the panel's own
four.

This step was worth doing whether or not Elm ever happens.

### 2. The property panel is a table-view mount, and Elm cannot read one

After step 1 the sheet still drives `pmount`: `el`, `setRows`, `select`,
`clearFlags`, and reaches its flags through `flagKey`'s `getFlagged`. The
writes are fire-and-forget and a port carries them fine. **`getFlagged` and
`el` are READS**, and Elm's ports are
asynchronous one-way — a `subscribe` cannot answer a question inside the update
that asked it.

Two ways, and the first is better:

**(a) Elm draws the panel itself.** The panel is a two-column key/value list
with a cursor and a flag set — `PCOLS` is literally `[{key},{value}]`. Elm
draws that in a screenful, owns the flags as ordinary model state, and
`getFlagged` stops existing. What is lost is the shared widget: CLAUDE.md notes
the panel is a table-view mount because "the renderer is the app's ONE list
widget". That rule buys consistency of look and of key handling; both are
reproducible, and the panel is the one mount whose rows are not records of the
same shape as the table's.

**(b) Keep the panel in JS** beside an Elm document pane. Rejected: it puts the
language boundary through the middle of one widget, and the two panes share
`dirty()`, the flush and the sheet ladder.

Taking (a), **the sheet has no synchronous renderer dependency at all**, and
that is the gate.

### 3. The remaining interface must be measured, not estimated

**Do not start the Elm until that number is on paper.** Every estimate in this
programme that was not measured came in high — and this one did too. The
estimate written here was "seven forward dependencies plus the floor". The
measurement is **50 out and 41 in**, and it moved the verdict: see "The sheet is
not an island" below.

## The port itself

**Toolchain.** `elm make --optimize` producing one JS file, committed as a
build input exactly as `assets/table-view.js` is, refreshed by a `make` target
beside `sync-renderer`. No new build step in `cabal build`; the binary stays the
whole deployment and `--assets` still serves a directory. This is the one part
of the earlier objection that dissolves: the repo already has the pattern.

**Shape** (as designed, before the stop). One Elm program, `Browser.element`,
mounted on `#modal`. Its model
is the sheet's: the entry, `raw`, the two baselines, the document rows and the
property rows, the cursor and the flags. Its view draws both panes. Its update
takes keys.

**Ports out** (Elm → JS, fire and forget): `writeSubtree` (the flush),
`runCommand` (the five `fire` sites), `materialize` (re-open at a child),
`closeSheet`, `logLine`, `echo`.

**Ports in** (JS → Elm, subscriptions): `opened` (a `GET /headline` answer),
`written` (a receipt, carrying the digest), `refused` (a 409 or a 400),
`keyPressed` for the keys the shell dispatches rather than the sheet.

**What must NOT become a port**: any question the update needs answered inside
the same tick. This was written expecting step 2 to leave none. It leaves two,
and the port round trip turns out to cost a whole macrotask — measured below.

**Keys.** The sheet's key handling is its own listener today, ahead of the
dispatch. In Elm it is `Browser.Events.onKeyDown` inside the program, with the
shell's dispatch standing down while the sheet is up (it already does — every
`table` binding is dead while `typing()`). `keyName`'s physical-key rule must
be ported exactly: `e.code` for `KeyA`–`KeyZ`, `e.key` otherwise. It is 20
lines and `05-keys.js` is the reference.

## What the suite does about it — THE PLAN WAS WRONG HERE, AND THE BLOCKER IS NOW GONE

**Superseded by the harness's own DOM.** `shell-harness.js` now owns a real node
tree and a selector engine, Elm's virtual DOM starts and renders under it, and
the descendant / `:not` / alternation / `closest` / `matches` shapes are asserted
by `domSpec`. Step 3 of the staging below is DONE. What follows is the finding
as it stood, kept because the two facts under it still hold: the renderer is a
stub, and the 56 panel assertions read that stub rather than the page.


This section claimed the port is safe because "~200 harness-driven cases in
`TestServe` press keys and read back the DOM", so "the same cases must pass
against the Elm sheet with no edit". **Both halves are false, and step 2 found
it in an hour.**

`test/fixtures/shell-harness.js` has NO DOM. `globalThis.document` is a
hand-written object of nine members over a `Proxy` that answers `""` to every
property and returns itself from every call; `querySelectorAll` is `() => []`.
And `globalThis.TableView` is a STUB — `makeMount` keeps rows and a selection
in memory. The panel probes read that stub, never the page:
`panel() = cellsOf(pan, ["key","value"])` walks `inst.own`, `patAt() = curOf(pan)`
reads its selection index. **56 assertions in `TestServe` read it.**

So an Elm panel is invisible to the cases meant to protect it — and it does not
even start. Run under the harness's own globals:

```
TypeError: _VirtualDom_doc.createTextNode is not a function
```

## Two toolchain results, both measured

**`npx --yes elm` works** — elm 0.19.2-0, `elm/compiler`, nothing installed,
the same ephemeral shape as the Makefile's `npx --yes -p typescript tsc`. The
`elm.json` must say `0.19.2`; `0.19.1` is a hard version-mismatch refusal.

**`Platform.worker` runs under the fake DOM and `Browser.element` does not.**
Ports carry values both ways. But the round trip takes a full MACROTASK, which
was measured rather than assumed:

```
shadow IMMEDIATELY after send: null
shadow after a microtask     : null
shadow after a macrotask     : {"seen":1,"got":{"k":1}}
```

So Elm cannot back state JS reads in the same tick — and the panel has two such
paths: `addProperty` → `repaint` → `openRow` → `patAt`, and `drawProps` →
`repaint` → `edited()` for `baseProps`.

**Sizes**, against a 387 KB payload (`table-view.js` 242 KB + glue 145 KB):
`Browser.element` hello-world 108 KB, `Platform.worker` 62 KB. That is the
runtime, before any panel code.

## The sheet is not an island — step 3, measured

Measured with a scope-accurate walk over acorn's AST (`bindings` per scope,
free identifiers per scope, member expressions counted at the object alone), so
locals and parameters are not mistaken for reaches:

| direction | count |
| --- | --- |
| names `20-sheet.js` reaches in other parts | 50, across all five, plus 13 globals |
| names other parts reach IN `20-sheet.js` | **41 distinct** |

The second number is the one that decides it, and its shape more so: **25 of the
41 are reached by `50-settings.js` and 25 by `70-shell.js`.** The settings sheet
borrows the materialize sheet's whole apparatus — `openEdit`, `shutEdit`, `hop`,
`flagPress`, `mountOnce`, `activeSheet`, `dirty`, `sync`, `note`, `show`,
`drawProps`, `props`, `planning`, `DTITLE`, `DPARA`, `edit`, `editing`, `raw` —
which is CLAUDE.md's own design: "ONE BUTTONLESS SHEET, and there are two of
them: the materialize sheet and the settings sheet run the SAME ladder, written
once".

Porting `20-sheet.js` therefore drags `50-settings.js` (643 lines) with it. The
unit is ~1750 lines and two surfaces, not 1111 lines and one.

## Staging — the gate moved

1. **Move the table machinery out.** **DONE** (`9f2ea2c`) — renderer accesses
   28 → 5, the file 1225 → 1111.
2. **Elm draws the property panel.** **DONE.** The 56 stub assertions cost far
   less than the estimate: `panel()` and `patAt()` are HARNESS-side helpers, so
   repointing those two at the DOM left every assertion's expected value
   untouched. Four cases changed — two source pins, and the two that asserted
   mount OPTIONS, which became `pinits`/`pfills`.
3. **A harness with a real DOM.** **DONE**, and hand-written rather than
   vendored: jsdom would have been 7 MB over 21 direct dependencies in a repo
   with no `package.json`, where what the page actually needs is a node tree,
   six selector shapes and `attributes`. Elm's virtual DOM renders under it.
4. **The document pane.** **DONE.** 268 lines out of the shell, 1458 lines of
   Elm in — the scanner, the parse, the splice, the cursor and the grain. The
   same-turn reads were four, and each took the panel's own answer: `docBody`
   for an edit, `docTook` for a delete, `docSaid` for a grain key's echo, and
   `soon` for the anchor, since Elm pushes a port before it paints.
5. **The ladder** is NOT taken, and the finding that argues against it stands:
   it is shared with `50-settings.js`, 25 of the sheet's 41 exported names being
   that file's. Porting it moves two surfaces, not one.

## The recommendation

**Port the panel; stop there.** That is what was done, and both costs the plan
feared came in low: the harness's real DOM was hand-written rather than
vendored, and the stub assertions needed two HELPERS repointed rather than 56
cases rewritten. What still argues against taking the rest, all measured:
126 KB of runtime;
a unit of ~1750 lines spanning two surfaces because the settings sheet shares
the ladder by design; and an async boundary through code that reads its model
synchronously.

What the port was reaching for has largely been banked by cheaper means already:
`make check-glue` type-checks the shell for real, the widgets are separate
files, three are wrapped with enforced boundaries, and this step put the table's
machinery back where it belongs. Those are the wins that were available; this
one is not, at this price.

The finding that stands on its own merits and outlives the Elm question is
step 3's: **the harness tests a stub, not the page.** That is worth knowing
whatever happens next.

## Open decisions, now moot

Kept because they record what was decided before the stop: reuse the `.tv-*`
class names and stylesheet; keep `keyName` in `05-keys.js` and port already-named
keys; let the Elm output join the asset directory; exclude it from `check-glue`
and let `elm make` be its own check.
