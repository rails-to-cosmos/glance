# Proposal — the materialize sheet in Elm

**Status:** proposed · **Date:** 2026-08-09 · **Origin:** user, asking whether
the split glue is ready for Elm · **Depends on:**
`docs/proposal-widget-files.md`, whose step B split the shell and whose step C
made the sheet one component

## A correction this plan is built on

Earlier notes in that proposal said the sheet has ZERO synchronous renderer
calls. That was measured on `10-document.js` ALONE, before the merge, and
repeated about the merged file. **`20-sheet.js` makes 27 synchronous calls into
table-view handles**, and that is the exact property deciding whether Elm's
ports can carry it. Split by whose handle:

| handle | calls | what it is |
| --- | --- | --- |
| `table.*` | 20 | the MAIN table — `select`, `getSelection`, `getMarked`, `getFlagged`, `markedCount`, `toggleMark`, `selectStep`, `nextPage`, `pageInfo` |
| `pmount`/`m` | 7 | the PROPERTY PANEL's own mount — `setRows`, `select`, `flagRow`, `unflagRow`, `getFlagged`, `clearFlags` |

The two are different problems and the plan treats them separately.

## What the port would take

`assets/glue/20-sheet.js`, 1225 lines. It owns `editing`, `raw`, `base`,
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

**Move them out first**, into `00-core` beside the other floor helpers. This is
the same relocation the eleven earlier ones took, it is verifiable the same way
(pure move, no test changes), and it drops the sheet's synchronous renderer
calls from 27 to 7 — all of them its own panel's.

This step is worth doing whether or not Elm ever happens.

### 2. The property panel is a table-view mount, and Elm cannot read one

After step 1 the sheet still drives `pmount`: `setRows`, `select`, `flagRow`,
`unflagRow`, `getFlagged`, `clearFlags`. The writes are fire-and-forget and a
port carries them fine. **`getFlagged` is a READ**, and Elm's ports are
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

After steps 1 and 2, measure the sheet's dependencies the way the other widgets
were measured (`docs/proposal-widget-files.md`). Today it names seven forward
dependencies (`config`, `configSheet`, `settings`, `momentary`, `docTargets`,
`promptNow`, `landing`) plus the floor. The port's port-API is exactly that
list plus the four accessors it already hands out — `editNow`, `dlinksNow`,
`docCursor`, `docRowById` — and the named operations `docClear`, `docFill`,
`docRestore`, `checkboxHere`.

**Do not start the Elm until that number is on paper.** Every estimate in this
programme that was not measured came in high.

## The port itself

**Toolchain.** `elm make --optimize` producing one JS file, committed as a
build input exactly as `assets/table-view.js` is, refreshed by a `make` target
beside `sync-renderer`. No new build step in `cabal build`; the binary stays the
whole deployment and `--assets` still serves a directory. This is the one part
of the earlier objection that dissolves: the repo already has the pattern.

**Shape.** One Elm program, `Browser.element`, mounted on `#modal`. Its model
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
the same tick. After step 2 there are none — that is what step 2 is for.

**Keys.** The sheet's key handling is its own listener today, ahead of the
dispatch. In Elm it is `Browser.Events.onKeyDown` inside the program, with the
shell's dispatch standing down while the sheet is up (it already does — every
`table` binding is dead while `typing()`). `keyName`'s physical-key rule must
be ported exactly: `e.code` for `KeyA`–`KeyZ`, `e.key` otherwise. It is 20
lines and `05-keys.js` is the reference.

## What the suite does about it

This is the part that decides whether the port is safe, and the answer is good.
The sheet's behaviour is pinned by ~200 harness-driven cases in `TestServe`
that press keys and read back the DOM — `insheet`, `onTable`, `keyed`. Those
tests do not know how the sheet is implemented; they drive a served page in
node and assert what the elements say.

So the port is verifiable in the way that matters: **the same cases must pass
against the Elm sheet with no edit.** Where a case pins a line of glue source
(`holdsAll` over the served script), it pins the JS implementation and must be
retired or repointed — those are countable in advance and should be counted
before starting.

## Staging

1. **Move the table machinery out** (§1). Pure relocation, suite green, no
   Elm. Sheet's renderer calls 27 → 7.
2. **Elm draws the property panel** — inside the existing JS sheet, as a first
   Elm program with a narrow surface, replacing `pmount`. Proves the toolchain,
   the committed-output pattern, and the port shapes on ~150 lines rather than
   1225. Sheet's renderer calls 7 → 0.
3. **Measure** the remaining interface (§3) and write it down.
4. **Port the document pane** into the same program.
5. **Port the ladder and the opening**, and delete the JS sheet.

Stages 1–3 are worth doing on their own merits and leave the repo better if
stages 4–5 never happen. Stage 2 is the real decision point: if writing a
two-column list plus flags in Elm is unpleasant against this suite, that is the
signal to stop, and it costs a day rather than a month.

## Open decisions

1. **Does the sheet keep table-view's look?** Elm redrawing the panel means
   re-implementing `.tv-*` classes or writing new ones. RECOMMEND: reuse the
   existing class names and stylesheet, so the palette and the theme keep
   working untouched — the renderer ships its palette at zero specificity and
   the page's own rules already win.
2. **Where does `keyName` live?** Two copies (Elm and `05-keys.js`) would be
   two grammars. RECOMMEND: Elm's program receives already-named keys through
   a port from `05-keys.js`, so the physical-key rule stays in one place.
3. **Does `--assets` still work?** The Elm output is one more file in the
   directory. RECOMMEND: yes, and it joins `gluePartFiles`' list so the
   concatenation is unchanged in shape.
4. **What happens to `check-glue`?** `tsc` cannot check Elm output usefully.
   RECOMMEND: exclude the generated file and let `elm make` be its own check;
   record it in `Makefile` beside `check-glue` so a reader sees both.
