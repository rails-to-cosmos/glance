# Proposal — a boundary the language enforces, and which language

**Status:** proposed · **Date:** 2026-08-14 · **Origin:** user — "the glue is
hard to read spaghetti hell; rewrite it in TypeScript" and "any parts that could
benefit from migrating to Elm?" · **Successor to:**
[2026-08-08-widget-files.partial.md](2026-08-08-widget-files.partial.md), whose
own words this answers: *what is missing is a boundary the language enforces.*

## The measurement, and what it says about the last attempt

| | then (widget-files, 2026-08-08) | now |
|---|---|---|
| shape | one `glue.js` | seven parts |
| lines | 5391 | 3236 |
| top-level bindings | 418 in ONE closure | **423 in ONE scope** |
| boundary | none | **none** |

**Splitting the file bought nothing that matters.** The parts are FRAGMENTS of
one script scope, concatenated in `gluePartFiles` order, so every part still
sees every other part's bindings — and they use them. That is the finding to
carry: the seam was drawn on disk and not in the language.

The reads, measured (locals excluded, shadowing accounted for):

```
00-core.js     <- 05-keys, 20-sheet, 50-settings
05-keys.js     <- (nothing)
20-sheet.js    <- 00-core, 05-keys, 30-capture, 40-popups, 50-settings, 70-shell
30-capture.js  <- 00-core, 05-keys, 20-sheet, 40-popups, 70-shell
40-popups.js   <- 00-core, 05-keys, 20-sheet, 30-capture, 70-shell
50-settings.js <- 00-core, 05-keys, 20-sheet, 30-capture, 40-popups, 70-shell
70-shell.js    <- 00-core, 05-keys, 20-sheet, 30-capture, 40-popups, 50-settings
```

**Ten genuine cycles**, and they are surface-to-surface — each overlay reaches
into the others' open/close/state functions by name:

| pair | crossing |
|---|---|
| sheet ↔ capture | `askState`, `askTags`, `docTargets` / `editing`, `postCommand`, `docTitle` |
| sheet ↔ popups | `shutLinks`, `shutTags` / `openEdit`, `cancelEdit`, `landed` |
| sheet ↔ settings | `settings`, `configSheet` / `DTITLE`, `DPARA`, `dparaing` |
| sheet ↔ shell | `momentary`, `remembered` / `docOpen`, `sheetOpen`, `cancelSheetEdit` |
| capture ↔ popups | `showLinks`, `showTags` / `askFrom`, `foldTag`, `tagFrom` |
| shell ↔ settings | `momentary`, `typing` / `ctab`, `wantPanel`, `focusFilter` |

That mutual reaching is the unreadability. It is not syntax.

## TypeScript is already here, and that is the point

`frontend/jsconfig.json` runs `tsc` with `checkJs: true` over all seven parts —
`make check-glue` is a standing gate and it is clean. What it is NOT doing:

```json
"strict": false, "strictNullChecks": false, "noImplicitAny": false
```

So the language is already TypeScript-checked; it is checking almost nothing.
**Renaming `.js` to `.ts` changes no line of the diagnosis above** — the same 423
bindings, the same ten cycles, now with type annotations. That is typed
spaghetti, bought at the price in the next section.

The real TypeScript win available today is *strictness*, and it costs no
rewrite: turning `noImplicitAny` and `strictNullChecks` on, per file, fixing
what falls out. That is a real improvement, available this week, and it is
independent of everything else here.

## The cost nobody counts

A rewrite does not just touch 3236 lines of glue:

- **395 source strings pinned by 40 `Glue` cases** in `TestServe.hs`. They assert
  the glue's own text — `"const LOG = CFG.log;"` and its like — because the node
  harness cannot measure geometry and the text is what is left.
- **`test/fixtures/shell-harness.js`, 1563 lines** — a hand-built DOM, a stubbed
  `TableView`, and an act vocabulary, which loads the glue as one script scope.
  Modules change how the glue loads, so the harness follows.

A big-bang rewrite therefore changes the code **and the safety net that checks
it, in one step**. That is the single thing most worth refusing.

## Which parts want Elm — and the answer is already in this repo

Two kinds of work are tangled in the glue:

- **Decisions over data** — which command a key names, what the DEL ladder does
  next, what the URL says, which panel is dirty, what the log ring holds.
- **Browser plumbing** — DOM handles, listeners, `fetch`, the socket, focus,
  caret geometry, and the renderer's MUTABLE api (`table.select`,
  `table.getMarked`).

The first is Elm-shaped. The second cannot be Elm at any price.

**The document pane already has this split, and it is the best evidence in the
repo.** `Doc.elm` owns the model and the decisions, `Scan`/`Body` are pure
functions over lines, and `20-sheet.js` keeps the DOM and the ports. That
surface carries **159 elm-tests**, and every subtle bug found in it this
month — the 2px grid drift, the region markers, the block closer — was found
and fixed as a pure function with a test, offline, in milliseconds.

No other surface has anything like that. So the question is not "should some
glue become Elm" but "should the other surfaces get what the pane already has".

Ranked by how much of a part is decision rather than plumbing:

| part | lines | verdict |
|---|---|---|
| `05-keys.js` | 59 | **Elm, first.** Key sequence to command over `MAPS`. Nearly pure, and the smallest thing in the tree. |
| URL state (in `70-shell`) | ~60 | **Elm.** `?q=&page=#panel` parse and serialize is a total function both ways, and a round-trip property is free. |
| the log ring (in `00-core`) | ~40 | **Elm.** Append, cap, severity. A fold over a list. |
| crumbs / drill stack | ~50 | **Elm.** A stack with a documented ladder. |
| `50-settings.js` | 636 | **Split.** The panel/dirty/flush ladder is decision; the states table is already a `Listing` mount; the fields are plumbing. |
| `30-capture.js` | 401 | **Split.** Which fields exist and what commits is decision; focus and the form are plumbing. |
| `40-popups.js` | 248 | **Mostly plumbing.** |
| `20-sheet.js` | 1021 | **Already split** — its decisions left for `Scan`/`Body` months ago. |
| `00-core.js` (rest) | 452 | **Plumbing.** `fetch`, the socket, the renderer's mount. |

**Elm forbids import cycles**, which is exactly the constraint missing today —
and the `Scan`/`Body` split proved this week that the compiler settles such a
question in one attempt rather than by argument.

## What this proposes

**Not** a rewrite. Four steps, cheapest first, each landing green, each
measurable, and any of them worth stopping after.

1. **Strictness where it already applies.** Turn on `noImplicitAny` and
   `strictNullChecks`, file by file, fixing the fallout. No behaviour change, no
   test churn, no new toolchain. Do this regardless of everything below.
2. **Break one cycle without moving a line into another language.** The
   surfaces reach into each other because there is no registry to coordinate
   through — which
   [2026-08-06-overlay-registry.proposed.md](2026-08-06-overlay-registry.proposed.md)
   already proposes for the edit shapes. Land that, measure how many of the ten
   pairs it dissolves, and re-count before doing more.
3. **Move `05-keys.js` to Elm as the worked example.** 59 lines, nearly pure,
   and the answer it computes — which command a key names — is already asserted
   by the suite through the served page, so the behaviour has an oracle before
   the move. If this does not visibly reduce coupling and lines, stop; the rest
   of the ladder rests on it.
4. **Then one surface, chosen by what step 3 taught.** `50-settings` or
   `30-capture`, decisions to Elm and plumbing to TypeScript, keeping the
   harness working at every commit.

## What would say this was wrong

- Step 3 lands and the JS side shrinks by less than the Elm side grows.
- The port boundary needs more traffic than the direct call it replaced.
- `shell-harness.js` cannot follow without being rewritten, which would mean
  step 4 is a big bang wearing a small step's clothes.

Each is checkable at the time, and each is a reason to stop rather than push on.

## What this is not

An argument that TypeScript is the wrong language. It is already the language,
it is already checked, and the plumbing that remains should be TypeScript with
strictness on. The argument is only that the **file rename is not the fix**, and
that the boundary `widget-files` asked for is a `port`, not a `.ts` extension.
