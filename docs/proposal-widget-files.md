# Proposal — one widget, one file

**Status:** proposed · **Date:** 2026-08-08 · **Origin:** fixme.org item 8,
"UX refactor: each reusable widget in a separate file" · **Successor to:**
`docs/proposal-glue-extraction.md`, which made the shell a real `.js` file
and stopped there

## The measurement

`assets/glue.js` is 5391 lines and 418 top-level bindings inside ONE
closure. The seams are already drawn — the file carries capitalised banner
comments naming each surface — and the surfaces are already enumerated in
code (`SURFACES`, the modal registry). What is missing is a boundary the
language enforces.

The cost is not the line count. It is that every binding is in scope
everywhere, so nothing states what a widget may touch: the tags popup can
read `drows`, the document sheet can call `shutCapture`, and the only thing
holding the layering is the reader. `TestServe` guards a handful of these
by grepping the served page for strings that must not appear (`closeFilter`,
`tv-veil`, `selCol`) — a must-not-appear list is the shape a missing
boundary takes.

## What a widget is here

A WIDGET IS A SURFACE PLUS ITS STATE. The file already knows which is
which, and the count is nine:

| File | What it owns | Rough lines |
| --- | --- | --- |
| `glue/log.js` | the strip, `append`, the ring, the height preference | 120 |
| `glue/echo.js` | `said`, `echo`, the pill | 60 |
| `glue/keys.js` | `keyName`, `keyToken`, the dispatch, `ONCE`, `RESERVED` | 400 |
| `glue/surfaces.js` | `SURFACES`, `momentary`, `typing`, `sole`, `cancel` | 120 |
| `glue/palette.js` | the value palette: letters, `/` mode, `askText`, `askFrom` | 450 |
| `glue/document.js` | the materialize sheet's left pane: `drows`, `drawDoc`, the grain walk | 900 |
| `glue/panel.js` | the property panel mount, `prows`, the edit overlay | 500 |
| `glue/popups.js` | the link popup and the tags popup, both `flagKey` surfaces | 700 |
| `glue/capture.js` | the `+` form, tag completion, the grown fields | 250 |
| `glue/settings.js` | the sheet, the tabs, layers, views, hues | 600 |
| `glue/shell.js` | the table mount, fetching, the socket, crumbs, `land` | 1200 |

Eleven files, and the residue is the boot.

## The three ways to draw the boundary

**(A) ES modules, one `<script type="module">`.** Each file exports what it
offers and imports what it uses; the boundary is the language's. Costs: the
page grows from two script tags to a module graph the asset route must
serve by name, and every module is a separate request unless they are
bundled — which would mean a build step this repo has never had
(`cabal build` was always the build).

**(B) One file per widget, concatenated at compile time.** `embedFile` over
a directory rather than a file, joined in a declared order, served as the
single `glue.js` it is today. No build step, no request count change, no
`--assets` change beyond naming a directory. Costs: the boundary is
conventional rather than enforced — the concatenated file is still one
closure, so nothing STOPS a widget reaching across. What it buys is the
review surface (a diff names its widget) and the ordering discipline.

**(C) One file per widget, each an IIFE returning a handle.** The shell
composes them: `const panel = Panel({ el, append, said })`. The boundary is
the ARGUMENT LIST — a widget reaches exactly what it was handed, and
reaching further is a `ReferenceError` at boot rather than a habit. Costs:
the largest edit, since every cross-reference in 5391 lines becomes an
explicit dependency, and some are genuinely circular today (the sheet calls
`append`, the log's height is a settings field, the settings sheet uses the
sheet ladder the materialize sheet also uses).

## Recommendation

**(B) first, (C) as what (B) makes possible.** The concatenation is
mechanical and byte-provable — the joined file must be the file that ships
today, modulo the join order — so it can land with the suite unchanged and
nothing to argue about. It buys the review surface immediately and turns
the second step into eleven independent edits instead of one.

Then take (C) one widget at a time, cheapest first (`log`, `echo`), each
landing green, each turning its banner comment into a signature. A widget
that will not take an argument list is a widget with a dependency worth
naming out loud; the log and the settings sheet's height field are the
first such pair and the honest answer there is that the preference belongs
to the log and the settings sheet READS it.

(A) is rejected for the build step. The binary is the whole deployment,
`--assets` is live hacking with no rebuild, and neither survives a bundler.

## What the split must preserve

- **ONE embedded asset.** The page names two scripts and fetches nothing
  else; `assetSource` stays the one door, so content type, gzip and the
  `--assets` override are inherited unchanged.
- **`--assets` stays live hacking.** A named directory replaces the whole
  asset set, so the split's files must be readable from there without a
  build.
- **`make check-glue` still typechecks the whole.** `tsc --checkJs` over
  eleven files rather than one, with the same jsconfig.
- **The suite's glue extractor.** `TestServe` pulls the served script and
  greps it; a concatenation keeps that working verbatim. Under (C) the
  must-not-appear lists become weaker guards and stronger ones replace
  them — a widget that cannot NAME `closeFilter` needs no test saying it
  does not.
- **No framework, no dependency.** The shell is vanilla JS and shrinking it
  beats adding to it.

## Open decisions

1. **The join order under (B).** RECOMMEND: declared as a list in
   `Glance.Web.Base` beside the asset names, so the order is data and a
   missing file fails the build. Hoisting makes most orders work; the
   declaration is for the ones it does not.
2. **Whether the keymap blob moves.** RECOMMEND: no. `keyBindings` is the
   server's and rides the page; `glue/keys.js` reads it where the shell
   reads it today.
3. **Whether `SURFACES` splits.** It names five widgets, so under (C) it
   becomes the shell's own list of handles rather than a file. RECOMMEND:
   the shell's, since exclusivity is a property of the PAGE and no widget
   can answer it alone.
4. **What happens to the must-not-appear guards.** RECOMMEND: keep them
   through (B) unchanged; under (C) replace each with the argument list it
   became, and delete the guard in the same commit that makes it
   unreachable — a guard nothing can violate is a test that passes for the
   wrong reason.
5. **Whether the document pane is one widget or two.** It is the largest
   file by a factor of two and holds both the model (`drows`) and the
   grain walk. RECOMMEND: one for now; splitting a model from its only
   view is a boundary with nothing on the other side of it.

## Step C, as it has gone (2026-08-09)

Two widgets converted, and what they cost was the finding rather than the
wrapping.

**Wrapping is free; the mutable state is not.** The parts sit at a cosmetic
four-space indent, so an IIFE re-indents nothing and every test pin on a body
line survives. What costs is that a handle CANNOT CARRY A `let`: destructuring
copies whatever it held at boot. `40-popups`' `lmount` went out by value, was
`null` for the life of the page, and its `n`/`p` stepped nothing — the suite
caught it, `tsc` did not.

So a dependency that is a `let` arrives as an accessor, and an export that is a
`let` leaves as one:

| widget | in | out | accessors it forced |
| --- | --- | --- | --- |
| `05-keys.js` | 1 | 5 | `pendingKeys` |
| `40-popups.js` | 22 | 21 | `editNow`, `openedBy`, `linkMount`, `tagMount` |

**Hoisting was load-bearing across the part boundaries.** A top-level line in
one part naming another's `function` is fine; naming its destructured `const`
is a TDZ error. `20-panel`'s backdrop closers are called at click time now
rather than named at registration time.

**The argument list documents the boundary and JS does not hold it** — the
parts share one script scope, so the IIFE still sees everything around it. A
planted reach compiles clean. The enforcement is `TestSelfContained`'s
`wrappedWidgets`, a must-not-appear list per widget, which is exact where an
allowlist over a shared scope cannot tell a local `t` from a foreign one
without a parser. A guard nobody has watched fail is not a guard: this one was
passing over `40-popups` for a while because the widget had been wrapped and
never added to the list.

### `10-document.js` needs its model back first

It is the best Elm candidate — zero synchronous renderer calls, already
model-view-update shaped — and it is the one part that CANNOT be wrapped as it
stands. Its model (`drows`, `dat`, `dcol`, `dgrain`, `dparent`, `dlinks`) is
declared in `00-core.js` and WRITTEN from three other files:

- `20-panel.js` clears it when the sheet shuts;
- `50-settings.js` restores `dat`/`dcol` across a remount;
- `00-core.js` fills `dlinks` and empties `drows` on materialize.

Read accessors do not cover writes, and accessor-plus-setter pairs for six
bindings would be a worse interface than the tangle. The prerequisite is to
move the five `let`s into the widget and give the three writers named
operations — `docClear()`, `docRestore(at, col)`, `docShow(h)`. Three call
sites, five bindings. That is the most valuable single change left here: it
turns the document pane from a model in core that four files poke into a
component, and nothing can be ported until it is one.

### The rule that decides which parts can wrap, measured

A wrapper evaluates its dependency object EAGERLY, at the point the part sits
in the concatenation. So a part can be wrapped exactly when every name it needs
is already bound there. Function declarations hoist and cost nothing; a `const`
or `let` declared in a LATER part is a TDZ error the moment the object is built.

Counting backward dependencies — names a part needs that a later part declares:

| part | late deps | of them `const`/`let` | wrapped? |
| --- | --- | --- | --- |
| `05-keys.js` | 0 | 0 | yes |
| `40-popups.js` | 4 | 0 | yes |
| `30-capture.js` | 4 | 0 | not tried |
| `50-settings.js` | 5 | 2 | no |
| `20-panel.js` | 12 | 6 | no |
| `00-core.js` | 22 | 11 | no |
| `10-document.js` | 25 | 18 | no |

THE TWO THAT WRAPPED ARE EXACTLY THE TWO WITH NO LATE `const`. That is the
whole rule, and hoisting is what had been hiding it: the shell has genuine
cycles — the document pane needs `pediting`, `cancelRow` and `props` from the
panel, and the panel needs `momentary`, `settings` and `configSheet` from the
settings sheet and the shell.

So the remaining parts cannot be converted one at a time in this order. Two
ways forward, and they are the real choice:

1. **A composition root.** Every part defines a factory and NOTHING else;
   a final part constructs them in dependency order and destructures the
   handles. This is what step C originally described. It is a flag day — a
   part that still uses free bindings cannot read a converted part's exports
   at its own top level, so the conversion is all-or-nothing.
2. **Break the cycles first.** Move the shared pieces down into the floor
   (`00-core`) or up into the root (`70-shell`) until the graph is acyclic,
   then convert bottom-up one at a time as before. `10-document`'s 18 late
   `const`s are the measure of that work.

The model move that preceded this attempt stands on its own either way: the
document pane owns its state now, which is a prerequisite for both.

### Breaking the cycles: what was accidental, and what is structural

Eleven names moved down into the floor — `said`, `failed`, `keySaid`, `cycled`,
`cells`, `column`, `visible`, `priorityIn`/`PRIORITY_RING`, `EMPTY`,
`badgeColor`, `rowOf`. Each reads `echo`, `append`, `cols` or the renderer
handle and nothing of the part it had settled in. Pure relocation, no test
changed. Backward `const` dependencies:

| part | before | after |
| --- | --- | --- |
| `10-document.js` | 18 | 9 |
| `00-core.js` | 11 | 8 |
| `20-panel.js` | 6 | 5 |
| `50-settings.js` | 2 | 2 |

WHAT IS LEFT IS NOT MISPLACEMENT. Read the remainder by owner:

- `10-document` needs `props`, `planning`, `pediting`, `cancelRow` from the
  panel, and `subtreeSheet`, `sync`, `stuck` — the sheet LADDER — from it too.
- `00-core` needs `DPARA`/`DTITLE` from the document pane and `asked`,
  `props`, `planning`, `sync` from the panel, because `fill`/`show` are the
  sheet's own opening.

THE MATERIALIZE SHEET IS ONE WIDGET CUT INTO THREE FILES. Its two panes and
its ladder reference each other in every direction because they are one thing:
a flush is one `POST /headline` carrying the document's body beside the panel's
properties and planning, so the pane that writes must read the pane that does
not. CLAUDE.md says as much already — "The sheet is two panes over one subtree"
— and the (B) seam cut through it.

So the cycles that remain are closed by a MERGE rather than by more moves: the
sheet's three fragments become one widget, `sheet.js`, owning `editing`, `raw`,
`base`, `baseProps`, both panes and the ladder. That is the next step, and it
is the last one before `10-document`'s successor can be wrapped — or ported.

The smaller residue is genuinely upward and stays: `20-panel` and
`50-settings` need `momentary`/`typing` from the shell, which is the
composition root answering about surfaces. A root is allowed to be needed;
that is what makes it the root.

### Where the graph stands after the merge

`sameColumns` moved down too — it compares against `cols`, which is the
floor's. Two of the core's three remaining names turned out to be a
MEASUREMENT ARTEFACT rather than a dependency: `asked` and `landing` are
PARAMETER names in `00-core` (`const post = (id, digest, asked, …)`,
`const fetchRows = (landing) =>`), and the scan was not reading parameter
lists as bindings. Fixed, the graph is:

| part | lines | late `const`/`let` |
| --- | --- | --- |
| `00-core.js` | 308 | **0** |
| `05-keys.js` | 63 | **0** |
| `30-capture.js` | 353 | **0** |
| `40-popups.js` | 253 | **0** |
| `70-shell.js` | 406 | **0** |
| `50-settings.js` | 643 | 2 — `momentary`, `typing` |
| `20-sheet.js` | 1236 | 7 — `config`, `configSheet`, `docTargets`, `landing`, `momentary`, `prompting`, `settings` |

FIVE OF SEVEN ARE WRAPPABLE by the eager-evaluation rule, where two were
before the merge. What is left in the other two points forward — at the
settings sheet, the capture form, the popups and the shell — which is the
composition-root direction rather than a cycle.

A lesson worth keeping with the numbers: a measurement that has not been
checked against a false positive is a claim. The first count of this graph
read 45 dependencies for the document pane where the honest figure was
lower, because locals and parameters were being counted as reaches.

### Which parts are worth wrapping, and which are not

Three are wrapped. The interface each one needed is the whole argument:

| part | lines | in | out | verdict |
| --- | --- | --- | --- | --- |
| `05-keys.js` | 63 | 1 | 5 | a widget |
| `40-popups.js` | 253 | 22 | 21 | a widget |
| `30-capture.js` | 404 | 25 | 28 | a widget |
| `50-settings.js` | 643 | **90** | 20 | not a widget |
| `20-sheet.js` | 1225 | — | — | not yet measured, larger |
| `00-core.js` | 308 | — | **70 out** | the FLOOR |
| `70-shell.js` | 406 | **138** | 10 | the ROOT |

A NINETY-NAME ARGUMENT LIST DOCUMENTS NOTHING. The three that wrapped have
interfaces a reader can hold; the rest do not, and the reason is structural
rather than fixable by more relocation: `00-core` is the floor every part
stands on and `70-shell` is the root that composes them, so both are SUPPOSED
to be wide. `50-settings` and `20-sheet` sit between — big surfaces with
genuinely broad reach, and a wrapper around either states a boundary nobody
could keep in their head.

So step C is DONE at three, not seven. What it bought is real and bounded:
three widgets whose reach is stated and enforced, a floor and a root that are
now honestly named as such, and a graph with no cycles left in it.

### Two shapes worth keeping, whatever comes next

**Forward dependencies go in as thunks.** Wrapping turns a part's exports into
destructured `const`s, so an earlier part naming one in an eagerly built
dependency object reads it before its initialiser has run.
`(...a) => showLinks(...a)` defers to call time, which is when it was always
used. This is what makes any remaining wrap possible in any order.

**A direct `eval` does not leak a `const`.** The suite drives `whichKeys` and
`letterAt` as pure functions, which worked because a `function` declaration
leaks out of a direct eval; inside a closure it does not. They leave through
the handle and arrive as `var`.

### What this means for a port

The Elm question is better posed than it was. `20-sheet.js` is now ONE
component — both panes, the ladder and the opening, owning `editing`, `raw`,
`base`, `baseProps` — where before it was a model in the floor that four files
poked. That is a coherent thing to port. CORRECTION: the "zero synchronous renderer
calls" figure written here was measured on `10-document.js` ALONE, before the
merge; the merged file makes 27, of which 20 belong to table machinery that
rode in with the panel and 7 to the property panel's own mount. See
`docs/proposal-elm-sheet.md`, which plans around the real number.

What still stands between it and a port is its size (1225 lines) and its reach
(seven forward dependencies plus a wide floor). A port would have to take the
DOM of both panes, and the four accessors it hands out (`editNow`, `dlinksNow`,
`docCursor`, `docRowById`) become its port API. Nothing about that is blocked;
it is simply a large piece of work that should be started with the interface
measured rather than estimated — which is now possible, and was not this
morning.
