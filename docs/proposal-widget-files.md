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
