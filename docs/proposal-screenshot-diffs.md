# Proposal — screenshot diffs over the shell's surfaces

**Status:** proposed · **Date:** 2026-08-11 · **Origin:** user, after a session
where four visual bugs shipped past 1781 green tests and were caught by looking
at the page

## The measurement that decides it

1781 Haskell tests plus 53 elm-test cases are green, `make check-glue` is clean,
and NOTHING IN THE REPO RENDERS ANYTHING. `test/fixtures/shell-harness.js` is a
real node tree with a selector engine and no layout: `getBoundingClientRect`
returns zeros (`shell-harness.js:1289`), there is no HTML parser (`:1150`), so
`TableView` is a stub (`:1033`). CLAUDE.md says it outright — "Geometry is
beyond it."

Every geometry rule is therefore asserted as CSS SOURCE TEXT. `groundSweep`
(`TestServe.hs:6248`) cuts four rule bodies out of the served page and greps
them for `underline`, `outline`, `border`, `text-decoration`, `box-shadow`. The
paragraph box's floor is the literal string `"min-height:calc(var(--g-doc-rows,
1)"` (`TestServe.hs:5893`). These assert what the stylesheet SAYS; a browser is
what decides what it DOES.

Four bugs this session, each found by eye against that green suite:

- The paragraph editor grew to ten lines and COVERED the document under it
  (`cb6db85`).
- A flag in the sheet drew in `--g-warn` (`Theme.hs:81`, `Theme/Types.hs:30`)
  where the table draws `--g-bad` (`14e13d9`, then `b2762c5`).
- An empty drawn paragraph collapsed to zero height.
- Badge hues lost in the Elm port.

`paletteSweep` (`TestServe.hs:6102`) could not have caught the second. It pairs
TWELVE roles across the `--g-*`/`--tv-*` namespaces and `--g-warn` is in neither
list, having no renderer twin — the two palettes agreed exactly, and the rule
named the wrong role out of one of them. The guard the fix left behind is one
more hand-spelled string, `"box-shadow:inset 3px 0 0 var(--g-bad)"`
(`TestServe.hs:6277`): a rule written after it broke, which is the shape of
every geometry guard here.

THE MISSING ORACLE IS THE PIXELS, and it is the only one that answers the
question the user has been answering by eye all session.

## Depends on the driver

docs/proposal-browser-driver.md owns the toolchain question whole, and this
proposal is worth nothing before it lands. Its measured starting point stands
here unargued: no Chromium, Chrome, Brave or Vivaldi on this machine, no
playwright or puppeteer cache, three spikes at Firefox's Remote Agent that never
made the port listen. Whatever it resolves, this asks it for exactly TWO
capabilities — navigate-and-evaluate, and capture the viewport as a PNG. No line
below reads a wire protocol.

## Proposed change

### A shot is a URL

EVERY POPUP HAS A URL and it is shareable — `?page=NAME` beside `?q=`, `&row=ID`
where the surface is rowed, the panel as the FRAGMENT. `SURFACES`
(`assets/glue/70-shell.js:15`) declares six entries with their `open` doors, and
`bootPage` (`:52`) raises what the URL names once the rows are in hand. So a
shot is ADDRESSED. No key timing, no settle chain, no dependence on which of two
listeners registered first.

Nine shots, each carrying a rule nothing else asserts:

| shot | address | keys after boot |
| --- | --- | --- |
| `table` | `/?q=` | — |
| `sheet` | `/?q=&page=sheet&row=ship-table-view` | — |
| `sheet-edit` | same | `RET` on a paragraph |
| `panel` | same | `TAB`, `d` |
| `palette` | `/?q=` | `t` |
| `links` | `/?q=&page=links&row=sample.org#5` | — |
| `tags` | `/?q=&page=tags&row=ship-table-view` | `d` |
| `settings` | `/?q=&page=config#theme` | — |
| `capture` | `/?q=&page=capture` | — |

`sheet-edit` is `cb6db85`'s own frame. `panel` and `tags` carry a flag down, so
they are `14e13d9`'s. `palette` carries the which-key letters marked inside
keywords in their badge hues, so it is the Elm port's. `settings` carries the
states table's colour column.

TWO of the nine press a key, because the state inside those surfaces has no
address: the value palette declares no `open` (a keystroke's answer rather than
a place) and an OPEN EDIT is not a place either. Each shot's key run is DATA
beside its address, so nothing is scripted twice.

Row ids are stable because the fixture is: `test/fixtures/view/sample.org` is 17
lines, one `ORG_GLANCE_ID` and five ordinals numbered off a file that does not
move.

### One viewport, and the reason there is one

1280×800, `deviceScaleFactor: 1`. `body` is `height:100vh;overflow:hidden`
(`Style.hs:58`) and `.pop-sheet` is `width:min(80vw,100%);height:var(--g-pop-max)`
(`Style.hs:296`) over `--g-pop-max: min(90vh, calc(100vh - 2 * 5vh))`
(`Style.hs:52`–`:54`), so every box on screen is a pure function of the viewport.
A second viewport doubles the baselines and asserts nothing: the layout has ONE
responsive branch, the `@media (pointer:coarse)` block, which a desktop viewport
never enters. DPR 2 quadruples the bytes and says the same thing.

### The theme axis is one attribute

A theme is switched CLIENT-SIDE with no refetch, so the second theme is
`document.documentElement.dataset.theme = "dark"` plus one capture — no reload,
no second boot. Two themes (`Theme.hs:47`–`:51`) × nine shots = 18 baselines,
and the second nine cost an attribute write each.

WHAT THE PAIR BUYS OVER `paletteSweep`: the sweep asserts the two NAMESPACES
agree role by role and is silent about which role a RULE NAMES. A shot pair asks
that second question derivedly — the flag bug moves pixels in both themes, in
the one place the flag is drawn, with no rule spelled by hand. Badge hues are
`var()` off the theme with the slot count on the wire (`stateSlots = 4`,
`Query.hs:3420`), so a slot that lost its declaration is a hole in the shot
rather than a token nothing reads.

### Decode, never byte-compare

PNG IS DEFLATE, so a byte compare is a compare of an entropy coder: one moved
pixel rewrites the whole stream, and two zlib builds encode identical pixels
differently. Baselines are compared as PIXELS.

Decoding costs no dependency. `node:zlib` is core, and PNG's layer over it is
small: read `IHDR`, concatenate the `IDAT` chunks, `inflateSync`, undo the five
per-scanline filters (None/Sub/Up/Average/Paeth), emit RGBA. Truecolour 8-bit
with and without alpha is the only form a driver emits; every other colour type
and any interlace is REFUSED loudly rather than half-supported.

NOTHING WRITES A PNG. A failing shot dumps three `P6` PPM files — a nine-byte
header and raw RGB — into a gitignored `dist-shots/`: baseline, current, and the
diff mask. A PPM costs six lines and any viewer opens one, so the encoder this
would otherwise owe never gets written. Baselines stay PNG because size is
theirs to carry; failures stay PPM because bytes are free in a scratch
directory.

### The tolerance is per-pixel and per-cell, never per-frame

Some tolerance is owed. Glyph antialiasing is compositor- and GPU-dependent, and
the stale wash composites `opacity:.55` in float. An exact compare goes red the
first time the driver moves and is disabled that week.

A GLOBAL PERCENTAGE IS THE TRAP, and the flag bug measures why: the flag's edge
is `3px` inset over one row, about 72 px of 1,024,000 at this viewport — 0.007%
of the frame. Any frame-percentage loose enough to absorb AA hides it entirely.

Two rules, neither a frame percentage:

1. **Per channel.** A pixel DIFFERS when any of R/G/B moves by more than 8 of
   255. AA is a blend along a glyph edge and lands under that; a hue swap does
   not — `--g-warn` and `--g-bad` are tens apart on two channels.
2. **Per cell.** The frame FAILS when any 8×8 cell holds four or more differing
   pixels. AA noise is one or two pixels per cell scattered along edges; a moved
   box, a collapsed line and a wrong colour fill cells solid.

THERE IS NO KNOB. A group going red for something that is not a real change is
answered by DELETING A SHOT, never by loosening either number — a tolerance dial
is how these suites die.

### Acceptance is `make shots`

`make shots` writes the baselines; `make shots-check` compares. Both are OUT of
`cabal test`, for the reason the Makefile already gives `elm-test`: "elm-test
fetches `elm-explorations/test` at run time, and the Haskell suite must stay
offline." A browser is heavier than a fetch.

A deliberate change is `make shots`, look at the file, `git add` —
`assets/elm.js` is accepted exactly this way and nobody reads its 8395 lines
either. The guard on both is that the target regenerates and the tree comes back
clean.

THE BROWSER BUILD IS RECORDED beside the baselines (`test/shots/BUILD`, the
`navigator.userAgent` a shot was taken under) and a mismatch REFUSES rather than
fails. A browser upgrade then prints "regenerate", which is what keeps the group
from going red the day a driver moves and dead the day after.

### The offline half lands in `cabal test`

`TestSelfContained.hs:79`–`:84` already asserts both directions over a declared
list: every glue part the build names is on disk, and nothing on disk is unread.
The same shape over shots — every surface the shot table names has a baseline
per theme, and no baseline names a surface `SURFACES` has dropped — runs
OFFLINE, needs no browser, and is what stops a new surface from quietly having
no shot. ~25 Haskell lines, in the suite that already runs everywhere.

## What makes it stable

The failure mode to avoid is a suite that reddens on a font hint and is disabled
within a week. Five decisions, each removing one source of drift:

- **The font is bundled.** `monoStack` is `"JetBrains Mono", "Fira Code", "SF
  Mono", Menlo, Consolas, monospace` (`Style.hs:19`), and the `@font-face` is
  emitted ONLY when the `--assets` directory holds the file (`fontAssets`,
  `Style.hs:22`; `fontFace`, `:27`). So the shot run serves with `--assets
  assets` — the documented dev flow — and one committed
  `assets/JetBrainsMono-Regular.woff2` makes the face deterministic through the
  page's OWN mechanism, with no new code. Without it the stack resolves to
  whatever the machine installed, which is the single largest source of drift
  here. The shot waits on `document.fonts.ready`, since `font-display:swap`
  (`Style.hs:27`) will otherwise hand back the fallback face.
- **Animations off.** Injected before every capture:
  `*{transition:none!important;animation:none!important;caret-color:transparent}`.
  The stale wash eases 180 ms, and a caret in an open edit is a two-state pixel.
- **The harness's own fixture, never the corpus.** `sample.org` is six rows over
  17 lines with fixed stamps, five keywords, three priorities, unicode titles and
  one link — every badge slot and the `linked` underline, in a file that does not
  move. The corpus is ~12.6k headlines that change daily and lives on one
  machine. The daemon is pointed at a temp COPY, so no `.org-glance` store is
  ever written into the fixture.
- **Nothing scrolls.** Six rows with the log strip at its default seven lines
  (`logLinesDefault = 7`, `Base.hs:90`) fits 800px, so no scrollbar's platform
  width is in any shot. A fixture that outgrows the viewport is a fixture to
  trim.
- **The clock is masked, once.** Every log line opens `HH:MM:SS`, and the strip's
  severity and scope are columns whose widths `TestServe` already derives, so the
  time sits at a fixed x-range. ONE ignore rectangle per shot, declared as data
  beside the address. A shot wanting a second rectangle is a shot to split. No
  shot drives a capture, whose `ORG_GLANCE_CREATION_TIME` is the server clock.

And the settle is a CONDITION with a cap, never a duration — the rows painted
and the surface's own class up — which is the rule the node harness already
lives by (`until:stale=off`).

## Files

New: `test/shots/png.js` (decode), `test/shots/diff.js` (compare, PPM dump),
`test/shots/shoot.js` (the shot table, the theme loop, the injection, the
settle), `test/shots/BUILD`, `test/shots/*.png` (18), `assets/JetBrainsMono-Regular.woff2`.
Edited: `Makefile` (two targets), `test/TestSelfContained.hs` (the offline
both-directions check), `.gitignore` (`dist-shots/`), CLAUDE.md (one paragraph),
docs/invariants.md (one entry — 40 of its entries carry `**none**` for evidence,
and this closes none of them; it opens an axis).

## LOC estimate

~90 (PNG→RGBA) + ~70 (compare + PPM) + ~140 (the shot script, ~35 of it the
declared table) + ~16 (Makefile) + ~25 Haskell = **~300 JS, ~25 Haskell, zero
dependencies, zero additions to what `cabal test` runs**. Plus 18 PNGs at ~1 MB
and one woff2.

## Risk

- **Downstream of an unproven driver.** Nothing here runs until
  docs/proposal-browser-driver.md lands, and on this machine today it has not.
- **The first binaries in the repo.** All 324 tracked files are text today
  (measured: `file --mime` over `git ls-files` finds only JSON outside `text/`)
  and `.git` is 19 MB. 18 PNGs plus a woff2 is ~1.1 MB — 6% of the history at
  once — and every accepted change rewrites the blobs it touches. `make elm`
  writes 182 KB per run, so the precedent exists at a fifth the size.
- **It says nothing about behaviour.** A shot cannot tell you what a key does.
  This is a second axis beside the 1781, and its budget must not come out of
  theirs.
- **The two key-pressing shots are the two that can flake.** If either does, it
  is deleted rather than retried; the seven addressed ones stand alone.
- **Today-relative faces.** The fixture's stamps are all fixed and all past, so
  nothing in a shot is dressed by the wall calendar today. A renderer that grew
  an overdue face would need the fixture re-dated or a frozen clock, and the
  daemon carries no flag for one.
- **A baseline nobody looks at is a baseline that blesses a bug.** The
  acceptance step is a human LOOKING at the regenerated file. `make shots`
  prints the paths for exactly this reason, and it is the weakest link in the
  design.

## Existing precedent

`assets/elm.js` and `assets/table-view.js` — committed build artifacts refreshed
by a make target that reproduces the committed bytes, accepted with `git add`.
`make elm-test` — a check needing more than the offline suite lives behind its
own target. `paletteSweep`/`groundSweep` — derived oracles that assert what they
SWEPT first, so an empty sweep cannot pass; the shot run's version is 18 named,
18 found. `TestSelfContained.hs:79`–`:84` — both directions over a declared list.
`until:` polling — a condition with a cap where a duration cannot say the moment.

## Open decisions

1. **THE ONE A HUMAN MUST TAKE: do ~1.1 MB of binaries belong in this git
   history?** In-repo is the `elm.js` precedent, makes `git add` the whole
   acceptance, and lets a fresh clone check something. Out-of-tree (baselines
   under a cache with only their digests committed) keeps the history text-only
   and makes a fresh clone unable to check anything until it regenerates — which
   is a baseline nobody reviewed.
2. Whether the woff2 goes into `assets/` (which also fixes the documented
   `--assets assets` dev flow for everyone) or into a shots-only directory that
   exists for the check alone.
3. Whether `sheet-edit` and `palette` earn their key presses, or whether the
   shell should grow a URL for an OPEN EDIT — which would make all nine shots
   addressed and is a real question about `remembered`'s vocabulary rather than
   a test convenience.
