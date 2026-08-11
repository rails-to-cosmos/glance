# Proposal — mutation testing as a scripted step

**Status:** DELIVERED 2026-08-11 · **Date:** 2026-08-11 · **Origin:** user, after a session
where ~10 hand-run mutations over a green 1781-case suite found two real holes,
and four bugs shipped past that same green suite because nobody thought to
mutate the thing they lived in.

## The measurement that decides it

The suite is DENSE and it is DERIVED. 1781 Haskell cases in eighteen groups
(`test/Spec.hs:24-43`), 53 Elm cases (`assets/elm/tests/ScanTest.elm`, measured
418 ms), `make check-glue` over the shell (`Makefile:111`). Oracles are computed
off the artifact rather than copied beside it: `paletteSweep` reads the served
page and compares `--g-*` against `--tv-*` role by role, `groundSweep`
(`test/TestServe.hs:6248`) cuts rules out of the page and ASSERTS WHAT IT SWEPT
FIRST so an empty sweep cannot pass (`:6251`).

So the prior on "do the assertions bite" is high, and the value is entirely in
the few that do not. That is the question mutation answers and nothing else in
this repo asks.

Two hand mutations this session found two holes, both TEST-SIDE, which is the
class an assertion-dense suite hides best:

- **The harness stub that answered nothing.** `test/fixtures/shell-harness.js:973`
  now returns `m.marks.size`; it returned `undefined`. `M` reads that count to
  decide whether the press toggled (`assets/glue/70-shell.js:133-142`), so every
  `markAll` case was asserting over a value the stub never produced.
- **A CSS permission nothing exercised.** `groundSweep` forbids `box-shadow`
  unless it is inset (`test/TestServe.hs:6265-6267`). Deleting the permission
  left 1781 green — no rule in the page used it. The fix asserts the rule that
  does: `.de.dfl` and its `box-shadow:inset 3px 0 0 var(--g-bad)` (`:6270-6277`).

Neither is reachable by reading the code. Both fall out of one textual rewrite
plus one suite run.

**The cost, measured today on this machine (GHC 9.6.7, cabal, 8 cores):**

| step | `-O2` | `-O0` |
|---|---|---|
| whole suite, warm | 37.5 s (1781 green) | 28.5 s (1781 green) |
| cold build, fresh worktree + own `--builddir` | 5:49.8 | 1:53.6 |
| ONE mutant in `src/Data/Org/Edit.hs`, warm, `-p Edit` | **4:36.0** | **22.3 s** |
| ONE mutant, warm, WHOLE suite | — | **33.4 s** |
| ONE mutant in `assets/glue/70-shell.js`, `-p Shell` (526 cases) | — | 40.3 s |
| ONE mutant in `assets/elm/src/Scan.elm`, all 53 cases | — | **2.7 s** |

`-O2` IS THE WHOLE PROBLEM. A body change to a module at the bottom of the graph
re-keys its `.hi` through cross-module unfoldings and puts the entire package
back through GHC: 4:36 a mutant, which is 12 mutants an hour and the reason
nobody does this by hand twice. At `-O0` the same mutant costs 22.3 s and the
suite is still 1781 green, so the optimisation level buys the runner a 12×
speedup and costs no verdict.

AND SCOPING THE SUITE IS NOT WORTH IT. Whole suite 33.4 s against 22.3 s for
`-p Edit` — eleven seconds, in exchange for a mutant killed only by `TestServe`
being reported as a survivor. Run everything.

## Pattern

A mutation today is: edit the file by hand, `cabal test`, read, `git checkout --`
the file, remember what you already tried. It happened this session because
someone thought of it. It has never happened on a schedule, it leaves no record,
and the ~10 sites it reached were picked by intuition rather than by the table.

## Proposed change

`tools/mutate`, one script, behind `make mutate` — OUT of `cabal test` for the
same reason `elm-test` is (`Makefile:36`): a check whose unit is minutes lives
behind its own target.

### 1. The rewrite table

Ten rules, textual, one table serving all three languages. A rewrite site is a
`(file, line, column, before, after, rule)` tuple, so a mutant is one in-place
substitution over one copy of one file.

| rule | Haskell | JS | Elm |
|---|---|---|---|
| `rel` | `<=`↔`<`, `>=`↔`>` | same | same |
| `eq` | `==`↔`/=` | `===`↔`!==`, `==`↔`!=` | `==`↔`/=` |
| `bool` | `True`↔`False` | `true`↔`false` | `True`↔`False` |
| `logic` | `&&`↔`\|\|` | same | same |
| `guard` | `\| p = …` → `\| not (p) = …` | `if (c)` → `if (!c)` | `if c then` → `if not c then` |
| `off-by-one` | `+ 1` → `+ 0`, `- 1` → `+ 1` | same | same |
| `zero` | a numeric literal → `0`, a string literal → `""` | same | same |
| `arms` | two adjacent `case` alternatives / guard alternatives exchanged | two `if/else` branches | two `case` branches |
| `identity` | `sortOn f`→`const id`-ish: `reverse`→`id`, `nub`→`id`, `T.strip`→`id`, `sortOn _`→`id` | `.sort(…)`→`(x)=>x`, `.trim()`→`` | `List.reverse`→`identity`, `String.trim`→`identity` |
| `body` | a top-level binding whose result has an obvious zero (`Bool`/`[a]`/`Maybe a`/`Text`) replaced by that zero | a function returning `undefined` | same |

`body` is the coarsest and the most informative: a whole function replaced by
`False`/`[]`/`Nothing` that leaves the suite green names a function nothing
depends on the answer of.

A rewrite that does not COMPILE is **invalid**, counted and reported apart from a
survivor. That distinction is the whole difference between a signal and a pile.

### 2. Isolation

`git worktree add $SCRATCH/mut HEAD` plus its own `--builddir`. Three properties
the in-place `git checkout --` loop does not have:

- The developer's `dist-newstyle` is never touched, so a mutation run and an
  ordinary `cabal test` can overlap.
- AN INTERRUPTED RUN CANNOT LEAVE A MUTANT IN THE WORKING TREE. `^C` at the
  wrong instant during a hand loop leaves a flipped comparison in
  `src/Data/Org/Edit.hs` and the next commit ships it.
- The postcondition is checkable: `git -C $WT diff --quiet` after every mutant,
  and a run that fails it stops and says so.

Measured price: 1:53.6 of cold build once per run (`-O0`), then 22–33 s a mutant
warm. The Elm half needs no worktree — `cp -r assets/elm $SCRATCH/elmmut` and
`npx --yes -p elm -p elm-test elm-test`, 2.7 s a mutant, exactly the Makefile's
own ephemeral-npx shape.

### 3. The run

Per mutant: substitute, `cabal test --builddir=$D0 --ghc-options=-O0`, classify.

- **KILLED** — the suite goes red, or the build fails to link, or the run exceeds
  3× the baseline wall (a timeout is a kill: `repeatDay`'s `++N` loop over a
  zero-width interval is the repo's own named non-termination hazard, and an
  `off-by-one` mutant reaches it).
- **INVALID** — the mutant does not typecheck.
- **SURVIVED** — the suite is green.
- **EQUIVALENT** — a survivor on the allowlist (see Risk).

A BASELINE RUN COMES FIRST and a red baseline reports nothing, the way
`TestDefaults.withCorpusSample` refuses to pass over a missing corpus.

### 4. The report

One line per survivor, and the line names what a reader has to decide:

```
SURVIVED  src/Data/Org/Edit.hs:97  rule=body  eolOf
          -  eolOf t = case T.breakOn "\n" t of …
          +  eolOf _ = "\n"
          suite: 1781 green in 31.2 s   groups that touch it: Edit, Subtree, Serve
```

plus a tally per target — `52 mutants: 44 killed · 3 invalid · 2 equivalent ·
3 SURVIVED` — and the same as JSON, so two runs diff. A survivor is a claim
about the SUITE, so the report names the groups that executed the line and left
its answer unasserted.

## Files

- `tools/mutate` — new, the runner: the table, the worktree, the loop, the report.
- `tools/mutate.allow` — new, per-target known-equivalent sites, keyed
  `(file, line, rule, file-digest)` so the entry expires when the file moves.
- `Makefile` — one `mutate` target beside `elm-test` (`:36`) and `check-glue`
  (`:111`), `.PHONY` line extended.
- No source file changes, no `glance.cabal` change, nothing in the binary. No
  `CHANGELOG.md` entry: the doctrine asks for user-visible behaviour and this has
  none.

## The first run's target list

Four targets, chosen where A SILENT WRONG ANSWER IS EXPENSIVE.

1. **`src/Data/Org/Edit.hs`** (247 lines, ~50 sites) — THE WRITE ENGINE. Every
   command in the table reaches a user's org file through `applyEdits` (`:68`),
   `disjoint` (`:108`) and `splice` (`:114`), and the failure mode is a corrupted
   document rather than a wrong screen. Also the demonstration: `<=` → `<` at
   `:111` kills 34 cases in 22.3 s, so the target is known to bite somewhere and
   the run is asking WHERE IT DOES NOT. Extra weight on `eolOf` (`:97`) and
   `openingFor` (`:103`), two small functions that decide bytes.
2. **`src-web/Glance/Web/Filter.hs`** (300 lines, ~130 sites) — THE PARITY PORT.
   CLAUDE.md's own words: "parity is the contract", and there is no schema
   revision mechanism between this and `table-view.js`. A survivor here is a
   term the port is free to drift on and nothing will say. `scanQuery` (`:102`),
   `splitKey` (`:168`), `keyTest` (`:250`), `cellsTest` (`:265`).
3. **`assets/elm/src/Scan.elm`** (846 lines, ~100 sites) — THE SPLICE. `bodyText`
   (`:554`) composes the bytes a `POST /headline` sends, and its rule — ONE GRAIN
   SPEAKS FOR A RANGE, `ownersOf` at `:542` — is documented as surviving most
   cases on bottom-up ordering alone, failing only where a leaf splice changes
   the line count. That is a rule asking to be mutated. Demonstrated: dropping
   `spare` at `:578` kills exactly 1 of 53 in 2.7 s, and knowing it is one case
   is itself worth the run.
4. **`src/Data/Org/Trash.hs`** (116 lines, ~12 sites) — THE ONE DESTRUCTIVE
   COMMAND, four exported functions, 7 tests (`test/TestExternal.hs:348`). The
   highest consequence-per-test ratio in the repo: `trashPathFor` (`:47`) decides
   where a deleted blob lands, `trashBlob` (`:71`) that the copy precedes the
   removal, `filesUnder` (`:104`) that a symlink is not followed.

A fifth once the four land: `assets/glue/20-sheet.js` (970 lines, ~270 sites),
the largest shell part and the one whose bug shipped (`cb6db85`). It is last
because 40 s a mutant makes it the expensive half.

## LOC estimate

`tools/mutate` ≈ 180 lines (shell + `git worktree` + `perl -0pi`), the rule table
≈ 40 of them; `tools/mutate.allow` starts empty; `Makefile` +8. **≈ 230 new
lines, none of them shipped.** Marginal cost of target N+1: one line in the
target list. Marginal cost of rule eleven: one row.

## The budget, and how a run stays inside it

Mutation is O(mutants × tests) and the arithmetic is unforgiving. Unbounded, the
four targets are ~290 Haskell/Elm mutants: 33.4 s each is 2 h 40, and
`20-sheet.js` alone at 40 s × 270 is 3 h.

Three bounds, all of them in the runner:

- **ONE TARGET PER INVOCATION.** `make mutate TARGET=src/Data/Org/Edit.hs`. The
  cold build is paid once per invocation, so a target is a sitting.
- **`SAMPLE=40` by default**, seeded by the target's own digest, so a re-run over
  an unchanged file repeats the same 40 mutants and an EDITED file draws a
  different 40 — which is how the remainder gets covered over weeks instead of in
  one sitting.
- **`DIFF=1`** takes the targets `git diff --name-only` names, `SAMPLE=20` each.

At `SAMPLE=40`: Edit 22 min, Filter 22 min, Trash 7 min, Scan.elm 2 min. The
whole first pass is ~53 min of wall across four invocations, and the diff-scoped
mode is under 15 min, which is a figure someone will actually run.

## Risk

- **Equivalent mutants are the noise floor**, and they are real: measured one
  today — `isSep c = False || …` at `src-web/Glance/Web/Filter.hs:99` survives
  120 Filter cases in 22.8 s and means nothing. The allowlist keeps each one
  reported once; keying it by the file's digest keeps a stale entry from
  silencing a line that has since changed.
- **`-O0` verdicts.** Measured green at 1781, so no case depends on the
  optimisation level today. A future one would show up as a mutant that survives
  at `-O0` and dies at `-O2`, which is a confusing shape; the runner can re-run
  each SURVIVOR at `-O2` before printing it, at 4:36 apiece over a handful.
- **A survivor is not automatically a defect.** Some are equivalent, some are
  code no invariant depends on. Triage is human and it is the recurring cost.
- **THE HONEST BOUNDARY: mutation grades the assertions over code the suite
  already executes, and says nothing about what the harness cannot see.** Four
  bugs this session were found by eye — an edit overlay covering the document
  (`cb6db85`), a flag drawn in warning orange where the table draws red, an empty
  paragraph collapsing to zero height, badge colours lost in an Elm port. All
  four are geometry and colour; `test/fixtures/shell-harness.js:1289` returns
  zeros from `getBoundingClientRect` and CLAUDE.md says so outright ("Geometry is
  beyond it", `:1088`). Every geometry rule in the page is asserted as CSS SOURCE
  TEXT — the edit box's floor is the literal string
  `"min-height:calc(var(--g-doc-rows, 1)"` at `test/TestServe.hs:5893`. A
  mutation runner makes those string assertions honest; it does not make them
  measurements. THE RENDERING GAP IS A DIFFERENT PROPOSAL.
- **Runner correctness.** The runner is untested code that reports on tests. The
  mitigations are that it never writes outside the worktree, that it asserts a
  green baseline before the first mutant, and that a KILLED verdict is
  self-checking — a rewrite that kills nothing anywhere is a rewrite that is not
  being applied, which the tally shows as 0 killed.

## Existing precedent

- `make elm-test` (`Makefile:36`) is OUT of `cabal test` because it fetches at
  run time. The standing rule: a check needing the network or a heavy tool lives
  behind its own target. `mutate` is that rule's second instance.
- Derived oracles that assert what they swept first: `groundSweep`
  (`test/TestServe.hs:6248-6252`), `paletteSweep`, `logColumnSweep`. A mutation
  runner is that idea one level up — it asserts the suite is not sweeping
  nothing.
- `TestSelfContained`'s must-not-appear lists: enforcement by textual sweep,
  which is the rewrite table's own register and its own limits.
- The corpus check (`cabal run -v0 glance -- scan ~/sync`, ~10 s walk): a heavy
  check run by hand on a cadence, with the expected numbers written down.
- `docs/invariants.md:6` declares the confidence ladder and 40 entries sit at
  `**none**` — "silently relied on". That file is already the repo's written
  backlog of unguarded rules; a survivor list is the same document generated
  rather than remembered.

## Open decisions

1. **THE ONE A HUMAN MUST TAKE: does a survivor block, or does it join the
   backlog?** Gating means a mutation score floor per target and a run in the
   loop; a backlog means the survivor list is written next to
   `docs/invariants.md`'s 40 `**none**` entries and worked when there is room.
   The two differ by an order of magnitude in cost, and every other decision here
   is downstream of it.
2. Whether the JS half gets a node-only path. `assets/glue/*.js` concatenated in
   `Glance.Web.Base.gluePartFiles` order (`src-web/Glance/Web/Base.hs:124`) IS
   the `shell.js` the harness runs (`test/TestServe.hs:5824`), so a runner could
   write it directly and skip GHC entirely — 40.3 s to roughly 3 s. The price is
   a second way to build the page, which the TH splice at
   `src-web/Glance/Web/Routes.hs:130` currently owns alone.
3. Runner language. Shell + `git` + `perl` adds no dependency and matches the
   Makefile's register. A Haskell executable would be typed and would be tested
   by the suite it mutates, which is circular.
4. Whether the rule table lives in the runner or in a data file the runner reads.
   ORDER IS DATA is already the house answer for `gluePartFiles`; the same
   argument applies here and costs one file.

---

## Delivered — 2026-08-11

`tools/mutate` (the driver), `tools/mutate-sites` (the site generator and the
verified substitution), `tools/mutate.rules` (the table, as data — open decision
4 resolved that way), `tools/mutate.allow` (empty), `make mutate` /
`make mutate-list` / `make mutate-clean`. 842 lines in `tools/` plus 31 of
`Makefile`, none of them shipped — against the estimate of ≈230, and the
overrun is one thing: the estimate budgeted a `perl -0pi` substitution and the
runner needs a LEXER. Masking, the four structural rules and the site model
that carries them are ~450 of the 842; the driver is 296 and the table is 63,
which is roughly what was costed.

### What was built against what was proposed

- **Isolation** as specified: `git worktree add --detach` at REV plus its own
  `--builddir`, `--disable-optimization` rather than `--ghc-options=-O0` so no
  flag-order argument decides the level. The runner mutates the COMMITTED
  revision and never reads or writes the working tree.
- **Classification** is BUILD-then-TEST as two commands, which is what tells
  INVALID from KILLED without grepping GHC's output for the difference. A
  timeout is a kill, at 3x the measured WARM suite wall.
- **Sampling** seeded by the target's blob digest, as proposed.
- **Masking**, which the proposal does not mention and the runner cannot work
  without: comments and literals are blanked before any rule is asked, or every
  Haddock paragraph is a survivor. A LITERAL masks to `\x01` rather than to
  space — masked to blanks, `| x == "a" = e` reads as a guard ending at `==`
  and the negation goes out half-written. That bug was live and the first Elm
  run found it as a pile of INVALIDs.
- **Ten rules, six delivered as data and three as code.** `arms` is Haskell and
  Elm (a JS if/else exchange is a block move, not a line one); `body` is
  Haskell alone, since it is the type signature that names the zero. `body` for
  Elm and `arms` for JS are the honest gap.
- **`DIFF=1` is NOT built.** One target per invocation is; the diff-scoped mode
  is not.
- **"Groups that touch it" is NOT built.** The report names the site, the rule,
  the before and the after; which test groups executed the line needs coverage
  instrumentation and is a different tool.

### The first runs, measured

| target | sites | mutants | killed | invalid | survived | score | wall |
|---|---|---|---|---|---|---|---|
| `assets/elm/src/Scan.elm` | 230 | 230 (all) | 167 | 0 | 63 | **72%** | 23 min |
| `src/Data/Org/Trash.hs` | 13 | 13 (all) | 12 | 0 | 1 | **92%** | 16 min |

Cost per mutant: 2–5 s Elm, 60–90 s Haskell under a competing build (the
proposal measured 22–33 s uncontended). Cold build once per invocation: 152 s,
against the proposal's 1:53.6.

`arms` WAS THE INSTRUMENT'S OWN DEFECT, and the first Trash run is what found
it. Swapping two case alternatives whose patterns are DISJOINT CONSTRUCTORS
(`Left`/`Right`, `Dir`/`Regular`) is a rewrite with no observable difference at
all — 2 of that run's 3 survivors and 2 of its 2 invalids were that one class.
The rule now refuses a distinct-constructor pair and a layout-opening RHS, and
what it is left pointing at is the case where order IS a fact: a catch-all, a
guard, a literal. Trash.hs went 17 sites to 13, 2 invalid to 0, and 80% to 92%
with no test touched — the run BEFORE the fix is the evidence the fix is right,
which is a measuring instrument measured by its own first reading.

### What the runs found

`src/Data/Org/Trash.hs`, ONE survivor over the four exported functions, and
seven tests kill everything else — the highest consequence-per-test ratio in
the repo is also its best-asserted file:

- `:77` `zero` — the refusal message for a trash destination that already
  exists reads back as `""`. The refusal itself is asserted; the WORDING is not,
  and it is what a `/command` 400 body says. `test/TestExternal.hs`'s
  second-deletion case should read the message.

`assets/elm/src/Scan.elm`, 63 survivors in four clusters:

- **`String.trim` → `identity` in `closes` (:191) and `isBlank` (:201).** No
  case has a `#+end_x` line with trailing whitespace, and none has a
  whitespace-only line standing in for a blank one. Org treats both as what
  they look like, and `ScanTest.elm` pins "a block closing by NAME" and "org's
  one-blank-line rule" without either. TWO CASES CLOSE BOTH.
- **`bodyText`'s splice (:565, :572).** `List.reverse` → `identity` survives,
  so no case deletes or edits TWO paragraphs at once — and the reverse is the
  whole reason a splice does not shift the next one's indices. The `spare`
  boundary (`r.to < List.length out - 1`) survives four ways, so deleting the
  LAST paragraph, whose blank line is the file's own last, is untested. This is
  the rule the proposal picked the target for.
- **`numberedAt` (:143–145) and `listOpener`'s bare bullets (:98–103).** `"."`,
  `")"` and the `||` between them all survive: nothing tests `1)` numbering.
  Nothing tests a bullet with no text after it either.
- **The reading tail, `placeOf`/`shown`/`cellCount`/`kidsOf`/`kindWord`
  (:806–846), 12 survivors and no assertion between them.** `ScanTest.elm` asks
  the scanner and the splice; these answer the echo's words and the pane's cell
  counts, and the Haskell harness reaches them only through what the pane draws.

### Open decision 1 stays open

Does a survivor block, or does it join the backlog? Nothing here gates: the
runner is out of `cabal test`, `make mutate` is run by hand, and its exit code
is 0 with survivors standing (it fails only on a red baseline, a worktree that
did not revert, and 0 killed). The 64 survivors above are a backlog entry next
to `docs/invariants.md`'s `**none**` rows, which is the cheaper of the two arms
and the one this delivery took by default rather than by argument.
