# Handoff — 2026-08-20 → next session: the additive-filters proposal

For a fresh agent picking up `glance` (`/home/akatovda/sync/stuff/glance`,
Haskell + Elm + JS org-mode viewer). Everything below points at artifacts
instead of repeating them.

## Where things stand

- Working tree clean at `9d40fda` ("The tail hides until the walk reaches
  it, and shows a line tall"). The day's batch sits in `eb09c76`,
  `8157529` (the user's commits) and `9d40fda`. Nothing is pushed; never push.
- Gates at HEAD, all green: `cabal test` 2002 · `make browser-check` 39/39
  (run twice — one flaky case in a dozen runs, see
  `docs/bugs/open/2026-08-20-a-full-run-fails-once-in-a-dozen-…md`) ·
  `make elm-test` 159 · `make interop` 13/13 · `make check-glue` clean ·
  `runghc AGENTS.hs` 504 notes.
- `CHANGELOG.md` Unreleased carries the batch; `docs/query.md`,
  `docs/capture.md`, `docs/commands.md`, `docs/config.md` are the user
  docs; README is the crib.

## The next job: additive filters

The proposal is **`docs/proposals/proposed/2026-08-20-additive-filters.md`**
— reviewed by the user today, with a formal-semantics section (grammar, the
axis denotation `(P∪N ≠ ∅ ∧ base) ∨ wide`, six laws, derivations, edge
cases, relational reading) written at their request. Status still
`proposed`: **confirm with the user that it is approved before
implementing**, then follow its "Implementation sketch" section. The user's
own test example: `priority:[#A] tag:book +priority:[#B]` ≡
`(priority A OR priority B) AND tag:book`; order of tokens never matters.

Implementation anchors (read these first):

- `src-web/Glance/Web/Filter.hs` — `scanQuery` (the `-` first-char rule
  the `+` twin copies), `Token`/`Term`, `resolve`, `matchesFilter`
  (today a flat `all`; becomes per-axis groups), `keyTest`, `Field`
  (`Col Int | Planned | Ref | Order | Whole` — the axis is `fieldOf`'s
  answer). `Sort.hs`/`Columns.hs` refuse `-`; `+` refuses the same way.
- `src-query/Glance/Query.hs` — `viewColumns`/`filterKeys` (the six
  column keys); `Meta` words; `refPrefixes :: [(Text, RefVia)]` (today's
  `id:` namespace law — `ref:` is a semi-join, leave it alone).
- `AGENTS.hs` — the spec model; `Field`/`narrows`/`refTest` live around
  lines 2176–2320; a `+` law wants a Note and a model function there, and
  the renderer divergence table gains an `AddKey` row (the renderer reads
  `+state:B` as free text).
- Tests: `test/TestFilter.hs` (`refSpec`, `virtualKeyCase`,
  `titlesMatching`, the fixture trees via `withDocDir`); parity vectors
  `fixtures/parity/filter-query.json`; one keyed TestServe drive over the
  wire; `docs/query.md` gains a section and the README table a row.
- `docs/invariants.md` before touching the write path, store or walk.

A related draft awaiting the user's call, touching the same box:
`docs/proposals/draft/2026-08-20-slash-filters-dot-expression.md` (`/`
filters-only vs `.` whole expression). Do not implement unless asked.

## Other open proposals written today (inert until reviewed)

`docs/proposals/proposed/2026-08-20-generalize-{popup-surfaces,row-kind-coverage,wire-field-names}.md`,
`…/2026-08-20-open-link-ships-the-row.md` (carries the `id:` law: `id:`
names the `:ID:` property, never `ORG_GLANCE_ID`),
`…/2026-08-20-breaks-registry-is-checked.md`.

## Conventions that bit today (beyond CLAUDE.md)

- Prose: no negation-reveal ("A, not B"); terse, caveman register in replies;
  code/commits/docs in the repo's own voice (ALL-CAPS lead phrases on law
  comments, one fact per clause). Commit identity: the protonmail address
  rule in `~/.claude/CLAUDE.md`.
- Never delete files without per-case approval; commit only when the user
  says so; never push.
- Multi-line Python edits: write a script file and run it — inline
  `python3 - <<'PY'` has died on certain replace strings.
- `cabal test 2>&1 | tail` hides the exit code (fish pipeline); redirect to
  a log and echo `$status`. Do not run cabal while a worker is running it.
- Browser harness: a `settled`/`until` right after a key passes vacuously
  until the press is processed — `stepped(p, key, sel, why)` in
  `test/browser/cases.mjs` waits for a NEW point id; `walkTo` dives with
  `f` off headlines. SHOTGEO-style probes must be removed before full runs.
- TestServe keyed drives: `insheet shell "press:…"`; `ontoChild` (five
  `n`) reaches the fixture's child; the stub browser reads assets live, so
  a `make elm` mid-run changes what a running suite sees.
- `/simplify` findings that touch `bootQuery()` are wrong: the live URL
  read is load-bearing (remount re-enters `start()`), see the comment in
  `frontend/glue/00-core.js`.

## Suggested skills

- `overnight` or plain implementation of the proposal once approved;
  `mattpocock-skills:tdd` for the per-axis law (TestFilter first).
- `simplify` on the diff before handing back; `depolarize` on any prose
  written; `generalizer` only if a second language feature lands.
- `code-review` against the proposal's formal section as the spec.
- `caveman:caveman` is active at `full` for replies.
