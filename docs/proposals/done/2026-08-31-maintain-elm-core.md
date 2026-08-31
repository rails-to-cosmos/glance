# Proposal — the Elm editor's core, and pulling its logic inside the test gate

**Status:** done — §1 (de-god `update`: applyFill/applyDelete/landingAfter), §2 (`Scan.blocksInRange` split), §3 (DocTest + uuidFrom, and the tight-style sweep of all four Elm sources) delivered 2026-08-31 in commit "elm: unit-test the Doc core, de-god update, tighten the style". Follow-ups spun off: §4 (invariants.md citations) — the agent-verified anchors were re-pointed in the /maintainability pass; a full ~30-citation re-verify remains its own task. §5 (Scan/Body scan-corpus) handed to /generalizer. · **Date:** 2026-08-31 · **Source:** `/maintainability` sweep
over the whole repo (rot / truth-drift / complexity / test-decay agents)

The audit found the Haskell core, the store, the write path and the comment
register all within the project's own discipline. The maintainability cost that
exists is concentrated in the Elm editor and in *where its logic is tested*.
The truth-drift findings were applied directly (doc/citation corrections); what
remains here is risk that wants a considered change, not a mechanical one.

## 1. `Doc.elm` `update` is a 4× god function

`Doc.elm:531` — one 461-line `update` (531–991), 27 `Msg` arms. Most arms are
3–10 lines, but `Delete ids` (806–905) is ~100 LOC and reaches 48-space
indentation (`Doc.elm:864`) — a `case` inside a `let` inside a `case` arm,
computing the post-delete landing row (`landsOn`) inline. `Fill fresh`
(540–591) is ~52 LOC. The file's next-largest def is `view` at 117; the average
def is ~27 LOC across 113 defs.

**Change:** lift the largest arm bodies (`Delete`, `Fill`, and the `landsOn`
sub-`case`) into named module-local functions. Behaviour-preserving, but real
surgery in the churniest file in the repo (four bugs fixed here in ~2 weeks),
so it wants its own `make elm && make elm-test && make browser-check` pass, not
an inline edit during an audit. Risk: MEDIUM.

## 2. `Scan.elm` `blocksInRange` length outlier

`Scan.elm:721` — 119 LOC (721–840), ~2× the file's next-largest (`listRun`,
64). One block scanner, well-covered by `ScanTest.elm`, lower blast radius than
#1. Split the block-open / block-close / nesting passes into named helpers,
module-local. Risk: LOW. Fold into the same change as #1 or defer.

## 3. The editor's pure core lives OUTSIDE `make test`

`make test` = `cabal test` + `elm-test` (Makefile:10-13). `elm-test` runs only
`frontend/elm/tests/ScanTest.elm`, which imports `Body`/`Scan` and never `Doc`
(ScanTest.elm:9-10). So every pure function behind the last four bug-fixes —
`rollUp` (`Doc.elm:2372`, derived checkbox), `cookieIn`/`findCookie`/`cookieKind`
(`:2498-2544`, `[/]`/`[%]` counting), `hiddenDone` (`:2560`), `compactedRun`
(`:2596`), `hiddenIn` (`:2805`) — has its ONLY regression net in the opt-in
`make browser-check` sitting (needs chromium, skips loudly). A regression in the
rollup/cookie/hide-scope math passes `make test` green.

**Change:** add `frontend/elm/tests/DocTest.elm` unit-testing the pure core,
runnable under `make elm-test` inside the always-run gate:
- `rollUp` — none/all/some children → `BoxEmpty`/`BoxFull`/`BoxPart`.
- `cookieIn "[/]"` → the empty done/total cookie; `cookieIn "[X] a [1/2]"` skips
  the `[X]` checkbox bracket and finds `[1/2]`; `cookieKind "foo"` → `Nothing`.
- `hiddenDone` / `compactedRun` over a small fixture doc → the expected `Set`.

Also add one pure Haskell case for `uuidFrom` (`src/Data/Org/Blob.hs:53`):
`uuidFrom (BS.replicate 16 0)` and a fixed 16-byte vector assert the v4 shape
`^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$` — the
version/variant nibbles are stamped (`:59-60`) but asserted only in the opt-in
`make interop`. Both additions are SAFE (additive); they sit here because they
want their gate run, not because they carry design risk.

## 4. Follow-up: a full `invariants.md` citation audit

The truth-drift pass re-pointed the citations the agents verified individually
(the `Style.hs`→`page.css` CSS move, the Store two-writer lines, `replaceSpans`
1208→1443, the drift-lock and empty-pin anchors, the dated-cell Note). But the
`Query.hs` region shifted ~235 lines when `replaceSpans` moved, and
`invariants.md` carries ~30 `Query.hs:` citations — several past the insertion
point are likely stale too. A dedicated pass should re-verify every `file:line`
in `invariants.md` against its symbol. Low risk, but it is the registry the
project trusts as its rulebook, so it earns a deliberate sweep, not sampling.

## Handoff — `/generalizer`

The org grammar is implemented twice: client-side region/block/planning
scanning in `Scan.elm`/`Body.elm`, and authoritatively in `Parser.hs`. The
*date* grammar mirror is cross-validated against ONE corpus
(`TestSpec.hs:1429`; `Query.hs monthWords` ≡ `20-sheet.js MONTH_WORDS`), but the
*scan* grammar has no shared fixture — `ScanTest.elm` asserts Elm-local
expectations while the server half is tested separately. No proven drift today;
this is a missing guard, not a divergence. A shared parse corpus fed to both
`ScanTest.elm` and the Haskell spec (the way the date corpus already is) is a
test-fixture generalization — `/generalizer`'s mandate, not this skill's.
