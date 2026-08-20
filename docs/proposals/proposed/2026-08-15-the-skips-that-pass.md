# Proposal — the skips that pass, and the four hazards nothing catches

**Status:** proposed · **Date:** 2026-08-15 · **Origin:** `/simplify-repo`'s
global-invariants angle, run over the whole tree the night of 2026-08-15.  It
was asked for the rules a careless refactor would break and the ranked list of
the ones nothing would catch.

## The measurement that changes what the other numbers mean

`runghc AGENTS.hs` reports 433 notes with **116 tagged `Unguarded`**.  That
number reads as 116 hazards.  It is not.

Roughly **82 of them sit under the repeated marker** *"Tier two until the checks
moved into the suite: what no gate asks"* (AGENTS.hs:403, 874, 1242, 2076, 2570,
3002, 3444, 4294).  Those are residue of checks that used to run, plus
measurements — *"The sweep stands at 31.7%, over by 13089"*, *"`resolveIds` has
four call sites"*.  Several are stale by construction: any note quoting a
percentage is a number someone took once.

**The hazard set is the ~34 inline notes.**  Splitting `Unguarded` into a hazard
nothing catches and something like `Measured` — a number taken once, with a date
— would make the count mean what a reader thinks it means.  ~10 lines in
`AGENTS.hs`'s `Proof` sum plus a re-tag pass.

Two notes to **retire rather than guard**, both already covered:
`AGENTS.hs:393` (`compactly`'s two date conditions) is asserted at
`TestSpec.hs:136` and `TestProperties.hs:263-267`; `AGENTS.hs:4195` (`drawText`
resting on non-overlap) is asserted at `TestSpec.hs:1280-1293`.

## 1. The skip that disarms a whole proof tier — do this one first

**AGENTS.hs:839-842**: *"The GLANCE_CORPUS groups pass when the variable is
unset and print SKIPPED on stderr; a green run without those lines is unverified
on the corpus half."*

Unset is the default, and with no CI the default is the only state that ever
runs.  The same shape, unrecorded and larger, is the node gate:
`TestServe.hs:4605` — `reading check = maybe (pure ()) (either assertFailure check)`
— makes several hundred shell and harness cases pass vacuously when `node` is
off `PATH`.  A third: both JS drivers `process.exit(0)` when their prerequisite
is absent (`browser/drive.mjs:226-234`, `interop/drive.mjs:194-203`), so a
zero-case run reports success, and **24 of the suite's cases live there**.

**The oracle.** A final `testCase` in `test/Spec.hs` that reads `GLANCE_CORPUS`
and probes for `node`, then prints one summary line naming every group that
skipped and its case count; plus `GLANCE_STRICT=1` turning a skip into
`assertFailure`.  `make test` stays as it is; a new `make test-full` sets
`GLANCE_CORPUS=$HOME/sync GLANCE_STRICT=1`.  **~25 lines in `test/Spec.hs`, 4 in
the `Makefile`.**

This is the cheapest item here and it is the precondition for the rest: every
"add a corpus case" proposal in the backlog is worth less until a skipped corpus
run says so out loud.

## 2. A symlinked root walks every mirror as truth

**AGENTS.hs:1219.**  The whole `.org-glance` denylist is a string test for that
path component (`Walk.hs:187-190`).  Serve through a symlink or a bind mount
that renames the component away and it matches nothing.  `--dir` takes whatever
the user typed, and nothing canonicalizes it.

The cost is measured, from the time it happened: **+23 files, +514 headlines,
one headline rendered twice under a tag filter.**  Worse than the count — mirror
rows are *writable*, so a commit into an overview is a write into a buffer
org-glance regenerates.

**The oracle.** An IO case in `TestStore`: build a tmp tree with a real
`.org-glance/overviews/x.org`, add `createSymbolicLink ".org-glance" (tmp </> "alias")`,
and assert `findOrgFiles [tmp </> "alias"]` agrees with
`findOrgFiles [tmp </> ".org-glance"]` on `foundFiles` and `foundDerived`.
`unix` is already a dependency and `Walk.hs` already imports
`System.Posix.Files`.  **~30 lines.**

**It will fail today.**  That is the point: it turns the note into either a fix
(canonicalize the root once in `serveAs`) or a `Test`-proved known gap.

## 3. The rename replaces a symlinked `.org` with a regular file

**AGENTS.hs:2975**, tagged `[Comment, Unguarded]`, and `Edit.hs:2-3` already
knows: *"The rename REPLACES A SYMLINK."*

Symlinked org files are ordinary — `~/org/inbox.org -> ~/sync/inbox.org`.  One
commit through glance converts the link into a regular file holding a copy.  The
real file silently stops receiving edits and the user's other tools keep writing
to it.  Low probability that a *change* introduces this, since it is already the
behaviour; the highest cost on the list, and the decision has never been taken.

**The oracle.** A `TestEdit` case: create `real.org`, `createSymbolicLink
"real.org" "link.org"`, run `editFile` against `link.org`, then assert whichever
semantics is chosen — either `getSymbolicLinkStatus "link.org"` is still a link
and `real.org` carries the bytes, or today's behaviour, pinned so it cannot
change unnoticed.  **~20 lines.**  The fix, if wanted, is one `canonicalizePath`
in `writeAtomically`'s open.

## 4. `storeKeywords` reads one record per file

**AGENTS.hs:2046.**  `recordsOf` computes `keywords` and `declared` once per
file and hands them to every `recordOf` (`Query.hs:330-334`) purely as an
optimization, while `storeKeywords` reads `listToMaybe . feRecords`
(`Store.hs:144-147`).  The day a record carries its own sets, this becomes a
silent truncation.

The palette drives `paletteRank` — so the default sort's `state` key —
`settableStates`, `/keywords`, and the badge order.  States vanish from the
picker and rows sort into the unknown bucket, with no log line.

**The oracle.** A property in `TestStore` over a generated multi-file store:
for every `FileEntry`, `all (== hrKeywords r0) (map hrKeywords rs)`, same for
`hrDeclared`; plus one concrete case where two files declare different `#+TODO:`
lines and a row deep in file 2 is asserted to carry file 2's set.  **~25 lines**;
`TestGen` already generates the documents.

## 5. `SURFACES` is a completeness claim nothing checks

**AGENTS.hs:3397/3399.**  The literal claim at 3399 is stale — both sheets are
registered surfaces today (`70-shell.js:30`, `20-sheet.js:349`), so `typing()`
catches them.  The invariant underneath is live and unenforced: **`SURFACES` is
the complete list of things that suppress table keys, and nothing asserts the
completeness.**

Add a seventh surface — three pending proposals add surfaces — forget the
registration, and `d`, `D` and `x` are armed under it.  `archiveFlag`,
`archiveRows` and `flaggedDelete` all scope `"table"` (`Keymap.hs:92-98`), and
all three write to the user's files.

**The oracle.** A shell-harness case: for each entry in `SURFACES`, open it and
assert every table-scope binding is filtered out of `MAPS.rows.filter(live)`;
then the completeness half — the set of surfaces the harness knows how to open
must EQUAL `SURFACES`' names, so a new surface with no harness entry goes red.
**~40 lines.**

## 6. An `EXTERNAL` create into a fresh shard is invisible until restart

**AGENTS.hs:2042**, a known gap.  `fsnotify` arms a newly created directory
without traversing into it, so glance's own blob writes are covered by nudging
their own path (`Commands.hs:299`, `Watch.hs:71`).  Nothing covers org-glance
capturing into `.org-glance/data/<2>/<rest>/data.org`.

For the interop story this **is** the headline failure: the two programs
disagree about what exists until glance restarts.

**The oracle.** An interop case in `test/interop/drive.mjs` (13 today, 7 with a
`BREAK`): have org-glance capture into a store the daemon is already watching,
poll `/headlines` for a bounded window, assert the row lands.  The harness, the
emacs runner and the skip-loudly gate all exist.  **~30 lines.**  This is the
single most valuable interop case the suite lacks.

## 7. A note that is confidently wrong

**AGENTS.hs:389**: *"Without the reserved-key guard `:END:` parses as a
property, `manyTill` runs to EOF and `hsProperties` swallows the rest of the
file."*

The guard is real (`Parser.hs:207-212`), but **the stated mechanism does not
hold**: megaparsec's `manyTill p end` tries `end` first, and the terminator
`try (MPC.hspace *> MPC.string ":END:")` (`Parser.hs:224`) matches before
`Property` is ever attempted.  Probed against the built library: a nested
`:PROPERTIES:` inside a drawer degrades the whole drawer to body
(`hsProperties = Nothing`, the headline's extent shrinks to its title) with the
following headlines still parsed.  The `"END"` entry appears to change nothing.

A confidently wrong note is worse than an absent one, because it will survive
the refactor that deletes the guard.  The outcome it names is still the worst
parse result the write path can produce — that row's `hrSubtree` covers the
file, so a commit splices over the whole file.

**The oracle.** Three `TestParser` cases pinning the real behaviour: a nested
`:PROPERTIES:` degrades to body with following headlines intact; an unterminated
drawer does the same; and an invariant over `TestGen`'s documents that
`spanEnd (hsFull (spans h)) <= T.length doc` for every headline, and that N
top-level headlines yield N records whatever the drawer shape.  **~30 lines.**
Then correct the note, or delete it.

## Smaller, and each worth its line

- **AGENTS.hs:4653** — *"a `cabal test` run hangs occasionally and has never
  reproduced on a retry."*  A flaky gate is a gate that gets skipped, and it is
  the only automatic one.  `--test-options=--timeout=120s` in the `test:` recipe
  makes a hang name its case.  Two words.
- **AGENTS.hs:2038** — the watch debounce has no ceiling and no leading edge, so
  a generator writing in a tight loop starves the table indefinitely.  Oracle: a
  `TestStore` case driving `due`/`drain` with a path re-nudged every tick,
  asserting a bounded number of ticks before it settles.
- **AGENTS.hs:397** — *"a `#+TODO:` line affects the headlines below it"*.  One
  `TestParser` case (a headline above the pragma keeps its keyword as title
  text) pins a rule that a two-pass parser refactor would quietly invert.

## Order

1, then 7 (it is wrong and cheap), then 2 and 3 — the two that touch the user's
own files.  4, 5 and 6 are each a day's work with a real oracle at the end.

Nothing here is safe to implement unattended: 2 and 3 land a red test on
purpose, and 5 is blocked on the harness's tag map.
