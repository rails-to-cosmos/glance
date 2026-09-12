# Proposal — the BREAKS registry is itself checked

**Status:** proposed · **Date:** 2026-08-20 · **Origin:** /generalizer — the
variant-cost sweep on the harness's own registries.

## Pattern

`test/browser/drive.mjs:48-149` holds 32 BREAKS — each a CSS sabotage plus
the name of the case that must go red — and nothing keeps the registry true:

- a target is a **substring of a case name copied by hand**; renaming the
  case leaves the break aimed at nothing, discoverable only by running that
  one break (`BREAK=` runs one per process, and `browser-check` is its own
  sitting outside the gate);
- 15 of 38 cases have no break at all, so a third of the suite has no proof
  it can fail;
- `test/interop/drive.mjs:62-70` repeats the shape (7 entries, same
  one-at-a-time env gate).

## Proposed change

Two cheap checks and one report, all inside the harness that already knows
both lists:

1. **Dangling-target check at startup**: when `drive.mjs` loads cases and
   BREAKS, every break's target must match exactly one case name — a
   mismatch aborts the run with the orphan named. Runs on *every*
   browser-check, not only under `BREAK=`.
2. **Same check in the interop drive** — same dozen lines, same shape.
3. **Coverage line in the summary**: `breaks cover 23/38 cases` printed at
   the end of a green run, so the uncovered tail is visible instead of
   silent. (Driving all 32 breaks in one sitting stays out of scope — one
   process per break is the mechanism's own law.)

## LOC estimate

+25 across the two drives / −0; every future rename or new case keeps the
registry honest for free.

## Risk

None to the suites' verdicts; the startup check can only turn an already-dead
break into a loud one.

## Existing precedent

`drive.mjs` already validates its world at startup (fixture presence, the
`SETTLE` discipline); TestServe's `objectKeys` check on `HANDLERS` is the
same move — the registry's consumer verifies the registry.

## 2026-09-12 re-measurement — and the first dead break

Re-counted against the tree of 2026-09-12, by loading `test/browser/cases.mjs`
and matching each break's target the way `drive.mjs:530-532` matches it
(`l.name.includes(want)`, a substring):

| | 2026-08-20 | 2026-09-12 |
|---|---|---|
| browser cases | 38 | **98** |
| breaks | 32 | **34** |
| cases a break can turn red | 23 | **24** |
| cases with no break at all | 15 | **74** |
| interop cases / breaks | 13 / 7 | 13 / 7 |

The suite grew 2.6× and the registry grew by two. **Three quarters of the
browser suite has no proof it can fail**, against two fifths when this was
filed. The interop half is unchanged and still whole: all seven targets name a
live `step(…)` (`test/interop/drive.mjs:61-69` against `:232-450`).

**And the dangling-target check this proposal asks for now has a customer.**
`"two-golds"` (`drive.mjs:152-158`) names

> `"the date widget stands in the value's own slot"`

and no case says that. The case was renamed to

> `"the date widget opens in the value's slot, wholly selected, and paints the
> selection"` (`cases.mjs:3963`)

so the substring no longer matches and the break aims at nothing. It is the
break whose own comment says *"ONLY THE PIXELS SEE IT"* — the one guarding a
fault no computed reading can catch — and it has been dead since the rename,
discoverable only by running `BREAK=two-golds` and reading the *"which no case
run here is"* line at `drive.mjs:532-536`. That line exists; it fires one break
per process, inside `browser-check`, which sits outside the gate. The startup
check proposed above fires on every run.

Repairing the target is one word and belongs with the check, so the check has
something to prove. The coverage line proposed in item 3 would now read
`breaks cover 24/98 cases`.
