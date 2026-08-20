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
