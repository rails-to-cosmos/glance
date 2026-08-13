# Proposal — the checks that report green when they should report red

**Status:** partial — items 1-4 landed 2026-08-14, each proven by making it
fail; items 5-7 are decisions and stay open ·
**Date:** 2026-08-13 · **Origin:** `/generalizer`, future-variant-cost angle,
after a session in which nine adversarial rounds each ran against a green
suite.

## Why this one first

Every other finding costs edits. These cost **trust**: each is a mechanism that
answers *green* in a state it exists to catch. A session that leaned on
`BREAK=` as its proof of non-vacuity should know which of its proofs were
capable of failing.

## 1. `make interop BREAK=x` can report a green run as proof of a break

`test/interop/drive.mjs` validates the env var against `Object.keys(BREAKS)`
and never checks that a BREAK's VALUE names a real `step()`. Rename a case and:

```
make interop BREAK=meta-moved
  ...
  13/13 cases
  BREAK=meta-moved — "meta-untouched" is the case that should be red
  exit 0
```

A green run, announcing the case it did not turn red. Seven of thirteen cases
have a BREAK; the other six have none.

**Fix.** Collect the `step()` names as they run and assert at the report that
the named case both exists and failed. Ten lines, and it makes `BREAK` mean
what the target's own comment claims: *"A CASE NOBODY HAS SEEN FAIL IS NOT
EVIDENCE."*

## 2. Browser `BREAK`s are keyed by ORDINAL

`test/browser/drive.mjs`'s `BREAKS` name their target as `"1"`, `"5"`, `"8"` —
positions computed over the `ONLY`-filtered list and only ever printed.
Inserting a case anywhere but the end rots all eight annotations in silence,
and `BREAK=x ONLY=y` renumbers so the printed ordinal means nothing.

**Fix.** Key by case name, as the interop driver already does.

Also: `known:` — an inverted xfail, where a green `known` case *fails* — is
implemented and has zero users.

## 3. The shell harness can assert the opposite of the truth

`test/fixtures/shell-harness.js` mints page elements on demand, which is right.
Their TAG comes from a hand-kept 18-entry map with a `|| "div"` fallback. Add
`<input id="cfoo">` to `Page.hs`, forget the row, and the harness mints a
`<div>` — so `typing()` reads the tag off `activeElement`, concludes the key
belongs to the TABLE rather than to a field, and every keyboard case around it
asserts the reverse of the truth and passes.

This is worse than the ordinary missing-registration failure, which merely
under-covers.

**Fix.** Default to `input` for an id the map does not name, or refuse the mint
outright. A test that cannot run beats one that runs backwards.

## 4. No test reads a version

`glance.cabal`, `README.org` and `CHANGELOG.md` carry the version, and
CLAUDE.md's Build section requires all three to move on a cut. Nothing checks
it.

This is structurally identical to the proposal-status rule already guarded in
`TestSelfContained.hs` — a filename/content pairing, compared rather than
trusted — and it is about eight lines in that same module. The cheapest
unclaimed guard in the repo.

Same file, same shape, also unguarded: `assets/glue.d.ts`'s port interfaces
against the Elm `port` declarations (`tsc` catches the glue *using* a port Elm
lacks; it never catches `glue.d.ts` DECLARING one, which is a runtime
`undefined.subscribe is not a function`), and `elm.json`'s `0.19.2`, which
CLAUDE.md calls a hard refusal at `0.19.1`.

## 5. A guard that looks like a cross-language join and is not

`TestServe.hs` compares `map csName configSettings` against `everySetting` — a
**Haskell** fixture in the same file. So a fifth `ConfigSetting` gets a forced
fixture row, a working route, and a settings sheet that never sends it, all
green.

The `commands` ↔ glue join is one-directional and text-only: glue naming a
command the server lacks is unchecked.

## 6. One theme assertion weakens rather than fails

`TestServe.hs`'s slot check is `length (breakOnAll (slot <> ":") page) >= 4`,
which proves four of five blocks declare the slot. Three sibling literals
(`4`, `4 * n`, `["light","dark"]`) fail loudly on a third theme; this one
passes quietly. The relation it encodes — two media blocks plus one per theme —
is written nowhere.

## 7. The parity vectors cannot fail here

`../table-view/fixtures/parity/` has a manifest, and it names two harnesses,
both table-view's own. glance's five mentions are prose; `TestFilter`'s cases
are hand-ported by eye. **There is no path in this repo by which a parity
vector can fail** — which is worth knowing, since parity is the standing
contract between the two implementations of the filter grammar.

## The counts, again

CLAUDE.md says the interop run is "the twelve cases"; it is thirteen. The
uncommitted diff **deletes the number rather than correcting it**. Elsewhere
the same class was fixed by taking the count out of the prose entirely and
letting the target report it — that decision should be applied to all of them,
or to none.

## Cost

Items 1–4 are roughly forty lines together and each closes a way for a check to
lie. Items 5–7 are larger and are worth deciding rather than doing: they are
about how much cross-language agreement this pair wants to buy with tests
versus with types.
