# Proposal — one key for a widget, and two bugs the three vocabularies hid

**Status:** proposed · **Date:** 2026-08-15 · **Origin:** `/generalizer`,
cross-cut angle.  Two live defects fell out of the measurement; they are stated
first because they are worth fixing whatever happens to the shape.

## Two bugs, found by asking what member N+1 costs

### `sole(keep)` is a complete no-op whenever it is given an argument

`AGENTS.hs:3183` models it as `sole :: [Surface] -> [Surface]`, no argument.
The code (`frontend/glue/70-shell.js:68-71`) is:

```js
function sole(keep) { if (keep) return; for (const s of SURFACES) if (s.momentary && s.up()) s.off(); }
```

The intent is plainly *close every momentary surface except `keep`*.  Any truthy
argument returns immediately.  `frontend/glue/30-capture.js:20` calls
`sole("capture")` and `:192` calls `sole(over)` — **so raising the capture form
closes no momentary surface.**  The spec and the code disagree in arity and
nothing compares them.

### The `archive` saved view shipped without a way to reach it

`savedViews` grew 2 → 3 on 2026-08-10.  Most of the fan-out is derived and cost
nothing — `TreeSettings`, `viewQueryIn`, the write dispatch, `GET /config`, the
pin palette.  Two steps are not derived, and both were missed:

- `frontend/glue/50-settings.js:511` — `const NAMED_VIEW = { default: …, agenda: … }`
  — **two entries for three views.**  `archive` falls through to
  `apply-view:archive`, a command name nothing binds.
- `src-web/Glance/Web/Keymap.hs:66` binds `g` to `default` and `:84` binds `A`
  to `agenda`.  **`archive` has no key**, no `keyHints` row, no `onceCommands`
  entry.

The view has been reachable only through the pin palette for five days, and
`cabal test` is green throughout.

## The shape underneath

The shell keeps **three registries over "a surface", in three files, in three
vocabularies, joined by nothing**:

| registry | members | where |
|---|---|---|
| `SURFACES` | 6 | `70-shell.js:17-37`; model `AGENTS.hs:3171` |
| `*FLAGS` | 5 | `40-popups.js:219`, `20-sheet.js:713`, `:726`, `:949`, `50-settings.js:238`; model `AGENTS.hs:3234` |
| mounts | 4 | three files; model `AGENTS.hs:3903` |

**No shared key exists.**  `tags` appears in all three under three names
(`tags`/`tags`/`ttable`); the property panel is flag-surface `panel` and mount
`mptable`; the config state table is flag-surface `states` and mount `cstates`;
`links` is a surface and a mount but no flag surface.  Adding a widget means
inventing a name in up to three vocabularies and registering in three files.

And the four mounts are the same five lines four times (`20-sheet.js:574`,
`40-popups.js:11`, `:90`, `50-settings.js:155`) — `if (x) return x; x = listing(…); return x`
— while `AGENTS.hs:3900` already models exactly `listing(host, cols, hint, pane)`
as `data Mount` and **nothing in `test/` reads it**.

## What member N+1 costs today

A surface needs: the registry row; predicate and closer functions that must
reach `70-shell.js`'s scope (`30-capture.js` and `40-popups.js` are IIFEs, so
each needs an entry in its `return {…}` **and** in the destructure); and **its
own keydown listener** — there is no shared surface-key dispatcher, so each of
the six hand-spells the `momentary()` guard (`20-sheet.js:654`,
`50-settings.js:52`, `:251`, `30-capture.js:85`, `70-shell.js:226`).
`AGENTS.hs:3399` names this cost and tags it `[Unguarded]`.

A mount needs ~18 further lines in whichever file hosts it, plus an `AGENTS.hs`
row nothing enforces.

ESC, `typing()` and `live()` are already generic (`70-shell.js:170`, `:72`,
`:78`) and cost nothing — that half is right.

## Proposed change

One `WIDGETS` registry keyed by a single surface name, carrying the surface
fields, the optional flag shape and the optional mount.  The three lists become
three projections:

```js
const SURFACES = WIDGETS.filter(w => w.up);
const mountOf  = (name) => …;   // memoized, from w.host/cols/hint/pane
```

Each mount call site becomes `mountOf("tags")`.  The per-surface keydown
listener becomes a `keys` field the one dispatcher folds — **six hand-written
listeners collapse to one**, and surface N+1 stops owning a `momentary()` guard
it can forget.  Then one case joins `Object.keys(WIDGETS)` and each row's fields
to `Spec.surfaces` / `Spec.flagSurfaces` / `Spec.mounts`, so three decorative
registries become enforced in the same commit.

For the saved views, the same move one layer down:

```haskell
data SavedView = SavedView
  { svId :: !Text, svPragma :: !Text, svBuiltin :: !Text
  , svCommand :: !Text          -- ^ the elisp name the echo shows; the shell's NAMED_VIEW
  , svKey :: !(Maybe Text)      -- ^ its table key, where it has one
  }
```

`NAMED_VIEW` is spliced from `savedViews`; `keyBindings` gains its rows by
`mapMaybe` over the same list — so a view without a key is a **declared**
`Nothing` rather than an oversight.

## LOC

Widgets: added ~35 (registry + one dispatcher), removed ~70 (six guard
preambles, the three-way name plumbing, four lazy-mount closures, `sole`'s
broken parameter).  Saved per member: **~18 lines → ~4.**
Saved views: added ~10, removed ~6; saved per member: three edits in two
languages → one row.

## Risk

The keydown consolidation is the risky half — six listeners becoming one
dispatcher moves real key handling, and this repo's own scar is a glue refactor
that left `n`/`p` stepping nothing while `tsc` stayed clean.  Land the registry
and the projections first, measure, and take the dispatcher separately.

The two bug fixes are independent of all of it and should go first.

## Overlap, stated

`2026-08-15-the-skips-that-pass.proposed.md` §5 asks for an *oracle* proving
`SURFACES` is complete.  That is the test; this is the shape.  They compose —
the oracle gets much cheaper once one key spans all three registries — and
neither replaces the other.
