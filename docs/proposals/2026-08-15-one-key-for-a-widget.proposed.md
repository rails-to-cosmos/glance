# Proposal — one key for a widget, and two names that promise more than they do

**Status:** proposed · **Date:** 2026-08-15 · **Origin:** `/generalizer`,
cross-cut angle.  Two findings the sweep called defects are stated first, checked back
down to what they actually are.

## Two name mismatches, first reported here as bugs and checked back down

Both were raised by the sweep as defects.  Reading the surrounding code shows
neither changes what a reader sees.  They are recorded because each is a place
where a NAME promises something the code does not do, which is what the registry
below is for.

### `sole(keep)`'s parameter is a flag wearing an except-this name

`AGENTS.hs:3183` models it as `sole :: [Surface] -> [Surface]`, no argument.
The code (`frontend/glue/70-shell.js:68-71`) is:

```js
function sole(keep) { if (keep) return; for (const s of SURFACES) if (s.momentary && s.up()) s.off(); }
```

`keep` reads as *close every momentary surface except this one*.  What it does is
boolean: `sole()` closes them all, `sole(anything)` closes none.

**It is used correctly at every site.**  `askFrom` passes `true` and its comment
says why — *"Raised OVER the popup that asked for it: this is that popup's own
field"* (`30-capture.js:240`).  `openCapture`'s `sole("capture")` means the same
"close nothing", and the resulting overlap is documented: `70-shell.js:65` says
*"The list ORDER breaks one tie: `+' over the tags popup leaves both up."*
`popupKeys` (`:261`) guards on `momentary() !== name`, so the earlier surface in
`SURFACES` owns the keyboard while both are visible.  That is the design.

What is owed is the name and the model: `keep` should be `quiet`, or the
except-this semantics its name promises should exist, and `AGENTS.hs:3183`'s
arity should match either way.

### `archive` is reachable; what it lacks is a key and an elisp name

`savedViews` grew 2 → 3 on 2026-08-10, and most of the fan-out is derived and
cost nothing — `TreeSettings`, `viewQueryIn`, the write dispatch, `GET /config`,
the pin palette.  Two steps are not derived:

- `50-settings.js:511` — `NAMED_VIEW = { default: …, agenda: … }`, two entries
  for three views.  `archive` falls through to the generic
  `apply-view:archive`, so **its echo names no elisp command** where its two
  siblings do.
- `Keymap.hs:66` binds `g` to `default`, `:84` binds `A` to `agenda`.
  `archive` has **no key**, no `keyHints` row, no `onceCommands` entry.

Applying it works: `filter()` resolves `view:archive` through `viewNamed` and
calls `applyNamed` (`00-core.js:285`), which is the documented `view:NAME`
grammar.  Whether the third view earns a key is a decision, not a defect — but
the registry cannot say it was taken, because `svKey` does not exist to hold a
declared `Nothing`.

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
preambles, the three-way name plumbing, four lazy-mount closures).  Saved per member: **~18 lines → ~4.**
Saved views: added ~10, removed ~6; saved per member: three edits in two
languages → one row.

## Risk

The keydown consolidation is the risky half — six listeners becoming one
dispatcher moves real key handling, and this repo's own scar is a glue refactor
that left `n`/`p` stepping nothing while `tsc` stayed clean.  Land the registry
and the projections first, measure, and take the dispatcher separately.

The two name mismatches above are independent of all of it: `sole`'s parameter
and `AGENTS.hs:3183`'s arity can be settled in one edit, and `archive`'s key is
a decision to record rather than work to do.

## Overlap, stated

`2026-08-15-the-skips-that-pass.proposed.md` §5 asks for an *oracle* proving
`SURFACES` is complete.  That is the test; this is the shape.  They compose —
the oracle gets much cheaper once one key spans all three registries — and
neither replaces the other.
