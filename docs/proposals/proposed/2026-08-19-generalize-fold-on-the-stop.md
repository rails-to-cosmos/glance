# Proposal — the scan says what folds, and one rule hides what a shut owner holds

**Status:** proposed · **Date:** 2026-08-19 · **Origin:** `/generalizer` audit over
`a74685c` and `4170ca1`, the abstraction-shape and next-variant angles.

## The finding, in one line

`foldable` re-reads the row's line at view level to recognize a drawer
(`Scan.drawerName (lineOf m r)`, `Doc.elm:886-891`) though the scan classified
that exact region when it minted the composite (`Scan.elm:857-881`) — the kind
is decided once and thrown away, so every new foldable kind (org folds
`#+begin_` blocks and headlines too) re-teaches the view what the scan knew.

## The pattern

The fold machinery is already generic over ids: the `shut` set
(`Doc.elm:83-84`), TAB (`:354-382`), `foldTarget` (`:620-631`), `reveal`
(`:573-585`), the fill's open-set carry (`:304-315`) and `f`'s open-on-entry
(`:325-338`) all work through `ownersOf` and would fold anything.  Three
places are kind-special, and they are where a new foldable kind bleeds:

1. **Membership.** `foldable` (`Doc.elm:886-891`) admits the synthesized
   drawer by `kind == Meta` and a raw one by re-parsing its line.  `Stop`
   carries `name` and drops the kind (`Scan.elm:807-808`), and names collide
   across kinds — a `:SRC:` drawer and a `#+begin_src` block both arrive as
   `"src"` — which is why the view must re-read the line at all.
2. **The folded face, twice.** `view` draws a shut raw composite as its opener
   line plus `…` (`Doc.elm:1520-1529`); `viewMeta` draws the shut drawer as a
   literal `":PROPERTIES:…"` (`:1567-1568`).  A third foldable kind adds a
   third rendering.
3. **The class.** `rowClass` stamps every foldable `d-drawer`
   (`Doc.elm:1073-1078`), and four stylesheet rules key on it
   (`Style.hs:202-203`, `:237`, `:240`, `:246`) — a folding block wearing
   `d-drawer` is a lie the harness reads.

Child subtrees have no fold at all: `view`'s walk draws every row flat
(`Doc.elm:1502-1537`), so org-cycle on a child headline — org's first meaning
of TAB — needs a hiding mechanism that does not exist yet.

## Files

`frontend/elm/src/Scan.elm`, `frontend/elm/src/Body.elm`,
`frontend/elm/src/Doc.elm`, `src-web/Glance/Web/Page/Style.hs`, `AGENTS.hs`,
`test/browser/cases.mjs` (`:373-409`, `:1575-1625`).

## Proposed change

Carry the fact, then spend it through one rule.

**Scan** mints foldability where it mints the composite:

```elm
type alias Stop =
    { from : Int, to : Int, grain : Grain, name : Maybe String
    , up : Maybe Int, folds : Bool }

-- one arm per kind at the mint: Drawer True, Block True, Item/Table False
```

**Body** threads it: `Row` gains `folds : Bool` from the Stop; `metaRows` sets
it on the synthesized drawer and gives that row its frame as `text`
(`":PROPERTIES:"`), so the folded face is the row's own line everywhere.

**Doc** spends it twice and nowhere else:

```elm
foldable : Row -> Bool
foldable r = r.folds                    -- no Model, no line re-read

hidden : Lit -> Row -> Bool             -- ONE rule: a row is hidden when any
hidden lit r = ...                      -- owner in its chain is shut
```

`view`'s fold arm and `viewMeta`'s both become `hidden` plus one folded-frame
rendering; `crumb`'s `":PROPERTIES:"` re-derivation (`:1645-1649`) reads the
row's text.  The shut-owner chain is computed once per render and threaded
through `Lit`, the way point's owners already are (`litOf`,
`Doc.elm:1013-1020`).  `rowClass` renames the class to `d-fold` off `r.folds`;
the wire's `"fold"` field (`Doc.elm:723-724`, read at `20-sheet.js:76`) keeps
its meaning and is fed by the carried fact.

**Then each next variant is one arm.**  Blocks: flip `Block`'s arm at the mint
— TAB on a `#+begin_src` folds it to its opener line, org's own cycle.  Child
subtrees: give `Child` rows `folds = True`; `hidden`'s owner walk already
covers every block a child owns (`rowsFrom` owns them to it,
`Body.elm:117-212`), so folding a headline is admission plus zero new
mechanism.

## LOC estimate

+30 now (the field, the mint arms, `hidden`, the `Lit` threading), −25 (the
line re-read, the second folded rendering, the crumb re-derivation).  Per
future foldable kind: one arm at the mint plus its browser case; today the
same kind costs edits in `foldable`, `view`, `viewMeta`-or-a-sibling, `crumb`,
`rowClass` and `Style.hs`.

## Risk

Behaviour: admitting blocks and children changes what TAB does — ship each
admission as its own deliberate arm, drawers-only being the conservative
mint.  The `d-drawer` → `d-fold` rename sweeps four stylesheet rules and the
browser cases that read the class.  `hidden` adds an owner walk per row per
render — the `Lit` threading exists because `4170ca1` paid this exact
quadratic down once already; follow it.

## Existing precedent

`docs/proposals/proposed/2026-08-13-carry-the-fact.md` names the class ("a
fact is known on the branch that decides, thrown away, and re-derived
downstream with less to go on"); `Stop` already carries `grain` and `name`
from the same decision point, and `folds` is the third passenger.  `litOf`
(`Doc.elm:1013-1020`) is the once-per-render threading this reuses.
