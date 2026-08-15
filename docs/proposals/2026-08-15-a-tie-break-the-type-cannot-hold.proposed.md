# Proposal — a tie-break the model states and the type cannot hold

**Status:** proposed · **Date:** 2026-08-15 · **Origin:** `/domain-modeling`,
reserved-word sweep over `AGENTS.hs:69-78`.

## The reserved word, and its third meaning

`AGENTS.hs:70` reserves one: *"REGION is the scanner's structural run alone
(`RegionKind`)."*  The tree spells it three ways.

| site | what it holds | meaning |
| --- | --- | --- |
| `frontend/elm/src/Scan.elm:593` | `Region { kind : RegionKind, from : Int, to : Int }` | a structural run of lines — the reserved meaning |
| `AGENTS.hs:1921` | `LensRegion = RPlanning \| RProps \| RLog` | one of three slices lifted out of a subtree |
| `src-query/Glance/Query.hs:778` | `data Region = Region !Int !Text` | a line number and the text to put there |

The third carries no extent and names no run.  It is a **splice**, and the
function eating it is already called `spliceRegions`.  It is module-local:
absent from `Glance.Query`'s export list, built once at `Query.hs:769`, consumed
once at `Query.hs:780`.

## The rule that lost its field

`AGENTS.hs:1972` states the ordering the recompose depends on:

> Regions go back at BODY indices — the subtree line less what every region
> ahead took out — so two naming one line land in `lensOrder`, and one past the
> body's length lands at the end.

and the model enforces it in the sort key:

```haskell
spliceRegions :: [String] -> [(LensRegion, Int, [String])] -> [String]
spliceRegions body regs = go 0 body (sortOn (\(r, i, _) -> (i, rankIn r)) regs)
```

`Query.hs:781` sorts by the line and nothing else:

```haskell
spliceRegions body regions = knit (go 0 (linesWith body) (sortOn above regions))
  where above (Region line _text) = line
```

The `LensRegion` tag is not in the constructor, so `rankIn` has nothing to sort
by.  Two splices naming one body line land in `lensOrder` today for two reasons
the type does not state:

1. `Data.List.sortOn` is stable, and
2. `Query.hs:769` builds the list `[plan, props, logs]`, which happens to be
   `lensOrder` spelled again as a literal.

Reorder that literal — a plausible edit, since the three are independent
bindings at `:770-776` — and planning and properties swap on a collision, with
`AGENTS.hs:1972` still claiming otherwise.

## Nothing asks

`spliceRegions`, `lensOrder`, `rankIn` and `LensRegion` appear nowhere under
`test/`.  The model's `spliceRegions` is a second implementation of the real one
that no case runs, and its tie-break is the half the real one does not have.
Same disease as `2026-08-13-oracles-that-cannot-fail` and
`2026-08-15-a-registry-nothing-walks`, in a function rather than a registry.

## Proposed change

In `src-query/Glance/Query.hs`, free the word and put the rule in the type:

```haskell
data LensRegion = RPlanning | RProps | RLog deriving (Eq, Ord, Show, Enum, Bounded)

-- | A block of text owed to a body line.  The region it came from rides along:
-- two splices naming one line go back in 'LensRegion' order.
data Splice = Splice !LensRegion !Int !Text

spliceRegions :: Text -> [Splice] -> Text
spliceRegions body splices = knit (go 0 (linesWith body) (sortOn key splices))
  where key (Splice r line _text) = (line, r)
```

and at `:769`, tag what is already built in that order:

```haskell
    splices = [ Splice r at text | (r, at, text) <- [plan, props, logs], not (T.null text) ]
```

`deriving Ord` on `LensRegion` makes `lensOrder` the constructor order, so
`rankIn` stops being a second spelling of it.

One case in `TestSpec.hs`, feeding a subtree whose planning line and property
drawer both recompose to body line 1, asserting `SCHEDULED:` lands above
`:PROPERTIES:`.  That is the assertion the reordering breaks and nothing
currently makes.

## LOC

Added ~6, removed ~2 in `Query.hs`; ~12 for the case.  `AGENTS.hs` loses
`rankIn` (2 lines) and its `Note` gains `[Test]` where it had nothing.

## Risk

The rename is private to one module.  The sort key change is behaviour-neutral
by the stability argument above — the new case is what turns that argument from
prose into red.  No wire fields, no org bytes.

## What it does not do

Elm keeps `Region`/`RegionKind`; that is the reserved meaning and stays.  This
takes the word back from the one site that borrowed it.
