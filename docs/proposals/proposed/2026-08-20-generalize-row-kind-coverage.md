# Proposal — Body.Kind decisions become total

**Status:** proposed · **Date:** 2026-08-20 · **Origin:** /generalizer — the
variant-cost sweep counted the compiler's help per family; `Body.Kind` got the
least.

## Pattern

`Body.Kind` (`frontend/elm/src/Body.elm:48-52`: `Head | Para | Child | Meta`)
is decided by exhaustive `case` in only 3 places (`Body.elm` `joinAt`,
`kindWord`; `Doc.elm:275-287`) and by silent `==`/`||` tests in ~31 —
`Doc.elm` alone carries 26. A fifth kind would compile clean and be silently
"not Head, not Child, not Meta" at every one of them. `Scan.RegionKind` is the
in-repo counter-example: five constructors, six wildcard-free total functions
(`Scan.elm:429` states the intent), and a missed arm is a compile error.

## Proposed change

Name the *cohorts* the `==` sites test, once each, beside `heading` (this
batch's helper for `Head || Child`):

- `heading : Row -> Bool` — done; adopted at the four spellings.
- `planningRow : Row -> Bool` for the `kind == Meta` cluster where the
  question is "is this a planning/drawer line" (`Doc.elm:982,1198,1452,1806…`)
  — each site reads as its question, and a new kind forces one decision per
  cohort instead of ~31.
- Where the answer genuinely differs per kind (`rowClass`'s Element arm,
  crumb naming), a total `case` over `Kind`, wildcard-free, the
  `Scan.elm:429` discipline.
- `AGENTS.hs`'s `RowKind` mirror (now four, `DMetaRow` added this batch) gets
  a `TestSpec` comparison against Elm's `kindWord` words, the way `metas` and
  `themes` are compared.

## LOC estimate

+15 (two predicates, one spec test) / −0 immediate; the payoff is kind #5
costing a handful of forced arms instead of an audit of 31 sites.

## Risk

Pure refactor of boolean spellings; Elm compiler enforces the total cases.
Behaviour identical until a fifth kind exists.

## Existing precedent

`Scan.RegionKind` and its six total functions; `heading` in this batch.
