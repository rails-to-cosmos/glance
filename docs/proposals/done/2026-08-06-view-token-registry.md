# Proposal — the view tokens become a list, on both sides of the wire

**Status:** done — delivered 2026-08-06, same day — all six moves, suite green either side · **Date:** 2026-08-06 · **Origin:** generalizer round
five, over the `columns:` feature (uncommitted at writing)

## The family

A VIEW TOKEN is a query token that narrows nothing and states a fact about
the view instead. There are two — `sort:` (the order) and `columns:` (the
column set) — and the family has a shape a third member would copy:

- a reader module (`Glance.Web.Sort`, `Glance.Web.Columns`) over the ONE
  parse, each carrying a verbatim copy of `refused`/`spelling`
  (Sort.hs:155–160, Columns.hs:64–70 — identical modulo the key constant);
- two membership arms in `Glance.Web.Filter` — `fieldOf`'s `Order` rows and
  `compile`'s `notElem [Just sortKey, Just columnsKey]` (the list half
  already exists there);
- a positional slot in `Routes.pageParams`'s tuple, now five wide;
- on the renderer, two pushes in `queryKeys()`, a two-key `continue` in
  `queryMatcher` (table-view.js:2190), and a chip classifier per member
  (`ordersRows`/`showsColumns`, near-twins over `asToken` + key check).

Marginal cost of a THIRD view token today: ~8 touchpoints across two repos,
two of them unenforced (a missed `queryKeys` push silently demotes the token
to free text; a missed matcher skip silently narrows).

## Proposed change

**Server.**

1. `Glance.Web.Filter` exports the list and answers membership from it:

   ```haskell
   viewKeys :: [Text]
   viewKeys = [sortKey, columnsKey]
   ```

   `fieldOf`'s two `Order` arms become `key `elem` viewKeys`, and `compile`'s
   filter reads `(`notElem` map Just viewKeys) . tmKey`. A new token is one
   list entry; the two arms can no longer disagree.

2. The refusal vocabulary moves beside `Term`, spelled once:

   ```haskell
   refusedOn :: Text -> Term -> Text -> Text   -- key, token, why
   refusedOn key t why = why <> ": '" <> spellingOf key t <> "'"

   spellingOf :: Text -> Term -> Text
   spellingOf key t = (if tmNegated t then "-" else "") <> key <> ":" <> tmValue t
   ```

   Sort and Columns keep one-line local aliases (`refused = refusedOn
   sortKey`), deleting the twin bodies.

3. `pageParams` answers a record instead of the widening tuple:

   ```haskell
   data PageAsk = PageAsk
     { paQuery  :: !Text
     , paLimit  :: !(Maybe Int)
     , paOffset :: !Int
     , paChain  :: !SortChain
     , paPicked :: !(Maybe [Text])
     }
   ```

   The next view token is a field, and `headlines` reads names rather than
   positions (`RecordWildCards`, the stanza's default).

4. Immediate cleanup the diff exposed: `viewJSONTextWith` has no caller left
   (Routes moved to `viewJSONTextFor`; the suite reads `viewJSON`). Drop the
   export; `viewJSONWith` stays as `viewJSON`'s own body.

**Renderer (table-view).**

5. `const VIEW_KEYS = [SORT_KEY, COLUMNS_KEY];` — `queryKeys()` pushes the
   list, `queryMatcher` skips on membership. The two silent-failure sites
   become one registration.

6. The chip classifier becomes the member's own:

   ```js
   const chipClassOf = (tok) =>
     ordersRows(tok) ? " tv-chip-sort" : showsColumns(tok) ? " tv-chip-cols" : "";
   ```

   (One call site today; worth it only as part of this change.)

## LOC estimate

+~20 now (list, record, shared vocabulary), −~30 now (twin helpers, tuple
threading, dead export). Per future view token: −8..10 across the two repos,
and the two unenforced renderer sites collapse to one list entry.

## Risk

None on the wire — the tokens, refusal sentences and JSON are byte-identical.
`pageParams`'s shape is module-internal. Suite baseline: the `dediting`-style
verbatim pins do not cover these lines; TestFilter/TestServe assert sentences
and behavior, both unchanged.

## Existing precedent

`compile`'s `notElem [...]` list is this proposal half-born; `commands` (name
→ entry) and `SURFACES` (the modal list) are the repo's registry idiom this
extends.

## Rejected on the way

- **A generic view-token reader framework** over Sort's and Columns' compose
  loops — the two semantics (chain with directions and `*none*`; name list
  with commas and a Title floor) share no loop worth parameterizing at two
  instances.
- **Merging the `viewJSONWith`/`viewJSONFor` default-instance pairs** — the
  pair is the facade's own API idiom (`viewJSON` → `viewJSONWith`), and the
  default instance is what Store's query-agnostic socket frames MUST keep.
- **`resolveColumns`' alias table as a CAF** — a twelve-pair list per
  request is noise.
- Reported in passing, perf rather than shape: `customCell` re-slices the
  subtree and re-scans the drawer PER ROW PER REQUEST; a store-wide custom
  column over ~10k rows pays ~10k drawer scans per page. Fine for v1; a
  per-record memo is the fix if it ever shows up in a profile.
