# Proposal — the ids a command named, resolved once

**Status:** done — DONE 2026-08-10 · **Date:** 2026-08-10 · **Found by:** /generalizer over
`e1ba099..6412f4a` (the `delete` command)

## Pattern

"Resolve the ids a request named against the store, answer one result per id in
the order they were named, refuse an unknown id the same way" is a contract the
route documents — and it now has two implementations that keep it by different
means.

`overRows` (`Commands.hs:381-393`) keeps the order DELIBERATELY, and says so:

```haskell
-- Answered in the order the client named the ids, so a caller can zip the
-- results against what it asked for.
pure (jsonResponse status200
        ["results" .= [ v | rid <- cmdIds cmd, Just v <- [lookup rid outcomes] ]])
```

`deleteRows` (`Commands.hs:355-379`) keeps it BY ACCIDENT — `mapM` over
`cmdIds` happens to preserve it:

```haskell
answers <- mapM taken (cmdIds cmd)
pure (jsonResponse status200 ["results" .= [ v | (_rid, v) <- answers ]])
```

Both then re-derive the same two things: `headlinesIn (storeRecords st)
(cmdIds cmd)` (`:360` and `:585`) and `refused rid (noSuchRow rid)` (`:363`,
`:365`, `:594`).

The order is a WIRE CONTRACT — SCHEMA.md's `{results: [{id, ok, …}]}` "in the
order the ids were named" — and a contract kept twice, once on purpose and once
as a property of `mapM`, is one a refactor of either site can drop without a
test noticing.

## Files

- `src-web/Glance/Web/Commands.hs:355-379` — `deleteRows`
- `src-web/Glance/Web/Commands.hs:381-393` — `overRows`
- `src-web/Glance/Web/Commands.hs:573-596` — `planCommand`

## Proposed change

One resolution, returning the named ids in order with each either refused or a
row, so the ORDER and the "no such row" refusal are written once:

```haskell
-- | CMD's ids in the order they were NAMED, each an answer already or a row to
-- work on.  The order is the wire's ('SCHEMA.md': a caller zips the results
-- against what it asked for), so it is kept HERE rather than by each caller's
-- choice of traversal.
namedRows :: Store -> Command -> [(Text, Either Value HeadlineRecord)]
namedRows st cmd =
  [ (rid, maybe (Left (refused rid (noSuchRow rid))) Right (lookup rid found))
  | rid <- cmdIds cmd ]
  where found = [ (hrId r, r) | r <- fst (headlinesIn (storeRecords st) (cmdIds cmd)) ]
```

`deleteRows` becomes the row function alone:

```haskell
deleteRows opts hub st cmd = do
  answers <- mapM (either pure . flip id <*> take') (namedRows st cmd)
  pure (jsonResponse status200 ["results" .= answers])
```

(spelled plainly rather than point-free in the patch), and its own
`rid `elem` absent` / `row rid == Nothing` pair — two spellings of one
question, `Commands.hs:363-365` — collapses to the one `Either`.

`planCommand` keeps its file grouping and takes `namedRows`' Rights.

## LOC estimate

- Added: ~6.
- Removed immediately: ~10 (the duplicate resolution, the doubled absent test,
  one of the two order spellings).
- Saved per future variant: ~8 per row-taking command, of which the valuable
  part is that the wire's order cannot be dropped by one of them.

## Risk

Internal to the module. The wire shape is unchanged and is exactly what the
change is protecting. The suite pins the order (`TestServe`'s per-id cases and
`TestWire`'s `outcomesOf`), so a green run is the check.

## Existing precedent

`Glance.Query.resolveIds` is the repo's own answer to the same shape one layer
down — CLAUDE.md names its four call sites and says why they are one function:
"so the store equals the load it stands in for and the stream equals both".
This is that argument applied to the route's own id resolution.
