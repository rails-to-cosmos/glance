# Proposal — a record is cells and file facts, and residency is typed

**Status:** proposed · **Date:** 2026-08-26 · **Origin:** /generalizer
(variant-cost and abstraction-shape lenses) over the stage-1 diff, which
added two precomputed cells and measured what each costs.

## Pattern

Adding a precomputed cell to `HeadlineRecord` costs four hand
registrations in one file — the field, its initializer in `recordOf`, the
`forceRecord` list, its reader — plus `shapeOf` in the tests; and the two
that implement the residency law are the unenforced ones: a field left
out of `forceRecord` compiles and keeps a thunk over the document; a
slice not passed through `detach` compiles and retains the file. `!` buys
WHNF alone — for `Maybe Text` that is the `Just`. The model spells
`Txt 'Sliced` / `Txt 'Copied` as a phantom (`AGENTS.hs` `Kept`); the code's
`Text` cannot tell the two apart. Forcing is already split: cells in
`forceRecord`, `hrKeywords`/`hrDeclared` by `forcedKeywords` at the
callers. Separately, five fields are "one value shared per file" by their
own docstrings (`hrFile`, `hrDigest`, `hrKeywords`, `hrDeclared`,
`hrCategory`); `recordOf` takes them as five of ten positionals with three
adjacent `Text`s, `subtreeEntries` copies four off the parent to pass them
back in, `draftRecord` passes placeholders, and the store reads them back
off the first row (`listToMaybe . feRecords`, three sites). At load,
`recordOf` slices four times from the FILE start per row (`sliceSpan` is
O(offset) on text-2) where the spans sit inside `hrSubtree`. The stage-1
cleanup drops `hrHeadline`, stops copying fresh Texts and slices from the
subtree; this proposal is what makes the next cell one line and the law
a type.

## Files

- `src-query/Glance/Query.hs` — `HeadlineRecord`, `recordOf`,
  `forceRecord`, `detach`, `subtreeEntries`, `draftRecord`, `viewColumns`.
- `src-web/Glance/Web/Store.hs` — `FileEntry`, the three first-row reads.
- `test/TestQuery.hs` — `shapeOf`, `residencySpec`.
- `AGENTS.hs` — `Kept`, `Txt`, `FileEntry` model.

## Proposed change

```haskell
-- Glance.Query: a copy is a type, and only a copy can be kept
newtype Kept = Kept Text          -- constructed by `keep' alone: T.copy + force
keep    :: Text -> Kept           -- the one door; a slice cannot be stored
fresh   :: Text -> Kept           -- for Texts the parser built (T.pack, showt, toLower): force, no copy
-- every retained Text field of HeadlineRecord is a Kept; forceRecord derives
-- (Generic + a one-line NFData-shaped fold over Kept), so a field cannot be missed.

-- per-file facts, one value
data FileFacts = FileFacts
  { ffPath :: !FilePath, ffDigest :: !Kept, ffDeclared, ffKeywords :: !TodoKeywords, ffCategory :: !Kept }
recordOf    :: ConfigLayers -> FileFacts -> Text -> Int -> Headline -> Span -> HeadlineRecord
fileFactsOf :: HeadlineRecord -> FileFacts        -- subtreeEntries/draftRecord pass this
-- Store.FileEntry holds the FileFacts; the three `listToMaybe . feRecords' reads go.
```

A cell then costs its field and its initializer; `forceRecord` and the
copy discipline are derived, `shapeOf` reads the record generically, and
the residency case in `TestQuery` becomes a property over the type: no
field of `HeadlineRecord` is a bare `Text`.

## LOC estimate

+~30 (`Kept`, `keep`/`fresh`, `FileFacts`, the derived force) / −~35
(hand `forceRecord` list, the per-field `detach` calls, five positionals
and their re-passing, three first-row reads). Per future cell: 2 lines,
enforced; today 4 + a test list, two of them silent when missed.

## Risk

Type churn on every cell reader (`Kept` unwrap); no behaviour or wire
change. `Generic`-derived forcing needs one instance; no `deepseq` in the
dependencies today (add it or hand-write the fold once).

## Existing precedent

`BlobSeed`, `DraftCargo`, `Asked` — records minted from same-typed
positionals; `Ref` with `detachRef` — a copy discipline attached to a
type; `forcedKeywords` — forcing owned by the value's own module; AGENTS's
`Txt 'Copied` phantom, which this makes real.

Inert until reviewed.
