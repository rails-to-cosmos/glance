# Proposal — one pin, one write, one refusal

**Status:** proposed · **Date:** 2026-08-26 · **Origin:** /generalizer over
the stage-1 diff ("bytes leave the heap") — the cross-cut, abstraction-shape
and variant-cost lenses all landed on the same seam.

## Pattern

The fact "a path, the bytes read from it, and the SHA of those bytes" is
spelled as a `(Text, Text)` tuple in the read path (`currentDocument`,
`Edit.readDocument`, `documentsFor`) and as a record in four other places
(`Snapshot`, `EditReceipt`, `ParsedDocument`, `ConfigLayerFile`) — with the
field order FLIPPING between the tuple (text, digest) and `ConfigLayerFile`
(digest, text); both halves are `Text`, so a transposition compiles. The
pin rule "these bytes digest to the row's pin" is compared at five sites in
three modules, rendered by six sentences (`reparsed`, `rewritten`,
`configMoved`, `captureMoved`, `driftWhy`, `staleWhy`) in three answer
shapes (the 409 `conflict`, a per-row `{ok:false}` inside a 200, the
planning 409 hand-built past `conflict`), with the 409 reason as bare
strings at four sites where `AGENTS.hs` already spells the sum
`Refused409 = R409Stale | R409Drift | R409Planning` — mirrored by no test.
A write travels as three positionals (path, digest, edits) assembled at
five call sites; the model's `data Write = Write Path Digest Edits` ("a
write cannot be BUILT without its pin") exists in code only as `FilePlan`,
local to one door.

The stage-1 cleanup collapses the two hand-rolled read-side pin checks
onto `Edit.currentText` (`pinnedDocument`); this proposal is the rest of
the family.

## Files

- `src/Data/Org/Edit.hs` — `Snapshot`, `EditReceipt`, `ParsedDocument`,
  `readDocument` (`Maybe (Text, Text)`, folding EACCES and non-UTF-8 into
  the create pin), `currentText`, `Edit`.
- `src-query/Glance/Query.hs` — `currentDocument`, `pinnedDocument`,
  `replaceSpans`, `WriteFailure`, the `[(Span, Text)]` family (~25
  signatures), `Repeat.rpEdits`.
- `src-web/Glance/Web/Base.hs` — `conflict`, `answerWrite`, the six
  sentences.
- `src-web/Glance/Web/Routes.hs` — `prepare` (returns `(digest, org)`, the
  other pair under the same tuple type), `commit`, the planning 409.
- `src-web/Glance/Web/Commands.hs` — `documentsFor`, `planCommand`,
  `writeOne`, `driftWhy`/`staleWhy`, `FilePlan`, `RowWrite.rwEdits`.
- `src-web/Glance/Web/Watch.hs` — `writeSpans path digest edits`.
- `src/Data/Org/Config.hs` — `ConfigLayerFile` and its tuple destructuring.
- `AGENTS.hs` — `Refused409`, `WSite`, `Write`.

## Proposed change

```haskell
-- Data.Org.Edit: the one shape of bytes-with-their-pin
data Document = Document { docPath :: !FilePath, docText :: !Text, docDigest :: !Text }
data Read     = Absent | Unreadable Text | Undecodable | Present Document
readDocument  :: FilePath -> IO Read
snapshotOf    :: Document -> Snapshot
data EditReceipt    = EditReceipt Document
data ParsedDocument = ParsedDocument { pdDoc :: !Document, pdElements, pdContext }
data Write = Write { wrAt :: !Snapshot, wrEdits :: ![Edit] }   -- the model's own

-- Glance.Query: the one comparison, carrying its fact
data Drift = Drift { driftPath :: FilePath, driftPinned :: Text, driftFound :: Text }
data Unpinned = Drifted Drift | Stale Text
pinned  :: HeadlineRecord -> Document -> Either Unpinned Text   -- disk vs the store's pin
holds   :: HeadlineRecord -> Text -> Either Unpinned ()         -- the client's pin vs the store's
data WriteFailure = WriteDrift Drift | WriteRefused Text
replaceSpans :: Write -> IO (Either WriteFailure Text)

-- Glance.Web.Base: one 409, one sentence table
data Conflict  = ConflictStale Text | ConflictDrift Text | ConflictPlanning Text Text
conflict       :: Conflict -> Response                         -- "field" rides Planning alone
data WriteSite = Inbox FilePath | Blob FilePath | Batch FilePath | Commit | Layer
movedAt        :: WriteSite -> Unpinned -> Text                 -- the six sentences, one table
answerWrite    :: WriteSite -> (Text -> [Pair]) -> Either WriteFailure Text -> Response
```

`ConfigLayerFile` becomes `{ lfDoc :: Document, lfTag }`; `prepare` returns a
`Write`; `writeSpans` takes a `Write`; `FilePlan` holds one. `TestSpec`
compares `Conflict`'s reason words against `Spec.reason409` (today
uncompared). Every route that wants bytes reads through `pinned`.

## LOC estimate

+~45 (`Document`, `Read`, `Write`, `Drift`/`Unpinned`, `Conflict`,
`WriteSite`, `movedAt`) / −~40 (five comparisons → two, six sentences →
one table, four `[(Span, Text)]`↔`Edit` conversions, tuple projections,
the two empty-pin literals). Net ≈ 0 now. Per future write door: one
`WriteSite` constructor and one sentence row, the compiler naming the
door whose sentence is missing; per future read door: one line through
`pinned`. Today: ≥4 registrations across 3 files, reason word decided per
door.

## Risk

Wire-visible only where a read failure today answers "drift" with an empty
digest (it would answer a refusal); the 409 bodies keep their shape. A
rename-scale edit across the `[(Span, Text)]` family — mechanical, and the
compiler drives it. `TestServe`'s eight reason-word pins stand.

## Existing precedent

`FilePlan` + `Snapshot` are the two halves of `Write`; `BlobSeed` and
`Asked` are records minted from same-typed positionals for the swap hazard;
`plannedValue` — "BOTH WRITE DOORS ASK ONE FUNCTION"; `addressed` shared by
materialize and commit "so a commit cannot address what a materialize
would refuse"; `CloseReason`/`closeReason` for a closed sum of reasons.

Inert until reviewed.
