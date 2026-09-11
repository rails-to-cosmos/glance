# Bug — a broken property drawer gives a row a path id that the other routes refuse

**Status:** fixed · **Reported:** 2026-09-10 (live use over `glance mcp`,
`~/sync/views/.org-glance/data/fe/6180a2-ac42-4243-a8ca-eb8e741b7899/data.org`)
· **Fixed:** 2026-09-11 · **Surface:** `rowIdIn` fallback, `get-headline`, `from:`
· **Fixed in:** `src/Data/Org/Types.hs`, `src-query/Glance/Query.hs`,
`src/Data/Org/Doctor.hs`, `src/Data/Org/Index.hs`, `src/Data/Org/External.hs`

## Fix (2026-09-11)

The id is read off the raw lines where the parse refused the drawer, and the
scan's report names the files.

- `src/Data/Org/Types.hs:345` — `identityOf h TEXT` is `identity` then the
  salvage, THE ONE READER both call sites use, so neither can disagree by
  omission.
- `src/Data/Org/Types.hs:350` — `salvagedIdentity h TEXT` reads
  `:ORG_GLANCE_ID:` out of the drawer-shaped run in `TEXT`, which starts at
  `H`'s own line. It answers `Nothing` where the parse read a drawer at all, so
  a drawer it COULD read is taken at its word, id or none, and a blob that truly
  carries no id keeps the path form. The drawer must open within two lines of
  the headline, `:PROPERTIES:` is matched on its first twelve characters, and
  only `:END` and `:END:` close — `:ENDORSED:` is a property line.
- `src-query/Glance/Query.hs:475` — `recordWith` takes it:
  `orgId = copy <$> identityOf h subtree`, so `hrId` and `hrOrgId` both spell
  the uuid. The text is the SUBTREE, never the file, so no row pays for its
  offset.
- `src/Data/Org/Doctor.hs:222` — the corpus scan claims the same ids, computing
  each headline's effective id ONCE on a `Cursor` walk (`tailWith`,
  `Doctor.hs:277`); `indexTerms` carries it and the salvage mark to
  `blobEntryOf`, which keeps its two arguments.
- `src/Data/Org/Index.hs:56` — `BlobEntry` gains `beSalvaged`; `IndexDrift`
  gains `dfIdlessPaths` and `dfBrokenPaths`, whole and path-ordered.
- `src/Data/Org/Index.hs:212` — the report lists the idless blobs under their
  count and adds a `drawers` line listing the broken-drawer ones, each capped at
  `driftSamples`. A path list prints only when it has paths, and the `drawers`
  count only when there are any.
- `src/Data/Org/External.hs:85` — `blobIdOf` keeps bare `identity`: org-glance's
  elisp cannot resolve a salvaged id, so no ledger line is written for one.

No wire field was added: `doctor` parses the tree itself, so the mark rides
`BlobEntry`, which never reaches a row or the JSON.

Tests: `test/TestSpec.hs` "a broken drawer's id still names the row" (the uuid
names the row, and a second entry with no drawer keeps `FILE#1`),
`test/TestServe.hs` "tools/call get-headline answers a broken drawer's own id",
`test/TestIndex.hs` "a broken drawer's own id is the entry, marked as salvaged"
(the real scan end to end), "a drawer the parse read is taken at its word",
"the salvage mark comes off that headline too", "the idless and the
broken-drawer blobs are named by path", "the named paths are capped where the
samples are" and "the idless and broken blobs are listed under their counts".

## Symptom

The blob's drawer read `:END` (no closing colon) on line 6 — the working
tree had drifted from `HEAD`, which has `:END:`; how is unknown, the file
was edited by an agent with Python and `sed` that never touched that line,
and by Emacs. With the drawer unreadable:

- `list-headlines` served the row under the id
  `/home/…/data/fe/6180a2-…/data.org#0`, title intact, tags intact.
- `get-headline {id: "fe6180a2-ac42-4243-a8ca-eb8e741b7899"}` answered
  `{"error":"no headline with id …"}` — the uuid the blob's own
  `ORG_GLANCE_ID` line still spells.
- `from:fe6180a2-…` served 0 rows; `from:/home/…/data.org#0` served the 8
  rows the subtree links to.
- `doctor` counted it among *"33 rows disagree with the org-glance index"*
  and *"17 carrying no id"*, one line each, with no path.

Restoring the colon fixed all four at the next re-read.

## Why it matters

The path id looks like an id — it is stable, it resolves in `from:` and in
`ref:` answers — so a client (an agent, a `[[glance:…]]` link an agent
writes) can carry it around and only later find that `get-headline`,
`edit-link` and the write tools refuse it. One malformed line silently
splits a row's identity in two. Seventeen such rows exist in the live store
today (`doctor`: *17 carrying no id*), and the drift line names none of
them.

## Mechanism

- `hrId = fromMaybe (rowIdIn path ordinal) orgId`
  (`src-query/Glance/Query.hs:458`); `rowIdIn` `:1268` spells
  `<path>#<ordinal>`.
- The drawer parse stops at the first non-`:KEY:` line, so an `:END` without
  its colon ends the drawer as a paragraph, and `ORG_GLANCE_ID` is never a
  property. The id line is still bytes in the file — a `grep` finds it.
- `doctor` reports the count only (`indexReportLines`, `Data.Org.Index`).

## Steps to reproduce

1. In any blob with a drawer, strip the colon: `sed -i 's/^:END:$/:END/'`.
2. `list-headlines tag:<its tag>` — the row is there, id is a path.
3. `get-headline` with the blob's uuid — *no headline*.
4. Restore the colon — the uuid answers again.

## Proposed fix

- The walk, on a blob whose FIRST headline yields no `ORG_GLANCE_ID`, reads
  the raw text for `^:ORG_GLANCE_ID:\s*(\S+)` inside the first drawer-shaped
  region and uses that as the id, marking the row (`"drawer": "broken"`)
  so `doctor` can list it with its path. The path form stays for blobs that
  truly carry no id.
- `doctor`'s *carrying no id* line grows the paths (it already samples ten
  for drift: `driftSamples`, `Index.hs:39`).
- A test: a fixture under `test/` with `:END` and an id line; the row id is
  the uuid, `get-headline` answers, `doctor` names the file.

Reproduce with a FAILING test before the fix, per `CLAUDE.md`.

All three landed as proposed. The scan reads the salvaged ids too, so its
id-collision count and the daemon's stay one set.
