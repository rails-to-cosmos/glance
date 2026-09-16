# Bug — a tag on an inbox headline leaves it in the inbox

**Status:** fixed 2026-09-16 · **Reported:** 2026-09-12 (live use: a jot captured to
`inbox.org`, tagged from the table, still served from the inbox) ·
**Surface:** `add-tag`, the tags cell, `set-title` with a `:tag:` run

## Symptom

An untagged capture appends to `inbox.org` with no `ORG_GLANCE_ID`
(`docs/capture.md:25`). Giving it a tag later edits the title line and
nothing else: the headline stays in `inbox.org`, mints no id, writes no
ledger line, and org-glance never learns of it. Expected: the first tag
names the destination, the way a tagged capture does — the subtree leaves
the inbox and lands as a blob under that tag's layer, id minted, ledger
noted.

## Steps to reproduce

1. `+`, a title, `RET` under a filter with no `tag:` — the row lands in
   `inbox.org`.
2. `add-tag {id, tag: "book"}` (or `t` on the row).
3. `list-headlines tag:book` serves the row with a path id
   (`…/inbox.org#N`); `inbox.org` still holds it; no blob under
   `.org-glance/data/…` exists; `EXTERNAL.jsonl` gained no line.

## Evidence

- `add-tag` is `Splices ReadsNothing` (`src-web/Glance/Web/Commands.hs:156`)
  over `addTagEdits` (`src-query/Glance/Query.hs:1696`): edits to the
  title-line tag run only. `renameTagEdits` (`:1717`) likewise.
- The tagged road exists at capture time only: `captureInto`
  (`Commands.hs:369`) picks `captureBlob` (`:392`), which mints the id and
  creation stamp through `blobDocument` (`Query.hs:2269`) and notes the
  ledger (`External.hs:90`). `captureInbox` (`:374`) appends raw.
- No verb moves a subtree between files. `delete` is the one `Moves` kind
  (`Commands.hs:163`, `deleteRows :294`, `Data.Org.Trash.trashBlob :40`) and
  moves a whole blob file. `planCommand` groups edits by file
  (`Commands.hs:510`); `writeSpans` writes one path (`Watch.hs:89`);
  invariants.md:125 says a batch is per file with no rollback across files.
- `tagCommandSpec` (`test/TestServe.hs:9867`) pins add/remove in place;
  nothing pins a move.

## Proposed fix

`add-tag` (and any tag edit) on a row whose file is the inbox and whose tag
run was empty becomes a two-file write: cut the subtree from `inbox.org`
(a splice to empty over its span, digest-locked), mint the blob under the
first tag with `blobDocument` (id, creation, the tags), note the ledger,
nudge both paths. Order: blob written first, then the inbox cut, so a
failure between leaves a duplicate rather than a loss; the answer names
the new id so point follows. A row already carrying an id or living
outside the inbox keeps today's in-place edit. Failing test first:
`tagCommandSpec` gains "a tag on an inbox jot moves it under the tag's
layer" asserting the blob's bytes, the inbox's bytes, the ledger line and
the answered id; a browser case asserts point follows the moved row.

Open: the second tag on an already-moved row stays an in-place edit (first
tag wins); `remove-tag` of the last tag does not move it back.

## Resolution

The command planner now recognizes a top-level, id-less inbox row whose edited
headline gains a tag run. It writes the edited subtree as a stamped blob first,
then cuts the source subtree under the inbox digest lock. The response names
both the source and destination IDs so the UI follows the moved row. Rows with
an existing ID and rows outside the inbox continue to edit in place.
