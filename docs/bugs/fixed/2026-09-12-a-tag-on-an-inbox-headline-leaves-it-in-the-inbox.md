# Bug — a tag on an inbox headline leaves it in the inbox

**Status:** fixed · 2026-09-13 · **Reported:** 2026-09-12 (live use: a jot
captured to `inbox.org`, tagged from the table, still served from the inbox) ·
**Surface:** `add-tag`, the tags cell

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
2. `add-tag {id, tag: "book"}` (or `:` then `+` on the row).
3. `list-headlines tag:book` serves the row with a path id
   (`…/inbox.org#N`); `inbox.org` still holds it; no blob under
   `.org-glance/data/…` exists; `EXTERNAL.jsonl` gained no line.

## Evidence

- `add-tag` was `Splices ReadsNothing` (`src-web/Glance/Web/Commands.hs:156`)
  over `addTagEdits` (`src-query/Glance/Query.hs:1696`): edits to the
  title-line tag run only. `renameTagEdits` (`:1717`) likewise.
- The tagged road existed at capture time only: `captureInto`
  (`Commands.hs:369`) picks `captureBlob` (`:392`), which mints the id and
  creation stamp through `blobDocument` (`Query.hs:2269`) and notes the
  ledger (`External.hs:90`). `captureInbox` (`:374`) appends raw.
- No verb moved a subtree between files. `delete` is the one `Moves` kind
  (`Commands.hs:163`, `deleteRows :294`, `Data.Org.Trash.trashBlob :40`) and
  moves a whole blob file. `planCommand` groups edits by file
  (`Commands.hs:510`); `writeSpans` writes one path (`Watch.hs:89`);
  invariants.md:125 says a batch is per file with no rollback across files.
- `tagCommandSpec` (`test/TestServe.hs:9867`) pinned add/remove in place;
  nothing pinned a move.

## Fix

**The sprout is a rule of the inbox WRITE, decided after a row's edits are
known** — no verb carries a flag for it. `planCommand` (`Commands.hs:593`) asks
`sprouted` (`:360`) of every row whose file group has passed the client's pin: a
**top-level** row of `captureTargetIn` carrying no `ORG_GLANCE_ID`, whose
**edited title line** wears a tag run, leaves. `add-tag`, a `set-title` spelling
`:tag:` and a `rename-tag` all inherit it; a `remove-tag` taking the last tag off
leaves no run, so it stays.

- `editedEntry` (`src-query/Glance/Query.hs:987`) applies the row's own edits to
  its subtree and **reads the run back off the composed line** — the only reading
  that sees a run `set-title` spelled inside a title.
- `mintedBlob` (`Commands.hs:390`) mints the id, folds in the store-exists wall
  and composes the document through `blobDocument`; `writeBlob` (`:406`) owns
  "the create pin is the empty digest". **`captureBlob` and the move call both**,
  so the tag is joined by `addTagEditsIn` (`add-tag`'s own rule, which folds a
  tag the run already wears) and a creation stamp the entry lacks comes off the
  request's ONE clock read (`askNow`, `Base.now`/`Base.dayAt`).
- A sprouting row's plan entry IS the cut — `(hrSubtree, "")` — so it rides the
  inbox's own per-file plan beside whatever the rows staying there edit.
  **Several inbox rows named at once are still one cut.** The `Sprout` rides
  `fpRows` (`:92`), so `writeOne` (`:561`) needs no side map: it lands each blob
  first and drops the cut of one that did not, so that row stands where it was.
- `add-tag` is `ReadsNothing` again, so an ordinary tag write opens **no file**.
  The inbox is read ONCE per request and only where a candidate stands
  (`pinnedDocument` for that path alone, reusing `documentsFor`'s read when the
  command took one).
- `stalePin` (`:632`) is `planCommand`'s own wall and nothing else's: a group it
  refuses computes no edits, mints no uuid and writes no byte.
- `stampedEntry` (`src-query/Glance/Query.hs:2293`) leaves an
  `ORG_GLANCE_CREATION_TIME` the drawer already spells alone — a jot keeps the
  stamp it was captured with — and **cuts an `ORG_GLANCE_ID` the drawer claims**,
  writing the minted one in its place: the blob's path, the answer and the row's
  identity name one id.
- The answer: `moved` (`:643`) names the id the row **arrives** under, with
  `from` naming the id the request spelled and `file` the blob; `orphaned`
  (`:649`) is the blob-landed-inbox-refused case, `ok: false` naming both. Both
  build on `okPairs` (`:637`), which `captured` spells too. `fire`
  (`frontend/glue/20-sheet.js`) reads `from` and sets `arriving`, so point
  follows the row, and its error line names `from || id` as `noted` and `unmark`
  do. `40-popups.js` counts **ok rows, never ids** — a moved row answers under
  two.

What does not move: a row already carrying an id, a tree with no `.org-glance` to
mint into, an edit leaving no run, and every row outside the inbox — each keeps
the in-place edit. A child headline is no row of its own, so nothing names one:
it rides the subtree its parent takes.

### Tests

- `test/TestServe.hs:9823` and the ten cases under it — the move, a `set-title`
  run moving one, a second tag in place, a tag on a blob row in place,
  `remove-tag` of the last tag in place, a child riding its parent, a row with an
  id in place, a tree with no store in place, a stale pin refusing with no blob minted,
  the orphaned answer (skipped where the OS allows the write), and two inbox jots
  in one cut. `withInboxJot` is the fixture.
- `test/TestQuery.hs` `blobCases` — the id a template claims is rewritten, the
  creation stamp it claims is kept.
- `test/browser/cases.mjs:4740` — `a tag on an inbox jot moves it, and point
  follows`.
- `AGENTS.hs` — the sprout note and the moved-row note beside it.
