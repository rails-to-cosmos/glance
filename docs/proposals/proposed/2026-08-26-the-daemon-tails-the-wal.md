# Proposal — the daemon tails org-glance's WAL

**Status:** proposed · **Date:** 2026-08-26 · **Origin:** user — *"it should
sync with the main WAL and understand metadata at least"* — over
`docs/bugs/open/2026-08-26-a-headline-org-glance-creates-is-invisible-until-restart.md`.

## The law in one line

What org-glance appends to `meta/headlines.jsonl`, the daemon reads as a
nudge: each record's id names a blob (`data/<2>/<rest>/data.org`), and that
path goes through the queue every daemon write already goes through.

## Why the WAL and not the directory

- It is the peer's DECLARED contract — append-only, `seq`-monotone, torn
  lines forgiven (`Data.Org.Index` already holds the read law) — and the
  mirror of `EXTERNAL.jsonl`, which the two programs already agreed is how
  one tells the other about a write. The README's "each notifies the other"
  becomes true in both directions.
- The file lives in a directory that exists for the store's whole life, so
  inotify fires on it reliably; the fresh-shard hole (`AGENTS.hs:2074`) is
  sidestepped rather than patched. Re-arming directories from glance's side
  would fight fsnotify's measured behaviour and still race a directory
  populated before its watch lands.
- org-glance writes the blob BEFORE the WAL record
  (`org-glance-graph:add`: metadata, `put-content`, then `insert`), so a
  nudged path exists by the time it is read — no retry, no provisional row.
- Edits ride too: every Emacs save of that headline appends a revision, so
  the "every later edit lost" half of the gap closes with the same lines.

## Mechanism

- `Watch.hs`: beside the tree's `watchTree`, one `watchDir` on
  `<root>/.org-glance/meta` (when it exists at boot; a reseed re-checks),
  filtered to the open segment and `MANIFEST`.
- A cursor: the open segment's byte offset, seeded at boot to the file's
  current size — the walk has already seen every blob, so nothing is
  replayed. On a Modified event, read from the cursor, keep only complete
  lines (a torn tail waits for the next event, `Index.hs`'s own policy),
  advance the cursor. A size below the cursor is a seal
  (`rename` + `f-touch`): the cursor resets to 0.
- Per line: decode `id` (a `tombstone` line too — the reload finds the
  blob gone and drops the row, the delete door); derive the path
  (`Data.Org.Blob` already spells the shard layout for captures); `nudge`
  it. `watched` accepts a blob path, so the existing `reload` →
  `applyFile` → `publish` does the rest, and the row lands over the socket.
- "Understand metadata": the blob carries it all and the parse is the
  truth; the record's `state`/`tags`/`archived` are not consulted for the
  row. (They remain the drift instrument's business, `glance scan`.)

## Oracle

- `test/interop/drive.mjs`: the case `the-skips-that-pass` §6 asks for —
  org-glance captures into a store the daemon is already watching; poll
  `/headlines` for a bounded window; the row lands; then an Emacs-side
  edit of it lands too. The most valuable interop case the suite lacks.
- TestServe, no Emacs: over a real store, append a record to
  `headlines.jsonl` after writing its blob; the row appears; a tombstone
  line drops it; a torn line waits; a seal resets the cursor.
- `AGENTS.hs:2074`'s `[Unguarded]` note retires into a `[Interop, Test]`
  law; CHANGELOG names the closed gap.

## Why not index FROM the WAL

Asked, and answered with the reporter's tree (2026-08-26): 6106 of its 6107
org files are store blobs and the WAL holds 6092 live records — coverage is
near-total, and the question is fair. Still no, on three counts:

- **Truth.** Org files are the source of truth here (README, "Under it");
  the WAL is org-glance's projection and it drifts — `glance scan` reports
  28 rows disagreeing today (27 states, 1 archived) and 14 blobs with no
  record at all: glance's own captures that Emacs never folded
  (`EXTERNAL.jsonl`'s adopt hole, CLAIM 17). An index read off the WAL would
  serve those wrong and hide these.
- **Bytes.** A record carries no body, no properties, no children (7539
  headlines live in those 6106 blobs), no spans — and every write is a
  byte-exact span replace, every material doc a parse. The parse is owed
  regardless.
- **Cost.** The walk is 0.71 s for 6107 files (3789 files/s). A warm-start
  index off the WAL — its `hash` field would allow it — buys nothing worth a
  second truth.

So the WAL is the NOTIFICATION channel for org-glance's writes, and stays a
drift instrument beside the files; fsnotify stays for what the WAL never
sees — plain files outside the store, config layers, any other writer.

## What consistency needs, measured (2026-08-26, the reporter's tree)

The WAL and the files disagree in 48 places; crossed with `EXTERNAL.jsonl`
(glance's ledger) and `EXTERNAL.cursor` (org-glance's fold position, at the
ledger's end — every line read):

- **21 blobs with no record — all 21 glance captures**, skipped by
  `refresh-external` as "unknown or deleted" (`org-glance-graph.el`, the
  fold's unknown-id branch). The adopt hole, CLAIM 17.
- **2 state drifts glance triggered**: `set-state` wrote `PENDING` /
  `CANCELLED` (keywords the tree's `system.org` declares); org-glance folded
  the write and recorded `state ""` (`seq` 6903/6921) — its parser reads the
  blob without the tree's `#+TODO` vocabulary, so a custom keyword is title
  text to it.
- **25 state drifts org-glance's own** (`Game-2021…`, `task-spbm-…` legacy
  rows, `wal=TODO blob=none`), never touched by glance.

So consistency is the FOLD's business, whichever process appends: adopt
unknown ids, read keywords from the tree's config layers, run promptly.
Those are org-glance-side fixes; with them the ledger-then-fold is a
consistent single-writer WAL, and this proposal's tail completes the loop.

A two-writer WAL (glance appending its own records) was weighed: glance can
spell 16 of the 17 fields from its parse, but `hash` is org-glance's private
buffer normalization (`org-glance-headline--buffer-hash`), and matching it
byte-for-byte from Haskell is a second implementation of a private law. It
also needs org-glance to re-read the tail before minting `seq`. Deferred
unless the hash becomes a shared content hash.

## Out of scope, named

Reconciling `wal=` vs `blob=` drifts (28 on the reporter's tree today) —
the scan reports them; this proposal only makes the daemon hear the writes.

Inert until reviewed.
