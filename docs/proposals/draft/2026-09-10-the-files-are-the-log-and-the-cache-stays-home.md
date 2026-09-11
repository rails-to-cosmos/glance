# Proposal — the files are the log, and the cache stays home

**Status:** draft — a direction, argued against the industry's answers, with
a cheaper middle stop named · **Date:** 2026-09-10 · **Origin:** user, after
`2026-09-10-two-writers-one-wal.md` — *"what do you think of the design
overall? Can it be better? What does the industry do — RocksDB, the best
people?"*

## What the design gets right

- **Files are the truth, everything else is derived.** org-roam, Obsidian
  and Logseq all land here: a tree of text files and a local cache keyed by
  the file's hash.
- **Temp-then-rename, `MANIFEST` as the commit point, sealed segments,
  compaction.** LevelDB/RocksDB, one to one: their `MANIFEST` is also a log
  of version edits, SSTs are immutable, compaction rewrites.
- **`EXTERNAL.jsonl` as an outbox.** The transactional-outbox / CDC pattern;
  standard.
- **Positional last-wins and union merge.** Pragmatic for a git-synced
  store.

## Where it argues with itself

1. **It is not a WAL.** RocksDB's WAL is written BEFORE the data, replayed
   after a crash, and dropped once the memtable flushes. `headlines.jsonl`
   is written AFTER the blob, never dropped, and IS the index. That is a
   log-structured index, a cache with a journal's shape. The name sets the
   wrong expectation — the first answer in the session that produced this
   file read it as the index of record and was wrong. Call it `journal` or
   `index-log`.
2. **Position decides order, and union merge decides position.** Union
   concatenates; which side lands last is git's choice, not causality
   (invariant 1's own violation note: *union-merged stores resolve the wrong
   record*). The industry answers with one writer per log (a Kafka partition
   per producer, Datomic's transactor), a logical clock (Lamport; HLC in
   CockroachDB), or a CRDT. This store has a fourth, cheaper answer: **a
   record is right iff its `hash` equals the blob's.** Then two records for
   one id are never resolved, they are recomputed from the blob. The index
   is a materialized view; `seq` and position stop carrying meaning.
3. **Derived facts in a synced log.** `links`, `relations`, `range`, `hash`
   are org-mode's reading (`org-glance-headline.el:222-238`); the daemon
   reads its own (`Query.hs`). Two parsers of one contract, with a drift
   instrument between them. Nobody syncs derivatives: the raw is what
   travels; each machine's cache of derivatives is its own and ignored by
   git.
4. **Git is already the journal.** Content-addressed, append-only, merged,
   with history and with tombstones (a deleted file). The JSONL log
   duplicates it. What the log adds over git is ONE thing: Emacs opens
   without walking 6000 files, because Elisp parsing is slow where the
   daemon's walk is 0.71 s (`2026-08-26-the-daemon-tails-the-wal.md`). So
   it is a cache for a slow client — and a cache is not synced.
5. **Multi-process.** RocksDB is explicit: one process owns the DB; others
   are read-only secondaries that catch up; multi-process writers mean a
   server. SQLite: one writer, the rest wait on the lock. The hand-rolled
   append protocol — per-line opens, rotation by rename, a cursor with two
   digests (invariant 34), `--heal`, `--reconcile-manifest` — re-implements
   what SQLite in WAL mode gives for free, in 1806 lines of
   `org-glance-graph.el` (123 definitions).
6. **Small things.** No CRC per record; a torn line is caught by JSON
   failing to parse (RocksDB: CRC32C per record). Notes are not fsynced —
   fine for a cache, worth saying so.

## The direction

Three moves, each one the industry's default.

### 1. One cache per machine, any writer; the lock is SQLite's

Not *the daemon is the writer* — the user's objection (2026-09-10) stands:
*"a single writer that is the daemon complicates org-glance for the Emacs
ecosystem; distributing it would then imply running a server."* org-glance
must install from MELPA and work alone. So:

- **Standalone Emacs** writes the blob, then its row in the cache. SQLite
  serializes writers; `sqlite` is built into Emacs ≥ 29 (the user's 30.2
  answers `sqlite-available-p` → `t`), so the package gains no dependency.
  The first fill walks the tree in Elisp — slow once, the way org-roam's
  first `org-roam-db-sync` is — and is incremental by hash on save and on
  idle after that.
- **The daemon is an optional accelerator**, org-roam-ui to org-roam: when
  present it fills the cache in 0.7 s instead of minutes, writes the same
  rows on its own edits, and serves HTTP, MCP and the web view. Absent,
  nothing breaks.
- **Two parsers, one cache, no drift instrument.** A row carries `hash` and
  `producer`. A writer replaces a row only when the blob's hash changed. At
  equal hash both parsers OWE the same facts; a difference is a bug, caught
  by a parity test over fixtures, never resolved at runtime. This is what
  `two-writers-one-wal.md`'s oracle (3) becomes.

The *one noter per host* of `two-writers-one-wal.md` survives as the
daemon's fsnotify path when it runs; when it does not, Emacs's own save hook
is the noter, and there is nothing to relay either way.

### 2. The cache is SQLite in WAL mode, git-ignored

```
headline(id PRIMARY KEY, path, hash, title, state, priority, tags, scheduled, deadline, closed, created)
edge(src, dst, kind, at)          -- at: 'title' | 'body' | 'logbook'
INDEX edge_dst ON edge(dst)       -- backlinks
INDEX headline_path ON headline(path)
```

- Invalidation key: the blob's hash. A row whose `hash` differs from the
  file is re-parsed; nothing else is ever "resolved".
- Backlinks are an index read; `ref:`/`from:`/`neighbors`
  (`the-graph-an-agent-can-walk.md`) are one query each, and the graph is
  free.
- Either parser feeds it (move 1); a row says which (`producer`). Emacs
  reads the rows the daemon wrote and the daemon reads Emacs's, and neither
  re-parses a blob whose hash it already holds.
- Multi-process safety is SQLite's: any number of readers, one writer at a
  time, the lock is the database's. Emacs and the daemon are both writers
  and neither knows about the other. Invariants 7 and 34 retire; so does
  invariant 8's resolver, because nothing under `meta/` is in git any more.

### 3. Only blobs travel

`meta/*.jsonl` leaves git. A tombstone is a deleted file; "deleted" versus
"not synced yet" is git's question, answered the way it already is for the
blobs themselves. Cross-host conflicts on one blob are git conflicts on
`data.org` and the per-file digest CAS
(`2026-08-18-one-writer-per-file-and-a-version-the-client-can-check.md`)
is the local answer; the cache never has an opinion.

## What it costs

- **org-glance swaps its store.** `org-glance-graph.el` (1806 lines: WAL,
  segments, seal, heal, manifest reconcile, the outbox fold) becomes a
  SQLite adapter of a few hundred lines: `upsert` by id, `delete`, the
  queries the overviews run, a walk for the first fill. The facts
  computation (`--buffer-content-facts`) STAYS — it is the standalone
  parser — and gains the parity fixture against the daemon's.
- **The daemon gains a cache.** One table pair, one hash check in
  `applyFile`, one migration from the walk. `Glance.Web.Store` keeps its
  in-memory `Store`; SQLite is what survives a restart and what Emacs may
  read.
- **A migration.** One run of the walk fills the cache; existing
  `meta/` files are left in place until every host has moved, then removed
  from git with `git rm --cached`.
- **What is lost.** The append-only history of index records. It was never
  used for anything but last-wins; git holds the history that matters.

## The middle stop, if the cut is too deep

`two-writers-one-wal.md` as written: `hash` on the note, `EXTERNAL.jsonl`
read as a provisional layer, pending told apart from drift, the notification
family out of git. It patches the relay rather than removing it, and every
one of its four refusals (PIPE_BUF, seal ownership, `seq`, two parsers) is
a reason the relay should not exist. Take it if the org-glance rewrite
cannot be scheduled; take this file when it can.

## What the industry would still ask

- **A CRC per record** if any log stays (RocksDB, Kafka: framing plus a
  checksum, not "did JSON parse").
- **fsync policy stated.** Blobs are fsynced (`Edit.hs:225`); the cache
  need not be, if the walk can rebuild it. Say so in `invariants.md`.
- **A version in the cache's header** (`PRAGMA user_version`) so a schema
  change is a rebuild, not a heal.

## Evidence

- `~/sync/stuff/org-glance/src/data/org-glance-graph.el`: 1806 lines, 123
  definitions; `docs/invariants.org` inv 1, 7, 8, 34
- `src/Data/Org/Index.hs`, `src/Data/Org/External.hs`,
  `src-web/Glance/Web/Store.hs`
- `docs/proposals/proposed/2026-08-26-the-daemon-tails-the-wal.md` (walk:
  0.71 s / 6107 files), `2026-08-18-one-writer-per-file-…`,
  `draft/2026-09-10-two-writers-one-wal.md`,
  `done/2026-09-10-the-graph-an-agent-can-walk.md`
- RocksDB wiki: *Write Ahead Log*, *MANIFEST*, *Secondary Instance*
  (one primary writer, read-only secondaries); SQLite *Write-Ahead Logging*
  (one writer, concurrent readers); org-roam v2 (`org-roam-db`, SQLite keyed
  by file hash)
