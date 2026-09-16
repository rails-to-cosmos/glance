# The files are the log; derived state stays local

**Status:** done — direction accepted · **Date:** 2026-09-16 · **Origin:**
repository review, choosing between the two 2026-09-10 persistence drafts

## Decision

Org documents and org-glance blobs are the durable, synchronised record. Git
carries those files and their history. Derived headline, edge and query state
belongs in one local, git-ignored SQLite cache per machine, in WAL mode and
versioned with `PRAGMA user_version`.

Both Emacs and the optional glance daemon may update that cache. SQLite owns
writer serialization. A cache row is valid only while its stored blob hash
equals the file's digest; a mismatch is reparsed rather than resolved by record
position, sequence number, or wall clock. Parser parity is tested over shared
fixtures instead of settled at runtime.

`EXTERNAL.jsonl` remains a local compatibility outbox until org-glance reads the
cache. `COMPLETIONS.jsonl` remains a local ledger. Neither notification file is
synced or assigned the union merge driver. Existing org-glance index segments
remain compatible during migration, but no new feature should deepen their
role as a cross-host source of truth.

## Why this direction

The files already provide the portable record, conflict surface, history, and
tombstones. Synchronising derived JSONL records duplicates that record and
lets git concatenation choose an order with no causal meaning. A local SQLite
cache supplies the actual missing property—fast startup for a slow parser—while
providing multi-process locking, indexed backlinks, schema versioning, and a
rebuild path.

Direct dual append to org-glance's index remains refused: long records can
interleave, seal ownership is singular, `seq` needs coordination, and the two
parsers can disagree. The relay described by the alternative draft is a
compatibility step, not the destination.

## Migration boundary

The migration is deliberately staged:

1. Keep notification files local, narrow merge attributes to the actual index
   segments, and diagnose an old store that still tracks notifications.
2. Add the versioned SQLite schema and fill it from the existing walk.
3. Let the daemon update it incrementally by file digest.
4. Add the org-glance adapter and parser-parity fixtures.
5. Stop using the JSONL index after every supported client has migrated.

Stage 1 is safe before the peer changes and is part of the integrity work that
implements this decision. Stages 2–5 require a coordinated org-glance release
and remain separate feature work.

## Supersedes

- `../draft/2026-09-10-the-files-are-the-log-and-the-cache-stays-home.md` is the
  analysis leading to this decision.
- `../draft/2026-09-10-two-writers-one-wal.md` remains useful as the migration
  fallback, but its relay is not the target architecture.
