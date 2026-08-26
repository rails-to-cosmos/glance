# Proposal — a persistent index the files still own

**Status:** proposed · **Date:** 2026-08-26 · **Origin:** user — *"what do you
think about a proper indexing here? The full fs scan doesn't scale much."*

## Where the wall is

Today the daemon rebuilds its whole projection from a full parse at every
boot, holds every document's text in memory, and answers every query by a
linear pass over every row. Measured on the reporter's tree: 6107 files,
7539 headlines, walk 0.71 s at 3789 files/s. Linear in files at every
stage, so the first wall is memory (every blob's bytes resident), the
second is boot (6107 → 60k files is 7 s, 600k is minutes), the third is
per-query time. The law that must survive: **org files are the source of
truth; the index is a projection, deletable at any moment, rebuilt from the
text** (README, "Under it").

## Three stages, each shippable alone

### 1. Bytes leave the heap — THE ASK (user, 2026-08-26: "I meant #1")

The row projection (cells, search text, links, spans, digest) is small and
stays in memory; the document text is read from disk on demand — a
materialize, an edit, a `/links` answer — by path and span. `hrDoc` becomes
a loader. Memory goes from O(bytes) to O(rows × cells). This is the change
the others build on, and it is the one a 10× tree hits first.

### 2. A parse cache keyed by what the parse depends on

Boot becomes: list the tree (readdir + stat, cheap), and for each file
compare `(mtime, size)` with the cache; on a mismatch read the bytes and
compare the SHA-256 the daemon already computes for its optimistic lock
(`hrDigest`); reparse only what changed; load the rest from the cache.
O(files) stats and O(changed) parses instead of O(files) parses.

- **Key**: the file's digest — AND the config digest, since the parse
  depends on the tree's `#+TODO` vocabulary (a keyword the layer stops
  declaring changes every row's state cell). A cache entry is a pure
  function of (bytes, config); invalidation is exact by construction.
- **Home**: `$XDG_CACHE_HOME/glance/<tree-key>/` — never inside the tree.
  `.org-glance/cache/` is org-glance's, and the tree is synced across
  machines while the cache is per machine and per glance version.
- **Shape**: one snapshot written atomically (temp-then-rename, the repo's
  own write law) in the background after the tree lands and after each
  drain that changed rows; loaded at boot if its version matches. No
  append log — losing the cache costs a slower boot and nothing else.
- **Freshness** is unchanged: the watch and the WAL tail
  (`2026-08-26-the-daemon-tails-the-wal.md`) reparse the one file that
  moved and update memory and cache alike.

### 3. Per-axis indexes when a query gets slow

The query language is conjunctive across axes with a per-axis disjunction
(the additive-filters law), which is exactly what inverted indexes serve:
`tag → ids`, `state → ids`, sorted date arrays for the prefix and comparison
keys, a trigram or simple substring index for free text last. Build them
from the projection at boot (cheap once bytes are off the heap), maintain
them per reparse. Not before a measurement says a query is slow — today a
full pass over 7539 rows is not.

## The WAL's part

org-glance's records carry a `hash` per blob. glance cannot compute it (a
private normalization) but can compare it: an opaque version token for the
store subset, read off the WAL tail instead of stat'ing 6106 blobs. A later
optimization over stage 2, and only once the fold is consistent (the 21
skipped captures have no record; a listing still finds them).

## What this is not

Not a second truth: every cached row is reproducible from bytes and config
by the same parser, and `glance scan` stays the instrument that says so.
Not org-glance's `property-index.eld`, which is theirs.

## Oracle

Stage 1: a materialize after boot reads the file (a mutated blob on disk
shows through without a reparse of the projection). Stage 2: boot twice,
the second parses zero files; touch one file, it parses one; change a
config layer's `#+TODO`, every row under it reparses; delete the cache,
boot is the old full walk and the answers are byte-identical
(`TestSelfContained`-style: the projection with and without cache compare
equal). Stage 3: the filter answers are unchanged by the index, pinned
over the corpus.

Inert until reviewed.
