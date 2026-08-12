# Proposal — the two programs are run against ONE store, so the contract is asserted rather than mirrored

**Status:** done — LANDED 2026-08-12 as `make interop`, twelve cases, both
directions, one of them seen red against the daemon's own inotify wiring with
the whole Haskell suite green (1857 when the experiment was run, 1867 today —
see "What landed") ·
**Date:** 2026-08-12 · **Origin:** M4 box 3 of
[plan-org-console-web.md](plan-org-console-web.md) — "round-trip demo,
browser↔Emacs" — the last unchecked box of the write-back milestone.

## The gap

glance appends `{"id","at"}` to `<store>/.org-glance/meta/EXTERNAL.jsonl` on
every blob write (`Data.Org.External`). org-glance's `refresh-external` adopts
each id and shortens the file by the prefix it read
(`src/data/org-glance-graph.el`). **Both sides have tests. Neither has ever
fed the other.**

The two suites pin the same format twice, independently and by hand:

| | glance | org-glance |
|---|---|---|
| the line | `TestExternal.hs:196` golden string | `test-external.el:10-20`, the format spelled out rather than encoded |
| the path | `TestExternal.hs:229` against literals | never asserted literally — every case goes through the accessor |
| the reader | never runs | `test-external.el` × 14 over lines the suite wrote itself |
| the writer | `TestExternal.hs` × 20 over files nobody reads | never runs |

Rename a field on one side and both suites stay green. The same holds one
layer out: glance's `Data.Org.Index` reads MANIFEST, segments, tombstones and
the `archived` flag off a store its own tests HAND-WRITE (`TestIndex.hs:53`
`metaStore`), never one org-glance produced — so how org-glance actually
serializes a false boolean (`{}`, since its encoder has no bool kind) is
inferred from reading the code and asserted nowhere.

And the Emacs→browser direction rests entirely on `watchOrgTree`, which has
**one call site in the repo and zero in `test/`**. Every pipeline test calls
`drain` or `settle` directly. Sever the inotify callback and the whole Haskell
suite (1857 when this was measured, 1867 today), 65 elm-test cases,
`check-glue` and `browser-check` are all still green — which this proposal's
target is what found.

## The shape

`make interop`, out of `cabal test` for `browser-check`'s reason one size up:
it needs Emacs, a sibling org-glance checkout with its dependencies installed,
and a daemon over a temp store. It skips loudly on each of those, naming
WHICH is missing.

**ONE STORE, and the ORDER IS THE STORY.** Emacs seeds the store, the daemon
serves it, and each step asserts about the state the step before it left. There is no `ONLY` — a case cut out of the
middle would leave the ones after it asking about a state that was never
reached. A failure is reported and the run continues, because the steps after
it are still evidence; the FIRST red line is the one to read.

**Two files.** `test/interop/og.el` is the Emacs half: one job per
invocation, the whole interface the environment, one JSON object on stdout.
It only REPORTS — every assertion is the driver's, so a step that quietly
agreed with itself is impossible. `test/interop/drive.mjs` is the
driver: daemon lifecycle, HTTP, one `?bootstrap=off` socket, and the
assertions. Zero dependencies, node's own `fetch` and `WebSocket`, the shape
`test/browser/drive.mjs` already established.

**It reads the LIVE peer.** `og.el` puts org-glance's `src/data` and
`src/view` on `load-path` and SKIPS the copies `eask install` left under
`.eask/*/elpa` — those are as old as the last release. `load-prefer-newer` is
set, so a stale `.elc` never answers for a `.el`.

**HOST EMACS IS THE DEFAULT**, because the peer is a sibling checkout like
`../table-view` and a whole run costs ~21 s. `EMACS_RUN=podman` is the pinned
path and builds org-glance's OWN Containerfile through its OWN `podman-build`
target — no second image is described here. The store is bind-mounted at ITS
OWN PATH, so every path string the harness compares means the same thing on
both sides of the mount.

## What it asserts

Twelve cases, each closing one claim a reader's census marked *asserted by
NEITHER*:

| case | claim | what it closes |
|---|---|---|
| `sidecars-are-not-rows` | 20 | a tree org-glance built yields exactly its blobs as rows — nothing out of `meta/`, `config/`, `trash/`, `occurrences/` |
| `blob-path-agrees` | 4 | `blobPathIn` and `headline-data-path` name the SAME string for one id |
| `external-bytes` | 2 + 3 | org-glance's reader takes glance's bytes, out of the file its own accessor names |
| `meta-untouched` | 21 | a glance write leaves every other `meta/` file byte-identical and creates only `EXTERNAL.jsonl` |
| `emacs-adopts` | 5 | `refresh-external` returns 1 and the WAL reports the keyword glance wrote |
| `file-emptied-and-reused` | 8 | the file is emptied, kept, its inode kept, and glance appends to it again |
| `tag-cycle-survives` | 7 | a keyword only the tag's own `#+TODO:` declares round-trips as a STATE |
| `browser-sees-emacs` | 19 | org-glance's `put-content` reaches an open socket as `upsert-row`, with no notification file involved |
| `archive-flag-round-trips` | 14 | the `archived` value org-glance serializes reads as glance's `(eq t VALUE)` flag |
| `scan-agrees-with-the-writer` | 15 | `glance scan` over a real org-glance store: 0 rows disagree, 0 unmatched either way, 0 span violations |
| `HOLE: a tagged capture never reaches the WAL` | 17 | PINNED |
| `delete-tombstones-the-record` | 18 | a browser delete appends `"tombstone":true`; org-glance reads the kind, drops the record, and glance's own fold reads the tombstone back |

**The delete leg closed on 2026-08-12; the create leg is still pinned.** Create
and delete were the two blob-lifecycle events the notification file did not
carry. A delete now appends the plain line plus a third field — out of
`Data.Org.Trash.trashBlob`'s success branch, the other door bytes move by, since
a delete splices no spans and reaches `replaceSpans` never — and
`refresh-external` folds it as the tombstone `graph:delete` would write. What is
left is the browser capture, which mints an id org-glance's fold skips as
*unknown or deleted*; its case asserts TODAY's behaviour, so closing it turns
that case red and names the decision rather than letting it drift. One line of
glance's own instrument reports both halves at once:

    unmatched 1 unindexed blobs, 0 records without blobs

The zero is `Data.Org.Index`'s fold rather than a count that happened to move: a
tombstoned id leaves the fold, so it can no longer be a record without a blob.

**The version skew is safe in both directions**, which is why the delete is a
third FIELD rather than a new `op` vocabulary. A NEW glance against an OLD
org-glance degrades to exactly the old behaviour — `--read-external` reads `id`
alone and ignores keys it does not know, so the id is read, the blob it names is
gone, and the line is skipped as *no stored blob*. An OLD glance against a NEW
org-glance writes the field never, so nothing changes.

## Seeing it fail

**`BREAK=name` takes ONE step out of the HARNESS** and names the case that
must go red — `browser-check`'s idiom, and it proves the assertions read what
the OTHER program did rather than what this one set up. Six knobs:
`no-write`, `no-refresh`, `no-put`, `wrong-id`, `meta-moved`, and
`no-delete-fold`, which leaves the tombstone line standing so the record it
names stays live. Each was run and each turned its own case red first.

**And the daemon itself was broken once**, which is the reading that matters.
`src-web/Glance/Web/Watch.hs`'s

```haskell
where note = nudge opts hub . FS.eventPath
```

replaced by `const (pure ())` — inotify events registered and delivered
nowhere — and reverted afterwards. With it broken:

- `cabal test` — **All 1857 tests passed** (the suite's count on the day; it is
  1867 today, and the experiment has not been re-run at that count)
- `make interop` — `not ok — browser-sees-emacs [CLAIM 19]`,
  *waited 10000ms for the watch to deliver org-glance's own write as a row*

Every browser-originated write still landed, because those reach the table
through the explicit `nudge` every write door makes. What died was the whole
Emacs→browser direction, silently and one-directionally. That is the coverage
hole this target exists for.

## What landed

    12/12 cases, 21.3s wall, host emacs
    put-content -> upsert-row in 142 ms, with EXTERNAL.jsonl untouched

The latency is measured from the clock Emacs read once its rename had landed,
so none of a batch Emacs's half-second start-up is counted as the daemon's. It
sits beside S5's own 105–107 ms for a plain editor write.

## What it does not reach

- **The podman path is unverified on this machine.** `podman build` fails
  before any of it — *mounting an overlay over build context directory … no
  such device*, a rootless-overlay limitation reproducible with org-glance's
  own `make podman-test` — so the pinned path ships and skips loudly, and the
  measurements above are host Emacs's.
- **The WS frame contract with the SHELL** is still asserted twice
  independently: `Store.frameJSON` here, `socket.onmessage` in the shell
  harness. This target speaks the socket, so the frame's SHAPE is now read by
  a second program — but that program is this driver, not `table-view.js`.
- **The concurrent window.** org-glance's `--truncate-external` re-reads the
  file and writes the tail back; a line appended between that read and that
  write is dropped (CLAIM 9's second half). Finding it needs glance writing
  while Emacs folds, which no case here does.
- **The suite's own count is unmoved.** Nothing here is a Haskell test and
  nothing here runs offline.

## What org-glance owes, if anything

Nothing, for this target to pass. Two holes were the only findings, both
decisions rather than defects, and one has since been taken:

1. **A tagged capture through glance is invisible to Emacs** until something
   calls `graph:add` for it. Either side could close it — an adopt-unknown
   path in `refresh-external`, or a distinct notification from glance. OPEN,
   and pinned by its own case.
2. **A delete through glance left the record live.** TAKEN on 2026-08-12, by
   both repos in one sitting: glance appends `"tombstone":true` beside the
   frozen two fields, and `refresh-external` folds that line into the tombstone
   `graph:delete` would write. The word is org-glance's own WAL spelling, and
   the field only ever carries JSON `true` — absence is the plain line, and
   `true` is what the peer's own `(eq t …)` reader takes as a delete, that being
   the stricter of the two readers (`Index.recordOf` reads the WAL's key with
   `truthy`).
