# Proposal — two writers, one WAL: the relay is the design, finish it

**Status:** draft · **Date:** 2026-09-10 · **Origin:** user, after
`2026-09-10-the-graph-an-agent-can-walk.md` — *"would it be fine to build the
graph from the WAL instead of the walk?"* then *"what if we adapt the system
so both write to it?"*

## The short answer

Both already do, through a relay: the daemon appends `{id, at}` to
`meta/EXTERNAL.jsonl` (`src/Data/Org/External.hs:75-96`, `O_CREAT|O_APPEND`
one line per open), and org-glance folds those notes into its WAL on the idle
poll (`org-glance-graph.el:866-885`, invariant 34, CLAIM 17 *"a tagged
capture reaches the WAL"*). The WAL is complete **eventually** — once an Emacs
has run. What this proposal changes is the gap before that, and it argues
AGAINST the literal reading, both processes appending to `headlines.jsonl`.

## Why not a direct dual append

1. **Atomicity.** `O_APPEND` writes are atomic only up to `PIPE_BUF` (4 KB on
   Linux). A record carrying `links` already exceeds that (the birthday row
   in the open segment holds three Amazon/bol URLs in one line). Two writers,
   one file, one long line: a torn record with the other writer's bytes in
   the middle. `Data.Org.Index` forgives a torn TAIL, not a torn MIDDLE.
2. **Seal ownership.** `MANIFEST` is rewritten on seal by org-glance
   (`--segment-path`, temp-then-rename, `org-glance-graph.el:276-307`). A
   second writer holding `headlines.jsonl` open across a seal writes into a
   file the manifest has just renamed — the exact race the EXTERNAL rotation
   commentary (`:1358-1365`) designed around by opening per line.
3. **`seq`.** One monotone counter, derived on heal (`--heal`, `:251`). Two
   writers means two counters or a lock; "latest record wins" stops meaning
   anything without one.
4. **Two parsers, one schema.** A WAL record's `links`, `relations`, `hash`,
   `range` are org-mode's reading
   (`org-glance-headline--buffer-content-facts`,
   `org-glance-headline.el:222-238`). The daemon's reading is
   `Query.hs`'s. The moment they differ in one edge's order, the drift
   instrument fires on every daemon record. Parity between two parsers is a
   test suite, not a promise.

The relay avoids all four by construction: one file per writer, a fold that
re-parses the blob with the ONE parser the WAL's fields belong to.

## What is actually missing

- **The note is blind.** `{id, at}` says *something changed*; the fold must
  open the blob to learn what. A `hash` in the note (the daemon has the bytes
  in hand at `noteBlob`) lets the fold skip a blob whose record already
  carries that hash, and lets any reader tell *adopted* from *pending*.
- **Pending reads as drift.** `doctor` reports the daemon's own unadopted
  captures as *"5 unindexed blobs"* beside genuine drift (33 rows). They are
  not the same fact. An agent, or a person, reading `doctor` cannot tell a
  store that needs an Emacs pass from one whose files were edited by hand.
- **The daemon's own view of the WAL stops at the last Emacs fold.** For a
  WAL-derived answer (drift, or a warm start if one is ever wanted —
  `2026-08-26-the-daemon-tails-the-wal.md` measured the walk at 0.71 s and
  declined), the daemon should fold `EXTERNAL.jsonl` as a PROVISIONAL segment
  after the sealed ones: same read law, records marked `pending: true`.

## Proposed change

1. **`hash` on the note.** `externalLine` grows `"hash": <the blob digest the
   walk already computes>`. Unknown keys are inert on the Emacs side by
   contract (`:869-870`), so old Emacsen ignore it; a new fold compares it
   to the record's `hash` and skips the parse on a match.
2. **`Data.Org.Index.foldSegments` takes `EXTERNAL.jsonl` last**, as a
   provisional layer: a note whose id has a WAL record with the same hash is
   spent; a note whose hash differs from the record's, or has no record,
   yields an `IndexRecord` marked pending. **The hash decides, never `at`**:
   `at` is one writer's clock, and with N writers on N hosts (below) two
   clocks cannot order anything. `driftOf` then reports three numbers —
   drift, pending, missing — and `doctor` prints them apart.
3. **A parity oracle for the graph fields only.** `test/interop`: for a
   fixture set, org-glance's `relations` (id + kind) and the daemon's
   `hrLinks` must agree as SETS. Not `links` (raw strings, order and dedup
   differ by design), not `hash`. This is the one place the two readings
   must meet, because `ref:`/`from:` and org-glance's relation overview
   should answer the same question the same way.

## Does it hold for N writers?

Asked (user, 2026-09-10): *"potentially there are many writers — check the
architecture scales."* Writers that exist or are coming: the daemon; agents
through `glance mcp`, which are the daemon; a second Emacs (invariant 7 already
allows it); scripts and hand edits (`sed`, an agent rewriting `data.org`);
and every one of those again on another host, the store being git-synced.
Taken per layer:

| layer | N-safe? | why |
|---|---|---|
| N note-writers, one host, one `EXTERNAL.jsonl` | yes | each line is one `open(O_APPEND)` + write + close (`External.hs`, `graph.el:1358-1361`); a note is ~80 bytes, ~150 with `hash`, far under `PIPE_BUF`, so appends never interleave; rotation is a rename by the reader and a line landing mid-rename lands in the generation, which is folded first |
| N folders (Emacsen) | yes | no lock, a doubled fold appends equal records (invariant 34, inv 7); positional last-wins (inv 1) makes the duplicate harmless |
| N hosts, the WAL through git | yes, by design | `headlines.jsonl` and `seg-*.jsonl` union-merge (inv 8) and resolve by position (inv 1), never by `seq` or a clock |
| N hosts, `EXTERNAL.jsonl` through git | **no, and the live store does it** | the family is meant to be git-ignored (`--ensure-gitignore`, `graph.el:880,1545`) and out of the union resolver (inv 8). `~/sync/views` tracks `meta/EXTERNAL.jsonl` and its `.gitattributes` still reads `*.jsonl merge=union` — the glob inv 8 says was retired. A note from host A folded on host B before A's blob has synced is a record with no blob; `doctor` counts 30 of those today |
| writers that leave no note | **no** | `sed`, an agent's Python, a git pull bringing another host's blobs: none append a note, so only the walk sees them. This is the gap the relay cannot close by adding writers to it |
| the provisional layer in the daemon's fold | yes | bounded by the 1 MiB rotation cap (`org-glance-graph-external-max-bytes`); it reads the live file and its generations, never `meta/spent/` |

Two consequences for the design:

- **One noter per host, and it is the daemon's watcher.** Instead of asking
  every writer to append a note (a contract no `sed` will honour), the daemon
  notes every blob its fsnotify sees change that it did not write itself —
  `Watch.hs` already distinguishes its own writes (`writeSpans`). Then N
  local writers collapse to one note stream per host, and a hand edit is
  adopted by the next Emacs pass like a capture. The fresh-shard gap
  (`docs/bugs/open/2026-08-26-a-headline-org-glance-creates-is-invisible-until-restart.md`)
  applies here too and is fixed by the same nudge.
- **Notes stay local; records travel.** The relay is N→1 per host; git is
  the 1→N between hosts, and only for the WAL. Fixing the tracked
  `EXTERNAL.jsonl` in the live store is a one-line `.gitattributes` and a
  `git rm --cached`; filed beside this as a bug.

What does NOT scale, and is out of scope: two hosts editing the same blob
between syncs. That is a git conflict on `data.org`, not a WAL question,
and the digest CAS (`2026-08-18-one-writer-per-file-and-a-version-the-client-can-check.md`)
is the per-file answer.

## Not proposed

- Direct appends to `headlines.jsonl` from the daemon (the four counts
  above).
- Moving fact computation to the daemon. The WAL's fields are org-glance's;
  the daemon's truth is the walk (`docs/proposals/proposed/2026-08-26-the-daemon-tails-the-wal.md`,
  "Why not index FROM the WAL").
- A backfill of `links`/`relations` on the 5670 pre-field records. It is an
  org-glance task, and after (2) the daemon does not need it.

## Cost

- (1): one field in `noteLine`; the digest is in scope at every `noteBlob`
  call site.
- (2): one more segment name in `segmentNames`' fold order plus a `pending`
  flag on `IndexRecord`; `indexReportLines` prints one more line.
- (3): one interop case over the existing fixture tree.

## Evidence

- `src/Data/Org/External.hs:37,75-96`; `src/Data/Org/Index.hs:36-80`
- `~/sync/stuff/org-glance/src/data/org-glance-graph.el:249-307,866-900,1335-1370`
- `AGENTS.hs:729-742` (CLAIM 5 emacs-adopts, CLAIM 17)
- `docs/proposals/proposed/2026-08-26-the-daemon-tails-the-wal.md`, "Why not
  index FROM the WAL"
- Live store 2026-09-10: `meta/EXTERNAL.jsonl` 1597 lines, five of them the
  session's captures, absent from the WAL and counted by `doctor` as
  unindexed; `meta/headlines.jsonl` line 1 is 1.9 KB, over `PIPE_BUF` when a
  second URL rides.
