# Proposal — the graph an agent can walk over MCP

**Status:** draft · **Date:** 2026-09-10 · **Origin:** user, while an agent
planned a trip over `glance mcp` against `~/sync/views` — *"what is missing
for building the graph efficiently?"* The agent's first answer was wrong (it
read `meta/headlines.jsonl` alone and called the index 4% complete); this
file holds what survived a second look.

## What already holds

- The walk normalizes every material scheme to one reference:
  `materialSchemes = ["glance", "org-glance-material", "org-glance-visit",
  "org-glance-open"]` (`src-query/Glance/Query.hs:631`), and `hrLinks` keeps
  the subtree's references with an optional `?kind=` (`Query.hs:307`,
  `refTargetOf` `:696`).
- Both ends of an edge are queryable: `ref:ID` (rows pointing at ID) and
  `from:ID` (rows ID points at), kind-blind or `?kind=SLUG`, plus `*any*`
  (`docs/query.md:75-150`). Measured over the live store: `ref:Contact-2021…`
  served 5 rows across four id generations, `from:<trip>` served 8.
- The MCP catalog exposes `list-headlines` with that query language
  (`src-web/Glance/Web/Mcp.hs:254`), so backlinks are one call away today.

So the graph is not missing. What is missing is the shape an agent can
consume without re-deriving it per call.

## What an agent hits

1. **Edges are not on the wire.** `list-headlines` returns rows and cells;
   the outgoing references (`hrLinks`) are not a field. `GET /links?id=`
   (`Routes.hs:161`, `linksView` `:857`) has them but is not an MCP tool.
   Walking one hop costs one `from:` query per node, and the answer names
   the neighbour rows but not the edge's kind or where it was written.
2. **The WAL is a different animal.** `meta/seg-*.jsonl` + `headlines.jsonl`
   hold 6167 records, of which 482 carry `links`/`relations`; the other 5670
   predate those fields and carry only `linked: true`. An agent that reads
   the WAL for edges sees ~30. The fields are org-glance's
   (`org-glance-headline--buffer-content-facts`,
   `src/data/org-glance-headline.el:222-238`); glance reads the WAL for
   drift only (`Data.Org.Index`). Nothing tells the agent which of the two
   to trust for edges.
3. **Every list answer carries the table's chrome.** ~2.7 KB of `columns`,
   badges and `actions` per call before the first row (measured: a
   zero-row `from:` answer was 2696 bytes). `total` is absent, so a capped
   answer cannot say what it cut.
4. **Row ids come in five spellings** — `Contact-2021…-<md5>`,
   `location-25033-…-d41d8…`, `city-spbm-…`, a 128-hex digest, uuid — and a
   sixth when the drawer is unreadable: `<path>#<ordinal>` (`rowIdIn`
   `Query.hs:1268`). The path form is a stable-looking id that resolves in
   `from:` and nowhere else (`get-headline` by the blob's own uuid answered
   *no headline*). See the bug filed alongside.
5. **No edge in the body can be written.** `edit-link` takes a title span;
   `capture` takes a body string. Linking two existing rows means rewriting
   `data.org` outside the daemon, which is what the trip session did.

## Proposed change

Three additions, each additive to SCHEMA.md's Row and the MCP catalog.

### 1. `links` and `backlinks` on the wire

- `list-headlines` and `get-headline` grow an optional `edges: true`
  argument. With it, each row carries
  `"refs": [{"to": ID, "kind": SLUG|null, "via": "row"|"org-id"}]` —
  `hrLinks` as it stands, no new parse — and
  `"referrers": [ID…]`, the `ref:ID` answer computed once per request.
- One new read tool, `neighbors`: `{id, depth?: 1, kind?}` → the induced
  subgraph as `{nodes: [{id, title, tags}], edges: [{from, to, kind}]}`.
  Depth capped (3) and node count capped (the `limit` the table already
  honours). This is `from:`/`ref:` iterated inside the daemon, where the
  rows are already in memory, instead of N round trips.

### 2. A `terse` answer for machines

`list-headlines` takes `shape: "rows"` (default keeps today's page) and
returns `{total, rows: [{id, title, state, tags, refs?}]}` — no columns, no
badges, no actions. `total` is the uncapped count so a `limit` is honest.

### 3. `add-link`, the body edge

`add-link {id, target, kind?, desc?, where?: "body"|"title"}` appends
`[[glance:TARGET?kind=KIND][DESC]]` under the headline (body: last paragraph
or a new one; title: after the text) through the same `/command` path
`edit-link` uses, digest-locked. The reverse edge needs no write — `ref:`
derives it.

## Not proposed

- Changing what the WAL carries. It is org-glance's contract; the daemon's
  walk already has the edges, and `docs/proposals/proposed/2026-08-26-the-daemon-tails-the-wal.md`
  covers the daemon reading it for nudges. A backfill of `links` on the
  5670 old records belongs to org-glance and is noted in its tasks, not
  here.
- An id alias table. Five spellings resolve today because the walk keys
  by `ORG_GLANCE_ID` verbatim; only the path fallback leaks. Fix the leak
  (the bug) rather than add a namespace.

## Cost

- `refs`/`referrers`: a field per row, computed from data the record already
  holds; `referrers` is one `ref:` pass per request when asked for.
- `neighbors`: one function over `qrRecords`, breadth-first with two caps.
- `shape: "rows"`: one alternative encoder beside `rowJSON`
  (`Query.hs:2712`).
- `add-link`: one `/command` verb; the write path and digest lock exist.

## Evidence

- `src-query/Glance/Query.hs:307,631,676-700,757-780,1268,2712`
- `src-web/Glance/Web/Mcp.hs:207-258`, `src-web/Glance/Web/Routes.hs:161,857-866`
- `docs/query.md:75-150`
- Live store 2026-09-10: 6165 blobs with `ORG_GLANCE_ID`; WAL 6167 ids
  (segs 17, 18 + open), 482 with `links`; 1904 blobs carry an
  `org-glance-visit:` link (658 in the title, 1246 in the body).
