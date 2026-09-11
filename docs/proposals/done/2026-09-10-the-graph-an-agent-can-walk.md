# Proposal — the graph an agent can walk over MCP

**Status:** done · 2026-09-12 · **Date:** 2026-09-10 · **Origin:** user, while an agent
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


## Landed

2026-09-12, four parts, each test-first.

1. **`edges: true` on the two read tools, and on the table's own shape.** ONE
   RESOLVED EDGE RELATION per store version rather than a `ref:` pass per row:
   `resolvedEdges` puts every `hrLinks` entry through the name index
   (`nameClaims`, keyed by `refNames`, so a title mention and an `[[id:…]]`
   link resolve the one way), drops the link that names no row and the row's
   own self-edge, and `edgeIndex` reads the result from both ends
   (`src-query/Glance/Query.hs`). It is cached lazily on the store
   (`stEdges`, `Store.hs`), so an `edges=true` request and a `neighbors` walk
   cost what the rows they serve cost. `referrersIn`, `edgePairs` and
   `neighborhood` all read that one structure; `Filter.storeEnv`'s `ref:*any*`
   half reads the same `nameClaims`. Each row gains `"refs": [{to, kind, via}]`
   — `to` a ROW ID — and `"referrers": [ID…]`, under `shape=rows` and under the
   table shape alike; without the flag the answer is the one it was. Tests:
   `TestServe.hs` "tools/call list-headlines with edges carries refs and
   referrers" (a link naming no row included), "without edges a row carries
   neither field", "tools/call get-headline with edges carries that row's own
   edges", "edges is true, and it rides either shape", "an edges argument that
   is no boolean is refused"; `TestQuery.hs` "the reverse index answers ref:
   over every namespace".

2. **`neighbors`, a tool and a route.** `neighborhood` (`Query.hs`) walks the
   store's index breadth-first over a `Set`, both directions a hop, and draws
   every edge between the nodes it holds — the induced subgraph, so an edge to
   a row the cap left out is dropped with it. `total` is the count reached
   before `limit` trims, `summaryEnvelope`'s own rule, and a node is
   `rowSummaryJSON`, the row shape `list-headlines` serves. `GET /neighbors`
   (`Routes.hs`, `AGENTS.hs`'s route table) is the door; the MCP tool
   synthesises that request the way `list-headlines` synthesises `/headlines`,
   so the numeric walls — `wholeNumber`, then `cappedAt` for the depth
   (`neighborDepthCap`, 3) and the limit (`limitCap`) — are inherited rather
   than spelled twice. Tests: "tools/call neighbors walks both ways, one hop by
   default", "neighbors narrows to a kind, caps its depth and refuses a
   stranger", "GET /neighbors walks the graph and meets the same walls".

3. **`shape: "rows"` was already there** — see *Where this disagreed with the
   code*. What it gained is the optional `refs`/`referrers`: `rowSummaryPairs`
   is the row's own fields and `summaryEnvelope` takes the function riding
   extra ones (`Query.hs`), exactly as `rowJSONFor` and `viewJSONFor` do for
   the table.

4. **`add-link`.** A twelfth `/command` verb (`Commands.hs`, spec `AGENTS.hs`),
   through the one door: `Watch.writeSpans` → `Query.replaceSpans` →
   `Edit.editFile`, digest-locked, ledger note, watch nudge. `LinkPlace`
   (`InBody`/`InTitle`) is the closed word the arg wall, the refusal's sentence
   and `addLinkEdits` share; the title road IS `setTitleEdits`, so the headline
   meets one wall. The body road (`bodyLinkEdit`) joins the last written line of
   `ownBody` — the subtree's lines less every lifted region, stopping above the
   first child by `headingStars`, so a `*bold*` line is body — else opens a line
   at the body's start. `glanceLink` renders through `spelling` and `reshaped`,
   slugs the kind the peer's way, and spells the scheme beside
   `materialSchemes` (`writtenScheme`). THE TARGET RESOLVES ONCE AT THE DOOR:
   `resolveAsked` names it among the rows the ids resolve among (`headlinesIn`),
   asks `linkTargetIn` for that row's org id, and puts the finished link on
   `Asked` beside the day — `Asks` (`AsksNothing`/`AsksDate`/`AsksLink`) is what
   says which command owes which. An unknown target is the request's 400, an
   unknown `id` the per-row refusal every command answers with. Tests:
   "tools/call add-link writes a body edge the reverse read answers", "add-link
   lands in the title where it is asked to", "add-link refuses a target no row
   carries, and an id that is no row", plus the placement group "the edge
   add-link appends" in `TestQuery.hs`, a `*bold*` line among them.

Gate: `cabal test` 2335, elm 189, `runghc AGENTS.hs` 0 (`commands 12`,
`routes 17`), `make browser-check` 98/98 (the explorer now lists 16 tools).

## Where this disagreed with the code

- **Part 2, the `terse` answer, had already landed** with
  `2026-09-10-the-doctor-is-its-own-tool.md`: `shape=rows` and
  `summaryEnvelope` were in the tree before this was read
  (`Routes.hs`, `Query.hs`). Its answer carries `clean` beside `total` and
  `rows`, which this file does not mention. Only `refs?` was owed.
- **The line numbers had drifted.** As the tree stands: `linksView` is
  `Routes.hs:865` and its route `:163` (the file said `:161,857`); `rowJSON` is
  `Query.hs:2619` (said `:2712`); `list-headlines` is `Mcp.hs:248` (said
  `:254`); `refTargetOf` is `Query.hs:688` (said `:696`).
- **`refs` is not `/links`.** The file speaks of the edges as "what the row
  points at", which `/links` also answers. They are two relations: `/links`
  lists every org link as written, span and all, and feeds `edit-link`; `refs`
  lists the RESOLVED row edges and feeds the graph. A link naming no row is in
  the first and not the second.
- **The sixth id spelling is gone.** The path form the file names under *What
  an agent hits* was the bug filed alongside, fixed the commit before this one:
  `hrOrgId` and `hrId` both address a row now, which is what `linkTargetIn`
  rests on.
- **`neighbors` grew a `limit`.** The file caps node count by "the `limit` the
  table already honours" without putting one in the tool's arguments; it is an
  argument here, defaulting to 200.
- **`referrers` rides one index, and it is the store's.** The file costs it as
  "one `ref:` pass per request"; it is one resolved edge relation built per
  STORE VERSION (`stEdges`), which answers every row served, every walk and
  every request over that version rather than the one row asked about.

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
