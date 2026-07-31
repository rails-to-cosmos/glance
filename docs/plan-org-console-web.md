# Plan — org-console-web

**Companion to:** [proposal-org-console-web.md](proposal-org-console-web.md) · **Date:** 2026-07-30

Steps ship in order unless marked parallel. Each step has an exit bar: every
box checked = step done. Perf budgets derive from the S1 baseline; steps
record actual numbers next to the boxes as they close.

## Current state (2026-07-30)

- glance: parser 278 + types 531 lines, 9 test modules; no source offsets, no
  `aeson`/web deps, `RefKind` defined and unwired, `Persist.Org`/`Data.Config`
  stubs, `Repl.Org` REPL.
- table-view: `SCHEMA.md` canonical, `table-view.el`, `web/table-view.js`
  (361 lines) + `demo.html`; no `fixtures/` dir yet.
- repos / ray-cluster: already emit the contract with live upsert.

## S1 — Span retention + corpus baseline

Thread source offsets (megaparsec `getOffset`) through the AST for every
mutable element: TODO keyword, priority, title, tags, timestamps, property
drawer, body extent (append point), full headline extent (capture/refile
point).

Exit:
- [x] Slice test: TestSpans.hs — exact slices (todo/priority/tags), words-check
      (title), structural (properties), full-span and element-span reparse,
      invariants (bounds, containment, ordering). Mutation-checked (span shifts
      break 11–24 tests).
- [x] Corpus run: `glance scan ~/sync` — 6305 files, 6276 ok, 13 parse
      failures (enumerated below), 16 decode failures (macOS AppleDouble
      sidecars, junk). Zero span violations corpus-wide.
- [x] Baseline recorded below.
- [x] Test suite green: 108 → 221 tests (span suite + fidelity fixes).

Baseline (2026-07-30): _files: 6305 · headlines: 13338 · elements: 567697 ·
wall: 13.6 s warm / ~34 s cold · 464 files/s · max residency 19 MB_

**Landed during S1 beyond span threading** — corpus truth forced four parser
fixes: trailing-hspace / indented-drawer handling (silent headline and drawer
destruction); timestamp ranges `[a]--[b]` (`Timestamp` now carries `TsMoment`
start/end with a has-time flag; CLOCK files were 97% of parse failures);
headline anchoring to column 1 (mid-line `*bold*` no longer a fake headline;
O(1) begin-of-line flag, no `getSourcePos`); case-sensitive TODO keywords
stored verbatim. Known divergence kept: indented stars are no longer
headlines (org's column-1 rule).

**Known parse failures (13, one class)** — an element parser stops mid-word
and leaves a non-space char where the top level expects whitespace: 6×
`" :: "` inside a title, 2× title ending in `:)`, 3× timestamp glued to
punctuation (`<…>,` `<…>]` `[…]]`), 2× hyphenated word in a commented
`#+TODO:` line. Fix direction (S2-adjacent): tags parse only at end of line;
timestamp closes on bracket regardless of following char. Emacs intra-day
time ranges `<… 10:30-12:00>` degrade to tokens (no file failure).

## S2 — `Glance.Query` facade + JSON layer

Stable API the web layer depends on; parser refactors stay behind it. Add
`aeson`; `ToJSON` for view/row per `SCHEMA.md`. `Display`/`TextShow` stay out
of the wire.

Exit:
- [x] `Glance.Query.viewJSON dir` output validates against `SCHEMA.md` field
      tables (golden test with committed expected JSON for a sample org file).
      Actual: `TestQuery`, group `Query` — golden `Query/View/matches
      test/fixtures/sample-view.json` (both sides decoded to `Value`, so key
      order is free) plus a `Schema conformance` group: cell keys ⊆ column
      keys, badge column carries a palette, sort column exists, every row has
      an id, no `actions` yet.
- [x] Rows carry: state badge, title, tags, priority, scheduled date.
      Actual columns: `state` (badge, sortable), `priority` (sortable, values
      A/B/C), `title`, `tags`, `scheduled` (sortable), `deadline` (sortable —
      our addition, schema-legal). Sort: scheduled ascending.
- [x] Web/daemon code imports only `Glance.Query` — enforced by a separate
      cabal library stanza (daemon target lacks `Data.Org.*` in build-depends).
      Actual: the public `library` exposes `Glance.Query` alone over the
      private sublibrary `glance-internal` (parser, AST, walk). The CLI and the
      suite name the sublibrary explicitly (`glance:{glance, glance-internal}`
      in the suite); nothing outside the package can, private being a
      sublibrary's default visibility.

**Landed during S2** — `aeson` 2.2 added. The file walk moved out of
`app/Scan.hs` into `Data.Org.Walk` so `runScan` and `loadDir` discover the same
files; the extraction changes no scan number. The `~/sync` run now reads 6308
files / 13343 headlines / 14 parse / 17 decode because this repo sits inside
`~/sync` and the three new fixtures join the corpus — S1's 6305 / 13337 / 13 /
16 plus exactly them. Span violations stay 0. Suite 243 → 261 tests. Cells
are cut from spans at load time and copied out of the document; `hrHeadline`
still shares text with its file, so full-store residency is the S3 number to
watch. Row identity is the `ORG_GLANCE_ID` property, else `FILE:START` — an
edit above a headline renames its row, which S5's watch has to answer.

## S3 — M0: headlines in a browser tab

`warp`. `GET /headlines` → view JSON. Serve `web/table-view.js` + a demo page.

Exit:
- [x] `curl :PORT/headlines | jq '.rows | length'` equals the corpus headline
      count. Actual: `glance serve --dir ~/sync --port 7799` →
      **13344 rows**, which is S2's 13343 plus the one headline this step's
      README quickstart section adds (this repo sits inside `~/sync`; no new
      `.org` file, `git show HEAD:README.org` counts 12 headlines against 13
      now). Headers on the same response: 6308 files, 14 parse failures, 17
      decode failures, 0 read failures — S2's numbers unchanged.
- [~] Browser tab renders the table; renderer sort works on it. **Not
      clicked** — no browser in this environment. Verified headlessly instead:
      `/` serves the shell as `text/html; charset=utf-8` referencing
      `src="table-view.js"`, `fetch("/headlines")` and `TableView.mount(`
      once each; `/table-view.js` comes back `text/javascript; charset=utf-8`
      and byte-identical to `table-view/web/table-view.js` (17678 B); both the
      renderer and the shell's extracted inline glue pass `node --check`
      (node v26.2.0). Sorting is the renderer's own behaviour, covered by
      table-view's suite; DOM rendering of *our* document is honestly an S4
      item — the shared fixtures are where a glance view gets executed by both
      renderers.
- [x] Full-store request wall time ≤ S1 baseline + 20% (13.6 s + 20% ≈
      16.3 s). Actual: **14.74 s** first request, **14.64 s** / **14.53 s**
      warm, 3057971 B of JSON over loopback. +7.6% on the S1 scan baseline for
      a walk that also builds and encodes the view.
- [x] `glance serve --dir DIR` documented in README quickstart; works from a
      clean checkout. Actual: README "Quickstart — headlines in a browser tab"
      covers the flags, the `X-Glance-*` headers and the loopback bind.

**Landed during S3** — `glance-web`, a second private sublibrary (`src-web/`,
`Glance.Web`) whose `build-depends` names the public library and the HTTP
packages and cannot name `glance-internal` without saying so. One user-facing
binary still: the `glance` executable depends on both sublibraries and does
nothing but dispatch — `serve` to `Glance.Web`, `scan` and the REPL to the
internals. `wai` + `warp` + `http-types` rather than `scotty`: four routes and
a static file need no routing DSL, and a bare `Application` is what
`Network.Wai.Test` drives, so the suite binds no port (`wai-extra` is a
test-only dep). The view document stays SCHEMA.md's four fields — the load
counts ride as `X-Glance-Rows/Files/Parse-Failures/Decode-Failures/Read-Failures`
response headers rather than as a `meta` sibling.

**Decision: 127.0.0.1 only.** `Warp.setHost "127.0.0.1"`, with no flag to
widen it. Every request is served at one privilege level until S7 splits
read/write/automate, and S8 adds write-back behind the same door; a bind on
`0.0.0.0` today would publish the whole store to the network and later hand it
edits. The address changes when authentication exists, not before.

**Parse-on-request.** No index, no cache: each request walks and parses the
directory. At 14.6 s for 6308 files that is plainly a page load you wait for,
and the persistence gate is already written down — full-store parse > 1 s is
one of the two triggers, and it is met — but the gate is checked at S5, where
the watch's re-parse latency is the other half of the decision. S3 records the
number and keeps the flat design. **Superseded at S5**: the directory is parsed
once at startup into an in-memory store and a request costs an encode (0.09 s).

## S4 — Contract fixtures (parallel, after S2)

`table-view/fixtures/`: shared JSON specs both renderers execute.

Exit:
- [ ] ≥ 5 fixtures covering: minimal, badges, multi-sort, actions, streamed
      upsert.
- [ ] `table-view-test.el` runs every fixture; JS harness runs the same files;
      both green in one `make test`.
- [ ] glance golden output from S2 committed as a fixture — producer and
      renderers proven against one file.

## S5 — M1: live updates

WS endpoint + file watch (`fsnotify`) → row upsert frames in the same shape
repos/ray already stream.

Exit:
- [x] Edit an org file in any editor → open browser tab updates the row with
      no reload. Watch-to-render latency ≤ 1 s. Actual: **105–107 ms** from
      `write()` to the `upsert-row` frame arriving at a socket client, measured
      three times against `glance serve --dir ~/sync --port 7799` by appending a
      headline to `glance.org` and removing it again (426 ms on a process's
      first event, which pays for the cold read). The debounce is 100 ms of
      that; the parse is 4 ms. Delete direction: **105 ms**, three of three.
- [x] Daemon restart: browser re-attaches and re-syncs the full view
      unattended. Every close leads through one door — the shell re-fetches
      `/headlines`, remounts, and reconnects, backing off 1 s → 30 s. That
      covers a restart, a dropped slow client, and `view-changed` alike.
- [x] Create + delete file cases handled (row appears / disappears). Actual,
      against a live server over a scratch directory: a new two-headline file →
      `upsert-row` ×2; `rm` of it → `delete-row` ×2; a `#+TODO:` line
      introducing `WAITING` → socket close with reason `view-changed`.

**Landed during S5** — `fsnotify` + `websockets` + `wai-websockets` (and `stm`
from the global package db). Two modules join `glance-web`, both on the public
facade alone: `Glance.Web.Store` (the store, the diff, the frames, the hub) and
`Glance.Web.Watch` (inotify, debounce, one re-parse per event). The facade grew
what they needed rather than being reached around: `loadFile`, `loadDirFiles`,
`LoadFailure`, `rowJSON` and `mergeKeywords` are new exports of `Glance.Query`,
and `loadDir` is now those pieces folded together, so a store built file by file
and a directory loaded in one call produce the same rows in the same order —
which a test asserts and which is why `/headlines` is still byte-for-byte S3's
document (3057971 B, 13344 rows, same `X-Glance-*` counts).

**The store.** One walk at startup (**15 s**, 6308 files, 13344 rows), kept in a
`TVar` keyed by path so `Map.elems` is walk order. `/headlines` renders it:
**0.087–0.110 s** warm over loopback, against 14.53 s at S3 — the same bytes,
150× faster. Residency is the price: **593 MB** RSS for the ~/sync store, since
`hrHeadline` still holds the parser's slices (the lever is written down in
`Glance.Query`'s haddock). The watch costs **89413** inotify watches for the
~/sync tree, one per directory, well inside the 524288 limit here.

**Frames.** SCHEMA.md's streaming ops and nothing else:
`{"op":"set-rows","rows":[…]}`, `{"op":"upsert-row","row":{…}}`,
`{"op":"delete-row","id":"…"}`. A socket opens with one `set-rows` of the whole
store — taken inside the transaction that subscribes, so an edit landing between
the client's `/headlines` fetch and its socket is in the bootstrap rather than
lost, and the server keeps no journal to replay from. Bootstrap of 13344 rows:
**164–180 ms**. The shell mounts `/headlines` for the columns and the sort, then
applies the bootstrap over it.

**Decision: a column change closes the socket.** A changed file can introduce a
TODO keyword, which moves the state column's badge palette. SCHEMA.md streams
rows; columns are initial-view only, and there is no op for this. Inventing one
would put this producer outside the contract it exists to prove, so the server
sends a websocket close with reason `view-changed` and the client's reconnect
path re-fetches the whole view. Cheap, honest, and it reuses the restart path
that had to exist anyway.

**Decision: drop slow clients, never block the watcher.** Each socket has a
bounded 256-frame mailbox, filled from the same STM transaction that updates the
store. A mailbox with no room drops its client rather than retrying the
transaction; the socket closes and the client resyncs on reconnect. Losing a
stalled reader's frames is recoverable, stalling the file watch is not.

**Parse failure keeps the file's rows.** `orgParse` is all-or-nothing, so a save
caught mid-write is indistinguishable from a file whose headlines all vanished.
The store keeps the last good parse's rows, counts the file as a parse failure
(the count `/headlines` already reports), logs one line, and streams nothing.

**Row-id churn, unchanged and documented.** A headline without an
`ORG_GLANCE_ID` is `FILE:START`, so text inserted above it renames its row and
the store cannot tell that from a deletion plus an insertion — it emits both.
Marked headlines edit in place under one id. S8's write-back is where a stable
id for an unmarked headline would have to come from.

Suite: 274 → **301 tests**.

## S5.5 — Materialize: the subtree round-trip (landed, ahead of M3/M4)

Click a row, read the headline's raw subtree, edit it, commit it back
drift-locked. The first raw-replacement command, running on the S8 engine over
loopback — the write path proven end to end before the tiers (S7) and the
structured commands (S8) that will share it.

Exit:
- [x] Subtree extent computed at load: stars through the next headline at that
      level or shallower, else end of document. Actual: one right-to-left pass
      over a file's headlines with an indent stack (`Glance.Query.subtreeSpans`),
      linear whatever the nesting; `HeadlineRecord` gains `hrSubtree`, plus
      `hrDoc` and `hrDigest` — the text is the one `hrHeadline` already shares,
      so the record pins a pointer and no new array.
- [x] The view declares the action. Actual: `actions: [{"key": "RET",
      "command": "materialize", "label": "Materialize"}]`, SCHEMA.md's Action
      object; the golden fixture and the conformance assertion moved with it
      (the old one asserted `actions` absent).
- [x] `GET /headline?id=…` → `{id, file, org, digest, span}`, 404 on an unknown
      id. Actual: the id rides in the query string rather than in a path
      segment — a row id is `FILE:START` and carries both a slash and a colon,
      and WAI has decoded the query by the time the route runs. Everything is
      served from the store; nothing re-reads the file.
- [x] `POST /headline?id=…` with `{org, digest}` writes the span and answers
      the new digest. Actual: one `Edit` through `editFile` — atomic
      temp+rename, permission bits kept — and 200 `{"digest": …}`.
- [x] Drift is refused and the file survives it. Actual: two checks, both 409
      with the file byte-identical — `stale` when the client's digest is not
      the store's (the file was re-parsed since), `drift` when `editFile`
      re-digests and finds someone else's bytes. The 409 body carries the
      digest to re-materialize with; the shell shows *File changed since
      materialize — re-open* and a Re-materialize button.
- [x] The store is not written by the write path. Actual: the route touches the
      file and returns; the watch re-reads it and streams the rows, so a
      browser save and an Emacs save reach open tabs by one channel. Asserted
      in `TestServe` — with no watcher running, a second `GET` after a
      successful `POST` still answers the store's old text and old digest.
- [x] Suite: 378 → **405 tests**, hlint clean, no new warnings. `TestSubtree`
      is new (extent as text over five fixtures, the geometry group, and a
      `GLANCE_CORPUS` sweep); `TestServe` gains the two route groups;
      `TestDefaults` now owns the temp-directory helper the file-writing suites
      share.
- [x] Bindings parity and the echo widget landed on top: `Glance.Web.sharedKeys`
      is org-glance's `overview-mode` map under org-glance's own command names,
      carried to the page as one JSON blob that both the dispatch and the
      corner echo pill read; nine sequences are staged behind M4's daemon
      commands and say so, `C-c`/`C-x` are claimed as prefixes only with the
      selection collapsed, and the shell is monospace with no font fetched.
      405 → **416 tests**.
- [x] Movement is then a profile in that same blob — `emacs` (org-glance's
      `n`/`p`/`g`, the default) and `vim` (`j`/`k`/`gg`/`G`, refresh on `R`),
      picked by a pill beside the connection dot, by `?keys=`, or by what
      `localStorage` remembers, and switched in place. `C-n`/`C-p` stay
      reserved for the browser in both. The suite parses both effective maps
      and asserts no profile row shadows a shared one or swallows its own
      longer sequence. 416 → **419 tests**.

**Measured** against `glance serve --dir ~/sync --port 7799` — 6313 files,
13359 rows, 16.9 s startup — over a keep-alive loopback connection, 40 requests
each, medians:

| request | latency |
|---|---|
| `/ws` 400 — the bare HTTP round trip | **0.35 ms** |
| `/headline` 400, no id, no lookup | **0.40 ms** |
| `/headline` 404 — the whole 13359-row lookup | **2.71 ms** (min 2.18) |
| `/headline` 200, 1251-char subtree | **2.92 ms** |
| `/headline` 200, largest subtree in the corpus: 68381 chars, 74 KB of JSON | **3.84 ms** (min 3.02) |
| `/headlines` 200, 13359 rows, 3.06 MB, for scale | **119 ms** |

So a materialize is **3–4 ms** end to end, and the lookup is nearly all of it:
**~2.5 ms** of scan over 13k records, against 0.4 ms of HTTP and — on a second
run of the same benchmark — a 68 KB subtree that came back in 3.39 ms while the
404 took 3.45 ms, the slice and the encode disappearing into run-to-run noise.
The scan is deliberate (`Glance.Web.Store.storeHeadline`): the store's id index
counts ids to decide deletions and holds no records, and an index keyed to
records is a second structure to keep in step with `stFiles` on every reload,
for a saving nobody can perceive on a modal that opens. It is written down as
the lever if `/headline` ever lands in a loop.

Residency is unchanged in kind — **416 MB** RSS after the load, against the
593 MB S5 recorded (GC timing moves that number more than these fields do).
`hrDoc` names the array `hrHeadline` already held, and `hrDigest` is one shared
64-character value per file.

**The full loop**, on a scratch file inside the watched tree, timestamps from a
`ws://` client running alongside: file created → `upsert-row` ×2 at **145 ms**
(the watch's 100 ms debounce plus the parse). `GET` **13.8 ms** cold on a
just-grown store, 3–4 ms after. Edit the text, `POST` → **200** in **10.3 ms**
with the new digest, and on disk the file is byte-equal to prefix + new subtree
+ suffix, its second headline untouched, its digest the one the response
reported. The edited row reaches the socket client **109 ms** after that
response — over the watch, like any other save, carrying
`state=DONE, title="Materialize scratch, edited"`. Re-posting the same body is a
**409 `drift`** in 4.5 ms with the file untouched; re-materializing hands out
the new digest and the same body then commits **200**, which is the shell's
Re-materialize button in two calls. `rm` of the scratch file → `delete-row` ×2
at **122 ms**. Afterwards: no scratch file, no `.glance-tmp`, `~/sync`
otherwise byte-identical, `git status` showing this step's files and nothing
else.

The loop also re-exhibits S5's documented row-id churn: the scratch file's
second headline has no `ORG_GLANCE_ID`, the edit above it moved its offset from
121 to 147, and the store streamed an upsert plus a delete because
`FILE:START` cannot tell that from a deletion and an insertion. Materialize
makes this reachable from a browser rather than only from an editor; a stable
id for an unmarked headline is still S8's problem.

**Corpus.** `GLANCE_CORPUS=~/sync cabal test` samples every 98th path of 6313 —
65 files, 91 headlines — and every subtree geometry claim holds on them:
extents nest, non-nesting pairs are disjoint, each covers its own `hsFull`,
consecutive headlines leave no gap, and the last extent of every file ends at
the document's character length. 12.8 s, all of it the walk.

**Left open.** A real editor component (CodeMirror) is M3.5; this ships a
`textarea`. Whether the committed text still parses as org is the author's
business, the way it is in any editor — a failed re-parse keeps the file's rows
and streams nothing, which is S5's rule unchanged. And the route is as
privileged as every other one until S7: loopback is still the whole access
story, and a write route is the reason that stops being enough.

## S6 — M2: graph + mindmap (parallel with S5, after S2)

Wire `RefKind` in parsing, assemble `fgl` graph, `GET /graph` → graph JSON,
cytoscape page. Graph contract documented beside the table contract.

Exit:
- [ ] Golden test: fixture corpus with known refs → exact expected node and
      edge counts.
- [ ] Full-store graph: node count equals headline count; edge count recorded.
      Actual: _—_
- [ ] Mindmap renders full corpus; time-to-interactive recorded, budget set
      for regressions. Actual: _—_
- [ ] Click node → headline detail (filtered table view).
- [ ] `GRAPH-SCHEMA.md` (or section in `SCHEMA.md`) committed.

## S7 — M3: protocol + privilege tiers

`PROTOCOL.md`: handshake with version, capability registration, view stream,
commands, events — and three tiers behind auth: read, write, automate.

Exit:
- [ ] Unauthenticated connection gets read tier only; write and automate
      frames rejected — automated test.
- [ ] Polyglot proof: Python client registers a capability and receives
      file-change events — scripted test.
- [ ] Emacs attaches as a protocol client and renders the same live view
      (table-view native-live path) — parity demo.
- [ ] `PROTOCOL.md` committed; every frame in the doc exercised by at least
      one test.

## S8 — M4: write-back

Span-edit engine + commands: toggle-state, retag, reschedule, append-note,
capture. Optimistic lock (hash vs parse snapshot), atomic replace
(temp + rename).

Exit:
- [ ] Surgical property: after any command, `diff before after` = exactly one
      hunk and the hunk equals the target span — automated across corpus
      samples.
- [ ] Conflict: file mutated between parse and write → daemon rejects with a
      drift error, file untouched — automated test.
- [ ] Round-trip demo: toggle TODO in browser → Emacs `auto-revert` shows it;
      edit in Emacs → browser row updates. Both directions.
- [ ] Capture from a phone browser appends an entry to the inbox file.

**Engine landed early (S8 core).** `Data.Org.Edit`, exposed by
`glance-internal`: `applyEdits :: Text -> [Edit] -> Either EditError Text` over
half-open char spans, `takeSnapshot`/`snapshotOf`, `editFile`, `EditReceipt`.
No commands, no protocol surface, and nothing through `Glance.Query` — the
command layer decides the public shape.

- **Splice.** Validated (in bounds, `start <= end`, pairwise non-overlapping;
  touching allowed), then one left-to-right pass ordered by span start then
  end: cost is the document plus the replacements whatever the batch size, and
  the result never depends on the order the batch arrived in.
- **Optimistic lock.** A `Snapshot` is a path plus the SHA-256 of the file's
  bytes (`crypton`, already in the dep graph under warp). `editFile` re-reads
  and re-digests first; a mismatch is `Drift` with the file untouched, and a
  rejected batch writes nothing either.
- **Atomic replace.** Temp file in the target's own directory (a rename must
  not cross filesystems), `hFlush` + `fsync`, permission bits copied over,
  rename. Owner, group and timestamps do not survive — the rename installs a
  new inode. The temp name ends `.glance-tmp`, so the S5 watch ignores it.
- **Content-agnostic.** The replacement text comes from the caller; the engine
  never consults `TextShow`. `EditReceipt` carries the post-write `Snapshot`
  and text, so a caller chains edits without re-reading.

Properties under test (`test/TestEdit.hs`, 77 cases): single splice by
construction, unicode included; **the surgical property at engine level** —
replacing any headline span with a marker leaves every character ahead of its
start and past its end identical, asserted over `hsFull` and every sub-span of
every headline in the planning, drawer and unicode fixtures; multi-edit
confluence against a quadratic reference implementation over 36 seeded batches;
overlap, out-of-bounds and backwards spans as typed errors; a TODO span toggled
and re-parsed, elements equal modulo spans and that one keyword and the
re-parse's own spans still slicing back; `editFile` happy path, receipt
chaining, drift with the bytes checked untouched, no leftover temp file, mode
0600 preserved, missing and undecodable files.

Corpus canary behind `GLANCE_CORPUS=<root>` — the walk alone is 12.8 s against
0.03 s for the rest of the suite, so it stays a command of its own, the way
`glance scan` is: `GLANCE_CORPUS=~/sync cabal test` samples every 98th path of
6308, giving 33 files and 50 headlines, and all **214** spans splice exactly.
Each sampled file is digest-checked before and after, which is what proves the
run never wrote to the corpus.

Suite: 301 → **378 tests**. The exit bars above stay open: the commands, the
one-hunk diff assertion that goes with them, the round-trip demo and capture.

## S9 — Automation extension

WebExtension speaking the automate tier; scripts are deterministic data
(command sequences), reviewed before they hold the session.

Exit:
- [ ] One real workflow end-to-end: org command → script → extension acts in
      the logged-in session → result written back as an org note via S8.
- [ ] Automate tier demands its own token, distinct from read/write —
      automated test.
- [ ] Replay: same script + same page state → identical action log.

## Decision gates (time-boxed, off the critical path)

- **Encryption** — before any shared view ships (S7): elisp-`aes` compat in
  Haskell vs migrate subtrees to age/gpg. Exit: decision recorded in proposal,
  spike branch proving the chosen path on one real encrypted subtree.
- **Persistence (SQLite)** — trigger metric, checked at S5: full-store parse
  > 1 s or watch re-parse > 200 ms per event ⇒ schedule incremental index;
  otherwise `Persist.Org` stays a stub. **Checked at S5: no index scheduled.**
  The first half fired at S3 (14.6 s per full-store request over 6308 files) and
  S5 answered it with the memory store instead — one 15 s parse at startup, then
  0.09 s per request, so the cost is a startup wait rather than a page load. The
  second half did not fire: watch re-parse is **4 ms** warm and 11 ms cold per
  event over the ~/sync corpus, 20–50× inside the 200 ms budget, because an
  event re-reads one file and leaves the store alone. `Persist.Org` stays a
  stub. Residency is what would reopen this — 593 MB for 13344 rows — and the
  cheaper lever there is `hrHeadline`, ahead of SQLite.

## Dependency order

S1 → S2 → S3 → S5 → S5.5 → S7 → S8 → S9; S4 after S2; S6 after S2. S4/S5/S6 can
run in parallel. S5.5 needed only S5's store and the S8 engine, which is why it
landed ahead of the tiers it will eventually sit behind.
