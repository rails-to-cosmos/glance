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
number and keeps the flat design.

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
- [ ] Edit an org file in any editor → open browser tab updates the row with
      no reload. Watch-to-render latency ≤ 1 s. Actual: _—_
- [ ] Daemon restart: browser re-attaches and re-syncs the full view
      unattended.
- [ ] Create + delete file cases handled (row appears / disappears).

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
  otherwise `Persist.Org` stays a stub. **First half already fired** (S3:
  14.6 s per full-store request over 6308 files); the decision waits for S5's
  re-parse number, since an incremental watch that never re-reads the store is
  a different index from one that does.

## Dependency order

S1 → S2 → S3 → S5 → S7 → S8 → S9; S4 after S2; S6 after S2. S4/S5/S6 can run
in parallel.
