# glance — invariants

Rules the code silently enforces. Violating one is a bug even when the suite
stays green. Fuller version with evidence: [docs/invariants.md](docs/invariants.md).

## Spans

- Spans are half-open CHAR offsets `[start, end)` into the `orgParse` input
  `Text` — never bytes, never line/column.
- Headline sub-spans are tight: each slices to exactly its component, no
  surrounding whitespace. Element spans are only well-formed + reparseable.
- `hsFull` runs from the stars to the max end of present components and never
  covers trailing whitespace. Sub-spans nest inside it, ordered
  todo < priority < title < tags < planning < properties, non-overlapping; a
  drawer, when present, ends exactly at `hsFull`'s end.
- The three planning spans permute freely on their line, so `headlineSpanParts`
  and the `hsFull` fold sort them by `spanStart`. Each covers the timestamp
  text alone — the keyword is not part of it.
- `stripSpans` must cover every span-carrying constructor; a new `Element`
  constructor that embeds spans must extend it.
- A subtree span runs from a headline's stars to the next headline at its level
  or shallower, else to the end of the document; they nest, non-nesting pairs
  are disjoint, and trailing blank lines belong to the subtree above.

## Parser

- A top-level element must end at whitespace or EOF; a sub-parser stopping
  mid-word fails the WHOLE file (the known 13-file corpus failure class).
- Headlines parse only at column 1, via the threaded begin-of-line Bool.
  Never `getSourcePos` — quadratic on failure-heavy input.
- TODO keywords are matched case-sensitively and stored verbatim;
  pragma/property KEYS are uppercased.
- In `spannedContainerUntil` the end-parser branch precedes the hspace-eol
  branch (tags open with `hspace1` and lose it otherwise).
- Trailing hspace terminates a container and stays unconsumed.
- The property parser rejects reserved `PROPERTIES`/`END` — that guard is what
  terminates the drawer.
- Timestamp range halves share one bracket kind; `tsmHasTime` alone decides
  whether a time renders; the weekday is recomputed from the date.
- A range is spelled `<a>--<b>` or compactly as `<date wd 10:30-11:30>`;
  `tsCompactRange` preserves which, and the renderer never canonicalizes one
  into the other (CLOCK lines are always `--`). A `-` before a time opens a
  range end, before a unit it is a negative repeater.
- The planning line is the one line after the title line, before any drawer:
  `SCHEDULED:`/`DEADLINE:`/`CLOSED:` uppercase-only, any order, last-wins per
  keyword. `CLOCK:` is not one. The whole line backtracks when it is not a
  planning line, and a `SCHEDULED:` further down the body stays body elements.
- `orgParse` on error returns zero elements AND the caller's context untouched.
- Context keyword sets are append-only; a `#+TODO:` affects only headlines
  below it; no Context merge operation exists — `defaultContext` seeds
  TODO/DONE.

## Render

- `TextShow` is a lossy REPL re-serializer (whitespace collapse, uppercased
  pragma keys, Set-ordered keyword lists). Never use it for write-back or the
  wire contract; spans are the only lossless channel. TestRoundtrip's
  exact-vs-stable split IS the documented lossiness budget.

## Scan

- Every accumulator is forced at each step; `forceResult` runs inside
  `evaluate` + `try`. Budget: ~19 MB max residency over 6305 files. `Cursor`
  assumes non-decreasing span starts.
- Corpus check: `cabal run -v0 glance -- scan ~/sync` — expect 0 span
  violations, ~12.9k headlines, wall ~14 s warm. (2026-07-31: 13.4k → 12.9k
  when the derived mirrors left the walk; a semantic correction, not a loss.)

## Walk

- Org files are the source of truth, so org-glance's derived mirrors are not
  walked: inside a `.org-glance` directory, `overviews` and `meta` are skipped
  and `data` — the canonical store — is kept. One rule (`Data.Org.Walk.isDerived`)
  serves the walk and the watch, so a file the store never loaded cannot arrive
  by inotify. `--include-derived` turns it off on `serve`, `desktop` and `scan`,
  and the scan reports what it skipped.
- One row per id. Two files claiming an `ORG_GLANCE_ID` are resolved by
  `Glance.Query.resolveIds` — a `.org-glance/data/` path wins, else walk order —
  and the losers are counted, in `X-Glance-Id-Collisions` and in the scan
  report. `loadDir` and `storeResult` call it, so the store still equals the
  load it stands in for.

## Architecture (docs/proposal-org-console-web.md, docs/plan-org-console-web.md)

- Org files are the single source of truth; no second authoritative store.
- Write-back (S8) = surgical span replacement, optimistic lock, atomic
  temp+rename; untouched bytes stay byte-identical.
- Write-back engine = `Data.Org.Edit`: char-span splice, drift-checked, atomic
  same-dir rename, content-agnostic (no `TextShow`).
- `Display`/`TextShow` stay out of the wire contract; the web layer is the
  private sublibrary `glance-web` (`src-web/`, `Glance.Web*`) with the public
  library alone in its `build-depends`, and it binds 127.0.0.1 until S7 brings
  privilege tiers.
- The served store is an in-memory projection keyed by path, so `Map.elems` is
  walk order and `/headlines` equals a fresh `loadDir`. The watch re-parses one
  file per event from `defaultContext`; a failed load keeps that file's rows and
  streams nothing.
- The server binds before it walks: the store starts `Loading`, the walk runs on
  its own thread, and the watch starts after `finishLoading`. Until then
  `/headlines`, `/headline` and `/ws` answer 503 + `Retry-After: 1` +
  `{"loading": true, "elapsed": S}` (the WS upgrade is refused, never accepted
  onto an empty store) while `/` and the assets serve, so the shell renders the
  indexing state and polls out of it.
- `glance desktop` = the same daemon with an app-mode window opened as soon as
  the socket listens, ahead of the loaded store. Browser order: `$GLANCE_BROWSER`,
  `--browser`, then chromium/chromium-browser/google-chrome-stable/google-chrome/brave/vivaldi
  on PATH, run as `CMD --app=URL`; then `xdg-open URL`; then the URL printed. No
  window failure ever fails the daemon. `--dry-run` prints the resolved command
  and exits before binding.
- The socket carries SCHEMA.md's row ops alone. A column change (the TODO
  keyword union moving) closes it with reason `view-changed` and the client
  re-fetches. The bootstrap `set-rows` is snapshotted inside the subscribing
  transaction, so there is no journal and no gap; `?bootstrap=off` drops that
  frame for a client that already fetched the rows, and trades the gap for it.
  A client whose bounded mailbox fills is dropped — the watcher never waits on a
  browser.
- The public library exposes `Glance.Query` alone over the private
  `glance-internal` sublibrary; cells are sliced from spans and the view
  `Value` is hand-built — no `ToJSON` on an internal type
  (table-view/SCHEMA.md is the contract).
- Materialize: `GET`/`POST /headline?id=…` serves and replaces a headline's raw
  subtree. The digest is pinned at load, any divergence is a 409 with the file
  untouched, and the write path never touches the store — the file watch is the
  only thing that updates rows.
- `/headlines` carries `ETag: "g<stGen>"` under `Cache-Control: no-cache`; the
  generation moves only in `Store.guarded`, and only when frames were produced
  or a file's load outcome moved. One tag covers every query variant: the
  parameters are in the URL and an HTTP cache is keyed by URL, so the response
  is a function of (generation, URL) and no `Vary` is owed for them — gzip
  writes the `Accept-Encoding` one itself.
- `?q=` matches `hrSearch`, a load-time mirror of `table-view.js`'s
  `displayText` (link → DESC, control-char runs → one space) lowercased and
  `\x1f`-joined, so server and renderer answer a query alike. Filter runs before
  page; a page slices `sortedForView`, never walk order; no `limit` means the
  whole set in walk order for the client to sort. The palette stays the store's
  whatever the page holds, and the shell re-asks the server for a row frame that
  lands while a filter is on.
- `?q=` is SCHEMA.md's filter query, parsed in `Glance.Web.Filter` as a port of
  `table-view.js`'s `scanQuery`/`parseQuery`/`tokenTest` — parity is the
  contract. Tokens split on whitespace and `&`; `key:value` (`=` alias) is a
  predicate only for a column key or a producer virtual key, so `:work:` and
  `=code=` stay text; a token opening with `"` is free text; `-` negates.
  Same-key predicates combine by field arity — single-valued OR (`state:`),
  multi-valued AND (the `tag` column and every virtual tag key) — and
  everything else ANDs. Per type: badge whole-value case-insensitive plus
  `state:active`/`inactive`; text substring; dates prefix; `key:none` is the
  empty cell on every type; `key:` narrows nothing. The virtual keys are the
  store's org tags (`storeTags`, kept per tag beside `stIds`): `TAG:text` is
  tagged whole-TAG and matching text, empty text being presence; a column
  shadows a tag of its name. The tags column's key is `tag`, singular (header
  stays `Tags`). A predicate reads one `\x1f`
  field of `hrSearch`, so per-cell matching and free text agree by construction.
- The served pages fetch nothing off this server: inline styles, inline glue,
  and one `<script src>` naming a file under `--assets`. No CDN, no web font, no
  analytics. The JetBrains Mono `@font-face` appears only when the assets
  directory holds the file, pointing at a bare name this server serves.
- The shell is vanilla inline JS with no framework, build step or dependency,
  and shrinking it beats adding to it. It boots on `?limit=1000`, pulls the rest
  in behind the painted table, mounts with `onFilter` so the server narrows, and
  opens its socket with `?bootstrap=off`. Rows are virtualized, so movement is
  ids out of `getVisible()` handed to `select(id, col)` — the DOM-walking path
  is gone, as are the frame branches `bootstrap=off` makes unreachable. The
  column is the renderer's selection, never a second copy here: row movement
  passes back whatever `getSelection()` reports, so it survives a profile
  switch and clears when the selection does. The applied `?q=` is restored the
  same way — handed to `mount` as `initialQuery`, with the box-stuffing path
  kept only as the fallback for an asset that drops the option.
- The shell's keymap is `Glance.Web`'s `sharedKeys` + `keyProfiles` and nothing
  else: the page carries them as a JSON blob and its own dispatch parses that
  blob. Movement is the only thing a profile changes (`emacs` default, `vim`);
  the effective map is `shared ++ profile`, and within one no sequence is bound
  twice or opens a longer one. Sequences and command names are org-glance's; a
  row with no handler is recognized and says what will back it. `C-c`/`C-x` are
  prefixes only with the selection collapsed; `C-l`, `C-r`, `C-t`, `C-w`, `C-n`,
  `C-p` and `<f5>` are never claimed, so no profile moves on `C-n`/`C-p`.
  Auto-repeat is movement's — a held `n` crosses the table — so the keys that
  must run once per press are named by command in `ONCE`, beside `RESERVED`.
- Browser writes are commands over the bridge: structured ones (toggle, retag,
  reschedule) and drift-locked raw replacement (materialize a subtree, later a
  file). Semantic org editing — refile, agenda logic — stays out of the browser.
  Automation = reviewed deterministic scripts, no LLM in the loop.

## UI

- Keyboard-first: every web-surface feature ships with a key path mirroring
  the Emacs org-glance maps; buttons only where keys cannot reach; the echo
  widget must know every new binding (keymap-is-data blob is the single
  source).
- The materialize sheet is buttonless and syncs itself. Dirty = textarea vs the
  materialized original, moved by each successful flush; ESC or the backdrop
  flushes a dirty sheet and closes on the 200, a pristine one closes with no
  request; `C-x C-s` flushes mid-edit and chains the receipt's digest; a 409
  keeps it open at `conflict`, where `C-x C-s` re-reads the digest and
  overwrites and ESC discards; `beforeunload` flushes with `keepalive` only when
  dirty. Header states: `synced` / `syncing…` / `conflict`. The sheet alone
  wears the author's Emacs theme (danneskjold, `--dk-*` vars, Hack first).
- The applied filter query is in the URL (`replaceState`, `keys` preserved) and
  applied from it on load. `DEL` over the table drops the query's last token
  through the renderer (`stripLastToken`/`getQuery`) — the chips are the
  renderer's, so the strip is too.
- One status corner, top right: the connection dot and a native `<select>` of
  the keymap blob's movement profiles. A focused `SELECT` counts as typing, so
  its own arrows reach it.

## Build

- `glance.cabal` is hand-maintained; package.yaml/hpack removed — do not
  regenerate.
- Components: private sublibrary `glance-internal` (`src/`), public library
  `glance` (`src-query/`, `Glance.Query` only), private sublibrary
  `glance-web` (`src-web/`) on the public library alone, one CLI dispatching
  to both sublibraries, one suite naming all three. A new web or daemon target
  depends on the public library alone.
