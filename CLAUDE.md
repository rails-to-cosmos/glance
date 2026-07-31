# glance — invariants

Rules the code silently enforces. Violating one is a bug even when the suite
stays green. Fuller version with evidence: [docs/invariants.md](docs/invariants.md).

## Spans

- Spans are half-open CHAR offsets `[start, end)` into the `orgParse` input
  `Text` — never bytes, never line/column.
- Headline sub-spans are tight, and tightness is per component: todo, priority
  and tags slice to the exact string, the title to it up to word normalization,
  each planning span to a bracketed timestamp that reparses, properties to a
  `:PROPERTIES:`…`:END:` block once stripped. Element spans are only
  well-formed + reparseable.
- `hsFull` is derived, never stored: a left fold of `<>` over `spanParts`,
  seeded with the `hsStars` field. Its start is therefore always the stars and
  its end is the LAST present part in that order — source order, never a
  maximum over ends. It never covers trailing whitespace. Sub-spans nest inside
  it, ordered todo < priority < title < tags < planning < properties,
  non-overlapping; a drawer, when present, ends exactly at `hsFull`'s end.
- The three planning spans permute freely on their line, so `spanParts` sorts
  that triple by `spanStart` and leaves the other five positional; `hsFull` and
  `headlineSpanParts` both read that one ordering. Each planning span covers the
  timestamp text alone — the keyword is not part of it.
- `stripSpans` must cover every span-carrying constructor; a new `Element`
  constructor that embeds spans must extend it. The suite reads elements
  through `bare = map (stripSpans . valueOf)`, so the ~150 span-insensitive
  assertions go span-sensitive the moment it stops being total.
- A subtree span runs from a headline's stars to the next headline at its level
  or shallower, else to the end of the document; they nest, non-nesting pairs
  are disjoint, and trailing blank lines belong to the subtree above.

## Parser

- A top-level element must end at whitespace or EOF; a sub-parser stopping
  mid-word fails the WHOLE file — the residual corpus failure class, 11 files
  of 6290 at 2026-07-31. The per-cause breakdown was counted at 13 files and
  has not been re-measured since the derived mirrors left the walk; treat it as
  needing a re-run before it is quoted.
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
  The backtrack is `try` around each entry, and it must roll back the leading
  hspace the entry skipped: the top loop requires whitespace BETWEEN elements,
  so eating it here fails the whole document.
- `spanRange` forces at every step (`foldl'` + `$!`); a thunk chain there would
  outlive the document it points into.
- `compactly` guards the compact range render with three conditions — the flag,
  both ends timed, one day. Only the flag is exercised; nothing renders a
  hand-built timestamp that sets the flag with a missing time or two dates, so
  the defensive half is untested.
- `orgParse` on error returns zero elements AND the caller's context untouched.
- Context keyword sets are append-only; a `#+TODO:` affects only headlines
  below it; no Context merge operation exists — `defaultContext` seeds
  TODO/DONE.
- IAS registration is last-writer-wins by plain `Map.insert`. `resolveHeadline`
  is reached from the suite alone; no library or daemon path consults it.

## Render

- `TextShow` is a lossy REPL re-serializer (whitespace collapse, uppercased
  pragma keys, Set-ordered keyword lists). Never use it for write-back or the
  wire contract; spans are the only lossless channel. TestRoundtrip's
  exact-vs-stable split IS the documented lossiness budget: 22 `Exact` rows and
  1 `Stable`, the `#+TODO:` set ordering. The seven rows promoted on 2026-07-31
  were measured to re-render byte for byte; the label had outrun the renderer.

## Scan

- Every accumulator is forced at each step; `forceResult` runs inside
  `evaluate` + `try`. Budget: ~19 MB max residency over 6290 files. `Cursor`
  assumes non-decreasing span starts.
- Forcing alone does not bound residency: a `Text` slice shares the document's
  array, so cells are `T.copy`'d out of it (`Glance.Query.detach`). `hrHeadline`
  and `hrDoc` deliberately keep the document, which is why a loaded store still
  retains what it parsed.
- Corpus check: `cabal run -v0 glance -- scan ~/sync` — expect 0 span
  violations, ~12.9k headlines, wall ~14 s warm. (2026-07-31: 13.4k → 12.9k
  when the derived mirrors left the walk; a semantic correction rather than a
  loss.)
- The `GLANCE_CORPUS` groups still PASS when the variable is unset, and say so:
  `TestDefaults.withCorpusSample` prints `SKIPPED — GLANCE_CORPUS is unset` on
  stderr for each. A green run without those two lines answered is unverified on
  the corpus half. A variable naming a directory that is not there fails loudly,
  and so does a run that samples nothing.

## Walk

- Org files are the source of truth, so org-glance's derived mirrors are not
  walked. The rule is a DENYLIST of names sitting directly under a
  `.org-glance` component — `overviews` and `meta`, with the whole subtree of
  either excluded. `data` is not privileged in the walk; it survives by not
  being on the list, and is privileged only in `beatsForId`
  (`Data.Org.Walk`), which is a different rule for a different question.
  One `Data.Org.Walk.isDerived` serves the walk and the watch — the watch
  reaches it, and `isOrg`, through the facade re-exports
  `Glance.Query.derivedPath`/`orgPath` — so a file the store never loaded
  cannot arrive by inotify. `--include-derived` turns it off on `serve`,
  `desktop` and `scan`.
- The exclusion is textual, over the path the walk builds from the root as
  typed. Point `--dir` inside a `.org-glance` tree (or `cd` into it and pass
  `.`) and no component matches, so the mirrors are walked. Nothing
  canonicalizes the root.
- The scan's `derived skipped` counts DIRECTORIES, not files: `keepDerived`
  runs only where a directory is declined. A skipped file is dropped with no
  record, which is reachable only in the cd-inside case above — where the run
  reports nothing at all.
- Symlinked directories are never followed, and the symlink probe failing is
  treated the same way: `pure acc`, no error kept, nothing counted. An
  unlistable directory IS reported; a symlinked one vanishes silently.
- A non-directory is kept on name alone — no existence check — so a dangling
  `.org` symlink is walked and its load fails as `ReadFailed`. The watch
  refuses it (`isWatchable` rejects the `.#` prefix, and the predicate is the
  fsnotify filter itself), so no event ever revisits it: the failure is counted
  once at startup and stays counted for the life of the process.
- `scan`'s argument parser recognizes `--include-derived` and treats every other
  token as a root, so `glance scan --dir X` walks a nonexistent `--dir`. `serve`
  and `desktop` reject unknown arguments; `scan` alone is permissive, and has no
  usage string.
- `dirs scanned` is the number of ROOTS given, not directories traversed.
- One row per id. Two files claiming an `ORG_GLANCE_ID` are resolved by
  `Glance.Query.resolveIds` — a `.org-glance/data/` path wins, else walk order —
  and the losers are counted, in `X-Glance-Id-Collisions` and in the scan
  report. It has exactly three call sites: `loadDir`'s `summarise`,
  `Store.storeRecords` and `Store.storeResult`, so the store still equals the
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
- **KNOWN BUG (open): streamed rows are unresolved.** `putFile` builds its
  `upsert-row`/`delete-row` frames straight off one file's records, never
  through `resolveIds`. Every HTTP answer and the bootstrap `set-rows` ARE
  resolved. So a client watching a tree with duplicate ids is shown the losing
  file's row live and the winning file's row on refresh; and when the losing
  file drops the id while the winner still holds it, no `delete-row` is emitted
  at all and the stale row sits there until reconnect.
- Two headlines inside ONE file sharing an id diverge by direction: the stream
  keeps the LAST (`Map.fromList`), the served view keeps the FIRST
  (`beatsForId` cannot separate two rows of the same path, so the incumbent
  stays). `stIds`/`stTags` never see the duplicate — the per-file projections
  are `Set`s.
- **KNOWN GAP (open): `stGen` starts at 0 every process and is not persisted.**
  `loadStoreWith` seeds through `putFile` and `finishLoading` bypasses
  `guarded`, so a fully loaded store still serves `ETag: "g0"`. A client holding
  `"g0"` from before a restart gets a 304 for a tree that has changed under it.
- The `X-Glance-*` stats and page headers ride on the 200 alone. A 304 carries
  the `ETag` and `Cache-Control` and nothing else, so a client that reads counts
  off the headers must not read them off a revalidation.
- The watch is a per-path trailing-edge debounce of 100 ms on a monotonic clock
  in seconds, drained by a 25 ms poll loop. There is no ceiling and no
  leading edge: a path taking events faster than every 100 ms is deferred for
  as long as that lasts.
- Deletion is decided by `doesFileExist` at reload time, not by the event kind.
- `stIds` and `stTags` count FILES, not rows: each is stepped by the set
  difference between a file's old and new projection, so a tag on forty rows of
  one file counts once.
- `stDirErrs` is frozen at startup — written by `loadStoreWith`, read by
  `storeResult`, and touched by nothing in the watch. A directory that becomes
  readable, or stops being, is invisible until restart.
- `storeKeywords` merges ONE record per file (`listToMaybe . feRecords`), which
  is sound because every row of a file shares the file's keyword sets.
- The server binds before it walks: the store starts `Loading`, the walk runs on
  its own thread, and the watch starts after `finishLoading` — the second and
  last writer of the store TVar, installing it in one transaction so no request
  sees the new store still described as loading. Until then `/headlines`,
  `/headline` and `/ws` answer 503 + `Retry-After: 1` while `/` and the assets
  serve, so the shell renders the indexing state and polls out of it. The two
  503 bodies differ: the HTTP one is `{"loading":true,"elapsed":S}`, S being
  seconds rounded to a tenth; the WS upgrade is rejected with the shorter
  `{"loading":true}` and never accepted onto an empty store. The load gate runs
  ahead of the method check, so `/ws` answers 503 to any method while loading,
  and 400/405 once loaded.
- `glance desktop` = the same daemon with an app-mode window opened as soon as
  the socket listens, ahead of the loaded store. Browser order: `$GLANCE_BROWSER`,
  `--browser`, then chromium/chromium-browser/google-chrome-stable/google-chrome/brave/vivaldi
  on PATH, run as `CMD --app=URL`; then `xdg-open URL`; then the URL printed. No
  window failure ever fails the daemon. `--dry-run` prints the resolved command
  and exits before binding.
- The socket carries SCHEMA.md's row ops alone. A column change (the TODO
  keyword union moving) closes it with reason `view-changed` and the client
  re-fetches. `ViewChanged` is a `Frame` like the row ops, and `frameJSON` gives
  it `Nothing` — it travels as a close rather than a message — and `guarded`
  REPLACES the step's frames with it rather than appending, so a column change
  never also ships rows describing the palette that just moved.
  The bootstrap `set-rows` is snapshotted inside the subscribing
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
  untouched, and the write path never WRITES the store — it reads it for the
  extent and the digest, and the file watch is the only thing that updates rows.
  A byte-identical commit still rewrites the file (temp + rename, no equality
  short-circuit), so it costs an inotify event and a re-parse; `guarded` then
  finds nothing moved and the generation stays put.
- `/headlines` carries `ETag: "g<stGen>"` under `Cache-Control: no-cache`; the
  generation moves only in `Store.guarded`, and only when frames were produced
  or a file's load outcome moved. One tag covers every query variant: the
  parameters are in the URL and an HTTP cache is keyed by URL, so the response
  is a function of (generation, URL) and no `Vary` is owed for them — gzip
  writes the `Accept-Encoding` one itself.
- The HTTP surface is a fixed route table, each entry declaring whether it needs
  a loaded store and whether it is read-only. GET/HEAD are the whole of it
  except `POST /headline`; anything else is 405 — JSON on `/headline`, plain
  text elsewhere. An upgrade aimed at any path but `/ws` is rejected.
- `POST /headline` caps the body at 1 MiB and answers 413 past it. The cap is
  checked before the id lookup, so 413 outranks 404.
- `?limit=` is capped at 20000 and a larger one is a 400; no `limit` serves the
  whole store, which is the mode the shell settles into.
- The asset route takes ONE path segment through `safeName`, which rejects the
  empty name, `.`, `..` and any name carrying `/` or `\`.
- `Content-Length` is written by `sized` on every JSON, HTML and plain response,
  the HTTP 503 included; warp supplies it for the 304 and for `responseFile`.
  The gzip middleware's `Vary: Accept-Encoding` rides every HTTP response, 304s
  included, and NOT the websocket rejection — that path is outside it.
- `?q=` matches `hrSearch`, a load-time mirror of `table-view.js`'s
  `displayText`, lowercased and `\x1f`-joined, so server and renderer answer a
  query alike. The link rule in full: `[[T][D]]` shows `D`, `[[T]]` and
  `[[T][]]` both show `T`, and text that never closes a link is left exactly as
  written. Runs of control characters collapse to one space. Filter runs before
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
  everything else ANDs. `Glance.Web.Filter` dispatches on the KEY NAME, never on
  the column's declared `kind` — it does not import it: `state` is whole-value
  case-insensitive plus the `active`/`inactive` meta values, `priority` is exact
  equality, `scheduled`/`deadline` are prefix, everything else is substring.
  `key:none` is the empty cell on the COLUMN keys only — `tag:none` is untagged,
  since `tag` is a column — and has no branch for a virtual key, where
  `contact:none` means tagged `contact` AND the row text containing `none`.
  `key:` narrows nothing. The virtual keys are the store's org tags
  (`storeTags`, kept per tag beside `stIds`): `TAG:text` is tagged whole-TAG and
  matching text, empty text being presence; a column shadows a tag of its name.
  The tags column's key is `tag`, singular (header stays `Tags`). A predicate
  reads one `\x1f` field of `hrSearch`, so per-cell matching and free text agree
  by construction.
- Parity discipline: there is NO schema revision mechanism between this producer
  and `table-view.js`. Agreement rests on the port being kept term for term,
  plus one loose runtime tripwire. Known divergences, all live:
  - Column lockstep is three-way through `viewColumns` (`columns`, `rowJSON`
    cells, `filterKeys`); `hrSearch`'s field order is a hand-written positional
    list in `recordOf` and is NOT derived from it. `TestFilter`'s layout guard
    compares `hrSearch` against its own hardcoded list, so it catches
    `recordOf` drifting from the test, not from `viewColumns`. Reorder
    `viewColumns` alone and every predicate reads the wrong field, green.
  - Arity is chosen by NAME here (`tagsColumn` = the index of `tag`) and by
    SAMPLED SHAPE there (`multiColumn` over ≤40 non-empty cells, needing ≥2
    tag-shaped and none contrary). Fewer than two tagged rows loaded, or one
    cell holding a stray colon, and the renderer finds no multi-valued column
    at all: its tag vocabulary empties and `tag:a tag:b` ORs where the server
    always ANDs.
  - Date-ness is likewise asymmetric: two hardcoded names here, sampled
    date-shape there. A page with under two dated rows makes the renderer
    substring-match `scheduled:` where the server prefix-matches it.
  - `state:active`/`state:inactive` are producer-only, blessed by SCHEMA.md.
    The renderer has no such logic and matches them as literal badge text; the
    `state` column ships `badges` rather than `values`, so they are not
    discoverable by its autocomplete either.
  - Vocabulary scopes differ: the server's virtual keys are the whole store's
    tags, the renderer's are derived from the rows it currently holds. A tag
    outside the loaded page is a predicate on one side and free text on the
    other.
  - Keys are matched case-sensitively on BOTH sides and every real key is
    lowercase, so `Tag:x` is free text either way. Values are folded on both.
  - Separators are exactly `&`, space, tab and newline. `\r` is not one, nor is
    any other Unicode space.
  - A bare `-` is a negated empty free-text term, and an empty term matches
    everything, so a lone hyphen empties the result set. Both sides agree.
  - `key:value` splits on the FIRST `:` or `=`, so `tag:a=b` is key `tag`,
    value `a=b`; a body opening with a separator has no key, which is what
    leaves `:work:` and `=code=` as text.
- The served pages fetch nothing off this server: inline styles, inline glue,
  and one `<script src>` naming a file under `--assets`. No CDN, no web font, no
  analytics. The JetBrains Mono `@font-face` appears only when the assets
  directory holds the file, pointing at a bare name this server serves.
- The shell is vanilla inline JS with no framework, build step or dependency,
  and shrinking it beats adding to it. It boots on `?limit=100`, pulls the rest
  in behind the painted table, mounts with `onFilter` so the server narrows, and
  opens its socket with `?bootstrap=off`. Rows are virtualized and shown a page
  at a time (`pageSize` = the boot's `limit`, so the first paint is page one),
  so a row step is `selectStep(±1)` — the page boundary is the renderer's,
  since only it knows there is one — and `[`/`]` turn a page, echoing where
  they landed. `getVisible()` is that page, so the buffer-ends keys reach its
  ends; ids out of it handed to `select(id, col)` are all that is left of the
  DOM-walking path, which is gone, as are the frame branches `bootstrap=off`
  makes unreachable. The column is the renderer's selection, never a second
  copy here: `selectStep` carries it, and what the shell passes back is
  whatever `getSelection()` reports, so it survives a profile switch and clears
  when the selection does. The applied `?q=` is restored the
  same way — handed to `mount` as `initialQuery`, with the box-stuffing path
  kept only as the fallback for an asset that drops the option.
- One fetch is in flight at a time: a single `AbortController`, aborted and
  replaced by whoever asks next, so the background full-set pull yields to a
  filter commit. A late paint is guarded by the query it was asked for.
- With a filter applied, a socket frame does not splice — it schedules a
  refetch 250 ms out, coalescing a burst into one request. Unfiltered frames
  splice straight into the renderer.
- Shell z-indexes are four: echo `2`, corner `3`, modal backdrop `100`, sheet
  `101`. The cross-repo constraint is the backdrop pair clearing the renderer's
  sticky header (`1`) and completion list (`5`); the corner and the echo sit
  below both on purpose, so they dim under the backdrop. The filter palette
  carries no shell z-index at all — the overlay is entirely the renderer's, and
  the suite forbids this page naming its parts.
- `--g-border` is a hand-copied LITERAL of the renderer's `--tv-border`
  (`#E3E6EA` / `#2A2D3D`), not a live `var()` read, so a renderer border change
  needs a matching edit here. `--g-sel` is likewise literal, and danneskjold's
  rather than the renderer's.
- Renderer internals this page may touch are enumerated by the suite as
  must-not-appear lists rather than by a comment here: the shell may not name
  `closeFilter`, `tv-veil`, `tv-panel`, may not reach rows by `tr.click()`,
  `scrollIntoView` or `rowEls(`, and may not keep a column of its own
  (`selCol`, `lastColumn`, a `col` local). What it does style is `.tv-root`'s
  font, `.tv-chips`/`.tv-chip` under a coarse pointer, and the selected-row
  fallback read.
- Every optional renderer capability is feature-detected before use —
  `parseQuery`, `stripLastToken` with `getQuery`, `selectStep`, `nextPage` with
  `pageInfo`, `getSelection`, `openFilter`, plus `matchMedia`. `initialQuery` is
  passed unguarded and detected afterwards by asking `getQuery()` whether it
  took.
- The page never scrolls: `body` is `100vh`, `overflow:hidden`, a flex column of
  table, log and key line. The table has a fixed share, the log takes what is
  left and scrolls inside itself, the key line is `flex:none` and scrolls
  sideways. A long message therefore moves nothing.
- Every touch-device rule lives in ONE `@media (pointer:coarse)` block — the
  chip row as a 44px tap target, its empty-state label, and the sheet's 16px
  textarea that stops iOS zooming in.
- A client whose mailbox fills is closed with the reason `dropped-slow-client`;
  a column change closes with `view-changed`. Those two strings are the whole
  vocabulary of a server-initiated close.
- The parity tripwire is loose in one direction and arms late. It only fires
  when the server returns zero, so the opposite skew is never reported; it arms
  only against a remembered unfiltered paint, so a `?q=` boot never arms it at
  all for that session; the local recount drops the key and tests the value
  against the whole row text, so a correct empty facet answer warns whenever the
  word appears elsewhere; and it consults column keys alone, so every virtual
  key is treated as suspect. It reports a suspicion and corrects nothing.
- The shell's keymap is `Glance.Web`'s `sharedKeys` + `keyProfiles` and nothing
  else: the page carries them as a JSON blob and its own dispatch parses that
  blob. Each row carries `kbKeys`, `kbCommand`, `kbScope` (`table`, `modal` or
  `any` — where it is live) and an optional `kbHelp` for what the command name
  does not say; the dispatch filters on the scope and the echo widget reads the
  help. Movement is the only thing a profile changes (`emacs` default, `vim`);
  the effective map is `shared ++ profile`, and within one no sequence is bound
  twice or opens a longer one. Sequences and command names are org-glance's; a
  row with no handler is recognized and says what will back it.
  `RESERVED` = `C-l`, `C-r`, `C-t`, `C-w`, `C-n`, `C-p`, `<f5>`: a reserved key
  reaches the browser UNLESS it completes a bound sequence, which is what keeps
  `C-c C-t` working while `C-x C-l` still opens a new window. What the list
  actually buys is the abandoned prefix — without it a dead-end chord would be
  swallowed as undefined. Prefix opening is guarded by `selecting()`, one
  predicate over the focused field's range and the document selection, and it
  covers every prefix rather than `C-c`/`C-x` alone, so vim's `g` obeys it too.
  Auto-repeat is movement's — a held `n` crosses the table — so the keys that
  must run once per press are named by COMMAND in `ONCE` (currently
  `filter-drop-token`), which holds under any profile that binds them.
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
  dirty. Header states: `synced` / `syncing…` / `conflict` / `error` — the last
  two are the ones that wait for a keystroke, so each spells the key that
  clears it.
- The whole page wears danneskjold, through one `--g-*` palette (surface, text,
  muted, border, selection, warn, bad) declared once and re-declared per theme.
  The sheet keeps exactly one variable of its own, `--dk-mono` (Hack first);
  everything else it uses is the page's.
- The applied filter query is in the URL (`replaceState`, `keys` preserved) and
  applied from it on load. `DEL` over the table drops the query's last token
  through the renderer (`stripLastToken`/`getQuery`) — the chips are the
  renderer's, so the strip is too.
- One status corner, top right, in this order: the connection dot (`live` /
  `wait` / `down`), `themesel`, `keysel`. Both are native `<select>`s — one over
  `auto`/`light`/`dark`, one over the keymap blob's movement profiles — and a
  focused `SELECT` counts as typing, so their own arrows reach them.
- Theme: `auto` follows `prefers-color-scheme` and is the default; `light` and
  `dark` stamp `data-theme` on the document element, and returning to `auto`
  removes the attribute. The choice lives in `localStorage` under
  `glance-theme`, and `themeBoot` — one unindented line in `<head>`, so the
  suite's glue extractor cannot mistake it for the shell's inline block — reads
  it and stamps the attribute before the first paint. Without that line a dark
  page flashes light.

## Build

- `glance.cabal` is hand-maintained; package.yaml/hpack removed — do not
  regenerate.
- Components: private sublibrary `glance-internal` (`src/`), public library
  `glance` (`src-query/`, `Glance.Query` only), private sublibrary
  `glance-web` (`src-web/`) on the public library alone, one CLI dispatching
  to both sublibraries, one suite naming all three. A new web or daemon target
  depends on the public library alone.
- `glance-web` exposes five modules and has no `other-modules`:
  `Glance.Desktop`, `Glance.Web`, `Glance.Web.Filter`, `Glance.Web.Store`,
  `Glance.Web.Watch`.
