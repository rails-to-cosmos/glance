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
  `evaluate` + `try`. Budget: pool width × one document, 21.9 MB at `-N1` and
  37.8 MB at `-N8` over 6290 files. `Cursor` assumes non-decreasing span starts.
- Per-file reads run on a pool of `getNumCapabilities`
  (`Data.Org.Walk.mapFilesConcurrently`, one implementation for
  `Glance.Query.loadDirFilesWith` and the scan): sound because every file parses
  from `defaultContext` and shares no state, deterministic because results
  reassemble by input index, bounded because each worker forces before
  returning. The walk itself stays serial and is most of the wall.
- Forcing alone does not bound residency: a `Text` slice shares the document's
  array, so cells are `T.copy`'d out of it (`Glance.Query.detach`). `hrHeadline`
  and `hrDoc` deliberately keep the document, which is why a loaded store still
  retains what it parsed.
- Corpus check: `cabal run -v0 glance -- scan ~/sync` — expect 0 span
  violations, ~12.9k headlines, wall ~14 s warm of which the `walk seconds` row
  is ~13. (2026-07-31: 13.4k → 12.9k when the derived mirrors left the walk; a
  semantic correction rather than a loss.)
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
  reaches it, and `isDocument`, through the facade re-exports
  `Glance.Query.derivedPath`/`documentPath` — so a file the store never loaded
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
  `.org` symlink is walked and its load fails as `ReadFailed`, counted once at
  startup and for the life of the process, since the watch is filtered by the
  same rule and no event ever revisits it. Emacs's sidecars are out of that
  rule: `isDocument` = `isOrg` minus `isSidecar` (`.#name.org`, the lock
  symlink that dangles, and `#name.org#`), one predicate for the walk and, via
  `Glance.Query.documentPath`, for `isWatchable`.
- `scan`'s argument parser recognizes `--include-derived` and treats every other
  token as a root, so `glance scan --dir X` walks a nonexistent `--dir`. `serve`
  and `desktop` reject unknown arguments; `scan` alone is permissive, and has no
  usage string.
- `dirs scanned` is the number of ROOTS given, not directories traversed.
- One row per id. Two files claiming an `ORG_GLANCE_ID` are resolved by
  `Glance.Query.resolveIds` — a `.org-glance/data/` path wins, else walk order —
  and the losers are counted, in `X-Glance-Id-Collisions` and in the scan
  report. It has exactly four call sites: `loadDir`'s `summarise`,
  `Store.storeRecords`, `Store.storeResult` and `Store.resolvedRows`, so the
  store equals the load it stands in for and the stream equals both.

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
- Streamed frames are id-resolved like every other answer: `applyFile` and
  `dropFile` wrap their store update in `streamed`, which diffs the touched
  ids' RESOLVED rows before and after (`resolvedRows` = `resolveIds` over the
  rows carrying them). Editing the loser of a shared id streams nothing, editing
  the winner streams the winner, and a winner that goes away re-points the id at
  the row behind it. Costs one pass over the store's rows per side, per event.
- Two headlines inside ONE file sharing an id keep the FIRST on both sides — a
  file does not outrank itself, so `resolveIds` leaves the incumbent, and the
  stream goes through that same call. `stTags` never sees the duplicate (the
  per-file projection is a `Set`). There is no index by id: `stIds` is gone with
  the delete rule that read it.
- `stGen` starts at 0 every process and is not persisted; what survives a
  restart is `stPrint`, a digest over each file's path and load-time digest
  taken once in `loadStoreWith`. The `ETag` is `"<fingerprint>-g<gen>"`:
  identical tree → identical fingerprint → an honest 304; a byte, a name or a
  root moved → a different tag whatever the generation says. The fingerprint is
  not recomputed per edit — the generation already says how far the tree moved.
- The `X-Glance-*` stats and page headers ride on the 200 alone. A 304 carries
  the `ETag` and `Cache-Control` and nothing else, so a client that reads counts
  off the headers must not read them off a revalidation.
- The watch is a per-path trailing-edge debounce of 100 ms on a monotonic clock
  in seconds, drained by a 25 ms poll loop. There is no ceiling and no
  leading edge: a path taking events faster than every 100 ms is deferred for
  as long as that lasts.
- Deletion is decided by `doesFileExist` at reload time, not by the event kind.
- `stTags` counts FILES, not rows: it is stepped by the set difference between
  a file's old and new projection, so a tag on forty rows of one file counts
  once.
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
  A client whose bounded 1024-frame mailbox fills loses its backlog and its
  registration — the watcher never waits on a browser — and the close is named
  `resync`, since one `/headlines` carries everything the backlog would have.
  The size is counted in frames and `publish` coalesces within a step, so what
  overruns it is a BURST of steps: an editor writing a directory is one step per
  file and nothing coalesces across them.
- The public library exposes `Glance.Query` alone over the private
  `glance-internal` sublibrary; cells are sliced from spans and the view
  `Value` is hand-built — no `ToJSON` on an internal type
  (table-view/SCHEMA.md is the contract).
- Commands: one route, `POST /command {name, id | ids, args, digests?}`, two
  names — `set-state {keyword: KW | null}` and `archive {}`. Ids group by FILE
  and each file is one drift-locked `replaceSpans` call, so a marked set over
  three files is three atomic writes; there is no cross-file rollback and the
  answer is per id (`{results: [{id, ok, digest | error}]}`, in the order the
  ids were named). Request-shape refusals are 400 with nothing written — a bad
  body, an unimplemented name, no ids, and a keyword ANY named row's file does
  not declare, which refuses the whole request rather than moving the rows whose
  files do. Per id: an unknown id, and a client digest the store no longer holds
  (per file, since a digest is). 413 outranks everything. The route never writes
  the store — the watch is still the sole updater.
- The span math is `Glance.Query`'s, because `HeadlineSpans` is
  `glance-internal`'s: `setStateEdits` replaces the keyword span, inserts
  `" KW"` at `spanEnd hsStars` when there is none, or deletes the keyword plus
  the HORIZONTAL run behind it (so a keyword ending its line keeps the newline);
  `archiveEdits` inserts `ARCHIVE:` at `spanEnd hsTags`, else `" :ARCHIVE:"` at
  the end of the TITLE LINE — the max end of stars/todo/priority/title, since
  `hsFull` ends at a planning timestamp or a drawer on a later line. Keyword
  legality is per file (`hrKeywords`); `*active*`/`*inactive*` are in no keyword
  set and are refused like any other word. An already-archived row costs no
  edit, which is what makes `archive` idempotent.
- `/headlines` hides archived rows unless the query names the `archive` key
  (`Glance.Web.Filter.namesArchive`, any spelling — negated, valued, whatever),
  and `X-Glance-Archived` counts what it took. The predicate is exactly
  `-archive:`. The vocabulary a query is parsed against stays the WHOLE store's
  (`storeTags`), so the exclusion can never hide the key that reaches what it
  hid. The socket is NOT filtered: it carries row ops whatever the client's
  query, so an unfiltered client splices in an archived row `/headlines` would
  not have served — the shell's default query makes it refetch instead.
- Materialize: `GET`/`POST /headline?id=…` serves and replaces a headline's raw
  subtree. The digest is pinned at load, any divergence is a 409 with the file
  untouched, and the write path never WRITES the store — it reads it for the
  extent and the digest, and the file watch is the only thing that updates rows.
  A byte-identical commit still rewrites the file (temp + rename, no equality
  short-circuit), so it costs an inotify event and a re-parse; `guarded` then
  finds nothing moved and the generation stays put.
- The subtree lens — ONE OWNER PER BYTE, over THREE regions. `GET /headline`
  serves the subtree twice, `org` whole and `body` + `properties` + `planning` +
  `logbook` split (`Glance.Query.headlineParts`); `POST` takes back
  `{org, digest}` or `{body, properties, planning, digest}`
  (`recomposedSubtree`), naming both shapes is a 400 and a `body` owes both lists
  beside it. The regions are the planning LINE, the headline's OWN property
  drawer and its OWN logbook drawer; every other byte is the body's — a child's
  drawer is body text. Every cut is by whole lines, the closing newline included.
  Decompose → recompose is byte-identical, which is the property the design rests
  on; the round trip covers permuted planning lines, a logbook, and a child's
  logbook that must stay body text.
- Two of the four parts are SERVER-PRESERVED and a client neither sees nor sends
  them: the keys in `Glance.Query.hiddenProperties` (`ORG_GLANCE_ID` — the row id
  a rename would break) and the whole logbook. `headlineParts` drops them,
  `recomposedSubtree` re-injects their original lines verbatim whatever the
  client said, and extending the list is one edit. A hidden property therefore
  survives a panel sync that never mentioned it, and an empty `properties` list
  empties the client's half of the drawer alone.
- Properties: an untouched pair goes back as the LINE it arrived on, verbatim;
  only an edited or added one renders `:KEY: value`, under the drawer's own
  indentation; a dropped one is not written and an empty list removes the drawer
  when nothing hidden is in it. Raw lines are consumed one per pair, so one pair
  spelled twice keeps both. Hidden lines are woven back at the INDEX they sat at.
  Pairs are read by splitting lines, never through the parser's `Properties` —
  that uppercases keys and re-tokenises values.
- Planning: entries arrive as `(KEYWORD, timestamp text)` in LINE order. An entry
  nobody changed goes back as the very text it was, where it was; anything else
  renders `KEYWORD: value` and joins behind them in `SCHEDULED`/`DEADLINE`/
  `CLOSED` order. An empty list drops the line. Every value is checked by
  REPARSE (`readsAsTimestamp`, which probes the line the write would produce) and
  a refusal is a 409 naming the field, which the sheet lands on as `error`.
- Region line indices are the BODY's, not the subtree's: each region's subtree
  line less the lines every region ahead of it took out. Subtree indices leave a
  gap where a region was cleared — a drawer whose planning line just came off
  lands a line late. `spliceRegions` counts only body lines consumed, so two
  regions naming one line land in list order (planning, properties, logbook),
  which is what a headline growing a planning line and a drawer in one commit
  needs. A region the headline never had goes on the line under the title.
- `/headlines` carries `ETag: "<stPrint16>-g<stGen>"` under
  `Cache-Control: no-cache`; the generation moves only in `Store.guarded`, and
  only when frames were produced or a file's load outcome moved, and the
  fingerprint is fixed at load. One tag covers every query variant: the
  parameters are in the URL and an HTTP cache is keyed by URL, so the response
  is a function of (tree, generation, URL) and no `Vary` is owed for them — gzip
  writes the `Accept-Encoding` one itself.
- The HTTP surface is a fixed route table, each entry declaring whether it needs
  a loaded store and whether it is read-only. GET/HEAD are the whole of it
  except `POST /headline` and `POST /command`; anything else is 405 — JSON on
  those two, plain text elsewhere. An upgrade aimed at any path but `/ws` is
  rejected.
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
  case-insensitive plus the `*active*`/`*inactive*` meta values (`starless`
  strips one matched asterisk pair before those two comparisons and nowhere
  else, so `state:active` is an alias and `state:*TODO*` is a literal that
  matches nothing), `priority` is exact
  equality, `scheduled`/`deadline` are prefix, everything else is substring.
  `key:none` is the empty cell on the COLUMN keys only — `tag:none` is untagged,
  since `tag` is a column — and has no branch for a virtual key, where
  `contact:none` means tagged `contact` AND the row text containing `none`.
  `key:` narrows nothing. The virtual keys are the store's org tags
  (`storeTags`, kept per tag beside the rows): `TAG:text` is tagged whole-TAG and
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
  - Arity is chosen by NAME here (`tagsColumn` = the index of `tag`) and
    DECLARED to the renderer: the `tag` column emits `"multi": true`, which
    beats its sampling (`multiColumn` over ≤40 non-empty cells, needing ≥2
    tag-shaped and none contrary — fewer than two tagged rows loaded, or one
    cell holding a stray colon, and it found no multi-valued column at all).
    An asset predating the field still samples.
  - Date-ness is likewise asymmetric: two hardcoded names here, sampled
    date-shape there. A page with under two dated rows makes the renderer
    substring-match `scheduled:` where the server prefix-matches it.
  - `state:*active*`/`state:*inactive*` are producer-only, blessed by
    SCHEMA.md, and are the canonical spelling (org-glance's own, and what the
    default view boots on). The renderer has no group logic and matches them as
    literal badge text; the `state` column now ships them as `values` beside
    its `badges`, so its autocomplete can at least offer them.
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
  opens its socket with `?bootstrap=off`. With no `q` in the URL the boot query
  is `state:*active*` — the default view, applied as a real query: written into
  the URL through `remember`, mounted as `initialQuery`, and asked of the server,
  so `DEL` strips it like any other token. A `q` that IS in the URL is the
  reader's intent, an empty one included, and nothing is injected over it. Rows are virtualized and shown a page
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
  filter commit. A late paint is guarded by the query it was asked for. A boot
  that was filtered — a `?q=` link or the default — chains one more fetch,
  `arm(total)`: the unfiltered set, kept as the parity baseline without being
  painted, bounded to once per page. `TestServe`'s "Shell boot" runs the glue
  under node and asserts that fetch sequence.
- With a filter applied, a socket frame does not splice — it schedules a
  refetch 250 ms out, coalescing a burst into one request. Unfiltered frames
  splice straight into the renderer.
- Shell z-indexes are four: echo `2`, corner `3`, modal backdrop `100`, sheet
  `101`. The value palette shares the pair with the sheet (`#modal,#prompt` and
  `#pbox`), so the four values stand whatever else is added. The cross-repo constraint is the backdrop pair clearing the renderer's
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
- The log strip is append-only and its whole interface is
  `append(scope, severity, message)`. A line is `HH:MM:SS SEV scope message` —
  severity `info`/`warn`/`error`, coloured and worn as the line's class; scope
  one of `ws`, `sync`, `cmd`, `filter`, `config`, `boot`; control characters in
  the message collapse to spaces. Nothing clears it, the boot line included; the
  ring holds 500 and drops the OLDEST; a line identical to the one before it
  bumps a `×N` counter instead of appending, which is the only mutation. The end
  is scrolled to unless the reader has scrolled up. Every write names its rows —
  `headline "TITLE" marked for deletion` / `unmarked for deletion` / `archived` /
  `→ KEYWORD` / `state cleared`, one line per ROW — with the title read through
  the renderer's `displayText` and the id as the fallback; refusals stay one
  `cmd error` line.
- Every touch-device rule lives in ONE `@media (pointer:coarse)` block — the
  chip row as a 44px tap target, its empty-state label, and the sheet's 16px
  textarea that stops iOS zooming in.
- A client whose mailbox fills is closed with the reason `resync`; a column
  change closes with `view-changed`. Those two strings are the whole vocabulary
  of a server-initiated close, and the client answers them differently. Only
  `view-changed` remounts. Everything else revalidates `/headlines` for the
  applied query against the tag the last answer carried (`If-None-Match`,
  `cache: no-store` so the 304 is this page's and not the browser cache's),
  re-attaches, and keeps the mount — sheet, palette, selection, URL. 304 means
  the rows on screen still stand; 200 replaces them in place. A 200 also
  compares the fetched columns to the mounted ones (whole, by `JSON.stringify`,
  since the badge palette rides inside them) and remounts when they differ: a
  daemon restarted while the page was away had no socket to send `view-changed`
  down. Across a real remount the shell stashes and restores a dirty sheet
  (`{id, text, digest}`) and the palette's typed text, re-reading the sheet's
  digest with a `GET` so a file that moved lands at `conflict` rather than being
  overwritten. One door for both closes is what a user reported as "a periodic
  page refresh resetting filters and popups".
- The parity tripwire is loose in one direction. It only fires when the server
  returns zero, so the opposite skew is never reported; the local recount drops
  the key and tests the value against the whole row text, so a correct empty
  facet answer warns whenever the word appears elsewhere; and it consults column
  keys alone, so every virtual key is treated as suspect. It reports a suspicion
  and corrects nothing. Its baseline is a remembered unfiltered paint, and a
  boot that had none — a `?q=` link, or the default view — arms it with `arm`'s
  own unfiltered fetch and re-runs the check the boot could not.
- The shell's keymap is `Glance.Web`'s `keyBindings` and nothing else — ONE map,
  no profiles: the page carries it as a JSON blob (`{rows, hints, reserved,
  once}`) and its own dispatch parses that blob. Each row carries `kbKeys`,
  `kbCommand`, `kbScope` (`table`, `modal` or `any` — where it is live) and an
  optional `kbHelp`; the dispatch filters on the scope and the echo widget reads
  the help. `seq` is derived in the blob (`T.unwords . kbKeys`), never stored.
  Movement carries BOTH spellings — `n`/`p` and `j`/`k` step a row, `f`/`b` and
  `l`/`h` step a cell — which costs a row each where a profile cost a selector, a
  stored choice, a URL parameter and a key line that had to be rewritten. Ends
  are `<` and `>`, plus vi's `G` beside `>`. `g` is `apply-default-filter`, `,`
  is `customize`, `o` and `!` are the open stub, `M` is `mark-all`, `d` is
  `archive-flag` and `D` is `org-glance-overview:delete` (both over FLAGS, never
  marks). No sequence is bound
  twice or opens a longer one. Sequences and command names are org-glance's where
  org-glance has one; a row with no handler is recognized and says what will
  back it.
  `RESERVED` = `C-l`, `C-r`, `C-t`, `C-w`, `C-n`, `C-p`, `<f5>`: a reserved key
  reaches the browser UNLESS it completes a bound sequence. What the list
  actually buys is the abandoned prefix — without it a dead-end chord would be
  swallowed as undefined. That rule is the PAGE's half and is all a page has:
  Chromium handles `Ctrl+T`, `Ctrl+N` and `Ctrl+W` above the document, so
  `preventDefault` on the completing chord does not reach them and `C-c C-t` is
  dead in the browser however correctly it is dispatched (`TestServe`, "the
  completing chord is claimed, reserved or not", pins the half that IS ours).
  `C-x C-s` works because `Ctrl+S` is a page default action rather than a
  browser one. Prefix opening is guarded by `selecting()`, one
  predicate over the focused field's range and the document selection, and it
  covers every prefix rather than `C-c`/`C-x` alone, so vim's `g` obeys it too.
  Auto-repeat is movement's — a held `n` crosses the table — so the keys that
  must run once per press are named by COMMAND in `ONCE` (`filter-drop-token`,
  `unmark-all`, `mark-all`, `archive-flag`, `org-glance-overview:delete`), which
  holds under both spellings of a command. `archive-flag` needs it most: a repeat
  that survived would flag a row and archive it from ONE press, which is the
  confirmation the two-press shape exists to be.
- Three keys write without a sheet, all `POST /command`, and WHICH ROWS is per
  command rather than one rule. `t`/`C-c C-t` (`set-state`) take the MARKED set
  when there is one and the row at point otherwise — dired's rule, and the
  generic bulk selection. `D` and `d` take the FLAGGED set instead and never read
  marks: a mark is what a reader lays down to set a state over a run of rows, and
  letting the archive key inherit one makes every mark a loaded gun. Both sets
  are the renderer's and are asked for AT command time; no set is kept here.
- `d` is dired's FLAG and dired's `dd`, in two presses: the first flags the row
  at point (`archive-flag`, echo `d → flagged — d again archives`) and a second
  `d` on an already-flagged row IS `D` — it calls the same handler, so it
  archives EVERY flagged row rather than the one under it. A lone flag is a set
  of one, which is what leaves the single-row flow unchanged. There is no
  sequence machinery: `d` stays one complete binding. `D`
  (`org-glance-overview:delete`) is that handler without the flagging press —
  every flagged row when there is one, the row at point otherwise, echoing
  `D → archived (4 flagged)` or `D → archived (row)` and giving that name up for
  the bare count when nothing landed, since a set name over zero rows reads as a
  write that worked. `D` SPENDS the flags it fired over, the way the second `d`
  spends its one: `setRows` keeps a flag whose row a filter is hiding — which is
  what makes a flag outlive the refetch the write causes — so a set left standing
  would be archived again by the next press and the row at point would never be
  reachable again. The flag is the confirmation, so there is no prompt; `u` on a
  flagged row takes the flag off before it touches a mark (`u → flag cleared`)
  and `U` clears flags with marks.
  Flags are the RENDERER's session state, keyed by id like marks —
  `flagRow`/`unflagRow`/`getFlagged`/`clearFlags` and the `.tv-flagged` wash,
  landed in table-view at 079fa20. The pair is still feature-detected: an asset
  predating it echoes `this table-view.js has no archive flags` and writes
  nothing, and `D` there falls through to the row at point.
- `t`/`C-c C-t` raise a value palette of the shell's OWN — the state column's
  `badges` plus `*clear*`, never its `values` (`*active*` is not a keyword) —
  typed to narrow, `C-n`/`C-p` and the arrows to walk, `RET` to commit, `ESC`
  through the keymap's `cancel`. Its keys live in a SECOND document listener
  behind the dispatch, which is safe because `typing()` has already killed every
  `table` row. Confirm-free: the drift lock is the safety. The pill counts what
  landed, the log names every row it landed on and every one refused, and the
  rows arrive over the watch.
- Row marks are the RENDERER's, behind `marks: true`: it draws the checkbox
  column, keys the marks by id and counts them, so a mark survives a `setRows`,
  a filter that hides its row and a page it is not on, and this page keeps no
  set of its own — not the count, not a membership test. dired's: `m` toggles and
  takes the renderer's word for where it landed, `u` toggles and puts back
  anything it just laid down (so it can only ever clear), both then
  `selectStep(+1)`, `U` clears, and `M` is `markAll()` — the renderer's call
  because the SET is, so a page it is not showing is marked too. `m`/`u` stay out
  of `ONCE` because the walk IS the feature. Feature-detected on `toggleMark` and
  on `markAll`, so an asset predating either echoes rather than throws.
- The mount passes `actionHints: false`: the renderer's per-row hint said RET
  materializes, which the resident key line already says and says for every
  command. One place.
- STARRED METAS. The `*word*` form marks a RESERVED META with semantics of its
  own — never a literal keyword and never a cell value. The family:
  `*active*`/`*inactive*` (filter group metas, producer-evaluated) and `*clear*`
  (the state palette's take-the-keyword-off entry, committed as a null keyword).
  A future meta joins by wearing the stars. The enforcing edge is
  `setStateEdits`, which refuses any word a file's `#+TODO:` does not declare, and
  `keywordTextP` (letters and underscores) makes a starred word undeclarable, so
  the two walls meet.
- Browser writes are commands over the bridge: structured ones (toggle, retag,
  reschedule) and drift-locked raw replacement (materialize a subtree, later a
  file). Semantic org editing — refile, agenda logic — stays out of the browser.
  Automation = reviewed deterministic scripts, no LLM in the loop.

## UI

- Keyboard-first: every web-surface feature ships with a key path mirroring
  the Emacs org-glance maps; buttons only where keys cannot reach; the echo
  widget must know every new binding (keymap-is-data blob is the single
  source).
- The materialize sheet is buttonless and syncs itself. Dirty = either pane vs
  the materialized original, moved by each successful flush; ESC or the backdrop
  flushes a dirty sheet and closes on the 200, a pristine one closes with no
  request; `C-x C-s` flushes mid-edit and chains the receipt's digest; a 409
  keeps it open at `conflict`, where `C-x C-s` re-reads the digest and
  overwrites and ESC discards; `beforeunload` flushes with `keepalive` only when
  dirty. Header states: `synced` / `syncing…` / `conflict` / `error` — the last
  two are the ones that wait for a keystroke, so each spells the key that
  clears it.
- The sheet is two panes over one subtree and the cut is the SERVER's: textarea
  = `body`, panel = `properties`, a flush posts both back. The page holds no org
  parser and must not grow one. A panel row is key then value in file order (no
  `tabindex` anywhere); the trailing empty row is the add affordance and grows
  the next on commit; an emptied key deletes; `ORG_GLANCE_ID` is shown with a
  line saying the row id is that value. `C-c '` (org's `org-edit-special`) swaps
  two-pane and raw org by RE-MATERIALIZING — a dirty sheet is refused with `sync
  first — C-x C-s`, since a local conversion would need the parser this keeps
  out, and the re-read lands at `synced`. Stash and restore carry both panes and
  the shape. The sheet is four fifths of the window each way (`min(80vw,100%)` ×
  `min(80vh,100%)`); the panes wrap rather than querying a width, and the
  `pointer:coarse` block pins the column.
- The property panel is MODAL, and its keys are a SECOND document listener
  behind the dispatch, like the value palette's. NAV: rows are read-only text
  (spans, nothing focusable), one wears the cursor (`pcur`, class `pat`, drawn
  only under `#mprops.on`), and movement is `n`/`p`, `j`/`k` and the arrows —
  both profiles' letters bound unconditionally, since a row with no field in it
  leaves every printable key free. Entering the panel BLURS the textarea and
  sets `pnav`, which `typing()` counts as a focus of its own; without that the
  table's own letters would move rows under the sheet. EDIT: `RET` opens the row
  at point (`pedit`), value focused first, key first where there is no key yet;
  `+` adds an empty property at the end and opens it — the add affordance is a
  KEY, where a row that is always empty was chrome every reader of the panel had
  to filter back out; `TAB` hops the row's two fields and the pane crossing is
  suspended; `RET` commits — the row takes its fields' text; `ESC` cancels
  through the keymap's `cancel`, restoring what the row holds. A row HOLDS its
  committed text and `props()` reads that, so an open edit is not dirty and only
  a commit is. `TAB`/`S-TAB` is one two-stop toggle between the panes and the
  cursor survives it; `shut` clears `pnav`/`pedit`. `preventDefault` fires only
  where one of those bindings does, and only over an open subtree sheet: raw
  mode has one pane so `TAB` is the browser's, and the settings sheet keeps
  native tabbing. The three planning rows are FIXED rows at the head of this same
  list — `SCHEDULED`, `DEADLINE`, `CLOSED` in org's order, key uncooked and
  unopenable, value the timestamp text verbatim, empty meaning absent — so
  clearing all three is how the planning line comes off. The logbook is a
  read-only strip under both panes: full width, muted, out of Tab and out of
  `dirty()`, showing the drawer's INTERIOR lines alone (the widget being the
  drawer says what it is), and never sent — the server re-splices the whole
  drawer, delimiters included.
- The whole page wears danneskjold, through one `--g-*` palette (surface, text,
  muted, border, selection, warn, bad) declared once and re-declared per theme.
  The sheet keeps exactly one variable of its own, `--dk-mono` (Hack first);
  everything else it uses is the page's.
- The applied filter query is in the URL (`replaceState`, `keys` preserved) and
  applied from it on load. `DEL` over the table drops the query's last token
  through the renderer (`stripLastToken`/`getQuery`) — the chips are the
  renderer's, so the strip is too. `remember` writes `q` unconditionally, so an
  emptied query leaves `?q=` present-and-empty: that is what `bootQuery` reads
  as intent and leaves alone, where an ABSENT `q` gets the default injected.
  Deleting the parameter instead made a cleared filter come back filtered on the
  next remount.
- One status corner, top right, in this order: the connection dot (`live` /
  `wait` / `down`) then `themesel`, a native `<select>` over
  `auto`/`light`/`dark`. A focused `SELECT` counts as typing, so its own arrows
  reach it. The keys picker is gone with the profiles.
- Theme: `auto` follows `prefers-color-scheme` and is the default; `light` and
  `dark` stamp `data-theme` on the document element, and returning to `auto`
  removes the attribute. The choice lives in `localStorage` under
  `glance-theme`, and `themeBoot` — one unindented line in `<head>`, so the
  suite's glue extractor cannot mistake it for the shell's inline block — reads
  it and stamps the attribute before the first paint. Without that line a dark
  page flashes light.

## Keyword config

- Recognition unions system + tag configs + file pragmas (superset — a
  keyword declared anywhere parses everywhere); classification is
  nearest-scope: file > tags (first wins) > system > built-in.
  Config lives at `<root>/.org-glance/config/{system.org,tags/*.org}`,
  is never a row source, and a config change reseeds and reloads the
  world (debounced, view-changed follows).
- `clSeed` is stored, not derived: `clTags` keeps the FIRST config of each tag
  across directories while the seed unions every entry read, shadowed ones
  included.
- The DEFAULT VIEW is `system.org`'s `#+GLANCE_DEFAULT_FILTER:` line, read into
  `clFilter` and answered by `defaultFilter`; absent means `builtinFilter` =
  `state:*active*`, a line naming nothing means the empty query, and the LAST
  line wins. The system layer alone, and the first config directory that names
  one — a default view belongs to a tree rather than to a tag. The daemon embeds
  it into the served page as `DEFAULT_QUERY` (off the store, per request), the
  bare-boot injection and `g` both read it, and the settings sheet edits it as
  one field beside the system layer's cycle: `POST /config` takes an optional
  `filter` and splices it in the SAME call as the `#+TODO:` block, since they are
  lines of one file and two calls would be two writes under two digests.
- `GET`/`POST /config` serve and replace ONE layer's `#+TODO:` block through the
  ordinary write path — `configEdits` for the spans, `replaceSpans` for the
  drift-locked atomic write — so a `#+TITLE:`, a comment and the capture
  template come back byte for byte. The route never writes the store; the watch
  reseeds. `GET` reads the files (the digest handed out is the lock), and its
  layer list IS the POST allowlist and the read the edits are measured in.
  Which directories comes off `clDirs`, falling back to `configDirIn` of the
  served root. An EMPTY digest is the pin for a file that is not there:
  `Data.Org.Edit` treats it as the empty document, makes the directories and
  creates; a file that turned up meanwhile drifts. Refusals: a non-`#+TODO:`
  line, a block declaring no keyword (which is also what refuses `*active*` —
  a keyword token is letters and `_`, so the group names cannot parse into one),
  an unknown path, a bad body — all 400; 409 on drift; 413 past 1 MiB. An empty
  `lines` deletes the layer's cycle.
- KNOWN GAP: creating the FIRST `.org-glance/config` in a tree that had none
  races fsnotify's watch-arming, so that one write does not reseed until a
  restart or a later config edit. The watch's property, not the route's.
- Settings sheet = `,` (`customize`), the materialize sheet's pattern over
  `/config`: buttonless, ESC/backdrop syncs the layers that moved and closes,
  pristine closes with no request, `C-x C-s` syncs mid-edit, `conflict` waits
  for a keystroke. One box per layer holding its `#+TODO:` lines VERBATIM — the
  page has no org parser and must not grow one. `#config`/`#cbox` share the
  existing z band with `#modal`/`#sheet` and `#prompt`/`#pbox`, so the four
  values still stand. A coarse pointer gets a gear in the corner, hidden by the
  one `pointer:coarse` block. The sheet is a sibling of `#app`, so the
  `view-changed` its own write causes leaves it standing.

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
