# Invariants

Rules the repo enforces, with evidence and failure modes. Compact index:
[../CLAUDE.md](../CLAUDE.md). Confidence: **test** = a test fails if violated ·
**corpus** = only `glance scan` catches it · **comment** = stated in a haddock,
unguarded · **docs** = stated in docs/, unguarded · **none** = silently relied
on.

## Spans

- **Half-open char offsets.** `Span` indexes the exact `Text` given to
  `orgParse`, in characters (`sliceSpan` = `T.take`/`T.drop`). A byte-offset
  consumer splices mid-codepoint on the first unicode title. Evidence:
  `Types.hs` `Span`/`sliceSpan` haddocks; unicode canary in `TestSpans`
  ("Привет мир": 24 chars, 41 bytes). **test + corpus**
- **Sub-spans tight, element spans loose.** Each headline sub-span slices to
  exactly its component (`getOffset` taken before trailing-space consumption:
  `indentP`, `priorityP`, `todoP`, `tagsP`, `propertiesP`); element spans may
  carry consumed trailing whitespace and are only bounds-checked +
  reparse-checked. S8 write-back replaces sub-spans verbatim — one blank in a
  span breaks the one-hunk exit bar. Evidence: `headlineSpanParts` (single
  source for scan + tests). **test + corpus**
- **`hsFull` geometry.** Starts at the stars, ends at the max end of present
  components (`foldr1 (<>)` over source-ordered spans — ordering guaranteed),
  never trailing whitespace. Capture/refile insertion points derive from it.
  **test** (`TestSpans` trailing-whitespace group)
- **Sub-span order.** todo < priority < title < tags < planning < properties.
  The three planning spans permute freely — org writes `SCHEDULED:`,
  `DEADLINE:` and `CLOSED:` in any order on the line — so both
  `headlineSpanParts` and the `hsFull` fold sort them by `spanStart`. Drop the
  sort and `hsFull` ends at whichever entry the record lists last, leaving the
  others outside it. **test** (`TestSpans` "planning keywords out of order")
- **Drawer placement.** When present, `hsProperties` starts past the newline
  after the headline line, after `hsTitle`, and ends exactly at `hsFull`'s
  end. An append-note command writing at `spanEnd hsFull` writes inside the
  drawer if this breaks. **test**
- **`stripSpans` totality.** Resets headline spans; every other constructor
  passes through. A new span-carrying `Element` constructor silently turns
  ~150 span-insensitive assertions span-sensitive unless added here. **test**
- **Subtree extents.** `Glance.Query.hrSubtree` runs from `spanStart (hsFull …)`
  to the start of the next headline in the same file at the headline's own
  level or shallower, and to the end of the document when there is none —
  org's outline rule, computed at load in one right-to-left pass over the
  headlines with a stack. The geometry the write path rests on: extents nest
  (a child's lies inside its parent's), two that do not nest are disjoint, each
  covers its own `hsFull`, consecutive headlines leave no gap, and the last
  extent of a file ends at `T.length doc`. Two consequences worth stating
  because a materialize shows them: whatever sits between a subtree's last body
  line and the next headline's stars, blank lines included, belongs to the
  subtree above; and a file's `#+`-preamble sits ahead of the first extent and
  belongs to no subtree, so a commit cannot carry it off. Evidence:
  `TestSubtree` — five fixtures asserted as text, the geometry group over all
  of them, and the same geometry over sampled real files behind
  `GLANCE_CORPUS`. **test + corpus**

## Parser

- **Whitespace-or-EOF element boundary.** Token = maximal non-space run; the
  top loop separates by whitespace and requires EOF. A sub-parser stopping on
  a non-space char fails the whole document — the entire residual corpus
  failure class (13/6305 files: `::` in titles, `:)`, timestamp glued to
  punctuation, hyphen in commented `#+TODO:`). Any recovery mechanism
  (`withRecovery`) changes `orgParse`'s all-or-nothing contract and every
  caller. **test + docs**
- **Column-1 headlines, O(1) begin-of-line.** `elementsP` threads a Bool
  computed from the consumed separator (`startsLine`); the headline branch is
  absent from `choice` when not at line start. `getSourcePos` re-scans from
  the last checkpoint per call — quadratic on failure-heavy documents; the
  13.6 s / 464 files/s / 19 MB baseline dies with zero test failures.
  Mid-line `*bold*` as fake headline and `  * Task` as headline are the
  behaviors this excludes (deliberate divergence: indented stars are not
  headlines). **test** (behavior) / **docs** (perf rationale)
- **Verbatim case-sensitive TODO keywords.** `todoP`/`#+TODO:` registration
  use `keywordTextP` (as written); `Parse Keyword` uppercases and serves
  pragma dispatch (`CATEGORY`/`TODO`), the `reserved` guard, and
  `ORG_GLANCE_ID` lookup. Swapping the two breaks either keyword matching or
  drawer termination. **test**
- **Drawer termination = reserved-keyword rejection.** `Parse Property`
  rejects `PROPERTIES`/`END`; without the guard `:END:` parses as a property,
  `manyTill` runs to EOF, and `hsProperties` swallows the rest of the file.
  **none** — no test feeds `:END:` as a property line.
- **Container stop order.** In `spannedContainerUntil` the end-parser (tags)
  branch precedes the hspace-eol branch; tags open with `hspace1`, and the
  eol branch would consume that space without backtracking. **comment + test**
  (indirect)
- **Trailing hspace unconsumed.** Containers terminate on it via
  `lookAhead (try …)`; spans stay tight, values gain no blank tokens.
  Trailing spaces belong to no element (inter-element gap) — span consumers
  must not assume elements tile the input. One exception: the `#+TODO:`
  pragma's element span swallows trailing spaces (`sepEndBy`), reparse-safe.
  **test**
- **Timestamps.** Range halves share one bracket kind; an end-half repeater
  parses and is discarded (start's wins). `tsmHasTime` is the only record of
  whether the source spelled a time — midnight-with-time and date-only both
  store 00:00. Weekday is parsed, discarded, recomputed on render (wrong
  source weekdays re-render corrected — a spurious hunk if rendered rather
  than span-spliced). **test** (has-time, brackets) / **comment** (weekday)
- **Two range spellings, source form preserved.** `<a>--<b>` and the compact
  same-day `<date wd 10:30-11:30>` both land in `tsStart`/`tsEnd`;
  `tsCompactRange` records which the source wrote and the renderer branches on
  it. Canonicalizing either into the other is a spurious hunk — emacs writes
  CLOCK ranges as `--` even though both halves share a date, which is what the
  `Exact` clock-range roundtrip row pins. The compact branch additionally
  requires both ends to carry a time on one day, so a hand-built timestamp
  cannot render its end date away. `Eq` includes the flag; `Ord` still compares
  start moments only. **test**
- **Range end versus negative repeater.** Both open with `-` and only the
  time's colon separates them. The end time is tried first: `-1d` gets through
  `MPL.decimal` and fails at the missing `:`, backtracking whole and leaving
  the repeater its text. No space may sit around the `-`, or ` -1d` would read
  as an end time instead. **test**
- **Planning line.** The one line right after a headline's title line, ahead of
  any drawer, fills `schedule`, `deadline` and `closed`. Keywords match
  uppercase-only, in any order, and a keyword repeated on the line keeps its
  last timestamp, the way org reads one. `CLOCK:` is not one of them. The whole
  line backtracks when it is not a planning line, so a body line parses exactly
  as it did before and a `SCHEDULED:` further down the body stays a Token +
  Timestamp pair. Each span covers the timestamp text alone, keyword excluded:
  S8 reschedules by replacing that slice and nothing else. Corpus: 4661 planning
  lines in parseable files carry 7220 entries, 7161 of which attach. The ~70
  stragglers are stamps the timestamp parser still rejects — two-letter weekday
  abbreviations, unit-less repeaters (`10:00+2`), a repeater written before the
  time, diary sexps, and a repeater followed by a warning period — and since
  the entry loop stops at the first failure, later entries on that line are
  stranded with it. A further 2642 planning lines sit inside the 13 files that
  fail to parse outright. **test + corpus**
- **All-or-nothing parse.** On error `orgParse` returns zero elements and the
  caller's context unchanged — scan buckets, the REPL, and pragma
  half-application all rely on it. **test**
- **Context discipline.** Keyword sets only grow (`setTodo` unions);
  `#+TODO:` affects only headlines below it (single left-to-right pass); a
  context carried across `orgParse` calls keeps earlier keywords. There is no
  Context merge — the old `Semigroup`/`Monoid` were unlawful (`mempty`
  re-seeded TODO/DONE, `<>` concatenated categories) and were removed;
  `defaultContext` is the seed. S5's file-watch must parse each file from
  `defaultContext`, not a shared long-lived context. **test** (persistence
  half) / **none** (no-retroactive half)
- **IAS registration.** Keyed on the `ORG_GLANCE_ID` property, opt-in,
  last-writer-wins — re-parsing a file is idempotent. **test**

## Render

- **`TextShow` is lossy by design.** Whitespace collapses to single spaces,
  pragma keys uppercase, `#+TODO:` sets re-emit in Set (alphabetical) order.
  `TestRoundtrip`'s `Fidelity` column (11 `Exact` rows, 8 `Stable`) is the
  documented budget: promoting a `Stable` case to `Exact` asserts fidelity the
  renderer lacks. Write-back and the future wire contract must never route
  through it — spans are the lossless channel. **test**
- **`Ord Timestamp` ≠ `Eq Timestamp`.** Ord compares start moments only;
  Set/Map keys would deduplicate distinct timestamps sharing a start. **none**
- **`resolveHeadline` last-wins.** Keeps h1 only when both scheduled and h1
  strictly later; everything else yields h2. **test**
- **Planning stays out of the render.** `TextShow Headline` emits the title
  line only, so a headline carrying `schedule`/`deadline`/`closed` re-renders
  without its planning line. Round-tripping a planning line through `showt`
  loses it; the span is the only channel that keeps it. **none**

## Scan

- **Strictness discipline.** Every accumulator forced per step (`$!`, strict
  fields, `seq`); `forceResult` inside `evaluate` + `try` so one pathological
  file cannot abort the run and no thunk retains a document. History: the
  first walk version retained 1.4 GB; budget is ~19 MB max residency.
  Invisible to `cabal test` — only `glance scan ~/sync` exposes regressions.
  **comment + docs**
- **Cursor linearity.** Left-to-right slicing assumes non-decreasing span
  starts; out-of-order visits silently degrade to O(start) per slice.
  **comment**

## Architecture (constrains this code; from docs/)

- Org files are the single source of truth; the daemon keeps no second
  authoritative store. Persistence returns only on the plan's trigger metric
  (parse > 1 s or watch re-parse > 200 ms) and stays a flat projection.
  Checked at S5: watch re-parse is 4 ms, so no index is scheduled.
- **The store is a projection, keyed by path.** `Glance.Web.Store.Store` holds
  one `FileEntry` per `.org` file, so `Map.elems` is walk order — the same order
  `loadDir` produces, since it sorts the paths it walked. `storeResult` therefore
  equals the load it stands in for, rows and counts both, which is what lets
  `/headlines` serve from memory and still be S3's document. Key the map on
  anything but the path, or build it from an unsorted walk, and the served row
  order silently diverges from the loader's. Evidence: `TestStore` "the store
  still equals the load it stands in for", `TestServe` "/headlines … rendered
  from the store". **test**
- **The watch parses one file, from `defaultContext`.** `Glance.Web.Watch.reload`
  calls `Glance.Query.loadFile`, which seeds every parse from `defaultContext`.
  A shared long-lived context would let one file's `#+TODO:` line reach another
  file's headlines — the Context-discipline invariant above, restated where a
  daemon is the thing that could break it. **test** (per-file context) /
  **docs** (the watch's use of it)
- **A failed load keeps the file's rows.** `orgParse` is all-or-nothing, so a
  save caught mid-write looks exactly like a file whose headlines all vanished.
  The store keeps the last good parse's records, marks the entry with the
  failure so the counts report it, and streams nothing. Dropping the rows
  instead empties the table between two keystrokes. Evidence: `TestStore`,
  Load-failure group. **test**
- **A column change closes the socket.** SCHEMA.md's streaming ops carry rows;
  columns are initial-view only. The state column's badge palette is the TODO
  keyword union, which a changed file can move, so the store answers with
  `ViewChanged` — a websocket close with reason `view-changed`, not a frame —
  and the client's reconnect path re-fetches `/headlines`. Inventing an op here
  would put the producer outside the contract it exists to prove. Evidence:
  `TestStore`, Keyword-palette group; verified live against a running server.
  **test**
- **A slow client is dropped and the watcher waits for no one.** Frames go into each socket's
  bounded 256-frame mailbox from the same STM transaction that updates the
  store. A full mailbox drops that client instead of retrying the transaction:
  a stalled reader's frames are recoverable (it resyncs on reconnect), a stalled
  file watch is not. Evidence: `TestStore` "a client that stops reading is
  dropped". **test**
- **The bootstrap is taken where subscription happens.** `subscribe` registers
  the mailbox and snapshots the store in one transaction, so no update can land
  between the two and no journal is needed to catch a client up. Split them and
  a row that changes between the snapshot and the registration is lost until the
  next edit to its file. **test**
- Write-back is surgical span replacement + optimistic lock (hash vs parse
  snapshot) + atomic temp-then-rename.
- **The engine is `Data.Org.Edit`** (S8 core, landed ahead of the commands):
  char-span splice, drift-checked against a SHA-256 snapshot, atomic
  same-directory rename, content-agnostic — the replacement text comes from the
  caller and no path through the module reaches `TextShow`. Validation is total
  (bounds, `start <= end`, pairwise overlap) and every refusal — drift, a
  rejected batch, an undecodable file — leaves the target byte-identical.
  Permission bits survive the rename; owner, group and timestamps do not, since
  the rename installs a new inode. Splicing the whole batch in one pass is what
  keeps a multi-span command O(document) rather than O(document × edits).
  Evidence: `TestEdit`, plus the ~/sync canary behind `GLANCE_CORPUS=<root>`
  (33 files, 214 spans, each file digest-checked before and after to prove the
  check never wrote). **test + corpus**
- **Materialize pins its digest at load.** `hrDigest` is the SHA-256 of the very
  bytes `loadFile` decoded and parsed (`Data.Org.Edit.digestOf`, taken there
  rather than by a later read), and `GET /headline` answers with that digest
  beside offsets measured in that same text. Re-reading the file at GET time is
  the shortcut that breaks it: the response would pin bytes the extent was never
  measured against, and the disagreement surfaces only as a splice landing in
  the wrong place. A store refreshed by the watch replaces records and digest
  together, so the next materialize hands out fresh coordinates. Evidence:
  `TestServe` "GET /headline" (the digest is the fixture's known `sha256sum`,
  written down rather than recomputed by the code under test). **test**
- **A commit is refused on any divergence, and the file survives it.**
  `POST /headline` checks the client's digest against the store's — a file
  re-parsed since the materialize is a 409 `stale` — and `replaceSpan` then
  re-digests the file itself, so a change that has not reached the store yet is
  a 409 `drift`. Both leave the target byte-identical (the `Data.Org.Edit`
  guarantee), and both mean one thing to a client: materialize again, because
  the text it edited is not there any more. The committed text is taken as
  given — org validity is the author's business, and a file that stops parsing
  keeps the rows it had, exactly as when the text came from an editor.
  Evidence: `TestServe` "POST /headline" group. **test**
- **The `ETag` is the store's generation, and one tag covers every query
  variant.** `Store.stGen` moves in `Glance.Web.Store.guarded` — the single
  wrapper both update paths go through — whenever the step produced frames or
  moved the touched file's load outcome, which is exactly when a `/headlines`
  response would change. `GET /headlines` sends it as `ETag: "gN"` under
  `Cache-Control: no-cache`, so a browser revalidates every time and an idle
  tree costs a 0.56 ms 304 instead of 3 MB. Every variant shares the tag on
  purpose: `q`, `limit` and `offset` are in the URL and an HTTP cache is keyed
  by URL, so `?q=foo` and `?q=bar` are separate entries each revalidating
  against the tag it was itself given, and the response is a function of
  (generation, URL) alone. That is why no `Vary` is owed for them — the one
  header the answer turns on is `Accept-Encoding`, and the gzip middleware
  writes that `Vary` itself, on the 304s too. Bump the generation from anywhere
  but `guarded` and two producers race it; skip the bump on a load-outcome
  change and the stats headers go stale behind a matching tag. Evidence:
  `TestServe` "GET /headlines cache validation" (including a published reload
  that changes nothing leaving the tag put). **test**
- **The server is the authority on the filtered set, and a page comes out of the
  view's own sort.** `?q=` is matched against `hrSearch`, built at load beside
  the cells: a Haskell mirror of `table-view.js`'s `displayText` (bracket link
  shown by its description, runs of control characters as one space),
  lowercased, cells joined by `\x1f` so a query cannot span two of them. The two
  implementations have to agree or the same query answers differently depending
  on who ran it, which is why `TestQuery`'s expected strings are written down
  rather than taken from the renderer. Filtering runs before paging, so
  `X-Glance-Total` is the match count; and a page is `take limit . drop offset`
  over `sortedForView` rather than over walk order, because page two has to be
  the rows the table would show after page one. With no `limit` the walk order
  stands and the client sorts the whole set — the full-fidelity mode, and the
  one the shell settles into. The shell mirrors the same rule live: with a
  filter on, a row frame off the socket is answered by re-asking the server
  rather than by splicing, since only it knows whether the changed row still
  matches. The state palette is the store's (`viewJSONWith` takes it
  explicitly), never the page's — deriving it from the rows on a page would move
  the badge list a client watches for a column change every time the page moved.
  Evidence: `TestServe` "GET /headlines filter and paging", `TestQuery` "Search
  text". **test**
- **The watch is the only channel that updates the store.** A commit writes the
  file and returns; no path through the route touches the `Hub` or the `Store`.
  The watch re-reads what was written and streams the rows, so a browser save
  reaches every open tab by the path an editor's save already takes. Updating
  the store from the write path too would be a second producer of row frames,
  racing the watch and diverging from it on the first failed re-parse.
  Evidence: `TestServe` "leaves the store alone — the watch is what updates
  rows". **test**
- The web layer depends only on the `Glance.Query` facade (S2), enforced at
  the cabal-stanza level; `Display`/`TextShow` stay out of the wire.
- **The web layer is one stanza, and the constraint lives on it.**
  `glance-web` (`src-web/`, `Glance.Web`) lists `glance:glance` and the HTTP
  packages; `glance-internal` is absent and cannot be added without the
  addition showing in that `build-depends`. The CLI is the only target naming
  both sublibraries, and it names them to dispatch — `serve` reaches
  `Glance.Web`, `scan` and the REPL reach the internals, and no code path
  crosses. Checkable without building: `jq` over
  `dist-newstyle/cache/plan.json` for the `lib:glance-web` unit's depends.
  **test** (it would not build otherwise) / **docs** (the one-binary shape)
- **The listener binds 127.0.0.1.** `Warp.setHost "127.0.0.1"` in
  `Glance.Web.serve`, no flag to widen it. Every request is served at the same
  privilege — the read/write/automate split is S7 — so a bind on `0.0.0.0`
  would hand the whole store to the network, and after S8 hand it write-back
  too. The address moves when auth arrives, not before. **none** (no test
  binds a socket; the call site is the only guard)
- **The served pages fetch nothing off this server.** Styles are inline, the
  glue is inline, and the one `<script src>` is a file name the asset route
  resolves inside `--assets`. No CDN, no web font, no analytics — a page that
  reaches the network renders differently on a laptop in a tunnel, and this
  daemon's whole point is that the org files are local. The JetBrains Mono
  `@font-face` is the shape a resource takes here: emitted only when the assets
  directory holds the file, pointing at a bare name this server serves. Evidence:
  `TestServe` "no page this server serves reaches off it" — neither page
  contains `http://`, `https://` or `@import`. **test**
- **The shell's keymap is data, and so are its profiles.** `Glance.Web`'s
  `sharedKeys` plus `keyProfiles` are the one table; the page carries them as a
  `<script type="application/json">` blob and its own dispatch parses that blob,
  so a binding cannot exist in the handler and not in the map, and a profile
  cannot be offered and unbound. Movement is the only thing a profile changes —
  `emacs` (the default) and `vim` — and the effective map is always
  `shared ++ profile`. Within one effective map no sequence is bound twice, and
  no complete sequence opens a longer one (which would leave the longer one
  unreachable — the reason `vim` binds no bare `g` beside its `gg`). Sequences
  and command names are org-glance's (`org-glance-overview-mode-map`); a row
  with no handler is recognized in full and says what backs it later. `C-c` and
  `C-x` are claimed as prefixes only with the selection collapsed — the browser
  decides copy and cut on the same keydown — and `C-l`, `C-r`, `C-t`, `C-w`,
  `C-n`, `C-p`, `<f5>` are never claimed, including as the continuation of a
  prefix, which is why neither profile moves on `C-n`/`C-p`. Evidence:
  `TestServe` "Shell keymap", which parses the blob, compares both profiles to a
  written-down map, and checks the two per-map uniqueness rules. **test**
- **The wire is built in `Glance.Query`, out of spans.** The public library
  exposes that one module over the private `glance-internal` sublibrary, so no
  outside target can name `Data.Org.*` at all. Title and tag cells are sliced
  from the source (`sliceSpan`) and copied out of the document; the `TextShow`
  render is the fallback for a component with no span, which is to say an empty
  one. Dates are ISO renders: the wire spells them, and org's brackets stay in
  the file. The view
  `Value` is assembled from `object`/`.=` combinators and no internal type
  carries a `ToJSON` instance: deriving one would make the AST the contract,
  and `SCHEMA.md` is. **test** (`TestQuery` imports the facade only; golden +
  schema-conformance groups)
- Browser writes are commands over the bridge, of two kinds (proposal rev 3):
  structured commands, and raw replacement of a whole span under the same drift
  lock — materialize is the first of those. Semantic org editing stays out of
  the browser. Automation: reviewed deterministic scripts behind a separate
  privilege tier; no LLM in the loop.

## Build

- `glance.cabal` is hand-maintained; hpack/package.yaml were removed after
  diverging (regeneration dropped `OverloadedRecordDot` and deps and broke
  the build). Do not reintroduce without making it authoritative again.
- **Five components, one direction.** `glance-internal` (`src/`) holds the
  parser, the AST and the file walk at `visibility: private`; the public
  `library` (`src-query/`) exposes `Glance.Query` and depends on it;
  `glance-web` (`src-web/`) is private and depends on the public library
  alone; the CLI depends on the two sublibraries and the suite on all three
  (`glance:{glance, glance-internal, glance-web}`, which pins internals in the
  older modules and exercises the facade alone in
  `TestQuery`/`TestServe`/`TestStore`). `glance-web` gained modules at S5 and
  no new direction: `Glance.Web.Store` and `Glance.Web.Watch` sit in the same
  stanza, and what they needed — per-file loading, row JSON, the keyword merge —
  was added to `Glance.Query` rather than reached for behind it.
  Putting `Data.Org.*` in a web or daemon target's build-depends is impossible
  from outside the package — the S2 exit bar, enforced by the solver rather
  than by review. **test** (it would not build)
