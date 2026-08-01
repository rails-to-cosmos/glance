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
  `Types.hs` `Span`/`sliceSpan` haddocks. The counting canary is
  `TestSubtree`'s unicode fixture, which asserts 61 characters against 105
  bytes on the same document — the one place a char/byte mix-up fails a test.
  `TestSpans` carries a `Привет мир` headline too, but it asserts no counts and
  flows through the generic groups, so it is a shape fixture rather than the
  canary. **test + corpus**
- **Sub-spans tight, element spans loose — and tightness is per component.**
  Each headline sub-span slices to exactly its component (`getOffset` taken
  before trailing-space consumption: `indentP`, `priorityP`, `todoP`, `tagsP`,
  `propertiesP`), but what "exactly" checks differs by part, and the one table
  in `headlineSpanParts` says which: todo, priority and tags are string
  equality against the rendered component; the title is equality up to word
  normalization (`T.words t == T.words (showt (title h))`), so internal
  whitespace may differ; each planning span is structural — longer than two
  characters, opening and closing on its bracket pair — with `TestSpans` adding
  the stronger check that the slice reparses to the same `ETimestamp`; and
  properties is a prefix/suffix test on the stripped slice (`:PROPERTIES:` …
  `:END:`), with `TestSpans` adding that every property key appears inside it.
  Element spans may carry consumed trailing whitespace and are only
  bounds-checked + reparse-checked. S8 write-back replaces sub-spans verbatim —
  one blank in a span breaks the one-hunk exit bar. Evidence:
  `headlineSpanParts` (single source for scan + tests). **test + corpus**
- **`hsFull` geometry — derived, never stored.** `hsFull` is a function over
  `HeadlineSpans`, not a field: `foldl' (<>) (hsStars hs) [ sp | (_, Just sp)
  <- spanParts hs ]`. The field is `hsStars`, the stars alone. Since
  `Span s _ <> Span _ e = Span s e` keeps the left start and the right end, the
  start of `hsFull` is always the stars, and its end is the end of the LAST
  present entry in `spanParts` order — an order-dependent fold, never a maximum
  over ends. With no sub-span present it is the stars' own extent. It never
  covers trailing whitespace. Capture/refile insertion points derive from it, so
  a part appended to `spanParts` out of source order silently shortens every
  extent past it. **test** (`TestSpans` trailing-whitespace group)
- **Sub-span order.** todo < priority < title < tags < planning < properties.
  The three planning spans permute freely — org writes `SCHEDULED:`,
  `DEADLINE:` and `CLOSED:` in any order on the line — so `spanParts` sorts
  that triple by `spanStart` and leaves the other five in fixed positional
  order. `hsFull` and `headlineSpanParts` both read that single ordering, which
  is what keeps the derived extent and the checked parts in agreement. Drop the
  sort and `hsFull` ends at whichever entry the record lists last, leaving the
  others outside it. **test** (`TestSpans` "planning keywords out of order")
- **Drawer placement.** When present, `hsProperties` starts past the newline
  after the headline line, after `hsTitle`, and ends exactly at `hsFull`'s
  end. An append-note command writing at `spanEnd hsFull` writes inside the
  drawer if this breaks. **test**
- **`stripSpans` totality.** Resets headline spans; every other constructor
  passes through. A new span-carrying `Element` constructor silently turns
  ~150 span-insensitive assertions span-sensitive unless added here. The route
  those assertions take is `TestDefaults`' `bare = map (stripSpans . valueOf)`,
  the suite's one span-blind lens over a parse; `bareParse` is the same thing
  wrapped around `orgParse`. **test**
- **`Element` is a closed sum**, not an existential: `EHeadline`, `EPragma`,
  `ETimestamp`, `EToken`. That is what lets `stripSpans` and the `TextShow` /
  `Display` instances be written as total case analyses rather than dispatched
  through a class dictionary. Any doc calling it existential is describing a
  design that no longer exists. **none** (the type declaration is the guard)
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
  failure class, 11 files of 6290 as measured on 2026-07-31. The per-cause
  breakdown once written down here (`::` in titles, `:)`, timestamp glued to
  punctuation, hyphen in a commented `#+TODO:`) was taken over the 13 failures
  of the pre-exclusion walk and has not been re-derived since the derived
  mirrors left it; two of the old thirteen are gone and the remaining split is
  unknown, so the taxonomy NEEDS RE-MEASURING before it is quoted again. Any
  recovery mechanism (`withRecovery`) changes `orgParse`'s all-or-nothing
  contract and every caller — this is a pinned invariant, and a proposal to add
  one is a proposal to change the contract, not a refinement of it.
  **test + docs**
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
  start moments only. **test** (the flag) / **none** (the two defensive
  conjuncts: nothing renders a value that sets `tsCompactRange` with a missing
  time or two different days, so the guard against a hand-built timestamp
  losing its end date is asserted by the comment alone)
- **`spanRange` forces at every step.** `foldl' (\acc sp -> Just $! maybe sp
  (<>) acc) Nothing` — the `$!` is the invariant, not decoration. A lazy
  accumulator here is a thunk chain holding `Span`s that reference the document,
  which is exactly the retention the scan budget exists to prevent, and it
  cannot be seen from `cabal test`. `Span`'s own `Int` fields are strict
  independently. **comment**
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
  Timestamp pair. The backtracking is `try` around each entry inside a `some`,
  plus `try` around the line as a whole, and what it must roll back is the
  LEADING HORIZONTAL SPACE the entry already skipped: the top element loop
  separates elements by whitespace, so an entry that fails after eating the gap
  leaves nothing to separate what follows and fails the entire document. The
  keyword and its timestamp are held apart by a required `hspace1`, which is
  also what makes a failed entry recoverable. Each span covers the timestamp
  text alone, keyword excluded: S8 reschedules by replacing that slice and
  nothing else. **test**

  *Historical corpus figures, not reproducible from this repo:* an earlier
  measurement over `~/sync` found 4661 planning lines in parseable files
  carrying 7220 entries, 7161 of which attached, with ~70 stragglers the
  timestamp parser rejects — two-letter weekday abbreviations, unit-less
  repeaters (`10:00+2`), a repeater written before the time, diary sexps, a
  repeater followed by a warning period — and, since the entry loop stops at
  the first failure, later entries on those lines stranded with them; a further
  2642 planning lines sat inside the files that fail to parse outright. Those
  numbers predate the derived-mirror exclusion and nothing in the tree
  re-derives them: `app/Scan.hs` has no planning counters and the
  `GLANCE_CORPUS` groups assert geometry rather than counts. Treat them as a
  dated observation against one private tree. **none**
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
- **`resolveHeadline` is test-only.** It keeps h1 only when both are scheduled
  and h1 is strictly later, and yields h2 otherwise — but `registerHeadline`
  reaches the IAS by a plain `Map.insert`, so nothing in the library, the CLI or
  the daemon ever consults it; `TestContext` is its one caller. Read it as a
  proposed merge rule under test rather than as behaviour the store exhibits,
  and do not cite it when reasoning about what a re-parse does. **test** (of the
  function) / **none** (of any production behaviour)

## Render

- **`TextShow` is lossy by design.** Whitespace collapses to single spaces,
  pragma keys uppercase, `#+TODO:` sets re-emit in Set (alphabetical) order.
  `TestRoundtrip`'s `Fidelity` column — 23 rows, 22 `Exact` and 1 `Stable` — is
  the documented budget: promoting a `Stable` case to `Exact` asserts fidelity
  the renderer lacks, so a promotion has to be measured first. Seven rows were
  measured and promoted on 2026-07-31 (multiple tokens, deep indent, the
  `#+CATEGORY:` and generic pragmas, the inactive and midnight timestamps, the
  `--` date range): each already re-rendered byte for byte, and the `Stable`
  label was budgeting for losses the renderer does not have. What is left is the
  one real loss, `#+TODO:` re-emitting its two keyword sets in Set order rather
  than as the source wrote them. Write-back and the future wire contract must
  never route through it — spans are the lossless channel. **test**
- **`Ord Timestamp` ≠ `Eq Timestamp`.** Ord compares start moments only;
  Set/Map keys would deduplicate distinct timestamps sharing a start. **none**
- **Planning stays out of the render.** `TextShow Headline` emits the title
  line only, so a headline carrying `schedule`/`deadline`/`closed` re-renders
  without its planning line. Round-tripping a planning line through `showt`
  loses it; the span is the only channel that keeps it. **none**

## Scan

- **Strictness discipline.** Every accumulator forced per step (`$!`, strict
  fields, `seq`); `forceResult` inside `evaluate` + `try` so one pathological
  file cannot abort the run and no thunk retains a document. History: the
  first walk version retained 1.4 GB; the budget was ~19 MB max residency while
  the reads were serial and is now pool width × one document — see the parallel
  entry below for the measured curve. Invisible to `cabal test` — only
  `glance scan ~/sync` exposes regressions. **comment + docs**
- **The per-file reads run on a pool, and three rules make that safe.**
  `Data.Org.Walk.mapFilesConcurrently` is one implementation with two callers:
  `Glance.Query.loadDirFilesWith` (hence `loadDir`, `loadDirWith` and
  `Glance.Web.Store.loadStoreWith`) and `app/Scan.hs`'s fold. The walk ahead of
  it stays serial, and so does every fold behind it.

  WHY IT PARALLELIZES AT ALL: there is no shared parse state. Every file is
  parsed from `defaultContext` — the per-file context invariant under Parser,
  restated here because it is what this rests on — so no context, accumulator or
  mutable structure threads between two files, and the serial version was
  already a map over independent reads. Give the loader a shared long-lived
  context and this stops being a scheduling choice and becomes a race.

  BOUND: `getNumCapabilities` workers, no more, pulling from one queue. A path
  list of one skips the pool outright (the file watch's shape,
  `Glance.Web.Watch.reload`, which calls `loadFile` directly and never reaches
  this at all); a runtime with one capability takes the serial loop.

  DETERMINISM: workers tag what they take with its index and the answer is
  reassembled by that index, so a parallel load is record for record the serial
  one whatever order the reads finished in. Three things read that sequence and
  would each answer differently off completion order: `resolveIds` is first-wins
  over it, the store is keyed by path so `Map.elems` reproduces it, and the
  scan's `capped` failure listings keep the first twenty of it.
  `loadDirFilesSerially` is the same load with the pool removed and is exported
  for the assertion — `TestQuery`'s "Parallel load" group compares the two
  record for record over a forty-document fixture carrying one failure of each
  kind, and re-runs the whole load five times to pin the id-resolution winner.

  FORCING: a worker forces its rows before returning — `loadFile` now ends in
  `forcing rs (Right rs)` under `evaluate` rather than handing back a thunk over
  the parse, and `scanFile` already forced inside `evaluate`. Without it the
  workers would build thunks in parallel and the caller's fold would do the work
  serially, so this is what makes the parallelism real; it is also what bounds
  in-flight memory to the pool's width times one document rather than the tree.
  Measured over ~/sync (6290 files, `+RTS -s`, 2026-07-31): 21.9 MB at `-N1`,
  23.4 at `-N2`, 28.9 at `-N4`, 37.8 at `-N8` — linear in the width at ~2.3 MB a
  worker, which is the model holding rather than a leak. The flat ~19 MB figure
  the budget used to name is a `-N1` number and does not survive a pool; quote
  the width with it. **test** (equality, buckets, determinism, the narrow-tree
  edge) / **docs** (the residency curve, which no test measures)
- **The pool needs the threaded runtime, and both stanzas carry it.** The
  executable and the test suite are built `-threaded -rtsopts -with-rtsopts=-N`.
  Under a non-threaded runtime `getNumCapabilities` is 1 whatever `-N` says, the
  pool degrades to the serial loop, and every assertion above still passes — the
  failure mode is silence, so `TestQuery` asserts `rtsSupportsBoundThreads`
  rather than trusting the stanza. **test**
- **Forcing is necessary and not sufficient — residency needs `T.copy`.** A
  `Text` slice shares the array it was cut from, so a forced cell still pins the
  whole document. `Glance.Query.detach = T.copy` is what actually bounds it, and
  it is applied to every cell the row keeps; `app/Scan.hs` copies the same way.
  Forcing alone (`forcing`, `forceRecord`) closes the other half of the problem
  — an unforced cell retains the file as a thunk over it — so the two are a pair
  and neither substitutes for the other. Two fields deliberately opt out:
  `hrHeadline` holds the parser's own slices and `hrDoc` names the same text, so
  a loaded store retains the documents it parsed on purpose, and that field is
  the lever if full-store residency ever exceeds the scan budget. No test
  measures residency; only `glance scan` does. **comment + docs**
- **Cursor linearity.** Left-to-right slicing assumes non-decreasing span
  starts; out-of-order visits silently degrade to O(start) per slice.
  **comment**
- **The corpus gates still pass when unset, and now say they were skipped.**
  `TestSubtree`'s geometry-over-real-files group and `TestEdit`'s splice canary
  both go through `TestDefaults.withCorpusSample`, which on a missing
  `GLANCE_CORPUS` prints `SKIPPED — GLANCE_CORPUS is unset: <label>` on stderr
  and passes. On any machine without that variable, every claim marked
  **test + corpus** here rests on the fixture half alone, and a green suite is
  evidence for the corpus half only when those two lines are absent from the
  run. A variable naming a directory that does not exist fails loudly, and so
  does a gate that sampled nothing — `withCorpusSample` takes the count its
  continuation checked and requires it to be positive, so an empty sample can no
  longer read as a pass. Making the pass itself a failure would need a second
  test-suite stanza, which is out of proportion to the problem. **none** (the
  gate still passes; what changed is that it is audible)

## Keyword configuration (layered)

- **Recognition is a superset; classification is nearest-scope.** The parse
  seed for every file unions `defaultContext` with `#+TODO:` sets read from
  `<root>/.org-glance/config/system.org` (when present) and
  `config/tags/*.org` (tag name = filename), so a keyword declared anywhere
  parses as a state everywhere — the STARTED-in-title misparse class ends
  here. Active-vs-inactive resolves per headline by nearest scope: file
  pragma > its tags' configs (first tag wins) > system > built-in
  TODO/DONE; the palette and the `state:*active*` metas consult the
  resolver, while parse-time `Todo.active` keeps its position-dependent
  snapshot semantics. Evidence: `src/Data/Org/Config.hs`, `TestConfig`.
  Breaks: dropping the union re-scatters foreign-keyword headlines into
  titles; flipping the precedence misclassifies file-local overrides.
  **test + corpus** (`scan` reports `config keywords`)
- **Config files are inputs, never rows.** `config/` under `.org-glance`
  is skipped by the walk (reported as `config skipped`); the config reader
  reaches it directly by path. A config-file change triggers a full reseed
  and reload (recognition changed means every file's parse may change),
  debounced, with `view-changed` following via the keyword-union move.
  **test**
- **The default view is a line of `system.org`.** `#+GLANCE_DEFAULT_FILTER:`
  reads into `ConfigLayers.clFilter` and is answered by `defaultFilter`; an
  ABSENT line means `builtinFilter` = `state:*active*`, a line naming nothing
  means the EMPTY query (the whole store), and the LAST line in a file wins, the
  way a reader scrolling it would read it. The SYSTEM layer alone, and only the
  first config directory that names one: a default view is a property of a tree
  rather than of a tag, and two stores nested under one root would otherwise take
  turns deciding what the table opens on — which is why `POST /config` drops the
  `filter` field for a tag layer whatever the request said. The daemon embeds it
  into the served page as `DEFAULT_QUERY`, read off the STORE at request time, so
  it is current for the same reason the badge palette is: the watch reseeds on a
  config change. The bare-boot injection and the `g` key both read that one
  constant, so nothing on the page spells the query itself. The reader and the
  writer are one pair of functions (`defaultFilterOf`, `defaultFilterEdits`), and
  the writer is `pragmaLineEdits` under a second predicate — the same whole-line
  splice the `#+TODO:` block gets, which is what makes "replace where it stands,
  insert under the header, empty deletes" true of both without being written
  twice. Evidence: `TestConfig` "the default view is a line of the system layer",
  "the default view is written by the same splice as the cycle"; `TestServe`
  "GET and POST /config" and "the served page carries the tree's default view".
  **test**
- **The default view rides in the layer's own write.** `POST /config` takes an
  optional `filter` beside `lines` and splices both in ONE `configEdits` call
  under ONE digest, because they are lines of one file: two requests would be two
  writes and the second would drift against a digest the first had just
  invalidated. Absent leaves the line exactly as it is, empty takes it away
  (which is the tree going back to the built-in), and anything else writes it.
  Two absent pragmas insert at the same offset, which `Data.Org.Edit.applyEdits`
  resolves in LIST order rather than refusing — touching edits are legal and two
  insertions at one offset land as the caller named them. **test**
- **The config write path is the ordinary write path.** `GET`/`POST /config`
  serve and replace one layer's `#+TODO:` block, and every rule the other two
  write routes keep is kept here rather than restated: the spans come from
  `Glance.Query.configEdits` over the file's own lines
  (`Data.Org.Config.todoLineEdits`), `Glance.Query.replaceSpans` splices them
  under the client's digest, and `Data.Org.Edit` writes temp-file-then-rename.
  So everything a config file is besides its cycle — the `#+TITLE:`, the
  comments, the org-capture template org-glance keeps in these files — is bytes
  the write never names, and a file that moved is a 409 with nothing written.
  The route does NOT touch the store: a config change is watched
  (`Glance.Web.Watch.settle`), and the reseed is what moves the rows and the
  palette, exactly as when Emacs saved the same file. Evidence: `TestServe`
  "GET and POST /config" (including the whole-file comparison after a replace),
  `TestConfig` "Writing a layer". **test + live**
- **The EMPTY digest is the pin for a file that is not there, and creation is
  that one lock rather than a second write path.** `Data.Org.Edit.currentText`
  answers `""` for an absent file pinned with `""`, `writeAtomically` makes the
  directories and skips the permission copy there being nothing to copy from,
  and everything downstream is the ordinary splice — which is why `POST /config`
  creating `system.org` in a tree that has never had one is the same request
  shape as editing one. A file that turned up before the write started is
  `Drift` carrying the digest it holds; a MISSING file pinned with a real digest
  stays `ReadFailed`, since a caller holding one believed there was something to
  read. The probe is at the start of the write and `rename(2)` has no exclusive
  form, so a file created inside that window is replaced rather than refused —
  the ordinary drift check has the same window for the same reason, and closing
  it would mean giving up the rename that makes the write atomic. **test**
- **What a layer may say is checked before the write.** Blank lines are dropped;
  every line left must be a `#+TODO:` pragma; the block must declare at least one
  keyword, since a pragma the parser reads as nothing leaves a layer looking
  configured and doing nothing; and an EMPTY block is always allowed and is the
  deletion. `*active*`/`*inactive*` need no rule of their own and a guard for
  them would be unreachable: a keyword token is letters and underscores
  (`keywordTextP`), so the group names cannot parse into a keyword set at all and
  are refused as declaring nothing — the same wall `setStateEdits` puts up from
  the other side, reached one step earlier. **test**
- **The layer list is read at request time, and it is also the allowlist.**
  `GET /config` reads the files rather than projecting the loaded
  `ConfigLayers`, because the digest a client is handed is the lock its write is
  checked against and has to be of the bytes it was shown; `POST /config` looks
  its `path` up in that same list, which is the whole of the traversal defence
  and is also the read the edits are measured in, so the two cannot describe
  different bytes. WHICH directories is the one thing a read cannot answer and
  comes off the store (`clDirs`, the config directories the walk met) — falling
  back to `configDirIn` of the served root when the walk met none, which is the
  only case where there is nothing yet to be right about. **test**
- **KNOWN GAP (open): the first config directory in a tree that had none may not
  be watched.** `mkdir -p .org-glance/config` and the write into it happen
  microseconds apart, and fsnotify arms a watch on a new directory only after it
  has seen it created — so the event for that first file is lost and the reseed
  does not fire until the daemon restarts or a later config edit lands. Measured
  2026-08-01: an external `echo > .org-glance/config/system.org` into a
  freshly created directory is missed the same way, and a new subdirectory
  written to a second later IS picked up, which is what makes it a race rather
  than a rule about hidden or nested paths. It is the watch's property and not
  the route's — any tool creating the directory and the file together loses the
  same event. `GET /config` reads the files, so the settings sheet itself is
  never wrong about them; it is the table that lags. **none**

## Walk

- **Derived org-glance directories are not walked.** org-glance keeps its
  canonical store under `.org-glance/data/` and writes overview and agenda
  buffers beside it under `.org-glance/overviews/` and `.org-glance/meta/`,
  repeating the same headlines under the same `ORG_GLANCE_ID`. Serving those is
  serving a derived artifact as truth: over `~/sync` it put 514 extra headlines
  in the table, and one of them — `Курс Екатерины Бондарь` — rendered twice
  under a `tanik` filter, once from `data/…/data.org` and once from
  `overviews/c1f3df767330/overview.org`. `Data.Org.Walk.isDerived` is the one
  rule, applied where a directory is entered and where a watch event is
  accepted, so a file the store was never given cannot arrive by inotify and
  appear in the table on its next rewrite. The watch reaches it — and
  `isDocument`, the extension-and-sidecar rule — through the facade re-exports
  `Glance.Query.derivedPath` and `Glance.Query.documentPath`, which is how one
  rule serves both sides of the `glance-web` boundary without `glance-web`
  naming `Data.Org.*`.

  The rule is a DENYLIST, and stating it as an allowlist gets it wrong:
  `derivedDirs = ["overviews", "meta"]`, tested against the component sitting
  directly under a `.org-glance` component, with the remainder unconstrained so
  the entire subtree under either name goes. `data` is therefore not privileged
  in the walk — it survives by not being on the list, exactly as any other name
  under `.org-glance` would. Where `data` IS privileged is `beatsForId`
  (`Data.Org.Walk`), a separate rule answering a separate question: which of two
  files claiming one id wins. Conflating them makes the walk look as though it
  understands the store, which it does not.

  `--include-derived` turns the exclusion off on `serve`, `desktop` and `scan`
  for someone who wants to look at them, and the scan reports `derived skipped`
  — a count of DIRECTORIES declined, never of files. `keepDerived` runs only in
  the directory branch; a file excluded on its own path is dropped with no
  record at all, which is reachable only in the textual-defeat case below, where
  the run therefore reports nothing.

  Corpus, 2026-07-31: 6313 → 6290 files, 13384 → 12870 headlines, 14 → 11 parse
  failures (`overviews/agenda.org` was one of them). Evidence: `TestStore`
  "Derived mirrors" over a fixture tree of the same shape. **test + corpus**
- **The exclusion is textual, and a root inside the tree defeats it.**
  `isDerived` splits the path the walk built — `dir </> name`, rooted at the
  string the caller typed — and looks for a `.org-glance` component. Nothing
  canonicalizes or absolutizes the root. So `cd ~/notes/.org-glance && glance
  scan .` produces paths like `./overviews/x.org` carrying no `.org-glance`
  component; `isDerived` is `False` and every mirror is walked and served as
  truth. A symlink or bind mount that renames the component away does the same.
  The complementary spelling fails the other way and just as quietly: a root
  that KEEPS the component, `--dir /x/.org-glance/overviews`, is entered — a
  named root is never itself tested — and then yields nothing, because every
  child is excluded on its full path. Zero files, no error, no `derived
  skipped` entry. A comment in `Walk.hs` claiming a mirror named as a root is
  read describes that second spelling and is wrong about it. **none** — no test
  builds a root inside a `.org-glance` tree.
- **Symlinked directories are never followed, and a failed probe looks exactly
  like one.** The walk asks `pathIsSymbolicLink` inside `try` and treats
  everything but `Right False` as "skip", so a symlinked directory and a
  directory whose probe raised are both dropped with no `dirErrs` entry, no
  `derived` entry and no counter. An unlistable directory IS reported, which
  makes the silence specific to this branch and easy to misread as "there was
  nothing there". The reason not to follow is a symlink loop, and counting one
  tree twice. **none**
- **A dangling `.org` symlink is a permanent read failure — but Emacs's lock is
  not one any more.** The non-directory branch keeps a path on `isDocument`,
  with no existence check — only a named root is probed — so a broken link is
  walked and `loadFile` answers `ReadFailed`. The store keeps that as a
  `FileEntry` with a failure and counts it for the life of the process, because
  the watch is filtered by the same rule and no event that would clear it is
  ever delivered.

  The common case of that was Emacs's `.#name.org`, a lock symlink to
  `user@host.pid:boot` that dangles and whose extension is `.org`: every open
  buffer in the tree cost one permanent `read failures` count. `isDocument` is
  now `isOrg` minus `isSidecar` (`.#` prefix, `#` prefix), one predicate in
  `Data.Org.Walk` serving the walk directly and the watch through
  `Glance.Query.documentPath`, so the two sides refuse the same set by
  construction rather than by two rules that agreed until one moved. What is
  left uncovered is a genuine `.org` symlink the author made and broke, which is
  a real file the walk is right to try. Evidence: `TestStore` "Editor sidecars"
  (a dangling `.#notes.org` beside a real document: one file, one row, zero read
  failures; and the walk's kept set is exactly what `isWatchable` accepts).
  **test**
- **`scan`'s argument parser eats unknown flags as roots.** It recognizes
  `--include-derived` and treats every other token as a directory, so
  `glance scan --dir ~/notes` walks two roots, one of them the literal string
  `--dir`, which lands in `unreadable dirs`. `serve` and `desktop` reject
  unknown arguments and carry a usage string; `scan` has neither. **none**
- **`dirs scanned` is the number of ROOTS given.** Nothing counts traversed
  directories — `Found` has no such field — so `glance scan` with no arguments
  reports `1` for a tree of any size, and `glance scan a b c` reports `3` even
  when `a` is a plain file. Read it as "arguments accepted", not as coverage.
  **none**
- **The serial walk is most of the wall, and the row that says so is new.**
  Measured 2026-07-31 over ~/sync, which is 6290 `.org` files inside 89874
  directories and 702962 entries: the walk is 11.8–13.5 s of a 13.6–15.4 s
  `glance scan`, and the parallel read of every file is 1.2 s of it. `serve` is
  the same shape — 14.2 s to `loaded:` over ~/sync against 1.7 s over
  ~/sync/views, which holds almost the same file count inside a tree of ~8700
  directories. So a corpus's cost here is its DIRECTORY count, and the pool
  cannot touch that half; `scan`'s `walk seconds` row exists to keep the two
  apart. Two further facts, both measured: the walk costs ~15 µs an entry
  against `find`'s ~0.5, which is two `stat`s per entry
  (`doesDirectoryExist` then `pathIsSymbolicLink`) plus `String` marshalling
  plus `isDerived` re-splitting the path — `--include-derived`, which
  short-circuits that last one, takes ~1 s off; and it gets SLOWER as `-N` rises
  (11.9 s at `-N1`, 13.5 s at `-N8`) with GC steady at 1.0 s elapsed either way,
  so that cost sits in the syscalls. The lever, when this is worth pulling, is one
  `getSymbolicLinkStatus` in `visit` answering both questions, and it would move
  the symlinked-directory rule above, so it is a decision rather than an
  optimization. **docs**
- **One row per id, and the canonical file wins it.** A row id is what a
  renderer keys updates off (SCHEMA.md), so two rows cannot share one: the
  second would overwrite the first on every frame while the table showed the
  headline twice. `Glance.Query.resolveIds` keeps one — a `.org-glance/data/`
  path beats one that is not, otherwise walk order does — and reports every
  loser rather than dropping it quietly, since a duplicate id is nearly always
  a tree that should not have been walked. It has exactly four call sites —
  `loadDir`'s `summarise`, `Glance.Web.Store.storeRecords`,
  `Glance.Web.Store.storeResult` and `Glance.Web.Store.resolvedRows` — which is
  what keeps the store equal to the load it stands in for and the stream equal
  to both; `storeHeadline` and `bootstrapFrame` resolve transitively through
  `storeRecords`, so materializing an id two files claim opens the one the table
  is showing. The count rides as
  `X-Glance-Id-Collisions` and the pairs
  are listed by the scan (capped at 20). Corpus: 522 collisions with the
  mirrors walked, 9 without — those nine are genuine duplicates between real
  files (an elpa working copy of a checkout; documents whose `data.org` repeats
  the source document's id). **test + corpus**

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
- **The socket binds before the store exists, and says so.** `Glance.Web.serve`
  listens first and walks the tree on a background thread, so the window between
  `bind` and a loaded store is served rather than refused — 16 s of it over
  ~/sync. Through it, `/headlines`, `/headline` and `/ws` answer `503` with
  `Retry-After: 1`, and the websocket upgrade is refused with the same status
  rather than accepted: a `set-rows` of
  an empty store is a claim that the tree has no headlines in it, and a client
  that mounts one has to be told to throw it away later. `/` and the assets are
  served the whole time, since the page carrying the indexing state is the
  reason to listen early. Clients rest on the `Retry-After` semantics — the
  shell polls at exactly that second and any other client is entitled to — so
  answering `200` with an empty view, or dropping the header, breaks them
  quietly rather than loudly. The state is one `LoadState` in the `Hub`, flipped
  by `finishLoading` in one transaction, and the watch starts after that flip:
  an event folded into a store that is about to be replaced wholesale would be
  lost with it. `finishLoading` is deliberately the SECOND writer of the store
  TVar — `publish` is the other, and there are no more — and it bypasses
  `guarded` because there is nothing to publish: no client can have subscribed
  while the socket was answering 503. Installing the store and opening the
  routes in one transaction is what stops a request seeing the loaded store
  still described as loading. Evidence: `TestServe` "Indexing (bind before
  load)", including the same `Application` answering 503 and then 200 across a
  `finishLoading`. **test**
- **The two 503 bodies differ, and the difference is per route rather than per
  state.** The HTTP one is `{"loading":true,"elapsed":S}` where S is a JSON
  number of SECONDS rounded to a tenth; the websocket rejection carries the
  shorter `{"loading":true}` and no elapsed at all, because a rejected upgrade
  has no reader that would use it. A plain, non-upgrade request to `/ws` is
  routed as HTTP and gets the long body — the short one is the upgrade path
  alone. The load gate is checked AHEAD of the method check, so while the walk
  runs every method on `/ws` answers 503; once loaded, GET and HEAD answer 400
  with an upgrade hint and everything else 405. A client keying on body shape
  rather than on status has to know which of the two it asked for. Evidence:
  `TestServe` "Indexing (bind before load)" drives the HTTP path. **test** (long
  body) / **none** (the short one)
- **The watch parses one file, from `defaultContext`.** `Glance.Web.Watch.reload`
  calls `Glance.Query.loadFile`, which seeds every parse from `defaultContext`.
  A shared long-lived context would let one file's `#+TODO:` line reach another
  file's headlines — the Context-discipline invariant above, restated where a
  daemon is the thing that could break it. **test** (per-file context) /
  **docs** (the watch's use of it)
- **The debounce is per path, trailing edge, and has no ceiling.** 100 ms
  (`debounceDelay = 0.1`) measured on `GHC.Clock.getMonotonicTime` — MONOTONIC
  SECONDS as a `Double`, so it cannot be moved by a clock adjustment — with a
  25 ms poll (`tick`) draining whatever has ripened. Every event OVERWRITES the
  path's timestamp, so a path receiving events faster than every 100 ms is
  deferred for as long as that continues: there is no maximum wait and no
  leading-edge fire. A generator writing a file in a tight loop is invisible
  until it stops, which is the intended behaviour for an editor's autosave and
  the wrong one for a log. It is a debounce, not a rate limit. **none**
- **Deletion is decided by `doesFileExist`, not by the event kind.** `reload`
  probes the path and picks `dropFile` or `applyFile` from the answer, so a
  rename-away, a delete and a delete-then-recreate inside one debounce window
  all resolve to whatever is true when the drain runs. fsnotify's event types
  are never consulted, which is what keeps the behaviour the same across
  backends. **none**
- **`stTags` counts FILES, not rows.** It is stepped by the difference between a
  file's old and new projection, and that projection is a `Set` built per file
  (`tagsOf`), so a tag carried by forty rows of one file contributes 1. The index
  answers "how many files claim this tag", which is what the filter vocabulary
  needs and is NOT a row count. Reading it as one overstates nothing and
  understates a lot. **none**
- **`stDirErrs` is frozen at startup.** Written once by `loadStoreWith` from the
  walk, read by `storeResult`, and touched by nothing in `putFile`,
  `removeFile`, `guarded` or the watch. A directory that becomes unreadable
  after the walk, or becomes readable again, is invisible until a restart. The
  count in `X-Glance-*` therefore describes the startup walk, not the tree now.
  **none**
- **`storeKeywords` merges one record per file.** `listToMaybe . feRecords` over
  each entry, then one `mergeKeywords` across files — an N-file fold rather than
  an N-row one. It is sound because every row of a file shares that file's
  keyword sets by construction; the day a record carries its own keywords, this
  becomes a silent truncation rather than an optimization. **none**
- **A failed load keeps the file's rows.** `orgParse` is all-or-nothing, so a
  save caught mid-write looks exactly like a file whose headlines all vanished.
  The store keeps the last good parse's records, marks the entry with the
  failure so the counts report it, and streams nothing. Dropping the rows
  instead empties the table between two keystrokes. Evidence: `TestStore`,
  Load-failure group. **test**
- **A column change closes the socket.** SCHEMA.md's streaming ops carry rows;
  columns are initial-view only. The state column's badge palette is the TODO
  keyword union, which a changed file can move, so the store answers with
  `ViewChanged` and the client's reconnect path re-fetches `/headlines`.
  Inventing an op here would put the producer outside the contract it exists to
  prove. Two details that are easy to get backwards. `ViewChanged` IS a `Frame`,
  the fourth constructor beside the three row ops; what makes it a close rather
  than a message is `frameJSON` answering `Nothing` for it, so every consumer
  that encodes a frame has to handle the absence. And `guarded` REPLACES the
  step's frames with `[ViewChanged]` rather than appending it, so a change that
  both moved rows and moved the palette ships the close alone — a client never
  receives rows built against a palette that is already gone. Evidence:
  `TestStore`, Keyword-palette group; verified live against a running server.
  **test**
- **The frames the watch streams are id-resolved, like every other answer.**
  `applyFile` and `dropFile` both run their store update inside `streamed`,
  which reads the ids the touched file claimed — before the update and after —
  through `resolvedRows`, and `resolvedRows` is `Glance.Query.resolveIds` over
  the store's rows carrying them. So an upsert carries the row `/headlines`
  would serve and a delete is owed only where the id is gone from the RESOLVED
  store. Three consequences over a tree with duplicate ids, each of them a case:
  an edit to the LOSING file streams nothing (the winner did not move, so no
  answer changed, and the generation stays put with it); an edit to the winner
  streams the winner's new cells; and a winner that goes away re-points its id
  at the row behind it with an upsert rather than leaving a stale one until the
  client reconnects. Cost: one pass over the store's rows per side, kept to the
  touched ids — the order of `storeHeadline`'s scan, and measured end to end at
  5–6 ms for the whole watch step (read, parse, both folds, publish) over a
  14000-row 2000-file store with a live subscriber, against the 4 ms the parse
  alone cost at S5. Building the frames off one file's records is what made it
  cheap and what made it wrong. Evidence: `TestStore` "Shared id"; verified
  live over a two-file fixture with a real websocket client (loser edited: no
  frame; winner edited: `from a, edited`; winner deleted: an upsert carrying
  `from b, edited` and no `delete-row`). **test + live**
- **A within-file duplicate id keeps the FIRST row, on both sides.**
  `resolveIds` cannot separate two rows of the same path — `beatsForId` says a
  file does not outrank itself — so the incumbent stands, and the streaming path
  now goes through that same call. The `Map.fromList` that made the stream keep
  the LAST is gone with `rowsById`. `stTags` still sees neither duplicate (the
  per-file projection is a `Set`), and `X-Glance-Id-Collisions` still reports one
  whose kept and dropped paths are the same file. **test** (`TestStore` "two
  headlines of one file sharing an id are one row, the first")
- **The store keeps no index by id.** `stIds` was one — a count of files per id
  — and its only reader was the delete rule the resolution replaced. Resolution
  answers a stronger question — which file wins, where the count said only how
  many claim it — so the field, its projection and its half of `stepIndex`'s two
  callers went with it. `stTags` remains, and remains a count of FILES per tag.
- **An overflowing mailbox is a resync, and the watcher waits for no one.**
  Frames go into each socket's bounded 1024-frame mailbox from the same STM
  transaction that updates the store. A full mailbox abandons that client's
  backlog and unregisters it instead of retrying the transaction: a stalled
  reader's frames are recoverable (one `/headlines` says everything any backlog
  could have), a stalled file watch is not. The close that follows is named
  `resync` for exactly that, and the shell answers it by revalidating rather
  than remounting — so a storm costs rows and never the page. Two things about
  the size. It is counted in frames and `publish` coalesces WITHIN a step, so
  one file's save is one transaction and a handful of frames; the overflow that
  motivated the raise from 256 is across a BURST of steps, which an editor
  writing a directory produces one per file and nothing coalesces. And the
  backlog is never drained on the way out — `nextFrame` reads `clDropped` before
  the queue, and the queue goes with the `Client` — so the cut is O(1) inside
  the transaction. Evidence: `TestStore` "a client that stops reading is
  dropped", "a burst four times the old mailbox is still delivered", "a burst of
  steps overflows, and the resubscribe is the whole store"; live over a 3000-file
  tree with the reader's receive window pinned at 4 KB, which closed with
  `resync` after 319 frames and revalidated 200 then 304. **test + live**
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
- **The command layer is one route, and its unit of work is a FILE.**
  `POST /command` takes `{name, id | ids, args, digests?}` and implements two
  names, `set-state {"keyword": KW | null}` and `archive {}`. The ids it is
  given are grouped by the file their rows came from, and each file is written
  ONCE — one `Glance.Query.replaceSpans` call carrying every span that file
  owes, under that file's own pinned digest. So a marked set of five rows in
  two files is two writes, each of them atomic: `Data.Org.Edit.applyEdits`
  validates the whole batch before a byte is written and `editFile` renames
  once, so a file takes all of its edits or none. Two rows of one file MUST land
  in the same write: a write per row would pin the second to the digest the
  first had just invalidated, and the second would drift. There is no rollback
  ACROSS files and none is possible, a rename that has happened being
  unrecoverable, so the answer reports per id instead:
  `{results: [{id, ok, digest | error}]}`, in the order the ids were named. A 200 therefore means "the command ran", never "every row
  moved". Evidence: `TestServe` "POST /command", where two rows of one file
  come back with one digest and rows in two files come back with two, and the
  live run below. **test + live**
- **Refusals split by whose mistake they are.** 400 with nothing written: a
  body that is not a command, a name nothing implements, no ids at all, and a
  `set-state` keyword that ANY named row's file does not declare — that last
  one refuses the WHOLE request deliberately, because half a state change over
  a marked set is worse than none of one, and because `#+TODO:` legality is per
  file so the alternative is a command that means different things to different
  rows of one keystroke. 413 outranks all of it, the way it does on
  `POST /headline`. Per id: an id the store has no row for (a marked set
  outliving its rows is ordinary), and a digest the client pinned that the store
  no longer holds — the same `stale` check the materialize commit makes, and
  applied per FILE because a digest is per file, so one stale pin refuses that
  file's whole group. **test**
- **The command route does not write the store either.** It reads it — for the
  row, the file, the spans and the digest — and writes the file; the watch
  re-reads what was written and streams the rows. Same invariant as
  `POST /headline`, restated because a second write path is exactly where a
  second producer of row frames would appear. Evidence: `TestServe` "leaves the
  store alone — the watch is what updates rows", which shows the store still
  answering the pre-command subtree and digest. A consequence with teeth in a
  suite that runs no watcher: a second command against a file the first one
  wrote drifts, because the store still holds the old digest. That is the live
  behaviour too, minus the drift, since a live daemon's watch has re-read the
  file by then. **test**
- **The span math lives in `Glance.Query`, and could not live anywhere else.**
  `HeadlineSpans` belongs to `glance-internal`, which `glance-web` may not name,
  so `setStateEdits` and `archiveEdits` are facade functions handing back
  `[(Span, Text)]` — the same currency `replaceSpans` takes. Neither reads or
  writes a file. Three shapes for `set-state`: a keyword over one already there
  is that keyword's span; a keyword where there is none is an insertion of
  `" KW"` at `spanEnd (hsStars …)`, which is org's own place for it and the one
  offset every headline has, priority, title and tags all being optional; and a
  null keyword deletes the keyword plus the run of HORIZONTAL space behind it,
  so `* TODO Title` closes up to `* Title` while `* TODO` at the end of a line
  keeps the newline that ends it. Two for `archive`: with tags present the tag
  joins the list at `spanEnd (hsTags …)`, which is past the closing colon, so
  the insertion is `ARCHIVE:` and the tags already there stay byte-identical;
  with none it is `" :ARCHIVE:"` at the end of the TITLE LINE, computed as the
  greatest end among stars, todo, priority and title. `hsFull` cannot serve
  there and the reason is its own invariant above: its end is the last part in
  span order, which for a scheduled headline is a timestamp on the next line and
  for one with a drawer is its `:END:`. Evidence: `TestQuery` "Commands", which
  splices with its own three-line oracle rather than through the engine and
  asserts the whole document each time. **test**
- **Keyword legality is per file, and the group meta-values are not keywords.**
  `setStateEdits` refuses anything outside `tkActive ++ tkInactive` of the
  record's own `hrKeywords`, which is its file's `#+TODO:` line plus org's
  TODO/DONE seed. The same word is a keyword in one document and the first word
  of a title in the next, and writing one a file does not declare makes a
  headline the parser reads differently than the writer meant.
  `state:*active*`/`state:*inactive*` are filter vocabulary the state column
  ships beside its badges and are in no keyword set, so they are refused here
  like any other word — which is why the shell's value palette offers `badges`
  and never `values`. **test**
- **`archive` is idempotent because an archived row costs no edit.**
  `archiveEdits` answers `[]` for a row already carrying the tag, matched
  through `Glance.Query.archived`, which reads `tagsOfCell . hrTags` — the same
  folding the filter vocabulary is built with, so "archived" means exactly what
  the query `archive:` means and a file spelling the tag `:archive:` counts. The
  file is still rewritten (the engine has no equality short-circuit), so the
  cost of archiving a marked set twice is an inotify event and a re-parse per
  file, and `guarded` then finds nothing moved and leaves the generation alone.
  Evidence: `TestServe` "archive is idempotent", which steps the watch by hand
  between the two runs and compares the file byte for byte. **test**
- **`D` archives and never deletes, and the default view is what makes that
  work.** `/headlines` drops rows carrying the archive tag unless the query
  names the `archive` key — any spelling of it, `archive:`, `-archive:`,
  `archive:draft`, since all of them are a reader who has said something about
  archived rows and layering a default exclusion under any of them would answer
  a different question than the one asked. The predicate is exactly what
  `-archive:` spells, and `X-Glance-Archived` reports how many rows it took, so
  a client can tell "nothing matches" from "the matches are all archived". The
  header is zero whenever the query named the key: a reader who asked is never
  told anything was withheld. Without the exclusion an org tree accumulates rows
  that are done with rather than gone and the default table grows without bound,
  which is the whole reason `D` can be an archive rather than a delete.
  Evidence: `TestServe` "GET /headlines and the archive", `TestFilter`
  "Archive key". **test**
- **The vocabulary is derived from the UNFILTERED store, and that is what keeps
  the key reachable.** `?q=` is parsed against `storeTags`, every tag in the
  tree, and the exclusion is applied to the ROWS afterwards. Derive the
  vocabulary from the rows a query left standing and the default view would take
  `archive` out of the keys a query may name, so the one query that reaches the
  hidden rows would degrade to free text — and free text finds them too, badly,
  since every archived row's tag cell holds the word. The test that tells the
  two apart is a valued predicate: `archive:filed` matches as a facet and
  matches nothing as text, because no row spells `archive:filed` anywhere.
  **test**
- **The socket is NOT filtered, and the exclusion is `/headlines`' alone.** Row
  frames carry whatever moved; the store has no client's query to apply and
  `resolvedRows` is the served view's own resolution rather than a per-client
  one. So a client with an EMPTY query splices in an archived row the same
  server would not have served it. The shell does not hit this, since it boots
  on `state:*active*` and a filtered client answers a frame by re-asking; a
  client that cleared its filter can. Stated rather than fixed: making the
  stream query-aware would mean a per-client projection of the store, which is
  the second authoritative structure this design does not have. **none**
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
  keeps the rows it had, exactly as when the text came from an editor. Both
  request shapes go through the same two checks, since the recomposition happens
  ahead of the write and nothing about the lock reads it. Evidence: `TestServe`
  "POST /headline" group. **test**
- **The subtree lens: one owner per byte, over three regions.** `GET /headline`
  serves the subtree twice — `org` whole, and `body` + `properties` + `planning`
  + `logbook` split — and `POST` takes back either `{org, digest}` or
  `{body, properties, planning, digest}`. The split is
  `Glance.Query.headlineParts` and the join is
  `Glance.Query.recomposedSubtree`, and the rule joining them is that every byte
  of a subtree belongs to exactly one owner. THREE regions are lifted out of the
  text — the planning LINE, the headline's OWN property drawer, its OWN logbook
  drawer — and every byte left is the body's. So a child's drawer and a child's
  logbook are body text (they belong to the child's lens), and every cut is by
  WHOLE lines including the newline that closes the region. Decompose followed by
  recompose is the identity on the file, byte for byte, which is the property the
  whole design rests on: the round trip runs over drawered, planned, unicode,
  odd-spacing, indented, CRLF, logbook, child-logbook and permuted-planning
  fixtures. Evidence: `TestQuery` "Subtree lens", `TestServe` "GET /headline" and
  "POST /headline". **test**
- **Two of the four parts are the SERVER's, and are preserved rather than
  round-tripped.** `Glance.Query.hiddenProperties` (today `["ORG_GLANCE_ID"]`)
  and the logbook are lifted out of what a client is shown and put back verbatim
  whatever the client sends. `ORG_GLANCE_ID` is the row id the table keys its
  updates off — renaming it renames the row and leaves the sheet looking at a
  different headline — and hiding it is cheaper than a rule about which edits to
  a shown value are allowed, and honest in a way a warning beside an editable
  field is not. The logbook is a record nothing in this page edits. ONE list is
  read by both halves, so extending it is one edit and no other: a listed key
  survives a panel sync that never mentioned it, byte-identical and at the line
  index it sat on (`weave`), and a client that DOES name one writes nothing.
  Consequently an empty `properties` list empties the client's half of the drawer
  and leaves the hidden lines — the drawer only disappears when nothing hidden is
  in it. The lens tests are written against `hiddenProperties` rather than
  against `ORG_GLANCE_ID`. Evidence: `TestQuery` "Subtree lens" decompose and
  recompose groups. **test**
- **Properties: an untouched pair is its own line.** A property nobody edited is
  written back as the very line it arrived on — `:A:one`, `:B:`, a padded value,
  an odd indent, all of it. Only an edited or added pair is rendered, as
  `:KEY: value` under the indentation the drawer's own lines carry; a dropped
  pair is simply not written. The raw lines are consumed one per pair rather than
  looked up, so one pair spelled twice keeps both spellings. Pairs are read by
  splitting lines rather than through the parser's `Properties`: that type
  uppercases keys and re-tokenises values, and the lens owes a client the file's
  own spelling. A drawer that reaches here came out of a document that parsed, so
  every line between the two markers is a property line — one that somehow is not
  comes back keyless, which a client reads as a row to drop. **test**
- **Planning: an untouched entry is its own TEXT, in its own place.** The three
  keywords permute freely on one line, so the region is the whole LINE the
  outermost timestamp sits on — the keywords and the spacing between the entries
  belong to it too, which is what lets an untouched line go back byte for byte
  and what stops a round trip tidying `CLOSED: … SCHEDULED: …` into org's order.
  An entry the file already carried and nobody changed is re-spliced as the text
  it was (`rawEntry`, the span from the keyword to the end of its timestamp);
  anything else renders `KEYWORD: value` and joins BEHIND them in
  `planningKeywords` order, so the entries that moved are canonical among
  themselves and the ones that did not are untouched. An empty list drops the
  line. Every value is validated by REPARSE before a byte is written —
  `readsAsTimestamp` probes the very line the write would produce rather than
  spelling a timestamp grammar a second time, and refuses a value carrying a
  newline outright, a planning line being one line — and a refusal is a 409 whose
  `reason` is `planning` and whose `error` names the FIELD, which the sheet lands
  on as its wait-for-a-keystroke state. Letting one through is silent: the line
  stops being a planning line on the next load and the entry the author set is
  body text. **test**
- **The logbook is located textually, and is display-only.** A `:LOGBOOK:`
  drawer is not part of a headline's parse, and what makes one THIS headline's is
  where it sits: past the title line, ahead of the first child's stars. The scan
  therefore stops at the first line opening with a star at column one and steps
  OVER the property drawer's extent rather than searching it, so a `:LOGBOOK:`
  line that somehow sat inside one stays the properties' — one byte, one owner,
  decided here rather than by whichever finder ran first. An unterminated drawer
  owns every line it may own. It rides OUT of `GET /headline` and never back in:
  `recomposedSubtree` takes it off the record, so a client cannot write one by
  sending one, and a headline that has none does not grow one. The sheet's strip
  shows the drawer's INTERIOR lines alone — the widget being the drawer says what
  it is, so the delimiters would spend two of its lines saying it twice — which
  is a DISPLAY cut and nothing else: what re-splices is the whole original
  drawer. **test**
- **Region line indices are the BODY's.** Each region goes back at its subtree
  line LESS the lines every region ahead of it took out. Subtree-absolute indices
  leave a gap wherever a region was cleared — a drawer whose planning line has
  just come off lands a line late, which is the bug the "clearing every entry
  takes the line with it" case caught. `spliceRegions` counts only the BODY lines
  consumed, so two regions naming one line land in list order (planning,
  properties, logbook) rather than one displacing the other, which is what a
  headline growing a planning line and a drawer in ONE commit needs. A region the
  headline never had goes on the line under the title — where org puts one — and
  a body with fewer lines than an index takes the region at the end, which is
  where a client that deleted the lines above it has left room. **test**
- **The `ETag` is the tree's fingerprint and the store's generation, and one tag
  covers every query variant.** `Store.stGen` moves in
  `Glance.Web.Store.guarded` — the single wrapper both update paths go through —
  whenever the step produced frames or moved the touched file's load outcome,
  which is exactly when a `/headlines` response would change. `GET /headlines`
  sends it as `ETag: "<fingerprint>-gN"` under
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
- **The generation is per process; the fingerprint is what survives a restart.**
  `emptyStore` still sets `stGen = 0`, `loadStoreWith` still seeds through
  `putFile` without bumping it, and `finishLoading` still bypasses `guarded` —
  so a fully loaded store is at generation zero in every process, and the
  counter is not persisted. What makes the tag mean something across a restart
  is the other half: `stPrint`, taken once in `loadStoreWith` over the finished
  store as `digestOfText` of each file's path and the digest of the bytes it was
  parsed from, folded in path order. Identical tree, identical fingerprint, so a
  client's 304 is honest; a byte, a name or a root moved, and it is a different
  tag whatever the generation says. The path is in it because an id-less
  headline's row id is `FILE:START` — same bytes under another name is a
  different document. A file that contributed no rows (empty, or a failed load)
  stands as its path alone, which is sound because it contributes no rows to a
  response either. The fingerprint is deliberately NOT recomputed per edit: the
  generation already says how far the tree has moved since it was loaded, and
  the pair answers both questions for the cost of one fold at startup.
  Evidence: `TestStore` "Fingerprint" (same tree twice, a changed byte, a
  rename, another root) and `TestServe` "and so is one from another tree at this
  very generation". **test**
- **The stats and page headers ride the 200 alone.** `statsHeaders`
  (`X-Glance-Rows`, `-Files`, `-Parse-Failures`, `-Decode-Failures`,
  `-Read-Failures`, `-Id-Collisions`) and `pageHeaders` (`X-Glance-Total`,
  `X-Glance-Has-Next`) are applied in the 200 branch only; a 304 carries the
  `ETag` and `Cache-Control` and nothing else. This is deliberate — the counts
  describe a body that a 304 does not send — but it couples them to the cache:
  a client that reads counts off headers gets nothing from a revalidation, and
  must either ignore the cache or keep the last 200's numbers. **test**
  (`TestServe` cache validation asserts the 304's header set)
- **A commit that changes nothing still rewrites the file.** `Data.Org.Edit`'s
  `editFile` has no equality short-circuit: it re-reads, drift-checks, splices
  and then writes unconditionally, temp file + `fileSynchronise` + rename. An
  empty batch does the same. The engine writes what it is told to, and deciding
  that a write is unnecessary is the caller's business — but the consequence
  reaches the daemon, since the rename fires an inotify event, the watch
  re-parses the file, and only then does `guarded` find that nothing moved and
  leave the generation alone. So a no-op `POST /headline` costs a parse and an
  mtime, and never a spurious `ETag` bump. **test** (`TestEdit`)
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
- **`?q=` is SCHEMA.md's filter query, and parity with the renderer is the
  contract.** `Glance.Web.Filter` is a port of `table-view.js`'s `scanQuery`,
  `parseQuery` and `tokenTest`, term for term, because the renderer filters
  locally with the same grammar and a query that means two things is a table
  that disagrees with itself. Tokens split on whitespace and `&`; `key:value`
  (`=` alias) is a predicate only when the key is a column key or one of the
  producer's virtual keys, which is what keeps org cell text — `:work:`,
  `=code=` — from becoming one by accident; a token that *opens* with a quote is
  free text; a leading `-` negates. Distinct keys and free text AND, and
  negations AND regardless. Predicates sharing one key combine
  by the field's arity: single-valued fields OR, since a badge cell ANDed with
  itself is always empty, and multi-valued ones AND — the `tag` column and
  every virtual tag key, where `tag:a tag:b` is a row carrying both and
  `contact:x contact:y` is tagged `contact` and matching both texts. Dispatch is
  on the KEY NAME, never on the column's declared `kind` — `Glance.Web.Filter`
  does not import it. `state` is whole-value case-insensitive plus this
  producer's `state:*active*`/`state:*inactive*` meta values, `priority` is exact
  equality, `scheduled`/`deadline` are prefix, everything else is substring; so
  a column declared `badge` but named something else is matched as text, and the
  `priority` column, declared `text`, is matched exactly. That last pair agrees
  with the renderer only because the cell is one character long, where a
  substring test and an equality test cannot differ. Then two uniform rules —
  `key:` narrows nothing, a value may be quoted — and one that is NOT uniform:
  `key:none` is the empty cell on the COLUMN keys only. `tag:none` is untagged
  because `tag` is a column; on a virtual key there is no `none` branch at all,
  so `contact:none` reads as "tagged `contact` AND the row text contains
  `none`". SCHEMA.md's blanket phrasing overstates that; the renderer agrees
  with the code rather than with the schema. The tags column's key is
  `tag`, singular, so the key a filter names and the tags it names read alike
  (`tag:travel`); the header stays `Tags` and `hrSearch`'s field order is
  unchanged, since only the name moved. The virtual keys are the store's org tags (`Glance.Web.Store.stTags`,
  counted per tag beside the rows so a query costs no fold over 13k rows):
  `TAG:text` is tagged whole-`TAG` *and* matching text, an empty value being
  presence alone, and a column shadows a tag of its name. Two consequences to
  keep: a predicate reads one `\x1f` field of `hrSearch` rather than
  re-deriving a cell, so per-cell matching and free text agree by construction;
  and the vocabulary moves only when rows do, which is exactly when `guarded`
  moves the generation the `ETag` spells, so a cached answer can never be one
  the old vocabulary produced. Evidence: `TestFilter` (tokens, predicates,
  virtual keys, shape, degenerate parity with `matchesSearch`), `TestServe`
  "GET /headlines filter and paging". **test**
- **There is no schema revision mechanism, so parity is discipline plus one
  tripwire.** Nothing versions the agreement between this producer and
  `table-view.js`: no capability handshake, no schema version in the view
  document, no negotiation. Both sides read `SCHEMA.md` and are kept term for
  term by hand, and the only runtime check is the shell's parity tripwire —
  loose and one-directional (its own entry below). Every divergence
  listed in the entries that follow is therefore silent by construction: the
  same query answers differently depending on which half evaluated it, and
  neither suite can see it, because each tests its own half. Read them as a
  standing list to re-check whenever either file moves. **none**
- **Column lockstep is three-way, and `hrSearch`'s field order sits outside
  it.** `Glance.Query.viewColumns` is the single source for three things — the
  `columns` array, `rowJSON`'s cells, and `filterKeys`. The fourth thing that
  must agree, the order of fields inside `hrSearch`, is a hand-written
  positional list in `recordOf` and is NOT derived from `viewColumns`. The guard
  that looks as though it closes the loop does not: `TestFilter`'s layout group
  compares `hrSearch`'s fields against its OWN hardcoded list, so it catches
  `recordOf` drifting from the test and is blind to `viewColumns` drifting from
  both. Reorder `viewColumns` alone — swap `title` and `tag`, say — and
  `filterKeys` and `tagsColumn` move while `hrSearch` stays put; every predicate
  then reads the wrong `\x1f` field, and the suite stays green. Five places move
  together: `viewColumns`, `recordOf`'s `searchTextOf` list, `Filter.dateKeys`,
  `Filter.keyTest`'s name switch with `tagsColumn`, and the test's own list.
  **none** (the guard exists and does not guard this)
- **Arity is chosen by NAME here and declared to the renderer.** The server's
  multi-valued column is `tagsColumn`, the index of the key literally named
  `tag`, and the `tag` column now emits `"multi": true` — SCHEMA's declaration,
  which the renderer prefers over its own sampling. That sampling is what the
  declaration exists to retire: `multiColumn` reads up to 40 non-empty cells and
  needs at least two shaped like `:a:b:` with none contrary, returning the FIRST
  column in view order that qualifies, so a page with fewer than two tagged rows
  found no multi-valued column at all (`tag:a tag:b` ORing where the server
  always ANDs), one cell holding an unrelated colon — a `10:30`, a URL —
  disqualified the column outright, and a column earlier in view order whose
  cells happened to look tag-shaped stole both the arity and the vocabulary.
  The verdict was re-derived on every row-set change, so it could flip between
  two pages of one session. What remains is the version skew this whole section
  is about: an asset predating the field still samples. Virtual keys are the
  other point of agreement: multi-valued unconditionally on both sides.
  Evidence: `TestQuery` "the multi-valued column says so, and it is the only
  one", plus the golden. **test**
- **Date-ness is asymmetric the same way.** The server prefix-matches exactly
  two hardcoded key names; the renderer decides per column by sampling cells for
  date shape. A loaded set with under two dated rows makes the renderer treat
  `scheduled` as text, so `scheduled:10:00` matches `2026-08-15 10:00` there and
  nothing here; conversely any other column whose sample looks dated gets
  renderer-side prefix matching that the server never applies. **none**
- **`state:*active*` / `state:*inactive*` are producer-only, and now
  discoverable.** SCHEMA.md blesses producer meta-values, and the server
  resolves these two against the record's own `#+TODO:` sets. The starred form
  is the canonical spelling — it is what org-glance calls the groups, and what
  the default view boots on — and the bare `state:active` stays an alias: the
  stars come off in `starless` before the two comparisons and NOWHERE else, so
  `state:*TODO*` is the literal badge text `*todo*`, which no cell holds. It is
  an alias on two values, not a glob; a half-starred value is literal too.
  Discovery is the `values` array the state column now ships beside its
  `badges`, holding exactly `["*active*", "*inactive*"]` — SCHEMA's own route
  for meta-values, and the reason the starred spelling is the canonical one: it
  cannot be mistaken for a keyword. The renderer still has no group logic of its
  own, so a locally-filtered table matches these as literal badge text and finds
  nothing; that half of the asymmetry is intended, since the server knows the
  keyword sets and the renderer does not. Evidence: `TestFilter` "and answer to
  org-glance's starred spelling of the same groups", `TestQuery` "and the two
  group values a filter can name". **test**
- **The two vocabularies have different scopes.** The server parses against
  `storeTags` — every tag in the tree, folded per file — so a predicate is a
  predicate whether or not any matching row is loaded. The renderer's
  `tagVocab` iterates the rows it currently holds and is rebuilt on every
  row-set change. A tag present in the store but absent from the page is
  therefore a predicate on one side and free text on the other, which is exactly
  the divergence the tripwire was built for. **none**
- **Keys are case-sensitive on both sides; values are folded on both.** The
  server tests membership of `filterKeys` and the tag vocabulary by exact
  `elem`, and every real key is lowercase — `filterKeys` are written that way
  and tags are lowercased by `tagsOfCell`; the renderer does the same with a
  `Set` over lowercased text. So `Tag:x` and `TAG:x` are predicates on NEITHER
  side, degrading to free text for the literal string. Values go through
  `T.toLower` against a haystack lowercased at load, and `toLowerCase` against
  one lowercased on the way in. Symmetric, and surprising enough to write down.
  **test** (`TestFilter`)
- **Separators are `&`, space, tab and newline — `\r` is not one.** Both
  implementations spell the predicate out character by character, and the two
  spellings are identical. A CRLF-pasted query therefore carries `\r` into the
  last token's value on both sides, as would a vertical tab, a form feed or a
  non-breaking space. Substituting a general "is this whitespace" test on either
  side breaks parity in the direction hardest to notice. **test**
- **A lone `-` empties the result set.** The scanner emits an empty token for
  it, an empty token has no key so it is free text, an empty free-text term
  matches every row, and the leading `-` negates it: nothing matches. Both sides
  arrive there by the same four steps. It is a consequence rather than a
  decision, so an "ignore empty terms" simplification silently changes the
  answer. **test**
- **`key:value` splits on the FIRST `:` or `=`, whichever comes first.** So
  `tag:a=b` is key `tag` with value `a=b`, and a body opening with a separator
  has no key at all — which is precisely what leaves `:work:` and `=code=` as
  the org text they are. A token opening with a quote skips the split entirely.
  Both sides agree, one by `T.break` and one by `min` of two `indexOf`s.
  **test**
- **The watch is the only channel that WRITES the store.** A commit writes the
  file and returns; no path through the route writes the `Hub` or the `Store`.
  It does READ them — `storeHeadline` is where the extent and the pinned digest
  come from — and that read is the point: the write is measured against what the
  store already holds. The invariant is one-directional, and stating it as "the
  route does not touch the store" is wrong in a way that makes the digest lock
  look impossible.
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
- **The HTTP surface is a fixed route table, and the method rule is uniform.**
  Each entry declares whether it needs a loaded store and whether it is
  read-only, and the load gate runs first. GET and HEAD are the whole surface
  except `POST /headline` and `POST /command`; anything else is 405, spelled as
  JSON on those two and as plain text elsewhere, so a client parsing a refusal
  gets the shape the route always uses. An upgrade aimed at any path but `/ws` is rejected rather
  than routed. **test** (`TestServe`)
- **`POST /headline` caps the body at 1 MiB, and the cap outranks the lookup.**
  The body is counted chunk-wise and a larger one is 413. Because the cap is
  checked before the id is resolved, an oversized POST to an unknown id is a 413
  rather than a 404 — the server declines to read a megabyte in order to
  discover it had nowhere to put it. **test**
- **`?limit=` is capped at 20000 and a larger one is a 400.** Absent, it serves
  the whole store, which is the mode the shell settles into; the cap exists to
  bound one request's encode, not to bound the store. It is a page-size cap and
  has nothing to do with the body limit above. **test**
- **The asset route takes ONE path segment, through `safeName`.** Rejected: the
  empty name, `.`, `..`, and any name carrying `/` or `\`. One segment and one
  guard is the whole of the traversal defence, so an asset directory laid out in
  subdirectories cannot be served without revisiting it. **test**
- **`Content-Length` comes from `sized`, and `Vary` from the gzip middleware.**
  `sized` writes the length on every JSON, HTML and plain response, the HTTP 503
  included; warp supplies it for the 304 and for `responseFile`. The gzip
  middleware writes `Vary: Accept-Encoding` on every HTTP response, 304s
  included — and NOT on the websocket rejection, which sits outside it. A client
  caching the WS refusal has no `Vary` to key on, which is harmless only because
  nothing caches a 503. **test** (the HTTP half)
- **The served pages fetch nothing off this server.** Styles are inline, the
  glue is inline, and the one `<script src>` is a file name the asset route
  resolves inside `--assets`. No CDN, no web font, no analytics — a page that
  reaches the network renders differently on a laptop in a tunnel, and this
  daemon's whole point is that the org files are local. The JetBrains Mono
  `@font-face` is the shape a resource takes here: emitted only when the assets
  directory holds the file, pointing at a bare name this server serves. Evidence:
  `TestServe` "no page this server serves reaches off it" — neither page
  contains `http://`, `https://` or `@import`. **test**
- **The shell's keymap is data, and there is ONE of it.** `Glance.Web`'s
  `keyBindings` is the one table; the page carries it as a
  `<script type="application/json">` blob — `{rows, hints, reserved, once}` — and
  its own dispatch parses that blob, so a binding cannot exist in the handler and
  not in the map. The movement PROFILES are gone, and with them a selector in the
  corner, a `localStorage` key, a `?keys=` parameter, a `setProfile` and a key
  line that had to be rewritten whenever the profile moved. What replaced them is
  two rows apiece: `n`/`p` and `j`/`k` both step a row, `f`/`b` and `l`/`h` both
  step a cell, and both spellings are live at once because a table has no text
  field to compete with. The ends are `<` and `>`, with vi's `G` beside `>`; `g`
  is `apply-default-filter`, `,` is `customize`, `o` and `!` are the open stub,
  `M` is `mark-all`, `d` is `archive-flag` and `D` is
  `org-glance-overview:delete`. No sequence is bound twice, and no complete
  sequence opens a longer one (which would leave the longer one unreachable —
  `gg` and `dd` are both gone for that reason, and nothing binds a bare prefix).
  `seq` is DERIVED in the blob rather than stored on a row: it is the keys with
  one space between them, the only notation left now that no row runs two keys
  together. Sequences and command names are org-glance's
  (`org-glance-overview-mode-map`) where org-glance has one and descriptive where
  it does not; a row with no handler is recognized in full and says what backs it
  later. `C-c` and `C-x` are claimed as prefixes only with the selection
  collapsed — the browser decides copy and cut on the same keydown — and `C-l`,
  `C-r`, `C-t`, `C-w`, `C-n`, `C-p`, `<f5>` are never claimed on their own. A
  reserved key reaches the browser UNLESS it completes a bound sequence; what the
  list actually buys is the ABANDONED prefix, a chord that opens nothing being
  otherwise swallowed as undefined. The collapsed-selection test is
  `selecting()`, one predicate over the focused field's range and the document
  selection, and it guards the generic prefix-OPENING branch rather than
  `C-c`/`C-x` by name. Each row carries `kbKeys`, `kbCommand`, a `kbScope` of
  `table`, `modal` or `any` that the dispatch filters on, and an optional
  `kbHelp` the echo widget reads when the command name does not say enough.
  Auto-repeat belongs to movement, so the keys that must run once per press are
  named by COMMAND in `ONCE` — `filter-drop-token`, `unmark-all`, `mark-all`,
  `archive-flag`, `org-glance-overview:delete` — which holds under both spellings
  of a command and takes the repeat off nothing else. The three writes are there
  for a different reason than the token strip: a held key must not be a hundred
  `/command` requests. `archive-flag` needs it most of all, since a repeat that
  survived would flag a row and archive it from ONE press, which is exactly the
  confirmation the two-press shape exists to be. `m` and `u` stay off the list on
  purpose: both advance, so a held one walks a column laying marks down rather
  than working the same row twice. Evidence: `TestServe` "Shell keymap", which
  parses the blob, compares it to a written-down map, checks the two uniqueness
  rules, asserts that both spellings of row and cell movement are present, and
  asserts that the profile machinery is absent from the page. **test**
- **`d` is dired's flag and dired's `dd`, and the flag IS the confirmation.** The
  first `d` on a row flags it for archiving and echoes `d → flagged — d again
  archives`; a second `d` on an ALREADY-FLAGGED row is `D`, and reaches `D`'s own
  handler: it archives EVERY flagged row, not the one under the cursor. Flag
  three rows and the third press takes all three. A lone flag is a set of one, so
  the single-row flow is the general one rather than a case beside it, and there
  is no sequence machinery — `d` stays one complete binding, in `ONCE` for the
  same reason it always was. There is no prompt and no
  undo to build: `u` on a flagged row takes the flag off BEFORE it touches a
  mark (it is the more recent thing a reader put there, and the one that would
  otherwise write a file) and `U` clears flags along with marks. `D` is that
  handler with no flagging press in front of it — every flagged row when there is
  one, the row at point otherwise — and both SPEND the flags they fire over.
  Spending is load-bearing rather than tidy: `setRows` keeps a
  flag whose row a filter is hiding, which is exactly what makes a flag outlive
  the refetch this write causes, so a set left standing would be archived again
  by the next press and the row at point would never be reachable again. The pill
  names the set it ran over (`D → archived (4 flagged)`, `D → archived (row)`)
  and gives that name up for the bare count when nothing landed, a set name over
  zero rows reading as a write that worked. The flags are the RENDERER's session
  state, keyed by id exactly as marks are, so a flag survives a `setRows`, a
  filter that hides its row and a page it is not on; this page keeps no set.
- **The two selections are per COMMAND, and stay apart.** `set-state` runs over
  the MARKED set (else the row at point); archiving runs over the FLAGGED set
  (else the row at point). `D` never reads `getMarked()` and `set-state` never
  reads `getFlagged()`. The reason is what each selection MEANS: a mark is the
  generic bulk selection a reader lays down to set a state over a run of rows,
  and a key that archives whatever is marked turns every one of them into a
  loaded gun; a flag is a selection made for archiving and nothing else, which is
  why the confirming key and the bulk key can share it. What the suite pins:
  `m m d` then `D` archives one row and leaves two marks standing; `m m D` with
  nothing flagged archives the row at point; and `m m d` then `C-c C-t` sets a
  state over the two MARKED rows with the flag still on and unspent; and `D`
  twice behind one flag, which archives the flagged row and then the row at
  point. **test**
- **The flag API lives on both sides, and the detection stays anyway.**
  `flagRow`, `unflagRow`, `getFlagged`, `clearFlags` and the `.tv-flagged` wash
  — warn-tinted, distinct from both the mark wash and the selection, with an
  inset edge on the mark box so a flagged row under the cursor keeps saying it is
  flagged — landed in `table-view.js` at 079fa20, which is what `d` and `D` are
  wired to. The shell still feature-detects the pair rather than assuming it: an
  asset predating them echoes `this table-view.js has no archive flags` and
  writes nothing, and `D` there falls through to the row at point. Evidence:
  `TestServe` "Shell marks" — the two-press flow, the synthetic auto-repeat burst
  that archives nothing, both flag-clear paths, and the bare-asset case — and
  "Shell commands" for the flags-versus-marks split. **test**
- **`*word*` is the reserved-meta form.** A starred word marks a value with
  semantics of its own — never a literal keyword, never a cell value a file could
  hold. The family today: `state:*active*` and `state:*inactive*`, the filter's
  group metas, evaluated by the producer; and `*clear*`, the state palette's
  entry that takes a keyword off, committed as a null keyword. A future meta
  joins the family by wearing the stars. The convention is ENFORCED from two
  sides rather than by a rule of its own: `setStateEdits` refuses any word the
  row's file does not declare in `#+TODO:`, and `Data.Org.Parser.keywordTextP`
  admits letters and underscores alone, so a starred word cannot be declared and
  therefore cannot be set — a guard against the group names inside `configEdits`
  would be unreachable code. `Glance.Web.Filter.starless` strips ONE matched
  asterisk pair before the two meta comparisons and nowhere else, so
  `state:active` is an alias and `state:*TODO*` is a literal that matches
  nothing. Evidence: `TestConfig` "what a layer may say, and what it may not",
  `TestQuery` "Commands", `TestServe` "the last choice clears the keyword rather
  than setting one". **test**
- **KNOWN GAP (open): completion-beats-reserved is the PAGE's half, and `C-c
  C-t` is dead in the browser anyway.** The claim this entry used to carry — "a
  reserved key reaches the browser unless it completes a bound sequence, which
  is what keeps `C-c C-t` working" — is true of the dispatch and false of the
  result. Chromium (and Firefox) handle `Ctrl+T`, `Ctrl+N` and `Ctrl+W` in the
  browser process, above the document: the page either never receives the
  keydown or receives one whose `preventDefault` is ignored, so the second chord
  of `C-c C-t` cannot complete the sequence and the value palette never opens.
  `C-x C-s` is unaffected because `Ctrl+S` is a page DEFAULT ACTION rather than a
  browser shortcut, and that is the whole of the difference. What the page owes
  is pinned: `TestServe` "the completing chord is claimed, reserved or not"
  drives `C-c` then `C-t` through the node harness and asserts the palette rises
  AND that both chords were `preventDefault`ed — the harness records them now,
  where before it discarded the call. Symptom to recognise live: a new tab
  opens, and the echo pill says `C-c - timed out` two seconds later. The fix is
  a second chord the browser does not own (an alias row, e.g. `C-c t`), which is
  a keymap decision rather than a bug fix. **test** (the page's half) / **none**
  (the browser's)
- **Row marks belong to the renderer.** `mount` asks for them with `marks:
  true` and the renderer does the rest: the leading checkbox column, the wash on
  a marked row, and a set of ids that keys them — which is why a mark outlives a
  `setRows`, a filter that hides its row, a page it is not on and a re-sort, and
  why this page keeps no set, no count and no membership test of its own.  A
  command asks `getMarked()` at the moment it runs, which is the opposite of
  keeping a copy: the suite's must-not-appear list forbids a set or a count
  here and not that call. What
  the shell owns is the keys: `m` toggles and echoes the state `toggleMark`
  answers with, `u` toggles and immediately puts back anything that turned a
  mark ON — so it can only ever clear, and the flip is never drawn, the renderer
  coalescing its painting to a frame — both then `selectStep(+1)` so the key
  that marks is the key that walks, `U` clears, and `M` is `markAll()` — the
  renderer's call because the SET is the renderer's, so a page it is not showing
  is marked too and a shell-side loop over `getVisible()` would be the wrong
  answer. The detection is one name, `toggleMark`, for the original four (they
  landed in one renderer release, so no asset can carry a subset) plus
  `markAll` for `M`; one predating either echoes `this table-view.js has no
  marks` / `… no mark-all` rather than throwing. Evidence: `TestServe`
  "Shell marks", which drives the keys through the node harness — including a
  handle the acts strip the calls off. **test**
- **The mount asks for no per-row action hints.** `actionHints: false`: the
  renderer's own per-row hint said RET materializes, which the resident key line
  under the table already says and says for every command rather than for the
  one. One place, asserted through the harness rather than by grepping the glue.
  **test**
- **The echo widget's key hints are data too.** `Glance.Web.keyHints` is a table
  of key-list/label pairs serialized into the same JSON blob the dispatch reads,
  under `hints`, and rendered into the resident key line from there. So the line
  cannot offer a key nothing is bound to, and a new binding that should appear
  in it is one table entry rather than an edit to a string. **test**
- **One fetch is in flight at a time.** A single `AbortController` is aborted
  and replaced by whoever asks next, so the background full-set pull yields to a
  filter commit instead of racing it, and a late response is discarded by
  comparing the query it was asked for against the query in force. Without the
  abort, the whole-set answer lands after the filtered one and paints the
  unfiltered table under an applied filter. **none**
- **A row frame under a filter schedules a refetch 250 ms out.** Only the server
  knows whether a changed row still matches, so the shell re-asks rather than
  splicing — and coalesces, since a burst of saves would otherwise be a burst of
  whole-set requests. Unfiltered frames splice straight into the renderer.
  **none**
- **The shell's z-index bands must clear the renderer's.** Four values, all of
  them here: echo `2`, corner `3`, modal backdrop `100`, sheet `101`.  The value
  palette shares that pair rather than adding to it (`#modal,#prompt` and
  `#pbox`), so a second overlay costs no band. The
  cross-repo constraint is the backdrop pair clearing the renderer's sticky
  header (`1`) and its completion list (`5`) — an unnumbered backdrop painted
  under both. The corner and the echo sit BELOW the backdrop deliberately, so
  they dim with the page; a consequence worth knowing is that they also sit
  below the completion list, which paints over them. The filter palette carries
  no shell z-index at all: the overlay is entirely the renderer's, and the suite
  forbids this page naming its parts. **test** (`TestServe` pins the four
  values)
- **`--g-border` and `--g-sel` are hand-copied literals.** `--g-border` is the
  renderer's own `--tv-border` written out (`#E3E6EA` light, `#2A2D3D` dark)
  rather than read through `var()`, because the shell's frames have to match the
  table's hairline and danneskjold's border faces framed each element at 1.8:1
  instead of receding. `--g-sel` is likewise a literal, and danneskjold's rather
  than the renderer's. The cost is real and worth stating: a renderer border
  change needs a matching edit here, and nothing detects the drift. **none**
- **The renderer internals this page may touch are enumerated by the suite, as
  must-not-appear lists.** There is no allowlist comment; the enforcement is
  negative. The shell may not name `closeFilter`, `tv-veil` or `tv-panel` (the
  palette is the renderer's); may not reach rows by `tr.click()`,
  `scrollIntoView` or `rowEls(` (the DOM-walking path is gone); and may not keep
  a column of its own under any of the names that path used. What it does touch
  is `.tv-root`'s font, `.tv-chips`/`.tv-chip` under a coarse pointer, and the
  selected-row read kept as the legacy-asset fallback. **test**
- **Every optional renderer capability is feature-detected before use.**
  `parseQuery`, `stripLastToken` together with `getQuery`, `selectStep`,
  `nextPage` together with `pageInfo`, `getSelection`, `openFilter`, plus
  `matchMedia` for the coarse-pointer branch. The one exception is deliberate:
  `initialQuery` is passed to `mount` unguarded and detected AFTERWARDS by
  asking `getQuery()` whether it took, because an option an older asset ignores
  cannot be probed for. An asset that drops a capability degrades to the older
  path rather than throwing. **test**
- **The ownership split: the renderer owns the selection, both halves of it.**
  The shell keeps no cursor — the row id comes from `getSelection().id` and the
  column from the same call, and the DOM read of the selected row survives only
  as the fallback for an asset without it. That is what makes a selection
  ride along with row movement and clear when the renderer clears it, and it is
  enforced negatively by the suite's must-not-appear list. **test**
- **The page never scrolls; the boxes inside it do.** `body` is `100vh`,
  `overflow:hidden`, a flex column of table, log and key line. The table takes a
  fixed share, the log takes what is left (`flex:1 1 auto`) and scrolls inside
  itself, and the key line is `flex:none` and scrolls sideways rather than
  wrapping. So an arriving message moves neither the corner nor the key line,
  which is the whole point: the two pieces of chrome a reader looks for hold
  their places. **test**
- **The log strip is APPEND-ONLY, and `append(scope, severity, message)` is the
  whole of its interface.** A line is `HH:MM:SS SEV scope message`: the stamp
  muted, the severity in colour (`info` muted, `warn` `--g-warn`, `error`
  `--g-bad`) and worn as the line's class as well as spelled in it, the scope one
  word out of a fixed six — `ws`, `sync`, `cmd`, `filter`, `config`, `boot`. The
  parts are spans so each can carry its own colour, and a message's control
  characters collapse to spaces, so an entry is one line whatever it was handed.
  Nothing clears the strip: the boot line is an ordinary `boot info` line rather
  than a placeholder the mount takes away, which is what makes a page's first
  second still readable an hour later. Two rules bound it without taking anything
  back. The ring holds `LOGCAP` = 500 and drops the OLDEST past it — a reader
  scrolled back is reading the recent past, so the far end is the end to lose.
  And a line identical to the one before it (same scope, severity and message)
  bumps a `×N` counter on that line instead of appending: the ONE mutation an
  append-only strip allows, and what keeps a retry loop from filling the ring
  with a single sentence. The end is scrolled to unless the reader has scrolled
  up, which is a place they are holding on purpose. Evidence: `TestServe` "Shell
  log", which drives the widget through the keys and asserts what the strip
  holds; the ring's cap is past what any key reaches a line at a time, so the
  harness has an act that appends straight into it. **test**
- **A write names the rows it landed on, and the pill counts them.** `d` logs
  `headline "TITLE" marked for deletion` and `u` the unmarking; every archived
  row logs `headline "TITLE" archived` and every state that landed
  `headline "TITLE" → KEYWORD`, the clear reading `state cleared`. Bulk is one
  line per ROW rather than per request, since a set spanning three files can come
  back two-thirds applied — the count in the pill cannot say which third. The
  title is the renderer's `displayText` over the row's title cell, out of the
  rows in hand (the page on screen, then the unfiltered baseline behind it), and
  a row in neither is named by its id. Refusals stay one `cmd error` line
  carrying the server's own words. **test**
- **Every touch-device rule lives in ONE `@media (pointer:coarse)` block.** The
  chip row as a 44px tap target, its empty-state label, and the sheet's 16px
  textarea that stops iOS zooming in and never zooming back out. Keeping them in
  one block is what makes "a mouse sees none of this" checkable by reading a
  single place, and the tap handler asks the same query before it runs. **test**
- **The server closes a socket for exactly two reasons, and the client answers
  them differently.** `resync` when a bounded mailbox fills — named for what the
  client owes, since the backlog behind it is gone and one `/headlines` carries
  everything it would have said — and `view-changed` when the columns move.
  Those two strings are the whole vocabulary of a server-initiated close. Only
  `view-changed` remounts. Everything else — `resync`, a restarted daemon, a
  dead network — revalidates `/headlines` for the applied query against the tag
  the last answer carried, re-attaches, and leaves the page standing: the sheet
  open, the palette up, the selection where it was, the URL untouched. 304 means
  the rows on screen are still the answer; 200 replaces them under the same
  mount. The old arrangement — one door for both, `start()` — is what a user
  reported as "a periodic page refresh resetting filters and popups", since an
  Emacs bulk write overran the mailbox every few seconds and each overrun cost
  the mount. Evidence splits by half. The CLIENT's two answers are pinned by
  `TestServe` "Shell reconnect", where the harness delivers each reason and the
  mount count is what the cases assert. The SERVER's two strings are `pump`'s
  literals, reachable only over a real socket, and are live-verified by the
  storm under the mailbox invariant; a suite-level claim on them would be a
  string search over Haskell. **test (client) + live (server)**
- **A cheap reconnect still checks the columns.** The reason a socket closed is
  not enough to decide the mount can be kept: a daemon restarted while the page
  was away had no socket to send `view-changed` down, and its palette can have
  moved anyway. So a 200 on the revalidation compares the fetched columns to the
  mounted ones — whole, by `JSON.stringify`, because the state column's badge
  palette rides inside them and a key-by-key check would miss it — and takes the
  remount door when they differ. A 304 needs no check: the generation did not
  move, so nothing in the view did. **test** (`TestServe` "columns that moved
  rebuild the mount, close reason or none")
- **A real remount carries the sheet and the palette across it.** The table is
  `#app`'s and goes when the mount is replaced; the palette is the renderer's
  chrome inside it and goes with it; the sheet is a SIBLING of `#app` and
  survives by where it sits — which is a fact about the layout rather than a
  promise, so the shell stashes and restores both explicitly and depends on
  neither. What is stashed is unsaved work only: a dirty sheet's `{id, text,
  digest}`, and the palette's typed text when its field has focus, which is the
  whole of what the shell may know about an overlay whose lifecycle is the
  renderer's. The restore re-reads the sheet's digest with a `GET` rather than
  carrying the stashed one over: a file that moved while the mount was rebuilt
  lands at `conflict`, where `C-x C-s` overwrites deliberately and `ESC`
  discards. Flushing against a digest the page merely remembers would be the
  silent overwrite that flow exists to stop, and dropping the text would be the
  loss. **test** (`TestServe` "view-changed mid-edit rebuilds the mount and keeps
  the sheet's text", "a sheet restored over a moved file lands in the conflict
  flow", "Shell palette")
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
- **Two keys write without a sheet, and neither asks for confirmation.** `D`
  archives over the FLAGGED set and `C-c C-t` sets a state over the MARKED one,
  each falling back to the row at point — dired's rule, and org-glance's. They
  are `POST /command`: the page sends row ids and a name, the server computes the
  spans, and the table is not touched at all, the rows arriving over the socket
  once the watch has re-read the files. There is no confirmation step and there
  should not be: the drift lock is the safety, `D` archives rather than deletes,
  and org-glance's own rhythm is a key that acts. The pill counts what landed
  and the log names every row — a line per landing and a line per refusal, which
  is what a per-file answer needs: a set spanning three files can come back
  two-thirds applied.
  `D` keeps org-glance's command name, `org-glance-overview:delete`, and earns a
  `kbHelp` because the name is wider than the behaviour. Evidence: `TestServe`
  "Shell commands", which drives both keys through the node harness and asserts
  the bodies they posted. **test**
- **The value palette is the shell's own, and the filter's is still the
  renderer's.** `C-c C-t` raises `#prompt`: the state column's `badges` plus a
  `clear` entry, typed to narrow, `C-n`/`C-p` and the arrows to walk, `RET` to
  commit. It is a second overlay rather than a reuse of `openFilter` because
  that one belongs to the filter and this page may not reach into its chrome —
  the same must-not-appear list that forbids `tv-veil` forbids driving it. What
  it offers is `badges` and never `values`: the two group meta-values are filter
  vocabulary, no file declares one, and offering a value the server will refuse
  is worse than not offering it. Its keys sit in a SECOND document listener
  registered behind the dispatch, which is safe rather than lucky: with its
  field focused `typing()` has already made every `table` row dead, so the only
  row that can have fired ahead of it is `ESC`, which is the one that should —
  `cancel` closes whichever overlay is up, prompt first. `unask` blurs as well
  as hides, since a focused field nobody can see would leave `typing()` true and
  swallow every key after it. **test**
- **The materialize sheet has no buttons, and closing it is the save.** Dirty is
  either pane against what the file holds as far as the page knows — the
  materialized original, then whatever the last 200 wrote — and it decides
  everything: `ESC` or a click on the backdrop flushes a dirty sheet and closes
  on the 200, while a pristine one closes with no request at all, so opening a
  subtree to read it never touches the file. `C-x C-s` flushes mid-edit and
  takes the receipt's digest as the next flush's lock, which is why a session of
  edits costs no re-materialize. A 409 keeps the sheet open at `conflict`, where
  `C-x C-s` re-reads the file's digest and posts the author's text over it —
  last writer wins, on a deliberate keystroke — and `ESC` discards. Closing the
  tab on an edited sheet flushes with `fetch(keepalive)`, and only when dirty.
  The header carries one word — `synced` / `syncing…` / `conflict` / `error` —
  because with no buttons the keys are the whole of the offer, and the two
  states that wait for one, `conflict` and `error`, each spell the key that
  clears it. A failed request lands in `error` rather than in `conflict`, so a
  409 and a dropped connection are told apart on screen. Evidence: `TestServe`
  "the sheet is buttonless and syncs on the way out", plus the curl-level round
  trip. **test**
- **The sheet is two panes over one subtree, and the cut is the server's.** The
  textarea holds `body` and a panel beside it holds `properties`, both handed
  over by `GET /headline`; a flush sends them back as `{body, properties}` and
  the server joins them. The page never looks for a drawer in the text it is
  holding — there is no org parser in this browser, and the whole point of the
  route serving both shapes is that there does not have to be. A panel row is
  two fields, key then value, in the order the file writes them — nothing sets a
  `tabindex`. `+` adds an empty property at the end and opens it: the add
  affordance is a KEY, which is what keyboard-first means here, and it replaced a
  row that was always empty and had to be filtered back out of everything the
  panel said. A row whose key is emptied is a property deleted.
  `ORG_GLANCE_ID` is in NEITHER pane: it is the row id the table keys its updates
  off, the server keeps it out of what it hands over and puts it back verbatim
  (`hiddenProperties`), so there is nothing here to warn about and no note to
  draw. The three planning entries are FIXED rows at the head of the same list —
  `SCHEDULED`, `DEADLINE`, `CLOSED` in org's order, key uneditable because org
  owns it, value the timestamp text verbatim, empty meaning absent — so clearing
  all three is how the line comes off. The logbook is a read-only strip under
  both panes: full width, muted, not focusable, out of Tab and out of `dirty()`,
  and out of every write.
  `C-c '` — org's `org-edit-special` rhyme — swaps the sheet between the two
  panes and the raw subtree, and does it by RE-MATERIALIZING: a dirty sheet is
  refused with the key that would let it through (`sync first — C-x C-s`),
  because a re-read cannot carry unsaved work and converting locally would need
  exactly the parser this design keeps out. The re-read is a fresh materialize,
  so it also lands the sheet at `synced`. Both panes are stashed and restored
  across a remount, in the shape the sheet was showing, with the baselines
  staying the file's so what was dirty stays dirty. The panes wrap rather than
  querying a width: `flex-wrap` puts the panel under the text when there is no
  room beside it, which is the same answer a breakpoint gives and is one less
  place to keep in step — the `pointer:coarse` block pins the column outright,
  since a thumb wants the text full-width whatever the tablet is. Evidence:
  `TestServe` "Shell sheet" (the node harness: two panes, growth, deletion, the
  identity note, both toggle directions, the dirty refusal, the remount) and "the
  sheet is a body pane and a property panel". **test**
- **The property panel is modal: nav moves over read-only rows, `RET` opens one
  for editing, and `TAB` crosses the panes.** The keys are a second document
  listener behind the dispatch, the way the value palette's are and safe for the
  same reason: while the panel holds the keys `typing()` is true, so every
  `table` row is dead and nothing here takes a key the map wanted. In NAV the
  rows are read-only text — spans, not fields, with nothing focusable in them —
  and one wears the cursor (`pcur`, class `pat`, painted only under
  `#mprops.on`). That is what pays for the movement being plain letters: `n`/`p`
  and `j`/`k` are both bound, unconditionally and under either profile, because
  a row with no field in it leaves every printable key free and satisfying both
  editors at once costs nothing; the arrows need no profile at all. Entering the
  panel BLURS the textarea and raises `pnav`, and `typing()` counts `pnav` as a
  focus of its own — without that, nothing is focused and the table's own
  letters move rows under the open sheet. `RET` opens the row at point: its
  cells become fields, the value taking the focus because editing a property
  that is there is almost always editing its value, and the key taking it where
  there is none yet. A planning row opens its VALUE whatever it holds, having no
  editable key. Inside an open row `TAB` is the hop between its two fields and the
  pane crossing is suspended, since one row and two fields leave it nothing else
  to mean. `RET` commits — the row takes the text its fields hold — and `ESC`
  cancels, putting back the text the row was opened on. A row HOLDS its committed text and `props()` reads that rather than
  the fields, so an edit nobody committed is not dirty and cannot be written;
  the commit is the thing that means yes. `ESC` runs through the keymap's
  `cancel`, which tries the open row before the sheet, so the sheet's own ladder
  only ever sees the key from nav. `TAB`/`S-TAB` is one toggle rather than a
  direction each — there are two stops, so a direction says nothing — between
  the body and the panel's cursor, which is where it was left; `shut` clears
  `pnav` and `pedit`, so the next sheet opens read-only at its top.
  `preventDefault` fires exactly where one of those bindings does, and only over
  an open subtree sheet — raw mode has one pane and nothing to cross to, so `TAB`
  is the browser's there, and the settings sheet keeps native tabbing. The
  planning rows are the same two modes over the same kind of row and belong in
  this list rather than a second one. Evidence: `TestServe` "Shell sheet" (the
  crossing and its remembered cursor, `S-TAB` parity, nav movement on all three
  pairs with the table's own row staying put under it, `RET` opening value-first,
  `+` adding and opening a row, `TAB` hopping the open row, the commit, `ESC`
  restoring the row and the next one closing the sheet, an open row not counting
  as an edit, an emptied planning row taking its entry off, raw mode leaving
  `TAB` alone, and the reset on close). **test**
- **The whole page wears danneskjold, through one `--g-*` palette.** Surface,
  text, muted text, border, selection, warn and bad are declared once and
  re-declared per theme, and every `var()` on the page reads one of them, the
  monospace stack, or the sheet's own. The sheet keeps exactly ONE variable of
  its own, `--dk-mono` (Hack first); it stopped carrying a private `--dk-*`
  palette when the page grew one. So "the sheet alone wears the author's theme"
  is the old arrangement, and a change to the page's colours is one block rather
  than two. **test** (`TestServe` pins the declarations)
- **The theme is a `data-theme` handshake with a pre-paint boot.** `themesel`
  offers `auto`, `light` and `dark`. `auto` follows `prefers-color-scheme` and
  is the default, and choosing it REMOVES the attribute rather than writing a
  value; the other two stamp `data-theme` on the document element, and the
  attribute rules are written so they beat the media query in both directions.
  The choice lives in `localStorage` under `glance-theme` — distinct from the
  keymap's `glance-keys` — and `themeBoot` reads it and stamps the attribute in
  `<head>`, before the first paint, because a dark page that resolves its theme
  after paint flashes light. `themeBoot` is emitted on one unindented line so
  the suite's glue extractor, which finds the shell's inline block by a
  newline-plus-indent delimiter, cannot mistake it for that block. **test**
- **The status corner holds two things, in order: the dot and `themesel`.** The
  dot carries `live` / `wait` / `down`. The selector is a native `<select>`, so
  Tab reaches it and its own arrows walk it without a chord — and a focused
  `SELECT` counts as typing, which is what stops the keymap eating those arrows.
  The order is asserted, since the corner is the one piece of chrome a reader
  navigates by position. The keys picker went with the profiles. **test**
- **The applied filter query is in the URL, and `DEL` is its backspace.** A
  commit writes `?q=` with `replaceState` and leaves `keys` where it is, so a
  filtered view is a link, a reload keeps it, and a remount comes back to it
  rather than to the whole store. `DEL` over the table drops the query's last
  token — through the renderer's `stripLastToken`/`getQuery`, never by
  recomposing the string here: the committed tokens are chips the renderer
  draws, and a shell-side strip would leave them on screen spelling a filter
  that is no longer applied. An asset without the pair says so instead of
  growing a second implementation. **test**
- **An EMPTY applied query is written too, and that is what makes it intent.**
  `remember` sets `q` unconditionally, so clearing the filter leaves `?q=` —
  present and empty — where deleting the parameter would leave the same URL a
  page nobody has filtered opens on. `bootQuery` reads exactly that difference:
  absent gets `state:*active*` injected, present gets left alone whatever it
  holds. The two are indistinguishable the moment the parameter is deleted, so
  `DEL`-ing the last chip and then hitting any remount put the default straight
  back — the "filters reset themselves" half of the reported bug, and the half a
  cheap reconnect alone would not have fixed, since a reload is a remount
  nothing can make cheap. **test** (`TestServe` "DEL over the table strips the
  default and shows everything" settles on `?q=`, and its deep-link twin on
  `?q=&keys=vim`; the `?q=` boot rows pin the absent-vs-present read)
- **A filtered answer of zero to a virtual key is checked against the rows the
  page holds.** The renderer suggests keys from the vocabulary it derives; the
  server parses with the vocabulary it derives; if the two are different
  versions the suggestion is a query the applied path evaluates as plain text
  and answers with nothing — which is what a user hit live (`task:tanik`,
  19 suggested, 0 returned). So the shell keeps the last unfiltered answer and,
  when the server returns 0 for a query carrying a `key:value` its columns do
  not name, counts locally: if the words are in the rows, it says
  `filter parity divergence — asset\/daemon version skew` and logs both counts.
  One-directional and deliberately loose — it reports a suspicion and corrects
  nothing, because guessing which half is right is how the two drift.
  Column predicates are excluded from the check: both halves read the columns
  out of the same view document, so they cannot skew. **test**
- **KNOWN GAP (open): the tripwire cries wolf, though it no longer arms late.**
  The baseline is an unfiltered answer, and the remembered set is still assigned
  in exactly one place — a paint with no query. What changed is that a filtered
  boot no longer leaves it empty: `arm(total)` fetches the unfiltered set once,
  behind everything else, keeps it without painting it and re-runs the check
  against the total that was painted before there was anything to check it
  against. It is bounded to one fetch per page (`all.length` guards it) and runs
  under a `?q=` link and under the default view alike — which matters more now
  than it did, since with a default query every session opens filtered.
  Evidence: `TestServe` "Shell boot", which runs the glue under node over a
  stubbed browser and asserts the fetch sequence — the deep-link and default
  boots both end in an unfiltered `/headlines`, while an explicit empty `?q=`
  needs no arming fetch because its own paint is the baseline.

  Three limits remain, all by construction and none fixable without deciding
  which half is right. It fires only when the server returned zero, so the
  opposite skew — the server matching rows the renderer would not — is never
  reported. The local recount DROPS THE KEY and tests the value against the
  whole joined row text, so a correct empty facet answer warns whenever the word
  happens to appear elsewhere: `contact:tanik` with no `contact`-tagged rows but
  "tanik" in a title fires it. And it consults column keys alone, so every
  virtual-key predicate is treated as suspect, while its `key:value` gate also
  admits ordinary text such as a bare URL. One more source of drift sits beside
  it: unfiltered frames splice into the renderer without updating the remembered
  set, so the baseline ages over the life of the page. **test** (that it fires,
  and that it is armed) / **none** (that it fires correctly)
- **The page opens on the active view, and that is a real query.** With no `q`
  in the address bar the shell boots on `state:*active*`: it goes into the URL
  through the same `remember` every commit uses, into the mount as
  `initialQuery` so the renderer shows its own chip, and into the boot fetch, so
  the first page that arrives is already the answer to it. `DEL` strips it like
  any other token and the whole store is one keystroke away. A `q` that IS in
  the address bar is the reader's intent whatever it holds — an empty `?q=`
  included — and nothing is injected over it. The cost is that a socket frame
  now schedules a refetch rather than splicing, since a filter is on by default
  (the shell cannot know whether a changed row still matches; only the server
  can). Evidence: `TestServe` "Shell boot", seven rows over the node harness:
  the default present on a bare boot and absent under any `?q=`, the fetch each
  one makes, and `DEL` stripping the query back to the whole store while the
  rest of the URL (`keys=`) stays where it was. **test**
- Browser writes are commands over the bridge, of two kinds (proposal rev 3):
  structured commands, and raw replacement of a whole span under the same drift
  lock — materialize is the first of those. Semantic org editing stays out of
  the browser. Automation: reviewed deterministic scripts behind a separate
  privilege tier; no LLM in the loop.

## Desktop

- **The window opens at the socket, not at the loaded store.** `glance desktop`
  is `serve` with a browser window launched as soon as the listener is up, which
  is only defensible because of the bind-before-load contract above: the page
  the window lands on is served immediately, `/headlines` answers 503 with
  `Retry-After: 1`, and the shell paints its indexing state and polls out of it.
  Wait for the store instead and the user watches a blank screen for the length
  of the walk — 16 s over ~/sync — with nothing saying why. The order is fixed:
  bind, window, walk, watch. **test** (the 503 contract) / **docs** (the
  ordering)
- **Browser resolution is a fixed ladder, environment first.**
  `$GLANCE_BROWSER`, then `--browser`, then the first of `chromium`,
  `chromium-browser`, `google-chrome-stable`, `google-chrome`, `brave`,
  `vivaldi` found on `PATH`, each run as `CMD --app=URL`. The environment leads
  the flag on purpose: a machine whose browser is on none of those lists is set
  up once in a shell profile and obeyed by every launcher. What it names has to
  be chromium-family, since `--app` is the flag that drops the chrome and
  Firefox no longer has it. Failing all of that, `xdg-open URL`; failing that,
  the URL is printed. **No window failure ever fails the daemon** — the server
  is the product and the window is a convenience, so every step degrades rather
  than aborts. **test**
- **`--dry-run` resolves and exits before binding.** It prints the command it
  would run and the URL it would open, and starts nothing, so it is the way to
  ask what a machine resolves to without taking the port. Anything that moves
  the resolution has to move this output with it, or the flag stops answering
  the question it exists for. **test**

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
  `TestQuery`/`TestServe`/`TestStore`). `glance-web` exposes five modules and
  declares no `other-modules`: `Glance.Desktop`, `Glance.Web`,
  `Glance.Web.Filter`, `Glance.Web.Store`, `Glance.Web.Watch`. It gained every
  one of them past `Glance.Web` without gaining a direction — what they needed,
  per-file loading, row JSON, the keyword merge, the derived and org path
  predicates, was added to `Glance.Query` rather than reached for behind it.
  Putting `Data.Org.*` in a web or daemon target's build-depends is impossible
  from outside the package — the S2 exit bar, enforced by the solver rather
  than by review. **test** (it would not build)
- **The suite shells out where a claim needs a real interpreter**, and degrades
  where the machine has none: `node --check` over the extracted glue, and
  `test/fixtures/shell-harness.js`, which boots that glue over a stubbed browser
  and reports the fetches it made. Both answer `pure ()` when `node` is not on
  `PATH`, so the suite is green either way and the boot contract is checked
  wherever there is something to check it with.
- **WATCH (2026-07-31): a test run hung once during the mutation pass and has
  not reproduced.** Not seen again across the batches since, under `cabal test`
  or `-p`; nothing in the suite waits on a socket, and the two node cases are
  bounded by the child process. Recorded so a second sighting is a pattern
  rather than a surprise. **none**
