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
  wrapped around `orgParse`. Every constructor is now SPELLED OUT with no
  catch-all, so `-Wall`'s incomplete-pattern warning is the pin: the obligation
  was unenforceable while a `stripSpans e = e` arm made the function total by
  construction and a fifth constructor compiled clean and passed through
  unstripped. **compiler + test**
- **`Element` is a closed sum**, not an existential: `EHeadline`, `EPragma`,
  `ETimestamp`, `EToken`. That is what lets `stripSpans` and the `TextShow` /
  `Display` instances be written as total case analyses rather than dispatched
  through a class dictionary. Any doc calling it existential is describing a
  design that no longer exists. **none** (the type declaration is the guard)
- **Subtree extents.** `Glance.Query.hrSubtree` runs from `spanStart (hsFull …)`
  to the start of the next headline in the same file at the headline's own
  level or shallower, and to the end of the document when there is none —
  org's outline rule, computed at load in one right-to-left pass over the
  headlines with a stack, over EVERY headline the file has. Records keep the
  top-level ones alone (Walk, "A row is a top entry"), so the extents that
  survive tile: each covers its own `hsFull` and every descendant, consecutive
  ones meet exactly, and the last extent of a file ends at `T.length doc`. Two
  consequences worth stating because a materialize shows them: whatever sits
  between a subtree's last body line and the next headline's stars, blank lines
  included, belongs to the subtree above; and a file's `#+`-preamble sits ahead
  of the first extent and belongs to no subtree, so a commit cannot carry it
  off. Evidence: `TestSubtree` — five fixtures asserted as text, the geometry
  group over all of them, and the same geometry over sampled real files behind
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
- **The star run has to end.** `indentP` consumes horizontal space after the
  stars and, failing that, looks at end-of-line or end-of-input; one of the two
  has to hold. Org's rule (`org-outline-regexp` is `\*+ `): a line opening
  `*bold*` or `*TODO* [[link][x]]` is emphasis, and reading it as a headline put
  body text in the table as rows of its own — 251 corpus lines, 29 of them
  level-one and so rows, `headlines` 12884 → 12606 with the ok/failure counts
  unmoved. A bare star run keeps its old reading, an empty headline, so
  `hsFull`'s reparse and the stars-only `set-state` insert still hold. The
  newline is never consumed: `MPC.space` here let an empty title run past the
  end of its line and take the next headline's stars and text as its title
  (`* ` above `* Delta` parsed as one headline titled `* Delta`). **test**
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
- **The weekday slot takes any word.** A run of LETTERS in any script, of any
  length, read and dropped. Being display-only is what licenses the width: the
  render recomputes the word from the date, so nothing downstream can tell one
  spelling from another, and a locale's word costs the parser nothing. Exactly
  three letters was English-only and the corpus is not: ~/sync writes Dutch
  `ma`, `do`, `zo`, `vr` and `za`, each of which failed the timestamp, failed
  the planning line, and left the drawer no longer next — the headline lost its
  properties and its id whole, 28 blobs of ~/sync/views' 6063. Letters is the
  whole charset because a repeater opens with `.`, `+`, `-` or a digit and a
  time with a digit, so requiring one letter is what keeps `.+3d` out of the
  slot; the trailing dot French and Catalan abbreviate with (`lun.`) is
  therefore still refused, and admitting it needs a guard `.+3d` would
  otherwise trip. The consequence a reader sees: a Dutch stamp re-rendered by
  `TextShow` comes back English, so the source spelling survives the span
  channel alone — which is every path that matters, materialize and the lens
  slicing spans rather than rendering. **test + corpus**
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
- **Range end versus warning cookie.** Both open with `-` and only the
  time's colon separates them. The end time is tried first: `-1d` gets through
  `MPL.decimal` and fails at the missing `:`, backtracking whole and leaving
  the cookie its text. No space may sit around the `-`, or ` -1d` would read
  as an end time instead. **test**
- **Two cookies, one slot each.** A stamp takes at most one repeater and one
  warning/delay cookie (`-3d`, first-only `--3d` — org's grammar, so a lone
  `-3d` is the warning and never a minus-signed repeater; `TRSMinus` survives
  in the type unreached). `tsCookieParser` tries the warning arm first and
  `many` accepts either order, first of each kind winning; the render spells
  repeater-then-warning, so a warning-first source re-renders conventionally —
  inside TextShow's documented lossiness, the span carrying the source. Before
  the second slot, `<... +1m -3d>` failed the whole timestamp at the bracket
  and a planning line demoted to body, taking the drawer and the id with it —
  the Dutch-weekday class. **test** (TestTimestamp two-cookie cases,
  TestParser "Planning line survives a warning cookie", TestRoundtrip
  `Repeater and warning`/`First-only delay`/`Warning cookie alone`.)
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
  timestamp parser rejects — two-letter weekday abbreviations (since fixed: the
  weekday slot takes any word), unit-less
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

- **`TextShow` is lossy by design.** Whitespace collapses to single spaces and
  pragma keys uppercase. `TestRoundtrip`'s `Fidelity` column — 23 rows, all
  `Exact` — is the documented budget: promoting a `Stable` case asserts fidelity
  the renderer lacks, so a promotion has to be measured first. Seven rows were
  measured and promoted on 2026-07-31 (multiple tokens, deep indent, the
  `#+CATEGORY:` and generic pragmas, the inactive and midnight timestamps, the
  `--` date range): each already re-rendered byte for byte, and the `Stable`
  label was budgeting for losses the renderer does not have. The last one,
  `#+TODO:` re-emitting its two keyword sets in Set order rather than as the
  source wrote them, STOPPED BEING A LOSS when `PTodo` took ordered lists
  (#67) — a keyword list is the tree's declared cycle everywhere else, and the
  re-serializer got the fix for free. The budget is now empty; `Stable` stays as
  the mechanism for a case that genuinely is. None of this makes `TextShow` a
  write-back channel — the whitespace and casing losses are untouched, and spans
  are still the lossless one. **test**
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
- **`hrLinks` is the row's reference list, and what counts as a reference is a
  census.** The field holds the rows a subtree POINTS AT, cut at load through
  the `/links` scanner (`orgLinks`, so the bracket grammar stays the one
  `displayText` reads a cell with) and `T.copy`-detached like every other cell
  the row keeps. `forceRecord` forces its SPINE beside its elements: a strict
  list field forces the outermost cons alone, and a lazy tail behind it is a
  thunk over the document. What counts is `Glance.Query.refTargetOf`, and the
  rule was written off a COUNT of ~/sync's 6291 walked files (2026-08-02)
  rather than off what org permits. Kept: the id-bearing protocols
  `org-glance-visit:` (3867 occurrences), `org-glance-open:` (568),
  `org-glance-material:` (28) and `id:` (0 — org's own, on the list because it
  is org's own), each stripped to a case-preserved `ORG_GLANCE_ID`; a leading
  `*` stripped, which is `[[*Title]]` (4); and a bare target carrying neither
  `:` nor `/`, which is `[[Title]]` (18, nearly all of them bracketed prose).
  Dropped, though the first two are commoner than everything kept together:
  `org-glance-overview:` (2726) names a TAG and `org-glance-state:` (880) names
  a keyword — of their 52 and 6 distinct targets, not one is an
  `ORG_GLANCE_ID` — along with `file:`, `http` and `mailto:`, each of which
  names something that is not a row. Cost, measured as store residency over
  ~/sync (`serve` + `+RTS -s` at `-N8`): 348.0 MB before, 330.8 and 322.5 MB
  after. Both after-samples sit BELOW the single before-sample, so the honest
  reading is no measurable cost — max residency is sampled at major GCs and
  this is inside that noise. The field is ~4.5k short targets over 10433 rows,
  under a megabyte by construction. `scan` does not move either way and its
  budget is NOT the number to quote here: it is a parser oracle off `orgParse`
  and builds no records at all. Evidence: `TestQuery` "Reference targets".
  **test** (the rule) / **docs** (the census and the residency, which no test
  measures)
- **`hrLinked` is the WIDER question the same scan answers, and it is the one
  the wire carries.** One `orgLinks` call per subtree at load feeds both fields:
  `hrLinks` keeps the references among those links and `hrLinked` says whether
  there were any. `rowJSON` emits `"linked": true` off the second and NOTHING
  off it when it is false — sparse, so a row with nowhere to go is the row it
  was before the field existed, which is what makes it additive under
  `table-view/SCHEMA.md`'s unknown-fields rule. The two fields are far apart and
  the wide one is what the underline means: over ~/sync at 2026-08-02, 4976 of
  10433 rows carry a link and only 1824 carry a reference, so a `linked` off
  `hrLinks` would leave 3152 rows unmarked that `o` opens. Every reference is a
  link, so nothing is marked that `GET /links` would answer empty for.
  The renderer's half is the `title` cell's underline, which is a decoration
  rather than a ground and therefore contests none of the row states.
  Evidence: `TestQuery` "Links" (the two fields over one subtree) and "Schema
  conformance" (the sparse wire shape, plus the golden). **test**
- **KNOWN LIMIT: a link nested in another link's DESCRIPTION is a reference at
  neither end.** Inherited from the `/links` grammar rather than introduced
  with `hrLinks`. The outer link fails to close — its description breaks at the
  inner link's first `]`, leaving `][` where `linkAt` wants `]]` — and the
  rescan picks the inner one up one bracket late, so its target arrives spelled
  `[org-…` and is refused for the leading bracket. org-glance's own "Referred
  from" footer writes exactly that shape, so the miss is live rather than
  hypothetical. Measured on the corpus: for the most-referenced contact in
  ~/sync, 126 of the 128 files holding the link answered (2 of those archived,
  and hidden by the default view), and both misses are this. The scanner is
  reused on purpose — a second one would be a second bracket grammar to keep in
  step with SCHEMA.md's link rule. Evidence: `TestQuery` "a reference nested in
  another link's description is not found", which pins the behaviour as known
  rather than fixing it. **test**
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
- **The scan folds org-glance's index and says where it disagrees with the
  blobs.** `Data.Org.Index` reads `.org-glance/meta/` — read only, and the only
  thing in this repo that reads that directory at all. One file under it is
  WRITTEN, by `Data.Org.External` and by nothing else: `EXTERNAL.jsonl`, the
  notification the write path leaves (Architecture, below). WHICH STORES: each root's
  own `<root>/.org-glance/meta`, plus every `meta` directory the walk DECLINED
  (`foundDerived` already holds them, so a store nested anywhere under a root is
  found without a second traversal). The roots are asked separately because
  `--include-derived` walks the mirrors instead of declining them; a nested store
  under that flag is the one shape this misses. No meta directory, no line at
  all.

  THE FOLD, which is `org-glance-graph--latest-records` and `--ensure-cache`
  (`src/data/org-glance-graph.el`) read forwards: the MANIFEST's sealed segments
  oldest-first, then the open `headlines.jsonl` LAST; every non-empty line one
  JSON record; the LATEST record per `id` supersedes every earlier one; an id
  whose latest record carries `tombstone` leaves the live set, and a later
  ordinary record brings it back. A name the MANIFEST lists is opened only when
  it spells `seg-<digits>.jsonl` — org-glance's own segment pattern, which
  doubles as the traversal guard — and a segment on disk the MANIFEST does not
  name is invisible, the MANIFEST rename being the format's sole commit point.
  Only the OPEN segment's final line may be torn (a crash tears the last append
  and nothing else); the elisp re-signals on any other parse failure and a
  read-only instrument counts it (`ifMalformed`) and carries on instead.

  THE COMPARISON is by `ORG_GLANCE_ID` against the blob at
  `.org-glance/data/<2>/<rest>/data.org`, in two terms: the TODO keyword always,
  and the archive flag only where the record CARRIES the key — `archived` joined
  the record schema late and 6024 of ~/sync/views' 6071 live records have no such
  key, so absent is a third answer rather than false. Elisp writes `nil` as `{}`
  and the decode is `(eq t VALUE)`, so only JSON `true` is a set flag. A blob's
  entry is its file's FIRST headline: first rather than level-one (six of the
  corpus's blobs open at level two) and first rather than first-with-an-id — a
  CHILD's id is not the blob's, and reaching past a headline whose drawer this
  parser lost would compare that child's keyword against the parent's record.

  Report: `org-glance index: N rows disagree (M state, K archived)`, the store,
  the fold's counts, the blob counts, the unmatched pair, then up to ten
  disagreeing ids with both values. Measured on ~/sync/views at 2026-08-02: 6502
  records read, 6071 live, 0 tombstones, 0 malformed; 6063 blobs parsed of 6071
  files; **21 rows disagree, 20 state and 1 archived**; 0 unindexed blobs and 59
  records without blobs.

  THE INSTRUMENT REPORTS ITSELF: `dfIdless` counts blobs this parser read and
  found no `ORG_GLANCE_ID` in, which with the parse failures accounted for every
  records-without-blobs the run reported — so none of that number was org-glance
  indexing something that is not there. Their causes were this parser's, and the
  count is what named the largest: 28 blobs carried a non-English weekday in the
  planning line (`CLOSED: [2025-12-04 do 22:34]`), which failed `planningP`, so
  the drawer was no longer next and the headline lost its properties whole.
  Without the count a parser gap would have read as index lag.

  THE GAP IS CLOSED and the same count is what says so. The weekday slot now
  takes any word (see Parser, above). Over ~/sync/views at 2026-08-02, before and
  after that one change: idless blobs **49 → 21**, records without blobs
  **57 → 29**, both moving by exactly the 28 the instrument had named; blobs
  parsed, rows disagreeing and the 8 parse failures all unmoved, since none of
  them was the weekday's. The figures in the paragraph above are the run that
  found it, and predate the fix. What is left of the 21 has not been attributed.
  Evidence: `TestIndex` — the fold over a real MANIFEST + sealed + open store in
  a temp directory, the comparison's five outcomes, and the report's shape.
  **test + corpus**

  RE-MEASURED 2026-08-03, same store: 6503 records read, 6071 live, 0 malformed;
  6063 blobs parsed of them, 21 idless; **39 rows disagree, 38 state and 1
  archived**; 0 unindexed blobs and 29 records without blobs. Everything but the
  disagreement count stood still, and that one moved 21 → 39 in a day of ordinary
  browser use — which is the number `EXTERNAL.jsonl` exists to stop growing
  (Architecture, below). A daemon carrying the notification cannot add to it, and
  `org-glance-graph:refresh-external` takes off what it names; what is left after
  a refresh is the pre-existing 21 plus whatever a write outside both sides left.

## Keyword configuration (layered)

- **Recognition is a superset; classification is widest-scope.** The parse
  seed for every file unions `defaultContext` with `#+TODO:` sets read from
  `<root>/.org-glance/config/system.org` (when present) and
  `config/tags/*.org` (tag name = filename), so a keyword declared anywhere
  parses as a state everywhere — the STARTED-in-title misparse class ends
  here. Active-vs-inactive resolves per headline by WIDEST scope: `default`
  (org's own TODO/DONE) > system > its tags' configs (first tag wins) > file
  pragma; the palette and the `state:*active*` metas consult the
  resolver, while parse-time `Todo.active` keeps its position-dependent
  snapshot semantics. The chain is ONE list — `keywordScopes`, an entry per
  scope carrying its rank, the name it answers under and what it declares —
  with two readers and three answers: `classify` folds it for the first scope
  with an opinion, `Glance.Query.keywordSources` reports what each one claims,
  and `Glance.Query.settableStates` — the words `setStateEdits` accepts — is
  that second answer FLATTENED rather than a third fold. Org's own cycle is
  `builtinKeywords`, read off `defaultContext` rather than spelled at either
  reader, so the scope that classifies and the scope a palette shows cannot come
  to hold different words. Evidence: `src/Data/Org/Config.hs`, `TestConfig`.
  Breaks: dropping the recognition seed re-scatters foreign-keyword headlines
  into titles; flipping the precedence moves every keyword's bucket.
  **test + corpus** (`scan` reports `config keywords`)
- **A keyword list is ORDERED, and the order is the org files'.** A `#+TODO:`
  line is a CYCLE — `TODO STARTED WAITING | DONE CANCELLED` names five states in
  the order work moves through them — and that spelling is the only thing a tree
  says about how its state column sorts and how a palette draws. So every
  keyword list a reader meets is segmented by `keywordScopes` precedence
  (`default`, `system`, the tag configs in walk order, `file`) and, inside a
  segment, is that layer's own left-to-right declarations, a repeat keeping its
  FIRST place. `Data.Org.Config.recognizedKeywords` is the one rule; `hrKeywords`
  and `Store.storeKeywords` both come off it, so a file's palette and the whole
  store's cannot order the same words differently. Sets answer RECOGNITION and
  nothing else: `Context`'s `todoActive`/`todoInactive` stay `Set Text` because a
  parse asks only whether a word is a keyword, and `seedContext` builds them from
  the ordered lists at that boundary.
  Until #67 (2026-08-04) three Sets stood between the line and the palette —
  `PTodo` took `Set.fromList` at the parse, `declaredKeywords` read it back with
  `Set.toAscList`, and `hrKeywords` was `Set.toAscList` over the parse's ending
  context — so every list downstream was ALPHABETICAL and the `#+TODO:` line
  governed nothing it was supposed to. Measured on ~/sync: `scan`'s
  `config keywords` read `DELEGATED PENDING REVIEW STARTED TODO READING |
  CANCELLED DONE ABANDONED READ` and now reads
  `TODO STARTED PENDING DELEGATED REVIEW READING | DONE CANCELLED READ ABANDONED`,
  which is the tree's actual cycle. Consequences that land with it: the state
  column sorts by the cycle (`paletteRank` over `badges`), the value palette's
  which-key letters are assigned over the declared order rather than a shuffled
  one, `GET /keywords` answers ordered inside each source, and reordering a
  `#+TODO:` line is now a palette move — so it closes the socket `view-changed`
  and the table comes back in the new order. Evidence: `TestConfig` "Palette"
  (multi-layer order, repeats keeping first position, "and reordering one
  `#+TODO:` line reorders the palette"), "a reordered cycle reorders the table"
  (the whole loop: splice, watch reseed, `sortedForViewWith`), "a shadowed
  redeclaration is still in the union, in its first place"; `TestServe` "a
  source's keywords arrive in the order its line spells them"; `TestTextShow`
  "and its keywords keep the order they were declared in". Breaks: putting a Set
  anywhere on the path from `PTodo` to `badges` re-alphabetizes every tree's
  cycle, silently and with the suite green unless a fixture happens to spell one
  against the alphabet.
  **test + corpus** (`scan` reports `config keywords` in declared order)
- **Widest-first is the DEFERRED BOUNDARY, and it inverts what a file's own
  `#+TODO:` buys.** The chain ran file > tags > system > builtin until
  2026-08-02 and now runs default > system > tags > file, for `classify` and
  for the `/keywords` draw alike — one rule, read forwards. What the order buys:
  the scope every reader of a tree shares settles a word once, and a narrower
  scope EXTENDS the vocabulary without redefining it, so `TODO` means what org
  says it means in every file of every tree and `system.org`'s cycle means what
  the tree says in every file of it. What it costs, stated honestly: a file
  redeclaring a word a wider scope already settled keeps the word RECOGNIZED and
  loses the redefinition — `#+TODO: | TODO` no longer makes that file's `TODO`
  rows done-like, and a `book`-tagged row's `READING` answers to `book.org` over
  its own file's line. SETTING is untouched in content: `settableStates` is the
  chain FLATTENED and a union has no order, so every word a row could be put
  into before it can be put into now; what moved is which source shows it, which
  is a DISPLAY change with a write-legality footprint of exactly zero. Evidence:
  `TestConfig` "the tag config outranks the file's own pragma", "org's own TODO
  and DONE outrank every layer under them", "a file redeclaring a wider scope's
  word does not move its rows"; `TestQuery` "and the reorder moved which source
  shows a word, never the set"; `TestServe` "a file redeclaring a wider scope's
  word gets no row of its own". Breaks: putting file first re-opens the private
  opinion about a public word. **test**
- **The recognition union is NOT a scope, and that is the whole of what it is
  not.** `clSeed` feeds `seedContext` and nothing else: it is absent from
  `keywordScopes`, so no headline is classified by it, no `/keywords` answer has
  a `union` row, and no row is settable to a word it alone reaches. It stayed a
  scope until a reader noticed that another tag's cycle was offered on — and
  written to — a row carrying no such tag, which is a keyword the file's own
  configuration says nothing about. What the union answers is which words PARSE
  as states under this root, which is a superset of what any one headline is
  configured for; the chain answers both of the other two questions, and it is
  one chain, so the palette and the wall cannot disagree. The cost, paid on
  purpose: a keyword no scope of a row's claims takes `classify`'s fallback
  (`True`, active) rather than the opinion of whichever layer happened to
  declare it, so `ABANDONED` on an untagged row is a state, unclassified, and
  stays in the default view. Evidence: `TestConfig` "recognition is the union of
  every layer, and classification is not", "a keyword no scope here claims is
  recognized and unclassified", "the resolver is the rule, and it is total".
  Breaks: putting it back re-opens both of the above. **test**
- **`GET /keywords` is `keywordScopes` read forwards, and the dedup IS the
  rule.**
  `?ids=A,B` answers `{sources: [{source, active, inactive}], unknown}`: one
  entry per SOURCE in precedence order over the rows named — `default`, then
  `system`, then their tags in row order, then `file` — with each
  keyword under the WIDEST source that declares it and nowhere below it, and a
  source left with nothing dropped rather than shown empty. So `default` always
  leads with org's TODO/DONE and a `system.org` redeclaring the pair shows its
  OTHER keywords and no row at all when it has none. Each entry's own
  active/inactive split is that source's, so the answer classifies as well as
  enumerates: `READING` under `book` rather than under the row's own `file` is
  `classify` saying which scope answered. FOUR sources and no
  `union` row: the recognition seed is not a scope, so a keyword only another
  tag's config names is offered nowhere and settable nowhere it is not
  configured. Over ONE row the offer IS `setStateEdits`' rule, `settableStates`
  being this answer flattened; over SEVERAL the merge below can
  offer a keyword part of the set cannot take, which is a whole-request 400
  naming the row. EVERY `ids`/`id`
  occurrence is read, so `?ids=a&ids=b` says what `?ids=a,b` says — and the
  repeated form is what an id CONTAINING a comma owes, since the fallback row id
  is `path#ordinal` and the split runs after percent-decoding, which is why the
  shell writes one parameter per id and the comma form is left to a caller
  typing one out. Evidence:
  `Glance.Query.keywordSources`, `TestServe` "GET /keywords". **test**
- **`GET /tags` is the tags popup's source of truth, and it answers PER ROW.**
  `?ids=A,B` answers `{rows: [{id, tags}], vocabulary, counts, unknown}`: `rows`
  in the order the ids were named, each row's tags in the order its FILE spells
  them and folded through `tagsOfCell` — the same reading `tagged` matches with
  and the filter vocabulary is built from, so what this reports about a row is
  exactly what a write to it will find there. Per row rather than as one union
  because the client needs to know WHICH rows lack a tag: adding one writes the
  rows that do not carry it and no others, and a union cannot say which those
  are. The union, its coverage counts and its first-seen order are the popup's,
  computed off this. `vocabulary` is the whole store's (`storeTags`) rather than
  the named rows', because a completing read has to reach a tag none of the
  targets carries and the rows a page holds are a fraction of the tree.
  `counts` is how many ROWS the store holds under each tag, which is the popup's
  third column and the one number no arithmetic over the rows in hand recovers:
  the store's own `stTags` counts FILES, a different question. It is counted per
  request (`tagRowCounts`, one pass over `storeRecords`) rather than kept, at the
  cost of a keystroke, and a row counts ONCE per tag however often its file
  spells one — so the number answers "how many rows would a `TAG:` predicate
  reach". Refusals follow `/keywords`': no ids is a 400, an unknown id is named
  in `unknown` and left out, POST is 405, and it waits for the store like every
  route that reads one.
  Evidence: `TestServe` "GET /tags". **test**
- **Several rows merge by source NAME, and the merge costs two properties.** The
  marked set is one answer: the `file` entry is the union of those rows' files'
  own pragmas, and the tags are every tag any of them carries in first-seen
  order across the rows as given. So a keyword one row reaches through its file
  and another through a tag lands in the WIDER of the two, and rows whose tag
  ORDER disagrees are resolved by the merged order — the table describes the
  SET rather than any one member of it. The second cost is new with the
  tightening: a keyword only PART of the set reaches is offered for the set, and
  committing it is a whole-request 400 naming the row it does not fit, since
  legality is per row's own chain. Offering nothing but the intersection was the
  alternative and is worse — a marked set spanning two tags would offer neither
  tag's cycle, and the reader is better told which row refused. The three
  reserved names are not taken
  out of the tag namespace: a tag called `system` keeps its TAG rank, so it
  now sits BELOW the system layer, and the table shows the name twice with
  the precedence order to tell them apart. Evidence: `TestServe` "a keyword
  wider in one row than another lands in the wider source", "a tag spelled
  like a reserved source keeps its own rank", "a marked set spanning tags is
  refused for the row that cannot take it". **test**
- **`hrDeclared` is stored because it is not recoverable, and forced because it
  is stored.** A record keeps the
  file's OWN `#+TODO:` sets beside the recognized union (`hrKeywords`): a file
  redeclaring a seeded keyword the other way adds nothing to the union it
  disagrees with, so the difference of the two loses exactly the case the
  widest-scope rule exists for. One value shared per file, like the union
  beside it — and through the same `forcedKeywords`, because a `TodoKeywords`
  field is strict only to WHNF: the first cons cell. An unforced set is a thunk
  over the file's `[Spanned Element]`, so storing one would pin the whole parse
  for the life of the process, against the scan's residency budget. `classify`
  did not expose that (it stops at the first `elem` and `forceRecord` collapses
  `hrActive`); a stored field does. **test** (residency: **corpus**) 
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
- **Both tree-wide lines ride in the layer's own write.** `POST /config` takes an
  optional `filter`, an optional `capture` and an optional `template` beside
  `lines` and splices all
  four in ONE `configEdits` call under ONE digest, because they are regions of
  one
  file: four requests would be four writes and each would drift against a
  digest the one before it had just invalidated. Absent leaves a line exactly as
  it is, empty takes it away (which is the tree going back to the built-in view,
  and to `inbox.org`), and anything else writes it. Three absent pragmas insert
  at the same offset, which `Data.Org.Edit.applyEdits` resolves in LIST order
  rather than refusing — touching edits are legal and insertions at one offset
  land as the caller named them. Both lines belong to the SYSTEM layer alone, so
  a tag layer's write drops them whatever the request said. One reader
  (`settingOf`, over `lastPragmaValue`, last line wins) and one writer
  (`settingEdits`, over `pragmaLineEdits`) serve both, which is what makes
  "replace where it stands, insert under the header, empty deletes" true of each
  without being written twice; each pragma NAME is one constant, folded for the
  read and rendered for the write by one `settingPragma`. **test**
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
- **THE DAEMON QUEUES EVERY PATH IT WRITES, because fsnotify will not.**
  fsnotify arms a newly created directory but does not TRAVERSE INTO it, so
  `mkdir -p a/b` leaves `b` unwatched permanently — a one-second pause before
  the file is written does not help, and staging the creates (`mkdir a`, wait,
  `mkdir a/b`, wait, write) IS picked up. Measured over a served temp tree,
  2026-08-04: `data/aa/bbbb/data.org` written after one `mkdir -p` — no event;
  the same with a second's pause between the directories and the file — no
  event; `data/ee/data.org`, ONE new level under a watched directory — event. It
  is a RULE rather than a race, and it is the watch's property rather than any
  route's: any tool creating the directory and the file together loses the same
  event.

  Two writes here mint territory of exactly that shape. A TAGGED CAPTURE's blob
  is `data/<shard>/<rest>/data.org`, both levels from one
  `createDirectoryIfMissing True`; the FIRST `.org-glance/config` in a tree is
  two directories the same way. The daemon knows the path at write time, so it
  says so: `Glance.Web.Watch.nudge` puts it into the watch's own debounce map
  (`hubPending`, on the `Hub` for the reason `hubStore` is — two sides reach
  it), and the existing serial drain loop answers it through the same `settle`
  step an inotify event gets.

  **THE RULE IS EVERY WRITE, and "the writes that CREATE" was the wrong cut.**
  Being unwatched is a property of the PATH and it outlives the write that
  caused it: the shard stays unwatched for the life of the daemon, so the
  capture arrived and then every LATER write to that row was lost. Measured
  before the widening: capture a `:work:` blob, `set-state STARTED` on it, and
  the file reads `* STARTED …` while `/headlines` still says `TODO`, with one
  watch line in the log instead of two. So all five write sites leave through
  `Glance.Web.Watch.writeSpans` — `captureInbox`, `captureBlob`, `writeOne`
  (every `/command` row write), `commit` (`POST /headline`) and `writeLayer`
  (`POST /config`) — which is `replaceSpans` plus a nudge of the path it just
  wrote, on the SUCCESS branch, so a drift or a refusal queues nothing. Nudging
  a watched file costs nothing: the queue is keyed by path, so the nudge
  coalesces with the inotify events landing microseconds behind it and the pair
  is one parse. What that buys is a rule with no list to keep in step with the
  routes, and the path spelled ONCE per write where a caller pairing
  `replaceSpans` with a nudge of its own could name two different files.

  FOUR THINGS THAT MAKE IT THE SAME MECHANISM RATHER THAN A SECOND ONE. `nudge`
  is the ONE door into the queue — inotify's own handler goes through it too —
  so `watched` filters a nudged path exactly as it filters an event and a route
  can no more smuggle a derived mirror into the table than an event can. The
  queue is keyed by path, so the debounce is unchanged. Nothing is loaded or
  published at the door — `settle` on the drain loop remains the only writer of
  the store, so `POST /command`, `POST /headline` and `POST /config` still never
  touch it. And the loop stays ONE serial `forever`, which is what the reseed's
  correctness argument rests on: `drain` is the loop's body lifted into a
  function so a test can turn it, and it takes the ripe paths out in the
  transaction before settling them, so a nudge arriving mid-parse waits a turn
  rather than being lost.

  A TURN WITH NOTHING RIPE WRITES THE TVar NOTHING, and that is a contention
  guard rather than tidiness: the loop takes 40 turns a second and request
  threads now write the same var, so an unconditional `writeTVar` of the map it
  had just read would dirty it 40 times a second and make every concurrent
  `nudge` retry for no reason.

  Evidence: `TestStore` "Nudge" (the door filters, coalesces, writes no store
  and streams nothing; a nudged path that fails to load keeps its rows),
  `TestServe` "and the row arrives with no event behind it", "and so does a
  later write to that same blob" and "the first config layer in a tree reseeds
  it with no event behind it" — each hands `drain` the directory and the hub and
  names no path, so they pass only because the write queued its own. Measured
  live against a served temp tree the same day: the capture logs `1 upsert` and
  `/headlines` carries the answer's `id`; a `set-state` and an `add-tag` behind
  it log one line each and the table matches the file; the first `POST /config`
  logs `config reseed`. **test** (+ live)
- **KNOWN GAP (open): an EXTERNAL create into a fresh shard is still invisible.**
  The nudge covers what THIS daemon writes and nothing else, so the control that
  proves the mechanism is also the gap: a blob written into a `mkdir -p`'d shard
  by another process raises no event and does not appear until a restart or a
  config reseed re-walks the tree. org-glance's own Emacs side is a primary blob
  writer, so this is not hypothetical. Closing it needs what was rejected here —
  a directory-creation event let through the predicate and answered with a
  nested `watchTree` plus a sweep of what it holds, which is machinery in the
  one loop whose seriality is the reseed's correctness argument. An ordinary
  edit to an EXISTING blob is unaffected either way: that directory was walked
  at startup and is watched. **live** (measured, open)
- **KNOWN CONSEQUENCE: the queue outlives the watch thread.** `hubPending` was a
  local of `watchOrgTree` and died with it; on the `Hub` it does not. The walk
  and watch run on one `forkIO`'d thread (`Glance.Web.indexTree`), so an
  exception there leaves the daemon serving and writing with nothing draining,
  and a tagged capture mints a fresh path per call — bounded by the tree's file
  count for ordinary edits, unbounded for captures (~1.4 KB an entry; 10k
  captures ≈ 14 MB). The honest fix is making a dead watch fatal rather than
  capping the map, and neither is taken. **none**

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
- **A blob's occurrence history is derived too, and it hides one level deeper.**
  org-glance snapshots a completed repetition as
  `.org-glance/data/<id>/occurrences/<STAMP>.org` — an immutable copy of what the
  entry said then, carrying the LIVE entry's `ORG_GLANCE_ID`. It is INSIDE
  `data`, so keeping `data` kept it, and `isCanonical` ranked it canonical for
  the same reason the live blob is: `beatsForId` called the pair a tie, walk
  order decided which one the table showed, and `POST /headline` would have
  written to whichever won. Serving history as the entry and letting an edit land
  in it are the same bug the overview mirrors were, one directory further in.

  `Data.Org.Walk.isOccurrence` is the rule and `isDerived` covers it, which is
  what makes the WATCH agree — `Glance.Query.derivedPath` is `isDerived`, so a
  snapshot the walk never collected cannot arrive by inotify. `isCanonical`
  excludes it too, so under `--include-derived`, which walks it, it loses the id
  to the live blob instead of tying. The flag reaching it is the flag meaning
  what it says.

  DEPTH IS LEFT OPEN: the name is asked for anywhere under `data`, because
  org-glance shards a blob directory by the id's first two characters
  (`data/<2>/<rest>`) and falls back to `data/<id>` for an id of two characters
  or fewer, so the history sits one component deeper in the usual case than in
  the degenerate one and no position test covers both. The cost is that a blob
  whose sharded remainder spells exactly `occurrences` would be declined as
  history — and that path is indistinguishable from a two-character id's history
  by the path alone, so no rule can separate them.

  Zero on disk under `~/sync` at 2026-08-02, so this closes the hazard before it
  is reachable and the corpus counts do not move. Evidence: `TestQuery` "Walk" —
  a fixture store where the blob, its occurrence and an overview all carry ONE
  id, asserting the walked file list and that the watch declines the snapshot
  through the same predicate. **test**
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
- **One `lstat` an entry classifies it, and a symlink pays a second stat.**
  `visit` asks `getSymbolicLinkStatus` — which never follows — inside `try`, and
  that one answer routes the entry: a real directory is entered, anything that
  is neither a directory nor a link is kept on its name, and a LINK is the only
  shape needing more. For a link the walk then asks `getFileStatus`, which does
  follow, and only when the answer could change what it collects — a link that
  is neither a document by name nor inside a declined directory contributes
  nothing whatever it points at, so Emacs's `.#name.org` is refused by name
  ahead of both stats rather than by dangling. The four answers are the ones the
  pair before it gave: a symlinked DIRECTORY is never entered (the reason is a
  link loop, and counting one tree twice), a symlinked FILE is kept on its name
  like a real one, a link whose target is missing reads as a non-directory and
  is walked, and a failed `lstat` lands in that same branch — silently, the way
  `doesDirectoryExist` used to swallow one into a `False`. A symlinked mirror or
  config directory is still counted where it is declined, which is why the
  target's type is asked at all in that case — no test reaches `foundDerived` or
  `foundConfig`, so that half was checked by running the same fixture tree
  through both implementations by hand (2 derived, 1 config, 2 config keywords,
  identical).

  The silence is worth naming twice: an unlistable directory IS reported in
  `dirErrs`, a symlinked one is not, and neither is one whose stat raised — so
  "nothing under here" and "we declined to look" read alike in a report.
  Evidence: `TestQuery` "Walk" — one fixture tree carrying all five shapes at
  once (real subdirectory, real directory named `*.org`, symlinked file,
  symlinked directory, symlinked directory named `*.org`, dangling link,
  Emacs's lock), asserted as the sorted list of files walked and their load
  outcomes, since a tree entered twice through a link and a file quietly
  dropped are both invisible in a total. Verified against the pre-`lstat`
  implementation: same list, same outcomes. **test**
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
- **The serial walk is most of the wall, and what it is made of has been
  measured.** Measured 2026-08-01 over ~/sync, which is 6287 `.org` files inside
  89691 directories and 702296 entries: the walk is 10.4 s of a `glance scan`,
  and the parallel read of every file is ~1.2 s of it. `serve` is the same shape.
  So a corpus's cost here is its DIRECTORY count, the pool cannot touch that
  half, and `scan`'s `walk seconds` row exists to keep the two apart.

  The 12.9 s that row used to report came down in two moves, both landed
  2026-08-01. `visit` went from two stats an entry (`doesDirectoryExist` then
  `pathIsSymbolicLink`) to one `lstat`, worth 12.9 → 12.1 s. And
  `orgGlanceTails` — which `isDerived` and `isConfig` each call, so twice an
  entry — grew an allocation-free character scan for `.org-glance` ahead of the
  `splitDirectories`/`tails` pair, worth 12.1 → 10.4 s. The guard is exact: a
  path that does not spell the string cannot hold the component, so the fast
  exit answers what the split would have. It is hand-written rather than
  `Data.List.isInfixOf` on measurement — the two allocate alike at `-O1`, but
  `isInfixOf` reaches `isPrefixOf` through the `Eq Char` dictionary per
  position and costs ~0.45 s more over the corpus.

  ONE MEASURED THING DECLINED: `isDerived` and `isConfig` each call
  `orgGlanceTails` themselves, so an entry scans its path twice. Sharing one
  result between them saves ~130 ms of the 10.4 s (~1.2%) and costs splitting
  both rules into a pair of functions — the one-definition-each shape is what
  keeps the walk and the watch answering alike, and it is not worth 1.2%.

  WHERE THE REST IS, decomposed with a standalone harness over the same tree
  (3-run medians, warm): `find .` crosses it in 2.0 s, so ~2 s is the syscall
  floor. A `listDirectory` + `lstat` loop in `String` is 7.6 s of the 10.4;
  the same loop on `RawFilePath` (`System.Posix.Directory.ByteString` +
  `System.Posix.Files.ByteString`) is 3.3 s. So ~4.3 s is GHC marshalling a
  `FilePath` — decoding every one of 702k names out of `readdir` and encoding
  each back for the stat — and it is the whole of the remaining gap to a
  2–5 s walk. THE PRICE of taking it is the reason it is a decision rather
  than an optimization: `isDocument`, `isDerived` and `isConfig` are one
  `FilePath` rule apiece serving both the walk and the watch
  (`Glance.Query.documentPath`/`derivedPath`), and a byte-level walk needs a
  byte-level spelling of each — two encodings of one rule, which is the drift
  those single definitions exist to prevent. A walk that decoded only the paths
  it KEEPS (~6.3k of 702k) would pay the marshalling where it is cheap, but it
  still owes a byte-level `isOrg`/`isSidecar` to decide what to keep.

  One older measurement stands: the walk got SLOWER as `-N` rose (11.9 s at
  `-N1`, 13.5 s at `-N8`) with GC steady at 1.0 s elapsed either way, so the
  cost is not the collector. **docs**
- **A row is a top entry.** `Glance.Query.recordsOf` keeps the headlines
  `topLevel` accepts — one star, no ancestor — and everything deeper is carried
  inside an ancestor's `hrSubtree` rather than beside it. `subtreeSpans` runs
  over the WHOLE headline sequence and the filter is applied to the zip
  afterwards. For THIS predicate the two orders agree — a level-one extent ends
  at the next headline at level one *or shallower*, which is another level-one
  headline, so the dropped ones never decided anything (checked exhaustively
  over every level shape up to five headlines). The order is kept anyway
  because `subtreeSpans` is org's outline rule over a DOCUMENT and running it
  over a subsequence is a different function: on a predicate keeping levels
  {1,3} the two disagree, the deeper row ending at the next KEPT headline
  instead of the next shallower one — a subtree missing its own children. Four
  consequences, all intended and
  each pinned: a word only a child carries matches nothing, `hrSearch` being
  built from the cells of the rows that exist, and materializing the entry is
  how the child is reached; an `ORG_GLANCE_ID` on a deeper headline is not a row
  id, so it addresses nothing and cannot collide; a file whose outline never
  reaches level one contributes no rows at all, the answer a file with no
  headlines gives; and the extents now TILE rather than nest — consecutive ones
  meet exactly, the nesting having moved inside a single extent. `scan` is
  unaffected in either tally: it counts headlines and `ORG_GLANCE_ID`s off
  `orgParse`'s own elements, never through `recordsOf`, because it is a parser
  oracle rather than a view of one. Measured on `~/sync` at 2026-08-01: store
  rows 12875 → 10685 and id collisions 9 → 7, while the scan's headline count
  stays 12884 and its collision tally stays 9. Evidence: `TestQuery` "Top
  entries" and the search-miss case,
  `TestSubtree`'s geometry (top-entry check plus abutment) over five fixtures
  and the corpus, `TestServe` "a top entry materializes with its children in
  it". **test + corpus**
- **A row has something to show.** `Glance.Query.blankEntry`, beside `topLevel`
  in `recordsOf`: a top entry carrying none of the six column sub-spans —
  `hsTodo`, `hsPriority`, `hsTitle`, `hsTags`, `hsSchedule`, `hsDeadline` —
  emits no row. The file keeps the entry, org being the source of truth; the
  table skips it, so what was a line of six empty cells is no line.

  THE LAYER IS A DECISION and it went to the headline. The rule MEANS "every
  cell this record would show is empty", which is a property of the record, but
  the ordinal numbers EMITTED rows (`rowId`), so the filter runs before the
  numbering and there is no record yet to ask. The two layers agree by
  construction: each of the six spans is `Nothing` exactly where `recordOf`
  would cut an empty cell, and a span that is there is tight, so it cuts a
  non-empty one. `TestQuery`'s "so no row the loader emits has six empty cells"
  is that agreement stated from the record's side.

  Nothing the table has no column for rescues an entry: a `CLOSED:` stamp, a
  properties drawer, a body, children. Two of those cost something and are
  pinned rather than described. A blank entry has no row id, so an
  `ORG_GLANCE_ID` on one addresses nothing and no command can reach it — which
  is why `TestQuery`'s set-state-into-bare-stars case moved to a headline whose
  only content is a priority. And a blank parent takes its whole subtree out of
  the view, the answer a file that never reaches level one already gives.
  Reading the rule's "no planning" as the two planning COLUMNS rather than org's
  three keywords is the one place this could have gone the other way: counting
  `CLOSED:` would keep an entry whose every cell is still empty.

  The tags clause never fires alone. Org spells tags after a title and the
  parser hands `* :tag:` its colons as the TITLE, so no headline carries
  `hsTags` without `hsTitle`. It is written down because the rule is over the
  columns rather than over what the parser happens to reach.

  ON THE WIRE: `set-state` with a null keyword over a title-less row leaves `* `
  in the file and deletes the row, which is the whole reachable path to a blank
  entry. The churn it lands in is the ordinal's — every K behind it moves up
  one, the shape a removal has. One interaction is worth naming: a file whose
  LAST row goes takes its keyword contribution with it, so in a tree where it
  was the only file declaring `TODO` the step is a moved palette and `guarded`
  answers `ViewChanged` INSTEAD of the delete. `TestStore` keeps a second file
  for exactly that reason.

  `scan` is unaffected, counting headlines off `orgParse` rather than through
  `recordsOf`. Corpus at 2026-08-01: 6287 files, 10441 top entries, 0 of them
  blank — the rule costs a real tree nothing and reaches only what an edit
  blanks. Evidence: `TestQuery` "Blank entries" (seven cases), `TestStore`'s
  clear-flow and renumber pair. **test + corpus**
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
  to both; every route resolves at its own door through `storeRecords` and
  `bootstrapFrame` resolves transitively through it, so materializing an id two
  files claim opens the one the table is showing. The count rides as
  `X-Glance-Id-Collisions` and the pairs
  are listed by the scan (capped at 20). Corpus: 522 collisions with the
  mirrors walked, 9 without — those nine are genuine duplicates between real
  files (an elpa working copy of a checkout; documents whose `data.org` repeats
  the source document's id). **test + corpus**
- **A row id is its `ORG_GLANCE_ID`, else `FILE#K`, where K is an ORDINAL.**
  K is the headline's 0-based place among its FILE's EMITTED ROWS, numbered in
  `Glance.Query.recordsOf` after both filters, so a child and a blank entry each
  spend no ordinal and a deeper headline can never take one. What that buys is
  what a table needs: the id survives every edit that does not move the file's
  rows past each other. A preamble inserted above row 0, a retitled headline,
  a state flipped, a body that grew, a drawer added, a child edited — none of
  them renames anything, so the store streams the row that actually changed and
  a reader's selection, marks and open sheet all hold.

  THE BREAKAGE CLASS, stated because it is real and cannot be designed away
  without an `ORG_GLANCE_ID`: reordering top entries, inserting one ahead of
  others, or removing one renumbers everything behind it, and an entry going
  BLANK is a removal for this purpose (`blankEntry`). A swap re-points two ids
  at each other's headlines; a new first entry re-points every id at its
  predecessor and adds one at the end. No delete-plus-insert is streamed in
  either case — the id set is the same or a superset — so a client sees cells
  change under stable ids, which is the honest wire answer and the reason
  writing an `ORG_GLANCE_ID` still matters for a file whose entries get shuffled.

  This replaced `FILE:START`, the character offset `hsFull` began at, which moved
  on ANY edit above the headline: a byte typed into the preamble renamed every
  row in the file, and the store could not tell that from every row being deleted
  and re-inserted. Measured live 2026-08-01 over a three-entry file — a preamble
  added, a body line added and one state flipped in one save — one `upsert-row`,
  where the offset id would have shipped three deletes and three inserts.

  The two forms share one namespace and are resolved by exact string
  (`resolveIds`); nothing anywhere parses an id apart, so no rule turns on the
  separator. It is `#` rather than `:` because a path may hold either and a
  walked path always ends in its `.org` extension, which makes `FILE#K`
  recoverable at its last `#` for every file this library can reach. A headline
  whose `ORG_GLANCE_ID` literally spells another row's `FILE#K` collides the way
  any two headlines claiming one id collide — one is kept, the other reported —
  so a pathological tree costs a row and never points an id at the wrong one.
  Ordinals cannot collide with each other: unique per file by construction, and
  prefixed by the path across files. Corpus at 2026-08-01: store rows 10685 and
  id collisions 7, both unchanged by the switch.

  ONE CONSEQUENCE ELSEWHERE: the id carries a `#`, which a raw URL reads as a
  fragment. `/headline?id=…` is safe because the id rides in the query string
  and both sides percent-encode it — `renderQuery` in the suite,
  `encodeURIComponent` in the shell — and `POST /command` carries ids in a JSON
  body. Evidence: `TestQuery` "without one the row id is FILE#K…" and the
  child-spends-no-ordinal case, `TestStore`'s five stability/churn cases,
  `TestServe` "an id carrying a hash and slashes round-trips". **test + corpus
  + live**

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
- **`stDirErrs` is written by `loadStoreWith` alone.** Set from the walk, read by
  `storeResult`, and touched by nothing in `putFile`, `removeFile` or `guarded`
  — so a per-file watch event never moves it. It DOES move on a config reseed:
  `Watch.reseed` calls `loadStoreWith` for a fresh store and `reseeded` installs
  that wholesale, walk included. A directory that becomes unreadable after the
  walk, or becomes readable again, is therefore invisible until the next config
  change or a restart, and the count in `X-Glance-*` describes the last full
  walk rather than the tree now. `stPrint` is written by the same one writer and
  has the same reach. **none**
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
  touched ids — the order of a route's own scan, and measured end to end at
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
- **A write to an org-glance BLOB tells org-glance, through
  `<store>/.org-glance/meta/EXTERNAL.jsonl`.** The cross-repo contract, frozen;
  org-glance's half is `org-glance-graph:refresh-external`
  (`src/data/org-glance-graph.el`, whose commentary states the same rules).

  WHY. A blob is canonical content and org-glance's write-ahead index is Emacs's
  projection of it. This daemon edits blobs and does not write that index, so a
  browser edit leaves the index one record behind — which is exactly the drift
  the instrument above counts. The file is where the two sides meet: this side
  names the ids it moved, Emacs re-derives a record for each and shortens the
  file.

  THE LINE. One JSON object per line, newline-terminated, two fields in this
  order: `{"id":"…","at":"2026-08-03T04:21:07Z"}`. `id` is the `ORG_GLANCE_ID`
  of the written blob's FIRST headline, read the way `Data.Org.Index.blobEntryOf`
  reads it, so a line names the record a refresh replaces — first rather than
  first-with-an-id, and an entry claiming none is skipped with no line at all.
  `at` is the server clock in UTC at second resolution, and nothing acts on it.
  Values go through the JSON encoder and the KEYS do not, which is what fixes
  the order without leaving an id unescaped.

  WHICH WRITES. `Data.Org.Walk.isBlob` — `data.org` inside the canonical store —
  and no other file. An ordinary document, a config layer, an overview, a blob's
  occurrence history and another `.org` sitting beside a blob all have no record
  to refresh, so none of them is noted. The store is the `.org-glance` directory
  the blob sits under (`Data.Org.Walk.orgGlanceRoot`, innermost wins), so a tree
  holding several stores notes each write in its own.

  ONE DOOR. The note is taken in `Glance.Query.replaceSpans` and nowhere else,
  because that is the one function every write in this program leaves through —
  the FIVE write sites (`captureInbox`, `captureBlob`, `writeOne`, `commit`,
  `writeLayer`) reach it through `Glance.Web.Watch.writeSpans`, which adds the
  nudge and nothing else, and `Data.Org.Edit.editFile` has no other caller. So a
  command over several rows of ONE blob is one `editFile` and therefore one
  line: the id names the entry rather than the edit. It costs one parse of the text
  just written, and it cannot fail the write — by the time it runs the rename
  has happened, so every IO error there is swallowed. A refused write (drift, a
  rejected batch) notes nothing, having written nothing.

  APPEND-ONLY, and it has to be `O_APPEND` with ONE `write(2)`
  (`Data.Org.External.appendLine`). A `Handle` in `AppendMode` remembers the
  offset it opened at, so concurrent writers overwrite each other's lines: eight
  concurrent blob writes through `BS.appendFile` left FIVE lines. Under
  `O_APPEND` the kernel re-seeks inside each write and all eight land. This side
  never truncates, never rewrites, and touches no other file under `meta`.

  THE CRASH RULE. Emacs appends every re-derived record to its log BEFORE
  shortening this file, so a crash between the two costs a repeated refresh and
  nothing else — re-deriving a record from a blob that has not moved appends a
  record equal to the one already there, and the latest-per-id fold cannot tell
  the difference. Idempotent by construction, which is what lets the two steps
  be unsynchronised. Emacs drops exactly the prefix it read rather than writing
  the file empty, so a line this daemon appends mid-refresh survives to the next
  one. Evidence: `TestExternal` — the door, the golden line, the path rules,
  append-only including the concurrent case, and the three write routes.
  **test**
- **The command layer is one route, and its unit of work is a FILE.**
  `POST /command` takes `{name, id | ids, args, digests?}` and implements ten
  names — `set-state {"keyword": KW | null}`,
  `set-planning {"keyword": "SCHEDULED" | "DEADLINE", "date": TEXT | null}`,
  `set-title {"title": …}`, `set-priority {"priority": LETTER | null}`,
  `archive {}`, `capture {"text": …, "tag": … | absent, "fields": {…} | absent}`,
  `add-tag {"tag": …}`,
  `remove-tag {"tag": …}`, `rename-tag {"from": …, "to": …}` and
  `edit-link {"span": [S, E], "target": …, "desc": … | null}`. The ids it is
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
  `set-state` keyword that ANY named row's own chain does not declare — that
  last one refuses the WHOLE request deliberately, because half a state change
  over a marked set is worse than none of one, and because legality is per row
  so the alternative is a command that means different things to different
  rows of one keystroke. The refusal names the keyword AND the row, since with a
  per-row rule the file alone no longer says which member of a set turned it
  down. 413 outranks all of it, the way it does on
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
- **Keyword legality is the ROW's own chain, and the group meta-values are not
  keywords.** `setStateEdits` refuses anything outside `settableStates` —
  `keywordSources` for this one record, flattened: its file's own `#+TODO:`, the
  configs of the tags THIS row carries, `system.org`, org's TODO/DONE. Derived
  from the palette's own function rather than folding the chain a second time,
  so "what a reader is offered is what a write takes" holds by construction and
  the coupling runs both ways — a change to what the palette SHOWS a row is a
  change to what that row may be set to. The bar
  used to be the file's recognized set (`hrKeywords`), which is the parse seed
  and so the whole tree's vocabulary, and it let a `film` keyword be written
  onto an untagged row and onto a `book` one. Recognition stays the superset it
  was — the same word is a keyword in one document and the first word of a title
  in the next, and the seed is what keeps it out of the title — while
  settability is the narrower question of what a row is CONFIGURED to be. The
  rule reads off one chain with the palette, so what `GET /keywords` offers for
  a row is exactly what a write for that row takes.
  `state:*active*`/`state:*inactive*` are filter vocabulary the state column
  ships beside its badges and are in no keyword set, so they are refused here
  like any other word — which is why the shell's value palette offers the
  resolution and never `values`. Evidence: `TestQuery` "another tag's keyword is
  refused on a row that does not carry it", "everything the palette shows for a
  row is settable on it", "each scope of the chain is settable on a row that
  reaches it"; `TestServe` the same three over the route. **test**
- **`args` is one record, and `.:!` is what tells absent from null.** The three
  commands that take arguments read into one `Args` — `keyword` (which is
  `set-state`'s state AND `set-planning`'s planning keyword, one field because
  the wire spells both that way), `date` and `text` — and the two nullable
  fields go through `.:!` rather than `.:?`. That is the whole of the
  distinction the layer turns on: `.:?` folds a null into an absence, so
  `{"args": {}}` would read as an instruction to CLEAR where it is a request
  that said nothing. Absent is a 400 naming the field; null is the clear.
  **test** (`TestServe` "a request with no date at all is a 400", "and one with
  no keyword either", and the set-state pair that predates them)
- **`set-planning` moves one timestamp, and the LINE is the lens's rule.**
  `Glance.Query.setPlanningEdits` has four shapes. An entry already there is its
  own span and nothing else, so a reschedule leaves the keywords, the spacing and
  every other entry on the line byte-identical. An entry the line lacks joins the
  END of it, behind whatever it already carries — which is the subtree lens's own
  rule for an entry that moved, reached from the command side. A headline with no
  planning line at all grows one under its TITLE LINE, at column 1, through the
  same `titleLineEnd` `archiveEdits` uses and for the same reason: `hsFull` ends
  at a drawer's `:END:` two lines down. And a clear takes the entry together with
  the horizontal run that separated it — the TRAILING one, or the LEADING one
  where the entry ends its line, never both, since eating both would glue the two
  neighbours of a middle entry together — and takes the WHOLE LINE when it was
  the last entry on it, a planning line with no entries not being one. `CLOSED:`
  counts as an entry for that last rule and is settable by nothing: it is org's
  own bookkeeping, so a key that wrote one would be forging a state change.
  Clearing an entry the headline never had costs no edit, which makes the command
  idempotent the way `archive` is. Evidence: `TestQuery` "set-planning", nine
  cases spliced with the suite's own three-line oracle and asserted as the whole
  document; `TestServe` "POST /command set-planning" for the route.
  **test + live**
- **The date is parsed ONCE per request, and an unreadable one refuses all of
  it.** `Glance.Query.planningTimestamp` takes the day the request was made and
  the text as typed. Four spellings: a value opening on a bracket is org's own
  and is kept VERBATIM once `readsAsTimestamp` says it reparses — so a repeater,
  a range or a warning period the author spelled out survives rather than being
  canonicalized away, and a wrong weekday in that form stands because the value
  is the author's; `today` and `tomorrow`, and `+Nd` / `+Nw` / `+Nm`, work a date
  out; and a bare ISO date carries an optional `HH:MM` (read with `%k`, so `9:05`
  is the time a reader meant rather than a refusal over a zero). The last three
  render `<YYYY-MM-DD Day>` with the weekday COMPUTED, which is the one thing a
  reader cannot be asked to get right. Anything else is the WHOLE request's 400
  naming the input, for the reason an undeclared keyword is: half a reschedule
  over a marked set is worse than none of one. Reading the clock once is what
  stops a set crossing midnight from landing on two days. The rendered stamp is
  then passed DOWN (`resolveDate` → `overRows` → `planCommand` → `commandEdits`)
  rather than written back into the request, so `agDate` means the text the
  client typed at every point in it — a field meaning one thing before a call and
  another after it is a trap for whoever reads it next. **test**
- **`capture` is the one command that names no rows, and the one whose target
  comes out of the config.** Every other name here edits a headline a client can
  point at; this one MAKES one, and the mechanism is the command table's own
  `csEdits :: Maybe RowEdits` — `runCommand` destructures that `Maybe` ONCE and
  hands the edits themselves down, so nothing below it has an arm for a command
  that edits no row and the rows-are-named rule is never relaxed. The answer is
  its own shape, `{ok, file, digest, id}`, since there is no per-id result to
  give — and `id` is the row it MADE, which is what the cursor lands on.
  The entry is `* <text>` and a drawer holding `:ORG_GLANCE_CREATION_TIME:`
  under it, appended at the END of the target so every byte already in the file
  stays where it was; a target whose last line has no newline gets one first, or
  the stars would land on a live line and be no headline at all. The text is raw
  org, written as spelled — `TODO Buy milk :errands:` captures a keyword, a title
  and a tag — and it takes the ONE-HEADLINE WALL, which is BOTH capture paths'
  (`Glance.Query.captureText`, over `oneLine`): empty after stripping, or
  carrying a newline, is refused, either of which makes the entry something other
  than the one headline the command promises. Under a TAG that wall covers every
  `fields` answer too — the line lands at the template's `%?` and an answer at a
  `%^{PROMPT}`, both spliced into ONE document, so a newline in either lands a
  column-1 star the parser reads as a second entry and a blob would hold two.
  A refusal there is the whole request's 400 naming the field, with nothing
  written. Evidence: `TestQuery` "capture", `TestServe` "POST /command capture".
  **test + live**
- **A TAG TURNS A CAPTURE INTO A BLOB, and absent is the whole of what "no tag"
  means.** `capture`'s `tag` is optional and takes the ordinary charset wall
  (`tagText`, refused with the rest of the request's SHAPE, since a word that is
  not a tag is not a tag for any tree), so past `wantsText` the field is either
  absent or a real tag and `captureInto` is one `maybe` with nothing to strip or
  test. Absent files into `#+GLANCE_CAPTURE_TARGET:`'s inbox exactly as it always
  did; present writes a new blob in the SERVED root's own `.org-glance`, and a
  tree that keeps no store is a 400 naming the directory rather than a daemon
  deciding a tree is an org-glance store by making one. THE REFUSALS ARE
  ORDERED, coarsest first and every one of them ahead of a byte: the store
  directory (the one answer that does not depend on what the reader typed), then
  the line and every `fields` answer against the one-headline wall, then the
  expansion's two, then a template that expands to no headline. Evidence:
  `TestServe` "POST /command capture, under a tag". **test + live**
- **A BLOB IS ORG-GLANCE'S LAYOUT, VERIFIED AGAINST ITS SOURCE AND THIS CORPUS**
  (2026-08-04). `Data.Org.Blob.mintBlobId` is `org-id-uuid`'s own form — a random
  version-4 UUID, 36 characters, lowercase, `8-4-4-4-12`, the version and variant
  nibbles stamped whatever the bytes say — and `blobPathIn` shards it the way
  `org-glance-graph:headline-data-path` does: the FIRST TWO CHARACTERS of the
  WHOLE id, verbatim and UNFOLDED, with the entire remainder as the next
  component, then `data.org`. An id of two characters or fewer is not sharded at
  all, which is the case `Data.Org.Walk.isOccurrence` already leaves its depth
  open for. READING an id is a different question from writing one: ~/sync's 6073
  blobs carry four superseded generations (`Article-20210511-<md5>`,
  `<tag>-<time>-<md5>`, `<tag>-<md5>`, 128-char hex) beside 45 modern UUIDs, and
  the store's own shard directories spell `Pa`, `Pe` and `al` side by side — so an
  `ORG_GLANCE_ID` is an OPAQUE STRING everywhere it is read and this module only
  ever mints the current form. `Data.Org.Blob` is a module rather than three
  functions in `Data.Org.Walk` because Walk CLASSIFIES a path that is there and
  this CONSTRUCTS one; it imports Walk's three layout names rather than respelling
  them, and keeping the mint out of Walk keeps `Crypto.Random.Entropy` and an `IO`
  off the walk's hot path. `uuidFrom` is TOTAL on a short byte string — it pads
  to sixteen with zeros rather than answering a string of the wrong length — so
  the shape is a pure function of the bytes and the suite pins it without a
  running entropy source. Evidence: `TestQuery` "Where a blob sits", "The id it
  is keyed by". **test + corpus**
- **No reservation, and the WRITE is the collision check.** org-glance mints by
  rejection against the directory it then creates; this side writes under the
  EMPTY digest, which creates the file and the directories over it and DRIFTS
  rather than overwrites should anything already sit there. 122 random bits make
  the question unreachable either way. The id is minted before the last refusal
  on purpose: nothing reserves anything, so an id that is not written is an id
  nobody ever sees. **test**
- **The `EXTERNAL.jsonl` note costs the capture nothing, because a blob write is
  a blob write.** `data.org` under a store's `data/` is `Data.Org.Walk.isBlob`,
  so `Glance.Query.replaceSpans` appends the line on its way out exactly as it
  does for a browser edit of an existing blob — blob first, line second, which is
  the order the cross-repo contract asks for (`Data.Org.External`). A capture
  therefore adds no rule to that door and cannot come to disagree with it.
  Evidence: `TestServe` "and one EXTERNAL.jsonl line naming it". **test + live**
- **`blobDocument` composes the blob out of the EXPANDED template, and its two
  rules are the command layer's own one grain lower.** IT ENDS THE TEXT FIRST AND
  MEASURES AFTERWARDS: a template is stored right-trimmed, so a title line with
  no newline of its own would take the drawer onto the end of itself, and every
  offset below is measured in the text that actually gets written. The tag goes
  on through
  `addTagEditsIn` — the very function `add-tag` runs, factored out of
  `addTagEdits` so the insertion point cannot come to differ between a capture and
  a command — and a headline already spelling the tag costs no edit. The drawer
  joins an existing `:PROPERTIES:` block under its OWN indentation and is written
  whole under the PLANNING LINE otherwise: measured from the title line instead it
  splices BETWEEN a headline and its `SCHEDULED:`, where the planning line is no
  longer the line after the title and stops being read as one at all. Both
  properties are written whatever the template said, since a template spelling
  `ORG_GLANCE_ID` would be claiming an identity the store hands out. A template
  that expands to no headline is refused rather than written — the blob would
  carry no entry, so the id would name nothing and `Data.Org.External.blobIdOf`
  would read none back out of it. Evidence: `TestQuery` "The blob a tagged capture
  composes". **test**
- **A TAG'S CAPTURE TEMPLATE IS ITS CONFIG LAYER'S FIRST HEADING, and no new file
  class.** The file that carries a tag's `#+TODO:` cycle carries its template
  too, which is org-glance's own convention; `captureTemplateSpan` reads it the
  way `org-glance-tag-config--entry` reads it — from the first `^\*+ ` LINE to
  the END of the file, right-trimmed — rather than as the outline extent, so
  ~/sync's own `book.org` (`* Book` over `*** Notes`) is ONE template. Everything
  ABOVE that heading is the file's pragmas and comments, which the `#+TODO:`
  splice and the two settings lines own between them, so the regions of a config
  file cannot overlap. Resolution is `captureTemplateIn`, which FOLDS THE TAG while the
  headline wears it VERBATIM: config file names are lowercase (`clTags`' own
  rule), so `:Book:` and `:book:` reach one template while the entry keeps the
  spelling the request asked for. The chain is the tag's own layer (the
  FIRST file configuring it), then the system layer's
  (`systemSetting`'s), then `bareTemplate` = `* %?` — a CONSTANT rather than a
  branch, so a tag with no config, a tag whose config has no heading and a tag
  spelling a template all take ONE path through `expandTemplate`. Read at capture
  time through the same `readConfigLayers` `GET /config` uses, so what a settings
  sheet shows is what a capture expands. Evidence: `TestQuery` "Where a template
  lives". **test**
- **ONE HEADING PREDICATE for the reader and the writer.** `headingStars` is
  org-glance's `^\*+ ` — a star run and then HORIZONTAL SPACE, so a bare star run
  is body text here where the PARSER reads it as an empty headline — and both
  `headingAt` (where a template starts) and `topEntry` (what a template may be)
  ask it. With two predicates the sheet was handed a `** Notes` template it would
  then refuse to write back, and a bare `*` could be written and never read again.
  The one-star wall is the writer's alone and is what keeps a blob's first
  headline the entry org-glance keys it by. Evidence: `TestQuery` "Editing a
  template". **test**
- **THE EXPANSION SUBSET IS ONE LIST AND ONE SCAN, AND THEY ARE TWO SPELLINGS.**
  `captureCodes` is the
  four codes with a line of meaning each — `%?`, `%U`, `%T`, `%^{PROMPT}` — and it
  is the CONTRACT's window: `GET /capture` serves it and the settings box
  completes over it. `templateParts` is the left-to-right scan, and it never
  consults the list: it spells the same four out as a case. So the two are kept
  in step by a case that puts every advertised code THROUGH the scan (`TestQuery`
  "every advertised code is one the scan expands") — a code the list gained alone
  would be offered to a reader as an expansion and written as its own text.
  `templatePrompts`
  (what a template will ask, in order, one spelled twice asked once) and
  `expandTemplate` are two answers off that one scan. **EVERYTHING ELSE COPIES
  THROUGH**: `%^` with no brace, an unclosed `%^{`, `%a`, a trailing `%` are all
  text and the scan goes on past them, so no template is unreadable and a code
  this server has never heard of is captured literally — honest, visible, and
  refusable later where a silent drop would not be. Two refusals, both the WHOLE
  request's: a template with no `%?` has nowhere for the line to go, and an ask
  nobody answered would write an entry with a hole in it. The clock is read once
  per request AND covers BOTH stamps a capture writes — one `getZonedTime` is
  handed to `expandTemplate` for `%U`/`%T` and to `captureStamp` for the drawer,
  so a template naming the moment and the creation time it is filed under can
  never name two. **KNOWN
  DIVERGENCE from org-glance**, deliberate and named: its own renderer
  additionally rewrites the template heading's TITLE from the capture's title, so
  a corpus template whose heading carries a placeholder (`* Book`) keeps it here
  and the typed line lands at `%?`. Evidence: `TestQuery` "The expansion subset",
  "What a template cannot do". **test + corpus**
- **`GET /capture[?tag=NAME]` is what a client reads before it can ASK anything.**
  `{template, prompts, tags, codes}` — whether a layer configures one, its
  `%^{PROMPT}` asks IN TEMPLATE ORDER, the tree's whole tag vocabulary for the tag
  prompt to complete over, and the expansion subset with its meanings. With NO tag
  it is the untagged path's own shape (no template, no prompts): the inbox capture
  stays bare on purpose, so there is nothing to resolve and the answer says so
  rather than being a refusal. `tags` is here rather than on `/tags` because that
  route answers about ROWS a caller names and a capture names none. A read, so
  POST is 405 and it needs a loaded store. Evidence: `TestServe` "GET /capture".
  **test**
- **The capture template is a REGION of a config layer and rides in its ONE
  write.** `configEdits` takes `ConfigParts` — a record rather than three
  positional `Maybe Text`, since all three have the same type and a caller
  swapping two would compile — and each of its three is three-valued the same way:
  absent leaves that part, empty takes it off, anything else writes it. `filter`
  and `capture` are the SYSTEM layer's alone and `writeLayer` scopes them; the
  template is EVERY layer's, which is the whole point of it being one. The client
  names a part only where it MOVED: sending the template unconditionally put every
  layer's own first heading back through the one-top-entry wall on every write, so
  a file whose heading is deeper than one could no longer have its cycle edited at
  all. Evidence: `TestServe` "each layer's capture template is served verbatim",
  "and written back in the same call as the block". **test**
- **The creation stamp is org-glance's own property, in org's INACTIVE form.**
  `:ORG_GLANCE_CREATION_TIME: [YYYY-MM-DD Day HH:MM]`, on the server's clock and
  in its zone, at column 1 like the stars. Inactive because a creation time is a
  record of when a row was written rather than something to turn up on an agenda;
  the property name and the bracketed spelling are org-glance's, read off its own
  store rather than invented here. It joins `hiddenProperties`, so the sheet
  never offers it and a recompose puts it back verbatim — the second entry on
  that list, which is what makes "hidden" the list rather than one key's special
  case. **test + corpus** (the spelling is what `~/sync`'s own `data.org` files
  carry)
- **The capture target is a line of `system.org`, and where it may point is
  decided when the config is READ.** `#+GLANCE_CAPTURE_TARGET:` reads into
  `ConfigLayers.clCapture` by the same `lastPragmaValue` the default view uses
  and is written by the same `pragmaLineEdits`, in the same `configEdits` call —
  three lines of one file, one digest, one splice.
  `Glance.Query.captureTargetIn` resolves it against the SERVED ROOT, because an
  org-glance store is not obliged to sit at the root being served while the tree
  a capture belongs in is; absent, the target is `<root>/inbox.org`, and so is an
  empty line. Three refusals, all TEXTUAL the way every other path rule in this
  repo is (Walk): an absolute path, a path climbing out through `..`, and a name
  the walk would not COLLECT — that last one because a capture into it writes an
  entry no watch ever delivers a row for, so the row would vanish rather than
  appear. That third one is `Data.Org.Walk.isWalked` — all three of `visit`'s
  predicates, `isDocument` minus `isConfig` and `isDerived` — and the difference
  is load-bearing rather than tidy: `.org-glance/config/x.org` and
  `.org-glance/overviews/x.org` are org files by extension that the walk
  declines, so stopping at `isDocument` would bless exactly the paths this
  refusal exists for. `visit` keeps spelling the conjunction itself, because it
  has the three answers in hand and calling `isWalked` there would re-scan every
  entry's path three more times — the cost this repo already declined to pay for
  sharing one of them (Walk, "ONE MEASURED THING DECLINED"). They are refused
  HERE rather than at capture time: a tree misconfigured
  in January says so on the startup banner instead of on the first `+` in March,
  and the same call answers the 400. Evidence: `TestConfig` "the capture target
  is a line of the system layer", "and it resolves against the served root, or is
  refused there", `TestServe` "POST /command capture". **test + live**
- **A capture pins the target's own digest, and creates under the empty one.**
  The document and the digest come off the STORE where it holds the file
  (`Glance.Web.Store.storeDocument`, the first row's, since every row of a file
  shares both) and off a fresh `Glance.Query.currentDocument` where it does not —
  one read either way, so the offset the entry is spliced at and the lock the
  write presents describe ONE text, which is materialize's rule kept for a file
  the store never loaded. The read itself is `Data.Org.Edit.readDocument`, which
  lives beside `takeSnapshot` because that is where the pin is defined and where
  the config reader takes its own layers from — one function, so a second raw
  read cannot drift from the loader's. `currentDocument` reads its `Nothing` as
  `("", "")`, and the empty digest is `Data.Org.Edit`'s pin for a file that is
  not there, so the first capture into a tree creates the file and the
  directories over it. An unreadable file that IS there answers the empty pin
  too, which is safe rather than lossy: the write re-reads, digests what it
  finds, and refuses as drift. **test**
- **One function spells an org timestamp, and the brackets are the difference.**
  `Glance.Query.orgStamp` renders `<YYYY-MM-DD Day[ HH:MM]>` or the same in
  square brackets, and its two callers are `planningTimestamp`'s renders and
  `captureStamp`. Both compute the weekday rather than taking one, which is the
  parser's own rule (Parser, "Timestamps") reached from the writing side. Two
  hand-rolled renderers twenty lines apart is how a creation stamp and a planning
  entry come to disagree about a shape org fixed. **test**
- **A written line ends the way the file's own lines do.** `captureEdits`,
  `setPlanningEdits`' grown planning line and `pragmaLineEdits`' `#+TODO:` block
  all take their line ending from `eolOf`, so a write into a CRLF file leaves a
  CRLF file rather than one with two kinds of line in it. The lens already did
  this for a drawer and a planning line it rewrites (`drawerStyle`,
  `planningStyle`). The config splice was the one that did NOT: it wrote the
  block and the opening a header-only file owes with `\n` whatever the file
  used, so one settings write left `system.org` speaking two conventions with the
  line the reader had just typed the odd one out. `eolOf` and `openingFor` are
  `Data.Org.Edit`'s now, beside the line splitting they are the other half of.
  **test** (`TestQuery` "into a CRLF file, the entry is CRLF too"; `TestConfig`
  "a CRLF layer keeps its own line endings")
- **Archiving IS adding one tag, so there is one insertion rule.**
  `archiveEdits = addTagEdits archiveTag`, and `addTagEdits` is the whole of the
  placement: `TAG:` at `spanEnd hsTags` where the headline has a run — the span
  ends past the closing colon, so the insertion is the tag and one colon and the
  entries already there stay byte-identical — else `" :TAG:"` at `titleLineEnd`,
  which is where an archive tag went and for the reason it went there (`hsFull`
  ends at a planning timestamp or an `:END:` on a later line). Two functions with
  a shared shape would be two functions to keep in step; there is one, and the
  suite asserts they agree over four headline shapes. Evidence: `TestQuery`
  "archive is add-tag at org's own name", "add-tag". **test**
- **`removeTagEdits` cuts an entry, and the LAST entry takes the run with it.**
  An entry with neighbours is cut as `TAG:` — itself and the colon that closes
  it — so `:a:b:c:` minus `b` is `:a:c:` and the survivors keep their bytes. The
  last one takes the whole run AND the horizontal space in front of it, because a
  lone `:` is not a tag list and `* Title :done:` has to close up to `* Title`
  rather than keep a trailing space. That space is always there: `tagsP` opens on
  `hspace1`, which is also why `* :tag:` parses its colons as a TITLE and
  `hsTags` never sits at a line start. Matching is FOLDED and takes EVERY entry
  spelling the tag, so "removed" means the row stops answering to `tagged` — a
  file spelling one tag twice, or spelling it `:Work:` where the caller said
  `work`, is clean afterwards. Add-then-remove is the identity on the bytes,
  which is the property a toggle rests on. Evidence: `TestQuery` "remove-tag",
  which asserts the whole document each time. **test**
- **`renameTagEdits` REPLACES the entry, which is why rename is a command and not
  a composition.** The entry's text is replaced without its closing colon, so the
  run's other entries and both delimiters keep their bytes and the tag stays
  where the author put it: `:a:work:b:` renamed to `projects` is
  `:a:projects:b:`. A remove and an add composed cannot do this, and the reason
  is what they DO rather than a refusal: the two edit sets APPLY. Removing a LAST
  entry ends exactly where the addition inserts, and `applyEdits` rejects only
  OVERLAP — an edit may start where the previous one ended — so the pair goes
  through. What it writes is wrong in two INDEPENDENT ways. The addition's anchor
  is `spanEnd hsTags` measured in the document BEFORE the removal, so for a lone
  tag it is the offset the run's closing colon sat at; the removal then takes the
  whole run AND the space in front of it, and the insertion lands flush against
  the title — `* TODO Ship itprojects:`, which the suite writes down. Separately,
  and whatever the anchor, `addTagEdits` APPENDS at the run's end, so an entry
  with neighbours survives the round trip having MOVED to the end of the run;
  re-measuring the anchor after the removal would not change that one. It would
  also be two writes under two digests where the rename is one drift-locked
  splice per file.
  ONE TAG ONCE is kept the way `removeTagEdits` keeps it: the FIRST entry
  spelling `from` becomes `to` and any further ones are cut, and where the row
  ALREADY carries `to` under another entry every `from` entry is cut instead —
  a branch that can never empty the run, since the entry carrying `to` is one it
  leaves standing. Matching folds and `to` is written as given, so a change of
  SPELLING is a rename like any other. A row not carrying `from` costs no edit,
  which is what makes the command safe to send over the whole set the popup was
  raised on, and what makes it idempotent. Rename and its inverse put the file
  back byte for byte. Evidence: `TestQuery` "rename-tag", `TestServe`
  "POST /command rename-tag". **test**
- **A tag is refused where the PARSER would not read it back.**
  `Glance.Query.tagText` checks against `Data.Org.isTagChar`, hoisted out of
  `tagsP` and exported for exactly this: what this server writes has to reparse
  HERE, and a tag carrying a character the parser declines does not land in the
  tags run — it takes the whole run down into title text on the next load, and
  the entry the author set is gone with it. Org's own `org-tag-re` is
  `[[:alnum:]_@#%]+`; this parser reads that set whole and adds `-`, the one
  divergence left, kept because the wild corpus writes it — the parser's set
  is the one that binds, since it is what reads the write. The refusal is the WHOLE request's, decided in `parseCommand` with
  the rest of the shape: a word that is not a tag is not a tag for any row.
  Both ends of `rename-tag` take the same wall, for the same reason: a `from`
  org could not have written names nothing, and a `to` it could not read takes
  the run down. Evidence: `TestQuery` "the tags add-tag and remove-tag take",
  `TestServe` "a tag no parser reads refuses the request, naming it" (both
  commands). **test**
- **All three tag commands are idempotent.** `addTagEdits` answers `[]` for a
  row `tagged` already finds it on, `removeTagEdits` for one it does not, and
  `renameTagEdits` for one carrying no `from`, so a reader may press the same
  key twice without the
  second press meaning anything. `archive`'s idempotence is that first half,
  matched through `Glance.Query.archived`, which reads `tagsOfCell . hrTags` —
  the same folding the tag list is built with, so "archived" means exactly what
  the query `tag:*archive*` means and a file spelling the tag `:archive:` counts.
  The
  file is still rewritten (the engine has no equality short-circuit), so the
  cost of archiving a marked set twice is an inotify event and a re-parse per
  file, and `guarded` then finds nothing moved and leaves the generation alone.
  Evidence: `TestServe` "archive is idempotent", which steps the watch by hand
  between the two runs and compares the file byte for byte. **test**
- **`D` archives and never deletes, and the default view is what makes that
  work.** `/headlines` drops rows carrying the archive tag unless the query
  names the META through the `tag` column — any spelling of it, `tag:*archive*`,
  `-tag:*archive*`, `tag:"*archive*"`, since all of them are a reader who has said
  something about archived rows and layering a default exclusion under any of
  them would answer a different question than the one asked. The predicate is
  exactly what `-tag:*archive*` spells, and `X-Glance-Archived` reports how many
  rows it took, so a client can tell "nothing matches" from "the matches are all
  archived". The header is zero whenever the query named the tag: a reader who
  asked is never told anything was withheld. Without the exclusion an org tree
  accumulates rows that are done with rather than gone and the default table
  grows without bound, which is the whole reason `D` can be an archive rather
  than a delete. Evidence: `TestServe` "GET /headlines and the archive",
  `TestFilter` "Archive key". **test**
- **THE STARRED SPELLING IS THE COUPLING, and the plain tag is an ordinary
  predicate.** `namesArchive` compares a `tag` predicate's folded value against
  `archiveMeta` — `*archive*`, and nothing else. `tag:archive` is the substring
  predicate every other tag value gets: it filters, it lifts nothing, and the
  rows it would have matched stay behind the exclusion, which `X-Glance-Archived`
  reports. That is what a tree using the word for something of its own needs, and
  it is what makes the two spellings tell apart: over ~/sync at 2026-08-02
  `tag:*archive*` serves 322 rows with nothing withheld, while `tag:archive`
  serves 0 and reports 322 withheld (the corpus has no unarchived row whose tags
  cell holds the letters, so the plain predicate reveals nothing there — the
  header is what says so). There is also no prefix question left: a meta is
  matched whole by construction, where the old bare `tag:arch` had to be ruled
  out by hand. `namesArchive` still takes the store's tag list and answers
  `False` when the tree carries no archive tag at all, which is sound: with
  nothing archived there is nothing for the exclusion to hide. Evidence:
  `TestFilter` "Archive key", `TestServe` "the plain tag predicate filters
  without lifting the exclusion". **test**
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
  round-tripped.** `Glance.Query.hiddenProperties` (today `["ORG_GLANCE_ID",
  "ORG_GLANCE_CREATION_TIME"]`) and the logbook are lifted out of what a client
  is shown and put back verbatim whatever the client sends. `ORG_GLANCE_ID` is
  the row id the table keys its updates off — renaming it renames the row and
  leaves the sheet looking at a different headline — and hiding it is cheaper
  than a rule about which edits to a shown value are allowed, and honest in a way
  a warning beside an editable field is not. `ORG_GLANCE_CREATION_TIME` is the
  same argument from the other end: a capture stamps it once, and a sheet that
  let it be edited would be offering to make the record say something else. The
  second entry is what makes this the LIST's rule rather than one key's special
  case, and the cases are written against the list. The logbook is a record
  nothing in this page edits. ONE list is
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
  headline's row id is `FILE#K` — same bytes under another name is a
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
  over the EFFECTIVE chain's order rather than over walk order, because page two
  has to be the rows the table would show after page one. That order is a CHAIN
  — `defaultSortChain`: state, title, deadline, scheduled, all ascending, with
  state read by BADGE PALETTE position, which is the tree's own `#+TODO:` cycle,
  so the table opens with the work in the order org names it rather than
  alphabetically. Priority is deliberately out of the chain: a fifth key behind
  four that have already separated nearly every pair of rows, and `sort:priority`
  is how a reader asks for it. The chain a query names replaces it (below), and
  whichever chain is in force is the one `declaredSort` puts on the wire, so the
  order a client is told about and the order it is served are one fact. The
  arrangement copies the renderers' rules term for term: empty cells last on
  each key and outside that key's direction, the state column by badge palette
  position (unlisted keywords tying at the back), a stable sort so rows equal on
  every key keep walk order, and text compared case-folded — the nearest this
  side gets to the browser's `localeCompare`, and the reason a title differing
  only by punctuation or script can still land elsewhere than the renderer would
  put it. With no `limit` the walk order
  stands and the client sorts the whole set — the full-fidelity mode, and the
  one the shell settles into. The shell mirrors the same rule live: with a
  filter on, a row frame off the socket is answered by re-asking the server
  rather than by splicing, since only it knows whether the changed row still
  matches. The state palette is the store's (`viewJSONWith` takes it
  explicitly), never the page's — deriving it from the rows on a page would move
  the badge list a client watches for a column change every time the page moved.
  Evidence: `TestServe` "GET /headlines filter and paging", `TestQuery` "Search
  text". **test**
- **The `tag` COLUMN sorts, case-folded; the file and `hrTags` do not.**
  `Glance.Query.sortedTagsCell` is applied in exactly one place — the `tag` entry
  of `viewColumns` — so `:task:nl:finance:` draws and searches as
  `:finance:nl:task:` and a reader scans a tags column in one order rather than
  in the author's typing order. Everything else is the file's own order, and each
  for a reason it would be a bug to lose:
  - The FILE, because the span is never touched: materialize hands back the
    author's bytes and `addTagEdits`/`removeTagEdits` splice into the run as it
    is spelled, measured in offsets the cell knows nothing about.
  - `hrTags` itself, because `classify` reads it and `keywordScopes` is
    FIRST-WINS over the tags: sorting the field would move which tag's config
    governs the row, which is a resolution rather than a rendering. A row tagged
    `:pile:book:` is `pile`'s and one tagged `:book:pile:` is `book`'s, whatever
    order either cell draws in.
  - `GET /tags` and the manage-tags palette behind it, whose union stays
    first-seen in the order the rows and their files introduce the tags —
    `whichKeys` is order-dependent, so an insert in the middle would take a
    letter out from under the reader's fingers.
  `hrSearch` inherits the sort by construction, `searchTextOf` joining
  `viewCells` and `viewCells` reading the column accessors, so there is no third
  answer to keep in step. No predicate changes: `tag:x` is a substring of one
  tag, `tag:*archive*` is membership of the list, and both are order-independent
  — what DOES follow the cell is free text read across the join, so `glance:web`
  finds the row `:web:glance:` spells and `web:glance` no longer does. Folded so
  a capital does not sort ahead of every lowercase tag, and STABLE so two
  spellings folding alike keep the file's order between them. Evidence:
  `TestQuery` "the tags CELL sorts, case-folded, where the field keeps the file",
  "the column is what carries the sort", "and a removal cuts the file's entry,
  not the cell's"; `TestFilter` "a predicate is order-independent"; `TestConfig`
  "the first tag with anything to say about the keyword wins". **test**
- **`?q=` is SCHEMA.md's filter query, and parity with the renderer is the
  contract.** `Glance.Web.Filter` is a port of `table-view.js`'s `scanQuery`,
  `parseQuery` and `tokenTest`, term for term, because the renderer filters
  locally with the same grammar and a query that means two things is a table
  that disagrees with itself. Tokens split on whitespace and `&`; `key:value`
  (`=` alias) is a predicate only when the key is a column key, `planned` or
  `ref`, which is what keeps org cell text — `:work:`, `=code=` — from becoming
  one by accident; a token that *opens* with a quote is free text; a leading `-`
  negates. One resolution answers both "is this a key" and "what does it read"
  (`fieldOf`, `Nothing` where it names none), so the grammar and the matcher
  cannot disagree about a token. COMBINATION IS ONE RULE: TOKENS AND,
  ALTERNATIVES OR (its own entry below). Dispatch is
  on the KEY NAME, never on the column's declared `kind` — `Glance.Web.Filter`
  does not import it. `state` is whole-value case-insensitive plus this
  producer's `state:*active*`/`state:*inactive*` meta values — `*active*` ORing
  in the EMPTY cell, where `*inactive*` does not — `priority` is exact
  equality, `scheduled`/`deadline` are prefix, everything else is substring; so
  a column declared `badge` but named something else is matched as text, and the
  `priority` column, declared `text`, is matched exactly. That last pair agrees
  with the renderer only because the cell is one character long, where a
  substring test and an equality test cannot differ. Then three uniform rules:
  a predicate with no alternative left narrows nothing (`key:`, `key:|`), a
  value may be quoted, and `key:*empty*` is the empty
  cell — `tag:*empty*` is untagged. The tags column's key is
  `tag`, singular, so the key a filter names and the tags it names read alike
  (`tag:travel`); the header stays `Tags` and `hrSearch`'s field order is
  unchanged, since only the name moved. One consequence to keep: a predicate
  reads one `\x1f` field of `hrSearch` rather than re-deriving a cell, so
  per-cell matching and free text agree by construction. Evidence: `TestFilter`
  (tokens, predicates, tags, shape, degenerate parity with `matchesSearch`),
  `TestServe` "GET /headlines filter and paging". **test**
- **COMBINATION IS ONE RULE: TOKENS AND, ALTERNATIVES OR.** Every token narrows,
  whether or not another token names its key. `state:TODO state:DONE` asks a
  cell holding one value to hold two, which is no row; `tag:a tag:b` is a row
  carrying both and `ref:a ref:b` a row pointing at both; and a negation narrows
  the same way, so `-a -b` is neither. A row matching EITHER value is the one
  token `state:TODO|DONE`: a predicate's VALUE splits on `|` (`alternatives`)
  and each alternative is read as that key's own value, the results OR'd. The
  bar is uniform over every key and every kind of value — a badge stays
  whole-value per alternative (`state:TOD|DON` is nothing), a date stays a
  prefix, `planned` reads both its cells per alternative, `ref` resolves each id,
  and a starred meta alternates like any other value (`state:*active*|DONE`,
  `tag:*web*|*archive*`). A negation covers the WHOLE token, so `-tag:a|b`
  carries neither — which De Morgan makes the two negations too. EMPTY
  ALTERNATIVES ARE DROPPED: `a|` is `a`, `|a` is `a`, `a||b` is `a|b`, and a
  value spelled with bars alone is left with none — a predicate with no
  alternative narrows nothing, which is one answer for `key:`, `key:|` and
  `key:||` alike. The bar is a PREDICATE's: a free-text token is the text it
  spells, bar and all, and a token opening with a quote is free text whatever it
  spells. A predicate's value has had its quotes taken out by the scanner
  (`scanQuery` records that the token opened with one, never where the rest sat),
  so a bar inside a predicate is always the operator and a literal bar is free
  text's alone — the one thing the grammar cannot spell as a predicate value.
  `namesArchive` reads the alternatives too, so `tag:*archive*|web` turns the
  archive exclusion off the way `tag:*archive*` does. BREAKING against what came
  before: same-key repeats used to OR when the field was single-valued, so
  `state:TODO state:DONE` answered either state and now answers none; the
  replacement idiom is `state:TODO|DONE`. What that bought is the arity rule's
  death — `multiValued`/`manyValued` are gone from both sides, `compile` is
  `map inverted` over the terms with no grouping in it, and `multi: true` is
  left saying only what its name says (the cells hold a list), which the
  whole-tag meta and the renderer's chips read. Evidence: `TestFilter`
  "Alternation" and "Shape", `fixtures/parity/filter-query.json`'s alternation
  cases, the driver's `filterQuery`. **test**
- **AN ORG TAG NAMES NO KEY, and the one spelling is `tag:`.** `course:text` is
  free text, colon and all; `tag:course text` is what it meant, the predicate
  reading the tags cell and the free text reading the row, so nothing
  expressible is lost. The keys a query may name are the view's own — the
  columns, `planned`, `ref` — and none of them is derived from the ROWS, which
  is the point: they were the WHOLE STORE's tags here and the LOADED ROWS' tags
  in `table-view.js`, so a tag outside the client's page was a predicate on one
  side of the wire and free text on the other, and no schema revision mechanism
  exists to reconcile them. Three consequences are the price, and are written
  down rather than papered over: `tag:` matches its column by SUBSTRING where a
  tag key was whole-tag, so `tag:glan` finds `:glance:` where `glan:` found
  nothing; org spells a tags cell `:web:`, so the free text `web:` is still
  inside every row carrying the tag and answers the same rows a facet did; and
  `contact:none` — which meant "tagged `contact` AND the row text holding
  `none`", the one place `key:none` was not the empty cell — is gone with the
  branch that produced it, and the bare `none` is gone with it (`*empty*`). Evidence: `TestFilter` "Tags are not keys", which
  runs every fixture query against the store's real tag list and against an
  empty one and asserts the two answers are equal. **test**
- **`planned` is a key over the two date columns, and both sides can
  decide it.** A row is planned when its `scheduled` OR its `deadline` cell
  holds anything, so `planned:*empty*` is an entry nobody has put a day on and
  `-planned:*empty*` is the agenda's half of its query. Nothing renders a `planned`
  cell, so it is a key with no column behind it; a tree tagged `:planned:`
  cannot take it, there being no tag keys to take it with. Its value is the date prefix `scheduled:` and `deadline:`
  each take, asked of both cells at once (`planned:2026-08` is either date in
  that month), and it obeys the one combination rule like every key:
  `planned:X|Y` is either, `planned:X planned:Y` is a row whose two date cells
  meet both. That is the whole rule, and it is stated this
  way because the renderer has to answer it identically off the same two cells:
  no keyword set, no vocabulary, no clock. `Glance.Web.Filter.plannedKey`,
  `TestFilter` "Planned". **test** (this half; the renderer's is
  table-view's)
- **`ref:ROWID` is the key no row can answer alone.** It is every row
  whose subtree points AT the row named, so the question needs the TARGET's row
  as much as the candidate's: `FilterEnv` is what carries the store to the
  matcher and `ref:` is now all it carries, `storeEnv` resolves the id through
  the same id-resolved rows every other answer is built from (exact-string, the
  way `resolveIds` is), and `emptyEnv` is that environment for a caller with no
  rows behind it, where `ref:` still parses and matches nothing. What is matched is `hrLinks` against
  `refSpellings` of the target — its `ORG_GLANCE_ID` where it has one, plus its
  title, which is what the `[[Title]]` and `[[*Title]]` forms resolve against.
  A row is NOT its own reference: org-glance's materialize footer writes a
  self-link, and a referrer list holding the row you came from holds one
  useless entry. An id no row claims matches nothing and does NOT 400 — this is
  a filter rather than a command, so a stale `ref:` in a bookmarked URL opens an
  empty view. `ref:a ref:b` is a row pointing at both, the way every repeated key
  narrows, and `ref:a|b` is a row pointing at either. And its
  value is the ONE predicate value that is not case-folded, since a row id is
  exact-string: ~/sync carries ids spelled `Password-…` and `Pets-…` that a fold
  would put beyond reach. Evidence: `TestFilter` "References". **test**
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
- **Column lockstep is FOUR-way, and `hrSearch` is derived like the rest.**
  `Glance.Query.viewColumns` is the single source for four things — the `columns`
  array, `rowJSON`'s cells, `filterKeys`, and the field order inside `hrSearch`,
  which `viewCells` reads straight off the same table (`recordOf` ties the record
  through its own cells). A cell is `HeadlineRecord -> Maybe Text`, so `Nothing`
  is the row JSON's `null` and the empty field a filter reads, in one statement.
  The APPEND hazard is closed by construction: a seventh column used to leave the
  hand-written search list six fields long and every predicate past it read the
  wrong `\x1f` field, greenly. A REORDER was already caught, by the predicate
  cases (`TestFilter` 622-645) reading actual cells. What is left to keep in step
  by hand is `Filter.dateKeys` and `Filter.keyTest`'s name switch, neither of
  which is positional. `TestFilter`'s layout guard keeps its own hardcoded
  six-cell list on purpose: it is now the one copy of the layout that is NOT
  derived from the table, which makes it an INDEPENDENT ORACLE rather than a
  mirror — a test derived from `viewColumns` would agree with any reordering of
  it. Beside it, a case quantified over the columns there are ("every column is
  reachable by the key it declares"). **test** (the oracle, and the quantified
  case)
- **Which column holds a LIST is chosen by NAME here and declared to the
  renderer.** The server's multi-valued column is `tagsColumn`, the index of the
  key literally named
  `tag`, and the `tag` column now emits `"multi": true` — SCHEMA's declaration,
  which the renderer prefers over its own sampling. That sampling is what the
  declaration exists to retire: `multiColumn` reads up to 40 non-empty cells and
  needs at least two shaped like `:a:b:` with none contrary, returning the FIRST
  column in view order that qualifies, so a page with fewer than two tagged rows
  found no multi-valued column at all, one cell holding an unrelated colon — a
  `10:30`, a URL —
  disqualified the column outright, and a column earlier in view order whose
  cells happened to look tag-shaped stole both the verdict and the vocabulary.
  The verdict was re-derived on every row-set change, so it could flip between
  two pages of one session. What rides on it shrank when the arity rule died: the
  whole-tag meta (`tag:*archive*`), the chip rendering and the value domain,
  where the combination of two `tag:` tokens once did too — a page that found no
  multi column used to answer `tag:a tag:b` as an OR where the server ANDed, and
  now both sides AND whatever the verdict says. What remains is the version skew
  this whole section is about: an asset predating the field still samples.
  Evidence: `TestQuery` "the multi-valued column says so, and it is the only
  one", plus the golden. **test**
- **Date-ness is asymmetric the same way.** The server prefix-matches exactly
  two hardcoded key names; the renderer decides per column by sampling cells for
  date shape. A loaded set with under two dated rows makes the renderer treat
  `scheduled` as text, so `scheduled:10:00` matches `2026-08-15 10:00` there and
  nothing here; conversely any other column whose sample looks dated gets
  renderer-side prefix matching that the server never applies. `planned` inherits
  the whole of it, since what it reads IS the date-column set: on such a page the
  renderer finds no date column, so `planned:*empty*` is every row there and
  `-planned:*empty*` is none of them, while the server answers off `scheduled` and
  `deadline` as always. The predicate is term for term; the column set under it
  is what differs. **none**
- **`ref:ROWID` is producer-only WHOLE.** The starred metas at least reach the
  renderer as literal text; this key is undecidable there. Resolving a reference
  needs the TARGET row's `ORG_GLANCE_ID` and title, which the store has and a
  page does not, so `table-view.js` has no branch for the key and reads the
  token as FREE TEXT — a substring hunt for `ref:rowid` over the row's display
  text, which almost nothing matches. The renderer is therefore NARROWER, which
  is the tripwire's blessed direction (it fires on a server zero alone) and
  leaves that direction unmoved. What keeps the divergence workable is that no
  locally-filtered path ever applies a `ref:`: the shell mounts with `onFilter`
  so the server narrows, and a socket frame arriving under a filter refetches
  rather than splicing. **none**
- **`state:*active*` / `state:*inactive*` are producer-only in their KEYWORD
  half, and discoverable.** SCHEMA.md blesses producer meta-values, and the
  server resolves these two against the record's own `#+TODO:` sets — plus one
  term that needs no set at all: `*active*` also matches the EMPTY state cell.
  A stateless entry is live work, and the default view is exactly what would
  otherwise hide it, so the group has to take it; `*inactive*` does not, since
  an entry nobody marked done is not done. The two therefore do NOT partition
  the column: `-state:*active*` drops the empty cell along with the active
  keywords, and `state:*empty*` — still the only way to ask for the
  empty cell alone — is a subset of `*active*` rather than a third group.
  The empty half is spelled over the CELL rather than over `hrActive`, which is
  what makes it the same predicate `*empty*` reads and the one term a renderer
  can decide for itself. The starred form is the ONLY spelling — it is what
  org-glance calls the groups, and what the default view boots on — and the bare
  `state:active` is the literal keyword `ACTIVE`, which is what keeps every word
  a file could declare reachable. There is no glob and no alias: `state:*TODO*`
  is the literal badge text `*todo*`, which no cell holds, and a half-starred
  value is literal too.
  Discovery is the `values` array the state column now ships beside its
  `badges`, holding exactly `["*active*", "*inactive*"]` — SCHEMA's own route
  for meta-values, and the reason the starred spelling is the canonical one: it
  cannot be mistaken for a keyword. The renderer still has no group logic of its
  own, so a locally-filtered table matches these as literal badge text and finds
  nothing — except for the starred `*active*`'s empty-cell term, which
  `tokenTest` answers, so a local `state:*active*` finds the stateless rows and
  remains a subset of the server's answer. The bare `state:active` is a literal
  on both sides, matching having read the stars everywhere since the family went
  total.
  The rest of that asymmetry is intended, since the server
  knows the keyword sets and the renderer does not; the autocomplete still shows
  a meta dimmed and uncounted, its counts being per cell value and a fraction of
  the server's answer being no better a number than zero. The same split rides on each badge as
  a `group` field (`active` / `inactive`) — the bar in a `#+TODO:` line is not
  recoverable from palette order, the hues are not a contract, and the shell's
  value palette rules its hairlines on it. Additive: a renderer with no use for
  the field ignores it. KNOWN GAP: `table-view/SCHEMA.md`'s Badge object still
  lists `value` and `color` alone, and SCHEMA has no general "unknown fields are
  ignored" clause — so "a renderer ignores it" holds by how JS reads objects
  rather than by the contract. `multi` went INTO SCHEMA marked *Experimental*
  (so did `depth`, which this producer has since stopped emitting — rows are top
  entries and describe no outline); this one owes the same row, one repo over.
  Evidence:
  `TestFilter` "and answer to org-glance's starred spelling of the same
  groups" and "the stateless row is active, and it is not inactive", `TestServe`
  "the default view carries the entry nobody stated", `TestQuery` "and the two
  group values a filter can name" plus the `sample-view.json` golden; over the
  wall, `fixtures/parity/filter-query.json`'s four meta cases and the driver's
  "== producer meta-values". **test** (the producer's half) / **none** (SCHEMA's)
- **The two vocabularies had different scopes, and that is why neither has one.**
  The server parsed against `storeTags` — every tag in the tree — while the
  renderer's `tagVocab` iterated the rows it currently held, so a tag present in
  the store and absent from the page was a predicate on one side of the wire and
  free text on the other. It was the divergence the tripwire was built for, and
  no schema revision mechanism exists to reconcile the two derivations. Both
  sides now name their keys — columns, `planned`, `ref` — and `tagVocab`
  survives as the tags column's VALUE DOMAIN, which is a completion aid rather
  than a grammar. **test** (`TestFilter` "Tags are not keys")
- **Keys are case-sensitive on both sides; values are folded on both.** The
  server tests membership of `filterKeys` by exact
  `elem`, and every real key is lowercase — `filterKeys` are written that way;
  the renderer does the same with a
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
  It does READ them — a route's own resolution is where the extent and the
  pinned digest come from — and that read is the point: the write is measured
  against what the store already holds. The invariant is one-directional, and
  stating it as "the route does not touch the store" is wrong in a way that
  makes the digest lock
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
- **`GET /links?id=ROW` is where a row points, and the rule is the DISPLAY
  rule.** `{digest, links: [{target, desc, type, span}]}`, extracted from the
  row's SUBTREE in order of appearance and one entry per (target, shown)
  pair. Two forms,
  which is what org writes:
  the bracket link, described by its `DESC` and by its target where it has none
  — `Glance.Query.linkAt` is the grammar and `linkShown` the display rule, the
  very pair `displayText` reads a cell with, so
  what `/links` calls a link is what the table shows for it — and the plain
  `http(s)`/`mailto:` URL, which is its own description. A plain URL is a WORD
  (a URL carries no whitespace), it must open at a non-word boundary so
  `xhttp://a` is not one, and the sentence punctuation `.,;:!?'"()[]{}<>` comes
  off its tail so `(https://x.org)` points where it reads as pointing. The scan
  is one left-to-right pass over the bracket links, so a `[[https://x][y]]`
  never also reports its own target as a bare URL. The dedup key is the pair a
  reader can SEE: a target respelled under the SAME description keeps the FIRST
  occurrence — its span, so an edit through it edits the first spelling and the
  others go on pointing where they did — while the same target under ANOTHER
  description is another entry. Keyed on the target alone, a tree writing one
  `elisp:` command under `pnl` and `alpha:grafana` served pnl and swallowed the
  second, which read as the link not parsing (2026-08-04, the real blob that
  found it). It is
  SERVER-side because it is org text work: the page holds no org parser and must
  not grow one, and a JS copy of the bracket grammar would be a second grammar
  to keep in step with SCHEMA.md's link rule. The SUBTREE rather than the cells,
  since an entry keeps its references in its body. A read: 404 on an id the
  store has no row for and 400 with no id, exactly as materialize, 503 while
  indexing, 405 on POST. Evidence: `TestQuery` "Links" (the rule),
  `TestServe` "GET /links" (the route). **test**
- **The answer is WRITEABLE, which is what `span` and `digest` are for.** `span`
  is the half-open CHAR range the link occupies in the FILE — the scan runs over
  the subtree slice and `subtreeLinks` shifts every span by where that slice
  starts — and `edit-link` takes exactly that range back. `digest` is the file's
  as the store holds it, and it is the LOCK: the spans describe the text this
  store last read, so a client that pins it (`digests` on `POST /command`) is
  refused rather than spliced blind. The disk-drift check in `replaceSpans`
  already refuses a file that moved under the daemon with no pin at all; what the
  pin adds is refusing one the STORE has re-read since, whose spans are somebody
  else's. One scanner answers
  all three questions asked of a bracket link — `showLinks` what it SHOWS,
  `orgLinks` where it POINTS, and the span itself where it SITS (`subtreeLinks`
  shifts it, `editLinkEdits` validates what comes back) — so a second pass is
  a second grammar. `linkAt` reports the width it consumed rather than measuring
  what is left, so a scan costs the links it finds rather than the tail behind
  each of them. Evidence: `TestQuery` "a link spans exactly the characters that
  spell it", "a row's link spans are offsets into the document it was read from",
  `TestServe` "every link carries the file range that spells it", "and the digest
  those spans were measured against". **test**
- **`edit-link` is the ONE command whose args name a row's own TEXT, so it names
  ONE row.** A span means nothing to a second row, and over two files it would
  name a different range in each. The rule is the command's own `csArgs`, which
  is handed the IDS beside the `args` because a shape refusal is about the
  REQUEST: seven of the eight commands ignore the list, and `wantsLink` names the
  count FIRST, ahead of the span and the target it also owes. CHARACTERS, like
  every other span in this codebase. THE FORM IS PRESERVED, which is what makes
  it a link edit rather than a rewrite of the text around one: `[[T][D]]` keeps
  its description under a target-only edit, `[[T]]` stays desc-less, a plain URL swaps its target
  and stays plain, and a description ARRIVING is the one thing that changes a
  shape — a plain URL has nowhere to write one, so it brackets. ABSENT IS NOT
  NULL (`.:!` rather than `.:?`, the whole command layer's discipline): a request
  saying nothing about the description leaves the author's, `null` takes it off,
  and a description that SHOWS nothing is the null spelled another way, since
  `[[T][]]` shows its target — the emptiness test strips and the value is
  written verbatim, content being nobody's to trim, which is the target's own
  rule. TWO WALLS, both the lens rule as a refusal: the
  span must sit inside the ROW's own subtree — a span outside it would let one
  row's write reach text no reader of that row was shown, under that row's
  digest — and must cover exactly one link EDGE TO EDGE; and the REPLACEMENT must
  read back as THE LINK IT CLAIMS TO BE, since `Data.Org.Edit` is
  content-agnostic by law and this is the layer that owes the check. That second
  wall REPARSES AND COMPARES rather than checking the shape, which is what a
  target spelling `a][b` needs: it renders `[[a][b]]`, which IS one link —
  pointing at `a`, described `b`, neither of them what was asked for — so a shape
  check alone would bless a link pointing somewhere the request never named. A
  NEWLINE is refused ahead of both, and it is the one thing reparsing cannot
  catch: this scanner has no line rule, so `[[a\n* B]]` reads back as itself and
  lands a column-1 star, which the ORG parser reads as a new headline — the
  subtree splits and a row appears that nobody wrote. Each is a 400 naming what
  it turned down. Evidence: `TestQuery` "edit-link" (the form table, both walls),
  `TestServe` "POST /command edit-link" (the round trip, the pin, the ids rule).
  **test**
- **A link's TYPE is its scheme, folded, and the rule is one pass over the
  PREFIX.** `Glance.Query.linkType` takes what sits before the first `:`,
  lowercases it, and answers with it — after refusing anything not shaped like
  RFC 3986's scheme (a letter, then letters, digits, `+`, `-`, `.`) and folding
  every `org-glance-*` protocol into the one word `glance`. So `https`, `http`,
  `mailto`, `id` and `file` fall out of the rule rather than being named
  anywhere, and `linkTypes` is a vocabulary the badge palette draws hues for
  rather than a classifier. The `org-glance-*` fold is deliberate and is a DIFFERENT question
  from `refPrefixes`: `org-glance-visit:` and `org-glance-overview:` name a row
  and a tag respectively, which is what decides `hrLinks`, but they are the same
  KIND of destination, which is what this answers. A scheme the six do not name
  travels under its own name — the popup exists to say what a link IS, and a
  catch-all would teach less. That last clause earns itself on the corpus: a
  300-row sample of ~/sync at 2026-08-02 answered `glance` 427, `https` 286,
  `file` 68, `http` 18, **`elisp` 6, `attachment` 2** and `other` 1, so two org
  link types nothing here declares came back named rather than swept away.
  Three honest costs, all from reading the
  prefix alone: org's internal `[[Title]]` and `[[*Title]]` are `other` (they
  name a place inside the tree, not a protocol); a relative path written without
  `file:` is `other` where `file:./x` is `file` (the type reports what the target
  SAYS); and a scheme-SHAPED word before a colon is taken at its word, so
  `[[Meeting: notes]]` reads `meeting`. The alternative to that last one is a
  registry of known schemes, and then an unlisted scheme reads as prose — the
  worse failure. The shell's `followable` reads this word rather than running its
  own regex over the target, so the badge a reader sees and the judgement `o`
  makes are one answer. Evidence: `TestQuery` "a link's type is its scheme,
  folded" and the four cases beside it, `TestServe` "every link carries its type,
  followable or not". **test**
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
- **The renderer is compiled in, and `--assets` REPLACES it.** `embeddedRenderer`
  is `assets/table-view.js` read by a Template Haskell splice, so the binary is
  the whole deployment: no directory beside it, no path off this repo, and a
  `glance` copied anywhere serves the same page. `assetSource` is the one place
  the two cases meet and they are exclusive — with `--assets` the named
  directory is the whole asset set, so a directory without a renderer in it does
  not silently fall back on the compiled one; that is what keeps `assetsMissing`
  reachable and honest, and it is now reachable under that flag alone. Both
  cases leave `asset` by the same door: one `mimeOf` content type, and
  `compressed` compresses either — a `responseFile` because `GzipCompress` says
  so, a `sized` body because a `Content-Length` is what the threshold reads. No
  FONT is embedded: `localFont` answers `Nothing` without `--assets`, so the
  `@font-face` stays exactly the `--assets` affordance it was. Evidence:
  `TestServe` "Embedded renderer" — the served bytes equal the file in the tree,
  `/` is the shell and never the JSON-only page, the fixture directory's stub
  wins under `--assets`, and a bare directory still gets the JSON-only page.
  **test**
- **No source file names an absolute path outside the repository.** Until
  2026-08-02 `defaultAssetsDir` was one author's home directory, read at run
  time, which made a correct build serve a table-less page on every other
  machine. The rule that replaced it is swept rather than reviewed:
  `TestSelfContained` reads every `.hs` file under `src/`, `src-query/`,
  `src-web/`, `src-desktop-native/` and `app/` and fails on the string `/home/`,
  after first asserting it swept `src-web/Glance/Web.hs` and at least a dozen
  files — a sweep that finds nothing passes, so it says what it looked at. The
  same module asserts the `sync-renderer` target exists, since a vendored file
  with no way to refresh it is a fork. A module of its own because neither case
  drives the server: repo hygiene has no `Application` to hand it. **test**
- **`Content-Length` comes from `sized`, and `Vary` from the gzip middleware.**
  `sized` writes the length on every JSON, HTML and plain response, the HTTP 503
  included; warp supplies it for the 304 and for `responseFile`. The gzip
  middleware writes `Vary: Accept-Encoding` on every HTTP response, 304s
  included — and NOT on the websocket rejection, which sits outside it. A client
  caching the WS refusal has no `Vary` to key on, which is harmless only because
  nothing caches a 503. **test** (the HTTP half)
- **The served pages fetch nothing off this server.** Styles are inline, the
  glue is inline, and the one `<script src>` is a file name the asset route
  answers out of the binary, or out of `--assets` when that is given. No CDN, no
  web font, no analytics — a page that reaches the network renders differently
  on a laptop in a tunnel, and this daemon's whole point is that the org files
  are local. The JetBrains Mono `@font-face` is the shape a resource takes here:
  emitted only when an `--assets` directory holds the file, pointing at a bare
  name this server serves. Evidence: `TestServe` "no page this server serves
  reaches off it" — neither page contains `http://`, `https://` or `@import`.
  **test**
- **The shell's keymap is data, and there is ONE of it.**
  `Glance.Web.Keymap.keyBindings` is the one table; the page carries it as a
  `<script type="application/json">` blob — `{rows, hints, reserved, once}` — and
  its own dispatch parses that blob, so a binding cannot exist in the handler and
  not in the map. The movement PROFILES are gone, and with them a selector in the
  status corner the page still had then, a `localStorage` key, a `?keys=`
  parameter, a `setProfile` and a key
  line that had to be rewritten whenever the profile moved. What replaced them is
  two rows apiece: `n`/`p` and `j`/`k` both step a row, `f`/`b` and `l`/`h` both
  step a cell, and both spellings are live at once because a table has no text
  field to compete with. The ARROWS ride both axes beside them — `<up>`/`<down>`
  a row, `<left>`/`<right>` a cell — and they ride SILENTLY: `hints` shows a
  command's FIRST binding, so the key line reads `n/p rows · f/b cells` and has
  never named an arrow. Same handler either way, so an arrow walking off the
  last cell lands in the whole-row look rather than meeting a wall. The ends are `<` and `>`, with vi's `G` beside `>`; `g`
  is `apply-default-filter`, `a` is `org-glance-agenda`, `,` is `customize`, `o`
  and `!` are `org-glance-overview:open`, `@` is
  `org-glance-overview:relations`,
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
  `archive-flag`, `org-glance-overview:delete`, `org-glance-overview:open`,
  `org-glance-agenda`, `org-glance-overview:relations` — which holds under both
  spellings
  of a command and takes the repeat off nothing else. The three writes are there
  for a different reason than the token strip: a held key must not be a hundred
  `/command` requests. The last three write nothing and are on the list for the
  same shape of reason: a leaned-on `o` is a browser tab per repeat, a
  leaned-on `a` a remount per repeat, and a leaned-on `@` a remount per repeat
  that leaves a crumb behind each time, so a held key would build a trail of
  identical steps for `DEL` to walk back one at a
  time. `archive-flag` needs it most of all, since a repeat that
  survived would flag a row and archive it from ONE press, which is exactly the
  confirmation the two-press shape exists to be. `m` and `u` stay off the list on
  purpose: both advance, so a held one walks a column laying marks down rather
  than working the same row twice. Evidence: `TestServe` "Shell keymap", which
  parses the blob, compares it to a written-down map, checks the two uniqueness
  rules, asserts that both spellings of row and cell movement are present, and
  asserts that the profile machinery is absent from the page. **test**
- **A letter binding names a PHYSICAL key, and the split is one function's.**
  `keyName` is where every listener on the page — the dispatch, the sheet, the
  value palette, the popups — turns a press into a name, so a rule spelled there
  reaches all of them at once and cannot be half-applied. `e.code` matching
  `KeyA`–`KeyZ` answers as that letter in lowercase, and `shiftKey` as the
  UPPERCASE binding rather than an `S-` modifier: `d` flags and `D` archives, and
  no layout can collapse the two into each other. A chord's second key is a
  letter like any other, so `C-c C-t` completes on the physical `t` — the
  reserved-chord rule is untouched, both presses still claimed. EVERYTHING ELSE
  IS THE CHARACTER `e.key` reports: the named keys (`RET`, `TAB`, the arrows,
  `DEL`), the function keys, and the PUNCTUATION — `^ : + < > [ ] / , ! @` sit at
  a different position on every layout, so there is no position to bind and the
  character is the honest answer. A press carrying no `code` at all falls back to
  it whole, which is what a browser sending none gets and what every other case
  in the suite presses. The reader this is for had the Cyrillic layout up and a
  table that would not move: `т з о л` are now `n p j k`, `в`/`В` the archive
  pair, `е` the `t` completing `C-c`. TWO CONSEQUENCES, named rather than worked
  around: the map is QWERTY's POSITIONS, so a Latin layout that moves its letters
  (AZERTY, Dvorak) reads its own `a` as this map's `q`; and a layout that spells
  no `<` or `[` — the Russian one does not — cannot reach the punctuation half at
  all, the letters still carrying movement, marks, states and the archive.
  Evidence: `TestServe` "Shell layout", which presses `{key: "т", code: "KeyN"}`
  and the rest through the harness, pins the fallback and the punctuation halves
  as unmoved, and counts the readers of `keyName` so a fifth listener cannot
  answer the question its own way. **test**
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
- **`*word*` is the reserved-meta form, and the family is TOTAL.** A starred
  word marks a value with semantics of its own — never a literal keyword, never a
  cell value a file could hold — and NO BARE WORD IS RESERVED ANYWHERE, which is
  the property that makes the form worth having: every spelling a cell can hold
  is reachable as itself. `state:none` finds a keyword `NONE`, `tag:archive`
  finds a tag holding those letters, `state:active` finds a keyword `ACTIVE`.
  The family: `*empty*`, the empty cell on EVERY column key and on `planned`;
  `*archive*`, the whole ARCHIVE tag on the `tag` column and the one query that
  lifts `/headlines`'s exclusion; `state:*active*`/`state:*inactive*`, the
  filter's group metas, evaluated by the producer; and `*empty*` again as the
  state palette's take-the-keyword-off entry, which is the same word for the
  same cell — the entry takes the state to exactly what `state:*empty*` finds —
  committed as a null keyword. The first two are decided off the CELL, so the
  renderer answers them identically and the parity vectors bind both sides; the
  group metas need the keyword sets and are the producer's alone. The two group
  metas are asymmetric over the row that carries no keyword: `*active*` takes it
  (a stateless entry is live work) and `*inactive*` does not, so they name two
  overlapping sets rather than a partition, `-state:*active*` drops the empty
  cell, and `state:*empty*` is the explicit spelling for that cell alone. A
  future meta joins the family by wearing the stars. The convention is ENFORCED
  from two sides rather than by a rule of its own: `setStateEdits` refuses any
  word no scope of the row's chain declares, and `Data.Org.Parser.keywordTextP`
  admits letters and underscores alone, so a starred word cannot be declared and
  therefore cannot be set — a guard against the group names inside `configEdits`
  would be unreachable code. The same wall stands on the tag side without a rule
  of its own: `Data.Org.Parser.isTagChar` has no `*`, so `add-tag` refuses
  `*archive*` and no file can spell a tag the meta would collide with. Matching reads the stars everywhere
  (`Glance.Web.Filter.metaOf` answers a starred value's word and `Nothing` for
  any other), so there is no alias and no glob: `state:*TODO*` is the literal
  badge text `*todo*`, which no cell holds. Star-blind matching survives in the
  RENDERER's completion alone, where `arch` reaches `*archive*`. Evidence:
  `TestFilter` "Starred metas" (per-key `*empty*`, the literal `none` and the
  literal `archive`, the whole-tag meta), `TestConfig` "what a layer may say, and
  what it may not", `TestQuery` "Commands", `TestServe` "the meta entry clears
  the keyword rather than setting one", and
  `table-view/fixtures/parity/filter-query.json`'s eight family cases. **test**
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
  in the keymap rather than in the dispatch: a plain `t` reaches the palette
  everywhere, and the org chord stays bound as the secondary spelling for
  browsers that deliver it. The native window is the other half of the fix and
  removes the cause: a bare `WebKitWebView` in a plain `GtkWindow` has no chrome
  to bind `Ctrl+T`, `Ctrl+N` or `Ctrl+W` to, so every chord the page claims
  reaches it. That claim is reasoned rather than measured: the flagged build
  compiles here now (Desktop, the vendored bindings) and nothing has opened its
  window, so this is the first thing on the eyeball list. **test** (the page's
  half) / **none** (the browser's, and the native window's)
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
- **The echo widget's key hints are data too.** `Glance.Web.Keymap.keyHints` is a
  table
  of key-list/label pairs serialized into the same JSON blob the dispatch reads,
  under `hints`, and rendered into the resident key line from there. So the line
  cannot offer a key nothing is bound to, and a new binding that should appear
  in it is one table entry rather than an edit to a string. **test**
- **The echo speaks the command's FUNCTION NAME, verbatim.** Commands are named
  as elisp functions — `next-row`, `last-row`, `org-glance-overview:delete` —
  and the pill reads `SEQ → command`, with anything else the key wants to say in
  brackets after it (`> → last-row (page 2/129)`, `m → mark-toggle (marked ·
  2)`). Never the prose spelling: a rebinding config will address a function by
  exactly this string, so a reader who learns one off the pill has to be able to
  type it back. One helper emits the shape — `said(b, what)` — and every keyed
  echo goes through it or through `run`'s default, which is the same shape with
  the row's `kbHelp` after a `·`. The resident key line is the exception and is
  meant to be: its labels are curated prose (`rows`, `pages`), because it names
  a GROUP of commands rather than reporting one that ran. **test** (the sweep
  reads every `${b.seq} → ` in the glue and requires `${b.command}` behind it,
  and no command in the blob may carry a space)
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
- **A view swaps ON ITS ANSWER.** The table on screen stands until the new rows
  are in hand, and then goes in ONE mount: the row count the renderer is handed
  never passes through zero, and never through a partial set either, unless the
  answer itself is empty. That is what decides the fetch a re-application makes.
  A BOOT has nothing on screen, so it asks for `?limit=100` and pulls the rest in
  behind the painted table — there the first page is the difference between a
  table and a blank page. A RE-APPLICATION (`g`, `a`, `@`, a pop, a
  `view-changed` remount) has a whole table standing, so `start` reads `!!table`
  and asks for the WHOLE answer once. Asking for a page there replaced a complete
  view with a hundred rows and reflowed the pager, the hint and the row heights
  under the reader a moment later, which is the flash `g` was reported for. The
  cost is that a re-application waits on the whole set rather than showing a page
  of it early, which is what the wash below exists to say. Evidence: `TestServe`
  "Shell wash", which reads the row count handed to the table at every mount and
  every `setRows`, plus "Shell boot" and "Shell reconnect", whose fetch lists now
  spell one URL where a remount used to spell two. **test**
- **The stale wash: ONE mechanism, TWO triggers, one clear discipline.** What is
  on screen stops being known to be current in exactly two ways — the view is
  being replaced and its answer has not landed, or the socket that would deliver
  a change is gone — and a reader can tell neither from a page that is simply
  quiet. Both wear one look, carried by ONE class (`stale`) on the document
  element and ONE declaration: `opacity:.55`, eased over 180 ms. Never blurred,
  because a stale row is still the row and has to stay readable while its
  replacement is on the way. Never `filter`, either, and that is a cross-repo
  constraint rather than a preference: any `filter` makes its element the
  containing block for `position:fixed` descendants, and the renderer's summoned
  filter palette is one — a `.tv-veil` inside `#app` — so a saturation wash would
  stop it covering the viewport and let `.tv-root`'s `overflow:hidden` clip it,
  every time a fetch went past its grace with the palette open. `opacity` creates
  a stacking context and no containing block, so it dims everything and
  re-anchors nothing; against the page's own ground it takes the colour out of a
  badge as it goes. It covers `#app` and
  the whole modal band (`#modal`, `#prompt`, `#config`): a sheet open over stale
  rows is stale with them, and floating clear of the wash would say otherwise.
  The event strip and the key line are EXEMPT by omission — they are
  where a reader finds out why, and dimming the answer along with the question
  leaves the page saying nothing. Each trigger arms on a DELAY, which is the
  whole of what keeps the wash off a page that is working: a view fetch at 300 ms
  and a lost socket at 400 ms, so a fetch that answers quickly and a socket that
  blips and comes back dim nothing at all. One state holder (`wash`) carries both
  — a count, a timer and an on-flag per reason, one `arm`/`off` pair, one `show`
  that toggles the class — and whoever arms a reason is who clears it. The two
  differ only in who counts: a view fetch STEPS the count, since `load` overlaps
  an abort with the fetch that replaced it and a boolean would clear the wash the
  replacement still wants; the socket SETS it, since a connection refused closes
  without ever having opened and would otherwise arm twice against one open. Only
  fetches whose answer REPLACES the rows hold it (`viewing`): the parity baseline
  and `@`'s probe go through `load` without it, since dimming a page for a fetch
  that will not change it is the same lie the other way round, and a boot holds
  nothing because a page with no table on it has no stale content to wash. The
  page never READS the class — the look is entirely the stylesheet's, and the
  suite forbids a branch here asking whether the wash is on. Evidence: `TestServe`
  "Shell wash" for the behaviour (armed past the grace and cleared on the answer,
  nothing at all inside it, an abort handing the wash to its replacement, a
  socket blip against a socket that stays gone, and an open sheet washed with the
  rows under it) and two `Shell glue` rows for the holder and the selectors,
  the second forbidding `filter:blur` and any `html.stale` reaching the exempt
  parts. **test**
- **The shell's z-index bands must clear the renderer's.** Three values, all of
  them here: echo `2`, modal backdrop `100`, sheet `101`. `3` was the status
  corner's and went with it; the value is unused and the suite FORBIDS it coming
  back. The value
  palette, the settings sheet, the link popup and the tags popup share that pair
  rather than adding to it (`#modal,#prompt,#config,#links,#tags` and their
  boxes), so a fifth overlay costs no band — which is the rule a new one joins
  under, and which #55 took without touching a value. The
  cross-repo constraint is the backdrop pair clearing the renderer's sticky
  header (`1`) and its completion list (`5`) — an unnumbered backdrop painted
  under both. The echo sits BELOW the backdrop deliberately, so it dims with
  the page; a consequence worth knowing is that it also sits below the
  completion list, which paints over it. The filter palette carries
  no shell z-index at all: the overlay is entirely the renderer's, and the suite
  forbids this page naming its parts. **test** (`Shell glue` "the sheet's
  backdrop covers the renderer's chrome" pins the three values and forbids
  `z-index:3`)
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
  is `.tv-root`'s font, `.tv-chips`/`.tv-chip` under a coarse pointer, the
  selected-row read kept as the legacy-asset fallback, and — for the property
  panel — `.tv-box`'s chrome and one row's box. A SECOND list guards a different
  thing and is worth not conflating with this one: `.prow`, `pcur`, `drawRow`
  and `addRow(` are this page's OWN deleted identifiers, forbidden so the panel
  the mount replaced cannot come back beside it. **test**
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
- **Cell movement walks OFF the cells rather than bumping.** `f`/`b` (`l`/`h`)
  past either end hands the out-of-range index straight to `select(id, want)`;
  the renderer reads a column index outside the table as no column at all
  (`cellCol`), so what comes back is the whole-row look a selection starts in,
  echoed `row mode`. The clamp this page used to keep returned BEFORE the select
  and echoed `at first` / `at last`, which swallowed the key at a wall the
  renderer does not have — the glue guard now forbids both strings, and
  `want >= cols.length` with them. Re-entry is unchanged
  (`at === null ? 0 : at + step`), and the landing column is read back out of
  `column()` rather than off `want`, since the renderer's answer is what decides.
  Evidence: `TestServe` "the landing column is echoed by its header, or the row
  mode it left for". **test**
- **The page never scrolls; the boxes inside it do.** `body` is `100vh`,
  `overflow:hidden`, a flex column of table, log and key line. Table and log are
  both `flex:1 1 auto` and both scroll inside themselves, the log stopping at
  its cap, so the table takes whatever the strip gives up; the key line is
  `flex:none` and scrolls sideways rather than wrapping. So an arriving message
  never moves the key line, which is the whole point: the one piece of chrome a
  reader looks for holds its place. The padding is one value on all four sides
  (`padding:24px`) — the extra 10px on top was the status corner's room and went
  with it, so the table starts where the page does. **test** (`TestServe` "with
  assets, the page is one column the viewport tall" pins the column and the
  padding)
- **The log strip is APPEND-ONLY, and `append(scope, severity, message)` is the
  whole of its interface.** A line is `HH:MM:SS SEV scope message`: the stamp
  muted, the severity in colour (`info` muted, `warn` `--g-warn`, `error`
  `--g-bad`), SPELLED uppercase (`INFO`, `WARN`, `ERROR`) so it is what a reader
  scans a screenful of chatter for and WORN lowercase as the line's class, which
  is the name the stylesheet and the suite use — one value, two cases, and the
  upcase happens at the one place the word is drawn. The scope is one
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
- **The strip's HEIGHT is a preference, and the stylesheet keeps the
  arithmetic.** `#log` declares `--g-logn:7` and caps itself at
  `calc(var(--g-logn) * 1.5em + 2 * 6px + 2 * 1px)` — N of its own line boxes,
  plus the padding twice and the border twice that `border-box` puts inside the
  figure. The knob writes a NUMBER onto the element
  (`el("log").style.setProperty("--g-logn", String(n))`), so the formula is in
  ONE place and a page whose glue has not run — or a reader who never touched
  the field — is capped at the same figure the sheet would put back. The value
  lives in `localStorage` under `glance-log`, beside `glance-theme`, applied at
  boot (`setLogLines(logLines(logPref.get()) || LOG.def)`) and on every accepted
  keystroke. `LOG = { key: "glance-log", def: 7, min: 1, max: 50 }` in the glue
  is mirrored in Haskell as `logLinesDefault`/`logLinesMin`/`logLinesMax` and
  `logLinesBand` (the placeholder's `1–50`), and the stylesheet's declared value
  is spelled from the same constant, so the three cannot drift. `logLines(text)`
  reads the field: blank is the DEFAULT, which is how a reader asks for it back;
  a whole number inside the band is that number; everything else is `null` —
  DECLINED rather than clamped, so the cap a reader had stands and nothing is
  stored, and half a number on the way to a whole one is the ordinary case of
  that. The box can therefore hold a refused value, and reopening the sheet
  draws the preference back over it. AN EMPTIED FIELD REMOVES THE KEY rather
  than storing `""`: a preference spelling the empty string is still a
  preference, and what the reader asked for is the absence of one — which is
  also the state a stored value the band no longer takes falls back to, since
  the boot reads it through `logLines` like anything else. Applied on `input`
  rather than `change`,
  which is what makes the field a knob rather than a form: a preference a reader
  has to leave the field to see is one they cannot aim. `LOGCAP` = 500 above is
  a DIFFERENT limit and the two are easy to confuse — that one is the RING, how
  many lines are KEPT — so the suite forbids the ring being spelled off the
  knob's own constants. **test** (`TestServe` "Shell settings" — the knob applying as it is
  typed and being remembered, the untouched default of seven with the key
  absent, a browser that arrives remembering one booting at it and opening the
  sheet on it, a stored value outside the band booting at the default, blanking
  restoring the default AND removing the key, a value outside the band and a
  value that is no number at all both
  declined with the cap standing, and a reopen drawing the preference back over
  a refused value; `Shell glue` "the log's height is a stored preference the
  general panel edits" and "the log wears the
  table's container under it", which pins `--g-logn:7` and the `var()` formula.
  The boot's read is unreachable from an act, so `bootWith` seeds
  `localStorage` from argv ahead of the glue)
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
  chip row as a 44px tap target, its empty-state label, the sheet's panes
  stacked whatever the width, and the 16px fields that stop iOS zooming in and
  never zooming back out. Keeping them in
  one block is what makes "a mouse sees none of this" checkable by reading a
  single place, and the tap handler asks the same query before it runs.
  **KNOWN GAP: a coarse pointer cannot open the settings.** The gear was its one
  door to `,`, which cannot be typed there, and the gear went with the status
  corner it sat in. A touch reader can filter and can read; nothing on the page
  opens the sheet for them. The gap is recorded rather than papered over, and
  the comment owning the question lives inside this block — whatever answers it,
  a chord surface, a long press, a control inside the sheet band, is one place
  and this is where the query for it already is. **test** (`Shell glue` "the
  settings door a coarse pointer had went with the corner", asserted from both
  sides: the block intact, no gear anywhere)
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
- **Eight keys write without a sheet, and none asks for confirmation.** `D`
  archives over the FLAGGED set; `t` / `C-c C-t` set a state, `:` manages tags
  and `C-c C-s` / `C-c C-d` set a planning entry, all four over the MARKED one;
  each falls back
  to the row at point — dired's rule, and org-glance's. `+` captures and takes no
  rows at all. `o` follows a row and, where it raised the popup, `RET` inside it
  edits a link over the row the popup was raised for (`lfor`) — neither the
  marked set nor the flagged one, the way `:` reaches its own writes. They are
  `POST /command`: the page sends row ids and a name and the server computes the
  spans — `edit-link` alone sends a range, which is a range the SERVER measured
  and handed out — and the table is not touched at all, the rows
  arriving over the socket once the watch has re-read the files. There is no
  confirmation step and there should not be: the drift lock is the safety, `D`
  archives rather than deletes, and org-glance's own rhythm is a key that acts.
  The pill counts what landed and the log names every row — a line per landing
  and a line per refusal, which is what a per-file answer needs: a set spanning
  three files can come back two-thirds applied.
  `D` keeps org-glance's command name, `org-glance-overview:delete`, and earns a
  `kbHelp` because the name is wider than the behaviour. Evidence: `TestServe`
  "Shell commands", which drives both keys through the node harness and asserts
  the bodies they posted. **test**
- **The same overlay collects a LINE, and that is all `askText` is.** `+`,
  `C-c C-s` and `C-c C-d` need one line of text rather than a choice, and they
  raise `#prompt` with `prompting.text` set: no list, no letters, RET commits the
  field as typed and every other key is the field's own. It is the value
  palette's overlay rather than a widget of its own because everything a prompt
  owes is already there — the z band, the blur on the way out that keeps
  `typing()` honest, and ESC through the keymap's `cancel` — and because a second
  overlay would be a second thing to keep in step with all three. `+` sends its
  line whole; the two chords send it as `date`, where an EMPTY line is the null
  that clears the entry, so the key that sets a date is the key that takes one
  off. Evidence: `TestServe` "Shell capture and reschedule". **test**
- **`+` IS ONE FORM, and ESC anywhere closes it with nothing sent.** The chain
  of palettes it replaces (2026-08-04, the same day it landed) closed and
  reopened the overlay per step, which read as a blink. `#capture`/`#kbox` is
  its own `SURFACES` entry between the palette and the link popup: the tag
  field with the tree's vocabulary narrowing under it (substring over the
  folded spelling, at most eight shown, `C-n`/`C-p` and the vertical arrows
  walking a highlight RET takes; no highlight commits the field as typed, so a
  name of the tree's own is reachable and the charset wall stays the
  server's), then one field per `%^{PROMPT}` GROWN IN PLACE when the tag
  settles — RET or TAB out of its field, since only the server knows the
  prompts, and editing the tag afterwards clears the grown fields as
  describing a template the field no longer names — then the line. RET moves
  the focus forward and at the line captures; TAB is its quiet twin; an EMPTY
  tag settles to the untagged inbox path exactly as it was. A refusal — the
  server's, or the empty line's — keeps the form UP with everything typed:
  `shutCapture` runs on the 200 alone, so fixing a line is an edit rather
  than a retype. The page holds no template grammar — what it grows is what
  `/capture?tag=` said to ask — and the form's keys are a document listener
  behind the dispatch, gated on the focused field. Evidence: `TestServe`
  "+ is one form, and an empty tag is the inbox", "a tag's template grows its
  fields in place", the three ESC cases, "a refused capture is one cmd error
  line, and the form stays". **test**
- **A capture SAYS WHERE POINT IS OWED, and `arriving`/`arrived()` is `leaving`'s
  mirror.** The answer names the row the write made, the shell keeps it as a
  one-shot, and the same three doors that spend the archive's anchor spend this
  one: the filtered refetch, the unfiltered splice, and a reconnect's repaint.
  It is `land`'s ordinary rule asked ONLY where there is something to land on — a
  filter that hides the new row, a page it is not on, or a watch step that has
  not delivered it yet all leave point exactly where it stands, since `land`
  falls through to an INDEX and there is no honest index to fall to here. Both
  anchors are dropped by a commit and by a remount, for one reason: an anchor
  belongs to its view. KNOWN LIMIT, inherited rather than introduced: it is spent
  at the FIRST door, so an unrelated watch step landing between the capture's 200
  and the delivery spends it and the cursor does not move. The delivery itself is
  no longer in doubt — the write nudges its own path (Config, above), which is
  what gave a TAGGED capture's landing anything to land on. Evidence:
  `TestServe` "the captured row is where point lands when it arrives" and "a
  tagged capture lands point on the blob when the watch delivers it". **test**
- **The reschedule chords survive the browser, and `C-c C-t` still does not.**
  `Ctrl+S` and `Ctrl+D` are page DEFAULT ACTIONS — save-page and bookmark — so
  `preventDefault` on the completing chord reaches them, exactly as it does for
  `C-x C-s`; `Ctrl+T`, `Ctrl+N` and `Ctrl+W` are handled in the browser process
  above the document and cannot be. So `C-c C-s` and `C-c C-d` are the org
  spellings that WORK, where `C-c C-t` needed a plain `t` beside it. What the
  page owes either way is the same and is what the suite pins: both halves of the
  chord claimed off the browser. **test** ("both reschedule chords are claimed,
  and name the keyword") / **none** (the browser's half, which no harness reaches)
- **The value palette is the shell's own, and the filter's is still the
  renderer's.** `C-c C-t` (and `t`) raises `#prompt` over `GET /keywords`'
  answer for the rows the command would run over, plus a `*empty*` entry. It is
  a second overlay rather than a reuse of
  `openFilter` because that one belongs to the filter and this page may not
  reach into its chrome — the same must-not-appear list that forbids `tv-veil`
  forbids driving it. What it offers is the RESOLUTION and never the state
  column's `values`: the two
  group meta-values are filter vocabulary, no file declares one, and offering a
  value the server will refuse is worse than not offering it. Nor is it
  `badges` any more — that is the union of every file loaded, a superset of what
  these rows may be set to and silent about where any of it came from; the
  badges are read for their HUES alone, by value, so a keyword the resolution
  names without one is drawn with none. Its keys sit in a
  SECOND document listener registered behind the dispatch, which is safe rather
  than lucky: while the palette is up `typing()` has already made every `table`
  row dead, so the only row that can have fired ahead of it is `ESC`, which is
  the one that should — `cancel` closes whichever overlay is up, prompt first.
  `unask` blurs as well as hides, since a focused field nobody can see would
  leave `typing()` true and swallow every key after it. Evidence: `TestServe`
  "Shell commands". **test**
- **The palette is which-key, and the palette IS the confirmation.** Every entry
  wears a letter and that letter commits on its own — one `/command`, the
  overlay dissolves, the pill and the log say what landed, exactly as the old
  `RET` did and through the same `takeChoice`. There is no second key and no
  confirm step: a reader who pressed `t` has just read the list saying `t` sets
  TODO, and the drift lock is what makes a mis-press cheap. `RET` in letter mode
  commits nothing and leaves the palette standing. `typing()` is what makes the
  letters exclusive — the palette turns it on with NO field focused, the way the
  property panel's nav does, so `n` moves no row and `d` lays down no archive
  flag while the palette is up. `/` falls back to the completing-read this used
  to be: the table FLATTENS to the one ordered list the letters were assigned
  over, the token column goes (no letter commits there, so drawing one would
  lie), the field appears, typing narrows, `C-n`/`C-p` and the arrows walk, `RET`
  commits. The fallback is entered and never left — `ESC` is the one door out of
  either mode. Evidence: `TestServe` "Shell which-key" and "Shell commands".
  **test**
- **`*empty*` answers to `DEL`, and spends no letter.** `DEL` already means
  take-it-off wherever this page binds one, and binding the meta to it hands the
  whole `a`–`z` pool back to the keywords — the entry a wide cycle used to lose
  is the letter `*empty*` was holding. It is the ONE entry that keeps a key
  token, since `DEL` names no position in a word to mark. In the fallback mode
  `DEL` is the field's own text editing and `*empty*` is reached by narrowing to
  it, like every other entry. A palette with no clear in it — the tag one —
  leaves the press to nobody: the map's own `DEL` is already dead under
  `typing()`. Evidence: `TestServe` "the meta entry clears the keyword rather
  than setting one", "DEL fires nothing in a palette that has no clear". **test**
- **The tags list is a MOUNT, and that is what made the letters go.** `:` — the
  agenda's own key for the same question over there — raises `#tags`, the page's
  FOURTH table-view mount (`#ttable`), and the first one that WROTE — the link
  popup grew an edit of its own later (#59). A tag over a
  set of rows is a RECORD: a name, a coverage over the set, a weight in the tree,
  and a reader deciding whether to drop one is READING those three. That is the
  link popup's case exactly ("two shapes for a choice"), so this list took the
  same shape and the which-key letters went with the list it left. What decides
  is the ENTRY rather than the count: a keyword is a single word committed from
  memory, and `t d e` for `TODO DONE DELEGATED` is muscle memory after the second
  use; a tag beside `2/3` and `40` is three facts a letter says nothing about.
  Columns are `Glance.Query.tagColumns`, declared server-side like the link
  popup's so the shape has one home: `title` — the tag, keyed the link popup's
  way because it is the readable NAME of the record, and because a column keyed
  `tag` would invite the renderer's multi-value sampling, which reads a cell as a
  whole `:a:b:` run — `on` (the coverage, `all` or `k/n`) and `rows` (`/tags`'
  store-wide count). A tag IS its row's id, since a tag appears once per popup
  and that is the whole of its identity: a flag, the cursor and a rename all name
  the same thing after any number of writes — which is also why the popup keeps
  no copy of the rows it is showing. `tagUnion()` answers every question a copy
  could (how many, whether one is still there, which the cursor is on), and a
  copy would be one more thing each of the three writes had to remember to
  refresh. Mounted once and kept, like the panel and the link popup. Evidence:
  `TestServe` "Shell tags", "Shell glue" ("the tags popup is a mutable mount with
  a rename overlay"). **test**
- **It is MUTABLE, and every gesture in it is one this page already spells.**
  `d`/`D`/`u` are dired's flag-then-confirm, and are literally the panel's now —
  one `flagKey` and a shape apiece; `+` is the value palette's completing field;
  `RET` is the panel's edit overlay, likewise literally — one `openEdit` and a
  shape apiece, with `cell` saying the box is one column wide. Nothing here is a
  new vocabulary, and since #64 nothing here is a second copy of one either. Mounted with
  `marks: false` — the set a tag command runs over is the TABLE's and was settled
  before this went up, so a second selection would be a second answer to a
  settled question — and `flags: true`, since the removal is the two-press
  gesture and the flag is its confirmation. The popup STAYS up under every write
  it carries: managing tags is several ops over one set where setting a state is
  one, and closing after each would make the second op a fresh press and a fresh
  resolution. Evidence: `TestServe` "the mount is mutable: flags on, marks off,
  no hints, no page". **test**
- **Raised LATE, on the answer.** `:` is no key inside the list it opens, so an
  empty mount put up on the press would buy nothing and cost a raising guard —
  which is the whole of why the state palette raises early and this does not. A
  set the store knows no row of is a REFUSAL rather than an empty popup, and a
  refused resolution raises nothing and writes one `cmd` error line. An untagged
  set does raise: it is honest rather than empty, and the foot names `+` as the
  way in. Evidence: `TestServe` "an untagged set opens on a popup that says so",
  "a refused resolution raises nothing and says so", "and a set the store knows
  no row of raises none either". **test**
- **`d`/`D` REMOVE, one command per flagged tag, over the rows CARRYING it.**
  The first `d` flags the tag at point, a second `d` on an already-flagged one IS
  `D` — the same handler, so it removes every flagged tag rather than the one
  under it — and `u` takes a flag off and walks on. A held key cannot flag and
  remove from one press (`e.repeat`), which is the confirmation the two-press
  shape exists to be. Several flags are SEVERAL commands, since a command names
  one tag; each is its own per-file batch of atomic writes, and each is aimed at
  the targets carrying THAT tag, so two flagged tags over one set can name two
  different row lists. The flags are SPENT before the first request goes out, the
  way the table's archive flags are: a repaint drops rows, and a set left
  standing would be removed again by the next press. Evidence: `TestServe` "a
  second d removes it from every row carrying it", "D over several flagged tags
  is one command each, over its own rows", "a held d flags once and never
  removes". **test**
- **`+` is the ADD, and it is the value palette's field with no letters behind
  it.** `askFrom` raises `#prompt` straight into typing mode over the ADDABLE
  vocabulary — `GET /tags`' `vocabulary`, the whole store's rather than the rows
  on screen, LESS every tag all the targets already carry, since adding one of
  those writes nothing and offering it is offering a no-op. A tag only SOME of
  them carry STAYS, wearing its `2/3`, because adding it LEVELS the set and does
  move rows; the set's partial tags lead and the rest of the tree follows. `RET`
  takes the highlighted entry or, where nothing matched, the line as typed
  (`freely`), so a tag the tree has never held is committable — a first use has
  to start somewhere, and the charset wall that refuses a name org could not read
  is the SERVER's. The write goes to the rows LACKING the tag, so the landed
  count is a count of rows that MOVED, and a tag every target already has costs a
  line in the pill and no round trip. One `ESC` closes the field and leaves the
  popup standing. Evidence: `TestServe` "+ raises the field over what can be
  added", "a tag some of the set carries is still addable, and says so", "RET
  there adds the tag to every row lacking it and stays open". **test**
- **`RET` is the RENAME, and it is the property panel's edit model over ONE
  cell.** `#tedit` is laid over the tag cell — the row's box read through the
  mount's published root (`tmount.el`), the cell's through
  `td:not(.tv-box)`, which is the class the renderer already stamps on the flag
  gutter rather than a column index this page would be counting. The other two
  columns are DERIVED and never open, exactly as the link popup's type cell does
  not. `RET` commits `rename-tag {from, to}` over the targets carrying `from`,
  `ESC` restores, and a name that folds to the one it opened on costs no request.
  The model is rewritten IN PLACE and deduplicated, mirroring the server's rule,
  so the union's first-seen order does not shuffle under the cursor. Evidence:
  `TestServe` "RET opens the tag at point over itself", "and RET again commits it
  as one rename-tag", "a rename to the same name writes nothing". **test**
- **THE LIST REFRESHES FROM THE ANSWER, never from a re-read.** `POST /command`
  does not write the store — the watch does, a debounce later — so asking
  `/tags` again after a commit would answer with what the files said BEFORE it.
  Every write folds its own per-id results into the per-row tag sets the popup is
  holding and repaints off those; a row the server refused keeps the tags it had,
  and a tag written for the first time joins the local vocabulary so `+` offers
  it before the watch has said anything. The `rows` count is STEPPED by what
  landed for the same reason — the number is the tree's and only the tree can be
  right about it, but a column standing still while the rows under it moved would
  read as a stale answer rather than as a different question — and the next
  resolution corrects it. The suite asserts the list is empty immediately after a
  removal while the fake store still says every row carries the tag. Evidence:
  `TestServe` "the list is what landed, and the store is not asked twice".
  **test**
- **The union is FIRST-SEEN, and now it is the CURSOR it keeps still.** The rows
  are the union over the target rows, in the order the ROWS introduce them and,
  within a row, the order its FILE spells them. Alphabetical would be no harder
  to compute and strictly worse: an insert in the middle moves the row out from
  under the cursor, where an append cannot. The rationale transfers unchanged
  from the letters it used to protect. Evidence: `TestServe` "Shell glue", "the
  tag union is first-seen, and the refresh is the answer". **test**
- **A tag is FOLDED at commit, because presence is.** `/tags` reports what
  `tagsOfCell` reads and `tagged` matches the same way, so a popup that wrote
  `Work` would go on showing `work` and offering to add it again.
- **Its keys are a private listener, and two guards keep it off the
  field raised OVER it.** The listener runs only while `momentary()` NAMES it,
  and `+` raising the palette takes that name away — the palette's `SURFACES`
  entry stands earlier in the list, so `momentary()` resolves the tie its way.
  Without it a reader narrowing the add field would be flagging tags
  underneath it — `typing()` has killed the map's rows and there is nothing else
  between the two surfaces. And a key the palette has already CLAIMED is declined
  too (`e.defaultPrevented`): the palette's listener runs AHEAD of this one and
  closes the overlay as it commits, so the very `RET` that added a tag would
  arrive here over a popup with no prompt on it and open the rename over the tag
  it had just written. `defaultPrevented` is the DOM's own word for "handled",
  which every listener on this page already says by calling `preventDefault`.
  Evidence: `TestServe` "and the popup's own keys are dead under its field", "the
  RET that adds does not open the rename behind it". **test**
- **The overlay goes up on the keydown; the resolution fills it.** `ask` raises
  `#prompt` EMPTY and synchronously, drawing a `resolving…` line, and the
  `/keywords` answer arrives afterwards through `setChoices`. Everything that
  hangs off the palette being up is therefore where it was — `prompting.raising`
  still declines the press that opened it, `typing()` still kills every `table`
  row, `ESC` still closes it — and none of that had to learn about a request.
  `ask` hands the prompt back, so a fill landing after the reader left finds
  another prompt or none and drops; a refusal closes the palette and writes one
  `cmd` error line, since a palette with nothing in it is no offer at all.
  Evidence: `TestServe` "the palette is up before the resolution is",
  "a refused resolution closes the palette and says so". **test**
- **An empty list is two things, and the palette says which.** Before the answer
  it is `resolving…`; after one it can be a set that honestly holds nothing — an
  untagged row — and the line then names `/` and `+` as the ways in.
  `prompting.empty` is written by the fill and by nothing else, so a palette
  waiting on a request cannot claim to have found nothing. A set the store knows
  no row of closes the palette instead, since there is nothing to tag. Evidence:
  `TestServe` "an untagged set opens on the line that says so", "and a set the
  store knows no row of closes it too". **test**
- **Two guards stand between `t` and a write it did not mean, and each has its
  own press.** `t` raises the palette AND is a letter inside it, and the
  palette's listener sits BEHIND the dispatch — so the very keydown that opened
  the overlay arrives in it next. This is what makes `ask` synchronous: raising
  the overlay off the `/keywords` answer instead would leave `raising` set with
  the opening press long gone, and it would decline the next real one.
  `prompting.raising`, set by `ask` and consumed
  by the first key the palette sees, declines exactly that event; without it one
  press would open and commit at once. The second guard is `e.repeat`, which
  keeps a HELD `t` from committing through what it just opened. The keymap's
  `ONCE` list cannot reach that: it governs dispatch rows, and the repeat arrives
  while every `table` row is already dead. Deleting either one is a live write
  from a key the reader never finished pressing. Evidence: `TestServe` "the press
  that raises the palette is not a key in it" and "a held t opens the palette and
  stops there". **test**
- **The letters are deterministic, and the rule is one pure function.**
  `whichKeys(labels)` walks the labels flattened in DRAW order — each source
  row's active cell, then its inactive one — and gives each the
  INDEX
  of the first letter of its OWN spelling, downcased, that no earlier entry
  claimed — one `a`–`z` namespace over the WHOLE table, `-1` for an entry with
  nothing left, so a letter is the reader's wherever in the table its keyword
  sits and the fallback narrows that same list. So
  `TODO` `DONE` `DELEGATED` is `t` `d` `e`, and a whole cycle
  `TODO NEXT STARTED WAITING DELEGATED CANCELLED DONE` is
  `t n s w d c o`. Order-only and side-effect-free, so one tree's cycle always
  yields the same letters and the muscle memory holds — and since the chain
  draws `default` first, `TODO` takes `t` and `DONE` takes `d` in EVERY tree,
  whatever a narrower scope declares. `DELEGATED` sitting in a tag's or a file's
  cycle cannot claim `d` ahead of it, which the reordered chain buys for free
  rather than by special-casing the pool. `*empty*` is OUT of the pool: it
  answers to `DEL`, which is no letter, so the namespace is spent on KEYWORDS
  alone and a cycle wide enough to run it dry keeps the letter the meta used to
  take — `CANCELLED` claims `c` outright where it once shared the pool with a
  word spelled `*empty*`. `offer` decides membership by the entry carrying a key
  of its OWN (`fixed`), never by its being the meta, so the rule reads as "an
  entry with a key does not need a letter" rather than as an exception. An
  unbound entry is drawn BARE — no slot, no dot — and is reachable through `/`
  alone; the reorder made one reachable on ~/sync, where a `book` row spends
  `t d e p s c r a` on `default` and `system` before `READ`, whose four letters
  are all gone by then. That is the price of a finite pool with the shared
  scopes drawn first, and `/` is what pays it. `setChoices` folds the letter into each entry once, so the drawing
  and the dispatch read ONE FIELD of one object and a letter drawn cannot drift
  from a letter honoured — a parallel array would have to stay indexed against
  `shown`, which narrows, rather than `choices`, which does not. Evidence:
  `TestServe` "Shell which-key", which drives `whichKeys` under the node harness
  as the pure function it is. **test**
- **The palette teaches why, and what it teaches is the resolution.** It is a
  TABLE — `Source | Active | Inactive`, one row per source in the precedence
  order `/keywords` sent, so the layer that answered for a keyword is the row it
  sits in and the classify chain is on screen rather than inferred. An entry is
  the keyword ALONE, in ITS OWN badge colour, with the claimed letter marked
  where it sits in the word — BOLD and UNDERLINED, the rule taking that same
  badge hue (written inline per entry, since only the entry knows it) at two
  pixels and offset clear of the descenders. `DELEGATED` marks its `E`, which is
  the whole of the explanation. There is no key-token column: an entry IS its
  keyword, and a boxed letter beside it said the same thing twice while pushing
  every word rightwards. ONE entry keeps a token, and it is the one whose key
  names no position in a word — `*empty*` answers to `DEL`. The old active-vs-done
  hairline is the two COLUMNS now, and the hairline between two source rows is
  the row's own top border — the table's border language, where a flat list
  needed a divider element of its own. `*empty*` spans a row at the foot in the
  muted italic every starred meta wears, since no scope declares taking a
  keyword off. The source cell is the muted small lowercase a tag wears
  everywhere else on this page, whether it holds a tag or one of the reserved
  labels (`default`, `system`, `file`), and every source is drawn under the NAME
  it arrived under — the page keeps no label table to hold in step. The foot
  names the keys the
  table cannot draw
  for itself — `a letter sets it · / to search · ESC leaves`, and the fallback's
  own line in its own mode. Evidence: `TestServe` "Shell which-key", and
  "the letter is marked in the word, and only *empty* wears a token", which
  reads the rule's colour back off the drawn element. **test**
- **`o` follows the row, and the ANSWER decides the gesture.** `o` (and `!`,
  org-glance's other spelling) fetches `GET /links?id=` for the row at point and
  then does one of three things: no links echoes
  `o → org-glance-overview:open (no links)` and stops, ONE opens with
  `window.open(target, "_blank", "noopener")`, and SEVERAL raise the POPUP. So
  a reader never confirms a choice there was only one of, and never guesses which
  of five references a key would take. Every open writes a `cmd` line naming the
  target, which is the only trace a followed link leaves on the page it was
  pressed from. `noopener` is not decoration: the opened page must not reach back
  into this one. The command is on `ONCE`, since a leaned-on `o` is a tab per
  repeat. Evidence: `TestServe` "Shell open". **test**
- **A tab can be pointed at `http`/`https` and NOTHING ELSE.** `followable` is
  that one test, and it reads the server's `type` — `l.type === "https" ||
  l.type === "http"` — rather than running a regex over the target a second
  time, so the badge a reader sees and the judgement a key makes are one answer.
  Everything else org writes — `mailto:`, `file:`, `id:`, org-glance's own
  protocols, the bare `[[Title]]` internal link — names something a tab is not,
  and `/links` reports them all, so the judgement is the COMMIT's. A
  non-followable target is one `cmd` WARN line —
  `link type not implemented: TARGET`, truncated at 80 characters (`shortly`) —
  plus the same words in the echo, and no tab opens. It lives in `openLink`
  rather than in a filter over the rows, which is what keeps the popup LISTING
  every link: what an entry holds is what teaches a reader what the entry is. So
  a lone `mailto:` warns without a popup, and a `mailto:` row beside an `http`
  one warns while its neighbour still opens. Evidence:
  `TestServe` "a single link that is not http(s) opens nothing and says so",
  "an o on a non-http row refuses the same way", "and an http row beside it
  still opens", "and only the two followable ones open a tab". **test**
- **TWO SHAPES FOR A CHOICE, and which one a list gets is decided by whether it
  has to be READ.** The page offers a set of options in one of two ways and
  there is no third.
  - A **which-key palette** (`#prompt`) is for a FIXED VOCABULARY a reader
    commits from memory, and the state palette's keywords are the whole of what
    is left in it. Every entry wears a letter, the letter commits on its own, and
    the palette IS the confirmation. It works because the entries are single
    words a reader already knows the shape of — `t d e` for `TODO DONE DELEGATED`
    is muscle memory after the second use — so nothing has to be read before a
    key is pressed. `/` is its completing-read fallback for a set too wide to
    have claimed a letter each, and the same field is what `+` raises over the
    tags popup (`askFrom`): one widget for typing, two doors into it.
  - A **table-view mount** is for a list of RECORDS that has to be read before it
    can be picked from: the link popup, where each entry has a kind, a name and a
    destination, and the tags popup, where each has a name, a coverage over the
    named rows and a weight in the tree. Letters are the wrong instrument there —
    they are noise laid over the columns that carry the answer, and a letter
    assigned to `First reference` teaches nothing about where it points. So the
    surface is move, look, act: `n`/`p` (and `j`/`k`, and the arrows), keys to
    commit, `ESC` to leave. The renderer draws it, because this page has ONE list
    widget and a table of records is what it is for — which is the same argument
    the property panel landed under (#50). Whether it WRITES is not part of the
    shape: both of these do, and the link popup did not when it landed.
  What decides is the ENTRY rather than the length: a two-entry table still wants
  columns and a forty-keyword cycle still wants letters. A list whose entries are
  single known words takes letters; a list whose entries are records takes a
  mount.

  **THE TAGS LIST MOVED ACROSS THIS LINE (#55), and it is the case that shows
  where the line is.** It was a which-key palette and it read as one — every tag
  under a letter, a letter toggling under a normalize-up rule, `3/5` in a muted
  aside. What it could never be was READ: the aside carried the only fact about
  the set, there was nowhere to put the tree's own count, and the letter had to
  mean add here and remove there depending on a coverage the reader had to work
  out from that aside. As a mount the three facts are three columns, the toggle
  splits into the two gestures the page already has (`+` adds, `d`/`D` remove),
  and `RET` is free to be the rename an in-place edit overlay was already the
  model for. The tell was the ASIDE: a palette entry that needs a note about
  itself is a record wearing a letter. Evidence: `TestServe` "Shell tags".

  Evidence: `TestServe` "Shell which-key", "Shell open" and "Shell tags".
  **test**
- **The link popup is a MOUNT, it browses, and it is raised LATE.** `o` on a
  row with several links raises `#links`, a sibling of `#app` sharing the two z
  levels with the sheets, the value palette and the tags popup, hosting the
  page's THIRD table-view mount (`#ltable`). Three columns, declared server-side in
  `Glance.Query.linkColumns` so the type vocabulary and its hues have one home
  next to the function deriving them: `type` as a badge, `title` as the
  description the entry itself wrote, `url` as the target. The `url` column is
  plain text — a muted aside is what the palette drew by hand and no column KIND
  offers one, so the target reads in the page's ordinary ink and the column it
  sits in is what tells it from the title; inventing a kind would be a renderer
  feature and styling one from the shell would be this page reaching into the
  table's cells. What it does NOT carry is STATED rather than inherited
  (`marks: false, flags: false, actionHints: false`, no `pageSize`): its one
  write is `RET`, over the link at point, so a gutter, a wash and a per-row hint
  would each be chrome about a gesture it does not have. `typing()` is what
  enforces that: the popup turns it on with no field focused at all — the way the
  property panel's nav does, and until `RET` opens the edit overlay, whose fields
  hold the focus themselves — so every `table` row is dead under it and `d`, `D`,
  `m`, `M`, `u` and `U` do nothing at all. Its keys are a THIRD document
  listener behind
  the dispatch, safe for the reason the other two are — the only row that can
  fire ahead of it is `ESC`, which is the one that should. Raised LATE, behind
  the fetch, because none and one are answered without a popup at all; by then
  the `o` that asked has been dispatched and gone, so nothing is travelling and
  no press is declined. Mounted once and kept, like the panel: a mount per press
  would leave a theme listener behind every time a reader followed a row. Row
  movement is `rowStep`/`stepIn`, shared with the property panel — both spellings
  and the arrows in one place, so the two modal surfaces cannot drift from each
  other or from the map's own `n`/`p`/`j`/`k` rows.
  Evidence: `TestServe` "Shell open". **test**
- **SINCE CLOSED — the surface list is ONE list now.** `SURFACES` holds
  `prompt`, `links`, `tags` and `sheet` in that order, each entry naming its
  `up`, its `off` and the open EDIT that is a rung under it, and FOUR readers
  take everything off it: `momentary()`, `typing()`, `sole()` and `cancel`'s ESC
  ladder. The property panel is NOT a member — it is the sheet's, reached
  through `sheetOpen`. There is no `covered()` function and there never was one
  in shipped code; a listener that must decline for a surface above it asks
  `momentary() !== NAME`. What is still hand-maintained is the ORDER, and it is
  load-bearing for exactly one pair (`+` over the tags popup leaves both
  `prompt` and `tags` up). The entry below is the shape that argument was made
  against, kept for its reasoning.
- **KNOWN SHAPE, and the fourth surface has arrived: four private listeners,
  THREE hand-maintained copies of the surface list — two of them ORDERED — and
  now a STACK between two of them.** The value palette, the property panel, the link popup and the tags
  popup each carry a document keydown listener of their own, and each had to add
  itself to `typing()` (`pnav() || !!prompting || linking() || managing()`).
  Forgetting is silent and destructive — the table's `d` and `D` stay live
  underneath it. #55 added the second half of the cost: two of these surfaces now
  STACK (`+` raises the palette over the popup), so the lower one owes two guards
  of its own — decline while `prompting` is set, and decline a key the palette
  already claimed (`e.defaultPrevented`). Neither is expressible in the
  predicate.
  THE SURFACE LIST IS WRITTEN DOWN THREE TIMES, in three unrelated notations,
  and nothing checks them against each other; two of the three are also ORDERED,
  and a wrong order there is a behaviour change rather than a missing surface.
  (1) `typing()`'s OR chain, which says WHICH surfaces exist and nothing more —
  the operands are booleans under `||`, so their order is inert. (2) The
  REGISTRATION order, which is source order and
  decides who sees a key first — and it does not match the comments: three of the
  four listeners are written after the dispatch and run behind it, but the
  property panel's sits with the sheet near the top of the glue and therefore
  runs AHEAD of the dispatch. It is harmless today only because its own
  `if (!editing) return` and `typing()` agree about when it is up, and because it
  falls through on every key it does not claim, `ESC` included. (3) `cancel`'s
  ESC ladder, an `else if` chain that spells the surfaces in precedence order —
  prompt, open link edit, link popup, open rename, tags popup, open panel row,
  sheet, focus — and
  is the copy a new surface is likeliest to be left out of, since leaving it out
  costs nothing until a reader presses `ESC` over it.
  A SECOND STACKING PAIR IS ALREADY REACHABLE, and it is `typing()`'s hole
  rather than a design: with the materialize sheet open, clicking any
  non-focusable sheet chrome (`#mhead`, `#mfile`, `#mnote`, `#mlog`, the sheet's
  padding) blurs `#mtext` without closing the sheet, and `typing()` — which is
  `pnav() || !!prompting || linking() || managing()` plus a focused
  field — goes FALSE. Every `table` row is live again under an open sheet, so
  `:` raises the tags popup over it. This is the same hole `openSettings`
  refuses by hand (`if (activeSheet()) return`), and the tags popup does not.
  Nothing is corrupted today — the popup is `position:fixed;inset:0` so the
  sheet cannot be reached under it, and `shutEdit` is scoped per surface so the
  sheet's own shutters cannot cancel an open tag rename — but the guard that
  makes it safe is spelled in three places instead of being a property of the
  map. Either the tags raise takes `openSettings`' refusal, or `typing()` takes
  `|| editing !== null`; neither is landed. **none**

  The keymap holds the machinery all of this wants: `keyBindings` rows carry a
  `scope` and `live` routes `any`/`modal`/`table` off page state, so the deeper
  answer is a scope per surface plus one "which list holds the keys"
  indirection, which would also put the popups' own keys in the blob where the
  key line and the echo can see them, and would leave ONE ordered list where
  there are three. It is still not taken, because it is a redesign of THREE
  landed listeners rather than an addition and because the palette's letters are
  dynamic (`whichKeys`). The count is four, they stack, and the second stacking
  pair above arrived without anyone choosing it; a fifth surface should force the
  question. **none**
- **`o` is the key inside the popup too, and `RET` is reserved for the edit.**
  The key that raised the list is the key that commits from it, over the link the
  cursor is on rather than the row's first — one gesture with one name. It opens
  and CLOSES, both outcomes alike, the tab and the type refusal: picking one link
  is what the popup was raised to do, and staying up on the refusal would be a
  second rule for the same key. `RET` EDITS the link at point IN PLACE — the
  row's own title and url cells becoming fields over themselves (`LROW`, the
  shared overlay's third shape), `TAB` between them, `RET` committing and `ESC`
  restoring, which is the property panel's edit model exactly, so a panel row, a
  tag and a link are edited alike and the derived type cell never opens. The
  commit is `edit-link` over the SPAN `/links` handed out, pinned to the digest
  that same answer carried: this page holds no bracket grammar and no offsets of
  its own, so it sends back the range it was given plus the two strings a reader
  typed. ABSENT IS NOT NULL here too, and the untouched FIELD is what says so:
  the description field opens on what the link SHOWS, which for a link carrying
  none of its own is its target, so a field left alone sends no `desc` at all and
  an emptied one sends the null. Both fields are TRIMMED on the way out, which is
  the page's rule and worth stating beside the server's opposite one:
  `[[T][ D ]]` sends `D`, the padding being the field's, where the server writes
  a description verbatim and refuses a padded target outright. THE POPUP CLOSES
  ON THE PRESS, both outcomes
  alike, which is `o`'s own rule — and it has to: the spans it is holding
  describe the file as it was and the write has just moved it, while the store
  does not know yet (`/command` never writes it, the watch does, a debounce
  later), so a popup left standing would be offering ranges into a text that is
  gone and a re-read HERE would answer with what the file said BEFORE the write —
  the tags popup's own documented reason for never re-reading. `o` again is one
  keystroke and comes back with fresh spans, fresh descriptions and a fresh type.
  KNOWN CONSEQUENCE: the popup is also the only editor, so a row holding exactly
  ONE link is followed and never listed, and that link has no editor; a key that
  LISTS whatever the count is would settle it. Evidence: `TestServe` "o opens the
  link at point and closes the popup", "RET opens the link at point over its own
  two cells", "TAB hops the two fields", "RET commits the span the server gave,
  under the digest it came with", "a description nobody moved is not sent at
  all", "and one the reader emptied is the null that takes it off", "the commit
  closes the popup, and the log names both ends", "a link nobody changed costs no
  write", "a click under an open link cannot redirect the write". **test**
- **`a` is a canned VIEW, not a mode — and the whole view is one string.**
  `org-glance-agenda` applies `state:*active* -planned:*empty* sort:scheduled`
  through the door `g` uses — `applyView` writes it into the URL, drops the
  socket, and remounts, so the query is the renderer's chips and a reader can
  read it, edit it with `DEL`, or link it. There is no agenda state anywhere: no
  flag saying it is on, no key that leaves it, and every other key means while it
  is applied exactly what it always meant. `g` is the way home. The ORDER the
  default view does not want rides in that same string as a `sort:` token, so the
  server answers page one in it and the renderer reads the chain off the query it
  mounted under — where a call behind the answer could state an order the applied
  query did not, and an asset without the call got no order at all. What still
  arrives through `landed` — a one-shot thunk `start` TAKES before it fetches, so
  a boot that never lands cannot leave it armed for the next one — is the ECHO,
  called with the server's own match count, which is the one number the first
  page cannot give. Evidence: `TestServe` "Shell agenda". **test**
- **Every column of the view opts into sorting.** `sortable` is SCHEMA.md's
  opt-in — absent is `false`, both renderers read it that way — and
  `Glance.Query.column` declares it on all six, so it sits on the column helper
  rather than in a per-kind list that would name every column anyway. It gates
  what a READER's GESTURE may reach: `^` and a header click consult it, where a
  declared chain and a query's `sort:` token open as written whatever it says.
  Evidence: `TestQuery` "every column opts into sorting", and the golden
  `sample-view.json`. **test**
- **`^` promotes the column at point, and the renderer decides all three of its
  rules.** WHICH column is the cell selection's (`getSelection().col`), so a
  whole-row selection is refused — `^ → toggle-sort (no column selected — f/l to
  pick one)` — rather than guessed at, the renderer's own `^` having point to
  read where this has none. WHETHER it sorts is that column's `sortable`, and
  `sortPromote` is where that is enforced: the refusal is READ OFF the call
  (`false` means the chain did not move) rather than derived a second time here,
  and the key still speaks it. And what the press DOES is composition: the column joins the head of the
  chain ascending, or flips where it already leads — pressing over columns in
  reverse priority order builds a chain, which is the web's spelling of
  `table-view.el`'s `C-u ^`. The command is in `ONCE`. Evidence: `TestServe`
  "Shell sort". **test**
- **The press is a QUERY EDIT, and no order is remembered anywhere on the page.**
  `sortPromote` writes the new chain into the applied query as `sort:` tokens and
  delivers it, so the press arrives at `onFilter` as an ordinary commit: the URL
  is rewritten, the server is asked for the order it was just told about — which
  is what makes page one of a limited answer the right hundred rows — and the
  rows in hand re-order before the answer lands. `DEL` takes the sort chip
  WHOLE, because a chip is a chip — the per-key peel (one tie-breaker per
  press) erased by a different rule than every neighbour and was retired
  2026-08-05 at the user's call; with the chip gone, the view's declared
  chain stands again. What the first press composes onto is
  the chain IN FORCE, declared keys and all, so only the promoted key moves and
  the reader loses no tie-breaker they were reading by. This page holds no
  `sortAt`, calls no `sortBy` and asks the handle for nothing but the chain it
  just wrote (`getSort`, for the echo). A REMOUNT re-seeds off the query it
  mounts under, so the order survives one. Evidence: `TestServe` "the press
  writes the order into the query and asks for it", "DEL takes the order back
  off", "a remount re-seeds the chain off the query it mounts under". **test**
- **The ORDER is a token of the query grammar, and refusing one is the
  producer's alone.** `sort:COL` / `sort:COL:desc` name one column in one
  direction; written order is precedence, repeats compose the chain, and the
  token NARROWS NOTHING — `Glance.Web.Filter` knows the key so it is never read
  as free text and `compile` drops the term ABOVE the negation inverter, so it
  narrows nothing in either polarity where a match-all under that inverter would
  make `-sort:x` empty the table. `Glance.Web.Sort.sortChainIn` reads the same
  module's `parseFilter` output for what those tokens say about the order, so
  one parse serves both questions. A query naming any sort key replaces the
  chain, and one naming none leaves the view's declared chain standing — which is
  what keeps the default chain invisible until a reader diverges from it.
  `sort:*none*` is the EMPTY CHAIN — document order, and no `sort` field on the
  wire — and it is a starred meta rather than a parameter: it travels in `q`, in
  the URL and back out of the renderer's chips like everything else a reader
  states about the answer. It ADMITS NO COMPANIONS: another sort key beside it,
  or a direction on it, is a 400 naming the meta, since two orders in one query
  is a reader meaning one of them; the half-typed `sort:` is no companion. The
  older `?order=document`/`?order=scheduled` parameter is GONE and is refused
  rather than ignored, which is the reason it was ever spelled out — a parameter
  silently dropped would serve the default order and read as a working request.
  A negation, an alternation, a
  column no view carries, a direction that is neither `asc` nor `desc` and a
  column named twice are each this request's 400 naming the token; the renderer
  has nobody to refuse to and drops the key instead, so the producer is STRICTER
  there — the one divergence in that direction, and deliberate, an order nobody
  can give being worth saying. Evidence: `TestFilter` "Sort tokens", `TestServe`
  "GET /headlines?q=sort:", and `table-view`'s
  `fixtures/parity/sort-tokens.json`. **test**
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
  panel said. `d`/`D` delete one, and a row whose key is emptied is a property
  deleted the other way.
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
- **The property panel IS a table-view mount, and that is what makes the
  renderer this page's one list widget.** A second `TableView.mount` into
  `#mptable` inside `#mprops`, over two columns — `key` and `value` — and the
  drawer is the rows: the three planning entries first, in org's order, then the
  properties in file order. What that buys is one implementation of everything a
  list does. The row element, the stripe, the cursor and its class, the movement
  that repainted them and the wash a flagged row wears were this page's own
  before and are the renderer's now; `.prow`, `pcur`, `drawRow` and `addRow` are
  gone, and the suite forbids each name coming back.

  TWO OF THE MOUNT OPTIONS CARRY A RULE; the rest are configuration and are
  spelled at the call. No `pageSize`, so the whole drawer is one page and no
  cursor can walk off the end of one — the panel binds no page keys, so a
  paged panel would have rows a reader could not reach. And `marks: true`,
  which is a PRICE rather than a choice: `isFlagged` is `marks &&
  flagged.has(id)` in the renderer as landed, and `hintHTML` returns before its
  flag segment without it, so the wash, the count and `flagHelp` are all gated
  on the mark column. Nothing in the panel reads a mark.

  THE PRICE IS LIVE, and this page pays it in CSS. The mark cell is clickable
  in the renderer (`onBox`), so left alone the panel would carry an affordance
  that toggles a set nothing reads and nothing clears. `#mptable .tv-table
  td.tv-box` therefore loses its `[ ]` glyph, its pointer and its hit-testing,
  and the click falls through to the row. The gutter itself STAYS: the flag's
  second channel is an inset edge the renderer draws on that very cell, which
  is what keeps a flagged row readable under the cursor, so hiding the column
  would blank the flag at the moment it is laid down. The option that retires
  all of this is a `flags:` gate of its own in table-view, which is a sibling-repo
  change plus a `make sync-renderer`. **test** (the options) / **none** (the
  three declarations, which no test renders)

  MODEL AND VIEW, and the split is the thing to keep. `prows` is the model — a
  key, a value and whether org owns the key — and the mount is a VIEW of it:
  `repaint()` is the one door, `props()` and `planning()` read the model, and a
  flush sends what the model holds. The cursor, the flags and the scrolling are
  the renderer's and this page keeps no copy of any of them: `patAt()` asks
  `getSelection()`, movement is `selectStep(±1)`, `flagKey` asks `getFlagged()`.
  A row's id is stable for the life of the sheet — `PLN:<KEYWORD>` for the three
  fixed rows, `P<n>` handed out once per property — so a flag and a selection
  both survive any number of edits above them, which an index would not.

  Mounted ONCE and re-set per sheet. A mount per sheet would leave a theme
  listener and a mutation observer behind each time a reader opened one, so
  `mounted()` is memoized and a new drawer is one `setRows` — which also keeps
  the panel standing across a remount that rebuilds the table. Evidence:
  `TestServe` "the panel is a table-view mount of its own", "the panel is
  mounted once and re-set per sheet". **test**
- **The property panel is modal: nav moves over the mount's rows, `RET` opens
  one in the edit overlay, and `TAB` crosses the panes.** The keys are a private
  document listener, and it is the one of the four that registers AHEAD of the
  dispatch — it is written with the sheet near the top of the glue, and
  registration order is source order. Safe for the reason the three behind the
  dispatch are, reached from the other side: while the panel holds the keys
  `typing()` is true, so every `table` row is dead anyway and nothing here takes
  a key the map wanted — which is what lets `d` flag a property rather than an
  org row — and every key it does not claim falls through untouched, `ESC`
  included, which is what leaves the ladder the map's. In NAV nothing
  is focusable, and that is what pays for the movement being plain letters:
  `n`/`p` and `j`/`k` are both bound, unconditionally and under either profile,
  because a row with no field in it leaves every printable key free and
  satisfying both editors at once costs nothing; the arrows need no profile at
  all. Entering the panel BLURS the textarea and raises `pnav`, and `typing()`
  counts `pnav` as a focus of its own — without that, nothing is focused and the
  table's own letters move rows under the open sheet.

  `RET` opens the row at point into the EDIT OVERLAY: `#pedit`, ONE pair of
  fields laid over the selected row. It has to be an overlay rather than fields
  inside the row, because the mount owns its rows and rewrites them as it
  scrolls — an edit living in a `td` would be thrown away by the next frame. The
  value takes the focus, since editing a property that is there is almost always
  editing its value, and the key takes it where there is none yet; a planning
  row's key field is `readonly`, org owning that half of it. Inside an open row
  `TAB` is the hop between the two fields and the pane crossing is suspended,
  since one row and two fields leave it nothing else to mean. `RET` commits —
  the MODEL takes the text the fields hold and the mount is re-set — and `ESC`
  cancels, leaving the row the text it was opened on. A row HOLDS its committed
  text and `props()` reads that rather than the fields, so an edit nobody
  committed is not dirty and cannot be written; the commit is the thing that
  means yes. `ESC` runs through the keymap's `cancel`, which tries the open row
  before the sheet, so the sheet's own ladder only ever sees the key from nav.
  `TAB`/`S-TAB` is one toggle rather than a direction each — there are two
  stops, so a direction says nothing — between the body and the panel's cursor,
  which is where it was left; `shut` clears `pnav` and closes the overlay, so
  the next sheet opens read-only at its top. `preventDefault` fires exactly
  where one of those bindings does, and only over an open subtree sheet — raw
  mode has one pane and nothing to cross to, so `TAB` is the browser's there,
  and the settings sheet keeps native tabbing. The planning rows are the same
  two modes over the same kind of row and belong in this list rather than a
  second one.

  ONE MECHANISM, THREE SURFACES, AND A SNAPSHOT. The overlay is `openEdit`/
  `shutEdit`/`placeEdit` and a SHAPE per surface (`PROW` here, `TROW` for the
  tags popup's rename, `LROW` for the link popup's two fields): the class that
  shows the box, the `tv-sel` anchor read
  through the mount's published root, the blur on the way out and the window
  resize are one implementation, and what a shape declares is its box, its pane,
  its fields, its mount, how to fill them and where the focus lands — plus
  `cells`, the inclusive RANGE of non-gutter `td`s the box covers (`[0, 0]` for a
  rename, `[1, 2]` for a link's title and url, absent for the whole row). KNOWN
  LOCKSTEP, in the class the parity discipline enumerates: that range is a
  POSITIONAL INDEX into a column list declared in Haskell
  (`Glance.Query.tagColumns`, `linkColumns`) with nothing tying the two together
  — reorder those columns and the box covers the wrong cells, greenly. The flag
  it replaced (`cell: true` = the first non-gutter `td`) needed no index and
  could not cover two. The
  property this shape exists to have is the SNAPSHOT: `edit` keeps the row the
  overlay OPENED over and a commit is handed it, never the cursor. No key can
  move the cursor while a row is open, but a MOUSE CLICK can, and the panel's
  `commitRow` used to re-read `prows[patAt()]` — so a click under an open row
  wrote the typed text into whichever row the reader landed on, silently, and
  onto a planning row it wrote a bare value where a timestamp goes. The tags
  rename guarded this from the start with a snapshot of its own; sharing the
  mechanism is what gave the panel the same guarantee rather than a second copy
  of the guard. Evidence: `TestServe` "Shell sheet" (the crossing and its
  remembered cursor, `S-TAB` parity, nav movement on all three pairs with the
  table's own row staying put under it, `RET` opening value-first, `+` adding
  and opening a row, `TAB` hopping the open row, the commit, `ESC` restoring the
  row and the next one closing the sheet, an open row not counting as an edit,
  an emptied planning row taking its entry off, the overlay following the
  cursor, raw mode leaving `TAB` alone, and the reset on close), "a click under
  an open row commits the row that was opened", "and a click cannot redirect the
  key an add-row is writing", "a click under an open rename still renames the
  tag it opened on", "a click under an open link cannot redirect the write",
  "Shell glue" ("the edit overlay is one mechanism the three
  surfaces declare a shape for"). **test**
- **Deleting from the panel is the TABLE's gesture, over the same renderer
  flags — one gesture, deliberately spelled twice.** `d` flags the row
  at point — the mount's `flagRow`, so the row wears `tv-flagged` and the count
  rides in the mount's own hint line — echoing `d → delete-flag (d again
  deletes)`. A second `d` on an already-flagged row IS `D`: it calls the same
  handler, so it deletes EVERY flagged row rather than the one under it, which
  is what makes the flag the confirmation and is why there is no prompt. `D`
  without a flag takes the row at point. `u` takes a flag off and steps on, the
  way it does over the table. `e.repeat` is guarded HERE rather than by the
  dispatch's `ONCE` list, which governs dispatch rows and cannot reach a key
  this listener owns — a held `d` that got through would flag a row and delete
  it from one press, which is exactly the confirmation the two-press shape
  exists to be.

  WHAT "TAKEN" MEANS IS THE ROW'S, and the split is the one place this differs
  from the table. A property is DROPPED from the model, which is the emptied key
  spelled as a key press. A planning entry is CLEARED and its row stands: the
  three are org's keys rather than the author's, an empty value is already how
  an entry is absent, and clearing all three is still how the whole line comes
  off. A deletion moves the model, so it is dirty like any commit and the way
  out of the sheet is a write.

  THREE SURFACES, TWO SPELLINGS, and the second one is now shared. The panel and
  the tags popup run ONE `flagKey`: the feature detection, the two-press rule,
  the set-or-row choice, the walk after `u` and the echo shapes are written once,
  and each surface declares a mount, where its cursor is, what "take these" means
  and FOUR words — its line for an empty cursor, the two command names its echo
  spells, and the verb the second press earns. A third modal surface joins by
  naming those four.

  The TABLE's `flagging`/`archiveFlag`/`archive` stays a spelling of its own, and
  that is the price of the modal surfaces' keys living outside `keyBindings`: the
  same five-step machine, but its ACT is a `POST /command` where theirs move a
  model in hand. Three consequences are live and none is caught by a test:
  `ONCE` guards the table's repeat and a hand-written `e.repeat` guards the modal
  ones, which is one correctness rule with two homes; `said(b, …)` cannot be
  reached without a binding row, so those surfaces hand-spell the `SEQ → command`
  shape the rebinding config will address; and they write no `noted()` line to
  the event strip where the table writes one per row. The fix that retires all
  three is routing the modal keys through the blob's existing `modal` scope,
  which the value palette's second listener would want too. Evidence:
  `TestServe` "Shell glue" ("the flag gesture is one implementation over two
  surfaces").

  Nothing hidden is rowed, so nothing hidden is flaggable — `hiddenProperties`
  and the logbook never reach `prows`, and the identity property is the case
  that matters, since a gesture that deleted it would break the row id every
  update is keyed off. Evidence: `TestServe` "d flags the row at point rather
  than deleting it", "d again deletes the flagged property, and D is that press
  alone", "deleting a planning row clears the entry and keeps the row", "u takes
  a flag off and steps on", "a held d flags once and never deletes what it
  flagged", "a deletion is an edit, and a cancelled one is not", "nothing hidden
  is rowed, so nothing hidden is flaggable". **test**
- **One geometry read, through the handle's published root, and it is the whole
  of what this page takes out of the mount's DOM.** `place()` asks `pmount.el`
  — a documented `Handle` field — for `tbody tr.tv-sel`, reads its box and puts
  the overlay at that offset with that height. Nothing about the row's CONTENT
  is read; the model already holds it. It re-runs on everything that can move
  that box: the mount's own scroll, caught in the CAPTURE phase so the scroller
  element is never named, and the window resizing, since the panes wrap rather
  than querying a width. It runs one frame after `openRow` (`soon`), because the
  renderer stamps `tv-sel` on a frame of its own — a synchronous read after `+`
  would measure the row the cursor was on before the add.

  WHAT IT CANNOT DO, stated because the alternative is believing it does.
  `Handle` publishes no COLUMN geometry, so the overlay's `40%`/`50%` split is a
  guess over columns the renderer measures from content: the fields sit on the
  right ROW and only approximately over the right cells. A selected row outside
  the rendered window has no element, and `place()` then leaves the overlay
  where it was rather than hiding it — reachable only in a drawer long enough to
  scroll, which is why it is a known limit rather than a bug. And the mount is
  built and re-set inside a `display:none` sheet (`fill` runs before `#modal.on`),
  which works because the renderer falls back to a screenful when
  `scroll.clientHeight` is 0. The fixed alternative — an edit strip at the foot
  of the panel, or reusing `askText` — needs no geometry at all and was declined
  for the in-place feel; a column-geometry accessor upstream would retire the
  guess. This is the one strand of the DOM-walking path that came back, and it
  came back for a position rather than for a row. **test** (the behaviour,
  without geometry: the suite's node harness answers the selector with nothing,
  so every panel case passes with the overlay unplaced) / **none** (the geometry
  itself, which no test measures)
- **The whole page wears danneskjold, through one `--g-*` palette.** Surface,
  text, muted text, border, selection, warn and bad are declared once and
  re-declared per theme, and every `var()` on the page reads one of them, the
  monospace stack, or the sheet's own. The sheet keeps exactly ONE variable of
  its own, `--dk-mono` (Hack first); it stopped carrying a private `--dk-*`
  palette when the page grew one. So "the sheet alone wears the author's theme"
  is the old arrangement, and a change to the page's colours is one block rather
  than two. **test** (`TestServe` pins the declarations)
- **The theme is a `data-theme` handshake with a pre-paint boot.** `themesel`,
  the settings sheet's theme panel, offers `auto`, `light` and `dark`. `auto`
  follows `prefers-color-scheme` and is the default, and choosing it REMOVES the
  attribute rather than writing a value; the other two stamp `data-theme` on the
  document element, and the attribute rules are written so they beat the media
  query in both directions.
  The choice lives in `localStorage` under `glance-theme` — distinct from the
  keymap's `glance-keys` — and `themeBoot` reads it and stamps the attribute in
  `<head>`, before the first paint, because a dark page that resolves its theme
  after paint flashes light. `themeBoot` is emitted on one unindented line so
  the suite's glue extractor, which finds the shell's inline block by a
  newline-plus-indent delimiter, cannot mistake it for that block. **test**
- **The settings sheet is PANELED, and one list is the panels.**
  `,` (`customize`) raises the page's one place for a preference, in three
  sections: **general** — the default view, the capture target and the log's
  height, the first two being `system.org`'s tree-wide lines and the third this
  page's own `localStorage` preference; **theme** — the `auto`/`light`/`dark`
  selector; **keywords** — a select over the config layers and ONE box holding
  the selected layer's `#+TODO:` lines verbatim, the union they come to, and the
  note saying what that union is. `SECTIONS` in the glue names the header and the parts of each,
  and the loop over it is the only thing that draws a frame, so a fourth panel
  is an entry there plus the markup it names — nothing else, because the panel
  bodies are laid out by CLASS (`.csec,.cpart`) rather than by a roll of ids.
  The bodies are declared in the markup and wrapped at boot rather than built
  from the list, because they are heterogeneous — three labelled inputs, two
  selects, a box the server fills — and a builder for that shape would be a
  template language this page has no use for. The join is by id, and a `parts` id the
  markup does not carry throws at boot and takes the whole inline script with
  it; the harness cannot see that (its stub answers every id), so the suite
  reads the ids back out of the shipped list and checks them against the page.
  The list order is also the TAB order — the sheet keeps native tabbing, so the
  DOM says what Tab reaches next — and the sheet opens focused on the general
  panel's first field. Where a field is DRAWN is a matter of reading and changes no
  write: the two general fields stay bound to the system layer's row and go out
  in its own `POST /config`, one file, one digest, one splice. The sync
  semantics are unmoved — buttonless, ESC or the backdrop syncs the layers that
  moved and closes, a pristine sheet costs no request, `C-x C-s` syncs mid-edit,
  `conflict` and `error` wait for a keystroke. Two fields ask nobody, and the
  panel they sit in says where a preference is READ rather than what writes it:
  the theme and the log's height are `localStorage`, apply as they are picked or
  typed, and close nothing — `cmoved` never sees `#clog`, so it costs no request
  and cannot make a pristine sheet dirty.
  **test** (the three headers in order, every `parts` id present in the markup,
  the theme applying and persisting with the sheet still up, both general fields
  riding the system write, and every sync flow re-run over the new layout)
- **The keywords panel is ONE select over ONE box.** A tree has as many config
  files as it has tags, and a stack of boxes was as tall as that number — the
  reader scrolled past every layer they were not editing to reach the one they
  were. So `#clayer` is a native `select` over the layers and `#ctext` is the
  one box, holding the SELECTED layer's `#+TODO:` lines verbatim, under `#clab`
  naming that layer (`system · PATH`, `tag · book · PATH`, plus
  ` · not created yet` where the digest is empty) and over `#clerr`, which
  carries whatever the server last said about a write to it. The ORDER is system
  first, then the tag layers by `localeCompare` (`byLayer`); `sort` is stable,
  so two system layers keep the order the server served them in, and the
  server's own order is the walk's — where the directories turned up. The select
  is the sheet's own chrome and takes the sheet's focus rules: native tabbing
  walks it in DOM order and it keeps the focus it is given. `SECTIONS`'s
  keywords entry is unchanged (`["clayers", "ceff", "cfoot"]`), the body is
  still declared in the markup and wrapped at boot, `.ctext` grew to `7em` since
  one box takes the room the stack shared, and `#clayer` shares `#themesel`'s
  select rule.
  **The text lives on the LAYER, and the box is a view of it.** `crows[i].text`
  is where a layer's lines are; the box shows `crows[cat]`; `takeLayer()` copies
  the on-screen box back into its layer, and every door calls it FIRST — the
  select's `change` handler, `cdirty`, `flushConfig`. That is the whole of what
  makes a switch free: an edit outlives every switch, and switching asks the
  server nothing. `cmoved(r)` is `r.text !== r.base`, plus the two general
  fields, which stay bound to the system layer. The sync semantics are unmoved —
  buttonless, ESC or the backdrop syncs the layers that moved and closes, a
  pristine sheet costs no request, `C-x C-s` syncs mid-edit, `conflict` and
  `error` wait for a keystroke — and it is still ONE drift-locked
  `POST /config` per FILE that moved, each awaited, each under its own digest,
  with no batch to roll back.
  **A refusal brings its layer with it.** With one box on screen, a message
  under it would otherwise describe a file the reader cannot see, so
  `flushConfig` remembers the FIRST refused layer's index and `showLayer`s it,
  and the box then shows the file the message describes. Every refusal is also
  an `append("config", "error", …)` line naming `SOURCE · PATH: message`, since
  only one can be shown. **test** (`TestServe` "Shell settings" — the order, the
  swap, an edit surviving a switch away and back, walking every layer writing
  nothing, the box holding a layer's lines byte for byte, every layer edited
  written one call each, and a 409 selecting the layer it refused and naming it
  — plus every existing sync flow re-run over the new layout; and `Shell glue`
  "the keyword layers are a select over one box". The harness's `/config`
  fixture serves THREE layers with `film` ahead of `book`, so the sheet's order
  and the server's differ and the sort is observable)
- **There is NO status corner, and the absence is what is asserted.** `#corner`
  held the connection dot (`live` / `wait` / `down`) and, under a coarse
  pointer, the gear that opened the settings; both are gone whole, with the
  dot's four call sites, the gear's click handler and every rule that drew
  either. The socket's state was already said twice over — the STALE WASH, the
  whole page fading back once a socket is gone, armed at 400 ms, and the strip's
  own `ws` lines (`disconnected · retrying in Ns`, `reconnected`, `reconnected ·
  rows refreshed`) — so a dot was a third spelling of one fact, and it cost a
  fixed box, a z-level and a top padding to keep clear of. The indexing state is
  now the strip's `boot info` line alone. `themesel` used to sit beside the dot
  and moved into the settings sheet with every other preference. What is checked
  is the ABSENCE of `id="corner"`, `#corner`, `id="dot"`, `#dot`, `dot("live")`,
  `dot("down")`, `dot("wait")`, `id="gear"` and `#gear`, so the box cannot come
  back under another name and bring its rule with it. **test** (`TestServe` "the
  page has no status corner, and nothing focusable outside a popup")
- **With no popup open, the TABLE holds the keys — and every control this page
  carries is inside a popup.** The legitimate focus holders are the popups —
  the materialize sheet, the settings sheet, the filter palette and the value
  palette — and the controls inside them. The page's own column, table, log and
  key line, holds no `select`, `input`, `textarea`, `button` or `a` at all, and
  that is the whole of the rule. It is also what a control outside a popup cost:
  a focused `SELECT` counts as typing, so one that kept the focus after its
  change had committed went on eating `n` and `p` as its own type-ahead, the
  reader had to click the table back before movement worked, and the answer was
  a hand-written `blur()` every such control owed. Inside a popup the focus is
  the popup's: `typing()` is true while a control of it holds the focus, the
  table's keys are dead under the sheet either way, and `ESC` (`any`) and
  `C-x C-s` (`modal`) reach the sheet regardless — which is why the settings
  sheet's theme select and its layer select each keep the focus they are given
  and owe no line. **The popup hands the keys back once,
  when it closes** — `shutSettings` blurs whatever it held, which a browser
  does anyway at `display:none` and which is stated so it is the sheet's rule
  and covers every control the sheet will ever grow. So no control on this page
  blurs on its own change, and the suite forbids the line that would. **test**
  (the theme picked from the sheet, then a movement key that must move nothing;
  then `ESC` and the same key, which must move; the page's column holding
  nothing focusable; and the absence of the per-control blur line)
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
- **Drill-down is ONE semantic at TWO GRAINS, and `DEL` is the single undo for
  both.** A JUMP — `@` — pushes a crumb and applies a whole new query; a
  REFINEMENT edits the query in place and pushes nothing. `DEL` undoes whichever
  is nearest, as a ladder: `stripLastToken` while the query still has tokens in
  it, and when the strip leaves it EMPTY and a trail stands, `popCrumb` plus the
  popped query INSTEAD of the empty one. So `@` then `DEL` is one step out and
  one step back rather than a step and a half, and a drill the reader went on to
  refine is walked back token by token before it pops. With no trail the second
  rung is not there and the key clears the filter exactly as it always did. `g`
  is HOME rather than a rung on that ladder: it throws the crumbs and their
  labels away. Evidence: `TestServe` "Shell drill" — "DEL on an emptied query
  pops the crumb and applies it", "DEL over a refined drill strips a token
  before it pops", "DEL with an empty stack clears the filter as it always has",
  "g is home and throws the trail away". **test**
- **The crumb STACK is the renderer's, and this page keeps no copy of it.**
  `setCrumbs`/`getCrumbs`/`pushCrumb`/`popCrumb`, drawn as muted chips left of
  the live ones, `CRUMB_MAX` 4 and then a `… +N` fold — the ownership rule the
  marks and the selected column already follow. `popCrumb` pops and returns
  WITHOUT applying, because whoever owns the fetching owns what a query means,
  so the shell applies what came back through `applyView`, the door `g` and the
  agenda use. What the page DOES keep is `crumbLabels`, token → label, because
  no lookup recovers it: the title belongs to the row referred TO, which is very
  rarely among its own referrers. One map serves both readers — the mount's
  `chipLabel`, naming the live `ref:` chip, and `hereLabel`, naming the crumb a
  further drill leaves behind — so a drill out of a drill chains honestly rather
  than leaving a raw `ref:` spelling in the trail. The detection is the four
  crumb calls together; an asset without them is told so and nothing is applied,
  since a view with no way back out of it is worse than no drill. Evidence:
  `TestServe` "an asset with no crumbs refuses the drill and stays put". **test**
- **The trail crosses a remount through the URL and NOTHING ELSE.** `?crumbs=`
  holds `{trail, labels, sels}` beside `q`, written by `remember` and read back
  by `mount` — before `TableView.mount`, since `chipLabel` can be called during
  the first paint. Every mutation of the stack (a drill, a pop, `g`) is followed
  by a `remember`, so the address bar is current whenever a `view-changed`
  remount re-reads it, and a trail is a link the way the query beside it is.
  `stash`/`restore` deliberately say nothing about crumbs: what they carry is
  work the reader has NOT committed, and there is no such thing as a
  half-applied crumb. A parameter that does not parse is one boot without a
  trail, and `setCrumbs` drops whatever is not a crumb. Evidence: `TestServe`
  "the trail and its labels ride in the URL beside the query", "a remount
  restores the trail and the labels", "a booted trail is restored from the URL
  and can be walked back". **test**
- **Where the cursor lands is THREE rules at one door, and a BOOT IS AN APPLY.**
  `land(sel, back)` is the whole of it: it takes the row `sel` names when the
  view still holds it, else the row at index `back`, else — with no rows at all
  — nothing. What the callers ask for is the whole difference between them:

  | landing | asks for | falls back to | door |
  | --- | --- | --- | --- |
  | apply a view (palette commit, `g`, `a`, `@`, a filter commit) | nothing | row one | `applyView`, `fetchRows` |
  | BOOT the page (a reload, a `?q=` link, a `view-changed` remount) | nothing | row one | `start` |
  | pop a crumb (`DEL` out of a drill) | the row the drill was pushed from | row one | `applyView` |
  | archive (`d`, `D`) | the next surviving row below point | that row's place among the survivors | `settled` |

  A MOUNT HAS NO CURSOR OF ITS OWN. The renderer selects nothing until it is
  asked to (`selectFirstVisible` has one caller and it is the filter box handing
  over), so a boot that landed nothing opened with `d`, `D` and `RET` all
  answering `no row` until the reader pressed `n` — and a boot IS an applied
  view, so the apply rule reaches it: `start` lands through `land` like every
  other caller rather than growing a first-row rule of its own. It lands on the
  MOUNT, so the `?limit=100` first paint carries it and the full set arriving
  behind it lands nothing more — one landing per mount, and `paint` keeps the
  cursor the way the renderer keeps every selection. A caller that PASSES a
  landing lands inside its own `after` and this door stands aside for it, which
  is what leaves a pop's remembered row untouched.

  `select` answers false for a row the view no longer holds, so a remembered row
  an edit or a narrower filter took away falls through rather than being forced
  back. `applyView` takes the remembered selection as a fourth argument so the
  rule runs once rather than once per caller, and `fetchRows` calls it too,
  since a commit REPAINTS rather than remounting and would otherwise leave the
  cursor on a row the new answer may not hold. The remembered selection rides
  BESIDE the trail (`crumbSels`, one entry per crumb) rather than inside it,
  because the renderer's `crumbOf` keeps a crumb's `label` and `query` and drops
  everything else — a selection put in a crumb would never come back out of
  `getCrumbs()`. The renderer's DEPTH stays the truth: `selsFit` compares
  lengths and a side table out of step is dropped whole rather than pairing a
  crumb with another crumb's row. Marks and flags need none of this, being
  id-keyed renderer state that already survives. Evidence: `TestServe` "a boot
  lands on row one, like every other applied view", "so the first key pressed
  already has a row to work on", "an empty answer leaves nothing selected, and d
  says so", "and RET says which key would pick one", "a pop out of a booted
  trail still lands on the remembered row", "a pop puts the cursor back on the
  row the drill was launched from", "and the column it was in, when one was
  set", "a remembered row the answer lost falls back to the first row", "g lands
  on the first row rather than where the reader was", "a commit that repaints
  lands on the first row too", "an applied view still lands on row one after an
  anchor did not". **test**
- **The harness's mount has NO SELECTION until something selects in it.**
  `shell-harness.js` models `state.selected === null` as `rowId` null and
  answers for it everywhere the renderer does: `keepSelection` returns at the
  guard, `indexOfSelected` answers -1, `getSelection` answers a null id, and
  `selectStep` from nothing lands on the end it is stepping away from. It used
  to answer `getSelection` with row 0 of the page whatever had happened, and
  that one lie hid the boot landing above from ~170 cases that pressed a row key
  as their first act. A harness that stands in for the renderer owes the
  renderer's empty states, or every case resting on one is unverified. The
  second half of the same rule: a `total` of 0 is an EMPTY STORE, since the
  count the server reports is the count of the set it answers with. It is argv
  rather than an act because no act can reach it in time — every one of them
  runs after the boot has painted. Evidence: `TestServe` "an empty answer leaves
  nothing selected, and d says so". **test**
- **An archive lands point on the NEXT SURVIVING ROW, and a refetch the watch
  caused lands nothing.** dired's rule, and the carve that makes room for it.
  The anchor is taken at FIRE time (`anchorFor`), because by the time the rows
  have gone the answer is unrecoverable — the gap they left is exactly what a
  later read cannot see. It is worked out from POINT rather than from the set:
  down the page for the first row not leaving, and only failing that back UP for
  the nearest one, which is what a reader is owed when the row they were
  standing on goes. It carries three things — `from` (the row point was on),
  `id` (the anchor) and `at` (the anchor's place among the SURVIVORS, the
  fallback for the anchor itself vanishing before the landing).

  THE DOOR THE ROWS LEAVE BY IS THE FILTERED REFETCH, and there is only one
  other. `archive` puts an UPSERT on the wire — `Store.streamed` emits a delete
  only for an id absent from the store afterwards, and adding `:ARCHIVE:` leaves
  the row emitted under the same id — so an UNFILTERED client splices the row
  straight back in and point does not move at all. A filtered one reads no frame
  content, refetches behind the 250 ms debounce, and gets an answer the row has
  dropped out of. The second door is `resync`'s repaint: a socket down while the
  write landed makes the reconnect's answer the first this page sees without the
  rows. All three call `settled`.

  `settled` ALWAYS SPENDS the anchor and lands it only where something is owed.
  Spending unconditionally is what keeps it describing ONE watch step: left
  armed, a page turn and somebody else's edit minutes later would pull the
  cursor to a row this write had an opinion about. It declines to land in two
  cases — `from` is still in the view (the unfiltered client, and a
  `tag:*archive*` query that still matches it), and the page showing is not the
  page the anchor was taken on, since `visible()` is ONE PAGE and can say
  nothing about a row outside it. `spent` drops the anchor when the answer says
  `from` was not archived, which is what a refusal and an archive over a set
  point is not in both look like; it is keyed to the anchor it answers for
  (`spent(mine)`), so an earlier archive's answer cannot disarm a later one's,
  and it decides the anchor BEFORE `unmark`, which can throw on an asset
  carrying half the mark calls. A `commit` and a `remount` each drop it outright:
  an anchor belongs to the view it was taken in.

  THE CARVE: `fetchRows` takes the landing as an argument, and the watch's
  refetch passes `settled` where a commit passes nothing. A refetch is the view
  the reader already had arriving again because a file moved, so it is not a new
  question and lands nothing of its own — the renderer keeps the cursor and only
  an armed anchor may override that. Before the carve, ANY watch event under a
  filter took a reader back to row one.

  What the anchor buys over the renderer's own `keepSelection` is the case where
  rows went from ABOVE point too: `keepSelection` keeps the visual PLACE, which
  is a row further down once the rows above have gone, so it skips one. Its
  other branches agree with the anchor exactly — the up-scan, the empty view and
  the surviving-row case are all guaranteed twice, which is why the cases for
  them pin the outcome rather than which half produced it, and why nothing
  exercises the up-scan alone. Evidence: `TestServe` "Shell landing", sixteen
  cases; the one that separates the anchor from the renderer is "the anchor is
  the next surviving row, not the place point stood". **test**
- **`@` takes the row at point and never the marked set.** A drill is a look,
  and a key that inherited a mark would make every mark change what it means —
  the reasoning that keeps `D` off marks, arrived at from the other side. The
  crumb goes down BEFORE the view changes, so what it records is where the
  reader was standing rather than where they landed; `applyView` then writes the
  query and the trail into the URL in one `remember`. The token is `refToken`,
  which quotes the value where the id carries whitespace, `&` or a quote — the
  fallback row id is `PATH#K` and a path may hold a space the grammar would
  otherwise cut the token at. The echo is
  `@ → org-glance-overview:relations (references of "TITLE" · N)`, the count
  being the server's answer, which is the one number a first page cannot give.
  On the `ONCE` list: a held `@` is a remount per repeat, each one leaving a
  crumb for `DEL` to walk back. Evidence: `TestServe` "@ applies a ref view over
  the row at point and leaves a crumb", "the pill names the command, the row and
  the count", "a held @ drills once". **test**
- **`@` ASKS BEFORE IT APPLIES, and zero references is no jump.** The drill is
  probed with the same query under `limit=1` — a count and one row, which is all
  the number costs — and a total of zero applies NO view: the table, the filter
  and the trail stay exactly where they were, with one `cmd` info line naming the
  headline and an echo saying the same. An empty view is the one landing a reader
  can read nothing off, and walking back out of it costs a keystroke to undo a
  keystroke. What it costs is a second fetch on a key that was going to refetch
  anyway, which is one keypress either way; what it changes is that the drill is
  now ASYNC, so a key pressed in the same tick lands ahead of it — every suite
  case that used to press `@ Backspace` together now presses the second as an
  act. Evidence: `TestServe` "@ onto a row nothing refers to applies no view at
  all", "Shell glue" / "the drill is probed before it is applied". **test**
- **A drill out of the EMPTY query pushes no crumb, and that is the absence of a
  special case.** "All rows" IS the empty filter, so `DEL`'s first rung already
  lands there: strip the `ref:` token, the query goes empty, and with no trail
  behind it the key clears the filter — the very view the crumb would have
  restored. The crumb, its label and its remembered selection would be
  bookkeeping for a step the ladder takes anyway. The accepted consequence is the
  cursor: `DEL` back out of that one drill lands on the FIRST row, like every
  applied view that is not a pop. `crumbLabels` is still written, since it also
  names the live `ref:` chip. Evidence: `TestServe` "@ out of an empty query
  leaves no crumb, and DEL is still the way back", "and that DEL lands on all
  rows, first row selected". **test**
- **A filtered answer of zero to a key the columns do not name is checked
  against the rows the page holds.** The renderer suggests tokens off the view
  it holds; the server parses off the view it serves; if the two are different
  versions the suggestion can be a query the applied path evaluates as plain
  text and answers with nothing — which is what a user hit live (`task:tanik`,
  19 suggested, 0 returned, back when a tag was a key). So the shell keeps the last unfiltered answer and,
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
  happens to appear elsewhere: `tag:contact` with no `contact`-tagged rows
  loaded but "contact" in a title fires it. And it consults column keys alone,
  so `planned:` and `ref:` are treated as suspect, while its `key:value` gate also
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
  the question it exists for. The native path prints `native window` in the
  same slot by REPLACING that one line of `dryRunLines` rather than writing
  three of its own, so the two lines the two paths share cannot drift. **test**
- **A build with a window of its own prefers it, and naming a browser beats
  it.** `prefersNative` is the flag AND neither `$GLANCE_BROWSER` nor
  `--browser`: eight rows, one of them True. Explicit beats native because the
  environment variable is how a machine is set up once and obeyed by every
  launcher — an operator who wrote it down meant it, and a build that grew a
  window since then must not quietly ignore it. Unflagged the whole table is
  False and `glance desktop` is stage 1 to the byte. **test**
- **GTK owns the main thread, so the daemon moves off it.** Every other path
  here blocks the main thread in warp; `runNative` forks the daemon and hands
  this thread to the window, because `gtk_main` may run nowhere else. The order
  it keeps is stage 1's: the window opens at the SOCKET rather than at the
  loaded store, so a cold daemon shows the indexing page instead of nothing.
  A daemon that stops before it listens — a missing directory, a taken port —
  has already said why, and this exits 1 rather than waiting for a socket that
  is not coming. **test**
- **The window IS the app: closing it stops the daemon, and `--keep-serving`
  puts stage 1 back.** Stage 1's window is a browser's and closing it is
  closing a tab; this one is this process, and leaving a headless daemon on the
  port after its window is gone would be a surprise with no way to see it. A
  window that never OPENED is the exception and leaves the daemon serving: a
  GTK that would not start is a window failure, and no window failure has ever
  taken this daemon down. `gtk_init_check` is what makes that reachable —
  `gtk_init` would print and exit the process over a missing display, taking
  the daemon with it. `Ctrl-C` stops both, through a handler that asks the
  GTK loop to quit: the main thread is inside a foreign call where the RTS
  cannot deliver the interrupt, and the handler puts the previous one back
  before it returns, so a `--keep-serving` session still waiting is
  interruptible again. **test** (the close rules) / **none** (the signal
  handler, which needs an open window; a flagged build alone reaches nothing
  here)
- **A new window goes to the system browser; this one stays the table.** The
  shell's `o` follows a link with `window.open(…, "_blank")`, and a
  `target="_blank"` anchor is the same request. Both arrive at
  `Glance.Desktop.WebKit` as a `WebKitPolicyDecision` of type
  `NewWindowAction`, and a `WebKitWebView` with nothing connected answers one by
  doing NOTHING — so following a link would work in a borrowed browser tab and
  silently fail in the window this build carries, which is the one place a
  reader cannot tell a missing feature from a broken one. `elsewhere` refuses
  the decision (`policyDecisionIgnore`) and hands the URI to
  `gtk_show_uri_on_window`, the desktop's own default handler: a glance window
  is the table, and a second chrome-less one would be a browser with no address
  bar. Every other decision type is left to WebKit, so ordinary navigation and
  the socket upgrade are untouched. The downcast to
  `NavigationPolicyDecision` is CHECKED (`castTo`, not the unsafe one) and a URI
  that fails to open is printed and dropped — a window failure has never taken
  this daemon down and a link does not either. That costs one
  dependency in the flagged stanza, `haskell-gi-base`, which every `gi-*`
  package already pulls. **none** (compiles; nothing has opened the window)
- **The bindings are vendored, and the patch is six lines across two packages.**
  Every Hackage `gi-webkit2` binds WebKit2 **4.0** — `pkgconfig webkit2gtk-4.0`,
  `gi-javascriptcore4` on `javascriptcoregtk-4.0`, `gi-soup2` on `libsoup-2.4`
  — and Arch has dropped that generation; this machine carries
  `webkit2gtk-4.1` (2.52.5, soup3) alone, and no `webkitgtk-6.0` for the GTK4
  binding either. `vendored/` answers it with upstream's own tarballs,
  `gi-webkit2-4.0.32` and `gi-javascriptcore4-4.0.29`, and three kinds of edit,
  every one marked `glance:`: `pkgconfig-depends` to the `-4.1` spelling (one
  line each), `Setup.hs`'s `version` to `"4.1"` (one line each), and `gi-soup2`
  to `gi-soup3` (two lines, gi-webkit2 alone — it names the dependency in both
  `custom-setup` and the library). That
  `version` string is the one that matters — it names the TYPELIB haskell-gi
  loads at configure time, so moving it is the whole of what repoints the
  generator. Nothing else needed moving, because 4.0 and 4.1 are the same C API
  modulo the soup swap. Two things make the patch stay this small: the
  `exposed-modules` lists are regenerated from the typelib
  (`CabalHooks.confCodeGenHook` rewrites them), so an API that differs is not a
  `.cabal` edit; and both packages keep upstream's NAME and VERSION, so a local
  package shadows every Hackage version of its name and
  `cabal get NAME-VERSION && diff -r` is the entire diff. **build**
- **The second missing piece was GIR XML.** `gobject-introspection` ships the
  hand-written GIR files for the foreign types — `cairo-1.0`, `xlib-2.0`,
  `freetype2-2.0` — and this machine has only
  `gobject-introspection-runtime`, which carries their typelibs and none of
  their XML. haskell-gi's generator reads that XML, so `gi-cairo` and
  `gi-freetype2` failed configure with `Did not find a GI repository for
  cairo-1.0`, and `Gtk-3.0.gir` and `Gdk-3.0.gir` include all three. They sit in
  `vendored/gir/` and `make native` puts that directory in
  `HASKELL_GI_GIR_SEARCH_PATH`, which haskell-gi searches BEFORE the system
  path — so installing the distribution package makes the directory dead weight
  rather than a conflict. This is the piece a project file cannot supply, which
  is why the documented command is `make native` rather than a `cabal` line.
  **build**
- **KNOWN GAP (open): the window has been compiled, never opened.** What the
  flagged build now proves is that `Glance.Desktop.WebKit` type-checks and links
  against the real bindings — unchanged, as it was written blind: `initCheck`'s
  `(Bool, Maybe [Text])`, the `Word32` style-provider priority against an
  `Int32` constant, `setRGBA*`, `onWidgetDestroy`'s implicit-parameter callback
  and `idleAdd`'s `Int32` priority all landed as guessed, and the whole build is
  warning-free under this package's `-Wall`. What no run here proves is anything
  a window does: the chords arriving in a chrome-less web view (the reason stage
  2 exists), the black-before-first-paint, closing the window stopping the
  daemon, `Ctrl-C` reaching `gtk_main` through the SIGINT handler, and
  `gtk_init_check` refusing a missing display without taking the daemon with it.
  The new-window policy handler joins the list at the same standing: `o` on a
  row with a link should raise the system browser and leave this window on the
  table, and nothing here has watched it do so.
  That list is the eyeball list, and it now starts at the keys. **none**

## Build

- `glance.cabal` is hand-maintained; hpack/package.yaml were removed after
  diverging (regeneration dropped `OverloadedRecordDot` and deps and broke
  the build). Do not reintroduce without making it authoritative again.
- **`assets/table-view.js` is a build input, and `make sync-renderer` is how it
  moves.** The renderer's home is the sibling `table-view` repository; the copy
  here is committed, listed in `extra-source-files`, and read by
  `Glance.Web.Routes`'s `embedFile` splice — so `file-embed`'s `addDependentFile` recompiles the
  module when the asset changes, and `cabal sdist` carries it. `sync-renderer`
  copies `../table-view/web/table-view.js` over it and prints
  `git diff --stat --no-index`; with no sibling checkout it says so and copies
  nothing, which is what keeps a bare clone buildable. Editing the vendored copy
  by hand is a fork — hack the sibling and sync. **test** (the target exists;
  the byte equality of the served asset and the file)
- **Six components, one direction.** `glance-internal` (`src/`) holds the
  parser, the AST and the file walk at `visibility: private`; the public
  `library` (`src-query/`) exposes `Glance.Query` and depends on it;
  `glance-web` (`src-web/`) is private and depends on the public library
  alone; `glance-desktop-native` (`src-desktop-native/`) is private and depends
  on `base` alone; the CLI depends on the three sublibraries and the suite on
  the three that carry testable code
  (`glance:{glance, glance-internal, glance-web}`, which pins internals in the
  older modules and exercises the facade alone in
  `TestQuery`/`TestServe`/`TestStore`). `glance-web` exposes fourteen modules
  and declares no `other-modules`: `Glance.Desktop`, `Glance.Desktop.Native`,
  `Glance.Web`, `Glance.Web.Base`, `Glance.Web.Commands`, `Glance.Web.Filter`,
  `Glance.Web.Keymap`, `Glance.Web.Page`, `Glance.Web.Page.Glue`,
  `Glance.Web.Page.Style`, `Glance.Web.Routes`, `Glance.Web.Sort`,
  `Glance.Web.Store`, `Glance.Web.Watch`. Inside the component the dependency
  runs one way, `Glance.Web.Base` at the floor and `Glance.Web` at the door, and
  the floor is exactly what more than one module above needs: `ServeOptions`,
  the response constructors, the body reader and the write-refusal vocabulary,
  which the route table and the command table both answer through. It
  gained every one of them past `Glance.Web` without gaining a direction — what
  they needed, per-file loading, row JSON, the keyword merge, the derived and
  org path predicates, was added to `Glance.Query` rather than reached for
  behind it. Putting `Data.Org.*` in a web or daemon target's build-depends is
  impossible from outside the package — the S2 exit bar, enforced by the solver
  rather than by review. **test** (it would not build)
- **One stanza sees the `native-window` flag.** `glance-desktop-native` is the
  only place `if flag(native-window)` appears, and all it does there is add
  `-DNATIVE_WINDOW` and the haskell-gi dependencies. Every other component is
  the same build either way, which is what lets the suite stay green and CI stay
  GTK-free with a window in the tree. The flag is `manual: True`, so the solver
  never turns it on to satisfy something else. The cost when it IS on is real —
  ~28 packages, of which 25 are the gi-gtk 3 tree, all of them generated from
  the system's typelibs at build time — and paying it is a choice made at the
  command line rather than by whoever runs `cabal build`. **test** (the
  unflagged build is the one the suite runs)
- **The native window has a project file, and `cabal.project` never grew one
  line for it.** `cabal.project.native` imports `cabal.project`, adds
  `vendored/`'s two packages and sets `flags: +native-window`; `make native`
  runs it with `HASKELL_GI_GIR_SEARCH_PATH`. Keeping the vendored packages OUT
  of the default project is the whole point: a local package is built by
  `cabal build all` whether or not anything depends on it, so listing them
  there would put GTK3 and WebKitGTK in the way of every unflagged build, which
  is exactly the property the flag exists to protect. It also ties the two
  halves together — the flag and the bindings that satisfy it are in one file,
  and `cabal build -f native-window all` against the default project still
  fails in the solver, the way it did before any of this. **build**
- **The unambiguous spellings, `gi-gtk3` and `gi-gdk3`.** They generate the same
  modules as `gi-gtk`/`gi-gdk` and they are the names `gi-webkit2` depends on;
  the old pair in `glance-desktop-native` would put two packages claiming
  `GI.Gtk` in one plan. **build**
- **A distribution upgrade re-keys the whole gi tree by itself.** cabal's
  package hash counts the resolved `pkg-config` dependency VERSIONS, so
  `glib2 2.88.1 → 2.88.3` and `webkit2gtk-4.1 2.52.4 → 2.52.5` invalidated the
  seventeen store entries built from a `.pc` that moved — `haskell-gi` and the
  whole gi tree — and left the ten pure-Haskell helpers beside them alone. So
  `make native` regenerated the bindings instead of linking the new libraries
  against bindings made from the old typelibs; nothing has to be cleaned by
  hand, and the reverse also holds — a gi package that is NOT rebuilt after an
  upgrade was generated from a `.pc` version that did not move. **build**
- **The suite shells out where a claim needs a real interpreter**, and degrades
  where the machine has none: `node --check` over the extracted glue, and
  `test/fixtures/shell-harness.js`, which boots that glue over a stubbed browser
  and reports the fetches it made. Both answer `pure ()` when `node` is not on
  `PATH`, so the suite is green either way and the boot contract is checked
  wherever there is something to check it with.
- **WATCH (2026-07-31, again 2026-08-01): a test run hangs occasionally and has
  never reproduced on a retry.** Nothing in the suite waits on a socket, the two
  node cases are bounded by the child process, and `TestDesktop.waitUntil` gives
  up after 200 × 10 ms. The second sighting: a `cabal test` sat at 0.1% CPU for
  ten minutes with its output buffer unflushed, and an immediate re-run under
  `--test-options=--timeout=120s` was green in 4.95 s. Two sightings in two
  days makes it a pattern; what is still missing is which test. Run the suite
  with that timeout when it matters — a hang then names the test instead of
  waiting forever. **none**
