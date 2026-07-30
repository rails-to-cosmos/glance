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
- **Drawer placement.** When present, `hsProperties` starts past the newline
  after the headline line, after `hsTitle`, and ends exactly at `hsFull`'s
  end. An append-note command writing at `spanEnd hsFull` writes inside the
  drawer if this breaks. **test**
- **`stripSpans` totality.** Resets headline spans; every other constructor
  passes through. A new span-carrying `Element` constructor silently turns
  ~150 span-insensitive assertions span-sensitive unless added here. **test**

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
  `TestRoundtrip`'s exact list (10 inputs) vs stable list (17) is the
  documented budget: promoting a stable case to exact asserts fidelity the
  renderer lacks. Write-back and the future wire contract must never route
  through it — spans are the lossless channel. **test**
- **`Ord Timestamp` ≠ `Eq Timestamp`.** Ord compares start moments only;
  Set/Map keys would deduplicate distinct timestamps sharing a start. **none**
- **`resolveHeadline` last-wins.** Keeps h1 only when both scheduled and h1
  strictly later; everything else yields h2. **test**
- **`schedule`/`deadline` unpopulated.** Parser hardcodes `Nothing`;
  `SCHEDULED:`/`DEADLINE:` lines parse as sibling Token + Timestamp elements.
  S2's row projection needs these wired first — recorded as S2 work. **none**

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
- Write-back is surgical span replacement + optimistic lock (hash vs parse
  snapshot) + atomic temp-then-rename.
- The web layer depends only on the `Glance.Query` facade (S2), enforced at
  the cabal-stanza level; `Display`/`TextShow` stay out of the wire.
- Browser: structured commands only. Automation: reviewed deterministic
  scripts behind a separate privilege tier; no LLM in the loop.

## Build

- `glance.cabal` is hand-maintained; hpack/package.yaml were removed after
  diverging (regeneration dropped `OverloadedRecordDot` and deps and broke
  the build). Do not reintroduce without making it authoritative again.
