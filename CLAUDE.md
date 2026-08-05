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
  or shallower, else to the end of the document; computed over EVERY headline,
  though only the top-level ones keep records, so the surviving extents tile
  and consecutive ones meet exactly. Trailing blank lines belong to the subtree
  above.

## Parser

- A top-level element must end at whitespace or EOF; a sub-parser stopping
  mid-word fails the WHOLE file — the residual corpus failure class, 11 files
  of 6290 at 2026-07-31. The per-cause breakdown was counted at 13 files and
  has not been re-measured since the derived mirrors left the walk; treat it as
  needing a re-run before it is quoted.
- Headlines parse only at column 1, via the threaded begin-of-line Bool.
  Never `getSourcePos` — quadratic on failure-heavy input.
- Column 1 is necessary and not sufficient: `indentP` also requires the star run
  to END — horizontal space, which it consumes, or end of line/input, which it
  only looks at. Org's own rule (`org-outline-regexp` is `\*+ `), so a body line
  opening `*bold*` is emphasis rather than a row of its own; 251 lines of the
  corpus were rows before it. A bare star run stays the empty headline it has
  always been. The stars never consume the NEWLINE: with `MPC.space` there, an
  empty title ran on and took the next line — its stars included — as its own,
  so `* ` above `* Delta` was ONE headline titled `* Delta`.
- TODO keywords are matched case-sensitively and stored verbatim;
  pragma/property KEYS are uppercased. `PTodo` carries the two halves as LISTS
  in the line's order — a `#+TODO:` line is a cycle, and its spelling is the
  tree's whole say over how states sort and how a palette draws. `#+SEQ_TODO:`
  and `#+TYP_TODO:` are org's older spellings of the same line and land in the
  same `PTodo` (a re-render says `#+TODO:` — TextShow lossiness); fast-access
  selectors (`TODO(t!)`) are consumed and dropped, the keyword stored bare. The parser
  folds the same words into `Context`'s two `Set`s, which is where recognition
  is answered and where order means nothing.
- In `spannedContainerUntil` the end-parser branch precedes the hspace-eol
  branch (tags open with `hspace1` and lose it otherwise).
- Trailing hspace terminates a container and stays unconsumed.
- The property parser rejects reserved `PROPERTIES`/`END` — that guard is what
  terminates the drawer.
- Timestamp range halves share one bracket kind; `tsmHasTime` alone decides
  whether a time renders; the weekday is recomputed from the date.
- The weekday slot takes a run of LETTERS in any script, of any length, and
  drops it — display-only, so a locale's word is as good as org's. Letters is
  the whole charset: a repeater opens with `.`, `+`, `-` or a digit, so one
  letter is what holds `.+3d` out of the slot, and French `lun.` stays refused.
  Exactly three letters lost ~/sync's Dutch stamps (`CLOSED: [2025-12-04 do
  22:34]`) their planning line and, behind it, the drawer and the id whole — 28
  blobs, `dfIdless` 49 → 21. A re-render comes back English; the span channel is
  what carries the source spelling.
- A range is spelled `<a>--<b>` or compactly as `<date wd 10:30-11:30>`;
  `tsCompactRange` preserves which, and the renderer never canonicalizes one
  into the other (CLOCK lines are always `--`). A `-` before a time opens a
  range end, before a unit it is org's WARNING cookie (`-3d`, first-only
  `--3d`), held in `tsWarning` — never a repeater, though `TRSMinus` still
  exists unreached. A stamp takes at most one repeater and one warning, either
  order, first of each kind winning; the render spells repeater-then-warning,
  so a warning-first source re-renders conventionally (TextShow lossiness; the
  span carries the spelling). The second cookie used to block the closing
  bracket and demote the planning line to body, the Dutch-weekday loss class.
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
  pragma keys). Never use it for write-back or the wire contract; spans are the
  only lossless channel. TestRoundtrip's exact-vs-stable split IS the documented
  lossiness budget, and the budget is now EMPTY: 23 `Exact` rows and no
  `Stable`. The seven rows promoted on 2026-07-31 were measured to re-render
  byte for byte; the label had outrun the renderer. The last `Stable` row was
  `#+TODO:`, re-emitting its keyword sets in Set order — ordering the keyword
  lists (#67) took that loss away, and it was measured before promoting.
  `Stable` stays as the mechanism for a case that genuinely is one.

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
- Where a root holds an org-glance store, `scan` also folds its WAL and reports
  `org-glance index: N rows disagree (M state, K archived)`. Stores are each
  root's own `.org-glance/meta` plus every `meta` the walk DECLINED, so a nested
  one is found for free — and missed under `--include-derived`, which walks it
  instead. The fold is `Data.Org.Index`, read-only and faithful to
  `org-glance-graph--latest-records`: MANIFEST's sealed segments oldest-first,
  the open `headlines.jsonl` LAST, latest record per id wins, tombstoned ids
  leave; only the open segment's final line may be torn, and a name is opened
  only when it spells `seg-<digits>.jsonl`. Compared by `ORG_GLANCE_ID` against
  the blob's FIRST headline (never a child's id): the keyword always, the archive
  flag only where the record CARRIES the key, `(eq t VALUE)` so `{}` is false.
  `blobs … carrying no id` is the instrument on itself — a blob glance parsed and
  read no id out of, which is what keeps `records without blobs` from reading as
  index lag. ~/sync/views at 2026-08-02: 6502 read, 6071 live, 6063 blobs, 51
  idless, 21 rows disagree (20 state, 1 archived), 0 unindexed, 59 recordless —
  and 39 one day later, which is the rate the next entry exists to stop.
- The drift FIX is a one-file contract: every successful write to a BLOB
  (`isBlob` — `data.org` in the canonical store; documents, config, overviews,
  occurrences note nothing) appends `{"id","at"}` to `meta/EXTERNAL.jsonl` —
  the blob's FIRST headline's `ORG_GLANCE_ID`, no id no line, one `editFile` one
  line. The note rides `replaceSpans`' success branch, which the five write
  sites reach through `Watch.writeSpans` — `captureInbox`, `captureBlob`,
  `writeOne`, `commit` and `writeLayer` (`Data.Org.External` owns
  format/path/append, by
  `openFd` append + one `fdWriteBuf` — `BS.appendFile` measurably LOSES lines
  under concurrency). The daemon appends only, never truncates, never touches
  another `meta/` file. Emacs's `org-glance-graph:refresh-external` adopts each
  id via `graph:insert` (never `put-content` — blobs are read, not rewritten)
  and shortens the file by the PREFIX IT READ; a crash between = a repeated
  refresh, no-op by construction.
- Corpus check: `cabal run -v0 glance -- scan ~/sync` — expect 0 span
  violations, ~12.6k headlines, and a `walk seconds` row of ~10–11 (2026-08-02:
  6287 files, 12594 headlines, 0 violations, 11.3 s; re-measured the same day at
  6289 files, 12596 headlines, 0 violations, 9.9 s; 2026-08-04: 6292 files,
  12611 headlines, 0 violations, 10.4 s — the tree is live, so a handful of
  files a day is drift rather than a rule moving. The `isSidecar` narrowing
  cost this corpus nothing: it holds no `#name.org` at all, every `#…#` there
  being an auto-save that `isOrg` refuses first). The headline figure was
  carried at ~12.9k after the derived mirrors left the walk (2026-07-31,
  13.4k → 12.9k, a semantic correction rather than a loss) and was not lowered
  when the star-run rule took it to 12.6k; walk seconds went ~13 → ~10.4 on the
  lstat and the `orgGlanceTails` guard, see Walk.
- The `GLANCE_CORPUS` groups still PASS when the variable is unset, and say so:
  `TestDefaults.withCorpusSample` prints `SKIPPED — GLANCE_CORPUS is unset` on
  stderr for each. A green run without those two lines answered is unverified on
  the corpus half. A variable naming a directory that is not there fails loudly,
  and so does a run that samples nothing.

## Walk

- Org files are the source of truth, so org-glance's derived mirrors are not
  walked. The rule is a DENYLIST of names sitting directly under a
  `.org-glance` component — `overviews` and `meta`, with the whole subtree of
  either excluded — plus `isOccurrence`, a blob's history one level further in
  (`data/<id>/occurrences/<STAMP>.org`), which carries the LIVE entry's
  `ORG_GLANCE_ID` and used to tie with it in `beatsForId`. The name is asked for
  ANYWHERE under `data`, since a two-character id is unsharded and no position
  test covers both layouts. `isCanonical` excludes it too, so under
  `--include-derived` it loses the id rather than tying. 0 on disk at 2026-08-02.
  `data` is not privileged in the walk; it survives by not
  being on the list, and is privileged only in `beatsForId`
  (`Data.Org.Walk`), which is a different rule for a different question.
  `isConfig` is a FOURTH exclusion beside those three, and an unconditional one:
  a `config` directory sitting directly under a `.org-glance` component is
  declined whatever `--include-derived` says, with an accumulator and a
  `config skipped` scan row of its own. Nothing there is derived, so nothing
  there becomes truth by asking louder — those files are INPUT to a parse.
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
- ONE `lstat` an entry classifies it (`getSymbolicLinkStatus`, never follows);
  a SYMLINK pays a second `getFileStatus` to classify its target, and only when
  the answer could change what is collected — a link neither named like a
  document nor inside a declined directory is dropped before that stat, which
  is where Emacs's lock exits. Symlinked directories are never followed;
  a failed `lstat` falls to the keep-on-name branch, silently, the way
  `doesDirectoryExist` swallowed one into `False`. An unlistable directory IS
  reported; a symlinked one vanishes silently.
- A non-directory is kept on name alone — no existence check — so a dangling
  `.org` symlink is walked and its load fails as `ReadFailed`, counted once at
  startup and for the life of the process, since the watch is filtered by the
  same rule and no event ever revisits it. Emacs's sidecars are out of that
  rule: `isDocument` = `isOrg` minus `isSidecar` (`.#name.org`, the lock
  symlink that dangles, and `#name.org#`), one predicate for the walk and, via
  `Glance.Query.documentPath`, for `isWatchable`. BOTH SHAPES ARE EXACT: the
  auto-save is matched on its closing `#` as well as its opening one. A bare
  leading `#` took every `.org` file whose name starts with one, so a
  hand-written `#inbox.org` was invisible to the walk, to the watch and to a
  capture target naming it. Only the lock shape can fire through `isDocument` —
  `#name.org#`'s extension is `.org#`, which `isOrg` refuses first — and the
  auto-save stays named so the pair is one rule.
- `orgGlanceTails` guards its `splitDirectories`/`tails` pair with an
  allocation-free character scan for `.org-glance`: a path that does not spell
  the string cannot hold the component, so it is the same function with a fast
  exit. Walk over ~/sync, 2026-08-01: 12.92 s → 12.09 (lstat) → 10.44 (guard).
  The 2–5 s band needs a `RawFilePath` walk — ~4.3 s of what is left is GHC
  marshalling 702k names — and that costs byte-level twins of `isOrg`,
  `isSidecar`, `isDerived` and `isConfig`, so it is an open decision.
- `scan`'s argument parser recognizes `--include-derived` and treats every other
  token as a root, so `glance scan --dir X` walks a nonexistent `--dir`. `serve`
  and `desktop` reject unknown arguments; `scan` alone is permissive, and has no
  usage string.
- `dirs scanned` is the number of ROOTS given, not directories traversed.
- A ROW IS A TOP ENTRY. `recordsOf` keeps `topLevel` headlines (one star) and
  drops the rest; the filter runs AFTER `subtreeSpans`. The orders agree for
  this predicate (a level-one extent ends at another level-one headline) and
  the order is kept because `subtreeSpans` is the outline rule over a DOCUMENT
  — widen `topLevel` and filtering first ends a deeper row at the next KEPT
  headline instead of the next shallower one. Intended consequences: a word only a
  child carries matches nothing (`hrSearch` is the existing rows' cells), a
  deeper `ORG_GLANCE_ID` is not a row id and cannot collide, and a file that
  never reaches level one has no rows. `scan` is unaffected in BOTH tallies —
  it counts headlines and ids off `orgParse`, never through `recordsOf`,
  because it is a parser oracle rather than a view. ~/sync at 2026-08-01: store
  rows 12875 → 10685, collisions 9 → 7; scan 12884 and 9, unmoved.
- AND A ROW HAS SOMETHING TO SHOW. `blankEntry` beside `topLevel` in
  `recordsOf`: a top entry carrying none of the six column sub-spans — todo,
  priority, title, tags, scheduled, deadline — emits no row. The file keeps the
  entry, the table skips it. It is the RECORD's rule (every cell empty) computed
  at the HEADLINE's layer, because the ordinal numbers EMITTED rows and there is
  no record to ask before the numbering; the layers agree because each span is
  `Nothing` exactly where `recordOf` cuts an empty cell. Nothing without a
  column rescues an entry: `CLOSED:`, a drawer — so a blank entry has no row id
  and no command can address it — a body, children. Reading the rule's "no
  planning" as the two planning COLUMNS is the one place it could have gone the
  other way. The tags clause never fires alone, org spelling tags after a title
  and the parser giving `* :tag:` its colons as one. `scan` is unaffected
  (parser oracle). ~/sync at 2026-08-01: 10441 top entries, 0 of them blank, so
  the rule costs the corpus nothing and reaches only what an edit blanks.
- A ROW ID IS `ORG_GLANCE_ID`, else `FILE#K` — K the headline's 0-based place
  among its FILE's EMITTED ROWS, numbered in `recordsOf` after BOTH filters, so
  a child and a blank entry each spend no ordinal. An edit ABOVE a row no longer
  renames it: preamble, title, state, body, drawer and child edits all keep the
  id, and the store streams the row that moved. What still renumbers is the rows
  moving past each other — reorder, insert-ahead, remove, and an entry going
  blank, which is a remove wearing another hat — and that ships cells under
  stable ids rather than a delete plus an insert; `ORG_GLANCE_ID` is the only
  immunity. `set-state` clearing the last keyword off a title-less row is how a
  reader reaches it: the row is deleted and every K behind it moves up one. Replaced `FILE:START`, the offset, which moved on any edit
  above the headline. Nothing parses an id apart (`resolveIds` is exact-string),
  so the separator carries no rule; `#` is safe in `/headline?id=` because both
  sides percent-encode, and `POST /command` carries ids in JSON. Ordinals cannot
  collide with each other — unique per file, path-prefixed across files — and an
  `ORG_GLANCE_ID` spelling another row's `FILE#K` is an ordinary collision.
- An edit under a child moves `hrDoc`/`hrDigest`/the extent and no cell: the
  store still refreshes the entry (so materialize is drift-free) and emits NO
  frame and no generation bump, `streamed` diffing row JSON and `guarded`
  moving on frames or a load outcome alone. `linked` rides in that JSON, so the
  one child edit that DOES stream is the one giving the subtree its first link
  or taking its last — the deeper text can move a row FIELD where it can move no
  cell.
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
  same-dir rename, content-agnostic (no `TextShow`). The rename replaces the
  destination NAME, so a write through a SYMLINKED `.org` file leaves a regular
  file where the link was and the real file untouched — the table then serves
  the copy for ever. The walk keeps symlinked documents on purpose, so it is
  reachable; resolving the target first is a policy decision nobody has taken.
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
- THE DRAIN LOOP IS SERIAL, and that is the correctness argument for reseed.
  ONE `forever` of `drain`, whose body is `settle`, so nothing else is settling
  while a step runs. `drain` is the loop's body lifted into a function — that is
  what lets a test turn it — and it takes the ripe paths OUT in the transaction
  before settling them, so a nudge arriving mid-parse waits a turn rather than
  being lost. A TURN WITH NOTHING RIPE WRITES THE TVar NOTHING: the loop takes
  40 turns a second and request threads write the same var, so an unconditional
  `writeTVar` of the map it just read would dirty it 40 times a second and make
  a concurrent `nudge` retry for no reason. `reseed` builds the fresh store OUTSIDE the transaction and `reseeded`
  installs it wholesale, discarding the live store's rows — sound only because
  the events queued during that walk are re-drained afterwards. Make the loop
  concurrent, which is the obvious fix for the stall below, and any edit that
  landed during the walk is silently reverted.
- A CONFIG RESEED BLOCKS THAT LOOP, so the 100 ms debounce above means "100 ms,
  or a full re-walk" — ~10 s over ~/sync. "No ceiling" is about the debounce and
  understates what a config edit costs every other pending path.
- THE DAEMON NUDGES EVERY PATH IT WRITES, because fsnotify arms a newly
  created directory and does not TRAVERSE INTO it — `mkdir -p a/b` leaves `b`
  unwatched permanently (measured 2026-08-04: one new level under a watched
  directory fires, two do not, and a pause between them does not help). Being
  unwatched is the PATH's property and outlives the write that made it, so
  "the writes that CREATE" was the wrong cut: a tagged capture's blob arrived
  and then every LATER write to that row was lost — the file read `* STARTED`
  while the table still said `TODO`. ALL FIVE write sites therefore leave
  through `Watch.writeSpans` (`replaceSpans` + a nudge of the path just
  written, on the SUCCESS branch): `captureInbox`, `captureBlob`, `writeOne`,
  `commit` and `writeLayer`. Nudging a watched file costs nothing — the queue
  is keyed by path, so it coalesces with the events landing behind it and the
  pair is one parse — and the path is spelled ONCE per write. The queue is the
  watch's debounce map, moved onto the `Hub` (`hubPending`) so a request thread
  reaches it; `Watch.nudge` is the ONE door into it and inotify's own handler
  goes through it too, so `watched` filters a nudge exactly as it filters an
  event. Nothing loads or publishes at the door — `settle` on the serial loop
  stays the sole store updater, and `drain` is that loop's body as a function,
  taking the ripe paths out in the transaction before settling them. KNOWN GAP:
  an EXTERNAL create into a fresh shard (org-glance's Emacs side) is still
  invisible until a restart — the nudge covers what this daemon writes.
- Deletion is decided by `doesFileExist` at reload time, not by the event kind.
- `stTags` counts FILES, not rows: it is stepped by the set difference between
  a file's old and new projection, so a tag on forty rows of one file counts
  once.
- `stDirErrs` and `stPrint` are written by `loadStoreWith` and by nothing else,
  which means they move on a RESEED as well as at startup: `Watch.reseed` calls
  that same loader and `reseeded` installs the fresh store wholesale. So a
  directory that becomes readable, or a file whose bytes moved, is invisible to
  those two fields until the next config change or a restart — a per-file watch
  event never touches either.
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
- A build carrying its own window (`make native`) prefers it, and
  naming a browser beats it: `prefersNative` is the flag AND neither
  `$GLANCE_BROWSER` nor `--browser`. GTK owns the MAIN thread, so `runNative`
  forks the daemon and hands this thread to the window, which is the reverse of
  every other path here. The window opens at the socket, like stage 1's. Closing
  it stops the daemon — the window IS the app — and `--keep-serving` restores
  stage 1, where the daemon outlives it. A window that never opened leaves the
  daemon serving; a daemon that stops before it listens exits 1 rather than
  waiting for a socket that is not coming. `--dry-run` prints `native window`
  where it prints the browser command, by replacing that one line of
  `dryRunLines` rather than by writing three of its own.
- A NEW WINDOW the page asks for opens as a READING PANE over this one, which
  stays the table: 80% × 90% of the main window, centred, transient, ESC or
  the manager's close ending it; its own new-window asks navigate in place. An
  `http(s)` target alone earns the pane (`webby`); everything else goes to
  `gtk_show_uri_on_window`, and a URI that will not open is printed and
  dropped, like every other window failure here. TWO DOORS BECAUSE WEBKIT HAS
  TWO, and only one is usable: a real `target="_blank"` anchor arrives as a
  `NewWindowAction` policy decision (`elsewhere`, downcast CHECKED via
  `castTo`, every other decision type left to WebKit), while the shell's
  `window.open` fires the `create` signal INSTEAD — unconnected it drops the
  open silently, and CONNECTED it aborts the daemon: WebKitGTK reads the
  scripted open's `WindowFeatures` optional, which `"noopener"` leaves
  disengaged (live SIGABRT under 2.50). So `openOverride` patches
  `window.open` at document start — top frame only, the popups keep the real
  one — to post its URL to the `popup` script-message handler, and
  `openMessage` opens the pane itself: WKWebView's own shape, which is what
  the iOS/Android ports inherit.
- The flag is manual and default False, and the unflagged build resolves no
  haskell-gi: `Glance.Desktop.WebKit` answers `nativeAvailable = False` and
  nothing else in the program asks about the flag. `Glance.Desktop.Native` holds
  the whole flow with no GTK in it and takes the window as a `String -> IO ()`,
  so both flag states compile and the suite tests the flow against a fake
  window in either. The engine knows no daemon and the flow knows no GTK; they
  meet in `app/Main.hs`.
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
- Commands: one route, `POST /command {name, id | ids, args, digests?}`, over ONE
  table — `commands`, name to `{argument shape, dated, one-row, edits}`. Ten
  entries: `set-state {keyword: KW | null}`, `set-planning {keyword:
  SCHEDULED|DEADLINE, date: TEXT | null}`, `set-title {title}`,
  `set-priority {priority: LETTER | null}`, `archive {}`,
  `capture {text, tag?, fields?}`, `add-tag {tag}`, `remove-tag {tag}`,
  `rename-tag {from, to}`
  and `edit-link {span: [S, E], target, desc: TEXT | null}`.
  `rename-tag` names both ends rather than reusing `tag` for one of them, and it
  is a command rather than a remove and an add a client fires in turn: those two
  edit sets APPLY — they touch, and `applyEdits` rejects only overlap — and what
  they compose to is the tag spelled onto the title, or moved to the end of the
  run (`renameTagEdits`); the pair would also be two writes under two digests
  where the rename is one. `commandNames` is its keys, the
  per-name request-shape guards are each entry's own `csArgs`, and only
  `set-planning` is `csDated` — the one command whose date is read against the
  server's today. `csArgs` is handed the IDS beside the `args`, because a shape
  refusal is about the REQUEST rather than about the `args` object alone: only
  `edit-link` reads them, its args naming a row's own TEXT (CHARACTERS, like
  every span here), so a span means nothing to a second row and over two files
  would name a different range in each. `wantsLink` owns that message and puts
  it FIRST — the row count is the coarsest thing wrong with the request, and
  naming a missing span instead would answer the smaller question. Nine of the
  ten entries ignore the list, which is what a rule only one command has looks
  like when it is not lifted into a flag every entry must answer.
  `parseCommand` resolves the name BEFORE anything else and a
  `Command` cannot be built without the entry it resolved to, so nothing below
  has an arm for a name this server does not implement.
  Ids group by FILE and each file is one drift-locked `replaceSpans` call, so a
  marked set over three files is three atomic writes; there is no cross-file
  rollback and the answer is per id (`{results: [{id, ok, digest | error}]}`, in
  the order the ids were named). Request-shape refusals are 400 with nothing
  written — a bad body, an unimplemented name, no ids, a keyword ANY named row's
  CHAIN does not declare (named with the row), a `set-planning` date no
  parser reads, and `edit-link`'s own five — more than one id, no span, an
  empty or padded target, a span that is
  outside the named row's subtree or is not one whole link, and a replacement
  that would not read back as the link it claims to be. Each of those refuses the
  WHOLE request rather than moving the rows it could, as does a
  `tag` that is not one, since a word that is not a tag is not a tag for any
  row (`Glance.Query.tagText`, the PARSER's charset). Per id: an
  unknown id, and a client digest the store no longer holds (per file, since a
  digest is). 413 outranks everything. `args` is read once into `Args`, and
  `.:!` rather than `.:?` is what tells an ABSENT field from a NULL one; `text`
  and `tag` are flat, neither having a value to clear. For `keyword` and `date`
  an absent field is a 400; for `edit-link`'s `desc` absent is the ORDINARY case
  and means the link keeps the description it has. The
  route never writes the store — the watch is still the sole updater.
- `capture` is the ONE id-less command: it makes a row rather than editing one,
  which in the table is the entry with no row function, so `{"ids": …}` is not
  owed and the rows-are-named rule does not reach it — `runCommand` reads the
  `Maybe` once and hands the edits themselves down. The
  answer is `{ok, file, digest, id}`, `id` being the row it MADE. WHERE it goes
  is the optional `tag`: ABSENT is the inbox, PRESENT is a blob in the store.
  `tag` takes the ordinary `tagText` wall with the rest of the request's SHAPE,
  so past `wantsText` the field is absent or a real tag and `captureInto` is one
  `maybe` with nothing to strip or test.
  UNTAGGED, WHERE comes off the config
  (`Glance.Query.captureTargetIn`), never the request; the entry is `* <text>`
  plus a drawer holding `:ORG_GLANCE_CREATION_TIME:` — org's INACTIVE stamp,
  server clock, at column 1, lines ending the way the target's own do
  (`eolOf`) — appended at the END of the target, under the
  target's own digest (the store's where it holds the file, else a fresh
  `currentDocument` read, which is `Data.Org.Edit.readDocument` under the
  absent-file convention and answers `("","")` for a file that is not there, so
  the capture creates it under the empty pin). The text is raw org.
  THE ONE-HEADLINE WALL IS BOTH PATHS' (`Glance.Query.captureText`, over
  `oneLine`): the line is refused empty-after-strip or carrying a newline, and
  under a TAG so is every `fields` answer — the line goes to the template's `%?`
  and an answer to a `%^{PROMPT}`, both spliced into ONE document, where a
  newline lands a column-1 star the parser reads as a second entry. A refusal is
  the whole request's 400 naming the field, with nothing written. Both stamps —
  the creation time and a planning timestamp — are rendered by one `orgStamp`,
  which differ only in their brackets and both compute the weekday. The `id` is
  `rowIdIn path K` with K the count of `recordsUnder` — the store's rows for that
  FILE, never `storeRecords`, which is `resolveIds` over the whole store and
  drops a collision loser the ordinal was handed out before. A race, honestly:
  `/command` never writes the store, so K is what the last load saw.
- A TAGGED capture is a BLOB, org-glance's own layout verified against its source
  and this corpus (2026-08-04). Store root = the SERVED root's own `.org-glance`,
  and a tree that keeps none is a 400 naming the directory rather than a daemon
  making one. The id is `Data.Org.Blob.mintBlobId` = `org-id-uuid`'s form, a
  random v4 UUID, 36 characters, lowercase, `8-4-4-4-12`; the path is
  `blobPathIn` = `data/<FIRST TWO CHARACTERS OF THE WHOLE ID, unfolded>/<the
  entire remainder>/data.org`, with an id of two characters or fewer unsharded.
  READING an id is a different question: ~/sync's 6073 blobs carry four
  superseded generations beside 45 UUIDs and shard under `Pa`, `Pe` and `al`
  alike, so an `ORG_GLANCE_ID` is an OPAQUE STRING everywhere it is read.
  `Data.Org.Blob` is a module because Walk CLASSIFIES a path that is there and
  this CONSTRUCTS one; it imports Walk's three layout names, and keeping the mint
  out of Walk keeps crypto and IO off the walk's hot path. `uuidFrom` is TOTAL
  on a short byte string — it pads to sixteen with zeros rather than answering a
  string of the wrong length — so the shape is a function of the bytes and a
  test can pin it without a running entropy source. NO RESERVATION — the
  write goes out under the EMPTY digest, so a path that already holds a file
  DRIFTS rather than being overwritten, and an id that is not written is one
  nobody sees. The `EXTERNAL.jsonl` line costs nothing: `data.org` under a store's
  `data/` is `isBlob`, so `replaceSpans` appends it on the way out as for any
  other blob write — blob first, line second, the order the contract asks for.
  `blobDocument` ENDS THE TEXT FIRST and measures afterwards: a template is
  stored right-trimmed, so a title line with no newline of its own would take the
  drawer onto the end of itself, and every offset below is measured in the text
  that gets written. It composes the blob out of the EXPANDED template and its
  two rules
  are the command layer's own: the tag through `addTagEditsIn` (the very function
  `add-tag` runs, factored out of `addTagEdits`), and the drawer joining an
  existing `:PROPERTIES:` under its OWN indentation else written whole under the
  PLANNING LINE — from the title line instead it splices BETWEEN a headline and
  its `SCHEDULED:`, where the planning line stops being read as one. A template
  that expands to no headline is refused: the blob would carry no entry and
  `blobIdOf` would read no id back out of it. THE REFUSALS ARE ORDERED, coarsest
  first and all of them ahead of a byte: the store directory, then the line and
  every `fields` answer against the one-headline wall (`captureText`), then the
  expansion's own two, then a template that expands to no headline. ONE CLOCK
  READ covers BOTH stamps — `Time.getZonedTime` once per request, handed to
  `expandTemplate` for `%U`/`%T` and to `captureStamp` for the drawer — so a
  template spelling a stamp and the creation time it is filed under name one
  moment. The blob's shard is unwatched for
  the daemon's life, so the capture AND every later write to that row reach the
  table only because every write nudges its own path (see Watch) — which is what
  the shell's `arriving` lands on.
- A TAG'S CAPTURE TEMPLATE IS ITS CONFIG LAYER'S FIRST HEADING — the file that
  already carries its `#+TODO:` cycle, org-glance's own convention and no new
  file class. Read the way `org-glance-tag-config--entry` reads it: from the
  first `^\*+ ` LINE to the END of the file, right-trimmed, rather than as the
  outline extent — so ~/sync's `book.org` (`* Book` over `*** Notes`) is ONE
  template. Everything ABOVE that heading is the pragmas and comments the
  `#+TODO:` splice and the two settings lines own, so the regions cannot overlap.
  `captureTemplateIn` FOLDS THE TAG to find the layer and the HEADLINE wears it
  verbatim: config file names are lowercase (`clTags`' own rule), so `:Book:` and
  `:book:` reach one template while the entry keeps the spelling the request
  asked for. The chain is the tag's own layer (the FIRST configuring it), then
  the system layer's (`systemSetting`'s), then `bareTemplate` = `*
  %?` — a CONSTANT rather than a branch, so every case takes ONE path through
  `expandTemplate`. Read at capture time through the same `readConfigLayers`
  `/config` uses, so what the settings sheet shows is what a capture expands.
  ONE HEADING PREDICATE, `headingStars` (`^\*+ `: stars then HORIZONTAL SPACE, so
  a bare star run is body text here where the PARSER reads it as an empty
  headline), asked by both `headingAt` and `topEntry`; the one-star wall is the
  WRITER's alone and keeps a blob's first headline the entry org-glance keys it
  by. With two predicates the sheet was handed a `** Notes` template it would
  then refuse to write back.
- THE EXPANSION SUBSET IS ONE LIST AND ONE SCAN, and they are TWO SPELLINGS.
  `captureCodes` is `%?`, `%U`, `%T` and `%^{PROMPT}` with a line of meaning each
  — what `GET /capture` serves and the settings box completes over; the scan
  (`templateParts`) spells the same four out as a case and never consults the
  list, so a `TestQuery` case puts every advertised code through the scan and a
  code the list gained alone would come back as its own text.
  `templatePrompts` (the asks in order, one spelled twice
  asked once) and `expandTemplate` are two answers off that one scan. EVERYTHING
  ELSE COPIES THROUGH — `%^` with no brace, an unclosed `%^{`, `%a`, a trailing
  `%` — so no template is unreadable and an unknown code is captured literally.
  Two refusals, both the WHOLE request's: no `%?` (nowhere for the line) and an
  ask nobody answered. The clock is read ONCE per request, so `%U` twice is one
  moment. KNOWN DIVERGENCE from org-glance, deliberate: its renderer also
  rewrites the template heading's TITLE from the capture's title, so a template
  whose heading carries a placeholder keeps it and the line lands at `%?`.
- `GET /capture[?tag=NAME]` is what a client reads before it can ASK anything:
  `{template, prompts, tags, codes}` — whether a layer configures one, its asks
  IN TEMPLATE ORDER, the tree's whole tag vocabulary, and the subset with its
  meanings. NO tag is the untagged path's own shape (no template, no prompts):
  the inbox capture stays bare, so there is nothing to resolve and the answer
  says so rather than refusing. `tags` is here rather than on `/tags` because
  that route answers about ROWS a caller names and a capture names none. Needs a
  loaded store; read-only, so POST is 405.
- The capture target is `#+GLANCE_CAPTURE_TARGET:` in `system.org`, resolved
  against the SERVED ROOT; absent means `<root>/inbox.org`. An absolute path,
  one climbing out through `..`, and a name the walk would not COLLECT are
  refused where the config is READ — printed on the startup banner and answered
  as a 400 — rather than at capture time, since a capture into an unwalked file
  writes an entry no watch delivers a row for. That third rule is
  `Data.Org.Walk.isWalked`, all three of `visit`'s predicates rather than
  `isDocument` alone: `.org-glance/config/x.org` and `.org-glance/overviews/x.org`
  are org files the walk declines, so an extension test would bless exactly what
  the refusal is for. `visit` keeps its own spelling because it holds the three
  answers already and re-asking would scan each path three more times.
- `set-planning`'s span math is `Glance.Query.setPlanningEdits`: an entry already
  there is its own span; an entry the line lacks joins the END of it; a headline
  with no line grows one under its TITLE LINE (`titleLineEnd`, shared with
  `archiveEdits`) at column 1; a clear takes the entry plus the TRAILING
  horizontal run, or the leading one where the entry ends its line, and takes the
  WHOLE LINE when it was the last entry — the lens rule, `CLOSED:` counting as an
  entry. Clearing what was never there costs no edit. `planningTimestamp` parses
  the date once per request against the server's today: a bracketed value is kept
  verbatim once it REPARSES, `today`/`tomorrow` and `+Nd`/`+Nw`/`+Nm` work a date
  out, and a bare ISO date takes an optional `HH:MM` — all three rendering an
  active timestamp with the weekday computed. Everything else is the whole
  request's 400, naming the input.
- `setPriorityEdits` is `setStateEdits`' three shapes one part along: a token
  already there is its own span; a headline with none takes `" [#X]"` behind the
  KEYWORD (org's place — a priority follows the state) else behind the stars; a
  null deletes the token plus the HORIZONTAL run behind it, so `* TODO [#A] T`
  closes up to `* TODO T`. Clearing a headline that carries none costs no edit,
  which is what lets the ring's wrap through NONE be pressed twice. `priorityText`
  is the wall and it is the whole request's: ONE ASCII letter, uppercased — org's
  `A`–`C` cycle is the READER's window, so a tree spelling `[#D]` is writable and
  simply unbadged.
- ORG'S PRIORITY RING, pressed rather than picked: `S-<up>` = `priority-up` runs
  `none → C → B → A → none` and `S-<down>` the reverse, both `table` scope and
  both in `ONCE` (a held key would walk the ring and land on the parity of the
  repeat count). Marked-else-point, like `set-state` — but EACH ROW CYCLES FROM
  ITS OWN VALUE, which `args` cannot carry for a mixed set, so the shell groups
  the targets by LANDING value and fires one command per group (the tags popup's
  rule from another side: a command names one value). A set that agrees is one
  request. The echo is `S-<up> → priority-up ([#B] · 3)`, `*empty*` where the
  wrap landed on none. The keys reach the DOCUMENT too, over the entry the sheet
  is standing on and refused on a child; `RET` on the priority cell still refuses
  and now names the two keys — a ring of three is pressed, not picked from a
  list.
- `setTitleEdits` replaces the title's own span, or inserts `" TITLE"` behind
  the priority, else the keyword, else PAST the horizontal run after the stars —
  never `titleLineEnd`, whose answer includes the TAGS, where a title would read
  back as tag text. `titleText` is the wall and it is the whole request's: a
  title is at least one character (a headline with none is a `blankEntry` and no
  longer a row) and is ONE line. What it may SAY is the author's — a title
  ending `:word:` reads back as a tag run, which is org's grammar rather than
  something the command refuses.
- The span math is `Glance.Query`'s, because `HeadlineSpans` is
  `glance-internal`'s: `setStateEdits` replaces the keyword span, inserts
  `" KW"` at `spanEnd hsStars` when there is none, or deletes the keyword plus
  the HORIZONTAL run behind it (so a keyword ending its line keeps the newline);
  `addTagEdits` inserts `TAG:` at `spanEnd hsTags`, else `" :TAG:"` at
  the end of the TITLE LINE — the max end of stars/todo/priority/title, since
  `hsFull` ends at a planning timestamp or a drawer on a later line;
  `removeTagEdits` cuts `TAG:` out of the run, and the LAST entry takes the whole
  run plus the horizontal run in front of it (the parser's own `hspace1`
  separator, so there is always one). `archiveEdits` IS `addTagEdits archiveTag`
  — one insertion rule, not two that have to agree — and its idempotence is that
  function's. Both tag commands are idempotent, from opposite sides: adding what
  `tagged` finds and removing what it does not each cost no edit. Presence is
  FOLDED, through the same `tagsOfCell` the filter vocabulary is built with, and
  a removal takes EVERY entry spelling the tag, so "removed" means the row stops
  answering to `tagged`. Keyword
  legality is per file (`hrKeywords`); `*active*`/`*inactive*` are in no keyword
  set and are refused like any other word.
- `/headlines` hides archived rows unless the query names the archive META
  `tag:*archive*` (`Glance.Web.Filter.namesArchive`, any spelling — negated,
  quoted, beside other tokens), and `X-Glance-Archived` counts what it took. The
  predicate is exactly `-tag:*archive*`. THE STARRED SPELLING ALONE: `tag:archive`
  is the ordinary substring predicate every other tag gets, so a tree using the
  word for something of its own filters on it and lifts nothing — over ~/sync at
  2026-08-02, `tag:*archive*` serves the 322 archived rows and `tag:archive`
  serves 0 while reporting all 322 withheld, which is what the two spellings
  differing looks like. The question is asked in two halves and each is asked
  once: whether the tree carries the tag at all is `/headlines`' (`storeTags`,
  and with nothing archived there is nothing to hide), whether the query named
  it is `namesArchive`'s, which takes the query alone. The socket is NOT
  filtered: it carries row ops whatever the client's query, so an unfiltered
  client splices in an archived row `/headlines` would not have served — the
  shell's default query makes it refetch instead.
- Materialize: `GET`/`POST /headline?id=…[&child=K]` serves and replaces a
  headline's raw subtree. The digest is pinned at load, any divergence is a 409
  with the file untouched, and the write path never WRITES the store — it reads
  it for the extent and the digest, and the file watch is the only thing that
  updates rows.
- SUB-ADDRESSING is `?child=K`: a child has no row of its own, so a ROW id plus
  an INDEX names one — K the K-th headline inside that row's subtree in DOCUMENT
  order (`Glance.Query.subtreeEntries`, one re-parse of the file per call from
  the load's own seed, extents off the same `subtreeSpans` the rows are cut by).
  So a grandchild is one number from the row rather than a path. The answer
  carries `child` (the index it IS), `parent` (the one `DEL` climbs to, null
  being the row), `children` (the DIRECT descendants with the index each answers
  to), `path` (the titles from the row down), `cells` (the four the table shows)
  and `ownLines` — how many lines of `body` are this entry's, ahead of the first
  child's stars, so the same bytes are never both a paragraph and the child that
  owns them. The digest and the id stay the ROW's: one file, one lock. An index
  the subtree has no entry for is a 404 and one that is not a number is a 400
  (`limit=`'s rule — a mistyped index that served the parent would look like a
  working request and a write pinned to it would splice the wrong subtree).
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
  a rename would break — and `ORG_GLANCE_CREATION_TIME`, which a capture stamps
  and nothing may restate) and the whole logbook. `headlineParts` drops them,
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
  `Cache-Control: no-cache`; the generation moves in ONE function after load,
  `Store.installed`, and it has two callers — `Store.guarded` for a per-file
  event and `Store.reseeded` for a config reseed — so the two cannot come to
  disagree about the rule, which is: frames produced, or a file's load outcome
  moved. `installed` takes the counter off the OLD store, which is what makes
  `reseeded` CARRY IT OVER rather than restart at the fresh store's zero, so a
  client revalidating across a reseed can never be handed a tag it has already
  seen. The fingerprint is not one of the conditions and is fixed per load. One
  tag covers every query variant: the
  parameters are in the URL and an HTTP cache is keyed by URL, so the response
  is a function of (tree, generation, URL) and no `Vary` is owed for them — gzip
  writes the `Accept-Encoding` one itself.
- The HTTP surface is a fixed route table, each entry declaring whether it needs
  a loaded store, the METHODS it takes with the handler for each, and how it
  spells a 405. `HEAD` aliases `GET` in one place, so no entry names it and no
  refusal sentence mentions it. GET is the whole of the table except `POST
  /headline`, `POST /command` and `POST /config`; anything else is 405 — JSON on
  those three, naming the route's own methods (`/config takes GET and POST`,
  derived from the entry), plain text elsewhere, where the sentence names the
  routes that DO write. An upgrade aimed at any path but `/ws` is rejected.
- `GET /keywords?ids=A,B` is the state palette's source of truth:
  `{sources: [{source, active, inactive}], unknown: […]}`, one entry per SOURCE
  in precedence order over the ROWS named — `default`, then `system`, then their
  tags in row order, then `file` — with each keyword under
  the WIDEST source that declares it and nowhere below it
  (`Glance.Query.keywordSources`, which is `classify` read forwards; the dedup
  IS the classification rule). A source left empty is dropped, so a `system.org`
  redeclaring TODO/DONE shows its other keywords and no row when it has none.
  FOUR sources and
  no `union` row: the recognition seed is not a scope, so another tag's cycle is
  neither shown nor settable on a row that does not carry the tag. Over ONE row
  the answer IS `setStateEdits`' rule. Several ids merge by source NAME, so a
  keyword one row reaches by file and another by tag lands in the WIDER — the
  table describes the SET rather than any one member of it, and a keyword only
  part of the set reaches is offered and refused with a 400 naming the row.
  Three reserved names
  are not taken
  out of the tag namespace: a tag called `system` keeps its tag rank and the
  table shows the name twice. Refusals follow `/command`'s: no ids is a 400, an
  unknown id is named in `unknown` and left out. Needs a loaded store (503 while
  indexing); read-only, so POST is 405. EVERY `ids`/`id` occurrence is read, so
  `?ids=a&ids=b` = `?ids=a,b`; the repeated form is what an id CONTAINING a
  comma owes (the fallback row id is `path#ordinal`, and the split runs after
  percent-decoding), and it is what the shell writes.
- `GET /tags?ids=A,B` is the tags popup's source of truth:
  `{rows: [{id, tags}], vocabulary: […], counts: {tag: n}, unknown: […]}` —
  `rows` in the order the ids were named, each row's tags folded through
  `tagsOfCell`, and `vocabulary` the whole store's (`storeTags`). PER ROW rather
  than as one union, because the client needs WHICH rows lack a tag: an add
  writes the rows that do not carry it and no others. The union, its coverage
  counts and their order are the popup's, computed off this. `counts` is how many
  ROWS the store holds under each tag, counted per request over `storeRecords`
  because `stTags` counts FILES and no arithmetic recovers a row count from that;
  a row counts once per tag however often its file spells one. Refusals follow
  `/keywords`'.
- `GET /links?id=ROW` is where a row points: `{digest, links: [{target, desc,
  type, span}]}`, out of the row's SUBTREE, in order of appearance and one entry
  per (target, shown) PAIR — a repeat under the same description keeps the
  FIRST occurrence and its span, so an edit through it edits the first
  spelling; the same target under another description is its own entry (keyed
  by target alone, `pnl` swallowed `alpha:grafana` and the elisp link read as
  unparsed). The rule is the DISPLAY rule — `Glance.Query.linkAt` is the grammar
  `displayText` reads a cell with and `linkShown` the display rule over it, so a
  bracket link is described by its `DESC` and by its target where it has none —
  plus bare `http(s)`/`mailto:` URLs,
  which describe themselves: a WORD, opening at a non-word boundary, with
  trailing `.,;:!?'"()[]{}<>` off the tail. One left-to-right pass over the
  bracket links, so `[[https://x][y]]` never also reports its target as a bare
  URL. Server-side because the page holds no org parser and must not grow one.
  404 on an unknown id, 400 with none, 503 while indexing, 405 on POST.
- The answer is WRITEABLE, which is what `span` and `digest` are for. `span` is
  the half-open CHAR range the link occupies in the FILE (`subtreeLinks` shifts
  the subtree scan's spans by where the slice starts) and `edit-link` takes that
  range back; `digest` is the file's as the store holds it, and pinning it
  (`digests` on `/command`) is what refuses a range the STORE has re-read since —
  a file that moved on DISK is already refused by `replaceSpans` with no pin. ONE
  scanner answers all three questions asked of a bracket link: what it SHOWS
  (`showLinks`, through `linkShown`), where it POINTS (`orgLinks`), and where it
  SITS (the span, shifted into document offsets by `subtreeLinks`).
- `edit-link` PRESERVES THE FORM: `[[T][D]]` keeps its description under a
  target-only edit, `[[T]]` stays desc-less, a plain URL swaps its target and
  stays plain, and a description ARRIVING brackets a plain URL — the one thing
  that changes a shape, a plain URL having nowhere to write one. Absent `desc`
  leaves the author's, `null` takes it off, and a description that shows nothing
  is the null spelled another way (`[[T][]]` shows its target) — the emptiness
  test strips and the value is written verbatim, content being nobody's to trim.
  TWO WALLS, both
  400: the span must sit inside the ROW's own subtree and cover exactly one link
  edge to edge, and the REPLACEMENT must read back as THE LINK IT CLAIMS TO BE —
  reparse and compare, since `a][b` renders `[[a][b]]`, which is one link
  pointing somewhere the request never named. A NEWLINE in either half is refused
  ahead of both walls: this scanner has no line rule, so the link reads back as
  itself and lands a column-1 star the ORG parser reads as a new headline.
  `Data.Org.Edit` is content-agnostic by law, so this is the layer that owes all
  three checks.
- `POST /headline` caps the body at 1 MiB and answers 413 past it. The cap is
  checked before the id lookup, so 413 outranks 404. `focusIn` extends that
  chain at the other end: a malformed `?child=` is a 400 raised BEFORE the id is
  looked up, so a bad child index over an unknown id answers 400 rather than
  404.
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
  page; a page slices the EFFECTIVE chain's order, never walk order; no `limit`
  means the whole set in walk order for the client to sort by the chain the view
  declares. The palette stays the store's whatever the page holds, and the shell
  re-asks the server for a row frame that lands while a filter is on.
- THE ORDER IS THE GRAMMAR'S. `sort:COL` / `sort:COL:desc` are query tokens
  (SCHEMA.md, Filter query): written order is precedence, repeats compose the
  chain, and the token NARROWS NOTHING — the one key that is no predicate.
  `Glance.Web.Sort.sortChainIn` reads them off `Glance.Web.Filter`'s own
  `parseFilter`, so the two modules split ONE parse and no token can be a
  predicate for one and an ordering for the other; `Filter` knows the key
  (`sortKey`) precisely so a sort token is never read as free text, and
  `compile` DROPS the term above the negation inverter — a match-all under it
  would make `-sort:x` the query that empties the table. A query naming any sort key REPLACES
  the chain it was asked under, one naming none leaves it standing, so the
  DEFAULT CHAIN IS INVISIBLE — tokens appear only on divergence, the same
  pattern as the default view's injection. `sort:*none*` is the EMPTY CHAIN and
  the query's whole vocabulary for document order: a starred meta, so no column
  is called it and no cell holds it, and it ADMITS NO COMPANIONS — a sort key
  beside it (or a direction on it) is a 400 naming the meta, since a reader who
  wrote both meant one of them. The half-typed `sort:` is no companion, naming
  nothing either way. `?order=` was the older half and is GONE: present at all it
  is a 400 naming its replacement, because a parameter silently ignored would
  serve the default order and look exactly like a working request.
- `->` CHAINS ONE TOKEN'S COLUMNS and is SUGAR: `sort:state->title:desc` parses
  to exactly the chain `sort:state sort:title:desc` composes. ONE semantics —
  `segmentsOf` splits the value and `nameOf` reads each segment where a whole
  token's value was read, so the segments and the tokens are one sequence and no
  rule below knows which spelling it came from. Written order is still
  precedence, first-wins dedup spans the segments AND the token boundaries, a
  refusal is the SEGMENT's and refuses the whole request naming the token as
  written, `sort:COL->` is a segment half typed and is the `key:` rule, and
  `*none*` admits no companion wherever in a chain it stands. NEGATION is the one
  rule that stays the token's, because the `-` is written before the key: it
  covers every segment rather than the first. The renderer's canonical form is
  that one token with `:asc` unwritten, and its chip door folds every sort token
  of an applied query into it — so the query this producer is asked carries one
  arrow-form token, and the shell never spells the `*none*`-with-companion the
  400 exists for.
- ONE COLUMN, ONE DIRECTION: a negation, an alternation, a column no view
  carries, a direction that is neither `asc` nor `desc`, and a column named
  twice are each the whole request's 400 naming the token. `sort:` half-typed
  orders nothing and refuses nothing (the `key:` rule). The renderer cannot
  refuse, so it DROPS the key and the token goes on narrowing nothing: a
  divergence in the loud direction, and the only one where the producer is
  stricter rather than wider. SCHEMA.md blesses it for four of the five and
  says the twice-named column is "no error on either side", which this side has
  refused since the tokens landed: a live disagreement with the contract, in the
  same loud direction, and one of the two documents owes an edit.
- The view declares a SORT CHAIN and is served in it: `defaultSortChain` is
  state, title, deadline, scheduled, every key ascending — state by the badge
  PALETTE, which is the declared `#+TODO:` cycle, so the table opens in org's
  own order rather than alphabetically. Priority left the chain: a fifth key
  behind four that have already separated nearly every pair of rows, reachable
  as `sort:priority`. The chain is ONE list read twice — `declaredSort` spells
  the EFFECTIVE chain onto the wire (SCHEMA.md's `sort` array),
  `sortedForViewWith` arranges the rows by that same chain — and a declaration
  disagreeing with the rows is one a renderer re-sorts out from under the
  reader. The empty chain is walk order AND no `sort` field, one function for
  both since a fold over no keys is the identity. The arrangement is the
  renderers' rules: empty cells last per key and OUTSIDE its direction, the
  state column by palette position with unlisted keywords tying at the back,
  `sortBy` stable so a full tie keeps walk order, text compared case-FOLDED as
  the nearest thing to `localeCompare`. `sortedForView` derives the palette from
  the records it is handed, which is right for ordering them and can differ from
  the store's where two files declare the same keywords in opposite orders;
  `/headlines` passes the store's.
- `?q=` is SCHEMA.md's filter query, parsed in `Glance.Web.Filter` as a port of
  `table-view.js`'s `scanQuery`/`parseQuery`/`tokenTest` — parity is the
  contract. Tokens split on whitespace and `&`; `key:value` (`=` alias) is a
  predicate only for a column key, `planned` or `ref`, so `:work:`, `=code=` and
  `course:x` stay text; a token opening with `"` is free text; `-` negates. One
  resolution decides both halves of that (`fieldOf` answering `Nothing`), so the
  grammar and the matcher cannot disagree about a token. COMBINATION IS ONE
  RULE: TOKENS AND, ALTERNATIVES OR. Every token narrows, whether or not another
  names its key — `state:TODO state:DONE` asks a one-value cell for two values,
  which is no row, and `tag:a tag:b`/`ref:a ref:b` carry or point at both. A row
  matching EITHER is the one token `state:TODO|DONE`: a predicate's VALUE splits
  on `|` (`alternatives`) and each alternative is read as that key's own value,
  the results OR'd, uniform over every key and value kind (metas included,
  `state:*active*|DONE`). A negation covers the whole token, so `-tag:a|b`
  carries neither. Empty alternatives are DROPPED (`a|` is `a`), and a value left
  with none narrows nothing, which is the `key:` rule — one answer for `key:`,
  `key:|` and `key:||`. The bar is a PREDICATE's: free text is the text it
  spells, bar and all, and a predicate's value has had its quotes taken out by
  the scanner, so a literal bar is free text's alone. `namesArchive` reads the
  alternatives too, so `tag:*archive*|web` still turns the exclusion off.
  `Glance.Web.Filter` dispatches on the KEY NAME, never on
  the column's declared `kind` — it does not import it: `state` is whole-value
  case-insensitive plus the `*active*`/`*inactive*` meta values (matched in
  their STARRED spelling alone, so `state:active` is the literal keyword
  `ACTIVE` and `state:*TODO*` is a literal that matches nothing; and `*active*`
  ORs in the EMPTY cell, where `*inactive*` does not), `priority` is exact
  equality, `scheduled`/`deadline` are prefix, everything else is substring.
  `key:*empty*` is the empty cell on EVERY key — `tag:*empty*` is untagged — a
  starred word on the `tag` column is that WHOLE tag (`tag:*archive*` where
  `tag:archive` is a substring), and `key:` narrows nothing. AN ORG TAG NAMES NO KEY: `course:text` is free text, colon and all,
  and `tag:course text` is the one spelling, the predicate reading the tags cell
  and the free text reading the row. Two consequences are the price: `tag:` is a
  SUBSTRING of the cell where a tag key was whole-tag (`tag:glan` finds
  `:glance:`), and org spells a tags cell `:web:`, so the free text `web:` is
  still inside every row carrying the tag. What it buys is the vocabulary
  divergence: the keys were the whole store's tags here and the loaded rows'
  tags in `table-view.js`, so one token meant two things across the wire.
  `planned` is one of the two keys that are not columns: a row is planned when
  its `scheduled` OR `deadline` cell holds anything, so `planned:*empty*` is
  neither and `-planned:*empty*` is the agenda's half. It takes a date PREFIX asked
  of both cells at once. Renderer-decidable off the same two cells — no keyword set, no
  vocabulary, no clock. It is no matcher of its own: a predicate reads the CELLS
  its key names (`fieldCells` — one for a column, the two date columns for
  `planned`), `*empty*` is every named cell empty and a value is any of them
  passing, so the virtual key is the column rule over a SET. The whole-tag meta
  stays keyed by the cell's INDEX, which is why `planned` can never reach it.
  The renderer's half is in the vendored
  `assets/table-view.js` (synced 2026-08-02, at table-view's starred-meta cut),
  and a skew there costs nothing anyway — `onFilter` means the renderer narrows
  nothing.
  `ref:ROWID` is the other key that is not a column, and the one a row cannot
  answer alone: it is every row whose subtree POINTS AT the row named, resolved
  through the store's own id-resolved rows (`storeEnv`, exact-string like
  `resolveIds`). Matched against `hrLinks`, over `refSpellings` of the target —
  its `ORG_GLANCE_ID` where it has one, plus its title, which is what the
  `[[Title]]`/`[[*Title]]` forms resolve against. A row is NOT its own reference
  (org-glance's materialize footer writes a self-link, and a referrer list
  holding the row you came from holds one useless entry). An id no row claims
  matches nothing and does not 400 — it is a filter, so a stale `ref:` in a
  bookmarked URL opens an empty view. Its
  value is the ONE predicate value not folded: a row id is exact-string, and
  ~/sync carries ids spelled `Password-…`/`Pets-…` that a fold would put beyond
  reach. `FilterEnv` is what carries the store to the matcher, and `ref:` is now
  all it carries — `emptyEnv` for a caller with no rows behind it, where `ref:`
  still parses and matches nothing.
  The tags column's key is `tag`, singular (header stays `Tags`). A predicate
  reads one `\x1f` field of `hrSearch`, so per-cell matching and free text agree
  by construction.
- `hrLinks` is the per-row reference list, cut from the SUBTREE at load through
  the `/links` scanner (`orgLinks`, so the bracket grammar stays the one
  `displayText` holds) and `T.copy`-detached like `hrSearch`; `forceRecord`
  forces its SPINE beside its elements, since a strict field forces the
  outermost cons alone and a lazy tail would retain the document. What counts as
  a reference is `refTargetOf`, and the rule is the CENSUS of ~/sync's walked
  6291 files (2026-08-02) rather than what org permits: the id-bearing protocols
  `org-glance-visit:` (3867), `org-glance-open:` (568), `org-glance-material:`
  (28) and `id:` (0 — org's own, in the list because it is org's own), stripped
  to a case-preserved target; a leading `*` stripped (`[[*Title]]`, 4); and a
  bare target carrying neither `:` nor `/` (`[[Title]]`, 18 and nearly all of
  them bracketed prose). Deliberately NOT references, though both are common:
  `org-glance-overview:` (2726) names a TAG and `org-glance-state:` (880) names
  a keyword — of their 52 and 6 distinct targets, not one is an
  `ORG_GLANCE_ID`. `file:`/`http`/`mailto` are dropped, which is what keeps the
  field small. Store residency over ~/sync did not move outside GC sampling
  noise: 348.0 MB before, 330.8 and 322.5 MB after (`serve` + `+RTS -s` at
  `-N8`, max residency being sampled at major GCs). Both after-samples sit BELOW
  the single before-sample, so the honest reading is no measurable cost — the
  field holds ~4.5k short targets over 10433 rows, under a megabyte by
  construction. `scan` is unaffected either way: it is a parser oracle off
  `orgParse` and builds no records, so the scan budget (~37.8 MB at `-N8`) does
  not move and this is NOT the number to quote for `hrLinks`.
- `hrLinked` is the same scan's wider answer — does the subtree hold ANY link —
  and it is what the wire carries: `rowJSON` emits `"linked": true` and nothing
  at all when it is false, sparse so a row with nowhere to go is the row it was
  before the field existed (SCHEMA.md's Row, additive). The renderer underlines
  that row's `title` cell, ink unchanged. It is the WIDE field on purpose: ~/sync
  at 2026-08-02 has 4976 linked rows against 1824 referencing ones, so marking
  off `hrLinks` would leave 3152 rows `o` opens unmarked. Every reference is a
  link, so nothing underlined answers `/links` empty.
- KNOWN LIMIT of `ref:`, inherited from the `/links` grammar rather than
  introduced: a link nested inside another link's DESCRIPTION yields no
  reference at either end. The outer link fails to close (its description breaks
  at the inner link's first `]`, leaving `][` where `linkAt` wants `]]`), and
  the rescan picks the inner one up one bracket late, so its target arrives
  spelled `[org-…` and is refused for the leading bracket. org-glance's own
  "Referred from" footer writes exactly this shape. Measured cost on the corpus:
  for the most-referenced contact in ~/sync, 126 of 128 files holding the link
  answered (2 of those archived and hidden by default), and the 2 misses are
  both this. Reused on purpose — a second scanner would be a second grammar to
  keep in step with SCHEMA.md's link rule.
- Parity discipline: there is NO schema revision mechanism between this producer
  and `table-view.js`. Agreement rests on the port being kept term for term,
  plus one loose runtime tripwire. Known divergences, all live:
  - `sort:` REFUSALS are the producer's alone, and it is the one divergence
    where this side is STRICTER: a negated, alternated, unknown-column,
    bad-direction or twice-named sort token is a 400 here and a dropped key
    there, so the renderer answers the rows in another order where this answers
    nothing. Deliberate — an order nobody can give is worth saying, and the
    query the shell writes never spells one, promotion composing the chain from
    the columns the view carries and the chip door folding what a reader typed.
    What is TERM FOR TERM is the reading: one column, one direction, `:`
    splitting a segment, `->` splitting the value, written order the precedence,
    `sort:` and `sort:COL->` half-typed narrowing and ordering nothing, a
    negation covering every segment, and the token narrowing nothing in either
    polarity (`Glance.Web.Sort.sortChainIn`'s `segmentsOf`/`nameOf` against
    `sortSegments`/`sortKeyOf`). `fixtures/parity/sort-tokens.json` runs the
    shared half over the browser renderer; `TestFilter`'s "Sort tokens" runs the
    same table here.
  - PRIORITY WEARS ORG'S BRACKETS, and the fold is the divergence. The cell is
    `[#A]`, the column is a BADGE column, and its three badges are danneskjold's
    org-priority hues (`[#A]` #E74C3C, `[#B]` #FFCC00, `[#C]` #27AE60 — three and
    no more; a `[#D]` takes the badge-less default ink). DISPLAY WEARS THE
    DECORATION AND MATCHING READS THROUGH IT: `Glance.Query.priorityLetter`
    strips `[#`…`]` and folds, so `priority:A` and `priority:[#A]` are one query
    on this side and `sortCell` orders by the LETTER. The renderer's own
    `tokenTest` priority arm does NOT fold, so a locally-filtered page answers
    `priority:A` with nothing — a handoff rather than a rule, and the narrower
    direction, which is the tripwire's blessed one. The `title` column's header
    moved `Headline` → `Title` in the same change; the key was already `title`,
    so nothing but the drawn word and the `^` echo reads it. The `priority`
    column's header moved `Pri` → `#` the same way, on the same rule: org's own
    glyph, and the header stops driving a column wider than `[#A]`. The `^`
    echo reads `# ▲`, which is the header doing its job.
  - COLUMN ORDER: `state | priority | title | scheduled | deadline | tag`. Tags
    are LAST because org writes them flush right on a headline. The reorder is
    the one-list edit — `columns`, `rowJSON`'s cells, `filterKeys` and
    `viewCells`→`hrSearch` all follow `viewColumns`, and `tagsColumn` is the
    INDEX of `tag` computed by NAME, so it followed too. What did not follow, by
    design, is `TestFilter`'s hardcoded layout oracle: it is moved by hand,
    which is the whole reason it exists.
  - Column lockstep is FOUR-way through `viewColumns` — `columns` declares them,
    `rowJSON` fills them, `filterKeys` names them, and `viewCells` joins them
    into `hrSearch`, `recordOf` tying the record through its own cells. A cell
    is `HeadlineRecord -> Maybe Text`: `Nothing` is the row JSON's `null` and the
    empty field a filter reads. `TestFilter`'s layout guard keeps its hardcoded
    six-cell list ON PURPOSE — it is the one copy of the layout NOT derived from
    the table, so it is an INDEPENDENT ORACLE rather than a mirror, and a guard
    derived from `viewColumns` would agree with any reordering of it. What used
    to go green was the APPEND: a seventh column left the hand-written search
    list six fields long and every predicate past it read the wrong field. A
    REORDER was already caught, by the predicate cases (`TestFilter` 622-645)
    reading actual cells. The append is closed by construction now, plus a case
    quantified over the columns there are ("every column is reachable by the key
    it declares"). What still moves by hand is `Filter.dateKeys` and
    `Filter.keyTest`'s name switch, neither of which is positional.
  - Which column holds a LIST is chosen by NAME here (`tagsColumn` = the index
    of `tag`) and DECLARED to the renderer: the `tag` column emits
    `"multi": true`, which beats its sampling (`multiColumn` over ≤40 non-empty
    cells, needing ≥2 tag-shaped and none contrary — fewer than two tagged rows
    loaded, or one cell holding a stray colon, and it found no multi-valued
    column at all). What rides on it shrank when the arity rule died: the
    whole-tag meta (`tag:*archive*`), the chip rendering and the value domain,
    where the combination of two `tag:` tokens once did too. An asset predating
    the field still samples.
  - Date-ness is likewise asymmetric: two hardcoded names here, sampled
    date-shape there. A page with under two dated rows makes the renderer
    substring-match `scheduled:` where the server prefix-matches it — and, since
    `planned` reads WHICH columns are dates, the same page answers `planned:` on
    the renderer's side over no columns at all, so `planned:*empty*` is every row
    there and `-planned:*empty*` is none of them. The predicate itself is
    term-for-term; the column set under it is not.
  - `ref:ROWID` is producer-only WHOLE, unlike the starred metas, which the
    renderer at least matches as literal text. It is undecidable from the rows a
    renderer holds — resolving a reference needs the target row's
    `ORG_GLANCE_ID` and title, which the store has and a page does not — so
    `table-view.js` has no branch and reads the token as FREE TEXT: a substring
    hunt for `ref:rowid` over the row's display text, which almost nothing
    matches. The renderer is therefore NARROWER, which is the tripwire's blessed
    direction (it fires only on a server zero) and leaves that direction
    unmoved. What keeps this workable is that no locally-filtered path applies
    `ref:` — the shell mounts with `onFilter`, so the server narrows, and a
    socket frame arriving under a filter refetches rather than splicing.
  - `state:*active*`/`state:*inactive*` are producer-only in their KEYWORD half
    alone, blessed by SCHEMA.md, and are the canonical spelling (org-glance's
    own, and what the default view boots on). The renderer has no group logic
    and matches them as literal badge text, EXCEPT for the starred `*active*`'s
    empty-cell term, which names no keyword: SCHEMA now puts the empty cell in
    the active group and `tokenTest` answers that half, so a locally-filtered
    `state:*active*` finds the stateless rows where it used to find nothing —
    still a subset of what the server answers, so the skew's direction is
    unmoved. `*inactive*` has no such term and stays a literal. The bare
    `state:active` is a literal on BOTH sides — matching reads the stars
    everywhere, and the star-blind reading is the renderer's COMPLETION alone.
    The `state` column ships the two as `values` beside
    its `badges`, so its autocomplete can at least offer them — dimmed and
    uncounted, since those counts are per cell value and a fraction of the
    server's answer is no better a number than zero. Each badge also
    names its `group` (`active`/`inactive`) — order cannot say where a `#+TODO:`
    bar fell and the hues are not a contract. Additive; a renderer ignores the
    field. The value palette reads the badges for their HUES alone; its own
    active/inactive split is `/keywords`'.
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
  and one `<script src>` the asset route answers out of the binary. No CDN, no
  web font, no analytics. The JetBrains Mono `@font-face` appears only when an
  `--assets` directory holds the file, pointing at a bare name this server
  serves.
- The renderer is COMPILED IN: `embeddedRenderer` = a TH splice over the
  committed `assets/table-view.js`, so the binary is the whole deployment.
  `--assets` REPLACES it (dev flag, live renderer hacking) — the named directory
  is then the whole asset set, which is what keeps `assetsMissing` reachable and
  makes it reachable under that flag alone. `assetSource` is where the two meet;
  both leave `asset` by one door, so content type and gzip are identical. No
  font is embedded. `make sync-renderer` copies from `../table-view/web`, prints
  the diff summary, and says so honestly when there is no sibling checkout.
- NO SOURCE FILE names an absolute path outside the repo. `TestSelfContained`
  sweeps every `.hs` under `src*/` and `app/` for `/home/`, and asserts what it
  swept first so an empty sweep cannot pass.
- The shell is vanilla JS with no framework or dependency — a real file,
  `assets/glue.js`, compiled into the binary the way the renderer is
  (docs/proposal-glue-extraction.md); the page inlines two JSON blobs (keymap,
  the `cfg` configuration the script reads as `CFG`) and the theme boot line,
  and names its two scripts in src tags. `--assets` replaces the whole set,
  which is live glue hacking with no rebuild. The shell has no build step —
  `cabal build` was always the build —
  and shrinking it beats adding to it. The BOOT — and only the boot — asks
  `?limit=100` and pulls the rest in behind the painted table; it
  mounts with `onFilter` so the server narrows, and
  opens its socket with `?bootstrap=off`. With no `q` in the URL the boot query
  is `state:*active*` — the default view, applied as a real query: written into
  the URL through `remember`, mounted as `initialQuery`, and asked of the server,
  so `DEL` strips it like any other token. A `q` that IS in the URL is the
  reader's intent, an empty one included, and nothing is injected over it. Rows are virtualized and shown a page
  at a time (`pageSize` = the boot's `limit`, so the first paint is page one),
  so a row step is `selectStep(±1)` — the page boundary is the renderer's,
  since only it knows there is one — and `[`/`]` turn a page, echoing where
  they landed. `getVisible()` is that page, so the buffer-ends keys reach its
  ends — and PROGRESSIVELY: `<`/`>` take the page's end row, and pressed again
  on it turn a page and land on the same end of that one (`endStop`), stopping
  at page one's first row and the last page's last. Each climb re-selects,
  since the renderer lands a turn at the end it arrives at — the opposite one
  in both directions — and the column comes back out of `column()` rather than
  a local. An asset with no pager keeps the within-page half.
  Ids out of `getVisible()` handed to `select(id, col)` are all that is left of the
  DOM-walking path over the TABLE, which is gone, as are the frame branches
  `bootstrap=off` makes unreachable; the panel's edit overlay reads one row's
  BOX and nothing else. The column is the renderer's selection, never a second
  copy here: `selectStep` carries it, and what the shell passes back is
  whatever `getSelection()` reports, so it survives a profile switch and clears
  when the selection does. Cell movement (`f`/`b`, `l`/`h`) walks OFF the cells
  rather than bumping: the renderer reads a column index outside the table as no
  column at all (`cellCol`), so `moveCol` hands the out-of-range step straight
  to `select(id, want)` and the answer is the whole-row look, echoed `row mode`.
  The clamp this page used to keep — `at first`/`at last`, returning before the
  select — swallowed the key at a wall the renderer does not have, and the glue
  guard now forbids those strings. Re-entry is unchanged (`at === null ? 0 : at
  + step`), and the landing column is read back out of `column()` rather than
  off `want`, since the renderer's answer decides. The applied `?q=` is restored the
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
- A VIEW SWAPS ON ITS ANSWER. The table on screen stands until the new rows are
  in hand and then goes in ONE mount — the count handed to the renderer never
  passes through zero or through a partial set unless the answer is empty. That
  decides the fetch: a boot (`!table`) takes `?limit=100` plus the pull behind
  it, a re-application (`g`, `a`, `@`, pop, a `view-changed` remount) asks for
  the WHOLE answer once, since a page-sized mount there replaced a complete
  table with a hundred rows and reflowed the pager and the hint a moment later.
- THE STALE WASH: one mechanism, two triggers, one clear discipline. One class
  (`stale`) on the document element dims `#app` and the whole modal band
  (`#modal`, `#prompt`, `#config`) to `opacity:.55`, eased 180 ms. One property,
  and never a `filter` of any kind: a filter makes its element the containing
  block for `position:fixed` descendants, and the renderer's palette backdrop
  (`.tv-veil`, inside `#app`) is one — it would stop covering the viewport and be
  clipped by `.tv-root`'s `overflow:hidden`. No blur either: a stale row is still
  the row. The log strip and the key line are exempt by omission: they explain
  the state. Triggers:
  a view fetch in flight past 300 ms, and a socket down past 400 ms; the delays
  are what keep a working page undimmed. One holder (`wash`) carries a count, a
  timer and an on-flag per reason with one `arm`/`off`/`show`; the view reason is
  STEPPED (an abort overlaps the fetch that replaced it) and the socket's is SET
  (a refused connection closes without ever opening). `viewing` marks the fetches
  whose answer replaces the rows — the parity baseline and `@`'s probe are not
  among them, and a boot holds nothing. The page never reads the class back.
- Shell z-indexes are three: echo `2`, modal backdrop `100`, sheet `101`. `3`
  was the corner's and went with it — the suite forbids the value. Every overlay
  shares that pair with the sheet
  (`#modal,#prompt,#config,#links,#tags` and `#pbox,#lbox,#tbox`), so the three
  values stand whatever else is added. The cross-repo constraint is the backdrop pair clearing the renderer's
  sticky header (`1`) and completion list (`5`); the echo sits
  below both on purpose, so it dims under the backdrop. The filter palette
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
  `pageInfo`, `getSelection`, `openFilter`, `sortPromote`, plus `matchMedia`.
  `initialQuery` is passed unguarded and detected afterwards by asking
  `getQuery()` whether it took. An asset with no `sortPromote` costs the ORDER
  alone: the query still carries its `sort:` tokens and the server still answers
  in them.
- The page never scrolls: `body` is `100vh`, `overflow:hidden`, a flex column of
  table, log and key line. The log grows to its capped share and scrolls inside
  itself, the table takes whatever it gives up (`#app` is `flex:1 1 auto`), the
  key line is `flex:none` and scrolls sideways. A long message therefore moves
  nothing.
- The log strip is append-only and its whole interface is
  `append(scope, severity, message)`. A line is `HH:MM:SS SEV scope message` —
  severity `info`/`warn`/`error`, coloured, SPELLED uppercase (`INFO`/`WARN`/
  `ERROR`) and WORN lowercase as the line's class, the upcase happening at the
  one place the word is drawn; scope
  one of `ws`, `sync`, `cmd`, `filter`, `config`, `boot`; control characters in
  the message collapse to spaces. Nothing clears it, the boot line included; the
  ring holds 500 (`LOGCAP`, lines KEPT — a different limit from the height cap
  below) and drops the OLDEST; a line identical to the one before it
  bumps a `×N` counter instead of appending, which is the only mutation. The end
  is scrolled to unless the reader has scrolled up. Every write names its rows —
  `headline "TITLE" marked for deletion` / `unmarked for deletion` / `archived` /
  `→ KEYWORD` / `state cleared`, one line per ROW — with the title read through
  the renderer's `displayText` and the id as the fallback; refusals stay one
  `cmd error` line.
- The log's HEIGHT is the page's second `localStorage` preference, under
  `glance-log` beside `glance-theme`, applied on boot and on every accepted
  keystroke. The stylesheet keeps the arithmetic and declares the default
  (`#log{--g-logn:7;max-height:calc(var(--g-logn) * 1.5em + 2 * 6px + 2 * 1px)}`)
  and the knob writes a NUMBER onto the element
  (`style.setProperty("--g-logn", …)`), so the formula is in one place and a page
  whose glue never ran is capped at the same figure. `LOG = {key:"glance-log",
  def:7, min:1, max:50}`, mirrored in Haskell as `logLinesDefault`/`logLinesMin`/
  `logLinesMax` and `logLinesBand` — the constants the declared value is spelled
  from. Blank is the default (how a reader asks for it back) and REMOVES the key
  rather than storing `""`, a preference spelling the empty string being still a
  preference; a whole number in
  the band is that number, and everything else is DECLINED rather than clamped:
  the cap stands, nothing is stored, and reopening the sheet draws the preference
  back over a refused value. A stored value the band no longer takes falls back
  to the default, the boot reading it through the same `logLines`.
  The field is `#clog`, the GENERAL panel's third row,
  applied on `input` rather than `change` so it is a knob rather than a form; the
  panel says where a preference is READ, and `cmoved` never sees it, so it costs
  no request and cannot dirty a pristine sheet.
- Every touch-device rule lives in ONE `@media (pointer:coarse)` block — the
  chip row as a 44px tap target, its empty-state label, the sheet's stacked
  panes, and its 16px fields that stop iOS zooming in.
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
  keys alone, so `planned:` and `ref:` are treated as suspect. It reports a suspicion
  and corrects nothing. Its baseline is a remembered unfiltered paint, and a
  boot that had none — a `?q=` link, or the default view — arms it with `arm`'s
  own unfiltered fetch and re-runs the check the boot could not.
- The shell's keymap is `Glance.Web.Keymap`'s `keyBindings` and nothing else — ONE
  map,
  no profiles: the page carries it as a JSON blob (`{rows, hints, reserved,
  once}`) and its own dispatch parses that blob. Each row carries `kbKeys`,
  `kbCommand`, `kbScope` (`table`, `modal` or `any` — where it is live) and an
  optional `kbHelp`; the dispatch filters on the scope and the echo widget reads
  the help. `seq` is derived in the blob (`T.unwords . kbKeys`), never stored.
  Movement carries BOTH spellings — `n`/`p` and `j`/`k` step a row, `f`/`b` and
  `l`/`h` step a cell — which costs a row each where a profile cost a selector, a
  stored choice, a URL parameter and a key line that had to be rewritten. The
  ARROWS ride both axes and SILENTLY: `<up>`/`<down>` step a row and
  `<left>`/`<right>` a cell, each behind its letters, so the key line — which
  shows a command's FIRST binding — still reads `n/p rows · f/b cells`. Same
  handler, so an arrow walks off the last cell into the whole-row look the way
  `f` does. Ends
  are `<` and `>`, plus vi's `G` beside `>`. `^` is `toggle-sort` — the cell
  selection's column, sortable-honoring — and it is a QUERY EDIT: the renderer
  composes the chain, writes it into the applied query as ONE arrow-form `sort:`
  token and delivers it, so the press arrives as an ordinary commit (URL,
  refetch, the server asked for the order it was just told about) and `DEL`
  takes the chip WHOLE — an order is one decision, taken off the way it went
  on, and a chip erasing by a different rule than its neighbours made `DEL` a
  thing to weigh (2026-08-05; the per-key peel is retired). What it composes onto is the chain IN FORCE, so the
  first press is where the declared chain becomes tokens and only the promoted
  key moves. This page keeps no record of an order and asks for none: `sortBy`
  is gone from the shell with the agenda's call, the canned view carrying
  `sort:scheduled` in its query instead.
  `g` is `apply-default-filter`, `P` is `set-default-view` (the pin: the
  applied query becomes the tree's default, ONCE), `a`
  is `org-glance-agenda`, `,`
  is `customize`, `:` is `org-agenda-set-tags` — the AGENDA's own key for the
  same question over there — `o` and `!` are `org-glance-overview:open`, `@` is
  `org-glance-overview:relations`, `M` is `mark-all`, `d` is
  `archive-flag` and `D` is `org-glance-overview:delete` (both over FLAGS, never
  marks). No sequence is bound
  twice or opens a longer one. Sequences and command names are org-glance's where
  org-glance has one; a row with no handler is recognized and says what will
  back it.
  A LETTER BINDING NAMES A PHYSICAL KEY, and the split is `keyName`'s alone —
  the one function every listener (dispatch, sheet, value palette, popups) names
  a press through, so all of them inherit it together. `e.code` matching
  `KeyA`–`KeyZ` answers as that letter, lowercase, and `shiftKey` as the
  UPPERCASE binding rather than an `S-` modifier, which is what keeps `d` and
  `D` two rows; a chord's second key comes through the same door, so `C-c C-t`
  completes on the physical `t`. Everything else is the CHARACTER `e.key`
  reports — the named keys, the function keys, and the PUNCTUATION (`^ : + < >
  [ ] / , ! @`), which sits at a different position on every layout and so has
  no position to bind. A press carrying no `code` falls back whole, which is
  what the suite's own events are. Consequences, both named: the map is
  QWERTY's POSITIONS, so a Latin layout that moves its letters (AZERTY, Dvorak)
  reads its own `a` as this map's `q`; and a layout spelling no `<` or `[` —
  the Russian one does not — cannot reach the punctuation half, the letters
  still carrying movement, marks, states and the archive.
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
  holds under both spellings of a command, plus `org-glance-overview:open` and
  `org-glance-agenda`, which write nothing and are ruinous held down — a tab per
  repeat, a remount per repeat. `archive-flag` needs it most: a repeat
  that survived would flag a row and archive it from ONE press, which is the
  confirmation the two-press shape exists to be.
- Seven keys write without a sheet, all `POST /command`, and WHICH ROWS is per
  command rather than one rule. `t`/`C-c C-t` (`set-state`), `:`
  (`org-agenda-set-tags`, which resolves the set once and then writes over it
  from a popup) and `C-c C-s`/`C-c
  C-d` (`set-planning`) take the MARKED set when there is one and the row at
  point otherwise — dired's rule, and the generic bulk selection. `D` and `d`
  take the FLAGGED set instead and never read marks: a mark is what a reader lays
  down to set a state over a run of rows, and letting the archive key inherit one
  makes every mark a loaded gun. `+` (`capture`) takes NO rows at all. Every set
  is the renderer's and is asked for AT command time; no set is kept here.
- `C-c C-s` and `C-c C-d` raise the value palette in its TEXT mode
  (`askText`): the same overlay, the same band, the same `unask` and the same
  ESC through `cancel`, with `prompting.text` set — no list, no letters, RET
  commits the line as typed. The two chords
  send the line as `date`, and an EMPTY line is the null that clears the entry.
  Both
  chords reach the page where `C-c C-t` does not: `Ctrl+S` and `Ctrl+D` are page
  default actions rather than chrome shortcuts, so `preventDefault` on the
  completing chord is the whole of what they need.
- `+` IS ONE FORM (`#capture`/`#kbox`, its own `SURFACES` entry between the
  palette and the link popup), where a chain of palettes used to blink: the tag
  field with the tree's vocabulary narrowing under it (substring over the
  folded spelling, at most eight shown, `C-n`/`C-p` and the vertical arrows
  walking a highlight RET takes; no highlight commits the field as typed, so a
  name of the tree's own is reachable and the charset wall stays the server's),
  then one field per `%^{PROMPT}` grown IN PLACE when the tag settles (RET or
  TAB out of its field — only the server knows the prompts, and editing the tag
  afterwards clears the grown fields), then the line. RET moves the focus
  forward and at the line captures; an EMPTY tag settles to the untagged inbox
  path exactly as it was; ESC anywhere is the keymap's `cancel` through
  `SURFACES` and closes the form with nothing sent. A refusal — the server's,
  or the empty line's — keeps the form UP with everything typed: `shutCapture`
  runs on the 200 alone, so fixing a line is an edit rather than a retype. This
  page still holds no template grammar; what it grows is what `/capture?tag=`
  said to ask. The form's keys are a document listener behind the dispatch,
  gated on the focused field, so the harness reaches them the way a reader
  does.
- A CAPTURE SAYS WHERE POINT IS OWED, and `arriving`/`arrived()` is `leaving`'s
  mirror: the answer names the row the write made, and the same three doors that
  spend the archive's anchor spend this one. It is `land`'s ordinary rule asked
  ONLY where there is something to land on — a filter that hides the new row, a
  page it is not on, or a watch step that has not delivered it leave point where
  it stands, since `land` falls through to an INDEX and there is no honest index
  here. Both are dropped by a commit and by a remount: an anchor belongs to its
  view. KNOWN LIMIT, inherited: it is spent at the FIRST door, so an unrelated
  watch step landing between the capture's 200 and the delivery spends it.
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
- ONE `d`/`D`/`u` GESTURE OVER THREE SURFACES — the table, the property panel and
  the tags popup — and `flagKey` is the whole of it: the cursor read, the
  two-press rule, the set-or-row choice, the spending of the flags before the
  take, the feature refusal and the walk after `u`. A surface DECLARES a shape:
  its mount, its cursor as an id, what "take these" means, what it LOGS when a
  flag moves, its walk, and four PHRASES (missing, none, flag, unflag). WHO
  SPEAKS belongs to the caller rather than to the shape — the popups say
  `KEY → phrase` out of
  a listener holding no binding, the table says it through `said`, which spells
  the binding's own command name and brackets the phrase — so a phrase is the
  whole line on one surface and the bracket on another, and `d` and `D` echo
  `archive-flag` and `org-glance-overview:delete` out of one gesture.
  The table's shape is a FUNCTION of the binding for exactly that reason. The
  CURSOR is asked for before the FLAGS: `D` means "take these" and a lone row is
  a set of one, so it lands on a mount whose renderer never had flags, while the
  two presses that MOVE a flag are what the refusal is for. `HOW` words the pill
  and is a function of what LANDED — the popups' takes are local and total and
  call it with the size of the set; the table's is a write that can come back
  partly refused. `u`'s flag-before-mark stays in `mark`: over the popups `u` is
  the flag key, over the table it is the MARK key preferring a flag, and that
  asymmetry is the table's own — what it hands over is the key.
- `t`/`C-c C-t` raise a value palette of the shell's OWN, and what it shows is
  the RESOLVER'S TRUTH: `GET /keywords?ids=…` answers with the classification
  chain behind those rows, and the palette draws it as a table — Source |
  Active | Inactive, one row per source in precedence order (widest first, so
  `default` leads), `*empty*` spanning
  a row of its own at the foot — FOUR sources at most (`default`, `system`,
  tags, `file`), no `union` row, each drawn under the NAME it arrived under.
  The keywords are the server's, never the state
  column's `badges` (a superset that says nothing about where a keyword came
  from) and never its `values` (`*active*` is not a keyword); only the HUES are
  read off the badges, by value. What it shows IS what is settable — one chain
  behind both — for a single row; over a marked set a keyword only part of the
  set reaches is offered and refused. It
  is WHICH-KEY: every entry wears a letter and that letter commits on its own,
  since the palette IS the confirmation. No `RET` in letter mode, no confirm
  step; the drift lock is the safety. `/` falls back to the completing-read —
  the table FLATTENS, the token column goes, a field appears, typing narrows,
  `C-n`/`C-p` and the
  arrows walk, `RET` commits — and is entered, never left; `ESC` (the keymap's
  `cancel`) is the one door out of either mode. Both modes commit through one
  `takeChoice`. The overlay goes up on the KEYDOWN and the answer fills it (a
  `resolving…` line until then), so the raising guard, `typing()` and `ESC` are
  where they were; a fill landing after the reader left finds another prompt or
  none and drops, and a refusal closes the palette with a `cmd` error line.
  Its keys live in a SECOND document listener behind the dispatch. `typing()` —
  which the palette turns on with NO field focused, the way the property panel's
  nav does — kills every `table` row, so `n` moves nothing and `d` flags nothing
  while it is up; that is what holds the DISPATCH off. What holds the SHEET's own
  listener off is `momentary()`, which names the palette the moment it is up:
  the sheet's listener runs AHEAD of the dispatch, so `typing()` never gets the
  chance to answer for it. The pill counts what
  landed, the log names every row it landed on and every one refused, and the
  rows arrive over the watch. TWO GUARDS, one press each: `prompting.raising`
  declines the keydown that OPENED the palette (that listener is behind the
  dispatch, and `t` is both the opener and a letter), and `e.repeat` stops a HELD
  `t` committing through what it opened — `ONCE` cannot reach it, since it
  governs dispatch rows and the repeat lands while every row is dead.
- AN EDIT OVERLAY NAMES ITS CELLS BY KEY. A shape carries `cells: ["title",
  "url"]` (the link popup) or `cells: ["title"]` (the tags popup) beside the
  `cols` list the SERVER declared (`Glance.Query.linkColumns`, `tagColumns`), and
  `cellSpan` resolves the keys to the leftmost and rightmost indices the
  placement reads — a pure, order-only function, so a column list that moves
  takes the box with it and inserting a column ahead of the run costs nothing.
  The run is the COLUMNS' order rather than the shape's, a box being drawn edge
  to edge. An unknown key resolves to nothing and the placement is a NO-OP: the
  box stays where it was rather than covering the wrong cells. Replaced a
  positional pair with nothing tying it to the list it indexed. The property
  panel names no cells and takes the whole row; the gutter `flags: true` puts in
  front is skipped by the renderer's own class.
- THE MODAL SURFACES ARE ONE LIST, `SURFACES`, in the order they are written:
  the value palette (`prompt`), the link popup, the tags popup, then the sheet.
  The first three are `momentary`; the sheet is the floor under them. Each entry
  names its `up`, the `off` that closes it, and the OPEN EDIT that is a rung
  under it. FOUR READERS: `momentary()` names whichever momentary one is up,
  `typing()` asks whether ANY is up (which kills every `table` row), `sole()`
  closes every momentary one on a raise, and `cancel` walks it for the rung ESC
  belongs to. The five listeners STAY, and so does `prompting.raising` — one
  surface declining the single keydown that RAISED it, which no list can answer.
  ORDER IS LOAD-BEARING FOR EXACTLY ONE PAIR: `+` over the tags popup leaves both
  `prompt` and `tags` up, and `momentary()` resolves that tie by list position,
  so swapping them makes the tags listener eat the add-field's letters.
  Everywhere else the surfaces are mutually exclusive — each is raised from a
  `table` key, which `typing()` has already killed by the time another is up.
- `:` (`org-agenda-set-tags`) raises the TAGS POPUP, the page's FOURTH
  table-view mount (`#ttable`) and the only MUTABLE one. A tag over a set of rows
  is a RECORD — a name, a coverage, a weight in the tree — and a reader deciding
  whether to drop one READS those three, so it is the link popup's shape rather
  than the which-key palette's, and the letters went with the list. Columns are
  `Glance.Query.tagColumns`: `title` (the tag, keyed the link popup's way — a
  column keyed `tag` would invite the renderer's multi-value sampling), `on` (the
  coverage, `all` or `k/n`) and `rows` (`/tags`' store-wide count). Rows are the
  UNION over the target rows, FIRST-SEEN across the rows as named and, within a
  row, the order its file spells them — an alphabetical insert in the middle
  would move the row out from under the cursor, where an append cannot. A tag IS
  its row's id, so a flag, the cursor and a rename name the same thing after any
  number of writes. Raised LATE, behind the fetch, like the link popup: `:` is no
  key inside the list it opens, so no raising guard is owed; a set the store
  knows no row of is a refusal rather than an empty popup. Mounted with
  `marks: false` (the set a tag command runs over is the TABLE's, settled before
  this went up) and `flags: true`.
  The popup STAYS up under every write it carries, since managing tags is several
  ops over one set; every write refreshes the list from the command's OWN per-id
  answer, never a re-read — `/command` does not write the store, so asking
  `/tags` again would report what the files said BEFORE it. The `rows` count is
  stepped by what landed and corrected by the next resolution.
  `d`/`D`/`u` are dired's gesture verbatim: `d` flags, a second `d` on a flagged
  tag IS `D`, `u` unflags and walks on, `e.repeat` cannot flag and remove from
  one press. `D` removes EVERY flagged tag from every target CARRYING it — one
  `remove-tag` per tag, since a command names one — and SPENDS the flags. `+`
  raises the value palette straight into its field (`askFrom`) over the ADDABLE
  vocabulary: the tree's `vocabulary` LESS the tags every target already carries,
  the set's partial ones leading and wearing their `2/3`, and RET commits the
  highlighted entry or the line as typed (`freely`), so a tag the tree has never
  held is reachable and the charset wall is the SERVER's. `RET` is the RENAME,
  through the property panel's edit model over ONE cell: `#tedit` is laid over
  the tag cell (`td:not(.tv-box)`, the row's box through the mount's published
  root), RET commits `rename-tag {from, to}` over the targets carrying `from`,
  ESC restores. A tag is FOLDED at commit, since presence is.
  Its keys are a document listener behind the dispatch, with two guards about
  the palette `+` raises OVER it: it runs only while `momentary()` names it —
  which the palette's own entry, standing EARLIER in `SURFACES`, takes away the
  moment `+` raises one — and it declines a key that palette has already CLAIMED
  (`e.defaultPrevented`); without the second, the very RET that added a tag would
  land on a popup with no prompt on it and open the rename.
- `o`/`!` (`org-glance-overview:open`) FOLLOW the row, and the ANSWER decides the
  gesture: `GET /links?id=` for the row at point, then no links is an echo
  refusal, ONE is `window.open(target, "_blank", "noopener")`, and SEVERAL raise
  the popup. Every open writes a `cmd` line naming the target. WHICH rows have
  one is on screen ahead of the press: `linked` underlines the title, over every
  link `/links` would report rather than the ones a tab can take, so an
  underlined `mailto:` row still warns on commit.
  A tab can be pointed at `http`/`https` and NOTHING ELSE (`followable`): org
  writes `mailto:`, `file:`, `id:`, its own org-glance protocols and bare
  `[[Title]]` internal links, `/links` reports them all, and each names
  something a tab is not. A non-followable target is one `cmd` WARN line —
  `link type not implemented: TARGET`, truncated at 80 characters
  (`shortly`) — plus the same words in the echo, and no tab. The judgement lives
  in `openLink`, which is why it is ONE function rather than a filter over the
  rows: the popup still LISTS every link, since that is what teaches a
  reader what the entry holds, and the COMMIT is where the answer is given — so
  a lone `mailto:` warns without a popup, and a `mailto:` entry beside an
  `http` one warns while its neighbour still opens. SEVERAL raise the LINK
  POPUP, the page's THIRD table-view mount, raised LATE — the answer decides
  whether there IS one. It browses; `RET` is its one write.
- `RET` over the link popup EDITS the link at point in place: the title and url
  cells become fields over themselves (`LROW`, the shared overlay's third shape),
  `TAB` hops, `RET` commits
  `edit-link` over the SPAN `/links` handed out under the digest that answer
  carried, and `ESC` restores. The page holds no bracket grammar and no offsets
  of its own: it sends the range it was given and the two strings a reader typed.
  The untouched FIELD is what makes absent-not-null reachable — the description
  field opens on what the link SHOWS, which for a link with none of its own is
  its target, so a field left alone sends no `desc` and an emptied one sends the
  null. Both fields are TRIMMED on the way out, so `[[T][ D ]]` sends `D`: the
  padding is the field's, and the server writes a description verbatim and
  refuses a padded target outright. The popup CLOSES on the press, both outcomes
  alike, which is `o`'s own
  rule and is forced: the spans it holds describe a file the write has just
  moved, the store does not know yet, and a re-read here would answer with what
  the file said BEFORE the write (the tags popup's documented reason). KNOWN
  CONSEQUENCE: a row with exactly ONE link is followed rather than listed, so
  that link has no editor.
- `a` (`org-glance-agenda`) is a canned VIEW, not a mode: `state:*active*
  -planned:*empty* sort:scheduled` through `applyView`, the door `g` uses — URL,
  socket dropped, remount — so the query is the renderer's chips and `DEL` strips
  it like any other, the ORDER included. No agenda state anywhere; `g` is the way
  home. The order is a token rather than a call behind the answer, so the whole
  view is one string: the server answers page one in it and the renderer reads
  the chain off the same query, where a call could have stated an order the
  applied query did not. What still arrives through `landed` — a one-shot thunk
  `start` TAKES before it fetches, so a boot that never lands cannot leave it
  armed — is the ECHO, called with the SERVER's match count, which is the one
  number the first page cannot give.
- Letters are `whichKeys(labels)`: over the labels flattened in DRAW order —
  each source row's active cell then its inactive one, `*empty*` last — each
  entry
  takes the INDEX of the first letter of its OWN spelling, downcased, that no
  earlier entry claimed — one `a`–`z` pool, `-1` for none left, so `TODO DONE
  DELEGATED` = `t d e`. Pure and order-only, so a tree's cycle always yields the
  same letters, and `default` leading the draw is what gives `TODO` `t` and
  `DONE` `d` in every tree. One pool over the WHOLE table, so a letter is the reader's
  wherever in it the keyword sits, and the fallback narrows that same list.
  `*empty*` is OUT of the pool: it answers to `DEL` — a key that already means
  take-it-off wherever this page binds one — so the `a`–`z` namespace is spent on
  KEYWORDS alone and a cycle wide enough to run it dry keeps the letter the meta
  used to take. `offer` decides that by the entry carrying a key of its OWN
  (`fixed`) rather than by its being the meta. In the typing mode `DEL` is the
  field's and `*empty*` is reached by narrowing to it, like every other entry;
  in the tag palette, which has no clear, `DEL` reaches nobody.
  `setChoices` folds the letter into
  each
  entry once, so the drawing and the dispatch read ONE FIELD of one object
  instead of agreeing on a parallel array's indices — `shown` narrows and
  `choices` does not. Display teaches why, and there is NO key-token column:
  the claimed letter is marked INSIDE the keyword, BOLD and UNDERLINED at its
  position with the rule taking that state's own badge hue (inline per entry,
  since only the entry knows it) under a word already wearing that colour; the
  source is named down the muted first column, a hairline sits between
  source rows (each row's own top border, the table's border language), and
  `*empty*` comes
  last in the starred-meta italic. An entry that claimed nothing is drawn BARE
  — no slot, no dot — and is reachable through `/` alone. ONE entry keeps a
  token, and it is `*empty*`: `DEL` names no position in a word to mark. The
  tag palette wears the same language, its `/` and `+` being mode keys rather
  than entries.
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
- STARRED METAS, and the family is TOTAL: `*word*` marks a value with semantics
  of its own — never a literal keyword, never a cell value — and NO BARE WORD IS
  RESERVED anywhere, so every spelling a cell can hold is reachable as itself
  (`state:none` finds a keyword `NONE`, `tag:archive` a tag holding the letters).
  The family: `*empty*` (the empty cell, EVERY column key and `planned`,
  decided off the cell so both sides answer it alike), `*archive*` (the whole
  ARCHIVE tag on the `tag` column, and the one query that lifts `/headlines`'s
  exclusion), `*active*`/`*inactive*` (the filter's group metas,
  producer-evaluated), and `*none*` (the ORDER's empty chain, under `sort:`
  alone, which is the one meta that names no cell at all). The state palette's
  take-the-keyword-off entry is
  `*empty*` too — it takes the cell to what `state:*empty*` then finds — and it
  commits a null keyword, answering to `DEL` rather than to a pool letter.
  `*active*` is the file's active keywords PLUS the EMPTY state cell — a
  stateless entry is live work, and the default view is what would otherwise
  hide it — while `*inactive*` is stated keywords alone, so the two do not
  partition the column, `-state:*active*` drops the empty cell, and
  `state:*empty*` stays the explicit spelling and is a subset of `*active*`. The
  empty half is read off the CELL, which is the one term the renderer can answer
  for itself.
  A future meta joins by wearing the stars. The enforcing edge is
  `setStateEdits`, which refuses any word a file's `#+TODO:` does not declare, and
  `keywordTextP` (letters and underscores) makes a starred word undeclarable, so
  the two walls meet. On the tag side it is `isTagChar`, which has no `*`: no
  file can spell a tag `*archive*`, and `add-tag`, `remove-tag` and both ends of
  `rename-tag` refuse one.
- Browser writes are commands over the bridge: structured ones (toggle, retag,
  reschedule) and drift-locked raw replacement (materialize a subtree, later a
  file). Semantic org editing — refile, agenda logic — stays out of the browser.
  Automation = reviewed deterministic scripts, no LLM in the loop.

## UI

- MOVEMENT NEVER CHANGES CONTEXT. `n`/`p`, `f`/`b` and the grain relocate
  attention alone: they never open, never close, never commit, never cross a
  boundary a reader would have to come back out of. `RET` and `DEL` are the
  context axis — `RET` goes deeper (opens the edit, enters the child, raises the
  thing's own popup) and `DEL` comes back out (unmark, token, frame, the sheet
  ladder, close). A key that both moved and switched would make every press a
  risk to weigh; the split is what makes holding `n` safe anywhere on the page,
  and it is why movement keys are the ones left OUT of `ONCE`. Stated in full in
  `docs/design-rhymes.md`.
- Keyboard-first: every web-surface feature ships with a key path mirroring
  the Emacs org-glance maps; buttons only where keys cannot reach; the echo
  widget must know every new binding (keymap-is-data blob is the single
  source).
- Commands are named as elisp functions and the ECHO speaks them verbatim:
  `SEQ → command`, with anything else in brackets after it (`> → last-row (page
  2/129)`, `m → mark-toggle (marked · 2)`) — never the prose spelling, since the
  rebinding config to come will address a function by exactly this string. One
  helper emits the shape (`said(b, what)`); `run`'s default is the same with
  `kbHelp` after a `·`. The resident key line is the exception on purpose: its
  labels are curated prose (`rows`, `pages`) naming a group, not a command that
  ran.
- ONE BUTTONLESS SHEET, and there are two of them: the materialize sheet and the
  settings sheet run the SAME ladder, written once (`saveSheet`, `leaveSheet`,
  `note`) over a sheet object holding `{dirty, flush, refresh, shut, scope}`
  and its own state word. `activeSheet()` is what either key asks, and it is
  total because neither sheet opens over the other (`openSettings` refuses, and
  it asks `activeSheet()` too). What differs stays in the verbs: the subtree's
  flush is one `POST /headline`, the settings' is a POST per moved layer with a
  note on each refused row; the log lines are filed under the sheet's own scope
  (`sync` / `config`). Dirty = either pane vs the materialized original, moved by
  each successful flush; ESC or the backdrop flushes a dirty sheet and closes on
  the 200, a pristine one closes with no request; `C-x C-s` flushes mid-edit and
  chains the receipt's digest; a 409 keeps it open at `conflict`, where `C-x C-s`
  is `refresh()` then `flush()` — the digests the files carry NOW — and ESC
  discards; `beforeunload` flushes with `keepalive` only when dirty. Header
  states: `synced` / `syncing…` / `conflict` / `error` — the last two are the
  ones that wait for a keystroke, so each spells the key that clears it, and the
  retry line is one constant rather than three copies.
- TWO KEYS COMMIT AN OPEN ELEMENT in the material document: `C-x C-s`
  (`save-buffer`) and org's own `C-c C-c` (`org-ctrl-c-ctrl-c`), over the
  paragraph textarea and the two-field overlay alike, `RET` keeping its landed
  meanings. `C-x C-s` keeps the half that is a BUFFER's — with nothing open it
  flushes the sheet and on a conflict it overwrites — where `C-c C-c` stops
  where the element does and says `nothing open here`. `commitDocEdit` takes the
  binding that fired, so the echo names the command that ran. `Ctrl+C` reaches
  the page (a page default action rather than a chrome shortcut, like `Ctrl+S`),
  and COPY is untouched because prefix opening is guarded by `selecting()`: with
  anything selected the first press is the browser's, which is exactly when a
  reader means to copy. The resident key line is the TABLE's, so a modal row
  carries its help on the binding rather than in `keyHints`.
- The sheet is two panes over one subtree and the cut is the SERVER's:
  STRUCTURED DOCUMENT = `body`, panel = `properties` + `planning`, a flush posts
  both back. The page holds no org parser and must not grow one. A panel row is
  key then value in file order (no
  `tabindex` anywhere); `+` adds one and `d`/`D` delete one; an emptied key
  deletes too; the hidden properties are not rowed at all
  (`Glance.Query.hiddenProperties`), so there is nothing to warn about and
  nothing a gesture can reach. `C-c '` (org's `org-edit-special`) swaps
  two-pane and raw org by RE-MATERIALIZING — a dirty sheet is refused with `sync
  first — C-x C-s`, since a local conversion would need the parser this keeps
  out, and the re-read lands at `synced`. Stash and restore carry both panes,
  the shape, where the document's cursor stood and what an open edit was
  holding — and only for a DIRTY sheet, a pristine one being a sibling of `#app`
  that a remount leaves standing. The sheet wears `.pop-sheet` like every other
  working surface; the panes wrap rather than querying a width, and the
  `pointer:coarse` block pins the column.
- POPUP SIZE IS A TIER and there are TWO: `.pop-band` (a list of single words —
  the state palette) and `.pop-sheet` (a working surface FIXED on both axes —
  the materialize sheet, the link and tag popups, the settings sheet). No box
  declares a width or a height of its own. `.pop-wide` was the third, growing
  between a floor and a ceiling; fixing its height at the bound made its
  definition character for character `.pop-sheet`'s, so it is gone — its floor
  existed only because a sparse entry made a GROWING box a strip, which a fixed
  box cannot become.
- A POPUP CLAMPS AND SCROLLS INSIDE, and it is a CHAIN rather than one
  declaration: `--g-pop-max` is `min(90vh, calc(100vh - 2 * var(--g-pop-top)))`
  — the foot margin is the HEAD's, derived from the anchor rather than spelled
  as a second figure, so a tall box stops as far from the bottom as it started
  from the top. The `min()` stays for a RAISED anchor, where the 90vh cap binds
  again; at the shipped `5vh` the two agree exactly. `#mpanes`
  carries `overflow:hidden` — `flex:1;min-height:0` lets the row be SIZED by the
  box but not stop its own content painting past that, and under `flex-wrap` the
  LINE is content-sized and `align-items:stretch` stretches the panes to the
  LINE — and no PANE carries a floor, a `min-height` on a flex child being a
  refusal to shrink. `#mdoc` owns its scroll, `#mprops`/`#mptable` and
  `#lpane`/`#ltable` hand theirs to the mount inside, `#cbox` and `#plist` scroll
  in their own right.
- The log strip's severity and scope are COLUMNS, each as wide as its own
  longest word — `error` 5ch, `config` 6ch — so every message starts at one x
  position. The vocabulary is not a list in the code, so `TestServe` derives both
  widths off the page's own `append` calls: a longer scope fails there.
- THE LEFT PANE IS THE STRUCTURED DOCUMENT, and it is NOT a table-view mount —
  the doctrine line: the renderer's list widget draws a list of RECORDS, one
  shape per row, and this is a list of KINDS. Elements in file order: the
  HEADLINE LINE (cells `state | priority | title | tags`), the body's own
  PARAGRAPHS (blank-line separated, each remembering the line range it came out
  of), and the CHILD headlines collapsed to one line each. `drows` is the model
  and `drawDoc` is the whole view.
- MOVEMENT IS TWO AXES, the table's habit read into the document (2026-08-04,
  replacing the one-walk grain). A LIST, a `#+begin_X`/`#+end_X` BLOCK and an
  org TABLE each still take TWO kinds of stop over the same bytes, laid out in
  document order as `[whole, leaf1..leafN]` and inline among everything else —
  the MODEL is unchanged; the WALK split: `n`/`p` step SIBLINGS at the cursor's
  grain and never dive (a composite is ONE stop, holding `n` skims; a leaf
  steps its owner's run, clamped at its ends), and `f`/`b` move the GRAIN —
  a LADDER, not two rungs: a list item carrying a nested run is itself a
  parent, `f` descends one rung (a composite's items, an item's nested run, a
  headline's cells — refusing with an echo at the finest), `b` climbs one —
  to the IMMEDIATE owner by id, never a scan — and back to the whole line in
  one press whatever the column,
  a no-op with an echo at the element grain, NEVER a close (out of the sheet
  stays `DEL`'s). `l`/`h` and the horizontal arrows stay the within-grain cell
  walk, off either end into the whole-element look. `RET` is
  pure edit at either grain (a leaf opens its own lines, a composite the whole
  block's), `DEL` stays the sheet's ladder, and `d` flags whatever the stop is.
  `grain` on a row names its kind: `element`, `composite`, `leaf`.
- ONE GRAIN SPEAKS FOR A RANGE. A composite and its leaves cover the same lines,
  so `bodyText` leaves a leaf out of the splice whenever its owner MOVED or is
  going — a reader flagging a list and one of its items gets one deletion.
  A composite is likewise DRAWN once with its leaves inside it, and what no leaf
  claims is drawn INERT (`.dg`, muted): the `#+begin_`/`#+end_` lines, the blank
  line org lets stand between two items, a lead-in the opener did not take.
  Every byte on screen exactly once — the lens's rule, one grain down.
- A TABLE'S LEAF IS A LINE, and that is the one place the table grain differs
  from the list's: a list's leaves are RUNS found by `listRun`, a table's are
  cut inline, one per `|`-opening line. `|---|` rules are leaves like any other
  line — a line is a line, and editing or deleting one is the same act — so
  there is no cell grain and no column awareness. Corpus at 2026-08-03: 101 of
  6337 files hold table rows, 2178 lines, 211 of them rules, which is what makes
  the coarse grain plenty.
- The openers are the CORPUS's, not a guess: `-` (28571 lines), `1.`/`1)`
  (2675), `+` (42) and an INDENTED `*` (34). A block is ANY `#+begin_X` with a
  matching `#+end_X` BY NAME — naming quote/src/example would have missed this
  corpus's commonest block by a factor of three (`pin` 1022, `src` 338, `quote`
  111, `notes` 42, `example` 38). ONE blank line stays inside a list (org's rule
  and 1173 corpus item pairs); two, or a blank with something else under it,
  close it. An item deeper than the first RIDES INSIDE the item above rather
  than taking a stop — v1's grain, and the nesting is still there in the text.
  An opener with no closer is ordinary text. A paragraph ends at the next
  STRUCTURE as readily as at a blank line, org letting a list follow its lead-in
  with no blank between.
- ORG LINKS RENDER, under org's own DISPLAY-VS-SOURCE model: what is SHOWN is
  the description (`[[T][D]]` shows `D`, `[[T]]` shows `T`, a bare URL shows
  itself — `table-view.js`'s `displayText` rule, which is the table's), and what
  `RET` opens is the RAW org, brackets and all. The display never becomes the
  source, so an edit is always over what the file says. NO SECOND PARSER: the
  shown text is the server's `desc` verbatim (`Glance.Query.linkShown`) and the
  range is its `span`, one scan in `Glance.Query`, so this page only intersects
  the file-spans into an element's own coordinates and draws segments
  (`drawText`). `drawText` walks the segments in order and SILENTLY DROPS a link
  that starts inside the previous one (`if (a < cut) continue;`), so it rests on
  a non-overlap guarantee that only `subtreeLinks` can give and that nothing
  checks: overlapping spans out of the scanner lose segments here with no
  complaint. A bare URL is drawn because it is in the same answer. SPAN-driven,
  never search-driven, which has one visible consequence: `/links` keeps one
  entry per (target, shown) pair (`orgLinks`), so a URL written twice under one
  look is MARKED ONCE and the later occurrences read as the text they are —
  while the same target under two descriptions is two entries and both mark. The
  paragraph/leaf elements and the headline's TITLE cell render alike; the title
  needs `titleAt` (the server's `Glance.Query.titleSpan`, `Span` rather than the
  internal `HeadlineSpans`) because only the server has that sub-span, and a
  CHILD's title stays text. `/links` is fetched once beside the materialize and
  the document is drawn without waiting, so a failed link scan costs the marks
  and never the sheet. Links are NOT stops and bind no mouse — `o` is the opener
  and shares `linksIn` with the draw, so what a reader sees marked in an element
  is exactly what `o` there will find. `--g-link` is hand-copied from the
  renderer's `--tv-link` (`#30739B` / `#7CC9F8`) like `--g-border`, since
  `--tv-link` is declared on `.tv-root` and a live `var()` resolves to nothing
  beside the mount; ALIASED, so every use reads the name and the suite forbids
  the hex at a use site.
- THE CURSOR CARRIES ITS PANE'S SCROLL. `keepInView` on every draw, and the band
  is CSS: `.de` carries `scroll-margin-block: var(--g-doc-off)` and
  `scrollIntoView({block:"nearest"})` honours it, so the scrolloff is three of
  the pane's OWN lines (`calc(3 * var(--g-doc-fs) * var(--g-doc-lh))`, the two
  numbers the pane is set in) and the movement code measures nothing.
  `scrollIntoView` is forbidden over the TABLE's rows — the renderer owns their
  scroller, their page and their selection — and ordinary here, the document's
  rows and scroller being the shell's. The suite keeps the distinction by
  COUNTING: one call site, plus the `typeof` that guards it.
- STARS, ORG-CLEANED. Every headline line — the root's and each child's — opens
  with its own stars drawn the way `org-hide-leading-stars` + `org-startup-indented`
  draw them: every star but the LAST rendered as a space, so the root reads
  `* Title`, a child ` * Title`, a grandchild `  * Title`. Depth is RELATIVE to
  the entry the sheet is standing on (the answer's `level`), so materializing
  into a child makes that line the root. It is DISPLAY CHROME ahead of the state
  cell rather than a cell — `f`/`b` walk past it — and the indentation IS the
  outline, so the child lines carry no padding of their own.
- AND CONTENT SITS UNDER THE TITLE TEXT, which is `org-startup-indented`'s other
  half: a paragraph starts at the head's own title column rather than at its
  stars. The width is DERIVED from `dstars` — the head is the root of its own
  document whatever entry the sheet walked into, so the answer is the same two at
  every depth — and is written onto `#mdoc` as a NUMBER (`--g-doc-indent`), with
  the arithmetic in the stylesheet, the way the log's cap is. PADDING rather than
  a margin or a `text-indent`: a margin would shrink the element's box and take
  the selection wash off the left of the line, and a `text-indent` would indent a
  block's first line alone. Chrome only — `bodyText` never reads it and the file
  bytes are untouched. The logbook strip keeps its own frame and is not content.
- AND A HEADLINE LINE IS LAID OUT AS ORG LAYS ONE OUT: the two headline kinds are
  flex rows where a paragraph is flowing text, the TITLE takes whatever room the
  line has left, and the TAGS are flushed to the far edge (`org-tags-column`).
  `margin-left:auto` on the tags rather than the title's flex alone, so a
  headline with no title still puts them at the edge. The selection is
  unaffected: the element's ground is the whole line, gap included, and the cell
  wash lands on the tags cell where it sits.
- NO PLACEHOLDERS, EVER. A part the headline has not got renders nothing in
  every state, and `f`/`b` stop on the PRESENT cells alone — a bare title is one
  stop, an absent priority is not a stop. Setting an absent part is the
  COMMANDS' job: `t` and `:` fire AT THE ELEMENT (the headline line, whatever
  the cell point, and refused on a child, which has no row id) over the row the
  sheet is on.
- EVERY SELECTION IN IT IS A GROUND, never a line. Vertical is the ROW language
  (`--g-sel`, the element's own wash) and horizontal is the COLUMN language: the
  cell under point wears the table's crosshair — the renderer's `--tv-col` band
  hue at its `--tv-cell-wash` step, aliased here as `--g-col`/`--g-cell-wash`
  (hand-copied literals like `--g-border`, since `.tv-root`'s properties are out
  of scope outside a mount). No underline, no border, no outline in any of the
  four rules; `TestServe`'s ground sweep cuts them out of the page and asserts
  it, and asserts what it swept first.
- Its movement is the TABLE's two axes: `n`/`p`, `j`/`k` and the vertical
  arrows walk siblings at the cursor's grain; `f`/`b` move the grain itself
  (the bullet above); `l`/`h` and the horizontal arrows walk the cells of the
  element that has any, off either end into the whole-element look rather than
  bumping. `TAB` crosses to the panel and back, each pane keeping its
  own cursor and each wearing the accent on its own frame (`#mdoc.on`,
  `#mprops.on`). The cursor's `dgrain` names its level: `element`, `leaf` or
  `cell`.
- RET is BY KIND: a CHILD re-materializes into it (`?child=`), a PARAGRAPH opens
  as a textarea over itself, and the TITLE cell opens in the shared overlay and
  commits `set-title` — as does RET on the headline LINE itself, the whole
  line's edit being its title (state and tags have popups, the priority ring is
  pressed), so no `f` is spent picking the cell and an absent title opens
  empty. The STATE and TAGS cells raise the value palette and the
  tags popup where they are present; `t` and `:` do the same at the element and
  are the only way to set a part that is absent. PRIORITY has no command yet.
  A CHILD's cells are read-only in v1 — a child has no row id, so no `/command`
  addresses it — while its planning, drawer, paragraphs and children are all
  editable through the lens that materialized it.
- `DEL` is UP: in a child it re-materializes the `parent` the server named (null
  being the row) and lands on the child it came out of; at the top it is the
  sheet's door. The dispatch stands aside for a key this listener claimed
  (`e.defaultPrevented`), or the table's own `DEL` would strip a filter token off
  the view underneath on the same press.
- PER-ELEMENT COMMITS in that pane: a paragraph edit or delete is one
  drift-locked `POST /headline` carrying `body` (that block's lines spliced over,
  every other byte where it was) beside the panel's own two lists, each answer
  re-pinning the digest and re-materializing off it. A cell edit is a
  `/command`, and what it wrote comes back through the WATCH — a socket frame
  naming this row re-reads the sheet, since `/command` never writes the store.
  `C-x C-s` is the commit for whichever edit is open (a paragraph has no other,
  RET being a newline inside one). `dirty()` is the PANEL's and raw mode's alone.
  `d`/`D`/`u` over the document take PARAGRAPHS; a headline refuses with a log
  line. The sheet is one entry in `SURFACES` (`up: docHolds`), the fourth
  `flagKey` surface (`DFLAGS`, a four-call mount over a Set of element ids) and
  the fourth `openEdit` shape pair (`DROW`, `DPARA`), whose `anchor` is the one
  thing a shape declares that a mount's does not.
- THE PANEL IS A TABLE-VIEW MOUNT — the renderer is the app's ONE list widget.
  A second `TableView.mount` in `#mptable` inside `#mprops`, columns `key |
  value`, `palette: true` (no bar, no resident filter), no `pageSize` (a drawer
  is short), `actionHints: false`, `flagHelp: "d/D delete · u unflag"`, and
  `marks: true` — the mark column is the PRICE of the flags, `isFlagged` and
  the hint's flag segment both being gated on `marks` in the renderer. Nothing
  here reads a mark, and the price is LIVE, so this page's CSS takes the
  checkbox off `#mptable .tv-table td.tv-box` (no glyph, no pointer, no
  hit-testing) and KEEPS the gutter, which carries the flag's inset edge. Only
  `pageSize` and `marks` carry rules; the rest is configuration. Mounted ONCE
  and re-set per sheet, so opening a sheet costs one `setRows` rather than a
  mount with a theme listener behind it.
- MODEL AND VIEW. `prows` is the model — key, value, `fixed` — and the mount is
  a view of it: `repaint()` is the one door and `props()`/`planning()` read the
  model. The cursor is the renderer's selection (`patAt()` asks
  `getSelection()`), movement is `selectStep(±1)`, the flags are the renderer's
  set, and this page keeps no copy of any of them. Row ids are stable for the
  sheet's life: `PLN:<KEYWORD>` for the three planning rows, `P<n>` handed out
  once per property, so a flag and a selection survive an edit above them.
- The panel is MODAL, and its keys are a document listener of its own — written
  with the sheet near the top of the glue, so it registers AHEAD of the dispatch
  and is the one private listener that sees a key first; safe for the reason the
  three behind the dispatch are, since `typing()` has already killed every
  `table` row and it falls through on every key it does not claim, `ESC`
  included. NAV: nothing is focusable, and movement is
  `n`/`p`, `j`/`k` and the arrows — both profiles' letters bound
  unconditionally, since a row with no field in it leaves every printable key
  free. Entering the panel BLURS the textarea and sets `pnav`, which `typing()`
  counts as a focus of its own; without that the table's own letters would move
  rows under the sheet, and `d` would flag an org row rather than a property.
  The panel's arrows are VERTICAL ONLY where the table's walk both axes: the
  mount has two columns, but `RET` opens the WHOLE row and `TAB` crosses its two
  fields, so a column selection would move a highlight and change nothing a
  reader can act on.
  EDIT: `RET` opens the row at point into the EDIT OVERLAY (`#pedit`, one pair
  of fields laid over the selected row — the mount rewrites its own rows as it
  scrolls, so an edit cannot live inside one), value focused first, key first
  where there is no key yet, a planning key `readonly` because org owns it; `+`
  adds an empty property at the end and opens it; `TAB` hops the two fields and
  the pane crossing is suspended; `RET` commits into the MODEL and re-sets the
  mount; `ESC` cancels through the keymap's `cancel`. A row HOLDS its committed
  text, so an open edit is not dirty and only a commit is. `TAB`/`S-TAB` is one
  two-stop toggle between the panes and the cursor survives it; `shut` clears
  `pnav` and closes the overlay. `preventDefault` fires only where one of those
  bindings does, and only over an open subtree sheet: raw mode has one pane so
  `TAB` is the browser's, and the settings sheet keeps native tabbing.
- DELETION IS THE TABLE'S GESTURE, over the same renderer flags — one gesture,
  deliberately spelled twice, since the panel's keys live outside `keyBindings`.
  `can`/`flagsOn` are shared; the ACT is not, and neither is `ONCE` (the panel
  guards `e.repeat` by hand), `said` (no binding row, so the shape is
  hand-spelled) or `noted` (the panel writes no log line). `d` flags the
  row at point (`tv-flagged` wash, echo `d → delete-flag (d again deletes)`), a
  second `d` on a flagged row IS `D` — the same handler, so it takes EVERY
  flagged row — and `u` unflags and steps on. `e.repeat` is guarded HERE rather
  than by the dispatch's `ONCE`, which cannot reach a key this listener owns.
  What "taken" means is the row's: a property is DROPPED, which is the emptied
  key spelled as a key press; a planning entry is CLEARED and its row stands,
  since an empty value is already how an entry is absent. A deletion moves the
  model, so it is dirty like any commit.
- The three planning rows are FIXED rows at the head of the same list —
  `SCHEDULED`, `DEADLINE`, `CLOSED` in org's order, key unopenable, value the
  timestamp text verbatim, empty meaning absent — so clearing all three is how
  the planning line comes off. The logbook is a read-only strip under both
  panes: full width, muted, out of Tab and out of `dirty()`, showing the
  drawer's INTERIOR lines alone (the widget being the drawer says what it is),
  and never sent — the server re-splices the whole drawer, delimiters included.
  Neither the logbook nor a hidden property is rowed, so neither is flaggable by
  construction.
- ONE FOCUS LANGUAGE ACROSS THE SHEET: whichever pane holds the keys wears the
  accent on its own FRAME — `#mtext:focus{outline:none;border-color:var(--g-accent)}`
  and `#mprops.on .tv-root{border-color:var(--g-accent)}` — and neither wears it
  otherwise. Declared for both rather than left to the browser, which can only
  dress the pane that takes a real focus: the panel holds the keys with nothing
  focused at all, so a reader crossing with `TAB` would watch the mark vanish.
  The panel's half is drawn from `#mprops.on`, the same state `pnav()` reads, so
  it cannot leak past the keys — `shut` clears the class, which is the leak the
  old mirrored `pnav` flag left behind a closed sheet.
- The panel's geometry read is the ONE thing this page takes out of the mount's
  DOM, and it goes through the handle's published root: `place()` asks
  `pmount.el` for `tbody tr.tv-sel`, reads its box and puts the overlay there.
  It re-runs on everything that moves that box — the mount's scroll through a
  CAPTURE-phase listener on `#mprops` (so the scroller is never named) and a
  window resize — and one frame after `openRow` (`soon`), since the renderer
  stamps `tv-sel` on its own frame. Nothing about the row's content is read.
  KNOWN LIMITS: `Handle` publishes no column geometry, so the `40%`/`50%` split
  is a guess over content-measured columns; a selected row outside the rendered
  window leaves the overlay where it was.
- The whole page wears danneskjold, through one `--g-*` palette (surface, text,
  muted, border, selection, warn, bad) declared once and re-declared per theme.
  The sheet keeps exactly one variable of its own, `--dk-mono` (Hack first);
  everything else it uses is the page's.
- `DEL` IN THE TABLE IS A LADDER, and the rhyme is the backspace's: ERASE THE
  LAST STRUCTURE STANDING. A MARKED SET is one, so while marks exist `DEL` clears
  them and stops — the MARKS alone, since a flag is the archive queue and a
  backspace must not empty it — then the query's last token, then the drill it
  was made in. It runs `U`'s own implementation (`clearMarking`, one function,
  `alsoFlags` telling the two keys apart) and the pill says `DEL → unmark-all
  (N)`, the command that RAN rather than the row's own name. A rung with nothing
  under it falls through in SILENCE; only the rung that runs speaks.
  `filter-drop-token` is already in `ONCE`, which matters more now.
- AND `DEL` CLOSES A POPUP WITH NO INNER LADDER, which is the same rung read one
  surface up: over the LINK and TAG popups the popup IS the last structure
  standing, so `DEL` steps out of it where `ESC` does, through the same `off`.
  The guard is the edit sub-mode — `DEL` inside an open rename or link edit stays
  the FIELD's character erase and closes nothing, so the two meanings never meet
  on one press. The STATE palette is the exception on purpose and keeps its
  landed meaning (`DEL` commits `*empty*`): a value is what that surface exists
  to hand back, and `*empty*` is out of the letter pool precisely because `DEL`
  already names it.
- A CURSOR IS ONLY DRAWN WHERE THE KEYS ARE. Each sheet pane already says on its
  frame whether it holds them (`#mdoc.on`, `#mprops.on`); the cursor inside it
  takes the same guard, so the sheet never shows two. The POSITION is not gated —
  it is the model's, so crossing away and back finds the cursor where it was left
  and the wash simply returns. A FLAG keeps its ground either way, being a queue
  rather than a cursor. The panel's costs TWO rules, since the wash it suppresses
  is the RENDERER's `tr.tv-sel` and the `tr.tv-alt` stripe under it has to be put
  back; a `cursor:` gate in table-view retires both.
- The applied filter query is in the URL (`replaceState`, `keys` preserved) and
  applied from it on load. `DEL` over the table drops the query's last token
  through the renderer (`stripLastToken`/`getQuery`) — the chips are the
  renderer's, so the strip is too. `remember` writes `q` unconditionally, so an
  emptied query leaves `?q=` present-and-empty: that is what `bootQuery` reads
  as intent and leaves alone, where an ABSENT `q` gets the default injected.
  Deleting the parameter instead made a cleared filter come back filtered on the
  next remount.
- DRILL-DOWN is ONE semantic at TWO GRAINS, and `DEL` is the single undo for
  both. A JUMP (`@`) pushes a crumb and applies a whole new query; a REFINEMENT
  edits the query in place and pushes nothing. `DEL` undoes whichever is
  nearest — a ladder: `stripLastToken` while the query has tokens, and when the
  strip leaves it EMPTY and a trail stands, `popCrumb` plus the popped query
  INSTEAD of the empty one, so `@` then `DEL` is one step out and one back
  rather than a step and a half. With no trail the second rung is not there and
  the key clears the filter as it always did. `g` is HOME rather than a rung: it
  throws the crumbs and their labels away.
- The crumb STACK is the renderer's (`setCrumbs`/`getCrumbs`/`pushCrumb`/
  `popCrumb`, muted chips left of the live ones, `CRUMB_MAX` 4 then a `… +N`
  fold) and this page keeps NO copy, the way it keeps none of the marks or the
  selected column. `popCrumb` pops and returns without applying — whoever owns
  the fetching owns what a query means — so the shell applies it through
  `applyView`, the same door `g` and the agenda use. What the page DOES keep is
  `crumbLabels`, token → label, because no lookup recovers it: the title belongs
  to the row referred TO, which is very rarely among its own referrers. One map
  serves both readers — the mount's `chipLabel` aliasing the live `ref:` chip,
  and `hereLabel` naming the crumb a further drill leaves behind, which is what
  makes a drill out of a drill chain honestly.
- The trail crosses a remount through the URL and NOTHING ELSE: `?crumbs=` holds
  `{trail, labels}` beside `q`, written by `remember` and read back by `mount`
  (before `TableView.mount`, since `chipLabel` can be called during the first
  paint). Every mutation of the stack — a drill, a pop, `g` — is followed by a
  `remember`, so the address bar is current whenever a `view-changed` remount
  re-reads it. `stash`/`restore` deliberately say nothing about crumbs: what
  they carry is work the reader has NOT committed, and there is no such thing as
  a half-applied crumb. A parameter that does not parse is one boot without a
  trail, and `setCrumbs` drops whatever is not a crumb.
- WHERE THE CURSOR LANDS is THREE rules at one door, `land(sel, back)`: it takes
  the row `sel` names while the view still holds it, else the row at index
  `back`, else — no rows at all — nothing. An APPLIED view (a palette commit,
  `g`, `a`, `@`, a filter commit) asks for nothing and takes row one; a POP asks
  for the row its drill was pushed from and falls back to row one; an ARCHIVE
  asks for the next surviving row below point and falls back to that row's place
  among the survivors. `select` answers false for a row the view no longer
  holds, so a remembered row an edit or a narrower filter took away falls through
  rather than being forced back. `applyView` takes the remembered selection as a
  fourth argument so the rule runs once rather than in each caller; `fetchRows`
  calls it too, since a commit REPAINTS rather than remounting and would
  otherwise leave the cursor on a row the new answer may not hold.
- A BOOT IS AN APPLIED VIEW, so it takes row one through that same `land`. A
  mount has no cursor of its own — the renderer selects nothing until it is
  asked to, `selectFirstVisible` having one caller and it being the filter box
  handing over — so a page that landed nothing opened with `d`, `D` and `RET`
  answering `no row` until the reader pressed `n`. `start` lands on the MOUNT,
  which is the `?limit=100` first paint; the full set arriving behind it lands
  nothing more, `paint` keeping the cursor the way the renderer keeps every
  selection. A caller that PASSES an `after` lands inside it and this door
  stands aside, which is what leaves a pop's remembered row alone; a
  `view-changed` remount passes none and takes row one like any other apply. The
  suite could not see any of this until `shell-harness.js` stopped answering
  `getSelection` with row 0 of the page: the stub now models `state.selected ===
  null` (`keepSelection` returns at the guard, `indexOfSelected` is -1,
  `selectStep` from nothing lands on the end it steps away from), and a `total`
  of 0 is an empty store — the one state no act can reach in time, every act
  running after the boot has painted.
- THE ARCHIVE ANCHOR, and the carve that makes room for it. `anchorFor` takes it
  at FIRE time — by the time the rows have gone the gap they left is exactly
  what a later read cannot see — scanning from POINT: down the page for the
  first row not leaving, else back up for the nearest one, else nothing. It
  carries `from` (the row point was on), `id`, `at` (the anchor's place among
  the SURVIVORS, for the anchor itself vanishing before the landing) and `on`
  (the page it was taken on). THE DOOR THE ROWS LEAVE BY IS THE FILTERED
  REFETCH: `archive` puts an UPSERT on the wire (`streamed` deletes only an id
  absent afterwards, and `:ARCHIVE:` leaves the row emitted), so an UNFILTERED
  client splices the row back in and point does not move; `resync`'s repaint is
  the only other. All three call `settled`, which ALWAYS SPENDS the anchor —
  that is what keeps it describing ONE watch step — and lands it only where
  something is owed: never while `from` is still in the view, and never on a
  page other than `on`, since `visible()` is one page. `spent(mine)` drops it
  when the answer says `from` was not archived (a refusal, and an archive over a
  set point is not in), keyed to its own anchor so an earlier answer cannot
  disarm a later archive's, and deciding before `unmark`, which can throw. A
  `commit` and a `remount` drop it outright — an anchor belongs to its view.
  THE CARVE:
  `fetchRows` takes the landing as an argument and the watch's refetch passes
  `settled` where a commit passes nothing — a refetch is the view the reader
  already had, so it lands nothing of its own and the renderer keeps the cursor.
  Before it, any watch event under a filter took a reader back to row one. What
  the anchor buys over the renderer's `keepSelection` is rows going from ABOVE
  point: that keeps the visual PLACE, which is a row further down once they have
  gone. Its other branches — the up-scan, the empty view, a surviving point row
  — agree with the renderer exactly and are guaranteed twice, so nothing
  exercises the up-scan alone.
- The remembered selection rides BESIDE the trail (`crumbSels`, one entry per
  crumb) rather than inside it, because the renderer's `crumbOf` keeps a crumb's
  `label` and `query` and drops everything else — a selection put in a crumb
  would never come back out of `getCrumbs()`. The renderer's DEPTH stays the
  truth: `selsFit` compares lengths and a side table out of step is dropped
  whole rather than pairing a crumb with another crumb's row. It rides in
  `?crumbs=` as `sels` beside `trail` and `labels`. Marks and flags need none of
  this — they are id-keyed renderer state and already survive.
- `@` ASKS BEFORE IT APPLIES. The drill is probed with the same query under
  `limit=1` — a count and one row — and a total of zero applies NO view: the
  table, the filter and the trail stay exactly where they were, with one `cmd`
  info line and an echo naming the headline. An empty view is the one landing a
  reader can read nothing off, and walking back out of it costs a keystroke to
  undo a keystroke. The cost is a second fetch on a key that was going to refetch
  anyway, which is one keypress either way — and the drill is now ASYNC, so a key
  pressed in the same tick lands before it.
- A drill out of the EMPTY query pushes NO crumb, and that is the absence of a
  special case rather than one: "all rows" IS the empty filter, and `DEL`'s first
  rung already lands there — strip the `ref:` token, the query goes empty, and
  with no trail behind it the key clears the filter, which is the very view the
  crumb would have restored. The crumb, its label and its remembered row would be
  bookkeeping for a step the ladder takes anyway. What goes with it is the
  cursor: `DEL` back out of that one drill lands on the FIRST row, like every
  applied view that is not a pop. `crumbLabels` is still written, since it names
  the live `ref:` chip.
- `@` (`org-glance-overview:relations`) takes the row AT POINT and never the
  marked set — a drill is a look, and letting it inherit a mark
  would make every mark change what the key means. It is on the ONCE list: a
  held `@` is a remount per repeat, each leaving a crumb. Feature-detected on
  the four crumb calls; an asset without them is told so and nothing is applied,
  since a view with no way back out of it is worse than no drill.
- There is NO status corner: `#corner` is gone whole, and with it the connection
  dot (`#dot`, `.live`/`.wait`/`.down`), the coarse-pointer gear (`#gear`), their
  CSS, `const dot` and its four call sites (`socket.onopen`, `socket.onclose`,
  `indexing`, `start`'s catch). The socket's state is carried twice over already
  — the stale wash, armed at 400 ms on a lost socket, and the strip's `ws` lines
  — and indexing by the strip's `boot info` line. Body padding was the corner's
  room: `34px 24px 24px` → `24px`, so nothing floats over the table's top edge.
  `themesel` sat there and is now the settings sheet's theme panel; the keys
  picker is gone with the profiles.
- With no popup open the TABLE holds the keys, and a control that keeps the
  focus belongs inside a popup. The popups — materialize sheet, settings sheet,
  filter palette, value palette, link popup, tags popup — and the controls in
  them are the only legitimate focus holders; the page has no chrome outside
  them. A focused `SELECT` counts as typing, so one loose on the page ate
  `n`/`p` as type-ahead until the reader clicked back, and the answer was a
  `blur()` every such control owed; having them all inside a popup retires the
  per-control rule. Inside a popup the focus is the popup's,
  `typing()` is true while a control of it holds the focus, and `ESC` (`any`)
  and `C-x C-s` (`modal`) reach the sheet regardless. The popup hands the keys
  back ONCE, on close (`shutSettings` blurs) — so no control on this page blurs
  on its own change.
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
  widest-scope: `default` (org's TODO/DONE) > system > tags (first wins) > file.
  FOUR scopes:
  the recognition union is NOT one of them, so a keyword only another tag's
  config names is unclassified here (`classify`'s fallback, active), shown by no
  palette and settable on no row that does not reach it.
  WIDEST-FIRST IS THE DEFERRED BOUNDARY, and it inverts what a file's own
  `#+TODO:` buys: the shared scope settles a word once and a narrower one
  extends the vocabulary without redefining it, so `#+TODO: | TODO` in a file no
  longer makes that file's `TODO` rows done-like and a `book` row's `READING`
  answers to `book.org` over its own file's line. A tag called `system` still
  keeps its TAG rank and so now sits BELOW the system layer. SETTING is
  unchanged in content — `settableStates` is the chain flattened and a union has
  no order — so the reorder moved which source SHOWS a word and no word a row
  may be set to. Letters fall out of it: `default` drawing first makes `TODO` =
  `t` and `DONE` = `d` in every tree, and a `DELEGATED` under any narrower scope
  cannot claim `d`.
  A KEYWORD LIST IS ORDERED, and the order is the ORG FILES' OWN. Segments run
  in `keywordScopes` precedence — `default`, `system`, the tag configs in walk
  order, `file` — and inside a segment the words are that layer's `#+TODO:`
  line left to right, a repeat keeping its FIRST place.
  `Config.recognizedKeywords` is the one rule; `hrKeywords` and `storeKeywords`
  both come off it, so one file's palette and the whole store's cannot order the
  same words differently. Sets answer RECOGNITION alone: `Context`'s two
  `Set Text` stay, `seedContext` builds them from the ordered lists, and that
  boundary is the only place a keyword becomes a Set. Three of them used to sit
  between the line and the palette (`PTodo`, `declaredKeywords`, `hrKeywords`
  off the ending context) and every downstream list was alphabetical, so THE
  `#+TODO:` LINE GOVERNED NOTHING — ~/sync's cycle read
  `DELEGATED PENDING REVIEW STARTED TODO …` and now reads
  `TODO STARTED PENDING DELEGATED REVIEW …`. What that buys, all of it live: the
  state column sorts by the cycle, so `docs/proposal-sort-comparators.md`'s "the
  org file IS the comparator config" is true rather than aspirational (option A,
  delivered); the palette's which-key letters are assigned over the declared
  order; `GET /keywords` answers ordered inside each source; and reordering a
  `#+TODO:` line is a palette move, so it closes the socket `view-changed` and
  the table comes back reordered. An empty store's palette is org's own pair
  rather than nothing, `default` being the chain's first scope under every root.
  Config lives at `<root>/.org-glance/config/{system.org,tags/*.org}`,
  is never a row source, and a config change reseeds and reloads the
  world (debounced, view-changed follows). The chain is ONE list,
  `Config.keywordScopes` (rank, name, keywords per scope), two readers and three
  answers: `classify` takes the first scope with an opinion, `Query.keywordSources`
  reports what each claims, and `Query.settableStates` — what `setStateEdits`
  accepts — is THAT flattened rather than a third fold, so the offer and the
  wall cannot come apart. Org's built-in cycle is `builtinKeywords`, off
  `defaultContext`, so the scope `classify` consults and the one a palette shows
  cannot hold different words. `GET /keywords` serves that chain per row, which
  is what the state palette draws.
- SET-STATE LEGALITY IS THE ROW'S CHAIN, not its file's recognized set: a
  keyword is settable only where org's cycle, `system.org`, one of THAT row's
  tags' configs or the file's own `#+TODO:` declares it. Whole-request 400
  naming the keyword and the row when any named row's chain lacks it, so a
  marked set spanning tags is refused for the member it does not fit. The
  palette is the truth — over one row what `/keywords` offers is exactly what a
  write takes; over several the merge can offer a keyword part of the set
  cannot take, and that is the 400.
- `hrDeclared` is the file's OWN `#+TODO:` and is stored beside `hrKeywords`
  (the recognized union) because neither recovers the other: a file redeclaring
  a seeded keyword the other way adds nothing to the union it disagrees with.
  One value shared per file, like the rest.
- `clSeed` is stored, not derived: `clTags` keeps the FIRST config of each tag
  across directories while the seed unions every entry read, shadowed ones
  included, in walk order — system layer first, then the tag files by name, so
  the seed's own order is the chain's under `default`. Its only consumers are
  `seedContext` (the parse) and `recognizedKeywords` (the badge palette, one
  file's and the store's alike, and the config preview) — it is out of
  `keywordScopes`, so nothing classifies or authorizes by it.
- `system.org` carries two TREE-WIDE lines beside its cycle —
  `#+GLANCE_DEFAULT_FILTER:` and `#+GLANCE_CAPTURE_TARGET:` — and each NAME is
  written once, as a key constant, with `settingOf key` reading it
  (`lastPragmaValue`, last line wins) and `settingEdits key` writing it
  (`pragmaLineEdits`: replace where it stands, insert under the header, empty
  deletes). The reader folds the key and the writer renders it, off one
  `settingPragma`, so a fold that drifted from a render can no longer rewrite a
  line nothing reads. Carried by `clFilter` and `clCapture`, and spliced in the
  SAME `configEdits` call as the block — with the layer's TEMPLATE, so four
  regions of one file ride one write, since four writes would be four digests.
  A tag layer names neither. The settings sheet edits both under the system
  layer — the default view in a table-view COMPOSER mount (below), the
  capture target as a plain field. `Config.systemSetting` is the ONE "first system
  layer that names one" fold, over the `ConfigLayerFile` list `readConfigLayers`
  returns, and both the load and the settings route call it.
- The DEFAULT VIEW is `system.org`'s `#+GLANCE_DEFAULT_FILTER:` line, read into
  `clFilter` and answered by `defaultFilter`; absent means `builtinFilter` =
  `state:*active*`, a line naming nothing means the empty query, and the LAST
  line wins. The system layer alone, and the first config directory that names
  one — a default view belongs to a tree rather than to a tag. The daemon embeds
  it into the served page as `DEFAULT_QUERY` (off the store, per request), the
  bare-boot injection and `g` both read it. WRITING it is `P`
  (`set-default-view`, ONCE): the applied query — sort tokens and all, the
  order being the grammar's — goes to `POST /config` as the optional `filter`
  under the digest `GET /config` just served — WITHOUT a `lines` key, which is
  why absent lines leave the `#+TODO:` block standing (the optional regions'
  own rule; the empty list is still the deletion). The settings sheet edits
  the same line in a COMPOSER: a fifth table-view mount over `#cfbox`
  (`composer: true` — the omnibox bar and the chips with no table behind
  them), mounted once, re-seeded per open by `setQuery` with the served value,
  handed the main table's rows for value completion; `cmoved` compares the
  mount's query against `viewBase` and the system write carries `filter` only
  where it moved. A pin landing while the sheet is open re-seeds it. The
  write reseeds, the reseed re-embeds `DEFAULT_QUERY`, and `pinnedQuery` is
  the LIVE default `g` applies, so a fresh pin needs no reload.
- `GET`/`POST /config` serve and replace ONE layer's `#+TODO:` block AND its
  capture template through the ordinary write path — `configEdits` for the spans,
  `replaceSpans` for the drift-locked atomic write — so a `#+TITLE:` and a
  comment come back byte for byte. The optional parts are `ConfigParts`, a RECORD
  rather than three positional `Maybe Text` (all three the same type, so a caller
  swapping two would compile), each three-valued the same way: absent leaves that
  part, empty takes it off, anything else writes it. `filter` and `capture` are
  the SYSTEM layer's alone and `writeLayer` scopes them; the TEMPLATE is every
  layer's. The client names a part only where it MOVED — sending the template
  unconditionally put every layer's own first heading back through the
  one-top-entry wall on every write, so a file whose heading is deeper than one
  could no longer have its cycle edited at all. The route never writes the store; the watch
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
- Creating the FIRST `.org-glance/config` in a tree that had none is two
  directories at once, which fsnotify arms and never enters, so that write
  reseeds because `writeLayer` goes through `Watch.writeSpans` like every other
  write (see Watch) rather than because an event arrives.
- Settings sheet = `,` (`customize`), the page's ONE place for a preference and
  the materialize sheet's own ladder over `/config`, which is the same code
  rather than a second copy of the shape: buttonless, ESC/backdrop syncs the
  layers that moved and closes, pristine closes with no request, `C-x C-s` syncs
  mid-edit, `conflict` waits for a keystroke. THREE PANELS, from ONE list
  (`SECTIONS`, header + part ids, the loop over it the only thing that draws a
  frame): GENERAL (the default view, the capture target and the log's height),
  THEME (the `auto`/`light`/`dark` select, a `localStorage` preference that
  applies as it is picked, asks no server and closes nothing), KEYWORDS
  (`clayers` — one select over one box holding the SELECTED layer's `#+TODO:`
  lines VERBATIM, since the page has no org parser and must not grow one — then
  the union `ceff` and its note `cfoot`). A fourth panel is an entry plus the
  markup it names and nothing else: bodies are laid out by class
  (`.csec,.cpart`), never by a roll of ids. They are markup wrapped at boot
  rather than built from the list, being heterogeneous enough that a builder
  would be a template language; the join is by id and a `parts` id the markup
  lacks throws at boot, which the suite checks statically since the harness stub
  answers every id. The list order is the TAB order (native tabbing) and the
  sheet opens on the general panel's first field. `shutSettings` blurs on the
  way out, so the popup hands the keys back once rather than per control. Where
  a field is DRAWN moves no write: the two general fields stay bound to the
  system layer and ride its own `POST /config`.
  `#config`/`#cbox` share the existing z band with `#modal`/`#sheet` and
  `#prompt`/`#pbox`, so the three values still stand. The sheet is a
  sibling of `#app`, so the `view-changed` its own write causes leaves it
  standing.
- KNOWN GAP: the gear was the coarse pointer's ONLY settings door, and it went
  with the corner. A touch reader can filter and read; `,` cannot be typed
  there, so they cannot open the settings.
- The KEYWORDS panel is ONE `<select id="clayer">` over the layers and TWO
  boxes — `<textarea id="ctext">` for the cycle and `<textarea id="ctpl">` for
  that layer's CAPTURE TEMPLATE, a region of the same file riding in the same
  write — with `#clab` naming the selected layer
  (`system · PATH` / `tag · book · PATH`, ` · not created yet` where the digest
  is empty) and `#clerr` carrying what the server last said about a write to it.
  Order is system first, then the tag layers by `localeCompare` (`byLayer`);
  `sort` is stable, so two system layers keep the server's order, which is the
  walk's. Both texts live on the LAYER (`crows[i].text`, `crows[i].tpl`) and the
  boxes are VIEWS of `crows[cat]`: `takeLayer()` copies both back to their layer
  and every door calls it first — the select's `change`, `cdirty`, `flushConfig`
  — so an edit outlives every switch and a switch asks the server nothing.
  `cmoved(r)` is `r.text !== r.base || r.tpl !== r.tplBase` plus the two general
  fields bound to the system layer, and a part is SENT only where it moved.
  `%` in the template box raises the value palette in its field mode over the
  SERVER's code list (read once per sheet open off `/capture`), so the completion
  cannot come to offer a code the expansion does not know; an answer that never
  lands leaves `%` typing itself.
  Still one drift-locked `POST /config` per FILE that moved, each awaited, each
  under its own digest. A refusal SELECTS its layer: `flushConfig` remembers the
  FIRST refused index and `showLayer`s it, so the box shows the file the message
  under it describes, and every refusal is also a `config error` line naming
  `SOURCE · PATH: message`. The select is popup chrome — native tabbing, DOM
  order is tab order — and it keeps the focus it is given; `#clayer` shares
  `#themesel`'s select rule and `.ctext` is `7em`.

## Build

- `glance.cabal` is hand-maintained; package.yaml/hpack removed — do not
  regenerate.
- `assets/table-view.js` is a committed BUILD INPUT: in `extra-source-files`,
  read by `Glance.Web.Routes`'s `embedFile` splice (`addDependentFile` recompiles on
  change). Refresh it with `make sync-renderer`, never by hand.
- Components: private sublibrary `glance-internal` (`src/`), public library
  `glance` (`src-query/`, `Glance.Query` only), private sublibrary
  `glance-web` (`src-web/`) on the public library alone, private sublibrary
  `glance-desktop-native` (`src-desktop-native/`) on `base` alone, one CLI
  dispatching to three sublibraries, one suite naming the three that carry
  testable code. A new web or daemon target depends on the public library alone.
- `glance-web` exposes fourteen modules and has no `other-modules`, and inside
  it the dependency runs ONE way — `Glance.Web.Base` the floor, `Glance.Web`
  the door: `Base` → `Keymap`/`Page.Style`/`Page.Glue` → `Page` → `Routes` →
  `Glance.Web` → `Glance.Desktop`(`.Native`), with `Watch` on `Store`,
  `Commands` on `Base` + `Store` + `Watch`, and `Routes` also reading `Filter`,
  `Sort`, `Store`, `Watch` and
  `Page.Style`. `Watch` sits under both write routes because `writeSpans` — the
  door every write leaves through — and the predicate filtering what it queues
  are the watch's; it names nothing above itself, so there is no cycle to
  close.
  `Base` holds exactly what more than one module above needs:
  `ServeOptions`, the response constructors, the body reader and the
  write-refusal vocabulary (`answerWrite` and the sentences it chooses between)
  — the route table and the command table both answer through them, and a cycle
  is what putting either above the other costs. The TH renderer splice is in
  `Routes` beside the asset serving that reads it, so `Routes` alone carries
  `TemplateHaskell`. `Glance.Web` is a facade: `serve`/`serveAs`, the banner,
  the indexing thread, and re-exports of `ServeOptions (..)`, `defaultPort`,
  `application`, `bootstrapWanted` and `viewTitleFor`, so `TestServe`,
  `TestWire` and `TestExternal` name one module as they always did.
- `glance-desktop-native` exposes `Glance.Desktop.WebKit` alone and is the ONLY
  stanza the `native-window` flag reaches: `if flag(native-window)` adds
  `-DNATIVE_WINDOW` and
  gi-gdk3/gi-glib/gi-gtk3/gi-webkit2/haskell-gi-base/text/unix there and
  nowhere else. Unflagged it builds on `base` in one module, so every other
  component is byte-identical either way and CI never needs GTK. Flagged, the
  solver pulls ~28 packages, each generated from the machine's typelibs.
- The flagged build is `make native` = `cabal.project.native` (imports
  `cabal.project`, adds `vendored/`'s packages, sets the flag) plus
  `HASKELL_GI_GIR_SEARCH_PATH=vendored/gir`. `vendored/` = gi-webkit2 and
  gi-javascriptcore4 as upstream cut them with the lines marked `glance:`
  moved to the 4.1 typelibs (`pkgconfig-depends`, `Setup.hs`'s `version`,
  gi-soup2 → gi-soup3), since Arch dropped the 4.0/libsoup2 generation; both
  keep upstream's name and version, which is what makes a local package shadow
  Hackage's. `vendored/gir/` = cairo-1.0, xlib-2.0, freetype2-2.0, the
  hand-written GIRs Arch keeps in `gobject-introspection` and this machine has
  only `-runtime` of. They stay OUT of `cabal.project` because a local package
  is built by `cabal build all` whether or not anything depends on it — that
  exclusion is what keeps the unflagged build GTK-free. `gi-gtk3`/`gi-gdk3`
  rather than `gi-gtk`/`gi-gdk`: same modules, and gi-webkit2 names the former,
  so the old spelling would put two packages claiming `GI.Gtk` in one plan.
  cabal's package hash counts resolved pkg-config VERSIONS, so a distribution
  upgrade re-keys every gi package generated from the typelibs that moved and
  `make native` regenerates them — no cache to clear by hand.
- Every implemented feature earns a `CHANGELOG.md` entry under `Unreleased`,
  written as user-visible behaviour (Added/Changed/Fixed, one line per feature);
  a coherent feature set cutting promotes `Unreleased` to a dated version and
  bumps `glance.cabal`'s `version` and README.org's to match. Commit message
  discipline is unchanged — the changelog is the reader's view, the log the
  builder's.
