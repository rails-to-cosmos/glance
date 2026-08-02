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
  violations, ~12.6k headlines, and a `walk seconds` row of ~10–11 (2026-08-02:
  6287 files, 12594 headlines, 0 violations, 11.3 s). The headline figure was
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
  `Glance.Query.documentPath`, for `isWatchable`.
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
  moving on frames or a load outcome alone.
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
- A NEW WINDOW the page asks for goes to the system browser and this one stays
  the table: `window.open(…, "_blank")` and a `target="_blank"` anchor both reach
  `Glance.Desktop.WebKit` as a `NewWindowAction` policy decision, which an
  unconnected `WebKitWebView` answers by doing nothing. `elsewhere` ignores the
  decision and hands the URI to `gtk_show_uri_on_window`; every other decision
  type is left to WebKit. The downcast is CHECKED (`castTo`) and a URI that will
  not open is printed and dropped, like every other window failure here.
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
- Commands: one route, `POST /command {name, id | ids, args, digests?}`, six
  names — `set-state {keyword: KW | null}`, `set-planning {keyword:
  SCHEDULED|DEADLINE, date: TEXT | null}`, `archive {}`, `capture {text}`,
  `add-tag {tag}` and `remove-tag {tag}`.
  Ids group by FILE and each file is one drift-locked `replaceSpans` call, so a
  marked set over three files is three atomic writes; there is no cross-file
  rollback and the answer is per id (`{results: [{id, ok, digest | error}]}`, in
  the order the ids were named). Request-shape refusals are 400 with nothing
  written — a bad body, an unimplemented name, no ids, a keyword ANY named row's
  CHAIN does not declare (named with the row), and a `set-planning` date no
  parser reads, both of which
  refuse the whole request rather than moving the rows they could — as does a
  `tag` that is not one, since a word that is not a tag is not a tag for any
  row (`Glance.Query.tagText`, the PARSER's charset). Per id: an
  unknown id, and a client digest the store no longer holds (per file, since a
  digest is). 413 outranks everything. `args` is read once into `Args`, and
  `.:!` rather than `.:?` is what tells an ABSENT field from a NULL one; `text`
  and `tag` are flat, neither having a value to clear. The
  route never writes the store — the watch is still the sole updater.
- `capture` is the ONE id-less command: it makes a row rather than editing one,
  so `{"ids": …}` is not owed and the rows-are-named rule does not reach it. The
  answer is `{ok, file, digest}`. WHERE comes off the config
  (`Glance.Query.captureTargetIn`), never the request; the entry is `* <text>`
  plus a drawer holding `:ORG_GLANCE_CREATION_TIME:` — org's INACTIVE stamp,
  server clock, at column 1, lines ending the way the target's own do
  (`eolOf`) — appended at the END of the target, under the
  target's own digest (the store's where it holds the file, else a fresh
  `currentDocument` read, which is `Data.Org.Edit.readDocument` under the
  absent-file convention and answers `("","")` for a file that is not there, so
  the capture creates it under the empty pin). The text is raw org, refused when
  empty or carrying a newline: a captured entry is ONE headline. Both stamps —
  the creation time and a planning timestamp — are rendered by one `orgStamp`,
  which differ only in their brackets and both compute the weekday.
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
- `GET /tags?ids=A,B` is the tag palette's source of truth:
  `{rows: [{id, tags}], vocabulary: […], unknown: […]}` — `rows` in the order
  the ids were named, each row's tags folded through `tagsOfCell`, and
  `vocabulary` the whole store's (`storeTags`). PER ROW rather than as one
  union, because the client needs WHICH rows lack a tag: an add writes the rows
  that do not carry it and no others. The union, its partial counts and their
  order are the palette's, computed off this. Refusals follow `/keywords`'.
- `GET /links?id=ROW` is where a row points: `{links: [{target, desc}]}`, out of
  the row's SUBTREE, in order of appearance and one entry per target (first desc
  kept). The rule is the DISPLAY rule — `Glance.Query.linkAt` is the parser
  `displayText` reads a cell with, so a bracket link is described by its `DESC`
  and by its target where it has none — plus bare `http(s)`/`mailto:` URLs,
  which describe themselves: a WORD, opening at a non-word boundary, with
  trailing `.,;:!?'"()[]{}<>` off the tail. One left-to-right pass over the
  bracket links, so `[[https://x][y]]` never also reports its target as a bare
  URL. Server-side because the page holds no org parser and must not grow one.
  404 on an unknown id, 400 with none, 503 while indexing, 405 on POST.
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
  matches nothing; and `*active*` ORs in the EMPTY cell, where `*inactive*`
  does not), `priority` is exact
  equality, `scheduled`/`deadline` are prefix, everything else is substring.
  `key:none` is the empty cell on the COLUMN keys only — `tag:none` is untagged,
  since `tag` is a column — and has no branch for a virtual key, where
  `contact:none` means tagged `contact` AND the row text containing `none`.
  `key:` narrows nothing. The virtual keys are the store's org tags
  (`storeTags`, kept per tag beside the rows): `TAG:text` is tagged whole-TAG and
  matching text, empty text being presence; a column shadows a tag of its name.
  `planned` is the one virtual key that is neither a column nor a tag: a row is
  planned when its `scheduled` OR `deadline` cell holds anything, so
  `planned:none` is neither and `-planned:none` is the agenda's half. It resolves
  ahead of the vocabulary (so it shadows a tag of that name), takes a date PREFIX
  asked of both cells at once, and is single-valued like the columns it stands
  over. Renderer-decidable off the same two cells — no keyword set, no
  vocabulary, no clock. The renderer's half landed in table-view alongside;
  the vendored `assets/table-view.js` predates it and `make sync-renderer`
  closes the gap, which costs nothing meanwhile — `onFilter` means the renderer
  narrows nothing.
  `ref:ROWID` is the second producer-only virtual key and the one a row cannot
  answer alone: it is every row whose subtree POINTS AT the row named, resolved
  through the store's own id-resolved rows (`storeEnv`, exact-string like
  `resolveIds`). Matched against `hrLinks`, over `refSpellings` of the target —
  its `ORG_GLANCE_ID` where it has one, plus its title, which is what the
  `[[Title]]`/`[[*Title]]` forms resolve against. A row is NOT its own reference
  (org-glance's materialize footer writes a self-link, and a referrer list
  holding the row you came from holds one useless entry). An id no row claims
  matches nothing and does not 400 — it is a filter, so a stale `ref:` in a
  bookmarked URL opens an empty view. Multi-valued, so `ref:a ref:b` ANDs. Its
  value is the ONE predicate value not folded: a row id is exact-string, and
  ~/sync carries ids spelled `Password-…`/`Pets-…` that a fold would put beyond
  reach. `FilterEnv` is what carries the store to the matcher — `tagsEnv` for a
  caller with no rows behind it, where `ref:` still parses and matches nothing.
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
    substring-match `scheduled:` where the server prefix-matches it — and, since
    `planned` reads WHICH columns are dates, the same page answers `planned:` on
    the renderer's side over no columns at all, so `planned:none` is every row
    there and `-planned:none` is none of them. The predicate itself is
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
    unmoved. `*inactive*` has no such term and stays a literal, and so does the
    bare `state:active`, since `starless` is this producer's alone.
    The `state` column ships the two as `values` beside
    its `badges`, so its autocomplete can at least offer them — dimmed and
    uncounted, since those counts are per cell value and a fraction of the
    server's answer is no better a number than zero. Each badge also
    names its `group` (`active`/`inactive`) — order cannot say where a `#+TODO:`
    bar fell and the hues are not a contract. Additive; a renderer ignores the
    field. The value palette reads the badges for their HUES alone; its own
    active/inactive split is `/keywords`'.
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
  stored choice, a URL parameter and a key line that had to be rewritten. The
  ARROWS ride both axes and SILENTLY: `<up>`/`<down>` step a row and
  `<left>`/`<right>` a cell, each behind its letters, so the key line — which
  shows a command's FIRST binding — still reads `n/p rows · f/b cells`. Same
  handler, so an arrow walks off the last cell into the whole-row look the way
  `f` does. Ends
  are `<` and `>`, plus vi's `G` beside `>`. `g` is `apply-default-filter`, `a`
  is `org-glance-agenda`, `,`
  is `customize`, `:` is `org-agenda-set-tags` — the AGENDA's own key for the
  same question over there — `o` and `!` are `org-glance-overview:open`, `@` is
  `org-glance-overview:relations`, `M` is `mark-all`, `d` is
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
  holds under both spellings of a command, plus `org-glance-overview:open` and
  `org-glance-agenda`, which write nothing and are ruinous held down — a tab per
  repeat, a remount per repeat. `archive-flag` needs it most: a repeat
  that survived would flag a row and archive it from ONE press, which is the
  confirmation the two-press shape exists to be.
- Seven keys write without a sheet, all `POST /command`, and WHICH ROWS is per
  command rather than one rule. `t`/`C-c C-t` (`set-state`), `:`
  (`org-agenda-set-tags`) and `C-c C-s`/`C-c
  C-d` (`set-planning`) take the MARKED set when there is one and the row at
  point otherwise — dired's rule, and the generic bulk selection. `D` and `d`
  take the FLAGGED set instead and never read marks: a mark is what a reader lays
  down to set a state over a run of rows, and letting the archive key inherit one
  makes every mark a loaded gun. `+` (`capture`) takes NO rows at all. Every set
  is the renderer's and is asked for AT command time; no set is kept here.
- `+`, `C-c C-s` and `C-c C-d` raise the value palette in its TEXT mode
  (`askText`): the same overlay, the same band, the same `unask` and the same
  ESC through `cancel`, with `prompting.text` set — no list, no letters, RET
  commits the line as typed. `+`'s line goes to the server whole; the two chords
  send it as `date`, and an EMPTY line is the null that clears the entry. Both
  chords reach the page where `C-c C-t` does not: `Ctrl+S` and `Ctrl+D` are page
  default actions rather than chrome shortcuts, so `preventDefault` on the
  completing chord is the whole of what they need.
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
- `t`/`C-c C-t` raise a value palette of the shell's OWN, and what it shows is
  the RESOLVER'S TRUTH: `GET /keywords?ids=…` answers with the classification
  chain behind those rows, and the palette draws it as a table — Source |
  Active | Inactive, one row per source in precedence order (widest first, so
  `default` leads), `*clear*` spanning
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
  Its keys live in a SECOND document listener behind the dispatch,
  safe because `typing()` — which the palette turns on with NO field focused,
  the way the property panel's nav does — has already killed every `table` row,
  so `n` moves nothing and `d` flags nothing while it is up. The pill counts what
  landed, the log names every row it landed on and every one refused, and the
  rows arrive over the watch. TWO GUARDS, one press each: `prompting.raising`
  declines the keydown that OPENED the palette (that listener is behind the
  dispatch, and `t` is both the opener and a letter), and `e.repeat` stops a HELD
  `t` committing through what it opened — `ONCE` cannot reach it, since it
  governs dispatch rows and the repeat lands while every row is dead.
- `:` (`org-agenda-set-tags`) raises the tag palette, and it is the ONE palette
  that STAYS UP: managing tags is several ops over one set where setting a state
  is one, so a letter commits and the list comes back rather than the overlay
  closing under the reader (`prompting.sticky`, the only thing `takeChoice`
  branches on). What it lists is the SET's tags — the union over the target rows,
  FIRST-SEEN across the rows as named, each row's in the order its file spells
  them. First-seen rather than alphabetical because an added tag then joins at
  the END and no letter already claimed moves; an alphabetical insert in the
  middle would take one out from under the reader's fingers.
  A letter TOGGLES, under dired's NORMALIZE-UP rule: a tag EVERY target carries
  comes off all of them, and one only SOME of them carry — or none — goes on to
  the rows that LACK it. So over a mixed set the first press levels it and only
  the second takes anything away. A partial entry says so: `3/5` in the muted
  `.pt` aside the link palette puts a target in, absent where the set is level.
  The write goes to the rows it is FOR, so the answer's landed count is a count
  of rows that MOVED.
  The refresh is the ANSWER, never a re-read: `/command` does not write the store
  — the watch does, a debounce later — so re-asking `/tags` would report what the
  files said BEFORE the write. Normalize-up makes the new state a function of
  what landed, so the palette folds the per-id results into the sets it holds and
  redraws off those; a refused row keeps the tags it had.
  ONE FIELD, TWO DOORS — `/` and `+` both raise it, the way `d` on an
  already-flagged row IS `D`. It only ever ADDS; a letter is the only toggle.
  What it COMPLETES over is the ADDABLE vocabulary (`prompting.wider`, a thunk
  so it is current after a commit): the tree's whole `vocabulary` LESS the tags
  every target already carries, since adding one of those is a no-op — a tag
  only SOME of them carry stays offered and wears its `3/5`, adding it being the
  normalize-up half of the letter's rule. The set's partial tags lead, then the
  rest of the tree's. RET takes the highlighted entry or, where nothing matched,
  the line as typed (`freely`) — so a tag the tree has never held is committable
  and the charset wall that refuses garbage is the SERVER's. ESC steps BACK to
  the letters from either door (`prompting.narrow && prompting.sticky`), and a
  second ESC closes; `letterMode` re-derives the letter list through
  `prompting.letters`, the field having replaced `choices` with what it
  completes over. `+` claims no which-key letter because `whichKeys` hands out
  `a`–`z` alone. A tag is FOLDED at commit, since presence is.
  Guards are the state palette's, one press each: `prompting.raising` declines
  the keydown that opened it and `e.repeat` stops a held letter committing
  through it. `:` stays OUT of `ONCE` for `t`'s reason — raising sets
  `prompting`, `typing()` kills every `table` row, and a held key cannot re-raise.
- `o`/`!` (`org-glance-overview:open`) FOLLOW the row, and the ANSWER decides the
  gesture: `GET /links?id=` for the row at point, then no links is an echo
  refusal, ONE is `window.open(target, "_blank", "noopener")`, and SEVERAL raise
  the palette. Every open writes a `cmd` line naming the target.
  A tab can be pointed at `http`/`https` and NOTHING ELSE (`followable`): org
  writes `mailto:`, `file:`, `id:`, its own org-glance protocols and bare
  `[[Title]]` internal links, `/links` reports them all, and each names
  something a tab is not. A non-followable target is one `cmd` WARN line —
  `link type not implemented: TARGET`, truncated at 80 characters
  (`shortly`) — plus the same words in the echo, and no tab. The judgement lives
  in `openLink`, which is why it is ONE function rather than a filter over the
  choices: the palette still LISTS every link, since that is what teaches a
  reader what the entry holds, and the COMMIT is where the answer is given — so
  a lone `mailto:` warns without a palette, and a `mailto:` entry beside an
  `http` one warns while its neighbour still opens. The link palette
  is the value palette's third shape and is raised LATE — the answer decides
  whether there IS a palette, so `askLinks` puts it up behind the fetch and
  clears `prompting.raising`: the `o` that asked has been dispatched and gone,
  and declining a press would eat the reader's first real key. It draws FLAT
  (`drawChoices` branches on `prompting.table`), an entry's label being the
  link's DESCRIPTION with the target beside it muted (`.pt`), and `/` narrows
  over both through the entry's `hay`.
- `a` (`org-glance-agenda`) is a canned VIEW, not a mode: `state:*active*
  -planned:none` through `applyView`, the door `g` uses — URL, socket dropped,
  remount — so the query is the renderer's chips and `DEL` strips it like any
  other. No agenda state anywhere; `g` is the way home. The sort arrives through
  `landed`, a one-shot thunk `start` TAKES before it fetches (so a boot that
  never lands cannot leave it armed), called with the SERVER's match count, which
  is the one number the first page cannot give. It insists on
  `sortBy("scheduled", true)`, feature-detected — the view already declares that
  sort and a remount re-reads it, so the call makes the order the agenda's own
  rather than a coincidence of the default. `sortBy` landed in table-view
  alongside; the vendored asset predates it and the detection is what carries
  that.
- Letters are `whichKeys(labels)`: over the labels flattened in DRAW order —
  each source row's active cell then its inactive one, `*clear*` last — each
  entry
  takes the INDEX of the first letter of its OWN spelling, downcased, that no
  earlier entry claimed — one `a`–`z` pool, `-1` for none left, so `TODO DONE
  DELEGATED` = `t d e`. Pure and order-only, so a tree's cycle always yields the
  same letters, and `default` leading the draw is what gives `TODO` `t` and
  `DONE` `d` in every tree. One pool over the WHOLE table, so a letter is the reader's
  wherever in it the keyword sits, and the fallback narrows that same list.
  `*clear*` is OUT of the pool: it answers to `DEL` — a key that already means
  take-it-off wherever this page binds one — so the `a`–`z` namespace is spent on
  KEYWORDS alone and a cycle wide enough to run it dry keeps the letter the meta
  used to take. `offer` decides that by the entry carrying a key of its OWN
  (`fixed`) rather than by its being the meta. In the typing mode `DEL` is the
  field's and `*clear*` is reached by narrowing to it, like every other entry;
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
  `*clear*` comes
  last in the starred-meta italic. An entry that claimed nothing is drawn BARE
  — no slot, no dot — and is reachable through `/` alone. ONE entry keeps a
  token, and it is `*clear*`: `DEL` names no position in a word to mark. The
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
- STARRED METAS. The `*word*` form marks a RESERVED META with semantics of its
  own — never a literal keyword and never a cell value. The family:
  `*active*`/`*inactive*` (filter group metas, producer-evaluated) and `*clear*`
  (the state palette's take-the-keyword-off entry, committed as a null keyword
  and answering to `DEL` rather than to a pool letter).
  `*active*` is the file's active keywords PLUS the EMPTY state cell — a
  stateless entry is live work, and the default view is what would otherwise
  hide it — while `*inactive*` is stated keywords alone, so the two do not
  partition the column, `-state:*active*` drops the empty cell, and `state:none`
  stays the explicit spelling and is now a subset of `*active*`. The empty half
  is read off the CELL, which is what `none` reads and the one term the renderer
  can answer for itself.
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
- Commands are named as elisp functions and the ECHO speaks them verbatim:
  `SEQ → command`, with anything else in brackets after it (`> → last-row (page
  2/129)`, `m → mark-toggle (marked · 2)`) — never the prose spelling, since the
  rebinding config to come will address a function by exactly this string. One
  helper emits the shape (`said(b, what)`); `run`'s default is the same with
  `kbHelp` after a `·`. The resident key line is the exception on purpose: its
  labels are curated prose (`rows`, `pages`) naming a group, not a command that
  ran.
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
  `tabindex` anywhere); `+` adds one and `d`/`D` delete one; an emptied key
  deletes too; the hidden properties are not rowed at all
  (`Glance.Query.hiddenProperties`), so there is nothing to warn about and
  nothing a gesture can reach. `C-c '` (org's `org-edit-special`) swaps
  two-pane and raw org by RE-MATERIALIZING — a dirty sheet is refused with `sync
  first — C-x C-s`, since a local conversion would need the parser this keeps
  out, and the re-read lands at `synced`. Stash and restore carry both panes and
  the shape. The sheet is four fifths of the window each way (`min(80vw,100%)` ×
  `min(80vh,100%)`); the panes wrap rather than querying a width, and the
  `pointer:coarse` block pins the column.
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
- The panel is MODAL, and its keys are a SECOND document listener behind the
  dispatch, like the value palette's. NAV: nothing is focusable, and movement is
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
- WHERE AN APPLIED VIEW LANDS THE CURSOR is one rule at one door (`land`), and
  it has two answers. A POP puts back the row its drill was pushed from; every
  other application — a palette commit, `g`, `a`, `@` — lands on the FIRST row
  of the answer. An empty answer selects nothing. `select` answers false for a
  row the view no longer holds, so a remembered row an edit or a narrower filter
  took away falls through to the same first-row landing and is never forced
  back. `applyView` takes the remembered selection as a fourth argument so the
  rule runs once rather than in each caller; `fetchRows` calls it too, since a
  commit REPAINTS rather than remounting and would otherwise leave the cursor on
  a row the new answer may not hold.
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
- One status corner, top right, in this order: the connection dot (`live` /
  `wait` / `down`) then `themesel`, a native `<select>` over
  `auto`/`light`/`dark`. A focused `SELECT` counts as typing, so its own arrows
  reach it. The keys picker is gone with the profiles.
- With no popup open the TABLE holds the keys. The popups — materialize sheet,
  settings sheet, filter palette, value palette — and the text fields in them
  are the only legitimate focus holders; corner chrome is not one, so `themesel`
  blurs itself in its own `change` handler once the theme is applied, and any
  control added there owes the same line. A `SELECT` counts as typing, so one
  that keeps the focus eats `n`/`p` as type-ahead until the reader clicks back.
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
  included. Its only consumers are `seedContext` (the parse) and
  `Store.storeKeywords` (the badge palette / config preview) — it is out of
  `keywordScopes`, so nothing classifies or authorizes by it.
- `system.org` carries two TREE-WIDE lines beside its cycle —
  `#+GLANCE_DEFAULT_FILTER:` and `#+GLANCE_CAPTURE_TARGET:` — read by one
  `lastPragmaValue` (last line wins), written by one `pragmaLineEdits` (replace
  where it stands, insert under the header, empty deletes), carried by `clFilter`
  and `clCapture`, and spliced in the SAME `configEdits` call as the block, since
  three writes would be three digests. A tag layer names neither. The settings
  sheet edits them as two fields under the system layer.
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
- `assets/table-view.js` is a committed BUILD INPUT: in `extra-source-files`,
  read by `Glance.Web`'s `embedFile` splice (`addDependentFile` recompiles on
  change). Refresh it with `make sync-renderer`, never by hand.
- Components: private sublibrary `glance-internal` (`src/`), public library
  `glance` (`src-query/`, `Glance.Query` only), private sublibrary
  `glance-web` (`src-web/`) on the public library alone, private sublibrary
  `glance-desktop-native` (`src-desktop-native/`) on `base` alone, one CLI
  dispatching to three sublibraries, one suite naming the three that carry
  testable code. A new web or daemon target depends on the public library alone.
- `glance-web` exposes six modules and has no `other-modules`:
  `Glance.Desktop`, `Glance.Desktop.Native`, `Glance.Web`, `Glance.Web.Filter`,
  `Glance.Web.Store`, `Glance.Web.Watch`.
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
