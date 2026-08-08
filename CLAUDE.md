# glance — invariants

Rules the code silently enforces. Violating one is a bug even when the suite
stays green. Terse on purpose: the RULE and what breaks without it. Evidence,
measurements and the history of superseded designs live in
[docs/invariants.md](docs/invariants.md).

## Spans

- Spans are half-open CHAR offsets `[start, end)` into the `orgParse` input —
  never bytes, never line/column.
- Headline sub-spans are tight, per component: todo/priority/tags slice exactly,
  the title up to word normalization, each planning span a bracketed timestamp
  that reparses, properties a `:PROPERTIES:`…`:END:` block. Element spans are
  only well-formed + reparseable.
- `hsFull` is derived, never stored: a left fold of `<>` over `spanParts` seeded
  with `hsStars`. Starts at the stars, ends at the LAST present part in source
  order — never a maximum over ends. Never covers trailing whitespace. Sub-spans
  nest inside it, ordered todo < priority < title < tags < planning < properties,
  non-overlapping; a drawer ends exactly at `hsFull`'s end.
- The three planning spans permute freely on their line, so `spanParts` sorts
  that triple by `spanStart` and leaves the other five positional. `hsFull` and
  `headlineSpanParts` read that one ordering. A planning span covers the
  timestamp alone — the keyword is not part of it.
- `stripSpans` must cover every span-carrying constructor; a new `Element` that
  embeds spans must extend it. The suite reads elements through `bare =
  map (stripSpans . valueOf)`, so ~150 span-insensitive assertions go
  span-sensitive the moment it stops being total.
- A subtree span runs from a headline's stars to the next headline at its level
  or shallower, else to EOF; computed over EVERY headline though only top-level
  ones keep records, so surviving extents tile and consecutive ones meet
  exactly. Trailing blank lines belong to the subtree above.

## Parser

- A top-level element must end at whitespace or EOF; a sub-parser stopping
  mid-word fails the WHOLE file (the residual corpus failure class).
- Headlines parse only at column 1, via the threaded begin-of-line Bool. Never
  `getSourcePos` — quadratic on failure-heavy input.
- Column 1 is necessary and not sufficient: `indentP` also requires the star run
  to END — horizontal space (consumed) or end of line/input (looked at). Org's
  own rule, so a body line opening `*bold*` is emphasis rather than a row. A
  bare star run stays the empty headline it has always been. The stars never
  consume the NEWLINE: with `MPC.space` there, an empty title ran on and took
  the next line's stars as its own.
- TODO keywords match case-sensitively and store verbatim; pragma/property KEYS
  uppercase. `PTodo` carries both halves as LISTS in line order — a `#+TODO:`
  line is a cycle, and its spelling is the tree's whole say over how states sort
  and how a palette draws. `#+SEQ_TODO:`/`#+TYP_TODO:` land in the same `PTodo`
  (a re-render says `#+TODO:`); fast-access selectors (`TODO(t!)`) are dropped.
  The parser folds the same words into `Context`'s two `Set`s, where recognition
  is answered and order means nothing.
- In `spannedContainerUntil` the end-parser branch precedes the hspace-eol branch
  (tags open with `hspace1` and lose it otherwise).
- Trailing hspace terminates a container and stays unconsumed.
- The property parser rejects reserved `PROPERTIES`/`END` — that guard is what
  terminates the drawer.
- Timestamp range halves share one bracket kind; `tsmHasTime` alone decides
  whether a time renders; the weekday is recomputed from the date.
- The weekday slot takes a run of LETTERS in any script, any length, and drops
  it — display-only, so a locale's word is as good as org's. Letters is the whole
  charset: a repeater opens with `.`, `+`, `-` or a digit, so one letter holds
  `.+3d` out of the slot. Exactly three letters lost the corpus its Dutch stamps
  and, behind them, the drawer and the id.
- A range is `<a>--<b>` or compact `<date wd 10:30-11:30>`; `tsCompactRange`
  preserves which and the renderer never canonicalizes (CLOCK lines are always
  `--`). A `-` before a time opens a range end; before a unit it is org's WARNING
  cookie (`-3d`, first-only `--3d`) in `tsWarning` — never a repeater. At most
  one repeater and one warning, either order, first of each kind winning; the
  render spells repeater-then-warning.
- The planning line is the one line after the title, before any drawer:
  `SCHEDULED:`/`DEADLINE:`/`CLOSED:` uppercase-only, any order, last-wins per
  keyword. `CLOCK:` is not one. The whole line backtracks when it is not a
  planning line; a `SCHEDULED:` further down stays body. The backtrack is `try`
  around each entry and must roll back the leading hspace it skipped — the top
  loop requires whitespace BETWEEN elements, so eating it fails the document.
- `spanRange` forces at every step (`foldl'` + `$!`); a thunk chain would outlive
  the document it points into.
- `compactly` guards the compact render with three conditions — the flag, both
  ends timed, one day. Only the flag is exercised.
- `orgParse` on error returns zero elements AND the caller's context untouched.
- Context keyword sets are append-only; a `#+TODO:` affects only headlines below
  it; no Context merge exists — `defaultContext` seeds TODO/DONE.
- IAS registration is last-writer-wins by `Map.insert`. `resolveHeadline` is
  reached from the suite alone.

## Render

- `TextShow` is a lossy REPL re-serializer (whitespace collapse, uppercased
  pragma keys). Never for write-back or the wire — spans are the only lossless
  channel. TestRoundtrip's exact-vs-stable split IS the lossiness budget, and it
  is EMPTY: 23 `Exact` rows, no `Stable`. `Stable` stays as the mechanism for a
  case that genuinely is one.

## Scan

- Every accumulator is forced at each step; `forceResult` runs inside `evaluate`
  + `try`. Budget: pool width × one document. `Cursor` assumes non-decreasing
  span starts.
- Per-file reads run on a pool of `getNumCapabilities`
  (`Data.Org.Walk.mapFilesConcurrently`, one implementation for
  `Glance.Query.loadDirFilesWith` and the scan): sound because every file parses
  from `defaultContext` sharing no state, deterministic because results
  reassemble by input index, bounded because each worker forces before
  returning. The walk itself stays serial and is most of the wall.
- Forcing alone does not bound residency: a `Text` slice shares the document's
  array, so cells are `T.copy`'d out (`Glance.Query.detach`). `hrHeadline` and
  `hrDoc` deliberately keep the document.
- Where a root holds an org-glance store, `scan` folds its WAL and reports
  `org-glance index: N rows disagree`. Stores are each root's own
  `.org-glance/meta` plus every `meta` the walk DECLINED, so a nested one is
  found for free — and missed under `--include-derived`. The fold is
  `Data.Org.Index`, read-only, faithful to `org-glance-graph--latest-records`:
  MANIFEST's sealed segments oldest-first, the open `headlines.jsonl` LAST,
  latest record per id wins, tombstoned ids leave; only the open segment's final
  line may be torn, and a name opens only when it spells `seg-<digits>.jsonl`.
  Compared by `ORG_GLANCE_ID` against the blob's FIRST headline (never a
  child's): the keyword always, the archive flag only where the record CARRIES
  the key, `(eq t VALUE)` so `{}` is false. `blobs … carrying no id` is the
  instrument on itself, which keeps `records without blobs` from reading as
  index lag.
- The drift FIX is a one-file contract: every successful write to a BLOB
  (`isBlob` — `data.org` in the canonical store) appends `{"id","at"}` to
  `meta/EXTERNAL.jsonl` — the blob's FIRST headline's `ORG_GLANCE_ID`, no id no
  line, one `editFile` one line. The note rides `replaceSpans`' success branch,
  which the five write sites reach through `Watch.writeSpans`.
  `Data.Org.External` owns format/path/append, by `openFd` append + one
  `fdWriteBuf` — `BS.appendFile` measurably LOSES lines under concurrency. The
  daemon appends only, never truncates, never touches another `meta/` file.
  Emacs's `refresh-external` adopts each id via `graph:insert` (never
  `put-content` — blobs are read, not rewritten) and shortens the file by the
  PREFIX IT READ; a crash between = a repeated refresh, no-op by construction.
- Corpus check: `cabal run -v0 glance -- scan ~/sync` — expect 0 span
  violations, ~12.6k headlines, ~10 s walk.
- The `GLANCE_CORPUS` groups PASS when the variable is unset and say so:
  `TestDefaults.withCorpusSample` prints `SKIPPED — GLANCE_CORPUS is unset` on
  stderr. A green run without those lines is unverified on the corpus half. A
  variable naming a missing directory fails loudly, as does a run that samples
  nothing.

## Walk

- Org files are the truth, so org-glance's derived mirrors are not walked. The
  rule is a DENYLIST of names directly under a `.org-glance` component —
  `overviews` and `meta`, whole subtrees — plus `isOccurrence`, a blob's history
  one level in (`data/<id>/occurrences/<STAMP>.org`), which carries the LIVE
  entry's `ORG_GLANCE_ID` and used to tie with it in `beatsForId`. The name is
  asked for ANYWHERE under `data`, since a two-character id is unsharded.
  `isCanonical` excludes it too, so under `--include-derived` it loses the id
  rather than tying. `data` is not privileged in the walk; it survives by not
  being on the list, and is privileged only in `beatsForId`. `isConfig` is a
  FOURTH exclusion and unconditional — a `config` directory directly under a
  `.org-glance` component is declined whatever `--include-derived` says, with its
  own accumulator and scan row. Nothing there is derived; those files are INPUT
  to a parse. One `Data.Org.Walk.isDerived` serves the walk and the watch — the
  watch reaches it, and `isDocument`, through the facade re-exports
  `Glance.Query.derivedPath`/`documentPath` — so a file the store never loaded
  cannot arrive by inotify.
- The exclusion is TEXTUAL, over the path the walk builds from the root as
  typed. Point `--dir` inside a `.org-glance` tree and no component matches, so
  the mirrors are walked. Nothing canonicalizes the root.
- The scan's `derived skipped` counts DIRECTORIES: `keepDerived` runs only where
  a directory is declined. A skipped file is dropped with no record.
- ONE `lstat` classifies an entry (`getSymbolicLinkStatus`, never follows); a
  SYMLINK pays a second `getFileStatus` on its target, and only when the answer
  could change what is collected — which is where Emacs's lock exits. Symlinked
  directories are never followed; a failed `lstat` falls to the keep-on-name
  branch silently. An unlistable directory IS reported; a symlinked one vanishes
  silently.
- A non-directory is kept on name alone — no existence check — so a dangling
  `.org` symlink is walked and its load fails as `ReadFailed`, counted once for
  the life of the process. Emacs's sidecars are out: `isDocument` = `isOrg` minus
  `isSidecar` (`.#name.org` and `#name.org#`), one predicate for the walk and,
  via `documentPath`, for `isWatchable`. BOTH SHAPES ARE EXACT — the auto-save is
  matched on its closing `#` too. A bare leading `#` took every `.org` file
  whose name starts with one, so a hand-written `#inbox.org` was invisible.
- `orgGlanceTails` guards its `splitDirectories`/`tails` pair with an
  allocation-free scan for `.org-glance`: a path that cannot hold the component
  exits early. The 2–5 s band needs a `RawFilePath` walk, which costs byte-level
  twins of four predicates — an open decision.
- `scan`'s argument parser recognizes `--include-derived` and treats every other
  token as a root, so `glance scan --dir X` walks a nonexistent `--dir`. `serve`
  and `desktop` reject unknown arguments; `scan` alone is permissive.
- `dirs scanned` is the number of ROOTS given, not directories traversed.
- A ROW IS A TOP ENTRY. `recordsOf` keeps `topLevel` headlines and drops the
  rest; the filter runs AFTER `subtreeSpans`, and that order is kept because
  `subtreeSpans` is the outline rule over a DOCUMENT — widen `topLevel` and
  filtering first ends a deeper row at the next KEPT headline instead of the next
  shallower one. Consequences: a word only a child carries matches nothing, a
  deeper `ORG_GLANCE_ID` is not a row id, a file that never reaches level one has
  no rows. `scan` is unaffected — it counts off `orgParse`, being a parser oracle.
- AND A ROW HAS SOMETHING TO SHOW. `blankEntry` beside `topLevel`: a top entry
  carrying none of the six column sub-spans emits no row. The file keeps the
  entry, the table skips it. It is the RECORD's rule computed at the HEADLINE's
  layer, because the ordinal numbers EMITTED rows; the layers agree because each
  span is `Nothing` exactly where `recordOf` cuts an empty cell. Nothing without
  a column rescues an entry — so a blank entry has no row id and no command can
  address it.
- A ROW ID IS `ORG_GLANCE_ID`, else `FILE#K` — K the headline's 0-based place
  among its FILE's EMITTED ROWS, numbered after BOTH filters, so a child and a
  blank entry spend no ordinal. An edit ABOVE a row no longer renames it. What
  renumbers is rows moving past each other — reorder, insert-ahead, remove, and
  an entry going blank — which ships cells under stable ids rather than a delete
  plus an insert; `ORG_GLANCE_ID` is the only immunity. Nothing parses an id
  apart (`resolveIds` is exact-string), so the separator carries no rule.
  Ordinals cannot collide with each other; an `ORG_GLANCE_ID` spelling another
  row's `FILE#K` is an ordinary collision.
- An edit under a child moves `hrDoc`/`hrDigest`/the extent and no cell: the
  store still refreshes the entry (so materialize is drift-free) and emits NO
  frame and no generation bump. `linked` rides in that JSON, so the one child
  edit that DOES stream is the one giving the subtree its first link or taking
  its last.
- One row per id. Two files claiming an `ORG_GLANCE_ID` are resolved by
  `Glance.Query.resolveIds` — a `.org-glance/data/` path wins, else walk order —
  and the losers are counted. Four call sites: `loadDir`'s `summarise`,
  `Store.storeRecords`, `Store.storeResult`, `Store.resolvedRows`, so the store
  equals the load it stands in for and the stream equals both.

## Architecture (docs/proposal-org-console-web.md, docs/plan-org-console-web.md)

- Org files are the single source of truth; no second authoritative store.
- Write-back = surgical span replacement, optimistic lock, atomic temp+rename;
  untouched bytes stay byte-identical. Engine is `Data.Org.Edit`:
  content-agnostic, no `TextShow`. The rename replaces the destination NAME, so
  a write through a SYMLINKED `.org` leaves a regular file where the link was and
  the real file untouched. The walk keeps symlinked documents on purpose, so it
  is reachable; resolving the target first is a policy nobody has taken.
- `Display`/`TextShow` stay out of the wire. The web layer is the private
  sublibrary `glance-web` with the public library alone in its `build-depends`;
  it binds 127.0.0.1 until privilege tiers land.
- The served store is an in-memory projection keyed by path, so `Map.elems` is
  walk order and `/headlines` equals a fresh `loadDir`. The watch re-parses one
  file per event from `defaultContext`; a failed load keeps that file's rows and
  streams nothing.
- Streamed frames are id-resolved like every other answer: `applyFile`/`dropFile`
  wrap their update in `streamed`, which diffs the touched ids' RESOLVED rows
  before and after. Editing the loser of a shared id streams nothing; a winner
  that goes away re-points the id at the row behind it.
- Two headlines in ONE file sharing an id keep the FIRST on both sides — a file
  does not outrank itself. `stTags` never sees the duplicate (the per-file
  projection is a `Set`). There is no index by id.
- `stGen` starts at 0 every process and is not persisted; what survives a restart
  is `stPrint`, a digest over each file's path and load-time digest. The `ETag`
  is `"<fingerprint>-g<gen>"`: identical tree → honest 304; a byte, a name or a
  root moved → a different tag whatever the generation says.
- The `X-Glance-*` stats and page headers ride on the 200 alone. A 304 carries
  the `ETag` and `Cache-Control` and nothing else.
- The watch is a per-path trailing-edge debounce of 100 ms on a monotonic clock,
  drained by a 25 ms poll loop. No ceiling, no leading edge.
- THE DRAIN LOOP IS SERIAL, and that is the correctness argument for reseed. One
  `forever` of `drain`, body `settle`, so nothing else settles while a step runs.
  `drain` takes the ripe paths OUT in the transaction before settling them, so a
  nudge arriving mid-parse waits a turn rather than being lost. A turn with
  nothing ripe writes the TVar NOTHING — 40 turns a second dirtying it would make
  a concurrent `nudge` retry for no reason. `reseed` builds the fresh store
  OUTSIDE the transaction and `reseeded` installs it wholesale, sound only
  because events queued during that walk are re-drained afterwards. Make the loop
  concurrent and any edit that landed during the walk is silently reverted.
- A CONFIG RESEED BLOCKS THAT LOOP, so the 100 ms debounce means "100 ms, or a
  full re-walk".
- THE DAEMON NUDGES EVERY PATH IT WRITES, because fsnotify arms a newly created
  directory and does not TRAVERSE INTO it — `mkdir -p a/b` leaves `b` unwatched
  permanently. Being unwatched is the PATH's property and outlives the write that
  made it, so all five write sites leave through `Watch.writeSpans` (`replaceSpans`
  + a nudge of the path just written, on the SUCCESS branch): `captureInbox`,
  `captureBlob`, `writeOne`, `commit`, `writeLayer`. Nudging a watched file costs
  nothing — the queue is keyed by path, so it coalesces. `Watch.nudge` is the ONE
  door into that queue and inotify's handler goes through it too, so `watched`
  filters a nudge exactly as it filters an event. Nothing loads or publishes at
  the door — `settle` stays the sole store updater. KNOWN GAP: an EXTERNAL create
  into a fresh shard is invisible until a restart.
- Deletion is decided by `doesFileExist` at reload time, not by the event kind.
- `stTags` counts FILES, not rows.
- `stDirErrs` and `stPrint` are written by `loadStoreWith` and nothing else, so
  they move on a RESEED as well as at startup. A directory that becomes readable
  is invisible to them until the next config change or a restart.
- `storeKeywords` merges ONE record per file — sound because every row of a file
  shares the file's keyword sets.
- The server binds before it walks: the store starts `Loading`, the walk runs on
  its own thread, the watch starts after `finishLoading` — the second and last
  writer of the store TVar, installing in one transaction so no request sees the
  new store still described as loading. Until then `/headlines`, `/headline` and
  `/ws` answer 503 + `Retry-After: 1` while `/` and the assets serve. The two 503
  bodies differ: HTTP `{"loading":true,"elapsed":S}`, the WS upgrade the shorter
  `{"loading":true}`, never accepted onto an empty store. The load gate runs
  ahead of the method check.
- `glance desktop` = the same daemon with an app-mode window opened as soon as
  the socket listens, ahead of the loaded store. Browser order: `$GLANCE_BROWSER`,
  `--browser`, then chromium/chrome/brave/vivaldi on PATH as `CMD --app=URL`;
  then `xdg-open`; then the URL printed. No window failure ever fails the daemon.
  `--dry-run` prints the resolved command and exits before binding.
- A build carrying its own window (`make native`) prefers it, and naming a
  browser beats it: `prefersNative` is the flag AND neither `$GLANCE_BROWSER` nor
  `--browser`. GTK owns the MAIN thread, so `runNative` forks the daemon and
  hands this thread to the window. Closing it stops the daemon — the window IS
  the app — and `--keep-serving` restores stage 1. A window that never opened
  leaves the daemon serving; a daemon that stops before it listens exits 1.
- A NEW WINDOW the page asks for opens as a READING PANE over this one: 80% × 90%
  of the main window, centred, transient, ESC or the manager's close ending it;
  its own new-window asks navigate in place. An `http(s)` target alone earns the
  pane (`webby`); everything else goes to `gtk_show_uri_on_window`, and a URI
  that will not open is printed and dropped. TWO DOORS BECAUSE WEBKIT HAS TWO,
  and only one is usable: a real `target="_blank"` anchor arrives as a
  `NewWindowAction` policy decision (`elsewhere`, downcast CHECKED via `castTo`),
  while the shell's `window.open` fires the `create` signal INSTEAD — unconnected
  it drops the open silently, CONNECTED it aborts the daemon (WebKitGTK reads the
  scripted open's `WindowFeatures` optional, which `"noopener"` leaves
  disengaged). So `openOverride` patches `window.open` at document start — top
  frame only — to post its URL to the `popup` script-message handler, and
  `openMessage` opens the pane: WKWebView's own shape, which the iOS/Android
  ports inherit.
- The `native-window` flag is manual and default False, and the unflagged build
  resolves no haskell-gi: `Glance.Desktop.WebKit` answers `nativeAvailable =
  False`. `Glance.Desktop.Native` holds the whole flow with no GTK in it and
  takes the window as a `String -> IO ()`, so both flag states compile and the
  suite tests the flow against a fake window in either.
- The socket carries SCHEMA.md's row ops alone. A column change closes it with
  reason `view-changed` and the client re-fetches. `ViewChanged` is a `Frame`
  whose `frameJSON` is `Nothing` — it travels as a close — and `guarded` REPLACES
  the step's frames with it, so a column change never also ships rows describing
  the palette that just moved. The bootstrap `set-rows` is snapshotted inside the
  subscribing transaction, so there is no journal and no gap; `?bootstrap=off`
  drops that frame and trades the gap for it. A client whose bounded 1024-frame
  mailbox fills loses its backlog and its registration — the watcher never waits
  on a browser — and the close is named `resync`. What overruns the mailbox is a
  BURST of steps: `publish` coalesces within a step, not across them.
- The public library exposes `Glance.Query` alone over the private
  `glance-internal`; cells are sliced from spans and the view `Value` is
  hand-built — no `ToJSON` on an internal type (SCHEMA.md is the contract).
- Commands: one route, `POST /command {name, id | ids, args, digests?}`, over ONE
  table — `commands`, name to `{argument shape, dated, one-row, edits}`. Ten
  entries: `set-state`, `set-planning`, `set-title`, `set-priority`, `archive`,
  `capture`, `add-tag`, `remove-tag`, `rename-tag`, `edit-link`. `rename-tag`
  names both ends and is a command rather than a remove plus an add: those two
  edit sets APPLY (`applyEdits` rejects only overlap) and compose to the tag
  spelled onto the title, and the pair would be two writes under two digests.
  `commandNames` is its keys; per-name request-shape guards are each entry's
  `csArgs`; only `set-planning` is `csDated`. `csArgs` is handed the IDS beside
  the `args`, because a shape refusal is about the REQUEST: only `edit-link`
  reads them, its args naming a row's own TEXT, so a span means nothing to a
  second row. `wantsLink` owns that message and puts it FIRST — the row count is
  the coarsest thing wrong. `parseCommand` resolves the name BEFORE anything
  else and a `Command` cannot be built without the entry it resolved to.
- Ids group by FILE and each file is one drift-locked `replaceSpans` call, so a
  marked set over three files is three atomic writes; no cross-file rollback, and
  the answer is per id (`{results: [{id, ok, digest | error}]}`, in the order the
  ids were named). Request-shape refusals are 400 with nothing written — a bad
  body, an unimplemented name, no ids, a keyword no named row's CHAIN declares, a
  `set-planning` date no parser reads, and `edit-link`'s own five. Each refuses
  the WHOLE request rather than moving the rows it could, as does a `tag` that is
  not one. Per id: an unknown id, and a client digest the store no longer holds.
  413 outranks everything. `args` is read once into `Args`, and `.:!` rather than
  `.:?` tells an ABSENT field from a NULL one. The route never writes the store.
- `capture` is the ONE id-less command: it makes a row rather than editing one,
  so `{"ids": …}` is not owed. The answer is `{ok, file, digest, id}`. WHERE it
  goes is the optional `tag`: ABSENT is the inbox, PRESENT is a blob in the
  store. UNTAGGED, WHERE comes off the config (`captureTargetIn`), never the
  request; the entry is `* <text>` plus a drawer holding
  `:ORG_GLANCE_CREATION_TIME:` (org's INACTIVE stamp, server clock, column 1,
  line endings the target's own) appended at the END of the target under the
  target's own digest — a fresh `currentDocument` read answers `("","")` for a
  missing file, so the capture creates it under the empty pin. The text is raw
  org. THE ONE-HEADLINE WALL IS BOTH PATHS' (`captureText`): the line is refused
  empty-after-strip or carrying a newline, and under a TAG so is every `fields`
  answer — both are spliced into ONE document, where a newline lands a column-1
  star the parser reads as a second entry. Both stamps are rendered by one
  `orgStamp`. The `id` is `rowIdIn path K` with K the count of `recordsUnder` —
  the store's rows for that FILE, never `storeRecords`, which drops a collision
  loser the ordinal was handed out before. A race, honestly: `/command` never
  writes the store, so K is what the last load saw.
- A TAGGED capture is a BLOB, org-glance's own layout. Store root = the SERVED
  root's own `.org-glance`, and a tree that keeps none is a 400 naming the
  directory rather than a daemon making one. The id is
  `Data.Org.Blob.mintBlobId` = `org-id-uuid`'s form (random v4, lowercase,
  8-4-4-4-12); the path is `blobPathIn` = `data/<first two chars>/<remainder>/
  data.org`, unsharded under three characters. READING an id is a different
  question: the corpus carries four superseded generations, so an
  `ORG_GLANCE_ID` is an OPAQUE STRING everywhere it is read. `Data.Org.Blob` is
  a module because Walk CLASSIFIES a path that is there and this CONSTRUCTS one;
  keeping the mint out of Walk keeps crypto and IO off the walk's hot path.
  `uuidFrom` is TOTAL on a short byte string, so a test can pin the shape with no
  entropy source. NO RESERVATION — the write goes out under the EMPTY digest, so
  a path that already holds a file DRIFTS rather than being overwritten.
  `blobDocument` ENDS THE TEXT FIRST and measures afterwards: a template is
  stored right-trimmed, so a title line with no newline would take the drawer
  onto itself. It composes the blob out of the EXPANDED template; the tag goes on
  through `addTagEditsIn` (the very function `add-tag` runs) and the drawer joins
  an existing `:PROPERTIES:` under its OWN indentation, else is written whole
  under the PLANNING LINE — from the title line it splices BETWEEN a headline and
  its `SCHEDULED:`. A template expanding to no headline is refused. THE REFUSALS
  ARE ORDERED, coarsest first, all ahead of a byte. ONE CLOCK READ covers BOTH
  stamps. The blob's shard is unwatched for the daemon's life, so the capture and
  every later write reach the table only because every write nudges its own path.
- A TAG'S CAPTURE TEMPLATE IS ITS CONFIG LAYER'S FIRST HEADING — the file that
  already carries its `#+TODO:` cycle. Read the way
  `org-glance-tag-config--entry` reads it: from the first `^\*+ ` LINE to EOF,
  right-trimmed, rather than as the outline extent. Everything ABOVE that heading
  is the pragmas the `#+TODO:` splice and the settings lines own, so the regions
  cannot overlap. `captureTemplateIn` FOLDS THE TAG to find the layer and the
  HEADLINE wears it verbatim. The chain is the tag's own layer, then the system
  layer's, then `bareTemplate` = `* %?` — a CONSTANT rather than a branch, so
  every case takes ONE path through `expandTemplate`. ONE HEADING PREDICATE,
  `headingStars` (`^\*+ `: stars then HORIZONTAL SPACE, so a bare star run is
  body text here where the PARSER reads it as an empty headline), asked by both
  `headingAt` and `topEntry`; the one-star wall is the WRITER's alone.
- THE EXPANSION SUBSET IS ONE LIST AND ONE SCAN, in TWO SPELLINGS.
  `captureCodes` is `%?`, `%U`, `%T`, `%^{PROMPT}` with a line of meaning each —
  what `GET /capture` serves; the scan (`templateParts`) spells the same four as
  a case and never consults the list, so a `TestQuery` case puts every advertised
  code through the scan. `templatePrompts` and `expandTemplate` are two answers
  off that one scan. EVERYTHING ELSE COPIES THROUGH, so no template is unreadable.
  Two refusals, both the WHOLE request's: no `%?`, and an ask nobody answered.
  The clock is read ONCE per request. KNOWN DIVERGENCE from org-glance,
  deliberate: its renderer also rewrites the template heading's TITLE from the
  capture's title.
- `GET /capture[?tag=NAME]` is what a client reads before it can ASK anything:
  `{template, prompts, tags, codes}`. NO tag is the untagged path's own shape (no
  template, no prompts). `tags` is here rather than on `/tags` because that route
  answers about ROWS a caller names and a capture names none.
- The capture target is `#+GLANCE_CAPTURE_TARGET:` in `system.org`, resolved
  against the SERVED ROOT; absent means `<root>/inbox.org`. An absolute path, one
  climbing out through `..`, and a name the walk would not COLLECT are refused
  where the config is READ rather than at capture time, since a capture into an
  unwalked file writes an entry no watch delivers a row for. That third rule is
  `Data.Org.Walk.isWalked`, all three of `visit`'s predicates rather than
  `isDocument` alone: `.org-glance/config/x.org` is an org file the walk
  declines, so an extension test would bless exactly what the refusal is for.
- `set-planning`'s span math is `setPlanningEdits`: an entry already there is its
  own span; an entry the line lacks joins the END of it; a headline with no line
  grows one under its TITLE LINE (`titleLineEnd`, shared with `archiveEdits`) at
  column 1; a clear takes the entry plus the TRAILING horizontal run, or the
  leading one where the entry ends its line, and takes the WHOLE LINE when it was
  the last entry. Clearing what was never there costs no edit. `planningTimestamp`
  parses once per request against the server's today: a bracketed value is kept
  verbatim once it REPARSES, `today`/`tomorrow` and `+Nd`/`+Nw`/`+Nm` work a date
  out, a bare ISO date takes an optional `HH:MM`. Everything else is a 400.
- `setPriorityEdits` is `setStateEdits`' three shapes one part along: a token
  already there is its own span; a headline with none takes `" [#X]"` behind the
  KEYWORD (org's place) else behind the stars; a null deletes the token plus the
  HORIZONTAL run behind it. Clearing a headline that carries none costs no edit,
  which is what lets the ring's wrap through NONE be pressed twice.
  `priorityText` is the wall: ONE ASCII letter, uppercased — org's `A`–`C` cycle
  is the READER's window, so a tree spelling `[#D]` is writable and unbadged.
- ORG'S PRIORITY RING, pressed rather than picked: `S-<up>` = `priority-up` runs
  `none → C → B → A → none`, `S-<down>` the reverse, both `table` scope and both
  in `ONCE` (a held key would land on the parity of the repeat count).
  Marked-else-point, like `set-state` — but EACH ROW CYCLES FROM ITS OWN VALUE,
  which `args` cannot carry for a mixed set, so the shell groups targets by
  LANDING value and fires one command per group. The echo is
  `S-<up> → priority-up ([#B] · 3)`, `*empty*` where the wrap landed on none. The
  keys reach the DOCUMENT too, over the entry the sheet stands on, refused on a
  child; `RET` on the priority cell refuses and names the two keys.
- `setTitleEdits` replaces the title's own span, or inserts `" TITLE"` behind the
  priority, else the keyword, else PAST the horizontal run after the stars —
  never `titleLineEnd`, whose answer includes the TAGS, where a title would read
  back as tag text. `titleText` is the wall: at least one character (a headline
  with none is a `blankEntry`) and ONE line. What it SAYS is the author's — a
  title ending `:word:` reads back as a tag run, which is org's grammar.
- Span math is `Glance.Query`'s, because `HeadlineSpans` is `glance-internal`'s.
  `setStateEdits` replaces the keyword span, inserts `" KW"` at `spanEnd hsStars`
  when there is none, or deletes the keyword plus the HORIZONTAL run behind it.
  `addTagEdits` inserts `TAG:` at `spanEnd hsTags`, else `" :TAG:"` at the end of
  the TITLE LINE (the max end of stars/todo/priority/title, since `hsFull` ends at
  a planning timestamp or a later drawer). `removeTagEdits` cuts `TAG:` out of the
  run, and the LAST entry takes the whole run plus the horizontal run in front.
  `archiveEdits` IS `addTagEdits archiveTag` — one insertion rule, not two that
  have to agree. Both tag commands are idempotent from opposite sides. Presence
  is FOLDED through `tagsOfCell`, and a removal takes EVERY entry spelling the
  tag. Keyword legality is per file (`hrKeywords`); `*active*`/`*inactive*` are
  in no keyword set and are refused like any other word.
- `/headlines` hides archived rows unless the query names the archive META
  `tag:*archive*` (`namesArchive`, any spelling), and `X-Glance-Archived` counts
  what it took. The predicate is exactly `-tag:*archive*`. THE STARRED SPELLING
  ALONE: `tag:archive` is the ordinary substring predicate, so a tree using the
  word for something of its own lifts nothing. The question is asked in two
  halves, each once: whether the tree carries the tag at all is `/headlines`'
  (`storeTags`), whether the query named it is `namesArchive`'s. The socket is
  NOT filtered — it carries row ops whatever the client's query, so an unfiltered
  client splices in an archived row `/headlines` would not have served.
- Materialize: `GET`/`POST /headline?id=…[&child=K]` serves and replaces a
  headline's raw subtree. The digest is pinned at load, any divergence is a 409
  with the file untouched, and the write path never WRITES the store.
- SUB-ADDRESSING is `?child=K`: a child has no row of its own, so a ROW id plus
  an INDEX names one — K the K-th headline inside that row's subtree in DOCUMENT
  order (`subtreeEntries`, one re-parse per call from the load's own seed). The
  answer carries `child`, `parent` (the one `DEL` climbs to, null being the row),
  `children`, `path`, `cells`, `ownLines` (how many lines of `body` are this
  entry's, so the same bytes are never both a paragraph and the child that owns
  them) and `links`. The digest and the id stay the ROW's: one file, one lock. An
  index the subtree lacks is a 404; one that is not a number is a 400 — a
  mistyped index that served the parent would look like a working request.
  A byte-identical commit still rewrites the file, so it costs an inotify event
  and a re-parse; `guarded` then finds nothing moved.
- The subtree lens — ONE OWNER PER BYTE, over THREE regions. `GET /headline`
  serves the subtree twice, `org` whole and `body` + `properties` + `planning` +
  `logbook` split (`headlineParts`); `POST` takes back `{org, digest}` or
  `{body, properties, planning, digest}` (`recomposedSubtree`), naming both is a
  400 and a `body` owes both lists beside it. The regions are the planning LINE,
  the headline's OWN property drawer and its OWN logbook; every other byte is the
  body's — a child's drawer is body text. Every cut is by whole lines.
  Decompose → recompose is byte-identical.
- Two of the four parts are SERVER-PRESERVED and a client neither sees nor sends
  them: `hiddenProperties` (`ORG_GLANCE_ID` — the row id a rename would break —
  and `ORG_GLANCE_CREATION_TIME`) and the whole logbook. `headlineParts` drops
  them, `recomposedSubtree` re-injects their original lines verbatim, and
  extending the list is one edit.
- Properties: an untouched pair goes back as the LINE it arrived on, verbatim;
  only an edited or added one renders `:KEY: value`, under the drawer's own
  indentation; a dropped one is not written and an empty list removes the drawer
  when nothing hidden is in it. Raw lines are consumed one per pair, so one pair
  spelled twice keeps both. Hidden lines are woven back at the INDEX they sat at.
  Pairs are read by splitting lines, never through the parser's `Properties` —
  that uppercases keys and re-tokenises values.
- Planning: entries arrive as `(KEYWORD, timestamp text)` in LINE order. An entry
  nobody changed goes back as the very text it was, where it was; anything else
  renders `KEYWORD: value` and joins behind them in SCHEDULED/DEADLINE/CLOSED
  order. An empty list drops the line. Every value is checked by REPARSE
  (`readsAsTimestamp`) and a refusal is a 409 naming the field.
- Region line indices are the BODY's: each region's subtree line less the lines
  every region ahead of it took out. Subtree indices leave a gap where a region
  was cleared. `spliceRegions` counts only body lines consumed, so two regions
  naming one line land in list order (planning, properties, logbook). A region
  the headline never had goes on the line under the title.
- `/headlines` carries `ETag: "<stPrint16>-g<stGen>"` under `Cache-Control:
  no-cache`; the generation moves in ONE function, `Store.installed`, with two
  callers (`guarded` for a per-file event, `reseeded` for a config reseed), so
  the two cannot disagree about the rule: frames produced, or a file's load
  outcome moved. `installed` takes the counter off the OLD store, which is what
  makes `reseeded` CARRY IT OVER rather than restart at zero, so a client
  revalidating across a reseed can never be handed a tag it has seen. One tag
  covers every query variant — an HTTP cache is keyed by URL, so no `Vary` is
  owed beyond the gzip middleware's own.
- The HTTP surface is a fixed route table, each entry declaring whether it needs
  a loaded store, the METHODS it takes with a handler each, and how it spells a
  405. `HEAD` aliases `GET` in one place, so no entry names it. GET is the whole
  table except `POST /headline`, `POST /command`, `POST /config`; anything else
  is 405 — JSON on those three naming the route's own methods, plain text
  elsewhere naming the routes that DO write. An upgrade aimed at any path but
  `/ws` is rejected.
- `GET /keywords?ids=A,B` is the state palette's source of truth: `{sources:
  [{source, active, inactive}], unknown: […]}`, one entry per SOURCE in
  precedence order over the ROWS named — `default`, `system`, their tags in row
  order, `file` — with each keyword under the WIDEST source that declares it and
  nowhere below (`keywordSources`, which is `classify` read forwards; the dedup
  IS the classification rule). An empty source is dropped. FOUR sources and no
  `union` row: the recognition seed is not a scope, so another tag's cycle is
  neither shown nor settable on a row that does not carry the tag. Over ONE row
  the answer IS `setStateEdits`' rule. Several ids merge by source NAME, so a
  keyword one row reaches by file and another by tag lands in the WIDER — the
  table describes the SET, and a keyword only part of it reaches is offered and
  refused with a 400 naming the row. Three reserved names are not taken out of
  the tag namespace. EVERY `ids`/`id` occurrence is read, so `?ids=a&ids=b` =
  `?ids=a,b`; the repeated form is what an id CONTAINING a comma owes.
- `GET /tags?ids=A,B` is the tags popup's source of truth: `{rows: [{id, tags}],
  vocabulary, counts, unknown}` — `rows` in the order the ids were named, each
  folded through `tagsOfCell`. PER ROW rather than as one union, because the
  client needs WHICH rows lack a tag. `counts` is how many ROWS the store holds
  under each tag, counted per request because `stTags` counts FILES.
- `GET /links?id=ROW` is where a row points: `{digest, links: [{target, desc,
  type, span}]}`, out of the row's SUBTREE, in order of appearance, one entry per
  (target, shown) PAIR — a repeat under the same description keeps the FIRST and
  its span; the same target under another description is its own entry. The rule
  is the DISPLAY rule — `linkAt` is the grammar `displayText` reads a cell with
  and `linkShown` the display rule over it — plus bare `http(s)`/`mailto:` URLs,
  which describe themselves: a WORD opening at a non-word boundary, with trailing
  punctuation off the tail. One left-to-right pass, so `[[https://x][y]]` never
  also reports its target as a bare URL. Server-side because the page holds no
  org parser and must not grow one.
- The answer is WRITEABLE: `span` is the half-open CHAR range in the FILE
  (`subtreeLinks` shifts the subtree scan's spans by where the slice starts) and
  `edit-link` takes it back; `digest` is the file's as the store holds it, and
  pinning it refuses a range the STORE has re-read since. ONE scanner answers all
  three questions asked of a bracket link: what it SHOWS, where it POINTS, where
  it SITS.
- `edit-link` PRESERVES THE FORM: `[[T][D]]` keeps its description under a
  target-only edit, `[[T]]` stays desc-less, a plain URL swaps its target and
  stays plain, and a description ARRIVING brackets a plain URL. Absent `desc`
  leaves the author's, `null` takes it off, and a description that shows nothing
  is the null spelled another way. TWO WALLS, both 400: the span must sit inside
  the ROW's own subtree and cover exactly one link edge to edge, and the
  REPLACEMENT must read back as THE LINK IT CLAIMS TO BE (reparse and compare,
  since `a][b` renders one link pointing somewhere the request never named). A
  NEWLINE in either half is refused ahead of both walls. `Data.Org.Edit` is
  content-agnostic by law, so this is the layer that owes all three checks.
- `POST /headline` caps the body at 1 MiB and answers 413 past it, before the id
  lookup, so 413 outranks 404. A malformed `?child=` is a 400 raised BEFORE the
  id is looked up.
- `?limit=` is capped at 20000; a larger one is a 400. No `limit` serves the
  whole store, the mode the shell settles into.
- The asset route takes ONE path segment through `safeName`, which rejects the
  empty name, `.`, `..` and any name carrying `/` or `\`.
- `Content-Length` is written by `sized` on every JSON, HTML and plain response,
  the 503 included; warp supplies it for the 304 and `responseFile`. The gzip
  middleware's `Vary` rides every HTTP response, 304s included, and NOT the
  websocket rejection.
- `?q=` matches `hrSearch`, a load-time mirror of `table-view.js`'s
  `displayText`, lowercased and `\x1f`-joined, so server and renderer answer a
  query alike. The link rule: `[[T][D]]` shows `D`, `[[T]]` and `[[T][]]` show
  `T`, text that never closes a link is left as written. Control runs collapse to
  one space. Filter runs before page; a page slices the EFFECTIVE chain's order,
  never walk order. The palette stays the store's whatever the page holds.
- THE ORDER IS THE GRAMMAR'S. `sort:COL` / `sort:COL:desc` are query tokens:
  written order is precedence, repeats compose, and the token NARROWS NOTHING.
  `Glance.Web.Sort.sortChainIn` reads them off `Glance.Web.Filter`'s own
  `parseFilter`, so the two modules split ONE parse; `Filter` knows the key so a
  sort token is never read as free text, and `compile` DROPS the term above the
  negation inverter — a match-all under it would make `-sort:x` the query that
  empties the table. A query naming any sort key REPLACES the chain; one naming
  none leaves it standing, so the DEFAULT CHAIN IS INVISIBLE. `sort:*none*` is
  the EMPTY CHAIN and the query's whole vocabulary for document order; it ADMITS
  NO COMPANIONS — a key beside it is a 400, since a reader who wrote both meant
  one of them. The half-typed `sort:` is no companion. `?order=` is GONE: present
  at all it is a 400 naming its replacement, because a parameter silently ignored
  would look like a working request.
- `columns:` IS THE THIRD VIEW TOKEN and shapes what the table SHOWS:
  `columns:State,Title,Tags` serves those columns in that order, narrowing
  nothing in either polarity (`Glance.Web.Columns.columnNamesIn` reads it off the
  same one parse). Names resolve case-insensitively against the default view's
  keys AND headers (`resolveColumns`), so `Tags`, `tag` and `#` all land; an
  unknown name is a CUSTOM column — key folded, header as written, kind `text` —
  whose cells `customCell` reads from the row's own subtree: `closed` is the
  planning `CLOSED:` timestamp verbatim, anything else the property drawer's
  value by folded key through the lens's own raw-line reader, hidden properties
  NOT hidden (a read-only cell rewrites nothing). THE MINIMAL SET IS TITLE: named
  anywhere it stays put, named nowhere it joins FIRST, and an all-empty token
  names no set at all — the default view stands. Repeats compose with first-wins
  dedup (folded); a negation or an alternation is a 400 naming the token; extras
  ride the KEY, so a picked `state` keeps its badges and a picked `tag` its
  `multi`. The renderer's half is chip dress and keeping the token out of free
  text — shaping is the producer's, and the shell remounts whenever a fetched
  answer's columns differ from the mounted ones. The view tokens are ONE LIST on
  each side (`Filter.viewKeys`, tv's `VIEW_KEYS`), read by `fieldOf`/`compile`
  and by `queryKeys`/`queryMatcher`, with the chip dress in `chipClassOf`.
  Sorting stays the BUILTIN columns'.
- `->` CHAINS ONE TOKEN'S COLUMNS and is SUGAR: `sort:state->title:desc` parses
  to exactly what `sort:state sort:title:desc` composes. ONE semantics —
  `segmentsOf` splits the value and `nameOf` reads each segment where a whole
  token's value was read, so no rule below knows which spelling it came from.
  Written order is precedence, first-wins dedup spans segments AND token
  boundaries, a refusal is the SEGMENT's and refuses the whole request naming the
  token as written, `sort:COL->` is the `key:` rule, and `*none*` admits no
  companion wherever it stands. NEGATION stays the token's, because the `-` is
  written before the key. The renderer's canonical form is that one token with
  `:asc` unwritten, and its chip door folds every sort token of an applied query
  into it.
- ONE COLUMN, ONE DIRECTION: a negation, an alternation, a column no view
  carries, a direction that is neither `asc` nor `desc`, and a column named twice
  are each a 400 naming the token. `sort:` half-typed orders nothing and refuses
  nothing. The renderer cannot refuse, so it DROPS the key — a divergence in the
  loud direction, and the only one where the producer is stricter. SCHEMA.md
  blesses four of the five and calls the twice-named column "no error on either
  side", which this side has refused since the tokens landed: a live disagreement
  one of the two documents owes an edit for.
- The view declares a SORT CHAIN and is served in it: `defaultSortChain` is
  state, title, deadline, scheduled, all ascending — state by the badge PALETTE,
  the declared `#+TODO:` cycle, so the table opens in org's order rather than
  alphabetically. Priority left the chain and is reachable as `sort:priority`.
  The chain is ONE list read twice — `declaredSort` spells the EFFECTIVE chain
  onto the wire, `sortedForViewWith` arranges by it — and a declaration
  disagreeing with the rows is one a renderer re-sorts out from under the reader.
  The empty chain is walk order AND no `sort` field, one function for both. The
  arrangement is the renderers' rules: empty cells last per key and OUTSIDE its
  direction, the state column by palette position with unlisted keywords tying at
  the back, `sortBy` stable so a full tie keeps walk order, text compared
  case-FOLDED. `sortedForView` derives the palette from the records it is handed;
  `/headlines` passes the store's.
- `?q=` is SCHEMA.md's filter query, parsed in `Glance.Web.Filter` as a port of
  `table-view.js`'s `scanQuery`/`parseQuery`/`tokenTest` — parity is the
  contract. Tokens split on whitespace and `&`; `key:value` (`=` alias) is a
  predicate only for a column key, `planned` or `ref`, so `:work:`, `=code=` and
  `course:x` stay text; a token opening with `"` is free text; `-` negates. One
  resolution decides both halves (`fieldOf` answering `Nothing`), so the grammar
  and the matcher cannot disagree about a token.
- COMBINATION IS ONE RULE: TOKENS AND, ALTERNATIVES OR. Every token narrows,
  whether or not another names its key — `state:TODO state:DONE` asks a one-value
  cell for two values, which is no row. A row matching EITHER is the one token
  `state:TODO|DONE`: a predicate's VALUE splits on `|` and each alternative is
  read as that key's own value, uniform over every key and value kind. A negation
  covers the whole token. Empty alternatives are DROPPED, and a value left with
  none narrows nothing — the `key:` rule. The bar is a PREDICATE's: free text is
  the text it spells, bar and all. `namesArchive` reads alternatives too.
- `Glance.Web.Filter` dispatches on the KEY NAME, never on the column's declared
  `kind`: `state` is whole-value case-insensitive plus the `*active*`/`*inactive*`
  metas (matched in their STARRED spelling alone; `*active*` ORs in the EMPTY
  cell, `*inactive*` does not), `priority` is exact equality, `scheduled`/
  `deadline` are prefix, everything else is substring. `key:*empty*` is the empty
  cell on EVERY key; a starred word on the `tag` column is that WHOLE tag; `key:`
  narrows nothing.
- FREE TEXT HAS A KEY: `substring:VALUE` is exactly what `VALUE` alone means, so
  the grammar reads `KEY:VALUE` throughout and a bare word is that spelling with
  the key elided. ONE matcher answers both — `freeTest`, reached by the `Whole`
  field `fieldOf` resolves the key to (renderer: `freeTest` off `tokenTest`,
  reached by `valueTest`'s own arm) — so the two cannot come to mean two things.
  Everything else falls out of the token rules: `-substring:x` negates,
  `substring:a|b` ORs, `substring:` narrows nothing. What the key buys over the
  bare word is a value spelling a separator's neighbour — a leading `-`, a
  colon, a bar — under quotes. A column keyed `substring` shadows it, the way
  one keyed `planned` does.
- AN ORG TAG NAMES NO KEY: `course:text` is free text, colon and all, and
  `tag:course text` is the one spelling. Two consequences are the price: `tag:`
  is a SUBSTRING of the cell, and org spells a tags cell `:web:` so the free text
  `web:` is inside every row carrying the tag. What it buys is the vocabulary
  divergence — the keys were the whole store's tags here and the loaded rows'
  there, so one token meant two things across the wire.
- `planned` is a key that is not a column: a row is planned when its `scheduled`
  OR `deadline` cell holds anything, so `planned:*empty*` is neither and
  `-planned:*empty*` is the agenda's half. It takes a date PREFIX asked of both
  cells at once, and is renderer-decidable off the same two cells. It is no
  matcher of its own: a predicate reads the CELLS its key names (`fieldCells`),
  `*empty*` is every named cell empty, a value is any of them passing. The
  whole-tag meta stays keyed by the cell's INDEX, which is why `planned` cannot
  reach it.
- `ref:ROWID` is the other key that is not a column, and the one a row cannot
  answer alone: every row whose subtree POINTS AT the row named, resolved through
  the store's own id-resolved rows (`storeEnv`, exact-string). Matched against
  `hrLinks`, over `refSpellings` of the target — its `ORG_GLANCE_ID` plus its
  title, which is what `[[Title]]`/`[[*Title]]` resolve against. A row is NOT its
  own reference. An id no row claims matches nothing and does not 400. Its value
  is the ONE predicate value not folded: a row id is exact-string. `FilterEnv`
  carries the store to the matcher and `ref:` is all it carries.
- The tags column's key is `tag`, singular (header `Tags`). A predicate reads one
  `\x1f` field of `hrSearch`, so per-cell matching and free text agree by
  construction.
- `hrLinks` is the per-row reference list, cut from the SUBTREE at load through
  the `/links` scanner and `T.copy`-detached like `hrSearch`; `forceRecord`
  forces its SPINE beside its elements, since a lazy tail would retain the
  document. What counts as a reference is `refTargetOf`, and the rule is the
  CORPUS CENSUS rather than what org permits: the id-bearing protocols
  `org-glance-visit:`, `org-glance-open:`, `org-glance-material:` and `id:`
  (org's own), stripped to a case-preserved target; a leading `*` stripped; and a
  bare target carrying neither `:` nor `/`. Deliberately NOT references, though
  common: `org-glance-overview:` names a TAG and `org-glance-state:` a keyword.
  `file:`/`http`/`mailto` are dropped, which keeps the field small. Store
  residency did not move outside GC noise. `scan` is unaffected — it builds no
  records.
- `hrLinked` is the same scan's wider answer — does the subtree hold ANY link —
  and it is what the wire carries: `rowJSON` emits `"linked": true` and nothing
  when false, sparse so a row with nowhere to go is the row it was (SCHEMA.md's
  Row is additive). The renderer underlines that row's `title`. It is the WIDE
  field on purpose: marking off `hrLinks` would leave thousands of rows `o` opens
  unmarked. Every reference is a link, so nothing underlined answers `/links`
  empty.
- KNOWN LIMIT of `ref:`, inherited from the `/links` grammar: a link nested
  inside another link's DESCRIPTION yields no reference at either end. The outer
  fails to close and the rescan picks the inner one up one bracket late.
  org-glance's own "Referred from" footer writes exactly this shape. Reused on
  purpose — a second scanner would be a second grammar to keep in step.
- Parity discipline: there is NO schema revision mechanism between this producer
  and `table-view.js`. Agreement rests on the port being kept term for term, plus
  one loose runtime tripwire. Known divergences, all live:
  - `sort:` REFUSALS are the producer's alone, and the one divergence where this
    side is STRICTER. Deliberate — an order nobody can give is worth saying, and
    the query the shell writes never spells one. TERM FOR TERM is the READING:
    one column, one direction, `:` splitting a segment, `->` splitting the value,
    written order the precedence, half-typed narrowing nothing, a negation
    covering every segment. `fixtures/parity/sort-tokens.json` runs the shared
    half over the browser renderer.
  - PRIORITY WEARS ORG'S BRACKETS and the fold is the divergence. The cell is
    `[#A]`, the column is a BADGE column, three badges (`[#A]` red, `[#B]`
    yellow, `[#C]` green; `[#D]` takes the badge-less default). DISPLAY WEARS THE
    DECORATION, MATCHING READS THROUGH IT: `priorityLetter` strips `[#`…`]` and
    folds, so `priority:A` and `priority:[#A]` are one query here and `sortCell`
    orders by the LETTER. The renderer's `tokenTest` does NOT fold, so a
    locally-filtered page answers `priority:A` with nothing — the narrower
    direction, which the tripwire blesses.
  - COLUMN ORDER: `state | priority | title | scheduled | deadline | tag`. Tags
    LAST because org writes them flush right. The reorder is the one-list edit —
    `columns`, `rowJSON`'s cells, `filterKeys` and `viewCells`→`hrSearch` all
    follow `viewColumns`, and `tagsColumn` is the INDEX of `tag` computed by
    NAME. What did not follow, by design, is `TestFilter`'s hardcoded layout
    oracle: it moves by hand, which is the whole reason it exists.
  - Column lockstep is FOUR-way through `viewColumns` — `columns` declares,
    `rowJSON` fills, `filterKeys` names, `viewCells` joins into `hrSearch`. A
    cell is `HeadlineRecord -> Maybe Text`: `Nothing` is the row JSON's `null`
    and the empty field a filter reads. `TestFilter`'s layout guard keeps its
    hardcoded six-cell list ON PURPOSE — an INDEPENDENT ORACLE rather than a
    mirror. What used to go green was the APPEND; a REORDER was already caught by
    the predicate cases. The append is closed by construction now, plus a case
    quantified over the columns there are. What still moves by hand is
    `Filter.dateKeys` and `keyTest`'s name switch, neither positional.
  - Which column holds a LIST is chosen by NAME here (`tagsColumn`) and DECLARED
    to the renderer: the `tag` column emits `"multi": true`, which beats its own
    sampling (`multiColumn` over ≤40 non-empty cells, ≥2 tag-shaped and none
    contrary). What rides on it: the whole-tag meta, chip rendering, the value
    domain.
  - Date-ness is likewise asymmetric: two hardcoded names here, sampled
    date-shape there. A page with under two dated rows makes the renderer
    substring-match `scheduled:` where the server prefix-matches it — and, since
    `planned` reads WHICH columns are dates, answers `planned:` over no columns
    at all. The predicate is term-for-term; the column set under it is not.
  - `ref:ROWID` is producer-only WHOLE: undecidable from the rows a renderer
    holds, so `table-view.js` reads it as FREE TEXT — narrower, the tripwire's
    blessed direction. What keeps this workable is that no locally-filtered path
    applies `ref:`.
  - `state:*active*`/`*inactive*` are producer-only in their KEYWORD half alone,
    blessed by SCHEMA.md, and are the canonical spelling. The renderer matches
    them as literal badge text EXCEPT for `*active*`'s empty-cell term, which
    names no keyword. The bare `state:active` is a literal on BOTH sides. The
    `state` column ships the two as `values` beside its `badges`, dimmed and
    uncounted. Each badge also names its `group` (`active`/`inactive`) — order
    cannot say where a `#+TODO:` bar fell and the hues are not a contract.
  - Keys are matched case-sensitively on BOTH sides and every real key is
    lowercase. Values are folded on both.
  - Separators are exactly `&`, space, tab and newline. `\r` is not one.
  - A bare `-` is a negated empty free-text term, and an empty term matches
    everything, so a lone hyphen empties the result set. Both sides agree.
  - `key:value` splits on the FIRST `:` or `=`, so `tag:a=b` is key `tag`, value
    `a=b`; a body opening with a separator has no key.
- The served pages fetch nothing off this server: inline styles, inline glue, and
  `<script src>` the asset route answers out of the binary. No CDN, no web font,
  no analytics. The JetBrains Mono `@font-face` appears only when an `--assets`
  directory holds the file.
- The renderer is COMPILED IN: `embeddedRenderer` = a TH splice over the
  committed `assets/table-view.js`, so the binary is the whole deployment.
  `--assets` REPLACES it (dev flag) — the named directory is then the whole asset
  set, which is what keeps `assetsMissing` reachable. `assetSource` is where the
  two meet; both leave by one door, so content type and gzip are identical.
  `make sync-renderer` copies from `../table-view/web`.
- NO SOURCE FILE names an absolute path outside the repo. `TestSelfContained`
  sweeps every `.hs` under `src*/` and `app/` for `/home/`, and asserts what it
  swept first so an empty sweep cannot pass.
- ONE WIDGET, ONE FILE: the shell is `assets/glue/*.js`, concatenated by the TH
  splice in the order `Glance.Web.Base.gluePartFiles` declares (ORDER IS DATA,
  stated once). The parts are FRAGMENTS of one script scope rather than modules
  — the script has no wrapper, every top-level name is a script-scope binding —
  so the join is plain concatenation, `tsc` resolves across the parts by
  listing them all, and the split was proven byte-identical against the file it
  came from. THE PARTS ARE THE ONLY SOURCE — no whole `glue.js` in the repo, so
  there is no second copy to keep in step, and `TestSelfContained` asserts both
  directions (every part the build names is on disk, nothing on disk is
  unread). `--assets DIR` takes either shape: `DIR/glue/` joined per request, a
  whole `DIR/glue.js` otherwise.
- COMMENTS EARN THEIR LINE. The shell runs ~6% comments, down from 49%: the
  code is meant to read without them, and every rule they used to restate is
  written here or in `docs/invariants.md`, where one copy can be kept true. A
  comment survives where the code cannot say it — an ordering constraint, a
  browser quirk, a hazard, a cross-reference — or where `tsc` reads it.
- `make check-glue` READS THE SHELL, which it did not until 2026-08-08: `checkJs`
  without `allowJs` excludes every `.js` from the program, so tsc was type-checking
  the five-line `glue.d.ts` and reporting clean. `files:` rather than `include:`
  (the Go tsc resolves the two differently) plus `allowJs`. What it checks is
  `strictNullChecks` off `el`'s control-union cast and FOUR declared model
  shapes — `LayerRow`, `StateRow`, `ViewRow`, `PropRow`, plus `Surface` — so a
  field a server answer stops sending fails at the annotation rather than
  reading `undefined` at the point of use. What it does NOT check is element
  KIND (`el` casts) and implicit `any` (~570 sites); both are open ratchets.
- The shell is vanilla JS with no framework or dependency — a real file,
  `assets/glue.js`, compiled in the way the renderer is; the page inlines two
  JSON blobs (keymap, the `cfg` the script reads as `CFG`) and the theme boot
  line, and names its two scripts in src tags. `--assets` replaces the whole set.
  The shell has no build step — `cabal build` was always the build — and
  shrinking it beats adding to it.
- The BOOT — and only the boot — asks `?limit=100` and pulls the rest in behind
  the painted table; it mounts with `onFilter` so the server narrows, and opens
  its socket with `?bootstrap=off`. With no `q` in the URL the boot query is the
  default view, applied as a real query: written into the URL through `remember`,
  mounted as `initialQuery`, and asked of the server, so `DEL` strips it like any
  other token. A `q` that IS in the URL is the reader's intent, an empty one
  included, and nothing is injected over it.
- Rows are virtualized and shown a page at a time (`pageSize` = the boot's
  `limit`), so a row step is `selectStep(±1)` — the page boundary is the
  renderer's — and `[`/`]` turn a page. `getVisible()` is that page, so the
  buffer-end keys reach its ends, PROGRESSIVELY: `<`/`>` take the page's end row,
  and pressed again turn a page and land on the same end (`endStop`), stopping at
  page one's first and the last page's last. Each climb re-selects, since the
  renderer lands a turn at the end it arrives at, and the column comes back out
  of `column()` rather than a local.
- Cell movement (`f`/`b`, `l`/`h`) walks OFF the cells rather than bumping: the
  renderer reads a column index outside the table as no column (`cellCol`), so
  `moveCol` hands the out-of-range step straight to `select(id, want)` and the
  answer is the whole-row look. The clamp this page used to keep swallowed the
  key at a wall the renderer does not have, and the glue guard forbids those
  strings. Re-entry is `at === null ? 0 : at + step`, and the landing column is
  read back out of `column()`.
- One fetch is in flight at a time: a single `AbortController`, aborted and
  replaced by whoever asks next, so the background full-set pull yields to a
  filter commit. A late paint is guarded by the query it was asked for. A boot
  that was filtered chains one more fetch, `arm(total)`: the unfiltered set, kept
  as the parity baseline without being painted, once per page.
- With a filter applied, a socket frame does not splice — it schedules a refetch
  250 ms out, coalescing a burst into one request. Unfiltered frames splice.
- A VIEW SWAPS ON ITS ANSWER. The table on screen stands until the new rows are
  in hand and then goes in ONE mount — the count never passes through zero or a
  partial set unless the answer is empty. That decides the fetch: a boot
  (`!table`) takes `?limit=100` plus the pull behind it; a re-application (`g`,
  `a`, `@`, pop, a `view-changed` remount) asks for the WHOLE answer once, since
  a page-sized mount there replaced a complete table with a hundred rows.
- THE STALE WASH: one mechanism, two triggers. One class (`stale`) on the
  document element dims `#app` and the whole modal band to `opacity:.55`, eased
  180 ms. One property, and NEVER a `filter` of any kind: a filter makes its
  element the containing block for `position:fixed` descendants, and the
  renderer's palette backdrop is one — it would stop covering the viewport and be
  clipped. No blur either: a stale row is still the row. The log strip and the
  key line are exempt by omission: they explain the state. Triggers: a view fetch
  in flight past 300 ms, and a socket down past 400 ms. One holder (`wash`)
  carries a count, a timer and an on-flag per reason; the view reason is STEPPED
  (an abort overlaps the fetch that replaced it) and the socket's is SET.
  `viewing` marks the fetches whose answer replaces the rows. The page never
  reads the class back.
- Shell z-indexes are three: echo `2`, modal backdrop `100`, sheet `101`. `3` was
  the corner's and the suite forbids the value. Every overlay shares that pair
  with the sheet, so the three values stand whatever else is added. The
  cross-repo constraint is the backdrop pair clearing the renderer's sticky
  header (`1`) and completion list (`5`); the echo sits below both on purpose.
  The filter palette carries no shell z-index at all.
- THE PALETTE IS ONE SOURCE, and a theme is one file. `Glance.Web.Theme` holds a
  `Palette` of ROLES per theme (`Theme/Default.hs`; `Theme/Types.hs` carries the
  type so a theme file names no registry) and `themeCSS` emits it into BOTH
  namespaces — the page's `--g-*` and the renderer's `--tv-*` — for the two
  system defaults and for every theme by name. So a role both spell (ground, ink,
  surface, muted, hairline, accent, selection, hover, link, crosshair, veil,
  shadow, cell wash, and `--g-bad` = `--tv-flag`, one red) has ONE value, and the
  hand-copied literals that kept the two in step are gone. WHOSE VALUE WINS is
  the renderer's own doing: it ships its palette blocks at ZERO specificity
  (`:where(.tv-root)`), so the page's ordinary rules override them whatever order
  the two stylesheets land in — the renderer injects its own into `<head>` at
  mount time, after the served page's. A NEW THEME is a record beside the default
  theme's plus a `themes` entry; the CSS blocks, the boot script's id test
  (`themeIds`) and the settings selector all read that list — `#themesel` is one
  option per entry under `auto`, which names the media query rather than a
  palette, and `TestServe` derives that oracle off `themes` so a hard-coded
  option fails. What stays out of a
  theme is GEOMETRY (`--g-doc-*`, `--g-pop-*`). `TestServe.paletteSweep` is the
  DERIVED oracle: it reads the served page and compares the two namespaces role
  by role, so agreement is asserted rather than mirrored.
- A BADGE HUE IS THE THEME'S, and the wire carries a SLOT rather than a colour:
  `badges` emits `var(--g-state-a<i>)` / `-i<i>`, `priorityBadges`
  `var(--g-priority-<i>)`, and the theme declares each slot. A theme is switched
  CLIENT-SIDE with no refetch, so a baked hue would go stale on the keystroke —
  the `var()` is what makes the palette follow. `Glance.Query.stateSlots` is the
  slot count and the WIRE's, the same for every theme; `Theme.slots` cycles a
  theme's own list to fill exactly that many, so a slot the badges name is
  always declared. The hue is also the INK: a pill draws its colour as text over
  a 15% wash of itself, that wash composites over whatever ground the ROW wears,
  and the renderer's `inkFor` cannot darken a `var()` — so a theme picks hues
  that read over its own `pBg` AND its `pSelection`. That is the collision this
  rule exists for: a light theme with a golden cursor row cannot use the
  mid-tone amber a white-only ground would take. A theme spells AS MANY OR AS
  FEW hues as it likes: `Theme.slots` cycles the list to fill the wire's count,
  so fewer repeat, more are never reached, and an empty list falls back to the
  theme's ink rather than leaving a token undefined and a pill unpainted.
  `paletteSweep` reads the slots the served ROWS name, asserts every theme
  declares each, and counts the slots rather than the hues behind them.
- AND A TREE NAMES ITS OWN, per keyword and per theme:
  `#+GLANCE_STATE_COLORS: light TODO=#7B1FA2 DONE=#00695C` in `system.org`,
  the theme first and `KEYWORD=VALUE` pairs after. EVERY such line is read (one
  per theme is the shape) and a keyword named twice takes its LAST spelling.
  The mechanism is a CSS FALLBACK CHAIN, so this costs the wire nothing:
  `badges` emits `var(--g-state-TODO, var(--g-state-a0))` and the config
  declares `--g-state-TODO` where it has an opinion. `stateColorsOf` validates
  only the SHAPE — a theme no build carries emits a block nothing reads, and a
  value that is not a colour is one CSS ignores, both being the author's
  business. The system layer's alone, like the other tree-wide settings, and
  emitted per REQUEST after `themeCSS` (it comes off the store's config, not
  out of the build).
- Renderer internals this page may touch are enumerated by the suite as
  must-not-appear lists: the shell may not name `closeFilter`, `.tv-veil`,
  `.tv-panel`, may not reach rows by `tr.click()`, `scrollIntoView` or `rowEls(`,
  and may not keep a column of its own. What it does style is `.tv-root`'s font,
  `.tv-chips`/`.tv-chip` under a coarse pointer, and the selected-row fallback.
- Every optional renderer capability is feature-detected before use —
  `parseQuery`, `stripLastToken` with `getQuery`, `selectStep`, `nextPage` with
  `pageInfo`, `getSelection`, `openFilter`, `sortPromote`, plus `matchMedia`.
  `initialQuery` is passed unguarded and detected afterwards by asking
  `getQuery()` whether it took. An asset with no `sortPromote` costs the ORDER
  alone.
- The page never scrolls: `body` is `100vh`, `overflow:hidden`, a flex column of
  table, log and key line. The log grows to its capped share and scrolls inside
  itself, the table takes what it gives up, the key line is `flex:none` and
  scrolls sideways. A long message moves nothing.
- The log strip is append-only and its whole interface is `append(scope,
  severity, message)`. A line is `HH:MM:SS SEV scope message` — severity
  `info`/`warn`/`error`, SPELLED uppercase and WORN lowercase as the line's
  class, the upcase at the one place the word is drawn; scope one of `ws`,
  `sync`, `cmd`, `filter`, `config`, `boot`; control characters collapse to
  spaces. Nothing clears it. The ring holds 500 (`LOGCAP`, lines KEPT) and drops
  the OLDEST; a line identical to the one before bumps a `×N` counter, the only
  mutation. The end is scrolled to unless the reader has scrolled up. Every write
  names its rows, one line per ROW, with the title read through `displayText` and
  the id as the fallback; refusals stay one `cmd error` line.
- The log's HEIGHT is the page's second `localStorage` preference (`glance-log`
  beside `glance-theme`), applied on boot and on every accepted keystroke. The
  stylesheet keeps the arithmetic and declares the default; the knob writes a
  NUMBER onto the element, so the formula is in one place and a page whose glue
  never ran is capped at the same figure. `LOG = {key, def:7, min:1, max:50}`,
  mirrored in Haskell as `logLinesDefault`/`Min`/`Max` and `logLinesBand`. Blank
  is the default and REMOVES the key; a whole number in the band is that number;
  everything else is DECLINED rather than clamped, and reopening the sheet draws
  the preference back over a refused value. The field is `#clog`, applied on
  `input` rather than `change` so it is a knob rather than a form; `cmoved` never
  sees it, so it costs no request and cannot dirty a pristine sheet.
- Every touch-device rule lives in ONE `@media (pointer:coarse)` block.
- A client whose mailbox fills is closed with reason `resync`; a column change
  closes with `view-changed`. Those two strings are the whole vocabulary of a
  server-initiated close. Only `view-changed` remounts. Everything else
  revalidates `/headlines` for the applied query against the tag the last answer
  carried (`If-None-Match`, `cache: no-store`), re-attaches, and keeps the mount
  — sheet, palette, selection, URL. 304 means the rows still stand; 200 replaces
  them in place. A 200 also compares the fetched columns to the mounted ones
  (whole, by `JSON.stringify`, since the badge palette rides inside them) and
  remounts when they differ: a daemon restarted while the page was away had no
  socket to send `view-changed` down. Across a real remount the shell stashes and
  restores a dirty sheet and the palette's typed text, re-reading the sheet's
  digest with a `GET` so a moved file lands at `conflict`.
- The parity tripwire is loose in one direction: it fires only when the server
  returns zero, the local recount drops the key and tests the value against the
  whole row text, and it consults column keys alone. It reports a suspicion and
  corrects nothing. Its baseline is a remembered unfiltered paint, and a boot
  that had none arms it with `arm`'s own unfiltered fetch.
- The shell's keymap is `Glance.Web.Keymap`'s `keyBindings` and nothing else —
  ONE map, no profiles: the page carries it as a JSON blob (`{rows, hints,
  reserved, once}`) and its own dispatch parses that blob. Each row carries
  `kbKeys`, `kbCommand`, `kbScope` (`table`, `modal`, `any`) and an optional
  `kbHelp`; `seq` is derived, never stored. Movement carries BOTH spellings —
  `n`/`p` and `j`/`k` step a row, `f`/`b` and `l`/`h` step a cell. The ARROWS
  ride both axes and SILENTLY, so the key line still reads `n/p rows · f/b
  cells`. Ends are `<` and `>`, plus vi's `G`. `^` is `toggle-sort` and it is a
  QUERY EDIT: the renderer composes the chain, writes it into the applied query
  as ONE arrow-form `sort:` token and delivers it, so the press arrives as an
  ordinary commit and `DEL` takes the chip WHOLE. What it composes onto is the
  chain IN FORCE. This page keeps no record of an order and asks for none.
- `g` is `apply-default-filter`, `P` is `set-default-view` (ONCE), `a` is
  `org-glance-agenda`, `,` is `customize`, `:` is `org-agenda-set-tags`, `o` and
  `!` are `org-glance-overview:open`, `@` is `org-glance-overview:relations`, `M`
  is `mark-all`, `d` is `archive-flag`, `D` is `org-glance-overview:delete` (both
  over FLAGS, never marks). No sequence is bound twice or opens a longer one.
  Sequences and command names are org-glance's where org-glance has one.
- A LETTER BINDING NAMES A PHYSICAL KEY, and the split is `keyName`'s alone — the
  one function every listener names a press through. `e.code` matching
  `KeyA`–`KeyZ` answers as that letter, lowercase, and `shiftKey` as the
  UPPERCASE binding rather than an `S-` modifier, which keeps `d` and `D` two
  rows; a chord's second key comes through the same door. Everything else is the
  CHARACTER `e.key` reports — named keys, function keys, and PUNCTUATION, which
  sits at a different position on every layout. A press carrying no `code` falls
  back whole. Consequences: the map is QWERTY's POSITIONS, so a Latin layout that
  moves its letters reads its own `a` as this map's `q`; and a layout spelling no
  `<` or `[` cannot reach the punctuation half.
- `RESERVED` = `C-l`, `C-r`, `C-t`, `C-w`, `C-n`, `C-p`, `<f5>`: a reserved key
  reaches the browser UNLESS it completes a bound sequence. What the list buys is
  the abandoned prefix. That rule is the PAGE's half and is all a page has:
  Chromium handles `Ctrl+T`/`N`/`W` above the document, so `C-c C-t` is dead in
  the browser however correctly it is dispatched. `C-x C-s` works because
  `Ctrl+S` is a page default action. Prefix opening is guarded by `selecting()`,
  one predicate over the focused field's range and the document selection,
  covering every prefix.
- Auto-repeat is movement's — a held `n` crosses the table — so keys that must
  run once per press are named by COMMAND in `ONCE` (`filter-drop-token`,
  `unmark-all`, `mark-all`, `archive-flag`, `org-glance-overview:delete`), which
  holds under both spellings, plus `org-glance-overview:open` and
  `org-glance-agenda`, which are ruinous held down. `archive-flag` needs it most:
  a surviving repeat would flag a row and archive it from ONE press.
- Seven keys write without a sheet, all `POST /command`, and WHICH ROWS is per
  command. `t`/`C-c C-t`, `:` and `C-c C-s`/`C-c C-d` take the MARKED set when
  there is one and the row at point otherwise — dired's rule. `D` and `d` take
  the FLAGGED set instead and never read marks: a mark is what a reader lays down
  to set a state over a run of rows, and letting the archive key inherit one
  makes every mark a loaded gun. `+` takes NO rows. Every set is the renderer's,
  asked for AT command time; no set is kept here.
- `C-c C-s` and `C-c C-d` raise the value palette in its TEXT mode (`askText`):
  same overlay, same band, same `unask`, same ESC through `cancel`, with
  `prompting.text` set — no list, no letters, RET commits the line as typed. Both
  send the line as `date`, and an EMPTY line is the null that clears the entry.
  Both reach the page where `C-c C-t` does not.
- `+` IS ONE FORM (`#capture`/`#kbox`, its own `SURFACES` entry): the tag field
  with the tree's vocabulary narrowing under it (substring over the folded
  spelling, at most eight, `C-n`/`C-p` and the arrows walking a highlight RET
  takes; no highlight commits the field as typed, so the charset wall stays the
  server's), then one field per `%^{PROMPT}` grown IN PLACE when the tag settles
  (only the server knows the prompts, and editing the tag afterwards clears the
  grown fields), then the line. RET moves the focus forward and at the line
  captures; an EMPTY tag settles to the untagged inbox path; ESC is the keymap's
  `cancel` through `SURFACES`. A refusal keeps the form UP with everything typed:
  `shutCapture` runs on the 200 alone. This page holds no template grammar. The
  form's keys are a document listener behind the dispatch, gated on the focused
  field.
- A CAPTURE SAYS WHERE POINT IS OWED, and `arriving`/`arrived()` is `leaving`'s
  mirror: the answer names the row the write made, and the same three doors that
  spend the archive's anchor spend this one. It is `land`'s ordinary rule asked
  ONLY where there is something to land on. Both are dropped by a commit and by a
  remount: an anchor belongs to its view. KNOWN LIMIT: it is spent at the FIRST
  door, so an unrelated watch step landing between the 200 and the delivery
  spends it.
- `d` is dired's FLAG and dired's `dd`, in two presses: the first flags the row
  at point, and a second `d` on an already-flagged row IS `D` — it calls the same
  handler, so it archives EVERY flagged row. A lone flag is a set of one. There
  is no sequence machinery: `d` stays one complete binding. `D` is that handler
  without the flagging press — every flagged row when there is one, the row at
  point otherwise — giving the set name up for the bare count when nothing
  landed, since a set name over zero rows reads as a write that worked. `D`
  SPENDS the flags it fired over: `setRows` keeps a flag whose row a filter is
  hiding, so a set left standing would be archived again by the next press. The
  flag is the confirmation, so there is no prompt; `u` on a flagged row takes the
  flag off before it touches a mark, and `U` clears flags with marks. Flags are
  the RENDERER's session state, keyed by id like marks. The pair is
  feature-detected: an asset predating it echoes and writes nothing.
- ONE `d`/`D`/`u` GESTURE OVER THREE SURFACES — table, property panel, tags popup
  — and `flagKey` is the whole of it: the cursor read, the two-press rule, the
  set-or-row choice, the spending of the flags, the feature refusal and the walk
  after `u`. A surface DECLARES a shape: its mount, its cursor as an id, what
  "take these" means, what it LOGS, and four PHRASES. WHO SPEAKS belongs to the
  caller — the popups say `KEY → phrase`, the table says it through `said`, which
  spells the binding's own command name and brackets the phrase. The CURSOR is
  asked for before the FLAGS: `D` means "take these" and a lone row is a set of
  one, so it lands on a mount whose renderer never had flags, while the two
  presses that MOVE a flag are what the refusal is for. `HOW` words the pill and
  is a function of what LANDED. `u`'s flag-before-mark stays in `mark`: over the
  popups `u` is the flag key, over the table it is the MARK key preferring a
  flag.
- `t`/`C-c C-t` raise a value palette of the shell's OWN, showing the RESOLVER'S
  TRUTH: `GET /keywords?ids=…` answers with the classification chain behind those
  rows, drawn as a table — Source | Active | Inactive, one row per source in
  precedence order, `*empty*` spanning a row at the foot — FOUR sources at most,
  no `union` row. The keywords are the server's, never the state column's
  `badges` (a superset saying nothing about origin) and never its `values`; only
  the HUES are read off the badges. What it shows IS what is settable. It is
  WHICH-KEY: every entry wears a letter and that letter commits on its own, since
  the palette IS the confirmation. No `RET` in letter mode, no confirm step; the
  drift lock is the safety. `/` falls back to the completing-read — the table
  FLATTENS, a field appears, typing narrows, `C-n`/`C-p` and the arrows walk,
  `RET` commits — and is entered, never left; `ESC` is the one door out of
  either. Both modes commit through one `takeChoice`. The overlay goes up on the
  KEYDOWN and the answer fills it, so the raising guard, `typing()` and `ESC` are
  where they were; a fill landing after the reader left drops. Its keys live in a
  SECOND document listener behind the dispatch. `typing()` — which the palette
  turns on with NO field focused — kills every `table` row. What holds the
  SHEET's own listener off is `momentary()`, since the sheet's listener runs
  AHEAD of the dispatch. TWO GUARDS, one press each: `prompting.raising` declines
  the keydown that OPENED the palette, and `e.repeat` stops a HELD `t` committing
  through what it opened — `ONCE` cannot reach it.
- AN EDIT OVERLAY NAMES ITS CELLS BY KEY. A shape carries `cells: ["title",
  "url"]` beside the `cols` list the SERVER declared, and `cellSpan` resolves the
  keys to the leftmost and rightmost indices the placement reads — pure and
  order-only, so a column list that moves takes the box with it. The run is the
  COLUMNS' order, a box being drawn edge to edge. An unknown key resolves to
  nothing and the placement is a NO-OP: the box stays rather than covering the
  wrong cells. The property panel names no cells and takes the whole row.
- THE MODAL SURFACES ARE ONE LIST, `SURFACES`, in the order written: the value
  palette, the link popup, the tags popup, then the sheet. The first three are
  `momentary`; the sheet is the floor under them. Each entry names its `up`, the
  `off` that closes it, and the OPEN EDIT that is a rung under it. FOUR READERS:
  `momentary()` names whichever momentary one is up, `typing()` asks whether ANY
  is up, `sole()` closes every momentary one on a raise, and `cancel` walks it
  for the rung ESC belongs to. The five listeners STAY, and so does
  `prompting.raising`. ORDER IS LOAD-BEARING FOR EXACTLY ONE PAIR: `+` over the
  tags popup leaves both up, and `momentary()` resolves that tie by list
  position, so swapping them makes the tags listener eat the add-field's letters.
- `:` raises the TAGS POPUP, the page's FOURTH table-view mount and the only
  MUTABLE one. A tag over a set of rows is a RECORD — a name, a coverage, a
  weight — so it takes the link popup's shape rather than the palette's. Columns
  are `tagColumns`: `title` (the tag, keyed the link popup's way — a column keyed
  `tag` would invite the multi-value sampling), `on` (coverage, `all` or `k/n`)
  and `rows` (the store-wide count). Rows are the UNION over the target rows,
  FIRST-SEEN, since an alphabetical insert in the middle would move the row out
  from under the cursor. A tag IS its row's id. Raised LATE, behind the fetch;
  `:` is no key inside the list it opens, so no raising guard is owed. Mounted
  with `marks: false` and `flags: true`. The popup STAYS up under every write,
  refreshing the list from the command's OWN per-id answer, never a re-read —
  `/command` does not write the store, so asking `/tags` again would report what
  the files said BEFORE it. `d`/`D`/`u` are dired's gesture verbatim; `D` removes
  EVERY flagged tag from every target CARRYING it and SPENDS the flags. `+`
  raises the value palette straight into its field over the ADDABLE vocabulary
  (the tree's LESS the tags every target already carries, partial ones leading
  with their `2/3`), and RET commits the highlighted entry or the line as typed.
  `RET` is the RENAME, through the property panel's edit model over ONE cell.
  A tag is FOLDED at commit, since presence is. Its keys are a document listener
  behind the dispatch, with two guards about the palette `+` raises OVER it: it
  runs only while `momentary()` names it, and it declines a key that palette has
  already CLAIMED (`e.defaultPrevented`) — without the second, the very RET that
  added a tag would open the rename.
- `o`/`!` FOLLOW the row, and the ANSWER decides the gesture: `GET /links?id=`
  for the row at point, then no links is an echo refusal, ONE is
  `window.open(target, "_blank", "noopener")`, and SEVERAL raise the popup. Every
  open writes a `cmd` line naming the target. WHICH rows have one is on screen
  ahead of the press: `linked` underlines the title, over every link `/links`
  would report rather than the ones a tab can take. A tab can be pointed at
  `http`/`https` and NOTHING ELSE (`followable`); a non-followable target is one
  `cmd` WARN line, truncated at 80 characters, plus the same words in the echo,
  and no tab. The judgement lives in `openLink`: the popup still LISTS every
  link, since that is what teaches a reader what the entry holds, and the COMMIT
  is where the answer is given.
- `RET` over the link popup EDITS the link at point in place: the title and url
  cells become fields over themselves, `TAB` hops, `RET` commits `edit-link` over
  the SPAN `/links` handed out under the digest that answer carried, `ESC`
  restores. The page holds no bracket grammar and no offsets of its own. The
  untouched FIELD is what makes absent-not-null reachable. Both fields are
  TRIMMED on the way out: padding is the field's, and the server writes a
  description verbatim and refuses a padded target. The popup CLOSES on the
  press, both outcomes alike — forced, since the spans describe a file the write
  has just moved. KNOWN CONSEQUENCE: a row with exactly ONE link is followed
  rather than listed, so that link has no editor.
- `a` is a canned VIEW, not a mode: `state:*active* -planned:*empty*
  sort:scheduled` through `applyView`, the door `g` uses. No agenda state
  anywhere; `g` is the way home. The order is a token rather than a call behind
  the answer, so the whole view is one string. What arrives through `landed` — a
  one-shot thunk `start` TAKES before it fetches — is the ECHO, called with the
  SERVER's match count.
- Letters are `whichKeys(labels)`: over the labels flattened in DRAW order, each
  entry takes the INDEX of the first letter of its OWN spelling, downcased, that
  no earlier entry claimed — one `a`–`z` pool, `-1` for none left. Pure and
  order-only, so a tree's cycle always yields the same letters, and `default`
  leading the draw gives `TODO` `t` and `DONE` `d` in every tree. One pool over
  the WHOLE table. `*empty*` is OUT of the pool: it answers to `DEL`, so the
  `a`–`z` namespace is spent on KEYWORDS alone. `offer` decides that by the entry
  carrying a key of its OWN (`fixed`) rather than by its being the meta.
  `setChoices` folds the letter into each entry once, so drawing and dispatch
  read ONE FIELD instead of agreeing on a parallel array's indices. Display
  teaches why, and there is NO key-token column: the claimed letter is marked
  INSIDE the keyword, bold and underlined, taking that state's own badge hue; an
  entry that claimed nothing is drawn BARE and reachable through `/` alone. ONE
  entry keeps a token, `*empty*`: `DEL` names no position in a word to mark.
- Row marks are the RENDERER's, behind `marks: true`: it draws the checkbox
  column, keys the marks by id and counts them, so a mark survives a `setRows`, a
  filter that hides its row and a page it is not on, and this page keeps no set
  of its own. dired's: `m` toggles and takes the renderer's word for where it
  landed, `u` toggles and puts back anything it just laid down, both then
  `selectStep(+1)`, `U` clears, `M` is `markAll()` — the renderer's call because
  the SET is. `m`/`u` stay out of `ONCE` because the walk IS the feature.
- The mount passes `actionHints: false`: the renderer's per-row hint said RET
  materializes, which the resident key line already says for every command.
- STARRED METAS, and the family is TOTAL: `*word*` marks a value with semantics
  of its own — never a literal keyword, never a cell value — and NO BARE WORD IS
  RESERVED, so every spelling a cell can hold is reachable as itself. The family:
  `*empty*` (the empty cell, EVERY column key and `planned`), `*archive*` (the
  whole ARCHIVE tag on `tag`, the one query that lifts `/headlines`'s exclusion),
  `*active*`/`*inactive*` (producer-evaluated), and `*none*` (the ORDER's empty
  chain, under `sort:` alone, the one meta naming no cell). The state palette's
  take-the-keyword-off entry is `*empty*` too. `*active*` is the file's active
  keywords PLUS the EMPTY state cell — a stateless entry is live work — while
  `*inactive*` is stated keywords alone, so the two do not partition the column.
  A future meta joins by wearing the stars. The enforcing edge is `setStateEdits`
  refusing any word a file's `#+TODO:` does not declare, and `keywordTextP`
  (letters and underscores) making a starred word undeclarable. On the tag side
  it is `isTagChar`, which has no `*`.
- Browser writes are commands over the bridge: structured ones and drift-locked
  raw replacement. Semantic org editing — refile, agenda logic — stays out of the
  browser. Automation = reviewed deterministic scripts, no LLM in the loop.

## UI

- MOVEMENT NEVER CHANGES CONTEXT. `n`/`p`, `f`/`b` and the grain relocate
  attention alone: they never open, close, commit, or cross a boundary a reader
  would have to come back out of. `RET` and `DEL` are the context axis — `RET`
  goes deeper, `DEL` comes back out. A key that both moved and switched would
  make every press a risk to weigh; the split is why movement keys are the ones
  left OUT of `ONCE`. Stated in full in `docs/design-rhymes.md`.
- Keyboard-first: every web-surface feature ships with a key path mirroring the
  Emacs org-glance maps; buttons only where keys cannot reach; the echo widget
  must know every new binding.
- Commands are named as elisp functions and the ECHO speaks them verbatim:
  `SEQ → command`, with anything else in brackets after it — never the prose
  spelling, since the rebinding config to come will address a function by exactly
  this string. One helper emits the shape (`said(b, what)`). The resident key
  line is the exception on purpose: curated prose naming a group.
- ONE BUTTONLESS SHEET, and there are two of them: the materialize sheet and the
  settings sheet run the SAME ladder, written once (`saveSheet`, `leaveSheet`,
  `note`) over a sheet object holding `{dirty, flush, refresh, shut, scope}` and
  its own state word. `activeSheet()` is what either key asks, and it is total
  because neither opens over the other. What differs stays in the verbs: the
  subtree's flush is one `POST /headline`, the settings' is a POST per moved
  layer. Dirty = either pane vs the materialized original; ESC or the backdrop
  flushes a dirty sheet and closes on the 200, a pristine one closes with no
  request; `C-x C-s` flushes mid-edit and chains the receipt's digest; a 409
  keeps it open at `conflict`, where `C-x C-s` is `refresh()` then `flush()` and
  ESC discards; `beforeunload` flushes with `keepalive` only when dirty. Header
  states: `synced` / `syncing…` / `conflict` / `error` — the last two wait for a
  keystroke, so each spells the key that clears it.
- TWO KEYS COMMIT AN OPEN ELEMENT: `C-x C-s` (`save-buffer`) and org's `C-c C-c`,
  over the paragraph textarea and the two-field overlay alike, `RET` keeping its
  landed meanings. `C-x C-s` keeps the half that is a BUFFER's — with nothing
  open it flushes the sheet and on a conflict it overwrites — where `C-c C-c`
  stops where the element does. `commitDocEdit` takes the binding that fired, so
  the echo names the command that ran. `Ctrl+C` reaches the page, and COPY is
  untouched because prefix opening is guarded by `selecting()`.
- The sheet is two panes over one subtree and the cut is the SERVER's: STRUCTURED
  DOCUMENT = `body`, panel = `properties` + `planning`, a flush posts both back.
  The page holds no org parser and must not grow one. A panel row is key then
  value in file order (no `tabindex` anywhere); `+` adds one and `d`/`D` delete
  one; an emptied key deletes too; the hidden properties are not rowed at all.
  `C-c '` swaps two-pane and raw org by RE-MATERIALIZING — a dirty sheet is
  refused with `sync first — C-x C-s`, since a local conversion would need the
  parser this keeps out. Stash and restore carry both panes, the shape, where the
  cursor stood and what an open edit was holding — and only for a DIRTY sheet.
- POPUP SIZE IS A TIER and there are TWO: `.pop-band` (a narrow column that grows
  with its content to the cap — the state palette, and the tag manager, whose
  three short columns are narrower than the palette) and `.pop-sheet` (a working
  surface FIXED on both axes — the materialize sheet, the link popup, the capture
  form, the settings sheet). No box declares a width or a height of its own.
- A POPUP CLAMPS AND SCROLLS INSIDE, as a CHAIN: `--g-pop-max` is
  `min(90vh, calc(100vh - 2 * var(--g-pop-top)))` — the foot margin is the
  HEAD's, derived from the anchor rather than spelled as a second figure.
  `#mpanes` carries `overflow:hidden` and no PANE carries a floor, a
  `min-height` on a flex child being a refusal to shrink. `#mdoc` owns its
  scroll; the mounts inside `#mprops`/`#lpane` own theirs.
- THE LEFT PANE IS THE STRUCTURED DOCUMENT, and it is NOT a table-view mount —
  the doctrine line: the renderer's list widget draws a list of RECORDS, one
  shape per row, and this is a list of KINDS. Elements in file order: the
  HEADLINE LINE (cells `state | priority | title | tags`), the body's own
  PARAGRAPHS (blank-line separated, each remembering its line range), and the
  CHILD headlines collapsed to one line each. `drows` is the model, `drawDoc` the
  whole view.
- MOVEMENT IS TWO AXES, the table's habit read into the document. A LIST, a
  `#+begin_X`/`#+end_X` BLOCK and an org TABLE each take TWO kinds of stop over
  the same bytes, laid out as `[whole, leaf1..leafN]` and inline among everything
  else. `n`/`p` step SIBLINGS at the cursor's grain and never dive (a composite is
  ONE stop; a leaf steps its owner's run, clamped at its ends), and `f`/`b` move
  the GRAIN — a LADDER: a list item carrying a nested run is itself a parent, `f`
  descends one rung (refusing with an echo at the finest), `b` climbs one to the
  IMMEDIATE owner by id and back to the whole line in one press whatever the
  column, a no-op with an echo at the element grain, NEVER a close. `l`/`h` and
  the horizontal arrows stay the within-grain cell walk. `RET` is pure edit at
  either grain, `DEL` stays the sheet's ladder, `d` flags whatever the stop is.
  `grain` names the kind: `element`, `composite`, `leaf`.
- ONE GRAIN SPEAKS FOR A RANGE. A composite and its leaves cover the same lines,
  so `bodyText` leaves a leaf out of the splice whenever its owner MOVED or is
  going — a reader flagging a list and one of its items gets one deletion. A
  composite is DRAWN once with its leaves inside it, and what no leaf claims is
  drawn INERT (`.dg`, muted). Every byte on screen exactly once.
- A TABLE'S LEAF IS A LINE, the one place the table grain differs from the
  list's: a list's leaves are RUNS found by `listRun`, a table's are cut inline,
  one per `|`-opening line. `|---|` rules are leaves like any other — so there is
  no cell grain and no column awareness.
- The openers are the CORPUS's: `-`, `1.`/`1)`, `+`, and an INDENTED `*`. A block
  is ANY `#+begin_X` with a matching `#+end_X` BY NAME — naming quote/src/example
  would have missed this corpus's commonest block. ONE blank line stays inside a
  list (org's rule); two, or a blank with something else under it, close it. An
  item deeper than the first RIDES INSIDE the item above rather than taking a
  stop. An opener with no closer is ordinary text. A paragraph ends at the next
  STRUCTURE as readily as at a blank line.
- ORG LINKS RENDER, under org's DISPLAY-VS-SOURCE model: what is SHOWN is the
  description (`[[T][D]]` shows `D`, `[[T]]` shows `T`, a bare URL shows itself),
  and what `RET` opens is the RAW org. The display never becomes the source, so
  an edit is always over what the file says. NO SECOND PARSER: the shown text is
  the server's `desc` verbatim and the range is its `span`, so this page only
  intersects file-spans into an element's coordinates and draws segments.
  `drawText` walks the segments in order and SILENTLY DROPS a link starting
  inside the previous one, so it rests on a non-overlap guarantee only
  `subtreeLinks` can give and nothing checks. SPAN-driven, never search-driven:
  `/links` keeps one entry per (target, shown) pair, so a URL written twice under
  one look is MARKED ONCE. The paragraph/leaf elements and the headline's TITLE
  cell render alike; the title needs `titleAt` because only the server has that
  sub-span, and a CHILD's title stays text. THE LINKS RIDE THE MATERIALIZE: `GET
  /headline` carries the ROW's whole scan as `links` (the same objects `/links`
  serves, one `linkJSON` builder), so the display is compact from the FIRST frame
  and there is no async gap to bridge — a second request opened one every fill
  had to cover, and the frames between drew the brackets raw. `/links` stays as
  the TABLE popup's and `edit-link`'s route. Links are NOT stops and bind no
  mouse — `o` is the opener and shares `linksIn` with the draw over the SAME held
  answer, so what a reader sees marked is exactly what `o` will find.
- THE CURSOR CARRIES ITS PANE'S SCROLL. `keepInView` on every draw, and the band
  is CSS: `.de` carries `scroll-margin-block: var(--g-doc-off)` and
  `scrollIntoView({block:"nearest"})` honours it, so the scrolloff is three of
  the pane's OWN lines and the movement code measures nothing. `scrollIntoView`
  is forbidden over the TABLE's rows — the renderer owns their scroller — and
  ordinary here. The suite keeps the distinction by COUNTING call sites.
- STARS, ORG-CLEANED. Every headline line opens with its own stars drawn the way
  `org-hide-leading-stars` + `org-startup-indented` draw them: every star but the
  LAST rendered as a space. Depth is RELATIVE to the entry the sheet stands on,
  so materializing into a child makes that line the root. It is DISPLAY CHROME
  ahead of the state cell rather than a cell — `f`/`b` walk past it — and the
  indentation IS the outline, so child lines carry no padding of their own.
- AND CONTENT SITS UNDER THE TITLE TEXT: a paragraph starts at the head's own
  title column rather than at its stars. The width is DERIVED from `dstars` and
  written onto `#mdoc` as a NUMBER (`--g-doc-indent`), with the arithmetic in the
  stylesheet. PADDING rather than a margin or a `text-indent`: a margin would
  shrink the element's box and take the selection wash off the left of the line,
  and a `text-indent` would indent a block's first line alone. Chrome only —
  `bodyText` never reads it.
- AND A HEADLINE LINE IS LAID OUT AS ORG LAYS ONE OUT: the two headline kinds are
  flex rows where a paragraph is flowing text, the TITLE takes what the line has
  left, and the TAGS are flushed to the far edge (`org-tags-column`) by
  `margin-left:auto` rather than the title's flex alone, so a headline with no
  title still puts them at the edge.
- NO PLACEHOLDERS, EVER. A part the headline has not got renders nothing in every
  state, and `f`/`b` stop on the PRESENT cells alone. Setting an absent part is
  the COMMANDS' job: `t` and `:` fire AT THE ELEMENT, refused on a child.
- EVERY SELECTION IN IT IS A GROUND, never a line. Vertical is the ROW language
  (`--g-sel`) and horizontal is the COLUMN language: the cell under point wears
  the table's crosshair (`--g-col`/`--g-cell-wash`). No underline, no border, no
  outline in any of the four rules; `TestServe`'s ground sweep cuts them out of
  the page and asserts it, and asserts what it swept first.
- Its movement is the TABLE's two axes; `TAB` crosses to the panel and back, each
  pane keeping its own cursor and wearing the accent on its own frame
  (`#mdoc.on`, `#mprops.on`). The cursor's `dgrain` names its level.
- RET is BY KIND: a CHILD re-materializes into it (`?child=`), a PARAGRAPH opens
  as a textarea over itself, and the TITLE edits IN PLACE and commits
  `set-title` — one field (`DTITLE`/`#dtitle`, `tight` placement off the title
  cell's own box, right edge at the tags) laid over the title text alone, the
  headline keeping its stars, badge and tags on screen. RET on the headline LINE
  is the same door — the whole line's edit is its title — so no `f` is spent
  picking the cell and an absent title opens empty. The STATE and TAGS cells
  raise the value palette and the tags popup where present; `t` and `:` do the
  same at the element and are the only way to set an absent part. PRIORITY has no
  command yet. A CHILD's cells are read-only in v1 — no row id, so no `/command`
  addresses it — while its planning, drawer, paragraphs and children are all
  editable through the lens that materialized it.
- `SPC` TOGGLES AN ORG CHECKBOX at the stop under point, and `C-c C-c` with no
  element open is the same toggle (org's second meaning of the key). The box is
  the item's FIRST line (`CHECKBOX`: bullet or number, then `[ ]`/`[X]`/`[x]`/
  `[-]`); the flip is org's own — `[ ]` checks, `[X]` clears, `[-]` checks — and
  the write is the paragraph edit's splice, that item's lines and nothing else.
  `SPC` off a box refuses with an echo and writes nothing; the harness spells the
  space bar `press:Space`, cooked to the `" "` a browser sends.
- `DEL` is UP: in a child it re-materializes the `parent` the server named and
  lands on the child it came out of; at the top it is the sheet's door. The
  dispatch stands aside for a key this listener claimed (`e.defaultPrevented`),
  or the table's own `DEL` would strip a filter token on the same press.
- PER-ELEMENT COMMITS: a paragraph edit or delete is one drift-locked `POST
  /headline` carrying `body` beside the panel's own two lists, each answer
  re-pinning the digest and re-materializing off it. THE STORE LAGS THE WRITE IT
  ANSWERS FOR: `GET /headline` serves the store and the watch is a debounce away,
  so the re-read a 200 fires DROPS any answer whose digest is not the write's own
  receipt — the model the write was built from stands, redrawn — and retries once
  (~300 ms). Taking the stale answer reverted the pane and poisoned the pin, and
  a body-only edit emits no frame to correct either. A CELL EDIT RE-PINS THE SAME
  WAY: `fire()` takes `editing.digest` off the `/command` answer's own per-id
  `digest`, since the frame that would re-read is guarded off under an open edit
  or the panel's keys. A cell edit is a `/command`, and what it wrote comes back
  through the WATCH. `C-x C-s` is the commit for whichever edit is open.
  `dirty()` is the PANEL's and raw mode's alone. `d`/`D`/`u` over the document
  take PARAGRAPHS; a headline refuses with a log line. The sheet is one entry in
  `SURFACES`, the fourth `flagKey` surface and the fourth `openEdit` shape pair,
  whose `anchor` is the one thing a shape declares that a mount's does not.
- THE PANEL IS A TABLE-VIEW MOUNT — the renderer is the app's ONE list widget. A
  second mount in `#mptable`, columns `key | value`, `palette: true`, no
  `pageSize`, `actionHints: false`, `flagHelp`, and `flags: true` alone — no
  marks, so NO GUTTER: the renderer's gutter is the CHECKBOX's own (`chrome =
  marks`), and the flag's inset edge rides the row's FIRST cell, so a mount that
  flags without marking pays no empty leading column. Mounted ONCE and re-set per
  sheet, so opening a sheet costs one `setRows`.
- MODEL AND VIEW. `prows` is the model — key, value, `fixed` — and the mount is a
  view of it: `repaint()` is the one door. The cursor is the renderer's selection,
  movement is `selectStep(±1)`, the flags are the renderer's set, and this page
  keeps no copy. Row ids are stable for the sheet's life: `PLN:<KEYWORD>` for the
  three planning rows, `P<n>` per property, so a flag and a selection survive an
  edit above them.
- The panel is MODAL, and its keys are a document listener of its own — written
  near the top of the glue so it registers AHEAD of the dispatch, safe because
  `typing()` has already killed every `table` row and it falls through on every
  key it does not claim. NAV: nothing is focusable, and movement is `n`/`p`,
  `j`/`k` and the arrows, both profiles' letters bound unconditionally. Entering
  the panel BLURS the textarea and sets `pnav`, which `typing()` counts as a
  focus of its own; without that the table's letters would move rows under the
  sheet. The panel's arrows are VERTICAL ONLY: `RET` opens the WHOLE row and
  `TAB` crosses its two fields, so a column selection would change nothing a
  reader can act on. EDIT: `RET` opens the row at point into the EDIT OVERLAY
  (`#pedit`, laid over the selected row — the mount rewrites its own rows as it
  scrolls), value focused first, key first where there is none yet, a planning
  key `readonly`; `+` adds an empty property and opens it; `TAB` hops the two
  fields and the pane crossing is suspended; `RET` commits into the MODEL and
  re-sets the mount; `ESC` cancels through `cancel`. A row HOLDS its committed
  text, so an open edit is not dirty and only a commit is. `shut` clears `pnav`.
  `preventDefault` fires only where a binding does, and only over an open subtree
  sheet.
- DELETION IS THE TABLE'S GESTURE, over the same renderer flags — one gesture,
  deliberately spelled twice, since the panel's keys live outside `keyBindings`.
  `can`/`flagsOn` are shared; the ACT is not, and neither is `ONCE` (the panel
  guards `e.repeat` by hand), `said` or `noted`. What "taken" means is the row's:
  a property is DROPPED, which is the emptied key spelled as a key press; a
  planning entry is CLEARED and its row stands, since an empty value is already
  how an entry is absent. A deletion moves the model, so it is dirty.
- The three planning rows are FIXED rows at the head of the same list —
  `SCHEDULED`, `DEADLINE`, `CLOSED` in org's order, key unopenable, value the
  timestamp verbatim, empty meaning absent — so clearing all three is how the
  planning line comes off. The logbook is a read-only strip under both panes:
  full width, muted, out of Tab and out of `dirty()`, showing the drawer's
  INTERIOR lines alone, and never sent. Neither the logbook nor a hidden property
  is rowed, so neither is flaggable by construction.
- ONE FOCUS LANGUAGE ACROSS THE SHEET: whichever pane holds the keys wears the
  accent on its own FRAME, and neither wears it otherwise. Declared for both
  rather than left to the browser, which can only dress the pane that takes a
  real focus: the panel holds the keys with nothing focused at all. The panel's
  half is drawn from `#mprops.on`, the same state `pnav()` reads, so it cannot
  leak past the keys.
- The log strip's severity and scope are COLUMNS, each as wide as its own longest
  word, so every message starts at one x position. The vocabulary is not a list
  in the code, so `TestServe` derives both widths off the page's own `append`
  calls.
- The whole page wears the default theme through the one palette
  `Glance.Web.Theme` declares. Two LIGHT values are lightness-only corrections
  the theme's own faces missed: muted `#7F8C8D` → `#667071` (3.5:1 → 5.1:1 on
  white) and accent `#4CB5F5` → `#31769F` (2.3:1 → 5.0:1), the hue held — muted
  text is read and a focus frame is looked for. The sheet keeps exactly one
  variable of its own, `--dk-mono` (Hack first).
- `DEL` IN THE TABLE IS A LADDER, and the rhyme is the backspace's: ERASE THE
  LAST STRUCTURE STANDING. A MARKED SET is one, so while marks exist `DEL` clears
  them and stops — the MARKS alone, since a flag is the archive queue and a
  backspace must not empty it — then the query's last token, then the drill it
  was made in. It runs `U`'s own implementation (`clearMarking`, `alsoFlags`
  telling the two apart) and the pill says the command that RAN. A rung with
  nothing under it falls through in SILENCE.
- AND `DEL` CLOSES A POPUP WITH NO INNER LADDER: over the LINK and TAG popups the
  popup IS the last structure standing, so `DEL` steps out where `ESC` does. The
  guard is the edit sub-mode — `DEL` inside an open rename or link edit stays the
  FIELD's character erase. The STATE palette is the exception and keeps its
  landed meaning (`DEL` commits `*empty*`): a value is what that surface exists
  to hand back.
- A CURSOR IS ONLY DRAWN WHERE THE KEYS ARE. Each sheet pane says on its frame
  whether it holds them; the cursor inside takes the same guard, so the sheet
  never shows two. The POSITION is not gated — it is the model's, so crossing
  away and back finds the cursor where it was left. A FLAG keeps its ground
  either way, being a queue rather than a cursor. The panel's costs TWO rules,
  since the wash it suppresses is the RENDERER's `tr.tv-sel` and the `tr.tv-alt`
  stripe under it has to be put back.
- EVERY POPUP HAS A URL, and it is shareable. PENDING ENFORCEMENT — stated here
  first, to land after the widget split (docs/proposal-widget-files.md). A
  surface that is up is `?page=NAME` beside `?q=`, written by the same
  `remember` that writes the query, and a boot carrying one RAISES that surface
  after the rows land. Within a surface the panel is the FRAGMENT, `#theme`,
  so every header a reader can reach has a name: the settings sheet on its
  theme tab is
  `/?q=state%3A*active*&page=settings#theme`. The names are the surfaces' own
  (`SURFACES` entries) and the panels' own (`SECTIONS` titles), so neither is a
  second list. Closing a surface takes the parameter off. What this buys is the
  thing the shell has nowhere else: a way to SEND someone a view — the settings
  a question is about, the tag manager over a set of rows — where today the URL
  describes the table alone and every overlay is invisible to it.
- The applied filter query is in the URL (`replaceState`, `keys` preserved) and
  applied from it on load. `DEL` over the table drops the query's last token
  through the renderer — the chips are the renderer's, so the strip is too.
  `remember` writes `q` unconditionally, so an emptied query leaves `?q=`
  present-and-empty: that is what `bootQuery` reads as intent, where an ABSENT
  `q` gets the default injected.
- DRILL-DOWN is ONE semantic at TWO GRAINS, and `DEL` is the single undo for
  both. A JUMP (`@`) pushes a crumb and applies a whole new query; a REFINEMENT
  edits the query in place and pushes nothing. `DEL` undoes whichever is nearest:
  `stripLastToken` while the query has tokens, and when the strip leaves it EMPTY
  and a trail stands, `popCrumb` plus the popped query INSTEAD of the empty one,
  so `@` then `DEL` is one step out and one back. With no trail the second rung
  is not there. `g` is HOME rather than a rung: it throws the crumbs away.
- The crumb STACK is the renderer's and this page keeps NO copy. `popCrumb` pops
  and returns without applying — whoever owns the fetching owns what a query
  means — so the shell applies it through `applyView`. What the page DOES keep is
  `crumbLabels`, token → label, because no lookup recovers it: the title belongs
  to the row referred TO, which is rarely among its own referrers. One map serves
  both readers.
- The trail crosses a remount through the URL and NOTHING ELSE: `?crumbs=` holds
  `{trail, labels}` beside `q`, written by `remember` and read back by `mount`
  (before `TableView.mount`, since `chipLabel` can be called during the first
  paint). Every mutation of the stack is followed by a `remember`.
  `stash`/`restore` deliberately say nothing about crumbs: what they carry is
  work the reader has NOT committed. A parameter that does not parse is one boot
  without a trail.
- WHERE THE CURSOR LANDS is THREE rules at one door, `land(sel, back)`: the row
  `sel` names while the view still holds it, else the row at index `back`, else
  nothing. An APPLIED view asks for nothing and takes row one; a POP asks for the
  row its drill was pushed from; an ARCHIVE asks for the next surviving row below
  point. `select` answers false for a row the view no longer holds, so a
  remembered row an edit took away falls through rather than being forced back.
  `applyView` takes the remembered selection as a fourth argument so the rule
  runs once rather than in each caller; `fetchRows` calls it too, since a commit
  REPAINTS rather than remounting.
- A BOOT IS AN APPLIED VIEW, so it takes row one through that same `land`. A
  mount has no cursor of its own — the renderer selects nothing until asked — so
  a page that landed nothing opened with `d`, `D` and `RET` answering `no row`.
  `start` lands on the MOUNT; the full set arriving behind it lands nothing more.
  A caller that PASSES an `after` lands inside it and this door stands aside.
- THE ARCHIVE ANCHOR. `anchorFor` takes it at FIRE time — by the time the rows
  have gone the gap they left is exactly what a later read cannot see — scanning
  from POINT: down the page for the first row not leaving, else back up for the
  nearest, else nothing. It carries `from`, `id`, `at` (the anchor's place among
  the SURVIVORS) and `on` (the page it was taken on). THE DOOR THE ROWS LEAVE BY
  IS THE FILTERED REFETCH: `archive` puts an UPSERT on the wire, so an UNFILTERED
  client splices the row back in and point does not move. All three doors call
  `settled`, which ALWAYS SPENDS the anchor — that is what keeps it describing
  ONE watch step — and lands it only where something is owed. `spent(mine)` drops
  it when the answer says `from` was not archived, keyed to its own anchor so an
  earlier answer cannot disarm a later archive's, and deciding before `unmark`,
  which can throw. A commit and a remount drop it outright. THE CARVE:
  `fetchRows` takes the landing as an argument and the watch's refetch passes
  `settled` where a commit passes nothing. What the anchor buys over the
  renderer's `keepSelection` is rows going from ABOVE point.
- The remembered selection rides BESIDE the trail (`crumbSels`) rather than
  inside it, because the renderer's `crumbOf` keeps a crumb's `label` and `query`
  and drops everything else. The renderer's DEPTH stays the truth: `selsFit`
  compares lengths and a side table out of step is dropped whole. Marks and flags
  need none of this — they are id-keyed renderer state.
- `@` ASKS BEFORE IT APPLIES. The drill is probed with the same query under
  `limit=1` and a total of zero applies NO view: the table, the filter and the
  trail stay where they were, with one `cmd` info line. An empty view is the one
  landing a reader can read nothing off. The cost is a second fetch on a key that
  was going to refetch anyway.
- A drill out of the EMPTY query pushes NO crumb, and that is the absence of a
  special case: "all rows" IS the empty filter, and `DEL`'s first rung already
  lands there. What goes with it is the cursor: `DEL` back out of that one drill
  lands on the FIRST row. `crumbLabels` is still written.
- `@` takes the row AT POINT and never the marked set — a drill is a look, and
  letting it inherit a mark would make every mark change what the key means. It
  is on the ONCE list. Feature-detected on the four crumb calls; an asset without
  them is told so and nothing is applied.
- There is NO status corner: the socket's state is carried twice over already
  (the stale wash and the strip's `ws` lines) and indexing by the strip's `boot`
  line. Body padding is one figure on all four sides, so nothing floats over the
  table's top edge.
- With no popup open the TABLE holds the keys, and a control that keeps the focus
  belongs inside a popup. The popups and the controls in them are the only
  legitimate focus holders; the page has no chrome outside them. A focused
  `SELECT` counts as typing, so one loose on the page ate `n`/`p` as type-ahead.
  Inside a popup the focus is the popup's, `typing()` is true while a control of
  it holds the focus, and `ESC` (`any`) and `C-x C-s` (`modal`) reach the sheet
  regardless. The popup hands the keys back ONCE, on close.
- Theme: `auto` follows `prefers-color-scheme` and is the default; a named theme
  stamps `data-theme` on the document element, and returning to `auto` removes
  it. The choice lives in `localStorage` under `glance-theme`, and `themeBoot` —
  one unindented line in `<head>`, so the suite's glue extractor cannot mistake
  it for the shell's inline block — reads it and stamps the attribute before the
  first paint. Without that line a dark page flashes light.

## Keyword config

- Recognition unions system + tag configs + file pragmas (superset — a keyword
  declared anywhere parses everywhere); classification is widest-scope: `default`
  (org's TODO/DONE) > system > tags (first wins) > file. FOUR scopes: the
  recognition union is NOT one, so a keyword only another tag's config names is
  unclassified (`classify`'s fallback, active), shown by no palette and settable
  on no row that does not reach it.
- WIDEST-FIRST IS THE DEFERRED BOUNDARY, and it inverts what a file's own
  `#+TODO:` buys: the shared scope settles a word once and a narrower one extends
  the vocabulary without redefining it, so `#+TODO: | TODO` in a file no longer
  makes that file's `TODO` rows done-like. A tag called `system` keeps its TAG
  rank and sits BELOW the system layer. SETTING is unchanged — `settableStates`
  is the chain flattened and a union has no order — so the reorder moved which
  source SHOWS a word and no word a row may be set to. Letters fall out of it:
  `default` drawing first makes `TODO` = `t` and `DONE` = `d` in every tree.
- A KEYWORD LIST IS ORDERED, and the order is the ORG FILES' OWN. Segments run in
  `keywordScopes` precedence and inside a segment the words are that layer's
  `#+TODO:` line left to right, a repeat keeping its FIRST place.
  `Config.recognizedKeywords` is the one rule; `hrKeywords` and `storeKeywords`
  both come off it, so one file's palette and the whole store's cannot order the
  same words differently. Sets answer RECOGNITION alone: `Context`'s two `Set`s
  stay, `seedContext` builds them from the ordered lists, and that boundary is
  the only place a keyword becomes a Set. What ordering buys, all live: the state
  column sorts by the cycle; the palette's which-key letters are assigned over
  the declared order; `GET /keywords` answers ordered inside each source; and
  reordering a `#+TODO:` line closes the socket `view-changed`. An empty store's
  palette is org's own pair, `default` being the chain's first scope.
- Config lives at `<root>/.org-glance/config/{system.org,tags/*.org}`, is never a
  row source, and a config change reseeds and reloads the world (debounced,
  view-changed follows). The chain is ONE list, `Config.keywordScopes`, two
  readers and three answers: `classify` takes the first scope with an opinion,
  `Query.keywordSources` reports what each claims, and `Query.settableStates` is
  THAT flattened rather than a third fold, so the offer and the wall cannot come
  apart. Org's built-in cycle is `builtinKeywords`, off `defaultContext`.
- SET-STATE LEGALITY IS THE ROW'S CHAIN, not its file's recognized set: settable
  only where org's cycle, `system.org`, one of THAT row's tags' configs or the
  file's own `#+TODO:` declares it. Whole-request 400 naming the keyword and the
  row when any named row's chain lacks it, so a marked set spanning tags is
  refused for the member it does not fit. The palette is the truth over one row;
  over several the merge can offer a keyword part of the set cannot take.
- `hrDeclared` is the file's OWN `#+TODO:` and is stored beside `hrKeywords` (the
  recognized union) because neither recovers the other. One value shared per file.
- `clSeed` is stored, not derived: `clTags` keeps the FIRST config of each tag
  across directories while the seed unions every entry read, shadowed ones
  included, in walk order. Its only consumers are `seedContext` and
  `recognizedKeywords` — it is out of `keywordScopes`, so nothing classifies or
  authorizes by it.
- `system.org` carries two TREE-WIDE lines beside its cycle —
  `#+GLANCE_DEFAULT_FILTER:` and `#+GLANCE_CAPTURE_TARGET:` — and each NAME is
  written once, as a key constant, with `settingOf key` reading it
  (`lastPragmaValue`, last line wins) and `settingEdits key` writing it
  (`pragmaLineEdits`: replace where it stands, insert under the header, empty
  deletes). The reader folds the key and the writer renders it off one
  `settingPragma`, so a fold that drifted from a render can no longer rewrite a
  line nothing reads. Carried by `clFilter`/`clCapture`, spliced in the SAME
  `configEdits` call as the block and the layer's TEMPLATE — four regions of one
  file ride one write, since four writes would be four digests. A tag layer names
  neither. `Config.systemSetting` is the ONE "first system layer that names one"
  fold.
- A SAVED VIEW IS A REGISTRY ENTRY: `Config.savedViews`, each a `SavedView`
  carrying an id, a `system.org` pragma and a built-in. TWO of them — `default`
  (`#+GLANCE_DEFAULT_FILTER:`, `state:*active*`) and `agenda`
  (`#+GLANCE_AGENDA_FILTER:`, `state:*active* -planned:*empty* sort:scheduled`)
  — and a third is one entry: the load folds the list into `clViews`, `/config`
  serves it, the settings selector is built from what it serves, and the write
  is keyed by id. `viewQuery id cfg` answers; absent means the built-in, a line
  naming nothing means the empty query, and the LAST line wins. The system layer
  alone, and the first config directory that names one — a saved view belongs to
  a tree rather than to a tag. THE QUERY IS THE ONE CARRIER OF A VIEW — filter,
  `sort:` and `columns:` tokens alike — so a view persists all three in its one
  line and `g`/`a` apply all three back; neither knows a token from a token.
  The wire is TWO SHAPES for two jobs: the ANSWER (`GET /config`, and the page
  blob) is an ordered ARRAY of `{id, query}` in registry order, so a client
  reads the order without iterating keys; the WRITE takes `views: {id: query}`,
  three-valued per view like every other optional region (absent leaves it,
  empty deletes the line), so editing one leaves the others where they are, and
  a view no build carries is a 400 naming it. WRITING is `P`
  (`set-default-view`, ONCE) under `views.default` and the digest `GET /config`
  just served, WITHOUT a `lines` key — which is why absent lines leave the
  `#+TODO:` block standing. The settings sheet edits any of them in ONE COMPOSER
  (`composer: true`, the omnibox bar and chips with no table behind them),
  `#cwhich` naming which: `vrows` holds each view's text and the box is a VIEW
  of the selected one, so switching asks the server nothing and loses no edit
  (the layer boxes' rule). `pinnedQuery` and `agendaQuery` are LIVE — a write
  under a running page is what the next press applies.
- Settings sheet = `,` (`customize`) and it is TABS: `SECTIONS` still owns the
  names and the order, and draws a strip of buttons over one pane at a time.
  A hidden panel is out of the flow, so its fields leave the tab order with it.
  TAB IS THE TAB KEY — it walks the panels and wraps, `S-TAB` walks back, and
  the newly shown panel's first control takes the focus — through the sheet's
  own listener, registered ahead of the dispatch and claiming nothing while the
  sheet is shut or a momentary popup stands over it. The horizontal arrows walk
  the strip while a tab button holds the focus. THREE panels: GENERAL (the saved
  view composer with its `#cwhich`, the capture target, the log height), THEME
  (the reader's `auto`/`light`/`dark` preference, and the TREE's own state hues)
  and KEYWORDS (the layer select, its two boxes, the union and its note).
- THE STATES TABLE is the theme panel's own mount, the page's FIFTH and its
  second MUTABLE one: `tag | state | group | colour`, one row per keyword the
  tree knows. BY LAYER, THEN CYCLE ORDER — `crows`' own order (system first,
  then the tags alphabetically) and inside a layer its `#+TODO:` line left to
  right, actives before the done-like. A word two layers declare is TWO rows: a
  state belongs to a FILE. `RET` opens the row into `#sedit` (the property
  panel's `openEdit` mechanism, three fields, `TAB` hopping), `+` adds a state
  to the layer the cursor stands in, and `d`/`dd`/`D`/`u` are dired's gesture
  through `flagKey`'s fifth surface. A STATE RIDES ITS LAYER'S WRITE and a
  COLOUR rides `system.org`'s, so one row can move two files and both leave in
  the one flush. A keyword no config layer declares is listed under the tag
  `file` — the tree recognizes it and this sheet cannot move it, so it is there
  to be COLOURED and `d` says so and leaves it standing.
- TWO EDITORS, ONE CYCLE: the states table and the keywords panel's box both
  write a layer's `text`, and each is a VIEW. So `takeLayer` reads the box only
  while its own panel is `showing("keywords")`, and a panel fills itself from
  the model on arrival — `SECTIONS` entries declare an `enter` hook and NO
  caller indexes that list by number. The page renders the one `#+TODO:` line
  (`writeCycle`); reading a cycle stays the server's, which serves each layer's
  `keywords` beside its `lines`.
- ONE THEME CONTROL, and the hues follow it. `#themesel` is the whole of the
  reader's choice; WHICH theme the state hues describe is DERIVED from it
  (`hueTheme`, `auto` resolving through the media query the boot line reads), so
  picking a theme moves the page and these rows together. The STORAGE stays per
  theme because readability is — a hue that reads on white is unreadable on
  black — so each theme's edits stand while the other is on screen and both ride
  one write. The model is `hues` = `{theme: {keyword: hue}}` on the SYSTEM layer
  beside its other tree-wide lines, so it is `cmoved` and posted the way they
  are; the rows are a VIEW of `hues[hueTheme()]`.
  The wire carries them FLAT — one `{theme, keyword, hue}` entry each — in both
  directions, so nothing iterates keys to read back what it wrote, and
  `stateColorsEdits` renders one line per theme where `stateColorsOf` reads
  them, sharing the key so a fold cannot drift from a render. `/config` also
  serves `themes`, the names this build carries.
- `GET`/`POST /config` serve and replace ONE layer's `#+TODO:` block AND its
  capture template through the ordinary write path, so a `#+TITLE:` and a comment
  come back byte for byte. The optional parts are `ConfigParts`, a RECORD rather
  than three positional `Maybe Text` (all three the same type, so a caller
  swapping two would compile), each three-valued: absent leaves that part, empty
  takes it off, anything else writes it. `filter` and `capture` are the SYSTEM
  layer's alone; the TEMPLATE is every layer's. The client names a part only
  where it MOVED — sending the template unconditionally put every layer's first
  heading through the one-top-entry wall on every write. The route never writes
  the store. `GET` reads the files (the digest handed out is the lock), and its
  layer list IS the POST allowlist. An EMPTY digest is the pin for a file that is
  not there: `Data.Org.Edit` treats it as the empty document and creates.
  Refusals: a non-`#+TODO:` line, a block declaring no keyword, an unknown path,
  a bad body — 400; 409 on drift; 413 past 1 MiB. An empty `lines` deletes the
  cycle.
- Creating the FIRST `.org-glance/config` in a tree that had none is two
  directories at once, which fsnotify arms and never enters, so that write
  reseeds because `writeLayer` goes through `Watch.writeSpans`.
- Settings sheet = `,` (`customize`), the page's ONE place for a preference and
  the materialize sheet's own ladder over `/config`. THREE PANELS from ONE list
  (`SECTIONS`, header + part ids): GENERAL (default view, capture target, log
  height), THEME (the `auto`/named select, a `localStorage` preference that
  applies as it is picked and asks no server), KEYWORDS (`clayers` — one select
  over one box holding the SELECTED layer's `#+TODO:` lines VERBATIM, since the
  page has no org parser — then the union `ceff` and its note). A fourth panel is
  an entry plus the markup it names: bodies are laid out by class, never by a
  roll of ids. They are markup wrapped at boot rather than built from the list;
  the join is by id and a `parts` id the markup lacks throws at boot. The list
  order is the TAB order and the sheet opens on the general panel's first field.
  `shutSettings` blurs on the way out. Where a field is DRAWN moves no write: the
  two general fields stay bound to the system layer.
- KNOWN GAP: the gear was the coarse pointer's ONLY settings door and went with
  the corner. A touch reader can filter and read; `,` cannot be typed there.
- The KEYWORDS panel is ONE select over the layers and TWO boxes — `#ctext` for
  the cycle and `#ctpl` for that layer's CAPTURE TEMPLATE, a region of the same
  file riding in the same write — with `#clab` naming the selected layer and
  `#clerr` carrying what the server last said. Order is system first, then the
  tag layers by `localeCompare`; `sort` is stable, so two system layers keep the
  walk's order. Both texts live on the LAYER and the boxes are VIEWS:
  `takeLayer()` copies both back and every door calls it first, so an edit
  outlives every switch and a switch asks the server nothing. `cmoved(r)` is
  `r.text !== r.base || r.tpl !== r.tplBase` plus the two general fields, and a
  part is SENT only where it moved. `%` in the template box raises the value
  palette in its field mode over the SERVER's code list, so the completion cannot
  offer a code the expansion does not know. Still one drift-locked `POST /config`
  per FILE that moved, each awaited, each under its own digest. A refusal SELECTS
  its layer, so the box shows the file the message describes.

## Build

- `glance.cabal` is hand-maintained; package.yaml/hpack removed — do not
  regenerate.
- `assets/table-view.js` is a committed BUILD INPUT: in `extra-source-files`,
  read by `Routes`'s `embedFile` splice (`addDependentFile` recompiles on
  change). Refresh it with `make sync-renderer`, never by hand.
- Components: private sublibrary `glance-internal` (`src/`), public library
  `glance` (`src-query/`, `Glance.Query` only), private sublibrary `glance-web`
  (`src-web/`) on the public library alone, private sublibrary
  `glance-desktop-native` (`src-desktop-native/`) on `base` alone, one CLI
  dispatching to three sublibraries, one suite naming the three that carry
  testable code. A new web or daemon target depends on the public library alone.
- Inside `glance-web` the dependency runs ONE way — `Base` the floor,
  `Glance.Web` the door: `Base` → `Keymap`/`Theme`/`Page.Style`/`Page.Glue` →
  `Page` → `Routes` → `Glance.Web` → `Glance.Desktop`(`.Native`), with `Watch` on
  `Store`, `Commands` on `Base` + `Store` + `Watch`, and `Routes` also reading
  `Filter`, `Sort`, `Columns`, `Store`, `Watch`, `Page.Style`. `Watch` sits under
  both write routes because `writeSpans` — the door every write leaves through —
  and the predicate filtering what it queues are the watch's. `Base` holds
  exactly what more than one module above needs: `ServeOptions`, the response
  constructors, the body reader and the write-refusal vocabulary. The TH renderer
  splice is in `Routes` beside the asset serving that reads it, so `Routes` alone
  carries `TemplateHaskell`. `Glance.Web` is a facade.
- `glance-desktop-native` exposes `Glance.Desktop.WebKit` alone and is the ONLY
  stanza the `native-window` flag reaches. Unflagged it builds on `base` in one
  module, so every other component is byte-identical either way and CI never
  needs GTK. Flagged, the solver pulls ~28 packages generated from the machine's
  typelibs.
- The flagged build is `make native` = `cabal.project.native` plus
  `HASKELL_GI_GIR_SEARCH_PATH=vendored/gir`. `vendored/` = gi-webkit2 and
  gi-javascriptcore4 as upstream cut them with the lines marked `glance:` moved
  to the 4.1 typelibs, since Arch dropped the 4.0/libsoup2 generation; both keep
  upstream's name and version, which is what makes a local package shadow
  Hackage's. `vendored/gir/` holds the hand-written GIRs this machine has only
  `-runtime` of. They stay OUT of `cabal.project` because a local package is
  built by `cabal build all` whether or not anything depends on it — that
  exclusion is what keeps the unflagged build GTK-free. `gi-gtk3`/`gi-gdk3`
  rather than `gi-gtk`/`gi-gdk`: gi-webkit2 names the former, so the old spelling
  would put two packages claiming `GI.Gtk` in one plan. cabal's package hash
  counts resolved pkg-config VERSIONS, so a distribution upgrade re-keys every gi
  package and `make native` regenerates them.
- Every implemented feature earns a `CHANGELOG.md` entry under `Unreleased`,
  written as user-visible behaviour (Added/Changed/Fixed, one line per feature);
  a coherent feature set cutting promotes `Unreleased` to a dated version and
  bumps `glance.cabal`'s `version` and README.org's to match.
