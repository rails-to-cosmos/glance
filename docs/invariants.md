# Invariants

Rules this repo enforces everywhere, with the evidence and what breaks. The
index is [../CLAUDE.md](../CLAUDE.md); the model is [../AGENTS.hs](../AGENTS.hs),
whose `Note` lines carry many of these with a proof tag. **Fragility** is how
easily a refactor breaks the rule without turning anything red — high means
nothing catches it.

## The write path

- **The pane is a narrowing.** Text entering the doc pane through an edit or a
  new paragraph passes `narrowed`: a typed headline at the materialized root's
  level or above is demoted to the first child level, so no write escapes the
  subtree. `Doc.elm` (`narrowed`, applied in the `Edit` and `Insert` arms);
  pinned end-to-end by the browser case "the pane is a narrowing…" and the
  AGENTS note. Bypassing it lets a pane edit rewrite content outside the
  materialized headline. *fragility: low*

- **One door.** Every byte written to the tree leaves through
  `Watch.writeSpans` → `Query.replaceSpans` → `Edit.editFile`; no module calls
  the splice engine directly. `Watch.hs:88`, `Query.hs:1489`,
  `TestSelfContained.hs:182`. A write that bypasses it skips the drift lock,
  the external ledger note and the watch nudge, so the store diverges from disk
  and org-glance never learns of the write. **The guard sweep filters to
  `src-web/`, so a splice under `src/`, `src-query/` or `app/` passes green.**
  *fragility: high*

- **Every write is drift-locked.** A write is pinned by a SHA-256 of the exact
  bytes it was read from, and a mismatch aborts before any byte is touched.
  `Edit.hs:206`, `Edit.hs:212`, `Routes.hs:666`. Dropping or defaulting the
  digest turns a stale tab into a silent whole-file overwrite. *fragility: high*

- **The empty digest is the create pin.** An absent file under `""` is created;
  an occupied path under `""` drifts. `Edit.hs:207`, `Query.hs:1454`,
  `Commands.hs:401`, `Config.hs:322`. Making a missing file a hard `ReadFailed`
  breaks blob capture and tag-layer minting; making a present file writable
  under `""` turns capture into a silent overwrite. *fragility: high*

- **`.glance-tmp` is the LAST dot-component of the temp name.**
  `openBinaryTempFile` splits its template at the last dot and the walk collects
  by `.org`. `Edit.hs:220`, `Walk.hs:252`, `TestEdit.hs:426`. Reordering leaves
  an interrupted write as `notes<rand>.org`, which the walk collects, parses and
  serves as real rows. *fragility: high*

- **The rename is atomic and not durable.** The containing directory is fsynced
  after it, and a write that created directories syncs each new one's parent.
  `Edit.hs:231`, `:247`, `:254`, `:265`. Dropping the syncs means a crash takes
  back a write that already answered 200. The sync failure is deliberately
  swallowed — reporting it would refuse a write that landed. *fragility: medium*

- **`Data.Org.Edit` is content-agnostic.** It splices character spans and knows
  no org syntax, so every org-shaped wall (tag charset, keyword charset,
  planning reparse, one-top-entry, trailing space) is owed by the layer above.
  `AGENTS.hs:3966`, `Query.hs:1674`, `TestQuery.hs:1751`. Pushing a check down
  makes it unreachable for callers off that path; pushing one out of `Query`
  lets bytes land that the next parse reads as body text. *fragility: high*

- **A write derives its line ending and its opening from the text it lands in**
  (`eolOf`, `openingFor`), never from a constant. `Edit.hs:95`, `:102`,
  `Query.hs:2145`, `Config.hs:212`. Hard-coding `"\n"` converts a CRLF file line
  by line on every edit; skipping `openingFor` joins an appended `* ` onto the
  last live line. A **capture blob lands in no text at all**, so it takes the
  ending of the bytes it is composed out of — the tag's template — and the
  pane's body is re-ended into that (`Commands.hs:401`, `Query.hs:draftEntry`);
  the page speaks `\n` and knows no other, so without it a CRLF layer lands an
  entry ending one way over a body ending the other. *fragility: medium*

- **Composed lines are `untrailed`, stepping over the terminator so a CRLF
  survives.** `Query.hs:1092`, `:1312`, `Routes.hs:696`. A `T.stripEnd` in its
  place eats the `\r` too, rewriting every line ending in the file.
  *fragility: medium*

- **Anything written back must reparse under the parser's own charsets** — tags,
  keywords and planning entries are validated by reparse of the very line the
  write would produce. `Query.hs:1511`, `:1519`, `Routes.hs:586`, `:597`.
  Loosening a charset is silent: the run falls into title text on the NEXT
  load, long after the 200. *fragility: medium*

- **`add-link` appends, and it appends to the row's OWN body.** `ownBody` takes
  the subtree's lines past the title, less every lifted region (planning line,
  drawer, logbook), and stops at the first line `headingStars` reads as a child
  — org's stars and a space, so a `*bold*` line is body text. The link joins the
  last written line, else opens the body under whatever header lines the row
  carries; the title road IS `setTitleEdits`, so a headline meets one wall
  whichever door writes it. `Query.hs:1808`, `:1820`, `:1797`,
  `TestQuery.hs:1876`. A walk to the topmost owner writes the row's link into a
  child's paragraph, and a second title path lands one past the tag run.
  *fragility: medium*

- **Deletion is a move.** The blob directory is gzipped under the trash mirror,
  the copy lands before the original goes, and an occupied destination is
  refused. `Trash.hs:38`, `:52`, `Commands.hs:136`. Remove-then-copy loses the
  blob on any IO failure mid-move. *fragility: medium*

- **The ledgers are append-only, best-effort and derived.** One `O_APPEND`
  `write(2)` per line, hand-assembled so field order is the contract, and no
  failure reaches the caller. `External.hs:109`, `:69`, `:102`. An `AppendMode`
  handle remembers its open offset, so concurrent writers overwrite each other;
  aeson would reorder the fields the peer's reader depends on.
  *fragility: medium*

## The store and the routes

- **The `Store` TVar has exactly two writers** — `finishLoading` and `publish` —
  and every route reads it only. `Store.hs:324`, `:357`. A third writer breaks
  the generation-bump/frame-derivation pairing, so live sockets stop receiving
  ops for rows that moved and the ETag stops changing. **Nothing tests this.**
  *fragility: high*

- **No write route publishes its own write.** `POST /command`, `POST /headline`
  and `POST /config` leave the store to the watch. `Commands.hs:269`,
  `Routes.hs:619`. A route that "helpfully" updates the store races the watch's
  reload, and the client model — which steps off the command's own answer —
  disagrees with what the socket streams. **The read a commit opens with is the
  exception and is no write of its own**: where the file has ALREADY moved under
  the store, `onRow` reloads it through the watch's own door before a byte is
  written, and the commit then refuses on the client's pin. `Routes.hs:547`.
  *fragility: high*

- **`nudge` runs on the success branch only.** `Watch.hs:98`, `Commands.hs:272`,
  `TestSpec.hs:819`. Nudging unconditionally costs a re-read per 409; skipping
  it on success leaves the store holding pre-write rows until an inotify event
  that may never come. *fragility: high*

- **A batch is one drift-locked write per FILE, with no rollback across files.**
  A 200 means the command ran, never that every row moved. `Commands.hs:230`,
  `:288`, `:390`. Merging the per-file plans turns a partial failure into a
  whole-request 500 and loses the per-id `results` the shell steps its model
  off. *fragility: high*

- **Every write route answers through `answerWrite`, every body arrives through
  `withBody`**, so 413 outranks every other refusal. `Base.hs:161`, `:188`.
  Taking the body outside `withBody` lets an oversized request be decided by a
  404 first, and `strictRequestBody` pays for the bytes before it can refuse
  them. *fragility: medium*

- **One clock read per request,** taken before any row, and there is ONE
  spelling of it: `Base.today`. `Base.hs:78`, `Commands.hs:296`, `:299`,
  `Routes.hs:301`. Per-row reads let a batch spanning midnight land on two
  days. A read taken BELOW a route's revalidation branch is the same fault
  wearing a cache: the store is unchanged, so a `today` query 304s into
  yesterday's rows — which is why the day rides in the ETag unconditionally
  (`Routes.hs:359`) — no reader tests for a clock word, so renaming one cannot
  leave a stale detector behind. One extra revalidation a day. THE DAY IS ONE
  OF WHAT THE DOOR RESOLVES ONCE, before any row: `Asks` spells them in a
  closed word rather than a flag apiece, `add-link`'s target resolving among
  the same rows the ids do, so a refusal there is the whole REQUEST's.
  `Commands.hs:122`, `:348`, `TestServe.hs:9421`. *fragility: medium*

- **One edge relation, resolved once per store version.** Every `hrLinks` entry
  resolves through `nameClaims` to the rows claiming that name in that
  namespace: a link naming no row is no edge, and a row is never its own
  reference. Both cuts are made at `resolvedEdges` and nowhere else, so `refs`,
  `referrers`, a `neighbors` walk and `ref:*any*`/`from:*any*` read ONE
  relation. The store's copy is DERIVED and LAZY, and every writer of `stFiles`
  passes through `withEdges`, which reads the files map rather than the store,
  so no store version retains the one before it. `Query.hs:803`, `:813`,
  `Store.hs:127`, `:228`, `:236`, `Filter.hs:381`, `TestQuery.hs:1946`,
  `TestServe.hs:9400`. A second hand-rolled adjacency answers `refs` what
  `ref:*any*` denies; a writer that skips `withEdges` serves the graph the store
  held before the write. *fragility: high*

- **An MCP tool meets the query string's own walls.** `get-headline`,
  `list-headlines` and `neighbors` synthesize the very `Request` the route
  takes: ONE RENDERER on the tool's side, ONE WALL on the route's, one refusal
  sentence per wall. `argBytes` renders every scalar argument to the query bytes
  it would have arrived as — a string as itself, a number in its own decimal
  spelling, `true` for a flag that is set, and NO PARAMETER for a flag that is
  off, for a `null`, or for an argument left out — and `param` hangs it on the
  request. The route reads it back: `wholeNumber` refuses a number that is none,
  `cappedAt` refuses rather than trims and spells every capped number's refusal
  in one sentence (`limit` and `depth` alike), `queryFlag` refuses a flag
  spelled any other way. `total` counts BEFORE the cap, so a `limit` is honest.
  Without the flag the answer is the one the door always gave, byte for byte.
  `Mcp.hs:346`, `:358`, `:364`, `Routes.hs:243`, `:249`, `:257`, `:264`,
  `:483`, `:504`, `:1025`, `:1031`, `Query.hs:851`, `:2731`,
  `TestServe.hs:9170`, `:9183`, `:9192`, `:9233`. A tool that reads its own
  arguments grows a second refusal vocabulary: the `argInt` deleted on
  2026-09-12 rounded `{"depth": 2.7}` to 3 and served `{"limit": "5"}` the
  uncapped page, where `?depth=2.7` and `?limit=abc` are 400s. A cap that trims
  makes `total` a lie. *fragility: medium*

## Parsing and the walk

- **A record retains no document bytes.** `HeadlineRecord` carries cells, spans,
  a digest and links, and no parsed headline; the text is read by path at
  request time and pinned against `hrDigest`. `Query.hs:311`, `:1017`,
  `Routes.hs:558`, `Commands.hs:330`. Putting the document back on the row
  returns the residency this removed — 26 MB of blobs held 305 MB of RSS after a
  day up. **A reader that reaches for a field instead of its `Text` argument is
  the way it comes back.** *fragility: high*

- **A request-time read is pinned, and the pin is the write door's own.**
  `pinnedDocument` IS `Edit.currentText`, so a reader and a writer agree to the
  byte on when a file has moved, and a file that cannot be read or decoded stays
  apart from one that drifted. `Query.hs:1464`, `Edit.hs:206`, `Base.hs:180`.
  Slicing new bytes with the parse's spans serves text no row ever named.
  *fragility: high*

- **A READ reloads on drift; a WRITE refuses.** The store lagging its own tree is
  this server's own signal, so a read whose file has moved runs `Watch.reload`,
  addresses the id again under the fresh digest and answers the file — a second
  drift being a genuine race, which takes the 409. A write takes the 409 the
  first time and a command refuses the row. `Routes.hs:547`, `Watch.hs:120`,
  `Commands.hs:515`. Letting a write reload would land bytes over a subtree the
  client never saw. *fragility: high*

- **No query touches the disk.** A custom column is asked per row per request,
  so `closed` and the drawer pairs are cut at parse into `hrClosed` and
  `hrDrawer`, and `/properties` walks those. `Query.hs:329`, `:493`, `:1237`,
  `Routes.hs:778`. Reading either back out of the document turns one request
  into one file read per row. *fragility: high*

- **Text kept past a document's parse is `T.copy`'d** (`detach`), and ONLY what
  is genuinely a slice: a `T.pack`, a `showt` or a `T.toLower` already owns its
  array and is FORCED instead. `Query.hs:1428`, `:699`, `Config.hs:98`. A field
  that forgets the copy retains the whole source document for the process's
  life — a residency regression `cabal test` cannot see; only
  `glance doctor ~/sync` exposes it. The forcing is `forceRecord`'s
  (`Query.hs:1435`): a cell left as a thunk retains the parse however carefully
  it was copied. *fragility: high*
- **A parse failure fails exactly one file.** `evaluate` inside `try` scopes it,
  and the store keeps that file's previous rows. `Edit.hs:170`, `Store.hs:227`.
  Dropping the rows empties the table between two keystrokes while a file is
  half-saved. *fragility: medium*

- **`identityOf` is the ONE reader of a row's effective id.** The id the parse
  read, else the one a BROKEN drawer still spells in the subtree's raw lines
  (`salvagedIdentity`); a drawer the parse DID read is taken at its word, so a
  blob carrying no id keeps its `<path>#K` form. The walk reads it off the
  subtree and the doctor's scan off a `Cursor` walk — once per headline, a
  `T.drop` apiece being quadratic in the document. `Org/Types.hs:345`, `:350`,
  `Query.hs:502`, `Doctor.hs:224`, `TestSpec.hs:555`, `TestIndex.hs:306`,
  `:314`, `TestServe.hs:9300`. A call site reading bare `identity` gives the row
  a path id that `get-headline`, `edit-link` and the ledger refuse while `from:`
  accepts it — the split this closed. The completion ledger keeps bare
  `identity` ON PURPOSE (`External.hs:87`): org-glance cannot resolve an id only
  a broken drawer spells, and a comment is all that guards that.
  *fragility: medium*

- **A symlinked directory is never followed**, and one `lstat` per entry decides
  what an entry is; a failed stat falls to the keep-on-name branch.
  `Walk.hs:164`, `:169`, `Trash.hs:74`. `doesDirectoryExist` in its place
  reintroduces link loops, and the trash copy would pull a foreign tree in.
  *fragility: high*

- **The walk and the watch share ONE set of path predicates,** reached through
  the `Glance.Query` facade. `Query.hs:755`, `Watch.hs:107`, `Walk.hs:243`. A
  second hand-rolled predicate lets a file the walk never loaded arrive by
  inotify, so the store gains rows the next full load deletes. *fragility: high*

- **The WAL tail hears two file names and reads no field but the id.** The one
  tree watch takes `.org-glance/meta`'s open segment and the `MANIFEST` beside
  the documents — `tailedFile` names those two and nothing else, a sealed
  segment being folded at boot and `EXTERNAL.jsonl` this daemon's own — and each
  COMPLETE record's blob path goes through the one queue door, a tombstone's
  too, the reload finding the blob gone. A torn tail waits for its newline. The
  seal is read off the INODE, two segments being the same length often enough,
  and a segment shorter than the cursor starts the read over at 0. The read
  takes NO GHC HANDLE LOCK (`withFd`): the segment is the peer's file, and a
  `System.IO` handle would refuse org-glance its own write. `Index.hs:167`,
  `:174`, `:205`, `:229`, `Watch.hs:208`, `TestIndex.hs:365`,
  `TestServe.hs:533`. Reading a record's state into the row makes the peer's
  index the truth and the file a rumour; a size-carried seal reads a fresh
  segment's bytes at the old offset. **The boot cursor is taken BEFORE the walk
  (`Web.hs:78`), so a record appended while it runs is replayed rather than
  missed — nothing tests that ordering.** *fragility: high*

## Shape

- **A fact several readers agree on is spelled in ONE list, indexed by key** —
  `viewColumns`, `docCells`, `popups`, `gluePartFiles`, `keyBindings`,
  `Palette`. `Query.hs:2576`, `Base.hs:125`, `Page/Popups.hs`. Re-spelling a
  membership at a second site is the failure the popup registry records: six
  sibling id lists were hand-edited and the seventh missed, so `#mint` neither
  faded nor dimmed. *fragility: medium*

- **One gold at a time; the coarser ground lifts.** `--g-sel` is spent twice —
  the cursor row's own wash and every selection or pick wash — so a finer gold
  standing INSIDE a coarser one is drawn on the colour already behind it unless
  the coarser lifts. Both lifts spell the same `background-color:transparent` on
  the OUTER ground: the pane wears `tight` while a box stands inside its row
  (`page.css:772`, `20-sheet.js:492`), and the planning line drops its wash
  while an entry is picked (`page.css:897`). Dropping either lift, or
  adding a third gold one grain finer without one, leaves the wash set, focused
  and invisible — a state only PIXELS see, which is why the entry's case counts
  them (`cases.mjs:4026`, `:4460`) rather than reading a class.
  *fragility: medium*

- **The empty cell sits outside every date comparison, and negation is no
  mirror.** `dated` guards all four operators and both range ends, because `""`
  is below every literal in byte order and an unguarded `<` would serve every
  undated row; `*empty*` stays the one name for those rows. It follows that
  `-k:<D` serves the undated rows where `k:>=D` does not, so the operators do
  not pair off under the sign and no surface may rewrite one into the other.
  `Filter.hs:644`, `table-view.js:702`, `AGENTS.hs:2681`. A tidying pass that
  normalizes `-k:<D` into `k:>=D`, or drops the guard because byte order
  "already sorts an empty cell first", turns nothing red but the one case that
  names the pair (`TestFilter.hs:978`). **That case and its renderer twin are
  the whole guard: the rewrite is the kind a normalizer or a query optimizer
  makes on purpose.** *fragility: high*

- **Closed sums are matched one equation per constructor, no wildcard,** so a
  new constructor is named by the compiler. `Filter.hs:302`, `Commands.hs:249`,
  `Store.hs:258`. A `_ ->` added for tidiness turns every future key, kind or
  frame into a silent default instead of a build error. *fragility: medium*

- **The client issues one drift-locked write per file, awaited,** and re-asks
  for the digest rather than reusing a remembered one. `40-popups.js:177`,
  `20-sheet.js:2160`, `50-settings.js:397`. Firing in parallel makes the second
  write drift against the first; a remembered digest across a reload is a silent
  overwrite. *fragility: medium*

- **`TextShow` is never a write-back or wire channel.** It is the lossy REPL
  re-serializer; cells are sliced from spans and the wire value is hand-built.
  `TestSpec.hs:188`, `Query.hs:440`. A `showt` in a write or wire module emits
  the title line alone, dropping planning lines and permuting drawers.
  *fragility: low*

- **A tag layer is minted only by being written to, and only under the FIRST
  config dir's `tags/`;** `POST /config` accepts only a path `GET /config`
  listed. `Config.hs:322`, `Routes.hs:737`, `:748`. That membership check is the
  whole path-traversal defence for `POST /config`. *fragility: low*

- **A child headline is a scope boundary; body under it behaves as body at the
  root.** Every editor operation on a paragraph or list item below a child
  headline acts within the child's own scope, exactly as the same operation acts
  at the entry root — a walk up the owner chain to name a row's container or its
  fold target stops BELOW the child and never crosses it. `Body.outermost`
  filters `Child` owners so `sibling`/`inside` find the item's own list rather
  than the child, and `Doc.foldTarget` filters them so TAB on nested body folds
  nothing rather than the whole child. `Body.elm:721`, `Body.elm:620`,
  `Doc.elm:1066`. Pinned by the paired root/nested browser cases over `pairs.org`
  (`cases.mjs:848` — `+`, RET, `d`, TAB, SPC), each asserting the nested outcome
  equals the root's. A walker that climbs to the TOPMOST owner instead reaches
  past the boundary: `+` on a nested list item drew a dash-less unindented row
  and TAB on it folded the enclosing child — content under a child diverging
  from identical content at the root, only the paired half going red.
  *fragility: high*

- **The browser suite's git control has a tree of its own.** A case declaring
  `repo: true` is served a second, `git init`ed copy — one commit, no remote, an
  identity the repo carries in its own `.git/config` — on its own daemon and
  fresh per case; every other case keeps the plain tree, because a mounted
  control draws `#ghead` above the table and moves every row below it.
  `drive.mjs:368`, `:396`, `cases.mjs:5396`, `:5447`. Serving the repo tree to
  all of them shifts every position-sensitive case by that row, and
  `BREAK=ghead-row` (`drive.mjs:160`) is what turns the control's own case red.
  NO REMOTE IS NO UPSTREAM, so the fixture wears the warned ⚠ dirty and clean
  alike; the glyph is a static span and its hover text is what tells the two
  apart. *fragility: medium*
