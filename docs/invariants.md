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
  the splice engine directly. `Watch.hs:66`, `Query.hs:1099`,
  `TestSelfContained.hs:182`. A write that bypasses it skips the drift lock,
  the external ledger note and the watch nudge, so the store diverges from disk
  and org-glance never learns of the write. **The guard sweep filters to
  `src-web/`, so a splice under `src/`, `src-query/` or `app/` passes green.**
  *fragility: high*

- **Every write is drift-locked.** A write is pinned by a SHA-256 of the exact
  bytes it was read from, and a mismatch aborts before any byte is touched.
  `Edit.hs:205`, `Edit.hs:211`, `Routes.hs:455`. Dropping or defaulting the
  digest turns a stale tab into a silent whole-file overwrite. *fragility: high*

- **The empty digest is the create pin.** An absent file under `""` is created;
  an occupied path under `""` drifts. `Edit.hs:206`, `Query.hs:1095`,
  `Commands.hs:298`, `Config.hs:322`. Making a missing file a hard `ReadFailed`
  breaks blob capture and tag-layer minting; making a present file writable
  under `""` turns capture into a silent overwrite. *fragility: high*

- **`.glance-tmp` is the LAST dot-component of the temp name.**
  `openBinaryTempFile` splits its template at the last dot and the walk collects
  by `.org`. `Edit.hs:219`, `Walk.hs:252`, `TestEdit.hs:426`. Reordering leaves
  an interrupted write as `notes<rand>.org`, which the walk collects, parses and
  serves as real rows. *fragility: high*

- **The rename is atomic and not durable.** The containing directory is fsynced
  after it, and a write that created directories syncs each new one's parent.
  `Edit.hs:230`, `:246`, `:253`, `:264`. Dropping the syncs means a crash takes
  back a write that already answered 200. The sync failure is deliberately
  swallowed — reporting it would refuse a write that landed. *fragility: medium*

- **`Data.Org.Edit` is content-agnostic.** It splices character spans and knows
  no org syntax, so every org-shaped wall (tag charset, keyword charset,
  planning reparse, one-top-entry, trailing space) is owed by the layer above.
  `AGENTS.hs:3179`, `Query.hs:1406`, `TestQuery.hs:1746`. Pushing a check down
  makes it unreachable for callers off that path; pushing one out of `Query`
  lets bytes land that the next parse reads as body text. *fragility: high*

- **A write derives its line ending and its opening from the text it lands in**
  (`eolOf`, `openingFor`), never from a constant. `Edit.hs:94`, `:101`,
  `Query.hs:1571`, `Config.hs:212`. Hard-coding `"\n"` converts a CRLF file line
  by line on every edit; skipping `openingFor` joins an appended `* ` onto the
  last live line. *fragility: medium*

- **Composed lines are `untrailed`, stepping over the terminator so a CRLF
  survives.** `Query.hs:1018`, `:1567`, `Routes.hs:468`. A `T.stripEnd` in its
  place eats the `\r` too, rewriting every line ending in the file.
  *fragility: medium*

- **Anything written back must reparse under the parser's own charsets** — tags,
  keywords and planning entries are validated by reparse of the very line the
  write would produce. `Query.hs:1133`, `:1143`, `Routes.hs:464`, `:475`.
  Loosening a charset is silent: the run falls into title text on the NEXT
  load, long after the 200. *fragility: medium*

- **Deletion is a move.** The blob directory is gzipped under the trash mirror,
  the copy lands before the original goes, and an occupied destination is
  refused. `Trash.hs:38`, `:52`, `Commands.hs:121`. Remove-then-copy loses the
  blob on any IO failure mid-move. *fragility: medium*

- **The ledgers are append-only, best-effort and derived.** One `O_APPEND`
  `write(2)` per line, hand-assembled so field order is the contract, and no
  failure reaches the caller. `External.hs:106`, `:68`, `:99`. An `AppendMode`
  handle remembers its open offset, so concurrent writers overwrite each other;
  aeson would reorder the fields the peer's reader depends on.
  *fragility: medium*

## The store and the routes

- **The `Store` TVar has exactly two writers** — `finishLoading` and `publish` —
  and every route reads it only. `Store.hs:309`, `:334`. A third writer breaks
  the generation-bump/frame-derivation pairing, so live sockets stop receiving
  ops for rows that moved and the ETag stops changing. **Nothing tests this.**
  *fragility: high*

- **No write route writes the store.** `POST /command`, `POST /headline` and
  `POST /config` leave it to the watch to re-read. `Commands.hs:205`,
  `Routes.hs:443`. A route that "helpfully" updates the store races the watch's
  reload, and the client model — which steps off the command's own answer —
  disagrees with what the socket streams. *fragility: high*

- **`nudge` runs on the success branch only.** `Watch.hs:70`, `Commands.hs:234`,
  `TestSpec.hs:802`. Nudging unconditionally costs a re-read per 409; skipping
  it on success leaves the store holding pre-write rows until an inotify event
  that may never come. *fragility: high*

- **A batch is one drift-locked write per FILE, with no rollback across files.**
  A 200 means the command ran, never that every row moved. `Commands.hs:203`,
  `:249`, `:322`. Merging the per-file plans turns a partial failure into a
  whole-request 500 and loses the per-id `results` the shell steps its model
  off. *fragility: high*

- **Every write route answers through `answerWrite`, every body arrives through
  `withBody`**, so 413 outranks every other refusal. `Base.hs:161`, `:182`.
  Taking the body outside `withBody` lets an oversized request be decided by a
  404 first, and `strictRequestBody` pays for the bytes before it can refuse
  them. *fragility: medium*

- **One clock read per request,** taken before any row, and there is ONE
  spelling of it: `Base.today`. `Base.hs:77`, `Commands.hs:257`, `:270`,
  `Routes.hs:250`. Per-row reads let a batch spanning midnight land on two
  days. A read taken BELOW a route's revalidation branch is the same fault
  wearing a cache: the store is unchanged, so a `*today*` query 304s into
  yesterday's rows — which is why the day rides in the ETag unconditionally
  (`Routes.hs:298`), at one extra revalidation a day. *fragility: medium*

## Parsing and the walk

- **Text kept past a document's parse is `T.copy`'d** (`detach`), because every
  cell is a slice of the file's text. `Query.hs:1071`, `:565`, `Config.hs:93`. A
  field that forgets it retains the whole source document for the process's
  life — a residency regression `cabal test` cannot see; only
  `glance scan ~/sync` exposes it. *fragility: high*

- **A parse failure fails exactly one file.** `evaluate` inside `try` scopes it,
  and the store keeps that file's previous rows. `Edit.hs:169`, `Store.hs:213`.
  Dropping the rows empties the table between two keystrokes while a file is
  half-saved. *fragility: medium*

- **A symlinked directory is never followed**, and one `lstat` per entry decides
  what an entry is; a failed stat falls to the keep-on-name branch.
  `Walk.hs:164`, `:169`, `Trash.hs:74`. `doesDirectoryExist` in its place
  reintroduces link loops, and the trash copy would pull a foreign tree in.
  *fragility: high*

- **The walk and the watch share ONE set of path predicates,** reached through
  the `Glance.Query` facade. `Query.hs:652`, `Watch.hs:82`, `Walk.hs:243`. A
  second hand-rolled predicate lets a file the walk never loaded arrive by
  inotify, so the store gains rows the next full load deletes. *fragility: high*

## Shape

- **A fact several readers agree on is spelled in ONE list, indexed by key** —
  `viewColumns`, `docCells`, `popups`, `gluePartFiles`, `keyBindings`,
  `Palette`. `Query.hs:1859`, `Base.hs:124`, `Page/Popups.hs`. Re-spelling a
  membership at a second site is the failure the popup registry records: six
  sibling id lists were hand-edited and the seventh missed, so `#mint` neither
  faded nor dimmed. *fragility: medium*

- **The empty cell sits outside every date comparison, and negation is no
  mirror.** `dated` guards all four operators and both range ends, because `""`
  is below every literal in byte order and an unguarded `<` would serve every
  undated row; `*empty*` stays the one name for those rows. It follows that
  `-k:<D` serves the undated rows where `k:>=D` does not, so the operators do
  not pair off under the sign and no surface may rewrite one into the other.
  `Filter.hs:457`, `table-view.js:504`, `AGENTS.hs:2446`. A tidying pass that
  normalizes `-k:<D` into `k:>=D`, or drops the guard because byte order
  "already sorts an empty cell first", turns nothing red but the one case that
  names the pair (`TestFilter.hs:427`). **That case and its renderer twin are
  the whole guard: the rewrite is the kind a normalizer or a query optimizer
  makes on purpose.** *fragility: high*

- **Closed sums are matched one equation per constructor, no wildcard,** so a
  new constructor is named by the compiler. `Filter.hs:306`, `Commands.hs:211`,
  `Store.hs:245`. A `_ ->` added for tidiness turns every future key, kind or
  frame into a silent default instead of a build error. *fragility: medium*

- **The client issues one drift-locked write per file, awaited,** and re-asks
  for the digest rather than reusing a remembered one. `40-popups.js:172`,
  `20-sheet.js:1062`, `50-settings.js:610`. Firing in parallel makes the second
  write drift against the first; a remembered digest across a reload is a silent
  overwrite. *fragility: medium*

- **`TextShow` is never a write-back or wire channel.** It is the lossy REPL
  re-serializer; cells are sliced from spans and the wire value is hand-built.
  `TestSpec.hs:187`, `Query.hs:1860`. A `showt` in a write or wire module emits
  the title line alone, dropping planning lines and permuting drawers.
  *fragility: low*

- **A tag layer is minted only by being written to, and only under the FIRST
  config dir's `tags/`;** `POST /config` accepts only a path `GET /config`
  listed. `Config.hs:322`, `Routes.hs:614`, `:625`. That membership check is the
  whole path-traversal defence for `POST /config`. *fragility: low*
