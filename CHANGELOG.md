# Changelog for `glance`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

Versions 0.1 through 0.4 were cut retroactively over one dense build: each
section groups a feature arc, and its date is that arc's last commit.

## Unreleased

### Removed
- Virtual tag keys leave `?q=`. An org tag no longer names a filter key:
  `course:text` is free text, colon and all, and `tag:course text` is the one
  spelling — the predicate reads the tags cell, the free text reads the row, and
  nothing expressible is lost. It kills the worst parity divergence the grammar
  had: the keys a query could name were the WHOLE STORE's tags here and the
  LOADED ROWS' tags in `table-view.js`, so one token was a predicate on one side
  of the wire and free text on the other. It also takes `contact:none` with it,
  which meant "tagged contact and the row text holding none" and read like the
  empty-cell rule it was not — a rule now spelled `key:*empty*`, see Changed.
  Two differences are written down rather than papered over: `tag:` matches its column by SUBSTRING where a tag key matched
  whole-tag (`tag:glan` finds `:glance:`), and org spells a tags cell `:web:`,
  so the free text `web:` is still inside every row carrying the tag.
- The archive exclusion is named through the `tag` column rather than by
  `archive:` (`Glance.Web.Filter.namesArchive`), the archive tag having been an
  ordinary virtual key. Any spelling of that predicate counts — negated, quoted,
  beside other tokens. (The spelling is `tag:*archive*` as of the meta entry
  under Changed below; it was `tag:archive` when this landed.)
- `Glance.Web.Filter` sheds the machinery the feature alone consumed: the `Tag`
  field constructor and its arity, `parseFilter`'s vocabulary parameter,
  `FilterEnv`'s tag list (`tagsEnv` is now the tag-free `emptyEnv`), and the
  `tagsOfCell` import. `Glance.Web.Store.storeTags` stays — it is `tag:`'s value
  domain, the tag palette's vocabulary and `namesArchive`'s "is anything
  archived" guard.

### Added
- A row whose subtree holds a link wears an UNDERLINED title, so which rows `o`
  has something to follow is on screen before the press. `/headlines` and every
  streamed row carry `"linked": true` where there is a link and carry nothing
  where there is not (SCHEMA.md's Row, additive and sparse); the renderer
  underlines the `title` cell and leaves its colour alone, which keeps the mark
  clear of the four row washes and the two selection bands — a linked row under
  the cursor still reads as linked. It is every link `GET /links` would report
  rather than the references `ref:` matches, since that is what `o` follows:
  ~/sync carries 4976 linked rows against 1824 referencing ones. An underlined
  row whose only link is a `mailto:` still warns on commit — the underline says
  there is a link, never that a tab can take it.
- `^` sorts the table by the column at point, which is the cell selection's:
  `f`/`l` pick a column and `^` orders by it, echoing `^ → toggle-sort
  (Scheduled ▲)`. A second press reverses it and a third is the first again —
  two states, because the renderer's handle states an order and offers no call
  that takes one off. A whole-row selection is refused rather than guessed at
  (`no column selected — f/l to pick one`), a column that declares no `sortable`
  is left alone, and a held `^` counts as one press. The order survives a filter
  refetch and a socket splice; a remount takes the view's declared sort back.
- Every column of `/headlines` declares `sortable: true`. SCHEMA.md makes the
  field opt-in and this producer opts all six in — a state cycle, a priority
  letter, a title, the tags, and the two dates all mean something in order —
  where `title` and `tag` had been left out. It is what a header click and `^`
  read; a producer's own `sortBy` never consulted it.
- `:` manages tags over the marked rows, or the row at point: a which-key
  palette of the set's own tags where a letter toggles one under dired's
  normalize-up rule — a tag every row carries comes off all of them, one only
  some of them carry goes on to the rows that lack it, and a partial entry wears
  its count. The palette stays open across commits and refreshes from what the
  write answered. `/` and `+` are two doors into one field, completing over the
  tags the set can still be given — the tree's vocabulary less what every target
  already carries — and taking a tag nobody has used yet as typed; `ESC` there
  steps back to the letters.
- `POST /command` takes `add-tag {tag}` and `remove-tag {tag}`, batched per file
  and answered per id like every other command; a tag the org parser would not
  read back refuses the whole request.
- `GET /tags?ids=…` reports what the named rows are tagged with and the whole
  store's tag vocabulary.
- `d`, `D` and `u` delete from the materialize sheet's property panel, the way
  they archive a row in the table: `d` flags, a second `d` or `D` deletes every
  flagged row, `u` unflags, and a held key counts as one press. A property is
  dropped and one of org's three planning rows has its entry cleared instead.
- The arrows step a cell as well as a row: `<left>`/`<right>` are
  `previous-column`/`next-column` beside `b`/`h` and `f`/`l`, and the key line
  is unchanged — an arrow rides behind its letters the way `<up>`/`<down>`
  always have.
- The page says when what is on screen has gone stale: one wash — faded back,
  never blurred — over the table and anything open above it, armed by a view
  fetch out past 300 ms or a socket down past 400 ms, and cleared by the answer
  or the reconnect. The status corner, the event log and the key line stay
  bright, being where a reader finds out why.

### Changed
- **BREAKING: combination is one rule — TOKENS AND, ALTERNATIVES OR.** Every
  `?q=` token narrows, whether or not another token names its key. `tag:a tag:b`
  is a row carrying both and `ref:a ref:b` one pointing at both, as before;
  `state:TODO state:DONE` now asks a cell holding one value to hold two, which is
  no row, **where it used to answer either state**. The replacement idiom is the
  new alternation: `state:TODO|DONE`. A predicate's VALUE splits on `|`
  (`Glance.Web.Filter.alternatives`) and each alternative is read as that key's
  own value, the results OR'd — uniform over every key and every kind of value,
  so `tag:work|home` carries either, `scheduled:2026-08|2026-09` is either month,
  `planned:A|B` is either date cell prefix-matching either, `ref:a|b` points at
  either, and a starred meta alternates like any other value
  (`state:*active*|DONE`, `tag:*web*|*archive*`). A negation covers the whole
  token, so `-tag:a|b` carries neither. **A saved URL or bookmark spelling a
  same-key OR now answers nothing; rewrite it with `|`.**
  Empty alternatives are DROPPED — `a|` is `a`, `|a` is `a`, `a||b` is `a|b` —
  and a value spelled with bars alone is left with none, which narrows nothing:
  one answer for `key:`, `key:|` and `key:||`. The bar is a PREDICATE's: free
  text is the text it spells, bar and all, and a predicate's value has had its
  quotes taken out by the scanner, so a literal bar is free text's alone.
  `namesArchive` reads the alternatives too, so `tag:*archive*|web` lifts the
  archive exclusion the way `tag:*archive*` does.
  What it buys is the arity rule's death: `multiValued` is gone, `compile` is
  `map inverted` over the terms with no grouping in it, and the `multi: true` the
  view declares is left saying only what its name says — the cells hold a list,
  which the whole-tag meta and the renderer's chips read. Parity is kept term for
  term (`table-view.js`'s `queryMatcher`/`tokenTest`), and the shared
  `fixtures/parity/filter-query.json` gains the alternation cases.
- **BREAKING: the empty cell is `key:*empty*`, and `key:none` is a literal
  value.** The bare word reserved a spelling a cell can hold, and that was
  exactly its cost: a state keyword `NONE`, a tag `none`, a title reading `none`
  were unreachable by predicate. The stars carry the meaning now and the word
  carries none — `state:*empty*` is the stateless row, `state:none` is a keyword
  spelled `NONE` — on every column key and on `planned`, so the agenda's query
  is `state:*active* -planned:*empty*`. **A saved URL or bookmark holding
  `key:none` now reads as an ordinary value and matches whatever holds that
  text, which is usually nothing.** No alias, no migration: the point is that no
  bare word is reserved.
- **BREAKING: the archive exclusion is lifted by `tag:*archive*` alone.**
  `tag:archive` is the ordinary substring predicate every other tag value gets:
  it filters, it lifts nothing, and `X-Glance-Archived` still reports what the
  default view withheld from it — so a tree that uses `archive` for something of
  its own can filter on the word without being handed the rows it files away.
  The meta matches the WHOLE tag, where the plain predicate is a substring of
  the tags cell (`:archived:` answers one and not the other). **A saved
  `?q=tag:archive` link stops showing archived rows**; add the stars. Over
  ~/sync at 2026-08-02: `tag:*archive*` serves the 322 archived rows,
  `tag:archive` serves 0 and reports all 322 withheld.
- **BREAKING: `state:active` / `state:inactive` are literal keywords.** The bare
  alias for the two group metas is gone with the rest of the bare words — and it
  was a parity divergence in its own right, since `table-view.js` never had it
  and matched those tokens as badge text. `state:*active*` and
  `state:*inactive*` are unchanged, and they are what the default view, the
  agenda and the state column's `values` have always spelled.
- **A starred word on the `tag` column is that whole tag.** `tag:*book*` is the
  tag `book` where `tag:boo` is any tag holding those letters — the whole-tag
  reading that left with the virtual tag keys, back as a meta on the one
  spelling, decided off the cell so the renderer answers it identically.
  `tag:*archive*` is one instance of it rather than a rule of its own.
- The state palette's take-the-keyword-off entry is `*empty*` rather than
  `*clear*`: it takes the state cell to exactly what `state:*empty*` then finds,
  which is one word for one thing. `DEL` is still its key, the commit is still a
  null keyword, and the log line is still `state cleared`; the pill now says
  `C-c C-t → org-glance-overview:todo (*empty* · 1)`.
- `assets/table-view.js` is resynced from the sibling checkout (`make
  sync-renderer`), which carries the renderer's half of all of the above —
  `*empty*` on every key, the whole-entry meta, `*empty*` at the foot of every
  value domain, a meta taking no sort position — and closes a skew that predated
  this work: the vendored copy still had the virtual tag keys the server dropped,
  and lacked `sortBy` and the title-offer tiers.
- The `tag` column declares `values: ["*archive*"]`, SCHEMA's route for a
  producer meta, so a renderer can offer it: typing `arch` in the filter box
  reaches `tag:*archive*` the way `act` reaches `state:*active*`.
- The settings sheet `,` raises is the page's one place for a preference, in
  three panels: **general** (the default view and the capture target),
  **theme**, and **keywords** (the per-layer `#+TODO:` boxes, which were the
  whole sheet). One list names the headers and what sits under each, so a
  fourth panel is an entry there; the list order is the tab order. Every sync
  rule is unmoved — buttonless, `ESC` or the backdrop syncs the layers that
  moved, `C-x C-s` syncs mid-edit, a conflict waits for a keystroke — and the
  two general fields still ride the system layer's own write.
- The theme selector moved out of the status corner and into that sheet's theme
  panel. Same `auto`/`light`/`dark`, same `localStorage`, same pre-paint boot,
  and it applies as it is picked without closing the sheet. The corner is now
  the connection dot alone — a readout with nothing in it to focus, which
  retires the hand-written `blur()` every control added there used to owe.
- A view now swaps on its answer: `g`, `a`, `@` and a walk back out of a drill
  ask for the whole set once and put it up in one mount, so a complete table is
  no longer replaced by a page of rows and reflowed a moment later. The
  page-sized first fetch stays where it earns its keep, on the boot.
- The event log spells its severity in upper case — `14:03:22 INFO cmd …` —
  which is what a reader scans a screenful of chatter for.
- Whichever pane of the materialize sheet holds the keys says so on its own
  frame, so crossing with `TAB` moves one mark rather than losing it.
- The property panel is a table-view mount, so the renderer draws every list on
  the page: the rows, the stripe, the cursor and the flag wash are its own, and
  the panel keeps the model alone. The sheet's edit fields now sit over the row
  they belong to rather than inside it.
- `@` asks before it applies: a row nothing refers to leaves the table, the
  filter and the trail exactly where they were, with one log line saying so.
- `@` out of an empty filter leaves no crumb — "all rows" already is the empty
  query, which `DEL` reaches without one.
- The value palette drops its key-token column: a keyword's committing letter is
  marked inside the word, bold and underlined in that state's own badge colour.
- `*clear*` commits on `DEL` instead of claiming a letter, so the whole `a`-`z`
  pool goes to keywords and a wide cycle keeps the letter the entry used to take.
- The row's search text is DERIVED from `viewColumns`: a column's cell is now
  `HeadlineRecord -> Maybe Text`, `rowJSON` encodes it (`Nothing` is the same
  `null` it always sent, `Just ""` the same `""`), and `recordOf` ties the record
  through `viewCells` instead of writing the six cells out a second time. What
  went green before this is an APPEND — a seventh column left the haystack six
  fields long and every predicate past it reading the wrong field — and it is
  closed by construction plus a `TestFilter` case quantified over the columns
  there are. A reorder was already caught, by the layout guard, whose hardcoded
  list stays as the now-real oracle. Byte-identical over ~/sync: 12594 rows of
  view JSON unchanged.
- `Glance.Web.Filter` reads a predicate's CELLS as a set (`fieldCells`): a column
  is its one cell and `planned` is the two date columns, so `*empty*` is every
  named cell empty and a value is any of them passing — one arm where the virtual
  key had a matcher of its own. The whole-tag meta stays keyed by cell index, so
  `planned` can never reach it.
- `namesArchive` drops its vocabulary parameter: `/headlines` already asks
  whether the tree carries the tag, and asking twice was the same conjunct twice
  (`V && not (V && N)` is `V && not N`). The claim it used to state moved to
  `TestServe`, over a tree with nothing archived, where it is a fact about the
  answer rather than about the parser.
- `POST /command` is ONE table, name to `{argument shape, dated, edits}`:
  `commandNames` is its keys, `parseCommand` resolves the name before anything
  else and builds a `Command` out of the entry it found, and the per-name guards
  are each command's own `csArgs`. The wildcard that made an unknown name
  `archive` is gone with the case it lived in — the edits are read off the entry,
  and the one command with no row function is the one that makes a row. Every
  refusal message is unchanged, verbatim.
- The HTTP route table declares its METHODS: each entry carries the handler per
  method and how it spells a 405, `HEAD` aliases `GET` in one place, and the JSON
  refusal sentence is derived from the entry's own method names. CLAUDE.md's
  "fixed route table, each entry declaring whether it needs a loaded store and
  whether it is read-only" is true as written now. The 405 surface is byte-
  identical over ten method/path pairs.
- ONE BUTTONLESS SHEET drives both the materialize sheet and the settings sheet:
  one state word per sheet through one writer, one `C-x C-s` ladder, one
  ESC/backdrop ladder, one backdrop registration, and the `C-x C-s retry · ESC
  discard` line spelled once where it had three copies. Each sheet supplies
  `{dirty, flush, refresh, shut, scope}` and nothing else; `activeSheet()` is
  total, since neither sheet opens over the other. Behaviour is unchanged — the
  harness drives both sheets through pristine, dirty, conflict and discard.

## 0.4.0.0 - 2026-08-02

The interaction surface: one keymap, a palette in front of every write, marks
and flags, a navigable filter stack, and a binary that carries its renderer.

### Added
- One keymap for the whole page, carried to the shell as data and read by the
  echo widget, so a binding is spelled once and echoes its elisp command name.
- Row marking — `m`/`u`/`U`/`M` over the renderer's id-keyed set, so a mark
  survives a filter, a page and a refetch.
- Archive flags — `d` flags the row at point and a second `d` archives every
  flagged row; `D` is that second press without the flagging one.
- `t`/`C-c C-t` raise a which-key state palette drawn from `GET /keywords`: the
  resolver's own chain as a Source | Active | Inactive table, one letter per
  keyword committing on its own, `*clear*` at the foot, `/` for a completing
  read.
- `+` captures an entry and `C-c C-s`/`C-c C-d` reschedule, each through the
  palette's text mode; an empty line clears the planning entry.
- `a` applies the agenda view — active rows carrying a date, sorted by schedule.
- `o`/`!` open a row's links, off `GET /links` and the same display rule the
  table renders by; several links raise the palette.
- A navigable filter stack: `@` drills into a row's references (`ref:ROWID`),
  `DEL` strips a token and pops the trail where that empties the query, and the
  trail crosses a remount through `?crumbs=`.
- `planned`, a filter key over the date columns, in parity with the renderer.
- An append-only log strip naming every row a command landed on, and a status
  corner carrying the connection dot and an auto/light/dark theme selector.
- `glance desktop` opens its own WebKitGTK window under the manual
  `native-window` flag (`make native`, plus `vendored/` bindings repointed at
  WebKit 4.1). Closing the window stops the daemon; `--keep-serving` restores
  the borrowed-browser behaviour.

### Changed
- The binary carries the renderer: `assets/table-view.js` is compiled in, so a
  copied binary reads no path off this checkout. `--assets DIR` replaces the
  whole asset set and stays a development flag.
- Keyword classification is one four-rank chain, widest first — the built-in
  `TODO | DONE`, then system, then the row's tags, then the file — read forwards
  by the resolver and by the palette, so what a palette offers is what a write
  accepts.
- `state:*active*` covers the stateless row, so an entry nobody has stated shows
  in the default view.
- The two movement profiles collapsed into one map carrying both spellings:
  `n`/`p` and `j`/`k` step a row, `f`/`b` and `l`/`h` step a cell.
- A row is a top entry with something to show — a level-one headline carrying at
  least one column — numbered `FILE#K` after both filters, so an edit above a row
  keeps its id.
- The table drops the renderer's per-row action hint; the resident key line
  already says it, for every command.

### Fixed
- A headline's star run must end at horizontal space or the line's end, so a
  body line opening `*bold*` stays emphasis (251 corpus lines were rows before).
- A held `t` no longer commits through the palette that press opened.
- `hrDeclared` is forced when stored, so a file's keyword set stopped pinning
  that file's whole element tree.

## 0.3.0.0 - 2026-08-01

The write path: org files edited from the browser, byte for byte.

### Added
- `Data.Org.Edit`, the span-edit engine — char-span splice, optimistic digest
  lock, atomic same-dir temp+rename; untouched bytes stay byte-identical.
- Materialize: `GET`/`POST /headline?id=` serves and replaces a headline's whole
  subtree under a pinned digest; drift is a 409 with the file untouched.
- The subtree lens — body, properties, planning and logbook as four regions with
  one owner per byte. Decompose and recompose is byte-identical; the row id and
  the creation stamp are server-preserved and never travel to the client.
- A buttonless materialize sheet that syncs itself: two panes over one subtree,
  a modal properties panel with keys of its own, a read-only logbook strip, and
  `C-c '` to swap between two-pane and raw org.
- `POST /command` — `set-state`, `set-planning`, `archive` and `capture`. Ids
  group by file and each file is one drift-locked write, answered per id.
- `capture` appends an entry to `#+GLANCE_CAPTURE_TARGET:` (default
  `<root>/inbox.org`, created on demand) under an `:ORG_GLANCE_CREATION_TIME:`
  stamp; a target the walk would decline is refused where the config is read.
- Layered keyword config under `<root>/.org-glance/config/`: system, per-tag and
  per-file `#+TODO:` lines, recognized as a union and classified nearest-scope.
  A config change reseeds and reloads the world.
- `GET`/`POST /config` and a settings sheet (`,`) editing one layer's `#+TODO:`
  block through the ordinary drift-locked write path, so a `#+TITLE:`, a comment
  and a capture template come back byte for byte.
- Tree-wide `#+GLANCE_DEFAULT_FILTER:` and `#+GLANCE_CAPTURE_TARGET:` lines in
  `system.org`, edited from that same sheet and spliced in one write.
- Archived rows are hidden from `/headlines` unless the query names the
  `archive` key; `X-Glance-Archived` counts what was taken.

### Changed
- Streamed frames are id-resolved like every other answer, so editing the loser
  of a shared id streams nothing and a winner going away re-points the id.
- The write routes never touch the store — the file watch stays the sole updater,
  so a browser write and an Emacs write arrive by the same door.

## 0.2.0.0 - 2026-07-31

`glance serve`: org headlines in a browser tab, live.

### Added
- `glance serve --dir DIR` — a local daemon on 127.0.0.1 serving one row per top
  entry: state badge, priority, title, tags, scheduled and deadline.
- `Glance.Query`, the public facade producing the table-view View object; cells
  are sliced from spans and no internal type reaches the wire.
- Live rows: a per-path debounced file watch re-parses one file per event and
  streams row ops over `/ws`, with the bootstrap snapshot taken inside the
  subscribing transaction.
- Server-side filter, paging and ordering — `?q=`, `?limit=`, `?offset=`,
  `?order=` — the query grammar a term-for-term port of the renderer's, so both
  halves answer a query alike.
- `ETag`/304 over a tree fingerprint and a generation, plus gzip: the full view
  went 3.06 MB to 580 KB, and a revalidation costs 0.56 ms against 102 ms.
- The shell — a keyboard-driven page with a resident key line, an echo widget,
  pager and cell-movement keys, and the applied query in the URL.
- `glance desktop` — the same daemon with an app-mode browser window opened as
  soon as the socket listens; `--browser`, `$GLANCE_BROWSER` and `--dry-run`.
- Bind-before-load: the server binds first and answers 503 + `Retry-After: 1`
  while it indexes, so the shell renders an indexing state and polls out of it.
- org-glance's derived mirrors stay out of the walk and the watch;
  `--include-derived` turns that off.
- Per-file reads run on a pool of `getNumCapabilities`, results reassembled by
  input index.
- Shared `ORG_GLANCE_ID`s resolve to one row — a `.org-glance/data/` path wins,
  else walk order — and the losers are counted in `X-Glance-Id-Collisions`.

### Fixed
- Emacs sidecars (the `.#name.org` lock symlink and `#name.org#` autosaves) are
  out of the walk and the watch, so a dangling lock costs no read failure.
- A dropped socket revalidates `/headlines` and re-attaches instead of
  remounting, so a filter, an open sheet and the selection survive a reconnect.
- Residency is bounded: cells are copied out of the document they were sliced
  from, so a loaded store stopped retaining every file it parsed.

## 0.1.0.0 - 2026-07-31

The parser base.

### Added
- An org parser over headlines (stars, TODO keyword, priority, title, tags),
  property drawers, logbook and generic drawers, code blocks, pragmas,
  timestamps, links and tokens.
- A REPL over parsed org with readline history; `glance FILE` seeds its context
  from that file.
- Half-open char spans retained through the parse, tight per headline component,
  which makes the source text losslessly addressable.
- `glance scan DIR` — a corpus oracle reporting files, headlines and ids, and
  validating every span it retained.
- Planning lines: `SCHEDULED:`/`DEADLINE:`/`CLOSED:` in any order on the line
  under the title, last wins per keyword.
- Timestamp ranges, spelled `<a>--<b>` or compactly as `<date wd 10:30-11:30>`,
  preserved as written.
- Context keyword sets, so a `#+TODO:` affects the headlines below it.

### Fixed
- Headlines parse at column 1 alone, so a mid-line `*bold*` stopped being a row.
- TODO keywords are matched case-sensitively and stored verbatim.
- Trailing horizontal space silently destroyed a headline or an indented drawer.
- A date-only timestamp rendered a phantom `00:00`.
