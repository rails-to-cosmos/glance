# Changelog for `glance`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

Versions 0.1 through 0.4 were cut retroactively over one dense build: each
section groups a feature arc, and its date is that arc's last commit.

## Unreleased

### Added
- `:` manages tags over the marked rows, or the row at point: a which-key
  palette of the set's own tags where a letter toggles one under dired's
  normalize-up rule — a tag every row carries comes off all of them, one only
  some of them carry goes on to the rows that lack it, and a partial entry wears
  its count. The palette stays open across commits and refreshes from what the
  write answered; `/` finds a tag the tree already holds and `+` creates one it
  does not.
- `POST /command` takes `add-tag {tag}` and `remove-tag {tag}`, batched per file
  and answered per id like every other command; a tag the org parser would not
  read back refuses the whole request.
- `GET /tags?ids=…` reports what the named rows are tagged with and the whole
  store's tag vocabulary.

### Changed
- `@` asks before it applies: a row nothing refers to leaves the table, the
  filter and the trail exactly where they were, with one log line saying so.
- `@` out of an empty filter leaves no crumb — "all rows" already is the empty
  query, which `DEL` reaches without one.
- The value palette drops its key-token column: a keyword's committing letter is
  marked inside the word, bold and underlined in that state's own badge colour.
- `*clear*` commits on `DEL` instead of claiming a letter, so the whole `a`-`z`
  pool goes to keywords and a wide cycle keeps the letter the entry used to take.

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
