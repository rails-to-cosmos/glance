# Changelog for `glance`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

Versions 0.1 through 0.4 were cut retroactively over one dense build: each
section groups a feature arc, and its date is that arc's last commit.

## Unreleased

### Removed
- **BREAKING: `?order=` is gone from `/headlines`.** `?order=document` and
  `?order=scheduled` were the ordering's own parameter, and the ordering is the
  query's now: **`?q=sort:*none*` is the replacement for `order=document`, and
  naming nothing is the replacement for `order=scheduled`.** The parameter is
  REFUSED rather than ignored — any `order=` at all is a 400 naming its
  replacement — which is exactly why it was spelled out in the first place: one
  silently dropped would serve the default order and read as a working request.
  Gone with it are `pageParams`' `ordering` arm, its two words, and the base
  parameter of `Glance.Web.Sort.sortChainIn`, which now reads
  `defaultSortChain` itself and is a function of the query alone. Neither the
  shell nor the agenda ever asked for `order=`, so nothing on the page changes.
- The status corner is gone whole. `#corner` carried the connection dot (`#dot`
  with `.live`/`.wait`/`.down`) and the coarse-pointer settings gear (`#gear`,
  its `display:none` and the 44px rule in the `pointer:coarse` block); swept with
  them are `const dot`, its four call sites (`socket.onopen`, `socket.onclose`,
  `indexing`, `start`'s catch), the gear's click handler and the
  `#corner`/`#corner:hover`/`#dot*`/`#gear` CSS. The socket's state was already
  said twice over — the stale wash (the whole page fading back once a socket is
  gone, armed at 400 ms) and the strip's own `ws` lines — so a dot was a third
  spelling of one fact, and it cost a fixed box, a z-level and a top padding to
  keep clear of. The indexing state is the strip's `boot info` line alone.
  Consequences: the body's padding goes `34px 24px 24px` → `24px`, so the table
  starts where the page does; the z-index bands are THREE rather than four (echo
  `2`, modal backdrop `100`, sheet `101`, with `3` unused and forbidden by the
  suite); and the stale-wash exemption list is now the event strip and the key
  line. KNOWN GAP, worth stating rather than burying: the gear was the coarse
  pointer's ONLY door to the settings sheet, `,` being untypable there. A touch
  reader can filter and read; they cannot open the settings, and the page has no
  other affordance to offer them. The `pointer:coarse` block keeps its other
  rules — the 44px chip row, its empty-state label, the stacked sheet panes and
  the 16px fields — and the comment owning the question lives inside that block.
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

### Changed
- **Every theme colour comes from one file per theme.** The palette used to be
  spread across three places — the page's `--g-*` set, the renderer's `--tv-*`
  set, and the hand-copied literals plus comments that kept the two in step.
  `Glance.Web.Theme` now holds a palette of ROLES per theme and emits both
  namespaces from it, so a role both spell has one value and the table is
  drawn in the palette the page around it is. Adding a theme is a record in a
  file beside the default theme's and one registry entry; the stylesheet, the
  boot script and the theme selector all read that list. Two light values are
  corrected on the way, the hue held: muted text (3.5:1 → 5.1:1 on white) and
  the focus accent (2.3:1 → 5.0:1) now clear the contrast floor on the page as
  they already did in the table.
- **The sheet's light-theme cursor row matches the table's.** The structured
  document's selection wash was the theme's golden `#FFD600` where the
  table's is the renderer's honeydew `#F0FFF0` — two hues for one meaning,
  side by side. `--g-sel` now copies `--tv-sel` on both themes (dark already
  agreed at `#373D4F`), so the cursor row reads as the same selection
  everywhere it appears.
- **The tag manager wears the band width.** Three short columns — the tag,
  its coverage, its store-wide count — sat in a sheet-sized box 80% of the
  viewport wide; the popup now takes `.pop-band`'s 560px and grows with its
  content to the cap, the state palette's own dress.

- **The `d`/`D`/`u` gesture is ONE implementation over THREE surfaces.** The
  table joined the property panel and the tags popup on `flagKey`, which now
  owns the whole gesture: the cursor read, the two-press rule, the set-or-row
  choice, the spending of the flags before the take, the feature refusal and the
  walk after `u`. Gone are the table's own `archiveFlag`, the fork inside
  `archive` choosing between the flagged set and the row at point, and the flag
  branch inside `mark`. A surface DECLARES a shape — its mount, its cursor as an
  id, what "take these" means, what it LOGS when a flag moves, its walk, and four
  phrases — and WHO SPEAKS belongs to the caller rather than to the shape: the
  popups say `KEY → phrase` out of a listener holding no binding, the table says it
  through `said`, so `d` and `D` echo `archive-flag` and
  `org-glance-overview:delete` out of one gesture. Every echo is
  byte-identical to what it was. Two rules moved and are now uniform: the CURSOR
  is asked for before the FLAGS (so `D` on a renderer that never had flags takes
  the row at point on every surface, where the popups used to refuse), and the
  flags are SPENT inside the gesture rather than by each take. `u`'s
  flag-before-mark stays in `mark`, named as the table's own asymmetry: over the
  popups `u` is the flag key, over the table it is the mark key preferring a
  flag.
- **An edit overlay names its cells BY KEY.** A shape carries `cells: ["title",
  "url"]` beside the `cols` list the server declared, and `cellSpan` — pure and
  order-only — resolves the keys to the leftmost and rightmost indices the
  placement reads. Replaced a positional pair (`cells: [1, 2]`) with nothing
  tying it to the list it indexed: reordering `Glance.Query.linkColumns` put the
  box over the wrong cells, greenly. A key no column carries resolves to nothing
  and the placement is a NO-OP. The suite drives the resolution as the pure
  function it is, against the SERVER's own column declaration.
- **The modal surfaces are one ordered list.** `SURFACES` names the property
  panel (whose listener registers ahead of the dispatch), then the value
  palette, the link popup and the tags popup, in the order their listeners are
  written — rank IS registration order. Three readers where there were three
  restatements: `typing()` asks whether any is up, `cancel` walks the list for
  the rung `ESC` belongs to, and a listener asks `covered(NAME)` whether anything
  above it is up. The five listeners stay, and so does `prompting.raising` —
  `covered` is one surface declining for another, `raising` is one surface
  declining the keydown that raised it, and a rank says nothing about a race with
  one surface in it.
- **Every route resolves the store's ids ONCE, at its own door.**
  `Glance.Web.Store` no longer offers anything that takes a `Store` and answers
  about an id: `storeHeadline` and `storeHeadlines` are gone and `headlinesIn`
  takes the RESOLVED rows. `storeRecords` resolves the whole store each time it
  is named (~28 ms over a 10435-row tree), so the old shapes let a route owing
  two folds pay twice and a fold-per-id spend seconds over a marked set. It is a
  structural rule now rather than a convention, which retires the
  `TestSelfContained` grep that guarded `/tags`'s shape by reading its source
  lines — and the `codeOf` helper with it.
- **`csOne` folds into `csArgs`.** A command spec's shape check is handed the
  IDS beside the `args`, because a shape refusal is about the REQUEST rather than
  about the `args` object alone. Seven of the eight commands ignore the list;
  `edit-link` owns its own "names one row" message and puts it FIRST, the row
  count being the coarsest thing wrong with a request. One flag fewer for every
  entry to answer.
- `Data.Org.Walk.derivedDirs` names `Data.Org.Index.metaDir` rather than
  spelling `"meta"` a second time: that module owns the store layout, and a walk
  declining a directory the index no longer wrote to would be excluding nothing
  while reading as though it were.
- After `d`/`D` archives rows out of the view, point lands on the NEXT SURVIVING
  ROW rather than resetting to row one. dired's rule, and it needed two changes.
  THE ANCHOR: `anchorFor` takes it at FIRE time, since by the time the rows have
  gone the gap they left is exactly what a later read cannot see. It scans from
  POINT — down the page for the first row not leaving, else back up for the
  nearest one, else nothing at all — and carries `from` (the row point was on),
  `id`, `at` (the anchor's place among the SURVIVORS, the fallback for the
  anchor itself vanishing before the landing) and `on` (the page it was taken
  on). THE DOOR THE ROWS LEAVE BY is the FILTERED REFETCH behind the 250 ms
  debounce: `archive` puts an UPSERT on the wire — `Store.streamed` emits a
  delete only for an id absent from the store afterwards, and adding `:ARCHIVE:`
  leaves the row emitted under the same id — so an UNFILTERED client splices the
  row straight back in and point does not move at all. `resync`'s repaint is the
  only other, for a socket that was down while the write landed. All three call
  `settled`, which ALWAYS SPENDS the anchor and lands it only where something is
  owed: spending unconditionally is what keeps it describing ONE watch step,
  where an anchor left armed would let a page turn and somebody else's edit
  minutes later pull the cursor to a row this write had an opinion about. It
  declines to land while `from` is still in the view, and on any page but the one
  the anchor was taken on — `visible()` is ONE PAGE and can say nothing about a
  row outside it. `spent(mine)` drops the anchor when the answer says `from` was
  not archived (a refusal, and an archive over a set point is not in), keyed to
  the anchor it answers for so an earlier archive's answer cannot disarm a later
  one's, and deciding the anchor before `unmark`, which can throw on an asset
  carrying half the mark calls. A `commit` and a `remount` drop it outright: an
  anchor belongs to the view it was taken in.
  THE CARVE: `fetchRows` takes the landing as an argument and the watch's refetch
  passes `settled` where a commit passes nothing. A refetch is the view the
  reader already had arriving again because a file moved, so it is not a new
  question and lands nothing of its own — the renderer keeps the cursor and only
  an armed anchor overrides it. Before this, ANY watch event under a filter took
  a reader back to row one, which is the larger half of what this fixes.
  `land` grew the fallback index that makes the three landings one function
  (apply → row one, pop → the drill's row, archive → the anchor), so the
  first-row rule is now the general rule's default rather than a case beside it.
  What the anchor buys over the renderer's own `keepSelection` is the case where
  rows went from ABOVE point too: that keeps the visual PLACE, which is a row
  further down once they have gone, so it skips one. Its other branches — the
  up-scan, the empty view, a point row that survives — agree with the anchor
  exactly and are guaranteed twice, so nothing exercises the up-scan alone.
  The shell harness grew the socket path to prove it: `frame:upsert=IDS` and
  `frame:delete=IDS` deliver row frames through `socket.onmessage`, the page's
  own door, and `unserved:IDS` drops rows out of what `/headlines` answers — an
  archive being an upsert on the wire and an absence in the answer. The table
  mount grew `upsertRow`/`deleteRow` and models `keepSelection` verbatim,
  including its stale visual index, which is what makes the two halves separable
  at all. Sixteen cases in `TestServe`'s "Shell landing".
  FOUND HERE, FIXED UNDER Fixed below: a freshly mounted table has NO selection,
  the renderer's `selectFirstVisible` having one caller and it being the filter
  box handing over — so `d`, `D` and `RET` on a just-booted page said "no row"
  until the reader pressed `n`, and the harness answering `getSelection` with
  row 0 of the page is why the suite never saw it. The boot now takes the apply
  landing through this same `land`, and the stub models the empty selection.
- The settings sheet's keywords panel is ONE select over ONE box. It showed a
  `<textarea>` per config layer, stacked in `#clayers`, and a tree has as many
  config files as it has tags — the stack was as tall as that number, so the
  reader scrolled past every layer they were not editing to reach the one they
  were. It is now one native `<select id="clayer">` over the layers and one
  `<textarea id="ctext">` holding the SELECTED layer's `#+TODO:` lines verbatim,
  with `#clab` naming that layer (`system · PATH` / `tag · book · PATH`, plus
  ` · not created yet` where the digest is empty) and `#clerr` carrying whatever
  the server last said about a write to it. Order in the select is system first,
  then the tag layers by `localeCompare` (`byLayer`); `sort` is stable, so two
  system layers keep the order the server served them in, which is the walk's.
  The text lives on the LAYER (`crows[i].text`) and the box is a view of
  `crows[cat]`: `takeLayer()` copies the on-screen box back into its layer and
  every door calls it first — the select's `change`, `cdirty`, `flushConfig` —
  so an edit outlives every switch and a switch asks the server nothing. Sync
  semantics are unmoved: buttonless, `ESC` or the backdrop syncs the layers that
  moved and closes, a pristine sheet costs no request, `C-x C-s` syncs mid-edit,
  `conflict` and `error` wait for a keystroke, and it is still one drift-locked
  `POST /config` per FILE that moved, each awaited, each under its own digest.
  NEW: a refusal brings its layer with it — `flushConfig` remembers the first
  refused layer's index and selects it, so the box on screen is the file the
  message under it describes; every refusal is also a `config error` log line
  naming `SOURCE · PATH: message`, since only one can be shown. `SECTIONS`'s
  keywords entry is unchanged (`clayers`, `ceff`, `cfoot`) and its body is still
  markup the list wraps at boot; `.ctext` grew `height:3.4em` → `7em`, and
  `#clayer` shares `#themesel`'s select rule.
- A blob's occurrence history is no longer walked. org-glance snapshots a
  completed repetition as `.org-glance/data/<id>/occurrences/<STAMP>.org`, an
  immutable copy carrying the LIVE entry's `ORG_GLANCE_ID`; it sits inside
  `data`, so keeping `data` kept it, and `isCanonical` ranked it canonical for
  the same reason the live blob is — `beatsForId` called the pair a tie, walk
  order decided which one the table showed, and `POST /headline` would have
  written to whichever won. `Data.Org.Walk.isOccurrence` is the rule and
  `isDerived` covers it, so the watch declines it through the same predicate a
  file the walk never collected is declined by, and `isCanonical` excludes it so
  that under `--include-derived` — which walks it — it loses the id rather than
  tying for it. The name is asked for anywhere under `data`: a two-character id
  is unsharded, so no position test covers both layouts, and the cost is that a
  blob whose sharded remainder spells exactly `occurrences` would be declined
  too. Zero on disk under `~/sync`, so the corpus counts do not move and the
  hazard is closed before it is reachable.
- The link list `o` raises is a READ-ONLY TABLE-VIEW MOUNT, the page's third,
  where it was a which-key palette. Three columns — `type` as a badge, `title`
  as the entry's own description, `url` as the target — and the whole surface is
  `n`/`p` (`j`/`k`, the arrows) to move, `o` to open the link at point, `ESC` to
  leave. The mount is stated read-only: no marks, no flags, no page, no hint
  line. The doctrine it lands is a division of labour between the two shapes: a
  WHICH-KEY palette is for a fixed vocabulary a reader commits from memory (a
  keyword, a tag), where the letter IS the confirmation; a READ-ONLY MOUNT is for
  a list that has to be READ before it can be picked from, where letters are
  noise over the columns carrying the answer. So `t` and `:` keep their letters
  and the links lose theirs, `/` narrowing with them.
- `GET /links` gains a `type` per link: the target's SCHEME, lowercased, with
  the whole `org-glance-*` family folded into `glance`
  (`Glance.Query.linkType`). `https`, `http`, `mailto`, `id`, `file` and
  `glance` are the six the corpus spells and the six the popup declares badge
  hues for — the two a tab can follow warm, the four it cannot cool — and a
  scheme those six do not name travels under its own name rather than being
  flattened away — a 300-row sample of ~/sync answers `glance` 427, `https` 286,
  `file` 68, `http` 18, `elisp` 6, `attachment` 2 and `other` 1, so two types
  nothing declares came back named. A target with no scheme-shaped word before a `:` is `other`,
  which is org's internal `[[Title]]` and `[[*Title]]` and a relative path
  written without `file:`. The honest cost of reading the prefix alone: `[[Meeting:
  notes]]` reads `meeting`, because the alternative is a registry and then an
  unlisted scheme would read as prose. `followable` is now that word rather than
  a regex the page ran over the target a second time.
- The `tag` COLUMN sorts, case-folded (`Glance.Query.sortedTagsCell`):
  `:task:nl:finance:` reads `:finance:nl:task:`, so a tags cell is scanned in
  one order rather than in the author's typing order. Display only. The FILE
  keeps its spelling — the span is untouched, so materialize and the tag edits
  splice into the run as written — and so does `hrTags`, which is what
  `classify` reads and where the order DECIDES which tag's config governs the
  row. `hrSearch` inherits the sort by construction, `GET /tags` and the tag
  palette's first-seen union do not, and no predicate changes answer: `tag:x` is
  a substring of one tag and `tag:*archive*` is membership of the list.
- The materialize sheet's two panes wear one radius. `#mtext` was 4px against
  the panel's `.tv-root` 8px; 8px is the page's shared value, which the log strip
  and the sheet's logbook already wear.
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

### Fixed
- **The property panel (and the tags popup) lost the empty leading column.**
  The renderer's gutter served both the checkbox and the flag's inset edge, so
  a mount that flags without marking — the panel, the tags popup — paid a
  blank 3ch column for an edge. The gutter is the checkbox's alone now; the
  flag's edge rides the row's first cell, and the key column starts flush
  left. Marking tables are pixel-identical, their first cell being the gutter.
- **A property just added is a full-height row while it is edited.** A `+` in
  the panel opened the edit over a row whose two cells were still empty, and
  an empty cell forms no line box — the row collapsed to its padding and the
  overlay anchored to its rect squashed with it, springing to size only on
  RET. The renderer holds the line now (a zero-width space after every empty
  cell), so the fresh row stands as tall as its neighbours from the first
  frame.
- **A cell edit from the sheet no longer poisons its digest.** A `set-title`,
  state, tag or priority write from the materialize sheet goes through
  `/command`, whose per-id 200 carries the file's new digest — but the sheet
  kept the old one until the watch frame re-read it, and that re-read is
  guarded off under an open edit or the panel's keys. Every subtree commit
  inside the window — a checkbox, `C-x C-s`, the panel's flush — 409'd at
  `conflict` for the reader's own landed write. The sheet now re-pins off the
  command's own answer, the tags popup's documented rule one surface over.
- **Org links no longer flash raw on a sheet refresh — the links ride the
  materialize now.** They travelled on a second request (`GET /links` beside
  `GET /headline`), so every fill had an async gap and the frames in between
  drew `[[url][desc]]` where the reader had been reading `desc`. The gap is
  gone structurally: the materialize answer carries the row's whole link scan
  beside the text it describes — one request instead of two, links atomic
  with their text, compact from the first frame on every fill — and the
  element's `o` opens off the held answer without asking the server. `/links`
  stays as the table popup's and `edit-link`'s route, built by the same
  `linkJSON` the materialize rider uses.
- **An element commit no longer reverts the sheet to the store's stale copy.**
  The re-read a successful commit fires reaches the store before the watch
  has re-parsed the file, so it answered with the PRE-write subtree — the pane
  flipped back to what the file just stopped saying, and the stale digest it
  carried poisoned the sheet's pin, so the NEXT write landed at `conflict`.
  A body-only edit emits no socket frame, so nothing ever corrected either.
  The reload now drops any answer whose digest is not the write's own receipt
  — the model the write was built from stands, redrawn — and retries once for
  the server's canonical reading after the watch has caught up. Found by the
  checkbox toggle, whose flip made the revert visible; it guarded every
  paragraph, table-line and deletion commit the same way.

### Added
- **`SPC` (and `C-c C-c`) toggles an org checkbox in the materialize sheet.**
  On a list item wearing a box — `- [ ] step`, numbered items included — `SPC`
  at the stop flips it org's own way: `[ ]` checks, `[X]` clears, the partial
  `[-]` a parent inherits checks. The write splices that item's lines and
  nothing else, drift-locked like every element commit, and the echo names
  `org-toggle-checkbox` with the state it landed. `C-c C-c` with no element
  open is the same toggle — org's own second meaning of the key — and still
  says `nothing open here` away from a box; `SPC` off a box refuses with
  `no checkbox here` and writes nothing.
- **The filter palette completes `columns:` the way it completes `sort:`.**
  After `columns:` the suggestion list offers the view's own columns, a comma
  re-opens the domain the way `->` re-opens the sort's — the set is completed
  one column at a time and stays one token — and a name already in the set is
  not offered twice. A name the view does not carry stays writable: it is the
  producer's custom property column, so the list is vocabulary, never a wall.
- **`columns:` shapes the table from the filter box.** A third view token
  beside the filter and the sort: `columns:State,Title,Tags` shows those
  columns in that order and narrows nothing — typed through `/` like any
  other token, one chip in the strip (same shape, the link hue instead of
  frost, so it reads apart from both the filter and the sort), `DEL` takes
  it whole and the default six come back. Names match case-insensitively
  against the view's keys and headers alike (`Tags`, `tag`, `#` all land);
  a name the view does not carry is a CUSTOM column reading that headline's
  own property drawer — `columns:state,ORG_GLANCE_ID` puts the id on
  screen — and `Closed` is the planning line's own timestamp. The minimal
  set is Title: a set naming it keeps it where it was put, one without it
  gets it first, and an empty list (`columns:`) falls back to the default
  view. Repeats compose in written order, a name named twice keeps its
  first place; a negation or an alternation is the whole request's 400
  naming the token. Server-shaped end to end: `/headlines` declares and
  fills the picked set, a picked `state` column keeps its badges, and the
  shell remounts whenever an answer's columns differ from the mounted ones.
  The query is the one carrier of a view, so `P` pins filter, sort AND
  columns in the config's one `#+GLANCE_DEFAULT_FILTER:` line and `g`
  applies all three back — no new mechanism, the tokens simply ride.
- **RET on the headline line in the materialize sheet opens its title, in
  place.** The whole line's edit is its title — state and tags have their
  popups, the priority ring is pressed — so the element-grain RET and RET on
  the title cell are one door, with no `f` spent picking the cell first. The
  editor is ONE field laid over the title text alone: the stars, the state
  badge and the tags stay on screen around it, the field wears the document's
  own font with no padding jump, and only the ground says an edit is open.
  A headline with no title yet opens it empty and `set-title` writes one in.
- **The native window opens `o`'s links in a reading pane of its own.** The
  window has no tabs to switch to, so an `http(s)` link opens in a popup — 80%
  wide and 90% tall of the main window, centred over it, transient so the
  window manager stacks the pair — with ESC or the manager's close ending it
  and the table untouched underneath; any other scheme still goes to the
  desktop's own handler. Before this, a scripted `window.open` in the native
  window went NOWHERE: it fires WebKit's `create` signal, which nothing
  answered, so the old system-browser handoff (wired to the policy door alone)
  had never fired for `o` at all. The scripted half is intercepted at document
  start — `window.open` is patched to post its URL to a script-message handler,
  the same shape an iOS/WKWebView port must use — because answering `create`
  with a view aborts the whole daemon on current WebKitGTK when the open was
  made with `"noopener"` (a disengaged `WindowFeatures` optional inside the
  engine). A real `target="_blank"` anchor keeps the policy door and lands in
  the same popup.
- **`+` CAPTURES INTO THE STORE, under a tag and through that tag's own
  template.** The key is a chain of prompts now — which tag, whatever that tag's
  capture template asks (`%^{PROMPT}`, one field per prompt in template order),
  then the line — and ESC at any of them ends the whole thing with nothing sent.
  A TAGGED capture writes a real org-glance blob: a minted `ORG_GLANCE_ID`
  (`org-id-uuid`'s own version-4 form), org-glance's sharded
  `data/<2>/<rest>/data.org` path, the tag on the headline, the creation stamp in
  the drawer, and the `meta/EXTERNAL.jsonl` line that makes `M-x
  org-glance-graph:refresh-external` adopt it — so a capture from a phone lands
  as a first-class org-glance headline and Emacs sees it on its next refresh.
  Leaving the tag EMPTY is the inbox capture exactly as it was, bare `* text`
  plus a creation drawer, byte for byte.
- **A tag's capture template is the first heading of its config layer**, which is
  the file that already carries its `#+TODO:` cycle — org-glance's own
  convention, read the way `org-glance-tag-config--entry` reads it (from the
  first `*` line to the end of the file, right-trimmed), so `book.org`'s `* Book`
  over `*** Notes` is ONE template. `system.org`'s is the tree's default, and a
  tag no layer configures takes the bare `* %?`. The expansion subset is `%?`
  (where the typed line lands, and a template without it is refused naming it),
  `%U`/`%T` (the moment of capture, inactive and active, one clock read per
  request) and `%^{PROMPT}`; **everything else copies through verbatim**, so a
  template using a code this server has never heard of captures it literally
  rather than being silently emptied.
- **`GET /capture[?tag=NAME]`** — what a capture will ask for before it asks it:
  `{template, prompts, tags, codes}`. `prompts` are the template's own asks in
  template order (one spelled twice is asked once), `tags` is the tree's whole
  vocabulary for the tag prompt to complete over, and `codes` is the expansion
  subset with a line of meaning each. The subset is spelled ONCE, server-side:
  what this route serves is what expands and what the settings box completes.
- **The settings sheet's selected layer gains its capture template**, verbatim,
  beside its cycle — the server slices the heading's extent and splices what
  comes back, in the SAME `/config` write, so a layer is still one file, one
  digest, one splice. `%` in the box raises the code list the server served.
- The answer to a capture names **the row it made** — the minted id for a blob,
  the target file's `FILE#K` ordinal for an inbox line — and the cursor lands on
  it when the watch delivers it. A row the view has not got (a filter that hides
  it, a watch step that has not arrived) leaves point exactly where it stands.

- LINKS ARE WRITEABLE, which is the write boundary the popup was waiting on.
  `GET /links` now carries a per-link `span` — the half-open CHAR range the link
  occupies in the FILE — and the file's `digest`, and `POST /command` implements
  an eighth name, `edit-link {span, target, desc}`, which splices exactly that
  range. The scanner grew the offsets rather than gaining a second pass: one
  `linkParts` answers all three questions asked of a bracket link (what it SHOWS,
  where it POINTS, where it SITS), `linkAt` reports the WIDTH it consumed so a
  scan costs the links it finds rather than the tail behind each of them, and
  `subtreeLinks` shifts the subtree scan's spans into document offsets — the
  currency `Data.Org.Edit` splices in. A target spelled twice is still ONE entry
  and the entry is now the first occurrence's description AND span, so an edit
  through a deduplicated link edits the first spelling and the others stand.
  THE FORM IS PRESERVED, which is what makes it a link edit rather than a rewrite
  of the text around one: `[[T][D]]` keeps its description under a target-only
  edit, `[[T]]` stays desc-less, a plain URL swaps its target and stays plain,
  and a description ARRIVING is the one thing that changes a shape — a plain URL
  has nowhere to write one, so it brackets. ABSENT IS NOT NULL, the `args`
  discipline (`.:!`) reaching its first non-keyword field: a request saying
  nothing about the description leaves the author's, `null` takes it off, and a
  description that SHOWS nothing is the null spelled another way, since
  `[[T][]]` shows its target — the emptiness test strips and the value is
  written verbatim, content being nobody's to trim, which is the target's own
  rule (a whitespace target is refused, a spaced one is written as given). TWO
  WALLS, both 400 naming what they turned down:
  the span must sit inside the ROW's own subtree — a span outside it would let
  one row's write reach bytes no reader of that row was shown, under that row's
  digest — and cover exactly one link edge to edge; and the REPLACEMENT must read
  back as THE LINK IT CLAIMS TO BE, which reparses and COMPARES rather than
  checking the shape (a target spelling `a][b` renders `[[a][b]]`, one link
  pointing at `a` described `b`, neither of them asked for). A newline in either
  half is refused ahead of both, being the one thing reparsing cannot catch: the
  scanner has no line rule, so the link reads back as itself and lands a column-1
  star that the ORG parser reads as a new headline. `Data.Org.Edit` is
  content-agnostic by law, so this is the layer that owes all three. `edit-link`
  is also the only command that names ONE ROW: its args name a row's own
  CHARACTERS, so a span means nothing to a second row and over two files would
  name a different range in each. That rule is its own `csArgs`, which is handed
  the ids beside the `args` (see Changed).
- `RET` over the link popup EDITS the link at point, and the stub that named the
  missing write is gone. The title and url cells become fields over themselves
  (`LROW`, the shared edit overlay's THIRD shape), `TAB` hops, `RET` commits
  `edit-link` over the span `/links` handed out under the digest that answer
  carried, and `ESC` restores — the property panel's edit model exactly, so a
  panel row, a tag and a link are edited alike and the derived type cell never
  opens. The overlay's `cell` flag became a `cells: [FROM, TO]` RANGE over the
  row's non-gutter cells, which is the one generalization the third surface
  needed (`[0, 0]` for the tag rename, `[1, 2]` for a link, absent for the whole
  row). `fire` gained a trailing `pin`, so a command measured against a text can
  say which one; the commands naming a PROPERTY of a row send none. The page
  holds no bracket grammar and no offsets of its own: it sends the range it was
  given and the two strings a reader typed, and the untouched FIELD is what makes
  absent-not-null reachable — the description field opens on what the link SHOWS,
  which for a link carrying none of its own is its target, so a field left alone
  sends no `desc` at all. THE POPUP CLOSES ON THE PRESS, both outcomes alike,
  which is `o`'s own rule and is forced rather than chosen: the spans it holds
  describe a file the write has just moved, the store does not know yet
  (`/command` never writes it — the watch does, a debounce later), and a re-read
  HERE would answer with what the file said BEFORE the write, which is the tags
  popup's own documented reason for folding answers instead. `o` again is one
  keystroke and comes back with fresh spans, descriptions and types. KNOWN
  CONSEQUENCE, stated rather than worked around: the popup is also the only
  editor, so a row holding exactly ONE link is followed and never listed, and
  that link has no editor — a key that LISTS whatever the count is would settle
  it.
- The log strip's height is a preference, and it is STATIC. It grew to what had
  arrived and stopped at seven of its own line boxes; it is now exactly that
  many, always, whatever it is holding — a fixed frame the messages scroll
  inside. A strip that grew was the table resizing under a reader's cursor every
  time a write logged a line, which is the one thing a keyboard surface must not
  do, and a quiet page now reads the same as a busy one. The figure is a
  `localStorage` preference edited from the settings sheet's GENERAL panel
  (`#clog`, the third row under `default view` and `capture target`). The
  stylesheet keeps the arithmetic and declares the default — `#log{ …
  --g-logn:7; height:calc(var(--g-logn) * 1.5em + 2 * 6px + 2 * 1px);
  flex:none … }` — and the knob
  writes a NUMBER onto the element (`style.setProperty("--g-logn", …)`), so
  there is one formula in one place and a page whose glue has not run — or a
  reader who never touched the field — gets the same figure the sheet
  would put back. Stored under `glance-log` beside `glance-theme`, applied on
  boot and on every accepted keystroke, on `input` rather than `change` so the
  field is a knob rather than a form. `LOG = {key:"glance-log", def:7, min:1,
  max:50}` in the glue is mirrored in Haskell as
  `logLinesDefault`/`logLinesMin`/`logLinesMax` and `logLinesBand` (the
  placeholder's `1–50`) — the same constants the stylesheet's declared value is
  spelled from, so the two cannot drift. Blank is the default, which is how a
  reader asks for it back, and it REMOVES the key rather than storing `""` — a
  preference spelling the empty string is still a preference. A whole number
  inside the band is that number;
  everything else is DECLINED rather than clamped, so the height a reader had
  stands, nothing is stored, and reopening the sheet draws the preference back
  over the refused value — half a number on the way to a whole one is the
  ordinary case of that. A stored value the band no longer takes falls back to
  the default, the boot reading it through the same check. The panel says where a preference is READ rather than
  what writes it: `cmoved` never sees `#clog`, so the knob costs no request and
  cannot make a pristine sheet dirty. The table takes the whole of the rest
  (`#app` is `flex:1 1 auto`, the strip `flex:none`). `LOGCAP` = 500 is the
  strip's RING (how many lines it keeps) and is a different limit, unchanged.
- **The order is part of the query.** `sort:COL` orders the answer by that
  column and `sort:COL:desc` reverses it; written order is precedence, so
  several tokens compose a chain (`sort:state sort:deadline` is state with
  deadline settling its ties). The token NARROWS NOTHING — it states an order
  and leaves the set to the predicates beside it — which makes it the one key in
  the grammar that is no predicate, and `?q=sort:deadline&limit=100` is
  therefore the first hundred rows OF THAT ORDER rather than a hundred arbitrary
  rows a browser then re-sorts. What the view declares is the effective chain,
  so what a client is told and what it is served stay one fact.
  A query naming any sort key replaces the default chain; naming none leaves it
  standing, which keeps the default invisible until a reader diverges from it.
  Refusals are per token and name it: one column, one direction, so a negation
  (`-sort:x`), an alternation (`sort:a|b`), a column that is not there and a
  direction other than `asc`/`desc` are each a 400, as is a column named twice.
  DOCUMENT ORDER is a token too: `sort:*none*` is the EMPTY CHAIN — walk order
  whatever the limit, and no `sort` field on the wire for a renderer to
  re-apply — and it wears the stars because it is a reserved meta rather than a
  column, the family `*empty*`/`*archive*`/`*active*`/`*inactive*` already being
  in. It ADMITS NO COMPANIONS: another sort key beside it, or a direction on it,
  is a 400 naming the meta, two orders in one query being a reader who meant one
  of them. The half-typed `sort:` is no companion, naming nothing either way.
  `^` is that grammar's key: it composes the chain the way it always did — the
  column at point to the head, or a flip where it already leads — and now WRITES
  IT INTO THE QUERY, so the press is an ordinary commit. The URL carries the
  order, `DEL` walks the keys back off one at a time, a `?q=` link opens in the
  order it names, and the daemon is asked for the order the reader just stated.
  The shell keeps no record of a sort and asks the renderer for none.
  New module `Glance.Web.Sort` beside `Glance.Web.Filter`: one query, split by
  what a token does — narrow, or order — over one scanner.
- **The default order opens on state, in the tree's own cycle.**
  `defaultSortChain` is state, title, deadline, scheduled, all ascending, with
  state read by BADGE PALETTE position — which is the order your `#+TODO:` line
  spells — so the table opens with the work in org's order rather than
  alphabetically, and the title settles rows sharing a state. Priority left the
  chain: a fifth key behind four that have already separated nearly every pair
  of rows, and `sort:priority` is how to ask for it.
- **`a` carries its own order.** The agenda query is
  `state:*active* -planned:*empty* sort:scheduled`, so the whole canned view is
  one string: the server answers page one in that order, `DEL` walks out of it
  like any other token, and nothing has to be asked of the renderer once the
  rows are up.
- The tags list on `:` is a MUTABLE MOUNT — the page's fourth table-view mount
  (`#ttable`), after the table, the property panel and the link popup. Three
  columns, declared server-side in `Glance.Query.tagColumns`: the tag, its
  COVERAGE over the rows the command would run over (`all`, or `2/3`), and how
  many rows the whole tree has under it. `GET /tags` grew the third as `counts`,
  one pass over the store's rows per request — `stTags` counts FILES, which is a
  different question. A tag is its row's id, so a flag, the cursor and a rename
  all name the same thing after any number of writes.
  It replaces the which-key palette that carried this list, and the letters go
  with it: a keyword is a single word committed from memory and a tag over a set
  of rows is a RECORD a reader has to read. The tell was the muted `3/5` aside —
  a palette entry that needs a note about itself is a record wearing a letter.
  The which-key machinery is now the state palette's alone (`prompting.sticky`,
  `letterMode` and `prompting.letters` are gone with it).
  Gestures are the ones this page already spells, borrowed rather than invented:
  `d` flags a tag red and a second `d` — or `D` — removes every flagged tag from
  every target carrying it, one `remove-tag` per tag and the flags spent, which
  is dired's and the table's own archive gesture; `u` unflags; `+` raises the
  value palette's completing field over the addable vocabulary and adds; `RET`
  opens the tag cell as a field over itself, which is the property panel's edit
  overlay over one cell.
- `POST /command rename-tag {from, to}` — the seventh command, and the write
  behind that `RET`. `Glance.Query.renameTagEdits` REPLACES the entry where the
  author put it, so `:a:work:b:` renamed to `projects` is `:a:projects:b:` and
  the run's order, its delimiters and every other byte stand. It is a command
  rather than a `remove-tag` and an `add-tag` fired in turn because of what those
  two edit sets compose to. They APPLY — removing a LAST entry ends where the
  addition inserts, and `applyEdits` rejects only overlap — and they write the
  wrong thing twice over: the addition's anchor is measured before the removal,
  so a lone tag lands flush against the title (`* TODO Ship itprojects:`), and
  independently of the anchor `add-tag` appends at the run's end, so an entry
  with neighbours comes back moved to it. The pair would also be two writes under
  two digests where this is one drift-locked splice per file. One tag once: the
  first entry spelling `from` becomes `to`, further ones are cut, and a row
  already carrying `to` loses `from` instead. Both ends take the parser's charset
  wall, a row not carrying `from` costs no edit, and rename-then-rename-back is
  the identity on the bytes.

- The view declares a SORT CHAIN rather than one key (`declaredSort`, over
  `Glance.Query.defaultSortChain` — whose keys are the entry above, state
  leading). SCHEMA.md's `sort` takes an array for exactly this and both
  renderers run every key of it, so the keys behind the first fire only where
  two rows are alike on everything ahead of them. The browser draws the chain as
  a chip per key beside the filter's chips; `table-view.el` prints it on its
  hint line.
  ONE list, read twice — `declaredSort` spells it onto the wire and
  `sortedForViewWith` arranges the rows by it — which is the whole reason a
  producer sorts at all: a page cut out of a different order than the one
  declared is a different set of rows than the table would have put there.
  The arrangement is the renderers' rules, term for term: empty cells last on
  each key and OUTSIDE that key's direction (a blank is a fact about a cell,
  never about a row), the state column by its badge PALETTE position with
  everything unlisted tying at the back, a stable sort so rows equal on every
  key keep walk order. Text compares case-FOLDED, the way the tags cell
  already folds: the browser collates with `localeCompare`, which is
  case-insensitive at its primary strength, and raw code-point order would put
  every capitalised title ahead of every lowercase one where the table shows
  them interleaved. Titles differing only by punctuation or script can still
  land elsewhere than `localeCompare` would put them — the residue of having no
  collation library on this side.
  `sortedForViewWith`/`orderedForViewWith` take the state palette; the
  palette-free `sortedForView` derives one from the records it is given, which
  orders those records correctly and can differ from the store's in one case
  (two files declaring the same keywords in opposite orders, and a filter that
  hides every row of the first). A caller holding the store's palette should
  pass it.
- **This daemon writes a file into org-glance's `meta/` for the first time:
  `EXTERNAL.jsonl`.** Every write to a BLOB — a document under
  `.org-glance/data/` — appends one JSON line, `{"id", "at"}`, naming the blob's
  first headline's `ORG_GLANCE_ID` and when it was written, so org-glance can
  see that its index is behind and refold that entry. It is the answer to a
  measured problem: a live store went from 21 to 39 drifted rows in ONE DAY of
  browser use, the index having no way to learn about a write it did not make.
  ONE DOOR carries it — `Glance.Query.replaceSpans`, which is the only
  `editFile` caller and the way all four write paths leave — and the note rides
  the SUCCESS branch alone. `Data.Org.Edit.editFile` itself was rejected as the
  site: it is content-agnostic by law and a note is content. `Data.Org.External`
  owns the format, the path and the append, and `isBlob` decides which writes
  note at all — documents, config, overviews and occurrences note nothing.
  Append-only, hand-assembled field order (so the line is a contract rather than
  whatever a `ToJSON` instance emits), and a failed note is SWALLOWED: the
  rename already happened, and a write that succeeded must not be reported as
  one that did not.
  Found by the concurrency test rather than by review: `BS.appendFile` is NOT
  atomic — eight concurrent writes produced FIVE lines, `AppendMode` writing at
  the offset the handle was opened at — so the append is an `openFd` in append
  mode and one `fdWriteBuf`.
- `glance scan` folds org-glance's write-ahead index and says where it and this
  parser disagree: `org-glance index: 21 rows disagree (20 state, 1 archived)`,
  with the store, the fold's counts, the blob counts, and up to ten disagreeing
  ids carrying both values. Read only — the one thing here that opens
  `.org-glance/meta/` at all, and it never writes, creates or seals anything.
  The fold is `org-glance-graph--latest-records` term for term: the MANIFEST's
  sealed segments oldest-first, the open `headlines.jsonl` last, the latest
  record per `ORG_GLANCE_ID` superseding every earlier one, tombstoned ids out,
  only the open segment's final line forgiven for being torn. It compares the
  TODO keyword always and the archive flag only where the record carries the
  key — `archived` joined the record schema late, so absent is a third answer
  rather than false. Stores are each root's own `.org-glance/meta` plus every
  `meta` the walk declined, so a nested store is compared without a second
  traversal; a tree org-glance never indexed prints no line.
  ~/sync/views at 2026-08-02: 6502 records read, 6071 live, 0 tombstones, 0
  malformed; 6063 blobs parsed; 21 rows disagreeing; 0 unindexed blobs.
- The same report counts what the instrument cannot compare: `blobs 6063
  parsed, 51 carrying no id` is blobs this parser read and found no
  `ORG_GLANCE_ID` in, which with the 8 parse failures accounts for all 59
  `records without blobs` — so none of that number is org-glance indexing
  something that is not there. 28 of the 51 are one parser gap: a non-English
  weekday in the planning line (`CLOSED: [2025-12-04 do 22:34]`) fails the
  planning parse, the property drawer is then no longer the next thing, and the
  headline loses its properties whole. Reported rather than fixed; without the
  count it read as index lag.
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
  (Scheduled ▲)` and, past one key, the length of the chain. A whole-row
  selection is refused rather than guessed at (`no column selected — f/l to pick
  one`), a column that declares no `sortable` is left alone, and a held `^`
  counts as one press. What the press DOES with the order is the query entry
  above: it writes the chain into `?q=` and the daemon answers in it.
- Every column of `/headlines` declares `sortable: true`. SCHEMA.md makes the
  field opt-in and this producer opts all six in — a state cycle, a priority
  letter, a title, the tags, and the two dates all mean something in order —
  where `title` and `tag` had been left out. It is what a header click and `^`
  read; a producer's own `sortBy` never consulted it.
- `:` manages tags over the MARKED rows, or the row at point, under dired's
  normalize-up rule: a tag every target carries comes off all of them, one only
  some of them carry goes on to the rows that lack it, and a partial entry says
  how far it reaches. It stays up across its own writes and refreshes from what
  each one answered. `+` opens a completing field over the tags the set can
  still be given — the tree's vocabulary less what every target already carries
  — and takes a tag nobody has used yet as typed. The SURFACE it draws on is the
  mutable mount above, which replaced the which-key palette this landed as.
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
- **The shell's script is a real JavaScript file.** 5.2k lines of JS lived as
  a Haskell string list for the sake of nine interpolations; `assets/glue.js`
  is now a file compiled into the binary the way the renderer is, with every
  server value riding one `cfg` JSON blob the page emits (eight per-build
  constants and the per-request default view). Editor modes, linters and
  honest diffs apply to the shell for the first time, and `--assets` gives
  live glue hacking with no rebuild. The migration is byte-proven: the old
  output and the new file differ in exactly the nine known lines.

### Fixed
- **A linked title shows its description alone.** The material headline drew
  the raw `[[url][desc]]` as the cell's own text and appended the rendered
  description beside it, so a browser showed both. Exactly one path writes
  the cell now; the harness models the browser's reading (own text plus
  children) so the double can never go green again.
- **A held key is one press, even when the event lies.** WebKitGTK's
  auto-repeat can arrive with `repeat` unset, which disarmed every
  once-per-press guard in the native window — a held `DEL` stripped the whole
  query, and a held `d` could flag and archive in one press. Repeat is now
  derived from the missing key release, whatever the event says.

### Added
- **`make run`, `make run-native`, `make run-wasm`.** All three read `.env`
  (committed; `GLANCE_DIR`, `GLANCE_PORT`, defaulting to `~/sync/views` and
  7777). `run` opens the browser flow, `run-native` the WebKitGTK window
  through its own project file, and `run-wasm` builds the new
  `glance-wasm-probe` and runs the core inside wasmtime over the tree —
  walk, parse, rows — with the directory preopened read-only.

### Fixed
- **The pin actually writes.** Two faults hid each other: the server still
  required `lines` on `POST /config`, so the pin's request was a 400 — and the
  shell's fetch resolves refusals, so the pin logged "pinned" while the file
  never moved. Absent `lines` now leaves the `#+TODO:` block standing, and a
  refused pin is a thrown error and one config error line.

### Changed
- **`DEL` takes the sort chip whole.** The chain used to give up one
  tie-breaker per press (`sort:title->priority` → `sort:title`); a chip
  erasing by a different rule than its neighbours made `DEL` a thing to
  think about. One rule now: the last chip goes whole, sort or not.
- **The settings sheet's default view is the main page's own filter widget.**
  A table-view composer — the omnibox bar and the chips, completion and DEL
  included, with no table behind them — replaces the plain field. It opens
  showing the served value, offers the tree's own values, and a composed
  query rides the system layer's drift-locked write.

### Added
- **`P` pins the applied view as the tree's default.** The query on screen —
  filter tokens and sort tokens alike — becomes `system.org`'s
  `#+GLANCE_DEFAULT_FILTER:` line, through the same drift-locked `/config`
  write the settings sheet rides. The sheet's "default view" field is
  read-only now: composing a query belongs to the table's own widget, and the
  field shows what is pinned.

### Fixed
- **`f` recurses into a nested list.** A deeper item used to ride inside its
  parent as opaque text, so the grain stopped one rung short. The grain is a
  LADDER now: an item carrying a nested run is itself a parent — `f` descends
  one rung, `b` climbs to the immediate owner, `n`/`p` clamp to one parent's
  run, a flag on any rung deletes its whole range, and the draw shows each
  rung inside the one above it.

### Changed
- **The material document walks on two axes.** `n`/`p` step siblings at the
  cursor's grain and never dive — a list, block or table is ONE stop, so
  holding `n` skims the document at reading grain — and `f`/`b` move the grain
  itself: `f` enters a composite's leaves or a headline's cells (and refuses,
  with an echo, at the finest), `b` re-selects the whole in one press and is a
  spoken no-op at the element grain — never a close. `l`/`h` and the
  horizontal arrows keep the within-grain cell walk. The table's own habit —
  `n`/`p` rows, `f`/`b` cells — now serves both surfaces; the earlier one-walk
  grain (where `n` stepped into every item) is retired.
- **Capture is one form.** `+` used to run a chain of palettes — tag, each
  template prompt, the line — and every step closed and reopened the overlay,
  which read as a blink. One popup now holds the whole flow: the tag field
  with the vocabulary narrowing under it, the template's fields grown in place
  when the tag settles (RET or TAB), and the line last. RET moves forward and
  captures at the line; ESC anywhere leaves with nothing sent; a refusal keeps
  the form up with everything typed, so fixing a line is an edit rather than a
  retype.

### Fixed
- **A link listed under two descriptions serves both.** `/links` deduplicated
  by target alone, so one `elisp:` command written under `pnl` and under
  `alpha:grafana` served the first and silently swallowed the second — which
  read as the link not parsing. The dedup key is now the (target, shown) pair a
  reader can see: repeats under one look still collapse to the first spelling
  (and its span), distinct looks are distinct entries in the popup, the
  document render, and `o`.
- **A warning cookie no longer costs the planning line.** org's agenda
  warning/delay (`<2026-01-01 Mon +1m -3d>`, first-only `--7d`) used to block
  the timestamp's closing bracket: the stamp failed, the planning entry
  backtracked, and the line — with the drawer and the id behind it — demoted to
  body (the Dutch-weekday loss class). A timestamp now takes one repeater and
  one warning cookie in either order, stores the warning (`tsWarning`), and
  re-renders it byte for byte; a lone `-3d` is read as org reads it — a
  warning, not a minus-signed repeater — with the render unchanged either way.
- **`#+SEQ_TODO:` and `#+TYP_TODO:` configure the cycle.** org's two older
  spellings fell through to a generic pragma, so `* NEXT Foo` under
  `#+SEQ_TODO: NEXT | DONE` kept NEXT as title text. All three spellings land
  in the same cycle now.
- **Tags accept `%`.** org's own `org-tag-re` carries it; `:50%:` used to take
  the whole tags run down into title text. The parser's set is now org's plus
  the hyphen the wild corpus writes.
- **The sheet cycles a bracketless priority cell like the table does.** `S-<up>`
  and `S-<down>` over the materialize sheet read the cell with a regexp of their
  own that accepted only `[#A]`, where the table's reading takes a bare letter
  too — so an entry whose priority cell had lost its brackets started every
  cycle from `none`. One reading now (`priorityIn`), and both keys call it.
- **A CRLF config file keeps its own line endings.** `POST /config` spliced its
  `#+TODO:` block with LF whatever the file used, so one settings write left a
  CRLF `system.org` speaking two conventions with the line the reader had just
  typed as the odd one out. The block and the opening a header-only file owes are
  both the file's own ending now, read by the same `eolOf` a capture already used.
- **A tagged capture refuses a line, or a template answer, that is not one
  headline.** The untagged path has always refused an empty or multi-line
  `text`; the tagged path took both straight to its template's `%?` and its
  `%^{PROMPT}` holes, so a newline landed a column-1 star org reads as a second
  entry — and a blob holds ONE entry, the headline org-glance keys it by. Both
  the line and every `fields` answer now take that wall, and a refusal is a 400
  naming the field with nothing written.
- **Writes into a tagged capture's blob now reach the table, and the first
  `.org-glance/config` in a tree reseeds it.** Both create their own directories,
  and fsnotify arms a newly created directory without traversing into it —
  measured: one new level under a watched directory fires an event, two do not,
  and pausing between them does not help. A blob at
  `data/<shard>/<rest>/data.org` therefore sat somewhere nothing was watching for
  as long as the daemon ran, so the captured row never appeared AND every later
  edit to it was lost too: setting a state wrote `* STARTED` to the file while
  the table went on saying `TODO`. The first config layer in a tree was invisible
  the same way. The daemon knows the path at write time, so every write now
  queues the one it just wrote — into the watch's own debounce map, drained by
  the same serial loop through the same step, so a nudge plus the real event
  still costs one parse and the watch is still the only thing that updates the
  store. A path the walk would decline is dropped at that door exactly as an
  event is, so nothing can arrive by nudging that could not arrive by saving.
  KNOWN GAP, stated rather than buried: this covers what the daemon itself
  writes. A blob created into a fresh shard by ANOTHER process — org-glance's own
  Emacs side — still waits for a restart.
- **A property key may hold a digit, an underscore or a non-Latin letter, and
  the drawer survives.** `propertyKeyP` is org's own rule now — any run without
  whitespace or a colon — where it had been the narrow keyword charset, so
  `:TELE2:` and `:ZhKH:` stopped the drawer dead and everything under them
  became body text. Reported against a real tree. Deliberately WIDER than
  `keywordTextP`, whose narrowness is what makes a starred meta undeclarable:
  the two walls guard different things, and a property key is the author's word
  where a TODO keyword is a value this producer has semantics for. The reserved
  `PROPERTIES`/`END` guard is untouched, being what terminates the drawer.
- **An archived row spends its MARK along with its flag.** A mark is the
  renderer's and survives a `setRows` and a filter that hides its row — which is
  what makes it useful, and what left an archived row marked where no reader
  could see it: `markedCount()` counted it, `U` and `M` answered about it, and it
  came back marked the moment anyone looked at `tag:*archive*`. Only the rows the
  answer says LANDED are spent, and none at all where the request itself failed;
  the spending is the ARCHIVE gesture's rather than `fire`'s, since a name test
  in the shared path would be one every command added after it has to be read
  against.
- **A freshly booted page has a row under the keys.** A mount has no cursor of
  its own — the renderer selects nothing until it is asked to, `selectFirstVisible`
  having one caller and it being the filter box handing over — so `d`, `D` and
  `RET` on a just-opened page all answered `no row` until the reader pressed
  `n`. The landing table already said an APPLIED VIEW lands on row one, and a
  boot IS a view applied: `start` now lands through the same `land`, so row one
  is spelled in exactly one place rather than growing a boot rule beside the
  three. It lands on the MOUNT, which is the `?limit=100` first paint, and the
  full set arriving behind it lands nothing more — `paint` keeps the cursor the
  way the renderer keeps every selection, so it is one landing per mount. A
  caller that PASSES an `after` lands inside it and this door stands aside,
  which is what leaves a pop's remembered row untouched; a `view-changed`
  remount passes none and takes row one like any other apply. An empty answer
  still selects nothing, and the keys say so (`d → archive-flag (no row)`,
  `no row focused — n or p picks one`).
  THE HARNESS LIE THAT HID IT: `shell-harness.js` answered `getSelection` with
  row 0 of the page whatever had happened, so ~170 cases pressing a row key as
  their first act were testing a selection the browser would not have had. The
  stub now models `state.selected === null` where the renderer does —
  `keepSelection` returns at the guard, `indexOfSelected` answers -1,
  `getSelection` answers a null id, and `selectStep` from nothing lands on the
  end it steps away from — and a `total` of 0 is an EMPTY STORE, the count the
  server reports being the count of the set it answers with, which is the one
  store state no act can reach. Flipping the stub alone fails those ~170 cases;
  the landing is what makes them honest rather than lucky. Five cases open
  `TestServe`'s "Shell landing".
- **A weekday in any language keeps its headline's property drawer.** The
  timestamp parser took exactly three letters in the weekday slot, so ~/sync's
  Dutch stamps — `CLOSED: [2025-12-04 do 22:34]`, with `ma`, `zo`, `vr` and `za`
  beside `do` — failed the timestamp, failed `planningP`, and left the drawer no
  longer the next thing after the title line, taking the headline's properties
  and its `ORG_GLANCE_ID` whole. The slot now takes a run of LETTERS in any
  script, of any length, and drops it the way it always did: the weekday is
  display-only, recomputed from the date on every render, so a locale's word
  carries nothing the parser needs. Letters alone is what holds a repeater out
  of the slot — `.+3d` opens with `.` — which leaves the trailing dot French and
  Catalan abbreviate with (`lun.`) still refused, a boundary the corpus does not
  reach. This is the gap the drift instrument reported and did not fix: over
  ~/sync/views the blobs carrying no id go **49 → 21** and the records without
  blobs **57 → 29**, both by exactly the 28 it named. The corpus oracle is
  unmoved — 12596 headlines, 0 span violations over ~/sync.

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
