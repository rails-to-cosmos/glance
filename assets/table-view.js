/*
 * table-view.js — browser renderer for the table-view contract (see ../SCHEMA.md).
 *
 * Renders a View object as an interactive HTML table: badges, alignment,
 * sortable columns, a substring filter, Org-link cells, action dispatch, and
 * live streaming updates (set-rows / upsert / delete / apply-delta).
 *
 * Dependency-free, theme-aware (light/dark). No build step.
 *
 *   const view = { title, columns, actions, sort, rows };
 *   const tv = TableView.mount(document.querySelector('#app'), view, {
 *     onAction(command, id, row) { ... },   // dispatch, like the Emacs handler alist
 *     onLink(target, row)        { ... },   // follow an Org link (default: open http[s])
 *     onFilter(q)                { ... },   // producer filters; setRows the answer
 *   });
 *   tv.setView(view);     // swap the whole view — columns, title, sort and all
 *   tv.setRows(rows); tv.upsertRow(row); tv.deleteRow(id); tv.applyDelta(ops);
 *   tv.getRows();         // the store, in producer order, unfiltered
 *   tv.el;                // the mounted root element
 *   tv.select(id, col);   // select a row (and optionally one cell) -> bool
 *   tv.getSelection();    // { id, col } — col is null for a whole-row selection
 *   tv.getVisible();      // the filtered + sorted rows, in display order
 *
 *   tv.sortBy(col, asc);  // state an order, replacing the chain -> bool
 *   tv.sortPromote(col);  // `^': COL to the head of the chain, or flip it,
 *                         // written into the query as one `sort:a->b' -> bool
 *   tv.getSort(); tv.setSort(chain);     // read and replace the whole chain
 *
 *   tv.getQuery();        // the query as last delivered
 *   tv.stripLastToken();  // drop the typed text, else the last chip whole -> bool
 *   tv.pushCrumb({label, query});   // drilling in: leave a crumb behind
 *   tv.popCrumb();        // walking out: {label, query} or null — the consumer applies it
 *   tv.setCrumbs(list); tv.getCrumbs();
 *   tv.setPinned(on);     // the chip strip's pin badge; drawn only under `onPin'
 *   tv.setQuery(q);       // re-seed the chips from q, delivering nothing
 *   composer: true        // mount option: the bar and the chips ARE the widget
 *                         // — no table behind them; the query still commits to
 *                         // `onFilter' and reads back off `getQuery'
 *   tv.openFilter(); tv.closeFilter();   // summon and dismiss the filter
 *   tv.selectStep(+1);    // move a row, turning the page at either end -> bool
 *   tv.nextPage(); tv.previousPage();    // turn a page -> bool
 *   tv.pageInfo();        // { page, pages, from, to, total } — the CURSOR's page
 *
 *   tv.toggleMark(id);    // mark or unmark a row -> the state it landed in
 *   tv.markAll();         // mark the whole filtered set -> how many are marked
 *   tv.flagRow(id);       // flag or unflag a row -> the state it landed in
 *   tv.unflagRow(id);     // take the flag off, whether or not it had one
 *   tv.getFlagged();      // the flagged ids, ordered like getMarked
 *   tv.clearFlags(); tv.flaggedCount();
 *   tv.getMarked();       // the marked ids: those on show first, then the rest
 *   tv.clearMarks(); tv.markedCount();
 *
 *   TableView.parseQuery(q, keys)         // SCHEMA.md's filter micro-syntax
 *   // -> [{ negated, key, value, quoted, start, end, sep }, ...]
 *   TableView.displayText(cell)           // a cell as the table writes it
 *   TableView.comparator(column)          // the column's sort function
 *
 * The handle is this renderer's own surface, versioned with it; SCHEMA.md
 * describes what a producer sends, not what a page may call.
 *
 * Also emits DOM CustomEvents on the container: `tableview-action`
 * ({detail:{command,id,row}}) and `tableview-link` ({detail:{target,row}}).
 *
 * Rendering (renderer-local; SCHEMA.md's "Not part of the contract"):
 *
 * - Theme is a handshake, and the page leads: `data-theme="dark"' or
 *   `"light"' on `<html>' decides, and only with neither does
 *   `prefers-color-scheme' get a say. Both are watched, so a page toggling the
 *   attribute repaints without a remount.
 * - One z-band, so a consumer knows what it is layering against: the selection's
 *   marks sit at 1, the suggestion list at 5, the palette backdrop at 90 and its
 *   panel at 91. Nothing here goes higher — a consumer's own modal is meant to
 *   win over the palette, materialize sheets included.
 * - Which column is multi-valued is resolved once and read everywhere: a
 *   column declaring `multi: true' settles it, and only when none does is it
 *   guessed from cell shape. The comparator, that column's value domain, its
 *   chip rendering and the whole-entry meta all read that one verdict.
 * - The chrome — bar, title, filter chips, filter input, table skeleton, hint —
 *   is built once at mount. Updates touch only the row window, the hint line,
 *   the sort arrows and the chips, so the filter input keeps focus and caret
 *   while typing.
 * - The sort chain is drawn over the columns it orders: every key of it marks
 *   its own header with the direction and, past one key, its place in the chain
 *   (`Headline ▲¹'), the leading key in full ink and the tie-breakers muted.
 *   The hint line spells the whole chain in words. A chain is composed by
 *   promotion — `^' or a header click puts a column at the head and shifts the
 *   rest down — and promotion WRITES THE QUERY, so the order the reader built
 *   is one of the query's own tokens rather than a second store beside it.
 * - There are no toolbar buttons. Actions render on the hint line as `KEY
 *   label' pairs, the way table-view.el prints its legend: the keys are the
 *   interface, a consumer binds them and dispatches the command, and a button
 *   would only offer a second way to reach what a key already reaches.
 * - Selection is a row and, optionally, one cell of it: `select(id, col)' washes
 *   the whole column (`.tv-colsel' on every rendered td of it and on its th) and
 *   stamps `.tv-cell-sel' where that band crosses the cursor row, which is the
 *   crosshair; `getSelection()' reports both. A column index outside the table
 *   is no column at all: cell movement is the consumer's loop — read the
 *   column, add a step, hand it back — so the index one past either end is how
 *   a reader walks OFF the cells, and the answer there is the whole-row
 *   selection rather than a cursor stalled against a wall. `select(id)' with no
 *   column is that same selection, with no band anywhere. Every class is
 *   re-derived from the same state on every render, so they survive a scroll,
 *   an upsert and a `setRows' that still carries the id.
 * - The whole selection is grounds — no outline, border or shadow on any of the
 *   three. The bands sit on the cells, where the table paints them above the
 *   rows, and the body's is translucent, so the zebra, a mark, a flag and the
 *   cursor all still read through the column they cross; the header's is the
 *   same wash mixed into the page, the sticky header owing the rows opacity.
 * - Movement is smooth in two places, and neither of them is an overlay. The
 *   marks crossfade where they are (80ms on the tr and td backgrounds), and
 *   the viewport eases toward the row rather than jumping to it. An absolutely
 *   positioned highlight bar was tried and thrown away: it duplicates row
 *   geometry it cannot own, so collapsed borders, the header's real height and
 *   sub-pixel metrics drift it off the row it is meant to mark, and every fix
 *   is another measurement chasing the DOM. The row already knows where it is.
 * - When the row under the selection goes — filtered away, deleted, paged past
 *   — the selection keeps its place rather than its id: it stays at that
 *   visual index, clamped to what is left, so the next keypress carries on
 *   from where the eye is instead of starting over at the top.
 * - The ease keeps a margin under the cursor, the way `scroll-margin' and
 *   `scrolloff' do: moving down the row's foot stops at two thirds of the
 *   port, moving up its head stops at one third, and between those the
 *   viewport holds still. Clamped to the content, so at either end the cursor
 *   walks into the margin instead of the view running past the rows. A click
 *   never scrolls — the row is under the pointer already.
 * - The ease is one rAF loop that retargets. Each frame it covers 30% of the
 *   distance left and stops within half a pixel; a new selection moves the
 *   target rather than queueing a second animation, so a held key converges on
 *   the latest row instead of replaying a backlog. Any wheel, touch or drag on
 *   the scroller cancels it — whoever is scrolling outranks it — and a rows,
 *   filter or sort change cancels it too, the target having been about an
 *   order that no longer holds.
 * - That loop is the only one. The window the scroll position implies, the
 *   selection marks and the ease all run in one frame callback, because two
 *   schedulers would re-render the same tbody twice a frame and read
 *   `scrollTop' while the other was writing it.
 * - `select' updates the state and returns at once — `getSelection' is
 *   synchronous truth — but the painting it implies coalesces to one animation
 *   frame. A consumer holding a movement key fires ~30 selects a second; per
 *   event that is 30 window rewrites, and per frame it is one. The frame
 *   re-stamps the trs that are already rendered rather than rebuilding them,
 *   which is what leaves the marks something to crossfade between.
 * - `prefers-reduced-motion: reduce' turns off both the crossfade and the ease:
 *   the marks land and the viewport jumps. The coalescing stays, being economy
 *   rather than motion.
 * - Three roles, three readings, so a glance tells them apart: a state is a
 *   filled pill in its palette colour, an applied filter is a frost chip, and
 *   a tag is small muted lowercase text with no box at all. The multi-valued column's
 *   cells render a chip per value, split by the one splitter the vocabulary
 *   uses, and the dropdown wears a tag the same way wherever it names one. It
 *   is presentation only: what is searched, sorted and measured is still the
 *   text the producer sent.
 * - Under `pageSize' the rows are drawn in one of TWO presentations. PAGED is
 *   the slice: the window runs inside one page, which is what an explicit turn
 *   wants — a different set of rows, arrived at crisply. CONTINUOUS lets the
 *   window run over the whole ordered set. Stepping the selection off the end
 *   of a page switches to it at that moment: the cursor moves onto the next
 *   row and the band eases as it does anywhere else, so a held key crosses the
 *   seam with nothing to see, where a page turn would blink. The pager then
 *   reads as ORIENTATION — `pageInfo' derives the page from where the CURSOR
 *   is, so the range moves as it crosses — and any explicit turn
 *   (`nextPage', `previousPage', a pager click) snaps back to PAGED at the
 *   page it asked for, landing first or last as it always did. A new query, a
 *   sort toggle and `setRows' all return to PAGED. "On show" means the
 *   cursor's page throughout: `getVisible', `getMarked' and `getFlagged' agree
 *   with the pager whichever presentation drew the rows. Marks, flags, the
 *   selection and its column are id-keyed and carry across untouched — the
 *   presentation changes what renders, not what is true.
 * - `flagHelp: "d/D archive · u unflag"' turns the flagged-count segment into
 *   a reminder while the CURSOR sits on a flagged row. The text is the
 *   consumer's whole string, since the keys are the consumer's to bind and to
 *   name; the renderer prepends the count and marks the token before each
 *   label up as a key. Off the flagged row, or without the option, the segment
 *   is the plain count.
 * - `actionHints: false' drops the `KEY label' pairs from the hint line and
 *   leaves the counts, the sort and the pager standing. For a consumer that
 *   prints its own keymap and would otherwise print a second, disagreeing one.
 *   Presentation only: the actions still dispatch, and the default is to show
 *   them, so a consumer that says nothing sees the line it always saw.
 * - Marks and flags are ONE mechanism instantiated twice. Both are id-keyed
 *   sets of rows the table draws a ground for, and the mechanism answers every
 *   question about one of them — does a row wear it, toggle it, take it off,
 *   put it on a whole set, take it off every row, list the ids — so what is
 *   true of one is true of the other by construction: the listing order, what
 *   a clear leaves standing, what survives a re-derivation of the rows. They
 *   stay two SETS because they are two QUESTIONS. A flag is a PENDING action
 *   (a consumer's two-press `d', say) where a mark is a standing selection, so
 *   a row can carry both, `clearMarks' leaves flags alone and `clearFlags'
 *   leaves marks alone, and a consumer that wants both gone asks for both. The
 *   asymmetry lives in the HANDLE, where `markAll' is offered on marks alone
 *   and `unflagRow' on flags alone, being what only that state is used for;
 *   the mechanism holds both.
 * - `marks: true' adds dired's row marking, and a row ground with it. The
 *   chrome is a leading gutter column — presentation like the pager, so the
 *   cells and columns a producer sends are untouched and SCHEMA.md keeps
 *   calling marking renderer-local. Its header is blank, its box is org's own
 *   `[ ]'/`[X]' drawn from the row's class, and a click on it toggles that row
 *   without moving the selection. Marks are id-keyed, so they outlive
 *   `setRows', an upsert, a filter, a page flip and a sort; `deleteRow' and a
 *   delta's delete drop the mark with the row, and `setView' drops all of them
 *   with the view. One predicate gates the class, the box and the count, so
 *   without the option there is nothing to hide rather than something hidden.
 *   Why the ground is what it is: the CSS rule, `tr.tv-marked'.
 * - `flags' is the same opt-in for the flag ground, and it DEFAULTS to `marks'
 *   — flags shipped under that one option, so a consumer that never names this
 *   gets the table it already had. Named, it is its own answer: `flags: true'
 *   alone draws the flag ground and its inset edge on the row's first cell,
 *   with no gutter and no checkbox — the gutter is the checkbox's alone —
 *   and `flags: false' under `marks: true' takes the flag drawing back off.
 * - Rows are virtualized. `tbody` holds the scrolled-to window plus ~15 rows of
 *   overscan, between two spacer rows standing in for the height of the rest.
 *   Rows outside the window have no DOM: drive selection with `select(id)`
 *   rather than by reaching for row elements. Zebra striping comes from a class
 *   stamped from the row's global index (`:nth-child` cannot see past the
 *   window). Column widths come from the widest cell in the filtered set, in
 *   `ch` — the renderer's font is monospace — so they hold still while
 *   scrolling.
 * - THE `title' COLUMN FILLS AND THE REST ARE MINIMAL. Every other column is
 *   exactly as wide as its own widest cell and no wider, capped at 40
 *   characters with an ellipsis past it; the leading gutter is exactly `[X]'
 *   plus the cell padding; and the title column carries no width at all, so
 *   under the `table-layout:fixed' this turns on it takes every pixel the
 *   others leave. Nothing measures the container — the numbers are all `ch`
 *   and the browser does the remainder on a resize — and the table's
 *   `min-width' is the sized columns plus a 40-character floor for the title,
 *   which is where a window too narrow for them begins to scroll sideways. A
 *   view carrying no `title' column has nothing to fill with and keeps the auto
 *   layout it always had, widths as hints. `title' is the same convention
 *   `linked' reads.
 * - Row and header events are delegated from the scroll container, attached
 *   once. `tr.click()` still selects a rendered row.
 * - Filter input is debounced 120ms; the row window renders on a rAF. With an
 *   `onFilter' option the debounced query goes to the producer instead and the
 *   rows given are the rows shown — no local narrowing.
 * - The filter box speaks SCHEMA.md's query micro-syntax: `key:value' field
 *   predicates (only where `key' names a column, so `:work:' stays org text),
 *   `"quoted text"', `-negation', everything else free text.
 *   `TableView.parseQuery' is the tokenizer, exported so a consumer can
 *   highlight the box and a producer can implement the same grammar. Filtering
 *   locally applies the parsed query; with `onFilter' the raw text goes to the
 *   producer and the grammar is its business.
 * - COMBINATION IS ONE RULE: TOKENS AND, ALTERNATIVES OR. Every token narrows,
 *   whether or not another token names its key, so `state:TODO state:DONE' is a
 *   row in both states — which for a cell holding one value is no row — and
 *   `tag:a tag:b' is a row carrying both. A row matching EITHER is the one
 *   token `state:TODO|DONE': a predicate's VALUE splits on `|' and each
 *   alternative is read as that key's own value, the results OR'd. Empty
 *   alternatives drop (`a|' is `a'), and a value left with none narrows
 *   nothing, which is the `key:' rule. A negation covers the whole token, so
 *   `-tag:a|b' carries neither. Alternation is a PREDICATE's rule: a free-text
 *   token is the text it spells, bar and all.
 * - The keys are the view's own: its columns, and `planned' (SCHEMA's one
 *   reserved key, over the date columns together). An org TAG is not one —
 *   `tag:course' is the one spelling, and `course:text' is the two tokens
 *   `tag:course text' — so the same token means the same thing on both halves
 *   of the wire, where a vocabulary read off the loaded rows made it a
 *   predicate for whoever held the tagged row and free text for whoever did not.
 * - A suggestion list under the box completes it. A bare word offers, in order:
 *   the value or key it already SPELLS (`book' → `tag:book', `tag' → `tag:'),
 *   which needs no more typing; the TEXT ITSELF as a free-text token; the
 *   column keys it opens; the columns whose declared domain holds it as a value
 *   by prefix (`TOD' → `state:TODO'); and up to five whole TITLES it is inside,
 *   prefix hits first. Exact beats fuzzy throughout. After `key:' comes that
 *   column's value domain (`values', else the badge palette, else the distinct
 *   cell values), each with the number of rows behind it and the value typed in
 *   full at its head; `planned' has no domain to offer. A `|' RE-OPENS that
 *   domain: `state:TODO|' asks for the values again, the prefix is what follows
 *   the last bar, and the offer lands after it — so an alternation is completed
 *   one alternative at a time and stays ONE token.
 *   Arrows — and C-n/C-p, which both editors' users reach for here — move it,
 *   Esc dismisses, and a click accepts without taking focus. Tab completes and
 *   stays, at either stage. Enter is stage-aware: completing a key leaves the
 *   caret past the colon with that key's values already listed, since `tag:'
 *   is half a predicate and the values are the next thing to choose; only a
 *   finished token sends it on to commit and hand over.
 * - TWO OF THOSE OFFERS ARE FREE TEXT rather than a predicate, and each says so
 *   in a muted aside where the others print a count. The LITERAL (`text
 *   search') is what was typed: drawn quoted, which is the grammar's notation
 *   for text and the thing the row teaches, and committed BARE, which is what a
 *   reader who knew the grammar would have written — the two match identically,
 *   and quotes are written only where the text holds whitespace or a colon. A
 *   TITLE (`title') is a whole title one of the loaded rows carries, committed
 *   quoted because titles hold spaces: a reader typing a fragment of a headline
 *   is after the ROW. Both are facts, so neither is dimmed; the titles are the
 *   loaded set's, deduplicated, and wait for two characters, one letter being
 *   inside most of a store and saying nothing about any of it.
 * - ROW ONE IS ALWAYS THE CHOICE. An open list means Enter takes its first
 *   offer, so the common case costs no arrow; the ordering above is the whole
 *   of what that key means. The literal being an offer of its own is what keeps
 *   a plain search one keystroke away under that rule, and it puts what Enter
 *   will do on show rather than leaving it implied — a bare word therefore
 *   always has a list. A quoted token still asks for no suggestions at all
 *   (`"boo"' is free text already), and Esc still puts the list away before
 *   Enter commits what is written.
 * - A STARRED VALUE IS A META, and a bare word is never one. `*empty*' is the
 *   empty cell and every key answers it, `planned' included; a starred word on
 *   a multi-valued column is that WHOLE entry (`tag:*book*' is the tag `book',
 *   where `tag:boo' is a substring of the cell); anything else is a PRODUCER
 *   meta over a set only the producer can enumerate (`state:*active*'), matched
 *   literally here, which narrows. The first two need no producer, so both
 *   halves of the wire answer them alike. `sort:*none*' is the family's one
 *   member on a key that is no predicate: the EMPTY chain, which reads no cell
 *   and answers no column.
 * - A STARRED META COMPLETES STAR-FREE. The asterisks are reading notation —
 *   the mark that says this value has semantics — so completion matches through
 *   them: `act' and `active' both reach `*active*', at the value stage and as a
 *   bare word, and the starred spelling still answers to itself. Display and
 *   commit wear the stars; only the completion's matching ignores them, and
 *   what a query MEANS reads them, so `state:active' is the literal `active'.
 * - AND A DECORATED CELL READS THROUGH ITS BRACKETS. Org draws a priority
 *   `[#A]' and means `A', so completion reaches that cell's own spelling from
 *   either (`a' offers `priority:[#A]', which still commits decorated) and a
 *   whole-value predicate answers both: `priority:A' and `priority:[#A]' are
 *   one query. The stars' rule from the cell's side rather than the
 *   vocabulary's, and the matching half is the producer's too.
 * - `palette: true' makes the filter a thing you summon. The page keeps the
 *   chip row and nothing else — an unfiltered table carries no filter chrome at
 *   all — and `openFilter()' raises a centred overlay holding the control, the
 *   way a minibuffer or a Telescope prompt appears. Every ladder ends one step
 *   further out: RET commits and dissolves, Escape goes list, then text, then
 *   dissolve, and a click on the backdrop is Escape. Backspace walks the chips
 *   off and then stops: it erases, and erasing is not leaving. It also filters
 *   on commit alone — typing moves the suggestion list and nothing else, and
 *   RET or a chip strip is what reaches the rows. The bar modes keep their
 *   120ms debounce. The chips are the theme's frost, which is the
 *   association its own ivy and company faces make. It supersedes `omnibox',
 *   which stays for consumers that want the control resident.
 * - `omnibox: true' makes the filter the bar: no title, the
 *   control takes the width, and the applied chips move to a row of their own
 *   under it that collapses to nothing when empty. A consumer that does not
 *   ask for it sees exactly what it saw before.
 * - `initialQuery' is a query a consumer is putting back rather than running:
 *   it arrives as chips with the box empty and nothing delivered. Remounting
 *   is the restoration idiom — after a reconnect, a view change, a `?q=' load
 *   — and there is no other way to hand the renderer committed state.
 * - The word index is built when the rows settle rather than when someone
 *   types — 200ms of quiet, then an idle turn. An edit burst re-queues it, and
 *   a keystroke that arrives first builds it synchronously, which is the cost
 *   this exists to avoid and the worst case it cannot exceed.
 * - A committed token leaves the box and becomes a chip. The query is always
 *   the chips and the box together — chips are where the finished tokens are
 *   kept, not a second filter — so the box holds only what is still being
 *   typed. Enter commits the box whole; a settling debounce commits only the
 *   tokens something follows, so a word is never chipped out from under the
 *   caret. Backspace on an empty box takes the last chip off, a click takes any
 *   chip off, and `onFilter' is handed the whole query joined.
 * - A drill-down leaves CRUMBS, and the strip is all the renderer does about
 *   it: `pushCrumb'/`popCrumb'/`setCrumbs'/`getCrumbs' keep a trail of
 *   `{label, query}', drawn as muted chips LEFT of the live ones in the same
 *   row. `popCrumb' pops and RETURNS — it never applies — because whoever owns
 *   the fetching owns what a query means; a consumer pushes as it drills in and
 *   applies the popped query itself. Past four the oldest fold into one `… +N'
 *   counter that takes a slot of its own, so the strip's width is fixed. Handle
 *   state like a mark: it outlives `setRows' and every filter, and `setView'
 *   drops it with the world it described. A crumb carries no `data-i', which is
 *   what makes it inert to the click that takes a live chip off.
 * - `chipLabel: (token) => string|null' aliases what a LIVE chip shows. The
 *   query is unchanged — `getQuery', `onFilter' and the token a click removes
 *   are all still the text as written — so a chip may lie prettily while the
 *   grammar does not. Anything but a non-empty string leaves the token raw, and
 *   crumbs never reach it: a label is already a label.
 * - Enter with the suggestion list open accepts a suggestion and stays. With
 *   the list closed it commits whatever is typed to a chip — cancelling the
 *   pending debounce, so the query is delivered exactly once — then selects the
 *   first visible row and blurs. That last part happens every time, in both
 *   modes, and never waits on a producer's reply. A longer query is built by
 *   coming back: the box reopens empty with its chips standing, so
 *   `/ tanik RET / passport RET' is two ANDed tokens, two queries sent, and the
 *   table focused after each RET.
 * - Escape walks out one step at a time: it closes the list if one is open,
 *   else drops what is half-typed, else blurs. Both keys stop there rather than
 *   bubbling into a consumer's own keymap. Nothing else moves focus or the
 *   selection: a debounce firing on its own leaves both where the typist left
 *   them.
 *
 * Type-checked with `// @ts-check` + the JSDoc @typedefs below (no build step);
 * run `make web-check`.  The typedefs are the JS mirror of ../SCHEMA.md.
 * `make web-perf` benchmarks a 13k-row view headlessly (web/perf-driver.js).
 */
// @ts-check

/**
 * @typedef {string|number|null} Cell  A cell value; null/missing render as "".
 * @typedef {{ value: string, color: string }} Badge
 * @typedef {{ key: string,
 *             header?: string,
 *             type?: "text"|"number"|"badge",
 *             align?: "left"|"right",
 *             sortable?: boolean,
 *             badges?: Badge[],
 *             values?: string[],
 *             multi?: boolean,
 *             compare?: string }} Column
 * @typedef {{ key?: string, command: string, label?: string }} Action
 * @typedef {{ column: string, ascending?: boolean, direction?: string,
 *             nullsFirst?: boolean }} Sort
 *          `nullsFirst' is read only where `direction' is absent; it is what
 *          `getSort' answers with, so a chain survives a read and a put back.
 * @typedef {{ column: string, ascending: boolean, nullsFirst: boolean }} SortKey
 *          A normalized sort key (internal).
 * @typedef {{ id: string, cells?: Record<string, Cell>, linked?: boolean }} Row
 *          `linked' says the row leads somewhere; its `title' cell is
 *          underlined, and a view with no such column shows nothing.
 * @typedef {{ name: string, query?: string }} SavedView
 *          A view the producer has named, which `view:NAME' completes from.
 *          What applying one MEANS is the producer's: this side offers the
 *          vocabulary and narrows nothing.
 * @typedef {{ title?: string,
 *             columns: Column[],
 *             actions?: Action[],
 *             sort?: Sort | Sort[],
 *             views?: SavedView[],
 *             rows?: Row[] }} View
 * @typedef {{ op: "insert", index: number, row: Row }
 *        | { op: "delete", index: number }
 *        | { op: "reset", rows: Row[] }} Op
 * @typedef {{ label: string, query: string }} Crumb
 *          One step of a drill-down trail: what to show, and the query that
 *          gets back to it. The renderer draws the label and never reads the
 *          query — applying one is the consumer's, who owns the fetching.
 * @typedef {{ onAction?: (command: string, id: string, row: Row) => void,
 *             onLink?: (target: string, row: Row | null) => void,
 *             onFilter?: (q: string) => void,
 *             omnibox?: boolean,
 *             palette?: boolean,
 *             marks?: boolean,
 *             flags?: boolean,
 *             actionHints?: boolean,
 *             flagHelp?: string,
 *             pageSize?: number,
 *             initialQuery?: string,
 *             chipLabel?: (token: string) => string|null,
 *             composer?: boolean,
 *             onPin?: () => void,
 *             pinned?: boolean }} MountOptions
 * @typedef {{ el: HTMLElement,
 *             setView: (v: View) => void,
 *             setRows: (rows: Row[]) => void,
 *             upsertRow: (row: Row) => void,
 *             deleteRow: (id: string) => void,
 *             applyDelta: (ops: Op[]) => void,
 *             getRows: () => Row[],
 *             getVisible: () => Row[],
 *             select: (id: string, col?: number) => boolean,
 *             getSelection: () => { id: string|null, col: number|null },
 *             getQuery: () => string,
 *             setCrumbs: (list: Crumb[]) => void,
 *             getCrumbs: () => Crumb[],
 *             setPinned: (on: boolean) => void,
 *             setQuery: (q: string) => void,
 *             pushCrumb: (c: Crumb) => number,
 *             popCrumb: () => Crumb|null,
 *             stripLastToken: () => boolean,
 *             openFilter: () => void,
 *             closeFilter: () => void,
 *             selectStep: (step: number) => boolean,
 *             nextPage: () => boolean,
 *             previousPage: () => boolean,
 *             pageInfo: () => { page: number, pages: number,
 *                               from: number, to: number, total: number },
 *             sortBy: (column: string, ascending: boolean) => void,
 *             sortPromote: (column: string) => boolean,
 *             getSort: () => SortKey[],
 *             setSort: (sort?: Sort|Sort[]|SortKey[]|null) => void,
 *             toggleMark: (id: string) => boolean,
 *             markAll: () => number,
 *             flagRow: (id: string) => boolean,
 *             unflagRow: (id: string) => void,
 *             getFlagged: () => string[],
 *             clearFlags: () => void,
 *             flaggedCount: () => number,
 *             getMarked: () => string[],
 *             clearMarks: () => void,
 *             markedCount: () => number }} Handle
 *   What `mount' returns.
 * @typedef {{ ids: Set<string>,
 *             shows: (id: string) => boolean,
 *             toggle: (id: string) => boolean,
 *             drop: (id: string) => void,
 *             addAll: (rows: Row[]) => number,
 *             clear: () => void,
 *             list: () => string[] }} RowState
 *   One id-keyed row state — a mark or a flag — and everything done to it.
 * @typedef {{ search: string, len: number[], cells: string[] }} RowText
 *   A row's cached display data: every cell's text lowercased and joined with
 *   \x1f (free-text filtering searches it), each cell's length (column widths),
 *   and the same per-cell strings (field predicates test one column).
 * @typedef {{ negated: boolean,
 *             key: string|null,
 *             value: string,
 *             quoted: boolean,
 *             start: number,
 *             end: number,
 *             sep: number }} Token  One filter-query token; see `parseQuery'.
 */

/** @param {*} root  The global object (`window`, or CommonJS `this`). */
(function (root) {
  "use strict";

  // ---- helpers -------------------------------------------------------------

  const esc = (s) =>
    String(s).replace(/[&<>"']/g, (c) =>
      ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));

  // Org bracket link: [[TARGET][DESC]] or [[TARGET]].
  const ORG_LINK = /\[\[([^\]]+?)\](?:\[([^\]]*?)\])?\]/g;

  // A cell's plain display string (links -> DESC), one line. Used for width,
  // filter and sort so a link column lines up and searches by its description.
  /** @param {Cell|undefined} val  @returns {string} */
  function displayText(val) {
    if (val === null || val === undefined) return "";
    let s = typeof val === "string" ? val : String(val);
    // The scan is the hot path (every cell, every re-measure); skip the link
    // rewrite for the strings that cannot contain one.
    if (s.indexOf("[[") !== -1)
      s = s.replace(ORG_LINK, (_, target, desc) => desc || target);
    return s.replace(/[\u0000-\u001f\u007f]+/g, " ");
  }

  // ---- badge legibility ----------------------------------------------------
  // A producer's badge colour is the badge's identity, and it was picked for
  // one background — usually a dark one. The renderer owns whether it can be
  // read on the ground it is actually drawn on, so the hue is kept and only its
  // lightness moves, until the label clears WCAG AA against its own pill.

  /**
   * Does this browser take C-n and C-p for itself before the page sees them?
   * Chrome and its family bind them to new window and print at the browser
   * level, so the renderer's handler never runs and two of the four keys the
   * list documents are dead through no fault of the page. Firefox delivers
   * both, and so do the webview shells an app embeds — Electron says so in the
   * agent string. Read per list render rather than once at load: two short
   * regexes against a string that is already in memory cost nothing beside the
   * suggestions they annotate, and a value baked in at import time is one no
   * test can reach.
   */
  function swallowsCtrlN() {
    const ua = typeof navigator === "object" && navigator ? navigator.userAgent || "" : "";
    return /Chrom(e|ium)\//.test(ua) && !/Firefox|Electron\//.test(ua);
  }

  /**
   * The values CELL spells, org-style: `:a:b:' is a and b. The one splitter —
   * the vocabulary is built with it and the cells are rendered with it, so a
   * chip on screen and a key in a query can never disagree about where a value
   * begins.
   * @param {string} cell
   */
  function tagsIn(cell) { return cell.split(":").filter(Boolean); }

  /** A delimited value list, org-style: `:a:b:'. What makes a column multi-valued. */
  const ORG_TAGS = /^:[^:]+(:[^:]+)*:$/;

  const HEX = /^#?([0-9a-f]{3}|[0-9a-f]{6})$/i;
  /** @param {string} hex  @returns {number[]|null} */
  function rgbOf(hex) {
    const m = HEX.exec(String(hex).trim());
    if (!m) return null;
    const h = m[1].length === 3 ? m[1].replace(/./g, (c) => c + c) : m[1];
    return [0, 2, 4].map((i) => parseInt(h.slice(i, i + 2), 16));
  }
  const channel = (c) => (c / 255 <= 0.03928 ? c / 255 / 12.92
                                             : Math.pow((c / 255 + 0.055) / 1.055, 2.4));
  /** WCAG relative luminance. @param {number[]} c */
  const luma = (c) => 0.2126 * channel(c[0]) + 0.7152 * channel(c[1]) + 0.0722 * channel(c[2]);
  /** WCAG contrast ratio. @param {number[]} a  @param {number[]} b */
  function contrast(a, b) {
    const x = luma(a) + 0.05, y = luma(b) + 0.05;
    return x > y ? x / y : y / x;
  }
  /** @param {number[]} a  @param {number[]} b  @param {number} t */
  const blend = (a, b, t) => a.map((v, i) => Math.round(v + (b[i] - v) * t));
  const hexOf = (c) => "#" + c.map((v) => v.toString(16).padStart(2, "0")).join("");

  /** @type {Map<string, string>} */
  const inkCache = new Map();

  /**
   * COLOR made legible on the pill it tints, under a dark or light ground.
   * Stepped toward black on light and toward white on dark — a scale of the
   * same hue, so the badge stays recognisably itself — until the label clears
   * 4.5:1 against the 15% wash it sits on. Cached per colour and theme.
   * @param {string} color  @param {boolean} dark  @returns {string}
   */
  function inkFor(color, dark) {
    const key = color + (dark ? "|d" : "|l");
    const had = inkCache.get(key);
    if (had !== undefined) return had;
    const hue = rgbOf(color), ground = rgbOf(dark ? "#000000" : "#FFFFFF");
    let ink = color;
    if (hue && ground) {
      const pill = blend(ground, hue, 0.15);          // what the wash comes out as
      const toward = dark ? [255, 255, 255] : [0, 0, 0];
      let out = hue;
      for (let t = 0; t < 0.95 && contrast(out, pill) < 4.5; ) {
        t += 0.05;
        out = blend(hue, toward, t);
      }
      ink = hexOf(out);
    }
    inkCache.set(key, ink);
    return ink;
  }

  // Cell inner HTML: badge colouring + Org links + escaping.
  /** @param {Column} col  @param {Cell|undefined} val  @param {boolean} [dark]
   *  @param {boolean} [asTags]  @returns {string} */
  function cellHTML(col, val, dark, asTags) {
    // A multi-valued cell is a list of values, and reads as one: a ghost chip
    // each, outlined rather than filled, so it is neither a state badge (a
    // filled pill) nor an applied filter (a frost chip). Three roles, three
    // shapes. Only the presentation changes — what is searched, sorted and
    // measured is still the text the producer sent.
    if (asTags) {
      const raw = displayText(val);
      const tags = tagsIn(raw);
      if (!tags.length) return esc(raw);
      return `<span class="tv-tags">`
           + tags.map((t) => `<span class="tv-tag">${esc(t)}</span>`).join(" · ")
           + `</span>`;
    }
    if (col.type === "badge") {
      const raw = displayText(val);
      const badge = (col.badges || []).find((b) => b.value === raw);
      const color = badge && badge.color;
      // A pill: the palette colour tints the ground and writes the label, so
      // one hue carries it in either scheme. A value the palette does not name
      // stays plain text.
      if (color)
        return `<span class="tv-pill" style="--tv-badge:${esc(color)};`
             + `--tv-ink:${esc(inkFor(color, !!dark))}">${esc(raw)}</span>`;
      return esc(raw);
    }
    const s = typeof val === "string" ? val : displayText(val);
    // Interleave escaped text with anchors for any Org links.
    let out = "", last = 0, m;
    ORG_LINK.lastIndex = 0;
    while ((m = ORG_LINK.exec(s))) {
      out += esc(s.slice(last, m.index).replace(/[\u0000-\u001f\u007f]+/g, " "));
      const target = m[1], desc = m[2] || m[1];
      out += `<a class="tv-link" href="#" data-target="${esc(target)}">${esc(desc)}</a>`;
      last = m.index + m[0].length;
    }
    out += esc(s.slice(last).replace(/[\u0000-\u001f\u007f]+/g, " "));
    return out;
  }

  // ---- filter query --------------------------------------------------------
  // SCHEMA.md's filter micro-syntax, so a producer filtering server-side and a
  // renderer filtering locally answer the same box the same way. Exported as
  // `TableView.parseQuery' — a consumer highlighting the box, and a producer
  // implementing the grammar at the other end, read it from here.

  /** Is C a token separator? `&' is an alias for whitespace. */
  const isSep = (c) => c === "&" || c === " " || c === "\t" || c === "\n";

  /** The first `:' or `=' in S, or -1. @param {string} s */
  function splitAt(s) {
    const a = s.indexOf(":"), b = s.indexOf("=");
    return a === -1 ? b : (b === -1 ? a : Math.min(a, b));
  }

  /** The alternation bar: what a predicate's VALUE splits on. */
  const ALT = "|";

  /**
   * VALUE's alternatives — `A|B' is either, each read as that key's own value.
   * An EMPTY alternative is dropped, so `a|' is `a' and `a||b' is `a|b';
   * a value that is bars alone is left with none, and a predicate with no
   * alternative has nothing to narrow by, which is the `key:' rule. One answer
   * for the whole half-typed family: `key:', `key:|', `key:||'.
   *
   * The split runs over the value the scanner produced, whose quotes are
   * already gone, so a bar inside a predicate is always the operator. A literal
   * one is free text's — `"a|b"' and the bare `a|b' are the text they spell.
   * @param {string} value  @returns {string[]}
   */
  const alternatives = (value) => value.split(ALT).filter((v) => v !== "");

  /**
   * Split Q into raw tokens: quotes removed, a leading `-' taken off, and the
   * offsets of the whole token kept so a caret can be placed inside one.
   * Separators inside quotes are ordinary characters.
   * @param {string} q
   */
  function scanQuery(q) {
    const out = [];
    let start = 0, body = "", neg = false, quoted = false;
    let seen = false, hasBody = false, inQ = false, sep = -1;
    const flush = (end) => {
      if (seen) out.push({ start, end, body, negated: neg, quoted, sep });
      body = ""; neg = false; quoted = false; seen = false; hasBody = false; sep = -1;
    };
    for (let i = 0; i < q.length; i++) {
      const c = q[i];
      if (c === '"') {
        if (!seen) start = i;
        if (!hasBody) quoted = true;      // a token that opens with a quote is free text
        seen = hasBody = true;
        inQ = !inQ;
      } else if (!inQ && isSep(c)) {
        flush(i);
      } else if (!seen && c === "-") {
        start = i; seen = true; neg = true;
      } else {
        if (!seen) start = i;
        if (!inQ && !quoted && sep === -1 && (c === ":" || c === "=")) sep = i;
        body += c;
        seen = hasBody = true;
      }
    }
    flush(q.length);
    return out;
  }

  /**
   * Q as tokens, against the column KEYS of the view it filters. `key:value'
   * is a field predicate only when KEY names a column, so org cell text like
   * `:work:' or `=code=' stays free text; a quoted token is always free text;
   * a leading `-' negates either form; tokens AND together.
   * @param {string} q  @param {string[]} keys  @returns {Token[]}
   */
  function parseQuery(q, keys) {
    const known = new Set(keys || []);
    return scanQuery(q).map((t) => {
      const at = t.quoted ? -1 : splitAt(t.body);
      const key = at > 0 ? t.body.slice(0, at) : null;
      const pred = key !== null && known.has(key);
      return {
        negated: t.negated,
        key: pred ? key : null,
        value: pred ? t.body.slice(at + 1) : t.body,
        quoted: t.quoted,
        start: t.start,
        end: t.end,
        sep: pred ? t.sep : -1,
      };
    });
  }

  // ---- sorting -------------------------------------------------------------

  /** @param {Cell|undefined} v  @returns {number|null} */
  const asNumber = (v) => {
    const n = typeof v === "number" ? v : parseFloat(displayText(v));
    return Number.isNaN(n) ? null : n;
  };

  // Ordered domain of a column: explicit `values`, else badge palette order.
  //
  // METAS ARE NOT POSITIONS. A meta is filter vocabulary — no cell holds one —
  // so a column that declares `*active*' among its `values' would otherwise
  // sort every real value into the one bucket "unlisted", which is no order at
  // all. They come out here, and a `values' that was metas alone falls through
  // to the palette the way a column declaring none does.
  /** @param {Column} col  @returns {string[]|null} */
  function valueOrder(col) {
    const declared = col.values ? col.values.map(String).filter((v) => !META.test(v)) : null;
    if (declared && declared.length) return declared;
    if (col.type === "badge") return (col.badges || []).map((b) => String(b.value));
    return null;
  }

  /** A meta value, which SCHEMA spells `*empty*' / `*active*'. */
  const META = /^\*.+\*$/;

  /**
   * A meta without its stars — its WORD, which is what a rule reading one needs
   * (the whole-entry match on a multi-valued column) and what completion
   * matches through, so `act' reaches `*active*'. What is drawn and what is
   * inserted keep the stars, a query MEANS them, and the starred spelling still
   * answers to itself.
   */
  const starless = (v) => (META.test(v) ? v.slice(1, -1) : v);

  /** Org's priority decoration, which a cell WEARS rather than means: `[#A]'. */
  const DECORATED = /^\[#(.*)\]$/;

  /**
   * V with that decoration off. DISPLAY WEARS THE DECORATION, MATCHING READS
   * THROUGH IT — the stars' rule from the other side: `[#A]' is what the table
   * shows and `A' is what a reader means by it, so a whole-value predicate
   * answers both spellings (`cellTest') and completion reaches the cell's from
   * either (`opensWith').
   */
  const undecorated = (v) => {
    const m = DECORATED.exec(v);
    return m ? m[1] : v;
  };

  /** V with its reading notation off, whichever of the two it wears. */
  const meant = (v) => undecorated(starless(v));
  /** Does the lowercased value LOWER open with P, notation either way? */
  const opensWith = (lower, p) => lower.startsWith(p) || meant(lower).startsWith(p);
  /** Is LOWER what P spells, notation either way? */
  const spells = (lower, p) => lower === p || meant(lower) === p;

  /**
   * The one PRODUCER meta this renderer can partly answer: SCHEMA puts the EMPTY
   * cell in the active group, and an empty cell needs no keyword set to
   * recognise.
   */
  const ACTIVE_META = "*active*";

  /**
   * The meta every key answers: SCHEMA's empty cell, on any column and on
   * `planned'. A cell is empty or it is not, so no producer set, no vocabulary
   * and no clock are needed and the two halves of the wire cannot disagree
   * about a row. It replaced the bare word `none', which reserved a spelling a
   * cell could hold: a cell reading `none' is ordinary text again, and
   * `key:none' finds it.
   */
  const EMPTY_META = "*empty*";

  /**
   * SCHEMA's virtual key over a view's DATE columns together: a row is planned
   * when any of them holds anything. Answered here in full, unlike the metas
   * above — the cells are all it takes, so no producer set, no vocabulary and
   * no clock are needed and the two sides cannot disagree about a row.
   */
  const PLANNED_KEY = "planned";

  /**
   * FREE TEXT'S OWN KEY: `substring:V' is exactly what `V' alone means — a
   * substring of the row as it displays. So the grammar is `KEY:VALUE'
   * throughout and a bare word is that spelling with the key elided.
   *
   * One matcher for both, so the two can never come to mean two things. What
   * the key buys is a value that may spell a separator's neighbour — a leading
   * `-', a colon, a bar — under quotes without being read as something else,
   * and a token a reader can see is a search.
   */
  const SUBSTRING_KEY = "substring";

  /**
   * SCHEMA's ORDER key: `sort:COL', `sort:COL:desc'. It states the order the
   * rows are read in and narrows nothing, so it is the one key in the grammar
   * that is no predicate at all — written order is precedence, and a query
   * naming any replaces the view's declared `sort'.
   */
  const SORT_KEY = "sort";

  /**
   * The key that states the COLUMN SET: `columns:State,Title,Tags'. The sort
   * key's twin — no predicate, narrows nothing in either polarity — and
   * producer-shaped: which columns a name resolves to (and what a name the
   * view does not carry reads out of the row's own subtree) is the server's
   * answer, arriving as the view's `columns'. This side only keeps the token
   * out of free text and dresses its chip.
   */
  const COLUMNS_KEY = "columns";

  /**
   * The VIEW TOKENS: the keys that state a fact about the view and narrow
   * nothing. One list, so the vocabulary (`queryKeys') and the matcher's skip
   * (`queryMatcher') cannot come to disagree — a key missed by either would
   * silently demote the token to free text or silently narrow. A new view
   * token is one entry here beside its chip class.
   */
  /**
   * `view:NAME' — the SAVED VIEW a producer has named. Like its two siblings it
   * states a fact about the view and narrows nothing here: what a name MEANS is
   * the producer's, so a page holding one un-expanded shows every row rather
   * than guessing. The names are the view's own (`views'), so a producer that
   * grows one is offered with nothing here to edit.
   */
  const VIEW_KEY = "view";

  const VIEW_KEYS = [SORT_KEY, COLUMNS_KEY, VIEW_KEY];

  /** The directions a sort token may spell; the empty one ascends. */
  const SORT_DIRS = { "": true, asc: true, desc: false };

  /**
   * The separator that CHAINS one sort token's columns:
   * `sort:title->priority:desc' is `sort:title sort:priority:desc' written once.
   * Sugar, and ONE semantics — a token's segments are read as exactly the tokens
   * they compose, so nothing downstream can tell the two spellings apart and
   * every rule the grammar has about repeats reaches across the arrow unchanged.
   */
  const SORT_ARROW = "->";

  /**
   * The meta that spells the EMPTY CHAIN. `sort:*none*' NAMES a sort key, so it
   * replaces the view's declared `sort' the way any other sort token does — with
   * nothing, leaving the rows in the order they arrived. It is what a reader has
   * instead of a token to take off, the declared order being invisible until
   * they diverge from it.
   *
   * It admits no companions: `sort:*none* sort:title' is a query a producer
   * refuses, and a renderer, having nobody to refuse to, drops the `*none*' and
   * lets the companions stand. The producer is the stricter of the two, which is
   * every other sort refusal's asymmetry, and it costs no rows either way — a
   * sort token narrows nothing in any polarity.
   */
  const NONE_META = "*none*";

  /**
   * SEGMENT as a sort key, or null where nothing orderable is spelled. KNOWN is
   * the columns a key may name.
   *
   * A segment names ONE column in ONE direction: an alternation, a column the
   * view does not carry and a direction that is neither `asc' nor `desc' each
   * yield null, and a negation is the whole token's (`sortSegments'). A producer
   * refuses those (SCHEMA: the query is an error and is answered as one); a
   * renderer, which has nobody to refuse to, drops the key and leaves the token
   * narrowing nothing like every other sort token.
   * @param {string} seg  @param {(k: string) => boolean} known
   * @returns {SortKey|null}
   */
  function sortKeyOf(seg, known) {
    if (seg.indexOf(ALT) !== -1) return null;
    const at = seg.indexOf(":");
    const column = at === -1 ? seg : seg.slice(0, at);
    const dir = at === -1 ? "" : seg.slice(at + 1).toLowerCase();
    if (!column || !known(column) || !(dir in SORT_DIRS)) return null;
    return { column, ascending: SORT_DIRS[dir], nullsFirst: false };
  }

  /**
   * The segments TOK chains, in written order, each read as a sort token's whole
   * value is. A NEGATED token chains none: the `-' covers everything after it,
   * so a refusal reaches every segment rather than the first.
   * @param {Token} tok  @returns {string[]}
   */
  function sortSegments(tok) {
    return tok.negated ? [] : tok.value.split(SORT_ARROW);
  }

  /**
   * The chain Q names, highest priority first: [] where it names the EMPTY one
   * and null where it names no chain at all. That difference is the whole
   * question of whether a declared `sort' still stands — a reader asking for no
   * order and a reader saying nothing about order are different readers.
   * KNOWN is the columns a key may name.
   *
   * Written order is precedence and repeats compose, so `sort:deadline
   * sort:title' opens on deadline with title behind it — and `->' spells that
   * same chain in one token, the segments read exactly where the tokens were. A
   * column named twice keeps its FIRST spelling and the later one is dropped —
   * the chain's own rule (a chain never names a column twice) read over the
   * segments that spell it, wherever the token boundaries fall — so what this
   * answers can always be handed to `applyChain'.
   *
   * `*none*' is the empty chain and takes no companions: a key that resolves
   * outranks it, so `sort:*none* sort:title' and `sort:*none*->title' are both
   * title. Every other refusal — a negation, an alternation, an unknown column,
   * a direction that is neither word — drops its own key and says nothing about
   * the chain, which is why a query holding those alone leaves the declared
   * order standing.
   * @param {string} q  @param {string[]} keys  @param {(k: string) => boolean} known
   * @returns {SortKey[]|null}
   */
  function sortsIn(q, keys, known) {
    /** @type {SortKey[]} */
    const chain = [];
    let none = false;
    for (const tok of parseQuery(q, keys)) {
      if (tok.key !== SORT_KEY) continue;
      for (const seg of sortSegments(tok)) {
        if (seg.toLowerCase() === NONE_META) { none = true; continue; }
        const k = sortKeyOf(seg, known);
        if (k && !chain.some((c) => c.column === k.column)) chain.push(k);
      }
    }
    if (chain.length) return chain;   // a key that resolves outranks `*none*'
    return none ? chain : null;       // the empty chain, or nothing said at all
  }

  /** KEY as the segment that spells it. @param {SortKey} key  @returns {string} */
  function sortSegment(key) {
    return `${key.column}${key.ascending ? "" : ":desc"}`;
  }

  /**
   * CHAIN as the ONE token that spells it — the CANONICAL form, and what every
   * door that writes an order into a query emits. `:asc' is not written, an
   * unspelled direction already meaning it.
   * @param {SortKey[]} chain  @returns {string}
   */
  function sortToken(chain) {
    return SORT_KEY + ":" + chain.map(sortSegment).join(SORT_ARROW);
  }

  /** The superscript digits, so a precedence mark is one character per digit. */
  const SUPERS = "⁰¹²³⁴⁵⁶⁷⁸⁹";
  /** N in superscript. @param {number} n  @returns {string} */
  const superscript = (n) =>
    String(n).split("").map((d) => SUPERS[Number(d)] || d).join("");

  /**
   * The values a column offers for completion: its declared `values' in their
   * own order, then any badge value they did not already name.  Merged rather
   * than shadowed — a producer adding meta-values to a badge column would
   * otherwise delete that column's concrete keywords from the list, which is
   * the opposite of what declaring them was for.
   * @param {Column} col  @returns {string[]|null}
   */
  function domainValues(col) {
    const declared = col.values ? col.values.map(String) : null;
    const badges = col.type === "badge"
      ? (col.badges || []).map((b) => String(b.value)) : null;
    if (!declared) return badges;
    if (!badges) return declared;
    const named = new Set(declared.map((v) => v.toLowerCase()));
    return declared.concat(badges.filter((v) => !named.has(v.toLowerCase())));
  }

  /**
   * The metas COL declares: producer vocabulary, which no cell of it holds. A
   * column whose values are derived rather than declared — a multi-valued one,
   * whose domain is the vocabulary its cells spell — takes them from here, so a
   * declared meta is offered whether or not the column's domain came from the
   * rows.
   * @param {Column} col  @returns {string[]}
   */
  function declaredMetas(col) {
    return (col.values || []).map(String).filter((v) => META.test(v));
  }

  // Less-than over raw cell values for a column (mirrors table-view.el).
  /**
   * @param {Column} col
   * @returns {(a: Cell|undefined, b: Cell|undefined) => number}
   */
  function comparator(col) {
    const compare = col.compare;
    if (compare === "number" || compare === "numeric")
      return (a, b) => (asNumber(a) ?? Infinity) - (asNumber(b) ?? Infinity);
    if (compare === "natural" || compare === "version")
      return (a, b) =>
        displayText(a).localeCompare(displayText(b), undefined, { numeric: true });
    if (compare === "string" || compare === "text")
      return (a, b) => displayText(a).localeCompare(displayText(b));
    const order = valueOrder(col);
    if (order) {
      const pos = (v) => {
        const i = order.indexOf(displayText(v));
        return i === -1 ? order.length : i;
      };
      return (a, b) => pos(a) - pos(b);
    }
    if (col.type === "number")
      return (a, b) => (asNumber(a) ?? Infinity) - (asNumber(b) ?? Infinity);
    return (a, b) => displayText(a).localeCompare(displayText(b));
  }

  // ---- component -----------------------------------------------------------

  const OVERSCAN = 15;         // rows rendered above and below the viewport
  const SAMPLE = 40;           // non-empty cells a column's shape is read off
  const SHAPED = 2;            // of them that have to carry the shape
  const ROW_H = 30;            // row height until a rendered row can be measured
  const CELL_PAD = 24;         // a cell's horizontal padding, both sides
  const PILL_CH = 2;           // a badge pill's ground, in characters
  const BOX_CH = 3;            // the gutter's glyph, `[X]', in characters
  // The two numbers the fill policy rests on, measured against a 12,674-headline
  // Org corpus. COL_MAX is the ceiling a sized column may not pass: the widest
  // non-title cell in that corpus is exactly 40 characters (one compact
  // timestamp range; the tag runs top out at 33), so the cap costs the corpus
  // nothing and bounds the pathological cell that would otherwise eat the
  // title's share. TITLE_MIN is the fill column's floor, taken by the table's
  // `min-width' rather than by the column, so a window too narrow for it
  // SCROLLS instead of crushing the title to nothing; 40 characters shows 83%
  // of that corpus's titles whole.
  const COL_MAX_CH = 40;       // ceiling on a sized column, in characters
  const TITLE_MIN_CH = 40;     // the fill column's floor, in characters
  const DEBOUNCE = 120;        // ms of quiet before a filter keystroke re-renders
  const SETTLE = 200;          // ms of quiet before the rows are taken to have settled
  const LONG_PRESS = 500;      // ms of a still finger before it means the row action
  const PRESS_SLOP = 10;       // px of drift that makes it a scroll instead
  const EASE = 0.3;            // fraction of the remaining scroll covered per frame
  const SNAP_PX = 0.5;         // closer than this and the ease is over
  const CRUMB_MAX = 4;         // crumb chips drawn before the oldest collapse

  /** Run CB when nothing else is pending (or soon, where there is no idle). */
  const idle = (cb) =>
    typeof requestIdleCallback === "function" ? requestIdleCallback(cb) : setTimeout(cb, 0);

  /** Run CB on the next frame (or soon, where there are no frames). */
  const frame = (cb) =>
    typeof requestAnimationFrame === "function" ? requestAnimationFrame(cb)
                                                : setTimeout(cb, 16);

  let styleInjected = false;
  function injectStyle() {
    if (styleInjected) return;
    styleInjected = true;
    // The applied-filter identity, spelled once. Swapping it is one edit here;
    // the palettes below carry only how much of it each theme wants.
    const FROST = "#D0E1F9";
    // The pending-action identity, spelled once, the way FROST is. Red is the
    // one signal that reads as "about to happen to this row" without being
    // borrowed from the applied filter or the cursor.
    const FLAG = "#E74C3C";
    // The selected column's identity, spelled once like the two above. Amber is
    // the one hue nothing else on the table uses (frost 215, flag 6, the light
    // cursor 120, the mark's ink 185/215), and it is pale on purpose: at
    // luminance .899 it sits level with the darkest ground a row can wear, so
    // washing it over one shifts the hue without spending the contrast the tag
    // ink needs. That is what lets a column be washed at all — every other
    // candidate darkened a marked or flagged row past 4.5:1 before it became
    // visible, the light mark and flag washes already sitting at the ink's cap.
    const COL = "#FFF3D0";
    // The link identity, spelled here like the three above and, unlike them, in
    // two weights: a wash can be one colour at two strengths, where INK cannot,
    // the two themes having nothing in common to be read on. Both are the
    // accent's own blue (hue 202, saturation held) moved in LIGHTNESS ALONE
    // until it clears 4.5:1 on every ground a cell can wear rather than on the
    // page alone — the four row washes, the column band over each of them, and
    // the crosshair. Light is the accent one point darker, floor 4.69 on a
    // flagged row; dark is ten points lighter, floor 4.63 on the crosshair,
    // where the accent itself was at 3.70. Dark lands within a hundredth of
    // --tv-muted on every ground, that ink being what the dark washes were cut
    // to, so a link is exactly as legible as the table already guarantees. The
    // accent stays where it is and keeps the chrome it inks — a hover, a
    // sortable header, the pager — all of which sit on the page's own ground.
    const LINK_LIGHT = "#30739B";
    const LINK_DARK = "#7CC9F8";
    const css = `
/* Both palettes are the author's Emacs theme, mapped role for role from its
   default faces for dark, its light-* block for light. Three values are
   lightness-only adjustments where the theme's own colour missed a contrast
   floor in this context, the hue held: light muted #7F8C8D -> #667071 (3.5:1
   -> 5.1:1 on white) and light accent #4CB5F5 -> #31769F (2.3:1 -> 5.0:1 on
   white). --tv-link is that same operation on the accent itself, in both
   themes and measured against the grounds a ROW can wear rather than against
   the page (LINK_LIGHT and LINK_DARK above).
   The selected row takes the theme's own highlight (light-golden #FFD600),
   which a host may override like any other token -- the cursor row is one
   role and a consumer draws it in one hue wherever it appears.

   Borders are the exception and stay hairlines: they carry no information, so
   contrast is not a goal for them and a visible rule only adds noise. Light
   keeps the quiet #E3E6EA (1.25:1 on white) rather than the theme's #BDC3C7,
   and dark takes #2a2d3d over the theme's #223959 (1.54:1 against true black
   against 1.80:1) — the quieter of the two. Every rule is 1px. */
/* THE PALETTE IS A DEFAULT, at zero specificity (:where), so a host that
   themes this widget wins with an ordinary rule whatever order the two
   stylesheets land in -- this one is injected into <head> at mount time, which
   is after a served page's own. The custom properties ARE the theming API;
   these are the values a consumer who declares none gets. Everything below
   this block is layout and keeps its specificity. */
:where(.tv-root){
  --tv-fg:#000000;
  --tv-muted:#667071;
  --tv-bg:#FFFFFF;
  --tv-alt:#F8F8FF;
  --tv-border:#E3E6EA;
  --tv-accent:#31769F;
  --tv-sel:#FFD600;
  --tv-hover:#FAFAFA;
  --tv-link:${LINK_LIGHT};
  --tv-frost:${FROST};
  --tv-chip-wash:45%;
  --tv-chip-edge:95%;
  --tv-mark-wash:8%;
  --tv-flag:${FLAG};
  --tv-flag-wash:8%;
  --tv-col:${COL};
  --tv-veil:#00000066;
  --tv-shadow:#00000033;
  --tv-col-wash:35%;
  --tv-cell-wash:60%;
  --tv-sort-wash:52%;
  --tv-cols-wash:52%;
}
.tv-root{
  color:var(--tv-fg);
  background:var(--tv-bg);
  font:13px/1.5 ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
  border:1px solid var(--tv-border);
  border-radius:8px;
  overflow:hidden;
  display:flex;
  flex-direction:column;
  max-height:100%;
}
@media (prefers-color-scheme:dark){
  :where(.tv-root){
    --tv-fg:#FFFFFF;
    --tv-muted:#A4C2EB;
    --tv-bg:#000000;
    --tv-alt:#21252B;
    --tv-border:#2a2d3d;
    --tv-accent:#4CB5F5;
    --tv-sel:#373D4F;
    --tv-link:${LINK_DARK};
    --tv-hover:#1F1F1F;
    --tv-veil:#00000099;
    --tv-shadow:#00000077;
    --tv-chip-wash:18%;
    --tv-chip-edge:34%;
    --tv-mark-wash:30%;
    --tv-flag-wash:30%;
    --tv-col-wash:8%;
    --tv-cell-wash:9%;
    --tv-sort-wash:18%;
    --tv-cols-wash:18%;
  }
}
:where(:root[data-theme="dark"] .tv-root){
  --tv-fg:#FFFFFF;
  --tv-muted:#A4C2EB;
  --tv-bg:#000000;
  --tv-alt:#21252B;
  --tv-border:#2a2d3d;
  --tv-accent:#4CB5F5;
  --tv-sel:#373D4F;
  --tv-link:${LINK_DARK};
  --tv-hover:#1F1F1F;
  --tv-veil:#00000099;
  --tv-shadow:#00000077;
  --tv-chip-wash:18%;
  --tv-chip-edge:34%;
  --tv-mark-wash:30%;
  --tv-flag-wash:30%;
  --tv-col-wash:8%;
  --tv-cell-wash:9%;
  --tv-sort-wash:18%;
  --tv-cols-wash:18%;
}
:where(:root[data-theme="light"] .tv-root){
  --tv-fg:#000000;
  --tv-muted:#667071;
  --tv-bg:#FFFFFF;
  --tv-alt:#F8F8FF;
  --tv-border:#E3E6EA;
  --tv-accent:#31769F;
  --tv-sel:#FFD600;
  --tv-hover:#FAFAFA;
  --tv-link:${LINK_LIGHT};
  --tv-veil:#00000066;
  --tv-shadow:#00000033;
  --tv-chip-wash:45%;
  --tv-chip-edge:95%;
  --tv-mark-wash:8%;
  --tv-flag-wash:8%;
  --tv-col-wash:35%;
  --tv-cell-wash:60%;
  --tv-sort-wash:52%;
  --tv-cols-wash:52%;
}
.tv-bar{
  display:flex;
  align-items:center;
  gap:10px;
  padding:8px 12px;
  border-bottom:1px solid var(--tv-border);
  flex-wrap:wrap;
}
.tv-title{
  font-weight:600;
  font-size:14px;
  margin-right:auto;
}
.tv-filter{
  font:inherit;
  padding:4px 8px;
  border:1px solid var(--tv-border);
  border-radius:6px;
  background:var(--tv-bg);
  color:var(--tv-fg);
  min-width:140px;
}
/* Quiet enough to be read past, not so quiet it cannot be read. Firefox dims
   placeholders on top of the colour, which is what the opacity is undoing. */
.tv-filter::placeholder{
  color:var(--tv-muted);
  opacity:1;
}
.tv-filter-wrap{
  position:relative;
  display:flex;
}
/* Omnibox: the filter is the bar's one control, and it takes the width the
   title was holding. The dropdown hangs under the whole of it. */
.tv-omni .tv-bar{
  gap:8px;
  padding:10px 12px;
}
.tv-omni .tv-filter-wrap{
  flex:1 1 auto;
}
.tv-omni .tv-filter{
  flex:1 1 auto;
  font-size:15px;
  padding:7px 11px;
}
/* Its own row under the box, and no gap at all when nothing is applied. The
   suggestion list is positioned and z-indexed, so it lays over this rather
   than being pushed down by it. */
.tv-omni > .tv-chips,.tv-pal > .tv-chips{
  padding:8px 12px;
  border-bottom:1px solid var(--tv-border);
}
/* Palette: the control is summoned, not resident. The veil dims the page and
   the panel sits in the upper third, where a minibuffer or a Telescope prompt
   sits — near the eye rather than centred in it. 90/91 leaves 100/101 free for
   a consumer's own modal, so a materialize sheet still wins over this. */
.tv-veil{
  position:fixed;
  inset:0;
  z-index:90;
  background:var(--tv-veil);
  display:flex;
  justify-content:center;
  align-items:flex-start;
  padding-top:18vh;
}
.tv-panel{
  z-index:91;
  width:min(560px,80vw);
  padding:10px;
  border-radius:8px;
  background:var(--tv-alt);
  border:1px solid var(--tv-border);
  box-shadow:0 10px 30px var(--tv-shadow);
}
.tv-panel .tv-filter{
  font-size:15px;
  padding:7px 11px;
  width:100%;
}
/* The applied filter's identity: the theme's frost, washed over whatever the
   page's ground is, with ordinary foreground for ink. Why frost and why a wash
   rather than the solid it was: CHANGELOG, "chips are a frost wash". */
.tv-pal .tv-chip{
  color:var(--tv-fg);
  background:color-mix(in srgb,var(--tv-frost) var(--tv-chip-wash),transparent);
  border-color:color-mix(in srgb,var(--tv-frost) var(--tv-chip-edge),transparent);
}
/* An ORDERING's identity, washed the way the applied filter's is: the column
   band's own amber, which already means COLUMN everywhere else here — the
   crosshair, the selected band — and a sort token is about a column. The GROUND
   carries the whole difference: one silhouette, the same ink, the same × and
   the same hover, so ordering and narrowing are told apart by hue.

   The two washes are the same WEIGHT, sitting within a step of each other in
   distance from the page they are drawn on (light 24.7 against frost's 24.9,
   dark 73.6 against 71.2), so neither chip reads as the louder. Amber is the
   paler hue (luminance .899 against frost's .741), and the light theme's 52
   against the chip's 45 is what that costs; over black the two travel alike, so
   dark asks for the chip's own 18. The edge takes the chip's strength in either
   theme, a hairline carrying no information. Only a token this renderer ACCEPTS
   as sort wears it — "ordersRows" is that test. */
.tv-pal .tv-chip-sort{
  background:color-mix(in srgb,var(--tv-col) var(--tv-sort-wash),transparent);
  border-color:color-mix(in srgb,var(--tv-col) var(--tv-chip-edge),transparent);
}
/* THE ARROWS ARE DRAWN, not spelled. A chained sort chip carries "->" between
   its columns and the renderer's face is monospace, so a coding font with
   contextual alternates (JetBrains Mono, Fira Code, Cascadia) ligates the pair
   into one arrow and the chip reads as the chain it is. Asked for explicitly
   because it is a fact about this chip rather than a default to inherit: a page
   that turns ligatures off wholesale would otherwise turn this one off with
   them, and a face without the alternate loses nothing but the join. */
.tv-chip-sort{
  font-variant-ligatures:contextual;
}
/* THE COLUMNS CHIP WEARS THE LINK HUE — the third chip voice: frost is the
   applied filter, the column band is the order, and the accent-derived link
   colour marks the token that shapes what the table SHOWS. Same shape, same
   edge rule, same wash arithmetic as the sort chip, one hue over. Only a
   token that names at least one column wears it — "showsColumns" is that
   test — so the half-typed "columns:" keeps the ordinary chip. */
.tv-pal .tv-chip-cols{
  background:color-mix(in srgb,var(--tv-link) var(--tv-cols-wash),transparent);
  border-color:color-mix(in srgb,var(--tv-link) var(--tv-chip-edge),transparent);
}
/* THE SAVED-VIEW CHIP WEARS THE ACCENT — the fourth chip voice, and the token
   that names a whole view rather than shaping one. Same shape, same edge rule,
   same wash arithmetic as its two siblings, one hue over. Only a token naming a
   view the producer DECLARED wears it — "namesView" is that test — so a
   half-typed "view:" and a name nobody carries keep the ordinary chip. */
.tv-pal .tv-chip-view{
  background:color-mix(in srgb,var(--tv-accent) var(--tv-cols-wash),transparent);
  border-color:color-mix(in srgb,var(--tv-accent) var(--tv-chip-edge),transparent);
}
.tv-pal .tv-chip:not(.tv-chip-muted):hover{
  border-color:var(--tv-accent);
  color:var(--tv-accent);
}
.tv-chips{
  display:flex;
  flex-wrap:wrap;
  gap:5px;
  align-items:center;
}
/* One silhouette, spelled once, for every chip in the strip: a live filter
   token and a crumb. What each of them then respells is ink, ground and
   cursor. */
.tv-chip{
  display:inline-flex;
  align-items:center;
  gap:5px;
  padding:1px 4px 1px 8px;
  border-radius:999px;
  font-size:12px;
  cursor:pointer;
  color:var(--tv-fg);
  border:1px solid var(--tv-border);
  background:var(--tv-alt);
}
.tv-chip:not(.tv-chip-muted):hover{
  border-color:var(--tv-accent);
  color:var(--tv-accent);
}
/* A crumb: where the reader came FROM. Same silhouette and same edge as the
   live chip beside it, so the strip reads as one row: the rule respells no
   border at all, which leaves a crumb wearing whatever chip rule reaches it —
   frost-tinted inside the palette, the plain hairline outside it. The right
   padding goes back to the left's wherever the chip's padding is spelled, the
   remove mark being what a live chip is lopsided for. Inertness is carried by
   the cursor and by the hover rules declining to select a crumb, so hovering
   one moves nothing.

   Ink and ground are the whole difference: --tv-muted instead of the
   foreground, the page's own ground instead of the chip panel's. The ink is
   the floor that binds, as everywhere else here. --tv-muted is the tag ink and
   the ground is --tv-bg, which is what a transparent chip is drawn on in every
   mode, so it clears 4.5:1 in both themes (light 5.1, dark 11.5) while sitting
   quieter than a live chip's ink does on its own ground (19.9 and 15.4).
   Spelled with the row it lives in so it outranks the palette's own chip rule,
   the one other place a chip's ground is set.

   AND IT IS STRUCK THROUGH, because a crumb is a query that is NO LONGER IN
   FORCE: the mute alone says quiet, where the rule says the tokens under it
   are not narrowing anything on screen. A drill -- glance's shell binds it to
   the @ key -- leaves
   the whole query it came from standing in the strip, and a reader has to be
   able to tell it from the one that is applied at a glance. */
.tv-chips .tv-chip-muted{
  color:var(--tv-muted);
  background:transparent;
  cursor:default;
  padding-right:8px;
  text-decoration:line-through;
}
.tv-chip-x{
  font-style:normal;
  opacity:.55;
  padding:0 3px;
}
.tv-chip:hover .tv-chip-x{
  opacity:1;
}
/* The pin button-badge: far edge of the strip, dim until it is true. */
.tv-chips .tv-pin{
  margin-left:auto;
  cursor:pointer;
  opacity:.35;
  font-size:12px;
  line-height:1.4;
  user-select:none;
  filter:grayscale(1);
}
.tv-chips .tv-pin:hover{
  opacity:.7;
}
.tv-chips .tv-pin.tv-pinned{
  opacity:1;
  filter:none;
}
/* The suggestion list hangs under the box, over the table. .tv-root clips with
   overflow:hidden, so it scrolls internally rather than growing past it. */
.tv-ac{
  position:absolute;
  top:100%;
  left:0;
  min-width:100%;
  z-index:5;
  margin-top:2px;
  max-height:min(288px,40vh);
  overflow-y:auto;
  background:var(--tv-bg);
  border:1px solid var(--tv-border);
  border-radius:6px;
  box-shadow:0 4px 12px var(--tv-shadow);
}
.tv-ac-item{
  display:flex;
  justify-content:space-between;
  align-items:baseline;
  gap:14px;
  padding:3px 10px;
  white-space:nowrap;
  cursor:pointer;
  color:var(--tv-fg);
}
.tv-ac-n{
  color:var(--tv-muted);
  font-variant-numeric:tabular-nums;
}
/* An offer that is free text rather than a predicate says so where the counts
   are, in the ink the counts wear: it annotates the row, and no row carries
   both. */
.tv-ac-aside{
  color:var(--tv-muted);
}
/* A producer meta names a set only the producer can enumerate, and reads as
   the notation it is rather than as a value beside the concrete ones. */
.tv-ac-dim{
  opacity:.6;
  font-style:italic;
}
.tv-ac-note{
  padding:5px 10px;
  border-top:1px solid var(--tv-border);
  color:var(--tv-muted);
  font-size:11px;
  white-space:nowrap;
}
.tv-ac-item:hover{
  background:var(--tv-hover);
  color:var(--tv-accent);
}
/* The theme's own selections (ivy-current-match, company-tooltip-selection)
   are full-strength golden with bold weight and the default foreground — an
   accent-coloured label on that ground would be unreadable. */
.tv-ac-on{
  background:var(--tv-sel);
  color:var(--tv-fg);
  font-weight:600;
}
.tv-scroll{
  overflow:auto;
  position:relative;
}
.tv-table{
  border-collapse:collapse;
  width:100%;
}
/* THE TITLE COLUMN FILLS; EVERY OTHER COLUMN IS EXACTLY ITS CONTENT.
   table-layout:fixed is what makes that real. Under auto a col width is a hint
   and the browser hands the window's slack to every column in proportion, so
   the gutter and the date columns grew with the window while the one column
   whose text runs long stayed as narrow as the rest. Fixed makes the col
   widths authoritative and leaves the ONE column carrying no width — the
   title's — to take what the others left; it is also what lets text-overflow
   reach a cell at all. The table keeps a min-width, written by applyWidths, so
   a window narrower than the sized columns plus the title's floor scrolls
   sideways, which is what overflow:auto on the scroller already did. A view
   with no title column has nothing to fill with and keeps the auto layout. */
.tv-table.tv-fill{
  table-layout:fixed;
}
/* A capped column, and a title narrower than its own text, end in an ellipsis
   rather than spilling under the column beside them. The gutter stays out of
   it: its glyph is exactly its width, so a rounding hair would eat the ]. */
.tv-fill th:not(.tv-box),.tv-fill td:not(.tv-box){
  overflow:hidden;
  text-overflow:ellipsis;
}
/* A header never widens its column: the cells set the width and a longer
   header is squeezed into it. What gets squeezed is the
   WORD — the pair is a flex row, the word shrinks to an ellipsis (min-width:0
   is what lets a flex item go under its own text) and the mark declines to
   shrink at all, so a sorted column always still says which way it is sorted
   and where it sits in the chain. The row is a span inside the cell rather
   than the cell itself because display:flex on a table-cell stops it being
   one. Nothing here fires without .tv-fill: with no column to fill, the header
   is paid for in the width and there is nothing to squeeze. */
.tv-fill th .tv-hd{
  display:flex;
  align-items:baseline;
  min-width:0;
}
.tv-fill th .tv-hn{
  overflow:hidden;
  text-overflow:ellipsis;
  min-width:0;
}
.tv-fill th .tv-arrow{
  flex:none;
}
/* A flex row does not take the cell's text-align, so the one alignment a
   column can declare is restated as the row's own. The CELLS are untouched —
   nothing made them flex — so this is the header catching up with them. */
.tv-fill th.tv-right .tv-hd{
  justify-content:flex-end;
}
/* The gutter's own measure and nothing over it: [X] is three characters, and
   24px is the cell padding both sides. The slack it used to carry was the auto
   layout's share of the window, which the fixed layout above no longer hands
   it. Written on the col because a cell's width is not what fixed layout
   reads, and for the same reason the coarse-pointer target below has to be
   restated here as a width — the min-width on the cell is inert under it. */
.tv-fill col.tv-gut{
  width:calc(3ch + 24px);
}
.tv-table th,.tv-table td{
  padding:5px 12px;
  text-align:left;
  white-space:nowrap;
  border-bottom:1px solid var(--tv-border);
}
/* AN EMPTY CELL STILL FORMS A LINE BOX. A td with no text has no inline
   content and collapses to its padding, so a row whose cells are all empty --
   a property just added, a record awaiting its first edit -- stood a third
   the height of its neighbours, and an overlay anchored to its rect squashed
   with it. A zero-width space costs nothing visible and holds the line. */
.tv-table td:empty::after{
  content:"\\200B";
}
.tv-table th{
  position:sticky;
  top:0;
  background:var(--tv-bg);
  font-weight:600;
  color:var(--tv-muted);
  user-select:none;
  z-index:1;
}
.tv-table th.tv-sortable{
  cursor:pointer;
}
.tv-table th.tv-sortable:hover{
  color:var(--tv-accent);
}
.tv-table td.tv-right,.tv-table th.tv-right{
  text-align:right;
  font-variant-numeric:tabular-nums;
}
.tv-table tbody tr.tv-alt{
  background:var(--tv-alt);
}
/* A marked row's ground: the muted ink washed over the page's. It REPLACES the
   zebra rather than layering over it — one background slot, and a mark outranks
   a stripe — and it is neither of the washes that already say something, frost
   being the applied filter and --tv-sel the cursor. The cursor's rule follows
   this one, so a row that is both reads as the cursor and keeps its checked
   box. Faint because the floor binds: the tag ink is --tv-muted too, so each
   theme washes only as far as that ink stays above 4.5:1 on it (light 4.6,
   dark 6.3). */
.tv-table tbody tr.tv-marked{
  background:color-mix(in srgb,var(--tv-muted) var(--tv-mark-wash),transparent);
}
/* A flagged row: the flag red washed over the page's ground, the same one-slot
   rule the mark follows. It sits between them in source order, which IS the
   precedence — cursor over flag over mark over zebra — because all four write
   the one background slot at the one specificity. Washed as far as the ink
   allows and no further, which for a colour this dark is not far on white:
   --tv-muted is the tag ink and the light strength is what keeps it above
   4.5:1 (4.6 at 8%, and under the floor by 10%). Dark has the room to take
   30%. The two numbers are measured, not chosen. */
.tv-table tbody tr.tv-flagged{
  background:color-mix(in srgb,var(--tv-flag) var(--tv-flag-wash),transparent);
}
.tv-table tbody tr.tv-sel{
  background:var(--tv-sel);
}
/* The background is one slot and the cursor wins it, so a flagged row under
   the cursor would otherwise stop saying it is flagged. The edge is a second
   channel that no other state writes: it survives every combination, which is
   what keeps the state readable rather than merely painted. */
.tv-table tbody tr.tv-flagged td:first-child{
  box-shadow:inset 3px 0 0 var(--tv-flag);
}
/* The gutter is chrome, the way the pager is: a fixed leading box that no
   producer sent and no width measurement sees. It is the CHECKBOX's alone —
   the flag's edge rides the row's FIRST cell whichever that is (the gutter
   under marks, the first data cell without them), so a mount that flags
   without marking pays no empty leading column.

   The checkbox is the MARKING table's alone, which is what .tv-marking on the
   root says. Blank header, org's own checkbox for a
   cell, and the box brightens on the rows it is checked on. The glyph is drawn
   from the row's class rather than written into the cell, so the state has one
   home: the class the row already carries. */
.tv-table th.tv-box,.tv-table td.tv-box{
  width:3ch;
  color:var(--tv-muted);
  user-select:none;
}
.tv-marking .tv-table td.tv-box{
  cursor:pointer;
}
.tv-marking .tv-table td.tv-box::before{
  content:"[ ]";
}
.tv-marking .tv-table tbody tr.tv-marked td.tv-box{
  color:var(--tv-fg);
}
.tv-marking .tv-table tbody tr.tv-marked td.tv-box::before{
  content:"[X]";
}
/* The selection is the row, and it crossfades in place — no overlay to keep in
   step with the rows underneath it. */
.tv-table tbody tr,.tv-table tbody td{
  transition:background-color .08s ease-out,
  box-shadow .08s ease-out;
}
.tv-calm .tv-table tbody tr,.tv-calm .tv-table tbody td{
  transition:none;
}
/* A cell selection draws two bands and their crossing, and all three are
   grounds — no outline, no border, no shadow anywhere in the selection. The
   column's band is a wash of the amber over whatever the ROW painted: the row
   states write the tr and these write the td, which the table paints above it,
   so the two never contest a slot, and the film being translucent is what
   leaves the zebra, the mark, the flag and the cursor all still reading
   through it, quieter inside the band than out. The one contest is here,
   between these two rules on the one td, and it is settled the way the row
   stack settles its own: equal specificity, source order, cell after column.

   The header is the same wash mixed into the page's ground rather than laid
   over it — the same colour, arrived at opaquely, because the header is sticky
   and rows scroll under it.

   Both strengths are measured against the grounds they can land on, and those
   grounds differ: the film lands on the page, the stripe, a mark and a flag,
   while the cell lands on the cursor row alone. Light is set by what reads —
   the band moving a ground between half and nine tenths as far as a mark moves
   the page, since a locator must stay quieter than a state — and dark's cell by
   what the ink allows: 9% leaves the tag ink at 4.61:1 on the cursor row and
   one point more puts it under 4.5, so the dark crosshair reads by the ground
   beneath it rather than by the point of wash above it. */
.tv-table th.tv-colsel{
  background:color-mix(in srgb,var(--tv-col) var(--tv-col-wash),var(--tv-bg));
}
.tv-table tbody td.tv-colsel{
  background:color-mix(in srgb,var(--tv-col) var(--tv-col-wash),transparent);
}
.tv-table tbody td.tv-cell-sel{
  background:color-mix(in srgb,var(--tv-col) var(--tv-cell-wash),transparent);
}
/* WHAT A LINK LOOKS LIKE, spelled once for the two places one is drawn: the
   anchor a cell's own Org markup produces, and the whole title cell of a row a
   producer marked linked. One declaration, so a title that is half markup and
   half plain words comes out ONE colour — it used to come out two, the markup
   in the accent and the words in body ink under a cell-wide underline.
   The only state on this table written in TEXT rather than in a ground: the
   four row washes and the two selection bands all write backgrounds, so this
   contests none of them and reads through every combination — a linked row
   under the cursor, with the column band across that very cell, is still a
   link. Which is what --tv-link is measured on: every one of those grounds,
   4.5:1 on all of them, rather than the page alone. */
.tv-link,.tv-table tbody td.tv-linked{
  color:var(--tv-link);
  text-decoration:underline;
  text-underline-offset:2px;
}
.tv-table tbody tr{
  cursor:default;
}
.tv-table tbody tr.tv-pad td{
  padding:0;
  border:0;
}
/* The third role, and the quietest: no box at all. A filled pill is a state, a
   frost chip is an applied filter, and a tag is small muted text — which is
   what a tag is, a word the row happens to carry. Several of them separate on a
   middot rather than on the colons the cell spells them with; the colons are
   the storage, not the reading. The ink is the muted one the palette already
   carries (dark #A4C2EB, light #667071), both clear of the text floor. */
.tv-tag,.tv-tags{
  color:var(--tv-muted);
  font-size:.92em;
}
.tv-tags .tv-tag{
  font-size:inherit;
  color:inherit;
}
/* never compound the two */
/* Shown in the form a query spells them, so what is read is what is typed: the
   value domain lowercases, and the tag key matches its value folded. Done in
   the stylesheet rather than in the markup, so the text a copy takes is the
   text the file holds. */
.tv-tag{
  text-transform:lowercase;
}
.tv-pill{
  display:inline-block;
  padding:0 8px;
  border-radius:999px;
  font-weight:600;
  color:var(--tv-ink,var(--tv-badge));
  background:color-mix(in srgb,var(--tv-badge) 15%,transparent);
}
/* The order, written over the columns it orders. Every key of the chain marks
   its own header: the leading one in full ink because it is what the reader is
   reading by, the tie-breakers behind it dimmed to the muted floor, each
   wearing the place it holds in the chain. */
.tv-arrow{
  margin-left:4px;
  opacity:.55;
}
.tv-arrow.tv-lead{
  opacity:1;
}
.tv-ord{
  font-style:normal;
  font-size:.75em;
  vertical-align:baseline;
}
.tv-empty{
  padding:16px 12px;
  color:var(--tv-muted);
}
.tv-hint{
  padding:6px 12px;
  border-top:1px solid var(--tv-border);
  color:var(--tv-muted);
  font-size:12px;
}
/* A finger is not a pointer. Targets grow to the ~44px everyone settled on, and
   they grow by padding rather than by a set height, so the rows stay uniform
   and the measured row height carries the change into the windowing and the
   scroll arithmetic on its own. The filter reaches 16px because anything under
   it makes iOS zoom the page on focus. The chip's remove mark stops hiding
   behind a hover nobody can perform. */
@media (pointer:coarse){
  .tv-table th,.tv-table td{
    padding:12px;
  }
  .tv-table td.tv-box{
    min-width:44px;
  }
  .tv-fill col.tv-gut{
    width:max(calc(3ch + 24px),44px);
  }
  .tv-ac-item{
    padding:12px 12px;
  }
  .tv-chip{
    padding:13px 8px 13px 12px;
  }
  .tv-chips .tv-chip-muted{
    padding-right:12px;
  }
  .tv-chip-x{
    opacity:1;
    padding:0 8px;
  }
  .tv-filter,.tv-omni .tv-filter,.tv-panel .tv-filter{
    font-size:16px;
  }
}
.tv-key{
  color:var(--tv-fg);
  font-weight:600;
}
.tv-pg{
  color:var(--tv-accent);
  font-weight:600;
  cursor:pointer;
}
.tv-pg:hover{
  text-decoration:underline;
}
.tv-pg-off{
  color:var(--tv-muted);
  font-weight:400;
  cursor:default;
  text-decoration:none;
}
`;
    const el = document.createElement("style");
    el.textContent = css;
    document.head.appendChild(el);
  }

  /**
   * Mount a table-view into CONTAINER; return a live handle (the streaming API).
   * @param {Element} container
   * @param {View} view
   * @param {MountOptions} [opts]
   * @returns {Handle}
   */
  function mount(container, view, opts) {
    injectStyle();
    const o = opts || {};   // narrowing sticks in closures (a reassigned param would not)
    // Composer mode: the filter IS the widget — the omnibox bar and the chip
    // strip, with no table, no status line and no row machinery behind them.
    // For a consumer that wants the query language (completion, chips, DEL)
    // as a form control: the committed query arrives at `onFilter' and reads
    // back off `getQuery', exactly as it does over a table.
    const composer = o.composer === true;
    const omnibox = o.omnibox === true || composer;
    const palette = o.palette === true;
    const marks = o.marks === true;
    /**
     * Whether the FLAG state is drawn. Absent it follows `marks', which is the
     * one opt-in flags shipped under, so a consumer that never named it gets
     * the table it already had. Named, it is its own answer: `flags: true'
     * alone draws the flag ground and its edge on the first cell, gutterless —
     * a consumer whose rows carry a pending action but no standing selection —
     * and `flags: false' under `marks: true' takes the flag drawing back off.
     */
    const flags = o.flags === undefined ? marks : o.flags === true;
    const actionHints = o.actionHints !== false;   // absent means the legend shows
    /**
     * What to offer about a flagged row, in the consumer's own words — e.g.
     * `"d/D archive · u unflag"'. Shown only while the cursor sits on a
     * flagged row; absent, the segment is the plain count it always was.
     */
    const flagHelp = typeof o.flagHelp === "string" && o.flagHelp.trim()
      ? o.flagHelp.trim() : "";
    /**
     * FLAGHELP marked up like the action legend: the token before each label
     * is a key, the rest is words. Split once at mount rather than per render,
     * since a hint line is rewritten on every selection move.
     */
    const flagHelpHTML = flagHelp.split("·").map((part) => {
      const t = part.trim();
      if (!t) return "";
      const at = t.indexOf(" ");
      return at === -1 ? `<b class="tv-key">${esc(t)}</b>`
        : `<b class="tv-key">${esc(t.slice(0, at))}</b> ${esc(t.slice(at + 1).trim())}`;
    }).filter(Boolean).join(" · ");
    /**
     * How a live chip should read, when the token itself is not what a reader
     * wants shown — `(token) => string|null', anything but a non-empty string
     * leaving the token raw. Display only: the query is never touched.
     * @type {((token: string) => string|null)|null}
     */
    const chipLabel = typeof o.chipLabel === "function" ? o.chipLabel : null;
    /**
     * The PIN: a button-badge at the chip strip's far edge. Present only when
     * a consumer passes `onPin' — the renderer knows nothing about what
     * pinning MEANS, it reports the click and wears the boolean. `pinned'
     * seeds the badge and `setPinned' moves it; the consumer decides both,
     * since only it knows what the applied query is being compared against.
     * @type {(() => void)|null}
     */
    const onPin = typeof o.onPin === "function" ? o.onPin : null;
    let pinned = !!o.pinned;
    /**
     * How many chrome cells lead a row; what a column index has to skip. The
     * gutter is the CHECKBOX's alone: the flag's edge rides the row's first
     * cell whichever that is, so a mount that flags without marking pays no
     * empty leading column for it.
     */
    const chrome = marks ? 1 : 0;
    /** Rows per page, or 0 for the whole set at once — which is every consumer
     *  that has not asked otherwise. */
    const pageSize = Math.max(0, Math.trunc(Number(o.pageSize) || 0));
    /** The page on show, counted from zero. */
    let page = 0;
    /**
     * Which of the two presentations a paged view is in.
     *
     * PAGED (false) is the slice: `paged' hands back one page and the
     * virtualizer runs inside it, which is what an explicit page turn wants —
     * a different set of rows, arrived at crisply.
     *
     * CONTINUOUS (true) lets the window run over the whole ordered set, which
     * is the machinery paging was layered on top of. Stepping the selection
     * off the end of a page switches to it AT THAT MOMENT: the cursor simply
     * moves onto the next row and the scroll band eases as it does within a
     * page, so a held key crosses the seam without the blink a page turn
     * would cost. The pager then reads as orientation rather than as state —
     * it says which page the CURSOR is in — and any explicit turn snaps back.
     */
    let continuous = false;

    /** Is the table being drawn dark? The page's choice outranks the system's. */
    function darkNow() {
      const root$ = document.documentElement;
      const asked = root$ && root$.getAttribute ? root$.getAttribute("data-theme") : null;
      if (asked === "dark") return true;
      if (asked === "light") return false;
      return typeof matchMedia === "function"
          && matchMedia("(prefers-color-scheme: dark)").matches;
    }
    let dark = darkNow();
    /**
     * @type {{ view: View, rows: Row[], filter: string,
     *          selected: string|null, selCol: number|null, sortKeys: SortKey[] }}
     */
    const state = {
      view: view || { columns: [] },
      rows: (view && view.rows) ? view.rows.slice() : [],
      filter: "",
      selected: null,
      selCol: null,
      sortKeys: normalizeSort(view && view.sort),
    };

    // ---- caches ------------------------------------------------------------
    // Two row lists stand between the store and the window. `sorted' is every
    // row in sort order; `order' is `sorted' under the filter, and is what the
    // window renders from. A filter change re-derives `order' alone, so typing
    // never re-sorts; upsert and delete splice both in place; a rows or sort
    // change drops both. A selection change touches neither.

    /** @type {Row[]|null} */
    let sorted = null;
    /** @type {Row[]|null} */
    let order = null;
    /**
     * The compiled query `order' was filtered by; null when unfiltered. An
     * upsert asks it whether the row still belongs.
     * @type {((r: Row) => boolean)|null}
     */
    let orderTest = null;
    /**
     * The comparator `sorted' is in, for binary-inserting an upsert.
     * @type {((a: Row, b: Row) => number)|null}
     */
    let orderCmp = null;
    /**
     * Max display length per column over `order'; null when stale.
     * @type {number[]|null}
     */
    let widths = null;
    /** @type {Map<string, RowText>} */
    const texts = new Map();
    /**
     * A column's distinct cell values, for the suggestion list — computed on
     * demand and thrown away with the text cache, since the rows are what it
     * was read off.
     * @type {Map<string, {list: string[], counts: Map<string, number>}>}
     */
    const domains = new Map();

    /** Cached display data for row R. @param {Row} r  @returns {RowText} */
    function rowText(r) {
      let t = texts.get(r.id);
      if (!t) {
        const cols = columns(), cs = r.cells || {};
        const parts = new Array(cols.length), len = new Array(cols.length);
        for (let i = 0; i < cols.length; i++) {
          const s = displayText(cs[cols[i].key]);
          len[i] = s.length;
          parts[i] = s.toLowerCase();
        }
        t = { search: parts.join("\x1f"), len, cells: parts };
        texts.set(r.id, t);
      }
      return t;
    }

    /** Forget the cached display data, and the value domains read off it. */
    function clearTexts() { texts.clear(); dropDomains(); }

    /**
     * Forget what was read off the rows: value domains, the tag vocabulary and
     * the title index. The index is the expensive one, so its rebuild is queued
     * for an idle moment rather than left for whoever types next.
     */
    function dropDomains() {
      domains.clear();
      vocab = null;
      wordIndex = null;
      // The verdict about which column holds lists was read off the rows like
      // everything else here, so it dies with them. Kept, it outlives its
      // evidence: a table mounted before its rows arrive — an empty store, a
      // query that matched nothing, a mount filled by `setRows' a moment later
      // — decides there is no such column and never looks again, and the tag
      // values and their arity go with it.  Date-ness is read off the rows the
      // same way and dies with them for the same reason.
      multiAt = undefined;
      dateAt = undefined;
      queueIndex();
    }

    /**
     * Build the word index once the rows have stopped moving, off the path a
     * keystroke takes. An edit burst — a stream of upserts, a paged load — re
     * -queues this rather than rebuilding per row, and only the quiet at the
     * end of it pays. A keystroke arriving first finds no index and builds one
     * itself, which is the cost this exists to avoid and the worst case it
     * cannot be worse than.
     */
    let idleAt = 0, idleGen = 0;
    function queueIndex() {
      const mine = ++idleGen;          // anything already queued is now stale
      if (idleAt) clearTimeout(idleAt);
      idleAt = setTimeout(() => {
        idleAt = 0;
        idle(() => { if (mine === idleGen) titleIndex(); });
      }, SETTLE);
    }

    /**
     * The multi-valued column's VALUE DOMAIN: every distinct tag its cells
     * spell, and the rows behind each. It is what `tag:' completes against and
     * counts by — a raw `:a:b:' cell can never prefix-match a bare word.
     * Cached and thrown away with the text cache, since the rows are what it
     * was read off.
     * @type {{list: string[], ids: Map<string, Set<string>>}|null}
     */
    let vocab = null;

    /**
     * Does column I hold cells of a shape, read off up to `SAMPLE' non-empty
     * ones? SHAPEDBY is the evidence FOR and CONTRARYTO the evidence AGAINST,
     * asked only of the cells the first declined: `SHAPED' cells carrying the
     * shape and none arguing against it settle it.
     *
     * The middle ground is the point of the pair. Asking every sampled cell to
     * carry the shape lets one import, one hand-edited headline, one stray
     * anywhere in the sample decide a whole column — for tags that is a corpus
     * with no vocabulary at all, no keys, no values, no completions; for dates
     * it is a column losing its prefix matching. So a cell that merely fails to
     * be the shape ABSTAINS: a bare word holds no delimiter to show either way,
     * and a stamp org spelled its own way is not prose. What argues against is a
     * cell that could not be the shape at all — a colon arranged some other way,
     * a sentence — which the column this is asking about would not be full of.
     * @param {number} i
     * @param {(s: string) => boolean} shapedBy
     * @param {(s: string) => boolean} contraryTo
     */
    function sampledShape(i, shapedBy, contraryTo) {
      let shaped = 0, contrary = 0, seen = 0;
      for (const r of state.rows) {
        const cell = rowText(r).cells[i];
        if (!cell) continue;
        if (shapedBy(cell)) shaped++;
        else if (contraryTo(cell)) contrary++;
        if (++seen >= SAMPLE) break;
      }
      return shaped >= SHAPED && !contrary;
    }

    /**
     * The multi-valued column's index, or -1. SCHEMA calls a column
     * multi-valued when its cells hold delimited value lists — org's `:a:b:'
     * being the canonical shape — so this is decided by looking at the cells
     * rather than by the column's name: glance's key has been `tags' and is
     * `tag', and neither spelling is the renderer's business.
     */
    function multiColumn() {
      if (multiAt !== undefined) return multiAt;
      const cols = columns();
      multiAt = -1;
      // A column that says what it is settles the question; the shape below is
      // how the answer is guessed when nobody said.
      const declared = cols.findIndex((c) => c.multi === true);
      if (declared !== -1) return (multiAt = declared);
      return (multiAt = cols.findIndex((_, i) => sampledShape(
        i, (s) => ORG_TAGS.test(s), (s) => s.indexOf(":") !== -1)));
    }
    /** @type {number|undefined} */
    let multiAt;

    /** The `title' column's index, or -1; where a whole-title offer is read. */
    function titleColumn() { return columns().findIndex((c) => c.key === "title"); }

    /**
     * The tag vocabulary, derived once per row set: the tags themselves, sorted,
     * and the rows each holds. The rows are what the count beside an offer
     * counts, so they are kept rather than recounted per keystroke.
     */
    function tagVocab() {
      if (vocab) return vocab;
      const at = multiColumn();
      const ids = new Map();
      if (at !== -1)
        for (const r of state.rows)
          for (const tag of tagsIn(rowText(r).cells[at])) {
            const held = ids.get(tag);
            if (held) held.add(r.id); else ids.set(tag, new Set([r.id]));
          }
      vocab = { list: Array.from(ids.keys()).sort(), ids };
      return vocab;
    }

    /** Drop the filtered list (and the widths it implies). */
    function dropOrder() { order = null; widths = null; cancelEase(); }
    /** Drop the sort too: the rows, the columns or the sort keys moved. */
    function dropSorted() { dropOrder(); sorted = null; orderCmp = null; }

    // ---- persistent chrome -------------------------------------------------
    // Built once. Only the tbody window, the hint, the sort arrows and the
    // buttons' disabled state change afterwards, so the filter input — never
    // recreated — keeps focus and caret across every update.

    // Read once. A page that asks for less motion gets neither the crossfade
    // nor the scroll ease — the selection lands and the viewport jumps — while
    // the coalescing, which is not motion, stays.
    const calm = typeof matchMedia === "function"
              && matchMedia("(prefers-reduced-motion: reduce)").matches;

    const root = document.createElement("div");
    // The whole root class in one derivation, the pairs `rowHTML' uses.
    // `tv-marking' is what scopes the checkbox: the gutter belongs to either
    // row state, the box in it only to a table that marks.
    root.className = classAttr([["tv-root", true], ["tv-marking", marks],
                                ["tv-calm", calm], ["tv-omni", omnibox && !palette],
                                ["tv-pal", palette]]);
    container.innerHTML = "";
    container.appendChild(root);

    const bar = document.createElement("div");
    bar.className = "tv-bar";
    const titleEl = document.createElement("span");
    titleEl.className = "tv-title";
    const input = document.createElement("input");
    input.className = "tv-filter";
    input.type = "search";
    // The box's purpose is obvious; its grammar is not. So the placeholder
    // teaches that instead — four concrete forms rather than a description of
    // a syntax, separated by middots so they read as examples and not as one
    // query someone is meant to complete. Keys are taught by the legend and
    // the list, so they stay out of it.
    input.placeholder = `tag:book · state:TODO|DONE · -word · "some phrase"`;
    // The box and its suggestion list travel together, so the list can be
    // positioned against the box and nothing else.
    const chipsEl = document.createElement("div");
    chipsEl.className = "tv-chips";
    const filterWrap = document.createElement("div");
    filterWrap.className = "tv-filter-wrap";
    const acEl = document.createElement("div");
    acEl.className = "tv-ac";
    acEl.style.display = "none";
    filterWrap.appendChild(input);
    filterWrap.appendChild(acEl);
    // In omnibox mode the filter is the bar: no title, and the control grows to
    // fill what the title was using. The applied parts then get a row of their
    // own under it rather than crowding the caret — appended to the root below,
    // between the bar and the table.
    //
    // In palette mode there is no bar at all. The page keeps the chip row and
    // nothing else, so a table nobody has filtered carries no filter chrome
    // whatever; the control lives in an overlay that `openFilter' summons.
    if (!omnibox && !palette) { bar.appendChild(titleEl); bar.appendChild(chipsEl); }
    if (!palette) bar.appendChild(filterWrap);

    // The palette: a backdrop that dims the page and a panel that holds the
    // control. Built whether or not it is used, so the machinery below has one
    // input to talk to either way.
    const veil = document.createElement("div");
    veil.className = "tv-veil";
    veil.style.display = "none";
    const panel = document.createElement("div");
    panel.className = "tv-panel";
    if (palette) {
      panel.appendChild(filterWrap);
      veil.appendChild(panel);
    }

    const scroll = document.createElement("div");
    scroll.className = "tv-scroll";
    const table = document.createElement("table");
    table.className = "tv-table";
    const colgroup = document.createElement("colgroup");
    const thead = document.createElement("thead");
    const headRow = document.createElement("tr");
    const tbody = document.createElement("tbody");
    thead.appendChild(headRow);
    table.appendChild(colgroup);
    table.appendChild(thead);
    table.appendChild(tbody);
    const empty = document.createElement("div");
    empty.className = "tv-empty";
    empty.textContent = "no rows";
    scroll.appendChild(table);
    scroll.appendChild(empty);
    const hint = document.createElement("div");
    hint.className = "tv-hint";

    if (!palette) root.appendChild(bar);
    if (omnibox || palette) root.appendChild(chipsEl);
    if (!composer) { root.appendChild(scroll); root.appendChild(hint); }
    if (palette) root.appendChild(veil);

    /** Per-column <col>, one per column. @type {HTMLElement[]} */
    let colEls = [];
    /** Per-column sort arrow, one per column. @type {HTMLElement[]} */
    let arrowEls = [];

    // Measured geometry and the window currently in the tbody.
    const geom = { row: ROW_H, head: ROW_H };
    /**
     * The window in the tbody: its half-open span, and the display order it was
     * DRAWN from. `renderRows' is the only thing that writes the tbody, so the
     * k-th data row in there is `rows[first + k]' by construction — which is
     * how `stampSelection' gets back to the row (and the index) behind a `tr'
     * without asking the state a second time and risking a different answer.
     * @type {{ first: number, last: number, rows: Row[] }}
     */
    const win = { first: -1, last: -1, rows: [] };
    let remeasuring = false;
    /** The selected row's index in display order, or -1; what the bar reads. */
    let selAt = -1;

    /**
     * @param {Sort|Sort[]|undefined} sort
     * @returns {SortKey[]}
     */
    /**
     * Read SCHEMA's sort list into sort keys.  A `direction' string wins over
     * `ascending' and is SCHEMA's way to ask for nulls first: bare "asc" and
     * "desc" put empty cells last whatever the column type.  With no
     * `direction' a boolean `nullsFirst' is read instead, which is the shape
     * `getSort' answers in — so a chain read out of the table and handed back
     * to `setSort' is the chain that was read.
     */
    function normalizeSort(sort) {
      if (!sort) return [];
      const list = Array.isArray(sort) ? sort : [sort];
      return list
        .filter((s) => s && s.column)
        .map((s) => {
          const dir = String(s.direction || "").toLowerCase();
          const desc = dir ? dir.slice(0, 4) === "desc" : s.ascending === false;
          return {
            column: s.column,
            ascending: !desc,
            nullsFirst: dir ? dir.indexOf("nulls-first") !== -1 : !!s.nullsFirst,
          };
        });
    }

    function columns() { return state.view.columns || []; }
    /**
     * The SAVED VIEWS the producer named, each a `{name, query}'. The
     * vocabulary `view:' completes from, and the whole of what this side knows
     * about them: a view is APPLIED by whoever owns the fetching, so a producer
     * that grows one needs nothing here.
     * @returns {{name: string, query?: string}[]}
     */
    function savedViews() { return state.view.views || []; }
    function actions() { return state.view.actions || []; }
    function colByKey(k) { return columns().find((c) => c.key === k); }
    /** The columns a sort key may name: every one the view carries, `sortable'
     *  gating the reader's gesture rather than the token. @param {string} k */
    const namesColumn = (k) => !!colByKey(k);

    // ---- order: filter, sort, widths ---------------------------------------

    /**
     * One comparator for the whole sort chain, built once per re-sort: each
     * column's comparator is resolved here, then reused for every comparison.
     * @returns {((a: Row, b: Row) => number)|null}
     */
    function chainComparator() {
      /** @type {{key: string, cmp: (a: Cell|undefined, b: Cell|undefined) => number, sign: number, nullsFirst: boolean}[]} */
      const keys = [];
      for (const sk of state.sortKeys) {
        const col = colByKey(sk.column);
        if (col)
          keys.push({
            key: sk.column,
            cmp: comparator(col),
            sign: sk.ascending ? 1 : -1,
            nullsFirst: !!sk.nullsFirst,
          });
      }
      if (!keys.length) return null;
      /**
       * Rank one key.  Empty cells are settled before the comparator runs and
       * outside the direction sign, so reversing the sort does not drag the
       * blanks along with it.
       */
      const rank = (k, a, b) => {
        const av = (a.cells || {})[k.key];
        const bv = (b.cells || {})[k.key];
        const ae = displayText(av) === "";
        const be = displayText(bv) === "";
        if (ae !== be) return (ae ? 1 : -1) * (k.nullsFirst ? -1 : 1);
        if (ae) return 0;
        return k.sign * k.cmp(av, bv);
      };
      if (keys.length === 1) return (a, b) => rank(keys[0], a, b);
      return (a, b) => {
        for (const k of keys) {
          const c = rank(k, a, b);
          if (c) return c;
        }
        return 0;
      };
    }

    /**
     * The sort chain as something to read, highest priority first. The hint
     * line's description is derived from this one walk, so nothing can name a
     * key the rows are not actually ordered by. A key naming no column is
     * dropped, the way `chainComparator' drops it.
     * @returns {{key: SortKey, header: string}[]}
     */
    function sortChain() {
      const out = [];
      for (const k of state.sortKeys) {
        const col = colByKey(k.column);
        if (col) out.push({ key: k, header: String(col.header || col.key) });
      }
      return out;
    }

    /**
     * The chain as the hint line spells it: `dept asc → score desc'. Column
     * KEYS rather than headers, and the same words `table-view.el' prints, so
     * the two renderers describe one order alike. Empty cells sort last by
     * default and that is left implicit; nulls-first is spelled out.
     */
    function sortText() {
      return sortChain()
        .map(({ key }) => `${key.column} ${key.ascending ? "asc" : "desc"}`
                        + (key.nullsFirst ? " nulls-first" : ""))
        .join(" → ");
    }

    /** The column keys `parseQuery' resolves predicates against. */
    const columnKeys = () => columns().map((c) => c.key);

    /**
     * Every key a token may name: the view's columns, then `planned' and
     * `sort' where no column already carries the name. One spelling for the two
     * places that ask — the resolution list and the completion tier — so a view
     * with a column of its own called `planned' cannot list it twice in one and
     * once in the other.
     *
     * The view's own, and nothing the rows imply: an org tag names no key, so
     * `tag:course' is the one spelling and `course:text' is the two tokens
     * `tag:course text'. A vocabulary read off the rows made the SAME token a
     * predicate for a renderer holding the tagged row and free text for one
     * that was not, which is a query meaning two things on one wire.
     */
    function queryKeys() {
      const keys = columnKeys();
      for (const k of [PLANNED_KEY, SUBSTRING_KEY])
        if (keys.indexOf(k) === -1) keys.push(k);
      for (const k of VIEW_KEYS) if (keys.indexOf(k) === -1) keys.push(k);
      return keys;
    }

    /**
     * The order in force under query Q: the chain Q names, else the order the
     * view was STATED in. So the declared sort is invisible until a reader
     * diverges from it, and taking the last sort token off is the way home.
     * `sort:*none*' names the EMPTY chain, which is a divergence like any other
     * — it replaces the declared order rather than falling back to it, and the
     * rows are read in the order they arrived.
     * @param {string} q  @returns {SortKey[]}
     */
    function chainFor(q) {
      const named = sortsIn(q, queryKeys(), namesColumn);
      return named === null ? stated : named;
    }

    /**
     * The order this view opens in: its declared `sort', or the last one a
     * producer stated through `sortBy'/`setSort'. What `chainFor' falls back to,
     * and the whole of what a query naming no sort key leaves standing.
     * @type {SortKey[]}
     */
    let stated = state.sortKeys;

    /** An ISO-ish date cell, which SCHEMA gives prefix matching. */
    const DATEISH = /^\d{4}-\d{2}(-\d{2})?([ T]\d{2}:\d{2})?$/;
    /** A cell that might be meant as a date: org brackets it, or it opens with
     *  a year. Not proof of one, but not evidence of prose either. */
    const COULD_BE_DATE = /^[<[]?\d/;

    /**
     * Does column I hold dates? Weighed by `sampledShape', the way the
     * multi-valued column is weighed: an ISO date is the evidence for, and a
     * cell that could not be a date at all is the evidence against. Decided
     * once per query off the sample rather than per cell, so a date column
     * costs no regex in the filter loop.
     * @param {number} i
     */
    function dateColumn(i) {
      return sampledShape(i, (s) => DATEISH.test(s), (s) => !COULD_BE_DATE.test(s));
    }

    /**
     * Which columns hold dates, for the one predicate that reads them all at
     * once. Sampled by `dateColumn' rather than named, which is the same
     * asymmetry the prefix rule already has: a producer knows which of its
     * columns are dates and this decides it off the cells, so a page carrying
     * fewer than two dated rows finds no date column and `planned' narrows
     * where a producer's own would not (SCHEMA, Filter query).
     *
     * Cached like `multiColumn''s verdict and thrown away with it: each column
     * costs a sample of up to forty cells, and this asks every column.
     * @returns {number[]}
     */
    function dateColumns() {
      if (dateAt !== undefined) return dateAt;
      dateAt = [];
      for (let i = 0; i < columns().length; i++) if (dateColumn(i)) dateAt.push(i);
      return dateAt;
    }
    /** @type {number[]|undefined} */
    let dateAt;

    /**
     * TOK as a row test, negation aside — `queryMatcher' applies that. Free
     * text is a substring of the whole row, bar and all: alternation is a
     * PREDICATE's rule. A predicate's value splits into its alternatives and
     * the row passes on ANY of them, each read as that key's own single value
     * (`valueTest'); with no alternative left there is nothing to narrow by.
     *
     * The alternatives' tests are built here, once per query, rather than per
     * row — the same reason the whole matcher is compiled ahead of the walk.
     * @param {Token} tok  @returns {(r: Row) => boolean}
     */
    function tokenTest(tok) {
      // A keyless token and `substring:' are ONE test, which is what makes the
      // key an elision rather than a second search.  The cached joined string
      // is the hot path, and the reason a free-text query costs what it did.
      if (tok.key === null) return freeTest(tok.value.toLowerCase());
      const key = tok.key;
      const alts = alternatives(tok.value.toLowerCase());
      if (!alts.length) return () => true;         // half-typed: narrows nothing
      if (alts.length === 1) return valueTest(key, alts[0]);
      const tests = alts.map((v) => valueTest(key, v));
      return (r) => {
        for (const t of tests) if (t(r)) return true;
        return false;
      };
    }

    /** V as a substring of the row as it displays; an empty V narrows nothing. */
    function freeTest(v) {
      return v ? (r) => rowText(r).search.includes(v) : () => true;
    }

    /**
     * The cells KEY names, by index — a column's own, or every date column for
     * `planned', the one key `queryKeys' names that is not a column. Null is
     * "no such key", which is a different answer from "no cells": `planned'
     * over a page carrying no date column names NOTHING, and a predicate over
     * nothing finds nothing, where an unknown key narrows nothing at all.
     *
     * A column of that name shadows the reserved key, so a producer shipping a
     * `planned' column of its own reads it as the column it is.
     * @param {string} key  @returns {number[]|null}
     */
    function fieldCells(key) {
      const col = colByKey(key);
      if (col) return [columns().indexOf(col)];
      return key === PLANNED_KEY ? dateColumns() : null;
    }

    /**
     * `KEY:V' as a row test for ONE alternative. V is lowercased and non-empty
     * — `tokenTest' dropped the empty alternatives before this ran.
     *
     * ONE reading over the cells the key names: `*empty*' asks that they ALL be
     * empty and any other value asks that ANY of them pass, each by its own
     * column's semantics (`cellTest'). A key naming one cell is that rule with
     * one cell in it, so `planned' is the same arm over two indices — which is
     * why a cell that prefix-matches is a cell with something in it and the
     * presence test is never spelled twice.
     * @param {string} key  @param {string} v  @returns {(r: Row) => boolean}
     */
    function valueTest(key, v) {
      // `substring:' is free text under a key, so it is that matcher.  Asked
      // before `fieldCells', which knows about cells and this reads the row.
      if (key === SUBSTRING_KEY && !colByKey(key)) return freeTest(v);
      const cells = fieldCells(key);
      if (!cells) return () => true;             // no such key: narrows nothing
      // Asking for an empty cell is what `*empty*' is for, on every key: it is
      // the uniform meta, so it is answered before any column's own reading and
      // before a producer's. The bare word `none' this was once spelled as is
      // ordinary text now, and a cell reading `none' is found by `key:none'.
      if (v === EMPTY_META) return (r) => cells.every((i) => rowText(r).cells[i] === "");
      const tests = cells.map((i) => cellTest(i, v));
      if (tests.length === 1) return tests[0];
      return (r) => {
        for (const t of tests) if (t(r)) return true;
        return false;
      };
    }

    /**
     * V as a test of cell I alone, by SCHEMA's semantics for that column's
     * type. @param {number} i  @param {string} v  @returns {(r: Row) => boolean}
     */
    function cellTest(i, v) {
      const col = columns()[i];
      // A starred word on a MULTI-valued column is that WHOLE entry, where the
      // bare word is a substring of the delimited cell: `tag:*book*' is the tag
      // `book' and `tag:boo' is any tag holding those letters. Decidable here —
      // the delimiter is in the cell — so a producer and this renderer answer
      // it identically, which is what makes it a meta both sides carry rather
      // than one the producer resolves alone. A date column is never the
      // multi-valued one, the two shapes excluding each other, so this cannot
      // fire for a cell `planned' named.
      if (i === multiColumn() && META.test(v)) {
        const want = starless(v);
        return (r) => tagsIn(rowText(r).cells[i]).indexOf(want) !== -1;
      }
      // A producer meta names a set only the producer can enumerate, so it is
      // matched literally here and finds nothing, and a view that declares
      // metas is expected to filter through `onFilter'. `*active*' has one term
      // that names no keyword and so survives the crossing: SCHEMA puts the
      // EMPTY cell in the active group — an unstated row is live work — and an
      // empty cell needs no keyword set to recognise. That half is what is
      // answered here; the keyword half is what drops out, leaving an answer
      // the producer's own can only widen. `*inactive*' has no such term, an
      // empty cell not being done, and stays the literal it was.
      if (col && col.type === "badge") {
        if (v === ACTIVE_META) return (r) => rowText(r).cells[i] === "";
        // A whole-value match reads through org's priority decoration, both
        // ways: the cell is drawn `[#A]' and `A' is what a reader means, so
        // `priority:A' and `priority:[#A]' are one query here and at the
        // producer. BOTH SPELLINGS OF THE VALUE are worked out once, so the
        // fold is a second string compare in the row loop rather than a regex.
        const want = undecorated(v), worn = `[#${want}]`;
        return (r) => { const c = rowText(r).cells[i]; return c === want || c === worn; };
      }
      if (dateColumn(i)) return (r) => rowText(r).cells[i].startsWith(v);
      return (r) => rowText(r).cells[i].includes(v);
    }

    /**
     * Q compiled to a row test, or null when it filters nothing. Built once per
     * filter change and reused for every row.
     *
     * SCHEMA's shape is ONE rule: TOKENS AND, ALTERNATIVES OR. Every token
     * narrows, whether or not another names its key, so there is no grouping
     * and no arity here — each token is its own test, negated or not, and a row
     * has to pass all of them. `state:TODO state:DONE' is therefore a row in
     * both states, which is none; a row in either is the one token
     * `state:TODO|DONE', and the OR lives inside `tokenTest'.
     *
     * A `sort' token is the exception, and it is one because it is no predicate:
     * it states the ORDER (`chainFor') and contributes no test at all, in either
     * polarity — negating an ordering names no rows, so `-sort:x' narrows
     * nothing here rather than emptying the table.
     * @param {string} q  @returns {((r: Row) => boolean)|null}
     */
    function queryMatcher(q) {
      /** @type {((r: Row) => boolean)[]} */
      const musts = [];
      for (const tok of parseQuery(q, queryKeys())) {
        if (tok.key && VIEW_KEYS.indexOf(tok.key) !== -1) continue;
        const test = tokenTest(tok);
        musts.push(tok.negated ? (r) => !test(r) : test);
      }
      if (!musts.length) return null;
      if (musts.length === 1) return musts[0];
      return (r) => {
        for (const t of musts) if (!t(r)) return false;
        return true;
      };
    }

    /** The rows to display: sorted, then filtered. Cached. @returns {Row[]} */
    function ordered() {
      if (order) return order;
      if (!sorted) {
        orderCmp = chainComparator();
        sorted = state.rows.slice();     // never sort the store itself
        if (orderCmp) sorted.sort(orderCmp);
      }
      orderTest = queryMatcher(state.filter);
      order = orderTest ? sorted.filter(orderTest) : sorted.slice();
      widths = null;
      return order;
    }

    /** How many pages the filtered set makes, never fewer than one. */
    function pageCount() {
      return pageSize ? Math.max(1, Math.ceil(ordered().length / pageSize)) : 1;
    }

    /**
     * The rows on show: one page of the filtered, sorted set, or all of it
     * where no page size was asked for. Everything that renders rows reads
     * this — the window, the spacers, the scroll arithmetic and `getVisible'
     * — so the virtualizer works inside the page and knows nothing about
     * paging. Widths are the exception: they measure the whole filtered set,
     * or columns would jump width every time the page turned.
     */
    function paged() {
      const rows = ordered();
      if (!pageSize || continuous) return rows;
      if (page >= pageCount()) page = pageCount() - 1;   // the set shrank under it
      const at = page * pageSize;
      return rows.slice(at, at + pageSize);
    }

    /**
     * The page the cursor sits in, from zero — the page a reader would say
     * they were on. In PAGED presentation that is `page' by construction; in
     * CONTINUOUS it is derived from where the selection landed, which is what
     * makes the pager move as the cursor crosses a boundary. With nothing
     * selected it falls back to `page', so an unselected continuous view
     * still says something rather than nothing.
     */
    function cursorPage() {
      if (!pageSize) return 0;
      if (!continuous) return Math.min(page, pageCount() - 1);
      const i = state.selected === null
        ? -1 : ordered().findIndex((r) => r.id === state.selected);
      return i === -1 ? Math.min(page, pageCount() - 1) : Math.floor(i / pageSize);
    }

    /**
     * Go continuous, keeping the viewport exactly where it is. The rows the
     * window indexes stop being the page and become the whole set, so a row
     * that was at index i is now at `page * pageSize + i' — the scroller is
     * moved by that difference in the same breath, which is what makes the
     * switch invisible. The selection's own index is re-read from the new
     * rows for the same reason.
     */
    /**
     * The rows a reader would call "on show": one page of the filtered set, or
     * all of it with no page size. In CONTINUOUS presentation the window is
     * over the whole set, but this stays the CURSOR's page — the same answer
     * the pager gives and `getVisible' returns, so "shown" means one thing
     * across the handle whichever way the rows were drawn.
     * @returns {Row[]}
     */
    function shownRows() {
      const rows = ordered();
      if (!pageSize || !continuous) return paged();
      const at = cursorPage() * pageSize;
      return rows.slice(at, at + pageSize);
    }

    function goContinuous() {
      if (!pageSize || continuous) return;
      const skipped = page * pageSize;
      continuous = true;
      scroll.scrollTop += skipped * geom.row;
      if (selAt >= 0) selAt += skipped;
    }

    /** Whether ROW passes the current filter. @param {Row} r */
    function matches(r) { return !orderTest || orderTest(r); }

    /**
     * Column widths in characters. Under the FILL POLICY the CELLS decide and a
     * header widens nothing: a column of `[#A]' badges reads exactly as wide as
     * `[#A]', and a header too long for that ellipsizes into it rather than
     * pushing it open — which is the whole of what makes a badge column read
     * tight. A column holding no cell at all has no content measure, so there
     * the header is the only measure there is. Without a `title' column to fill,
     * the header is content like any other and the width is what it always was:
     * the widest cell, or the header and its mark.
     * @returns {number[]}
     */
    function colWidths() {
      if (widths) return widths;
      const cols = columns(), chain = sortChain(), fill = titleColumn() !== -1;
      /** The widest CELL each column holds, in characters; 0 where it holds none. */
      const cell = cols.map(() => 0);
      for (const r of ordered()) {
        const len = rowText(r).len;
        for (let i = 0; i < cell.length; i++) if (len[i] > cell[i]) cell[i] = len[i];
      }
      widths = cols.map((c, i) => {
        const at = chain.findIndex(({ key }) => key.column === c.key);
        // Every mark a header wears is paid for here, or the column it widens
        // clips the word underneath it — the mark itself plus the space in front
        // of it, measured off the very text `renderArrows' draws. Under the fill
        // policy it is paid for OUTSIDE the cells' measure, and the header's own
        // box is what shrinks, so a squeezed header loses its word and keeps its
        // mark (`.tv-hn' flexes, `.tv-arrow' declines to).
        const mark = at === -1 ? 0 : sortMark(chain, at).length + 1;
        const head = String(c.header || c.key).length;
        // A badge cell draws a pill around its text, whose padding the cached
        // length knows nothing about. A tag cell needs no allowance: `:a:b:' and
        // `a · b' are the same length, `:a:' is longer than `a', and the smaller
        // type shrinks it further — the rendering never outgrows the raw text
        // the widths were measured from.
        const pill = c.type === "badge" ? PILL_CH : 0;
        return fill ? (cell[i] ? cell[i] + pill : head) + mark
                    : Math.max(head + mark, cell[i]) + pill;
      });
      return widths;
    }

    /** Widen the cached widths for ROW (an upsert can only add text). */
    function growWidths(r) {
      if (!widths) return;
      const len = rowText(r).len, cols = columns();
      for (let i = 0; i < widths.length; i++) {
        // The pill the measured pass allows for, allowed for again: a cell that
        // arrives longer than every cell that was measured brings its ground
        // with it. The mark rides outside the cells' measure and is not owed.
        const n = len[i] + (len[i] && cols[i].type === "badge" ? PILL_CH : 0);
        if (n > widths[i]) widths[i] = n;
      }
    }

    /**
     * Write the measured widths onto the columns under the FILL POLICY: the
     * `title' column takes every pixel the others leave and each other column is
     * exactly as wide as its own widest cell, capped at `COL_MAX_CH'. The title
     * gets no width at all — under the fixed layout the class turns on, the one
     * column without one absorbs the remainder — so what is written here is the
     * OTHERS, and the title's share is arithmetic the browser does on a resize
     * for nothing. Only the table's `min-width' knows the title exists: it is
     * the sized columns plus the title's floor, which is where a narrow window
     * starts scrolling sideways instead of crushing the title.
     *
     * `ch' is exact in the monospace face the renderer sets, so every number
     * here is resolution-independent and no measurement of the container is
     * taken. A view carrying no `title' column keeps the widths as hints under
     * the auto layout it always had.
     */
    function applyWidths() {
      const w = colWidths(), at = titleColumn(), fill = at !== -1;
      if (table.classList.contains("tv-fill") !== fill)
        table.classList.toggle("tv-fill", fill);
      // The floor and the gutter, in characters and in cells of padding. The
      // gutter is counted at its fine-pointer measure; the coarse block's 44px
      // floor is inert at this face and a few pixels short of the sum at a much
      // smaller one, which moves only where the sideways scroll begins.
      let ch = fill ? Math.min(w[at], TITLE_MIN_CH) : 0;
      let cells = fill ? 1 : 0;
      if (fill && chrome) { ch += BOX_CH; cells++; }
      for (let i = 0; i < colEls.length; i++) {
        const n = fill ? Math.min(w[i], COL_MAX_CH) : w[i];
        const px = fill && i === at ? "" : `calc(${n}ch + ${CELL_PAD}px)`;
        if (fill && i !== at) { ch += n; cells++; }
        if (colEls[i].style.width !== px) colEls[i].style.width = px;
      }
      const min = fill ? `calc(${ch}ch + ${cells * CELL_PAD}px)` : "";
      if (table.style.minWidth !== min) table.style.minWidth = min;
    }

    // ---- rendering ---------------------------------------------------------

    /** Rebuild the colgroup and the header row (mount, and a view change). */
    function renderHead() {
      colgroup.innerHTML = "";
      headRow.innerHTML = "";
      colEls = [];
      arrowEls = [];
      // The gutter leads and is nobody's column: it is left out of `colEls'
      // and `arrowEls', which stay one entry per column the view declared, so
      // widths and sort arrows keep indexing what they always did.
      if (chrome) {
        const gut = document.createElement("col");
        gut.className = "tv-gut";   // pinned to the glyph's measure by the sheet
        colgroup.appendChild(gut);
        const box = document.createElement("th");
        box.className = "tv-box";      // blank: the count is the hint line's
        headRow.appendChild(box);
      }
      for (const c of columns()) {
        const col = document.createElement("col");
        colgroup.appendChild(col);
        colEls.push(col);

        const th = document.createElement("th");
        th.className = (c.sortable === true ? "tv-sortable" : "")
          + (c.align === "right" ? " tv-right" : "");
        th.dataset.key = c.key;
        // The word and its mark are two boxes, so a header wider than the cells
        // under it loses the WORD and keeps the mark: under the fill policy the
        // cells set the width and this pair is a flex row, the word shrinking to
        // an ellipsis and the mark declining to shrink at all. Without a fill
        // column the pair is inert — the header is paid for in the width there,
        // so nothing is ever squeezed — and the text a reader copies out of the
        // header is what it always was.
        const hd = document.createElement("span");
        hd.className = "tv-hd";
        const label = document.createElement("span");
        label.className = "tv-hn";
        label.textContent = String(c.header || c.key);
        const arrow = document.createElement("span");
        arrow.className = "tv-arrow";
        hd.appendChild(label);
        hd.appendChild(arrow);
        th.appendChild(hd);
        headRow.appendChild(th);
        arrowEls.push(arrow);
      }
      renderArrows();
    }

    /**
     * The mark CHAIN's key at AT wears: its direction, and its place in the
     * chain where there is more than one key to order. Read twice — the header
     * draws it and `colWidths' pays for it — so what is measured is the text
     * that is drawn, however many digits the ordinal runs to.
     * @param {{key: SortKey, header: string}[]} chain  @param {number} at
     */
    const sortMark = (chain, at) =>
      (chain[at].key.ascending ? "▲" : "▼")
        + (chain.length > 1 ? superscript(at + 1) : "");

    /**
     * Mark every sorted column's header with the direction it is sorted in and,
     * where the chain has more than one key, its place in the chain. The whole
     * order is therefore readable over the columns it is about — the leading key
     * in full ink, the tie-breakers muted, and no ordinal at all where there is
     * nothing to order.
     */
    function renderArrows() {
      const chain = sortChain(), cols = columns();
      for (let i = 0; i < arrowEls.length; i++) {
        const at = chain.findIndex(({ key }) => key.column === cols[i].key);
        const mark = at === -1 ? "" : sortMark(chain, at);
        arrowEls[i].innerHTML = mark.length < 2 ? mark
          : mark[0] + `<i class="tv-ord">${mark.slice(1)}</i>`;
        arrowEls[i].className = "tv-arrow" + (at === 0 ? " tv-lead" : "");
        arrowEls[i].style.display = at === -1 ? "none" : "";  // no empty arrow's margin
      }
    }


    // Every class a row or a cell wears is DERIVED, in one place, as
    // [name, on] pairs. Two things write them — `rowHTML' builds a window from
    // scratch and `stampSelection' re-stamps the window already in the DOM —
    // and they used to spell the derivation twice, so a state could be drawn
    // one way when the row was built and another when it was re-stamped. The
    // pairs feed both: the builder joins the names that are on, the stamper
    // toggles each pair to its value, which is a no-op wherever the element
    // already agrees.

    /**
     * Which cell a producer's `linked' marks: the `title' column's, that being
     * the text a reader reads the row by. A view without that column carries no
     * mark — the flag says the row leads somewhere and there is no other cell
     * it would be true of. -1 for a row that leads nowhere.
     * @param {Row} r
     */
    function linkedCell(r) { return r.linked ? titleColumn() : -1; }

    /**
     * The classes row R wears at display index I. Zebra striping is index-borne,
     * since `:nth-child' sees only the window.
     * @param {Row} r  @param {number} i  @returns {[string, boolean][]}
     */
    function rowClasses(r, i) {
      return [["tv-alt", i % 2 === 1],
              ["tv-marked", markSet.shows(r.id)],
              ["tv-flagged", flagSet.shows(r.id)],
              ["tv-sel", r.id === state.selected]];
    }

    /**
     * The classes column C's cell of row R wears. LINKEDAT is `linkedCell(r)',
     * passed in because both callers already have it for the whole row.
     * @param {Row} r  @param {number} c  @param {number} linkedAt
     * @returns {[string, boolean][]}
     */
    function cellClasses(r, c, linkedAt) {
      const col = columns()[c], inCol = c === state.selCol;
      return [["tv-right", !!col && col.align === "right"],
              ["tv-colsel", inCol],
              ["tv-cell-sel", inCol && r.id === state.selected],
              ["tv-linked", c === linkedAt]];
    }

    /** The names that are on, as a class attribute. @param {[string, boolean][]} pairs */
    function classAttr(pairs) {
      let out = "";
      for (let k = 0; k < pairs.length; k++)
        if (pairs[k][1]) out += out ? " " + pairs[k][0] : pairs[k][0];
      return out;
    }

    /**
     * Put PAIRS on EL, writing only the ones that MOVED: the list is asked
     * what it holds before it is asked to change it, so re-deriving every class
     * of a window a held movement key never altered writes nothing at all.
     * @param {Element} el  @param {[string, boolean][]} pairs
     */
    function stampClasses(el, pairs) {
      const cl = el.classList;
      for (let k = 0; k < pairs.length; k++)
        if (cl.contains(pairs[k][0]) !== pairs[k][1]) cl.toggle(pairs[k][0], pairs[k][1]);
    }

    /**
     * A row's <tr>. I is its index in the display order.
     * @param {Row} r  @param {number} i  @returns {string}
     */
    function rowHTML(r, i) {
      const cols = columns(), cs = r.cells || {};
      const multi = multiColumn();
      const linkedAt = linkedCell(r);
      let tds = chrome ? `<td class="tv-box"></td>` : "";
      for (let c = 0; c < cols.length; c++)
        tds += `<td class="${classAttr(cellClasses(r, c, linkedAt))}">`
             + `${cellHTML(cols[c], cs[cols[c].key], dark, c === multi)}</td>`;
      return `<tr class="${classAttr(rowClasses(r, i))}" data-id="${esc(r.id)}">${tds}</tr>`;
    }

    /** A spacer row H pixels tall, standing in for the rows outside the window. */
    function padHTML(h) {
      return `<tr class="tv-pad" style="height:${h}px">`
           + `<td colspan="${columns().length + chrome}"></td></tr>`;
    }

    /**
     * Render the window of rows around the scroll position, with the hint,
     * widths and empty state that go with it. FORCE redraws even when the
     * window has not moved (the rows themselves changed).
     * @param {boolean} [force]
     */
    function renderRows(force) {
      if (composer) return;   // no table behind the bar: nothing to paint
      keepSelection();
      const rows = paged();
      const total = rows.length;
      const rowH = geom.row;
      const port = scroll.clientHeight || rowH * 20;   // before layout: a screenful
      // Row I sits at geom.head + I * rowH in the scroller; the overscan covers
      // the rounding and the band the sticky header hides.
      const top = Math.max(0, (scroll.scrollTop || 0) - geom.head);
      const first = Math.max(0, Math.floor(top / rowH) - OVERSCAN);
      const last = Math.min(total, first + Math.ceil(port / rowH) + OVERSCAN * 2);
      if (!force && first === win.first && last === win.last) return;
      win.first = first;
      win.last = last;
      win.rows = rows;
      let html = first > 0 ? padHTML(first * rowH) : "";
      for (let i = first; i < last; i++) html += rowHTML(rows[i], i);
      if (last < total) html += padHTML((total - last) * rowH);
      tbody.innerHTML = html;

      applyWidths();
      table.style.display = total ? "" : "none";
      empty.style.display = total ? "none" : "";
      renderHint();
      measure();
    }

    /** The status line, off the state it reads; clears whoever asked for it. */
    function renderHint() {
      wantHint = false;
      hint.innerHTML = hintHTML(ordered().length);
    }

    /** Re-read the row and header heights the spacers are sized from. */
    function measure() {
      const tr = /** @type {HTMLElement|null} */ (tbody.querySelector("tr[data-id]"));
      if (!tr || typeof tr.getBoundingClientRect !== "function") return;
      const head = thead.getBoundingClientRect().height;
      if (head > 0) geom.head = head;
      const h = tr.getBoundingClientRect().height;
      if (h > 0 && Math.abs(h - geom.row) > 0.5 && !remeasuring) {
        geom.row = h;                      // the spacers are wrong; redraw once
        remeasuring = true;
        renderRows(true);
        remeasuring = false;
      }
    }

    /**
     * The furthest this scroller can travel with PORT pixels on show.
     *
     * The scroller's OWN content is the answer wherever it is an answer about
     * the rows on show, because a row's box is fractional and every rect is
     * snapped: `geom.row' is a ROUNDING of the row height rather than the
     * height, so `geom.head + rows * geom.row' runs a fraction of a pixel short
     * PER ROW — over a page of a hundred, twenty, which is the tail parking
     * under the hint bar with its last row two thirds covered. `scrollHeight'
     * is that sum without the rounding, and it is the number the browser itself
     * clamps `scrollTop' against.
     *
     * It answers for the rows in the TBODY, which at a page turn and at the
     * continuous seam are not yet the rows on show — the set changes and the
     * scroller is still holding the one before it. Every row being one row tall,
     * the count is what tells the two apart, and where they differ the modelled
     * sum is all there is until the render lands.
     */
    function maxScroll(port) {
      const rows = paged();
      const content = win.rows.length === rows.length
        ? scroll.scrollHeight : geom.head + rows.length * geom.row;
      return Math.max(0, content - port);
    }

    /**
     * Where the current page sits in the filtered set: 1-based `page' of
     * `pages', rows `from'..`to' of `total'.  The hint line and the handle
     * both read it, so the range on screen and the range a consumer is told
     * are one calculation.
     * @returns {{page: number, pages: number, from: number, to: number, total: number}}
     */
    function pageInfo() {
      const total = ordered().length;
      const pages = pageCount();
      const at = cursorPage();
      return pageSize
        ? { page: at + 1, pages, from: total ? at * pageSize + 1 : 0,
            to: Math.min(total, (at + 1) * pageSize), total }
        : { page: 1, pages: 1, from: total ? 1 : 0, to: total, total };
    }

    /** N with thousands grouped, written the same wherever the page runs. */
    function grouped(n) {
      return String(n).replace(/\B(?=(\d{3})+(?!\d))/g, ",");
    }

    /**
     * Where in the filtered set this page sits, and the way to either side.
     * The range is `pageInfo''s, so the line a reader sees and the numbers a
     * consumer reads back are one calculation rather than two that agree.
     */
    function pagerHTML() {
      const p = pageInfo();
      // One row is a range of itself, and says so once rather than twice.
      const span = p.from === p.to
        ? grouped(p.from) : `${grouped(p.from)}–${grouped(p.to)}`;
      const step = (dir, label, can) =>
        `<b class="tv-pg${can ? "" : " tv-pg-off"}" data-pg="${dir}">${label}</b>`;
      return `${esc(span)} of ${esc(grouped(p.total))}`
           + ` · ${step(-1, "‹ prev", p.page > 1)}`
           + ` · ${step(1, "next ›", p.page < p.pages)}`;
    }

    /**
     * The status line: what is on show, how it is sorted, and the actions as
     * `KEY label' pairs — the way table-view.el prints its legend. The keys are
     * the interface; a consumer binds them and dispatches the command, and the
     * renderer offers no button to press instead.
     */
    function hintHTML(shown) {
      const total = state.rows.length;
      const count = shown === total ? `${total} rows` : `${shown}/${total} rows`;
      const chain = sortText();
      const sort = chain ? `sort ${chain}` : "unsorted";
      // With one page there is nothing to page through, and the line is the
      // line it has always been. With more, the range says what the count
      // said and where in the set it is, so it stands in that place rather
      // than beside it.
      let out = pageCount() > 1 ? pagerHTML() : `${esc(count)}`;
      out += ` · ${esc(sort)}`;
      // The legend is the renderer's way of saying which keys a consumer bound;
      // a page that prints its own keymap has said it already, and two legends
      // disagreeing is worse than one. Presentation alone — the actions still
      // dispatch, and nothing about the view changes.
      if (actionHints)
        for (const a of actions()) {
          if (!a.key) continue;
          out += ` · <b class="tv-key">${esc(a.key)}</b> ${esc(a.label || a.command)}`;
        }
      // A standing choice leads the line, ahead of what is merely on show: the
      // count is of every mark, the ones a filter or a page is hiding included,
      // which is the number a bulk action would run over. Nothing marked and the
      // line is the line it has always been.
      if (marks && markSet.ids.size)
        out = `${esc(grouped(markSet.ids.size))} marked · ${out}`;
      if (flags && flagSet.ids.size) {
        // With the cursor ON a flagged row the segment turns into a reminder
        // of what can be done about it. The text is the CONSUMER's whole
        // string — the keys are theirs to bind and theirs to name, and a
        // renderer inventing `d' or `u' here would be asserting a keymap it
        // does not own. Rendered in the legend's own shape: key tokens small
        // and the words between them plain.
        const help = flagHelp && state.selected !== null && flagSet.ids.has(state.selected)
          ? ` · ${flagHelpHTML}` : "";
        out = `${esc(grouped(flagSet.ids.size))} flagged${help} · ${out}`;
      }
      return out;
    }

    /**
     * COL as a real column index, or null for a whole-row selection — which is
     * what a column outside the table is. A consumer steps the selection by
     * reading the column and handing back one more, so the index past an end is
     * a reader walking off the cells, and the answer there is the row they are
     * still on: an edge that swallows the key says nothing, and the row-only
     * selection is a look the table already draws.
     * @param {number|null|undefined} col  @returns {number|null}
     */
    function cellCol(col) {
      if (col === null || col === undefined) return null;
      const at = Math.trunc(col);
      return at >= 0 && at < columns().length ? at : null;
    }

    /**
     * Move the selection to row ID, and within it to column COL — or to no
     * column at all, which is the whole-row look this had before cells were
     * selectable. Stamps the window in place; the classes are re-derived from
     * the same state on every render, so a scroll, an upsert or a `setRows'
     * puts them back on whatever row still carries the id.
     * @param {string|null} id  @param {number|null} [col]
     */
    function setSelected(id, col) {
      state.selected = id ?? null;
      state.selCol = id === null || id === undefined ? null : cellCol(col);
      selAt = indexOfSelected();
      stampSelection();
    }

    /**
     * Keep the selection where it was on screen when the row it was on stops
     * being there — filtered away, deleted, replaced by a new page. The id is
     * gone, but the place is not, so the selection stays at that visual index
     * (clamped to what is left) rather than disappearing and making the next
     * keypress start over from the top.
     */
    function keepSelection() {
      if (state.selected === null) return;
      const rows = paged();
      if (!rows.length) { state.selected = null; state.selCol = null; selAt = -1; return; }
      if (selAt >= 0 && rows[selAt] && rows[selAt].id === state.selected) return;
      if (rows.some((r) => r.id === state.selected)) return;
      selAt = Math.max(0, Math.min(rows.length - 1, selAt));
      state.selected = rows[selAt].id;
    }

    /** Where the selected row sits in display order, or -1. */
    function indexOfSelected() {
      if (state.selected === null) return -1;
      const rows = paged();
      // The cached index is right unless the order moved under it.
      if (selAt >= 0 && rows[selAt] && rows[selAt].id === state.selected) return selAt;
      return rows.findIndex((r) => r.id === state.selected);
    }

    /**
     * The row and column state the window wears, re-derived from the state
     * rather than rebuilt — which is what leaves the grounds something to
     * crossfade between. `rowClasses' and `cellClasses' answer WHAT the classes
     * are, the same pairs `rowHTML' builds the window from, and this walks the
     * window toggling each to its value: a no-op wherever the element already
     * agrees, so a held movement key rewrites nothing and the stripe, the
     * alignment and the link mark cost their comparison and no DOM write.
     *
     * One pass for all of them, since a toggle and a step arrive in the same
     * frame. Only the window is stamped, the header apart, which is all there
     * is to stamp — the rows outside it have no elements. The row and the index
     * behind a `tr' come out of `win', which holds the order the tbody was
     * DRAWN from: asking `paged()' again would be asking the state a second
     * time and getting an answer the DOM had never been told about.
     */
    function stampSelection() {
      const trs = tbody.children;
      let k = 0;
      for (let i = 0; i < trs.length; i++) {
        const tr = /** @type {HTMLElement} */ (trs[i]);
        if (tr.dataset.id === undefined) continue;      // a spacer, not a row
        const at = win.first + k++, r = win.rows[at];
        stampClasses(tr, rowClasses(r, at));
        // The chrome cell is nobody's column, so the column a cell selection
        // names is counted past it.
        const linkedAt = linkedCell(r), tds = tr.children;
        for (let c = chrome; c < tds.length; c++)
          stampClasses(tds[c], cellClasses(r, c - chrome, linkedAt));
      }
      // A column highlight that stopped at the header would read as broken, and
      // the header is not rebuilt per window, so it is stamped here rather than
      // in `rowHTML'. A whole-row selection has no column and clears both.
      const ths = headRow.children;
      for (let c = chrome; c < ths.length; c++)
        ths[c].classList.toggle("tv-colsel", c - chrome === state.selCol);
    }


    // ---- row states --------------------------------------------------------
    // Dired's marks, and dired's flags beside them: a set of ids that owes the
    // rows nothing. A row can be re-sent, re-sorted, filtered away or paged past
    // and its mark is still the same entry in the same set — which is the whole
    // reason either state is keyed by `id' and not by a row object or an index.
    //
    // The two are ONE mechanism instantiated twice, so a question answered for
    // marks is answered the same way for flags: what a toggle returns, what
    // `getMarked' and `getFlagged' order by, what a clear leaves standing, what
    // survives a re-derivation of the rows. They stay two SETS because they are
    // two questions — a flag is a PENDING action a consumer is about to confirm
    // where a mark is a standing selection — so a row can carry both and
    // neither clear touches the other.

    /**
     * A set of row ids the table draws a state for. DRAWN says whether this
     * table draws it at all: it gates the class, the chrome and the count
     * together, so without the option there is nothing to hide rather than
     * something hidden. The set itself fills either way, which is what lets a
     * consumer read the ids back off a table that paints none of them.
     * @param {boolean} drawn  @returns {RowState}
     */
    function rowState(drawn) {
      /** @type {Set<string>} */
      const ids = new Set();
      return {
        ids,
        /** Does ID wear this state, as the table draws it? @param {string} id */
        shows(id) { return drawn && ids.has(id); },
        /** Put the state on ID or take it off.
         *  @param {string} id  @returns {boolean} the state it landed in */
        toggle(id) {
          const on = !ids.has(id);
          if (on) ids.add(id); else ids.delete(id);
          paintMarks();
          return on;
        },
        /** Take it off ID, whether or not it was there. @param {string} id */
        drop(id) { if (ids.delete(id)) paintMarks(); },
        /**
         * Put it on every row of ROWS. Idempotent: a row already carrying it
         * stays, so running this twice is running it once.
         * @param {Row[]} rows  @returns {number} how many rows carry it after
         */
        addAll(rows) {
          const before = ids.size;
          for (const r of rows) ids.add(r.id);
          if (ids.size !== before) paintMarks();
          return ids.size;
        },
        /** Take it off every row. The other state is a different question and
         *  is left standing. */
        clear() {
          if (!ids.size) return;
          ids.clear();
          paintMarks();
        },
        /**
         * The ids, in the order a reader would read them off: the ones on show,
         * in display order, then the ones a filter or another page is hiding,
         * in the order they were laid down. Stable either way — a bulk action
         * over this runs in the same order twice.
         * @returns {string[]}
         */
        list() {
          const out = shownRows().filter((r) => ids.has(r.id)).map((r) => r.id);
          const shown = new Set(out);
          for (const id of ids) if (!shown.has(id)) out.push(id);
          return out;
        },
      };
    }

    const markSet = rowState(marks);
    const flagSet = rowState(flags);

    /**
     * Mark every row of the CURRENT FILTERED SET — all of it, not the page on
     * show, since a filter is what a reader narrowed to and the page is only
     * how much of it fits. With no filter that is every row. Idempotent: a row
     * already marked stays marked, so running it twice is running it once.
     *
     * `addAll' is the MECHANISM's and both states hold it; this is where the
     * HANDLE offers it, on marks alone. `unflagRow' is `drop', offered on flags
     * alone for the same reason: taking a whole set down at once is what a
     * STANDING selection is for, and a pending action is spent on the rows it
     * was pending over one at a time. Completing either pair is one line —
     * neither is asked for, so the handle offers what the two states are used
     * for rather than a symmetric surface nobody calls.
     * @returns {number} how many rows carry a mark afterwards
     */
    function markAll() {
      // No mark column, nothing to mark.
      return marks ? markSet.addAll(ordered()) : 0;
    }

    /**
     * Repaint what a mark moved: the grounds, the boxes and the count, all
     * through the frame the selection already uses. The count needs asking for
     * separately because `renderRows' turns back at the door when the window
     * has not moved, which is the common case for a toggle — and a held `m'
     * would otherwise rewrite the status line thirty times a second.
     */
    function paintMarks() {
      wantSelection = true;
      wantHint = true;
      schedule();
    }

    /**
     * Select the row with ID, scrolling its place in the (virtual) list into
     * view. Rows outside the rendered window have no element to click, so this
     * is how a consumer moves the selection. False when no visible row has that
     * id — a filtered-out row does not steal the selection.
     *
     * COL selects one cell of that row; a COL outside the columns that exist
     * selects none of them, so a consumer stepping past either end lands on the
     * whole-row selection. Omitted, it is that same selection, which is what it
     * always was.
     *
     * This scrolls, keeping a margin under the cursor. A click does not: the
     * row is under the pointer already, and yanking the viewport out from
     * under a hand that just aimed at something is the one thing it must not
     * do — so the delegated handler sets the selection through `setSelected'
     * instead, which moves the marks and nothing else.
     * @param {string} id  @param {number} [col]  @returns {boolean}
     */
    function selectRow(id, col) {
      const rows = paged();
      const i = rows.findIndex((r) => r.id === id);
      if (i === -1) return false;
      const was = selAt;
      state.selected = id;
      state.selCol = cellCol(col);
      selAt = i;
      paintSelection(was);
      return true;
    }

    /**
     * Repaint the selection on the next frame, once however many times it moved
     * in between. A consumer holding a movement key fires ~30 of these a
     * second; each one is a scroll adjustment and a window rewrite, and doing
     * them per event is what makes held-key movement stutter. The state is
     * already correct — `getSelection' answers from it synchronously — so the
     * frame only has to paint where the selection ended up.
     */
    function paintSelection(was) {
      wantSelection = true;
      // Nothing on the hint line depends on WHICH row is selected — unless a
      // flag helper does, and then moving the cursor on or off a flagged row
      // changes it. Asked for only when there is one, so the common case still
      // never rewrites the status line to move a cursor; and it is one rewrite
      // per frame either way, since this is the coalescing path.
      if (flagHelp) wantHint = true;
      if (selAt >= 0) easeToRow(selAt, was === undefined ? selAt : was);
      schedule();
    }

    // ---- the frame loop ----------------------------------------------------
    // One callback drives everything that wants a frame: the window the scroll
    // position implies, the selection's marks, and the viewport ease. Two
    // schedulers racing each other would re-render the same tbody twice a
    // frame and read `scrollTop' while the other was writing it.

    let frameId = 0;
    let wantWindow = false;      // the scroll moved; re-window if it has to
    let wantSelection = false;   // the selection moved; re-stamp the marks
    let wantHint = false;        // the count moved; rewrite the status line
    let easeAt = 0;              // where the viewport is heading
    let easing = false;
    /** What that heading is worked out from: the row, its direction, the origin. */
    let aim = { row: -1, down: true, from: 0 };

    function schedule() { if (!frameId) frameId = frame(tick); }

    function tick() {
      frameId = 0;
      if (easing) {
        // The aim is worked out from `geom', and `geom' is only re-read where a
        // row is DRAWN — past the door `renderRows' turns back at when the
        // window has not moved. So an ease can otherwise run to its end against
        // a header or row height some earlier frame read, and park short of the
        // row it chose. Re-read it here, where the tick already owns the frame,
        // and take the aim again against what it says.
        measure();
        const port = scroll.clientHeight || 0;
        if (port) easeAt = aimed(port);
        const step = easeAt - scroll.scrollTop;
        if (Math.abs(step) < SNAP_PX) { scroll.scrollTop = easeAt; easing = false; }
        else {
          const was = scroll.scrollTop;
          scroll.scrollTop = was + step * EASE;
          // A scroller that took NOTHING is at its own end, or rounding the
          // step away: `scrollHeight' is an integer over fractional content and
          // `scrollTop' snaps to a device pixel, so a target can sit a pixel
          // past anything this scroller will hold. Ending only on ARRIVAL then
          // runs a frame loop against that clamp for as long as the page is
          // open — which it did at both ends, before ever a target came off
          // `scrollHeight'. A refused step is an arrival.
          if (scroll.scrollTop === was) easing = false;
        }
        wantWindow = true;
      }
      // Forced only when the rows themselves changed; a selection that has not
      // moved the window re-stamps the trs that are already there, which is
      // what lets the marks crossfade instead of being rebuilt at their new
      // value.
      if (wantWindow || wantSelection) renderRows();
      if (wantSelection) stampSelection();
      // `renderRows' writes the line when it gets that far and clears the flag
      // doing it, so this is the toggle that never moved the window.
      if (wantHint) renderHint();
      wantWindow = wantSelection = false;
      if (easing) schedule();
    }

    /**
     * Where the viewport has to sit for `aim' to hold, worked out against the
     * geometry as it now measures and clamped to the content. PORT is the
     * viewport's height.
     */
    function aimed(port) {
      const top = geom.head + aim.row * geom.row, foot = top + geom.row;
      let to = aim.from;
      if (aim.down) { if (foot - aim.from > port * 2 / 3) to = foot - port * 2 / 3; }
      else if (top - aim.from < port / 3) to = top - port / 3;
      return Math.max(0, Math.min(maxScroll(port), to));
    }

    /**
     * Aim the viewport at row I, arrived at from row WAS, keeping a margin
     * under the cursor the way `scroll-margin' and `scrolloff' do: moving down,
     * the row's foot is not allowed past two thirds of the port; moving up, its
     * head is not allowed above one third. Between those the viewport holds
     * still, and on a held run it follows a row at a time with the cursor
     * pinned to the band edge — which is what makes a long movement readable,
     * against `block: "nearest"' leaving the cursor on the very edge of the
     * viewport with nothing ahead of it.
     *
     * Clamped to the content, so at either end the cursor walks into the margin
     * rather than the view scrolling past the rows — standard scrolloff.
     *
     * Retargeting rather than queueing: a held key lands a new target every
     * 30ms or so, and the one loop heads for the latest, so it converges
     * instead of playing back a backlog. The aim is taken from where the ease
     * is going, not from where it is, or each keypress would re-derive against
     * a viewport still in flight and creep.
     *
     * The aim is kept rather than only its answer, because a row measures what
     * it measures when it is drawn: the frame loop works the target out again
     * from these three each frame, so a measure landing after the move that
     * chose the row still moves where the ease ends up.
     */
    function easeToRow(i, was) {
      const port = scroll.clientHeight || 0;
      if (!port) return;
      const from = easing ? easeAt : scroll.scrollTop;
      aim = { row: i, down: was < 0 || i >= was, from };  // downward, or the first pick
      const to = aimed(port);
      if (to === from && !easing) return;              // the band already holds it
      if (calm) { scroll.scrollTop = to; easing = false; return; }
      easeAt = to;
      easing = true;
    }

    /**
     * Summon the control. In palette mode that means raising the overlay; in
     * the others the box is on the page already and this only takes it. Either
     * way it is the one entry point a consumer's key binds to.
     */
    function openFilter() {
      if (palette) veil.style.display = "";
      input.focus();
      if (input.select) input.select();
    }

    /** Put it away again, and give the keyboard back to the table. */
    function closeFilter() {
      closeAc();
      if (palette) veil.style.display = "none";
      input.blur();
    }

    /**
     * The end of every ladder: the table takes the selection and the control
     * goes. In palette mode going means dissolving, which is the same gesture
     * one step further out — there is no box left on the page to merely blur.
     */
    function handOver() {
      selectFirstVisible();
      closeFilter();
    }

    /** Give the viewport up: whoever is scrolling it now outranks the ease. */
    function cancelEase() { easing = false; }

    /**
     * Turn to page N (from zero), landing the selection on the row LAND names
     * — its first or its last. The page is a different set of rows, so the
     * viewport jumps to the end the reader arrives at rather than gliding
     * across a hundred rows they never asked to see; the band then places the
     * landing row from there, which is a short move or none.
     * @param {number} to  @param {"first"|"last"} land  @returns {boolean}
     */
    function turnTo(to, land) {
      const pages = pageCount();
      const at = Math.max(0, Math.min(pages - 1, to));
      // An explicit turn is the crisp presentation by definition, so it snaps
      // back out of continuous — including to the page the cursor is already
      // showing, which in continuous is a real move (the slice) rather than
      // the no-op it is when the two agree.
      if (at === page && !continuous) return false;
      const col = state.selCol;
      continuous = false;
      page = at;
      // Reachable only from continuous mode: paged mode cannot get here empty
      // (pages === 1, at === 0 === page, the guard returns first), but in
      // continuous that same equality is a real move — so a producer that
      // emptied the set while the reader glided leaves this turn a rows-less
      // snap-back.  The mutation pass dated the empty branch dead; continuous
      // mode revived it.
      const rows = paged();
      if (!rows.length) { renderRows(true); return true; }
      const first = land === "first";
      // Arriving from the other page, the scroller is wherever the last one
      // left it: put it at the end being arrived at before the band reads it.
      scroll.scrollTop = first ? 0 : maxScroll(scroll.clientHeight || 0);
      easing = false;
      // The band wants to know which way this came from; a flip forward is a
      // move down whatever the indices say, and back is a move up.
      selAt = first ? -1 : rows.length;
      renderRows(true);
      selectRow(rows[first ? 0 : rows.length - 1].id, col ?? undefined);
      return true;
    }

    /**
     * Move the selection one row, and off the end of a page onto the next —
     * landing on its first row going forward, its last going back, with the
     * column carried through. A consumer's next-row and previous-row keys are
     * this: the page boundary is the renderer's to know about, since only it
     * knows there is one.
     * @param {number} step  @returns {boolean}
     */
    function selectStep(step) {
      let rows = paged();
      if (!rows.length) return false;
      const dir = step < 0 ? -1 : 1;
      const col = state.selCol;
      const at = state.selected === null
        ? -1 : rows.findIndex((r) => r.id === state.selected);
      if (at === -1) return selectRow(rows[dir > 0 ? 0 : rows.length - 1].id, col ?? undefined);
      const next = at + dir;
      if (next >= 0 && next < rows.length) return selectRow(rows[next].id, col ?? undefined);
      // Off the end of the page. Rather than turning it — a new set of rows and
      // a jumped scroller, which is a blink under a held key — the presentation
      // becomes continuous and the cursor steps onto the row that was always
      // there. The viewport is not touched beyond the offset `goContinuous'
      // applies, so the band eases across the seam as it does anywhere else.
      if (!pageSize || continuous) return false;         // the true end of the set
      goContinuous();
      rows = paged();
      const here = rows.findIndex((r) => r.id === state.selected);
      const across = here + dir;
      if (across < 0 || across >= rows.length) return false;
      return selectRow(rows[across].id, col ?? undefined);
    }

    /**
     * Put the selection on the first visible row, unless it is already on one.
     * What Enter in the filter box hands the table, so the keys a consumer
     * binds to rows have something to move from.
     */
    function selectFirstVisible() {
      const rows = paged();
      if (!rows.length) return;
      if (state.selected !== null && rows.some((r) => r.id === state.selected)) return;
      selectRow(rows[0].id, state.selCol ?? undefined);
    }

    /**
     * Put CHAIN in force and redraw. The one place an order is installed —
     * every gesture that changes it lands here, so a new order always resets
     * the page and the scroll the same way and always redraws both things that
     * describe it: the headers and the hint line.
     * @param {SortKey[]} chain
     */
    function applyChain(chain) {
      state.sortKeys = chain;
      page = 0;                          // a different order, read from the top
      continuous = false;
      dropSorted();
      scroll.scrollTop = 0;
      renderArrows();
      renderChips();
      renderRows(true);
    }

    /** Are A and B the same order? @param {SortKey[]} a @param {SortKey[]} b */
    const sameChain = (a, b) => a.length === b.length
      && a.every((k, i) => k.column === b[i].column && k.ascending === b[i].ascending
                        && !!k.nullsFirst === !!b[i].nullsFirst);

    /**
     * Sort on KEY in ASCENDING, replacing whatever sort was in force. False
     * when no column carries that key, so a caller can tell a sort that did not
     * happen from one that did.
     *
     * A producer's call, so it RESTATES the view's order: it is what a query
     * naming no sort key leaves standing, the way the declared `sort' is.
     * @param {string} key @param {boolean} ascending @returns {boolean}
     */
    function sortTo(key, ascending) {
      if (!colByKey(key)) return false;
      stated = [{ column: key, ascending, nullsFirst: false }];
      applyChain(stated);            // a stated order is in force as it is stated
      return true;
    }

    /**
     * PROMOTE KEY to the head of the sort chain, ascending, with the chain it
     * had shifting down behind it and KEY dropped from wherever it sat below —
     * a chain never names a column twice. KEY already leading instead FLIPS its
     * direction and leaves the keys behind it where they are.
     *
     * This is how a chain is COMPOSED in a browser. Pressing this over columns
     * in reverse priority order builds one: promote `deadline', then `state',
     * then `title', and the chain is title > state > deadline, with the query
     * showing it grow at each press. `table-view.el' composes the same chain
     * with a prefix argument — `C-u ^' appends a tie-breaker at the bottom —
     * which a page has no spelling for; ordered presses are the web's answer.
     *
     * The chain is WRITTEN INTO THE QUERY as ONE arrow-form token
     * (`sort:title->state:desc') and delivered like any other filter change, so
     * one representation carries the order everywhere: the chip shows it, DEL
     * takes a key off, the URL a consumer writes carries it, and a producer that
     * filters server-side is told what order to answer in. `deliver' is what
     * then puts it in force.
     *
     * What it composes onto is the order IN FORCE, declared chain and all, so
     * only the promoted key ever moves: a view opening on `state → scheduled'
     * that is asked for `deadline' opens on `deadline → state → scheduled' and
     * the reader loses no tie-breaker they were reading by. The first press is
     * therefore where the declared chain becomes tokens — which is the
     * divergence, spelled in full so what is on the strip is what the rows are
     * in.
     *
     * `sortable' gates this, the way it gates a header click: promotion is a
     * READER's gesture. A producer stating an order calls `sortBy', which is
     * ungated, replaces the chain outright and touches no query.
     * @param {string} key @returns {boolean} whether the chain moved
     */
    function sortPromote(key) {
      const col = colByKey(key);
      if (!col || col.sortable !== true) return false;
      const chain = state.sortKeys, lead = chain[0];
      writeSort(
        lead && lead.column === key
          ? [{ column: key, ascending: !lead.ascending, nullsFirst: lead.nullsFirst }]
              .concat(chain.slice(1))
          : [{ column: key, ascending: true, nullsFirst: false }]
              .concat(chain.filter((k) => k.column !== key)));
      return true;
    }

    /**
     * Put CHAIN into the applied query: the sort tokens it already carries come
     * off, the chain goes on the end as ONE arrow-form token, and the query is
     * delivered. The tail is where it lands because the strip reads left to
     * right as what is on show and then what ORDER it is in.
     * @param {SortKey[]} chain
     */
    function writeSort(chain) {
      const keys = queryKeys();
      chips = chips.filter((c) => !parseQuery(c, keys).some((t) => t.key === SORT_KEY));
      if (chain.length) pushChip(sortToken(chain));
      renderChips();
      deliver();
    }

    function dispatch(command, row) {
      if (!row) return;
      if (o.onAction) o.onAction(command, row.id, row);
      root.dispatchEvent(new CustomEvent("tableview-action",
        { detail: { command, id: row.id, row } }));
    }

    function followLink(target, row) {
      if (o.onLink) { o.onLink(target, row); }
      else if (/^https?:\/\//i.test(target)) window.open(target, "_blank", "noopener");
      root.dispatchEvent(new CustomEvent("tableview-link", { detail: { target, row } }));
    }

    function defaultCommand() {
      const a = actions().find((x) => x.key === "RET") || actions()[0];
      return a && a.command;
    }

    /** Where an event landed (clicks always land on an element).
     * @param {Event} e  @returns {Element|null} */
    const hit = (e) => /** @type {Element|null} */ (e.target);
    /** @param {Row[]} rows  @param {HTMLElement} tr */
    const rowOf = (rows, tr) => rows.find((r) => r.id === tr.dataset.id);

    /**
     * Which column of TR the cell TD is, or null where it is none — no cell at
     * all, or the mark box, which is chrome and belongs to no column. One
     * answer for both pointer paths, so the offset the chrome introduces is
     * applied once rather than at every place a td is turned into an index.
     * @param {HTMLElement} tr  @param {HTMLElement|null} td  @returns {number|null}
     */
    function colOf(tr, td) {
      if (!td) return null;
      const at = Array.prototype.indexOf.call(tr.children, td) - chrome;
      return at < 0 ? null : at;
    }

    /** Is TARGET inside the mark box of a row this table is marking? */
    const onBox = (target) => marks && !!target.closest("td.tv-box");

    // ---- events (delegated, attached once) ---------------------------------

    scroll.addEventListener("click", (e) => {
      const t = hit(e);
      if (!t) return;
      const a = /** @type {HTMLElement|null} */ (t.closest("a.tv-link"));
      if (a) {
        e.preventDefault();
        const tr = /** @type {HTMLElement|null} */ (a.closest("tr[data-id]"));
        followLink(a.dataset.target, (tr && rowOf(state.rows, tr)) || null);
        return;
      }
      // A header click is the pointer's spelling of `^': one command, so the
      // two gestures compose a chain the same way rather than a click quietly
      // throwing away what the keyboard just built.
      const th = /** @type {HTMLElement|null} */ (t.closest("th[data-key]"));
      if (th) { sortPromote(String(th.dataset.key)); return; }
      const tr = /** @type {HTMLElement|null} */ (t.closest("tr[data-id]"));
      if (!tr) return;
      // The box is the one cell that is not a selection: a mark is a standing
      // choice about a row and says nothing about where the cursor is, so
      // checking one leaves the cursor where the reader put it.
      if (onBox(t)) {
        if (tr.dataset.id !== undefined) markSet.toggle(tr.dataset.id);
        return;
      }
      setSelected(tr.dataset.id ?? null,
                  colOf(tr, /** @type {HTMLElement|null} */ (t.closest("td"))));
    });

    // A long press is the touch reading of the row's default action — what RET
    // and a double click already do. It has to survive being the start of a
    // scroll, which is what every touch on a list might be, so drift or a
    // scroll of any size calls it off; only a finger that stayed put counts.
    let pressAt = 0, pressX = 0, pressY = 0, pressRan = false;
    /** @type {string|null} */
    let pressOn = null;

    function cancelPress() {
      if (pressAt) { clearTimeout(pressAt); pressAt = 0; }
      pressOn = null;
    }

    scroll.addEventListener("touchstart", (e) => {
      const t = hit(e);
      const tr = t && /** @type {HTMLElement|null} */ (t.closest("tr[data-id]"));
      const touch = e.touches && e.touches[0];
      if (!tr || !touch) return;
      // A finger resting on the box is still aiming at the box. Without this
      // the press falls through to the row's default action, and the touchend
      // that completes it swallows the click the toggle would have arrived on
      // — so on the one pointer the 44px target was widened for, the box could
      // not be checked at all.
      if (onBox(t)) return;
      cancelPress();
      pressRan = false;
      pressOn = tr.dataset.id ?? null;
      pressX = touch.clientX;
      pressY = touch.clientY;
      const at = colOf(tr, /** @type {HTMLElement|null} */ (t.closest("td")));
      pressAt = setTimeout(() => {
        pressAt = 0;
        if (pressOn === null) return;
        pressRan = true;
        setSelected(pressOn, at);
        const cmd = defaultCommand();
        if (cmd) dispatch(cmd, state.rows.find((r) => r.id === pressOn));
      }, LONG_PRESS);
    });

    scroll.addEventListener("touchmove", (e) => {
      const touch = e.touches && e.touches[0];
      if (!touch || pressOn === null) return;
      if (Math.abs(touch.clientX - pressX) > PRESS_SLOP
       || Math.abs(touch.clientY - pressY) > PRESS_SLOP) cancelPress();
    });

    // Only the touchend that completes one is swallowed — that press has been
    // spent, and letting it through would follow with a click and a context
    // menu on top of the action. Every other touchend is the page's as usual.
    scroll.addEventListener("touchend", (e) => {
      if (pressRan) { e.preventDefault(); pressRan = false; }
      cancelPress();
    });
    scroll.addEventListener("touchcancel", cancelPress);

    scroll.addEventListener("dblclick", (e) => {
      const t = hit(e);
      const tr = t && /** @type {HTMLElement|null} */ (t.closest("tr[data-id]"));
      if (!tr) return;
      const cmd = defaultCommand();
      if (cmd) dispatch(cmd, rowOf(state.rows, tr));
    });

    scroll.addEventListener("scroll", () => { wantWindow = true; cancelPress(); schedule(); });
    // A hand on the wheel, a finger on the glass, a drag of the scrollbar: the
    // ease stops chasing its target and leaves the viewport where it is put.
    for (const how of ["wheel", "touchmove", "pointerdown", "keydown"])
      scroll.addEventListener(how, cancelEase);

    // ---- chips -------------------------------------------------------------
    // A committed token leaves the box and becomes a chip. The query is the
    // chips and the box together, always — chips are where the finished tokens
    // are kept, not a second filter — so the box holds only what is still being
    // typed and a long query stops scrolling out of sight.

    /** The committed tokens, each the source text it was written as. */
    /** @type {string[]} */
    let chips = [];

    /**
     * The trail a drill-down left: where the reader came FROM, oldest first.
     * Handle state, the way a mark is — the consumer owns the drilling and this
     * owns the strip — so it survives `setRows' and every filter, and `setView'
     * takes it with the world it described.
     * @type {Crumb[]}
     */
    let crumbs = [];

    /** The query as it stands: every chip, then whatever is in the box. */
    function effectiveQuery() {
      const typed = input.value.trim();
      if (!chips.length) return typed;
      const front = chips.join(" ");
      return typed ? front + " " + typed : front;
    }

    /** C as this keeps a crumb, or null when it is not one. @param {*} c */
    function crumbOf(c) {
      return c && typeof c === "object"
        ? { label: String(c.label ?? ""), query: String(c.query ?? "") } : null;
    }

    /**
     * How a live chip reads. A `chipLabel' formatter may alias the token to
     * something a reader would rather see; the QUERY is untouched, so what
     * `getQuery' answers, what `onFilter' is handed and what a click takes off
     * are all still the token as written. Crumbs never reach this — a crumb's
     * label IS its label, and running a token formatter over one would be
     * asking a query question about a word that is not a query.
     * @param {string} tok
     */
    function chipText(tok) {
      if (chipLabel) {
        const alias = chipLabel(tok);
        if (typeof alias === "string" && alias) return alias;
      }
      return spelled(tok);
    }

    /**
     * TOK in the grammar's own `key:value' spelling. A bare word is free text,
     * which is `substring:' with the key elided (SCHEMA.md, Filter query), so
     * the chip spells the key out and the strip reads `key:value' throughout.
     * The QUERY keeps what the reader typed — this is the label alone.
     * @param {string} tok  @returns {string}
     */
    function spelled(tok) {
      const t = asToken(tok);
      if (!t || t.key !== null || !t.value) return tok;
      const value = /[\s&"]/.test(t.value) ? `"${t.value}"` : t.value;
      return `${t.negated ? "-" : ""}${SUBSTRING_KEY}:${value}`;
    }

    /**
     * The crumbs as the strip draws them, leftmost first. Past CRUMB_MAX the
     * oldest collapse into one `… +N' counter, and the counter takes a slot of
     * its own — so the strip is never wider than CRUMB_MAX chips however deep
     * the drilling went, and the fifth crumb is what folds the first two away.
     * @returns {string[]}
     */
    function crumbStrip() {
      if (crumbs.length <= CRUMB_MAX) return crumbs.map((c) => c.label);
      const kept = crumbs.slice(crumbs.length - (CRUMB_MAX - 1));
      return ["… +" + (crumbs.length - kept.length)].concat(kept.map((c) => c.label));
    }

    // Crumbs lead and live chips follow: the strip reads left to right as where
    // the reader came FROM and what is on show. The ORDER is in the strip too,
    // as the `sort:' tokens of the query itself, and the headers carry it over
    // the columns it is about — a chip per key beside them said the same thing
    // twice, out of a second store that could describe an order the rows were
    // not in. Only a live chip carries `data-i', which is what the click
    // delegation reads the removable one by; history is no token to take off.
    //
    // A live chip that ORDERS says so in its class, and the class is what wears
    // the column band's hue: the strip tells ordering from narrowing at a
    // glance. A crumb is a LABEL rather than a token, so it takes no ordering
    // class however it is spelled.
    function renderChips() {
      let html = "";
      for (const text of crumbStrip())
        html += `<span class="tv-chip tv-chip-muted">${esc(text)}</span>`;
      for (let i = 0; i < chips.length; i++)
        html += `<span class="tv-chip${chipClassOf(chips[i])}"`
              + ` data-i="${i}" title="remove">${esc(chipText(chips[i]))}`
              + `<i class="tv-chip-x">×</i></span>`;
      // The pin rides the strip's far edge and keeps the strip visible even
      // with nothing applied: the badge is a BOOLEAN a reader can always see,
      // and the click is the touch door to whatever the consumer pins.
      if (onPin)
        html += `<span class="tv-pin${pinned ? " tv-pinned" : ""}" title="${
          pinned ? "this view is the default" : "pin this view as the default"}">📌</span>`;
      chipsEl.innerHTML = html;
      chipsEl.style.display = (crumbs.length || chips.length || onPin) ? "" : "none";
    }

    /** The one token TOK spells, parsed. @param {string} tok  @returns {Token|undefined} */
    const asToken = (tok) => parseQuery(tok, queryKeys())[0];

    /**
     * Whether TOK states an order this renderer READS: a sort key that resolves
     * to a column, or `*none*', the empty chain. Those are the tokens `chainFor'
     * builds the order out of, and the ones the strip may colour as an ordering.
     *
     * A refusal — a negation, an alternation, an unknown column, a direction
     * that is neither word — is dropped from the chain, and no sort token is a
     * predicate, so it orders no rows and narrows none. It keeps the ordinary
     * chip: the strip promises an order where there is one, and shows what was
     * typed where there is not. `sortable' gates the reader's GESTURE rather
     * than the token, so a column that opts out still orders and still wears it.
     * @param {string} tok  @returns {boolean}
     */
    function ordersRows(tok) {
      const t = asToken(tok);
      if (!t || t.key !== SORT_KEY) return false;
      return sortSegments(t).some((s) => s.toLowerCase() === NONE_META
                                      || !!sortKeyOf(s, namesColumn));
    }

    /**
     * Whether TOK states a column set: a columns key naming at least one
     * column. Every nonempty name counts — a name this view does not carry is
     * the producer's custom column, so this side cannot call any unknown —
     * and the half-typed `columns:' keeps the ordinary chip, naming none.
     * @param {string} tok  @returns {boolean}
     */
    function showsColumns(tok) {
      const t = asToken(tok);
      if (!t || t.key !== COLUMNS_KEY || t.negated) return false;
      return t.value.split(",").some((n) => n !== "");
    }

    /**
     * Whether TOK names a saved view: a view key naming one the producer
     * declared. An unknown name keeps the ordinary chip — what a name MEANS is
     * the producer's, so this side calls none of them wrong — and so does the
     * half-typed `view:', naming none.
     * @param {string} tok  @returns {boolean}
     */
    function namesView(tok) {
      const t = asToken(tok);
      if (!t || t.key !== VIEW_KEY || t.negated) return false;
      const want = t.value.toLowerCase();
      return savedViews().some((v) => String(v.name || "").toLowerCase() === want);
    }

    /**
     * The dress a chip wears for the view token it states: the sort hue, the
     * columns hue, the saved-view hue, or none — one classifier, so the strip's
     * render names no token kind of its own and a new view token registers its
     * class here.
     * @param {string} tok  @returns {string}
     */
    const chipClassOf = (tok) =>
      ordersRows(tok) ? " tv-chip-sort"
        : showsColumns(tok) ? " tv-chip-cols"
        : namesView(tok) ? " tv-chip-view" : "";

    /**
     * The ONE token spelling the order query Q names, in canonical arrow form.
     * `sortsIn' is what decides it, so the strip cannot describe an order the
     * rows are not in: it is the same reading `applyChain' is handed. Called
     * behind `ordersRows', so Q always names an order and the empty answer is
     * the meta Q spelled rather than a query that said nothing.
     * @param {string} q  @returns {string}
     */
    function sortChip(q) {
      const chain = sortsIn(q, queryKeys(), namesColumn);
      return chain && chain.length ? sortToken(chain) : `${SORT_KEY}:${NONE_META}`;
    }

    /**
     * Put TOK on the strip, unless the strip already carries the same token.
     * Every token is idempotent under the one combination rule — a repeated
     * predicate narrows to what it narrowed, a repeated sort key is the position
     * it already holds — so a second copy is chrome the reader has to read past,
     * in the strip, in the URL and in what the producer is asked. A predicate is
     * itself as spelled, so a near twin (`tag:game' beside `tag:games') asks a
     * different question and stays a second chip.
     *
     * ONE ORDER, ONE CHIP. Every token that STATES an order folds into the chip
     * already stating one, and what lands is the canonical arrow form of the
     * chain the two name together: `sort:title sort:priority' is the single chip
     * `sort:title->priority', which is what the URL then carries and what the
     * producer is asked. First-wins dedup rides in `sortsIn', so a column already
     * chained keeps its place and its spelling however the twin is written and
     * whichever side of an arrow it falls. A token this renderer reads NO order
     * from — a negation, an unknown column, a direction that is neither word —
     * folds into nothing: it stays its own chip as spelled, and the query carries
     * it back to the producer verbatim, which is the whole of how a refusal is
     * ever answered.
     * @param {string} tok
     */
    function pushChip(tok) {
      if (ordersRows(tok)) {
        const at = chips.findIndex(ordersRows);
        const folded = sortChip(at === -1 ? tok : chips[at] + " " + tok);
        if (at === -1) chips.push(folded); else chips[at] = folded;
        return;
      }
      if (!chips.some((c) => c === tok)) chips.push(tok);
    }

    /**
     * Move the box's finished tokens into chips. A token with nothing after it
     * is still being typed and stays put, so a word is never chipped out from
     * under the caret; ALL overrides that, which is what Enter means.
     * @param {boolean} [all]  @returns {boolean} whether anything moved
     */
    function chipUp(all) {
      const v = input.value;
      const toks = parseQuery(v, queryKeys());
      if (!toks.length) return false;
      const last = toks[toks.length - 1];
      const keep = !all && last.end === v.length ? last : null;
      for (const t of toks) if (t !== keep) pushChip(v.slice(t.start, t.end));
      if (keep && toks.length === 1) return false;      // nothing finished yet
      input.value = keep ? v.slice(keep.start) : "";
      if (input.setSelectionRange) input.setSelectionRange(input.value.length, input.value.length);
      renderChips();
      return true;
    }

    /** Adopt the query as it stands: re-filter, and redraw from the top. */
    function applyFilter() {
      const v = effectiveQuery();
      if (v === state.filter) return;
      state.filter = v;
      dropOrder();                       // `sorted' stands: only the filter moved
      scroll.scrollTop = 0;
      renderRows(true);
    }

    /** What the last delivery sent; `getQuery' answers with it. */
    let lastQuery = "";

    // With `onFilter', the producer narrows the rows and this hands it the
    // query instead of filtering locally: `state.filter' stays empty, so
    // `order' is `sorted' and the rows given are the rows shown. Either way it
    // is the whole query — chips and box joined — that travels, and this is the
    // one place it does. ONFRAME defers the local re-filter to a frame, which
    // is what a keystroke wants and what an explicit commit does not.
    function deliver(onFrame) {
      const q = effectiveQuery();
      // Nothing changed, so there is nothing to say. Local filtering worked
      // this out for itself; a producer had no way to, and was being asked the
      // same question twice — by Escape dropping text that was never sent, by
      // a commit on an empty box, by a debounce settling on what it settled on
      // before.
      if (q === lastQuery) return;
      lastQuery = q;
      page = 0;                          // a different question, read from the top
      continuous = false;
      // The ORDER travels in the query too, so this is where a `sort' token
      // takes effect — before the producer is asked, so the rows in hand
      // re-order under the reader's hand and the answer lands in the order they
      // asked for rather than moving again when it arrives.
      const chain = chainFor(q);
      if (!sameChain(chain, state.sortKeys)) applyChain(chain);
      if (o.onFilter) o.onFilter(q);
      else if (onFrame) frame(applyFilter);
      else applyFilter();
    }

    /**
     * Take off the last unit of the query: what is half-typed in the box if
     * there is any, else the last chip — WHOLE, the sort chip included. The
     * chain used to give up one tie-breaker per press; a chip that erased by
     * a different rule than its neighbours made DEL a thing to think about,
     * and an order is one decision, taken off the way it went on.
     * False when there was nothing left to take off, so a consumer can walk
     * the query down and know when it has hit the end.
     * @returns {boolean}
     */
    function stripLastToken() {
      if (input.value.trim()) {
        input.value = "";
        if (debounce) { clearTimeout(debounce); debounce = 0; }
        closeAc();
        deliver();
        return true;
      }
      if (!chips.length) return false;
      dropChip(chips.length - 1);
      return true;
    }

    let debounce = 0;
    /**
     * Arm the delivery a keystroke implies — in the modes where a keystroke
     * implies one. The palette filters on commit alone: it is summoned over
     * the table, so narrowing it as the query is typed animates a thing the
     * typist is not looking at and cannot see the whole of, and every
     * half-written token is a query of its own. RET says when the query is a
     * query. The suggestion list stays live regardless; that is what the
     * typing is for.
     */
    function armFilter() {
      if (palette) return;
      if (debounce) clearTimeout(debounce);
      debounce = setTimeout(() => {
        debounce = 0;
        chipUp(false);
        deliver(true);
      }, DEBOUNCE);
    }
    input.addEventListener("input", () => { armFilter(); openAc(); });

    // ---- the suggestion list -----------------------------------------------
    // SCHEMA.md's autocomplete: a bare word suggests column keys, `key:'
    // suggests that column's value domain. Renderer-local — the producer is
    // never asked, and the list is only ever an aid to typing the grammar.

    const AC_MAX = 12;          // suggestions offered at once
    const TITLE_MAX = 5;        // whole titles offered
    const TITLE_MIN = 2;        // ... and only past this much typing
    const DOMAIN_MAX = 200;     // distinct values kept before the prefix narrows them

    /**
     * @type {{stage: string, tok: Token,
     *         items: {text: string, count: number, full: boolean, dim: boolean,
     *                  show?: string, aside?: string}[]}|null}
     */
    let ac = null;
    let acAt = 0;

    /**
     * COL's value domain, and how many rows stand behind each value.
     *
     * The order is the column's own where it has one — `values', else the badge
     * palette — because that order is semantic (it is the sort order too);
     * distinct cell values have none, so they sort. The counts come from one
     * pass over every row, which is what makes them counts rather than
     * estimates; `displayText' runs once per distinct value rather than once
     * per row, the cell text itself being cached already. The pass is lazy,
     * per column, and thrown away with the text cache.
     * @returns {{list: string[], counts: Map<string, number>}}
     */
    function domainOf(col) {
      let d = domains.get(col.key);
      if (!d) {
        const i = columns().indexOf(col);
        // The tags column's values are the tags, not the `:a:b:' strings its
        // cells spell them in — the vocabulary already holds them, counted.
        if (i === multiColumn()) {
          const v = tagVocab();
          const counts = new Map();
          for (const tag of v.list) counts.set(tag, (v.ids.get(tag) || new Set()).size);
          d = { list: declaredMetas(col).concat(v.list), counts };
          domains.set(col.key, d);
          return d;
        }
        const counts = new Map();
        const found = [];
        for (const r of state.rows) {
          const lower = rowText(r).cells[i];
          if (!lower) continue;
          const n = counts.get(lower);
          if (n !== undefined) { counts.set(lower, n + 1); continue; }
          counts.set(lower, 1);
          if (found.length < DOMAIN_MAX) found.push(displayText((r.cells || {})[col.key]));
        }
        const fixed = domainValues(col);
        d = { list: fixed || found.sort(), counts };
        domains.set(col.key, d);
      }
      return d;
    }

    /** The token the caret sits in, or null. @returns {Token|null} */
    function tokenAtCaret() {
      const v = input.value;
      const caret = typeof input.selectionStart === "number" ? input.selectionStart : v.length;
      for (const t of parseQuery(v, queryKeys()))
        if (caret >= t.start && caret <= t.end) return t;
      return null;
    }

    /**
     * What the token under the caret is asking for, or null when it asks for
     * nothing: a quoted token is free text and takes no suggestions, and so
     * does one carrying punctuation that named no column — `:work:' is org
     * text, not a half-typed predicate.
     * @returns {{stage: string, tok: Token, col: Column|null, prefix: string}|null}
     */
    function stageAt() {
      const t = tokenAtCaret();
      if (!t || t.quoted) return null;
      if (t.key !== null) {
        // `sort' has a domain of its own and it is no column's: the columns a
        // reader may order by, and the direction behind whichever one is named.
        // A `->' RE-OPENS that domain the way a `|' re-opens a value's — the
        // prefix is what follows the LAST arrow, so a chain is completed one
        // column at a time and the committed token stays one token.
        if (t.key === SORT_KEY) {
          const v = t.value.toLowerCase(), arrow = v.lastIndexOf(SORT_ARROW);
          return { stage: "sort", tok: t, col: null,
                   prefix: arrow === -1 ? v : v.slice(arrow + SORT_ARROW.length) };
        }
        // `columns' likewise: the domain is the view's own columns, and a `,'
        // RE-OPENS it the way `->' re-opens the sort's — the prefix is what
        // follows the LAST comma, so a set is completed one column at a time
        // and the committed token stays one token.
        if (t.key === COLUMNS_KEY) {
          const v = t.value.toLowerCase(), comma = v.lastIndexOf(",");
          return { stage: "columns", tok: t, col: null,
                   prefix: comma === -1 ? v : v.slice(comma + 1) };
        }
        // `view' likewise has a domain of its own and it is no column's: the
        // saved views the producer named, completed whole.
        if (t.key === VIEW_KEY)
          return { stage: "view", tok: t, col: null, prefix: t.value.toLowerCase() };
        // `planned' takes no value list: what follows it is a date prefix over
        // several columns at once, which is no domain to enumerate. It is the
        // one key with no column behind it, so every other one has a domain.
        const col = colByKey(t.key);
        // A `|' RE-OPENS the domain: the prefix is what follows the LAST bar,
        // so `state:TODO|d' is asking for the values again and completes the
        // alternative being typed rather than the whole value.
        return col ? { stage: "value", tok: t, col,
                       prefix: t.value.slice(t.value.lastIndexOf(ALT) + 1) } : null;
      }
      if (!t.value || splitAt(t.value) !== -1) return null;
      return { stage: "key", tok: t, col: null, prefix: t.value };
    }

    /**
     * The suggestions for STAGE: the text each one inserts, the number of rows
     * behind it, and whether it finishes a token. A column completion does not
     * — it lands as `key:' with the value still to type, and carries no count
     * because it narrows nothing on its own.
     *
     * Row one is what Enter takes (`openAc'), so the ordering here is the whole
     * of what that key means: WHAT THE WORD SPELLS IN FULL LEADS WHAT IT MERELY
     * OPENS, at either stage.
     * @returns {{text: string, count: number, full: boolean, dim: boolean,
     *             show?: string, aside?: string}[]}
     */
    function suggestFor(st) {
      const p = st.prefix.toLowerCase();
      const out = [];
      // `sort:' — the columns a reader may order by, `sortable' deciding which,
      // and `asc'/`desc' once one is named in full, with `*none*' behind them.
      // The offers finish the token, so each lands with the space that opens
      // the next. What is offered is `sortable''s because completing IS the
      // reader's gesture; the token a reader may WRITE is not gated by it, and
      // a chain naming a column that opts out opens as written either way.
      if (st.stage === "sort") {
        const at = p.indexOf(":");
        const wantCol = at === -1 ? p : p.slice(0, at);
        const wantDir = at === -1 ? null : p.slice(at + 1);
        // What the token ALREADY chains — every segment but the one being typed.
        // A chain never names a column twice, so past an arrow the domain is the
        // columns left to order by, and a reader is offered no tie-breaker that
        // would be dropped the moment it was accepted.
        const chained = sortSegments(st.tok).slice(0, -1)
          .map((s) => s.split(":")[0].toLowerCase());
        const offer = (text, dim) =>
          out.push({ text, count: -1, full: true, dim: !!dim });
        // THE ORDER IN FORCE LEADS AN EMPTY `sort:'.  Row one is what Enter
        // takes, so a reader who typed the key and nothing else gets the chain
        // the table is ALREADY in -- canonical arrow form, the same string the
        // chip door writes -- and edits it from there: a segment off with
        // backspace, another on with `->'. Without it the reader had to spell a
        // chain the view could have told them, which is the one thing an
        // autocomplete is for. Only with nothing typed and nothing chained: past
        // either, they are picking a column.
        if (!p && !chained.length && state.sortKeys.length)
          // NOT `full': accepting it leaves the caret at the end of the chain
          // with the list still open, so the reader edits from there — a
          // segment off with backspace, another on with `->' — rather than
          // having the view they already had applied back at them.
          out.push({ text: sortToken(state.sortKeys).slice(SORT_KEY.length + 1),
                     count: -1, full: false, dim: false });
        for (const c of columns()) {
          if (out.length >= AC_MAX) break;
          if (c.sortable !== true) continue;
          const key = String(c.key), lower = key.toLowerCase();
          if (chained.indexOf(lower) !== -1) continue;
          if (wantDir === null) {
            if (!lower.startsWith(wantCol)) continue;
            offer(key);
            if (lower === wantCol) offer(key + ":desc");
          } else if (lower === wantCol) {
            for (const d of ["asc", "desc"]) if (d.startsWith(wantDir)) offer(key + ":" + d);
          }
        }
        // The empty chain, offered last and gated by nothing: it names no
        // column, so there is no `sortable' to consult, and it wears no
        // direction. Star-blind like every meta, so `non' reaches it, and drawn
        // dim like every meta — vocabulary rather than a fact about a column.
        // Not past an arrow: a token that has named a column has already said
        // the order is not the empty one, and `*none*' takes no companions.
        if (wantDir === null && !chained.length && opensWith(NONE_META, wantCol))
          offer(NONE_META, true);
        return out.slice(0, AC_MAX);
      }
      // `columns:' — the view's own columns, less the ones the token has
      // already named (a set never names a column twice; the producer keeps
      // the first spelling anyway).  Every offer finishes the token; a reader
      // chaining types the comma and the domain re-opens.  A name the view
      // does not carry is still WRITABLE — it is the producer's custom
      // property column — so nothing here is a wall, only the vocabulary.
      if (st.stage === "columns") {
        const taken = st.tok.value.toLowerCase().split(",").slice(0, -1)
          .filter((n) => n !== "");
        for (const c of columns()) {
          if (out.length >= AC_MAX) break;
          const key = String(c.key), lower = key.toLowerCase();
          if (taken.indexOf(lower) !== -1) continue;
          if (!lower.startsWith(p)) continue;
          out.push({ text: key, count: -1, full: true, dim: false });
        }
        return out.slice(0, AC_MAX);
      }
      // `view:' — the saved views the producer named, each with the query it
      // holds now as the aside, so a reader picks by what it DOES rather than by
      // what it is called.  Every offer finishes the token: a view is one name.
      if (st.stage === "view") {
        for (const v of savedViews()) {
          if (out.length >= AC_MAX) break;
          const name = String(v.name || "");
          if (!name.toLowerCase().startsWith(p)) continue;
          out.push({ text: name, count: -1, full: true, dim: false,
                     aside: v.query ? String(v.query) : undefined });
        }
        return out.slice(0, AC_MAX);
      }
      if (!st.col) {
        // Values some column actually has, reached by prefix: `TOD' means
        // `state:TODO' and `alberbl' means `tags:alberblanc'. Facts about the
        // data rather than guesses about it — but only where a column has a
        // domain worth enumerating: its declared `values', its badge palette,
        // or the tag vocabulary. A free-text column has no such set, and
        // offering one word of it is what the third tier is for.
        const hits = [];
        for (const c of columns()) {
          if (!domainValues(c) && columns().indexOf(c) !== multiColumn()) continue;
          const dom = domainOf(c);
          for (const v of dom.list) {
            const lower = String(v).toLowerCase();
            if (!opensWith(lower, p)) continue;
            const meta = META.test(String(v));
            hits.push({ text: c.key + ":" + v,
                        count: meta ? -1 : dom.counts.get(lower) || 0,
                        whole: spells(lower, p), dim: meta });
          }
        }
        // What was typed in full outranks what merely opens with it.
        hits.sort((a, b) => (b.whole ? 1 : 0) - (a.whole ? 1 : 0)
                         || b.count - a.count
                         || (a.text < b.text ? -1 : 1));
        const exact = hits.length > 0 && hits[0].whole;
        // 1. The value the word already SPELLS, where a column holds one:
        //    `book' is `tag:book'. It leads, because it is the one offer that
        //    needs no more typing — ahead of the `book:' key beside it, which
        //    asks for the same rows in a token still half written. Seeded
        //    before the tiers below so their caps cannot crowd it out.
        if (exact) {
          const top = hits.shift();
          out.push({ text: top.text, count: top.count, full: true, dim: top.dim });
        }
        // 2. The keys the word opens — the view's columns, and `planned' with
        //    them, it being the view's own vocabulary too. Exact facts, so
        //    none is dimmed, and none carries a count: a column has no one
        //    number to show. A TAG is not among them — `tag:course' is the one
        //    spelling, and tier 1 or 3 offers it as the value it is.
        const keys = queryKeys();
        const opens = keys.filter((k) => k.toLowerCase().startsWith(p));
        // A key the word spells in full leads the ones it only opens.
        for (const k of opens.filter((k) => k.toLowerCase() === p)
                             .concat(opens.filter((k) => k.toLowerCase() !== p))) {
          out.push({ text: k + ":", count: -1, full: false, dim: false });
          if (out.length === AC_MAX) break;
        }
        // 3. The values it merely opens, in the order the sort left them.
        for (const hit of hits) {
          if (out.length === AC_MAX) break;
          out.push({ text: hit.text, count: hit.count, full: true, dim: hit.dim });
        }
        // 4. The TITLES the text is inside, whole. Someone typing a fragment of
        //    a headline is looking for the ROW, so the offer is that row's own
        //    title as a free-text token — a title is a thing the reader has
        //    seen. Prefix hits lead the ones that merely hold it, the same rule
        //    the tiers above follow. On a floor of its own: one letter is
        //    inside most of the store and says nothing about any of it.
        if (p.length >= TITLE_MIN) {
          const opensT = [], holds = [];
          for (const t of titleIndex().titles) {
            if (t.lower.indexOf(p) === -1) continue;
            if (t.lower === p) continue;          // spelled already; the literal has it
            // The grammar has no escape inside a quoted token, so a title
            // carrying one would commit as a token that no longer matches the
            // row it came from. Better absent than offered and empty.
            if (t.lower.indexOf('"') !== -1) continue;
            (t.lower.startsWith(p) ? opensT : holds).push(t);
          }
          for (const t of opensT.concat(holds).slice(0, TITLE_MAX)) {
            if (out.length === AC_MAX) break;
            // The cased text is read HERE, for the five on offer, rather than
            // per title when the index was built: `displayText' parses org
            // links, and every title of a loaded store is a bill this tier can
            // pay five rows of instead.
            const show = displayText(t.cell);
            // Quoted, titles holding spaces; the aside says which row it is,
            // where the tiers above show a count.
            out.push({ text: `"${show}"`, show, aside: "title",
                       count: -1, full: true, dim: false });
          }
        }
        // THE LITERAL, spliced to its rank rather than pushed: it leads, so the
        // caps above cannot crowd it out, the way the exact value is seeded
        // ahead of them. Row one is what RET takes, and without this row a
        // plain text search is reachable only by quoting or by Escape — a
        // grammar lesson charged for a search. It yields to one thing: an offer
        // that SPELLS what was typed, which is an answer where this is the
        // letters back again.
        // A tag spelled in full needs no clause of its own: the tags column's
        // domain IS the vocabulary, so tier 1 has already called it exact.
        const spelled = exact || keys.some((k) => k.toLowerCase() === p);
        out.splice(spelled ? 1 : 0, 0, literalOffer(st.prefix));
        if (out.length > AC_MAX) out.pop();
        return out;
      }
      // The column's value domain, led by the value typed in FULL — the one
      // offer that finishes the token as written. It is looked for past the
      // twelve on offer, so a domain deep enough to bury it still leads with
      // it, and the search stops once it is in hand and the list is full.
      const dom = domainOf(st.col);
      // `*empty*' rides at the foot of every column's domain, declared or not:
      // it is the one meta every key answers, and no column's own order has a
      // place for a value no cell holds. A producer that named it itself keeps
      // the place it gave it.
      const domain = dom.list.indexOf(EMPTY_META) === -1
        ? dom.list.concat([EMPTY_META]) : dom.list;
      /** @type {{text: string, count: number, full: boolean, dim: boolean}|null} */
      let whole = null;
      for (const v of domain) {
        if (whole && out.length >= AC_MAX) break;
        const lower = String(v).toLowerCase();
        if (!opensWith(lower, p)) continue;
        // A producer meta stands apart from the concrete values beside it:
        // dimmed and italic, and with no count. These counts are per cell
        // VALUE, and no cell holds the literal `*active*', so counting one here
        // would print 0 — or, once `tokenTest' answers the empty half of
        // `*active*', the stateless rows alone, which is a fraction of what the
        // producer will match. Either number beside a value that in fact
        // matches many rows is worse than no number. What it means is the
        // producer's to say; see `tokenTest'.
        const meta = META.test(String(v));
        const item = { text: String(v), count: meta ? -1 : dom.counts.get(lower) || 0,
                       full: false, dim: meta };
        if (spells(lower, p)) { whole = item; continue; }
        if (out.length < AC_MAX) out.push(item);
      }
      if (whole) {
        out.unshift(whole);
        if (out.length > AC_MAX) out.pop();
      }
      return out;
    }

    /**
     * The literal offer: what was typed, as a free-text token. It is DRAWN
     * quoted, the grammar's own notation for "this is text" and the thing the
     * row is there to teach, and it COMMITS bare, which is what a reader who
     * knew the grammar would have written — the two match identically, and
     * quotes are owed only where a separator would break the token up. (Of
     * those, only whitespace can reach here, through a quote written mid-token:
     * a colon makes the token a predicate, or free text with no list at all.)
     * @param {string} text
     */
    function literalOffer(text) {
      // SPELLED, key and all: free text IS `substring:' with the key elided,
      // so committing it writes the grammar's own `key:value' and the chip
      // that comes back reads the same way (SCHEMA.md, Filter query).
      const value = /[\s:&"]/.test(text) ? `"${text}"` : text;
      const tok = `${SUBSTRING_KEY}:${value}`;
      return { text: tok, show: tok, aside: "text search",
               count: -1, full: true, dim: false };
    }

    /**
     * The distinct titles, in row order, lowercased beside the RAW cell they
     * came from — the cased text is what `displayText' costs a link parse for,
     * and only the few titles offered need it. Built whole on first use rather
     * than patched: an upsert can move any of them, and rebuilding on the next
     * keystroke is both simpler and cheaper than keeping the set correct
     * through every row change. Thrown away with the text cache, which is where
     * it was read from.
     * @type {{titles: {lower: string, cell: Cell|undefined}[]}|null}
     */
    let wordIndex = null;

    function titleIndex() {
      if (wordIndex) return wordIndex;
      const at = titleColumn();
      /** @type {{lower: string, cell: Cell|undefined}[]} */
      const titles = [];
      const seen = new Set();
      const titleKey = at === -1 ? "" : columns()[at].key;
      if (at !== -1)
        for (const r of state.rows) {
          const lower = rowText(r).cells[at];
          if (!lower || seen.has(lower)) continue;
          seen.add(lower);
          titles.push({ lower, cell: (r.cells || {})[titleKey] });
        }
      wordIndex = { titles };
      return wordIndex;
    }

    function closeAc() {
      if (!ac) return;
      ac = null;
      acEl.innerHTML = "";
      acEl.style.display = "none";
    }

    function renderAc() {
      if (!ac) return;
      let html = "";
      for (let i = 0; i < ac.items.length; i++) {
        const it = ac.items[i];
        const label = esc(it.show === undefined ? it.text : it.show);
        // A row saying what it IS takes the slot a count would have used: both
        // annotate the offer, and neither has anything to say beside the other.
        html += `<div class="tv-ac-item${it.dim ? " tv-ac-dim" : ""}`
              + `${i === acAt ? " tv-ac-on" : ""}" data-i="${i}">`
              + `<span class="tv-ac-label">${label}</span>`
              + (it.aside ? `<span class="tv-ac-aside">${esc(it.aside)}</span>`
                          : it.count < 0 ? "" : `<span class="tv-ac-n">${it.count}</span>`)
              + `</div>`;
      }
      // Where the browser eats C-n before the page can see it, say so rather
      // than leaving two of the four documented keys silently dead. Only there,
      // and only while there is a list for them to have moved.
      if (swallowsCtrlN())
        html += `<div class="tv-ac-note">C-n/C-p need Firefox/webview`
              + ` — arrows/Tab work everywhere</div>`;
      acEl.innerHTML = html;
      acEl.style.display = "";
    }

    /** Offer what the caret is asking for, or close when that is nothing. */
    function openAc() {
      const st = stageAt();
      if (!st) { closeAc(); return; }
      const items = suggestFor(st);
      if (!items.length) { closeAc(); return; }
      ac = { stage: st.stage, tok: st.tok, items };
      // ROW ONE IS ALWAYS THE CHOICE. A list that has something to offer offers
      // a best guess, so Enter takes it and the common case costs no arrow;
      // `suggestFor' is what makes that honest, leading with what the word
      // spells in full. The literal stays reachable through the grammar rather
      // than through a second meaning for Enter: a quoted token asks for no
      // suggestions at all, and Escape puts the list away before Enter commits
      // what is written.
      acAt = 0;
      renderAc();
    }

    function moveAc(step) {
      if (!ac) return;
      const n = ac.items.length;
      acAt = (acAt + step + n) % n;
      renderAc();
    }

    /**
     * Put TEXT in place of the token under the caret, leaving the rest of the
     * box alone. A key lands as `key:' with the caret against the colon, ready
     * for the value; a value lands with a trailing space, ready for the next
     * token. Focus stays in the box either way.
     *
     * A value keeps everything through the token's LAST bar, so completing
     * inside an alternation appends one more alternative and the token stays
     * one token; a SORT segment keeps everything through the last ARROW, which
     * is the same rule over the same shape and is what makes a chain one token
     * too. Both are looked for in the RAW text rather than in the token's value,
     * which has had its quotes taken out and no longer lines up with the box.
     */
    function acceptAc(item) {
      if (!ac) return;
      const stage = ac.stage;
      const v = input.value, t = ac.tok;
      const bar = v.lastIndexOf(ALT, t.end - 1);
      const arrow = ac.stage === "sort" ? v.lastIndexOf(SORT_ARROW, t.end - 1) : -1;
      // The comma is the columns stage's own re-opener, the arrow's twin: the
      // accepted name replaces the segment being typed and the set ahead of it
      // stands.
      const comma = ac.stage === "columns" ? v.lastIndexOf(",", t.end - 1) : -1;
      const head = ac.stage === "key" ? (t.negated ? "-" : "")
        : v.slice(t.start, Math.max(t.sep + 1, bar + 1, comma + 1,
                                    arrow === -1 ? 0 : arrow + SORT_ARROW.length));
      const ins = head + item.text + (item.full || ac.stage === "value" ? " " : "");
      input.value = v.slice(0, t.start) + ins + v.slice(t.end);
      const caret = t.start + ins.length;
      if (input.setSelectionRange) input.setSelectionRange(caret, caret);
      armFilter();
      // A VIEW IS THE WHOLE ANSWER, so the pick is the commit: what a name holds
      // replaces the query rather than narrowing it, and there is nothing left
      // to type.  Every other stage leaves the box open for the next token.
      if (stage === "view") { flushFilter(true); handOver(); return; }
      openAc();          // a key opens its values; a finished value closes the list
    }

    acEl.addEventListener("mousedown", (e) => e.preventDefault());   // the box keeps focus
    acEl.addEventListener("click", (e) => {
      const t = hit(e);
      const item = t && /** @type {HTMLElement|null} */ (t.closest(".tv-ac-item"));
      if (item && ac) acceptAc(ac.items[Number(item.dataset.i)]);
    });
    input.addEventListener("blur", closeAc);

    /**
     * Apply the box now, cancelling whatever the debounce still owes — so the
     * query reaches the producer (or the local filter) exactly once, with the
     * text as it stands rather than as it stood a keystroke ago.
     */
    function flushFilter(all) {
      if (debounce) { clearTimeout(debounce); debounce = 0; }
      chipUp(all);
      deliver();                         // synchronous: Enter waits for no frame
    }

    // Enter applies and hands the table over; Escape clears and steps out of
    // the box. Both are the input's own keys — they are stopped here rather
    // than left to bubble into a consumer's document-level keymap, the way a
    // focused text field claims Enter anywhere else on the web. Nothing else
    // touches focus or the selection: a debounce firing on its own leaves both
    // exactly where the typist left them.
    //
    // The suggestion list gets first refusal, which is SCHEMA.md's precedence:
    // arrows and Tab drive it, Enter accepts a suggestion rather than applying
    // the filter, and Esc dismisses the list "before it clears anything" — so
    // the first Esc closes the list and the second does what it does here.
    input.addEventListener("keydown", (e) => {
      if (ac) {
        // C-n and C-p move the list too, while it is open and the box has the
        // keyboard. Both editors' users reach for them here — the Emacs
        // minibuffer and vim's insert-mode completion agree — so neither
        // profile has to ask. Platform reality, stated rather than wished
        // away: Chrome-family browsers take C-n for a new window before the
        // page ever sees it, so the arrows are the fallback there; Firefox and
        // system-webview shells deliver both. Outside an open list these keys
        // are left alone — they stay the browser's, and the table's keymap
        // reserves them.
        const down = e.key === "ArrowDown" || (e.ctrlKey && e.key === "n");
        const up = e.key === "ArrowUp" || (e.ctrlKey && e.key === "p");
        const accepts = e.key === "Tab" || e.key === "Enter";
        if (down || up || accepts || e.key === "Escape") {
          e.preventDefault();
          e.stopPropagation();
          if (down) { moveAc(1); return; }
          if (up) { moveAc(-1); return; }
          if (e.key === "Escape") { closeAc(); return; }
          const taken = ac.items[acAt];
          // Whether this accept finishes the token — the same question the
          // trailing space answers.
          const finished = taken.full || ac.stage === "value";
          acceptAc(taken);
          // Tab always leaves the caret where more can be typed. Enter does too
          // when what it completed was a key: the token is `key:' now, the
          // value is the next thing to choose, and `acceptAc' has already
          // opened the list of them. Only a finished token sends Enter on to
          // commit, which is the gesture it is with no list at all.
          if (e.key === "Tab" || !finished) return;
          closeAc();
        }
        // Every other key falls through to what it means with no list at all.
      }
      // Backspace walks the query down, and how far depends on where the box
      // is. On the page it is the last rung of the ladder Enter ends on: the
      // browser eats the characters, this takes the chips off one at a time,
      // and with none left it hands the table over. In the palette it goes no
      // further than the characters — the chips are elsewhere, on the page
      // behind the overlay, and a key cannot reach past what it is editing.
      if (e.key === "Backspace" && !input.value) {
        e.preventDefault();
        e.stopPropagation();
        // One press, one part. Held down, the browser's repeat deletes the
        // typed characters and then stops here — taking a chip off is a
        // decision, and a row of them should not vanish under a resting finger.
        if (e.repeat) return;
        // In the palette the applied parts are not this key's to take. It edits
        // what is typed and nothing else, and with nothing typed it does
        // nothing at all — a chip is removed by its own click, or by the key
        // the consumer binds over the table, where the chips are on show.
        if (palette) return;
        if (chips.length) dropChip(chips.length - 1);
        else handOver();
        return;
      }
      if (e.key !== "Enter" && e.key !== "Escape") return;
      e.preventDefault();               // and, for Escape, the native search-box clear
      e.stopPropagation();
      if (e.key === "Escape") {
        // Escape walks out one step at a time: the half-typed token first, the
        // box's focus only once there is nothing left in it to drop.
        if (input.value) { input.value = ""; closeAc(); deliver(); }
        else closeFilter();
        return;
      }
      // Enter commits whatever is typed and hands the table back, every time.
      // A longer query is built by coming back for it: the consumer's key for
      // the filter box refocuses an empty box with the chips still standing,
      // and the next token joins them.
      if (input.value.trim()) {
        flushFilter(true);              // `chipUp' reads the box, then empties it
      } else {
        input.value = "";               // stray whitespace is nothing to commit
        // Nothing to commit, but a debounce may still owe a change — text typed
        // and then deleted again. Settle it here rather than dropping it.
        if (debounce) { clearTimeout(debounce); debounce = 0; deliver(); }
      }
      handOver();
    });

    // The backdrop is the palette's own Escape: clicking off it puts it away.
    veil.addEventListener("mousedown", (e) => {
      if (hit(e) === veil) { e.preventDefault(); closeFilter(); }
    });

    // The pager is two words in the status line rather than a control of its
    // own; the keys belong to the consumer, and these are for the pointer.
    hint.addEventListener("click", (e) => {
      const t = hit(e);
      const step = t && /** @type {HTMLElement|null} */ (t.closest(".tv-pg"));
      if (!step || step.classList.contains("tv-pg-off")) return;
      turnTo(cursorPage() + Number(step.dataset.pg),
             Number(step.dataset.pg) > 0 ? "first" : "last");
    });

    /** Take chip AT off and re-run what is left. */
    function dropChip(at) {
      chips.splice(at, 1);
      renderChips();
      deliver();
    }

    chipsEl.addEventListener("mousedown", (e) => e.preventDefault());   // box keeps focus
    chipsEl.addEventListener("click", (e) => {
      const t = hit(e);
      if (t && t.closest(".tv-pin")) { if (onPin) onPin(); return; }
      const chip = t && /** @type {HTMLElement|null} */ (t.closest(".tv-chip"));
      // A crumb wears the chip's shape and carries no index, so it lands here
      // and has nothing to drop. Without the guard the index reads NaN and
      // `splice' takes it for zero, which is the FIRST live chip.
      if (!chip || chip.dataset.i === undefined) return;
      dropChip(Number(chip.dataset.i));
    });

    // ---- streaming ---------------------------------------------------------

    /**
     * Move ROW to where it now belongs in the cached list ARR, or leave it out
     * when FILTERED and the query excludes it. No re-sort, no re-filter.
     * @param {Row[]} arr  @param {Row} row  @param {boolean} filtered
     */
    function place(arr, row, filtered) {
      const at = arr.findIndex((r) => r.id === row.id);
      if (at !== -1) arr.splice(at, 1);
      if (filtered && !matches(row)) return;
      if (!orderCmp) {                     // unsorted: mirror the store's order
        if (at === -1) arr.push(row); else arr.splice(at, 0, row);
        return;
      }
      let lo = 0, hi = arr.length;         // after its equals, like a stable sort
      while (lo < hi) {
        const mid = (lo + hi) >> 1;
        if (orderCmp(row, arr[mid]) < 0) hi = mid; else lo = mid + 1;
      }
      arr.splice(lo, 0, row);
    }

    /** Drop the row with ID from the cached list ARR. @param {Row[]} arr */
    function unplace(arr, id) {
      const at = arr.findIndex((r) => r.id === id);
      if (at !== -1) arr.splice(at, 1);
    }

    // A query the consumer is restoring: chips as an Enter commit would leave
    // them, and nothing delivered. Remounting is how a consumer puts state
    // back — after a reconnect, a view change, a `?q=' load — and without this
    // the only way in is `input.value', which the first commit then chips a
    // second time while the chips it already had go missing.  `setQuery' is
    // the same seeding as a call, for a consumer re-showing a mount it kept —
    // a composer reopened over a value that moved — and it delivers nothing
    // for the same reason.
    function seedQuery(q) {
      chips.length = 0;
      if (typeof q === "string" && q.trim())
        for (const t of parseQuery(q, queryKeys())) pushChip(q.slice(t.start, t.end));
      else renderChips();
      lastQuery = effectiveQuery();
      // Local filtering has to catch up to it; a producer has already filtered.
      if (!o.onFilter) state.filter = lastQuery;
      // The order it names, in force before the first paint: a `?q=' carrying
      // `sort:' opens in that order rather than in the declared one.
      state.sortKeys = chainFor(lastQuery);
    }
    if (typeof o.initialQuery === "string" && o.initialQuery.trim())
      seedQuery(o.initialQuery);

    // Whether or not anything was restored: one function decides what the chip
    // row shows, including that it shows nothing. Stamping the collapsed state
    // at creation as well left two places to agree about it, and a mount that
    // never called this one was a mount whose row was collapsed by the other —
    // near enough until the two drift, which is the sort of thing that goes
    // unnoticed because the checks drive the function the mount skipped.
    renderChips();

    titleEl.textContent = state.view.title || "Table";
    renderHead();
    renderRows(true);
    queueIndex();

    // A theme flip changes what a badge colour has to become to stay legible,
    // and the ink is baked into the row HTML — so redraw when the scheme moves
    // under us, whether the page asked for it or the system did.
    function onTheme() {
      const now = darkNow();
      if (now === dark) return;
      dark = now;
      renderRows(true);
    }
    if (typeof matchMedia === "function") {
      const q = matchMedia("(prefers-color-scheme: dark)");
      if (q.addEventListener) q.addEventListener("change", onTheme);
    }
    if (typeof MutationObserver === "function" && document.documentElement)
      new MutationObserver(onTheme).observe(document.documentElement,
                                            { attributes: true, attributeFilter: ["data-theme"] });

    return {
      el: root,
      /** @param {View} v */
      setView(v) {
        state.view = v || { columns: [] };
        state.rows = (v && v.rows) ? v.rows.slice() : [];
        stated = normalizeSort(v && v.sort);
        state.sortKeys = stated;
        state.selected = null;
        state.selCol = null;
        state.filter = "";
        markSet.ids.clear();     // a different view; these were about the last one
        flagSet.ids.clear();
        crumbs = [];             // and the trail was a path through it
        chips = [];
        input.value = "";
        renderChips();
        clearTexts();
        dropSorted();
        titleEl.textContent = state.view.title || "Table";
        renderHead();
        scroll.scrollTop = 0;
        renderRows(true);
      },
      /**
       * Replace every row. Marks are deliberately NOT pruned against the new
       * set: a producer that filters server-side answers a narrowed query
       * through here, so an id that did not come back is a row being hidden
       * rather than a row being deleted, and it has to still be marked when the
       * filter comes off. A delta's `reset' is the same op and keeps them for
       * the same reason; `deleteRow' and a delta's `delete' are the ones that
       * say a row is gone, and those do drop it.
       * @param {Row[]} rows
       */
      setRows(rows) {
        state.rows = (rows || []).slice();
        clearTexts();
        dropSorted();
        // The presentation resets — a new set is not the set the seam was
        // crossed in — but `page' is left to CLAMP rather than reset, which is
        // what keeps a reader near where they were when rows go away.
        continuous = false;
        renderRows(true);
      },
      /** @param {Row} row */
      upsertRow(row) {
        const i = state.rows.findIndex((r) => r.id === row.id);
        if (i === -1) state.rows.push(row); else state.rows[i] = row;
        texts.delete(row.id);
        dropDomains();
        if (sorted) place(sorted, row, false);
        // Unsorted, `order' is `sorted' filtered, and a row the filter has just
        // started matching has no place to be spliced into: re-derive it (one
        // linear pass, no sort).
        if (order && orderCmp) { place(order, row, true); growWidths(row); }
        else if (order) dropOrder();
        renderRows(true);
      },
      /** @param {string} id */
      deleteRow(id) {
        state.rows = state.rows.filter((r) => r.id !== id);
        markSet.ids.delete(id);  // the row is gone; a mark on it would outlive it
        flagSet.ids.delete(id);
        texts.delete(id);
        dropDomains();
        if (sorted) unplace(sorted, id);
        if (order) unplace(order, id);
        renderRows(true);                               // which keeps the place
      },
      /** @param {Op[]} ops */
      applyDelta(ops) {
        for (const op of ops || []) {
          if (op.op === "reset") {
            state.rows = (op.rows || []).slice();
            clearTexts();
            dropSorted();
            continue;
          }
          // SCHEMA counts delta indices in the window, which is the order the
          // rows are displayed in; with no local sort, filter or page that is
          // the store's own order, so the mapping costs nothing there.  Each
          // op is placed against the window the ops before it left behind.
          const win = paged();
          const store = (row) => state.rows.findIndex((r) => r.id === row.id);
          if (op.op === "insert") {
            const at = op.index < win.length ? store(win[op.index]) : -1;
            state.rows.splice(at === -1 ? state.rows.length : at, 0, op.row);
            texts.delete(op.row.id);
          } else if (op.op === "delete") {
            const gone = win[op.index];
            const at = gone ? store(gone) : -1;
            if (at !== -1) {
              texts.delete(gone.id);
              markSet.ids.delete(gone.id);  // as `deleteRow': the row is gone
              flagSet.ids.delete(gone.id);
              state.rows.splice(at, 1);
            }
          }
          dropSorted();
        }
        dropDomains();
        renderRows(true);
      },
      getRows() { return state.rows.slice(); },
      /**
       * The rows on show as a reader would name them: one page of the
       * filtered, sorted set, or all of it where no page size was asked for.
       * In CONTINUOUS presentation the window is over the whole set, but this
       * still answers the CURSOR's page — a consumer's buffer-end keys mean
       * the ends of the page it is looking at, and the pager says the same
       * page, so the two agree whichever way the rows were drawn.
       */
      getVisible() { return shownRows().slice(); },
      select: selectRow,
      /**
       * Where the selection is: the row's id and the column index within it,
       * `col' being null for a whole-row selection. A consumer moving the
       * selection reads this, adds a step, and hands it back to `select'.
       * @returns {{id: string|null, col: number|null}}
       */
      getSelection() { return { id: state.selected, col: state.selCol }; },
      /**
       * The query as last delivered: the chips and whatever had been committed
       * with them. What a consumer echoes, or writes into a URL.
       * @returns {string}
       */
      getQuery() { return lastQuery; },
      /**
       * Replace the crumb trail, oldest first. Anything that is not an object
       * is dropped; a missing `label' or `query' reads as "".
       * @param {Crumb[]} list
       */
      setCrumbs(list) {
        crumbs = [];
        for (const c of list || []) {
          const one = crumbOf(c);
          if (one) crumbs.push(one);
        }
        renderChips();
      },
      /**
       * The trail as it stands, oldest first — copies, so a consumer reading
       * it cannot move the strip by editing what it was handed.
       * @returns {Crumb[]}
       */
      getCrumbs() { return crumbs.map((c) => ({ label: c.label, query: c.query })); },
      // The badge is the consumer's boolean: only it knows what the applied
      // query is being measured against.
      setPinned(on) { pinned = !!on; renderChips(); },
      setQuery(q) { seedQuery(String(q == null ? "" : q)); },
      /**
       * Push one crumb on the end. What a consumer does as it drills IN.
       * @param {Crumb} c  @returns {number} how deep the trail is now
       */
      pushCrumb(c) {
        const one = crumbOf(c);
        if (one) { crumbs.push(one); renderChips(); }
        return crumbs.length;
      },
      /**
       * Take the last crumb off and hand it back, or null on an empty trail.
       * It is popped and NOT applied: whoever owns the fetching owns what a
       * query means, so a consumer walking out re-applies the `query' itself.
       * @returns {Crumb|null}
       */
      popCrumb() {
        if (!crumbs.length) return null;
        const gone = crumbs[crumbs.length - 1];
        crumbs.pop();
        renderChips();
        return gone;
      },
      stripLastToken,
      /**
       * Sort on COLUMN, ascending unless ASCENDING is false, replacing whatever
       * sort is in force. A header click TOGGLES; this states an order, so a
       * consumer applying a canned view lands on the same one every time. It
       * ignores `sortable', which gates what a READER may reach rather than
       * what a producer's own agent may ask for.
       * @param {string} column @param {boolean} [ascending]
       * @returns {boolean} false when no column carries that key
       */
      sortBy(column, ascending) { return sortTo(column, ascending !== false); },
      /**
       * `^': promote COLUMN to the head of the chain ascending, flipping it
       * where it already leads and dropping it from wherever it sat below.
       * Composing a chain is pressing this over columns in reverse priority
       * order. The new chain is written into the query as `sort:' tokens and
       * delivered, so a consumer narrowing server-side is asked for the order
       * it has just been told about. Gated by `sortable' — a reader's gesture,
       * where `sortBy' is a producer's.
       * @param {string} column @returns {boolean} whether the chain moved
       */
      sortPromote,
      /**
       * The chain in force, highest priority first, as copies. What a consumer
       * persists (a URL, a saved layout); `setSort' takes it back.
       * @returns {SortKey[]}
       */
      getSort() { return state.sortKeys.map((k) => Object.assign({}, k)); },
      /**
       * Replace the whole chain — SCHEMA's `sort' shape or `getSort''s. An
       * empty one leaves the rows unsorted, in the order they arrived, which
       * is the CLEAR a consumer binds when a reader wants the composition
       * undone. A producer's call, like `sortBy': it restates the view's order
       * and writes no query, so a query naming sort keys still outranks it.
       * @param {Sort|Sort[]|SortKey[]|null} [sort]
       */
      setSort(sort) { stated = normalizeSort(sort); applyChain(stated); },
      openFilter,
      closeFilter,
      selectStep,
      /** Turn forward a page, landing on its first row. @returns {boolean} */
      nextPage() { return turnTo(cursorPage() + 1, "first"); },
      /** Turn back a page, landing on its last. @returns {boolean} */
      previousPage() { return turnTo(cursorPage() - 1, "last"); },
      /**
       * Where the reading is: the page and how many there are, and the span of
       * the filtered set on show. Counted from one, the way it reads.
       * @returns {{page: number, pages: number, from: number, to: number, total: number}}
       */
      pageInfo,
      // The two row states, over one mechanism: a toggle, a listing, a clear
      // and a count each, plus the one operation apiece that only its own
      // state is used for.
      /** Mark ID, or unmark it. @param {string} id  @returns {boolean} its new state */
      toggleMark(id) { return markSet.toggle(id); },
      markAll,
      /** Take every mark off. Flags are a different question and stay. */
      clearMarks() { markSet.clear(); },
      /** The marked ids: those on show first, then the rest. @returns {string[]} */
      getMarked() { return markSet.list(); },
      /** How many rows are marked, the hidden ones counted. @returns {number} */
      markedCount() { return markSet.ids.size; },
      /** Flag ID, or unflag it. @param {string} id  @returns {boolean} its new state */
      flagRow(id) { return flagSet.toggle(id); },
      /** Take the flag off ID, whether or not it had one. @param {string} id */
      unflagRow(id) { flagSet.drop(id); },
      /** Take every flag off. Marks are a different question and stay. */
      clearFlags() { flagSet.clear(); },
      /** The flagged ids, ordered like `getMarked'. @returns {string[]} */
      getFlagged() { return flagSet.list(); },
      /** How many rows are flagged, the hidden ones counted. @returns {number} */
      flaggedCount() { return flagSet.ids.size; },
    };
  }

  const TableView = { mount, displayText, comparator, parseQuery };
  root.TableView = TableView;
  // @ts-ignore -- optional CommonJS export (no @types/node dependency)
  if (typeof module !== "undefined" && module.exports) module.exports = TableView;
})(typeof window !== "undefined" ? window : this);
