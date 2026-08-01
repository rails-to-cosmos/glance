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
 *   tv.getQuery();        // the query as last delivered
 *   tv.stripLastToken();  // drop the typed text, else the last chip -> bool
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
 *   guessed from cell shape. The comparator, the vocabulary, the virtual tag
 *   keys and the filter's AND/OR rule all read that one verdict.
 * - The chrome — bar, title, filter chips, filter input, table skeleton, hint —
 *   is built once at mount. Updates touch only the row window, the hint line,
 *   the sort arrows and the chips, so the filter input keeps focus and caret
 *   while typing.
 * - There are no toolbar buttons. Actions render on the hint line as `KEY
 *   label' pairs, the way table-view.el prints its legend: the keys are the
 *   interface, a consumer binds them and dispatches the command, and a button
 *   would only offer a second way to reach what a key already reaches.
 * - Selection is a row and, optionally, one cell of it: `select(id, col)' washes
 *   the whole column (`.tv-colsel' on every rendered td of it and on its th) and
 *   stamps `.tv-cell-sel' where that band crosses the cursor row, which is the
 *   crosshair; `getSelection()' reports both. The column is clamped to the ones
 *   that exist, never wrapped, and `select(id)' with no column is the whole-row
 *   selection this had before, with no band anywhere. Every class is re-derived
 *   from the same state on every render, so they survive a scroll, an upsert and
 *   a `setRows' that still carries the id.
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
 * - Flags ride `marks: true' — one chrome opt-in covers both state sets, since
 *   the leading box column is where either of them is read. A flag is a
 *   PENDING action (a consumer's two-press `d', say) where a mark is a
 *   standing selection, so they are separate id-keyed sets: a row can carry
 *   both, `clearMarks' leaves flags alone and `clearFlags' leaves marks alone,
 *   and a consumer that wants both gone asks for both. They survive and die
 *   together otherwise — a filter, a page, a sort or a `setRows' keeps them;
 *   the row going away, or the view, takes them.
 * - `marks: true' adds dired's row marking, and a fourth row ground with it.
 *   The chrome is a leading checkbox column — presentation like the pager, so
 *   the cells and columns a producer sends are untouched and SCHEMA.md keeps
 *   calling marking renderer-local. Its header is blank, its box is org's own
 *   `[ ]'/`[X]' drawn from the row's class, and a click on it toggles that row
 *   without moving the selection. Marks are id-keyed, so they outlive
 *   `setRows', an upsert, a filter, a page flip and a sort; `deleteRow' and a
 *   delta's delete drop the mark with the row, and `setView' drops all of them
 *   with the view. One predicate gates the class, the box and the count, so
 *   without the option there is nothing to hide rather than something hidden.
 *   Why the ground is what it is: the CSS rule, `tr.tv-marked'.
 * - Rows are virtualized. `tbody` holds the scrolled-to window plus ~15 rows of
 *   overscan, between two spacer rows standing in for the height of the rest.
 *   Rows outside the window have no DOM: drive selection with `select(id)`
 *   rather than by reaching for row elements. Zebra striping comes from a class
 *   stamped from the row's global index (`:nth-child` cannot see past the
 *   window). Column widths come from the widest cell in the filtered set, in
 *   `ch` — the renderer's font is monospace — so they hold still while
 *   scrolling.
 * - Row and header events are delegated from the scroll container, attached
 *   once. `tr.click()` still selects a rendered row.
 * - Filter input is debounced 120ms; the row window renders on a rAF. With an
 *   `onFilter' option the debounced query goes to the producer instead and the
 *   rows given are the rows shown — no local narrowing.
 * - The filter box speaks SCHEMA.md's query micro-syntax: `key:value' field
 *   predicates (only where `key' names a column, so `:work:' stays org text),
 *   `"quoted text"', `-negation', everything else free text. Predicates
 *   sharing a key group by the field's arity: a single-valued one ORs (a row
 *   has one state), a multi-valued one ANDs (a row carries several tags, so
 *   `tag:a tag:b' is a row with both). Distinct keys, free text and negations
 *   AND. A column counts as multi-valued when its cells hold delimited lists —
 *   decided by their shape, never by the column's name.
 *   `TableView.parseQuery' is the tokenizer, exported so a consumer can
 *   highlight the box and a producer can implement the same grammar. Filtering
 *   locally applies the parsed query; with `onFilter' the raw text goes to the
 *   producer and the grammar is its business.
 * - Besides the view's columns, a key may be one the rows imply: SCHEMA calls
 *   these virtual keys, and the one derivation a producer and a renderer can
 *   both arrive at is org's — every distinct tag in the `tags' column is a key,
 *   so `contact:tanik' is tagged `contact' and matching `tanik'. Membership is
 *   whole-tag (`con:' is not `:contact:'), an empty value is presence alone, and
 *   a column of the same name shadows the tag.
 * - A suggestion list under the box completes it. A bare word offers, in order:
 *   the column keys it opens; the columns whose declared domain holds it as a
 *   value (`TODO' → `state:TODO'); and, only when nothing exact was found, up
 *   to five tags whose rows merely contain it, dimmed. Exact beats fuzzy and
 *   fuzzy never crowds — a scoped count is a substring count and must not dress
 *   like a value match. After `key:' comes that column's value domain
 *   (`values', else the badge palette, else the distinct cell values), each
 *   with the number of rows behind it; a virtual key has no domain to offer.
 *   Arrows — and C-n/C-p, which both editors' users reach for here — move it,
 *   Esc dismisses, and a click accepts without taking focus. Tab completes and
 *   stays, at either stage. Enter is stage-aware: completing a key leaves the
 *   caret past the colon with that key's values already listed, since `tag:'
 *   is half a predicate and the values are the next thing to choose; only a
 *   finished token sends it on to commit and hand over. Nothing at the value
 *   stage starts highlighted, so Enter with `tag:' typed and no value chosen
 *   commits the presence predicate that was written rather than whichever
 *   value happened to sort first.
 *   Only a column completion starts highlighted, so Enter still commits the
 *   word as typed and an arrow is how you step into the offers.
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
 * @typedef {{ column: string, ascending?: boolean, direction?: string }} Sort
 * @typedef {{ column: string, ascending: boolean, nullsFirst: boolean }} SortKey
 *          A normalized sort key (internal).
 * @typedef {{ id: string, cells?: Record<string, Cell> }} Row
 *          draws it and every other mode ignores it.
 * @typedef {{ title?: string,
 *             columns: Column[],
 *             actions?: Action[],
 *             sort?: Sort | Sort[],
 *             rows?: Row[] }} View
 * @typedef {{ op: "insert", index: number, row: Row }
 *        | { op: "delete", index: number }
 *        | { op: "reset", rows: Row[] }} Op
 * @typedef {{ onAction?: (command: string, id: string, row: Row) => void,
 *             onLink?: (target: string, row: Row | null) => void,
 *             onFilter?: (q: string) => void,
 *             omnibox?: boolean,
 *             palette?: boolean,
 *             marks?: boolean,
 *             actionHints?: boolean,
 *             flagHelp?: string,
 *             pageSize?: number,
 *             initialQuery?: string }} MountOptions
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
 *             stripLastToken: () => boolean,
 *             openFilter: () => void,
 *             closeFilter: () => void,
 *             selectStep: (step: number) => boolean,
 *             nextPage: () => boolean,
 *             previousPage: () => boolean,
 *             pageInfo: () => { page: number, pages: number,
 *                               from: number, to: number, total: number },
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
   * Punctuation a word wears in prose and never in a query. Stripped from both
   * edges of every indexed word and of every prefix matched against them, so
   * the two forms agree — and, colons being among them, so that no title word
   * can compose a suggestion that reads like a tag it is not. "Lisp:" indexes
   * as lisp; a colon in a suggestion comes from a real tag or from nowhere.
   * Interior punctuation stays: hyphens and underscores are part of a word.
   */
  const EDGES = /^[:,.;!?"'()[\]{}]+|[:,.;!?"'()[\]{}]+$/g;
  const bareWord = (w) => w.replace(EDGES, "");

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
  /** @param {Column} col  @returns {string[]|null} */
  function valueOrder(col) {
    if (col.values) return col.values.map(String);
    if (col.type === "badge") return (col.badges || []).map((b) => String(b.value));
    return null;
  }

  /** A producer meta-value, which SCHEMA spells `*active*'. */
  const META = /^\*.+\*$/;

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
  const ROW_H = 30;            // row height until a rendered row can be measured
  const CELL_PAD = 24;         // a cell's horizontal padding, both sides
  const PILL_CH = 2;           // a badge pill's ground, in characters
  const DEBOUNCE = 120;        // ms of quiet before a filter keystroke re-renders
  const SETTLE = 200;          // ms of quiet before the rows are taken to have settled
  const LONG_PRESS = 500;      // ms of a still finger before it means the row action
  const PRESS_SLOP = 10;       // px of drift that makes it a scroll instead
  const EASE = 0.3;            // fraction of the remaining scroll covered per frame
  const SNAP_PX = 0.5;         // closer than this and the ease is over

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
    const css = `
/* Both palettes are danneskjold-theme's, mapped role for role from
   /home/akatovda/sync/stuff/danneskjold-theme/danneskjold-theme.el — its
   default faces for dark, its light-* block for light. Three values are
   lightness-only adjustments where the theme's own colour missed a contrast
   floor in this context, the hue held: light muted #7F8C8D -> #667071 (3.5:1
   -> 5.1:1 on white) and light accent #4CB5F5 -> #31769F (2.3:1 -> 5.0:1, it
   is link text here). The selected row takes the theme's highlight face
   (#F0FFF0) rather than its golden: the cursor row is its own role and must
   not read as either the applied filter (frost, on the chips) or a state
   pill.

   Borders are the exception and stay hairlines: they carry no information, so
   contrast is not a goal for them and a visible rule only adds noise. Light
   keeps the quiet #E3E6EA (1.25:1 on white) rather than the theme's #BDC3C7,
   and dark takes #2a2d3d over the theme's #223959 (1.54:1 against true black
   against 1.80:1) — the quieter of the two. Every rule is 1px. */
.tv-root{--tv-fg:#000000;--tv-muted:#667071;--tv-bg:#FFFFFF;--tv-alt:#F8F8FF;
  --tv-border:#E3E6EA;--tv-accent:#31769F;--tv-sel:#F0FFF0;--tv-hover:#FAFAFA;
  --tv-frost:${FROST};--tv-chip-wash:45%;--tv-chip-edge:95%;--tv-mark-wash:8%;
  --tv-flag:${FLAG};--tv-flag-wash:8%;
  --tv-col:${COL};--tv-col-wash:35%;--tv-cell-wash:60%;
  color:var(--tv-fg);background:var(--tv-bg);font:13px/1.5 ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
  border:1px solid var(--tv-border);border-radius:8px;overflow:hidden;display:flex;flex-direction:column;max-height:100%}
@media (prefers-color-scheme:dark){.tv-root{--tv-fg:#FFFFFF;--tv-muted:#A4C2EB;--tv-bg:#000000;
  --tv-alt:#21252B;--tv-border:#2a2d3d;--tv-accent:#4CB5F5;--tv-sel:#373D4F;
  --tv-hover:#1F1F1F;--tv-chip-wash:18%;--tv-chip-edge:34%;--tv-mark-wash:30%;--tv-flag-wash:30%;
  --tv-col-wash:8%;--tv-cell-wash:9%;}}
:root[data-theme="dark"] .tv-root{--tv-fg:#FFFFFF;--tv-muted:#A4C2EB;--tv-bg:#000000;
  --tv-alt:#21252B;--tv-border:#2a2d3d;--tv-accent:#4CB5F5;--tv-sel:#373D4F;
  --tv-hover:#1F1F1F;--tv-chip-wash:18%;--tv-chip-edge:34%;--tv-mark-wash:30%;--tv-flag-wash:30%;
  --tv-col-wash:8%;--tv-cell-wash:9%;}
:root[data-theme="light"] .tv-root{--tv-fg:#000000;--tv-muted:#667071;--tv-bg:#FFFFFF;--tv-alt:#F8F8FF;
  --tv-border:#E3E6EA;--tv-accent:#31769F;--tv-sel:#F0FFF0;--tv-hover:#FAFAFA;
  --tv-chip-wash:45%;--tv-chip-edge:95%;--tv-mark-wash:8%;--tv-flag-wash:8%;
  --tv-col-wash:35%;--tv-cell-wash:60%}
.tv-bar{display:flex;align-items:center;gap:10px;padding:8px 12px;border-bottom:1px solid var(--tv-border);flex-wrap:wrap}
.tv-title{font-weight:600;font-size:14px;margin-right:auto}
.tv-filter{font:inherit;padding:4px 8px;border:1px solid var(--tv-border);border-radius:6px;
  background:var(--tv-bg);color:var(--tv-fg);min-width:140px}
/* Quiet enough to be read past, not so quiet it cannot be read. Firefox dims
   placeholders on top of the colour, which is what the opacity is undoing. */
.tv-filter::placeholder{color:var(--tv-muted);opacity:1}
.tv-filter-wrap{position:relative;display:flex}
/* Omnibox: the filter is the bar's one control, and it takes the width the
   title was holding. The dropdown hangs under the whole of it. */
.tv-omni .tv-bar{gap:8px;padding:10px 12px}
.tv-omni .tv-filter-wrap{flex:1 1 auto}
.tv-omni .tv-filter{flex:1 1 auto;font-size:15px;padding:7px 11px}
/* Its own row under the box, and no gap at all when nothing is applied. The
   suggestion list is positioned and z-indexed, so it lays over this rather
   than being pushed down by it. */
.tv-omni > .tv-chips,.tv-pal > .tv-chips{padding:8px 12px;
  border-bottom:1px solid var(--tv-border)}
/* Palette: the control is summoned, not resident. The veil dims the page and
   the panel sits in the upper third, where a minibuffer or a Telescope prompt
   sits — near the eye rather than centred in it. 90/91 leaves 100/101 free for
   a consumer's own modal, so a materialize sheet still wins over this. */
.tv-veil{position:fixed;inset:0;z-index:90;background:#0006;
  display:flex;justify-content:center;align-items:flex-start;padding-top:18vh}
.tv-panel{z-index:91;width:min(560px,80vw);padding:10px;border-radius:8px;
  background:var(--tv-alt);border:1px solid var(--tv-border);
  box-shadow:0 10px 30px #0007}
.tv-panel .tv-filter{font-size:15px;padding:7px 11px;width:100%}
/* The applied filter's identity: the theme's frost, washed over whatever the
   page's ground is, with ordinary foreground for ink. Why frost and why a wash
   rather than the solid it was: CHANGELOG, "chips are a frost wash". */
.tv-pal .tv-chip{color:var(--tv-fg);
  background:color-mix(in srgb,var(--tv-frost) var(--tv-chip-wash),transparent);
  border-color:color-mix(in srgb,var(--tv-frost) var(--tv-chip-edge),transparent)}
.tv-pal .tv-chip:hover{border-color:var(--tv-accent);color:var(--tv-accent)}
.tv-chips{display:flex;flex-wrap:wrap;gap:5px;align-items:center}
.tv-chip{display:inline-flex;align-items:center;gap:5px;padding:1px 4px 1px 8px;
  border-radius:999px;font-size:12px;cursor:pointer;color:var(--tv-fg);
  border:1px solid var(--tv-border);background:var(--tv-alt)}
.tv-chip:hover{border-color:var(--tv-accent);color:var(--tv-accent)}
.tv-chip-x{font-style:normal;opacity:.55;padding:0 3px}
.tv-chip:hover .tv-chip-x{opacity:1}
/* The suggestion list hangs under the box, over the table. .tv-root clips with
   overflow:hidden, so it scrolls internally rather than growing past it. */
.tv-ac{position:absolute;top:100%;left:0;min-width:100%;z-index:5;margin-top:2px;
  max-height:min(288px,40vh);overflow-y:auto;background:var(--tv-bg);
  border:1px solid var(--tv-border);border-radius:6px;box-shadow:0 4px 12px #0003}
.tv-ac-item{display:flex;justify-content:space-between;align-items:baseline;gap:14px;
  padding:3px 10px;white-space:nowrap;cursor:pointer;color:var(--tv-fg)}
.tv-ac-n{color:var(--tv-muted);font-variant-numeric:tabular-nums}
/* A scoped tag is a substring count, and reads as one. */
.tv-ac-dim{opacity:.6;font-style:italic}
.tv-ac-note{padding:5px 10px;border-top:1px solid var(--tv-border);
  color:var(--tv-muted);font-size:11px;white-space:nowrap}
.tv-ac-item:hover{background:var(--tv-hover);color:var(--tv-accent)}
/* The theme's own selections (ivy-current-match, company-tooltip-selection)
   are full-strength golden with bold weight and the default foreground — an
   accent-coloured label on that ground would be unreadable. */
.tv-ac-on{background:var(--tv-sel);color:var(--tv-fg);font-weight:600}
.tv-scroll{overflow:auto;position:relative}
.tv-table{border-collapse:collapse;width:100%}
.tv-table th,.tv-table td{padding:5px 12px;text-align:left;white-space:nowrap;
  border-bottom:1px solid var(--tv-border)}
.tv-table th{position:sticky;top:0;background:var(--tv-bg);font-weight:600;color:var(--tv-muted);
  user-select:none;z-index:1}
.tv-table th.tv-sortable{cursor:pointer}
.tv-table th.tv-sortable:hover{color:var(--tv-accent)}
.tv-table td.tv-right,.tv-table th.tv-right{text-align:right;font-variant-numeric:tabular-nums}
.tv-table tbody tr.tv-alt{background:var(--tv-alt)}
/* A marked row's ground: the muted ink washed over the page's. It REPLACES the
   zebra rather than layering over it — one background slot, and a mark outranks
   a stripe — and it is neither of the washes that already say something, frost
   being the applied filter and --tv-sel the cursor. The cursor's rule follows
   this one, so a row that is both reads as the cursor and keeps its checked
   box. Faint because the floor binds: the tag ink is --tv-muted too, so each
   theme washes only as far as that ink stays above 4.5:1 on it (light 4.6,
   dark 6.3). */
.tv-table tbody tr.tv-marked{
  background:color-mix(in srgb,var(--tv-muted) var(--tv-mark-wash),transparent)}
/* A flagged row: the flag red washed over the page's ground, the same one-slot
   rule the mark follows. It sits between them in source order, which IS the
   precedence — cursor over flag over mark over zebra — because all four write
   the one background slot at the one specificity. Washed as far as the ink
   allows and no further, which for a colour this dark is not far on white:
   --tv-muted is the tag ink and the light strength is what keeps it above
   4.5:1 (4.6 at 8%, and under the floor by 10%). Dark has the room to take
   30%. The two numbers are measured, not chosen. */
.tv-table tbody tr.tv-flagged{
  background:color-mix(in srgb,var(--tv-flag) var(--tv-flag-wash),transparent)}
.tv-table tbody tr.tv-sel{background:var(--tv-sel)}
/* The background is one slot and the cursor wins it, so a flagged row under
   the cursor would otherwise stop saying it is flagged. The edge is a second
   channel that no other state writes: it survives every combination, which is
   what keeps the state readable rather than merely painted. */
.tv-table tbody tr.tv-flagged td.tv-box{box-shadow:inset 3px 0 0 var(--tv-flag)}
/* The mark column is chrome, the way the pager is: a fixed leading box that no
   producer sent and no width measurement sees. Blank header, org's own checkbox
   for a cell, and the box brightens on the rows it is checked on. The glyph is
   drawn from the row's class rather than written into the cell, so the state
   has one home: the class the row already carries. */
.tv-table th.tv-box,.tv-table td.tv-box{width:3ch;color:var(--tv-muted);user-select:none}
.tv-table td.tv-box{cursor:pointer}
.tv-table td.tv-box::before{content:"[ ]"}
.tv-table tbody tr.tv-marked td.tv-box{color:var(--tv-fg)}
.tv-table tbody tr.tv-marked td.tv-box::before{content:"[X]"}
/* The selection is the row, and it crossfades in place — no overlay to keep in
   step with the rows underneath it. */
.tv-table tbody tr,.tv-table tbody td{transition:background-color .08s ease-out,
  box-shadow .08s ease-out}
.tv-calm .tv-table tbody tr,.tv-calm .tv-table tbody td{transition:none}
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
.tv-table th.tv-colsel{background:color-mix(in srgb,var(--tv-col) var(--tv-col-wash),var(--tv-bg))}
.tv-table tbody td.tv-colsel{background:color-mix(in srgb,var(--tv-col) var(--tv-col-wash),transparent)}
.tv-table tbody td.tv-cell-sel{background:color-mix(in srgb,var(--tv-col) var(--tv-cell-wash),transparent)}
.tv-table tbody tr{cursor:default}
.tv-table tbody tr.tv-pad td{padding:0;border:0}
/* The third role, and the quietest: no box at all. A filled pill is a state, a
   frost chip is an applied filter, and a tag is small muted text — which is
   what a tag is, a word the row happens to carry. Several of them separate on a
   middot rather than on the colons the cell spells them with; the colons are
   the storage, not the reading. The ink is the muted one the palette already
   carries (dark #A4C2EB, light #667071), both clear of the text floor. */
.tv-tag,.tv-tags{color:var(--tv-muted);font-size:.92em}
.tv-tags .tv-tag{font-size:inherit;color:inherit}   /* never compound the two */
/* Shown in the form a query spells them, so what is read is what is typed: the
   vocabulary lowercases, and a key typed in any other case is free text. Done
   in the stylesheet rather than in the markup, so the text a copy takes is the
   text the file holds. */
.tv-tag{text-transform:lowercase}
.tv-pill{display:inline-block;padding:0 8px;border-radius:999px;
  font-weight:600;color:var(--tv-ink,var(--tv-badge));
  background:color-mix(in srgb,var(--tv-badge) 15%,transparent)}
.tv-link{color:var(--tv-accent);text-decoration:underline}
.tv-arrow{margin-left:4px;opacity:.7}
.tv-empty{padding:16px 12px;color:var(--tv-muted)}
.tv-hint{padding:6px 12px;border-top:1px solid var(--tv-border);color:var(--tv-muted);font-size:12px}
/* A finger is not a pointer. Targets grow to the ~44px everyone settled on, and
   they grow by padding rather than by a set height, so the rows stay uniform
   and the measured row height carries the change into the windowing and the
   scroll arithmetic on its own. The filter reaches 16px because anything under
   it makes iOS zoom the page on focus. The chip's remove mark stops hiding
   behind a hover nobody can perform. */
@media (pointer:coarse){
  .tv-table th,.tv-table td{padding:12px}
  .tv-table td.tv-box{min-width:44px}
  .tv-ac-item{padding:12px 12px}
  .tv-chip{padding:13px 8px 13px 12px}
  .tv-chip-x{opacity:1;padding:0 8px}
  .tv-filter,.tv-omni .tv-filter,.tv-panel .tv-filter{font-size:16px}
}
.tv-key{color:var(--tv-fg);font-weight:600}
.tv-pg{color:var(--tv-accent);font-weight:600;cursor:pointer}
.tv-pg:hover{text-decoration:underline}
.tv-pg-off{color:var(--tv-muted);font-weight:400;cursor:default;text-decoration:none}`;
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
    const omnibox = o.omnibox === true;
    const palette = o.palette === true;
    const marks = o.marks === true;
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
    /** How many chrome cells lead a row; what a column index has to skip. */
    const chrome = marks ? 1 : 0;
    /** The marked ids. @type {Set<string>} */
    const marked = new Set();
    /**
     * The flagged ids — a pending action a consumer is about to confirm, which
     * is a different question from a mark and so a different set. A row can
     * carry both, and neither clears the other.
     * @type {Set<string>}
     */
    const flagged = new Set();
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
     * the word index. The index is the expensive one, so its rebuild is queued
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
      // keys, their values and their arity all go with it.
      multiAt = undefined;
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
     * The virtual keys, and the rows behind each. SCHEMA lets a producer define
     * keys that are not columns, provided a renderer can derive the same set
     * from the same view data; the one derivation both sides agree on is org's
     * — every distinct tag in the `tags' column is a key. Cached and thrown
     * away with the text cache, since the rows are what it was read off.
     * @type {{list: string[], ids: Map<string, Set<string>>,
     *          byRow: Map<string, string[]>}|null}
     */
    let vocab = null;

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
      // A column that says what it is settles the question; the shapes below
      // are how the answer is guessed when nobody said.
      const declared = cols.findIndex((c) => c.multi === true);
      if (declared !== -1) return (multiAt = declared);
      for (let i = 0; i < cols.length && multiAt === -1; i++) {
        let shaped = 0, contrary = 0, seen = 0;
        for (const r of state.rows) {
          const cell = rowText(r).cells[i];
          if (!cell) continue;
          if (ORG_TAGS.test(cell)) shaped++;
          else if (cell.indexOf(":") !== -1) contrary++;
          if (++seen >= 40) break;
        }
        // Evidence for, and evidence against — a bare word being neither.
        // Asking every sampled cell to be a well-formed list lets one import,
        // one hand-edited headline, one stray anywhere in the sample decide
        // that a corpus has no tags at all, and the whole vocabulary goes with
        // it: no tag keys, no values under them, no completions, and the raw
        // `:a:b:' strings offered as values instead. But a cell holding a
        // single value holds no delimiter to show, so it cannot argue either
        // way; `tagsIn' reads it as the one value it plainly is. What does
        // argue against is a colon arranged some other way — a time, a URL, a
        // sentence — which no column of delimited lists would carry.
        if (shaped >= 2 && !contrary) multiAt = i;
      }
      return multiAt;
    }
    /** @type {number|undefined} */
    let multiAt;

    /** The `title' column's index, or -1; where a scoped completion finds words. */
    function titleColumn() { return columns().findIndex((c) => c.key === "title"); }

    /**
     * The tag vocabulary, derived once per row set: the tags themselves, the
     * rows each holds, and each row's tags. Both directions are kept because
     * both are asked for on the hot paths — membership when a predicate runs,
     * and a row's tags when the suggestions count them — and deriving either
     * from the other per keystroke means splitting cells all over again.
     */
    function tagVocab() {
      if (vocab) return vocab;
      const at = multiColumn();
      const ids = new Map(), byRow = new Map();
      if (at !== -1)
        for (const r of state.rows) {
          const tags = tagsIn(rowText(r).cells[at]);
          if (!tags.length) continue;
          byRow.set(r.id, tags);
          for (const tag of tags) {
            const held = ids.get(tag);
            if (held) held.add(r.id); else ids.set(tag, new Set([r.id]));
          }
        }
      vocab = { list: Array.from(ids.keys()).sort(), ids, byRow };
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

    const root = document.createElement("div");
    root.className = "tv-root";
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
    input.placeholder = `tag:book · state:active · -word · "some phrase"`;
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
    // Read once. A page that asks for less motion gets neither the crossfade
    // nor the scroll ease — the selection lands and the viewport jumps — while
    // the coalescing, which is not motion, stays.
    const calm = typeof matchMedia === "function"
              && matchMedia("(prefers-reduced-motion: reduce)").matches;
    if (calm || omnibox || palette)
      root.className = "tv-root" + (calm ? " tv-calm" : "")
                     + (omnibox && !palette ? " tv-omni" : "") + (palette ? " tv-pal" : "");

    const hint = document.createElement("div");
    hint.className = "tv-hint";

    if (!palette) root.appendChild(bar);
    if (omnibox || palette) root.appendChild(chipsEl);
    root.appendChild(scroll);
    root.appendChild(hint);
    if (palette) root.appendChild(veil);

    /** Per-column <col>, one per column. @type {HTMLElement[]} */
    let colEls = [];
    /** Per-column sort arrow, one per column. @type {HTMLElement[]} */
    let arrowEls = [];

    // Measured geometry and the window currently in the tbody.
    const geom = { row: ROW_H, head: ROW_H };
    const win = { first: -1, last: -1 };
    let remeasuring = false;
    /** The selected row's index in display order, or -1; what the bar reads. */
    let selAt = -1;

    /**
     * @param {Sort|Sort[]|undefined} sort
     * @returns {SortKey[]}
     */
    /**
     * Read SCHEMA's sort list into sort keys.  A `direction' string wins over
     * `ascending' and is the only way to ask for nulls first: bare "asc" and
     * "desc" put empty cells last whatever the column type.
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
            nullsFirst: dir.indexOf("nulls-first") !== -1,
          };
        });
    }

    function columns() { return state.view.columns || []; }
    function actions() { return state.view.actions || []; }
    function colByKey(k) { return columns().find((c) => c.key === k); }

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

    /** The column keys `parseQuery' resolves predicates against. */
    const columnKeys = () => columns().map((c) => c.key);

    /**
     * Every key a predicate may name: the columns, then the virtual keys the
     * rows imply. Columns lead, so a tag sharing a column's name is shadowed by
     * it — SCHEMA's collision rule, and the reason resolution is one ordered
     * list rather than two lookups.
     */
    function queryKeys() {
      const keys = columnKeys();
      for (const tag of tagVocab().list) if (keys.indexOf(tag) === -1) keys.push(tag);
      return keys;
    }

    /** An ISO-ish date cell, which SCHEMA gives prefix matching. */
    const DATEISH = /^\d{4}-\d{2}(-\d{2})?([ T]\d{2}:\d{2})?$/;
    /** A cell that might be meant as a date: org brackets it, or it opens with
     *  a year. Not proof of one, but not evidence of prose either. */
    const COULD_BE_DATE = /^[<[]?\d/;

    /**
     * Does column I hold dates? Decided once per query off a sample rather than
     * per cell, so a date column costs no regex in the filter loop. A column
     * whose first 20 non-empty cells are ISO dates counts as one.
     * @param {number} i
     */
    function dateColumn(i) {
      let shaped = 0, contrary = 0, seen = 0;
      for (const r of state.rows) {
        const s = rowText(r).cells[i];
        if (!s) continue;
        if (DATEISH.test(s)) shaped++;
        else if (!COULD_BE_DATE.test(s)) contrary++;
        if (++seen >= 40) break;
      }
      // Weighed the way `multiColumn' weighs its own: evidence for, evidence
      // against, and cells that are neither. A stamp org spelled its own way,
      // or a date this parser does not quite recognise, is not a date column
      // saying it holds prose — it abstains, and one of them must not cost the
      // column its prefix matching. What argues against is a cell that could
      // not be a date at all, which is what a column of sentences is full of.
      return shaped >= 2 && !contrary;
    }

    /**
     * TOK as a row test, negation aside — `queryMatcher' applies that, since
     * where a token lands in the AND/OR shape depends on it. Free text is a
     * substring of the whole row; a field predicate reads one cell, by SCHEMA's
     * semantics for that column's type.
     * @param {Token} tok  @returns {(r: Row) => boolean}
     */
    function tokenTest(tok) {
      const v = tok.value.toLowerCase();
      if (tok.key === null)
        // The cached joined string, which is the hot path and the reason the
        // pure-free-text query costs exactly what it did before.
        return v ? (r) => rowText(r).search.includes(v) : () => true;
      const col = colByKey(tok.key);
      if (!col) {
        // A virtual key: carrying the tag, and matching the text beside it.
        // Membership comes from the vocabulary rather than from the cell, so
        // `con:' cannot match `:contact:' — the same split built both.
        const ids = tagVocab().ids.get(tok.key) || new Set();
        if (!v) return (r) => ids.has(r.id);
        return (r) => ids.has(r.id) && rowText(r).search.includes(v);
      }
      const i = columns().indexOf(col);
      // `key:' with nothing after it yet — the half-typed state the suggestion
      // list exists to serve — narrows nothing, whatever the column's type.
      // Asking for an empty cell is what `none' is for.
      if (!v) return () => true;
      if (v === "none") return (r) => rowText(r).cells[i] === "";
      // A producer meta (`state:*active*') is matched literally here and so
      // matches nothing: only the producer knows which keywords it stands for,
      // and a view that declares metas is expected to filter through `onFilter'.
      if (col.type === "badge") return (r) => rowText(r).cells[i] === v;
      if (dateColumn(i)) return (r) => rowText(r).cells[i].startsWith(v);
      return (r) => rowText(r).cells[i].includes(v);
    }

    /**
     * Q compiled to a row test, or null when it filters nothing. Built once per
     * filter change and reused for every row.
     *
     * SCHEMA's shape: predicates sharing one key OR together — `state:TODO
     * state:DONE' is either — while distinct keys and free text AND, and a
     * negation ANDs whatever it is. So the positive predicates group by key,
     * each group passes on any member, and everything else stands on its own.
     * @param {string} q  @returns {((r: Row) => boolean)|null}
     */
    /** Does KEY name a field a row may hold several of at once? */
    function manyValued(key) {
      const col = colByKey(key);
      if (!col) return true;                 // a virtual key is one of its values
      return columns().indexOf(col) === multiColumn();
    }

    function queryMatcher(q) {
      /** @type {Map<string, ((r: Row) => boolean)[]>} */
      const groups = new Map();
      /** @type {((r: Row) => boolean)[]} */
      const musts = [];
      for (const tok of parseQuery(q, queryKeys())) {
        const test = tokenTest(tok);
        if (tok.negated) musts.push((r) => !test(r));
        else if (tok.key === null) musts.push(test);
        else {
          const g = groups.get(tok.key);
          if (g) g.push(test); else groups.set(tok.key, [test]);
        }
      }
      // SCHEMA splits same-key grouping by arity. A single-valued field can
      // only hold one of them, so repeating it means either (`state:TODO
      // state:DONE'). A multi-valued one can hold both, so repeating it means
      // both — GitHub's label semantics, and org's, `tag:a tag:b' being a row
      // carrying each. The virtual keys are that column's values, so they
      // inherit its arity.
      for (const [key, g] of groups)
        musts.push(g.length === 1 ? g[0] : manyValued(key) ? (r) => {
          for (const t of g) if (!t(r)) return false;
          return true;
        } : (r) => {
          for (const t of g) if (t(r)) return true;
          return false;
        });
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
     * Column widths in characters: the widest cell in the filtered set, and the
     * header (plus its sort arrow). @returns {number[]}
     */
    function colWidths() {
      if (widths) return widths;
      const cols = columns(), primary = state.sortKeys[0];
      const w = cols.map((c) =>
        String(c.header || c.key).length + (primary && primary.column === c.key ? 2 : 0));
      for (const r of ordered()) {
        const len = rowText(r).len;
        for (let i = 0; i < w.length; i++) if (len[i] > w[i]) w[i] = len[i];
      }
      // A badge cell draws a pill around its text, whose padding the cached
      // length knows nothing about.
      for (let i = 0; i < cols.length; i++) if (cols[i].type === "badge") w[i] += PILL_CH;
      // A tag cell needs no allowance: `:a:b:' and `a · b' are the same length,
      // `:a:' is longer than `a', and the smaller type shrinks it further — the
      // rendering never outgrows the raw text the widths were measured from.
      widths = w;
      return w;
    }

    /** Widen the cached widths for ROW (an upsert can only add text). */
    function growWidths(r) {
      if (!widths) return;
      const len = rowText(r).len;
      for (let i = 0; i < widths.length; i++) if (len[i] > widths[i]) widths[i] = len[i];
    }

    function applyWidths() {
      const w = colWidths();
      for (let i = 0; i < colEls.length; i++) {
        // `ch' is exact in the monospace face the renderer sets; a consumer that
        // overrides it with a proportional font gets a hint, and the table's own
        // min-content width still wins.
        const px = `calc(${w[i]}ch + ${CELL_PAD}px)`;
        if (colEls[i].style.width !== px) colEls[i].style.width = px;
      }
    }

    // ---- rendering ---------------------------------------------------------

    /** Rebuild the colgroup and the header row (mount, and a view change). */
    function renderHead() {
      colgroup.innerHTML = "";
      headRow.innerHTML = "";
      colEls = [];
      arrowEls = [];
      // The mark column leads and is nobody's column: it is left out of
      // `colEls' and `arrowEls', which stay one entry per column the view
      // declared, so widths and sort arrows keep indexing what they always did.
      if (marks) {
        colgroup.appendChild(document.createElement("col"));
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
        th.textContent = String(c.header || c.key);
        const arrow = document.createElement("span");
        arrow.className = "tv-arrow";
        th.appendChild(arrow);
        headRow.appendChild(th);
        arrowEls.push(arrow);
      }
      renderArrows();
    }

    /** Point the sort arrow at the primary sort column, and hide the rest. */
    function renderArrows() {
      const primary = state.sortKeys[0], cols = columns();
      for (let i = 0; i < arrowEls.length; i++) {
        const on = !!primary && primary.column === cols[i].key;
        arrowEls[i].textContent = on ? (primary.ascending ? "▲" : "▼") : "";
        arrowEls[i].style.display = on ? "" : "none";   // no empty arrow's margin
      }
    }


    /**
     * A row's <tr>. I is its index in the display order: zebra striping is
     * stamped from it, since `:nth-child' sees only the window.
     * @param {Row} r  @param {number} i  @returns {string}
     */
    function rowHTML(r, i) {
      const cols = columns(), cs = r.cells || {};
      const on = r.id === state.selected;
      const multi = multiColumn();
      let tds = marks ? `<td class="tv-box"></td>` : "";
      for (let c = 0; c < cols.length; c++) {
        const inCol = c === state.selCol;
        const cell = (cols[c].align === "right" ? "tv-right" : "")
                   + (inCol ? " tv-colsel" : "") + (on && inCol ? " tv-cell-sel" : "");
        tds += `<td class="${cell}">`
             + `${cellHTML(cols[c], cs[cols[c].key], dark, c === multi)}</td>`;
      }
      const cls = (i % 2 ? " tv-alt" : "") + (isMarked(r.id) ? " tv-marked" : "")
                + (isFlagged(r.id) ? " tv-flagged" : "")
                + (on ? " tv-sel" : "");
      return `<tr class="${cls}" data-id="${esc(r.id)}">${tds}</tr>`;
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
     * The furthest this scroller can travel with PORT pixels on show: the
     * header plus every row of the page, less the viewport.
     */
    function maxScroll(port) {
      return Math.max(0, geom.head + paged().length * geom.row - port);
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
      const s = state.sortKeys[0];
      const sort = s ? `sort ${s.column} ${s.ascending ? "asc" : "desc"}` : "unsorted";
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
      if (!marks) return out;
      if (marked.size) out = `${esc(grouped(marked.size))} marked · ${out}`;
      if (flagged.size) {
        // With the cursor ON a flagged row the segment turns into a reminder
        // of what can be done about it. The text is the CONSUMER's whole
        // string — the keys are theirs to bind and theirs to name, and a
        // renderer inventing `d' or `u' here would be asserting a keymap it
        // does not own. Rendered in the legend's own shape: key tokens small
        // and the words between them plain.
        const help = flagHelp && state.selected !== null && flagged.has(state.selected)
          ? ` · ${flagHelpHTML}` : "";
        out = `${esc(grouped(flagged.size))} flagged${help} · ${out}`;
      }
      return out;
    }

    /**
     * COL clamped to a real column index, or null for a whole-row selection.
     * Clamped rather than wrapped: walking off the last column stays there,
     * which is what a table does.
     * @param {number|null|undefined} col  @returns {number|null}
     */
    function clampCol(col) {
      if (col === null || col === undefined) return null;
      const n = columns().length;
      if (!n) return null;
      return Math.max(0, Math.min(n - 1, Math.trunc(col)));
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
      state.selCol = id === null || id === undefined ? null : clampCol(col);
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
     * The row and column state the window wears — `tv-sel', `tv-marked',
     * `tv-flagged', the selected column's `tv-colsel' and the one
     * `tv-cell-sel' where they cross — re-derived from the state rather than
     * rebuilt, which is what leaves the grounds something to crossfade
     * between. One pass for all of them, since a toggle and a step arrive in
     * the same frame, and `classList.toggle's rather than any DOM write: each
     * is a no-op where the state already matches, so a held movement key
     * rewrites nothing. Only the window is stamped, the header apart, which is
     * all there is to stamp — the rows outside it have no elements.
     */
    function stampSelection() {
      const id = state.selected;
      for (let i = 0; i < tbody.children.length; i++) {
        const tr = /** @type {HTMLElement} */ (tbody.children[i]);
        const rowId = tr.dataset.id;
        if (rowId === undefined) continue;
        const on = id !== null && rowId === id;
        tr.classList.toggle("tv-sel", on);
        tr.classList.toggle("tv-marked", isMarked(rowId));
        tr.classList.toggle("tv-flagged", isFlagged(rowId));
        // The chrome cell is nobody's column, so the column a cell selection
        // names is counted past it.
        for (let c = chrome; c < tr.children.length; c++) {
          const inCol = c - chrome === state.selCol;
          tr.children[c].classList.toggle("tv-colsel", inCol);
          tr.children[c].classList.toggle("tv-cell-sel", on && inCol);
        }
      }
      // A column highlight that stopped at the header would read as broken, and
      // the header is not rebuilt per window, so it is stamped here rather than
      // in `rowHTML'. A whole-row selection has no column and clears both.
      for (let c = chrome; c < headRow.children.length; c++)
        headRow.children[c].classList.toggle("tv-colsel", c - chrome === state.selCol);
    }


    // ---- marks -------------------------------------------------------------
    // Dired's, and kept the way dired keeps them: a set of ids that owes the
    // rows nothing. A row can be re-sent, re-sorted, filtered away or paged past
    // and its mark is still the same entry in the same set — which is the whole
    // reason marking is keyed by `id' and not by a row object or an index.

    /**
     * Does ID wear a mark the table is drawing? One predicate for the class,
     * the box and the count, so the option gates all three together: without
     * `marks' the chrome is not merely hidden, there is nothing to hide.
     * @param {string} id
     */
    function isMarked(id) { return marks && marked.has(id); }

    /** Whether ID is flagged, the chrome being opt-in the same way. */
    function isFlagged(id) { return marks && flagged.has(id); }

    /** Flag ID, or unflag it. @param {string} id  @returns {boolean} its new state */
    function flagRow(id) {
      const on = !flagged.has(id);
      if (on) flagged.add(id); else flagged.delete(id);
      paintMarks();
      return on;
    }

    /** Take the flag off ID, whether or not it had one. @param {string} id */
    function unflagRow(id) {
      if (flagged.delete(id)) paintMarks();
    }

    /** Take every flag off. Marks are a different question and are left alone. */
    function clearFlags() {
      if (!flagged.size) return;
      flagged.clear();
      paintMarks();
    }

    /**
     * The flagged ids, read the way `getMarked' reads its own: the ones on
     * show in display order, then the ones a filter or another page is hiding,
     * in the order they were flagged.
     * @returns {string[]}
     */
    function getFlagged() {
      const out = shownRows().filter((r) => flagged.has(r.id)).map((r) => r.id);
      const shown = new Set(out);
      for (const id of flagged) if (!shown.has(id)) out.push(id);
      return out;
    }

    /** Mark ID, or unmark it. @param {string} id  @returns {boolean} its new state */
    function toggleMark(id) {
      const on = !marked.has(id);
      if (on) marked.add(id); else marked.delete(id);
      paintMarks();
      return on;
    }

    /**
     * Mark every row of the CURRENT FILTERED SET — all of it, not the page on
     * show, since a filter is what a reader narrowed to and the page is only
     * how much of it fits. With no filter that is every row. Idempotent: a row
     * already marked stays marked, so running it twice is running it once.
     * @returns {number} how many rows carry a mark afterwards
     */
    function markAll() {
      if (!marks) return 0;                     // no mark column, nothing to mark
      const before = marked.size;
      for (const r of ordered()) marked.add(r.id);
      if (marked.size !== before) paintMarks();
      return marked.size;
    }

    /** Take every mark off. */
    function clearMarks() {
      if (!marked.size) return;
      marked.clear();
      paintMarks();
    }

    /**
     * The marked ids, in the order a reader would read them off: the ones on
     * show, in display order, then the ones a filter or another page is hiding,
     * in the order they were marked. Stable either way — a bulk action over this
     * runs in the same order twice.
     * @returns {string[]}
     */
    function getMarked() {
      const out = shownRows().filter((r) => marked.has(r.id)).map((r) => r.id);
      const shown = new Set(out);
      for (const id of marked) if (!shown.has(id)) out.push(id);
      return out;
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
     * COL selects one cell of that row, clamped to the columns that exist;
     * omitted, the selection is the whole row, which is what it always was.
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
      state.selCol = clampCol(col);
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
        else scroll.scrollTop = scroll.scrollTop + step * EASE;
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

    function toggleSort(key) {
      const col = colByKey(key);
      if (!col || col.sortable !== true) return;
      const primary = state.sortKeys[0];
      state.sortKeys = (primary && primary.column === key)
        ? [{ column: key, ascending: !primary.ascending, nullsFirst: false }]
        : [{ column: key, ascending: true, nullsFirst: false }];
      page = 0;                          // a different order, read from the top
      continuous = false;
      dropSorted();
      scroll.scrollTop = 0;
      renderArrows();
      renderRows(true);
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
      const th = /** @type {HTMLElement|null} */ (t.closest("th[data-key]"));
      if (th) { toggleSort(th.dataset.key); return; }
      const tr = /** @type {HTMLElement|null} */ (t.closest("tr[data-id]"));
      if (!tr) return;
      // The box is the one cell that is not a selection: a mark is a standing
      // choice about a row and says nothing about where the cursor is, so
      // checking one leaves the cursor where the reader put it.
      if (onBox(t)) {
        if (tr.dataset.id !== undefined) toggleMark(tr.dataset.id);
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

    /** The query as it stands: every chip, then whatever is in the box. */
    function effectiveQuery() {
      const typed = input.value.trim();
      if (!chips.length) return typed;
      const front = chips.join(" ");
      return typed ? front + " " + typed : front;
    }

    function renderChips() {
      let html = "";
      for (let i = 0; i < chips.length; i++)
        html += `<span class="tv-chip" data-i="${i}" title="remove">${esc(chips[i])}`
              + `<i class="tv-chip-x">×</i></span>`;
      chipsEl.innerHTML = html;
      chipsEl.style.display = chips.length ? "" : "none";
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
      for (const t of toks) if (t !== keep) chips.push(v.slice(t.start, t.end));
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
      if (o.onFilter) o.onFilter(q);
      else if (onFrame) frame(applyFilter);
      else applyFilter();
    }

    /**
     * Take off the last unit of the query: what is half-typed in the box if
     * there is any, else the last chip. Reapplies through the one delivery
     * point, and leaves focus alone — the caller owns that. False when there
     * was nothing left to take off, so a consumer can walk the query down and
     * know when it has hit the end.
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
    const SCOPED_MAX = 5;       // scoped completions offered, when nothing exact was found
    const SCOPED_MIN = 2;       // ... and only past this much typing
    const DOMAIN_MAX = 200;     // distinct values kept before the prefix narrows them

    /**
     * @type {{stage: string, tok: Token,
     *         items: {text: string, count: number, full: boolean, dim: boolean,
     *                  pick: boolean, tag?: string}[]}|null}
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
          d = { list: v.list, counts };
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
        // A virtual key takes no value list — what follows it is free text over
        // the rows it scopes, and there is no domain to offer for that.
        const col = colByKey(t.key);
        return col ? { stage: "value", tok: t, col, prefix: t.value } : null;
      }
      if (!t.value || splitAt(t.value) !== -1) return null;
      return { stage: "key", tok: t, col: null, prefix: t.value };
    }

    /**
     * The suggestions for STAGE: the text each one inserts, the number of rows
     * behind it, and whether it finishes a token. A column completion does not
     * — it lands as `key:' with the value still to type, and carries no count
     * because it narrows nothing on its own.
     * @returns {{text: string, count: number, full: boolean, dim: boolean,
     *             pick: boolean, tag?: string}[]}
     */
    function suggestFor(st) {
      const p = st.prefix.toLowerCase();
      const out = [];
      if (!st.col) {
        // 1. The keys the word opens — the view's columns, then the keys the
        //    rows imply. Both are exact facts, so neither is dimmed; the
        //    columns come first because they are the view's own vocabulary,
        //    and a tag carries the count of the rows that hold it, a column
        //    having no one number to show.
        const keys = columnKeys();
        for (const k of keys) {
          if (!k.toLowerCase().startsWith(p)) continue;
          out.push({ text: k + ":", count: -1, full: false, dim: false, pick: true });
          if (out.length === AC_MAX) break;
        }
        const held = tagVocab().ids;
        for (const tag of tagVocab().list) {
          if (out.length === AC_MAX) break;
          const rows = held.get(tag);
          if (!rows || keys.indexOf(tag) !== -1 || !tag.startsWith(p)) continue;
          out.push({ text: tag + ":", count: rows.size, full: false, dim: false,
                     pick: false, tag });
        }
        // 2. Values some column actually has, reached by prefix: `TOD' means
        //    `state:TODO' and `alberbl' means `tags:alberblanc'. Facts about
        //    the data rather than guesses about it — but only where a column
        //    has a domain worth enumerating: its declared `values', its badge
        //    palette, or the tag vocabulary. A free-text column has no such
        //    set, and offering one word of it is what the third tier is for.
        let exact = 0;
        const hits = [];
        for (const c of columns()) {
          if (!domainValues(c) && columns().indexOf(c) !== multiColumn()) continue;
          const dom = domainOf(c);
          for (const v of dom.list) {
            const lower = String(v).toLowerCase();
            if (!lower.startsWith(p)) continue;
            if (lower === p) exact++;
            const meta = META.test(String(v));
            hits.push({ text: c.key + ":" + v,
                        count: meta ? -1 : dom.counts.get(lower) || 0,
                        whole: lower === p, full: true, dim: meta, pick: false });
          }
        }
        // What was typed in full outranks what merely opens with it.
        hits.sort((a, b) => (b.whole ? 1 : 0) - (a.whole ? 1 : 0)
                         || b.count - a.count
                         || (a.text < b.text ? -1 : 1));
        for (const hit of hits) {
          if (out.length === AC_MAX) break;
          out.push({ text: hit.text, count: hit.count, full: true,
                     dim: hit.dim, pick: false });
        }
        // 3. Words the rows finish for it, scoped to the tag they were found
        //    under. Only an EXACT value match makes these redundant — a value
        //    merely opening with what was typed is a guess of the same kind, so
        //    the two stand together. They are dimmed either way: a scoped count
        //    counts a word in a title and must not dress like a value match. A
        //    single letter completes to most of the store, which says nothing
        //    and costs a pass over every row to say.
        if (!exact && p.length >= SCOPED_MIN)
          for (const hit of scopedCompletions(p).slice(0, SCOPED_MAX)) {
            if (out.length === AC_MAX) break;
            out.push({ text: hit.tag + ":" + hit.word, count: hit.count,
                       full: true, dim: true, pick: false, tag: hit.tag });
          }
        return out;
      }
      const dom = domainOf(st.col);
      for (const v of dom.list) {
        const lower = String(v).toLowerCase();
        if (!lower.startsWith(p)) continue;
        // Not preselected: with `tag:' typed and no value chosen, Enter has to
        // mean the presence predicate the user wrote, not whichever value
        // happened to sort first.
        //
        // A producer meta stands apart from the concrete values beside it:
        // dimmed and italic, and with no count. Counting it locally would
        // print 0 — no cell holds the literal `*active*' — and a 0 beside a
        // value that in fact matches many rows is worse than no number.
        // What it means is the producer's to say; see `predicate'.
        const meta = META.test(String(v));
        out.push({ text: String(v), count: meta ? -1 : dom.counts.get(lower) || 0,
                   full: false, dim: meta, pick: false });
        if (out.length === AC_MAX) break;
      }
      return out;
    }

    /**
     * Every title word, sorted, with the tags it appears under and how many
     * rows each of those pairings covers. Built whole on first use rather than
     * patched: a prefix query wants sorted words, an upsert can move any of
     * them, and rebuilding on the next keystroke is both simpler and cheaper
     * than keeping a sorted structure correct through every row change.
     * Thrown away with the text cache, which is where it was read from.
     * A posting is a flat `[tag, count, tag, count, …]' rather than a map:
     * most words sit under one or two tags, and at this size the allocation of
     * a map per word costs more than the linear scan of a short array saves.
     * @type {{words: string[], posts: (string|number)[][]}|null}
     */
    let wordIndex = null;

    function titleIndex() {
      if (wordIndex) return wordIndex;
      const byRow = tagVocab().byRow;
      const at = titleColumn();
      /** @type {Map<string, (string|number)[]>} */
      const acc = new Map();
      if (byRow.size && at !== -1)
        for (const r of state.rows) {
          const tags = byRow.get(r.id);
          if (!tags) continue;
          // A literal split does here what a regex one would: `displayText'
          // has already turned every run of control characters into a single
          // space, and the empty strings a double space leaves are skipped.
          // Edge punctuation goes before anything else looks at the word, so
          // the deduplication below sees the forms a query would.
          const words = rowText(r).cells[at].split(" ").map(bareWord);
          for (let w = 0; w < words.length; w++) {
            const word = words[w];
            // A word twice in one title is still one row; the titles are short
            // enough that looking back beats a set per row.
            if (!word || words.indexOf(word) !== w) continue;
            let post = acc.get(word);
            if (!post) acc.set(word, (post = []));
            for (const tag of tags) {
              let i = 0;
              while (i < post.length && post[i] !== tag) i += 2;
              if (i < post.length) post[i + 1] = /** @type {number} */ (post[i + 1]) + 1;
              else post.push(tag, 1);
            }
          }
        }
      const words = Array.from(acc.keys()).sort();
      const built = { words, posts: words.map((w) => acc.get(w) || []) };
      wordIndex = built;
      return built;
    }

    /** The first index in WORDS at or after P. @param {string[]} words */
    function lowerBound(words, p) {
      let lo = 0, hi = words.length;
      while (lo < hi) {
        const mid = (lo + hi) >> 1;
        if (words[mid] < p) lo = mid + 1; else hi = mid;
      }
      return lo;
    }

    /**
     * Scoped completions for the prefix P: title words that begin with it,
     * each paired with a tag of the rows it was found in. Typing `tan' offers
     * `contact:tanik' — a completion of the word rather than an echo of the
     * fragment, so every row offered matches at least the rows it was counted
     * from, and accepting one can never come back empty.
     *
     * The sorted index makes this the prefix range and its postings: a binary
     * search and a walk to the end of the range, rather than a pass over the
     * rows per keystroke.
     * @param {string} prefix  @returns {{tag: string, word: string, count: number}[]}
     */
    function scopedCompletions(prefix) {
      const idx = titleIndex();
      const p = bareWord(prefix);       // matched against words cleaned the same way
      if (!p) return [];
      const out = [];
      for (let i = lowerBound(idx.words, p); i < idx.words.length; i++) {
        const word = idx.words[i];
        if (!word.startsWith(p)) break;
        if (word.length === p.length) continue;   // what was typed completes nothing
        const post = idx.posts[i];
        for (let k = 0; k < post.length; k += 2)
          out.push({ tag: /** @type {string} */ (post[k]),
                     word, count: /** @type {number} */ (post[k + 1]) });
      }
      return out.sort((a, b) => b.count - a.count
                             || (a.tag === b.tag ? (a.word < b.word ? -1 : 1)
                                                 : (a.tag < b.tag ? -1 : 1)));
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
        // A row naming a tag wears it the way the cells do, so the same value
        // is the same shape wherever it is read.
        const label = it.tag
          ? `<span class="tv-tag">${esc(it.tag)}</span>${esc(it.text.slice(it.tag.length))}`
          : esc(it.text);
        html += `<div class="tv-ac-item${it.dim ? " tv-ac-dim" : ""}`
              + `${i === acAt ? " tv-ac-on" : ""}" data-i="${i}">`
              + `<span class="tv-ac-label">${label}</span>`
              + (it.count < 0 ? "" : `<span class="tv-ac-n">${it.count}</span>`)
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
      // A column name is what the typist is visibly reaching for, so it starts
      // highlighted and Enter takes it. Everything else is an offer beside what
      // they typed — a tag name is very often the word they are actually
      // searching for — so nothing starts highlighted, Enter commits the word
      // as written, and an arrow key is how you step into the offers.
      acAt = items[0] && items[0].pick ? 0 : -1;
      renderAc();
    }

    function moveAc(step) {
      if (!ac) return;
      const n = ac.items.length;
      acAt = acAt < 0 ? (step > 0 ? 0 : n - 1) : (acAt + step + n) % n;
      renderAc();
    }

    /**
     * Put TEXT in place of the token under the caret, leaving the rest of the
     * box alone. A key lands as `key:' with the caret against the colon, ready
     * for the value; a value lands with a trailing space, ready for the next
     * token. Focus stays in the box either way.
     */
    function acceptAc(item) {
      if (!ac) return;
      const v = input.value, t = ac.tok;
      const head = ac.stage === "key" ? (t.negated ? "-" : "") : v.slice(t.start, t.sep + 1);
      const ins = head + item.text + (item.full || ac.stage === "value" ? " " : "");
      input.value = v.slice(0, t.start) + ins + v.slice(t.end);
      const caret = t.start + ins.length;
      if (input.setSelectionRange) input.setSelectionRange(caret, caret);
      armFilter();
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
        const accepts = (e.key === "Tab" || e.key === "Enter") && acAt >= 0;
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
        // Nothing highlighted: the keys fall through to what they mean with no
        // list at all, so a typed word is still committed by Enter.
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
      const chip = t && /** @type {HTMLElement|null} */ (t.closest(".tv-chip"));
      if (!chip) return;
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
    // second time while the chips it already had go missing.
    if (typeof o.initialQuery === "string" && o.initialQuery.trim()) {
      for (const t of parseQuery(o.initialQuery, queryKeys()))
        chips.push(o.initialQuery.slice(t.start, t.end));
      lastQuery = effectiveQuery();
      // Local filtering has to catch up to it; a producer has already filtered.
      if (!o.onFilter) state.filter = lastQuery;
    }

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
        state.sortKeys = normalizeSort(v && v.sort);
        state.selected = null;
        state.selCol = null;
        state.filter = "";
        marked.clear();          // a different view; these were about the last one
        flagged.clear();
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
        marked.delete(id);       // the row is gone; a mark on it would outlive it
        flagged.delete(id);
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
              marked.delete(gone.id);      // as `deleteRow': the row is gone
              flagged.delete(gone.id);
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
      stripLastToken,
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
      toggleMark,
      markAll,
      flagRow,
      unflagRow,
      getFlagged,
      clearFlags,
      /** How many rows are flagged, the hidden ones counted. @returns {number} */
      flaggedCount() { return flagged.size; },
      getMarked,
      clearMarks,
      /** How many rows are marked, the hidden ones counted. @returns {number} */
      markedCount() { return marked.size; },
    };
  }

  const TableView = { mount, displayText, comparator, parseQuery };
  root.TableView = TableView;
  // @ts-ignore -- optional CommonJS export (no @types/node dependency)
  if (typeof module !== "undefined" && module.exports) module.exports = TableView;
})(typeof window !== "undefined" ? window : this);
