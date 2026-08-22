/*
 * table-view.js — browser renderer for the table-view contract (../SCHEMA.md).
 *
 * Renders a View object as an interactive HTML table: badges, alignment,
 * sortable columns, a filter with suggestions, Org-link cells, action dispatch
 * and streaming updates.  Dependency-free, theme-aware, no build step.
 *
 *   const tv = TableView.mount(el, view, { onAction, onLink, onFilter });
 *
 * THE HANDLE AND THE RENDERING RULES ARE IN ../README.md under `## Browser
 * renderer'; the geometry, the filter grammar and the suggestion ordering are
 * also in ../docs/web-renderer.org.  What is below is what those do not hold.
 *
 * - `composer: true' — the bar and the chips ARE the widget, with no table
 *   behind them; the query still commits to `onFilter' and reads back off
 *   `getQuery'.
 * - `inline: true' — the mount is a small box inside someone else's chrome: the
 *   chips stay, the filter box is summoned by `openFilter' onto the chips' own
 *   line, and the title, the hint line, the sort marks and the page furniture
 *   go. The window is capped rather than filling, Escape out of the filter is
 *   ONE step, and Backspace over an emptied box puts that box away rather than
 *   taking a chip. What a picker hung at a caret wants.
 * - `filterDock: "overlay" | "strip"' — WHERE a summoned box lands: over the
 *   page behind the veil, or on the chip strip's own row. `palette' docks
 *   overlay and `inline' docks strip unless the option says otherwise; on a
 *   plain mount it is what makes the box summoned at all, and the rest of that
 *   mount — the filling table, the sort marks, the hint line — stays.
 * - `setPinned(on)' — the chip strip's pin badge, drawn only under `onPin'.
 * - Emits DOM CustomEvents on the container: `tableview-action'
 *   ({detail:{command,id,row}}) and `tableview-link' ({detail:{target,row}}).
 * - Selection is drawn as GROUNDS on the cells.  An absolutely positioned
 *   highlight bar was tried and thrown away: it duplicates row geometry it
 *   cannot own, so collapsed borders and sub-pixel metrics drift it off the row.
 *
 * Type-checked by `// @ts-check' and the @typedefs below; `make web-check'.
 * `make web-perf' benchmarks a 13k-row view headlessly (web/perf-driver.js).
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
 *             inline?: boolean,
 *             filterDock?: "overlay"|"strip",
 *             onPin?: () => void,
 *             onRefused?: (token: string) => void,
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
 *             filtering: () => boolean,
 *             destroy: () => void,
 *             openFilter: (how?: { narrow?: boolean }) => void,
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
 *             added: boolean,
 *             key: string|null,
 *             value: string,
 *             quoted: boolean,
 *             start: number,
 *             end: number,
 *             sep: number }} Token  One filter-query token; see `parseQuery'.
 *   `negated' is the `-' sign and `added' the `+' one; `start' sits AT the sign,
 *   so a raw slice carries it.
 */

/** @param {*} root  The global object (`window`, or CommonJS `this`). */
(function (root) {
  "use strict";


  const esc = (s) =>
    String(s).replace(/[&<>"']/g, (c) =>
      ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c]));

  const ORG_LINK = /\[\[([^\]]+?)\](?:\[([^\]]*?)\])?\]/g;

  /** @param {Cell|undefined} val  @returns {string} */
  function displayText(val) {
    if (val === null || val === undefined) return "";
    let s = typeof val === "string" ? val : String(val);
    // the scan is the hot path; skip the link rewrite for strings that can't hold one.
    if (s.indexOf("[[") !== -1)
      s = s.replace(ORG_LINK, (_, target, desc) => desc || target);
    return s.replace(/[\u0000-\u001f\u007f]+/g, " ");
  }

  // badge ink (hue kept, lightness moved to WCAG AA): docs/web-renderer.org

  /**
   * Does this browser take C-n and C-p before the page sees them?  Chrome's
   * family binds them to new-window and print; Firefox and the webview shells
   * deliver both.  Read per render, so a test can reach it.
   */
  function swallowsCtrlN() {
    const ua = typeof navigator === "object" && navigator ? navigator.userAgent || "" : "";
    return /Chrom(e|ium)\//.test(ua) && !/Firefox|Electron\//.test(ua);
  }

  /**
   * The values CELL spells, org-style: `:a:b:' is a and b.  THE ONE SPLITTER —
   * the vocabulary is built with it and the cells rendered with it, so a chip
   * and a query key cannot disagree about where a value begins.
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
   * COLOR made legible on the pill it tints.  Stepped toward black on light and
   * white on dark — a scale of the same hue — until the label clears 4.5:1
   * against the 15% wash it sits on.  Cached per colour and theme.
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

  /** @param {Column} col  @param {Cell|undefined} val  @param {boolean} [dark]
   *  @param {boolean} [asTags]  @returns {string} */
  function cellHTML(col, val, dark, asTags) {
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
      if (color)
        return `<span class="tv-pill" style="--tv-badge:${esc(color)};`
             + `--tv-ink:${esc(inkFor(color, !!dark))}">${esc(raw)}</span>`;
      return esc(raw);
    }
    const s = typeof val === "string" ? val : displayText(val);
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

  // filter micro-syntax, exported as TableView.parseQuery: docs/web-renderer.org (mirrors SCHEMA.md)

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
   * VALUE's alternatives — `A|B' is either; an EMPTY one is dropped, so a value
   * that is bars alone is left with none, which is the `key:' rule.  Quotes are
   * gone by here, so a bar inside a predicate is always the operator.
   * @param {string} value  @returns {string[]}
   */
  const alternatives = (value) => value.split(ALT).filter((v) => v !== "");

  /**
   * Split Q into raw tokens: quotes removed, a leading sign taken off, and the
   * whole token's offsets kept so a caret can be placed inside one — `start'
   * sits AT the sign, so the raw slice a chip keeps still spells it.  Separators
   * inside quotes are ordinary characters.
   * @param {string} q
   */
  function scanQuery(q) {
    const out = [];
    let start = 0, body = "", neg = false, add = false, quoted = false;
    let seen = false, hasBody = false, inQ = false, sep = -1;
    const flush = (end) => {
      if (seen) out.push({ start, end, body, negated: neg, added: add, quoted, sep });
      body = ""; neg = false; add = false; quoted = false; seen = false; hasBody = false; sep = -1;
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
      } else if (!seen && (c === "-" || c === "+")) {
        // SEEN GUARDS THE SIGN, so a second one lands in the body: `+-x' is an
        // added free-text token spelling `-x'.
        start = i; seen = true;
        if (c === "-") neg = true; else add = true;
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
   * Q as tokens, against the column KEYS of the view it filters.  `key:value'
   * is a field predicate only when KEY names a column, so org cell text like
   * `:work:' stays free text; a quoted token is always free text; a leading `-'
   * negates either form and a leading `+' widens its key's axis
   * (`queryMatcher').
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
        added: t.added,
        key: pred ? key : null,
        value: pred ? t.body.slice(at + 1) : t.body,
        quoted: t.quoted,
        start: t.start,
        end: t.end,
        sep: pred ? t.sep : -1,
      };
    });
  }

  /**
   * How TOK's sign is written: `+' added, `-' negated, and the empty string
   * where it opens with neither.  ONE reading, so a chip and a completion
   * cannot spell the same token two ways.
   * @param {Token} tok  @returns {string}
   */
  const signMark = (tok) => (tok.added ? "+" : tok.negated ? "-" : "");

  /** @param {Cell|undefined} v  @returns {number|null} */
  const asNumber = (v) => {
    const n = typeof v === "number" ? v : parseFloat(displayText(v));
    return Number.isNaN(n) ? null : n;
  };

  // metas are not sort positions: docs/web-renderer.org
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
   * and what completion matches through, so `act' reaches `*active*'.  What is
   * drawn and inserted keeps the stars, and a query MEANS them.
   */
  const starless = (v) => (META.test(v) ? v.slice(1, -1) : v);

  /** Org's priority decoration, which a cell WEARS rather than means: `[#A]'. */
  const DECORATED = /^\[#(.*)\]$/;

  /**
   * V with that decoration off.  DISPLAY WEARS THE DECORATION, MATCHING READS
   * THROUGH IT: `[#A]' is what the table shows and `A' what a reader means, so
   * a whole-value predicate answers both spellings (`cellTest').
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
   * `planned'.  A cell is empty or it is not, so no producer set, vocabulary or
   * clock is needed and the two halves of the wire cannot disagree.  It
   * replaced the bare word `none', which reserved a spelling a cell could hold.
   */
  const EMPTY_META = "*empty*";

  // date comparisons (the value forms, `*today*'): docs/web-renderer.org

  /**
   * The date literal naming the day the query is read on: `scheduled:*today*',
   * `deadline:>=*today*'.  Legal wherever a literal stands, and resolved once
   * per compile (`queryMatcher').
   */
  const TODAY_META = "*today*";

  /**
   * The four comparisons, each spelled ONCE: a fifth is owed a `cmpTest' arm,
   * where the compiler would ask a producer for one.
   */
  const CMP_GE = ">=", CMP_LE = "<=", CMP_GT = ">", CMP_LT = "<";

  /** The comparisons a date value may open with, read LONGEST FIRST. */
  const CMPS = [CMP_GE, CMP_LE, CMP_GT, CMP_LT];

  /** The range separator inside a date value: `A..B'. */
  const RANGE = "..";

  /** A date literal opens with a digit; anything else is no date at all. */
  const DATE_LIT = /^\d/;

  /**
   * V read as a date value: `op' is the comparison it opens with, `RANGE' where
   * it names one and "" for the bare prefix; `lo' and `hi' are the literals
   * compared, EMPTY where one is owed and missing.  THE THREE FIELDS ARE THE
   * WHOLE READING: whether the value is an atom at all (`atomsIn') and what
   * stands before a literal still being typed (`suggestFor') are read off them
   * at the two places that ask, so this stays the split and nothing more.
   * @param {string} v
   * @returns {{op: string, lo: string, hi: string}}
   */
  function dateValue(v) {
    // THE OPERATOR IS READ FIRST and the range behind it, so `>=A..B' is a
    // comparison against a literal holding a separator — no cell spells one, so
    // it serves nothing — where `A..B' is the range it looks like.
    for (const op of CMPS)
      if (v.startsWith(op)) return { op, lo: v.slice(op.length), hi: "" };
    const at = v.indexOf(RANGE);
    if (at !== -1)
      return { op: RANGE, lo: v.slice(0, at), hi: v.slice(at + RANGE.length) };
    return { op: "", lo: v, hi: "" };
  }

  /**
   * P asked of a NON-EMPTY cell alone.  THE EMPTY CELL SITS OUTSIDE EVERY
   * COMPARISON AND EVERY RANGE: "" is below every literal in byte order, so an
   * unguarded `<' would serve every undated row.  `*empty*' stays the one name
   * for that cell, which is why `-k:<D' and `k:>=D' differ and NEGATION IS NO
   * MIRROR.  Worn ONCE PER AXIS — the range wraps its pair rather than each end
   * — and the bare prefix wears none and needs none, a non-empty literal being
   * the prefix of no empty cell.
   * @param {(c: string) => boolean} p  @returns {(c: string) => boolean}
   */
  const dated = (p) => (c) => c !== "" && p(c);

  /**
   * THE GRANULARITY LAW, one arm per operator: `<' and `>=' cut at literal D's
   * FIRST instant, `<=' and `>' at its LAST.  The last instant is spelled as
   * "everything the prefix reaches", which is the prefix test the bare form
   * already runs — so NO DATE ARITHMETIC is owed anywhere, and `k:D' is exactly
   * `k:>=D' and `k:<=D' together.  OP comes off the `CMPS' roster; ANYTHING
   * ELSE IS NO COMPARISON AND SERVES NO CELL, which is the guard's own refusal.
   * @param {string} op  @param {string} d  @param {string} c  @returns {boolean}
   */
  function cmpTest(op, d, c) {
    if (op === CMP_LT) return c < d;
    if (op === CMP_GE) return c >= d;
    if (op === CMP_LE) return c < d || c.startsWith(d);
    if (op === CMP_GT) return c > d && !c.startsWith(d);
    return false;
  }

  /**
   * Today as ISO `YYYY-MM-DD' in the reader's own zone, which is what `*today*'
   * resolves to.  THE CLOCK IS THE PAGE'S OWN: a producer resolves the same
   * word against its day, so the two disagree for the hour one of them is past
   * midnight and the other is not.  The skew is accepted — same machine.
   * @param {Date} [now]  @returns {string}
   */
  function localDay(now) {
    const t = now || new Date();
    const pad = (n) => String(n).padStart(2, "0");
    return `${t.getFullYear()}-${pad(t.getMonth() + 1)}-${pad(t.getDate())}`;
  }

  /**
   * SCHEMA's virtual key over a view's DATE columns together: a row is planned
   * when any of them holds anything.  Answered here in full — the cells are all
   * it takes, so the two sides cannot disagree about a row.
   */
  const PLANNED_KEY = "planned";

  /**
   * FREE TEXT'S OWN KEY: `substring:V' is what `V' alone means, so the grammar
   * is `KEY:VALUE' throughout, one matcher serving both.  The key buys a value
   * that may spell a separator under quotes without being read as one.
   */
  const SUBSTRING_KEY = "substring";

  /**
   * SCHEMA's ORDER key: `sort:COL', `sort:COL:desc'.  No predicate — written
   * order is precedence, and any replaces the view's declared `sort'.
   */
  const SORT_KEY = "sort";

  /**
   * The COLUMN SET: `columns:State,Title,Tags'.  The sort key's twin, narrowing
   * nothing; which columns a name resolves to is the producer's answer.
   */
  const COLUMNS_KEY = "columns";

  /**
   * `view:NAME' — a SAVED VIEW the producer named, narrowing nothing here.
   * `VIEW_KEYS' is the one list, so `queryKeys' and `queryMatcher' cannot come
   * to disagree about which keys skip narrowing.
   */
  const VIEW_KEY = "view";

  const VIEW_KEYS = [SORT_KEY, COLUMNS_KEY, VIEW_KEY];

  /**
   * Whether KEY SHAPES the view rather than NARROWING it.  THE GRAMMAR HAS TWO
   * HALVES: every other key answers which rows, and these three answer the
   * table around them.  One test, so a door that edits the narrowing half alone
   * draws the line where every reader of a query already draws it.
   * @param {string|null} key  @returns {boolean}
   */
  const shapesView = (key) => key !== null && VIEW_KEYS.indexOf(key) !== -1;

  /** The directions a sort token may spell; the empty one ascends. */
  const SORT_DIRS = { "": true, asc: true, desc: false };

  /**
   * The separator that CHAINS one sort token's columns:
   * `sort:title->priority:desc' is `sort:title sort:priority:desc' written once.
   * A token's segments are read as exactly the tokens they compose.
   */
  const SORT_ARROW = "->";

  /**
   * The meta that spells the EMPTY CHAIN.  `sort:*none*' NAMES a sort key, so it
   * replaces the view's declared `sort' with nothing.  It admits no companions,
   * and a renderer, having nobody to refuse to, drops it and lets them stand.
   */
  const NONE_META = "*none*";

  /**
   * SEGMENT as a sort key, or null where nothing orderable is spelled.
   * An alternation, an unknown column and a direction that is neither `asc' nor
   * `desc' each yield null; a negation is the whole token's (`sortSegments').
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
   * The segments TOK chains, in written order.  A SIGNED token chains none: the
   * sign covers everything after it, so a refusal reaches every segment.  ORDER
   * NARROWS NOTHING AND SO HAS NOTHING TO WIDEN — the producer answers `+sort:'
   * with a refusal, and this side reads no chain out of it.
   * @param {Token} tok  @returns {string[]}
   */
  function sortSegments(tok) {
    return tok.negated || tok.added ? [] : tok.value.split(SORT_ARROW);
  }

  /**
   * The chain Q names, highest priority first: [] where it names the EMPTY one
   * and null where it names no chain at all — a reader asking for no order and
   * a reader saying nothing are different readers.  Written order is precedence
   * and repeats compose; a column named twice keeps its FIRST spelling, so the
   * answer can always be handed to `applyChain'.  `*none*' takes no companions:
   * a key that resolves outranks it.
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
   * than shadowed, or a producer adding meta-values to a badge column would
   * delete that column's concrete keywords from the list.
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
   * The metas COL declares: producer vocabulary, which no cell of it holds.  A
   * column whose values are DERIVED takes them from here too, so a declared
   * meta is offered whether or not the domain came from the rows.
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


  const OVERSCAN = 15;         // rows rendered above and below the viewport
  const SAMPLE = 40;           // non-empty cells a column's shape is read off
  const SHAPED = 2;            // of them that have to carry the shape
  const ROW_H = 30;            // row height until a rendered row can be measured
  const CELL_PAD = 24;         // a cell's horizontal padding, both sides
  // column geometry (a pill's ground is paid in pixels, +1px): docs/web-renderer.org
  const PILL_PAD = 17;         // a badge pill's ground, both sides, in px
  const BOX_CH = 3;            // the gutter's glyph, `[X]', in characters
  // column geometry (COL_MAX, TITLE_MIN): docs/web-renderer.org
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
    // palette & contrast (identity consts): docs/web-renderer.org
    const FROST = "#D0E1F9";
    const FLAG = "#E74C3C";
    const COL = "#FFF3D0";
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
   than being pushed down by it. A SUMMONED mount's strip is the same strip
   however the box is docked — the row is there whether or not the box is. */
.tv-omni > .tv-chips,.tv-pal > .tv-chips,.tv-summon > .tv-chips{
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
   rather than the solid it was: CHANGELOG, "chips are a frost wash".

   IT IS THE SUMMONED MOUNT'S, whichever dock the box landed in: where the box
   is away until it is called for, the strip is the only standing evidence of
   the query, and it is coloured to carry that. The picker declines all four
   voices — it sits in someone else's chrome, which owns the page's colour.
   ONE CLASS DEEP, every one of them: the .tv-chips .tv-chip-muted rule below
   is what keeps a crumb quiet, and a selector heavier than that one outranks
   it. */
.tv-pal .tv-chip,.tv-summon .tv-chip{
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
.tv-pal .tv-chip-sort,.tv-summon .tv-chip-sort{
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
.tv-pal .tv-chip-cols,.tv-summon .tv-chip-cols{
  background:color-mix(in srgb,var(--tv-link) var(--tv-cols-wash),transparent);
  border-color:color-mix(in srgb,var(--tv-link) var(--tv-chip-edge),transparent);
}
/* THE SAVED-VIEW CHIP WEARS THE ACCENT — the fourth chip voice, and the token
   that names a whole view rather than shaping one. Same shape, same edge rule,
   same wash arithmetic as its two siblings, one hue over. Only a token naming a
   view the producer DECLARED wears it — "namesView" is that test — so a
   half-typed "view:" and a name nobody carries keep the ordinary chip. */
.tv-pal .tv-chip-view,.tv-summon .tv-chip-view{
  background:color-mix(in srgb,var(--tv-accent) var(--tv-cols-wash),transparent);
  border-color:color-mix(in srgb,var(--tv-accent) var(--tv-chip-edge),transparent);
}
.tv-pal .tv-chip:not(.tv-chip-muted):hover,
.tv-summon .tv-chip:not(.tv-chip-muted):hover{
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
/* THE SCROLLER SCROLLS AND DRAWS NO BAR. The rows are driven by key and by
   wheel, so the bar is a stripe of chrome that carries nothing the header and
   the count do not already say — and a classic bar takes LAYOUT WIDTH, which
   the fill column then loses and the sideways scroll begins a bar's width
   early. Both spellings: Firefox reads the property, Chromium the pseudo. */
.tv-scroll{
  overflow:auto;
  position:relative;
  scrollbar-width:none;
}
.tv-scroll::-webkit-scrollbar{
  width:0;
  height:0;
}
/* THE DOCK: A SUMMONED BOX COMES ON THE CHIP STRIP'S OWN ROW, a grid row the
   two share — the chips take their width, the box takes the slack — rather than
   a second stripe of chrome over the table or an overlay across it. The row is
   the strip's whether or not the box is in it, so summoning moves nothing but
   the box appearing beside the chips: tv-typing is that summons, put on
   before the focus (display:none takes no keys) and taken off by the blur.
   Every mount that docks wears these, the tv-inline picker included. */
.tv-dock{
  display:grid;
  grid-template-columns:auto minmax(0,1fr);
  align-items:center;
}
/* One hairline under the strip, and it is the scroller's own top edge. */
.tv-dock > .tv-chips{
  grid-area:1 / 1;
  border-bottom:none;
}
.tv-dock > .tv-chips:empty{
  display:none;
}
.tv-dock > .tv-bar{
  grid-area:1 / 2;
  display:none;
}
.tv-dock.tv-typing > .tv-bar{
  display:flex;
}
.tv-dock > .tv-scroll{
  grid-area:2 / 1 / 2 / -1;
  border-top:1px solid var(--tv-border);
}
/* THE HINT LINE IS THE THIRD ROW, spanning both columns. A picker has none, and
   a mount that keeps its page furniture has to say where this one goes: an
   auto-placed hint takes the first free cell, which carries its top edge across
   one column of the two. */
.tv-dock > .tv-hint{
  grid-area:3 / 1 / 3 / -1;
}
/* A DOCK OVER A WHOLE PAGE. The rows are named so the table takes the slack the
   strip and the hint line leave: under a max-height a grid sized by its content
   grows past the mount and is clipped, where the flex column this replaces
   shrank. The box then fills its half of the row, in the mount's own face and
   on the strip's own rhythm — a docked box is the page's control, and only the
   picker wants a small one. */
.tv-dock.tv-summon{
  grid-template-rows:auto minmax(0,1fr) auto;
}
/* THE ROW DOES NOT GROW WHEN THE BOX ARRIVES: the strip's own padding is the
   air the box is centred in, so the table under it does not jump 8px down on
   every summons. The right edge keeps the strip's rhythm; the left is the
   chips' own right padding, already there. */
.tv-dock.tv-summon > .tv-bar{
  padding:0 12px 0 0;
  border-bottom:none;
}
.tv-dock.tv-summon .tv-filter-wrap{
  flex:1 1 auto;
}
.tv-dock.tv-summon .tv-filter{
  flex:1 1 auto;
}
/* The row is centred, which the table declines: it takes the row it was given
   and scrolls inside it. */
.tv-dock.tv-summon > .tv-scroll{
  align-self:stretch;
  min-height:0;
}
/* INLINE: the host has already drawn the box, so the mount brings none of its
   own, caps its window and marks no order — a picker is chosen from, not sorted.
   ITS BOX IS SUMMONED, NOT RESIDENT, and the dock above is where it lands; what
   is left here is the compact furniture, the small face included. */
.tv-inline{
  border:none;
  border-radius:0;
}
.tv-inline .tv-scroll{
  max-height:calc(12 * 2.05em);
}
.tv-inline th .tv-arrow{
  display:none;
}
.tv-inline > .tv-chips{
  padding:5px 8px;
}
.tv-inline > .tv-bar{
  padding:5px 8px 5px 0;
}
.tv-inline .tv-filter{
  font-size:12px;
  padding:2px 6px;
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
/* WHAT A READER SCANS IS THE COLUMN OF WORDS, not the column of grounds. A pill
   sets its text in from the cell edge by its own padding, so a badge column's
   HEADER is set in by the same amount and the first letters line up: State over
   TODO, # over [#A]. The mark keeps the right edge, the padding riding on the
   flex row rather than on the cell. */
.tv-fill th.tv-badge .tv-hd{
  padding-left:var(--tv-pill-pad, 8px);
}
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
  padding:0 var(--tv-pill-pad, 8px);
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
    const composer = o.composer === true;
    // INLINE: the mount IS a small box someone else has drawn, so it keeps the
    // rows and summons the filter, dropping the page furniture — no title, no
    // hint line, no sort marks, and a capped window.  It is what a PICKER hung
    // at a caret wants, where the ordinary mount wants the page.
    const palette = o.palette === true;
    // PALETTE WINS the pair: it draws its own overlay, and the two modes give
    // Backspace and Escape opposite answers.  One flag decides, so no branch
    // has to agree with another about which came first.
    const inline = o.inline === true && !palette;
    /**
     * THE DOCK IS WHERE A SUMMONED BOX LANDS.  `"overlay"' raises it over the
     * page behind the veil; `"strip"' lays it on the chip strip's own row, the
     * chips taking their width and the box the slack.  Named, the option
     * decides; absent, the mode does — `palette' summons over the page,
     * `inline' onto the strip, and a plain mount summons nothing at all.
     * @type {"overlay"|"strip"|"none"}
     */
    const dock = o.filterDock === "overlay" || o.filterDock === "strip" ? o.filterDock
               : palette ? "overlay" : inline ? "strip" : "none";
    /**
     * THE SUMMONED LADDER IS THE PAGE'S OWN BOX, whichever dock it landed in:
     * Escape in two steps (the typed text, then the box), a DEAD Backspace over
     * an emptied box — the chips are on the page behind it, not in it — and a
     * query delivered on COMMIT alone, since narrowing as each character lands
     * animates a table the typist is looking away from.  The picker (`inline')
     * summons a box too and answers with its own rungs: one Escape, a Backspace
     * that eats the box, and rows that narrow as it is typed, because a compact
     * table is a thing to pick FROM.  So it is spelled out of this predicate,
     * and the sites that are its own name it.
     */
    const summoned = dock !== "none" && !inline;
    const omnibox = o.omnibox === true || composer || inline;
    const marks = o.marks === true;
    /**
     * Whether the FLAG state is drawn.  Absent it follows `marks', the one
     * opt-in flags shipped under.  Named, it is its own answer: `flags: true'
     * alone draws the flag ground gutterless, `flags: false' under
     * `marks: true' takes the flag drawing back off.
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
     * How a live chip should read — `(token) => string|null', anything but a
     * non-empty string leaving the token raw.  Display only.
     * @type {((token: string) => string|null)|null}
     */
    const chipLabel = typeof o.chipLabel === "function" ? o.chipLabel : null;
    /**
     * The PIN: a button-badge at the chip strip's far edge, present only under
     * `onPin'.  The renderer reports the click and wears the boolean; the
     * consumer decides both, knowing what the query is compared against.
     * @type {(() => void)|null}
     */
    const onPin = typeof o.onPin === "function" ? o.onPin : null;
    /**
     * What a NARROWED session says about a token it will not take —
     * `(token) => void', handed the source text as the reader wrote it, once
     * per spelling.  This side refuses; naming the other door is the
     * consumer's sentence, in the consumer's own words.
     * @type {((token: string) => void)|null}
     */
    const onRefused = typeof o.onRefused === "function" ? o.onRefused : null;
    let pinned = !!o.pinned;
    /**
     * How many chrome cells lead a row; what a column index has to skip.  The
     * gutter is the CHECKBOX's alone — the flag's edge rides the row's first
     * cell — so a mount that flags without marking pays no leading column.
     */
    const chrome = marks ? 1 : 0;
    /** Rows per page, or 0 for the whole set at once — which is every consumer
     *  that has not asked otherwise. */
    const pageSize = Math.max(0, Math.trunc(Number(o.pageSize) || 0));
    /** The page on show, counted from zero. */
    let page = 0;
    /**
     * Which of the two presentations a paged view is in.  PAGED (false) is the
     * slice, the virtualizer running inside one page.  CONTINUOUS (true) runs
     * the window over the whole ordered set; stepping off the end of a page
     * switches at that moment, and any explicit turn snaps back.
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

    // two row lists between store and window: 'sorted' (all, in sort order) and 'order'
    // ('sorted' under the filter). Filter re-derives 'order' only; upsert/delete splice both; rows/sort drops both.

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
     * Per column over `order': max display length in characters, and the ground
     * its cells sit on in px. Null when stale.
     * @type {{ch: number, ground: number}[]|null}
     */
    let widths = null;
    /** @type {Map<string, RowText>} */
    const texts = new Map();
    /**
     * A column's distinct cell values, for the suggestion list — computed on
     * demand and thrown away with the text cache it was read off.
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
      // the list-column and date-ness verdicts are read off the rows and die with them:
      // cached, an empty or early mount would decide "no such column" and never look again.
      multiAt = undefined;
      dateAt = undefined;
      queueIndex();
    }

    /**
     * Build the word index once the rows have stopped moving, off the path a
     * keystroke takes.  An edit burst re-queues it, so only the quiet at the
     * end pays.  A keystroke arriving first builds one itself, which is the
     * cost this avoids and the worst case it cannot exceed.
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
     * spell, and the rows behind each.  What `tag:' completes against — a raw
     * `:a:b:' cell can never prefix-match a bare word.  Thrown away with the
     * text cache it was read off.
     * @type {{list: string[], ids: Map<string, Set<string>>}|null}
     */
    let vocab = null;

    /**
     * Does column I hold cells of a shape, read off up to `SAMPLE' non-empty
     * ones?  `SHAPED' cells carrying the shape and none arguing against it
     * settle it.  A cell that merely fails to be the shape ABSTAINS — asking
     * every sampled cell to carry it lets one stray decide a whole column.
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
     * The multi-valued column's index, or -1.  Decided by looking at the CELLS
     * rather than the column's name: glance's key has been `tags' and is `tag',
     * and neither spelling is the renderer's business.
     */
    function multiColumn() {
      if (multiAt !== undefined) return multiAt;
      const cols = columns();
      multiAt = -1;
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

    // chrome built once; the filter input is never recreated, so it keeps focus and caret across updates.

    const calm = typeof matchMedia === "function"
              && matchMedia("(prefers-reduced-motion: reduce)").matches;

    const root = document.createElement("div");
    root.className = classAttr([["tv-root", true], ["tv-marking", marks],
                                ["tv-calm", calm], ["tv-omni", omnibox && !palette],
                                ["tv-pal", palette], ["tv-inline", inline],
                                ["tv-dock", dock === "strip"],
                                ["tv-summon", summoned]]);
    container.innerHTML = "";
    container.appendChild(root);

    const bar = document.createElement("div");
    bar.className = "tv-bar";
    const titleEl = document.createElement("span");
    titleEl.className = "tv-title";
    const input = document.createElement("input");
    input.className = "tv-filter";
    input.type = "search";
    /**
     * WHAT THE BOX TAKES, spelled in the grammar it takes.  The whole door
     * offers the grammar; the narrowed one names the half it edits first, the
     * keys it refuses being spelled nowhere in what it offers.
     */
    const WHOLE_HINT = `key:value · status:open|closed · -word · "some phrase"`;
    const NARROW_HINT = `filter rows · ${WHOLE_HINT}`;
    input.placeholder = WHOLE_HINT;
    const chipsEl = document.createElement("div");
    chipsEl.className = "tv-chips";
    const filterWrap = document.createElement("div");
    filterWrap.className = "tv-filter-wrap";
    const acEl = document.createElement("div");
    acEl.className = "tv-ac";
    acEl.style.display = "none";
    filterWrap.appendChild(input);
    filterWrap.appendChild(acEl);
    // A SUMMONED MOUNT DRAWS NO TITLE and hangs its chips in a row of their
    // own: the bar it would sit in belongs to the box, which is away until it
    // is called for, and a title that comes and goes with the box is no title.
    if (!omnibox && !summoned) { bar.appendChild(titleEl); bar.appendChild(chipsEl); }
    if (dock !== "overlay") bar.appendChild(filterWrap);

    const veil = document.createElement("div");
    veil.className = "tv-veil";
    veil.style.display = "none";
    const panel = document.createElement("div");
    panel.className = "tv-panel";
    if (dock === "overlay") {
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

    const hasHint = !composer && !inline;
    if (dock !== "overlay") root.appendChild(bar);
    if (omnibox || summoned) root.appendChild(chipsEl);
    if (!composer) root.appendChild(scroll);
    if (hasHint) root.appendChild(hint);
    if (dock === "overlay") root.appendChild(veil);

    /** Per-column <col>, one per column. @type {HTMLElement[]} */
    let colEls = [];
    /** Per-column sort arrow, one per column. @type {HTMLElement[]} */
    let arrowEls = [];

    const geom = { row: ROW_H, head: ROW_H };
    /**
     * The window in the tbody: its half-open span, and the display order it was
     * DRAWN from.  `renderRows' is the only writer, so the k-th data row is
     * `rows[first + k]' by construction — which is how `stampSelection' gets
     * back to the row behind a `tr' without asking the state a second time.
     * @type {{ first: number, last: number, rows: Row[] }}
     */
    const win = { first: -1, last: -1, rows: [] };
    let remeasuring = false;
    /** The selected row's index in display order, or -1; what the bar reads. */
    let selAt = -1;

    /**
     * Read SCHEMA's sort list into sort keys.  A `direction' string wins over
     * `ascending': bare "asc" and "desc" put empty cells last whatever the
     * column type.  With no `direction' a boolean `nullsFirst' is read instead,
     * which is the shape `getSort' answers in, so a chain read out and handed
     * back to `setSort' is the chain that was read.
     * @param {Sort|Sort[]|null} [sort]  falsy is the empty chain; a `SortKey'
     *   satisfies `Sort', so `getSort''s answer goes back in unchanged
     * @returns {SortKey[]}
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
     * Every key a token may name: the view's columns, then `planned' and `sort'
     * where no column already carries the name.  One spelling for the two places
     * that ask.  The view's own, and nothing the rows imply — an org tag names
     * no key, or the same token would mean two things on one wire.
     */
    function queryKeys() {
      const keys = columnKeys();
      for (const k of [PLANNED_KEY, SUBSTRING_KEY])
        if (keys.indexOf(k) === -1) keys.push(k);
      for (const k of VIEW_KEYS) if (keys.indexOf(k) === -1) keys.push(k);
      return keys;
    }

    /**
     * The keys a KEY-STAGE offer may name: every key, or the narrowing half
     * alone while the box is narrowed.  `queryKeys' stays whole either way — a
     * shaping token has to PARSE as one to be refused rather than read as free
     * text — so these two answers are one list and its subset.
     * @returns {string[]}
     */
    function offeredKeys() {
      return narrowing ? queryKeys().filter((k) => !shapesView(k)) : queryKeys();
    }

    /**
     * The order in force under query Q: the chain Q names, else the order the
     * view was STATED in.  `sort:*none*' names the EMPTY chain, a divergence
     * like any other rather than a fall back to the declared order.
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
     * once.  Sampled by `dateColumn' rather than named, so a page carrying
     * fewer than two dated rows finds none and `planned' narrows where a
     * producer's own would not (SCHEMA, Filter query).  Cached like
     * `multiColumn''s verdict and thrown away with it.
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
     * The day `*today*' resolves to: ONE CLOCK READ PER QUERY, taken at the
     * head of `queryMatcher' before any row.  Every atom of one query then
     * names one day, whatever the clock does while that query is applied.
     */
    let compiledDay = localDay();

    /**
     * Does KEY name DATE cells alone?  The comparison forms are read there and
     * nowhere else, so `title:>x' is the substring it has always been.
     * @param {string} key  @returns {boolean}
     */
    function datedKey(key) {
      const cells = fieldCells(key);
      const dc = dateColumns();
      return !!cells && cells.length > 0 && cells.every((i) => dc.indexOf(i) !== -1);
    }

    /**
     * VALUE's alternatives that name an ATOM on KEY.  A comparison left with no
     * literal — `scheduled:>', `scheduled:2026-08..' — is half-typed and drops,
     * so it narrows nothing and establishes no axis, the way `state:' does.
     * @param {string} key  @param {string} value  @returns {string[]}
     */
    function atomsIn(key, value) {
      const alts = alternatives(value);
      if (!datedKey(key)) return alts;
      // A LITERAL IS OWED AT EVERY END THE VALUE NAMES: behind the operator, and
      // on both sides of the separator — which is the whole of what `dateValue'
      // leaves empty.
      return alts.filter((v) => {
        const d = dateValue(v);
        return d.lo !== "" && (d.op !== RANGE || d.hi !== "");
      });
    }

    /**
     * The atoms TOK offers its axis: a predicate's alternatives that name one
     * HERE, or free text's own word.  The bar is a PREDICATE's, so free text
     * reads its whole value and `+|' is one atom.
     * @param {Token} tok  @returns {string[]}
     */
    function atomsOf(tok) {
      return tok.key === null ? (tok.value ? [tok.value] : [])
                              : atomsIn(tok.key, tok.value.toLowerCase());
    }

    /**
     * Does TOK narrow nothing and establish no axis?  AN UNSIGNED OR ADDED
     * TOKEN NAMING NO ATOM — `state:', `+state:', `+state:|', a lone `+' — is
     * dropped ahead of the grouping: left standing it saturates its axis's
     * disjunction, so `state: +state:DONE' would serve every row where it must
     * serve the DONE ones.  A NEGATED one keeps its own law, and `-state:'
     * still empties the table.  A date key rides the same rule while it is
     * being typed, its half-typed comparison naming no atom, so `+scheduled:>'
     * saturates nothing.  ATOMS is TOK's own, read once by `queryMatcher'.
     * @param {Token} tok  @param {string[]} atoms  @returns {boolean}
     */
    function vacuousHere(tok, atoms) {
      return !tok.negated && atoms.length === 0;
    }

    /**
     * TOK as a row test, negation aside — `queryMatcher' applies that.  Free
     * text is a substring of the whole row, bar and all: alternation is a
     * PREDICATE's rule.  A predicate passes on ANY of ATOMS, which is what its
     * value left (`atomsOf') and is read once by `queryMatcher'.  Built once per
     * query, never per row.
     * @param {Token} tok  @param {string[]} atoms  @returns {(r: Row) => boolean}
     */
    function tokenTest(tok, atoms) {
      // filter grammar (tokenTest, metas): docs/web-renderer.org — mirrors SCHEMA.md
      if (tok.key === null) return freeTest(tok.value.toLowerCase());
      const key = tok.key;
      if (!atoms.length) return () => true;        // half-typed: narrows nothing
      if (atoms.length === 1) return valueTest(key, atoms[0]);
      const tests = atoms.map((v) => valueTest(key, v));
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
     * `planned'.  Null is "no such key", a different answer from "no cells":
     * `planned' over a page carrying no date column names NOTHING and finds
     * nothing, where an unknown key narrows nothing at all.  A column of that
     * name shadows the reserved key.
     * @param {string} key  @returns {number[]|null}
     */
    function fieldCells(key) {
      const col = colByKey(key);
      if (col) return [columns().indexOf(col)];
      return key === PLANNED_KEY ? dateColumns() : null;
    }

    /**
     * `KEY:V' as a row test for ONE alternative; V is lowercased and non-empty.
     * ONE reading over the cells the key names: `*empty*' asks that they ALL be
     * empty, any other value that ANY of them pass by its own column's
     * semantics (`cellTest').
     * @param {string} key  @param {string} v  @returns {(r: Row) => boolean}
     */
    function valueTest(key, v) {
      if (key === SUBSTRING_KEY && !colByKey(key)) return freeTest(v);
      const cells = fieldCells(key);
      if (!cells) return () => true;             // no such key: narrows nothing
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
      if (i === multiColumn() && META.test(v)) {
        const want = starless(v);
        return (r) => tagsIn(rowText(r).cells[i]).indexOf(want) !== -1;
      }
      if (col && col.type === "badge") {
        if (v === ACTIVE_META) return (r) => rowText(r).cells[i] === "";
        const want = undecorated(v), worn = `[#${want}]`;
        return (r) => { const c = rowText(r).cells[i]; return c === want || c === worn; };
      }
      if (dateColumn(i)) return stampTest(i, v);
      return (r) => rowText(r).cells[i].includes(v);
    }

    /**
     * V as a test of DATE cell I: the bare prefix, one of the four comparisons,
     * or the range `A..B'.  `*today*' stands for `compiledDay' wherever a
     * literal may.  THREE PIECES, ONE GUARD: `cmpTest' carries the granularity
     * law, `dated' the empty cell, and the range is the two inclusives composed
     * under ONE guard rather than a table arm of its own.
     * @param {number} i  @param {string} v  @returns {(r: Row) => boolean}
     */
    function stampTest(i, v) {
      const d = dateValue(v);
      const lo = d.lo === TODAY_META ? compiledDay : d.lo;
      const hi = d.hi === TODAY_META ? compiledDay : d.hi;
      // THE BARE ARM CARRIES NO `dated' GUARD and needs none — a non-empty
      // literal is the prefix of no empty cell — which is what keeps it byte
      // for byte the arm it was.
      if (d.op === "") return (r) => rowText(r).cells[i].startsWith(lo);
      // A LITERAL THAT DOES NOT OPEN WITH A DIGIT IS NO DATE and matches no
      // row, the reading `state:TOD' has — where byte order would happily
      // serve every dated row against `>*empty*'.  `*today*' resolved above,
      // so it is a date by the time this asks.
      if (!DATE_LIT.test(lo)) return () => false;
      if (d.op === RANGE && !DATE_LIT.test(hi)) return () => false;
      // A RANGE IS ONE ATOM, which is the whole of what two tokens cannot say:
      // asked of each cell in turn (`valueTest'), `planned:A..B' is ONE date
      // cell inside the interval where `planned:>=A planned:<=B' lets either
      // cell answer either end.
      const holds = d.op === RANGE
        ? dated((c) => cmpTest(CMP_GE, lo, c) && cmpTest(CMP_LE, hi, c))
        : dated((c) => cmpTest(d.op, lo, c));
      return (r) => holds(rowText(r).cells[i]);
    }

    /**
     * ONE axis as a single test: its plain and negated tokens AND, and its
     * ADDED ones OR against that conjunction.  An axis of added tokens alone is
     * the disjunction, so a lone `+tag:work' is `tag:work'.
     * @param {{base: ((r: Row) => boolean)[], wide: ((r: Row) => boolean)[]}} ax
     * @returns {(r: Row) => boolean}
     */
    function axisTest(ax) {
      const base = ax.base, wide = ax.wide;
      /** @param {Row} r */
      const every = (r) => { for (const t of base) if (!t(r)) return false; return true; };
      if (!wide.length) return base.length === 1 ? base[0] : every;   // an unwidened axis: one AND
      const some = base.length > 0;
      return (r) => {
        if (some && every(r)) return true;
        for (const t of wide) if (t(r)) return true;
        return false;
      };
    }

    /**
     * Q compiled to a row test, or null when it filters nothing.  Built once
     * per filter change and reused for every row.
     *
     * ONE rule: AXES AND, ALTERNATIVES OR, AND WITHIN AN AXIS THE `+' TOKENS OR
     * AGAINST WHAT THE OTHERS AND.  An axis is a KEY — each column's own,
     * `planned''s own, and free text sharing `substring''s — so grouping is by
     * key and never by adjacency and token order carries nothing; the
     * alternatives' OR lives inside `tokenTest'.  A VACUOUS token establishes no
     * axis and is dropped ahead of the grouping, so `state: +state:DONE' is the
     * DONE rows.  A `sort' token is the exception, being no predicate: it states
     * the ORDER (`chainFor') and contributes no test in either polarity.
     * @param {string} q  @returns {((r: Row) => boolean)|null}
     */
    function queryMatcher(q) {
      /** @type {Map<string, {base: ((r: Row) => boolean)[], wide: ((r: Row) => boolean)[]}>} */
      const axes = new Map();
      compiledDay = localDay();   // one clock read, before any row
      for (const tok of parseQuery(q, queryKeys())) {
        if (tok.key && VIEW_KEYS.indexOf(tok.key) !== -1) continue;
        // ONE READING PER TOKEN: the same atoms decide whether it is dropped and
        // what it tests, and reaching them samples the columns (`datedKey').
        const atoms = atomsOf(tok);
        if (vacuousHere(tok, atoms)) continue;
        const key = tok.key === null ? SUBSTRING_KEY : tok.key;
        let ax = axes.get(key);
        if (!ax) { ax = { base: [], wide: [] }; axes.set(key, ax); }
        const test = tokenTest(tok, atoms);
        if (tok.added) ax.wide.push(test);
        else ax.base.push(tok.negated ? (r) => !test(r) : test);
      }
      /** @type {((r: Row) => boolean)[]} */
      const musts = [];
      for (const ax of axes.values()) musts.push(axisTest(ax));
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
     * The rows on show: one page of the filtered, sorted set, or all of it with
     * no page size.  Everything that renders rows reads this, so the
     * virtualizer works inside the page and knows nothing about paging.  Widths
     * are the exception, measuring the whole filtered set or columns would jump
     * every time the page turned.
     */
    function paged() {
      const rows = ordered();
      if (!pageSize || continuous) return rows;
      if (page >= pageCount()) page = pageCount() - 1;   // the set shrank under it
      const at = page * pageSize;
      return rows.slice(at, at + pageSize);
    }

    /**
     * The page the cursor sits in, from zero.  In PAGED that is `page' by
     * construction; in CONTINUOUS it is derived from where the selection
     * landed, which is what makes the pager move as the cursor crosses a
     * boundary.  With nothing selected it falls back to `page'.
     */
    function cursorPage() {
      if (!pageSize) return 0;
      if (!continuous) return Math.min(page, pageCount() - 1);
      const i = state.selected === null
        ? -1 : ordered().findIndex((r) => r.id === state.selected);
      return i === -1 ? Math.min(page, pageCount() - 1) : Math.floor(i / pageSize);
    }

    /**
     * The rows a reader would call "on show": one page of the filtered set, or
     * all of it with no page size.  Stays the CURSOR's page even in CONTINUOUS
     * presentation, so "shown" means one thing across the handle.
     * @returns {Row[]}
     */
    function shownRows() {
      const rows = ordered();
      if (!pageSize || !continuous) return paged();
      const at = cursorPage() * pageSize;
      return rows.slice(at, at + pageSize);
    }

    /**
     * Go continuous, keeping the viewport exactly where it is: a row at index i
     * is now at `page * pageSize + i', and the scroller moves by that difference
     * in the same breath, which is what makes the switch invisible.
     */
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
     * Column widths.  Under the FILL POLICY the CELLS decide and a header
     * widens nothing: a column of `[#A]' badges reads exactly as wide as
     * `[#A]', a longer header ellipsizing into it.  A column holding no cell
     * has only its header to measure.  Without a `title' column to fill, the
     * width is the widest cell or the header and its mark.
     *
     * TEXT IN `ch', GROUNDS IN `px' — the units each is spent in, so a pill
     * allowed for in characters is right at one font size and short at the rest.
     * @returns {{ch: number, ground: number}[]}
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
        // column geometry (header marks paid outside the cells' measure): docs/web-renderer.org
        const mark = at === -1 ? 0 : sortMark(chain, at).length + 1;
        const head = String(c.header || c.key).length;
        const pill = c.type === "badge" && cell[i] ? PILL_PAD : 0;
        return { ch: fill ? (cell[i] || head) + mark
                          : Math.max(head + mark, cell[i]),
                 ground: CELL_PAD + pill };
      });
      return widths;
    }

    /** Widen the cached widths for ROW (an upsert can only add text). */
    function growWidths(r) {
      if (!widths) return;
      const len = rowText(r).len, cols = columns();
      for (let i = 0; i < widths.length; i++) {
        if (len[i] > widths[i].ch) widths[i].ch = len[i];
        if (len[i] && cols[i].type === "badge")
          widths[i].ground = CELL_PAD + PILL_PAD;
      }
    }

    /**
     * Write the measured widths onto the columns under the FILL POLICY.  The
     * `title' column gets no width at all — under the fixed layout the one
     * column without one absorbs the remainder — so what is written here is the
     * OTHERS.  Only the table's `min-width' knows the title exists: the sized
     * columns plus the title's floor, which is where a narrow window starts
     * scrolling sideways instead of crushing the title.  A view carrying no
     * `title' column keeps the widths as hints under the auto layout.
     */
    function applyWidths() {
      const w = colWidths(), at = titleColumn(), fill = at !== -1;
      if (table.classList.contains("tv-fill") !== fill)
        table.classList.toggle("tv-fill", fill);
      let ch = fill ? Math.min(w[at].ch, TITLE_MIN_CH) : 0;
      let pad = fill ? w[at].ground : 0;
      if (fill && chrome) { ch += BOX_CH; pad += CELL_PAD; }
      for (let i = 0; i < colEls.length; i++) {
        const n = fill ? Math.min(w[i].ch, COL_MAX_CH) : w[i].ch;
        const px = fill && i === at ? "" : `calc(${n}ch + ${w[i].ground}px)`;
        if (fill && i !== at) { ch += n; pad += w[i].ground; }
        if (colEls[i].style.width !== px) colEls[i].style.width = px;
      }
      const min = fill ? `calc(${ch}ch + ${pad}px)` : "";
      if (table.style.minWidth !== min) table.style.minWidth = min;
    }


    /** Rebuild the colgroup and the header row (mount, and a view change). */
    function renderHead() {
      colgroup.innerHTML = "";
      headRow.innerHTML = "";
      colEls = [];
      arrowEls = [];
      // the gutter is nobody's column — left out of colEls/arrowEls so widths and sort arrows keep their indexing.
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
          + (c.align === "right" ? " tv-right" : "")
          + (c.type === "badge" ? " tv-badge" : "");
        th.dataset.key = c.key;
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


    // class derivation is single-source, as [name, on] pairs: rowHTML joins the names that are on,
    // stampSelection toggles each. Spelled twice, build and re-stamp could disagree; re-stamp (not rebuild) lets marks crossfade.

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
      // overscan covers the rounding and the band the sticky header hides.
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
      if (!hasHint) return;             // the node was never appended
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
     * `scrollHeight' answers wherever the question is about the rows on show:
     * `geom.row' is a ROUNDING of the row height, so `geom.head + rows *
     * geom.row' runs a fraction short PER ROW — twenty pixels over a hundred
     * rows, which parks the tail under the hint bar.  At a page turn and at the
     * continuous seam the TBODY is not yet the rows on show, and there the
     * modelled sum is all there is until the render lands; the row count is
     * what tells the two apart.
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
      let out = pageCount() > 1 ? pagerHTML() : `${esc(count)}`;
      out += ` · ${esc(sort)}`;
      if (actionHints)
        for (const a of actions()) {
          if (!a.key) continue;
          out += ` · <b class="tv-key">${esc(a.key)}</b> ${esc(a.label || a.command)}`;
        }
      // the mark count is of every mark (filtered- or paged-away included) — the number a bulk action runs over.
      if (marks && markSet.ids.size)
        out = `${esc(grouped(markSet.ids.size))} marked · ${out}`;
      if (flags && flagSet.ids.size) {
        // the flag-helper text is the consumer's own string; a renderer inventing keys would assert a keymap it doesn't own.
        const help = flagHelp && state.selected !== null && flagSet.ids.has(state.selected)
          ? ` · ${flagHelpHTML}` : "";
        out = `${esc(grouped(flagSet.ids.size))} flagged${help} · ${out}`;
      }
      return out;
    }

    /**
     * COL as a real column index, or null for a whole-row selection — which is
     * what a column outside the table is.  A consumer steps by reading the
     * column and handing back one more, so the index past an end is a reader
     * walking off the cells onto the row they are still on.
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
     * The row and column state the window wears, re-derived rather than
     * rebuilt — which is what leaves the grounds something to crossfade
     * between.  One pass, a no-op wherever the element already agrees.  Only
     * the window is stamped; the rows outside it have no elements.
     *
     * The row and index behind a `tr' come out of `win', which holds the order
     * the tbody was DRAWN from: asking `paged()' again would get an answer the
     * DOM had never been told about.
     */
    function stampSelection() {
      const trs = tbody.children;
      let k = 0;
      for (let i = 0; i < trs.length; i++) {
        const tr = /** @type {HTMLElement} */ (trs[i]);
        if (tr.dataset.id === undefined) continue;      // a spacer, not a row
        const at = win.first + k++, r = win.rows[at];
        stampClasses(tr, rowClasses(r, at));
        const linkedAt = linkedCell(r), tds = tr.children;
        for (let c = chrome; c < tds.length; c++)
          stampClasses(tds[c], cellClasses(r, c - chrome, linkedAt));
      }
      const ths = headRow.children;
      for (let c = chrome; c < ths.length; c++)
        ths[c].classList.toggle("tv-colsel", c - chrome === state.selCol);
    }


    // marks and flags are one mechanism, two id-keyed sets: keyed by id so a mark survives
    // re-send/re-sort/filter/page; two sets because two questions (a flag is a pending action, a mark a standing selection).

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
     * Mark every row of the CURRENT FILTERED SET — all of it, never the page on
     * show.  Idempotent.  `addAll' is the MECHANISM's and both states hold it;
     * this is where the HANDLE offers it, on marks alone, `unflagRow' being the
     * same asymmetry on flags.
     * @returns {number} how many rows carry a mark afterwards
     */
    function markAll() {
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
     * view.  Rows outside the rendered window have no element to click, so this
     * is how a consumer moves the selection.  False when no visible row has
     * that id.  A COL outside the columns that exist selects none of them, so a
     * consumer stepping past either end lands on the whole-row selection.
     *
     * This scrolls, keeping a margin under the cursor; a CLICK must not, the
     * row being under the pointer already — the delegated handler goes through
     * `setSelected', which moves the marks and nothing else.
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
     * in between.  A held movement key fires ~30 a second, each a scroll
     * adjustment and a window rewrite; per event is what makes it stutter.  The
     * state is already correct, so the frame only has to paint.
     */
    function paintSelection(was) {
      wantSelection = true;
      if (flagHelp) wantHint = true;
      if (selAt >= 0) easeToRow(selAt, was === undefined ? selAt : was);
      schedule();
    }

    // one frame loop drives window + marks + ease; two schedulers would re-render the tbody twice a frame and race scrollTop.

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
        // re-read geom here, where the tick owns the frame, or an ease parks short against a height an earlier frame read.
        measure();
        const port = scroll.clientHeight || 0;
        if (port) easeAt = aimed(port);
        const step = easeAt - scroll.scrollTop;
        if (Math.abs(step) < SNAP_PX) { scroll.scrollTop = easeAt; easing = false; }
        else {
          const was = scroll.scrollTop;
          scroll.scrollTop = was + step * EASE;
          // a refused step is an arrival: scrollTop snaps to a device pixel past scrollHeight's fractional end, so ending only on arrival loops forever.
          if (scroll.scrollTop === was) easing = false;
        }
        wantWindow = true;
      }
      if (wantWindow || wantSelection) renderRows();
      if (wantSelection) stampSelection();
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
     * the row's foot stops at two thirds of the port; moving up, its head stops
     * at one third; between those the viewport holds still.  Clamped to the
     * content, so at either end the cursor walks into the margin.
     *
     * Retargeting rather than queueing: a held key lands a new target every
     * 30ms or so and the one loop heads for the latest.  The aim is taken from
     * where the ease is GOING rather than where it is, or each keypress would
     * re-derive against a viewport in flight and creep.  The three inputs are
     * kept rather than their answer, so the frame loop works the target out
     * again as rows measure what they measure when drawn.
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
     * WHICH HALF OF THE GRAMMAR THE BOX EDITS.  Narrowed, it offers the
     * narrowing keys alone and refuses a shaping token on commit; whole, it
     * takes everything.  The flag is the SESSION's — every summons states it
     * and the box closing clears it — so a plain `openFilter' is the whole
     * grammar however the last one opened.
     */
    let narrowing = false;

    /** The refusals already spoken this session; one spelling is echoed once.
     *  @type {Set<string>} */
    let spoken = new Set();

    /**
     * Summon the control, WHERE THE DOCK PUTS IT: the overlay dock raises the
     * veil, the strip dock draws the box onto the chips' line, and a mount that
     * docks nowhere has it on the page already and only takes it. Either way it
     * is the one entry point a consumer's key binds to.
     *
     * HOW.NARROW OPENS THE FILTER HALF: this session offers the narrowing keys
     * alone and refuses a shaping token on commit (`chipUp'), while the chips
     * already standing ride along untouched — the strip is not the box.
     * @param {{narrow?: boolean}} [how]
     */
    function openFilter(how) {
      narrowing = !!(how && how.narrow === true);
      spoken = new Set();
      input.placeholder = narrowing ? NARROW_HINT : WHOLE_HINT;
      if (dock === "overlay") veil.style.display = "";
      // before the focus: the docked box is display:none until the class is on,
      // and a box that is not drawn takes no keys.
      if (dock === "strip") root.classList.add("tv-typing");
      input.focus();
      if (input.select) input.select();
    }

    /** Put it away again, and give the keyboard back to the table. */
    function closeFilter() {
      closeAc();
      if (dock === "overlay") veil.style.display = "none";
      input.blur();          // the blur listener un-summons; one owner for the class
    }

    /**
     * The narrowed session ends with the box, and ITS REFUSALS END WITH IT: the
     * shaping tokens left standing are taken out on the way, so a token this
     * door would not deliver cannot ride the next gesture that reads the box.
     */
    function endNarrow() {
      if (!narrowing) return;
      const kept = typedQuery();          // the box, less what it refused
      narrowing = false;
      spoken = new Set();
      input.placeholder = WHOLE_HINT;
      if (kept !== input.value.trim()) input.value = kept;
    }

    /** Drop what is half-typed, answering whether there was any.
     *  @returns {boolean} */
    function clearTyped() {
      if (!input.value) return false;
      input.value = "";
      closeAc();
      deliver();
      return true;
    }

    /** True while the filter box holds the keyboard. @returns {boolean} */
    const filtering = () => document.activeElement === input;

    /**
     * Escape's ONE STEP out of `inline''s editor: the half-typed filter is
     * dropped AND the cursor lands on a row.  A compact table is a thing to pick
     * FROM, so an emptied box is an editor the reader was already done with.
     * Backspace over an already-empty box lands in the same place, through
     * `handOver' alone — there is nothing left for it to drop.
     */
    function abandonFilter() {
      clearTyped();
      handOver();
    }

    /**
     * The end of every ladder: the table takes the selection and the control
     * goes. For a summoned box going means the box is gone — the overlay
     * dissolves, the docked box leaves the strip — which is the same gesture
     * one step further out than a resident box's blur. The chips stay either
     * way: they are the page's, not the box's.
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
      if (at === page && !continuous) return false;
      const col = state.selCol;
      continuous = false;
      page = at;
      // continuous mode can reach this empty where paged can't: a producer emptying the set mid-glide leaves a rows-less snap-back.
      const rows = paged();
      if (!rows.length) { renderRows(true); return true; }
      const first = land === "first";
      scroll.scrollTop = first ? 0 : maxScroll(scroll.clientHeight || 0);
      easing = false;
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
     * PROMOTE KEY to the head of the sort chain, ascending, the chain it had
     * shifting down behind it and KEY dropped from wherever it sat below — a
     * chain never names a column twice.  KEY already leading instead FLIPS its
     * direction and leaves the keys behind it where they are.
     *
     * The chain is WRITTEN INTO THE QUERY as ONE arrow-form token
     * (`sort:title->state:desc') and delivered like any other filter change, so
     * one representation carries the order everywhere; `deliver' puts it in
     * force.  What it composes onto is the order IN FORCE, declared chain and
     * all, so only the promoted key ever moves and no tie-breaker is lost.
     *
     * `sortable' gates this, the way it gates a header click: promotion is a
     * READER's gesture.  `sortBy' is the producer's — ungated, replacing the
     * chain outright and touching no query.
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
      if (th) { sortPromote(String(th.dataset.key)); return; }
      const tr = /** @type {HTMLElement|null} */ (t.closest("tr[data-id]"));
      if (!tr) return;
      if (onBox(t)) {
        if (tr.dataset.id !== undefined) markSet.toggle(tr.dataset.id);
        return;
      }
      setSelected(tr.dataset.id ?? null,
                  colOf(tr, /** @type {HTMLElement|null} */ (t.closest("td"))));
    });

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
      // a press on the box stays the box; else the completing touchend swallows the toggle's click and the 44px target can't be checked.
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
    for (const how of ["wheel", "touchmove", "pointerdown", "keydown"])
      scroll.addEventListener(how, cancelEase);


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

    /**
     * WHAT THE BOX CONTRIBUTES: its text, less every shaping token while the
     * session is narrowed.  Such a token is refused and left where the reader
     * can see it, and a refusal the producer still received would be no refusal
     * at all.  The CHIPS are read whole either way — the strip is not the box.
     * @returns {string}
     */
    function typedQuery() {
      const v = input.value;
      if (!narrowing || !v.trim()) return v.trim();
      const kept = [];
      for (const t of parseQuery(v, queryKeys()))
        if (!shapesView(t.key)) kept.push(v.slice(t.start, t.end));
      return kept.join(" ");
    }

    /** The query as it stands: every chip, then whatever is in the box. */
    function effectiveQuery() {
      const typed = typedQuery();
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
     * How a live chip reads.  A `chipLabel' formatter may alias the token; the
     * QUERY is untouched, so `getQuery', `onFilter' and what a click takes off
     * are all still the token as written.  Crumbs never reach this — a crumb's
     * label IS its label.
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
     * THE SIGN IS THE HEAD and rides the spelling: `+bread' draws as
     * `+substring:bread', the way `-bread' draws as `-substring:bread'.
     * The QUERY keeps what the reader typed — this is the label alone.
     * @param {string} tok  @returns {string}
     */
    function spelled(tok) {
      const t = asToken(tok);
      if (!t || t.key !== null || !t.value) return tok;
      const value = /[\s&"]/.test(t.value) ? `"${t.value}"` : t.value;
      return `${signMark(t)}${SUBSTRING_KEY}:${value}`;
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

    function renderChips() {
      let html = "";
      for (const text of crumbStrip())
        html += `<span class="tv-chip tv-chip-muted">${esc(text)}</span>`;
      for (let i = 0; i < chips.length; i++)
        html += `<span class="tv-chip${chipClassOf(chips[i])}"`
              + ` data-i="${i}" title="remove">${esc(chipText(chips[i]))}`
              + `<i class="tv-chip-x">×</i></span>`;
      if (onPin)
        html += `<span class="tv-pin${pinned ? " tv-pinned" : ""}" title="${
          pinned ? "this view is the default" : "pin this view as the default"}">📌</span>`;
      chipsEl.innerHTML = html;
      chipsEl.style.display = (crumbs.length || chips.length || onPin) ? "" : "none";
    }

    /** The one token TOK spells, parsed. @param {string} tok  @returns {Token|undefined} */
    const asToken = (tok) => parseQuery(tok, queryKeys())[0];

    /**
     * Whether TOK states an order this renderer READS: a sort key resolving to
     * a column, or `*none*'.  Those are what `chainFor' builds the order out of
     * and what the strip may colour as an ordering.  A refusal keeps the
     * ordinary chip — the strip promises an order where there is one.
     * `sortable' gates the reader's GESTURE rather than the token, so a column
     * that opts out still orders and still wears it.
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
     * and the half-typed `columns:' keeps the ordinary chip, naming none.  A
     * SIGNED token states no set in either sign: the producer answers
     * `+columns:' with a refusal, and `-columns:' names nothing here either.
     * @param {string} tok  @returns {boolean}
     */
    function showsColumns(tok) {
      const t = asToken(tok);
      if (!t || t.key !== COLUMNS_KEY || t.negated || t.added) return false;
      return t.value.split(",").some((n) => n !== "");
    }

    /**
     * Whether TOK names a saved view: a view key naming one the producer
     * declared. An unknown name keeps the ordinary chip — what a name MEANS is
     * the producer's, so this side calls none of them wrong — and so does the
     * half-typed `view:', naming none.  A SIGNED token names none either: the
     * producer answers `+view:' with a refusal, as it does `+sort:'.
     * @param {string} tok  @returns {boolean}
     */
    function namesView(tok) {
      const t = asToken(tok);
      if (!t || t.key !== VIEW_KEY || t.negated || t.added) return false;
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
     * `sortsIn' decides it — the same reading `applyChain' is handed — so the
     * strip cannot describe an order the rows are not in.  Called behind
     * `ordersRows', so Q always names one.
     * @param {string} q  @returns {string}
     */
    function sortChip(q) {
      const chain = sortsIn(q, queryKeys(), namesColumn);
      return chain && chain.length ? sortToken(chain) : `${SORT_KEY}:${NONE_META}`;
    }

    /**
     * Put TOK on the strip, unless the strip already carries the same token.
     * Every token is idempotent under the one combination rule, so a second
     * copy is chrome to read past.  A predicate is itself AS SPELLED, so a near
     * twin (`tag:game' beside `tag:games') stays a second chip.
     *
     * ONE ORDER, ONE CHIP.  Every token that STATES an order folds into the
     * chip already stating one, landing as the canonical arrow form of the
     * chain the two name together; first-wins dedup rides in `sortsIn'.  A
     * token this renderer reads NO order from stays its own chip as spelled and
     * goes back to the producer verbatim, which is how a refusal is answered.
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
     * Where TOK's opposite-signed twin stands in the strip, or -1.  Twins are
     * matched on what a token MEANS — the key it names and the value it spells,
     * as `parseQuery' resolves them — never on source text, so a pair spelled
     * two ways still meets: `-priority:"[#B]"' finds `+priority:[#B]', where
     * the quote opening `-"priority:[#B]"' makes it free text and no twin at
     * all.  Only `-' against `+' pairs; an unsigned token names its added
     * form's rows and cancels nothing.  Values are compared as written, so
     * `+state:A|B' leaves `-state:B|A' standing — the strip cancels the pair a
     * reader can SEE is one.  A chip spelling more than one token is a folded
     * order chain, which wears no sign and is no token's twin.
     * @param {Token} tok  @returns {number}
     */
    function twinAt(tok) {
      if (tok.negated === tok.added) return -1;      // unsigned: no twin to find
      return chips.findIndex((c) => {
        const ts = parseQuery(c, queryKeys());
        if (ts.length !== 1) return false;
        const t = ts[0];
        return t.negated === tok.added && t.added === tok.negated
            && t.key === tok.key && t.value === tok.value;
      });
    }

    /**
     * Commit TEXT, the source token TOK was written as, to the strip: the
     * ANNIHILATION rule first — a token whose opposite-signed twin already
     * stands removes both, the pair being ¬v ∨ v and so every row
     * (docs/query.md) — and otherwise the ordinary push.
     *
     * The rule is the STRIP's affordance over the token the reader just
     * committed, never the grammar's: it runs on this interactive path alone.
     * `seedQuery' pushes around it, since a producer's query arrives whole and
     * a scan of it would eat a standing pair the reader never touched.
     * @param {string} text  @param {Token} tok
     */
    function commitChip(text, tok) {
      const at = twinAt(tok);
      if (at === -1) pushChip(text); else chips.splice(at, 1);
    }

    /**
     * REFUSE TEXT: say so once, and leave the rest to the caller, which keeps
     * the token in the box.  ONCE PER SPELLING PER SESSION — the box commits on
     * every settling debounce and a refused token stays standing through all of
     * them, so an echo per pass would be an echo nobody reads.
     * @param {string} text
     */
    function refuse(text) {
      if (spoken.has(text)) return;
      spoken.add(text);
      if (onRefused) onRefused(text);
    }

    /**
     * Move the box's finished tokens into chips. A token with nothing after it
     * is still being typed and stays put, so a word is never chipped out from
     * under the caret; ALL overrides that, which is what Enter means.
     *
     * A NARROWED SESSION CHIPS THE NARROWING HALF ALONE: a shaping token is
     * REFUSED where it would have been chipped — never on the strip, never in
     * the delivered query (`typedQuery'), left in the box where the reader sees
     * what was refused, and spoken through `refuse'.  The tokens beside it land
     * as they always do.
     * @param {boolean} [all]  @returns {boolean} whether anything moved
     */
    function chipUp(all) {
      const v = input.value;
      const toks = parseQuery(v, queryKeys());
      if (!toks.length) return false;
      const last = toks[toks.length - 1];
      const keep = !all && last.end === v.length ? last : null;
      /** What the box is left holding: the refusals, then the half-typed tail. */
      const left = [];
      let moved = false;
      // one at a time and in order: a token annihilates against the strip as it
      // stands when IT commits, so `-x +x' typed together cancels to nothing.
      for (const t of toks) {
        if (t === keep) continue;
        const text = v.slice(t.start, t.end);
        if (narrowing && shapesView(t.key)) { left.push(text); refuse(text); continue; }
        commitChip(text, t);
        moved = true;
      }
      if (!moved) return false;         // nothing finished: the box stands as typed
      if (keep) left.push(v.slice(keep.start));
      input.value = left.join(" ");
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

    function deliver(onFrame) {
      const q = effectiveQuery();
      // skip when nothing changed — a producer can't dedup Escape-dropped text, an empty commit, or a debounce settling on the same value.
      if (q === lastQuery) return;
      lastQuery = q;
      page = 0;                          // a different question, read from the top
      continuous = false;
      // the sort token takes effect here, before the producer is asked, so rows re-order under the reader's hand.
      const chain = chainFor(q);
      if (!sameChain(chain, state.sortKeys)) applyChain(chain);
      if (o.onFilter) o.onFilter(q);
      else if (onFrame) frame(applyFilter);
      else applyFilter();
    }

    /**
     * Take off the last unit of the query: what is half-typed in the box, else
     * the last chip — WHOLE, the sort chip included, an order being one
     * decision taken off the way it went on.  False when there was nothing left,
     * so a consumer can walk the query down and know when it has hit the end.
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
     * Arm the delivery a keystroke implies, in the modes where one does.  A
     * SUMMONED BOX FILTERS ON COMMIT ALONE, over the veil or on the strip
     * alike: the reader called the box up and is looking at it, so narrowing
     * as the query is typed animates a table they are not watching, and every
     * half-written token is a query of its own.  The suggestion list stays live
     * regardless, and the picker narrows as it is typed — that is what a picker
     * is for, and `summoned' is spelled to leave it out.
     */
    function armFilter() {
      if (summoned) return;
      if (debounce) clearTimeout(debounce);
      debounce = setTimeout(() => {
        debounce = 0;
        chipUp(false);
        deliver(true);
      }, DEBOUNCE);
    }
    input.addEventListener("input", () => { armFilter(); openAc(); });

    // suggestion tiers (renderer-local autocomplete): docs/web-renderer.org

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
     * COL's value domain, and how many rows stand behind each value.  The order
     * is the column's own where it has one — `values', else the badge palette —
     * that order being semantic; distinct cell values sort instead.  Counts come
     * from one lazy pass per column, thrown away with the text cache.
     * @returns {{list: string[], counts: Map<string, number>}}
     */
    function domainOf(col) {
      let d = domains.get(col.key);
      if (!d) {
        const i = columns().indexOf(col);
        if (i === multiColumn()) {
          const v = tagVocab();
          const counts = new Map();
          for (const tag of v.list) counts.set(tag, (v.ids.get(tag) || new Set()).size);
          d = { list: declaredMetas(col).concat(v.list), counts };
          domains.set(col.key, d);
          return d;
        }
        const fixed = domainValues(col);
        const counts = new Map();
        const found = [];
        for (const r of state.rows) {
          const lower = rowText(r).cells[i];
          if (!lower) continue;
          const n = counts.get(lower);
          if (n !== undefined) { counts.set(lower, n + 1); continue; }
          counts.set(lower, 1);
          if (!fixed && found.length < DOMAIN_MAX) found.push(displayText((r.cells || {})[col.key]));
        }
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
        // THE NARROWED DOOR COMPLETES NOTHING IT WILL REFUSE: a shaping key
        // hand-typed into this box asks for no stage, so the sort, columns and
        // view lists never open behind it.  The refusal `chipUp' speaks at
        // commit stays the one answer the typist is given.
        if (narrowing && shapesView(t.key)) return null;
        if (t.key === SORT_KEY) {
          const v = t.value.toLowerCase(), arrow = v.lastIndexOf(SORT_ARROW);
          return { stage: "sort", tok: t, col: null,
                   prefix: arrow === -1 ? v : v.slice(arrow + SORT_ARROW.length) };
        }
        if (t.key === COLUMNS_KEY) {
          const v = t.value.toLowerCase(), comma = v.lastIndexOf(",");
          return { stage: "columns", tok: t, col: null,
                   prefix: comma === -1 ? v : v.slice(comma + 1) };
        }
        if (t.key === VIEW_KEY)
          return { stage: "view", tok: t, col: null, prefix: t.value.toLowerCase() };
        const col = colByKey(t.key);
        if (!col) return null;
        // A DATE COLUMN'S VALUE IS A STAGE OF ITS OWN: what stands there is a
        // grammar as well as a domain, and an operator head OPENS the value
        // where a value finishes it (`suggestFor', `acceptAc').
        const onDate = dateColumn(columns().indexOf(col));
        return { stage: onDate ? "date" : "value", tok: t, col,
                 prefix: t.value.slice(t.value.lastIndexOf(ALT) + 1) };
      }
      if (!t.value || splitAt(t.value) !== -1) return null;
      return { stage: "key", tok: t, col: null, prefix: t.value };
    }

    /**
     * What KEY's axis already carries, each folded to what it MEANS (`meant'):
     * every alternative of every OTHER token naming KEY, whatever its sign,
     * chips and box alike.  TOK IS THE CARET'S OWN AND COUNTS FOR NOTHING — the
     * partial value being typed must not suppress the offers it is opening.
     * @param {string} key  @param {Token} tok  @returns {Set<string>}
     */
    function axisCarries(key, tok) {
      const keys = queryKeys();
      const out = new Set();
      /** @param {string} q  @param {number} skip */
      const take = (q, skip) => {
        for (const t of parseQuery(q, keys)) {
          if (t.key !== key || t.start === skip) continue;
          for (const alt of alternatives(t.value)) out.add(meant(alt.toLowerCase()));
        }
      };
      take(chips.join(" "), -1);
      take(input.value, tok.start);
      return out;
    }

    /**
     * The suggestions for STAGE: the text each inserts, the rows behind it, and
     * whether it finishes a token.  A column completion does not — it lands as
     * `key:' and carries no count, narrowing nothing on its own; nor does an
     * operator head on a date value, which lands as `key:>='.  `full' IS THE
     * WHOLE OF THAT ANSWER and no stage adds finality behind it (`acceptAc').
     * Row one is what Enter takes (`openAc'): WHAT THE WORD SPELLS IN FULL
     * LEADS WHAT IT MERELY OPENS, at either stage.
     * @returns {{text: string, count: number, full: boolean, dim: boolean,
     *             show?: string, aside?: string}[]}
     */
    function suggestFor(st) {
      const p = st.prefix.toLowerCase();
      const out = [];
      if (st.stage === "sort") {
        const at = p.indexOf(":");
        const wantCol = at === -1 ? p : p.slice(0, at);
        const wantDir = at === -1 ? null : p.slice(at + 1);
        const chained = sortSegments(st.tok).slice(0, -1)
          .map((s) => s.split(":")[0].toLowerCase());
        const offer = (text, dim) =>
          out.push({ text, count: -1, full: true, dim: !!dim });
        if (!p && !chained.length && state.sortKeys.length)
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
        if (wantDir === null && !chained.length && opensWith(NONE_META, wantCol))
          offer(NONE_META, true);
        return out.slice(0, AC_MAX);
      }
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
        hits.sort((a, b) => (b.whole ? 1 : 0) - (a.whole ? 1 : 0)
                         || b.count - a.count
                         || (a.text < b.text ? -1 : 1));
        const exact = hits.length > 0 && hits[0].whole;
        // A SAVED VIEW SHAPES, so the narrowed door offers none: `offeredKeys'
        // drops the three keys, and this drops the names that spell one.
        if (!narrowing)
          for (const v of savedViews()) {
            if (out.length >= AC_MAX) break;
            const name = String(v.name || "");
            if (!name.toLowerCase().startsWith(p)) continue;
            out.push({ text: VIEW_KEY + ":" + name, count: -1, full: true, dim: false,
                       aside: v.query ? String(v.query) : undefined });
          }
        if (exact) {
          const top = hits.shift();
          out.push({ text: top.text, count: top.count, full: true, dim: top.dim });
        }
        const keys = offeredKeys();
        const opens = keys.filter((k) => k.toLowerCase().startsWith(p));
        for (const k of opens.filter((k) => k.toLowerCase() === p)
                             .concat(opens.filter((k) => k.toLowerCase() !== p))) {
          out.push({ text: k + ":", count: -1, full: false, dim: false });
          if (out.length === AC_MAX) break;
        }
        for (const hit of hits) {
          if (out.length === AC_MAX) break;
          out.push({ text: hit.text, count: hit.count, full: true, dim: hit.dim });
        }
        if (p.length >= TITLE_MIN) {
          const opensT = [], holds = [];
          for (const t of titleIndex().titles) {
            if (t.lower.indexOf(p) === -1) continue;
            if (t.lower === p) continue;          // spelled already; the literal has it
            if (t.lower.indexOf('"') !== -1) continue;
            (t.lower.startsWith(p) ? opensT : holds).push(t);
          }
          for (const t of opensT.concat(holds).slice(0, TITLE_MAX)) {
            if (out.length === AC_MAX) break;
            const show = displayText(t.cell);
            out.push({ text: `"${show}"`, show, aside: "title",
                       count: -1, full: true, dim: false });
          }
        }
        const spelled = exact || keys.some((k) => k.toLowerCase() === p);
        out.splice(spelled ? 1 : 0, 0, literalOffer(st.prefix));
        if (out.length > AC_MAX) out.pop();
        return out;
      }
      const dom = domainOf(st.col);
      // `*today*' AND THE OPERATOR HEADS RIDE THE FOOT of a date column's
      // domain, the way `*empty*' rides every column's, and what is offered
      // behind a typed head is the LITERAL: `scheduled:>=2026-0' completes
      // dates.  A TYPED HEAD DROPS `*empty*', the empty cell sitting outside
      // every comparison.
      const onDate = dateColumn(columns().indexOf(st.col));
      const dv = onDate ? dateValue(p) : null;
      // THE HEAD is what stands before a literal still being typed: the
      // operator, or the range's low end and the separator behind it.
      const head = dv === null ? "" : dv.op === RANGE ? dv.lo + RANGE : dv.op;
      const p2 = p.slice(head.length);
      const listed = onDate ? dom.list.concat([TODAY_META]) : dom.list;
      const domain = head || listed.indexOf(EMPTY_META) !== -1
        ? listed : listed.concat([EMPTY_META]);
      // AN ADDED TOKEN WIDENS, AND A CARRIED VALUE WIDENS BY NOTHING: `A ∨ A'
      // is `A', so behind a `+' the values its axis already stands on are dead
      // offers and drop out.  The plain and negated stages take the whole
      // domain, each of them narrowing.
      const carried = st.tok.added ? axisCarries(st.tok.key, st.tok) : null;
      /** @type {{text: string, count: number, full: boolean, dim: boolean}|null} */
      let whole = null;
      for (const v of domain) {
        if (whole && out.length >= AC_MAX) break;
        const lower = String(v).toLowerCase(), text = head + String(v);
        if (carried && carried.has(meant(text.toLowerCase()))) continue;
        if (!opensWith(lower, p2)) continue;
        // THE DOMAIN COUNTS ROWS SPELLING A CELL, where `>=D' serves rows that
        // spell something else — so an offer behind a head prints no count
        // rather than a wrong one.
        const meta = META.test(String(v));
        const item = { text, count: meta || head ? -1 : dom.counts.get(lower) || 0,
                       full: true, dim: meta };
        if (spells(lower, p2)) { whole = item; continue; }
        if (out.length < AC_MAX) out.push(item);
      }
      // WHAT SPELLS IN FULL LEADS WHAT MERELY OPENS, so a head rides under the
      // values and carries no count.  `full: false' is what says it opens one:
      // accepting leaves the token unfinished for the literal (`acceptAc').
      if (onDate)
        for (const op of CMPS.filter((c) => c !== p && c.startsWith(p))) {
          if (out.length >= AC_MAX) break;
          out.push({ text: op, count: -1, full: false, dim: true });
        }
      if (whole) {
        out.unshift(whole);
        if (out.length > AC_MAX) out.pop();
      }
      return out;
    }

    /**
     * The literal offer: what was typed, as a free-text token.  DRAWN quoted,
     * the grammar's notation for "this is text", and COMMITTED bare, which is
     * what a reader who knew the grammar would have written.  Quotes are owed
     * only where a separator would break the token up, and of those only
     * whitespace can reach here.
     * @param {string} text
     */
    function literalOffer(text) {
      const value = /[\s:&"]/.test(text) ? `"${text}"` : text;
      const tok = `${SUBSTRING_KEY}:${value}`;
      return { text: tok, show: tok, aside: "text search",
               count: -1, full: true, dim: false };
    }

    /**
     * The distinct titles, in row order, lowercased beside the RAW cell — only
     * the few titles offered pay `displayText''s link parse.  Built whole on
     * first use rather than patched, an upsert being able to move any of them,
     * and thrown away with the text cache it was read from.
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
        html += `<div class="tv-ac-item${it.dim ? " tv-ac-dim" : ""}`
              + `${i === acAt ? " tv-ac-on" : ""}" data-i="${i}">`
              + `<span class="tv-ac-label">${label}</span>`
              + (it.aside ? `<span class="tv-ac-aside">${esc(it.aside)}</span>`
                          : it.count < 0 ? "" : `<span class="tv-ac-n">${it.count}</span>`)
              + `</div>`;
      }
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
     * box alone.  A key lands as `key:' with the caret against the colon; a
     * value lands with a trailing space.  Focus stays in the box either way.
     *
     * A value keeps everything through the token's LAST bar and a SORT segment
     * through the last ARROW, so completing inside either keeps ONE token.
     * Both are looked for in the RAW text: the token's value has had its quotes
     * taken out and no longer lines up with the box.
     *
     * THE SIGN SURVIVES THE COMPLETION at every stage: a key head spells it
     * (`signMark'), and every other stage slices from `start', which sits at the
     * sign — so `+sta' completes to `+state:' and `+state:DO' to `+state:DONE'.
     */
    function acceptAc(item) {
      if (!ac) return;
      const stage = ac.stage;
      const v = input.value, t = ac.tok;
      const bar = v.lastIndexOf(ALT, t.end - 1);
      const arrow = ac.stage === "sort" ? v.lastIndexOf(SORT_ARROW, t.end - 1) : -1;
      const comma = ac.stage === "columns" ? v.lastIndexOf(",", t.end - 1) : -1;
      const head = ac.stage === "key" ? signMark(t)
        : v.slice(t.start, Math.max(t.sep + 1, bar + 1, comma + 1,
                                    arrow === -1 ? 0 : arrow + SORT_ARROW.length));
      const ins = head + item.text + (item.full ? " " : "");
      input.value = v.slice(0, t.start) + ins + v.slice(t.end);
      const caret = t.start + ins.length;
      if (input.setSelectionRange) input.setSelectionRange(caret, caret);
      armFilter();
      if (stage === "view") {
        flushFilter(true);
        if (!input.value.trim()) handOver();   // a refusal stays, and keeps the box
        return;
      }
      openAc();          // a key opens its values; a finished value closes the list
    }

    acEl.addEventListener("mousedown", (e) => e.preventDefault());   // the box keeps focus
    acEl.addEventListener("click", (e) => {
      const t = hit(e);
      const item = t && /** @type {HTMLElement|null} */ (t.closest(".tv-ac-item"));
      if (item && ac) acceptAc(ac.items[Number(item.dataset.i)]);
    });
    // A SUMMONED BOX THAT LOST THE KEYS IS NOT SUMMONED.  Tab walks out of it
    // natively and a row click takes the focus, either of which used to leave a
    // docked editor DRAWN while the keys were elsewhere — a consumer reading
    // `filtering()' and a reader reading the screen would then disagree.
    input.addEventListener("blur", () => {
      closeAc();
      endNarrow();           // the session is the box's; one owner for the flag
      if (dock === "strip") root.classList.remove("tv-typing");
    });

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

    input.addEventListener("keydown", (e) => {
      if (ac) {
        // C-n/C-p move the list too; Chrome-family takes C-n for a new window before the page sees it, so arrows are the fallback there (Firefox/webview deliver both).
        const down = e.key === "ArrowDown" || (e.ctrlKey && e.key === "n");
        const up = e.key === "ArrowUp" || (e.ctrlKey && e.key === "p");
        const accepts = e.key === "Tab" || e.key === "Enter";
        if (down || up || accepts || e.key === "Escape") {
          e.preventDefault();
          e.stopPropagation();
          if (down) { moveAc(1); return; }
          if (up) { moveAc(-1); return; }
          // In `inline' the suggestion list is no rung of its own.
          if (e.key === "Escape") { closeAc(); if (inline) abandonFilter(); return; }
          const taken = ac.items[acAt];
          const finished = taken.full;
          acceptAc(taken);
          if (e.key === "Tab" || !finished) return;
          closeAc();
        }
      }
      if (e.key === "Backspace" && !input.value) {
        e.preventDefault();
        e.stopPropagation();
        if (e.repeat) return;
        // BACKSPACE OVER A SUMMONED BOX IS DEAD, veiled or docked alike: the
        // chips are on the page BEHIND the box, not in the box being edited,
        // so there is no rung here to take one — the strip's own × is how one
        // comes off, and `stripLastToken' is the consumer's key to it.
        if (summoned) return;
        // The picker's editor is summoned too, and there an EMPTY one is itself
        // the thing to take away: it was the last thing the reader put there.
        // This ENDS the ladder — the box is blurred, so the chips behind it are
        // the consumer's own to walk.  A resident box has no such rung and
        // never gets here.  The SAME exit Escape takes, said once: the box is
        // empty by the guard above, so `abandonFilter' has nothing left to drop.
        if (inline) { abandonFilter(); return; }
        if (chips.length) dropChip(chips.length - 1);
        else handOver();
        return;
      }
      if (e.key !== "Enter" && e.key !== "Escape") return;
      e.preventDefault();               // and, for Escape, the native search-box clear
      e.stopPropagation();
      if (e.key === "Escape") {
        // ONE STEP OUT OF THE PICKER, where an emptied box is an editor the
        // reader was already done with.
        if (inline) { abandonFilter(); return; }
        // TWO STEPS EVERYWHERE ELSE: the typed text first, the box second —
        // which for a summoned box IS the box going, over the veil or off the
        // strip, and for a resident one is the blur.
        if (!clearTyped()) closeFilter();
        return;
      }
      if (input.value.trim()) {
        flushFilter(true);              // `chipUp' reads the box, then empties it
        // A REFUSAL IS THE READER'S TO SEE: the narrowed door leaves the shaping
        // token standing, and a box with something still in it is not finished.
        if (input.value.trim()) return;
      } else {
        input.value = "";               // stray whitespace is nothing to commit
        if (debounce) { clearTimeout(debounce); debounce = 0; deliver(); }
      }
      handOver();
    });

    veil.addEventListener("mousedown", (e) => {
      if (hit(e) === veil) { e.preventDefault(); closeFilter(); }
    });

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
      // a crumb carries no data-i; without this guard the index reads NaN and splice takes it as 0 (the first live chip).
      if (!chip || chip.dataset.i === undefined) return;
      dropChip(Number(chip.dataset.i));
    });


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

    // streaming: setQuery seeds chips and delivers nothing — docs/web-renderer.org
    function seedQuery(q) {
      chips.length = 0;
      if (typeof q === "string" && q.trim())
        for (const t of parseQuery(q, queryKeys())) pushChip(q.slice(t.start, t.end));
      renderChips();     // `pushChip' fills the array and draws nothing
      lastQuery = effectiveQuery();
      if (!o.onFilter) state.filter = lastQuery;
      state.sortKeys = chainFor(lastQuery);
    }
    if (typeof o.initialQuery === "string" && o.initialQuery.trim())
      seedQuery(o.initialQuery);
    else renderChips();

    titleEl.textContent = state.view.title || "Table";
    renderHead();
    renderRows(true);
    queueIndex();

    function onTheme() {
      const now = darkNow();
      if (now === dark) return;
      dark = now;
      renderRows(true);
    }
    /** What `destroy' has to undo: the two theme watchers outlive the DOM. */
    const themeQuery = typeof matchMedia === "function"
                     ? matchMedia("(prefers-color-scheme: dark)") : null;
    if (themeQuery && themeQuery.addEventListener)
      themeQuery.addEventListener("change", onTheme);
    const themeWatch = typeof MutationObserver === "function" && document.documentElement
                     ? new MutationObserver(onTheme) : null;
    if (themeWatch)
      themeWatch.observe(document.documentElement,
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
       * Replace every row.  Marks are deliberately NOT pruned against the new
       * set: a producer filtering server-side answers a narrowed query through
       * here, so an id that did not come back is HIDDEN rather than deleted and
       * must still be marked when the filter comes off.  A delta's `reset' is
       * the same op; `deleteRow' and a delta's `delete' do drop it.
       * @param {Row[]} rows
       */
      setRows(rows) {
        state.rows = (rows || []).slice();
        clearTexts();
        dropSorted();
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
          // delta op indices count in the window (display order) per SCHEMA.md; with no local sort/filter/page that's the store's own order.
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
       * The rows on show: one page of the filtered, sorted set, or all of it
       * with no page size.  In CONTINUOUS the window is over the whole set,
       * but this still answers the CURSOR's page, so a consumer's buffer-end
       * keys and the pager agree whichever way the rows were drawn.
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
      filtering,
      /**
       * Let the mount go.  Emptying the container drops the DOM; these two
       * theme watchers are registered OUTSIDE it and would hold this whole
       * scope — rows, caches, the detached tree — alive without this.  A
       * consumer that mounts once a session may skip it; one that mounts per
       * gesture may not.
       */
      destroy() {
        if (themeQuery && themeQuery.removeEventListener)
          themeQuery.removeEventListener("change", onTheme);
        if (themeWatch) themeWatch.disconnect();
      },
      /**
       * Sort on COLUMN, ascending unless ASCENDING is false, replacing whatever
       * sort is in force.  A header click TOGGLES; this STATES an order.  It
       * ignores `sortable', which gates what a READER may reach.
       * @param {string} column @param {boolean} [ascending]
       * @returns {boolean} false when no column carries that key
       */
      sortBy(column, ascending) { return sortTo(column, ascending !== false); },
      /**
       * `^': promote COLUMN to the head of the chain ascending, flipping it
       * where it already leads.  The new chain is written into the query and
       * delivered, so a consumer narrowing server-side is asked for the order
       * it has just been told about.  Gated by `sortable' — a reader's gesture,
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
       * Replace the whole chain — SCHEMA's `sort' shape or `getSort''s.  An
       * empty one leaves the rows in the order they arrived, which is the CLEAR
       * a consumer binds.  A producer's call, like `sortBy': it writes no
       * query, so a query naming sort keys still outranks it.
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
