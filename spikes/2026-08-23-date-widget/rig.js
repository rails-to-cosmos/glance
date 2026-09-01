// The stage every variant is judged on: ONE sheet, ONE calendar, ONE grammar,
// ONE set of commit/cancel laws.  A variant brings a `look' and its own CSS and
// nothing else, so what differs between two tabs is WHERE THE WIDGET STANDS and
// never what it knows.
//
// A CLASSIC SCRIPT ON PURPOSE: a module over `file://' is an opaque origin and
// will not load, and every page here must open by double-clicking it.
"use strict";

var RIG = (function () {

  // ===================================================================== bugs
  // THE CHECK HAS TO BITE.  Seven deliberate faults, off unless `?bug=' asks for
  // one: a day-walk that skips, a weekday that is remembered rather than
  // computed, a resolution one step behind the field it speaks for, a ghost
  // written INTO the field where the caret can walk into it, an edit that OPENS
  // with the caret collapsed instead of the value selected, an edit whose
  // selection is painted in the ground already behind it, and an ESC that takes
  // the menu instead of the edit.  `check.mjs' runs each variant clean AND
  // under each fault; a fault that still passes is a rung that was never testing
  // anything.
  // FAULTS COMPOSE: `?bug=blend,opencaret' turns on both.  The paint pin needs
  // it — to prove a fault is caught it must hold that fault CONSTANT and vary
  // only the caret, or it is comparing two different pages.
  var BUGS = (function () {
    var m = /[?&]bug=([a-z,]+)/.exec(location.search || "");
    return m ? m[1].split(",").filter(function (x) { return x; }) : [];
  })();
  function bug(name) { return BUGS.indexOf(name) !== -1; }

  // ================================================================= calendar
  // Civil dates as {y, m, d}, and the day number they sit on.  UTC throughout:
  // a local-midnight `Date' shifts a day across a DST boundary, and a widget
  // whose arithmetic moved with the reader's timezone would be a different
  // widget in Berlin.
  var DAY = 86400000;
  function dnum(c) { return Math.round(Date.UTC(c.y, c.m - 1, c.d) / DAY); }
  function civil(n) {
    var t = new Date(n * DAY);
    return { y: t.getUTCFullYear(), m: t.getUTCMonth() + 1, d: t.getUTCDate() };
  }
  function leap(y) { return (y % 4 === 0 && y % 100 !== 0) || y % 400 === 0; }
  function daysIn(y, m) {
    return [31, leap(y) ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][m - 1];
  }
  function valid(c) {
    return !!c && c.m >= 1 && c.m <= 12 && c.d >= 1 && c.d <= daysIn(c.y, c.m);
  }
  function addDays(c, n) { return civil(dnum(c) + n); }
  // A MONTH STEP KEEPS THE DAY OF THE MONTH and clamps at the month's end, so
  // `31 jan' + 1m is the 28th and never the 3rd of March.  Said out loud
  // because it is a CHOICE: adding thirty days would make `+1m' mean a
  // different span in every month.
  function addMonths(c, n) {
    var k = (c.m - 1) + n;
    var y = c.y + Math.floor(k / 12);
    var m = ((k % 12) + 12) % 12 + 1;
    return { y: y, m: m, d: Math.min(c.d, daysIn(y, m)) };
  }
  function addYears(c, n) { return addMonths(c, 12 * n); }
  var DOW = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
  // THE WEEKDAY IS COMPUTED, never carried: `TsMoment' holds no weekday field
  // and recomputes on render (AGENTS.hs:213), which is why a locale's word
  // costs nothing and why a stamp cannot disagree with its own date.
  function dow(c) {
    var i = new Date(dnum(c) * DAY).getUTCDay();
    // The fault: the word is taken from the day BEFORE — every stamp exactly
    // one weekday wrong, which is the shape a hand-kept weekday field takes.
    if (bug("weekday")) i = (i + 6) % 7;
    return DOW[i];
  }
  function pad2(n) { return (n < 10 ? "0" : "") + n; }
  function iso(c) { return c.y + "-" + pad2(c.m) + "-" + pad2(c.d); }
  // MONDAY FIRST, org's own week: `org-agenda-start-on-weekday' is 1, and the
  // agenda is the surface this widget feeds.
  var WEEK = ["Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"];
  function mondayOf(c) {
    var i = new Date(dnum(c) * DAY).getUTCDay();     // 0 = Sunday
    return addDays(c, -((i + 6) % 7));
  }
  var MONTHS = ["January", "February", "March", "April", "May", "June", "July",
                "August", "September", "October", "November", "December"];

  // ================================================================== grammar
  // The shipped grammar (docs/commands.md "Dates", docs/query.md's date
  // sections) plus the English day-and-month forms the proposal owes
  // (docs/proposals/proposed/2026-08-22-a-date-is-read-where-a-date-is-owed.md).
  // ONE reader for every tab; A alone is told to leave the English half out,
  // because A is TODAY and today does not read it.

  // THE DATE ORG READS, and the stamp wall the pane already spells
  // (`frontend/glue/20-sheet.js:652'): a decimal run per field, month and day
  // range-checked, ONE stamp or two joined by org's own `--' and WEARING THE
  // SAME BRACKET.
  var D_RE = "\\d+-(?:0?[1-9]|1[0-2])-(?:0?[1-9]|[12]\\d|3[01])(?!\\d)";
  var ACTIVE_RE = "<" + D_RE + "[^<>\\n]*>";
  var INACTIVE_RE = "\\[" + D_RE + "[^\\[\\]\\n]*\\]";
  var STAMP = new RegExp("^(?:" + ACTIVE_RE + "(?:--" + ACTIVE_RE + ")?|"
                         + INACTIVE_RE + "(?:--" + INACTIVE_RE + ")?)$");

  var MONTH_WORDS = {
    jan: 1, january: 1, feb: 2, february: 2, mar: 3, march: 3,
    apr: 4, april: 4, may: 5, jun: 6, june: 6, jul: 7, july: 7,
    aug: 8, august: 8, sep: 9, september: 9, oct: 10, october: 10,
    nov: 11, november: 11, dec: 12, december: 12,
  };
  // WHAT A REFUSAL NAMES, the way the shipped 400 names the accepted spellings —
  // and, beside it, the SHORT form.  A refusal that rides an input line as
  // trailing ghost has one line to say it in, so every refusal carries both: the
  // sentence for an echo or a log, the word for the ghost.  `hard' marks the
  // ones no further typing can rescue — a day that is not on the calendar, a
  // range that ends before it starts — which is what keeps the ghost from
  // calling a half-typed month a mistake.
  var NOWAY = "no date here — try 2026-08-18, today, +3d, or 18 aug";
  var NOWAY_SHIPPED = "no date here — try 2026-08-18, today, tomorrow or +3d";
  var NOWAY_SHORT = "no date";

  function isoOf(s) {
    var m = /^(\d{4})-(\d{1,2})-(\d{1,2})$/.exec(s);
    if (!m) return null;
    var c = { y: +m[1], m: +m[2], d: +m[3] };
    return valid(c) ? c : false;              // `false' is "spelled, not real"
  }

  /** Org's own spelling, KEPT VERBATIM once it reparses — the one form whose
   * weekday is NOT recomputed (`AGENTS.hs:3426' `verbatimDate Bracketed';
   * `test/TestQuery.hs:1791' pins that `<2026-08-05 Mon>' goes through
   * unchanged though that day is a Wed).  A MIXED pair names a timestamp that
   * does not exist (`AGENTS.hs:218' — one bracket kind, both halves). */
  function verbatim(s) {
    if (!/^[<[]/.test(s)) return null;
    if (STAMP.test(s)) {
      var a = /^[<[](\d{4}-\d{1,2}-\d{1,2})/.exec(s);
      return { ok: true, verbatim: s, start: a ? (isoOf(a[1]) || null) : null };
    }
    if (/^<[^>]*>--\[/.test(s) || /^\[[^\]]*\]--</.test(s))
      return { ok: false, hard: true, short: "mixed brackets",
               why: "both halves of a range share one bracket" };
    return { ok: false, short: "no stamp org reads back",
             why: "that bracket is no stamp org would read back" };
  }

  /** A shift's BASE, or null when the text is not one at all, or false when it
   * is SPELLED as one and is no real day.  An empty base is today-relative,
   * which is the reading the planning grammar already gives a bare `+3d'. */
  function baseOf(t, today) {
    if (t === "") return today;
    if (t === "today" || t === "*today*") return today;
    if (t === "tomorrow") return addDays(today, 1);
    if (/^\d{4}-\d{1,2}-\d{1,2}$/.test(t)) return isoOf(t) || false;
    return null;
  }

  /** The shipped grammar: ISO, `today'/`*today*'/`tomorrow', and a SHIFT on
   * any of them.  Returns null when the text is not this grammar's at all, so
   * the English reader gets its turn. */
  function shipped(s, today) {
    var t = s.toLowerCase();
    // A SHIFT'S SIGN IS THE ONE BEFORE THE UNIT, so a date's own hyphens are
    // never mistaken for it (docs/query.md:339): the base match is GREEDY,
    // which leaves the last sign to open the shift.
    var sh = /^(.*)([+-])(\d+)([dwmy])$/.exec(t);
    if (sh) {
      var base = baseOf(sh[1].trim(), today);
      if (base === null) return null;
      if (base === false)
        return { ok: false, hard: true, short: "no such day on the calendar",
                 why: sh[1].trim() + " is no day on the calendar" };
      var n = (sh[2] === "-" ? -1 : 1) * (+sh[3]);
      var u = sh[4];
      return { ok: true, shifted: true,
               start: u === "d" ? addDays(base, n)
                    : u === "w" ? addDays(base, 7 * n)
                    : u === "m" ? addMonths(base, n)
                    : addYears(base, n) };
    }
    // A HALF-TYPED SHIFT NARROWS NOTHING (docs/query.md:344) — and it is no
    // refusal either: it is a term still being WRITTEN, which is what keeps
    // the offers standing over it.
    var half = /^(.*?)[+-]\d*$/.exec(t);
    if (half && baseOf(half[1].trim(), today) !== null
        && baseOf(half[1].trim(), today) !== false)
      return { ok: false, unfinished: true,
               short: "half a shift — d, w, m or y",
               why: "half a shift — say the unit: d, w, m or y" };
    var b = baseOf(t, today);
    if (b === null) return null;
    if (b === false)
      return { ok: false, hard: true, short: "no such day on the calendar",
               why: t + " is no day on the calendar" };
    return { ok: true, start: b };
  }

  // `day month [year]' or `month day [year]'.  Two digits at most, no ordinal
  // suffix, a year of four digits and never two.
  function readDate(w, today) {
    if (w.length < 2 || w.length > 3) return null;
    var y = null;
    if (w.length === 3) {
      if (!/^\d{4}$/.test(w[2])) return null;
      y = +w[2];
    }
    var d = null, mo = null;
    if (/^\d{1,2}$/.test(w[0]) && MONTH_WORDS[w[1]]) { d = +w[0]; mo = MONTH_WORDS[w[1]]; }
    else if (MONTH_WORDS[w[0]] && /^\d{1,2}$/.test(w[1])) { mo = MONTH_WORDS[w[0]]; d = +w[1]; }
    else return null;
    var c = { y: y === null ? today.y : y, m: mo, d: d };
    // `Time.fromGregorianValid' is the wall: `31 feb' never reaches the disk.
    if (!valid(c)) return { bad: w.join(" ") + " is no day on the calendar" };
    return { c: c };
  }

  // The interval's left end: a day alone, a day and a month, or all three.
  // EACH ELIDED FIELD TAKES THE RIGHT END'S VALUE — the English idiom says one
  // month once.
  function readLeft(w, right) {
    if (!w.length || w.length > 3) return null;
    if (!/^\d{1,2}$/.test(w[0])) return null;
    var c = { y: right.y, m: right.m, d: +w[0] };
    if (w.length >= 2) {
      if (!MONTH_WORDS[w[1]]) return null;
      c.m = MONTH_WORDS[w[1]];
    }
    if (w.length === 3) {
      if (!/^\d{4}$/.test(w[2])) return null;
      c.y = +w[2];
    }
    if (!valid(c)) return { bad: w.join(" ") + " is no day on the calendar" };
    return { c: c };
  }

  /** The English day-and-month forms, single and interval. */
  function english(s, today) {
    var w = s.toLowerCase().split(/[ \t]+/).filter(function (x) { return x; });
    if (!w.length) return null;
    if (w[0] === "from") w = w.slice(1);
    var i = w.indexOf("to");
    if (i > 0 && i < w.length - 1) {
      var right = readDate(w.slice(i + 1), today);
      if (!right) return null;
      if (right.bad) return { ok: false, hard: true, why: right.bad,
                              short: "no such day on the calendar" };
      var left = readLeft(w.slice(0, i), right.c);
      if (!left) return null;
      if (left.bad) return { ok: false, hard: true, why: left.bad,
                             short: "no such day on the calendar" };
      var a = dnum(left.c), b = dnum(right.c);
      // THE DEGENERATE INTERVAL COLLAPSES: with no times `<D>--<D>' and `<D>'
      // denote the same interval, so the parser keeps one answer per meaning.
      if (a === b) return { ok: true, start: left.c };
      // AN INVERTED RANGE IS REFUSED — that is what keeps the year rule
      // statable without a calendar.
      if (a > b) return { ok: false, hard: true,
                          short: "ends before it starts",
                          why: "the range ends before it starts — spell the years" };
      return { ok: true, start: left.c, end: right.c };
    }
    var one = readDate(w, today);
    if (!one) return null;
    if (one.bad) return { ok: false, hard: true, why: one.bad,
                          short: "no such day on the calendar" };
    return { ok: true, start: one.c };
  }

  /** TEXT read as a date, against TODAY.  `withEnglish' is off in A, which
   * reads today's grammar and nothing more. */
  function parse(text, today, withEnglish) {
    var s = String(text == null ? "" : text).trim();
    if (!s) return { ok: true, clear: true };
    var v = verbatim(s);
    if (v) return v;
    if (withEnglish) {
      var e = english(s, today);
      if (e) return e;
    }
    var g = shipped(s, today);
    if (g) return g;
    return { ok: false, short: NOWAY_SHORT,
             why: withEnglish ? NOWAY : NOWAY_SHIPPED };
  }

  var MONTH_LIST = Object.keys(MONTH_WORDS);
  function extends_(list, w) {
    return list.some(function (x) { return x.indexOf(w) === 0; });
  }

  /** Is TEXT a term still being WRITTEN — a proper prefix of something this
   * grammar would accept?  THE GHOST SAYS NOTHING OVER ONE: a refusal flashed
   * at every keystroke is a refusal nobody reads, and `18 a' is not a mistake,
   * it is a month halfway typed.  A `hard' refusal is never merely being
   * written — no further character rescues a day that is not on the calendar,
   * and `31 feb' is a prefix of `31 february', which is the same wrong day. */
  function writing(s, r) {
    if (r && r.hard) return false;
    if (r && r.unfinished) return true;
    var t = s.trim().toLowerCase();
    if (!t) return false;
    // An ISO being spelled — but never one already spelled to the full shape,
    // which is a date the calendar has judged.
    if (/^\d{0,4}(-\d{0,2}(-\d{0,2})?)?$/.test(t)
        && !/^\d{4}-\d{1,2}-\d{1,2}$/.test(t)) return true;
    if (extends_(VOCAB, t)) return true;
    if ("from".indexOf(t) === 0) return true;
    var w = t.replace(/^from[ \t]+/, "").split(/[ \t]+/);
    var to = w.indexOf("to");
    if (to > 0) {
      var right = w.slice(to + 1);
      if (!right.length) return true;
      if (right.length === 1
          && (/^\d{1,2}$/.test(right[0]) || extends_(MONTH_LIST, right[0])))
        return true;
      if (right.length === 2 && /^\d{1,2}$/.test(right[0])
          && extends_(MONTH_LIST, right[1])) return true;
      return false;
    }
    if (w.length === 1)
      return /^\d{1,2}$/.test(w[0]) || extends_(MONTH_LIST, w[0]);
    if (w.length === 2 && /^\d{1,2}$/.test(w[0]))
      return extends_(MONTH_LIST, w[1]) || "to".indexOf(w[1]) === 0;
    if (w.length === 2 && MONTH_WORDS[w[0]]) return /^\d{0,2}$/.test(w[1]);
    return false;
  }

  /** THE STAMP THE COMMIT WRITES.  ONE renderer, the brackets the difference,
   * the weekday COMPUTED (`AGENTS.hs:3415') — except on the verbatim branch,
   * which is org's own spelling kept as written. */
  function stampOf(r) {
    if (!r || !r.ok || r.clear) return "";
    if (r.verbatim) return r.verbatim;
    var a = "<" + iso(r.start) + " " + dow(r.start) + ">";
    return r.end ? a + "--<" + iso(r.end) + " " + dow(r.end) + ">" : a;
  }

  // ================================================================== fixture
  // The sheet the widget is summoned onto: a headline, its planning line, a
  // drawer whose `:Due:' pair holds a stamp — the proposal's date-shaped
  // property, a value being replaced that already reparses — and paragraphs
  // under it, which is what makes a DOCKED widget's push visible.
  var LOOK_DEFAULT = { klass: "", mount: "dock", grid: "month", field: "door",
                       echo: true, ghost: false, preview: true, english: true };

  var state = {
    today: null, mdoc: null, look: null, rows: [], at: 0,
    plan: [],                        // [[KEYWORD, value], …] in written order
    pair: { key: "Due", value: "" },
    open: null,
  };

  function el(id) { return document.getElementById(id); }
  function part(host, tag, cls, text) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (text !== undefined) e.textContent = text;
    host.appendChild(e);
    return e;
  }

  // ===================================================================== pane
  function row(cls, kind, build) {
    var d = document.createElement("div");
    d.className = "de " + cls;
    build(d);
    state.mdoc.appendChild(d);
    state.rows.push({ el: d, kind: kind });
    return d;
  }

  function drawHead() {
    row("d-head", "head", function (d) {
      part(d, "span", "ds", "* ");
      part(d, "span", "dc dc-state", "TODO");
      part(d, "span", "dc dc-title", "Read a date where a date is owed");
      part(d, "span", "dc dc-tags", ":spike:");
    });
  }

  function previewing(where, keyword) {
    var o = state.open;
    return !!(o && state.look.preview && o.where === where
              && (where === "pair" || o.keyword === keyword));
  }

  /** The planning line, each keyword a reserved token the way org paints
   * `SCHEDULED:' — the timestamp itself stays the line's own
   * (`frontend/elm/src/Doc.elm:1592').  NO ENTRIES, NO LINE: the absence is
   * the display, and only a standing summon ghosts the keyword in. */
  function drawPlan() {
    var entries = state.plan.map(function (p) { return p.slice(); });
    var o = state.open;
    // A SHEET BEHIND A VEIL DOES NOT MOVE: only a variant that previews at all
    // ghosts the keyword in, so A's document is the bytes it was.
    var ghost = !!(o && o.where === "plan" && state.look.preview
                   && !entries.some(function (p) { return p[0] === o.keyword; }));
    if (ghost) entries = ORDER(entries.concat([[o.keyword, ""]]));
    if (!entries.length) return;
    row("d-meta d-plan" + (ghost && entries.length === 1 ? " bare" : ""),
        "plan", function (d) {
      d.style.setProperty("--g-doc-indent", "2");
      entries.forEach(function (p, i) {
        if (i) d.appendChild(document.createTextNode(" "));
        var k = part(d, "span", "dk", "");
        k.appendChild(document.createTextNode(p[0]));
        part(k, "span", "dpunc", ":");
        d.appendChild(document.createTextNode(" "));
        var pend = previewing("plan", p[0]);
        // FULLY INLINE: where the mount is the SLOT, the widget stands in the
        // value's own place on this line.  Nothing is added to the sheet and
        // nothing is restated — the row already said the keyword.
        if (pend && state.look.mount === "slot") {
          d.classList.add("d-editing");
          d.appendChild(buildWidget(state.open));
        } else part(d, "span", "dv" + (pend ? " pending" : ""),
                    pend ? (state.open.stamp || "…") : p[1]);
      });
    });
  }

  function drawDrawer() {
    var frame = row("d-meta d-drawer", "drawer", function (d) {
      d.style.setProperty("--g-doc-indent", "2");
      var g = part(d, "span", "dg", "");
      part(g, "span", "dpunc dlead", ":");
      g.appendChild(document.createTextNode("PROPERTIES"));
      part(g, "span", "dpunc", ":");
    });
    var pairRow = document.createElement("div");
    pairRow.className = "de d-meta d-pair";
    pairRow.style.setProperty("--g-doc-indent", "2");
    var k = part(pairRow, "span", "dk", "");
    part(k, "span", "dpunc dlead", ":");
    k.appendChild(document.createTextNode(state.pair.key));
    part(k, "span", "dpunc", ":");
    pairRow.appendChild(document.createTextNode(" "));
    var pend = previewing("pair");
    if (pend && state.look.mount === "slot") {
      pairRow.classList.add("d-editing");
      pairRow.appendChild(buildWidget(state.open));
    } else part(pairRow, "span", "dv" + (pend ? " pending" : ""),
                pend ? (state.open.stamp || "…") : state.pair.value);
    frame.appendChild(pairRow);
    state.rows.push({ el: pairRow, kind: "pair" });
    var end = document.createElement("div");
    end.className = "de d-meta";
    end.style.setProperty("--g-doc-indent", "2");
    var g2 = part(end, "span", "dg", "");
    part(g2, "span", "dpunc dlead", ":");
    g2.appendChild(document.createTextNode("END"));
    part(g2, "span", "dpunc", ":");
    frame.appendChild(end);
  }

  var PARAS = [
    "Org's own answer is a calendar in another window and a live minibuffer: "
      + "the grid walks, the field types, and the two are ONE value.",
    "This spike asks where that value's widget stands in a pane that draws "
      + "text and hairlines — over the rows, or in among them.",
  ];

  function drawPane() {
    state.mdoc.textContent = "";
    state.rows = [];
    drawHead();
    drawPlan();
    drawDrawer();
    PARAS.forEach(function (t) {
      row("d-para", "para", function (d) {
        d.style.setProperty("--g-doc-indent", "2");
        d.textContent = t;
      });
    });
    if (state.at >= state.rows.length) state.at = state.rows.length - 1;
    if (state.at < 0) state.at = 0;
    state.rows[state.at].el.classList.add("dat");
  }

  // SCHEDULED BEFORE DEADLINE, org's own order on the line.
  function ORDER(ps) {
    var rank = { SCHEDULED: 0, DEADLINE: 1, CLOSED: 2 };
    var of = function (k) { return rank[k] === undefined ? 9 : rank[k]; };
    return ps.slice().sort(function (a, b) { return of(a[0]) - of(b[0]); });
  }

  // ================================================================== the eye
  /** THE PANE'S PICTURE, byte for byte: every row's classes, its text, and
   * where it stands.  ESC's whole claim is that this string comes back
   * unchanged — and a widget still standing is a row this string does not
   * have. */
  function picture() {
    var bits = [];
    var box = state.mdoc.getBoundingClientRect();
    var all = state.mdoc.querySelectorAll(".de,.dw");
    for (var i = 0; i < all.length; i += 1) {
      var e = all[i], r = e.getBoundingClientRect();
      bits.push([e.className, JSON.stringify(e.textContent),
                 Math.round(r.top - box.top), Math.round(r.height)].join("/"));
    }
    return bits.join(";");
  }

  function planText() {
    return state.plan.map(function (p) { return p[0] + ": " + p[1]; }).join(" ");
  }

  // ================================================================== offers
  // THE TYPED VALUE IS ALWAYS AN OFFER and the vocabulary is OPEN, so the line
  // the reader typed leads its own list (docs/design-rhymes.md).  What is new
  // here is the HINT COLUMN: a date offer RESOLVES, so the list is its own
  // preview — the one thing a date vocabulary can do that a property
  // vocabulary cannot.
  var VOCAB = ["today", "tomorrow", "+1d", "+1w", "+2w", "+1m", "+3m", "+1y"];

  /** OFFERS STAND AT FRESH AND UNFINISHED POSITIONS.  A finished term — a text
   * that reads as a whole date — carries none, and RET there APPLIES.  THE
   * WALK ALWAYS LANDS ON A FINISHED TERM, so the grid and the offers are never
   * both asking. */
  function finished(o) {
    var r = parse(o.text, state.today, o.english);
    return r.ok && !r.clear;
  }

  function offersFor(o) {
    // A BLIND FIELD OFFERS NOTHING, which is the whole of what makes it blind:
    // today's prompt is a text box with no vocabulary behind it.
    if (state.look.mount === "raise") return [];
    if (!o.inField || finished(o)) return [];
    var typed = String(o.text).trim();
    var want = typed.toLowerCase();
    var pool = VOCAB.slice();
    // The month words earn their place only once a DAY has been typed: a bare
    // month name is refused by the grammar anyway, so offering one would be
    // offering a refusal.
    var dayFirst = o.english && /^(\d{1,2})(?:[ \t]+(\S*))?$/.exec(want);
    if (dayFirst) {
      var day = dayFirst[1], frag = dayFirst[2] || "";
      pool = Object.keys(MONTH_WORDS)
        .filter(function (w) { return w.length > 3 || w === "may"; })
        .filter(function (w) { return w.indexOf(frag) === 0; })
        .map(function (w) { return day + " " + w; });
      if (frag === "" || "to".indexOf(frag) === 0) pool.unshift(day + " to ");
    }
    var fits = pool.filter(function (w) {
      return want === "" || w.toLowerCase().indexOf(want) === 0;
    }).slice(0, 6);
    var dress = function (w) {
      var r = parse(w, state.today, o.english);
      return { word: w,
               hint: r.ok && r.start ? iso(r.start) + " " + dow(r.start) : "…" };
    };
    // AN EMPTY FIELD OFFERS NO LITERAL, and point stands on NOTHING over one:
    // with nothing typed the list is a menu to walk, and RET there is the
    // empty value's own meaning — which on this key is CLEAR.
    var lead = typed && !fits.some(function (w) { return w === typed; })
      ? [{ word: typed, hint: "new" }] : [];
    return lead.concat(fits.map(dress));
  }

  // ================================================================== summon
  /** C-c C-s / C-c C-d, the app's own two.  The value the widget opens on is
   * the entry already standing, else today — org-read-date's own default. */
  function summon(keyword, where) {
    if (state.open) return;
    var look = state.look;
    var had = where === "plan"
      ? (state.plan.filter(function (p) { return p[0] === keyword; })[0]
         || [null, ""])[1]
      : state.pair.value;
    var r = parse(had, state.today, look.english);
    var at = r.ok && r.start ? r.start : state.today;
    // THE FIELD OPENS ON THE VALUE THE WIDGET STANDS ON.  Where there is a grid
    // they are ONE value — a widget whose calendar stood on today while its
    // field said nothing would be two widgets in a coat, and `RET' would have
    // to pick.  Where there is no grid the reason survives the frame: the
    // shifted arrows ADJUST a value, and an empty field has none to adjust.
    // A opens EMPTY, which is what the shipped prompt does — it is blind, and
    // has neither a grid nor a walk to agree with.
    state.open = {
      keyword: keyword, where: where,
      text: had || (look.field === "blind" ? "" : iso(at)), at: at,
      english: look.english, inField: look.field !== "door", virgin: true,
      offers: [], offerAt: -1, stamp: "", refused: "", note: "",
      // WHAT THE EDIT FOUND, remembered at edit-OPEN: the cancel puts back the
      // spelling the edit found, never the one it was given.
      wasPlan: state.plan.map(function (p) { return p.slice(); }),
      wasPair: state.pair.value,
    };
    resolve();
    // The picture BEFORE the widget, taken by drawing the pane once with the
    // summon suppressed: ESC has a byte string to land on.
    state.open.before = pictureWithout();
    draw();
    focusIfNeeded();
    repaint();
  }

  function pictureWithout() {
    var keep = state.open;
    state.open = null;
    drawPane();
    var p = picture();
    state.open = keep;
    return p;
  }

  function resolve() {
    var o = state.open;
    if (!o) return;
    var r = parse(o.text, state.today, o.english);
    // WHAT THE ECHO SAID LAST TIME, kept for one fault alone: an echo one
    // resolve behind the field is the failure a live preview is FOR, and it is
    // invisible in every screenshot.
    o.prev = o.stamp;
    o.parsed = r;
    o.refused = r.ok ? "" : r.why;
    o.short = r.ok ? "" : (r.short || r.why);
    o.writing = r.ok ? false : writing(o.text, r);
    o.unfinished = !!r.unfinished;
    o.stamp = r.ok ? stampOf(r) : "";
    o.note = r.ok && r.verbatim ? "verbatim — org's own spelling, kept"
           : r.ok && r.end ? "a range, org's own “--” pair"
           : r.ok && r.clear ? "empty clears the entry"
           : r.ok && r.shifted ? "the shift resolved" : "";
    if (r.ok && r.start) o.at = r.start;
    o.range = r.ok && r.end ? { a: dnum(r.start), b: dnum(r.end) } : null;
    o.offers = offersFor(o);
    if (o.offerAt >= o.offers.length) o.offerAt = o.offers.length - 1;
  }

  // WALKING WRITES THE FIELD.  The grid and the text are ONE value with two
  // views, which is org-read-date's own model: walking the calendar rewrites
  // the minibuffer, and typing the minibuffer moves the calendar.
  function walkTo(c) {
    if (!state.open) return;
    state.open.at = c;
    state.open.text = iso(c);
    resolve();
    draw();
    focusIfNeeded();
    repaint();
  }
  function step(days) {
    var o = state.open;
    if (!o) return;
    // The fault: the day walk takes two.  A widget whose `n' skipped a day
    // looks right in every screenshot and is wrong in every use.
    walkTo(addDays(o.at, bug("skip") && Math.abs(days) === 1 ? 2 * days : days));
  }
  function stepMonth(n) {
    if (state.open) walkTo(addMonths(state.open.at, n));
  }

  /** RET.  DRY AND FINAL: an offer under point is taken and NOTHING ELSE
   * happens — no trailing space, no apply — and a FINISHED term applies.  An
   * empty field is the shipped `empty clears it'. */
  function accept() {
    var o = state.open;
    if (!o) return;
    if (state.look.mount === "raise") { commitBlind(); return; }
    if (o.offerAt >= 0 && o.offers.length) {
      var w = o.offers[o.offerAt].word;
      // AN OFFER ALREADY STANDING IN THE FIELD IS NOTHING TO TAKE, so the same
      // key goes on to apply rather than sticking here (`takeOffer').
      if (w !== o.text) {
        o.text = w; o.offerAt = -1;
        resolve(); redrawSoft(); repaint();
        return;
      }
    }
    if (o.refused) { say(o.refused); return; }
    commit();
  }

  function commit() {
    var o = state.open;
    var stamp = o.stamp;
    if (o.where === "plan") {
      var rest = state.plan.filter(function (p) { return p[0] !== o.keyword; });
      state.plan = stamp ? ORDER(rest.concat([[o.keyword, stamp]])) : rest;
    } else {
      state.pair.value = stamp;
    }
    var word = o.where === "plan" ? o.keyword : ":" + state.pair.key + ":";
    state.open = null;
    unraise();
    drawPane();
    repaint();
    say(stamp ? word + " " + stamp : word + " cleared");
  }

  /** A's commit: the prompt SHUTS, and then the wall answers.  What was typed
   * is gone by the time the refusal arrives, with nothing left on screen to
   * fix it in — which is the whole of A's argument against itself. */
  function commitBlind() {
    var o = state.open;
    if (!o) return;
    var r = parse(o.text, state.today, o.english);
    if (!r.ok) {
      var typed = o.text;
      state.open = null;
      unraise();
      drawPane();
      repaint();
      say(o.keyword + " is not a timestamp org would read back — "
          + JSON.stringify(typed) + " · " + r.why);
      return;
    }
    commit();
  }

  /** ESC CANCELS THE INPUT, AND THAT IS THE WHOLE OF WHAT IT DOES.  One press
   * abandons the open summon WHOLE — whether or not the offers stand and
   * whether or not anything was typed — and the sheet comes back spelled the
   * way the edit found it, byte for byte.  THE READER'S ESCAPE IS FROM THE
   * EDIT, NEVER FROM THE MENU. */
  function cancel() {
    var o = state.open;
    if (!o) return;
    // The fault: the graduated ladder, where the menu is a rung of its own —
    // one press takes the offers and leaves the edit standing.
    if (bug("escmenu") && o.offers.length) {
      o.offers = []; o.offerAt = -1;
      draw(); repaint();
      say("offers closed");
      return;
    }
    state.plan = o.wasPlan;
    state.pair.value = o.wasPair;
    state.open = null;
    unraise();
    drawPane();
    repaint();
    say("cancelled");
  }

  // =================================================================== paint
  function draw() {
    drawPane();
    var o = state.open;
    if (!o) { unraise(); return; }
    if (state.look.mount === "raise") { raise(o); return; }
    // `drawPane' has already put a SLOT widget in the row's value place.
    if (state.look.mount === "slot") return;
    var anchor = rowOf(o.where === "plan" ? "plan" : "pair");
    if (!anchor) return;
    var w = buildWidget(o);
    if (state.look.mount === "float") {
      // BESIDE THE FIELD, hanging where `#doffer' already hangs: absolutely
      // placed inside the pane, so the rows beneath keep their tops and the
      // widget COVERS them.  That is the popup's whole claim, and its cost.
      w.style.position = "absolute";
      var box = state.mdoc.getBoundingClientRect();
      var r = anchor.el.getBoundingClientRect();
      w.style.top = (r.bottom - box.top + state.mdoc.scrollTop + 2) + "px";
      w.style.left = (r.left - box.left + state.mdoc.scrollLeft
                      + 12 * chWidth()) + "px";
      state.mdoc.appendChild(w);
    } else {
      // DOCKED: a row in the document's own flow, so what stands under it is
      // PUSHED DOWN rather than covered.
      anchor.el.parentNode.insertBefore(w, anchor.el.nextSibling);
    }
  }

  function rowOf(kind) {
    return state.rows.filter(function (r) { return r.kind === kind; })[0];
  }

  var CH = 0;
  function chWidth() {
    if (CH) return CH;
    var p = document.createElement("span");
    p.style.cssText = "position:absolute;visibility:hidden;white-space:pre";
    p.textContent = new Array(101).join("0");
    state.mdoc.appendChild(p);
    CH = p.getBoundingClientRect().width / 100;
    p.remove();
    return CH;
  }

  function buildWidget(o) {
    var look = state.look;
    var w = document.createElement("div");
    // The fault: the edit standing in the cursor row keeps no ground of its own,
    // so its text selection is painted in the colour already behind it.  SET,
    // FOCUSED, AND INVISIBLE — the one failure no rung reading the model can
    // see, and the one a reader reports first (README round 4).
    w.className = "dw dw-" + look.mount + (bug("blend") ? " blend" : "");
    w.id = "dw";

    if (look.grid) {
      var head = part(w, "div", "dw-head", "");
      part(head, "span", "dw-month", MONTHS[o.at.m - 1] + " " + o.at.y);
      part(head, "span", "dw-keys",
           look.field === "door" ? "n p day · f b week · < > month · . today · / types"
                                 : "S-arrows walk · TAB hops to the grid");
    }

    // NOTHING IS RESTATED.  The keyword is the ROW's: on the planning line the
    // row already says SCHEDULED, and in the drawer the row already says
    // `:Due:'.  A widget standing IN that row's value slot spells neither, and
    // one standing in its own docked row spells only what the row above does
    // not — which on a PAIR is the key half, because there the widget IS the
    // pair box.
    var slot = look.mount === "slot";
    var line = part(w, "div",
                    "dw-line" + (o.where === "pair" && !slot ? " dpair" : ""), "");
    if (o.where === "pair" && !slot)
      part(line, "span", "dw-lead", ":" + state.pair.key + ":");
    var f = document.createElement("input");
    f.className = "dw-field";
    f.id = "dwfield";
    // The fault: the resolution is written INTO the field, so it is editable
    // text the caret can walk into and the reader can half-delete.
    f.value = bug("caret") ? o.text + ghostText(o) : o.text;
    f.setAttribute("spellcheck", "false");
    f.placeholder = o.english ? "2026-08-18 · today · +3d · 18 aug"
                              : "2026-08-18 · today · +3d";
    if (!o.inField) f.setAttribute("readonly", "readonly");
    // A GHOSTED FIELD IS EXACTLY AS WIDE AS WHAT IT HOLDS, so the resolution
    // lands one space after the last character typed rather than at some column
    // the layout picked.  Monospace does the arithmetic.
    if (look.ghost)
      f.style.width = Math.min(46, Math.max(1, f.value.length) + 1) + "ch";
    line.appendChild(f);
    f.addEventListener("input", function () {
      o.text = f.value;
      o.virgin = false;
      // POINT STANDS ON THE LINE THE READER TYPED, and on NOTHING over an
      // empty field (`pairMoved', 20-sheet.js:540).
      o.offerAt = f.value.trim() ? 0 : -1;
      resolve();
      redrawSoft();
      repaint();
    });
    if (look.ghost) line.appendChild(ghostEl(o));
    // THE MENU IS THE LINE'S SIBLING, not its neighbour: it belongs UNDER the
    // field it is offering into, above the grid, in the widget's own flow.
    var box = part(w, "div",
                   "doffer" + (o.offers.length && o.inField ? " on" : ""), "");
    box.id = "dwoffer";
    o.offers.forEach(function (of, i) {
      var r2 = part(box, "div", i === o.offerAt ? "dof dat" : "dof", "");
      part(r2, "span", "dow", of.word);
      part(r2, "span", "dot", of.hint);
    });

    if (look.grid === "month") w.appendChild(gridEl(o));
    else if (look.grid === "strip") w.appendChild(stripEl(o));
    if (look.echo) w.appendChild(echoEl(o));
    return w;
  }

  // Redraw without losing the caret: the field is rebuilt every paint, so the
  // text and the selection are carried across by hand.
  function redrawSoft() {
    var o = state.open;
    var f = el("dwfield");
    if (!f || !o) { draw(); return; }
    // THE CARET, AND ONLY THE CARET.  The value comes from the model on the
    // rebuild; carrying the field's own back would let a widget that wrote into
    // its own field write into it twice.
    var caret = f.selectionStart;
    draw();
    var g = el("dwfield");
    if (g) {
      if (o.inField) {
        g.removeAttribute("readonly");
        g.focus();
        g.setSelectionRange(caret, caret);
      }
    }
  }

  /** THE OPENING VALUE COMES UP WHOLLY SELECTED, never appended to.  The field
   * opens holding what the widget is standing on — the entry already written,
   * else the day it opened on — so `RET' on an untouched widget takes that
   * value the way org-read-date's does, and the FIRST CHARACTER THE READER
   * TYPES REPLACES THE WHOLE OF IT rather than landing after it.  Emptying the
   * field by hand is still the shipped `empty clears it'.
   *
   * THE SELECTION IS THE ENTRY'S ALONE.  Nothing about the commit: `RET' writes
   * the resolved stamp and closes, unadorned. */
  function focusIfNeeded() {
    var o = state.open;
    if (!o || !o.inField) return;
    var f = el(state.look.mount === "raise" ? "pinput" : "dwfield");
    if (!f) return;
    f.removeAttribute("readonly");
    f.focus();
    // The fault: the edit OPENS with the caret collapsed at the end, so the
    // reader who types to overwrite appends instead — the failure the selection
    // exists to prevent, and one no screenshot shows.
    if (o.virgin && !bug("opencaret")) f.setSelectionRange(0, f.value.length);
    else f.setSelectionRange(f.value.length, f.value.length);
  }

  /** SIX ROWS, ALWAYS.  A grid that grew and shrank with the month would push
   * the rows under a DOCKED widget by a different amount every `<' — the
   * geometry has to be the month's PICTURE and never the month's length. */
  function gridEl(o) {
    var host = document.createElement("div");
    host.className = "dw-body";
    var g = part(host, "div", "dw-grid", "");
    WEEK.forEach(function (d) { part(g, "div", "dw-dow", d); });
    var first = mondayOf({ y: o.at.y, m: o.at.m, d: 1 });
    for (var i = 0; i < 42; i += 1) {
      var c = addDays(first, i);
      part(g, "div", "dw-cell" + cellClass(o, c), String(c.d));
    }
    return host;
  }

  /** THE STRIP: a fortnight, ONE ROW TALL, the month named at the edge.  The
   * window is anchored on the Monday of point's own week, so a walk that
   * leaves it re-anchors rather than scrolling the pane sideways. */
  function stripEl(o) {
    var host = document.createElement("div");
    host.className = "dw-body";
    var s = part(host, "div", "dw-strip", "");
    part(s, "span", "dw-edge", MONTHS[o.at.m - 1].slice(0, 3) + " " + o.at.y);
    var first = mondayOf(o.at);
    for (var i = 0; i < 14; i += 1) {
      var c = addDays(first, i);
      part(s, "span", "dw-cell" + cellClass(o, c), WEEK[i % 7] + " " + pad2(c.d));
    }
    return host;
  }

  function cellClass(o, c) {
    var n = dnum(c), t = dnum(state.today), cls = "";
    if (c.m !== o.at.m) cls += " out";
    if (n === t) cls += " now";
    if (n === dnum(o.at)) cls += " at";
    else if (o.range && n > o.range.a && n <= o.range.b) cls += " in-range";
    return cls;
  }

  /** WHAT THE GHOST SAYS, or "" for nothing.  Three states and no fourth: an
   * empty field says nothing, a term still being WRITTEN says nothing, a term
   * that RESOLVES shows the stamp `RET` would write, and a term the grammar
   * REFUSES shows the refusal's short word.  This is the dry law's complete-term
   * reading (the dot-chain spike's round 15) read for ink instead of for keys:
   * a finished term has something to say and an unfinished one has not. */
  function ghostText(o) {
    var t = String(o.text).trim();
    if (!t) return "";
    // The fault: the ghost speaks for the resolve BEFORE this one — right until
    // the first keystroke and wrong ever after.
    if (bug("stale")) return o.prev ? " → " + o.prev : "";
    if (o.refused) return o.writing ? "" : " ✗ " + o.short;
    // NOTHING TO ADD.  Where the resolution IS what was typed — which is org's
    // own bracketed spelling, kept verbatim — the ghost says nothing rather than
    // drawing the same string twice on one line.
    if (!o.stamp || o.stamp === t) return "";
    return " → " + o.stamp;
  }

  /** THE GHOST: the resolved stamp riding as trailing text right after what was
   * typed, on the input's own line — `10 jan → <2026-01-10 Sat>'.  A SPAN and
   * never the field's value: the caret cannot enter it, nothing selects it, and
   * `RET' commits the resolution rather than the characters it drew. */
  function ghostEl(o) {
    var g = document.createElement("span");
    g.className = "dw-ghost" + (o.refused && !o.writing ? " bad" : "");
    g.id = "dwghost";
    g.setAttribute("aria-hidden", "true");
    g.textContent = ghostText(o);
    return g;
  }

  /** THE RESOLVED ECHO: one line, the stamp the RET would write, the weekday
   * computed — and a refusal SPOKEN HERE, above the commit, where what was
   * typed is still on screen to be fixed. */
  function echoEl(o) {
    var e = document.createElement("div");
    e.className = "dw-echo" + (o.refused ? " bad" : "")
      + (o.parsed && o.parsed.verbatim ? " verbatim" : "");
    e.id = "dwecho";
    if (o.refused) {
      part(e, "span", "dw-arrow", "✗");
      part(e, "span", "dw-stamp", o.refused);
      return e;
    }
    part(e, "span", "dw-arrow", "→");
    // The fault: the echo speaks for the resolve BEFORE this one, so it is
    // right until the reader types and wrong ever after.
    var shown = bug("stale") ? (o.prev || "") : o.stamp;
    part(e, "span", "dw-stamp",
         shown
           ? (o.where === "plan" ? o.keyword + ": " : ":" + state.pair.key + ": ")
             + shown
           : "(cleared)");
    if (o.note) part(e, "span", "dw-note", "· " + o.note);
    return e;
  }

  function say(what) {
    var t = el("truth");
    if (t) t.textContent = what;
  }

  function repaint() {
    var o = state.open, t = el("state");
    if (!t) return;
    t.textContent = o
      ? (o.where === "plan" ? o.keyword : ":" + state.pair.key + ":")
        + " · " + iso(o.at) + " " + dow(o.at)
        + (o.inField ? " · field" : " · grid")
        + (o.offers.length && o.inField ? " · " + o.offers.length + " offers" : "")
      : (planText() || "no planning line");
  }

  // ==================================================================== keys
  var NEXT = { n: 1, j: 1 }, PREV = { p: 1, k: 1 };
  var IN = { f: 1, l: 1 }, OUT = { b: 1, h: 1 };
  var prefix = false;

  function walkRows(d) {
    state.rows[state.at].el.classList.remove("dat");
    state.at = Math.max(0, Math.min(state.rows.length - 1, state.at + d));
    state.rows[state.at].el.classList.add("dat");
    repaint();
  }

  function keys(e) {
    var k = e.key;
    // C-c C-s / C-c C-d, the app's own summons (`Keymap.hs:109').  `C-s' is
    // the browser's own find in some builds, which is the Keymap's own caveat:
    // "the org spelling, where the browser lets it through".
    if (e.ctrlKey && (k === "c" || k === "C")) {
      prefix = true; e.preventDefault(); return;
    }
    if (prefix && e.ctrlKey && (k === "s" || k === "S")) {
      prefix = false; summon("SCHEDULED", "plan"); e.preventDefault(); return;
    }
    if (prefix && e.ctrlKey && (k === "d" || k === "D")) {
      prefix = false; summon("DEADLINE", "plan"); e.preventDefault(); return;
    }
    prefix = false;
    if (e.ctrlKey || e.altKey || e.metaKey) return;

    var o = state.open;
    if (!o) {
      if (NEXT[k] || k === "ArrowDown") walkRows(1);
      else if (PREV[k] || k === "ArrowUp") walkRows(-1);
      else if (k === "Enter" && state.rows[state.at].kind === "pair")
        summon(state.pair.key, "pair");
      else if (k === "t") theme();
      else return;
      e.preventDefault();
      return;
    }

    if (k === "Escape") { cancel(); e.preventDefault(); return; }
    if (k === "Enter") { accept(); e.preventDefault(); return; }

    // THE SHIFTED ARROWS ARE THE WALK WHEREVER A FIELD HOLDS THE KEYS, which
    // is org-read-date's own answer to this very collision: in the minibuffer
    // `S-<right>' is a day and `S-<down>' a week, because the plain ones
    // belong to the caret.
    // A HAS NO WALK AT ALL: today's prompt is a text box, and a shifted arrow
    // there is a shifted arrow.
    if (e.shiftKey && state.look.field !== "blind") {
      if (k === "ArrowRight") { step(1); e.preventDefault(); return; }
      if (k === "ArrowLeft") { step(-1); e.preventDefault(); return; }
      if (k === "ArrowDown") { step(7); e.preventDefault(); return; }
      if (k === "ArrowUp") { step(-7); e.preventDefault(); return; }
    }

    if (o.inField) {
      // TAB HOPS, the pane's own edit rhyme: out of the field and onto the
      // grid, where the letters are the walk again.  WITH NO GRID THERE IS
      // NOWHERE TO HOP, and the press is swallowed rather than let out to move
      // the browser's focus off a widget that is still open.
      if (k === "Tab") {
        e.preventDefault();
        if (state.look.grid) {
          o.inField = false; o.offerAt = -1;
          resolve(); draw(); repaint();
        } else {
          say("the field is the widget — there is nothing to hop to");
        }
        return;
      }
      if (k === "ArrowDown" && o.offers.length) {
        o.offerAt = Math.min(o.offers.length - 1, o.offerAt + 1);
        redrawSoft(); repaint(); e.preventDefault(); return;
      }
      if (k === "ArrowUp" && o.offers.length) {
        o.offerAt = Math.max(0, o.offerAt - 1);
        redrawSoft(); repaint(); e.preventDefault(); return;
      }
      return;                                  // every other key is a character
    }

    // THE GRID HOLDS THE KEYS: the letters ARE the walk, three grains of it.
    if (NEXT[k] || k === "ArrowRight") step(1);
    else if (PREV[k] || k === "ArrowLeft") step(-1);
    else if (IN[k] || k === "ArrowDown") step(7);
    else if (OUT[k] || k === "ArrowUp") step(-7);
    else if (k === "<") stepMonth(-1);
    else if (k === ">") stepMonth(1);
    else if (k === ".") walkTo(state.today);
    else if (k === "/" || k === "Tab") {
      o.inField = true;
      o.offerAt = o.text.trim() ? 0 : -1;
      resolve(); draw(); focusIfNeeded(); repaint();
    } else if (k === "t") theme();
    else return;
    e.preventDefault();
  }

  function theme() {
    document.documentElement.dataset.theme =
      document.documentElement.dataset.theme === "dark" ? "light" : "dark";
    repaint();
  }

  // =================================================================== mount
  /** A's door: the shipped `askText' prompt, raised WHOLE over the sheet — a
   * title, a blind field, a foot, and the document it is asking about behind a
   * veil (`frontend/glue/30-capture.js:168' `planRows'). */
  function raise(o) {
    var p = el("prompt");
    if (!p) return;
    p.className = "on";
    el("phead").textContent = o.keyword.toLowerCase() + " · 1 row";
    el("pfoot").textContent = "RET sets it · empty clears it · ESC leaves";
    var f = el("pinput");
    if (document.activeElement !== f) f.value = o.text;
    f.oninput = function () { o.text = f.value; resolve(); repaint(); };
  }

  function unraise() {
    var p = el("prompt");
    if (p) p.className = "";
  }

  function mount(opts) {
    state.look = {};
    Object.keys(LOOK_DEFAULT).forEach(function (k) {
      state.look[k] = LOOK_DEFAULT[k];
    });
    Object.keys((opts && opts.look) || {}).forEach(function (k) {
      state.look[k] = opts.look[k];
    });
    state.mdoc = el("mdoc");
    if (state.look.klass) document.body.classList.add(state.look.klass);
    // ONE CLOCK READ, at mount: a walk that crosses midnight must not land on
    // two days (docs/invariants.md, "One clock read per request").
    var n = new Date();
    state.today = { y: n.getFullYear(), m: n.getMonth() + 1, d: n.getDate() };
    var due = addDays(state.today, 30);
    state.pair.value = "<" + iso(due) + " " + dow(due) + ">";
    drawPane();
    document.addEventListener("keydown", keys, true);
    addEventListener("resize", function () { draw(); repaint(); });
    repaint();
    return state;
  }

  return {
    mount: mount,
    // what the check reads
    today: function () { return iso(state.today); },
    picture: picture,
    plan: planText,
    state: function () {
      var o = state.open;
      return {
        open: !!o, mount: state.look.mount, grid: state.look.grid,
        field: state.look.field, english: state.look.english,
        ghost: !!state.look.ghost,
        keyword: o ? o.keyword : "", where: o ? o.where : "",
        text: o ? o.text : "", at: o ? iso(o.at) : "",
        dow: o ? dow(o.at) : "", stamp: o ? o.stamp : "",
        refused: o ? o.refused : "", note: o ? o.note : "",
        inField: o ? !!o.inField : false,
        offers: o ? o.offers.map(function (x) { return x.word; }) : [],
        offerAt: o ? o.offerAt : -1,
        before: o ? o.before : "",
        atRow: state.rows[state.at] ? state.rows[state.at].kind : "",
        plan: planText(), pair: state.pair.value,
        month: o ? MONTHS[o.at.m - 1] + " " + o.at.y : "",
        bug: BUGS.join(","),
      };
    },
    // the arithmetic, so a variant and the check can agree on one calendar
    iso: iso, dow: dow, addDays: addDays, parse: parse, stampOf: stampOf,
    theme: theme,
  };
})();
