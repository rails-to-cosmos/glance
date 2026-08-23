// The stage every variant is judged on: ONE fixture, ONE grammar, ONE docked
// box.  A variant brings its own CSS and a `look' of flags and nothing else, so
// what differs between two tabs is the entry to the COMPOSE door and never the
// rig.  `/' was the rig's in all five and was the spike's own control; D has
// since taken it for the filter STAGE's edit key, and `DEL' for the chain's own
// backspace, which is the user's answer and a DECLARED departure — `check.mjs'
// names it rather than letting it drift.
//
// A CLASSIC SCRIPT ON PURPOSE: a module over `file://' is an opaque origin and
// will not load, and every page here must open by double-clicking it.
"use strict";

var RIG = (function () {
  // ---------------------------------------------------------------- fixture
  // Six rows, the six view cells.  One title carries a DOT on purpose — the
  // chain's own separator is a legal character in every argument.
  // `closed' is the CLOSED: planning stamp and `props' the property drawer —
  // the two sources a CUSTOM column reads (`Query.hs' `customCell': `closed'
  // off `hsClosed', every other name off the drawer, folded).  They sit here so
  // the rig can DRAW a custom column rather than describe one.
  var ROWS = [
    { state: "TODO", priority: "A", title: "Ship the dot chain",
      scheduled: "2026-08-24", deadline: "2026-08-28", tags: ":spike:web:",
      closed: "", props: { owner: "dmitry", effort: "2h" } },
    { state: "NEXT", priority: "B", title: "Port table-view v1.2 notes",
      scheduled: "", deadline: "2026-08-22", tags: ":web:",
      closed: "", props: { owner: "ana", effort: "45m" } },
    { state: "TODO", priority: "B", title: "Write the release notes",
      scheduled: "2026-09-01", deadline: "", tags: ":docs:",
      closed: "", props: { owner: "ana" } },
    { state: "DONE", priority: "", title: "Rename the query keys",
      scheduled: "2026-08-11", deadline: "2026-08-12", tags: ":docs:chore:",
      closed: "2026-08-12", props: { owner: "dmitry", effort: "1h" } },
    { state: "", priority: "C", title: "Read the org-mode manual",
      scheduled: "", deadline: "", tags: ":read:", closed: "", props: {} },
    { state: "CANCELLED", priority: "A", title: "Drop the ?order= parameter",
      scheduled: "", deadline: "2026-08-30", tags: ":web:chore:",
      closed: "2026-08-19", props: { owner: "ana" } },
  ];

  /** A CUSTOM COLUMN'S CELL, the app's own law rather than a new one:
   *  `Query.hs''s `customCell' reads `closed' off the planning stamp and every
   *  other name off the property drawer, folded.  The rig stands in for the
   *  `/properties' door with a list of the keys its fixture wears. */
  var PROPS = ["owner", "effort"];

  function customCell(row, name) {
    var f = fold(name);
    if (f === "closed") return row.closed || "";
    return (row.props || {})[f] || "";
  }

  // The builtin columns: a name resolves against the KEY and the HEADER alike.
  var COLS = [
    { key: "state", head: "State" }, { key: "priority", head: "#" },
    { key: "title", head: "Title" }, { key: "scheduled", head: "Scheduled" },
    { key: "deadline", head: "Deadline" }, { key: "tags", head: "Tags" },
  ];
  var SORTABLE = ["state", "priority", "title", "scheduled", "deadline", "tag"];
  var NARROW_KEYS = ["state", "priority", "title", "scheduled", "deadline",
                     "tag", "planned", "ref", "substring"];
  var SHAPING_KEYS = ["sort", "columns", "view"];
  var ACTIVE = { TODO: 1, NEXT: 1 };
  var STATE_ORDER = { TODO: 0, NEXT: 1, DONE: 2, CANCELLED: 3 };
  var DEFAULT_CHAIN = [["state", "asc"], ["title", "asc"],
                       ["deadline", "asc"], ["scheduled", "asc"]];
  // Five metas, spelled with matched stars; where each is legal is the law's.
  var METAS = {
    state: ["*empty*", "*active*", "*inactive*"],
    priority: ["*empty*"], title: ["*empty*"], scheduled: ["*empty*"],
    deadline: ["*empty*"], tag: ["*empty*", "*archive*"], planned: ["*empty*"],
  };
  var BOOT = "state:*active* -tag:chore";

  // ------------------------------------------------------------- the grammar
  // `docs/query.md', the slice this stage needs: signs, alternatives, metas,
  // the per-axis fold of the additive-filters law, sort chains, column lists.

  /** One token, read the way `scanQuery'/`resolve' read it.  QUOTING IS THE
   *  GRAMMAR'S ONE ESCAPE, in the value position as much as at the token's
   *  head: `substring:"-x"' searches a leading hyphen, and the quotes
   *  themselves never reach the value. */
  function term(text) {
    var sign = "", s = text;
    if (s.charAt(0) === "-" || s.charAt(0) === "+") { sign = s.charAt(0); s = s.slice(1); }
    if (s.charAt(0) === '"') {
      var end = s.length > 1 && s.charAt(s.length - 1) === '"' ? s.length - 1 : s.length;
      return { text: text, sign: sign, key: "substring", value: s.slice(1, end), quoted: true };
    }
    var m = /^([a-z]+)[:=]([\s\S]*)$/.exec(s);
    var known = m && (NARROW_KEYS.indexOf(m[1]) >= 0 || SHAPING_KEYS.indexOf(m[1]) >= 0);
    if (!known) return { text: text, sign: sign, key: "substring", value: s };
    var v = m[2], q = false;
    if (v.charAt(0) === '"') {
      q = true;
      v = v.slice(1, v.length > 1 && v.charAt(v.length - 1) === '"' ? -1 : undefined);
    }
    return { text: text, sign: sign, key: m[1], value: v, quoted: q,
             shaping: SHAPING_KEYS.indexOf(m[1]) >= 0 };
  }

  /** Split on spaces, tabs, newlines and `&'; inside quotes they are literal. */
  function scan(q) {
    var out = [], i = 0, n = q.length;
    while (i < n) {
      while (i < n && /[\s&]/.test(q.charAt(i))) i += 1;
      if (i >= n) break;
      var start = i, inq = false;
      if (q.charAt(i) === "-" || q.charAt(i) === "+") i += 1;
      while (i < n && (inq || !/[\s&]/.test(q.charAt(i)))) {
        if (q.charAt(i) === '"') inq = !inq;
        i += 1;
      }
      out.push(term(q.slice(start, i)));
    }
    return out;
  }

  var fold = function (s) { return String(s).toLowerCase(); };
  var bare = function (v) { return fold(v).replace(/^\[#?|]$/g, ""); };

  /** The alternatives a token names; empties drop. */
  function alts(t) {
    return String(t.value).split("|").filter(function (a) { return a !== ""; });
  }

  // ------------------------------------------------- the date value, compiled
  // `docs/query.md''s comparison section, the slice a SQL surface needs: an
  // operator or a range on the three date keys, `*today*' for the request's own
  // day, and a SHIFT that resolves at COMPILE to a plain day literal — `w' is
  // seven days, `m' and `y' are calendar arithmetic and CLIP, so Jan 31 `+1m'
  // is February's last day and never March 3.  A value carrying neither an
  // operator nor `..' reads exactly as it read before.
  //
  // THE RIG READS ONE DAY AND NEVER THE WALL CLOCK.  The shipped grammar takes
  // one clock read per request; here the day is PINNED, because a check that
  // moved with the calendar would be a check about the calendar.
  var TODAY = "2026-08-21";
  var DATE_KEYS = { scheduled: 1, deadline: 1, planned: 1 };
  var CMP_MARKS = [">=", "<=", ">", "<"];        // declared LONGEST FIRST
  var UNIT_DAYS = { d: 1, w: 7 };
  var SHIFT_RE = /^(.*?)([+-])(\d+)([dwmy])$/;
  var pad = function (n, w) {
    var s = String(n);
    while (s.length < w) s = "0" + s;
    return s;
  };

  /** ISO shifted by N units.  `m' and `y' CLIP: a month too short for the day
   *  takes its own last one. */
  function shifted(iso, sign, n, unit) {
    var y = +iso.slice(0, 4), m = +iso.slice(5, 7), d = +iso.slice(8, 10);
    var k = sign === "-" ? -n : n;
    if (unit === "d" || unit === "w")
      return new Date(Date.UTC(y, m - 1, d) + k * UNIT_DAYS[unit] * 86400000)
        .toISOString().slice(0, 10);
    var mm = unit === "m" ? m - 1 + k : m - 1, yy = unit === "y" ? y + k : y;
    yy += Math.floor(mm / 12);
    mm = ((mm % 12) + 12) % 12;
    var last = new Date(Date.UTC(yy, mm + 1, 0)).getUTCDate();
    return pad(yy, 4) + "-" + pad(mm + 1, 2) + "-" + pad(Math.min(d, last), 2);
  }

  /** The day L names, or "" where it names none.  A SHIFT WANTS A WHOLE DAY
   *  under it — a month has no next day to name. */
  function resolveLit(l) {
    var m = SHIFT_RE.exec(String(l));
    if (!m) return String(l) === "*today*" ? TODAY : String(l);
    var base = m[1] === "*today*" ? TODAY : m[1];
    if (!/^\d{4}-\d{2}-\d{2}$/.test(base)) return "";
    return shifted(base, m[2], +m[3], m[4]);
  }

  /** V as a date value: the bare prefix, an operator and its literal, or a
   *  range.  A literal owed and missing is HALF-TYPED and narrows nothing. */
  function stampOf(v) {
    var s = String(v);
    for (var i = 0; i < CMP_MARKS.length; i += 1) {
      if (s.indexOf(CMP_MARKS[i]) === 0) {
        var lit = s.slice(CMP_MARKS[i].length);
        return lit ? { cmp: CMP_MARKS[i], lit: lit } : null;
      }
    }
    var at = s.indexOf("..");
    if (at < 0) return { prefix: s };
    var lo = s.slice(0, at), hi = s.slice(at + 2);
    return lo && hi ? { lo: lo, hi: hi } : null;
  }

  var compared = function (v) { return /^[<>]|\.\./.test(String(v)); };

  /** THE GRANULARITY LAW, one line per operator: `<' and `>=' cut at the
   *  literal's FIRST instant, `<=' and `>' at its LAST — the last instant
   *  spelled as everything the prefix reaches, so no date arithmetic is owed. */
  function cmpHit(mk, d, c) {
    if (mk === "<") return c < d;
    if (mk === ">=") return c >= d;
    if (mk === "<=") return c < d || c.indexOf(d) === 0;
    return c > d && c.indexOf(d) !== 0;
  }

  /** THE EMPTY CELL SITS OUTSIDE EVERY COMPARISON: byte order puts it below
   *  every date, which says nothing true about the row. */
  var dated = function (d, c) { return c !== "" && d !== "" && /^\d/.test(d); };

  function stampHit(s, c) {
    if (!s) return false;                        // half-typed: it narrows nothing
    if (s.prefix !== undefined) {
      var p = resolveLit(s.prefix);
      return p !== "" && c.indexOf(p) === 0;
    }
    if (s.cmp) {
      var d = resolveLit(s.lit);
      return dated(d, c) && cmpHit(s.cmp, d, c);
    }
    var lo = resolveLit(s.lo), hi = resolveLit(s.hi);
    return dated(lo, c) && dated(hi, c) && cmpHit(">=", lo, c) && cmpHit("<=", hi, c);
  }

  /** Does ROW answer this atom, under KEY's own rule? */
  function atom(key, v, row) {
    var w = fold(v);
    if (key === "state") {
      if (w === "*empty*") return row.state === "";
      if (w === "*active*") return row.state === "" || !!ACTIVE[row.state];
      if (w === "*inactive*") return row.state !== "" && !ACTIVE[row.state];
      return fold(row.state) === bare(v);
    }
    if (key === "priority") {
      if (w === "*empty*") return row.priority === "";
      return row.priority !== "" && fold(row.priority) === bare(v);
    }
    if (key === "title") {
      if (w === "*empty*") return row.title === "";
      return fold(row.title).indexOf(w) >= 0;
    }
    if (key === "scheduled" || key === "deadline") {
      if (w === "*empty*") return row[key] === "";
      // THE EMPTY CELL'S LAW LIVES IN `dated' AND NOWHERE ELSE: said twice, one
      // of the two could be wrong for a release without anything saying so.
      return stampHit(stampOf(w), fold(row[key]));
    }
    if (key === "tag") {
      if (w === "*empty*") return row.tags === "" || row.tags === "::";
      if (w === "*archive*") return fold(row.tags).indexOf(":archive:") >= 0;
      return fold(row.tags).indexOf(w) >= 0;
    }
    if (key === "planned") {
      var both = row.scheduled + " " + row.deadline;
      if (w === "*empty*") return row.scheduled === "" && row.deadline === "";
      // WHERE A KEY NAMES SEVERAL CELLS the stamp is asked of each and ORed, so
      // a RANGE on `planned' is ONE CELL INSIDE the interval — the reading no
      // pair of tokens has, two tokens ANDing at the axis instead.
      var s = stampOf(w);
      return [row.scheduled, row.deadline].some(function (c) {
        return stampHit(s, fold(c));
      }) || (w === "" && both.trim() !== "");
    }
    if (key === "ref") return false;          // no relations on this stage
    return COLS.some(function (c) { return fold(cell(row, c.key)).indexOf(w) >= 0; });
  }

  function hit(t, row) {
    var a = alts(t);
    // A `-' naming no atom negates the MATCH-EVERYTHING term; a lone `-' empties.
    if (!a.length) return true;
    return a.some(function (v) { return atom(t.key, v, row); });
  }

  var axisOf = function (t) { return t.key === "substring" ? "text" : t.key; };
  var vacuous = function (t) { return alts(t).length === 0; };

  /** The narrowing half of Q as one predicate: the per-axis law, verbatim. */
  function keeper(q) {
    var axes = {};
    scan(q).forEach(function (t) {
      if (t.shaping) return;
      // A token naming no atom is dropped ahead of grouping — unsigned and
      // added alike; the NEGATED sign keeps its inversion law.
      if (t.sign !== "-" && vacuous(t)) return;
      var a = axisOf(t), g = axes[a] || (axes[a] = { P: [], N: [], W: [] });
      g[t.sign === "-" ? "N" : t.sign === "+" ? "W" : "P"].push(t);
    });
    return function (row) {
      for (var a in axes) {
        if (!Object.prototype.hasOwnProperty.call(axes, a)) continue;
        var g = axes[a];
        var base = g.P.every(function (t) { return hit(t, row); })
          && g.N.every(function (t) { return !hit(t, row); });
        var wide = g.W.some(function (t) { return hit(t, row); });
        if (!(((g.P.length + g.N.length) > 0 && base) || wide)) return false;
      }
      return true;
    };
  }

  /** The order chain Q asks for: written order, first spelling wins. */
  function chainFor(q) {
    var toks = scan(q).filter(function (t) { return t.key === "sort" && !t.sign; });
    if (!toks.length) return DEFAULT_CHAIN;
    var out = [], seen = {};
    toks.forEach(function (t) {
      String(t.value).split("->").forEach(function (seg) {
        if (!seg) return;
        if (fold(seg) === "*none*") { out.none = true; return; }
        var bits = seg.split(":");
        var col = fold(bits[0]) === "tags" ? "tag" : fold(bits[0]);
        if (SORTABLE.indexOf(col) < 0 || seen[col]) return;
        seen[col] = 1;
        out.push([col, fold(bits[1] || "asc") === "desc" ? "desc" : "asc"]);
      });
    });
    return out;
  }

  var cell = function (row, key) { return key === "tag" ? row.tags : row[key]; };

  function rank(row, col) {
    if (col === "state") {
      return row.state === "" ? null : STATE_ORDER[row.state];
    }
    var v = cell(row, col);
    return v === "" || v === undefined ? null : fold(v);
  }

  /** Empty cells sort LAST whatever the direction; ties keep document order. */
  function ordered(rows, chain) {
    var kept = rows.slice();
    kept.forEach(function (r, i) { r._doc = i; });
    return kept.sort(function (a, b) {
      for (var i = 0; i < chain.length; i += 1) {
        var col = chain[i][0], dir = chain[i][1];
        var x = rank(a, col), y = rank(b, col);
        if (x === null && y === null) continue;
        if (x === null) return 1;
        if (y === null) return -1;
        if (x < y) return dir === "desc" ? 1 : -1;
        if (x > y) return dir === "desc" ? -1 : 1;
      }
      return a._doc - b._doc;
    });
  }

  /** The columns Q asks for: names resolve against key AND header; Title stays. */
  function columnsFor(q) {
    var toks = scan(q).filter(function (t) { return t.key === "columns" && !t.sign; });
    var names = [];
    toks.forEach(function (t) {
      String(t.value).split(",").forEach(function (n) { if (n.trim()) names.push(n.trim()); });
    });
    if (!names.length) return COLS.slice();
    var out = [], seen = {};
    names.forEach(function (n) {
      var c = COLS.filter(function (c2) {
        return fold(c2.key) === fold(n) || fold(c2.head) === fold(n);
      })[0] || { key: "custom:" + fold(n), head: n, custom: true };
      if (seen[c.key]) return;
      seen[c.key] = 1;
      out.push(c);
    });
    if (!seen.title) out.unshift(COLS[2]);
    return out;
  }

  /** What Q serves, ordered and shaped. */
  function served(q) {
    return { rows: ordered(ROWS.filter(keeper(q)), chainFor(q)), cols: columnsFor(q) };
  }

  // =========================================================== the normal form
  // ONE CANONICAL S-EXPRESSION, TWO PARSERS.  The flat grammar's reader and F's
  // typed reader each produce TERMS — sign, key, atoms — and both hand them to
  // the same builder, which is the additive proposal's own denotation written
  // out: axes grouped by key, each axis `(P∪N ≠ ∅ ∧ base) ∨ wide', with `and'
  // and `or' flattened, sorted and deduped so associativity, commutativity and
  // idempotence (its laws 1 and 4) are quotiented away.  Two spellings that
  // MEAN the same thing print the same bytes; two that do not, do not.

  var TRUE = ["true"], FALSE = ["false"];

  /** Print a node.  Values are quoted; markers and key names stay bare —
   *  `(select default)' is the fallback, `(select "default")' a column
   *  somebody named that, and a two-element select can only be the first. */
  function sx(n) {
    if (typeof n === "string") return n;
    var bareSelect = n[0] === "select" && n.length === 2 && n[1] === "default";
    return "(" + n.map(function (k, i) {
      if (typeof k !== "string") return sx(k);
      var quote = (n[0] === "atom" && i === 2)
        || (n[0] === "select" && i > 0 && !bareSelect);
      return quote ? JSON.stringify(k) : k;
    }).join(" ") + ")";
  }

  /** A DATE VALUE, WRITTEN DOWN: the shift resolves and `*today*' names the
   *  day, so two spellings of one day print one atom.  The law is the flat
   *  grammar's own — the sum is written down once, before any row is asked. */
  function normDate(v) {
    var s = stampOf(v);
    if (!s) return String(v);              // half-typed: itself, narrowing nothing
    if (s.prefix !== undefined) return resolveLit(s.prefix) || s.prefix;
    if (s.cmp) return s.cmp + (resolveLit(s.lit) || s.lit);
    return (resolveLit(s.lo) || s.lo) + ".." + (resolveLit(s.hi) || s.hi);
  }

  /** Every key's own fold, applied to one atom — org's brackets read through. */
  function normAtom(key, v) {
    var s = String(v);
    // `*today*' IS A DATE and not a cell shape, so it resolves with the shifts;
    // `*empty*' on the same key stays the meta it is.
    if (DATE_KEYS[key] && fold(s) !== "*empty*") return normDate(fold(s));
    if (/^\*[a-z]+\*$/.test(fold(s))) return fold(s);
    if (key === "state" || key === "priority") return bare(s);
    if (key === "ref") return s;                 // the one value not case-folded
    return fold(s);
  }

  function atomNode(key, v) {
    var a = normAtom(key, v);
    return /^\*[a-z]+\*$/.test(a) ? ["meta", key, a.replace(/\*/g, "")] : ["atom", key, a];
  }

  /** `and'/`or', flattened, sorted, deduped, and collapsed at one child. */
  function join(kind, kids) {
    var flat = [];
    kids.forEach(function (k) {
      if (k[0] === kind) flat = flat.concat(k.slice(1));
      else flat.push(k);
    });
    var seen = {}, keep = [];
    flat.map(function (k) { return [sx(k), k]; })
      .sort(function (a, b) { return a[0] < b[0] ? -1 : a[0] > b[0] ? 1 : 0; })
      .forEach(function (p) { if (!seen[p[0]]) { seen[p[0]] = 1; keep.push(p[1]); } });
    if (!keep.length) return kind === "and" ? TRUE : FALSE;
    if (keep.length === 1) return keep[0];
    return [kind].concat(keep);
  }

  function notNode(n) {
    return sx(n) === sx(TRUE) ? FALSE : sx(n) === sx(FALSE) ? TRUE : ["not", n];
  }

  /** A term's own expression: the alternatives, OR'd.  No atoms is ⊤ — which is
   *  why a lone `-' empties the table and nothing else does. */
  function termNode(t) {
    if (!t.atoms.length) return TRUE;
    return join("or", t.atoms.map(function (a) { return atomNode(t.key, a); }));
  }

  var AXIS_OF = function (key) { return key === "substring" ? "text" : key; };

  /** The narrowing half as axes, sorted by name. */
  function irAxes(terms) {
    var group = {};
    terms.forEach(function (t) {
      if (t.shaping || t.bad) return;
      // A token naming no atom is dropped ahead of grouping — unsigned and
      // added alike; the NEGATED sign keeps its inversion law.
      if (t.sign !== "-" && !t.atoms.length) return;
      var a = AXIS_OF(t.key), g = group[a] || (group[a] = { P: [], N: [], W: [] });
      g[t.sign === "-" ? "N" : t.sign === "+" ? "W" : "P"].push(t);
    });
    return Object.keys(group).sort().map(function (a) {
      var g = group[a];
      var wide = g.W.map(termNode);
      if (!g.P.length && !g.N.length) return ["axis", a, join("or", wide)];
      var base = join("and", g.P.map(termNode)
        .concat(g.N.map(function (t) { return notNode(termNode(t)); })));
      return ["axis", a, join("or", [base].concat(wide))];
    });
  }

  /** A column NAME to its key — against the key and the header alike, which is
   *  `columns:''s own rule and what lets the typed surface spell both in the
   *  header the reader sees. */
  function colKeyOf(n) {
    var f = fold(String(n).trim());
    var c = COLS.filter(function (c2) {
      return fold(c2.key) === f || fold(c2.head) === f;
    })[0];
    return c ? (c.key === "tags" ? "tag" : c.key) : null;
  }

  /** The order, as written: `default' until a sort token appears, then whole. */
  function chainSpecOf(segs) {
    if (!segs.length) return { kind: "default" };
    var out = [], seen = {}, none = false;
    segs.forEach(function (s) {
      if (s.none) { none = true; return; }
      var col = colKeyOf(s.col);
      if (!col || SORTABLE.indexOf(col) < 0 || seen[col]) return;
      seen[col] = 1;
      out.push([col, s.dir === "desc" ? "desc" : "asc"]);
    });
    if (none && !out.length) return { kind: "none" };
    if (out.length) return { kind: "chain", chain: out };
    // SEGMENTS THAT NAME NOTHING THE CHAIN CAN CARRY take effect nowhere: the
    // flat grammar refuses such a query outright, and the nearest honest
    // reading without a refusal path is the one an ABSENT stage has — never
    // document order, which is a meaning nobody asked for.
    return { kind: "default" };
  }

  function irOrder(spec) {
    if (spec.kind !== "chain") return ["order", spec.kind];
    return ["order"].concat(spec.chain.map(function (c) { return ["by", c[0], c[1]]; }));
  }

  /** The shape: `default' until a columns token names something.  A CUSTOM
   *  column is named by its KEY, which `resolveColumns' folds — the header it
   *  wears is the spelling as written, and display is not denotation. */
  function colsSpecOf(names) {
    var kept = names.filter(function (n) { return String(n).trim(); });
    if (!kept.length) return { kind: "default" };
    var out = [], seen = {};
    kept.forEach(function (n) {
      var c = COLS.filter(function (c2) {
        return fold(c2.key) === fold(n.trim()) || fold(c2.head) === fold(n.trim());
      })[0];
      var name = c ? c.head : fold(n.trim());
      if (seen[fold(name)]) return;
      seen[fold(name)] = 1;
      out.push(name);
    });
    if (!seen.title) out.unshift("Title");
    // NAMING EVERY COLUMN OF THE DEFAULT VIEW, IN ITS OWN ORDER, *IS* THE
    // DEFAULT: the string differs and the view does not, and this is the
    // denotation.  It is what lets a surface that must always write a `SELECT'
    // say "the six" and still round-trip through a query that says nothing.
    var six = COLS.map(function (c) { return c.head; });
    if (out.length === six.length && out.every(function (n, i) {
      return fold(n) === fold(six[i]);
    })) return { kind: "default" };
    return { kind: "list", names: out };
  }

  function irSelect(spec) {
    return spec.kind === "default" ? ["select", "default"] : ["select"].concat(spec.names);
  }

  function irOf(terms, order, cols) {
    return sx(["query", ["filter"].concat(irAxes(terms)), irOrder(order), irSelect(cols)]);
  }

  /** THE FLAT PARSER'S PATH: scan, resolve, group. */
  function irFlat(q) {
    var toks = scan(q);
    var terms = toks.filter(function (t) { return !t.shaping; })
      .map(function (t) {
        return { sign: t.sign, key: t.key, atoms: alts(t) };
      });
    var segs = [];
    toks.filter(function (t) { return t.key === "sort" && !t.sign; }).forEach(function (t) {
      String(t.value).split("->").forEach(function (s) {
        if (!s) return;
        if (fold(s) === "*none*") { segs.push({ none: true }); return; }
        var b = s.split(":");
        segs.push({ col: b[0], dir: fold(b[1] || "asc") });
      });
    });
    var names = [];
    toks.filter(function (t) { return t.key === "columns" && !t.sign; }).forEach(function (t) {
      String(t.value).split(",").forEach(function (n) { if (n.trim()) names.push(n.trim()); });
    });
    return irOf(terms, chainSpecOf(segs), colsSpecOf(names));
  }

  // ============================================== F's typed surface, in Haskell
  // Record syntax for the fields, `/=' for the negation, Haskell lists for the
  // alternatives, double-quoted literals, and ONE sum type for the metas — the
  // starred family of `docs/query.md' written as constructors.

  /** The whole constructor roster.  `where' is the fields each is legal on. */
  var CTORS = {
    Active: { meta: "*active*", on: ["state"] },
    Inactive: { meta: "*inactive*", on: ["state"] },
    Empty: { meta: "*empty*", on: ["state", "priority", "title", "scheduled",
                                   "deadline", "tag", "planned"] },
    Archive: { meta: "*archive*", on: ["tag"] },
  };
  var METACTOR = { "*active*": "Active", "*inactive*": "Inactive",
                   "*empty*": "Empty", "*archive*": "Archive" };
  var FIELDS = NARROW_KEYS.slice();

  // ------------------------------------------- the closed world, case-blind
  // QUOTING IS THE ONE DISAMBIGUATION: a BARE name is looked up in the closed
  // world — the prelude's constructors, the wrappers, the two operator words
  // and the stage's own fields — and a name that resolves to nothing is an
  // error the surface marks; a QUOTED string is an open value and is never
  // looked up.  Case carries nothing on either side of that line.
  var WRAPPERS = { all: "All", any: "Any", none: "None" };
  // THE TWO DIRECTIONS ARE CONSTRUCTORS APPLIED TO THE STRING — closed word,
  // open argument, the same figure as `state = Active' beside `state = "TODO"'.
  // The suffix spelling they replace put a second grammar inside a literal,
  // which is the one place this surface's quotes would not have meant
  // taken-as-written.
  var DIRS = { asc: "Asc", desc: "Desc" };
  var WORDS = { not: "not", raw: "raw" };
  // THE CLOSED WORDS THAT STAND ALONE.  A meta's constructor is a whole value
  // the moment it is spelled, and `None' with it; `All', `Any', `Asc', `Desc',
  // `not' and `raw' are all still waiting for an argument.  It is the one thing
  // `dslDone' needs the roster for.
  var NULLARY = Object.keys(CTORS).concat([WRAPPERS.none]);

  /** The canonical constructor a bare name spells, or null. */
  function ctorOf(v) {
    var f = fold(v);
    var hit = Object.keys(CTORS).filter(function (c) { return fold(c) === f; })[0];
    return hit || WRAPPERS[f] || DIRS[f] || null;
  }

  /** The canonical field a bare name spells IN THIS STAGE, or null. */
  function fieldOf(v, fn) {
    var f = fold(v);
    return (STAGE_FIELDS[fn] || FIELDS).filter(function (k) { return k === f; })[0] || null;
  }

  var wordOf = function (v) { return WORDS[fold(v)] || null; };

  /** How a bare name should read: its canonical spelling, or null when nothing
   *  in the closed world answers to it. */
  function canonOf(v, fn) {
    return ctorOf(v) || fieldOf(v, fn) || wordOf(v);
  }

  /** Could this still BECOME a name?  A word half-typed is not yet an error. */
  function partialName(v, fn) {
    var f = fold(v);
    return Object.keys(CTORS)
      .concat(Object.keys(WRAPPERS).map(function (k) { return WRAPPERS[k]; }))
      .concat(Object.keys(DIRS).map(function (k) { return DIRS[k]; }))
      .concat(Object.keys(WORDS), STAGE_FIELDS[fn] || FIELDS)
      .some(function (w) { return fold(w).indexOf(f) === 0; });
  }

  /** A tiny lexer: identifiers, constructors, string literals, punctuation. */
  function lexDsl(s) {
    var out = [], i = 0;
    while (i < s.length) {
      var c = s.charAt(i);
      if (/\s/.test(c)) { i += 1; continue; }
      if (c === '"') {
        var j = i + 1;
        while (j < s.length && s.charAt(j) !== '"') j += 1;
        out.push({ t: "str", v: s.slice(i + 1, j), at: i, end: Math.min(j + 1, s.length) });
        i = j + 1;
        continue;
      }
      if (s.slice(i, i + 2) === "/=") { out.push({ t: "op", v: "/=", at: i, end: i + 2 }); i += 2; continue; }
      if ("=,()[]".indexOf(c) >= 0) {
        out.push({ t: c === "=" ? "op" : "punc", v: c, at: i, end: i + 1 });
        i += 1;
        continue;
      }
      var m = /^[A-Za-z_][A-Za-z0-9_]*/.exec(s.slice(i));
      if (m) {
        // ONE KIND OF BARE NAME.  Case carries nothing here — `active',
        // `Active' and `ACTIVE' are one word — so what a name MEANS is settled
        // by looking it up in the closed world, never by its first letter.
        out.push({ t: "name", v: m[0], at: i, end: i + m[0].length });
        i += m[0].length;
        continue;
      }
      out.push({ t: "junk", v: c, at: i, end: i + 1 });
      i += 1;
    }
    return out;
  }

  /**
   * F's filter arguments, parsed to TERMS — never through the flat string, so
   * the two readers meet at the IR and nowhere earlier.
   * @returns {{terms: Array, bad: Array}}
   */
  function parseDslFilter(src) {
    var lx = lexDsl(src), i = 0, terms = [], bad = [];
    var peek = function () { return lx[i]; };
    var eat = function (v) {
      if (lx[i] && lx[i].v === v) { i += 1; return true; }
      return false;
    };
    /** A value: a literal, a list, a constructor, or `All'/`Any' over a list. */
    // A value answers with its ATOMS (the alternatives it names, flattened) and
    // its PARTS (the elements as written) — `All' spreads over the parts, so a
    // nested list inside it stays ONE token's alternation.
    function value() {
      var t = peek();
      if (!t) return null;
      if (t.t === "str") { i += 1; return { atoms: [t.v], parts: [[t.v]] }; }
      if (t.v === "[") {
        i += 1;
        var xs = [], ps = [];
        while (peek() && peek().v !== "]") {
          var v = value();
          if (!v) break;
          xs = xs.concat(v.atoms);
          ps.push(v.atoms);
          if (!eat(",")) break;
        }
        eat("]");
        return { atoms: xs, parts: ps };
      }
      if (t.t === "name") {
        var name = ctorOf(t.v);
        if (name === "All" || name === "Any") {
          i += 1;
          var inner = value();
          if (!inner) return null;
          // ALL is the intersection — one token per element; ANY is the list.
          return { atoms: inner.atoms, parts: inner.parts, spread: name === "All" };
        }
        i += 1;
        if (name && CTORS[name])
          return { atoms: [CTORS[name].meta], parts: [[CTORS[name].meta]], ctor: name };
        // A BARE NAME THE PRELUDE DOES NOT KNOW is an error, not a string: the
        // quotes are what say "open value", and they were not typed.
        bad.push(t.v + " is not a name the prelude knows — an open value is quoted");
        return { atoms: [], parts: [], err: true };
      }
      return null;
    }
    /** One item: a field constraint, a `not (…)' wrapper, `raw', or free text. */
    function item(neg) {
      var t = peek();
      if (!t) return [];
      if (t.t === "name" && wordOf(t.v) === "not") {
        i += 1;
        var open = eat("(");
        var inner = item(!neg);
        if (open) eat(")");
        if (inner.length > 1) {
          bad.push("not (…) over an intersection is De Morgan's, not a token's");
          return inner.map(function (x) { x.bad = true; return x; });
        }
        return inner;
      }
      if (t.t === "name" && wordOf(t.v) === "raw") {
        i += 1;
        var par = eat("(");
        var s = peek();
        if (!s || s.t !== "str") { bad.push("raw wants a string"); return []; }
        i += 1;
        if (par) eat(")");
        // THE ESCAPE HATCH IS THE FLAT GRAMMAR ITSELF, read by its own reader.
        return scan(s.v).map(function (tok) {
          return { sign: tok.sign, key: tok.key, atoms: alts(tok),
                   shaping: tok.shaping, raw: s.v };
        });
      }
      if (t.t === "str") {                       // a bare literal is free text
        i += 1;
        return [{ sign: neg ? "-" : "", key: "substring", atoms: [t.v] }];
      }
      if (t.t !== "name") { i += 1; return []; }
      var key = fieldOf(t.v, "filter") || t.v;
      i += 1;
      var op = peek();
      if (!op || op.t !== "op") { bad.push(key + " has no = or /="); return []; }
      i += 1;
      var sign = (op.v === "/=") !== !!neg ? "-" : "";
      var v2 = value();
      // A BARE WORD IS NOT A VALUE HERE: in Haskell it would be a variable, and
      // a half-written kwarg narrows nothing rather than inverting into a lone
      // `-' — which is what an empty NEGATED term means in the flat grammar.
      if (!v2) {
        bad.push(key + " has no value — a literal is quoted, a constructor is capitalised");
        return [{ sign: sign, key: key, atoms: [], bad: true }];
      }
      v2.atoms = v2.atoms.filter(function (a) { return a !== ""; });
      v2.parts = (v2.parts || []).map(function (p) {
        return p.filter(function (a) { return a !== ""; });
      }).filter(function (p) { return p.length; });
      if (!fieldOf(key, "filter")) bad.push(key + " is not a field");
      if (v2.ctor && CTORS[v2.ctor].on.indexOf(key) < 0)
        bad.push(v2.ctor + " is not a value of " + key);
      if (v2.spread) {
        // `All [a, b]' is one token per ELEMENT: the axis INTERSECTS, which is
        // what a repeated key does in the flat string — and an element that is
        // itself a list keeps its alternation inside that one token.
        return v2.parts.map(function (p) {
          return { sign: sign, key: key, atoms: p };
        });
      }
      return [{ sign: sign, key: key, atoms: v2.atoms }];
    }
    while (i < lx.length) {
      var got = item(false);
      got.forEach(function (x) { terms.push(x); });
      if (!eat(",")) {
        if (i < lx.length && !got.length) i += 1;   // step over what made no sense
      }
    }
    return { terms: terms, bad: bad };
  }

  /**
   * F's `.sort(columns = ["Deadline:desc", "Title"])'.  The list carries the
   * chain in written order and each item is a QUOTED NAME — columns are an open
   * set, so they sit on the string side of the closed/open law with the
   * keywords, and the constructors stay the metas' alone.
   *
   * THE DIRECTION IS A CONSTRUCTOR APPLIED TO THE NAME: `Desc "Deadline"'.  It
   * is per SEGMENT, which is what the flat grammar needs
   * (`sort:state:desc->title' and `sort:state->title:desc' are different
   * orders), and it keeps the quotes meaning taken-as-written everywhere — a
   * `:desc' suffix inside the literal would have been a second grammar hidden
   * in a value the surface otherwise treats as opaque.  `Asc' is spellable and
   * never emitted, matching the flat grammar's "nothing or `:asc'".
   *
   * `None' stands alone as a positional: `*none*' is a meta, and metas are what
   * constructors are for.
   */
  function parseDslSort(src) {
    var lx = lexDsl(src), segs = [], depth = 0, dir = "asc";
    for (var i = 0; i < lx.length; i += 1) {
      var t = lx[i];
      if (t.v === "[" || t.v === "(") { depth += 1; continue; }
      if (t.v === "]" || t.v === ")") { depth -= 1; continue; }
      if (t.v === "," || t.t === "op") continue;
      var name = t.t === "name" ? ctorOf(t.v) : null;
      if (name === "None") { segs.push({ none: true }); continue; }
      if (name === "Desc" || name === "Asc") { dir = fold(name); continue; }
      if (t.t === "name") continue;               // the `columns' kwarg's name
      if (t.t !== "str") continue;                // a bare word is not a name
      // THE STRING IS TAKEN AS WRITTEN, colon and all: a name the six do not
      // answer to is an unknown column, which the flat grammar refuses outright
      // — here it takes effect nowhere and the surface marks it.
      segs.push({ col: t.v, dir: dir, unknown: !sortName(t.v) });
      dir = "asc";
    }
    return segs;
  }

  /** F's `.columns("State", "Deadline")' — POSITIONAL, and quoted, for the same
   *  reason: a custom column is any name at all, so no roster can close it. */
  function parseDslCols(src) {
    return lexDsl(src).filter(function (t) { return t.t === "str"; })
      .map(function (t) { return t.v; });
  }

  /**
   * THE ACCEPT IS THE FORMATTER'S MOMENT.  Any case is typed and any case
   * parses; what STANDS afterwards is the canonical spelling — constructors
   * capitalised, fields and the operator words in the lower case the flat keys
   * wear.  A name nothing answers to is left exactly as written, because it is
   * an error to be shown and not a word to be corrected.  Case-only rewriting,
   * so no offset moves and the caret needs no arithmetic.
   */
  /** A FRESH ARGUMENT LEFT UNTOUCHED LEAVES NO TRACE: the comma `/' appended
   *  goes at the close, and the stage returns to the spelling it had. */
  var dslDangle = function (args) { return String(args).replace(/,\s*$/, ""); };

  function dslCanon(args, fn) {
    var out = String(args), lx = lexDsl(out);
    for (var i = lx.length - 1; i >= 0; i -= 1) {
      var t = lx[i];
      if (t.t !== "name") continue;
      var c = canonOf(t.v, fn);
      if (!c || c === t.v) continue;
      out = out.slice(0, t.at) + c + out.slice(t.end);
    }
    return out;
  }

  /** A whole chain of `.fn(…)' calls, parens balanced. */
  function parseChain(src) {
    var out = [], i = 0, s = String(src);
    while (i < s.length) {
      var dot = s.indexOf(".", i);
      if (dot < 0) break;
      var m = /^\.([A-Za-z]+)\(/.exec(s.slice(dot));
      if (!m) { i = dot + 1; continue; }
      var j = dot + m[0].length, depth = 1, inq = false;
      while (j < s.length && depth > 0) {
        var c = s.charAt(j);
        if (c === '"') inq = !inq;
        else if (!inq && (c === "(" || c === "[")) depth += 1;
        else if (!inq && (c === ")" || c === "]")) depth -= 1;
        if (depth > 0) j += 1;
      }
      out.push({ fn: fold(m[1]), args: s.slice(dot + m[0].length, j) });
      i = j + 1;
    }
    return out;
  }

  /** THE TYPED PARSER'S PATH: the same IR, reached without the flat string. */
  function irDsl(src) {
    var terms = [], segs = [], names = [];
    parseChain(src).forEach(function (st) {
      if (st.fn === "filter") terms = terms.concat(parseDslFilter(st.args).terms);
      else if (st.fn === "sort") segs = segs.concat(parseDslSort(st.args));
      else if (st.fn === "columns") names = names.concat(parseDslCols(st.args));
    });
    // A `raw' fragment may carry shaping tokens; they belong to the same halves.
    var shaping = terms.filter(function (t) { return t.shaping; });
    shaping.forEach(function (t) {
      if (t.key === "sort") {
        String(t.atoms.join("|")).split("->").forEach(function (s) {
          if (!s) return;
          if (fold(s) === "*none*") { segs.push({ none: true }); return; }
          var b = s.split(":");
          segs.push({ col: b[0], dir: fold(b[1] || "asc") });
        });
      } else if (t.key === "columns") {
        t.atoms.join("|").split(",").forEach(function (n) { if (n.trim()) names.push(n.trim()); });
      }
    });
    return irOf(terms, chainSpecOf(segs), colsSpecOf(names));
  }

  // ------------------------------------------- F: the typed surface, composed
  /** Does this atom need the grammar's one escape? */
  var needsQuote = function (a) {
    return a === "" || /[\s&:|"]/.test(a) || /^[-+]/.test(a);
  };

  /** One term as a flat token; "" where it is merely half-written and narrows
   *  nothing, null where the flat string CANNOT say what it says. */
  function flatOfTerm(t) {
    if (t.raw !== undefined) return null;         // raw items compose verbatim
    if (t.bad) return "";                         // half-written narrows nothing
    // AN EMPTY NEGATION KEEPS ITS KEY: `-tag:' inverts the match-everything
    // term the way the flat grammar says, where a bare `-' would lose the axis.
    if (!t.atoms.length) return t.sign === "-" ? "-" + t.key + ":" : "";
    var quoted = t.atoms.filter(needsQuote);
    // Quoting protects a WHOLE value; an alternation of quoted alternatives is
    // a spelling the flat grammar has no room for.
    if (quoted.length && t.atoms.length > 1) return null;
    var v = quoted.length ? '"' + t.atoms[0] + '"' : t.atoms.join("|");
    return t.sign + t.key + ":" + v;
  }

  /** F's `.filter(…)' as the flat string it stands for.  A `raw' item's own
   *  text goes through whole and once; anything the flat string cannot say is
   *  LOST rather than mis-said, and the count is the reader's to see. */
  function dslFilterFlat(src) {
    var got = parseDslFilter(src);
    var out = [], raws = [], lost = 0;
    got.terms.forEach(function (t) {
      if (t.raw !== undefined) {
        if (raws.indexOf(t.raw) < 0) raws.push(t.raw);
        return;
      }
      var f = flatOfTerm(t);
      if (f === null) { lost += 1; return; }
      if (f) out.push(f);
    });
    return { flat: out.concat(raws).join(" "), lost: lost, bad: got.bad };
  }

  var CTOR_DIR = { asc: "", desc: "Desc " };

  function dslSortFlat(src) {
    var spec = chainSpecOf(parseDslSort(src));
    if (spec.kind === "default") return "";
    if (spec.kind === "none") return "sort:*none*";
    return "sort:" + spec.chain.map(function (c) {
      return c[0] + (c[1] === "desc" ? ":desc" : "");
    }).join("->");
  }

  /** WHAT WAS WRITTEN, not what it resolves to: `Title' is always present by
   *  the grammar's own rule, so spelling it into the token would change the
   *  string the URL carries without changing what it means. */
  function dslColsFlat(src) {
    var names = parseDslCols(src).filter(function (n) { return String(n).trim(); });
    return names.length ? "columns:" + names.map(function (n) { return n.trim(); }).join(",") : "";
  }

  // --------------------------------- F: the flat string, shown as the surface
  /** One atom as F spells it: a constructor for a meta, a literal otherwise. */
  var dslAtom = function (a) {
    return METACTOR[fold(a)] || JSON.stringify(a);
  };

  var dslValue = function (atoms) {
    if (atoms.length === 1) return dslAtom(atoms[0]);
    return "[" + atoms.map(dslAtom).join(", ") + "]";
  };

  /** THE SURFACE IS NOT TOTAL over the grammar, and this is where it says so:
   *  an axis carrying both a base and a widening is `raw "…"', the flat string
   *  quoted into the typed surface rather than mis-said in it. */
  function dslOfFilter(tokens) {
    var group = {}, order = [];
    tokens.forEach(function (t) {
      var a = AXIS_OF(t.key);
      if (!group[a]) { group[a] = { P: [], N: [], W: [], all: [] }; order.push(a); }
      group[a][t.sign === "-" ? "N" : t.sign === "+" ? "W" : "P"].push(t);
      group[a].all.push(t);
    });
    var items = [];
    order.forEach(function (a) {
      var g = group[a];
      if (g.W.length && (g.P.length || g.N.length)) {
        items.push("raw " + JSON.stringify(g.all.map(function (t) { return t.text; }).join(" ")));
        return;
      }
      var field = g.all[0].key;
      if (g.W.length) {                       // the axis IS the disjunction
        var wide = [];
        g.W.forEach(function (t) { wide = wide.concat(alts(t)); });
        items.push(field + " = " + dslValue(wide));
        return;
      }
      if (g.P.length === 1) items.push(field + " = " + dslValue(alts(g.P[0])));
      else if (g.P.length > 1) {
        // Repeated plain tokens on one axis INTERSECT — `All' names that, ONE
        // ELEMENT PER TOKEN, so a token's own alternation stays a nested list.
        items.push(field + " = All ["
          + g.P.map(function (t) { return dslValue(alts(t)); }).join(", ") + "]");
      }
      g.N.forEach(function (t) {
        items.push(t.key + " /= " + dslValue(alts(t)));
      });
    });
    return items.join(", ");
  }

  /** A key back to the header the reader sees: `deadline' is `"Deadline"'. */
  function headOf(key) {
    var k = fold(key) === "tag" ? "tags" : fold(key);
    var c = COLS.filter(function (c2) { return fold(c2.key) === k; })[0];
    return c ? c.head : String(key);
  }

  function dslOfSort(value) {
    var segs = String(value).split("->").filter(Boolean);
    if (!segs.length) return "";
    if (segs.length === 1 && fold(segs[0]) === "*none*") return "None";
    return "columns = [" + segs.map(function (s) {
      var b = s.split(":");
      var name = JSON.stringify(headOf(b[0]));
      return fold(b[1] || "asc") === "desc" ? "Desc " + name : name;
    }).join(", ") + "]";
  }

  function dslOfCols(value) {
    return String(value).split(",").filter(function (n) { return n.trim(); })
      .map(function (n) { return JSON.stringify(n.trim()); }).join(", ");
  }

  // ------------------------- F: where the grammar is honest and nothing passes
  /**
   * PER-AXIS SATISFIABILITY, AND IT IS A WARNING AND NEVER A REFUSAL.  Two
   * bindings can be perfectly legal and still name a query no row can answer:
   * `tag = All ["docs", "chore"], tag /= "chore"' composes
   * `tag:docs tag:chore -tag:chore', and the flat grammar is RIGHT to serve the
   * empty table for it — the emptiness IS the answer.  What the typed surface
   * owes is the sentence, so the query still composes, still applies, and the
   * line under it says why nothing came back.
   *
   * Two rules, read over the ATOMS the surface COMPOSES and never over its
   * text, so however a binding was spelled it is judged the same:
   *
   *   (a) ANY axis — a value both REQUIRED by the base conjunction and
   *       FORBIDDEN by it.  A token every one of whose alternatives is refused
   *       is the general form: one surviving alternative is a row.
   *   (b) A SINGLE-VALUED axis — two requirements one CELL cannot answer at
   *       once.  Which axes those are is the key's OWN test rather than a list
   *       of names: `state' and `priority' match a cell exactly, so two values
   *       part; `scheduled' and `deadline' match a PREFIX, so they part unless
   *       one extends the other; `title', `planned', free text and `tag' — the
   *       tags cell being the one list — match anywhere INSIDE, where two
   *       requirements sit together happily.
   *
   * An axis carrying a WIDENING is skipped whole: `(base) ∨ wide' has a second
   * way to be true, so a contradiction in the base is not the query's.
   */
  var CELL_TEST = { state: "is", priority: "is",
                    scheduled: "starts", deadline: "starts" };

  var isMeta = function (a) { return /^\*[a-z]+\*$/.test(a); };

  /** Can ONE cell answer both atoms at once, under this key's own test? */
  function agree(key, a, b) {
    if (a === b) return true;
    // THE METAS OVERLAP BY THEIR OWN LAW — `*active*' takes the empty state in
    // with it — so no pair either of them is in is judged here.
    if (isMeta(a) || isMeta(b)) return true;
    var t = CELL_TEST[key];
    if (t === "is") return false;
    if (t === "starts") return a.indexOf(b) === 0 || b.indexOf(a) === 0;
    return true;                    // anywhere inside: one cell holds them both
  }

  /**
   * The bindings of Q that no row can satisfy — the flat TOKENS they compose
   * to, since a token is what a binding becomes, and one sentence each.
   * @returns {{said: Array<string>, tokens: Array<string>}}
   */
  function unsatisfied(q) {
    var axes = {}, order = [], said = [], tokens = [];
    scan(q).forEach(function (t) {
      if (t.shaping) return;
      if (t.sign !== "-" && vacuous(t)) return;      // the vacuity rule, first
      var a = axisOf(t);
      if (!axes[a]) { axes[a] = { P: [], N: [], W: [] }; order.push(a); }
      axes[a][t.sign === "-" ? "N" : t.sign === "+" ? "W" : "P"].push(t);
    });
    var atomsOf = function (t) {
      return alts(t).map(function (v) { return normAtom(t.key, v); });
    };
    var spelt = function (t) { return dslValue(alts(t)); };
    var blame = function (x, y, sentence) {
      [x, y].forEach(function (t) {
        if (tokens.indexOf(t.text) < 0) tokens.push(t.text);
      });
      if (said.indexOf(sentence) < 0) said.push(sentence);
    };
    order.forEach(function (a) {
      var g = axes[a];
      if (g.W.length) return;             // the widening is the other way to be
      // AN INTERVAL IS NO VALUE.  `deadline:>=A deadline:<=B' asks one cell to
      // lie between two days and is answered every day of the year, so an axis
      // carrying a comparison or a range is not read here at all — the same
      // reason the roster is the key's own test: a false warning is worse than
      // a silent one, and interval satisfiability is a law nobody asked for.
      if (g.P.concat(g.N).some(function (t) { return alts(t).some(compared); })) return;
      g.P.forEach(function (p) {
        // (a) REQUIRED AND REFUSED, alternative by alternative.
        var want = atomsOf(p);
        var by = g.N.filter(function (n) {
          var no = atomsOf(n);
          return want.some(function (x) { return no.indexOf(x) >= 0; });
        });
        var dead = by.length && want.every(function (x) {
          return by.some(function (n) { return atomsOf(n).indexOf(x) >= 0; });
        });
        if (!dead) return;
        by.forEach(function (n) {
          blame(p, n, p.key + ": " + spelt(p) + " is both required and refused"
                + " — no row can carry that");
        });
      });
      // (b) TWO REQUIREMENTS ONE CELL CANNOT ANSWER, pair by pair.  A pair is
      // what a warning can NAME, so that is the granularity of the reading.
      g.P.forEach(function (p, i) {
        g.P.slice(i + 1).forEach(function (o) {
          var can = atomsOf(p).some(function (x) {
            return atomsOf(o).some(function (y) { return agree(p.key, x, y); });
          });
          if (can) return;
          blame(p, o, p.key + ": " + spelt(p) + " and " + spelt(o)
                + " are both required — no row is both");
        });
      });
    });
    return { said: said, tokens: tokens };
  }

  // ================================================ G: SQL, the fourth dialect
  // The same three stages under SQL's own words — `SELECT' the shape, `WHERE'
  // the narrowing half, `ORDER BY' the chain — plus `FROM', which names the one
  // table there is and composes NOTHING.
  //
  // SQL HAS TWO QUOTE CHARACTERS, which is the whole of what g has and F does
  // not: `'value'' is a literal and `"name"' is an identifier.  So the columns
  // can be BARE and case-folded, SQL's own convention, and a name no bare
  // identifier can spell — a custom column with a space in it — still has a
  // spelling.  F had to put every column on the string side because one pair of
  // quotes had to carry the whole closed/open law.
  //
  // THE FRAGMENT IS THIS VARIANT'S CENTRAL LAW.  The flat grammar is axes-AND
  // with per-axis disjunction, so `AND' composes across anything and `OR' only
  // between predicates of ONE column.  A cross-axis `OR' is REFUSED — named,
  // and composing nothing at all.

  var SQL_TABLE = "headlines";
  var SQL_CLAUSE = { columns: "SELECT", from: "FROM", filter: "WHERE",
                     sort: "ORDER BY" };
  var SQL_STAGE = { select: "columns", from: "from", where: "filter",
                    "order by": "sort" };
  // The metas as SQL spells an enum: bare, upper, case-blind.  The roster is
  // F's own — the language's starred family, and never the tree's keywords.
  var SQL_META = { active: "*active*", inactive: "*inactive*",
                   empty: "*empty*", archive: "*archive*" };
  var SQL_UNIT = { day: "d", days: "d", week: "w", weeks: "w", month: "m",
                   months: "m", year: "y", years: "y" };
  var SQL_KEYWORDS = ["select", "from", "where", "order", "by", "and", "or",
                      "not", "in", "like", "between", "is", "null", "asc",
                      "desc", "current_date", "interval", "date"];
  // What each key's ONE test is, which is what a LIKE pattern has to name.
  var TEST_SAID = { is: "matches a cell exactly",
                    starts: "matches a date prefix",
                    inside: "looks inside the cell" };
  var SHAPE_SAID = { is: "an exact match", starts: "a prefix",
                     inside: "a substring", ends: "a suffix" };
  var testOf = function (key) {
    return CELL_TEST[key] || (DATE_KEYS[key] ? "starts" : "inside");
  };

  /** A tiny SQL lexer: bare names, `'literals'', `"identifiers"', the six
   *  comparisons, and the punctuation an interval and a list need. */
  function lexSql(s) {
    var out = [], i = 0, str = String(s);
    var quoted = function (q, kind) {
      var j = i + 1;
      while (j < str.length && str.charAt(j) !== q) j += 1;
      out.push({ t: kind, v: str.slice(i + 1, j), at: i,
                 end: Math.min(j + 1, str.length) });
      i = j + 1;
    };
    while (i < str.length) {
      var c = str.charAt(i);
      if (/\s/.test(c)) { i += 1; continue; }
      if (c === "'") { quoted("'", "str"); continue; }
      if (c === '"') { quoted('"', "ident"); continue; }
      var two = str.slice(i, i + 2);
      if (two === "<=" || two === ">=" || two === "<>" || two === "!=") {
        out.push({ t: "op", v: two, at: i, end: i + 2 });
        i += 2;
        continue;
      }
      if ("=<>".indexOf(c) >= 0) {
        out.push({ t: "op", v: c, at: i, end: i + 1 });
        i += 1;
        continue;
      }
      if ("(),*;+-".indexOf(c) >= 0) {
        out.push({ t: "punc", v: c, at: i, end: i + 1 });
        i += 1;
        continue;
      }
      var m = /^[A-Za-z_][A-Za-z0-9_]*/.exec(str.slice(i));
      if (m) {
        out.push({ t: "name", v: m[0], at: i, end: i + m[0].length });
        i += m[0].length;
        continue;
      }
      var d = /^\d+/.exec(str.slice(i));
      if (d) {
        out.push({ t: "num", v: d[0], at: i, end: i + d[0].length });
        i += d[0].length;
        continue;
      }
      out.push({ t: "junk", v: c, at: i, end: i + 1 });
      i += 1;
    }
    return out;
  }

  var sqlWord = function (t) { return t && t.t === "name" ? fold(t.v) : null; };
  var isKeyword = function (v) { return SQL_KEYWORDS.indexOf(fold(v)) >= 0; };

  /** A COLUMN REFERENCE, resolved against key and header alike — the same rule
   *  `columns:' has, so a reader may write the name the table shows.  In the
   *  WHERE clause the namespace is CLOSED (the twelve keys); in SELECT it is
   *  open, a custom column being whatever the drawer holds. */
  function sqlKeyOf(n) {
    var f = fold(String(n).trim());
    if (f === "tags") return "tag";
    if (NARROW_KEYS.indexOf(f) >= 0) return f;
    var c = colKeyOf(f);
    return c && NARROW_KEYS.indexOf(c) >= 0 ? c : null;
  }

  /** Is the caret inside a literal or an identifier?  TWO QUOTES, counted
   *  apart: a `'' inside `"…"' is a character and the other way round. */
  function sqlInString(args, at) {
    var s = String(args), q = null;
    for (var i = 0; i < at; i += 1) {
      var c = s.charAt(i);
      if (q) { if (c === q) q = null; }
      else if (c === "'" || c === '"') q = c;
    }
    return !!q;
  }

  /**
   * G's WHERE, parsed to TERMS — never through the flat string, so the readers
   * meet at the IR and nowhere earlier.
   *
   * `bad' is what the ink marks; `no' is the FRAGMENT LAW speaking, and it
   * refuses the whole clause.  A refused clause composes NOTHING: an `OR' is
   * the SHAPE of an expression rather than a conjunct that can be dropped, and
   * dropping it would serve MORE rows than were asked for — the one direction a
   * reader cannot check.
   * @returns {{terms: Array, bad: Array, refusals: Array, refused: boolean}}
   */
  function parseSqlWhere(src) {
    var lx = lexSql(src), i = 0, bad = [], refusals = [], refused = false;
    var say = function (m) { if (bad.indexOf(m) < 0) bad.push(m); };
    var no = function (m) {
      refused = true;
      say(m);
      if (refusals.indexOf(m) < 0) refusals.push(m);
      return null;
    };
    var peek = function (k) { return lx[i + (k || 0)]; };
    var word = function (k) { return sqlWord(peek(k)); };
    var eatWord = function (w) {
      if (word() === w) { i += 1; return true; }
      return false;
    };
    var eatPunc = function (p) {
      var t = peek();
      if (t && t.t === "punc" && t.v === p) { i += 1; return true; }
      return false;
    };

    /**
     * `OR', under the axis law.  Every arm must sit on ONE axis; at most one of
     * them may be the BASE — a conjunction, or anything negated — and the rest
     * are the widenings the flat `+' spells.  `(P∪N ≠ ∅ ∧ base) ∨ wide' read
     * backwards, which is the shape SQL can say and kwargs cannot.
     */
    function orJoin(arms) {
      if (arms.length === 1) return arms[0];
      var axes = {}, live = arms.filter(function (a) { return a.length; });
      live.forEach(function (a) {
        a.forEach(function (t) { axes[AXIS_OF(t.key)] = 1; });
      });
      var names = Object.keys(axes);
      if (names.length > 1)
        return no("OR across columns has no flat spelling — see the axis law") || [];
      if (!live.length) return [];
      var compound = live.filter(function (a) {
        return a.length > 1 || a.some(function (t) { return t.sign === "-"; });
      });
      if (compound.length > 1)
        return no("an axis takes one base and any number of widenings — "
                  + "two of these arms are bases") || [];
      if (live.some(function (a) {
        return a.some(function (t) { return t.sign === "+"; });
      })) return no("a widening is already a disjunct — it cannot be OR'd again") || [];
      if (!compound.length) {
        // EVERY ARM A SINGLE POSITIVE TERM: one token, the alternation, which
        // is law 5's agreement half — `k:v₁|v₂ ≡ k:v₁ +k:v₂' on a bare axis.
        var atoms = [];
        live.forEach(function (a) { atoms = atoms.concat(a[0].atoms); });
        return [{ sign: "", key: live[0][0].key, atoms: atoms }];
      }
      var out = compound[0].slice();
      live.forEach(function (a) {
        if (a === compound[0]) return;
        out.push({ sign: "+", key: a[0].key, atoms: a[0].atoms });
      });
      return out;
    }

    /** `NOT': the sign, where one token can carry it.  Over an intersection it
     *  is De Morgan's and no conjunction of negated tokens says that. */
    function notOf(conj) {
      if (!conj.length) return conj;
      if (conj.length > 1)
        return no("NOT over an intersection is De Morgan's, not a token's") || [];
      var t = conj[0];
      if (t.sign === "+")
        return no("NOT over a widening has no flat spelling") || [];
      return [{ sign: t.sign === "-" ? "" : "-", key: t.key, atoms: t.atoms }];
    }

    /** A date expression: `CURRENT_DATE', `DATE 'lit'' or a bare literal, with
     *  SQL's own interval arithmetic after it.  It composes the flat grammar's
     *  own spelling — `*today*+30d' — and the shift's law is that grammar's. */
    function dateValue() {
      var t = peek(), base = null;
      if (!t) return null;
      if (sqlWord(t) === "current_date") { i += 1; base = "*today*"; }
      else if (sqlWord(t) === "date") {
        i += 1;
        var s = peek();
        if (!s || s.t !== "str") { say("DATE wants a literal"); return null; }
        i += 1;
        base = s.v;
      } else if (t.t === "str") { i += 1; base = t.v; }
      else return null;
      var sign = peek();
      if (!sign || sign.t !== "punc" || (sign.v !== "+" && sign.v !== "-")) return base;
      if (word(1) !== "interval") return base;
      i += 2;
      var n = peek();
      if (!n || (n.t !== "str" && n.t !== "num") || !/^\d+$/.test(String(n.v))) {
        say("INTERVAL wants a count");
        return base;
      }
      i += 1;
      var u = SQL_UNIT[word()];
      if (!u) { say("INTERVAL wants DAY, WEEK, MONTH or YEAR"); return base; }
      i += 1;
      return base + sign.v + n.v + u;
    }

    /** One value: a literal, a bare constructor from the closed roster, or a
     *  date expression where the key takes one. */
    function valueOf(key) {
      var t = peek();
      if (!t) { say(key + " has no value"); return null; }
      if (DATE_KEYS[key]) {
        var d = dateValue();
        if (d !== null) return [d];
      }
      if (t.t === "str") { i += 1; return [t.v]; }
      if (t.t === "ident") {
        i += 1;
        say('"' + t.v + '" is an identifier where a value was owed'
            + " — a literal wears single quotes");
        return null;
      }
      if (t.t === "name") {
        var m = SQL_META[fold(t.v)];
        i += 1;
        if (!m) {
          say(t.v + " is not a value — an open value is quoted, '…'");
          return null;
        }
        if (CTORS[METACTOR[m]].on.indexOf(key) < 0)
          say(fold(t.v).toUpperCase() + " is not a value of " + key);
        return [m];
      }
      say(key + " has no value");
      return null;
    }

    function inList(key) {
      if (!eatPunc("(")) { say("IN wants a list"); return null; }
      var out = [], guard = 0;
      while (peek() && !(peek().t === "punc" && peek().v === ")") && guard < 99) {
        guard += 1;
        var v = valueOf(key);
        if (!v) break;
        out = out.concat(v);
        if (!eatPunc(",")) break;
      }
      eatPunc(")");
      return out.length ? out : null;
    }

    /**
     * LIKE'S WILDCARDS MUST NAME THE KEY'S OWN TEST.  The flat grammar has ONE
     * test per key — exact on `state' and `priority', a PREFIX on the dates,
     * INSIDE on the rest — so a pattern is taken where its shape IS that test
     * and refused where it asks for one the grammar does not have.  This is the
     * one thing the pattern says out loud that `key:value' leaves implicit.
     */
    function likeAtoms(key, pat) {
      if (pat.indexOf("_") >= 0)
        return no("LIKE's _ has no flat spelling — this grammar has no "
                  + "single-character wildcard");
      var lead = pat.charAt(0) === "%";
      var tail = pat.length > 1 && pat.charAt(pat.length - 1) === "%";
      var body = pat.slice(lead ? 1 : 0, tail ? pat.length - 1 : pat.length);
      if (body.indexOf("%") >= 0)
        return no("a % inside the pattern has no flat spelling");
      var shape = lead && tail ? "inside" : tail ? "starts" : lead ? "ends" : "is";
      if (shape === "ends")
        return no("nothing in the flat grammar anchors at the END of a cell");
      if (shape !== testOf(key))
        return no("LIKE '" + pat + "' asks for " + SHAPE_SAID[shape] + " where "
                  + key + " " + TEST_SAID[testOf(key)]);
      return [body];
    }

    /** One predicate, as the flat TERMS it stands for. */
    function predicate() {
      var t = peek();
      if (!t) return [];
      if (t.t === "str") {
        i += 1;
        say("a bare string is no predicate — free text is substring LIKE '%…%'");
        return [];
      }
      if (!(t.t === "name" || t.t === "ident")) { i += 1; return []; }
      var key = sqlKeyOf(t.v);
      i += 1;
      if (!key) { say(t.v + " is not a column"); return []; }
      var neg = false, atoms = null;
      if (word() === "not" && (word(1) === "in" || word(1) === "like")) {
        neg = true;
        i += 1;
      }
      var w = word();
      if (w === "in") {
        i += 1;
        atoms = inList(key);
      } else if (w === "like") {
        i += 1;
        var p = peek();
        if (!p || p.t !== "str") { say("LIKE wants a pattern"); return []; }
        i += 1;
        atoms = likeAtoms(key, p.v);
      } else if (w === "between") {
        // THE RANGE, and on `planned' it says what no pair of tokens can: ONE
        // date cell inside the interval.
        i += 1;
        if (!DATE_KEYS[key])
          return no("BETWEEN is the date range — " + key + " has no interval") || [];
        var lo = dateValue();
        if (!eatWord("and")) { say("BETWEEN wants AND"); return []; }
        var hi = dateValue();
        if (lo === null || hi === null) { say("BETWEEN wants two dates"); return []; }
        atoms = [lo + ".." + hi];
      } else if (w === "is") {
        // SQL ALREADY HAS A WORD FOR THE EMPTY CELL, and it is the one meta the
        // flat grammar spells most awkwardly.
        i += 1;
        if (eatWord("not")) neg = true;
        if (!eatWord("null")) { say("IS wants NULL"); return []; }
        if ((METAS[key] || []).indexOf("*empty*") < 0)
          say(key + " has no empty cell to be NULL");
        atoms = ["*empty*"];
      } else {
        var op = peek();
        if (!op || op.t !== "op") { say(key + " has no operator"); return []; }
        i += 1;
        if (op.v === "<>" || op.v === "!=") neg = true;
        if (op.v === "=" || op.v === "<>" || op.v === "!=") {
          atoms = valueOf(key);
        } else {
          // THE COMPARISON IS READ ON THE DATE KEYS AND NOWHERE ELSE, which is
          // the flat grammar's own line: `title:>x' is the substring it always
          // was, so a comparison there would compose a search for the operator.
          if (!DATE_KEYS[key])
            return no("the comparison is read on the date keys alone — on "
                      + key + " it is the substring it always was") || [];
          var lit = dateValue();
          if (lit === null) { say(key + " has no date"); return []; }
          atoms = [op.v + lit];
        }
      }
      // AN EMPTY LITERAL NARROWS NOTHING — the opened slot before the reader
      // has filled it — which is the flat grammar's own reading of a value that
      // names no atom.
      if (!atoms) return [];
      atoms = atoms.filter(function (a) { return a !== ""; });
      return [{ sign: neg ? "-" : "", key: key, atoms: atoms }];
    }

    function prim() {
      if (eatWord("not")) return notOf(prim());
      if (eatPunc("(")) {
        var e = expr();
        eatPunc(")");
        return e;
      }
      return predicate();
    }

    function andExpr() {
      var out = prim();
      while (eatWord("and")) out = out.concat(prim());
      return out;
    }

    function expr() {
      var arms = [andExpr()];
      while (eatWord("or")) arms.push(andExpr());
      return orJoin(arms);
    }

    var terms = lx.length ? expr() : [];
    // WHAT THE PARSER COULD NOT REACH IS HALF-WRITTEN AND NOT WRONG.  It is
    // left alone rather than read again: the painter marks a name nothing
    // answers to, and one mistake owes the reader ONE sentence — reading the
    // tail a second time would spell a second diagnostic for the same slip.
    return { terms: refused ? [] : terms, bad: bad, refusals: refusals,
             refused: refused };
  }

  /** G's `ORDER BY': bare names, case-folded, `ASC'/`DESC' per segment, and
   *  `NULL' for the order MySQL spells that way — document order, the one
   *  thing an absent clause cannot mean. */
  function parseSqlOrder(src) {
    var lx = lexSql(src), segs = [], dir = "asc", cur = null;
    var push = function () {
      if (cur !== null) segs.push({ col: cur, dir: dir, unknown: !sortName(cur) });
      cur = null;
      dir = "asc";
    };
    lx.forEach(function (t) {
      var w = sqlWord(t);
      if (w === "asc" || w === "desc") { dir = w; return; }
      if (w === "null") { push(); segs.push({ none: true }); return; }
      if (t.t === "punc" && t.v === ",") { push(); return; }
      if (t.t === "name" || t.t === "ident") { push(); cur = t.v; return; }
    });
    push();
    return segs;
  }

  /**
   * G's `SELECT'.  A name is a bare identifier or SQL's delimited one; the
   * builtins resolve by key OR header (`resolveColumns'' own rule) and
   * EVERYTHING ELSE PASSES THROUGH AS THE CUSTOM COLUMN IT NAMES — the app
   * already reads those off the property drawer, so `SELECT owner, title'
   * composes `columns:owner,title' and invents nothing.  A single-quoted string
   * here is a literal where a column was meant, and composes nothing.
   *
   * `*' IS THE NAMED DEFAULT SET, and it is SEVEN where the app's own default
   * view is SIX: `closed' is the difference — a custom column reading the
   * planning stamp, which `viewColumns' leaves out and `*' puts in.  So the
   * star composes an EXPLICIT token; it is not the absent one.
   */
  var SQL_STAR = ["State", "#", "Title", "Scheduled", "Deadline", "Closed", "Tags"];

  function parseSqlCols(src) {
    var out = [];
    lexSql(src).forEach(function (t) {
      if (t.t === "punc" && t.v === "*") out = out.concat(SQL_STAR);
      else if (t.t === "name" && !isKeyword(t.v)) out.push(t.v);
      else if (t.t === "ident") out.push(t.v);
    });
    return out;
  }

  /**
   * G's `FROM': THE TAG AXIS IS THE TABLE NAMESPACE.  A tree has one row space
   * and its tags cut datasets out of it, so `FROM work' is `tag:work' and the
   * clause is a filter wearing a relational word.
   *
   * `*', `all' and `default' are the WHOLE STORE and compose nothing, which is
   * also what an omitted `FROM' means — the three aliases and the silence are
   * one thing, and the corpus says so.
   *
   * THE COMMA IS A UNION AND NOT SQL'S JOIN.  SQL's comma is a cross join: it
   * wants two relations and makes each row a PAIR of rows.  Here there is one
   * row space and a dataset is a SUBSET of it, so the only composition that
   * leaves a row a row is the union — and the union is already the flat
   * grammar's per-axis disjunction, `tag:work|home'.  The intersection needs no
   * comma either: `FROM work WHERE tag = 'home'' says it, both landing on the
   * tag axis where the axis law ANDs them.
   *
   * AND THE NAMESPACE IS OPEN, the tree's tags being the tree's: a name no row
   * wears composes all the same and serves nothing, which is the flat grammar's
   * own answer to `tag:nosuch' and not an error the surface may invent.
   */
  var SQL_ALL = { "*": 1, all: 1, "default": 1 };

  function parseSqlFrom(src) {
    return lexSql(src).filter(function (t) {
      return t.t === "name" || t.t === "ident" || (t.t === "punc" && t.v === "*");
    }).map(function (t) {
      return { name: t.v, all: !!SQL_ALL[fold(t.v)] };
    });
  }

  function sqlFromFlat(src) {
    var names = parseSqlFrom(src).filter(function (t) {
      return !t.all && String(t.name).trim();
    }).map(function (t) { return String(t.name).trim(); });
    return names.length ? "tag:" + names.join("|") : "";
  }

  // ---------------------------------------- G: the flat string a clause spells
  function sqlWhereFlat(src) {
    var got = parseSqlWhere(src);
    if (got.refused) return { flat: "", bad: got.bad, refusals: got.refusals };
    var out = [];
    got.terms.forEach(function (t) {
      var f = flatOfTerm(t);
      if (f) out.push(f);
    });
    return { flat: out.join(" "), bad: got.bad, refusals: got.refusals };
  }

  function sqlOrderFlat(src) {
    var spec = chainSpecOf(parseSqlOrder(src));
    if (spec.kind === "default") return "";
    if (spec.kind === "none") return "sort:*none*";
    return "sort:" + spec.chain.map(function (c) {
      return c[0] + (c[1] === "desc" ? ":desc" : "");
    }).join("->");
  }

  function sqlColsFlat(src) {
    var names = parseSqlCols(src).filter(function (n) { return String(n).trim(); });
    return names.length ? "columns:" + names.map(function (n) {
      return n.trim();
    }).join(",") : "";
  }

  // ------------------------------- G: the flat string, said as a SQL statement
  var sqlLit = function (v) { return "'" + String(v) + "'"; };

  /** One date atom as SQL says it: `CURRENT_DATE' for the day, the interval for
   *  the shift, and the key's own PREFIX test as `LIKE 'x%'' where the literal
   *  is not a whole day — which is the one place the flat grammar's test is
   *  invisible and SQL can spell it. */
  function sqlDate(key, v, negated) {
    var s = stampOf(v);
    var day = function (lit) {
      var m = SHIFT_RE.exec(lit);
      var base = m ? m[1] : lit;
      var head = base === "*today*" ? "CURRENT_DATE" : sqlLit(base);
      if (!m) return head;
      return head + " " + m[2] + " INTERVAL '" + m[3] + "' "
        + { d: "DAY", w: "WEEK", m: "MONTH", y: "YEAR" }[m[4]];
    };
    if (!s) return key + " = " + sqlLit(v);           // half-typed: as written
    if (s.cmp) return key + " " + s.cmp + " " + day(s.lit);
    if (s.lo) return key + (negated ? " NOT" : "") + " BETWEEN " + day(s.lo)
      + " AND " + day(s.hi);
    var whole = s.prefix === "*today*" || /^\d{4}-\d{2}-\d{2}$/.test(s.prefix);
    if (whole) return key + " " + (negated ? "<>" : "=") + " " + day(s.prefix);
    return key + (negated ? " NOT" : "") + " LIKE '" + s.prefix + "%'";
  }

  /** One binding as SQL: the constructor for a meta, `IS NULL' for the empty
   *  cell, `IN (…)' for the alternation, and the key's own operator. */
  function sqlBinding(key, atoms, sign) {
    var neg = sign === "-";
    if (atoms.length === 1) {
      var a = atoms[0];
      if (fold(a) === "*empty*") return key + " IS " + (neg ? "NOT " : "") + "NULL";
      if (METACTOR[fold(a)])
        return key + (neg ? " <> " : " = ") + fold(a).replace(/\*/g, "").toUpperCase();
      if (DATE_KEYS[key]) return sqlDate(key, a, neg);
      if (testOf(key) === "inside")
        return key + (neg ? " NOT" : "") + " LIKE '%" + a + "%'";
      return key + (neg ? " <> " : " = ") + sqlLit(a);
    }
    // THE ALTERNATION IS `IN (…)', which is the naturalness SQL is here for.
    return key + (neg ? " NOT IN (" : " IN (") + atoms.map(function (a) {
      return METACTOR[fold(a)] ? fold(a).replace(/\*/g, "").toUpperCase() : sqlLit(a);
    }).join(", ") + ")";
  }

  /**
   * THE WHOLE NARROWING HALF AS ONE SQL EXPRESSION.  The per-axis law is
   * `(base) ∨ wide', and SQL has parens and `OR', so the shape that cost F its
   * `raw "…"' escape hatch is spellable here: `(tag = 'a' AND tag <> 'b') OR
   * tag = 'c''.  The intersection needs no name either — a repeated column IS
   * the AND, where record syntax could not repeat a field and had to invent
   * `All'.
   */
  function sqlOfFilter(tokens) {
    var group = {}, order = [];
    tokens.forEach(function (t) {
      var a = AXIS_OF(t.key);
      if (!group[a]) { group[a] = { P: [], N: [], W: [] }; order.push(a); }
      group[a][t.sign === "-" ? "N" : t.sign === "+" ? "W" : "P"].push(t);
    });
    var items = [];
    order.forEach(function (a) {
      var g = group[a], key = (g.P[0] || g.N[0] || g.W[0]).key;
      var base = g.P.map(function (t) { return sqlBinding(key, alts(t), ""); })
        .concat(g.N.map(function (t) { return sqlBinding(key, alts(t), "-"); }));
      var wide = [];
      g.W.forEach(function (t) { wide = wide.concat(alts(t)); });
      if (!wide.length) { items.push(base.join(" AND ")); return; }
      // A LONE POSITIVE BASE AND ITS WIDENINGS ARE ONE ALTERNATION — law 5's
      // agreement half — so the shorter spelling is the true one.
      if (!g.N.length && g.P.length <= 1) {
        var all = (g.P.length ? alts(g.P[0]) : []).concat(wide);
        items.push(sqlBinding(key, all, ""));
        return;
      }
      items.push("(" + base.join(" AND ") + ")" + wide.map(function (v) {
        return " OR " + sqlBinding(key, [v], "");
      }).join(""));
    });
    return items.filter(Boolean).join(" AND ");
  }

  function sqlOfOrder(value) {
    var segs = String(value).split("->").filter(Boolean);
    if (!segs.length) return "";
    if (segs.length === 1 && fold(segs[0]) === "*none*") return "NULL";
    return segs.map(function (s) {
      var b = s.split(":");
      return fold(b[0]) + (fold(b[1] || "asc") === "desc" ? " DESC" : "");
    }).join(", ");
  }

  /** A COLUMN NAME AS AN IDENTIFIER: bare where a bare one can spell it, and
   *  SQL's delimited quotes where it cannot — which is the custom column with
   *  a space in it, the case that made F quote every name it had. */
  var sqlIdent = function (n) {
    return /^[A-Za-z_][A-Za-z0-9_]*$/.test(String(n).trim())
      ? fold(String(n).trim()) : '"' + String(n).trim() + '"';
  };

  function sqlOfCols(value) {
    var names = String(value).split(",").filter(function (n) { return n.trim(); });
    return names.map(sqlIdent).join(", ");
  }

  /**
   * The whole flat query as ONE STATEMENT — the sentence the clause badges cut
   * into words.
   *
   * SQL REQUIRES A `SELECT', so the render always writes one, and where the
   * query names no columns it writes THE SIX BY NAME rather than `*': the star
   * is the seven and would be a different query.  Naming the default view's own
   * columns in its own order IS the default, which is what lets the sentence be
   * written without changing what it says.  `FROM all' for the same reason: the
   * clause is a dataset filter now, and `all' is the alias for the whole store.
   */
  var sqlSix = function () {
    return COLS.map(function (c) { return sqlIdent(c.key); }).join(", ");
  };

  function sqlStatementOf(q) {
    var toks = scan(q);
    var where = toks.filter(function (t) { return stageOfToken(t) === "filter"; });
    var sortTok = toks.filter(function (t) { return t.key === "sort" && !t.sign; });
    var colTok = toks.filter(function (t) { return t.key === "columns" && !t.sign; });
    var cols = colTok.map(function (t) { return t.value; }).join(",");
    var chain = sortTok.map(function (t) { return t.value; }).join("->");
    var out = "SELECT " + (cols ? sqlOfCols(cols) : sqlSix()) + " FROM all";
    if (where.length) out += " WHERE " + sqlOfFilter(where);
    if (chain) out += " ORDER BY " + sqlOfOrder(chain);
    return out;
  }

  /** One statement split into its clauses, at the keywords, top level only. */
  function parseSqlStatement(src) {
    var s = String(src), out = [], at = 0, fn = null, depth = 0;
    var lx = lexSql(s);
    var cut = function (end, next) {
      if (fn) out.push({ fn: fn, args: s.slice(at, end) });
      fn = next;
    };
    for (var i = 0; i < lx.length; i += 1) {
      var t = lx[i], w = sqlWord(t);
      if (t.t === "punc" && t.v === "(") depth += 1;
      else if (t.t === "punc" && t.v === ")") depth -= 1;
      if (depth > 0) continue;
      var name = w === "order" && sqlWord(lx[i + 1]) === "by" ? "order by" : w;
      if (!SQL_STAGE[name]) continue;
      if (name === "and" || name === "or") continue;
      cut(t.at, SQL_STAGE[name]);
      i += name === "order by" ? 1 : 0;
      at = lx[i].end;
    }
    cut(s.length, null);
    return out;
  }

  /** THE TYPED PARSER'S PATH, third reader: the same IR, reached from SQL. */
  function irSql(src) {
    var terms = [], segs = [], names = [];
    parseSqlStatement(src).forEach(function (st) {
      if (st.fn === "filter") terms = terms.concat(parseSqlWhere(st.args).terms);
      else if (st.fn === "sort") segs = segs.concat(parseSqlOrder(st.args));
      else if (st.fn === "columns") names = names.concat(parseSqlCols(st.args));
      else if (st.fn === "from") {
        // THE DATASET IS A TAG, so `FROM' joins the narrowing half — and the
        // axis law is what ANDs it with a `WHERE tag = …' beside it.  Nothing
        // here is special-cased: it is one more term on one more axis.
        var f = sqlFromFlat(st.args);
        if (f) terms.push({ sign: "", key: "tag", atoms: alts(term(f)) });
      }
    });
    return irOf(terms, chainSpecOf(segs), colsSpecOf(names));
  }

  // ---------------------------------------------- G: the accept as a formatter
  /** ANY CASE PARSES; what STANDS is the canonical spelling — SQL's keywords
   *  upper, the columns it knows lower, the enum constructors upper.  A name
   *  nothing answers to is left exactly as written, being an error to show and
   *  not a word to correct; and so is a custom column, whose spelling is the
   *  drawer's.  Case-only, so no offset moves. */
  function sqlCanon(args, fn) {
    // …EXCEPT IN `SELECT', where a name is left exactly as typed: an unknown
    // one is a CUSTOM column, and `resolveColumns' makes the spelling its
    // HEADER.  Correcting the case there would rename the reader's column.
    if (fn === "columns") return String(args);
    var out = String(args), lx = lexSql(out);
    for (var i = lx.length - 1; i >= 0; i -= 1) {
      var t = lx[i];
      if (t.t !== "name") continue;
      var c = null;
      if (isKeyword(t.v) || SQL_UNIT[fold(t.v)]) c = fold(t.v).toUpperCase();
      else if (SQL_META[fold(t.v)]) c = fold(t.v).toUpperCase();
      else if (fn !== "columns" && sqlKeyOf(t.v)) c = sqlKeyOf(t.v);
      if (!c || c === t.v) continue;
      out = out.slice(0, t.at) + c + out.slice(t.end);
    }
    return out;
  }

  /** A FRESH ARGUMENT LEFT UNTOUCHED LEAVES NO TRACE.  Per dialect the gesture
   *  writes its own separator, so per dialect the dangle is its own: an `AND'
   *  the `/' summoned goes at the close the way F's comma does. */
  var sqlDangle = function (args) {
    return String(args).replace(/(\s+(and|or)\s*|,\s*)$/i, "");
  };

  // ------------------------------------------ G: where the caret is, and what
  //                                             may be said at that position
  /**
   * IS THE TERM AT THE CARET FINISHED?  Round 15's law, read over SQL's own
   * quoting: a closed `'literal'', a closed `"identifier"', a closed list, a
   * bare word that stands ALONE.  Two per-variant pins:
   *
   *   - a bare identifier is finished in `SELECT' and `ORDER BY', where a name
   *     is a whole answer, and UNFINISHED in `WHERE', where it is waiting for
   *     its operator — the same arity reading, asked of three namespaces;
   *   - `<' and `>' are unfinished as OPERATORS too: either can still grow an
   *     `=', so the slot does not open until the reader says it is done.
   */
  function sqlDone(fn, args, at) {
    if (sqlInString(args, at)) return false;
    var s = String(args).slice(0, at), lx = lexSql(s), end = lx[lx.length - 1];
    if (!end || !/^\s*$/.test(s.slice(end.end))) return false;   // fresh ground
    if (end.t === "str" || end.t === "ident") return true;
    if (end.t === "punc") return end.v === ")" || end.v === "*";
    if (end.t === "op" || end.t === "num" || end.t === "junk") return false;
    var w = fold(end.v);
    if (w === "null" || w === "current_date" || w === "asc" || w === "desc") return true;
    if (SQL_META[w] || SQL_UNIT[w]) return true;
    if (isKeyword(w)) return false;              // a word still owed an argument
    if (fn === "filter") return false;           // a column awaits its operator
    // A NAME IS WHOLE where a name is the whole answer — but only once it names
    // something, and half of one is still being written.  Where the namespace
    // is OPEN — `SELECT''s custom columns, `FROM''s datasets — every name names
    // something, so every name is whole.
    return fn === "columns" || fn === "from" ? true : !!sortName(end.v);
  }

  /**
   * What the caret is inside, read off the text before it: which of SQL's four
   * positions it stands at — a column, its operator, a value, or the JOIN
   * between two predicates, which is a position F does not have and where g's
   * offers are the connectives themselves.
   */
  function sqlWhere(fn, args, at) {
    var s = String(args).slice(0, at), lx = lexSql(s);
    var wants = "col", key = null, depth = 0, inList = false, between = false;
    var prev = null, keyAt = -1;
    lx.forEach(function (t, ti) {
      var w = sqlWord(t);
      if (t.t === "punc" && t.v === "(") {
        depth += 1;
        inList = !!(prev && sqlWord(prev) === "in");
        wants = inList ? "value" : "col";
      } else if (t.t === "punc" && t.v === ")") {
        depth -= 1;
        inList = false;
        wants = "join";
      } else if (t.t === "punc" && t.v === ",") wants = inList ? "value" : "col";
      else if (t.t === "op") wants = "value";
      else if (w === "and" || w === "or") {
        // BETWEEN'S OWN `AND' is not the connective, and this is the one place
        // the two are told apart.
        if (between && w === "and") { wants = "value"; between = false; }
        else { wants = "col"; key = null; }
      } else if (w === "not") { if (wants === "op") wants = "op"; }
      else if (w === "in" || w === "like" || w === "is") wants = "value";
      else if (w === "between") { wants = "value"; between = true; }
      else if (t.t === "name" || t.t === "ident") {
        if (wants === "col") { key = sqlKeyOf(t.v); wants = "op"; keyAt = ti; }
        else if (SQL_UNIT[w] || w === "current_date" || SQL_META[w] || w === "null")
          wants = "join";
      } else if (t.t === "str") {
        // A LITERAL STILL BEING WRITTEN — its closing quote untyped, which is
        // where the opened slot leaves the caret — has not finished the value
        // position it stands in.  Only a CLOSED one moves the reader on.
        var open2 = t.end === s.length && s.slice(t.at, t.end).length === t.v.length + 1;
        if (!open2) wants = inList || between ? "value" : "join";
      }
      prev = t;
    });
    if (fn !== "filter") wants = "col";
    // THE FRAGMENT IS THE LAST TOKEN, and only while it is still being written.
    var last = lx.length - 1, end = lx[last], frag = "";
    if (end && end.end === s.length) {
      var raw = s.slice(end.at, end.end);
      if (end.t === "name") frag = raw;
      else if (end.t === "str" && raw.length === end.v.length + 1) frag = raw;
    }
    // A NAME STILL BEING WRITTEN is a column being chosen; one that already
    // NAMES a column is a column CHOSEN, and what follows it is its operator —
    // which is a word, so the fragment under the caret may be the operator's
    // own (`IS', `IN', `LIKE') and not the column's at all.
    var onCol = frag && wants === "op" && keyAt === last;
    if (frag && wants === "op" && !onCol && !sqlKeyOf(frag)
        && !sqlOpOffers(key, frag).length) { wants = "col"; key = null; }
    if (onCol && !sqlKeyOf(frag)) { wants = "col"; key = null; }
    return { wants: wants, key: key, list: inList, frag: frag, onCol: !!onCol,
             at: at - frag.length, deep: depth };
  }

  var sqlCloses = function (args, at, frag) {
    return String(args).charAt(at) === "'" && /^'/.test(frag) ? 1 : 0;
  };

  /** The clause keywords that may follow this one — the transition, offered at
   *  a position where a new predicate could have begun. */
  function sqlNextClauses(fn, frag) {
    var after = { columns: ["FROM", "WHERE", "ORDER BY"], from: ["WHERE", "ORDER BY"],
                  filter: ["ORDER BY"], sort: [] };
    return (after[fn] || []).filter(function (k) {
      return fold(k).indexOf(fold(frag)) === 0;
    }).map(function (k) {
      return { text: k, insert: k, clause: SQL_STAGE[fold(k)], full: true,
               aside: "the next clause" };
    });
  }

  /**
   * THE OPERATORS A KEY TAKES, SAID OUT LOUD.  The flat grammar has one test
   * per key and never says which; here the offer list IS that law — `state'
   * gets no `LIKE '%…%'', `title' gets no `<', and every one of them lands with
   * its slot already open.
   *
   * The inserts LEAD WITH A SPACE because the column is already written and
   * SQL's operators stand apart from it: the space is the spelling's own, the
   * way F's parens are.
   */
  function sqlOpOffers(key, frag, lead) {
    if (!key) return [];
    var out = [], want = function (t) { return fold(t).indexOf(fold(frag)) === 0; };
    var add = function (text, insert, back, aside) {
      if (want(text))
        out.push({ text: text, insert: (lead === undefined ? " " : lead) + insert,
                   back: back || 0, aside: aside, full: !back });
    };
    add("= '…'", "= ''", 1, TEST_SAID[testOf(key)]);
    add("<> '…'", "<> ''", 1, "and not that");
    add("IN ( '…' )", "IN ('')", 2, "any one of them — the axis widens");
    add("NOT IN ( '…' )", "NOT IN ('')", 2, "none of them");
    if (testOf(key) === "inside") add("LIKE '%…%'", "LIKE '%%'", 2, "a substring");
    if (testOf(key) === "starts") add("LIKE '…%'", "LIKE '%'", 2, "a date prefix");
    if (DATE_KEYS[key]) {
      ["<", "<=", ">", ">="].forEach(function (o) {
        add(o + " '…'", o + " ''", 1, "the date comparison");
      });
      // ONE SLOT AT A TIME, which is the opened-slot law's own rule: the low
      // date first, and the `AND' offers itself once that literal is closed.
      add("BETWEEN … AND …", "BETWEEN ''", 1, "one cell inside the interval");
    }
    if ((METAS[key] || []).indexOf("*empty*") >= 0) {
      add("IS NULL", "IS NULL", 0, "the empty cell");
      add("IS NOT NULL", "IS NOT NULL", 0, "any cell at all");
    }
    return out;
  }

  /** The values a key takes, as SQL spells them: the constructors bare, the
   *  day as `CURRENT_DATE', the observed values single-quoted. */
  function sqlValueOffers(key, frag, closes) {
    var bare2 = frag.replace(/^'/, "");
    var out = Object.keys(CTORS).filter(function (c) {
      return CTORS[c].on.indexOf(key) >= 0 && fold(c).indexOf(fold(bare2)) === 0
        && fold(c) !== "empty";
    }).map(function (c) {
      return { text: fold(c).toUpperCase(), insert: fold(c).toUpperCase(),
               full: true, dim: true, eats: closes,
               aside: "meta · " + CTORS[c].meta,
               n: counted(function (r) { return atom(key, CTORS[c].meta, r); }) };
    });
    if (DATE_KEYS[key]) {
      if ("current_date".indexOf(fold(bare2)) === 0) {
        out.push({ text: "CURRENT_DATE", insert: "CURRENT_DATE", full: true,
                   dim: true, eats: closes, aside: "today · " + TODAY });
        out.push({ text: "CURRENT_DATE + INTERVAL '30' DAY",
                   insert: "CURRENT_DATE + INTERVAL '30' DAY", full: true,
                   dim: true, eats: closes, aside: "the shift · d w m y" });
      }
    }
    valueOffers("", key === "substring" ? "title" : key, bare2)
      .filter(function (o) { return !/^\*/.test(o.text.split(":").pop()); })
      .forEach(function (o) {
        var v = o.text.slice(o.text.indexOf(":") + 1);
        out.push({ text: sqlLit(v), insert: sqlLit(v), full: true, n: o.n,
                   aside: "", eats: closes });
      });
    return out;
  }

  /** G's offers, per clause. */
  function sqlOffers(fn, args, at) {
    var w = sqlWhere(fn, args, at);
    var closes = sqlCloses(args, at, w.frag);
    if (fn === "from") {
      // THE DATASETS ARE THE TAGS THE STORE WEARS, plus the three aliases for
      // the whole of it.  An open namespace: what is offered is what has been
      // SEEN, and a name nobody wears is still a name.
      var seen = {}, tags = [];
      ROWS.forEach(function (r) {
        r.tags.split(":").filter(Boolean).forEach(function (t) {
          if (!seen[t]) { seen[t] = 1; tags.push(t); }
        });
      });
      var from = tags.filter(function (n) {
        return fold(n).indexOf(fold(w.frag)) === 0;
      }).map(function (n) {
        return { text: n, insert: n, full: true, aside: "dataset · tag:" + n,
                 n: counted(function (r) { return atom("tag", n, r); }) };
      });
      Object.keys(SQL_ALL).forEach(function (a) {
        if (fold(a).indexOf(fold(w.frag)) === 0)
          from.push({ text: a, insert: a, full: true, dim: true,
                      aside: "the whole store — composes nothing" });
      });
      return { items: from.concat(sqlNextClauses(fn, w.frag)), stage: "sql-from",
               where: w };
    }
    if (fn === "columns") {
      var named = parseSqlCols(String(args)).map(fold);
      var cols = COLS.filter(function (c) {
        return fold(c.key).indexOf(fold(w.frag)) === 0
          && (named.indexOf(fold(c.key)) < 0 || fold(c.key) === fold(w.frag));
      }).map(function (c) {
        return { text: sqlIdent(c.key), insert: sqlIdent(c.key), full: true,
                 aside: "builtin · " + c.head };
      });
      // …AND THE OPEN HALF OF THE NAMESPACE, which the app reads off the
      // property drawer.  The rig's list stands in for the `/properties' door;
      // a name it does not know is still a column, and still composes.
      ["closed"].concat(PROPS).forEach(function (p) {
        if (fold(p).indexOf(fold(w.frag)) !== 0) return;
        if (named.indexOf(fold(p)) >= 0 && fold(p) !== fold(w.frag)) return;
        cols.push({ text: p, insert: p, full: true, dim: true,
                    aside: p === "closed" ? "custom · the planning stamp"
                                          : "custom · the property drawer" });
      });
      if ("*".indexOf(w.frag) === 0 && !String(args).trim())
        cols.unshift({ text: "*", insert: "*", full: true, dim: true,
                       aside: "the seven — the six and closed" });
      return { items: cols.concat(sqlNextClauses(fn, w.frag)), stage: "sql-select",
               where: w };
    }
    if (fn === "sort") {
      var used = parseSqlOrder(String(args)).map(function (g) { return fold(g.col); });
      var out = SORTABLE.filter(function (c) {
        return c.indexOf(fold(w.frag)) === 0
          && (used.indexOf(c) < 0 || c === fold(w.frag));
      }).map(function (c) {
        return { text: c, insert: c, full: true, aside: "A→Z, empties last" };
      });
      ["ASC", "DESC"].forEach(function (d) {
        if (fold(d).indexOf(fold(w.frag)) === 0 && String(args).trim())
          out.push({ text: d, insert: d, full: true,
                     aside: d === "DESC" ? "Z→A, empties last" : "A→Z, and never emitted" });
      });
      if ("null".indexOf(fold(w.frag)) === 0 && !String(args).trim())
        out.push({ text: "NULL", insert: "NULL", full: true, dim: true,
                   aside: "document order" });
      return { items: out, stage: "sql-order", where: w };
    }
    if (w.wants === "value" && w.key)
      return { items: sqlValueOffers(w.key, w.frag, closes), stage: "sql-value",
               where: w };
    if (w.wants === "op" && w.key) {
      // THE OPERATOR IS WRITTEN AFTER THE COLUMN, never over it — the name
      // under the caret is a name already chosen — unless what is under the
      // caret is the OPERATOR'S own first letters, which it replaces.
      var takes = w.onCol ? "" : w.frag;
      var opw = { wants: "op", key: w.key, frag: takes,
                  at: w.onCol ? at : w.at, deep: w.deep };
      return { items: sqlOpOffers(w.key, takes, w.onCol ? " " : ""),
               stage: "sql-op", where: opw };
    }
    if (w.wants === "join")
      return { items: [{ text: "AND", insert: "AND ", more: true,
                         aside: "and also — any column" },
                       { text: "OR", insert: "OR ", more: true,
                         aside: "or — THIS column only" }]
        .filter(function (o) { return fold(o.text).indexOf(fold(w.frag)) === 0; })
        .concat(sqlNextClauses(fn, w.frag)), stage: "sql-join", where: w };
    // THE COLUMN COMES WITHOUT ITS OPERATOR, where F's field came with its `='.
    // Record syntax has ONE operator and SQL has ten, so the choice is the
    // reader's and the surface owes them the list: taking a column finishes no
    // term, and that position's own offers stand at once.
    var keys = NARROW_KEYS.filter(function (k) {
      return k.indexOf(fold(w.frag)) === 0;
    }).map(function (k) {
      return { text: k, insert: k, more: true, aside: ASIDE[k] || "" };
    });
    if ("not".indexOf(fold(w.frag)) === 0)
      keys.push({ text: "NOT ( … )", insert: "NOT ()", back: 1,
                  aside: "negate what is inside" });
    return { items: keys.concat(sqlNextClauses(fn, w.frag)), stage: "sql-col",
             where: w };
  }

  // ---------------------------------------------- G: the keys the surface owns
  /** TYPING THE OPERATOR OPENS THE SLOT, the way F's `=' does — but only where
   *  the operator is FINISHED: `<' can still grow an `=', so it waits. */
  function sqlSlot(st) {
    var at = caretAt(st), s = st.args.slice(0, at), after = st.args.slice(at);
    var lx = lexSql(s), end = lx[lx.length - 1];
    if (!end || end.t !== "op" || end.end !== s.length) return;
    if (end.v === "<" || end.v === ">") return;
    if (after && !/^\s*[,)]/.test(after)) return;
    st.args = s + " ''" + after;
    st.at = at + 2;
  }

  /** A comma inside an `IN (…)' opens the next slot; everywhere else in SQL the
   *  next thing is a bare name and there is nothing to open. */
  function sqlComma(st) {
    var at = caretAt(st), s = st.args.slice(0, at), after = st.args.slice(at);
    if (s.charAt(at - 1) !== ",") return;
    if (after && !/^\s*[,)]/.test(after)) return;
    if (st.fn !== "filter" || !inBracket2(s)) return;
    st.args = s + " ''" + after;
    st.at = at + 2;
  }

  /** Is the caret inside an unclosed `(' — a list, where the items are values? */
  function inBracket2(s) {
    var depth = 0, q = null;
    for (var i = 0; i < s.length; i += 1) {
      var c = s.charAt(i);
      if (q) { if (c === q) q = null; continue; }
      if (c === "'" || c === '"') q = c;
      else if (c === "(") depth += 1;
      else if (c === ")") depth -= 1;
    }
    return depth > 0;
  }

  /** THE CLAUSE ENDS WHERE THE NEXT ONE BEGINS, which is SQL's own rule and the
   *  one gesture g does not have to invent: the reserved word closes what
   *  stands and opens what follows.  `ORDER' alone opens nothing — it is `BY'
   *  that finishes the keyword. */
  var SQL_TAIL = /(^|[\s)'"])(from|where|order\s+by)\s$/i;

  function sqlSplit(st) {
    var at = caretAt(st);
    if (String(st.args).slice(at).trim()) return false;
    var s = st.args.slice(0, at), m = SQL_TAIL.exec(s);
    if (!m) return false;
    if (sqlInString(s, m.index + m[1].length)) return false;
    st.args = s.slice(0, m.index + m[1].length).replace(/\s+$/, "");
    st.at = st.args.length;
    takeClause(SQL_STAGE[fold(m[2]).replace(/\s+/g, " ")]);
    return true;
  }

  /** G's painter: keywords, identifiers, literals, and the two namespaces. */
  function paintSql(frag, text, fn, warn, refused) {
    var lx = lexSql(text), i = 0, depth = 0;
    lx.forEach(function (t) {
      if (t.at > i) frag.appendChild(document.createTextNode(text.slice(i, t.at)));
      var w = fold(t.v), cls;
      if (t.t === "punc" && t.v === "(") depth += 1;
      if (t.t === "punc" && t.v === ")") depth -= 1;
      if (t.t === "str")
        // A LITERAL IS A LITERAL IN `WHERE' AND A MISTAKE IN THE OTHER TWO,
        // where a column was owed and single quotes name no column.
        cls = fn === "filter" ? "cx-str" : "cx-bad";
      else if (t.t === "ident") cls = "cx-col";
      else if (t.t === "op") cls = t.v === "<>" || t.v === "!=" ? "cx-neg" : "cx-eq";
      else if (t.t === "punc") cls = "cx-punc";
      else if (t.t === "num") cls = "cx-str";
      else if (SQL_META[w]) cls = "cx-ctor";
      else if (w === "not" || w === "or") cls = "cx-neg";
      else if (isKeyword(t.v) || SQL_UNIT[w]) cls = "cx-sqlkw";
      else if (fn === "columns") cls = "cx-col";
      else if (fn === "sort") cls = sortName(t.v) ? "cx-kw"
        : SORTABLE.some(function (c) { return c.indexOf(w) === 0; })
          ? "cx-partial-name" : "cx-bad";
      // THE DATASET NAMESPACE IS OPEN — the tags are the tree's — so nothing
      // here can be wrong; the three aliases wear the language's ink and every
      // other name wears a column's.
      else if (fn === "from") cls = SQL_ALL[w] ? "cx-ctor" : "cx-col";
      else cls = sqlKeyOf(t.v) ? "cx-kw"
        : NARROW_KEYS.concat(SQL_KEYWORDS).some(function (k) { return k.indexOf(w) === 0; })
          ? "cx-partial-name" : "cx-bad";
      // THE REFUSED FRAGMENT WEARS THE REFUSAL: the `OR' that has no flat
      // spelling is the word the reader has to take back, so it is the word
      // that is marked.
      if (refused && depth === 0 && w === "or") cls = "cx-bad";
      var inside = function (sp) { return t.at >= sp.start && t.end <= sp.end; };
      if (warn && warn.some(inside)) cls += " cx-warn";
      frag.appendChild(span(text.slice(t.at, t.end), cls));
      i = t.end;
    });
    if (i < text.length) frag.appendChild(document.createTextNode(text.slice(i)));
  }

  /** A WHERE clause split at its top-level connectives — what the comma is to a
   *  call's arguments, `AND' is to a predicate list.  BETWEEN's own `AND' is
   *  not one of them. */
  function sqlItemSpans(args) {
    var lx = lexSql(String(args)), depth = 0, out = [], start = 0, between = false;
    lx.forEach(function (t) {
      var w = fold(t.v);
      if (t.t === "punc" && t.v === "(") depth += 1;
      else if (t.t === "punc" && t.v === ")") depth -= 1;
      else if (t.t === "name" && depth === 0) {
        if (w === "between") { between = true; return; }
        if (w !== "and" && w !== "or") return;
        if (between && w === "and") { between = false; return; }
        out.push({ start: start, end: t.at });
        start = t.end;
      }
    });
    out.push({ start: start, end: String(args).length });
    return out;
  }

  // --------------------------------------------------------------- the state
  var S = {
    chips: [],              // the applied query, token by token — the strip
    look: {},               // the variant's flags
    door: null,             // "filter" | "compose" | null
    narrow: false,          // the `/' session refuses the shaping keys
    at: 0,                  // the row cursor
    refused: "",            // what the narrowed door would not take
  };
  var el = {};              // the mount's parts

  var query = function () { return S.chips.join(" "); };

  // ------------------------------------------------------------- completions
  // ONE ENGINE, both doors: the flat box asks for the `filter' stage (plus the
  // shaping keys when the session is whole), and the composer asks per stage.

  function counted(pred) {
    return ROWS.filter(pred).length;
  }

  function keyOffers(frag, whole) {
    var sign = /^[-+]/.test(frag) ? frag.charAt(0) : "";
    var body = sign ? frag.slice(1) : frag;
    var keys = NARROW_KEYS.concat(whole ? SHAPING_KEYS : []);
    var out = keys.filter(function (k) { return k.indexOf(fold(body)) === 0; })
      .map(function (k) {
        return { text: sign + k + ":", insert: sign + k + ":", stay: true,
                 aside: ASIDE[k] || "" };
      });
    if (body && !/[:=]/.test(body)) {
      out.push({ text: sign + body, insert: sign + body, aside: "free text",
                 n: counted(function (r) { return atom("substring", body, r); }) });
    }
    return out;
  }

  var ASIDE = {
    state: "the whole keyword", priority: "the letter", title: "substring",
    scheduled: "date prefix", deadline: "date prefix", tag: "substring of :a:b:",
    planned: "either date cell", ref: "rows linking to an id",
    substring: "free text under a key", sort: "the order", columns: "what shows",
    view: "a saved view",
  };

  /** The distinct values a key wears in the fixture, with the counts. */
  function valueOffers(sign, key, val) {
    var seen = {}, out = [];
    ROWS.forEach(function (r) {
      var v = key === "tag" ? null : cell(r, key);
      if (key === "tag") {
        r.tags.split(":").filter(Boolean).forEach(function (t) {
          if (!seen[t]) { seen[t] = 1; out.push(t); }
        });
        return;
      }
      if (key === "planned") { v = r.scheduled || r.deadline; }
      if (key === "priority" && v) v = "[#" + v + "]";
      if (v === "" || v === undefined || v === null || seen[v]) return;
      seen[v] = 1;
      out.push(v);
    });
    (METAS[key] || []).forEach(function (m) { out.push(m); });
    return out.filter(function (v) { return fold(v).indexOf(fold(val)) === 0; })
      .map(function (v) {
        return { text: sign + key + ":" + v, insert: sign + key + ":" + v,
                 full: true, dim: /^\*.*\*$/.test(v),
                 n: counted(function (r) { return atom(key, v, r); }) };
      });
  }

  /** The FILTER stage: keys before the colon, values after it. */
  function filterOffers(frag, whole) {
    var sign = /^[-+]/.test(frag) ? frag.charAt(0) : "";
    var body = sign ? frag.slice(1) : frag;
    var m = /^([a-z]+)[:=]([\s\S]*)$/.exec(body);
    if (!m || NARROW_KEYS.concat(whole ? SHAPING_KEYS : []).indexOf(m[1]) < 0)
      return { items: keyOffers(frag, whole), stage: "key" };
    if (SHAPING_KEYS.indexOf(m[1]) >= 0)
      return m[1] === "sort" ? { items: sortOffers(m[2], sign), stage: "value" }
           : m[1] === "columns" ? { items: colsOffers(m[2], sign), stage: "value" }
           : { items: [], stage: "value" };
    return { items: valueOffers(sign, m[1], m[2]), stage: "value" };
  }

  // ---------------------------------------------- the comma, per stage
  // A COMMA IS AN ARGUMENT SEPARATOR INSIDE THE PARENS, which is what a call
  // looks like everywhere else; the flat string it composes to is the stage's
  // own separator, and the three stages do not agree on what that is.

  /** Does a new token begin here?  A sign, or one of the grammar's own keys. */
  var opensToken = function (rest) {
    return rest === "" || /^\s/.test(rest) || /^[-+]/.test(rest) || /^[a-z]+[:=]/.test(rest);
  };

  /**
   * `.filter(…)''s arguments, split.  Whitespace separates as it always did; a
   * comma separates when a new TOKEN begins after it, and belongs to the value
   * when one does not — so `tag:a,b' stays one token and `tag:a,priority:A' is
   * two.  Quoting wins over both, as it does in the flat string.
   */
  function filterTokens(args) {
    var out = [], buf = "", q = false;
    for (var i = 0; i < args.length; i += 1) {
      var ch = args.charAt(i);
      if (ch === '"') { q = !q; buf += ch; continue; }
      if (!q && /\s/.test(ch)) {
        if (buf) { out.push(buf); buf = ""; }
        continue;
      }
      if (!q && ch === "," && opensToken(args.slice(i + 1))) {
        if (buf) { out.push(buf); buf = ""; }
        continue;
      }
      buf += ch;
    }
    if (buf) out.push(buf);
    return out;
  }

  /** `.sort(…)''s segments: a comma is the arrow, spelled the short way. */
  var sortSegs = function (args) {
    return String(args).split(/\s*(?:,|->)\s*/).filter(Boolean);
  };

  /** `.columns(…)''s names: the comma is already the flat list's own. */
  var colParts = function (args) {
    return String(args).split(",").map(function (p) { return p.trim(); }).filter(Boolean);
  };

  /** The flat spelling of one stage — the ONE truth the chain is a view of. */
  function stageString(fn, args) {
    var a = String(args).trim();
    if (!fn || !a) return "";
    if (S.look.sql) {
      // `FROM' NAMES A DATASET, and a dataset is a TAG — so the clause composes
      // onto the tag axis, which is where the strip will keep it.
      return fn === "filter" ? sqlWhereFlat(a).flat
           : fn === "sort" ? sqlOrderFlat(a)
           : fn === "columns" ? sqlColsFlat(a) : sqlFromFlat(a);
    }
    if (S.look.dsl) {
      return fn === "filter" ? dslFilterFlat(a).flat
           : fn === "sort" ? dslSortFlat(a) : dslColsFlat(a);
    }
    if (fn === "filter") return filterTokens(a).join(" ");
    if (fn === "sort") {
      var s = sortSegs(a);
      return s.length ? "sort:" + s.join("->") : "";
    }
    var c = colParts(a);
    return c.length ? "columns:" + c.join(",") : "";
  }

  /** Where the fragment under the caret starts: after the last separator. */
  var fragAt = function (a, re) {
    var m = String(a).match(re);
    return m ? m.index + m[0].length : 0;
  };

  /** The SORT stage: the six columns, the two directions, and the `->' chain. */
  function sortOffers(frag, pre) {
    var lead = pre === undefined ? "" : pre + "sort:";
    // The separators are kept VERBATIM: a completion never rewrites a comma the
    // reader typed into an arrow.
    var bits = String(frag).split(/(\s*(?:,|->)\s*)/);
    var head = bits.slice(0, -1).join(""), last = bits[bits.length - 1];
    var join = function (seg) { return lead + head + seg; };
    var dirBits = last.split(":");
    var chainOn = function (out) {
      out.push({ text: lead + head + last + "->", insert: lead + head + last + "->",
                 stay: true, aside: "then by …" });
      return out;
    };
    if (dirBits.length > 1) {
      var dirs = ["asc", "desc"].filter(function (d) { return d.indexOf(fold(dirBits[1])) === 0; })
        .map(function (d) {
          return { text: join(dirBits[0] + ":" + d), insert: join(dirBits[0] + ":" + d),
                   full: true, aside: d === "desc" ? "Z→A, empties last" : "A→Z, empties last" };
        });
      // Already spelled whole: what can FOLLOW is the offer, never itself.
      if (dirs.length === 1 && fold(dirBits[1]) === dirs[0].text.split(":").pop()) return chainOn([]);
      return dirs;
    }
    // A COLUMN ALREADY NAMED offers what may follow it, not itself again.
    if (SORTABLE.indexOf(fold(last)) >= 0) {
      return chainOn([
        { text: join(last + ":asc"), insert: join(last + ":asc"), full: true, aside: "A→Z, empties last" },
        { text: join(last + ":desc"), insert: join(last + ":desc"), full: true, aside: "Z→A, empties last" },
      ]);
    }
    var out = SORTABLE.filter(function (c) { return c.indexOf(fold(last)) === 0; })
      .map(function (c) {
        return { text: join(c), insert: join(c), full: true, aside: "column" };
      });
    if ("*none*".indexOf(fold(last)) === 0 || last === "*")
      out.push({ text: join("*none*"), insert: join("*none*"), full: true,
                 dim: true, aside: "document order" });
    return out;
  }

  /** The COLUMNS stage: the builtin names, comma-wise. */
  function colsOffers(frag, pre) {
    var lead = pre === undefined ? "" : pre + "columns:";
    // As in `sort', the reader's own separators stand: `State, ' keeps its
    // space here and loses it when the stage composes, the flat list carrying
    // no spaces at all.
    var bits = String(frag).split(/(\s*,\s*)/);
    var head = bits.slice(0, -1).join(""), last = bits[bits.length - 1];
    var named = bits.filter(function (_, i) { return i % 2 === 0; })
      .slice(0, -1).map(function (p) { return fold(p.trim()); });
    var join = function (n) { return lead + head + n; };
    // A NAME ALREADY SPELLED offers the comma, never itself again.
    if (last && COLS.some(function (c) { return fold(c.head) === fold(last.trim()); }))
      return [{ text: lead + head + last + ",", insert: lead + head + last + ",",
                stay: true, aside: "and …" }];
    var out = COLS.filter(function (c) {
      return named.indexOf(fold(c.head)) < 0 && fold(c.head).indexOf(fold(last)) === 0;
    }).map(function (c) {
      return { text: join(c.head), insert: join(c.head), full: true, aside: "builtin" };
    });
    if (last && !out.length)
      out.push({ text: join(last), insert: join(last), full: true, dim: true,
                 aside: "custom — reads the property drawer" });
    return out;
  }

  var FN_OFFERS = [
    { text: "filter", aside: "narrow the rows" },
    { text: "sort", aside: "order them" },
    { text: "columns", aside: "choose what shows" },
  ];

  // G'S FOUR CLAUSES, in SQL's own written order.  A reader may open any of
  // them first — the strip prints them in this order whatever order they were
  // written, which is a DISPLAY rule where SQL's is a grammar rule.
  var SQL_OFFERS = [
    { text: "SELECT", aside: "what shows · * is the default" },
    { text: "FROM", aside: "the one table · inert" },
    { text: "WHERE", aside: "narrow the rows" },
    { text: "ORDER BY", aside: "order them" },
  ];

  var NOTE = {
    key: "TAB completes · RET applies · ESC drops",
    value: "TAB completes · RET applies · ESC drops",
    fn: "TAB or RET takes the call · ESC drops the chain",
    filter: "TAB completes · , or space separates · ) closes the stage · . chains",
    sort: "TAB completes · , or -> chains a column · ) closes the stage · . chains",
    columns: "TAB completes · , adds a column · ) closes the stage · . chains",
    "dsl-field": "TAB completes · - negates with /= · , separates · ) closes",
    "dsl-value": "TAB completes · + makes it a list · - negates with /= · ) closes",
    "dsl-sort": "TAB completes · Desc reverses · , chains · ) closes",
    "dsl-cols": "TAB completes · , adds a column · ) closes",
    "sql-clause": "TAB or RET takes the clause · ESC drops the statement",
    "sql-col": "TAB completes · AND joins any column · ; closes the clause",
    "sql-op": "TAB completes · the operators this column takes · ; closes",
    "sql-value": "TAB completes · 'literal' · BARE is a meta · ; closes",
    "sql-join": "AND across anything · OR within ONE column · ; closes",
    "sql-order": "TAB completes · DESC reverses · , chains · ; closes",
    "sql-select": "TAB completes · , adds a column · ; closes",
    "sql-from": "one table, and it composes nothing · ; closes",
  };

  // ---------------------------------------------- F's offers, in the typed idiom
  /**
   * What the caret is inside, read off the text before it: the enclosing
   * bracket, the field whose value is being written, and the fragment.
   */
  function dslWhere(args, at) {
    var s = String(args).slice(0, at);
    var lx = lexDsl(s), stack = [], field = null, wants = "field", inList = false;
    var last = null;
    lx.forEach(function (t) {
      if (t.v === "(" || t.v === "[") { stack.push(t.v); if (t.v === "[") inList = true; }
      else if (t.v === ")" || t.v === "]") { stack.pop(); if (t.v === "]") inList = false; }
      else if (t.t === "op") { wants = "value"; }
      else if (t.v === ",") { if (!inList) { wants = "field"; field = null; } }
      else if (t.t === "name" && wants === "field") field = t.v;
      last = t;
    });
    // THE FRAGMENT IS THE LAST TOKEN, AND ONLY IF IT IS STILL BEING WRITTEN: a
    // name, or a string whose closing quote has not been typed.  Read with the
    // lexer rather than a tail regex, which cannot tell an unclosed quote from
    // the closing one of the value before it.
    var end = lx[lx.length - 1], frag = "";
    if (end && end.end === s.length) {
      var raw = s.slice(end.at, end.end);
      if (end.t === "name") frag = raw;
      else if (end.t === "str" && raw.length === end.v.length + 1) frag = raw;
    }
    return { wants: wants, field: field, list: inList, frag: frag,
             at: at - frag.length, deep: stack.length };
  }

  /**
   * The values a field takes, as F spells them.  A CONSTRUCTOR IS NO STRING:
   * taking one out of the opened slot swallows the quotes with it, where taking
   * a literal keeps them — which is `eats', the one closing quote sitting on
   * the far side of the caret.
   */
  function dslValueOffers(field, frag, closes) {
    var bare = frag.replace(/^"/, "");
    var out = Object.keys(CTORS).filter(function (c) {
      return CTORS[c].on.indexOf(field) >= 0 && fold(c).indexOf(fold(bare)) === 0;
    }).map(function (c) {
      return { text: c, insert: c, full: true, dim: true, eats: closes,
               aside: "meta · " + CTORS[c].meta,
               n: counted(function (r) { return atom(field, CTORS[c].meta, r); }) };
    });
    valueOffers("", field === "substring" ? "title" : field, bare)
      .filter(function (o) { return !/^\*/.test(o.text.split(":").pop()); })
      .forEach(function (o) {
        var v = o.text.slice(o.text.indexOf(":") + 1);
        out.push({ text: JSON.stringify(v), insert: JSON.stringify(v), full: true,
                   n: o.n, aside: "", eats: closes });
      });
    out.push({ text: 'All [ "…" ]', insert: 'All [""]', back: 2, eats: closes,
               aside: "every one of them — the axis intersects" });
    return out;
  }

  /** F's `.filter(…)': fields, then that field's own values. */
  function dslFilterOffers(args, at) {
    var w = dslWhere(args, at);
    // A CLOSING QUOTE ON THE FAR SIDE OF THE CARET is the opened slot's, and
    // whatever is taken replaces the slot whole.
    var closes = slotCloses(args, at, w.frag);
    if (w.wants === "value" && w.field)
      return { items: dslValueOffers(w.field, w.frag, closes), stage: "dsl-value", where: w };
    // THE KEY AND ITS EQUALS COME WITH AN OPENED SLOT: `state = "|"', so the
    // reader types the value and never the punctuation around it.
    var out = FIELDS.filter(function (k) { return k.indexOf(fold(w.frag)) === 0; })
      .map(function (k) {
        return { text: k + ' = "…"', insert: k + ' = ""', back: 1,
                 aside: ASIDE[k] || "" };
      });
    if ("not".indexOf(fold(w.frag)) === 0)
      out.push({ text: "not ( … )", insert: "not ()", back: 1,
                 aside: "negate what is inside" });
    if ("raw".indexOf(fold(w.frag)) === 0)
      out.push({ text: 'raw " … "', insert: 'raw ""', back: 1, dim: true,
                 aside: "the flat string, verbatim" });
    return { items: out, stage: "dsl-field", where: w };
  }

  /** A closing quote on the far side of the caret is the opened slot's. */
  var slotCloses = function (args, at, frag) {
    return String(args).charAt(at) === '"' && /^"/.test(frag) ? 1 : 0;
  };

  /** F's `.sort(…)': the `columns' kwarg, then quoted names inside its list —
   *  each offered twice, once plain and once with the `:desc' the flat grammar
   *  hangs off the segment. */
  function dslSortOffers(args, at) {
    var w = dslWhere(args, at);
    var closes = slotCloses(args, at, w.frag);
    if (w.wants === "value") {
      var bare = w.frag.replace(/^"/, "").split(":")[0];
      var out = [];
      COLS.forEach(function (c) {
        if (fold(c.head).indexOf(fold(bare)) !== 0) return;
        if (SORTABLE.indexOf(colKeyOf(c.head)) < 0) return;
        out.push({ text: JSON.stringify(c.head), insert: JSON.stringify(c.head),
                   full: true, eats: closes, aside: "A→Z, empties last" });
        out.push({ text: "Desc " + JSON.stringify(c.head),
                   insert: "Desc " + JSON.stringify(c.head),
                   full: true, eats: closes, aside: "Z→A, empties last" });
      });
      return { items: out, stage: "dsl-sort", where: w };
    }
    var top = [];
    if ("columns".indexOf(fold(w.frag)) === 0)
      top.push({ text: 'columns = [ "…" ]', insert: 'columns = [""]', back: 2,
                 aside: "the chain, in written order" });
    if ("none".indexOf(fold(w.frag)) === 0)
      top.push({ text: "None", insert: "None", full: true, dim: true,
                 aside: "document order" });
    return { items: top, stage: "dsl-sort", where: w };
  }

  /** F's `.columns(…)': positional names, quoted, one slot at a time. */
  function dslColsOffers(args, at) {
    var w = dslWhere(args, at);
    var closes = slotCloses(args, at, w.frag);
    var bare = w.frag.replace(/^"/, "");
    var named = lexDsl(String(args)).filter(function (t) { return t.t === "str"; })
      .map(function (t) { return fold(t.v); });
    var out = COLS.filter(function (c) {
      return fold(c.head).indexOf(fold(bare)) === 0
        && (named.indexOf(fold(c.head)) < 0 || fold(c.head) === fold(bare));
    }).map(function (c) {
      return { text: JSON.stringify(c.head), insert: JSON.stringify(c.head),
               full: true, eats: closes, aside: "builtin" };
    });
    return { items: out, stage: "dsl-cols", where: w };
  }

  // ------------------------------------------------------------- the menu
  var M = { open: false, items: [], at: 0, stage: "" };

  function showMenu(items, stage) {
    M.items = items;
    M.stage = stage;
    M.at = Math.max(0, Math.min(M.at, items.length - 1));
    M.open = items.length > 0;
    drawMenu();
  }

  function closeMenu() { M.open = false; M.items = []; M.at = 0; drawMenu(); }

  function drawMenu() {
    var ac = el.ac;
    ac.textContent = "";
    if (!M.open) { ac.style.display = "none"; return; }
    ac.style.display = "";
    M.items.forEach(function (it, i) {
      var d = document.createElement("div");
      d.className = "tv-ac-item" + (i === M.at ? " tv-ac-on" : "");
      var t = document.createElement("span");
      if (it.dim) t.className = "tv-ac-dim";
      t.textContent = it.text;
      d.appendChild(t);
      var r = document.createElement("span");
      r.className = it.n === undefined ? "tv-ac-aside" : "tv-ac-n";
      r.textContent = it.n === undefined ? (it.aside || "") : String(it.n);
      d.appendChild(r);
      ac.appendChild(d);
    });
    var note = document.createElement("div");
    note.className = "tv-ac-note";
    note.textContent = NOTE[M.stage] || "";
    ac.appendChild(note);
  }

  function moveMenu(d) {
    if (!M.open) return;
    M.at = (M.at + d + M.items.length) % M.items.length;
    drawMenu();
  }

  // ------------------------------------------------- door one: the flat box
  // Today's control, and the `/' door in every variant: one text field over the
  // whole flat grammar, one dropdown, the shipped two-step ESC.

  var WHOLE_HINT = 'key:value · state:TODO|DONE · -word · "some phrase"';
  var NARROW_HINT = "filter rows · " + WHOLE_HINT;

  function boxFrag() {
    var v = el.input.value;
    var at = v.lastIndexOf(" ") + 1;
    return { head: v.slice(0, at), frag: v.slice(at) };
  }

  function boxOffer() {
    var f = boxFrag();
    if (!f.frag) { closeMenu(); return; }
    var o = filterOffers(f.frag, !S.narrow);
    showMenu(o.items, o.stage);
  }

  function boxAccept(it) {
    var f = boxFrag();
    el.input.value = f.head + it.insert + (it.full ? " " : "");
    el.input.setSelectionRange(el.input.value.length, el.input.value.length);
    boxOffer();
  }

  function openFilter(how) {
    S.narrow = !!(how && how.narrow === true);
    S.door = "filter";
    S.refused = "";
    el.input.placeholder = S.narrow ? NARROW_HINT : WHOLE_HINT;
    el.app.classList.add("tv-typing");
    el.cx.style.display = "none";
    el.input.style.display = "";
    el.input.value = "";
    el.input.focus();
    closeMenu();
    paint();
  }

  function closeDoor() {
    S.door = null;
    closeMenu();
    el.app.classList.remove("tv-typing");
    el.input.blur();
    el.cx.blur();
    el.stage.focus();
    paint();
  }

  /** A refusal names THE OTHER DOOR rather than the rule — the shell's words. */
  function refuse(text) {
    S.refused = text.replace(/^[-+]/, "").split(/[:=]/)[0]
      + ": autocomplete restricted, this key belongs to #'compose (kbd \".\")";
  }

  /** The box onto the strip, one token at a time, refusals left standing. */
  function boxCommit() {
    var v = el.input.value.trim();
    if (!v) { closeDoor(); return; }
    var left = [];
    scan(v).forEach(function (t) {
      if (S.narrow && t.shaping) { left.push(t.text); refuse(t.text); return; }
      chipUp(t.text);
    });
    el.input.value = left.join(" ");
    apply();
    if (el.input.value) { boxOffer(); return; }   // a refusal is the reader's to see
    closeDoor();
  }

  function boxKeys(e) {
    var k = e.key;
    if (e.ctrlKey || e.altKey || e.metaKey) return;
    if (M.open && (k === "ArrowDown" || k === "ArrowUp" || k === "Tab"
                   || k === "Enter" || k === "Escape")) {
      e.preventDefault(); e.stopPropagation();
      if (k === "ArrowDown") return moveMenu(1);
      if (k === "ArrowUp") return moveMenu(-1);
      if (k === "Escape") return closeMenu();
      var taken = M.items[M.at];
      boxAccept(taken);
      if (k === "Enter" && taken.full) closeMenu();
      return;
    }
    if (k === "Backspace" && !el.input.value) {
      // DEAD over a summoned box: the chips are on the page behind it.
      e.preventDefault(); e.stopPropagation();
      return;
    }
    if (k === "Tab") { e.preventDefault(); e.stopPropagation(); boxOffer(); return; }
    if (k !== "Enter" && k !== "Escape") return;
    e.preventDefault(); e.stopPropagation();
    if (k === "Escape") {
      // TWO STEPS: the typed text first, the box second.
      if (el.input.value) { el.input.value = ""; closeMenu(); paint(); return; }
      closeDoor();
      return;
    }
    boxCommit();
  }

  // ------------------------------------------ door two: the structured chain
  // `.' spawns a DOT and offers the three calls; a taken call opens its parens
  // and the caret lands inside them; `)' closes the stage and the next `.'
  // chains.  Inside the parens a dot is a CHARACTER — every argument may carry
  // one — so the chain operator lives outside them alone.

  var CX = { stages: [], where: "chain", buf: "" };

  var STAGE_OF = { filter: "filter", sort: "sort", columns: "columns" };

  function live() { return CX.stages[CX.stages.length - 1] || null; }

  /** What the caret is inside: the last argument, after the last separator. */
  var FILTER_SEP = /[\s,](?=[^\s,]*$)/;

  function cxFrag() {
    var st = live();
    if (!st) return "";
    var a = st.args.slice(0, caretAt(st));
    if (st.fn === "filter") return a.slice(fragAt(a, FILTER_SEP));
    return a;
  }

  // ------------------------------------------- F: the two signs, as helpers
  // THE SIGN IS A KEY, NOT A CHARACTER.  In the typed surface `-' and `+' are
  // not spellings at all — they are the gestures that reach for the two
  // spellings the surface DOES have: `/=' and a list.

  /** The argument list, split at its top-level commas. */
  function itemSpans(args) {
    var out = [], depth = 0, inq = false, start = 0;
    for (var i = 0; i < args.length; i += 1) {
      var c = args.charAt(i);
      if (c === '"') inq = !inq;
      else if (inq) continue;
      else if (c === "(" || c === "[") depth += 1;
      else if (c === ")" || c === "]") depth -= 1;
      else if (c === "," && depth === 0) { out.push({ start: start, end: i }); start = i + 1; }
    }
    out.push({ start: start, end: args.length });
    return out;
  }

  var spanAt = function (args, at) {
    return itemSpans(args).filter(function (s) { return at >= s.start && at <= s.end; })[0]
      || { start: 0, end: args.length };
  };

  /** The item's own `=' or `/=' — never one inside a string or a bracket. */
  function opTokenOf(text) {
    var lx = lexDsl(text), depth = 0;
    for (var i = 0; i < lx.length; i += 1) {
      var t = lx[i];
      if (t.v === "(" || t.v === "[") depth += 1;
      else if (t.v === ")" || t.v === "]") depth -= 1;
      else if (t.t === "op" && depth === 0) return t;
    }
    return null;
  }

  /** `-': the kwarg under the caret flips between `=' and `/='; on empty
   *  ground it spawns the wrapper the operator cannot carry, `not (|)'. */
  function dslNegate(st) {
    var at = caretAt(st), sp = spanAt(st.args, at);
    var text = st.args.slice(sp.start, sp.end);
    var op = opTokenOf(text);
    var put = function (s, caret) {
      st.args = st.args.slice(0, sp.start) + s + st.args.slice(sp.end);
      st.at = sp.start + caret;
    };
    if (op && op.v === "/=") {
      put(text.slice(0, op.at) + "=" + text.slice(op.end), Math.max(0, at - sp.start - 1));
      return;
    }
    if (op) {
      put(text.slice(0, op.at) + "/=" + text.slice(op.end), at - sp.start + 1);
      return;
    }
    if (!text.trim()) {                       // nothing to flip: the wrapper
      put(text + "not ()", text.length + 5);
      return;
    }
    put("not (" + text.trim() + ")", 5 + text.trim().length + 1);
  }

  /**
   * TYPING THE EQUALS OPENS THE SLOT, the way completing the field does: a
   * kwarg whose value is still nothing gets `= "|"' and the reader types the
   * string straight into it.  Nothing happens where a value already stands, or
   * inside a string, where an `=' is just a character.
   */
  function dslSlot(st) {
    var at = caretAt(st), s = st.args.slice(0, at), after = st.args.slice(at);
    var lx = lexDsl(s), end = lx[lx.length - 1];
    // The caret has to be sitting on an operator it just finished, with nothing
    // but a closer ahead of it — at any depth, `not (…)' included.
    if (!end || end.t !== "op" || end.end !== s.length) return;
    if (after && !/^\s*[,)\]]/.test(after)) return;
    st.args = s + ' ""' + after;
    st.at = at + 2;                                // between the quotes
  }

  /** Is the caret inside an unclosed `[' — a list, where the items are values? */
  function inBracket(s) {
    var depth = 0, inq = false;
    for (var i = 0; i < s.length; i += 1) {
      var c = s.charAt(i);
      if (c === '"') inq = !inq;
      else if (inq) continue;
      else if (c === "[") depth += 1;
      else if (c === "]") depth -= 1;
    }
    return depth > 0;
  }

  /**
   * THE COMMA OPENS THE NEXT SLOT wherever the next thing is a NAME rather than
   * a field: every argument of `.columns(…)', and every item of `.sort(…)''s
   * list.  In `.filter(…)' a comma starts a kwarg, whose field comes first, so
   * nothing is opened there.
   */
  function dslComma(st) {
    var at = caretAt(st), s = st.args.slice(0, at), after = st.args.slice(at);
    if (s.charAt(at - 1) !== ",") return;
    if (after && !/^\s*[,)\]]/.test(after)) return;
    if (!(st.fn === "columns" || (st.fn === "sort" && inBracket(s)))) return;
    st.args = s + ' ""' + after;
    st.at = at + 2;
  }

  /** Is the caret inside a string?  An odd number of quotes stands before it. */
  function inString(args, at) {
    var n = 0;
    for (var i = 0; i < at; i += 1) if (args.charAt(i) === '"') n += 1;
    return n % 2 === 1;
  }

  /** Is the caret inside an unclosed `(' of the arguments' own? */
  function inParen(args, at) {
    var depth = 0, inq = false;
    for (var i = 0; i < at; i += 1) {
      var c = args.charAt(i);
      if (c === '"') inq = !inq;
      else if (inq) continue;
      else if (c === "(") depth += 1;
      else if (c === ")") depth -= 1;
    }
    return depth > 0;
  }

  /**
   * IS THE TERM AT THE CARET FINISHED?  Round 11 read the dry law forwards: an
   * accept that leaves the caret INSIDE what it just wrote has finished nothing
   * and that new position's offers stand at once.  This is the same law read
   * backwards, and it is the TERM's completeness rather than any gesture's — a
   * closed string literal, a constructor that stands alone, a closed list or
   * wrapper is a whole VALUE, and a whole value ends the conversation.  Over
   * one the menu is down and `RET' applies the stage exactly as it does on
   * untouched ground.  Fresh ground, a half-typed name and a caret inside a
   * literal are all unfinished, and they keep their offers.
   *
   * The term is what decides and never the offset, so a trailing space carries
   * nothing either way.
   */
  function dslDone(args, at) {
    if (inString(args, at)) return false;    // the open world finishes nothing
    var s = String(args).slice(0, at);
    var lx = lexDsl(s), end = lx[lx.length - 1];
    if (!end || !/^\s*$/.test(s.slice(end.end))) return false;      // fresh ground
    if (end.t === "str") return true;                          // a closed literal
    if (end.v === "]" || end.v === ")") return true;   // a closed list or wrapper
    // A NAME IS WHOLE ONLY IF IT STANDS ALONE: `All' and `Desc' are waiting for
    // their argument, and a field is waiting for its `='.
    return end.t === "name" && NULLARY.indexOf(ctorOf(end.v)) >= 0;
  }

  /** `+': the value under the caret becomes a Haskell list with a fresh slot —
   *  which composes to the flat alternation, the widened axis itself. */
  function dslWiden(st) {
    var at = caretAt(st), sp = spanAt(st.args, at);
    var text = st.args.slice(sp.start, sp.end);
    var op = opTokenOf(text);
    if (!op) return;                          // no kwarg here: nothing to widen
    var head = text.slice(0, op.end), val = text.slice(op.end);
    var lead = /^\s*/.exec(val)[0], body = val.trim();
    var put = function (s, caret) {
      st.args = st.args.slice(0, sp.start) + head + lead + s + st.args.slice(sp.end);
      st.at = sp.start + head.length + lead.length + caret;
    };
    if (/^\[[\s\S]*]$/.test(body)) {
      var inner = body.slice(1, -1).replace(/\s*$/, "");
      put("[" + inner + ", ]", inner.length + 3);
      return;
    }
    put("[" + body + ", ]", body.length + 3);
  }

  function cxOffer() {
    var st = live();
    if (!st) { closeMenu(); return; }
    if (CX.where === "fn") {
      var roster = S.look.sql ? SQL_OFFERS : FN_OFFERS;
      showMenu(roster.filter(function (o) {
        return fold(o.text).indexOf(fold(CX.buf)) === 0;
      }), S.look.sql ? "sql-clause" : "fn");
      return;
    }
    if (CX.where !== "args") { closeMenu(); return; }
    if (S.look.sql) {
      // ROUND 15'S LAW, ASKED IN SQL'S QUOTING.  A finished term ends the
      // conversation here too; what differs is what FINISHES one, and that is
      // `sqlDone''s to say.
      if (sqlDone(st.fn, st.args, caretAt(st))) { st.span = null; closeMenu(); return; }
      var q = sqlOffers(st.fn, st.args, caretAt(st));
      st.span = q.where;
      showMenu(q.items, q.stage);
      return;
    }
    if (S.look.dsl) {
      // A COMPLETE TERM ENDS THE CONVERSATION.  The offers stand at fresh and
      // unfinished positions; over a finished one the menu is down, whichever
      // path asked — the quote stepped over, the caret walked back, a gesture,
      // a repaint.  One reading, one place: `dslDone'.
      if (dslDone(st.args, caretAt(st))) { st.span = null; closeMenu(); return; }
      // F asks in its own idiom, and asks the TEXT rather than a fragment: the
      // typed surface is nested, so where the caret is is a parse question.
      var d = st.fn === "filter" ? dslFilterOffers(st.args, caretAt(st))
            : st.fn === "sort" ? dslSortOffers(st.args, caretAt(st))
            : dslColsOffers(st.args, caretAt(st));
      st.span = d.where;
      showMenu(d.items, d.stage);
      return;
    }
    var frag = cxFrag();
    if (st.fn === "filter") {
      var o = filterOffers(frag, false);
      showMenu(o.items, "filter");
    } else if (st.fn === "sort") {
      showMenu(sortOffers(frag), "sort");
    } else {
      showMenu(colsOffers(frag), "columns");
    }
  }

  function cxAccept(it) {
    var st = live();
    // THE KEYWORD THAT ENDS A CLAUSE IS THE ONE THAT OPENS THE NEXT: SQL's own
    // rule, so the transition needs no punctuation of g's invention.
    if (it.clause && CX.where === "args") { takeClause(it.clause); return; }
    if (CX.where === "fn") {
      st.fn = S.look.sql ? SQL_STAGE[fold(it.text)] : STAGE_OF[it.text];
      CX.buf = "";
      CX.where = "args";
      // THE POSITIONAL SLOT OPENS WITH THE CALL: `.columns(` takes names and
      // nothing else, so its first argument is a quoted slot the way a kwarg's
      // value is.  `.filter(` and `.sort(` want a field name first.  G OPENS NO
      // SLOT ANYWHERE: its column names are BARE identifiers, which is what the
      // second pair of quotes bought it.
      if (S.look.dsl && !S.look.sql && st.fn === "columns") { st.args = '""'; st.at = 1; }
      cxOffer();
      return;
    }
    // DRY AND FINAL INSIDE THE PARENS: what is taken lands exactly as it is
    // spelled — no trailing space — the offers close, and the next one waits
    // for the next keystroke.  A separator or a `.' is what asks again.
    //
    // THE DRY LAW'S EDGE IS THE VALUE, NEVER THE POSITION.  An accept that
    // leaves the caret INSIDE what it just wrote — a field's opened slot, a
    // `not (|)', a list's first element — has not finished a term at all: it
    // has moved the reader to a NEW position, and that position's own offers
    // open at once.  `back' is exactly that fact, so it is what decides.
    var at = caretAt(st);
    if (S.look.dsl) {
      var from = st.span ? st.span.at : at;
      // `eats' takes the opened slot's closing quote with it; `back' walks the
      // caret into what was just written — inside the quotes, inside the parens.
      st.args = st.args.slice(0, from) + it.insert + st.args.slice(at + (it.eats || 0));
      st.at = from + it.insert.length - (it.back || 0);
      // the formatter's moment, in the dialect that owns the spelling
      st.args = S.look.sql ? sqlCanon(st.args, st.fn) : dslCanon(st.args, st.fn);
    } else if (st.fn === "filter") {
      var a = st.args, from2 = fragAt(a.slice(0, at), FILTER_SEP);
      st.args = a.slice(0, from2) + it.insert + a.slice(at);
      st.at = from2 + it.insert.length;
    } else {
      st.args = it.insert;
      st.at = st.args.length;
    }
    // MID-CONSTRUCTION: ASK AGAIN.  `back' is the caret left INSIDE what was
    // just written; `more' is g's other half of the same fact — a column or a
    // connective finishes no term, so the reader has been moved to a new
    // position and that position's own offers stand.
    if (it.back || it.more) { cxOffer(); return; }
    closeMenu();
  }

  function newStage() {
    CX.stages.push(bornOf({ fn: null, args: "", done: false, at: 0 }, "", false));
    CX.where = "fn";
    CX.buf = "";
    cxOffer();
    paint();
  }

  function closeStage() {
    var st = live();
    if (!st || CX.where !== "args") return;
    st.args = S.look.sql ? sqlCanon(sqlDangle(st.args.trim()), st.fn)
            : S.look.dsl ? dslCanon(dslDangle(st.args.trim()), st.fn)
            : st.args.trim();
    st.done = true;
    CX.where = "chain";
    if (S.look.pills) pend(st);
    closeMenu();
    paint();
  }

  /** G: THE KEYWORD CLOSES WHAT STANDS AND OPENS WHAT FOLLOWS, which is SQL's
   *  own rule — a clause ends where the next one begins and nowhere else. */
  function takeClause(fn) {
    var st = live();
    if (st && st.fn && CX.where === "args") closeStage();
    else if (st && !st.fn) CX.stages.pop();
    CX.stages.push(bornOf({ fn: fn, args: "", done: false, at: 0 }, "", false));
    CX.where = "args";
    cxOffer();
    paint();
  }

  /** D alone: a closed stage LEAVES the box and lands on the strip as a pill. */
  function pend(st) { st.pending = true; }

  /** WHAT THE EDIT FOUND, remembered the moment a stage opens for writing: the
   *  spelling it already had, and whether it was standing CLOSED rather than
   *  being written.  `cxCancel' is the only reader of it. */
  function bornOf(st, spelling, pending) {
    st.born = String(spelling);
    st.bornPending = !!pending;
    return st;
  }

  /**
   * F's `ESC': THE READER'S ESCAPE IS FROM THE EDIT, NEVER FROM THE MENU.  One
   * press abandons the open edit WHOLE and puts back what the edit found — the
   * spelling the stage was opened on, byte for byte, and the strip it was
   * opened over.  Everything the edit wrote goes together: the text typed into
   * it, the offers standing over it, and the comma a `/' summoned, which is the
   * edit's own writing and so wants no rule of its own.  A cancelled edit asked
   * nothing, so nothing it wrote reaches the table.
   */
  function cxCancel() {
    var st = CX.stages.pop();
    // A STAGE THE EDIT TOOK OFF THE STRIP GOES BACK ONTO IT, spelled the way it
    // stood: `reopen' and the chain's own backspace both pull a CLOSED stage
    // into the box, and the box is not where the edit found it.  One that was
    // already committed needs nothing — the chips hold it and the edit never
    // touched them.
    if (st && st.bornPending)
      CX.stages.push({ fn: st.fn, args: st.born, at: st.born.length,
                       done: true, pending: true, replacing: st.replacing });
    CX.where = "chain";
    closeMenu();
    if (!CX.stages.length) { closeDoor(); return; }   // nothing held: the strip
    // AN EDIT THE SUMMON INTERRUPTED IS WHERE THE READER GOES BACK TO.  `/' may
    // be pressed inside another stage's parens, and what the cancel found there
    // is a stage still being WRITTEN — not one standing closed — so the box
    // returns to it open, with that position's offers standing.
    var under = live();
    if (under && under.fn && !under.pending) { CX.where = "args"; cxOffer(); }
    paint();
  }

  function cxBack() {
    var st = live();
    if (CX.where === "chain") {
      if (!st) return;                       // dead: a summoned box takes nothing
      bornOf(st, st.args, !!st.pending);     // the backspace is an edit-open too
      st.done = false;
      st.pending = false;
      CX.where = "args";
      cxOffer();
      paint();
      return;
    }
    if (CX.where === "args") {
      var at = caretAt(st);
      if (st.args && at > 0) {
        st.args = st.args.slice(0, at - 1) + st.args.slice(at);
        st.at = at - 1;
        cxOffer(); paint(); return;
      }
      if (st.args) return;                   // at the head, with text ahead of it
      CX.where = "fn";                       // the parens go, the call comes back
      CX.buf = st.fn || "";
      st.fn = null;
      cxOffer();
      paint();
      return;
    }
    if (CX.buf) { CX.buf = CX.buf.slice(0, -1); cxOffer(); paint(); return; }
    CX.stages.pop();
    CX.where = "chain";
    if (!CX.stages.length) closeMenu();
    else cxOffer();
    paint();
  }

  /** Where the caret sits inside a stage's arguments; the end by default. */
  function caretAt(st) {
    if (!st) return 0;
    if (st.at === undefined || st.at === null || st.at > st.args.length) st.at = st.args.length;
    return st.at;
  }

  function cxType(ch) {
    var st = live();
    if (CX.where === "fn") { CX.buf += ch; cxOffer(); paint(); return; }
    if (CX.where === "args") {
      var at = caretAt(st);
      // THE SLOT ALREADY SPENT THE SPACE.  A reader typing `state = TODO"'
      // types the space they always would; the one the slot inserted is the
      // one that stands, so the first keystroke inside an EMPTY slot is not a
      // second one.  (A value that genuinely opens with a space wants `raw'.)
      var Q = S.look.sql ? "'" : '"';
      if (S.look.dsl && ch === " "
          && st.args.charAt(at - 1) === Q && st.args.charAt(at) === Q) return;
      st.args = st.args.slice(0, at) + ch + st.args.slice(at);
      st.at = at + ch.length;
      if (S.look.sql) {
        if (ch === "=" || ch === ">") sqlSlot(st);
        if (ch === ",") sqlComma(st);
        // THE CLAUSE ENDS AT THE NEXT KEYWORD, so the space that finishes one
        // is the gesture: `WHERE ' closes what stands and opens the narrowing.
        if (ch === " " && sqlSplit(st)) return;
      } else if (S.look.dsl) {
        if (ch === "=") dslSlot(st);
        if (ch === ",") dslComma(st);
      }
      cxOffer(); paint();
    }
  }

  /** Walk the caret through the arguments; the offers follow it. */
  function cxMove(d) {
    var st = live();
    if (!st || CX.where !== "args") return;
    st.at = Math.max(0, Math.min(caretAt(st) + d, st.args.length));
    cxOffer();
    paint();
  }

  /** The chain's own flat string — the ONE truth underneath, recomposed. */
  function composed() {
    return CX.stages.map(function (st) { return stageString(st.fn, st.args); })
      .filter(Boolean).join(" ");
  }

  function openCompose() {
    if (S.look.flat) { openFilter(); return; }   // A: the whole grammar, flat
    S.door = "compose";
    S.refused = "";
    S.narrow = false;
    CX.stages = [];
    CX.where = "chain";
    CX.buf = "";
    el.app.classList.add("tv-typing");
    el.input.style.display = "none";
    el.cx.style.display = "";
    el.cx.focus();
    newStage();                              // `.' spawns the dot AND the menu
  }

  /**
   * D's `/': THE FILTER DOOR AND THE FILTER STAGE ARE ONE THING.  A standing
   * `.filter(…)' pill reopens for editing, caret at the end of its contents,
   * and the commit rewrites THAT stage rather than adding a second one; with no
   * filter stage anywhere, `/' spawns a fresh `.filter(|)'.
   */
  function openFilterStage() {
    var standing = pillsOf(query()).filter(function (x) { return x.fn === "filter"; })[0];
    var pending = S.door === "compose" && CX.stages.some(function (st) { return st.fn === "filter"; });
    if (standing || pending) { reopen("filter"); return; }
    if (S.door !== "compose") openCompose();
    if (CX.stages.length && !live().fn) CX.stages.pop();
    CX.stages.push(bornOf({ fn: "filter", args: "", done: false }, "", false));
    CX.where = "args";
    cxOffer();
    paint();
  }

  /**
   * D's `DEL': THE CHAIN'S OWN BACKSPACE — stage-sized, last in first out.  One
   * press takes the latest badge whole, whichever call it is, and the earlier
   * ones stand; pressing it again walks the chain backward.  Inside an open
   * paren edit it is ordinary text editing and eats nothing.
   */
  function delLastStage() {
    // A stage closed but not yet committed IS the last badge while it stands.
    var pend = CX.stages.filter(function (st) {
      return st.pending && st.fn && String(st.args).trim();
    });
    if (pend.length) {
      var st = pend[pend.length - 1];
      CX.stages.splice(CX.stages.indexOf(st), 1);
      if (st.replacing) dropStage(st.fn);      // the pill it was rewriting goes too
      closeMenu();
      paint();
      return;
    }
    var fn = lastStage();
    if (!fn) return;                           // nothing to take is nothing done
    dropStage(fn);
    closeMenu();
    paint();
  }

  function cxCommit() {
    // A REOPENED STAGE REPLACES its tokens WHERE THEY STOOD, so the badge keeps
    // its place in the chain; a fresh stage joins the strip the way a committed
    // token always has.
    CX.stages.forEach(function (st) {
      if (!st.replacing) return;
      replaceStage(st.fn, scan(stageString(st.fn, st.args)).map(function (t) { return t.text; }));
    });
    CX.stages.filter(function (st) { return !st.replacing; }).forEach(function (st) {
      scan(stageString(st.fn, st.args)).forEach(function (t) { chipUp(t.text); });
    });
    CX.stages = [];
    CX.where = "chain";
    apply();
    closeDoor();
  }

  function cxKeys(e) {
    var k = e.key;
    if (e.ctrlKey || e.altKey || e.metaKey) return;
    // THE MENU DOES NOT ANSWER `ESC' IN THE TYPED SURFACE.  There the offers are
    // incidental to the input — they stand over a position rather than being
    // asked for — so the key falls through to the cancel below and takes them
    // with the edit.  D keeps the menu on the top rung of its own ladder.
    if (M.open && !(S.look.dsl && k === "Escape")
        && (k === "ArrowDown" || k === "ArrowUp" || k === "Tab"
            || k === "Enter" || k === "Escape")) {
      e.preventDefault(); e.stopPropagation();
      if (k === "ArrowDown") return moveMenu(1);
      if (k === "ArrowUp") return moveMenu(-1);
      if (k === "Escape") return closeMenu();
      var taken = M.items[M.at];
      cxAccept(taken);
      if (k === "Enter" && taken.full) closeMenu();
      paint();
      return;
    }
    if (k !== "Escape" && k !== "Enter" && k !== "Tab" && k !== "Backspace"
        && k !== "Delete" && k !== "ArrowLeft" && k !== "ArrowRight"
        && k.length !== 1) return;                    // a lone modifier is nobody's
    e.preventDefault(); e.stopPropagation();
    if (k === "ArrowLeft") { cxMove(-1); return; }
    if (k === "ArrowRight") { cxMove(1); return; }
    // THE TWO SIGNS ARE HELPERS IN THE TYPED SURFACE, where neither is a
    // spelling: `-' reaches for `/=' and `+' for a list.  IN G THEY ARE
    // CHARACTERS AGAIN — `CURRENT_DATE + INTERVAL '30' DAY' needs the sign to
    // type, so the shift is what takes the two keys back.
    if (S.look.dsl && !S.look.sql && CX.where === "args" && live()
        && live().fn === "filter" && (k === "-" || k === "+")) {
      if (k === "-") dslNegate(live()); else dslWiden(live());
      cxOffer();
      paint();
      return;
    }
    if (k === "Delete") {
      // THE STAGE'S OWN ERASER, and only at the strip level: inside the parens
      // it is ordinary text editing, and there is nothing ahead of the caret.
      if (S.look.delDropsStage && CX.where === "chain") delLastStage();
      return;
    }
    // `/' IS THE FILTER STAGE'S EDIT KEY where the strip holds the chain.  In
    // the TYPED surface every open value is quoted, so the line is exact: a
    // slash inside a string is a character (`title = "a/b"'), and everywhere
    // else it is the add-a-condition gesture — even mid-stage, where it opens
    // one more fresh argument.  The flat dialect quotes nothing by default, so
    // there the key stays at the chain level alone.
    if (k === "/" && S.look.slashStage
        && (CX.where === "chain"
            || (S.look.dsl && CX.where === "args" && live()
                && !(S.look.sql ? sqlInString(live().args, caretAt(live()))
                                : inString(live().args, caretAt(live())))))) {
      openFilterStage();
      return;
    }
    if (k === "Escape") {
      // ESC CANCELS INPUT, and in the typed surface that is the WHOLE of what
      // it does: one press abandons the open edit entire, whether or not the
      // offers stand and whether or not anything was typed.  With no edit open
      // there is no input to cancel and the key takes the box, dropping what
      // was written but never asked for.
      if (S.look.dsl) {
        if (CX.where !== "chain") { cxCancel(); return; }
        CX.stages = [];
        closeMenu();
        closeDoor();
        return;
      }
      // D AND THE FLAT DIALECT KEEP THE GRADUATED LADDER, the shipped one with
      // the menu on top: the offers, the chain, the box.
      if (CX.stages.length) { CX.stages = []; CX.where = "chain"; closeMenu(); paint(); return; }
      closeDoor();
      return;
    }
    if (k === "Enter") { cxCommit(); return; }
    if (k === "Tab") { cxOffer(); paint(); return; }
    if (k === "Backspace") { cxBack(); return; }
    // `;' IS THE CLAUSE'S OWN CLOSE IN G, where `)' is F's.  SQL ends a
    // STATEMENT with it and a clause with nothing at all, so this is the one
    // key g spends on a gesture SQL does not have.
    if (S.look.sql && k === ";") { closeStage(); return; }
    if (k === ")" && !S.look.sql) {
      // AN UNCLOSED `(' OF THE ARGUMENTS' OWN takes the paren first — `not (…)'
      // has to be typable — and the STAGE closes only when nothing is open.
      var st2 = live();
      if (S.look.dsl && CX.where === "args" && st2 && inParen(st2.args, caretAt(st2))) {
        if (st2.args.charAt(caretAt(st2)) === ")") { st2.at = caretAt(st2) + 1; paint(); }
        else cxType(")");
        return;
      }
      closeStage();
      return;
    }
    // TYPING PAST THE CLOSING QUOTE MOVES ON rather than doubling it: the slot
    // was opened for the reader, so its far edge is theirs to step over.  Two
    // quote characters in g, and each steps over its own.
    if (S.look.dsl && CX.where === "args" && live()
        && (k === '"' || (S.look.sql && k === "'"))
        && live().args.charAt(caretAt(live())) === k) {
      // …AND THE SLOT ALREADY SPENT THE OPENING ONE.  A reader typing
      // `tag = 'web'' types the quote they always would; the one the slot
      // inserted is the one that stands, so inside an EMPTY slot the keystroke
      // is not a second quote and not a step over the first.  It is round 7's
      // cost line, asked of the character SQL opens a literal with.
      if (S.look.sql && live().args.charAt(caretAt(live()) - 1) === k) return;
      live().at = caretAt(live()) + 1;
      cxOffer();
      paint();
      return;
    }
    if (S.look.sql && k === ")" && CX.where === "args" && live()
        && live().args.charAt(caretAt(live())) === ")") {
      // THE LIST'S OWN CLOSER, stepped over rather than doubled — the same
      // courtesy the quote gets, since the offers opened it.
      live().at = caretAt(live()) + 1;
      cxOffer();
      paint();
      return;
    }
    // `(' TAKES THE CALL, the way an IDE does: `.filter(' typed straight
    // through lands in the parens without a TAB.
    if (k === "(" && !S.look.sql && CX.where === "fn" && M.open) {
      cxAccept(M.items[M.at]);
      paint();
      return;
    }
    if (k === ".") {
      // OUTSIDE THE PARENS ONLY: inside them a dot is a character.
      if (CX.where === "chain") newStage();
      else if (CX.where === "args") cxType(".");
      return;
    }
    cxType(k);
  }

  // -------------------------------------------------------------- the strip
  /** One token onto the strip; an opposite-signed twin ANNIHILATES with it. */
  function chipUp(text) {
    var t = term(text);
    if (t.sign) {
      var twin = (t.sign === "-" ? "+" : "-") + text.slice(1);
      var at = S.chips.indexOf(twin);
      if (at >= 0) { S.chips.splice(at, 1); return; }   // ¬v ∨ v — both go
    }
    if (S.chips.indexOf(text) < 0) S.chips.push(text);
  }

  function chipClass(tok) {
    var t = term(tok);
    if (t.key === "sort" && chainFor(tok).length) return " tv-chip-sort";
    if (t.key === "columns" && String(t.value).trim()) return " tv-chip-cols";
    return "";
  }

  var JOIN = { filter: " ", sort: "->", columns: "," };

  /** D's reading of the strip: the flat query GROUPED back into stages, in the
   *  order the stages were first written — so the badges read left to right the
   *  way the chain was typed, and the LAST one is the last thing said. */
  function pillsIn(q, dsl) {
    var order = [], group = {};
    scan(q).forEach(function (t) {
      var fn = stageOfToken(t);
      if (!group[fn]) { group[fn] = []; order.push(fn); }
      group[fn].push(t);
    });
    return order.map(function (fn) {
      var toks = group[fn];
      var flat = toks.map(function (t) { return fn === "filter" ? t.text : t.value; })
        .join(JOIN[fn]);
      // F AND G SHOW THE SAME GROUP IN THEIR OWN IDIOM — the badge is the
      // surface, the strip underneath is still the flat string.
      if (dsl === "sql")
        return { fn: fn, args: fn === "filter" ? sqlOfFilter(toks)
                 : fn === "sort" ? sqlOfOrder(flat) : sqlOfCols(flat) };
      if (!dsl) return { fn: fn, args: flat };
      return { fn: fn, args: fn === "filter" ? dslOfFilter(toks)
               : fn === "sort" ? dslOfSort(flat) : dslOfCols(flat) };
    });
  }

  function pillsOf(q) {
    return pillsIn(q, S.look.sql ? "sql" : !!S.look.dsl);
  }

  /** The whole flat query, said in F's surface — `raw "…"' where it must be. */
  function dslChainOf(q) {
    return pillsIn(q, true).map(function (p) {
      return "." + p.fn + "(" + p.args + ")";
    }).join("");
  }

  /** The stage the last token on the strip belongs to — the chain's last badge. */
  function lastStage() {
    var p = pillsOf(query());
    return p.length ? p[p.length - 1].fn : null;
  }

  function renderChips() {
    var strip = el.chips;
    strip.textContent = "";
    if (S.look.pills) {
      // A stage being REWRITTEN keeps its pill, marked: the query it wrote is
      // still the one on the table.
      var editing = CX.stages.filter(function (st) { return st.replacing; })
        .map(function (st) { return st.fn; });
      pillsOf(query()).forEach(function (p, i) {
        strip.appendChild(pillEl(p, i, editing.indexOf(p.fn) >= 0 ? "cx-editing" : ""));
      });
      CX.stages.filter(function (st) { return st.pending && st.fn && st.args; })
        .forEach(function (st, i) { strip.appendChild(pillEl(st, i, "cx-pending")); });
    } else {
      S.chips.forEach(function (tok, i) {
        var c = document.createElement("span");
        c.className = "tv-chip" + chipClass(tok);
        c.dataset.at = String(i);
        c.appendChild(document.createTextNode(tok));
        var x = document.createElement("i");
        x.className = "tv-chip-x";
        x.textContent = "×";
        c.appendChild(x);
        strip.appendChild(c);
      });
    }
    var pin = document.createElement("span");
    pin.className = "tv-pin";
    pin.textContent = "📌";
    strip.appendChild(pin);
  }

  /** MARK says what is true of the badge: nothing, `cx-pending' for a stage
   *  closed but not yet asked for, `cx-editing' for one open in the box. */
  function pillEl(p, i, mark) {
    var c = document.createElement("span");
    // A BADGE THE WARNING IS ABOUT SAYS SO ON ITSELF: the collapse shows the
    // first argument and a count, so the binding the warning names may be
    // inside a badge and out of sight, and the reader still has to find it.
    var warn = warnSpans(p.args, p.fn).length ? " cx-warn" : "";
    c.className = "tv-chip cx-pill cx-pill-" + p.fn + (mark ? " " + mark : "") + warn;
    c.dataset.fn = p.fn;
    var a = document.createElement("span");
    a.className = "cx-args";
    a.appendChild(argsFrag(p.args, p.fn, true));
    if (S.look.sql) {
      // G'S BADGE IS A CLAUSE: the keyword is the head, and there are no parens
      // to draw because SQL has none — a clause ends where the next begins.
      c.title = SQL_CLAUSE[p.fn] + " " + p.args;
      var kw = document.createElement("b");
      kw.className = "cx-head";
      kw.textContent = SQL_CLAUSE[p.fn];
      c.appendChild(kw);
      c.appendChild(document.createTextNode(" "));
      c.appendChild(a);
      return c;
    }
    c.title = "." + p.fn + "(" + p.args + ")";
    var dot = document.createElement("b");
    dot.className = "cx-dot";
    dot.textContent = ".";
    var fn = document.createElement("b");
    fn.className = "cx-fn";
    fn.textContent = p.fn;
    var o = document.createElement("b");
    o.className = "cx-par";
    o.textContent = "(";
    var s = document.createElement("b");
    s.className = "cx-par";
    s.textContent = ")";
    [dot, fn, o, a, s].forEach(function (n) { c.appendChild(n); });
    return c;
  }

  // ---------------------------------------------------------- the chain draw
  var SPLIT = { filter: /\s+/, sort: "->", columns: "," };
  var SPLIT_SQL = { filter: /\s+AND\s+/i, sort: ",", columns: ",", from: /\s+/ };

  function span(text, cls) {
    var s = document.createElement("span");
    s.className = cls;
    s.textContent = text;
    return s;
  }

  /** The argument list, painted the way an editor paints a call's arguments. */
  function paintFilter(frag, args) {
    args.split(/(\s+)/).forEach(function (piece) {
      if (!piece) return;
      if (/^\s+$/.test(piece)) { frag.appendChild(document.createTextNode(piece)); return; }
      var sign = /^[-+]/.test(piece) ? piece.charAt(0) : "";
      if (sign) frag.appendChild(span(sign, "cx-sign cx-sign-" + (sign === "+" ? "add" : "neg")));
      var body = sign ? piece.slice(1) : piece;
      var m = /^([a-z]+)([:=])([\s\S]*)$/.exec(body);
      if (m && NARROW_KEYS.indexOf(m[1]) >= 0) {
        frag.appendChild(span(m[1], "cx-key"));
        frag.appendChild(span(m[2], "cx-punc"));
        if (m[3]) frag.appendChild(span(m[3], /^\*.*\*$/.test(m[3]) ? "cx-meta" : "cx-val"));
      } else {
        frag.appendChild(span(body, "cx-text"));
      }
    });
  }

  function paintSort(frag, args) {
    args.split(/(->)/).forEach(function (p) {
      if (!p) return;
      if (p === "->") { frag.appendChild(span(p, "cx-arrow")); return; }
      var bits = p.split(":");
      frag.appendChild(span(bits[0], /^\*.*\*$/.test(bits[0]) ? "cx-meta" : "cx-key"));
      if (bits.length > 1) {
        frag.appendChild(span(":", "cx-punc"));
        frag.appendChild(span(bits[1], "cx-val"));
      }
    });
  }

  function paintCols(frag, args) {
    args.split(/(,)/).forEach(function (p) {
      if (p) frag.appendChild(span(p, p === "," ? "cx-punc" : "cx-col"));
    });
  }

  /** Is this the name of a column the sort chain may carry? */
  function sortName(v) {
    var k = colKeyOf(v);
    return k && SORTABLE.indexOf(k) >= 0 ? k : null;
  }

  /** Which field names a stage takes: the twelve keys, or the one kwarg the
   *  shaping stages have.  `.columns(…)' has none — it is positional. */
  var STAGE_FIELDS = { filter: FIELDS, sort: ["columns"], columns: [] };

  /** F's own painter: fields, constructors, literals, and the two operators.
   *  `Desc'/`Asc' are gone from the roster — the direction rides inside the
   *  name's string now — so a capital that is not a meta or a wrapper is bad. */
  function paintDsl(frag, text, fn, warn) {
    var lx = lexDsl(text), i = 0;
    var fields = STAGE_FIELDS[fn] || FIELDS;
    lx.forEach(function (t) {
      if (t.at > i) frag.appendChild(document.createTextNode(text.slice(i, t.at)));
      var cls = t.t === "str"
        // A SORT SEGMENT NAMES ONE OF THE SIX; `.columns(…)' takes any name at
        // all, a custom column being whatever the drawer holds.
        ? (fn !== "sort" || sortName(t.v) ? "cx-str"
           : COLS.some(function (c) { return fold(c.head).indexOf(fold(t.v)) === 0; })
             ? "cx-partial-name" : "cx-bad")
        : t.t === "op" ? (t.v === "/=" ? "cx-neg" : "cx-eq")
        : t.t === "punc" ? "cx-punc"
        : ctorOf(t.v) ? "cx-ctor"
        : wordOf(t.v) ? "cx-op"
        : fields.indexOf(fold(t.v)) >= 0 ? "cx-kw"
        // A WORD HALF-TYPED IS NOT YET WRONG; one that can never finish is.
        : partialName(t.v, fn) ? "cx-partial-name" : "cx-bad";
      // A CONTRADICTION IS NO ERROR: both bindings are legal and the query is
      // still asked, so the pair wears its own ink over the syntax colouring
      // rather than the refusal's — what is wrong is the two of them TOGETHER.
      var inside = function (sp) { return t.at >= sp.start && t.end <= sp.end; };
      if (warn && warn.some(inside)) cls += " cx-warn";
      frag.appendChild(span(text.slice(t.at, t.end), cls));
      i = t.end;
    });
    if (i < text.length) frag.appendChild(document.createTextNode(text.slice(i)));
  }

  /** The paint's own reading of what no row can answer; see `unsatisfied'. */
  var WARN = { said: [], tokens: [] };
  /** …and of what has no flat spelling at all; see `parseSqlWhere'. */
  var REFUSED = [];

  /**
   * The character spans of the bindings THIS text writes that no row can
   * answer: one item at a time, composed on its own and looked up in the
   * paint's reading — so a binding is marked wherever it shows, in the live
   * stage's parens or in the badge on the strip, and BOTH sides of a
   * contradiction are marked wherever each of them stands.
   */
  function warnSpans(args, fn) {
    if (!WARN.tokens.length || fn !== "filter") return [];
    // PER DIALECT THE ITEM IS ITS OWN: a comma separates a call's arguments and
    // `AND' separates a predicate list, so the reading is the same and the
    // splitter is not.
    return (S.look.sql ? sqlItemSpans : itemSpans)(String(args)).filter(function (sp) {
      var one = String(args).slice(sp.start, sp.end);
      if (!one.trim()) return false;
      return scan(stageString(fn, one)).some(function (t) {
        return WARN.tokens.indexOf(t.text) >= 0;
      });
    });
  }

  /** A DONE stage collapses to its first argument plus a dim count. */
  function argsFrag(args, fn, done) {
    var frag = document.createDocumentFragment();
    var spans = S.look.sql ? sqlItemSpans : itemSpans;
    var parts = S.look.dsl && fn === "filter"
      ? spans(String(args)).map(function (s) { return args.slice(s.start, s.end); })
        .filter(function (x) { return x.trim(); })
      : String(args).split(S.look.sql ? SPLIT_SQL[fn] : SPLIT[fn]).filter(Boolean);
    var cut = !!done && !!S.look.collapse && args.length > 24 && parts.length > 1;
    var shown = cut ? parts[0].trim() : args;
    if (S.look.sql) {
      paintSql(frag, shown, fn, warnSpans(shown, fn),
               fn === "filter" && parseSqlWhere(args).refused);
    } else if (S.look.dsl) {
      paintDsl(frag, shown, fn, warnSpans(shown, fn));
    } else if (S.look.syntax) {
      (fn === "sort" ? paintSort : fn === "columns" ? paintCols : paintFilter)(frag, shown);
    } else {
      frag.appendChild(document.createTextNode(shown));
    }
    if (cut) {
      var more = document.createElement("b");
      more.className = "cx-more";
      // "+2 more" is the IDE's own spelling and this grammar has taken the
      // sign, so the count rides an ellipsis instead.
      more.textContent = " …" + (parts.length - 1);
      frag.appendChild(more);
    }
    return frag;
  }

  function caretEl() {
    var i = document.createElement("i");
    i.className = "cx-caret";
    return i;
  }

  /** Put the caret at a CHARACTER offset inside a painted run, so colouring is
   *  computed over the whole text and the caret lands in the middle of it. */
  function insertCaretAt(host, offset) {
    var walk = document.createTreeWalker(host, NodeFilter.SHOW_TEXT, null);
    var seen = 0, node;
    while ((node = walk.nextNode())) {
      var len = node.nodeValue.length;
      if (seen + len >= offset) {
        var tail = node.splitText(offset - seen);
        tail.parentNode.insertBefore(caretEl(), tail);
        return;
      }
      seen += len;
    }
    host.appendChild(caretEl());
  }

  function renderChain() {
    var box = el.cx;
    box.textContent = "";
    if (!CX.stages.length) {
      var hint = document.createElement("span");
      hint.className = "cx-empty";
      hint.textContent = S.look.sql
        ? "press . to begin — SELECT | FROM | WHERE | ORDER BY"
        : "press . to begin — filter | sort | columns";
      box.appendChild(caretEl());
      box.appendChild(hint);
      return;
    }
    CX.stages.forEach(function (st, i) {
      if (S.look.pills && st.pending) return;      // it lives on the strip now
      var isLive = i === CX.stages.length - 1;
      var s = document.createElement("span");
      s.className = "cx-stage" + (st.done ? " cx-done" : " cx-live");
      s.dataset.i = String(i);
      if (!S.look.sql) {
        var dot = document.createElement("b");
        dot.className = "cx-dot";
        dot.textContent = ".";
        s.appendChild(dot);
      }
      var fn = document.createElement("b");
      fn.className = (S.look.sql ? "cx-head" : "cx-fn") + (st.fn ? "" : " cx-partial");
      fn.textContent = st.fn ? (S.look.sql ? SQL_CLAUSE[st.fn] : st.fn)
        : (isLive ? (S.look.sql ? CX.buf.toUpperCase() : CX.buf) : "");
      s.appendChild(fn);
      if (isLive && CX.where === "fn") s.appendChild(caretEl());
      if (st.fn) {
        var o = document.createElement("b");
        o.className = S.look.sql ? "cx-space" : "cx-par";
        o.textContent = S.look.sql ? " " : "(";
        s.appendChild(o);
        var a = document.createElement("span");
        a.className = "cx-args";
        a.appendChild(argsFrag(st.args, st.fn, st.done));
        s.appendChild(a);
        // INSIDE THE CONTENTS, at the offset the model holds — which is the end
        // until an arrow, a `-' or a `+' puts it somewhere else.
        if (isLive && CX.where === "args") insertCaretAt(a, caretAt(st));
        // THE GHOST: empty parens say what goes in them, in dim type, and go
        // the moment a character lands.
        if (S.look.ghost && isLive && CX.where === "args" && !st.args) {
          var g = document.createElement("span");
          g.className = "cx-ghost";
          g.textContent = (S.look.sql ? GHOST_SQL
                           : S.look.dsl ? GHOST_DSL : GHOST)[st.fn];
          s.appendChild(g);
        }
        if (!S.look.sql) {
          var c = document.createElement("b");
          c.className = "cx-par";
          c.textContent = ")";
          s.appendChild(c);
        }
      }
      box.appendChild(s);
      if (isLive && CX.where === "chain") box.appendChild(caretEl());
    });
  }

  var GHOST = { filter: "key:value …", sort: "column[:desc]…", columns: "Name,…" };
  var GHOST_DSL = { filter: "field = value, …", sort: 'columns = ["Name"]',
                    columns: '"Name", …' };
  var GHOST_SQL = { filter: "column = 'value' AND …", sort: "column [DESC], …",
                    columns: "* or a column list",
                    from: "a tag — or all" };

  // ---------------------------------------------------------------- painting
  function renderTable() {
    var a = served(query());
    var t = document.createElement("table");
    var thead = document.createElement("thead");
    var hr = document.createElement("tr");
    a.cols.forEach(function (c) {
      var th = document.createElement("th");
      th.textContent = c.head;
      hr.appendChild(th);
    });
    thead.appendChild(hr);
    t.appendChild(thead);
    var tb = document.createElement("tbody");
    S.at = Math.max(0, Math.min(S.at, a.rows.length - 1));
    a.rows.forEach(function (r, i) {
      var tr = document.createElement("tr");
      if (i === S.at) tr.className = "tv-sel";
      a.cols.forEach(function (c) {
        var td = document.createElement("td");
        td.className = "c-" + c.key.replace(/[^a-z]/g, "");
        if (c.key === "state" && r.state && !ACTIVE[r.state]) td.className += " done";
        // A CUSTOM COLUMN READS THE DRAWER (or the planning stamp), which is
        // what makes the open half of the column namespace real rather than a
        // promise: `SELECT owner' draws what the row's `:OWNER:' pair holds.
        td.textContent = c.custom ? customCell(r, c.head)
          : String(cell(r, c.key === "tag" ? "tag" : c.key) || "");
        tr.appendChild(td);
      });
      tb.appendChild(tr);
    });
    t.appendChild(tb);
    el.scroll.textContent = "";
    if (!a.rows.length) {
      var none = document.createElement("div");
      none.className = "tv-empty";
      none.textContent = "no rows";
      el.scroll.appendChild(none);
    } else {
      el.scroll.appendChild(t);
    }
    return a;
  }

  function orderNote(q) {
    var chain = chainFor(q);
    if (!chain.length) return "document order";
    return chain.map(function (c) {
      return c[0] + (c[1] === "desc" ? " ↓" : " ↑");
    }).join(" → ");
  }

  function renderHint(a) {
    var h = el.hint;
    h.textContent = "";
    var add = function (cls, text) {
      var s = document.createElement("span");
      if (cls) s.className = cls;
      s.textContent = text;
      h.appendChild(s);
    };
    add("", a.rows.length + " of " + ROWS.length + " rows");
    add("", "order: " + orderNote(query()));
    add("", "columns: " + (scan(query()).some(function (t) { return t.key === "columns"; })
                             ? a.cols.map(function (c) { return c.head; }).join(",")
                             : "the six"));
    if (S.refused) add("tv-refused", S.refused);
    // THE FRAGMENT LAW, SPEAKING.  A refusal is not a warning: what it names
    // composes NOTHING, so the sentence wears the refusal's own red and says
    // which spelling the flat grammar has no room for.
    REFUSED.forEach(function (line) { add("tv-refused", line); });
    // THE COURTESY, NEVER THE REFUSAL: the query composed and the query
    // applied, and the empty table is the truthful answer — this says why.
    WARN.said.forEach(function (line) { add("tv-warn", line); });
  }

  /** The query as it WOULD be asked: the strip, less any stage being rewritten,
   *  plus what the chain has written. */
  function effectiveFlat() {
    var replacing = CX.stages.filter(function (st) { return st.replacing; })
      .map(function (st) { return st.fn; });
    var kept = S.chips.filter(function (tok) {
      return replacing.indexOf(stageOfToken(term(tok))) < 0;
    }).join(" ");
    return [kept, composed()].filter(Boolean).join(" ");
  }

  /** F: the whole query as F's own text — the standing badges plus the live
   *  stage, which is what the IR line below the box is READ FROM. */
  function chainSource() {
    var replacing = CX.stages.filter(function (st) { return st.replacing; })
      .map(function (st) { return st.fn; });
    var head = pillsIn(query(), true)
      .filter(function (p) { return replacing.indexOf(p.fn) < 0; })
      .map(function (p) { return "." + p.fn + "(" + p.args + ")"; }).join("");
    var tail = CX.stages.filter(function (st) { return st.fn && String(st.args).trim(); })
      .map(function (st) { return "." + st.fn + "(" + st.args + ")"; }).join("");
    return head + tail;
  }

  /**
   * G: THE WHOLE QUERY AS ONE STATEMENT — the standing badges plus the live
   * clause, in SQL'S OWN ORDER whatever order they were written.  This is the
   * sentence the clause badges cut into words, and the reason the pills cost
   * nothing: the reader sees it whole, live, under the box.
   */
  function sqlSource() {
    var replacing = CX.stages.filter(function (st) { return st.replacing; })
      .map(function (st) { return st.fn; });
    var said = {};
    pillsIn(query(), "sql").forEach(function (p) {
      if (replacing.indexOf(p.fn) < 0) said[p.fn] = p.args;
    });
    CX.stages.forEach(function (st) {
      if (st.fn && String(st.args).trim()) said[st.fn] = String(st.args).trim();
    });
    var out = "SELECT " + (said.columns || sqlSix())
      + " FROM " + (said.from || "all");
    if (said.filter) out += " WHERE " + said.filter;
    if (said.sort) out += " ORDER BY " + said.sort;
    return out;
  }

  /** E alone: the flat string the chain is writing, live, under the box. */
  function renderEcho() {
    if (!el.echo) return;
    var applied = query(), chain = composed();
    var whole = [applied, chain].filter(Boolean).join(" ");
    el.echo.textContent = "";
    var lab = function (t) {
      var b = document.createElement("b");
      b.textContent = t + " ";
      return b;
    };
    var flat = document.createElement("div");
    flat.appendChild(lab("flat"));
    // WHAT STANDS AND WHAT IS BEING WRITTEN, told apart: the strip's own
    // tokens, then the chain's.
    if (applied) flat.appendChild(span(applied, "e-applied"));
    if (applied && chain) flat.appendChild(document.createTextNode(" "));
    if (chain) flat.appendChild(span(chain, "e-chain"));
    if (!whole) flat.appendChild(span("(every row)", "e-applied"));
    // G ALONE: THE STATEMENT, WHOLE.  The badges are the entry and the sentence
    // is the view — which is the trade the clause-pills made, given back here.
    if (S.look.sql) {
      var stmt = document.createElement("div");
      stmt.appendChild(lab("sql"));
      stmt.appendChild(span(sqlSource(), "e-sql"));
      el.echo.appendChild(stmt);
    }
    el.echo.appendChild(flat);
    if (S.look.ir) {
      // THE NORMAL FORM, READ FROM THE TYPED SIDE while the table is served by
      // the flat one — so a divergence between the two readers shows here,
      // live, and `check.mjs' is the same comparison run over a corpus.
      var ir = document.createElement("div");
      ir.appendChild(lab("ir"));
      ir.appendChild(span(S.look.sql ? irSql(sqlSource()) : irDsl(chainSource()),
                          "e-ir"));
      el.echo.appendChild(ir);
      return;
    }
    var url = document.createElement("div");
    url.appendChild(lab("url"));
    // A bare `+' decodes to a space in a URL, so the sign travels as `%2B'.
    url.appendChild(span("?q=" + encodeURIComponent(whole), "eq"));
    el.echo.appendChild(url);
  }

  function truth() {
    if (!el.truth) return;
    var st = live();
    var named = st && st.fn ? (S.look.sql ? SQL_CLAUSE[st.fn] : st.fn) : "?";
    var where = S.door === null ? "the table"
      : S.door === "filter" ? "the flat box" + (S.narrow ? " (narrowed)" : " (whole)")
      : CX.where === "fn" ? (S.look.sql ? "choosing the clause" : "choosing the call")
      : CX.where === "args" ? (S.look.sql ? "inside " + named
                               : "inside " + named + "'s parens")
      : S.look.sql ? "on the statement" : "on the chain";
    el.truth.textContent = where + " · chain: " + (composed() || "—");
  }

  function paint() {
    // WHAT NO ROW CAN ANSWER, read ONCE PER PAINT over the query as it would be
    // asked — the strip plus what the box is writing — so the badge, the live
    // stage and the hint all speak from one reading.
    WARN = S.look.dsl ? unsatisfied(effectiveFlat()) : { said: [], tokens: [] };
    // AND WHAT THE FLAT STRING CANNOT CARRY, read the same way and once: over
    // every clause the box holds, since a refusal is the clause's whole answer.
    REFUSED = [];
    if (S.look.sql) {
      CX.stages.forEach(function (st) {
        if (st.fn !== "filter") return;
        parseSqlWhere(st.args).refusals.forEach(function (r) {
          if (REFUSED.indexOf(r) < 0) REFUSED.push(r);
        });
      });
    }
    renderChain();
    renderChips();
    var a = renderTable();
    renderHint(a);
    renderEcho();
    truth();
  }

  // ------------------------------------------------------------------ mount
  function pageKeys(e) {
    if (e.target === el.input || e.target === el.cx) return;
    if (e.ctrlKey || e.altKey || e.metaKey) return;
    var k = e.key;
    if (k === "/") {
      e.preventDefault();
      if (S.look.slashStage) openFilterStage(); else openFilter({ narrow: true });
      return;
    }
    if (k === "Delete") {
      if (!S.look.delDropsStage) return;
      e.preventDefault();
      delLastStage();
      return;
    }
    if (k === ".") { e.preventDefault(); openCompose(); return; }
    if (k === "t") { e.preventDefault(); theme(); return; }
    if (k === "n" || k === "ArrowDown") { e.preventDefault(); S.at += 1; paint(); return; }
    if (k === "p" || k === "ArrowUp") { e.preventDefault(); S.at -= 1; paint(); return; }
  }

  function theme() {
    document.documentElement.dataset.theme =
      document.documentElement.dataset.theme === "dark" ? "light" : "dark";
    paint();
  }

  function mount(opts) {
    S.look = (opts && opts.look) || {};
    el.stage = document.getElementById("stage");
    el.app = document.getElementById("app");
    el.echo = document.getElementById("echo");
    el.truth = document.getElementById("truth");
    if (S.look.klass) el.app.classList.add(S.look.klass);

    el.chips = document.createElement("div");
    el.chips.className = "tv-chips";
    var bar = document.createElement("div");
    bar.className = "tv-bar";
    var wrap = document.createElement("div");
    wrap.className = "tv-filter-wrap";
    el.input = document.createElement("input");
    el.input.className = "tv-filter";
    el.input.type = "text";
    el.input.spellcheck = false;
    el.input.setAttribute("autocomplete", "off");
    el.input.placeholder = WHOLE_HINT;
    el.cx = document.createElement("div");
    el.cx.className = "cx";
    el.cx.tabIndex = 0;
    el.cx.style.display = "none";
    el.ac = document.createElement("div");
    el.ac.className = "tv-ac";
    el.ac.style.display = "none";
    wrap.appendChild(el.input);
    wrap.appendChild(el.cx);
    wrap.appendChild(el.ac);
    bar.appendChild(wrap);
    el.scroll = document.createElement("div");
    el.scroll.className = "tv-scroll";
    el.hint = document.createElement("div");
    el.hint.className = "tv-hint";
    el.app.appendChild(el.chips);
    el.app.appendChild(bar);
    el.app.appendChild(el.scroll);
    el.app.appendChild(el.hint);

    el.input.addEventListener("keydown", boxKeys);
    el.input.addEventListener("input", boxOffer);
    el.cx.addEventListener("keydown", cxKeys);
    document.addEventListener("keydown", pageKeys);
    el.ac.addEventListener("mousedown", function (e) {
      var item = e.target.closest ? e.target.closest(".tv-ac-item") : null;
      if (!item) return;
      e.preventDefault();
      M.at = [].indexOf.call(el.ac.children, item);
      var taken = M.items[M.at];
      if (S.door === "compose") { cxAccept(taken); paint(); } else boxAccept(taken);
    });
    // D's pill: a click reopens that stage's parens.
    el.chips.addEventListener("mousedown", function (e) {
      var pill = e.target.closest ? e.target.closest(".cx-pill") : null;
      if (!pill || !S.look.pills) return;
      e.preventDefault();
      reopen(pill.dataset.fn);
    });

    scan(BOOT).forEach(function (t) { chipUp(t.text); });
    apply();
    el.stage.tabIndex = 0;
    el.stage.focus();
    paint();
    return S;
  }

  /** D: pull a stage back off the strip and into the box for editing.  THE
   *  QUERY STANDS while it is edited — the stage is REPLACED on commit, not
   *  taken away and put back, so the table under the box does not blank. */
  function reopen(fn) {
    var p = pillsOf(query()).filter(function (x) { return x.fn === fn; })[0];
    var pending = S.door === "compose"
      ? CX.stages.filter(function (st) { return st.fn === fn; })[0] : null;
    if (S.door !== "compose") openCompose();
    if (CX.stages.length && !live().fn) CX.stages.pop();
    var args = pending ? pending.args : p ? p.args : "";
    // WHAT THE EDIT FINDS, before the gesture writes into it: a stage taken off
    // the strip has to be spellable back exactly, and a PENDING one is only in
    // the box — the splice below is what makes remembering it necessary.
    var was = args, wasPending = !!(pending && pending.pending);
    if (pending) CX.stages.splice(CX.stages.indexOf(pending), 1);
    // `/' IS THE ADD-A-CONDITION GESTURE, so it lands on a FRESH ARGUMENT and
    // not at the tail of the last one: the comma is the gesture's own.  Editing
    // an argument already written is a cursor movement, which is a different
    // act.  An empty stage gets no comma — there is nothing to follow.
    // PER DIALECT THE GESTURE WRITES ITS OWN SEPARATOR, which is round 2's law
    // read at the gesture rather than at the compose: a comma joins a call's
    // arguments and `AND' joins a predicate list.
    if (S.look.sql && fn === "filter" && String(args).trim())
      args = sqlDangle(String(args)) + " AND ";
    else if (S.look.dsl && !S.look.sql && fn === "filter" && String(args).trim())
      args = String(args).replace(/,\s*$/, "") + ", ";
    // A STAGE ALREADY REWRITING A BADGE GOES ON REWRITING IT: reopening one
    // that is open carries the flag, or a second `/' would turn a rewrite into
    // an addition and the badge's tokens would land twice.
    CX.stages.push(bornOf({ fn: fn, args: args, done: false,
                            replacing: pending ? !!pending.replacing : !!p },
                          was, wasPending));
    CX.where = "args";
    // THE CARET-EDGE LAW'S OWN CONSEQUENCE: the reader was moved somewhere new,
    // so that position's offers stand at once.
    cxOffer();
    paint();
  }

  var stageOfToken = function (t) {
    return t.key === "sort" ? "sort" : t.key === "columns" ? "columns" : "filter";
  };

  /** Take a whole stage's tokens off the strip. */
  function dropStage(fn) {
    S.chips = S.chips.filter(function (tok) {
      return stageOfToken(term(tok)) !== fn;
    });
  }

  /** Swap a stage's tokens for new ones, IN THE PLACE the old ones held. */
  function replaceStage(fn, toks) {
    var at = -1, kept = [];
    S.chips.forEach(function (tok) {
      if (stageOfToken(term(tok)) === fn) { if (at < 0) at = kept.length; return; }
      kept.push(tok);
    });
    if (at < 0) at = kept.length;
    var fresh = toks.filter(function (t, i, a) { return a.indexOf(t) === i; });
    S.chips = kept.slice(0, at).concat(fresh, kept.slice(at));
  }

  function apply() { paint(); }

  // ---------------------------------------------------- what the check reads
  return {
    mount: mount, repaint: paint, theme: theme,
    query: query, composed: composed,
    chips: function () { return S.chips.slice(); },
    effective: function () {
      return [query(), composed()].filter(Boolean).join(" ");
    },
    door: function () { return S.door; },
    menu: function () {
      return { open: M.open, stage: M.stage, at: M.at,
               items: M.items.map(function (i) { return i.text; }) };
    },
    cx: function () {
      return { where: CX.where, buf: CX.buf,
               stages: CX.stages.map(function (s) {
                 return { fn: s.fn, args: s.args, done: !!s.done, pending: !!s.pending };
               }) };
    },
    rows: function () { return served(query()).rows.length; },
    cols: function () {
      return served(query()).cols.map(function (c) { return c.head; });
    },
    refused: function () { return S.refused; },
    openFilter: openFilter, openCompose: openCompose,
    pills: function () {
      return pillsOf(query()).map(function (p) { return p.fn + "(" + p.args + ")"; });
    },
    // THE PROOF: THREE readers, one normal form.  `irFlat' is the flat
    // grammar's path, `irDsl' F's typed one, `irSql' G's, and none of the three
    // goes through another.
    irFlat: irFlat, irDsl: irDsl, irSql: irSql, dslChainOf: dslChainOf,
    sqlStatementOf: sqlStatementOf, sqlSource: sqlSource, sqlCanon: sqlCanon,
    sqlDone: sqlDone,
    /** What the ink marks, and what the FRAGMENT LAW refuses — told apart,
     *  because one changes the compose and the other does not. */
    sqlErrors: function (fn, args) {
      if (fn === "filter") return parseSqlWhere(String(args)).bad;
      if (fn === "from") return parseSqlFrom(String(args))
        .filter(function (t) { return t.unknown; })
        .map(function (t) { return t.name + " is not a table"; });
      if (fn !== "sort") return [];
      return parseSqlOrder(String(args)).filter(function (g) { return g.unknown; })
        .map(function (g) { return g.col + " is not a column the chain can carry"; });
    },
    sqlRefusals: function (args) { return parseSqlWhere(String(args)).refusals; },
    refusedLines: function () { return REFUSED.slice(); },
    dslErrors: function (fn, args) {
      if (fn === "filter") return parseDslFilter(String(args)).bad;
      if (fn !== "sort") return [];
      return parseDslSort(String(args)).filter(function (g) { return g.unknown; })
        .map(function (g) { return g.col + " is not a column the chain can carry"; });
    },
    dslCanon: dslCanon,
    // WHAT NO ROW CAN ANSWER — the warning's own reading, over a flat query, so
    // a check can assert the LAW as well as the ink on the screen.
    unsat: unsatisfied,
    chainSource: chainSource, effectiveFlat: effectiveFlat,
    caret: function () {
      var st = live();
      return st ? { at: caretAt(st), len: st.args.length } : null;
    },
    // The grammar itself, so a check can assert against the law and not the DOM.
    scan: scan, chainFor: chainFor, served: served, stageString: stageString,
  };
})();
