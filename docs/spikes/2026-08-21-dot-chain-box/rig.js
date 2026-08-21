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
  var ROWS = [
    { state: "TODO", priority: "A", title: "Ship the dot chain",
      scheduled: "2026-08-24", deadline: "2026-08-28", tags: ":spike:web:" },
    { state: "NEXT", priority: "B", title: "Port table-view v1.2 notes",
      scheduled: "", deadline: "2026-08-22", tags: ":web:" },
    { state: "TODO", priority: "B", title: "Write the release notes",
      scheduled: "2026-09-01", deadline: "", tags: ":docs:" },
    { state: "DONE", priority: "", title: "Rename the query keys",
      scheduled: "2026-08-11", deadline: "2026-08-12", tags: ":docs:chore:" },
    { state: "", priority: "C", title: "Read the org-mode manual",
      scheduled: "", deadline: "", tags: ":read:" },
    { state: "CANCELLED", priority: "A", title: "Drop the ?order= parameter",
      scheduled: "", deadline: "2026-08-30", tags: ":web:chore:" },
  ];

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
      return row[key] !== "" && fold(row[key]).indexOf(w) === 0;
    }
    if (key === "tag") {
      if (w === "*empty*") return row.tags === "" || row.tags === "::";
      if (w === "*archive*") return fold(row.tags).indexOf(":archive:") >= 0;
      return fold(row.tags).indexOf(w) >= 0;
    }
    if (key === "planned") {
      var both = row.scheduled + " " + row.deadline;
      if (w === "*empty*") return row.scheduled === "" && row.deadline === "";
      return fold(row.scheduled).indexOf(w) === 0 || fold(row.deadline).indexOf(w) === 0
        || (w === "" && both.trim() !== "");
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

  /** Every key's own fold, applied to one atom — org's brackets read through. */
  function normAtom(key, v) {
    var s = String(v);
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

  /** The shape: `default' until a columns token names something. */
  function colsSpecOf(names) {
    var kept = names.filter(function (n) { return String(n).trim(); });
    if (!kept.length) return { kind: "default" };
    var out = [], seen = {};
    kept.forEach(function (n) {
      var c = COLS.filter(function (c2) {
        return fold(c2.key) === fold(n.trim()) || fold(c2.head) === fold(n.trim());
      })[0];
      var name = c ? c.head : n.trim();
      if (seen[fold(name)]) return;
      seen[fold(name)] = 1;
      out.push(name);
    });
    if (!seen.title) out.unshift("Title");
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
      showMenu(FN_OFFERS.filter(function (o) { return o.text.indexOf(fold(CX.buf)) === 0; }), "fn");
      return;
    }
    if (CX.where !== "args") { closeMenu(); return; }
    if (S.look.dsl) {
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
    if (CX.where === "fn") {
      st.fn = STAGE_OF[it.text];
      CX.buf = "";
      CX.where = "args";
      // THE POSITIONAL SLOT OPENS WITH THE CALL: `.columns(` takes names and
      // nothing else, so its first argument is a quoted slot the way a kwarg's
      // value is.  `.filter(` and `.sort(` want a field name first.
      if (S.look.dsl && st.fn === "columns") { st.args = '""'; st.at = 1; }
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
      st.args = dslCanon(st.args, st.fn);        // the formatter's moment
    } else if (st.fn === "filter") {
      var a = st.args, from2 = fragAt(a.slice(0, at), FILTER_SEP);
      st.args = a.slice(0, from2) + it.insert + a.slice(at);
      st.at = from2 + it.insert.length;
    } else {
      st.args = it.insert;
      st.at = st.args.length;
    }
    if (it.back) { cxOffer(); return; }          // mid-construction: ask again
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
    st.args = S.look.dsl ? dslCanon(dslDangle(st.args.trim()), st.fn) : st.args.trim();
    st.done = true;
    CX.where = "chain";
    if (S.look.pills) pend(st);
    closeMenu();
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
      if (S.look.dsl && ch === " "
          && st.args.charAt(at - 1) === '"' && st.args.charAt(at) === '"') return;
      st.args = st.args.slice(0, at) + ch + st.args.slice(at);
      st.at = at + ch.length;
      if (S.look.dsl && ch === "=") dslSlot(st);
      if (S.look.dsl && ch === ",") dslComma(st);
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
    // spelling: `-' reaches for `/=' and `+' for a list.
    if (S.look.dsl && CX.where === "args" && live() && live().fn === "filter"
        && (k === "-" || k === "+")) {
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
                && !inString(live().args, caretAt(live()))))) {
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
    if (k === ")") {
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
    // was opened for the reader, so its far edge is theirs to step over.
    if (S.look.dsl && k === '"' && CX.where === "args" && live()
        && live().args.charAt(caretAt(live())) === '"') {
      live().at = caretAt(live()) + 1;
      cxOffer();
      paint();
      return;
    }
    // `(' TAKES THE CALL, the way an IDE does: `.filter(' typed straight
    // through lands in the parens without a TAB.
    if (k === "(" && CX.where === "fn" && M.open) { cxAccept(M.items[M.at]); paint(); return; }
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
      // F SHOWS THE SAME GROUP IN ITS OWN IDIOM — the badge is the surface, the
      // strip underneath is still the flat string.
      if (!dsl) return { fn: fn, args: flat };
      return { fn: fn, args: fn === "filter" ? dslOfFilter(toks)
               : fn === "sort" ? dslOfSort(flat) : dslOfCols(flat) };
    });
  }

  function pillsOf(q) { return pillsIn(q, !!S.look.dsl); }

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
    c.className = "tv-chip cx-pill cx-pill-" + p.fn + (mark ? " " + mark : "");
    c.dataset.fn = p.fn;
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
    var a = document.createElement("span");
    a.className = "cx-args";
    a.appendChild(argsFrag(p.args, p.fn, true));
    var s = document.createElement("b");
    s.className = "cx-par";
    s.textContent = ")";
    [dot, fn, o, a, s].forEach(function (n) { c.appendChild(n); });
    return c;
  }

  // ---------------------------------------------------------- the chain draw
  var SPLIT = { filter: /\s+/, sort: "->", columns: "," };

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
  function paintDsl(frag, text, fn) {
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
      frag.appendChild(span(text.slice(t.at, t.end), cls));
      i = t.end;
    });
    if (i < text.length) frag.appendChild(document.createTextNode(text.slice(i)));
  }

  /** A DONE stage collapses to its first argument plus a dim count. */
  function argsFrag(args, fn, done) {
    var frag = document.createDocumentFragment();
    var parts = S.look.dsl && fn === "filter"
      ? itemSpans(String(args)).map(function (s) { return args.slice(s.start, s.end); })
        .filter(function (x) { return x.trim(); })
      : String(args).split(SPLIT[fn]).filter(Boolean);
    var cut = !!done && !!S.look.collapse && args.length > 24 && parts.length > 1;
    var shown = cut ? parts[0].trim() : args;
    if (S.look.dsl) {
      paintDsl(frag, shown, fn);
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
      hint.textContent = "press . to begin — filter | sort | columns";
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
      var dot = document.createElement("b");
      dot.className = "cx-dot";
      dot.textContent = ".";
      s.appendChild(dot);
      var fn = document.createElement("b");
      fn.className = "cx-fn" + (st.fn ? "" : " cx-partial");
      fn.textContent = st.fn || (isLive ? CX.buf : "");
      s.appendChild(fn);
      if (isLive && CX.where === "fn") s.appendChild(caretEl());
      if (st.fn) {
        var o = document.createElement("b");
        o.className = "cx-par";
        o.textContent = "(";
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
          g.textContent = (S.look.dsl ? GHOST_DSL : GHOST)[st.fn];
          s.appendChild(g);
        }
        var c = document.createElement("b");
        c.className = "cx-par";
        c.textContent = ")";
        s.appendChild(c);
      }
      box.appendChild(s);
      if (isLive && CX.where === "chain") box.appendChild(caretEl());
    });
  }

  var GHOST = { filter: "key:value …", sort: "column[:desc]…", columns: "Name,…" };
  var GHOST_DSL = { filter: "field = value, …", sort: 'columns = ["Name"]',
                    columns: '"Name", …' };

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
        td.textContent = c.custom ? "" : String(cell(r, c.key === "tag" ? "tag" : c.key) || "");
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
    el.echo.appendChild(flat);
    if (S.look.ir) {
      // THE NORMAL FORM, READ FROM THE TYPED SIDE while the table is served by
      // the flat one — so a divergence between the two readers shows here,
      // live, and `check.mjs' is the same comparison run over a corpus.
      var ir = document.createElement("div");
      ir.appendChild(lab("ir"));
      ir.appendChild(span(irDsl(chainSource()), "e-ir"));
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
    var where = S.door === null ? "the table"
      : S.door === "filter" ? "the flat box" + (S.narrow ? " (narrowed)" : " (whole)")
      : CX.where === "fn" ? "choosing the call"
      : CX.where === "args" ? "inside " + (st && st.fn ? st.fn : "?") + "'s parens"
      : "on the chain";
    el.truth.textContent = where + " · chain: " + (composed() || "—");
  }

  function paint() {
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
    if (S.look.dsl && fn === "filter" && String(args).trim())
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
    // THE PROOF: two readers, one normal form.  `irFlat' is the flat grammar's
    // path, `irDsl' F's typed one, and neither goes through the other.
    irFlat: irFlat, irDsl: irDsl, dslChainOf: dslChainOf,
    dslErrors: function (fn, args) {
      if (fn === "filter") return parseDslFilter(String(args)).bad;
      if (fn !== "sort") return [];
      return parseDslSort(String(args)).filter(function (g) { return g.unknown; })
        .map(function (g) { return g.col + " is not a column the chain can carry"; });
    },
    dslCanon: dslCanon,
    chainSource: chainSource, effectiveFlat: effectiveFlat,
    caret: function () {
      var st = live();
      return st ? { at: caretAt(st), len: st.args.length } : null;
    },
    // The grammar itself, so a check can assert against the law and not the DOM.
    scan: scan, chainFor: chainFor, served: served, stageString: stageString,
  };
})();
