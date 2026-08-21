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

  /** One token, read the way `scanQuery'/`resolve' read it. */
  function term(text) {
    var sign = "", s = text;
    if (s.charAt(0) === "-" || s.charAt(0) === "+") { sign = s.charAt(0); s = s.slice(1); }
    if (s.charAt(0) === '"') {
      var end = s.length > 1 && s.charAt(s.length - 1) === '"' ? s.length - 1 : s.length;
      return { text: text, sign: sign, key: "substring", value: s.slice(1, end) };
    }
    var m = /^([a-z]+)[:=]([\s\S]*)$/.exec(s);
    if (m && NARROW_KEYS.indexOf(m[1]) >= 0)
      return { text: text, sign: sign, key: m[1], value: m[2] };
    if (m && SHAPING_KEYS.indexOf(m[1]) >= 0)
      return { text: text, sign: sign, key: m[1], value: m[2], shaping: true };
    return { text: text, sign: sign, key: "substring", value: s };
  }

  /** Split on spaces, tabs, newlines and `&'; inside quotes they are literal. */
  function scan(q) {
    var out = [], i = 0, n = q.length;
    while (i < n) {
      while (i < n && /[\s&]/.test(q.charAt(i))) i += 1;
      if (i >= n) break;
      var start = i;
      if (q.charAt(i) === "-" || q.charAt(i) === "+") i += 1;
      if (q.charAt(i) === '"') {
        i += 1;
        while (i < n && q.charAt(i) !== '"') i += 1;
        if (i < n) i += 1;
      } else {
        while (i < n && !/[\s&]/.test(q.charAt(i))) i += 1;
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
  };

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
    var a = st.args;
    if (st.fn === "filter") return a.slice(fragAt(a, FILTER_SEP));
    return a;
  }

  function cxOffer() {
    var st = live();
    if (!st) { closeMenu(); return; }
    if (CX.where === "fn") {
      showMenu(FN_OFFERS.filter(function (o) { return o.text.indexOf(fold(CX.buf)) === 0; }), "fn");
      return;
    }
    if (CX.where !== "args") { closeMenu(); return; }
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
      cxOffer();
      return;
    }
    // DRY AND FINAL INSIDE THE PARENS: what is taken lands exactly as it is
    // spelled — no trailing space — the offers close, and the next one waits
    // for the next keystroke.  A separator or a `.' is what asks again.
    if (st.fn === "filter") {
      var a = st.args;
      st.args = a.slice(0, fragAt(a, FILTER_SEP)) + it.insert;
    } else {
      st.args = it.insert;
    }
    closeMenu();
  }

  function newStage() {
    CX.stages.push({ fn: null, args: "", done: false });
    CX.where = "fn";
    CX.buf = "";
    cxOffer();
    paint();
  }

  function closeStage() {
    var st = live();
    if (!st || CX.where !== "args") return;
    st.args = st.args.trim();
    st.done = true;
    CX.where = "chain";
    if (S.look.pills) pend(st);
    closeMenu();
    paint();
  }

  /** D alone: a closed stage LEAVES the box and lands on the strip as a pill. */
  function pend(st) { st.pending = true; }

  function cxBack() {
    var st = live();
    if (CX.where === "chain") {
      if (!st) return;                       // dead: a summoned box takes nothing
      st.done = false;
      st.pending = false;
      CX.where = "args";
      cxOffer();
      paint();
      return;
    }
    if (CX.where === "args") {
      if (st.args) { st.args = st.args.slice(0, -1); cxOffer(); paint(); return; }
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

  function cxType(ch) {
    var st = live();
    if (CX.where === "fn") { CX.buf += ch; cxOffer(); paint(); return; }
    if (CX.where === "args") { st.args += ch; cxOffer(); paint(); return; }
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
    CX.stages.push({ fn: "filter", args: "", done: false });
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
    if (M.open && (k === "ArrowDown" || k === "ArrowUp" || k === "Tab"
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
        && k !== "Delete" && k.length !== 1) return;   // a lone modifier is nobody's
    e.preventDefault(); e.stopPropagation();
    if (k === "Delete") {
      // THE STAGE'S OWN ERASER, and only at the strip level: inside the parens
      // it is ordinary text editing, and there is nothing ahead of the caret.
      if (S.look.delDropsStage && CX.where === "chain") delLastStage();
      return;
    }
    // `/' IS THE FILTER STAGE'S EDIT KEY where the strip holds the chain — but
    // inside the parens it is a character, values carrying slashes like any other.
    if (k === "/" && S.look.slashStage && CX.where === "chain") {
      openFilterStage();
      return;
    }
    if (k === "Escape") {
      // THREE RUNGS, the shipped ladder with the menu on top: the offers, the
      // chain, the box.
      if (CX.stages.length) { CX.stages = []; CX.where = "chain"; closeMenu(); paint(); return; }
      closeDoor();
      return;
    }
    if (k === "Enter") { cxCommit(); return; }
    if (k === "Tab") { cxOffer(); paint(); return; }
    if (k === "Backspace") { cxBack(); return; }
    if (k === ")") { closeStage(); return; }
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
  function pillsOf(q) {
    var order = [], group = {};
    scan(q).forEach(function (t) {
      var fn = stageOfToken(t);
      if (!group[fn]) { group[fn] = []; order.push(fn); }
      group[fn].push(fn === "filter" ? t.text : t.value);
    });
    return order.map(function (fn) {
      return { fn: fn, args: group[fn].join(JOIN[fn]) };
    });
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

  /** A DONE stage collapses to its first argument plus a dim count. */
  function argsFrag(args, fn, done) {
    var frag = document.createDocumentFragment();
    var parts = String(args).split(SPLIT[fn]).filter(Boolean);
    var cut = !!done && !!S.look.collapse && args.length > 24 && parts.length > 1;
    var shown = cut ? parts[0] : args;
    if (S.look.syntax) {
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
        if (isLive && CX.where === "args") s.appendChild(caretEl());
        // THE GHOST: empty parens say what goes in them, in dim type, and go
        // the moment a character lands.
        if (S.look.ghost && isLive && CX.where === "args" && !st.args) {
          var g = document.createElement("span");
          g.className = "cx-ghost";
          g.textContent = GHOST[st.fn];
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
    if (pending) CX.stages.splice(CX.stages.indexOf(pending), 1);
    CX.stages.push({ fn: fn, args: args, done: false, replacing: !pending && !!p });
    CX.where = "args";
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
    // The grammar itself, so a check can assert against the law and not the DOM.
    scan: scan, chainFor: chainFor, served: served, stageString: stageString,
  };
})();
