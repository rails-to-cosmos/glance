/* THE RIG every variant of this spike mounts.  The fixture, the filter and its
   tokeniser, the seeding rule, the draft row, the commit and dismiss laws, the
   sort and the measurements — all of it here, so a tab differs from its
   neighbour by ONE `look' object and nothing else.  Whatever two tabs disagree
   about is therefore exactly the thing the spike is asking about.

   Nothing in here is production code.  The fixture is invented; the dress, the
   tokeniser and the seeding rule are glance's own, transcribed with the source
   line beside each.  `file://', no build step, no modules. */
const RIG = (function () {
  "use strict";

  // ---- the fixture: six rows, as the server answered them ------------------
  // Already in the applied order (`sort:scheduled->title'): the dated rows
  // earliest first, then the undated by title.  A capture leaves SCHEDULED
  // empty, so a fresh row always sorts into the TAIL — which is the whole of
  // B's argument and costs the fixture nothing to show.
  const FIXTURE = [
    { id: "r1", state: "TODO", priority: "B", title: "Renew the passport",
      tag: "trip:admin", scheduled: "2026-09-15" },
    { id: "r2", state: "NEXT", priority: "A", title: "Book the Kyoto flights",
      tag: "trip:travel", scheduled: "2026-09-18" },
    { id: "r3", state: "NEXT", priority: "B", title: "Write the overnight report",
      tag: "glance", scheduled: "2026-09-24" },
    { id: "r4", state: "NEXT", priority: "A", title: "Pack the camera gear",
      tag: "trip:gear", scheduled: "" },
    { id: "r5", state: "DONE", priority: "", title: "Read the visa rules",
      tag: "trip", scheduled: "" },
    { id: "r6", state: "TODO", priority: "C", title: "Ship the fold marks",
      tag: "spike", scheduled: "" },
  ];

  /* THE FOUR FILTERS, and each is here to pin one clause of the seeding rule.
     ` cycles them.  THE ROW SET DOES NOT RE-FILTER — the fixture is the same
     six rows under all four, because what the spike is asking about is the
     DRAFT the filter lends to, never the rows it keeps. */
  const PRESETS = [
    { q: "priority:A tag:trip state:NEXT sort:scheduled->title",
      says: "three positive atoms, one of each: the draft wears all three" },
    { q: "tag:trip tag:gear priority:B sort:scheduled->title",
      says: "the FIRST positive tag names the layer; the rest ride as the draft's own tags" },
    { q: "state:NEXT -tag:work sort:scheduled->title",
      says: "a negated atom lends nothing, so the destination falls back to the inbox" },
    { q: "state:NEXT|TODO title:visa sort:scheduled->title",
      says: "an alternation pins no value and `title:' is no fact a row wears: nothing is lent" },
  ];

  // ---- the columns ---------------------------------------------------------
  // `cells.tag' is the org `:a:b:' cell and `tag:' is the query key for it
  // (`docs/query.md:53'), which is why the key is singular and the header plural.
  const COLS = [
    { key: "state", head: "State", w: 78 },
    { key: "priority", head: "#", w: 54 },
    { key: "title", head: "Title", w: 0 },
    { key: "tag", head: "Tags", w: 190 },
    { key: "scheduled", head: "Scheduled", w: 118 },
  ];
  const KEYS = ["state", "priority", "title", "tag", "scheduled", "deadline"];
  // THE CELLS A DRAFT OPENS, in the order C walks them on `RET' and E on `TAB'.
  // SCHEDULED is not among them: a capture leaves planning empty and the date
  // widget is its own door (`Keymap.hs:100'), which this spike does not re-argue.
  const WALK = ["title", "state", "priority", "tag"];

  // ---- the filter, tokenised the way the client already tokenises it -------
  /* A small faithful port of `TableView.parseQuery' (`assets/table-view.js:402'):
     `key:value' is a predicate only when KEY names a column, a quoted token is
     always free text, a leading `-' negates and a leading `+' widens.  The page
     holds no second copy of this in production — `00-core.js:261' calls the
     renderer's own — and it is ported here only because a `file://' page has no
     renderer to call. */
  function scan(q) {
    const out = [];
    let i = 0;
    while (i < q.length) {
      while (i < q.length && /\s/.test(q[i])) i += 1;
      if (i >= q.length) break;
      const start = i;
      let body = "", quoted = false;
      while (i < q.length && !/\s/.test(q[i])) {
        if (q[i] === '"') {
          quoted = true; i += 1;
          while (i < q.length && q[i] !== '"') { body += q[i]; i += 1; }
          i += 1;
        } else { body += q[i]; i += 1; }
      }
      out.push({ raw: q.slice(start, i), body, quoted });
    }
    return out;
  }
  function parseQuery(q, keys) {
    const known = new Set(keys);
    return scan(q).map((t) => {
      let body = t.body, negated = false, added = false;
      if (!t.quoted && body[0] === "-") { negated = true; body = body.slice(1); }
      else if (!t.quoted && body[0] === "+") { added = true; body = body.slice(1); }
      const at = t.quoted ? -1 : body.search(/[:=]/);
      const key = at > 0 ? body.slice(0, at) : null;
      const pred = key !== null && known.has(key);
      return { raw: t.raw, negated, added, quoted: t.quoted,
               key: pred ? key : null, value: pred ? body.slice(at + 1) : body };
    });
  }

  /* A FACT THE FILTER PINS TO ONE CONCRETE POSITIVE VALUE, verbatim from
     `30-capture.js:48': a negated atom, a widening, an alternation and a meta
     (`*active*') each describe a SET of rows rather than a value a row wears. */
  const pinned = (t) =>
    !t.negated && !t.added && t.value
    && !t.value.includes("|") && !/^\*.*\*$/.test(t.value);

  /** WHAT THE STANDING FILTER LENDS A DRAFT.  Named ONCE is the whole rule for
   * the scalar facts (`30-capture.js:58'): two `state:' atoms describe a union,
   * and a capture inherits only what the filter leaves no choice about.  Tags
   * are the exception the shipped code already makes — every positive `tag:'
   * counts, the FIRST being the destination and the rest the draft's own. */
  function seedFrom(q) {
    // `sort:' and `columns:' are modifiers rather than column keys, peeled off
    // the filter half before it is parsed (`docs/query.md:421').
    const half = q.split(/\s+/).filter((w) => !/^[-+]?(sort|columns):/.test(w)).join(" ");
    const terms = parseQuery(half, KEYS);
    const on = (k) => terms.filter((t) => t.key === k && pinned(t));
    const sole = (k) => (on(k).length === 1 ? String(on(k)[0].value) : "");
    const tags = on("tag").map((t) => t.value);
    const priority = sole("priority").replace(/^\[#(.)\]$/, "$1").toUpperCase();
    const seed = {
      dest: tags[0] || null,
      tags: tags.slice(1),
      state: sole("state").toUpperCase(),
      priority,
    };
    // Which token lent which fact, so the chip strip can say it.
    seed.lends = new Map();
    for (const t of terms) {
      if (!pinned(t)) continue;
      if (t.key === "tag") seed.lends.set(t, tags[0] === t.value ? "destination" : "tag");
      else if (t.key === "state" && sole("state")) seed.lends.set(t, "state");
      else if (t.key === "priority" && sole("priority")) seed.lends.set(t, "priority");
    }
    seed.terms = terms;
    return seed;
  }

  // ---- the sort the omnibox names -----------------------------------------
  // `sort:scheduled->title' (`docs/query.md:424'): the dated rows earliest
  // first, the undated behind them, title breaking every tie.
  const ordered = (rows) => rows.slice().sort((a, b) => {
    const ad = a.scheduled || "￿", bd = b.scheduled || "￿";
    if (ad !== bd) return ad < bd ? -1 : 1;
    return a.title.toLowerCase() < b.title.toLowerCase() ? -1 : 1;
  });

  // ---- the drawing ---------------------------------------------------------
  const el = (id) => document.getElementById(id);
  const part = (into, tag, cls, text) => {
    const n = document.createElement(tag);
    if (cls) n.className = cls;
    if (text !== undefined) n.textContent = text;
    into.appendChild(n);
    return n;
  };
  const STATE_INK = { TODO: "--g-todo", NEXT: "--g-todo", READING: "--g-todo",
                      DONE: "--g-done", READ: "--g-done" };
  const PRIO_INK = { A: "--g-prio-a", B: "--g-prio-b", C: "--g-prio-c" };

  function pill(into, text, token, ghost) {
    if (!text) return;
    const n = part(into, "span", "tv-pill" + (ghost ? " cx-ghost" : ""), text);
    n.style.setProperty("--tv-badge", "var(" + token + ")");
  }
  const tagText = (t) => (t ? ":" + t + ":" : "");

  return { mount: mount, parseQuery: parseQuery, seedFrom: seedFrom };

  // ==========================================================================
  function mount(opts) {
    const look = opts.look;
    let rows = FIXTURE.slice();
    let point = 0;              // the row index the cursor stands on
    let preset = 0;
    let seed = seedFrom(PRESETS[0].q);
    let draft = null;           // { cells, at, cell, warned, keys }
    let minted = 0;
    let told = "";              // the last thing the rig said
    let cost = "";              // the last commit's key count
    let dwell = 1500;           // how long the refusal stands before the row goes
    let warnTimer = 0;
    let chord = false;          // a `C-c' waiting for its second half

    const wrap = el("tablewrap");
    const strip = el("strip");

    // ---- draw ------------------------------------------------------------
    function draw() {
      drawBar();
      drawTable();
      drawStrip();
      drawFoot();
      requestAnimationFrame(mirror);
    }

    function drawBar() {
      el("filter").value = PRESETS[preset].q;
      const chips = el("chips");
      chips.textContent = "";
      for (const t of seed.terms) {
        const lent = seed.lends.get(t);
        const c = part(chips, "span",
          "tv-chip" + (lent ? " cx-lends" : pinned(t) ? "" : " cx-mute"), t.raw);
        if (lent) part(c, "span", "cx-lendmark", "→ " + lent);
      }
      part(chips, "span", "tv-chip cx-mute", "sort:scheduled->title");
    }

    function drawTable() {
      wrap.textContent = "";
      const table = part(wrap, "table", "tv-table");
      const cg = part(table, "colgroup");
      for (const c of COLS) {
        const col = part(cg, "col");
        if (c.w) col.style.width = c.w + "px";
      }
      const thead = part(table, "thead");
      const hr = part(thead, "tr");
      for (const c of COLS) part(hr, "th", null, c.head);
      const tb = part(table, "tbody");
      tb.id = "tbody";

      const list = shown();
      list.forEach((r, i) => {
        const tr = part(tb, "tr", rowClass(r, i));
        tr.dataset.id = r.id;
        if (r.draft) drawDraftCells(tr, r);
        else drawRowCells(tr, r);
      });
    }

    /** The rows as the strip shows them: the six the server answered, with the
     * draft SPLICED IN where the variant puts it.  D's draft is not in here at
     * all — it is a surface of its own above the table, which is its claim. */
    function shown() {
      if (!draft || look.at === "strip") return rows;
      const out = rows.slice();
      out.splice(draft.at, 0, { id: "draft", draft: true });
      return out;
    }
    const rowClass = (r, i) => [
      r.draft ? "cx-draft" : "",
      r.draft && draft && draft.warned ? "cx-refused" : "",
      r.fresh ? "cx-fresh" : "",
      !r.draft && i % 2 ? "tv-alt" : "",
      !r.draft && !draft && realIndex(r) === point ? "tv-sel" : "",
    ].filter(Boolean).join(" ");
    const realIndex = (r) => rows.indexOf(r);

    function drawRowCells(tr, r) {
      pill(part(tr, "td"), r.state, STATE_INK[r.state] || "--g-mute", false);
      pill(part(tr, "td"), r.priority ? "[#" + r.priority + "]" : "",
           PRIO_INK[r.priority] || "--g-mute", false);
      const td = part(tr, "td");
      part(td, "span", null, r.title);
      if (r.moved) part(td, "span", "cx-moved", r.moved);
      part(part(tr, "td"), "span", "tv-tag", tagText(r.tag));
      part(part(tr, "td"), "span", "tv-date", r.scheduled);
    }

    /* THE DRAFT ROW.  Every cell the filter seeded is drawn as a GHOST — mute
       ink under a dashed rule — and the cell being edited carries the table's
       own in-cell editor (`.tv-cell-edit', `table-view.js:1233') and nothing
       else.  The title cell also carries WHERE IT WILL LAND and the `draft'
       badge, because a row among six facts has to say it is not one. */
    function drawDraftCells(tr, _r) {
      const c = draft.cells;
      cell(tr, "state", () => pill(tr.lastChild, c.state,
                                   STATE_INK[c.state] || "--g-mute", !c.typed.state));
      cell(tr, "priority", () => pill(tr.lastChild,
        c.priority ? "[#" + c.priority + "]" : "",
        PRIO_INK[c.priority] || "--g-mute", !c.typed.priority));
      const td = part(tr, "td");
      const box = part(td, "div", "cx-titlecell");
      if (draft.cell === "title") editor(box, "title");
      else part(box, "span", c.title ? null : "cx-ghost", c.title || "a title");
      part(box, "span", "cx-where", "→ " + (c.dest || "inbox"));
      if (draft.warned) part(box, "span", "cx-refuse", "nothing to capture");
      else part(box, "span", "cx-badge", "draft");
      cell(tr, "tag", () => part(tr.lastChild, "span",
        "tv-tag" + (c.typed.tag ? "" : " cx-ghost"), tagText(c.tag) || "no tags"));
      cell(tr, "scheduled", () => {});
    }
    /** One draft cell: the editor where point stands, the drawn value elsewhere. */
    function cell(tr, key, drawn) {
      const td = part(tr, "td");
      if (draft.cell === key) editor(td, key);
      else drawn();
    }
    function editor(into, key) {
      const input = document.createElement("input");
      input.className = "tv-cell-edit";
      input.id = "cxin";
      input.value = draft.cells[key] || "";
      input.dataset.cell = key;
      into.appendChild(input);
      // THE INPUT TAKES ITS OWN KEYS and a driver's map never sees them —
      // `openCellEditor' already does exactly this (`table-view.js:3917'), and
      // it is the whole reason a draft row is not walkable: `n' in an open
      // title types an `n'.
      input.addEventListener("keydown", onEditorKey);
      setTimeout(() => { input.focus(); input.select(); }, 0);
    }

    /* D'S STRIP: the same cells on the same column grid, in a surface of its
       own between the omnibox and the table.  The grid is MIRRORED from the
       table's measured header, which is the cost this variant pays in build
       rather than in motion. */
    function drawStrip() {
      if (!strip) return;
      strip.textContent = "";
      if (!draft || look.at !== "strip") return;
      const t = part(strip, "table");
      const cg = part(t, "colgroup");
      for (const _c of COLS) part(cg, "col");
      drawDraftCells(part(part(t, "tbody"), "tr"), null);
    }
    /** The strip's columns set to the table's own measured widths, and the
     * misalignment that remains, printed. */
    function mirror() {
      if (!strip || !strip.firstChild) { el("align").textContent = ""; return; }
      const ths = [...wrap.querySelectorAll("thead th")];
      const cols = [...strip.querySelectorAll("colgroup col")];
      ths.forEach((th, i) => {
        if (cols[i]) cols[i].style.width = th.getBoundingClientRect().width + "px";
      });
      requestAnimationFrame(() => {
        const tds = [...strip.querySelectorAll("tbody td")];
        let worst = 0;
        ths.forEach((th, i) => {
          if (!tds[i]) return;
          worst = Math.max(worst, Math.abs(
            th.getBoundingClientRect().left - tds[i].getBoundingClientRect().left));
        });
        el("align").textContent = "strip off the grid by " + worst.toFixed(1) + "px";
      });
    }

    function drawFoot() {
      el("state").textContent = draft
        ? "draft · " + draft.cell + " · → " + (draft.cells.dest || "inbox")
        : minted ? minted + " captured" : "no draft";
      el("truth").textContent = [told, cost].filter(Boolean).join("  ·  ");
      el("says").textContent = PRESETS[preset].says;
    }
    const say = (s) => { told = s; };

    // ---- the laws ----------------------------------------------------------
    /** `+' OPENS A DRAFT ROW, seeded from the standing filter, with the title
     * cell open.  The reader supplies a title and nothing else. */
    function open() {
      if (draft) return;
      draft = {
        at: look.at === "point" ? point + 1 : 0,
        cell: "title",
        warned: false,
        keys: 1,                       // `+' itself
        cells: { title: "", state: seed.state, priority: seed.priority,
                 tag: [seed.dest, ...seed.tags].filter(Boolean).join(":"),
                 dest: seed.dest, typed: {} },
      };
      say("+ · a draft row · type a title");
      cost = "";
      draw();
    }
    /** `ESC' drops the draft and says nothing: no file was ever written, so
     * there is nothing to put back (`Keymap.hs:141', `keyboard-quit'). */
    function drop() {
      if (!draft) return;
      draft = null;
      clearTimeout(warnTimer);
      say("ESC · the draft is gone, silently");
      draw();
    }
    /** AN EMPTY TITLE REFUSES, in the shipped words (`20-sheet.js:1628'), and
     * the row goes with the refusal — a draft that cannot commit is not a row
     * the reader should have to dismiss twice. */
    function refuse() {
      draft.warned = true;
      say("RET on an empty title · nothing to capture");
      draw();
      clearTimeout(warnTimer);
      warnTimer = setTimeout(() => {
        if (draft && draft.warned) { draft = null; say("the draft was dismissed"); draw(); }
      }, dwell);
    }
    /** THE COMMIT: the draft becomes a real row through the one command that
     * mints a blob (`/command {"name":"capture"}', `Commands.hs:160'), with the
     * destination the first positive tag named and the rest riding as the
     * draft's own tags. */
    function commit() {
      const c = draft.cells;
      if (!String(c.title).trim()) { refuse(); return; }
      const keys = draft.keys;
      // THE STRIP HELD NO ROW POSITION, so its write has none to claim: the row
      // goes to the end and the order the server answers puts it where it goes.
      const at = look.at === "strip" ? rows.length : draft.at;
      const row = {
        id: "n" + (minted += 1), state: c.state, priority: c.priority,
        title: String(c.title).trim(), tag: c.tag, scheduled: "",
        fresh: true,
      };
      for (const r of rows) { r.fresh = false; r.moved = ""; }
      rows.splice(Math.min(at, rows.length), 0, row);
      point = rows.indexOf(row);
      draft = null;
      cost = keys + " keys for the jot";
      say("captured · " + (c.dest ? ":" + c.dest + ":" : "inbox.org")
          + " · " + args(row, c));
      // THE SORT IS THE SERVER'S.  A client cannot re-order a `sort:' view on
      // its own, so it either leaves the fresh row where it put it until the
      // next `/headlines' answers (`00-core.js:244', `paint'), or it asks for
      // the answer at once and watches the row move.
      if (look.settle === "now") settle();
      else {
        row.moved = "sorted on the next paint (r)";
        draw();
      }
    }
    /** A repaint: the order the server would answer, and how far the fresh row
     * had to travel to reach it. */
    function settle() {
      const before = rows.indexOf(rows.find((r) => r.fresh));
      rows = ordered(rows);
      const after = rows.indexOf(rows.find((r) => r.fresh));
      const fresh = rows[after];
      if (fresh) {
        const d = after - before;
        fresh.moved = look.at === "strip"
          ? "landed at row " + (after + 1) + " of " + rows.length + ", nothing moved for it"
          : d === 0 ? "landed where it was inserted"
          : "moved " + Math.abs(d) + " row" + (Math.abs(d) === 1 ? "" : "s")
            + " " + (d > 0 ? "down" : "up") + " (" + Math.abs(d) * rowPx() + "px)";
        point = after;
      }
      draw();
    }
    const rowPx = () => {
      const tr = wrap.querySelector("tbody tr");
      return tr ? Math.round(tr.getBoundingClientRect().height) : 0;
    };
    /** What the commit sends, the shape `commitCapture' builds
     * (`20-sheet.js:1630'): the title the reader typed and the facts the filter
     * lent, the destination tag riding apart as the capture's address. */
    const args = (row, c) => JSON.stringify(Object.assign(
      { title: row.title },
      c.dest ? { tag: c.dest } : {},
      row.state ? { state: row.state } : {},
      row.priority ? { priority: row.priority } : {},
      seed.tags.length ? { tags: seed.tags.join(",") } : {}));

    /** C'S WALK: `RET' closes the cell and takes point to the next editable
     * one, so the draft can be refined before it is written. */
    function step() {
      const i = WALK.indexOf(draft.cell);
      if (i === WALK.length - 1) { commit(); return; }
      if (draft.cell === "title" && !String(draft.cells.title).trim()) { refuse(); return; }
      draft.cell = WALK[i + 1];
      say("RET · " + draft.cell + " · RET again walks on, C-c C-c captures now");
      draw();
    }
    /** E'S WALK: `TAB' takes the OPEN EDITOR to the next editable cell of the
     * draft and `S-TAB' back, wrapping at both ends, and `RET' keeps its one
     * meaning wherever point stands.  The walk is a movement key's job, which
     * is what leaves the commit key free. */
    function hop(dir) {
      const i = WALK.indexOf(draft.cell);
      draft.cell = WALK[(i + dir + WALK.length) % WALK.length];
      say((dir > 0 ? "TAB" : "S-TAB") + " · " + draft.cell
          + " · RET captures from here · ESC drops the draft");
      draw();
    }

    // ---- keys --------------------------------------------------------------
    function onEditorKey(e) {
      // The input takes its own keys; the page's map does not see them.
      e.stopPropagation();
      if (draft) draft.keys += 1;
      const input = e.target;
      draft.cells[input.dataset.cell] = input.value;
      draft.cells.typed[input.dataset.cell] =
        input.dataset.cell === "title" ? true : input.value !== seedOf(input.dataset.cell);
      if (draft.warned && e.key.length === 1) {
        draft.warned = false; clearTimeout(warnTimer);
      }
      if (e.ctrlKey && e.key === "c") {
        e.preventDefault();
        if (chord) { chord = false; commit(); }
        else { chord = true; say("C-c —"); drawFoot();
               setTimeout(() => { chord = false; }, 1500); }
        return;
      }
      chord = false;
      // THE COLLISION, DRAWN.  In an open table cell `TAB' commits exactly as
      // `RET' does (`table-view.js:3918'), which is the reading every tab but E
      // keeps.  E spends the key on the walk INSIDE A DRAFT and nowhere else.
      if (e.key === "Tab") {
        e.preventDefault();
        if (look.tab) hop(e.shiftKey ? -1 : 1); else commit();
        return;
      }
      if (e.key === "Enter") {
        e.preventDefault();
        if (look.walk) step(); else commit();
      } else if (e.key === "Escape") { e.preventDefault(); drop(); }
    }
    const seedOf = (key) =>
      key === "state" ? seed.state
      : key === "priority" ? seed.priority
      : key === "tag" ? [seed.dest, ...seed.tags].filter(Boolean).join(":") : "";

    document.addEventListener("keydown", (e) => {
      if (e.defaultPrevented) return;
      const k = e.key;
      if (k === "+") { e.preventDefault(); open(); return; }
      if (draft) return;                       // every other key belongs to the editor
      if (k === "n" || k === "ArrowDown" || k === "j") {
        e.preventDefault(); point = Math.min(point + 1, rows.length - 1); draw(); return;
      }
      if (k === "p" || k === "ArrowUp" || k === "k") {
        e.preventDefault(); point = Math.max(point - 1, 0); draw(); return;
      }
      if (k === "r") { e.preventDefault(); settle(); say("a repaint: the server's order"); drawFoot(); return; }
      if (k === "`") {
        e.preventDefault();
        preset = (preset + 1) % PRESETS.length;
        seed = seedFrom(PRESETS[preset].q);
        say("the filter changed; the defaults follow it");
        draw();
        return;
      }
      if (k === "~") {
        e.preventDefault();
        const now = document.documentElement.getAttribute("data-theme");
        document.documentElement.setAttribute("data-theme", now === "dark" ? "light" : "dark");
      }
    });

    draw();
    // The hooks `shots.mjs' drives the pages through; no variant reads them.
    window.RIG_TEST = {
      dwell: (ms) => { dwell = ms; },
      open, drop, settle,
      state: () => ({
        draft: !!draft, cell: draft && draft.cell, keys: draft && draft.keys,
        cost, told, rows: rows.map((r) => r.title),
        point, minted, dest: draft ? draft.cells.dest : null,
      }),
    };
  }
})();
