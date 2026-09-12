/* THE RIG every variant of this spike mounts.  The fixture, the table, the
   column measure, the date widget's laws, the draft row and the measurements —
   all of it here, so a tab differs from its neighbour by ONE `look' field and
   nothing else.  Whatever two tabs disagree about is therefore exactly the
   thing the spike is asking about, which is WHERE THE GHOST GOES.

   The GRAMMAR is not here: `dates.js' carries it, ported whole out of
   `frontend/glue/20-sheet.js'.  A rig that reimplemented the parser would be
   measuring its own reimplementation.

   Nothing in here is production code.  The fixture is invented; the dress and
   the column measure are glance's own, transcribed with the source line beside
   each.  `file://', no build step, no modules. */
const RIG = (function () {
  "use strict";
  const D = DATES;

  // ---- the fixture: the capture-in-table spike's six rows, dated -----------
  // The same six, so the two spikes read as one table, with DEADLINE filled on
  // three of them: B needs a neighbour that HOLDS something and a neighbour
  // that holds nothing, and both have to be on screen at once.
  const FIXTURE = [
    { id: "r1", state: "TODO", priority: "B", title: "Renew the passport",
      tag: "trip:admin", scheduled: "2026-09-15", deadline: "2026-09-30" },
    { id: "r2", state: "NEXT", priority: "A", title: "Book the Kyoto flights",
      tag: "trip:travel", scheduled: "2026-09-18", deadline: "2026-10-02" },
    { id: "r3", state: "NEXT", priority: "B", title: "Write the overnight report",
      tag: "glance", scheduled: "2026-09-24", deadline: "" },
    { id: "r4", state: "NEXT", priority: "A", title: "Pack the camera gear",
      tag: "trip:gear", scheduled: "", deadline: "" },
    { id: "r5", state: "DONE", priority: "", title: "Read the visa rules",
      tag: "trip", scheduled: "", deadline: "2026-09-20" },
    { id: "r6", state: "TODO", priority: "C", title: "Ship the fold marks",
      tag: "spike", scheduled: "", deadline: "" },
  ];

  /* THE APPLIED QUERY, and it is chosen for the WIDTH it produces.  Under
     `sort:scheduled->deadline' BOTH date columns carry a header mark, so both
     measure `calc(13ch + 24px)' — ten characters of `isoStamp' plus three of
     `▲¹ ' plus the cell padding (`colWidths', table-view.js:2988).  That is the
     118px this spike is about, and a query that sorted on one of them would
     have left the other three characters narrower and the comparison muddy. */
  const QUERY = "tag:trip sort:scheduled->deadline";

  // The columns `viewColumns' declares, in its own order (`Query.hs:2492').
  const COLS = [
    { key: "state", head: "State", w: "78px", kind: "badge" },
    { key: "priority", head: "#", w: "54px", kind: "badge" },
    { key: "title", head: "Title", w: "", kind: "text" },
    { key: "scheduled", head: "Scheduled", w: "var(--cd-date-col)",
      kind: "date", mark: "▲¹" },
    { key: "deadline", head: "Deadline", w: "var(--cd-date-col)",
      kind: "date", mark: "▲²" },
    { key: "tag", head: "Tags", w: "190px", kind: "text" },
  ];
  const DATE_COLS = ["scheduled", "deadline"];
  /* THE DRAFT'S RING.  `DRAFT_CELLS' ships as `["title","state","priority","tag"]`
     with the reason beside it — *"A date is not among them, a row carrying no
     planning line"* (`35-draft.js:136') — and this spike's whole draft question
     is what happens when the two dates join it.  The ring follows the HEADER's
     order, which is `draftWalk''s own law (`35-draft.js:141'), so `tag' comes
     last and `TAB' from `tag' wraps to the title. */
  const WALK = ["title", "state", "priority", "scheduled", "deadline", "tag"];

  const el = (id) => document.getElementById(id);
  const part = (into, tag, cls, text) => {
    const n = document.createElement(tag);
    if (cls) n.className = cls;
    if (text !== undefined) n.textContent = text;
    into.appendChild(n);
    return n;
  };
  const STATE_INK = { TODO: "--g-todo", NEXT: "--g-todo",
                      DONE: "--g-done", WAITING: "--g-todo" };
  const PRIO_INK = { A: "--g-prio-a", B: "--g-prio-b", C: "--g-prio-c" };
  function pill(into, text, token, ghost) {
    if (!text) return;
    const n = part(into, "span", "tv-pill" + (ghost ? " cx-ghost" : ""), text);
    n.style.setProperty("--tv-badge", "var(" + token + ")");
  }
  const tagText = (t) => (t ? ":" + t + ":" : "");
  const trimmed = (s) => String(s == null ? "" : s).trim();

  return { mount: mount };

  // ==========================================================================
  function mount(opts) {
    const look = opts.look;
    let rows = FIXTURE.map((r) => Object.assign({}, r));
    let point = 0;                 // the row the cursor stands on
    let col = DATE_COLS[0];        // the cell the cursor stands on, within it
    let edit = null;               // { id, key, stood, phrase, caret, refused }
    let draft = null;              // { cells, cell, refused }
    let told = "";
    let wire = "";
    let chord = false;             // a `C-c' waiting for its second half

    /* ONE CLOCK READ, pinned at mount (docs/invariants.md, "One clock read per
       request").  `?day=' fixes it so a screenshot and a measurement are the
       same every run; with no `?day=' the reader's own day stands. */
    const TODAY = (function () {
      const said = new URLSearchParams(location.search).get("day");
      const m = said && /^(\d{4})-(\d{2})-(\d{2})$/.exec(said);
      return m ? { y: +m[1], m: +m[2], d: +m[3] } : D.dateNow();
    })();

    const wrap = el("tablewrap");

    // ---- draw --------------------------------------------------------------
    function draw() {
      drawBar();
      drawTable();
      drawFoot();
    }

    function drawBar() {
      el("filter").value = QUERY;
      const chips = el("chips");
      chips.textContent = "";
      for (const t of QUERY.split(/\s+/))
        part(chips, "span", "tv-chip" + (/^sort:/.test(t) ? " cx-lends" : ""), t);
    }

    function drawTable() {
      wrap.textContent = "";
      const table = part(wrap, "table", "tv-table tv-fill");
      const cg = part(table, "colgroup");
      for (const c of COLS) {
        const n = part(cg, "col");
        if (c.w) n.style.width = c.w;
      }
      const hr = part(part(table, "thead"), "tr");
      for (const c of COLS) {
        const th = part(hr, "th", c.kind === "badge" ? "tv-badge" : null);
        part(th, "span", null, c.head);
        if (c.mark) part(th, "span", "tv-arrow", " " + c.mark);
      }
      const tb = part(table, "tbody");
      shown().forEach((r, i) => {
        if (r.strip) { drawStrip(tb); return; }
        const tr = part(tb, "tr", rowClass(r, i));
        tr.dataset.id = r.id;
        if (r.draft) drawDraftCells(tr);
        else drawRowCells(tr, r);
      });
    }

    /** The rows as the strip shows them, with the draft spliced in at the head
     * (the capture-in-table spike's pick) and C's strip under the edited row. */
    function shown() {
      const out = rows.slice();
      if (draft) out.unshift({ id: "· draft", draft: true });
      if (edit && look.ghost === "strip") {
        const at = out.findIndex((r) => r.id === edit.id);
        if (at !== -1) out.splice(at + 1, 0, { id: "· strip", strip: true });
      }
      return out;
    }
    const rowClass = (r, i) => [
      r.draft ? "cx-draft" : "",
      r.draft && draft.refused ? "cx-refused" : "",
      !r.draft && i % 2 ? "tv-alt" : "",
      !r.draft && !draft && !edit && rows[point] === r ? "tv-sel" : "",
    ].filter(Boolean).join(" ");

    function drawRowCells(tr, r) {
      for (const c of COLS) {
        const td = part(tr, "td", c.kind === "date" ? "cd-date" : null);
        if (editingCell(r.id, c.key)) { drawEdit(td); continue; }
        if (lentTo(r.id, c.key)) { drawLent(td, r, c); continue; }
        drawValue(td, r, c, false);
        if (!draft && !edit && rows[point] === r && c.key === col)
          td.classList.add("cd-at");
      }
    }
    function drawValue(td, r, c, ghost) {
      if (c.kind === "badge") {
        pill(td, c.key === "priority" && r.priority ? "[#" + r.priority + "]"
                 : r[c.key],
             (c.key === "priority" ? PRIO_INK[r.priority] : STATE_INK[r.state])
               || "--g-mute", ghost);
        return;
      }
      if (c.key === "tag") {
        part(td, "span", "tv-tag" + (ghost ? " cx-ghost" : ""),
             tagText(r.tag) || (ghost ? "no tags" : ""));
        return;
      }
      part(td, "span", ghost ? "cx-ghost" : null, r[c.key] || "");
      if (c.key === "title" && r.moved) part(td, "span", "cx-moved", r.moved);
    }

    /* THE OPEN EDIT.  The field is the table's OWN in-cell editor
       (`.tv-cell-edit', `table-view.js:1255'), which already carries its own
       ground — it has to, the row at point being washed in the very token a
       text selection is painted in.  A's ghost rides beside it in the same
       cell; B's and C's do not. */
    function drawEdit(td) {
      const box = part(td, "div", "cd-cell" + (look.ghost === "cell" ? " cd-in-cell" : ""));
      const input = document.createElement("input");
      input.className = "tv-cell-edit";
      input.id = "cdin";
      input.value = edit.phrase;
      box.appendChild(input);
      if (look.ghost === "cell") drawGhost(box);
      input.addEventListener("keydown", onEditKey);
      input.addEventListener("input", () => {
        edit.phrase = input.value;
        edit.refused = "";
        repaintGhost();
      });
      setTimeout(() => {
        input.focus();
        if (edit.caret === "end") input.setSelectionRange(input.value.length, input.value.length);
        else input.select();          // THE OPENING VALUE COMES UP WHOLLY SELECTED
      }, 0);
    }
    /** B's borrowed cell: the ghost where the neighbour's value stood, and the
     * neighbour's own value stepped aside — dimmed, so the reader can see what
     * is being borrowed and what will come back. */
    function drawLent(td, r, c) {
      td.classList.add("cd-lent");
      const box = part(td, "div", "cd-cell");
      const said = ghostText();
      if (said.text) {
        part(box, "span", "dgh" + (said.bad ? " bad" : ""), said.text.replace(/^ /, ""));
        return;
      }
      const stood = c.key === "tag" ? tagText(r.tag) : r[c.key];
      part(box, "span", "cd-stood", stood || "");
    }
    /** A's ghost: right-aligned in the cell the reader is typing into. */
    function drawGhost(box) {
      const said = ghostText();
      const g = part(box, "span", "dgh" + (said.bad ? " bad" : ""), said.text);
      g.id = "cdghost";
    }
    /** C's strip: ONE LINE under the edited row, carrying the phrase and its
     * resolution — `18 aug → <2026-08-18 Tue>' — at the table's whole width. */
    function drawStrip(tb) {
      const tr = part(tb, "tr", "cd-strip");
      const td = part(tr, "td");
      td.colSpan = COLS.length;
      drawStripBody(td);
    }
    /** WHAT THE GHOST SAYS, straight out of the shipped reader: nothing over an
     * empty field, nothing over a term still being WRITTEN, `→ <stamp>' over one
     * that resolves, and the refusal's own short word over one the grammar
     * refuses (`dateGhost', 15-dates.js:313). */
    const ghostText = () => D.dateGhost(edit.phrase, TODAY);

    /** REDRAW THE GHOST AND NOTHING ELSE.  A full redraw would destroy the very
     * input the reader is typing into and take the caret with it, which is the
     * same surgical rule the draft's refusal note already lives by
     * (`clearRefusal', 35-draft.js:259). */
    function repaintGhost() {
      const said = ghostText();
      if (look.ghost === "cell") {
        const g = el("cdghost");
        if (g) { g.className = "dgh" + (said.bad ? " bad" : ""); g.textContent = said.text; }
      } else if (look.ghost === "neighbour") {
        const td = document.querySelector("td.cd-lent");
        if (td) {
          td.textContent = "";
          const r = rows.find((x) => x.id === edit.id) || draftRow();
          const c = COLS.find((x) => x.key === lentKey());
          drawLent(td, r, c);
          td.classList.add("cd-lent");
        }
      } else {
        const td = document.querySelector("tr.cd-strip td");
        if (td) { td.textContent = ""; drawStripBody(td); }
      }
      drawFoot();
    }
    function drawStripBody(td) {
      const said = ghostText();
      part(td, "span", "cd-said", edit.phrase || "");
      if (said.text) part(td, "span", said.bad ? "cd-bad" : "cd-stamp", said.text);
      else if (!edit.phrase)
        part(td, "span", "cd-arrow", edit.key + " · type a date · RET sets it"
             + " · empty clears it · ESC leaves");
    }

    function drawFoot() {
      el("state").textContent = edit
        ? "editing " + edit.key + " · " + JSON.stringify(edit.phrase)
        : draft ? "draft · " + draft.cell
        : "row " + (point + 1) + " · " + col;
      el("truth").textContent = told;
      el("wire").textContent = wire;
      el("align").textContent = metrics();
    }
    const say = (s) => { told = s; };

    // ---- WHICH CELL DRAWS WHAT --------------------------------------------
    const editingCell = (id, key) => !!edit && edit.id === id && edit.key === key;
    /** B'S NEIGHBOUR.  Editing SCHEDULED lends DEADLINE; editing DEADLINE lends
     * the cell AFTER it, which in `viewColumns`' own order is Tags.  A view
     * whose date column is LAST has no cell after it and must fall back to the
     * left neighbour — the cell the reader may be about to edit next, which is
     * the reason this placement is conditional and A's and C's are not. */
    function lentKey() {
      if (!edit || look.ghost !== "neighbour") return null;
      const at = COLS.findIndex((c) => c.key === edit.key);
      return at + 1 < COLS.length ? COLS[at + 1].key : COLS[at - 1].key;
    }
    const lentTo = (id, key) => !!edit && edit.id === id && lentKey() === key;

    // ---- the widget's laws -------------------------------------------------
    /** `RET' OVER A DATE CELL (or `C-c C-s' / `C-c C-d' over the row) OPENS THE
     * WIDGET ON THE CELL'S OWN VALUE, WHOLLY SELECTED — the 2026-08-23 spike's
     * round 3, so one keystroke replaces the whole of it and `RET' with none
     * recommits it byte for byte.  The value it opens on is the cell's own
     * spelling, `isoStamp''s `2026-09-15' (`Query.hs:1342'), which is a form the
     * grammar reads back unchanged. */
    function openDate(id, key) {
      if (edit) return;
      const r = rows.find((x) => x.id === id) || draftRow();
      if (!r) return;
      edit = { id, key, stood: r[key] || "", phrase: r[key] || "",
               caret: "whole", refused: "" };
      wire = "";
      say(key + " · RET sets it · empty clears it · ESC leaves"
          + " · S-<left>/S-<right> a day · S-<up>/S-<down> a week");
      draw();
    }
    /** `RET' COMMITS THE RESOLVED STAMP, never the characters that drew it, and
     * the CELL then shows the table's own spelling.  Two spellings, one value:
     * the wire carries org's `<2026-08-18 Tue>' (`set-planning''s date, already
     * rendered — `Commands.hs:115') and the cell draws `isoStamp''s
     * `2026-08-18', which is what every other row in the column wears. */
    function commitDate() {
      const phrase = trimmed(edit.phrase);
      if (!phrase) { clearDate(); return; }
      const r = D.readsDate(phrase, TODAY);
      // THE REFUSAL IS ABOVE THE SHUT and the cell STAYS OPEN: a wall the model
      // alone knew would land with nothing left to fix it in
      // (`pairRefused', 20-sheet.js:1079).
      if (!r.ok) {
        edit.refused = r.why;
        edit.caret = "end";
        say("RET refused · " + r.why);
        draw();
        return;
      }
      const row = rows.find((x) => x.id === edit.id) || draftRow();
      const key = edit.key;
      row[key] = D.isoDay(r.start);
      wire = draft
        ? 'capture {"planning": [["' + key.toUpperCase() + '", "' + r.stamp + '"]]}'
        : 'set-planning {"keyword": "' + key.toUpperCase()
          + '", "date": "' + r.stamp + '"}';
      say("set · the cell shows " + JSON.stringify(row[key])
          + " · the wire carries " + JSON.stringify(r.stamp));
      edit = null;
      draw();
    }
    /** EMPTY CLEARS IT, the shipped foot's own promise kept verbatim
     * (`planningHelp', Keymap.hs:157; `20-sheet.js:1006' sends a null date). */
    function clearDate() {
      const row = rows.find((x) => x.id === edit.id) || draftRow();
      row[edit.key] = "";
      wire = draft ? "" : 'set-planning {"keyword": "' + edit.key.toUpperCase()
        + '", "date": null}';
      say("cleared · the entry comes off");
      edit = null;
      draw();
    }
    /** `ESC' CANCELS THE INPUT WHOLE and the cell comes back byte for byte —
     * the spelling the edit FOUND, never the one it was given
     * (`keyboard-quit', Keymap.hs:141). */
    function cancelDate() {
      const row = rows.find((x) => x.id === edit.id) || draftRow();
      row[edit.key] = edit.stood;
      wire = "";
      say("ESC · " + JSON.stringify(edit.stood) + " is back, byte for byte");
      edit = null;
      draw();
    }
    /** THE SHIFTED ARROWS ADJUST THE VALUE, which is why they need no frame to
     * act on and survived the calendar's removal (`dateAdjust', 20-sheet.js:1021).
     * A day on the horizontal pair, a week on the vertical. */
    function stepDate(by) {
      const r = D.readsDate(trimmed(edit.phrase), TODAY);
      if (!r.ok || !r.start) { say("no date here to move"); return; }
      edit.phrase = D.dateStepped(r, D.addDays(r.start, by));
      edit.caret = "end";
      edit.refused = "";
      const f = el("cdin");
      if (f) {
        f.value = edit.phrase;
        f.setSelectionRange(f.value.length, f.value.length);
      }
      say((by > 0 ? "+" : "") + by + "d · " + edit.phrase);
      repaintGhost();
    }

    // ---- the draft row (carried from the capture-in-table spike) -----------
    const draftRow = () => (draft ? draft.cells : null);
    /** `+': A DRAFT ROW AT THE HEAD, seeded from the standing filter's one
     * pinned tag, with its title cell open (`openDraft', 35-draft.js:90). */
    function openDraft() {
      if (draft || edit) return;
      draft = {
        cell: "title", refused: "",
        cells: { id: "· draft", state: "NEXT", priority: "A", title: "",
                 tag: "trip", scheduled: "", deadline: "", typed: {} },
      };
      say("+ · a draft row · → trip · type a title");
      wire = "";
      draw();
    }
    function dropDraft() {
      draft = null; edit = null;
      say("ESC · the draft is gone, silently");
      draw();
    }
    /** THE WALK, one stop wider than the shipped ring: `TAB' from the tags cell
     * reaches SCHEDULED, and a date stop opens THE DATE WIDGET rather than a
     * plain box — same ghost, same laws, same keys. */
    function hopDraft(dir) {
      const i = WALK.indexOf(draft.cell);
      const next = WALK[(i + dir + WALK.length) % WALK.length];
      draft.cell = next;
      if (DATE_COLS.indexOf(next) !== -1) {
        edit = null;
        openDate("· draft", next);
      } else {
        edit = null;
        say((dir > 0 ? "TAB" : "S-TAB") + " · " + next
            + " · RET captures from here · ESC drops the draft");
        draw();
      }
    }
    /** `RET' OVER A DRAFT FINALIZES THE CAPTURE from whatever cell point stands
     * in (`commitDraft', 35-draft.js:226).  A DATE CELL HAS NO WRITE OF ITS
     * OWN — the capture carries the planning line — so the one press resolves
     * the phrase and captures; a phrase that does not read refuses in place,
     * exactly as an empty title does. */
    function commitDraft() {
      const c = draft.cells;
      if (edit) {
        const phrase = trimmed(edit.phrase);
        if (phrase) {
          const r = D.readsDate(phrase, TODAY);
          if (!r.ok) {
            edit.refused = r.why; edit.caret = "end";
            say("RET refused · " + r.why + " · the cell stays open");
            draw();
            return;
          }
          c[edit.key] = D.isoDay(r.start);
          c[edit.key + "Stamp"] = r.stamp;
        }
        edit = null;
      }
      if (!trimmed(c.title)) {
        draft.refused = "nothing to capture";
        draft.cell = "title";
        say("RET on an empty title · nothing to capture · the title is waiting");
        draw();
        return;
      }
      const plan = DATE_COLS
        .filter((k) => c[k + "Stamp"])
        .map((k) => [k.toUpperCase(), c[k + "Stamp"]]);
      wire = 'capture {"title": ' + JSON.stringify(trimmed(c.title))
        + ', "tag": "trip", "state": "NEXT", "priority": "A"'
        + (plan.length ? ', "planning": ' + JSON.stringify(plan) : "") + "}";
      rows.unshift({ id: "n1", state: c.state, priority: c.priority,
                     title: trimmed(c.title), tag: c.tag,
                     scheduled: c.scheduled, deadline: c.deadline,
                     moved: plan.length ? "with its planning line" : "" });
      draft = null;
      say("captured · :trip:"
          + (plan.length ? " · " + plan.map((p) => p[0] + ": " + p[1]).join(" ") : ""));
      draw();
    }
    function drawDraftCells(tr) {
      const c = draft.cells;
      for (const k of COLS.map((x) => x.key)) {
        const td = part(tr, "td", k === "scheduled" || k === "deadline" ? "cd-date" : null);
        if (editingCell("· draft", k)) { drawEdit(td); continue; }
        if (lentTo("· draft", k)) { drawLent(td, c, COLS.find((x) => x.key === k)); continue; }
        if (k === "title") {
          const box = part(td, "div", "cx-titlecell");
          if (draft.cell === "title") drawTitleEditor(box);
          else part(box, "span", c.title ? null : "cx-ghost", c.title || "a title");
          part(box, "span", "cx-where", "→ trip");
          if (draft.refused) part(box, "span", "cx-refuse", draft.refused);
          else part(box, "span", "cx-badge", "draft");
          continue;
        }
        if (draft.cell === k && DATE_COLS.indexOf(k) === -1) { drawCellEditor(td, k); continue; }
        drawValue(td, c, COLS.find((x) => x.key === k), !c.typed[k]);
      }
    }
    function drawTitleEditor(box) { drawCellEditor(box, "title"); }
    function drawCellEditor(into, key) {
      const input = document.createElement("input");
      input.className = "tv-cell-edit";
      input.id = "cdin";
      input.value = draft.cells[key] || "";
      into.appendChild(input);
      input.addEventListener("keydown", onDraftKey);
      input.addEventListener("input", () => {
        draft.cells[key] = input.value;
        draft.cells.typed[key] = true;
        if (draft.refused) {
          draft.refused = "";
          const note = document.querySelector(".cx-refuse");
          if (note) { note.className = "cx-badge"; note.textContent = "draft"; }
          const tr = document.querySelector("tr.cx-draft");
          if (tr) tr.classList.remove("cx-refused");
        }
      });
      setTimeout(() => { input.focus(); input.select(); }, 0);
    }

    // ---- keys --------------------------------------------------------------
    const nameOf = (e) => {
      const k = e.key;
      if (k === "Enter") return e.shiftKey ? "S-RET" : "RET";
      if (k === "Escape") return "ESC";
      if (k === "Tab") return e.shiftKey ? "S-TAB" : "TAB";
      if (k.indexOf("Arrow") === 0) {
        const w = "<" + k.slice(5).toLowerCase() + ">";
        return (e.shiftKey ? "S-" : "") + w;
      }
      return k;
    };

    /** A KEY INSIDE THE OPEN DATE CELL.  The input takes its own keys and the
     * page's map never sees them (`openCellEditor', table-view.js:4064), which
     * is why `n' in an open cell types an `n'. */
    function onEditKey(e) {
      e.stopPropagation();
      const k = nameOf(e);
      const by = D.dateStep(k);
      if (by) { e.preventDefault(); stepDate(by); return; }
      if (k === "RET") { e.preventDefault(); draft ? commitDraft() : commitDate(); return; }
      if (k === "ESC") {
        e.preventDefault();
        if (draft) dropDraft(); else cancelDate();
        return;
      }
      // TAB IN A LANDED ROW'S CELL COMMITS, the in-cell editor's own reading
      // (`table-view.js:4070'); inside a DRAFT it walks, which is the narrowing
      // the capture-in-table spike settled.
      if (k === "TAB" || k === "S-TAB") {
        e.preventDefault();
        if (draft) hopDraft(k === "TAB" ? 1 : -1); else commitDate();
      }
    }
    function onDraftKey(e) {
      e.stopPropagation();
      const k = nameOf(e);
      if (k === "TAB" || k === "S-TAB") { e.preventDefault(); hopDraft(k === "TAB" ? 1 : -1); return; }
      if (k === "RET") { e.preventDefault(); commitDraft(); return; }
      if (k === "ESC") { e.preventDefault(); dropDraft(); }
    }

    document.addEventListener("keydown", (e) => {
      if (e.defaultPrevented) return;
      const k = nameOf(e);
      if (e.ctrlKey && e.key === "c") {
        e.preventDefault();
        chord = true;
        say("C-c —");
        drawFoot();
        setTimeout(() => { chord = false; }, 2000);
        return;
      }
      // THE APP'S OWN TWO, org's own spelling, over the row at point
      // (`Keymap.hs:100', `:102').
      if (chord && e.ctrlKey && (e.key === "s" || e.key === "d")) {
        e.preventDefault();
        chord = false;
        if (!draft) openDate(rows[point].id, e.key === "s" ? "scheduled" : "deadline");
        return;
      }
      chord = false;
      if (edit || draft) return;
      if (k === "+") { e.preventDefault(); openDraft(); return; }
      if (k === "n" || k === "<down>" || k === "j") {
        e.preventDefault(); point = Math.min(point + 1, rows.length - 1); draw(); return;
      }
      if (k === "p" || k === "<up>" || k === "k") {
        e.preventDefault(); point = Math.max(point - 1, 0); draw(); return;
      }
      // THE CELL CURSOR.  `l'/`h' and the arrows alias `f'/`b' on every surface
      // glance draws (docs/design-rhymes.md, "Movement is two axes").
      // With two date columns the two directions are one toggle; the rig keeps
      // both spellings so the rhyme is driven rather than described.
      if ("flbh".indexOf(k) !== -1 || k === "<right>" || k === "<left>") {
        e.preventDefault(); col = DATE_COLS[(DATE_COLS.indexOf(col) + 1) % 2]; draw(); return;
      }
      if (k === "RET") { e.preventDefault(); openDate(rows[point].id, col); return; }
      if (k === "~") {
        e.preventDefault();
        const now = document.documentElement.getAttribute("data-theme");
        document.documentElement.setAttribute("data-theme", now === "dark" ? "light" : "dark");
      }
    });

    // ---- the measurements ---------------------------------------------------
    /** How wide the ghost WANTS to be, measured in the cell's own font off a
     * span nothing can see.  A ghost is clipped by exactly what this exceeds. */
    function wants(text) {
      const m = el("cdmeasure");
      if (!m) return 0;
      m.textContent = text;
      return Math.round(m.getBoundingClientRect().width);
    }
    function room() {
      const td = document.querySelector("td.cd-date");
      if (!td) return { col: 0, text: 0 };
      const cs = getComputedStyle(td);
      const w = td.getBoundingClientRect().width;
      return { col: Math.round(w),
               text: Math.round(w - parseFloat(cs.paddingLeft) - parseFloat(cs.paddingRight)) };
    }
    function metrics() {
      const r = room();
      if (!edit) return "a date column is " + r.col + "px, its text run " + r.text + "px";
      const said = ghostText();
      const box = look.ghost === "cell" ? el("cdghost")
        : look.ghost === "neighbour" ? document.querySelector("td.cd-lent .dgh")
        : document.querySelector("tr.cd-strip td");
      const drew = box ? Math.round(box.getBoundingClientRect().width) : 0;
      const want = wants(look.ghost === "strip"
        ? edit.phrase + said.text : said.text);
      const cut = Math.max(0, want - drew);
      return "the ghost wants " + want + "px, is drawn " + drew + "px"
        + (cut ? ", CLIPPED by " + cut + "px" : ", whole");
    }

    draw();
    // The hooks `shots.mjs' drives the pages through; no variant reads them.
    window.RIG_TEST = {
      openDate, openDraft, step: stepDate,
      day: () => TODAY,
      state: () => ({
        editing: !!edit, key: edit && edit.key, phrase: edit && edit.phrase,
        refused: edit ? edit.refused : "", told, wire, point, col,
        draft: !!draft, cell: draft && draft.cell,
        draftRefused: draft ? draft.refused : "",
        rows: rows.map((r) => ({ id: r.id, title: r.title,
                                 scheduled: r.scheduled, deadline: r.deadline })),
      }),
      /** The ghost as DRAWN, the box it was given, and what it wanted. */
      ghost: () => {
        if (!edit) return null;
        const said = ghostText();
        const box = look.ghost === "cell" ? el("cdghost")
          : look.ghost === "neighbour" ? document.querySelector("td.cd-lent .dgh")
          : document.querySelector("tr.cd-strip td .cd-stamp")
            || document.querySelector("tr.cd-strip td .cd-bad");
        const drew = box ? Math.round(box.getBoundingClientRect().width) : 0;
        return { text: said.text, bad: said.bad, drew,
                 want: wants(said.text),
                 cut: Math.max(0, wants(said.text) - drew) };
      },
      room,
      /** The caret's own reading: where it stands, and where it can reach. */
      caret: () => {
        const f = el("cdin");
        return f ? { value: f.value, start: f.selectionStart, end: f.selectionEnd } : null;
      },
      /** The strip's height, and the top of every row, so a shift is a number. */
      geom: () => {
        const tops = {};
        for (const tr of document.querySelectorAll("#tablewrap tbody tr[data-id]"))
          tops[tr.dataset.id] = Math.round(tr.getBoundingClientRect().top);
        const strip = document.querySelector("tr.cd-strip");
        return { tops, strip: strip ? Math.round(strip.getBoundingClientRect().height) : 0,
                 rowH: Math.round((document.querySelector("#tablewrap tbody tr")
                                   || { getBoundingClientRect: () => ({ height: 0 }) })
                                  .getBoundingClientRect().height) };
      },
    };
  }
})();
