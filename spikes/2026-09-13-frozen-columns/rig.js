/* THE RIG — one file, five tabs, and the widget's own column arithmetic
   transcribed rather than approximated.

   WHY A TRANSCRIPTION AND NOT A DRAWING.  The question this spike asks is
   *"what moves a column?"*, so a rig that invented its own widths would be
   answering about itself.  Everything from `CELL_PAD' down to `applyWidths'
   below is `assets/table-view.js' line for line, with the line beside it; the
   FIXTURE is the one `spikes/2026-09-12-date-overlay/rig.js' carries, so the
   two spikes read as one table; the columns are `viewColumns' (`Query.hs:2492')
   in its own order.

   WHAT THE RIG LEAVES OUT, on purpose: the virtualizer (eighteen rows fit, and
   a window that never scrolls cannot confuse a width with a repaint), the
   filter parser, the mark and flag sets, the sort itself.  None of them touches
   a width.  What it keeps is everything that does.

   Nothing in here is production code.  The fixture is invented.  `file://', no
   build step, no modules. */
const RIG = (function () {
  "use strict";

  // ---- the widget's constants, verbatim (`table-view.js:1061'-`:1070') ----
  const CELL_PAD = 24;         // a cell's horizontal padding, both sides
  const PILL_PAD = 17;         // a badge pill's ground, both sides, in px
  const COL_MAX_CH = 40;       // ceiling on a sized column, in characters
  const TITLE_MIN_CH = 40;     // the fill column's floor, in characters
  const TAG_EM = 0.92;         // the tag type's size, as a share of the table's

  // ---- the multi-valued cell, verbatim (`table-view.js:237'-`:268') ------
  const TAG_SEP = " · ";
  const TAG_MORE = "…";
  const tagsIn = (cell) => cell.split(":").filter(Boolean);
  function tagsWide(tags) {
    let n = 0;
    for (let i = 0; i < tags.length; i++) n += tags[i].length + (i ? TAG_SEP.length : 0);
    return n;
  }
  /** How many of TAGS fit WHOLE in ROOM characters (`tagsFit', `:257'). */
  function tagsFit(tags, room) {
    if (tagsWide(tags) <= room) return tags.length;
    let used = 0, kept = 0;
    for (const t of tags) {
      const w = used + (kept ? TAG_SEP.length : 0) + t.length;
      if (w + 1 + TAG_MORE.length > room) break;   // the mark rides a space behind
      used = w;
      kept++;
    }
    return kept;
  }
  /** The COLUMN width a multi-valued CELL's drawn form needs (`tagsCh', `:1086'). */
  function tagsCh(cell) {
    const tags = tagsIn(cell);
    return tags.length ? Math.ceil(tagsWide(tags) * TAG_EM) : cell.length;
  }
  const tagRoom = (ch) => Math.floor(ch / TAG_EM);
  const displayText = (v) =>
    String(v == null ? "" : v).replace(/[\u0000-\u001f\u007f]+/g, " ");

  /* THE FIXTURE, carried whole from `spikes/2026-09-12-date-overlay/rig.js' —
     eighteen rows, `:trip:' on all but two, three of them carrying no DEADLINE
     and one carrying neither date.  The widths it produces are the numbers the
     prior spike argued about, which is why it is not re-invented. */
  const FIXTURE = [
    ["r1",  "TODO", "B", "Renew the passport",         "trip:admin",  "2026-09-15", "2026-09-30"],
    ["r2",  "NEXT", "A", "Book the Kyoto flights",     "trip:travel", "2026-09-18", "2026-10-02"],
    ["r3",  "NEXT", "B", "Write the overnight report", "glance",      "2026-09-24", ""],
    ["r4",  "NEXT", "A", "Pack the camera gear",       "trip:gear",   "",           ""],
    ["r5",  "DONE", "",  "Read the visa rules",        "trip",        "",           "2026-09-20"],
    ["r6",  "TODO", "C", "Ship the fold marks",        "spike",       "",           ""],
    ["r7",  "TODO", "B", "Buy the rail pass",          "trip:travel", "2026-09-26", "2026-10-05"],
    ["r8",  "NEXT", "C", "Copy the insurance card",    "trip:admin",  "2026-09-19", ""],
    ["r9",  "TODO", "",  "Learn twenty words",         "trip",        "2026-09-13", "2026-10-01"],
    ["r10", "NEXT", "B", "Cancel the milk",            "trip:admin",  "2026-09-29", ""],
    ["r11", "TODO", "A", "Charge the power bank",      "trip:gear",   "2026-09-30", "2026-09-30"],
    ["r12", "DONE", "",  "Renew the card",             "trip:admin",  "",           "2026-09-08"],
    ["r13", "NEXT", "B", "Print the itinerary",        "trip",        "2026-10-01", ""],
    ["r14", "TODO", "C", "Water the plants, ask Ana",  "trip:admin",  "2026-09-28", "2026-09-29"],
    ["r15", "TODO", "B", "Pick up the yen",            "trip:admin",  "2026-09-27", ""],
    ["r16", "NEXT", "A", "Confirm the ryokan",         "trip:travel", "2026-09-16", "2026-09-22"],
    ["r17", "TODO", "",  "Download the maps",          "trip:gear",   "",           ""],
    ["r18", "TODO", "C", "Set the out-of-office",      "trip:admin",  "2026-10-02", "2026-10-02"],
  ].map(([id, state, priority, title, tag, scheduled, deadline]) => ({
    id,
    cells: { state, priority: priority ? "[#" + priority + "]" : "",
             title, scheduled, deadline, tag: ":" + tag + ":" },
  }));

  /* THE APPLIED QUERY, chosen for the WIDTH it produces: under
     `sort:scheduled->deadline' BOTH date columns carry a header mark, so both
     measure `calc(13ch + 24px)' — ten characters of `isoStamp' plus three of
     `▲¹ ' (`colWidths', `:3004'). */
  const QUERY = "tag:trip sort:scheduled->deadline";

  // `viewColumns' in its own draw order (`Query.hs:2492').
  const COLS = [
    { key: "state",     header: "State",     type: "badge" },
    { key: "priority",  header: "#",         type: "badge" },
    { key: "title",     header: "Title",     type: "text"  },
    { key: "scheduled", header: "Scheduled", type: "text", mark: "▲¹" },
    { key: "deadline",  header: "Deadline",  type: "text", mark: "▲²" },
    { key: "tag",       header: "Tags",      type: "text"  },
  ];
  /* THE SECOND VIEW, and why it is a sort and not a filter: the header's sort
     MARK is paid for inside the column's measure (`colWidths', `:3004'), so
     dropping `sort:' takes three characters off both date columns and hands
     them to the title.  A query change that re-measures nothing else still
     re-measures this — which is exactly the case D has to answer for. */
  const QUERY_B = "tag:trip";
  const AT = (key) => COLS.findIndex((c) => c.key === key);
  const TITLE_AT = AT("title");
  const MULTI_AT = AT("tag");

  const STATE_INK = { TODO: "--g-todo", NEXT: "--g-todo", DONE: "--g-done" };
  const PRIO_INK = { A: "--g-prio-a", B: "--g-prio-b", C: "--g-prio-c" };

  const el = (id) => document.getElementById(id);
  const part = (into, tag, cls, text) => {
    const n = document.createElement(tag);
    if (cls) n.className = cls;
    if (text !== undefined) n.textContent = text;
    into.appendChild(n);
    return n;
  };
  const EDGE = 8;              // the window's gutter, what a laid box clamps to
  const px = (n) => Math.round(n * 100) / 100 + "px";
  const r2 = (n) => Math.round(n * 100) / 100;

  return { mount: mount };

  // ==========================================================================
  function mount(opts) {
    /* THE ONE FIELD THAT DIFFERS BETWEEN TABS.
       "none"    — 0, the control: widths are re-measured whenever the store is
                   republished, and `closeCellEditor' keeps its shipped order.
       "clip"    — A, pinned at open; the input fills its cell and scrolls in it.
       "overlay" — B, pinned at open; the input is laid OVER the cell and grows
                   right, over its neighbours in that row alone.
       "row"     — C, pinned at open; the editing row is detached and reflows by
                   itself over its own slot.
       "always"  — D, pinned at mount and never lifted. */
    const mode = opts.mode;
    const pins = mode !== "none";

    let rows = FIXTURE.map((r) => ({ id: r.id, cells: Object.assign({}, r.cells) }));
    let widths = null;             // the cache (`table-view.js:2192')
    let pin = null;                // px per column while the pin stands, else null
    let point = 0;                 // the row the cursor stands on
    let col = TITLE_AT;            // the cell within it
    let cellEdit = null;           // { id, at, td, input, raw }
    let drafting = null;           // the producer's own row, or none
    let told = "";
    let ruler = false;
    let sorted = true;             // does the applied query name a sort chain?
    let jump = null;               // what the last thaw cost, in px

    const DRAFT_ID = "· draft";
    const DRAFT_CELLS = ["title", "state", "priority", "scheduled", "deadline", "tag"];

    const wrap = el("tablewrap");
    let table, colgroup, thead, headRow, tbody, colEls = [];
    let over = null;               // B's laid-over input / C's detached line

    // ---- the measure, transcribed -----------------------------------------
    /** Every row the table draws, the draft among them (`ordered', `:2896'). */
    const ordered = () => rows;

    /** Column widths (`colWidths', `table-view.js:2988').  TEXT IN `ch',
     *  GROUNDS IN `px'.  A `title' column is present, so FILL is always on and
     *  the cells decide: a header widens nothing. */
    function colWidths() {
      if (widths) return widths;
      const cell = COLS.map(() => 0);
      for (const r of ordered())
        for (let i = 0; i < COLS.length; i++) {
          const raw = displayText(r.cells[COLS[i].key]);
          const n = i === MULTI_AT ? tagsCh(raw) : raw.length;
          if (n > cell[i]) cell[i] = n;
        }
      widths = COLS.map((c, i) => ({
        // the header's sort mark is paid outside the cells' measure (`:3004')
        ch: (cell[i] || String(c.header).length) + (sorted && c.mark ? c.mark.length + 1 : 0),
        ground: CELL_PAD + (c.type === "badge" && cell[i] ? PILL_PAD : 0),
      }));
      return widths;
    }

    /** Widen the cached widths for ROW (`growWidths', `:3015') — an upsert can
     *  only add text, so this never narrows anything. */
    function growWidths(r) {
      if (!widths) return;
      for (let i = 0; i < widths.length; i++) {
        const raw = displayText(r.cells[COLS[i].key]);
        const n = i === MULTI_AT ? tagsCh(raw) : raw.length;
        if (n > widths[i].ch) widths[i].ch = n;
        if (raw.length && COLS[i].type === "badge") widths[i].ground = CELL_PAD + PILL_PAD;
      }
    }

    /** Characters the tag column may draw in (`tagsRoom', `:3035'). */
    const tagsRoom = () => tagRoom(Math.min(colWidths()[MULTI_AT].ch, COL_MAX_CH));

    /** Write the measured widths onto the columns (`applyWidths', `:3050').
     *  THE PIN SHORT-CIRCUITS IT: while one stands, the measure is not what the
     *  columns are — which is the whole of what A, B, C and D do to the widget. */
    function applyWidths() {
      if (pin) { writePin(); return; }
      const w = colWidths();
      let ch = Math.min(w[TITLE_AT].ch, TITLE_MIN_CH);
      let pad = w[TITLE_AT].ground;
      for (let i = 0; i < colEls.length; i++) {
        const n = Math.min(w[i].ch, COL_MAX_CH);
        // the fill column carries NO width: fixed layout hands it the remainder
        const s = i === TITLE_AT ? "" : "calc(" + n + "ch + " + w[i].ground + "px)";
        if (i !== TITLE_AT) { ch += n; pad += w[i].ground; }
        if (colEls[i].style.width !== s) colEls[i].style.width = s;
      }
      table.style.width = "";
      table.style.minWidth = "calc(" + ch + "ch + " + pad + "px)";
    }

    // ---- the pin ----------------------------------------------------------
    /** FREEZE: every column's CURRENT drawn width, in px, written onto the cols
     *  — the fill column among them, or the fixed layout would hand it whatever
     *  the others left and the pin would be a pin of five columns out of six.
     *  The table's own width is pinned with them: under `table-layout:fixed' a
     *  column set that sums short of the table gets the slack back in
     *  proportion, which is a reflow wearing a pin's clothes. */
    function freezeColumns() {
      const ths = [...headRow.querySelectorAll("th")];
      pin = ths.map((th) => th.getBoundingClientRect().width);
      writePin();
      table.classList.add("fz-pinned");
    }
    function writePin() {
      let total = 0;
      for (let i = 0; i < colEls.length; i++) {
        colEls[i].style.width = px(pin[i]);
        total += pin[i];
      }
      table.style.width = px(total);
      table.style.minWidth = px(total);
    }
    /** D'S ONE DOOR — `fitColumns()', the measure run ONCE PER VIEW.  Every
     *  column is measured off the header word and the widest value the result
     *  set holds AT THAT MOMENT, written in px, and left there: no upsert, no
     *  delta, no draft and no editor re-opens it.  What DOES: the first rows
     *  paint after mount, a view or query change, and a window resize.
     *  The jump it costs is reported per column, so "it re-fits once" is a
     *  number rather than a promise. */
    function fitColumns() {
      const had = pin ? pin.slice() : null;
      pin = null;
      widths = null;
      applyWidths();                     // the measure, exactly once
      freezeColumns();                   // and the answer pinned to what it said
      if (had) jump = pin.map((n, i) => r2(n - had[i]));
    }

    /** THAW, and the ONE JUMP it costs: the widths the pin held, against the
     *  widths the measure gives once it is lifted.  Reported in px per column —
     *  a thaw that moves nothing reports zeroes, which is the outcome A wants
     *  when nothing the reader typed changed a sized column. */
    function thawColumns() {
      if (!pin) return;
      const had = pin.slice();
      pin = null;
      table.classList.remove("fz-pinned");
      widths = null;                       // the store changed; measure again
      applyWidths();
      const now = [...headRow.querySelectorAll("th")].map((th) =>
        th.getBoundingClientRect().width);
      jump = now.map((n, i) => r2(n - had[i]));
    }

    // ---- the surface ------------------------------------------------------
    function build() {
      el("filter").value = sorted ? QUERY : QUERY_B;
      const chips = el("chips");
      chips.textContent = "";
      for (const t of (sorted ? QUERY : QUERY_B).split(/\s+/))
        part(chips, "span", "tv-chip" + (/^sort:/.test(t) ? " cx-lends" : ""), t);
      wrap.textContent = "";
      table = part(wrap, "table", "tv-table tv-fill");
      colgroup = part(table, "colgroup");
      thead = part(table, "thead");
      headRow = part(thead, "tr");
      tbody = part(table, "tbody");
      renderHead();
      renderRows();
    }

    /** Rebuild the colgroup and the header row (`renderHead', `:3072').
     *  THE COLS COME BACK BARE — which is the shipped behaviour and, in tab 0,
     *  the bug this spike found on the way in. */
    function renderHead() {
      colgroup.innerHTML = "";
      headRow.innerHTML = "";
      colEls = [];
      for (const c of COLS) {
        colEls.push(part(colgroup, "col"));
        const th = part(headRow, "th", c.type === "badge" ? "tv-badge" : null);
        th.dataset.key = c.key;
        part(th, "span", null, c.header);
        if (sorted && c.mark) part(th, "span", "tv-arrow", " " + c.mark);
      }
    }

    function renderRows() {
      tbody.textContent = "";
      const room = tagsRoom();
      rows.forEach((r, i) => {
        const tr = part(tbody, "tr", [i % 2 ? "tv-alt" : "",
                                      !cellEdit && point === i ? "tv-sel" : "",
                                      r.id === DRAFT_ID ? "tv-producer" : ""]
                                     .filter(Boolean).join(" "));
        tr.dataset.id = r.id;
        COLS.forEach((c, j) => {
          const td = part(tr, "td", c.key === "scheduled" || c.key === "deadline"
                                    ? "cd-date" : null);
          td.dataset.key = c.key;
          drawValue(td, r, c, j === MULTI_AT ? room : null);
          if (!cellEdit && point === i && col === j) td.classList.add("cd-at");
        });
      });
      applyWidths();
      drawRuler();
    }

    /** A cell's drawn form (`cellHTML', `:322'). */
    function drawValue(td, r, c, room) {
      const raw = displayText(r.cells[c.key]);
      if (room !== null) {
        const tags = tagsIn(raw);
        if (!tags.length) { td.textContent = raw; return; }
        const kept = tagsFit(tags, room);
        const span = part(td, "span", "tv-tags");
        span.textContent = tags.slice(0, kept).join(TAG_SEP)
          + (kept === tags.length ? "" : (kept ? " " : "") + TAG_MORE);
        return;
      }
      if (c.type === "badge") {
        if (!raw) return;
        const n = part(td, "span", "tv-pill", raw);
        const token = c.key === "priority" ? PRIO_INK[raw.slice(2, 3)] : STATE_INK[raw];
        n.style.setProperty("--tv-badge", "var(" + (token || "--g-mute") + ")");
        return;
      }
      td.textContent = raw;
    }

    /** THE HEADER'S X-POSITIONS DRAWN OVER THE ROWS, so a screenshot carries the
     *  evidence a measurement makes.  `g' shows them. */
    function drawRuler() {
      [...wrap.querySelectorAll(".fz-rule")].forEach((n) => n.remove());
      if (!ruler) return;
      const base = wrap.getBoundingClientRect();
      for (const th of headRow.querySelectorAll("th")) {
        const b = th.getBoundingClientRect();
        const n = part(wrap, "div", "fz-rule");
        n.style.left = px(b.right - base.left + wrap.scrollLeft - 1);
      }
    }

    // ---- the in-cell editor, transcribed ----------------------------------
    /** `openCellEditor' (`table-view.js:4040'): an `<input>' placed IN the td,
     *  filled with the cell's RAW value.  What each tab does DIFFERENTLY starts
     *  at `pins' — the pin is taken before the input goes in, so the width it
     *  reads is the width the reader was looking at. */
    function openCellEditor(id, at) {
      closeCellEditor();
      const tr = tbody.querySelector('tr[data-id="' + CSS.escape(id) + '"]');
      if (!tr) return false;
      const td = tr.querySelectorAll("td")[at];
      if (!td) return false;
      if (pins && !pin) freezeColumns();
      const row = rows.find((r) => r.id === id);
      const raw = displayText(row.cells[COLS[at].key]);
      const input = document.createElement("input");
      input.className = "tv-cell-edit";
      input.value = raw;
      if (mode === "overlay") layOver(td, input);
      else if (mode === "row") detachRow(tr, at, input);
      else { td.textContent = ""; td.appendChild(input); }
      cellEdit = { id, at, td, input, raw };
      input.focus();
      input.select();
      input.addEventListener("input", onTyped);
      input.addEventListener("keydown", onCellKey);
      paintSelection();
      say(told);
      return true;
    }

    /** `closeCellEditor' (`:4017').  THE SHIPPED ORDER IS KEPT IN TAB 0 AND
     *  REPAIRED IN THE OTHERS: `renderRows' writes the widths onto the cols
     *  `renderHead' is about to throw away, so the shipped pair leaves the
     *  colgroup BARE and a fixed-layout table with no col widths divides the
     *  window equally.  Every pinning variant has to repair it before it can
     *  pin anything, which is finding one of this spike. */
    function closeCellEditor() {
      if (!cellEdit) return;
      cellEdit = null;
      dropOver();
      renderRows();
      renderHead();
      if (mode !== "none") applyWidths();
    }

    /** What the reader typed, folded back into the row. */
    function commitCellEditor() {
      if (!cellEdit) return;
      const { id, at, input } = cellEdit;
      const value = input.value;
      const row = rows.find((r) => r.id === id);
      closeCellEditor();
      row.cells[COLS[at].key] = value;
      upsert(row);
    }

    /** `upsertRow' (`:5283'): the store's own door.  The text cache is dropped
     *  for that row, the widths are GROWN by it, and the table repaints. */
    function upsert(row) {
      if (rows.indexOf(row) === -1) rows.push(row);
      growWidths(row);
      if (mode !== "always") thawColumns();
      renderRows();
    }

    // ---- B: the cell's content laid over its neighbours -------------------
    /** THE INPUT OVER THE CELL, growing right to its own text — the way the
     *  date box is laid over a cell (`36-date-cell.js', `cellRect').  The table
     *  under it does not move; the row's later cells are COVERED.
     *
     *  FIXED, AGAINST THE VIEWPORT, which is where the app hangs its own laid
     *  editor (`page.css:752'): a box inside `#tablewrap' would be owned by that
     *  scroller's `overflow' and either clipped at its edge or given it a
     *  sideways scrollbar the reader never asked for. */
    function layOver(td, input) {
      const b = td.getBoundingClientRect();
      over = part(document.body, "div", "fz-over");
      over.style.top = px(b.top);
      over.style.height = px(b.height);
      over.dataset.floor = String(b.width);
      over.dataset.left = String(b.left);
      over.appendChild(input);
      growOver();
    }
    /** The box takes the cell's width as a FLOOR and its text's as the rest,
     *  and is CLAMPED at the window's right edge: a cell in the LAST column has
     *  no room to the right, so past the edge the box grows the other way and
     *  the run it covers is the row's own tail.  Which is the one thing B has to
     *  answer for and the number the rig reports as `spill'. */
    function growOver() {
      if (!over || !cellEdit) return;
      const floor = Number(over.dataset.floor), at = Number(over.dataset.left);
      const wide = Math.max(floor, textWidth(cellEdit.input) + CELL_PAD + 4);
      over.style.width = px(wide);
      over.style.left = px(Math.max(EDGE, Math.min(at, window.innerWidth - EDGE - wide)));
    }
    function dropOver() {
      if (over) over.remove();
      over = null;
    }

    // ---- C: the editing row, detached ------------------------------------
    /** THE ROW ALONE.  Its `<tr>' is left standing as a spacer, blanked, and a
     *  one-row LINE is stacked exactly over its slot: the cells start at the
     *  header's x-positions and each takes its own content's width, so the open
     *  one may widen and the ones behind it slide.  Every OTHER row keeps the
     *  grid — which is the trade this tab is here to show. */
    function detachRow(tr, at, input) {
      const b = tr.getBoundingClientRect();
      const row = rows.find((r) => r.id === tr.dataset.id);
      const ths = [...headRow.querySelectorAll("th")];
      tr.classList.add("fz-slot");
      over = part(document.body, "div", "fz-line");
      over.style.left = px(b.left);
      over.style.top = px(b.top);
      over.style.height = px(b.height);
      COLS.forEach((c, j) => {
        const cell = part(over, "span", "fz-cell");
        cell.dataset.key = c.key;
        // THE HEADER'S X-POSITIONS ARE THE STARTING POINT and no more: a closed
        // cell keeps the width its column had, so a row opened and closed again
        // without a keystroke lands back exactly where it stood.
        if (j === at) cell.appendChild(input);
        else { cell.textContent = displayText(row.cells[c.key]);
               cell.style.width = px(ths[j].getBoundingClientRect().width - CELL_PAD); }
      });
      growLine();
    }
    /** The open cell's box follows its text; the cells behind it slide. */
    function growLine() {
      if (!over || !cellEdit) return;
      const box = over.querySelector(".fz-cell > input").parentNode;
      box.style.width = px(textWidth(cellEdit.input) + 4);
      let wide = 0;
      for (const c of over.querySelectorAll(".fz-cell")) wide += c.offsetWidth;
      over.dataset.wants = String(Math.round(wide));
      over.style.width = px(Math.min(wide, window.innerWidth - EDGE
                                           - over.getBoundingClientRect().left));
    }

    /** THE TEXT'S OWN WIDTH, measured in the table's face rather than guessed
     *  at from a character count — the ch the arithmetic spends is the advance
     *  of `0', and a title is not written in zeroes. */
    function textWidth(input) {
      const m = el("fzmeasure");
      m.style.font = getComputedStyle(input).font;
      m.textContent = input.value || "";
      return m.getBoundingClientRect().width;
    }

    function onTyped() {
      if (mode === "overlay") growOver();
      if (mode === "row") growLine();
      drawFoot();
    }

    // ---- the draft --------------------------------------------------------
    /** `+': A DRAFT ROW UNDER THE ROW AT POINT (`35-draft.js:openDraft'), wearing
     *  what the filter pins — `:trip:' is the one tag this query names, so it is
     *  the destination and the draft's whole run. */
    function openDraft() {
      if (drafting) { openCellEditor(DRAFT_ID, AT("title")); return; }
      drafting = { id: DRAFT_ID, producer: true,
                   cells: { state: "", priority: "", title: "",
                            scheduled: "", deadline: "", tag: ":trip:" } };
      rows.splice(point + 1, 0, drafting);
      growWidths(drafting);
      renderRows();
      openCellEditor(DRAFT_ID, AT("title"));
      say("→ trip");
    }
    /** `TAB': ONE STEP OF THE RING (`walkDraft', `35-draft.js').  The closing
     *  cell's value goes into the row first — the draft IS the row the widget
     *  holds, one object, so the assignment is the republish — and the next stop
     *  opens through `editCell', which closes before it looks a cell up.
     *  NOTHING IS UPSERTED HERE, exactly as the glue does not: which is why tab
     *  0's columns come back BARE at every step and the pinned tabs' do not. */
    function walkDraft(step) {
      const ring = DRAFT_CELLS.map(AT).sort((a, b) => a - b);
      const { at, input } = cellEdit;
      drafting.cells[COLS[at].key] = input.value;
      const i = ring.indexOf(at);
      const next = ring[((i === -1 ? 0 : i + step) + ring.length) % ring.length];
      openCellEditor(DRAFT_ID, next);
    }
    /** `RET' over a draft finalizes the capture; the row lands as a standing one. */
    function commitDraft() {
      const { at, input } = cellEdit;
      drafting.cells[COLS[at].key] = input.value;
      const landed = drafting;
      drafting = null;
      landed.id = "r" + (rows.length + 20);
      delete landed.producer;
      closeCellEditor();
      upsert(landed);
      say("captured · :trip:");
    }
    /** `ESC': the whole draft goes; no file was written, so nothing is put back. */
    function dropDraft() {
      rows = rows.filter((r) => r !== drafting);
      drafting = null;
      closeCellEditor();
      if (mode !== "always") thawColumns();
      renderRows();
      say("the draft is gone");
    }

    /** A QUERY CHANGE — the one thing that DOES re-measure under D.  Dropping
     *  `sort:' takes the header marks off both date columns, which is three
     *  characters each inside their own measure; the title takes them back. */
    function changeView() {
      closeCellEditor();
      sorted = !sorted;
      if (mode === "always") { renderHead(); fitColumns(); renderRows(); }
      else { widths = null; renderHead(); renderRows(); }
      say(sorted ? "sort:scheduled->deadline applied" : "the sort is dropped");
    }

    /** A ROW THAT LANDS LATER, with a run no column was measured for — what a
     *  capture from elsewhere looks like, and the one case D cannot answer. */
    function arrive() {
      const row = { id: "r" + (rows.length + 40),
                    cells: { state: "WAITING", priority: "[#A]",
                             title: "Confirm the consulate appointment slot",
                             scheduled: "2026-10-08", deadline: "2026-10-09",
                             tag: ":trip:admin:visa:consulate:paperwork:" } };
      rows.splice(point + 1, 0, row);
      upsert(row);
      say("a row arrived");
    }

    // ---- the keys ---------------------------------------------------------
    function onCellKey(e) {
      e.stopPropagation();
      if (e.key === "Tab") {
        e.preventDefault();
        if (drafting && cellEdit.id === DRAFT_ID) walkDraft(e.shiftKey ? -1 : 1);
        else commitCellEditor();
        return;
      }
      if (e.key === "Enter") {
        e.preventDefault();
        if (drafting && cellEdit.id === DRAFT_ID) commitDraft();
        else commitCellEditor();
        return;
      }
      if (e.key === "Escape") {
        e.preventDefault();
        if (drafting && cellEdit.id === DRAFT_ID) { dropDraft(); return; }
        // THE SHIPPED ESCAPE IS `closeCellEditor()' AND NOTHING ELSE (`:4082'),
        // which in tab 0 is where the bare colgroup is left standing.
        closeCellEditor();
        if (mode === "none") return;
        thawColumns();
        renderRows();
      }
    }

    document.addEventListener("keydown", (e) => {
      if (cellEdit) return;                      // the open input takes its own
      const k = e.key;
      if (k === "n" || k === "ArrowDown") { point = Math.min(rows.length - 1, point + 1); }
      else if (k === "p" || k === "ArrowUp") { point = Math.max(0, point - 1); }
      else if (k === "f" || k === "ArrowRight") { col = Math.min(COLS.length - 1, col + 1); }
      else if (k === "b" || k === "ArrowLeft") { col = Math.max(0, col - 1); }
      else if (k === "Enter") { e.preventDefault(); openCellEditor(rows[point].id, col); return; }
      else if (k === "+" || k === "=") { e.preventDefault(); openDraft(); return; }
      else if (k === "a") { arrive(); return; }
      else if (k === "q") { changeView(); return; }
      else if (k === "g") { ruler = !ruler; drawRuler(); return; }
      else if (k === "~") {
        const r = document.documentElement;
        r.dataset.theme = r.dataset.theme === "dark" ? "light" : "dark";
        drawFoot();
        return;
      }
      else return;
      e.preventDefault();
      renderRows();
      keepInView();
      drawFoot();
    });

    function keepInView() {
      const at = tbody.querySelector('tr[data-id="' + CSS.escape(rows[point].id) + '"]');
      if (at && at.scrollIntoView) at.scrollIntoView({ block: "nearest" });
    }
    function paintSelection() { drawFoot(); }
    const say = (s) => { told = s; drawFoot(); };

    function drawFoot() {
      el("state").textContent =
        (pin ? "PINNED" : "fluid") + " · " + geom().cols.map((c) => c.key + " " + c.width).join(" · ");
      el("truth").textContent = cellEdit
        ? "editing " + COLS[cellEdit.at].key + " · " + seen().visible + "/" + seen().chars + " chars on screen"
        : "row " + rows[point].id + " · cell " + COLS[col].key;
      el("says").textContent = told;
    }

    // ---- what the shots read ----------------------------------------------
    /** EVERY COLUMN'S DRAWN GEOMETRY and every row's, as the reader's eye has
     *  it: the header's boxes, and each rendered row's cell boxes. */
    function geom() {
      const base = table.getBoundingClientRect();
      const cols = [...headRow.querySelectorAll("th")].map((th, i) => {
        const b = th.getBoundingClientRect();
        return { key: COLS[i].key, left: r2(b.left - base.left), width: r2(b.width) };
      });
      const rs = [...tbody.querySelectorAll("tr[data-id]")].map((tr) => ({
        id: tr.dataset.id,
        top: r2(tr.getBoundingClientRect().top - base.top),
        text: [...tr.querySelectorAll("td")].map((td) => td.textContent),
        cells: [...tr.querySelectorAll("td")].map((td) => {
          const b = td.getBoundingClientRect();
          // CLIPPED is what the reader actually loses: a cell whose drawn text
          // is wider than the box it was given, ellipsis and all.
          return { left: r2(b.left - base.left), width: r2(b.width),
                   clipped: td.scrollWidth > td.clientWidth + 0.5 };
        }),
      }));
      // WHAT THE LAID BOX DOES TO THE ROW IT LIES ON: how far past its own
      // cell's right edge it reaches (B), and how far the detached line's cell
      // boundaries have drifted from the header's (C).
      let laid = null;
      if (over && cellEdit) {
        const b = over.getBoundingClientRect();
        const cell = cellEdit.td.getBoundingClientRect();
        laid = { kind: over.className, left: r2(b.left), width: r2(b.width),
                 spill: r2(b.right - cell.right),
                 wants: over.dataset.wants ? Number(over.dataset.wants) : r2(b.width),
                 // C's own cost, once the row's cells sum past the window: the
                 // line scrolls to keep the caret in sight and the row's LEFT
                 // goes off the edge.  A number, so it is not an opinion.
                 scrolled: r2(over.scrollLeft),
                 drift: [...over.querySelectorAll(".fz-cell")].map((c, i) =>
                   r2(c.getBoundingClientRect().left
                      - headRow.querySelectorAll("th")[i].getBoundingClientRect().left)) };
      }
      return { mode, pinned: !!pin, width: r2(base.width), cols: cols, rows: rs,
               laid: laid, jump: jump };
    }

    /** WHAT THE READER CAN SEE OF WHAT THEY TYPED: the value, and the slice of
     *  it the open box actually shows.  An input scrolls its own text, so the
     *  visible run is found by measuring prefixes rather than by dividing. */
    function seen() {
      if (!cellEdit) return null;
      const input = cellEdit.input;
      const v = input.value;
      const m = el("fzmeasure");
      m.style.font = getComputedStyle(input).font;
      const at = (i) => { m.textContent = v.slice(0, i); return m.getBoundingClientRect().width; };
      const from = input.scrollLeft, to = input.scrollLeft + input.clientWidth;
      let a = 0, b = v.length;
      while (a < v.length && at(a + 1) <= from) a++;
      while (b > a && at(b - 1) >= to) b--;
      return { key: COLS[cellEdit.at].key, value: v, chars: v.length,
               clientWidth: r2(input.clientWidth), scrollWidth: r2(input.scrollWidth),
               visible: b - a, text: v.slice(a, b),
               cut: r2(input.scrollWidth - input.clientWidth) };
    }

    /** A RESIZE IS A RE-FIT, and under D it is the ONLY one a reader can cause
     *  without changing the query: the pinned px do not stretch, so the title
     *  would keep a width the window no longer has.  Every other tab needs
     *  nothing here — `table-layout:fixed' hands the slack to the fill column
     *  on its own. */
    addEventListener("resize", () => {
      if (cellEdit) return;             // an open editor is not a layout event
      if (mode === "always") { fitColumns(); renderRows(); }
      else { widths = null; renderRows(); }
    });

    window.RIG_TEST = {
      geom: geom,
      /** The view change, driven without a key — what `q' does. */
      changeView: () => changeView(),
      seen: seen,
      state: () => ({ mode, pinned: !!pin, point: point, col: COLS[col].key,
                      editing: cellEdit ? COLS[cellEdit.at].key : null,
                      drafting: !!drafting, rows: rows.length, jump: jump }),
      /** The px a column's WIDTH would take under the measure as it stands —
       *  what a thaw would give, asked without thawing. */
      wouldBe: () => {
        const had = widths;
        widths = null;
        const w = colWidths();
        widths = had;
        return w.map((x, i) => ({ key: COLS[i].key,
                                  ch: Math.min(x.ch, COL_MAX_CH), ground: x.ground }));
      },
      ruler: (on) => { ruler = !!on; drawRuler(); },
    };

    build();
    if (mode === "always") fitColumns();   // D: the measure, once, at mount
    drawFoot();
  }
})();
