/* THE RIG every variant of this spike mounts.  The surfaces, the fixture, the
   column measure, the widget's own code and the measurements — all of it here,
   so a tab differs from its neighbour by ONE `look' field and nothing else.
   Whatever two tabs disagree about is therefore exactly what the spike asks:
   WHERE THE BOX IS ANCHORED AND WHAT SHAPE IT TAKES.

   THE WIDGET IS NOT REIMPLEMENTED.  `#ddate' is the markup of
   `src-web/Glance/Web/Page.hs:46'-`:50', dressed by `pane.css''s transcription
   of `assets/page.css', and driven by `dateMoved' / `drawGhost' / `fitCh' /
   `paintOffers' / `menuWalk' / `menuTake' / `dateKey' out of
   `frontend/glue/20-sheet.js', bodies verbatim.  The GRAMMAR is `dates.js'.
   What is left for the rig to own is the two surfaces under the box, the
   PLACEMENT, and the numbers.

   TAB 0 IS NOT A DRAWING OF THE PANE.  Its `place' branch is `placeEdit''s
   `tight' branch line for line (`20-sheet.js:546'), and TAB A's is the same
   function's `under' branch (`:520') — which is the working tree's own answer
   to this spike's question, so A is the CONTROL and not a proposal.

   Nothing in here is production code.  The fixture is invented.  `file://', no
   build step, no modules. */
const RIG = (function () {
  "use strict";
  const D = DATES;

  /* THE WIDGET'S MARKUP, character for character out of `Page.hs:46'-`:50'.
     The comment there is the reason it carries no label: *"THE DATE WIDGET
     STANDS IN THE VALUE'S OWN SLOT, and carries no label of its own: the row it
     is laid over already names the keyword."*  A and B reuse this string; so do
     C and D, which is half of what this spike found out. */
  const DDATE_HTML =
    '<div id="ddate">'
    + '<input id="dwhen" spellcheck="false" autocomplete="off" autocapitalize="off"'
    + ' autocorrect="off" placeholder="2026-08-18 · today · +3d · 18 aug">'
    + '<span id="dghost" class="dgh" aria-hidden="true"></span>'
    + '<div id="dwoffer"></div>'
    + '</div>';

  // ---- the fixture --------------------------------------------------------
  // The 2026-09-12-date-cell spike's six rows, carried whole so the two spikes
  // read as one table: DEADLINE filled on three of them, and a row that carries
  // neither, so the empty slot is on screen with the full ones.
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
    { id: "r7", state: "TODO", priority: "B", title: "Buy the rail pass",
      tag: "trip:travel", scheduled: "2026-09-26", deadline: "2026-10-05" },
    { id: "r8", state: "NEXT", priority: "C", title: "Copy the insurance card",
      tag: "trip:admin", scheduled: "2026-09-19", deadline: "" },
    { id: "r9", state: "TODO", priority: "", title: "Learn twenty words",
      tag: "trip", scheduled: "2026-09-13", deadline: "2026-10-01" },
    { id: "r10", state: "NEXT", priority: "B", title: "Cancel the milk",
      tag: "trip:admin", scheduled: "2026-09-29", deadline: "" },
    { id: "r11", state: "TODO", priority: "A", title: "Charge the power bank",
      tag: "trip:gear", scheduled: "2026-09-30", deadline: "2026-09-30" },
    { id: "r12", state: "DONE", priority: "", title: "Renew the card",
      tag: "trip:admin", scheduled: "", deadline: "2026-09-08" },
    { id: "r13", state: "NEXT", priority: "B", title: "Print the itinerary",
      tag: "trip", scheduled: "2026-10-01", deadline: "" },
    { id: "r14", state: "TODO", priority: "C", title: "Water the plants, ask Ana",
      tag: "trip:admin", scheduled: "2026-09-28", deadline: "2026-09-29" },
    { id: "r15", state: "TODO", priority: "B", title: "Pick up the yen",
      tag: "trip:admin", scheduled: "2026-09-27", deadline: "" },
    { id: "r16", state: "NEXT", priority: "A", title: "Confirm the ryokan",
      tag: "trip:travel", scheduled: "2026-09-16", deadline: "2026-09-22" },
    { id: "r17", state: "TODO", priority: "", title: "Download the maps",
      tag: "trip:gear", scheduled: "", deadline: "" },
    /* THE LAST ROW, and it is the one every fifth screenshot is about: a box
       that hangs under THIS cell has the footer and nothing else beneath it. */
    { id: "r18", state: "TODO", priority: "C", title: "Set the out-of-office",
      tag: "trip:admin", scheduled: "2026-10-02", deadline: "2026-10-02" },
  ];

  /* THE APPLIED QUERY, chosen for the WIDTH it produces.  Under
     `sort:scheduled->deadline' BOTH date columns carry a header mark, so both
     measure `calc(13ch + 24px)' — ten characters of `isoStamp' plus three of
     `▲¹ ' plus the cell padding (`colWidths', table-view.js:2988).  A query
     that sorted on one of them would have left the other three characters
     narrower and the comparison muddy. */
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

  /* THE DOCUMENT TAB 0 DRAWS: the fixture's first row as org spells it, so the
     pane's planning line holds the very entry the table's cell holds.  Two
     entries on one line, which is what makes the pane's box interesting — the
     shipped box runs from the value's left edge to the END OF THE LINE'S OWN
     BLOCK, so it covers the entry beside it. */
  const DOC = [
    { cls: "d-head", mark: "*", text: "Renew the passport", tag: ":trip:admin:" },
    { cls: "d-plan", plan: [["SCHEDULED", "<2026-09-15 Tue>"],
                            ["DEADLINE", "<2026-09-30 Wed>"]] },
    { cls: "d-body", text: "The old one runs out in November; the consulate wants" },
    { cls: "d-body", text: "six weeks and the photo shop is shut on Mondays." },
    { cls: "d-head", mark: "**", text: "Book the appointment", tag: ":admin:" },
    { cls: "d-plan", plan: [["SCHEDULED", "<2026-09-18 Fri>"]] },
    { cls: "d-body", text: "Slots open at 08:00 and are gone by 08:03." },
    { cls: "d-body", text: "" },
    { cls: "d-head", mark: "**", text: "Photograph, 35×45mm", tag: "" },
    { cls: "d-body", text: "No smile, no glasses, plain background." },
    { cls: "d-body", text: "The shop on Rue Cler does them while you wait." },
    { cls: "d-body", text: "" },
    { cls: "d-head", mark: "*", text: "Book the Kyoto flights", tag: ":trip:travel:" },
    { cls: "d-body", text: "Two legs, and the second one is the one that sells out." },
    { cls: "d-body", text: "" },
    { cls: "d-head", mark: "**", text: "Watch the fare", tag: "" },
    { cls: "d-body", text: "It moved 40% in a week last spring." },
    { cls: "d-body", text: "" },
    { cls: "d-head", mark: "**", text: "Hold the seats", tag: ":travel:" },
    { cls: "d-body", text: "The hold is 24 hours and it is free." },
    // THE LAST PLANNING LINE, at the pane's bottom edge on purpose: it is where
    // a box that hangs downward runs out of room, and the pane's own answer to
    // that is the fifth shot of tab 0.
    { cls: "d-plan", plan: [["SCHEDULED", "<2026-10-02 Fri>"],
                            ["DEADLINE", "<2026-10-04 Sun>"]] },
    { cls: "d-body", text: "" },
  ];

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
  function pill(into, text, token) {
    if (!text) return;
    const n = part(into, "span", "tv-pill", text);
    n.style.setProperty("--tv-badge", "var(" + token + ")");
  }
  const trimmed = (s) => String(s == null ? "" : s).trim();
  const px = (n) => Math.round(n) + "px";
  const round = (n) => Math.round(n);

  return { mount: mount };

  // ==========================================================================
  function mount(opts) {
    /* THE ONE FIELD THAT DIFFERS BETWEEN TABS.
       `place'   — "pane" | "under" | "over" | "card" | "beside"
       `surface' — "pane" | "table"
       `dress'   — the class the open puts on the box (`dateShape', :1040) */
    const look = opts.look;
    let rows = FIXTURE.map((r) => Object.assign({}, r));
    let point = 0;                 // the row (or doc line) the cursor stands on
    let col = DATE_COLS[0];        // the cell the cursor stands on, within it
    let open = null;               // { id, key, stood, anchor, refused }
    let told = "";
    let wire = "";
    let chord = false;             // a `C-c' waiting for its second half

    /* ONE CLOCK READ, pinned at mount (docs/invariants.md, "One clock read per
       request"), and `?day=' fixes it so a screenshot and a measurement are the
       same every run.  In the app this is `openDateBox''s own `today:
       dateNow()' (`20-sheet.js:1028', `36-date-cell.js:36') — stamped at the
       summon and spent by the ghost, the offers and the wall alike. */
    const TODAY = (function () {
      const said = new URLSearchParams(location.search).get("day");
      const m = said && /^(\d{4})-(\d{2})-(\d{2})$/.exec(said);
      return m ? { y: +m[1], m: +m[2], d: +m[3] } : D.dateNow();
    })();

    if (opts.look.surface === "pane") point = DOC.findIndex((l) => !!l.plan);

    // THE BOX HANGS AT THE PAGE'S ROOT, which is where the app hangs it: it is
    // `position:fixed' and placed against the viewport, so no surface's
    // `overflow' owns it (`page.css:752'; `placeEdit' fixed, `20-sheet.js:495').
    document.body.insertAdjacentHTML("beforeend", DDATE_HTML);
    const box = el("ddate"), field = el("dwhen"), ghost = el("dghost");

    // ---- the surfaces -------------------------------------------------------
    function draw() {
      if (look.surface === "pane") drawDoc(); else drawTable();
      keepInView();
      drawFoot();
    }
    /** POINT STAYS ON SCREEN, the app's own rule on every walk (`keepInView',
     * 20-sheet.js:76).  `block:"nearest"' is what leaves the last row AT THE
     * BOTTOM EDGE rather than centred, which is the case the flip is for. */
    function keepInView() {
      const at = look.surface === "pane"
        ? document.querySelectorAll("#dlist > .de")[point]
        : document.querySelector(`tr[data-id="${rows[point].id}"]`);
      if (at && at.scrollIntoView) at.scrollIntoView({ block: "nearest" });
    }

    function drawTable() {
      el("filter").value = QUERY;
      const chips = el("chips");
      chips.textContent = "";
      for (const t of QUERY.split(/\s+/))
        part(chips, "span", "tv-chip" + (/^sort:/.test(t) ? " cx-lends" : ""), t);
      const wrap = el("tablewrap");
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
      rows.forEach((r, i) => {
        const tr = part(tb, "tr", [i % 2 ? "tv-alt" : "",
                                   !open && point === i ? "tv-sel" : ""]
                                  .filter(Boolean).join(" "));
        tr.dataset.id = r.id;
        for (const c of COLS) {
          const td = part(tr, "td", c.kind === "date" ? "cd-date" : null);
          td.dataset.key = c.key;
          drawValue(td, r, c);
          if (!open && point === i && c.key === col) td.classList.add("cd-at");
        }
      });
    }
    function drawValue(td, r, c) {
      if (c.kind === "badge") {
        pill(td, c.key === "priority" && r.priority ? "[#" + r.priority + "]"
                 : r[c.key],
             (c.key === "priority" ? PRIO_INK[r.priority] : STATE_INK[r.state])
               || "--g-mute");
        return;
      }
      if (c.key === "tag") { part(td, "span", "tv-tag", r.tag ? ":" + r.tag + ":" : ""); return; }
      part(td, "span", null, r[c.key] || "");
    }

    /* THE DOC PANE, drawn the way `Doc.elm' draws it: a `.de' row per line, the
       planning line a `.dp' block holding `.dk' keywords and `.dpv' VALUE SLOTS
       (`Doc.elm:1897'), which is the rect the shipped box anchors to
       (`dPlanAt', 20-sheet.js:782). */
    function drawDoc() {
      const list = el("dlist");
      list.textContent = "";
      DOC.forEach((line, i) => {
        const de = part(list, "div", "de " + line.cls
                        + (!open && point === i ? " dat" : ""));
        de.dataset.at = String(i);
        const dp = part(de, "div", "dp");
        if (line.plan) { drawPlan(dp, line.plan, i); return; }
        if (line.mark) {
          part(dp, "span", "dm", line.mark + " ");
          part(dp, "span", "dstate", "TODO ");
          part(dp, "span", "dtitle", line.text);
          if (line.tag) part(dp, "span", "dc-tags", "  " + line.tag);
          return;
        }
        part(dp, "span", null, line.text || "​");
      });
    }
    function drawPlan(dp, plan, at) {
      plan.forEach((pair, i) => {
        if (i) dp.appendChild(document.createTextNode(" "));
        const dk = part(dp, "span", "dk");
        part(dk, "span", null, pair[0]);
        part(dk, "span", "dpunc", ":");
        dp.appendChild(document.createTextNode(" "));
        const slot = part(dp, "span",
                          "dpv" + (!open && point === at && planAt === i ? " dat" : ""),
                          pair[1]);
        slot.dataset.key = pair[0];
      });
    }
    // Which entry of the planning line point stands on; the pane walks entries
    // where the table walks columns, and both feed one `anchorRect'.
    let planAt = 0;
    // WHICH LINES CARRY A PLANNING ENTRY, and which one the box is about: the
    // OPEN's own line while it stands, point's while it does not.
    const planLines = () => DOC.map((l, i) => (l.plan ? i : -1)).filter((i) => i >= 0);
    const planLine = () => (open ? open.line : point);
    const nearestPlan = () => {
      const at = planLines();
      return at.filter((i) => i <= point).pop() ?? at[0];
    };

    // ---- the widget, transcribed --------------------------------------------
    /* `paintOffers' (20-sheet.js:879), `menuPaint' / `menuWalk' / `menuTake'
       (`:891'-`:910'), `fitCh' (`:940'), `drawGhost' (`:973') and `dateMoved'
       (`:987') — bodies verbatim, with `dmenu' and the pair box's half left
       behind because no pair box stands here. */
    const atIn = (list, i) => Math.max(0, Math.min(i, list.length - 1));
    const wmenu = { box: "dwoffer", list: [], at: -1 };   // `-1' is point on NO offer
    function paintOffers(boxId, list, at) {
      const b = el(boxId);
      b.textContent = "";
      b.className = list.length ? "on" : "";
      list.forEach((o, i) => {
        const row = part(b, "div", i === at ? "dof dat" : "dof");
        part(row, "span", "dow", o.word);
        if (o.hint) part(row, "span", "dot", o.hint);
      });
    }
    const menuPaint = (m) => {
      if (m.at >= m.list.length) m.at = m.list.length - 1;
      paintOffers(m.box, m.list, m.at);
    };
    const menuWalk = (m, step) => {
      if (!m.list.length) return;
      m.at = atIn(m.list, m.at + step);
      paintOffers(m.box, m.list, m.at);
    };
    /** Take M's offer into FIELD; MOVED redraws it, since the take fires no input. */
    const menuTake = (m, id, moved) => {
      const want = m.at < 0 ? undefined : m.list[m.at].word;
      const f = el(id);
      if (want === undefined || want === f.value.trim()) return false;
      f.value = want;
      f.setSelectionRange(want.length, want.length);
      moved();
      return true;
    };
    /** Size F to LEN plus PLUS characters (`ch'), never past CAP, only on change. */
    const fitCh = (f, len, plus, cap) => {
      const w = `${Math.min(cap === undefined ? Infinity : cap, len + (plus || 0))}ch`;
      if (f.style.width !== w) f.style.width = w;
    };
    const GHOST_CAP = 46;
    function drawGhost(read) {
      const said = D.dateGhost(field.value, TODAY, read);
      ghost.className = said.bad ? "dgh bad" : "dgh";
      ghost.textContent = said.text;
      field.style.flex = "none";
      fitCh(field, Math.max(1, field.value.length), 1, GHOST_CAP);
    }
    /** Redraw the ghost and the offers.  ONE READING PER KEYSTROKE, handed to
     * both (`dateMoved', 20-sheet.js:987).
     *
     * THE ONE LINE THE SHIPPED `dateMoved' DOES NOT HAVE IS THE `place()'.  The
     * pane's box is the line's remainder and never changes width, so nothing
     * re-lays it per keystroke; a box that SHRINK-WRAPS ITS FIELD grows as the
     * phrase does, and over a cell it must be measured again or its far edge
     * walks off the screen unclamped.  Call it a fourth thing the table adds. */
    function dateMoved() {
      const typed = field.value.trim();
      const r = D.readsDate(typed, TODAY);
      wmenu.at = typed ? 0 : -1;
      wmenu.list = D.dateOffers(typed, TODAY, r);
      menuPaint(wmenu);
      drawGhost(r);
      place();
      drawFoot();
    }

    // ---- the placement, which is the whole question --------------------------
    /** WHAT THE BOX IS ANCHORED TO.  In the app this is `o.rect' — `dPlanAt' for
     * the pane (a `.dpv' element) and `() => table.cellRect(id, at)' for a cell
     * (a bare rect the widget hands over, `table-view.js:4103').  `rectOf'
     * takes either (`20-sheet.js:489'). */
    function anchorEl() {
      if (look.surface === "pane") {
        const line = el("dlist").children[planLine()];
        return line ? line.querySelectorAll(".dpv")[open ? open.slot : planAt] : null;
      }
      const tr = document.querySelector(`tr[data-id="${open ? open.id : rows[point].id}"]`);
      return tr ? tr.querySelector(`td[data-key="${open ? open.key : col}"]`) : null;
    }
    const anchorRect = () => {
      const a = anchorEl();
      return a ? a.getBoundingClientRect() : null;
    };

    const EDGE = 8;   // what a box laid against the viewport keeps clear of it
    // A FIXED BOX MEASURES AGAINST THE VIEWPORT, which is the origin it is
    // placed in (`viewRect', 20-sheet.js:495).
    const viewRect = () =>
      ({ top: 0, left: 0, right: window.innerWidth, bottom: window.innerHeight });

    /** Return the ROW's vertical padding, measured once (`rowPads', `:553'). */
    function rowPads(row) {
      const c = getComputedStyle(row);
      return [parseFloat(c.paddingTop) || 0, parseFloat(c.paddingBottom) || 0];
    }
    /** The height the offers claim under (or over) the box, 0 while shut. */
    const offersHeight = () => {
      const o = el("dwoffer");
      return o.className.indexOf("on") === -1 ? 0
        : Math.round(o.getBoundingClientRect().height);
    };

    function place() {
      if (!open) return;
      const a = anchorRect();
      if (!a) return;
      const s = box.style;
      box.classList.remove("flipped");
      if (look.place === "pane") return placePane(s, a);
      s.top = s.left = s.width = s.height = s.minWidth = "";
      const b = viewRect();
      if (look.place === "under") return placeUnder(s, a, b);
      if (look.place === "over") return placeOver(s, a, b);
      if (look.place === "card") return placeCard(s, a, b);
      return placeBeside(s, a, b);
    }

    /* 0 — THE PANE, AS SHIPPED.  `placeEdit''s `tight' branch, line for line
       (`20-sheet.js:546'): the ROW vouches for the vertical — an empty slot's
       rect has no height — and the box runs from the VALUE'S LEFT EDGE to the
       end of the slot's own block, which for a two-entry planning line means it
       covers the entry beside it.  The pane lifts its row wash while the box
       stands (`page.css:803'), which is the `tight' class on `#mdoc'. */
    function placePane(s, a) {
      // `dateShape' sets `fixed' UNCONDITIONALLY (`20-sheet.js:1037'), so even
      // the pane's box is placed against the viewport: `b.top' and `b.left' are
      // zero and drop out of every line below.
      const slot = anchorEl();
      const row = slot.closest(".de");
      const [padT, padB] = rowPads(row);
      const rr = row.getBoundingClientRect();
      s.top = px(rr.top + padT);
      s.height = px(rr.height - padT - padB);
      const stop = slot.parentElement.getBoundingClientRect().right;
      s.left = px(a.left);
      s.width = px(stop - a.left);
      s.minWidth = "";
    }

    /* A — THE PANE'S BOX, DROPPED UNDER THE CELL.  `placeEdit''s `under' branch,
       line for line (`20-sheet.js:520') — which is the WORKING TREE'S OWN
       ANSWER and therefore this spike's control: *"A CELL'S BOX HANGS UNDER THE
       CELL, left-aligned and no narrower than it, so the value it is about
       stays on screen above it.  It shrink-wraps its own field, and the near
       edge gives way where the far one would run off."*

       THE ONE THING ADDED IS THE FLIP.  The tree clamps the box HORIZONTALLY
       and says nothing about the bottom, so on the last visible row the box and
       its six offers hang past the viewport with no wall to stop them. */
    function placeUnder(s, a, b) {
      s.height = "";
      s.width = "";
      s.minWidth = px(a.width);
      const wide = box.offsetWidth || a.width;
      s.left = px(Math.max(b.left + EDGE, Math.min(a.left, b.right - wide - EDGE)));
      const tall = box.offsetHeight + offersHeight();
      if (a.bottom + tall + EDGE <= b.bottom) { s.top = px(a.bottom); return; }
      // THE FLIP: the stack goes above the cell, offers above the box, so the
      // cell the reader is editing is still the last thing under their eye.
      box.classList.add("flipped");
      s.top = px(a.top - box.offsetHeight);
    }

    /* B — IN PLACE OVER THE CELL.  The same box, given the cell's own top, left
       and HEIGHT, so the field replaces the cell's text where it stood; the
       ghost keeps running right past the cell's edge over the neighbour, which
       the shrink-wrap gives for nothing.  Offers drop below as they always do. */
    function placeOver(s, a, b) {
      s.height = px(a.height);
      s.width = "";
      s.minWidth = px(a.width);
      const wide = box.offsetWidth || a.width;
      s.left = px(Math.max(b.left + EDGE, Math.min(a.left, b.right - wide - EDGE)));
      s.top = px(a.top);
      // Only the OFFERS can want the flip here: the box itself is the cell.
      if (a.bottom + offersHeight() + EDGE > b.bottom) box.classList.add("flipped");
    }

    /* C — A CARD BELOW THE CELL: the field on the first line, `→ <stamp>' on the
       second, the offers as chips on the third, at about two and a half cells
       wide.  The box is the same `#ddate'; what it costs is six forked rules and
       the offers' hint column (see `pane.css'). */
    const CARD_CELLS = 2.5;
    function placeCard(s, a, b) {
      s.width = px(a.width * CARD_CELLS);
      s.minWidth = "";
      s.height = "";
      s.left = px(Math.max(b.left + EDGE,
                           Math.min(a.left, b.right - a.width * CARD_CELLS - EDGE)));
      const tall = box.offsetHeight;
      if (a.bottom + tall + EDGE <= b.bottom) { s.top = px(a.bottom); return; }
      box.classList.add("flipped");
      s.top = px(a.top - tall);
    }

    /* D — BESIDE THE CELL, to the LEFT, because the date columns sit at the
       right edge of a wide table and the left is where the room is.  Field and
       ghost on one line — which is the box's own shape, unbent — offers below,
       right-aligned so they hang under the box rather than off it, and a
       pointer at the cell. */
    const GAP = 6;
    function placeBeside(s, a, b) {
      s.minWidth = px(a.width);
      s.width = "";
      s.height = "";
      const w = box.offsetWidth, h = box.offsetHeight;
      const want = a.left - w - GAP;
      s.left = px(Math.max(b.left + EDGE, want));
      s.top = px(Math.max(b.top + EDGE, a.top + (a.height - h) / 2));
      if (a.top + (a.height + h) / 2 + offersHeight() + EDGE > b.bottom)
        box.classList.add("flipped");
    }

    // ---- the widget's laws ---------------------------------------------------
    /** `RET' OVER A DATE CELL — or over the planning entry in the pane — OPENS
     * THE BOX ON THE VALUE IT FINDS, WHOLLY SELECTED (`openDateBox', :1060;
     * `selectWhole', 00-core.js:160), which is org-read-date's own default: one
     * keystroke replaces the whole of it and `RET' with none recommits it.
     *
     * TWO SPELLINGS, ONE VALUE.  The pane opens on the planning line's own
     * `<2026-09-15 Tue>' and the cell on `isoStamp''s `2026-09-15'
     * (`cellValue', 36-date-cell.js:12) — which is why the ghost is SILENT at
     * the pane's entry and SPEAKS at the cell's, the 2026-09-12-date-cell
     * spike's finding 2, and it is pinned here on every tab. */
    function openDate(id, key) {
      if (open) return;
      let stood, slot = 0, line = -1;
      if (look.surface === "pane") {
        line = nearestPlan();
        if (line === undefined) return;
        const plan = DOC[line].plan;
        slot = plan.findIndex((p) => p[0] === key.toUpperCase());
        if (slot < 0) return;
        stood = plan[slot][1];
        point = line;
        planAt = slot;
      } else {
        const r = rows.find((x) => x.id === id);
        if (!r) return;
        stood = r[key] || "";
      }
      open = { id, key, slot, line, stood, refused: "" };
      wire = "";
      // THE DRESS IS THE OPEN'S: one box over two surfaces wears what the
      // surface it stands over asks for (`dateShape', 20-sheet.js:1036).
      box.className = look.dress ? "on " + look.dress : "on";
      if (look.place === "pane") el("mdoc").classList.add("tight");
      draw();
      field.value = stood;
      dateMoved();
      place();
      field.focus();
      field.setSelectionRange(0, field.value.length);
      // `DATE_FOOT' — the one sentence every date surface says (15-dates.js:363).
      say(key.toUpperCase() + " · RET sets it · empty clears it · ESC leaves"
          + " · S-<left>/S-<right> a day · S-<up>/S-<down> a week");
      drawFoot();
    }
    /** `RET' INSIDE THE BOX: THE OFFER THAT STANDS, else the wall and the open's
     * own commit (`dateKey', 20-sheet.js:1070).  `datePassed' says the reader's
     * own word and posts nothing where no reading takes the phrase. */
    function dateKey() {
      if (menuTake(wmenu, "dwhen", dateMoved)) { drawFoot(); return; }
      const typed = trimmed(field.value);
      if (!typed) { clearDate(); return; }
      const r = D.readsDate(typed, TODAY);
      // A PHRASE NO READING TAKES REFUSES IN PLACE, the box standing where it
      // can be fixed (`36-date-cell.js:52'; `pairRefused', 20-sheet.js).
      if (!r.ok) {
        open.refused = r.why;
        say("RET refused · " + r.why + " · the box stays open");
        field.setSelectionRange(field.value.length, field.value.length);
        drawFoot();
        return;
      }
      commitDate(typed, r);
    }
    /** THE PHRASE TRAVELS AND THE SERVER RESOLVES IT (AGENTS.hs; `commitDate',
     * 20-sheet.js:1092 — *"Send TYPED verbatim: ONE CLOCK READ, the server's
     * own"*).  The rig prints both spellings because the reader sees both: the
     * wire's phrase, and the ISO day the settle brings back to the cell. */
    function commitDate(typed, r) {
      const key = open.key;
      wire = 'set-planning {"keyword": "' + key.toUpperCase()
        + '", "date": ' + JSON.stringify(typed) + "}";
      if (look.surface === "pane") DOC[open.line].plan[open.slot][1] = r.stamp;
      else (rows.find((x) => x.id === open.id) || {})[key] = D.isoDay(r.start);
      say("set · the phrase travels · the settle brings " + JSON.stringify(r.stamp));
      shut();
    }
    /** EMPTY CLEARS IT, the shipped foot's own promise kept verbatim. */
    function clearDate() {
      const key = open.key;
      wire = 'set-planning {"keyword": "' + key.toUpperCase() + '", "date": ""}';
      if (look.surface === "pane") DOC[open.line].plan.splice(open.slot, 1);
      else (rows.find((x) => x.id === open.id) || {})[key] = "";
      say("cleared · the entry comes off");
      shut();
    }
    /** `ESC' CANCELS THE INPUT WHOLE and the value comes back byte for byte —
     * the spelling the edit FOUND, never the one it was given. */
    function cancelDate() {
      wire = "";
      say("ESC · " + JSON.stringify(open.stood) + " is back, byte for byte");
      shut();
    }
    function shut() {
      box.className = "";
      el("dwoffer").className = "";
      if (look.place === "pane") el("mdoc").classList.remove("tight");
      field.blur();
      open = null;
      draw();
    }
    /** `S-<arrows>': THE STEPPED STAMP INTO THE FIELD, a day either way and a
     * week either way (`dateAdjust', 20-sheet.js:1106; `dateStepInto',
     * 15-dates.js:341).  Setting `value' fires no `input', so the caller
     * redraws its own ghost. */
    function stepDate(by) {
      const r = D.readsDate(trimmed(field.value), TODAY);
      if (!r.ok || !r.start) { say("no date here to move"); drawFoot(); return; }
      field.value = D.dateStepped(r, D.addDays(r.start, by));
      field.setSelectionRange(field.value.length, field.value.length);
      open.refused = "";
      say((by > 0 ? "+" : "") + by + "d · " + field.value);
      dateMoved();
    }

    // ---- keys ---------------------------------------------------------------
    const nameOf = (e) => {
      const k = e.key;
      if (k === "Enter") return e.shiftKey ? "S-RET" : "RET";
      if (k === "Escape") return "ESC";
      if (k === "Tab") return e.shiftKey ? "S-TAB" : "TAB";
      if (k.indexOf("Arrow") === 0)
        return (e.shiftKey ? "S-" : "") + "<" + k.slice(5).toLowerCase() + ">";
      return k;
    };
    // `walkStep' (00-core.js): the offers walk on the vertical pair, and the
    // arrows alias it — the movement vocabulary's own axis.
    const walkStep = (k) =>
      k === "<down>" || k === "n" ? 1 : k === "<up>" || k === "p" ? -1 : 0;

    /* A KEY INSIDE THE OPEN BOX.  The field takes its own keys and the page's
       map never sees them, which is why `n' inside the box types an `n' — and
       why the offers' walk is bound to the ARROWS here rather than to `n'/`p'
       (`20-sheet.js:1421': the sheet's own gate reads `ddating()' first, and
       the field swallows the letters). */
    field.addEventListener("keydown", (e) => {
      e.stopPropagation();
      const k = nameOf(e);
      const by = D.dateStep(k);
      if (by) { e.preventDefault(); stepDate(by); return; }
      const walk = k === "<down>" ? 1 : k === "<up>" ? -1 : 0;
      if (walk) { e.preventDefault(); menuWalk(wmenu, walk); return; }
      if (k === "RET") { e.preventDefault(); dateKey(); return; }
      if (k === "ESC") { e.preventDefault(); cancelDate(); return; }
      // `TAB' INSIDE THE BOX: the offer first, then the surface's own ring — and
      // a box with no ring swallows the key (`dateTab', 20-sheet.js:1078).
      if (k === "TAB") {
        e.preventDefault();
        if (!menuTake(wmenu, "dwhen", dateMoved)) say("TAB · no offer stands");
        drawFoot();
      } else if (k === "S-TAB") e.preventDefault();
    });
    field.addEventListener("input", () => {
      if (!open) return;
      open.refused = "";
      dateMoved();
    });

    document.addEventListener("keydown", (e) => {
      if (e.defaultPrevented || open) return;
      const k = nameOf(e);
      if (e.ctrlKey && e.key === "c") {
        e.preventDefault(); chord = true; say("C-c —"); drawFoot();
        setTimeout(() => { chord = false; }, 2000);
        return;
      }
      // THE APP'S OWN TWO, org's own spelling, over the row at point
      // (`Keymap.hs:100', `:102'; `planKey', 36-date-cell.js:49).
      if (chord && e.ctrlKey && (e.key === "s" || e.key === "d")) {
        e.preventDefault(); chord = false;
        const key = e.key === "s" ? "scheduled" : "deadline";
        col = key;
        if (look.surface === "pane") point = nearestPlan();
        openDate(look.surface === "pane" ? "r1" : rows[point].id, key);
        return;
      }
      chord = false;
      if (look.surface === "pane") {
        const step = walkStep(k);
        if (step) {
          e.preventDefault();
          point = Math.max(0, Math.min(point + step, DOC.length - 1));
          draw();
          return;
        }
        // `f'/`b' (`l'/`h', arrows) walk WITHIN the line — here, the planning
        // line's two entries.
        if ("flbh".indexOf(k) !== -1 || k === "<right>" || k === "<left>") {
          e.preventDefault();
          const plan = DOC[point] && DOC[point].plan;
          if (!plan) { say("no entries on this line"); drawFoot(); return; }
          const n = plan.length;
          planAt = (planAt + ("fl".indexOf(k) !== -1 || k === "<right>" ? 1 : n - 1)) % n;
          draw();
          return;
        }
        if (k === "RET") {
          e.preventDefault();
          const plan = DOC[point] && DOC[point].plan;
          if (plan) openDate("r1", plan[Math.min(planAt, plan.length - 1)][0].toLowerCase());
          else say("RET · no date here");
          drawFoot();
          return;
        }
      } else {
        const step = walkStep(k) || (k === "j" ? 1 : k === "k" ? -1 : 0);
        if (step) {
          e.preventDefault();
          point = Math.max(0, Math.min(point + step, rows.length - 1));
          draw();
          return;
        }
        // `l'/`h' and the arrows alias `f'/`b' on every surface glance draws
        // (docs/design-rhymes.md, "Movement is two axes").  With two date
        // columns the two directions are one toggle; both spellings are kept so
        // the rhyme is driven rather than described.
        if ("flbh".indexOf(k) !== -1 || k === "<right>" || k === "<left>") {
          e.preventDefault();
          col = DATE_COLS[(DATE_COLS.indexOf(col) + 1) % 2];
          draw();
          return;
        }
        if (k === "RET") { e.preventDefault(); openDate(rows[point].id, col); return; }
      }
      if (k === "~") {
        e.preventDefault();
        const now = document.documentElement.getAttribute("data-theme");
        document.documentElement.setAttribute("data-theme", now === "dark" ? "light" : "dark");
      }
    });
    window.addEventListener("resize", place);

    // ---- the measurements ----------------------------------------------------
    /** How wide a run WANTS to be, measured in the BOX's own font off a span
     * nothing can see.  A run is clipped by exactly what this exceeds. */
    function wants(text) {
      const m = el("cdmeasure");
      if (!m) return 0;
      const cs = getComputedStyle(box);
      m.style.fontFamily = cs.fontFamily;
      m.style.fontSize = cs.fontSize;
      m.textContent = text;
      return round(m.getBoundingClientRect().width);
    }
    /** THE ANCHOR'S OWN ROOM: the column, and the text run inside its padding. */
    function room() {
      const td = anchorEl() || document.querySelector("td.cd-date");
      if (!td || !td.getBoundingClientRect) return { col: 0, text: 0, h: 0 };
      const cs = getComputedStyle(td);
      const r = td.getBoundingClientRect();
      return { col: round(r.width), h: round(r.height),
               text: round(r.width - parseFloat(cs.paddingLeft)
                           - parseFloat(cs.paddingRight)) };
    }
    /** THE FOOTPRINT: the box, its offers, and the union of the two — which is
     * what the reader's eye has to take in and what the table has to give up. */
    function footprint() {
      if (!open) return null;
      const b = box.getBoundingClientRect();
      const o = el("dwoffer");
      const on = o.className.indexOf("on") !== -1;
      const orect = on ? o.getBoundingClientRect() : null;
      const u = {
        left: orect ? Math.min(b.left, orect.left) : b.left,
        right: orect ? Math.max(b.right, orect.right) : b.right,
        top: orect ? Math.min(b.top, orect.top) : b.top,
        bottom: orect ? Math.max(b.bottom, orect.bottom) : b.bottom,
      };
      return {
        box: { w: round(b.width), h: round(b.height),
               left: round(b.left), top: round(b.top) },
        offers: orect ? { w: round(orect.width), h: round(orect.height) } : null,
        all: { w: round(u.right - u.left), h: round(u.bottom - u.top),
               left: round(u.left), top: round(u.top),
               right: round(u.right), bottom: round(u.bottom) },
        flipped: box.className.indexOf("flipped") !== -1,
      };
    }
    /** HOW MUCH OF THE SURFACE THE BOX COVERS, and WHAT it covers: the fraction
     * of the drawn rows' area the footprint hides, how many rows tall it is,
     * and whether the EDITED ROW'S OWN TITLE is under it — the one cell the
     * reader needs to know which row they are dating. */
    function covers() {
      const f = footprint();
      if (!f) return null;
      const body = look.surface === "pane"
        ? el("dlist") : document.querySelector("#tablewrap tbody");
      if (!body) return null;
      const g = body.getBoundingClientRect();
      const hit = (r) => Math.max(0, Math.min(f.all.right, r.right) - Math.max(f.all.left, r.left))
                       * Math.max(0, Math.min(f.all.bottom, r.bottom) - Math.max(f.all.top, r.top));
      const rowH = look.surface === "pane"
        ? (el("dlist").firstChild ? el("dlist").firstChild.getBoundingClientRect().height : 21)
        : (body.firstChild ? body.firstChild.getBoundingClientRect().height : 29);
      const titleTd = look.surface === "pane" ? null
        : document.querySelector(`tr[data-id="${open.id}"] td[data-key="title"]`);
      /** How many pixels of R's WIDTH the footprint takes, where it takes any of
       * R's height at all.  Under 2px is the seam between two columns. */
      const across = (r) => {
        const w = Math.min(f.all.right, r.right) - Math.max(f.all.left, r.left);
        const h = Math.min(f.all.bottom, r.bottom) - Math.max(f.all.top, r.top);
        return w >= 2 && h >= 2 ? round(w) : 0;
      };
      const area = Math.max(1, g.width * g.height);
      return {
        rowsArea: round(g.width) + "×" + round(g.height),
        hidden: round(hit(g)),
        share: Math.round((hit(g) / area) * 1000) / 10,
        rowsTall: Math.round((f.all.h / rowH) * 10) / 10,
        rowH: round(rowH),
        title: titleTd ? across(titleTd.getBoundingClientRect()) : 0,
        cells: Math.round((f.all.w / Math.max(1, room().col)) * 10) / 10,
      };
    }
    /** WHAT RUNS OFF, and off WHAT.  The box is `position:fixed', so no pane's
     * `overflow' can cut it — the four walls are the viewport's. */
    function clip() {
      const f = footprint();
      if (!f) return null;
      const v = viewRect();
      return {
        top: Math.max(0, round(v.top - f.all.top)),
        left: Math.max(0, round(v.left - f.all.left)),
        right: Math.max(0, round(f.all.right - v.right)),
        bottom: Math.max(0, round(f.all.bottom - v.bottom)),
        view: window.innerWidth + "×" + window.innerHeight,
      };
    }
    /** DOES THE WIDEST THING FIT?  The widest ghost the grammar can draw is a
     * range's, and the widest word the offers can hold is `september'. */
    function fits() {
      const r = room();
      const wide = wants(" → <2026-08-18 Tue>--<2026-08-19 Wed>");
      const one = wants(" → <2026-08-18 Tue>");
      const sept = wants("18 september");
      const f = footprint();
      const o = el("dwoffer");
      return {
        cell: r.col, cellText: r.text,
        ghostOne: one, ghostRange: wide, september: sept,
        boxNow: f ? f.box.w : 0,
        offersW: f && f.offers ? f.offers.w : 0,
        offersMin: round(parseFloat(getComputedStyle(o).minWidth)),
      };
    }
    function metrics() {
      const r = room();
      if (!open) return "the anchor is " + r.col + "px wide, its text run " + r.text + "px";
      const f = footprint(), c = covers(), x = clip();
      return "box " + f.box.w + "×" + f.box.h
        + (f.offers ? " + offers " + f.offers.w + "×" + f.offers.h : "")
        + " → " + f.all.w + "×" + f.all.h + "px"
        + " · " + c.cells + " cells wide, " + c.rowsTall + " rows tall"
        + " · covers " + c.share + "% of the rows"
        + (c.title ? ", TITLE included" : "")
        + (f.flipped ? " · FLIPPED above" : "")
        + (x.bottom || x.right || x.left
           ? " · RUNS OFF by " + [x.left && "left " + x.left,
                                  x.right && "right " + x.right,
                                  x.bottom && "bottom " + x.bottom]
             .filter(Boolean).join(", ")
           : " · inside the viewport");
    }

    function drawFoot() {
      el("state").textContent = open
        ? "editing " + open.key + " · " + JSON.stringify(field.value)
        : look.surface === "pane" ? "doc line " + (point + 1) + " · entry " + (planAt + 1)
        : "row " + (point + 1) + " · " + col;
      el("truth").textContent = told;
      el("wire").textContent = wire;
      el("align").textContent = metrics();
    }
    const say = (s) => { told = s; };

    draw();
    // The hooks `shots.mjs' drives the pages through; no variant reads them.
    window.RIG_TEST = {
      openDate, step: stepDate, place, day: () => TODAY,
      state: () => ({
        open: !!open, key: open && open.key, phrase: open ? field.value : null,
        refused: open ? open.refused : "", told, wire, point, col,
        ghost: ghost.textContent, ghostBad: ghost.className.indexOf("bad") !== -1,
        dress: box.className,
        offers: wmenu.list.map((o) => o.word), at: wmenu.at,
        drawn: [...el("dwoffer").querySelectorAll(".dof")]
          .map((n) => n.textContent),
        rows: rows.map((r) => ({ id: r.id, scheduled: r.scheduled, deadline: r.deadline })),
        plans: planLines(),
        plan: DOC[planLines()[0]].plan.map((p) => p.slice()),
      }),
      caret: () => ({ value: field.value, start: field.selectionStart,
                      end: field.selectionEnd }),
      footprint, covers, clip, fits, room,
      /** THE GHOST AS DRAWN against the ghost as WANTED.  A box that shrink-wraps
       * its field never cuts it; a box given a width does. */
      ghostCut: () => {
        const drew = round(ghost.getBoundingClientRect().width);
        const want = wants(ghost.textContent);
        return { text: ghost.textContent, drew, want,
                 cut: Math.max(0, want - drew) };
      },
      /** WHAT THE FLIP IS WORTH: how far past the viewport's bottom edge the
       * box and its offers WOULD hang if they were left to hang downward, which
       * is what the working tree does today — `placeEdit''s `under' branch
       * clamps the horizontal and says nothing at all about the vertical
       * (`20-sheet.js:520'). */
      unflipped: () => {
        const a = anchorRect();
        if (!a || !open) return null;
        // WHAT EACH VARIANT HANGS BELOW ITS ANCHOR.  B's box IS the cell, so
        // only its offers hang; C's offers are in the card and counted once.
        const hangs = look.place === "over" ? offersHeight()
          : look.place === "card" ? box.offsetHeight
          : box.offsetHeight + offersHeight();
        const room = window.innerHeight - a.bottom;
        return { hangs: round(hangs + EDGE), room: round(room),
                 over: Math.max(0, round(hangs + EDGE - room)),
                 view: window.innerHeight };
      },
      /** Every row's top, so a claim that NOTHING MOVED is a number.  An overlay
       * that moved a row would be an overlay pretending to be a strip. */
      geom: () => {
        const tops = {};
        const sel = look.surface === "pane" ? "#dlist > .de" : "#tablewrap tbody tr[data-id]";
        [...document.querySelectorAll(sel)].forEach((n, i) => {
          tops[n.dataset.id || ("line" + i)] = round(n.getBoundingClientRect().top);
        });
        return tops;
      },
    };
  }
})();
