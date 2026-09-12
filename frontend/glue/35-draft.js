    // THE DRAFT ROW.  `+' types a capture into the table already on screen: a
    // phantom row spliced below the row at point, seeded from the standing
    // filter, with its title cell open.  Rules in AGENTS.hs.

    // THE APPLIED QUERY'S OWN PREDICATES, or none where there is no query and no
    // renderer to read one.  EVERY seeding below reads this one parse.
    const filterTerms = () =>
      (query && typeof TableView.parseQuery === "function"
        ? TableView.parseQuery(query, cols.map((c) => c.key)) : []);
    /** A FACT THE FILTER PINS TO ONE CONCRETE POSITIVE VALUE, or `""'.  A
     * negated predicate, a WIDENING (an alternative, never a facet every shown
     * row carries), an alternation and a meta (`*active*') each describe a SET
     * of rows rather than a value a capture could wear. */
    const pinned = (t) =>
      !t.negated && !t.added && t.value
      && !t.value.includes("|") && !/^\*.*\*$/.test(t.value);
    const pinnedTo = (key) => filterTerms().filter((t) => t.key === key && pinned(t));
    // EVERY tag the applied query names, in the order it names them.
    const filteredTags = () => pinnedTo("tag").map((t) => t.value);
    /** THE ONE ORDINARY POSITIVE VALUE the filter pins KEY to, or `""'.  Named
     * ONCE is the whole rule: two `state:' predicates describe a union, and a
     * capture inherits from a filter only what that filter leaves no choice
     * about. */
    const soleValue = (key) => {
      const hits = pinnedTo(key);
      return hits.length === 1 ? String(hits[0].value) : "";
    };
    /** WHAT THE FILTER SEEDS A DRAFT ROW WITH: the FIRST positive `tag:' is the
     * destination, every later one rides as the draft's own, and each scalar the
     * filter pins once is worn as it stands.  A day is not among them — the
     * SCHEDULED cell carries the destination hint instead (AGENTS.hs). */
    function draftSeed() {
      const tags = filteredTags();
      return {
        dest: tags[0] || "",
        tags: tags.slice(1),
        state: soleValue("state").toUpperCase(),
        priority: soleValue("priority").replace(/^\[#(.)\]$/, "$1").toUpperCase(),
      };
    }
    // The draft's tag run as org spells one, the destination leading it.
    const draftTags = (list) => {
      const run = list.filter(Boolean);
      return run.length ? `:${run.join(":")}:` : "";
    };
    /** THE DESTINATION, SAID IN THE ROW: `→ book' mints a blob under that layer
     * and `→ inbox' appends to the inbox.  It rides the SCHEDULED cell, the one
     * column a capture never fills — the title cell cannot hold it, since
     * `openCellEditor' empties the cell it opens in.  DROPPED names a seeded
     * state the destination's own cycle does not have. */
    const draftHint = (dest, dropped) =>
      `→ ${dest || "inbox"}${dropped ? ` · ${dropped} dropped` : ""}`;

    // A ROW ID NO STORE ANSWERS: an id is a uuid or a path, and neither spells a
    // space.  A draft is never a target, so this id never reaches a `/command'.
    const DRAFT_ID = "· draft";
    /** THE DRAFT AS THE PAGE HOLDS IT: the row it stands under, its destination,
     * the keyword that destination's cycle dropped, the refusal it is standing
     * on, and the cells themselves -- which the drawn row shares by reference.
     * @type {{under: string|null, dest: string, dropped: string, refused: string,
     *         cells: Record<string, string>} | null} */
    let drafting = null;
    const colAt = (key) => cols.findIndex((c) => c.key === key);

    /** ROWS with the draft spliced back under the row it was opened beneath, or
     * at the head where that row is gone.  `setRows' resets the widget's rows,
     * so a draft left out of the splice is erased — and suppressing the paint
     * instead would leave a stale table under a live draft. */
    function withDraft(rows) {
      if (!drafting) return rows;
      const out = rows.filter((r) => !r.draft);
      const at = out.findIndex((r) => r.id === drafting.under);
      out.splice(at + 1, 0, { id: DRAFT_ID, draft: true, cells: drafting.cells });
      return out;
    }
    const draftInput = () =>
      /** @type {any} */
      (document.querySelector("#app tr.tv-draft input.tv-cell-edit"));
    /** Which of the draft's cells the open editor stands in, or -1.  READ OFF
     * THE DOM because the widget owns that state and answers no question about
     * it; the box cell is stepped over, the columns starting after it. */
    function draftCol() {
      const box = draftInput();
      const tr = box && box.closest("tr");
      if (!tr) return -1;
      const tds = [...tr.querySelectorAll("td:not(.tv-box)")];
      return tds.indexOf(box.closest("td"));
    }
    /** EVERY PAINT GOES THROUGH HERE: the draft is re-spliced, the line the
     * reader had typed is carried into its cell first — `setRows' rebuilds the
     * tbody and the open input goes with it — and the editor is put back where
     * it stood, a draft always carrying one. */
    function paintRows(rows) {
      if (!drafting) { table.setRows(rows); return; }
      const at = draftCol(), box = draftInput();
      if (at !== -1 && cols[at]) drafting.cells[cols[at].key] = box.value;
      table.setRows(withDraft(rows));
      if (at !== -1 && can(table, "editCell")) table.editCell(DRAFT_ID, at);
      dressDraft();
    }

    /** `+': A DRAFT ROW UNDER THE ROW AT POINT, wearing what the filter pins,
     * with the title cell's editor open.  The open input takes every key it
     * sees, so an editor-less draft would be a row with no id, no span and no
     * file that the movement keys could stand on.  The destination's cycle is
     * asked for in the same breath and lands behind the row. */
    function openDraft(b) {
      if (drafting) { openDraftCell("title"); said(b, "the draft is up"); return; }
      if (!can(table, "setRows", "getRows")) { said(b, lacks("row splicing")); return; }
      const seed = draftSeed();
      drafting = {
        under: focusedId(),
        dest: seed.dest,
        dropped: "",
        refused: "",
        cells: { state: seed.state, priority: seed.priority, title: "",
                 tag: draftTags([seed.dest].concat(seed.tags)),
                 scheduled: draftHint(seed.dest, ""), deadline: "" },
      };
      paintRows(table.getRows().filter((r) => !r.draft));
      openDraftCell("title");
      askCycle(seed.dest, seed.state);
      said(b, draftHint(seed.dest, ""));
    }
    /** The editor over one of the draft's cells; false where this view draws no
     * such column, the draft being the rows' shape and not the view's. */
    function openDraftCell(key) {
      const at = colAt(key);
      const opened = at !== -1 && can(table, "editCell")
        && table.editCell(DRAFT_ID, at);
      // `editCell' REDRAWS THE ROWS on its way in, taking the dress with them.
      dressDraft();
      return opened;
    }
    const cycleWords = (sources) =>
      (sources || []).reduce((all, s) =>
        all.concat(s.active || [], s.inactive || []), []);
    /** THE DESTINATION'S OWN `#+TODO:' CYCLE, asked at the moment the row is
     * drawn.  A seeded state the cycle lacks is DROPPED before the wire ever
     * carries it and the hint says so, which leaves `stated''s 400 exactly as
     * strict as it is for every other caller. */
    function askCycle(dest, state) {
      getJSON(`/capture${dest ? `?tag=${encodeURIComponent(dest)}` : ""}`)
        .then((a) => {
          if (!drafting || drafting.dest !== dest || !state) return;
          if (cycleWords(a.cycle).indexOf(state) !== -1) return;
          drafting.cells.state = "";
          drafting.dropped = state;
          drafting.cells.scheduled = draftHint(dest, state);
          drawDraftCells(["state", "scheduled"]);
        })
        .catch((e) => append("cmd", "error", `capture failed: ${e.message}`));
    }
    /** The named cells written STRAIGHT INTO THE DRAWN ROW: a repaint here would
     * destroy the very input the reader is typing into and take the caret with
     * it, so the cycle's answer lands in place. */
    function drawDraftCells(keys) {
      const tr = document.querySelector("#app tr.tv-draft");
      if (!tr) return;
      const tds = [...tr.querySelectorAll("td:not(.tv-box)")];
      for (const key of keys) {
        const td = tds[colAt(key)];
        if (td) td.textContent = drafting.cells[key];
      }
    }

    // THE WALK.  `openCellEditor' STOPS PROPAGATION, so no key typed in a cell
    // reaches the shell's dispatch: a draft's keys can be bound nowhere but the
    // editor, and `onCellKey' (assets/table-view.js) is that seam.

    /** THE CELLS THE WALK VISITS, in the order `TAB' takes them; `S-TAB' is the
     * same ring the other way.  The SCHEDULED cell is not among them — it
     * carries the destination hint, which is the capture's address rather than
     * one of the facts it wears. */
    const DRAFT_WALK = ["title", "state", "priority", "tag"];

    /** Does KEY put a character into the open box or take one out?  A refusal's
     * note is the reader's to CLEAR BY TYPING, so a walk and a movement leave it
     * standing and the word survives the trip to the cell that needs fixing. */
    const contentKey = (key) =>
      key === "SPC" || key === "DEL" || key === "<delete>" || key.length === 1;

    /** A KEY INSIDE AN OPEN CELL, asked before the widget's own reading of it; a
     * `true' answer says this glue took it.  OVER THE DRAFT ALONE: `TAB'/`S-TAB'
     * walk the ring, `RET' commits and `ESC' drops the row.  Every other row
     * keeps the shipped reading, which costs nothing while no other row is
     * editable. */
    function draftKey(e, cell) {
      if (!drafting || cell.id !== DRAFT_ID) return false;
      // NAMED THE WAY EVERY OTHER LISTENER NAMES A KEY: the raw event is read in
      // `keyName' and nowhere else, so `S-TAB' is a name here rather than a flag.
      const key = keyName(e);
      if (!key) return false;
      // THE READER IS ANSWERING THE REFUSAL, so the row stops standing on it.
      if (drafting.refused && contentKey(key)) clearRefusal();
      if (key === "TAB" || key === "S-TAB") {
        e.preventDefault();
        walkDraft(cell, key === "TAB" ? 1 : -1);
        return true;
      }
      if (key === "RET") { e.preventDefault(); commitDraft(cell); return true; }
      // `ESC' DROPS THE ROW AND LEAVES THE CLOSE TO THE WIDGET, whose own
      // reading of the key is exactly that: taking the key here would strand the
      // widget holding the editor the splice has already unparented.
      if (key === "ESC") dropDraft();
      return false;
    }

    /** ONE STEP OF THE WALK: the CLOSING cell's value into the phantom row, then
     * the next cell opens.  `editCell' closes the standing editor first and that
     * close redraws the rows, so a value written after it is drawn gone — and
     * the row's value is what the next editor opens on, so a cell walked through
     * untouched keeps what it held. */
    function walkDraft(cell, step) {
      const from = cols[cell.col];
      const at = from ? DRAFT_WALK.indexOf(from.key) : -1;
      if (at === -1) { openDraftCell(DRAFT_WALK[0]); return; }
      drafting.cells[from.key] = cell.value;
      openDraftCell(DRAFT_WALK[(at + step + DRAFT_WALK.length) % DRAFT_WALK.length]);
    }

    /** `ESC': THE WHOLE DRAFT GOES.  No file was written, so nothing is put back
     * — the phantom is spliced out and the count is the count it was. */
    function dropDraft() {
      drafting = null;
      if (can(table, "setRows", "getRows"))
        table.setRows(table.getRows().filter((r) => !r.draft));
    }

    // THE COMMIT.  `RET' from ANY cell sends the whole capture through the one
    // command that mints a blob; the draft is committed or it never was.

    /** `RET' OVER A DRAFT FINALIZES A CAPTURE, which is the verb org-capture
     * spells rather than any row-write; no binding is added for it, the key
     * belonging to the editor and reaching no dispatch. */
    const FINALIZE = docBinding("org-capture-finalize");

    /** WHAT A ROW CAN CARRY AND NO MORE, as the capture command's own args.  The
     * DESTINATION rides as `tag' — it is the capture's address, `→ book' minting
     * a blob under that layer and `→ inbox' appending to the inbox — and the
     * row's whole run rides as `tags', the destination leading it.  A ROW HAS NO
     * BODY, NO DRAWER AND NO PLANNING LINE, so the widened cargo's other three
     * keys are absent.  THE STATE IS ALREADY THE DESTINATION'S OWN: `askCycle'
     * cleared a keyword that cycle lacks, so the wire carries none `stated'
     * would refuse. */
    function draftArgs(title, c, dest) {
      const args = { title };
      if (dest) args.tag = dest;
      const state = String(c.state || "").trim();
      if (state) args.state = state;
      const priority = priorityIn(c.priority);
      if (priority) args.priority = priority;
      const tags = cellTags(c.tag);
      if (tags.length) args.tags = tags;
      return args;
    }

    /** `RET' FROM ANY CELL: THE WHOLE CAPTURE AT ONE PRESS.  The OPEN editor's
     * value is folded in first — the walk accumulates and posts nothing, so the
     * cell the reader stands in has not reached the row yet.  POINT FOLLOWS THE
     * ROW THE SERVER PLACES: the id the command answers is spent by `arrived' on
     * the settle that carries the row, and the re-query is asked for at once so
     * the fresh row arrives where `sort:' puts it rather than where it was typed. */
    function commitDraft(cell) {
      const from = cols[cell.col];
      if (from && DRAFT_WALK.indexOf(from.key) !== -1)
        drafting.cells[from.key] = cell.value;
      const c = drafting.cells, dest = drafting.dest;
      const title = String(c.title || "").trim();
      if (!title) { refuseDraft("nothing to capture"); return; }
      postCommand({ name: "capture", args: draftArgs(title, c, dest) })
        .then((a) => {
          arriving = a.id || null;
          dropDraft();
          fetchRows(settled);
          said(FINALIZE, dest ? `captured · :${dest}:` : `captured · ${a.file}`);
          append("cmd", "info",
                 `headline ${JSON.stringify(title)} captured into ${a.file}`);
        })
        .catch((e) => {
          refuseDraft(e.message);
          append("cmd", "error", `capture failed: ${e.message}`);
        });
    }

    /** A REFUSAL KEEPS THE DRAFT STANDING — a row that cannot commit is a row the
     * reader would otherwise have to retype.  The word takes the hint's place
     * beside the row, the editor goes back to the title with its text selected,
     * and the dress turns `--g-warn'.  Only `ESC' dismisses the draft; the next
     * content keystroke takes the note and the dress back. */
    function refuseDraft(why) {
      drafting.refused = why;
      drafting.cells.scheduled = why;
      openDraftCell("title");
      said(FINALIZE, why);
    }
    function clearRefusal() {
      drafting.refused = "";
      drafting.cells.scheduled = draftHint(drafting.dest, drafting.dropped);
      // IN PLACE, never through a paint: a redraw here would destroy the very
      // input the reader is answering into and take the caret with it.
      drawDraftCells(["scheduled"]);
      dressDraft();
    }
    /** THE REFUSAL'S OWN DRESS, STAMPED ON THE DRAWN ROW: every redraw rebuilds
     * the tbody, so the class is put back after each rather than set once.  Hue
     * is no channel of its own — the WORD stands in the hint beside it. */
    function dressDraft() {
      const tr = document.querySelector("#app tr.tv-draft");
      if (tr) tr.classList.toggle("g-refused", !!(drafting && drafting.refused));
    }
