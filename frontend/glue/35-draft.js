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
    // A DAY THE FILTER PINS: a bare ISO or one word the server resolves.  A
    // comparison, a range or an alternation names a SPAN of days, and a planning
    // entry is one day — so those seed nothing.  READ SYNTACTICALLY: the day
    // WORDS are the server's vocabulary and this page holds no copy of them.
    const ONE_DAY = /^(?:\d{4}-\d{2}-\d{2}|[A-Za-z]+)$/;
    /** WHAT THE STANDING FILTER LENDS THE DRAFT SHEET, as the read door's own
     * args.  TEMPLATE-FIRST IS THE SERVER'S: these are what the filter leaves no
     * choice about, and the composer there fills only the silences the template
     * left.  The destination TAG rides apart, being the capture's address rather
     * than one of its facts. */
    function inherited(tag) {
      const args = [];
      const state = soleValue("state");
      if (state) args.push(["state", state]);
      const priority = soleValue("priority");
      // ORG'S OWN SPELLING IS `[#B]' and the wire takes the letter.
      if (priority) args.push(["priority", priority.replace(/^\[#(.)\]$/, "$1")]);
      // EVERY POSITIVE TAG BEYOND THE DESTINATION joins the draft's own.
      const more = filteredTags().filter((t) => t !== tag);
      if (more.length) args.push(["tags", more.join(",")]);
      for (const word of CFG.settable) {
        const day = soleValue(word.toLowerCase());
        if (day && ONE_DAY.test(day)) args.push([word.toLowerCase(), day]);
      }
      return args;
    }

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
    /** @type {{under: string|null, dest: string, cells: Record<string, string>} | null} */
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
      return at !== -1 && can(table, "editCell") && table.editCell(DRAFT_ID, at);
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
