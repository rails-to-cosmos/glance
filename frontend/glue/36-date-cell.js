    // THE DATE IN THE CELL.  A DATE IS EDITED WHERE IT IS DRAWN: with point on a
    // SCHEDULED or DEADLINE cell `RET' opens the widget's own in-cell editor over
    // the day that cell holds, the reading rides the STRIP under that row, and the
    // PHRASE is what travels -- the server resolves it once, against its own clock
    // (docs/invariants.md).  A BARE FRAGMENT of the one script scope: the grammar
    // is `15-dates.js''s, the road is the one `commitDate' and `planRows' take,
    // and the column cursor is the table's own.  Rules in AGENTS.hs.

    /** THE COLUMNS A DATE IS DRAWN IN, and the planning keyword each names.  ONE
     * LIST: the mount's `editableKeys', the cell editor's door and the `C-c C-s'
     * split all read it. */
    const DATE_CELLS = [["scheduled", "SCHEDULED"], ["deadline", "DEADLINE"]];
    const DATE_KEYS = DATE_CELLS.map((pair) => pair[0]);
    const dateCell = (key) => DATE_KEYS.indexOf(key) !== -1;
    const planKeyword = (key) => (DATE_CELLS.find((p) => p[0] === key) || ["", ""])[1];
    /** The command the cell commits under, `planCommand''s own naming (20-sheet.js). */
    const cellBinding = (key, seq) =>
      ({ seq: seq || "RET", command: planCommand(planKeyword(key)) });

    /** The day the open cell reads against, stamped when the editor ENTERED it and
     * spent on INK alone.  ONE CLOCK READ PER OPEN: a repaint re-enters the same
     * cell and keeps the day it opened on, so the ghost, the step and the wall
     * above the commit cannot disagree mid-edit; only a move to another cell reads
     * the clock again.
     * @type {{id: string|null, key: string, today: {y: number, m: number, d: number}} | null} */
    let dateCellOn = null;
    function dateCellDay(cell) {
      if (!dateCellOn || dateCellOn.id !== cell.id || dateCellOn.key !== cell.key)
        dateCellOn = { id: cell.id, key: cell.key, today: dateNow() };
      return dateCellOn.today;
    }

    /** ISO's ten characters as the stamp they name, or `""'.  A cell draws
     * `isoStamp''s `2026-09-15' and the ghost spells org's `<2026-09-15 Tue>', so
     * the two are compared in ONE spelling rather than as text. */
    const cellStamp = (value) => {
      const c = dayOf(String(value || "").trim());
      return c ? stampOf(c, null, false, "") : "";
    };
    /** The value the CELL holds: the store's for a standing row, the draft's own
     * for the phantom, whose cells never reach the store. */
    const cellHeld = (cell) => {
      const row = cell.id === DRAFT_ID && drafting ? drafting : rowOf(cell.id);
      return String(((row || {}).cells || {})[cell.key] || "");
    };

    /** WHAT THE STRIP SAYS UNDER AN OPEN DATE CELL: the phrase with the stamp it
     * resolves to riding after it, `✗' and the reader's own refusal word where no
     * reading takes it, and NOTHING while the phrase names the day the cell
     * already holds -- the pane's own silence, read against the cell's ISO
     * spelling.  Every other column answers nothing at all, so the draft's plain
     * cells draw no strip. */
    function dateCellNote(e, cell) {
      if (!dateCell(cell.key)) return null;
      const typed = String(cell.value || "").trim();
      if (!typed) return null;
      const today = dateCellDay(cell);
      const r = readsDate(typed, today);
      if (r.ok && r.stamp && r.stamp === cellStamp(cellHeld(cell))) return null;
      const said = dateGhost(typed, today, r);
      if (!said.text) return null;
      // THE MARK LEADS: the widget dresses a strip opening on `✗' as refused.
      return said.bad ? said.text.trim() : typed + said.text;
    }

    /** A KEY INSIDE AN OPEN DATE CELL, asked ahead of the draft's own reading of
     * it (`onCellKey', assets/table-view.js).  The STEP belongs to every date
     * cell, the draft's among them; `RET' over a STANDING row commits that one
     * date, while a draft's `RET' belongs to the capture and falls through to it. */
    function dateCellKey(e, cell) {
      if (!dateCell(cell.key)) return false;
      const key = keyName(e);
      if (!key) return false;
      const by = dateStep(key);
      if (by) { e.preventDefault(); dateCellStep(e, cell, by); return true; }
      if (key !== "RET" || cell.id === DRAFT_ID) return false;
      return commitDateCell(e, cell);
    }

    /** `S-<arrows>': THE STEPPED STAMP INTO THE FIELD, a day either way and a week
     * either way, which is what the pane's own walk writes (`dateAdjust',
     * 20-sheet.js).  The reader sees it and it travels because it is what stands
     * there.  Setting `value' fires no `input', so the widget is told the way a
     * keystroke tells it and the strip redraws through its own seam. */
    function dateCellStep(e, cell, by) {
      const f = targetOf(e);
      const r = readsDate(String(f.value).trim(), dateCellDay(cell));
      if (!r.ok || !r.start) { said(cellBinding(cell.key), "no date here to move"); return; }
      f.value = dateStepped(r, addDays(r.start, by));
      f.setSelectionRange(f.value.length, f.value.length);
      f.dispatchEvent(new Event("input"));
    }

    /** `RET' OVER A STANDING ROW'S DATE CELL: the field's own bytes go out as
     * `set-planning', the road `commitDate' and `planRows' already take, and an
     * emptied field commits `null' -- `""' is no date and would meet the wall's
     * 400 rather than clear the entry (docs/invariants.md).  A phrase no reading
     * takes REFUSES IN PLACE: the editor stands, the strip wears the mark, the
     * pill carries the reader's own word and nothing is posted.  On a commit the
     * key is handed BACK to the widget, whose close redraws the cell off the
     * store -- this editor paints no stamp and the settle brings the ISO day. */
    function commitDateCell(e, cell) {
      const b = cellBinding(cell.key);
      const typed = String(cell.value || "").trim();
      if (typed) {
        const r = readsDate(typed, dateCellDay(cell));
        if (!r.ok) { e.preventDefault(); said(b, r.why); return true; }
      }
      fire(b, "set-planning", [cell.id],
           { keyword: planKeyword(cell.key), date: typed || null },
           typed || "cleared");
      return false;
    }

    /** ONE `onCellKey' FOR THE TABLE: a date cell's keys first, the draft's under
     * them.  A draft's date cell takes the step from the one and `TAB', `RET' and
     * `ESC' from the other -- ONE EDITOR, two readings of the same press. */
    const cellKey = (e, cell) => dateCellKey(e, cell) || draftKey(e, cell);

    /** The editor over ID's cell in COL, where that column draws a date: whether
     * it opened.  The column cursor is the table's own (`getSelection().col',
     * `f'/`b' and their aliases), so `RET' over a date column has a referent
     * already and no cursor is built here. */
    function openDateCell(id, at) {
      const c = at === null || at === undefined ? null : cols[at];
      if (!c || !dateCell(c.key)) return false;
      if (!(can(table, "editCell") && table.editCell(id, at))) return false;
      // The shipped foot, kept verbatim (`summonPlan', 20-sheet.js).
      said(cellBinding(c.key), "RET sets it · empty clears it · ESC leaves");
      return true;
    }
    /** `RET' IN THE TABLE IS COLUMN-SENSITIVE, the way `^' is: over a date column
     * it opens that cell's editor, and over every other column it materializes. */
    const dateCellAt = (id) => openDateCell(id, column());

    /** `C-c C-s' / `C-c C-d': the PROMPT over a MARKED set, the CELL over the row
     * at point.  `targets()' splits exactly there (00-core.js), so which rows the
     * key takes is unchanged and only the surface differs: a set of rows has no
     * cell to stand in, and one row then has ONE date surface. */
    function planKey(b, keyword) {
      const marked = marking() ? table.getMarked() : [];
      const id = marked.length ? null : focusedId();
      if (id && openPlanCell(id, keyword)) return;
      planRows(b, keyword);
    }
    /** The editor over ID's own KEYWORD column, the column cursor moved onto it
     * first: the key names the column, so the reader is left standing in it. */
    function openPlanCell(id, keyword) {
      const at = cols.findIndex((c) => planKeyword(c.key) === keyword);
      if (at === -1 || !can(table, "select", "editCell")) return false;
      table.select(id, at);
      return openDateCell(id, at);
    }
