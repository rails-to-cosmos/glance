    // THE DATE IN THE CELL: `RET' opens the editor where the date is DRAWN, the
    // reading rides the STRIP under that row, and the PHRASE travels (AGENTS.hs).

    /** The command the cell commits under, `planCommand''s own naming (20-sheet.js). */
    const cellBinding = (key) => ({ seq: "RET", command: planCommand(planKeyword(key)) });

    /** THE DAY THE OPEN CELL READS AGAINST, stamped when the editor opened and
     * spent on INK alone.  ONE CLOCK READ PER OPEN (docs/invariants.md): the ghost,
     * the step and the wall above the commit cannot disagree mid-edit, and a
     * repaint's re-open carries the same `token' and keeps the day with it.
     * @type {{token: number, day: {y: number, m: number, d: number}} | null} */
    let cellOn = null;
    function cellDay(cell) {
      if (!cellOn || cellOn.token !== cell.token)
        cellOn = { token: cell.token, day: dateNow() };
      return cellOn.day;
    }

    /** WHAT THE STRIP SAYS UNDER AN OPEN DATE CELL: the phrase with the stamp it
     * resolves to riding after it, the reader's own refusal word where no reading
     * takes it, and NOTHING while the phrase names the day THE EDITOR OPENED ON --
     * the pane's own silence, read against the cell's ISO spelling.  A range and a
     * timed day never match those ten characters and always speak.  Every other
     * column answers nothing at all, so a plain cell's strip stays empty. */
    function dateCellNote(cell) {
      if (!dateCell(cell.key)) return null;
      const typed = String(cell.value || "").trim();
      if (!typed) return null;
      const today = cellDay(cell);
      const r = readsDate(typed, today);
      if (r.ok && !r.end && r.start && isoDay(r.start) === cell.raw) return null;
      const said = dateGhost(typed, today, r);
      // THE PHRASE LEADS ITS OWN READING; a refusal is the mark and the word, the
      // ghost's leading space being the ink of the field it rides after.
      return said.bad ? { text: said.text.trim(), bad: true }
                      : { text: said.text && typed + said.text, bad: false };
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
      const b = cellBinding(cell.key);
      const typed = datePassed(b, cell.value, cellDay(cell));
      // A PHRASE NO READING TAKES REFUSES IN PLACE, the editor standing where it
      // can be fixed; a commit HANDS THE KEY BACK, and the widget's own close
      // redraws the cell off the store.
      if (typed === null) { e.preventDefault(); return true; }
      commitDateCell(b, cell, typed);
      return false;
    }

    /** `S-<arrows>': THE STEPPED STAMP INTO THE FIELD, a day either way and a week
     * either way, which is what the pane's own walk writes (`dateAdjust',
     * 20-sheet.js).  Setting `value' fires no `input', so the widget is told the
     * way a keystroke tells it and the strip redraws through its own seam. */
    function dateCellStep(e, cell, by) {
      const f = targetOf(e);
      const r = readsDate(String(f.value).trim(), cellDay(cell));
      if (!dateStepInto(f, r, by)) { said(cellBinding(cell.key), NO_DATE_HERE); return; }
      f.dispatchEvent(new Event("input"));
    }

    /** `RET' OVER A STANDING ROW'S DATE CELL: the field's own bytes out through
     * `set-planning''s one road, an emptied field clearing the entry. */
    const commitDateCell = (b, cell, typed) =>
      firePlanning(b, [cell.id], planKeyword(cell.key), typed);

    /** ONE `onCellKey' FOR THE TABLE: a date cell's keys first, the draft's under
     * them.  A draft's date cell takes the step from the one and `TAB', `RET' and
     * `ESC' from the other -- ONE EDITOR, two readings of the same press. */
    const cellKey = (e, cell) => dateCellKey(e, cell) || draftKey(e, cell);
    /** ONE `onCellInput' FOR THE TABLE, the same way: a refused DRAFT says what it
     * wants whichever cell is open, and every other cell reads its own date. */
    const cellNote = (e, cell) => draftNote(cell) || dateCellNote(cell);

    /** The editor over ID's cell in COL, where that column draws a date: whether
     * it opened.  The column cursor is the table's own (`getSelection().col',
     * `f'/`b' and their aliases), so `RET' over a date column has a referent
     * already and no cursor is built here. */
    function openDateCell(id, at) {
      const c = at === null || at === undefined ? null : cols[at];
      if (!c || !dateCell(c.key)) return false;
      if (!(can(table, "editCell") && table.editCell(id, at))) return false;
      said(cellBinding(c.key), DATE_FOOT);
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
      const id = marks().length ? null : focusedId();
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
