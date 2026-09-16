    // THE DATE IN THE CELL.  A date is edited where it is DRAWN, with THE VERY
    // WIDGET THE MATERIAL DOCUMENT HAS -- the field, the ghost, the offers, the
    // step keys and RET/ESC (`openDateBox', 20-sheet.js) -- laid over the cell
    // as an overlay.  ONE WIDGET, ONE CODE PATH: the cell cannot lack what the
    // pane has.  The PHRASE travels and the server resolves it (AGENTS.hs).

    /** The command a date cell commits under, `planCommand''s own naming. */
    const cellBinding = (key) => ({ seq: "RET", command: planCommand(planKeyword(key)) });

    /** THE VALUE ID's KEY CELL DRAWS, which for a date column is `isoStamp''s ten
     * characters and `""' where the row carries no entry. */
    const cellValue = (id, key) => {
      const r = visible().find((x) => x.id === id);
      return String((r && r.cells ? r.cells[key] : "") || "");
    };

    /** THE DATE BOX OVER ID's CELL IN COLUMN AT, where that column draws a date:
     * whether it opened.  The field opens on the cell's own ISO day wholly
     * selected -- org-read-date's own default -- and `RET' sends the field's own
     * bytes through `set-planning''s ONE road, an emptied field clearing the
     * entry.  The cell is not painted here: the wire's date cell is ISO and the
     * file's is org's stamp, so THE SETTLE BRINGS THE DAY.
     *
     * The column cursor is the table's own (`getSelection().col', `f'/`b' and
     * their aliases), so `RET' over a date column has a referent already. */
    function openDateCell(id, at) {
      const c = at === null || at === undefined ? null : cols[at];
      if (!c || !dateCell(c.key)) return false;
      if (!can(table, "cellRect", "closeEditor") || !table.cellRect(id, at)) return false;
      const keyword = planKeyword(c.key), b = cellBinding(c.key);
      table.closeEditor();
      openDateBox({
        rect: () => table.cellRect(id, at),
        initial: cellValue(id, c.key), key: keyword, b,
        // THE CELL'S OWN SPELLING for what `TAB' resolves: the ISO day this very
        // cell draws once the settle brings it, so the box says what the reader
        // will see rather than a stamp only the file wears (15-dates.js).
        today: dateNow(), foot: DATE_FOOT, spell: isoSpell,
        onCommit: (typed, k) => { shutEdit(DDATE); firePlanning(k, [id], keyword, typed); },
        onCancel: () => {},
      });
      return true;
    }
    /** `RET' IN THE TABLE IS COLUMN-SENSITIVE, the way `^' is: over a date column
     * it opens that cell's own box, and over every other column it materializes. */
    const dateCellAt = (id) => openDateCell(id, column());

    /** `C-c C-s' / `C-c C-d': the PROMPT over a MARKED set, the BOX over the row
     * at point.  `targets()' splits exactly there (00-core.js), so which rows the
     * key takes is unchanged and only the surface differs: a set of rows has no
     * cell to stand in, and one row then has ONE date surface. */
    function planKey(b, keyword) {
      const id = marks().length ? null : focusedId();
      if (id && openPlanCell(id, keyword)) return;
      planRows(b, keyword);
    }
    /** The box over ID's own KEYWORD column, the column cursor moved onto it
     * first: the key names the column, so the reader is left standing in it. */
    function openPlanCell(id, keyword) {
      const at = cols.findIndex((c) => planKeyword(c.key) === keyword);
      if (at === -1 || !can(table, "select")) return false;
      table.select(id, at);
      return openDateCell(id, at);
    }
