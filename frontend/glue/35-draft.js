    // THE DRAFT ROW.  `+' types a capture into the table already on screen: a
    // PRODUCER-OWNED row the widget splices below the row at point, seeded from
    // the standing filter, with its title cell open.  The row is the widget's to
    // place, to dress and to paint (`producer', assets/table-view.js); this part
    // owns what it says and where it lands.  Rules in AGENTS.hs.

    // THE APPLIED QUERY'S OWN PREDICATES, or none where there is no query and no
    // renderer to read one.
    const filterTerms = () =>
      (query && typeof TableView.parseQuery === "function"
        ? TableView.parseQuery(query, cols.map((c) => c.key)) : []);
    /** A FACT THE FILTER PINS TO ONE CONCRETE POSITIVE VALUE.  A negated
     * predicate, a WIDENING (an alternative, never a facet every shown row
     * carries), an alternation and a meta (`*active*') each describe a SET of
     * rows rather than a value a capture could wear. */
    const pinned = (t) =>
      !t.negated && !t.added && t.value
      && !t.value.includes("|") && !/^\*.*\*$/.test(t.value);
    const pinnedTo = (key, terms) => terms.filter((t) => t.key === key && pinned(t));
    /** EVERY tag the applied query names, in the order it names them.  TERMS is
     * the parse a caller already holds; a caller with none pays for one. */
    const filteredTags = (terms) =>
      pinnedTo("tag", terms || filterTerms()).map((t) => t.value);
    /** THE ONE ORDINARY POSITIVE VALUE the filter pins KEY to, or `""'.  Named
     * ONCE is the whole rule: two `state:' predicates describe a union, and a
     * capture inherits from a filter only what that filter leaves no choice
     * about. */
    const soleValue = (key, terms) => {
      const hits = pinnedTo(key, terms);
      return hits.length === 1 ? String(hits[0].value) : "";
    };
    /** WHAT THE FILTER SEEDS A DRAFT ROW WITH: the FIRST positive `tag:' is the
     * destination, every later one rides as the draft's own, and each scalar the
     * filter pins once is worn as it stands.  A day is not among them: a date
     * cell is the reader's own to type, and `draftArgs' carries what they typed
     * (AGENTS.hs).  ONE PARSE of the query feeds every clause. */
    function draftSeed() {
      const terms = filterTerms();
      const tags = filteredTags(terms);
      return {
        dest: tags[0] || "",
        tags: tags.slice(1),
        state: soleValue("state", terms).toUpperCase(),
        priority: priorityCell(priorityIn(soleValue("priority", terms))),
      };
    }
    // The draft's tag run as org spells one, the destination leading it.
    const draftTags = (list) => {
      const run = list.filter(Boolean);
      return run.length ? `:${run.join(":")}:` : "";
    };
    /** A PRIORITY AS THE TABLE SPELLS ONE: the draft's cell reads `[#A]' the way
     * every landed row's does (`priorityCell', Query.hs), and `draftArgs' folds
     * it back to the letter the wire takes.  LETTER is `priorityIn''s answer,
     * null where the filter pinned none. */
    const priorityCell = (letter) => (letter ? `[#${letter}]` : "");

    // A ROW ID NO STORE ANSWERS: an id is a uuid or a path, and neither spells a
    // space.  A draft is never a target, so this id never reaches a `/command'.
    const DRAFT_ID = "· draft";
    /** THE DRAFT, AS ONE OBJECT: the very row the widget holds, with the page's
     * own two facts on it.  The cells, the anchor and the refusal are the row's —
     * two copies of a cell is how the drawn row and the posted capture come to
     * disagree.  `refused' DRESSES the row warn and says nothing; the WORD is
     * the echo pill's, which belongs to no cell and survives the walk.
     * @type {{id: string, producer: boolean, under: string|null,
     *         refused: string, dest: string,
     *         cells: Record<string, string>} | null} */
    let drafting = null;
    const colAt = (key) => cols.findIndex((c) => c.key === key);
    /** THE ROW REPUBLISHED: the widget draws it, so a field this page changed
     * goes back through the door that paints it — which carries the open editor
     * and its caret across the redraw. */
    const redrawDraft = () => {
      if (drafting && can(table, "upsertRow")) table.upsertRow(drafting);
    };
    /** The editor over one of the draft's cells, named by its column; false
     * where this view draws no such column, the draft being the rows' shape and
     * not the view's.  `editCell' redraws the rows on its way in, so whatever
     * the row was changed to is drawn with it. */
    const openDraftAt = (key) => {
      const at = colAt(key);
      return at !== -1 && can(table, "editCell") && table.editCell(DRAFT_ID, at);
    };
    /** THE STOP KEY OPENED.  A DATE CELL TAKES THE DATE BOX, laid over it -- the
     * same widget the material document has, offers and all -- and every other
     * cell the widget's own in-cell editor.  ONE RING either way. */
    const openDraftStop = (key) =>
      ((dateCell(key) && openDraftDate(key)) || openDraftAt(key));
    /** THE THREE KEYS A DRAFT'S DATE STOP ANSWERS, which are not the three a
     * landed row's cell answers: nothing is SET here, the whole row being
     * captured or never having been. */
    const DRAFT_DATE_FOOT =
      "RET captures · TAB resolves and walks on · ESC drops the draft";
    /** THE DRAFT'S DATE STOP.  `RET' commits the WHOLE capture (a draft has no id
     * and no span for a per-cell verb to name), `TAB' takes the offer that stands
     * and else RESOLVES the phrase and walks on at the same press, and `ESC'
     * drops the draft the way it does from every other cell.  What the field
     * holds is folded into the row before either leaves -- the stamp, where the
     * walk resolved one -- the walk accumulating and posting nothing. */
    function openDraftDate(key) {
      const at = colAt(key);
      // ASKED BEFORE THE CLOSE: a renderer that cannot answer where a cell is
      // leaves the reader in the plain editor rather than in nothing at all.
      if (at === -1 || !can(table, "cellRect", "closeEditor")
            || !table.cellRect(DRAFT_ID, at)) return false;
      // THE IN-CELL EDITOR GOES NEXT: the box is laid over the cell, so the
      // widget's own input must not be standing under it.
      table.closeEditor();
      // THE ROW IS REPUBLISHED WITH THE PHRASE IN IT: the box is no cell editor,
      // so nothing else redraws the cell it was laid over.
      const fold = (typed) => {
        drafting.cells[key] = typed;
        shutEdit(DDATE);
        redrawDraft();
      };
      openDateBox({
        rect: () => table.cellRect(DRAFT_ID, at),
        initial: String(drafting.cells[key] || ""), key: planKeyword(key),
        today: dateNow(), b: cellBinding(key), foot: DRAFT_DATE_FOOT,
        onCommit: (typed) => { fold(typed); commitDraft(null); },
        onCancel: dropDraft,
        onWalk: (step, typed) => { fold(typed); walkFrom(key, step); },
      });
      return true;
    }

    /** `+': A DRAFT ROW UNDER THE ROW AT POINT, wearing what the filter pins,
     * with the title cell's editor open.  The open input takes every key it
     * sees, so an editor-less draft would be a row with no id, no span and no
     * file that the movement keys could stand on.  The destination's cycle is
     * asked for in the same breath and lands behind the row. */
    function openDraft(b) {
      if (drafting) {
        // ONE DRAFT AT A TIME, and the editor is already in it: putting it back
        // where it stands would select the line the reader has typed.
        const at = can(table, "getEditing") && table.getEditing();
        if (!at || at.key !== "title") openDraftAt("title");
        said(b, "the draft is up");
        return;
      }
      if (!can(table, "upsertRow", "deleteRow")) { said(b, lacks("row splicing")); return; }
      const seed = draftSeed();
      drafting = {
        id: DRAFT_ID, producer: true, under: focusedId(),
        refused: "", dest: seed.dest,
        cells: { state: seed.state, priority: seed.priority, title: "",
                 tag: draftTags([seed.dest].concat(seed.tags)) },
      };
      table.upsertRow(drafting);
      openDraftAt("title");
      askCycle();
      said(b, `→ ${seed.dest || "inbox"}`);
    }
    /** THE DESTINATION'S OWN `#+TODO:' CYCLE, asked at the moment the row is
     * drawn.  A seeded state the cycle lacks is DROPPED before the wire ever
     * carries it, which leaves the commit door's 400 exactly as strict as it is
     * for every other caller.  WITH NO SEEDED STATE THERE IS NOTHING TO CHECK,
     * so the door is not knocked on. */
    function askCycle() {
      const { dest, cells } = drafting;
      const state = cells.state;
      if (!state) return;
      getJSON(`/keywords?tag=${encodeURIComponent(dest)}`)
        .then((a) => {
          if (!drafting || drafting.dest !== dest) return;
          if ((a.states || []).indexOf(state) !== -1) return;
          drafting.cells.state = "";
          redrawDraft();
        })
        .catch((e) => append("cmd", "error", `capture failed: ${e.message}`));
    }

    // THE WALK.  A draft's keys can be bound nowhere but `onCellKey'
    // (assets/table-view.js), which is where the seam and its reason are stated.

    /** THE CELLS A DRAFT OWNS.  The date columns are among them, SPLICED FROM THE
     * ONE LIST (15-dates.js): a draft's date rides out in the capture's own
     * `planning' (`draftArgs'), and the stop it is typed at is the document's own
     * date widget laid over that cell (36-date-cell.js). */
    const DRAFT_CELLS = ["title", "state", "priority"].concat(DATE_KEYS, ["tag"]);
    /** THE RING `TAB' WALKS: those of the draft's cells this view draws, IN THE
     * ORDER THE HEADER DRAWS THEM, left to right; `S-TAB' is the same ring the
     * other way.  The walk follows the eye rather than a list of its own, so a
     * view that reorders its columns reorders the walk with them. */
    const draftWalk = () =>
      cols.map((c) => c.key).filter((k) => DRAFT_CELLS.indexOf(k) !== -1);

    /** Does KEY put a character into the open box or take one out?  A refusal's
     * note is the reader's to CLEAR BY TYPING, so a walk and a movement leave it
     * standing and the word survives the trip to the cell that needs fixing. */
    const contentKey = (key) =>
      key === "SPC" || key === "DEL" || key === "<delete>" || key.length === 1;

    /** A KEY INSIDE AN OPEN CELL, asked before the widget's own reading of it; a
     * `true' answer says this glue took it.  OVER THE DRAFT ALONE: `TAB'/`S-TAB'
     * walk the ring, `RET' commits and `ESC' drops the row. */
    function draftKey(e, cell) {
      if (!drafting || cell.id !== DRAFT_ID) return false;
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
      // reading of the key is exactly that.
      if (key === "ESC") dropDraft();
      return false;
    }

    /** ONE STEP OF THE WALK: the CLOSING cell's value into the row, then the
     * next stop opens.  The row's value is what the next editor opens on, so a
     * cell walked through untouched keeps what it held. */
    function walkDraft(cell, step) {
      if (draftWalk().indexOf(cell.key) === -1) { openDraftStop(draftWalk()[0]); return; }
      drafting.cells[cell.key] = cell.value;
      walkFrom(cell.key, step);
    }
    /** ONE STEP OF THE RING FROM KEY, whose value is already in the row: what a
     * stop calls once it has folded what was typed at it into the draft. */
    function walkFrom(key, step) {
      const ring = draftWalk();
      const at = ring.indexOf(key);
      openDraftStop(at === -1 ? ring[0] : ring[(at + step + ring.length) % ring.length]);
    }

    /** `ESC': THE WHOLE DRAFT GOES.  No file was written, so nothing is put back
     * — the row is spliced out and the count is the count it was. */
    function dropDraft() {
      drafting = null;
      shutEdit(DDATE);
      if (can(table, "deleteRow")) table.deleteRow(DRAFT_ID);
    }

    // THE COMMIT.  `RET' from ANY cell sends the whole capture through the one
    // command that mints a blob; the draft is committed or it never was.

    /** `RET' OVER A DRAFT FINALIZES A CAPTURE, which is the verb org-capture
     * spells rather than any row-write; no binding is added for it, the key
     * belonging to the editor and reaching no dispatch. */
    const FINALIZE = docBinding("org-capture-finalize");

    const draftTitle = () => String(drafting.cells.title || "").trim();

    /** WHAT A ROW CAN CARRY AND NO MORE, as the capture command's own args.  The
     * DESTINATION rides as `tag' — it is the capture's address, `→ book' minting
     * a blob under that layer and `→ inbox' appending to the inbox — and the
     * row's whole run rides as `tags', the destination leading it.  A ROW HAS NO
     * BODY AND NO DRAWER, so two of the widened cargo's keys are absent; the
     * PLANNING LINE it does carry is the two date cells, each present only where
     * its cell holds something.  THE STATE IS ALREADY THE DESTINATION'S OWN:
     * `askCycle' cleared a keyword that cycle lacks, so the wire carries none the
     * commit door would refuse. */
    function draftArgs() {
      const c = drafting.cells;
      const args = { title: draftTitle() };
      if (drafting.dest) args.tag = drafting.dest;
      const state = String(c.state || "").trim();
      if (state) args.state = state;
      const priority = priorityIn(c.priority);
      if (priority) args.priority = priority;
      const tags = cellTags(c.tag);
      if (tags.length) args.tags = tags;
      const planning = draftPlanning(c);
      if (planning.length) args.planning = planning;
      return args;
    }

    /** THE DRAFT'S PLANNING LINE, in keyword order: the PHRASE each date cell
     * holds, never the stamp the ghost drew -- `plannedEntry' resolves it against
     * the request's one clock read, the way `set-planning' does
     * (docs/invariants.md).  An empty cell is no entry, and no entry is no line. */
    const draftPlanning = (c) =>
      DATE_KEYS.map((key) => [planKeyword(key), String(c[key] || "").trim()])
               .filter((pair) => pair[1]);

    /** `RET' FROM ANY CELL: THE WHOLE CAPTURE AT ONE PRESS.  The OPEN editor's
     * value is folded in first — the walk accumulates and posts nothing, so the
     * cell the reader stands in has not reached the row yet.  POINT FOLLOWS THE
     * ROW THE SERVER PLACES: the id the command answers is spent by `arrived' on
     * the settle that carries the row, and the re-query is asked for at once so
     * the fresh row arrives where `sort:' puts it rather than where it was typed. */
    function commitDraft(cell) {
      if (cell && DRAFT_CELLS.indexOf(cell.key) !== -1)
        drafting.cells[cell.key] = cell.value;
      const title = draftTitle(), dest = drafting.dest;
      if (!title) { refuseDraft("nothing to capture"); return; }
      postCommand({ name: "capture", args: draftArgs() })
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
     * reader would otherwise have to retype.  THE WORD IS THE ECHO PILL'S: the
     * refusal is about the ROW and not about the cell, so it belongs to no column
     * and survives the walk.  The editor goes back to the title with its text
     * selected and the widget dresses the row warn; only `ESC' dismisses the
     * draft, and the next content keystroke takes the dress back. */
    function refuseDraft(why) {
      drafting.refused = why;
      // THE DRESS IS THE ROW'S, so the row is REPUBLISHED for it rather than
      // left to whatever the next open happens to redraw -- a refusal from a
      // date stop closes no cell editor and so redraws nothing by itself.
      redrawDraft();
      openDraftAt("title");
      said(FINALIZE, why);
    }
    /** ONE FRAME BEHIND THE KEY that answered it: the redraw rebuilds the very
     * cell the keystroke is still landing in, so it is left to finish first. */
    function clearRefusal() {
      drafting.refused = "";
      soon(() => {
        if (!drafting || drafting.refused) return;
        redrawDraft();
      });
    }
