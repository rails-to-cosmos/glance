    let prows = [];
    let pmount = null, pseq = 0;
    // A MOUNT THIS PAGE KEEPS, made on the first ask and handed back afterwards:
    // a mount per raise would leave a theme listener behind every time the
    // reader opened a sheet or followed a row.  PANE is the scroller the edit
    // overlay is anchored inside — caught in the CAPTURE phase, which reaches it
    // without this page naming the element that scrolls; the window resizing is
    // the other half and is registered once, with `placeEdit'.  Three surfaces
    // mount this way and differ only in their host, their columns and their
    // options.
    function mountOnce(host, cols, opts, pane) {
      const m = TableView.mount(el(host), { columns: cols, rows: [] }, opts);
      el(pane).addEventListener("scroll", placeEdit, true);
      return m;
    }
    function mounted() {
      if (pmount) return pmount;
      pmount = mountOnce("mptable", PCOLS, {
        // No bar and no resident filter: five rows of a drawer are not something
        // a reader narrows, and the overlay this leaves behind is never raised.
        palette: true,
        // Flags alone: the gutter carries the flag's edge, no checkbox is
        // drawn, and nothing here reads a mark.
        flags: true,
        // The key line under the table already names every key, once.
        actionHints: false,
        flagHelp: "d/D delete · u unflag",
      }, "mprops");
      return pmount;
    }
    const prowsOf = () =>
      prows.map((r) => ({ id: r.id, cells: { key: r.key, value: r.val } }));
    // Every change to the model ends here.  AT is the row to land the cursor on
    // and is left out where it should stay where it is.
    function repaint(at) {
      const m = mounted();
      m.setRows(prowsOf());
      if (at) m.select(at);
    }
    function drawProps(list, plan) {
      mounted();
      prows = []; pseq = 0;
      shutEdit(PROW);
      el("mprops").className = "";   // and the panel gives the keys back
      const held = new Map(plan || []);
      for (const key of PLANNING)
        prows.push({ id: `PLN:${key}`, key, val: held.get(key) || "", fixed: true });
      for (const p of list)
        prows.push({ id: `P${pseq++}`, key: p[0], val: p[1], fixed: false });
      // A different drawer: these flags were about the last one.  `setRows'
      // deliberately keeps them, so taking them off is this page's to ask for.
      pmount.clearFlags();
      repaint(prows[0].id);
    }
    // Where the cursor is, in the model's terms.  The renderer's answer is the
    // one that decides; this page keeps no copy of it.
    const patAt = () => prows.findIndex((r) => r.id === selectedId(pmount));
    // The add affordance, and the whole of it: `+' puts an empty property at the
    // end of the drawer and opens it.  Keyboard-first means the KEY is the offer,
    // where a row that is always empty was chrome every reader of the panel had
    // to filter back out.  A row whose key is emptied is still a property
    // deleted, which is what `d' spells as a key press.
    function addProperty() {
      const id = `P${pseq++}`;
      prows.push({ id, key: "", val: "", fixed: false });
      repaint(id);
      openRow();
    }
    // What the panel would write: every property row carrying a key, in the
    // order they sit in.  A row whose key has been emptied is a deletion.  Both
    // fields are trimmed, because the server hands them over trimmed: what the
    // panel can show is then exactly what it can write, and a space nobody could
    // ever see again cannot be typed into a file.
    const props = () => prows
      .filter((r) => !r.fixed)
      .map((r) => [r.key.trim(), r.val.trim()])
      .filter((p) => p[0] !== "");
    // And the planning line: the fixed rows carrying a value, in org's order.
    // An empty row is that entry absent, so clearing all three is how the line
    // comes off — the server drops it rather than writing a bare keyword.
    const planning = () => prows
      .filter((r) => r.fixed && r.val.trim() !== "")
      .map((r) => [r.key, r.val.trim()]);
    // Crossing the panes, and the two modes.  NEITHER pane focuses anything in
    // the structured shape: the document holds the keys on the left and the panel
    // on the right, both with nothing focused, which leaves every printable key
    // free to be movement and a command.  `pnav' says which of the two has them;
    // `typing()' counts the whole sheet as a focus of its own (`docHolds'), so
    // the table's keys stay dead under either.  Raw mode is the exception, a
    // textarea focusing itself.
    const pnav = () => el("mprops").className === "on";
    function enterPanel() {
      el("mprops").className = "on"; el("mdoc").className = "";
      el("mtext").blur();
    }
    function leavePanel() {
      el("mprops").className = ""; el("mdoc").className = "on";
    }
    // THE PANEL'S SHAPE: a key and a value over the whole row.  A planning
    // row's key is ORG's rather than the author's (`fixed'), so its field is
    // read-only text with a caret in it and the focus opens on the VALUE —
    // as it does wherever there is a key already; only the add row, which has
    // none yet, opens on the key.
    const PROW = {
      box: "pedit", pane: "mprops", fields: ["pkey", "pval"],
      mount: () => pmount,
      fill: (r) => {
        el("pkey").value = r.key;
        el("pval").value = r.val;
        el("pkey").readOnly = r.fixed;
      },
      focus: (r) => (r.fixed || r.key ? el("pval") : el("pkey")).focus(),
    };
    const pediting = () => !!edit && edit.o === PROW;
    function openRow() {
      const at = patAt();
      if (at !== -1) openEdit(PROW, prows[at]);
    }
    // Committing: the row takes the text the fields are holding and the overlay
    // goes.  This is the one thing that can make the sheet dirty from the panel
    // — an edit nobody committed was never in `props()'.  A fixed row keeps its
    // key, which is org's rather than the author's.  The row is the one the
    // overlay OPENED over, never the one the cursor is on now.
    function commitRow() {
      const r = edit.row;
      if (!r.fixed) r.key = el("pkey").value;
      r.val = el("pval").value;
      shutEdit(PROW);
      repaint();
    }
    // ESC over an open row is the ROW's: the overlay goes and the text the row
    // is holding stands, which is the text it was opened on.  The sheet's own
    // ESC ladder therefore only ever sees the key from nav — that is why this
    // runs from the keymap's `cancel' rather than from a listener of its own.
    const cancelRow = () => cancelEdit("row", PROW);
    // DELETION IS THE TABLE'S GESTURE, over the renderer's own flags: `d' flags
    // the row at point, `d' again — or `D' — takes every flagged row, and `u'
    // takes a flag off.  One implementation of the gesture in this page, the set,
    // the wash and the count all being the mount's.
    //
    // WHAT "taken" MEANS is the row's.  A property is dropped, the emptied key
    // spelled as a key press.  A planning entry is CLEARED and its row stands:
    // the three are org's keys rather than the author's, and an empty value is
    // already how an entry is absent.
    // IDS is the set the key worked out, HOW the word the pill calls it: a caller
    // that has already found the row and read the flags does not make this look
    // for them again.  HOW is a function of what LANDED, and this deletion is
    // local and total, so it is asked about the whole set.
    function pdelete(ids, how) {
      const gone = new Set(ids);
      const cleared = prows.filter((r) => gone.has(r.id) && r.fixed);
      for (const r of cleared) r.val = "";
      prows = prows.filter((r) => r.fixed || !gone.has(r.id));
      repaint();
      // The command name is the BINDING's and the brackets carry what it did:
      // org has no one function for taking a planning entry off — it is
      // `org-schedule' or `org-deadline' under a prefix — so the line names the
      // keys it cleared rather than claiming a property function did it.
      const also = cleared.map((r) => r.key).join(", ");
      echo(`D → org-delete-property (${how(ids.length)}${also ? ` · ${also} cleared` : ""})`);
    }
    // THE SHEET'S OWN KEYS, over BOTH panes, and the ONE private listener that
    // registers AHEAD of the dispatch, so it sees a key first.
    //
    // WHY A PRIVATE LISTENER IS SAFE, said once for all of them: a surface
    // holding the keys makes `typing()' true, which kills every `table' row, so
    // the only map row that can fire around one of these is `ESC' — which is the
    // one that should, a key this does not claim falling through.  The listeners
    // BEHIND the dispatch take it from the other side; this one stands down
    // under a `momentary()'.  FOUR STATES, only ever one true: an open PANEL
    // row, an open DOCUMENT element, the panel in nav, and the document.
    //
    // TAB CROSSES THE PANES, each cursor where it was left; two stops, so both
    // directions are one toggle.  Inside an OPEN row TAB hops that row's fields,
    // suspending the crossing.  IN THE DOCUMENT the movement is the table's
    // letters exactly; RET dispatches by kind, DEL is UP, `d'/`D'/`u' are the
    // deletion gesture.  With a PARAGRAPH open the keys are the textarea's own —
    // a paragraph is text and RET a newline in it — so the commit is `C-x C-s'.
    //
    // AUTO-REPEAT IS MOVEMENT'S, and this listener owes the rule itself: running
    // AHEAD of the dispatch, the map's `ONCE' list can never reach a key of
    // this one's.  A held `n' crosses the pane; every key that WRITES delivers
    // exactly one press — a held `d' would flag and delete from one, and a held
    // `S-<up>' was a burst of 409s off a cell the answer before it had moved.
    document.addEventListener("keydown", (e) => {
      // THE SHEET STANDS DOWN UNDER A MOMENTARY.  It is the workspace, and a
      // palette or a popup raised over it — from the table or from the document
      // itself — holds the keys until it dissolves.  This listener registers
      // FIRST, so without the guard it would claim the very letter the palette
      // was raised to read.
      if (!editing || raw || momentary()) return;
      const k = keyName(e), crossing = k === "TAB" || k === "S-TAB";
      if (!k) return;
      if (dparaing()) return;   // the textarea's; C-x C-s commits and ESC restores
      const once = (act) => { if (!repeating(e)) act(); };
      if (pediting()) {
        if (crossing) hop();
        else if (k === "RET") once(commitRow);
        else return;   // ESC is the keymap's, and puts the row back
      } else if (dediting()) {
        if (crossing) hop();
        else if (k === "RET") once(commitDocEdit);
        else return;   // ESC is the keymap's, and puts the element back
      } else if (pnav()) {
        if (crossing) leavePanel();
        else if (k === "RET") once(openRow);
        else if (k === "+") addProperty();
        else if (rowStep(k)) stepIn(pmount, rowStep(k));
        else if (!flagPress(k, e, PFLAGS)) return;
      } else if (crossing) enterPanel();
      else {
        const step = rowStep(k), side = colStep(k), depth = grainStep(k);
        if (step) docStep(step);
        else if (depth > 0) docFiner(k);
        else if (depth < 0) docBroader(k);
        else if (side) moveDocCol(k, side);
        else if (k === "RET") once(docEnter);
        else if (k === "DEL") once(docUp);
        // The table's own keys, over the entry the sheet is standing on: a
        // priority is a cell of the headline line, so the ring is the same ring
        // and the command is the same command.  Refused on a child for the cells'
        // own reason.
        else if (k === "S-<up>" || k === "S-<down>")
          once(() => atElement(() => cycleHere(k === "S-<up>" ? 1 : -1)));
        // `o' at one grain finer than the table's: the ELEMENT's links rather than
        // the row's.
        else if (k === "o" || k === "!") once(openHere);
        // The two keys that SET a part rather than edit one, and they work at the
        // ELEMENT: an absent state or an absent tag is no cell to walk onto, so
        // the question is asked of the headline the sheet is standing on and never
        // of a column point.  They are the table's own keys, over one row.
        else if (k === "t") once(() => atElement(stateHere));
        else if (k === ":") once(() => atElement(tagsHere));
        // org's own toggle, at the stop: a checkbox item flips, anything else
        // says so.  In `once' — a held SPC would flip-flop the box.
        else if (k === "SPC")
          once(() => toggleCheckbox(docBinding("org-toggle-checkbox", "SPC")));
        else if (!flagPress(k, e, DFLAGS)) return;
      }
      e.preventDefault();
    });
    // DIRED'S `d', ONE implementation over THREE surfaces — the table, this panel
    // and the tags popup.  The first press flags the row at point; a second `d'
    // on a flagged row IS `D' — the same handler, so it takes EVERY flagged row;
    // `u' takes a flag off and walks on.  The flag is the confirmation, so there
    // is no prompt, and a lone flag is a set of one.
    //
    // THE CURSOR IS ASKED FOR FIRST AND THE FLAGS SECOND: `D' means "take
    // these" and a lone row is a set of one, so it lands on a mount whose
    // renderer never had flags, while the two presses that MOVE a flag are what
    // the refusal is for.
    //
    // A SHAPE says what differs: a mount, where the cursor is, what "take these"
    // means, what the surface LOGS, and FOUR PHRASES.  The feature detection,
    // the two-press rule, the set-or-row choice and the walk after `u' are the
    // gesture, and the gesture is here.
    //
    // SAY is the caller's rather than the shape's, because WHO IS SPEAKING is:
    // the popups say `KEY → phrase', the table says it through `said', which
    // spells the binding's own command name and brackets the phrase.
    //
    // HOW words the count for the pill, a FUNCTION of what LANDED: the popups'
    // takes are local and total, where the table's is a write that can come back
    // partly refused.
    function flagKey(k, s, say) {
      const m = s.mount();
      const at = s.at();
      if (at === null) { say(s.none); return; }
      const flags = flagsOn(m) ? m.getFlagged() : [];
      if (k === "D" || (k === "d" && flags.indexOf(at) !== -1)) {
        const ids = flags.length ? flags : [at];
        // The flags are SPENT before the take, on every surface: a mount keeps a
        // flag whose row is hidden — which is what makes a flag outlive the
        // repaint the take causes — so a set left standing would be taken again by
        // the next press and the row at point would never be reachable again.
        if (can(m, "clearFlags")) m.clearFlags();
        s.take(ids, flags.length ? (n) => `${n} flagged` : (n) => (n ? "row" : n));
        return;
      }
      if (!flagsOn(m)) { say(s.missing); return; }
      if (k === "u") {
        m.unflagRow(at);
        s.note(at, false);
        say(s.unflag);
        s.walk();
        return;
      }
      m.flagRow(at);
      s.note(at, true);
      say(s.flag);
    }
    // The popups have nothing to log: their rows are a property and a tag, which
    // the echo already names, where the table's are org headlines the strip
    // reports one line per.  So the hook is theirs to leave empty rather than a
    // branch inside the gesture.
    const unlogged = () => {};
    // The panel's phrases, and its cursor as an ID: `patAt' answers with an
    // INDEX, which is the panel's own currency and nothing the gesture reads.
    const PFLAGS = {
      mount: () => pmount, take: pdelete, note: unlogged,
      walk: () => stepIn(pmount, 1),
      missing: "this table-view.js has no delete flags",
      none: "org-delete-property (no row)",
      unflag: "delete-unflag (flag cleared)",
      flag: "delete-flag (d again deletes)",
      at: () => { const i = patAt(); return i === -1 ? null : prows[i].id; },
    };
    // And the document's, whose `mount' is no renderer's — four calls over a Set
    // of element ids — which makes it a fourth surface of the SAME gesture rather
    // than a second implementation: `flagKey' asks a mount for four things and
    // never what kind of mount it is.  `missing' is therefore unreachable here
    // and is still spelled, a shape leaving it out being one field short of the
    // three beside it.
    const DFLAGS = {
      mount: () => dmount, take: ddelete, note: unlogged,
      walk: () => docStep(1),
      missing: "this document has no flags",
      none: "org-delete-element (no element)",
      unflag: "delete-unflag (flag cleared)",
      flag: "delete-flag (d again deletes)",
      at: () => (drows[dat] ? drows[dat].id : null),
    };
    // How a surface with no binding in its hand speaks: the key, the arrow, and
    // the phrase whole.
    const keySaid = (k) => (what) => echo(`${k} → ${what}`);
    // THE GESTURE'S THREE KEYS AS ONE PRESS, over whichever SHAPE the surface
    // declares, and false for a key that is not one of them so a caller's chain
    // goes on past it.  The HELD-key guard is here rather than on each surface:
    // `ONCE' governs dispatch rows and these three live in listeners the
    // dispatch does not own, so a repeat that survived would flag a row and take
    // it in ONE press — which is the confirmation the two-press shape exists to
    // be.
    const flagPress = (k, e, shape) => {
      if (k !== "d" && k !== "D" && k !== "u") return false;
      if (!repeating(e)) flagKey(k, shape, keySaid(k));
      return true;
    };
    // What a flush sends: the subtree whole in raw mode, the two panes apart
    // otherwise.  The server joins them, so this page never spells a drawer.
    const asked = () => raw
      ? { org: el("mtext").value }
      : { body: bodyText(), properties: props(), planning: planning() };
    // ONE BUTTONLESS SHEET, twice over.  The subtree sheet and the settings
    // sheet are the same flow over different files — a state word, a flush, and
    // a close that syncs on the way out — so the ladder is written once and each
    // sheet supplies the verbs it differs in: `dirty', `flush', `refresh' (the
    // digests a conflict overwrites under), `shut', and the log `scope' its own
    // lines are filed under.  Never both up at once, `openSettings' refusing over
    // an open sheet, which is what makes `activeSheet' total.  Where a sheet
    // stands is ONE word and `note' is its only writer: the header wears it as
    // text and as a class, and everything that asks reads it back off the sheet.
    // With no buttons the keys are the whole of the offer, so the states that
    // wait for one say which key — and the retry line is spelled once, three
    // copies of it being three chances to drift.
    const RETRY = " — C-x C-s retry · ESC discard";
    const WORDS = { synced: "synced", syncing: "syncing…",
      conflict: "conflict — C-x C-s overwrite · ESC discard",
      error: "error" + RETRY };
    function note(s, next, message) {
      s.state = next;
      el(s.noteId).className = next;
      el(s.noteId).textContent = message || WORDS[next];
    }
    const stuck = (s, why) => note(s, "error", why && `${why}${RETRY}`);
    const subtreeSheet = {
      noteId: "mnote", scope: "sync", state: "synced",
      closed: "closed without writing — the file is as it was",
      dirty: () => dirty(),
      flush: () => flush(editing.digest),
      // What a conflict overwrites under: the digest the file carries NOW, unless
      // the sheet moved on to another headline while the read was out.
      refresh: () => {
        const h = editing;
        return headline(h.id, h.child).then((b) => {
          if (editing !== h) return false;
          h.digest = b.digest;
          return true;
        });
      },
      shut: () => shut(),
    };
    const activeSheet = () => (editing ? subtreeSheet : settings ? configSheet : null);
    const sync = (next, message) => note(subtreeSheet, next, message);
    function shut() {
      el("modal").className = ""; editing = null; base = ""; baseProps = null;
      shutEdit(DTITLE); shutEdit(DPARA); shutEdit(PROW);
      drows = []; dlines = []; dflags.clear(); dcursor = null;
      dlinks = [];
      el("dlist").textContent = "";
      el("mprops").className = ""; el("mdoc").className = "";
    }
    // POST the sheet over the subtree it is standing on, pinned to DIGEST —
    // the ROW's extent where it never left the row, and the entry's under a
    // `child='.  A 200 carries the file's new digest, so the receipt chains
    // and the next flush needs no re-materialize.
    function flush(digest) {
      const h = editing, sent = asked();
      sync("syncing");
      return post(h.id, digest, sent, null, h.child)
        .then(outcome)
        .then(landed(h, () => {
          base = raw ? sent.org : base;
          baseProps = raw ? null : JSON.stringify([sent.properties, sent.planning]);
        }))
        .catch((e) => { stuck(subtreeSheet, e.message); return false; });
    }
    // C-x C-s, over whichever sheet is up.  Mid-edit it is a manual flush;
    // on a conflict it is the deliberate keystroke that overwrites — ask for
    // the digests the files carry now and post what the author is looking at
    // over them.
    function saveSheet(b) {
      // COMMIT THE OPEN EDIT.  The structured document has no ladder of its own,
      // so `save-buffer' here is what the design calls it: the alias for
      // committing the element that is open, and the only commit a PARAGRAPH has,
      // RET being a newline inside one.  With nothing open the key falls through
      // to the sheet's own flush, which in raw mode is the whole ladder and in
      // the structured mode writes the document as the model holds it.
      if (docOpen()) { commitDocEdit(b); return; }
      const s = activeSheet();
      if (!s || s.state === "syncing") return;
      if (s.state !== "conflict") { s.flush(); return; }
      s.refresh().then((ok) => ok && s.flush()).catch((e) => stuck(s, e.message));
    }
    // The way out — ESC, the backdrop, q.  Pristine costs no request and
    // touches no file; dirty flushes and closes on the 200; a sheet with
    // trouble in it discards, which is what a second ESC is.
    function leaveSheet() {
      const s = activeSheet();
      if (!s) return;
      if (s.state === "conflict" || s.state === "error") {
        s.shut();
        append(s.scope, "info", s.closed);
        return;
      }
      if (!s.dirty()) { s.shut(); return; }
      if (s.state !== "syncing") s.flush().then((ok) => ok && s.shut());
    }
    // The backdrop is the mouse's ESC, for both sheets: a click that lands on
    // the veil itself rather than on the box over it.
    for (const id of ["modal", "config"])
      el(id).addEventListener("click",
        (e) => { if (e.target === el(id)) leaveSheet(); });
    // And for the two MOMENTARY veils, backdrops of the same family that had
    // none: a click landed on them and nothing happened, where the same click on
    // a sheet closed it.  What it does differs because the surfaces do — a sheet
    // leaves through its own ladder, pristine costing no request, and a momentary
    // is answered and gone — so the two loops are two rules rather than one with
    // a branch in it.
    /** @type {[string, () => void][]} */
    const backdrops = [["links", shutLinks], ["tags", shutTags]];
    for (const [id, off] of backdrops)
      el(id).addEventListener("click",
        (e) => { if (e.target === el(id)) off(); });
    // C-c ' — org's `edit-special' rhyme, one subtree seen two ways: body and
    // panel, or the raw org the panes were cut out of.  The cut is the server's,
    // so the toggle RE-READS the headline rather than splitting or joining
    // anything here, which keeps an org parser out of this page.  A re-read
    // cannot carry unsaved work, so a dirty sheet is refused and told which key
    // would let it through; being a fresh materialize, it lands at `synced'
    // whatever it was at before.
    function toggleRaw(b) {
      if (!editing) return;
      if (dirty()) { said(b, "sync first — C-x C-s"); return; }
      const want = !raw;
      reread(editing.child, (_h, fresh) => {
        editing = fresh; raw = want;
        fill(fresh);
        sync("synced");
        if (raw) el("mtext").focus(); else el("mtext").blur();
        said(b, raw ? "raw org" : "structured document");
      });
    }
    // A tab closing on an edited sheet still owes the file the text:
    // `keepalive' outlives the document, and a pristine sheet sends nothing.
    addEventListener("beforeunload", () => {
      if (!dirty()) return;
      post(editing.id, editing.digest, asked(), { keepalive: true }, editing.child)
        .catch(() => {});
    });

    // Rows.  The renderer virtualizes, so a row outside the window has no
    // element: movement is ids out of `getVisible()' handed to `select(id)'.
    // Which row is on is the renderer's too — it answers with the column,
    // and a click moves both without telling us — so the DOM read is the
    // fallback for an asset predating the call, and nothing is kept here.
    const visible = () => (table ? table.getVisible() : []);
    const focusedId = () => {
      if (cells()) return table.getSelection().id;
      const tr = /** @type {HTMLElement | null} */
        (document.querySelector("#app .tv-table tbody tr.tv-sel"));
      return tr ? tr.dataset.id : null;
    };
    function pick(list, i) {
      if (!list.length) { append("cmd", "info", "no rows to move through"); return; }
      const id = list[Math.max(0, Math.min(list.length - 1, i))].id;
      // Row movement carries the column along: null until a horizontal key
      // picks one, so a page nobody has moved sideways in keeps whole rows.
      table.select(id, column());
    }
    // A row step is the renderer's `selectStep': it carries the column, and it
    // turns the page at either end, which only the renderer knows there is —
    // `getVisible()' is one page's worth, so index arithmetic here would stop
    // dead at a boundary.  An asset predating the call has no pages either, so
    // the old walk over the visible ids is exactly right for it.
    const steps = () => can(table, "selectStep");
    function move(step) {
      if (steps()) {
        if (visible().length) table.selectStep(step);
        else append("cmd", "info", "no rows to move through");
        return;
      }
      const list = visible(), at = list.findIndex((r) => r.id === focusedId());
      pick(list, at === -1 ? (step > 0 ? 0 : list.length - 1) : at + step);
    }
    // What a key says when it has run: the sequence, the COMMAND, and what
    // happened in brackets after it.  The command is the blob's own identifier,
    // spoken verbatim — `> → last-row', never `> → last row' — these names being
    // the handle a rebinding config will address a function by, and a reader who
    // learns one off the echo has to be able to type it.  The prose goes in the
    // brackets, naming an outcome rather than a function.  Every key echoes
    // through here, so there is one shape and one place the rule can be broken.
    const said = (b, what) =>
      echo(`${b.seq} → ${b.command}${what ? ` (${what})` : ""}`);
    // Pages.  The turn is the renderer's, and the bracket says where it landed
    // rather than repeating the key: `] → next-page (page 3/129)' reads the
    // same at a stop as at a turn.
    const pager = () => can(table, "nextPage") && can(table, "pageInfo");
    // WHICH page is showing, 1 for an asset with no pages: `visible()' is one
    // page's worth, so anything asking what the view still holds has to know
    // which page it asked about.
    const pageNow = () => (pager() ? table.pageInfo().page : 1);
    // The sort, which is `^''s alone now: an ORDER IS A QUERY, so the agenda
    // states its own by carrying a `sort:' token and no page here calls `sortBy'.
    // Named with the rest of the optional calls, this being where a reader greps
    // for which renderer calls are feature-detected.  `sortPromote' composes the
    // chain and WRITES IT INTO THE QUERY as ONE arrow-form `sort:' token
    // (`sort:state->title:desc'), which comes back through `onFilter' like any
    // other query change — so the URL carries the order, DEL takes a key off it,
    // and the server is asked for the order it is about to be sent.  This page
    // keeps no record of the chain: the handle publishes it (getSort) and the
    // query spells it.
    const sorts = () => can(table, "sortPromote");
    function turnPage(b, step) {
      if (!pager()) { said(b, "this table-view.js has no pager"); return; }
      if (step > 0) table.nextPage(); else table.previousPage();
      const at = table.pageInfo();
      said(b, `page ${at.page}/${at.pages}`);
    }
    // The ends of the buffer, progressively.  `<' takes the page's first row;
    // pressed AGAIN, already on it, it turns back a page and lands on THAT page's
    // first row, and `>' mirrors it — so the pair reaches the ends of the SET
    // rather than of the page, and a reader who wants one page turned still has
    // the brackets.  Page one's first row and the last page's last row are stops:
    // the turn declines and nothing moves.
    //
    // Both climbs land at the wrong end and need a select of their own, the
    // renderer putting the cursor on the end it ARRIVES at — `nextPage' on the
    // new page's first row, `previousPage' on its last — the opposite end from
    // the one the key is named for, in both directions.  The column comes back
    // out of the renderer: a turn re-selects with the column it had, so reading
    // `column()' after one reads what it kept.
    //
    // A turn is an explicit page action, so the renderer snaps out of continuous
    // presentation back to paged at the page it turned to — which is what a key
    // named for an end of the buffer means, the reader having asked for a
    // boundary and paged being the presentation that has them.
    function endStop(b, last) {
      const list = visible();
      if (!list.length) { append("cmd", "info", "no rows to move through"); return; }
      const end = (rows) => rows[last ? rows.length - 1 : 0].id;
      // Not there yet — or an asset with no pages, where there is nowhere to
      // climb to and the within-page jump is the whole of the key.
      if (!pager() || focusedId() !== end(list)) {
        table.select(end(list), column());
        said(b, "");
        return;
      }
      if (!(last ? table.nextPage() : table.previousPage())) { said(b, ""); return; }
      const turned = visible();
      if (turned.length) table.select(end(turned), column());
      const at = table.pageInfo();
      said(b, `page ${at.page}/${at.pages}`);
    }
    // Cells.  The column is part of the renderer's selection, so it needs no
    // state here: it rides along with row
    // movement, and goes when the selection that holds it goes.  A whole-row
    // selection has none, and the first horizontal key lands on the first
    // column whichever direction asked.
    const cells = () => can(table, "getSelection");
    const column = () => (cells() ? table.getSelection().col : null);
    function moveCol(b, step) {
      if (!cells()) { said(b, "this table-view.js has no cell selection"); return; }
      const at = column(), want = at === null ? 0 : at + step;
      // Walking off the cells LANDS rather than bumping: a column index outside
      // the table is no column at all to the renderer, which nulls it and gives
      // back the whole-row look, so the step is handed over out of range and the
      // exit is a real move — where a clamp here used to swallow the key and say
      // `at last' at a wall the renderer does not have.  The column comes back
      // out of `column()' rather than off `want', the renderer's answer deciding.
      const id = focusedId();
      if (!id || !table.select(id, want)) { said(b, "no row"); return; }
      const now = column();
      said(b, now === null ? "row mode" : (cols[now].header || cols[now].key));
    }
    // Marks.  The renderer holds them, keyed by id, so nothing is kept here:
    // which rows are marked, how many there are and what a mark survives are all
    // its answers.  Dired's advance is this page's — the key that marks is the
    // key that walks, which makes a held `m' a run down a column.
    const marking = () => can(table, "toggleMark");
    // Archive flags are the renderer's for the same reason marks are: a flag has
    // to outlive a `setRows', a filter hiding its row and a page it is not on,
    // and only the thing that draws the rows can do that.  An asset predating
    // the calls says so rather than growing a shell-side set a paint would lose.
    const flagging = () => flagsOn(table);
    const isFlagged = (id) => flagging() && table.getFlagged().indexOf(id) !== -1;
    // The same question of the other set, and asked the same way: the renderer
    // is consulted at the moment it matters rather than copied into a set here.
    const isMarked = (id) => marking() && table.getMarked().indexOf(id) !== -1;
    // The log names a row the way the table does: by its title, out of the rows
    // in hand — the page on screen, and the unfiltered baseline behind it.  A row
    // in neither is named by its id, a lookup failure a reader can still act on.
    // `displayText' is the renderer's own link rule, so what the line spells is
    // what the cell shows.
    // The row ID names, out of the two lists this page has in hand — the page on
    // screen, and the unfiltered baseline behind it — or an empty one, so a
    // caller reads a cell off the answer rather than guarding the lookup.
    const rowOf = (id) => visible().concat(all).find((r) => r.id === id) || {};
    const titleOf = (id) => {
      const cell = (rowOf(id).cells || {}).title;
      const shown = typeof TableView.displayText === "function"
        ? TableView.displayText(cell) : String(cell || "");
      return shown || id;
    };
    // One wording for every write a key makes: the pill counts what landed, the
    // log says which rows they were.  Bulk is one line per row, since a set
    // spanning three files can come back two-thirds applied.
    const noted = (id, what) =>
      append("cmd", "info", `headline ${JSON.stringify(titleOf(id))} ${what}`);
    // TOGGLING is `m', which flips the way dired's does and takes the renderer's
    // word for where it landed.  `u' is never a toggle: it flips too, then puts
    // back anything it just laid down, so walking a column of marks clears it
    // rather than laying it again.  Both calls are one statement apart and the
    // renderer coalesces its painting to a frame, so the flip is never drawn.
    function mark(b, toggling) {
      if (!marking()) { said(b, "this table-view.js has no marks"); return; }
      const id = focusedId();
      if (!id) { said(b, "no row"); return; }
      // `u' takes the archive FLAG off first: it is the more recent thing a reader
      // put on the row and the one that would otherwise write a file.  One key for
      // both, which is what dired does, and the echo says which.  THE ASYMMETRY IS
      // THE TABLE'S and stays here: over the two popups `u' is the flag key and
      // nothing else, where over the table it is the MARK key preferring a flag
      // when the row is wearing one — so the clearing, the log line and the walk
      // belong to the shared gesture (`flagKey' does all three) and the choice to
      // hand it the key belongs to this surface.
      if (!toggling && isFlagged(id))
        { flagKey("u", XFLAGS(b), (what) => said(b, what)); return; }
      let on = table.toggleMark(id);
      if (on && !toggling) on = table.toggleMark(id);
      said(b, `${on ? "marked" : "unmarked"} · ${table.markedCount()}`);
      move(1);
    }
    // Commands.  A structured write names ROWS and lets the server compute the
    // spans, so nothing here knows what a headline looks like; `edit-link' names
    // a RANGE and knows no more for it, the range coming out of `GET /links' and
    // going back as it came.  Nothing here touches the table afterwards either:
    // the rows arrive over the socket once the watch has re-read the files, the
    // way an editor's save arrives.
    //
    // Which rows a command runs over is per COMMAND, and the two answers are
    // deliberately different.  `set-state' takes the MARKED set, the generic bulk
    // selection — mark a run of rows, set them all.  Archiving takes the FLAGGED
    // set, a selection made for archiving and nothing else (`flagged' below): the
    // destructive-looking command must not inherit a selection a reader built for
    // some other purpose.  Either way the set is the renderer's and is asked for
    // when the command runs rather than tracked here.
    const targets = () => {
      const marked = marking() ? table.getMarked() : [];
      if (marked.length) return marked;
      const id = focusedId();
      return id ? [id] : [];
    };
    // A partial answer is ordinary here: each file is its own write, so one that
    // moved on disk refuses its rows while the rest land.  The count goes in the
    // pill and every refusal in the log. HOW names what the pill says inside the
    // parentheses, and is given the number of rows that LANDED so a partial
    // answer cannot read as a whole one: the count alone is the default, and a
    // key that ran over a named set says which set it was, falling back to the
    // bare count when nothing landed, since "row" over zero rows would be a lie.
    // The route, and the only place this page spells it: a body in, the answer
    // out, and the server's own words thrown where it refused.  Both writing keys
    // go through it — the one that names rows and the one that makes one — so
    // what a refusal looks like is decided once.
    const postCommand = (body) => postJSON("/command", body).then(unwrap);
    // And the one shape a failed write takes: the pill says what went wrong and
    // the strip keeps it, named by the command that was asked for.
    const failed = (b, name) => (e) => {
      said(b, e.message);
      append("cmd", "error", `${name} failed: ${e.message}`);
    };
    // And the shape a palette raised over an unanswered request takes: a
    // palette with nothing in it is no offer, so the overlay comes down and the
    // reason goes to the strip.  It takes the prompt it was raised FOR, since a
    // reader who left and raised another must not have that one closed.
    const askFailed = (mine, name) => (e) => {
      if (prompting === mine) unask();
      append("cmd", "error", `${name} failed: ${e.message}`);
    };
    // The results come back out, undefined where the request failed: a caller
    // with state of its own to fold them into reads them, every other one
    // ignoring the answer, which is the pill and the log this already wrote.
    // The tags popup's three flows fold them into the tag sets it is drawing and
    // `archive' spends the marks they landed on, each guarding the undefined the
    // same way, since a failed write landed nothing.
    //
    // WHAT a command means to the rows it touched is the CALLER's, so nothing
    // here branches on the name past the wording below: a per-name arm in this
    // shared path is one every future command has to be read against.
    //
    // PIN is the optimistic lock, and the caller's, because knowing what a write
    // was measured against is: `edit-link' holds char offsets into a file and
    // sends the digest that file had when `/links' measured them, where the
    // commands naming a PROPERTY of a row — a keyword, a tag — are measured
    // against nothing and send none.  Absent, the route still refuses a file that
    // moved on DISK; the pin adds refusing one the STORE has re-read since.
    // What one landed write did, per row.  The names are the route's whole
    // vocabulary, so the wording is a TABLE beside them rather than a ladder
    // inside the shared path — one entry per name, the way `HANDLERS' is one
    // entry per command, and a name added to the route is a line here.  The
    // fallback is `set-state', whose phrase reads off the keyword it set;
    // `edit-link' is the one whose pill line IS the line the log wants.
    const VERBED = {
      "edit-link": (args, verb) => verb,
      "set-title": (args) => `retitled ${JSON.stringify(args.title)}`,
      "set-priority": (args) =>
        (args.priority ? `priority [#${args.priority}]` : "priority cleared"),
      archive: () => "archived",
      "add-tag": (args) => `tagged :${args.tag}:`,
      "remove-tag": (args) => `untagged :${args.tag}:`,
      "rename-tag": (args) => `retagged ${args.from}→${args.to}`,
      "set-planning": (args) =>
        `${args.keyword.toLowerCase()} ${args.date || "cleared"}`,
    };
    const stated = (args) => (args.keyword ? `→ ${args.keyword}` : "state cleared");
    const verbed = (name, args, verb) => (VERBED[name] || stated)(args, verb);
    function fire(b, name, ids, args, verb, how, pin) {
      return postCommand({ name, ids, args, digests: pin }).then((answer) => {
        const results = answer.results || [];
        // THE ANSWER RE-PINS THE SHEET, the tags popup's own rule one surface
        // over: a command fired from the sheet has just moved the file, and
        // the per-id 200 carries the file's NEW digest — while the store, a
        // watch debounce behind, still spells the old one, and the frame that
        // would re-read is guarded off under an open edit or the panel's
        // keys.  Left unpinned, every subtree commit inside that window 409'd
        // at `conflict' for the reader's own landed write.
        if (editing) {
          const mine = results.find((x) => x.ok && x.id === editing.id && x.digest);
          if (mine) editing.digest = mine.digest;
        }
        const bad = results.filter((x) => !x.ok);
        const landed = results.length - bad.length;
        said(b, `${verb} · ${how ? how(landed) : landed}`);
        const what = verbed(name, args, verb);
        for (const x of results) if (x.ok) noted(x.id, what);
        if (bad.length)
          append("cmd", "error", bad.map((x) => `${x.id}: ${x.error}`).join(" · "));
        return results;
      }).catch(failed(b, name));
    }
    // AN ARCHIVED ROW SPENDS ITS MARK, the way it spends its flag: the marks the
    // rows RESULTS landed on were carrying, taken off.  The mark is the
    // renderer's and survives a `setRows' and a filter that hides its row — which
    // is what makes it useful, and what would otherwise leave an archived row
    // marked INVISIBLY: `markedCount()' would count it, `M' and `U' would answer
    // about it, and it would come back marked the moment a reader looked at
    // `tag:*archive*'.  Only the rows that LANDED, a refused one not having been
    // archived, and none at all where the request itself failed, which is the
    // undefined `fire' hands a `.then' after its own `catch'.  `toggleMark' is
    // the only door the renderer offers, so a membership test comes first, and it
    // is `isMarked' — the renderer asked at the moment it matters, never a set
    // kept here — which also feature-detects, so an asset with no marks has none
    // to spend.
    function unmark(results) {
      for (const x of results || [])
        if (x.ok && isMarked(x.id)) table.toggleMark(x.id);
    }
    // WHERE POINT GOES AFTER AN ARCHIVE: THE NEXT SURVIVING ROW.  Worked out
    // from POINT rather than from the set — down the page for the first row not
    // leaving, failing that back UP for the nearest — since what a reader is
    // owed is the row taking the place of the one they stood on.  Nothing where
    // every row on the page is leaving.
    //
    // The UP half always agrees with the renderer's own keeping, so nothing
    // exercises it alone; it is here so the rule rests on no other component
    // for half of itself.
    //
    // Taken HERE, at fire time: once the rows have gone the gap they left is
    // exactly what a later read cannot see, which is why the renderer's keeping
    // falls back to the visual PLACE.
    //
    // WHETHER anything is owed is decided elsewhere — `settled' fires only once
    // the row point was on has left, and `spent' drops the whole thing when the
    // answer says it was not archived.  `at' is the anchor's place among the
    // SURVIVORS, the fallback for the anchor itself vanishing.
    function anchorFor(ids) {
      const rows = visible(), going = (id) => ids.indexOf(id) !== -1;
      const from = focusedId();
      const here = from ? rows.findIndex((r) => r.id === from) : -1;
      if (here === -1) return null;
      // The PAGE it was taken on.  `visible()' is one page, so "the row point
      // was on has left the view" is only answerable about the page it was on:
      // a reader who turned a page between the write and its watch event would
      // otherwise be told every row of it had gone.
      const on = pageNow();
      let want = null;
      for (let i = here + 1; want === null && i < rows.length; i += 1)
        if (!going(rows[i].id)) want = rows[i];
      for (let i = here - 1; want === null && i >= 0; i -= 1)
        if (!going(rows[i].id)) want = rows[i];
      if (want === null) return null;
      return { from, on, id: want.id,
               at: rows.filter((r) => !going(r.id)).indexOf(want) };
    }
    // And the landing, run at every door the archive's rows can reach the view
    // through: the filtered REFETCH, the one they actually leave by; the
    // unfiltered SPLICE, which for an archive re-sends the row rather than
    // removing it (the tag moved, the headline did not) but is what a row leaving
    // over the socket would come through; and the repaint a reconnect costs, the
    // same rows arriving by a third road.
    //
    // IT IS ALWAYS SPENT, and lands only where there is something to land: the
    // anchor describes ONE watch step and must not outlive it, or a page turn and
    // somebody else's edit later would pull the cursor to a row this write had an
    // opinion about long ago.  Nothing to land in two cases — the row point was
    // standing on is still there (an unfiltered client keeps it, and so does a
    // `tag:*archive*' query that still matches it), and the page showing is not
    // the page the anchor was taken on, where `visible()' can say nothing about
    // whether that row is still in the view.
    function settled() {
      arrived();
      const want = leaving;
      leaving = null;
      if (!want || !table) return;
      if (pageNow() !== want.on) return;
      if (visible().some((r) => r.id === want.from)) return;
      land({ id: want.id, col: column() }, want.at);
    }
    // The capture's landing, at those same doors and spent the same way, and
    // `land''s ordinary rule asked only where there is something to land ON: a
    // filter that hides the new row, a page it is not on, or a watch step that
    // has not delivered it yet all leave point exactly where it stands.  Asking
    // unguarded would pull the cursor to row one, since `land' falls through to
    // an index and there is no honest index to fall to here.
    function arrived() {
      const want = arriving;
      arriving = null;
      if (!want || !table) return;
      if (visible().some((r) => r.id === want)) land({ id: want, col: column() });
    }
    // Archiving: the tag goes on, the headline stays, and the default view stops
    // showing it.  WHICH ROWS is `flagKey''s — the FLAGGED set when there is one
    // and the row at point otherwise — and never the marked one: a mark is the
    // generic bulk selection a reader lays down to set a state over a run of
    // rows, and letting the archive key inherit it makes every mark a loaded
    // gun.  So the table names no set of its own here; it hands the gesture a
    // key and takes back the ids.
    //
    // The marks are spent HERE rather than in `fire': what an archived row owes
    // its mark is the archive gesture's rule, and a name test in the shared path
    // would be one every command added after it has to be read against.
    //
    // `fire' catches its own request failures and resolves to `undefined', so the
    // tail this hangs off it needs a catch of its OWN or a throw inside the
    // spending would be an unhandled rejection where the old in-`fire' placement
    // wrote a `cmd error' line.  It is reachable: `marking()' feature-detects
    // `toggleMark' alone while `isMarked' also calls `getMarked', so an asset
    // carrying one and not the other throws here. A refused write moved no row,
    // so the landing it armed is dropped with the marks it did not spend — the
    // rows are all still there and point is still on one of them.  Both are what
    // the ANSWER says rather than what the request asked for, which is why they
    // are folded in one place.  MINE is the anchor this answer is about, compared
    // rather than assumed: two archives can be out at once, and an earlier answer
    // naming none of the later one's rows would otherwise disarm it.  The anchor
    // is decided BEFORE the marks, `unmark' being able to throw on an asset
    // carrying half the mark calls, which would leave a landed write still armed.
    const spent = (mine) => (results) => {
      if (mine && leaving === mine
          && !(results || []).some((x) => x.ok && x.id === mine.from))
        leaving = null;
      unmark(results);
    };
    // WHAT THE TABLE'S SECOND PRESS TAKES: the ids `flagKey' worked out, archived
    // in one request under the binding that asked.  The anchor is taken HERE, at
    // fire time, while the view still holds the rows about to go; `fire' notes
    // the landed rows one line each; and HOW is the gesture's, a function of the
    // LANDED count so a partly refused write cannot read as a whole one.
    function archive(b, ids, how) {
      leaving = anchorFor(ids);
      fire(b, "archive", ids, {}, "archived", how)
        .then(spent(leaving)).catch(failed(b, "archive"));
    }
    // THE TABLE'S SHAPE, and the third `flagKey' surface.  A function of the
    // BINDING, everything it says and everything it fires being spoken through
    // one — `said' spells the binding's own command name, so `d' reads
    // `archive-flag' and `D' reads `org-glance-overview:delete' out of the same
    // gesture, which is what the two keys mean.  Its phrases are BRACKET CONTENTS
    // where the popups' are whole lines, `said' supplying the arrow and the
    // command name; its `note' is the one that has anything to log, a table row
    // being an org headline and the strip naming every one a key touches.
    const XFLAGS = (b) => ({
      mount: () => table, at: focusedId, walk: () => move(1),
      take: (ids, how) => archive(b, ids, how),
      note: (id, on) =>
        noted(id, on ? "marked for deletion" : "unmarked for deletion"),
      missing: "this table-view.js has no archive flags",
      none: "no row",
      unflag: "flag cleared",
      flag: "flagged — d again archives",
    });
    // ORG'S PRIORITY RING, and the wrap is THROUGH NONE: up runs
    // @none → C → B → A → none@ and down the reverse.  That is org's own cycle,
    // and it makes the token removable with the key that sets it — there is no
    // second key for "no priority" because the ring has a stop for it.  Pure and
    // order-only, so the two directions are one list read two ways.
    const PRIORITY_RING = [null, "C", "B", "A"];
    const cycled = (now, step) => {
      const at = PRIORITY_RING.indexOf(now || null);
      const n = PRIORITY_RING.length;
      return PRIORITY_RING[((at === -1 ? 0 : at) + (step > 0 ? 1 : n - 1)) % n];
    };
    // A priority CELL as the RING spells it: the cell wears org's brackets and
    // the ring holds the letter, so this is `priorityLetter' on the page's side
    // of the wire — the same reading the filter and the comparator make, and a
    // BRACKETLESS cell is taken as the letter it is.  ONE function, because the
    // table's ring and the sheet's each had their own regexp and the sheet's
    // refused what this accepts.
    const priorityIn = (cell) => {
      const t = String(cell || "").trim();
      const m = /^\[#(.)\]$/.exec(t);
      return m ? m[1].toUpperCase() : (t ? t.toUpperCase() : null);
    };
    const priorityOf = (id) => priorityIn((rowOf(id).cells || {}).priority);
    // EACH ROW CYCLES FROM ITS OWN VALUE, org's per-entry semantics and the one
    // thing a single request cannot carry: `args' is one object for the whole
    // call, so a marked set of MIXED priorities is one command per landing value,
    // each over the rows that land there.  A set that agrees is one request, the
    // common press; a set that does not stays mixed and moves together, which is
    // what a reader who marked them meant.  It is the tags popup's rule reached
    // from another side: several flags are several commands, a command naming one
    // value.
    async function cyclePriority(b, step) {
      const ids = targets();
      if (!ids.length) { said(b, "no row"); return; }
      const groups = new Map();
      for (const id of ids) {
        const want = cycled(priorityOf(id), step);
        const key = want === null ? "" : want;
        groups.set(key, (groups.get(key) || []).concat([id]));
      }
      // ONE COMMAND AT A TIME, AWAITED.  Two landing values over rows that share a
      // FILE are two requests against ONE drift lock: fired together, each is
      // measured against a digest the other is moving, so half the press comes back
      // refused or the later write lands over the earlier and both answer `ok'.
      // Awaited, the refusal is deterministic and the log names it, where the race
      // named nothing.  AND EVERY VALUE IS STILL ATTEMPTED: `fire' THROWS on a
      // whole-request refusal, so an unguarded `await' would abandon the values
      // behind it with no pill and no line — the flags are already spent by then,
      // leaving the reader with some rows moved, some not, and nothing said.  The
      // refusal is logged where it happened and the loop goes on.
      for (const [key, over] of groups)
        await fire(b, "set-priority", over, { priority: key || null },
                   key ? `[#${key}]` : EMPTY).catch(failed(b, "set-priority"));
    }
    // Capture: the one write that names no row, so it takes none of the selection
    // machinery above.  The line is raw org — `TODO Buy milk :errands:' captures
    // a keyword, a title and a tag — and the server decides WHERE, out of the
    // tree's own `#+GLANCE_CAPTURE_TARGET:'.  The row comes back over the socket
    // once the watch has read the file it was written to, like every write here.
    //
    // ONE POPUP, not a chain of palettes: the sequential prompts closed and
    // reopened the overlay per step, and the swap read as a blink.  `+' raises
    // the form whole — the tag field with the tree's vocabulary under it, then
    // one field per `%^{PROMPT}' the tag's template asks (grown in place when
    // the tag settles, since only the server knows them), then the line.  RET
    // moves the focus forward and, at the line, captures; TAB is RET's quiet
    // twin; ESC anywhere closes the form with nothing sent.  An EMPTY tag is
    // the untagged inbox path exactly as it was.
    //
    // The vocabulary is the server's (`/capture'), narrowed as the reader
    // types; a name of the tree's own is committable, the charset wall being
    // the server's.  A refusal keeps the form up — the reader fixes the line
    // rather than retyping the form — so `shutCapture' runs on the 200 alone.
    let capping = null;   // the capture form's state while it is up
    const capUp = () => !!capping;
    function shutCapture() {
      capping = null;
      el("kfields").textContent = "";
      el("klist").textContent = "";
      el("ktag").value = ""; el("ktext").value = "";
      el("capture").className = "";
      const held = active();
      if (held && held.blur) held.blur();
    }
