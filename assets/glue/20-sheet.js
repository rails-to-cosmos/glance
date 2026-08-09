// THE MATERIALIZE SHEET, one widget over two panes (docs/proposal-widget-files.md).
// The step-B seam cut it into three files, and the cycles that survived every
// relocation were all this: a flush is ONE `POST /headline' carrying the
// document's body beside the panel's properties and planning, so the pane that
// writes must read the pane that does not.  CLAUDE.md's own words for it are
// "two panes over one subtree".
//
// Its model lives here now -- the entry on show, whether the sheet is raw, and
// what it was opened holding, which is what `dirty' compares against.
    let editing = null;
    let base = "", baseProps = null, raw = false;
    // THE PANE'S OWN MODEL, which lived in the core while four files wrote it
    // (docs/proposal-widget-files.md, step C).  It is this widget's now, and
    // what the others need of it they ask for by name below.
    // THE DOCUMENT PANE IS AN ELM PROGRAM, `assets/elm/src/Doc.elm': it owns the
    // parse, the rows, the two-axis cursor, the grain and the delete flags, and
    // it draws them.  This side keeps a MIRROR of what it pushes back — the same
    // arrangement the property panel is under, and for the same reason: a port
    // round trip costs a macrotask and every reader here is synchronous, running
    // at the top of a key handler a turn after whatever moved the model last.
    const DCELLS = CFG.dcells;
    let drows = [], dat = 0, dcol = null, dgrain = "element";
    let dflags = [], dbody = "", dlinks = [];
    let dport = null, dtook = null, dwrote = null;
    const cellsOf = (o) => DCELLS.map((k) => ({ key: k, val: (o || {})[k] || "" }));
    const shown = (r) => (r.cells || []).filter((c) => c.val);
    function docPane() {
      if (dport) return dport;
      dport = Elm.Doc.init({ node: part(el("dlist"), "div", "") }).ports;
      dport.docState.subscribe((now) => {
        drows = now.rows; dat = now.at; dcol = now.col;
        dgrain = now.grain; dflags = now.flags; dbody = now.body;
        // Elm pushes a port BEFORE it paints, so what the cursor is scrolled
        // to and what the overlay is laid over are read a turn later.
        soon(() => { keepInView(docElAt()); placeEdit(); });
      });
      dport.docSaid.subscribe((what) => { if (dwrote) { dwrote(what); dwrote = null; } });
      dport.docBody.subscribe(commitDoc);
      dport.docTook.subscribe(took);
      return dport;
    }
    const dsend = (m) => docPane().docIn.send(m);
    // A GRAIN KEY'S ECHO is Elm's answer: the word for what it landed on is the
    // model's, so it is said where the move was decided.
    const dsay = (k, m) => { dwrote = keySaid(k); dsend(m); };
    /** The handle `flagKey' asks for — `pmount' one pane over is the same idea. */
    const dmount = {
      flagRow: (id) => dsend({ kind: "flag", id }),
      unflagRow: (id) => dsend({ kind: "unflag", id }),
      getFlagged: () => dflags.slice(),
      clearFlags: () => dsend({ kind: "clearFlags" }),
      selectStep: (by) => dsend({ kind: "step", by }),
    };
    // Forbidden over the TABLE's rows (the renderer's); the suite counts call
    // sites.  `block:"nearest"' honours `.de''s `scroll-margin', the scrolloff.
    function keepInView(row) {
      if (row && typeof row.scrollIntoView === "function")
        row.scrollIntoView({ block: "nearest" });
    }
    const docLevel = () => (editing && editing.level) || 1;
    // OFFSETS ARE IN CHARACTERS (docs/invariants.md); JS counts UTF-16 units.
    const clen = (s) => Array.from(String(s)).length;
    // The three regions the lens lifts out sit ABOVE the paragraphs, so a body
    // offset past the title line is displaced by ONE constant.
    const bodyShift = (h) => clen(h.org || "") - clen(h.body || "");
    const linksIn = (at, links) => (links || dlinks).filter((l) =>
      l.span && l.span[0] >= at[0] && l.span[1] <= at[1]);
    // The row's own span, worked out where the model is and carried in the state.
    const spanOf = (r) => (r && r.span) || null;
    /** The stops as the model has them, and where point stands among them. */
    const docRowAt = () => drows[dat] || null;
    const dcells = (r) => (r && (r.kind === "head" || r.kind === "child")
                            ? shown(r).length : 0);
    // MOVEMENT IS TWO AXES (docs/design-rhymes.md): siblings, then the grain.
    const colStep = (k) => (k === "<right>" || k === "l" ? 1
                          : k === "<left>" || k === "h" ? -1 : 0);
    const grainStep = (k) => (k === "f" ? 1 : k === "b" ? -1 : 0);
    const docStep = (step) => dsend({ kind: "step", by: step });
    const docFiner = (k) => dsay(k, { kind: "finer" });
    const docBroader = (k) => dsay(k, { kind: "broader" });
    const moveDocCol = (k, step) => dsay(k, { kind: "col", by: step });
    function openHere() {
      const r = docRowAt(), b = docBinding("org-glance-overview:open");
      const at = spanOf(r);
      if (!at) { said(b, "nothing to open here"); return; }
      const links = linksIn(at);
      followLinks(b, editing.id, { digest: editing.digest, links }, links);
    }
    const docTitle = () =>
      ((editing && editing.cells && editing.cells.title) || (editing || {}).id || "");
    const docBinding = (command, seq) => ({ seq: seq || "RET", command });
    function docEnter() {
      const r = drows[dat];
      if (!r) return;
      if (r.kind === "child") { into(r.index); return; }
      if (r.kind === "para") { openEdit(DPARA, r); return; }
      headEnter(r);
    }
    function headEnter(r) {
      // A cursor can outlive the cells it was taken on, so the cell is READ.
      const c = dcol === null ? null : shown(r)[dcol];
      if (editing.child !== null) {
        echo(`RET → a child's ${c ? c.key : "title"} is not settable yet — DEL opens its parent`);
        return;
      }
      if (c && c.key === "state") { stateHere(); return; }
      if (c && c.key === "tags") { tagsHere(); return; }
      if (!c || c.key === "title") {
        const t = shown(r).find((x) => x.key === "title");
        openEdit(DTITLE, { id: "CELL:title", val: t ? t.val : "" });
        return;
      }
      echo("RET → priority cycles on S-<up>/S-<down>");
    }
    function atElement(act) {
      const r = drows[dat];
      if (!r || (r.kind !== "head" && r.kind !== "child"))
        { echo("the headline line takes this — n/p to it"); return; }
      if (r.kind === "child" || editing.child !== null) {
        echo("a child is not settable yet — DEL opens its parent");
        return;
      }
      act();
    }
    function cycleHere(step) {
      const b = docBinding(step > 0 ? "priority-up" : "priority-down",
                           step > 0 ? "S-<up>" : "S-<down>");
      const want = cycled(priorityIn((editing.cells || {}).priority), step);
      fire(b, "set-priority", [editing.id], { priority: want },
           want ? `[#${want}]` : EMPTY);
    }
    const stateHere = () =>
      docTargets(docBinding("org-glance-overview:todo"), "set state", askState);
    const tagsHere = () =>
      docTargets(docBinding("org-agenda-set-tags"), "tags", askTags);
    function reread(child, k) {
      if (!editing) return;
      const h = editing;
      headline(h.id, child).then((fresh) => { if (editing === h) k(h, fresh); })
        .catch((e) => stuck(subtreeSheet, e.message));
    }
    function docUp() {
      if (!editing) return;
      if (editing.child === null) { leaveSheet(); return; }
      const up = editing.parent;
      reread(up === null ? undefined : up, (h, fresh) => {
        show(fresh, raw);
        const back = drows.find((r) => r.kind === "child" && r.index === h.child);
        if (back) dsend({ kind: "select", id: back.id });
        echo(`DEL → org-glance-overview:up (${docWhere(fresh)})`);
      });
    }
    function into(index) {
      reread(index, (_h, fresh) => {
        show(fresh, raw);
        echo(`RET → org-glance-overview:materialize (${docWhere(fresh)})`);
      });
    }
    const docWhere = (h) => (h.path || []).slice(-1)[0] || h.id;
    function landed(h, onOk) {
      return (a) => {
        if (a.status === 200) {
          h.digest = a.body.digest;
          sync("synced");
          onOk(a);
          return true;
        }
        // A 409 naming `planning' is a refused entry rather than a moved file.
        if (a.status === 409 && a.body.reason !== "planning") sync("conflict");
        else stuck(subtreeSheet, a.body.error || `sync failed (${a.status})`);
        return false;
      };
    }
    let dcommit = null;
    const commitDoc = (body) => {
      const spoke = dcommit;
      dcommit = null;
      commitDocWith(body, () => { if (spoke) spoke(); });
    };
    /** Edit ROW's text to TEXT, and commit what Elm composes out of it. */
    const editPara = (r, text, say) => {
      dcommit = say;
      dsend({ kind: "edit", id: r.id, text });
    };
    const CHECKBOX = /^(\s*(?:[-+*]|\d+[.)])\s+)\[( |X|x|-)\]/;
    const checkboxAt = (r) =>
      r && r.kind === "para"
        ? (CHECKBOX.exec((r.text || "").split("\n")[0]) || [])[2] ?? null
        : null;
    function toggleCheckbox(b) {
      const r = docRowAt();
      const was = checkboxAt(r);
      if (was === null) { said(b, "no checkbox here"); return; }
      const now = was === " " ? "X" : was === "-" ? "X" : " ";
      editPara(r, r.text.replace(CHECKBOX, `$1[${now}]`), () => said(b, `[${now}]`));
    }
    // THE STORE LAGS THE WRITE: the watch is a debounce away, so an answer
    // under any digest but the 200's own is dropped and retried once.  Taking
    // it reverted the pane and poisoned the pin, which no frame then corrected.
    function reload() {
      if (!editing) return;
      const h = editing;
      const read = (retry) => headline(h.id, h.child).then((fresh) => {
        if (editing !== h) return;
        if (fresh.digest !== h.digest) {
          // The model the write was built from stands, and Elm is still
          // showing it, so there is nothing to redraw.
          if (retry) setTimeout(() => { if (editing === h) read(false); }, 300);
          return;
        }
        editing = fresh;
        fill(fresh);
        sync("synced");
      }).catch((e) => stuck(subtreeSheet, e.message));
      read(true);
    }
    // SNAPSHOTTED AT OPEN — a mouse click moves the cursor under an open edit.
    let edit = null;
    function openEdit(o, row) {
      edit = { o, row };
      el(o.box).className = "on";
      o.fill(row);
      // The renderer stamps `tv-sel' a frame later, so measure a frame later.
      soon(placeEdit);
      o.focus(row);
    }
    function hop() {
      const ids = edit.o.fields;
      const at = ids.findIndex((id) => el(id) === active());
      el(ids[(at + 1) % ids.length]).focus();
    }
    // SHUT MINE: one `edit' over four surfaces, and the tags popup can stand
    // over an open sheet — an unscoped shut would cancel its rename.
    function shutEdit(o) {
      if (!edit || edit.o !== o) return;
      el(edit.o.box).className = "";
      for (const id of edit.o.fields) el(id).blur();
      edit = null;
    }
    const cancelEdit = (what, ...shapes) => {
      for (const o of shapes) shutEdit(o);
      echo(`ESC → keyboard-quit (${what} unchanged)`);
    };
    const anchorOf = (o) => {
      if (o.anchor) return o.anchor();
      const m = o.mount();
      return m ? m.el.querySelector("tbody tr.tv-sel") : null;
    };
    function placeEdit() {
      if (!edit) return;
      const o = edit.o;
      const tr = anchorOf(o);
      // A page with no layout measures nothing and leaves the overlay put.
      if (!tr || typeof tr.getBoundingClientRect !== "function") return;
      const span = o.cells && cellSpan(o.cells, o.cols);
      if (o.cells && !span) return;
      const tds = span && [...tr.querySelectorAll("td:not(.tv-box)")];
      const from = tds && tds[span[0]], to = tds && tds[span[1]];
      if (o.cells && !(from && to)) return;
      const pane = el(o.pane);
      if (typeof pane.getBoundingClientRect !== "function") return;
      const a = tr.getBoundingClientRect();
      const b = pane.getBoundingClientRect();
      const s = el(o.box).style;
      // An absolute child sits against the PADDING box and scrolls with the
      // content, so a bordered, scrolling pane owes `clientTop' + `scrollTop'.
      s.top = `${a.top - b.top - pane.clientTop + pane.scrollTop}px`;
      s.height = `${a.height}px`;
      if (o.tight) {
        const e = o.edge && o.edge();
        const stop = e && typeof e.getBoundingClientRect === "function"
          ? e.getBoundingClientRect().left
          : tr.parentElement
              && typeof tr.parentElement.getBoundingClientRect === "function"
            ? tr.parentElement.getBoundingClientRect().right
            : b.right;
        s.left = `${a.left - b.left}px`;
        s.width = `${stop - a.left}px`;
        return;
      }
      if (!o.cells) return;
      const l = from.getBoundingClientRect(), rt = to.getBoundingClientRect();
      s.left = `${l.left - b.left}px`;
      s.width = `${rt.right - l.left}px`;
    }
    // A declaration rather than a `const', so a direct `eval' leaks it.
    function cellSpan(keys, cols) {
      const at = (keys || []).map((k) => (cols || []).findIndex((c) => c.key === k));
      if (!at.length || at.some((i) => i < 0)) return null;
      return [Math.min(...at), Math.max(...at)];
    }
    window.addEventListener("resize", placeEdit);
    el("mdoc").addEventListener("scroll", placeEdit, true);
    // The stop under point, read off what Elm drew — the pane is not `#dlist''s
    // `dat'-th child, since a composite draws its leaves inside itself.
    const docElAt = () => el("dlist").querySelector(".dat");
    // `tight' over the TITLE CELL's box, right edge at the tags.  A headline
    // with no title cell has none, so the anchor falls back to the line.
    const dTitleAt = () =>
      (docElAt() && docElAt().querySelector(".dc-title")) || docElAt();
    const DTITLE = {
      box: "dtitle", pane: "mdoc", fields: ["dtin"],
      mount: () => null, anchor: dTitleAt, tight: true,
      edge: () => docElAt() && docElAt().querySelector(".dc-tags"),
      fill: (r) => { el("dtin").value = r.val; },
      focus: () => el("dtin").focus(),
    };
    const DPARA = {
      box: "dpara", pane: "mdoc", fields: ["dtext"],
      mount: () => null, anchor: docElAt,
      fill: (r) => { el("dtext").value = r.text; },
      focus: () => el("dtext").focus(),
    };
    const dediting = () => !!edit && edit.o === DTITLE;
    const dparaing = () => !!edit && edit.o === DPARA;
    const docOpen = () => dediting() || dparaing();
    // The document holds the keys with NOTHING focused and raw mode's textarea
    // is blurrable, so an open sheet counts as typing or `table' rows go live.
    const docHolds = () => editing !== null;
    function commitDocEdit(b) {
      const spoke = (what) => (b ? said(b, what) : echo(`RET → ${what}`));
      if (!edit) return;
      const r = edit.row;
      if (edit.o === DPARA) {
        const text = el("dtext").value;
        shutEdit(DPARA);
        if (text === r.text) { spoke("paragraph unchanged"); return; }
        editPara(r, text, () => spoke("paragraph written"));
        return;
      }
      const val = el("dtin").value;
      shutEdit(DTITLE);
      retitle(val);
    }
    function retitle(val) {
      fire(docBinding("org-glance-overview:rename"), "set-title", [editing.id],
           { title: val }, `retitled ${JSON.stringify(val.trim())}`);
    }
    const cancelDocEdit = () => cancelEdit("element", DTITLE, DPARA);
    const sheetOpen = () => docOpen() || pediting();
    const cancelSheetEdit = () => (pediting() ? cancelRow() : cancelDocEdit());
    function ddelete(ids, how) {
      dtook = how;
      dsend({ kind: "delete", ids });
    }
    /** What a delete came back with: `ddelete' left the wording here. */
    function took(answer) {
      const how = dtook;
      dtook = null;
      if (!how) return;
      if (answer.named !== answer.taken.length)
        append("sync", "warn",
               "a headline is not deleted from the sheet — this writes elements only");
      if (!answer.taken.length) { echo(`D → org-delete-element (${how(0)})`); return; }
      commitDocWith(answer.body,
        () => echo(`D → org-delete-element (${how(answer.taken.length)} taken)`));
    }
    // BODY is the caller's: a deletion cannot rebuild it out of the model.
    function commitDocWith(body, say) {
      if (!editing) return;
      const h = editing;
      sync("syncing");
      post(h.id, h.digest, { body, properties: props(), planning: planning() },
           null, h.child)
        .then(outcome)
        .then((a) => { if (editing === h && landed(h, say)(a)) reload(); })
        .catch((e) => stuck(subtreeSheet, e.message));
    }
    const PLANNING = CFG.planning;
    /**
     * ONE PANEL ROW: a property or one of the three fixed planning entries.
     * @typedef {object} PropRow
     * @property {string} id   stable for the sheet's life: `PLN:KEYWORD` or `P<n>`.
     * @property {string} key
     * @property {string} val
     * @property {boolean} fixed  planning row: org's key, and a delete CLEARS.
     */
    /** @type {PropRow[]} */

    // WHAT THE REST OF THE PAGE MAY DO TO THIS MODEL, and the whole of it.
    // Each replaces a line that reached in and assigned.
    /** Empty the pane: the sheet shut, so the document it held is gone. */
    function docClear() {
      dlinks = [];
      dsend({ kind: "clear" });
    }
    /** Fill it from H, or empty it in RAW mode where the textarea is the view. */
    function docFill(h, isRaw) {
      dlinks = h.links || [];
      if (isRaw) { dsend({ kind: "clear" }); return; }
      // CONTENT SITS UNDER THE TITLE TEXT: the width is the ROOT's own stars,
      // which are org-cleaned to one star and a space whatever the depth.  The
      // arithmetic is the stylesheet's.
      el("mdoc").style.setProperty("--g-doc-indent", String("* ".length));
      const body = String(h.body || "");
      dsend({ kind: "fill",
              lines: body.split("\n"),
              own: h.ownLines === undefined ? body.split("\n").length : h.ownLines,
              cells: cellsOf(h.cells),
              kids: (h.children || []).map((c) =>
                ({ index: c.index, level: c.level, cells: cellsOf(c) })),
              links: dlinks.map((l) =>
                ({ from: l.span[0], to: l.span[1], desc: l.desc })),
              spanAt: (h.span || {}).start ?? null,
              shift: bodyShift(h),
              level: h.level || 1,
              titleAt: typeof h.titleAt === "number" ? h.titleAt : null });
    }
    /** Where point stands, as a row ID and a column — what a remount stashes. */
    const docCursor = () => ({ at: drows[dat] ? drows[dat].id : null, col: dcol });
    /** Put it back after one, landing on the row ID names where it survives. */
    function docRestore(at, col) {
      dsend({ kind: "restore", id: at, col });
      const back = drows.findIndex((r) => r.id === at);
      if (back !== -1) dat = back;
      dcol = col;
    }
    /** The row ID names, for a caller holding an id rather than a place. */
    const docRowById = (id) => drows.find((x) => x.id === id);
    /** The checkbox under point, when the stop there has one. */
    const checkboxHere = () => checkboxAt(drows[dat]);
    // THE PANEL IS AN ELM PROGRAM, `assets/elm/src/Panel.elm\': it owns the rows,
    // the cursor and the delete flags, and it draws them where a table-view
    // mount used to be.  This side keeps a MIRROR of the state Elm pushes back,
    // because a port round trip costs a macrotask and the readers below are
    // synchronous — every one of them runs at the top of a key handler, a turn
    // after whatever moved the model last.
    let prows = [], pat = -1, pflags = [], pseq = 0;
    let pport = null, ptook = null;
    // Caught in the CAPTURE phase, so the scroller inside PANE need not be named.
    function mountOnce(host, cols, opts, pane) {
      const m = TableView.mount(el(host), { columns: cols, rows: [] }, opts);
      el(pane).addEventListener("scroll", placeEdit, true);
      return m;
    }
    function mounted() {
      if (pport) return pport;
      // `Browser.element\' REPLACES the node it is given, so it takes a child and
      // `#mptable\' survives as the container `anchorOf\' queries.
      pport = Elm.Panel.init({ node: part(el("mptable"), "div", ""),
                               flags: "d/D delete · u unflag" }).ports;
      pport.panelState.subscribe((now) => {
        prows = now.rows; pat = now.at; pflags = now.flags;
      });
      pport.panelOpen.subscribe((row) => openEdit(PROW, row));
      // The delete's echo rides the answer rather than a second copy of the
      // rule: Elm says which planning rows it CLEARED, and `pdelete\' left the
      // wording it was called with here.
      pport.panelTook.subscribe((cleared) => {
        if (!ptook) return;
        const also = cleared.join(", ");
        echo(`D → org-delete-property (${ptook.how(ptook.n)}`
             + `${also ? ` · ${also} cleared` : ""})`);
        ptook = null;
      });
      el("mprops").addEventListener("scroll", placeEdit, true);
      return pport;
    }
    const psend = (m) => mounted().panelIn.send(m);
    /**
     * The panel's handle, the shape `flagKey\' and the movement keys ask for —
     * `dmount\' one pane over is the same idea over a `Set\'.  Reads answer off
     * the mirror; writes go out as ports.
     */
    const pmount = {
      get el() { return el("mptable"); },
      selectStep: (by) => psend({ kind: "step", by }),
      getSelection: () => ({ id: pat === -1 ? null : (prows[pat] || {}).id }),
      flagRow: (id) => psend({ kind: "flag", id }),
      unflagRow: (id) => psend({ kind: "unflag", id }),
      getFlagged: () => pflags.slice(),
      clearFlags: () => psend({ kind: "clearFlags" }),
    };
    function drawProps(list, plan) {
      pseq = 0;
      shutEdit(PROW);
      el("mprops").className = "";   // and the panel gives the keys back
      const held = new Map(plan || []);
      const rows = PLANNING.map((key) =>
        ({ id: `PLN:${key}`, key, val: held.get(key) || "", fixed: true }))
        .concat(list.map((p) => ({ id: `P${pseq++}`, key: p[0], val: p[1], fixed: false })));
      // SEEDED with what is being sent, so `baseProps\' can be taken in this same
      // turn.  Every other mutation waits for Elm, whose rules decide it.
      prows = rows; pat = 0; pflags = [];
      psend({ kind: "fill", rows, at: rows[0].id });
    }
    const patAt = () => pat;
    function addProperty() {
      psend({ kind: "add", id: `P${pseq++}` });
    }
    const props = () => prows
      .filter((r) => !r.fixed)
      .map((r) => [r.key.trim(), r.val.trim()])
      .filter((p) => p[0] !== "");
    const planning = () => prows
      .filter((r) => r.fixed && r.val.trim() !== "")
      .map((r) => [r.key, r.val.trim()]);
    const pnav = () => el("mprops").className === "on";
    function enterPanel() {
      el("mprops").className = "on"; el("mdoc").className = "";
      el("mtext").blur();
    }
    function leavePanel() {
      el("mprops").className = ""; el("mdoc").className = "on";
    }
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
    // The row is the one the overlay OPENED over, never the one point is on now.
    function commitRow() {
      const r = edit.row;
      psend({ kind: "commit", id: r.id, key: el("pkey").value, val: el("pval").value });
      shutEdit(PROW);
    }
    const cancelRow = () => cancelEdit("row", PROW);
    function pdelete(ids, how) {
      ptook = { how, n: ids.length };
      psend({ kind: "delete", ids });
    }
    // Registers AHEAD of the dispatch, so it sees a key first — CLAUDE.md (UI).
    document.addEventListener("keydown", (e) => {
      // Without the guard the sheet claims the letter a palette was raised to read.
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
        else if (k === "S-<up>" || k === "S-<down>")
          once(() => atElement(() => cycleHere(k === "S-<up>" ? 1 : -1)));
        else if (k === "o" || k === "!") once(openHere);
        else if (k === "t") once(() => atElement(stateHere));
        else if (k === ":") once(() => atElement(tagsHere));
        else if (k === "SPC")
          once(() => toggleCheckbox(docBinding("org-toggle-checkbox", "SPC")));
        else if (!flagPress(k, e, DFLAGS)) return;
      }
      e.preventDefault();
    });
    const unlogged = () => {};
    const PFLAGS = {
      mount: () => pmount, take: pdelete, note: unlogged,
      walk: () => stepIn(pmount, 1),
      missing: "this table-view.js has no delete flags",
      none: "org-delete-property (no row)",
      unflag: "delete-unflag (flag cleared)",
      flag: "delete-flag (d again deletes)",
      at: () => (pat === -1 ? null : (prows[pat] || {}).id),
    };
    // This mount is a Set of ids rather than a renderer, so `missing' is unreachable.
    const DFLAGS = {
      mount: () => dmount, take: ddelete, note: unlogged,
      walk: () => docStep(1),
      missing: "this document has no flags",
      none: "org-delete-element (no element)",
      unflag: "delete-unflag (flag cleared)",
      flag: "delete-flag (d again deletes)",
      at: () => docCursor().at,
    };
    // The held-key guard is here: `ONCE' governs dispatch rows, these three live outside.
    const flagPress = (k, e, shape) => {
      if (k !== "d" && k !== "D" && k !== "u") return false;
      if (!repeating(e)) flagKey(k, shape, keySaid(k));
      return true;
    };
    const asked = () => raw
      ? { org: el("mtext").value }
      : { body: dbody, properties: props(), planning: planning() };
    // ONE BUTTONLESS SHEET, twice over: each sheet supplies the verbs — CLAUDE.md (UI).
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
      soon(remembered);
      shutEdit(DTITLE); shutEdit(DPARA); shutEdit(PROW);
      docClear();
      el("dlist").textContent = "";
      el("mprops").className = ""; el("mdoc").className = "";
    }
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
    function saveSheet(b) {
      if (docOpen()) { commitDocEdit(b); return; }
      const s = activeSheet();
      if (!s || s.state === "syncing") return;
      if (s.state !== "conflict") { s.flush(); return; }
      s.refresh().then((ok) => ok && s.flush()).catch((e) => stuck(s, e.message));
    }
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
    for (const id of ["modal", "config"])
      el(id).addEventListener("click",
        (e) => { if (e.target === el(id)) leaveSheet(); });
    /** @type {[string, () => void][]} */
    // CALLED at click time, never named at registration time: a wrapped widget
    // hands its closers out through a `const' handle, which -- unlike the
    // hoisted `function' these were -- is in TDZ while this line runs.
    const backdrops = [["links", () => shutLinks()], ["tags", () => shutTags()]];
    for (const [id, off] of backdrops)
      el(id).addEventListener("click",
        (e) => { if (e.target === el(id)) off(); });
    // Re-materializes rather than splitting here, keeping an org parser off this page.
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
    // `keepalive' outlives the document; a pristine sheet sends nothing.
    addEventListener("beforeunload", () => {
      if (!dirty()) return;
      post(editing.id, editing.digest, asked(), { keepalive: true }, editing.child)
        .catch(() => {});
    });

    const postCommand = (body) => postJSON("/command", body).then(unwrap);
    const askFailed = (mine, name) => (e) => {
      if (promptNow() === mine) unask();
      append("cmd", "error", `${name} failed: ${e.message}`);
    };
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
        // The store lags this write by a watch debounce and the frame that would
        // re-read is guarded off, so the per-id 200's digest re-pins the sheet.
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
    // An archived row SPENDS its mark, or it stays marked invisibly behind the filter.
    function unmark(results) {
      for (const x of results || [])
        if (x.ok && isMarked(x.id)) table.toggleMark(x.id);
    }
    // Taken at FIRE time: once the rows have gone, a later read cannot see the gap.
    function anchorFor(ids) {
      const rows = visible(), going = (id) => ids.indexOf(id) !== -1;
      const from = focusedId();
      const here = from ? rows.findIndex((r) => r.id === from) : -1;
      if (here === -1) return null;
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
    // ALWAYS spent, so the anchor describes ONE watch step and outlives no other.
    function settled() {
      arrived();
      const want = leaving;
      leaving = null;
      if (!want || !table) return;
      if (pageNow() !== want.on) return;
      if (visible().some((r) => r.id === want.from)) return;
      land({ id: want.id, col: column() }, want.at);
    }
    function arrived() {
      const want = arriving;
      arriving = null;
      if (!want || !table) return;
      if (visible().some((r) => r.id === want)) land({ id: want, col: column() });
    }
    // MINE is compared rather than assumed: two archives can be out at once.
    const spent = (mine) => (results) => {
      if (mine && leaving === mine
          && !(results || []).some((x) => x.ok && x.id === mine.from))
        leaving = null;
      unmark(results);
    };
    function archive(b, ids, how) {
      leaving = anchorFor(ids);
      fire(b, "archive", ids, {}, "archived", how)
        .then(spent(leaving)).catch(failed(b, "archive"));
    }
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
    // `priorityLetter' on the page's side of the wire.
    // `args' is one object per call, so a MIXED set is one command per landing value.
    async function cyclePriority(b, step) {
      const ids = targets();
      if (!ids.length) { said(b, "no row"); return; }
      const groups = new Map();
      for (const id of ids) {
        const want = cycled(priorityOf(id), step);
        const key = want === null ? "" : want;
        groups.set(key, (groups.get(key) || []).concat([id]));
      }
      // AWAITED one at a time — two landing values in one FILE are two writes
      // against one drift lock; the inner catch stops one refusal ending the loop.
      for (const [key, over] of groups)
        await fire(b, "set-priority", over, { priority: key || null },
                   key ? `[#${key}]` : EMPTY).catch(failed(b, "set-priority"));
    }

    // THE OPENING, which is the sheet's own: a row's subtree fetched, shown
    // across both panes, and compared against what it arrived as.
    function materialize(id) {
      headline(id).then((h) => show(h, false))
        .catch((e) => append("sync", "error", `materialize failed: ${e.message}`));
    }
    function show(h, asRaw) {
      editing = h; raw = !!asRaw;
      el("mfile").textContent = `${h.file}  ·  ${h.id}`;
      fill(h);
      sync("synced");
      el("modal").className = "on";
      soon(remembered);
      if (raw) el("mtext").focus(); else el("mtext").blur();
    }
    function fill(h) {
      base = raw ? h.org : "";
      el("mtext").value = base;
      // TOGGLE, never assign: the class also carries the sheet's size tier.
      el("sheet").classList.toggle("raw", raw);
      shutEdit(DTITLE); shutEdit(DPARA);
      docFill(h, raw);
      drawProps(raw ? [] : h.properties || [], raw ? [] : h.planning || []);
      el("mdoc").className = raw ? "" : "on";
      drawWhere(h.path || []);
      drawLog(raw ? "" : h.logbook || "");
      baseProps = raw ? null : edited();
    }
    const edited = () => JSON.stringify([props(), planning()]);
    function drawWhere(path) {
      const bar = el("mwhere");
      bar.textContent = "";
      path.forEach((title, i) =>
        part(bar, "span", "wc" + (i === path.length - 1 ? " wat" : ""),
             title || "(untitled)"));
    }
    // Display-only: the file keeps the whole drawer, delimiters and all.
    function drawLog(text) {
      const inner = text.replace(/\n$/, "").split("\n").slice(1, -1).join("\n");
      el("mlog").textContent = inner;
      el("mlog").className = inner ? "on" : "";
    }
    const dirty = () => editing !== null
      && (raw ? el("mtext").value !== base : edited() !== baseProps);
