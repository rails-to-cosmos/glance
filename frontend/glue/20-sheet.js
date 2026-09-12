// THE MATERIALIZE SHEET: two panes over one subtree, one flush — AGENTS.hs.

    // AN ORG TABLE IS A TABLE-VIEW MOUNT.  Elm draws a CHILDLESS <glance-table>
    // host, so its vdom never fights the renderer mounted inside it.
    if (window.customElements && !customElements.get("glance-table")) {
      class GlanceTable extends HTMLElement {
        constructor() { super(); this._view = null; this._tv = null; this._viewJson = null; }
        set view(v) {
          // Elm re-sets an equal-but-fresh value each point move; rebuilding blinks.
          const json = JSON.stringify(v);
          if (json === this._viewJson) return;
          this._viewJson = json;
          this._view = v;
          this.classList.toggle("noheader", !(v && v.hasHeader));
          if (this._tv) this._tv.setView(v); else this._mount();
        }
        get view() { return this._view; }
        connectedCallback() { this._mount(); }
        disconnectedCallback() { if (this._tv) { this._tv.destroy(); this._tv = null; } }
        _mount() {
          if (this._tv || !this.isConnected || !this._view) return;
          // `onLink' re-bubbles to #mdoc; the renderer's own stays in its root.
          const host = this;
          this._tv = TableView.mount(this, this._view, {
            onLink: (target) => host.dispatchEvent(
              new CustomEvent("glance-open", { bubbles: true, detail: { target } })),
            // Widget edits, doc writes: the commit re-bubbles to #mdoc.
            onEdit: (id, col, value, kind) => host.dispatchEvent(
              new CustomEvent("glance-edit",
                { bubbles: true, detail: { id, col, value, kind, host } })),
          });
        }
      }
      customElements.define("glance-table", GlanceTable);
    }

    let editing = null;
    let base = "", baseProps = null, raw = false;
    // THE DOC PANE IS AN ELM PROGRAM; the MIRROR is a macrotask behind — AGENTS.hs.
    const DCELLS = CFG.dcells;
    let drows = [], dat = 0;
    let dflags = [], dbody = "", dlinks = [], dprops = [], dplan = [];
    // WHICH PLANNING ENTRY POINT STANDS IN, by KEYWORD; `null' is the whole line.
    let dplankey = null;
    // WHICH COLUMN POINT STANDS IN, `null' the whole row; mirrored for the push.
    let dcol = null;
    let dhead = false;
    // POINT IS ON A HEADERLESS TABLE'S EPHEMERAL HEADER: RET materializes it.
    let dephem = false;
    let dport = null, dtook = null, dwrote = null;
    const cellsOf = (o) => DCELLS.map((k) => {
      const val = (o || {})[k] || "";
      return { key: k, val, colour: val ? badgeColor(val, k) : "" };
    });
    const shown = (r) => (r.cells || []).filter((c) => c.val);
    const flagPort = (send, held) => ({
      flagRow: (id) => send({ kind: "flag", id }),
      unflagRow: (id) => send({ kind: "unflag", id }),
      getFlagged: () => held().slice(),
      clearFlags: () => send({ kind: "clearFlags" }),
      selectStep: (by) => send({ kind: "step", by }),
    });
    function docPane() {
      if (dport) return dport;
      dport = Elm.Doc.init({ node: part(el("dlist"), "div", "") }).ports;
      dport.docState.subscribe((now) => {
        drows = now.rows; dat = now.at;
        dflags = now.flags; dbody = now.body;
        dprops = now.properties; dplan = now.planning;
        dplankey = now.planKey || null;
        dcol = (now.col === undefined ? null : now.col);
        dhead = !!now.head;
        dephem = !!now.ephem;
        // Elm pushes a port BEFORE it paints, so these are read a turn later.
        soon(() => {
          seedInsert(now.caret); keepInView(docElAt()); placeEdit(); reselectDate();
          tableSelSync();
        });
      });
      dport.docSaid.subscribe((what) => { if (dwrote) { dwrote(what); dwrote = null; } });
      dport.docBody.subscribe(commitDoc);
      dport.docTook.subscribe(took);
      return dport;
    }
    const dsend = (m) => docPane().docIn.send(m);
    const dsay = (k, m) => { dwrote = keySaid(k); dsend(m); };
    const dmount = flagPort(dsend, () => dflags);
    /** THE BAND POINT'S ROW MOVES IN: the nearest scroller over the document and
     * its INSIDE, the one `block:"nearest"' would move.  NULL where none scrolls. */
    function docBand(row) {
      for (let pane = row.parentElement; pane; pane = pane.parentElement) {
        if (pane.clientHeight > 0 && pane.scrollHeight > pane.clientHeight + 1)
          return { pane, top: pane.getBoundingClientRect().top + pane.clientTop,
                   height: pane.clientHeight };
        if (pane.id === "modal") break;
      }
      return null;
    }
    const bandOff = (row) =>
      parseFloat(getComputedStyle(row).scrollMarginBlockStart) || 0;
    // Forbidden over the TABLE's rows; the one ask a page with no layout has.
    const askScroller = (row, block) => {
      if (row && typeof row.scrollIntoView === "function")
        row.scrollIntoView({ block });
    };
    /** ONE PLACEMENT LAW, TWO CALLERS: put one of ROW's edges -- `top', `bottom'
     * or `middle' -- on a LINE down the band; WITH NO LAYOUT it asks `block'. */
    function placeRow(row, want) {
      const band = row && docBand(row);
      if (!band) { askScroller(row, want.block); return; }
      const r = row.getBoundingClientRect();
      const edge = want.edge === "top" ? r.top
                 : want.edge === "bottom" ? r.bottom
                 : r.top + r.height / 2;
      band.pane.scrollTop += edge - band.top - want.line(band, row);
    }
    // `readingLine' is a `const' in a later part, so this CALLS it at use time.
    const READING = { edge: "bottom", block: "nearest",
                      line: (b) => b.height * readingLine() / 100 };
    /** POINT'S ROW COMES TO REST ON THE READING LINE: a row whose BOTTOM fell
     * below scrolls up; a taller row, or one above the top, asks `nearest'. */
    function keepInView(row) {
      const band = row && docBand(row);
      if (!band) { askScroller(row, "nearest"); return; }
      const r = row.getBoundingClientRect(), line = READING.line(band);
      if (r.top < band.top || r.height > line || r.bottom - band.top <= line)
        { askScroller(row, "nearest"); return; }
      placeRow(row, READING);
    }
    /** `C-l' IS org's own `recenter-top-bottom': MIDDLE, then TOP, then BOTTOM.
     * ANY OTHER KEY STARTS THE CYCLE OVER, which makes a run of presses one. */
    const RECENTER = [
      { word: "center", edge: "middle", block: "center",
        line: (b) => b.height / 2 },
      { word: "top", edge: "top", block: "start",
        line: (b, row) => bandOff(row) },
      { word: "bottom", edge: "bottom", block: "end",
        line: (b, row) => b.height - bandOff(row) },
    ];
    let recentres = 0;
    function recenterHere(k) {
      const want = RECENTER[recentres % RECENTER.length];
      recentres += 1;
      placeRow(docElAt(), want);
      keySaid(k)(`recenter-top-bottom (${want.word})`);
    }
    // OFFSETS ARE IN CHARACTERS (AGENTS.hs); JS counts UTF-16 units.
    const clen = (s) => Array.from(String(s)).length;
    // Title and properties sit ABOVE the paragraphs: a constant shifts body offsets.
    const bodyShift = (h) => clen(h.org || "") - clen(h.body || "");
    const linksIn = (at, links) => (links || dlinks).filter((l) =>
      l.span && l.span[0] >= at[0] && l.span[1] <= at[1]);
    const spanOf = (r) => (r && r.span) || null;
    const reachOf = (r) => (r && r.reach) || null;
    const docRowAt = () => drows[dat] || null;
    // `var', so the suite's direct eval reaches the caller's scope.  The DOM
    // paints on rAF and the port lands a macrotask apart: a driver sees both.
    var docAtNow = () => (drows[dat] || {}).id || "";
    // MOVEMENT IS TWO AXES, and `l'/`h' and the arrows ALIAS `f'/`b' — AGENTS.hs.
    const grainStep = (k) => (k === "f" || k === "l" || k === "<right>" ? 1
                            : k === "b" || k === "h" || k === "<left>" ? -1 : 0);
    // A KEY ARMS THE ECHO (`dsay'); the programmatic walk (`dsend') stays quiet.
    const docStep = (step, k) =>
      k ? dsay(k, { kind: "step", by: step }) : dsend({ kind: "step", by: step });
    /** `M-<left>'/`M-<right>': org's `org-promote-subtree'/`org-demote-subtree'.
     * THE MODEL OWNS ROWS, WALLS AND WORD; write and refusal come back named. */
    function shiftHere(k, by) {
      const say = keySaid(k);
      answerOnce((cargo) => say(cargo.said), say);
      dsend({ kind: "shift", by });
    }
    function openHere() {
      const r = docRowAt(), b = docBinding("org-glance-overview:open");
      const at = reachOf(r) || spanOf(r);
      if (!at) { said(b, "nothing to open here"); return; }
      const links = linksIn(at);
      followLinks(b, editing.id, { digest: editing.digest, links }, links);
    }
    const docTitle = () =>
      ((editing && editing.cells && editing.cells.title) || (editing || {}).id || "");
    const docBinding = (command, seq) => ({ seq: seq || "RET", command });
    function docEnter(r = drows[dat]) {
      if (!r) return;
      if (r.name === "table") { echo("RET → f enters the table"); return; }
      if (r.owner) {
        const comp = drows.find((x) => x.id === r.owner);
        if (comp && comp.name === "table") {
          if (dcol == null) { echo(`RET → f takes a ${dhead || dephem ? "column" : "cell"}`); return; }
          const tv = tvOf(hostFor(comp.id));
          if (!tv) return;
          if (dhead || dephem) tv.editHeader(dcol);
          else tv.editCell(r.id, dcol);
          return;
        }
      }
      if (r.kind === "child") { into(r.index); return; }
      if (r.fold) { echo("RET → f reaches the rows inside — TAB folds"); return; }
      if (r.entries) { planEnter(); return; }
      if (r.kind === "para" || r.kind === "meta") { openEdit(DPARA, r); return; }
      headEnter(r);
    }
    /** `RET' over the planning line: OVER AN ENTRY it raises the widget `C-c C-s'
     * raises, keyed by the entry the walk stands in; OVER THE LINE it is INERT. */
    function planEnter() {
      if (!dplankey) { echo("RET → f reaches the entries — RET on one edits it"); return; }
      planHere(docBinding(planCommand(dplankey)), dplankey);
    }
    function headEnter(r) {
      if (editing.child !== null) {
        echo("RET → a child's title is not settable yet — DEL opens its parent");
        return;
      }
      // THE HEADLINE IS ONE STOP: RET opens the title; `t', `:', S-<up> the rest.
      const t = shown(r).find((x) => x.key === "title");
      openTitle(t ? t.val : "");
    }
    // THE TITLE EDIT over the head row.
    const openTitle = (val) => openEdit(DTITLE, { id: "CELL:title", val });
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
    /** Materialize the child at INDEX, and run K over the document it landed on.
     * K RIDES THE REREAD'S CONTINUATION: a caller beside it sees stale mirrors. */
    function into(index, k) {
      reread(index, (_h, fresh) => {
        show(fresh, raw);
        echo(`RET → org-glance-overview:materialize (${docWhere(fresh)})`);
        if (k) k();
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
    /** Arm a one-shot pair: each answer disarms the other, so only one lands. */
    const answerOnce = (onCommit, onSaid) => {
      dcommit = (cargo) => { dwrote = null; onCommit(cargo); };
      dwrote = (what) => { dcommit = null; onSaid(what); };
    };
    // THE MODEL'S WORD RIDES THE CARGO: two ports carry no order, so a second races.
    const commitDoc = (cargo) => {
      const spoke = dcommit;
      dcommit = null;
      commitDocWith(cargo, () => { if (spoke) spoke(cargo); });
    };
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
      // A DERIVED BOX IS ITS CHILDREN'S TO TELL (`boxFace', Doc.elm): toggle a leaf.
      if (drows.some((x) => x.owner === r.id && checkboxAt(x) !== null)) {
        said(b, "derived from children"); return;
      }
      const now = was === " " || was === "-" ? "X" : " ";
      editPara(r, r.text.replace(CHECKBOX, `$1[${now}]`), () => said(b, `[${now}]`));
    }
    /** `X' — HIDE DONE CHECKBOXES, a display-only mode the Elm side owns; the model
     * decides the scope.  `X' IS A CHARACTER FIRST: a doc field takes the letter. */
    function hideDoneHere(b) {
      const box = active();
      if (box && typeof box.selectionStart === "number") {
        spliceIn(box, box.selectionStart, box.selectionEnd, "X");
        box.dispatchEvent(new Event("input", { bubbles: true }));
        return;
      }
      if (!editing || raw) { said(b, "no document here"); return; }
      dwrote = (what) => said(b, what);
      dsend({ kind: "hidedone" });
    }
    // TAB WALKS A LIST ITEM'S RUNGS: one deeper needs a SIBLING, one out a parent.
    const OPENER = /^([ \t]*)(?:[-+*]|\d+[.)])\s/;
    const CONT = /^([ \t]*(?:[-+*]|\d+[.)])\s+(?:\[[ xX-]\]\s+)?)/;
    const TABB = docBinding("org-metaright", "TAB");
    const STEP = 2;                 // org's own, and the pane's `--g-doc-indent'
    let drung = null;
    const openerIn = (text) => OPENER.exec((text || "").split("\n")[0]);
    const indentOf = (text) => { const o = openerIn(text); return o ? o[1].length : null; };
    /** Return the rungs ROW may take from WAS: its own, ONE CHILD, EVERY PARENT. */
    function rungsFor(row, was) {
      const out = [0];
      const i = drows.findIndex((r) => r.id === row.id);
      const ups = [];
      let sib = false, cur = was;
      for (let j = i - 1; j >= 0; j--) {
        const d = indentOf(drows[j].text);
        if (d === null) break;
        if (d === cur && cur === was) sib = true;
        if (d < cur) { ups.push(d); cur = d; }
      }
      if (sib) out.push(STEP);
      for (const d of ups) out.push(d - was);
      return out;
    }
    function tabRung() {
      const box = el("dtext");
      const lines = box.value.split("\n");
      const now = indentOf(lines[0]);
      if (now === null) { said(TABB, "not a list item"); return; }
      const row = edit.row;
      if (!drung || drung.id !== row.id)
        drung = { id: row.id, was: now, rungs: rungsFor(row, now), at: 0 };
      drung.at = (drung.at + 1) % drung.rungs.length;
      const want = drung.was + drung.rungs[drung.at];
      lines[0] = " ".repeat(want) + lines[0].replace(/^[ \t]*/, "");
      box.value = lines.join("\n");
      sizeDocEdit();
      said(TABB, want > drung.was ? "one level in"
                 : want < drung.was ? "out to column " + want
                 : "back where it was");
    }
    const INSERT = docBinding("org-insert-element", "+");
    // The same command one key over: `S-RET' is `+' with the commit in front.
    const NEXT = docBinding("org-insert-element", "S-RET");
    /** Seed the box ONCE from the DRAWN row; a second seeding overwrites typing. */
    function seedInsert(caret) {
      if (!dparaing() || !edit.row.add || edit.row.lead !== undefined) return;
      const drawn = drows.find((r) => r.id === "D");
      const lead = drawn ? String(drawn.text || "") : "";
      edit.row.lead = lead;
      if (!lead) return;
      const box = el("dtext");
      const at = caret == null ? lead.length : caret;
      box.value = lead;
      box.setSelectionRange(at, at);
      sizeDocEdit();
    }
    function insertHere(at) {
      const r = docRowAt();
      if (!r) { said(INSERT, "no element"); return; }
      // A TABLE: `+' adds a ROW after a whole row, a COLUMN after a cell.
      if (r.owner) {
        const comp = drows.find((x) => x.id === r.owner);
        if (comp && comp.name === "table") {
          // The org line construction is Elm's; the shell names row and column.
          if (dcol == null) {
            dcommit = (cargo) => said(INSERT, cargo.said || "row added");
            dsend({ kind: "addrow", id: r.id });
          } else {
            dcommit = (cargo) => said(INSERT, cargo.said || "column added");
            dsend({ kind: "addcol", id: r.id, col: dcol });
          }
          return;
        }
      }
      if (r.kind === "child")
        { said(INSERT, "a child's body is its own — RET opens it"); return; }
      // `+' IN THE DRAWER TYPES THE PAIR IN PLACE; no half-typed pair is written.
      if (r.kind === "meta") {
        dsend({ kind: "draftpair" });
        // ONE CLOCK READ PER SUMMON: the value half wears the date widget's ghost.
        openEdit(DPAIR, { id: r.id, add: true, today: dateNow() });
        // THE BOX WEARS NO CHROME, so the echo carries what the popup's foot did.
        said(docBinding("org-set-property", "+"),
             "a key, then its value — RET applies · ESC cancels");
        return;
      }
      const off = at == null ? null : at;
      // WHERE it lands is the MODEL's answer; a reading of it here got it wrong.
      dwrote = (what) => said(INSERT, what);
      // THE ROW IS DRAWN FIRST; Elm pushes state a turn later and `placeEdit' re-lays.
      const m = { kind: "draft", id: r.id };
      if (off !== null) m.at = off;
      dsend(m);
      openEdit(DPARA, { id: r.id, text: "", add: true, at: off });
    }
    const insertPara = (r, text, done) => {
      answerOnce(done, (what) => said(INSERT, what));
      const m = { kind: "insert", id: r.id, text };
      if (r.at != null) m.at = r.at;
      dsend(m);
    };
    // THE STORE LAGS THE WRITE: any digest but the 200's own is dropped, retried once.
    function reload() {
      if (!editing) return;
      const h = editing;
      const read = (retry) => headline(h.id, h.child).then((fresh) => {
        if (editing !== h) return;
        // GUARD AT ARRIVAL: `fill' would shut an edit opened while the fetch flew.
        if (sheetOpen()) return;
        if (fresh.digest !== h.digest) {
          // The model the write was built from stands, so there is nothing to redraw.
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
      // A TIGHT BOX STANDS INSIDE THE ROW, which lifts its own wash (Style.hs).
      el(o.pane).classList.toggle("tight", !!o.tight);
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
    // ONE `edit' OVER FOUR SURFACES: an unscoped shut would cancel a rename.
    const editIn = (o) => !!edit && edit.o === o;
    function shutEdit(o) {
      if (!editIn(o)) return;
      el(edit.o.box).className = "";
      el(edit.o.pane).classList.remove("tight");
      for (const id of edit.o.fields) el(id).blur();
      edit = null;
      sizeDocEdit();
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
      // THE ROW VOUCHES FOR A TIGHT BOX'S VERTICAL: an empty slot's rect has no height
      // (docs/bugs/fixed/2026-08-25-the-title-box-sits-on-the-baseline-when-the-title-is-empty.md).
      const row = o.tight && tr.closest ? tr.closest(".de") : null;
      const rowed = row && typeof row.getBoundingClientRect === "function";
      const [padT, padB] = rowed ? rowPads(row) : [0, 0];
      const rr = rowed ? row.getBoundingClientRect() : a;
      // Absolute against the PADDING box: a scrolling pane owes clientTop+scrollTop.
      s.top = `${(rowed ? rr.top + padT : a.top) - b.top - pane.clientTop + pane.scrollTop}px`;
      s.height = `${rowed ? rr.height - padT - padB : a.height}px`;
      // THE BOX COVERS THE BLOCK ON EVERY EDGE, and EVERY field of a two-field box.
      if (o.block) {
        s.left = `${a.left - b.left - pane.clientLeft + pane.scrollLeft}px`;
        s.width = `${a.width}px`;
        for (const id of o.fields) inset(el(id), tr);
        return;
      }
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
    /** Return the ROW's vertical padding, measured ONCE PER ROW (style recalc). */
    let padsRow = null, padsTB = [0, 0];
    function rowPads(row) {
      if (padsRow !== row) {
        const c = typeof getComputedStyle === "function" ? getComputedStyle(row) : null;
        if (!c) return [0, 0];
        padsRow = row;
        padsTB = [parseFloat(c.paddingTop) || 0, parseFloat(c.paddingBottom) || 0];
      }
      return padsTB;
    }
    /** Pad FIELD the way ROW is, measured ONCE PER ROW (style recalc). */
    let insetRow = null, insetPad = "";
    function inset(field, row) {
      if (insetRow !== row) {
        const c = typeof getComputedStyle === "function" ? getComputedStyle(row) : null;
        if (!c || !c.paddingLeft) return;
        insetRow = row;
        insetPad =
          `${c.paddingTop} ${c.paddingRight} ${c.paddingBottom} ${c.paddingLeft}`;
      }
      if (field.style.padding !== insetPad) field.style.padding = insetPad;
    }
    // A declaration rather than a `const', so a direct `eval' leaks it.
    function cellSpan(keys, cols) {
      const at = (keys || []).map((k) => (cols || []).findIndex((c) => c.key === k));
      if (!at.length || at.some((i) => i < 0)) return null;
      return [Math.min(...at), Math.max(...at)];
    }
    // Typing is the third door; `placeEdit' after it, so a box that grew is re-laid.
    el("dtext").addEventListener("input", () => { sizeDocEdit(); placeEdit(); });
    window.addEventListener("resize", placeEdit);
    el("mdoc").addEventListener("scroll", placeEdit, true);
    // A LEFT CLICK SELECTS the row it lands on, a DOUBLE CLICK edits it, waiting on
    // no round-trip.  Clicks inside the open edit box carry no row and are ignored.
    const deUnder = (e) => (e.target instanceof Element ? e.target.closest("#mdoc .de") : null);
    const rowOfDe = (de) => (de ? drows.find((x) => x.id === de.getAttribute("data-id")) : null);
    const foldUnder = (e) => (e.target instanceof Element ? e.target.closest("#mdoc .fold") : null);
    const gtUnder = (e) => (e.target instanceof Element ? e.target.closest("#mdoc glance-table") : null);
    const tvOf = (host) => (host ? /** @type {any} */ (host)._tv : null);
    // The composite-id -> mounted-widget DOM path, spelled once.
    const hostFor = (compId) => el("mdoc").querySelector(`.de[data-id="${compId}"] glance-table`);
    const tableCompOfRow = (r) => {
      const comp = r && r.owner && drows.find((x) => x.id === r.owner);
      return comp && comp.name === "table" ? comp : null;
    };
    const tableHostCompId = (host) => {
      const de = host && host.closest ? host.closest(".de[data-id]") : null;
      return de ? de.getAttribute("data-id") : null;
    };
    // An org HLINE (a `|---+---|' rule row): a separator, no cells to edit.
    const isRuleLine = (t) => {
      const s = (t || "").trim();
      return s.startsWith("|") && /^[|+\-\s]+$/.test(s) && s.includes("-");
    };
    // COLUMNS FLAGGED FOR DELETION, dired-style: `d' flags, a second `d' deletes.
    let dcolFlags = new Set();
    const colFlagKey = (compId, col) => `${compId}:${col}`;
    // Re-stamp on every push (a re-render drops the classes); forget dead flags.
    const applyColFlags = () => {
      for (const el2 of document.querySelectorAll("#mdoc glance-table .gt-cflag"))
        el2.classList.remove("gt-cflag");
      for (const key of [...dcolFlags]) {
        const sep = key.lastIndexOf(":");
        const compId = key.slice(0, sep), col = +key.slice(sep + 1);
        const host = hostFor(compId);
        const ths = host ? host.querySelectorAll("thead th") : [];
        if (!host || col >= ths.length) { dcolFlags.delete(key); continue; }
        ths[col].classList.add("gt-cflag");
        for (const tr of host.querySelectorAll("tbody tr[data-id]")) {
          const td = tr.querySelectorAll("td")[col];
          if (td) td.classList.add("gt-cflag");
        }
      }
    };
    // SELECT THE CELL POINT STANDS IN: the widget's row ids ARE the Elm leaf ids.
    const tableSelSync = () => {
      for (const th of document.querySelectorAll("#mdoc glance-table thead th.gt-hsel"))
        th.classList.remove("gt-hsel");
      applyColFlags();
      const r = drows[dat];
      const comp = r && r.owner && drows.find((x) => x.id === r.owner);
      const host = comp && comp.name === "table" ? hostFor(comp.id) : null;
      // A TABLE SHOWS ITS ROW SELECTION ONLY while point stands on a BODY cell, and
      // a HEADERLESS one its ghost header only while point is on the ephemeral one.
      const onHead = dhead || dephem;
      for (const g of document.querySelectorAll("#mdoc glance-table")) {
        g.classList.toggle("gt-nosel", g !== host || onHead);
        g.classList.toggle("ephemeral", dephem && g === host);
      }
      const tv = tvOf(host);
      if (!tv || !host) return;
      // ON THE HEADER, mark the column's th; on a body row, select the cell.
      if (onHead) {
        if (dcol != null) {
          const th = host.querySelectorAll("thead th")[dcol];
          if (th) th.classList.add("gt-hsel");
        }
        return;
      }
      if (dcol == null) tv.select(r.id);
      else tv.select(r.id, dcol);
    };
    el("mdoc").addEventListener("click", (e) => {
      if (edit && e.target instanceof Node && el("dpara").contains(e.target)) return;
      // The renderer already set the cell selection as the event bubbled through.
      const gt = gtUnder(e);
      if (gt) {
        if (e.target instanceof Element
            && (e.target.closest("thead") || e.target.closest("a.tv-link"))) return;
        const tv = tvOf(gt);
        if (tv) {
          const s = tv.getSelection();
          if (s.id != null) { dsend({ kind: "selectcell", id: s.id, col: s.col }); }
        }
        return;
      }
      const de = deUnder(e), r = rowOfDe(de);
      if (!r || !de) return;
      // The spine sign names the row, so it folds without point reaching it.
      if (foldUnder(e)) {
        dsend({ kind: "fold", id: r.id });
        return;
      }
      const dpv = e.target instanceof Element ? e.target.closest(".dpv") : null;
      if (dpv && r.entries) {
        dsend({ kind: "select", id: r.id, plan: [...de.querySelectorAll(".dpv")].indexOf(dpv) });
      } else {
        dsend({ kind: "select", id: r.id });
      }
    });
    el("mdoc").addEventListener("dblclick", (e) => {
      if (foldUnder(e)) return;
      if (gtUnder(e)) return;
      const r = rowOfDe(deUnder(e));
      if (r) docEnter(r);
    });
    // The renderer hands up a raw target; the doc's `o' door follows it.
    el("mdoc").addEventListener("glance-open", (e) => {
      const detail = /** @type {CustomEvent} */ (e).detail;
      const target = detail && detail.target;
      if (!target || !editing) return;
      // Matched to the entry's own typed link for its type; unknown ones are urls.
      const link = dlinks.find((l) => l.target === target)
        || { target: String(target), desc: String(target), type: "url" };
      followLinks(docBinding("org-glance-overview:open"), editing.id,
                  { digest: editing.digest, links: [link] }, [link]);
    });
    // The WRITE is the doc's: a header on a HEADERLESS table materializes it.
    el("mdoc").addEventListener("glance-edit", (e) => {
      const d = /** @type {CustomEvent} */ (e).detail;
      const host = d && d.host, view = host && /** @type {any} */ (host).view;
      if (!view) return;
      if (d.kind === "header") {
        const cur = ((view.columns[d.col] || {}).header || "").trim();
        if (d.value.trim() === cur) { echo("RET → column unchanged"); return; }
        if (view.hasHeader) {
          // The header leaf is the composite's row that is neither body nor hline.
          const compId = tableHostCompId(host);
          const bodyIds = new Set(view.rows.map((x) => x.id));
          const head = drows.find((x) => x.owner === compId
                        && !bodyIds.has(x.id) && !isRuleLine(x.text));
          if (!head) return;
          dcommit = (cargo) => echo(`RET → ${cargo.said || "column named"}`);
          dsend({ kind: "editcell", id: head.id, col: d.col, text: d.value });
        } else {
          if (!d.value.trim()) { echo("RET → nothing named"); return; }
          const first = view.rows[0];
          if (!first) return;
          dcommit = (cargo) => echo(`RET → ${cargo.said || "header added"}`);
          dsend({ kind: "namecol", id: first.id, col: d.col, text: d.value });
        }
        return;
      }
      const row = view.rows.find((x) => x.id === d.id);
      const cur = row && row.cells ? String(row.cells["c" + d.col] || "") : "";
      if (d.value === cur) { echo("RET → cell unchanged"); return; }
      dcommit = (cargo) => echo(`RET → ${cargo.said || "cell written"}`);
      dsend({ kind: "editcell", id: d.id, col: d.col, text: d.value });
    });
    const docElAt = () => el("dlist").querySelector(".dat");
    const dTitleAt = () =>
      (docElAt() && docElAt().querySelector(".dc-title")) || docElAt();
    const DTITLE = {
      box: "dtitle", pane: "mdoc", fields: ["dtin"],
      mount: () => null, anchor: dTitleAt, tight: true,
      edge: () => docElAt() && docElAt().querySelector(".dc-tags"),
      fill: (r) => { el("dtin").value = r.val; },
      focus: () => el("dtin").focus(),
    };
    // Elm draws `own ++ deeper', so a nested row's own line is its FIRST child;
    // over a composite the two are one node and the edit rewrites the whole list.
    const dParaAt = () => {
      const at = docElAt();
      if (!at) return at;
      const kid = at.querySelector(".de"), own = at.children[0];
      return kid && own && own !== kid ? own : at;
    };
    const DPARA = {
      box: "dpara", pane: "mdoc", fields: ["dtext"],
      mount: () => null, anchor: dParaAt, block: true,
      // A fresh edit is a fresh walk: `drung' recounts from where the line stands.
      fill: (r) => { drung = null; el("dtext").value = r.text; sizeDocEdit(); },
      focus: () => el("dtext").focus(),
    };
    // A table cell is edited in the widget, so no doc overlay stands over a table.
    const DPAIR = {
      box: "dpair", pane: "mdoc", fields: ["dkey", "dval"],
      mount: () => null, anchor: dParaAt, block: true,
      fill: () => {
        el("dkey").value = ""; el("dval").value = "";
        askVocab();
        sizeDocEdit();
      },
      // `openEdit' fills before it focuses, so the fill drew the other half's list.
      focus: () => { el("dkey").focus(); pairMoved(); },
    };
    // `viewPlanning' gives each value its own span (`Doc.elm'); the box lies there.
    const dPlanAt = () => {
      const key = editIn(DDATE) ? edit.row.key : null;
      return key ? el("dlist").querySelector(`.dpv[data-key="${key}"]`) : null;
    };
    const DDATE = {
      box: "ddate", pane: "mdoc", fields: ["dwhen"],
      mount: () => null, anchor: dPlanAt, tight: true,
      // Placed synchronously -- `soon(placeEdit)' would land a frame late.
      fill: (r) => { el("dwhen").value = r.val; dateMoved(); placeEdit(); },
      focus: () => selectWhole(el("dwhen")),
    };
    const dediting = () => editIn(DTITLE);
    const dparaing = () => editIn(DPARA);
    const dpairing = () => editIn(DPAIR);
    const ddating = () => editIn(DDATE);
    // `edit' is shared with the table's rename, so this asks MEMBERSHIP of it.
    const DOCEDITS = [DTITLE, DPARA, DPAIR, DDATE];
    const sheetOpen = () => !!edit && DOCEDITS.indexOf(edit.o) !== -1;
    /** Answer the day the open edit reads against, stamped when it was summoned.
     * The ghost and the wall above the commit must never disagree on the day. */
    const editDay = () => (edit && edit.row.today) || dateNow();

    // A read-only debug surface for console and harness: `caret' is the box offset
    // no rendered row shows, and `rows' mirrors Elm (`D' is the open draft's row).
    window["__glance"] = Object.assign(window["__glance"] || {}, {
      editor() {
        const box = el("dtext");
        const drawn = drows.find((r) => r.id === "D") || null;
        const pill = el("echo");
        return {
          dtext: box ? box.value : null,
          caret: box ? box.selectionStart : null,
          caretEnd: box ? box.selectionEnd : null,
          editing: !!edit,
          box: edit ? edit.o.box : null,
          add: edit ? !!edit.row.add : null,
          lead: edit ? edit.row.lead : null,
          rowId: edit ? edit.row.id : null,
          rowAt: edit ? edit.row.at : null,
          drawnLead: drawn ? drawn.text : null,
          at: dat,
          planKey: dplankey,
          echo: pill ? pill.textContent : null,
          rows: drows.map((r) => ({ id: r.id, grain: r.grain, owner: r.owner,
                                    level: r.level, text: (r.text || "").slice(0, 40) })),
        };
      },
    });
    const onPairKey = () => active() === el("dkey");
    // The door answers `{ keys: {KEY: n}, values: {KEY: {VALUE: n}} }', or 404.
    let dvocab = null, dvocabAsked = false;
    function askVocab() {
      if (dvocabAsked) return;
      dvocabAsked = true;
      getJSON("/properties")
        .then((v) => { dvocab = v; if (dpairing()) drawOffers(); })
        .catch(() => {});
    }
    const OFFERS = 6;   // the cap, spelled once
    const PLAN_HINT = "planning";
    const valueOwesDate = () =>
      dpairing() && DATED.indexOf(planningWord(el("dkey").value.trim())) !== -1;
    function offersFor() {
      if (!dpairing()) return [];
      const onKey = onPairKey();
      // The key routes to planning, so the tree's property words fit no date half.
      if (!onKey && valueOwesDate()) return dateOffers(el("dval").value, editDay());
      const vocab = dvocab || {};
      // Org upcases a property key: the door is keyed upper, verbatim as fallback.
      const key = el("dkey").value.trim(), vals = vocab.values || {};
      const from = onKey ? (vocab.keys || {})
                         : (vals[key.toUpperCase()] || vals[key] || {});
      const typed = String(el(onKey ? "dkey" : "dval").value).trim();
      const want = typed.toLowerCase();
      const fits = (w) => w.toLowerCase().includes(want);
      const listed = Object.keys(from).filter(fits)
        .sort((a, c) => (from[c] - from[a]) || (a < c ? -1 : a > c ? 1 : 0));
      // `/properties' walks DRAWERS and the parser lifts planning off the headline,
      // so org's three ride out of `PLANNING' -- last, but never cut by the cap.
      const planned = onKey
        ? PLANNING.filter((w) => fits(w) && !listed.includes(w)) : [];
      // The reader's own line leads the offers, above the cap (AGENTS.hs); asked of
      // the WHOLE vocabulary, so a word the tree spells leads as its own entry.
      const words = listed.concat(planned);
      const minted = leadTyped(typed, words);
      const folds = minted || !typed
        ? null : words.find((w) => w.toLowerCase() === want);
      const under = planned.filter((w) => w !== folds);
      const shown = listed.filter((w) => w !== folds)
        .slice(0, Math.max(0, OFFERS - under.length)).concat(under);
      // Only the key half routes, so only its offers may say `planning'.
      const dress = (w) =>
        ({ word: w, hint: onKey && planningWord(w) ? PLAN_HINT : "" });
      return (minted ? [{ word: typed, hint: NEW_HINT }]
              : folds ? [dress(folds)] : []).concat(shown.map(dress));
    }
    function paintOffers(boxId, list, at) {
      const box = el(boxId);
      box.textContent = "";
      box.className = list.length ? "on" : "";
      list.forEach((o, i) => {
        const row = part(box, "div", i === at ? "dof dat" : "dof");
        part(row, "span", "dow", o.word);
        if (o.hint) part(row, "span", "dot", o.hint);
      });
    }
    const dmenu = { box: "doffer", list: [], at: -1 };   // `-1' is point on NO offer
    const wmenu = { box: "dwoffer", list: [], at: -1 };
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
    const menuTake = (m, field, moved) => {
      const want = m.at < 0 ? undefined : m.list[m.at].word;
      const f = el(field);
      if (want === undefined || want === f.value.trim()) return false;
      f.value = want;
      f.setSelectionRange(want.length, want.length);
      moved();
      return true;
    };
    function drawOffers() {
      dmenu.list = offersFor();
      menuPaint(dmenu);
    }
    /** Answer what a date-owed field offers over TEXT, each hinted with its date.
     * Resolved against TODAY; READ reuses a reading of TEXT the caller has. */
    function dateOffers(text, today, read) {
      const typed = String(text || "").trim();
      if (typed && (read || readsDate(typed, today)).ok) return [];
      const want = typed.toLowerCase();
      // A bare month is refused by the grammar, so month words wait for a day.
      const dayFirst = /^(\d{1,2})(?:[ \t]+(\S*))?$/.exec(want);
      let pool = DATE_VOCAB;
      if (dayFirst) {
        const day = dayFirst[1], frag = dayFirst[2] || "";
        pool = MONTH_FULL.filter((w) => w.indexOf(frag) === 0)
          .map((w) => `${day} ${w}`);
        if (frag === "" || "to".indexOf(frag) === 0) pool = [`${day} to `].concat(pool);
      }
      const fits = pool.filter((w) => !want || w.toLowerCase().indexOf(want) === 0)
        .slice(0, OFFERS);
      const dress = (w) => {
        const r = readsDate(w, today);
        return { word: w, hint: r.ok ? r.stamp : "…" };
      };
      // An empty field offers no literal: `RET' there means CLEAR.
      const lead = leadTyped(typed, fits) ? [{ word: typed, hint: NEW_HINT }] : [];
      return lead.concat(fits.map(dress));
    }
    /** Size F to LEN plus PLUS characters (`ch'), never past CAP, only on change. */
    const fitCh = (f, len, plus, cap) => {
      const w = `${Math.min(cap === undefined ? Infinity : cap, len + (plus || 0))}ch`;
      if (f.style.width !== w) f.style.width = w;
    };
    const pairMoved = () => {
      dmenu.at = el(onPairKey() ? "dkey" : "dval").value.trim() ? 0 : -1;
      drawOffers();
      // The key field hugs its text, so the closing colon stands flush.
      fitCh(el("dkey"), el("dkey").value.length);
      drawGhost("dval", "dvghost", valueOwesDate());
    };
    const takeOffer = () =>
      menuTake(dmenu, onPairKey() ? "dkey" : "dval", pairMoved);
    // An assigned value fires neither door, so those callers redraw by hand.
    for (const id of ["dkey", "dval"])
      for (const ev of ["input", "focus"]) el(id).addEventListener(ev, pairMoved);
    el("dwhen").addEventListener("input", () => {
      if (!ddating()) return;
      dateMoved();
    });
    /** Handle TAB, RET or `:' (key K) over the pair: offer, then hop or apply. */
    function pairKey(k) {
      const onKey = onPairKey();
      const took = takeOffer();
      // A key hands over whether or not an offer was taken, which is `:''s rule.
      if (onKey) { hop(); pairMoved(); return; }
      if (took) return;
      commitDocEdit(docBinding("org-set-property", k));
    }

    // ================================================ THE DATE WIDGET

    const GHOST_CAP = 46;
    function drawGhost(fieldId, ghostId, owes, read) {
      const f = el(fieldId), g = el(ghostId);
      const said = owes ? dateGhost(f.value, editDay(), read)
                        : { text: "", bad: false };
      g.className = said.bad ? "dgh bad" : "dgh";
      g.textContent = said.text;
      f.style.flex = owes ? "none" : "";
      if (owes) fitCh(f, Math.max(1, f.value.length), 1, GHOST_CAP);
      else f.style.width = "";
    }
    const verbatimOnly = () => ddating() && DATED.indexOf(edit.row.key) === -1;
    const readsWhen = (text) =>
      verbatimOnly() ? readsStamp(text, edit.row.key) : readsDate(text, editDay());
    /** Redraw the ghost and the offers.  ONE READING PER KEYSTROKE, handed to both. */
    function dateMoved() {
      const typed = el("dwhen").value.trim(), today = editDay();
      const only = verbatimOnly();
      const r = readsWhen(typed);
      wmenu.at = typed ? 0 : -1;
      wmenu.list = only ? [] : dateOffers(typed, today, r);
      menuPaint(wmenu);
      drawGhost("dwhen", "dghost", true, r);
    }
    /** Re-assert the VIRGIN selection: the port lands a macrotask behind the open. */
    function reselectDate() {
      if (!dateVirgin()) return;
      selectWhole(el("dwhen"));
    }
    /** Is the widget on the open's own selection (`laidWhole', 00-core.js)? */
    const dateVirgin = () => ddating() && laidWhole(el("dwhen"));
    const dateBinding = (k) => docBinding(edit.row.b.command, k);
    const PLAN_COMMANDS = { SCHEDULED: "org-glance-overview:schedule",
                            DEADLINE: "org-glance-overview:deadline" };
    const planCommand = (key) => PLAN_COMMANDS[key] || "org-add-planning-info";
    function planHere(b, keyword) {
      if (!editing || raw) { said(b, "no document here"); return; }
      if (ddating()) restoreSheetEdit();
      else if (sheetOpen())
        { said(b, "an edit is open — RET writes it, ESC leaves"); return; }
      const r = docRowAt();
      if (r && r.kind === "child") { into(r.index, () => summonPlan(b, keyword)); return; }
      summonPlan(b, keyword);
    }
    function summonPlan(b, keyword) {
      const at = dplan.find((p) => p[0] === keyword);
      const stood = at ? at[1] : "";
      const drew = !at;
      // Read BEFORE the draft moves point: the port lands a macrotask later.
      const back = docCursor().at;
      if (drew) redraftPlan(keyword);
      // ONE CLOCK READ PER SUMMON: ghost, offers and commit all read this day.
      openEdit(DDATE, { key: keyword, val: stood, add: drew, back, b,
                        today: dateNow() });
      said(b, "RET sets it · empty clears it · ESC leaves");
    }
    function dateKey(b) {
      if (menuTake(wmenu, "dwhen", dateMoved)) return;
      const typed = el("dwhen").value.trim();
      if (typed) {
        const r = readsWhen(typed);
        if (!r.ok) { said(b, r.why); return; }
      }
      commitDate(b, typed);
    }
    /** Send TYPED verbatim: ONE CLOCK READ, the server's own (docs/invariants.md).
     * A CHILD has no row id and rides `?child='. */
    function commitDate(b, typed) {
      const row = edit.row, keyword = row.key, h = editing;
      shutEdit(DDATE);
      if (row.add) undraftPlan(row);
      if (h.child !== null) {
        answerOnce(() => said(b, typed || "cleared"), (what) => said(b, what));
        dsend({ kind: "addprop", key: keyword, value: typed });
        return;
      }
      fire(b, "set-planning", [h.id], { keyword, date: typed || null },
           typed || "cleared")
        .then((results) => {
          if (editing === h && (results || []).some((x) => x.ok)) reload();
        });
    }
    function dateAdjust(b, by) {
      const f = el("dwhen");
      const r = readsWhen(f.value.trim());
      if (!r.ok || !r.start) { said(b, "no date here to move"); return; }
      f.value = dateStepped(r, addDays(r.start, by));
      f.setSelectionRange(f.value.length, f.value.length);
      dateMoved();
    }
    const docHolds = () => editing !== null;
    const paraBinding = docBinding("org-ctrl-c-ctrl-c", "RET");
    const quitBinding = docBinding("quit-window", "q");
    const DOCROWS = 10;   // at most N lines, and the block is what grows
    /** The rows the open edit's text OCCUPIES; `scrollHeight' needs a flat box. */
    const docRowsDrawn = () => {
      const t = el("dtext"), s = t.style;
      if (typeof getComputedStyle !== "function" || !t.scrollHeight) return 0;
      const flex = s.flex, height = s.height;
      s.flex = "none"; s.height = "0px";
      const cs = getComputedStyle(t);
      const lh = parseFloat(cs.lineHeight) || 0;
      const pad = parseFloat(cs.paddingTop) + parseFloat(cs.paddingBottom);
      const rows = lh > 0 ? Math.round((t.scrollHeight - pad) / lh) : 0;
      s.flex = flex; s.height = height;
      return rows;
    };
    // Counting org's newlines alone left the box a line short over a wrapped item.
    const sizeDocEdit = () => el("mdoc").style.setProperty("--g-doc-rows",
      String(dpairing()
        ? 1
        : dparaing()
        ? clamp(Math.max(el("dtext").value.split("\n").length,
                         docRowsDrawn()), 1, DOCROWS)
        : 0));
    function newlineIn(id) {
      const box = el(id);
      const o = CONT.exec((box.value || "").split("\n")[0]);
      const under = o ? " ".repeat(o[1].length) : "";
      spliceIn(box, box.selectionStart, box.selectionEnd, "\n" + under);
      // Setting `value' fires no `input', so the newline places the box itself.
      sizeDocEdit();
      placeEdit();
    }
    const caretLine = (id) => {
      const box = el(id);
      return box.value.slice(0, box.selectionStart).split("\n").length - 1;
    };
    const PLANNING = CFG.planning;
    const DATED = CFG.settable;
    // Written as a key, a frame word TERMINATES the drawer -- AGENTS.hs.
    const DRAWER_FRAME = ["PROPERTIES", "END"];
    const IDENTITY_KEYS = ["ORG_GLANCE_ID", "ORG_GLANCE_CREATION_TIME"];
    const planningWord = (key) => {
      const up = String(key || "").toUpperCase();
      return PLANNING.indexOf(up) === -1 ? null : up;
    };

    /** Why this pair is not written, or `null'.  Asked above the shut, since a
     * wall the model alone knew would land with nothing left to fix it in. */
    function pairRefused(key, value) {
      if (!key) return "a key is required";
      if (/[\s:]/.test(key)) return "a key holds no spaces and no colons";
      const up = key.toUpperCase();
      if (DRAWER_FRAME.indexOf(up) !== -1)
        return `:${up}: frames the drawer — writing it would end the drawer here`;
      if (IDENTITY_KEYS.indexOf(up) !== -1)
        return `:${up}: is the store's own — this would forge the headline's identity`;
      if (!value) return "a value is required";
      // THE WALL'S OWN SENTENCE: the write would 409 with nothing left to fix it in.
      if (planningWord(up)) {
        if (DATED.indexOf(up) !== -1)
          return readsDate(value, editDay()).ok
            ? null : `${up} is not a date org would read back`;
        if (!STAMP.test(value)) return notReadBack(up);
      }
      return null;
    }
    // Org's own `:KEY: value'.  A LINE THAT OPENS NEITHER READS AS TWO EMPTY HALVES.
    const PAIRKEY = /^\s*:([^\s:]+):\s*(.*)$/;
    const pairIn = (text) => PAIRKEY.exec(text) || ["", "", ""];
    const migrating = (r, text) =>
      r.kind === "meta" ? planningWord(pairIn(text)[1]) : null;
    /** Commit the open edit, and with AT put another stop in under it — `S-RET''s
     * whole difference from `RET'.  AT is the CARET'S LINE, read at the press. */
    function commitDocEdit(b, at) {
      const spoke = (what) => (b ? said(b, what) : echo(`RET → ${what}`));
      const more = () => { if (at != null) soon(() => insertHere(at)); };
      if (!edit) return;
      const r = edit.row;
      // THROUGH ITS OWN DOOR, or `C-c C-c' falls through and retitles with a date.
      if (editIn(DDATE)) { dateKey(b || dateBinding("RET")); return; }
      if (editIn(DPARA)) {
        const text = el("dtext").value;
        const add = !!r.add;
        // ONE WALL, BOTH DOORS: a `:SCHEDULED:' line committed here meets that
        // refusal above the shut.  AN EMPTIED VALUE CLEARS — only this door can.
        const going = add ? null : migrating(r, text);
        const worth = going ? pairIn(text)[2].trim() : "";
        if (worth) {
          const no = pairRefused(going, worth);
          if (no) { spoke(no); return; }
        }
        shutEdit(DPARA);
        if (add) {
          // WHAT THE BOX HOLDS IS WHAT IS WRITTEN; Elm prepends nothing.
          const lead = r.lead || "";
          if (!text.trim() || text === lead) { undraft(r); spoke("nothing added"); return; }
          insertPara(r, text, () => {
            spoke(lead ? "item added" : "paragraph added");
            more();
          });
          return;
        }
        // A PAIR THAT WOULD MIGRATE IS NEVER UNCHANGED: it has somewhere to go.
        if (text === r.text && !migrating(r, text))
          { spoke("paragraph unchanged"); more(); return; }
        editPara(r, text, (cargo) => {
          spoke(cargo.said || "paragraph written");
          more();
        });
        return;
      }
      if (editIn(DPAIR)) {
        const key = el("dkey").value.trim(), value = el("dval").value.trim();
        const no = pairRefused(key, value);
        if (no) { spoke(no); return; }
        shutEdit(DPAIR);
        answerOnce((cargo) => spoke(cargo.said || "property written"), spoke);
        dsend({ kind: "addprop", key, value });
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
    const redraft = (r) => dsend({ kind: "draft", id: r.id });
    const undraft = (r) => dsend({ kind: "undraft", id: r.id });
    const redraftPair = () => dsend({ kind: "draftpair" });
    const undraftPair = (r) => dsend({ kind: "undraftpair", id: r.id });
    const redraftPlan = (keyword) => dsend({ kind: "draftplan", key: keyword });
    const undraftPlan = (r) => dsend({ kind: "undraftplan", id: r.back });
    /** Take the open sheet edit down, restoring what it DREW; name what stood
     * there.  Silent: the caller owns the echo. */
    function restoreSheetEdit() {
      const drawn = editIn(DPARA) && edit.row.add ? edit.row : null;
      const pair = editIn(DPAIR) ? edit.row : null;
      const when = editIn(DDATE) ? edit.row : null;
      for (const o of DOCEDITS) shutEdit(o);
      if (drawn) undraft(drawn);
      if (pair) undraftPair(pair);
      if (when && when.add) undraftPlan(when);
      return when ? "the planning line" : pair ? "the drawer" : "element";
    }
    const cancelSheetEdit = () => cancelEdit(restoreSheetEdit());

    function ddelete(ids, how) {
      dtook = how;
      dsend({ kind: "delete", ids });
    }
    /** What a delete came back with: `ddelete' left the wording here. */
    function took(answer) {
      const how = dtook;
      dtook = null;
      if (!how) return;
      // Props and planning lines leave through the LISTS, counted beside the body's.
      if (answer.refused)
        append("sync", "warn",
               "a headline is not deleted from the sheet — this writes elements only");
      const n = answer.taken.length + answer.meta;
      if (!n) { echo(`D → org-delete-element (${how(0)})`); return; }
      commitDocWith(answer,
        () => echo(`D → org-delete-element (${how(n)} taken)`));
    }
    // THE CARGO IS THE CALLER'S: a flush reading mirrors would race the push.
    function commitDocWith(cargo, say) {
      if (!editing) return;
      const h = editing;
      sync("syncing");
      post(h.id, h.digest,
           { body: cargo.body, properties: cargo.properties, planning: cargo.planning },
           null, h.child)
        .then(outcome)
        .then((a) => { if (editing === h && landed(h, say)(a)) reload(); })
        .catch((e) => stuck(subtreeSheet, e.message));
    }
    function docClear() {
      dlinks = [];
      dsend({ kind: "clear" });
    }
    function docFill(h, isRaw) {
      dlinks = h.links || [];
      if (isRaw) { dsend({ kind: "clear" }); return; }
      // CONTENT SITS UNDER THE TITLE TEXT; the arithmetic is the stylesheet's.
      el("mdoc").style.setProperty("--g-doc-indent", String("* ".length));
      const body = String(h.body || "");
      dsend({ kind: "fill",
              lines: body.split("\n"),
              own: h.ownLines === undefined ? body.split("\n").length : h.ownLines,
              props: h.properties || [],
              plan: h.planning || [],
              planKeys: PLANNING,
              // SCHEDULED and DEADLINE draw as unset slots; CLOSED keeps none.
              planSlots: DATED,
              cells: cellsOf(h.cells),
              kids: (h.children || []).map((c) =>
                ({ index: c.index, level: c.level, line: c.line,
                   cells: cellsOf(c) })),
              links: dlinks.map((l) =>
                ({ from: l.span[0], to: l.span[1], desc: l.desc })),
              spanAt: (h.span || {}).start ?? null,
              shift: bodyShift(h),
              level: h.level || 1,
              titleAt: typeof h.titleAt === "number" ? h.titleAt : null });
    }
    const docCursor = () => ({ at: drows[dat] ? drows[dat].id : null });
    function docRestore(at) {
      dsend({ kind: "select", id: at });
      const back = drows.findIndex((r) => r.id === at);
      if (back !== -1) dat = back;
    }
    const docRowById = (id) => drows.find((x) => x.id === id);
    const checkboxHere = () => checkboxAt(drows[dat]);
    // THE SHELL'S SMALL LISTS ARE ONE ELM PROGRAM, one per surface — AGENTS.hs.
    function listing(host, cols, hint, pane) {
      // `Browser.element' REPLACES its node, so HOST survives as the anchor container.
      const ports = Elm.Listing.init({ node: part(el(host), "div", ""),
                                       flags: { cols, hint: hint || "" } }).ports;
      const seen = { at: -1, id: "", ids: [], flags: [], narrow: null, all: 0 };
      /** @returns {(HTMLInputElement & HTMLElement) | null} */
      const narrowBox = () =>
        /** @type {any} */ (el(host).querySelector("input.tv-filter"));
      let owed = false;
      ports.listState.subscribe((now) => {
        Object.assign(seen, now);
      // ELM PUSHES ITS STATE BEFORE IT PAINTS: the field is reachable a turn later.
        if (!owed || seen.narrow === null) return;
        owed = false;
        soon(() => { const b = narrowBox(); if (b) b.focus(); });
      });
      // Caught in the CAPTURE phase, so the scroller inside PANE need not be named.
      if (pane) el(pane).addEventListener("scroll", placeEdit, true);
      // SEEDED WITH WHAT IS BEING SENT: a port round trip costs a macrotask.
      const landed = (id) => {
        const at = seen.ids.indexOf(id);
        if (at === -1) return;
        seen.at = at; seen.id = id;
      };
      const send = (m) => ports.listIn.send(m);
      return {
        ...flagPort(send, () => seen.flags),
        get el() { return el(host); },
        at: () => seen.at,
        onClick: (f) => ports.listClicked.subscribe(f),
        setRows: (rows, at) => {
          seen.ids = rows.map((r) => r.id);
          if (at) landed(at);
          send({ kind: "setRows", rows, at: at === undefined ? null : at });
        },
        select: (id) => { landed(id); send({ kind: "select", id }); },
        getSelection: () => ({ id: seen.id || null }),
        narrowing: () => seen.narrow,
        narrowBox,
        counted: () => ({ shown: seen.ids.length, all: seen.all }),
        openNarrow: () => {
          seen.narrow = seen.narrow || "";
          owed = true;
          send({ kind: "narrow", text: seen.narrow });
        },
        shutNarrow: () => {
          const b = narrowBox();
          if (b) b.blur();
          seen.narrow = null;
          send({ kind: "narrow", text: null });
        },
      };
    }
    /** `/' NARROWS A SMALL LIST, one gesture over every mount — AGENTS.hs. */
    const narrows = (m) => can(m, "openNarrow", "shutNarrow", "narrowing");
    const narrowed = (m) => narrows(m) && m.narrowing() !== null;
    const narrowTyping = (m) => narrowed(m) && active() === m.narrowBox();
    const narrowBinding = (k) => ({ seq: k, command: "filter-rows" });
    const unnarrow = (m) => { if (narrowed(m)) m.shutNarrow(); };
    const widen = (m, k) => {
      if (!narrowed(m)) return false;
      m.shutNarrow();
      keySaid(k)("keyboard-quit (narrow cleared)");
      return true;
    };
    /** The press over M, and whether it was spent.  WHILE THE FIELD HOLDS THE KEYS
     * exactly four are claimed: `RET', `C-n'/`C-p' and the vertical arrows. */
    function narrowPress(k, m) {
      if (!narrows(m)) return false;
      if (!narrowTyping(m)) {
        if (k !== "/") return false;
        m.openNarrow();
        said(narrowBinding(k), "");
        return true;
      }
      const step = walkStep(k);
      if (step) { stepIn(m, step); return true; }
      if (k !== "RET") return false;
      const box = m.narrowBox();
      if (box) box.blur();
      const n = m.counted();
      said(narrowBinding(k), `${n.shown} of ${n.all}`);
      return true;
    }
    // Registers AHEAD of the dispatch, so it sees a key first — AGENTS.hs.
    // Without the guard the sheet claims the letter a palette was raised to read.
    onKeys(() => editing && !raw && !momentary(), (k, e) => {
      // ANY OTHER KEY STARTS `C-l''s CYCLE OVER, org's own rule for it.
      if (k !== "C-l") recentres = 0;
      const once = (act) => { if (!repeating(e)) act(); };
      // THE PAIR TAKES FOUR KEYS: the walk, TAB, RET, and `:', which hands a KEY
      // over to its value, swallowed.  TAB out would unfocus a box still open.
      if (ddating()) {
        const by = dateStep(k);
        if (by) { e.preventDefault(); dateAdjust(dateBinding(k), by); return; }
        const walk = walkStep(k);
        if (walk) { e.preventDefault(); once(() => menuWalk(wmenu, walk)); return; }
        if (k !== "TAB" && k !== "RET") return;
        e.preventDefault();
        if (k === "RET") once(() => dateKey(dateBinding(k)));
        return;
      }
      if (dpairing()) {
        const step = walkStep(k);
        if (step) { e.preventDefault(); once(() => menuWalk(dmenu, step)); return; }
        if (k !== "TAB" && k !== "RET" && !(k === ":" && onPairKey())) return;
        e.preventDefault();
        once(() => pairKey(k));
        return;
      }
      if (dparaing()) {
        if (k === "RET") { e.preventDefault(); once(() => commitDocEdit(paraBinding)); }
        else if (k === "S-RET")
          { e.preventDefault(); once(() => commitDocEdit(NEXT, caretLine("dtext"))); }
        else if (k === "M-RET") { e.preventDefault(); newlineIn("dtext"); }
        // THE BROWSER WOULD TAKE THE FOCUS OUT OF THE BOX, so the key is claimed.
        else if (k === "TAB") { e.preventDefault(); once(tabRung); }
        return;
      }
      // `q' IS `quit-window' ONE WINDOW IN, dead inside an open edit.
      if (k === "q" && !dediting()) {
        e.preventDefault();
        once(() => { said(quitBinding, ""); leaveSheet(); });
        return;
      }
      if (dediting()) {
        if (k === "RET") once(commitDocEdit);
        else if (k !== "TAB") return;   // ESC is the keymap's, puts the element back
      } else {
        const step = rowStep(k), depth = grainStep(k);
        if (step) docStep(step, k);
        else if (depth > 0) dsay(k, { kind: "finer" });
        else if (depth < 0) dsay(k, { kind: "broader" });
        // `B' climbs the grain to the owner; `b' is `f' reversed and steps back.
        else if (k === "B") dsay(k, { kind: "climb" });
        else if (k === "RET") once(docEnter);
        else if (k === "DEL") once(docUp);
        else if (k === "TAB")
          once(() => dsay(k, { kind: "tab" }));
        else if (k === "S-<up>" || k === "S-<down>")
          once(() => atElement(() => cycleHere(k === "S-<up>" ? 1 : -1)));
        else if (k === "o" || k === "!") once(openHere);
        // THE BROWSER OWNS `C-l' FOR ITS ADDRESS BAR, so the key is claimed.
        else if (k === "C-l") once(() => recenterHere(k));
        else if (k === "M-<left>" || k === "M-<right>")
          once(() => shiftHere(k, k === "M-<right>" ? 1 : -1));
        else if (k === "t") once(() => atElement(stateHere));
        else if (k === ":") once(() => atElement(tagsHere));
        else if (k === "SPC")
          once(() => toggleCheckbox(docBinding("org-toggle-checkbox", "SPC")));
        // `S-RET' IS `+' WHEREVER IT IS PRESSED; none of the three reads a caret here.
        else if (k === "+" || k === "S-RET" || k === "M-RET") once(insertHere);
        // `d' FLAGS a selected column, dired-style; a second `d' deletes it.
        else if (k === "d" && dcol != null && tableCompOfRow(docRowAt())) once(() => {
          const r = docRowAt(), comp = tableCompOfRow(r);
          const key = colFlagKey(comp.id, dcol);
          if (dcolFlags.has(key)) {
            // The columns shift on a delete, so this table's flags are spent.
            dcolFlags = new Set([...dcolFlags].filter((k2) => !k2.startsWith(`${comp.id}:`)));
            dcommit = (cargo) => echo(`d → ${cargo.said || "column deleted"}`);
            dsend({ kind: "delcol", id: r.id, col: dcol });
          } else {
            dcolFlags.add(key);
            applyColFlags();
            echo("d → column flagged (d again deletes)");
          }
        });
        else if (!flagPress(k, e, DFLAGS)) return;
      }
      e.preventDefault();
    });
    const unlogged = () => {};
    /** WHAT EVERY DELETING SURFACE SAYS THE SAME WAY, spread into each shape. */
    const FLAG_WORDS = {
      note: unlogged,
      missing: lacks("delete flags"),
      idle: "dired-do-flagged-delete (no deletions requested)",
      unflag: "delete-unflag (flag cleared)",
    };
    const DFLAGS = {
      ...FLAG_WORDS,
      mount: () => dmount, take: ddelete,
      walk: () => docStep(1),
      missing: "this document has no flags",
      none: "org-delete-element (no element)",
      verb: "delete",
      flag: "delete-flag (d again deletes)",
      at: () => docCursor().at,
    };
    // Held-key guard here: `ONCE' governs dispatch rows, these four live outside.
    const flagPress = (k, e, shape) => {
      if (k !== "d" && k !== "D" && k !== "u" && k !== "x") return false;
      if (!repeating(e)) flagKey(k, shape, keySaid(k));
      return true;
    };
    const asked = () => raw
      ? { org: el("mtext").value }
      : { body: dbody, properties: dprops, planning: dplan };
    // ONE BUTTONLESS SHEET, twice over: each sheet supplies the verbs — AGENTS.hs.
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
    const activeSheet = () =>
      (editing ? subtreeSheet : settings ? configSheet : null);
    // ONE SHORTHAND PER SHEET: reaching for another's moves a state you do not own.
    const sync = (next, message) => note(subtreeSheet, next, message);
    function shut() {
      el("modal").className = ""; editing = null; base = ""; baseProps = null;
      soon(remembered);
      for (const o of DOCEDITS) shutEdit(o);
      docClear();
      el("mdoc").className = "";
      dvocab = null; dvocabAsked = false;
    }
    function flush(digest) {
      const h = editing, sent = asked();
      sync("syncing");
      return post(h.id, digest, sent, null, h.child)
        .then(outcome)
        .then(landed(h, () => {
          base = raw ? sent.org : base;
          baseProps = raw ? null : stamp(sent.properties, sent.planning);
        }))
        .catch((e) => { stuck(subtreeSheet, e.message); return false; });
    }
    function saveSheet(b) {
      if (sheetOpen()) { commitDocEdit(b); return; }
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
    // CALLED at click time: the wrapped widget's `const' is in TDZ while this runs.
    const backdrops = [["links", () => shutLinks()], ["tags", () => shutTags()]];
    for (const [id, off] of backdrops)
      el(id).addEventListener("click",
        (e) => { if (e.target === el(id)) off(); });
    // Re-materializes here, which keeps an org parser off this page.
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
      "add-link": (args) =>
        `linked → ${args.target}${args.kind ? ` (${args.kind})` : ""}`,
      "set-title": (args) => `retitled ${JSON.stringify(args.title)}`,
      "set-priority": (args) =>
        (args.priority ? `priority [#${args.priority}]` : "priority cleared"),
      archive: () => "archived",
      "add-tag": (args) => `tagged :${args.tag}:`,
      "remove-tag": (args) => `untagged :${args.tag}:`,
      "rename-tag": (args) => `retagged ${args.from}→${args.to}`,
      "set-planning": (args) =>
        `${args.keyword.toLowerCase()} ${args.date || "cleared"}`,
      "set-state": (args) => (args.keyword ? `→ ${args.keyword}` : "state cleared"),
      delete: () => "deleted",
    };
    // The caller's own word where no entry names one — every command names one.
    const verbed = (name, args, verb) => (VERBED[name] || ((_args, v) => v))(args, verb);
    const cellTags = (cell) => String(cell || "").split(":").filter(Boolean);
    function fire(b, name, ids, args, verb, how, pin) {
      return postCommand({ name, ids, args, digests: pin }).then((answer) => {
        const results = answer.results || [];
        // The store lags this write, so the per-id 200's digest re-pins the sheet.
        if (editing) {
          const held = results.find((x) => x.ok && x.id === editing.id && x.digest);
          if (held) editing.digest = held.digest;
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
    /** ONE SETTLE, TWO WATCHES, EACH ON ITS OWN RULE: the ANCHOR is spent every
     * time, so it describes one watch step and outlives no other, while the
     * ARRIVING id is HELD until the row it names is visible. */
    function settled() {
      arrived();
      const want = leaving;
      leaving = null;
      if (!want || !table) return;
      if (pageNow() !== want.on) return;
      if (visible().some((r) => r.id === want.from)) return;
      land({ id: want.id, col: column() }, want.at);
    }
    /** HOW MANY SETTLES A ROW HAS TO ARRIVE IN.  A capture the standing filter
     * hides never comes, and an id left standing would take the NEXT write's
     * settle with it.  The id it is counting is held beside the count, so a
     * fresh arrival starts its own wait. */
    const ARRIVAL_SETTLES = 10;
    let awaited = null, arrivals = 0;
    /** POINT ONTO THE ROW A WRITE PLACED, once it is there to stand on.  A
     * capture's row arrives BEHIND its own 200 — `/command' publishes nothing,
     * so the watch's nudge is what reloads the store — and the FIRST settle that
     * carries the row is the one that spends this.  HELD UNTIL THEN, and dropped
     * with the view it belonged to (`commit', 00-core.js) or after
     * `ARRIVAL_SETTLES' settles that never held it. */
    function arrived() {
      if (!arriving || !table) return;
      if (awaited !== arriving) { awaited = arriving; arrivals = 0; }
      if (!visible().some((r) => r.id === arriving)) {
        arrivals += 1;
        if (arrivals >= ARRIVAL_SETTLES) arriving = null;
        return;
      }
      const want = arriving;
      arriving = null;
      land({ id: want, col: column() });
    }
    // MINE is compared, since two archives can be out at once.
    const spent = (mine) => (results) => {
      if (mine && leaving === mine
          && !(results || []).some((x) => x.ok && x.id === mine.from))
        leaving = null;
      unmark(results);
    };
    // THE TAG DECIDES WHAT `D' MEANS; a MIXED set archives.  The cell is `:a:b:'.
    const ARCHIVE = CFG.archiveTag;
    const archivedRow = (id) =>
      String((rowOf(id).cells || {}).tag || "").split(":").indexOf(ARCHIVE) !== -1;
    function archive(b, ids, how) {
      if (ids.length && ids.every(archivedRow)) { confirmDelete(b, ids, how); return; }
      leaving = anchorFor(ids);
      fire(b, "archive", ids, {}, "archived", how)
        .then(spent(leaving)).catch(failed(b, "archive"));
    }
    const DELETE_WORD = "DELETE";
    function confirmDelete(b, ids, how) {
      askText(`delete · ${rowsWord(ids.length)} permanently`,
              `type ${DELETE_WORD} and RET · ESC leaves them`,
              (c) => {
                if (c.text.trim().toUpperCase() !== DELETE_WORD) {
                  said(b, "not deleted");
                  return;
                }
                leaving = anchorFor(ids);
                fire(b, "delete", ids, {}, "deleted", how)
                  .then(spent(leaving)).catch(failed(b, "delete"));
              });
    }
    const XFLAGS = (b) => ({
      mount: () => table, at: focusedId, walk: () => move(1),
      take: (ids, how) => archive(b, ids, how),
      note: (id, on) =>
        noted(id, on ? "marked for deletion" : "unmarked for deletion"),
      missing: lacks("archive flags"),
      none: "no row",
      idle: "no deletions requested",
      verb: "archive",
      unflag: "flag cleared",
      flag: "flagged — d again archives",
    });
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
      // AWAITED singly: two landing values in one FILE are two writes under one lock.
      for (const [key, over] of groups)
        await fire(b, "set-priority", over, { priority: key || null },
                   key ? `[#${key}]` : EMPTY).catch(failed(b, "set-priority"));
    }

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
      // Toggle it: the class also carries the sheet's size tier.
      el("sheet").classList.toggle("raw", raw);
      for (const o of DOCEDITS) shutEdit(o);
      docFill(h, raw);
      el("mdoc").className = raw ? "" : "on";
      drawWhere(h.path || []);
      drawLog(raw ? "" : h.logbook || "");
      // THE BASELINE COMES OFF THE FILL ITSELF: the mirrors land a macrotask
      // behind the push, so reading them here called every fresh sheet dirty.
      baseProps = raw ? null : stamp(h.properties, h.planning);
    }
    // ONE SPELLING for the baseline and the reading, or the two drift into dirt.
    const stamp = (props, plan) => JSON.stringify([props || [], plan || []]);
    const edited = () => stamp(dprops, dplan);
    // An org link reads as its DESCRIPTION, or its target where it has none.
    const linkText = (s) =>
      String(s || "").replace(/\[\[([^\]]*)\](?:\[([^\]]*)\])?\]/g, (_, tgt, desc) => desc || tgt);
    function drawWhere(path) {
      const bar = el("mwhere");
      bar.textContent = "";
      path.forEach((title, i) =>
        part(bar, "span", "wc" + (i === path.length - 1 ? " wat" : ""),
             linkText(title) || "(untitled)"));
    }
    // Display-only: the file keeps the whole drawer, delimiters and all.
    function drawLog(text) {
      const inner = text.replace(/\n$/, "").split("\n").slice(1, -1).join("\n");
      el("mlog").textContent = inner;
      el("mlog").className = inner ? "on" : "";
    }
    const dirty = () => editing !== null
      && (raw ? el("mtext").value !== base : edited() !== baseProps);

