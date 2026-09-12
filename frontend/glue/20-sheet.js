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
    // A CAPTURE IS THE SHEET OVER A SUBTREE THAT DOES NOT EXIST YET -- a DRAFT.
    const capturing = () => !!(editing && editing.capture);
    const CAPTURE_WORD = "the capture";
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
    // A DRAFT WHOSE `%?' STOOD IN THE BODY still owes its editor: that row lands
    // a macrotask behind the fill, so the open waits.  ONE SHOT.
    let dlanding = false;
    // What the reader typed per keyword for a RESOLVED entry; a fill empties it.
    let dtyped = {};
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
          openLanding(); tableSelSync();
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
      ((editing && editing.cells && editing.cells.title)
       || (capturing() ? CAPTURE_WORD : (editing || {}).id) || "");
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
    /** THE TITLE EDIT over the head row.  OVER A BARE DRAFT IT IS THE CAPTURE
     * ITSELF: `RET' writes the jot, `ESC' drops it, and the row says so as `bare'. */
    const openTitle = (val) =>
      openEdit(DTITLE,
               { id: "CELL:title", val, bare: capturing() && bareDraft(editing) });
    /** Is the draft the BARE DEFAULT -- star-space and nothing else?  Read off
     * the ANSWER, INHERITED facts counting; the destination tag is its ADDRESS. */
    const bareDraft = (h) =>
      !String((h.cells || {}).title || "").trim()
      && !String((h.cells || {}).state || "")
      && !String((h.cells || {}).priority || "")
      && !tagsBeyond(h).length
      && !(h.properties || []).length && !(h.planning || []).length
      && !(h.children || []).length && !bodyBelow(h.body).trim();
    const tagsBeyond = (h) =>
      cellTags((h.cells || {}).tags).filter((t) => t !== (h.capture || {}).tag);
    // A subtree's `body' opens with its own headline, so the first line is cut.
    const bodyBelow = (body) => String(body || "").split("\n").slice(1).join("\n");
    /** THE EDITOR A DRAFT'S BODY POINT OWES, once the fill that placed it has
     * settled.  A row no editor claims keeps point and opens nothing. */
    function openLanding() {
      if (!dlanding) return;
      dlanding = false;
      if (!capturing()) return;
      const r = drows[dat];
      if (!r || r.kind === "head") {
        openTitle(String((editing.cells || {}).title || ""));
        return;
      }
      if (r.kind === "para" || r.kind === "meta") openEdit(DPARA, r);
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
      if (capturing()) { echo("DEL → a capture has nowhere up — ESC leaves it"); return; }
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
     * A CHILD or a DRAFT has no row id and rides `?child='. */
    function commitDate(b, typed) {
      const row = edit.row, keyword = row.key, h = editing;
      shutEdit(DDATE);
      if (row.add) undraftPlan(row);
      if (h.child !== null || capturing()) {
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
    const dateStep = (k) =>
      k === "S-<right>" ? 1 : k === "S-<left>" ? -1
      : k === "S-<down>" ? 7 : k === "S-<up>" ? -7 : 0;
    const dateStepped = (r, to) => {
      if (!r.bracketed) return isoDay(to);
      const stood = r.stamp;
      const head = stood.indexOf("--") === -1 && STAMP_HEAD.exec(stood);
      // The tail excludes the closing bracket: the one org-stamp writer adds it.
      return stampOf(to, null, stood.charAt(0) === "[",
                    head ? stood.slice(head[0].length, -1) : "");
    };
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
    // Month and day RANGE-CHECKED; the lookahead stops `32' reading as day `3'.
    const DATE = "\\d+-(?:0?[1-9]|1[0-2])-(?:0?[1-9]|[12]\\d|3[01])(?!\\d)";
    // ONE ORG STAMP, or two joined by `--' wearing the SAME bracket; kept no looser
    // than the server's wall (`settledPlanning') and no wider until
    // docs/proposals/proposed/2026-08-22-a-date-is-read-where-a-date-is-owed.md.
    const ACTIVE = `<${DATE}[^<>\\n]*>`;
    const INACTIVE = `\\[${DATE}[^\\[\\]\\n]*\\]`;
    const STAMP = new RegExp(
      `^(?:${ACTIVE}(?:--${ACTIVE})?|${INACTIVE}(?:--${INACTIVE})?)$`);
    // Everything past the head is the stamp's TAIL; declared below `DATE' (TDZ).
    const STAMP_HEAD = new RegExp(`^[<[]${DATE}(?:[ \\t]+[A-Za-z]+)?`);

    // ================================================== THE DATE, READ FOR INK
    // Wall's fourth spelling, DRIFT-PINNED over `test/fixtures/english-dates.json'.
    const DAY_MS = 86400000;
    // UTC THROUGHOUT: a local-midnight `Date' shifts a day across a DST boundary.
    // The year is set explicitly since `Date.UTC' reads 0..99 as 1900+y.
    const dnum = (c) => {
      const t = new Date(0);
      t.setUTCFullYear(c.y, c.m - 1, c.d);
      t.setUTCHours(0, 0, 0, 0);
      return Math.round(t.getTime() / DAY_MS);
    };
    const civil = (n) => {
      const t = new Date(n * DAY_MS);
      return { y: t.getUTCFullYear(), m: t.getUTCMonth() + 1, d: t.getUTCDate() };
    };
    const leapYear = (y) => (y % 4 === 0 && y % 100 !== 0) || y % 400 === 0;
    const daysInMonth = (y, m) =>
      [31, leapYear(y) ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][m - 1];
    // The wall `Time.fromGregorianValid' stands for: `31 feb' never reaches disk.
    const dayReal = (c) => !!c && c.m >= 1 && c.m <= 12 && c.d >= 1 && c.d <= daysInMonth(c.y, c.m);
    const addDays = (c, n) => civil(dnum(c) + n);
    /** Is C finite and real on the calendar?  A shift off `Date''s range is `NaN'. */
     const showable = (c) => !!c
      && Number.isFinite(c.y)
      && Number.isFinite(c.m)
      && Number.isFinite(c.d)
      && dayReal(c);
    const addMonths = (c, n) => {
      const k = c.m - 1 + n;
      const y = c.y + Math.floor(k / 12);
      const m = ((k % 12) + 12) % 12 + 1;
      return { y, m, d: Math.min(c.d, daysInMonth(y, m)) };
    };
    const DOW = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
    const dowOf = (c) => DOW[new Date(dnum(c) * DAY_MS).getUTCDay()];
    const pad2 = (n) => (n < 10 ? "0" : "") + n;
    const isoDay = (c) => `${c.y}-${pad2(c.m)}-${pad2(c.d)}`;
    const stampOf = (c, time, inactive, tail) =>
      `${inactive ? "[" : "<"}${isoDay(c)} ${dowOf(c)}`
      + `${time ? " " + time : ""}${tail || ""}${inactive ? "]" : ">"}`;
    const MONTH_WORDS = {
      jan: 1, january: 1, feb: 2, february: 2, mar: 3, march: 3,
      apr: 4, april: 4, may: 5, jun: 6, june: 6, jul: 7, july: 7,
      aug: 8, august: 8, sep: 9, september: 9, oct: 10, october: 10,
      nov: 11, november: 11, dec: 12, december: 12,
    };
    const MONTH_LIST = Object.keys(MONTH_WORDS);
    // Offers spell a month in full: a three-letter form abbreviates its neighbour.
    const MONTH_FULL = MONTH_LIST.filter((w) => w.length > 3 || w === "may");
    const NOT_A_DATE = "not a date";
    const INVERTED = "ends before it starts";
    const NO_DATE_WHY = "not a date — try 2026-08-18, today, +3d, 18 aug,"
      + " from 18 to 19 aug, or org's own <2026-08-05 Wed>";
    const INVERTED_WHY = "ends before it starts — spell a year at each end,"
      + " as in from 30 dec 2026 to 2 jan 2027";
    // The server REPARSES CLOSED's value (`Glance.Web.Base.unreadable').
    const NOT_A_STAMP = "not a timestamp";
    const notReadBack = (key) => `${key} is not a timestamp org would read back`;
    /** The reader's ONE refusal; HOW adds `hard' (dead) or `unfinished' (typing). */
    const noDate = (how) => ({ ok: false, ...how, short: NOT_A_DATE, why: NO_DATE_WHY });
    // Two-digit month and day, org's canonical spelling; `STAMP' stays liberal.
    const dayOf = (s) => {
      const m = /^(\d+)-(\d{2})-(\d{2})$/.exec(s);
      if (!m) return null;
      const c = { y: +m[1], m: +m[2], d: +m[3] };
      return dayReal(c) ? c : false;          // `false' is spelled, no such day
    };
    /** Org's own spelling, KEPT VERBATIM once it reparses — the one form whose
     * weekday is NOT recomputed, wrong weekday and all (pinned at
     * test/TestQuery.hs:1791). */
    function verbatimDate(s) {
      if (!/^[<[]/.test(s)) return null;
      if (STAMP.test(s)) {
        // The RAW stamp rides along, so the step splices into THAT (`dateStepped').
        const m = /^[<[](\d+)-(\d{1,2})-(\d{1,2})/.exec(s);
        const c = m ? { y: +m[1], m: +m[2], d: +m[3] } : null;
        return { ok: true, bracketed: true, stamp: s,
                 start: c && dayReal(c) ? c : undefined };
      }
      // STILL BEING TYPED.  The text must END on the closer: an interval holds two.
      if (!/[>\]]$/.test(s)) return noDate({ unfinished: true });
      return { ok: false, hard: true, short: NOT_A_DATE,
               why: "that bracket is no stamp org would read back" };
    }
    /** A shift's BASE: `null' where there is none, `false' where none is real. */
    function shiftBase(t, today) {
      // ONE ROSTER with the filter's (`Glance.Query.dayWords'); `*today*' is old.
      if (t === "" || t === "today" || t === "*today*") return today;
      if (t === "tomorrow") return addDays(today, 1);
      // THE SAME DOOR AS THE BARE FORM: a second ISO regex here is drift.
      const iso = dayOf(t);
      if (iso !== null) return iso;
      return null;
    }
    function shippedDate(s, today) {
      const t = s.toLowerCase();
      const tm = /^(\d{4}-\d{1,2}-\d{1,2})[ \t]+(\d{1,2}):([0-5]\d)$/.exec(t);
      if (tm) {
        const c = dayOf(tm[1]);
        if (!c || +tm[2] > 23) return noDate({ hard: true });
        return { ok: true, start: c, time: `${pad2(+tm[2])}:${tm[3]}` };
      }
      // ONE SHIFT GRAMMAR, THE FILTER'S OWN (`shiftIn', Glance.Query), read off the
      // END, so `2026-09-15-7d' is the week before.  NO TRIM: the wall trims none.
      const sh = /^(.*)([+-])(\d+)([dwmy])$/.exec(t);
      if (sh) {
        const base = shiftBase(sh[1], today);
        if (base === null) return null;
        if (base === false) return noDate({ hard: true });
        const n = (sh[2] === "-" ? -1 : 1) * +sh[3], u = sh[4];
        return { ok: true,
                 start: u === "d" ? addDays(base, n)
                      : u === "w" ? addDays(base, 7 * n)
                      : u === "m" ? addMonths(base, n)
                      : addMonths(base, 12 * n) };
      }
      const half = /^(.*?)[+-]\d*$/.exec(t);
      if (half) {
        const under = shiftBase(half[1], today);
        if (under !== null && under !== false) return noDate({ unfinished: true });
      }
      const b = shiftBase(t, today);
      if (b === null) return null;
      if (b === false) return noDate({ hard: true });
      return { ok: true, start: b };
    }
    /** `day month [year]' or `month day [year]'; an elided year is the clock's. */
    function englishDay(w, today) {
      if (w.length < 2 || w.length > 3) return null;
      let y = null;
      if (w.length === 3) {
        if (!/^\d{4}$/.test(w[2])) return null;
        y = +w[2];
      }
      let d = null, mo = null;
      if (/^\d{1,2}$/.test(w[0]) && MONTH_WORDS[w[1]])
        { d = +w[0]; mo = MONTH_WORDS[w[1]]; }
      else if (MONTH_WORDS[w[0]] && /^\d{1,2}$/.test(w[1]))
        { mo = MONTH_WORDS[w[0]]; d = +w[1]; }
      else return null;
      const c = { y: y === null ? today.y : y, m: mo, d };
      return dayReal(c) ? { c } : { bad: true };
    }
    /** The interval's left end, a day alone or a whole date.  EACH ELIDED FIELD
     * TAKES THE RIGHT END'S, or `from 18 to 19 august 2027' spans twelve months. */
    function englishLeft(w, right) {
      if (w.length === 1) {
        if (!/^\d{1,2}$/.test(w[0])) return null;
        const c = { y: right.y, m: right.m, d: +w[0] };
        return dayReal(c) ? { c } : { bad: true };
      }
      return englishDay(w, right);
    }
    function englishDate(s, today) {
      let w = s.toLowerCase().split(/[ \t]+/).filter(Boolean);
      if (!w.length) return null;
      if (w[0] === "from") w = w.slice(1);
      const i = w.indexOf("to");
      if (i > 0 && i < w.length - 1) {
        const right = englishDay(w.slice(i + 1), today);
        if (!right) return null;
        if (right.bad) return noDate({ hard: true });
        const left = englishLeft(w.slice(0, i), right.c);
        if (!left) return null;
        if (left.bad) return noDate({ hard: true });
        const a = dnum(left.c), b = dnum(right.c);
        // WITH NO TIMES `<D>--<D>' AND `<D>' ARE ONE INTERVAL: one spelling.
        if (a === b) return { ok: true, start: left.c };
        // Refused, which keeps the current-year default statable: spell both years.
        if (a > b)
          return { ok: false, hard: true, short: INVERTED, why: INVERTED_WHY };
        return { ok: true, start: left.c, end: right.c };
      }
      const one = englishDay(w, today);
      if (!one) return null;
      if (one.bad) return noDate({ hard: true });
      return { ok: true, start: one.c };
    }
    /** Resolve the phrase inside org's brackets, wearing THE ACTIVITY THE BRACKET
     * NAMES.  THE INVERSION TRAVELS; every other refusal stays the bracket's. */
    function wrappedDate(s, today) {
      const inactive = s[0] === "[";
      if (!s.endsWith(inactive ? "]" : ">")) return null;
      const body = s.slice(1, -1).trim();
      if (!body) return null;
      const r = resolvedDate(body, today, inactive);
      if (!r) return null;
      if (!r.ok) return r.short === INVERTED ? r : null;
      return { ...r, bracketed: true };
    }
    /** Resolve PHRASE by the grammar both readings share, stamped per INACTIVE. */
    const resolvedDate = (phrase, today, inactive) => {
      const g = englishDate(phrase, today) || shippedDate(phrase, today);
      if (!g) return null;
      if (!g.ok) return g;
      if (!showable(g.start) || (g.end && !showable(g.end)))
        return noDate({ hard: true });
      const one = (c, time) => stampOf(c, time, inactive);
      return { ok: true, start: g.start, end: g.end,
               stamp: g.end ? `${one(g.start)}--${one(g.end)}`
                            : one(g.start, g.time) };
    };
    /** TEXT read as a planning date against TODAY.  A declaration, so a direct
     * `eval' of this glue reaches it: the drift pin drives it over the corpus
     * `test/fixtures/english-dates.json' the server's own reader is driven over. */
    function readsDate(text, today) {
      const s = String(text == null ? "" : text).trim();
      if (!s) return noDate();
      // ORG'S OWN SPELLING OUTRANKS THE WRAPPED READING, as `planningTimestamp' does.
      const v = verbatimDate(s);
      if (v) return v.ok || v.unfinished ? v : (wrappedDate(s, today) || v);
      return resolvedDate(s, today, false) || noDate();
    }
    /** TEXT read the way the plain stamp wall reads it, for KEY's own refusal.
     * A SECOND READER AND NEVER A SECOND GRAMMAR, and NO CLOCK: nothing resolves. */
    function readsStamp(text, key) {
      const s = String(text == null ? "" : text).trim();
      const v = s ? verbatimDate(s) : null;
      if (v && v.ok) return v;
      return { ok: false,
               ...(s && !(v && v.unfinished) ? { hard: true } : { unfinished: true }),
               short: NOT_A_STAMP, why: notReadBack(key) };
    }
    /** The reader's own day, civil, read for INK alone — the server's clock
     * decides.  A summon pins it once at open (one clock read, docs/invariants.md). */
    function dateNow() {
      const n = new Date();
      return { y: n.getFullYear(), m: n.getMonth() + 1, d: n.getDate() };
    }
    const extendsAny = (list, w) => list.some((x) => x.indexOf(w) === 0);
    // `*today*' rides along unoffered, and last: `shiftBase' reads its prefixes.
    const DATE_VOCAB = ["today", "tomorrow", "+1d", "+1w", "+2w", "+1m", "+3m",
                        "+1y", "*today*"];
    const partWriting = (p, hi) =>
      p === undefined || p === ""
      || (p.length === 1 ? +p <= Math.floor(hi / 10)
                         : p.length === 2 && +p >= 1 && +p <= hi);
    const dayAndMonthTyped = (a, b) =>
      (/^\d{1,2}$/.test(a) && !!MONTH_WORDS[b])
      || (!!MONTH_WORDS[a] && /^\d{1,2}$/.test(b));
    const yearTyped = (y) => /^\d{1,3}$/.test(y);
    /** Is TEXT still being WRITTEN?  R is the reader's own answer for TEXT. */
    function dateWriting(text, r) {
      if (r.hard) return false;
      if (r.unfinished) return true;
      const t = String(text).trim().toLowerCase();
      if (!t) return false;
      // `2026-08-1' is on the way to the 18th; `2026-8' is a refusal already.
      const iso = /^(\d*)(?:-(\d*)(?:-(\d*))?)?$/.exec(t);
      if (iso) {
        const yy = iso[1], mm = iso[2], dd = iso[3];
        const whole = !!yy && mm?.length === 2 && dd?.length === 2;
        if (!whole && partWriting(mm, 12) && partWriting(dd, 31)) return true;
      }
      const tw = /^(\d+-\d{2}-\d{2})[ \t]+(\d{0,2})(:(\d{0,2}))?$/.exec(t);
      if (tw && dayOf(tw[1])) {
        const hh = tw[2], mm = tw[4] || "";
        if (hh.length === 2 && +hh > 23) return false;
        if (tw[3] === undefined) return true;      // no colon typed yet
        return mm.length < 2 ? mm.length === 0 || +mm <= 5 : +mm <= 59;
      }
      if (extendsAny(DATE_VOCAB, t)) return true;
      if ("from".indexOf(t) === 0) return true;
      const w = t.replace(/^from[ \t]+/, "").split(/[ \t]+/);
      const to = w.indexOf("to");
      if (to > 0) {
        const right = w.slice(to + 1);
        if (!right.length) return true;
        if (right.length === 1
            && (/^\d{1,2}$/.test(right[0]) || extendsAny(MONTH_LIST, right[0])))
          return true;
        if (right.length === 2)
          return (/^\d{1,2}$/.test(right[0]) && extendsAny(MONTH_LIST, right[1]))
            || (!!MONTH_WORDS[right[0]] && /^\d{1,2}$/.test(right[1]));
        return right.length === 3 && dayAndMonthTyped(right[0], right[1])
          && yearTyped(right[2]);
      }
      const last = w[w.length - 1];
      if (w.length > 1 && last !== "" && "to".indexOf(last) === 0) {
        const left = w.slice(0, -1);
        if ((left.length === 1 && /^\d{1,2}$/.test(left[0]))
            || (left.length === 2 && dayAndMonthTyped(left[0], left[1]))
            || (left.length === 3 && dayAndMonthTyped(left[0], left[1])
                && /^\d{4}$/.test(left[2])))
          return true;
      }
      if (w.length === 1)
        return /^\d{1,2}$/.test(w[0]) || extendsAny(MONTH_LIST, w[0]);
      if (w.length === 2 && /^\d{1,2}$/.test(w[0]))
        return extendsAny(MONTH_LIST, w[1]);
      // FOUR DIGITS ARE NO LONGER WRITING: `18 aug 1899' must show its answer.
      if (w.length === 3 && dayAndMonthTyped(w[0], w[1])) return yearTyped(w[2]);
      return false;
    }
    /** WHAT THE GHOST SAYS, or `""' for nothing.  READ is the caller's answer. */
    function dateGhost(text, today, read) {
      const t = String(text == null ? "" : text).trim();
      if (!t) return { text: "", bad: false };
      const r = read || readsDate(t, today);
      if (!r.ok)
        return dateWriting(t, r) ? { text: "", bad: false }
                                 : { text: ` ✗ ${r.short}`, bad: true };
      if (!r.stamp || r.stamp === t) return { text: "", bad: false };
      return { text: ` → ${r.stamp}`, bad: false };
    }

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
      // READ BEFORE THE SHUT: `shutEdit' takes the box away.
      const jot = bareCapture();
      // Over a bare draft this FINALIZES A CAPTURE; there is no row to retitle.
      const finalize = docBinding("org-capture-finalize", (b || {}).seq || "RET");
      // NOTHING TO CAPTURE IS NO COMMIT, AND THE BOX STAYS UP behind the word.
      if (jot && !String(val).trim()) { said(finalize, "nothing to capture"); return; }
      shutEdit(DTITLE);
      if (jot) {
        editing.cells.title = String(val).trim();
        drawCells();
        commitCapture(finalize);
        return;
      }
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
    // In a BARE draft's title the edit IS the capture: ESC takes the sheet with it.
    function cancelSheetEdit() {
      if (bareCapture()) {
        leaveSheet();
        echo("ESC → keyboard-quit (nothing captured)");
        return;
      }
      cancelEdit(restoreSheetEdit());
    }
    const bareCapture = () => capturing() && dediting() && !!edit.row.bare;

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
      // A DRAFT'S CARGO IS HELD, NEVER POSTED: the model pushes to the mirrors
      // a macrotask behind, so the word goes out FIRST and the settle rewrites.
      if (capturing()) { say(cargo); settleDraftPlan(cargo); return; }
      const h = editing;
      sync("syncing");
      post(h.id, h.digest,
           { body: cargo.body, properties: cargo.properties, planning: cargo.planning },
           null, h.child)
        .then(outcome)
        .then((a) => { if (editing === h && landed(h, say)(a)) reload(); })
        .catch((e) => stuck(subtreeSheet, e.message));
    }
    /** Redraw a draft's planning entries as `readsDate' reads them.  A row's
     * value is posted raw and comes back transformed; a draft posts nothing, so
     * the pane is made to show what the file will hold.  WHAT TRAVELS IS STILL
     * WHAT WAS TYPED (`typedPlan'); a phrase the resolver refuses stays RAW. */
    function settleDraftPlan(cargo) {
      for (const [key, value] of cargo.planning || []) {
        if (DATED.indexOf(key) === -1) continue;
        const read = readsDate(value, dateNow());
        // Org's own spelling passes through, so the settle reaches a fixed point.
        if (!read.ok || read.stamp === value) continue;
        dtyped[key] = { raw: value, shown: read.stamp };
        dsend({ kind: "addprop", key, value: read.stamp });
      }
    }
    const typedPlan = (plan) => (plan || []).map(([key, value]) => {
      const was = dtyped[key];
      return [key, was && was.shown === value ? was.raw : value];
    });

    /** `C-c C-c' OVER A DRAFT: the whole capture at one press, through the ONE
     * command that mints a blob.  THE BODY STARTS UNDER THE HEADLINE LINE: the
     * capture spells its own headline from the cells, so the whole cargo would
     * spell it twice. */
    function commitCapture(b) {
      const h = editing, c = h.cells || {};
      const tag = h.capture.tag;
      const title = String(c.title || "").trim();
      const body = bodyBelow(dbody);
      if (!title && !body.trim()) { said(b, "nothing to capture"); return; }
      const args = { title, body, properties: dprops, planning: typedPlan(dplan) };
      if (tag) args.tag = tag;
      if (c.state) args.state = c.state;
      const priority = priorityIn(c.priority);
      if (priority) args.priority = priority;
      const tags = cellTags(c.tags);
      if (tags.length) args.tags = tags;
      postCommand({ name: "capture", args }).then((a) => {
        // The cursor lands on the new row; `arrived' spends it on the next settle.
        arriving = a.id || null;
        shut();
        said(b, tag ? `captured · :${tag}:` : `captured · ${a.file}`);
        append("cmd", "info",
               `headline ${JSON.stringify(title)} captured into ${a.file}`);
      }).catch(failed(b, "capture"));
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
      // WHERE `%?' STOOD, in the body's line coordinates; `null' is the headline row.
      const at = h.capture ? h.capture.point : null;
      dsend({ kind: "fill",
              ...(at === null ? {} : { landing: at }),
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
    /** THE CAPTURE SHEET'S OWN VERBS.  A draft owes nothing to a file: never
     * dirty, never flushed, never refreshed, so `ESC' shuts it byte-identically. */
    const captureSheet = {
      noteId: "mnote", scope: "sync", state: "synced",
      closed: "left · nothing was captured",
      dirty: () => false,
      flush: () => { capnote("synced", "C-c C-c captures · ESC leaves");
                     return Promise.resolve(false); },
      refresh: () => Promise.resolve(false),
      shut: () => shut(),
    };
    const activeSheet = () =>
      (editing ? (capturing() ? captureSheet : subtreeSheet)
       : settings ? configSheet : null);
    // ONE SHORTHAND PER SHEET: reaching for another's moves a state you do not own.
    const sync = (next, message) => note(subtreeSheet, next, message);
    const capnote = (next, message) => note(captureSheet, next, message);
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
      // RAW IS THE FILE'S OWN BYTES RE-READ, and a draft has no file to re-read.
      if (capturing()) { said(b, "a capture has no file behind it yet"); return; }
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
    const tagCell = (list) => (list.length ? `:${list.join(":")}:` : "");
    /** WHAT A ROW-ADDRESSING DOOR WRITES ON A DRAFT, or `null' where the command is
     * not one a draft holds.  A CAPTURE NAMES NO ROW, so the four commands setting a
     * headline's cells land here in the wire's shape, empty digest as create pin. */
    function draftWrote(name, ids, args) {
      if (!capturing() || ids.length !== 1 || ids[0] !== editing.id) return null;
      const c = editing.cells;
      const tags = cellTags(c.tags);
      if (name === "set-title") c.title = String(args.title || "").trim();
      else if (name === "set-state") c.state = args.keyword || "";
      else if (name === "set-priority")
        c.priority = args.priority ? `[#${args.priority}]` : "";
      else if (name === "add-tag")
        c.tags = tagCell(tags.concat(tags.indexOf(args.tag) === -1 ? [args.tag] : []));
      else if (name === "remove-tag")
        c.tags = tagCell(tags.filter((t) => t !== args.tag));
      else if (name === "rename-tag")
        // The server's rule (`renameTagEdits'): in place, deduplicated.
        c.tags = tagCell([...new Set(tags.map((t) => (t === args.from ? args.to : t)))]);
      else return null;
      drawCells();
      return Promise.resolve({ results: [{ ok: true, id: editing.id, digest: "" }] });
    }
    // THE HEAD LINE REDRAWN off the handle's own cells: a draft has no reread.
    const drawCells = () => dsend({ kind: "cells", cells: cellsOf(editing.cells) });
    function fire(b, name, ids, args, verb, how, pin) {
      const mine = draftWrote(name, ids, args);
      return (mine || postCommand({ name, ids, args, digests: pin })).then((answer) => {
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
        // A DRAFT IS NO ROW, so the log says what it is; `titleOf' would name none.
        for (const x of results)
          if (x.ok) {
            if (mine) append("cmd", "info", `${CAPTURE_WORD} ${what}`);
            else noted(x.id, what);
          }
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
    /** THE SHEET OVER A SERVED DRAFT — the same open over an answer with no file
     * behind it.  A is `GET /capture''s answer: `/headline''s shape with `id' null
     * and `digest' "" — the create pin — plus the tag's cycle and `%?''s line.
     * EVERY DRAFT OPENS EDITING at `%?''s place; on a BARE draft that box is the
     * capture, so the reader's keys are `+', RET, the line, RET — AGENTS.hs. */
    function showDraft(b, tag, a) {
      editing = draftOf(tag, a);
      raw = false;
      el("mfile").textContent = captureWhere(tag, a);
      fill(editing);
      capnote("synced");
      el("modal").className = "on";
      soon(remembered);
      el("mtext").blur();
      // POINT ON THE HEADLINE OPENS NOW off the handle's cells.  A BODY LINE waits
      // for the fill: the row that seeds the editor lands a macrotask behind the send.
      if (editing.capture.point === null)
        openTitle(String((editing.cells || {}).title || ""));
      else dlanding = true;
      said(b, bareDraft(editing) ? "a headline · RET captures it · ESC leaves"
                                 : "C-c C-c captures · ESC leaves");
    }
    /** The editing handle a served draft stands behind: the answer's own fields, plus
     * the three the capture's alone under `capture'.  THE SPANLESS SHAPE IS CORRECT
     * for a file-less document: `spanAt' null makes every span null, so the links
     * door opens nothing and no delete names a byte range that does not exist. */
    const draftOf = (tag, a) => ({
      id: a.id === undefined ? null : a.id,
      file: a.file || "", child: null, parent: null,
      path: a.path || [], level: a.level || 1,
      cells: { ...(a.cells || {}) },
      children: a.children || [],
      org: a.org || "", body: a.body || "", ownLines: a.ownLines,
      properties: a.properties || [], planning: a.planning || [],
      logbook: "", digest: "", span: null, links: [], titleAt: null,
      capture: { tag, cycle: a.cycle || [],
                 point: typeof a.point === "number" ? a.point : null },
    });
    // The sheet's file line over a draft: WHERE IT WILL LAND, since there is no
    // file and no id yet to name.
    const captureWhere = (tag, a) =>
      `${CAPTURE_WORD}  ·  ${tag ? `:${tag}:` : a.file || "the inbox"}`;
    function fill(h) {
      base = raw ? h.org : "";
      el("mtext").value = base;
      // THE PLANNING PHRASES ARE THIS DOCUMENT'S: one kept across would misname a row.
      dtyped = {};
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
    // A CAPTURE IS COMMITTED OR IT NEVER WAS: a draft reads clean, so ESC is free.
    const dirty = () => editing !== null && !capturing()
      && (raw ? el("mtext").value !== base : edited() !== baseProps);

