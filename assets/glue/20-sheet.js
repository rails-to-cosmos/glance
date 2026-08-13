// THE MATERIALIZE SHEET: two panes over one subtree, and one flush carrying
// both — AGENTS.hs,
// docs/proposals/2026-08-08-widget-files.partial.md.
    let editing = null;
    let base = "", baseProps = null, raw = false;
    // THE DOCUMENT PANE IS AN ELM PROGRAM (`assets/elm/src/Doc.elm').  The
    // MIRROR below is a macrotask behind it, which every reader here survives by
    // running at the top of a key handler — AGENTS.hs.
    const DCELLS = CFG.dcells;
    let drows = [], dat = 0, dcol = null, dgrain = "element";
    let dflags = [], dbody = "", dlinks = [];
    let dport = null, dtook = null, dwrote = null;
    const cellsOf = (o) => DCELLS.map((k) => {
      const val = (o || {})[k] || "";
      return { key: k, val, colour: val ? badgeColor(val, k) : "" };
    });
    const shown = (r) => (r.cells || []).filter((c) => c.val);
    /**
     * THE FIVE CALLS A FLAG SURFACE OWES, over whichever program holds its rows.
     * `flagKey' and the movement keys ask for exactly these, and every surface
     * answers them the same way — a port out, the mirror in.
     */
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
        drows = now.rows; dat = now.at; dcol = now.col;
        dgrain = now.grain; dflags = now.flags; dbody = now.body;
        // Elm pushes a port BEFORE it paints, so what the cursor is scrolled
        // to and what the overlay is laid over are read a turn later.
        soon(() => { seedInsert(now.caret); keepInView(docElAt()); placeEdit(); });
      });
      dport.docSaid.subscribe((what) => { if (dwrote) { dwrote(what); dwrote = null; } });
      dport.docBody.subscribe(commitDoc);
      dport.docTook.subscribe(took);
      return dport;
    }
    const dsend = (m) => docPane().docIn.send(m);
    // The word for what a grain key landed on is the model's, so Elm says it.
    const dsay = (k, m) => { dwrote = keySaid(k); dsend(m); };
    const dmount = flagPort(dsend, () => dflags);
    // Forbidden over the TABLE's rows (the renderer's); the suite counts call
    // sites.  `block:"nearest"' honours `.de''s `scroll-margin', the scrolloff.
    function keepInView(row) {
      if (row && typeof row.scrollIntoView === "function")
        row.scrollIntoView({ block: "nearest" });
    }
    // OFFSETS ARE IN CHARACTERS (AGENTS.hs); JS counts UTF-16 units.
    const clen = (s) => Array.from(String(s)).length;
    // The three regions the lens lifts out sit ABOVE the paragraphs, so a body
    // offset past the title line is displaced by ONE constant.
    const bodyShift = (h) => clen(h.org || "") - clen(h.body || "");
    const linksIn = (at, links) => (links || dlinks).filter((l) =>
      l.span && l.span[0] >= at[0] && l.span[1] <= at[1]);
    const spanOf = (r) => (r && r.span) || null;
    /** The stops as the model has them, and where point stands among them. */
    const docRowAt = () => drows[dat] || null;
    // MOVEMENT IS TWO AXES (docs/design-rhymes.md): siblings, then the grain.
    // THREE DIALECTS, ONE AXIS: emacs, vim and the arrows are ALIASES rather
    // than variants, so `l'/`h' and the horizontal arrows are `f'/`b' — the
    // grain ladder, which already falls through to the cell walk where the stop
    // has cells.  One axis with three spellings, where there were two axes.
    const grainStep = (k) => (k === "f" || k === "l" || k === "<right>" ? 1
                            : k === "b" || k === "h" || k === "<left>" ? -1 : 0);
    const docStep = (step) => dsend({ kind: "step", by: step });
    const docFiner = (k) => dsay(k, { kind: "finer" });
    const docBroader = (k) => dsay(k, { kind: "broader" });
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
    const INSERT = docBinding("org-insert-element", "+");
    // The same command one key over: `S-RET' is `+' with the commit in front.
    const NEXT = docBinding("org-insert-element", "S-RET");
    /**
     * `+' ADDS A SIBLING of the stop, in the widget `RET' edits a paragraph
     * with.  NOTHING joins the model: the row is this snapshot until `RET'
     * grows the carrier, so `ESC' undoes it by having nothing to undo.  The
     * LEAD is `Scan''s and never reaches the box, so a blank `+' on an item
     * writes no bare bullet.
     */
    /**
     * A REGION'S MARKER IS ON SCREEN WHILE IT IS TYPED.  The drawn row wears it
     * — `- ', `1. ', `- [ ] ', `|   |   |' — and the box is laid over that row
     * exactly, opaquely, so a reader typing into it saw an EMPTY field and the
     * bullet only on `RET'.  The box carries the marker itself now, and what
     * goes over the wire is the WHOLE line the reader hands back.
     *
     * Seeded from the DRAWN row rather than computed here — the marker is
     * `Scan.markerFor''s answer and this page spells no org grammar — and once,
     * since every later state push would otherwise overwrite the typing.
     *
     * AND SO IS WHERE POINT LANDS IN IT: at the end of a lead, INSIDE the first
     * cell of a table row, which closes with a pipe a reader typing past would
     * turn into a column of its own.
     */
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
      if (r.kind === "child")
        { said(INSERT, "a child's body is its own — RET opens it"); return; }
      const off = at == null ? null : at;
      // WHERE it lands is said by the MODEL, with the draw: which region a
      // caret stands in is `Scan''s answer, and a reading of it here called a
      // table row inside a list item "an item at this level".
      dwrote = (what) => said(INSERT, what);
      // THE ROW IS DRAWN FIRST and the cursor goes to it, so the box is laid
      // over a line of the reader's own rather than over the one they stood
      // on.  Elm pushes its state a turn later and `placeEdit' runs again
      // there, which is what moves the box onto the paragraph just drawn.
      //
      // AT rides along to the write as well as to the draw: `Scan' asks which
      // REGION holds that line a second time there, and two answers would
      // indent a multi-line item's continuations under a bullet it never wore
      // — and land the write on a line the draw never drew.
      //
      // ABSENT is a caret nobody read, which `Scan' rides past the whole
      // structure with.  Line 0 is a line, so the field is left off rather
      // than sent as one.
      dsend(off === null ? { kind: "draft", id: r.id }
                         : { kind: "draft", id: r.id, at: off });
      openEdit(DPARA, { id: r.id, text: "", add: true, at: off });
    }
    /** Put TEXT in under ROW, and commit whichever answer Elm sends back. */
    const insertPara = (r, text, done) => {
      // TWO ANSWERS, ONE ASK — a body to write, or a word instead of one — so
      // each one-shot disarms the other and neither outlives the press.
      dcommit = () => { dwrote = null; done(); };
      dwrote = (what) => { dcommit = null; said(INSERT, what); };
      const m = { kind: "insert", id: r.id, text };
      if (r.at != null) m.at = r.at;
      dsend(m);
    };
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
      // The block goes back to its own height, the edit that grew it being over.
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
      // An absolute child sits against the PADDING box and scrolls with the
      // content, so a bordered, scrolling pane owes `clientTop' + `scrollTop'.
      s.top = `${a.top - b.top - pane.clientTop + pane.scrollTop}px`;
      s.height = `${a.height}px`;
      // THE BOX IS THE BLOCK IT COVERS ON EVERY EDGE, so the ground it paints
      // is the row's own.  An ITEM's box opens at its OWNER's content edge —
      // `.d-item' carries no horizontal padding, the nesting being the org
      // source's own spaces — where the pane's inset ran the highlight to the
      // beginning of the line.  Reading the row is what covers `.d-item' and
      // `.d-para' with one rule; the stylesheet's span is the fallback for a
      // page that measured nothing.
      if (o.block) {
        s.left = `${a.left - b.left - pane.clientLeft + pane.scrollLeft}px`;
        s.width = `${a.width}px`;
        inset(el(o.fields[0]), tr);
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
    /**
     * FIELD padded the way ROW is, so the text does not move when the box goes
     * over it: the box starts where the row's BORDER box does, and the inset
     * between that edge and the text is the row's own declaration.  A page
     * with no layout engine reads nothing and keeps the stylesheet's.
     *
     * MEASURED ONCE PER ROW.  `placeEdit' runs on every typed character, on
     * every scroll and on every state push, and `getComputedStyle' there forces
     * a style recalc each time; a row's padding cannot move under an open box,
     * so the answer stands until the anchor ELEMENT is replaced — which Elm's
     * vdom is free to do, and is why the ROW is the key: one open edit can
     * outlive the element it was laid over.
     */
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
    // Typing is the third door — the field itself, since nothing else sees a
    // character land.  `placeEdit' after it, so a box that grew is re-laid.
    el("dtext").addEventListener("input", () => { sizeDocEdit(); placeEdit(); });
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
      mount: () => null, anchor: docElAt, block: true,
      fill: (r) => { el("dtext").value = r.text; sizeDocEdit(); },
      focus: () => el("dtext").focus(),
    };
    const dediting = () => !!edit && edit.o === DTITLE;
    const dparaing = () => !!edit && edit.o === DPARA;
    const docOpen = () => dediting() || dparaing();
    // The document holds the keys with NOTHING focused and raw mode's textarea
    // is blurrable, so an open sheet counts as typing or `table' rows go live.
    const docHolds = () => editing !== null;
    const paraBinding = docBinding("org-ctrl-c-ctrl-c", "RET");
    const quitBinding = docBinding("quit-window", "q");
    // AT MOST N LINES, AND THE BLOCK IS WHAT GROWS.  The box has never had a
    // size of its own — `placeEdit' takes the block's — so what is TYPED grows
    // the BLOCK and the box follows, which is what makes an edit read as inline
    // rather than as something laid over the document: the lines under it move
    // down instead of being covered.  The number goes on the PANE and the row
    // at point reads it, one property inherited by both; the arithmetic is the
    // stylesheet's, so a page whose glue never ran still stands one line tall.
    const DOCROWS = 10;   // the knob, and the only place the cap is spelled
    const sizeDocEdit = () => el("mdoc").style.setProperty("--g-doc-rows",
      String(dparaing()
        ? Math.max(1, Math.min(DOCROWS, el("dtext").value.split("\n").length))
        : 0));
    /** Put a newline in at the caret, which is what the key would have done. */
    function newlineIn(id) {
      const box = el(id), at = box.selectionStart, to = box.selectionEnd;
      box.value = `${box.value.slice(0, at)}\n${box.value.slice(to)}`;
      box.setSelectionRange(at + 1, at + 1);
      sizeDocEdit();
    }
    /** The line the caret stands on in BOX, counted in newlines ahead of it. */
    const caretLine = (id) => {
      const box = el(id);
      return box.value.slice(0, box.selectionStart).split("\n").length - 1;
    };
    /**
     * Commit the open edit, and with AT put another stop in under it.
     *
     * AT is `S-RET''s whole difference from `RET': the write goes out the
     * same door, and what follows it is `+' over the row the commit landed on
     * — so an item begets an item, a paragraph a paragraph, and the grain
     * picks the lead.  `soon' because Elm pushes its state before it paints
     * and the next stop has to be read off the rescan rather than off the
     * model the commit was built from.  A write that lands nothing opens
     * nothing: a box holding only its own token is no item, and chaining
     * there would spend a press on a row the reader never made.
     *
     * It is the CARET'S LINE inside the box and the asking-for-another flag at
     * once, line 0 being a line: the next stop wears the prefix of the line the
     * press was made on, which is the only thing a multi-line stop can mean.
     * Read at the press, the box being gone by the time this is over.
     */
    function commitDocEdit(b, at) {
      const spoke = (what) => (b ? said(b, what) : echo(`RET → ${what}`));
      const more = () => { if (at != null) soon(() => insertHere(at)); };
      if (!edit) return;
      const r = edit.row;
      if (edit.o === DPARA) {
        const text = el("dtext").value;
        const add = !!r.add;
        shutEdit(DPARA);
        if (add) {
          // WHAT THE BOX HOLDS IS WHAT IS WRITTEN.  The lead was drawn into
          // the box when `+' was pressed, so the line goes out WHOLE and Elm
          // prepends nothing — a reader who edits `- [ ] ' into `- DONE' gets
          // `- DONE' rather than both.
          const lead = r.lead || "";
          // NO PLACEHOLDERS, EVER: a line that is still only its own token is
          // no item, and no row was ever made, so this writes nothing.
          if (!text.trim() || text === lead) { undraft(r); spoke("nothing added"); return; }
          insertPara(r, text, () => {
            spoke(lead ? "item added" : "paragraph added");
            more();
          });
          return;
        }
        if (text === r.text) { spoke("paragraph unchanged"); more(); return; }
        editPara(r, text, () => { spoke("paragraph written"); more(); });
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
    const cancelDocEdit = () => {
      const drawn = edit && edit.o === DPARA && edit.row.add ? edit.row : null;
      cancelEdit("element", DTITLE, DPARA);
      if (drawn) undraft(drawn);
    };
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
    // THE SHELL'S SMALL LISTS ARE ONE ELM PROGRAM (`assets/elm/src/Listing.elm'),
    // one instance per surface, handing back the shape `flagKey', `stepIn' and
    // `selectedId' already ask for — AGENTS.hs.
    function listing(host, cols, hint, pane) {
      // `Browser.element\' REPLACES the node it is given, so it takes a child
      // and HOST survives as the container an overlay is anchored inside.
      const ports = Elm.Listing.init({ node: part(el(host), "div", ""),
                                       flags: { cols, hint: hint || "" } }).ports;
      const seen = { at: -1, id: "", ids: [], flags: [], narrow: null, all: 0 };
      // The narrow's field is the LIST's own, drawn by the program that holds
      // the rows — there is no page markup for it and no second box to keep in
      // step.  `#app''s is the renderer's; this one wears the same classes.
      /** @returns {(HTMLInputElement & HTMLElement) | null} */
      const narrowBox = () =>
        /** @type {any} */ (el(host).querySelector("input.tv-filter"));
      let owed = false;
      ports.listState.subscribe((now) => {
        Object.assign(seen, now);
        // ELM PUSHES ITS STATE BEFORE IT PAINTS, so the field it has just drawn
        // is reachable a turn later — the document pane's own rule.
        if (!owed || seen.narrow === null) return;
        owed = false;
        soon(() => { const b = narrowBox(); if (b) b.focus(); });
      });
      // Caught in the CAPTURE phase, so the scroller inside PANE need not be named.
      if (pane) el(pane).addEventListener("scroll", placeEdit, true);
      // SEEDED WITH WHAT IS BEING SENT.  A port round trip costs a macrotask, and
      // both of these are followed IN THE SAME TURN by a reader asking where
      // point is — `RET' over a popup the raise just filled is the case.  What
      // is seeded is the value this side already holds, never a rule of Elm's;
      // the answer confirms it a turn later.
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
        // THE NARROW IS THE LIST'S OWN STATE, seeded here for the same reason
        // the cursor is: `/' is answered in the turn it was pressed in.
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
    /**
     * `/' NARROWS A SMALL LIST, one gesture over every `listing' mount — the
     * link popup, the tags popup, the sheet's property panel and the settings
     * sheet's states table (AGENTS.hs).
     */
    const narrows = (m) => can(m, "openNarrow", "shutNarrow", "narrowing");
    const narrowed = (m) => narrows(m) && m.narrowing() !== null;
    // WHO HOLDS THE LETTERS: the field, or the surface the list is in.
    const narrowTyping = (m) => narrowed(m) && active() === m.narrowBox();
    const narrowBinding = (k) => ({ seq: k, command: "filter-rows" });
    /** Take the narrow off, silently: a narrow belongs to the list it was typed
     * over, so a surface that closes takes its own with it. */
    const unnarrow = (m) => { if (narrowed(m)) m.shutNarrow(); };
    /** And with a word for it, which is what ESC and DEL owe a reader. */
    const widen = (m, k) => {
      if (!narrowed(m)) return false;
      m.shutNarrow();
      keySaid(k)("keyboard-quit (narrow cleared)");
      return true;
    };
    /**
     * The press over M, and whether it was spent.  `/' opens the field over the
     * rows at hand, or re-enters one already typed.
     *
     * WHILE THE FIELD HOLDS THE KEYS THE LETTERS ARE THE READER'S — every
     * binding the surface has is suspended and exactly four keys are claimed:
     * `RET' leaves the field with the narrow standing, `C-n'/`C-p' and the
     * vertical arrows step rows.  `DEL' is the field's own erase, and `ESC'
     * reaches the keymap, where clearing the narrow is a rung of its own.
     */
    function narrowPress(k, m) {
      if (!narrows(m)) return false;
      if (!narrowTyping(m)) {
        if (k !== "/") return false;
        m.openNarrow();
        said(narrowBinding(k), "");
        return true;
      }
      const step = k === "C-n" || k === "<down>" ? 1
                 : k === "C-p" || k === "<up>" ? -1 : 0;
      if (step) { stepIn(m, step); return true; }
      if (k !== "RET") return false;
      const box = m.narrowBox();
      if (box) box.blur();
      const n = m.counted();
      said(narrowBinding(k), `${n.shown} of ${n.all}`);
      return true;
    }
    const PCOLS = [ { key: "key", header: "Key" },
                    { key: "value", header: "Value" } ];
    let prows = [], pseq = 0, pmount = null;
    function mounted() {
      if (pmount) return pmount;
      pmount = listing("mptable", PCOLS, "d/D delete · u unflag", "mprops");
      return pmount;
    }
    const prowsOf = () =>
      prows.map((r) => ({ id: r.id, cells: { key: r.key, value: r.val } }));
    const repaint = (at) => mounted().setRows(prowsOf(), at);
    function drawProps(list, plan) {
      pseq = 0;
      shutEdit(PROW);
      el("mprops").className = "";   // and the panel gives the keys back
      const held = new Map(plan || []);
      prows = PLANNING.map((key) =>
        ({ id: `PLN:${key}`, key, val: held.get(key) || "", fixed: true }))
        .concat(list.map((p) => ({ id: `P${pseq++}`, key: p[0], val: p[1], fixed: false })));
      // `setRows\' keeps flags and the narrow deliberately, so a new drawer must
      // ask for both to go: it is another entry's, and another question.
      mounted().clearFlags();
      unnarrow(pmount);
      repaint(prows[0].id);
    }
    const patAt = () => prows.findIndex((r) => r.id === selectedId(pmount));
    function addProperty() {
      const id = `P${pseq++}`;
      prows.push({ id, key: "", val: "", fixed: false });
      repaint(id);
      openRow();
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
      if (!r.fixed) r.key = el("pkey").value;
      r.val = el("pval").value;
      shutEdit(PROW);
      repaint();
    }
    const cancelRow = () => cancelEdit("row", PROW);
    // A PLANNING ROW IS CLEARED AND STAYS, since an empty value is already how
    // an entry is absent; a property is DROPPED.
    function pdelete(ids, how) {
      const gone = new Set(ids);
      const cleared = prows.filter((r) => gone.has(r.id) && r.fixed);
      for (const r of cleared) r.val = "";
      prows = prows.filter((r) => r.fixed || !gone.has(r.id));
      repaint();
      const also = cleared.map((r) => r.key).join(", ");
      echo(`D → org-delete-property (${how(ids.length)}${also ? ` · ${also} cleared` : ""})`);
    }
    // Registers AHEAD of the dispatch, so it sees a key first — AGENTS.hs.
    document.addEventListener("keydown", (e) => {
      // Without the guard the sheet claims the letter a palette was raised to read.
      if (!editing || raw || momentary()) return;
      const k = keyName(e), crossing = k === "TAB" || k === "S-TAB";
      if (!k) return;
      // The panel's narrow field holds the letters while it has the focus, so
      // every binding below — `q' included — is typing until it gives them back.
      if (narrowTyping(pmount)) {
        if (narrowPress(k, pmount)) e.preventDefault();
        return;
      }
      const once = (act) => { if (!repeating(e)) act(); };
      // OVER THE OPEN TEXTAREA `RET' COMMITS, org's `C-c C-c' by another name
      // — the region is a value being handed back rather than a buffer being
      // typed into.  `S-RET' commits and asks for ANOTHER, and `M-RET' is the
      // newline.  One rule across the kinds: `S-RET' hands back a SIBLING of
      // whatever is open, so at a leaf it is the newline with the stop's own
      // lead attached — an item, a table row, a source line.  The bare newline
      // keeps `M-RET' for the whole-composite edit, where a sibling would be
      // another table rather than another row.  Everything else is the
      // textarea's, and ESC still restores.
      if (dparaing()) {
        if (k === "RET") { e.preventDefault(); once(() => commitDocEdit(paraBinding)); }
        else if (k === "S-RET")
          { e.preventDefault(); once(() => commitDocEdit(NEXT, caretLine("dtext"))); }
        else if (k === "M-RET") { e.preventDefault(); newlineIn("dtext"); }
        return;
      }
      // `q' IS `quit-window' ONE WINDOW IN: over the table it closes the app's,
      // here it closes the sheet's, by the door ESC leaves through.  Dead inside
      // an open edit, where it is a letter being typed — which is also why the
      // SETTINGS sheet keeps ESC alone: its panels are fields.
      if (k === "q" && !pediting() && !dediting()) {
        e.preventDefault();
        once(() => { said(quitBinding, ""); leaveSheet(); });
        return;
      }
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
        else if (!(narrowPress(k, pmount) || flagPress(k, e, PFLAGS))) return;
      } else if (crossing) enterPanel();
      else {
        const step = rowStep(k), depth = grainStep(k);
        if (step) docStep(step);
        else if (depth > 0) docFiner(k);
        else if (depth < 0) docBroader(k);
        else if (k === "RET") once(docEnter);
        else if (k === "DEL") once(docUp);
        else if (k === "S-<up>" || k === "S-<down>")
          once(() => atElement(() => cycleHere(k === "S-<up>" ? 1 : -1)));
        else if (k === "o" || k === "!") once(openHere);
        else if (k === "t") once(() => atElement(stateHere));
        else if (k === ":") once(() => atElement(tagsHere));
        else if (k === "SPC")
          once(() => toggleCheckbox(docBinding("org-toggle-checkbox", "SPC")));
        // `S-RET' IS `+' WHEREVER IT IS PRESSED.  With an edit open it commits
        // first and puts the next stop in behind the write; with none it is
        // the insert alone, so one key means ANOTHER ONE OF THESE at either
        // grain and in either state.  `M-RET' is org's own `org-insert-item'
        // and joins them here, the newline it spells inside a box being a key
        // there is no box to press it in.  None of the three reads a caret
        // out here, so the sibling rides past the whole structure.
        else if (k === "+" || k === "S-RET" || k === "M-RET") once(insertHere);
        else if (!flagPress(k, e, DFLAGS)) return;
      }
      e.preventDefault();
    });
    const unlogged = () => {};
    const PFLAGS = {
      mount: () => pmount, take: pdelete, note: unlogged,
      walk: () => stepIn(pmount, 1),
      missing: lacks("delete flags"),
      none: "org-delete-property (no row)",
      idle: "dired-do-flagged-delete (no deletions requested)",
      spared: "dired-do-flagged-delete (left standing)",
      verb: "drop",
      unflag: "delete-unflag (flag cleared)",
      flag: "delete-flag (d again deletes)",
      at: () => { const i = patAt(); return i === -1 ? null : prows[i].id; },
    };
    // This mount is a Set of ids rather than a renderer, so `missing' is unreachable.
    const DFLAGS = {
      mount: () => dmount, take: ddelete, note: unlogged,
      walk: () => docStep(1),
      missing: "this document has no flags",
      none: "org-delete-element (no element)",
      idle: "dired-do-flagged-delete (no deletions requested)",
      spared: "dired-do-flagged-delete (left standing)",
      verb: "delete",
      unflag: "delete-unflag (flag cleared)",
      flag: "delete-flag (d again deletes)",
      at: () => docCursor().at,
    };
    // The held-key guard is here: `ONCE' governs dispatch rows, these four live outside.
    const flagPress = (k, e, shape) => {
      if (k !== "d" && k !== "D" && k !== "u" && k !== "x") return false;
      if (!repeating(e)) flagKey(k, shape, keySaid(k));
      return true;
    };
    const asked = () => raw
      ? { org: el("mtext").value }
      : { body: dbody, properties: props(), planning: planning() };
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
    const activeSheet = () => (editing ? subtreeSheet : settings ? configSheet : null);
    const sync = (next, message) => note(subtreeSheet, next, message);
    function shut() {
      el("modal").className = ""; editing = null; base = ""; baseProps = null;
      soon(remembered);
      shutEdit(DTITLE); shutEdit(DPARA); shutEdit(PROW);
      docClear();
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
      // SPELLED RATHER THAN LEFT TO A FALLBACK, both of them.  `set-state' was
      // the fallback, so EVERY command without an entry logged "state cleared"
      // over rows it had done something else to — `delete' said it over each
      // file it moved out of the tree, in the strip that is this page's audit.
      "set-state": (args) => (args.keyword ? `→ ${args.keyword}` : "state cleared"),
      delete: () => "deleted",
    };
    // The caller's own word where no entry names one.  A command that NAMES
    // ROWS owes an entry and the suite asks for it, so this is reached by
    // nothing the table carries.
    const verbed = (name, args, verb) => (VERBED[name] || ((_args, v) => v))(args, verb);
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
    // THE TAG DECIDES WHAT `D' MEANS.  A row org has archived is one step from
    // gone, so the same key takes the next step over it — and a MIXED set
    // archives, which moves the whole set one step rather than doing two things
    // in one press.  The cell is org's own run, `:a:b:'.
    const ARCHIVE = CFG.archiveTag;
    const archivedRow = (id) =>
      String((rowOf(id).cells || {}).tag || "").split(":").indexOf(ARCHIVE) !== -1;
    function archive(b, ids, how) {
      if (ids.length && ids.every(archivedRow)) { confirmDelete(b, ids, how); return; }
      leaving = anchorFor(ids);
      fire(b, "archive", ids, {}, "archived", how)
        .then(spent(leaving)).catch(failed(b, "archive"));
    }
    // THE ONE KEY THAT MOVES A FILE OUT OF THE TREE, so it is the one that asks
    // for a word rather than a letter: a palette entry commits on a keystroke,
    // and this is not a keystroke's worth of decision.  Anything but the word
    // writes nothing and says so.
    const DELETE_WORD = "DELETE";
    function confirmDelete(b, ids, how) {
      askText(`delete · ${rowsWord(ids.length)} permanently`,
              `type ${DELETE_WORD} and RET · ESC leaves them`, "",
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
      spared: "left standing",
      verb: "archive",
      // Already archived is the delete path, and it asks for a WORD of its own.
      walled: (ids) => ids.every(archivedRow),
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
