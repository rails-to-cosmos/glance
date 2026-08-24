// THE MATERIALIZE SHEET: two panes over one subtree, one flush carrying both — AGENTS.hs.
    let editing = null;
    let base = "", baseProps = null, raw = false;
    // THE DOCUMENT PANE IS AN ELM PROGRAM; the MIRROR below is a macrotask behind it — AGENTS.hs.
    const DCELLS = CFG.dcells;
    let drows = [], dat = 0;
    let dflags = [], dbody = "", dlinks = [], dprops = [], dplan = [];
    // WHICH PLANNING ENTRY POINT STANDS IN, by its KEYWORD -- `null' is the
    // whole line.  The model's own axis, MIRRORED rather than counted here: the
    // shell holds no index into a list Elm draws, and the keyword is what every
    // door below it asks for anyway.
    let dplankey = null;
    let dport = null, dtook = null, dwrote = null;
    const cellsOf = (o) => DCELLS.map((k) => {
      const val = (o || {})[k] || "";
      return { key: k, val, colour: val ? badgeColor(val, k) : "" };
    });
    const shown = (r) => (r.cells || []).filter((c) => c.val);
    /** THE FIVE CALLS A FLAG SURFACE OWES, over whichever program holds its rows. */
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
        // Elm pushes a port BEFORE it paints, so these are read a turn later.
        soon(() => {
          seedInsert(now.caret); keepInView(docElAt()); placeEdit(); reselectDate();
        });
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
    // Forbidden over the TABLE's rows; `block:"nearest"' honours `.de''s scroll-margin.
    function keepInView(row) {
      if (row && typeof row.scrollIntoView === "function")
        row.scrollIntoView({ block: "nearest" });
    }
    // OFFSETS ARE IN CHARACTERS (AGENTS.hs); JS counts UTF-16 units.
    const clen = (s) => Array.from(String(s)).length;
    // The lifted title and properties sit ABOVE the paragraphs: one constant displaces a body offset.
    const bodyShift = (h) => clen(h.org || "") - clen(h.body || "");
    const linksIn = (at, links) => (links || dlinks).filter((l) =>
      l.span && l.span[0] >= at[0] && l.span[1] <= at[1]);
    const spanOf = (r) => (r && r.span) || null;
    // A headline's own span is one line; what `o' opens is its REACH, the
    // whole subtree under it.
    const reachOf = (r) => (r && r.reach) || null;
    const docRowAt = () => drows[dat] || null;
    // The suite reads the MIRROR's cursor as the pure value it is, through a
    // direct eval -- where a `var' reaches the caller's scope and a `const' does
    // not.  The DOM paints on rAF and the port lands a macrotask apart, so a
    // driver that watched the draw must also see the mirror agree before a key.
    var docAtNow = () => (drows[dat] || {}).id || "";
    // MOVEMENT IS TWO AXES, and `l'/`h' and the arrows ALIAS `f'/`b' — AGENTS.hs.
    const grainStep = (k) => (k === "f" || k === "l" || k === "<right>" ? 1
                            : k === "b" || k === "h" || k === "<left>" ? -1 : 0);
    const docStep = (step) => dsend({ kind: "step", by: step });
    const docFiner = (k) => dsay(k, { kind: "finer" });
    const docBroader = (k) => dsay(k, { kind: "broader" });
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
    function docEnter() {
      const r = drows[dat];
      if (!r) return;
      if (r.kind === "child") { into(r.index); return; }
      // A FRAME is not a line, the raw drawer's as much as the synthesized one:
      // what RET edits is a row inside, and TAB folds.  RET itself is reserved.
      if (r.fold) { echo("RET → f reaches the rows inside — TAB folds"); return; }
      // THE PLANNING LINE IS A LINE OF ENTRIES, not one value: what RET edits is
      // an entry inside it, and `f' is what reaches one.  The same rule the frame
      // above states, one grain finer, and asked as a CAPABILITY so no row is
      // named here by its id.
      if (r.entries) { planEnter(); return; }
      if (r.kind === "para" || r.kind === "meta") { openEdit(DPARA, r); return; }
      headEnter(r);
    }
    /** `RET' over the planning line.  OVER AN ENTRY it raises the very widget
     * `C-c C-s' raises, keyed by the entry the walk stands in -- so the two
     * doors are one box and one wall.  OVER THE WHOLE LINE it is INERT and says
     * where the entries are, the frame's own answer above it. */
    function planEnter() {
      if (!dplankey) { echo("RET → f reaches the entries — RET on one edits it"); return; }
      planHere(docBinding(planCommand(dplankey)), dplankey);
    }
    function headEnter(r) {
      if (editing.child !== null) {
        echo("RET → a child's title is not settable yet — DEL opens its parent");
        return;
      }
      // THE HEADLINE IS ONE STOP, so RET opens the title; `t', `:' and
      // S-<up>/S-<down> are what reach the other parts.
      const t = shown(r).find((x) => x.key === "title");
      openEdit(DTITLE, { id: "CELL:title", val: t ? t.val : "" });
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
    // THE MODEL'S OWN WORD RIDES THE CARGO wherever the model has one — where
    // it named the landing, and where it REROUTED the edit — since two ports
    // carry no order between them and a second one would race this.  `docSaid'
    // carries REFUSALS alone: they move no rows, so they race nothing.
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
      const now = was === " " || was === "-" ? "X" : " ";
      editPara(r, r.text.replace(CHECKBOX, `$1[${now}]`), () => said(b, `[${now}]`));
    }
    // TAB WALKS THE RUNGS A LIST ITEM MAY SIT ON: its own, one deeper -- which
    // needs a PREVIOUS SIBLING to hang under -- and one shallower, which needs a
    // parent.  It is a toggle, so illegal rungs are skipped and the walk comes
    // back where it started.  The subtree rides along on the commit.
    const OPENER = /^([ \t]*)(?:[-+*]|\d+[.)])\s/;
    /** What a continuation line sits under: the indent, the bullet, and the box. */
    const CONT = /^([ \t]*(?:[-+*]|\d+[.)])\s+(?:\[[ xX-]\]\s+)?)/;
    const TABB = docBinding("org-metaright", "TAB");
    const STEP = 2;                 // org's own, and the pane's `--g-doc-indent'
    let drung = null;
    const openerIn = (text) => OPENER.exec((text || "").split("\n")[0]);
    const indentOf = (text) => { const o = openerIn(text); return o ? o[1].length : null; };
    /** The rungs ROW may take from WAS: its own, then ONE CHILD, then EVERY PARENT
     * out to the outermost.  A child needs a sibling above it at the same indent to
     * hang under; a parent is any indent the lines above it actually stand at, so a
     * tree indented by something other than two is walked as it is written. */
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
    /** A REGION'S MARKER IS ON SCREEN WHILE IT IS TYPED, seeded ONCE from the DRAWN
     * row — a second seeding would overwrite the typing, and this page spells no org. */
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
      // `+' IN THE DRAWER TYPES THE PAIR IN PLACE, org's own way: a property is
      // a KEY and a VALUE, both required, and the two fields stand over a row
      // drawn where the pair will go.  THE DRAWN ROW IS THE MODEL'S OWN and no
      // half-typed pair joins the drawer's list, which is what a flush writes.
      if (r.kind === "meta") {
        dsend({ kind: "draftpair" });
        // ONE CLOCK READ PER SUMMON here too: the value half wears the date
        // widget's own ghost and meets the same wall.
        openEdit(DPAIR, { id: r.id, add: true, today: dateNow() });
        // THE BOX WEARS NO CHROME, so the echo carries what the popup's foot did.
        said(docBinding("org-set-property", "+"),
             "a key, then its value — RET applies · ESC cancels");
        return;
      }
      const off = at == null ? null : at;
      // WHERE it lands is the MODEL's answer; a reading of it here got it wrong.
      dwrote = (what) => said(INSERT, what);
      // THE ROW IS DRAWN FIRST and point goes to it; Elm pushes state a turn later
      // and `placeEdit' re-lays the box.  AT rides to the write as well as the draw.
      const m = { kind: "draft", id: r.id };
      if (off !== null) m.at = off;
      dsend(m);
      openEdit(DPARA, { id: r.id, text: "", add: true, at: off });
    }
    const insertPara = (r, text, done) => {
      // TWO ANSWERS, ONE ASK, so each one-shot disarms the other.
      dcommit = () => { dwrote = null; done(); };
      dwrote = (what) => { dcommit = null; said(INSERT, what); };
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
        // THE GUARD AGAIN AT ARRIVAL: the reader may have opened an edit while
        // this fetch flew, and `fill' would shut it over their caret.
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
      // A TIGHT BOX STANDS INSIDE THE ROW RATHER THAN COVERING IT, so the pane
      // says one is up and the row lifts its own wash while it is (Style.hs:
      // the two golds).  The stylesheet names no one box, so a new tight shape
      // is dressed by declaring itself one.
      el(o.pane).classList.toggle("tight", !!o.tight);
      o.fill(row);
      // The renderer stamps `tv-sel' a frame later, so measure a frame later.
      soon(placeEdit);
      o.focus(row);
    }
    // A TWO-FIELD EDIT CROSSES ITS FIELDS ON TAB; a one-field edit stays put.
    function hop() {
      const ids = edit.o.fields;
      const at = ids.findIndex((id) => el(id) === active());
      el(ids[(at + 1) % ids.length]).focus();
    }
    // SHUT MINE: one `edit' over four surfaces — an unscoped shut would cancel a rename.
    function shutEdit(o) {
      if (!edit || edit.o !== o) return;
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
      // Absolute against the PADDING box, so a scrolling pane owes clientTop + scrollTop.
      s.top = `${a.top - b.top - pane.clientTop + pane.scrollTop}px`;
      s.height = `${a.height}px`;
      // THE BOX IS THE BLOCK IT COVERS ON EVERY EDGE: `.d-item' carries no horizontal padding.
      // EVERY field of it, since a two-field box laid over one row is two halves
      // of that row's line -- padding the first alone would inset only the key.
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
    /** FIELD padded the way ROW is, MEASURED ONCE PER ROW: `getComputedStyle' forces
     * a style recalc, and one open edit can outlive the element it was laid over. */
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
    // Read off what Elm drew: a composite draws its leaves inside itself.
    const docElAt = () => el("dlist").querySelector(".dat");
    // `tight' over the TITLE CELL's box, falling back to the line where there is none.
    const dTitleAt = () =>
      (docElAt() && docElAt().querySelector(".dc-title")) || docElAt();
    const DTITLE = {
      box: "dtitle", pane: "mdoc", fields: ["dtin"],
      mount: () => null, anchor: dTitleAt, tight: true,
      edge: () => docElAt() && docElAt().querySelector(".dc-tags"),
      fill: (r) => { el("dtin").value = r.val; },
      focus: () => el("dtin").focus(),
    };
    // A NESTED ITEM IS DRAWN INSIDE ITS PARENT, so the ROW's box is as tall as
    // the subtree under it.  The edit covers the item's OWN line, which is what
    // it writes.  A composite draws no own line and keeps the whole box: the
    // list is one stop, and editing it rewrites the list.
    // Elm draws `own ++ deeper', so the own line is the FIRST child and the
    // first nested row is not; over a composite the two are the same node.
    const dParaAt = () => {
      const at = docElAt();
      if (!at) return at;
      const kid = at.querySelector(".de"), own = at.children[0];
      return kid && own && own !== kid ? own : at;
    };
    const DPARA = {
      box: "dpara", pane: "mdoc", fields: ["dtext"],
      mount: () => null, anchor: dParaAt, block: true,
      // A FRESH EDIT IS A FRESH WALK: the rungs are counted from where the line
      // stands when it opens.
      fill: (r) => { drung = null; el("dtext").value = r.text; sizeDocEdit(); },
      focus: () => el("dtext").focus(),
    };
    // A PAIR IS TWO FIELDS OVER ONE ROW: `block', so the box is the row's own
    // box on every edge and each field is padded the way the row is.
    const DPAIR = {
      box: "dpair", pane: "mdoc", fields: ["dkey", "dval"],
      mount: () => null, anchor: dParaAt, block: true,
      fill: () => {
        el("dkey").value = ""; el("dval").value = "";
        askVocab();
        sizeDocEdit();
      },
      // THE OFFERS ARE THE FOCUSED HALF'S, so they are drawn once it HAS the
      // focus: `openEdit' fills before it focuses, and a fill drew the other
      // half's list.
      focus: () => { el("dkey").focus(); pairMoved(); },
    };
    // THE PLANNING VALUE'S OWN SLOT, named by the keyword the summon carries:
    // `viewPlanning' draws each value in its own span (`Doc.elm'), and the box
    // is laid over THAT rather than over the whole row -- the row already says
    // the keyword, so the widget restates nothing.  `tight' runs the box to the
    // row's edge, so the ghost has line to ride on.
    const dPlanAt = () => {
      const key = edit && edit.o === DDATE ? edit.row.key : null;
      return key ? el("dlist").querySelector(`.dpv[data-key="${key}"]`) : null;
    };
    const DDATE = {
      box: "ddate", pane: "mdoc", fields: ["dwhen"],
      mount: () => null, anchor: dPlanAt, tight: true,
      // THE FIELD OPENS ON THE VALUE THE WIDGET STANDS ON, so `RET' on an
      // untouched widget recommits it and the shifted arrows have a day to adjust.
      // THE OPEN PLACES THE BOX, synchronously: `soon(placeEdit)' lands a frame
      // late, and the slot's anchor already stands.  Keystrokes never re-place.
      fill: (r) => { el("dwhen").value = r.val; dateMoved(); placeEdit(); },
      focus: () => selectWhole(el("dwhen")),
    };
    const dediting = () => !!edit && edit.o === DTITLE;
    const dparaing = () => !!edit && edit.o === DPARA;
    const dpairing = () => !!edit && edit.o === DPAIR;
    const ddating = () => !!edit && edit.o === DDATE;
    // THE DOC PANE'S OWN SHAPES, and the only enumeration of them: `edit' is
    // shared with the table's, so this is asked as MEMBERSHIP rather than as
    // `!!edit' -- an open rename on another surface is no open sheet edit.
    const DOCEDITS = [DTITLE, DPARA, DPAIR, DDATE];
    const sheetOpen = () => !!edit && DOCEDITS.indexOf(edit.o) !== -1;
    /** THE DAY THE OPEN EDIT READS AGAINST, stamped once when the box was
     * SUMMONED: the ghost must not answer two days for one phrase while the
     * reader is looking at it, and the wall above the commit must not refuse
     * what the ghost accepted.  A door opened over an unstamped row reads the
     * clock itself, which is one read at that door too. */
    const editDay = () => (edit && edit.row.today) || dateNow();
    const onPairKey = () => active() === el("dkey");
    // THE TREE'S OWN PROPERTY VOCABULARY, asked ONCE PER SHEET and kept: the
    // door answers `{ keys: {KEY: n}, values: {KEY: {VALUE: n}} }'.  A build
    // whose server has no such door answers 404 and the fields offer nothing.
    let dvocab = null, dvocabAsked = false;
    function askVocab() {
      if (dvocabAsked) return;
      dvocabAsked = true;
      getJSON("/properties")
        .then((v) => { dvocab = v; if (dpairing()) drawOffers(); })
        .catch(() => {});
    }
    const OFFERS = 6;   // the knob, and the only place the cap is spelled
    // What an offer that would REROUTE says about itself, and the only warning
    // the key half gives that the word is no property.
    const PLAN_HINT = "planning";
    /** ONE WIDGET, BOTH DOORS: a pair whose key routes to a settable planning
     * word owes a DATE in its value half, so that half offers dates and wears
     * the same ghost the date widget wears.  Asked in one place, or the offers
     * and the ghost could disagree about what the half is for. */
    const valueOwesDate = () =>
      dpairing() && DATED.indexOf(planningWord(el("dkey").value.trim())) !== -1;
    /** What the FOCUSED half offers, each with the HINT that names where it
     * lands: every key, or the values the tree spells under the key standing
     * beside it.  Filtered the way this page filters everywhere -- a fold-case
     * SUBSTRING of the offer -- and ordered by how often the tree writes it,
     * ties alphabetical. */
    function offersFor() {
      if (!dpairing()) return [];
      const onKey = onPairKey();
      // The key beside it routes to the planning line, so what the tree spells
      // under other keys is no vocabulary for this half -- and a date offer
      // RESOLVES, which no property vocabulary can: the hint column IS the
      // offer's own preview.
      if (!onKey && valueOwesDate()) return dateOffers(el("dval").value, editDay());
      const vocab = dvocab || {};
      // ORG UPPERCASES A PROPERTY KEY, so the door is keyed by the upper form;
      // the verbatim reading answers for a tree that spells one otherwise.
      const key = el("dkey").value.trim(), vals = vocab.values || {};
      const from = onKey ? (vocab.keys || {})
                         : (vals[key.toUpperCase()] || vals[key] || {});
      const typed = String(el(onKey ? "dkey" : "dval").value).trim();
      const want = typed.toLowerCase();
      const fits = (w) => w.toLowerCase().includes(want);
      const listed = Object.keys(from).filter(fits)
        .sort((a, c) => (from[c] - from[a]) || (a < c ? -1 : a > c ? 1 : 0));
      // ORG'S THREE RIDE THE KEY HALF OUT OF `CFG.planning' RATHER THAN THE
      // TREE: `/properties' walks DRAWERS, and the parser lifts planning off the
      // headline before one is read, so no tree spells them there and a build
      // whose server has no such door still offers these.  UPCASED, since that
      // is what the write makes of them, and LAST but never squeezed out by the
      // cap — a key that folds to one is a reroute, and the hint says so.
      const planned = onKey
        ? PLANNING.filter((w) => fits(w) && !listed.includes(w)) : [];
      // BOTH HALVES ARE OPEN VOCABULARIES, so THE READER'S OWN LINE LEADS the
      // offers, above the cap: `RET' over it commits the word that was typed
      // rather than the one it reads as a prefix of, and the walk clamps back
      // onto it (AGENTS.hs).  ASKED OF THE WHOLE VOCABULARY rather than of what
      // the cap left — a word the tree really spells COINCIDES with its own
      // entry however it ranks, so that entry leads instead, drawn ONCE and
      // wearing its own hint rather than calling itself new.  An empty field
      // leads with nothing.
      const words = listed.concat(planned);
      const minted = leadTyped(typed, words);
      const folds = minted || !typed
        ? null : words.find((w) => w.toLowerCase() === want);
      // The cap is the TREE'S keys to spend; org's three ride under it.
      const under = planned.filter((w) => w !== folds);
      const shown = listed.filter((w) => w !== folds)
        .slice(0, Math.max(0, OFFERS - under.length)).concat(under);
      // ONLY THE KEY HALF ROUTES, so only its offers may say `planning': a VALUE
      // that folds to one of org's three is a value like any other, and the hint
      // would name a landing it has not got.
      const dress = (w) =>
        ({ word: w, hint: onKey && planningWord(w) ? PLAN_HINT : "" });
      return (minted ? [{ word: typed, hint: NEW_HINT }]
              : folds ? [dress(folds)] : []).concat(shown.map(dress));
    }
    /** LIST into the menu BOX names, point on AT.  ONE renderer over two mounts
     * -- the pair's own and the date widget's -- since two walks of the same
     * shape could answer about two different draws. */
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
    /** A MENU IS ITS BOX, ITS LIST AND ITS POINT, and ONE SET OF VERBS runs both
     * of them -- the pair box's and the date widget's. */
    const dmenu = { box: "doffer", list: [], at: -1 };   // `-1' is point on NO offer
    const wmenu = { box: "dwoffer", list: [], at: -1 };
    // POINT NEVER STANDS PAST THE LIST, and on an empty list it stands nowhere.
    const menuPaint = (m) => {
      if (m.at >= m.list.length) m.at = m.list.length - 1;
      paintOffers(m.box, m.list, m.at);
    };
    /** The walk is a REPAINT of the list already drawn: nothing the arrows touch
     * is an input to it, so asking for the offers again per press would spend a
     * whole vocabulary filter to redraw the same words. */
    const menuWalk = (m, step) => {
      if (!m.list.length) return;
      m.at = Math.max(0, Math.min(m.list.length - 1, m.at + step));
      paintOffers(m.box, m.list, m.at);
    };
    /** The offer under point into FIELD, and whether that MOVED anything: an
     * offer already standing in the field is nothing to take, so the same key
     * goes on to hop or to apply rather than sticking here.  MOVED is the
     * field's own redraw, since the take is a keystroke the field never saw. */
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
    /** What a date-owed field offers over TYPED: the words of the grammar that
     * still fit it, each hinted with WHAT IT RESOLVES TO, and the reader's own
     * line leading them as it leads every open vocabulary on this page.
     * OFFERS STAND AT FRESH AND UNFINISHED POSITIONS AND NOWHERE ELSE: a term
     * that reads as a whole date carries none, and `RET' there APPLIES rather
     * than taking a word the reader never chose (the dry law).
     * READ is the reader's own answer for TEXT where the caller already has one,
     * so one keystroke reads one date once. */
    function dateOffers(text, today, read) {
      const typed = String(text || "").trim();
      if (typed && (read || readsDate(typed, today)).ok) return [];
      const want = typed.toLowerCase();
      // THE MONTH WORDS EARN THEIR PLACE ONCE A DAY IS TYPED: a bare month is
      // refused by the grammar, so offering one would be offering a refusal.
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
      // AN EMPTY FIELD OFFERS NO LITERAL, and point stands on NOTHING over one:
      // `RET' there is the empty value's own meaning, which on this key is CLEAR.
      // THE PAGE'S ONE FOLD-EQUALITY TEST decides it, as it does in the pair box.
      const lead = leadTyped(typed, fits) ? [{ word: typed, hint: NEW_HINT }] : [];
      return lead.concat(fits.map(dress));
    }
    /** Size F to LEN characters plus whatever rides after them, never past CAP:
     * MONOSPACE DOES THE ARITHMETIC.  NO FLOOR HERE: the stylesheet's `min-width'
     * is the one an empty field keeps for its caret.  WRITTEN ONLY WHEN IT
     * CHANGES, since every keystroke in either box reaches here and a restated
     * inline style still costs a layout. */
    const fitCh = (f, len, plus, cap) => {
      const w = `${Math.min(cap === undefined ? Infinity : cap, len + (plus || 0))}ch`;
      if (f.style.width !== w) f.style.width = w;
    };
    /** The field or its text moved, so the list under it is another list.  POINT
     * STANDS ON THE LINE THE READER TYPED, which leads the offers, and on
     * NOTHING over an empty field: with nothing typed the list is a menu to
     * walk, and `RET' there is the empty key's own refusal rather than a word
     * the reader never chose. */
    const pairMoved = () => {
      dmenu.at = el(onPairKey() ? "dkey" : "dval").value.trim() ? 0 : -1;
      drawOffers();
      // THE KEY FIELD HUGS ITS TEXT, so the closing colon stands flush against
      // it as it does in the drawer.  ONE SPELLING: every door that moves the
      // key reaches here, the assignments included.
      fitCh(el("dkey"), el("dkey").value.length);
      // A key that routes nowhere owes no date and carries no ghost.
      drawGhost("dval", "dvghost", valueOwesDate());
    };
    /** THE TYPED LINE IS ONE OFFER LIKE ANY OTHER, which is what carries a
     * partial key or value out of this box as the reader spelled it. */
    const takeOffer = () =>
      menuTake(dmenu, onPairKey() ? "dkey" : "dval", pairMoved);
    // TYPING IS ONE DOOR AND THE CROSSING IS THE OTHER.  A value ASSIGNED fires
    // neither, so the two callers that assign one ask for the list themselves.
    for (const id of ["dkey", "dval"])
      for (const ev of ["input", "focus"]) el(id).addEventListener(ev, pairMoved);
    // The widget has ONE field and nothing to cross to, so typing is its only
    // door -- and the first character typed ends the entry's own selection.
    el("dwhen").addEventListener("input", () => {
      if (!ddating()) return;
      edit.row.virgin = false;
      dateMoved();
    });
    /** TAB, RET or `:' over the pair.  An OFFER under point is taken first; a
     * KEY then hands over to its value -- taking one advances too, which is
     * `:''s own rule -- and a VALUE applies.  Taking a value offer is DRY: it
     * fills the field, and the apply stays the reader's own next press. */
    function pairKey(k) {
      const onKey = onPairKey();
      const took = takeOffer();
      // A KEY HANDS OVER whether or not an offer was taken, which is `:''s rule.
      if (onKey) { hop(); pairMoved(); return; }
      if (took) return;
      commitDocEdit(docBinding("org-set-property", k));
    }

    // ================================================ THE DATE WIDGET
    // ONE LINE, AND THE ROW ALREADY SAYS THE KEYWORD.  `C-c C-s' and `C-c C-d'
    // in the material document raise a FIELD in the value's own slot: what was
    // typed stands in the field and the resolution rides after it as GHOST, so
    // the document says what it is about to say while there is still something
    // on screen to fix.

    // The knob, and the only place the ghosted field's width cap is spelled.
    const GHOST_CAP = 46;
    /** The ghost after the field FIELDID, drawn where the field OWES a date and
     * blank where it does not, against the day the summon stamped.  READ is the
     * reader's own answer for what the field holds where the caller already has
     * one.  A GHOSTED FIELD IS EXACTLY AS WIDE AS WHAT IT HOLDS, so the
     * resolution lands one space after the last character typed rather than at a
     * column the layout picked, and the `flex' is taken off so the row's own
     * stretch cannot undo it. */
    function drawGhost(fieldId, ghostId, owes, read) {
      const f = el(fieldId), g = el(ghostId);
      const said = owes ? dateGhost(f.value, editDay(), read)
                        : { text: "", bad: false };
      g.className = said.bad ? "dgh bad" : "dgh";
      g.textContent = said.text;
      f.style.flex = owes ? "none" : "";
      // THE FIELD HAS NO `min-width' OF ITS OWN, so an empty one keeps its
      // caret's cell here; the `+ 1' is the space the ghost rides after.
      if (owes) fitCh(f, Math.max(1, f.value.length), 1, GHOST_CAP);
      else f.style.width = "";
    }
    /** Does the open widget stand under the plain stamp wall -- a planning word
     * this server does not SET, which is CLOSED on it?  THE SUMMONED KEY PICKS
     * THE READER and nothing else does: one widget, two walls, and the mode is
     * asked in ONE place so the ghost, the offers and the commit cannot
     * disagree about which wall the field is standing under.  THE CARRIED LIST
     * IS THE TRUTH, as it is at the two sibling walls (`valueOwesDate',
     * `pairRefused'): a server that settled a third word moves all three. */
    const verbatimOnly = () => ddating() && DATED.indexOf(edit.row.key) === -1;
    /** What the open widget's field reads as, against the day it was summoned. */
    const readsWhen = (text) =>
      verbatimOnly() ? readsStamp(text, edit.row.key) : readsDate(text, editDay());
    /** The field or its text moved, so the ghost and the list under it are
     * another ghost and another list.  POINT STANDS ON THE LINE THE READER
     * TYPED, and on NOTHING over an empty field -- where `RET' means CLEAR.
     * ONE READING PER KEYSTROKE: the offers and the ghost ask the same question
     * of the same text, so the answer is read once and handed to both. */
    function dateMoved() {
      const typed = el("dwhen").value.trim(), today = editDay();
      const only = verbatimOnly();
      const r = readsWhen(typed);
      wmenu.at = typed ? 0 : -1;
      // NOTHING THE CLOSED WALL TAKES IS A WORD: it reparses a bracket and
      // resolves no English, so there is no vocabulary to propose -- offering
      // one would be offering a refusal.
      wmenu.list = only ? [] : dateOffers(typed, today, r);
      menuPaint(wmenu);
      // NO `placeEdit' HERE: the box hangs off the row's own slot, and no
      // keystroke in the overlay moves that.  The open, the port's redraw and
      // the resize listener are the three doors placement has.
      drawGhost("dwhen", "dghost", true, r);
    }
    /** THE ENTRY'S SELECTION SURVIVES EVERY REDRAW THE OPEN TRIGGERS.  A summon
     * that had to DRAW the planning line opens the box before Elm has painted
     * the slot it stands in, and the port lands a macrotask behind with
     * `placeEdit' after it -- so a selection set once at open is one a reader in
     * a real browser may never see.  It is re-asserted while the widget is
     * VIRGIN, nothing typed and nothing walked, and never after, which would
     * fight the caret. */
    function reselectDate() {
      if (!ddating() || !edit.row.virgin) return;
      selectWhole(el("dwhen"));
    }
    const dateBinding = (k) => docBinding(edit.row.b.command, k);
    /** What the door onto KEY is called.  The two settable words are the summon
     * keys' own commands, so a widget raised by `RET' over an entry echoes what
     * `C-c C-s' echoes; CLOSED has no key of its own and takes org's own name
     * for the function that writes a planning line. */
    const PLAN_COMMANDS = { SCHEDULED: "org-glance-overview:schedule",
                            DEADLINE: "org-glance-overview:deadline" };
    const planCommand = (key) => PLAN_COMMANDS[key] || "org-add-planning-info";
    /** `C-c C-s' / `C-c C-d' in the material document: the widget over the row's
     * own SCHEDULED / DEADLINE slot.  THE LINE IS DRAWN IF ABSENT, the draft
     * pair's own move one row up -- a widget that stands in the value's place
     * needs the place to exist.  The TABLE's own pair of keys is untouched: they
     * ask over the marked rows through the shipped prompt, and reach the same
     * grammar at the same door. */
    function planHere(b, keyword) {
      if (!editing || raw) { said(b, "no document here"); return; }
      if (sheetOpen()) { said(b, "an edit is open — RET writes it, ESC leaves"); return; }
      const at = dplan.find((p) => p[0] === keyword);
      const stood = at ? at[1] : "";
      const drew = !at;
      // Read BEFORE the draft moves point: the port lands a macrotask later, so
      // this is the stop the key was pressed over and the one ESC comes back to.
      const back = docCursor().at;
      if (drew) redraftPlan(keyword);
      // ONE CLOCK READ PER SUMMON, stamped on the row the box opened over: the
      // ghost, the offers and the wall above the commit all read this one day.
      openEdit(DDATE, { key: keyword, val: stood, add: drew, back, b,
                        virgin: true, today: dateNow() });
      said(b, "RET sets it · empty clears it · ESC leaves");
    }
    /** `RET' over the widget.  DRY OVER AN OFFER AND FINAL OVER A COMPLETED
     * VALUE: an offer under point is taken and nothing else happens, and the
     * same key over the finished term applies -- the pair box's own `takeOffer'
     * rule.  A REFUSAL IS SPOKEN HERE, above the commit and while what was typed
     * is still on screen to fix. */
    function dateKey(b) {
      // A take is a keystroke the field never saw, so the widget redraws itself
      // and the entry stops being VIRGIN -- the same move typing one would make.
      if (menuTake(wmenu, "dwhen", () => { edit.row.virgin = false; dateMoved(); }))
        return;
      // AN EMPTY VALUE CLEARS THE ENTRY, the shipped foot's own promise kept
      // verbatim -- and clearing is the widget's law, not the grammar's, so it
      // never asks the reader whether nothing is a date.
      const typed = el("dwhen").value.trim();
      if (typed) {
        const r = readsWhen(typed);
        if (!r.ok) { said(b, r.why); return; }
      }
      commitDate(b, typed);
    }
    /** WHAT TRAVELS IS WHAT WAS TYPED.  The ghost resolved for INK alone; the
     * RAW text goes to `set-planning', the server resolves it ONCE against its
     * own clock, and the pane redraws off THAT answer as it does off every
     * other.  Two resolutions against two clocks is the midnight bug the
     * one-clock-read invariant exists to prevent. */
    function commitDate(b, typed) {
      const row = edit.row, keyword = row.key, h = editing;
      shutEdit(DDATE);
      if (row.add) undraftPlan(row);
      fire(b, "set-planning", [h.id], { keyword, date: typed || null },
           typed || "cleared")
        .then((results) => {
          // THE SERVER'S ANSWER IS THE TRUTH THE PANE REDRAWS FROM: `fire'
          // re-pinned the digest off the 200, so this re-read matches it.
          if (editing === h && (results || []).some((x) => x.ok)) reload();
        });
    }
    /** THE SHIFTED ARROWS ADJUST THE VALUE IN PLACE -- org-read-date's own walk
     * in its own minibuffer, and for its own reason: the plain arrows belong to
     * the caret.  A day on the horizontal, a week on the vertical, and THE GHOST
     * FOLLOWS, because what they move is the field's own text.  THE WALK ALWAYS
     * LANDS ON A WHOLE DATE, which is what keeps the walk and the offers from
     * ever both asking. */
    const dateStep = (k) =>
      k === "S-<right>" ? 1 : k === "S-<left>" ? -1
      : k === "S-<down>" ? 7 : k === "S-<up>" ? -7 : 0;
    function dateAdjust(b, by) {
      const f = el("dwhen");
      const r = readsWhen(f.value.trim());
      if (!r.ok || !r.start) { said(b, "no date here to move"); return; }
      const to = addDays(r.start, by);
      // THE BRACKET KIND STANDS.  A walk under the plain stamp wall moves the
      // DAY and never the spelling: that wall takes a bracket and nothing else,
      // so a step that wrote its answer back bare would leave the field holding
      // a value the very next `RET' refuses.  The walk still lands on a WHOLE
      // date, so a time of day goes with the step as it does under the other.
      f.value = verbatimOnly()
        ? stampOf(to, null, r.stamp.charAt(0) === "[") : isoDay(to);
      f.setSelectionRange(f.value.length, f.value.length);
      edit.row.virgin = false;
      dateMoved();
    }
    // The document holds the keys with NOTHING focused, so an open sheet counts as typing.
    const docHolds = () => editing !== null;
    const paraBinding = docBinding("org-ctrl-c-ctrl-c", "RET");
    const quitBinding = docBinding("quit-window", "q");
    // AT MOST N LINES, AND THE BLOCK IS WHAT GROWS; the arithmetic is the stylesheet's.
    const DOCROWS = 10;   // the knob, and the only place the cap is spelled
    /** The rows the open edit's text OCCUPIES, wrapping counted.  MEASURED FLAT:
     * `scrollHeight' never reads under the height the box already stands at, so the
     * field is collapsed for the reading and put back. */
    const docRowsDrawn = () => {
      const t = el("dtext"), s = t.style;
      // A page with no layout measures nothing, and org's own newlines still count.
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
    // ORG'S OWN NEWLINES ARE THE FLOOR and what wraps takes the rows it occupies:
    // counting newlines alone left the box a line short over a wrapped item, so what
    // `M-RET' added was typed out of sight.  A PAIR IS ONE LINE and never grows:
    // the drawn row it stands over is what would otherwise collapse under it.
    const sizeDocEdit = () => el("mdoc").style.setProperty("--g-doc-rows",
      String(dpairing()
        ? 1
        : dparaing()
        ? Math.max(1, Math.min(DOCROWS,
                               Math.max(el("dtext").value.split("\n").length,
                                        docRowsDrawn())))
        : 0));
    /** A NEWLINE INSIDE A LIST ITEM LANDS UNDER ITS OWN TEXT: org reads a
     * continuation by its indent, so the spaces the marker occupies are carried
     * onto the next line rather than left for the reader to type. */
    function newlineIn(id) {
      const box = el(id);
      // THE BOX IS PART OF WHAT THE TEXT SITS AFTER: `- [ ] Test' continues under
      // the T, so the marker's width counts the checkbox and its gap.
      const o = CONT.exec((box.value || "").split("\n")[0]);
      const under = o ? " ".repeat(o[1].length) : "";
      spliceIn(box, box.selectionStart, box.selectionEnd, "\n" + under);
      // SETTING `value' FIRES NO `input', so the listener that re-lays the box after
      // typing never runs: the newline has to place the box itself.
      sizeDocEdit();
      placeEdit();
    }
    const caretLine = (id) => {
      const box = el(id);
      return box.value.slice(0, box.selectionStart).split("\n").length - 1;
    };
    // ORG'S THREE PLANNING WORDS, the server's own list and the pane's `planKeys'.
    const PLANNING = CFG.planning;
    // THE PLANNING WORDS THIS SERVER SETS, carried in the same blob rather than
    // respelled.  What is off the list -- CLOSED, org's own bookkeeping, whose
    // value the server REPARSES rather than resolves -- keeps the plain stamp
    // wall, and every door that meets that wall derives it from here
    // (`valueOwesDate', `verbatimOnly', `pairRefused').
    const DATED = CFG.settable;
    // A KEY ORG WOULD READ AS SOMETHING OTHER THAN A PROPERTY.  The frame words
    // are the drawer's own (AGENTS.hs `reservedProperties'): written as a key,
    // one of them TERMINATES the drawer and everything under it falls out of it.
    const DRAWER_FRAME = ["PROPERTIES", "END"];
    // The store's own two, kept out of every drawer the pane draws: typed by
    // hand they would forge the identity a headline is found and linked by.
    const IDENTITY_KEYS = ["ORG_GLANCE_ID", "ORG_GLANCE_CREATION_TIME"];
    /** KEY as one of org's three planning words, or `null'.  A KEY THAT FOLDS TO
     * ONE OF THEM IS NO PROPERTY: the write routes it to the planning line,
     * upcased, and the drawer never sees it — AGENTS.hs. */
    const planningWord = (key) => {
      const up = String(key || "").toUpperCase();
      return PLANNING.indexOf(up) === -1 ? null : up;
    };
    // THE DATE ORG READS: a DECIMAL RUN per field, the month and the day
    // RANGE-CHECKED, so `2026-8-1' is a date and `2026-13-45' is not.  The
    // lookahead is what keeps the day from stopping short — without it `32' is
    // read as day `3' with `2' falling into the tail.
    const DATE = "\\d+-(?:0?[1-9]|1[0-2])-(?:0?[1-9]|[12]\\d|3[01])(?!\\d)";
    // ONE ORG STAMP, or two joined by org's `--' AND WEARING THE SAME BRACKET —
    // the server takes the pair's OPENING bracket again after the join, which is
    // why each kind is spelled as a whole alternative rather than one half twice.
    // THE BOX'S READING OF THE SERVER'S OWN WALL (`settledPlanning', which reads
    // the date grammar): a planning value that does not read back stops being a
    // planning entry on the next load.  Kept no looser than the server's, since a
    // value this let through would meet the 409 with the box already shut, and no
    // wider until
    // docs/proposals/proposed/2026-08-22-a-date-is-read-where-a-date-is-owed.md.
    const ACTIVE = `<${DATE}[^<>\\n]*>`;
    const INACTIVE = `\\[${DATE}[^\\[\\]\\n]*\\]`;
    const STAMP = new RegExp(
      `^(?:${ACTIVE}(?:--${ACTIVE})?|${INACTIVE}(?:--${INACTIVE})?)$`);

    // ================================================== THE DATE, READ FOR INK
    // THE PAGE SPELLS NO ORG, and this reader does not break that.  What a
    // commit sends is the RAW text the reader typed; the SERVER resolves it once
    // against its own clock at the planning wall, and the pane redraws off that
    // answer as it does off every other (docs/commands.md "Dates").  This reader
    // writes nothing and is never the value: its only output is INK.
    //
    // IT IS THE WALL'S FOURTH SPELLING -- beside `Glance.Query.planningTimestamp',
    // `AGENTS.hs' `stampShaped' and the `STAMP' regex above -- and it is
    // DRIFT-PINNED against the server's over ONE corpus,
    // `test/fixtures/english-dates.json', the house pattern the planning wall's
    // spellings already live by: a vector added there is owed an answer by both
    // halves.  Where the two part, the server's answer is what the reader ends
    // up looking at.
    const DAY_MS = 86400000;
    // UTC THROUGHOUT: a local-midnight `Date' shifts a day across a DST
    // boundary, and arithmetic that moved with the reader's zone would give two
    // answers for one phrase.  The CLOCK is read local (below) and the
    // arithmetic runs in this civil space.
    // THE YEAR IS SET EXPLICITLY, never passed to `Date.UTC': that constructor
    // reads 0..99 as 1900+y, which would put `<0099-08-18>' in 1999 -- splitting
    // the weekday from the wall's and landing an arrow walk nine centuries off.
    // `civil' needs no such guard: it builds from a TIMESTAMP, where no
    // two-digit window exists.
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
    // `Time.fromGregorianValid' is the wall this stands for: `31 feb' is refused
    // here as it is refused there, and never reaches the disk.
    const dayReal = (c) =>
      !!c && c.m >= 1 && c.m <= 12 && c.d >= 1 && c.d <= daysInMonth(c.y, c.m);
    const addDays = (c, n) => civil(dnum(c) + n);
    /** A resolved day FIT TO SHOW: every field finite, and the day real on the
     * calendar.  The arithmetic above can return neither -- a shift far enough
     * out runs off `Date''s own range and every field comes back `NaN' -- and a
     * ghost that drew one would present `<NaN-NaN-NaN undefined>' as a
     * RESOLUTION, with RET committing that string.  THE GHOST PRESENTS A STAMP
     * OR A REFUSAL, never a third thing. */
    const showable = (c) =>
      !!c && Number.isFinite(c.y) && Number.isFinite(c.m) && Number.isFinite(c.d)
      && dayReal(c);
    // A MONTH STEP KEEPS THE DAY OF THE MONTH and clamps at the month's end, so
    // `31 jan' + 1m is the 28th and never the 3rd of March.
    const addMonths = (c, n) => {
      const k = c.m - 1 + n;
      const y = c.y + Math.floor(k / 12);
      const m = ((k % 12) + 12) % 12 + 1;
      return { y, m, d: Math.min(c.d, daysInMonth(y, m)) };
    };
    const DOW = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
    // THE WEEKDAY IS COMPUTED, never carried: `TsMoment' holds no weekday field
    // and recomputes on render, which is why a stamp cannot disagree with its
    // own date.  Org's own bracket is the ONE exception, kept verbatim below.
    const dowOf = (c) => DOW[new Date(dnum(c) * DAY_MS).getUTCDay()];
    const pad2 = (n) => (n < 10 ? "0" : "") + n;
    const isoDay = (c) => `${c.y}-${pad2(c.m)}-${pad2(c.d)}`;
    /** C as org's own stamp, with TIME where there is one; INACTIVE takes org's
     * other bracket, `[...]'.  THE ONE ORG-STAMP WRITER: a bracket kind is chosen
     * here and never spelled a second time. */
    const stampOf = (c, time, inactive) =>
      `${inactive ? "[" : "<"}${isoDay(c)} ${dowOf(c)}`
      + `${time ? " " + time : ""}${inactive ? "]" : ">"}`;
    // THE MONTH TABLE IS EXACT and folds totally: twelve short forms and twelve
    // full ones, no `sept', no form carrying a full stop.
    const MONTH_WORDS = {
      jan: 1, january: 1, feb: 2, february: 2, mar: 3, march: 3,
      apr: 4, april: 4, may: 5, jun: 6, june: 6, jul: 7, july: 7,
      aug: 8, august: 8, sep: 9, september: 9, oct: 10, october: 10,
      nov: 11, november: 11, dec: 12, december: 12,
    };
    const MONTH_LIST = Object.keys(MONTH_WORDS);
    // WHAT THE OFFERS SPELL A MONTH AS: the full word, since a list is read and
    // a three-letter form is the abbreviation of the word beside it.  `may' is
    // its own full form.  Computed once -- the filter would otherwise walk
    // twenty-four words per keystroke to answer the same twelve.
    const MONTH_FULL = MONTH_LIST.filter((w) => w.length > 3 || w === "may");
    // THE TWO WORDS A REFUSAL IS SPELLED IN, the corpus's own `refusals' and
    // the ghost's whole vocabulary: a refusal riding an input line as trailing
    // ghost has ONE LINE to say it in.  "Not a date" reads oddly of a phrase
    // naming two perfectly good days in the wrong order, which is why the
    // inversion is spelled apart -- the server spends its second word there too.
    const NOT_A_DATE = "not a date";
    const INVERTED = "ends before it starts";
    // The sentence beside each, for the echo the commit's refusal writes: the
    // wall's own 400 names the accepted spellings, and so does this.
    const NO_DATE_WHY = "not a date — try 2026-08-18, today, +3d, 18 aug,"
      + " from 18 to 19 aug, or org's own <2026-08-05 Wed>";
    const INVERTED_WHY = "ends before it starts — spell a year at each end,"
      + " as in from 30 dec 2026 to 2 jan 2027";
    // THE OTHER WALL'S TWO WORDS.  CLOSED is org's own bookkeeping: the server
    // REPARSES its value rather than resolving one (`Glance.Web.Base.unreadable'),
    // so the box's word for it names the timestamp and never a date.  SPELLED
    // ONCE and spent at both doors that meet that wall -- the pair box's key
    // routed to CLOSED, and the widget summoned over its entry.
    const NOT_A_STAMP = "not a timestamp";
    const notReadBack = (key) => `${key} is not a timestamp org would read back`;
    /** The reader's ONE refusal, with HOW it refuses — `{hard: true}' where no
     * further character rescues the term, `{unfinished: true}' where the very
     * next one may, and neither where the answer is simply no.  The two words
     * ride every one of them, so a refusal cannot be spelled short of them. */
    const noDate = (how) => ({ ok: false, ...how, short: NOT_A_DATE, why: NO_DATE_WHY });
    // THE BARE ISO'S MONTH AND DAY ARE TWO-DIGIT — org's canonical spelling, and
    // `Glance.Query.dayOf' (`%Y-%m-%d') reads no other.  DELIBERATELY STRICTER
    // THAN `DATE' ABOVE: org's parser reads single digits inside a bracket, so
    // `STAMP' stays liberal and `<2026-8-1 Sat>' is kept verbatim, while the same
    // digits BARE are refused here exactly as the wall refuses them.
    //
    // THE YEAR IS ANY RUN OF DIGITS, which is what `%Y' reads: `99-01-01' and
    // `12026-08-18' are both days the wall takes, and the corpus carries the
    // wall's own answers for the small years, unpadded as `%Y' writes them.
    const dayOf = (s) => {
      const m = /^(\d+)-(\d{2})-(\d{2})$/.exec(s);
      if (!m) return null;
      const c = { y: +m[1], m: +m[2], d: +m[3] };
      return dayReal(c) ? c : false;          // `false' is "spelled, not real"
    };
    /** Org's own spelling, KEPT VERBATIM once it reparses — the one form whose
     * weekday is NOT recomputed, wrong weekday and all (pinned at
     * test/TestQuery.hs:1791). */
    function verbatimDate(s) {
      if (!/^[<[]/.test(s)) return null;
      if (STAMP.test(s)) {
        // THE START RIDES ALONG for the shifted arrows: a summon lands on a
        // STANDING stamp selected whole, and a walk with no day to move from
        // would refuse the commonest open there is.  `STAMP' vouched for the
        // shape; `dayReal' still guards a syntactic day the calendar lacks.
        const m = /^[<[](\d+)-(\d{1,2})-(\d{1,2})/.exec(s);
        const c = m ? { y: +m[1], m: +m[2], d: +m[3] } : null;
        return { ok: true, verbatim: true, stamp: s,
                 start: c && dayReal(c) ? c : undefined };
      }
      // A BRACKET STILL OPEN IS STILL BEING TYPED, and the very next character
      // may be the one that closes it: `<2026-08-05 Mon' is rescued by `>'.  So
      // an unclosed bracket is UNFINISHED and the ghost stays dark over it.
      // THE TEXT MUST END ON THE CLOSER, not merely contain one, or the second
      // half of an interval would count itself finished at the first `>'.
      if (!/[>\]]$/.test(s)) return noDate({ unfinished: true });
      // A CLOSED BRACKET THAT DOES NOT REPARSE IS NO STAMP, and no further
      // character rescues one: what the field takes is org's own spelling or
      // English, never half of one.  A MIXED pair names a timestamp that does
      // not exist — one bracket kind, both halves.
      return { ok: false, hard: true, short: NOT_A_DATE,
               why: "that bracket is no stamp org would read back" };
    }
    /** A shift's BASE, `null' where the text is not one at all, `false` where it
     * is SPELLED as one and names no real day.  An empty base is today-relative,
     * the reading the planning grammar already gives a bare `+3d'. */
    function shiftBase(t, today) {
      // `*today*' IS THE FILTER'S OWN WORD for the request's day, read here too
      // so one field spells one grammar.
      if (t === "" || t === "today" || t === "*today*") return today;
      if (t === "tomorrow") return addDays(today, 1);
      // THE SAME DOOR AS THE BARE FORM, asked rather than re-spelled: a second
      // ISO regex here is drift, one side reading a year the other refuses.
      // `dayOf' answers `null' for "not this shape at all", which is this
      // function's own answer for it.
      const iso = dayOf(t);
      if (iso !== null) return iso;
      return null;
    }
    /** The grammar the planning path already reads: ISO, `today'/`*today*'/
     * `tomorrow', and org's own shift charset on any of them.  `null' where the
     * text is not this grammar's at all, so the English reader gets its turn. */
    function shippedDate(s, today) {
      const t = s.toLowerCase();
      // AN ISO DATE TAKES A TIME OF DAY, org's own `<D W HH:MM>'.  The hour is
      // read one digit or two, so `9:05' is the time a reader meant rather than
      // a refusal over a missing zero.
      const tm = /^(\d{4}-\d{1,2}-\d{1,2})[ \t]+(\d{1,2}):([0-5]\d)$/.exec(t);
      if (tm) {
        const c = dayOf(tm[1]);
        if (!c || +tm[2] > 23) return noDate({ hard: true });
        return { ok: true, start: c, time: `${pad2(+tm[2])}:${tm[3]}` };
      }
      // ONE SHIFT GRAMMAR, THE FILTER'S OWN (`shiftIn', Glance.Query): both
      // signs, read off the END so a date's own hyphens are never the shift's —
      // the greedy base leaves the LAST sign to open it, and `2026-09-15-7d'
      // is the week before that day.  The wall reads the very same grammar
      // (`planningTimestamp'), so what the ghost previews is what the commit
      // lands.  NO TRIM ON THE BASE: the wall reads none, and a space the wall
      // refuses previewed here would be the drift the corpus pins against.
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
      // A HALF-TYPED SHIFT NARROWS NOTHING — and it is no refusal either: it is
      // a term still being WRITTEN, which is what keeps the ghost silent over
      // it.  BOTH SIGNS, like the finished form: an ISO's own hyphens fall
      // through harmlessly because the base they leave is no base.
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
    /** `day month [year]' or `month day [year]'.  THE YEAR IS FOUR DIGITS AND
     * NEVER TWO, and where it is elided it is THE CLOCK'S, flat: a typist
     * meaning next year writes the year.  A BARE DAY AND A BARE MONTH ARE NO
     * DATE, and A WEEKDAY IS NEVER READ. */
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
    /** The interval's left end: a day alone, or a whole date read exactly as
     * `englishDay' reads one — BOTH ARRANGEMENTS, so `from aug 18 to 19 sep'
     * is the interval the wall already takes, the year optional.  EACH ELIDED
     * FIELD TAKES THE RIGHT END'S VALUE — the English idiom says one month
     * once, and defaulting the left year independently would read `from 18 to
     * 19 august 2027' as a twelve-month span; the RIGHT end stands in for the
     * clock below, so the inheritance is one rule spelled once. */
    function englishLeft(w, right) {
      if (w.length === 1) {
        if (!/^\d{1,2}$/.test(w[0])) return null;
        const c = { y: right.y, m: right.m, d: +w[0] };
        return dayReal(c) ? { c } : { bad: true };
      }
      return englishDay(w, right);
    }
    /** The English day-and-month forms, single and interval.  THE SEPARATOR IS
     * WHITESPACE and a RUN of it is one; `from' is optional and `to' is not. */
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
        // THE DEGENERATE INTERVAL COLLAPSES: with no times `<D>--<D>' and `<D>'
        // denote the same interval, so one meaning keeps one spelling.
        if (a === b) return { ok: true, start: left.c };
        // AN INVERTED RANGE IS REFUSED — which is what keeps current-year-flat
        // statable without a calendar: the typist spells both years.
        if (a > b)
          return { ok: false, hard: true, short: INVERTED, why: INVERTED_WHY };
        return { ok: true, start: left.c, end: right.c };
      }
      const one = englishDay(w, today);
      if (!one) return null;
      if (one.bad) return noDate({ hard: true });
      return { ok: true, start: one.c };
    }
    /** TEXT read as a planning date against TODAY.  A declaration, so a direct
     * `eval' of this glue reaches it: the drift pin drives it over the corpus
     * the server's own reader is driven over. */
    function readsDate(text, today) {
      const s = String(text == null ? "" : text).trim();
      // NOTHING TYPED IS NOTHING RESOLVED.  Clearing an entry is the WIDGET's
      // law and not the grammar's: an empty field names no date, so the ghost
      // stays dark over one and the commit sends the clear without asking here.
      if (!s) return noDate();
      const v = verbatimDate(s);
      if (v) return v;
      // THE ENGLISH PHRASE IS READ BEHIND ORG'S OWN BRACKETS AND AHEAD OF THE
      // REST, the server's own order: it is the one reading with a refusal of
      // its own to spend, and a phrase it declines falls through to the rest.
      const g = englishDate(s, today) || shippedDate(s, today);
      if (!g) return noDate();
      if (!g.ok) return g;
      // A RESOLUTION THAT CANNOT BE SPELLED IS A REFUSAL, and a HARD one: the
      // arithmetic ran off the calendar and no further character walks it back.
      if (!showable(g.start) || (g.end && !showable(g.end)))
        return noDate({ hard: true });
      return { ok: true, start: g.start, end: g.end,
               stamp: g.end ? stampOf(g.start) + "--" + stampOf(g.end)
                            : stampOf(g.start, g.time) };
    }
    /** TEXT read the way the plain stamp wall reads it, for KEY's own refusal.
     * A SECOND READER AND NEVER A SECOND GRAMMAR: `verbatimDate' is org's own
     * bracket, the whole of what that wall reparses, and English is never widened
     * to it.  NO CLOCK: nothing here resolves, so there is no day to resolve
     * against.  IT REFUSES IN THE TWO SHAPES `dateWriting' READS -- unfinished
     * while a bracket is still open, hard once no character can close one -- so
     * the ghost stays dark through org's own spelling and speaks only past it.
     * NOTHING TYPED IS NOTHING REFUSED: the empty field is the widget's own
     * CLEAR, so it refuses UNFINISHED and stays dark too. */
    function readsStamp(text, key) {
      const s = String(text == null ? "" : text).trim();
      const v = s ? verbatimDate(s) : null;
      // THE VALUE PASSES THROUGH: what the wall reads back is what it writes,
      // so the resolution IS the text and the ghost falls silent over it.
      if (v && v.ok) return v;
      return { ok: false,
               ...(s && !(v && v.unfinished) ? { hard: true } : { unfinished: true }),
               short: NOT_A_STAMP, why: notReadBack(key) };
    }
    /** The reader's own day, civil.  THE GHOST IS A PREVIEW AND THE SERVER'S
     * CLOCK DECIDES what a phrase resolves to, so this is read for INK alone; a
     * summon pins it once at open, so a phrase does not change its answer under
     * the reader mid-edit. */
    function dateNow() {
      const n = new Date();
      return { y: n.getFullYear(), m: n.getMonth() + 1, d: n.getDate() };
    }
    const extendsAny = (list, w) => list.some((x) => x.indexOf(w) === 0);
    // The words a fresh field offers, and what `writing' reads a prefix against.
    // `*today*' RIDES ALONG UNOFFERED: `shiftBase' reads it, so its prefixes are
    // a term being written like any other and the ghost owes them the same
    // silence.  It sits at the END because the offer list is drawn in order and
    // the filter's own spelling is not what a fresh field proposes first.
    const DATE_VOCAB = ["today", "tomorrow", "+1d", "+1w", "+2w", "+1m", "+3m",
                        "+1y", "*today*"];
    /** An ISO month or day HALFWAY TYPED, against its own ceiling HI: absent, a
     * first digit that some two-digit value under HI still starts with, or the
     * finished pair itself.  `08' and `1' are both on the way to a real day; `8'
     * is not, no month reading `8' alone. */
    const partWriting = (p, hi) =>
      p === undefined || p === ""
      || (p.length === 1 ? +p <= Math.floor(hi / 10)
                         : p.length === 2 && +p >= 1 && +p <= hi);
    /** A FINISHED day and month, either arrangement — what a year may follow. */
    const dayAndMonthTyped = (a, b) =>
      (/^\d{1,2}$/.test(a) && !!MONTH_WORDS[b])
      || (!!MONTH_WORDS[a] && /^\d{1,2}$/.test(b));
    /** A YEAR HALFWAY TYPED: one digit to three.  The fourth completes it. */
    const yearTyped = (y) => /^\d{1,3}$/.test(y);
    /** Is TEXT a term still being WRITTEN — a proper prefix of something this
     * grammar would accept?  THE GHOST SAYS NOTHING OVER ONE: a refusal flashed
     * at every keystroke is a refusal nobody reads, and `18 a' is not a mistake,
     * it is a month halfway typed.  A HARD refusal is never merely being
     * written — no further character rescues a day that is not on the calendar,
     * and `31 feb' is a prefix of `31 february', which is the same wrong day.
     * R IS THE READER'S OWN ANSWER FOR TEXT, and the one caller asks only where
     * it REFUSED, so a term this reads as written is one the reader declined. */
    function dateWriting(text, r) {
      if (r.hard) return false;
      if (r.unfinished) return true;
      const t = String(text).trim().toLowerCase();
      if (!t) return false;
      // AN ISO BEING SPELLED, COMPONENT BY COMPONENT — never one already spelled
      // to the full shape, which is a date the calendar has judged.  The year run
      // is unbounded, as at `dayOf': a fifth digit is still a year.  Each of the
      // other two is judged AS A PREFIX OF THE CANONICAL TWO-DIGIT ONE, so
      // `2026-08-1' on the way to the 18th stays dark, while `2026-8' — which no
      // further character rescues, the wall reading two digits and never one —
      // shows its refusal at the keystroke that makes it wrong.
      const iso = /^(\d*)(?:-(\d*)(?:-(\d*))?)?$/.exec(t);
      if (iso) {
        const yy = iso[1], mm = iso[2], dd = iso[3];
        const whole = !!yy && mm?.length === 2 && dd?.length === 2;
        if (!whole && partWriting(mm, 12) && partWriting(dd, 31)) return true;
      }
      // A TIME OF DAY BEING SPELLED over a real ISO day, `<D HH:MM>' halfway.
      // The HOUR AND MINUTE ARE JUDGED AS PREFIXES: `2026-08-18 1' may still
      // become 13:00, where `2026-08-18 25' can become no hour at all and is
      // left to the refusal below.
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
        // THE RIGHT END TAKES A YEAR TOO, and reads it exactly as the single
        // phrase does below — `englishDay' is the one reader for both.
        return right.length === 3 && dayAndMonthTyped(right[0], right[1])
          && yearTyped(right[2]);
      }
      // `to' HALFWAY TYPED behind a finished left end: `from 30 dec 2026 t' is
      // one keystroke from an interval and names no date at all on its own.
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
      // A YEAR BEING TYPED behind a finished day and month: `18 aug 20' is three
      // keystrokes from `18 aug 2027' and no mistake at any of them.  FOUR
      // DIGITS ARE NOT WRITING — that is a year the calendar has judged, and
      // `18 aug 1899' must show its answer rather than stay dark.
      if (w.length === 3 && dayAndMonthTyped(w[0], w[1])) return yearTyped(w[2]);
      return false;
    }
    /** WHAT THE GHOST SAYS, or `""' for nothing.  THREE STATES AND NO FOURTH: an
     * empty field says nothing, a term still being WRITTEN says nothing, a term
     * that RESOLVES shows the stamp the commit would land, and a term the
     * grammar refuses outright shows the refusal's short word.  This is the dry
     * law's complete-term reading, read for ink instead of for keys.
     * AND IT FALLS SILENT WHEN IT HAS NOTHING TO ADD: where the resolution IS
     * what was typed — org's own bracketed spelling, kept verbatim — drawing the
     * same string twice on one line is the duplication the shape is against.
     * READ is the reader's own answer for TEXT where the caller already has one,
     * so the offers and the ghost read one keystroke once. */
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

    /** Why this pair is not written, or `null'.  EVERY refusal the model has is
     * one of these: the box is shut before the model answers, so a wall it
     * alone knew would leave a drawn row with nothing left to reach it. */
    function pairRefused(key, value) {
      if (!key) return "a key is required";
      if (/[\s:]/.test(key)) return "a key holds no spaces and no colons";
      const up = key.toUpperCase();
      if (DRAWER_FRAME.indexOf(up) !== -1)
        return `:${up}: frames the drawer — writing it would end the drawer here`;
      if (IDENTITY_KEYS.indexOf(up) !== -1)
        return `:${up}: is the store's own — this would forge the headline's identity`;
      if (!value) return "a value is required";
      // THE WALL'S OWN SENTENCE, echoed where the box still stands: the write
      // would come back 409 with the same words and nothing left to fix them in.
      // THE TWO SETTABLE WORDS READ THE WHOLE GRAMMAR, since the wall transforms
      // rather than reparses: the box asks the reader, not the regex — a phrase
      // refused here would never reach the wall that accepts it.  CLOSED is not
      // settable, so its wall stays the plain stamp.
      if (planningWord(up)) {
        if (DATED.indexOf(up) !== -1)
          return readsDate(value, editDay()).ok
            ? null : `${up} is not a date org would read back`;
        if (!STAMP.test(value)) return notReadBack(up);
      }
      return null;
    }
    // The key and the value a drawer line opens with; the same reading the
    // model makes of it, org's own `:KEY: value'.  A LINE THAT OPENS NEITHER
    // READS AS TWO EMPTY HALVES, so every caller may trim what it takes.
    const PAIRKEY = /^\s*:([^\s:]+):\s*(.*)$/;
    const pairIn = (text) => PAIRKEY.exec(text) || ["", "", ""];
    /** The planning word committing TEXT over ROW would route the pair to, or
     * `null'.  The one thing the model alone would know: the shortcut below
     * needs it, and so does the WALL, since the box shuts before the model
     * answers and a refusal it alone knew would land with nothing left on
     * screen to fix it in. */
    const migrating = (r, text) =>
      r.kind === "meta" ? planningWord(pairIn(text)[1]) : null;
    /** Commit the open edit, and with AT put another stop in under it — `S-RET''s
     * whole difference from `RET'.  AT is the CARET'S LINE, read at the press. */
    function commitDocEdit(b, at) {
      const spoke = (what) => (b ? said(b, what) : echo(`RET → ${what}`));
      const more = () => { if (at != null) soon(() => insertHere(at)); };
      if (!edit) return;
      const r = edit.row;
      // THE WIDGET COMMITS THROUGH ITS OWN DOOR wherever the key came from, or
      // `C-c C-c' over an open one would fall through to the title's branch and
      // rename the headline with a date.
      if (edit.o === DDATE) { dateKey(b || dateBinding("RET")); return; }
      if (edit.o === DPARA) {
        const text = el("dtext").value;
        const add = !!r.add;
        // ONE WALL, BOTH DOORS: a `:SCHEDULED:' line committed here ROUTES to
        // the planning line exactly as the pair box's does, so its value meets
        // the same refusal — asked HERE, above the shut, where what was typed
        // is still on screen to be fixed rather than at the write's 409 with
        // nothing left to fix it in.  AN EMPTIED VALUE IS NO REFUSAL AT THIS
        // DOOR: that is org's own way of CLEARING an entry, and this door is
        // the only one that can — the pair box needs both halves.
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
          // NO PLACEHOLDERS: a line still only its own token is no item.
          if (!text.trim() || text === lead) { undraft(r); spoke("nothing added"); return; }
          insertPara(r, text, () => {
            spoke(lead ? "item added" : "paragraph added");
            more();
          });
          return;
        }
        // A PAIR THAT WOULD MIGRATE IS NEVER UNCHANGED: a `:SCHEDULED:' line
        // standing in the drawer has somewhere to go without a byte typed, so
        // the model is asked even where the line reads as it did.
        if (text === r.text && !migrating(r, text))
          { spoke("paragraph unchanged"); more(); return; }
        editPara(r, text, (cargo) => {
          spoke(cargo.said || "paragraph written");
          more();
        });
        return;
      }
      // THE PAIR ARRIVES WHOLE, both halves off the box: the walls are the
      // box's own and it STAYS OPEN behind each one, so what was typed is
      // still there to be fixed.
      if (edit.o === DPAIR) {
        const key = el("dkey").value.trim(), value = el("dval").value.trim();
        const no = pairRefused(key, value);
        if (no) { spoke(no); return; }
        shutEdit(DPAIR);
        // TWO ANSWERS, ONE ASK, so each one-shot disarms the other: the model's
        // own word for where the pair landed rides the CARGO, and `docSaid'
        // carries a refusal alone — which moves no rows and so races nothing.
        dcommit = (cargo) => { dwrote = null; spoke(cargo.said || "property written"); };
        dwrote = (what) => { dcommit = null; spoke(what); };
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
    // THE PLANNING LINE'S OWN PAIR: the widget stands in the value's place, so
    // the line is drawn to stand in and taken back with the box.
    const redraftPlan = (keyword) => dsend({ kind: "draftplan", key: keyword });
    const undraftPlan = (r) => dsend({ kind: "undraftplan", id: r.back });
    // THE ESCAPE IS FROM THE EDIT: the box goes, the drawn row with it, and the
    // drawer is the bytes it was — nothing typed here ever entered its list.
    const cancelSheetEdit = () => {
      const drawn = edit && edit.o === DPARA && edit.row.add ? edit.row : null;
      const pair = edit && edit.o === DPAIR ? edit.row : null;
      // AND THE KEYWORD THE SUMMON GHOSTED IN goes with the box: the planning
      // line comes back the bytes it was, its own ABSENCE included.
      const when = edit && edit.o === DDATE ? edit.row : null;
      cancelEdit(when ? "the planning line" : pair ? "the drawer" : "element",
                 ...DOCEDITS);
      if (drawn) undraft(drawn);
      if (pair) undraftPair(pair);
      if (when && when.add) undraftPlan(when);
    };

    function ddelete(ids, how) {
      dtook = how;
      dsend({ kind: "delete", ids });
    }
    /** What a delete came back with: `ddelete' left the wording here. */
    function took(answer) {
      const how = dtook;
      dtook = null;
      if (!how) return;
      // A property or a planning line leaves through the LISTS, counted beside
      // the body's own; the model says outright what it REFUSED -- a headline.
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

    /** Empty the pane: the sheet shut, so the document it held is gone. */
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
    // THE SHELL'S SMALL LISTS ARE ONE ELM PROGRAM, one instance per surface — AGENTS.hs.
    function listing(host, cols, hint, pane) {
      // `Browser.element' REPLACES its node, so HOST survives as the anchor container.
      const ports = Elm.Listing.init({ node: part(el(host), "div", ""),
                                       flags: { cols, hint: hint || "" } }).ports;
      const seen = { at: -1, id: "", ids: [], flags: [], narrow: null, all: 0 };
      // The narrow's field is the LIST's own, wearing the renderer's classes.
      /** @returns {(HTMLInputElement & HTMLElement) | null} */
      const narrowBox = () =>
        /** @type {any} */ (el(host).querySelector("input.tv-filter"));
      let owed = false;
      ports.listState.subscribe((now) => {
        Object.assign(seen, now);
        // ELM PUSHES ITS STATE BEFORE IT PAINTS, so the field is reachable a turn later.
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
        // THE NARROW IS THE LIST'S OWN STATE, seeded here as the cursor is.
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
    /** `/' NARROWS A SMALL LIST, one gesture over every `listing' mount — AGENTS.hs. */
    const narrows = (m) => can(m, "openNarrow", "shutNarrow", "narrowing");
    const narrowed = (m) => narrows(m) && m.narrowing() !== null;
    // WHO HOLDS THE LETTERS: the field, or the surface the list is in.
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
    document.addEventListener("keydown", (e) => {
      // Without the guard the sheet claims the letter a palette was raised to read.
      if (!editing || raw || momentary()) return;
      const k = keyName(e);
      if (!k) return;
      const once = (act) => { if (!repeating(e)) act(); };
      // THE PAIR TAKES FOUR KEYS: the offers walk on the arrows and `C-n'/`C-p',
      // `:' hands a KEY over to its value — org's own muscle, and the character
      // is swallowed since no key holds one — and TAB and RET carry the form.
      // In the VALUE `:' is a character like any other, which a value may spell.
      // TAB IS SWALLOWED over the widget: there is nowhere to hop, and letting it
      // out would move the browser's focus off a box that is still open.  ESC is
      // the keymap's own graduated ladder, as it is everywhere here.
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
      // `RET' COMMITS here, `S-RET' commits and asks for ANOTHER, `M-RET' is the newline.
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
        // THE BROWSER WOULD TAKE THE FOCUS OUT OF THE FIELD, so the key is claimed.
        else if (k !== "TAB") return;   // ESC is the keymap's, and puts the element back
      } else {
        const step = rowStep(k), depth = grainStep(k);
        if (step) docStep(step);
        else if (depth > 0) docFiner(k);
        else if (depth < 0) docBroader(k);
        else if (k === "RET") once(docEnter);
        else if (k === "DEL") once(docUp);
        // TAB FOLDS, as it does in org: the model says whether anything did.
        else if (k === "TAB")
          once(() => dsay(k, { kind: "tab" }));
        else if (k === "S-<up>" || k === "S-<down>")
          once(() => atElement(() => cycleHere(k === "S-<up>" ? 1 : -1)));
        else if (k === "o" || k === "!") once(openHere);
        else if (k === "t") once(() => atElement(stateHere));
        else if (k === ":") once(() => atElement(tagsHere));
        else if (k === "SPC")
          once(() => toggleCheckbox(docBinding("org-toggle-checkbox", "SPC")));
        // `S-RET' IS `+' WHEREVER IT IS PRESSED; none of the three reads a caret here.
        else if (k === "+" || k === "S-RET" || k === "M-RET") once(insertHere);
        else if (!flagPress(k, e, DFLAGS)) return;
      }
      e.preventDefault();
    });
    const unlogged = () => {};
    /** WHAT EVERY DELETING SURFACE SAYS THE SAME WAY; a shape spreads it and then
     * names its own mount, its take, and the words that are its alone. */
    const FLAG_WORDS = {
      note: unlogged,
      missing: lacks("delete flags"),
      idle: "dired-do-flagged-delete (no deletions requested)",
      spared: "dired-do-flagged-delete (left standing)",
      unflag: "delete-unflag (flag cleared)",
    };
    // This mount is a Set of ids rather than a renderer, so `missing' is unreachable.
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
    // The held-key guard is here: `ONCE' governs dispatch rows, these four live outside.
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
    const activeSheet = () => (editing ? subtreeSheet : settings ? configSheet : null);
    const sync = (next, message) => note(subtreeSheet, next, message);
    function shut() {
      el("modal").className = ""; editing = null; base = ""; baseProps = null;
      soon(remembered);
      for (const o of DOCEDITS) shutEdit(o);
      docClear();
      el("mdoc").className = "";
      // THE VOCABULARY IS THE SHEET'S: the next one asks again, so a property
      // written between two sheets is offered by the second.
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
    // CALLED at click time: a wrapped widget's `const' handle is in TDZ while this runs.
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
      // Both are SPELLED, so no command without an entry logs another's word.
      "set-state": (args) => (args.keyword ? `→ ${args.keyword}` : "state cleared"),
      delete: () => "deleted",
    };
    // The caller's own word where no entry names one; the table reaches this never.
    const verbed = (name, args, verb) => (VERBED[name] || ((_args, v) => v))(args, verb);
    function fire(b, name, ids, args, verb, how, pin) {
      return postCommand({ name, ids, args, digests: pin }).then((answer) => {
        const results = answer.results || [];
        // The store lags this write, so the per-id 200's digest re-pins the sheet.
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
    // THE TAG DECIDES WHAT `D' MEANS; a MIXED set archives.  The cell is org's `:a:b:'.
    const ARCHIVE = CFG.archiveTag;
    const archivedRow = (id) =>
      String((rowOf(id).cells || {}).tag || "").split(":").indexOf(ARCHIVE) !== -1;
    function archive(b, ids, how) {
      if (ids.length && ids.every(archivedRow)) { confirmDelete(b, ids, how); return; }
      leaving = anchorFor(ids);
      fire(b, "archive", ids, {}, "archived", how)
        .then(spent(leaving)).catch(failed(b, "archive"));
    }
    // THE ONE KEY THAT MOVES A FILE OUT OF THE TREE asks for a typed WORD.
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
      spared: "left standing",
      verb: "archive",
      // Already archived is the delete path, and it asks for a WORD of its own.
      walled: (ids) => ids.every(archivedRow),
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
      // AWAITED one at a time: two landing values in one FILE are two writes under one lock.
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
      // TOGGLE, never assign: the class also carries the sheet's size tier.
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
