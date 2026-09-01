// THE MATERIALIZE SHEET: two panes over one subtree, one flush carrying both — AGENTS.hs.

    // AN ORG TABLE IN THE DOC IS A TABLE-VIEW MOUNT.  Elm draws one
    // <glance-table> host carrying the view as a `view' property and NO children
    // of its own; this element mounts the renderer into itself, so Elm's vdom —
    // which owns zero children here — never fights the mount.  The composite and
    // its leaves stay Elm's walk stops; only the DRAW moves onto the renderer.
    if (window.customElements && !customElements.get("glance-table")) {
      class GlanceTable extends HTMLElement {
        constructor() { super(); this._view = null; this._tv = null; }
        set view(v) { this._view = v; if (this._tv) this._tv.setView(v); else this._mount(); }
        get view() { return this._view; }
        connectedCallback() { this._mount(); }
        disconnectedCallback() { if (this._tv) { this._tv.destroy(); this._tv = null; } }
        _mount() {
          if (this._tv || !this.isConnected || !this._view) return;
          this._tv = TableView.mount(this, this._view, {});
        }
      }
      customElements.define("glance-table", GlanceTable);
    }

    let editing = null;
    // A CAPTURE IS THE SHEET OVER A SUBTREE THAT DOES NOT EXIST YET.  The handle
    // is the served DRAFT, and `capture' is everything true of it and of no
    // materialized row: the tag it will be filed under, the cycle its state door
    // offers, and the line `%?' stood on.  ASKED AS THE HANDLE'S OWN FIELD, so
    // every door below reads the one flag rather than a second mutable.
    const capturing = () => !!(editing && editing.capture);
    // What a draft is called before a title has been typed into it.
    const CAPTURE_WORD = "the capture";
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
    // WHICH COLUMN POINT STANDS IN inside a table, `null' the whole row -- the
    // model's own axis, mirrored so the push can select the cell in the widget.
    let dcol = null;
    // A DRAFT WHOSE `%?' STOOD IN THE BODY still owes its editor: the row that
    // line became lands a macrotask behind the fill, so the open waits for it.
    // ONE SHOT — the next fill is somebody else's document.
    let dlanding = false;
    // WHAT THE READER TYPED for a planning entry the draft drew RESOLVED, by
    // keyword (`settleDraftPlan').  A document's own, so a fresh fill empties it.
    let dtyped = {};
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
        dcol = (now.col === undefined ? null : now.col);
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
    // The word for what a grain key landed on is the model's, so Elm says it.
    const dsay = (k, m) => { dwrote = keySaid(k); dsend(m); };
    const dmount = flagPort(dsend, () => dflags);
    // THE READING LINE AS A WHOLE PERCENT, a FORWARD dep taken as a thunk: the
    // preference is a `const' in a later part, so it is CALLED here rather than
    // named at load.
    const readingPct = () => readingLine();
    /** THE BAND POINT'S ROW MOVES IN: the nearest scroller over the document,
     * and its INSIDE -- the border stepped over, which is where `scrollTop' is
     * measured from and what a line down it is a fraction of.  THE SCROLLER IS
     * ASKED FOR RATHER THAN NAMED, and it is the one `block:"nearest"' would
     * have moved: `#mdoc' stretches to `#mpanes' where the document fits its
     * box, and grows past it -- `#mpanes' clipping -- where it does not.  NULL
     * where nothing scrolls, which is a document that fits and a page with no
     * layout alike. */
    function docBand(row) {
      for (let pane = row.parentElement; pane; pane = pane.parentElement) {
        if (pane.clientHeight > 0 && pane.scrollHeight > pane.clientHeight + 1)
          return { pane, top: pane.getBoundingClientRect().top + pane.clientTop,
                   height: pane.clientHeight };
        if (pane.id === "modal") break;
      }
      return null;
    }
    // The `.de' scroll-margin as a number: three of the pane's own lines, and
    // the very band `block:"nearest"' honours.
    const bandOff = (row) =>
      parseFloat(getComputedStyle(row).scrollMarginBlockStart) || 0;
    // Forbidden over the TABLE's rows; THE ONE CALL SITE, and the whole of what
    // a page with no layout can still ask for.
    const askScroller = (row, block) => {
      if (row && typeof row.scrollIntoView === "function")
        row.scrollIntoView({ block });
    };
    /** ONE PLACEMENT LAW, TWO CALLERS: put one of ROW's edges -- its `top', its
     * `bottom' or its `middle' -- on a LINE measured in pixels down the pane's
     * band, which `line' reads off the band and the row.  The pane clamps at
     * either end, so every ask is "if possible".  WITH NO LAYOUT there is
     * nothing to measure and the row asks its pane's own scroller instead,
     * under `block'. */
    function placeRow(row, want) {
      const band = row && docBand(row);
      if (!band) { askScroller(row, want.block); return; }
      const r = row.getBoundingClientRect();
      const edge = want.edge === "top" ? r.top
                 : want.edge === "bottom" ? r.bottom
                 : r.top + r.height / 2;
      band.pane.scrollTop += edge - band.top - want.line(band, row);
    }
    // The reading line itself, in pixels down the band.
    const READING = { edge: "bottom", block: "nearest",
                      line: (b) => b.height * readingPct() / 100 };
    /** POINT'S ROW COMES TO REST ON THE READING LINE: after a move, a row whose
     * BOTTOM has fallen below the line drawn that far down the pane is scrolled
     * up until its bottom sits on it -- DIRECTION-FREE, and if possible, the
     * pane clamping at its end.  A ROW TALLER THAN THE BAND ABOVE THE LINE has
     * no rest there, and one standing above the pane's top is the
     * scroll-margin's business, so both keep the `nearest' ask. */
    function keepInView(row) {
      const band = row && docBand(row);
      if (!band) { askScroller(row, "nearest"); return; }
      const r = row.getBoundingClientRect(), line = READING.line(band);
      if (r.top < band.top || r.height > line || r.bottom - band.top <= line)
        { askScroller(row, "nearest"); return; }
      placeRow(row, READING);
    }
    /** `C-l' IS org's own `recenter-top-bottom': point's row to the pane's
     * MIDDLE, then its TOP under the scroll-margin, then its BOTTOM above it,
     * and round again.  ANY OTHER KEY STARTS THE CYCLE OVER, which is the whole
     * of what makes a run of presses one gesture. */
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
    // A KEY ARMS THE ECHO (`dsay'), the programmatic walk does not (`dsend'):
    // `n'/`p' say their word, the flag-delete's own step stays quiet.
    const docStep = (step, k) =>
      k ? dsay(k, { kind: "step", by: step }) : dsend({ kind: "step", by: step });
    const docFiner = (k) => dsay(k, { kind: "finer" });
    const docBroader = (k) => dsay(k, { kind: "broader" });
    // `B' -- SHIFT-b -- CLIMBS THE GRAIN to the owner in one press, the headline
    // over a body; `b' itself is `f' reversed and steps a row back.
    const docClimb = (k) => dsay(k, { kind: "climb" });
    /** `M-<left>'/`M-<right>' OVER A NESTED HEADLINE: org's own
     * `org-promote-subtree' / `org-demote-subtree'.  THE MODEL OWNS THE ROWS,
     * THE WALLS AND THE WORD -- the star arithmetic is a LINE rewrite the
     * paragraph splice cannot express -- so the key names the direction and
     * nothing else, and both the write and the refusal come back named.
     */
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
      openTitle(t ? t.val : "");
    }
    /** THE TITLE EDIT over the head row.  OVER A BARE DRAFT IT IS THE CAPTURE
     * ITSELF — the template brought nothing to fill in, so there is nothing for
     * `C-c C-c' to gather that this box does not already hold: `RET' writes the
     * jot and `ESC' drops it whole.  That is today's one-line form kept key for
     * key (`+', RET, the line, RET), and the row carries it as `bare' so the
     * commit and the escape read one answer rather than two. */
    const openTitle = (val) =>
      openEdit(DTITLE,
               { id: "CELL:title", val, bare: capturing() && bareDraft(editing) });
    /** Is the draft the BARE DEFAULT — star-space and nothing else?  A fact
     * about the TEMPLATE, so it is read off the ANSWER: a title typed into a
     * bare draft leaves it bare, and a template that brought a drawer, a
     * planning entry, a body or a tag of its own never was.  INHERITED FACTS
     * COUNT: a filter pinning a state makes the draft rich, and `C-c C-c' the
     * door out of it.
     *
     * THE DESTINATION TAG IS NOT ONE OF THEM.  It is the capture's ADDRESS —
     * the answer `+' already asked for — and the tag cell names it so the pane
     * can say where this lands.  A bare template under a tag is still the bare
     * draft, which is what keeps the tagged jot four keys as well. */
    const bareDraft = (h) =>
      !String((h.cells || {}).title || "").trim()
      && !String((h.cells || {}).state || "")
      && !String((h.cells || {}).priority || "")
      && !tagsBeyond(h).length
      && !(h.properties || []).length && !(h.planning || []).length
      && !(h.children || []).length && !bodyBelow(h.body).trim();
    // The run a draft wears APART FROM its destination: what the template spelled
    // and what the filter lent, which is what makes a draft rich.
    const tagsBeyond = (h) =>
      cellTags((h.cells || {}).tags).filter((t) => t !== (h.capture || {}).tag);
    // THE BODY UNDER THE HEADLINE LINE.  A subtree's `body' opens with its own
    // headline (the pane draws that line from `cells' and never splices it), so
    // every reading of "what is under the title" cuts the first line off here.
    const bodyBelow = (body) => String(body || "").split("\n").slice(1).join("\n");
    /** THE EDITOR A DRAFT'S BODY POINT OWES, once the fill that placed it has
     * settled.  The row `%?' named is the one the walk landed on, so this is the
     * very box `RET' there would open, seeded off the same row — the caret at the
     * line's end, `point' naming a LINE and no offset there is to aim at.  A row
     * no editor claims (a child headline) keeps point and nothing opens: the
     * pane's own `RET' is the way in, as it is on any doc. */
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
      // A DRAFT HAS NOWHERE UP: it is not in a file yet, so there is no parent to
      // climb to and no sheet under it to come back to.  ESC is the way out.
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
    /** Materialize the child at INDEX, and run K over the document it landed
     * on.  K RIDES THE REREAD'S OWN CONTINUATION: the fetch is async and the
     * fill behind it is not, so a caller that ran beside this would act on the
     * mirrors of the document it just left. */
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
    /** TWO ANSWERS, ONE ASK: each one-shot disarms the other, so the model's
     * word on the CARGO and its word on `docSaid' cannot both land.  THE
     * PROTOCOL IS SPELLED HERE ALONE, and no caller wires the pair by hand. */
    const answerOnce = (onCommit, onSaid) => {
      dcommit = (cargo) => { dwrote = null; onCommit(cargo); };
      dwrote = (what) => { dcommit = null; onSaid(what); };
    };
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
      // A DERIVED BOX IS ITS CHILDREN'S TO TELL: an item with checkbox children
      // wears the face rolled up from them (`boxFace', Doc.elm), so ticking it by
      // hand is refused -- toggle a leaf and the parents follow.
      if (drows.some((x) => x.owner === r.id && checkboxAt(x) !== null)) {
        said(b, "derived from children"); return;
      }
      const now = was === " " || was === "-" ? "X" : " ";
      editPara(r, r.text.replace(CHECKBOX, `$1[${now}]`), () => said(b, `[${now}]`));
    }
    /** `X' — HIDE DONE CHECKBOXES.  A display-only, ephemeral UI mode the Elm
     * side owns: on the row's own list run when point is inside one, across
     * every list when it is not.  The model decides the scope and speaks the
     * word, so this only opens the door and echoes the answer.
     *
     * `X' IS A CHARACTER FIRST, the way `@' is: the binding claimed the key, so
     * while a doc field holds the keys this writes the letter and the mode
     * stays put.  A raw sheet has no rows to hide, so it says so. */
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
      // THE LINE VOUCHES FOR A TIGHT BOX'S VERTICAL: an empty slot has no
      // baseline, so its rect sits on the row's baseline at zero height
      // (docs/bugs/fixed/2026-08-25-the-title-box-sits-on-the-baseline-when-the-title-is-empty.md).
      // The row carries the axis a cell's content cannot; the cell keeps the
      // horizontal, where flex places even an empty box truly.
      const row = o.tight && tr.closest ? tr.closest(".de") : null;
      const rowed = row && typeof row.getBoundingClientRect === "function";
      const [padT, padB] = rowed ? rowPads(row) : [0, 0];
      const rr = rowed ? row.getBoundingClientRect() : a;
      // Absolute against the PADDING box, so a scrolling pane owes clientTop + scrollTop.
      s.top = `${(rowed ? rr.top + padT : a.top) - b.top - pane.clientTop + pane.scrollTop}px`;
      s.height = `${rowed ? rr.height - padT - padB : a.height}px`;
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
    /** The ROW's vertical padding, measured ONCE PER ROW the way `inset' measures:
     * `getComputedStyle' forces a style recalc, and `placeEdit' runs per scroll
     * frame while a box is up. */
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
    // A LEFT CLICK SELECTS the element it lands on; a DOUBLE CLICK edits it, as
    // RET does.  Point is the model's -- the click NAMES the row and the model
    // moves point there; the edit opens on the very row named, waiting on no
    // round-trip.  Clicks inside the open edit box carry no row and are ignored.
    const deUnder = (e) => (e.target instanceof Element ? e.target.closest("#mdoc .de") : null);
    const rowOfDe = (de) => (de ? drows.find((x) => x.id === de.getAttribute("data-id")) : null);
    const foldUnder = (e) => (e.target instanceof Element ? e.target.closest("#mdoc .fold") : null);
    const gtUnder = (e) => (e.target instanceof Element ? e.target.closest("#mdoc glance-table") : null);
    // The mounted table-view handle a <glance-table> host carries.
    const tvOf = (host) => (host ? /** @type {any} */ (host)._tv : null);
    // SELECT THE CELL POINT STANDS IN, in the mounted widget: the leaf row's
    // own id, `null' the whole row.  The widget's row ids ARE the Elm leaf ids,
    // so no lookup -- Elm's point drives the renderer's selection.
    const tableSelSync = () => {
      const r = drows[dat];
      if (!r || !r.owner) return;
      const comp = drows.find((x) => x.id === r.owner);
      if (!comp || comp.name !== "table") return;
      const tv = tvOf(el("mdoc").querySelector(`.de[data-id="${comp.id}"] glance-table`));
      if (tv) {
        if (dcol == null) tv.select(r.id);
        else tv.select(r.id, dcol);
      }
    };
    el("mdoc").addEventListener("click", (e) => {
      if (edit && e.target instanceof Node && el("dpara").contains(e.target)) return;
      // A CLICK IN THE TABLE'S WIDGET names a CELL: the renderer set its own
      // selection first (the event bubbles out through it), so read it back and
      // move point there.  A header sorts and a link opens -- neither is a cell.
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
      // THE SPINE SIGN FOLDS ITS OWN DRAWER in one press: it names the row, so
      // point need not have reached it -- unlike TAB, which folds at point.
      if (foldUnder(e)) {
        dsend({ kind: "fold", id: r.id });
        return;
      }
      const dpv = e.target instanceof Element ? e.target.closest(".dpv") : null;
      // A PLANNING VALUE names its OWN entry, not the whole line: `plan' is the
      // value's index among the line's values, so point lands on the value.
      if (dpv && r.entries) {
        dsend({ kind: "select", id: r.id, plan: [...de.querySelectorAll(".dpv")].indexOf(dpv) });
      } else {
        dsend({ kind: "select", id: r.id });
      }
    });
    el("mdoc").addEventListener("dblclick", (e) => {
      // A double-click on the spine sign is two folds, not an edit: the sign
      // has no text to open, so it never becomes the editor's row.
      if (foldUnder(e)) return;
      const r = rowOfDe(deUnder(e));
      if (r) docEnter(r);
    });
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

    // A DEBUG SURFACE, read-only, always on: the editor state the closures hold
    // out of the console and the harness.  `window.__glance.editor()' answers the
    // questions a doc-editing bug asks first -- where point is, what the box holds,
    // what marker the draft was seeded with, what the model drew.  The single most
    // useful thing is `caret': the cursor's OFFSET in the box, which no rendered
    // row shows.  `rows' is the mirror Elm drew (`D' is the open draft's own row).
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
    // door.
    el("dwhen").addEventListener("input", () => {
      if (!ddating()) return;
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
      if (!dateVirgin()) return;
      selectWhole(el("dwhen"));
    }
    /** Is the open widget standing on the selection THE OPEN MADE -- the whole
     * value selected, nothing typed and nothing walked?  THE FLAG IS THE
     * SHELL'S (`laidWhole', 00-core.js), so this door and the dispatcher's
     * copy-and-cut carve-out (`selecting', 70-shell.js) cannot part on what it
     * means.  The widget is asked for as well: the flag outlives the box, and a
     * shut widget is nothing to re-select into. */
    const dateVirgin = () => ddating() && laidWhole(el("dwhen"));
    const dateBinding = (k) => docBinding(edit.row.b.command, k);
    /** What the door onto KEY is called.  The two settable words are the summon
     * keys' own commands, so a widget raised by `RET' over an entry echoes what
     * `C-c C-s' echoes; CLOSED has no key of its own and takes org's own name
     * for the function that writes a planning line. */
    const PLAN_COMMANDS = { SCHEDULED: "org-glance-overview:schedule",
                            DEADLINE: "org-glance-overview:deadline" };
    const planCommand = (key) => PLAN_COMMANDS[key] || "org-add-planning-info";
    /** `C-c C-s' / `C-c C-d' in the material document: the widget over the
     * ENTRY AT POINT's own SCHEDULED / DEADLINE slot.  The TABLE's own pair of
     * keys is untouched: they ask over the marked rows through the shipped
     * prompt, and reach the same grammar at the same door. */
    function planHere(b, keyword) {
      if (!editing || raw) { said(b, "no document here"); return; }
      // A STANDING WIDGET IS SWITCHED, NEVER REFUSED: the reader asked for the
      // other keyword's box, so the open one leaves by the very restore ESC
      // takes -- byte-identical, the keyword a summon ghosted in going back with
      // it -- and the asked word is summoned over what that left.  WITHOUT THE
      // DOOR'S ECHO: this switch says what it opened, and an `ESC → keyboard-quit'
      // logged behind it would name a key nobody pressed.  The SAME key is a
      // re-summon and takes the same road.  Any OTHER open edit still refuses:
      // it holds text nobody has decided about.
      if (ddating()) restoreSheetEdit();
      else if (sheetOpen())
        { said(b, "an edit is open — RET writes it, ESC leaves"); return; }
      // ORG SCHEDULES THE ENTRY AT POINT, so a CHILD row is materialized first
      // -- the very move `RET' makes over it -- and the widget opens over the
      // child's own planning line, where the commit below lands it.  ONE STEP
      // IN, never two: the summon runs over the document the materialize
      // landed on rather than through this door a second time.
      const r = docRowAt();
      if (r && r.kind === "child") { into(r.index, () => summonPlan(b, keyword)); return; }
      summonPlan(b, keyword);
    }
    /** The widget over the open document's own KEYWORD slot.  THE LINE IS DRAWN
     * IF ABSENT, the draft pair's own move one row up -- a widget that stands in
     * the value's place needs the place to exist. */
    function summonPlan(b, keyword) {
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
                        today: dateNow() });
      said(b, "RET sets it · empty clears it · ESC leaves");
    }
    /** `RET' over the widget.  DRY OVER AN OFFER AND FINAL OVER A COMPLETED
     * VALUE: an offer under point is taken and nothing else happens, and the
     * same key over the finished term applies -- the pair box's own `takeOffer'
     * rule.  A REFUSAL IS SPOKEN HERE, above the commit and while what was typed
     * is still on screen to fix. */
    function dateKey(b) {
      // A take is a keystroke the field never saw, so the widget redraws itself;
      // no `input' fires, and the caret it leaves is what ends the open's own
      // selection (`laidWhole', 00-core.js).
      if (menuTake(wmenu, "dwhen", dateMoved)) return;
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
     * RAW text goes to the server, which resolves it ONCE against its own clock,
     * and the pane redraws off THAT answer as it does off every other.  Two
     * resolutions against two clocks is the midnight bug the one-clock-read
     * invariant exists to prevent.
     *
     * WHICH DOOR is the ROW's own question, and the two meet the same wall.  A
     * MATERIALIZED CHILD HAS NO ROW ID -- `set-planning' addresses rows, so the
     * entry would land on the ROOT headline -- and it takes the COMMIT door the
     * pair box's planning-routed pair already takes: the model sets the entry,
     * the cargo carries the planning list, and `post' aims it at the child
     * (`?child='), where `settledPlanning' reads the raw phrase at the very key
     * `set-planning' meets.  ONE TRANSPORT, never a second spelling of it --
     * the clear rides it too, an empty value being how org takes an entry off.
     *
     * A DRAFT HAS NO ROW ID EITHER, and takes that same road for that same
     * reason: the entry it is owed is a planning entry the capture carries, and
     * the capture command is what writes it. */
    function commitDate(b, typed) {
      const row = edit.row, keyword = row.key, h = editing;
      shutEdit(DDATE);
      if (row.add) undraftPlan(row);
      if (h.child !== null || capturing()) {
        // THE WIDGET KEEPS ITS OWN WORD over the model's: the row the box stood
        // in already says the keyword, and the model's word names a landing out
        // of a drawer this door never touched.
        answerOnce(() => said(b, typed || "cleared"), (what) => said(b, what));
        dsend({ kind: "addprop", key: keyword, value: typed });
        return;
      }
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
     * FOLLOWS, because what they move is the field's own text. */
    const dateStep = (k) =>
      k === "S-<right>" ? 1 : k === "S-<left>" ? -1
      : k === "S-<down>" ? 7 : k === "S-<up>" ? -7 : 0;
    /** THE WALK MOVES THE DAY AND NOTHING ELSE: the value the reading R stood on,
     * carried to the day TO.
     *
     * A step LANDS ON A WHOLE DATE -- which is what keeps the walk and the offers
     * from ever both asking -- and everything the standing text carried BESIDE
     * its day rides along.  Two things do: org's own BRACKET, in the kind that
     * stood, since a step that wrote its answer back bare would drop the
     * inactive intent the reader spelled (and, under the plain stamp wall, leave
     * the field holding a value the very next `RET' refuses); and the stamp's
     * TAIL, a time of day or a repeater cookie, kept BYTE FOR BYTE because
     * recomposing a stamp is what loses the `+1y' nobody asked to move.  A BARE
     * PHRASE CARRIES NEITHER, so it walks as bare ISO -- the one spelling every
     * wall reads back.
     *
     * A RANGE COLLAPSES onto the day the step landed on, which is what the walk
     * has always done with one: its two ends move by a rule of their own or not
     * at all, and this walk has none to give them. */
    const dateStepped = (r, to) => {
      // EITHER BRACKETED READING, and the reader is the one that knows which:
      // org's own spelling kept verbatim, and the grammar read inside a pair.
      if (!r.bracketed) return isoDay(to);
      const stood = r.stamp;
      const head = stood.indexOf("--") === -1 && STAMP_HEAD.exec(stood);
      // THE TAIL IS EVERY BYTE THE DAY AND ITS WEEKDAY DID NOT OCCUPY, the
      // closing bracket EXCLUDED: it is handed to the one org-stamp writer,
      // which closes its own stamp as it does everywhere else.
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
    // What an org stamp OPENS with: the bracket, the day, and the weekday behind
    // it where one is written.  Everything past this is the stamp's TAIL.  BUILT
    // FROM `DATE', so the day the head measures is the day the wall reads;
    // declared below it, since a `const' read from above is a TDZ error.
    const STAMP_HEAD = new RegExp(`^[<[]${DATE}(?:[ \\t]+[A-Za-z]+)?`);

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
    const dayReal = (c) => !!c && c.m >= 1 && c.m <= 12 && c.d >= 1 && c.d <= daysInMonth(c.y, c.m);
    const addDays = (c, n) => civil(dnum(c) + n);
    /** A resolved day FIT TO SHOW: every field finite, and the day real on the
     * calendar.  The arithmetic above can return neither -- a shift far enough
     * out runs off `Date''s own range and every field comes back `NaN' -- and a
     * ghost that drew one would present `<NaN-NaN-NaN undefined>' as a
     * RESOLUTION, with RET committing that string.  THE GHOST PRESENTS A STAMP
     * OR A REFUSAL, never a third thing. */
     const showable = (c) => !!c
      && Number.isFinite(c.y)
      && Number.isFinite(c.m)
      && Number.isFinite(c.d)
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
    /** C as org's own stamp, with TIME where there is one and TAIL -- a time of
     * day and a repeater cookie the walk is carrying over, kept byte for byte --
     * riding after it, inside the closer.  INACTIVE takes org's other bracket,
     * `[...]'.  THE ONE ORG-STAMP WRITER: a bracket kind is chosen here and
     * never spelled a second time, and no caller cuts a closer back off. */
    const stampOf = (c, time, inactive, tail) =>
      `${inactive ? "[" : "<"}${isoDay(c)} ${dowOf(c)}`
      + `${time ? " " + time : ""}${tail || ""}${inactive ? "]" : ">"}`;
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
        // The RAW stamp rides with it: the step splices its new day into THAT
        // rather than composing one, which is how a tail survives (`dateStepped').
        const m = /^[<[](\d+)-(\d{1,2})-(\d{1,2})/.exec(s);
        const c = m ? { y: +m[1], m: +m[2], d: +m[3] } : null;
        return { ok: true, bracketed: true, stamp: s,
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
      // ONE ROSTER OF DAY WORDS with the filter's (`Glance.Query.dayWords'):
      // `today' and `tomorrow' are what both read, and `*today*' is `today''s
      // old spelling, read here too so one field spells one grammar.
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
    /** The phrase inside org's own brackets, resolved wearing THE ACTIVITY THE
     * BRACKET NAMES: `[today]' is the clock day inactive, `<today>' the bare
     * word's own bytes.  The body is read by the very grammar a bare field reads
     * — every alias of it, English, shift and ISO — so the bracket adds one
     * thing and nothing else.  `null' where this reading has nothing to say: a
     * MISMATCHED pair names no timestamp org holds, an empty one names no day,
     * and a body outside the grammar keeps THE BRACKET'S OWN refusal rather than
     * earning a second one.  THE INVERSION TRAVELS, though — it is the one
     * refusal a reading spends a word of its own on, and an interval runs the
     * wrong way inside brackets exactly as it does outside them.
     * THE ANSWER SAYS IT WAS BRACKETED, the same field `verbatimDate' answers
     * with: the walk writes the bracket that stood back (`dateStepped'), and a
     * resolution alone cannot tell `<today>' from a bare `today'. */
    function wrappedDate(s, today) {
      // THE HEAD IS THE CALLER'S GUARANTEE -- `readsDate' reaches this only past
      // `verbatimDate''s own bracket test -- so the KIND is read off it and the
      // closer has only to MATCH.
      const inactive = s[0] === "[";
      if (!s.endsWith(inactive ? "]" : ">")) return null;
      const body = s.slice(1, -1).trim();
      if (!body) return null;
      const r = resolvedDate(body, today, inactive);
      if (!r) return null;
      if (!r.ok) return r.short === INVERTED ? r : null;
      return { ...r, bracketed: true };
    }
    /** PHRASE resolved by the grammar BOTH readings share, stamped in the
     * bracket kind INACTIVE names.  THE ENGLISH PHRASE IS READ AHEAD OF THE
     * REST, the server's own order: it is the one reading with a refusal of its
     * own to spend, and a phrase it declines falls through to the rest.  A
     * RESOLUTION THAT CANNOT BE SPELLED IS A REFUSAL, and a HARD one: the
     * arithmetic ran off the calendar and no further character walks it back.
     * `null' where the phrase is not this grammar's at all, which each caller
     * answers for itself. */
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
     * the server's own reader is driven over. */
    function readsDate(text, today) {
      const s = String(text == null ? "" : text).trim();
      // NOTHING TYPED IS NOTHING RESOLVED.  Clearing an entry is the WIDGET's
      // law and not the grammar's: an empty field names no date, so the ghost
      // stays dark over one and the commit sends the clear without asking here.
      if (!s) return noDate();
      // ORG'S OWN SPELLING OUTRANKS THE WRAPPED READING, and an OPEN bracket is
      // still being written: only a CLOSED bracket that reparses NOTHING is read
      // as the grammar wrapped, so nothing the wall already answers moves by a
      // byte (`planningTimestamp' orders its arms the same way).
      const v = verbatimDate(s);
      if (v) return v.ok || v.unfinished ? v : (wrappedDate(s, today) || v);
      return resolvedDate(s, today, false) || noDate();
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
    // `*today*' RIDES ALONG UNOFFERED, the way the filter's own list carries it:
    // `shiftBase' reads it, so its prefixes are a term being written like any
    // other and the ghost owes them the same silence.  It sits at the END
    // because the offer list is drawn in order and the retired spelling is not
    // what a fresh field proposes first.
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
        // The model's own word for where the pair landed rides the CARGO.
        answerOnce((cargo) => spoke(cargo.said || "property written"), spoke);
        dsend({ kind: "addprop", key, value });
        return;
      }
      const val = el("dtin").value;
      // READ BEFORE THE SHUT: the answer is the open box's own, and `shutEdit'
      // takes the box away.
      const jot = bareCapture();
      // THE KEY IS THE READER'S AND THE COMMAND IS WHAT IT RAN: whatever reached
      // this box, over a bare draft it FINALIZES A CAPTURE rather than writing a
      // title into a row that does not exist.
      const finalize = docBinding("org-capture-finalize", (b || {}).seq || "RET");
      // NOTHING TO CAPTURE IS NO COMMIT, AND THE BOX STAYS UP behind the word —
      // the wall every other edit's is asked at, above the shut and while what
      // was typed is still on screen to be fixed.  The sheet stands with it.
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
    // THE PLANNING LINE'S OWN PAIR: the widget stands in the value's place, so
    // the line is drawn to stand in and taken back with the box.
    const redraftPlan = (keyword) => dsend({ kind: "draftplan", key: keyword });
    const undraftPlan = (r) => dsend({ kind: "undraftplan", id: r.back });
    /** Take the open sheet edit down and put back whatever it DREW, answering
     * with the WORD for what stood there.  SILENT, because the ECHO BELONGS TO
     * THE DOOR THAT ASKED: ESC's own is below, and the summon switch takes the
     * same byte-identical restore and then speaks for the box it opens. */
    function restoreSheetEdit() {
      const drawn = edit && edit.o === DPARA && edit.row.add ? edit.row : null;
      const pair = edit && edit.o === DPAIR ? edit.row : null;
      // AND THE KEYWORD THE SUMMON GHOSTED IN goes with the box: the planning
      // line comes back the bytes it was, its own ABSENCE included.
      const when = edit && edit.o === DDATE ? edit.row : null;
      for (const o of DOCEDITS) shutEdit(o);
      if (drawn) undraft(drawn);
      if (pair) undraftPair(pair);
      if (when && when.add) undraftPlan(when);
      return when ? "the planning line" : pair ? "the drawer" : "element";
    }
    // THE ESCAPE IS FROM THE EDIT: the box goes, the drawn row with it, and the
    // drawer is the bytes it was — nothing typed here ever entered its list.
    // The restore shut every shape, so `cancelEdit' is asked for the ECHO alone.
    //
    // IN THE BARE DRAFT'S TITLE THE EDIT IS THE CAPTURE, and there is nothing
    // under it to come back to: the escape is from the whole capture, which
    // never existed, so it takes the sheet with it.
    function cancelSheetEdit() {
      if (bareCapture()) {
        leaveSheet();
        echo("ESC → keyboard-quit (nothing captured)");
        return;
      }
      cancelEdit(restoreSheetEdit());
    }
    // Standing in the title of a BARE draft: the one box whose RET captures and
    // whose ESC drops everything.  ASKED IN ONE PLACE, so the two cannot part.
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
      // A DRAFT'S CARGO IS HELD, NEVER POSTED: no file stands behind it, and the
      // capture goes out WHOLE at `C-c C-c'.  The model already holds what was
      // typed and pushes it to the mirrors a macrotask behind, which is what the
      // commit reads — so the door still answers with the model's own word.
      // The word goes out FIRST: it is the answer to the key that was pressed,
      // and the settle below writes again.
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
    /** THE DRAFT'S OWN STAND-IN FOR THE ROUND TRIP A ROW MAKES.  A row's planning
     * value is posted raw and comes back TRANSFORMED, and the pane redraws off
     * that answer; a draft posts nothing, so the phrase the reader typed used to
     * stand in the planning line until the capture landed.  The preview is made
     * flesh here instead: the entry is redrawn as the GHOST'S OWN RESOLVER reads
     * it — the same `readsDate' the ghost previewed with, drift-pinned to the
     * wall over one corpus — so what the pane says is what the file will hold.
     *
     * WHAT TRAVELS IS STILL WHAT WAS TYPED (`typedPlan'): the wall transforms
     * ONCE, against the server's clock, and this reading is for ink alone — the
     * very rule the widget already keeps.  A PHRASE THE RESOLVER REFUSES STAYS
     * RAW and meets the wall's own sentence at the commit, the sheet standing.
     * CLOSED is not settable, so it keeps its verbatim value untouched. */
    function settleDraftPlan(cargo) {
      for (const [key, value] of cargo.planning || []) {
        if (DATED.indexOf(key) === -1) continue;
        const read = readsDate(value, dateNow());
        // ALREADY ITS OWN RESOLUTION — org's own spelling passes through — so
        // there is nothing to redraw and this settle reaches a fixed point.
        if (!read.ok || read.stamp === value) continue;
        dtyped[key] = { raw: value, shown: read.stamp };
        dsend({ kind: "addprop", key, value: read.stamp });
      }
    }
    /** PLAN as the reader typed it: the phrase behind each entry the settle
     * above resolved, and the entry itself where nothing stands behind it or
     * where a later door moved it. */
    const typedPlan = (plan) => (plan || []).map(([key, value]) => {
      const was = dtyped[key];
      return [key, was && was.shown === value ? was.raw : value];
    });

    /** `C-c C-c' OVER A DRAFT: the whole capture at one press, through the ONE
     * command that mints a blob — its id, its shard path, its creation drawer
     * and its ledger line.  THE ARGS ARE THE PANE'S OWN STANDING CARGO widened
     * with what the head row holds: `body', `properties' and `planning' are the
     * mirrors the model pushed, and the title, state, priority and tags come off
     * the handle's cells, where every door above wrote them.
     *
     * THE BODY IS WHAT STANDS UNDER THE HEADLINE LINE: the cargo opens with that
     * line (the pane draws it from `cells' and never splices it), and the
     * capture composes its own headline out of the cells beside it — so sending
     * the cargo whole would spell the headline twice. */
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
      // THE HEADLINE WEARS EXACTLY THESE, the destination tag among them: the
      // tags door writes this cell and the filter's own tags were inherited into
      // it, so the list the draft shows is the list the blob is written with.
      const tags = cellTags(c.tags);
      if (tags.length) args.tags = tags;
      postCommand({ name: "capture", args }).then((a) => {
        // THE CURSOR LANDS ON THE NEW ROW when the watch delivers it and the
        // view carries it — `arrived' spends this on the next settle.
        arriving = a.id || null;
        shut();
        said(b, tag ? `captured · :${tag}:` : `captured · ${a.file}`);
        append("cmd", "info",
               `headline ${JSON.stringify(title)} captured into ${a.file}`);
      }).catch(failed(b, "capture"));
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
      // WHERE `%?' STOOD, in the body's own line coordinates: a served draft
      // carries one and a materialized subtree does not, and `null' is the
      // headline row — which is where a fresh fill lands anyway.
      const at = h.capture ? h.capture.point : null;
      dsend({ kind: "fill",
              ...(at === null ? {} : { landing: at }),
              lines: body.split("\n"),
              own: h.ownLines === undefined ? body.split("\n").length : h.ownLines,
              props: h.properties || [],
              plan: h.planning || [],
              planKeys: PLANNING,
              // THE SETTABLE WORDS DRAW ALWAYS: SCHEDULED and DEADLINE stand as
              // unset slots where the file gave no value, so the reader can set
              // one.  CLOSED is org's own bookkeeping and keeps no unset slot.
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
      // ANY OTHER KEY STARTS `C-l''s CYCLE OVER, org's own rule for it.
      if (k !== "C-l") recentres = 0;
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
        if (step) docStep(step, k);
        else if (depth > 0) docFiner(k);
        else if (depth < 0) docBroader(k);
        else if (k === "B") docClimb(k);
        else if (k === "RET") once(docEnter);
        else if (k === "DEL") once(docUp);
        // TAB FOLDS, as it does in org: the model says whether anything did.
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
    /** THE CAPTURE SHEET'S OWN VERBS.  NOTHING IS OWED TO A FILE: a draft has no
     * digest to flush against and no bytes on disk to conflict with, so it is
     * never dirty, never flushed, never refreshed — `C-x C-s' says so and `ESC'
     * simply shuts.  The BORN-AT-OPEN MEMORY IS TRIVIALLY EMPTY, which is the
     * whole of why the escape is byte-identical: no file ever existed. */
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
    // ONE SHORTHAND PER SHEET, each bound to the sheet it speaks for (`cnote' is
    // settings'): `note' writes the sheet's own `state', so a verb that reached
    // for another sheet's shorthand would move a state its caller never owned.
    const sync = (next, message) => note(subtreeSheet, next, message);
    const capnote = (next, message) => note(captureSheet, next, message);
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
    // ORG'S `:a:b:' AS A LIST, and back.  The doc cell carries the headline's own
    // spelling, and a draft's tags are edited as the list every door speaks in.
    const cellTags = (cell) => String(cell || "").split(":").filter(Boolean);
    const tagCell = (list) => (list.length ? `:${list.join(":")}:` : "");
    /** WHAT A ROW-ADDRESSING DOOR WRITES ON A DRAFT, or `null' where the command
     * is not one a draft holds.  A CAPTURE NAMES NO ROW, so the four commands
     * that set a headline's own cells land HERE instead — one interception, and
     * the title, state, priority and tags doors above it are the doors they
     * always were, popup and palette included.  The answer wears the wire's own
     * shape, so nothing downstream can tell the two apart.  The empty digest
     * rides back untouched: it is the create pin, and `fire' only re-pins off a
     * non-empty one. */
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
    // THE HEAD LINE REDRAWN off the handle's own cells: a draft has no reread to
    // bring them back, so the shell hands them straight to the pane.
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
        // A DRAFT IS NO ROW and `titleOf' would name none, so the log says what
        // it is instead of looking a title up in a table that has never held it.
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
    /** THE SHEET OVER A SERVED DRAFT — the same open, over an answer with no file
     * behind it.  A is `GET /capture''s answer: the shape `/headline' serves,
     * with `id' null and `digest' "" — the create pin — plus the tag's cycle and
     * the line `%?' stood on.
     *
     * EVERY DRAFT OPENS EDITING, at the place `%?' named: a reader who asked for
     * a capture is composing one, and a pane that made them press `RET' first
     * asked a question `+' had already answered.  On the BARE draft that box IS
     * the capture, which is the whole of the bare-draft law: the reader's keys
     * are `+', RET, the line, RET, exactly today's form's. */
    function showDraft(b, tag, a) {
      editing = draftOf(tag, a);
      raw = false;
      el("mfile").textContent = captureWhere(tag, a);
      fill(editing);
      capnote("synced");
      el("modal").className = "on";
      soon(remembered);
      el("mtext").blur();
      // POINT ON THE HEADLINE OPENS HERE AND NOW: the title edit reads the
      // handle's own cells and owes the pane's rows nothing.  A BODY LINE waits
      // for the fill to settle — the row `%?' named lands a macrotask behind the
      // send, and it is that row the editor is seeded from.
      if (editing.capture.point === null)
        openTitle(String((editing.cells || {}).title || ""));
      else dlanding = true;
      said(b, bareDraft(editing) ? "a headline · RET captures it · ESC leaves"
                                 : "C-c C-c captures · ESC leaves");
    }
    /** The editing handle a served draft stands behind: the answer's own fields,
     * and the three that are the capture's alone under `capture'.  THE SPANLESS
     * SHAPE IS THE CORRECT ONE for a document with no file: `spanAt' null makes
     * every row's span null, so the links door says there is nothing to open and
     * no delete can name a byte range that does not exist. */
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
      // THE PHRASES BEHIND THE PLANNING LINE ARE THIS DOCUMENT'S: a fresh fill
      // draws another's, and a phrase kept across would be spoken of a row that
      // never heard it.
      dtyped = {};
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
    // An org link reads as its DESCRIPTION (or its target, description-less), the
    // way the body draws it -- a breadcrumb shows the word, not `[[..]]'.
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
    // A CAPTURE IS COMMITTED OR IT NEVER WAS: nothing about a draft is owed to a
    // file, so the flush on leaving, the `beforeunload' keepalive and the
    // socket's reload all read it as clean — which is what makes ESC free.
    const dirty = () => editing !== null && !capturing()
      && (raw ? el("mtext").value !== base : edited() !== baseProps);
