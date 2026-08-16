// `@' over the materialize sheet turns a row of the store into a link in the
// prose.  Rules and consequences: AGENTS.hs.
//
// THE WIDGET IS THE RENDERER'S.  `inline: true' is table-view's own compact mode
// — chips alone until `/' summons the editor ONTO THEIR LINE, no title, no hint
// line, no sort marks, a capped window — so the columns, the badge hues, the
// cursor, the filter grammar, its suggestions and DEL all arrive built.  The shape was settled in
// docs/spikes/2026-08-16-refer-picker (widget C) and folded BACK INTO the
// renderer rather than kept beside it.
//
// The NARROWING is the server's: `GET /refer' is `/headlines'' own pipeline with
// two cuts, re-asked on every query change.

    const REFER_LIMIT = 200;
    const REFER_COLS = ["state", "priority", "title", "tag"];
    let picking = null;

    const referUp = () => !!picking;
    // WHICHEVER BOX IS OPEN.  A title edit is a box like any other, so `@' there
    // links INTO THE TITLE rather than drafting a body line under it.
    const referOpen = () => (dediting() ? "dtin" : dparaing() ? "dtext" : null);
    const referBox = () => el(referOpen());
    // The run the picker owns: the `@' it wrote, or the region it stands over.
    const referRun = () => picking.at + (picking.desc === null ? 1 : picking.desc.length);

    function shutRefer(why) {
      if (!picking) return;
      // The mount outlives its DOM: the theme watchers are registered on the
      // document, and `@' mounts once a press.
      if (can(picking.tv, "destroy")) picking.tv.destroy();
      picking = null;
      el("refer").className = "";
      el("rmount").textContent = "";
      if (referOpen()) referBox().focus();
      if (why) append("cmd", "info", `refer: ${why}`);
    }

    /** The caret in the open paragraph — OPENING one where none is. */
    function referAnchor(b) {
      if (!docHolds()) { said(b, "the sheet is not open"); return null; }
      if (!referOpen()) insertHere(null);       // nothing open: `+''s own path
      if (!referOpen()) { said(b, "no line to write in"); return null; }
      return referBox();
    }

    const referDefault = () => savedQuery("default").trim();

    // ONE ASK AT A TIME: a typist outruns the store, and the answer that ARRIVES
    // last would otherwise win over the one asked last.
    let referAsking = null;
    function referFetch(q) {
      if (referAsking) referAsking.abort();
      referAsking = new AbortController();
      const row = editing ? `&row=${encodeURIComponent(editing.id)}` : "";
      const asked = `${q} columns:${REFER_COLS.join(",")}`.trim();
      return getJSON(`/refer${asking(asked)}&limit=${REFER_LIMIT}${row}`,
                     { signal: referAsking.signal });
    }

    /** A SUPERSEDED ask is no failure: the next one is already out. */
    const referFailed = (b) => {
      const say = failed(b, "refer");
      return (e) => { if (!e || e.name !== "AbortError") say(e); };
    };

    /** THE DOMAINS ARE THE STORE'S: `/refer' answers 200 rows at most, so
     *  suggestions drawn from the rows to hand would offer a fraction of what a
     *  `tag:' could name.  `values' is the renderer's own hook for a declared
     *  domain — a column that declares one already keeps it, so the picker
     *  completes a `state:' from exactly what the table does. */
    function stockDomains(view) {
      const vocab = view.vocabulary || {};
      for (const col of view.columns || [])
        if (!col.values && vocab[col.key]) col.values = vocab[col.key];
    }

    /** Under the caret, flipping above it near the foot.  A textarea will not
     *  say where its caret is drawn, so a mirror of it is measured instead. */
    function referPlace(box) {
      const pop = el("rbox");
      const cs = getComputedStyle(box);
      const mirror = document.createElement("div");
      for (const p of ["fontFamily", "fontSize", "fontWeight", "letterSpacing",
                       "lineHeight", "paddingTop", "paddingLeft", "paddingRight",
                       "borderTopWidth", "borderLeftWidth", "boxSizing"])
        mirror.style[p] = cs[p];
      Object.assign(mirror.style, { position: "absolute", visibility: "hidden",
        whiteSpace: "pre-wrap", overflowWrap: "anywhere", top: "0", left: "0",
        width: `${box.clientWidth}px` });
      mirror.textContent = box.value.slice(0, picking.at);
      const dot = part(mirror, "span", "", "​");
      document.body.appendChild(mirror);
      const d = dot.getBoundingClientRect(), m = mirror.getBoundingClientRect();
      const r = box.getBoundingClientRect();
      mirror.remove();
      const line = parseFloat(cs.lineHeight) || 18;
      const x = Math.max(8, Math.min(r.left + (d.left - m.left),
                                     window.innerWidth - pop.offsetWidth - 8));
      let y = r.top + (d.top - m.top) - box.scrollTop + line + 2;
      if (y + pop.offsetHeight > window.innerHeight - 8)
        y = Math.max(8, r.top + (d.top - m.top) - box.scrollTop - pop.offsetHeight - 2);
      pop.style.left = `${Math.round(x)}px`;
      pop.style.top = `${Math.round(y)}px`;
    }

    // The picker's whole widget is the mount's, so an older `table-view.js' is
    // NAMED rather than crashed into — the rule every other surface follows.
    const REFER_VERBS = ["setRows", "getVisible", "getSelection", "select",
                         "getQuery", "stripLastToken", "openFilter", "selectStep"];

    function openRefer(b) {
      const box = referAnchor(b);
      if (!box) return;
      sole();                             // the picker is a momentary surface too
      // A SELECTED REGION BECOMES THE LINK, and its own words are what the link
      // READS AS.  They are no query: seeding the filter with them narrows the
      // store by an accident of phrasing and puts the reader's prose on the strip.
      const at = box.selectionStart;
      const desc = box.selectionEnd > at ? box.value.slice(at, box.selectionEnd) : null;
      // `@' IS WRITTEN, ALWAYS, and the picker is what happens ON TOP of it: the
      // reader typed a character and sees it, so dismissing the picker costs them
      // nothing and leaves the `@' they asked for.  A region is the exception —
      // an `@' over it would eat the very words the link is to read as.
      if (desc === null) writeIn(box, at, at, "@");
      referFetch(referDefault()).then((view) => {
        if (!(view.rows || []).length) { said(b, "no addressable row to link to"); return; }
        stockDomains(view);
        const tv = TableView.mount(el("rmount"), view, {
          inline: true,                  // the renderer's own compact mode
          initialQuery: referDefault(),
          onFilter: (q) => referFetch(q).then((v) => tv.setRows(v.rows || []))
                                        .catch(referFailed(b)),
        });
        if (!can(tv, ...REFER_VERBS))
          { el("rmount").textContent = ""; said(b, lacks("a picker")); return; }
        picking = { tv, at, desc, box };
        el("refer").className = "on";
        referPlace(box);
        const first = tv.getVisible()[0];
        if (first) tv.select(first.id);
        echo("@ → org-glance-material:refer");
      }).catch(referFailed(b));
    }

    /** Take the row under the cursor: the run from `@' becomes the link. */
    function referTake() {
      const at = selectedId(picking.tv);
      const row = picking.tv.getVisible().find((r) => r.id === at);
      if (!row) { append("cmd", "info", "refer: no row is under the cursor"); return; }
      // The reader's own words win over the row's title where they chose some.
      const title = picking.desc !== null ? picking.desc
                                          : ((row.cells || {}).title || row.id);
      const link = `[[glance:${row.id}][${title}]]`;
      const box = picking.box, from = picking.at, to = referRun();
      shutRefer(null);                    // the run the picker owns — the `@', or the region
      writeIn(box, from, to, link);
      echo(`@ → ${title}`);
    }

    /** Put TEXT over [FROM, TO) in BOX, telling the box it was written in. */
    function writeIn(box, from, to, text) {
      spliceIn(box, from, to, text);
      box.dispatchEvent(new Event("input", { bubbles: true }));
    }

    /** DEL walks the query down: the typed text, then the last chip, then the
     *  `@' itself.  DEL deletes where ESC abandons, so the mark goes here and
     *  stands there.  Each rung the mount takes it re-asks through `onFilter'. */
    function referDrop() {
      if (!picking.tv.stripLastToken()) dropMark();
    }

    /** The last rung: the picker goes and takes the `@' it wrote with it. */
    function dropMark() {
      const { box, at, desc } = picking;
      const to = referRun();
      shutRefer("nothing left to drop");
      if (desc === null) writeIn(box, at, to, "");   // a region's words are the reader's
    }

    // `@' IS A CHARACTER FIRST: the binding claimed the key, so the literal is
    // written here — mid-word that is all that happens, and at a WORD BOUNDARY the
    // picker rides on top of it.
    function referKey(b) {
      const box = referOpen() && referBox();
      if (box && active() === box && box.selectionEnd === box.selectionStart) {
        // A region is as explicit as a prefix, so no boundary gates it.
        const at = box.selectionStart;
        const before = at === 0 ? "" : box.value[at - 1];
        if (!(at === 0 || /\s/.test(before))) {
          writeIn(box, at, box.selectionEnd, "@");     // mid-word it is text alone
          return;
        }
      }
      openRefer(b);
    }

    /** Is the renderer's own filter box holding the keys?  The MOUNT answers:
     *  a box under `#rmount' will not stay the only one there. */
    const referTyping = () => can(picking.tv, "filtering") && picking.tv.filtering();

    // THE PICKER HOLDS THE KEYBOARD while it is up, or a keystroke meant for it
    // lands in the prose behind it.  Claimed outright: taking a row SHUTS the
    // picker, so a listener further along would find no momentary surface up and
    // read the same RET as its own.
    document.addEventListener("keydown", (e) => {
      if (!picking || e.defaultPrevented) return;
      const k = keyName(e);
      if (!k) return;
      // The filter box is the RENDERER'S, ESC included: `inline' leaves it in one
      // step and stands on a row, so nothing here rehearses that ladder.
      if (referTyping()) return;
      e.preventDefault(); e.stopPropagation();
      const step = rowStep(k) || (k === "C-n" ? 1 : k === "C-p" ? -1 : 0);
      if (step) { stepIn(picking.tv, step); return; }
      if (k === "/") { picking.tv.openFilter(); return; }
      if (k === "DEL") { referDrop(); return; }
      if (k === "RET") { referTake(); return; }
      if (k === "ESC") { shutRefer(null); return; }
    }, true);
