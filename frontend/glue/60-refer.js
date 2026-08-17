// `@' over the materialize sheet turns a row of the store into a link in the
// prose.  Rules and consequences: AGENTS.hs.
//
// THE WIDGET IS THE RENDERER'S — `inline: true' — and the NARROWING is the
// server's: `GET /refer' is `/headlines'' own pipeline with two cuts.

    const REFER_LIMIT = 200;
    const REFER_COLS = ["state", "priority", "title", "tag"];
    let picking = null;

    const referUp = () => !!picking;
    // WHICHEVER BOX IS OPEN: a title edit takes the link into the TITLE.
    const referOpen = () => (dediting() ? "dtin" : dparaing() ? "dtext" : null);
    const referBox = () => el(referOpen());
    // The run the picker owns: the `@' it wrote, or the region it stands over.
    const referRun = () => picking.at + (picking.desc === null ? 1 : picking.desc.length);

    function shutRefer(why) {
      if (!picking) return;
      // The mount outlives its DOM: the theme watchers sit on the document.
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

    // ONE ASK AT A TIME, or the answer that ARRIVES last wins over the one asked last.
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

    /** THE DOMAINS ARE THE STORE'S, over the 200 rows to hand.  A column that
     *  declares its own keeps it, so picker and table complete alike. */
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

    // An older `table-view.js' is NAMED rather than crashed into (`lacks').
    // `filtering' is LOAD-BEARING: without it the listener below never stands
    // aside and the filter box gets none of the keys it is for.
    const REFER_VERBS = ["setRows", "getVisible", "getSelection", "select",
                         "getQuery", "stripLastToken", "openFilter", "selectStep",
                         "filtering"];

    function openRefer(b) {
      const box = referAnchor(b);
      if (!box) return;
      sole();                             // the picker is a momentary surface too
      // A SELECTED REGION BECOMES THE LINK and reads as its own words; it is no query.
      const at = box.selectionStart;
      const desc = box.selectionEnd > at ? box.value.slice(at, box.selectionEnd) : null;
      // `@' IS WRITTEN, ALWAYS, the picker riding on top.  A region is the
      // exception: an `@' over it would eat the words the link reads as.
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

    /** DEL's rungs OUT HERE: the last chip, then the `@' — what is typed and the
     *  box itself are the MOUNT's, so it has the keys until they are gone.  It
     *  re-asks through `onFilter' for every chip it gives up. */
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

    // `@' IS A CHARACTER FIRST: the binding claimed the key, so this writes it.
    function referKey(b) {
      const box = referOpen() && referBox();
      if (box && active() === box && box.selectionEnd === box.selectionStart) {
        const at = box.selectionStart;
        const before = at === 0 ? "" : box.value[at - 1];
        if (!(at === 0 || /\s/.test(before))) {
          writeIn(box, at, box.selectionEnd, "@");     // mid-word it is text alone
          return;
        }
      }
      openRefer(b);
    }

    /** Is the renderer's own filter box holding the keys?  THE MOUNT answers. */
    const referTyping = () => picking.tv.filtering();

    // THE PICKER HOLDS THE KEYBOARD, claimed outright: taking a row SHUTS it, so
    // a listener further along would read the same RET as its own.
    document.addEventListener("keydown", (e) => {
      if (!picking || e.defaultPrevented) return;
      const k = keyName(e);
      if (!k) return;
      // The filter box is the RENDERER'S, ESC included; no ladder is rehearsed here.
      if (referTyping()) return;
      e.preventDefault(); e.stopPropagation();
      const step = rowStep(k) || (k === "C-n" ? 1 : k === "C-p" ? -1 : 0);
      if (step) { stepIn(picking.tv, step); return; }
      // ONE PRESS, ONE PART, past the movement a held key is FOR: a held DEL
      // reaches here the moment the mount hands the box back, and unguarded
      // would walk the chip, the picker and the `@' in one key, then eat the
      // prose behind it.  Every rung below is one-per-press.
      if (repeating(e)) return;
      if (k === "/") { picking.tv.openFilter(); return; }
      if (k === "DEL") { referDrop(); return; }
      if (k === "RET") { referTake(); return; }
      if (k === "ESC") { shutRefer(null); return; }
    }, true);
