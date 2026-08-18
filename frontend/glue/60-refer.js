// `@' in the sheet writes a store row into the prose as a link.  Rules: AGENTS.hs.

    const REFER_LIMIT = 200;
    const REFER_COLS = ["state", "priority", "title", "tag"];
    let picking = null;

    const referUp = () => !!picking;
    const referOpen = () => (dediting() ? "dtin" : dparaing() ? "dtext" : null);
    const referBox = () => el(referOpen());
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

    function referAnchor(b) {
      if (!docHolds()) { said(b, "the sheet is not open"); return null; }
      if (!referOpen()) insertHere(null);       // nothing open: `+''s own path
      if (!referOpen()) { said(b, "no line to write in"); return null; }
      return referBox();
    }

    const referDefault = () => savedQuery("default").trim();

    // ONE ASK AT A TIME, or the answer that ARRIVES last wins over the one asked last.
    let referAsking = null;
    function referFetch(q, kind) {
      if (referAsking) referAsking.abort();
      referAsking = new AbortController();
      const row = editing ? `&row=${encodeURIComponent(editing.id)}` : "";
      const want = kind ? `&kind=${encodeURIComponent(kind)}` : "";
      const asked = `${q} columns:${REFER_COLS.join(",")}`.trim();
      return getJSON(`/refer${asking(asked)}&limit=${REFER_LIMIT}${row}${want}`,
                     { signal: referAsking.signal });
    }

    const referFailed = (b) => {
      const say = failed(b, "refer");
      return (e) => { if (!e || e.name !== "AbortError") say(e); };
    };

    function stockDomains(view) {
      const vocab = view.vocabulary || {};
      for (const col of view.columns || [])
        if (!col.values && vocab[col.key]) col.values = vocab[col.key];
    }

    const KIND_KEY = "kind";
    /** The kind the query declares, and the query with it TAKEN OUT: `kind:' says
     *  what the link will be, so it never narrows the rows it is written from. */
    function splitKind(q) {
      const text = String(q || "");
      if (typeof TableView.parseQuery !== "function") return { rows: text, kind: null };
      const mine = TableView.parseQuery(text, REFER_COLS.concat([KIND_KEY]))
        .filter((t) => t.key === KIND_KEY && !t.negated);
      let rows = text;
      for (let i = mine.length - 1; i >= 0; i -= 1)
        rows = rows.slice(0, mine[i].start) + rows.slice(mine[i].end);
      const last = mine.length ? mine[mine.length - 1].value : "";
      return { rows: rows.replace(/\s+/g, " ").trim(), kind: last || null };
    }

    function drawKind() {
      const badge = el("rkind");
      badge.textContent = picking.kind ? `kind:${picking.kind}` : "";
      badge.className = picking.kind ? "on" : "";
    }

    function askKind() {
      const held = picking, b = held.b;
      const shown = (held.kinds || []).map((k) =>
        ({ label: k.kind, hint: `${k.rows} ${k.rows === 1 ? "row" : "rows"}`, tag: k.kind }));
      askFrom("kind", shown,
              "RET takes it · a kind not listed is just typed · DEL clears it · ESC leaves",
              (c) => {
                if (picking !== held) return;   // the picker went while it was up
                takeKind(b, String(c.tag || "").trim());
              });
    }
    function tookKind(view) {
      picking.kind = view.kind || null;
      picking.kinds = view.kinds || picking.kinds;
      drawKind();
    }
    // `K' AND `/ kind:' ARE ONE STATE; the canonical slug is the server's.
    function takeKind(b, raw) {
      const held = picking, base = splitKind(held.tv.getQuery()).rows;
      if (!raw) { held.tv.setQuery(base); tookKind({}); echo("kind cleared"); return; }
      referFetch(base, raw).then((view) => {
        if (picking !== held) return;
        held.tv.setRows(view.rows || []);
        tookKind(view);
        held.tv.setQuery(picking.kind ? `${base} ${KIND_KEY}:${picking.kind}`.trim() : base);
        echo(picking.kind ? `${KIND_KEY}:${picking.kind}` : "kind cleared");
      }).catch(referFailed(b));
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

    // `filtering' IS LOAD-BEARING: without it the listener below never stands aside.
    const REFER_VERBS = ["setRows", "getVisible", "getSelection", "select",
                         "getQuery", "stripLastToken", "openFilter", "selectStep",
                         "filtering"];

    function openRefer(b) {
      const box = referAnchor(b);
      if (!box) return;
      sole();                             // the picker is a momentary surface too
      const at = box.selectionStart;
      const desc = box.selectionEnd > at ? box.value.slice(at, box.selectionEnd) : null;
      // NO `@' OVER A REGION: it would eat the words the link is to read as.
      if (desc === null) writeIn(box, at, at, "@");
      referFetch(referDefault()).then((view) => {
        if (!(view.rows || []).length) { said(b, "no addressable row to link to"); return; }
        stockDomains(view);
        const tv = TableView.mount(el("rmount"), view, {
          inline: true,                  // the renderer's own compact mode
          initialQuery: referDefault(),
          onFilter: (q) => {
            const asked = splitKind(q);
            return referFetch(asked.rows, asked.kind).then((v) => {
              tv.setRows(v.rows || []);
              if (picking) tookKind(v);
            }).catch(referFailed(b));
          },
        });
        if (!can(tv, ...REFER_VERBS))
          { el("rmount").textContent = ""; said(b, lacks("a picker")); return; }
        picking = { tv, at, desc, box, b, kind: null, kinds: view.kinds || [] };
        drawKind();
        el("refer").className = "on";
        referPlace(box);
        const first = tv.getVisible()[0];
        if (first) tv.select(first.id);
        echo("@ → org-glance-material:refer");
      }).catch(referFailed(b));
    }

    function referTake() {
      const at = selectedId(picking.tv);
      const row = picking.tv.getVisible().find((r) => r.id === at);
      if (!row) { append("cmd", "info", "refer: no row is under the cursor"); return; }
      const title = picking.desc !== null ? picking.desc
                                          : ((row.cells || {}).title || row.id);
      const kind = picking.kind ? `?kind=${picking.kind}` : "";
      const link = `[[glance:${row.id}${kind}][${title}]]`;
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

    function referDrop() {
      if (!picking.tv.stripLastToken()) dropMark();
    }

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

    const referTyping = () => picking.tv.filtering();

    // TAKING A ROW SHUTS THE PICKER, so a listener further along would read the same RET.
    document.addEventListener("keydown", (e) => {
      if (momentary() !== "refer" || e.defaultPrevented) return;
      const k = keyName(e);
      if (!k) return;
      // The filter box is the RENDERER'S, ESC included; no ladder is rehearsed here.
      if (referTyping()) return;
      e.preventDefault(); e.stopPropagation();
      const step = rowStep(k) || walkStep(k);
      if (step) { stepIn(picking.tv, step); return; }
      // ONE PRESS, ONE RUNG: a held DEL would walk chip, picker and `@' in one key.
      if (repeating(e)) return;
      if (k === "/") { picking.tv.openFilter(); return; }
      // `k' IS THE PREVIOUS ROW in the vim dialect, so the kind takes the shift.
      if (k === "K") { askKind(); return; }
      if (k === "DEL") { referDrop(); return; }
      if (k === "RET") { referTake(); return; }
      if (k === "ESC") { shutRefer(null); return; }
    }, true);
