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
    const DCELLS = CFG.dcells;
    let drows = [], dat = 0, dcol = null, dgrain = "element";
    let dparent = {};
    const downersOf = (id) => {
      const chain = [];
      for (let o = dparent[id]; o; o = dparent[o]) chain.push(o);
      return chain;
    };
    let dlines = [];
    // Not `#dlist''s `dat'-th child: a composite draws its leaves inside it.
    let dcursor = null;
    const dflags = new Set();
    const dmount = {
      flagRow: (id) => { dflags.add(id); drawDoc(); },
      unflagRow: (id) => { dflags.delete(id); drawDoc(); },
      getFlagged: () => [...dflags],
      clearFlags: () => { dflags.clear(); drawDoc(); },
    };
    const cellsOf = (o) => DCELLS.map((k) => ({ key: k, val: (o || {})[k] || "" }));
    // A `* ' at COLUMN 1 is a headline rather than an item, hence the guard.
    const LIST_AT = /^(\s*)([-+*]|\d+[.)])(\s+|$)/;
    function listOpener(line) {
      const m = LIST_AT.exec(String(line));
      return m && !(m[2] === "*" && !m[1]) ? m : null;
    }
    const BEGIN_AT = /^\s*#\+begin_(\S+)/i;
    const closerOf = (name) => new RegExp(
      "^\\s*#\\+end_" + name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")
        + "\\s*$", "i");
    function blockRun(lines, i, end) {
      const shut = closerOf(BEGIN_AT.exec(lines[i])[1]);
      for (let j = i + 1; j < end; j += 1) if (shut.test(lines[j])) return j + 1;
      return -1;
    }
    const TABLE_AT = /^\s*\|/;
    const rides = (line) => !!listOpener(line) || /^\s/.test(String(line));
    // ONE BLANK LINE STAYS IN — org's rule.  Two close the list.
    function listRun(lines, i, end) {
      const base = listOpener(lines[i])[1].length;
      const items = [];
      let at = i, from = -1, last = i;
      while (at < end) {
        if (String(lines[at]).trim() === "") {
          let j = at;
          while (j < end && String(lines[j]).trim() === "") j += 1;
          if (j - at > 1 || j >= end || !rides(lines[j])) break;
          at = j; continue;
        }
        const m = listOpener(lines[at]);
        if (m && m[1].length <= base) {
          if (from !== -1) items.push({ from, to: last });
          from = at;
        } else if (!rides(lines[at])) break;
        at += 1; last = at;
      }
      if (from !== -1) items.push({ from, to: last });
      return { to: last, items };
    }
    function runsIn(lines, a, b) {
      const out = [];
      let from = -1;
      for (let i = a; i <= b; i += 1) {
        const blank = i === b || String(lines[i]).trim() === "";
        if (!blank) { if (from === -1) from = i; continue; }
        if (from === -1) continue;
        out.push({ from, to: i, text: lines.slice(from, i).join("\n") });
        from = -1;
      }
      return out;
    }
    // Composites emit INLINE as `[whole, leaf1..leafN]'; the walk reads that
    // order.  OWN is the server's `ownLines', so no byte is both a paragraph
    // and the child that owns it.
    function blocksIn(lines, own) {
      const out = [];
      const end = Math.max(0, Math.min(own, lines.length));
      const cut = (a, b) => lines.slice(a, b).join("\n");
      // `up' indexes OUT: the leaf's IMMEDIATE parent — the grain is a LADDER.
      const whole = (a, b, name, leaves) => {
        const at = out.length;
        out.push({ from: a, to: b, text: cut(a, b), grain: "composite", name });
        for (const p of leaves)
          out.push({ from: p.from, to: p.to, text: p.text, grain: "leaf",
                     up: at });
      };
      const pushItem = (from, to, up) => {
        const at = out.length;
        out.push({ from, to, text: cut(from, to), grain: "leaf", up });
        const base = listOpener(lines[from])[1].length;
        for (let n = from + 1; n < to; n += 1) {
          const m = listOpener(lines[n]);
          if (m && m[1].length > base) {
            const run = listRun(lines, n, to);
            for (const it of run.items) pushItem(it.from, it.to, at);
            n = run.to - 1;
          }
        }
      };
      let i = 1;
      while (i < end) {
        if (String(lines[i]).trim() === "") { i += 1; continue; }
        if (BEGIN_AT.test(lines[i])) {
          const shut = blockRun(lines, i, end);
          if (shut !== -1) {
            whole(i, shut, BEGIN_AT.exec(lines[i])[1].toLowerCase(),
                  runsIn(lines, i + 1, shut - 1));
            i = shut; continue;
          }
        }
        if (TABLE_AT.test(lines[i])) {
          let j = i;
          while (j < end && TABLE_AT.test(lines[j])) j += 1;
          const rows = [];
          for (let n = i; n < j; n += 1)
            rows.push({ from: n, to: n + 1, text: cut(n, n + 1) });
          whole(i, j, "table", rows);
          i = j; continue;
        }
        if (listOpener(lines[i])) {
          const run = listRun(lines, i, end);
          const at = out.length;
          out.push({ from: i, to: run.to, text: cut(i, run.to),
                     grain: "composite", name: "list" });
          for (const it of run.items) pushItem(it.from, it.to, at);
          i = run.to; continue;
        }
        let j = i + 1;
        while (j < end && String(lines[j]).trim() !== ""
               && !listOpener(lines[j]) && !BEGIN_AT.test(lines[j])
               && !TABLE_AT.test(lines[j])) j += 1;
        out.push({ from: i, to: j, text: cut(i, j), grain: "element" });
        i = j;
      }
      return out;
    }
    function docFrom(h) {
      // Derived from `dstars' so the two cannot drift; the arithmetic is CSS's.
      el("mdoc").style.setProperty("--g-doc-indent",
                                    String(dstars(docLevel()).length));
      const was = drows[dat] ? drows[dat].id : null;
      drows = [];
      dlines = String(h.body || "").split("\n");
      drows.push({ id: "H", kind: "head", cells: cellsOf(h.cells) });
      const own = h.ownLines === undefined ? dlines.length : h.ownLines;
      let seq = 0;
      const idOf = [];
      for (const b of blocksIn(dlines, own)) {
        const id = `B${seq++}`;
        idOf.push(id);
        drows.push({ id, kind: "para", grain: b.grain, name: b.name || null,
                     owner: b.up === undefined ? null : idOf[b.up],
                     from: b.from, to: b.to, text: b.text, was: b.text });
      }
      dparent = {};
      for (const r of drows)
        if (r.kind === "para" && r.owner) dparent[r.id] = r.owner;
      for (const c of h.children || [])
        drows.push({ id: `C${c.index}`, kind: "child", index: c.index,
                     level: c.level, cells: cellsOf(c) });
      const back = drows.findIndex((r) => r.id === was);
      dat = back === -1 ? 0 : back;
      dcol = null;
      drawDoc();
    }
    // Bottom-up, so an earlier range is never moved by a later splice.
    function bodyText(drop) {
      const gone = drop || new Set();
      const out = dlines.slice();
      // ONE GRAIN SPEAKS FOR A RANGE: a composite and its leaves cover the same
      // lines, so a moved or going ancestor silences every rung under it.
      const spoken = new Set(drows.filter((r) => r.kind === "para"
        && (gone.has(r.id) || r.text !== r.was)).map((r) => r.id));
      const silenced = (r) => downersOf(r.id).some((o) => spoken.has(o));
      const paras = drows.filter((r) => r.kind === "para"
        && !silenced(r)).slice().reverse();
      for (const p of paras) {
        if (gone.has(p.id)) {
          const spare = p.to < out.length - 1 && String(out[p.to]).trim() === "";
          out.splice(p.from, p.to - p.from + (spare ? 1 : 0));
        } else if (p.text !== p.was) {
          out.splice(p.from, p.to - p.from, ...p.text.split("\n"));
        }
      }
      return out.join("\n");
    }
    const dclass = (r, here) => `de d-${r.grain === "leaf" ? "item"
      : r.grain === "composite" ? `comp d-${r.name}` : r.kind}`
      + (here ? " dat" : "") + (dflags.has(r.id) ? " dfl" : "");
    // ONE OWNER PER BYTE: what no child claims is drawn INERT (`dg').
    function drawKids(box, parent, from, at0) {
      let at = at0 === undefined ? parent.from : at0, j = from;
      while (j < drows.length && drows[j].kind === "para"
             && drows[j].owner === parent.id) {
        const kid = drows[j];
        if (kid.from > at)
          part(box, "div", "dg", dlines.slice(at, kid.from).join("\n"));
        const kbox = part(box, "div", dclass(kid, j === dat));
        if (j === dat) dcursor = kbox;
        const under = j + 1 < drows.length && drows[j + 1].owner === kid.id;
        if (under) {
          const head = drows[j + 1].from;
          if (head > kid.from)
            drawPara(kbox, { from: kid.from, to: head,
                             text: dlines.slice(kid.from, head).join("\n") });
          // Start where the head ENDED, or its lines are drawn twice.
          j = drawKids(kbox, kid, j + 1, head);
        } else { drawPara(kbox, kid); j += 1; }
        at = kid.to;
      }
      if (at < parent.to)
        part(box, "div", "dg", dlines.slice(at, parent.to).join("\n"));
      return j;
    }
    function drawDoc() {
      const list = el("dlist");
      list.textContent = "";
      dcursor = null;
      for (let i = 0; i < drows.length; i += 1) {
        const r = drows[i];
        const here = i === dat;
        const row = part(list, "div", dclass(r, here));
        if (here) dcursor = row;
        if (r.grain === "composite") {
          i = drawKids(row, r, i + 1) - 1;
        } else if (r.kind === "para") drawPara(row, r);
        else drawCells(row, r, here);
      }
      keepInView(dcursor);
      placeEdit();
    }
    // Forbidden over the TABLE's rows (the renderer's); the suite counts call
    // sites.  `block:"nearest"' honours `.de''s `scroll-margin', the scrolloff.
    function keepInView(row) {
      if (row && typeof row.scrollIntoView === "function")
        row.scrollIntoView({ block: "nearest" });
    }
    // ORG-CLEANED STARS: every star but the last a space, two spaces a level.
    const dstars = (level) =>
      " ".repeat(Math.max(0, 2 * (level - docLevel()))) + "* ";
    const docLevel = () => (editing && editing.level) || 1;
    const shown = (r) => (r.cells || []).filter((c) => c.val);
    function drawPara(row, r) {
      const at = elementSpan(r);
      const box = part(row, "div", "dp");
      if (at) drawText(box, r.text, at[0], null); else box.textContent = r.text;
    }
    function drawCells(row, r, here) {
      part(row, "span", "ds", dstars(r.kind === "child" ? r.level : docLevel()));
      shown(r).forEach((c, j) => {
        const cell = part(row, "span", `dc dc-${c.key}`
          + (here && j === dcol ? " don" : ""));
        // EXACTLY ONE of the two paths writes the cell: presetting the text and
        // then appending segments leaves the brackets standing in a real DOM.
        if (c.key === "title" && r.kind === "head"
            && editing && typeof editing.titleAt === "number")
          drawText(cell, c.val, editing.titleAt, null);
        else cell.textContent = c.val;
        if (c.key === "state") cell.style.color = badgeColor(c.val);
      });
    }
    // MOVEMENT IS TWO AXES (docs/design-rhymes.md): siblings, then the grain.
    const colStep = (k) => (k === "<right>" || k === "l" ? 1
                          : k === "<left>" || k === "h" ? -1 : 0);
    const grainStep = (k) => (k === "f" ? 1 : k === "b" ? -1 : 0);
    const dcells = (r) => (r && (r.kind === "head" || r.kind === "child")
                            ? shown(r).length : 0);
    function docStep(step) {
      if (!drows.length) return;
      const cur = drows[dat];
      let i = dat + step;
      if (cur && cur.grain === "leaf") {
        // A COMPOSITE sits between any two parents' runs, so every leaf between
        // two siblings is deeper in this one's subtree.
        while (i >= 0 && i < drows.length) {
          const kin = drows[i];
          if (kin.grain !== "leaf") { drawDoc(); return; }
          if (kin.owner === cur.owner) break;
          i += step;
        }
        if (i < 0 || i >= drows.length) { drawDoc(); return; }
        dat = i;
      } else {
        while (i >= 0 && i < drows.length && drows[i].grain === "leaf") i += step;
        if (i < 0 || i >= drows.length) { drawDoc(); return; }
        dat = i;
      }
      if (!dcells(drows[dat])) dcol = null;
      dgrain = dcol !== null ? "cell"
             : drows[dat].grain === "leaf" ? "leaf" : "element";
      drawDoc();
    }
    const kidsOf = (id) =>
      drows.filter((r) => r.kind === "para" && r.owner === id).length;
    function docFiner(k) {
      const say = keySaid(k), r = drows[dat];
      if (!r) return;
      const kids = r.kind === "para" ? kidsOf(r.id) : 0;
      if (kids) {
        // The first child immediately follows its parent in emission order.
        dat += 1; dgrain = "leaf"; drawDoc();
        say(`grain-finer (${r.name || "item"} 1/${kids})`);
      } else if (dcells(r)) { moveDocCol(k, 1); }
      else if (r.grain === "leaf") say("grain-finer (at the finest)");
      else say("grain-finer (nothing finer here)");
    }
    function docBroader(k) {
      const say = keySaid(k), r = drows[dat];
      if (!r) return;
      if (dcol !== null) {
        dcol = null; dgrain = "element"; drawDoc();
        say("grain-broader (element)");
      } else if (r.grain === "leaf") {
        const i = drows.findIndex((x) => x.id === r.owner);
        if (i === -1) { say("grain-broader (at the element grain)"); return; }
        dat = i;
        dgrain = drows[i].grain === "leaf" ? "leaf" : "element";
        drawDoc();
        say(`grain-broader (${drows[i].name || (drows[i].grain === "leaf"
                              ? "item" : drows[i].kind)})`);
      } else say("grain-broader (at the element grain)");
    }
    function moveDocCol(k, step) {
      const say = keySaid(k), n = dcells(drows[dat]);
      if (!n) { say("next-column (no cells in this element)"); return; }
      const want = dcol === null ? (step > 0 ? 0 : n - 1) : dcol + step;
      dcol = want < 0 || want >= n ? null : want;
      dgrain = dcol === null ? "element" : "cell";
      drawDoc();
      say(`next-column (${dcol === null ? "element mode" : shown(drows[dat])[dcol].key})`);
    }
    // The three regions the lens lifts out sit ABOVE the paragraphs, so a body
    // offset past the title line is displaced by ONE constant (`bodyShift').
    // OFFSETS ARE IN CHARACTERS (docs/invariants.md); JS counts UTF-16 units.
    const chars = (s) => Array.from(String(s));
    const clen = (s) => chars(s).length;
    const cslice = (s, a, b) => chars(s).slice(a, b).join("");
    const bodyShift = () => clen(editing.org || "") - clen(editing.body || "");
    const charOf = (line) =>
      dlines.slice(0, line).reduce((n, l) => n + clen(l), 0) + line;
    let dlinks = [];
    const linksIn = (at, links) => (links || dlinks).filter((l) =>
      l.span && l.span[0] >= at[0] && l.span[1] <= at[1]);
    function drawText(into, text, at, links) {
      const n = clen(text);
      let cut = 0;
      for (const l of linksIn([at, at + n], links)) {
        const a = l.span[0] - at, b = l.span[1] - at;
        if (a < cut) continue;
        if (a > cut) part(into, "span", "dt", cslice(text, cut, a));
        part(into, "span", "dl", l.desc);
        cut = b;
      }
      if (cut === 0) { into.textContent = text; return; }
      if (cut < n) part(into, "span", "dt", cslice(text, cut));
    }
    function elementSpan(r) {
      const at = (editing.span || {}).start;
      if (at === undefined || !r) return null;
      if (r.kind === "child") return r.span ? [r.span.start, r.span.end] : null;
      if (r.kind === "head") return [at, at + charOf(1)];
      if (r.kind !== "para") return null;
      const shift = at + bodyShift();
      return [shift + charOf(r.from), shift + charOf(r.to)];
    }
    function openHere() {
      const r = drows[dat], b = docBinding("org-glance-overview:open");
      const at = elementSpan(r);
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
        const back = drows.findIndex((r) => r.kind === "child" && r.index === h.child);
        if (back !== -1) { dat = back; drawDoc(); }
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
    const commitDoc = (what, drop) =>
      commitDocWith(bodyText(drop), () => { if (what) echo(`RET → ${what}`); });
    const CHECKBOX = /^(\s*(?:[-+*]|\d+[.)])\s+)\[( |X|x|-)\]/;
    const checkboxAt = (r) =>
      r && r.kind === "para"
        ? (CHECKBOX.exec((r.text || "").split("\n")[0]) || [])[2] ?? null
        : null;
    function toggleCheckbox(b) {
      const r = drows[dat];
      const was = checkboxAt(r);
      if (was === null) { said(b, "no checkbox here"); return; }
      const now = was === " " ? "X" : was === "-" ? "X" : " ";
      r.text = r.text.replace(CHECKBOX, `$1[${now}]`);
      drawDoc();
      commitDocWith(bodyText(), () => said(b, `[${now}]`));
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
          if (retry) setTimeout(() => { if (editing === h) read(false); }, 300);
          drawDoc();
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
    const docElAt = () => dcursor;
    // `tight' over the TITLE CELL's box, right edge at the tags.  A headline
    // with no title cell has none, so the anchor falls back to the line.
    const dTitleAt = () =>
      (dcursor && dcursor.querySelector && dcursor.querySelector(".dc-title"))
        || dcursor;
    const DTITLE = {
      box: "dtitle", pane: "mdoc", fields: ["dtin"],
      mount: () => null, anchor: dTitleAt, tight: true,
      edge: () =>
        dcursor && dcursor.querySelector && dcursor.querySelector(".dc-tags"),
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
        r.text = text;
        commitDoc("paragraph written");
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
      const gone = new Set(ids);
      const named = drows.filter((r) => gone.has(r.id));
      const taken = named.filter((r) => r.kind === "para");
      if (named.length !== taken.length)
        append("sync", "warn",
               "a headline is not deleted from the sheet — this writes elements only");
      if (!taken.length) { echo(`D → org-delete-element (${how(0)})`); return; }
      const body = bodyText(new Set(taken.map((r) => r.id)));
      commitDocWith(body,
        () => echo(`D → org-delete-element (${how(taken.length)} taken)`));
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
    const PCOLS = [ { key: "key", header: "Key" },
                    { key: "value", header: "Value" } ];
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
      drows = []; dlines = []; dflags.clear(); dcursor = null; dlinks = [];
    }
    /** Fill it from H, or empty it in RAW mode where the textarea is the view. */
    function docFill(h, isRaw) {
      dflags.clear();
      dlinks = h.links || [];
      if (isRaw) { drows = []; dlines = []; drawDoc(); } else docFrom(h);
    }
    /** Where point stands, as a row ID and a column — what a remount stashes. */
    const docCursor = () => ({ at: drows[dat] ? drows[dat].id : null, col: dcol });
    /** Put it back after one, landing on the row ID names where it survives. */
    function docRestore(at, col) {
      const back = drows.findIndex((r) => r.id === at);
      if (back !== -1) dat = back;
      dcol = col;
    }
    /** The row ID names, for a caller holding an id rather than a place. */
    const docRowById = (id) => drows.find((x) => x.id === id);
    /** The checkbox under point, when the stop there has one. */
    const checkboxHere = () => checkboxAt(drows[dat]);
    let prows = [];
    let pmount = null, pseq = 0;
    // Caught in the CAPTURE phase, so the scroller inside PANE need not be named.
    function mountOnce(host, cols, opts, pane) {
      const m = TableView.mount(el(host), { columns: cols, rows: [] }, opts);
      el(pane).addEventListener("scroll", placeEdit, true);
      return m;
    }
    function mounted() {
      if (pmount) return pmount;
      pmount = mountOnce("mptable", PCOLS, {
        palette: true,
        flags: true,
        actionHints: false,
        flagHelp: "d/D delete · u unflag",
      }, "mprops");
      return pmount;
    }
    const prowsOf = () =>
      prows.map((r) => ({ id: r.id, cells: { key: r.key, value: r.val } }));
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
      // `setRows' keeps flags deliberately, so a new drawer must ask for the drop.
      pmount.clearFlags();
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
    function pdelete(ids, how) {
      const gone = new Set(ids);
      const cleared = prows.filter((r) => gone.has(r.id) && r.fixed);
      for (const r of cleared) r.val = "";
      prows = prows.filter((r) => r.fixed || !gone.has(r.id));
      repaint();
      const also = cleared.map((r) => r.key).join(", ");
      echo(`D → org-delete-property (${how(ids.length)}${also ? ` · ${also} cleared` : ""})`);
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
    // dired's `d'/`D'/`u' over four surfaces, each declaring a SHAPE — CLAUDE.md (UI).
    function flagKey(k, s, say) {
      const m = s.mount();
      const at = s.at();
      if (at === null) { say(s.none); return; }
      const flags = flagsOn(m) ? m.getFlagged() : [];
      if (k === "D" || (k === "d" && flags.indexOf(at) !== -1)) {
        const ids = flags.length ? flags : [at];
        // SPENT before the take, or the set is taken again by the next press.
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
    const unlogged = () => {};
    const PFLAGS = {
      mount: () => pmount, take: pdelete, note: unlogged,
      walk: () => stepIn(pmount, 1),
      missing: "this table-view.js has no delete flags",
      none: "org-delete-property (no row)",
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
      : { body: bodyText(), properties: props(), planning: planning() };
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

    // The renderer virtualizes; the DOM read is the fallback for an older asset.
    const focusedId = () => {
      if (cells()) return table.getSelection().id;
      const tr = /** @type {HTMLElement | null} */
        (document.querySelector("#app .tv-table tbody tr.tv-sel"));
      return tr ? tr.dataset.id : null;
    };
    function pick(list, i) {
      if (!list.length) { append("cmd", "info", "no rows to move through"); return; }
      const id = list[Math.max(0, Math.min(list.length - 1, i))].id;
      table.select(id, column());
    }
    // `selectStep' turns the page at either end, which only the renderer knows about.
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
    // The COMMAND is verbatim — a rebinding config addresses a function by this string.
    const pager = () => can(table, "nextPage") && can(table, "pageInfo");
    const pageNow = () => (pager() ? table.pageInfo().page : 1);
    const sorts = () => can(table, "sortPromote");
    function turnPage(b, step) {
      if (!pager()) { said(b, "this table-view.js has no pager"); return; }
      if (step > 0) table.nextPage(); else table.previousPage();
      const at = table.pageInfo();
      said(b, `page ${at.page}/${at.pages}`);
    }
    // A turn lands the cursor at the end it ARRIVES at, so each climb re-selects.
    function endStop(b, last) {
      const list = visible();
      if (!list.length) { append("cmd", "info", "no rows to move through"); return; }
      const end = (rows) => rows[last ? rows.length - 1 : 0].id;
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
    function moveCol(b, step) {
      if (!cells()) { said(b, "this table-view.js has no cell selection"); return; }
      const at = column(), want = at === null ? 0 : at + step;
      // Out of range on purpose: the renderer nulls it and gives the whole-row look.
      const id = focusedId();
      if (!id || !table.select(id, want)) { said(b, "no row"); return; }
      const now = column();
      said(b, now === null ? "row mode" : (cols[now].header || cols[now].key));
    }
    const marking = () => can(table, "toggleMark");
    const flagging = () => flagsOn(table);
    const isFlagged = (id) => flagging() && table.getFlagged().indexOf(id) !== -1;
    const isMarked = (id) => marking() && table.getMarked().indexOf(id) !== -1;
    const titleOf = (id) => {
      const cell = (rowOf(id).cells || {}).title;
      const shown = typeof TableView.displayText === "function"
        ? TableView.displayText(cell) : String(cell || "");
      return shown || id;
    };
    const noted = (id, what) =>
      append("cmd", "info", `headline ${JSON.stringify(titleOf(id))} ${what}`);
    function mark(b, toggling) {
      if (!marking()) { said(b, "this table-view.js has no marks"); return; }
      const id = focusedId();
      if (!id) { said(b, "no row"); return; }
      // `u' takes the archive FLAG off first — it is the one that would write a file.
      if (!toggling && isFlagged(id))
        { flagKey("u", XFLAGS(b), (what) => said(b, what)); return; }
      // `u' flips, then puts back anything it just laid down.
      let on = table.toggleMark(id);
      if (on && !toggling) on = table.toggleMark(id);
      said(b, `${on ? "marked" : "unmarked"} · ${table.markedCount()}`);
      move(1);
    }
    const targets = () => {
      const marked = marking() ? table.getMarked() : [];
      if (marked.length) return marked;
      const id = focusedId();
      return id ? [id] : [];
    };
    const postCommand = (body) => postJSON("/command", body).then(unwrap);
    const askFailed = (mine, name) => (e) => {
      if (prompting === mine) unask();
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
