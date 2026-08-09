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
