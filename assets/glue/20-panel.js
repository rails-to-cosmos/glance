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
      at: () => (drows[dat] ? drows[dat].id : null),
    };
    const keySaid = (k) => (what) => echo(`${k} → ${what}`);
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
      shutEdit(DTITLE); shutEdit(DPARA); shutEdit(PROW);
      drows = []; dlines = []; dflags.clear(); dcursor = null;
      dlinks = [];
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
    const backdrops = [["links", shutLinks], ["tags", shutTags]];
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
    const visible = () => (table ? table.getVisible() : []);
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
    const said = (b, what) =>
      echo(`${b.seq} → ${b.command}${what ? ` (${what})` : ""}`);
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
    const cells = () => can(table, "getSelection");
    const column = () => (cells() ? table.getSelection().col : null);
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
    const rowOf = (id) => visible().concat(all).find((r) => r.id === id) || {};
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
    const failed = (b, name) => (e) => {
      said(b, e.message);
      append("cmd", "error", `${name} failed: ${e.message}`);
    };
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
    const PRIORITY_RING = [null, "C", "B", "A"];
    const cycled = (now, step) => {
      const at = PRIORITY_RING.indexOf(now || null);
      const n = PRIORITY_RING.length;
      return PRIORITY_RING[((at === -1 ? 0 : at) + (step > 0 ? 1 : n - 1)) % n];
    };
    // `priorityLetter' on the page's side of the wire.
    const priorityIn = (cell) => {
      const t = String(cell || "").trim();
      const m = /^\[#(.)\]$/.exec(t);
      return m ? m[1].toUpperCase() : (t ? t.toUpperCase() : null);
    };
    const priorityOf = (id) => priorityIn((rowOf(id).cells || {}).priority);
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
    let capping = null;   // the capture form's state while it is up
    const capUp = () => !!capping;
    function shutCapture() {
      capping = null;
      el("kfields").textContent = "";
      el("klist").textContent = "";
      el("ktag").value = ""; el("ktext").value = "";
      el("capture").className = "";
      const held = active();
      if (held && held.blur) held.blur();
    }
