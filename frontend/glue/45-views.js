    // Main-table navigation and the saved-view settings it owns.
    /** @param {{id: string, query: string}} view @param {string} source
     *  @returns {SettingDescriptor} */
    const viewSetting = (view, source) => settingDescriptor({
      id: `view:${view.id}`,
      label: `${view.id} view`,
      area: "Views",
      appliesTo: "whole tree",
      source,
      read: () => view.query || "",
      state: () => "saved",
      commit: (raw) => {
        const query = String(raw).trim();
        return writeView(view.id, query, (message) => echo(message))
          .then(() => { view.query = query; });
      },
    });
    const Views = {
      /** @param {ConfigResponse} config @param {string} source
       *  @returns {SettingDescriptor[]} */
      settings(config, source) {
        return (config && config.views || []).map((view) => viewSetting(view, source));
      },
    };

    const summons = () => can(table, "openFilter");
    /** Raise the filter box on DOOR; `{narrow: true}' is the filter half alone.
     *  An asset that knows no door opens its one box, which is the whole grammar. */
    const raiseFilter = (door) => {
      if (summons()) { table.openFilter(door); return; }
      const box = filterBox();
      if (box) selectWhole(box);
    };
    // TWO DOORS, ONE QUERY: `/' edits the filter half and `.' the whole
    // expression; the standing sort: and columns: ride a `/' commit along.
    const focusFilter = () => raiseFilter({ narrow: true });
    const focusQuery = () => raiseFilter();
    // The one exception to keyboard-first: a coarse pointer has no `/' to press.
    const coarse = () => typeof matchMedia === "function"
      && matchMedia("(pointer: coarse)").matches;
    el("app").addEventListener("click", (e) => {
      if (!coarse()) return;
      const t = targetOf(e);
      if (!t.closest || !t.closest(".tv-chips") || t.closest(".tv-chip")) return;
      focusFilter();
    });
    let stashed = null;
    function typedFilter() {
      const box = filterBox();
      return box && active() === box ? box.value || "" : null;
    }
    function stash() {
      stashed = {
        sheet: editing && dirty()
          ? { id: editing.id, child: editing.child, raw,
              text: el("mtext").value, props: dprops, plan: dplan,
              at: docCursor().at,
              open: openEditState(), digest: editing.digest }
          : null,
        palette: typedFilter(),
      };
    }
    // THE OPEN BOX, BY NAME: a pair carries BOTH halves, since either alone
    // reopens as a pair the reader never typed.
    function openEditState() {
      if (!sheetOpen()) return null;
      if (dpairing())
        return { box: "dpair", id: edit.row.id, add: true,
                 val: el("dkey").value, val2: el("dval").value };
      return { box: dparaing() ? "dpara" : "dtitle", id: edit.row.id,
               add: !!(dparaing() && edit.row.add),
               val: el(dparaing() ? "dtext" : "dtin").value };
    }
    function restore() {
      const was = stashed;
      stashed = null;
      if (!was) return;
      if (was.palette !== null) {
        // WHAT WAS TYPED COMES BACK, THROUGH THE COMMON DOOR: the stash carries
        // the text and the renderer keeps no door across a remount, so a
        // re-raise is `/'.  `.' reopens the whole one on the same text.
        focusFilter();
        // Assigning fires no `input', so the renderer completes nothing.
        const box = filterBox();
        if (box) { box.value = was.palette; box.focus(); }
      }
      if (was.sheet) reopen(was.sheet);
    }
    // The digest is re-asked for — a remembered one is the silent overwrite.
    function reopen(s) {
      headline(s.id, s.child).then((h) => {
        show(h, s.raw);   // which opens the sheet on the file as it now is
        el("mtext").value = s.text;   // dirty again, against the file now
        if (!s.raw) {
          dsend({ kind: "meta", props: s.props, plan: s.plan });
          docRestore(s.at);
          if (s.open) reopenEdit(s.open);
        }
        if (h.digest !== s.digest) sync("conflict");
      }).catch((e) => append("sync", "error", `sheet restore failed: ${e.message}`));
    }
    function reopenEdit(o) {
      // A PAIR IS DRAWN BEFORE IT IS TYPED, so the row goes back in first and
      // the box over it after — the halves as they stood.
      if (o.box === "dpair") {
        redraftPair();
        openEdit(DPAIR, { id: o.id, add: true });
        el("dkey").value = o.val;
        el("dval").value = o.val2 || "";
        // Assigning fires no `input', so the offers are asked for by hand.
        pairMoved();
        return;
      }
      // AN INSERT holds none of the file's text: reopened as a paragraph, RET would REPLACE it.
      const stop = o.box === "dpara" ? docRowById(o.id) : null;
      const r = o.box !== "dpara" ? { id: o.id, val: o.val }
              : !stop ? null
              : o.add ? { id: stop.id, text: "", add: true }
              : stop;
      if (!r) return;
      if (o.add) redraft(r);
      openEdit(o.box === "dpara" ? DPARA : DTITLE, r);
      el(o.box === "dpara" ? "dtext" : "dtin").value = o.val;
    }
    function remount(after) { leaving = arriving = null; stash(); start(after); }
    // `onclose' goes first, or the reconnect timer opens a second socket.
    function applyView(b, q, landing, sel) {
      said(b, q ? `filter: ${JSON.stringify(q)}` : "filter cleared");
      if (socket) { socket.onclose = null; socket.close(); socket = null; }
      backoff = 1000;
      remember(q);
      remount((total) => { land(sel || null); if (landing) landing(total); });
    }
    function applyDefault(b) {
      const here = { id: focusedId(), col: column() };
      if (crumbing()) table.setCrumbs([]);
      crumbLabels = {};
      crumbSels = [];
      applyView(b, savedQuery("default"), undefined, here);
    }
    function applyNamed(id) {
      const b = { seq: `view:${id}`, command: NAMED_VIEW[id] || `apply-view:${id}` };
      if (id === "default") { applyDefault(b); return; }
      applyView(b, savedQuery(id), (total) => said(b, `${id} · ${rowsWord(total)}`));
    }
    const NAMED_VIEW = { default: "apply-default-filter", agenda: "org-glance-agenda" };
    const PIN = "set-saved-view";
    // `-' IS A FLAG, magit's own shape: armed, a letter puts the BUILT-IN back.
    function askView(byKey, take, back) {
      const q = back || !can(table, "getQuery") ? "" : table.getQuery().trim();
      const mine = ask(back ? "reset · which view" : `pin · ${q || "all rows"}`,
                       (c) => (c.reset ? askView(false, take, !back)
                                       : take(String(c.tag), q)),
                       back ? "a letter resets it · - pins again · / to search · ESC leaves"
                            : "a letter pins it · - resets one · / to search · ESC leaves");
      // The BUTTON has no keydown behind it to spend the guard, nor does the `-'.
      mine.raising = byKey;
      const views = (CFG.views || []).map((v) =>
        ({ label: v.id, hint: savedQuery(v.id) || "all rows", tag: v.id }));
      offer([...views,
             { label: "reset", key: "-", cut: -1, fixed: true, reset: true,
               hint: back ? "on · a letter puts the built-in back"
                          : "off · put a view's built-in back" }]);
    }
    function writeView(id, q, spoke) {
      return getJSON("/config").then((a) => {
        const sys = (a.layers || []).find((l) => !l.tag);
        if (!sys) { spoke("no system layer to pin into"); return; }
        // Through `unwrap' so a refusal THROWS — `postJSON' resolves any status.
        return postJSON("/config",
                        { path: sys.path, digest: sys.digest, views: { [id]: q } })
          .then(unwrap)
          .then(() => (q ? landedView(id, q, false, spoke)
                         : getJSON("/config").then((fresh) =>
                             landedView(id, servedView(fresh, id), true, spoke))));
      });
    }
    function viewLanded(id, q) {
      saved[id] = q;
      if (id === "default" && can(table, "setPinned"))
        table.setPinned(table.getQuery().trim() === q);
    }
    const servedView = (a, id) =>
      String(((a.views || []).find((v) => v.id === id) || {}).query || "").trim();
    function landedView(id, q, back, spoke) {
      viewLanded(id, q);
      spoke(`${id}${back ? " reset" : ""} · ${q || "all rows"}`);
      append("config", "info", back
        ? `${id} view reset to its built-in: ${JSON.stringify(q)}`
        : `${id} view pinned: ${JSON.stringify(q)}`);
    }
    function pinView(b) {
      askView(true, (id, q) =>
        writeView(id, q, (w) => said(b, w)).catch(failed(b, PIN)));
    }
    function pinHere() {
      askView(false, (id, q) =>
        writeView(id, q, (w) => echo(`pin → ${PIN} (${w})`))
          .catch((e) => append("config", "error", `${PIN} failed: ${e.message}`)));
    }
    function relations(b) {
      const id = focusedId();
      if (!id) { said(b, "no row"); return; }
      if (!wants(b, "crumbs", "pushCrumb", "popCrumb", "getCrumbs", "setCrumbs"))
        return;
      const token = refToken(id), name = titleOf(id);
      load(`${asking(token)}&limit=1`).then((a) => {
        if (!a.total) {
          said(b, `no references to ${JSON.stringify(name)}`);
          append("cmd", "info", `no references to headline ${JSON.stringify(name)}`);
          return;
        }
        drill(b, token, name);
      }).catch((e) => {
        if (e.name !== "AbortError") failed(b, "relations")(e);
      });
    }

    function drill(b, token, name) {
        if (query.trim()) {
            const at = cells() ? table.getSelection() : null;
            const n = table.pushCrumb({ label: hereLabel(), query: query });
            crumbSels[n - 1] = at && at.id ? { id: at.id, col: at.col } : null;
            crumbSels.length = n;
        }
        crumbLabels[token] = `references of «${name}»`;
        applyView(b, token, (total) => said(b, `references of ${JSON.stringify(name)} · ${total}`));
    }
    function landedAgenda(b, total) {
      said(b, `agenda · ${rowsWord(total)}`);
    }
