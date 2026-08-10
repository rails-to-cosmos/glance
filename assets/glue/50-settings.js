    // The settings sheet: panels, config layers, state hues.  Rules in CLAUDE.md.
    const SECTIONS = [
      { title: "general", parts: ["cgen"] },
      { title: "theme", parts: ["ctheme"],
        enter: () => { if (srows.length) repaintStates(null); } },
      { title: "keywords", parts: ["clayers", "ceff", "cfoot"],
        enter: () => { if (crows[cat]) showLayer(cat); } },
    ];
    const showing = (title) => (SECTIONS[ctab] || {}).title === title;
    const csecs = el("csecs"), ctabs = el("ctabs");
    const cpanes = SECTIONS.map((s) => {
      const sec = part(csecs, "div", "csec");
      for (const id of s.parts) sec.appendChild(el(id));
      return sec;
    });
    const ctabels = SECTIONS.map((s, i) => {
      const b = part(ctabs, "button", "ctab", s.title);
      b.addEventListener("click", () => pickTab(i));
      b.addEventListener("keydown", (e) => {
        const k = keyName(e);
        const step = k === "<right>" ? 1 : k === "<left>" ? -1 : 0;
        if (!step) return;
        e.preventDefault();
        const at = (i + step + SECTIONS.length) % SECTIONS.length;
        pickTab(at);
        ctabels[at].focus();
      });
      return b;
    });
    let ctab = -1;
    function showTab(i) {
      ctab = i;
      cpanes.forEach((p, k) => { p.className = k === i ? "csec on" : "csec"; });
      ctabels.forEach((t, k) => { t.className = k === i ? "ctab on" : "ctab"; });
    }
    // `takeLayer' first: the panel being left may be a view of the same cycle.
    function pickTab(i) {
      takeLayer();
      showTab(i);
      if (SECTIONS[i].enter) SECTIONS[i].enter();
      // THE PANEL IS THE FRAGMENT (`?page=config#theme'), written by the one
      // URL writer so a closed sheet leaves no stale hash behind.
      remembered();
    }
    // TAB walks the panels; its listener registers ahead of the key dispatch.
    function stepTab(step) {
      pickTab((ctab + step + SECTIONS.length) % SECTIONS.length);
      const first = cpanes[ctab].querySelector("input, select, textarea");
      if (first) first.focus(); else ctabels[ctab].focus();
    }
    document.addEventListener("keydown", (e) => {
      if (!settings || momentary()) return;
      const k = keyName(e);
      if (k !== "TAB" && k !== "S-TAB") return;
      e.preventDefault();
      stepTab(k === "TAB" ? 1 : -1);
    });
    showTab(0);
    /** @type {LayerRow[]} */
    let crows = [];
    let settings = false, cat = 0;
    // The panel a booted `#fragment' asked for, spent on the next open.
    let wantPanel = "";
    const configSheet = {
      noteId: "cnote", scope: "config", state: "synced",
      closed: "settings closed — the files are as they were",
      dirty: () => cdirty(),
      flush: () => flushConfig(),
      refresh: () => config().then((b) => {
        for (const r of crows) {
          const fresh = (b.layers || []).find((l) => l.path === r.path);
          if (fresh) r.digest = fresh.digest;
        }
        return true;
      }),
      shut: () => shutSettings(),
    };
    // `typing()' misses the other sheet — a click on its header blurs the textarea.
    function openSettings() {
      if (activeSheet()) return;
      settings = true;
      config().then((b) => {
        if (!settings) return;   // an ESC arrived while the layers were out
        drawLayers(b);
        el("clog").value = logPref.get();
        cnote("synced");
        el("config").className = "on";
        // A URL naming a panel opens on it.
        if (wantPanel) {
          const at = SECTIONS.findIndex((x) => x.title === wantPanel);
          wantPanel = "";
          if (at !== -1) pickTab(at);
        }
        soon(remembered);
        el("ctarget").focus();
      }).catch((e) => {
        settings = false;
        append("config", "error", `settings failed: ${e.message}`);
      });
    }
    const config = () => getJSON("/config");
    function drawLayers(b) {
      crows = (b.layers || []).map(layerRow).sort(byLayer);
      const pick = el("clayer");
      pick.textContent = "";
      crows.forEach((r, i) => {
        const o = part(pick, "option", "", layerName(r));
        o.value = String(i);
      });
      showLayer(0);
      const cap = el("ctarget");
      cap.value = b.capture || "";
      const sys = crows.find((r) => r.tag === null);
      sys.cap = cap; sys.capBase = cap.value;
      const kw = b.keywords || {};
      el("ceff").textContent =
        `${(kw.active || []).join(" ")} | ${(kw.inactive || []).join(" ")}`;
      drawHues(b, kw);
    }
    /**
     * @typedef {object} StateRow
     * @property {string} id
     * @property {LayerRow|null} layer  null where a plain org file declares it.
     * @property {string} state
     * @property {"active"|"inactive"} group
     * @property {boolean} fixed  no layer owns it, so this sheet cannot move it.
     */
    /** @type {Record<string, Record<string, string>>} */
    let hues = {};
    /** @type {StateRow[]} */
    let srows = [];
    let huesBase = "", sseq = 0, smount = null;
    const SCOLS = [ { key: "tag", header: "Tag" }
                  , { key: "state", header: "State" }
                  , { key: "group", header: "Group" }
                  , { key: "colour", header: "Colour" } ];
    function drawHues(b, kw) {
      hues = {};
      for (const c of b.colors || []) {
        (hues[c.theme] = hues[c.theme] || {})[c.keyword] = c.hue;
      }
      huesBase = JSON.stringify(hues);
      srows = []; sseq = 0;
      const owned = new Set();
      crows.forEach((r) => {
        /** @type {("active"|"inactive")[]} */ (["active", "inactive"]).forEach((group) => {
          (r.kw[group] || []).forEach((state) => {
            owned.add(state);
            srows.push({ id: `S${sseq++}`, layer: r, state, group, fixed: false });
          });
        });
      });
      (kw.active || []).concat(kw.inactive || []).forEach((state) => {
        if (owned.has(state)) return;
        owned.add(state);
        srows.push({ id: `S${sseq++}`, layer: null,
                     state, group: (kw.inactive || []).includes(state)
                                     ? "inactive" : "active", fixed: true });
      });
      repaintStates(srows.length ? srows[0].id : null);
    }
    function statesMounted() {
      if (smount) return smount;
      smount = listing("cstates", SCOLS, "d/D remove · u unflag", "cstates");
      return smount;
    }
    const stateLabel = (r) => (r.layer ? layerName(r.layer) : "file");
    const srowsOf = () => srows.map((r) => ({
      id: r.id,
      cells: { tag: stateLabel(r), state: r.state, group: r.group,
               colour: (hues[hueTheme()] || {})[r.state] || "" },
    }));
    function repaintStates(at) {
      const m = statesMounted();
      m.setRows(srowsOf());
      if (at) m.select(at);
    }
    const hueTheme = () => {
      const want = themed.get();
      if (want !== "auto") return want;
      return matchMedia && matchMedia("(prefers-color-scheme:dark)").matches
        ? "dark" : "light";
    };
    const showHues = () => { if (srows.length) repaintStates(null); };
    const satAt = () => srows.findIndex((r) => r.id === selectedId(smount));
    const SROW = {
      box: "sedit", pane: "cstates", fields: ["sname", "sgroup", "shue"],
      mount: () => smount,
      fill: (r) => {
        el("sname").value = r.state;
        el("sgroup").value = r.group;
        el("shue").value = (hues[hueTheme()] || {})[r.state] || "";
        el("sname").readOnly = r.fixed;
        el("sgroup").readOnly = r.fixed;
      },
      focus: (r) => (r.fixed || r.state ? el("shue") : el("sname")).focus(),
    };
    const sediting = () => !!edit && edit.o === SROW;
    function commitState() {
      const r = edit.row, was = r.state;
      if (!r.fixed) {
        const name = el("sname").value.trim();
        const group = el("sgroup").value.trim().toLowerCase() === "inactive"
                        ? "inactive" : "active";
        r.layer.kw.active = r.layer.kw.active.filter((k) => k !== was);
        r.layer.kw.inactive = r.layer.kw.inactive.filter((k) => k !== was);
        r.state = name; r.group = group;
        if (name) r.layer.kw[group].push(name);
        writeCycle(r.layer);
      }
      const at = (hues[hueTheme()] = hues[hueTheme()] || {});
      const hue = el("shue").value.trim();
      delete at[was];
      if (hue && r.state) at[r.state] = hue;
      shutEdit(SROW);
      repaintStates(r.id);
    }
    function addState() {
      const at = satAt();
      const host = (at !== -1 && srows[at].layer) || crows.find((r) => !r.tag);
      if (!host) { append("config", "warn", "no config layer to add a state to"); return; }
      /** @type {StateRow} */
      const r = { id: `S${sseq++}`, layer: host, state: "", group: "active",
                  fixed: false };
      srows.splice(at === -1 ? srows.length : at + 1, 0, r);
      repaintStates(r.id);
      openEdit(SROW, r);
    }
    function sdelete(ids, how) {
      const gone = new Set(ids);
      const named = srows.filter((r) => gone.has(r.id));
      const stuck = named.filter((r) => r.fixed);
      for (const r of named) {
        if (r.fixed) continue;
        r.layer.kw.active = r.layer.kw.active.filter((k) => k !== r.state);
        r.layer.kw.inactive = r.layer.kw.inactive.filter((k) => k !== r.state);
        writeCycle(r.layer);
      }
      srows = srows.filter((r) => r.fixed || !gone.has(r.id));
      repaintStates(null);
      const held = stuck.map((r) => r.state).join(", ");
      echo(`D → org-todo-remove-state (${how(ids.length - stuck.length)}`
             + `${held ? ` · ${held} is a file's own` : ""})`);
    }
    const SFLAGS = {
      mount: () => smount, take: sdelete, note: unlogged,
      walk: () => stepIn(smount, 1),
      missing: lacks("delete flags"),
      none: "org-todo-remove-state (no row)",
      unflag: "delete-unflag (flag cleared)",
      flag: "delete-flag (d again removes)",
      at: () => { const i = satAt(); return i === -1 ? null : srows[i].id; },
    };
    document.addEventListener("keydown", (e) => {
      if (!settings || momentary() || !smount || !showing("theme")) return;
      const k = keyName(e);
      if (sediting()) {
        if (k === "TAB" || k === "S-TAB") { e.preventDefault(); hop(); }
        else if (k === "RET" && !repeating(e)) { e.preventDefault(); commitState(); }
        return;
      }
      if (k === "RET") {
        const at = satAt();
        if (at !== -1) { e.preventDefault(); openEdit(SROW, srows[at]); }
        return;
      }
      if (k === "+") { e.preventDefault(); addState(); return; }
      if (flagPress(k, e, SFLAGS)) { e.preventDefault(); return; }
      const step = k === "n" || k === "j" || k === "<down>" ? 1
                 : k === "p" || k === "k" || k === "<up>" ? -1 : 0;
      if (step) { e.preventDefault(); stepIn(smount, step); }
    });
    const hueList = () =>
      Object.keys(hues).flatMap((theme) =>
        Object.keys(hues[theme]).map((keyword) =>
          ({ theme, keyword, hue: hues[theme][keyword] })));

    /**
     * @typedef {object} LayerRow
     * @property {string} path       the file, and the write's address.
     * @property {string|null} tag   the tag it configures; null is `system.org`.
     * @property {string} digest     the pin a write to it is checked against.
     * @property {string} base       its `#+TODO:` lines as served.
     * @property {string} text       as this sheet holds them now.
     * @property {string} err        what the server last said about a write.
     * @property {string} tpl        its capture template as served.
     * @property {string} tplBase
     * @property {HTMLInputElement|null} cap  the capture-target field, system only.
     * @property {string|null} capBase
     * @property {{active: string[], inactive: string[]}} kw  the same lines PARSED.
     */
    /**
     * @param {any} layer  one entry of `GET /config`'s `layers`.
     * @returns {LayerRow}
     */
    const layerRow = (layer) => ({
      path: layer.path, tag: layer.tag, digest: layer.digest,
      base: (layer.lines || []).join("\n"),
      text: (layer.lines || []).join("\n"), err: "",
      tpl: layer.template || "", tplBase: layer.template || "",
      cap: null, capBase: null,
      kw: { active: ((layer.keywords || {}).active || []).slice(),
            inactive: ((layer.keywords || {}).inactive || []).slice() },
    });
    function writeCycle(r) {
      const act = r.kw.active, done = r.kw.inactive;
      r.text = act.length || done.length
        ? `#+TODO: ${act.join(" ")}${done.length ? ` | ${done.join(" ")}` : ""}`
        : "";
    }
    const byLayer = (a, b) => (a.tag === null ? 0 : 1) - (b.tag === null ? 0 : 1)
      || String(a.tag).localeCompare(String(b.tag));
    const layerName = (r) => (r.tag ? `tag:${r.tag}` : "system");
    // Read back only while its panel shows: the states table writes the same `text'.
    function takeLayer() {
      if (!crows[cat] || !showing("keywords")) return;
      crows[cat].text = el("ctext").value;
      crows[cat].tpl = el("ctpl").value;
    }
    function showAround() {
      const r = crows[cat];
      el("clab").textContent = r ? `${layerName(r)} · ${r.path}`
        + (r.digest ? "" : " · not created yet") : "";
      el("clerr").textContent = r ? r.err : "";
    }
    function showLayer(i) {
      cat = Math.max(0, Math.min(i, crows.length - 1));
      el("clayer").value = String(cat);
      el("ctext").value = crows[cat] ? crows[cat].text : "";
      el("ctpl").value = crows[cat] ? crows[cat].tpl : "";
      showAround();
    }
    el("clayer").addEventListener("change", (e) => {
      takeLayer();
      showLayer(Number(targetOf(e).value));
    });
    el("ctpl").addEventListener("keydown", (e) => {
      if (keyName(e) !== "%") return;
      e.preventDefault();
      const box = el("ctpl"), at = box.selectionStart, to = box.selectionEnd;
      askFrom("capture template · which code",
              CODES.map((c) => ({ label: c.code, hint: c.means, tag: c.code })),
              "RET writes it · C-n/C-p walks · ESC leaves",
              (c) => insertCode(at, to, String(c.tag || "")));
    });
    // The palette blurred the box on its way up, so the caret is put back by hand.
    function insertCode(at, to, code) {
      const box = el("ctpl"), text = box.value;
      box.value = text.slice(0, at) + code + text.slice(to);
      box.focus();
      box.setSelectionRange(at + code.length, at + code.length);
      takeLayer();
    }
    const cnote = (next, message) => note(configSheet, next, message);
    const cdirty = () => (takeLayer(), crows.some(cmoved));
    const cmoved = (r) => r.text !== r.base || r.tpl !== r.tplBase
      || (r.tag === null && huesMoved())
      || (r.cap !== null && r.cap.value !== r.capBase);
    const huesMoved = () => JSON.stringify(hues) !== huesBase;
    // A pin landed: the live view moves, and the badge with it where the
    // DEFAULT is what moved — the renderer's badge says "this is the default".
    function viewLanded(id, q) {
      saved[id] = q;
      if (id === "default" && can(table, "setPinned"))
        table.setPinned(table.getQuery().trim() === q);
    }
    async function flushConfig() {
      takeLayer();
      cnote("syncing");
      let ok = true, clashed = false, landed = -1;
      for (const r of crows) {
        if (!cmoved(r)) { r.err = ""; continue; }
        // Snapshotted before the await: a keystroke landing mid-write stays dirty.
        const sent = r.text, tpl = r.tpl;
        const colors = r.tag === null && huesMoved() ? hueList() : null;
        const cap = r.cap && r.cap.value;
        const a = await postJSON("/config",
          { path: r.path, lines: sent.split("\n"),
            // Named only where it moved: always sending it hits the one-top-entry wall.
            ...(tpl !== r.tplBase ? { template: tpl } : {}),
            ...(colors ? { colors } : {}),
            ...(r.cap ? { capture: cap } : {}),
            digest: r.digest }).then(outcome)
          .catch((e) => ({ status: 0, body: { error: e.message } }));
        if (a.status === 200) {
          r.digest = a.body.digest; r.base = sent; r.tplBase = tpl; r.err = "";
          if (colors) huesBase = JSON.stringify(hues);
          if (r.cap) r.capBase = cap;
        } else {
          ok = false;
          if (a.status === 409) clashed = true;
          r.err = a.body.error || `sync failed (${a.status})`;
          if (landed === -1) landed = crows.indexOf(r);
          append("config", "error", `${layerName(r)} · ${r.path}: ${r.err}`);
        }
      }
      // The box stands where nothing was refused: `C-x C-s' syncs mid-edit.
      if (landed === -1) showAround();
      else { takeLayer(); showLayer(landed); }
      cnote(ok ? "synced" : clashed ? "conflict" : "error");
      return ok;
    }
    function shutSettings() {
      el("config").className = ""; settings = false; crows = []; cat = 0;
      soon(remembered);
      configSheet.state = "synced";
      if (typing()) active().blur();
    }
    const summons = () => can(table, "openFilter");
    const focusFilter = () => {
      if (summons()) { table.openFilter(); return; }
      const box = filterBox();
      if (box) { box.focus(); box.select(); }
    };
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
        // A pristine sheet survives the remount: it is a sibling of `#app'.
        sheet: editing && dirty()
          ? { id: editing.id, child: editing.child, raw,
              text: el("mtext").value, props: props(), plan: planning(),
              at: docCursor().at, col: docCursor().col,
              open: openEditState(), digest: editing.digest }
          : null,
        palette: typedFilter(),
      };
    }
    function openEditState() {
      if (!docOpen()) return null;
      return { box: dparaing() ? "dpara" : "dtitle", id: edit.row.id,
               val: el(dparaing() ? "dtext" : "dtin").value };
    }
    function restore() {
      const was = stashed;
      stashed = null;
      if (!was) return;
      if (was.palette !== null) {
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
          drawProps(s.props, s.plan);
          docRestore(s.at, s.col);
          if (s.open) reopenEdit(s.open);
        }
        if (h.digest !== s.digest) sync("conflict");
      }).catch((e) => append("sync", "error", `sheet restore failed: ${e.message}`));
    }
    function reopenEdit(o) {
      const r = o.box === "dpara" ? docRowById(o.id)
                                  : { id: o.id, val: o.val };
      if (!r) return;
      openEdit(o.box === "dpara" ? DPARA : DTITLE, r);
      el(o.box === "dpara" ? "dtext" : "dtin").value = o.val;
    }
    // Both anchors belong to the view being thrown away, so they go with it.
    function remount(after) { leaving = arriving = null; stash(); start(after); }
    // `onclose' goes first, or the reconnect timer opens a second socket.
    function applyView(b, q, landing, sel) {
      said(b, q ? `filter: ${JSON.stringify(q)}` : "filter cleared");
      if (socket) { socket.onclose = null; socket.close(); socket = null; }
      backoff = 1000;
      remember(q);
      remount((total) => { land(sel || null); if (landing) landing(total); });
    }
    // SAVE-EXCURSION: going home is a view change rather than a place change, so
    // point stays on the row it was on wherever the default still holds it.
    // `land' is the whole rule — the row named, else row one, else an empty
    // table has nowhere to land at all.
    function applyDefault(b) {
      const here = { id: focusedId(), col: column() };
      if (crumbing()) table.setCrumbs([]);
      crumbLabels = {};
      crumbSels = [];
      applyView(b, savedQuery("default"), undefined, here);
    }
    /**
     * THE VIEW A QUERY NAMED, applied by the door its key uses: `default' is
     * home and throws the crumbs away, and every other view is one application.
     * The registry is the server's, so a view it grows is applicable here with
     * nothing named on this page.
     */
    function applyNamed(id) {
      // The TOKEN is what asked, so the echo names it where a key would be.
      const b = { seq: `view:${id}`, command: NAMED_VIEW[id] || `apply-view:${id}` };
      if (id === "default") { applyDefault(b); return; }
      applyView(b, savedQuery(id), (total) => said(b, `${id} · ${rowsWord(total)}`));
    }
    const NAMED_VIEW = { default: "apply-default-filter", agenda: "org-glance-agenda" };
    const PIN = "set-saved-view";
    // THE PIN ASKS WHICH SAVED VIEW the applied query becomes.  The list is the
    // registry's own, off the boot blob, so the palette goes up filled and a
    // view the server grows is offered with nothing here to name it.
    //
    // `-' IS A FLAG, magit's own shape: it toggles RESET on and off over the
    // same list, and with it armed a letter puts that view's BUILT-IN back
    // rather than pinning.  Toggling re-raises, which is how the flag reaches
    // the head, the foot and the rung's own line; a commit closes the palette,
    // so the flag never outlives the question it was set on.
    function askView(byKey, take, back) {
      const q = back || !can(table, "getQuery") ? "" : table.getQuery().trim();
      const mine = ask(back ? "reset · which view" : `pin · ${q || "all rows"}`,
                       (c) => (c.reset ? askView(false, take, !back)
                                       : take(String(c.tag), q)),
                       back ? "a letter resets it · - pins again · / to search · ESC leaves"
                            : "a letter pins it · - resets one · / to search · ESC leaves");
      // The BUTTON raises it with no keydown behind it to spend the guard, and
      // so does the `-' that re-raises it: that press is already answered.
      mine.raising = byKey;
      const views = (CFG.views || []).map((v) =>
        ({ label: v.id, hint: savedQuery(v.id) || "all rows", tag: v.id }));
      offer(views.concat([{ label: "reset", key: "-", cut: -1, fixed: true, reset: true,
                            hint: back ? "on · a letter puts the built-in back"
                                       : "off · put a view's built-in back" }]));
    }
    // ONE WRITE for both halves: `views' names ONE view, so the others' lines
    // stay where they are, and an EMPTY query TAKES THE LINE OFF — which is how
    // a view goes back to its built-in.  What that built-in IS is the server's,
    // so that half re-reads rather than guessing.
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
    // The chip strip's button, which no keymap row fired: it spells the command
    // by hand, the way every other buttoned door does.
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

    const MAPS = JSON.parse(el("keys").textContent);
    const pref = (key, def) => ({
      get() { try { return localStorage.getItem(key) || def; }
              catch (e) { return def; } },
      set(v) {
        try { if (v) localStorage.setItem(key, v);
              else localStorage.removeItem(key); } catch (e) { /* denied */ }
      },
    });
    const themed = pref("glance-theme", "auto");
    function setTheme(name) {
      if (name === "auto") delete document.documentElement.dataset.theme;
      else document.documentElement.dataset.theme = name;
      themed.set(name);
      el("themesel").value = name;
    }
    setTheme(themed.get());
    el("themesel").addEventListener("change", (e) => {
      setTheme(targetOf(e).value);
      if (settings) showHues();
      echo(`theme: ${targetOf(e).value}`);
    });
    const LOG = CFG.log;
    const logLines = (text) => {
      const t = String(text).trim();
      if (!t) return LOG.def;
      return /^[0-9]+$/.test(t) && +t >= LOG.min && +t <= LOG.max ? +t : null;
    };
    const logPref = pref(LOG.key, "");
    const setLogLines = (n) =>
      el("log").style.setProperty("--g-logn", String(n));
    setLogLines(logLines(logPref.get()) || LOG.def);
    el("clog").addEventListener("input", (e) => {
      const n = logLines(targetOf(e).value);
      if (n === null) return;
      logPref.set(String(targetOf(e).value).trim());
      setLogLines(n);
      echo(`log: ${n} lines`);
    });

    function hints() {
      const seq = (command) => {
        const b = MAPS.rows.find((x) => x.command === command && x.scope === "table");
        return b && b.handler ? b.seq : null;   // a staged row is no offer
      };
      el("kbd").textContent = MAPS.hints
        .map((h) => [h.commands.map(seq).filter(Boolean), h.label])
        .filter(([keys]) => keys.length)
        .map(([keys, label]) => `${keys.join("/")} ${label}`)
        .join(" · ");
    }
    hints();
