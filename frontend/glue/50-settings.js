    // Settings is a route over the same table-view used by the main catalogue.
    /** @type {LayerRow[]} */
    let crows = [];
    let settings = false;
    let configData = null, settingsTable = null, settingsBack = null;
    const settingStates = new Map();
    let settingModels = new Map();
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

    const encodedLines = (text) => String(text || "")
      .replace(/\\/g, "\\\\").replace(/\n/g, "\\n");
    const decodedLines = (text) => String(text || "")
      .replace(/\\(n|\\)/g, (_m, c) => c === "n" ? "\n" : "\\");
    const systemLayer = () => crows.find((r) => r.tag === null) || null;
    const settingState = (r) => r.err
      ? (configSheet.state === "conflict" ? "conflict" : "error")
      : cmoved(r) ? "changed" : "saved";
    const settingsEditing = () => !!(settingsTable && settingsTable.getEditing
      && settingsTable.getEditing());
    const cancelSettingsEdit = () => {
      if (settingsTable && settingsTable.closeEditor) settingsTable.closeEditor();
    };

    function renderPageCrumbs() {
      const ctl = document.getElementById("gitctl");
      if (!ctl) return;
      const page = ctl.querySelector(".g-page");
      if (page) {
        const routed = !!settingsTable;
        page.textContent = routed ? "main -> settings" : "default";
        if (routed) page.removeAttribute("disabled");
        else page.setAttribute("disabled", "");
        page.setAttribute("title", routed ? "back to the default view" : "");
      }
    }
    renderPageCrumbs();

    function settingsRows() {
      /** @type {any[]} */ const rows = [];
      settingModels = new Map();
      const add = (id, setting, value, area, applies, source, state, write) => {
        settingModels.set(id, { setting, write, readOnly: !write });
        rows.push({ id, cells: { setting, value, area, applies, source, state } });
      };
      const themeNames = ["auto", ...((configData && configData.themes) || [])];
      add("local:theme", "Theme", themed.get(), "Interface", "this browser",
          "glance-theme", "saved", (value) => {
            if (!themeNames.includes(value)) throw new Error(`theme is one of ${themeNames.join(", ")}`);
            setTheme(value);
          });
      add("local:reading-line", "Reading line", `${readingLine()}%`, "Interface",
          "material document", READ.key, "saved", (value) => {
            const text = String(value).trim().replace(/%$/, "");
            if (!/^[0-9]+$/.test(text) || +text < READ.min || +text > READ.max)
              throw new Error(`reading line is ${READ.min}–${READ.max}%`);
            setReadingLine(+text);
          });
      add("local:zoom", "Zoom", hosted("zoom") ? `${zoomAt}%` : "browser controlled",
          "Interface", "this window", ZOOM.key,
          hosted("zoom") ? "saved" : "read-only", hosted("zoom") ? (value) => {
            const text = String(value).trim().replace(/%$/, "");
            if (!/^[0-9]+$/.test(text)) throw new Error("zoom is a whole percent");
            wearZoom(+text);
          } : null);
      add("local:log-lines", "Log panel rows", String(logLines(logPref.get()) || LOG.def),
          "Interface", "this browser", LOG.key, "saved", (value) => {
            const count = logLines(value);
            if (count === null) throw new Error(`log rows is ${LOG.min}–${LOG.max}`);
            logPref.set(count === LOG.def ? "" : String(count));
            setLogLines(count);
          });

      const sys = systemLayer();
      for (const view of (configData && configData.views) || []) {
        const id = `view:${view.id}`;
        add(id, `${view.id} view`, view.query || "", "Views", "whole tree",
            sys ? sys.path : "system.org", settingStates.get(id) || "saved",
            (value) => {
              settingStates.set(id, "syncing");
              repaintSettings(id);
              return writeView(view.id, String(value).trim(), (message) => echo(message))
                .then(() => { view.query = String(value).trim(); settingStates.set(id, "saved"); });
            });
      }

      for (const r of crows) {
        const scope = r.tag ? `tag:${r.tag}` : "whole tree";
        const prefix = r.tag ? `tag:${r.tag}` : "system";
        add(`cycle:${r.path}`, `${prefix} TODO cycle`, encodedLines(r.text), "Keywords",
            scope, r.path, settingState(r), (value) => { r.text = decodedLines(value); });
        add(`template:${r.path}`, `${prefix} capture template`, encodedLines(r.tpl),
            "Capture", r.tag ? `captures tagged ${r.tag}` : "capture fallback",
            r.path, settingState(r), (value) => { r.tpl = decodedLines(value); });
      }

      const words = new Set(knownStates);
      for (const theme of Object.keys(hues))
        for (const keyword of Object.keys(hues[theme])) words.add(keyword);
      const hueBase = (() => { try { return JSON.parse(huesBase || "{}"); }
                               catch (_e) { return {}; } })();
      for (const theme of ["light", "dark"])
        for (const keyword of [...words].sort()) {
          const id = `hue:${theme}:${keyword}`;
          const value = (hues[theme] || {})[keyword] || "";
          const was = (hueBase[theme] || {})[keyword] || "";
          add(id, `${keyword} hue`, value, "Colours", `${theme} · ${keyword}`,
              sys ? sys.path : "system.org", value === was ? "saved" : "changed",
              (next) => {
                const at = (hues[theme] = hues[theme] || {});
                if (String(next).trim()) at[keyword] = String(next).trim();
                else delete at[keyword];
              });
        }

      const kw = (configData && configData.keywords) || {};
      add("effective:keywords", "Effective keywords",
          `${(kw.active || []).join(" ")} | ${(kw.inactive || []).join(" ")}`,
          "Keywords", "every parsed file", "resolved union", "read-only", null);
      return rows;
    }

    const SETTINGS_COLUMNS = [
      { key: "setting", header: "Setting", sortable: true },
      { key: "value", header: "Value", sortable: true, editable: true },
      { key: "area", header: "Area", sortable: true },
      { key: "applies", header: "Applies to", sortable: true },
      { key: "source", header: "Source", sortable: true },
      { key: "state", header: "State", sortable: true },
    ];
    const settingsView = () => ({ title: "settings", columns: SETTINGS_COLUMNS,
                                  rows: settingsRows() });
    function repaintSettings(id) {
      if (!settingsTable) return;
      const selected = id || selectedId(settingsTable);
      settingsTable.setView(settingsView());
      cols = SETTINGS_COLUMNS;
      if (selected) settingsTable.select(selected);
    }
    function settingEdited(id, col, value, kind) {
      if (kind !== "cell" || col !== 1 || !id) return;
      const model = settingModels.get(id);
      if (!model || !model.write) {
        repaintSettings(id);
        echo(`${model ? model.setting : "setting"} is read-only`);
        return;
      }
      try {
        const work = model.write(value);
        repaintSettings(id);
        echo(`${model.setting}: changed`);
        if (work && typeof work.then === "function")
          work.then(() => { repaintSettings(id); echo(`${model.setting}: saved`); })
            .catch((e) => {
              settingStates.set(id, "error");
              repaintSettings(id);
              append("config", "error", `${model.setting}: ${e.message}`);
            });
      } catch (e) {
        const message = e instanceof Error ? e.message : String(e);
        repaintSettings(id);
        append("config", "warn", `${model.setting}: ${message}`);
        echo(`${model.setting}: ${message}`);
      }
    }
    function mountSettings() {
      settingsBack = cells() ? table.getSelection() : null;
      if (socket) { socket.onclose = null; socket.close(); socket = null; }
      if (table && table.destroy) table.destroy();
      table = TableView.mount(el("app"), settingsView(), {
        filterDock: "strip", pageSize: PAGE, actionHints: false,
        onEdit: settingEdited,
      });
      settingsTable = table;
      cols = SETTINGS_COLUMNS;
      const first = table.getVisible()[0];
      if (first) table.select(first.id);
      renderPageCrumbs();
    }
    function openSetting() {
      const id = selectedId(settingsTable), model = id && settingModels.get(id);
      if (!model) return;
      if (!model.write) { echo(`${model.setting} is read-only`); return; }
      settingsTable.editCell(id, 1);
    }
    onKeys(() => settings && !momentary(), (k, e) => {
      if (narrowTyping(settingsTable)) {
        if (narrowPress(k, settingsTable)) e.preventDefault();
        return;
      }
      const step = rowStep(k);
      if (step) stepIn(settingsTable, step);
      else if (k === "RET") openSetting();
      else if (k === "/") settingsTable.openFilter({ narrow: true });
      else if (k === "DEL") leaveSheet();
      else return;
      e.preventDefault();
    });

    function openSettings() {
      if (activeSheet()) return;
      settings = true;
      config().then((b) => {
        if (!settings) return;   // an ESC arrived while the layers were out
        configData = b;
        drawLayers(b);
        mountSettings();
        cnote("synced");
        soon(remembered);
      }).catch((e) => {
        settings = false;
        renderPageCrumbs();
        append("config", "error", `settings failed: ${e.message}`);
      });
    }
    const config = () => getJSON("/config");
    function drawLayers(b) {
      crows = (b.layers || []).map(layerRow).sort(byLayer);
      drawHues(b, b.keywords || {});
    }
    /** @type {Record<string, Record<string, string>>} */
    let hues = {};
    let knownStates = [], huesBase = "";
    function drawHues(b, kw) {
      hues = {};
      for (const c of b.colors || []) {
        (hues[c.theme] = hues[c.theme] || {})[c.keyword] = c.hue;
      }
      huesBase = JSON.stringify(hues);
      const owned = new Set();
      crows.forEach((r) => {
        /** @type {("active"|"inactive")[]} */ (["active", "inactive"]).forEach((group) => {
          (r.kw[group] || []).forEach((state) => owned.add(state));
        });
      });
      (kw.active || []).concat(kw.inactive || []).forEach((state) => owned.add(state));
      knownStates = [...owned];
    }
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
    /**
     * @typedef {object} CField
     * @property {string} key  the field a `POST /config` names it by.
     * @property {(r: LayerRow) => boolean} on  does this layer carry it?
     * @property {(r: LayerRow) => string} now  what it holds, comparable.
     * @property {(r: LayerRow) => string} was  what it was served as.
     * @property {(r: LayerRow) => any} send  the wire value.
     * @property {(r: LayerRow, was: string) => void} kept  take the receipt.
     */
    /** @type {CField[]} */
    const CFIELDS = [
      { key: "template", on: () => true,
        now: (r) => r.tpl, was: (r) => r.tplBase, send: (r) => r.tpl,
        kept: (r, was) => { r.tplBase = was; } },
      { key: "colors", on: (r) => r.tag === null,
        now: () => JSON.stringify(hues), was: () => huesBase,
        send: () => hueList(), kept: (_r, was) => { huesBase = was; } },
    ];
    const cfmoved = (r) => CFIELDS.filter((f) => f.on(r) && f.now(r) !== f.was(r));
    const cnote = (next, message) => {
      note(configSheet, next, message);
      if (settingsTable) repaintSettings();
    };
    const cdirty = () => crows.some(cmoved);
    const cmoved = (r) => r.text !== r.base || cfmoved(r).length > 0;
    function viewLanded(id, q) {
      saved[id] = q;
      if (id === "default" && can(table, "setPinned"))
        table.setPinned(table.getQuery().trim() === q);
    }
    async function flushConfig() {
      cnote("syncing");
      let ok = true, clashed = false, landed = -1;
      for (const r of crows) {
        if (!cmoved(r)) { r.err = ""; continue; }
        // Snapshotted before the await: a keystroke landing mid-write stays dirty.
        const sent = r.text;
        // NAMED ONLY WHERE IT MOVED: always sending the template hits the one-top-entry wall.
        const moved = cfmoved(r).map((f) => ({ f, was: f.now(r), body: f.send(r) }));
        /** @type {Record<string, any>} */
        const body = { path: r.path, lines: sent.split("\n"), digest: r.digest };
        for (const m of moved) body[m.f.key] = m.body;
        const a = await postJSON("/config", body).then(outcome)
          .catch((e) => ({ status: 0, body: { error: e.message } }));
        if (a.status === 200) {
          r.digest = a.body.digest; r.base = sent; r.err = "";
          for (const m of moved) m.f.kept(r, m.was);
        } else {
          ok = false;
          if (a.status === 409) clashed = true;
          r.err = a.body.error || `sync failed (${a.status})`;
          if (landed === -1) landed = crows.indexOf(r);
          append("config", "error", `${layerName(r)} · ${r.path}: ${r.err}`);
        }
      }
      if (landed !== -1) repaintSettings(`cycle:${crows[landed].path}`);
      cnote(ok ? "synced" : clashed ? "conflict" : "error");
      return ok;
    }
    // `+' IN THE STATE PALETTE mints a state the store does not have: DECLARED
    // in a config layer, then set on the rows the palette was raised over.  The
    // namespace says where the declaration goes — `system' the tree, `tag:X' the
    // rows carrying X — and `default' is org's builtin pair, code with no file.
    let minting = null;
    const mintUp = () => !!minting;
    const NFIELDS = ["nspace", "nname", "ngroup", "nlight", "ndark"];

    function shutMint(why) {
      if (!minting) return;
      minting = null;
      el("mint").className = "";
      for (const id of NFIELDS) el(id).blur();
      if (why) append("config", "info", `state: ${why}`);
    }
    // `system', then the tags the applied query names — the rows on screen are the
    // rows that filter chose — then any tag layer the tree already has.  FOLDED,
    // because `tagOf' lowercases a layer's basename into its tag: `Book' and
    // `book' name one layer, and offering both would mint the file twice.
    function mintSpaces(cfg) {
      const held = (cfg.layers || []).map((l) => l.tag).filter(Boolean);
      const named = filteredTags();
      return ["system"]
        .concat([...new Set(named.concat(held).map(foldTag))].map((t) => `tag:${t}`));
    }
    // THE BINDING IS THE PALETTE'S: a failure here is answered where `t' was pressed.
    function openMint() {
      const asking = promptNow();
      if (!asking || !asking.states) return;
      const b = asking.states.b;
      config().then((cfg) => {
        if (promptNow() !== asking) return;   // the palette went while /config was out
        minting = { b, cfg, asking };
        const pick = el("nspace");
        pick.textContent = "";
        const spaces = mintSpaces(cfg);
        for (const name of spaces) part(pick, "option", "", name).value = name;
        pick.value = spaces[1] || "system";   // the filter's own tag, where it named one
        el("nname").value = "";
        el("ngroup").value = "active";
        el("nlight").value = ""; el("ndark").value = "";
        el("mint").className = "on";
        el("nname").focus();
        echo("+ → org-todo-add-state");
      }).catch(failed(b, "config"));
    }
    // The layer the namespace names, MINTED where the tag has no file: an absent
    // layer is a path and an empty digest, which is what a write reads as "create".
    function mintLayer(cfg, space) {
      const layers = cfg.layers || [];
      if (space === "system") return layers.find((l) => !l.tag) || null;
      const tag = foldTag(space.slice(4));
      const held = layers.find((l) => foldTag(l.tag) === tag);
      if (held) return held;
      if (!cfg.tagsDir) return null;
      return { path: `${cfg.tagsDir}/${tag}.org`, tag, digest: "", lines: [],
               keywords: { active: [], inactive: [] }, template: "" };
    }
    // A HUE PER THEME, over the colours already declared: the write replaces the
    // whole `#+GLANCE_STATE_COLORS:' block, so what is kept is sent again.
    function mintHues(cfg, name) {
      const want = [["light", el("nlight").value.trim()], ["dark", el("ndark").value.trim()]]
        .filter(([, hue]) => hue);
      if (!want.length) return null;
      return (cfg.colors || []).filter((c) => c.keyword !== name)
        .concat(want.map(([theme, hue]) => ({ theme, keyword: name, hue })));
    }
    const wroteConfig = (body) =>
      postJSON("/config", body).then(outcome).then((a) => {
        if (a.status !== 200) throw new Error((a.body || {}).error || `sync failed (${a.status})`);
        return a.body;
      });
    // The write nudges a RESEED, so the chain the store answers with lags the file
    // by a settle.  Asked again until it holds the state, then given up on.
    function awaitState(ids, name, tries) {
      return keywordSources(ids).then((answer) => {
        const has = (answer.sources || []).some((s) =>
          (s.active || []).includes(name) || (s.inactive || []).includes(name));
        if (has || tries <= 0) return has;
        return new Promise((go) => setTimeout(go, 120))
          .then(() => awaitState(ids, name, tries - 1));
      });
    }
    function mintState() {
      const { b, cfg, asking } = minting;
      const name = el("nname").value.trim();
      if (!/^[A-Za-z_]+$/.test(name)) {
        append("config", "warn",
               `${name || "a state"} is not a TODO state: a state is letters and _`);
        el("nname").focus();
        return;
      }
      const layer = mintLayer(cfg, el("nspace").value);
      if (!layer) { append("config", "warn", "no config layer to add a state to"); return; }
      const r = layerRow(layer);
      const group = el("ngroup").value === "inactive" ? "inactive" : "active";
      r.kw.active = r.kw.active.filter((k) => k !== name);
      r.kw.inactive = r.kw.inactive.filter((k) => k !== name);
      r.kw[group].push(name);
      writeCycle(r);
      const hues = mintHues(cfg, name);
      /** @type {Record<string, any>} */
      const body = { path: r.path, lines: r.text.split("\n"), digest: r.digest };
      // A COLOUR IS THE SYSTEM LAYER'S; a state minted under a tag moves two files.
      const system = cfg.layers.find((l) => !l.tag);
      const rides = hues && r.tag === null;
      if (rides) body.colors = hues;
      const ids = asking.states.ids;
      shutMint(null);
      wroteConfig(body)
        .then(() => (hues && !rides && system
                       ? wroteConfig({ path: system.path, digest: system.digest, colors: hues })
                       : null))
        .then(() => awaitState(ids, name, 20))
        .then((has) => {
          if (!has) { append("config", "warn", `${name} declared; the store has not reread yet`); return; }
          if (!promptNow()) { said(b, `${name} added`); return; }
          restate().then(() => takeChoice({ keyword: name, label: name }));
        })
        .catch((e) => append("config", "error", `state: ${e.message}`));
    }
    document.addEventListener("keydown", (e) => {
      if (!minting || e.defaultPrevented) return;
      const k = keyName(e);
      if (k === "TAB" || k === "S-TAB") {
        e.preventDefault();
        const at = NFIELDS.findIndex((id) => el(id) === active());
        const step = k === "TAB" ? 1 : NFIELDS.length - 1;
        el(NFIELDS[(at + step + NFIELDS.length) % NFIELDS.length]).focus();
        return;
      }
      if (k === "RET" && !repeating(e)) { e.preventDefault(); mintState(); return; }
      if (k === "ESC") {
        e.preventDefault(); e.stopPropagation();
        shutMint(null);
        echo("ESC → keyboard-quit (no state added)");
      }
    }, true);

    function shutSettings() {
      const mounted = settingsTable;
      const back = settingsBack;
      const held = active();
      if (held) held.blur();
      if (mounted && mounted.destroy) mounted.destroy();
      settingsTable = null; settings = false;
      configData = null; crows = []; settingsBack = null;
      configSheet.state = "synced";
      renderPageCrumbs();
      remembered();
      if (mounted) start(() => land(back));
    }
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
      offer(views.concat([{ label: "reset", key: "-", cut: -1, fixed: true, reset: true,
                            hint: back ? "on · a letter puts the built-in back"
                                       : "off · put a view's built-in back" }]));
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

    const MAPS = JSON.parse(el("keys").textContent);
    /** The SEQUENCE the map spells COMMAND as in SCOPE, or `null' where the row
     * is staged: A ROW WITH NO HANDLER IS NO OFFER, so neither the resident key
     * line nor the zoom row may advertise a key nothing is bound to. */
    const seqOf = (command, scope) => {
      const b = MAPS.rows.find((x) => x.command === command && x.scope === scope);
      return b && b.handler ? b.seq : null;
    };
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
    }
    setTheme(themed.get());
    // THE READING LINE the document pane rests point's row on: a per-machine
    // display preference like the theme, held as a WHOLE PERCENT of the pane's
    // visible height.  BANDED 20-90 -- outside that there is no band above the
    // line for a row to rest in -- and anything else stored falls to the default.
    const READ = { key: "glance-reading-line", def: 60, min: 20, max: 90 };
    const readPref = pref(READ.key, String(READ.def));
    const readingLine = () => {
      const t = String(readPref.get()).trim();
      if (!/^[0-9]+$/.test(t)) return READ.def;
      return clamp(+t, READ.min, READ.max);
    };
    function setReadingLine(pct) {
      readPref.set(String(pct));
    }
    setReadingLine(readingLine());
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

    // THE WINDOW'S ZOOM, a per-machine display preference like the theme, and
    // held here as a WHOLE PERCENT: what the reader is told, what is stored, and
    // what the row shows are one number.  The window wears it as a level.
    const ZOOM = CFG.zoom;
    const zoomPref = pref(ZOOM.key, "");
    const zoomBand = (n) => clamp(Math.round(n), ZOOM.min, ZOOM.max);
    const zoomStored = () => {
      const t = String(zoomPref.get()).trim();
      return /^[0-9]+$/.test(t) ? zoomBand(+t) : ZOOM.def;
    };
    let zoomAt = zoomStored();
    // The POST IS THE WHOLE APPLICATION: this page draws nothing at its own
    // scale.  The settings row is repainted while the catalogue shows it.
    function applyZoom() {
      const door = hosted("zoom");
      if (door) door.postMessage(String(zoomAt / 100));
      if (settingsTable) repaintSettings("local:zoom");
    }
    // A HELD `C-+' REPEATS SOME THIRTY TIMES A SECOND and `localStorage' is
    // synchronous, so the store's write TRAILS the walk while the window's own
    // post stays immediate — what the reader is looking at is the window.  The
    // settle is shorter than any way of closing one.
    const ZOOM_SETTLE = 200;
    let zoomSoon = 0;
    // Blank REMOVES the key, the log height's own reading of "default".
    function keepZoom() {
      clearTimeout(zoomSoon);
      zoomSoon = setTimeout(() => {
        zoomSoon = 0;
        zoomPref.set(zoomAt === ZOOM.def ? "" : String(zoomAt));
      }, ZOOM_SETTLE);
    }
    function wearZoom(pct) {
      zoomAt = zoomBand(pct);
      keepZoom();
      applyZoom();
      return zoomAt;
    }
    const zoomedBy = (step) =>
      wearZoom(step > 0 ? zoomAt * ZOOM.step : zoomAt / ZOOM.step);
    // THE KEYS AS THE MAP SPELLS THEM, the resident key line's own rule.
    // COMPUTED ONCE: `MAPS' is the boot blob and nothing moves it.
    const ZOOM_KEYS =
      ["text-scale-increase", "text-scale-decrease", "text-scale-set"]
        .map((c) => seqOf(c, "window")).filter(Boolean).join(" / ");
    // WORN AT BOOT and only where there is a window to wear it: a browser tab
    // keeps whatever zoom its own reader gave it.  BOOT APPLIES WITHOUT
    // STORING: the band is clamped on every read, so writing the clamp back
    // would buy a write per boot and nothing else.
    if (hosted("zoom")) applyZoom();

    function hints() {
      el("kbd").textContent = MAPS.hints
        .map((h) => [h.commands.map((c) => seqOf(c, "table")).filter(Boolean),
                     h.label])
        .filter(([keys]) => keys.length)
        .map(([keys, label]) => `${keys.join("/")} ${label}`)
        .join(" · ");
    }
    hints();
