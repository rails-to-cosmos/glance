    // Settings is a route over the same table-view used by the main catalogue.
    /** @type {LayerRow[]} */
    let crows = [];
    let configData = null, settingsTable = null;
    /** @type {Map<string, SettingState>} */
    const settingPhases = new Map();
    /** @type {Map<string, SettingDescriptor>} */
    let settingsById = new Map();
    /** @type {SaveSession} */
    const configSession = {
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
      shut: () => Pages.finish(settingsRoute),
    };

    const encodedLines = (text) => String(text || "")
      .replace(/\\/g, "\\\\").replace(/\n/g, "\\n");
    const decodedLines = (text) => String(text || "")
      .replace(/\\(n|\\)/g, (_m, c) => c === "n" ? "\n" : "\\");
    const systemLayer = () => crows.find((r) => r.tag === null) || null;
    const settingState = (r) => r.err
      ? (configSession.state === "conflict" ? "conflict" : "error")
      : cmoved(r) ? "changed" : "saved";
    const settingsEditing = () => !!(settingsTable && settingsTable.getEditing
      && settingsTable.getEditing());
    const cancelSettingsEdit = () => {
      if (settingsTable && settingsTable.closeEditor) settingsTable.closeEditor();
    };
    const settingsUp = () => Pages.current() === settingsRoute;

    /** @type {PageRoute} */
    const settingsRoute = {
      name: "config", address: "⌂ → settings", session: configSession,
      enter: enterSettings, leave: resetSettings,
      editing: settingsEditing, cancelEdit: cancelSettingsEdit,
      narrowed: () => narrowed(settingsTable),
      widen: () => widen(settingsTable, "ESC"),
    };

    /** @param {Omit<SettingDescriptor, "source" | "state"> &
     *  {layer: LayerRow}} descriptor @returns {SettingDescriptor} */
    const layerSetting = (descriptor) => settingDescriptor({
      ...descriptor,
      source: descriptor.layer.path,
      state: () => settingState(descriptor.layer),
    });
    /** @param {string} theme @param {string} keyword @param {string} source
     *  @param {string} was @returns {SettingDescriptor} */
    const hueSetting = (theme, keyword, source, was) => settingDescriptor({
      id: `hue:${theme}:${keyword}`,
      label: `${keyword} hue`,
      area: "Colours",
      appliesTo: `${theme} · ${keyword}`,
      source,
      read: () => (hues[theme] || {})[keyword] || "",
      state: () => ((hues[theme] || {})[keyword] || "") === was ? "saved" : "changed",
      commit: (raw) => {
        const at = (hues[theme] = hues[theme] || {});
        const value = String(raw).trim();
        if (value) at[keyword] = value; else delete at[keyword];
      },
    });
    /** @param {Omit<SettingDescriptor, "state" | "commit">} descriptor
     *  @returns {SettingDescriptor} */
    const resolvedSetting = (descriptor) => settingDescriptor({
      ...descriptor,
      state: () => "read-only",
    });
    const Config = {
      /** @returns {SettingDescriptor[]} */
      settings() {
        /** @type {SettingDescriptor[]} */ const descriptors = [];
        const sys = systemLayer();
        for (const layer of crows) {
          const prefix = layer.tag ? `tag:${layer.tag}` : "system";
          descriptors.push(
            layerSetting({
              id: `cycle:${layer.path}`, label: `${prefix} TODO cycle`,
              area: "Keywords", appliesTo: layer.tag ? `tag:${layer.tag}` : "whole tree",
              layer, read: () => encodedLines(layer.text),
              commit: (raw) => { layer.text = decodedLines(raw); },
            }),
            layerSetting({
              id: `template:${layer.path}`, label: `${prefix} capture template`,
              area: "Capture",
              appliesTo: layer.tag ? `captures tagged ${layer.tag}` : "capture fallback",
              layer, read: () => encodedLines(layer.tpl),
              commit: (raw) => { layer.tpl = decodedLines(raw); },
            }),
          );
        }

        const words = new Set(knownStates);
        for (const theme of Object.keys(hues))
          for (const keyword of Object.keys(hues[theme])) words.add(keyword);
        const before = (() => { try { return JSON.parse(huesBase || "{}"); }
                                catch (_e) { return {}; } })();
        for (const theme of ["light", "dark"])
          for (const keyword of [...words].sort())
            descriptors.push(hueSetting(
              theme, keyword, sys ? sys.path : "system.org",
              (before[theme] || {})[keyword] || ""));

        const kw = (configData && configData.keywords) || {};
        descriptors.push(resolvedSetting({
          id: "effective:keywords", label: "Effective keywords", area: "Keywords",
          appliesTo: "every parsed file", source: "resolved union",
          read: () => `${(kw.active || []).join(" ")} | ${(kw.inactive || []).join(" ")}`,
        }));
        return descriptors;
      },
    };

    /** @returns {SettingDescriptor[]} */
    function settingsDescriptors() {
      const source = systemLayer();
      return Preferences.settings(configData)
        .concat(Views.settings(configData, source ? source.path : "system.org"))
        .concat(Config.settings());
    }

    const SETTINGS_COLUMNS = [
      { key: "setting", header: "Setting", sortable: true },
      { key: "value", header: "Value", sortable: true, editable: true },
      { key: "area", header: "Area", sortable: true },
      { key: "applies", header: "Applies to", sortable: true },
      { key: "source", header: "Source", sortable: true },
      { key: "state", header: "State", sortable: true },
    ];
    const settingsView = () => {
      const descriptors = settingsDescriptors();
      settingsById = new Map(descriptors.map((descriptor) => [descriptor.id, descriptor]));
      return {
        title: "settings", columns: SETTINGS_COLUMNS,
        rows: descriptors.map((descriptor) => {
          const row = settingRow(descriptor);
          const phase = settingPhases.get(descriptor.id);
          if (phase) row.cells.state = phase;
          return row;
        }),
      };
    };
    function repaintSettings(id) {
      if (!settingsTable) return;
      const selected = id || selectedId(settingsTable);
      settingsTable.setView(settingsView());
      cols = SETTINGS_COLUMNS;
      if (selected) settingsTable.select(selected);
    }
    function settingEdited(id, col, value, kind) {
      if (kind !== "cell" || col !== 1 || !id) return;
      const descriptor = settingsById.get(id);
      if (!descriptor || !descriptor.commit) {
        repaintSettings(id);
        echo(`${descriptor ? descriptor.label : "setting"} is read-only`);
        return;
      }
      try {
        const work = descriptor.commit(value);
        repaintSettings(id);
        echo(`${descriptor.label}: changed`);
        if (work !== undefined) {
          settingPhases.set(id, "syncing");
          repaintSettings(id);
          Promise.resolve(work).then(() => {
            settingPhases.delete(id);
            repaintSettings(id);
            echo(`${descriptor.label}: saved`);
          })
            .catch((e) => {
              settingPhases.set(id, "error");
              repaintSettings(id);
              append("config", "error", `${descriptor.label}: ${e.message}`);
            });
        }
      } catch (e) {
        const message = e instanceof Error ? e.message : String(e);
        repaintSettings(id);
        append("config", "warn", `${descriptor.label}: ${message}`);
        echo(`${descriptor.label}: ${message}`);
      }
    }
    function mountSettings() {
      settingsTable = Pages.mount(settingsRoute, settingsView(), {
        filterDock: "strip", pageSize: PAGE, actionHints: false,
        onEdit: settingEdited,
      });
      cols = SETTINGS_COLUMNS;
      const first = table.getVisible()[0];
      if (first) table.select(first.id);
    }
    function openSetting() {
      const id = selectedId(settingsTable), descriptor = id && settingsById.get(id);
      if (!descriptor) return;
      if (!descriptor.commit) { echo(`${descriptor.label} is read-only`); return; }
      settingsTable.editCell(id, 1);
    }
    onKeys(() => settingsUp() && !momentary(), (k, e) => {
      if (narrowTyping(settingsTable)) {
        if (narrowPress(k, settingsTable)) e.preventDefault();
        return;
      }
      const step = rowStep(k);
      if (step) stepIn(settingsTable, step);
      else if (k === "RET") openSetting();
      else if (k === "/") settingsTable.openFilter({ narrow: true });
      else if (k === "DEL") leaveSession();
      else return;
      e.preventDefault();
    });

    function openSettings() {
      Pages.open(settingsRoute);
    }
    function enterSettings() {
      config().then((b) => {
        if (!settingsUp()) return;   // an ESC arrived while the layers were out
        configData = b;
        drawLayers(b);
        mountSettings();
        cnote("synced");
        soon(remembered);
      }).catch((e) => {
        Pages.fail(settingsRoute);
        append("config", "error", `settings failed: ${e.message}`);
      });
    }
    /** @returns {Promise<ConfigResponse>} */
    const config = () => getJSON("/config");
    /** @param {ConfigResponse} b */
    function drawLayers(b) {
      crows = (b.layers || []).map(layerRow).sort(byLayer);
      drawHues(b, b.keywords);
    }
    /** @type {Record<string, Record<string, string>>} */
    let hues = {};
    let knownStates = [], huesBase = "";
    /** @param {ConfigResponse} b @param {ConfigKeywords} kw */
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
     * @param {ConfigLayer} layer  one entry of `GET /config`'s `layers`.
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
      note(configSession, next, message);
      if (settingsTable) repaintSettings();
    };
    const cdirty = () => crows.some(cmoved);
    const cmoved = (r) => r.text !== r.base || cfmoved(r).length > 0;
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
    /** @param {ConfigResponse} cfg */
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
    /** @param {ConfigResponse} cfg */
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
    /** @param {ConfigResponse} cfg */
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

    function resetSettings() {
      const held = active();
      if (held) held.blur();
      settingsTable = null;
      configData = null; crows = [];
      settingsById.clear(); settingPhases.clear();
      configSession.state = "synced";
    }
