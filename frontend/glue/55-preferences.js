    // Browser and native-window preferences, including their catalogue rows.
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

    /** @param {Omit<SettingDescriptor, "area" | "state"> &
     *  {state?: () => SettingState}} descriptor @returns {SettingDescriptor} */
    const localSetting = (descriptor) => settingDescriptor({
      area: "Interface",
      state: () => descriptor.commit ? "saved" : "read-only",
      ...descriptor,
    });
    const Preferences = {
      /** @param {ConfigResponse} config @returns {SettingDescriptor[]} */
      settings(config) {
        const themeNames = ["auto", ...((config && config.themes) || [])];
        return [
          localSetting({
            id: "local:theme", label: "Theme", appliesTo: "this browser",
            source: "glance-theme", read: () => themed.get(),
            commit: (raw) => {
              const value = String(raw);
              if (!themeNames.includes(value))
                throw new Error(`theme is one of ${themeNames.join(", ")}`);
              setTheme(value);
            },
          }),
          localSetting({
            id: "local:reading-line", label: "Reading line",
            appliesTo: "material document", source: READ.key,
            read: () => `${readingLine()}%`,
            commit: (raw) => {
              const text = String(raw).trim().replace(/%$/, "");
              if (!/^[0-9]+$/.test(text) || +text < READ.min || +text > READ.max)
                throw new Error(`reading line is ${READ.min}–${READ.max}%`);
              setReadingLine(+text);
            },
          }),
          localSetting({
            id: "local:zoom", label: "Zoom", appliesTo: "this window",
            source: ZOOM.key,
            read: () => hosted("zoom") ? `${zoomAt}%` : "browser controlled",
            commit: hosted("zoom") ? (raw) => {
              const text = String(raw).trim().replace(/%$/, "");
              if (!/^[0-9]+$/.test(text))
                throw new Error("zoom is a whole percent");
              wearZoom(+text);
            } : undefined,
          }),
          localSetting({
            id: "local:log-lines", label: "Log panel rows",
            appliesTo: "this browser", source: LOG.key,
            read: () => String(logLines(logPref.get()) || LOG.def),
            commit: (raw) => {
              const count = logLines(raw);
              if (count === null) throw new Error(`log rows is ${LOG.min}–${LOG.max}`);
              logPref.set(count === LOG.def ? "" : String(count));
              setLogLines(count);
            },
          }),
        ];
      },
    };

    function hints() {
      el("kbd").textContent = MAPS.hints
        .map((h) => [h.commands.map((c) => seqOf(c, "table")).filter(Boolean),
                     h.label])
        .filter(([keys]) => keys.length)
        .map(([keys, label]) => `${keys.join("/")} ${label}`)
        .join(" · ");
    }
    hints();
