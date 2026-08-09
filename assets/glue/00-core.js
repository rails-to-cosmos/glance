// The shell.  Rules and consequences: docs/invariants.md; grammar: SCHEMA.md.

    const CFG = JSON.parse(document.getElementById("cfg").textContent);
    const part = (into, tag, cls, text) => {
      const e = document.createElement(tag);
      e.className = cls;
      if (text !== undefined) e.textContent = text;
      into.appendChild(e);
      return e;
    };
    const LOGCAP = 500;
    let logLast = null;
    function append(scope, sev, message) {
      const box = document.getElementById("log");
      const text = String(message).replace(/[\x00-\x1f]+/g, " ");
      const end = box.scrollTop + box.clientHeight >= box.scrollHeight - 4;
      if (logLast && logLast.scope === scope && logLast.sev === sev
          && logLast.text === text) {
        logLast.count.textContent = `×${(logLast.n += 1)}`;
      } else {
        const line = document.createElement("div");
        line.className = sev;
        part(line, "span", "lt", new Date().toTimeString().slice(0, 8));
        part(line, "span", "lv", sev.toUpperCase());
        part(line, "span", "lc", scope);
        part(line, "span", "lm", text);
        logLast = { scope, sev, text, n: 1, count: part(line, "span", "ln", "") };
        box.appendChild(line);
        while (box.children.length > LOGCAP) box.removeChild(box.children[0]);
      }
      if (end) box.scrollTop = box.scrollHeight;
    }
    /**
     * @returns {HTMLInputElement & HTMLTextAreaElement & HTMLElement}
     */
    const active = () => /** @type {any} */ (document.activeElement);
    /**
     * @param {Event} e
     * @returns {HTMLInputElement & HTMLTextAreaElement & HTMLElement}
     */
    const targetOf = (e) => /** @type {any} */ (e.target);
    /**
     * @param {string} id
     * @returns {HTMLInputElement & HTMLTextAreaElement & HTMLSelectElement}
     */
    const el = (id) =>
      /** @type {any} */ (document.getElementById(id));
    const WASH = { view: 300, socket: 400 };
    const wash = {
      n: { view: 0, socket: 0 }, at: { view: 0, socket: 0 },
      on: { view: false, socket: false },
      // A view fetch STEPS (an abort overlaps its replacement); the socket SETS.
      want(why, count) {
        const was = this.n[why];
        this.n[why] = Math.max(0, count);
        if (this.n[why] === was) return;
        if (this.n[why]) this.arm(why); else this.off(why);
      },
      step(why, by) { this.want(why, this.n[why] + by); },
      arm(why) {
        if (this.on[why] || this.at[why]) return;
        this.at[why] = setTimeout(() => {
          this.at[why] = 0; this.on[why] = true; this.show();
        }, WASH[why]);
      },
      off(why) {
        clearTimeout(this.at[why]); this.at[why] = 0;
        this.on[why] = false; this.show();
      },
      show() {
        document.documentElement.classList.toggle("stale",
          this.on.view || this.on.socket);
      },
    };
    const can = (mount, name) => !!mount && typeof mount[name] === "function";
    const rowStep = (k) => (k === "<down>" || k === "n" || k === "j" ? 1
                          : k === "<up>" || k === "p" || k === "k" ? -1 : 0);
    const stepIn = (mount, step) =>
      can(mount, "selectStep") && mount.selectStep(step);
    const flagsOn = (mount) => can(mount, "flagRow") && can(mount, "getFlagged");
    const selectedId = (mount) =>
      (can(mount, "getSelection") ? (mount.getSelection() || {}).id : null) || null;
    const soon = (fn) =>
      (typeof requestAnimationFrame === "function" ? requestAnimationFrame(fn)
                                                    : setTimeout(fn, 0));
    let table = null, socket = null, backoff = 1000, editing = null;
    let base = "", baseProps = null, raw = false;
    let query = "", inflight = null, requeryAt = 0;
    let leaving = null;
    let arriving = null;
    let etag = null;
    const PAGE = 100;   // rows in the first paint, and rows to a page
    function mount(view) {
      // Before the mount — `chipLabel' can be asked during the first paint.
      const was = bootTrail();
      crumbLabels = was.labels;
      crumbSels = was.sels;
      table = TableView.mount(document.getElementById("app"), view, {
        palette: true,     // the filter is summoned, never resident
        pageSize: PAGE,
        marks: true,       // dired's m/u/U/M, drawn and counted by the renderer
        flagHelp: "d/D archive · u unflag",
        actionHints: false,
        initialQuery: query,
        chipLabel: (tok) => crumbLabels[tok] || null,
        onAction: (command, id) =>
          command === "materialize" ? materialize(id)
                                     : append("cmd", "info", `action: ${command}  id=${id}`),
        onLink: (target) => append("cmd", "info", `link: ${target}`),
        onFilter: filter,   // the server narrows; the renderer shows what it is given
        onPin: () => pinHere(),
        pinned: query.trim() === savedQuery("default"),
      });
      if (query && !holds(query)) showQuery();
      if (crumbing() && was.trail.length) table.setCrumbs(was.trail);
      cols = view.columns || [];
      restore();
    }
    // `no-store' steps around the browser cache, so the 304 arrives as itself.
    function load(params, tag) {
      if (inflight) inflight.abort();
      inflight = new AbortController();
      const init = { signal: inflight.signal };
      if (tag) { init.headers = { "if-none-match": tag }; init.cache = "no-store"; }
      return fetch(`/headlines${params}`, init).then((r) =>
        r.status === 304 ? { view: null, total: 0 }
        : r.ok ? r.json().then((view) => {
            etag = r.headers.get("ETag") || etag;
            return { view, total: +r.headers.get("X-Glance-Total") };
          })
        : r.status === 503 ? r.json().then((b) => { throw Object.assign(new Error("indexing"), { indexing: b }); })
             : r.text().then((t) => { throw new Error(t); }));
    }
    const quiet = (e) => {
      if (e.name !== "AbortError") append("ws", "error", `load failed: ${e.message}`);
    };
    const viewing = (p) => {
      if (!table) return p;
      wash.step("view", 1);
      return p.finally(() => wash.step("view", -1));
    };
    let all = [], cols = [];
    const paint = (a) => {
      const rows = a.view.rows || [];
      table.setRows(rows);
      if (!query) all = rows;
      parity(a.total);
    };
    function arm(total) {
      if (!query || all.length) return;
      load("").then((a) => { all = a.view.rows || []; parity(total); }).catch(quiet);
    }
    function parity(total) {
      if (total !== 0 || !query || !all.length) return;
      if (typeof TableView.parseQuery !== "function") return;
      const keys = cols.map((c) => c.key);
      const loose = TableView.parseQuery(query, keys).filter((t) =>
        t.key === null && !t.quoted && !t.negated && /^[^:=]+[:=]./.test(t.value));
      if (!loose.length) return;
      const wants = loose.map((t) => t.value.slice(t.value.search(/[:=]/) + 1).toLowerCase());
      const text = (r) => keys.map((k) => TableView.displayText((r.cells || {})[k]))
        .join("\x1f").toLowerCase();
      const local = all.filter((r) => wants.every((v) => text(r).includes(v))).length;
      if (!local) return;
      const note = "filter parity divergence — asset/daemon version skew";
      console.warn(note, { query, server: total, local });
      append("filter", "warn", note);
      echo(note);
    }

    const params = () => new URLSearchParams(location.search);
    const urlQuery = () => params().get("q") || "";
    // THE SAVED VIEWS, LIVE, keyed by the registry's own ids: seeded from the
    // boot blob and moved by a pin, so `g' and `a' apply what the last write
    // landed rather than the constant this page booted on.  A view the server
    // grows joins by being in the blob — nothing here names one.
    /** @type {Record<string, string>} */
    const saved = {};
    (CFG.views || []).forEach((v) => { saved[v.id] = String(v.query || "").trim(); });
    const savedQuery = (id) => saved[id] || "";
    const DEFAULT_QUERY = savedQuery("default");
    const bootQuery = () => (params().has("q") ? urlQuery() : DEFAULT_QUERY);
    let crumbLabels = {};
    const crumbing = () => can(table, "pushCrumb") && can(table, "popCrumb")
      && can(table, "getCrumbs") && can(table, "setCrumbs");
    const trail = () => (crumbing() ? table.getCrumbs() : []);
    let crumbSels = [];
    const selsFit = () => crumbSels.length === trail().length;
    function land(sel, back) {
      if (!can(table, "select")) return;
      const rows = visible();
      if (!rows.length) return;
      if (sel && sel.id
          && table.select(sel.id, sel.col === null ? undefined : sel.col)) return;
      const at = column();
      const i = Math.max(0, Math.min(back || 0, rows.length - 1));
      table.select(rows[i].id, at === null ? undefined : at);
    }
    const refToken = (id) => `ref:${/[\s&"]/.test(id) ? `"${id}"` : id}`;
    const hereLabel = () => crumbLabels[query] || query || "all rows";
    // An absent `q' takes the default, so a cleared view is written empty.
    function remember(q) {
      const p = params();
      p.set("q", q);   // `keys' and anything else in the URL survives
      const t = trail(), labels = Object.keys(crumbLabels).length ? crumbLabels : null;
      if (!t.length && !labels) p.delete("crumbs");
      else p.set("crumbs", JSON.stringify( { trail: t, labels: crumbLabels, sels: selsFit() ? crumbSels : [] }));
      // `page'/`row'/the fragment are `remembered''s and survive: a query
      // committed under an open popup leaves it named in the URL.
      history.replaceState(null, "", `?${p.toString()}${location.hash || ""}`);
      if (can(table, "setPinned")) table.setPinned(q.trim() === savedQuery("default"));
    }
    function bootTrail() {
      try {
        const was = JSON.parse(params().get("crumbs") || "null");
        if (!was || typeof was !== "object")
          return { trail: [], labels: {}, sels: [] };
        return {
          trail: Array.isArray(was.trail) ? was.trail : [],
          labels: was.labels && typeof was.labels === "object" ? was.labels : {},
          sels: Array.isArray(was.sels) ? was.sels : [],
        };
      } catch (e) { return { trail: [], labels: {}, sels: [] }; }
    }
    const asking = (q) => (q ? `?q=${encodeURIComponent(q)}` : "");
    const fetchRows = (landing) =>
      viewing(load(asking(query)))
        .then((a) => { if (!table) return;
                       if (a.view && !sameColumns(a.view.columns || []))
                         { remount(landing && (() => landing())); return; }
                       paint(a);
                       if (landing) landing(); else land(null); })
        .catch(quiet);
    function commit(q) {
      if (q === query) return;
      query = q;
      leaving = arriving = null;   // both belonged to the view being left
      remember(q);
      fetchRows();
    }
    const filter = (q) => commit(q.trim());
    const strips = () => can(table, "stripLastToken") && can(table, "getQuery");
    const holds = (q) => can(table, "getQuery") && table.getQuery() === q;
    /**
     * @returns {(HTMLInputElement & HTMLElement) | null}
     */
    const filterBox = () =>
      /** @type {any} */ (document.querySelector("#app .tv-filter"));
    // Setting the box's value fires no input event, so this commits nothing.
    function showQuery() {
      const box = filterBox();
      if (box) box.value = query;
    }

    const unwrap = (r) => r.json().then((b) => {
      if (!r.ok) throw new Error(b.error || r.status);
      return b;
    });
    const getJSON = (url) => fetch(url).then(unwrap);
    const postJSON = (url, body, extra) =>
      fetch(url, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify(body),
        ...extra,
      });
    const outcome = (r) => r.json().then((b) => ({ status: r.status, body: b }));

    const at = (id, child) => `/headline?id=${encodeURIComponent(id)}`
      + (child === null || child === undefined ? "" : `&child=${child}`);
    const headline = (id, child) => getJSON(at(id, child));
    const post = (id, digest, asked, extra, child) =>
      postJSON(at(id, child), { ...asked, digest }, extra);
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
