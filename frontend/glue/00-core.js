// The shell.  Rules and consequences: AGENTS.hs; grammar: SCHEMA.md.

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
    // THE FLOOR'S OWN HELPERS, above the parts that need them: a `const'
    // declared later is a TDZ error in an eagerly built dependency object.
    const keySaid = (k) => (what) => echo(`${k} → ${what}`);
    const cells = () => can(table, "getSelection");
    const column = () => (cells() ? table.getSelection().col : null);
    const visible = () => (table ? table.getVisible() : []);
    const said = (b, what) =>
      echo(`${b.seq} → ${b.command}${what ? ` (${what})` : ""}`);
    const failed = (b, name) => (e) => {
      said(b, e.message);
      append("cmd", "error", `${name} failed: ${e.message}`);
    };
    const PRIORITY_RING = [null, "C", "B", "A"];
    const cycled = (now, step) => {
      const at = PRIORITY_RING.indexOf(now || null);
      const n = PRIORITY_RING.length;
      return PRIORITY_RING[((at === -1 ? 0 : at) + (step > 0 ? 1 : n - 1)) % n];
    };
    const priorityIn = (cell) => {
      const t = String(cell || "").trim();
      const m = /^\[#(.)\]$/.exec(t);
      return m ? m[1].toUpperCase() : (t ? t.toUpperCase() : null);
    };
    const priorityOf = (id) => priorityIn((rowOf(id).cells || {}).priority);
    const EMPTY = "*empty*";
    const badgeColor = (value, key) =>
      (((cols.find((c) => c.key === (key || "state")) || {}).badges || [])
        .find((b) => b.value === value) || {}).color || "";
    const rowOf = (id) => visible().concat(all).find((r) => r.id === id) || {};
    // Whole-compare: the state column's badge palette rides inside the columns.
    const sameColumns = (next) => JSON.stringify(next) === JSON.stringify(cols);
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
    // Variadic because a capability is usually a PAIR the shell needs whole.
    const can = (mount, ...names) =>
      !!mount && names.every((n) => typeof mount[n] === "function");
    const lacks = (what) => `this table-view.js has no ${what}`;
    const wants = (b, what, ...names) =>
      can(table, ...names) || (said(b, lacks(what)), false);
    const rowStep = (k) => (k === "<down>" || k === "n" || k === "j" ? 1
                          : k === "<up>" || k === "p" || k === "k" ? -1 : 0);
    // BESIDE A FIELD the letters are spoken for, so the walk is the arrows and C-n/C-p.
    const walkStep = (k) => (k === "<down>" || k === "C-n" ? 1
                           : k === "<up>" || k === "C-p" ? -1 : 0);
    const stepIn = (mount, step) =>
      can(mount, "selectStep") && mount.selectStep(step);
    const flagsOn = (mount) => can(mount, "flagRow", "getFlagged");
    /** Put TEXT over [FROM, TO) in BOX and leave the caret after it. */
    const spliceIn = (box, from, to, text) => {
      box.value = box.value.slice(0, from) + text + box.value.slice(to);
      box.setSelectionRange(from + text.length, from + text.length);
    };
    const selectedId = (mount) =>
      (can(mount, "getSelection") ? (mount.getSelection() || {}).id : null) || null;
    const soon = (fn) =>
      (typeof requestAnimationFrame === "function" ? requestAnimationFrame(fn)
                                                    : setTimeout(fn, 0));
    let table = null, socket = null, backoff = 1000;
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
        // The filter is summoned onto the chip strip's own row, never resident.
        filterDock: "strip",
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
        onRefused: refused, // a shaping token typed at `/', which the box keeps
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
      // AN ADDED TOKEN IS NO EVIDENCE OF A DROPPED KEY: `+state:DONE' widens its
      // own axis, so a zero the server answered correctly would be reported as
      // skew. A current asset marks the sign as `added'; the leading `+' is the
      // same reading for a stale one that still takes the sign for text.
      const loose = TableView.parseQuery(query, keys).filter((t) =>
        t.key === null && !t.quoted && !t.negated && !t.added
          && !t.value.startsWith("+") && /^[^:=]+[:=]./.test(t.value));
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
    // THE SAVED VIEWS, LIVE: seeded from the boot blob and moved by a pin.
    /** @type {Record<string, string>} */
    const saved = {};
    const seedViews = (views) =>
      (views || []).forEach((v) => { saved[v.id] = String(v.query || "").trim(); });
    seedViews(CFG.views);
    const savedQuery = (id) => saved[id] || "";
    const VIEW_AT = /(?:^|\s)view:([A-Za-z0-9_-]+)(?=\s|$)/;
    function viewNamed(q) {
      const m = VIEW_AT.exec(String(q || ""));
      return m && Object.prototype.hasOwnProperty.call(saved, m[1]) ? m[1] : null;
    }
    // NOT A CONSTANT: the tree's own default arrives after the walk (`adopt').
    let bootedOn = savedQuery("default");
    // WHETHER THE READER ASKED: `remember' writes `q' back on boot, so the
    // door's own URL is read once, before anything writes it.
    const bootHadQ = params().has("q");
    function bootQuery() {
      // THE URL IS READ LIVE: a remount re-enters here after `remember'
      // wrote `q', and must keep the query it stands in.
      const q = params().has("q") ? urlQuery() : bootedOn;
      const named = viewNamed(q);
      return named ? savedQuery(named) : q;
    }
    let crumbLabels = {};
    const crumbing = () =>
      can(table, "pushCrumb", "popCrumb", "getCrumbs", "setCrumbs");
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
      // `page'/`row'/the fragment are `remembered''s and survive.
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
    const filter = (q) => {
      const named = viewNamed(q);
      if (named) { applyNamed(named); return; }
      commit(q.trim());
    };
    /** The KEY a refused token names, its sign and value off: `+sort:title' is `sort'. */
    const shapingKey = (spelling) =>
      String(spelling || "").replace(/^[-+]/, "").split(/[:=]/)[0];
    // A REFUSAL NAMES THE OTHER DOOR rather than the rule: `/' took a shaping
    // token, and the one line covers sort:, columns: and view: alike.
    const refused = (spelling) => {
        const note = `${shapingKey(spelling)}: autocomplete restricted, this key belongs to #'compose (kbd ".")`;
        append("filter", "info", note);
        echo(note);
    };
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
    const getJSON = (url, extra) => fetch(url, extra).then(unwrap);
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

    // dired's `d'/`D'/`x'/`u' over five surfaces, each declaring a SHAPE — AGENTS.hs.
    const YES = "YES";
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
      // `x' takes the FLAGS alone and ASKS, naming the count.  ONE QUESTION,
      // WEIGHTED TO THE ACT: a take raising a wall of its own is not asked twice.
      if (k === "x") {
        if (!flags.length) { say(s.idle); return; }
        const go = () => {
          if (can(m, "clearFlags")) m.clearFlags();
          s.take(flags, (n) => `${n} flagged`);
        };
        if (s.walled && s.walled(flags)) { go(); return; }
        askText(`${s.verb} · ${flags.length} flagged`,
                `type ${YES} and RET · ESC leaves them`,
                (c) => (c.text.trim().toUpperCase() === YES ? go() : say(s.spared)));
        return;
      }
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
    const pager = () => can(table, "nextPage", "pageInfo");
    const pageNow = () => (pager() ? table.pageInfo().page : 1);
    function turnPage(b, step) {
      if (!wants(b, "pager", "nextPage", "pageInfo")) return;
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
      if (!wants(b, "cell selection", "getSelection")) return;
      const at = column();
      // NOTHING SITS LEFT OF A WHOLE ROW.  Forward from one enters its first
      // cell; backward has nowhere to go, and landing on the first column made
      // the two directions the same press.
      if (at === null && step < 0) { said(b, ""); return; }
      const want = at === null ? 0 : at + step;
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
      if (!wants(b, "marks", "toggleMark")) return;
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
