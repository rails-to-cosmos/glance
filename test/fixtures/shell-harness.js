// Boots the shell's inline glue under node and reports what the page asked the
// server for and what survived: a call that is present and never reached matches
// a string search just the same.  The glue is the page's own, extracted from a
// rendered `/' by TestServe.
//
//   node shell-harness.js DIR SEARCH TOTAL [KEYS] [ACTS] [STORE]
//
// DIR holds `shell.js', `keys.json', `cfg.json' and `elm.js'.  KEYS are pressed
// once the boot has settled; ACTS run one at a time, each settled before the
// next, and their vocabulary is `ACTIONS' below.  NOTATION, shared by both:
// `C-'/`S-'/`M-' are the modifiers, a `%CODE' tail is the PHYSICAL key under
// the character (`т%KeyN'), and in typed text `_' is a space, `|' a newline
// and `~' a literal bar.

const fs = require("fs");
// Argv: the BOOT reads a stored preference and every act runs after the eval.
const [dir, search, total, keys, acts, store] = process.argv.slice(2);

const asked = [];
const tags = [];
// A TOTAL OF NONE IS AN EMPTY STORE, and argv since every act runs after the boot.
let rows = +total === 0 ? [] : ["one", "two", "three"].map((title, i) =>
  ({ id: `r${i + 1}`, cells: { state: "TODO", title, tag: ":web:" } }));
let hidden = [];
// ONE column declares `sortable' where the real producer opts every column in,
// so `^' can reach a column that sorts and one that refuses.
let columns = [
  { key: "state", sortable: true,
    badges: [ { value: "TODO", color: "#e0af68", group: "active" }
            , { value: "READING", color: "#bb9af7", group: "active" }
            , { value: "DONE", color: "#73daca", group: "inactive" } ] },
  { key: "tag" },
];
const declaredSort = { column: "state", ascending: true };
let tag = "\"t0\"";
// `noreferences' aims at the ref query alone: the boot still answers for all.
let unreferenced = false;
let served = +total;
// `ORG_GLANCE_ID' is in the org text and NOT in the properties: a hidden key
// the server keeps for itself, and `?child=0' walks into the child.  The
// grainy list's second item is separated by a BLANK LINE, which org lets
// stand inside a list — so the run is one list.
const grainBody = [ "* TODO one",
                    "lead in",
                    "- alpha",
                    "  more alpha",
                    "  - nested",
                    "",
                    "- beta",
                    "- gamma",
                    "",
                    "#+begin_quote",
                    "quoted one",
                    "",
                    "quoted two",
                    "#+end_quote",
                    "",
                    "tail para",
                    "** two",
                    "child body", "" ].join("\n");
const checkyBody = [ "* TODO one",
                     "- [ ] alpha",
                     "- [X] beta",
                     "- [-] gamma",
                     "- delta",
                     "** two",
                     "child body", "" ].join("\n");
// MIXED on purpose: the end-to-end stop count says the table rides the one walk.
const tabledBody = [ "* TODO one",
                     "lead in",
                     "| a | b |",
                     "|---+---|",
                     "| 1 | 2 |",
                     "| 3 | 4 |",
                     "",
                     "- alpha",
                     "- beta",
                     "",
                     "tail para",
                     "** two",
                     "child body", "" ].join("\n");
// The bare url is written TWICE where the answer holds ONE entry, so the
// second occurrence has no span.
const linkyBody = [ "* TODO one [[https://t.example/][the title link]]",
                    "see [[https://a.example/][alpha]] and [[https://b.example/]] here",
                    "",
                    "bare https://c.example/ then https://c.example/ twice",
                    "** two",
                    "child body", "" ].join("\n");
const linkyTitle = "one [[https://t.example/][the title link]]";
const linkyLinks = [
  { target: "https://t.example/", desc: "the title link", type: "https", span: [11, 49] },
  { target: "https://a.example/", desc: "alpha", type: "https", span: [54, 83] },
  { target: "https://b.example/", desc: "https://b.example/", type: "https", span: [88, 110] },
  { target: "https://c.example/", desc: "https://c.example/", type: "https", span: [122, 140] },
];
let linky = false;
let grainy = false;
let tabled = false;
let checky = false;
const org = "* TODO one\nSCHEDULED: <2026-08-01 Sat>\n:PROPERTIES:\n"
  + ":ORG_GLANCE_ID: r1\n:EFFORT: 0:30\n:END:\n:LOGBOOK:\n- moved here\n:END:\n"
  + "first para\n\nsecond para\n** two\nchild body\n";
const body = "* TODO one\nfirst para\n\nsecond para\n** two\nchild body\n";
const properties = [["EFFORT", "0:30"]];
const planning = [["SCHEDULED", "<2026-08-01 Sat>"]];
const logbook = ":LOGBOOK:\n- moved here\n:END:\n";
let digest = "d0";
let headPriority = null;
/** GET /headline's answer: `child' is the index this answer is FOR, `parent'
 * the one DEL climbs to, null being the row. */
const subtree = (child) => (child === null
  ? { id: "r1", file: "a.org", child: null, parent: null, path: ["one"],
      cells: { state: "TODO", priority: headPriority,
               title: linky ? linkyTitle : "one", tags: "" },
      children: [ { index: 0, level: 2, state: null, priority: null,
                    title: "two", tags: ":web:" } ],
      level: 1, properties, planning, logbook, digest,
      titleAt: linky ? 7 : 11,
      // The link scan rides the materialize, one source with the `/links' stub.
      links: linky || grainy ? links : [],
      org: linky ? linkyBody : grainy ? grainBody : tabled ? tabledBody
           : checky ? checkyBody : org,
      span: { start: 0,
              end: (linky ? linkyBody : grainy ? grainBody : tabled ? tabledBody
                          : checky ? checkyBody : org).length },
      body: linky ? linkyBody : grainy ? grainBody : tabled ? tabledBody
            : checky ? checkyBody : body,
      ownLines: grainy ? 16 : tabled ? 11 : checky ? 5 : 4 }
  : { id: "r1", file: "a.org", child: 0, parent: null, path: ["one", "two"],
      cells: { state: null, priority: null, title: "two", tags: ":web:" },
      children: [],
      org: "** two :web:\nchild body\n",
      body: "** two :web:\nchild body\n", ownLines: 3, level: 2,
      properties: [], planning: [], logbook: "", digest,
      links: linky || grainy ? links : [] });
const wroteAt = [];
const readAt = [];
const writes = [];
const commands = [];
let refusing = false;
// SERVED OUT OF ALPHABET on purpose: the server's order is the walk's and the
// sheet's is system-then-tags, so a fixture in order could not tell them apart.
let layers = [
  { path: "/o/.org-glance/config/system.org", tag: null,
    lines: ["#+TODO: TODO | DONE"],
    keywords: { active: ["TODO"], inactive: ["DONE"] },
    template: "", digest: "" },
  { path: "/o/.org-glance/config/tags/film.org", tag: "film",
    lines: ["#+TODO: WATCHING | WATCHED"],
    keywords: { active: ["WATCHING"], inactive: ["WATCHED"] },
    template: "", digest: "f1" },
  { path: "/o/.org-glance/config/tags/book.org", tag: "book",
    lines: ["#+TODO: TODO READING | READ"],
    keywords: { active: ["TODO", "READING"], inactive: ["READ"] },
    template: "* %?", digest: "c1" },
];
const configWrites = [];
let configTick = 1;
let sources = [
  { source: "default", active: ["TODO"],    inactive: ["DONE"] },
  { source: "book",    active: ["READING"], inactive: ["READ"] },
  { source: "file",    active: ["LATER"],   inactive: [] },
];
const resolved = [];
let stalling = false;
let links = [
  { target: "https://one.example/a", desc: "First reference", type: "https",
    span: [10, 48] },
  { target: "https://two.example/b", desc: "Second reference", type: "https",
    span: [60, 99] },
  { target: "mailto:t@example.org", desc: "mailto:t@example.org", type: "mailto",
    span: [120, 140] },
];
let linkDigest = "d0";
const linked = [];
const opened = [];
// A tag command does NOT move this: the route never writes the store.
let rowTags = { r1: ["web"], r2: ["web"], r3: ["web"] };
let vocabulary = ["archive", "book", "web", "work"];
const tagCounts = { archive: 12, book: 3, web: 40, work: 7 };
const tagged = [];
// An emptied view is a LINE TAKEN OFF, so the answer falls back to the built-in.
const BUILTIN = { default: "state:*active*"
                , agenda: "state:*active* -planned:*empty* sort:scheduled" };
let viewQuery = BUILTIN.default;
let agendaQuery = BUILTIN.agenda;
let stateHues = [];
let captureLine = "";
const captureTarget = "/o/inbox.org";
const capturedId = "r3";
const captureCodes = [
  { code: "%?", means: "where the text you type lands" },
  { code: "%U", means: "the moment of capture, inactive" },
];
// `template' and a non-empty `prompts' are two facts: `film' has one and no ask.
const capturePrompts = { book: ["Author"] };
const captureTemplates = ["book", "film"];
const captureAsked = [];

globalThis.location = { search, protocol: "http:", host: "h", pathname: "/" };
globalThis.history = {
  replaceState: (_state, _title, url) => {
    location.search = String(url).startsWith("?") ? url : "";
  },
};
/** EVERY TIMER THE PAGE SET, with when it was due: `wait'/`settle' drain on the
 * page's SCHEDULE, and a 30s reconnect backoff is never owed to a short wait. */
const owing = new Set();
const realTimeout = globalThis.setTimeout, realClear = globalThis.clearTimeout;
globalThis.setTimeout = (fn, ms, ...rest) => {
  const box = { due: Date.now() + (Number(ms) || 0) };
  box.id = realTimeout((...a) => { owing.delete(box); return fn(...a); }, ms, ...rest);
  owing.add(box);
  return box.id;
};
globalThis.clearTimeout = (id) => {
  for (const box of owing) if (box.id === id) { owing.delete(box); break; }
  return realClear(id);
};
const drainTo = async (when) => {
  for (let turn = 0; turn < 600; turn += 1) {
    let due = false;
    for (const box of owing) if (box.due <= when) { due = true; break; }
    if (!due) return;
    await new Promise((go) => realTimeout(go, 2));
  }
};
/** And FOLLOW THE CHAIN: a landed fetch schedules a paint which schedules the
 * next thing, so the window moves with the clock. */
const SOON = 30;
const TURN = 20;
const drainSoon = async () => {
  for (let turn = 0; turn < 600; turn += 1) {
    const by = Date.now() + SOON;
    let due = false;
    for (const box of owing) if (box.due <= by) { due = true; break; }
    if (!due) return;
    await new Promise((go) => realTimeout(go, 2));
  }
};

const answer = (status, body, headers) => Promise.resolve({
  ok: status >= 200 && status < 300,
  status,
  headers: { get: (name) => (headers || {})[String(name).toLowerCase()] || null },
  json: () => Promise.resolve(body),
  text: () => Promise.resolve(""),
});
let down = false;
// The only way to observe a swap in flight: everything else settles as a microtask.
let hanging = false;
const held = [];
let changing = false;
const cheld = [];
const capped = (url, list) => {
  const at = /[?&]limit=(\d+)/.exec(String(url));
  return at ? list.slice(0, Number(at[1])) : list;
};
globalThis.fetch = (url, init) => {
  if (down) return Promise.reject(new Error("fetch failed"));
  const sent = ((init || {}).headers || {})["if-none-match"];
  if (String(url).startsWith("/headlines")) {
    asked.push(url);
    if (sent) tags.push(sent);
    const send = () => {
      if (sent === tag) return answer(304, null, {});
      const empty = unreferenced && String(url).indexOf("q=ref%3A") !== -1;
      return answer(200, { title: "t", columns, sort: declaredSort,
                           rows: empty ? [] : capped(url, rows) },
                    { "x-glance-total": empty ? "0" : String(served), etag: tag });
    };
    if (hanging) return new Promise((go) => held.push(() => go(send())));
    return send();
  }
  if (String(url) === "/command") {
    const sent = JSON.parse((init || {}).body || "{}");
    commands.push(sent);
    if (sent.name === "capture")
      return refusing
        ? answer(400, { error: "inbox.org changed on disk" })
        : answer(200, { ok: true, file: captureTarget, digest: "d1", id: capturedId });
    return answer(200, {
      results: (sent.ids || []).map((id) =>
        refusing ? { id, ok: false, error: "a.org changed on disk" }
                 : { id, ok: true, digest: "d1" }),
    });
  }
  // Not gated on `refusing': what that flag stands for is a WRITE the server
  // turns down, and a chain that could not resolve its tag would never reach one.
  if (String(url) === "/capture" || String(url).startsWith("/capture?")) {
    captureAsked.push(url);
    const at = /[?&]tag=([^&]*)/.exec(String(url));
    const tag = at ? decodeURIComponent(at[1]) : null;
    return answer(200, {
      template: !!(tag && captureTemplates.indexOf(tag) !== -1),
      prompts: tag ? (capturePrompts[tag] || []) : [],
      tags: vocabulary,
      codes: captureCodes,
    });
  }
  if (String(url).startsWith("/keywords?ids=")) {
    resolved.push(url);
    // Never settling: the one way to observe the overlay up and the answer not.
    if (stalling) return new Promise(() => {});
    return refusing ? answer(400, { error: "GET /keywords?ids=<row id>" })
                    : answer(200, { sources, unknown: [] });
  }
  if (String(url).startsWith("/tags?ids=")) {
    tagged.push(url);
    if (stalling) return new Promise(() => {});
    if (refusing) return answer(400, { error: "GET /tags?ids=<row id>" });
    const ids = String(url).slice("/tags?".length).split("&")
      .map((p) => decodeURIComponent(p.slice("ids=".length)));
    return answer(200, {
      rows: ids.filter((id) => rowTags[id]).map((id) => ({ id, tags: rowTags[id].slice() })),
      vocabulary,
      counts: tagCounts,
      unknown: ids.filter((id) => !rowTags[id]),
    });
  }
  if (String(url).startsWith("/links?id=")) {
    linked.push(url);
    return refusing ? answer(404, { error: "no headline with id r1" })
                    : answer(200, { links, digest: linkDigest });
  }
  if (String(url) === "/config") {
    if ((init || {}).method !== "POST")
      return answer(200, { layers,
                           views: [ { id: "default", query: viewQuery }
                                  , { id: "agenda", query: agendaQuery } ],
                           themes: ["light", "dark"], colors: stateHues,
                           capture: captureLine,
                           keywords: { active: ["TODO"], inactive: ["DONE"] } });
    const sent = JSON.parse((init || {}).body || "{}");
    configWrites.push(sent);
    const layer = layers.find((l) => l.path === sent.path);
    if (!layer || layer.digest !== sent.digest)
      return answer(409, { reason: "drift", digest: (layer || {}).digest || "",
                           error: "the config file changed on disk since it was read" });
    layer.lines = (sent.lines || []).filter(Boolean);
    if (sent.lines !== undefined) {
      const body = (layer.lines[0] || "").replace(/^#\+TODO:/, "");
      const [act, done] = body.split("|");
      const words = (t) => String(t || "").split(/\s+/).filter(Boolean);
      layer.keywords = { active: words(act), inactive: words(done) };
    }
    const views = sent.views || {};
    if (views.default !== undefined) viewQuery = views.default || BUILTIN.default;
    if (views.agenda !== undefined) agendaQuery = views.agenda || BUILTIN.agenda;
    if (sent.colors !== undefined) stateHues = sent.colors;
    if (sent.capture !== undefined) captureLine = sent.capture;
    layer.digest = `c${(configTick += 1)}`;
    const send = () => answer(200, { path: sent.path, digest: layer.digest });
    if (changing) return new Promise((go) => cheld.push(() => go(send())));
    return send();
  }
  if (String(url).startsWith("/headline?")) {
    const named = /[?&]child=(\d+)/.exec(String(url));
    const child = named ? Number(named[1]) : null;
    if ((init || {}).method === "POST") {
      writes.push(JSON.parse((init || {}).body || "{}"));
      wroteAt.push(child === null ? "r1" : `r1#${child}`);
      // THE STORE LAGS THE WRITE IT ANSWERS FOR, and this models the lag at its
      // worst — the GET never catches up, so a reload that trusts it reverts.
      return refusing
        ? answer(409, { reason: "drift", digest,
                        error: "a.org changed on disk since this subtree was materialized" })
        : answer(200, { digest: `w${writes.length}` });
    }
    if (child !== null && child !== 0)
      return answer(404, { error: `r1 has no child ${child}; it holds 1` });
    readAt.push(child === null ? "r1" : `r1#${child}`);
    return answer(200, subtree(child));
  }
  return answer(404, {});
};
let socket = null;
globalThis.WebSocket = function () {
  socket = this;
  this.close = () => { socket = null; };
  // A socket opens on a LATER turn than its constructor: the page assigns
  // `onopen' after it returns, and the wash's other half is cleared by it.
  setTimeout(() => { if (socket === this && this.onopen) this.onopen(); }, 0);
};
// Everything a renderer holds is held PER INSTANCE, and the table's rows are
// the STORE's — which is what lets an act move the store and the table follow.
let mounts = 0, sets = 0, raises = 0;
let lmounts = 0, tmounts = 0, tsets = 0;
const paints = [];
// Row ops SPLICED, recorded as well as their effect: landing right without
// splicing reads the same off the rows alone.
const spliced = [];
let sorted = null, sortCalls = 0, sortChain = [];
const tokensOf = (q) => String(q || "").split(/[\s&]+/).filter(Boolean);
const sortTokensIn = (q) => tokensOf(q)
  .filter((t) => t.startsWith("sort:") && t.length > "sort:".length)
  .map((t) => t.slice("sort:".length).split(":"))
  .map(([column, dir]) => ({ column, ascending: dir !== "desc" }));
const withSort = (q, chain) => tokensOf(q)
  .filter((t) => !t.startsWith("sort:"))
  .concat(chain.map((k) => `sort:${k.column}${k.ascending ? "" : ":desc"}`))
  .join(" ");
let main = null;
const cellCol = (cols, col) => {
  if (col === null || col === undefined) return null;
  const at = Math.trunc(col);
  return at >= 0 && at < cols.length ? at : null;
};
/** Set by `bare', `pageless', `sortless' and `crumbless', remounts included. */
let markless = false, pagerless = false, sortnone = false, crumbless = false;
const makeMount = (host, view, options, own) => {
  const o = options || {};
  const m = {
    own,
    // Per instance and never a second copy: a hardcoded pair would go on agreeing.
    cols: (view || {}).columns || [],
    // The chain in force is the QUERY's where it names a `sort:' token, else the
    // view's own.
    _seedSort: (() => {
      const named = sortTokensIn(o.initialQuery || "");
      const d = (view || {}).sort;
      sortChain = named.length ? named
        : (Array.isArray(d) ? d : d ? [d] : [])
            .map((k) => ({ column: k.column, ascending: k.ascending !== false }));
      return null;
    })(),
    held: o.initialQuery || "",
    marksOn: o.marks === true,
    flagsOn: o.flags === undefined ? o.marks === true : o.flags === true,
    hintsOn: o.actionHints !== false,
    flagHelp: o.flagHelp || "",
    pageSize: o.pageSize || 0,
    // Two terms, because rows go away under a mount: `keepSelection' keeps the
    // ROW while it is on the page and falls back to the PLACE, clamped.
    cursor: 0, rowId: null, selCol: null, pageAt: 0,
    marks: new Set(), flags: new Set(), crumbs: [],
    pinned: !!o.pinned, onPin: typeof o.onPin === "function" ? o.onPin : null,
    onFilter: typeof o.onFilter === "function" ? o.onFilter : null,
  };
  const all = () => (m.own ? m.own : rows);
  const pageMax = () =>
    (m.pageSize ? Math.max(1, Math.ceil(all().length / m.pageSize)) : 1);
  const onPage = () => {
    if (!m.pageSize) return all();
    m.pageAt = Math.max(0, Math.min(m.pageAt, pageMax() - 1));
    return all().slice(m.pageAt * m.pageSize, (m.pageAt + 1) * m.pageSize);
  };
  /** `keepSelection' verbatim: the place is the last index something landed on
   * and is NOT re-derived while the row is there, so rows going from ABOVE
   * point land the fallback lower. */
  const keep = () => {
    if (m.rowId === null) return;
    const on = onPage();
    // The place going is what makes the next set land on row 0.
    if (!on.length) { m.rowId = null; m.selCol = null; m.cursor = -1; return; }
    if (on[m.cursor] && on[m.cursor].id === m.rowId) return;
    if (on.some((r) => r.id === m.rowId)) return;
    m.cursor = Math.max(0, Math.min(m.cursor, on.length - 1));
    m.rowId = on[m.cursor].id;
  };
  const held = () => {
    const on = onPage();
    if (m.rowId === null || !on.length) return -1;
    if (on[m.cursor] && on[m.cursor].id === m.rowId) return m.cursor;
    const i = on.findIndex((r) => r.id === m.rowId);
    return i !== -1 ? i : Math.max(0, Math.min(m.cursor, on.length - 1));
  };
  const sit = (i) => {
    const on = onPage();
    m.cursor = on.length ? Math.max(0, Math.min(i, on.length - 1)) : 0;
    m.rowId = on.length ? on[m.cursor].id : null;
  };
  const pageTo = (to, first) => {
    const at = Math.max(0, Math.min(pageMax() - 1, to));
    if (at === m.pageAt) return false;
    m.pageAt = at;
    sit(first ? 0 : onPage().length - 1);
    return true;
  };
  m.onPage = onPage;
  m.at = held;
  m.sit = sit;
  m.handle = {
    el: host || { querySelector: () => null },
    // A count for the table, whose rows are the store's; a model mount keeps them.
    setRows: (list) => {
      if (m.own) {
        m.own = (list || []).slice();
      } else { sets += 1; paints.push((list || []).length); }
      keep();
    },
    upsertRow: (row) => {
      spliced.push(`upsert ${row.id}`);
      const list = all(), at = list.findIndex((r) => r.id === row.id);
      if (at === -1) list.push(row); else list[at] = row;
      keep();
    },
    deleteRow: (id) => {
      spliced.push(`delete ${id}`);
      const list = all(), at = list.findIndex((r) => r.id === id);
      if (at !== -1) list.splice(at, 1);
      m.marks.delete(id);   // the row is gone; a mark on it would outlive it
      m.flags.delete(id);
      keep();
    },
    getQuery: () => m.held,
    getRows: () => all().slice(),
    setQuery: (q) => { m.held = String(q == null ? "" : q).trim(); },
    setPinned: (on) => { m.pinned = !!on; },
    stripLastToken: () => {
      if (!m.held) return false;
      m.held = tokensOf(m.held).slice(0, -1).join(" ");
      return true;
    },
    getSelection: () => {
      const at = held();
      return { id: at === -1 ? null : onPage()[at].id, col: m.selCol };
    },
    getVisible: () => onPage(),
    // Clamped, never wrapped; from NO selection it lands on the end it steps from.
    selectStep: (step) => {
      const on = onPage();
      if (!on.length) return false;
      const at = held();
      if (at === -1) { sit(step < 0 ? on.length - 1 : 0); return true; }
      const to = at + step;
      if (to < 0 || to >= on.length) return false;
      sit(to);
      return true;
    },
    // A column index OUTSIDE the table is a WHOLE-ROW selection, which makes
    // walking off the last cell a landing.
    select: (id, col) => {
      const at = onPage().findIndex((r) => r.id === id);
      if (at === -1) return false;
      sit(at);
      m.selCol = cellCol(m.cols, col);
      return true;
    },
    nextPage: () => pageTo(m.pageAt + 1, true),
    previousPage: () => pageTo(m.pageAt - 1, false),
    pageInfo: () => {
      const size = m.pageSize || all().length;
      return { page: m.pageAt + 1, pages: pageMax(),
               from: all().length ? m.pageAt * size + 1 : 0,
               to: Math.min(all().length, (m.pageAt + 1) * size), total: all().length };
    },
    toggleMark: (id) => {
      const on = !m.marks.has(id);
      if (on) m.marks.add(id); else m.marks.delete(id);
      return on;
    },
    getMarked: () => [...m.marks],
    clearMarks: () => m.marks.clear(),
    markedCount: () => m.marks.size,
    // The count AFTER, which is the handle's documented answer.
    markAll: () => { for (const r of all()) m.marks.add(r.id); return m.marks.size; },
    flagRow: (id) => m.flags.add(id),
    unflagRow: (id) => m.flags.delete(id),
    getFlagged: () => [...m.flags],
    clearFlags: () => m.flags.clear(),
    openFilter: () => { raises += 1; field("filter").focus(); },
    sortBy: (column, ascending) => { sorted = { column, ascending }; sortCalls += 1;
      sortChain = [{ column, ascending }]; },
    // The promotion rule verbatim, and it WRITES THE QUERY: the press arrives at
    // `onFilter' as an ordinary commit.
    sortPromote: (column) => {
      const col = (m.cols || []).find((c) => c.key === column);
      if (!col || col.sortable !== true) return false;
      const head = sortChain[0];
      if (head && head.column === column) {
        head.ascending = head.ascending === false;
      } else {
        sortChain = [{ column, ascending: true }]
          .concat(sortChain.filter((k) => k.column !== column));
      }
      sorted = { column: sortChain[0].column, ascending: sortChain[0].ascending };
      sortCalls += 1;
      m.held = withSort(m.held, sortChain);
      if (o.onFilter) o.onFilter(m.held);
      return true;
    },
    getSort: () => sortChain.map((k) => ({ column: k.column, ascending: k.ascending })),
    setSort: (chain) => { sortChain = (chain || []).map((k) => ({ column: k.column, ascending: k.ascending !== false })); },
    // `popCrumb' pops and RETURNS: whoever owns the fetching owns what a query means.
    setCrumbs: (list) => {
      m.crumbs = (Array.isArray(list) ? list : [])
        .filter((c) => c && typeof c === "object")
        .map((c) => ({ label: String(c.label || ""), query: String(c.query || "") }));
    },
    getCrumbs: () => m.crumbs.map((c) => ({ label: c.label, query: c.query })),
    pushCrumb: (c) => { m.handle.setCrumbs(m.crumbs.concat([c])); return m.crumbs.length; },
    popCrumb: () => (m.crumbs.length ? m.crumbs.pop() : null),
  };
  return m;
};
main = makeMount(null, null, {}, null);
globalThis.TableView = {
  mount: (host, view, options) => {
    const inst = makeMount(host, view, options, null);
    {
      mounts += 1; main = inst; paints.push(((view || {}).rows || []).length);
      // The renderer draws its filter box inside the mount and the page finds it
      // by selector.
      const box = field("filter");
      box.className = "tv-filter";
      if (host) host.appendChild(box);
    }
    if (markless) strip(inst.handle, MARK_CALLS);
    if (pagerless) strip(inst.handle, PAGE_CALLS);
    if (sortnone) strip(inst.handle, SORT_CALLS);
    if (crumbless) strip(inst.handle, CRUMB_CALLS);
    return inst.handle;
  },
  // A plain token split: it stops the stub answering `no tokens' where the real
  // renderer answers with some.
  parseQuery: (q) => String(q || "").split(/\s+/).filter(Boolean).map((raw) => {
    const negated = raw.startsWith("-");
    const body = negated ? raw.slice(1) : raw;
    const quoted = body.startsWith("\"");
    const at = quoted ? -1 : body.search(/[:=]/);
    return at === -1
      ? { key: null, value: body, negated, quoted }
      : { key: body.slice(0, at), value: body.slice(at + 1), negated, quoted };
  }),
  displayText: (s) => String(s || ""),
};
const MARK_CALLS = [ "toggleMark", "getMarked", "clearMarks", "markedCount"
                   , "markAll", "flagRow", "unflagRow", "getFlagged", "clearFlags" ];
const PAGE_CALLS = ["nextPage", "previousPage", "pageInfo"];
const SORT_CALLS = ["sortBy", "sortPromote", "getSort", "setSort"];
const CRUMB_CALLS = ["setCrumbs", "getCrumbs", "pushCrumb", "popCrumb"];
const strip = (h, names) => { for (const name of names) delete h[name]; };
const stripLive = (names) => {
  if (main) strip(main.handle, names);
};
// Recorded whole: `noopener' is half of what makes following a link safe.
globalThis.open = (url, target, features) => {
  opened.push({ url, target, features });
  return null;   // what a browser answers for a `noopener' window
};
const stored = {};
for (const pair of (store || "").split(",").filter(Boolean)) {
  const at = pair.indexOf("=");
  stored[pair.slice(0, at)] = pair.slice(at + 1);
}
globalThis.localStorage = {
  getItem: (k) => (Object.prototype.hasOwnProperty.call(stored, k) ? stored[k] : null),
  setItem: (k, v) => { stored[k] = String(v); },
  removeItem: (k) => { delete stored[k]; },
};
globalThis.matchMedia = () => ({ matches: false, addEventListener: () => {} });

const KEYS = fs.readFileSync(dir + "/keys.json", "utf8");
const CFGJSON = fs.readFileSync(dir + "/cfg.json", "utf8");
let active = null;
const fields = {};
// The tag matters: `typing()' reads it off `document.activeElement'.
const TAGS = { mtext: "textarea", filter: "input", pinput: "input",
               dtin: "input", dtext: "textarea",
               pkey: "input", pval: "input",
               tname: "input", themesel: "select",
               ltitle: "input", lurl: "input",
               ctarget: "input", clog: "input",
               ktag: "input", ktext: "textarea",
               clayer: "select", ctext: "textarea", ctpl: "textarea" };
const styleOf = () => ({
  custom: {},
  setProperty(name, value) { this.custom[name] = String(value); },
  getPropertyValue(name) { return this.custom[name] || ""; },
});
/** A REAL NODE TREE, minus the HTML parser only `innerHTML' needs. */
const ELEMENT_NODE = 1, TEXT_NODE = 3, FRAGMENT_NODE = 11;
const scrolls = [];
const isEl = (n) => n && n.nodeType === ELEMENT_NODE;
function unlink(n) {
  const up = n.parentNode;
  if (!up) return n;
  const at = up.childNodes.indexOf(n);
  if (at !== -1) up.childNodes.splice(at, 1);
  n.parentNode = null;
  return n;
}
const makeText = (data) => {
  const t = { nodeType: TEXT_NODE, nodeName: "#text", data: String(data),
              parentNode: null, childNodes: [],
              replaceData(off, count, s) {
                t.data = t.data.slice(0, off) + s + t.data.slice(off + count);
              } };
  for (const name of ["textContent", "nodeValue"])
    Object.defineProperty(t, name,
      { get: () => t.data, set: (v) => { t.data = String(v); } });
  // A virtual DOM rewrites a text node as `replaceData(0, node.length, text)'.
  Object.defineProperty(t, "length", { get: () => t.data.length });
  Object.defineProperty(t, "up", { get: () => t.parentNode });
  return t;
};

/** The selector subset written here: descendant chains of tag, `#id', `.class'
 * and `:not(...)'. */
const parseSel = (sel) => String(sel).split(",")
  .map((alt) => alt.trim().split(/\s+/).filter(Boolean).map((step) => {
    const s = { tag: "", id: "", cls: [], not: [] };
    let rest = step.replace(/:not\(([^)]*)\)/g, (_m, inner) => { s.not.push(inner); return ""; });
    const tag = rest.match(/^[A-Za-z][\w-]*/);
    if (tag) { s.tag = tag[0].toUpperCase(); rest = rest.slice(tag[0].length); }
    for (const bit of rest.split(/(?=[.#])/).filter(Boolean))
      if (bit[0] === "#") s.id = bit.slice(1); else if (bit[0] === ".") s.cls.push(bit.slice(1));
    return s;
  }))
  .filter((chain) => chain.length);
const stepHits = (el, s) => isEl(el)
  && (!s.tag || el.tagName === s.tag)
  && (!s.id || el.id === s.id)
  && s.cls.every((c) => el.classList.contains(c))
  && s.not.every((inner) => !selHits(el, parseSel(inner)));
const chainHits = (el, chain) => {
  if (!stepHits(el, chain[chain.length - 1])) return false;
  let up = el.parentNode, k = chain.length - 2;
  while (k >= 0 && up) { if (stepHits(up, chain[k])) k -= 1; up = up.parentNode; }
  return k < 0;
};
const selHits = (el, chains) => chains.some((c) => chainHits(el, c));
function descend(root, into) {
  for (const kid of root.childNodes) { if (isEl(kid)) { into.push(kid); descend(kid, into); } }
  return into;
}

const make = (tag) => {
  const e = {
    nodeType: ELEMENT_NODE,
    tagName: String(tag).toUpperCase(),
    nodeName: String(tag).toUpperCase(),
    id: "", value: "", className: "", placeholder: "", spellcheck: false,
    readOnly: false, disabled: false, selectedIndex: -1,
    style: styleOf(), dataset: {}, attrs: {},
    parentNode: null, childNodes: [],
    scrollTop: 0, scrollLeft: 0,
    clientHeight: 0, scrollHeight: 0, clientTop: 0, clientLeft: 0,
    selectionStart: 0, selectionEnd: 0,
    setSelectionRange(from, to) { this.selectionStart = from; this.selectionEnd = to; },
    focus() { active = this; },
    blur() { if (active === this) active = null; },
    // Kept: a palette narrows on its field's own `input', which no document press gives.
    on: {},
    addEventListener(type, fn) { (this.on[type] = this.on[type] || []).push(fn); },
    removeEventListener(type, fn) {
      this.on[type] = (this.on[type] || []).filter((f) => f !== fn);
    },
    fire(type, event) { for (const fn of (this.on[type] || []).slice()) fn(event); },
    appendChild(child) { return this.insertBefore(child, null); },
    insertBefore(child, before) {
      if (child.nodeType === FRAGMENT_NODE) {
        for (const kid of child.childNodes.slice()) this.insertBefore(kid, before);
        return child;
      }
      unlink(child);
      const at = before ? this.childNodes.indexOf(before) : -1;
      if (at === -1) this.childNodes.push(child); else this.childNodes.splice(at, 0, child);
      child.parentNode = this;
      return child;
    },
    replaceChild(now, was) {
      this.insertBefore(now, was);
      return unlink(was);
    },
    removeChild(child) { return unlink(child); },
    remove() { unlink(this); },
    // `id'/`class' stay on the properties, so `attributes' cannot report them twice.
    setAttribute(name, value) {
      if (name === "id") this.id = String(value);
      else if (name === "class") this.className = String(value);
      else this.attrs[name] = String(value);
    },
    setAttributeNS(_ns, name, value) { this.setAttribute(name, value); },
    getAttribute(name) {
      return name === "id" ? this.id
        : name === "class" ? this.className
        : Object.prototype.hasOwnProperty.call(this.attrs, name) ? this.attrs[name] : null;
    },
    hasAttribute(name) { return this.getAttribute(name) !== null; },
    removeAttribute(name) { delete this.attrs[name]; if (name === "id") this.id = ""; },
    matches(sel) { return selHits(this, parseSel(sel)); },
    closest(sel) {
      const chains = parseSel(sel);
      for (let at = this; at; at = at.parentNode) if (selHits(at, chains)) return at;
      return null;
    },
    querySelector(sel) { return this.querySelectorAll(sel)[0] || null; },
    querySelectorAll(sel) {
      const chains = parseSel(sel);
      return descend(this, []).filter((el) => selHits(el, chains));
    },
    select() {},
    // Geometry is beyond this harness; whether the page ASKED can be answered.
    scrollIntoView(opts) { scrolls.push({ className: this.className, opts }); },
    getBoundingClientRect: () => ({ top: 0, left: 0, right: 0, bottom: 0,
                                    width: 0, height: 0, x: 0, y: 0 }),
  };
  Object.defineProperty(e, "children", { get: () => e.childNodes.filter(isEl) });
  // A `NamedNodeMap' as far as anything reads one: how a virtual DOM takes over.
  Object.defineProperty(e, "attributes", { get: () => {
    const out = Object.keys(e.attrs).map((name) => ({ name, value: e.attrs[name] }));
    if (e.id) out.push({ name: "id", value: e.id });
    if (e.className) out.push({ name: "class", value: e.className });
    return out;
  } });
  Object.defineProperty(e, "firstChild", { get: () => e.childNodes[0] || null });
  Object.defineProperty(e, "nextSibling", { get: () => {
    const up = e.parentNode;
    return up ? up.childNodes[up.childNodes.indexOf(e) + 1] || null : null;
  } });
  // `up' is this harness's own name for the parent, which its probes climb by.
  Object.defineProperty(e, "up", { get: () => e.parentNode });
  Object.defineProperty(e, "parentElement",
    { get: () => (isEl(e.parentNode) ? e.parentNode : null) });
  e.classList = {
    contains: (name) => String(e.className).split(" ").indexOf(name) !== -1,
    add: (name) => { if (!e.classList.contains(name)) e.className = `${e.className} ${name}`.trim(); },
    remove: (name) => {
      e.className = String(e.className).split(" ").filter((c) => c !== name).join(" ");
    },
    toggle: (name, force) => {
      const on = force === undefined ? !e.classList.contains(name) : !!force;
      if (on) e.classList.add(name); else e.classList.remove(name);
      return on;
    },
  };
  // Every value the text was SET to: a repeat write leaves no other trace.
  e.wrote = [];
  Object.defineProperty(e, "textContent", {
    // The whole SUBTREE's text; a set drops the children, so nothing double-counts.
    get: () => e.childNodes.map((n) => n.textContent).join(""),
    set: (v) => {
      e.wrote.push(String(v));
      for (const kid of e.childNodes.splice(0)) kid.parentNode = null;
      if (String(v) !== "") e.appendChild(makeText(v));
    },
  });
  return e;
};
const docBody = make("body");
/** Every id the page asks for exists, hanging FLAT off `body' where the served
 * page nests. */
const field = (id) => {
  if (fields[id]) return fields[id];
  const e = make(TAGS[id] || "div");
  e.id = id;
  fields[id] = e;
  docBody.appendChild(e);
  return e;
};
const root = make("html");
root.appendChild(docBody);
const washed = [];
root.classList = {
  contains: (name) => root.className.split(" ").indexOf(name) !== -1,
  toggle: (name, force) => {
    const has = root.classList.contains(name);
    const on = force === undefined ? !has : !!force;
    if (on === has) return on;
    root.className = on ? `${root.className} ${name}`.trim()
                        : root.className.split(" ").filter((c) => c !== name).join(" ");
    if (name === "stale") washed.push(on ? "on" : "off");
    return on;
  },
};
const pressed = [];
const released = [];
globalThis.document = {
  getElementById: (id) =>
    id === "keys" ? { textContent: KEYS }
      : id === "cfg" ? { textContent: CFGJSON }
      : field(id),
  querySelector: (sel) => docBody.querySelector(sel),
  querySelectorAll: (sel) => docBody.querySelectorAll(sel),
  createElement: (tag) => make(tag),
  createElementNS: (_ns, tag) => make(tag),
  createTextNode: (data) => makeText(data),
  createDocumentFragment: () => {
    const f = make("#document-fragment");
    f.nodeType = FRAGMENT_NODE;
    return f;
  },
  addEventListener: (type, handler) => {
    if (type === "keydown") pressed.push(handler);
    if (type === "keyup") released.push(handler);
  },
  removeEventListener: () => {},
  getSelection: () => null,
  get activeElement() { return active; },
  documentElement: root,
  body: docBody,
  title: "",
  location: { href: "http://127.0.0.1/" },
};
// Elm's virtual DOM schedules its paints on this; `soon' falls back to `setTimeout'.
globalThis.requestAnimationFrame = (fn) => setTimeout(() => fn(0), 0);
globalThis.window = globalThis;
globalThis.addEventListener = () => {};

/** THE SMALL LISTS ARE ONE ELM PROGRAM; indirect eval, its output publishing
 * onto `this'. */
(0, eval)(fs.readFileSync(dir + "/elm.js", "utf8"));
let pinits = 0, pfills = 0;
const elmInit = globalThis.Elm.Listing.init;
globalThis.Elm.Listing.init = (opts) => {
  const host = opts && opts.node && opts.node.up ? opts.node.up.id : "";
  if (host === "mptable") pinits += 1;
  if (host === "ltable") lmounts += 1;
  if (host === "ttable") tmounts += 1;
  const app = elmInit(opts);
  const send = app.ports.listIn.send;
  app.ports.listIn.send = (m) => {
    if (m && m.kind === "setRows") {
      if (host === "mptable") pfills += 1;
      if (host === "ttable") tsets += 1;
    }
    return send(m);
  };
  return app;
};
eval(fs.readFileSync(dir + "/shell.js", "utf8"));

// Whether the dispatch CLAIMED a key is recorded under the name the script
// PRESSED: a chord left to the browser and one taken both look like nothing.
const press = (name, repeating, held) => {
  const cut = name.indexOf("%"), tailed = cut > 0 && cut < name.length - 1;
  const code = tailed ? name.slice(cut + 1) : undefined;
  const spelled = tailed ? name.slice(0, cut) : name;
  const ctrl = spelled.startsWith("C-"), shift = spelled.startsWith("S-"),
        alt = spelled.startsWith("M-");
  // `Space' is cooked to the " " a browser sends; the glue learns no new name.
  const bare = ctrl || shift || alt ? spelled.slice(2) : spelled;
  const event = {
    key: bare === "Space" ? " " : bare,
    code,
    ctrlKey: ctrl, altKey: alt, metaKey: false, shiftKey: shift,
    repeat: !!repeating, target: active || docBody,
    defaultPrevented: false,
    preventDefault: () => { prevented.push(name); event.defaultPrevented = true; },
  };
  for (const handler of pressed) handler(event);
  // The browser's OWN default for the one key a field needs it for: without it
  // "the page left this key to the field" and "nothing happened" read alike.
  if (spelled === "Backspace" && !event.defaultPrevented && active
      && (active.tagName === "INPUT" || active.tagName === "TEXTAREA"))
    active.value = String(active.value).slice(0, -1);
  // The KEYUP the page's derived-repeat set waits for; a HELD key sends none,
  // which is the native window's GTK auto-repeat with `repeat' unset.
  if (!held)
    for (const handler of released)
      handler({ key: event.key, code: event.code });
};
const prevented = [];

// The store moving is a new tag: a client holding the old one gets a body.
const step = () => { tag = `"t${Number(tag.slice(2, -1)) + 1}"`; };
const typeSetting = (id, text) => {
  if (field("config").className !== "on")
    throw new Error(`the settings sheet is not open: ${id}`);
  typed(field(id), text);
};
const typed = (box, text) => {
  box.value = text;
  box.selectionStart = text.length;
  box.selectionEnd = text.length;
  // A REAL EVENT, because a real listener calls back into it: Elm's `onInput' is
  // a `stopPropagationOn', which throws on an object carrying the target alone.
  box.fire("input", { target: box,
                      stopPropagation: () => {}, preventDefault: () => {} });
};
const typeIn = (box, which, text) => {
  if (field(box).className !== "on")
    throw new Error(`the document has no ${box} open: ${which}`);
  typed(field(which), text);
};
const typeOver = (which, arg) => {
  const at = arg.indexOf("=");
  if (field("pedit").className !== "on")
    throw new Error(`no panel row is open for editing: ${which}:${arg}`);
  if (String(patAt()) !== arg.slice(0, at))
    throw new Error(`panel row ${patAt()} is open, not ${arg}`);
  typed(field(which), arg.slice(at + 1));
};
const typeLink = (which, text) => {
  if (field("ledit").className !== "on")
    throw new Error(`no link is open for editing: ${which}`);
  typed(field(which), text);
};
const onKeywords = () => {
  const tab = field("ctabs").children.find((t) => t.textContent === "keywords");
  if (tab && tab.className !== "ctab on") tab.fire("click", {});
};
const cellsOf = (inst, keys) =>
  (inst ? inst.own.map((r) => keys.map((k) => r.cells[k])) : []);
const curOf = (inst) => (inst ? inst.at() : -1);
const mountFields = (prefix, inst) => ({
  [`${prefix}cols`]: inst ? inst.cols : [],
  [`${prefix}marks`]: inst ? inst.marksOn : null,
  [`${prefix}flags`]: inst ? inst.flagsOn : null,
  [`${prefix}hints`]: inst ? inst.hintsOn : null,
  [`${prefix}page`]: inst ? inst.pageSize : null,
  [`${prefix}flagHelp`]: inst ? inst.flagHelp : "",
  [`${prefix}marked`]: inst ? [...inst.marks] : [],
  [`${prefix}flagged`]: inst ? [...inst.flags] : [],
});
/** THE STRUCTURED DOCUMENT, read off what it DREW: `#dlist' holds one element
 * per row wearing its KIND as a class, reading as `[KIND, ...parts]'. */
const kindOf = (cls) => String(cls).split(" ")
  .filter((c) => c.startsWith("d-")).map((c) => c.slice(2)).join(":");
const wears = (e, cls) => String(e.className).split(" ").indexOf(cls) !== -1;
/** THE WALK, FLATTENED OUT OF THE DRAW, off a SELECTOR so the pane is free to
 * wear the wrapper an Elm mount adds. */
const flatRows = () => field("dlist").querySelectorAll(".de");
const ownerOf = () => {
  const rows = flatRows();
  return rows.map((row) => {
    for (let e = row.up; e; e = e.up)
      if (rows.indexOf(e) !== -1) return rows.indexOf(e);
    return -1;
  });
};
const segsOf = (row) => {
  const out = [];
  const walk = (e) => {
    for (const kid of e.children) {
      if (wears(kid, "dt") || wears(kid, "dl"))
        out.push(`${wears(kid, "dl") ? "dl" : "dt"}:${kid.textContent}`);
      else if (!wears(kid, "de")) walk(kid);
    }
  };
  walk(row);
  return out;
};
const docRows = () => flatRows().map((row) =>
  [kindOf(row.className)].concat(row.children
    .filter((p) => !wears(p, "de")).map((p) => p.textContent)));
/** Which element wears the cursor, and which of its CELLS.  Over the `dc' parts
 * alone: a headline's org-cleaned stars are chrome that `f'/`b' walk past. */
const docAt = () => flatRows().findIndex((row) => wears(row, "dat"));
const docCell = () => {
  const row = flatRows()[docAt()];
  if (!row) return -1;
  return row.children.filter((p) => wears(p, "dc")).findIndex((p) => wears(p, "don"));
};
const docFlagged = () => flatRows()
  .map((row, i) => (wears(row, "dfl") ? i : -1))
  .filter((i) => i !== -1);
/** THE PROPERTY PANEL, READ OFF WHAT IT DREW: there is no model here to ask. */
const listEls = (host) => field(host).querySelectorAll("tbody tr");
const listCells = (host) =>
  listEls(host).map((tr) => tr.children.map((td) => td.textContent));
const listAt = (host) => listEls(host).findIndex((tr) => wears(tr, "tv-sel"));
const listFlagged = (host) => listEls(host).filter((tr) => wears(tr, "tv-flagged"))
  .map((tr) => tr.getAttribute("data-id"));
const listHint = (host) =>
  (field(host).querySelector(".tv-hint") || { textContent: "" }).textContent;
const LISTS = ["ltable", "ttable", "mptable", "cstates"];
const narrowIn = (host) => field(host).querySelector("input.tv-filter");
const narrows = () => LISTS.map((h) => [h, narrowIn(h)])
  .filter(([, box]) => box).map(([h, box]) => [h, String(box.value)]);
const listCols = (host) =>
  field(host).querySelectorAll("thead .tv-hn").map((h) => h.textContent);
const panel = () => listCells("mptable");
const patAt = () => listAt("mptable");
const FOCUSABLE = ["mtext", "dtin", "dtext", "ltitle", "lurl", "tname",
                   "pinput", "ktag", "ktext"];
const focused = () => {
  if (!active) return "";
  // Drawn by the program that holds the rows, so it carries no id of its own.
  const list = LISTS.find((h) => narrowIn(h) === active);
  if (list) return `narrow:${list}`;
  const which = active === field("pkey") ? "pkey"
    : active === field("pval") ? "pval" : "";
  if (which) return `${which}:${patAt()}`;
  return FOCUSABLE.find((id) => active === field(id)) || "";
};
const parts = (e, cls) =>
  e.children.filter((x) => x.className.split(" ").indexOf(cls) !== -1);
const asWord = (word) => ({ key: "", word, color: "", hint: "", mark: "" });
/** One palette entry as drawn: the key token, the word with its BOLD letter
 * bracketed (`DELEGAT[E]D'), and the badge hue off the inline style. */
const paletteEntry = (e) => {
  const token = parts(e, "pk")[0], word = parts(e, "pw")[0], aside = parts(e, "pt")[0];
  const hot = word.children.find((p) => p.tagName === "B");
  return {
    // Empty for every entry but the one whose key names no position in a word.
    key: token ? token.textContent : "",
    word: word.children.length
      ? word.children.map((p) => (p.tagName === "B" ? `[${p.textContent}]`
                                                    : p.textContent)).join("")
      : word.textContent,
    color: word.style.color || "",
    mark: hot ? hot.style.textDecorationColor || "" : "",
    hint: aside ? aside.textContent : "",
  };
};
const paletteCell = (cell) =>
  cell.children.length ? cell.children.map(paletteEntry)
    : cell.textContent ? [asWord(cell.textContent)] : [];
/** The value palette's list, per ROW of `#plist'.  Three shapes: two cells,
 * ONE entry (the meta's or the fallback mode's body), or the standing line. */
const paletteRows = () => field("plist").children.map((row) => {
  const cells = parts(row, "pc"), own = parts(row, "pe")[0] || row;
  const [active, inactive] = cells.length ? cells.map(paletteCell)
    : [[parts(own, "pw").length ? paletteEntry(own) : asWord(row.textContent)], []];
  return {
    cls: row.className,
    source: (parts(row, "ps")[0] || {}).textContent || "",
    active,
    inactive,
  };
});

const logged = () => field("log").children.map((line) => ({
  sev: line.className,
  text: line.children.map((part) => part.textContent).filter(Boolean).join(" "),
}));

let assigned = [];

// What `cells' worked out: `FROM,TO', `«none»' for a key naming no column.
let span = null;

/** A stored value, `«unset»' for a key that is not there — a state apart from
 * one holding the empty string. */
const unset = (v) => (v === null ? "«unset»" : v);

const ACTIONS = {
  close: (reason) => { if (socket && socket.onclose) socket.onclose({ reason }); },
  // The glue is eval'd into this scope, so its own function answers here.
  assign: (arg) => {
    const labels = arg.split(",");
    assigned = whichKeys(labels).map((at, i) =>
      (at === -1 ? "-" : `${letterAt(labels[i], at)}@${at}`));
  },
  cells: (arg) => {
    const [keys, cols] = arg.split("@");
    const at = cellSpan(keys ? keys.split(",") : [],
                        (cols ? cols.split(",") : []).map((key) => ({ key })));
    span = at ? at.join(",") : "«none»";
  },
  stall: () => { stalling = true; },
  partly: () => { rowTags = { r1: ["web"], r2: ["web"], r3: [] }; },
  archived: (which) => {
    const want = new Set(String(which).split(",").filter(Boolean));
    // Off the same blob the page reads it from; a second spelling would drift.
    const tag = JSON.parse(CFGJSON).archiveTag;
    for (const r of rows)
      if (want.has(r.id)) r.cells.tag = `:web:${tag}:`;
  },
  untagged: () => { rowTags = { r1: [], r2: [], r3: [] }; },
  unknownrows: () => { rowTags = {}; },
  twotags: () => {
    sources = [
      { source: "default", active: ["TODO"], inactive: ["DONE"] },
      { source: "book", active: ["READING"], inactive: ["READ"] },
      { source: "film", active: ["WATCHING"], inactive: ["WATCHED"] },
    ];
  },
  sheet: (text) => { field("mtext").value = text; },
  filter: (text) => { field("filter").value = text; },
  commit: (text) => {
    if (!main) throw new Error("no table to commit a query to");
    const q = String(text).replace(/_/g, " ");
    main.held = q;
    field("filter").value = q;
    if (main.onFilter) main.onFilter(q);
  },
  moved: () => {
    step();
    rows = rows.concat([{ id: "r4", cells: { state: "TODO", title: "four", tag: "" } }]);
    served += 1;
  },
  recolumn: () => { step(); columns = columns.concat([{ key: "deadline" }]); },
  frame: (arg) => {
    const at = arg.indexOf("=");
    const op = at === -1 ? arg : arg.slice(0, at);
    if (op !== "upsert" && op !== "delete")
      throw new Error(`no such frame op: ${arg}`);
    if (!socket || !socket.onmessage)
      throw new Error(`no socket to carry a frame: frame:${arg}`);
    for (const id of (at === -1 ? "" : arg.slice(at + 1)).split(",").filter(Boolean)) {
      if (op === "delete") {
        // The frame FIRST, so an unfiltered client's own `deleteRow' takes the row
        // out and a shell that ignored the frame is visible in what is left.
        socket.onmessage({ data: JSON.stringify({ op: "delete-row", id }) });
        rows = rows.filter((r) => r.id !== id);
        served -= 1;
        step();
        continue;
      }
      const row = rows.concat(hidden).find((r) => r.id === id);
      if (!row) throw new Error(`no such row to upsert: ${id}`);
      step();   // a frame is a store that moved, so the tag moves with it
      socket.onmessage({ data: JSON.stringify({ op: "upsert-row", row }) });
    }
  },
  // It describes an APPLIED QUERY: an unfiltered client splices a frame back in.
  unserved: (arg) => {
    const ids = arg.split(",").filter(Boolean);
    hidden = hidden.concat(rows.filter((r) => ids.indexOf(r.id) !== -1));
    rows = rows.filter((r) => ids.indexOf(r.id) === -1);
    served -= ids.length;
    step();
  },
  rewritten: () => { digest = "d1"; },
  press: (key) => press(key),
  // The ONE thing that can move a cursor out from under an open edit overlay.
  click: (at) => {
    const i = Number(at);
    const host = field("modal").className === "on" ? "mptable"
      : field("links").className === "on" ? "ltable" : "ttable";
    const rows = listEls(host);
    if (!(i >= 0 && i < rows.length))
      throw new Error(`no row ${at} to click in ${host}`);
    rows[i].fire("click", { target: rows[i] });
  },
  theme: (name) => {
    const box = field("themesel");
    box.focus();
    box.value = name;
    box.fire("change", { target: box });
  },
  repeat: (key) => press(key, true),
  // A keydown with NO keyup and `repeat' UNSET: the native window's own quirk.
  stuck: (key) => press(key, false, true),
  type: (text) => {
    if (field("pbox").className !== "narrow")
      throw new Error("the value palette is not in its typing mode");
    const box = field("pinput");
    box.value = text;
    box.fire("input", { target: box });
  },
  narrow: (text) => {
    const host = LISTS.find((h) => narrowIn(h));
    if (!host) throw new Error("no list is narrowed: press / first");
    typed(narrowIn(host), String(text).replace(/_/g, " "));
  },
  tname: (text) => {
    if (field("tedit").className !== "on")
      throw new Error("no tag is open for renaming");
    typed(field("tname"), text);
  },
  pinclick: () => {
    if (!main.onPin) throw new Error("no onPin was wired: pinclick");
    main.onPin();
  },
  ktag: (text) => {
    if (field("capture").className !== "on")
      throw new Error("the capture form is not open: ktag");
    const box = field("ktag");
    box.focus();
    typed(box, text);
  },
  kf: (text) => {
    if (field("capture").className !== "on")
      throw new Error("the capture form is not open: kf");
    if (!active || active === field("ktag") || active === field("ktext"))
      throw new Error("no template field holds the focus: kf");
    typed(active, text);
  },
  ktext: (text) => {
    if (field("capture").className !== "on")
      throw new Error("the capture form is not open: ktext");
    const box = field("ktext");
    box.focus();
    typed(box, text);
  },
  ltitle: (text) => typeLink("ltitle", text),
  lurl: (text) => typeLink("lurl", text),
  dtin: (text) => typeIn("dtitle", "dtin", text),
  // `~' is a LITERAL bar, put back after the newlines so an org table row can be
  // typed into a paragraph spelling its line breaks with the same character.
  dpara: (text) => typeIn("dpara", "dtext",
    String(text).replace(/_/g, " ").replace(/\|/g, "\n").replace(/~/g, "|")),
  pkey: (arg) => typeOver("pkey", arg),
  pval: (arg) => typeOver("pval", arg),
  ctext: (text) => (onKeywords(), typeSetting("ctext", text)),
  // TAKING AN EDIT BACK: an act splits on spaces and a `#+TODO:' line is spaces.
  crevert: () => {
    onKeywords();
    const at = Number(field("clayer").value) || 0;
    const shown = layers.slice()
      .sort((a, b) => (a.tag === null ? 0 : 1) - (b.tag === null ? 0 : 1)
                   || String(a.tag).localeCompare(String(b.tag)))[at];
    typed(field("ctext"), (shown.lines || []).join("\n"));
  },
  clayer: (at) => {
    if (field("config").className !== "on")
      throw new Error("the settings sheet is not open: clayer");
    onKeywords();
    const box = field("clayer");
    box.focus();
    box.value = String(at);
    box.fire("change", { target: box });
  },
  // A box is typeable only while its own tab shows, two editors writing one cycle.
  ctab: (name) => {
    if (field("config").className !== "on")
      throw new Error("the settings sheet is not open: ctab");
    const tab = field("ctabs").children.find((t) => t.textContent === name);
    if (!tab) throw new Error(`no settings tab called ${name}`);
    tab.fire("click", {});
  },
  sat: (state) => {
    const rows = listEls("cstates");
    const row = rows.find((tr) => tr.children[1].textContent === state);
    if (!row) throw new Error(`no state row for ${state}`);
    row.fire("click", { target: row });
  },
  sfields: (spec) => {
    const [name, group, hue] = String(spec).split("/");
    if (name !== undefined && name !== "") field("sname").value = name;
    if (group !== undefined && group !== "") field("sgroup").value = group;
    if (hue !== undefined && hue !== "") field("shue").value = hue;
  },
  ccap: (text) => typeSetting("ctarget", text),
  clog: (text) => typeSetting("clog", text),
  cmoved: () => { for (const l of layers) l.digest = "gone"; },
  // A cell the store does not hold is what an entry with NO priority is, so the
  // ring's mixed set is one act.
  priorities: (arg) => {
    arg.split(",").forEach((p, i) => {
      if (!rows[i]) return;
      if (p) rows[i].cells.priority = `[#${p.toUpperCase()}]`;
      else delete rows[i].cells.priority;
      if (i === 0) headPriority = p ? `[#${p.toUpperCase()}]` : null;
    });
  },
  // Set before the sheet opens: the document is built out of the answer.
  grain: () => { grainy = true; },
  checky: () => { checky = true; },
  tabled: () => { tabled = true; },
  linky: () => { linky = true; links = linkyLinks; },
  grainlinks: () => {
    links = [ { target: "https://alpha.example/", desc: "in alpha",
                type: "https", span: [21, 40] },
              { target: "https://beta.example/", desc: "in beta",
                type: "https", span: [53, 58] } ];
  },
  refuse: () => { refusing = true; },
  noreferences: () => { unreferenced = true; },
  // `typing()' goes false again over a sheet still up, which no other act reaches.
  blur: () => { if (active) active.blur(); },
  // It STICKS, so a remount does not hand the calls back.
  bare: () => { markless = true; stripLive(MARK_CALLS); },
  pageless: () => { pagerless = true; stripLive(PAGE_CALLS); },
  sortless: () => { sortnone = true; stripLive(SORT_CALLS); },
  crumbless: () => { crumbless = true; stripLive(CRUMB_CALLS); },
  onelink: () => { links = links.slice(0, 1); },
  nolinks: () => { links = []; },
  onemailto: () => { links = links.slice(2, 3); },
  everytype: () => {
    links = [ { target: "https://a.example", desc: "secure", type: "https" },
              { target: "http://b.example", desc: "plain", type: "http" },
              { target: "org-glance-visit:XYZ", desc: "the other row", type: "glance" },
              { target: "mailto:t@example.org", desc: "write", type: "mailto" },
              { target: "id:99", desc: "org's own", type: "id" },
              { target: "file:notes.org", desc: "a file", type: "file" },
              { target: "Some Headline", desc: "Some Headline", type: "other" } ];
  },
  rows: (n) => {
    rows = Array.from({ length: Number(n) }, (_x, i) =>
      ({ id: `r${i + 1}`, cells: { state: "TODO", title: `row ${i + 1}`, tag: ":web:" } }));
    main.pageAt = 0;
    main.sit(0);
  },
  paged: (n) => { main.pageSize = Number(n); main.pageAt = 0; main.sit(0); },
  spam: (n) => {
    for (let i = 0; i < Number(n); i += 1) append("boot", "info", `line ${i}`);
  },
  offline: () => { down = true; },
  online: () => { down = false; },
  hang: () => { hanging = true; },
  deliver: () => {
    hanging = false;
    while (held.length) held.shift()();
  },
  chang: () => { changing = true; },
  cdeliver: () => {
    changing = false;
    while (cheld.length) cheld.shift()();
  },
  /** WAIT FOR THE THING: a duration cannot express "once the reconnect lands" —
   * the page's own backoff decides when that is and load moves it. */
  until: async (spec) => {
    const [what, want] = String(spec).split("=");
    const reads = { stale: () => (root.classList.contains("stale") ? "on" : "off") };
    const read = reads[what];
    if (!read) throw new Error(`no such condition: until:${spec}`);
    for (let turn = 0; turn < 400 && read() !== want; turn += 1)
      await new Promise((go) => realTimeout(go, 25));
  },
  // MS of the PAGE'S schedule: sleep the span, then let what fell due run.
  wait: async (ms) => {
    const until = Date.now() + Number(ms);
    await new Promise((done) => realTimeout(done, Number(ms)));
    await drainTo(until);
    await drainSoon();
  },
};

// Every fetch settles as a microtask, so one turn of the loop is past the whole
// boot; a close leads to a fetch which leads to a mount, each owed a turn.
const settle = async () => {
  await new Promise((done) => realTimeout(done, TURN));
  await drainSoon();
};
(async () => {
  await settle();
  for (const key of (keys || "").split(/\s+/).filter(Boolean)) press(key);
  await settle();
  for (const act of (acts || "").split(/\s+/).filter(Boolean)) {
    const at = act.indexOf(":");
    const verb = at === -1 ? act : act.slice(0, at);
    if (!ACTIONS[verb]) throw new Error(`no such act: ${act}`);
    await ACTIONS[verb](at === -1 ? "" : act.slice(at + 1));
    await settle();
  }
  await settle();
  const said = JSON.stringify({
    asked, tags, url: location.search, mounts, sets, raises,
    washed, stale: root.classList.contains("stale"),
    paints, spliced,
    sheet: field("mtext").value, state: field("mnote").className,
    modal: field("modal").className,
    palette: field("filter").value,
    doc: docRows(), dat: docAt(), dcol: docCell(), dflagged: docFlagged(),
    dopen: field("dtitle").className === "on",
    dparaopen: field("dpara").className === "on",
    dprows: field("mdoc").style.getPropertyValue("--g-doc-rows"),
    dtin: field("dtin").value,
    dtext: field("dtext").value,
    dcaret: field("dtext").selectionStart,
    where: field("mwhere").children.map((c) => c.textContent),
    whereAt: field("mwhere").children
      .map((c, i) => (wears(c, "wat") ? i : -1)).filter((i) => i !== -1),
    dactive: field("mdoc").className === "on",
    dindent: field("mdoc").style.getPropertyValue("--g-doc-indent"),
    dgrains: flatRows().map((row) => (wears(row, "d-comp") ? "composite"
      : wears(row, "d-item") ? "leaf" : "element")),
    downers: ownerOf(),
    dsegs: flatRows().map(segsOf),
    dhues: ["state", "priority"].map((key) => {
      const head = flatRows()[0];
      const cell = head && head.children.find((c) => wears(c, `dc-${key}`));
      return cell ? String(cell.style.color || "") : "";
    }),
    // A browser shows textContent and appended children side by side, so a cell
    // that drew segments must hold no raw text of its own.
    dtitleraw: (() => {
      const head = flatRows()[0];
      const cell = head && head.children.find((c) => wears(c, "dc-title"));
      return cell ? String(cell.textContent || "") : "";
    })(),
    // THE HARNESS'S OWN DOM, ASSERTED EACH RUN: a broken selector engine would
    // answer `null' forever while every case that never queries went on passing.
    dom: (() => {
      const box = make("div");
      box.className = "tv-root";
      // A DECOY FIRST, wearing the class but under no `tbody': without it a
      // chain that never checked its ancestors would answer this correctly.
      const decoy = box.appendChild(make("tr"));
      decoy.className = "tv-sel";
      decoy.textContent = "decoy";
      const table = box.appendChild(make("table"));
      table.className = "tv-table";
      const rows = table.appendChild(make("tbody"));
      ["tv-alt", "tv-sel", ""].forEach((cls, i) => {
        const tr = rows.appendChild(make("tr"));
        tr.className = cls;
        tr.appendChild(make("td")).className = "tv-box";
        const td = tr.appendChild(make("td"));
        td.textContent = `c${i}`;
      });
      const sel = box.querySelector("tbody tr.tv-sel");
      return {
        rows: box.querySelectorAll("tbody tr").length,
        sel: sel ? sel.textContent : null,
        gutterless: box.querySelectorAll("td:not(.tv-box)").map((td) => td.textContent),
        list: box.querySelectorAll("tr.tv-alt, tr.tv-sel").length,
        decoyed: box.querySelectorAll("tr.tv-sel").length,
        closest: !!sel && sel.closest(".tv-root") === box,
        matches: !!sel && sel.matches("tr.tv-sel") && !sel.matches("tr.tv-alt"),
        detached: box.parentNode === null,
        text: box.textContent,
      };
    })(),
    scrolled: scrolls.map((s) => s.className),
    scrollAsked: scrolls.length ? scrolls[scrolls.length - 1].opts : null,
    props: panel(), pat: patAt(), pnav: field("mprops").className === "on",
    pinits, pfills, pflagged: listFlagged("mptable"),
    pcols: listCols("mptable"), pflagHelp: listHint("mptable"),
    narrows: narrows(),
    focus: focused(),
    wroteAt, readAt,
    holding: active ? active.tagName : "",
    logbook: field("mlog").textContent,
    shape: field("sheet").className, writes,
    marksOn: main.marksOn, hintsOn: main.hintsOn, flagHelp: main.flagHelp,
    marked: [...main.marks], flagged: [...main.flags], cursor: main.at(),
    selected: main.at() === -1 ? null : main.onPage()[main.at()].id,
    col: main.selCol,
    page: main.pageAt + 1,
    echo: field("echo").textContent, echoes: field("echo").wrote,
    log: logged(),
    prompt: field("prompt").className, phead: field("phead").textContent,
    pmode: field("pbox").className, plist: paletteRows(), resolved,
    pfoot: field("pfoot").textContent, assigned, commands, span,
    linked, opened, sorted, sortCalls, chain: sortChain, tagged,
    pinned: main.pinned,
    capture: field("capture").className, khead: field("khead").textContent,
    ktag: field("ktag").value, ktext: field("ktext").value,
    kfields: field("kfields").children.map((row) => [
      (row.children[0] || {}).textContent || "",
      (row.children[1] || {}).value || "" ]),
    popup: field("links").className, lhead: field("lhead").textContent,
    lfoot: field("lfoot").textContent, lmounts,
    llinks: listCells("ltable"), lat: listAt("ltable"),
    lcols: listCols("ltable"), lflagged: listFlagged("ltable"),
    lflagHelp: listHint("ltable"),
    lopen: field("ledit").className === "on",
    ltitle: field("ltitle").value, lurl: field("lurl").value,
    tagpop: field("tags").className, thead: field("thead").textContent,
    tfoot: field("tfoot").textContent, tmounts, tsets,
    ttags: listCells("ttable"), tat: listAt("ttable"),
    tcols: listCols("ttable"), tflagged: listFlagged("ttable"),
    tflagHelp: listHint("ttable"),
    trename: field("tedit").className === "on", tname: field("tname").value,
    crumbs: main.crumbs.map((c) => c.label),
    prevented,
    settings: field("config").className, cstate: field("cnote").className,
    clayers: field("clayer").children.map((o) => o.textContent),
    cat: field("clayer").value, cshown: field("ctext").value,
    clab: field("clab").textContent, clerr: field("clerr").textContent,
    csecs: field("ctabs").children.map((t) => t.textContent),
    ctab: (field("ctabs").children.find((t) => t.className === "ctab on")
             || { textContent: "" }).textContent,
    ccap: field("ctarget").value,
    served: viewQuery, servedAgenda: agendaQuery,
    servedCapture: captureLine, capturing: captureAsked,
    chues: listCells("cstates").map((c) => c.join("|")),
    sat: listAt("cstates"), sflagged: listFlagged("cstates"),
    sedit: field("sedit").className,
    sfields: [field("sname").value, field("sgroup").value, field("shue").value],
    servedHues: stateHues,
    ctpl: field("ctpl").value,
    ceff: field("ceff").textContent, configWrites,
    clog: field("clog").value, logStored: unset(localStorage.getItem("glance-log")),
    logn: field("log").style.getPropertyValue("--g-logn"),
    theme: root.dataset.theme || "",
    themeStored: localStorage.getItem("glance-theme"),
  });
  // Exit on the write's own callback: a keystroke leaves the echo pill's timer
  // pending, and node would otherwise sit out its second and a half.
  process.stdout.write(said + "\n", () => process.exit(0));
})();
