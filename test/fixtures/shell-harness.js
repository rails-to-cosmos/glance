// Boots the shell's inline glue under node and reports what it asked the
// server for and what survived what happened to it next.  The glue is the
// page's own, extracted from a rendered `/' by TestServe; the browser around it
// is stubbed down to what a boot touches, so what this measures is the page's
// behaviour — which string-matching the glue cannot answer, since a call that
// is present and never reached matches just the same.
//
//   node shell-harness.js DIR SEARCH TOTAL [KEYS] [ACTS]
//
// DIR holds `shell.js' (the glue) and `keys.json' (the page's keymap blob).
// SEARCH is `location.search' the page opens on and TOTAL what the server
// reports as `X-Glance-Total', which is what decides whether the boot pulls
// the rest of the set in behind the first page.  KEYS is an optional
// space-separated list of `KeyboardEvent.key' names pressed over the table once
// the boot has settled.  Both overlays are opened through the page's own keys:
// `Enter' materializes the first row and `/' raises the filter palette.  ACTS
// is what happens after that, one verb at a time, each settled before the next:
//
//   close:REASON  the socket closes, the way the server closes one
//   sheet:TEXT    TEXT typed into the open sheet's textarea
//   pkey:I=TEXT   TEXT typed into property row I's key field, the row having
//                 been opened for editing first — a closed one has no fields
//   pval:I=TEXT   TEXT typed into property row I's value field, likewise
//   filter:TEXT   TEXT typed into the raised palette
//   moved         the store moves: a new ETag, and a row more to fetch
//   recolumn      the store moves and its columns move with it
//   rewritten     the file behind the open sheet moves: a new digest
//   press:KEY     KEY pressed, so a key can follow an act rather than precede
//                 it; `C-x' and `S-Tab' spell the modifiers
//   theme:NAME    NAME picked in the corner's theme select, event and all
//   type:TEXT     TEXT typed into the value palette's field, which narrows it —
//                 `/' has to have put the palette in that mode first
//   assign:A,B,C  the which-key assignment run over that cycle, as the pure
//                 function it is
//   refuse        the next /command refuses — every row it named, or the
//                 capture whole, which names none
//   bare          the mounted handle loses its mark calls, the way an older
//                 table-view.js never had them
//   pageless      and its pager calls, the way one older still never had those
//   rows:N        the store holds N rows rather than the three at the top
//   paged:N       the renderer shows N of them a page, so there are pages to
//                 turn and ends of a page to reach
//   spam:N        N distinct lines appended to the page's event log, which is
//                 the only way to reach a ring that holds five hundred
//   offline       the daemon goes away: every request after this fails
//
// The answer is what the page asked for and what it still holds afterwards.
const fs = require("fs");
const [dir, search, total, keys, acts] = process.argv.slice(2);

// Every /headlines URL the page asked for, in order, and the tags it sent with
// them — a revalidation is what a cheap reconnect looks like from the server.
const asked = [];
const tags = [];
// The store this harness stands in for, and the tag that says which version of
// it callers hold.  `moved' and `recolumn' step it.
// Three, because a walk needs somewhere to walk to: `m' marks and steps, so a
// one-row store cannot tell marking from advancing.
let rows = ["one", "two", "three"].map((title, i) =>
  ({ id: `r${i + 1}`, cells: { state: "TODO", title, tag: ":web:" } }));
// The state column carries its badge palette, which is where the value palette
// C-c C-t raises reads its COLOURS — the keywords themselves are /keywords'
// answer, and a keyword no badge names simply carries no hue.
let columns = [
  { key: "state", badges: [ { value: "TODO", color: "#e0af68", group: "active" }
                          , { value: "READING", color: "#bb9af7", group: "active" }
                          , { value: "DONE", color: "#73daca", group: "inactive" } ] },
  { key: "tag" },
];
let tag = "\"t0\"";
let served = +total;
// The subtree behind /headline, in the two shapes the route serves it in — the
// raw text, and the body with the drawer lifted out — plus the digest a write
// is pinned to.  The split is the server's, so what the sheet gets here is what
// a real one would hand it.
// `ORG_GLANCE_ID' is in the org text and NOT in the properties: it is a hidden
// key the server keeps for itself, so the panel never sees it and never sends
// it back.  The planning line and the logbook are the other two regions.
const org = "* TODO one\nSCHEDULED: <2026-08-01 Sat>\n:PROPERTIES:\n"
  + ":ORG_GLANCE_ID: r1\n:EFFORT: 0:30\n:END:\n:LOGBOOK:\n- moved here\n:END:\n";
const body = "* TODO one\n";
const properties = [["EFFORT", "0:30"]];
const planning = [["SCHEDULED", "<2026-08-01 Sat>"]];
const logbook = ":LOGBOOK:\n- moved here\n:END:\n";
let digest = "d0";
// Every POST /headline body, which is the whole of what a sync can be observed
// to have written: the rows come back over a socket this harness does not run.
const writes = [];
// Every structured command the page posted, as the body it sent — which is the
// whole of what a key like `D' can be observed to have done, the rows coming
// back over a socket this harness does not run.
const commands = [];
let refusing = false;
// The keyword layers behind /config, and every write to one.  The system layer
// carries no digest: it is a file that does not exist yet, which is the shape
// the settings sheet has to be able to create.
let layers = [
  { path: "/o/.org-glance/config/system.org", tag: null, lines: [], digest: "" },
  { path: "/o/.org-glance/config/tags/book.org", tag: "book",
    lines: ["#+TODO: TODO READING | READ"], digest: "c1" },
];
const configWrites = [];
let configTick = 1;
// What /keywords resolves for the rows a command names: the classification
// chain, nearest source first, each source holding what it is the nearest to
// declare.  Canned, the way the layers above are: the resolution is the
// server's and TestConfig is where the rule itself is tested — what the page
// owes is drawing whatever comes back, in the order it comes back.
let sources = [
  { source: "file",    active: ["LATER"],   inactive: [] },
  { source: "book",    active: ["READING"], inactive: ["READ"] },
  { source: "builtin", active: ["TODO"],    inactive: ["DONE"] },
];
// Every /keywords URL the page asked for, which is the whole of what says WHICH
// rows it resolved the palette for.  `stalling' holds one out forever.
const resolved = [];
let stalling = false;
// The default view `system.org' names, which `g' applies and the settings sheet
// edits beside that layer's cycle.
let viewQuery = "state:*active*";
// And the capture target it names, which is the other line of that file the
// sheet edits — plus the path the server resolves it to, which is what a
// capture reports back and the log names.
let captureLine = "";
const captureTarget = "/o/inbox.org";

globalThis.location = { search, protocol: "http:", host: "h", pathname: "/" };
globalThis.history = {
  // The page writes its applied query here; the search string it leaves behind
  // is the link a reload would come back to.
  replaceState: (_state, _title, url) => {
    location.search = String(url).startsWith("?") ? url : "";
  },
};
const answer = (status, body, headers) => Promise.resolve({
  ok: status >= 200 && status < 300,
  status,
  headers: { get: (name) => (headers || {})[String(name).toLowerCase()] || null },
  json: () => Promise.resolve(body),
  text: () => Promise.resolve(""),
});
// Set by `offline': the daemon is gone and every request fails at the network.
let down = false;
globalThis.fetch = (url, init) => {
  if (down) return Promise.reject(new Error("fetch failed"));
  const sent = ((init || {}).headers || {})["if-none-match"];
  if (String(url).startsWith("/headlines")) {
    asked.push(url);
    if (sent) tags.push(sent);
    // The server's own answer to a tag it still stands behind: no body at all.
    if (sent === tag) return answer(304, null, {});
    return answer(200, { title: "t", columns, rows },
                  { "x-glance-total": String(served), etag: tag });
  }
  if (String(url) === "/command") {
    const sent = JSON.parse((init || {}).body || "{}");
    commands.push(sent);
    // Capture names no row, so it answers in its own shape: the file the
    // server picked and the digest that file carries now.
    if (sent.name === "capture")
      return refusing
        ? answer(400, { error: "#+GLANCE_CAPTURE_TARGET: /x.org is an absolute path" })
        : answer(200, { ok: true, file: captureTarget, digest: "d1" });
    return answer(200, {
      results: (sent.ids || []).map((id) =>
        refusing ? { id, ok: false, error: "a.org changed on disk" }
                 : { id, ok: true, digest: "d1" }),
    });
  }
  if (String(url).startsWith("/keywords?ids=")) {
    resolved.push(url);
    // Never settling, which is the only way to observe the moment the overlay
    // is up and the resolution is not: everything else here answers as a
    // microtask, and one turn of the loop is past it.
    if (stalling) return new Promise(() => {});
    return refusing ? answer(400, { error: "GET /keywords?ids=<row id>" })
                    : answer(200, { sources, unknown: [] });
  }
  if (String(url) === "/config") {
    if ((init || {}).method !== "POST")
      return answer(200, { layers, filter: viewQuery, capture: captureLine,
                           keywords: { active: ["TODO"], inactive: ["DONE"] } });
    const sent = JSON.parse((init || {}).body || "{}");
    configWrites.push(sent);
    // The digest is the whole of the lock, an absent file's empty one included,
    // so a layer whose digest has moved refuses exactly as the server's does.
    const layer = layers.find((l) => l.path === sent.path);
    if (!layer || layer.digest !== sent.digest)
      return answer(409, { reason: "drift", digest: (layer || {}).digest || "",
                           error: "the config file changed on disk since it was read" });
    layer.lines = (sent.lines || []).filter(Boolean);
    // The default view and the capture target are lines of the same file, so
    // both ride in the same write under the same digest — never a second
    // request, which a second digest would refuse anyway.
    if (sent.filter !== undefined) viewQuery = sent.filter;
    if (sent.capture !== undefined) captureLine = sent.capture;
    layer.digest = `c${(configTick += 1)}`;
    return answer(200, { path: sent.path, digest: layer.digest });
  }
  if (String(url).startsWith("/headline?")) {
    if ((init || {}).method === "POST") {
      writes.push(JSON.parse((init || {}).body || "{}"));
      return answer(200, { digest });
    }
    return answer(200,
      { id: "r1", file: "a.org", org, body, properties, planning, logbook, digest });
  }
  return answer(404, {});
};
// The live socket, kept so a close can be delivered to it the way the server
// delivers one — with the reason that says which close it is.
let socket = null;
globalThis.WebSocket = function () {
  socket = this;
  this.close = () => { socket = null; };
};
// The renderer owns the applied query: it takes it at mount as chips and hands
// it back, and a strip takes the last token off it.  Enough of that here for
// the shell's own half of the round trip to be exercised.
let held = "";
let mounts = 0, sets = 0, raises = 0;
// The renderer's own state, which the shell keeps no copy of: where the cursor
// is, whether it was asked for marks, and which ids carry one.
let cursor = 0, marksOn = false, hintsOn = true, marks = new Set(), flags = new Set();
// The other two halves of that state: which column the cursor sits in, and
// which page the renderer is showing.  `pageAt' counts from zero and `pageSize'
// is 0 for a set with no pages; `cursor' indexes the page rather than the set,
// which is the same thing while there is one page.
let selCol = null, pageAt = 0, pageSize = 0;
/** The rows on show: one page's worth, or the whole set when there are none. */
const onPage = () =>
  (pageSize ? rows.slice(pageAt * pageSize, (pageAt + 1) * pageSize) : rows);
const pageMax = () => (pageSize ? Math.max(1, Math.ceil(rows.length / pageSize)) : 1);
/**
 * Turn to page TO, counting from zero, landing the cursor on the end it
 * arrives at — FIRST says which.  The column rides across untouched, which is
 * what lets the shell read it back rather than carry it.  False when there is
 * no such page, which is how a stop at either end is told from a turn.
 */
const pageTo = (to, first) => {
  const at = Math.max(0, Math.min(pageMax() - 1, to));
  if (at === pageAt) return false;
  pageAt = at;
  cursor = first ? 0 : Math.max(0, onPage().length - 1);
  return true;
};
// The hint a flagged row wears, which the renderer draws and the shell names.
let flagHelp = "";
/** The live handle, so `bare' can take calls off the one the shell is holding. */
let handle = null;
/** Set by `bare' and `pageless': this asset never had those calls, remounts
 * included. */
let markless = false, pagerless = false;
globalThis.TableView = {
  mount: (_el, _view, options) => {
    mounts += 1;
    held = (options || {}).initialQuery || "";
    marksOn = (options || {}).marks === true;
    hintsOn = (options || {}).actionHints !== false;
    flagHelp = (options || {}).flagHelp || "";
    // The page size is the mount's, the way the real one takes it, so a script
    // that never asks for pages gets the one the shell always requests.
    pageSize = (options || {}).pageSize || 0;
    cursor = 0;
    selCol = null;
    pageAt = 0;
    marks = new Set();
    flags = new Set();
    handle = {
      setRows: () => { sets += 1; },
      getQuery: () => held,
      stripLastToken: () => {
        if (!held) return false;
        held = held.split(/\s+/).slice(0, -1).join(" ");
        return true;
      },
      // The selection is the renderer's, both halves of it, and the shell reads
      // the row id back out of here to materialize one.
      getSelection: () => ({ id: onPage().length ? onPage()[cursor].id : null, col: selCol }),
      getVisible: () => onPage(),
      // Clamped, never wrapped, and false at the end — which is what tells the
      // shell that a mark on the last row has nowhere to walk to.
      selectStep: (step) => {
        if (cursor + step < 0 || cursor + step >= onPage().length) return false;
        cursor += step;
        return true;
      },
      // A row of the page in hand, and the column to land in.  Null is a
      // WHOLE-ROW selection, the way the real one's `clampCol' reads it, so a
      // caller meaning to keep the column has to hand it back.  False for a row
      // this page is not showing.
      select: (id, col) => {
        const at = onPage().findIndex((r) => r.id === id);
        if (at === -1) return false;
        cursor = at;
        selCol = col === null || col === undefined ? null : col;
        return true;
      },
      // The pager, landing the cursor on the end it arrives at — the new page's
      // first row going forward, its last coming back.
      nextPage: () => pageTo(pageAt + 1, true),
      previousPage: () => pageTo(pageAt - 1, false),
      pageInfo: () => {
        const size = pageSize || rows.length;
        return { page: pageAt + 1, pages: pageMax(),
                 from: rows.length ? pageAt * size + 1 : 0,
                 to: Math.min(rows.length, (pageAt + 1) * size), total: rows.length };
      },
      // Marks are the renderer's, keyed by id.
      toggleMark: (id) => {
        const on = !marks.has(id);
        if (on) marks.add(id); else marks.delete(id);
        return on;
      },
      getMarked: () => [...marks],
      clearMarks: () => marks.clear(),
      markedCount: () => marks.size,
      markAll: () => { for (const r of rows) marks.add(r.id); },
      // Archive flags, keyed by id the way marks are: `d' puts one on and a
      // second `d' on the same row is what archives it.
      flagRow: (id) => flags.add(id),
      unflagRow: (id) => flags.delete(id),
      getFlagged: () => [...flags],
      clearFlags: () => flags.clear(),
      // What the renderer's palette does: the overlay goes up and its field
      // takes focus, which is the whole of what the shell can see of it.
      openFilter: () => { raises += 1; field("filter").focus(); },
    };
    if (markless) strip(MARK_CALLS);
    if (pagerless) strip(PAGE_CALLS);
    return handle;
  },
  parseQuery: () => [],
  displayText: (s) => String(s || ""),
};
/** The mark calls off the live handle: what an older table-view.js looks like. */
const MARK_CALLS = [ "toggleMark", "getMarked", "clearMarks", "markedCount"
                   , "markAll", "flagRow", "unflagRow", "getFlagged", "clearFlags" ];
/** And the pager's, which an asset that old has none of either. */
const PAGE_CALLS = ["nextPage", "previousPage", "pageInfo"];
const strip = (names) => { for (const name of names) delete handle[name]; };
globalThis.localStorage = { getItem: () => null, setItem: () => {} };
globalThis.matchMedia = () => ({ matches: false, addEventListener: () => {} });

// One element that answers to anything: the boot reads and writes chrome this
// harness has no opinion about, and the keymap blob is the one thing it has to
// hand back for real.
const KEYS = fs.readFileSync(dir + "/keys.json", "utf8");
const node = new Proxy(
  {},
  {
    get: (_target, key) =>
      key === "textContent" || key === "className" || key === "value" ? ""
        : key === "scrollTop" || key === "clientHeight" || key === "scrollHeight" ? 0
        : () => node,
    set: () => true,
  }
);
// The few elements whose contents are the answer to a question asked here: the
// sheet's two panes and its one-word state, and the renderer's filter field.  A
// proxy answering "" to everything cannot hold text a restore is checked
// against, cannot hold a tree the property panel is built into, and
// `document.activeElement' is what tells a raised palette from a committed
// query.
let active = null;
const fields = {};
// The tag matters: `typing()' reads it off `document.activeElement' to decide
// whether a key belongs to the table or to whatever has focus.
const TAGS = { mtext: "textarea", filter: "input", pinput: "input",
               themesel: "select" };
/** A stand-in element, enough of one for the page to build its own chrome in. */
const make = (tag) => {
  const e = {
    tagName: String(tag).toUpperCase(),
    value: "", className: "", placeholder: "", spellcheck: false,
    style: {}, dataset: {}, children: [],
    scrollTop: 0, clientHeight: 0, scrollHeight: 0,
    focus() { active = this; },
    blur() { if (active === this) active = null; },
    // Kept rather than dropped: the value palette narrows on its field's own
    // `input' event, and the property panel grows a row on one — neither of
    // which a document-level press can stand in for.
    on: {},
    addEventListener(type, fn) { (this.on[type] = this.on[type] || []).push(fn); },
    fire(type, event) { for (const fn of this.on[type] || []) fn(event); },
    appendChild(child) { this.children.push(child); return child; },
    // What the log's ring drops the oldest line with.
    removeChild(child) {
      const at = this.children.indexOf(child);
      if (at !== -1) this.children.splice(at, 1);
      return child;
    },
    select() {},
  };
  // The real one drops every child when it is set, which is how the panel is
  // cleared before it is drawn again.
  let text = "";
  Object.defineProperty(e, "textContent", {
    get: () => text,
    set: (v) => { text = String(v); e.children.length = 0; },
  });
  return e;
};
const field = (id) => (fields[id] = fields[id] || make(TAGS[id] || "div"));
const STATEFUL = [ "mtext", "mnote", "mfile", "modal", "mprops", "mlog", "sheet"
                 // The value palette: its list is a tree of key tokens and
                 // underlined words, so it has to hold one.
                 , "echo", "prompt", "phead", "pinput", "pbox", "plist", "pfoot"
                 , "config", "cnote", "clayers", "ceff"
                 // The event strip: a line per entry, each a row of spans, so it
                 // has to hold a tree rather than answer "" to everything.
                 , "log"
                 // The corner's theme select, which has to be a real element
                 // for the focus it takes and gives back to be observable.
                 , "themesel" ];
// The page's own key dispatch, kept so a press can be delivered to it.
const pressed = [];
globalThis.document = {
  getElementById: (id) =>
    id === "keys" ? { textContent: KEYS }
      : STATEFUL.indexOf(id) === -1 ? node : field(id),
  querySelector: (sel) => (sel === "#app .tv-filter" ? field("filter") : null),
  querySelectorAll: () => [],
  createElement: (tag) => make(tag),
  addEventListener: (type, handler) => {
    if (type === "keydown") pressed.push(handler);
  },
  getSelection: () => null,
  get activeElement() { return active; },
  documentElement: node,
  body: node,
};
globalThis.window = globalThis;
globalThis.addEventListener = () => {};

eval(fs.readFileSync(dir + "/shell.js", "utf8"));

// A `C-' prefix is the chord the page's own `keyName' spells that way, so a
// sequence like `C-c C-t' is two of these and needs no other notation here.
// `S-' is the shift held with it — `S-Tab' is the crossing back out of the
// sheet's property panel, which the page tells from `Tab' by the modifier
// alone.
//
// Whether the dispatch CLAIMED a key is recorded, because that is the half of
// the reserved-chord rule behaviour can otherwise not show: a chord the page
// leaves to the browser and one it takes both look like nothing happening.
const press = (name, repeating) => {
  const ctrl = name.startsWith("C-"), shift = name.startsWith("S-");
  const event = {
    key: ctrl || shift ? name.slice(2) : name,
    ctrlKey: ctrl, altKey: false, metaKey: false, shiftKey: shift,
    repeat: !!repeating, target: node, preventDefault: () => prevented.push(name),
  };
  for (const handler of pressed) handler(event);
};
const prevented = [];

// The store moving is a new tag: a client holding the old one is answered with
// a body rather than a 304, which is the reconnect that has rows to apply.
const step = () => { tag = `"t${Number(tag.slice(2, -1)) + 1}"`; };
/**
 * WHICH field of the row ARG names inside the panel ID, given ARG's
 * `INDEX=TEXT', typed into.  Both panels this page builds are rows of fields,
 * so one act serves the property drawer and the settings layers alike.
 */
const typeInto = (id, which, arg) => {
  const at = arg.indexOf("=");
  const row = field(id).children[Number(arg.slice(0, at))];
  if (!row) throw new Error(`no ${id} row ${arg}`);
  const box = row.children[which];
  // A property row is read-only text until it is opened, so typing into a
  // closed one is a script that means nothing: say so rather than write into a
  // cell nobody can see.
  if (box.tagName !== "INPUT" && box.tagName !== "TEXTAREA")
    throw new Error(`${id} row ${arg} is not open for editing`);
  box.value = arg.slice(at + 1);
  box.fire("input", { target: box });
};
/**
 * The property panel as it stands: a [key, value] pair per row it is showing.
 * A closed row shows text and an open one holds fields, and the pair reads the
 * same either way — which is what makes one assertion cover both modes.
 */
const shown = (e) => (e.tagName === "INPUT" ? e.value : e.textContent);
const panel = () =>
  field("mprops").children.map((row) => [shown(row.children[0]), shown(row.children[1])]);
/** Which row wears the panel's cursor, and -1 when none does. */
const patAt = () => field("mprops").children
  .findIndex((row) => row.className.split(" ").indexOf("pat") !== -1);
/**
 * Which field of the sheet has the focus, named the way an act names one:
 * `mtext' for the body pane, and the panel's own class over the row index for a
 * panel field (`pkey:1', `pval:1').  A focus call moves nothing else, so this
 * is the whole of what the sheet's navigation can be observed to have done.
 */
const focused = () => {
  if (!active) return "";
  if (active === field("mtext")) return "mtext";
  const at = field("mprops").children
    .findIndex((row) => row.children.indexOf(active) !== -1);
  return at === -1 ? "" : `${active.className}:${at}`;
};
/** Everything under E with CLS, by class the way `patAt' reads the property
 * panel: the producer labels each part, so one added later cannot be mistaken
 * for another. */
const parts = (e, cls) =>
  e.children.filter((x) => x.className.split(" ").indexOf(cls) !== -1);
/** A bare word where an entry is read: the header's column names, and the line
 * the palette stands on while the resolution is out. */
const asWord = (word) => ({ key: "", word, color: "" });
/**
 * One palette entry as it is drawn: the key token it claimed, its word spelled
 * with the BOLD letter in brackets where it sits (`DELEGAT[E]D'), and the
 * colour read back off the inline style, which is where the badge's own hue is
 * written.
 */
const paletteEntry = (e) => {
  const token = parts(e, "pk")[0], word = parts(e, "pw")[0];
  return {
    key: token ? token.textContent : "",
    word: word.children.length
      ? word.children.map((p) => (p.tagName === "B" ? `[${p.textContent}]`
                                                    : p.textContent)).join("")
      : word.textContent,
    color: word.style.color || "",
  };
};
/** One table cell's entries.  The header's cells hold a word rather than
 * entries and read as one; an empty cell reads as nothing. */
const paletteCell = (cell) =>
  cell.children.length ? cell.children.map(paletteEntry)
    : cell.textContent ? [asWord(cell.textContent)] : [];
/**
 * The value palette's list as it stands: per ROW of `#plist', its class, the
 * source it names, and the entries in its Active and Inactive halves.  The
 * hairlines between rows are the rows' own borders, so what is observable here
 * is the table's shape rather than a divider.
 *
 * Three row shapes, told apart by what the producer put in them.  A table row
 * carries its two cells.  A row holding ONE entry is the meta's, or the
 * fallback mode's own body, and reads as that entry in the active half.  A row
 * holding neither is the standing line, and reads as its text.
 */
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

/**
 * The log strip as it stands: a line's severity class and the text it renders,
 * the parts joined by the space that separates them on screen.  The repeat
 * counter is empty until a line repeats, which is why the empty parts go.
 */
const logged = () => field("log").children.map((line) => ({
  sev: line.className,
  text: line.children.map((part) => part.textContent).filter(Boolean).join(" "),
}));

// What `assign' worked out, as `LETTER@INDEX' per entry and `-' for one that
// claimed nothing.
let assigned = [];

const ACTIONS = {
  close: (reason) => { if (socket && socket.onclose) socket.onclose({ reason }); },
  // The which-key assignment driven as the pure function it is: a comma-separated
  // cycle in, the claimed letters out.  The glue is eval'd into this scope, so
  // its own function is what answers — no second copy of the rule here.
  assign: (arg) => {
    const labels = arg.split(",");
    assigned = whichKeys(labels).map((at, i) =>
      (at === -1 ? "-" : `${letterAt(labels[i], at)}@${at}`));
  },
  // The resolution never arrives, which is what leaves the palette standing in
  // the state between the press that raised it and the answer that fills it.
  stall: () => { stalling = true; },
  // The resolution a marked set spanning two tags comes back as: two tag
  // sources, in the order the server put them, and no file layer at all.
  twotags: () => {
    sources = [
      { source: "book", active: ["READING"], inactive: ["READ"] },
      { source: "film", active: ["WATCHING"], inactive: ["WATCHED"] },
      { source: "builtin", active: ["TODO"], inactive: ["DONE"] },
    ];
  },
  sheet: (text) => { field("mtext").value = text; },
  filter: (text) => { field("filter").value = text; },
  moved: () => {
    step();
    rows = rows.concat([{ id: "r4", cells: { state: "TODO", title: "four", tag: "" } }]);
    served += 1;
  },
  recolumn: () => { step(); columns = columns.concat([{ key: "deadline" }]); },
  rewritten: () => { digest = "d1"; },
  press: (key) => press(key),
  // The corner's theme select, driven the way a reader drives it: focus it,
  // pick a theme, and let the change event fire.  What it is here to show is
  // what happens AFTER — whether the control keeps the keys or gives them back.
  theme: (name) => {
    const box = field("themesel");
    box.focus();
    box.value = name;
    box.fire("change", { target: box });
  },
  // The same key delivered as an AUTO-REPEAT, which is what the ONCE list is
  // about: the dispatch claims it either way and runs it only when it is not
  // one of the commands a hold must not repeat.
  repeat: (key) => press(key, true),
  // The field is the fallback mode's and is hidden until `/' raises it, so a
  // script that types without pressing `/' first is typing into nothing on a
  // real page: say so rather than narrow a list no reader could have narrowed.
  type: (text) => {
    if (field("pbox").className !== "narrow")
      throw new Error("the value palette is not in its typing mode");
    const box = field("pinput");
    box.value = text;
    box.fire("input", { target: box });
  },
  // Typing into the property panel: `pkey:1=EFFORT' is the key field of row 1,
  // `pval:1=0:45' its value.  The `input' event is the whole point — the panel
  // grows its next empty row on one.
  pkey: (arg) => typeInto("mprops", 0, arg),
  pval: (arg) => typeInto("mprops", 1, arg),
  // And into the settings sheet: `ctext:0=#+TODO: A | B' is the box of layer 0,
  // which is the file's `#+TODO:' lines as the sheet edits them.
  ctext: (arg) => typeInto("clayers", 1, arg),
  // And the default view, which is the system layer's third child, and the
  // capture target, which is its fourth.
  cview: (arg) => typeInto("clayers", 2, arg),
  ccap: (arg) => typeInto("clayers", 3, arg),
  // Every config layer moves out from under the sheet, which is the drift a
  // second writer causes.
  cmoved: () => { for (const l of layers) l.digest = "gone"; },
  refuse: () => { refusing = true; },
  // A click on an open sheet's own chrome — its header, its file line — takes
  // the focus off whatever field had it without closing anything.  That is when
  // `typing()' goes false again and every `table' row comes back to life over a
  // sheet that is still up, which no other act reaches.
  blur: () => { if (active) active.blur(); },
  // An asset that never had marking: the calls are simply not on the handle,
  // which is the shape the shell's feature detection is written against. It
  // sticks, so a remount later in the same script does not hand them back and
  // quietly turn the fallback case into the ordinary one.
  bare: () => { markless = true; strip(MARK_CALLS); },
  // And one that never had paging, which is what leaves the buffer-end keys
  // their within-page half and nothing to climb with.
  pageless: () => { pagerless = true; strip(PAGE_CALLS); },
  // A store with pages in it: N rows in place of the three at the top, and the
  // renderer showing SIZE of them at a time.  Acts rather than argv, so every
  // script that wants neither reads exactly as it did.
  rows: (n) => {
    rows = Array.from({ length: Number(n) }, (_x, i) =>
      ({ id: `r${i + 1}`, cells: { state: "TODO", title: `row ${i + 1}`, tag: ":web:" } }));
    cursor = 0;
    pageAt = 0;
  },
  paged: (n) => { pageSize = Number(n); cursor = 0; pageAt = 0; },
  // N distinct lines through the page's own `append': the glue is eval'd into
  // this scope, so its functions are reachable from here.  The ring holds five
  // hundred and nothing a key presses writes them faster than one at a time, so
  // a script that overran it any other way would be longer than the cap.
  spam: (n) => {
    for (let i = 0; i < Number(n); i += 1) append("boot", "info", `line ${i}`);
  },
  // The daemon goes away: every request after this fails at the network, which
  // is what the reconnect's error line and the retry behind it are written for.
  offline: () => { down = true; },
};

// Every fetch here settles as a microtask, so one turn of the event loop is
// past the whole boot — the arming fetch chained behind the set included.  The
// keys go in after that, then the acts one at a time, and the answer last: a
// close leads to a fetch which leads to a mount, and each of those needs its
// own turn before the next act can mean anything.
const settle = () => new Promise((done) => setTimeout(done, 20));
(async () => {
  await settle();
  for (const key of (keys || "").split(/\s+/).filter(Boolean)) press(key);
  await settle();
  for (const act of (acts || "").split(/\s+/).filter(Boolean)) {
    const at = act.indexOf(":");
    const verb = at === -1 ? act : act.slice(0, at);
    if (!ACTIONS[verb]) throw new Error(`no such act: ${act}`);
    ACTIONS[verb](at === -1 ? "" : act.slice(at + 1));
    await settle();
  }
  await settle();
  const said = JSON.stringify({
    asked, tags, url: location.search, mounts, sets, raises,
    sheet: field("mtext").value, state: field("mnote").className,
    modal: field("modal").className,
    palette: field("filter").value,
    // The sheet's other pane: every row the panel is showing, where its cursor
    // is, whether it is the thing holding the keys, which field the focus is on
    // if any, the lines it puts under a row, which shape the sheet is in, and
    // every POST the syncs sent.
    props: panel(), pat: patAt(), pnav: field("mprops").className === "on",
    focus: focused(),
    // What holds the keyboard, as its tag — empty for nothing, which is the
    // state the table's own keys are live in.
    holding: active ? active.tagName : "",
    // The logbook strip: shown, never focusable, never written.
    logbook: field("mlog").textContent,
    shape: field("sheet").className, writes,
    // The renderer's side of marking, and the last thing the echo pill said —
    // which is where a key that could not do what it was asked reports it.
    marksOn, hintsOn, flagHelp, marked: [...marks], flagged: [...flags], cursor,
    // Where the cursor is in terms a page-local index cannot give: the row it
    // sits on, the column it is in, and the page it is reading.
    selected: onPage().length ? onPage()[cursor].id : null, col: selCol,
    page: pageAt + 1,
    echo: field("echo").textContent,
    // The event strip, which is append-only: what is here is everything the
    // page has said since it booted, oldest first.
    log: logged(),
    // The value palette: whether it is up, which mode it is in, what it is
    // setting, the resolution it drew, which rows it resolved for, the keys it
    // names, and what a commit posted.
    prompt: field("prompt").className, phead: field("phead").textContent,
    pmode: field("pbox").className, plist: paletteRows(), resolved,
    pfoot: field("pfoot").textContent, assigned, commands,
    // Which keys the dispatch took off the browser, in press order.
    prevented,
    // The settings sheet: whether it is up, the one word it wears, the lines
    // each layer is showing, the union it previews, and every write it sent.
    settings: field("config").className, cstate: field("cnote").className,
    cshown: field("clayers").children.map((row) => row.children[1].value),
    // What the two tree-wide fields are showing, and what the server holds now.
    cview: ((field("clayers").children[0] || { children: [] }).children[2] || {}).value,
    ccap: ((field("clayers").children[0] || { children: [] }).children[3] || {}).value,
    served: viewQuery, servedCapture: captureLine,
    ceff: field("ceff").textContent, configWrites,
  });
  // Exit on the write's own callback: a keystroke leaves the echo pill's timer
  // pending, and node would otherwise sit out its second and a half.
  process.stdout.write(said + "\n", () => process.exit(0));
})();
