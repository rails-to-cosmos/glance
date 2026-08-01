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
//   type:TEXT     TEXT typed into the value palette's field, which narrows it —
//                 `/' has to have put the palette in that mode first
//   assign:A,B,C  the which-key assignment run over that cycle, as the pure
//                 function it is
//   refuse        the next /command answers that every row was refused
//   bare          the mounted handle loses its mark calls, the way an older
//                 table-view.js never had them
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
// The state column carries its badge palette, since that is where the value
// palette C-c C-t raises gets its keywords, its colours and its groups from.
let columns = [
  { key: "state", badges: [ { value: "TODO", color: "#e0af68", group: "active" }
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
// The default view `system.org' names, which `g' applies and the settings sheet
// edits beside that layer's cycle.
let viewQuery = "state:*active*";

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
    return answer(200, {
      results: (sent.ids || []).map((id) =>
        refusing ? { id, ok: false, error: "a.org changed on disk" }
                 : { id, ok: true, digest: "d1" }),
    });
  }
  if (String(url) === "/config") {
    if ((init || {}).method !== "POST")
      return answer(200, { layers, filter: viewQuery,
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
    // The default view is a line of the same file, so it rides in the same
    // write and under the same digest — never a second request.
    if (sent.filter !== undefined) viewQuery = sent.filter;
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
// The hint a flagged row wears, which the renderer draws and the shell names.
let flagHelp = "";
/** The live handle, so `bare' can take calls off the one the shell is holding. */
let handle = null;
/** Set by `bare': this asset never had marking, remounts included. */
let markless = false;
globalThis.TableView = {
  mount: (_el, _view, options) => {
    mounts += 1;
    held = (options || {}).initialQuery || "";
    marksOn = (options || {}).marks === true;
    hintsOn = (options || {}).actionHints !== false;
    flagHelp = (options || {}).flagHelp || "";
    cursor = 0;
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
      getSelection: () => ({ id: rows.length ? rows[cursor].id : null, col: null }),
      getVisible: () => rows,
      // Clamped, never wrapped, and false at the end — which is what tells the
      // shell that a mark on the last row has nowhere to walk to.
      selectStep: (step) => {
        if (cursor + step < 0 || cursor + step >= rows.length) return false;
        cursor += step;
        return true;
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
    if (markless) strip();
    return handle;
  },
  parseQuery: () => [],
  displayText: (s) => String(s || ""),
};
/** The mark calls off the live handle: what an older table-view.js looks like. */
const MARK_CALLS = [ "toggleMark", "getMarked", "clearMarks", "markedCount"
                   , "markAll", "flagRow", "unflagRow", "getFlagged", "clearFlags" ];
const strip = () => { for (const name of MARK_CALLS) delete handle[name]; };
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
const TAGS = { mtext: "textarea", filter: "input", pinput: "input" };
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
                 , "log" ];
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
/**
 * The value palette's list as it stands: one entry per row, with the key token
 * it claimed and its word spelled with the underlined letter in brackets where
 * it sits (`DELEGAT[E]D').  A hairline is a row of its own, so the groups are
 * observable too.  The colour is read back off the inline style, which is where
 * the badge's own hue is written.
 */
const paletteRows = () => field("plist").children.map((row) => {
  // By CLASS, the way `patAt' reads the property panel: the producer labels the
  // token and the word, so a third part added later cannot be mistaken for
  // either.  A hairline has neither.
  const kid = (cls) =>
    row.children.find((e) => e.className.split(" ").indexOf(cls) !== -1);
  const token = kid("pk"), word = kid("pw");
  return {
    cls: row.className,
    key: token ? token.textContent : "",
    word: !word ? ""
      : word.children.length
        ? word.children.map((p) => (p.tagName === "U" ? `[${p.textContent}]`
                                                      : p.textContent)).join("")
        : word.textContent,
    color: word ? word.style.color || "" : "",
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
  // And the default view, which is the system layer's third child.
  cview: (arg) => typeInto("clayers", 2, arg),
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
  bare: () => { markless = true; strip(); },
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
    // The logbook strip: shown, never focusable, never written.
    logbook: field("mlog").textContent,
    shape: field("sheet").className, writes,
    // The renderer's side of marking, and the last thing the echo pill said —
    // which is where a key that could not do what it was asked reports it.
    marksOn, hintsOn, flagHelp, marked: [...marks], flagged: [...flags], cursor,
    echo: field("echo").textContent,
    // The event strip, which is append-only: what is here is everything the
    // page has said since it booted, oldest first.
    log: logged(),
    // The value palette: whether it is up, which mode it is in, what it is
    // setting, the entries it drew, the keys it names, and what a commit posted.
    prompt: field("prompt").className, phead: field("phead").textContent,
    pmode: field("pbox").className, plist: paletteRows(),
    pfoot: field("pfoot").textContent, assigned, commands,
    // Which keys the dispatch took off the browser, in press order.
    prevented,
    // The settings sheet: whether it is up, the one word it wears, the lines
    // each layer is showing, the union it previews, and every write it sent.
    settings: field("config").className, cstate: field("cnote").className,
    cshown: field("clayers").children.map((row) => row.children[1].value),
    // What the default-view field is showing, and what the server holds now.
    cview: ((field("clayers").children[0] || { children: [] }).children[2] || {}).value,
    served: viewQuery,
    ceff: field("ceff").textContent, configWrites,
  });
  // Exit on the write's own callback: a keystroke leaves the echo pill's timer
  // pending, and node would otherwise sit out its second and a half.
  process.stdout.write(said + "\n", () => process.exit(0));
})();
