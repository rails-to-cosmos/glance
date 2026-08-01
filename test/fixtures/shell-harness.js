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
//   pkey:I=TEXT   TEXT typed into property row I's key field
//   pval:I=TEXT   TEXT typed into property row I's value field
//   filter:TEXT   TEXT typed into the raised palette
//   moved         the store moves: a new ETag, and a row more to fetch
//   recolumn      the store moves and its columns move with it
//   rewritten     the file behind the open sheet moves: a new digest
//   press:KEY     KEY pressed, so a key can follow an act rather than precede it
//   type:TEXT     TEXT typed into the raised value palette, which narrows it
//   refuse        the next /command answers that every row was refused
//   bare          the mounted handle loses its mark calls, the way an older
//                 table-view.js never had them
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
// palette C-c C-t raises gets its keywords from.
let columns = [
  { key: "state", badges: [{ value: "TODO" }, { value: "DONE" }] },
  { key: "tag" },
];
let tag = "\"t0\"";
let served = +total;
// The subtree behind /headline, in the two shapes the route serves it in — the
// raw text, and the body with the drawer lifted out — plus the digest a write
// is pinned to.  The split is the server's, so what the sheet gets here is what
// a real one would hand it.
const org = "* TODO one\n:PROPERTIES:\n:ORG_GLANCE_ID: r1\n:EFFORT: 0:30\n:END:\n";
const body = "* TODO one\n";
const properties = [["ORG_GLANCE_ID", "r1"], ["EFFORT", "0:30"]];
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
globalThis.fetch = (url, init) => {
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
      return answer(200, { layers, keywords: { active: ["TODO"], inactive: ["DONE"] } });
    const sent = JSON.parse((init || {}).body || "{}");
    configWrites.push(sent);
    // The digest is the whole of the lock, an absent file's empty one included,
    // so a layer whose digest has moved refuses exactly as the server's does.
    const layer = layers.find((l) => l.path === sent.path);
    if (!layer || layer.digest !== sent.digest)
      return answer(409, { reason: "drift", digest: (layer || {}).digest || "",
                           error: "the config file changed on disk since it was read" });
    layer.lines = (sent.lines || []).filter(Boolean);
    layer.digest = `c${(configTick += 1)}`;
    return answer(200, { path: sent.path, digest: layer.digest });
  }
  if (String(url).startsWith("/headline?")) {
    if ((init || {}).method === "POST") {
      writes.push(JSON.parse((init || {}).body || "{}"));
      return answer(200, { digest });
    }
    return answer(200, { id: "r1", file: "a.org", org, body, properties, digest });
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
let cursor = 0, marksOn = false, marks = new Set();
/** The live handle, so `bare' can take calls off the one the shell is holding. */
let handle = null;
/** Set by `bare': this asset never had marking, remounts included. */
let markless = false;
globalThis.TableView = {
  mount: (_el, _view, options) => {
    mounts += 1;
    held = (options || {}).initialQuery || "";
    marksOn = (options || {}).marks === true;
    cursor = 0;
    marks = new Set();
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
const MARK_CALLS = ["toggleMark", "getMarked", "clearMarks", "markedCount"];
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
const STATEFUL = [ "mtext", "mnote", "mfile", "modal", "mprops", "sheet"
                 , "echo", "prompt", "phead", "pinput"
                 , "config", "cnote", "clayers", "ceff" ];
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
//
// Whether the dispatch CLAIMED a key is recorded, because that is the half of
// the reserved-chord rule behaviour can otherwise not show: a chord the page
// leaves to the browser and one it takes both look like nothing happening.
const press = (name) => {
  const ctrl = name.startsWith("C-");
  const event = {
    key: ctrl ? name.slice(2) : name,
    ctrlKey: ctrl, altKey: false, metaKey: false, shiftKey: false,
    repeat: false, target: node, preventDefault: () => prevented.push(name),
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
  box.value = arg.slice(at + 1);
  box.fire("input", { target: box });
};
/** The property panel as it stands: a [key, value] pair per row it is showing. */
const panel = () =>
  field("mprops").children.map((row) => [row.children[0].value, row.children[1].value]);
const ACTIONS = {
  close: (reason) => { if (socket && socket.onclose) socket.onclose({ reason }); },
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
  type: (text) => {
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
    // The sheet's other pane: every row the panel is showing, the lines it puts
    // under one, which shape it is in, and every POST the syncs sent.
    props: panel(),
    notes: field("mprops").children.map((row) => row.children[2].textContent)
      .filter(Boolean),
    shape: field("sheet").className, writes,
    // The renderer's side of marking, and the last thing the echo pill said —
    // which is where a key that could not do what it was asked reports it.
    marksOn, marked: [...marks], cursor, echo: field("echo").textContent,
    // The value palette, and what the keys posted through it.
    prompt: field("prompt").className, phead: field("phead").textContent, commands,
    // Which keys the dispatch took off the browser, in press order.
    prevented,
    // The settings sheet: whether it is up, the one word it wears, the lines
    // each layer is showing, the union it previews, and every write it sent.
    settings: field("config").className, cstate: field("cnote").className,
    cshown: field("clayers").children.map((row) => row.children[1].value),
    ceff: field("ceff").textContent, configWrites,
  });
  // Exit on the write's own callback: a keystroke leaves the echo pill's timer
  // pending, and node would otherwise sit out its second and a half.
  process.stdout.write(said + "\n", () => process.exit(0));
})();
