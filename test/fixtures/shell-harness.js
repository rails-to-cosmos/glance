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
//   sheet:TEXT    TEXT typed into the open sheet
//   filter:TEXT   TEXT typed into the raised palette
//   moved         the store moves: a new ETag, and a row more to fetch
//   recolumn      the store moves and its columns move with it
//   rewritten     the file behind the open sheet moves: a new digest
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
let rows = [{ id: "r1", cells: { state: "TODO", title: "one", tag: ":web:" } }];
let columns = [{ key: "state" }, { key: "tag" }];
let tag = "\"t0\"";
let served = +total;
// The subtree behind /headline, and the digest a write is pinned to.
const org = "* TODO one\n";
let digest = "d0";

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
  if (String(url).startsWith("/headline?")) {
    if ((init || {}).method === "POST") return answer(200, { digest });
    return answer(200, { id: "r1", file: "a.org", org, digest });
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
globalThis.TableView = {
  mount: (_el, _view, options) => {
    mounts += 1;
    held = (options || {}).initialQuery || "";
    return {
      setRows: () => { sets += 1; },
      getQuery: () => held,
      stripLastToken: () => {
        if (!held) return false;
        held = held.split(/\s+/).slice(0, -1).join(" ");
        return true;
      },
      // The selection is the renderer's, both halves of it, and the shell reads
      // the row id back out of here to materialize one.
      getSelection: () => ({ id: rows.length ? rows[0].id : null, col: null }),
      getVisible: () => rows,
      // What the renderer's palette does: the overlay goes up and its field
      // takes focus, which is the whole of what the shell can see of it.
      openFilter: () => { raises += 1; field("filter").focus(); },
    };
  },
  parseQuery: () => [],
  displayText: (s) => String(s || ""),
};
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
// sheet's text and its one-word state, and the renderer's filter field.  A
// proxy answering "" to everything cannot hold text a restore is checked
// against, and `document.activeElement' is what tells a raised palette from a
// committed query.
let active = null;
const fields = {};
// The tag matters: `typing()' reads it off `document.activeElement' to decide
// whether a key belongs to the table or to whatever has focus.
const TAGS = { mtext: "TEXTAREA", filter: "INPUT" };
const field = (id) =>
  (fields[id] = fields[id] || {
    id, tagName: TAGS[id] || "DIV",
    value: "", textContent: "", className: "", style: {}, dataset: {},
    scrollTop: 0, clientHeight: 0, scrollHeight: 0,
    focus() { active = this; },
    blur() { if (active === this) active = null; },
    select() {}, addEventListener() {}, appendChild() {},
  });
const STATEFUL = ["mtext", "mnote", "mfile", "modal"];
// The page's own key dispatch, kept so a press can be delivered to it.
const pressed = [];
globalThis.document = {
  getElementById: (id) =>
    id === "keys" ? { textContent: KEYS }
      : STATEFUL.indexOf(id) === -1 ? node : field(id),
  querySelector: (sel) => (sel === "#app .tv-filter" ? field("filter") : null),
  querySelectorAll: () => [],
  createElement: () => node,
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

const press = (key) => {
  const event = {
    key, ctrlKey: false, altKey: false, metaKey: false, shiftKey: false,
    repeat: false, target: node, preventDefault: () => {},
  };
  for (const handler of pressed) handler(event);
};

// The store moving is a new tag: a client holding the old one is answered with
// a body rather than a 304, which is the reconnect that has rows to apply.
const step = () => { tag = `"t${Number(tag.slice(2, -1)) + 1}"`; };
const ACTIONS = {
  close: (reason) => { if (socket && socket.onclose) socket.onclose({ reason }); },
  sheet: (text) => { field("mtext").value = text; },
  filter: (text) => { field("filter").value = text; },
  moved: () => {
    step();
    rows = rows.concat([{ id: "r2", cells: { state: "TODO", title: "two", tag: "" } }]);
    served += 1;
  },
  recolumn: () => { step(); columns = columns.concat([{ key: "deadline" }]); },
  rewritten: () => { digest = "d1"; },
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
    palette: field("filter").value,
  });
  // Exit on the write's own callback: a keystroke leaves the echo pill's timer
  // pending, and node would otherwise sit out its second and a half.
  process.stdout.write(said + "\n", () => process.exit(0));
})();
