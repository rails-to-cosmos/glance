// Boots the shell's inline glue under node and reports what it asked the
// server for.  The glue is the page's own, extracted from a rendered `/' by
// TestServe; the browser around it is stubbed down to what a boot touches, so
// what this measures is the boot's fetch sequence and the URL it settles on —
// which string-matching the glue cannot answer, since a call that is present
// and never reached matches just the same.
//
//   node shell-harness.js DIR SEARCH TOTAL [KEYS]
//
// DIR holds `shell.js' (the glue) and `keys.json' (the page's keymap blob).
// SEARCH is `location.search' the page opens on and TOTAL what the server
// reports as `X-Glance-Total', which is what decides whether the boot pulls
// the rest of the set in behind the first page.  KEYS is an optional
// space-separated list of `KeyboardEvent.key' names pressed over the table once
// the boot has settled.
const fs = require("fs");
const [dir, search, total, keys] = process.argv.slice(2);

// Every /headlines URL the page asked for, in order.
const asked = [];
const rows = [{ id: "r1", cells: { state: "TODO", title: "one", tag: ":web:" } }];
const view = { title: "t", columns: [{ key: "state" }, { key: "tag" }], rows };

globalThis.location = { search, protocol: "http:", host: "h", pathname: "/" };
globalThis.history = {
  // The page writes its applied query here; the search string it leaves behind
  // is the link a reload would come back to.
  replaceState: (_state, _title, url) => {
    location.search = String(url).startsWith("?") ? url : "";
  },
};
globalThis.fetch = (url) => {
  asked.push(url);
  return Promise.resolve({
    ok: true,
    status: 200,
    headers: { get: () => String(total) },
    json: () => Promise.resolve(view),
    text: () => Promise.resolve(""),
  });
};
globalThis.WebSocket = function () {
  this.close = () => {};
};
// The renderer owns the applied query: it takes it at mount as chips and hands
// it back, and a strip takes the last token off it.  Enough of that here for
// the shell's own half of the round trip to be exercised.
let held = "";
globalThis.TableView = {
  mount: (_el, _view, options) => {
    held = (options || {}).initialQuery || "";
    return {
      setRows: () => {},
      getQuery: () => held,
      stripLastToken: () => {
        if (!held) return false;
        held = held.split(/\s+/).slice(0, -1).join(" ");
        return true;
      },
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
// The page's own key dispatch, kept so a press can be delivered to it.
const pressed = [];
globalThis.document = {
  getElementById: (id) => (id === "keys" ? { textContent: KEYS } : node),
  querySelector: () => null,
  querySelectorAll: () => [],
  createElement: () => node,
  addEventListener: (type, handler) => {
    if (type === "keydown") pressed.push(handler);
  },
  getSelection: () => null,
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

// Every fetch here settles as a microtask, so one turn of the event loop is
// past the whole boot — the arming fetch chained behind the set included.  The
// keys go in after that, and the answer after them.
setTimeout(() => {
  for (const key of (keys || "").split(/\s+/).filter(Boolean)) press(key);
  // Exit on the write's own callback: a keystroke leaves the echo pill's timer
  // pending, and node would otherwise sit out its second and a half.
  setTimeout(() => {
    const answer = JSON.stringify({ asked, url: location.search });
    process.stdout.write(answer + "\n", () => process.exit(0));
  }, 20);
}, 20);
