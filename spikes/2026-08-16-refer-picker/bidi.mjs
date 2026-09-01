// A WebDriver BiDi client for Firefox — node's global WebSocket, no deps, the
// same shape as test/browser/drive.mjs' CDP class.
import { spawn } from "node:child_process";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { createServer } from "node:net";

export const KEY = {
  Shift: "", Control: "", Alt: "",
  Enter: "", Escape: "", Tab: "",
  ArrowDown: "", ArrowUp: "", ArrowLeft: "", ArrowRight: "",
  Backspace: "",
};

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const freePort = () => new Promise((ok, no) => {
  const s = createServer();
  s.on("error", no);
  s.listen(0, "127.0.0.1", () => { const { port } = s.address(); s.close(() => ok(port)); });
});

class Bidi {
  constructor(ws) {
    this.ws = ws; this.n = 0; this.q = new Map();
    ws.addEventListener("message", (m) => {
      const x = JSON.parse(m.data);
      if (x.id === undefined) return;                 // events are dropped
      const q = this.q.get(x.id);
      if (!q) return;
      this.q.delete(x.id);
      x.type === "error" ? q.no(new Error(q.m + ": " + x.error + " — " + x.message)) : q.ok(x.result);
    });
  }
  static async open(url) {
    const ws = new WebSocket(url);
    await new Promise((ok, no) => {
      ws.addEventListener("open", ok, { once: true });
      ws.addEventListener("error", () => no(new Error("cannot open " + url)), { once: true });
    });
    return new Bidi(ws);
  }
  send(method, params = {}) {
    const id = ++this.n;
    return new Promise((ok, no) => {
      this.q.set(id, { ok, no, m: method });
      this.ws.send(JSON.stringify({ id, method, params }));
      setTimeout(() => { if (this.q.delete(id)) no(new Error(method + " never answered")); }, 30000);
    });
  }
  close() { try { this.ws.close(); } catch { /* gone */ } }
}

/** Launch headless Firefox, speak BiDi, and hand back a small page handle. */
export async function firefox(binary = "/usr/bin/firefox") {
  const port = await freePort();
  const profile = await mkdtemp(join(tmpdir(), "ff-bidi-"));
  const proc = spawn(binary, [
    "--headless", "--no-remote", "--profile", profile,
    "--remote-debugging-port", String(port),
  ], { stdio: ["ignore", "ignore", "pipe"] });

  let buf = "";
  proc.stderr.on("data", (d) => { buf += d; });
  // The banner is not machine-readable across versions; poll the port instead.
  let bidi = null;
  for (let i = 0; i < 300 && !bidi; i++) {
    try { bidi = await Bidi.open(`ws://127.0.0.1:${port}/session`); }
    catch { await sleep(100); }
  }
  if (!bidi) { proc.kill(); throw new Error("firefox never opened a BiDi port:\n" + buf); }

  await bidi.send("session.new", { capabilities: { alwaysMatch: { acceptInsecureCerts: true } } });
  const tree = await bidi.send("browsingContext.getTree", {});
  const context = tree.contexts[0].context;

  const evaluate = async (fn, ...args) => {
    const r = await bidi.send("script.evaluate", {
      expression: `(${fn.toString()})(${args.map((a) => JSON.stringify(a)).join(",")})`,
      target: { context }, awaitPromise: true, resultOwnership: "none",
    });
    if (r.type === "exception") throw new Error("page threw: " + r.exceptionDetails.text);
    return unwrap(r.result);
  };
  const unwrap = (v) => {
    if (!v || typeof v !== "object") return v;
    switch (v.type) {
      case "string": case "number": case "boolean": return v.value;
      case "null": case "undefined": return null;
      case "array": return v.value.map(unwrap);
      case "object": return Object.fromEntries((v.value || []).map(([k, x]) => [unwrap(k), unwrap(x)]));
      default: return v.value === undefined ? null : v.value;
    }
  };
  /** KEYS is a flat list: a bare string types it, {down}/{up} hold a modifier. */
  const keys = async (list) => {
    const actions = [];
    for (const k of list) {
      if (typeof k === "string") { actions.push({ type: "keyDown", value: k }, { type: "keyUp", value: k }); }
      else if (k.down) actions.push({ type: "keyDown", value: k.down });
      else if (k.up) actions.push({ type: "keyUp", value: k.up });
      else if (k.pause) actions.push({ type: "pause", duration: k.pause });
    }
    await bidi.send("input.performActions", {
      context, actions: [{ type: "key", id: "kb", actions }],
    });
    await sleep(60);
  };

  return {
    context,
    async goto(url) {
      await bidi.send("browsingContext.navigate", { context, url, wait: "complete" });
      await sleep(150);
    },
    eval: evaluate,
    keys,
    async shot(path) {
      const r = await bidi.send("browsingContext.captureScreenshot", { context });
      const { writeFile } = await import("node:fs/promises");
      await writeFile(path, Buffer.from(r.data, "base64"));
    },
    async close() {
      bidi.close(); proc.kill();
      await rm(profile, { recursive: true, force: true }).catch(() => {});
    },
    stderr: () => buf,
  };
}
