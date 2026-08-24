// A CDP client for Chromium — node's global WebSocket, no deps, the same shape
// as `bidi.mjs' so a check can be driven under either engine.
//
// WHY A SECOND ENGINE.  `bidi.mjs' drives Firefox and every rung passed under it
// while a real browser showed the opposite (README round 4).  A pin that only
// ever runs on one engine is a pin that only knows one engine's timing, so this
// one exists to be the SECOND opinion — and to hand back PIXELS, because a
// selection is a thing the reader SEES and bytes do not lie.
import { spawn } from "node:child_process";
import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { createServer } from "node:net";
import { inflateSync } from "node:zlib";

export const KEY = {
  Enter:      { code: "Enter",      key: "Enter",      vk: 13, text: "\r" },
  Escape:     { code: "Escape",     key: "Escape",     vk: 27 },
  Tab:        { code: "Tab",        key: "Tab",        vk: 9,  text: "\t" },
  Backspace:  { code: "Backspace",  key: "Backspace",  vk: 8 },
  ArrowLeft:  { code: "ArrowLeft",  key: "ArrowLeft",  vk: 37 },
  ArrowUp:    { code: "ArrowUp",    key: "ArrowUp",    vk: 38 },
  ArrowRight: { code: "ArrowRight", key: "ArrowRight", vk: 39 },
  ArrowDown:  { code: "ArrowDown",  key: "ArrowDown",  vk: 40 },
};
export const MOD = { Ctrl: 2, Shift: 8 };

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const freePort = () => new Promise((ok, no) => {
  const s = createServer();
  s.on("error", no);
  s.listen(0, "127.0.0.1", () => { const { port } = s.address(); s.close(() => ok(port)); });
});

class Cdp {
  constructor(ws) {
    this.ws = ws; this.n = 0; this.q = new Map();
    ws.addEventListener("message", (m) => {
      const x = JSON.parse(m.data);
      if (x.id === undefined) return;                   // events are dropped
      const q = this.q.get(x.id);
      if (!q) return;
      this.q.delete(x.id);
      x.error ? q.no(new Error(q.m + ": " + JSON.stringify(x.error))) : q.ok(x.result);
    });
  }
  static async open(url) {
    const ws = new WebSocket(url);
    await new Promise((ok, no) => {
      ws.addEventListener("open", ok, { once: true });
      ws.addEventListener("error", () => no(new Error("cannot open " + url)), { once: true });
    });
    return new Cdp(ws);
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

/** Launch Chromium, speak CDP, and hand back a page handle shaped like
 * `bidi.mjs''s.  FOCUS IS EMULATED ON: a headless page that does not believe it
 * is focused paints no selection at all, and the reader's browser is focused —
 * so a pin that skipped this would be testing the wrong document. */
export async function chromium(binary = "/usr/bin/chromium") {
  const port = await freePort();
  const dir = await mkdtemp(join(tmpdir(), "cdp-"));
  const proc = spawn(binary, [
    "--headless=new", "--disable-gpu", "--no-first-run", "--no-default-browser-check",
    "--hide-scrollbars", "--window-size=1366,700",
    "--user-data-dir=" + dir, "--remote-debugging-port=" + port,
    "about:blank",
  ], { stdio: ["ignore", "ignore", "pipe"] });

  let buf = "";
  proc.stderr.on("data", (d) => { buf += d; });

  let target = null;
  for (let i = 0; i < 300 && !target; i += 1) {
    try {
      const r = await fetch(`http://127.0.0.1:${port}/json/list`);
      const list = await r.json();
      target = list.find((t) => t.type === "page");
    } catch { /* not up yet */ }
    if (!target) await sleep(100);
  }
  if (!target) { proc.kill(); throw new Error("chromium never opened a CDP port:\n" + buf); }

  const cdp = await Cdp.open(target.webSocketDebuggerUrl);
  await cdp.send("Page.enable");
  await cdp.send("Runtime.enable");
  await cdp.send("Emulation.setFocusEmulationEnabled", { enabled: true });

  const evaluate = async (fn, ...args) => {
    const r = await cdp.send("Runtime.evaluate", {
      expression: `(${fn.toString()})(${args.map((a) => JSON.stringify(a)).join(",")})`,
      returnByValue: true, awaitPromise: true,
    });
    if (r.exceptionDetails)
      throw new Error("page threw: " + (r.exceptionDetails.exception?.description
                                        || r.exceptionDetails.text));
    return r.result.value === undefined ? null : r.result.value;
  };

  const press = async (k, modifiers = 0) => {
    const spec = typeof k === "string"
      ? { key: k, code: null, vk: k.toUpperCase().charCodeAt(0), text: k }
      : k;
    const base = { modifiers, key: spec.key,
                   windowsVirtualKeyCode: spec.vk, nativeVirtualKeyCode: spec.vk };
    if (spec.code) base.code = spec.code;
    // A CHARACTER needs its `text' on the keyDown or nothing is typed; a NAMED
    // key needs a rawKeyDown, or Chromium inserts its name as text.
    await cdp.send("Input.dispatchKeyEvent", spec.text !== undefined && !spec.code
      ? { ...base, type: "keyDown", text: spec.text, unmodifiedText: spec.text }
      : { ...base, type: "rawKeyDown" });
    if (spec.code && spec.text !== undefined && !modifiers)
      await cdp.send("Input.dispatchKeyEvent", { ...base, type: "char", text: spec.text });
    await cdp.send("Input.dispatchKeyEvent", { ...base, type: "keyUp" });
  };

  /** KEYS is a flat list: a bare string types it, a KEY entry presses it, and
   * `{down}`/`{up}` hold a modifier across the entries between them. */
  const keys = async (list) => {
    let mods = 0;
    for (const k of list) {
      if (k && k.down) { mods |= MOD[k.down] || 0; continue; }
      if (k && k.up) { mods &= ~(MOD[k.up] || 0); continue; }
      await press(k, mods);
    }
    await sleep(60);
  };

  return {
    async goto(url) {
      await cdp.send("Page.navigate", { url });
      for (let i = 0; i < 100; i += 1) {
        const ready = await evaluate(() => document.readyState === "complete");
        if (ready) break;
        await sleep(50);
      }
      await sleep(120);
    },
    eval: evaluate,
    keys,
    /** THE PAGE'S OWN SETTLE: every microtask drained, then two frames painted.
     * Reading a widget's state before this is reading the middle of a redraw. */
    settle: () => evaluate(() => new Promise((ok) => {
      requestAnimationFrame(() => requestAnimationFrame(() => setTimeout(ok, 0)));
    })),
    async shot(path) {
      const r = await cdp.send("Page.captureScreenshot", { format: "png" });
      const png = Buffer.from(r.data, "base64");
      if (path) await writeFile(path, png);
      return png;
    },
    async close() {
      cdp.close(); proc.kill();
      await rm(dir, { recursive: true, force: true }).catch(() => {});
    },
    stderr: () => buf,
  };
}

/** A PNG decoded to {w, h, at(x, y) -> "#rrggbb"} — node's own zlib, no deps.
 * Only what Chromium emits: 8-bit RGB or RGBA, no interlace, no palette. */
export function pixels(png) {
  if (png.readUInt32BE(0) !== 0x89504e47) throw new Error("not a PNG");
  let at = 8, w = 0, h = 0, depth = 0, colour = 0, interlace = 0;
  const idat = [];
  while (at < png.length) {
    const len = png.readUInt32BE(at);
    const kind = png.toString("ascii", at + 4, at + 8);
    const body = png.subarray(at + 8, at + 8 + len);
    if (kind === "IHDR") {
      w = body.readUInt32BE(0); h = body.readUInt32BE(4);
      depth = body[8]; colour = body[9]; interlace = body[12];
    } else if (kind === "IDAT") idat.push(body);
    else if (kind === "IEND") break;
    at += len + 12;
  }
  if (depth !== 8 || interlace !== 0 || (colour !== 2 && colour !== 6))
    throw new Error(`unsupported PNG: depth ${depth} colour ${colour} interlace ${interlace}`);
  const n = colour === 6 ? 4 : 3;
  const raw = inflateSync(Buffer.concat(idat));
  const out = Buffer.alloc(w * h * n);
  const stride = w * n;
  for (let y = 0; y < h; y += 1) {
    const filter = raw[y * (stride + 1)];
    const line = raw.subarray(y * (stride + 1) + 1, y * (stride + 1) + 1 + stride);
    const to = y * stride, up = to - stride;
    for (let x = 0; x < stride; x += 1) {
      const a = x >= n ? out[to + x - n] : 0;
      const b = y > 0 ? out[up + x] : 0;
      const c = x >= n && y > 0 ? out[up + x - n] : 0;
      let v = line[x];
      if (filter === 1) v += a;
      else if (filter === 2) v += b;
      else if (filter === 3) v += (a + b) >> 1;
      else if (filter === 4) {
        const p = a + b - c, pa = Math.abs(p - a), pb = Math.abs(p - b), pc = Math.abs(p - c);
        v += (pa <= pb && pa <= pc) ? a : (pb <= pc ? b : c);
      }
      out[to + x] = v & 0xff;
    }
  }
  const hex = (v) => v.toString(16).padStart(2, "0");
  return {
    w, h,
    at(x, y) {
      if (x < 0 || y < 0 || x >= w || y >= h) return null;
      const i = y * stride + x * n;
      return "#" + hex(out[i]) + hex(out[i + 1]) + hex(out[i + 2]);
    },
    /** How many pixels in the box carry HEX — the reading a wash is judged by. */
    count(box, hexWant) {
      let k = 0;
      for (let y = Math.max(0, box.y); y < Math.min(h, box.y + box.h); y += 1)
        for (let x = Math.max(0, box.x); x < Math.min(w, box.x + box.w); x += 1)
          if (this.at(x, y) === hexWant) k += 1;
      return k;
    },
  };
}
