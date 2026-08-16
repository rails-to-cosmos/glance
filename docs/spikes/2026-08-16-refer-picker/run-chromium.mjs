// The same case file, against Chromium over CDP — one adapter behind the two
// names `refer-cases.mjs' uses, so a case is written once and run on both.
//   node run-chromium.mjs <page.html> <cases.mjs>
import { spawn } from "node:child_process";
import { existsSync } from "node:fs";
import { readdir } from "node:fs/promises";
import { homedir, tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { KEY } from "./bidi.mjs";

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
async function browserPath() {
  for (const p of ["/usr/bin/chromium", "/usr/bin/google-chrome-stable"]) if (existsSync(p)) return p;
  const walk = async (d, n) => {
    if (n > 4) return null;
    let x = [];
    try { x = await readdir(d, { withFileTypes: true }); } catch { return null; }
    for (const e of x) if (!e.isDirectory() && (e.name === "headless_shell" || e.name === "chrome")) return join(d, e.name);
    for (const e of x) if (e.isDirectory()) { const h = await walk(join(d, e.name), n + 1); if (h) return h; }
    return null;
  };
  return walk(join(homedir(), ".cache", "ms-playwright"), 0);
}
class CDP {
  constructor(w) { this.ws = w; this.n = 0; this.q = new Map();
    w.addEventListener("message", (m) => { const x = JSON.parse(m.data); const q = this.q.get(x.id);
      if (!q) return; this.q.delete(x.id); x.error ? q.no(new Error(x.error.message)) : q.ok(x.result); }); }
  static async open(u) { const w = new WebSocket(u);
    await new Promise((ok, no) => { w.addEventListener("open", ok, { once: true });
      w.addEventListener("error", () => no(new Error("cannot open")), { once: true }); });
    return new CDP(w); }
  send(m, p = {}, s) { const id = ++this.n; return new Promise((ok, no) => { this.q.set(id, { ok, no });
    this.ws.send(JSON.stringify({ id, method: m, params: p, ...(s ? { sessionId: s } : {}) }));
    setTimeout(() => { if (this.q.delete(id)) no(new Error(m + " timed out")); }, 20000); }); }
}

// WebDriver's private-use codepoints, mapped onto what CDP wants.  `vk' matters:
// an EDITING key (Backspace, Enter) only performs its edit when it arrives as
// `keyDown' carrying its virtual key code — `rawKeyDown' fires the JS event and
// changes no text, which is a harness bug that looks exactly like a page bug.
const AS_CDP = new Map([
  [KEY.Shift,      { key: "Shift", code: "ShiftLeft", mod: 8, vk: 16 }],
  [KEY.Control,    { key: "Control", code: "ControlLeft", mod: 2, vk: 17 }],
  [KEY.Alt,        { key: "Alt", code: "AltLeft", mod: 1, vk: 18 }],
  [KEY.Enter,      { key: "Enter", code: "Enter", vk: 13, edits: true }],
  [KEY.Escape,     { key: "Escape", code: "Escape", vk: 27 }],
  [KEY.Tab,        { key: "Tab", code: "Tab", vk: 9, edits: true }],
  [KEY.ArrowDown,  { key: "ArrowDown", code: "ArrowDown", vk: 40, edits: true }],
  [KEY.ArrowUp,    { key: "ArrowUp", code: "ArrowUp", vk: 38, edits: true }],
  [KEY.ArrowLeft,  { key: "ArrowLeft", code: "ArrowLeft", vk: 37, edits: true }],
  [KEY.ArrowRight, { key: "ArrowRight", code: "ArrowRight", vk: 39, edits: true }],
  [KEY.Backspace,  { key: "Backspace", code: "Backspace", vk: 8, edits: true }],
]);
const CODE = { "@": "Digit2", "u": "KeyU", "c": "KeyC", " ": "Space" };
const spell = (ch) => AS_CDP.get(ch)
  || { key: ch, code: CODE[ch] || ("Key" + ch.toUpperCase()), text: ch };

const { mkdtemp, rm } = await import("node:fs/promises");
const profile = await mkdtemp(join(tmpdir(), "refer-cr-"));   // a fresh one, or a
const chrome = spawn(await browserPath(), [                   // killed run's lock stays
  "--headless=new", "--remote-debugging-port=0", "--user-data-dir=" + profile,
  "--no-first-run", "--no-default-browser-check", "--disable-gpu", "--disable-dev-shm-usage",
  "--no-sandbox", "--window-size=1200,900", "about:blank",
], { stdio: ["ignore", "ignore", "pipe"] });
const wsURL = await new Promise((ok, no) => { let b = "";
  const t = setTimeout(() => no(new Error("no ws:\n" + b)), 20000);
  chrome.stderr.on("data", (d) => { b += d; const m = /ws:\/\/[^\s]+/.exec(b); if (m) { clearTimeout(t); ok(m[0]); } }); });
const cdp = await CDP.open(wsURL);
const { targetId } = await cdp.send("Target.createTarget", { url: "about:blank" });
const { sessionId } = await cdp.send("Target.attachToTarget", { targetId, flatten: true });
const call = (m, p) => cdp.send(m, p, sessionId);
await call("Page.enable"); await call("Runtime.enable");

let mods = 0;
const p = {
  async goto(url) {
    await call("Page.navigate", { url });
    for (let i = 0; i < 200; i++) {
      const r = await call("Runtime.evaluate", { expression: "document.readyState === 'complete'", returnByValue: true });
      if (r.result.value) break;
      await sleep(25);
    }
    mods = 0;
    await sleep(120);
  },
  async eval(fn, ...args) {
    const r = await call("Runtime.evaluate", {
      expression: `(${fn.toString()})(${args.map((a) => JSON.stringify(a)).join(",")})`,
      returnByValue: true, awaitPromise: true });
    if (r.exceptionDetails) throw new Error(r.exceptionDetails.exception?.description || r.exceptionDetails.text);
    return r.result.value === undefined ? null : r.result.value;
  },
  async keys(list) {
    for (const k of list) {
      if (typeof k === "string") {
        const s = spell(k);
        const vk = s.vk !== undefined ? s.vk
                 : s.key.length === 1 ? s.key.toUpperCase().charCodeAt(0) : 0;
        await call("Input.dispatchKeyEvent", {
          type: (s.text || s.edits) ? "keyDown" : "rawKeyDown",
          key: s.key, code: s.code, modifiers: mods,
          text: mods & 2 ? undefined : s.text,
          windowsVirtualKeyCode: vk, nativeVirtualKeyCode: vk });
        await call("Input.dispatchKeyEvent", { type: "keyUp", key: s.key, code: s.code, modifiers: mods });
      } else if (k.down) {
        const s = spell(k.down); mods |= s.mod || 0;
        await call("Input.dispatchKeyEvent", { type: "rawKeyDown", key: s.key, code: s.code, modifiers: mods });
      } else if (k.up) {
        const s = spell(k.up);
        await call("Input.dispatchKeyEvent", { type: "keyUp", key: s.key, code: s.code, modifiers: mods });
        mods &= ~(s.mod || 0);
      }
      await sleep(25);
    }
    await sleep(60);
  },
};

const [pagePath, hash] = process.argv[2].split("#");
const base = "file://" + resolve(pagePath);
const urlFor = (i) => base + "?case=" + i + (hash ? "#" + hash : "");
const { CASES } = await import(resolve(process.argv[3]));
let fails = 0;
for (const [i, c] of CASES.entries()) {
  await p.goto(urlFor(i));
  let got, want, note = "";
  try { [got, want, note] = await c.run(p); }
  catch (e) { got = "threw: " + e.message; want = "(no throw)"; }
  const ok = JSON.stringify(got) === JSON.stringify(want);
  console.log((ok ? "ok   " : "not ok ") + c.name + (note ? "\n       " + note : ""));
  if (!ok) { fails++;
    console.log("       want " + JSON.stringify(want));
    console.log("       got  " + JSON.stringify(got)); }
}
console.log(fails ? `\n${fails} of ${CASES.length} FAILED` : `\n${CASES.length}/${CASES.length} cases`);
chrome.kill();
await rm(profile, { recursive: true, force: true }).catch(() => {});
process.exit(fails ? 1 : 0);
