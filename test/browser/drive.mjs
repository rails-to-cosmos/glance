// THE DRIVER — a real engine measures the page, because nothing else in this
// repo renders anything (docs/proposal-browser-driver.md).  ONE WIDGET, ONE
// FILE: browser launch, CDP client, daemon lifecycle, the six-call page handle
// and the report.  ZERO DEPENDENCIES — node's global WebSocket onto Chrome's
// `/json/version', which is the whole transport.
//
// TEARDOWN IS A `finally' AND IT IS THE TARGET'S LIFE: a leaked daemon or a
// hung browser on a red run is what gets a check deleted.  Every wait is a
// CONDITION with a cap, never a duration (CLAUDE.md's `until:stale=off' rule).
//
//   GLANCE_BIN   the daemon to serve with          (else `cabal list-bin')
//   CHROME       the browser binary to drive       (else ~/.cache/ms-playwright)
//   KEEP=1       leave the temp tree and the shots behind
//   ONLY=substr  run the cases whose name carries it
//   BREAK=name   take ONE rule out of the page — see `BREAKS' below

import { spawn } from "node:child_process";
import { mkdtemp, cp, rm, writeFile, readdir } from "node:fs/promises";
import { existsSync } from "node:fs";
import { tmpdir, homedir } from "node:os";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { createServer } from "node:net";

const HERE = dirname(fileURLToPath(import.meta.url));
const TURN = 25;              // the poll, in ms — the watch's own drain rate
const READY = 30_000;         // the daemon's walk, capped
const SETTLE = 8_000;         // a page condition, capped

// ---------------------------------------------------------------- utilities

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

/** A port nothing holds: bind 0, read what the kernel gave, close. */
const freePort = () => new Promise((ok, no) => {
  const s = createServer();
  s.on("error", no);
  s.listen(0, "127.0.0.1", () => {
    const { port } = s.address();
    s.close(() => ok(port));
  });
});

/** Poll FN until it answers truthy, or fail naming WHAT was waited for. */
async function poll(fn, cap, what) {
  const till = Date.now() + cap;
  let last;
  for (;;) {
    try { last = await fn(); if (last) return last; } catch (e) { last = String(e); }
    if (Date.now() > till) throw new Error(`waited ${cap}ms for ${what} (last: ${JSON.stringify(last)})`);
    await sleep(TURN);
  }
}

/** The browser this machine has: $CHROME, then PATH, then the cache. */
async function browserPath() {
  if (process.env.CHROME) return process.env.CHROME;
  for (const p of ["/usr/bin/chromium", "/usr/bin/google-chrome-stable",
                   "/usr/bin/google-chrome", "/usr/bin/brave"])
    if (existsSync(p)) return p;
  const root = join(homedir(), ".cache", "ms-playwright");
  const walk = async (dir, depth) => {
    if (depth > 4) return null;
    let names = [];
    try { names = await readdir(dir, { withFileTypes: true }); } catch { return null; }
    for (const n of names)
      if (!n.isDirectory() && (n.name === "headless_shell" || n.name === "chrome"))
        return join(dir, n.name);
    for (const n of names)
      if (n.isDirectory()) { const hit = await walk(join(dir, n.name), depth + 1); if (hit) return hit; }
    return null;
  };
  return walk(root, 0);
}

// ------------------------------------------------------------------- breaks

/**
 * A CASE NOBODY HAS SEEN FAIL IS NOT EVIDENCE, and no source file has to move
 * to see one: each entry takes ONE rule out of the served page with a
 * stylesheet injected at document start, and names the case it should turn
 * red.  `make browser-check BREAK=edit-covers' is the proof.
 */
const BREAKS = {
  // 1 — the box stands over the document instead of pushing it down (cb6db85).
  "edit-covers": ["1", "#dpara.on{height:220px !important}"],
  // 1 — the block keeps a one-line floor whatever is typed into it.
  "edit-floor": ["1", ".de.dat{min-height:0 !important}"],
  // 2 — the drawn paragraph collapses to nothing (d7ba44b).
  "draft-floor": ["2", ".d-draft{min-height:0 !important}"],
  // 3 — the pane draws its flag in `--g-warn' at a strength of its own (14e13d9).
  "flag-red": ["3", ".de.dfl{box-shadow:inset 3px 0 0 var(--g-warn) !important}"],
  // 4 — something wide, and the viewport told to scroll to it.
  "no-clip": ["4", "html,body{overflow:visible !important}"
                 + "#log{width:220vw !important}"],
  // 6 — the sheet's state cell falls back to the page's own ink (80c3732).
  "badge-hue": ["6", "#mdoc .dc-state{color:var(--g-fg) !important}"],
  // 7 — content sits at the stars rather than under the title text.
  "para-indent": ["7", "#mdoc .d-para{padding-left:0 !important}"],
  // 8 — the cursor is drawn as a line rather than as a ground.
  "cursor-line": ["8", "#mdoc.on .de.dat{background:transparent !important;"
                     + "text-decoration:underline !important}"],
};

// ---------------------------------------------------------------- CDP client

/**
 * The whole protocol surface: one socket, one id counter, one session.
 * `flatten' puts the page session on the browser socket, so there is one
 * connection to close and one place a failure can leak from.
 */
class CDP {
  constructor(ws) {
    this.ws = ws; this.n = 0; this.waiting = new Map();
    // EVENTS ARE DROPPED: every wait here is a CONDITION polled in the page, so
    // nothing subscribes and a protocol event owes this client nothing.
    ws.addEventListener("message", (m) => {
      const msg = JSON.parse(m.data);
      const w = msg.id === undefined ? null : this.waiting.get(msg.id);
      if (!w) return;
      this.waiting.delete(msg.id);
      if (msg.error) w.no(new Error(`${w.method}: ${msg.error.message}`));
      else w.ok(msg.result);
    });
  }
  static async open(url) {
    const ws = new WebSocket(url);
    await new Promise((ok, no) => {
      ws.addEventListener("open", ok, { once: true });
      ws.addEventListener("error", () => no(new Error(`cannot open ${url}`)), { once: true });
    });
    return new CDP(ws);
  }
  send(method, params = {}, sessionId) {
    const id = ++this.n;
    return new Promise((ok, no) => {
      this.waiting.set(id, { ok, no, method });
      this.ws.send(JSON.stringify({ id, method, params, ...(sessionId ? { sessionId } : {}) }));
      setTimeout(() => {
        if (this.waiting.delete(id)) no(new Error(`${method} never answered in 20s`));
      }, 20_000);
    });
  }
  close() { try { this.ws.close(); } catch { /* already gone */ } }
}

// ---------------------------------------------------------- the page handle

/**
 * SIX CALLS AND NO MORE — `goto', `eval', `until', `press', `type', `size',
 * plus the two artifacts a failure owes (`shot', `strip').  Swapping CDP for
 * playwright or BiDi later is one adapter behind these names.
 */
function pageHandle(cdp, sid) {
  const call = (m, p) => cdp.send(m, p, sid);
  const evaluate = async (fn, ...args) => {
    const src = `(${fn.toString()})(${args.map((a) => JSON.stringify(a)).join(",")})`;
    const r = await call("Runtime.evaluate",
      { expression: src, returnByValue: true, awaitPromise: true });
    if (r.exceptionDetails)
      throw new Error(`page threw: ${r.exceptionDetails.exception?.description
        || r.exceptionDetails.text}`);
    return r.result.value;
  };
  // A LETTER BINDING NAMES A PHYSICAL KEY, so a press carries `code' beside
  // `key' and an uppercase binding is shift + the same code — which is exactly
  // what `keyName' (assets/glue/05-keys.js) reads back.
  const NAMED = { RET: ["Enter", "Enter"], TAB: ["Tab", "Tab"], SPC: [" ", "Space"],
    ESC: ["Escape", "Escape"], DEL: ["Backspace", "Backspace"],
    "<up>": ["ArrowUp", "ArrowUp"], "<down>": ["ArrowDown", "ArrowDown"],
    "<left>": ["ArrowLeft", "ArrowLeft"], "<right>": ["ArrowRight", "ArrowRight"] };
  const CODE = { "+": "Equal", ":": "Semicolon", ",": "Comma", "@": "Digit2",
    "!": "Digit1", "<": "Comma", ">": "Period", "[": "BracketLeft",
    "]": "BracketRight", "/": "Slash", "-": "Minus", "^": "Digit6" };
  function keyEvent(name) {
    let mods = 0, rest = name;
    for (;;) {
      if (rest.startsWith("C-")) { mods |= 2; rest = rest.slice(2); }
      else if (rest.startsWith("M-")) { mods |= 1; rest = rest.slice(2); }
      else if (rest.startsWith("S-")) { mods |= 8; rest = rest.slice(2); }
      else break;
    }
    if (NAMED[rest]) {
      const [key, code] = NAMED[rest];
      return { key, code, modifiers: mods, text: key === " " ? " " : undefined };
    }
    if (/^[A-Za-z]$/.test(rest))
      return { key: rest, code: `Key${rest.toUpperCase()}`,
               modifiers: mods | (rest === rest.toUpperCase() ? 8 : 0), text: rest };
    return { key: rest, code: CODE[rest] || `Key${rest.toUpperCase()}`,
             modifiers: mods, text: rest };
  }
  return {
    async goto(url) {
      await call("Page.navigate", { url });
      await poll(() => evaluate(() => document.readyState === "complete"),
                 SETTLE, `${url} to finish loading`);
    },
    eval: evaluate,
    until(fn, what, cap = SETTLE, ...args) {
      return poll(() => evaluate(fn, ...args), cap, what);
    },
    async press(name) {
      const e = keyEvent(name);
      await call("Input.dispatchKeyEvent",
        { type: e.text ? "keyDown" : "rawKeyDown", key: e.key, code: e.code,
          windowsVirtualKeyCode: e.key.length === 1 ? e.key.toUpperCase().charCodeAt(0) : 0,
          modifiers: e.modifiers, text: e.modifiers & 2 ? undefined : e.text });
      await call("Input.dispatchKeyEvent",
        { type: "keyUp", key: e.key, code: e.code, modifiers: e.modifiers });
    },
    type(text) { return call("Input.insertText", { text }); },
    async size(width, height) {
      await call("Emulation.setDeviceMetricsOverride",
        { width, height, deviceScaleFactor: 1, mobile: false });
    },
    async shot(path) {
      const r = await call("Page.captureScreenshot", { format: "png" });
      await writeFile(path, Buffer.from(r.data, "base64"));
      return path;
    },
    // THE PAGE'S OWN TESTIMONY: the log strip is the shell's append-only
    // account of what it did, and a failing geometry case wants it beside
    // the picture.
    strip() {
      return evaluate(() => {
        const box = document.getElementById("log");
        if (!box) return [];
        return [...box.children].slice(-6).map((n) => n.textContent.trim());
      }).catch(() => []);
    },
  };
}

// ---------------------------------------------------------------- lifecycle

async function main() {
  const only = process.env.ONLY || "";
  const keep = process.env.KEEP === "1";
  const broke = process.env.BREAK || "";
  if (broke && !BREAKS[broke]) {
    console.error(`browser-check: no break named "${broke}" — `
      + `try one of: ${Object.keys(BREAKS).join(", ")}`);
    process.exit(2);
  }
  const chrome = await browserPath();
  if (!chrome) {
    console.error("browser-check: no browser found -- SKIPPED (run `make browser')");
    process.exit(0);
  }
  const bin = process.env.GLANCE_BIN;
  if (!bin || !existsSync(bin)) {
    console.error(`browser-check: no daemon at GLANCE_BIN=${bin || "<unset>"} -- SKIPPED`);
    process.exit(0);
  }

  const shots = await mkdtemp(join(tmpdir(), "glance-drive-"));
  const tree = join(shots, "tree");
  // THE CASES WRITE: paragraph edits and flags land in a COPY, so the repo's
  // fixtures stay byte-identical whatever a run does.
  await cp(join(HERE, "tree"), tree, { recursive: true });

  const port = await freePort();
  let daemon = null, profile = null, browser = null, cdp = null, failed = 0, daemonSaid = "";
  const started = Date.now();
  try {
    // The daemon's own stderr is HELD rather than inherited: a socket the
    // driver closes between cases is one `CloseRequest' line each, which would
    // bury the report.  It is printed whole where the run went red.
    daemon = spawn(bin, ["serve", "--dir", tree, "--port", String(port)],
                   { stdio: ["ignore", "ignore", "pipe"] });
    daemon.stderr.on("data", (d) => { daemonSaid += d; });
    daemon.on("error", (e) => { throw e; });
    const base = `http://127.0.0.1:${port}`;
    // THE BIND IS NOT THE LOAD: `/' and the assets serve while the walk runs
    // and `/headlines' answers 503 + Retry-After until it lands, so readiness
    // is the route that NEEDS the store.
    const rows = await poll(async () => {
      const r = await fetch(`${base}/headlines?limit=1`).catch(() => null);
      return r && r.status === 200 ? r.json() : null;
    }, READY, "the daemon to finish its walk");
    if (!rows.rows || !rows.rows.length)
      throw new Error("the daemon served zero rows: the fixture tree loaded nothing");

    profile = await mkdtemp(join(tmpdir(), "glance-chrome-"));
    browser = spawn(chrome, [
      "--headless=new", "--remote-debugging-port=0", `--user-data-dir=${profile}`,
      "--no-first-run", "--no-default-browser-check", "--disable-gpu",
      "--disable-dev-shm-usage", "--no-sandbox", "--window-size=1400,900",
      "about:blank",
    ], { stdio: ["ignore", "ignore", "pipe"] });
    const wsURL = await new Promise((ok, no) => {
      let buf = "";
      const at = setTimeout(() => no(new Error(`the browser never printed a debugger URL:\n${buf}`)), 20_000);
      browser.stderr.on("data", (d) => {
        buf += d;
        const m = /ws:\/\/[^\s]+/.exec(buf);
        if (m) { clearTimeout(at); ok(m[0]); }
      });
      browser.on("exit", (c) => { clearTimeout(at); no(new Error(`the browser exited ${c}:\n${buf}`)); });
    });
    cdp = await CDP.open(wsURL);
    const { targetId } = await cdp.send("Target.createTarget", { url: "about:blank" });
    const { sessionId } = await cdp.send("Target.attachToTarget", { targetId, flatten: true });
    await cdp.send("Page.enable", {}, sessionId);
    await cdp.send("Runtime.enable", {}, sessionId);
    const p = pageHandle(cdp, sessionId);
    await p.size(1400, 900);
    if (broke) {
      const [expect, css] = BREAKS[broke];
      await cdp.send("Page.addScriptToEvaluateOnNewDocument", { source:
        `(()=>{const put=()=>{const s=document.createElement("style");`
        + `s.textContent=${JSON.stringify(css)};document.head.append(s);};`
        + `if(document.head)put();else addEventListener("DOMContentLoaded",put);})()`
      }, sessionId);
      console.log(`BREAK=${broke} — case ${expect} is the one that should go red\n`);
    }

    const { default: cases } = await import(join(HERE, "cases.mjs"));
    const picked = cases.filter((c) => c.name.includes(only));
    const lines = [];
    let n = 0;
    for (const c of picked) {
      n += 1;
      const at = Date.now();
      // A `known' CASE IS EXPECTED RED and IS NOT AN XFAIL THAT SILENCES: a
      // GREEN one is itself a failure, so the day the defect is fixed the
      // target says to take the field off.  The reading still prints.
      try {
        const said = await c.run(p, base);
        if (c.known) {
          failed += 1;
          lines.push({ ok: false, n, name: c.name, shot: null, strip: [],
                       why: `the known defect is gone — take \`known' off this case: ${c.known}` });
          console.log(`not ok ${n} — ${c.name} (the known defect is gone)`);
        } else {
          lines.push({ ok: true, n, name: c.name, said, ms: Date.now() - at });
          console.log(`ok   ${n} — ${c.name}`);
          for (const w of said || []) console.log(`       ${w}`);
        }
      } catch (e) {
        const shot = await p.shot(join(shots, `${n}.png`)).catch(() => null);
        const strip = await p.strip();
        if (c.known) {
          console.log(`known ${n} — ${c.name}`);
          console.log(`       ${e.message}`);
          console.log(`       ${c.known}`);
        } else {
          failed += 1;
          lines.push({ ok: false, n, name: c.name, why: e.message, shot, strip,
                       ms: Date.now() - at });
          console.log(`not ok ${n} — ${c.name}`);
        }
      }
      // Every case starts from a fresh page: a surface one left up is not the
      // next one's problem.
      await p.goto("about:blank").catch(() => {});
      await p.size(1400, 900);
    }

    console.log("");
    for (const l of lines.filter((x) => !x.ok)) {
      console.log(`not ok ${l.n} — ${l.name}`);
      console.log(`     ${l.why}`);
      if (l.shot) console.log(`     screenshot: ${l.shot}`);
      for (const s of l.strip || []) console.log(`     the page's own account: ${s}`);
    }
    if (failed && daemonSaid.trim())
      console.log(`\nthe daemon said:\n${daemonSaid.trim().split("\n").map((l) => `     ${l}`).join("\n")}`);
    const wall = ((Date.now() - started) / 1000).toFixed(1);
    const known = picked.filter((c) => c.known).length;
    console.log(`\n${picked.length - failed}/${picked.length} cases`
      + (known ? `, ${known} of them a KNOWN defect this repo has not fixed` : "")
      + `, ${wall}s wall` + (failed ? `, artifacts under ${shots}` : ""));
    if (failed) console.log(`browser-check: ${failed} FAILED`);
  } finally {
    if (cdp) cdp.close();
    if (browser) await end(browser);
    if (daemon) await end(daemon);
    if (profile) await rm(profile, { recursive: true, force: true }).catch(() => {});
    // The artifacts of a RED run stay, named in the report; a green one leaves
    // nothing behind.
    if (!keep && !failed) await rm(shots, { recursive: true, force: true }).catch(() => {});
  }
  process.exit(failed ? 1 : 0);
}

/** SIGTERM, then SIGKILL at 5 s: nothing this run started outlives it. */
function end(proc) {
  if (proc.exitCode !== null || proc.signalCode) return Promise.resolve();
  return new Promise((ok) => {
    const hard = setTimeout(() => { try { proc.kill("SIGKILL"); } catch { /* gone */ } }, 5_000);
    proc.on("exit", () => { clearTimeout(hard); ok(); });
    try { proc.kill("SIGTERM"); } catch { clearTimeout(hard); ok(); }
  });
}

main().catch(async (e) => {
  console.error(`browser-check: the driver itself failed — ${e.stack || e.message}`);
  process.exit(1);
});
