// ZERO DEPENDENCIES: node's global WebSocket onto CDP.
// Every wait is a CONDITION with a cap, never a duration (AGENTS.hs).
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
import { freePort, polling, sleep } from "../harness.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const TURN = 25;              // the poll, in ms — the watch's own drain rate
const poll = polling(TURN);
const READY = 30_000;         // the daemon's walk, capped
const SETTLE = 8_000;         // a page condition, capped


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


/** A CASE NOBODY HAS SEEN FAIL IS NOT EVIDENCE: each entry takes ONE rule out
 * of the served page and names the case it should turn red. */
const BREAKS = {
  "edit-covers": ["an open edit moves the line under it down",
                  "#dpara.on{height:220px !important}"],
  "edit-floor": ["an open edit moves the line under it down",
                 ".de.dat{min-height:0 !important}"],
  // The drawn paragraph collapses to nothing.
  "draft-floor": ["a paragraph drawn before it is written",
                  ".d-draft{min-height:0 !important}"],
  // The pane draws its flag in `--g-warn' at a strength of its own.
  "flag-red": ["a flag paints one red on both surfaces",
               ".de.dfl,#mdoc.on .de.dfl{--ink:var(--g-warn) !important}"],
  "no-clip": ["the page never scrolls",
              "html,body{overflow:visible !important}#log{width:220vw !important}"],
  // The tier stops clamping.
  "pop-clamp": ["a popup clamps inside the viewport",
                ".pop-band,.pop-sheet{box-sizing:content-box !important;"
                  + "height:96vh !important;max-height:96vh !important}"],
  // The sheet's state cell falls back to the page's own ink.
  "badge-hue": ["a badge in the sheet paints the hue",
                "#mdoc .dc-state{color:var(--g-fg) !important}"],
  "para-indent": ["a paragraph is indented under the title text",
                  "#mdoc .d-para{padding-left:0 !important}"],
  "cursor-line": ["the cursor in the pane is a mark",
                  "#mdoc.on .de.dat{--ink:transparent !important}"],
  // The picker stops hanging at the caret and centres like a popup instead.
  "refer-veil": ["@ in the sheet links the row under the cursor",
                 "#refer{background:var(--g-veil) !important}"],
  // The header goes back to the cell edge, a padding left of its own badges.
  "badge-head": ["a badge column's header lines up with its badges",
                 ".tv-fill th.tv-badge .tv-hd{padding-left:0 !important}"],
  // The pill outgrows the ground `applyWidths' bought it (`PILL_PAD').
  "pill-ground": ["a badge sized by its own pill is drawn whole",
                  ".tv-pill{padding:0 14px !important}"],
  // The picker's filter editor is resident again, no summoning needed.
  "filter-resident": ["ESC in the picker's filter drops the edit",
                      ".tv-inline .tv-bar{display:flex !important}"],
  // The mint form goes back to one hue field, for whichever theme is on.
  "one-hue": ["+ over the state palette draws a form",
              ".nrow-ndark{display:none !important}"],
  // What point CARRIES takes point's own ink, so the stop and its subtree read
  // as one thing again.
  "carried-ink": ["the cursor on a list item lights itself",
                  "#mdoc.on .de.dat .de{--ink:var(--g-point-off) !important}"],
  // The line box goes back to a fraction, so rows start off the device grid.
  "line-fraction": ["the elbow turns on the dash's own ink",
                    ":root{--g-doc-lh:20.8px !important}"],
  // A paragraph loses the ground the table's cursor wears.
  "para-ground": ["the cursor in the pane is a mark",
                  "#mdoc.on .de.dat.d-para{background-color:transparent !important}"],
  // The flag stops at the row it was pressed on, leaving its branch unmarked.
  "flag-branch": ["a flag paints one red on both surfaces",
                  ".de.dfl .de,#mdoc.on .de.dfl .de{--ink:var(--g-fg) !important}"],
  // A sibling's branch dims with the rest, so the choice cannot be weighed.
  "sib-dim": ["the pane dims every branch but the one",
              "#mdoc.on .focus .sib,#mdoc.on .focus .sib .de"
                + "{color:var(--g-point-off) !important}"],
  // The elbow turns on the middle of the line box again, a pixel off the dash.
  "turn-half": ["the elbow turns on the dash's own ink",
                ".d-list .d-item::before{height:calc(var(--g-doc-fs) * "
                  + "var(--g-doc-lh) / 2) !important}"],
  // The pane stops dimming, so a branch reads like every other.
  "no-focus": ["the pane dims every branch but the one",
               "#mdoc.on .focus .de{color:var(--g-fg) !important}"],
  // The strip stops agreeing with the connectors: its last crumb is ordinary ink.
  "crumb-ink": ["the strip names the way back",
                "#mdoc.on .cr-0{color:var(--g-mute) !important}"],
  // The light runs the whole subtree again, as a ground once did.
  "bullet-subtree": ["the marker org wrote lights with the line",
                     "#mdoc.on .de.dat .dm{color:var(--g-point) !important}"],
  // The kind badge reads like a row's own badge — a washed ground, no outline.
  "kind-badge": ["K declares the kind",
                 "#rkind{border-style:solid !important;"
                   + "background:var(--g-surface) !important}"],
  // Both surfaces draw the classic bar back, layout width and all.
  "bar-space": ["no surface on the page draws a scrollbar of its own",
                ".tv-scroll,#kbd{scrollbar-width:auto !important}"
                  + ".tv-scroll::-webkit-scrollbar,#kbd::-webkit-scrollbar"
                  + "{width:15px !important;height:15px !important}"],
};

class CDP {
  constructor(ws) {
    this.ws = ws; this.n = 0; this.waiting = new Map();
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


function pageHandle(cdp, sid) {
  const call = (m, p) => cdp.send(m, p, sid);
  // A PAGE FUNCTION IS SERIALIZED WHOLE and can call nothing of this module, so the
  // two probes every case wanted ride in ahead of it: `rgb' resolves a colour the way
  // the engine would paint it, and `ink' reads the tier a row wears.
  const PRELUDE = 'const rgb=(v)=>{const d=document.createElement("div");'
    + 'd.style.color=v;document.body.append(d);'
    + 'const c=getComputedStyle(d).color;d.remove();return c;};'
    + 'const ink=(n)=>rgb(getComputedStyle(n).getPropertyValue("--ink").trim());';
  const evaluate = async (fn, ...args) => {
    const src = `(()=>{${PRELUDE}return (${fn.toString()})(`
      + `${args.map((a) => JSON.stringify(a)).join(",")});})()`;
    const r = await call("Runtime.evaluate",
      { expression: src, returnByValue: true, awaitPromise: true });
    if (r.exceptionDetails)
      throw new Error(`page threw: ${r.exceptionDetails.exception?.description
        || r.exceptionDetails.text}`);
    return r.result.value;
  };
  // A press carries `code' beside `key', read by `keyName' (frontend/glue/05-keys.js),
  // and the vk: without it a held Backspace deletes no character.
  const NAMED = { RET: ["Enter", "Enter", 13], TAB: ["Tab", "Tab", 9],
    SPC: [" ", "Space", 32],
    ESC: ["Escape", "Escape", 27], DEL: ["Backspace", "Backspace", 8],
    "<up>": ["ArrowUp", "ArrowUp", 38], "<down>": ["ArrowDown", "ArrowDown", 40],
    "<left>": ["ArrowLeft", "ArrowLeft", 37], "<right>": ["ArrowRight", "ArrowRight", 39] };
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
      const [key, code, vk] = NAMED[rest];
      return { key, code, vk, modifiers: mods, text: key === " " ? " " : undefined };
    }
    const vk = rest.toUpperCase().charCodeAt(0);
    if (/^[A-Za-z]$/.test(rest))
      return { key: rest, code: `Key${rest.toUpperCase()}`, vk,
               modifiers: mods | (rest === rest.toUpperCase() ? 8 : 0), text: rest };
    return { key: rest, code: CODE[rest] || `Key${rest.toUpperCase()}`, vk,
             modifiers: mods, text: rest };
  }
  async function held(name, times) {
    const e = keyEvent(name);
    const down = (autoRepeat) => call("Input.dispatchKeyEvent",
      { type: e.text ? "keyDown" : "rawKeyDown", key: e.key, code: e.code,
        windowsVirtualKeyCode: e.vk, autoRepeat,
        modifiers: e.modifiers, text: e.modifiers & 2 ? undefined : e.text });
    await down(false);
    for (let i = 0; i < times; i += 1) await down(true);
    await call("Input.dispatchKeyEvent",
      { type: "keyUp", key: e.key, code: e.code, modifiers: e.modifiers });
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
    press: (name) => held(name, 0),
    hold: held,
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
    strip() {
      return evaluate(() => {
        const box = document.getElementById("log");
        if (!box) return [];
        return [...box.children].slice(-6).map((n) => n.textContent.trim());
      }).catch(() => []);
    },
  };
}


async function main() {
  const only = process.env.ONLY || "";
  const keep = process.env.KEEP === "1";
  let lied = false;   // a BREAK whose claim this run did not bear out
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
  // THE CASES WRITE, so the repo's fixtures stay byte-identical.
  await cp(join(HERE, "tree"), tree, { recursive: true });

  const port = await freePort();
  let daemon = null, profile = null, browser = null, cdp = null, failed = 0, daemonSaid = "";
  const started = Date.now();
  try {
    // The daemon's stderr is HELD: a `CloseRequest' per closed socket would bury the report.
    daemon = spawn(bin, ["serve", "--dir", tree, "--port", String(port)],
                   { stdio: ["ignore", "ignore", "pipe"] });
    daemon.stderr.on("data", (d) => { daemonSaid += d; });
    daemon.on("error", (e) => { throw e; });
    const base = `http://127.0.0.1:${port}`;
    // Readiness is the route that NEEDS the store: the bind lands before the walk ends.
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
      console.log(`BREAK=${broke} — "${expect}" is the case that should go red\n`);
    }

    const { default: cases } = await import(join(HERE, "cases.mjs"));
    const picked = cases.filter((c) => c.name.includes(only));
    const lines = [];
    let n = 0;
    for (const c of picked) {
      n += 1;
      const at = Date.now();
      // A `known' CASE IS EXPECTED RED: a GREEN one is itself a failure.
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
    // Keyed by NAME: an ordinal over the ONLY-filtered list rots when a case moves.
    if (broke) {
      const [want] = BREAKS[broke];
      const hit = lines.filter((l) => l.name.includes(want));
      if (!hit.length) {
        console.log(`browser-check: BREAK=${broke} names "${want}", which no case `
          + `run here is — rename it in BREAKS or drop ONLY`);
        lied = true;
      } else if (hit.every((l) => l.ok)) {
        console.log(`browser-check: BREAK=${broke} left "${want}" GREEN, so the `
          + `break proves nothing: that case does not measure what it took away`);
        lied = true;
      }
    }
  } finally {
    if (cdp) cdp.close();
    if (browser) await end(browser);
    if (daemon) await end(daemon);
    if (profile) await rm(profile, { recursive: true, force: true }).catch(() => {});
    if (!keep && !failed) await rm(shots, { recursive: true, force: true }).catch(() => {});
  }
  process.exit(lied ? 2 : failed ? 1 : 0);
}

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
