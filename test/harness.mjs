// WHAT BOTH DRIVERS NEED: `test/browser/drive.mjs' and `test/interop/drive.mjs'
// share `sleep', `freePort', `poll', `end', `guardEnv' and `serveDaemon'.
import { createServer } from "node:net";
import { spawn } from "node:child_process";
import { existsSync } from "node:fs";

const TURN = 25;              // the poll, in ms — the watch's own drain rate
const READY = 30_000;         // the daemon's walk, capped

export const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

/** A port nothing is listening on: bind zero, read it back, give it up. */
export const freePort = () => new Promise((ok, no) => {
  const s = createServer();
  s.on("error", no);
  s.listen(0, "127.0.0.1", () => {
    const { port } = s.address();
    s.close(() => ok(port));
  });
});

/** SIGTERM, then SIGKILL at 5 s: nothing this run started outlives it. */
export function end(proc) {
  if (proc.exitCode !== null || proc.signalCode) return Promise.resolve();
  return new Promise((ok) => {
    const hard = setTimeout(() => { try { proc.kill("SIGKILL"); } catch { /* gone */ } }, 5_000);
    proc.on("exit", () => { clearTimeout(hard); ok(); });
    try { proc.kill("SIGTERM"); } catch { clearTimeout(hard); ok(); }
  });
}

/** Poll FN every TURN until it answers truthy, or fail naming WHAT was waited for
 * AND the last thing seen -- a bare timeout says nothing about why. */
export function polling(TURN) {
  return async function poll(fn, cap, what) {
    const till = Date.now() + cap;
    let last;
    for (;;) {
      try { last = await fn(); if (last) return last; } catch (e) { last = String(e); }
      if (Date.now() > till)
        throw new Error(`waited ${cap}ms for ${what} (last: ${JSON.stringify(last)})`);
      await sleep(TURN);
    }
  };
}

/** THE TWO ENV FACTS NEITHER DRIVER STARTS WITHOUT: a `BREAK' that BREAKS knows
 * (a name it does not is a typo, and exits 2) and a daemon at `GLANCE_BIN' (its
 * absence is a SKIP, and exits 0).  LABEL names the target in either message. */
export function guardEnv(label, BREAKS) {
  const broke = process.env.BREAK || "";
  if (broke && !BREAKS[broke]) {
    console.error(`${label}: no break named "${broke}" — `
      + `try one of: ${Object.keys(BREAKS).join(", ")}`);
    process.exit(2);
  }
  const bin = process.env.GLANCE_BIN;
  if (!bin || !existsSync(bin)) {
    console.error(`${label}: no daemon at GLANCE_BIN=${bin || "<unset>"} — SKIPPED`);
    process.exit(0);
  }
  return { broke, bin };
}

/** BRING A DAEMON UP on PORT over DIR and wait its walk out.  Readiness is the
 * route that NEEDS the store: the bind lands before the walk ends.  Everything
 * the child says goes to ONSAID -- held rather than printed, or a `CloseRequest'
 * per closed socket would bury the report.  Answers with the child, the base URL
 * and the boot reading, which each driver counts rows off for itself. */
export async function serveDaemon(bin, dir, port, onSaid) {
  const child = spawn(bin, ["serve", "--dir", dir, "--port", String(port)],
                      { stdio: ["ignore", "pipe", "pipe"] });
  child.stdout.on("data", (d) => onSaid(String(d)));
  child.stderr.on("data", (d) => onSaid(String(d)));
  child.on("error", (e) => { throw e; });
  const base = `http://127.0.0.1:${port}`;
  const boot = await polling(TURN)(async () => {
    const r = await fetch(`${base}/headlines?limit=100`).catch(() => null);
    return r && r.status === 200 ? r.json() : null;
  }, READY, "the daemon to finish its walk");
  return { child, base, boot };
}
