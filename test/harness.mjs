// WHAT BOTH DRIVERS NEED: `test/browser/drive.mjs' and `test/interop/drive.mjs'
// share `sleep', `freePort', `poll' and `end'.
import { createServer } from "node:net";

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
