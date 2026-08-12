// THE INTEROP DRIVER — ONE STORE, TWO PROGRAMS, BOTH DIRECTIONS.
//
// glance and org-glance each test their own half of the EXTERNAL.jsonl contract
// and neither has ever fed the other.  This stands up ONE org-glance store,
// lets Emacs seed it, lets the daemon write it, and asks Emacs what it sees —
// then turns the traffic around and lets Emacs write while the daemon watches.
// Every case below names the CLAIM it closes, from the reader's census of what
// was "asserted by NEITHER".
//
// THE ORDER IS THE STORY: one store carries every case, and a step's assertion
// is about the state the step before it left.  A
// failure is reported and the run continues, because the steps after it are
// still evidence — but the FIRST red line is the one to read.
//
// ZERO DEPENDENCIES: node's global fetch and WebSocket, `emacs -Q -batch', and
// the daemon.  Teardown is ONE function reached by the normal exit, by a thrown
// case and by a signal alike, and it is the target's life — a leaked daemon or
// a temp store on a red run is what gets a check deleted.
//
//   GLANCE_BIN   the daemon to serve with       (else `cabal list-bin')
//   OG_HOME      the org-glance checkout        (else ../org-glance)
//   EMACS_RUN    `host' (default) or `podman'
//   BREAK=name   take ONE step out of the harness — see `BREAKS'
//   KEEP=1       leave the store behind, named in the report
//
// There is no `ONLY': the cases share one store and one order, so a case cut
// out of the middle would leave the ones after it asking about a state that
// was never reached.

import { spawn, spawnSync } from "node:child_process";
import { mkdtemp, rm, readFile } from "node:fs/promises";
import { existsSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createServer } from "node:net";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..");
const TURN = 25;              // the poll, in ms — the watch's own drain rate
const READY = 30_000;         // the daemon's walk, capped
const WATCH = 10_000;         // a watch step: debounce + parse + publish, capped

// The two ids the seed mints by hand.  Spelled as org's own uuid form, since
// that is what `org-id-uuid' produces and what both sides shard by.
const ALPHA = "aaaa1111-2222-3333-4444-555566667777";
const BETA = "bbbb1111-2222-3333-4444-555566667777";
const TAG = "book";

// ---------------------------------------------------------------- utilities

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

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
    if (Date.now() > till)
      throw new Error(`waited ${cap}ms for ${what} (last saw: ${JSON.stringify(last)})`);
    await sleep(TURN);
  }
}

function eq(got, want, what) {
  const a = JSON.stringify(got), b = JSON.stringify(want);
  if (a !== b) throw new Error(`${what}: got ${a}, wanted ${b}`);
  return got;
}

function ok(cond, what) {
  if (!cond) throw new Error(what);
  return true;
}

// ------------------------------------------------------------------- breaks
//
// A CASE NOBODY HAS SEEN FAIL IS NOT EVIDENCE, and browser-check's `BREAK'
// is the idiom: each entry takes ONE step out of the HARNESS and names the
// case that must turn red.  These prove the assertions are not vacuous —
// that they read what the other program did rather than what this one set up.
// They do NOT prove the daemon's wiring; for that, break the daemon (the
// target's own comment names the line).
const BREAKS = {
  // Nothing is written, so there is nothing for Emacs to adopt.
  "no-write": "external-bytes",
  // The line is left standing, and only Emacs's fold moves the WAL.
  "no-refresh": "emacs-adopts",
  // Emacs never writes, so the watch has nothing to deliver.
  "no-put": "browser-sees-emacs",
  // Emacs is asked where a DIFFERENT id lives.
  "wrong-id": "blob-path-agrees",
  // Something writes the WAL behind glance's back.
  "meta-moved": "meta-untouched",
  // The tombstone line is left standing, so the record it names stays live.
  "no-delete-fold": "delete-tombstones-the-record",
};

// ------------------------------------------------------------- the two sides

/**
 * The peer's own HEAD, short.  A CONTRACT is asserted against a VERSION of the
 * other program, and the report is the only place that fact can be read — so a
 * case that goes red after the peer moved says which peer it was measured on.
 * The checkout stays a sibling rather than a submodule: the deps beside it are
 * eask's and a pin would not bring them, and both repos move in one sitting.
 */
function peerHead(ogHome) {
  const r = spawnSync("git", ["-C", ogHome, "describe", "--always", "--dirty"],
                      { encoding: "utf8" });
  return (r.stdout || "").trim() || "unknown";
}

/** Run ARGV, returning {code, out, err}; never throws on a non-zero exit. */
function run(argv, env) {
  const r = spawnSync(argv[0], argv.slice(1), {
    env: { ...process.env, ...env }, encoding: "utf8", maxBuffer: 32 << 20,
  });
  if (r.error) throw r.error;
  return { code: r.status, out: r.stdout || "", err: r.stderr || "" };
}

/**
 * The Emacs half.  `host' is `emacs -Q -batch' on PATH — fast, and the peer is
 * a sibling checkout; `podman' is org-glance's OWN Containerfile, pinned, with
 * the store bind-mounted at ITS OWN PATH so every path string this harness
 * compares means the same thing on both sides of the mount.
 */
function emacsRunner(mode, ogHome, image) {
  if (mode === "podman")
    return (root, env) => [
      ["podman", "run", "--rm",
        "-v", `${root}:${root}:Z`,
        "-v", `${HERE}:${HERE}:ro,Z`,
        ...Object.entries({ OG_HOME: "/work", IROOT: root, ...env })
          .flatMap(([k, v]) => ["-e", `${k}=${v}`]),
        image, "emacs", "-Q", "-batch", "-l", join(HERE, "og.el")],
      {},
    ];
  return (root, env) => [
    ["emacs", "-Q", "-batch", "-l", join(HERE, "og.el")],
    { OG_HOME: ogHome, IROOT: root, ...env },
  ];
}

/**
 * ONE STEP OF THE EMACS HALF.  Its stdout is one JSON object; its stderr is
 * org-glance's own `message' output, which a failure prints because that is
 * where "refresh-external skips X" lives.
 */
function emacs(runner, root, cmd, env = {}) {
  const [argv, e] = runner(root, { ICMD: cmd, ...env });
  const r = run(argv, e);
  const line = r.out.trim().split("\n").filter(Boolean).pop();
  if (r.code !== 0 || !line)
    throw new Error(`emacs ${cmd} exited ${r.code}\n${r.out}\n${r.err}`);
  let said;
  try { said = JSON.parse(line); }
  catch { throw new Error(`emacs ${cmd} said no JSON: ${line}\n${r.err}`); }
  if (said.error) throw new Error(`emacs ${cmd}: ${said.error}\n${r.err}`);
  said.$said = r.err.trim();
  return said;
}

/** POST /command, returning the parsed answer whatever the status. */
async function command(base, body) {
  const r = await fetch(`${base}/command`, { method: "POST", body: JSON.stringify(body) });
  return { status: r.status, ...(await r.json()) };
}

const getJSON = async (url) => {
  const r = await fetch(url);
  if (!r.ok) throw new Error(`${url} answered ${r.status}`);
  return r.json();
};

/** `glance scan ROOT' as lines, so the index verdict can be read off it. */
function scan(bin, root) {
  const r = run([bin, "scan", root], {});
  if (r.code !== 0) throw new Error(`glance scan exited ${r.code}\n${r.err}`);
  return r.out.split("\n").map((l) => l.trim());
}

/**
 * The one line of SCAN's output opening with WHAT, with its column padding
 * collapsed: the report lays its labels out in a fixed width, which is a
 * question about the report rather than about the index.
 */
function scanLine(lines, what) {
  const hit = lines.find((l) => l.startsWith(what));
  if (!hit) throw new Error(`glance scan printed no "${what}" line:\n${lines.join("\n")}`);
  return hit.replace(/\s+/g, " ").trim();
}

// ---------------------------------------------------------------- the socket

/**
 * A CLIENT LIKE THE BROWSER'S: `?bootstrap=off', so every frame it holds is a
 * frame the WATCH produced rather than the snapshot a subscription opens with.
 */
async function socket(base) {
  const ws = new WebSocket(`${base.replace("http", "ws")}/ws?bootstrap=off`);
  const frames = [];
  await new Promise((yes, no) => {
    ws.addEventListener("open", yes, { once: true });
    ws.addEventListener("error", () => no(new Error("the socket never opened")), { once: true });
  });
  ws.addEventListener("message", (m) => frames.push(JSON.parse(m.data)));
  return {
    frames,
    /** Wait for a frame WANT accepts, or fail naming what never arrived. */
    until: (want, what, cap = WATCH) =>
      poll(() => frames.find(want) || false, cap, what),
    close: () => { try { ws.close(); } catch { /* already gone */ } },
  };
}

// ------------------------------------------------------------------ the run

async function main() {
  const keep = process.env.KEEP === "1";
  const broke = process.env.BREAK || "";
  if (broke && !BREAKS[broke]) {
    console.error(`interop: no break named "${broke}" — `
      + `try one of: ${Object.keys(BREAKS).join(", ")}`);
    process.exit(2);
  }
  const bin = process.env.GLANCE_BIN;
  if (!bin || !existsSync(bin)) {
    console.error(`interop: no daemon at GLANCE_BIN=${bin || "<unset>"} — SKIPPED`);
    process.exit(0);
  }
  const ogHome = process.env.OG_HOME || resolve(REPO, "..", "org-glance");
  // The target guards this too; the driver keeps its own so `node drive.mjs'
  // skips rather than crashing halfway through a store it made.
  if (!existsSync(join(ogHome, "src", "data"))) {
    console.error(`interop: no org-glance checkout at ${ogHome} — SKIPPED`);
    process.exit(0);
  }
  const mode = process.env.EMACS_RUN === "podman" ? "podman" : "host";
  const image = process.env.OG_IMAGE || "org-glance-test:emacs-29.1";
  const runner = emacsRunner(mode, ogHome, image);

  // THE PORT IS TAKEN BEFORE THE STORE IS MADE, so nothing that can throw sits
  // between the temp directory and the `try' whose teardown removes it.
  const port = await freePort();
  const base = `http://127.0.0.1:${port}`;
  const root = await mkdtemp(join(tmpdir(), "glance-interop-"));
  const store = join(root, ".org-glance");
  const results = [];
  let daemon = null, ws = null, daemonSaid = "";
  const started = Date.now();

  /**
   * TEARDOWN IS ONE PATH AND EVERY EXIT REACHES IT.  A `finally' covers the
   * normal end and a thrown case and covers no signal at all, which is how a
   * run piped through `head' left `glance serve' holding a temp store for
   * hours.  So this is both the `finally' body and the signal handler, and it
   * runs AT MOST ONCE: the first caller keeps the promise and every later one
   * awaits that same promise, since a SIGINT can land while the normal path is
   * already inside `rm'.
   */
  let torn = null;
  const teardown = () => (torn ??= (async () => {
    if (ws) { try { ws.close(); } catch { /* already gone */ } }
    if (daemon) await end(daemon);
    if (!keep) await rm(root, { recursive: true, force: true }).catch(() => {});
  })());

  // A SIGNAL IS AN EXIT LIKE ANY OTHER.  `head' closing the pipe is the case
  // that got here, and node reports THAT as an EPIPE on stdout rather than as a
  // signal — an unhandled one is an uncaught exception, which ends the process
  // with the `finally' unrun — so it is wired beside the two.  Each leaves by
  // the conventional 128+N.  Nothing is printed on the way out: the pipe this
  // was reached through may be the thing that is gone.
  const bye = (code) => {
    const leave = () => process.exit(code);
    teardown().then(leave, leave);
  };
  process.once("SIGINT", () => bye(130));
  process.once("SIGTERM", () => bye(143));
  process.stdout.on("error", (e) => { if (e.code === "EPIPE") bye(141); });

  /** Record one case.  FN returns the lines the report prints under it. */
  const step = async (name, claim, fn) => {
    const at = Date.now();
    try {
      const said = (await fn()) || [];
      results.push({ ok: true, name, claim, said, ms: Date.now() - at });
      console.log(`ok   ${results.length} — ${name}  [${claim}]`);
      for (const w of said) console.log(`       ${w}`);
    } catch (e) {
      results.push({ ok: false, name, claim, why: e.message, ms: Date.now() - at });
      console.log(`not ok ${results.length} — ${name}  [${claim}]`);
      console.log(`       ${e.message.split("\n").join("\n       ")}`);
    }
  };
  const taken = (what) => broke === what;

  try {
    // ------------------------------------------------------ seed, then serve
    const seeded = emacs(runner, root, "seed",
      { ITAG: TAG, IALPHA: ALPHA, IBETA: BETA });
    console.log(`interop: ${mode} emacs, org-glance ${peerHead(ogHome)}, `
      + `store ${seeded.store}\n`);

    daemon = spawn(bin, ["serve", "--dir", root, "--port", String(port)],
                   { stdio: ["ignore", "pipe", "pipe"] });
    daemon.stdout.on("data", (d) => { daemonSaid += d; });
    daemon.stderr.on("data", (d) => { daemonSaid += d; });
    daemon.on("error", (e) => { throw e; });
    const boot = await poll(async () => {
      const r = await fetch(`${base}/headlines?limit=100`).catch(() => null);
      return r && r.status === 200 ? r.json() : null;
    }, READY, "the daemon to finish its walk");
    ok(boot.rows.length === 2,
       `the daemon served ${boot.rows.length} rows off the seeded store, wanted 2`);

    // Asked FIRST, while nothing has been archived and nothing deleted: what a
    // tree org-glance built looks like to the walk.
    await step("sidecars-are-not-rows", "CLAIM 20", async () => {
      const all = await getJSON(`${base}/headlines?limit=20000`);
      const files = [];
      for (const row of all.rows)
        files.push((await getJSON(`${base}/headline?id=${encodeURIComponent(row.id)}`)).file);
      eq(files.length, 2, "the rows a store org-glance wrote yields");
      for (const f of files) {
        ok(f.startsWith(join(store, "data") + "/"), `a row served off ${f}`);
        for (const derived of ["/meta/", "/config/", "/trash/", "/occurrences/"])
          ok(!f.includes(derived), `a row served out of ${derived}: ${f}`);
      }
      return [`${files.length} rows, both under ${join(store, "data")}`,
              `nothing out of meta/, config/, trash/ or occurrences/`];
    });

    // --------------------------------------------------------- direction one
    // glance writes a blob; org-glance adopts it through EXTERNAL.jsonl.

    await step("blob-path-agrees", "CLAIM 4", async () => {
      const asked = taken("wrong-id") ? "0000dead-0000-0000-0000-000000000000" : ALPHA;
      const mine = await getJSON(`${base}/headline?id=${ALPHA}`);
      const theirs = emacs(runner, root, "paths", { IID: asked });
      eq(mine.file, theirs.content, "the file the two name for one id");
      return [`both name ${mine.file}`,
              `org-glance's external path: ${theirs.external}`];
    });

    const before = emacs(runner, root, "meta-digest").files;

    await step("external-bytes", "CLAIM 2 + CLAIM 3", async () => {
      if (!taken("no-write")) {
        const r = await command(base, { name: "set-state", ids: [ALPHA],
                                        args: { keyword: "DONE" } });
        eq(r.results[0].ok, true, "set-state over the blob");
      }
      const seen = emacs(runner, root, "read-external");
      // CLAIM 3: the file org-glance's own `external-path' names is the file
      // glance appended to — asked by reading it THROUGH that accessor.
      eq(seen.exists, true, "org-glance finds a notification file where it looks");
      // CLAIM 2: the bytes glance produced are the bytes org-glance's reader
      // accepts, and the ids it takes out of them are glance's own.
      eq(Array.from(seen.ids), [ALPHA], "the ids org-glance reads out of the file");
      const raw = await readFile(join(store, "meta", "EXTERNAL.jsonl"), "utf8");
      eq(seen.text, raw, "the text org-glance read vs the file on disk");
      ok(/^\{"id":"[^"]+","at":"\d{4}-\d\d-\d\dT\d\d:\d\d:\d\dZ"\}\n$/.test(raw),
         `the frozen line shape: ${JSON.stringify(raw)}`);
      return [`org-glance read ${seen.bytes} B and took ${seen.ids.length} id`,
              `line: ${raw.trim()}`];
    });

    await step("meta-untouched", "CLAIM 21", async () => {
      if (taken("meta-moved"))
        run(["sh", "-c", `printf '\\n' >> ${join(store, "meta", "headlines.jsonl")}`], {});
      const digest = (rows) => Object.fromEntries(rows.map((l) => l.split(" ")));
      const was = digest(before), now = digest(emacs(runner, root, "meta-digest").files);
      const gone = Object.keys(was).filter((n) => !(n in now));
      const changed = Object.keys(was).filter((n) => n in now && was[n] !== now[n]);
      const added = Object.keys(now).filter((n) => !(n in was));
      eq(gone, [], "the meta/ files a glance write removed");
      eq(changed, [], "the meta/ files a glance write rewrote");
      eq(added, ["EXTERNAL.jsonl"], "the meta/ files a glance write created");
      return [`${Object.keys(was).length} files stood byte-identical: `
                + Object.keys(was).join(", "),
              `EXTERNAL.jsonl is the one thing that appeared`];
    });

    await step("emacs-adopts", "CLAIM 5", async () => {
      const n = taken("no-refresh") ? { n: 0 } : emacs(runner, root, "refresh");
      eq(n.n, 1, "the entries org-glance's refresh folded");
      const field = emacs(runner, root, "field", { IID: ALPHA });
      eq(field.state, "DONE", "the keyword org-glance reports after the fold");
      eq(field.title, "alpha", "the title, which the fold must not have moved");
      return [`refresh-external folded ${n.n}; the WAL now says DONE`];
    });

    await step("file-emptied-and-reused", "CLAIM 8", async () => {
      const spent = emacs(runner, root, "read-external");
      eq(spent.exists, true, "the file survives the fold");
      eq(spent.bytes, 0, "the bytes left after the fold");
      const r = await command(base, { name: "set-state", ids: [BETA],
                                      args: { keyword: "READING" } });
      eq(r.results[0].ok, true, "the second write, over the tagged blob");
      const again = emacs(runner, root, "read-external");
      eq(Array.from(again.ids), [BETA], "the ids the file holds after the second write");
      eq(again.inode, spent.inode, "the inode, which Emacs rewrote in place");
      return [`emptied to 0 B, inode ${spent.inode} kept, then ${again.bytes} B again`];
    });

    await step("tag-cycle-survives", "CLAIM 7", async () => {
      const n = emacs(runner, root, "refresh");
      eq(n.n, 1, "the entries folded for the tagged blob");
      const field = emacs(runner, root, "field", { IID: BETA });
      // A keyword only the tag's own `#+TODO:' declares: without the cycle in
      // scope org folds it into the TITLE, which is the failure this asks about.
      eq(field.state, "READING", "the keyword only book.org's cycle declares");
      eq(field.title, "beta", "the title, which must not have swallowed it");
      eq(Array.from(field.tags), [TAG], "the tags");
      return [`READING survived as a state under the tag's own cycle`];
    });

    // --------------------------------------------------------- direction two
    // org-glance writes a blob; the daemon's watch delivers the row.  No
    // notification file is involved in this direction at all — this is the leg
    // that rests entirely on `watchOrgTree', which no offline suite drives.

    await step("browser-sees-emacs", "CLAIM 19", async () => {
      ws = await socket(base);
      const wrote = `* DONE alpha, edited in emacs\n:PROPERTIES:\n`
        + `:ORG_GLANCE_ID: ${ALPHA}\n:END:\n`;
      // THE LATENCY IS THE WATCH'S, so it is measured from the clock Emacs read
      // once the rename had landed — a batch Emacs costs half a second to
      // start, and none of that is the daemon's.
      const put = taken("no-put") ? { at: Date.now() }
        : emacs(runner, root, "put", { IID: ALPHA, ITEXT: wrote });
      const frame = await ws.until(
        (f) => f.op === "upsert-row" && f.row.id === ALPHA
               && f.row.cells.title === "alpha, edited in emacs",
        "the watch to deliver org-glance's own write as a row");
      const ms = Date.now() - put.at;
      const empty = emacs(runner, root, "read-external");
      eq(empty.bytes, 0, "the notification file, which this direction never uses");
      const served = await getJSON(`${base}/headlines?limit=20000`);
      eq(served.rows.find((r) => r.id === ALPHA).cells.title, "alpha, edited in emacs",
         "the row /headlines serves after the watch step");
      return [`put-content -> upsert-row in ${ms} ms, with EXTERNAL.jsonl untouched`,
              `frame: ${JSON.stringify(frame.row.cells)}`];
    });

    // ---------------------------------------------------- both sides at rest
    // Everything above has moved; these two ask whether the WAL and the blobs
    // still describe one another.

    await step("archive-flag-round-trips", "CLAIM 14", async () => {
      const r = await command(base, { name: "archive", ids: [ALPHA], args: {} });
      eq(r.results[0].ok, true, "archive over the blob");
      eq(emacs(runner, root, "refresh").n, 1, "the entries folded for the archive");
      const field = emacs(runner, root, "field", { IID: ALPHA });
      eq(field.archived, true, "the archive flag org-glance derived from the blob");
      const lines = scan(bin, root);
      // org-glance has no boolean kind: a false flag serializes as `{}', which
      // is what glance's `(eq t VALUE)' reading exists for.  A true one has to
      // read back as true through the same reading.
      eq(scanLine(lines, "org-glance index:"),
         "org-glance index: 0 rows disagree (0 state, 0 archived)",
         "glance's verdict on the flag org-glance serialized");
      return [`org-glance wrote the flag; glance read it back with no drift`,
              scanLine(lines, "records"), scanLine(lines, "blobs")];
    });

    await step("scan-agrees-with-the-writer", "CLAIM 15", async () => {
      const lines = scan(bin, root);
      eq(scanLine(lines, "org-glance index:"),
         "org-glance index: 0 rows disagree (0 state, 0 archived)",
         "glance's fold of a store org-glance wrote");
      eq(scanLine(lines, "unmatched"),
         "unmatched 0 unindexed blobs, 0 records without blobs",
         "blobs and records, matched both ways");
      eq(scanLine(lines, "span violations"), "span violations 0",
         "spans over org-glance's own blobs");
      return [scanLine(lines, "records"), scanLine(lines, "blobs"),
              scanLine(lines, "unmatched")];
    });

    // ------------------------------------------- the lifecycle at both ends
    // Create and delete were the two blob-lifecycle events the notification
    // file did not carry.  DELETE IS CLOSED — a third field, `"tombstone":true',
    // and the case below reads the record org-glance dropped for it.  CREATE IS
    // STILL A HOLE and is pinned AS IT STANDS: a fresh id names no record, so
    // the fold has nothing to replace and drops the line.  Closing it turns its
    // case red, which is the point — it names the decision rather than letting
    // it drift.

    await step("HOLE: a tagged capture never reaches the WAL", "CLAIM 17", async () => {
      const made = await command(base, { name: "capture",
                                         args: { text: "gamma from the browser", tag: TAG } });
      ok(made.ok, `the capture: ${JSON.stringify(made)}`);
      const seen = emacs(runner, root, "read-external");
      eq(Array.from(seen.ids), [made.id], "the id the capture notified");
      // …and the fold drops it, because there is no record to replace.
      const fold = emacs(runner, root, "refresh");
      eq(fold.n, 0, "the entries a fresh id folds");
      const field = emacs(runner, root, "field", { IID: made.id });
      eq(field.live, false, "whether org-glance holds the captured entry");
      const table = await getJSON(`${base}/headline?id=${encodeURIComponent(made.id)}`);
      ok(table.file.startsWith(join(store, "data")), "glance serves it as a row");
      return [`glance made ${made.id} and serves it; org-glance holds nothing`,
              `org-glance said: ${fold.$said || "(nothing)"}`];
    });

    await step("delete-tombstones-the-record", "CLAIM 18", async () => {
      const armed = await command(base, { name: "archive", ids: [BETA], args: {} });
      eq(armed.results[0].ok, true, "the archive that a delete is the step after");
      eq(emacs(runner, root, "refresh").n, 1, "the archive folded");
      const gone = await command(base, { name: "delete", ids: [BETA], args: {} });
      eq(gone.results[0].ok, true, "the delete");
      // THE THIRD FIELD, and it is the whole of what a delete adds to the wire:
      // the frozen two fields, then `"tombstone":true' before the brace.
      const raw = await readFile(join(store, "meta", "EXTERNAL.jsonl"), "utf8");
      ok(new RegExp(`^\\{"id":"${BETA}","at":"\\d{4}-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\dZ"`
                    + `,"tombstone":true\\}\\n$`).test(raw),
         `the frozen delete line: ${JSON.stringify(raw)}`);
      // And this is org-glance READING those bytes, through its own accessor.
      const seen = emacs(runner, root, "read-external");
      eq(Array.from(seen.ids), [BETA], "the ids org-glance takes out of the delete line");
      eq(Array.from(seen.kinds), ["tombstone"], "the kind org-glance reads the line as");
      const folded = taken("no-delete-fold") ? { n: 0 } : emacs(runner, root, "refresh");
      eq(folded.n, 1, "the entries a delete folds");
      const spent = emacs(runner, root, "read-external");
      eq(spent.bytes, 0, "the notification file after the fold");
      const field = emacs(runner, root, "field", { IID: BETA });
      eq(field.live, false, "whether org-glance still holds a live record for it");
      eq(field.tombstone, true, "whether org-glance holds a tombstone for it");
      eq(emacs(runner, root, "content", { IID: BETA }).exists, false,
         "whether the blob the record named is still there");
      // A TOMBSTONED ID LEAVES THE FOLD (`Data.Org.Index.recordOf' answers
      // `Nothing' on it), so it can no longer be a record without a blob — and
      // glance reads back the tombstone org-glance wrote because glance asked.
      const lines = scan(bin, root);
      eq(scanLine(lines, "unmatched"),
         "unmatched 1 unindexed blobs, 0 records without blobs",
         "what glance's own instrument says with the delete leg closed");
      ok(scanLine(lines, "records").includes("1 tombstones"),
         `the tombstones glance's fold reads: ${scanLine(lines, "records")}`);
      return [`${BETA} is tombstoned in the WAL and its blob is in the trash`,
              `line: ${raw.trim()}`,
              scanLine(lines, "records"),
              scanLine(lines, "unmatched")
                + "  (the unindexed blob is the capture hole, still open)"];
    });

  } finally {
    await teardown();
  }

  // ---------------------------------------------------------------- report
  const failed = results.filter((r) => !r.ok);
  const wall = ((Date.now() - started) / 1000).toFixed(1);
  console.log("");
  for (const f of failed) {
    console.log(`not ok — ${f.name}  [${f.claim}]`);
    console.log(`     ${f.why.split("\n").join("\n     ")}`);
  }
  if (failed.length && daemonSaid.trim())
    console.log(`\nthe daemon said:\n${daemonSaid.trim().split("\n")
      .map((l) => `     ${l}`).join("\n")}`);
  console.log(`\n${results.length - failed.length}/${results.length} cases, `
    + `${wall}s wall, ${mode} emacs`
    + (keep ? `, store kept at ${root}` : ""));
  if (broke)
    console.log(`BREAK=${broke} — "${BREAKS[broke]}" is the case that should be red`);
  if (failed.length) console.log(`interop: ${failed.length} FAILED`);
  process.exit(failed.length ? 1 : 0);
}

/** SIGTERM, then SIGKILL at 5 s: nothing this run started outlives it. */
function end(proc) {
  if (proc.exitCode !== null || proc.signalCode) return Promise.resolve();
  return new Promise((yes) => {
    const hard = setTimeout(() => { try { proc.kill("SIGKILL"); } catch { /* gone */ } }, 5_000);
    proc.on("exit", () => { clearTimeout(hard); yes(); });
    try { proc.kill("SIGTERM"); } catch { clearTimeout(hard); yes(); }
  });
}

main().catch(async (e) => {
  console.error(`interop: the driver itself failed — ${e.stack || e.message}`);
  process.exit(1);
});
