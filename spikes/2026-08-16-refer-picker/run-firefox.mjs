// Run a case file against real Firefox over WebDriver BiDi.
//   node run-firefox.mjs <page.html> <cases.mjs>
import { firefox } from "./bidi.mjs";
import { resolve } from "node:path";

const [pagePath, hash] = process.argv[2].split("#");
const base = "file://" + resolve(pagePath);
// A URL that differs only in its HASH is a same-document navigation, so the
// page would keep the previous case's state.  A per-case query forces a load.
const urlFor = (i) => base + "?case=" + i + (hash ? "#" + hash : "");
const { CASES } = await import(resolve(process.argv[3]));

const p = await firefox();
let fails = 0;
try {
  for (const [i, c] of CASES.entries()) {
    await p.goto(urlFor(i));
    let got, want, note = "";
    try { [got, want, note] = await c.run(p); }
    catch (e) { got = "threw: " + e.message; want = "(no throw)"; }
    const ok = JSON.stringify(got) === JSON.stringify(want);
    console.log((ok ? "ok   " : "not ok ") + c.name + (note ? "\n       " + note : ""));
    if (!ok) {
      fails++;
      console.log("       want " + JSON.stringify(want));
      console.log("       got  " + JSON.stringify(got));
    }
  }
} finally {
  await p.close();
}
console.log(fails ? `\n${fails} of ${CASES.length} FAILED` : `\n${CASES.length}/${CASES.length} cases`);
process.exit(fails ? 1 : 0);
