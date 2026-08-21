// One PNG per variant, headless, each driven to the same moment: a completed
// call standing, the next one open on its own menu.  The README's table reads
// off these.
//
//   node shots.mjs
import { firefox, KEY } from "./bidi.mjs";
import { pathToFileURL, fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const HERE = dirname(fileURLToPath(import.meta.url));
const chars = (s) => [...s];

// THE SAME MOMENT IN THE CHAINING TABS: `.filter(state:TODO, tag:web)' closed —
// the comma standing where a call's arguments separate — and `.sort(' open and
// waiting, ghost and all, its menu offering the columns.
const CHAIN = [["."], [KEY.Tab], chars("state:TODO, tag:web"),
               [")", "."], ["s"], [KEY.Tab]];
// The control has no chain to drive: the flat box, one letter in, showing the
// whole grammar — the narrowing keys and the shaping ones in one list.
const FLAT = [["."], chars("s")];
// D IS SHOT ON ITS OWN TWO KEYS: an order committed, then `/' reopening the
// standing filter badge — dashed on the strip, its parens open in the box with
// the caret at the end of what it already says.
const PILLS = [["."], ["s"], [KEY.Tab], chars("deadline"), [")", KEY.Enter], ["/"]];

const SHOTS = [
  ["a-control.html", "a-control.png", FLAT],
  ["b-plain-chain.html", "b-plain-chain.png", CHAIN],
  ["c-ide-chain.html", "c-ide-chain.png", CHAIN],
  ["d-stage-pills.html", "d-stage-pills.png", PILLS],
  ["e-echo-line.html", "e-echo-line.png", CHAIN],
];

const ff = await firefox().catch((e) => {
  console.error("no firefox: " + e.message);
  process.exit(2);
});

for (const [page, png, script] of SHOTS) {
  await ff.goto(pathToFileURL(join(HERE, page)).href);
  for (const step of script) await ff.keys(step);
  await ff.shot(join(HERE, png));
  console.log("shot " + png);
}

await ff.close();
