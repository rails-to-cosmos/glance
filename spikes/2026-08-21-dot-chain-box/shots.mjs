// One PNG per variant, headless, each driven to the same moment: a completed
// call standing, the next one open on its own menu.  The README's table reads
// off these.
//
//   node shots.mjs                     # all seven
//   node shots.mjs f-typed-dsl.html    # one, when only that moment changed
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
// F IS SHOT ON ITS OWN IDIOM, and its own signatures: `.sort(columns = [Desc
// "Deadline"])' committed — the direction as a constructor applied to the name
// — the filter badge reopened by `/' with its record syntax and its `/=', and a
// second field opened onto its quoted slot so the value offers stand under the
// box beside the normal form: the constructor first, the literals with their
// counts under it.
const TYPED = [["."], ["s"], [KEY.Tab], [KEY.Tab], chars("Dead"),
               [KEY.ArrowDown], [KEY.Tab], [")", KEY.Enter],
               // `/' lands on a FRESH argument, comma and all, so the shot
               // types the condition alone.
               ["/"], chars("priority = ")];

// G IS SHOT ON ITS OWN CLAUSE LADDER: an `ORDER BY' committed, then `/'
// reopening the standing `WHERE' badge — dashed on the strip, its predicates
// open in the box with the `AND' the gesture appended — and one more column
// named so the operator offers stand under the box beside the statement and the
// normal form.
const SQLISH = [["."], ["o"], [KEY.Tab], chars("deadline DESC"), [";", KEY.Enter],
                ["/"], chars("planned ")];

const ALL = [
  ["a-control.html", "a-control.png", FLAT],
  ["b-plain-chain.html", "b-plain-chain.png", CHAIN],
  ["c-ide-chain.html", "c-ide-chain.png", CHAIN],
  ["d-stage-pills.html", "d-stage-pills.png", PILLS],
  ["e-echo-line.html", "e-echo-line.png", CHAIN],
  ["f-typed-dsl.html", "f-typed-dsl.png", TYPED],
  ["g-sql.html", "g-sql.png", SQLISH],
];
// ONE TAB'S MOMENT CAN MOVE ALONE — a legend, a key's answer — and the other
// five PNGs stay the bytes they were.
const SHOTS = process.argv[2] ? ALL.filter((s) => s[0] === process.argv[2]) : ALL;

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
