// THE COMPLAINT, MECHANISED: "it does not make any sense currently if I am on
// indent 1 or on indent 2."  Walk each variant to four depths and count how many
// DISTINCT pictures it draws.  Four means the four depths are told apart; one
// means the drawing says the same thing wherever point is.
//
//   node depth-check.mjs            # every variant
//   node depth-check.mjs b-active.html
import { firefox } from "./bidi.mjs";
import { resolve } from "node:path";

const PAGES = process.argv.slice(2).length ? process.argv.slice(2) : [
  "a-rails.html", "b-active.html", "c-bracket.html",
  "d-ladder.html", "e-ancestry.html", "f-path.html", "g-ancestry-path.html",
  "h-no-ground.html", "i-hook.html",
  "j-brackets.html", "k-tree.html", "l-marker.html", "m-under.html",
  "n-gutter.html", "o-weight.html",
  "p-attention.html",
];

// The rig mounts at depth 2; `b' climbs out and `f' descends.  The four stops
// are the same four rows in every variant, so the signatures are comparable.
const WALK = [[], ["b"], ["b"], ["f", "f", "n", "f"]];

// WHAT THE GUIDE SAYS, and never what the cursor's own ground says: the ground
// moves on every step and would make a blind variant look like a seeing one.
const signature = () => {
  const rails = [...document.querySelectorAll("#rails i")]
    .filter((i) => i.className)
    .map((i) => i.className + "@" + Math.round(parseFloat(i.style.left)))
    .sort();
  const rungs = [...document.querySelectorAll("#ladder u")].map((u) => u.className || "-");
  const path = (document.getElementById("path") || {}).textContent || "";
  const own = document.querySelector(".de.dat > .dp");
  const bg = own && getComputedStyle(own).backgroundImage !== "none"
    ? getComputedStyle(own).backgroundSize : "-";
  return JSON.stringify({ rails, rungs, path, bg });
};

const p = await firefox();
const rows = [];
for (const page of PAGES) {
  await p.goto("file://" + resolve(page) + "?d=" + Date.now());
  const seen = [];
  for (const step of WALK) {
    if (step.length) await p.keys(step);
    seen.push(await p.eval(signature));
  }
  const distinct = new Set(seen).size;
  const truth = await p.eval(() => document.getElementById("truth").textContent);
  rows.push({ page, distinct, truth });
  console.log(
    (distinct === 4 ? "ok   " : "flat ") + page.padEnd(21)
    + distinct + "/4 distinct   last stop: " + truth);
}
await p.close();
const flat = rows.filter((r) => r.distinct < 4);
console.log("\n" + (rows.length - flat.length) + " of " + rows.length
  + " variants tell the four depths apart"
  + (flat.length ? "; flat: " + flat.map((r) => r.page).join(", ") : ""));
