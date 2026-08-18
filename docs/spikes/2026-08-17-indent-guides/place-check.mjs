// TWO PLACEMENT TRAPS, both of which shipped once and were seen before they were
// understood:
//
//   SETTLED   a variant with a strip writes it into the pane's own flow, so
//             filling it moves every row below.  Marks measured first sit a line
//             high, and the page looks right again the moment a key is pressed.
//             A repaint that changes nothing must change nothing.
//   COLUMN    a mark is at one tab stop per level and at ONE x per level: a
//             paragraph, the list and a top-level item are all at the outermost
//             level and must mark in the same place.
//
//   node place-check.mjs
import { firefox } from "./bidi.mjs";
import { resolve } from "node:path";

const RAILED = ["b-active.html", "c-bracket.html", "e-ancestry.html",
                "g-ancestry-path.html", "h-no-ground.html", "i-hook.html",
                "j-brackets.html", "k-tree.html",
                "l-marker.html", "m-under.html",
                "n-gutter.html", "o-weight.html",
                "p-attention.html"];

const marks = () => [...document.querySelectorAll("#rails i, #ladder u")].map(
  (i) => (i.className || "-") + "@" + Math.round(parseFloat(i.style.left || 0))
       + "+" + Math.round(parseFloat(i.style.top || 0))
       + "/" + Math.round(parseFloat(i.style.height || 0)));

const barX = () => {
  const bar = document.querySelector("#rails i.at");
  const probe = document.createElement("span");
  probe.style.cssText = "position:absolute;visibility:hidden;white-space:pre";
  probe.textContent = "0".repeat(100);
  document.getElementById("mdoc").appendChild(probe);
  const ch = probe.getBoundingClientRect().width / 100;
  probe.remove();
  return { x: bar ? Math.round(parseFloat(bar.style.left)) : null, ch: Math.round(ch * 100) / 100 };
};

const p = await firefox();
let bad = 0;

for (const page of RAILED) {
  await p.goto("file://" + resolve(page) + "?p=" + Date.now());
  await new Promise((r) => setTimeout(r, 400));
  const first = await p.eval(marks);
  const again = await p.eval(() => { RIG.repaint(); return null; });
  const settled = await p.eval(marks);
  const ok = JSON.stringify(first) === JSON.stringify(settled);
  console.log((ok ? "ok   " : "FAIL ") + "SETTLED " + page.padEnd(21)
              + (ok ? first.length + " marks, unmoved by a repaint"
                    : "first paint differs: " + JSON.stringify(first) + " vs "
                      + JSON.stringify(settled)));
  if (!ok) bad++;
}

// The stops, in the order the keys reach them from the mount at depth 2.
const STOPS = [
  ["depth 2", [], 2],
  ["depth 1", ["b"], 1],
  ["depth 0", ["b"], 0],
  ["the list", ["b"], 0],
  ["a paragraph", ["p"], 0],
  // The headline draws no bar at all -- HEAD is what checks its stars instead.
];
await p.goto("file://" + resolve("h-no-ground.html") + "?p=" + Date.now());
await new Promise((r) => setTimeout(r, 400));
const seen = [];
for (const [name, keys, level] of STOPS) {
  if (keys.length) await p.keys(keys);
  await new Promise((r) => setTimeout(r, 120));
  const { x, ch } = await p.eval(barX);
  seen.push({ name, level, x, ch });
}
const base = seen.find((s) => s.level === 0).x;
for (const s of seen) {
  const want = base + s.level * 2 * s.ch;
  const ok = s.x !== null && Math.abs(s.x - want) <= 1;
  console.log((ok ? "ok   " : "FAIL ") + "COLUMN  h-no-ground.html    "
              + s.name.padEnd(13) + "level " + s.level + " at x=" + s.x
              + " (want " + Math.round(want) + ")");
  if (!ok) bad++;
}

// OPAQUE   two rails share the outermost column -- the document's and the list's
//          -- so a translucent one composites darker along the list than beside a
//          paragraph, and the same unhighlighted mark reads as two styles.
await p.goto("file://" + resolve("h-no-ground.html") + "?p=" + Date.now());
await new Promise((r) => setTimeout(r, 400));
const inks = await p.eval(() => [...document.querySelectorAll("#rails i")].map((i) => {
  const c = getComputedStyle(i);
  // A gradient paints over the colour, so a rail is opaque when EITHER its colour
  // or its image covers the box with no see-through stop.
  return c.backgroundImage === "none" ? c.backgroundColor : c.backgroundImage;
}));
const clear = inks.filter((c) => /rgba\([^)]*,\s*(0?\.\d+|0)\s*\)|\btransparent\b/.test(c));
console.log((clear.length ? "FAIL " : "ok   ") + "OPAQUE  h-no-ground.html    "
            + inks.length + " rails"
            + (clear.length ? ", translucent: " + JSON.stringify(clear) : ", none translucent"));
if (clear.length) bad++;

// HEAD    SOLID IS THE ROW'S OWN TEXT, DASHED IS WHAT HANGS OFF IT.  An item with
//          a subtree is solid on its own line and broken under it; a paragraph
//          carries nothing and is solid over every line it wraps to.
await p.goto("file://" + resolve("h-no-ground.html") + "?p=" + Date.now());
await new Promise((r) => setTimeout(r, 400));
const readMark = () => {
  const at = [...document.querySelectorAll("#rails i.at")];
  const lh = parseFloat(getComputedStyle(document.getElementById("mdoc")).lineHeight);
  const stars = document.querySelector("#mdoc .de.dat.d-head .ds")
             || document.querySelector("#mdoc .de.dat.d-item > .dp > .dm");
  const root = getComputedStyle(document.documentElement).getPropertyValue("--g-bar").trim();
  const bar = (() => {                       // resolve the token to what it paints
    const probe = document.createElement("span");
    probe.style.color = root; document.body.appendChild(probe);
    const c = getComputedStyle(probe).color; probe.remove(); return c;
  })();
  return { n: at.length, lh: Math.round(lh),
           hs: at.map((i) => Math.round(parseFloat(i.style.height))),
           ws: at.map((i) => Math.round(parseFloat(getComputedStyle(i).width))),
           solid: at.map((i) => getComputedStyle(i).backgroundImage === "none"),
           stars: stars ? getComputedStyle(stars).color === bar : null };
};
for (const [name, keys, want] of [
  ["an item with a subtree", ["b", "b"], "split"],
  ["the list, whole", ["b"], "whole"],
  ["a 2-line paragraph", ["p"], "whole"],
  ["the headline", ["p"], "stars"],
]) {
  await p.keys(keys);
  await new Promise((r) => setTimeout(r, 150));
  const m = await p.eval(readMark);
  // The own text takes the BOLD stroke and what hangs off it the THIN one, both
  // solid; a headline draws no bar at all and turns its own stars gold.
  const ok = want === "split"
    ? m.n === 2 && Math.abs(m.hs[0] - m.lh) <= 2
      && m.solid.every(Boolean) && m.ws[0] === 3 && m.ws[1] === 1
      && m.stars === true          // and its own bullet is lit, like a headline's stars
    : want === "whole"
    ? m.n === 1 && m.solid[0] && m.ws[0] === 3 && m.hs[0] > m.lh
    : m.n === 0 && m.stars === true;
  console.log((ok ? "ok   " : "FAIL ") + "HEAD    h-no-ground.html    "
              + name.padEnd(22) + JSON.stringify(m));
  if (!ok) bad++;
}

// RAMP    a block's rail and its crumb must carry the SAME strength, or the strip
//          is a legend that misdirects by one level.  Both lists come out
//          outermost-first, so the rails are the tail of the crumbs.
for (const page of ["g-ancestry-path.html", "h-no-ground.html"]) {
  await p.goto("file://" + resolve(page) + "?p=" + Date.now());
  await new Promise((r) => setTimeout(r, 400));
  const v = await p.eval(() => ({
    rails: [...document.querySelectorAll("#rails i.up")].map((i) => i.style.getPropertyValue("--near")),
    crumbs: [...document.querySelectorAll("#path span")].map((c) => c.style.getPropertyValue("--near")),
  }));
  const tail = v.crumbs.slice(v.crumbs.length - v.rails.length);
  const ok = v.rails.length > 0 && JSON.stringify(v.rails) === JSON.stringify(tail);
  console.log((ok ? "ok   " : "FAIL ") + "RAMP    " + page.padEnd(21)
              + "rails " + JSON.stringify(v.rails) + " crumbs " + JSON.stringify(v.crumbs));
  if (!ok) bad++;
}

await p.close();
console.log(bad ? "\n" + bad + " FAILED" : "\nevery mark is where it says it is");
process.exit(bad ? 1 : 0);
