// The complaint, mechanised: a variant must DRAW THE DIFFERENCE.  Three walks
// out of the grandchild's paragraph must change the picture each time (the
// mark reads point, never just structure), and TAB must take a block's marks
// with it and give back exactly what it took.
//
//   node check.mjs                # every variant
//   node check.mjs c-spine.html   # one
import { firefox, KEY } from "./bidi.mjs";
import { pathToFileURL } from "node:url";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ALL = ["a-shipped.html", "b-tree.html", "c-spine.html",
             "d-perforated.html", "e-bracket.html", "f-ramp.html"];
const picked = process.argv[2] ? [process.argv[2]] : ALL;

// The picture, read from whatever the variant draws: the overlay's own marks
// when it keeps one, the rows' ::before bars when it paints in CSS.  The
// cursor's ground is never read — it moves on every step and would make a
// blind variant look like a seeing one.
const picture = () => {
  const layer = document.getElementById("rails");
  if (layer) {
    return [...layer.children].map((i) => {
      const s = getComputedStyle(i);
      const r = i.getBoundingClientRect();
      return [Math.round(r.left), Math.round(r.top), Math.round(r.height),
              s.backgroundColor, s.borderLeftColor, s.borderLeftStyle].join("/");
    }).sort().join(";");
  }
  return [...document.querySelectorAll("#mdoc .de")].map((e) => {
    if (e.style.display === "none") return "";
    const s = getComputedStyle(e, "::before");
    const r = e.getBoundingClientRect();
    return [Math.round(r.top), s.backgroundColor, s.borderLeftColor].join("/");
  }).filter(Boolean).sort().join(";");
};

let failed = 0;
const ff = await firefox().catch((e) => {
  console.error("no firefox: " + e.message);
  process.exit(2);
});

for (const page of picked) {
  const bad = [];
  await ff.goto(pathToFileURL(join(HERE, page)).href);

  // WALK: grandchild's paragraph → grandchild → child → root headline.  The
  // three HEADLINE stops must each draw their own picture — depth must read
  // off the marks alone.  Being ON a headline vs INSIDE it is the ground's
  // job, so stop 0 only has to differ from the shelf above it.
  const sigs = [await ff.eval(picture)];
  for (let i = 0; i < 3; i++) {
    await ff.keys(["b"]);
    sigs.push(await ff.eval(picture));
  }
  const distinct = new Set(sigs.slice(1)).size;
  const flat = page === "a-shipped.html";
  if (!flat && distinct < 3)
    bad.push(`blind: ${distinct}/3 distinct pictures walking out`);
  if (!flat && sigs[0] === sigs[2])
    bad.push("the child's shelf reads the same in and out");

  // FOLD: on the child headline (two `b' in), TAB must change the picture and
  // a second TAB must restore it byte for byte.
  await ff.goto(pathToFileURL(join(HERE, page)).href);
  await ff.keys(["b", "b"]);
  const open = await ff.eval(picture);
  await ff.keys([KEY.Tab]);
  const shut = await ff.eval(picture);
  const hidden = await ff.eval(() =>
    [...document.querySelectorAll("#mdoc .de")]
      .filter((e) => e.style.display === "none").length);
  await ff.keys([KEY.Tab]);
  const back = await ff.eval(picture);
  if (hidden === 0) bad.push("TAB hid no rows");
  if (shut === open) bad.push("the fold did not change the picture");
  if (back !== open) bad.push("reopening did not restore the picture");

  // SETTLED: a repaint that changes nothing changes nothing.
  await ff.keys(["n"]);
  const a = await ff.eval(picture);
  await ff.eval(() => RIG.repaint());
  const b = await ff.eval(picture);
  if (a !== b) bad.push("a repaint moved the marks");

  if (bad.length) { failed += 1; console.log(`FAIL ${page}\n  ${bad.join("\n  ")}`); }
  else if (flat)
    // THE CONTROL IS FLAT BY CONSTRUCTION: at the root and at the child every
    // bar is lit the same, which is what the other tabs exist to answer.
    console.log(`flat ${page} (${distinct}/3 — the bars read the path, not the block)`);
  else console.log(`ok   ${page}`);
}

await ff.close();
process.exit(failed ? 1 : 0);
