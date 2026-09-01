// The complaint, mechanised: a fold must SHOW.  TAB on the child must hide
// rows and change the picture, a second TAB must give back exactly what it
// took, and a repaint that changes nothing changes nothing.
//
//   node check.mjs                  # every variant
//   node check.mjs e-stub.html      # one
import { firefox, KEY } from "./bidi.mjs";
import { pathToFileURL, fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const HERE = dirname(fileURLToPath(import.meta.url));
const ALL = ["a-ellipsis.html", "b-signs.html", "c-chevron.html",
             "d-count.html", "e-stub.html"];
const picked = process.argv[2] ? [process.argv[2]] : ALL;

// The picture: the spines, the variant's marks, and any text mark riding a
// folded title — never the cursor's ground.
const picture = () => {
  const bits = [];
  for (const layer of ["rails", "marks"]) {
    const el = document.getElementById(layer);
    if (el) for (const i of el.children) {
      const s = getComputedStyle(i);
      const r = i.getBoundingClientRect();
      bits.push([layer, Math.round(r.left), Math.round(r.top),
                 Math.round(r.height), i.textContent, s.backgroundColor,
                 s.borderTopColor, s.color].join("/"));
    }
  }
  for (const d of document.querySelectorAll("#mdoc .dfold"))
    bits.push("fold/" + d.textContent);
  return bits.sort().join(";");
};

let failed = 0;
const ff = await firefox().catch((e) => {
  console.error("no firefox: " + e.message);
  process.exit(2);
});

for (const page of picked) {
  const bad = [];
  await ff.goto(pathToFileURL(join(HERE, page)).href);

  // Onto the child headline, two `b' out of the grandchild's paragraph.
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
  const a = await ff.eval(picture);
  await ff.eval(() => RIG.repaint());
  const b = await ff.eval(picture);
  if (a !== b) bad.push("a repaint moved the marks");

  if (bad.length) { failed += 1; console.log(`FAIL ${page}\n  ${bad.join("\n  ")}`); }
  else console.log(`ok   ${page}`);
}

await ff.close();
process.exit(failed ? 1 : 0);
