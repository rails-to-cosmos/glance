// Does the tabbed shell mount each variant over file://?
//   node shell-check.mjs
import { firefox } from "./bidi.mjs";
import { resolve } from "node:path";
const p = await firefox();
const base = "file://" + resolve("index.html");
let bad = 0;
for (const key of ["a", "b", "c", "d", "e", "f"]) {
  await p.goto(base + "#" + key);
  await new Promise((r) => setTimeout(r, 900));
  const seen = await p.eval(() => {
    const f = document.getElementById("stage");
    const d = f.contentDocument;
    return { src: f.getAttribute("src"),
             mounted: !!(d && d.querySelector(".de.dat")),
             marked: !!(d && (d.getElementById("rails") || d.getElementById("ladder")
                              || d.getElementById("path") || d.querySelector(".dp"))),
             tab: (document.querySelector('#tabs a[aria-current="page"]') || {}).textContent };
  });
  const ok = seen.mounted && seen.marked;
  console.log((ok ? "ok   " : "FAIL ") + "#" + key + " -> " + JSON.stringify(seen));
  if (!ok) bad++;
}
await p.close();
console.log(bad ? bad + " FAILED" : "\nthe shell mounts all six");
process.exit(bad ? 1 : 0);
