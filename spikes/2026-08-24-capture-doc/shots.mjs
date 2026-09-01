// The five screenshots, headless — each tab at the moment that shows what it is
// for, plus the geometry the README quotes.  `node shots.mjs'.
//
// THE NUMBERS ARE MEASURED, NOT CHOSEN: the doc's own box where it stands, and
// how far the row under the capture moved.  A trade-off with no number on it is
// an opinion.
import { chromium, KEY } from "./cdp.mjs";
import { fileURLToPath, pathToFileURL } from "node:url";
import { dirname, join } from "node:path";

const HERE = dirname(fileURLToPath(import.meta.url));
const url = (f) => pathToFileURL(join(HERE, f)).href;
const type = (s) => [...s];
const CTRL = (...ks) => [{ down: "Ctrl" }, ...ks, { up: "Ctrl" }];

const page = await chromium();

/** The doc's own box where it stands, and how far it pushed the strip. */
const geom = () => page.eval(() => {
  const m = document.getElementById("mdoc");
  const b = m && m.getBoundingClientRect();
  const anchor = [...document.querySelectorAll("#tablewrap tbody tr")]
    .find((r) => r.textContent.includes("Read a date where"));
  return { w: b ? Math.round(b.width) : 0, h: b ? Math.round(b.height) : 0,
           anchor: anchor ? Math.round(anchor.getBoundingClientRect().top) : 0 };
});

async function shot(file, name, drive) {
  await page.goto(url(file));
  const base = (await geom()).anchor;
  await drive();
  await page.settle();
  const g = await geom();
  await page.shot(join(HERE, name + ".png"));
  console.log(name.padEnd(16)
    + (g.h ? "doc " + g.w + "×" + g.h + "px (" + (g.h / 21).toFixed(1)
             + " doc lines) · the row under it moved " + (g.anchor - base) + "px"
           : "no doc standing"));
}

// A — the sheet, at the moment the date ghost speaks: the whole draft on
// screen, the pair already filled, and the resolution riding after `fri'.
await shot("a-sheet.html", "a-sheet", async () => {
  await page.keys(["+"]);
  await page.keys(type("book"));
  await page.keys([KEY.Enter]);
  await page.keys([KEY.Escape]);
  await page.keys(["p", "p", "p", "p", KEY.Enter]);
  await page.keys(type("The Iliad"));
  await page.keys([KEY.Enter, "n", KEY.Enter]);
  await page.keys(type("Homer"));
  await page.keys([KEY.Enter]);
  await page.keys(CTRL("c", "s"));
  await page.keys(type("fri"));
});

// B — in place, at the moment the doc has grown under its own row and the rows
// beneath have been pushed down rather than covered.
await shot("b-in-place.html", "b-in-place", async () => {
  await page.keys(["+"]);
  await page.keys(type("meeting"));
  await page.keys([KEY.Enter, KEY.Escape]);
  await page.keys(["p", "p", "p", KEY.Enter]);
  await page.keys(type("Standup with Ada"));
});

// C — jot then grow, at the moment the line has LANDED: a real row, and the
// transient word beside it offering the doc.
await shot("c-jot-then-grow.html", "c-jot-then-grow", async () => {
  await page.keys(["+"]);
  await page.keys(type("book"));
  await page.keys([KEY.Enter]);
  await page.keys(type("Dune"));
  await page.keys([KEY.Enter]);
});

// D — escalation, at the moment before the moult: today's one-line form with
// the typed line in it and the structure keys named under it.
await shot("d-escalation.html", "d-escalation", async () => {
  await page.keys(["+"]);
  await page.keys([KEY.Enter]);
  await page.keys(type("Call the dentist"));
});

// E — the gallery, whole: three templates, each saying what picking it means.
await shot("e-gallery.html", "e-gallery", async () => {
  await page.keys(["+"]);
});

// ---- the table the README quotes ------------------------------------------
// THE DOC AT REST, per variant and per template — the draft as it ARRIVES,
// before anything is typed into it, which is the only moment two variants can
// be compared at.  `push' is how far the strip's first real row moved.
console.log("\nthe doc as it arrives\n");
for (const [file, name] of [["a-sheet.html", "A"], ["b-in-place.html", "B"]])
  for (const tag of ["", "book", "meeting"]) {
    await page.goto(url(file));
    const base = (await geom()).anchor;
    await page.keys(["+"]);
    if (tag) await page.keys(type(tag));
    await page.keys([KEY.Enter]);
    await page.settle();
    const g = await geom();
    console.log("  " + name + " · " + (tag || "inbox").padEnd(8)
      + String(g.w).padStart(5) + "×" + String(g.h).padStart(3) + "px  ("
      + (g.h / 21).toFixed(1).padStart(4) + " doc lines)  push "
      + String(g.anchor - base).padStart(4) + "px");
  }
const vp = await page.eval(() => ({ w: innerWidth, h: innerHeight }));
console.log("\n  viewport " + vp.w + "×" + vp.h);

await page.close();
