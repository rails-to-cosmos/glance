// The screenshots, headless — five moments per tab, plus every number this
// README quotes, plus a rung per key per variant.  `node shots.mjs'.
//
// THE NUMBERS ARE MEASURED, NOT CHOSEN: the box's footprint, how many cells wide
// and rows tall it is, how much of the surface it hides, whether the edited row's
// own title is under it, what runs off the viewport, and whether the widest ghost
// and the widest offer word fit.  A trade-off with no number on it is an opinion.
//
// THE LAST PASS IS A CHECK: every key of every variant driven, and what the rig
// says afterwards compared against what the law says it should.  The laws are
// the widget's, not the placement's — which is the point, since a placement that
// broke one would have disqualified itself.
//
// THE DAY IS PINNED at `?day=2026-09-12' so a screenshot taken tomorrow is the
// same screenshot.  One clock read per open is the rig's own law and the check
// asserts the page took it.
import { chromium, KEY } from "./cdp.mjs";
import { fileURLToPath, pathToFileURL } from "node:url";
import { dirname, join } from "node:path";

const HERE = dirname(fileURLToPath(import.meta.url));
const DAY = "2026-09-12";
const WIDE = [1366, 700];
const NARROW = [900, 700];
const url = (f) => pathToFileURL(join(HERE, f)).href + "?day=" + DAY;
const type = (s) => [...s];
const SHIFT = (...ks) => [{ down: "Shift" }, ...ks, { up: "Shift" }];

const VARIANTS = [
  ["0", "0-pane-as-shipped.html", "the pane, as shipped", "pane"],
  ["a", "a-below-the-cell.html", "below the cell", "table"],
  ["b", "b-over-the-cell.html", "over the cell", "table"],
  ["c", "c-card-below.html", "a card, chips", "table"],
  ["d", "d-beside-the-cell.html", "beside the cell", "table"],
];
const STAMP = "<2026-08-18 Tue>";
const RANGE = "<2026-08-18 Tue>--<2026-08-19 Wed>";

const page = await chromium();
let bad = 0;
const fail = (what) => { bad += 1; console.log("  FAIL  " + what); };
const ok = (what) => console.log("  ok    " + what);
const is = (what, got, want) =>
  (JSON.stringify(got) === JSON.stringify(want)
   ? ok(what)
   : fail(what + "\n          got  " + JSON.stringify(got)
          + "\n          want " + JSON.stringify(want)));
const yes = (what, got) => (got ? ok(what) : fail(what));

const rig = () => page.eval(() => window.RIG_TEST.state());
const caret = () => page.eval(() => window.RIG_TEST.caret());
const foot = () => page.eval(() => window.RIG_TEST.footprint());
const covers = () => page.eval(() => window.RIG_TEST.covers());
const clip = () => page.eval(() => window.RIG_TEST.clip());
const fits = () => page.eval(() => window.RIG_TEST.fits());
const geom = () => page.eval(() => window.RIG_TEST.geom());

async function open(file, size) {
  await page.resize(...(size || WIDE));
  await page.goto(url(file));
  await page.settle();
}
/** `RET' where the cursor already stands — the cell, or the planning entry. */
const RET = async () => { await page.keys([KEY.Enter]); await page.settle(); };
const shot = (name) => page.shot(join(HERE, name + ".png"));
const pad = (n, w) => String(n).padStart(w);

// ---------------------------------------------------------------- the anchor
console.log("\nthe anchor each box is given\n");
await open("a-below-the-cell.html");
{
  const f = await fits();
  console.log("  a date column           " + f.cell + "px, its text run " + f.cellText + "px"
    + "   `calc(13ch + 24px)' at the table's own 13px face");
  console.log("  ` → " + STAMP + "'   " + f.ghostOne + "px   the ghost over one day");
  console.log("  ` → " + RANGE + "'   " + f.ghostRange + "px   the ghost over a range");
  console.log("  `18 september'          " + f.september + "px   the widest word the offers hold");
  console.log("  the offers' own floor   " + f.offersMin + "px   `min-width:12em' (page.css:818)");
  console.log("\n  THE CELL IS " + f.cell + "px AND THE WIDEST GHOST IS " + f.ghostRange
    + "px.  That is the whole reason this spike exists:");
  console.log("  a box that stood INSIDE the cell would be " + (f.ghostRange - f.cellText)
    + "px short before a key was pressed.");
}
await open("0-pane-as-shipped.html");
{
  const r = await page.eval(() => window.RIG_TEST.room());
  console.log("\n  the PANE's anchor       " + r.col + "px — the planning value's own slot,");
  console.log("  and the box it gets     runs from that slot's left edge to the end of the line's block");
}

// ------------------------------------------------------------ the five shots
console.log("\nthe five moments, per tab\n");
const TABLE = [];
for (const [tag, file, what, surface] of VARIANTS) {
  const row = { tag, what, surface };

  // 1. AT REST, straight after the open, the ISO day wholly selected.
  await open(file);
  await RET();
  {
    const c = await caret();
    const f = await foot(), cv = await covers(), x = await clip();
    row.rest = { box: f.box.w + "×" + f.box.h, all: f.all.w + "×" + f.all.h,
                 cells: cv.cells, rows: cv.rowsTall, share: cv.share,
                 title: cv.title, off: x };
    row.opened = c.value;
    row.selected = c.start === 0 && c.end === c.value.length;
    await shot(tag + "-rest");
  }

  // 2. `18 au' — the offers open, a month word half typed.
  await page.keys([KEY.Backspace]);
  await page.keys(type("18 au"));
  await page.settle();
  {
    const r = await rig(), f = await foot(), cv = await covers(), x = await clip();
    row.offers = { list: r.offers, box: f.box.w + "×" + f.box.h,
                   menu: f.offers ? f.offers.w + "×" + f.offers.h : "none",
                   all: f.all.w + "×" + f.all.h, cells: cv.cells,
                   rows: cv.rowsTall, share: cv.share, title: cv.title, off: x };
    row.writingGhost = r.ghost;
    await shot(tag + "-offers");
  }

  // 3. `18 aug' — the ghost, whole.
  await page.keys(type("g"));
  await page.settle();
  {
    const r = await rig(), f = await foot(), cv = await covers(), x = await clip();
    row.ghost = { said: r.ghost, box: f.box.w + "×" + f.box.h,
                  all: f.all.w + "×" + f.all.h, cells: cv.cells,
                  rows: cv.rowsTall, share: cv.share, title: cv.title, off: x };
    await shot(tag + "-ghost");
  }

  // 4. THE REFUSAL.  `18 auk' is no date, and `RET' leaves the box open.
  await open(file);
  await RET();
  await page.keys([KEY.Backspace]);
  await page.keys(type("18 auk"));
  await page.keys([KEY.Enter]);
  await page.settle();
  {
    const r = await rig(), f = await foot();
    row.refusal = { said: r.ghost, bad: r.ghostBad, still: r.open,
                    wrote: r.wire, box: f.box.w + "×" + f.box.h };
    await shot(tag + "-refusal");
  }

  // 5. THE BOTTOM ROW, where the box must turn over.  In the pane the LAST
  //    planning line stands for it — walked to, one `n' at a time, rather than
  //    opened by hand, so the case is reached the way a reader would reach it.
  await open(file);
  if (surface === "table") {
    for (let i = 0; i < 17; i += 1) await page.keys(["n"]);
  } else {
    const want = (await rig()).plans.slice(-1)[0];
    for (let i = 0; i < 40 && (await rig()).point !== want; i += 1)
      await page.keys(["n"]);
  }
  await page.settle();
  await RET();
  await page.keys([KEY.Backspace]);
  await page.keys(type("18 au"));
  await page.settle();
  {
    const f = await foot(), x = await clip(), cv = await covers();
    const u = await page.eval(() => window.RIG_TEST.unflipped());
    row.flip = { flipped: f.flipped, all: f.all.w + "×" + f.all.h,
                 off: x, title: cv.title, wouldRunOff: u.over,
                 room: u.room, hangs: u.hangs };
    await shot(tag + "-flip");
  }

  // THE WIDEST THINGS THE WIDGET CAN DRAW, measured rather than shot: a range's
  // ghost, and a month word long enough to test the offers' own floor.
  await open(file);
  await RET();
  await page.keys([KEY.Backspace]);
  await page.keys(type("from 18 to 19 aug"));
  await page.settle();
  {
    const f = await foot(), x = await clip(), cv = await covers();
    const g = await page.eval(() => window.RIG_TEST.ghostCut());
    row.widest = { said: g.text, drew: g.drew, want: g.want, cut: g.cut,
                   box: f.box.w, all: f.all.w + "×" + f.all.h,
                   cells: cv.cells, off: x, title: cv.title };
  }
  await open(file);
  await RET();
  await page.keys([KEY.Backspace]);
  await page.keys(type("18 s"));
  await page.settle();
  {
    const r = await rig(), f = await foot(), x = await clip();
    row.sept = { list: r.offers, menu: f.offers ? f.offers.w + "×" + f.offers.h : "none",
                 all: f.all.w + "×" + f.all.h, off: x };
  }

  // 900px, where the table narrows and the box does not.
  await open(file, NARROW);
  await RET();
  await page.keys([KEY.Backspace]);
  await page.keys(type("18 au"));
  await page.settle();
  {
    const f = await foot(), x = await clip(), cv = await covers();
    row.narrow = { all: f.all.w + "×" + f.all.h, left: f.all.left,
                   off: x, share: cv.share, title: cv.title, cells: cv.cells };
    await shot(tag + "-900");
  }
  TABLE.push(row);
  console.log("  " + tag + " · " + what.padEnd(22)
    + "at rest " + pad(row.rest.all, 9)
    + " · with offers " + pad(row.offers.all, 9)
    + " · " + pad(row.offers.cells, 4) + " cells, " + pad(row.offers.rows, 4) + " rows"
    + " · covers " + pad(row.offers.share, 5) + "%"
    + (row.offers.title ? " · TITLE" : "        ")
    + (row.flip.flipped ? " · flips" : " · NO FLIP"));
}

// ------------------------------------------------------ the widest things
console.log("\nthe widest things the widget can draw\n");
const W = [5, 12, 10, 6, 7, 16, 10, 12];
console.log("  " + ["tab", "range ghost", "box", "cells", "title",
                    "`18 september'", "the menu", "its floor"]
  .map((h, i) => h.padEnd(W[i])).join(" "));
for (const r of TABLE) {
  console.log("  " + [
    r.tag,
    r.widest.cut ? "CUT by " + r.widest.cut : r.widest.drew + "px whole",
    r.widest.all, String(r.widest.cells),
    r.widest.title ? r.widest.title + "px" : "clear",
    r.sept.list.join(", "), r.sept.menu,
    r.sept.menu === "none" ? "—" : "12em = 132px",
  ].map((v, i) => String(v).padEnd(W[i])).join(" "));
}

// ------------------------------------------------------------ the table
console.log("\nthe measurements\n");
const CELLS = [
  ["tab", 5], ["at rest", 10], ["+offers", 10], ["cells", 6], ["rows", 6],
  ["covers", 8], ["title", 6], ["flip", 6], ["the bottom row", 38], ["900px", 22],
];
console.log("  " + CELLS.map(([h, w]) => h.padEnd(w)).join(" "));
for (const r of TABLE) {
  const off = (x) => [x.left && "L" + x.left, x.right && "R" + x.right,
                      x.bottom && "B" + x.bottom].filter(Boolean).join(" ") || "—";
  console.log("  " + [
    r.tag, r.rest.all, r.offers.all, String(r.offers.cells), String(r.offers.rows),
    r.offers.share + "%", r.offers.title ? r.offers.title + "px" : "clear",
    r.flip.flipped ? "yes" : "no",
    r.flip.hangs + "px under a row with " + r.flip.room + "px"
      + (r.flip.wouldRunOff ? " → B" + r.flip.wouldRunOff : " → fits"),
    r.narrow.all + " @" + r.narrow.left + " " + off(r.narrow.off),
  ].map((v, i) => String(v).padEnd(CELLS[i][1])).join(" "));
}

// ------------------------------------------------------------------ the check
console.log("\nthe check — every key, every tab\n");
for (const [tag, file, what] of VARIANTS) {
  console.log("  " + tag + " · " + what);

  // ONE CLOCK READ, and the page took the pinned one.
  await open(file);
  is("    the day is pinned", await page.eval(() => window.RIG_TEST.day()),
     { y: 2026, m: 9, d: 12 });

  // `RET' OPENS ON THE VALUE IT FINDS, WHOLLY SELECTED.
  const tops = await geom();
  await RET();
  {
    const c = await caret(), r = await rig();
    yes("    RET opens the box", r.open);
    yes("    the value comes up wholly selected",
        c.start === 0 && c.end === c.value.length && c.value.length > 0);
    // THE ENTRY-GHOST, and it differs by surface for a reason: the pane opens on
    // org's own stamp, which the ghost has nothing to add to; a cell opens on
    // `isoStamp''s ten characters, which resolve to a stamp and so DO speak.
    if (tag === "0") is("    the pane's entry ghost is silent", r.ghost, "");
    else yes("    the cell's entry ghost speaks", r.ghost.indexOf(" → <") === 0);
    is("    nothing moved to make room", await geom(), tops);
  }

  // TYPING: the ghost is silent over a term still being WRITTEN, and the offers
  // stand at the unfinished position.
  await page.keys([KEY.Backspace]);
  await page.keys(type("18 au"));
  await page.settle();
  {
    const r = await rig();
    is("    `18 au' draws no ghost", r.ghost, "");
    is("    `18 au' offers the month and the reader's own line",
       r.offers, ["18 au", "18 august"]);
    yes("    the offers are painted", r.drawn.length === 2);
  }

  // THE OFFERS WALK on the vertical pair, and `TAB' takes the one that stands.
  await page.keys([KEY.ArrowDown]);
  await page.settle();
  is("    <down> walks to the second offer", (await rig()).at, 1);
  await page.keys([KEY.Tab]);
  await page.settle();
  {
    const r = await rig(), c = await caret();
    is("    TAB takes it into the field", c.value, "18 august");
    is("    and the ghost then speaks", r.ghost, " → " + STAMP);
    is("    a finished term carries no offers", r.offers, []);
    is("    the caret sits at the end", [c.start, c.end],
       ["18 august".length, "18 august".length]);
  }

  // THE GHOST IS A SPAN: the caret cannot enter it.
  for (let i = 0; i < 8; i += 1) await page.keys([KEY.ArrowRight]);
  await page.settle();
  is("    the caret cannot walk into the ghost", (await caret()).start, 9);

  // THE SHIFTED ARROWS ADJUST THE VALUE: a day, then a week.
  await page.keys(SHIFT(KEY.ArrowRight));
  await page.settle();
  is("    S-<right> steps a day", (await caret()).value, "2026-08-19");
  await page.keys(SHIFT(KEY.ArrowDown));
  await page.settle();
  is("    S-<down> steps a week", (await caret()).value, "2026-08-26");
  await page.keys(SHIFT(KEY.ArrowUp));
  await page.keys(SHIFT(KEY.ArrowLeft));
  await page.settle();
  is("    S-<up> and S-<left> come back", (await caret()).value, "2026-08-18");

  // `RET' COMMITS, AND THE PHRASE TRAVELS.
  await page.keys([KEY.Enter]);
  await page.settle();
  {
    const r = await rig();
    yes("    RET shuts the box", !r.open);
    yes("    the wire carries the PHRASE, not a resolution",
        r.wire.indexOf('"2026-08-18"') !== -1);
    if (tag === "0") is("    the pane's line shows the stamp", r.plan[0][1], STAMP);
    else is("    the cell shows the ISO day", r.rows[0].scheduled, "2026-08-18");
  }

  // A PHRASE NO READING TAKES REFUSES IN PLACE.
  await open(file);
  await RET();
  await page.keys([KEY.Backspace]);
  await page.keys(type("18 auk"));
  await page.keys([KEY.Enter]);
  await page.settle();
  {
    const r = await rig();
    is("    `18 auk' shows the refusal", r.ghost, " ✗ not a date");
    yes("    in the refusal's own red", r.ghostBad);
    yes("    RET leaves the box standing", r.open);
    is("    and writes nothing", r.wire, "");
  }

  // `ESC' CANCELS THE INPUT WHOLE, byte for byte.
  await page.keys([KEY.Escape]);
  await page.settle();
  {
    const r = await rig();
    yes("    ESC shuts it", !r.open);
    is("    and writes nothing", r.wire, "");
    if (tag === "0") is("    the pane's own spelling is back", r.plan[0][1], "<2026-09-15 Tue>");
    else is("    the cell's own spelling is back", r.rows[0].scheduled, "2026-09-15");
  }

  // EMPTY + `RET' CLEARS THE ENTRY, the shipped foot's own promise.
  await open(file);
  await RET();
  await page.keys([KEY.Backspace]);
  await page.keys([KEY.Enter]);
  await page.settle();
  {
    const r = await rig();
    yes("    empty RET clears the entry", r.wire.indexOf('"date": ""') !== -1);
    if (tag === "0") yes("    the pane's entry comes off", r.plan.length === 1);
    else is("    the cell empties", r.rows[0].scheduled, "");
  }

  // ORG'S OWN SPELLING IS KEPT VERBATIM, though `2026-08-05' is a Wed.
  await open(file);
  await RET();
  await page.keys([KEY.Backspace]);
  await page.keys(type("<2026-08-05 Mon>"));
  await page.settle();
  is("    org's own stamp adds no ghost", (await rig()).ghost, "");

  // THE BOX TAKES ITS OWN KEYS, and ONE KEYSTROKE REPLACES THE WHOLE OPENING
  // VALUE: re-opened, the value is selected, so `n' types an `n' over it rather
  // than walking a row the way `n' does everywhere else on this surface.
  await page.keys([KEY.Escape]);
  await page.settle();
  await RET();
  await page.keys(type("n"));
  await page.settle();
  is("    `n' inside the box types an `n', over the whole selection",
     (await caret()).value, "n");

  // NOTHING MOVED, start to finish.  An overlay that moved a row would be a
  // strip wearing an overlay's coat.
  await page.keys([KEY.Escape]);
  await page.settle();
  is("    every row is where it began", await geom(), tops);
}

console.log(bad ? `\n${bad} FAILED\n` : "\nall rungs green\n");
await page.close();
process.exit(bad ? 1 : 0);
