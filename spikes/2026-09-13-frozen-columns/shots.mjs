// The screenshots, headless — five moments per tab, plus every number this
// README quotes, plus a rung per claim per variant.  `node shots.mjs'.
//
// THE NUMBERS ARE MEASURED, NOT CHOSEN: every column's width at rest, during
// the edit and after it; how many rows' cell boundaries moved; how much of what
// the reader typed the open box actually shows; and the ONE JUMP a thaw costs,
// in px per column.  A trade-off with no number on it is an opinion.
//
// THE LAST PASS IS A CHECK: every claim the README makes about a tab, driven
// from the keyboard and asserted.  The claims are about WIDTHS — which is the
// point, since a variant that moved one while an editor stood would have
// disqualified itself.
//
// NO CLOCK IS READ.  This rig draws no date box and computes no day, so a
// screenshot taken tomorrow is the same screenshot without a `?day=' to pin.
import { chromium, KEY } from "./cdp.mjs";
import { fileURLToPath, pathToFileURL } from "node:url";
import { dirname, join } from "node:path";

const HERE = dirname(fileURLToPath(import.meta.url));
const WIDE = [1366, 700];
const NARROW = [900, 700];
const url = (f) => pathToFileURL(join(HERE, f)).href;

const VARIANTS = [
  ["0", "0-as-shipped.html", "as shipped — the control"],
  ["a", "a-clipped-cell.html", "frozen, clipped cell"],
  ["b", "b-overlay-cell.html", "frozen, the cell overflows"],
  ["c", "c-row-alone.html", "frozen, the row reflows alone"],
  ["d", "d-frozen-always.html", "frozen always"],
];

// SIXTY CHARACTERS EXACTLY, counted rather than eyeballed, and a tag run long
// enough to drive the tag column into its `COL_MAX_CH = 40' ceiling.
const TITLE = "A sixty character title typed into the draft row and its box";
const TAGS = "trip:admin:visa:consulate:photography:paperwork";

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

const geom = () => page.eval(() => window.RIG_TEST.geom());
const seen = () => page.eval(() => window.RIG_TEST.seen());
const state = () => page.eval(() => window.RIG_TEST.state());
const wouldBe = () => page.eval(() => window.RIG_TEST.wouldBe());

async function open(file, size) {
  await page.resize(...(size || WIDE));
  await page.goto(url(file));
  await page.settle();
}
const shot = (name, clip) => page.shot(join(HERE, name + ".png"), clip);
const type = (s) => [...s];
const SHIFT = (...ks) => [{ down: "Shift" }, ...ks, { up: "Shift" }];
const keys = async (...ks) => { await page.keys(ks.flat()); await page.settle(); };

const pad = (n, w) => String(n).padStart(w);
const w2 = (n) => pad(Math.round(n), 5);
/** One column line: the six widths as the reader has them. */
const line = (g) => g.cols.map((c) => w2(c.width)).join(" ");
const HEAD = "  " + ["state", "prio", "title", "sched", "dline", "tags"]
  .map((k) => pad(k, 5)).join(" ");

/** HOW MANY ROWS MOVED between two readings: a row counts as moved when any of
 *  its cells changed left edge or width by more than half a pixel.  Which is
 *  the whole of what "the table reflowed" means to an eye. */
function moved(a, b) {
  let n = 0;
  for (const ra of a.rows) {
    const rb = b.rows.find((r) => r.id === ra.id);
    if (!rb) continue;
    if (ra.cells.some((c, i) => rb.cells[i]
                     && (Math.abs(c.left - rb.cells[i].left) > 0.5
                         || Math.abs(c.width - rb.cells[i].width) > 0.5))) n += 1;
  }
  return n;
}
/** Did the columns end up EQUAL — the shape a fixed-layout table takes when its
 *  colgroup carries no widths at all.  Tab 0's own signature. */
const equal = (g) => g.cols.every((c) => Math.abs(c.width - g.cols[0].width) < 1);
/** Per-column px moved, `a' to `b'. */
const delta = (a, b) => b.cols.map((c, i) => Math.round(c.width - a.cols[i].width));

// =========================================================== the columns at rest
console.log("\nthe columns the transcription produces, at 1366px\n");
await open("0-as-shipped.html");
{
  const g = await geom(), w = await wouldBe();
  console.log(HEAD + "   px, drawn");
  console.log("  " + line(g));
  console.log("  " + w.map((x) => pad(x.key === "title" ? "fill" : x.ch + "ch", 5)).join(" ")
    + "   what `colWidths' measured");
  console.log("  " + w.map((x) => pad(x.ground + "px", 5)).join(" ") + "   the grounds");
  const ch = (g.cols[3].width - 24) / 13;
  console.log("\n  one `ch' is " + Math.round(ch * 100) / 100 + "px at this face, so a sorted");
  console.log("  date column is `calc(13ch + 24px)' = " + Math.round(g.cols[3].width) + "px — the");
  console.log("  very number spikes/2026-09-12-date-cell and -date-overlay argued about.");
  console.log("\n  THE TITLE IS THE FILL COLUMN and carries no width of its own: it is");
  console.log("  " + Math.round(g.cols[2].width) + "px here because the other five left that much.  Which is the");
  console.log("  whole mechanism — every character a SIZED column gains, the title pays.");
}

// ============================================================== 0 — the control
console.log("\n\n0 — WHAT MOVES TODAY\n");
await open("0-as-shipped.html");
const REST = await geom();
await shot("0-rest");
{
  console.log("  a draft opens (`+'), and its title cell with it");
  await keys("+");
  const g1 = await geom();
  console.log("  " + line(g1) + "   " + moved(REST, g1) + " rows moved");

  console.log("\n  sixty characters typed into it");
  await keys(type(TITLE));
  const g2 = await geom();
  await shot("0-title");
  const s2 = await seen();
  console.log("  " + line(g2) + "   " + moved(g1, g2) + " rows moved");
  console.log("  the box shows " + s2.visible + " of " + s2.chars + " characters"
    + (s2.cut ? ", " + Math.round(s2.cut) + "px cut" : ", nothing cut"));
  console.log("  NOTHING MOVED.  Under `table-layout:fixed' a cell's content does not");
  console.log("  size its column, so the input is clipped by the width already there.");

  console.log("\n  `TAB' walks to the next stop — and `editCell' closes before it opens");
  await keys(KEY.Tab);
  const g3 = await geom();
  await shot("0-walk");
  console.log("  " + line(g3) + "   " + moved(g2, g3) + " rows moved");
  console.log("  " + (equal(g3) ? "SIX EQUAL COLUMNS." : "columns held.")
    + "  `closeCellEditor' (`table-view.js:4017') runs");
  console.log("  `renderRows(); renderHead();' in that order: the widths are written");
  console.log("  onto cols the next line throws away, and a fixed-layout table with a");
  console.log("  bare colgroup divides the window equally.  EVERY walk step does this.");

  console.log("\n  on to the tag cell, and a six-value run typed into it");
  await keys(KEY.Tab, KEY.Tab);
  await keys(type(TAGS));
  const g4 = await geom();
  await shot("0-tags");
  const s4 = await seen();
  console.log("  " + line(g4) + "   " + moved(g3, g4) + " rows moved");
  console.log("  the box shows " + s4.visible + " of " + s4.chars + " characters");

  console.log("\n  `RET' captures, the row lands, and the measure runs over the new set");
  await keys(KEY.Enter);
  const g5 = await geom();
  await shot("0-after");
  console.log("  " + line(g5) + "   " + moved(REST, g5) + " rows moved against rest");
  console.log("  " + delta(REST, g5).map((n) => pad((n > 0 ? "+" : "") + n, 5)).join(" ")
    + "   px moved, rest → after");
  console.log("\n  THE TAG COLUMN TOOK ITS CEILING (`COL_MAX_CH = 40') AND THE TITLE PAID.");
  console.log("  Every one of the " + REST.rows.length + " standing rows re-truncated to a title "
    + Math.abs(delta(REST, g5)[2]) + "px narrower");
  console.log("  than the one the reader had been reading a keystroke earlier.");
}

// ============================================================ the four candidates
for (const [key, file, label] of VARIANTS.slice(1)) {
  console.log("\n\n" + key.toUpperCase() + " — " + label + "\n");
  await open(file);
  const rest = await geom();
  await shot(key + "-rest");
  is("  rests at the same six widths as the control",
     rest.cols.map((c) => Math.round(c.width)),
     REST.cols.map((c) => Math.round(c.width)));

  await keys("+");
  const opened = await geom();
  is("  a draft opens and no column moves", moved(rest, opened), 0);
  yes("  the pin stands", (await state()).pinned);

  await keys(type(TITLE));
  const typed = await geom();
  await shot(key + "-title");
  const st = await seen();
  is("  sixty characters typed and no column moves", moved(opened, typed), 0);
  is("  and the sixty arrived whole", st.chars, 60);
  console.log("        the title box shows " + st.visible + "/" + st.chars
    + " characters" + (st.cut ? ", " + Math.round(st.cut) + "px cut" : ", nothing cut")
    + (typed.laid ? ", laid box " + Math.round(typed.laid.width) + "px" : ""));

  await keys(KEY.Tab);
  const walked = await geom();
  await shot(key + "-walk");
  is("  `TAB' walks a stop and no column moves", moved(typed, walked), 0);
  yes("  the colgroup kept its widths across the close", !equal(walked));

  await keys(KEY.Tab, KEY.Tab);
  await keys(type(TAGS));
  const tagged = await geom();
  await shot(key + "-tags");
  const sg = await seen();
  is("  a six-value tag run typed and no column moves", moved(walked, tagged), 0);
  console.log("        the tag box shows " + sg.visible + "/" + sg.chars
    + " characters" + (sg.cut ? ", " + Math.round(sg.cut) + "px cut" : ", nothing cut"));
  if (tagged.laid) {
    console.log("        the laid box is " + Math.round(tagged.laid.width) + "px and wants "
      + tagged.laid.wants + "px; it reaches " + Math.round(tagged.laid.spill)
      + "px past its cell's right edge");
    if (tagged.laid.drift.length)
      console.log("        the detached line's cells sit "
        + tagged.laid.drift.map(Math.round).join(", ") + "px off the header's,"
        + " scrolled " + Math.round(tagged.laid.scrolled) + "px");
  }

  await keys(KEY.Enter);
  const after = await geom();
  await shot(key + "-after");
  console.log("        the one jump on close: "
    + (after.jump ? after.jump.map((n) => (n > 0 ? "+" : "") + Math.round(n)).join(" ")
                  : "none — nothing was ever unpinned"));
  console.log("        " + moved(tagged, after) + " rows moved at that one moment, "
    + moved(rest, after) + " against rest");

  if (key === "d") {
    console.log("\n  D's own question: a row that ARRIVES with a run no column was sized for");
    await keys("a");
    const arrived = await geom();
    await shot("d-arrived");
    is("  the arriving row moves no column", moved(after, arrived), 0);
    const row = arrived.rows.find((r) => r.text[5].includes("…"));
    yes("  and its tag run is cut and stays cut", !!row);
    if (row) console.log("        it reads  " + JSON.stringify(row.text[5]));
  }
}

// ============================================ D, the recommendation, measured
console.log("\n\nD — THE RECOMMENDATION, MEASURED\n");
/** How many drawn cells are cut by the box they were given. */
const clipped = (g) => g.rows.reduce((n, r) =>
  n + r.cells.filter((c) => c.clipped).length, 0);
{
  await open("d-frozen-always.html");
  const rest = await geom();
  console.log("  at rest  " + line(rest) + "   " + clipped(rest) + " cells clipped");

  await keys("+");
  await keys(type(TITLE));
  const t = await geom();
  is("  a 60-character title typed into a draft moves nothing",
     [delta(rest, t).reduce((a, b) => a + Math.abs(b), 0), moved(rest, t)], [0, 0]);
  await keys(KEY.Escape);

  await keys("a");
  const arrived = await geom();
  is("  a row arriving with a longer run moves nothing",
     [delta(rest, arrived).reduce((a, b) => a + Math.abs(b), 0), moved(rest, arrived)], [0, 0]);
  console.log("        it is drawn clipped instead: " + clipped(arrived)
    + " cells cut against " + clipped(rest) + " before it landed");

  console.log("\n  what the arriving row is OWED, and when it is paid:");
  await keys("q");
  await keys("q");
  const paid = await geom();
  console.log("  " + line(paid));
  console.log("  " + delta(rest, paid).map((n) => pad((n > 0 ? "+" : "") + n, 5)).join(" ")
    + "   px, paid at the next view change and not before");

  console.log("\n  `q' drops `sort:' — a view change, on a table nothing landed in");
  await open("d-frozen-always.html");
  const clean = await geom();
  await keys("q");
  const unsorted = await geom();
  await shot("d-view");
  console.log("  " + line(unsorted));
  console.log("  " + unsorted.jump.map((n) => pad((n > 0 ? "+" : "") + Math.round(n), 5)).join(" ")
    + "   px, the one re-fit");
  is("  both date columns lose their sort mark, three characters each",
     [Math.round(unsorted.jump[3]), Math.round(unsorted.jump[4])], [-22, -22]);
  is("  and the fill column takes the six back",
     Math.round(unsorted.jump[2]), 43);
  await keys("q");
  is("  `q' again puts the view back exactly", delta(clean, await geom()),
     [0, 0, 0, 0, 0, 0]);

  console.log("\n  the window narrows by 266px — the ONE other re-fit");
  await page.resize(1100, 700);
  await page.settle();
  const small = await geom();
  await shot("d-resize");
  console.log("  " + line(small) + "   " + clipped(small) + " cells clipped");
  console.log("  " + delta(clean, small).map((n) => pad((n > 0 ? "+" : "") + n, 5)).join(" ")
    + "   px, and it is the fill column that pays the whole of it");
  is("  the five sized columns hold their px across a resize",
     delta(clean, small).filter((_, i) => i !== 2), [0, 0, 0, 0, 0]);
}

// ================================================== the standing row's own editor
console.log("\n\nTHE STANDING ROW'S IN-CELL EDITOR — `RET' on a cell that is not a draft\n");
for (const [key, file] of VARIANTS) {
  await open(file);
  const rest = await geom();
  // point is on r1 and the cell cursor on `title'; `f' walks it to SCHEDULED.
  await keys("f");
  await keys(KEY.Enter);
  const open1 = await geom();
  await shot(key + "-cell");
  const s = await seen();
  console.log("  " + key + "  open on " + s.key + ": " + moved(rest, open1)
    + " rows moved, " + (equal(open1) ? "SIX EQUAL COLUMNS" : "columns held")
    + ", box shows " + s.visible + "/" + s.chars);
  // A 118px CELL AND A VALUE THAT DOES NOT FIT IT — the case a date column is
  // always in, and the one where B's box has neighbours to cover.  THE ARROW
  // FIRST: `openCellEditor' selects the whole value, and a spike that typed
  // over it would be measuring a shorter string than it meant to.
  await keys(KEY.ArrowRight);
  await keys(type(" or thereabouts, ask Ana first"));
  const long1 = await geom();
  await shot(key + "-spill");
  const sl = await seen();
  console.log("     30 more characters: " + moved(open1, long1) + " rows moved, box shows "
    + sl.visible + "/" + sl.chars
    + (long1.laid ? ", laid box " + Math.round(long1.laid.width) + "px reaching "
                    + Math.round(long1.laid.spill) + "px past the cell" : ""));
  await keys(KEY.Escape);
  const shut = await geom();
  await shot(key + "-shut");
  console.log("     `ESC' out: " + moved(open1, shut) + " rows moved, "
    + (equal(shut) ? "SIX EQUAL COLUMNS" : "columns held")
    + (key === "0"
       ? "   ← nothing repaints after `closeCellEditor', so this is what stands"
       : ""));
}

// ============================================================ the narrow window
console.log("\n\nAT 900px, WHERE THE TITLE HAS NO SLACK TO GIVE\n");
for (const [key, file] of VARIANTS) {
  await open(file, NARROW);
  const rest = await geom();
  await keys("+");
  await keys(type(TITLE));
  const typed = await geom();
  await shot(key + "-900");
  const s = await seen();
  console.log("  " + key + "  title column " + Math.round(rest.cols[2].width)
    + "px, the box shows " + s.visible + "/" + s.chars + " characters"
    + (typed.laid ? ", laid box " + Math.round(typed.laid.width) + "px over "
                    + Math.round(typed.laid.spill) + "px of its neighbours" : "")
    + ", " + moved(rest, typed) + " rows moved");
  // AND WHAT THE TAG RUN COSTS HERE: the table's `min-width' is the sized
  // columns plus the title's 40ch floor, so a run at its ceiling pushes the
  // floor past the window and the scroller starts scrolling sideways.
  await keys(KEY.Tab, KEY.Tab, KEY.Tab);
  await keys(type(TAGS));
  await keys(KEY.Enter);
  const landed = await geom();
  const port = await page.eval(() =>
    Math.round(document.getElementById("tablewrap").clientWidth));
  console.log("     after the run lands: table " + Math.round(landed.width)
    + "px in a " + port + "px port"
    + (landed.width > port + 0.5 ? "   ← SIDEWAYS SCROLL" : "   (no sideways scroll)")
    + ", title " + Math.round(landed.cols[2].width) + "px");
}

// ========================================================== the cell highlight
// THE DRESS THE RIG DRAWS ON THE SELECTED CELL, cropped and magnified so it can
// be argued about, in both themes.  The rig's cursor is a ROW (`tv-sel', the
// widget's own gold) with a CELL RING inside it (`cd-at', a 1px inset shadow in
// `--g-point'), which is the pair the README's "The cell highlight" is about.
console.log("\n\nTHE CELL HIGHLIGHT — the rig's dress, cropped\n");
for (const [name, mode] of [["light", "light"], ["dark", "dark"]]) {
  await page.theme(mode);
  await open("0-as-shipped.html");
  await keys("g");                     // the ruler off; it would cross the crop
  await keys("g");
  const g = await geom();
  const base = await page.eval(() => {
    const t = document.querySelector(".tv-table").getBoundingClientRect();
    return { x: t.left, y: t.top };
  });
  const row = g.rows[0];
  const cell = row.cells[2];           // the title cell, where point stands
  await shot("highlight-" + name, {
    x: Math.round(base.x + cell.left - 80), y: Math.round(base.y + row.top - 34),
    width: Math.round(cell.width + 200), height: 96, scale: 3,
  });
  const ink = await page.eval(() => {
    const td = document.querySelector("tr.tv-sel td.cd-at");
    const tr = td.closest("tr");
    const cs = getComputedStyle(td);
    return { ring: cs.boxShadow, cellGround: cs.backgroundColor,
             rowGround: getComputedStyle(tr).backgroundColor,
             point: getComputedStyle(document.documentElement)
                      .getPropertyValue("--g-point").trim() };
  });
  console.log("  " + pad(name, 5) + "  row ground " + ink.rowGround
    + "   cell ground " + ink.cellGround);
  console.log("         ring " + ink.ring + "   (--g-point " + ink.point + ")");
  // THE INVARIANT, CHECKED RATHER THAN ASSERTED: a cell ring inside the cursor
  // row must write NO ground of its own, or two golds stack (docs/invariants.md
  // "One gold at a time; the coarser ground lifts").
  yes("  " + name + ": the cell writes no ground of its own",
      /rgba\(0, 0, 0, 0\)|transparent/.test(ink.cellGround));
  yes("  " + name + ": and the row keeps the one gold",
      !/rgba\(0, 0, 0, 0\)|transparent/.test(ink.rowGround));
}
await page.theme("dark");

// ================================================================== the rungs
console.log("\n\nTHE CHECK — every claim a tab makes, driven from the keyboard\n");

await open("0-as-shipped.html");
{
  await keys("+");
  await keys(type("x"));
  const before = await geom();
  await keys(KEY.Tab);
  const after = await geom();
  yes("0  a walk step leaves the colgroup bare (six equal columns)", equal(after));
  yes("0  and the widths it had are gone", !equal(before));
  await keys(KEY.Escape);
  const dropped = await geom();
  is("0  `ESC' drops the draft and the table re-fits", dropped.rows.length, 18);
}

for (const [key, file] of VARIANTS.slice(1)) {
  await open(file);
  const rest = await geom();
  await keys("+");
  await keys(type(TITLE));
  await keys(KEY.Tab, KEY.Tab, KEY.Tab);
  await keys(type(TAGS));
  const mid = await geom();
  is(key + "  no column moves anywhere in the walk", moved(rest, mid), 0);
  yes(key + "  the columns never go equal", !equal(mid));
  const s = await seen();
  if (key === "a" || key === "d")
    yes(key + "  the reader sees a window onto the run (" + s.visible + "/" + s.chars + ")",
        s.cut > 0);
  else
    is(key + "  the reader sees the whole run", [s.visible, s.cut], [s.chars, 0]);
  if (key === "b")
    yes("b  the box reaches past its own cell", mid.laid.spill > 0);
  if (key === "c")
    yes("c  the detached line has left the header's x-positions",
        mid.laid.drift.some((d) => Math.abs(d) > 1));

  await keys(KEY.Escape);
  const dropped = await geom();
  if (key === "d") {
    is("d  a drop moves nothing, there being no pin to lift", moved(rest, dropped), 0);
    is("d  and no jump was ever reported", dropped.jump, null);
  } else {
    is(key + "  a drop lifts the pin and re-fits to the standing set",
       moved(rest, dropped), 0);
    yes(key + "  and the jump is reported in px per column",
        Array.isArray(dropped.jump));
  }
}

console.log(bad ? "\n" + bad + " FAILED\n" : "\nall rungs green\n");
await page.close();
process.exit(bad ? 1 : 0);
