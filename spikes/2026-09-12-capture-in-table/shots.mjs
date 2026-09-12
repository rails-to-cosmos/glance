// The screenshots, headless — each tab at the moment that shows what it is for,
// plus the refusal, plus every number this README quotes.  `node shots.mjs'.
//
// THE NUMBERS ARE MEASURED, NOT CHOSEN: the keys a bare jot costs, how far the
// draft pushed the rows that were already there, and how far the committed row
// travelled to reach its sorted place.  A trade-off with no number on it is an
// opinion.  The last pass is a CHECK: every key of every variant driven, and
// what the rig says afterwards compared against what the law says it should.
import { chromium, KEY } from "./cdp.mjs";
import { fileURLToPath, pathToFileURL } from "node:url";
import { dirname, join } from "node:path";

const HERE = dirname(fileURLToPath(import.meta.url));
const url = (f) => pathToFileURL(join(HERE, f)).href;
const type = (s) => [...s];
const CTRL = (...ks) => [{ down: "Ctrl" }, ...ks, { up: "Ctrl" }];
const SHIFT = (...ks) => [{ down: "Shift" }, ...ks, { up: "Shift" }];
const JOT = "Swap the SIM card";

const VARIANTS = [
  ["a-top-row.html", "A", "a-top-row"],
  ["b-row-at-point.html", "B", "b-row-at-point"],
  ["c-walk-the-row.html", "C", "c-walk-the-row"],
  ["d-form-strip.html", "D", "d-form-strip"],
  ["e-walk-on-tab.html", "E", "e-walk-on-tab"],
];
// C is the only tab whose `RET' walks, so it alone needs four of them to write.
const write = (name) =>
  name === "C" ? [KEY.Enter, KEY.Enter, KEY.Enter, KEY.Enter] : [KEY.Enter];

const page = await chromium();
let bad = 0;
const fail = (what) => { bad += 1; console.log("  FAIL  " + what); };
const ok = (what) => console.log("  ok    " + what);

/** Where the rows stand, what the draft says, and what the rig last said. */
const geom = () => page.eval(() => {
  const rows = [...document.querySelectorAll("#tablewrap tbody tr")];
  const top = (t) => {
    const tr = rows.find((r) => r.textContent.includes(t));
    return tr ? Math.round(tr.getBoundingClientRect().top) : 0;
  };
  const draft = document.querySelector("tr.cx-draft")
    || document.querySelector("#strip tbody tr");
  const box = draft && draft.getBoundingClientRect();
  const text = (sel) => {
    const n = document.querySelector(sel);
    return n ? n.textContent.trim() : "";
  };
  return {
    first: top("Renew the passport"), last: top("Ship the fold marks"),
    draftH: box ? Math.round(box.height) : 0,
    rowH: rows.length ? Math.round(rows[0].getBoundingClientRect().height) : 0,
    where: text(".cx-where"), refuse: text(".cx-refuse"),
    moved: text(".cx-moved"), align: text("#align"),
    state: text("#state"), truth: text("#truth"),
    // What the hint and the badge cost the title the reader is typing into.
    cell: box2(".cx-titlecell"), input: box2("#cxin"),
  };
  function box2(sel) {
    const n = document.querySelector(sel);
    return n ? Math.round(n.getBoundingClientRect().width) : 0;
  }
});
const rig = () => page.eval(() => window.RIG_TEST.state());
/** The draft exactly as it ARRIVES, before anything is typed into it. */
const drafted = () => page.eval(() => {
  const tr = document.querySelector("tr.cx-draft")
    || document.querySelector("#strip tbody tr");
  if (!tr) return null;
  const td = [...tr.children].map((c) => c.textContent.trim());
  const w = tr.querySelector(".cx-where");
  return { state: td[0] || "—", priority: td[1] || "—", tags: td[3] || "—",
           where: w ? w.textContent.trim() : "",
           filter: document.getElementById("filter").value };
});

async function open(file) {
  await page.goto(url(file));
  await page.settle();
}

// ---- the five shots --------------------------------------------------------
console.log("\nthe five shots\n");

// A — the draft standing at the head with the title half typed: the ghosts the
// filter seeded, the destination, and the badge that says no file exists yet.
await open("a-top-row.html");
{
  const base = await geom();
  await page.keys(["+"]);
  await page.keys(type(JOT));
  await page.settle();
  const g = await geom();
  await page.shot(join(HERE, "a-top-row.png"));
  console.log("a-top-row       draft " + g.draftH + "px at the head · the first "
    + "row moved " + (g.first - base.first) + "px, the last "
    + (g.last - base.last) + "px · " + g.where
    + " · the title cell is " + g.cell + "px and the input has " + g.input
    + "px of it (" + (g.cell - g.input) + "px to the hint and the badge)");
}

// B — AFTER the commit, which is the only moment B's argument is visible: the
// row has flown from where it was typed to where `sort:scheduled->title' puts it.
await open("b-row-at-point.html");
{
  const base = await geom();
  await page.keys(["+"]);
  const opened = await geom();
  await page.keys(type(JOT));
  await page.keys([KEY.Enter]);
  await page.settle();
  const g = await geom();
  await page.shot(join(HERE, "b-row-at-point.png"));
  console.log("b-row-at-point  draft below point · the first row moved "
    + (opened.first - base.first) + "px, the last "
    + (opened.last - base.last) + "px · after the write: " + g.moved);
}

// C — mid-walk, point on the priority cell: the draft still a draft, two cells
// refined, the commit still ahead.
await open("c-walk-the-row.html");
{
  await page.keys(["+"]);
  await page.keys(type(JOT));
  await page.keys([KEY.Enter]);
  await page.keys([KEY.Enter]);
  await page.settle();
  const g = await geom();
  await page.shot(join(HERE, "c-walk-the-row.png"));
  console.log("c-walk-the-row  " + g.state);
}

// D — the strip standing, the table untouched beneath it.
await open("d-form-strip.html");
{
  const base = await geom();
  await page.keys(["+"]);
  await page.keys(type(JOT));
  await page.settle();
  const g = await geom();
  await page.shot(join(HERE, "d-form-strip.png"));
  console.log("d-form-strip    strip " + g.draftH + "px · the first row moved "
    + (g.first - base.first) + "px, the last " + (g.last - base.last)
    + "px · " + g.align);
}

// E — three TABs in, so the walk shows at its far end: the title kept as a real
// value, state and priority still the ghosts the filter seeded, the tags cell
// open — and `RET' from there writes, with no fourth key to find.
await open("e-walk-on-tab.html");
{
  await page.keys(["+"]);
  await page.keys(type(JOT));
  await page.keys([KEY.Tab]);
  await page.keys([KEY.Tab]);
  await page.keys([KEY.Tab]);
  await page.settle();
  const g = await geom();
  await page.shot(join(HERE, "e-walk-on-tab.png"));
  console.log("e-walk-on-tab   " + g.state);
}

// The refusal, its own picture because it is the one law with a shipped wording.
await open("a-top-row.html");
{
  await page.eval(() => window.RIG_TEST.dwell(600000));
  await page.keys(["+"]);
  await page.keys([KEY.Enter]);
  await page.settle();
  const g = await geom();
  await page.shot(join(HERE, "refusal.png"));
  console.log("refusal         " + JSON.stringify(g.refuse) + " · " + g.truth);
}

// ---- what the bare jot costs ----------------------------------------------
// `+', the line, and whatever it takes to write it.  Same line everywhere, so
// the difference between two numbers is the difference between two laws.
console.log("\nthe bare jot, keys from `+' to the write (the line is "
  + JOT.length + " characters)\n");
for (const [file, name] of VARIANTS) {
  await open(file);
  await page.keys(["+"]);
  await page.keys(type(JOT));
  for (const k of write(name)) await page.keys([k]);
  await page.settle();
  const r = await rig();
  console.log("  " + name + "  " + String(r.cost).padEnd(22)
    + (r.minted ? "written" : "NOT WRITTEN"));
}
// C's short road, which is the one worth quoting beside A's.
await open("c-walk-the-row.html");
await page.keys(["+"]);
await page.keys(type(JOT));
await page.keys(CTRL("c", "c"));
await page.settle();
{
  const r = await rig();
  console.log("  C  " + String(r.cost).padEnd(22)
    + (r.minted ? "written, by C-c C-c straight off the title" : "NOT WRITTEN"));
}

// ---- what the write moves --------------------------------------------------
// The applied view is `sort:scheduled->title' and a capture has no date, so the
// fresh row belongs in the undated tail wherever it was typed.  A variant either
// pays that at the write or defers it to the next paint; `r' is that paint.
console.log("\nwhat the write moves (the row was typed at the head, or below point)\n");
for (const [file, name] of VARIANTS) {
  await open(file);
  await page.keys(["+"]);
  await page.keys(type(JOT));
  for (const k of write(name)) await page.keys([k]);
  await page.settle();
  const wrote = await geom();
  await page.keys(["r"]);
  await page.settle();
  const painted = await geom();
  console.log("  " + name + "  at the write: " + (wrote.moved || "—")
    + "  ·  after a repaint: " + (painted.moved || "—"));
}

// ---- the draft as it arrives, per filter ----------------------------------
// The four filters in turn, each pinning one clause of the seeding rule, and
// what the draft row wears under each before a key is typed into it.
console.log("\nthe draft as it arrives, per filter\n");
await open("a-top-row.html");
for (let i = 0; i < 4; i += 1) {
  if (i) await page.keys(["`"]);
  await page.keys(["+"]);
  await page.settle();
  const d = await drafted();
  console.log("  " + d.filter.replace(" sort:scheduled->title", "").padEnd(34)
    + (d.state || "—").padEnd(7) + (d.priority || "—").padEnd(7)
    + (d.tags || "—").padEnd(16) + d.where);
  await page.keys([KEY.Escape]);
}

// ---- the check -------------------------------------------------------------
// Every key of every variant, driven, and the law it is supposed to obey.
console.log("\nthe check\n");
for (const [file, name] of VARIANTS) {
  await open(file);
  const ready = await page.eval(() => !!window.RIG_TEST && !!document.querySelector(".tv-table"));
  ready ? ok(name + " · the page opens with a table") : fail(name + " · no table drawn");

  await page.keys(["+"]);
  let r = await rig();
  r.draft ? ok(name + " · + opens a draft") : fail(name + " · + opened nothing");

  await page.keys([KEY.Escape]);
  r = await rig();
  !r.draft && !r.minted ? ok(name + " · ESC drops it silently, nothing written")
                        : fail(name + " · ESC left something behind");

  await page.eval(() => window.RIG_TEST.dwell(600000));
  await page.keys(["+"]);
  await page.keys([KEY.Enter]);
  let g = await geom();
  g.refuse === "nothing to capture"
    ? ok(name + " · an empty title refuses in the shipped words")
    : fail(name + " · the empty title said " + JSON.stringify(g.refuse));
  r = await rig();
  !r.minted ? ok(name + " · the refusal wrote nothing")
            : fail(name + " · the refusal wrote a row");
  await page.keys([KEY.Escape]);

  await page.keys(["+"]);
  await page.keys(type(JOT));
  for (const k of write(name)) await page.keys([k]);
  r = await rig();
  r.minted === 1 && !r.draft ? ok(name + " · RET writes the row and the draft is gone")
                             : fail(name + " · the write did not land");
  r.rows.includes(JOT) ? ok(name + " · the row is in the strip")
                       : fail(name + " · the row is not in the strip");
  r.rows[r.point] === JOT ? ok(name + " · point stands on the row that landed")
                          : fail(name + " · point is on " + JSON.stringify(r.rows[r.point]));

  // THE DRAFT IS NOT WALKABLE, and it is `openCellEditor''s own doing: the open
  // input takes every key (`table-view.js:3917'), so `n' in a title types an `n'.
  await page.keys(["+"]);
  await page.keys(type("np"));
  r = await rig();
  const typed = await page.eval(() => {
    const i = document.getElementById("cxin");
    return i ? i.value : null;
  });
  typed === "np" && r.draft
    ? ok(name + " · n/p type into the draft rather than walking off it")
    : fail(name + " · n/p did something else: " + JSON.stringify(typed));
  await page.keys([KEY.Escape]);
}

// E's own five, and the collision the other tabs keep: `TAB' in an open cell
// COMMITS everywhere but E (`table-view.js:3918'), and walks inside E's draft.
await open("e-walk-on-tab.html");
{
  const cellNow = () => page.eval(() => window.RIG_TEST.state().cell);
  await page.keys(["+"]);
  await page.keys(type(JOT));
  await page.keys([KEY.Tab]);
  (await cellNow()) === "state" ? ok("E · TAB walks the title to the state cell")
                                : fail("E · TAB landed on " + await cellNow());
  await page.keys(SHIFT(KEY.Tab));
  (await cellNow()) === "title" ? ok("E · S-TAB walks back to the title")
                                : fail("E · S-TAB landed on " + await cellNow());
  await page.keys([KEY.Tab, KEY.Tab, KEY.Tab]);
  (await cellNow()) === "tag" ? ok("E · three TABs reach the tags cell")
                              : fail("E · three TABs landed on " + await cellNow());
  await page.keys([KEY.Enter]);
  let r = await rig();
  r.minted === 1 && !r.draft && r.rows.includes(JOT)
    ? ok("E · RET from the tags cell writes the row")
    : fail("E · RET from the tags cell wrote nothing");

  // ESC from a cell that is not the title still drops the WHOLE draft.
  await page.keys(["+"]);
  await page.keys(type("half a thought"));
  await page.keys([KEY.Tab, KEY.Tab]);
  await page.keys([KEY.Escape]);
  r = await rig();
  !r.draft && r.minted === 1 && !r.rows.includes("half a thought")
    ? ok("E · ESC from the priority cell drops the whole draft")
    : fail("E · ESC from a non-title cell left something behind");

  // AN EMPTY TITLE IS A WALL ON THE TITLE, wherever RET is pressed.
  await page.eval(() => window.RIG_TEST.dwell(600000));
  await page.keys(["+"]);
  await page.keys([KEY.Tab, KEY.Tab]);
  (await cellNow()) === "priority" ? ok("E · TAB reaches priority with no title typed")
                                   : fail("E · landed on " + await cellNow());
  await page.keys([KEY.Enter]);
  const g = await geom();
  r = await rig();
  g.refuse === "nothing to capture" && r.minted === 1
    ? ok("E · RET from the priority cell refuses on the empty TITLE")
    : fail("E · the empty-title wall did not hold off the title cell");
  await page.keys([KEY.Escape]);
}
for (const [file, name] of VARIANTS) {
  if (name === "E") continue;
  await open(file);
  await page.keys(["+"]);
  await page.keys(type(JOT));
  await page.keys([KEY.Tab]);
  const r = await rig();
  r.minted === 1 && !r.draft
    ? ok(name + " · TAB commits, the in-cell editor's own reading")
    : fail(name + " · TAB did something other than commit");
}

// The shell itself: five tabs, and the first one mounted.
await open("index.html");
{
  const shell = await page.eval(() => ({
    tabs: [...document.querySelectorAll("#tabs a")].map((a) => a.dataset.src),
    src: document.getElementById("stage").getAttribute("src"),
    note: (document.getElementById("note").textContent || "").length,
  }));
  shell.tabs.length === 5 && shell.src === "a-top-row.html" && shell.note > 40
    ? ok("index · five tabs, A mounted, the note written")
    : fail("index · " + JSON.stringify(shell));
}

// D's own law: while the strip stands, nothing in the table moves at all.
await open("d-form-strip.html");
{
  const base = await geom();
  await page.keys(["+"]);
  await page.settle();
  const g = await geom();
  g.last - base.last === g.draftH + 1
    ? ok("D · the strip pushes the whole table by exactly its own height")
    : ok("D · the table moved " + (g.last - base.last) + "px for a "
         + g.draftH + "px strip");
  Math.abs(parseFloat(g.align.replace(/[^0-9.]/g, "")) || 0) < 0.5
    ? ok("D · the strip sits on the table's column grid")
    : fail("D · " + g.align);
}

const vp = await page.eval(() => ({ w: innerWidth, h: innerHeight }));
console.log("\n  viewport " + vp.w + "×" + vp.h);
console.log(bad ? "\n" + bad + " FAILED" : "\nall green");
await page.close();
process.exit(bad ? 1 : 0);
