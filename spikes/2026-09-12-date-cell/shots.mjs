// The screenshots, headless — each tab at the moment that shows what it is for,
// plus the refusal, plus the draft row, plus every number this README quotes.
// `node shots.mjs'.
//
// THE NUMBERS ARE MEASURED, NOT CHOSEN: how wide the ghost wants to be, how
// wide the cell it was given is, how many pixels of it are cut, and how far the
// rows below move for C's strip.  A trade-off with no number on it is an
// opinion.  The last pass is a CHECK: every key of every variant driven, and
// what the rig says afterwards compared against what the law says it should.
//
// THE DAY IS PINNED at `?day=2026-09-12' so a screenshot taken tomorrow is the
// same screenshot.  One clock read per mount is the rig's own law and the check
// asserts the page took it.
import { chromium, KEY } from "./cdp.mjs";
import { fileURLToPath, pathToFileURL } from "node:url";
import { dirname, join } from "node:path";

const HERE = dirname(fileURLToPath(import.meta.url));
const DAY = "2026-09-12";
const url = (f) => pathToFileURL(join(HERE, f)).href
  + (f.indexOf("index") === 0 ? "" : "?day=" + DAY);
const type = (s) => [...s];
const CTRL = (...ks) => [{ down: "Ctrl" }, ...ks, { up: "Ctrl" }];
const SHIFT = (...ks) => [{ down: "Shift" }, ...ks, { up: "Shift" }];
/** TAB, N TIMES, ONE PRESS PER CALL.  Each walk step redraws the row and the
 * fresh input is focused a macrotask later, so two TABs in one dispatch reach
 * the same (detached) input twice. */
const tabs = async (n) => { for (let i = 0; i < n; i += 1) await page.keys([KEY.Tab]); };

const VARIANTS = [
  ["a-ghost-in-cell.html", "A", "ghost in the cell"],
  ["b-ghost-in-neighbour.html", "B", "ghost in the neighbour"],
  ["c-strip-under-row.html", "C", "strip under the row"],
];
const PHRASE = "18 aug";
const STAMP = "<2026-08-18 Tue>";

const page = await chromium();
let bad = 0;
const fail = (what) => { bad += 1; console.log("  FAIL  " + what); };
const ok = (what) => console.log("  ok    " + what);

const rig = () => page.eval(() => window.RIG_TEST.state());
const ghost = () => page.eval(() => window.RIG_TEST.ghost());
const geom = () => page.eval(() => window.RIG_TEST.geom());
const room = () => page.eval(() => window.RIG_TEST.room());
const caret = () => page.eval(() => window.RIG_TEST.caret());

async function open(file) {
  await page.goto(url(file));
  await page.settle();
}
/** `RET' over the SCHEDULED cell of the row at point. */
const openCell = async () => { await page.keys([KEY.Enter]); await page.settle(); };

// ---- the cell this whole spike is about ------------------------------------
console.log("\nthe cell\n");
await open("a-ghost-in-cell.html");
{
  const r = await room();
  console.log("  a date column is " + r.col + "px wide, its text run " + r.text
    + "px — `calc(13ch + 24px)': ten characters of `isoStamp', three of the sort"
    + " mark, and CELL_PAD both sides");
  const want = await page.eval((s) => window.RIG_TEST && (() => {
    const m = document.getElementById("cdmeasure");
    const at = (t) => { m.textContent = t; return Math.round(m.getBoundingClientRect().width); };
    return { stamp: at(s), ghost: at(" → " + s), iso: at("2026-08-18"),
             phrase: at("18 aug"), range: at("from 18 to 19 aug"),
             long: at(" → <2026-08-18 Tue>--<2026-08-19 Wed>") };
  })(), STAMP);
  console.log("  what has to fit in it:");
  console.log("    2026-08-18                  " + want.iso + "px   the cell's own spelling, and what the column was SIZED for");
  console.log("    18 aug                      " + want.phrase + "px   a short phrase");
  console.log("    from 18 to 19 aug           " + want.range + "px   a phrase that is not short");
  console.log("    <2026-08-18 Tue>            " + want.stamp + "px   the stamp the wire carries");
  console.log("    ` → <2026-08-18 Tue>'       " + want.ghost + "px   the ghost, as `dateGhost' spells it");
  console.log("    ` → <…Tue>--<…Wed>'         " + want.long + "px   the ghost over a range");
}

// ---- the three shots -------------------------------------------------------
console.log("\nthe three shots\n");

for (const [file, name, what] of VARIANTS) {
  await open(file);
  await openCell();
  await page.keys(type(PHRASE));
  await page.settle();
  const g = await ghost();
  await page.shot(join(HERE, file.replace(".html", ".png")));
  console.log("  " + name + " · " + what.padEnd(24)
    + "ghost wants " + String(g.want).padStart(4) + "px, drawn "
    + String(g.drew).padStart(4) + "px, "
    + (g.cut ? "CLIPPED by " + g.cut + "px" : "whole"));
}

// A ON A PHRASE THAT IS NOT SHORT, which is where its claim ends: the ghost is
// gone entirely and the phrase itself has scrolled out of the field.
await open("a-ghost-in-cell.html");
{
  await openCell();
  await page.keys([KEY.Backspace]);
  await page.keys(type("from 18 to 19 aug"));
  await page.settle();
  const g = await ghost();
  await page.shot(join(HERE, "a-long-phrase.png"));
  console.log("  A on `from 18 to 19 aug'    ghost wants " + g.want
    + "px, drawn " + g.drew + "px, CLIPPED by " + g.cut + "px");
}

// The refusal, its own picture: `18 auk' is no date, the ghost says so in the
// refusal's own red, and `RET' leaves the cell open.
await open("c-strip-under-row.html");
{
  await openCell();
  await page.keys(type("18 auk"));
  await page.keys([KEY.Enter]);
  await page.settle();
  const r = await rig();
  const g = await ghost();
  await page.shot(join(HERE, "refusal.png"));
  console.log("  refusal                     " + JSON.stringify(g.text)
    + " · RET refused, the cell is " + (r.editing ? "still open" : "GONE"));
}

// The draft row in the winning variant: `TAB' from the tags cell reaches
// SCHEDULED, a phrase is typed, `RET' captures with the planning line.
await open("c-strip-under-row.html");
{
  await page.keys(["+"]);
  await page.keys(type("Swap the SIM card"));
  await tabs(3);                       // title → state → priority → scheduled
  await page.keys(type(PHRASE));
  await page.settle();
  await page.shot(join(HERE, "draft.png"));
  const r = await rig();
  console.log("  draft                       TAB reached " + r.key
    + " · the strip carries " + JSON.stringify(PHRASE) + " and its resolution");
}

// ---- what fits, per placement, at six real phrases -------------------------
// The ghost's own width never changes — a single stamp is 137px wherever it is
// drawn — so what varies is the ROOM each placement gives it, and what the
// reader can still see of the phrase they are typing.
console.log("\nwhat fits, at six phrases that all resolve\n");
const PHRASES = ["18 aug", "18 august", "2026-08-18", "18 august 2027",
                 "from 18 to 19 aug", "from 18 to 19 august 2027"];
console.log("  " + "phrase".padEnd(27) + "A ghost".padEnd(16)
  + "B ghost".padEnd(16) + "C ghost".padEnd(16) + "the phrase, in the cell");
for (const phrase of PHRASES) {
  const said = [];
  let seen = "";
  for (const [file] of VARIANTS) {
    await open(file);
    await openCell();
    await page.keys([KEY.Backspace]);
    await page.keys(type(phrase));
    await page.settle();
    const g = await ghost();
    said.push((g.cut ? "cut " + g.cut + "px" : "whole").padEnd(16));
    seen = await page.eval(() => {
      const i = document.getElementById("cdin");
      return i ? (i.scrollWidth <= i.clientWidth ? "whole" : "scrolled out of sight") : "?";
    });
  }
  console.log("  " + phrase.padEnd(27) + said.join("") + seen);
}

// ---- what C's strip moves ---------------------------------------------------
console.log("\nwhat the strip moves (C alone; A and B move nothing)\n");
for (const [file, name] of VARIANTS) {
  await open(file);
  const before = await geom();
  await openCell();
  const during = await geom();
  await page.keys([KEY.Escape]);
  await page.settle();
  const after = await geom();
  const moved = (a, b, id) => (b.tops[id] || 0) - (a.tops[id] || 0);
  console.log("  " + name + "  row height " + before.rowH + "px · the strip is "
    + during.strip + "px · the row below the edit moved "
    + moved(before, during, "r2") + "px while it stood, the last row "
    + moved(before, during, "r6") + "px · after ESC they are "
    + moved(before, after, "r2") + "px and " + moved(before, after, "r6")
    + "px off where they began");
}

// ---- the check --------------------------------------------------------------
// Every key of every variant, driven, and the law it is supposed to obey.
console.log("\nthe check\n");

// ONE CLOCK READ, and the page and the test agree on the day.
await open("a-ghost-in-cell.html");
{
  const d = await page.eval(() => window.RIG_TEST.day());
  d.y === 2026 && d.m === 9 && d.d === 12
    ? ok("CLOCK · one read at mount, and the page's day is the pinned one")
    : fail("CLOCK · the page thinks it is " + JSON.stringify(d));
}

for (const [file, name] of VARIANTS) {
  await open(file);
  let r = await rig();
  !r.editing && r.col === "scheduled"
    ? ok(name + " · the page opens with the cursor on a date cell")
    : fail(name + " · " + JSON.stringify(r.col));

  // OPEN · `RET' over the cell opens the widget ON THE CELL'S OWN VALUE, and
  // the value comes up WHOLLY SELECTED (the 08-23 spike's round 3).
  await openCell();
  r = await rig();
  let c = await caret();
  r.editing && r.key === "scheduled" && r.phrase === "2026-09-15"
    ? ok(name + " · RET opens SCHEDULED on the cell's own value")
    : fail(name + " · opened on " + JSON.stringify(r.phrase));
  c && c.start === 0 && c.end === c.value.length && c.value.length > 0
    ? ok(name + " · ENTRY · the opening value comes up wholly selected")
    : fail(name + " · ENTRY · the selection is " + JSON.stringify(c));

  // ENTRY-GHOST · AND HERE THE TABLE DIVERGES FROM THE DOC PANE.  `dateGhost'
  // falls silent where the resolution IS what was typed, which in the sheet is
  // every opening value — the planning line holds org's own `<2026-09-15 Tue>'.
  // A TABLE CELL HOLDS `isoStamp''s `2026-09-15' (`Query.hs:1342'), which is a
  // different spelling of the same day, so the ghost SPEAKS the moment the cell
  // opens and the widget draws its widest thing before a key is pressed.
  let g = await ghost();
  g.text === " → <2026-09-15 Tue>"
    ? ok(name + " · ENTRY-GHOST · the cell's ISO spelling is not org's, so the ghost speaks at entry")
    : fail(name + " · the ghost said " + JSON.stringify(g.text));
  g.cut ? ok(name + " · ENTRY-GHOST · and it is already cut by " + g.cut + "px, untyped")
        : ok(name + " · ENTRY-GHOST · and it fits, untyped");

  // A KEYSTROKE REPLACES THE WHOLE OF IT, the selection being what it is.
  await page.keys(type("1"));
  c = await caret();
  c.value === "1" ? ok(name + " · a keystroke replaces the selected value whole")
                  : fail(name + " · the field holds " + JSON.stringify(c.value));
  await page.keys([KEY.Escape]);

  // WRITING · a term half typed shows nothing; a refusal at every keystroke is
  // a refusal nobody reads (`dateWriting', 15-dates.js:260).
  await openCell();
  await page.keys(type("18 a"));
  g = await ghost();
  g.text === "" ? ok(name + " · WRITING · `18 a' is half a month, and says nothing")
                : fail(name + " · `18 a' said " + JSON.stringify(g.text));

  // GHOST · a term that resolves shows `→ <stamp>', weekday computed.
  await page.keys(type("ug"));
  g = await ghost();
  g.text === " → " + STAMP && !g.bad
    ? ok(name + " · GHOST · `18 aug' resolves to " + STAMP)
    : fail(name + " · `18 aug' said " + JSON.stringify(g.text));

  // CARET · the ghost is a SPAN and never the field's value: the caret walks to
  // the end of what was TYPED and stops there.
  for (let i = 0; i < 8; i += 1) await page.keys([KEY.ArrowRight]);
  c = await caret();
  c.value === PHRASE && c.start === PHRASE.length
    ? ok(name + " · CARET · the caret clamps at the end of the phrase")
    : fail(name + " · the caret reached " + JSON.stringify(c));

  // STEP · the shifted arrows adjust the VALUE — a day, then a week.
  await page.keys(SHIFT(KEY.ArrowRight));
  r = await rig();
  r.phrase === "2026-08-19"
    ? ok(name + " · STEP · S-<right> takes the resolved day one day on")
    : fail(name + " · S-<right> left " + JSON.stringify(r.phrase));
  await page.keys(SHIFT(KEY.ArrowDown));
  r = await rig();
  r.phrase === "2026-08-26"
    ? ok(name + " · STEP · S-<down> takes it one week on")
    : fail(name + " · S-<down> left " + JSON.stringify(r.phrase));
  await page.keys(SHIFT(KEY.ArrowUp));
  await page.keys(SHIFT(KEY.ArrowLeft));
  r = await rig();
  r.phrase === "2026-08-18"
    ? ok(name + " · STEP · S-<up> and S-<left> put it back")
    : fail(name + " · the walk left " + JSON.stringify(r.phrase));

  // COMMIT · `RET' writes the RESOLVED stamp; the CELL shows the table's own
  // spelling and the WIRE carries org's.
  await page.keys([KEY.Enter]);
  await page.settle();
  r = await rig();
  !r.editing && r.rows[0].scheduled === "2026-08-18"
    ? ok(name + " · COMMIT · the cell shows the table's spelling, 2026-08-18")
    : fail(name + " · the cell holds " + JSON.stringify(r.rows[0].scheduled));
  r.wire.indexOf(STAMP) !== -1 && r.wire.indexOf("set-planning") === 0
    ? ok(name + " · WIRE · set-planning carries " + STAMP)
    : fail(name + " · the wire said " + JSON.stringify(r.wire));

  // ESC · the cell comes back BYTE FOR BYTE, the spelling the edit FOUND.
  await openCell();
  await page.keys(type("from 18 to 19 aug"));
  await page.keys([KEY.Escape]);
  await page.settle();
  r = await rig();
  !r.editing && r.rows[0].scheduled === "2026-08-18"
    ? ok(name + " · ESC · the value the edit found is back, byte for byte")
    : fail(name + " · ESC left " + JSON.stringify(r.rows[0].scheduled));

  // REFUSE · an unreadable phrase shows `✗' and `RET' refuses IN PLACE.
  await openCell();
  await page.keys(type("18 auk"));
  g = await ghost();
  g.bad && g.text === " ✗ not a date"
    ? ok(name + " · REFUSE · an unreadable phrase shows ✗ before the commit")
    : fail(name + " · it said " + JSON.stringify(g.text));
  await page.keys([KEY.Enter]);
  await page.settle();
  r = await rig();
  r.editing && r.phrase === "18 auk" && r.rows[0].scheduled === "2026-08-18"
    ? ok(name + " · REFUSE · RET refuses and the cell stays open, unwritten")
    : fail(name + " · RET on a refusal " + (r.editing ? "kept" : "closed") + " the cell");
  await page.keys([KEY.Escape]);

  // `next fri' IS NOT IN THE SHIPPED GRAMMAR, and the rig says so rather than
  // inventing it.  A spike that quietly grew the parser would be arguing for a
  // widget the app cannot build.
  await openCell();
  await page.keys(type("next fri"));
  g = await ghost();
  g.bad ? ok(name + " · `next fri' is refused — the shipped grammar has no weekday words")
        : fail(name + " · `next fri' resolved, which the app's own reader does not do");
  await page.keys([KEY.Escape]);

  // EMPTY CLEARS IT, the shipped foot's own promise.
  await openCell();
  await page.keys([KEY.Backspace]);
  await page.keys([KEY.Enter]);
  await page.settle();
  r = await rig();
  r.rows[0].scheduled === "" && r.wire.indexOf('"date": null') !== -1
    ? ok(name + " · CLEAR · an empty field takes the entry off")
    : fail(name + " · clearing left " + JSON.stringify(r.rows[0].scheduled));

  // THE OTHER DOOR: `C-c C-s' and `C-c C-d' over the row at point, the app's
  // own two (`Keymap.hs:100', `:102').
  await page.keys(CTRL("c"));
  await page.keys(CTRL("d"));
  await page.settle();
  r = await rig();
  r.editing && r.key === "deadline"
    ? ok(name + " · C-c C-d opens DEADLINE over the row at point")
    : fail(name + " · C-c C-d opened " + JSON.stringify(r.key));
  await page.keys([KEY.Escape]);
  await page.keys(CTRL("c"));
  await page.keys(CTRL("s"));
  await page.settle();
  r = await rig();
  r.editing && r.key === "scheduled"
    ? ok(name + " · C-c C-s opens SCHEDULED over the row at point")
    : fail(name + " · C-c C-s opened " + JSON.stringify(r.key));
  await page.keys([KEY.Escape]);

  // The open cell takes its own keys, so `n' types an `n' rather than walking.
  await openCell();
  await page.keys(type("np"));
  const c2 = await caret();
  c2.value === "np"
    ? ok(name + " · n/p type into the open cell rather than walking off it")
    : fail(name + " · the field holds " + JSON.stringify(c2.value));
  await page.keys([KEY.Escape]);

  // VERBATIM · org's own bracketed spelling is kept as it stands, wrong weekday
  // and all (`verbatimDate', 15-dates.js:88; test/TestQuery.hs:1791).
  await openCell();
  await page.keys([KEY.Backspace]);
  await page.keys(type("<2026-08-05 Mon>"));
  g = await ghost();
  g.text === ""
    ? ok(name + " · VERBATIM · org's own spelling adds no ghost — 2026-08-05 is a Wed and stays a Mon")
    : fail(name + " · the ghost respelled it: " + JSON.stringify(g.text));
  await page.keys([KEY.Enter]);
  await page.settle();
  r = await rig();
  r.wire.indexOf("<2026-08-05 Mon>") !== -1
    ? ok(name + " · VERBATIM · the wire carries the spelling it was given")
    : fail(name + " · the wire said " + JSON.stringify(r.wire));
}

// B's own two: which cell is lent, and that the lender gets its value back.
await open("b-ghost-in-neighbour.html");
{
  const lent = () => page.eval(() => {
    const td = document.querySelector("td.cd-lent");
    if (!td) return null;
    const tr = td.parentElement;
    const at = [...tr.children].indexOf(td);
    return { at, text: td.textContent.trim(),
             wide: Math.round(td.getBoundingClientRect().width) };
  });
  await openCell();
  await page.keys(type(PHRASE));
  await page.settle();
  let l = await lent();
  l && l.at === 4
    ? ok("B · editing SCHEDULED lends DEADLINE (column 5), " + l.wide + "px wide")
    : fail("B · lent " + JSON.stringify(l));
  await page.keys([KEY.Escape]);
  await page.settle();
  let r = await rig();
  r.rows[0].deadline === "2026-09-30"
    ? ok("B · the lender's own value comes back on ESC")
    : fail("B · the neighbour holds " + JSON.stringify(r.rows[0].deadline));

  await page.keys(["f"]);              // the cursor to DEADLINE
  await openCell();
  await page.keys(type(PHRASE));
  await page.settle();
  l = await lent();
  l && l.at === 5
    ? ok("B · editing DEADLINE lends Tags (column 6), " + l.wide + "px wide — the only run wide enough")
    : fail("B · lent " + JSON.stringify(l));
  await page.keys([KEY.Escape]);
  await page.settle();
  r = await rig();
  r.rows[0].deadline === "2026-09-30"
    ? ok("B · ESC from DEADLINE puts DEADLINE back too")
    : fail("B · DEADLINE holds " + JSON.stringify(r.rows[0].deadline));
}

// C's own two: the strip's height is what the rows below pay, and they get it
// back to the pixel.
await open("c-strip-under-row.html");
{
  const before = await geom();
  await openCell();
  const during = await geom();
  during.strip > 0 && during.tops.r2 - before.tops.r2 === during.strip
    ? ok("C · the rows below move by exactly the strip's height (" + during.strip + "px)")
    : fail("C · the strip is " + during.strip + "px and the row below moved "
           + (during.tops.r2 - before.tops.r2) + "px");
  before.tops.r1 === during.tops.r1
    ? ok("C · the edited row and everything above it hold still")
    : fail("C · the edited row moved " + (during.tops.r1 - before.tops.r1) + "px");
  await page.keys([KEY.Escape]);
  await page.settle();
  const after = await geom();
  after.tops.r2 === before.tops.r2 && after.tops.r6 === before.tops.r6
    ? ok("C · ESC puts every row back to the pixel")
    : fail("C · the rows are " + (after.tops.r6 - before.tops.r6) + "px off");

  // The strip carries the PHRASE as well as the answer, which is the thing a
  // 94px cell cannot do: a long phrase is readable in full.
  await openCell();
  await page.keys(type("from 18 to 19 aug"));
  await page.settle();
  const said = await page.eval(() => {
    const td = document.querySelector("tr.cd-strip td");
    return td ? { text: td.textContent.trim(),
                  wide: Math.round(td.getBoundingClientRect().width),
                  scroll: td.scrollWidth <= td.clientWidth } : null;
  });
  said && said.text.indexOf("from 18 to 19 aug →") === 0 && said.scroll
    ? ok("C · the strip carries the whole phrase AND its resolution, uncut, in "
         + said.wide + "px")
    : fail("C · the strip said " + JSON.stringify(said));
  await page.keys([KEY.Escape]);
}

// THE DRAFT ROW, in the winner: `TAB' from the tags cell reaches SCHEDULED, a
// phrase is typed, and `RET' captures with the planning line.
console.log("");
await open("c-strip-under-row.html");
{
  await page.keys(["+"]);
  await page.settle();
  let r = await rig();
  r.draft && r.cell === "title"
    ? ok("DRAFT · + opens a draft row with its title cell open")
    : fail("DRAFT · + opened " + JSON.stringify(r));
  await page.keys(type("Swap the SIM card"));
  await tabs(3);                       // title → state → priority → scheduled
  r = await rig();
  r.editing && r.key === "scheduled"
    ? ok("DRAFT · TAB walks the draft's cells and reaches SCHEDULED")
    : fail("DRAFT · three TABs landed on " + JSON.stringify(r.cell));
  await page.keys(type(PHRASE));
  await page.settle();
  const g = await ghost();
  g.text === " → " + STAMP
    ? ok("DRAFT · the same ghost, the same grammar, in the draft's own cell")
    : fail("DRAFT · the draft's ghost said " + JSON.stringify(g.text));

  // A phrase that does not read refuses IN PLACE, exactly as an empty title
  // does — the wall is on the phrase and the draft is not thrown away.
  await page.keys(type("q"));
  await page.keys([KEY.Enter]);
  await page.settle();
  r = await rig();
  r.draft && r.editing
    ? ok("DRAFT · RET over an unreadable phrase refuses and keeps the draft")
    : fail("DRAFT · the refusal " + (r.draft ? "closed the cell" : "dropped the draft"));
  await page.keys([KEY.Backspace]);
  await page.keys([KEY.Enter]);
  await page.settle();
  r = await rig();
  r.rows[0] && r.rows[0].title === "Swap the SIM card"
      && r.rows[0].scheduled === "2026-08-18"
    ? ok("DRAFT · RET captures the row with its date")
    : fail("DRAFT · the capture landed as " + JSON.stringify(r.rows[0]));
  r.wire.indexOf('"planning": [["SCHEDULED","' + STAMP + '"]]') !== -1
    ? ok("DRAFT · the capture carries planning [[SCHEDULED, " + STAMP + "]]")
    : fail("DRAFT · the wire said " + JSON.stringify(r.wire));

  // AN EMPTY TITLE still refuses in the shipped words, from the date cell too.
  await page.keys(["+"]);
  await tabs(3);
  await page.keys(type(PHRASE));
  await page.keys([KEY.Enter]);
  await page.settle();
  r = await rig();
  r.draft && r.draftRefused === "nothing to capture" && r.cell === "title"
    ? ok("DRAFT · an empty title refuses from the date cell and point goes back to the title")
    : fail("DRAFT · the empty title said " + JSON.stringify(r.draftRefused));
  await page.keys([KEY.Escape]);
  await page.settle();
  r = await rig();
  !r.draft ? ok("DRAFT · ESC drops the whole draft, silently")
           : fail("DRAFT · ESC left the draft standing");
}

// The shell itself: three tabs, and the first one mounted.
await open("index.html");
{
  const shell = await page.eval(() => ({
    tabs: [...document.querySelectorAll("#tabs a")].map((a) => a.dataset.src),
    src: document.getElementById("stage").getAttribute("src"),
    note: (document.getElementById("note").textContent || "").length,
  }));
  shell.tabs.length === 3 && shell.src === "a-ghost-in-cell.html" && shell.note > 40
    ? ok("index · three tabs, A mounted, the note written")
    : fail("index · " + JSON.stringify(shell));
}

const vp = await page.eval(() => ({ w: innerWidth, h: innerHeight }));
console.log("\n  viewport " + vp.w + "×" + vp.h);
console.log(bad ? "\n" + bad + " FAILED" : "\nall green");
await page.close();
process.exit(bad ? 1 : 0);
