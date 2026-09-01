// The complaint, mechanised: a date widget must SAY THE DATE BEFORE IT WRITES
// IT.  The summons are the app's own two, the walk lands where a calendar says
// it lands, the typed grammar resolves live, the committed stamp carries the
// RIGHT weekday — computed here by Zeller's congruence, which shares no code
// with the page — RET is dry and final, ESC takes the whole edit and gives the
// sheet back byte for byte, and the geometry is what each placement claims.
//
// THE CHECK ALSO HAS TO BITE.  `rig.js' carries five deliberate faults behind
// `?bug=': a day-walk that skips, a weekday that is remembered rather than
// computed, a resolution one step behind the field it speaks for, a ghost
// written INTO the field where the caret can walk into it, and an ESC that
// takes the menu instead of the edit, and an edit that opens with the caret
// collapsed instead of the value selected.  Each is run against the rung that should
// catch it, and a fault that passes is reported as loudly as a failure — a rung
// that cannot fail was never testing anything.
//
//   node check.mjs                     # every variant, then the six faults
//   node check.mjs c-month-inline.html # one
import { firefox, KEY } from "./bidi.mjs";
import { chromium, KEY as CK, pixels } from "./cdp.mjs";
import { readFile } from "node:fs/promises";
import { pathToFileURL, fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { rm } from "node:fs/promises";

const HERE = dirname(fileURLToPath(import.meta.url));
const ALL = ["a-control.html", "b-month-popup.html", "c-month-inline.html",
             "d-text-first.html", "e-day-strip.html"];
const picked = process.argv[2] ? [process.argv[2]] : ALL;

// THE CONTROL MISSES BY CONSTRUCTION, the way the sibling spikes' controls do:
// A is TODAY — a blind field raised over a veil.  It has no calendar to walk,
// no preview to read, no offers to be dry about, and nothing in the pane for a
// geometry rung to measure.  Declared, so the run is green and the misses are
// the argument rather than a broken tab.
const MISSES = {
  "a-control.html": ["WALK", "PREVIEW", "DRY", "QUIET", "GEOM", "ECHO"],
  // CARET runs only where a GHOST exists to walk into, which is D alone; the
  // narrowing is read off the variant rather than listed, below.
};
// A DEPARTS ON THE WASH, and the departure is the app's own.  Every other field
// in this spike wears the pane's edit dress, whose `::selection' rule is
// `Style.hs:374'.  The shipped `askText' prompt is dressed at `Style.hs:391' and
// is named in NO selection list anywhere in the renderer — so today's blind
// prompt paints its selection in the BROWSER'S default, not the pane's.  The
// paint rung still demands a VISIBLE selection there; it just cannot demand the
// pane's own colour, because the pane never gave it one.
const NO_WASH = {
  "a-control.html":
    "the shipped prompt wears no ::selection rule (Style.hs:391 dresses #pinput; "
    + "no selection list names it), so its wash is the browser's, not the pane's",
};
// A DEPARTS ON THE GRAMMAR, and the departure is asserted rather than skipped:
// its reader is the shipped one (`docs/commands.md:51'), so the English forms
// the proposal owes are REFUSED there — and refused after the box has shut,
// which is the whole of what B..E are arguing against.
const SHIPPED_ONLY = "a-control.html";

// ===================================================================== dates
// The check's own calendar, kept apart from the page's on purpose.
const civil = (n) => {
  const t = new Date(n * 86400000);
  return { y: t.getUTCFullYear(), m: t.getUTCMonth() + 1, d: t.getUTCDate() };
};
const dnum = (c) => Math.round(Date.UTC(c.y, c.m - 1, c.d) / 86400000);
const addDays = (c, n) => civil(dnum(c) + n);
const leap = (y) => (y % 4 === 0 && y % 100 !== 0) || y % 400 === 0;
const daysIn = (y, m) =>
  [31, leap(y) ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31][m - 1];
const addMonths = (c, n) => {
  const k = (c.m - 1) + n;
  const y = c.y + Math.floor(k / 12);
  const m = ((k % 12) + 12) % 12 + 1;
  return { y, m, d: Math.min(c.d, daysIn(y, m)) };
};
const pad2 = (n) => (n < 10 ? "0" : "") + n;
const iso = (c) => `${c.y}-${pad2(c.m)}-${pad2(c.d)}`;
/** ZELLER'S CONGRUENCE — the weekday derived from the date by arithmetic that
 * shares nothing with the page's `Date.getUTCDay'.  Two implementations, one
 * answer: that is what makes "the weekday is COMPUTED" a claim a test can
 * hold rather than a comment. */
const zeller = ({ y, m, d }) => {
  let Y = y, M = m;
  if (M < 3) { M += 12; Y -= 1; }
  const K = Y % 100, J = Math.floor(Y / 100);
  const h = (d + Math.floor(13 * (M + 1) / 5) + K + Math.floor(K / 4)
             + Math.floor(J / 4) + 5 * J) % 7;
  return ["Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri"][h];
};
const stamp = (c) => `<${iso(c)} ${zeller(c)}>`;
const range = (a, b) => `${stamp(a)}--${stamp(b)}`;

const chars = (s) => [...s];
const CTRL = (k) => [{ down: KEY.Control }, k, { up: KEY.Control }];
// C-c C-s / C-c C-d — the org spelling, one Control held across the pair, which
// is the finger motion the Keymap names.
const SCHEDULE = [{ down: KEY.Control }, "c", "s", { up: KEY.Control }];
const DEADLINE = [{ down: KEY.Control }, "c", "d", { up: KEY.Control }];

// ====================================================================== page
const read = () => RIG.state();
const shot = () => RIG.picture();

const url = (page, bug) =>
  pathToFileURL(join(HERE, page)).href + (bug ? "?bug=" + bug : "");

const ff = await firefox().catch((e) => {
  console.error("no firefox: " + e.message);
  process.exit(2);
});

/** Put the widget's field under the caret.  A field the variant leads with is
 * already there; one behind a door needs the door's own key, which is `/' —
 * "`/' always narrows", the shell's own rule. */
async function intoField() {
  const s = await ff.eval(read);
  if (!s.inField) await ff.keys(["/"]);
}
/** Clear the field and type TEXT into it. */
async function type(text) {
  await ff.keys(CTRL("a"));
  await ff.keys(chars(text));
}
/** Summon SCHEDULED, from a closed widget. */
async function summon() {
  await ff.keys(SCHEDULE);
}
/** Walk point onto the drawer's date-shaped pair, wherever it sits. */
async function ontoPair() {
  for (let i = 0; i < 6; i += 1) {
    if ((await ff.eval(read)).atRow === "pair") return true;
    await ff.keys(["n"]);
  }
  return (await ff.eval(read)).atRow === "pair";
}
const paraTop = () => ff.eval(() =>
  Math.round(document.querySelector("#mdoc .d-para").getBoundingClientRect().top));
/** THE CYCLE'S TRUE END: every microtask drained, then two frames painted, then
 * one more turn of the loop.  A rung that reads a just-opened widget before this
 * is reading the middle of a redraw — round 4 was hunted here before it turned
 * out to be paint rather than timing, and the settle stays because the next one
 * may not be. */
const settle = () => ff.eval(() => new Promise((ok) => {
  requestAnimationFrame(() => requestAnimationFrame(() => setTimeout(ok, 0)));
}));

/** The open field, whichever surface this variant draws it on, with its
 * selection and whatever ghost rides beside it. */
const fieldNow = () => ff.eval(() => {
  const f = document.getElementById("dwfield") || document.getElementById("pinput");
  const g = document.getElementById("dwghost");
  if (!f) return null;
  return { v: f.value, a: f.selectionStart, b: f.selectionEnd,
           ghost: g ? g.textContent : null,
           ghostInField: g ? f.contains(g) : false };
});

// ===================================================================== rungs
for (const page of picked) {
  const bad = [];
  const misses = MISSES[page] || [];
  const skip = (rung) => misses.includes(rung);
  await ff.goto(url(page));

  const today = await ff.eval(() => RIG.today());
  const now = { y: +today.slice(0, 4), m: +today.slice(5, 7), d: +today.slice(8, 10) };
  const nodeToday = (() => {
    const n = new Date();
    return iso({ y: n.getFullYear(), m: n.getMonth() + 1, d: n.getDate() });
  })();

  // --- CLOCK: ONE READ, and the page and this process are on the same day.
  if (today !== nodeToday)
    bad.push(`the page's day is ${today}, node's is ${nodeToday}`
             + " — a run that crossed midnight, or two clocks");

  const before = await ff.eval(shot);

  // --- SUMMON: the app's own key opens the widget on THIS keyword, at today.
  await summon();
  let s = await ff.eval(read);
  if (!s.open) bad.push("C-c C-s opened nothing");
  if (s.keyword !== "SCHEDULED") bad.push(`C-c C-s summoned ${s.keyword}`);
  if (s.at !== today) bad.push(`the summon opened on ${s.at}, not today`);

  // --- PREVIEW: the planning line shows the stamp the RET would write, before
  //     the RET.  A has none: a veiled sheet cannot preview anything.
  if (!skip("PREVIEW")) {
    // WHERE THE PREVIEW LIVES IS THE VARIANT'S; THAT THERE IS ONE IS NOT.  A
    // docked variant draws it on the planning line's value span; a SLOT variant
    // stands in that span's place and says it in the ghost.
    const shown = await ff.eval(() => {
      const d = document.querySelector("#mdoc .d-plan .dv.pending");
      if (d) return d.textContent;
      const g = document.querySelector("#mdoc .d-plan #dwghost");
      return g ? g.textContent.replace(/^[\s→]*/, "") : "";
    });
    if (shown !== stamp(now))
      bad.push(`the planning line previews ${JSON.stringify(shown)}, `
               + `wanted ${stamp(now)}`);
  }

  // --- WALK: three grains, each landing where a calendar says it lands.
  //     TWO DIALECTS, chosen by who holds the keys.  Where a GRID holds them
  //     the letters are the walk and `< > .' are live.  Where the FIELD holds
  //     them the plain keys belong to the caret, so the walk is org's own
  //     shifted arrows — and since round 1 took D's grid away, `< > .' retire
  //     with it: the check asserts that retirement rather than dropping it.
  if (!skip("WALK")) {
    const land = async (keys) => {
      await ff.keys(keys);
      return (await ff.eval(read)).at;
    };
    const SR = [{ down: KEY.Shift }, KEY.ArrowRight, { up: KEY.Shift }];
    const SL = [{ down: KEY.Shift }, KEY.ArrowLeft, { up: KEY.Shift }];
    const SD = [{ down: KEY.Shift }, KEY.ArrowDown, { up: KEY.Shift }];
    const SU = [{ down: KEY.Shift }, KEY.ArrowUp, { up: KEY.Shift }];
    // The two grains that need no frame to act on, asserted in EVERY gridless
    // and gridded tab alike: they are the walk the field keeps.
    const f1 = await land(SR);
    if (f1 !== iso(addDays(now, 1))) bad.push(`S-→ landed on ${f1}, not tomorrow`);
    const f2 = await land(SD);
    if (f2 !== iso(addDays(now, 8))) bad.push(`S-↓ landed on ${f2}, not a week on`);
    const f3 = await land([...SU, ...SL]);
    if (f3 !== today) bad.push(`S-↑ S-← landed on ${f3}, not back on today`);

    if (!s.grid) {
      // ROUND 1's RETIREMENT, asserted: with no calendar frame, `<', `>' and
      // `.' have nothing to scroll and nothing to go to, so they are CHARACTERS
      // in the field — and the grains they carried are words the field reads.
      await type(".");
      let st = await ff.eval(read);
      if (st.at === today && st.text !== ".")
        bad.push("`.' still jumped to today — with no calendar it is a character");
      if (st.text !== ".") bad.push(`\`.' left the field saying ${JSON.stringify(st.text)}`);
      await type(">");
      st = await ff.eval(read);
      if (st.text !== ">") bad.push("`>' still moved a month with no calendar to move");
      // …and the month grain is spelled instead, in the grammar the app ships.
      await type("today+1m");
      st = await ff.eval(read);
      if (st.stamp !== stamp(addMonths(now, 1)))
        bad.push(`today+1m resolved to ${JSON.stringify(st.stamp)}`);
      await type("today");
      st = await ff.eval(read);
      if (st.stamp !== stamp(now))
        bad.push("the word today no longer stands in for the retired `.'");
      // TAB HAS NOWHERE TO HOP and must not let focus out of an open widget.
      await ff.keys([KEY.Tab]);
      if (!(await ff.eval(read)).inField)
        bad.push("TAB left the field though there is no grid to hop to");
    } else {
      if (s.field === "primary") {
        await ff.keys([KEY.Tab]);
        if ((await ff.eval(read)).inField)
          bad.push("TAB did not hop out of the field");
      }
      const at1 = await land(["n"]);
      if (at1 !== iso(addDays(now, 1))) bad.push(`n landed on ${at1}, not tomorrow`);
      const at2 = await land(["p"]);
      if (at2 !== today) bad.push(`p landed on ${at2}, not back on today`);
      const at3 = await land(["f"]);
      if (at3 !== iso(addDays(now, 7))) bad.push(`f landed on ${at3}, not a week on`);
      const at4 = await land(["b"]);
      if (at4 !== today) bad.push(`b landed on ${at4}, not back on today`);
      const at5 = await land([">"]);
      if (at5 !== iso(addMonths(now, 1))) bad.push(`> landed on ${at5}, not a month on`);
      const at6 = await land(["<"]);
      if (at6 !== today) bad.push(`< landed on ${at6}, not back on today`);
      const at7 = await land(["f", "f", "."]);
      if (at7 !== today) bad.push(`. landed on ${at7}, not on today`);
      // THE WALK WRITES THE FIELD: the grid and the text are ONE value.
      const st = await ff.eval(read);
      if (st.text !== today)
        bad.push(`the walk left the field saying ${JSON.stringify(st.text)}`);
      // …AND LANDS ON A FINISHED TERM, so the offers are not asking.
      if (st.offers.length)
        bad.push(`the walk left ${st.offers.length} offers standing`);
    }
  }

  // --- GEOM: what each placement claims about the rows beneath it.
  if (!skip("GEOM")) {
    const g = await ff.eval(() => {
      const w = document.getElementById("dw");
      const box = document.getElementById("mdoc");
      const para = document.querySelector("#mdoc .d-para");
      const r = w ? w.getBoundingClientRect() : null;
      const p = para.getBoundingClientRect();
      const grid = w && w.querySelector(".dw-grid");
      const strip = w && w.querySelector(".dw-strip");
      return {
        has: !!w,
        wTop: r ? Math.round(r.top) : 0, wBottom: r ? Math.round(r.bottom) : 0,
        wLeft: r ? Math.round(r.left) : 0, wRight: r ? Math.round(r.right) : 0,
        pTop: Math.round(p.top), pBottom: Math.round(p.bottom),
        pLeft: Math.round(p.left), pRight: Math.round(p.right),
        cells: grid ? grid.children.length : strip ? strip.children.length : 0,
        body: !!(w && w.querySelector(".dw-body")),
        ghost: !!(w && w.querySelector("#dwghost")),
        inRow: !!(w && w.closest(".de")),
        bodyH: w && w.querySelector(".dw-body")
          ? Math.round(w.querySelector(".dw-body").getBoundingClientRect().height) : 0,
        wH: r ? Math.round(r.height) : 0,
        echo: !!(w && w.querySelector("#dwecho")),
        over: box.scrollWidth - box.clientWidth,
        boxRight: Math.round(box.getBoundingClientRect().right),
      };
    });
    if (!g.has) bad.push("the widget is nowhere in the pane");
    // THE PANE NEVER SCROLLS SIDEWAYS.
    if (g.over > 1) bad.push(`the widget made the pane scroll sideways by ${g.over}px`);
    if (g.wRight > g.boxRight + 1) bad.push("the widget hangs out of the pane");
    const covers = g.wBottom > g.pTop && g.wTop < g.pBottom
                && g.wRight > g.pLeft && g.wLeft < g.pRight;
    // The popup's cost is that it COVERS; the dock's claim is that it does not.
    if (s.mount === "float" && !covers)
      bad.push("the popup covers nothing — then it is not a popup");
    if (s.mount !== "float" && covers)
      bad.push("an inline widget covers the rows under it");
    if (!s.grid) {
      // THE FOOTPRINT: no calendar, and something that speaks.  The doc line is
      // 21px; a SLOT widget is one line because it IS a line the sheet had.
      if (g.body) bad.push("a gridless widget still drew a calendar body");
      if (!g.echo && !g.ghost)
        bad.push("a gridless widget with neither echo nor ghost says nothing at all");
      const ceiling = s.mount === "slot" ? Math.round(1.6 * 21) : 3 * 21;
      if (g.wH > ceiling)
        bad.push(`the compact widget is ${g.wH}px — over ${ceiling}px`);
    }
    // FULLY INLINE: a slot widget stands INSIDE a document row, never beside it.
    if (s.mount === "slot" && !g.inRow)
      bad.push("the slot widget is not inside a row of the document");
    if (s.grid === "month" && g.cells !== 49)
      bad.push(`the month grid drew ${g.cells} cells, wanted 7 heads + 42 days`);
    if (s.grid === "strip") {
      if (g.cells !== 15)
        bad.push(`the strip drew ${g.cells} cells, wanted an edge + 14 days`);
      // ONE ROW TALL is the strip's whole claim; the doc line is 21px.
      if (g.bodyH > 32)
        bad.push(`the strip is ${g.bodyH}px tall — that is not one row`);
    }
  }

  // --- the tops of the rows beneath, before and after: the dock/float split
  //     stated as a number rather than as a look.  MEASURED OVER A SHEET THAT
  //     ALREADY HAS A PLANNING LINE, so what is read is the WIDGET's own
  //     displacement and not the line the summon ghosts in.
  if (!skip("GEOM")) {
    await ff.goto(url(page));
    await summon();
    await intoField();
    await type(today);
    await ff.keys([KEY.Enter]);
    const shut = await paraTop();
    await summon();
    const open = await paraTop();
    if (s.mount === "float" && open !== shut)
      bad.push(`the popup moved the rows beneath by ${open - shut}px`);
    if (s.mount === "dock" && open - shut < 15)
      bad.push(`the dock pushed the rows beneath by only ${open - shut}px`);
    // THE SLOT'S FIRST CLAIM: over a sheet whose planning line already stands,
    // the widget takes the value's own place and the sheet gains NOTHING.
    if (s.mount === "slot" && open !== shut)
      bad.push(`the slot widget moved the rows beneath by ${open - shut}px `
               + "— it stands in the value's place and should cost nothing");
    // …AND ITS SECOND: on a bare sheet the whole cost is the line it ghosts in,
    // which is one doc line and not one more.
    if (s.mount === "slot") {
      await ff.goto(url(page));
      const bare = await paraTop();
      await summon();
      const push = (await paraTop()) - bare;
      if (push < 15 || push > 40)
        bad.push(`the slot summon cost ${push}px on a bare sheet — wanted one doc line`);
    }
  }

  // --- TYPED: the refinement, live, before the commit.
  await ff.goto(url(page));
  await summon();
  await intoField();
  const resolves = async (text) => {
    await type(text);
    const st = await ff.eval(read);
    return { stamp: st.stamp, refused: st.refused, note: st.note };
  };
  // The shipped forms every tab reads.
  const isoWant = iso(addDays(now, 5));
  let r = await resolves(isoWant);
  if (r.stamp !== stamp(addDays(now, 5)))
    bad.push(`${isoWant} resolved to ${JSON.stringify(r.stamp)}`);
  r = await resolves("today");
  if (r.stamp !== stamp(now)) bad.push(`today resolved to ${JSON.stringify(r.stamp)}`);
  r = await resolves("+3d");
  if (r.stamp !== stamp(addDays(now, 3)))
    bad.push(`+3d resolved to ${JSON.stringify(r.stamp)}`);
  r = await resolves("*today*+30d");
  if (r.stamp !== stamp(addDays(now, 30)))
    bad.push(`*today*+30d resolved to ${JSON.stringify(r.stamp)}`);
  // A HALF-TYPED SHIFT NARROWS NOTHING, and says so rather than guessing.
  r = await resolves("today+30");
  if (r.stamp) bad.push(`today+30 resolved to ${r.stamp} — a half-typed shift`);
  if (!r.refused) bad.push("today+30 was neither resolved nor refused");
  // A DAY THAT IS NOT A DAY.
  r = await resolves("2026-02-31");
  if (r.stamp) bad.push(`2026-02-31 resolved to ${r.stamp}`);

  // The English forms — the proposal's, prototyped here.  A DEPARTS: its
  // reader is today's, so these are refused, and the departure is asserted.
  const english = page !== SHIPPED_ONLY;
  const yr = now.y;
  const cases = [
    ["18 aug", stamp({ y: yr, m: 8, d: 18 })],
    ["18 August", stamp({ y: yr, m: 8, d: 18 })],
    ["AUGUST 18", stamp({ y: yr, m: 8, d: 18 })],
    ["18 aug 2029", stamp({ y: 2029, m: 8, d: 18 })],
    ["from 18 to 19 august",
     range({ y: yr, m: 8, d: 18 }, { y: yr, m: 8, d: 19 })],
    ["18 to 19 aug", range({ y: yr, m: 8, d: 18 }, { y: yr, m: 8, d: 19 })],
    ["from 30 aug to 2 sep",
     range({ y: yr, m: 8, d: 30 }, { y: yr, m: 9, d: 2 })],
    // THE DEGENERATE INTERVAL COLLAPSES to the single stamp.
    ["from 18 to 18 aug", stamp({ y: yr, m: 8, d: 18 })],
  ];
  for (const [text, want] of cases) {
    r = await resolves(text);
    if (english && r.stamp !== want)
      bad.push(`${JSON.stringify(text)} resolved to ${JSON.stringify(r.stamp)}, `
               + `wanted ${want}`);
    if (!english && r.stamp)
      bad.push(`A read ${JSON.stringify(text)} — its grammar is today's, `
               + "which has no English in it");
  }
  // The two refusals the grammar owes, SPOKEN before the commit.
  for (const [text, word] of [["31 feb", "calendar"],
                              ["from 30 dec to 2 jan", "ends before"]]) {
    r = await resolves(text);
    if (r.stamp) bad.push(`${JSON.stringify(text)} resolved to ${r.stamp}`);
    if (english && !(r.refused || "").includes(word))
      bad.push(`${JSON.stringify(text)} was refused with `
               + `${JSON.stringify(r.refused)} — no mention of "${word}"`);
  }

  // --- VERBATIM: org's own spelling is KEPT, and its weekday is NOT
  //     recomputed — the one form where the widget must not know better
  //     (`AGENTS.hs:3426'; `test/TestQuery.hs:1791' pins `<2026-08-05 Mon>'
  //     going through unchanged though that day is a Wed).
  r = await resolves("<2026-08-05 Mon>");
  if (r.stamp !== "<2026-08-05 Mon>")
    bad.push(`a bracketed stamp was respelled as ${JSON.stringify(r.stamp)}`);
  if (zeller({ y: 2026, m: 8, d: 5 }) === "Mon")
    bad.push("2026-08-05 is a Mon — pick another verbatim vector");

  // --- ECHO: what the reader can SEE, read off the drawn line rather than off
  //     the model.  An echo one resolve behind its own field is right until the
  //     first keystroke and wrong ever after, and every other rung here would
  //     pass with it standing.
  if (!skip("ECHO")) {
    // WHAT THE READER CAN SEE, whichever surface this variant says it on: the
    // ghost riding the input line, or the echo line under the widget.
    const spoken = async (text) => {
      await type(text);
      return ff.eval(() => {
        const g = document.getElementById("dwghost");
        if (g) return g.textContent;
        const e = document.getElementById("dwecho");
        return e ? e.textContent : "";
      });
    };
    const e1 = await spoken(iso(addDays(now, 11)));
    if (!e1.includes(stamp(addDays(now, 11))))
      bad.push(`the echo said ${JSON.stringify(e1)}, wanted ${stamp(addDays(now, 11))}`);
    const e2 = await spoken(iso(addDays(now, 12)));
    if (!e2.includes(stamp(addDays(now, 12))))
      bad.push(`the echo did not follow the field: ${JSON.stringify(e2)}`);
    if (e2.includes(stamp(addDays(now, 11))))
      bad.push("the echo is one resolve behind the field it speaks for");
    const e3 = await spoken("31 feb");
    if (english && !e3.includes("calendar"))
      bad.push(`the echo swallowed a refusal: ${JSON.stringify(e3)}`);
  }

  // --- CARET: THE GHOST IS NEVER EDITABLE TEXT.  It rides the input's line but
  //     it is not the input's value: the caret cannot enter it, no keystroke
  //     edits it, and `RET' commits the RESOLUTION rather than the characters
  //     that drew it.  Runs where a ghost exists to walk into, which is D.
  if (s.ghost && english) {
    await type("10 jan");
    const held = await ff.eval(() => {
      const f = document.getElementById("dwfield");
      const g = document.getElementById("dwghost");
      return { value: f ? f.value : null, ghost: g ? g.textContent : "" };
    });
    const want = stamp({ y: now.y, m: 1, d: 10 });
    if (held.value !== "10 jan")
      bad.push(`the field holds ${JSON.stringify(held.value)} — the ghost has `
               + "leaked into the field's own value");
    if (!held.ghost.includes(want))
      bad.push(`the ghost said ${JSON.stringify(held.ghost)}, wanted ${want}`);
    // Walk the caret at the end of what was typed and keep pressing.
    await ff.keys(new Array(10).fill(KEY.ArrowRight));
    const at = await ff.eval(() => {
      const f = document.getElementById("dwfield");
      return { start: f.selectionStart, value: f.value };
    });
    if (at.start !== "10 jan".length)
      bad.push(`the caret walked to ${at.start} — past the `
               + `${"10 jan".length} characters actually typed`);
    if (at.value !== "10 jan")
      bad.push("walking the caret changed what the field holds");
    // …and RET commits the RESOLVED stamp, never the typed characters.
    await ff.keys([KEY.Enter]);
    const wrote = await ff.eval(() => RIG.plan());
    if (wrote !== `SCHEDULED: ${want}`)
      bad.push(`RET over a ghosted value wrote ${JSON.stringify(wrote)}`);
    await ff.goto(url(page));
    await summon();
    await intoField();
  }

  // --- QUIET and DRY: offers stand at a fresh and UNFINISHED position and
  //     NOWHERE ELSE; RET over one takes it and applies nothing.
  if (!skip("QUIET")) {
    await type("18 a");
    let st = await ff.eval(read);
    if (!st.offers.length)
      bad.push("no offers over a half-typed day and month");
    if (st.offerAt !== 0)
      bad.push("point does not lead the offers on the reader's own line");
    await type(iso(addDays(now, 2)));
    st = await ff.eval(read);
    if (st.offers.length)
      bad.push(`${st.offers.length} offers over a FINISHED term`);
  }
  if (!skip("DRY")) {
    await type("18 a");
    await ff.keys([KEY.ArrowDown]);
    const planWas = (await ff.eval(read)).plan;
    await ff.keys([KEY.Enter]);
    let st = await ff.eval(read);
    if (!st.open) bad.push("RET over an offer applied the whole edit");
    if (st.plan !== planWas) bad.push("RET over an offer wrote to the sheet");
    if (!/^18 a\w+$/.test(st.text))
      bad.push(`RET over an offer left ${JSON.stringify(st.text)}`);
    if (st.text.endsWith(" ")) bad.push("the accept was not dry — a trailing space");
    if (st.offers.length) bad.push("the accept left its offers standing");
    // …and the SAME key, over the finished term, applies.
    await ff.keys([KEY.Enter]);
    st = await ff.eval(read);
    if (st.open) bad.push("RET over a finished term did not apply");
    if (st.plan === planWas) bad.push("RET over a finished term wrote nothing");
  }

  // --- WEEKDAY: what actually lands on the planning line, checked against a
  //     weekday this file computes by a different algorithm.
  await ff.goto(url(page));
  await summon();
  await intoField();
  const want = addDays(now, 9);
  await type(iso(want));
  await ff.keys([KEY.Enter]);
  let plan = await ff.eval(() => RIG.plan());
  if (plan !== `SCHEDULED: ${stamp(want)}`)
    bad.push(`the commit wrote ${JSON.stringify(plan)}, wanted `
             + `"SCHEDULED: ${stamp(want)}"`);

  // --- the second keyword lands beside the first, org's own order.
  await ff.keys(DEADLINE);
  await intoField();
  const due = addDays(now, 21);
  await type(iso(due));
  await ff.keys([KEY.Enter]);
  plan = await ff.eval(() => RIG.plan());
  if (plan !== `SCHEDULED: ${stamp(want)} DEADLINE: ${stamp(due)}`)
    bad.push(`C-c C-d wrote ${JSON.stringify(plan)}`);

  // --- EMPTY CLEARS IT, the shipped foot's own promise.
  await summon();
  await intoField();
  await ff.keys(CTRL("a"));
  await ff.keys([KEY.Backspace, KEY.Enter]);
  plan = await ff.eval(() => RIG.plan());
  if (plan !== `DEADLINE: ${stamp(due)}`)
    bad.push(`an empty commit left ${JSON.stringify(plan)} — it must clear`);

  // --- PAIR: the same widget on the pair box's VALUE SLOT, where a date is
  //     owed by a property rather than by the planning line.
  await ff.goto(url(page));
  const wasPair = await ff.eval(() => RIG.state().pair);
  if (!(await ontoPair())) bad.push("no date-shaped pair to walk onto");
  await ff.keys([KEY.Enter]);
  let st2 = await ff.eval(read);
  if (!st2.open || st2.where !== "pair")
    bad.push("RET on the drawer's date-shaped pair opened no widget");
  else {
    if (st2.text !== wasPair)
      bad.push("the pair's widget did not open on the value it is replacing");
    await intoField();
    const pd = addDays(now, 4);
    await type(iso(pd));
    await ff.keys([KEY.Enter]);
    const got = await ff.eval(() => RIG.state().pair);
    if (got !== stamp(pd))
      bad.push(`the pair slot took ${JSON.stringify(got)}, wanted ${stamp(pd)}`);
  }

  // --- ENTRY: THE FIRST RET BRINGS THE VALUE UP WHOLLY SELECTED.  Opening an
  //     edit over an entry that already stands must show the whole spelled
  //     value selected, so the reader who types to overwrite overwrites — and
  //     the ghost stays outside it, being a span and never the field's text.
  //     BOTH DOORS ARE WALKED.  `C-c C-s' opens over the planning line, where
  //     point is elsewhere; `RET' opens over the drawer pair, where point is ON
  //     the row the edit stands in — and those are different rows to paint
  //     against, which is what round 4 cost.
  //     READ AT THE CYCLE'S TRUE END, after every redraw the open triggers.
  for (const door of ["plan", "pair"]) {
    await ff.goto(url(page));
    let want, open;
    if (door === "plan") {
      await summon();
      await intoField();
      const held = addDays(now, 15);
      await type(iso(held));
      await ff.keys([KEY.Enter]);
      want = stamp(held);
      if ((await ff.eval(() => RIG.plan())) !== `SCHEDULED: ${want}`)
        bad.push("the ENTRY rung never got a planning value to open over");
      open = async () => { await summon(); await intoField(); };
    } else {
      want = await ff.eval(() => RIG.state().pair);
      if (!/^<\d{4}-\d\d-\d\d \w{3}>$/.test(want))
        bad.push(`the drawer pair holds ${JSON.stringify(want)} — no value to open over`);
      open = async () => {
        await ontoPair();
        await ff.keys([KEY.Enter]);
        await intoField();
      };
    }

    await open();
    await settle();
    let f = await fieldNow();
    const at = `${door} door`;
    if (!f) bad.push(`${at}: the edit opened with no field to select in`);
    else {
      if (f.v !== want)
        bad.push(`${at}: the edit opened holding ${JSON.stringify(f.v)}, not the `
                 + `${want} that stands`);
      if (!(f.a === 0 && f.b === f.v.length && f.v.length))
        bad.push(`${at}: the opened value selects ${f.a}..${f.b} of ${f.v.length} `
                 + "— the FIRST RET must bring it up wholly selected");
      if (f.ghost !== null) {
        // THE GHOST IS OUTSIDE THE SELECTION because it is outside the field.
        if (f.ghostInField)
          bad.push(`${at}: the ghost is inside the field — then it is selectable text`);
        if (f.ghost.trim() && f.v.includes(f.ghost.trim()))
          bad.push(`${at}: the ghost's text is in the field's own value`);
        // …and it says nothing here: the value that stands IS its own
        // resolution, org's own spelling, so there is nothing to add.
        if (f.ghost.trim())
          bad.push(`${at}: the ghost says ${JSON.stringify(f.ghost)} over a value `
                   + "identical to it");
      }
      // ONE KEYSTROKE REPLACES THE WHOLE OF IT.
      await ff.keys(["9"]);
      f = await fieldNow();
      if (f.v !== "9")
        bad.push(`${at}: a keystroke over the opened value left ${JSON.stringify(f.v)}`
                 + " — it must replace the selection, never append to it");
      await ff.keys([KEY.Escape]);
    }

    // …AND RET WITH NO KEYSTROKE RECOMMITS THE SAME VALUE, DRY: the default is
    // taken exactly as it stands, byte for byte.
    if (door === "plan") {
      await summon();
      await ff.keys([KEY.Enter]);
      const again = await ff.eval(() => RIG.plan());
      if (again !== `SCHEDULED: ${want}`)
        bad.push(`${at}: RET with no keystroke rewrote the entry as `
                 + JSON.stringify(again));
    } else {
      await ontoPair();
      await ff.keys([KEY.Enter, KEY.Enter]);
      const again = await ff.eval(() => RIG.state().pair);
      if (again !== want)
        bad.push(`${at}: RET with no keystroke rewrote the pair as `
                 + JSON.stringify(again));
    }
  }

  // --- ESC: ONE press, from over the offers, and the sheet comes back byte
  //     for byte.  The reader's escape is from the EDIT, never from the menu.
  await ff.goto(url(page));
  const clean = await ff.eval(shot);
  if (clean !== before) bad.push("two loads drew two different sheets");
  await summon();
  await intoField();
  await type("18 a");
  const menu = (await ff.eval(read)).offers.length;
  if (!skip("QUIET") && !menu) bad.push("the ESC rung never raised a menu to cancel through");
  await ff.keys([KEY.Escape]);
  const back = await ff.eval(read);
  if (back.open) bad.push("ESC left the edit open — it is the EDIT it cancels");
  const after = await ff.eval(shot);
  if (after !== clean) bad.push("ESC did not give the sheet back byte for byte");
  if (back.plan !== "") bad.push(`ESC left ${JSON.stringify(back.plan)} written`);

  // …and again over a COMMITTED value, where the cancel must put back the
  // spelling the edit FOUND rather than the one it was given.
  await summon();
  await intoField();
  await type(iso(addDays(now, 6)));
  await ff.keys([KEY.Enter]);
  const settled = await ff.eval(shot);
  await summon();
  await intoField();
  await type("18 aug");
  await ff.keys([KEY.Escape]);
  if ((await ff.eval(shot)) !== settled)
    bad.push("ESC over a standing value did not restore what the edit found");

  if (bad.length) {
    console.log(`FAIL ${page}\n  ${bad.join("\n  ")}`);
    process.exitCode = 1;
  } else {
    console.log(`ok   ${page}`
                + (misses.length ? `   (misses: ${misses.join(", ")})` : ""));
  }
}

// ============================================================ does it bite?
// Each fault, against the rung that owes it.  A PASS here is the failure.
const BITES = [
  {
    bug: "skip", page: "c-month-inline.html", rung: "WALK",
    why: "a day-walk that skips",
    run: async () => {
      await summon();
      await ff.keys(["n"]);
      const s = await ff.eval(read);
      const n = new Date();
      const t = { y: n.getFullYear(), m: n.getMonth() + 1, d: n.getDate() };
      return s.at === iso(addDays(t, 1));
    },
  },
  {
    bug: "weekday", page: "e-day-strip.html", rung: "WEEKDAY",
    why: "a weekday remembered rather than computed",
    run: async () => {
      const n = new Date();
      const t = { y: n.getFullYear(), m: n.getMonth() + 1, d: n.getDate() };
      const w = addDays(t, 9);
      await summon();
      await intoField();
      await type(iso(w));
      await ff.keys([KEY.Enter]);
      return (await ff.eval(() => RIG.plan())) === `SCHEDULED: ${stamp(w)}`;
    },
  },
  {
    bug: "stale", page: "d-text-first.html", rung: "ECHO",
    why: "an echo one resolve behind the field it speaks for",
    run: async () => {
      const n = new Date();
      const t = { y: n.getFullYear(), m: n.getMonth() + 1, d: n.getDate() };
      await summon();
      await intoField();
      await type(iso(addDays(t, 11)));
      await type(iso(addDays(t, 12)));
      const said = await ff.eval(() => {
        const e = document.getElementById("dwecho");
        return e ? e.textContent : "";
      });
      return said.includes(stamp(addDays(t, 12)))
          && !said.includes(stamp(addDays(t, 11)));
    },
  },
  {
    bug: "caret", page: "d-text-first.html", rung: "CARET",
    why: "a ghost written into the field, where the caret can walk into it",
    run: async () => {
      await summon();
      await intoField();
      await type("10 jan");
      await ff.keys(new Array(10).fill(KEY.ArrowRight));
      const at = await ff.eval(() => {
        const f = document.getElementById("dwfield");
        return { start: f.selectionStart, value: f.value };
      });
      return at.value === "10 jan" && at.start === "10 jan".length;
    },
  },
  {
    bug: "opencaret", page: "d-text-first.html", rung: "ENTRY",
    why: "an edit that opens with the caret collapsed instead of the value selected",
    run: async () => {
      const n = new Date();
      const t = { y: n.getFullYear(), m: n.getMonth() + 1, d: n.getDate() };
      await summon();
      await intoField();
      await type(iso(addDays(t, 15)));
      await ff.keys([KEY.Enter]);
      await summon();
      await intoField();
      const f = await fieldNow();
      if (!f || !(f.a === 0 && f.b === f.v.length && f.v.length)) return false;
      await ff.keys(["9"]);
      return (await fieldNow()).v === "9";
    },
  },
  {
    bug: "escmenu", page: "b-month-popup.html", rung: "ESC",
    why: "an ESC that takes the menu instead of the edit",
    run: async () => {
      const clean = await ff.eval(shot);
      await summon();
      await intoField();
      await type("18 a");
      await ff.keys([KEY.Escape]);
      const s = await ff.eval(read);
      return !s.open && (await ff.eval(shot)) === clean;
    },
  },
];

if (!process.argv[2]) {
  console.log("");
  for (const b of BITES) {
    await ff.goto(url(b.page, b.bug));
    let passed = false;
    try { passed = await b.run(); } catch (e) { passed = false; }
    if (passed) {
      console.log(`BLUNT ${b.rung} · ${b.why} — ${b.page}?bug=${b.bug} still passes`);
      process.exitCode = 1;
    } else {
      console.log(`bites ${b.rung} · ${b.why}`);
    }
  }
}

await ff.close();

// ================================================ the paint, in a second engine
// A SELECTION IS SOMETHING THE READER SEES.  Round 4 was set, focused, and
// INVISIBLE — the edit stood inside the cursor row, whose wash is `--g-sel',
// and painted its text selection in `--g-sel' over it.  Every rung above passed
// while the screen showed nothing, because every rung above reads the MODEL.
// So the last pass reads PIXELS, and bytes do not lie.
//
// IT RUNS UNDER CHROMIUM ON PURPOSE, and that is the round's other lesson.
// Headless Firefox never gives the document focus — `document.hasFocus()' is
// false and stays false through `window.focus()' — and NO engine paints a text
// selection in an unfocused document.  The driver every rung above uses is
// therefore blind to this whole class of fault by construction.  Chromium's
// `Emulation.setFocusEmulationEnabled' gives the page the focus a reader's
// browser has, which is the only way a headless shot can be evidence at all.
{
  const cr = await chromium().catch((e) => {
    console.log("\nSKIP  the paint pass — no chromium: " + e.message);
    return null;
  });
  if (cr) {
    const tmp = join(HERE, ".paint.png");
    const intoFieldC = async () => {
      if (!(await cr.eval(() => RIG.state().inField))) await cr.keys(["/"]);
    };
    const openDoor = async (door) => {
      if (door === "plan") await cr.keys([{ down: "Ctrl" }, "c", "s", { up: "Ctrl" }]);
      else {
        for (let i = 0; i < 6; i += 1) {
          if ((await cr.eval(() => RIG.state().atRow)) === "pair") break;
          await cr.keys(["n"]);
        }
        await cr.keys([CK.Enter]);
      }
      await intoFieldC();
      await cr.settle();
    };
    /** The just-opened frame: the field's box, the wash the palette spells, and
     * the pixels inside that box. */
    const frame = async (page, theme, door, bug) => {
      await cr.goto(url(page, bug));
      await cr.eval((t) => { document.documentElement.dataset.theme = t; }, theme);
      await openDoor(door);
      const seen = await cr.eval(() => {
        const f = document.getElementById("dwfield") || document.getElementById("pinput");
        if (!f) return null;
        const r = f.getBoundingClientRect(), cs = getComputedStyle(f);
        // THE BOX IS THE TEXT'S, NEVER THE FIELD'S.  A field is as wide as its
        // flex lets it; the selection is as wide as the VALUE, so a threshold
        // read over the field's own box would call a correct wash a failure on
        // every wide field and pass a wrong one on every narrow.  Measured with
        // the field's own computed font, which is the pane's doc line.
        const probe = document.createElement("span");
        probe.style.cssText = "position:absolute;visibility:hidden;white-space:pre";
        probe.style.font = cs.font;
        probe.style.letterSpacing = cs.letterSpacing;
        probe.textContent = f.value;
        document.body.appendChild(probe);
        const tw = probe.getBoundingClientRect().width;
        probe.remove();
        const left = r.x + parseFloat(cs.paddingLeft || 0) + parseFloat(cs.borderLeftWidth || 0);
        return { box: { x: Math.round(left), y: Math.round(r.y),
                        w: Math.max(0, Math.min(Math.round(tw), Math.round(r.width))),
                        h: Math.round(r.height) },
                 sel: [f.selectionStart, f.selectionEnd], n: f.value.length,
                 focused: document.hasFocus() && document.activeElement === f,
                 wash: getComputedStyle(document.documentElement)
                   .getPropertyValue("--g-sel").trim().toLowerCase() };
      });
      await cr.shot(tmp);
      return seen ? { ...seen, px: pixels(await readFile(tmp)) } : null;
    };

    let paintBad = 0;
    for (const page of picked) {
      const bad2 = [], empty = new Set();
      for (const theme of ["light", "dark"]) {
        for (const door of ["plan", "pair"]) {
          const at = `${theme}/${door}`;
          const on = await frame(page, theme, door, "");
          if (!on) { bad2.push(`${at}: nothing opened to look at`); continue; }
          // A PAINT RUNG ON AN UNFOCUSED PAGE PROVES NOTHING: say so rather than
          // going green on a document that cannot paint a selection at all.
          if (!on.focused)
            bad2.push(`${at}: the page is not focused — this rung would be blind`);
          // A DOOR THAT OPENS EMPTY HAS NOTHING TO SELECT.  A's blind prompt is
          // the one, and the ENTRY law already exempts it: declared, not failed.
          if (on.n === 0) { empty.add(`${at}`); continue; }
          if (!(on.sel[0] === 0 && on.sel[1] === on.n))
            bad2.push(`${at}: opened selecting ${on.sel[0]}..${on.sel[1]} of ${on.n}`);
          const off = await frame(page, theme, door, "opencaret");
          const b = on.box, area = b.w * b.h;
          if (area < 200) { bad2.push(`${at}: the value's box is ${b.w}x${b.h}`); continue; }
          let diff = 0;
          for (let y = b.y; y < b.y + b.h; y += 1)
            for (let x = b.x; x < b.x + b.w; x += 1)
              if (on.px.at(x, y) !== (off ? off.px.at(x, y) : null)) diff += 1;
          const wash = on.px.count(b, on.wash);
          // THE OPEN MUST LOOK OPEN: the selected frame and the collapsed-caret
          // frame differ across the value, and the difference is the palette's
          // own selection wash rather than some accident of antialiasing.
          if (diff < area * 0.25)
            bad2.push(`${at}: the selected frame differs from the collapsed one by `
                      + `${diff}/${area}px — the selection is set but not SEEN`);
          if (wash < area * 0.2 && !NO_WASH[page])
            bad2.push(`${at}: only ${wash}/${area}px of the field wear ${on.wash} `
                      + "— the value does not visibly carry the selection wash");
        }
      }
      if (bad2.length) {
        paintBad += 1;
        console.log(`FAIL ${page} · paint\n  ${bad2.join("\n  ")}`);
        process.exitCode = 1;
      } else console.log(`ok   ${page} · paint (chromium, both themes, both doors)`
                         + (empty.size ? `   (opens empty: ${[...empty].join(", ")})` : "")
                         + (NO_WASH[page] ? `\n     departs: ${NO_WASH[page]}` : ""));
    }

    // …AND THE FAULT THAT IS ROUND 4 ITSELF: the edit keeps no ground, so its
    // selection is painted in the wash already behind it.
    const blend = await frame("d-text-first.html", "light", "pair", "blend");
    const blendOff = await frame("d-text-first.html", "light", "pair", "blend,opencaret");
    let bdiff = 0;
    if (blend) {
      const b = blend.box;
      for (let y = b.y; y < b.y + b.h; y += 1)
        for (let x = b.x; x < b.x + b.w; x += 1)
          if (blend.px.at(x, y) !== (blendOff ? blendOff.px.at(x, y) : null)) bdiff += 1;
      const area = b.w * b.h;
      if (bdiff >= area * 0.25) {
        console.log("BLUNT PAINT · an edit whose selection is painted in the ground "
                    + "already behind it still passes");
        process.exitCode = 1;
      } else {
        console.log("bites PAINT · an edit whose selection is painted in the ground "
                    + `already behind it (${bdiff}px of ${area} differ)`);
      }
    }
    await rm(tmp, { force: true }).catch(() => {});
    await cr.close();
  }
}
