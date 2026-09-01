// One PNG per variant, headless, each driven to the moment that shows what the
// tab is FOR.  The README's table reads off these.
//
//   node shots.mjs                      # all five
//   node shots.mjs c-month-inline.html  # one, when only that moment changed
import { firefox, KEY } from "./bidi.mjs";
import { chromium, KEY as CK } from "./cdp.mjs";
import { pathToFileURL, fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const HERE = dirname(fileURLToPath(import.meta.url));
const chars = (s) => [...s];
const CTRL = (k) => [{ down: KEY.Control }, k, { up: KEY.Control }];
const SCHEDULE = [{ down: KEY.Control }, "c", "s", { up: KEY.Control }];

// A IS SHOT BLIND, which is the whole of its argument: the prompt raised over a
// veiled sheet with `18 aug' typed into it and NOTHING anywhere saying what
// that will become — or that today's grammar will refuse it.
const BLIND = [SCHEDULE, chars("18 aug")];
// B IS SHOT ON ITS MENU: the field opened by `/', a day and a month half typed,
// and the offers hanging under it with each one RESOLVED in the hint column —
// over the rows the popup is covering.
const MENU = [SCHEDULE, ["/"], chars("18 a")];
// C, D AND E ARE SHOT ON A RANGE, which is the frame that shows the most: the
// interval washed across the grid or the strip, and the echo carrying org's own
// `--' pair with the weekday computed at both ends.
const RANGE = [SCHEDULE, ["/"], chars("from 18 to 19 aug")];
// D IS SHOT ON THE USER'S OWN EXAMPLE — `10 jan [-> <2026-01-10 Sat>]' — with
// the widget standing IN the planning line's value slot and the resolution
// riding after what was typed as mute, uneditable ghost.  One line, and it is a
// line the sheet already had.  Its field is primary, so it needs no door; `/'
// there would be a character in the middle of the date.  Nothing clears the
// field first in any of these: the opening value comes up SELECTED, so the
// first character typed replaces it.
const GHOST_D = [SCHEDULE, chars("10 jan")];

const ALL = [
  ["a-control.html", "a-control.png", BLIND],
  ["b-month-popup.html", "b-month-popup.png", MENU],
  ["c-month-inline.html", "c-month-inline.png", RANGE],
  ["d-text-first.html", "d-text-first.png", GHOST_D],
  ["e-day-strip.html", "e-day-strip.png", RANGE],
];
const SHOTS = process.argv[2] ? ALL.filter((s) => s[0] === process.argv[2]) : ALL;

const ff = await firefox().catch((e) => {
  console.error("no firefox: " + e.message);
  process.exit(2);
});

for (const [page, png, script] of SHOTS) {
  await ff.goto(pathToFileURL(join(HERE, page)).href);
  for (const step of script) await ff.keys(step);
  await ff.shot(join(HERE, png));
  console.log("shot " + png);
}

await ff.close();

// …AND ONE UNDER CHROMIUM, which is the only engine here that can SHOW a
// selection: headless Firefox never gives the document focus, and no engine
// paints a selection in an unfocused document (README round 4).  This is the
// just-opened moment on the property row — the edit standing in the value's own
// slot, wearing the pane's edit ground, with the value it is about to replace
// WHOLLY SELECTED in the pane's own `--g-sel'.
if (!process.argv[2] || process.argv[2] === "d-text-first.html") {
  const cr = await chromium().catch(() => null);
  if (cr) {
    await cr.goto(pathToFileURL(join(HERE, "d-text-first.html")).href);
    await cr.eval(() => { document.documentElement.dataset.theme = "light"; });
    for (let i = 0; i < 6; i += 1) {
      if ((await cr.eval(() => RIG.state().atRow)) === "pair") break;
      await cr.keys(["n"]);
    }
    await cr.keys([CK.Enter]);
    await cr.settle();
    await cr.shot(join(HERE, "d-selected.png"));
    console.log("shot d-selected.png  (chromium, the just-opened moment)");
    await cr.close();
  } else console.log("skip d-selected.png — no chromium");
}
