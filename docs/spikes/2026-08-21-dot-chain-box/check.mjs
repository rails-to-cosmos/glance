// The complaint, mechanised: "." must be a CALL and not a character.  A dot
// spawns, three functions are offered, the taken one opens its parens and the
// caret lands INSIDE them, a comma separates arguments the way a call's
// arguments separate, an accept lands DRY and closes the offers, a scripted
// chain composes exactly the flat string the grammar spells, and the ESC ladder
// keeps its three rungs.
//
// `/' was the spike's control — byte for byte the same door in every tab — and
// D has since been given the user's own answer, where `/' is the filter STAGE's
// edit key and `DEL' its eraser.  That departure is DECLARED here rather than
// dropped, so the run still says what the four other tabs owe each other.
//
//   node check.mjs                     # every variant
//   node check.mjs c-ide-chain.html    # one
import { firefox, KEY } from "./bidi.mjs";
import { pathToFileURL, fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const HERE = dirname(fileURLToPath(import.meta.url));
const ALL = ["a-control.html", "b-plain-chain.html", "c-ide-chain.html",
             "d-stage-pills.html", "e-echo-line.html"];
const picked = process.argv[2] ? [process.argv[2]] : ALL;

// THE CONTROL MISSES BY CONSTRUCTION, the way headline-bars' `flat' tab does:
// A opens the flat box, where a dot is a character, so it cannot spawn one,
// cannot land a caret in parens, composes no chain, and has no parens for a
// comma or an accept to land in.  Declared, so the run is green and the misses
// are the argument rather than a broken tab.
const MISSES = { "a-control.html": ["DOT", "PARENS", "CHAIN", "COMMA", "DRY"] };
// D ANSWERS THE TWO KEYS DIFFERENTLY, on purpose: `/' edits the filter stage
// and `DEL' takes it whole, so the flat-door rungs are not its to pass.
const DEPARTS = { "d-stage-pills.html": ["SLASH", "SIG"] };

const BOOT_CHIPS = ["state:*active*", "-tag:chore"];
const BOOT_FILTER = "state:*active* -tag:chore";
const WANT_CHAIN = "state:TODO sort:deadline";
const WANT_QUERY = "state:*active* -tag:chore state:TODO sort:deadline";

/** Everything on the screen that carries an answer; the caret's blink does not. */
const picture = () => {
  const bits = [];
  for (const c of document.querySelectorAll("#app .tv-chips > *"))
    bits.push("chip/" + c.className + "/" + c.textContent);
  for (const s of document.querySelectorAll("#app .cx > *"))
    bits.push("cx/" + s.className + "/" + s.textContent);
  for (const tr of document.querySelectorAll("#app tbody tr"))
    bits.push("row/" + tr.className + "/" + tr.textContent);
  const h = document.querySelector("#app .tv-hint");
  bits.push("hint/" + (h ? h.textContent : ""));
  const e = document.getElementById("echo");
  if (e) bits.push("echo/" + e.textContent);
  return bits.join(";");
};

/** The caret between the two parens, AFTER the arguments — in DOM order and on
 *  the screen.  "The caret lands inside the parens" is the whole claim of the
 *  completed-call look, so it is read off the drawing and not off the model. */
const caretInParens = () => {
  const st = document.querySelector("#app .cx .cx-live");
  if (!st) return { ok: false, why: "no live stage" };
  const kids = [...st.children];
  const caret = st.querySelector(".cx-caret");
  const pars = kids.filter((k) => k.classList.contains("cx-par"));
  const args = st.querySelector(".cx-args");
  if (!caret) return { ok: false, why: "no caret" };
  if (pars.length !== 2) return { ok: false, why: `${pars.length} parens` };
  const i = kids.indexOf(caret), o = kids.indexOf(pars[0]), c = kids.indexOf(pars[1]);
  if (!(o < i && i < c)) return { ok: false, why: "caret is not between the parens" };
  if (args && !(kids.indexOf(args) < i))
    return { ok: false, why: "caret is not at the end of the contents" };
  const rc = caret.getBoundingClientRect();
  const ro = pars[0].getBoundingClientRect(), rk = pars[1].getBoundingClientRect();
  if (!(ro.right <= rc.left + 1.5 && rc.right <= rk.left + 1.5))
    return { ok: false, why: "caret is drawn outside the parens" };
  return { ok: true, where: RIG.cx().where, fn: RIG.cx().stages.slice(-1)[0].fn,
           args: args ? args.textContent : "" };
};

const eq = (a, b) => JSON.stringify(a) === JSON.stringify(b);
const chars = (s) => [...s];
// The copied driver's key map carries the rungs the fold-marks spike needed and
// no DELETE; the WebDriver codepoint is spelled here rather than in the driver,
// which stays the copy it is.
const DEL = "";

let failed = 0;
const sigs = {};
const ff = await firefox().catch((e) => {
  console.error("no firefox: " + e.message);
  process.exit(2);
});

for (const page of picked) {
  const bad = [], surprised = [];
  const known = MISSES[page] || [];
  const departs = DEPARTS[page] || [];
  const url = pathToFileURL(join(HERE, page)).href;
  /** A rung: green when it holds, silent when the control is declared to miss. */
  const want = (name, ok, why) => {
    if (ok && known.includes(name)) surprised.push(`${name} passed where the control is declared to miss`);
    else if (!ok && !known.includes(name)) bad.push(`${name}: ${why}`);
  };

  // ---- BOOT: the strip carries the query, the table serves what it asks for
  await ff.goto(url);
  const boot = await ff.eval(() => ({ chips: RIG.chips(), rows: RIG.rows(),
                                      q: RIG.query(), cols: RIG.cols() }));
  want("BOOT", eq(boot.chips, BOOT_CHIPS) && boot.rows === 4,
       `${JSON.stringify(boot.chips)} / ${boot.rows} rows`);

  // ---- DOT: "." spawns a dot and offers exactly the three calls
  await ff.keys(["."]);
  const dot = await ff.eval(() => ({
    door: RIG.door(),
    dots: document.querySelectorAll("#app .cx .cx-dot").length,
    menu: RIG.menu(),
  }));
  want("DOT", dot.door === "compose" && dot.dots === 1
       && eq(dot.menu.items, ["filter", "sort", "columns"]),
       `door=${dot.door} dots=${dot.dots} offers=${JSON.stringify(dot.menu.items)}`);

  // ---- PARENS: the taken call opens them and the caret lands inside
  await ff.keys([KEY.Tab]);
  const par = await ff.eval(caretInParens);
  want("PARENS", par.ok === true && par.where === "args" && par.fn === "filter",
       par.why || `where=${par.where} fn=${par.fn}`);

  // ---- CHAIN: the scripted sequence composes the grammar's own flat string
  await ff.goto(url);
  await ff.keys(["."]);
  await ff.keys([KEY.Tab]);
  await ff.keys(chars("state:TODO"));
  await ff.keys([")", ".", "s"]);
  await ff.keys([KEY.Tab]);
  await ff.keys(chars("deadline"));
  await ff.keys([")"]);
  const chain = await ff.eval(() => ({ composed: RIG.composed(), cx: RIG.cx() }));
  await ff.keys([KEY.Enter]);
  const after = await ff.eval(() => ({ q: RIG.query(), rows: RIG.rows(),
                                       door: RIG.door(), first: RIG.served(RIG.query()).rows
                                         .map((r) => r.title) }));
  want("CHAIN", chain.composed === WANT_CHAIN && after.q === WANT_QUERY
       && after.rows === 2 && after.door === null
       && eq(after.first, ["Ship the dot chain", "Write the release notes"]),
       `composed=${JSON.stringify(chain.composed)} applied=${JSON.stringify(after.q)} `
       + `rows=${after.rows} order=${JSON.stringify(after.first)}`);

  // ---- COMMA: the argument separator, per stage.  The law first — the same
  // arguments spelled three ways compose one string — then one drive through
  // the keys, since a law nothing types is a law about nothing.
  await ff.goto(url);
  const law = await ff.eval(() => ({
    f_comma_space: RIG.stageString("filter", "state:TODO, tag:web"),
    f_comma: RIG.stageString("filter", "state:TODO,tag:web"),
    f_space: RIG.stageString("filter", "state:TODO tag:web"),
    f_value: RIG.stageString("filter", "tag:a,b"),
    f_quoted: RIG.stageString("filter", 'title:"a, b" tag:web'),
    f_signed: RIG.stageString("filter", "state:TODO,+priority:[#B],-tag:chore"),
    s_comma_space: RIG.stageString("sort", "state, title"),
    s_comma: RIG.stageString("sort", "state,title"),
    s_arrow: RIG.stageString("sort", "state->title"),
    s_dir: RIG.stageString("sort", "deadline:desc, title"),
    c_comma_space: RIG.stageString("columns", "State, Deadline"),
    c_comma: RIG.stageString("columns", "State,Deadline"),
  }));
  const WANT_LAW = {
    f_comma_space: "state:TODO tag:web", f_comma: "state:TODO tag:web",
    f_space: "state:TODO tag:web", f_value: "tag:a,b",
    f_quoted: 'title:"a, b" tag:web',
    f_signed: "state:TODO +priority:[#B] -tag:chore",
    s_comma_space: "sort:state->title", s_comma: "sort:state->title",
    s_arrow: "sort:state->title", s_dir: "sort:deadline:desc->title",
    c_comma_space: "columns:State,Deadline", c_comma: "columns:State,Deadline",
  };
  const lawOff = Object.keys(WANT_LAW).filter((k) => law[k] !== WANT_LAW[k]);
  await ff.keys(["."]);
  await ff.keys([KEY.Tab]);
  await ff.keys(chars("state:TODO, tag:web"));
  await ff.keys([")", ".", "s"]);
  await ff.keys([KEY.Tab]);
  await ff.keys(chars("state, title"));
  await ff.keys([")"]);
  const typed = await ff.eval(() => RIG.composed());
  want("COMMA", lawOff.length === 0 && typed === "state:TODO tag:web sort:state->title",
       (lawOff.length ? lawOff.map((k) => `${k}=${JSON.stringify(law[k])}`).join(" ") + " · " : "")
       + `typed=${JSON.stringify(typed)}`);

  // ---- DRY: an accept inside the parens lands bare and closes the offers
  await ff.goto(url);
  await ff.keys(["."]);
  await ff.keys([KEY.Tab]);
  await ff.keys(chars("sta"));
  await ff.keys([KEY.Tab]);
  const liveArgs = () => {
    const s = RIG.cx().stages.slice(-1)[0];
    return { args: s ? s.args : null, menu: RIG.menu().open, door: RIG.door() };
  };
  const dry1 = await ff.eval(liveArgs);
  await ff.keys(chars("TO"));
  const woke = await ff.eval(() => RIG.menu().open);
  await ff.keys([KEY.Enter]);
  const dry2 = await ff.eval(liveArgs);
  want("DRY", dry1.args === "state:" && dry1.menu === false && woke === true
       && dry2.args === "state:TODO" && dry2.menu === false && dry2.door === "compose",
       `key=${JSON.stringify(dry1.args)}/${dry1.menu} woke=${woke} `
       + `value=${JSON.stringify(dry2.args)}/${dry2.menu} door=${dry2.door}`);

  // ---- ESC: three rungs — the offers, what is half-written, the box
  await ff.goto(url);
  await ff.keys([".", "s"]);
  const rung0 = await ff.eval(() => ({ menu: RIG.menu().open, door: RIG.door() }));
  await ff.keys([KEY.Escape]);
  const rung1 = await ff.eval(() => ({ menu: RIG.menu().open, door: RIG.door(),
                                       held: RIG.door() === "compose" ? RIG.cx().stages.length
                                         : document.querySelector("#app .tv-filter").value.length }));
  await ff.keys([KEY.Escape]);
  const rung2 = await ff.eval(() => ({ door: RIG.door(),
                                       held: RIG.door() === "compose" ? RIG.cx().stages.length
                                         : document.querySelector("#app .tv-filter").value.length }));
  await ff.keys([KEY.Escape]);
  const rung3 = await ff.eval(() => ({ door: RIG.door(), chips: RIG.chips() }));
  want("ESC", rung0.menu === true && rung1.menu === false && rung1.door !== null
       && rung1.held > 0 && rung2.door !== null && rung2.held === 0
       && rung3.door === null && eq(rung3.chips, BOOT_CHIPS),
       `offers=${rung0.menu}→${rung1.menu} held=${rung1.held}→${rung2.held} `
       + `door=${rung2.door}→${rung3.door} chips=${JSON.stringify(rung3.chips)}`);

  if (!departs.includes("SLASH")) {
    // ---- SLASH: the same door in every tab, and it still refuses the shaping half
    await ff.goto(url);
    await ff.keys(["/"]);
    await ff.keys(chars("sta"));
    sigs[page] = await ff.eval(() => {
      const b = document.querySelector("#app .tv-filter");
      return JSON.stringify({ tag: b.tagName, cls: b.className, type: b.type,
                              ph: b.placeholder, shown: getComputedStyle(b).display,
                              items: RIG.menu().items });
    });
    await ff.goto(url);
    await ff.keys(["/"]);
    await ff.keys(chars("sort:title"));
    await ff.keys([KEY.Enter]);
    const ref = await ff.eval(() => ({
      said: RIG.refused(), left: document.querySelector("#app .tv-filter").value,
      chips: RIG.chips(),
    }));
    want("SLASH", /^sort: autocomplete restricted/.test(ref.said)
         && /compose/.test(ref.said) && ref.left === "sort:title"
         && eq(ref.chips, BOOT_CHIPS),
         `said=${JSON.stringify(ref.said)} left=${JSON.stringify(ref.left)}`);
  } else {
    // ---- SLASH-STAGE: `/' IS the filter stage.  It reopens the standing pill
    // with the caret at the end of its contents, the commit rewrites THAT stage
    // rather than adding a second one, and the other badges are not its business.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys(["s"]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars("deadline"));
    await ff.keys([")", KEY.Enter]);
    await ff.keys(["/"]);
    const open = await ff.eval(caretInParens);
    const opened = await ff.eval(() => ({ door: RIG.door(), cx: RIG.cx(),
                                          pills: RIG.pills(), q: RIG.query() }));
    await ff.keys(chars(" +tag:docs"));
    await ff.keys([")", KEY.Enter]);
    const rewrote = await ff.eval(() => ({ pills: RIG.pills(), q: RIG.query() }));
    want("SLASH-STAGE",
         open.ok === true && open.args === BOOT_FILTER && open.fn === "filter"
         && opened.cx.stages.length === 1 && opened.q.startsWith(BOOT_FILTER)
         && eq(rewrote.pills, ["filter(state:*active* -tag:chore +tag:docs)",
                               "sort(deadline)"]),
         open.why || `opened=${JSON.stringify(open.args)} stages=${opened.cx.stages.length} `
         + `pills=${JSON.stringify(rewrote.pills)}`);

    // …and with no filter stage standing it spawns exactly one fresh one.  The
    // way to none is the edit itself: reopen, empty, close, commit.
    await ff.keys(["/"]);
    const held = await ff.eval(() => {
      const s = RIG.cx().stages.slice(-1)[0];
      return s && s.args ? s.args.length : 0;
    });
    await ff.keys(new Array(held).fill(KEY.Backspace));
    await ff.keys([")", KEY.Enter]);
    const gone = await ff.eval(() => ({ pills: RIG.pills(), q: RIG.query() }));
    await ff.keys(["/"]);
    const fresh = await ff.eval(() => ({ cx: RIG.cx(), door: RIG.door() }));
    want("SLASH-FRESH",
         eq(gone.pills, ["sort(deadline)"]) && fresh.door === "compose"
         && fresh.cx.stages.length === 1 && fresh.cx.stages[0].fn === "filter"
         && fresh.cx.stages[0].args === "" && fresh.cx.where === "args",
         `gone=${JSON.stringify(gone.pills)} fresh=${JSON.stringify(fresh.cx)}`);

    // ---- DEL-STAGE: the chain's own backspace — the latest badge, whole, and
    // pressing it again walks the chain backward.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys(["s"]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars("deadline"));
    await ff.keys([")", "."]);
    await ff.keys(["c"]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars("State,Deadline"));
    await ff.keys([")", KEY.Enter]);
    const built = await ff.eval(() => ({ pills: RIG.pills(), q: RIG.query() }));
    const walk = [];
    for (let i = 0; i < 4; i += 1) {
      await ff.keys([DEL]);
      walk.push(await ff.eval(() => RIG.query()));
    }
    want("DEL-STAGE",
         eq(built.pills, ["filter(state:*active* -tag:chore)", "sort(deadline)",
                          "columns(State,Deadline)"])
         && eq(walk, ["state:*active* -tag:chore sort:deadline",
                      "state:*active* -tag:chore", "", ""]),
         `built=${JSON.stringify(built.pills)} walk=${JSON.stringify(walk)}`);

    // …and inside an open paren edit it is ordinary text editing, eating nothing.
    await ff.goto(url);
    await ff.keys(["/"]);
    await ff.keys([DEL]);
    const inside = await ff.eval(() => ({ cx: RIG.cx(), q: RIG.query() }));
    want("DEL-INSIDE",
         inside.cx.stages.length === 1 && inside.cx.stages[0].args === BOOT_FILTER
         && inside.q === BOOT_FILTER,
         `cx=${JSON.stringify(inside.cx)} q=${JSON.stringify(inside.q)}`);
  }

  // ---- SETTLED: a repaint that changes nothing changes nothing
  await ff.goto(url);
  await ff.keys(["."]);
  const a = await ff.eval(picture);
  await ff.eval(() => RIG.repaint());
  const b = await ff.eval(picture);
  want("SETTLED", a === b, "a repaint moved the picture");

  const said = [known.length ? `${known.length} declared misses: ${known.join(", ")}` : "",
                departs.length ? `departs: ${departs.join(", ")}` : ""].filter(Boolean);
  if (bad.length || surprised.length) {
    failed += 1;
    console.log(`FAIL ${page}\n  ${bad.concat(surprised).join("\n  ")}`);
  } else {
    console.log(`ok   ${page}` + (said.length ? `  (${said.join(" · ")})` : ""));
  }
}

// THE CONTROL OF THE SPIKE: `/' has to be the same door wherever `.' goes — in
// every tab that still HAS a flat door.  D's departure is the user's own answer
// and is declared above; the four that keep the door still owe each other one.
const held = picked.filter((p) => sigs[p] !== undefined);
if (held.length > 1) {
  const one = sigs[held[0]];
  const off = held.filter((p) => sigs[p] !== one);
  if (off.length) {
    failed += 1;
    console.log(`FAIL /  the filter door differs in: ${off.join(", ")}`);
  } else {
    console.log(`ok   /  the same door in all ${held.length} tabs that keep one`);
  }
}

await ff.close();
process.exit(failed ? 1 : 0);
