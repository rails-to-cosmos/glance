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
             "d-stage-pills.html", "e-echo-line.html", "f-typed-dsl.html"];
const picked = process.argv[2] ? [process.argv[2]] : ALL;
const TYPED = "f-typed-dsl.html";

// THE CONTROL MISSES BY CONSTRUCTION, the way headline-bars' `flat' tab does:
// A opens the flat box, where a dot is a character, so it cannot spawn one,
// cannot land a caret in parens, composes no chain, and has no parens for a
// comma or an accept to land in.  Declared, so the run is green and the misses
// are the argument rather than a broken tab.
const MISSES = { "a-control.html": ["DOT", "PARENS", "CHAIN", "COMMA", "DRY"] };
// D ANSWERS THE TWO KEYS DIFFERENTLY, on purpose: `/' edits the filter stage
// and `DEL' takes it whole, so the flat-door rungs are not its to pass.  F is
// D's machinery with a TYPED surface, so it departs the same way — and its
// arguments are Haskell, which is why the rungs below are spelled per dialect
// rather than once.
const DEPARTS = { "d-stage-pills.html": ["SLASH", "SIG"],
                  "f-typed-dsl.html": ["SLASH", "SIG"] };

const BOOT_CHIPS = ["state:*active*", "-tag:chore"];
const BOOT_FILTER = "state:*active* -tag:chore";
const WANT_CHAIN = "state:TODO sort:deadline";
const WANT_QUERY = "state:*active* -tag:chore state:TODO sort:deadline";

/**
 * THE CORPUS THE NORMAL FORM IS PROVED ON.
 *
 * `same' pairs a flat spelling with F's typed one: two different readers, and
 * the IR they print has to be the same bytes.  `trip' runs the other way — the
 * flat string rendered INTO the surface and read back, which is the path the
 * `/'-edit takes, `raw "…"' and all.  `diff' is the check biting the other way:
 * queries whose semantics part have to print IRs that part with them, or the
 * form is quotienting away something real.
 */
const IR_CORPUS = {
  same: [
    // the README's own examples, and the laws under them
    ["state:*active* -tag:chore", '.filter(state = Active, tag /= "chore")'],
    ["state:TODO sort:deadline",
     '.filter(state = "TODO").sort(columns = ["Deadline"])'],
    // law 1: grouping is by key, never adjacency — order carries nothing
    ["priority:[#A] tag:book", '.filter(tag = "book", priority = "A")'],
    ["tag:book priority:[#A]", '.filter(priority = "A", tag = "book")'],
    // org's brackets and the case fold
    ["state:[#TODO]", '.filter(state = "todo")'],
    ["priority:[#a]", '.filter(priority = "A")'],
    // the metas, as constructors
    ["tag:*archive*", ".filter(tag = Archive)"],
    ["planned:*empty*", ".filter(planned = Empty)"],
    ["state:*inactive*", ".filter(state = Inactive)"],
    // negation scopes the whole token, alternatives included — De Morgan
    ["-state:TODO|DONE", '.filter(state /= ["TODO", "DONE"])'],
    ["-tag:chore", '.filter(not (tag = "chore"))'],
    // law 5's agreement half: on a bare axis, `|' and `+' are one thing
    ["state:TODO|DONE", '.filter(state = ["TODO", "DONE"])'],
    ["state:TODO +state:DONE", '.filter(state = ["TODO", "DONE"])'],
    // the tag intersection, which is NOT the list
    ["tag:web tag:glance", '.filter(tag = All ["web", "glance"])'],
    ["tag:web tag:glance|docs", '.filter(tag = All ["web", ["glance", "docs"]])'],
    // law 1 INSIDE an axis, and law 4: the conjuncts and the alternatives are
    // sets, so a different written order and a repeat print the same bytes
    ["tag:web tag:glance", '.filter(tag = All ["glance", "web"])'],
    ["state:TODO|DONE", '.filter(state = ["DONE", "TODO"])'],
    ["tag:web tag:web", '.filter(tag = "web")'],
    ["state:DONE|TODO|DONE", '.filter(state = ["TODO", "DONE"])'],
    // quoting: a literal hyphen is a literal, never a sign
    ['tag:"-chore"', '.filter(tag = "-chore")'],
    ['substring:"-x"', '.filter(substring = "-x")'],
    // free text, keyed and bare, share one axis
    ["milk", '.filter(substring = "milk")'],
    ["milk", '.filter("milk")'],
    // the shaping halves
    ["sort:deadline->title:desc", '.sort(columns = ["Deadline", Desc "Title"])'],
    ["sort:*none*", ".sort(None)"],
    ["columns:State,Deadline", '.columns("State", "Deadline")'],
    // CASE CARRIES NOTHING: names are looked up, never read off their first
    // letter, and the stage names go the same way
    ["state:*active*", ".filter(STATE = ACTIVE)"],
    ["state:*active*", ".FILTER(State = active)"],
    ["sort:deadline", '.SORT(columns = ["deadline"])'],
    ["tag:web tag:glance", '.filter(TAG = all ["web", "glance"])'],
    ["-tag:chore", '.filter(NOT (Tag = "chore"))'],
    // the shape kwargs cannot say, said as the flat string it is
    ["priority:[#A] +priority:[#B] tag:book",
     '.filter(raw "priority:[#A] +priority:[#B]", tag = "book")'],
  ],
  trip: [
    "state:*active* -tag:chore state:TODO|DONE",
    "tag:web tag:glance|docs",
    "priority:[#A] tag:book +priority:[#B]",
    "state:*active* -planned:*empty* sort:scheduled",
    'substring:"-x" -state:TODO|DONE',
    "columns:State,Title,owner sort:deadline:desc->title",
    "milk +bread",
  ],
  diff: [
    // law 5's PARTING case: (u ∧ v1) ∨ v2 is not u ∧ (v1 ∨ v2)
    ["state:NEXT state:TODO|DONE", "state:NEXT state:TODO +state:DONE"],
    // the intersection is not the alternation
    ["tag:web tag:glance", "tag:web|glance"],
    // and negation does not distribute the way a reader might hope
    ["-state:TODO|DONE", "-state:TODO -state:DONE"],
    // the order is written order
    ["sort:state->title", "sort:title->state"],
    // a direction is part of the answer
    ["sort:deadline", "sort:deadline:desc"],
    // and `columns:' is not narrowing
    ["columns:State", "columns:Deadline"],
  ],
};

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
  // THE CARET LIVES INSIDE THE PAINTED RUN, so DOM order is read with the
  // document's own comparison rather than off the stage's child list.
  const after = (a, b) =>
    (a.compareDocumentPosition(b) & Node.DOCUMENT_POSITION_FOLLOWING) !== 0;
  if (!(after(pars[0], caret) && after(caret, pars[1])))
    return { ok: false, why: "caret is not between the parens" };
  const rc = caret.getBoundingClientRect();
  const ro = pars[0].getBoundingClientRect(), rk = pars[1].getBoundingClientRect();
  if (!(ro.right <= rc.left + 1.5 && rc.right <= rk.left + 1.5))
    return { ok: false, why: "caret is drawn outside the parens" };
  const c = RIG.caret();
  return { ok: true, where: RIG.cx().where, fn: RIG.cx().stages.slice(-1)[0].fn,
           args: args ? args.textContent : "",
           at: c ? c.at : -1, len: c ? c.len : -1 };
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
  // THE SAME RUNGS, SPELLED IN THE TAB'S OWN DIALECT.  What is asserted is the
  // flat string every one of them composes; what differs is what is TYPED.
  const typed = page === TYPED;
  const FILTER_TEXT = typed ? 'state = Active, tag /= "chore"' : BOOT_FILTER;
  // WHAT A READER TYPES, not what stands afterwards: the opened slot supplies
  // the quotes and spends the space, so the value goes in bare and the closing
  // quote is stepped over.
  const CHAIN_ARG = typed ? 'state = TODO"' : "state:TODO";
  // `/' IS THE ADD-A-CONDITION GESTURE in the typed dialect: it lands on a
  // FRESH argument, comma already appended, so what is typed is the condition
  // alone.  An empty stage gets no comma — there is nothing to follow.
  const ADD_ARG = typed ? 'tag = docs"' : " +tag:docs";
  const REOPEN_TEXT = typed ? FILTER_TEXT + ", " : FILTER_TEXT;
  const REOPEN_LEAD = typed ? 'state = "…"' : null;
  const FRESH_LEAD = typed ? 'state = "…"' : "state:";
  const ADD_TOKEN = typed ? "tag:docs" : "+tag:docs";
  const DRY_HALF = typed ? 'state = ""' : "state:";
  const DRY_NEXT = typed ? "A" : "TO";
  const DRY_FULL = typed ? "state = Active" : "state:TODO";
  // A KEY ACCEPT THAT OPENS A SLOT IS MID-CONSTRUCTION, so its value offers
  // stand at once; the flat dialect has no slot and its key accept is done.
  const DRY_MENU = typed;
  const COLS_ARG = typed ? 'State", Deadline"' : "State,Deadline";
  // `.sort(' in F wants the `columns' kwarg first, which the offers spell: one
  // TAB takes `columns = [""]' with the caret in the slot.
  const SORT_KEYS = typed ? [[KEY.Tab], chars('Deadline"')] : [chars("deadline")];
  const SORT2_KEYS = typed ? [[KEY.Tab], chars('State", Title"')]
                           : [chars("state, title")];
  const SORT_PILL = typed ? 'sort(columns = ["Deadline"])' : "sort(deadline)";
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
  await ff.keys(chars(CHAIN_ARG));
  await ff.keys([")", ".", "s"]);
  await ff.keys([KEY.Tab]);
  for (const batch of SORT_KEYS) await ff.keys(batch);
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
  const FLAT_LAW = {
    f_comma_space: ["filter", "state:TODO, tag:web", "state:TODO tag:web"],
    f_comma: ["filter", "state:TODO,tag:web", "state:TODO tag:web"],
    f_space: ["filter", "state:TODO tag:web", "state:TODO tag:web"],
    f_value: ["filter", "tag:a,b", "tag:a,b"],
    f_quoted: ["filter", 'title:"a, b" tag:web', 'title:"a, b" tag:web'],
    f_signed: ["filter", "state:TODO,+priority:[#B],-tag:chore",
               "state:TODO +priority:[#B] -tag:chore"],
    s_comma_space: ["sort", "state, title", "sort:state->title"],
    s_comma: ["sort", "state,title", "sort:state->title"],
    s_arrow: ["sort", "state->title", "sort:state->title"],
    s_dir: ["sort", "deadline:desc, title", "sort:deadline:desc->title"],
    c_comma_space: ["columns", "State, Deadline", "columns:State,Deadline"],
    c_comma: ["columns", "State,Deadline", "columns:State,Deadline"],
  };
  // F'S COMMA IS THE SURFACE'S OWN — a Haskell argument list's separator, and
  // inside brackets the list's.  The law it owes is the same: what the stage
  // COMPOSES, whatever the spacing.
  const TYPED_LAW = {
    f_comma_space: ["filter", 'state = "TODO", tag = "web"', "state:TODO tag:web"],
    f_comma: ["filter", 'state = "TODO",tag = "web"', "state:TODO tag:web"],
    f_space: ["filter", 'state = "TODO" , tag = "web"', "state:TODO tag:web"],
    // A comma inside a VALUE needs no quoting in the flat string — only
    // `columns:' splits on one — so the literal travels bare.
    f_value: ["filter", 'tag = "a,b"', "tag:a,b"],
    f_quoted: ["filter", 'title = "a, b", tag = "web"', 'title:"a, b" tag:web'],
    f_list: ["filter", 'state = ["TODO", "DONE"]', "state:TODO|DONE"],
    f_all: ["filter", 'tag = All ["web", "glance"]', "tag:web tag:glance"],
    f_neg: ["filter", 'state /= ["TODO", "DONE"]', "-state:TODO|DONE"],
    f_ctor: ["filter", "state = Active, tag /= Archive", "state:*active* -tag:*archive*"],
    // POSITIONALS AND KWARGS MIX, positionals first: a bare literal in
    // `.filter(…)' is free text, which shares the `substring' axis.
    f_mixed: ["filter", '"milk", state = Active', "substring:milk state:*active*"],
    // `.sort(columns = […])': the list is the chain, in written order, and the
    // names are QUOTED — an open set sits on the string side.
    s_list: ["sort", 'columns = ["State", "Title"]', "sort:state->title"],
    // THE DIRECTION IS A CONSTRUCTOR APPLIED TO THE NAME, per segment.
    s_desc: ["sort", 'columns = [Desc "Deadline", "Title"]', "sort:deadline:desc->title"],
    s_desc2: ["sort", 'columns = ["Deadline", Desc "Title"]', "sort:deadline->title:desc"],
    // `Asc' is spellable and never emitted — "nothing or `:asc'".
    s_asc: ["sort", 'columns = [Asc "Title"]', "sort:title"],
    // AND THE SUFFIX IS NO LONGER A SPELLING: taken as written, the string
    // names a column with a colon in it, which is not one of the six — the
    // flat grammar refuses such a segment, and here it takes effect nowhere.
    s_suffix: ["sort", 'columns = ["Deadline:desc"]', ""],
    s_one: ["sort", 'columns = "Deadline"', "sort:deadline"],
    s_head: ["sort", 'columns = ["Tags", "#"]', "sort:tag->priority"],
    s_none: ["sort", "None", "sort:*none*"],
    // `.columns("State", "Title")': positional, quoted, custom names and all.
    c_comma_space: ["columns", '"State", "Deadline"', "columns:State,Deadline"],
    c_custom: ["columns", '"State", "owner"', "columns:State,owner"],
    // A BARE WORD IS NO NAME: nothing composes rather than something wrong.
    c_bare: ["columns", "State, Deadline", ""],
  };
  const LAW = typed ? TYPED_LAW : FLAT_LAW;
  const law = await ff.eval((spec) => {
    const out = {};
    for (const k of Object.keys(spec)) out[k] = RIG.stageString(spec[k][0], spec[k][1]);
    return out;
  }, LAW);
  const lawOff = Object.keys(LAW).filter((k) => law[k] !== LAW[k][2]);
  await ff.keys(["."]);
  await ff.keys([KEY.Tab]);
  await ff.keys(chars(typed ? 'state = TODO", tag = web"' : "state:TODO, tag:web"));
  await ff.keys([")", ".", "s"]);
  await ff.keys([KEY.Tab]);
  for (const batch of SORT2_KEYS) await ff.keys(batch);
  await ff.keys([")"]);
  const drove = await ff.eval(() => RIG.composed());
  want("COMMA", lawOff.length === 0 && drove === "state:TODO tag:web sort:state->title",
       (lawOff.length ? lawOff.map((k) => `${k}=${JSON.stringify(law[k])}`).join(" ") + " · " : "")
       + `drove=${JSON.stringify(drove)}`);

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
  await ff.keys(chars(DRY_NEXT));
  const woke = await ff.eval(() => RIG.menu().open);
  await ff.keys([KEY.Enter]);
  const dry2 = await ff.eval(liveArgs);
  want("DRY", dry1.args === DRY_HALF && dry1.menu === DRY_MENU && woke === true
       && dry2.args === DRY_FULL && dry2.menu === false && dry2.door === "compose",
       `key=${JSON.stringify(dry1.args)}/${dry1.menu} woke=${woke} `
       + `value=${JSON.stringify(dry2.args)}/${dry2.menu} door=${dry2.door}`);

  // ---- ESC: the ladder, in the dialect that owns it.  The flat door and D's
  // chain keep the shipped THREE RUNGS — the offers, what is half-written, the
  // box.  F answers to the reader's own rule, ESC CANCELS INPUT, and there is
  // exactly ONE rung: the same press takes the offers, the half-written call
  // and the box together.
  const rung = () => ({ menu: RIG.menu().open, door: RIG.door(), chips: RIG.chips(),
                        held: RIG.door() === "compose" ? RIG.cx().stages.length
                          : document.querySelector("#app .tv-filter").value.length });
  await ff.goto(url);
  await ff.keys([".", "s"]);
  const rung0 = await ff.eval(rung);
  await ff.keys([KEY.Escape]);
  const rung1 = await ff.eval(rung);
  if (typed) {
    want("ESC", rung0.menu === true && rung0.held > 0
         && rung1.menu === false && rung1.door === null && rung1.held === 0
         && eq(rung1.chips, BOOT_CHIPS),
         `offers=${rung0.menu}→${rung1.menu} held=${rung0.held}→${rung1.held} `
         + `door=${rung0.door}→${rung1.door} chips=${JSON.stringify(rung1.chips)}`);
  } else {
    await ff.keys([KEY.Escape]);
    const rung2 = await ff.eval(rung);
    await ff.keys([KEY.Escape]);
    const rung3 = await ff.eval(rung);
    want("ESC", rung0.menu === true && rung1.menu === false && rung1.door !== null
         && rung1.held > 0 && rung2.door !== null && rung2.held === 0
         && rung3.door === null && eq(rung3.chips, BOOT_CHIPS),
         `offers=${rung0.menu}→${rung1.menu} held=${rung1.held}→${rung2.held} `
         + `door=${rung2.door}→${rung3.door} chips=${JSON.stringify(rung3.chips)}`);
  }

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
    for (const batch of SORT_KEYS) await ff.keys(batch);
    await ff.keys([")", KEY.Enter]);
    await ff.keys(["/"]);
    const open = await ff.eval(caretInParens);
    const opened = await ff.eval(() => ({ door: RIG.door(), cx: RIG.cx(),
                                          pills: RIG.pills(), q: RIG.query(),
                                          menu: RIG.menu().open,
                                          lead: RIG.menu().items[0] }));
    await ff.keys(chars(ADD_ARG));
    await ff.keys([")", KEY.Enter]);
    const rewrote = await ff.eval(() => ({ pills: RIG.pills(), q: RIG.query() }));
    const filters = rewrote.pills.filter((p) => p.startsWith("filter("));
    want("SLASH-STAGE",
         open.ok === true && open.args === REOPEN_TEXT && open.fn === "filter"
         && open.at === open.len && opened.menu === true
         && (!REOPEN_LEAD || opened.lead === REOPEN_LEAD)
         && opened.cx.stages.length === 1 && opened.q.startsWith(BOOT_FILTER)
         && filters.length === 1 && rewrote.pills.length === 2
         && rewrote.pills[1] === SORT_PILL
         && rewrote.q === BOOT_FILTER + " " + ADD_TOKEN + " sort:deadline",
         open.why || `opened=${JSON.stringify(open.args)} caret=${open.at}/${open.len} `
         + `menu=${opened.menu}/${JSON.stringify(opened.lead)} `
         + `stages=${opened.cx.stages.length} pills=${JSON.stringify(rewrote.pills)} `
         + `q=${JSON.stringify(rewrote.q)}`);

    // …and with no filter stage standing it spawns exactly one fresh one.  The
    // way to none is the edit itself: reopen, empty, close, commit.
    await ff.keys(["/"]);
    const held = await ff.eval(() => {
      const s = RIG.cx().stages.slice(-1)[0];
      return s && s.args ? s.args.length : 0;
    });
    await ff.keys(new Array(held).fill(KEY.Backspace));
    // …and `/' over a stage that is open and EMPTY adds no comma either: there
    // is nothing standing for a fresh argument to follow.  It also stays the
    // same rewrite, or the badge's tokens would land twice.
    if (typed) await ff.keys(["/"]);
    const reEmpty = typed
      ? await ff.eval(() => ({ args: RIG.cx().stages.slice(-1)[0].args,
                               at: (RIG.caret() || {}).at }))
      : { args: "", at: 0 };
    await ff.keys([")", KEY.Enter]);
    const gone = await ff.eval(() => ({ pills: RIG.pills(), q: RIG.query() }));
    await ff.keys(["/"]);
    const fresh = await ff.eval(() => ({ cx: RIG.cx(), door: RIG.door(),
                                         menu: RIG.menu().open,
                                         lead: RIG.menu().items[0] }));
    want("SLASH-FRESH",
         eq(gone.pills, [SORT_PILL]) && fresh.door === "compose"
         && fresh.cx.stages.length === 1 && fresh.cx.stages[0].fn === "filter"
         // NO COMMA where nothing stands to be added to — and the offers are
         // open here too, the position being new either way.
         && fresh.cx.stages[0].args === "" && fresh.cx.where === "args"
         && fresh.menu === true && fresh.lead === FRESH_LEAD
         && reEmpty.args === "" && reEmpty.at === 0,
         `gone=${JSON.stringify(gone.pills)} fresh=${JSON.stringify(fresh.cx)} `
         + `menu=${fresh.menu}/${JSON.stringify(fresh.lead)} `
         + `reEmpty=${JSON.stringify(reEmpty)}`);

    // ---- SLASH-ABANDON: a fresh argument the reader never wrote leaves no
    // trace — the dangling comma goes at the close and the badge returns to
    // the spelling it had, byte for byte.
    await ff.goto(url);
    const was = await ff.eval(() => ({ pills: RIG.pills(), q: RIG.query() }));
    await ff.keys(["/"]);
    await ff.keys([")"]);
    const shut = await ff.eval(() => RIG.cx().stages.slice(-1)[0].args);
    await ff.keys([KEY.Enter]);
    const back2 = await ff.eval(() => ({ pills: RIG.pills(), q: RIG.query() }));
    want("SLASH-ABANDON",
         shut === FILTER_TEXT && eq(back2.pills, was.pills) && back2.q === was.q,
         `closed=${JSON.stringify(shut)} was=${JSON.stringify(was)} `
         + `now=${JSON.stringify(back2)}`);

    // ---- DEL-STAGE: the chain's own backspace — the latest badge, whole, and
    // pressing it again walks the chain backward.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys(["s"]);
    await ff.keys([KEY.Tab]);
    for (const batch of SORT_KEYS) await ff.keys(batch);
    await ff.keys([")", "."]);
    await ff.keys(["c"]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars(COLS_ARG));
    await ff.keys([")", KEY.Enter]);
    const built = await ff.eval(() => ({ pills: RIG.pills(), q: RIG.query() }));
    const walk = [];
    for (let i = 0; i < 4; i += 1) {
      await ff.keys([DEL]);
      walk.push(await ff.eval(() => RIG.query()));
    }
    want("DEL-STAGE",
         built.pills.length === 3 && built.pills[1] === SORT_PILL
         && built.q === BOOT_FILTER + " sort:deadline columns:State,Deadline"
         && eq(walk, ["state:*active* -tag:chore sort:deadline",
                      "state:*active* -tag:chore", "", ""]),
         `built=${JSON.stringify(built.pills)} q=${JSON.stringify(built.q)} `
         + `walk=${JSON.stringify(walk)}`);

    // …and inside an open paren edit it is ordinary text editing, eating nothing.
    await ff.goto(url);
    await ff.keys(["/"]);
    await ff.keys([DEL]);
    const inside = await ff.eval(() => ({ cx: RIG.cx(), q: RIG.query() }));
    want("DEL-INSIDE",
         inside.cx.stages.length === 1 && inside.cx.stages[0].args === REOPEN_TEXT
         && inside.q === BOOT_FILTER,
         `cx=${JSON.stringify(inside.cx)} q=${JSON.stringify(inside.q)}`);
  }

  if (typed) {
    // ---- ESC-ABANDON: ESC CANCELS INPUT.  SLASH-ABANDON above is the reader
    // who closes an untouched edit; this is the reader who WALKS OUT of one,
    // and in the typed surface that takes a single press whatever is on the
    // screen — the offers standing over the position, the text typed into it,
    // and the comma the `/' summoned all go with the edit, and the box goes
    // back to the strip it was summoned from.  What is asserted is the WHOLE
    // picture: chips, box, rows, hint and the two lines under it.
    const held2 = () => ({ q: RIG.query(), pills: RIG.pills(), door: RIG.door(),
                           chips: RIG.chips(), stages: RIG.cx().stages });
    const open2 = () => ({ args: (RIG.cx().stages.slice(-1)[0] || {}).args,
                           at: (RIG.caret() || {}).at, menu: RIG.menu().open });
    /** One route in and one press out: the screen has to come back byte for byte. */
    const walkOut = async (into) => {
      await ff.goto(url);
      const was = await ff.eval(picture), wasQ = await ff.eval(held2);
      for (const batch of into) await ff.keys(batch);
      const mid = await ff.eval(open2);
      await ff.keys([KEY.Escape]);
      return { was, wasQ, mid, now: await ff.eval(picture), nowQ: await ff.eval(held2) };
    };
    // (a) THE SUMMON ALONE: `/' dangles a comma and stands its offers; one ESC
    // takes both, and the badge is spelled the way it was.
    const escA = await walkOut([["/"]]);
    // (b) …AND WITH A CONDITION TYPED INTO IT.  The typed text is the reader's
    // own work and it goes too: a cancel that kept it would be a commit.  What
    // this route has to lose is the text and the comma and NOT the offers — the
    // condition is complete, so the menu is already down over it (see DONE),
    // and the cancel is the same one press either way.
    const escB = await walkOut([["/"], chars('tag = docs"')]);
    // (c) …AND MID-EDIT OF AN ARGUMENT ALREADY WRITTEN, which is reached by
    // moving the caret rather than by the gesture: `"chore"' is retyped as
    // `"milk"' and ESC brings the whole pre-edit spelling back.
    const escC = await walkOut([["/"], new Array(3).fill(KEY.ArrowLeft),
                                new Array(5).fill(KEY.Backspace), chars("milk")]);
    const same = (r) => r.was === r.now && eq(r.wasQ, r.nowQ)
      && r.nowQ.door === null && r.nowQ.stages.length === 0;
    want("ESC-ABANDON",
         // the edit really had something to lose: the comma, the offers, the text
         escA.mid.args === REOPEN_TEXT && escA.mid.menu === true && same(escA)
         && escB.mid.args === FILTER_TEXT + ', tag = "docs"' && escB.mid.menu === false
         && same(escB)
         && escC.mid.args === 'state = Active, tag /= "milk", ' && same(escC),
         `a=${JSON.stringify(escA.mid)}/${same(escA)} `
         + `b=${JSON.stringify(escB.mid)}/${same(escB)} `
         + `c=${JSON.stringify(escC.mid)}/${same(escC)} `
         + `door=${escA.nowQ.door} q=${JSON.stringify(escA.nowQ.q)} `
         + `pills=${JSON.stringify(escA.nowQ.pills)}`);

    // ---- ESC-RESTORE: WHAT THE EDIT FOUND GOES BACK, and there are two things
    // the chips cannot speak for.  A stage closed but not yet ASKED FOR lives
    // in the box rather than on the strip, so the cancel is the only thing that
    // can put it back — and it has to put back the spelling the edit found
    // rather than the one it was given.  An edit the summon INTERRUPTED — `/'
    // is legal inside another stage's parens — is still being written, so the
    // box returns to it open, caret and offers where they stood.  With no edit
    // open the same key takes the box, and the uncommitted stage with it.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars('state = TODO"'));
    await ff.keys([")"]);
    const pend0 = await ff.eval(picture), pendQ = await ff.eval(held2);
    await ff.keys(["/"]);
    const pendOpen = await ff.eval(open2);
    await ff.keys(chars("zz"));
    await ff.keys([KEY.Escape]);
    const pend1 = await ff.eval(picture), pendBack = await ff.eval(held2);
    await ff.keys([KEY.Escape]);
    const pendGone = await ff.eval(held2);
    // …and the interrupted edit: `.sort(' half written, `/' summoned over it,
    // one press and the sort stage is back with its caret where it was.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys(["s"]);
    await ff.keys([KEY.Tab]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars('Deadline"'));
    const under0 = await ff.eval(open2), underPic = await ff.eval(picture);
    await ff.keys(["/"]);
    const underOn = await ff.eval(open2);
    await ff.keys(chars('tag = docs"'));
    await ff.keys([KEY.Escape]);
    const under1 = await ff.eval(open2), underPic1 = await ff.eval(picture);
    const underCx = await ff.eval(() => ({ where: RIG.cx().where, door: RIG.door(),
                                           stages: RIG.cx().stages.length }));
    want("ESC-RESTORE",
         pendOpen.args === 'state = "TODO", ' && pendOpen.menu === true
         && pend1 === pend0 && eq(pendBack, pendQ)
         && pendBack.door === "compose" && pendBack.stages.length === 1
         && pendBack.stages[0].args === 'state = "TODO"'
         && pendBack.stages[0].pending === true
         && pendGone.door === null && eq(pendGone.chips, BOOT_CHIPS)
         && pendGone.stages.length === 0
         && under0.args === 'columns = ["Deadline"]' && underOn.args === REOPEN_TEXT
         && eq(under1, under0) && underPic1 === underPic
         && underCx.where === "args" && underCx.door === "compose"
         && underCx.stages === 1,
         `open=${JSON.stringify(pendOpen)} back=${JSON.stringify(pendBack.stages)} `
         + `strip=${pend1 === pend0} gone=${JSON.stringify(pendGone)} `
         + `under=${JSON.stringify(under0)}→${JSON.stringify(under1)}`
         + `/${underPic1 === underPic} cx=${JSON.stringify(underCx)}`);

    // ---- SIGNS: neither sign is a spelling in the typed surface, so both are
    // KEYS: `-' flips the kwarg under the caret between `=' and `/=' and flips
    // it back, `+' turns its value into a Haskell list with a fresh slot, and
    // the flat string each composes is the grammar's own.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars('state = TODO"'));
    const read = () => ({ args: (RIG.cx().stages.slice(-1)[0] || {}).args,
                          at: RIG.caret(), composed: RIG.composed() });
    await ff.keys(["-"]);
    const neg = await ff.eval(read);
    await ff.keys(["-"]);
    const back = await ff.eval(read);
    await ff.keys(["+"]);
    const wide = await ff.eval(read);
    await ff.keys(chars('"DONE"'));
    const list = await ff.eval(read);
    await ff.keys([")", KEY.Enter]);
    const gotSigns = await ff.eval(() => ({ q: RIG.query(), rows: RIG.rows() }));
    want("SIGNS",
         neg.args === 'state /= "TODO"' && neg.composed === "-state:TODO"
         && back.args === 'state = "TODO"' && back.composed === "state:TODO"
         && wide.args === 'state = ["TODO", ]' && wide.at.at === 17
         && list.args === 'state = ["TODO", "DONE"]'
         && list.composed === "state:TODO|DONE"
         && gotSigns.q === BOOT_FILTER + " state:TODO|DONE" && gotSigns.rows === 2,
         `neg=${JSON.stringify(neg.args)}/${JSON.stringify(neg.composed)} `
         + `back=${JSON.stringify(back.args)} wide=${JSON.stringify(wide.args)}@${wide.at.at} `
         + `list=${JSON.stringify(list.args)}/${JSON.stringify(list.composed)} `
         + `q=${JSON.stringify(gotSigns.q)}`);

    // ---- SLOT: the key and its equals come with an OPENED QUOTED SLOT, and
    // what is taken out of it decides the quotes' fate — a constructor is no
    // string, so accepting one swallows them; a literal keeps them.  Both stay
    // dry.  Typing the equals by hand opens the same slot.
    const held = () => ({ args: (RIG.cx().stages.slice(-1)[0] || {}).args,
                          at: (RIG.caret() || {}).at, menu: RIG.menu().open,
                          items: RIG.menu().items.slice(0, 4), c: RIG.composed() });
    // ROUTE ONE — RET over the key.  The slot opens AND its offers do: a key
    // accept has finished no term, it has moved the reader somewhere new.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars("sta"));
    await ff.keys([KEY.Enter]);
    const slot = await ff.eval(held);
    await ff.keys([KEY.Tab]);                    // the constructor, out of the slot
    const ctor = await ff.eval(held);
    // ROUTE TWO — the equals typed by hand.  The same slot, the same offers,
    // and this field's own domain leads them.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars("tag ="));
    const byHand = await ff.eval(held);
    // …AND THE VALUE ACCEPT IS THE ONE THAT IS FINAL: dry, closed, and a
    // repaint does not resurrect the offers.
    await ff.keys(chars("chor"));
    await ff.keys([KEY.Tab]);
    const str = await ff.eval(held);
    await ff.eval(() => RIG.repaint());
    const settledMenu = await ff.eval(() => RIG.menu().open);
    await ff.keys(chars(', state = TODO"'));      // the closing quote is stepped over
    const past = await ff.eval(held);
    want("SLOT",
         slot.args === 'state = ""' && slot.at === 9 && slot.menu === true
         && eq(slot.items, ["Active", "Inactive", "Empty", '"TODO"'])
         && ctor.args === "state = Active" && ctor.menu === false
         && ctor.c === "state:*active*"
         && byHand.args === 'tag = ""' && byHand.at === 7 && byHand.menu === true
         && eq(byHand.items.slice(0, 2), ["Empty", "Archive"])
         && /^"/.test(byHand.items[2])
         && str.args === 'tag = "chore"' && str.menu === false && str.c === "tag:chore"
         && settledMenu === false
         && past.args === 'tag = "chore", state = "TODO"'
         && past.c === "tag:chore state:TODO",
         `slot=${JSON.stringify(slot)} ctor=${JSON.stringify(ctor)} `
         + `hand=${JSON.stringify(byHand)} str=${JSON.stringify(str)} `
         + `settled=${settledMenu} past=${JSON.stringify(past)}`);

    // ---- DONE: A COMPLETE TERM ENDS THE CONVERSATION.  SLOT above is round
    // 11's law read forwards — an accept that leaves the caret inside what it
    // wrote asks again — and this is the same law read backwards: the offers
    // stand at fresh and UNFINISHED positions, and a position whose term is
    // finished carries none, whichever path asked for them.  So `RET' over one
    // applies the stage exactly as it does on untouched ground, where the
    // reported bug was a menu standing over `tag /= "chore"|' and eating the
    // key — RET accepting an offer, forever, with no way to commit the filter.
    const stood = () => ({ args: (RIG.cx().stages.slice(-1)[0] || {}).args,
                           at: (RIG.caret() || {}).at, menu: RIG.menu().open,
                           lead: RIG.menu().items[0] });
    const landed = () => ({ q: RIG.query(), pills: RIG.pills(), door: RIG.door(),
                            rows: RIG.rows() });
    // (a) A CLOSED STRING LITERAL, the caret stepped over its far quote.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars('tag = docs"'));
    const doneStr = await ff.eval(stood);
    // …and the TERM is what decides, never the offset: a space after it is
    // still a finished term, and the menu stays down over it.
    await ff.keys([" "]);
    const doneSpace = await ff.eval(stood);
    await ff.keys([KEY.Backspace]);
    // …and the counter-cases ride the same drive: a comma is a FRESH position,
    // whose own offers stand at once, and taking the comma back puts the menu
    // down again — the term is read at the caret and never latched.
    await ff.keys([","]);
    const doneFresh = await ff.eval(stood);
    await ff.keys([KEY.Backspace]);
    const doneAgain = await ff.eval(stood);
    await ff.keys([KEY.Enter]);
    const doneApplied = await ff.eval(landed);
    // (b) A FINISHED CONSTRUCTOR, and the caret WALKED onto its tail rather
    // than left there by the accept: what decides is the term, not the gesture.
    // One step short of the whole word the name is still being written, so
    // those offers stand — and the step back forward puts them down.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars("tag ="));
    await ff.keys(chars("Arch"));
    await ff.keys([KEY.Tab]);
    const doneCtor = await ff.eval(stood);
    await ff.keys([KEY.ArrowLeft]);
    const doneHalf = await ff.eval(stood);
    await ff.keys([KEY.ArrowRight]);
    const doneWalk = await ff.eval(stood);
    await ff.keys([KEY.Enter]);
    const ctorApplied = await ff.eval(landed);
    // (c) A CLOSED WRAPPER, the paren the READER typed rather than the stage's
    // own: the literal inside it is finished the moment its quote is stepped
    // over, and so is the `not (…)' the moment it shuts.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars("not (tag ="));
    await ff.keys(chars('docs"'));
    const doneInner = await ff.eval(stood);
    await ff.keys([")"]);
    const doneWrap = await ff.eval(stood);
    await ff.keys([KEY.Enter]);
    const wrapApplied = await ff.eval(landed);
    want("DONE",
         doneStr.args === 'tag = "docs"' && doneStr.at === 12
         && doneStr.menu === false
         && doneSpace.args === 'tag = "docs" ' && doneSpace.at === 13
         && doneSpace.menu === false
         && doneFresh.args === 'tag = "docs",' && doneFresh.at === 13
         && doneFresh.menu === true && doneFresh.lead === FRESH_LEAD
         && eq(doneAgain, doneStr)
         && doneApplied.q === BOOT_FILTER + " tag:docs"
         && doneApplied.door === null && doneApplied.rows === 1
         && eq(doneApplied.pills,
               ['filter(state = Active, tag = "docs", tag /= "chore")'])
         && doneCtor.args === "tag = Archive" && doneCtor.at === 13
         && doneCtor.menu === false
         && doneHalf.at === 12 && doneHalf.menu === true
         && doneHalf.lead === "Archive"
         && eq(doneWalk, doneCtor)
         && ctorApplied.q === BOOT_FILTER + " tag:*archive*"
         && ctorApplied.door === null
         && eq(ctorApplied.pills,
               ['filter(state = Active, tag = Archive, tag /= "chore")'])
         && doneInner.args === 'not (tag = "docs"' && doneInner.at === 17
         && doneInner.menu === false
         && doneWrap.args === 'not (tag = "docs")' && doneWrap.at === 18
         && doneWrap.menu === false
         && wrapApplied.q === BOOT_FILTER + " -tag:docs"
         && wrapApplied.door === null && wrapApplied.rows === 3,
         `str=${JSON.stringify(doneStr)} space=${JSON.stringify(doneSpace)} `
         + `fresh=${JSON.stringify(doneFresh)} again=${JSON.stringify(doneAgain)} `
         + `applied=${JSON.stringify(doneApplied)} `
         + `ctor=${JSON.stringify(doneCtor)} half=${JSON.stringify(doneHalf)} `
         + `walk=${JSON.stringify(doneWalk)} ctorApplied=${JSON.stringify(ctorApplied)} `
         + `inner=${JSON.stringify(doneInner)} wrap=${JSON.stringify(doneWrap)} `
         + `wrapApplied=${JSON.stringify(wrapApplied)}`);

    // ---- QUOTED: the shaping stages take NAMES, and a name is a string —
    // columns are an open set, so no roster can close them.  The offers
    // complete INTO the quotes: `.columns(' spawns its positional slot with the
    // call, a comma spawns the next one, and the sort list's items are quoted
    // too, each offered plainly and with the `:desc' the segment carries.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys(["c"]);
    await ff.keys([KEY.Tab]);
    const colsSlot = await ff.eval(() => ({ args: (RIG.cx().stages.slice(-1)[0] || {}).args,
                                            at: (RIG.caret() || {}).at,
                                            items: RIG.menu().items.slice(0, 2) }));
    await ff.keys([KEY.Tab]);                  // …and take the first name
    const colsTook = await ff.eval(() => ({ args: (RIG.cx().stages.slice(-1)[0] || {}).args,
                                            c: RIG.composed() }));
    await ff.keys([","]);
    const colsNext = await ff.eval(() => ({ args: (RIG.cx().stages.slice(-1)[0] || {}).args,
                                            at: (RIG.caret() || {}).at }));
    await ff.keys(chars('Deadline"'));
    await ff.keys([")"]);
    const colsDone = await ff.eval(() => RIG.composed());

    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys(["s"]);
    await ff.keys([KEY.Tab]);
    const sortTop = await ff.eval(() => RIG.menu().items);
    await ff.keys([KEY.Tab]);
    const sortSlot = await ff.eval(() => ({ args: (RIG.cx().stages.slice(-1)[0] || {}).args,
                                            at: (RIG.caret() || {}).at,
                                            menu: RIG.menu().open }));
    // The list's first slot is a position, not a finished term: its offers are
    // already standing, so nothing has to ask for them.
    const sortItems = await ff.eval(() => RIG.menu().items.slice(0, 2));
    await ff.keys(chars("Dead"));
    await ff.keys([KEY.ArrowDown]);            // the second row is the reversed one
    await ff.keys([KEY.Tab]);
    await ff.keys([","]);
    const sortNext = await ff.eval(() => ({ args: (RIG.cx().stages.slice(-1)[0] || {}).args,
                                            at: (RIG.caret() || {}).at }));
    await ff.keys(chars('Title"'));
    await ff.keys([")"]);
    const sortDone = await ff.eval(() => RIG.composed());
    // …and the suffix spelling, taken as written, is an unknown column: marked
    // on the screen, composing nothing, mirroring the flat grammar's refusal.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys(["s"]);
    await ff.keys([KEY.Tab]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars('Deadline:desc"'));
    const suffix = await ff.eval(() => ({
      errs: RIG.dslErrors("sort", RIG.cx().stages.slice(-1)[0].args).length,
      composes: RIG.composed(),
      marks: document.querySelectorAll("#app .cx .cx-live .cx-bad").length,
    }));
    want("QUOTED",
         colsSlot.args === '""' && colsSlot.at === 1
         && eq(colsSlot.items, ['"State"', '"#"'])
         && colsTook.args === '"State"' && colsTook.c === "columns:State"
         && colsNext.args === '"State", ""' && colsNext.at === 10
         && colsDone === "columns:State,Deadline"
         && eq(sortTop, ['columns = [ "…" ]', "None"])
         && sortSlot.args === 'columns = [""]' && sortSlot.at === 12
         && sortSlot.menu === true && eq(sortItems, ['"State"', 'Desc "State"'])
         && sortNext.args === 'columns = [Desc "Deadline", ""]'
         && sortDone === "sort:deadline:desc->title"
         && suffix.errs === 1 && suffix.composes === "" && suffix.marks === 1,
         `colsSlot=${JSON.stringify(colsSlot)} took=${JSON.stringify(colsTook)} `
         + `next=${JSON.stringify(colsNext)} done=${JSON.stringify(colsDone)} `
         + `sortTop=${JSON.stringify(sortTop)} sortSlot=${JSON.stringify(sortSlot)} `
         + `sortItems=${JSON.stringify(sortItems)} `
         + `sortNext=${JSON.stringify(sortNext)} sortDone=${JSON.stringify(sortDone)} `
         + `suffix=${JSON.stringify(suffix)}`);

    // ---- CASE: any case is typed and any case parses — the closed world is
    // looked up, never read off a first letter — and what STANDS after the
    // accept is the canonical spelling.  A bare name nothing answers to is left
    // exactly as written and MARKED, because quoting is the one thing that says
    // "open value" and it was not typed.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars("NOT (TAG ="));
    await ff.keys(chars('chore"'));
    await ff.keys([")", ")"]);
    const canon = await ff.eval(() => ({ args: RIG.cx().stages.slice(-1)[0].args,
                                         c: RIG.composed() }));
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars("sta"));
    const partial = await ff.eval(() =>
      document.querySelectorAll("#app .cx .cx-live .cx-bad").length);
    await ff.keys(chars("rtzz"));
    const marked = await ff.eval(() => ({
      bad: document.querySelectorAll("#app .cx .cx-live .cx-bad").length,
      c: RIG.composed(),
    }));
    const apiCase = await ff.eval(() => ({
      canon: RIG.dslCanon('STATE = ACTIVE, tAg /= "Chore"', "filter"),
      left: RIG.dslCanon("state = chore", "filter"),
      errs: RIG.dslErrors("filter", "state = chore").length,
      composes: RIG.stageString("filter", "state = chore"),
      variant: RIG.stageString("filter", "STATE = ACTIVE"),
    }));
    want("CASE",
         canon.args === 'not (tag = "chore")' && canon.c === "-tag:chore"
         && partial === 0 && marked.bad === 1 && marked.c === ""
         && apiCase.canon === 'state = Active, tag /= "Chore"'
         && apiCase.left === "state = chore" && apiCase.errs === 1
         && apiCase.composes === "" && apiCase.variant === "state:*active*",
         `canon=${JSON.stringify(canon)} partial=${partial} `
         + `marked=${JSON.stringify(marked)} api=${JSON.stringify(apiCase)}`);

    // ---- WARN: THE DSL WARNS WHERE THE GRAMMAR IS MERELY HONEST.  Two legal
    // bindings can name a query no row can answer — `tag = All ["docs",
    // "chore"], tag /= "chore"' composes `tag:docs tag:chore -tag:chore' — and
    // the flat grammar is RIGHT to serve the empty table for it.  So this is a
    // WARNING and never a refusal: the pair is marked, one line says which
    // value contradicts which, and the compose and the apply are untouched.
    const warned = () => ({
      // the bindings, as the ink finds them; the innocent one is absent from it
      ink: [...document.querySelectorAll("#app .cx .cx-live .cx-warn")]
        .map((n) => n.textContent).join(""),
      said: [...document.querySelectorAll("#app .tv-hint .tv-warn")]
        .map((n) => n.textContent),
      badges: document.querySelectorAll("#app .tv-chips .cx-pill.cx-warn").length,
      args: (RIG.cx().stages.slice(-1)[0] || {}).args,
      c: RIG.composed(), q: RIG.query(), rows: RIG.rows(), door: RIG.door(),
    });
    /** `/' reopens the badge, which already carries `tag /= "chore"'; the
     *  reader adds the `All' list beside it, one element or two. */
    const addAll = async (tail) => {
      await ff.goto(url);
      await ff.keys(["/"]);
      await ff.keys(chars("tag ="));
      const at = await ff.eval(() => RIG.menu().items.indexOf('All [ "…" ]'));
      await ff.keys(new Array(Math.max(0, at)).fill(KEY.ArrowDown));
      await ff.keys([KEY.Tab]);
      await ff.keys(chars('docs"'));
      if (tail) await ff.keys(chars(tail));
      return at;
    };
    const allAt = await addAll(', "chore"');
    const conflict = await ff.eval(warned);
    await ff.keys([KEY.ArrowRight]);              // out of the list, then close
    await ff.keys([")", KEY.Enter]);
    const stillAsked = await ff.eval(warned);
    // …and the near miss stays QUIET: one element, and the axis is answerable.
    await addAll("");
    const quiet = await ff.eval(warned);
    await ff.keys([KEY.ArrowRight]);
    await ff.keys([")", KEY.Enter]);
    const quietOn = await ff.eval(warned);
    // …and the law itself, over flat queries, where the reading lives.
    const law = await ff.eval((boot) => ({
      pair: RIG.unsat("tag:docs tag:chore -tag:chore"),
      near: RIG.unsat("tag:docs -tag:chore"),
      // the SINGLE-VALUED rule, which `All' can spell where a repeated field
      // cannot — and which the tags cell, being the one list, never trips
      single: RIG.unsat("state:TODO state:DONE").said,
      tags: RIG.unsat("tag:web tag:docs").said,
      // the metas overlap by their own law, so no pair either is in is judged
      meta: RIG.unsat("state:*active* state:TODO").said,
      // a PREFIX key parts only where neither prefix extends the other
      nested: RIG.unsat("deadline:2026 deadline:2026-08").said,
      apart: RIG.unsat("deadline:2026-08 deadline:2027-01").said,
      // free text and titles match INSIDE a cell: two of them sit together
      text: RIG.unsat("title:ship title:chain").said,
      // one surviving alternative is a row, on either side of the sign
      alt: RIG.unsat("state:TODO|DONE state:TODO").said,
      altN: RIG.unsat("tag:web|docs -tag:web").said,
      // and a WIDENED axis has a second way to be true
      wide: RIG.unsat("tag:chore -tag:chore +tag:web").said,
      boot: RIG.unsat(boot).said,
    }), BOOT_FILTER);
    const SAID = 'tag: "chore" is both required and refused — no row can carry that';
    want("WARN",
         allAt >= 0
         && conflict.args === 'state = Active, tag /= "chore", tag = All ["docs", "chore"]'
         // BOTH bindings marked, and the innocent one is not among them
         && conflict.ink === 'tag/="chore"tag=All["docs","chore"]'
         && eq(conflict.said, [SAID])
         // …and nothing about the compose moved
         && conflict.c === BOOT_FILTER + " tag:docs tag:chore"
         // …and the apply goes through: the empty table IS the answer
         && stillAsked.q === BOOT_FILTER + " tag:docs tag:chore"
         && stillAsked.rows === 0 && stillAsked.door === null
         && eq(stillAsked.said, [SAID]) && stillAsked.badges === 1
         // the near miss says nothing, before or after the apply
         && quiet.ink === "" && eq(quiet.said, []) && quiet.badges === 0
         && quiet.c === BOOT_FILTER + " tag:docs"
         && quietOn.q === BOOT_FILTER + " tag:docs" && quietOn.rows === 1
         && eq(quietOn.said, []) && quietOn.badges === 0
         && eq(law.pair.said, [SAID])
         && eq(law.pair.tokens, ["tag:chore", "-tag:chore"])
         && eq(law.near.said, []) && eq(law.near.tokens, [])
         && eq(law.single,
               ['state: "TODO" and "DONE" are both required — no row is both'])
         && eq(law.tags, []) && eq(law.meta, []) && eq(law.nested, [])
         && law.apart.length === 1 && eq(law.text, []) && eq(law.alt, [])
         && eq(law.altN, []) && eq(law.wide, []) && eq(law.boot, []),
         `allAt=${allAt} conflict=${JSON.stringify(conflict)} `
         + `applied=${JSON.stringify(stillAsked)} quiet=${JSON.stringify(quiet)} `
         + `quietOn=${JSON.stringify(quietOn)} law=${JSON.stringify(law)}`);

    // ---- IR: TWO PARSERS, ONE NORMAL FORM.  Paired spellings have to print
    // the SAME bytes; the divergence corpus has to print different ones, or the
    // form is proving nothing.
    await ff.goto(url);
    const irs = await ff.eval((c) => ({
      same: c.same.map(([f, d]) => [f, d, RIG.irFlat(f), RIG.irDsl(d)]),
      trip: c.trip.map((q) => [q, RIG.dslChainOf(q), RIG.irFlat(q),
                               RIG.irDsl(RIG.dslChainOf(q))]),
      diff: c.diff.map(([x, y]) => [x, y, RIG.irFlat(x), RIG.irFlat(y)]),
      one: RIG.irFlat("state:*active* -tag:chore"),
    }), IR_CORPUS);
    const sameOff = irs.same.filter((r) => r[2] !== r[3]);
    const tripOff = irs.trip.filter((r) => r[2] !== r[3]);
    const diffOff = irs.diff.filter((r) => r[2] === r[3]);
    want("IR",
         sameOff.length === 0 && tripOff.length === 0 && diffOff.length === 0
         && irs.same.length === IR_CORPUS.same.length,
         [sameOff.map((r) => `${r[0]} ≠ ${r[1]}`).join(" · "),
          tripOff.map((r) => `round trip ${r[0]} → ${r[1]}`).join(" · "),
          diffOff.map((r) => `${r[0]} and ${r[1]} print the same IR`).join(" · ")]
           .filter(Boolean).join(" | "));
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
