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
             "d-stage-pills.html", "e-echo-line.html", "f-typed-dsl.html",
             "g-sql.html"];
const picked = process.argv[2] ? [process.argv[2]] : ALL;
const TYPED = "f-typed-dsl.html";
const SQL = "g-sql.html";

// THE CONTROL MISSES BY CONSTRUCTION, the way headline-bars' `flat' tab does:
// A opens the flat box, where a dot is a character, so it cannot spawn one,
// cannot land a caret in parens, composes no chain, and has no parens for a
// comma or an accept to land in.  Declared, so the run is green and the misses
// are the argument rather than a broken tab.
const MISSES = { "a-control.html": ["DOT", "PARENS", "CHAIN", "COMMA", "DRY", "REL"] };
// D ANSWERS THE TWO KEYS DIFFERENTLY, on purpose: `/' edits the filter stage
// and `DEL' takes it whole, so the flat-door rungs are not its to pass.  F is
// D's machinery with a TYPED surface, so it departs the same way — and its
// arguments are Haskell, which is why the rungs below are spelled per dialect
// rather than once.
// G DEPARTS TWICE OVER.  It is D's machinery, so `/' and `DEL' go the way D
// sent them; and it is SQL, so there is NO DOT — a clause is opened by its
// keyword and closed by the next one, and there are no parens for a caret to
// land in.  Both are declared here and answered by rungs of g's own (KEYWORD
// and HEAD), so what the tab owes is stated rather than dropped.
const DEPARTS = { "d-stage-pills.html": ["SLASH", "SIG"],
                  "f-typed-dsl.html": ["SLASH", "SIG"],
                  "g-sql.html": ["SLASH", "SIG", "DOT", "PARENS"] };

// THE ONE SENTENCE THE WARNING SAYS, read by both typed rungs.  G's WARN is the
// claim that it IS the same sentence — `unsatisfied' reads the atoms and never
// the text, so the surface that spells them differently still says this.  Two
// literals could only both be edited; one asserts it.
const SAID = 'tag: "chore" is both required and refused — no row can carry that';
// …AND THE SAME SENTENCE OVER A RELATION AXIS.  The value is spelled as the
// CALL, because on this axis the call already names the axis — and it is
// spelled off the ATOM, which is what makes `?kind=Blocked By' and
// `?kind=blocked-by' say one sentence rather than two.
const REL_SAID = (v) => `${v} is both required and refused — no row can carry that`;
// The one edge the relation rungs are driven on, in each surface's spelling.
const REL_TOKEN = "ref:def456?kind=blocked-by";

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
    // THE RELATIONS: one leaf, two readers.  The typed surface spells them as
    // CALLS, the kind binding as a kwarg — a kind inside the quotes would be a
    // second grammar hidden in a literal.
    ["ref:def456", '.filter(refs_to("def456"))'],
    ["from:abc123", '.filter(refs_from("abc123"))'],
    ["ref:def456?kind=blocked-by", '.filter(refs_to("def456", kind = "blocked-by"))'],
    // the slug is the PEER'S — downcased, whitespace folded — on the write and
    // on the read, so one edge written two ways is one atom
    ['ref:"def456?kind=Blocked By"', '.filter(refs_to("def456", kind = "Blocked By"))'],
    // the existence meta, spelled as the constructor every meta wears
    ["ref:*any*", ".filter(refs_to(Any))"],
    ["-from:*any*", ".filter(not (refs_from(Any)))"],
    ["from:*any*?kind=blocked-by", '.filter(refs_from(Any, kind = "blocked-by"))'],
    // two anchors AND on one axis; and the two keys are TWO axes
    ["ref:abc123 ref:def456", '.filter(refs_to("abc123"), refs_to("def456"))'],
    ["ref:abc123 from:abc123", '.filter(refs_to("abc123"), refs_from("abc123"))'],
    // …and case carries nothing on the call or its kwarg, while the ANCHOR is
    // the one value that is not folded at all
    ["ref:def456?kind=blocked-by", '.filter(REFS_TO("def456", KIND = "Blocked By"))'],
  ],
  trip: [
    "state:*active* -tag:chore state:TODO|DONE",
    "tag:web tag:glance|docs",
    "priority:[#A] tag:book +priority:[#B]",
    "state:*active* -planned:*empty* sort:scheduled",
    'substring:"-x" -state:TODO|DONE',
    "columns:State,Title,owner sort:deadline:desc->title",
    "milk +bread",
    // the relations render back into calls, kind and all — and an ALTERNATION
    // OF ANCHORS rides `raw "…"', one call being about ONE anchor
    "ref:def456?kind=blocked-by from:*any* state:TODO",
    "ref:abc123|def456",
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
    // ONE EDGE, TWO TOKENS: the direction is the whole of the difference
    ["ref:abc123", "from:abc123"],
    // …and so is the kind, the bare form being kind-BLIND and not kindless
    ["ref:def456", "ref:def456?kind=blocked-by"],
    // …and `*any*' is the union where `any' is an id somebody wears
    ["ref:*any*", "ref:any"],
  ],
};

/**
 * THE CORPUS THE THIRD READER IS PROVED ON.
 *
 * `three' is one query in three spellings — the flat string, F's Haskell, g's
 * SQL — and all three IRs have to print the same bytes.  Where a row's three
 * READ differently that is the finding, not a bug to be hidden: the `raw "…"'
 * rows are the shape kwargs cannot say and SQL can, and the date rows are the
 * shift F carries as an opaque literal because it has no date grammar at all.
 *
 * `trip' runs the other way — the flat query rendered INTO a statement and read
 * back, which is the path a `/'-edit takes.  `diff' is the check biting the
 * other way: SQL whose semantics part has to print IRs that part with it.
 */
const IR3_CORPUS = {
  three: [
    // the README's own example, and the boot query
    ["state:*active* -tag:chore", '.filter(state = Active, tag /= "chore")',
     "WHERE state = ACTIVE AND tag NOT LIKE '%chore%'"],
    // the whole statement, all three clauses
    ["state:TODO sort:deadline columns:State,Deadline",
     '.filter(state = "TODO").sort(columns = ["Deadline"]).columns("State", "Deadline")',
     "SELECT state, deadline FROM all WHERE state = 'TODO' ORDER BY deadline"],
    // THE ALTERNATION: a list in F, `IN (…)' in g
    ["state:TODO|DONE", '.filter(state = ["TODO", "DONE"])',
     "WHERE state IN ('TODO', 'DONE')"],
    // …and its negation, which scopes the WHOLE token — De Morgan's own pin
    ["-state:TODO|DONE", '.filter(state /= ["TODO", "DONE"])',
     "WHERE state NOT IN ('TODO', 'DONE')"],
    ["-state:TODO|DONE", '.filter(not (state = ["TODO", "DONE"]))',
     "WHERE NOT (state = 'TODO' OR state = 'DONE')"],
    // THE INTERSECTION: `All' in F, and a repeated column in g, which needs no
    // name at all because SQL can repeat one where record syntax cannot
    ["tag:web tag:glance", '.filter(tag = All ["web", "glance"])',
     "WHERE tag LIKE '%web%' AND tag LIKE '%glance%'"],
    ["tag:web tag:glance", '.filter(tag = All ["web", "glance"])',
     "WHERE tag = 'web' AND tag = 'glance'"],
    // THE METAS, as each surface spells them
    ["tag:*archive*", ".filter(tag = Archive)", "WHERE tag = ARCHIVE"],
    ["state:*inactive*", ".filter(state = Inactive)", "WHERE state = INACTIVE"],
    // …and `*empty*', where SQL already had the word
    ["priority:*empty*", ".filter(priority = Empty)", "WHERE priority IS NULL"],
    ["-priority:*empty*", ".filter(priority /= Empty)", "WHERE priority IS NOT NULL"],
    // THE INFIX LAW, which `LIKE' says out loud where `key:value' leaves it
    // implicit and F's `=' says nothing about at all
    ["title:ship", '.filter(title = "ship")', "WHERE title LIKE '%ship%'"],
    ["title:ship", '.filter(title = "ship")', "WHERE title = 'ship'"],
    // free text shares the `substring' axis, and g spells it as the column it is
    ["milk", '.filter("milk")', "WHERE substring LIKE '%milk%'"],
    // LAW 5'S AGREEMENT HALF: on a bare axis `|' and `+' are one thing
    ["state:TODO +state:DONE", '.filter(state = ["TODO", "DONE"])',
     "WHERE state = 'TODO' OR state = 'DONE'"],
    // …AND ITS PARTING CASE, the one shape F must escape into `raw "…"' and g
    // spells with the parens and the `OR' it already has
    ["tag:web tag:docs +tag:chore", '.filter(raw "tag:web tag:docs +tag:chore")',
     "WHERE (tag = 'web' AND tag = 'docs') OR tag = 'chore'"],
    ["priority:[#A] +priority:[#B] tag:book",
     '.filter(raw "priority:[#A] +priority:[#B]", tag = "book")',
     "WHERE priority IN ('A', 'B') AND tag = 'book'"],
    // CASE CARRIES NOTHING in any of the three
    ["state:*active*", ".FILTER(State = active)", "where STATE = Active"],
    ["sort:deadline", '.SORT(columns = ["deadline"])', "ORDER BY DEADLINE"],
    // THE SHAPING HALVES
    ["sort:deadline:desc->title", '.sort(columns = [Desc "Deadline", "Title"])',
     "ORDER BY deadline DESC, title"],
    ["sort:*none*", ".sort(None)", "ORDER BY NULL"],
    ["columns:State,Deadline", '.columns("State", "Deadline")',
     "SELECT state, deadline"],
    // …and the custom column, which is the app's own law in all three
    ["columns:owner,Title", '.columns("owner", "Title")', "SELECT owner, title"],
    // THE DATES.  F has no date grammar, so the shift travels as an opaque
    // literal — which is exactly what the third reader is for: g READS it.
    ["deadline:<=*today*+30d", '.filter(deadline = "<=*today*+30d")',
     "WHERE deadline <= CURRENT_DATE + INTERVAL '30' DAY"],
    ["deadline:<=2026-09-20", '.filter(deadline = "<=*today*+30d")',
     "WHERE deadline <= CURRENT_DATE + INTERVAL '30' DAY"],
    ["planned:*today*..*today*+30d", '.filter(planned = "*today*..*today*+30d")',
     "WHERE planned BETWEEN CURRENT_DATE AND CURRENT_DATE + INTERVAL '30' DAY"],
    // THE DATASET IS A TAG, so `FROM' is one more term on one more axis
    ["tag:work", '.filter(tag = "work")', "FROM work"],
    ["tag:work|home", '.filter(tag = ["work", "home"])', "FROM work, home"],
    ["tag:work tag:urgent", '.filter(tag = All ["work", "urgent"])',
     "FROM work WHERE tag = 'urgent'"],
    // THE RELATIONS, four-way: the flat token (which two surfaces read), F's
    // call with its kwarg, and g's with the kind by POSITION — SQL has no
    // kwargs, so the word F binds with has nowhere to be written.
    ["ref:def456", '.filter(refs_to("def456"))', "WHERE REFS_TO('def456')"],
    ["from:abc123", '.filter(refs_from("abc123"))', "WHERE REFS_FROM('abc123')"],
    ["ref:def456?kind=blocked-by", '.filter(refs_to("def456", kind = "blocked-by"))',
     "WHERE REFS_TO('def456', 'blocked-by')"],
    // …and the peer's slug is applied by all three
    ['ref:"def456?kind=Blocked By"', '.filter(refs_to("def456", kind = "Blocked By"))',
     "WHERE REFS_TO('def456', 'Blocked By')"],
    // THE EXISTENCE META, as each surface spells a meta
    ["ref:*any*", ".filter(refs_to(Any))", "WHERE REFS_TO(ANY)"],
    ["-from:*any*", ".filter(not (refs_from(Any)))", "WHERE NOT REFS_FROM(ANY)"],
    ["from:*any*?kind=blocked-by", '.filter(refs_from(Any, kind = "blocked-by"))',
     "WHERE REFS_FROM(ANY, 'blocked-by')"],
    // two anchors AND on one axis, where a repeated column is g's own `AND'
    ["ref:abc123 ref:def456", '.filter(refs_to("abc123"), refs_to("def456"))',
     "WHERE REFS_TO('abc123') AND REFS_TO('def456')"],
    // …AND THE ALTERNATION OF ANCHORS, which g's `OR' spells and one call
    // cannot: F escapes into `raw "…"', the same admission the `+' key makes
    ["ref:abc123|def456", '.filter(raw "ref:abc123|def456")',
     "WHERE REFS_TO('abc123') OR REFS_TO('def456')"],
    // case carries nothing on the call, and the ANCHOR is not folded at all
    ["ref:def456?kind=blocked-by", '.filter(REFS_TO("def456", KIND = "Blocked By"))',
     "where refs_to('def456', 'Blocked By')"],
  ],
  trip: [
    "state:*active* -tag:chore state:TODO|DONE",
    "tag:web tag:glance|docs",
    "priority:[#A] tag:book +priority:[#B]",
    "state:*active* -planned:*empty* sort:scheduled",
    "columns:State,Title,owner sort:deadline:desc->title",
    "deadline:<=*today*+30d sort:deadline",
    "tag:web tag:docs +tag:chore",
    "ref:def456?kind=blocked-by -from:*any*",
  ],
  diff: [
    // the alternation is not the intersection
    ["WHERE tag IN ('web', 'docs')", "WHERE tag = 'web' AND tag = 'docs'"],
    // …and negation does not distribute the way a reader might hope
    ["WHERE state NOT IN ('TODO', 'DONE')",
     "WHERE state <> 'TODO' AND state <> 'DONE'"],
    // the order is written order, and a direction is part of the answer
    ["ORDER BY state, title", "ORDER BY title, state"],
    ["ORDER BY deadline", "ORDER BY deadline DESC"],
    // `SELECT' is not narrowing
    ["SELECT state", "SELECT deadline"],
    // …AND THE STAR IS SEVEN WHERE THE DEFAULT IS SIX: `closed' is the
    // difference, so the two cannot be one spelling
    ["SELECT *", "SELECT state, priority, title, scheduled, deadline, tags"],
    // the granularity cut is the interval's end and not its start
    ["WHERE deadline < CURRENT_DATE", "WHERE deadline <= CURRENT_DATE"],
    // …and a dataset narrows where its alias does not
    ["FROM work", "FROM all"],
    // ONE EDGE, TWO TOKENS: the direction, the kind, and the meta against an
    // id somebody actually wears
    ["WHERE REFS_TO('abc123')", "WHERE REFS_FROM('abc123')"],
    ["WHERE REFS_TO('def456')", "WHERE REFS_TO('def456', 'blocked-by')"],
    ["WHERE REFS_TO(ANY)", "WHERE REFS_TO('any')"],
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

/** G's twin of it, and the same claim in a surface with no parens: the caret
 *  lands AFTER the clause keyword, inside the clause's own contents.  A clause
 *  ends where the next one begins, so there is no closer to be inside of —
 *  which is exactly why `PARENS' is declared a departure and this stands in its
 *  place. */
const caretInClause = () => {
  const st = document.querySelector("#app .cx .cx-live");
  if (!st) return { ok: false, why: "no live stage" };
  const kw = st.querySelector(".cx-head");
  const caret = st.querySelector(".cx-caret");
  const args = st.querySelector(".cx-args");
  if (!kw) return { ok: false, why: "no keyword head" };
  if (!caret) return { ok: false, why: "no caret" };
  if (st.querySelectorAll(".cx-par").length)
    return { ok: false, why: "a paren was drawn" };
  const after = (a, b) =>
    (a.compareDocumentPosition(b) & Node.DOCUMENT_POSITION_FOLLOWING) !== 0;
  if (!after(kw, caret)) return { ok: false, why: "caret before the keyword" };
  if (!(kw.getBoundingClientRect().right <= caret.getBoundingClientRect().left + 1.5))
    return { ok: false, why: "caret is drawn inside the keyword" };
  const c = RIG.caret();
  return { ok: true, where: RIG.cx().where, fn: RIG.cx().stages.slice(-1)[0].fn,
           kw: kw.textContent, args: args ? args.textContent : "",
           at: c ? c.at : -1, len: c ? c.len : -1 };
};

// THE PROBES THE TYPED RUNGS READ THROUGH.  SLOT and DONE genuinely DEPART per
// dialect — the quotes differ, the operator differs, what finishes a term
// differs — and they rightly have two spellings apiece.  The machinery
// underneath does not: these three ask `RIG' the same questions whichever
// dialect is answering, so they are declared once, here with the other shared
// probes, rather than once per dialect.
/** The live stage mid-edit: what it holds, where the caret is, what stands. */
const held = () => ({ args: (RIG.cx().stages.slice(-1)[0] || {}).args,
                      at: (RIG.caret() || {}).at, menu: RIG.menu().open,
                      items: RIG.menu().items.slice(0, 4), c: RIG.composed() });
/** The same, for DONE, which asks only whether the offers are down. */
const stood = () => ({ args: (RIG.cx().stages.slice(-1)[0] || {}).args,
                       at: (RIG.caret() || {}).at, menu: RIG.menu().open,
                       lead: RIG.menu().items[0] });
/** Where a committed stage LANDS: the query, the strip, the door, the rows. */
const landed = () => ({ q: RIG.query(), pills: RIG.pills(), door: RIG.door(),
                        rows: RIG.rows() });
/** The model half of the WHOLE picture, which ABANDON asks to come back. */
const whole = () => ({ q: RIG.query(), pills: RIG.pills(), door: RIG.door(),
                       chips: RIG.chips(), stages: RIG.cx().stages });
/** A stage mid-edit, as ABANDON reads it on the way in. */
const midEdit = () => ({ args: (RIG.cx().stages.slice(-1)[0] || {}).args,
                         at: (RIG.caret() || {}).at, menu: RIG.menu().open });

const eq = (a, b) => JSON.stringify(a) === JSON.stringify(b);
const chars = (s) => [...s];
// The copied driver's key map carries the rungs the fold-marks spike needed and
// no DELETE; the WebDriver codepoint is spelled here rather than in the driver,
// which stays the copy it is.
const DEL = "";

let failed = 0;
const sigs = {};
// …and whether that tab is one of the four that KEEP the flat door, so the
// control at the foot can hold the departed ones to the opposite promise.
const kept = {};
const ff = await firefox().catch((e) => {
  console.error("no firefox: " + e.message);
  process.exit(2);
});

// ======================================================== THE LAW PASS
// WHAT THE API ANSWERS, ASKED ONCE PER DIALECT AND NOT ONCE PER TAB.  A stage
// string is the RIG's law and not a tab's: `stageString' reads the `DIALECT'
// table and nothing else, so the flat table takes the same branch on all five
// flat tabs and asking it five times says nothing the first asking did not.
//
// AND A RED LINE THEN NAMES WHICH HALF BROKE.  Welded together, "the rig's law
// moved" and "this tab's keys moved" arrived as one string; split, a red LAW-*
// is the law and a red COMMA, CASE or WARN is the keystrokes.  No coverage
// goes with the split — no variant departs any of these laws, which is why
// they were the same twelve on five tabs in the first place.

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
  // THE RELATION KEYS ARE ORDINARY TOKENS IN THE FLAT STRING, which is the
  // whole of what the typed surfaces are a view of: the `?kind=' rides inside
  // the value, and the long spelling is quoted whole because the scanner cuts
  // on the space.
  r_ref: ["filter", "ref:def456, from:abc123", "ref:def456 from:abc123"],
  r_kind: ["filter", "ref:def456?kind=blocked-by", "ref:def456?kind=blocked-by"],
  r_any: ["filter", "ref:*any*, -from:*any*", "ref:*any* -from:*any*"],
  r_quoted: ["filter", 'ref:"def456?kind=Blocked By"', 'ref:"def456?kind=Blocked By"'],
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
  // THE RELATIONS ARE CALLS AND NOT FIELDS, because the kind cannot live
  // inside the quotes: a kwarg BINDS and a positional is what the predicate is
  // ABOUT, so the id is positional and the kind binds.
  r_to: ["filter", 'refs_to("def456")', "ref:def456"],
  r_from: ["filter", 'refs_from("abc123")', "from:abc123"],
  r_kind: ["filter", 'refs_to("def456", kind = "blocked-by")',
           "ref:def456?kind=blocked-by"],
  // …and the slug is the peer's, applied on the WRITE as well as the read
  r_slug: ["filter", 'refs_to("def456", kind = "Blocked By")',
           "ref:def456?kind=blocked-by"],
  // `Any' IS THE `*any*' META'S CONSTRUCTOR — the word the list wrapper used to
  // wear.  One name, one arity, and the metas' law had the better claim.
  r_any: ["filter", "refs_to(Any)", "ref:*any*"],
  r_any_kind: ["filter", 'refs_from(Any, kind = "blocked-by")',
               "from:*any*?kind=blocked-by"],
  // the negation is the WRAPPER, there being no operator on a call to flip
  r_not: ["filter", "not (refs_from(Any))", "-from:*any*"],
  // two anchors AND on one axis, and the two keys are two axes
  r_two: ["filter", 'refs_to("abc123"), refs_from("abc123")',
          "ref:abc123 from:abc123"],
  // …AND THE FIELD SPELLING COMPOSES NOTHING.  `ref:x' is a real token of the
  // grammar, so writing one out of a spelling the surface marks would be
  // inventing the reader's query — where an unknown FIELD composes the free
  // text it meant anyway.
  r_field: ["filter", 'ref = "def456"', ""],
  // case carries nothing on the call or its kwarg; the ANCHOR is not folded
  r_case: ["filter", 'REFS_TO("def456", KIND = "Blocked By")',
           "ref:def456?kind=blocked-by"],
};
// G'S SEPARATOR IS `AND', which is the same law asked of a language that
// spells its conjunction as a WORD: the comma stays where SQL puts one — the
// `SELECT' list, the `ORDER BY' chain, an `IN' list — and never between two
// predicates.
const SQL_LAW = {
  f_and: ["filter", "state = 'TODO' AND tag = 'web'", "state:TODO tag:web"],
  f_and_case: ["filter", "state = 'TODO' and tag = 'web'", "state:TODO tag:web"],
  f_space: ["filter", "state='TODO'   AND   tag='web'", "state:TODO tag:web"],
  // A comma inside a VALUE needs no quoting in the flat string.
  f_value: ["filter", "tag = 'a,b'", "tag:a,b"],
  f_quoted: ["filter", "title LIKE '%a, b%' AND tag = 'web'", 'title:"a, b" tag:web'],
  // `IN (…)' IS THE ALTERNATION, which is the naturalness g is here for.
  f_in: ["filter", "state IN ('TODO', 'DONE')", "state:TODO|DONE"],
  f_not_in: ["filter", "state NOT IN ('TODO', 'DONE')", "-state:TODO|DONE"],
  // …AND THE INTERSECTION NEEDS NO NAME: a repeated column IS the AND, where
  // record syntax could not repeat a field and had to invent `All'.
  f_all: ["filter", "tag = 'web' AND tag = 'glance'", "tag:web tag:glance"],
  f_neq: ["filter", "tag <> 'chore'", "-tag:chore"],
  f_bang: ["filter", "tag != 'chore'", "-tag:chore"],
  f_not: ["filter", "NOT (tag = 'chore')", "-tag:chore"],
  f_ctor: ["filter", "state = ACTIVE AND tag <> ARCHIVE", "state:*active* -tag:*archive*"],
  // SQL ALREADY HAS A WORD FOR THE EMPTY CELL.
  f_null: ["filter", "priority IS NULL", "priority:*empty*"],
  f_not_null: ["filter", "priority IS NOT NULL", "-priority:*empty*"],
  // THE ONE-AXIS `OR' IS THE ADDITIVE LAW, and its parenthesised form is the
  // shape F could not spell at all — law 5's parting case, said in SQL.
  f_or: ["filter", "state = 'TODO' OR state = 'DONE'", "state:TODO|DONE"],
  f_or_base: ["filter", "(tag = 'web' AND tag = 'docs') OR tag = 'chore'",
              "tag:web tag:docs +tag:chore"],
  f_or_neg: ["filter", "(tag = 'web' AND tag <> 'docs') OR tag = 'chore'",
             "tag:web -tag:docs +tag:chore"],
  // …AND A CROSS-AXIS ONE COMPOSES NOTHING AT ALL.
  f_cross: ["filter", "state = 'TODO' OR tag = 'web'", ""],
  // LIKE'S WILDCARDS NAME THE KEY'S OWN TEST, and are refused where they ask
  // for a test the flat grammar does not have.
  f_like: ["filter", "title LIKE '%ship%'", "title:ship"],
  f_like_pre: ["filter", "deadline LIKE '2026-08%'", "deadline:2026-08"],
  f_like_bad: ["filter", "state LIKE '%TODO%'", ""],
  f_like_end: ["filter", "title LIKE '%ship'", ""],
  // THE DATES: `CURRENT_DATE' is the day, the interval is the shift.
  f_today: ["filter", "deadline = CURRENT_DATE", "deadline:*today*"],
  f_shift: ["filter", "deadline <= CURRENT_DATE + INTERVAL '30' DAY",
            "deadline:<=*today*+30d"],
  f_back: ["filter", "scheduled >= CURRENT_DATE - INTERVAL '1' WEEK",
           "scheduled:>=*today*-1w"],
  f_between: ["filter", "planned BETWEEN CURRENT_DATE AND CURRENT_DATE + INTERVAL '30' DAY",
              "planned:*today*..*today*+30d"],
  f_cmp_bad: ["filter", "title > 'x'", ""],
  // `ORDER BY': bare names, the comma is the arrow, `NULL' is document order.
  s_list: ["sort", "state, title", "sort:state->title"],
  s_desc: ["sort", "deadline DESC, title", "sort:deadline:desc->title"],
  s_asc: ["sort", "title ASC", "sort:title"],
  s_head: ["sort", "tags, priority", "sort:tag->priority"],
  s_null: ["sort", "NULL", "sort:*none*"],
  s_unknown: ["sort", "owner", ""],
  // `SELECT': bare identifiers, `*' the seven, a literal no column at all.
  c_list: ["columns", "state, deadline", "columns:state,deadline"],
  c_star: ["columns", "*", "columns:State,#,Title,Scheduled,Deadline,Closed,Tags"],
  c_custom: ["columns", "owner, title", "columns:owner,title"],
  c_ident: ["columns", '"ship date", title', "columns:ship date,title"],
  c_literal: ["columns", "'state'", ""],
  // `FROM': the dataset is a TAG, the comma is a union, the aliases are all.
  t_one: ["from", "work", "tag:work"],
  t_union: ["from", "work, home", "tag:work|home"],
  t_star: ["from", "*", ""],
  t_all: ["from", "all", ""],
  t_default: ["from", "default", ""],
  t_unknown: ["from", "nosuch", "tag:nosuch"],
  // THE RELATIONS ARE BOOLEAN FUNCTIONS IN `WHERE', which is SQL's own shape
  // for a predicate that is not a column comparison — and the kind is the
  // SECOND POSITIONAL, SQL having no kwargs for F's word to be written in.
  r_to: ["filter", "REFS_TO('def456')", "ref:def456"],
  r_from: ["filter", "REFS_FROM('abc123')", "from:abc123"],
  r_kind: ["filter", "REFS_TO('def456', 'blocked-by')", "ref:def456?kind=blocked-by"],
  r_slug: ["filter", "REFS_TO('def456', 'Blocked By')", "ref:def456?kind=blocked-by"],
  r_any: ["filter", "REFS_TO(ANY)", "ref:*any*"],
  r_any_kind: ["filter", "REFS_FROM(ANY, 'blocked-by')", "from:*any*?kind=blocked-by"],
  r_not: ["filter", "NOT REFS_FROM(ANY)", "-from:*any*"],
  r_case: ["filter", "refs_to('def456', 'blocked-by')", "ref:def456?kind=blocked-by"],
  // the axis law over the new keys: two anchors AND, an `OR' within one axis
  // is the alternation, and across two of them there is no flat spelling
  r_and: ["filter", "REFS_TO('abc123') AND REFS_TO('def456')", "ref:abc123 ref:def456"],
  r_or: ["filter", "REFS_TO('abc123') OR REFS_TO('def456')", "ref:abc123|def456"],
  r_cross: ["filter", "REFS_TO('abc123') OR tag = 'web'", ""],
  // …and `ref' as a COLUMN is an error, the `WHERE' namespace being closed
  r_col: ["filter", "ref = 'def456'", ""],
  // THE CLOSURE IS PROPOSED AND UNSHIPPED, so the keyword that asks for it
  // composes nothing and says where the walk lives.
  r_closure: ["filter", "WITH RECURSIVE reach AS (SELECT 1)", ""],
};
/** A control at the RUN's level, the way `/' is: no tab owns it, so no tab can
 *  declare it a miss or a departure. */
const control = (name, ok, why, say) => {
  if (ok) { console.log(`ok   ${name}` + (say ? `  (${say})` : "")); return; }
  failed += 1;
  console.log(`FAIL ${name}  ${why}`);
};

/** One dialect's law table, asked of the API on a page of that dialect, plus
 *  whatever else that dialect answers without a keystroke. */
const lawPass = async (name, page, table, extra) => {
  await ff.goto(pathToFileURL(join(HERE, page)).href);
  const got = await ff.eval((spec) => {
    const out = {};
    for (const k of Object.keys(spec)) out[k] = RIG.stageString(spec[k][0], spec[k][1]);
    return out;
  }, table);
  const off = Object.keys(table).filter((k) => got[k] !== table[k][2]);
  const more = extra ? await extra() : { ok: true, why: "" };
  const n = Object.keys(table).length;
  control(name, off.length === 0 && more.ok,
          (off.length ? off.map((k) => `${k}=${JSON.stringify(got[k])}`).join(" ") : "")
          + (off.length && !more.ok ? " · " : "") + (more.ok ? "" : more.why),
          `${n} spellings, one string each`);
};

/**
 * THE WARNING'S LAW, over FLAT queries, where the reading lives.  `unsat' is
 * exported on every page and is the SAME function for every dialect — the
 * sentence spells its values in F's idiom, which G inherits along with the law
 * — so it is asked here once rather than from inside one dialect's WARN.
 */
const unsatLaw = async () => {
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
    // THE NEW AXES INHERIT THE READING WHOLE.  Required-and-refused reads them
    // the way it reads any axis; the SINGLE-CELL rule does not read them at
    // all, a row sitting on as many edges as its subtree carries.
    rel: RIG.unsat("ref:abc123 -ref:abc123"),
    relTwo: RIG.unsat("ref:abc123 ref:def456").said,
    relAny: RIG.unsat("ref:*any* -ref:*any*").said,
    // …and the sentence is read off the ATOM, so one edge written two ways is
    // one contradiction and says it once
    relKind: RIG.unsat('ref:"def456?kind=Blocked By" -ref:def456?kind=blocked-by').said,
    // …while a KINDED refusal does not kill the kind-blind requirement: a
    // plain mention answers both
    relLoose: RIG.unsat("ref:def456 -ref:def456?kind=blocked-by").said,
    // …and the two keys are two axes, so neither reads the other's atoms
    relAxes: RIG.unsat("ref:abc123 -from:abc123").said,
  }), BOOT_FILTER);
  return { ok: eq(law.pair.said, [SAID])
             && eq(law.pair.tokens, ["tag:chore", "-tag:chore"])
             && eq(law.near.said, []) && eq(law.near.tokens, [])
             && eq(law.single,
                   ['state: "TODO" and "DONE" are both required — no row is both'])
             && eq(law.tags, []) && eq(law.meta, []) && eq(law.nested, [])
             && law.apart.length === 1 && eq(law.text, []) && eq(law.alt, [])
             && eq(law.altN, []) && eq(law.wide, []) && eq(law.boot, [])
             && eq(law.rel.said, [REL_SAID('refs_to("abc123")')])
             && eq(law.rel.tokens, ["ref:abc123", "-ref:abc123"])
             && eq(law.relTwo, []) && eq(law.relAny, [REL_SAID("refs_to(Any)")])
             && eq(law.relKind,
                   [REL_SAID('refs_to("def456", kind = "blocked-by")')])
             && eq(law.relLoose, []) && eq(law.relAxes, []),
           why: `unsat=${JSON.stringify(law)}` };
};

/**
 * THE STORE'S OWN GRAPH, asked of the API — the RIG's law and no tab's, so it
 * sits in the LAW pass beside the warning's.  Six rows, four resolved edges,
 * and every law `docs/query.md' states about a reference read off THIS fixture
 * rather than described: one edge from its two ends, the kind test, the union
 * over the anchor, and the two links that are no reference at all.
 */
const relStoreLaw = async () => {
  const g = await ff.eval(() => {
    const rows = (q) => RIG.served(q).rows.map((r) => r.title);
    return {
      edges: RIG.edges().map((e) => e.from + ">" + e.to + "/" + e.kind),
      kinds: RIG.kinds("ref", "def456"),
      // ONE EDGE, TWO TOKENS: `ref' from the target's end, `from' from the
      // source's, and neither serves the row it was asked about.
      ref: rows("ref:def456"), from: rows("from:abc123"),
      // the kind test — and the bare form stays KIND-BLIND, so a plain mention
      // answers it and answers no kind at all
      kind: rows("ref:def456?kind=blocked-by"),
      other: rows("ref:def456?kind=see-also"),
      plain: rows("ref:mno345"), plainKind: rows("ref:mno345?kind=blocked-by"),
      // the long spelling, quoted whole because the scanner cuts on the space
      quoted: rows('ref:"def456?kind=Blocked By"'),
      // `*any*' IS THE UNION OVER THE ANCHOR, and its negation is the orphans
      any: rows("ref:*any*"), fromAny: rows("from:*any*"),
      orphans: rows("-from:*any*"),
      anyKind: rows("ref:*any*?kind=blocked-by"),
      fromAnyKind: rows("from:*any*?kind=blocked-by"),
      // THE TWO NEGATIVE LAWS, on the fixture: a link back to the row it was
      // written in is no reference, and one naming no row is none either.
      self: rows("ref:pqr678"), dangling: rows("ref:zzz999"),
      // the anchor is the ONE VALUE NOT CASE-FOLDED, so the star is lower or
      // it is an id nobody wears — and the bare word is an ordinary id
      upper: rows("ref:*ANY*"), bare: rows("ref:any"),
      // …and the BOUNDED CLOSURE is unshipped, so its mark is part of an
      // anchor no row claims
      depth: rows("ref:abc123..3"),
      // THE CUT IS TAKEN ONLY WHERE A `kind=' COMES OUT OF THE `?': an id
      // carrying one that declares no kind stays WHOLE and resolves to the row
      // it always did, and it is the RIGHT `?' that cuts and not the first.
      question: [RIG.anchor("abc?123"), RIG.anchor("abc?kind=Blocked By"),
                 RIG.anchor("abc?x=1?kind=see-also")],
      // THE TWO KEYS ARE TWO AXES, and the axes AND: the row that points at
      // `mno345' and is pointed at by `abc123' is `Write the release notes'.
      both: rows("ref:mno345 from:abc123"),
      // …while two anchors on ONE axis AND too, and nobody points at both
      oneAxis: rows("ref:abc123 ref:def456"),
    };
  });
  const SHIP = "Ship the dot chain", PORT = "Port table-view v1.2 notes";
  const WRITE = "Write the release notes", DROP = "Drop the ?order= parameter";
  return { ok: eq(g.edges, ["abc123>def456/blocked-by", "abc123>ghi789/see-also",
                            "ghi789>mno345/null", "pqr678>def456/blocked-by"])
             && eq(g.kinds, ["blocked-by"])
             && eq(g.ref, [SHIP, DROP]) && eq(g.from, [WRITE, PORT])
             && eq(g.kind, [SHIP, DROP]) && eq(g.other, [])
             && eq(g.plain, [WRITE]) && eq(g.plainKind, [])
             && eq(g.quoted, [SHIP, DROP])
             && eq(g.any, [SHIP, WRITE, DROP])
             && eq(g.fromAny, [WRITE, PORT, "Read the org-mode manual"])
             && eq(g.orphans, [SHIP, "Rename the query keys", DROP])
             && eq(g.anyKind, [SHIP, DROP]) && eq(g.fromAnyKind, [PORT])
             && eq(g.self, []) && eq(g.dangling, [])
             && eq(g.upper, []) && eq(g.bare, []) && eq(g.depth, [])
             && eq(g.both, [WRITE]) && eq(g.oneAxis, [])
             && eq(g.question, [{ id: "abc?123", kind: null },
                                { id: "abc", kind: "blocked-by" },
                                { id: "abc?x=1", kind: "see-also" }]),
           why: `store=${JSON.stringify(g)}` };
};

/** Two laws under one `LAW-*' line: the pass takes one extra and neither of
 *  these belongs inside the other. */
const both = (...fns) => async () => {
  for (const f of fns) {
    const r = await f();
    if (!r.ok) return r;
  }
  return { ok: true, why: "" };
};

/** F'S CASE LAW, asked of the API: any case parses, the accept canonicalises,
 *  and a bare name nothing answers to is left as written and composes nothing. */
const dslCaseLaw = async () => {
  const api = await ff.eval(() => ({
    canon: RIG.dslCanon('STATE = ACTIVE, tAg /= "Chore"', "filter"),
    left: RIG.dslCanon("state = chore", "filter"),
    errs: RIG.dslErrors("filter", "state = chore").length,
    composes: RIG.stageString("filter", "state = chore"),
    variant: RIG.stageString("filter", "STATE = ACTIVE"),
    // …AND A RELATION SPELLED AS A FIELD IS MARKED AND COMPOSES NOTHING, where
    // an unknown field composes the free text it meant anyway: `ref:x' is a
    // real token, so writing one would invent the reader's query.
    relField: [RIG.dslErrors("filter", 'ref = "def456"').length,
               RIG.stageString("filter", 'ref = "def456"')],
    // …and the call's own arity is what the offers and the silence read: the
    // call is owed an argument, the meta's constructor stands alone.
    relArity: [RIG.dslErrors("filter", "refs_to()").length,
               RIG.stageString("filter", "refs_to()")],
  }));
  return { ok: api.canon === 'state = Active, tag /= "Chore"'
             && api.left === "state = chore" && api.errs === 1
             && api.composes === "" && api.variant === "state:*active*"
             && eq(api.relField, [1, ""]) && eq(api.relArity, [1, ""]),
           why: `case=${JSON.stringify(api)}` };
};

/** G'S CASE LAW, in SQL's own convention: the language upper, the columns it
 *  knows lower, and `SELECT' left as written because there the spelling IS the
 *  header — with the one thing `WHERE''s closed namespace does not share, the
 *  OPEN one under `FROM'. */
const sqlApiLaw = async () => {
  const api = await ff.eval(() => ({
    canon: RIG.sqlCanon("STATE = active and TAG Not Like '%x%'", "filter"),
    kept: RIG.sqlCanon("Ship Date, TITLE", "columns"),
    lower: RIG.stageString("filter", "state = active and tag <> 'chore'"),
    upper: RIG.stageString("filter", "STATE = ACTIVE AND TAG <> 'chore'"),
    mixed: RIG.stageString("sort", "Deadline desc, TITLE"),
    // …and a column nothing answers to is left exactly as written, MARKED, and
    // composes nothing: the WHERE namespace is the closed one.
    unknown: [RIG.sqlErrors("filter", "owner = 'x'").length,
              RIG.stageString("filter", "owner = 'x'")],
    // …while the DATASET namespace is OPEN, so `FROM' can name nothing wrong:
    // an unworn tag composes and serves nothing, which is the flat grammar's
    // own answer and not an error the surface may invent.  The `0' is that law
    // and not a field nobody sets.
    open: RIG.sqlErrors("from", "nosuch").length,
    // …AND THE RELATION CALLS ARE KEYWORDS, so the formatter upper-cases them
    // and the two literals inside are left alone — the anchor is the one value
    // that is not folded anywhere.
    relCanon: RIG.sqlCanon("refs_to('def456', 'blocked-by')", "filter"),
    relCol: [RIG.sqlErrors("filter", "ref = 'def456'").length,
             RIG.stageString("filter", "ref = 'def456'")],
  }));
  return { ok: api.canon === "state = ACTIVE AND tag NOT LIKE '%x%'"
             && api.kept === "Ship Date, TITLE"
             && api.lower === "state:*active* -tag:chore"
             && api.upper === api.lower
             && api.mixed === "sort:deadline:desc->title"
             && api.unknown[0] === 1 && api.unknown[1] === ""
             && api.open === 0
             && api.relCanon === "REFS_TO('def456', 'blocked-by')"
             && eq(api.relCol, [1, ""]),
           why: `case=${JSON.stringify(api)}` };
};

// A DIALECT IS ASKED WHERE IT IS PICKED: running one tab runs the law of that
// tab's dialect and no other, so `node check.mjs g-sql.html' still says what
// g's own spellings owe.
const FLAT_PAGE = picked.find((p) => p !== TYPED && p !== SQL);
if (FLAT_PAGE) await lawPass("LAW-FLAT", FLAT_PAGE, FLAT_LAW,
                             both(unsatLaw, relStoreLaw));
if (picked.includes(TYPED)) await lawPass("LAW-DSL", TYPED, TYPED_LAW, dslCaseLaw);
if (picked.includes(SQL)) await lawPass("LAW-SQL", SQL, SQL_LAW, sqlApiLaw);

for (const page of picked) {
  const bad = [], surprised = [];
  const known = MISSES[page] || [];
  const departs = DEPARTS[page] || [];
  const url = pathToFileURL(join(HERE, page)).href;
  // THE SAME RUNGS, SPELLED IN THE TAB'S OWN DIALECT.  What is asserted is the
  // flat string every one of them composes; what differs is what is TYPED.
  const typed = page === TYPED;
  const sqlish = page === SQL;
  // THREE DIALECTS, ONE SET OF RUNGS: what is asserted is the flat string every
  // one of them composes, and what differs is what a reader TYPES.
  const pick = (flat, dsl, sql) => (sqlish ? sql : typed ? dsl : flat);
  const SQL_FILTER = "state = ACTIVE AND tag NOT LIKE '%chore%'";
  const FILTER_TEXT = pick(BOOT_FILTER, 'state = Active, tag /= "chore"', SQL_FILTER);
  // WHAT A READER TYPES, not what stands afterwards: the opened slot supplies
  // the quotes and spends the opening one, so the value goes in bare and the
  // closing quote is stepped over.
  const CHAIN_ARG = pick("state:TODO", 'state = TODO"', "state = TODO'");
  // `/' IS THE ADD-A-CONDITION GESTURE in both typed dialects: it lands on a
  // FRESH argument with the separator already appended — a comma in F, `AND' in
  // g, each dialect's own — so what is typed is the condition alone.  An empty
  // stage gets no separator, there being nothing to follow.
  const ADD_ARG = pick(" +tag:docs", 'tag = docs"', "tag = docs'");
  const REOPEN_TEXT = pick(FILTER_TEXT, FILTER_TEXT + ", ", FILTER_TEXT + " AND ");
  const REOPEN_LEAD = pick(null, 'state = "…"', "state");
  const FRESH_LEAD = pick("state:", 'state = "…"', "state");
  const ADD_TOKEN = pick("+tag:docs", "tag:docs", "tag:docs");
  const DRY_HALF = pick("state:", 'state = ""', "state");
  // …and in g the OPERATOR is what wakes them: a column finishes no term, so
  // the offers over it are the ten operators, and the one the reader types
  // opens the slot the value goes in.
  const DRY_NEXT = pick("TO", "A", " =");
  const DRY_FULL = pick("state:TODO", "state = Active", "state = ACTIVE");
  // A KEY ACCEPT THAT OPENS A SLOT IS MID-CONSTRUCTION, so its value offers
  // stand at once; the flat dialect has no slot and its key accept is done.  In
  // g the COLUMN accept opens no slot and still asks again — SQL has ten
  // operators where record syntax has one, so the choice is the reader's.
  const DRY_MENU = typed || sqlish;
  const COLS_ARG = pick("State,Deadline", 'State", Deadline"', "state, deadline");
  const COLS_TOKEN = pick("columns:State,Deadline", "columns:State,Deadline",
                          "columns:state,deadline");
  // `.sort(' in F wants the `columns' kwarg first, which the offers spell: one
  // TAB takes `columns = [""]' with the caret in the slot.  G's `ORDER BY'
  // takes a bare name and nothing else.
  const SORT_KEYS = typed ? [[KEY.Tab], chars('Deadline"')] : [chars("deadline")];
  const SORT2_KEYS = typed ? [[KEY.Tab], chars('State", Title"')]
                           : sqlish ? [chars("state, title")]
                           : [chars("state, title")];
  const SORT_PILL = pick("sort(deadline)", 'sort(columns = ["Deadline"])',
                         "sort(deadline)");
  // A CLAUSE IS OPENED BY ITS KEYWORD, so g names the one it wants where the
  // other tabs take whatever the offers lead with.
  const OPEN_FILTER = sqlish ? [["."], ["w"], [KEY.Tab]] : [["."], [KEY.Tab]];
  const OPEN_SORT = sqlish ? [["."], ["o"], [KEY.Tab]] : [["."], ["s"], [KEY.Tab]];
  const OPEN_COLS = sqlish ? [["."], ["s"], [KEY.Tab]] : [["."], ["c"], [KEY.Tab]];
  // …and closed by `;' where F closes with `)': SQL ends a STATEMENT with one
  // and a clause with nothing at all, so this is the one key g spends on a
  // gesture SQL does not have.
  const CLOSE = sqlish ? ";" : ")";
  const drive = async (batches) => { for (const b of batches) await ff.keys(b); };
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
  if (!departs.includes("DOT"))
    want("DOT", dot.door === "compose" && dot.dots === 1
         && eq(dot.menu.items, ["filter", "sort", "columns"]),
         `door=${dot.door} dots=${dot.dots} offers=${JSON.stringify(dot.menu.items)}`);

  if (sqlish) {
    // ---- KEYWORD: G IS THE TAB WHERE `.' OPENS SOMETHING WITH NO DOT IN IT.
    // The key is the door — the spike's question was always what `.' opens —
    // and what it opens here is a STATEMENT, whose four clauses are the offers.
    want("KEYWORD", dot.door === "compose" && dot.dots === 0
         && eq(dot.menu.items, ["SELECT", "FROM", "WHERE", "ORDER BY"]),
         `door=${dot.door} dots=${dot.dots} offers=${JSON.stringify(dot.menu.items)}`);
    // ---- HEAD: the taken clause draws its KEYWORD and the caret lands after
    // it, in DOM order and on the screen — which is what PARENS asserts for a
    // call, asked of a surface that has no parens to assert it with.
    await ff.keys(["w"]);
    await ff.keys([KEY.Tab]);
    const head = await ff.eval(() => {
      const st = document.querySelector("#app .cx .cx-live");
      if (!st) return { ok: false, why: "no live stage" };
      const kw = st.querySelector(".cx-head");
      const caret = st.querySelector(".cx-caret");
      const args = st.querySelector(".cx-args");
      if (!kw) return { ok: false, why: "no keyword head" };
      if (!caret) return { ok: false, why: "no caret" };
      if (st.querySelectorAll(".cx-par").length)
        return { ok: false, why: "a paren was drawn" };
      const after = (a, b) =>
        (a.compareDocumentPosition(b) & Node.DOCUMENT_POSITION_FOLLOWING) !== 0;
      if (!after(kw, caret)) return { ok: false, why: "caret before the keyword" };
      if (!(kw.getBoundingClientRect().right
            <= caret.getBoundingClientRect().left + 1.5))
        return { ok: false, why: "caret is drawn inside the keyword" };
      return { ok: true, kw: kw.textContent, where: RIG.cx().where,
               fn: RIG.cx().stages.slice(-1)[0].fn,
               args: args ? args.textContent : "", menu: RIG.menu().items[0] };
    });
    want("HEAD", head.ok === true && head.kw === "WHERE" && head.where === "args"
         && head.fn === "filter" && head.args === "" && head.menu === "state",
         head.why || `kw=${head.kw} where=${head.where} fn=${head.fn} `
         + `args=${JSON.stringify(head.args)} lead=${head.menu}`);
  }

  // ---- PARENS: the taken call opens them and the caret lands inside
  if (!departs.includes("PARENS")) {
    await ff.keys([KEY.Tab]);
    const par = await ff.eval(caretInParens);
    want("PARENS", par.ok === true && par.where === "args" && par.fn === "filter",
         par.why || `where=${par.where} fn=${par.fn}`);
  }

  // ---- CHAIN: the scripted sequence composes the grammar's own flat string
  await ff.goto(url);
  await drive(OPEN_FILTER);
  await ff.keys(chars(CHAIN_ARG));
  if (sqlish) {
    // THE KEYWORD IS THE GESTURE: no `)' and no `.', just the word that ends
    // one clause by beginning the next.
    await ff.keys(chars(" ORDER BY "));
  } else {
    await ff.keys([")", ".", "s"]);
    await ff.keys([KEY.Tab]);
  }
  for (const batch of SORT_KEYS) await ff.keys(batch);
  if (!sqlish) await ff.keys([")"]);
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

  // ---- COMMA: the argument separator, per stage — DRIVEN, since a law nothing
  // types is a law about nothing.  The law itself is the rig's and not this
  // tab's, so it is asked once per dialect in the LAW pass above; what is left
  // here is the keystrokes, which are each tab's own.
  await ff.goto(url);
  await drive(OPEN_FILTER);
  await ff.keys(chars(sqlish ? "state = TODO' AND tag = web'"
                     : typed ? 'state = TODO", tag = web"' : "state:TODO, tag:web"));
  if (sqlish) await ff.keys(chars(" ORDER BY "));
  else {
    await ff.keys([")", ".", "s"]);
    await ff.keys([KEY.Tab]);
  }
  for (const batch of SORT2_KEYS) await ff.keys(batch);
  if (!sqlish) await ff.keys([")"]);
  const drove = await ff.eval(() => RIG.composed());
  want("COMMA", drove === "state:TODO tag:web sort:state->title",
       `drove=${JSON.stringify(drove)}`);

  // ---- REL: THE RELATION PREDICATES, driven through each surface's own
  // offers.  One edge — `Ship the dot chain' is blocked by `Port table-view' —
  // and three spellings of it: the flat token with the `?kind=' inside its
  // value, F's call with the kind as a KWARG, g's with the kind by POSITION.
  // Every one composes the same flat token, and the drive goes through the
  // OFFERS because both rosters are new: the ids the store wears, and the kinds
  // THAT anchor's own edges carry.
  const REL_DRIVE = pick(
    // the key, the anchor out of the offers, then the `?' that opens the kind
    [chars("ref:def"), [KEY.Tab], chars("?"), [KEY.Tab]],
    // the call, its anchor SLOT, then the `kind' kwarg and its own slot — two
    // TABs, because the kwarg and its value are two positions
    [chars("refs_"), [KEY.Tab], chars("def456"), [KEY.Tab], chars(", "),
     [KEY.Tab], [KEY.Tab]],
    // …and in g the COMMA opens the second positional's slot the way an `IN'
    // list's comma does, so there is no kwarg to take first
    [chars("refs_"), [KEY.Tab], chars("def456"), [KEY.Tab], chars(","),
     chars("blo"), [KEY.Tab]]);
  // …AND THE EXISTENCE META, spelled as the constructor every meta wears.  In
  // F that word is `Any', which the list wrapper used to have.
  const ANY_DRIVE = pick([chars("ref:*a"), [KEY.Tab]],
                         [chars("refs_"), [KEY.Tab], chars("A"), [KEY.Tab]],
                         [chars("refs_"), [KEY.Tab], chars("A"), [KEY.Tab]]);
  const REL_LEAD = pick(["ref:def456"],
                        ['refs_to( "…" )', 'refs_from( "…" )'],
                        ["REFS_TO( '…' )", "REFS_FROM( '…' )"]);
  // A CALL HAS A CLOSER OF ITS OWN, so the typed dialects step over it before
  // the stage's own close; the flat token has no parens for either.
  const REL_CLOSE = pick([CLOSE], [")", CLOSE], [")", CLOSE]);
  await ff.goto(url);
  await drive(OPEN_FILTER);
  await ff.keys(REL_DRIVE[0]);
  // BOTH DIRECTIONS COME OFF ONE ROSTER: `refs_' names neither, so the pair
  // standing here is the direction law with nowhere to drift.
  const relPair = await ff.eval(() => RIG.menu().items);
  await drive(REL_DRIVE.slice(1));
  const relLive = await ff.eval(held);
  await ff.keys(REL_CLOSE.concat([KEY.Enter]));
  const relOn = await ff.eval(() => ({ q: RIG.query(), rows: RIG.rows(),
                                       first: RIG.served(RIG.query()).rows
                                         .map((r) => r.title) }));
  await ff.goto(url);
  await drive(OPEN_FILTER);
  await drive(ANY_DRIVE);
  const anyLive = await ff.eval(held);
  // …AND THE ARITY IS WHAT THE SILENCE READS.  Walking the caret off the
  // constructor and back onto its tail asks the position again: half a name is
  // still being written and offers, and the whole one is a finished TERM and
  // does not.  That is round 15's law over the word this round moved.
  let anyWalk = { half: true, whole: false };
  if (typed || sqlish) {
    await ff.keys([KEY.ArrowLeft]);
    const half = await ff.eval(() => RIG.menu().open);
    await ff.keys([KEY.ArrowRight]);
    anyWalk = { half: half, whole: await ff.eval(() => RIG.menu().open) };
  }
  await ff.keys(REL_CLOSE.concat([KEY.Enter]));
  const anyOn = await ff.eval(() => ({ q: RIG.query(), rows: RIG.rows() }));
  want("REL",
       eq(relPair, REL_LEAD)
       && relLive.c === REL_TOKEN && relLive.menu === false
       && relOn.q === BOOT_FILTER + " " + REL_TOKEN && relOn.rows === 1
       && eq(relOn.first, ["Ship the dot chain"])
       && anyLive.c === "ref:*any*" && anyWalk.half === true
       && anyWalk.whole === false
       && anyOn.q === BOOT_FILTER + " ref:*any*" && anyOn.rows === 2,
       `offers=${JSON.stringify(relPair)} live=${JSON.stringify(relLive)} `
       + `on=${JSON.stringify(relOn)} any=${JSON.stringify(anyLive)}/`
       + `${JSON.stringify(anyWalk)}/${JSON.stringify(anyOn)}`);

  // ---- DRY: an accept inside the parens lands bare and closes the offers
  await ff.goto(url);
  await drive(OPEN_FILTER);
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
  // and the box together.  G INHERITS THAT RULE UNCHANGED — it is the typed
  // door's, and g is a typed door: every open value is quoted, so the cancel
  // has the same line to draw.
  const rung = () => ({ menu: RIG.menu().open, door: RIG.door(), chips: RIG.chips(),
                        held: RIG.door() === "compose" ? RIG.cx().stages.length
                          : document.querySelector("#app .tv-filter").value.length });
  await ff.goto(url);
  await ff.keys([".", "s"]);
  const rung0 = await ff.eval(rung);
  await ff.keys([KEY.Escape]);
  const rung1 = await ff.eval(rung);
  if (typed || sqlish) {
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

  // ---- SIG: THE DOOR'S SIGNATURE, CAPTURED IN EVERY TAB.  A declared MISS
  // that starts passing is reported, and a declared DEPARTURE owes the same
  // symmetry: one that quietly came back is a change nothing else in the run
  // would say out loud.  So `/' is pressed everywhere and the signature kept
  // everywhere — the tabs that keep the flat door owe each other a byte for
  // byte identical one, and the tabs that DEPARTED owe the opposite, a
  // signature that differs from it.  The control at the foot reads both.
  await ff.goto(url);
  await ff.keys(["/"]);
  await ff.keys(chars("sta"));
  sigs[page] = await ff.eval(() => {
    const b = document.querySelector("#app .tv-filter");
    if (!b) return "no flat box at all";
    return JSON.stringify({ tag: b.tagName, cls: b.className, type: b.type,
                            ph: b.placeholder, shown: getComputedStyle(b).display,
                            items: RIG.menu().items });
  });
  kept[page] = !departs.includes("SIG");

  if (!departs.includes("SLASH")) {
    // ---- SLASH: the same door in every tab, and it still refuses the shaping half
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
    await drive(OPEN_SORT);
    for (const batch of SORT_KEYS) await ff.keys(batch);
    await ff.keys([CLOSE, KEY.Enter]);
    await ff.keys(["/"]);
    const open = await ff.eval(sqlish ? caretInClause : caretInParens);
    const opened = await ff.eval(() => ({ door: RIG.door(), cx: RIG.cx(),
                                          pills: RIG.pills(), q: RIG.query(),
                                          menu: RIG.menu().open,
                                          lead: RIG.menu().items[0] }));
    await ff.keys(chars(ADD_ARG));
    await ff.keys([CLOSE, KEY.Enter]);
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
    const written = await ff.eval(() => {
      const s = RIG.cx().stages.slice(-1)[0];
      return s && s.args ? s.args.length : 0;
    });
    await ff.keys(new Array(written).fill(KEY.Backspace));
    // …and `/' over a stage that is open and EMPTY adds no comma either: there
    // is nothing standing for a fresh argument to follow.  It also stays the
    // same rewrite, or the badge's tokens would land twice.
    if (typed || sqlish) await ff.keys(["/"]);
    const reEmpty = typed || sqlish
      ? await ff.eval(() => ({ args: RIG.cx().stages.slice(-1)[0].args,
                               at: (RIG.caret() || {}).at }))
      : { args: "", at: 0 };
    await ff.keys([CLOSE, KEY.Enter]);
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
    await ff.keys([CLOSE]);
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
    await drive(OPEN_SORT);
    for (const batch of SORT_KEYS) await ff.keys(batch);
    await ff.keys([CLOSE]);
    await drive(OPEN_COLS);
    await ff.keys(chars(COLS_ARG));
    await ff.keys([CLOSE, KEY.Enter]);
    const built = await ff.eval(() => ({ pills: RIG.pills(), q: RIG.query() }));
    const walk = [];
    for (let i = 0; i < 4; i += 1) {
      await ff.keys([DEL]);
      walk.push(await ff.eval(() => RIG.query()));
    }
    want("DEL-STAGE",
         built.pills.length === 3 && built.pills[1] === SORT_PILL
         && built.q === BOOT_FILTER + " sort:deadline " + COLS_TOKEN
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
    /** One route in and one press out: the screen has to come back byte for byte. */
    const walkOut = async (into) => {
      await ff.goto(url);
      const was = await ff.eval(picture), wasQ = await ff.eval(whole);
      for (const batch of into) await ff.keys(batch);
      const mid = await ff.eval(midEdit);
      await ff.keys([KEY.Escape]);
      return { was, wasQ, mid, now: await ff.eval(picture), nowQ: await ff.eval(whole) };
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
    const pend0 = await ff.eval(picture), pendQ = await ff.eval(whole);
    await ff.keys(["/"]);
    const pendOpen = await ff.eval(midEdit);
    await ff.keys(chars("zz"));
    await ff.keys([KEY.Escape]);
    const pend1 = await ff.eval(picture), pendBack = await ff.eval(whole);
    await ff.keys([KEY.Escape]);
    const pendGone = await ff.eval(whole);
    // …and the interrupted edit: `.sort(' half written, `/' summoned over it,
    // one press and the sort stage is back with its caret where it was.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys(["s"]);
    await ff.keys([KEY.Tab]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars('Deadline"'));
    const under0 = await ff.eval(midEdit), underPic = await ff.eval(picture);
    await ff.keys(["/"]);
    const underOn = await ff.eval(midEdit);
    await ff.keys(chars('tag = docs"'));
    await ff.keys([KEY.Escape]);
    const under1 = await ff.eval(midEdit), underPic1 = await ff.eval(picture);
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
    // (d) THE OFFER'S OWN CLOSING PAREN, STEPPED OVER — the caret-edge law at
    // the one site that used to move the caret WITHOUT asking again.  `not ( … )'
    // lands the caret INSIDE the parens with the field offers standing; `)'
    // walks past the closer the offer wrote rather than doubling it, and that
    // step is a MOVE, so the new position is asked.  It stands after a CLOSED
    // wrapper, which is a finished term, so the menu goes down and `RET'
    // applies the stage.  With the re-ask missed, the field offers stood over a
    // finished term and RET spliced `state = "…"' after the wrapper instead.
    await ff.goto(url);
    await ff.keys(["."]);
    await ff.keys([KEY.Tab]);
    await ff.keys(chars("not"));
    await ff.keys([KEY.Tab]);                  // take `not ( … )'
    const wrapOpen = await ff.eval(stood);
    await ff.keys([")"]);
    const wrapStep = await ff.eval(stood);
    await ff.keys([KEY.Enter]);
    const stepApplied = await ff.eval(landed);
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
         && wrapApplied.door === null && wrapApplied.rows === 3
         && wrapOpen.args === "not ()" && wrapOpen.at === 5
         && wrapOpen.menu === true
         && wrapStep.args === "not ()" && wrapStep.at === 6
         && wrapStep.menu === false
         && stepApplied.door === null && stepApplied.q === BOOT_FILTER,
         `str=${JSON.stringify(doneStr)} space=${JSON.stringify(doneSpace)} `
         + `fresh=${JSON.stringify(doneFresh)} again=${JSON.stringify(doneAgain)} `
         + `applied=${JSON.stringify(doneApplied)} `
         + `ctor=${JSON.stringify(doneCtor)} half=${JSON.stringify(doneHalf)} `
         + `walk=${JSON.stringify(doneWalk)} ctorApplied=${JSON.stringify(ctorApplied)} `
         + `inner=${JSON.stringify(doneInner)} wrap=${JSON.stringify(doneWrap)} `
         + `wrapApplied=${JSON.stringify(wrapApplied)} `
         + `open=${JSON.stringify(wrapOpen)} step=${JSON.stringify(wrapStep)} `
         + `stepApplied=${JSON.stringify(stepApplied)}`);

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
    want("CASE",
         canon.args === 'not (tag = "chore")' && canon.c === "-tag:chore"
         && partial === 0 && marked.bad === 1 && marked.c === "",
         `canon=${JSON.stringify(canon)} partial=${partial} `
         + `marked=${JSON.stringify(marked)}`);

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
         && eq(quietOn.said, []) && quietOn.badges === 0,
         `allAt=${allAt} conflict=${JSON.stringify(conflict)} `
         + `applied=${JSON.stringify(stillAsked)} quiet=${JSON.stringify(quiet)} `
         + `quietOn=${JSON.stringify(quietOn)}`);

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

  if (sqlish) {
    // ---- SLOT: THE OPERATOR OPENS THE QUOTED SLOT, where F's `=' does.  Two
    // routes, pinned apart: the operator taken out of the offers, and the
    // operator typed by hand.  What is taken out of the slot decides the
    // quotes' fate — a constructor is no string, so accepting one swallows
    // them; a literal keeps them.  Both stay dry.
    // ROUTE ONE — the column, then its operator, then its value.  THE COLUMN
    // ACCEPT OPENS NO SLOT AND STILL ASKS AGAIN: SQL has ten operators where
    // record syntax has one, so the choice is the reader's and the offers owe
    // them the list.
    await ff.goto(url);
    await drive(OPEN_FILTER);
    await ff.keys(chars("sta"));
    await ff.keys([KEY.Enter]);
    const col = await ff.eval(held);
    await ff.keys([KEY.Tab]);                    // …the `=' and its slot
    const slot = await ff.eval(held);
    await ff.keys([KEY.Tab]);                    // …the constructor, out of it
    const ctor = await ff.eval(held);
    // ROUTE TWO — the operator typed by hand.  The same slot, the same offers,
    // and this column's own domain leads them.
    await ff.goto(url);
    await drive(OPEN_FILTER);
    await ff.keys(chars("tag ="));
    const byHand = await ff.eval(held);
    // …AND THE OPENED SLOT SPENDS THE OPENING QUOTE: a reader who types the one
    // they always would gets no second quote and no step over the first.
    await ff.keys(["'"]);
    const spent = await ff.eval(held);
    // …AND THE VALUE ACCEPT IS THE ONE THAT IS FINAL: dry, closed, and a
    // repaint does not resurrect the offers.
    await ff.keys(chars("chor"));
    await ff.keys([KEY.Tab]);
    const str = await ff.eval(held);
    await ff.eval(() => RIG.repaint());
    const settledMenu = await ff.eval(() => RIG.menu().open);
    await ff.keys(chars(" AND state = TODO'"));   // the closing quote is stepped over
    const past = await ff.eval(held);
    want("SLOT",
         col.args === "state" && col.menu === true
         && eq(col.items.slice(0, 2), ["= '…'", "<> '…'"])
         && slot.args === "state = ''" && slot.at === 9 && slot.menu === true
         && eq(slot.items, ["ACTIVE", "INACTIVE", "'TODO'", "'NEXT'"])
         && ctor.args === "state = ACTIVE" && ctor.menu === false
         && ctor.c === "state:*active*"
         && byHand.args === "tag = ''" && byHand.at === 7 && byHand.menu === true
         && byHand.items[0] === "ARCHIVE" && /^'/.test(byHand.items[1])
         && eq(spent, byHand)
         && str.args === "tag = 'chore'" && str.menu === false && str.c === "tag:chore"
         && settledMenu === false
         && past.args === "tag = 'chore' AND state = 'TODO'"
         && past.c === "tag:chore state:TODO",
         `col=${JSON.stringify(col)} slot=${JSON.stringify(slot)} `
         + `ctor=${JSON.stringify(ctor)} hand=${JSON.stringify(byHand)} `
         + `spent=${JSON.stringify(spent)} str=${JSON.stringify(str)} `
         + `settled=${settledMenu} past=${JSON.stringify(past)}`);

    // ---- DONE: A COMPLETE TERM ENDS THE CONVERSATION, asked in SQL's own
    // quoting.  Round 15's law is inherited whole; what g pins per variant is
    // what FINISHES a term here — two quote characters, a list that closes with
    // a paren, and a BARE NAME whose completeness is the namespace's: a column
    // in `WHERE' is waiting for its operator, and a name in `SELECT' or `ORDER
    // BY' is the whole answer.
    // (a) A CLOSED LITERAL, the caret stepped over its far quote.
    await ff.goto(url);
    await drive(OPEN_FILTER);
    await ff.keys(chars("tag = docs'"));
    const doneStr = await ff.eval(stood);
    // …and the TERM is what decides, never the offset: a space after it is
    // still a finished term, and the menu stays down over it.
    await ff.keys([" "]);
    const doneSpace = await ff.eval(stood);
    // …and the counter-case is the CONNECTIVE, which is a word: g has no
    // punctuation that opens a fresh predicate, so the position wakes when the
    // reader starts writing one.
    await ff.keys(["A"]);
    const doneJoin = await ff.eval(stood);
    await ff.keys([KEY.Backspace, KEY.Backspace]);
    const doneAgain = await ff.eval(stood);
    await ff.keys([KEY.Enter]);
    const doneApplied = await ff.eval(landed);
    // (b) A CLOSED LIST — the paren the offers opened, stepped over.
    await ff.goto(url);
    await drive(OPEN_FILTER);
    await ff.keys(chars("tag I"));
    await ff.keys([KEY.Tab]);
    const inOpen = await ff.eval(stood);
    await ff.keys(chars("docs'"));
    await ff.keys([","]);
    const inNext = await ff.eval(stood);
    await ff.keys(chars("chore'"));
    await ff.keys([")"]);
    const inShut = await ff.eval(stood);
    await ff.keys([KEY.Enter]);
    const inApplied = await ff.eval(landed);
    // (c) A BARE NAME, whose completeness is the NAMESPACE'S.  In `WHERE' a
    // column is unfinished; one step short of a constructor is still being
    // written; and the constructor itself stands alone.
    await ff.goto(url);
    await drive(OPEN_FILTER);
    await ff.keys(chars("tag"));
    const bareCol = await ff.eval(stood);
    await ff.keys(chars(" = ARCHIV"));
    const halfCtor = await ff.eval(stood);
    await ff.keys([KEY.Tab]);                    // …the constructor eats the slot
    const wholeCtor = await ff.eval(stood);
    // …and the caret WALKED onto its tail rather than left there by the accept:
    // what decides is the term, and never the gesture that reached it.
    await ff.keys([KEY.ArrowLeft]);
    const walkedOff = await ff.eval(stood);
    await ff.keys([KEY.ArrowRight]);
    const walkedBack = await ff.eval(stood);
    await ff.keys([KEY.Enter]);
    const ctorApplied = await ff.eval(landed);
    want("DONE",
         doneStr.args === "tag = 'docs'" && doneStr.at === 12
         && doneStr.menu === false
         && doneSpace.args === "tag = 'docs' " && doneSpace.menu === false
         && doneJoin.menu === true && doneJoin.lead === "AND"
         && eq(doneAgain, doneStr)
         && doneApplied.q === BOOT_FILTER + " tag:docs"
         && doneApplied.door === null && doneApplied.rows === 1
         && inOpen.args === "tag IN ('')" && inOpen.at === 9
         && inNext.args === "tag IN ('docs', '')" && inNext.at === 17
         && inShut.args === "tag IN ('docs', 'chore')" && inShut.menu === false
         && inApplied.q === BOOT_FILTER + " tag:docs|chore"
         && inApplied.door === null
         && bareCol.args === "tag" && bareCol.menu === true
         && bareCol.lead === "= '…'"
         && halfCtor.args === "tag = 'ARCHIV'" && halfCtor.menu === true
         && halfCtor.lead === "ARCHIVE"
         && wholeCtor.args === "tag = ARCHIVE" && wholeCtor.at === 13
         && wholeCtor.menu === false
         && walkedOff.menu === true && eq(walkedBack, wholeCtor)
         && ctorApplied.q === BOOT_FILTER + " tag:*archive*"
         && ctorApplied.door === null,
         `str=${JSON.stringify(doneStr)} space=${JSON.stringify(doneSpace)} `
         + `join=${JSON.stringify(doneJoin)} again=${JSON.stringify(doneAgain)} `
         + `applied=${JSON.stringify(doneApplied)} in=${JSON.stringify(inOpen)}`
         + `/${JSON.stringify(inNext)}/${JSON.stringify(inShut)}`
         + `/${JSON.stringify(inApplied)} col=${JSON.stringify(bareCol)} `
         + `half=${JSON.stringify(halfCtor)} whole=${JSON.stringify(wholeCtor)} `
         + `walked=${JSON.stringify(walkedOff)}/${JSON.stringify(walkedBack)} `
         + `ctorApplied=${JSON.stringify(ctorApplied)}`);

    // ---- ESC-ABANDON: ESC CANCELS INPUT, and g inherits the rule whole —
    // it is the TYPED DOOR'S rule, and g is a typed door: every open value is
    // quoted, so the same line is available and the same press abandons the
    // whole edit.  Three routes in, one press out of each, and the WHOLE
    // picture back: chips, box, rows, hint and the three lines under them.
    const walkOut = async (into) => {
      await ff.goto(url);
      const was = await ff.eval(picture), wasQ = await ff.eval(whole);
      for (const batch of into) await ff.keys(batch);
      const mid = await ff.eval(midEdit);
      await ff.keys([KEY.Escape]);
      return { was, wasQ, mid, now: await ff.eval(picture), nowQ: await ff.eval(whole) };
    };
    // (a) THE SUMMON ALONE: `/' dangles an `AND' — the gesture's own separator,
    // per dialect — and stands its offers; one ESC takes both.
    const escA = await walkOut([["/"]]);
    // (b) …AND WITH A CONDITION TYPED INTO IT.  What this route loses is the
    // text and the `AND' and NOT the offers: the condition is complete, so the
    // menu is already down over it by round 15's law.
    const escB = await walkOut([["/"], chars("tag = docs'")]);
    // (c) …AND MID-EDIT OF A PREDICATE ALREADY WRITTEN, reached by moving the
    // caret rather than by the gesture.
    const escC = await walkOut([["/"], new Array(5).fill(KEY.ArrowLeft),
                                new Array(8).fill(KEY.Backspace), chars("%milk%'")]);
    const same = (r) => r.was === r.now && eq(r.wasQ, r.nowQ)
      && r.nowQ.door === null && r.nowQ.stages.length === 0;
    want("ESC-ABANDON",
         escA.mid.args === REOPEN_TEXT && escA.mid.menu === true && same(escA)
         && escB.mid.args === SQL_FILTER + " AND tag = 'docs'"
         && escB.mid.menu === false && same(escB)
         && escC.mid.args === "state = ACTIVE AND tag NOT LIKE '%milk%' AND "
         && same(escC),
         `a=${JSON.stringify(escA.mid)}/${same(escA)} `
         + `b=${JSON.stringify(escB.mid)}/${same(escB)} `
         + `c=${JSON.stringify(escC.mid)}/${same(escC)}`);

    // ---- CASE: SQL'S OWN CONVENTION IS THE SPIKE'S CASE LAW.  Any case is
    // typed and any case parses — keywords, columns and the enum roster alike —
    // and what STANDS after the accept is the canonical spelling: the language
    // upper, the columns it knows lower.  In `SELECT' alone the spelling is
    // left as written, because for a CUSTOM column the spelling IS the header.
    await ff.goto(url);
    await drive(OPEN_FILTER);
    await ff.keys(chars("not (TAG = chore'"));
    await ff.keys([")"]);
    await ff.keys([";"]);
    const canon = await ff.eval(() => ({
      args: RIG.cx().stages.slice(-1)[0].args, c: RIG.composed() }));
    want("CASE",
         canon.args === "NOT (tag = 'chore')" && canon.c === "-tag:chore",
         `canon=${JSON.stringify(canon)}`);

    // ---- FRAGMENT: THE VARIANT'S CENTRAL LAW.  The flat grammar is axes-AND
    // with per-axis disjunction, so `AND' composes across anything and `OR'
    // only within ONE column.  A cross-axis `OR' is REFUSED: the clause
    // composes NOTHING, the word itself is marked, and the hint row names the
    // fragment.  Nothing is dropped quietly — a filter that loses a conjunct
    // serves MORE rows, and more is the one direction a reader cannot check.
    await ff.goto(url);
    await ff.keys(["/"]);
    await ff.keys(chars("state = TODO' OR tag = web'"));
    const refused = await ff.eval(() => ({
      args: RIG.cx().stages.slice(-1)[0].args,
      c: RIG.composed(),
      said: [...document.querySelectorAll("#app .tv-hint .tv-refused")]
        .map((n) => n.textContent),
      marks: [...document.querySelectorAll("#app .cx .cx-live .cx-bad")]
        .map((n) => n.textContent),
    }));
    // …and the near miss composes: one axis, and the `OR' is the alternation.
    await ff.goto(url);
    await drive(OPEN_FILTER);
    await ff.keys(chars("state = TODO' OR state = DONE'"));
    const oneAxis = await ff.eval(() => ({
      c: RIG.composed(),
      said: document.querySelectorAll("#app .tv-hint .tv-refused").length,
      marks: document.querySelectorAll("#app .cx .cx-live .cx-bad").length,
    }));
    await ff.keys([KEY.Enter]);
    const orApplied = await ff.eval(() => ({ q: RIG.query(), rows: RIG.rows() }));
    // …AND THE TRAP IS PRECEDENCE, not the axis law.  `AND' binds tighter, so a
    // one-column `OR' written beside another column's predicate hands the OR an
    // arm that spans both — and the refusal is TRUE of what was written.  The
    // parens are the reader's answer, and they are SQL's own.
    await ff.goto(url);
    await ff.keys(["/"]);
    await ff.keys(chars("state = TODO' OR state = DONE'"));
    const looseOr = await ff.eval(() => ({
      c: RIG.composed(),
      said: [...document.querySelectorAll("#app .tv-hint .tv-refused")]
        .map((n) => n.textContent).length,
    }));
    await ff.goto(url);
    await ff.keys(["/"]);
    await ff.keys(chars("(state = TODO' OR state = DONE'"));
    await ff.keys([")"]);
    const tightOr = await ff.eval(() => ({
      c: RIG.composed(),
      said: document.querySelectorAll("#app .tv-hint .tv-refused").length,
    }));
    // …and the whole fragment, as law.  Each refusal names itself, composes
    // nothing, and prints the empty query's own IR.
    const REFUSALS = {
      cross: ["state = 'TODO' OR tag = 'web'", "OR across columns"],
      twoBases: ["tag <> 'web' OR tag <> 'docs'", "one base"],
      deMorgan: ["NOT (tag = 'web' AND tag = 'docs')", "De Morgan"],
      cmpOffDate: ["title > 'x'", "the date keys alone"],
      likeShape: ["state LIKE '%TODO%'", "matches a cell exactly"],
      likePrefix: ["title LIKE 'ship%'", "looks inside the cell"],
      likeEnd: ["title LIKE '%ship'", "END of a cell"],
      likeUnderscore: ["title LIKE '%s_ip%'", "single-character wildcard"],
      // …A `%' IN THE MIDDLE, which is the wildcard shape the flat grammar has
      // no test for at all: not a prefix, not a suffix, not a substring.
      midPercent: ["title LIKE 'sh%ip'", "a % inside"],
      betweenOff: ["priority BETWEEN 'A' AND 'B'", "no interval"],
      // …and a WIDENING OR'D AGAIN.  `threeWide' below shows the flat list of
      // three arms composing; NESTING one inside another `OR' is the shape that
      // cannot, because the inner arm is already a disjunct and the flat `+'
      // has no way to say a disjunct of disjuncts.
      orWidening: ["((tag = 'web' AND tag = 'docs') OR tag = 'chore') OR tag = 'read'",
                   "already a disjunct"],
      // …AND THE AXIS LAW READS THE NEW KEYS THE WAY IT READS ANY OTHER: a
      // relation is an axis, so an `OR' out of one is refused by the same
      // sentence.
      relCross: ["REFS_TO('abc123') OR tag = 'web'", "OR across columns"],
      // …AND THE CLOSURE, refused BY NAME.  A bounded walk is what SQL calls
      // `WITH RECURSIVE'; it is proposed and unshipped, and a recursive CTE is
      // a named subquery with an arbitrary body of which the flat grammar
      // composes exactly one shape.  So the refusal names the document rather
      // than teaching a language that is not there.
      closure: ["WITH RECURSIVE reach AS (SELECT 1)",
                "2026-08-23-the-chain-a-row-hangs-on.md"],
    };
    // …AND THE SHAPES THAT DO COMPOSE, which is what makes the refusal a line
    // and not a wall: the alternation, the base-and-widening law 5 could not
    // spell in kwargs at all, and De Morgan the OTHER way round.
    const ALLOWED = {
      alternation: ["state = 'TODO' OR state = 'DONE'", "state:TODO|DONE"],
      widened: ["(tag = 'web' AND tag = 'docs') OR tag = 'chore'",
                "tag:web tag:docs +tag:chore"],
      widenedNeg: ["(tag = 'web' AND tag <> 'docs') OR tag = 'chore'",
                   "tag:web -tag:docs +tag:chore"],
      notOr: ["NOT (tag = 'web' OR tag = 'docs')", "-tag:web|docs"],
      threeWide: ["(tag = 'web' AND tag = 'docs') OR tag = 'chore' OR tag = 'read'",
                  "tag:web tag:docs +tag:chore +tag:read"],
      // …and a one-axis `OR' of two anchors composes, which is the alternation
      // one call cannot spell and this one can
      relOr: ["REFS_TO('abc123') OR REFS_TO('def456')", "ref:abc123|def456"],
      relBase: ["(REFS_TO('abc123') AND REFS_TO('def456')) OR REFS_TO('ghi789')",
                "ref:abc123 ref:def456 +ref:ghi789"],
    };
    const frag = await ff.eval((spec) => {
      const out = { no: {}, yes: {}, empty: RIG.irFlat("") };
      for (const k of Object.keys(spec.no)) {
        const src = spec.no[k][0];
        out.no[k] = { c: RIG.stageString("filter", src),
                      said: RIG.sqlRefusals(src),
                      ir: RIG.irSql("WHERE " + src) };
      }
      for (const k of Object.keys(spec.yes)) {
        const src = spec.yes[k][0];
        out.yes[k] = { c: RIG.stageString("filter", src),
                       said: RIG.sqlRefusals(src).length,
                       ir: RIG.irSql("WHERE " + src),
                       flat: RIG.irFlat(spec.yes[k][1]) };
      }
      return out;
    }, { no: REFUSALS, yes: ALLOWED });
    const noOff = Object.keys(REFUSALS).filter((k) => {
      const g = frag.no[k];
      return g.c !== "" || g.said.length !== 1 || g.ir !== frag.empty
        || !g.said[0].includes(REFUSALS[k][1]);
    });
    const yesOff = Object.keys(ALLOWED).filter((k) => {
      const g = frag.yes[k];
      return g.c !== ALLOWED[k][1] || g.said !== 0 || g.ir !== g.flat;
    });
    want("FRAGMENT",
         refused.args === SQL_FILTER + " AND state = 'TODO' OR tag = 'web'"
         && refused.c === "" && refused.said.length === 1
         && /^OR across columns has no flat spelling/.test(refused.said[0])
         && eq(refused.marks, ["OR"])
         && oneAxis.c === "state:TODO|DONE"
         && oneAxis.said === 0 && oneAxis.marks === 0
         && orApplied.q === BOOT_FILTER + " state:TODO|DONE"
         && orApplied.rows === 2
         && looseOr.c === "" && looseOr.said === 1
         && tightOr.c === BOOT_FILTER + " state:TODO|DONE" && tightOr.said === 0
         && noOff.length === 0 && yesOff.length === 0,
         `refused=${JSON.stringify(refused)} oneAxis=${JSON.stringify(oneAxis)} `
         + `applied=${JSON.stringify(orApplied)} loose=${JSON.stringify(looseOr)} `
         + `tight=${JSON.stringify(tightOr)} `
         + `no=${noOff.map((k) => k + ":" + JSON.stringify(frag.no[k])).join(" ")} `
         + `yes=${yesOff.map((k) => k + ":" + JSON.stringify(frag.yes[k])).join(" ")}`);

    // ---- DATES: `CURRENT_DATE' is the day and the INTERVAL is the shift, and
    // every law under them is the flat grammar's: the granularity cuts, the
    // clip, the empty cell outside every comparison, negation no mirror, and
    // the range that on `planned' says what no pair of tokens can.
    const dates = await ff.eval(() => {
      const rows = (sql) => RIG.served(RIG.stageString("filter", sql)).rows.length;
      const ir = (sql) => RIG.irSql("WHERE " + sql);
      return {
        today: [RIG.stageString("filter", "deadline = CURRENT_DATE"),
                ir("deadline = CURRENT_DATE"), RIG.irFlat("deadline:2026-08-21")],
        shift: [ir("deadline <= CURRENT_DATE + INTERVAL '30' DAY"),
                RIG.irFlat("deadline:<=2026-09-20")],
        week: [ir("scheduled >= CURRENT_DATE - INTERVAL '1' WEEK"),
               RIG.irFlat("scheduled:>=2026-08-14")],
        // THE CLIP: a month too short for the day takes its own last one.
        clip: [ir("scheduled = DATE '2026-01-31' + INTERVAL '1' MONTH"),
               RIG.irFlat("scheduled:2026-02-28")],
        year: [ir("deadline = DATE '2026-08-28' + INTERVAL '1' YEAR"),
               RIG.irFlat("deadline:2027-08-28")],
        // …and the YEAR clips too, which is `addGregorianYearsClip' — the one
        // day of the calendar that has no twin in the next year.
        leap: [ir("deadline = DATE '2028-02-29' + INTERVAL '1' YEAR"),
               RIG.irFlat("deadline:2029-02-28")],
        // THE BARE BASE is today-relative in the flat grammar, and g has no
        // spelling for it: `INTERVAL' is an operand and wants something to be
        // added to.  So the mirror is asserted on the flat side alone.
        bare: [RIG.irFlat("deadline:<=+30d"), RIG.irFlat("deadline:<=2026-09-20")],
        // The rows, so the law is not merely a string rewrite.
        lookahead: rows("deadline <= CURRENT_DATE + INTERVAL '30' DAY"),
        overdue: rows("deadline < CURRENT_DATE"),
        // ONE CELL INSIDE THE INTERVAL, which two tokens cannot say.
        planned: rows("planned BETWEEN CURRENT_DATE AND CURRENT_DATE + INTERVAL '30' DAY"),
        plannedPair: RIG.served("planned:>=2026-08-21 planned:<=2026-09-20").rows.length,
        // …AND THE CASE THAT PARTS THEM, which the wide interval cannot show —
        // there the two readings agree because no row's cells straddle it.  Row
        // one is scheduled 08-24 and due 08-28, so [08-25, 08-27] falls BETWEEN
        // its two cells: the token pair is answered, each token finding its own
        // cell, and the BETWEEN is answered by nothing.  Without this the law
        // "one cell inside the interval" is stated and unpinned, and a reading
        // that ANDed the two comparisons over the axis would stay green.
        straddle: rows("planned BETWEEN DATE '2026-08-25' AND DATE '2026-08-27'"),
        straddlePair: RIG.served("planned:>=2026-08-25 planned:<=2026-08-27").rows.length,
        // THE EMPTY CELL, ASKED OF ITS OWN CELLS.  `*empty*' is legal on seven
        // keys and every one of them used to spell `=== ""' for itself; the law
        // has one home now, and `planned' is the arm that carries the reason —
        // it names TWO cells and is empty only when BOTH are, where a single
        // key is empty when its one cell is.  Read `||' there and the count
        // triples, which is what this asks.
        emptyPlanned: RIG.served("planned:*empty*").rows.length,
        emptyDeadline: RIG.served("deadline:*empty*").rows.length,
        // NEGATION IS NO MIRROR: they differ on exactly the undated rows.
        notLess: rows("NOT (deadline < CURRENT_DATE)"),
        atLeast: rows("deadline >= CURRENT_DATE"),
        // …and the granularity cut is the interval's end and not its start.
        cutLt: RIG.irSql("WHERE deadline < CURRENT_DATE"),
        cutLe: RIG.irSql("WHERE deadline <= CURRENT_DATE"),
        // AN INTERVAL IS NO VALUE, so the contradiction rule does not read one:
        // two comparisons on a date key are answered every day of the year, and
        // a false warning is worse than a silent one.
        quiet: RIG.unsat("deadline:>=2026-08-01 deadline:<=2026-08-31").said,
      };
    });
    want("DATES",
         dates.today[0] === "deadline:*today*" && dates.today[1] === dates.today[2]
         && dates.shift[0] === dates.shift[1] && dates.week[0] === dates.week[1]
         && dates.clip[0] === dates.clip[1] && dates.year[0] === dates.year[1]
         && dates.leap[0] === dates.leap[1] && dates.bare[0] === dates.bare[1]
         && dates.lookahead === 4 && dates.overdue === 1
         && dates.planned === 4 && dates.plannedPair === 4
         && dates.straddle === 0 && dates.straddlePair === 1
         && dates.emptyPlanned === 1 && dates.emptyDeadline === 2
         && dates.notLess === 5 && dates.atLeast === 3
         && dates.cutLt !== dates.cutLe && eq(dates.quiet, []),
         JSON.stringify(dates));

    // ---- SHAPE: the two clauses that are not the narrowing half, and the one
    // that turned out to be it in disguise.  `SELECT' names columns and its
    // open half is the app's own custom column; `FROM' names a DATASET, which
    // is a tag, so it composes onto the tag axis and the axis law does the
    // rest; `ORDER BY' is the chain.
    await ff.goto(url);
    const shape = await ff.eval(() => ({
      star: RIG.stageString("columns", "*"),
      // THE STAR IS SEVEN WHERE THE DEFAULT VIEW IS SIX — `closed' is the
      // difference, a custom column reading the planning stamp — so it is not
      // the absent token and its IR says so.
      starIr: RIG.irSql("SELECT *"),
      hand: RIG.irSql("SELECT state, priority, title, scheduled, deadline, closed, tags"),
      six: RIG.irSql("SELECT state, priority, title, scheduled, deadline, tags"),
      none: RIG.irFlat(""),
      // AND THE OPEN HALF IS THE APP'S CUSTOM COLUMN, drawer and all.
      custom: RIG.stageString("columns", "owner, title"),
      customIr: [RIG.irSql("SELECT owner, title"), RIG.irFlat("columns:owner,Title")],
      spaced: RIG.stageString("columns", '"ship date"'),
      // `FROM' IS A DATASET, and a dataset is a tag.
      one: RIG.stageString("from", "work"),
      union: [RIG.irSql("FROM work, home"), RIG.irFlat("tag:work|home")],
      aliases: ["*", "all", "default"].map((a) => RIG.stageString("from", a)),
      omitted: [RIG.irSql("WHERE state = ACTIVE"),
                RIG.irSql("FROM all WHERE state = ACTIVE"),
                RIG.irFlat("state:*active*")],
      // …so a `FROM' beside a `WHERE tag' is the axis law's own consequence.
      both: [RIG.irSql("FROM docs WHERE tag = 'chore'"),
             RIG.irFlat("tag:docs tag:chore")],
      bothRows: RIG.served(RIG.stageString("from", "docs") + " "
                           + RIG.stageString("filter", "tag = 'chore'")).rows.length,
      // AN UNKNOWN DATASET COMPOSES ALL THE SAME: the tags are the tree's, so
      // the namespace is open and the empty table is the truthful answer.
      unknown: [RIG.stageString("from", "nosuch"),
                RIG.served("tag:nosuch").rows.length],
      order: [RIG.stageString("sort", "deadline DESC, title"),
              RIG.stageString("sort", "NULL"),
              RIG.stageString("sort", "owner")],
    }));
    // …and the custom column DRAWS, which is what makes the open half real.
    await drive(OPEN_COLS);
    await ff.keys(chars("owner, title"));
    await ff.keys([CLOSE, KEY.Enter]);
    const drawn = await ff.eval(() => ({
      cols: RIG.cols(),
      cells: [...document.querySelectorAll("#app tbody tr")]
        .map((tr) => tr.firstElementChild.textContent),
    }));
    want("SHAPE",
         shape.star === "columns:State,#,Title,Scheduled,Deadline,Closed,Tags"
         && shape.starIr === shape.hand && shape.starIr !== shape.none
         && shape.six === shape.none
         && shape.custom === "columns:owner,title"
         && shape.customIr[0] === shape.customIr[1]
         && shape.spaced === "columns:ship date"
         && shape.one === "tag:work"
         && shape.union[0] === shape.union[1]
         && eq(shape.aliases, ["", "", ""])
         && shape.omitted[0] === shape.omitted[2]
         && shape.omitted[1] === shape.omitted[2]
         && shape.both[0] === shape.both[1] && shape.bothRows === 1
         && shape.unknown[0] === "tag:nosuch" && shape.unknown[1] === 0
         && eq(shape.order, ["sort:deadline:desc->title", "sort:*none*", ""])
         && eq(drawn.cols, ["owner", "Title"])
         && drawn.cells.includes("dmitry") && drawn.cells.includes("ana"),
         JSON.stringify(shape) + " drawn=" + JSON.stringify(drawn));

    // ---- WARN: the courtesy is inherited whole — it reads the ATOMS the
    // surface composes and never its text, so g's spelling is judged the same
    // as F's and the sentence is the same sentence.
    await ff.goto(url);
    await ff.keys(["/"]);
    await ff.keys(chars("tag = docs' AND tag = chore'"));
    const warned = await ff.eval(() => ({
      ink: [...document.querySelectorAll("#app .cx .cx-live .cx-warn")]
        .map((n) => n.textContent).join(""),
      said: [...document.querySelectorAll("#app .tv-hint .tv-warn")]
        .map((n) => n.textContent),
      c: RIG.composed(),
    }));
    await ff.keys([KEY.Enter]);
    const warnOn = await ff.eval(() => ({ q: RIG.query(), rows: RIG.rows(),
                                          said: [...document.querySelectorAll(
                                            "#app .tv-hint .tv-warn")]
                                            .map((n) => n.textContent),
                                          badges: document.querySelectorAll(
                                            "#app .tv-chips .cx-pill.cx-warn").length }));
    want("WARN",
         warned.c === BOOT_FILTER + " tag:docs tag:chore"
         && eq(warned.said, [SAID])
         // BOTH bindings marked and the innocent one left alone — the ink
         // reads the atoms the surface composes, so `tag = 'docs'' beside them
         // is not in it.
         && warned.ink === "tagNOTLIKE'%chore%'tag='chore'"
         && warnOn.q === BOOT_FILTER + " tag:docs tag:chore"
         && warnOn.rows === 0 && eq(warnOn.said, [SAID]) && warnOn.badges === 1,
         `warned=${JSON.stringify(warned)} on=${JSON.stringify(warnOn)}`);

    // ---- IR3: THREE PARSERS, ONE NORMAL FORM.  The flat grammar's reader, F's
    // typed one and g's each build TERMS on their own and hand them to the one
    // builder; where the laws agree the bytes agree, and where they part the
    // bytes part with them.
    await ff.goto(url);
    const ir3 = await ff.eval((c) => ({
      three: c.three.map(([f, d, s]) => [f, d, s, RIG.irFlat(f), RIG.irDsl(d),
                                         RIG.irSql(s)]),
      trip: c.trip.map((q) => [q, RIG.sqlStatementOf(q), RIG.irFlat(q),
                               RIG.irSql(RIG.sqlStatementOf(q))]),
      diff: c.diff.map(([x, y]) => [x, y, RIG.irSql(x), RIG.irSql(y)]),
    }), IR3_CORPUS);
    const threeOff = ir3.three.filter((r) => !(r[3] === r[4] && r[4] === r[5]));
    const trip3Off = ir3.trip.filter((r) => r[2] !== r[3]);
    const diff3Off = ir3.diff.filter((r) => r[2] === r[3]);
    want("IR3",
         threeOff.length === 0 && trip3Off.length === 0 && diff3Off.length === 0
         && ir3.three.length === IR3_CORPUS.three.length,
         [threeOff.map((r) => `${r[0]} / ${r[1]} / ${r[2]}\n    flat ${r[3]}\n`
                       + `    dsl  ${r[4]}\n    sql  ${r[5]}`).join(" · "),
          trip3Off.map((r) => `round trip ${r[0]} → ${r[1]}`).join(" · "),
          diff3Off.map((r) => `${r[0]} and ${r[1]} print the same IR`).join(" · ")]
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
const keeping = picked.filter((p) => sigs[p] !== undefined && kept[p]);
const gone = picked.filter((p) => sigs[p] !== undefined && !kept[p]);
if (keeping.length > 1) {
  const one = sigs[keeping[0]];
  const off = keeping.filter((p) => sigs[p] !== one);
  // …AND THE DEPARTED TABS OWE THE OPPOSITE.  `/' is the filter STAGE's edit
  // key there, so its signature has to PART from the flat door's; one that
  // matched again would be a departure that quietly came back.
  const back = gone.filter((p) => sigs[p] === one);
  if (off.length) {
    failed += 1;
    console.log(`FAIL /  the filter door differs in: ${off.join(", ")}`);
  } else if (back.length) {
    failed += 1;
    console.log(`FAIL /  a declared departure came back in: ${back.join(", ")}`);
  } else {
    console.log(`ok   /  the same door in all ${keeping.length} tabs that keep one`
                + (gone.length ? `, and none of the ${gone.length} that left it` : ""));
  }
}

await ff.close();
process.exit(failed ? 1 : 0);
