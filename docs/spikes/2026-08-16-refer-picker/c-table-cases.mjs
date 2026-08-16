// Cases for the TABLE picker: it is the table-view, so it must look and behave
// like one — the config's default filter, `/' to filter, RET to choose, and the
// mount options owned by table-view rather than by the picker.
import { KEY } from "./bidi.mjs";

export const AT = [{ down: KEY.Shift }, "@", { up: KEY.Shift }];
export const CU = [{ down: KEY.Control }, "u", { up: KEY.Control }];

const seed = (text) => (p) => p.eval((t) => {
  const el = document.getElementById("dpara");
  el.value = t; el.focus(); el.setSelectionRange(t.length, t.length);
}, text);
const select = (text, a, b) => (p) => p.eval((t, from, to) => {
  const el = document.getElementById("dpara");
  el.value = t; el.focus(); el.setSelectionRange(from, to);
}, text, a, b);

const isUp = (p) => p.eval(() => document.getElementById("ac").classList.contains("on"));
const titles = (p) => p.eval(() => [...document.querySelectorAll("#acrows .row")].map(
  (n) => n.querySelector(".ttl").textContent));
const atRow = (p) => p.eval(() => {
  const n = document.querySelector("#acrows .row.at .ttl") || document.querySelector("#acrows .ke.at span");
  return n ? n.textContent : null;
});
// The filter chips ALONE: the kind wears its own class, being no filter.
const chips = (p) => p.eval(() => [...document.querySelectorAll("#acbar .chip")].map((n) => n.textContent));
const kindOf = (p) => p.eval(() => {
  const n = document.querySelector("#acbar .kindchip");
  return n ? n.textContent : null;
});
const pane = (p) => p.eval(() => document.getElementById("dpara").value);
const stageText = (p) => p.eval(() => document.getElementById("acstage").textContent);
const suggs = (p) => p.eval(() => [...document.querySelectorAll("#acsugg .sg")].map(
  (n) => n.firstChild.textContent + "|" + n.lastChild.textContent));
const cols = (p) => p.eval(() => [...document.querySelectorAll("#achead span")].map((n) => n.textContent));
const setCfg = (p, id, v) => p.eval((i, val) => {
  const n = document.getElementById(i);
  if (n.type === "checkbox") n.checked = val; else n.value = val;
  n.dispatchEvent(new Event("input", { bubbles: true }));
}, id, v);

export const CASES = [
  { name: "the picker opens on the CONFIG default, not on everything",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      const t = await titles(p);
      return [[await isUp(p), await chips(p),
               t.includes("org-glance relation model"),      // DONE — filtered out
               t.includes("Screenshot diffs over the shell") // CANCELLED — filtered out
              ],
              [true, ["state:*active*"], false, false]];
    } },

  { name: "it wears the table's four columns",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      return [await cols(p), ["State", "#", "Title", "Tag"]];
    } },

  { name: "changing views.default changes the rows — the config is respected",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await setCfg(p, "cfgfilter", "state:done");
      return [[await chips(p), await titles(p)],
              [["state:done"], ["org-glance relation model", "Wrike migration notes"]]];
    } },

  { name: "the top row is the cursor, and n moves it",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      const first = await atRow(p);
      await p.keys(["n"]);
      return [[first, await atRow(p)], ["Wrike MDE Team", "Wrike MDE onboarding"]];
    } },

  { name: "RET chooses the headline under the cursor",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["n", "n"]);
      await p.keys([KEY.Enter]);
      return [await pane(p), "see [[glance:…a003][MDE weekly]]"];
    } },

  { name: "/ opens the filter, and the default chip stays applied",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."wrike"]);
      return [[await chips(p), await titles(p)],
              [["state:*active*"], ["Wrike MDE Team", "Wrike MDE onboarding"]]];
    } },

  { name: "a letter is MOVEMENT until / is pressed, as in the table",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["j", "j"]);                 // vi's, not a query
      return [[await atRow(p), await pane(p)], ["MDE weekly", "see @"]];
    } },

  { name: "ESC leaves the filter first, then dismisses",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."wr"]);
      await p.keys([KEY.Escape]);                 // the suggestions close
      const one = await isUp(p);
      await p.keys([KEY.Escape]);                 // the half-typed text goes
      const two = await isUp(p);
      await p.keys([KEY.Escape]);                 // and the picker
      return [[one, two, await isUp(p), await pane(p)], [true, true, false, "see @"]];
    } },

  { name: "no gutter by default; `marks' is table-view's option and adds one",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      const before = await p.eval(() => document.querySelectorAll("#acrows .gut").length);
      await setCfg(p, "cfgmarks", true);
      const after = await p.eval(() => document.querySelectorAll("#acrows .gut").length);
      return [[before, after > 0], [0, true]];
    } },

  { name: "`actions' is table-view's too, and the picker ships it off",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      const before = await p.eval(() => document.getElementById("acacts").hidden);
      await setCfg(p, "cfgacts", true);
      const after = await p.eval(() => document.getElementById("acacts").hidden);
      return [[before, after], [true, false]];
    } },

  { name: "the kind stage takes typing directly — no / , it is no table",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      const first = await atRow(p);
      await p.keys([..."depend"]);
      return [[first, await atRow(p), await pane(p)],
              ["author", "depend", "see @"]];     // `depend' leads as a new kind
    } },

  { name: "C-u @ asks the kind first, then the row under the config default",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await p.keys([..."depends"]);
      await p.keys([KEY.Enter]);
      const c = [await kindOf(p), await chips(p)];
      await p.keys([KEY.Enter]);
      return [[c, await pane(p)],
              [["kind:depends", ["state:*active*"]],
               "see [[glance:…a001?kind=depends][Wrike MDE Team]]"]];
    } },

  { name: "previous-row is movement and never walks back to the kind",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await p.keys([..."depends"]);
      await p.keys([KEY.Enter]);                  // kind settled, on the rows
      await p.keys(["n"]);                        // down one, then back up
      await p.keys(["p"]);
      await p.keys(["p"]);                        // and again, at the top
      return [[await kindOf(p), await chips(p), await atRow(p)],
              ["kind:depends", ["state:*active*"], "Wrike MDE Team"]];
    } },

  { name: "DEL empties the QUERY before the kind, so the default stays reachable",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await p.keys([..."depends"]);
      await p.keys([KEY.Enter]);
      const settled = [await kindOf(p), await chips(p)];
      await p.keys([KEY.Backspace]);              // the config default goes first
      const mid = [await kindOf(p), await chips(p), await stageText(p)];
      await p.keys([KEY.Backspace]);              // only then the kind
      return [[settled, mid, await kindOf(p), await stageText(p)],
              [["kind:depends", ["state:*active*"]],
               ["kind:depends", [], "row for kind:depends"],
               null, "kind — new kinds welcome"]];
    } },

  { name: "with no kind, DEL goes straight at the filter chips",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);                           // plain @ — no kind at all
      const before = await chips(p);
      await p.keys([KEY.Backspace]);
      return [[before, await chips(p), await isUp(p)],
              [["state:*active*"], [], true]];
    } },

  { name: "DEL on an empty strip closes the picker and takes the @ with it",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys([KEY.Backspace]);              // the one default chip
      const empty = await chips(p);
      await p.keys([KEY.Backspace]);              // nothing left — the @ goes
      return [[empty, await isUp(p), await pane(p)], [[], false, "see "]];
    } },

  { name: "the whole DEL ladder, top to bottom, ends with the @ gone",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await p.keys([..."depends"]);
      await p.keys([KEY.Enter]);                  // kind:depends + state:*active*
      await p.keys(["/"]);
      await p.keys([..."wrike"]);
      await p.keys([KEY.Enter]);                  // + wrike
      await p.keys([KEY.Backspace]);              // - wrike
      await p.keys([KEY.Backspace]);              // - state:*active*
      await p.keys([KEY.Backspace]);              // - kind, back to the kind stage
      const back = await stageText(p);
      await p.keys([KEY.Backspace, KEY.Backspace, KEY.Backspace,
                    KEY.Backspace, KEY.Backspace, KEY.Backspace, KEY.Backspace]);
      const typedGone = await isUp(p);            // "depends" is 7 characters
      await p.keys([KEY.Backspace]);              // and now the @
      return [[back, typedGone, await isUp(p), await pane(p)],
              ["kind — new kinds welcome", true, false, "see "]];
    } },

  { name: "DEL over a selection closes but never touches the region",
    async run(p) {
      await select("see weekly notes", 4, 10)(p);
      await p.keys(AT);
      await p.keys([KEY.Backspace]);              // the default chip
      await p.keys([KEY.Backspace]);              // nothing left
      return [[await isUp(p), await pane(p)], [false, "see weekly notes"]];
    } },

  { name: "a selected region becomes the link and reads as itself",
    async run(p) {
      await select("see weekly notes", 4, 10)(p);
      await p.keys(AT);
      await p.keys(["/"]);                        // the region is NOT the query
      await p.keys([..."weekly"]);
      await p.keys([KEY.Enter]);                  // commit the chip
      await p.keys([KEY.Enter]);                  // take the row
      return [await pane(p), "see [[glance:…a003][weekly]] notes"];
    } },

  { name: "the region is the link's WORDS, never a filter",
    async run(p) {
      await select("see weekly notes", 4, 10)(p);
      await p.keys(AT);
      return [[await chips(p), await stageText(p), (await titles(p)).length > 2],
              [["state:*active*"], 'the link will read "weekly"', true]];
    } },

  { name: "DEL drops the last filter chip, as stripLastToken does",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      const before = await titles(p);
      await p.keys([KEY.Backspace]);
      const after = await titles(p);
      return [[await chips(p), before.includes("org-glance relation model"),
               after.includes("org-glance relation model")],
              [[], false, true]];       // the DONE row appears once the chip goes
    } },

  { name: "RET in the filter commits a chip and does NOT choose a row",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."wrike"]);
      await p.keys([KEY.Enter]);                  // commits the box, picks nothing
      return [[await chips(p), await isUp(p), await pane(p), await titles(p)],
              [["state:*active*", "substring:wrike"], true, "see @",
               ["Wrike MDE Team", "Wrike MDE onboarding"]]];
    } },

  { name: "and the SECOND RET, on the row, is what chooses",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."wrike"]);
      await p.keys([KEY.Enter]);
      await p.keys([KEY.Enter]);
      return [[await isUp(p), await pane(p)],
              [false, "see [[glance:…a001][Wrike MDE Team]]"]];
    } },

  { name: "ESC in the filter drops what was half-typed",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."wrike"]);
      await p.keys([KEY.Escape]);                 // suggestions
      await p.keys([KEY.Escape]);                 // then the text
      return [[await isUp(p), await chips(p), (await titles(p)).length > 2],
              [true, ["state:*active*"], true]];
    } },

  { name: "DEL then walks the committed chips down, last first",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."wrike"]);
      await p.keys([KEY.Enter]);
      const both = await chips(p);
      await p.keys([KEY.Backspace]);
      const one = await chips(p);
      await p.keys([KEY.Backspace]);
      return [[both, one, await chips(p)],
              [["state:*active*", "substring:wrike"], ["state:*active*"], []]];
    } },

  { name: "DEL inside the filter box is an ordinary edit",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."wrikex"]);
      await p.keys([KEY.Backspace]);
      return [[await p.eval(() => document.getElementById("acbox").value), await chips(p)],
              ["wrike", ["state:*active*"]]];
    } },

  { name: "the cursor is a ground: a badge keeps its own hue on the cursor row",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      // rows 0 and 1 are both TODO; only row 0 wears the cursor.
      const same = await p.eval(() => {
        const rows = [...document.querySelectorAll("#acrows .row")];
        const a = rows[0].querySelector(".pill"), b = rows[1].querySelector(".pill");
        const ca = getComputedStyle(a), cb = getComputedStyle(b);
        return { text: a.textContent === b.textContent,
                 colour: ca.color === cb.color,
                 wash: ca.backgroundColor === cb.backgroundColor,
                 noBorder: ca.borderTopWidth === "0px",
                 rowGround: getComputedStyle(rows[0]).backgroundColor
                            !== getComputedStyle(rows[1]).backgroundColor };
      });
      return [same, { text: true, colour: true, wash: true, noBorder: true, rowGround: true }];
    } },

  // ---- SCHEMA's filter grammar, and the suggestion tiers over it ----

  { name: "after `key:' the column's own domain is offered, with counts",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."state:"]);
      const list = await suggs(p);
      return [[list[0], list.includes("state:TODO|6 rows"),
               list.includes("state:NEXT|3 rows"), list.some((x) => x.startsWith("state:*empty*"))],
              ["state:*active*|10 rows", true, true, true]];
    } },

  { name: "free text wears substring:, so every chip reads KEY:VALUE",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."wrike"]);
      const list = await suggs(p);
      // FREE TEXT WEARS ITS OWN KEY, so the offer is a predicate like any other
      // and prints a count rather than an aside.
      return [[list[0], list.some((x) => x.endsWith("|title"))],
              ["substring:wrike|2 rows", true]];
    } },

  { name: "a value it SPELLS IN FULL leads the literal",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."next"]);
      const list = await suggs(p);
      return [list[0], "state:NEXT|3 rows"];
    } },

  { name: "RET on a `key:' opens its values rather than committing a chip",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."tag"]);
      const before = (await suggs(p))[0];
      await p.keys([KEY.Enter]);                  // takes `tag:' — opens, commits nothing
      const after = await suggs(p);
      return [[before, await chips(p), after.includes("tag:glance|4 rows")],
              ["tag:|column", ["state:*active*"], true]];
    } },

  { name: "tag: is a whole-entry predicate over the tag run",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."tag:work"]);
      await p.keys([KEY.Enter]);
      return [[await chips(p), (await titles(p)).length],
              [["state:*active*", "tag:work"], 4]];
    } },

  { name: "a negation covers the whole token",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."-tag:work"]);
      await p.keys([KEY.Enter]);
      const t = await titles(p);
      return [[await chips(p), t.includes("Wrike MDE Team"), t.includes("Reverse index for in-edges")],
              [["state:*active*", "-tag:work"], false, true]];
    } },

  { name: "a predicate's value splits on | and the alternatives OR",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."state:TODO|WAITING"]);
      await p.keys([KEY.Enter]);
      const t = await titles(p);
      return [[t.length, t.includes("Materialize sheet in Elm"), t.includes("MDE weekly")],
              [7, true, false]];                  // 6 TODO + 1 WAITING, no NEXT
    } },

  { name: "priority reads THROUGH its brackets, both spellings",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."priority:A"]);
      await p.keys([KEY.Enter]);
      // `a' offers `priority:[#A]' and still COMMITS decorated — display wears
      // the brackets, matching reads through them.
      return [[await chips(p), await titles(p)],
              [["state:*active*", "priority:[#A]"],
               ["Wrike MDE Team", "glance: the protocol decision"]]];
    } },

  { name: "*empty* is answered on any column",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."priority:*empty*"]);
      await p.keys([KEY.Enter]);
      const t = await titles(p);
      return [[t.includes("Wrike MDE onboarding"), t.includes("Wrike MDE Team")],
              [true, false]];
    } },

  { name: "substring: is free text's own key, and the two spellings are one query",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."substring:wrike"]);
      await p.keys([KEY.Enter]);
      return [[await chips(p), await titles(p)],
              [["state:*active*", "substring:wrike"],
               ["Wrike MDE Team", "Wrike MDE onboarding"]]];
    } },

  { name: "a negated predicate completes as its positive form does",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."-tag:wor"]);
      const list = await suggs(p);
      return [[list[0], list[1]], ["-tag:work|4 rows", "-tag:wor|4 rows"]];
    } },

  { name: "a title offer is a title: predicate, quoted for its spaces",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys(["/"]);
      await p.keys([..."weekly"]);
      const list = await suggs(p);
      await p.keys([KEY.ArrowDown]);
      await p.keys([KEY.Enter]);
      return [[list.some((x) => x.startsWith('title:"MDE weekly"')), await chips(p)],
              [true, ["state:*active*", 'title:"MDE weekly"']]];
    } },

  { name: "a badge column's header lines up with the badge's FIRST LETTER",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      const x = await p.eval(() => {
        // Measure the TEXT, not the box: a Range over the text node is where the
        // letters actually start.
        const textX = (n) => {
          const r = document.createRange();
          r.selectNodeContents(n);
          return Math.round(r.getBoundingClientRect().left);
        };
        const head = [...document.querySelectorAll("#achead span")];
        const row = document.querySelector("#acrows .row");
        const pills = [...row.querySelectorAll(".pill")];
        return { state: [textX(head[0]), textX(pills[0])],
                 prio:  [textX(head[1]), textX(pills[1])],
                 title: [textX(head[2]), textX(row.querySelector(".ttl"))] };
      });
      return [[x.state[0] === x.state[1], x.prio[0] === x.prio[1], x.title[0] === x.title[1]],
              [true, true, true]];
    } },

  { name: "the columns line up: one set of tracks for the head and every row",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      const same = await p.eval(() => {
        const rows = [...document.querySelectorAll("#acrows .row")];
        const tracks = (n) => getComputedStyle(n).gridTemplateColumns;
        const head = tracks(document.getElementById("achead"));
        const all = rows.map(tracks);
        // every title starts at the same x, which is what "aligned" means
        const lefts = rows.map((r) => Math.round(r.querySelector(".ttl").getBoundingClientRect().left));
        return { rowsAgree: all.every((t) => t === all[0]),
                 headAgrees: head === all[0],
                 oneTitleX: new Set(lefts).size };
      });
      return [same, { rowsAgree: true, headAgrees: true, oneTitleX: 1 }];
    } },

  { name: "@ inside a word stays text",
    async run(p) {
      await seed("mail me at dmitry")(p);
      await p.keys(AT);
      return [[await isUp(p), await pane(p)], [false, "mail me at dmitry@"]];
    } },
];
