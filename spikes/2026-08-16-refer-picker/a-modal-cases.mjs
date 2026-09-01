// The scenarios, written once and run against either engine.  A driver supplies
// { eval, keys } where `keys' takes the flat list bidi.mjs documents.
import { KEY } from "./bidi.mjs";

/** Shift+@ the way a keyboard sends it: Shift down, `@', Shift up. */
export const AT = [{ down: KEY.Shift }, "@", { up: KEY.Shift }];
/** C-u the way a keyboard sends it. */
export const CU = [{ down: KEY.Control }, "u", { up: KEY.Control }];

const seed = (text) => (p) => p.eval((t) => {
  const el = document.getElementById("dpara");
  el.value = t; el.focus(); el.setSelectionRange(t.length, t.length);
  window.__keys = [];
  if (!window.__probe) {
    window.__probe = true;
    document.addEventListener("keydown", (e) => (window.__keys || []).push(e.key), true);
  }
}, text);

/** Seed the pane and leave a REGION selected, as a reader would. */
const select = (text, a, b) => (p) => p.eval((t, from, to) => {
  const el = document.getElementById("dpara");
  el.value = t; el.focus(); el.setSelectionRange(from, to);
  window.__keys = [];
}, text, a, b);

const isUp = (p) => p.eval(() => document.getElementById("refer").classList.contains("on"));
const stage = (p) => p.eval(() => document.getElementById("rstage").textContent);
const hot = (p) => p.eval(() => {
  const n = document.querySelector("#rlist .ke.kh");
  return n ? n.firstChild.textContent : null;
});
const rows = (p) => p.eval(() => [...document.querySelectorAll("#rlist .ke")].map(
  (n) => n.firstChild.textContent + "|" + n.lastChild.textContent));
const pane = (p) => p.eval(() => document.getElementById("dpara").value);
const settled = (p) => p.eval(() => {
  const s = document.getElementById("rsettled");
  return s.hidden ? null : s.firstChild.textContent;
});
const type = (p, s) => p.eval((t) => {
  const q = document.getElementById("rq");
  q.value = t; q.dispatchEvent(new Event("input"));
}, s);
const esc = (p) => p.keys([KEY.Escape]);

export const CASES = [
  { name: "a real @ arrives after a real Shift keydown",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      const seen = await p.eval(() => (window.__keys || []).join(","));
      return [seen.includes("Shift"), true, "the probe saw: " + seen];
    } },

  { name: "@ at a boundary raises the picker on the target stage",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      const r = [await isUp(p), await stage(p)];
      await esc(p);
      return [r, [true, "target — match required"]];
    } },

  { name: "the top row is selected, so RET takes it",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await type(p, "weekly");
      const h = await hot(p);
      await p.keys([KEY.Enter]);
      return [[h, await pane(p)],
              ["MDE weekly", "see [[glance:7db7af20-1c4e-4c2a-9f0a-2b1d55e1a003][MDE weekly]]"]];
    } },

  { name: "@ inside a word stays text",
    async run(p) {
      await seed("mail me at dmitry")(p);
      await p.keys(AT);
      return [[await isUp(p), await pane(p)], [false, "mail me at dmitry@"]];
    } },

  { name: "C-u @ asks the KIND first",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      const r = [await isUp(p), await stage(p)];
      await esc(p);
      return [r, [true, "kind — new kinds welcome"]];
    } },

  { name: "C-u @ fires mid-word, being explicit",
    async run(p) {
      await seed("dmitry")(p);
      await p.keys(CU); await p.keys(AT);
      const r = [await isUp(p), await stage(p)];
      await esc(p);
      return [r, [true, "kind — new kinds welcome"]];
    } },

  { name: "an unknown kind is row one, selected, marked new",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await type(p, "Reviewed By");
      const r = [await rows(p), await hot(p)];
      await esc(p);
      return [r, [["reviewed-by|new kind"], "reviewed-by"]];
    } },

  { name: "a kind that exists is not offered twice",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await type(p, "author");
      const r = await rows(p);
      await esc(p);
      return [r, ["author|41 rows", "authorised-by|2 rows"]];
    } },

  { name: "a prefix of an existing kind still reaches the new one",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await type(p, "auth");
      const r = [await rows(p), await hot(p)];
      await esc(p);
      return [r, [["auth|new kind", "author|41 rows", "authorised-by|2 rows"], "auth"]];
    } },

  { name: "kind then target, and the link carries both",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await type(p, "depends");
      await p.keys([KEY.Enter]);
      const s = await settled(p);
      const st = await stage(p);
      await type(p, "weekly");
      await p.keys([KEY.Enter]);
      return [[s, st, await pane(p)],
              ["depends", "target — match required",
               "see [[glance:7db7af20-1c4e-4c2a-9f0a-2b1d55e1a003?kind=depends][MDE weekly]]"]];
    } },

  { name: "C-p at the top of the target walks back to the kind",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await type(p, "blocks");
      await p.keys([KEY.Enter]);
      await p.keys([KEY.ArrowUp]);
      const r = [await stage(p), await settled(p),
                 await p.eval(() => document.getElementById("rq").value)];
      await esc(p);
      return [r, ["kind — new kinds welcome", null, "blocks"]];
    } },

  { name: "ESC leaves the literal @ standing",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await esc(p);
      return [[await isUp(p), await pane(p)], [false, "see @"]];
    } },

  { name: "C-n walks down from the top row",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await type(p, "wrike");
      await p.keys([KEY.ArrowDown]);
      const h = await hot(p);
      await esc(p);
      return [h, "Wrike MDE onboarding"];
    } },

  { name: "ESC after settling a kind leaves no kind behind",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await type(p, "depends");
      await p.keys([KEY.Enter]);
      const settledKind = await settled(p);
      await esc(p);
      return [[settledKind, await isUp(p), await pane(p)],
              ["depends", false, "see @"]];
    } },

  { name: "and re-opening starts with no kind carried over",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await type(p, "depends");
      await p.keys([KEY.Enter]);
      await esc(p);
      await seed("then ")(p);
      await p.keys(AT);
      await type(p, "weekly");
      await p.keys([KEY.Enter]);
      return [await pane(p),
              "then [[glance:7db7af20-1c4e-4c2a-9f0a-2b1d55e1a003][MDE weekly]]"];
    } },

  { name: "a selected region opens the picker seeded with its own text",
    async run(p) {
      await select("see weekly notes", 4, 10)(p);
      await p.keys(AT);
      const r = [await isUp(p),
                 await p.eval(() => document.getElementById("rq").value),
                 await hot(p), await pane(p)];
      await esc(p);
      return [r, [true, "weekly", "MDE weekly", "see weekly notes"]];
    } },

  { name: "taking a row turns the region into a link READING THE REGION",
    async run(p) {
      await select("see weekly notes", 4, 10)(p);
      await p.keys(AT);
      await p.keys([KEY.Enter]);
      return [await pane(p),
              "see [[glance:7db7af20-1c4e-4c2a-9f0a-2b1d55e1a003][weekly]] notes"];
    } },

  { name: "ESC over a selection writes nothing at all",
    async run(p) {
      await select("see weekly notes", 4, 10)(p);
      await p.keys(AT);
      await esc(p);
      return [[await isUp(p), await pane(p)], [false, "see weekly notes"]];
    } },

  { name: "C-u @ over a selection asks the kind, then keeps the region's words",
    async run(p) {
      await select("see weekly notes", 4, 10)(p);
      await p.keys(CU); await p.keys(AT);
      const st = await stage(p);
      await type(p, "depends");
      await p.keys([KEY.Enter]);
      await p.keys([KEY.Enter]);
      return [[st, await pane(p)],
              ["kind — new kinds welcome",
               "see [[glance:7db7af20-1c4e-4c2a-9f0a-2b1d55e1a003?kind=depends][weekly]] notes"]];
    } },

  { name: "a selected region fires @ even inside a word",
    async run(p) {
      await select("dmitryweekly", 6, 12)(p);
      await p.keys(AT);
      const r = await isUp(p);
      await esc(p);
      return [r, true];
    } },

  { name: "no addressable match refuses RET and keeps the picker",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await type(p, "zzzz no such row");
      await p.keys([KEY.Enter]);
      const r = [await isUp(p), await pane(p),
                 await p.eval(() => document.getElementById("echo").textContent)];
      await esc(p);
      return [r, [true, "see @", "no match — narrow it, or ESC"]];
    } },
];
