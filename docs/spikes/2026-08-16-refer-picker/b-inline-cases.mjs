// Cases for the INLINE widget: the query is the document text, and the pane
// never loses the keyboard.
import { KEY } from "./bidi.mjs";

export const AT = [{ down: KEY.Shift }, "@", { up: KEY.Shift }];
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

const isUp = (p) => p.eval(() => document.getElementById("ac").classList.contains("on"));
const focused = (p) => p.eval(() => (document.activeElement || {}).id || null);
const rows = (p) => p.eval(() => [...document.querySelectorAll("#aclist .ke")].map(
  (n) => n.firstChild.textContent + "|" + n.lastChild.textContent));
const hot = (p) => p.eval(() => {
  const n = document.querySelector("#aclist .ke.kh");
  return n ? n.firstChild.textContent : null;
});
const pane = (p) => p.eval(() => document.getElementById("dpara").value);
const stage = (p) => p.eval(() => document.getElementById("acstage").textContent);

export const CASES = [
  { name: "@ opens the widget and the PANE KEEPS THE KEYBOARD",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      return [[await isUp(p), await focused(p)], [true, "dpara"]];
    } },

  { name: "there is no veil — the page behind stays reachable",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      const veiled = await p.eval(() => {
        const n = document.getElementById("ac");
        const r = n.getBoundingClientRect();
        return { w: Math.round(r.width), full: r.width >= window.innerWidth - 1 };
      });
      return [[veiled.full, veiled.w > 100], [false, true]];
    } },

  { name: "the query is the document text, and it filters as you type",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys([..."weekly"]);
      return [[await pane(p), await rows(p)],
              ["see @weekly", ["MDE weekly|7 refs"]]];
    } },

  { name: "the top row is selected and RET replaces the typed run",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys([..."weekly"]);
      const h = await hot(p);
      await p.keys([KEY.Enter]);
      return [[h, await isUp(p), await pane(p)],
              ["MDE weekly", false,
               "see [[glance:7db7af20-1c4e-4c2a-9f0a-2b1d55e1a003][MDE weekly]]"]];
    } },

  { name: "the widget hangs BELOW the caret, not over the page",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      const geom = await p.eval(() => {
        const a = document.getElementById("ac").getBoundingClientRect();
        const t = document.getElementById("dpara").getBoundingClientRect();
        return { below: a.top > t.top, near: Math.abs(a.left - t.left) < 260 };
      });
      return [geom, { below: true, near: true }];
    } },

  { name: "@ inside a word stays text and opens nothing",
    async run(p) {
      await seed("mail me at dmitry")(p);
      await p.keys(AT);
      return [[await isUp(p), await pane(p)], [false, "mail me at dmitry@"]];
    } },

  { name: "C-u @ asks the kind first, still inline",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      return [[await isUp(p), await stage(p), await focused(p)],
              [true, "kind — new kinds welcome", "dpara"]];
    } },

  { name: "an unknown kind is row one, selected, marked new",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await p.keys([..."reviewed"]);
      return [[await rows(p), await hot(p)], [["reviewed|new kind"], "reviewed"]];
    } },

  { name: "a prefix of an existing kind still reaches the new one",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await p.keys([..."auth"]);
      return [[await rows(p), await hot(p)],
              [["auth|new kind", "author|41 rows", "authorised-by|2 rows"], "auth"]];
    } },

  { name: "taking the kind rewrites the run inline and moves to the target",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await p.keys([..."depends"]);
      await p.keys([KEY.Enter]);
      return [[await pane(p), await stage(p)], ["see @depends ", "target for depends"]];
    } },

  { name: "kind then target writes one link carrying both",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await p.keys([..."depends"]);
      await p.keys([KEY.Enter]);
      await p.keys([..."weekly"]);
      await p.keys([KEY.Enter]);
      return [await pane(p),
              "see [[glance:7db7af20-1c4e-4c2a-9f0a-2b1d55e1a003?kind=depends][MDE weekly]]"];
    } },

  { name: "C-p at the top of the target puts the kind back under the caret",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await p.keys([..."blocks"]);
      await p.keys([KEY.Enter]);
      await p.keys([KEY.ArrowUp]);
      return [[await pane(p), await stage(p)], ["see @blocks", "kind — new kinds welcome"]];
    } },

  { name: "ESC dismisses and what was typed stays as text",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys([..."weekly"]);
      await p.keys([KEY.Escape]);
      return [[await isUp(p), await pane(p)], [false, "see @weekly"]];
    } },

  { name: "deleting the @ closes the widget",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys([..."we"]);
      await p.keys([KEY.Backspace, KEY.Backspace, KEY.Backspace]);
      return [[await isUp(p), await pane(p)], [false, "see "]];
    } },

  { name: "a selected region opens the widget over its own text, writing nothing",
    async run(p) {
      await select("see weekly notes", 4, 10)(p);
      await p.keys(AT);
      return [[await isUp(p), await hot(p), await pane(p), await focused(p)],
              [true, "MDE weekly", "see weekly notes", "dpara"]];
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
      await p.keys([KEY.Escape]);
      return [[await isUp(p), await pane(p)], [false, "see weekly notes"]];
    } },

  { name: "over a selection the widget owns the query and shows it",
    async run(p) {
      await select("see weekly notes", 4, 10)(p);
      await p.keys(AT);
      await p.keys([{ down: KEY.Backspace }, { up: KEY.Backspace }]);
      await p.keys([..."x"]);
      const shownQ = await p.eval(() => {
        const q = document.getElementById("acquery");
        return q.hidden ? null : q.textContent;
      });
      return [[shownQ, await pane(p)], ["link to weeklx", "see weekly notes"]];
    } },

  { name: "C-u @ over a selection asks the kind, then keeps the region's words",
    async run(p) {
      await select("see weekly notes", 4, 10)(p);
      await p.keys(CU); await p.keys(AT);
      const st = await stage(p);
      await p.keys([..."depends"]);
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
      return [await isUp(p), true];
    } },

  { name: "ESC after settling a kind takes the KIND back out too",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await p.keys([..."depends"]);
      await p.keys([KEY.Enter]);
      const settled = await pane(p);
      await p.keys([KEY.Escape]);
      return [[settled, await isUp(p), await pane(p)],
              ["see @depends ", false, "see @"]];
    } },

  { name: "the kind goes but a half-typed TARGET stays, as a plain @ would",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await p.keys([..."depends"]);
      await p.keys([KEY.Enter]);
      await p.keys([..."week"]);
      await p.keys([KEY.Escape]);
      return [await pane(p), "see @week"];
    } },

  { name: "ESC before the kind is settled leaves what was typed, as ever",
    async run(p) {
      await seed("see ")(p);
      await p.keys(CU); await p.keys(AT);
      await p.keys([..."depends"]);
      await p.keys([KEY.Escape]);
      return [[await isUp(p), await pane(p)], [false, "see @depends"]];
    } },

  { name: "over a selection a cancelled kind leaves the pane untouched",
    async run(p) {
      await select("see weekly notes", 4, 10)(p);
      await p.keys(CU); await p.keys(AT);
      await p.keys([..."depends"]);
      await p.keys([KEY.Enter]);
      await p.keys([KEY.Escape]);
      return [[await isUp(p), await pane(p)], [false, "see weekly notes"]];
    } },

  { name: "typing on past a dead query keeps the pane usable",
    async run(p) {
      await seed("see ")(p);
      await p.keys(AT);
      await p.keys([..."zzz"]);
      return [[await isUp(p), await rows(p), await pane(p)],
              [true, [], "see @zzz"]];
    } },
];
