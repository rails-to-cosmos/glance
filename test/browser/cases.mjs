// THE CASES, each a COMPUTED reading the text suite cannot take, and each
// naming the bug it exists for.  FIGURES ARE RELATIONAL; a case RETURNS them.

const assert = (ok, why) => { if (!ok) throw new Error(why); };
const px = (n) => `${Math.round(n * 10) / 10}px`;

/** Open the sheet over ROW by URL, which is the page's own contract. */
async function sheet(p, base, row) {
  await p.goto(`${base}/?page=sheet&row=${row}`);
  await p.until(() => !!document.querySelector("#modal.on"),
                `the sheet to open over ${row}`);
  await p.until(() => document.querySelectorAll("#mdoc .de").length > 1,
                `${row}'s body to draw more than a headline`);
}

/** The sheet over ROW with its first paragraph open for editing. */
async function paraOpen(p, base, row) {
  await sheet(p, base, row);
  await p.press("n");                                   // onto the paragraph
  await p.press("RET");                                 // open it
  await p.until(() => document.getElementById("dpara").classList.contains("on"),
                "the paragraph edit box to open");
}

const pickerUp = (p, why) =>
  p.until(() => !!document.querySelector("#refer.on .tv-table tbody tr"),
          why || "the picker to raise with rows");
const pickerGone = (p, why) =>
  p.until(() => !document.getElementById("refer").classList.contains("on"), why);

/** The picker raised over ROW's first paragraph, at a word boundary. */
async function pickerOver(p, base, row) {
  await paraOpen(p, base, row);
  await p.type(" ");                    // `@' opens on a word boundary alone
  await p.press("@");
  await pickerUp(p);
}
// A PAGE FUNCTION IS SERIALIZED WHOLE, so it may call nothing of this module's:
// these are handed to `eval'/`until' entire, and a wrapper that CALLED one would
// reach the page naming something that is not there.
/** THE EDITOR IS SUMMONED, NOT RESIDENT: is its bar drawn at all? */
const barDrawn = () => {
  const bar = document.querySelector("#rmount .tv-bar");
  return !!bar && getComputedStyle(bar).display !== "none";
};
const barAway = () => {
  const bar = document.querySelector("#rmount .tv-bar");
  return !bar || getComputedStyle(bar).display === "none";
};
const boxKeys = () => document.activeElement.tagName === "INPUT"
                   && document.getElementById("rmount").contains(document.activeElement);
const boxFocused = (p, why) => p.until(boxKeys, why || "the filter box to take the focus");
const boxAway = (p, why) => p.until(barAway, why || "the summoned editor to go");

export default [

// cb6db85.  THE BOX GREW AND STOOD OVER THE DOCUMENT.  Where the next line
// ENDS UP is unaskable in TestServe.hs: the node harness returns zeroed rects.
{ name: "an open edit moves the line under it down, never covers it",
  async run(p, base) {
    await sheet(p, base, "drv-box");
    await p.press("n");                                   // onto the paragraph
    await p.press("RET");                                 // open it
    // `placeEdit' sizes the box a turn after the raise, so wait for the two to agree.
    await p.until(() => {
      const b = document.getElementById("dpara");
      const at = document.querySelector("#mdoc .de.dat");
      if (!b || !b.classList.contains("on") || !at) return false;
      const h = b.getBoundingClientRect().height;
      return h > 0 && Math.abs(h - at.getBoundingClientRect().height) < 1;
    }, "the paragraph edit box to open over its block");
    const before = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const cs = getComputedStyle(document.getElementById("mdoc"));
      return { box: document.getElementById("dpara").getBoundingClientRect().height,
               line: parseFloat(cs.getPropertyValue("--g-doc-fs"))
                   * parseFloat(cs.getPropertyValue("--g-doc-lh")),
               under: at.nextElementSibling.getBoundingClientRect().top };
    });
    for (let i = 0; i < 10; i += 1) {
      await p.type(`typed line ${i}`);
      if (i < 9) await p.press("M-RET");   // the newline; S-RET commits
    }
    const seen = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const under = at.nextElementSibling;
      const box = document.getElementById("dpara").getBoundingClientRect();
      const a = at.getBoundingClientRect(), b = under.getBoundingClientRect();
      return { box: box.height, ends: box.bottom, block: a.height, blockEnds: a.bottom,
               starts: b.top, under: under.textContent.trim().slice(0, 32) };
    });
    assert(before.line > 0, `the pane declares no line box: --g-doc-fs * --g-doc-lh = ${before.line}`);
    assert(seen.box - before.box >= before.line * 5,
      `the box never grew for ten lines: ${px(before.box)} -> ${px(seen.box)}, `
      + `under five of its own ${px(before.line)} lines`);
    assert(seen.starts >= seen.ends - 1,
      `"${seen.under}" is covered: it starts at ${px(seen.starts)}, `
      + `the box ends at ${px(seen.ends)} (${px(seen.ends - seen.starts)} over it)`);
    assert(seen.starts >= seen.blockEnds - 1,
      `"${seen.under}" is under the BLOCK: it starts at ${px(seen.starts)}, `
      + `the block ends at ${px(seen.blockEnds)}`);
    return [`box ${px(before.box)} -> ${px(seen.box)}, block ${px(seen.block)}, `
      + `the line under it moved from ${px(before.under)} to ${px(seen.starts)}`];
  } },

// d7ba44b, Style.hs's `.d-draft'.  Elm emits an empty text node, so `:empty'
// cannot find a drawn-but-unwritten paragraph and the floor is DECLARED.  TWO
// READINGS: `.de.dat' carries a floor of its own, so a PROBE isolates `.d-draft'.
{ name: "a paragraph drawn before it is written still owns a line",
  async run(p, base) {
    await sheet(p, base, "drv-box");
    await p.press("n");
    await p.press("+");
    await p.until(() => !!document.querySelector("#mdoc .d-draft"),
                  "the drawn paragraph to appear");
    const seen = await p.eval(() => {
      const cs = getComputedStyle(document.getElementById("mdoc"));
      const line = parseFloat(cs.getPropertyValue("--g-doc-fs"))
                 * parseFloat(cs.getPropertyValue("--g-doc-lh"));
      const real = document.querySelector("#mdoc .d-draft");
      const probe = document.createElement("div");
      probe.className = "de d-para d-draft";
      real.parentElement.append(probe);
      const h = probe.getBoundingClientRect().height;
      probe.remove();
      return { real: real.getBoundingClientRect().height, probe: h, line };
    });
    assert(seen.line > 0, `the pane declares no line box: --g-doc-fs * --g-doc-lh = ${seen.line}`);
    assert(seen.real >= seen.line - 0.5,
      `the drawn paragraph collapsed to ${px(seen.real)}, under one line of ${px(seen.line)}`);
    assert(seen.probe >= seen.line - 0.5,
      `.d-draft alone collapsed to ${px(seen.probe)}, under one line of ${px(seen.line)}`);
    return [`the drawn row is ${px(seen.real)}, `
      + `.d-draft alone is ${px(seen.probe)}, one line is ${px(seen.line)}`];
  } },

// 14e13d9.  ONE gesture over ONE queue must paint one red on both surfaces,
// which `paletteSweep' cannot ask: it compares the served TEXT, never a mount.
{ name: "a flag paints one red on both surfaces, and draws its inset edge",
  async run(p, base) {
    await p.goto(`${base}/`);
    await p.until(() => !!document.querySelector("#app table tbody tr"),
                  "the table to mount rows");
    await p.press("d");                                   // dired's flag
    await p.until(() => !!document.querySelector("#app tr.tv-flagged"),
                  "the table row to wear its flag");
    const table = await p.eval(() => {
      const rgb = (v) => { const d = document.createElement("div");
        d.style.color = v; document.body.append(d);
        const c = getComputedStyle(d).color; d.remove(); return c; };
      // `--tv-*' lives on `.tv-root' and `--g-*' on the document element.
      const root = getComputedStyle(document.documentElement);
      const tv = getComputedStyle(document.querySelector("#app .tv-root"));
      const tr = document.querySelector("#app tr.tv-flagged");
      const td = tr.querySelector("td");
      return { flag: rgb(tv.getPropertyValue("--tv-flag").trim()),
               bad: rgb(root.getPropertyValue("--g-bad").trim()),
               ground: getComputedStyle(tr).backgroundColor,
               plain: getComputedStyle(tr.parentElement
                 .querySelector("tr:not(.tv-flagged)")).backgroundColor,
               edge: getComputedStyle(td).boxShadow };
    });
    assert(table.bad === table.flag,
      `--g-bad paints ${table.bad} and --tv-flag paints ${table.flag}`);
    assert(table.ground !== table.plain,
      `a flagged row's ground is ${table.ground}, the same as an unflagged one`);
    assert(/inset/.test(table.edge),
      `the table's flagged cell draws no inset edge: box-shadow is "${table.edge}"`);

    await sheet(p, base, "drv-box");
    await p.press("n");
    await p.press("d");
    await p.until(() => !!document.querySelector("#mdoc .de.dfl"),
                  "the pane's row to wear its flag");
    const pane = await p.eval(() => {
      const fl = document.querySelector("#mdoc .de.dfl");
      return { edge: getComputedStyle(fl).boxShadow,
               ground: getComputedStyle(fl).backgroundColor };
    });
    // Both strings came out of the same engine, so the red is compared as spelled.
    assert(pane.edge.includes(table.flag) && /inset/.test(pane.edge),
      `the pane's flag edge is "${pane.edge}", the table's red is ${table.flag}`);
    assert(pane.ground !== "rgba(0, 0, 0, 0)",
      `the pane's flagged row has no ground at all: ${pane.ground}`);
    return [`--g-bad and --tv-flag both paint ${table.flag}`,
            `the table's flagged ground is ${table.ground} against ${table.plain}`,
            `the pane's edge is "${pane.edge}"`];
  } },

// The KEY LINE is the one sideways scroller and is exempt; the reading is the
// DOCUMENT's scroller.  Every surface opens by its OWN URL, so no keymap copy.
{ name: "the page never scrolls, sideways or down, at any width or surface",
  async run(p, base) {
    // A SURFACE THAT NEVER ROSE would have this measure the table three times
    // over and report ok, so each is waited for BY ITS OWN CONTAINER.
    const HOST = { sheet: "#modal", config: "#config", tags: "#tags", links: "#links" };
    const seen = [];
    for (const [w, h] of [[360, 720], [800, 900], [1400, 900]]) {
      await p.size(w, h);
      for (const page of ["", "sheet", "config", "tags", "links"]) {
        const q = page ? `?page=${page}&row=drv-wide` : "";
        await p.goto(`${base}/${q}`);
        await p.until(() => !!document.querySelector("#app table tbody tr"),
                      `the table to mount at ${w}px`);
        if (page) await p.until((host) => !!document.querySelector(`${host}.on`),
                                `${page} to raise at ${w}px`, 12_000, HOST[page]);
        const at = await p.eval(() => {
          const e = document.scrollingElement;
          const past = [...document.querySelectorAll("body *")]
            .filter((n) => !n.closest("#keys")
                        && n.getBoundingClientRect().right > innerWidth + 1)
            .map((n) => n.id || String(n.className).slice(0, 30)).slice(0, 5);
          return { over: e.scrollWidth - e.clientWidth,
                   down: e.scrollHeight - e.clientHeight, past };
        });
        assert(at.over <= 1,
          `"${page || "table"}" at ${w}px scrolls ${at.over}px sideways; `
          + `past the edge: ${at.past.join(", ") || "nothing the sweep could name"}`);
        assert(at.down <= 1,
          `"${page || "table"}" at ${w}px scrolls ${at.down}px down`);
        seen.push(`${page || "table"}@${w}: ${at.over}/${at.down}`);
      }
    }
    return [`sideways/down overflow, per surface and width: ${seen.join("  ")}`];
  } },

// A POPUP CLAMPS AND SCROLLS INSIDE, as a CHAIN: `--g-pop-max' derives the foot
// margin from the HEAD's (Style.hs).  Nothing measures the chain, and a box
// taller than the viewport is a reader who cannot reach its foot.
{ name: "a popup clamps inside the viewport at every height",
  async run(p, base) {
    const seen = [];
    for (const [w, h] of [[1400, 900], [900, 480], [700, 360]]) {
      await p.size(w, h);
      for (const page of ["sheet", "config"]) {
        await p.goto(`${base}/?page=${page}&row=drv-box`);
        await p.until(() => { const n = document.querySelector("#modal.on #sheet, #config.on #cbox");
                              return !!n && n.getBoundingClientRect().height > 0; },
                      `${page} to raise at ${h}px tall`);
        const at = await p.eval(() => {
          const n = document.querySelector("#modal.on #sheet, #config.on #cbox");
          const r = n.getBoundingClientRect();
          return { top: r.top, bottom: r.bottom, height: r.height, view: innerHeight,
                   cap: getComputedStyle(n).height,
                   id: n.id || String(n.className) };
        });
        assert(at.bottom <= at.view + 1,
          `#${at.id} at ${h}px tall runs ${px(at.top)}..${px(at.bottom)}, `
          + `${px(at.bottom - at.view)} past the viewport's ${px(at.view)} `
          + `(the box is ${px(at.height)} tall, its cap says ${at.cap})`);
        assert(at.top >= -1, `#${at.id} at ${h}px tall starts at ${px(at.top)}, above the viewport`);
        seen.push(`${page}@${h}: ${px(at.top)}..${px(at.bottom)} of ${px(at.view)}`);
      }
    }
    return [seen.join("  ")];
  } },

// 80c3732: the Elm view invented a CSS variable name and the state badge lost
// its colour.  ONE KEYWORD, TWO SURFACES, ONE PAINTED COLOUR.
{ name: "a badge in the sheet paints the hue its column paints in the table",
  async run(p, base) {
    await p.goto(`${base}/`);
    await p.until(() => !!document.querySelector("#app .tv-pill"),
                  "the table to draw a state badge");
    const table = await p.eval(() => {
      const pill = [...document.querySelectorAll("#app .tv-pill")]
        .find((n) => n.textContent.trim() === "TODO")
        || document.querySelector("#app .tv-pill");
      const ink = getComputedStyle(pill).color;
      return { word: pill.textContent.trim(), ink,
               plain: getComputedStyle(document.body).color };
    });
    assert(table.ink !== table.plain,
      `the table's ${table.word} badge paints ${table.ink}, the page's own ink`);
    await sheet(p, base, "drv-box");
    const sheetInk = await p.eval((word) => {
      const cell = [...document.querySelectorAll("#mdoc .dc-state")]
        .find((n) => n.textContent.trim() === word);
      return cell ? getComputedStyle(cell).color : null;
    }, table.word);
    assert(sheetInk, `the sheet drew no ${table.word} state cell at all`);
    assert(sheetInk === table.ink,
      `${table.word} paints ${table.ink} in the table and ${sheetInk} in the sheet`);
    return [`${table.word} paints ${table.ink} on both surfaces, `
      + `against the page's own ${table.plain}`];
  } },

// AND CONTENT SITS UNDER THE TITLE TEXT (AGENTS.hs).  PADDING rather than a
// margin — a margin would take the selection wash off the left of the line.
{ name: "a paragraph is indented under the title text, and keeps its full ground",
  async run(p, base) {
    await sheet(p, base, "drv-box");
    const seen = await p.eval(() => {
      const head = document.querySelector("#mdoc .d-head");
      const para = document.querySelector("#mdoc .d-para");
      const title = head.querySelector(".dc-title") || head;
      const list = document.querySelector("#dlist > div") || document.getElementById("dlist");
      return { headLeft: head.getBoundingClientRect().left,
               titleLeft: title.getBoundingClientRect().left,
               paraLeft: para.getBoundingClientRect().left,
               paraText: para.querySelector(".dp")
                 ? para.querySelector(".dp").getBoundingClientRect().left : null,
               listLeft: list.getBoundingClientRect().left };
    });
    assert(seen.paraLeft <= seen.listLeft + 1,
      `the paragraph's BOX starts at ${px(seen.paraLeft)}, inside the list's `
      + `${px(seen.listLeft)}: a margin, so the selection wash is off the line`);
    assert(seen.paraText > seen.headLeft + 1,
      `the paragraph's TEXT starts at ${px(seen.paraText)}, at the stars' own `
      + `${px(seen.headLeft)} rather than under the title`);
    return [`stars at ${px(seen.headLeft)}, title at ${px(seen.titleLeft)}, `
      + `paragraph box at ${px(seen.paraLeft)} and its text at ${px(seen.paraText)}`];
  } },

// EVERY SELECTION IN THE PANE IS A GROUND, never a line (AGENTS.hs).
// `groundSweep' greps the served TEXT; what the row PAINTS needs an engine.
{ name: "the cursor in the pane is a ground, and the pane that lost the keys draws none",
  async run(p, base) {
    await sheet(p, base, "drv-box");
    await p.press("n");
    const seen = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const off = [...document.querySelectorAll("#mdoc .de")].find((n) => n !== at);
      const cs = getComputedStyle(at);
      return { on: cs.backgroundColor, off: getComputedStyle(off).backgroundColor,
               deco: cs.textDecorationLine, outline: cs.outlineStyle,
               border: cs.borderTopStyle };
    });
    assert(seen.on !== seen.off,
      `the cursor row and an ordinary row both paint ${seen.on}`);
    assert(seen.deco === "none" && seen.outline === "none" && seen.border === "none",
      `the cursor row is drawn with a line: decoration ${seen.deco}, `
      + `outline ${seen.outline}, border ${seen.border}`);
    // TAB crosses to the panel, and A CURSOR IS ONLY DRAWN WHERE THE KEYS ARE.
    await p.press("TAB");
    await p.until(() => document.getElementById("mprops").classList.contains("on"),
                  "the panel to take the keys");
    const gone = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return at ? getComputedStyle(at).backgroundColor : null;
    });
    assert(gone === seen.off,
      `the pane that lost the keys still paints its cursor ${gone} against ${seen.off}`);
    return [`cursor ${seen.on} against ${seen.off}; with the keys away it paints ${gone}`];
  } },

// A LEAF IS ONE LINE OF THE FIELD THAT COVERS IT.  `.de' pads every stop and a
// composite's leaves spent it AGAIN, walking 2px per leaf off the field's grid.
{ name: "a composite's drawn lines sit on the same grid as the field over it",
  async run(p, base) {
    await sheet(p, base, "drv-plan");
    await p.press("n"); await p.press("n");      // onto the whole-list composite
    await p.press("RET");
    // THE CLASS FLIPS BEFORE THE PANE HAS DRAWN, as `placeEdit' does in the case
    // above: waiting on the box alone found an empty composite one run in eight
    // (docs/bugs/2026-08-17-the-composite-case-measures-an-empty-pane).  The wait
    // is on the MEASUREMENT, so what is asserted is what was waited for.
    await p.until(() => {
      const box = document.getElementById("dpara");
      const at = document.querySelector("#mdoc .de.dat");
      return box.classList.contains("on") && !!at && at.querySelectorAll(".de").length > 0;
    }, "the edit to open over a list with leaves drawn in it");
    const s = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const t = document.getElementById("dtext");
      const line = parseFloat(getComputedStyle(at).lineHeight);
      const top = at.getBoundingClientRect().top;
      const leaves = [...at.querySelectorAll(".de")].map((e, i) => ({
        i, cls: e.className,
        drawn: +(e.getBoundingClientRect().top - top).toFixed(1),
        pad: getComputedStyle(e).paddingTop,
        h: +e.getBoundingClientRect().height.toFixed(1),
      }));
      return { line, h: at.getBoundingClientRect().height, leaves,
               atPad: getComputedStyle(at).paddingTop,
               fieldH: t.getBoundingClientRect().height, scrollH: t.scrollHeight,
               text: t.value.split("\n").length };
    });
    const out = [`line ${s.line}px, row h${s.h}, field h${s.fieldH} scroll${s.scrollH}, `
      + `${s.text} text lines, row padTop ${s.atPad}`];
    // EVERY ASSERTION BELOW RIDES THIS LIST, so an empty one reports green
    // having measured nothing.
    assert(s.leaves.length > 0, "the pane drew no leaves to measure");
    let uniform = parseFloat(s.atPad);
    for (const l of s.leaves) {
      const drift = +(l.drawn - uniform).toFixed(1);
      out.push(`  leaf${l.i} drawn@${l.drawn} uniform@${uniform.toFixed(1)} `
        + `drift ${drift} (h${l.h} padTop${l.pad})`);
      assert(Math.abs(drift) <= 0.5,
        `leaf ${l.i} is drawn at ${l.drawn} where the field's line ${l.i} sits at `
        + `${uniform.toFixed(1)} — ${drift}px out, and it accumulates per leaf`);
      assert(Math.abs(l.h % s.line) <= 0.5,
        `leaf ${l.i} stands ${l.h}px, off the ${s.line}px line box by `
        + `${(l.h % s.line).toFixed(1)}px, so every leaf under it is pushed down`);
      uniform += Math.round(l.h / s.line) * s.line;
    }
    return out;
  } },

// AN EDIT BOX IS THE BLOCK IT COVERS, and nothing measured the two TOGETHER.
// Both padding cases matter, and so does a CYRILLIC item that WRAPS.
// A COMMIT CLOSES WHAT THE TYPING OPENED.  The completion is the SCANNER's, so
// only an engine running the compiled Elm can see it: `Scan.closers' is unit
// tested over lines, and this is the one reading where a TYPED line reaches it.
{ name: "a committed block opener arrives with its closer",
  async run(p, base) {
    await sheet(p, base, "drv-box");
    await p.press("n");                                   // onto the paragraph
    await p.press("+");                                   // a new one under it
    await p.until(() => !!document.querySelector("#dpara.on"),
                  "the draft edit to open");
    await p.type("#+begin_src elisp");
    await p.press("RET");                                 // commit
    // THE WATCH DELIVERS, so the reading waits for the row rather than a duration.
    await p.until(() => [...document.querySelectorAll("#mdoc .de")]
                          .some((e) => e.textContent.includes("#+end_src")),
                  "the closer to arrive over the watch");
    // ONE STOP: balanced, the two lines are a BLOCK, which the scanner draws as
    // a single region — so the order is read INSIDE the row rather than across.
    const seen = await p.eval(() => {
      const rows = [...document.querySelectorAll("#mdoc .de")].map((e) => e.textContent);
      const at = rows.findIndex((t) => t.includes("#+begin_src"));
      const text = at === -1 ? "" : rows[at];
      return { at, text, opens: text.indexOf("#+begin_src"),
               shuts: text.indexOf("#+end_src"), rows: rows.length };
    });
    assert(seen.at !== -1, "the opener never landed in any row");
    assert(seen.shuts !== -1,
      `the closer was never written: the row reads ${JSON.stringify(seen.text)}`);
    assert(seen.shuts > seen.opens,
      `the closer precedes its opener: ${JSON.stringify(seen.text)}`);
    // The ARGUMENTS are the opener's alone.
    assert(!seen.text.includes("#+end_src elisp"),
      `the closer carried the opener's arguments: ${JSON.stringify(seen.text)}`);
    // AN EMPTY BLOCK GETS A LINE TO TYPE ON, so the two are never adjacent.
    assert(/#\+begin_src elisp\n\n#\+end_src/.test(seen.text),
      `the empty block has no line to type on: ${JSON.stringify(seen.text)}`);
    return [`row ${seen.at} of ${seen.rows} reads ${JSON.stringify(seen.text)}`];
  } },

{ name: "the open box covers its row edge to edge, at the row's own metrics",
  async run(p, base) {
    const read = () => {
      const one = (e) => {
        const r = e.getBoundingClientRect(), c = getComputedStyle(e);
        return { cls: e.className || e.id, left: Math.round(r.left * 10) / 10,
                 w: Math.round(r.width * 10) / 10, h: Math.round(r.height * 10) / 10,
                 padL: c.paddingLeft, padR: c.paddingRight,
                 font: c.fontSize + "/" + c.lineHeight, fam: c.fontFamily.slice(0, 24),
                 bg: c.backgroundColor };
      };
      const dat = [...document.querySelectorAll("#mdoc .dat")].map(one);
      const b = document.getElementById("dpara");
      const t = document.getElementById("dtext");
      return { dat, box: b.classList.contains("on") ? one(b) : null,
               field: b.classList.contains("on")
                 ? { ...one(t), bar: t.offsetWidth - t.clientWidth,
                     scrollH: t.scrollHeight, clientH: t.clientHeight } : null };
    };
    await sheet(p, base, "drv-plan");
    const out = [];
    // A paragraph and the whole-list composite carry the title indent; leaves do not.
    const walk = ["n", "n", "f", "n", "n", "n"];
    for (let i = 0; i < walk.length; i += 1) {
      await p.press(walk[i]);
      await p.press("RET");
      await p.until(() => document.getElementById("dpara").classList.contains("on"),
                    "the edit to open");
      // `placeEdit' sizes the box a turn after the raise: waiting for the two to
      // AGREE is waiting for it to have run.  An unsized box is one line tall.
      await p.until(() => {
        const b = document.getElementById("dpara").getBoundingClientRect();
        const at = document.querySelector("#mdoc .de.dat");
        return at && b.height > 0
          && Math.abs(b.height - at.getBoundingClientRect().height) < 1;
      }, "the box to be placed over its row");
      const s = await p.eval(read);
      const row = s.dat[s.dat.length - 1];
      out.push(`leaf ${i} ${row.cls}`);
      out.push(`  row   x${row.left} w${row.w} h${row.h} padL${row.padL} ${row.font}`);
      out.push(`  box   x${s.box.left} w${s.box.w} h${s.box.h}`);
      out.push(`  field x${s.field.left} w${s.field.w} h${s.field.h} padL${s.field.padL} `
        + `${s.field.font} bar${s.field.bar} scroll${s.field.scrollH}/${s.field.clientH}`);
      const note = `${row.cls}: row x${row.left} w${row.w} h${row.h} `
        + `pad${row.padL} ${row.font} / box x${s.box.left} w${s.box.w} h${s.box.h} `
        + `/ field pad${s.field.padL} ${s.field.font} bar${s.field.bar}`;
      out.push(`  ${note}`);
      assert(Math.abs(s.box.left - row.left) <= 1 && Math.abs(s.box.w - row.w) <= 1,
        `the box does not cover its row — ${note}`);
      assert(s.field.font === row.font,
        `the field renders in another metric than its row — ${note}`);
      assert(s.field.padL === row.padL,
        `the field is inset unlike its row, so its text sits elsewhere — ${note}`);
      // A SCROLLBAR THAT TAKES LAYOUT WIDTH WRAPS THE FIELD NARROWER than the
      // div it covers.  Chromium overlays its own and measures 0 here.
      assert(s.field.bar === 0,
        `the field carries a ${s.field.bar}px scrollbar, so it wraps narrower `
        + `than the row it covers — ${note}`);
      assert(Math.abs(s.box.h - row.h) <= 1,
        `the box stands ${s.box.h}px over a ${row.h}px row — ${note}`);
      await p.press("ESC");
      await p.until(() => !document.getElementById("dpara").classList.contains("on"),
                    "the edit to close");
    }
    return out;
  } },

// A COLUMN SIZED BY ITS OWN BADGE CAME UP A FIFTH OF A PIXEL SHORT.  The pill's
// 16px of padding was allowed for as 2 characters, which is 15.6px at a 13px
// monospace face; the pill is an inline-block and cannot be cut, so what
// `text-overflow' drew was the WHOLE badge with an ellipsis after it — `[#A]…'
// in the priority column, whose widest cell IS the badge.  Every other badge
// column hid it, sized by a cell longer than its pill.  Unaskable in
// TestServe.hs: the node harness returns zeroed rects.
{ name: "a badge sized by its own pill is drawn whole, never ellipsized",
  async run(p, base) {
    const seen = [];
    for (const [w, h] of [[1400, 900], [1024, 768]]) {
      await p.size(w, h);
      await p.goto(`${base}/`);
      await p.until(() => document.querySelectorAll("#app td .tv-pill").length > 0,
                    `a badge to draw at ${w}px`);
      const at = await p.eval(() => [...document.querySelectorAll("#app td .tv-pill")].map((n) => {
        const td = n.closest("td"), cs = getComputedStyle(td);
        // FRACTIONAL: `clientWidth' is a rounded integer and rounds the fifth of
        // a pixel this case exists for out of the reading.
        const inner = td.getBoundingClientRect().width
          - parseFloat(cs.paddingLeft) - parseFloat(cs.paddingRight)
          - parseFloat(cs.borderLeftWidth) - parseFloat(cs.borderRightWidth);
        return { text: n.textContent, pill: n.getBoundingClientRect().width, inner };
      }));
      for (const b of at)
        assert(b.pill <= b.inner + 0.001,
          `the "${b.text}" pill measures ${px(b.pill)} in a cell holding ${px(b.inner)} `
          + `at ${w}px, so an ellipsis is drawn beside a badge that cannot be cut`);
      seen.push(...at.map((b) => `${b.text}@${w}: ${px(b.pill)} in ${px(b.inner)}`));
    }
    return [`each pill against its cell's content box: ${seen.join("  ")}`];
  } },

// `@' IN THE SHEET LINKS A HEADLINE INTO THE PROSE.  The picker is a table-view
// mount, so what is asked here is the WIRING: that it raises at the caret over
// the rows /refer offers, that RET writes the link into the box the sheet
// commits, and that an `@' mid-word is still an `@'.
{ name: "@ in the sheet links the row under the cursor into the prose",
  async run(p, base) {
    await paraOpen(p, base, "drv-box");
    await p.type(" see ");
    await p.press("@");
    await pickerUp(p);
    const shown = await p.eval(() => {
      const box = document.getElementById("rbox").getBoundingClientRect();
      const pane = document.getElementById("mdoc").getBoundingClientRect();
      return { rows: document.querySelectorAll("#rmount .tv-table tbody tr").length,
               // it hangs at the caret rather than centring over the page
               placed: box.top > pane.top - 200 && box.width > 100,
               veil: getComputedStyle(document.getElementById("refer")).backgroundColor };
    });
    assert(shown.rows > 0, "the picker mounted no rows");
    assert(shown.placed, "the picker did not hang near the pane");
    assert(/rgba\(0, 0, 0, 0\)|transparent/.test(shown.veil),
      `the picker drew a veil (${shown.veil}); it is a completion, not a surface`);

    await p.press("RET");
    await pickerGone(p, "the picker to close on RET");
    const wrote = await p.eval(() => document.getElementById("dtext").value);
    assert(/\[\[glance:[^\]]+\]\[[^\]]+\]\]/.test(wrote),
      `RET wrote no org link into the box: ${JSON.stringify(wrote)}`);
    assert(!/@\[\[glance:/.test(wrote),
      `the link was written BESIDE the @ rather than over it: ${JSON.stringify(wrote)}`);

    assert(await p.eval(() => document.getElementById("dpara").classList.contains("on")),
      "taking a row closed the paragraph the link was written into");

    // THE `@' IS WRITTEN THE MOMENT IT IS TYPED, and dismissing the picker leaves
    // it standing: the reader typed a character and must be able to keep it.
    await p.type(" ");
    await p.press("@");
    await pickerUp(p, "the picker to raise a second time");
    const marked = await p.eval(() => document.getElementById("dtext").value);
    assert(marked.endsWith("@"),
      `the @ was not written while the picker stood: ${JSON.stringify(marked.slice(-12))}`);
    await p.press("ESC");
    await pickerGone(p, "the picker to go on ESC");
    const kept = await p.eval(() => document.getElementById("dtext").value);
    assert(kept.endsWith("@"), `ESC took the @ with it: ${JSON.stringify(kept.slice(-12))}`);
    // AND `@' IS A CHARACTER FIRST: mid-word it is text, not a command.
    await p.type("mail me at dmitry");
    await p.press("@");
    const after = await p.eval(() => ({
      up: document.getElementById("refer").classList.contains("on"),
      text: document.getElementById("dtext").value }));
    assert(!after.up, "an @ inside a word raised the picker");
    assert(after.text.endsWith("dmitry@"),
      `the literal @ was not written: ${JSON.stringify(after.text.slice(-24))}`);
    return [`the box holds ${JSON.stringify(wrote.slice(0, 60))}`];
  } },

// A SELECTED REGION BECOMES THE LINK, and its OWN WORDS are what the link reads
// as.  They are not a query: seeding the filter with the reader's prose narrowed
// the store by an accident of phrasing.
{ name: "@ over a selected region links it, and the region is no filter",
  async run(p, base) {
    await paraOpen(p, base, "drv-box");
    await p.type(" the weekly note ");
    // select `weekly' — the words the link is to read as
    await p.eval(() => {
      const box = document.getElementById("dtext");
      const at = box.value.indexOf("weekly");
      box.setSelectionRange(at, at + "weekly".length);
    });
    await p.press("@");
    await pickerUp(p, "the picker to raise over the region");
    const chips = await p.eval(() =>
      [...document.querySelectorAll("#rmount .tv-chip")].map((n) => n.textContent));
    assert(!chips.some((c) => c.includes("weekly")),
      `the region reached the filter as a chip: ${JSON.stringify(chips)}`);
    await p.press("RET");
    await pickerGone(p, "the picker to close");
    const wrote = await p.eval(() => document.getElementById("dtext").value);
    assert(/\[\[glance:[^\]]+\]\[weekly\]\]/.test(wrote),
      `the link does not read as the region: ${JSON.stringify(wrote.slice(-70))}`);
    assert(!/weekly note.*weekly note/.test(wrote), "the region was duplicated");
    return [`chips ${JSON.stringify(chips)} · box ends ${JSON.stringify(wrote.slice(-46))}`];
  } },

// `@' WRITES INTO WHATEVER BOX IS OPEN.  Over a title edit that is the TITLE:
// drafting a body line under it and linking there answers a question nobody asked.
{ name: "@ in the title editor links into the title itself",
  async run(p, base) {
    await sheet(p, base, "drv-box");
    const before = await p.eval(() => document.querySelectorAll("#mdoc .dline").length);
    await p.press("RET");                                 // the title edit
    await p.until(() => document.getElementById("dtin") &&
                        document.activeElement.id === "dtin",
                  "the title edit box to take the focus");
    await p.type(" see ");
    await p.press("@");
    await pickerUp(p, "the picker to raise over the title");
    await p.press("RET");
    await pickerGone(p, "the picker to close on RET");
    const got = await p.eval(() => ({
      title: document.getElementById("dtin") ? document.getElementById("dtin").value : null,
      body: document.getElementById("dtext") ? document.getElementById("dtext").value : "",
      lines: document.querySelectorAll("#mdoc .dline").length }));
    assert(got.title && /\[\[glance:[^\]]+\]\[[^\]]+\]\]/.test(got.title),
      `the link did not land in the title: ${JSON.stringify(got.title)}`);
    assert(!/\[\[glance:/.test(got.body), "the link also went into the body");
    assert(got.lines === before,
      `linking from the title drew ${got.lines - before} line(s) into the body`);
    return [`title ${JSON.stringify(got.title.slice(0, 56))}`];
  } },

// ESC IN THE PICKER'S FILTER IS ONE STEP: the half-typed filter goes AND the
// cursor lands on a row.  Stopping at an emptied box leaves the reader in an
// editor they were already done with, and a second ESC away from the rows.
{ name: "ESC in the picker's filter drops the edit and stands on a row",
  async run(p, base) {
    await pickerOver(p, base, "drv-box");
    assert(!(await p.eval(barDrawn)),
      "the filter editor sits on the picker before anyone asked for it");
    await p.press("/");
    await boxFocused(p);
    assert(await p.eval(barDrawn), "/ summoned no filter editor");
    // AND IT COMES ON THE CHIPS' OWN LINE, not as a second stripe above them.
    const oneLine = await p.eval(() => {
      const bar = document.querySelector("#rmount .tv-bar").getBoundingClientRect();
      const chips = document.querySelector("#rmount .tv-chips").getBoundingClientRect();
      const mid = (r) => r.top + r.height / 2;
      return { gap: Math.round(Math.abs(mid(bar) - mid(chips))),
               apart: Math.round(bar.left - chips.right) };
    });
    assert(oneLine.gap <= 3,
      `the editor sits on its own line, ${oneLine.gap}px off the chips' middle`);
    assert(oneLine.apart >= 0, "the editor overlaps the chips");
    await p.type("zzz");
    await p.press("ESC");
    const after = await p.eval(() => ({
      up: document.getElementById("refer").classList.contains("on"),
      typing: document.activeElement.tagName === "INPUT" &&
              document.getElementById("rmount").contains(document.activeElement),
      text: (document.querySelector("#rmount input") || {}).value || "",
      rows: document.querySelectorAll("#rmount .tv-table tbody tr").length,
      at: document.querySelectorAll("#rmount tbody tr.tv-sel").length }));
    assert(after.up, "one ESC in the filter dismissed the whole picker");
    assert(!after.typing, "ESC left the keyboard in the filter box");
    assert(after.text === "", `ESC kept the abandoned edit: ${JSON.stringify(after.text)}`);
    assert(after.rows > 0, "ESC left no rows to pick from");
    assert(after.at === 1, `ESC left ${after.at} rows under the cursor, not one`);
    assert(!(await p.eval(barDrawn)), "ESC left the filter editor on the picker");
    return [`${after.rows} rows, cursor on one, editor away`];
  } },

// DEL'S RUNGS IN THE PICKER, and an EMPTIED summoned editor is now the rung
// under the typed text.  The box is the last thing the reader put there, so it
// is taken back before what is under it; the chips under it belong to the PICKER's own listener,
// which stands aside while the box holds the keys and so only sees the press
// once the box has gone.
{ name: "DEL on the picker's empty filter hides it, and the next DEL takes a chip",
  async run(p, base) {
    await pickerOver(p, base, "drv-box");
    // SELF-CONTAINED: a page function is serialized whole, so it can call no
    // helper of this module's — only the helpers passed to `eval'/`until' whole.
    const state = () => ({
      chips: document.querySelectorAll("#rmount .tv-chip[data-i]").length,
      typing: document.activeElement.tagName === "INPUT"
           && document.getElementById("rmount").contains(document.activeElement),
      up: document.getElementById("refer").classList.contains("on") });
    const seeded = await p.eval(state);
    assert(seeded.chips > 0,
      "the picker opened with no chip to take, so the rungs cannot be told apart");

    await p.press("/");
    await boxFocused(p);
    // The FIRST DEL over an empty box: the box goes, and nothing else does.
    await p.press("DEL");
    await boxAway(p, "the summoned editor to go on the first DEL");
    const once = await p.eval(state);
    assert(once.chips === seeded.chips,
      `the first DEL took a chip as well as the box: ${seeded.chips} -> ${once.chips}`);
    assert(!once.typing, "the first DEL left the keyboard in the box it emptied");
    assert(once.up, "the first DEL dismissed the whole picker");

    // The SECOND, with the box gone, reaches the shell and walks the query down.
    await p.press("DEL");
    await p.until((n) => document.querySelectorAll("#rmount .tv-chip[data-i]").length < n,
                  "the second DEL to take a chip", 8000, seeded.chips);
    const twice = await p.eval(state);
    assert(twice.up, "the second DEL dismissed the picker instead of taking a chip");
    return [`chips ${seeded.chips} → ${once.chips} (box away) → ${twice.chips}`];
  } },

// ONE PRESS, ONE PART — and the box rung is what made this reachable.  A held
// DEL over an emptied box hands the keyboard back on the FIRST press, so every
// repeat after it lands on the picker's own listener, and past that on the
// prose behind it: unguarded, one key walks the chip, the picker, the `@' and
// then the reader's sentence.  The renderer's `e.repeat' guard cannot help —
// by the second repeat the renderer is no longer the one being pressed.
{ name: "a held DEL over the picker's emptied box takes the box and nothing else",
  async run(p, base) {
    await pickerOver(p, base, "drv-box");
    const before = await p.eval(() => ({
      chips: document.querySelectorAll("#rmount .tv-chip[data-i]").length,
      prose: document.getElementById("dtext").value }));
    assert(before.chips > 0, "no chip to lose, so a runaway hold would look clean");
    assert(/@$/.test(before.prose), "the @ was not written, so its loss cannot be seen");

    await p.press("/");
    await boxFocused(p);
    await p.hold("DEL", 6);
    await boxAway(p, "the summoned editor to go on the first press of the hold");
    const after = await p.eval(() => ({
      chips: document.querySelectorAll("#rmount .tv-chip[data-i]").length,
      up: document.getElementById("refer").classList.contains("on"),
      prose: document.getElementById("dtext").value }));
    assert(after.chips === before.chips,
      `the hold walked on into the chips: ${before.chips} -> ${after.chips}`);
    assert(after.up, "the hold closed the picker it was only meant to unsummon");
    assert(after.prose === before.prose,
      `the hold reached the prose behind the picker: `
      + `${JSON.stringify(before.prose.slice(-24))} -> ${JSON.stringify(after.prose.slice(-24))}`);
    return [`six repeats after the box went: chips ${after.chips}, picker up, `
      + `prose ${JSON.stringify(after.prose.slice(-12))} unmoved`];
  } },

// THE KIND IS THE EDGE'S, and `K' is what declares it.  `k' is the previous row
// in the vim dialect, so the kind takes the shift.  The badge is drawn APART
// from the row's own badges and from the filter's chips — those narrow what is
// offered, this says what the link about to be written will BE — and the SLUG
// comes back from the server, so the page writes what org-glance would have.
{ name: "K declares the kind, and the link is written with the server's own slug",
  async run(p, base) {
    await pickerOver(p, base, "drv-box");
    const before = await p.eval(() => ({
      badge: document.getElementById("rkind").className,
      text: document.getElementById("rkind").textContent }));
    assert(before.badge !== "on", "the picker drew a kind before one was declared");

    await p.press("K");
    await p.until(() => document.getElementById("prompt").classList.contains("on"),
                  "the kind field to raise over the picker");
    // FREE TEXT IS HOW A KIND IS MINTED: this store has none to complete against.
    await p.type("Roasted By");
    await p.press("RET");
    await p.until(() => document.getElementById("rkind").className === "on",
                  "the kind badge to be drawn");
    const shown = await p.eval(() => {
      const b = document.getElementById("rkind");
      const cs = getComputedStyle(b);
      const pill = document.querySelector("#rmount .tv-pill");
      return { text: b.textContent, up: document.getElementById("refer").classList.contains("on"),
               // It reads as an OUTLINE where a row's badge is a washed ground.
               edge: cs.borderStyle, ink: cs.color, ground: cs.backgroundColor,
               rowBadge: pill ? getComputedStyle(pill).backgroundColor : "" };
    });
    assert(shown.up, "declaring a kind dismissed the picker");
    assert(shown.text === "kind:roasted-by",
      `the server's slug did not come back: ${JSON.stringify(shown.text)}`);
    assert(/dashed/.test(shown.edge),
      `the kind badge draws no outline of its own: ${shown.edge}`);
    assert(/rgba\(0, 0, 0, 0\)|transparent/.test(shown.ground),
      `the kind badge wears a ground like a row's badge: ${shown.ground}`);

    await p.press("RET");
    await pickerGone(p, "the picker to close on RET");
    const wrote = await p.eval(() => document.getElementById("dtext").value);
    assert(/\[\[glance:[^\]?]+\?kind=roasted-by\]\[[^\]]+\]\]/.test(wrote),
      `the kind did not reach the link: ${JSON.stringify(wrote.slice(-64))}`);
    return [`typed "Roasted By", wrote ${JSON.stringify(wrote.slice(-40))}`];
  } },

// THE FILTER IS THE SECOND ROUTE TO THE SAME KIND.  `kind:' is the EDGE's, so
// it never narrows the rows it would be written from: it comes OUT of the row
// query, lands on the badge, and stays in the strip as the chip that removes it.
{ name: "kind: typed into the picker's filter sets the kind and narrows nothing",
  async run(p, base) {
    await pickerOver(p, base, "drv-box");
    const rowsBefore = await p.eval(() =>
      document.querySelectorAll("#rmount .tv-table tbody tr").length);
    await p.press("/");
    await boxFocused(p);
    await p.type("kind:cites");
    await p.press("RET");
    await p.until(() => document.getElementById("rkind").className === "on",
                  "the kind badge to follow the filter");
    const seen = await p.eval(() => ({
      badge: document.getElementById("rkind").textContent,
      rows: document.querySelectorAll("#rmount .tv-table tbody tr").length,
      chips: [...document.querySelectorAll("#rmount .tv-chip[data-i]")].map((c) => c.textContent) }));
    assert(seen.badge === "kind:cites",
      `the filter's kind did not reach the badge: ${JSON.stringify(seen.badge)}`);
    assert(seen.rows === rowsBefore,
      `kind: narrowed the rows it is written from: ${rowsBefore} -> ${seen.rows}`);
    assert(seen.chips.some((c) => /kind:cites/.test(c)),
      `the strip lost the chip that removes it: ${JSON.stringify(seen.chips)}`);

    // THE CHIP IS THE CONTROL: taking it off takes the kind with it.
    await p.press("DEL");
    await p.until(() => document.getElementById("rkind").className !== "on",
                  "the badge to go with the chip");
    const after = await p.eval(() => ({
      rows: document.querySelectorAll("#rmount .tv-table tbody tr").length,
      badge: document.getElementById("rkind").textContent }));
    assert(after.badge === "", `the badge outlived its chip: ${JSON.stringify(after.badge)}`);
    return [`${rowsBefore} rows throughout, badge "kind:cites" then cleared`];
  } },

// THE PLATFORM PAINTS THE `<select>', and only `color-scheme' tells it which
// way.  The native WebKitGTK window drew every dropdown white on white: the
// page's own `color' was inherited over a control ground the UA painted from
// its LIGHT palette, because nothing declared the page was dark.  Chromium
// cannot reproduce GTK's paint, so what is asked here is the declaration the
// native window reads, and that no dropdown is given one colour twice.
{ name: "every dropdown declares the scheme its platform paints it in",
  async run(p, base) {
    await p.goto(`${base}/`);
    await p.until(() => !!document.querySelector("#app table tbody tr"),
                  "the table to mount rows");
    const SELECTS = ["themesel", "clayer", "nspace", "ngroup"];
    const read = (theme, ids) => {
      const root = document.documentElement;
      const was = root.dataset.theme;
      if (theme) root.dataset.theme = theme; else delete root.dataset.theme;
      const scheme = getComputedStyle(root).colorScheme;
      const boxes = ids.map((id) => {
        const el = document.getElementById(id);
        const cs = getComputedStyle(el);
        return { id, fg: cs.color, bg: cs.backgroundColor };
      });
      if (was) root.dataset.theme = was; else delete root.dataset.theme;
      return { scheme, boxes };
    };
    const dark = await p.eval(read, "dark", SELECTS);
    const light = await p.eval(read, "light", SELECTS);

    assert(/dark/.test(dark.scheme),
      `a dark page declares ${JSON.stringify(dark.scheme)}, so the platform paints `
      + `its controls light`);
    assert(/light/.test(light.scheme) && !/dark/.test(light.scheme),
      `a light page declares ${JSON.stringify(light.scheme)}`);

    // AND WE DO NOT DO IT TO OURSELVES: a box whose ink is its own ground is
    // unreadable however the platform paints around it.
    for (const seen of [dark, light])
      for (const b of seen.boxes)
        assert(b.fg !== b.bg,
          `#${b.id} draws ${b.fg} on ${b.bg} — one colour twice`);

    const missed = SELECTS.filter((id) => !dark.boxes.some((b) => b.id === id));
    assert(!missed.length, `the page lost a dropdown: ${missed.join(", ")}`);
    return [`${SELECTS.length} dropdowns · dark declares ${JSON.stringify(dark.scheme)}, `
      + `light ${JSON.stringify(light.scheme)}`];
  } },

// A BADGE COLUMN'S HEADER SITS OVER ITS BADGES' FIRST LETTER.  A pill sets its
// text in from the cell edge by its own padding, so a header aligned to the CELL
// is a header sitting a padding's width left of the words underneath it — which
// is what a reader actually scans.  Unaskable in TestServe.hs: the node harness
// returns zeroed rects.
{ name: "a badge column's header lines up with its badges, not with the cell",
  async run(p, base) {
    await p.size(1400, 900);
    await p.goto(`${base}/`);
    await p.until(() => !!document.querySelector("#app td .tv-pill"),
                  "a badge to draw");
    const at = await p.eval(() => {
      // The TEXT, not the box: a Range over the text node is where letters start.
      const textX = (n) => {
        const r = document.createRange();
        r.selectNodeContents(n);
        return Math.round(r.getBoundingClientRect().left * 100) / 100;
      };
      const out = [];
      for (const th of document.querySelectorAll("#app th.tv-badge")) {
        const key = th.dataset.key;
        const cell = document.querySelector(`#app td[data-key="${key}"] .tv-pill`)
          || [...document.querySelectorAll("#app tbody tr")]
               .map((tr) => tr.children[[...th.parentNode.children].indexOf(th)])
               .map((td) => td && td.querySelector(".tv-pill")).find(Boolean);
        if (!cell) continue;
        out.push({ key, head: textX(th.querySelector(".tv-hn")), pill: textX(cell) });
      }
      return out;
    });
    assert(at.length > 0, "no badge column drew a pill to measure against");
    for (const c of at)
      assert(Math.abs(c.head - c.pill) < 0.5,
        `the "${c.key}" header starts at ${px(c.head)} and its badges at ${px(c.pill)}`);
    return [`badge header against badge text: `
      + at.map((c) => `${c.key} ${px(c.head)}/${px(c.pill)}`).join("  ")];
  } },

// THE PAGE NEVER SCROLLS AND NOTHING DRAWS A BAR SAYING IT MIGHT.  Two surfaces
// did: `.tv-scroll', whose rows are driven by key and wheel, and `#kbd', one
// nowrap line wider than the window at every size — 15px of chrome under a hint
// nobody drags, and 15px the fill column no longer has.  The sibling case above
// asks whether the DOCUMENT scrolls; this asks what CHROME the surfaces draw,
// which is a bar's width of layout either way.
{ name: "no surface on the page draws a scrollbar of its own",
  async run(p, base) {
    const seen = [];
    for (const [w, h] of [[1400, 900], [1024, 768], [800, 900]]) {
      await p.size(w, h);
      await p.goto(`${base}/`);
      await p.until(() => !!document.querySelector("#app table tbody tr"),
                    `the table to mount at ${w}px`);
      const bars = await p.eval(() => {
        const out = [];
        for (const n of document.querySelectorAll("body *")) {
          const cs = getComputedStyle(n);
          // An INLINE box reports a meaningless `clientWidth' of 0; a box that
          // cannot clip cannot draw a bar. Neither is asked.
          if (cs.display === "inline") continue;
          if (cs.overflowX === "visible" && cs.overflowY === "visible") continue;
          const gy = n.offsetWidth - n.clientWidth
            - parseFloat(cs.borderLeftWidth) - parseFloat(cs.borderRightWidth);
          const gx = n.offsetHeight - n.clientHeight
            - parseFloat(cs.borderTopWidth) - parseFloat(cs.borderBottomWidth);
          if (gx > 1 || gy > 1)
            out.push({ el: n.id ? "#" + n.id
                         : n.tagName.toLowerCase() + "." + String(n.className).split(/\s+/)[0],
                       gx: Math.round(gx), gy: Math.round(gy) });
        }
        return out;
      });
      assert(!bars.length,
        `at ${w}x${h} a scrollbar takes layout space on `
        + bars.map((b) => `${b.el} (${b.gy}px wide, ${b.gx}px tall)`).join(", "));
      seen.push(`${w}x${h}: none`);
    }
    return [`surfaces taking a bar's width: ${seen.join("  ")}`];
  } },

// THE MINT FORM STANDS OVER THE PALETTE THAT RAISED IT.  Two readings the text
// suite cannot take: the box is drawn inside the viewport at the height it
// wants, and the palette under it is still painted rather than merely `on'.
{ name: "+ over the state palette draws a form over a palette that stands",
  async run(p, base) {
    await p.goto(`${base}/`);
    await p.until(() => !!document.querySelector("#app table tbody tr"),
                  "the table to mount rows");
    await p.press("t");
    await p.until(() => !!document.querySelector("#plist .pr"),
                  "the state palette to draw its sources");
    await p.press("+");
    await p.until(() => document.getElementById("mint").classList.contains("on"),
                  "the mint form to raise");
    const seen = await p.eval(() => {
      const box = document.getElementById("nbox").getBoundingClientRect();
      const under = document.getElementById("pbox").getBoundingClientRect();
      // DRAWN, rather than merely present: a row the stylesheet hides is a field
      // the reader has not got, and the count alone cannot tell the two apart.
      const rows = [...document.querySelectorAll("#nbox .krow")]
        .filter((r) => r.getBoundingClientRect().height > 0);
      return { top: box.top, bottom: box.bottom, left: box.left, right: box.right,
               vh: window.innerHeight, vw: window.innerWidth,
               rows: rows.length,
               labels: rows.map((r) => r.querySelector(".klab").textContent),
               // The palette is BEHIND it, drawn, rather than merely flagged on.
               palette: under.width > 0 && under.height > 0,
               over: box.top >= 0 };
    });
    assert(seen.rows === 5,
      `the form drew ${seen.rows} rows of the five it asks for`);
    assert(seen.labels.filter((l) => /hue/.test(l)).length === 2,
      `one hue field per theme, and the form has ${JSON.stringify(seen.labels)}`);
    assert(seen.top >= 0 && seen.bottom <= seen.vh + 1,
      `the form is outside the viewport: ${px(seen.top)}..${px(seen.bottom)} `
      + `in ${px(seen.vh)}`);
    assert(seen.left >= 0 && seen.right <= seen.vw + 1,
      `the form is off the side: ${px(seen.left)}..${px(seen.right)} in ${px(seen.vw)}`);
    assert(seen.palette, "the palette under it drew nothing");

    await p.press("ESC");
    await p.until(() => !document.getElementById("mint").classList.contains("on"),
                  "the mint to go on ESC");
    const after = await p.eval(() =>
      document.getElementById("prompt").classList.contains("on"));
    assert(after, "ESC took the palette with it; it was raised over one that stands");
    return [`${seen.rows} rows (${seen.labels.join(", ")}), `
      + `the box ${px(seen.top)}..${px(seen.bottom)} inside ${px(seen.vh)}`];
  } },

// THE MINT, END TO END, AGAINST A REAL TREE.  The node harness serves a FAKE
// config, so nothing there proves the file is written, the store rereads it, or
// the state becomes settable — which is the whole of what `+' has to do.  This
// tree has no `.org-glance/config' at all, so the layer is minted by the write.
{ name: "+ writes a state into a tree that had no config, then sets it",
  async run(p, base) {
    await p.goto(`${base}/`);
    await p.until(() => !!document.querySelector("#app table tbody tr"),
                  "the table to mount rows");
    const row = await p.eval(() => {
      const tr = document.querySelector("#app tr.tv-sel");
      return { id: tr.getAttribute("data-id"), text: tr.textContent.trim() };
    });
    await p.press("t");
    await p.until(() => !!document.querySelector("#plist .pr"),
                  "the state palette to draw its sources");
    await p.press("+");
    await p.until(() => document.getElementById("mint").classList.contains("on"),
                  "the mint form to raise");
    await p.type("HANDED");
    await p.press("RET");
    // THE WHOLE CHAIN: the write, the reseed, `/keywords' answering the new
    // chain, `set-state', and the socket bringing the row back changed.
    await p.until((id) => {
      const tr = document.querySelector(`#app tr[data-id="${id}"]`);
      return !!tr && /HANDED/.test(tr.textContent);
    }, "the row to wear the state that did not exist a moment ago", 20000, row.id);
    const seen = await p.eval((id) => {
      const tr = document.querySelector(`#app tr[data-id="${id}"]`);
      return { text: tr.textContent.trim(),
               mint: document.getElementById("mint").className,
               prompt: document.getElementById("prompt").className };
    }, row.id);
    assert(seen.mint !== "on", "the form is still up over a state it already set");
    assert(seen.prompt !== "on", "the palette stayed up after the state landed");
    return [`row ${row.id} wears HANDED, off a config layer that did not exist `
      + `when the page loaded`];
  } },
];
