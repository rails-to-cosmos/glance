// THE CASES, each a COMPUTED reading the text suite provably cannot take, and
// each naming the bug it exists for (docs/proposal-browser-driver.md).
//
// FIGURES ARE RELATIONAL — does B start below A's bottom, is this colour that
// colour — so no case depends on a font's advance width.  An absolute pixel
// figure in a case here is a bug in the case.
//
// A case RETURNS the numbers it measured, which the report prints on the green
// line too: a case whose reading nobody sees is a case nobody can doubt.

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

export default [

// cb6db85.  THE BOX GREW AND STOOD OVER THE DOCUMENT: ten typed lines covered
// the nine under them, with 1781 tests green.  TestServe.hs asserts the STRING
// "min-height:calc(var(--g-doc-rows, 1)"; where the next line ENDS UP is
// unaskable there, the node harness returning zeros from every rect
// (shell-harness.js: "Geometry is beyond this harness").
{ name: "an open edit moves the line under it down, never covers it",
  async run(p, base) {
    await sheet(p, base, "drv-box");
    await p.press("n");                                   // onto the paragraph
    await p.press("RET");                                 // open it
    // `placeEdit' sizes the box off the BLOCK a turn after the raise, so the
    // reading waits for the two to agree rather than for a duration.
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
               line: parseFloat(cs.getPropertyValue("--g-edit-fs"))
                   * parseFloat(cs.getPropertyValue("--g-edit-lh")),
               under: at.nextElementSibling.getBoundingClientRect().top };
    });
    for (let i = 0; i < 10; i += 1) {
      await p.type(`typed line ${i}`);
      if (i < 9) await p.press("S-RET");
    }
    const seen = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const under = at.nextElementSibling;
      const box = document.getElementById("dpara").getBoundingClientRect();
      const a = at.getBoundingClientRect(), b = under.getBoundingClientRect();
      return { box: box.height, ends: box.bottom, block: a.height, blockEnds: a.bottom,
               starts: b.top, under: under.textContent.trim().slice(0, 32) };
    });
    assert(before.line > 0, `the box declares no line box: --g-edit-fs * --g-edit-lh = ${before.line}`);
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

// d7ba44b, Style.hs's `.d-draft'.  A paragraph drawn before it is written holds
// nothing and `:empty' cannot find it — Elm emits an empty text node — so the
// floor is DECLARED.  Nothing measured the declaration, and a collapsed row is
// a cursor standing on nothing.
//
// TWO READINGS, because `+' leaves the draft AT POINT and `.de.dat' carries a
// floor of its own while an edit is open: the real row answers the user-visible
// question, and a PROBE wearing the draft's classes without `.dat' attributes
// the height to `.d-draft' itself.
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

// 14e13d9.  THE PANE DREW ITS FLAG IN `--g-warn' at a strength of its own, so
// `d' over the table and `d' over the pane — ONE gesture over ONE queue —
// looked like two states.  `paletteSweep' compares the two NAMESPACES in the
// served TEXT; it cannot mount the renderer, whose palette is injected into
// <head> AT MOUNT TIME at zero specificity, so what a flagged row PAINTS is
// unaskable there (the harness's TableView is a stub).
//
// EVERY COLOUR IS RESOLVED BY THE ENGINE, both sides: a hex token and a
// computed shadow string are the same colour only once something has painted
// them.
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
      // The renderer injects its palette AT MOUNT TIME at zero specificity, so
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
    // Both strings came out of the same engine, so the red is compared as the
    // engine spells it rather than re-parsed.
    assert(pane.edge.includes(table.flag) && /inset/.test(pane.edge),
      `the pane's flag edge is "${pane.edge}", the table's red is ${table.flag}`);
    assert(pane.ground !== "rgba(0, 0, 0, 0)",
      `the pane's flagged row has no ground at all: ${pane.ground}`);
    return [`--g-bad and --tv-flag both paint ${table.flag}`,
            `the table's flagged ground is ${table.ground} against ${table.plain}`,
            `the pane's edge is "${pane.edge}"`];
  } },

// CLAUDE.md: "The page never scrolls: body is 100vh, overflow:hidden ... the
// key line is flex:none and scrolls sideways" — the KEY LINE is the one
// sideways scroller and is exempt; the reading is the DOCUMENT's scroller.
// Every surface opens by its OWN URL (SURFACES / bootPage), so this sweep keeps
// no copy of the keymap.
{ name: "the page never scrolls, sideways or down, at any width or surface",
  async run(p, base) {
    // A SURFACE THAT NEVER ROSE would have this sweep measure the table three
    // times over and report ok, so each one is waited for BY ITS OWN CONTAINER.
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

// A POPUP CLAMPS AND SCROLLS INSIDE, as a CHAIN: `--g-pop-max' is
// `min(90vh, calc(100vh - 2 * var(--g-pop-top)))', the foot margin derived from
// the HEAD's rather than spelled as a second figure (Style.hs).  Nothing
// measures the chain, and a box taller than the viewport is a reader who cannot
// reach its foot.
//
// THIS CASE FOUND A DEFECT ON ITS FIRST RUN and it is the reason the driver
// exists: `.pop-sheet' set `height:var(--g-pop-max)' with no
// `box-sizing:border-box' — the reset spelled it for `body' and `#app,#log'
// alone — so `#sheet' drew its own 14px padding and 1px border OUTSIDE the cap
// and stood 30px taller than it was told to.  `5vh + 90vh + 30px > 100vh'
// wherever the viewport is under 600px tall, which put the sheet's foot off
// screen on a split window and on any phone in landscape.  Fixed by the
// `.pop-band,.pop-sheet' pair in `Glance.Web.Page.Style'; 1838 text-level tests
// were green over it, and no reading this suite can make would have caught it.
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

// 80c3732: "THE STATE BADGE LOST ITS COLOUR ... the Elm view invented a CSS
// variable name" — invisible to 1737 tests.  The hue is handed over WITH the
// cell (`badgeColor(value, key)'), worn as an inline `color' on
// `span.dc.dc-state'.  The table's own pill is the renderer's, drawn by
// something no text case mounts.  ONE KEYWORD, TWO SURFACES, ONE PAINTED
// COLOUR.
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

// AND CONTENT SITS UNDER THE TITLE TEXT: a paragraph starts at the head's own
// title column rather than at its stars, the width DERIVED from `dstars' and
// written onto `#mdoc' as a NUMBER (CLAUDE.md, UI).  PADDING rather than a
// margin — a margin would take the selection wash off the left of the line —
// which is a rule about where two boxes' LEFT EDGES sit, and nothing measured
// either.
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

// EVERY SELECTION IN THE PANE IS A GROUND, never a line (CLAUDE.md, UI).
// `groundSweep' cuts the four rules out of the SERVED TEXT and greps them for
// `border'/`outline'/`text-decoration'/`box-shadow'; what the cursor row
// actually PAINTS — a ground that differs from the row above it — is a
// reading only an engine takes.
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
];
