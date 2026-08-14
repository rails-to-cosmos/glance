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
    await p.until(() => document.getElementById("dpara").classList.contains("on"),
                  "the edit to open over the list");
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
];
