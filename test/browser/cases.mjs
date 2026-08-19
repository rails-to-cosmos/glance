// THE CASES, each a COMPUTED reading the text suite cannot take, and each
// naming the bug it exists for.  FIGURES ARE RELATIONAL; a case RETURNS them.

const assert = (ok, why) => { if (!ok) throw new Error(why); };
const px = (n) => `${Math.round(n * 10) / 10}px`;

/** Boot the table page, which every table-surface case starts from. */
async function tableUp(p, base) {
  await p.goto(`${base}/`);
  await p.until(() => !!document.querySelector("#app table tbody tr"),
                "the table to mount rows");
}

/** Raise the mint form over the state palette, the table already up. */
async function mintForm(p) {
  await p.press("t");
  await p.until(() => !!document.querySelector("#plist .pr"),
                "the state palette to draw its sources");
  await p.press("+");
  await p.until(() => document.getElementById("mint").classList.contains("on"),
                "the mint form to raise");
}

/** Open the sheet over ROW by URL, which is the page's own contract. */
async function sheet(p, base, row) {
  await p.goto(`${base}/?page=sheet&row=${row}`);
  await p.until(() => !!document.querySelector("#modal.on"),
                `the sheet to open over ${row}`);
  await p.until(() => document.querySelectorAll("#mdoc .de").length > 1,
                `${row}'s body to draw more than a headline`);
}

/** WALK THE SHELF TO THE FIRST ROW THE PREDICATE LIKES.  A FIXED key count
 * lands elsewhere the moment a row is added above — the header rows are. */
async function walkTo(p, cls, what) {
  for (let i = 0; i < 12; i += 1) {
    // THE MIRROR MUST AGREE WITH THE DRAW before the walk trusts a reading: the
    // DOM paints on rAF and the port lands a macrotask apart, and a key pressed
    // in that gap acts on the row the reader just left.
    const seen = await p.eval((c) => {
      const at = document.querySelector("#mdoc .de.dat");
      const synced = !!at && at.dataset.id === docAtNow();
      return { hit: synced && at.classList.contains(c),
               synced,
               text: at ? at.textContent.slice(0, 30) : "" };
    }, cls);
    if (seen.hit) return;
    if (!seen.synced) {
      await settled(p, `the mirror to settle on the way to ${what}`);
      continue;
    }
    await p.press("n");
    await p.until((was) => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && at.dataset.id === docAtNow()
        && at.textContent.slice(0, 30) !== was;
    }, `the step toward ${what}`, undefined, seen.text);
  }
  assert(false, `\`n' never reached ${what}`);
}

/** The draw and the mirror agreeing on the row under point — REQUIRED before a
 * key that acts on the cursor, whenever the wait watched the DOM alone. */
const settled = (p, why) =>
  p.until(() => {
    const at = document.querySelector("#mdoc .de.dat");
    return !!at && at.dataset.id === docAtNow();
  }, why || "the mirror to agree with the draw");

/** Down to an item that has rows drawn inside it. */
async function intoNestedItem(p, base, row) {
  await sheet(p, base, row);
  await walkTo(p, "d-list", "the list itself");
  await p.press("f");
  await p.until(() => {
    const at = document.querySelector("#mdoc .de.dat");
    return !!at && at.classList.contains("d-item");
  }, "`f' to descend into the list");
  const on = () => p.eval(() => {
    const at = document.querySelector("#mdoc .de.dat");
    return !!at && at.dataset.id === docAtNow()
      && at.classList.contains("d-item") && !!at.querySelector(".de");
  });
  for (let i = 0; i < 8 && !(await on()); i++) await p.press("n");
  assert(await on(),
    "`f' then `n' never reached an item with rows drawn inside it");
}

/** The sheet over ROW with its first paragraph open for editing. */
async function paraOpen(p, base, row) {
  await sheet(p, base, row);
  await walkTo(p, "d-para", "the first paragraph");
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
// A PAGE FUNCTION IS SERIALIZED WHOLE, so it may call nothing of this module's.
/** THE EDITOR IS SUMMONED ON DEMAND: is its bar drawn at all? */
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
    await walkTo(p, "d-para", "the first paragraph");
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
               // THE LINE BOX IS A LENGTH, a whole number of pixels, so a hairline
               // and a hinted glyph land on one device row.
               line: parseFloat(cs.getPropertyValue("--g-doc-lh")),
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
// cannot find it; `.de.dat' has a floor of its own, so a PROBE isolates it.
{ name: "a paragraph drawn before it is written still owns a line",
  async run(p, base) {
    await sheet(p, base, "drv-box");
    await walkTo(p, "d-para", "the first paragraph");
    await p.press("+");
    await p.until(() => !!document.querySelector("#mdoc .d-draft"),
                  "the drawn paragraph to appear");
    const seen = await p.eval(() => {
      const cs = getComputedStyle(document.getElementById("mdoc"));
      const line = parseFloat(cs.getPropertyValue("--g-doc-lh"));
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

// 14e13d9.  ONE red on both surfaces; `paletteSweep' only ever compares served TEXT.
{ name: "a flag paints one red on both surfaces, and draws its inset edge",
  async run(p, base) {
    await tableUp(p, base);
    await p.press("d");                                   // dired's flag
    await p.until(() => !!document.querySelector("#app tr.tv-flagged"),
                  "the table row to wear its flag");
    const table = await p.eval(() => {
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

    // A ROW THAT HAS A BRANCH, so "the flag takes the branch" is asked of something.
    await intoNestedItem(p, base, "drv-wide");
    await p.press("d");
    await p.until(() => {
      const fl = document.querySelector("#mdoc .de.dfl");
      return !!fl && !!fl.querySelector(".de");
    }, "the pane's row to wear its flag, with rows inside it");
    // THE TABLE WASHES ITS ROW AND THE DOCUMENT MARKS ITS LINE: the pane's ground is
    // the CURSOR's, so a flagged row wears one only while point stands on it.
    const pane = await p.eval(() => {
      const fl = document.querySelector("#mdoc .de.dfl");
      // THE INK IS THE ROW'S: an elbow spends it on borders, a run on background.
      // A FLAG TAKES THE BRANCH, since a delete takes the subtree.
      const under = fl.querySelector(".de");
      return { under: under ? ink(under) : null,
               thin: ink(fl),
               wide: getComputedStyle(fl, "::before").width,
               at: fl.classList.contains("dat"),
               sel: rgb(getComputedStyle(document.documentElement)
                 .getPropertyValue("--g-sel").trim()),
               ground: getComputedStyle(fl).backgroundColor };
    });
    // Both strings came out of the same engine, so the red is compared as spelled.
    assert(pane.thin === table.flag,
      `the pane's flag connector paints ${pane.thin}, the table's red is ${table.flag}`);
    assert(pane.under === null || pane.under === table.flag,
      `a row under the flagged one paints ${pane.under}, so the branch is not marked`);
    assert(pane.ground === (pane.at ? pane.sel : "rgba(0, 0, 0, 0)"),
      `the flagged row ${pane.at ? "under point" : "away from point"} grounds `
      + `${pane.ground}, so the flag has a wash of its own`);
    return [`--g-bad and --tv-flag both paint ${table.flag}`,
            `the table's flagged ground is ${table.ground} against ${table.plain}`,
            `the pane's flag is a ${pane.wide} connector in ${pane.thin}`];
  } },

// The KEY LINE is the one sideways scroller and is exempt.
{ name: "the page never scrolls, sideways or down, at any width or surface",
  async run(p, base) {
    // A SURFACE THAT NEVER ROSE would measure the table again and report ok.
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
// margin from the HEAD's (Style.hs).
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

// 80c3732.  ONE KEYWORD, TWO SURFACES, ONE PAINTED COLOUR.
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

// CONTENT SITS UNDER THE TITLE TEXT (AGENTS.hs).  PADDING, since a margin takes
// the selection wash off the left of the line.
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

// POINT IS A MARK BESIDE THE LINE (AGENTS.hs).  `groundSweep' greps the served
// TEXT; what the row PAINTS needs an engine.
{ name: "the cursor in the pane is a ground, and TAB over prose folds nothing",
  async run(p, base) {
    await sheet(p, base, "drv-box");
    await walkTo(p, "d-para", "the first paragraph");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && at.classList.contains("d-para");
    }, "the cursor to land on the entry's first paragraph");
    const seen = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const off = [...document.querySelectorAll("#mdoc .de")].find((n) => n !== at);
      const cs = getComputedStyle(at);
      const root = getComputedStyle(document.documentElement);
      const point = root.getPropertyValue("--g-point").trim();
      return { sel: rgb(root.getPropertyValue("--g-sel").trim()),
               mark: ink(at),
               fg: rgb(root.getPropertyValue("--g-fg").trim()),
               ink: rgb(point),
               offMark: ink(off),
               ground: cs.backgroundColor,
               deco: cs.textDecorationLine, outline: cs.outlineStyle,
               border: cs.borderTopStyle };
    });
    // THE GROUND SAYS WHERE POINT IS; its connector wears the page's own ink.
    assert(seen.mark === seen.fg,
      `the cursor's mark paints ${seen.mark}, not the page's ink ${seen.fg}`);
    // A PARAGRAPH CARRIES NOTHING, so it wears the ground the table's cursor wears.
    // What made a ground wrong is the subtree drawn inside an ITEM, and the case
    // named "the cursor on a list item lights itself" owns that half.
    assert(seen.ground === seen.sel,
      `the cursor row paints ${seen.ground} rather than the selection ${seen.sel}`);
    assert(seen.deco === "none" && seen.outline === "none" && seen.border === "none",
      `the cursor row is drawn with a line: decoration ${seen.deco}, `
      + `outline ${seen.outline}, border ${seen.border}`);
    // TAB FOLDS NOW; on a paragraph it folds nothing and the ground stands.
    await p.press("TAB");
    await p.until(() => /nothing folds here/.test(document.getElementById("echo").textContent),
                  "TAB to answer over a paragraph");
    const still = await p.eval(() =>
      getComputedStyle(document.querySelector("#mdoc .de.dat")).backgroundColor);
    assert(still === seen.sel,
      `TAB moved the ground: ${still} against ${seen.sel}`);
    return [`the cursor grounds ${seen.ground} with its mark in ${seen.mark}`];
  } },

// A LEAF IS ONE LINE OF THE FIELD THAT COVERS IT: `.de' padding spent twice drifts.
{ name: "a composite's drawn lines sit on the same grid as the field over it",
  async run(p, base) {
    await sheet(p, base, "drv-plan");
    await walkTo(p, "d-list", "the whole-list composite");
    await p.press("RET");
    // THE CLASS FLIPS BEFORE THE PANE HAS DRAWN, so the wait is on the MEASUREMENT
    // (docs/bugs/2026-08-17-the-composite-case-measures-an-empty-pane).
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
    // EVERY ASSERTION BELOW RIDES THIS LIST; an empty one reports green.
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

// AN EDIT BOX IS THE BLOCK IT COVERS, and A COMMIT CLOSES WHAT THE TYPING OPENED.
// `Scan.closers' is unit tested over lines; a TYPED line reaches it only here.
{ name: "a committed block opener arrives with its closer",
  async run(p, base) {
    await sheet(p, base, "drv-box");
    await walkTo(p, "d-para", "the first paragraph");
    await p.press("+");                                   // a new one under it
    await p.until(() => !!document.querySelector("#dpara.on"),
                  "the draft edit to open");
    await p.type("#+begin_src elisp");
    await p.press("RET");                                 // commit
    // THE WATCH DELIVERS, so the reading waits for the row itself.
    await p.until(() => [...document.querySelectorAll("#mdoc .de")]
                          .some((e) => e.textContent.includes("#+end_src")),
                  "the closer to arrive over the watch");
    // ONE STOP: balanced, the two lines are one BLOCK, so the order is read inside it.
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
    assert(!seen.text.includes("#+end_src elisp"),
      `the closer carried the opener's arguments: ${JSON.stringify(seen.text)}`);
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
    // Onto the first PARAGRAPH first: the header rows sit above it, and RET on
    // the drawer opens no box.
    await walkTo(p, "d-para", "the first paragraph");
    const out = [];
    // A paragraph and the whole-list composite carry the title indent; leaves do not.
    const walk = ["", "n", "f", "n", "n", "n"];
    for (let i = 0; i < walk.length; i += 1) {
      if (walk[i]) await p.press(walk[i]);
      await p.press("RET");
      await p.until(() => document.getElementById("dpara").classList.contains("on"),
                    "the edit to open");
      // `placeEdit' sizes the box a turn after the raise; unsized it is one line tall.
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
      // A BAR TAKING LAYOUT WIDTH WRAPS THE FIELD NARROWER; Chromium overlays, so 0.
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

// A COLUMN SIZED BY ITS OWN BADGE CAME UP A FIFTH OF A PIXEL SHORT: a pill is an
// inline-block `text-overflow' cannot cut, so it drew `[#A]…', the whole badge.
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
        // FRACTIONAL: `clientWidth' rounds the fifth of a pixel away.
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

// `@' IN THE SHEET LINKS A HEADLINE INTO THE PROSE; the picker is a table-view mount.
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
               // it hangs at the caret, so it sits near the pane's own top
               placed: box.top > pane.top - 200 && box.width > 100,
               veil: getComputedStyle(document.getElementById("refer")).backgroundColor };
    });
    assert(shown.rows > 0, "the picker mounted no rows");
    assert(shown.placed, "the picker did not hang near the pane");
    assert(/rgba\(0, 0, 0, 0\)|transparent/.test(shown.veil),
      `the picker drew a veil (${shown.veil}); a completion draws none`);

    await p.press("RET");
    await pickerGone(p, "the picker to close on RET");
    const wrote = await p.eval(() => document.getElementById("dtext").value);
    assert(/\[\[glance:[^\]]+\]\[[^\]]+\]\]/.test(wrote),
      `RET wrote no org link into the box: ${JSON.stringify(wrote)}`);
    assert(!/@\[\[glance:/.test(wrote),
      `the link was written BESIDE the @ rather than over it: ${JSON.stringify(wrote)}`);

    assert(await p.eval(() => document.getElementById("dpara").classList.contains("on")),
      "taking a row closed the paragraph the link was written into");

    // THE `@' IS WRITTEN THE MOMENT IT IS TYPED, and dismissing the picker leaves it.
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
    // AND `@' IS A CHARACTER FIRST: mid-word it is text.
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

// A SELECTED REGION BECOMES THE LINK AND ITS OWN WORDS ARE WHAT IT READS AS.
// Seeding the filter with them narrows the store by an accident of phrasing.
{ name: "@ over a selected region links it, and the region is no filter",
  async run(p, base) {
    await paraOpen(p, base, "drv-box");
    await p.type(" the weekly note ");
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

// `@' WRITES INTO WHATEVER BOX IS OPEN; over a title edit that is the TITLE.
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

// ESC IN THE PICKER'S FILTER IS ONE STEP: the filter goes AND the cursor lands on a row.
{ name: "ESC in the picker's filter drops the edit and stands on a row",
  async run(p, base) {
    await pickerOver(p, base, "drv-box");
    assert(!(await p.eval(barDrawn)),
      "the filter editor sits on the picker before anyone asked for it");
    await p.press("/");
    await boxFocused(p);
    assert(await p.eval(barDrawn), "/ summoned no filter editor");
    // AND IT COMES ON THE CHIPS' OWN LINE.
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

// DEL'S RUNGS IN THE PICKER: an EMPTIED summoned editor is the rung under the
// typed text, and the picker's own listener sees DEL once the box has gone.
{ name: "DEL on the picker's empty filter hides it, and the next DEL takes a chip",
  async run(p, base) {
    await pickerOver(p, base, "drv-box");
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
    await p.press("DEL");
    await boxAway(p, "the summoned editor to go on the first DEL");
    const once = await p.eval(state);
    assert(once.chips === seeded.chips,
      `the first DEL took a chip as well as the box: ${seeded.chips} -> ${once.chips}`);
    assert(!once.typing, "the first DEL left the keyboard in the box it emptied");
    assert(once.up, "the first DEL dismissed the whole picker");

    await p.press("DEL");
    await p.until((n) => document.querySelectorAll("#rmount .tv-chip[data-i]").length < n,
                  "the second DEL to take a chip", 8000, seeded.chips);
    const twice = await p.eval(state);
    assert(twice.up, "the second DEL dismissed the picker instead of taking a chip");
    return [`chips ${seeded.chips} → ${once.chips} (box away) → ${twice.chips}`];
  } },

// ONE PRESS, ONE PART.  A held DEL hands the keyboard back on the FIRST press, so
// the renderer's `e.repeat' guard cannot help: it is no longer the one pressed.
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

// THE KIND IS THE EDGE'S, and `K' declares it: `k' is the previous row in the vim
// dialect, so the kind takes the shift.  The SLUG comes back from the server.
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

// THE FILTER IS THE SECOND ROUTE TO THE SAME KIND.  `kind:' is the EDGE's, so it
// comes OUT of the row query and never narrows the rows it is written from.
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

// THE PLATFORM PAINTS THE `<select>', and only `color-scheme' tells it which way.
// Chromium cannot reproduce GTK's paint, so the DECLARATION is what is asked.
{ name: "every dropdown declares the scheme its platform paints it in",
  async run(p, base) {
    await tableUp(p, base);
    const SELECTS = ["themesel", "clayer", "nspace", "ngroup"];
    const read = (theme, ids) => {
      const root = document.documentElement;
      const was = root.dataset.theme;
      if (theme) root.dataset.theme = theme; else delete root.dataset.theme;
      const scheme = getComputedStyle(root).colorScheme;
      // A MISSING BOX IS REPORTED, since `getComputedStyle(null)' raises.
      const boxes = ids.map((id) => {
        const el = document.getElementById(id);
        if (!el) return { id, missing: true };
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

    const missed = dark.boxes.filter((b) => b.missing).map((b) => b.id);
    assert(!missed.length, `the page lost a dropdown: ${missed.join(", ")}`);

    // AND WE DO NOT DO IT TO OURSELVES: ink on its own ground is unreadable.
    for (const seen of [dark, light])
      for (const b of seen.boxes)
        assert(b.fg !== b.bg,
          `#${b.id} draws ${b.fg} on ${b.bg} — one colour twice`);
    return [`${SELECTS.length} dropdowns · dark declares ${JSON.stringify(dark.scheme)}, `
      + `light ${JSON.stringify(light.scheme)}`];
  } },

// A BADGE COLUMN'S HEADER SITS OVER ITS BADGES' FIRST LETTER: a pill sets its text
// in from the cell edge, so a header aligned to the CELL sits a padding's width left.
{ name: "a badge column's header lines up with its badges' first letter",
  async run(p, base) {
    await p.size(1400, 900);
    await p.goto(`${base}/`);
    await p.until(() => !!document.querySelector("#app td .tv-pill"),
                  "a badge to draw");
    const at = await p.eval(() => {
      // A Range over the text node is where the letters start.
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

// NOTHING DRAWS A BAR SAYING THE PAGE MIGHT SCROLL.  The sibling case asks whether
// the DOCUMENT scrolls; this asks what CHROME the surfaces draw.
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
          // Neither is asked: an INLINE `clientWidth' is 0, and no clip means no bar.
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

// THE MINT FORM STANDS OVER THE PALETTE THAT RAISED IT.
{ name: "+ over the state palette draws a form over a palette that stands",
  async run(p, base) {
    await tableUp(p, base);
    await mintForm(p);
    const seen = await p.eval(() => {
      const box = document.getElementById("nbox").getBoundingClientRect();
      const under = document.getElementById("pbox").getBoundingClientRect();
      // DRAWN, since a hidden row is a field the reader has not got.
      const rows = [...document.querySelectorAll("#nbox .krow")]
        .filter((r) => r.getBoundingClientRect().height > 0);
      return { top: box.top, bottom: box.bottom, left: box.left, right: box.right,
               vh: window.innerHeight, vw: window.innerWidth,
               rows: rows.length,
               labels: rows.map((r) => r.querySelector(".klab").textContent),
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

// THE MINT, END TO END, AGAINST A REAL TREE: the node harness serves a FAKE config.
// This tree has no `.org-glance/config', so the layer is minted by the write.
{ name: "+ writes a state into a tree that had no config, then sets it",
  async run(p, base) {
    await tableUp(p, base);
    const row = await p.eval(() => {
      const tr = document.querySelector("#app tr.tv-sel");
      return { id: tr.getAttribute("data-id"), text: tr.textContent.trim() };
    });
    await mintForm(p);
    await p.type("HANDED");
    await p.press("RET");
    // THE WHOLE CHAIN: the write, the reseed, `/keywords', `set-state', and the socket.
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

// A NESTED LIST ITEM IS DRAWN INSIDE ITS PARENT, so its element stands taller than
// its own line.  `f' goes finer: the list itself is ONE stop at the coarse grain.
{ name: "the cursor on a list item lights itself, its subtree and the way back",
  async run(p, base) {
    await intoNestedItem(p, base, "drv-wide");
    const seen = await p.eval(() => {
      const root = getComputedStyle(document.documentElement);
      const at = document.querySelector("#mdoc .de.dat");
      const kid = at.querySelector(":scope > .de");
      // NEITHER POINT, NOR ITS SUBTREE, NOR AN OWNER, NOR A SIBLING: a sibling is the
      // choice the reader is standing in and stays readable, so it is not "outside".
      // OUTSIDE THE BRANCH THE READER IS CHOOSING IN.  With point at the list's own
      // level every item is point, a sibling, or inside one, so what is left outside
      // is the prose around the list.
      const other = [...document.querySelectorAll("#mdoc .de.d-para.lvl-top")]
        .find((n) => !n.classList.contains("d-head"));
      return { point: rgb(root.getPropertyValue("--g-point").trim()),
               fg: rgb(root.getPropertyValue("--g-fg").trim()),
               off: rgb(root.getPropertyValue("--g-point-off").trim()),
               sel: rgb(root.getPropertyValue("--g-sel").trim()),
               atInk: ink(at),
               kidInk: ink(kid),
               otherInk: other
                 ? ink(other) : null,
               ground: getComputedStyle(at).backgroundColor,
               tall: Math.round(parseFloat(getComputedStyle(at, "::before").height)),
               rowH: Math.round(at.getBoundingClientRect().height) };
    });
    // THE GROUND SAYS WHERE POINT IS: its connector wears the page's own ink.
    assert(seen.atInk === seen.fg,
      `the item's connector paints ${seen.atInk}, not the page's ink ${seen.fg}`);
    // WHAT POINT CARRIES TAKES THE PAGE'S OWN INK; everything off the path drops to
    // the ink nobody is looking at.
    assert(seen.kidInk === seen.fg,
      `a row under point paints ${seen.kidInk}, not the page's ink ${seen.fg}`);
    assert(seen.otherInk === seen.off,
      `an item outside point's subtree paints ${seen.otherInk}, not ${seen.off}`);
    // AND THE ROW STANDS ON THE TABLE'S OWN GROUND, its connector outside it.
    assert(seen.ground === seen.sel,
      `the item grounds ${seen.ground}, not the table's ${seen.sel}`);
    // THE SPINE BARS THE ROW'S WHOLE EXTENT: own line and subtree together,
    // the run's unbroken column.
    assert(Math.abs(seen.tall - seen.rowH) < 1,
      `the spine is ${seen.tall}px against the row's ${seen.rowH}px extent`);
    // THE LIST GROUNDS THE ROWS IT OPENS: a composite has no connector of its own,
    // and a connector standing on that ground takes the page's ink to read over it.
    await p.press("b");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && at.classList.contains("d-comp");
    }, "the cursor to go back out to the list itself");
    const whole = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const root = at.querySelector(":scope > .de");
      const deep = at.querySelector(":scope > .de > .de");
      return { rootInk: ink(root),
               ground: getComputedStyle(at).backgroundColor,
               deepInk: deep
                 ? ink(deep) : null };
    });
    assert(whole.ground === seen.sel,
      `the list grounds ${whole.ground}, not the table's ${seen.sel}`);
    assert(whole.rootInk === seen.fg,
      `a root on the list's ground paints ${whole.rootInk}, not the page's ${seen.fg}`);
    assert(whole.deepInk !== seen.point,
      `a row two deep paints ${whole.deepInk} as well, so the light ran the tree`);
    return [`point ${seen.atInk}, what it carries ${seen.kidInk}, elsewhere `
      + `${seen.otherInk}; the spine bars all ${seen.tall}px of the row; `
      + `the list grounds its rows ${whole.ground} and their rails read ${whole.rootInk}`];
  } },

// THE BOX IS THE LINE IT WRITES: the ROW is as tall as its subtree, and the edit
// covers the item's own line.  A composite has no own line and keeps the whole box.
{ name: "the open edit over a nested item covers its own line alone",
  async run(p, base) {
    await sheet(p, base, "drv-wide");
    await walkTo(p, "d-list", "the list");
    await p.press("f");
    // BOTH a nested row AND an own line: a composite has kids too.
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      if (!at) return false;
      const kid = at.querySelector(".de");
      return !!kid && !!at.children[0] && at.children[0] !== kid;
    }, "the cursor on an item with rows drawn inside it");
    const shape = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const px = (n) => Math.round(n.getBoundingClientRect().height);
      return { row: px(at), own: px(at.children[0]) };
    });
    assert(shape.row >= shape.own * 2,
      `the row is ${shape.row}px against its own line's ${shape.own}px, so a box `
      + `covering the subtree would look the same as one covering the line`);

    await p.press("RET");
    await p.until(() => document.getElementById("dpara").classList.contains("on"),
                  "the edit to open");
    // `placeEdit' sets the top and the height together a frame after the raise.
    await p.until(() => {
      const b = document.getElementById("dpara").getBoundingClientRect();
      const at = document.querySelector("#mdoc .de.dat");
      const own = at && at.children[0];
      return !!own && Math.abs(b.top - own.getBoundingClientRect().top) <= 2;
    }, "the box to be placed over the line it edits");
    const box = await p.eval(() => {
      const b = document.getElementById("dpara").getBoundingClientRect();
      const at = document.querySelector("#mdoc .de.dat");
      const own = at.children[0].getBoundingClientRect();
      return { h: Math.round(b.height), top: Math.round(b.top),
               ownTop: Math.round(own.top), ownH: Math.round(own.height),
               text: document.getElementById("dtext").value };
    });
    assert(Math.abs(box.h - box.ownH) <= 2,
      `the box stands ${box.h}px over a ${box.ownH}px line — it covers the subtree`);
    assert(Math.abs(box.top - box.ownTop) <= 2,
      `the box opens at ${box.top} against the line's ${box.ownTop}`);
    assert(!/\n/.test(box.text),
      `the box holds more than the item's own line: ${JSON.stringify(box.text)}`);
    return [`row ${shape.row}px, own line ${box.ownH}px, box ${box.h}px holding `
      + `${JSON.stringify(box.text.slice(0, 34))}`];
  } },
{ name: "the cursor is a ground over its own line, and the marker reads over it",
  async run(p, base) {
    await sheet(p, base, "drv-wide");
    // Onto the list, then into it: the item under point has one drawn inside it.
    await walkTo(p, "d-list", "the list");
    await p.press("f");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && !!at.querySelector(":scope > .dp > .dm") && !!at.querySelector(".de");
    }, "the cursor to land on an item that has one drawn inside it");
    const seen = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const root = getComputedStyle(document.documentElement);
      // THE INK IS READ OFF THE GLYPH, the bullet's own span where org wrote a
      // steppable bullet, since that is what the reader sees.
      const glyph = (n) => n && (n.querySelector(".dbul") || n);
      const own = at.querySelector(":scope > .dp > .dm");
      // `:scope': a descendant combinator may match through an ancestor outside `at'.
      const kidRow = at.querySelector(":scope > .de");
      const kid = glyph(kidRow.querySelector(":scope > .dp > .dm"));
      const plainRow = [...document.querySelectorAll("#mdoc .de:not(.dat)")]
        .find((n) => !at.contains(n) && n.querySelector(":scope > .dp > .dm"));
      return { sel: rgb(root.getPropertyValue("--g-sel").trim()),
               bg: rgb(root.getPropertyValue("--g-bg").trim()),
               fg: rgb(root.getPropertyValue("--g-fg").trim()),
               point: rgb(root.getPropertyValue("--g-point").trim()),
               // THE GROUND THE TABLE'S CURSOR WEARS, and the tree in its own column.
               ground: getComputedStyle(at).backgroundColor,
               rail: ink(at),
               kidGround: getComputedStyle(kidRow).backgroundColor,
               plainGround: plainRow ? getComputedStyle(plainRow).backgroundColor : null,
               ink: getComputedStyle(glyph(own)).color, text: own.textContent,
               weight: getComputedStyle(glyph(own)).fontWeight,
               plainWeight: getComputedStyle(own.parentElement).fontWeight,
               nested: kid ? getComputedStyle(kid).color : null };
    });
    assert(seen.ground === seen.sel,
      `the cursor row grounds ${seen.ground}, not the table's ${seen.sel}`);
    // A NESTED ROW IS DRAWN INSIDE POINT, so the ground would run the whole subtree.
    assert(seen.kidGround === seen.bg,
      `a row drawn inside point grounds ${seen.kidGround}, so the ground runs the subtree`);
    assert(seen.plainGround !== seen.sel,
      `an ordinary row grounds ${seen.plainGround} as well`);
    // THE TREE WEARS THE PAGE'S INK; the ground alone says where point is.
    assert(seen.rail === seen.fg,
      `the cursor's connector paints ${seen.rail}, not the page's ink ${seen.fg}`);
    // AND WHAT STANDS ON THE GROUND READS OVER IT: point's hue is the ground's in the
    // light theme, so a marker painted in it went missing, ordinals and all.
    assert(seen.ink === seen.fg,
      `the marker on the ground paints ${seen.ink}, not the page's ${seen.fg}`);
    assert(seen.nested === seen.fg,
      `a marker nested under point paints ${seen.nested}`);
    // THE CHECKBOX IS PART OF THE MARKER: `- [X]' is one thing the reader points at.
    assert(/^\s*([-+*]|\d+[.)])\s+(\[[ xX-]\]\s+)?$/.test(seen.text),
      `the span read is "${seen.text}", which is not an org list marker`);
    // AND THE HEADLINE DRAWS NO MARK: its stars sit in the connector's own column.
    await p.press("b"); await p.press("b");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && at.classList.contains("d-head");
    }, "the cursor to climb back to the headline");
    const head = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return { stars: getComputedStyle(at.querySelector(".ds")).color,
               ground: getComputedStyle(at).backgroundColor,
               mark: getComputedStyle(at, "::before").display };
    });
    assert(head.ground === seen.sel,
      `the headline under point grounds ${head.ground}, not the table's ${seen.sel}`);
    assert(head.stars === seen.fg,
      `the headline's stars paint ${head.stars}, not the page's ${seen.fg}`);
    assert(head.mark === "none",
      `the headline draws a connector as well as standing on the ground`);
    // COLOUR ALONE: the marker keeps the face the line is set in.
    assert(seen.weight === seen.plainWeight,
      `the marker is weight ${seen.weight} against the line's ${seen.plainWeight}`);
    return [`the row grounds ${seen.ground} with its rail in ${seen.rail}; `
      + `"${seen.text.trim()}" reads ${seen.ink} at weight ${seen.weight}; `
      + `a row inside it grounds ${seen.kidGround}`];
  } },
{ name: "the strip names the way back, and agrees with the connectors",
  async run(p, base) {
    await intoNestedItem(p, base, "drv-wide");
    await p.press("f");                                  // one deeper, so the chain is long
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && document.querySelectorAll("#mdoc .dpath .dcr").length >= 3;
    }, "the strip to name three steps or more");
    const seen = await p.eval(() => {
      const root = getComputedStyle(document.documentElement);
      const crumbs = [...document.querySelectorAll("#mdoc .dpath .dcr")];
      const at = document.querySelector("#mdoc .de.dat");
      const own = at.querySelector(":scope > .dp");
      return { words: crumbs.map((c) => c.textContent),
               last: getComputedStyle(crumbs[crumbs.length - 1]).color,
               point: rgb(root.getPropertyValue("--g-point").trim()),
               said: own ? own.textContent.trim() : "",
               sticky: getComputedStyle(document.querySelector("#mdoc .dpath")).position };
    });
    // THE LAST CRUMB IS POINT, in the ink point's own connector takes.
    assert(seen.last === seen.point,
      `the last crumb paints ${seen.last}, not the point ink ${seen.point}`);
    // AND IT NAMES THE LINE IT IS ON: the marker org wrote is not part of the name.
    const tail = seen.words[seen.words.length - 1].replace(/…$/, "");
    assert(seen.said.replace(/^\s*([-+*]|\d+[.)])\s+(\[[ xX-]\]\s+)?/, "").startsWith(tail),
      `the strip says "${tail}" where the line reads "${seen.said}"`);
    // EVERYTHING IS UNDER THE HEADLINE, so the way back starts there and the list
    // it is inside comes next.
    assert(seen.words[0] === "headline",
      `the strip opens on "${seen.words[0]}" rather than the entry's own line`);
    assert(seen.words[1] === "list",
      `the strip's second step is "${seen.words[1]}" rather than the list`);
    assert(seen.sticky === "sticky",
      `the strip is ${seen.sticky}, so it leaves the top when the rows scroll`);
    return [`${seen.words.join(" → ")} — the last in ${seen.last}`];
  } },
{ name: "the pane dims every branch but the one the reader is in",
  async run(p, base) {
    // AT REST THE DOCUMENT IS FULL INK: dimming answers "which branch am I in", so
    // it waits until there is a branch to be in.
    await sheet(p, base, "drv-wide");
    const inks = () => {
      const rows = [...document.querySelectorAll("#mdoc .de")];
      const root = getComputedStyle(document.documentElement);
      const head = document.querySelector("#mdoc .de.d-head");
      // A SIBLING IS THE CHOICE THE READER IS STANDING IN, and its own branch comes
      // with it: a branch whose contents are dimmed cannot be weighed.
      const at = document.querySelector("#mdoc .de.dat");
      const sib = at && [...at.parentElement.children]
        .find((n) => n !== at && n.classList.contains("de"));
      const inSib = sib && sib.querySelector(".de");
      // A LINK ON A DIMMED LINE: `.dl' carries its own ink and outranks what it
      // inherits, so it is the one part that can stay lit while its line goes.
      const dimmed = (n) => getComputedStyle(n.closest(".de")).color
                            === rgb(root.getPropertyValue("--g-point-off").trim());
      const box = [...document.querySelectorAll("#mdoc .dbx.on")].find(dimmed);
      const link = [...document.querySelectorAll("#mdoc .de .dl")]
        .find((n) => getComputedStyle(n.closest(".de")).color
                     === rgb(root.getPropertyValue("--g-point-off").trim()));
      return { fg: rgb(root.getPropertyValue("--g-fg").trim()),
               off: rgb(root.getPropertyValue("--g-point-off").trim()),
               focus: !!document.querySelector("#mdoc .focus"),
               head: head ? getComputedStyle(head).color : null,
               sib: sib ? getComputedStyle(sib).color : null,
               inSib: inSib ? getComputedStyle(inSib).color : null,
               link: link ? getComputedStyle(link).color : null,
               box: box ? getComputedStyle(box).color : null,
               inked: rows.map((n) => getComputedStyle(n).color) };
    };
    const rest = await p.eval(inks);
    assert(!rest.focus, "the pane dims before the reader has gone into anything");
    assert(rest.inked.every((c) => c === rest.fg),
      `a row is dimmed at rest: ${rest.inked.find((c) => c !== rest.fg)}`);

    await intoNestedItem(p, base, "drv-wide");
    const held = await p.eval(inks);
    assert(held.focus, "the pane never entered the dimmed mode");
    assert(held.inked.some((c) => c === held.off),
      "no row dimmed while the reader stands inside a list");
    assert(held.inked.some((c) => c === held.fg),
      "every row dimmed, so the branch the reader is in went with them");
    assert(held.sib === null || held.sib === held.fg,
      `a sibling of point paints ${held.sib}, so the choice went dim`);
    assert(held.inSib === null || held.inSib === held.fg,
      `a row inside a sibling paints ${held.inSib}, so its branch cannot be weighed`);
    // THE HEADLINE IS THE ROOT OF THE PATH, so it keeps its ink.
    assert(held.head === held.fg,
      `the headline dimmed to ${held.head} while the reader worked in a list`);
    assert(held.link === null || held.link === held.off,
      `a link on a dimmed line still paints ${held.link}`);
    // `> .dp' OR AN ANCESTOR LIGHTS ITS WHOLE SUBTREE: rows nest, so a ticked box
    // under an owner of point kept the DONE face while its line was dimmed.
    assert(held.box === null || held.box === held.off,
      `a ticked box on a dimmed line still paints ${held.box}`);
    return [`at rest ${rest.inked.length} rows all ${rest.fg}; inside a list `
      + `${held.inked.filter((c) => c === held.off).length} dimmed to ${held.off}, `
      + `${held.inked.filter((c) => c === held.fg).length} kept`];
  } },
{ name: "the line box is whole pixels, so rows sit on the device grid",
  async run(p, base) {
    await sheet(p, base, "drv-wide");
    const seen = await p.eval(() => {
      const item = document.querySelector("#mdoc .d-list .d-item");
      const lh = parseFloat(getComputedStyle(item.querySelector(":scope > .dp")).lineHeight);
      const rows = [...document.querySelectorAll("#mdoc .d-list .d-item")].slice(0, 2);
      const step = rows.length > 1
        ? rows[1].getBoundingClientRect().top - rows[0].getBoundingClientRect().top
        : lh;
      return { lh, step: Math.round(step * 100) / 100 };
    });
    // A WHOLE NUMBER OF PIXELS PER LINE: a 1px hairline and a hinted glyph land on
    // one device row only when every row starts at the same sub-pixel offset, which
    // a fractional line box (13 x 1.6 = 20.8) denies.
    assert(seen.lh % 1 === 0,
      `the line box is ${seen.lh}px, so rows start off the device grid`);
    assert(seen.step % 1 === 0,
      `rows step ${seen.step}px apart, so they do not share one offset`);
    return [`a ${seen.lh}px line box, rows ${seen.step}px apart`];
  } },
{ name: "a continuation lands under the item's own text, checkbox and all",
  async run(p, base) {
    // `drv-plan' carries both a ticked and an empty box, which is where the marker
    // is widest and the arithmetic is worth asking about.
    await sheet(p, base, "drv-plan");
    await walkTo(p, "d-list", "the list");
    await p.press("f");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && at.classList.contains("d-item");
    }, "`f' to reach the list's first item");
    // Walk to the item that carries a box, then open it and split the line.
    // A KEY IS ANSWERED A TURN LATER, so each step waits for the cursor to move --
    // pressing again over a pane that has not answered walks past the box.
    const rowNow = () => p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return at ? at.textContent.slice(0, 40) : "";
    });
    for (let i = 0; i < 6; i++) {
      const box = await p.eval(() => {
        const at = document.querySelector("#mdoc .de.dat");
        return !!at && !!at.querySelector(":scope > .dp > .dbx");
      });
      if (box) break;
      const was = await rowNow();
      await p.press("n");
      await p.until((seen) => {
        const at = document.querySelector("#mdoc .de.dat");
        return !!at && at.dataset.id === docAtNow()
          && at.textContent.slice(0, 40) !== seen;
      }, "the cursor to move on", undefined, was);
    }
    await settled(p);
    await p.press("RET");
    await p.until(() => document.getElementById("dpara").classList.contains("on"),
                  "the edit to open over it");
    // `placeEdit' SIZES THE BOX A TURN AFTER THE RAISE: measured before that, the
    // box still stands over the row the cursor was on, and no growth can beat it.
    await p.until(() => {
      const b = document.getElementById("dpara").getBoundingClientRect();
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && b.height > 0
             && Math.abs(b.height - at.getBoundingClientRect().height) < 1;
    }, "the box to be placed over its row");
    const shut = await p.eval(() =>
      document.getElementById("dpara").getBoundingClientRect().height);
    await p.press("M-RET");
    // SETTING `value' FIRES NO `input', so the listener that re-lays the box after
    // typing never runs for M-RET: the newline places the box itself.
    await p.until((was) => document.getElementById("dpara")
                    .getBoundingClientRect().height > was,
                  "the box to grow by the line M-RET added", undefined, shut);
    const seen = await p.eval(() => {
      const v = document.getElementById("dtext").value;
      const first = v.split("\n").find((l) => l.trim());
      return { value: v, first,
               under: (v.match(/\n( *)/) || [])[1] || "",
               marker: (first.match(/^([ \t]*(?:[-+*]|\d+[.)])\s+(?:\[[ xX-]\]\s+)?)/)
                        || [])[1] || "" };
    });
    assert(seen.marker.length > 0,
      `the item under point spells no marker: ${JSON.stringify(seen.first)}`);
    assert(seen.under.length === seen.marker.length,
      `the newline carried ${seen.under.length} spaces against a marker of `
      + `${seen.marker.length}: ${JSON.stringify(seen.value)}`);
    return [`"${seen.marker}" is ${seen.marker.length} wide, and the continuation `
      + `carries ${seen.under.length}`];
  } },

{ name: "a bullet always paints, and a run wears an unbroken spine",
  async run(p, base) {
    // `drv-marks' spells every marker org writes: `-', `+', an indented `*', both
    // boxes, both ordinals, and a dash quoted inside a block.
    await sheet(p, base, "drv-marks");
    await p.until(() => document.querySelectorAll("#mdoc .dbul").length === 8,
                  "the pane to draw all eight unordered bullets");
    const read = () => {
      // THE PANE IS MONOSPACE, so one cell is the unit every column is counted in.
      const probe = document.createElement("span");
      probe.style.cssText = "position:absolute;visibility:hidden;white-space:pre";
      probe.textContent = "0".repeat(50);
      document.getElementById("mdoc").append(probe);
      const ch = probe.getBoundingClientRect().width / 50;
      probe.remove();
      const readMark = (n) => ({
        text: n.textContent,
        textAt: n.getBoundingClientRect().right,
        left: n.getBoundingClientRect().left,
        ink: getComputedStyle(n).color,
        buls: [...n.querySelectorAll(".dbul")].map((b) => ({
          text: b.textContent,
          ink: getComputedStyle(b).color,
          cell: b.getBoundingClientRect().width })) });
      return { ch,
               stamp: document.documentElement.dataset.bullets || null,
               marks: [...document.querySelectorAll("#mdoc .de > .dp > .dm")].map(readMark),
               items: [...document.querySelectorAll("#mdoc .d-list .d-item")].map((e) => {
                 const b = getComputedStyle(e, "::before");
                 return { text: e.textContent.trim().slice(0, 20),
                          nested: !!(e.parentElement && e.parentElement.closest(".d-item")),
                          barBg: b.backgroundColor, barX: parseFloat(b.left),
                          barH: parseFloat(b.height),
                          h: e.getBoundingClientRect().height }; }),
               boxes: [...document.querySelectorAll("#mdoc .dbx")].map((b) => ({
                 text: b.textContent, ticked: b.classList.contains("on"),
                 ink: getComputedStyle(b).color })) };
    };
    const seen = await p.eval(read);
    const clear = "rgba(0, 0, 0, 0)";
    const loose = (m) => /^\s*[-+*]\s/.test(m.text);
    const ordered = (m) => /^\s*\d+[.)]\s/.test(m.text);
    const cells = (m) => (m.textAt - m.left) / seen.ch;
    // THE LOOK MACHINERY IS GONE: no stamp, whatever an old store still holds.
    await p.eval(() => localStorage.setItem("glance-bullets", "shown"));
    await sheet(p, base, "drv-marks");
    const stamped = await p.eval(() => document.documentElement.dataset.bullets || null);
    await p.eval(() => localStorage.removeItem("glance-bullets"));
    assert(seen.stamp === null && stamped === null,
      `the retired bullets look still stamps "${seen.stamp}"/"${stamped}"`);
    assert(seen.marks.length === 10 && seen.marks.filter(loose).length === 8
             && seen.marks.filter(ordered).length === 2,
      `the sheet drew ${JSON.stringify(seen.marks.map((m) => m.text))}`);
    // ORG'S OWN MARKER, ALWAYS DRAWN: the bullet paints in its marker's ink,
    // one cell wide, and the marker spans the columns org wrote.
    for (const m of seen.marks.filter(loose)) {
      assert(m.buls.length === 1 && m.buls[0].ink !== clear && m.buls[0].ink === m.ink,
        `"${m.text}" paints its bullet ${JSON.stringify(m.buls)} against ${m.ink}`);
      assert(Math.abs(m.buls[0].cell - seen.ch) < 0.05,
        `"${m.text}" draws its bullet ${px(m.buls[0].cell)} wide against a `
        + `${px(seen.ch)} cell`);
      assert(Math.abs(cells(m) - m.text.length) < 0.05,
        `"${m.text}" spans ${cells(m).toFixed(2)} cells for ${m.text.length} characters`);
    }
    for (const m of seen.marks.filter(ordered))
      assert(m.buls.length === 0 && m.ink !== clear,
        `the ordinal "${m.text}" reads ${JSON.stringify(m)}`);
    assert(seen.boxes.length === 2 && seen.boxes.filter((b) => b.ticked).length === 1
             && seen.boxes[0].ink !== seen.boxes[1].ink,
      `the sheet drew ${JSON.stringify(seen.boxes)}`);
    // A RUN WEARS A SPINE: every item bars its whole extent at its run's rail,
    // siblings at one column, a nested run one deeper inside its parent's.
    const tops = seen.items.filter((i) => !i.nested);
    const deeps = seen.items.filter((i) => i.nested);
    assert(tops.length >= 2 && deeps.length >= 1,
      `the fixture lists ${tops.length} top items and ${deeps.length} nested`);
    for (const i of seen.items) {
      assert(i.barBg !== clear && i.barBg !== "none",
        `"${i.text}" wears no spine: ${i.barBg}`);
      assert(Math.abs(i.barH - i.h) < 1,
        `"${i.text}" bars ${i.barH}px of its ${i.h}px extent`);
    }
    assert(new Set(tops.map((i) => Math.round(i.barX * 10))).size === 1,
      `sibling runs bar at ${JSON.stringify(tops.map((i) => i.barX))}`);
    assert(deeps.every((d) => d.barX > tops[0].barX + seen.ch),
      `a nested run bars at ${JSON.stringify(deeps.map((d) => d.barX))} against `
      + `its parent's ${tops[0].barX}`);
    return [`8 bullets paint, 2 ordinals and 2 boxes stay content; `
      + `${seen.items.length} items bar their extent, nested runs a column deeper`];
  } },
{ name: "the drawer is a stop: folded to one line, TAB opens it, the tree marks its pairs",
  async run(p, base) {
    await sheet(p, base, "drv-marks");
    const read = () => p.eval(() => {
      const drawer = document.querySelector("#mdoc .d-drawer");
      const point = rgb(getComputedStyle(document.documentElement)
        .getPropertyValue("--g-point").trim());
      return { folded: drawer.textContent,
               point,
               frameInk: [...drawer.querySelectorAll(".dg")].map((g) =>
                 getComputedStyle(g).color),
               keyInk: [...drawer.querySelectorAll(".dk")].map((k) =>
                 getComputedStyle(k).color),
               frames: [...drawer.querySelectorAll(".dg")].map((g) => g.textContent),
               pairs: [...drawer.querySelectorAll(".d-meta")].map((i) => i.textContent),
               bars: [...drawer.querySelectorAll(".d-meta")].map((i) =>
                 [getComputedStyle(i, "::before").borderLeftWidth,
                  getComputedStyle(i, "::before").width].join(" ")),
               crumb: document.querySelector("#mdoc .dpath").textContent,
               planning: [...document.querySelectorAll("#mdoc .d-meta")].map((e) => e.textContent) };
    });
    const shut = await read();
    // FOLDED IS THE DEFAULT, org's own ellipsis with its breathing space.
    assert(shut.folded === ":PROPERTIES: …" && shut.pairs.length === 0,
      `the drawer opens showing ${JSON.stringify(shut.folded)}`);
    assert(shut.planning.length === 1 && /^DEADLINE: </.test(shut.planning[0]),
      `the planning line reads ${JSON.stringify(shut.planning)}`);
    // Onto the drawer, and TAB opens it.
    await walkTo(p, "d-drawer", "the drawer");
    await p.press("TAB");
    await p.until(() => document.querySelectorAll("#mdoc .d-drawer .d-meta").length > 0,
                  "TAB to open the drawer");
    const open = await read();
    assert(open.frames.length === 2
             && open.frames[0] === ":PROPERTIES:" && open.frames[1] === ":END:",
      `the frame reads ${JSON.stringify(open.frames)}`);
    // THE HIDDEN PAIR STAYS HIDDEN: the id is the store's, never the reader's.
    assert(open.pairs.length === 2 && !open.pairs.some((t) => /ORG_GLANCE_ID/.test(t)),
      `the open drawer holds ${JSON.stringify(open.pairs)}`);
    // A PAIR IS NOT NESTED: no elbow, a paragraph's own thin bar -- which spends
    // `--ink', so a flag has somewhere to say so.
    assert(open.bars.length === 2 && open.bars.every((b) => b === "0px 1px"),
      `a pair's mark reads ${JSON.stringify(open.bars)}`);
    assert(/:PROPERTIES:/.test(open.crumb),
      `the strip reads ${JSON.stringify(open.crumb)}`);
    // THE DRAWER IS A RESERVED TOKEN, frame and keys alike, in point's own ink.
    assert(open.frameInk.every((c) => c === open.point)
             && open.keyInk.length === 2 && open.keyInk.every((c) => c === open.point),
      `the frame paints ${JSON.stringify(open.frameInk)} and the keys `
      + `${JSON.stringify(open.keyInk)} against point's ${open.point}`);
    // TAB folds it back.
    await p.press("TAB");
    await p.until(() => document.querySelectorAll("#mdoc .d-drawer .d-meta").length === 0,
                  "TAB to fold the drawer");
    // And `f' INTO the folded drawer opens it and lands on the first pair.
    await p.press("f");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && at.classList.contains("d-meta") && /OWNER/.test(at.textContent);
    }, "f to open the drawer and land on its first pair");
    return [`folded "${shut.folded}", open ${JSON.stringify(open.pairs)}, `
      + `crumb "${open.crumb.trim()}"`];
  } },

{ name: "a pair edits as its own line and leaves through the lists, never the splice",
  async run(p, base) {
    const served = () => p.eval(async () => {
      const r = await fetch("/headline?id=drv-marks");
      const h = await r.json();
      return { props: h.properties, plan: h.planning };
    });
    await sheet(p, base, "drv-marks");
    await walkTo(p, "d-drawer", "the drawer");
    await p.press("f");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && at.classList.contains("d-meta") && /OWNER/.test(at.textContent);
    }, "point on the OWNER pair");
    await settled(p);
    // RET EDITS THE LINE AS ORG SPELLS IT.
    await p.press("RET");
    await p.until(() => document.getElementById("dpara").classList.contains("on"), "the edit");
    const seeded = await p.eval(() => document.getElementById("dtext").value);
    assert(seeded === ":OWNER: reader", `the edit seeds ${JSON.stringify(seeded)}`);
    await p.eval(() => { document.getElementById("dtext").value = ":OWNER: writer"; });
    await p.press("RET");
    await p.until(async () => {
      const h = await (await fetch("/headline?id=drv-marks")).json();
      return (h.properties || []).some(([k, v]) => k === "OWNER" && v === "writer");
    }, "the file to carry the new value", 15000);
    // THE HIDDEN ID SURVIVES THE WRITE, or every write would orphan the row.
    const alive = await p.eval(async () =>
      (await (await fetch("/headline?id=drv-marks")).json()).id);
    assert(alive === "drv-marks", `the id came back as ${JSON.stringify(alive)}`);
    // `d d' DROPS THE PAIR through the same door.
    await p.until(() => !!document.querySelector("#mdoc .de.dat"), "point back");
    await p.press("n");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && /EFFORT/.test(at.textContent);
    }, "point on the EFFORT pair");
    await settled(p);
    await p.press("d");
    await p.until(() => !!document.querySelector("#mdoc .de.dfl"), "the flag to draw");
    await p.press("d");
    await p.until(async () => {
      const h = await (await fetch("/headline?id=drv-marks")).json();
      return !(h.properties || []).some(([k]) => k === "EFFORT");
    }, "the pair to leave the file", 15000);
    // `+' ASKS FROM THE DRAWER; the sheet is reopened so the walk is the reader's own.
    await sheet(p, base, "drv-marks");
    await walkTo(p, "d-drawer", "the drawer");
    await p.press("+");
    // `+' ASKS, org's way: the key, then the value, both required.
    await p.until(() => /property key/.test(document.getElementById("phead").textContent),
                  "the key prompt");
    await p.type("ROOM");
    await p.press("RET");
    await p.until(() => /value for :ROOM:/.test(document.getElementById("phead").textContent),
                  "the value prompt");
    await p.type("12");
    await p.press("RET");
    await p.until(async () => {
      const h = await (await fetch("/headline?id=drv-marks")).json();
      return (h.properties || []).some(([k, v]) => k === "ROOM" && v === "12");
    }, "the minted pair to reach the file", 15000);
    const end = await served();
    return [`OWNER edited, EFFORT dropped, ROOM added: ${JSON.stringify(end.props)}`];
  } },
{ name: "a child is drawn whole, walked like a list, and edits through the same splice",
  async run(p, base) {
    await sheet(p, base, "drv-marks");
    const shelf = await p.eval(() => {
      const rows = [...document.querySelectorAll("#mdoc .de")];
      const kids = rows.filter((e) => e.classList.contains("d-child"));
      return { kids: kids.map((e) => e.textContent.trim().slice(0, 30)),
               // A CHILD'S CONTENTS ARE IN THE PANE, not behind a materialize.
               paras: rows.filter((e) => e.classList.contains("d-para"))
                 .map((e) => e.textContent.slice(0, 30)) };
    });
    assert(shelf.kids.length === 3,
      `the pane drew ${shelf.kids.length} child headlines: ${JSON.stringify(shelf.kids)}`);
    assert(shelf.paras.some((t) => /A paragraph the child owns/.test(t))
             && shelf.paras.some((t) => /The grandchild's own line/.test(t)),
      `the children's contents are missing: ${JSON.stringify(shelf.paras)}`);
    // A SHELF INDENTS UNDER ITS OWN FIRST LETTER: the cleaned stars step two
    // characters a level, and the contents step with them -- org's own geometry.
    const insets = await p.eval(() => {
      const pad = (sel, re) => {
        const e = [...document.querySelectorAll(sel)]
          .find((n) => re.test(n.textContent));
        return e ? parseFloat(getComputedStyle(e).paddingLeft) : null;
      };
      const probe = document.createElement("span");
      probe.style.cssText = "position:absolute;visibility:hidden;white-space:pre";
      probe.textContent = "0".repeat(20);
      document.getElementById("mdoc").append(probe);
      const ch = probe.getBoundingClientRect().width / 20;
      probe.remove();
      return { ch,
               root: pad("#mdoc .d-para", /The entry a case reads/),
               kid: pad("#mdoc .d-para", /A paragraph the child owns/),
               grand: pad("#mdoc .d-para", /The grandchild's own line/) };
    });
    assert(Math.abs(insets.kid - insets.root - 2 * insets.ch) < 0.1
             && Math.abs(insets.grand - insets.root - 4 * insets.ch) < 0.1,
      `the shelves indent ${insets.root}/${insets.kid}/${insets.grand} on a `
      + `${insets.ch}px character`);
    // THE SHELF WALKS LIKE A LIST: n from the first child lands on the second,
    // never inside the first — a sibling shares an owner.
    for (let i = 0; i < 9; i += 1) {
      const seen = await p.eval(() => {
        const at = document.querySelector("#mdoc .de.dat");
        return { kid: at.classList.contains("d-child"), text: at.textContent.slice(0, 30) };
      });
      if (seen.kid) break;
      await p.press("n");
      await p.until((was) => document.querySelector("#mdoc .de.dat")
                      .textContent.slice(0, 30) !== was,
                    "the step to settle", undefined, seen.text);
    }
    const first = await p.eval(() => document.querySelector("#mdoc .de.dat").textContent);
    assert(/A child whose body/.test(first), `the walk reached ${JSON.stringify(first)}`);
    await p.press("n");
    await p.until(() => /A second child/.test(
      document.querySelector("#mdoc .de.dat").textContent),
      "n to step to the SECOND child, over the first's whole subtree");
    await p.press("p");
    // And `f' goes INTO the child: its first block, owned by it.
    await p.until(() => /A child whose body/.test(
      document.querySelector("#mdoc .de.dat").textContent), "p back to the first child");
    await p.press("f");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && /A paragraph the child owns/.test(at.textContent)
        && at.classList.contains("d-para");
    }, "f to enter the child's own paragraph");
    await settled(p);
    // THE SPLICE IS THE SAME DOOR: editing the child's paragraph writes the file.
    await p.press("RET");
    await p.until(() => document.getElementById("dpara").classList.contains("on"), "the edit");
    await p.eval(() => { document.getElementById("dtext").value = "A paragraph the child edits."; });
    await p.press("RET");
    await p.until(async () => {
      const h = await (await fetch("/headline?id=drv-marks")).json();
      return /A paragraph the child edits\./.test(h.org || "");
    }, "the child's line to reach the file", 15000);
    // `b' CLIMBS TO THE OWNER: the paragraph's owner is the child headline.
    await p.until(() => !!document.querySelector("#mdoc .de.dat"), "point back");
    await settled(p);
    await p.press("b");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && at.classList.contains("d-child");
    }, "b to climb to the child headline");
    // TAB ON A HEADLINE FOLDS ITS SUBTREE, org's own cycle: the contents leave
    // the pane whole -- the grandchild with them -- and TAB brings them back.
    await settled(p);
    await p.press("TAB");
    await p.until(() => ![...document.querySelectorAll("#mdoc .d-para")]
        .some((e) => /A paragraph the child edits/.test(e.textContent)),
      "TAB to fold the child's subtree away");
    const foldedKid = await p.eval(() => ({
      gone: ![...document.querySelectorAll("#mdoc .d-para")]
        .some((e) => /The grandchild's own line/.test(e.textContent)),
      mark: /…/.test(document.querySelector("#mdoc .de.dat").textContent) }));
    assert(foldedKid.gone, "the grandchild's line survived the fold");
    assert(foldedKid.mark, "the folded child wears no ellipsis");
    await p.press("TAB");
    await p.until(() => [...document.querySelectorAll("#mdoc .d-para")]
        .some((e) => /A paragraph the child edits/.test(e.textContent)),
      "TAB to open it again");
    return [`3 children inline; the shelf steps over subtrees; a child's paragraph `
      + `edits through the splice; TAB folds the subtree whole`];
  } },
{ name: "the pane is a narrowing, so a typed root-level headline is demoted",
  async run(p, base) {
    await sheet(p, base, "drv-marks");
    await walkTo(p, "d-para", "the entry's own paragraph");
    await p.press("RET");
    await p.until(() => document.getElementById("dpara").classList.contains("on"), "the edit");
    // NOTHING WRITTEN MAY ESCAPE THE SUBTREE: `* ' at the root's level would,
    // so it lands as `** ', the first child level -- org's narrowed buffer.
    await p.eval(() => { document.getElementById("dtext").value
      += "\n\n* Sneak past the narrowing"; });
    await p.press("RET");
    await p.until(async () => {
      const h = await (await fetch("/headline?id=drv-marks")).json();
      return /Sneak past the narrowing/.test(h.org || "");
    }, "the write to reach the file", 15000);
    const seen = await p.eval(async () => {
      const h = await (await fetch("/headline?id=drv-marks")).json();
      return { deep: /\n\*\* Sneak past the narrowing/.test(h.org || ""),
               flat: /\n\* Sneak past the narrowing/.test(h.org || "") };
    });
    assert(seen.deep && !seen.flat,
      "a typed `* ' headline was not demoted to `** ' inside the subtree");
    return ["`* Sneak' landed as `** Sneak': the subtree kept its walls"];
  } },
];
