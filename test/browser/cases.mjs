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

/** WALK THE SHELF TO THE FIRST ROW SEL MATCHES.  A FIXED key count lands
 * elsewhere the moment a row is added above — the header rows are. */
async function walkTo(p, sel, what) {
  for (let i = 0; i < 12; i += 1) {
    // THE MIRROR MUST AGREE WITH THE DRAW before the walk trusts a reading: the
    // DOM paints on rAF and the port lands a macrotask apart, and a key pressed
    // in that gap acts on the row the reader just left.
    const seen = await p.eval((s) => {
      const at = document.querySelector("#mdoc .de.dat");
      const synced = !!at && at.dataset.id === docAtNow();
      return { hit: synced && at.matches(s),
               synced,
               // A HEADLINE WALKS HEADLINES NOW, so the way into contents is
               // `f' -- the walk dives off a headline unless one is the target.
               dive: !!at && at.matches(".d-head, .d-child") && !at.matches(s),
               text: at ? at.textContent.slice(0, 30) : "" };
    }, sel);
    if (seen.hit) return;
    if (!seen.synced) {
      await settled(p, `the mirror to settle on the way to ${what}`);
      continue;
    }
    await p.press(seen.dive ? "f" : "n");
    await p.until((was) => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && at.dataset.id === docAtNow()
        && at.textContent.slice(0, 30) !== was;
    }, `the step toward ${what}`, undefined, seen.text);
  }
  assert(false, `\`n' never reached ${what}`);
}

/** WALK BACKWARD to the first row SEL matches — `walkTo''s mirror, `p' where it
 * presses `n'.  A capture's point opens where `%?' stood, which is BELOW the
 * header rows the drawer is among. */
async function walkBackTo(p, sel, what) {
  for (let i = 0; i < 12; i += 1) {
    const seen = await p.eval((s) => {
      const at = document.querySelector("#mdoc .de.dat");
      const synced = !!at && at.dataset.id === docAtNow();
      return { hit: synced && at.matches(s), synced,
               text: at ? at.textContent.slice(0, 30) : "" };
    }, sel);
    if (seen.hit) return;
    if (!seen.synced) {
      await settled(p, `the mirror to settle on the way back to ${what}`);
      continue;
    }
    await p.press("p");
    await p.until((was) => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && at.dataset.id === docAtNow()
        && at.textContent.slice(0, 30) !== was;
    }, `the step back toward ${what}`, undefined, seen.text);
  }
  assert(false, `\`p' never reached ${what}`);
}

/** `+' with the tag field up, which every capture starts from. */
async function captureForm(p, base) {
  await tableUp(p, base);
  await p.press("+");
  await p.until(() => document.getElementById("capture").className === "on"
                  && document.activeElement === document.getElementById("ktag"),
                "the tag field to open and take the focus");
}

/** The sheet over the draft TAG's template expands to; `""' is the inbox and
 * its default template, which is star-space and nothing else. */
async function draftOver(p, base, tag) {
  await captureForm(p, base);
  if (tag) await p.type(tag);
  await p.press("RET");
  await p.until(() => !!document.querySelector("#modal.on")
                  && document.getElementById("capture").className !== "on",
                `the sheet to open over the ${tag || "inbox"} draft`);
  await settled(p);
}

/** The ids the STORE holds under QUERY, asked of the daemon rather than read off
 * the table: what a capture did or did not write is a fact about the tree. */
const idsUnder = (p, q) => p.eval(async (query) => {
  const r = await fetch(`/headlines?limit=500&q=${encodeURIComponent(query)}`);
  const j = await r.json();
  return ((j.view && j.view.rows) || j.rows || []).map((x) => x.id);
}, q);

/** The draw and the mirror agreeing on the row under point — REQUIRED before a
 * key that acts on the cursor, whenever the wait watched the DOM alone. */
const settled = (p, why) =>
  p.until(() => {
    const at = document.querySelector("#mdoc .de.dat");
    return !!at && at.dataset.id === docAtNow();
  }, why || "the mirror to agree with the draw");

/** THE SCROLL A PORT PUSH MAKES: `keepInView' runs a macrotask behind the state
 * the walk waited on, so a geometry read waits a frame out first. */
const rested = (p) => p.eval(() => new Promise((done) =>
  requestAnimationFrame(() => requestAnimationFrame(done))));

/** A PAGE FUNCTION, passed to `p.eval': the pane, point's row and where the two
 * stand, taken as ONE reading so nothing drifts between two round trips. */
const paneRead = () => {
  const at = document.querySelector("#mdoc .de.dat");
  // THE SCROLLER THE PANE ACTUALLY HAS, asked for the way the page asks: `#mdoc'
  // stretches to `#mpanes' where the document fits and grows past it where it
  // does not, and `#mpanes' is what clips and scrolls then.
  let pane = at.parentElement;
  for (; pane; pane = pane.parentElement) {
    if (pane.clientHeight > 0 && pane.scrollHeight > pane.clientHeight + 1) break;
    if (pane.id === "modal") { pane = document.getElementById("mdoc"); break; }
  }
  if (!pane) pane = document.getElementById("mdoc");
  const r = at.getBoundingClientRect();
  const top = pane.getBoundingClientRect().top + pane.clientTop;
  return { top, height: pane.clientHeight, scroll: pane.scrollTop,
           room: pane.scrollHeight - pane.clientHeight,
           off: parseFloat(getComputedStyle(at).scrollMarginBlockStart) || 0,
           rowTop: r.top, rowBottom: r.bottom, rowHeight: r.height,
           text: at.textContent.trim().slice(0, 28) };
};

/** WHERE THE THREE RUNGS WOULD PUT THE SCROLLER, off one `paneRead'.  A rung
 * outside [0, room] is one the pane must clamp, and clamping proves nothing. */
const rungs = (b) => {
  const deep = b.rowTop - b.top + b.scroll;
  return { center: deep + b.rowHeight / 2 - b.height / 2,
           top: deep - b.off,
           bottom: deep + b.rowHeight - b.height + b.off };
};

/** Press KEY and wait for point to land on SEL -- a NEW row the mirror owns,
 * since the old row satisfies every wait until the press is processed. */
async function stepped(p, key, sel, why) {
  const prev = await p.eval(() => {
    const at = document.querySelector("#mdoc .de.dat");
    return at ? at.dataset.id : "";
  });
  await p.press(key);
  await p.until((a) => {
    const at = document.querySelector("#mdoc .de.dat");
    return !!at && at.dataset.id === docAtNow() && at.dataset.id !== a.prev
      && at.matches(a.sel);
  }, why, undefined, { sel, prev });
}

/** The paragraph edit box open. */
const editUp = (p, why) =>
  p.until(() => document.getElementById("dpara").classList.contains("on"),
          why || "the edit to open");

/** The pair box open over its drawn row, which the box needs to be placed on. */
const pairUp = (p, why) =>
  p.until(() => document.getElementById("dpair").classList.contains("on")
            && document.querySelectorAll("#mdoc .d-draft").length === 1,
          why || "the pair's two fields to open");

/** The date widget's field open, focused and PLACED over the slot it writes.
 * Placed is waited for by the box's own inline `top': the summon's placement can
 * run before the pane has drawn the slot (the port redraw places it a beat
 * later), and a rect measured in that window reads the box's resting spot — the
 * filed read-races-render family. */
const widgetUp = (p, why) => p.until(() => {
  const box = document.getElementById("ddate");
  const f = document.getElementById("dwhen");
  return box.classList.contains("on") && document.activeElement === f
    && f.getBoundingClientRect().width > 0 && box.style.top !== "";
}, why);

/** `placeEdit' SIZES THE BOX A TURN AFTER THE RAISE: read before that, the box
 * still stands over the row point left, one line tall. */
const boxPlaced = (p, why) =>
  p.until(() => {
    const b = document.getElementById("dpara");
    const at = document.querySelector("#mdoc .de.dat");
    if (!b || !b.classList.contains("on") || !at) return false;
    const h = b.getBoundingClientRect().height;
    return h > 0 && Math.abs(h - at.getBoundingClientRect().height) < 1;
  }, why || "the box to be placed over its row");

/** Down to an item that has rows drawn inside it. */
async function intoNestedItem(p, base, row) {
  await sheet(p, base, row);
  await walkTo(p, ".d-list", "the list itself");
  await stepped(p, "f", ".d-item", "`f' to descend into the list");
  await walkTo(p, ".d-item:has(> .de)", "an item with rows drawn inside it");
}

/** WALK POINT TO THE ROW WHOSE OWN LINE CARRIES TEXT.  `walkTo''s twin, keyed on
 * the row's own `.dp'/`.ds' rather than a selector: `f' dives off a container
 * (a headline, a list, an item with rows inside it) and `n' steps along a run,
 * so a leaf nested two lists deep is reachable by its words alone. */
async function walkToText(p, text, why) {
  for (let i = 0; i < 25; i += 1) {
    const seen = await p.eval((t) => {
      const at = document.querySelector("#mdoc .de.dat");
      if (!at) return { synced: false };
      const synced = at.dataset.id === docAtNow();
      const own = at.querySelector(":scope > .dp") || at.querySelector(":scope > .ds");
      const inOwn = !!own && own.textContent.includes(t);
      // A HEADLINE'S BODY IS A SIBLING, so `f' off it is unconditional; every
      // other container is dived only when the target is somewhere inside it, so
      // the walk never descends into the empty drawer that has none.
      const isHead = at.matches(".d-head, .d-child");
      const holds = (at.matches(".d-list, .d-comp")
        || (at.matches(".d-item") && !!at.querySelector(":scope > .de")))
        && at.textContent.includes(t);
      return { synced, hit: synced && inOwn,
               dive: synced && !inOwn && (isHead || holds),
               // A COMPOSITE'S text STARTS WITH ITS FIRST CHILD'S, so the step is
               // read off the id the mirror owns, never off the words.
               id: at.dataset.id };
    }, text);
    if (seen.hit) return;
    if (!seen.synced) { await settled(p, why); continue; }
    await p.press(seen.dive ? "f" : "n");
    await p.until((was) => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && at.dataset.id === docAtNow() && at.dataset.id !== was;
    }, why || `the walk to "${text}"`, undefined, seen.id);
  }
  assert(false, `the walk never reached "${text}"`);
}

/** The sheet over ROW with its first paragraph open for editing. */
async function paraOpen(p, base, row) {
  await sheet(p, base, row);
  await walkTo(p, ".d-para", "the first paragraph");
  await p.press("RET");                                 // open it
  await editUp(p, "the paragraph edit box to open");
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

// THE TABLE'S OWN FILTER IS SUMMONED TOO (`filterDock: "strip"'), onto the chip
// strip's own row, and lives under `#app'.
// ONE BOX, TWO DOORS: `/' edits the filter half and `.' the whole expression.
/** Summon the table's box with KEY and wait for it to take the keyboard. */
async function boxUp(p, key, why) {
  await p.press(key);
  await p.until(() => document.activeElement.tagName === "INPUT"
                   && document.getElementById("app").contains(document.activeElement),
                why);
}
/** Raise the table's filter box with `/' and wait for it to take the keyboard. */
const filterUp = (p, why) =>
  boxUp(p, "/", why || "the table's filter box to take the focus");
/** Raise the same box on the whole expression, which is `.'. */
const queryUp = (p, why) =>
  boxUp(p, ".", why || "the whole-query box to take the focus");
/** The suggestion list, closed. */
const acShut = () => {
  const ac = document.querySelector("#app .tv-ac");
  return !ac || !ac.children.length || ac.style.display === "none";
};
/** The suggestion list, up with something in it. */
const acOpen = () => {
  const ac = document.querySelector("#app .tv-ac");
  return !!ac && ac.children.length > 0 && ac.style.display !== "none";
};
/** What the open list offers, as the reader reads it. */
const acLabels = () =>
  [...document.querySelectorAll("#app .tv-ac-item .tv-ac-label")].map((e) => e.textContent);
/** RET on the table's filter box, and the strip settling at N chips.  An open
 *  suggestion list takes Enter for itself, so it is dismissed FIRST -- and only
 *  when it is up, ESC over a closed list being what drops the typed token. */
async function committed(p, n, why) {
  if (!(await p.eval(acShut))) {
    await p.press("ESC");
    await p.until(acShut, "the suggestion list to close, so RET commits");
  }
  await p.press("RET");
  await p.until((k) => document.querySelectorAll("#app .tv-chip[data-i]").length === k,
                why, undefined, n);
}
/** The strip's chips as the reader reads them, the `×' left off. */
const stripText = () => [...document.querySelectorAll("#app .tv-chip[data-i]")]
  .map((c) => c.firstChild.textContent);
const appRows = () => document.querySelectorAll("#app .tv-table tbody tr").length;

export default [

// A material doc taller than the pane must SCROLL inside the pane, not grow the
// sheet past the viewport.  `tall.org' `drv-tall' is a body of 80 items.
{ name: "a material doc taller than the pane scrolls in the pane",
  async run(p, base) {
    await sheet(p, base, "drv-tall");
    await settled(p, "the tall sheet");
    const d = await p.eval(() => {
      const D = (id) => { const e = document.getElementById(id); const r = e.getBoundingClientRect(); return { ch: e.clientHeight, sh: e.scrollHeight, bottom: Math.round(r.bottom) }; };
      const mdoc = document.getElementById("mdoc");
      mdoc.scrollTop = 400;
      const moved = mdoc.scrollTop;
      mdoc.scrollTop = 0;
      return { sheet: D("sheet"), mdoc: D("mdoc"), moved, vh: window.innerHeight };
    });
    assert(d.mdoc.sh > d.mdoc.ch + 50,
      `the pane does not contain the overflow: content ${d.mdoc.sh}px is not taller than its ${d.mdoc.ch}px visible box — it grew to fit instead of scrolling`);
    assert(d.sheet.bottom <= d.vh + 1,
      `the sheet overflows the viewport: it ends at ${d.sheet.bottom}px past ${d.vh}px`);
    assert(d.moved > 0, `#mdoc did not scroll: scrollTop stuck at ${d.moved}`);
    return [`content ${d.mdoc.sh}px scrolls in a ${d.mdoc.ch}px pane (moved ${d.moved}px); sheet ends at ${d.sheet.bottom}/${d.vh}`];
  } },

// A left click selects the element it lands on (point moves there); a double
// click edits it, exactly as RET does.  marks.org `B5' is a checkbox item.
{ name: "a left click selects the material-doc element under it",
  async run(p, base) {
    await sheet(p, base, "drv-marks");
    await settled(p, "the sheet");
    await p.click('#mdoc .de[data-id="B5"]');
    await p.until(() => { const a = document.querySelector("#mdoc .de.dat"); return !!a && a.dataset.id === "B5"; },
                  "the click to select B5");
    const id = await p.eval(() => { const a = document.querySelector("#mdoc .de.dat"); return a ? a.dataset.id : null; });
    assert(id === "B5", `a click did not select the element it landed on; point is on ${id}`);
    return [`click selected ${id}`];
  } },

{ name: "a double click edits the material-doc element, as RET does",
  async run(p, base) {
    await sheet(p, base, "drv-marks");
    await settled(p, "the sheet");
    await p.dblclick('#mdoc .de[data-id="B5"]');
    await p.until(() => !!document.querySelector("#dpara.on") && document.activeElement === document.getElementById("dtext"),
                  "the double-click to open an edit");
    const st = await p.editorState();
    assert((st.dtext || "").includes("a box the reader has not"),
      `a double click did not open the element's edit; dtext=${JSON.stringify(st.dtext)}`);
    await p.press("ESC");
    return [`double-click opened edit ${JSON.stringify(st.dtext)}`];
  } },

// The planning line is ONE row of several values; a click on a value selects
// THAT entry (SCHEDULED/DEADLINE/CLOSED), not the whole line, so the mouse can
// reach one to edit.  plans.org `drv-plan' has SCHEDULED and DEADLINE.
{ name: "a click on a planning value selects that entry, not the whole line",
  async run(p, base) {
    await sheet(p, base, "drv-plan");
    await settled(p, "the plan sheet");
    await p.click('#mdoc .dpv[data-key="DEADLINE"]');
    await p.until(() => { const s = window["__glance"] && window["__glance"].editor(); return !!s && (s.planKey || "").toUpperCase() === "DEADLINE"; },
                  "the click to select the deadline entry").catch(() => {});
    const st = await p.editorState();
    assert((st.planKey || "").toUpperCase() === "DEADLINE",
      `a click on the deadline value did not select it; planKey=${JSON.stringify(st.planKey)}`);
    return [`click selected planning entry ${JSON.stringify(st.planKey)}`];
  } },

// EVERY HEADLINE ALWAYS SHOWS A SCHEDULED AND A DEADLINE SLOT: where the file
// spells no value the slot stands UNSET -- a muted, clickable placeholder -- so
// the reader can reach one to set it.  unset.org `drv-unset-none' has neither
// keyword; `drv-unset-one' scheduled but never dated.
{ name: "a headline with no planning still shows unset SCHEDULED and DEADLINE slots",
  async run(p, base) {
    const slots = () => p.eval(() => {
      const read = (key) => {
        const s = document.querySelector(`#mdoc .dpv[data-key="${key}"]`);
        return s ? { has: true, unset: !!s.querySelector(".dpunset"),
                     text: s.textContent } : { has: false, unset: false, text: null };
      };
      return { sched: read("SCHEDULED"), dead: read("DEADLINE") };
    });
    // ------- a headline the file gave NO planning line: both slots stand unset.
    await sheet(p, base, "drv-unset-none");
    await settled(p, "the no-planning sheet");
    const none = await slots();
    assert(none.sched.has && none.dead.has,
      `the no-planning headline drew no slots: SCHEDULED ${none.sched.has}, `
      + `DEADLINE ${none.dead.has}`);
    assert(none.sched.unset && none.dead.unset,
      `a slot is not the unset placeholder: ${JSON.stringify(none)}`);
    // ------- a headline scheduled but never dated: the set value stands beside
    // the still-unset DEADLINE slot.
    await sheet(p, base, "drv-unset-one");
    await settled(p, "the one-planning sheet");
    const one = await slots();
    assert(one.sched.has && !one.sched.unset && /</.test(one.sched.text || ""),
      `the scheduled headline drew its SCHEDULED value wrong: ${JSON.stringify(one.sched)}`);
    assert(one.dead.has && one.dead.unset,
      `the scheduled headline did not draw an unset DEADLINE slot: ${JSON.stringify(one.dead)}`);
    return [`no-planning headline draws SCHEDULED ${JSON.stringify(none.sched.text)} and `
      + `DEADLINE ${JSON.stringify(none.dead.text)} unset; the scheduled one keeps its `
      + `${JSON.stringify(one.sched.text)} beside an unset DEADLINE`];
  } },

// AND AN UNSET SLOT IS EDITABLE END TO END: a click selects that entry, RET
// opens the date widget over it, and committing WRITES the keyword -- a headline
// that had no planning line at all gains one.
{ name: "an unset DEADLINE slot selects, edits, and writes to a headline that had none",
  async run(p, base) {
    await sheet(p, base, "drv-unset-none");
    await settled(p, "the no-planning sheet");
    const was = await p.eval(async () =>
      (await (await fetch("/headline?id=drv-unset-none")).json()).planning || []);
    assert(was.length === 0, `drv-unset-none already carries ${JSON.stringify(was)}`);
    // A CLICK ON THE UNSET SLOT SELECTS THAT ENTRY, not the whole line.
    await p.click('#mdoc .dpv[data-key="DEADLINE"]');
    await p.until(() => { const s = window["__glance"] && window["__glance"].editor();
      return !!s && (s.planKey || "").toUpperCase() === "DEADLINE"; },
      "the click to select the unset DEADLINE entry");
    // RET OVER IT OPENS THE DATE WIDGET, keyed on the entry point stands in.
    await p.press("RET");
    await widgetUp(p, "the date widget over the unset DEADLINE slot");
    // TYPING A DATE AND COMMITTING WRITES DEADLINE to a headline that had none.
    await p.type("20 aug");
    await p.press("RET");
    const landed = await p.until(async () => {
      const h = await (await fetch("/headline?id=drv-unset-none")).json();
      const at = (h.planning || []).find(([k]) => k === "DEADLINE");
      return at ? at[1] : false;
    }, "the DEADLINE to reach the planning line", 15000);
    assert(/^<\d{4}-08-20 /.test(landed),
      `the file wrote ${JSON.stringify(landed)} for DEADLINE`);
    return [`clicked the unset DEADLINE slot, RET opened the widget, "20 aug" wrote `
      + `${JSON.stringify(landed)} to a headline that had no planning line`];
  } },

{ name: "the breadcrumbs render a link as its text, not raw org syntax",
  async run(p, base) {
    await sheet(p, base, "drv-crumbs");
    await settled(p, "the crumbs sheet");
    const crumb = await p.eval(() => { const b = document.getElementById("mwhere"); return b ? b.textContent : null; });
    assert(!/\[\[/.test(crumb || ""), `the breadcrumb shows raw link syntax: ${JSON.stringify(crumb)}`);
    assert(/Klarenbeekstraat/.test(crumb || ""), `the breadcrumb lost the link text: ${JSON.stringify(crumb)}`);
    return [`breadcrumb reads ${JSON.stringify(crumb)}`];
  } },

// THE IN-DOC PATH STRIP (`.dpath') NAMES A CHILD BY ITS TITLE: a title carrying
// an org link must read its DESCRIPTION, not `[[glance:id][Straat]]', the way
// the breadcrumbs do.  crumbs.org `drv-crumb-kid' holds a child titled with a
// link; point in its body puts that child on the strip.
{ name: "the in-doc path renders a linked crumb as its text, not raw org syntax",
  async run(p, base) {
    await sheet(p, base, "drv-crumb-kid");
    await walkToText(p, "Body under the child", "the child's body");
    const path = await p.eval(() => {
      const d = document.querySelector("#mdoc .dpath");
      return d ? d.textContent : null;
    });
    assert(!/\[\[/.test(path || ""), `the path shows raw link syntax: ${JSON.stringify(path)}`);
    assert(/Straat/.test(path || ""), `the path lost the link text: ${JSON.stringify(path)}`);
    return [`the in-doc path reads ${JSON.stringify(path)}`];
  } },

// A DRAWER WEARS A CLICKABLE SIGN ON ITS SPINE: one press folds or expands it,
// no keyboard TAB and no point move.  A drawer starts shut, so its box reads
// `+' and its pairs are hidden; a click opens it -- the sign flips, the pairs
// stand -- and a second click shuts it again.  drv-plan carries a PROPERTIES
// drawer (its ORG_GLANCE_ID).
{ name: "a click on a drawer's spine sign folds and expands it",
  async run(p, base) {
    await sheet(p, base, "drv-plan");
    await settled(p, "the plan sheet");
    // A SHUT DRAWER IS ITS FRAME AND ORG'S `…'; OPEN, the frame stands with no
    // ellipsis.  The sign rides its spine: `+' shut, flipped open.
    const drawer = () => p.eval(() => {
      const de = document.querySelector("#mdoc .de.d-drawer");
      const fold = de ? de.querySelector(":scope > .fold") : null;
      return { has: !!de, sign: fold ? fold.textContent.trim() : null,
               folded: de ? /…/.test(de.textContent) : true };
    });
    const shut = await drawer();
    assert(shut.has, "drv-plan drew no properties drawer");
    assert(shut.sign === "+", `a shut drawer's sign is not '+': ${JSON.stringify(shut.sign)}`);
    assert(shut.folded, "a shut drawer does not show org's ellipsis");
    // ONE CLICK OPENS IT: the sign flips off `+' and the ellipsis goes.
    await p.click("#mdoc .de.d-drawer > .fold");
    await p.until(() => {
      const de = document.querySelector("#mdoc .de.d-drawer");
      return !!de && !/…/.test(de.textContent);
    }, "the click to expand the drawer");
    const open = await drawer();
    assert(open.sign && open.sign !== "+",
      `an open drawer's sign did not flip off '+': ${JSON.stringify(open.sign)}`);
    // A SECOND CLICK SHUTS IT AGAIN, back to `+' and the ellipsis.
    await p.click("#mdoc .de.d-drawer > .fold");
    await p.until(() => {
      const de = document.querySelector("#mdoc .de.d-drawer");
      return !!de && /…/.test(de.textContent);
    }, "the second click to fold the drawer");
    const reshut = await drawer();
    assert(reshut.sign === "+", `the refolded drawer's sign is not '+': ${JSON.stringify(reshut.sign)}`);
    return [`the spine sign folded (${JSON.stringify(shut.sign)}), expanded `
      + `(${JSON.stringify(open.sign)}), and folded again`];
  } },

// cb6db85.  THE BOX GREW AND STOOD OVER THE DOCUMENT.  Where the next line
// ENDS UP is unaskable in TestServe.hs: the node harness returns zeroed rects.
{ name: "the editor debug surface reports point, box, marker and the mirror",
  async run(p, base) {
    // The #1/#2 debug tooling.  `typeKeys' types through the REAL keydown path (a
    // human's, not `insertText'); `editorState' (window.__glance.editor) dumps what
    // the closures hide.  The field a doc-editing bug needs FIRST is `caret' --
    // point's offset in the box, which no rendered row shows.
    await sheet(p, base, "drv-roll");
    await walkToText(p, "a leaf with no children", "a list item");
    await p.press("+");                                    // draft a sibling item
    await p.until(() => !!document.querySelector("#dpara.on")
      && document.activeElement === document.getElementById("dtext"), "the draft, focused");
    await p.typeKeys("hi");                                // real keystrokes
    const st = await p.editorState();
    assert(st && typeof st.caret === "number",
      `editorState gave no caret offset: ${JSON.stringify(st)}`);
    assert((st.dtext || "").includes("hi"),
      `typeKeys did not reach the box through the real path: ${JSON.stringify(st.dtext)}`);
    assert(st.box === "dpara" && Array.isArray(st.rows),
      `editorState is missing fields: ${JSON.stringify(st)}`);
    assert(st.rows.some((r) => r.id === "D"),
      `the mirror carries no open-draft row: ${JSON.stringify(st.rows.map((r) => r.id))}`);
    await p.press("ESC");
    return [`caret ${st.caret}, box ${JSON.stringify(st.dtext)}, `
      + `marker ${JSON.stringify(st.drawnLead)}, ${st.rows.length} mirror rows`];
  } },

// A list under a NESTED headline must take a new item with its bullet, exactly
// like a list at the top level.  `outermost' crossed the child-headline boundary
// and `sibling' fell through to the empty-marker branch, so `+'/S-RET/M-RET drew
// an unindented, dash-less row (marks.org `C0:B2' under the `** A child').
{ name: "a list under a nested headline takes a new item with its marker",
  async run(p, base) {
    const pointId = () => p.eval(() => { const a = document.querySelector("#mdoc .de.dat"); return a ? a.dataset.id : null; });
    await sheet(p, base, "drv-marks");
    await settled(p, "the nested-headline sheet");
    let reached = (await pointId()) === "C0:B2";
    for (let i = 0; i < 40 && !reached; i += 1) {
      const was = await pointId();
      await p.press("f");
      await p.until((w) => {
        const a = document.querySelector("#mdoc .de.dat");
        return !!a && a.dataset.id !== w;
      }, "an f-step down the graph", 2000, was).catch(() => {});
      reached = (await pointId()) === "C0:B2";
    }
    assert(reached, "the walk never reached the nested list item C0:B2");
    await p.press("+");
    await p.until(() => !!document.querySelector("#dpara.on"), "the nested-list draft");
    const st = await p.editorState();
    assert((st.drawnLead || "").trimStart().startsWith("-"),
      `the nested list item drew no bullet: lead=${JSON.stringify(st.drawnLead)}`);
    await p.press("ESC");
    return [`nested list item drafts with marker ${JSON.stringify(st.drawnLead)}`];
  } },

// ===================================================================
// PAIRED ROOT/NESTED REGRESSION over `pairs.org' (`drv-pairs'): one entry
// carrying a top-level list (a plain item `B2', a `- [ ]' box `B3') and a child
// `** ' (`C0') whose body carries the SAME shape (plain item `C0:B2', box
// `C0:B3').  THE INVARIANT: body content under a child headline behaves EXACTLY
// as content at the root, within the child's own scope -- so every editor
// operation is run at a ROOT item and at the MATCHING NESTED one, and the two
// outcomes are asserted equivalent.  A child-boundary divergence (an owner-chain
// walker that climbs past the child, the class `Body.outermost' was fixed for)
// turns the paired case red rather than passing silently.  Each half reopens a
// fresh sheet, so a mutating op at the root cannot perturb the nested half.
{ name: "+ on a list item draws the same bullet at the root and under a child",
  async run(p, base) {
    // `+' drafts a sibling wearing the list's own bullet (`Body.sibling' ->
    // `outermost').  Under a child the owner chain runs item -> list -> CHILD;
    // the walk must stop at the list so the nested item drafts with the "- " a
    // root item does, not the empty marker the past-the-container branch gives.
    const leadAt = async (text) => {
      await sheet(p, base, "drv-pairs");
      await walkToText(p, text, `the item "${text}"`);
      await settled(p, "the mirror before +");
      await p.press("+");
      await p.until(() => !!document.querySelector("#dpara.on"), "the draft to open");
      return (await p.editorState()).drawnLead || "";
    };
    const root = await leadAt("a plain root item");
    const nested = await leadAt("a plain nested item");
    assert(root.trim() !== "" && root.trimStart().startsWith("-"),
      `the root item drew no bullet: ${JSON.stringify(root)}`);
    assert(nested === root,
      `nested drafted ${JSON.stringify(nested)}, root drafted ${JSON.stringify(root)}`);
    return [`+ drafts ${JSON.stringify(root)} at the root and the child alike`];
  } },

{ name: "RET edits a list item's text the same at the root and under a child",
  async run(p, base) {
    // Opening an item, appending to it and committing rewrites its own line
    // through the same splice (`Edit' -> `narrowed' -> `bodyText'), the item's
    // OWN extent (`ownEnd') the same whether it sits at the root or under a child.
    const editAt = async (text) => {
      await sheet(p, base, "drv-pairs");
      await walkToText(p, text, `the item "${text}"`);
      await settled(p, "the mirror before RET");
      await p.press("RET");
      await editUp(p, "the item's edit box");
      const now = await p.eval(() => document.getElementById("dtext").value);
      await p.eval((v) => { document.getElementById("dtext").value = v; }, now + " edited");
      await p.press("RET");
      await p.until((t) => [...document.querySelectorAll("#mdoc .d-item > .dp")]
          .some((l) => l.textContent.includes(t) && l.textContent.includes("edited")),
        `"${text} edited" to reach the pane`, 15000, text);
      return true;
    };
    await editAt("a plain root item");
    const nested = await editAt("a plain nested item");
    assert(nested, "the nested item never took the edit");
    return ["RET appends and commits equivalently at the root and under the child"];
  } },

{ name: "SPC ticks a checkbox item the same at the root and under a child",
  async run(p, base) {
    // A leaf box toggles through the same `Edit' the shell rewrites its marker
    // with (`toggleCheckbox').  Point on the box at the root or under a child:
    // the box flips to `[X]' and the door echoes `[X]' either way.
    const tickAt = async (text) => {
      await sheet(p, base, "drv-pairs");
      await walkToText(p, text, `the box "${text}"`);
      await settled(p, "the mirror before SPC");
      await p.press("SPC");
      await p.until((t) => {
        const box = [...document.querySelectorAll("#mdoc .d-item")].find((e) => {
          const l = e.querySelector(":scope > .dp");
          return l && l.textContent.includes(t);
        });
        const l = box && box.querySelector(":scope > .dp");
        const bx = l && l.querySelector(":scope > .dbx");
        return !!bx && bx.classList.contains("on");
      }, `the box "${text}" to flip on`, 12000, text);
      return p.eval(() => document.getElementById("echo").textContent);
    };
    const root = await tickAt("a root box the reader has not ticked");
    const nested = await tickAt("a nested box the reader has not ticked");
    assert(/\[X\]/.test(root), `the root tick echoed ${JSON.stringify(root)}`);
    assert(/\[X\]/.test(nested), `the nested tick echoed ${JSON.stringify(nested)}`);
    return [`SPC flips the box to [X] at the root and the child, echoing `
      + `${JSON.stringify(root)} / ${JSON.stringify(nested)}`];
  } },

{ name: "TAB on a list item folds nothing, at the root and under a child alike",
  async run(p, base) {
    // TAB climbs to the nearest foldable stop AT OR ABOVE point (`Doc.foldTarget').
    // A list and its items fold nothing, so at the root TAB on an item says so.
    // Under a child the owner chain runs item -> list -> CHILD, and the climb
    // must STOP at the child boundary -- else TAB on a nested item folds a whole
    // subtree a root item never could.  This is the `outermost' bug class in
    // `foldTarget': the paired echo goes red on the divergence.
    const foldAt = async (text) => {
      await sheet(p, base, "drv-pairs");
      await walkToText(p, text, `the item "${text}"`);
      await settled(p, "the mirror before TAB");
      const before = await p.eval(() => document.getElementById("echo").textContent);
      await p.press("TAB");
      await p.until((b) => document.getElementById("echo").textContent !== b,
        "TAB to answer", undefined, before);
      return p.eval(() => document.getElementById("echo").textContent);
    };
    const root = await foldAt("a plain root item");
    const nested = await foldAt("a plain nested item");
    assert(/nothing folds here/.test(root),
      `TAB on the root item said ${JSON.stringify(root)}`);
    assert(/nothing folds here/.test(nested),
      `TAB on the nested item reached past the child boundary: ${JSON.stringify(nested)}`);
    return [`TAB folds nothing on an item at the root and under the child alike`];
  } },

{ name: "d deletes a list item and lands on its box sibling, at root and child",
  async run(p, base) {
    // Deleting the plain item drops it and lands point on the next sibling in the
    // SAME list -- the checkbox item -- whether the list is at the root or under a
    // child (`applyDelete' -> `landingAfter', scoped by shared owner).
    const deleteAt = async (text) => {
      await sheet(p, base, "drv-pairs");
      await walkToText(p, text, `the item "${text}"`);
      await settled(p, "the mirror before the delete");
      await p.press("d");                                 // flag it
      await p.until(() => !!document.querySelector("#mdoc .de.dfl"), "the flag to land");
      await p.press("d");                                 // d again deletes
      await p.until((t) => ![...document.querySelectorAll("#mdoc .d-item > .dp")]
          .some((l) => l.textContent.includes(t)),
        `the item "${text}" to leave the pane`, 15000, text);
      return p.eval(() => {
        const at = document.querySelector("#mdoc .de.dat");
        const l = at && at.querySelector(":scope > .dp");
        return { id: at ? at.dataset.id : null,
                 text: l ? l.textContent : (at ? at.textContent : null) };
      });
    };
    const root = await deleteAt("a plain root item");
    const nested = await deleteAt("a plain nested item");
    assert(/box the reader has not ticked/.test(root.text || ""),
      `deleting the root item landed on ${JSON.stringify(root)}`);
    assert(/box the reader has not ticked/.test(nested.text || ""),
      `deleting the nested item landed on ${JSON.stringify(nested)}`);
    return [`delete lands on the box sibling at the root and under the child alike`];
  } },
// ===================================================================

{ name: "an open edit moves the line under it down, never covers it",
  async run(p, base) {
    await sheet(p, base, "drv-box");
    await walkTo(p, ".d-para", "the first paragraph");
    await p.press("RET");                                 // open it
    await boxPlaced(p, "the paragraph edit box to open over its block");
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
    await walkTo(p, ".d-para", "the first paragraph");
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
      // `--tv-*' lives on `.tv-root', and `g' reads `--g-*' off the document element.
      const tv = getComputedStyle(document.querySelector("#app .tv-root"));
      const tr = document.querySelector("#app tr.tv-flagged");
      const td = tr.querySelector("td");
      return { flag: rgb(tv.getPropertyValue("--tv-flag").trim()),
               bad: g("bad"),
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
      // THE INK IS THE ROW'S, spent on the spine its run bars with.
      // A FLAG TAKES THE BRANCH, since a delete takes the subtree.
      const under = fl.querySelector(".de");
      return { under: under ? ink(under) : null,
               thin: ink(fl),
               wide: getComputedStyle(fl, "::before").width,
               at: fl.classList.contains("dat"),
               sel: g("sel"),
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
    await walkTo(p, ".d-para", "the first paragraph");
    const seen = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const off = [...document.querySelectorAll("#mdoc .de")].find((n) => n !== at);
      const cs = getComputedStyle(at);
      return { sel: g("sel"),
               mark: ink(at),
               mk: mark(),
               fg: g("fg"),
               ink: g("point"),
               offMark: ink(off),
               ground: cs.backgroundColor,
               deco: cs.textDecorationLine, outline: cs.outlineStyle,
               border: cs.borderTopStyle };
    });
    // THE GROUND SAYS WHERE POINT IS; its connector wears the MARK ink, a
    // step short of the page's so the bar leads without shouting.
    assert(seen.mark === seen.mk && seen.mark !== seen.fg,
      `the cursor's mark paints ${seen.mark}, not the mark ink ${seen.mk}`);
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
    await walkTo(p, ".d-list", "the whole-list composite");
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
    await walkTo(p, ".d-para", "the first paragraph");
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
    await walkTo(p, ".d-para", "the first paragraph");
    const out = [];
    // A paragraph and the whole-list composite carry the title indent; leaves do not.
    const walk = ["", "n", "f", "n", "n", "n"];
    for (let i = 0; i < walk.length; i += 1) {
      if (walk[i]) await p.press(walk[i]);
      await p.press("RET");
      await editUp(p);
      await boxPlaced(p);
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

// THE ADDED SIGN, END TO END ON THE TABLE'S OWN FILTER.
// docs/bugs/fixed/2026-08-20-the-renderer-reads-the-added-sign-as-text.md: the
// renderer had no `+' in its grammar, so the sign was body text -- the strip
// drew `substring:+priority:[#B]', a narrowing where the reader wrote a
// widening, and completion behind the sign offered a dead literal alone.
// The last step is the STRIP's own rule (docs/query.md, "Adding"): committing a
// token whose opposite-signed twin already stands removes both, the pair being
// the tautology the grammar answers as every row.  TWINS ARE MATCHED ON WHAT A
// TOKEN MEANS, so the near miss is the quote that OPENS a token and makes it
// free text -- `-"priority:[#B]"' names no key and cancels nothing.
{ name: "the strip spells the added sign, completes behind it, and cancels a twin",
  async run(p, base) {
    await tableUp(p, base);
    // THE PAGE BOOTS ON ITS SAVED VIEW, so the strip already holds that query's
    // chips; every count here is read against what the boot left standing.
    const whole = await p.eval(appRows);
    const booted = (await p.eval(stripText)).length;
    assert(whole > 1, `the tree served ${whole} rows, too few to tell a filter from none`);

    // (1) THE CHIP IS THE TOKEN THE READER TYPED, sign and all, beside a plain one.
    await filterUp(p, "the filter box for the added token");
    await p.type("state:TODO +priority:[#B]");
    await committed(p, booted + 2, "both tokens to land as chips");
    const spelled = (await p.eval(stripText)).slice(booted);
    assert(spelled[0] === "state:TODO" && spelled[1] === "+priority:[#B]",
      `the strip respelled what was typed: ${JSON.stringify(spelled)}`);
    assert(!spelled.some((c) => c.indexOf("substring:") !== -1),
      `a keyed token reached the strip as free text: ${JSON.stringify(spelled)}`);
    await p.until((n) => document.querySelectorAll("#app .tv-table tbody tr").length < n,
                  "the added token to narrow its own axis", undefined, whole);

    // (2) COMPLETION READS BEHIND THE SIGN: `+sta' asks for a key, not a literal.
    await filterUp(p, "the filter box for the half-typed added token");
    await p.type("+sta");
    await p.until(acOpen, "the suggestion list to open behind the sign");
    const offers = await p.eval(acLabels);
    const at = offers.indexOf("state:");
    assert(at !== -1, `the key behind the sign was not offered: ${JSON.stringify(offers)}`);
    assert(!offers.some((o) => o.indexOf("+") !== -1),
      `the sign was offered as text to search for: ${JSON.stringify(offers)}`);
    for (let i = 0; i < at; i += 1) await p.press("<down>");
    await p.press("TAB");
    await p.until(() => {
      const box = document.querySelector("#app .tv-filter");
      return !!box && box.value === "+state:";
    }, "the accepted key to land with its sign still standing");

    // ESC twice: the first closes the list, the second drops what is half-typed.
    await p.press("ESC");
    await p.until(acShut, "the list to close on the first ESC");
    await p.press("ESC");
    await p.until(() => {
      const box = document.querySelector("#app .tv-filter");
      return !!box && box.value === "";
    }, "the half-typed token to go on the second ESC");

    // (3) THE NEAR MISS: a quote at the head makes it free text, which is no twin.
    await p.type('-"priority:[#B]"');
    await committed(p, booted + 3,
      "the free-text token to land beside the pair it does not cancel");
    const missed = (await p.eval(stripText)).slice(booted);
    assert(missed.indexOf("+priority:[#B]") !== -1,
      `a quoted free-text token cancelled a keyed one: ${JSON.stringify(missed)}`);

    // (4) THE PAIR CANCELS, and the rows the boot served come back.  A fresh
    // boot, so the twins are the only chips the reader put on the strip.
    await tableUp(p, base);
    await filterUp(p, "the filter box for the added token to stand alone");
    await p.type("+priority:[#B]");
    await committed(p, booted + 1, "the added token to stand as a chip of its own");
    await filterUp(p, "the filter box for its negated twin");
    await p.type("-priority:[#B]");
    await committed(p, booted, "the committed twin to take the standing chip with it");
    await p.until((n) => document.querySelectorAll("#app .tv-table tbody tr").length === n,
                  "the cancelled pair to serve every row again", undefined, whole);
    const left = (await p.eval(stripText)).length - booted;
    const back = await p.eval(appRows);

    // (5) BEHIND A `+', A CARRIED VALUE IS A DEAD OFFER: `A ∨ A' is `A', so a
    // standing `priority:[#A]' takes the A out of what `+priority:' offers and
    // leaves the letters that would widen the axis.  The fold is the column's
    // own, so BOTH SPELLINGS go -- `[#A]' and the bare letter alike.
    await tableUp(p, base);
    await filterUp(p, "the filter box for the priority to stand in");
    await p.type("priority:[#A]");
    await committed(p, booted + 1, "the standing priority to land as a chip");
    await filterUp(p, "the filter box for the added token's value stage");
    await p.type("+priority:");
    await p.until(acOpen, "the value list to open behind the sign");
    const widen = await p.eval(acLabels);
    const dead = widen.filter((v) => v === "[#A]" || v === "A" || v === "a");
    assert(dead.length === 0,
      `a carried value was offered back behind the sign: ${JSON.stringify(widen)}`);
    assert(widen.indexOf("[#B]") !== -1 && widen.indexOf("[#C]") !== -1,
      `the letters that would widen the axis went missing: ${JSON.stringify(widen)}`);

    // THE PLAIN STAGE NARROWS AND IS UNTOUCHED: it offers the whole domain,
    // the carried letter with it.
    await p.press("ESC");
    await p.until(acShut, "the value list to close");
    await p.press("ESC");
    await p.until(() => {
      const box = document.querySelector("#app .tv-filter");
      return !!box && box.value === "";
    }, "the half-typed added token to go");
    await p.type("priority:");
    await p.until(acOpen, "the plain value list to open");
    const plain = await p.eval(acLabels);
    assert(plain.indexOf("[#A]") !== -1,
      `an unsigned value stage lost the standing letter: ${JSON.stringify(plain)}`);

    return [`chips ${JSON.stringify(spelled)} · offered ${JSON.stringify(offers.slice(0, 3))}`
      + ` · near miss left ${missed.length} · pair cancelled to ${left},`
      + ` ${back}/${whole} rows over ${booted} booted chip(s)`
      + ` · behind \`+' ${JSON.stringify(widen)} against plain ${JSON.stringify(plain)}`];
  } },

// TWO DOORS ONTO ONE `?q=', docs/proposals/done/2026-08-20-slash-filters-dot-
// expression.md: `/' edits the FILTER half and `.' the whole expression.  What
// each door OFFERS, what the box KEEPS when it refuses a shaping token, and
// which chips rode a narrowing commit are unaskable in TestServe.hs -- the node
// harness stubs the renderer away, so the completion list and the box's own
// state after Enter are the real renderer's or nothing.  The refusal is read
// off the LOG, the one place a spoken notice outlives its second.
{ name: "`/' offers the filter half and refuses shaping, `.' composes the whole",
  async run(p, base) {
    await tableUp(p, base);
    const booted = (await p.eval(stripText)).length;

    // (1) THE WHOLE DOOR OFFERS THE SHAPING KEYS, and its commit shapes: the
    // order lands on the STRIP, which is where a token that was taken goes.
    await queryUp(p, "`.' to summon the box on the whole expression");
    await p.type("s");
    await p.until(acOpen, "the key list to open under `.'");
    const whole = await p.eval(acLabels);
    assert(whole.indexOf("sort:") !== -1,
      `the whole door offered no shaping key: ${JSON.stringify(whole)}`);
    // AND THE KEY OPENS THE STAGE BEHIND IT: `sort:' spelled out here lists the
    // orders, which is the completion `/' must not have.  Read as the key list
    // being REPLACED, so a stale list left standing would not pass for one.
    await p.type("ort:");
    await p.until((was) => {
      const now = [...document.querySelectorAll("#app .tv-ac-item .tv-ac-label")]
        .map((e) => e.textContent);
      return now.length > 0 && JSON.stringify(now) !== was;
    }, "the order list to open under `.'", undefined, JSON.stringify(whole));
    const sortStage = await p.eval(acLabels);
    await p.press("ESC");
    await p.until(acShut, "the list to close on the first ESC");
    await p.press("ESC");
    await p.until(() => {
      const box = document.querySelector("#app .tv-filter");
      return !!box && box.value === "";
    }, "the half-typed key to go on the second ESC");
    await p.type("sort:title");
    await committed(p, booted + 1, "the order to land as a chip");
    const shaped = (await p.eval(stripText)).slice(booted);
    assert(shaped.indexOf("sort:title") !== -1,
      `the order never reached the strip: ${JSON.stringify(shaped)}`);

    // (2) ONE QUERY UNDERNEATH: the address bar carries the WHOLE expression,
    // filters and shaping in one `q=', which is what both doors read and write.
    const shapedAt = await p.eval(() => ({
      q: new URLSearchParams(location.search).get("q") || "",
      chips: [...document.querySelectorAll("#app .tv-chip[data-i]")]
        .map((c) => c.firstChild.textContent).join(" "),
    }));
    assert(shapedAt.q === shapedAt.chips,
      `the applied query and the strip disagree: ${JSON.stringify(shapedAt)}`);

    // (3) THE FILTER DOOR OFFERS THE NARROWING KEYS ALONE, and the two lists
    // differ BY THE SHAPING KEYS -- read as one relation over the same prefix,
    // so a door that merely offered less would not pass.
    await filterUp(p, "the filter box on `/'");
    await p.type("s");
    await p.until(acOpen, "the key list to open under `/'");
    const narrow = await p.eval(acLabels);
    const gone = whole.filter((o) => narrow.indexOf(o) === -1);
    assert(gone.length > 0 && gone.every((o) => /^(sort|columns|view):/.test(o)),
      `the doors differ by something other than the shaping keys: `
      + `${JSON.stringify(gone)} (whole ${JSON.stringify(whole)}, `
      + `narrow ${JSON.stringify(narrow)})`);

    // AND THE STANDING ORDER RIDES A NARROWING COMMIT: the strip is not the box.
    await p.press("ESC");
    await p.until(acShut, "the list to close");
    await p.press("ESC");
    await p.until(() => {
      const box = document.querySelector("#app .tv-filter");
      return !!box && box.value === "";
    }, "the half-typed key to go");
    await p.type("state:TODO");
    await committed(p, booted + 2, "the narrowing token to land beside the order");
    const rode = (await p.eval(stripText)).slice(booted);
    assert(rode.indexOf("sort:title") !== -1,
      `narrowing dropped the standing order: ${JSON.stringify(rode)}`);

    // (4) A SHAPING TOKEN TYPED AT `/' IS REFUSED, SPOKEN AND LEFT STANDING:
    // never chipped, never in the query, and the box is not finished with it.
    await filterUp(p, "the filter box for the token it will refuse");
    // THE NARROWED DOOR COMPLETES NOTHING IT WILL REFUSE: the key list is up
    // over `sor', and the `t:' that spells a shaping key CLOSES it -- the stage
    // `.' just opened is not offered where the commit would refuse it.
    await p.type("sor");
    await p.until(acOpen, "the key list to open under `/'");
    await p.type("t:");
    await p.until(acShut, "the shaping key to close the list it was typed into");
    await p.type("scheduled");
    if (!(await p.eval(acShut))) {
      await p.press("ESC");
      await p.until(acShut, "the sort list to close, so RET commits");
    }
    await p.press("RET");
    await p.until(() => [...document.getElementById("log").children]
                    .some((n) => n.textContent.indexOf("autocomplete restricted") !== -1),
                  "the refusal to reach the log");
    const after = await p.eval(() => {
      const box = document.querySelector("#app .tv-filter");
      return {
        chips: [...document.querySelectorAll("#app .tv-chip[data-i]")]
          .map((c) => c.firstChild.textContent),
        box: box ? box.value : null,
        typing: !!box && document.activeElement === box,
        said: [...document.getElementById("log").children]
          .map((n) => n.textContent).filter((t) => t.indexOf("autocomplete restricted") !== -1),
        q: new URLSearchParams(location.search).get("q") || "",
      };
    });
    assert(after.chips.length === booted + 2,
      `a refused token reached the strip: ${JSON.stringify(after.chips)}`);
    assert(after.box.indexOf("sort:scheduled") !== -1,
      `the refusal was swallowed: the box holds ${JSON.stringify(after.box)}`);
    assert(after.typing, "the box gave the keyboard back over a token it refused");
    assert(after.said.length === 1,
      `the refusal was said ${after.said.length} times: ${JSON.stringify(after.said)}`);
    assert(after.said[0].indexOf("sort: autocomplete restricted") !== -1,
      `the refusal named no key of its own: ${JSON.stringify(after.said)}`);
    assert(after.q === shapedAt.q + " state:TODO",
      `a refused token reached the query: ${JSON.stringify(after.q)}`);

    return [`\`.' offered ${JSON.stringify(whole)}, \`/' ${JSON.stringify(narrow)}`
      + ` (the doors differ by ${JSON.stringify(gone)})`
      + ` · \`sort:' opened ${JSON.stringify(sortStage)} at \`.' and nothing at \`/'`
      + ` · ${JSON.stringify(rode)} over ${booted} booted chip(s)`
      + ` · refused "sort:scheduled" standing in the box, said`
      + ` ${JSON.stringify(after.said[0].slice(-52))}`];
  } },

// THE DAY WORD, END TO END THROUGH THE WIDGET.  `today' is one word spelled in
// two programs -- the renderer's grammar and the daemon's -- and only the real
// page can ask them together: the offer list is the renderer's, the rows on
// show are the daemon's answer to the very token that was chipped.  A word one
// half reads and the other does not shows here as an empty table under a chip
// that looks right, which no unit on either side can see.  `*today*' is the OLD
// spelling and rides along: read everywhere, offered nowhere.
{ name: "`today' completes, chips and narrows, and `*today*' answers the same rows",
  async run(p, base) {
    await tableUp(p, base);
    const booted = (await p.eval(stripText)).length;
    const all = await p.eval(appRows);

    // (1) THE OFFER IS THE BARE WORD, and the stars are proposed nowhere.
    await filterUp(p, "the filter box on `/'");
    await p.type("deadline:tod");
    await p.until(acOpen, "the value list to open behind a date key");
    const offered = await p.eval(acLabels);
    assert(offered.indexOf("today") !== -1,
      `the bare word was not offered: ${JSON.stringify(offered)}`);
    assert(offered.every((o) => o.indexOf("*") === -1),
      `a starred spelling was offered: ${JSON.stringify(offered)}`);

    // (2) THE WHOLE TOKEN COMMITS: a word at BOTH range ends, a shift on each.
    await p.type("ay-30d..today+30d");
    await committed(p, booted + 1, "the range to land as a chip");
    const chip = "deadline:today-30d..today+30d";
    const strip = (await p.eval(stripText)).slice(booted);
    assert(strip.indexOf(chip) !== -1,
      `the chip is not the word that was typed: ${JSON.stringify(strip)}`);
    const q = await p.eval(() => new URLSearchParams(location.search).get("q") || "");
    assert(q.indexOf(chip) !== -1,
      `the word never reached the query: ${JSON.stringify(q)}`);
    // THE ROWS ON SHOW ARE THE DAEMON'S ANSWER to that very query, so the two
    // grammars are compared against each other rather than against a count.
    const served = await p.eval(async (query) => {
      const r = await fetch(`/headlines?limit=20000&q=${encodeURIComponent(query)}`);
      const j = await r.json();
      return ((j.view && j.view.rows) || j.rows || []).map((x) => x.id);
    }, q);
    await p.until((want) => JSON.stringify(
        [...document.querySelectorAll("#app .tv-table tbody tr")].map((r) => r.dataset.id)
      ) === JSON.stringify(want),
      "the table to settle on the daemon's answer", undefined, served);
    assert(served.length > 0 && served.length < all,
      `the daemon narrowed to ${served.length} of ${all} rows under ${chip}`);

    // (3) THREE SPELLINGS, ONE ANSWER, each asked of the daemon by URL: the
    // bare word, the OLD starred one, and the bare shift that predates the
    // rename -- the independent oracle, since it never spelled the word at all.
    const idsUnder = async (q) => {
      await p.goto(`${base}/?q=${encodeURIComponent(q)}`);
      await p.until(() => !!document.querySelector("#app table tbody tr"),
                    `rows under ${q}`);
      return p.eval(() => [...document.querySelectorAll("#app .tv-table tbody tr")]
        .map((r) => r.dataset.id));
    };
    const bare = await idsUnder(chip);
    const starred = await idsUnder("deadline:*today*-30d..*today*+30d");
    const shifted = await idsUnder("deadline:-30d..+30d");
    assert(bare.length > 0, "the fortnight around today served no row at all");
    assert(JSON.stringify(bare) === JSON.stringify(starred),
      `the old spelling answers other rows: ${JSON.stringify([bare, starred])}`);
    assert(JSON.stringify(bare) === JSON.stringify(shifted),
      `the bare shift answers other rows: ${JSON.stringify([bare, shifted])}`);

    // (4) THE DAY RIDES IN THE TAG WHATEVER THE QUERY SPELLS: no reader tests
    // for a clock word, so the rename cannot leave a stale detector behind and
    // a store nothing touched across midnight revalidates rather than 304ing
    // yesterday's rows.
    const tag = await p.eval(async (q) => {
      const r = await fetch(`/headlines?limit=1&q=${encodeURIComponent(q)}`);
      const t = new Date(), pad = (n) => String(n).padStart(2, "0");
      return { etag: r.headers.get("etag") || "",
               day: `${t.getFullYear()}-${pad(t.getMonth() + 1)}-${pad(t.getDate())}` };
    }, chip);
    assert(tag.etag.indexOf(`-d${tag.day}`) !== -1,
      `the tag does not carry the day: ${JSON.stringify(tag)}`);

    return [`\`${chip}' chipped and served ${served.length} of ${all} row(s)`
      + ` · the starred and bare-shift spellings served ${JSON.stringify(bare)} too`
      + ` · offered ${JSON.stringify(offered)} behind \`deadline:tod'`
      + ` · tag ${JSON.stringify(tag.etag)}`];
  } },

// THE BOX DOCKS ON THE STRIP (`filterDock: "strip"'): `/' summons it onto the
// chip strip's OWN ROW, the table under it keeping its full height and its hint
// line, and the veil the centred palette drew is gone.  WHERE a box lands, and
// whether the list it opens hangs off the bottom of the window, are unaskable
// in TestServe.hs -- the node harness zeroes every rect.
{ name: "`/' docks the box on the chip strip, over a table with no veil on it",
  async run(p, base) {
    await tableUp(p, base);
    const chips = (await p.eval(stripText)).length;
    await filterUp(p, "the docked box to take the focus");

    const docked = await p.eval(() => {
      const box = document.querySelector("#app .tv-filter");
      const strip = document.querySelector("#app .tv-chips");
      const veil = document.querySelector("#app .tv-veil");
      const hint = document.querySelector("#app .tv-hint");
      const mid = (r) => r.top + r.height / 2;
      const b = box.getBoundingClientRect(), c = strip.getBoundingClientRect();
      return { gap: Math.round(Math.abs(mid(b) - mid(c))),
               apart: Math.round(b.left - c.right),
               wide: Math.round(b.width),
               // The strip carries the pin badge whatever the query is, so it
               // is drawn to measure against even with no chip standing.
               tall: Math.round(c.height),
               veiled: !!veil && getComputedStyle(veil).display !== "none",
               drawn: !!hint && getComputedStyle(hint).display !== "none",
               // The order is on the line whatever the count is; the pager
               // steps join it only once the set runs past one page.
               says: hint ? hint.textContent.trim() : "",
               pager: document.querySelectorAll("#app .tv-hint .tv-pg").length,
               rows: document.querySelectorAll("#app .tv-table tbody tr").length };
    });
    assert(docked.tall > 0, "the chip strip is not drawn, so its row reads as nothing");
    assert(docked.gap <= 3,
      `the box sits on its own line, ${docked.gap}px off the chips' middle`);
    assert(docked.apart >= 0, "the docked box overlaps the chips");
    assert(docked.wide > 0, "the docked box was summoned with no width to type in");
    assert(!docked.veiled, "the docked box veiled the table it sits on");
    assert(docked.drawn, "the docked box took the hint line with it");
    assert(/(unsorted|sort )/.test(docked.says) && /(rows|of )/.test(docked.says),
      `the hint line stopped saying what is on show: ${JSON.stringify(docked.says)}`);
    assert(docked.rows > 0, "the docked box left no rows drawn under it");

    // AND THE LIST IT OPENS IS READ WHOLE: docked low on the chrome, a list
    // hanging BELOW the box is the one that can fall out of the window.
    await p.type("s");
    await p.until(acOpen, "the suggestion list to open over the docked box");
    const list = await p.eval(() => {
      const r = document.querySelector("#app .tv-ac").getBoundingClientRect();
      return { top: Math.round(r.top), height: Math.round(r.height),
               under: Math.round(window.innerHeight - r.bottom) };
    });
    assert(list.height > 0, "the list opened with nothing drawn in it");
    assert(list.top >= 0, `the list starts ${-list.top}px above the window`);
    assert(list.under >= 0,
      `the list runs ${-list.under}px past the bottom of the window`);

    return [`the box ${docked.gap}px off the strip's middle, ${docked.apart}px`
      + ` clear of ${chips} chip(s), ${docked.wide}px wide · no veil over`
      + ` ${docked.rows} rows · hint line ${JSON.stringify(docked.says.slice(0, 40))}`
      + ` with ${docked.pager} pager step(s)`
      + ` · list ${list.height}px with ${list.under}px of window under it`];
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
// The tree carries ONE tag layer (the capture fixture's) and no `system.org', so
// the SYSTEM layer is still the one the write brings into being — and the
// namespace is PICKED rather than defaulted, the form opening on the first tag
// layer it finds when the filter names none.
{ name: "+ writes a state into a tree whose system layer is not there yet",
  async run(p, base) {
    await tableUp(p, base);
    const row = await p.eval(() => {
      const tr = document.querySelector("#app tr.tv-sel");
      return { id: tr.getAttribute("data-id"), text: tr.textContent.trim() };
    });
    await mintForm(p);
    const spaces = await p.eval(() => {
      const pick = document.getElementById("nspace");
      // THE ROWS ON SCREEN ARE THE ROWS THE FILTER CHOSE and there is no filter,
      // so the tree's own tag layers are what it offers; this row wears none of
      // them, and the state it is owed belongs to the tree.
      pick.value = "system";
      return [...pick.children].map((o) => o.textContent);
    });
    assert(spaces[0] === "system" && spaces.indexOf("tag:book") !== -1,
      `the namespaces read ${JSON.stringify(spaces)}`);
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
    return [`row ${row.id} wears HANDED, off a system layer that did not exist `
      + `when the page loaded, picked out of ${JSON.stringify(spaces)}`];
  } },

// A NESTED LIST ITEM IS DRAWN INSIDE ITS PARENT, so its element stands taller than
// its own line.  `f' goes finer: the list itself is ONE stop at the coarse grain.
{ name: "the cursor on a list item lights itself, its subtree and the way back",
  async run(p, base) {
    await intoNestedItem(p, base, "drv-wide");
    const seen = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const kid = at.querySelector(":scope > .de");
      // NEITHER POINT, NOR ITS SUBTREE, NOR AN OWNER, NOR A SIBLING: a sibling is the
      // choice the reader is standing in and stays readable, so it is not "outside".
      // OUTSIDE THE BRANCH THE READER IS CHOOSING IN.  With point at the list's own
      // level every item is point, a sibling, or inside one, so what is left outside
      // is the prose around the list.
      const other = [...document.querySelectorAll("#mdoc .de.d-para.lvl-top")]
        .find((n) => !n.classList.contains("d-head"));
      return { point: g("point"),
               fg: g("fg"),
               off: g("point-off"),
               sel: g("sel"),
               atInk: ink(at),
               mk: mark(),
               kidInk: ink(kid),
               otherInk: other
                 ? ink(other) : null,
               tall: Math.round(parseFloat(getComputedStyle(at, "::before").height)),
               rowH: Math.round(at.getBoundingClientRect().height) };
    });
    // WHAT POINT CARRIES TAKES THE PAGE'S OWN INK; everything off the path drops to
    // the ink nobody is looking at.  The ground itself is the case named "the cursor
    // is a ground over its own line".
    assert(seen.kidInk === seen.mk,
      `a row under point paints ${seen.kidInk}, not the mark ink ${seen.mk}`);
    assert(seen.otherInk === seen.off,
      `an item outside point's subtree paints ${seen.otherInk}, not ${seen.off}`);
    // THE SPINE BARS THE ROW'S WHOLE EXTENT: own line and subtree together,
    // the run's unbroken column.
    assert(Math.abs(seen.tall - seen.rowH) < 1,
      `the spine is ${seen.tall}px against the row's ${seen.rowH}px extent`);
    // THE LIST GROUNDS THE ROWS IT OPENS: a composite has no connector of its own,
    // and a connector standing on that ground takes the page's ink to read over it.
    await stepped(p, "b", ".d-comp", "the cursor to go back out to the list itself");
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
    assert(whole.rootInk === seen.mk,
      `a root on the list's ground paints ${whole.rootInk}, not the mark's ${seen.mk}`);
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
    await walkTo(p, ".d-list", "the list");
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
    await editUp(p);
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
    await walkTo(p, ".d-list", "the list");
    await p.press("f");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && !!at.querySelector(":scope > .dp > .dm") && !!at.querySelector(".de");
    }, "the cursor to land on an item that has one drawn inside it");
    const seen = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      // THE INK IS READ OFF THE GLYPH, the bullet's own span where org wrote a
      // steppable bullet, since that is what the reader sees.
      const glyph = (n) => n && (n.querySelector(".dbul") || n);
      const own = at.querySelector(":scope > .dp > .dm");
      // `:scope': a descendant combinator may match through an ancestor outside `at'.
      const kidRow = at.querySelector(":scope > .de");
      const kid = glyph(kidRow.querySelector(":scope > .dp > .dm"));
      const plainRow = [...document.querySelectorAll("#mdoc .de:not(.dat)")]
        .find((n) => !at.contains(n) && n.querySelector(":scope > .dp > .dm"));
      return { sel: g("sel"),
               bg: g("bg"),
               fg: g("fg"),
               point: g("point"),
               // THE GROUND THE TABLE'S CURSOR WEARS, and the tree in its own column.
               ground: getComputedStyle(at).backgroundColor,
               rail: ink(at),
               mk: mark(),
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
    assert(seen.rail === seen.mk,
      `the cursor's connector paints ${seen.rail}, not the mark ink ${seen.mk}`);
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
    await stepped(p, "b", ".d-comp", "the cursor to go back out to the list");
    await stepped(p, "B", ".d-head", "the cursor to climb back to the headline");
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
      const crumbs = [...document.querySelectorAll("#mdoc .dpath .dcr")];
      const at = document.querySelector("#mdoc .de.dat");
      const own = at.querySelector(":scope > .dp");
      const sep = document.querySelector("#mdoc .dpath .dsep");
      return { words: crumbs.map((c) => c.textContent),
               last: getComputedStyle(crumbs[crumbs.length - 1]).color,
               point: g("point"),
               said: own ? own.textContent.trim() : "",
               sep: sep ? sep.textContent.trim() : "",
               sticky: getComputedStyle(document.querySelector("#mdoc .dpath")).position };
    });
    // THE PATH READS `A › B', org-breadcrumb fashion, and stays sticky.
    assert(seen.sep === "›",
      `the strip separator is "${seen.sep}", not the angle glyph`);
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
    return [`${seen.words.join(" › ")} — the last in ${seen.last}`];
  } },
{ name: "the pane dims every branch but the one the reader is in",
  async run(p, base) {
    // AT REST THE DOCUMENT IS FULL INK: dimming answers "which branch am I in", so
    // it waits until there is a branch to be in.
    await sheet(p, base, "drv-wide");
    const inks = () => {
      const rows = [...document.querySelectorAll("#mdoc .de")];
      const head = document.querySelector("#mdoc .de.d-head");
      // A SIBLING IS THE CHOICE THE READER IS STANDING IN, and its own branch comes
      // with it: a branch whose contents are dimmed cannot be weighed.
      const at = document.querySelector("#mdoc .de.dat");
      const sib = at && [...at.parentElement.children]
        .find((n) => n !== at && n.classList.contains("de"));
      const inSib = sib && sib.querySelector(".de");
      // A LINK ON A DIMMED LINE: `.dl' carries its own ink and outranks what it
      // inherits, so it is the one part that can stay lit while its line goes.
      const dimmed = (n) => getComputedStyle(n.closest(".de")).color === g("point-off");
      const box = [...document.querySelectorAll("#mdoc .dbx.on")].find(dimmed);
      const link = [...document.querySelectorAll("#mdoc .de .dl")].find(dimmed);
      return { fg: g("fg"),
               off: g("point-off"),
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
    await walkTo(p, ".d-list", "the list");
    await stepped(p, "f", ".d-item", "`f' to reach the list's first item");
    // Onto the item that carries a box, then open it and split the line.
    await walkTo(p, ".d-item:has(> .dp > .dbx)", "the item that carries a box");
    await p.press("RET");
    await editUp(p, "the edit to open over it");
    await boxPlaced(p);
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

{ name: "X hides done checkboxes and the branches all of whose boxes are done",
  async run(p, base) {
    // `drv-hide' carries two runs: the first mixes open and done leaves with two
    // interim items -- one still open, one fully done -- and the second is a done
    // leaf beside an open one, so scope has two lists to tell apart.
    //
    // ONE READING keyed on each item's OWN line: `marked' is the class the mode
    // stamps, `visible' the client rects an ancestor's `display:none' zeroes too.
    const read = () => [...document.querySelectorAll("#mdoc .d-item")].map((e) => {
      const line = e.querySelector(":scope > .dp");
      return { text: (line || e).textContent.trim().slice(0, 30),
               marked: e.classList.contains("d-hidden"),
               visible: e.getClientRects().length > 0 };
    });
    const of = (rows, t) => rows.find((r) => r.text.includes(t)) || {};
    const someHidden = () =>
      [...document.querySelectorAll("#mdoc .d-item")]
        .some((e) => e.classList.contains("d-hidden"));
    const dat = () => {
      const at = document.querySelector("#mdoc .de.dat");
      const line = at && at.querySelector(":scope > .dp");
      return { text: (line || at || {}).textContent
                 ? (line || at).textContent.trim().slice(0, 30) : "",
               hidden: !!at && at.classList.contains("d-hidden"),
               visible: !!at && at.getClientRects().length > 0 };
    };
    // THE RUN CONTAINERS, by the words inside them: `marked'/`visible' say
    // whether a wholly-done run's own box vanished with its items.
    const lists = () => [...document.querySelectorAll("#mdoc .d-comp.d-list")]
      .map((e) => ({ text: e.textContent.trim().slice(0, 40),
                    marked: e.classList.contains("d-hidden"),
                    compacted: e.classList.contains("d-compacted"),
                    // The dashed spine the compacted run wears is ONE bar on the
                    // composite itself, read off its own `::before'.
                    dashed: getComputedStyle(e, "::before").backgroundImage,
                    visible: e.getClientRects().length > 0 }));
    const figures = [];

    // --- OFF: every row stands ---------------------------------------------
    await sheet(p, base, "drv-hide");
    const before = await p.eval(read);
    assert(before.length === 12, `the fixture drew ${before.length} items, not twelve`);
    assert(before.every((r) => r.visible && !r.marked),
      `a row was hidden before the mode: ${JSON.stringify(before)}`);
    assert((await p.eval(lists)).every((r) => r.visible && !r.marked && !r.compacted),
      "a run container was hidden or compacted before the mode");
    // --- MASTER TOGGLE from a row off every list ---------------------------
    await walkToText(p, "paragraph before", "the paragraph before the first list");
    await settled(p, "the mirror before the master toggle");
    await p.press("X");
    await p.until(someHidden, "the master toggle to hide the done rows");
    const all = await p.eval(read);
    // The done leaves and the fully-done branches go, across all three runs; the
    // open leaves and the interim over an open box stay.
    for (const t of ["beta", "delta", "delta one", "delta two", "gamma one",
                     "epsilon", "iota", "kappa"])
      assert(of(all, t).marked && !of(all, t).visible,
        `${t} was not hidden by the master toggle: ${JSON.stringify(of(all, t))}`);
    for (const t of ["alpha", "gamma,", "gamma two", "zeta"])
      assert(!of(all, t).marked && of(all, t).visible,
        `${t} was hidden but must stand: ${JSON.stringify(of(all, t))}`);
    // THE WHOLLY-DONE RUN VANISHES CONTAINER AND ALL: its `.d-list' box is gone,
    // never present-but-empty; the partly-done runs keep theirs.
    const onLists = await p.eval(lists);
    const runOf = (rs, t) => rs.find((r) => r.text.includes(t)) || {};
    assert(runOf(onLists, "iota").marked && !runOf(onLists, "iota").visible,
      `the wholly-done run's container stood: ${JSON.stringify(runOf(onLists, "iota"))}`);
    for (const t of ["alpha", "epsilon"])
      assert(!runOf(onLists, t).marked && runOf(onLists, t).visible,
        `a partly-done run's container vanished: ${JSON.stringify(runOf(onLists, t))}`);
    // THE PARTIAL RUNS WEAR THE DASHED SPINE; the vanished one wears none.
    for (const t of ["alpha", "epsilon"]) {
      assert(runOf(onLists, t).compacted,
        `a partly-done run lost the dashed-spine class: ${JSON.stringify(runOf(onLists, t))}`);
      assert(runOf(onLists, t).dashed.includes("repeating-linear-gradient"),
        `a compacted run drew no dashed spine: ${runOf(onLists, t).dashed}`);
    }
    assert(!runOf(onLists, "iota").compacted,
      "the wholly-done run wore the compacted spine instead of vanishing");
    assert((await p.eval(dat)).visible,
      "point was left on a hidden row after the master toggle");
    // THE PARAGRAPHS KEEP THEIR BAR: a paragraph borrows its headline block's
    // one spine, so compacting a list in that block must not erase it -- the
    // compacted run occludes the block spine on its OWN rows, never the block's.
    const blkSpine = await p.eval(() => {
      const b = document.querySelector("#mdoc .blk");
      return b ? getComputedStyle(b, "::before").backgroundColor : "none";
    });
    assert(blkSpine !== "none" && !/,\s*0\)\s*$/.test(blkSpine)
             && blkSpine !== "transparent",
      `the block spine went transparent under a compacted list: ${blkSpine}`);
    figures.push(`master on: ${all.filter((r) => r.marked).length} of 12 hidden, `
      + `two dashed spines, the wholly-done container gone`);

    // Again, off every list, shows them all back.
    await p.press("X");
    await p.until(() => ![...document.querySelectorAll("#mdoc .d-item")]
                    .some((e) => e.classList.contains("d-hidden")),
                  "the second master press to restore every row");
    const restored = await p.eval(read);
    assert(restored.every((r) => r.visible && !r.marked),
      `a row stayed hidden after the toggle off: ${JSON.stringify(restored)}`);
    assert((await p.eval(lists)).every((r) => r.visible && !r.marked && !r.compacted),
      "a run stayed hidden or dashed after the toggle off");

    // --- SCOPE: point in the first list hides that run alone ----------------
    await walkToText(p, "beta", "the done leaf in the first list");
    await settled(p, "the mirror before the scoped toggle");
    await p.press("X");
    await p.until(someHidden, "the scoped toggle to hide the first list's done rows");
    const scoped = await p.eval(read);
    assert(of(scoped, "beta").marked && of(scoped, "delta").marked,
      "the scoped toggle missed the first list's done rows");
    // The SECOND list is untouched: `epsilon' is done but was not in scope.
    assert(!of(scoped, "epsilon").marked && of(scoped, "epsilon").visible,
      `the scoped toggle reached the second list: ${JSON.stringify(of(scoped, "epsilon"))}`);
    // ONLY THE SCOPED RUN GOES DASHED: the first list's container is compacted,
    // the second's rests.
    const scopedLists = await p.eval(lists);
    assert(runOf(scopedLists, "alpha").compacted,
      "the scoped run wore no dashed spine");
    assert(!runOf(scopedLists, "epsilon").compacted,
      "a run outside the scope went dashed");
    // POINT SAFETY: `X' hid the beta row point stood on, so point stepped off it.
    const snapped = await p.eval(dat);
    assert(snapped.visible && !snapped.hidden && !snapped.text.includes("beta"),
      `point stayed on the hidden beta row: ${JSON.stringify(snapped)}`);
    figures.push(`scoped: point snapped to "${snapped.text}"`);

    // --- `f' DESCENDS PAST A HIDDEN HEAD -----------------------------------
    // A run whose first child is done reads as one with fewer rows: grain-finer
    // into gamma steps over the hidden gamma-one onto the open gamma-two, never
    // resting point on a row the mode took out.
    await sheet(p, base, "drv-hide");
    await walkToText(p, "gamma,", "the interim gamma");
    await settled(p, "the mirror before arming the mode");
    await p.press("X");
    await p.until(someHidden, "the scoped mode to hide the done gamma-one");
    await p.press("f");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const l = at && at.querySelector(":scope > .dp");
      return !!at && at.dataset.id === docAtNow() && !!l
        && l.textContent.includes("gamma two");
    }, "`f' to descend past the hidden gamma-one onto gamma-two");
    const dived = await p.eval(dat);
    assert(dived.visible && !dived.hidden && dived.text.includes("gamma two"),
      `f landed on the hidden gamma-one: ${JSON.stringify(dived)}`);
    figures.push(`f descends past the hidden head onto "${dived.text}"`);

    // --- n/p STEP PAST THE HIDDEN SET --------------------------------------
    // A fresh sheet, point armed ON the open alpha above the hidden beta, so the
    // step has a deterministic start.
    await sheet(p, base, "drv-hide");
    await walkToText(p, "alpha", "the open leaf above the hidden beta");
    await settled(p, "the mirror before arming the mode");
    await p.press("X");
    await p.until(someHidden, "the scoped mode over alpha's own run");
    const onAlpha = await p.eval(dat);
    assert(onAlpha.text.includes("alpha"),
      `arming the mode moved point off alpha: ${JSON.stringify(onAlpha)}`);
    // `n' lands on gamma, the hidden beta between them stepped over.
    await p.press("n");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const l = at && at.querySelector(":scope > .dp");
      return !!at && at.dataset.id === docAtNow() && !!l
        && l.textContent.includes("gamma,");
    }, "`n' to skip the hidden beta and land on gamma");
    const afterN = await p.eval(dat);
    assert(!afterN.text.includes("beta"),
      `n landed on the hidden beta: ${JSON.stringify(afterN)}`);
    // `p' walks back over the same hidden beta to alpha.
    await p.press("p");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const l = at && at.querySelector(":scope > .dp");
      return !!at && at.dataset.id === docAtNow() && !!l
        && l.textContent.includes("alpha");
    }, "`p' to skip the hidden beta on the way back to alpha");
    figures.push(`n/p step over the hidden beta, alpha↔"${afterN.text}"`);

    // --- REACTIVE: ticking the last box hides its branch live ---------------
    // A fresh sheet drops the ephemeral mode; walk to the open box first, then
    // turn the run's mode on while it still stands, then tick it.
    await sheet(p, base, "drv-hide");
    await walkToText(p, "gamma two", "the open box under gamma");
    await settled(p, "the mirror before arming the mode");
    await p.press("X");
    await p.until(someHidden, "the mode on the first list to take");
    assert(!of(await p.eval(read), "gamma,").marked,
      "the interim gamma hid while a box under it was still open");
    await p.press("SPC");
    // The tick writes the box, the file round-trips, and the fill re-hides: the
    // interim gamma now has every leaf done.
    await p.until(() => {
      const g = [...document.querySelectorAll("#mdoc .d-item")].find((e) => {
        const l = e.querySelector(":scope > .dp");
        return l && l.textContent.includes("gamma,");
      });
      return !!g && g.classList.contains("d-hidden");
    }, "ticking gamma's last box to hide the branch live", 12_000);
    const live = await p.eval(dat);
    assert(live.visible && !live.hidden,
      `point was left on the branch the tick hid: ${JSON.stringify(live)}`);
    figures.push(`live tick hid gamma; point rests on "${live.text}"`);

    return figures;
  } },

{ name: "a checkbox with children shows a face rolled up from them, and is read-only",
  async run(p, base) {
    // `drv-roll' has two parents written `[ ]' -- one with a done child and an
    // open one, one with two done -- and a childless leaf, so the DERIVED face is
    // visibly not the literal box.
    await sheet(p, base, "drv-roll");
    const boxes = () => [...document.querySelectorAll("#mdoc .d-item")].map((e) => {
      const line = e.querySelector(":scope > .dp");
      const bx = line && line.querySelector(":scope > .dbx");
      return { text: (line || e).textContent.trim().slice(0, 40),
               glyph: bx ? bx.textContent.replace(/\s+/g, " ").trim() : null,
               on: !!bx && bx.classList.contains("on"),
               part: !!bx && bx.classList.contains("part"),
               derived: !!bx && bx.classList.contains("derived") };
    });
    const of = (rs, t) => rs.find((r) => r.text.includes(t)) || {};
    await p.until(() => document.querySelectorAll("#mdoc .d-item .dbx").length >= 5,
                  "the pane to draw the rolled boxes");
    const seen = await p.eval(boxes);

    // SOME DONE, SOME NOT -> `[-]', partial and derived, though written `[ ]'.
    const mix = of(seen, "some done, some not");
    assert(mix.derived && mix.part && !mix.on && (mix.glyph || "").includes("-"),
      `a part-done parent did not roll to a dash: ${JSON.stringify(mix)}`);

    // EVERY CHILD DONE -> `[X]', on and derived, though written `[ ]'.
    const full = of(seen, "every child done");
    assert(full.derived && full.on && !full.part && (full.glyph || "").includes("X"),
      `an all-done parent did not roll to a tick: ${JSON.stringify(full)}`);

    // A LEAF WEARS ITS OWN BOX, never derived.
    const leaf = of(seen, "a leaf with no children");
    assert(!leaf.derived && !leaf.on && !leaf.part && (leaf.glyph || "").includes("[ ]"),
      `a leaf's own box was rewritten: ${JSON.stringify(leaf)}`);

    // READ-ONLY: SPC on a derived box is refused, its state its children's to tell.
    await walkToText(p, "some done, some not", "the part-done parent");
    await settled(p, "the mirror before the toggle");
    await p.press("SPC");
    await p.until(() => /derived from children/.test(
                    document.getElementById("echo").textContent),
                  "the derived box to refuse a hand tick");
    const after = of(await p.eval(boxes), "some done, some not");
    assert(after.part && (after.glyph || "").includes("-"),
      `a derived box changed under a hand tick: ${JSON.stringify(after)}`);

    // STATISTICS COOKIES read their count off the same children: `[/]' fills to
    // `[done/total]', `[%]' to the whole percent, and a full one wears the done
    // face -- the literal `[/]'/`[%]' stays in the file, the count collapses over.
    const cookies = () => [...document.querySelectorAll("#mdoc .d-item")].map((e) => {
      const line = e.querySelector(":scope > .dp");
      const ck = line && line.querySelector(":scope > .dcookie");
      return { text: (line || e).textContent.trim().slice(0, 48),
               cookie: ck ? ck.textContent.trim() : null,
               done: !!ck && ck.classList.contains("done") };
    });
    const cks = await p.eval(cookies);
    const frac = of(cks, "a fraction cookie");
    assert(frac.cookie === "[1/2]" && !frac.done,
      `[/] did not fill to [1/2]: ${JSON.stringify(frac)}`);
    const pct = of(cks, "a percent cookie");
    assert(pct.cookie === "[100%]" && pct.done,
      `[%] did not fill to a done [100%]: ${JSON.stringify(pct)}`);

    return [`written [ ] rolls: mix -> "${mix.glyph}", full -> "${full.glyph}", `
      + `leaf literal "${leaf.glyph}"; SPC on a derived box refused; `
      + `cookies [/] -> "${frac.cookie}", [%] -> "${pct.cookie}"`];
  } },

{ name: "f walks the graph depth-first, rolling to the next node where nothing is finer",
  async run(p, base) {
    // `drv-roll' nests: the first parent holds two leaf children, then a second
    // parent follows.  Held `f' descends into the first child, and at that leaf --
    // nothing finer -- falls through to `n': on to the sibling, then climbing to
    // the next parent.  A pre-order walk of the whole tree from one key.
    await sheet(p, base, "drv-roll");
    const datText = () => {
      const e = document.querySelector("#mdoc .de.dat");
      const l = e && e.querySelector(":scope > .dp");
      return { id: e ? e.dataset.id : null,
               text: (l || e || {}).textContent ? (l || e).textContent.trim().slice(0, 40) : "" };
    };
    await walkToText(p, "some done, some not", "the first parent");
    await settled(p, "the mirror before the walk");
    // The three stops a held `f' makes, in order: descend, advance at the leaf,
    // climb to the next subtree.
    const want = ["a child that is done", "a child still open", "every child done"];
    const seen = [];
    for (const expect of want) {
      await p.press("f");
      await p.until((ex) => {
        const e = document.querySelector("#mdoc .de.dat");
        const l = e && e.querySelector(":scope > .dp");
        return !!e && e.dataset.id === docAtNow() && !!l && l.textContent.includes(ex);
      }, `f to reach "${expect}"`, undefined, expect);
      seen.push((await p.eval(datText)).text);
    }
    assert(seen[0].includes("a child that is done"),
      `f did not descend into the first child: ${JSON.stringify(seen)}`);
    assert(seen[1].includes("a child still open"),
      `f did not fall through to n at the leaf: ${JSON.stringify(seen)}`);
    assert(seen[2].includes("every child done"),
      `f did not climb to the next subtree: ${JSON.stringify(seen)}`);
    // `b' IS `f' REVERSED: from where the walk stopped, held `b' retraces the
    // same three stops back to the first parent.
    const back = ["a child still open", "a child that is done", "some done, some not"];
    const rseen = [];
    for (const expect of back) {
      await p.press("b");
      await p.until((ex) => {
        const e = document.querySelector("#mdoc .de.dat");
        const l = e && e.querySelector(":scope > .dp");
        return !!e && e.dataset.id === docAtNow() && !!l && l.textContent.includes(ex);
      }, `b to reach "${expect}"`, undefined, expect);
      rseen.push((await p.eval(datText)).text);
    }
    assert(rseen[2].includes("some done, some not"),
      `b did not retrace f back to the parent: ${JSON.stringify(rseen)}`);
    return [`f DFS: parent -> ${seen.map((s) => JSON.stringify(s.slice(0, 16))).join(" -> ")}`
      + `; b retraces to parent`];
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
      const ch = cell();
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
      const point = g("point");
      return { // THE FOLDED LINE IS THE FRAME ALONE: the spine's `.fold' sign is
               // chrome beside it, so the reading takes the `.dg' frame text.
               folded: [...drawer.querySelectorAll(".dg")].map((g) => g.textContent).join(""),
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
    // THE LINE ALWAYS DRAWS BOTH SETTABLE SLOTS: an unset SCHEDULED stands
    // beside drv-marks's own DEADLINE value, still one `.d-meta' row.
    assert(shut.planning.length === 1 && /SCHEDULED:/.test(shut.planning[0])
             && /DEADLINE: </.test(shut.planning[0]),
      `the planning line reads ${JSON.stringify(shut.planning)}`);
    await walkTo(p, ".d-drawer", "the drawer");
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
    // A PAIR IS NOT NESTED: no spine, a paragraph's own thin bar -- which spends
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
    // A CANCELLED DRAFT LEAVES THE COUNTS: the row `+' draws is a ROW and no
    // pair, so a drawer with no draft up holds exactly the pairs it held.
    await p.press("+");
    await pairUp(p, "the pair's fields over a drawn row");
    await p.press("ESC");
    await p.until(() => document.querySelectorAll("#mdoc .d-draft").length === 0,
                  "ESC to take the drawn row back");
    const back = await read();
    assert(JSON.stringify(back.pairs) === JSON.stringify(open.pairs),
      `the cancelled drawer holds ${JSON.stringify(back.pairs)} `
      + `against ${JSON.stringify(open.pairs)}`);
    await p.press("TAB");
    await p.until(() => document.querySelectorAll("#mdoc .d-drawer .d-meta").length === 0,
                  "TAB to fold the drawer");
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
    await walkTo(p, ".d-drawer", "the drawer");
    await p.press("f");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && at.classList.contains("d-meta") && /OWNER/.test(at.textContent);
    }, "point on the OWNER pair");
    await settled(p);
    // RET EDITS THE LINE AS ORG SPELLS IT.
    await p.press("RET");
    await editUp(p, "the edit");
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
    // `+' TYPES THE PAIR INLINE, in a row drawn at the drawer's end; the sheet is
    // reopened so the walk to it is the reader's own.
    await sheet(p, base, "drv-marks");
    await walkTo(p, ".d-drawer", "the drawer");
    await p.press("TAB");
    await p.until(() => document.querySelectorAll("#mdoc .d-drawer .d-meta").length > 0,
                  "TAB to open the drawer, so the reading below is the open one's");
    // THE DRAWER AS IT STANDS, against which the cancel below is byte-identical.
    const drawerText = () => p.eval(() =>
      [...document.querySelectorAll("#mdoc .d-drawer .d-meta")].map((e) => e.textContent));
    const before = await drawerText();
    await p.press("+");
    await pairUp(p, "the two fields to open over a drawn row");
    // THE BOX IS THE PAIR IT WILL BECOME: the drawer's own colons around a key
    // field and a value field, over the row drawn where the pair will stand.
    const dressed = await p.eval(() => {
      const box = document.getElementById("dpair");
      return { punc: [...box.querySelectorAll(".dpunc")].map((s) => s.textContent),
               fields: [...box.querySelectorAll("input")].map((i) => i.id),
               focus: document.activeElement.id,
               // THE KEY IS SIZED BEFORE THE FIRST PAINT: the open fills, then
               // focuses, and the focus asks `pairMoved' -- so an empty key
               // stands one cell wide, its caret's own room.
               ch: cell(),
               keyW: document.getElementById("dkey").getBoundingClientRect().width,
               // The drawn row is the model's own and joins no list.
               drafts: document.querySelectorAll("#mdoc .d-draft").length,
               metas: document.querySelectorAll("#mdoc .d-drawer .d-meta").length };
    });
    assert(dressed.punc.join("") === "::" && dressed.fields.join(",") === "dkey,dval",
      `the box reads ${JSON.stringify(dressed)}`);
    assert(dressed.focus === "dkey", `the focus opened on ${dressed.focus}`);
    assert(Math.abs(dressed.keyW - dressed.ch) < 2,
      `the empty key opened ${dressed.keyW}px wide against one cell of ${dressed.ch}px`);
    assert(dressed.drafts === 1 && dressed.metas === before.length + 1,
      `the draft drew ${dressed.drafts} rows, the drawer now ${dressed.metas}`);
    // BOTH HALVES COMPLETE FROM THE TREE'S OWN VOCABULARY, which is a live route
    // here: no stub answers this browser.
    await p.type("OW");
    // THE VOCABULARY IS AN ASYNC ARRIVAL WITH A REPAINT OF ITS OWN (`askVocab',
    // 20-sheet.js): the box goes up on the typed literal ALONE and grows the
    // tree's own keys when `/properties' lands.  Reading on `className' is the
    // filed read-races-arrival flake caught on this very case, so the wait is
    // for a SECOND entry -- the cap still fails the case where none ever comes.
    const keyOffers = await p.until(() => {
      const box = document.getElementById("doffer");
      if (box.className !== "on" || box.children.length < 2) return false;
      return [...box.children].map((c) => ({
        word: (c.querySelector(".dow") || {}).textContent || "",
        hint: (c.querySelector(".dot") || {}).textContent || "" }));
    }, "the key's offers to draw off GET /properties");
    assert(keyOffers.some((o) => o.word === "OWNER"),
      `the key offers ${JSON.stringify(keyOffers)}`);
    // THE TYPED VALUE IS ALWAYS AN OFFER (AGENTS.hs): `OW' leads the key it reads
    // as a prefix of, so RET here would commit `OW' — and the match is a walk
    // away, `C-n' onto it and `:' taking it.
    assert(keyOffers[0].word === "OW" && keyOffers[0].hint === "new",
      `the typed line does not lead the offers: ${JSON.stringify(keyOffers)}`);
    await p.press("C-n");
    await p.press(":");
    const completed = await p.until(() => document.activeElement.id === "dval"
        && document.getElementById("dkey").value === "OWNER"
      ? document.getElementById("dkey").value : false,
      "`C-n' then `:' to complete the key and hand it over");
    // ":OWNER:" READS AS THE DRAWER LINE IT STANDS OVER: the key field is
    // exactly its text wide, so the closing colon lands against the R rather
    // than out at a column the layout picked.  RELATIVE GEOMETRY ONLY -- where
    // the BOX sits is `placeEdit''s business and a frame behind this read.
    const hug = await p.eval(() => {
      const key = document.getElementById("dkey").getBoundingClientRect();
      const shut = [...document.querySelectorAll("#dpair .dpunc")]
        .filter((s) => !s.classList.contains("dlead"))[0].getBoundingClientRect();
      return { ch: cell(), width: key.width, gap: shut.left - key.right,
               chars: document.getElementById("dkey").value.length };
    });
    assert(Math.abs(hug.gap) <= 2,
      `the closing colon stands ${hug.gap}px off the key's own right edge`);
    assert(Math.abs(hug.width - hug.chars * hug.ch) < 2,
      `the key holds ${hug.chars} characters and measures ${hug.width}px `
      + `against ${hug.chars * hug.ch}px of cells`);
    // ESC CANCELS THE INPUT WHOLE: the box goes, the drawn row with it, and the
    // drawer is the rows it was, character for character.
    await p.press("ESC");
    await p.until(() => !document.getElementById("dpair").classList.contains("on")
                    && document.querySelectorAll("#mdoc .d-draft").length === 0,
                  "ESC to take the box and the drawn row together");
    const after = await drawerText();
    assert(JSON.stringify(after) === JSON.stringify(before),
      `the cancelled drawer reads ${JSON.stringify(after)} against ${JSON.stringify(before)}`);
    // AND AGAIN, THROUGH THE REAL FIELDS, `:' handing the key over to its value.
    await p.press("+");
    await pairUp(p, "the two fields to open again");
    await p.type("ROOM");
    await p.press(":");
    await p.until(() => document.activeElement.id === "dval",
                  "`:' to hand the key over to its value");
    await p.type("12");
    await p.press("RET");
    await p.until(async () => {
      const h = await (await fetch("/headline?id=drv-marks")).json();
      return (h.properties || []).some(([k, v]) => k === "ROOM" && v === "12");
    }, "the minted pair to reach the file", 15000);
    // A KEY THAT FOLDS TO A PLANNING WORD IS NO PROPERTY: the same box, and what
    // it commits lands on the PLANNING LINE with the drawer left as it stood.
    const planless = await served();
    await settled(p);
    await p.press("+");
    await pairUp(p, "the two fields to open a third time");
    await p.type("scheduled");
    await p.press(":");
    await p.until(() => document.activeElement.id === "dval",
                  "the planning key to hand over to its value");
    await p.type("<2026-09-01 Tue>");
    await p.press("RET");
    const routed = await p.until(async () => {
      const h = await (await fetch("/headline?id=drv-marks")).json();
      return (h.planning || []).some(([k, v]) =>
        k === "SCHEDULED" && v === "<2026-09-01 Tue>") ? h : false;
    }, "the typed entry to reach the PLANNING line, upcased", 15000);
    assert(JSON.stringify(routed.properties) === JSON.stringify(planless.props),
      `the drawer reads ${JSON.stringify(routed.properties)} `
      + `against ${JSON.stringify(planless.props)}`);
    const end = await served();
    return [`OWNER edited, EFFORT dropped, ROOM typed inline off `
      + `${JSON.stringify(keyOffers.map((o) => o.word))}; "OW" led them and `
      + `C-n completed it to ${JSON.stringify(completed)}; `
      + `ESC left ${JSON.stringify(before)} standing; `
      + `":OWNER:" closed ${hug.gap.toFixed(1)}px off a ${hug.width.toFixed(1)}px key; `
      + `scheduled routed to ${JSON.stringify(end.plan)} over `
      + `${JSON.stringify(end.props)}`];
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
      return { ch: cell(),
               root: pad("#mdoc .d-para", /The entry a case reads/),
               kid: pad("#mdoc .d-para", /A paragraph the child owns/),
               grand: pad("#mdoc .d-para", /The grandchild's own line/) };
    });
    assert(Math.abs(insets.kid - insets.root - 2 * insets.ch) < 0.1
             && Math.abs(insets.grand - insets.root - 4 * insets.ch) < 0.1,
      `the shelves indent ${insets.root}/${insets.kid}/${insets.grand} on a `
      + `${insets.ch}px character`);
    // THE OUTLINE DIVES: n from the first child enters its subtree headfirst,
    // and the second child is the step after the grandchild.
    await walkTo(p, ".d-child", "the first child headline");
    const first = await p.eval(() => document.querySelector("#mdoc .de.dat").textContent);
    assert(/A child whose body/.test(first), `the walk reached ${JSON.stringify(first)}`);
    await p.press("n");
    await p.until(() => /A grandchild/.test(
      document.querySelector("#mdoc .de.dat").textContent),
      "n to dive to the grandchild");
    await p.press("n");
    await p.until(() => /A second child/.test(
      document.querySelector("#mdoc .de.dat").textContent),
      "n to climb out to the SECOND child");
    await p.press("p");
    await p.until(() => /A grandchild/.test(
      document.querySelector("#mdoc .de.dat").textContent), "p back in");
    await p.press("p");
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
    await editUp(p, "the edit");
    await p.eval(() => { document.getElementById("dtext").value = "A paragraph the child edits."; });
    await p.press("RET");
    await p.until(async () => {
      const h = await (await fetch("/headline?id=drv-marks")).json();
      return /A paragraph the child edits\./.test(h.org || "");
    }, "the child's line to reach the file", 15000);
    // `b' CLIMBS TO THE OWNER: the paragraph's owner is the child headline.
    await p.until(() => !!document.querySelector("#mdoc .de.dat"), "point back");
    await settled(p);
    await stepped(p, "b", ".d-child", "b to climb to the child headline");
    // A HEADLINE AT POINT LIGHTS ONE SHELF: on the entry's own headline the
    // shelf's runs bar in the mark, and the child's list keeps its resting
    // bar -- the light stops at the child's block.
    await stepped(p, "B", ".d-head", "B climbs to the entry's own headline");
    const shelfLight = await p.eval(() => {
      const item = (re) => [...document.querySelectorAll("#mdoc .d-item")]
        .find((e) => re.test(e.textContent));
      const own = item(/a dash, the bullet/);
      const kid = item(/a list the child owns/);
      return { own: own ? ink(own) : null,
               kid: kid ? ink(kid) : null,
               mk: mark(), off: g("point-off") };
    });
    assert(shelfLight.own === shelfLight.mk,
      `the entry's own run bars ${shelfLight.own}, not the mark's ${shelfLight.mk}`);
    assert(shelfLight.kid === shelfLight.off,
      `the child's run bars ${shelfLight.kid}, so the light ran into the child's block`);
    await walkTo(p, ".d-child", "back down to the first child");
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
    // THE RAMP, THE NAMES AND THE SHELVES ARE OBSERVED: point inside the child,
    // its block wears rank 0 and the root's a step out; a sibling child's rows
    // sit BESIDE the first child's block; the strip names a child by its TITLE.
    await stepped(p, "f", ".de", "f to enter the child before the ranks are read");
    const ranked = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const inner = at.closest(".blk");
      const outer = inner && inner.parentElement.closest(".blk");
      const second = [...document.querySelectorAll("#mdoc .d-para")]
        .find((e) => /Its paragraph/.test(e.textContent));
      return { inner: inner ? inner.className : "",
               outer: outer ? outer.className : "",
               apart: !!second && !!inner && !inner.contains(second),
               crumb: document.querySelector("#mdoc .dpath").textContent };
    });
    assert(/\bsp-0\b/.test(ranked.inner) && /\bsp-1\b/.test(ranked.outer),
      `the ramp ranks inner "${ranked.inner}" and outer "${ranked.outer}"`);
    assert(ranked.apart,
      "a sibling child's rows nest inside the first child's block");
    assert(/A child whose body/.test(ranked.crumb),
      `the strip reads ${JSON.stringify(ranked.crumb)}, never the child's title`);
    return [`3 children inline; the shelf steps over subtrees; a child's paragraph `
      + `edits through the splice; TAB folds the subtree whole; the ramp ranks `
      + `${(ranked.inner.match(/sp-\d/) || [])[0]}/${(ranked.outer.match(/sp-\d/) || [])[0]}`];
  } },
{ name: "n on a headline walks headlines at every depth, p past a body's edge "
      + "lands on one, and a fold is skipped",
  async run(p, base) {
    await sheet(p, base, "drv-marks");
    await walkTo(p, ".d-child", "the first child");
    const at = () => p.eval(() => {
      const a = document.querySelector("#mdoc .de.dat");
      return { text: a.textContent.slice(0, 30),
               head: a.classList.contains("d-head") };
    });
    const to = async (key, re, why) => {
      await stepped(p, key, ".d-head, .d-child", why);
      const seen = await at();
      assert(re.test(seen.text), `${why}: landed on ${JSON.stringify(seen.text)}`);
      return seen;
    };
    // DOWN THE OUTLINE: into the child's subtree, then out to its sibling.
    await to("n", /grandchild/, "n dives to the grandchild");
    await to("n", /second child/, "n climbs out to the second child");
    // AND BACK UP, ending on the entry's own line.
    await to("p", /grandchild/, "p returns to the grandchild");
    await to("p", /child whose body/, "p returns to the first child");
    const root = await to("p", /Every marker/, "p ends on the headline");
    assert(root.head, "the walk's top is not the entry's own line");
    // THE ROOT IS THE READER'S EXCEPTION: n from the entry's line steps into
    // its own contents, the way f does -- a headline is not the landing.
    await stepped(p, "n", ".de", "n from the root into the entry's own body");
    const led = await p.eval(() =>
      document.querySelector("#mdoc .de.dat").matches(".d-head, .d-child"));
    assert(!led, "n from the root skipped the body for a headline");
    // A FOLDED SUBTREE IS SKIPPED WHOLE, org's next-visible-heading.
    await walkTo(p, ".d-child", "back down to the first child");
    await p.press("TAB");
    await p.until(() => /…/.test(document.querySelector("#mdoc .de.dat").textContent),
      "TAB to fold the child");
    await settled(p);
    await to("n", /second child/, "n skips the folded subtree");
    await to("p", /child whose body/, "p lands on the folded line, never inside");
    await p.press("TAB");
    await p.until(() => !/…/.test(document.querySelector("#mdoc .de.dat").textContent),
      "TAB to open it again");
    // `p' IS HEADLINE-SIZED PAST A BODY'S EDGE: from the body's FIRST element
    // it is the headline that body hangs under, where before it stood still.
    await stepped(p, "f", ".d-para", "f into the child's own body");
    await to("p", /child whose body/, "p from a body's first element is its headline");
    // AND INSIDE THE BODY IT IS AN ELEMENT STEP, exactly as it was: the shelf's
    // own previous element comes before any headline going up.
    await stepped(p, "f", ".d-para", "f back into the body");
    await stepped(p, "n", ".d-list", "n across the body to the list the child owns");
    await stepped(p, "p", ".d-para", "p back to the paragraph beside it");
    // A RUN'S LEAVES KEEP THEIR LIST'S EDGE: they answer to the composite, not
    // to the body, so their walk is untouched.
    await stepped(p, "n", ".d-list", "n to the list again");
    await stepped(p, "f", ".d-item", "f into the list's items");
    await stepped(p, "n", ".d-item", "n to the second item");
    await stepped(p, "p", ".d-item", "p back to the first item, mid-run");
    const item = await at();
    assert(/a list the child owns/.test(item.text),
      `the run's step back landed on ${JSON.stringify(item.text)}`);
    // THE TAIL IS ONE PRESS FROM THE DOCUMENT'S LAST HEADLINE, the subtrees
    // between them crossed whole.
    await stepped(p, "b", ".d-list", "b out of the item to its list");
    await stepped(p, "B", ".d-child", "B climbs out of the list to the child headline");
    await to("n", /grandchild/, "n to the grandchild");
    await to("n", /second child/, "n to the last headline");
    await stepped(p, "n", ".d-tail", "n past the last subtree onto the tail");
    await to("p", /second child/, "one p from the tail is the last headline");
    // A BODY ACROSS THE BOUNDARY CLIMBS TO ITS OWN HEADLINE the same way.
    await stepped(p, "f", ".d-para", "f into the last child's own body");
    await to("p", /second child/, "p from its first element climbs back to it");
    // AND `p' FROM BELOW NEVER LANDS INSIDE A FOLD: the last child shut, the
    // tail's one press is its folded LINE.
    await p.press("TAB");
    await p.until(() => /…/.test(document.querySelector("#mdoc .de.dat").textContent),
      "TAB to fold the last child");
    await settled(p);
    await stepped(p, "n", ".d-tail", "n onto the tail past the folded subtree");
    await stepped(p, "p", ".d-child", "p from the tail back to the folded child");
    const shut = await p.eval(() =>
      document.querySelector("#mdoc .de.dat").textContent);
    assert(/A second child/.test(shut) && /…/.test(shut),
      `p from the tail landed on ${JSON.stringify(shut.slice(0, 40))}, not the folded line`);
    await p.press("TAB");
    await p.until(() => !/…/.test(document.querySelector("#mdoc .de.dat").textContent),
      "TAB to open the last child again");
    return ["the outline walks child, grandchild, sibling and back to the top; "
      + "a folded subtree is one step; p is an element step inside a body and "
      + "the nearest visible headline past its edge, one press from the tail"];
  } },
{ name: "the pane is a narrowing, so a typed root-level headline is demoted",
  async run(p, base) {
    await sheet(p, base, "drv-marks");
    await walkTo(p, ".d-para", "the entry's own paragraph");
    await p.press("RET");
    await editUp(p, "the edit");
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
{ name: "the pane ends on one empty line, and RET there writes a paragraph",
  async run(p, base) {
    await sheet(p, base, "drv-marks");
    // THE TAIL IS THERE BUT HIDDEN: one empty row past everything, shown
    // only when the walk reaches it.
    const tail = await p.eval(() => {
      const rows = [...document.querySelectorAll("#mdoc .de")];
      const last = rows[rows.length - 1];
      return { last: last.matches(".d-tail"),
               empty: last.textContent === "",
               hidden: last.getBoundingClientRect().height === 0,
               count: rows.filter((e) => e.matches(".d-tail")).length };
    });
    assert(tail.last && tail.empty, "the last row is not the empty tail");
    assert(tail.hidden, "the tail shows before the walk reaches it");
    assert(tail.count === 1, `${tail.count} tail rows`);
    // THE WALK ENDS ON IT: n alone reaches it, through the child subtrees.
    for (let i = 0; i < 14; i += 1) {
      const there = await p.eval(() =>
        document.querySelector("#mdoc .de.dat").matches(".d-tail"));
      if (there) break;
      await stepped(p, "n", ".de", "n toward the tail");
    }
    assert(await p.eval(() => {
      const a = document.querySelector("#mdoc .de.dat");
      return a.matches(".d-tail") && a.getBoundingClientRect().height > 10;
    }), "the reached tail is not shown a line tall");
    // RET THERE IS THE DOOR: type, commit, and the paragraph lands at the end.
    await p.press("RET");
    await editUp(p, "the edit over the tail");
    await p.eval(() => { document.getElementById("dtext").value
      = "A tail paragraph, minted at the end"; });
    await p.press("RET");
    await p.until(async () => {
      const h = await (await fetch("/headline?id=drv-marks")).json();
      return /A tail paragraph, minted at the end/.test(h.org || "");
    }, "the write to reach the file", 15000);
    // AND THE TAIL REGROWS: still one empty line, now past the new paragraph.
    await p.until(() => {
      const rows = [...document.querySelectorAll("#mdoc .de")];
      const last = rows[rows.length - 1];
      return last.matches(".d-tail")
        && rows.some((e) => /A tail paragraph/.test(e.textContent));
    }, "the rescan to draw the paragraph with a fresh tail after it");
    return ["one empty tail line; RET wrote a paragraph at the end; the tail regrew"];
  } },

// THE READING LINE.  `block:"nearest"' put point's row on the pane's LAST line
// and left the reader with no text under it; the pane now rests the row on a
// line drawn part of the way down.  Only a real layout can say where that is.
{ name: "walking down, point's row rests on the reading line and never falls past it",
  async run(p, base) {
    // A SHORT PANE, so an ordinary fixture is a long document against it.
    await p.size(1000, 380);
    await sheet(p, base, "drv-marks");
    await rested(p);
    const opened = await p.eval(paneRead);
    assert(opened.room > 100,
      `the pane does not overflow: ${px(opened.room)} of scroll under `
      + `${px(opened.height)} of pane`);
    assert(opened.scroll === 0,
      `the sheet opened already scrolled to ${px(opened.scroll)}`);

    const line = (b) => b.height * 0.6;
    const past = [], onLine = [];
    let first = null;
    for (let i = 0; i < 8; i += 1) {
      await stepped(p, "n", ".de", `the step ${i + 1} rows down the document`);
      await rested(p);
      const now = await p.eval(paneRead);
      if (first === null && now.scroll > 0) first = i + 1;
      // AT THE END THE PANE CLAMPS, which is the law's own "if possible"; and a
      // row TALLER than the band above the line has no rest there to take.
      if (now.scroll >= now.room - 1 || now.rowHeight > line(now)) continue;
      if (now.rowBottom - now.top > line(now) + 1)
        past.push(`${JSON.stringify(now.text)} sits ${px(now.rowBottom - now.top)} `
          + `down a ${px(now.height)} pane, past its ${px(line(now))} line`);
      if (now.scroll > 0) onLine.push(now.rowBottom - now.top - line(now));
    }
    assert(past.length === 0, past.join(" · "));
    assert(first !== null && first > 1,
      `the pane scrolled on step ${first} — the rows above the line must move nothing`);
    assert(onLine.length > 0,
      "no step ever scrolled the pane, so the line was never measured");
    assert(onLine.every((d) => Math.abs(d) <= 1),
      `a scrolled row came to rest off the line by ${onLine.map(px).join(", ")}`);
    return [`the pane first scrolled on step ${first}; `
      + `${onLine.length} rows came to rest on the line, off by at most `
      + px(Math.max(...onLine.map((d) => Math.abs(d))))];
  } },

// THE LINE IS THE READER'S, remembered per browser like the theme.
{ name: "the reading line is a stored preference, and 40 puts it two fifths down",
  async run(p, base) {
    await p.size(1000, 380);
    await sheet(p, base, "drv-marks");
    await p.eval(() => localStorage.setItem("glance-reading-line", "40"));
    try {
      await sheet(p, base, "drv-marks");
      const seen = [];
      for (let i = 0; i < 8; i += 1) {
        await stepped(p, "n", ".de", `the step ${i + 1} rows down the document`);
        await rested(p);
        const now = await p.eval(paneRead);
        if (now.scroll <= 0 || now.scroll >= now.room - 1) continue;
        if (now.rowHeight > now.height * 0.4) continue;
        seen.push({ at: (now.rowBottom - now.top) / now.height, text: now.text });
      }
      assert(seen.length > 0, "no step scrolled the pane, so the line was never measured");
      const off = seen.filter((s) => Math.abs(s.at - 0.4) > 0.02);
      assert(off.length === 0,
        `the stored 40% was not honoured: `
        + off.map((s) => `${JSON.stringify(s.text)} at ${Math.round(s.at * 100)}%`).join(", "));
      const shown = await p.eval(() => document.getElementById("readsel").value);
      assert(shown === "40", `the settings select reads ${JSON.stringify(shown)}`);
      return [`${seen.length} rows rested at `
        + `${seen.map((s) => Math.round(s.at * 100) + "%").join(", ")}`];
    } finally {
      await p.eval(() => localStorage.removeItem("glance-reading-line"));
    }
  } },

// `C-l' IS `recenter-top-bottom': three placements in a cycle, and only a real
// layout can say whether the row landed on any of them.
{ name: "C-l puts the row at the middle, then the top, then the bottom",
  async run(p, base) {
    await p.size(1000, 380);
    await sheet(p, base, "drv-marks");
    // Far enough down that every rung is a scroll the pane can actually make:
    // a clamped one would measure the pane's end rather than the placement.
    const reachable = (b) => {
      const r = rungs(b);
      return Math.min(r.center, r.top, r.bottom) >= 0
        && Math.max(r.center, r.top, r.bottom) <= b.room;
    };
    let now = null;
    for (let i = 0; i < 12; i += 1) {
      await stepped(p, "n", ".de", `the step ${i + 1} rows down the document`);
      await rested(p);
      now = await p.eval(paneRead);
      if (reachable(now)) break;
    }
    assert(reachable(now),
      `no row the walk reached leaves room for all three rungs: `
      + `${JSON.stringify(rungs(now))} against ${px(now.room)} of scroll`);
    const deep = now.rowTop - now.top + now.scroll;

    await p.press("C-l");
    await rested(p);
    const mid = await p.eval(paneRead);
    assert(Math.abs((mid.rowTop + mid.rowHeight / 2) - (mid.top + mid.height / 2)) <= 2,
      `the first press left the row's middle ${px((mid.rowTop + mid.rowHeight / 2)
        - (mid.top + mid.height / 2))} off the pane's`);

    await p.press("C-l");
    await rested(p);
    const top = await p.eval(paneRead);
    assert(Math.abs(top.rowTop - (top.top + top.off)) <= 2,
      `the second press left the row's top ${px(top.rowTop - (top.top + top.off))} `
      + `off the ${px(top.off)} band`);

    await p.press("C-l");
    await rested(p);
    const low = await p.eval(paneRead);
    assert(Math.abs(low.rowBottom - (low.top + low.height - low.off)) <= 2,
      `the third press left the row's bottom `
      + `${px(low.rowBottom - (low.top + low.height - low.off))} off the band above `
      + "the pane's end");
    return [`the row is ${px(deep)} down a ${px(now.height)} pane: middle `
      + `${px(mid.rowTop + mid.rowHeight / 2 - mid.top)}, top ${px(top.rowTop - top.top)}, `
      + `bottom ${px(low.rowBottom - low.top)} of ${px(low.height)}`];
  } },

// `M-<left>'/`M-<right>' ARE org's `org-promote-subtree'/`org-demote-subtree':
// the headline under point and every headline inside it, one star at a time.
{ name: "M-arrows move a child's whole subtree, and the narrowing wall refuses",
  async run(p, base) {
    const org = () => p.eval(async () =>
      (await (await fetch("/headline?id=drv-marks")).json()).org);
    const pill = () => p.eval(() => document.getElementById("echo").textContent);
    await sheet(p, base, "drv-marks");
    const was = await org();
    assert(/\n\*\* TODO A child whose body/.test(was)
      && /\n\*\*\* DONE A grandchild/.test(was),
      "drv-marks no longer carries a child with a grandchild under it");

    // ------- THE NARROWING WALL IS A FLOOR, and it is asked FIRST, over bytes
    // nothing has written: a DIRECT child promoted would leave the subtree.
    // BY ITS WORDS, never by its class: earlier cases leave drv-marks carrying
    // children of their own, and the FIRST `.d-child' is one of those.
    let landed = false;
    for (let i = 0; i < 20 && !landed; i += 1) {
      const seen = await p.eval(() => {
        const at = document.querySelector("#mdoc .de.dat");
        return { synced: !!at && at.dataset.id === docAtNow(),
                 hit: !!at && at.matches(".d-child")
                   && /A child whose body/.test(at.textContent) };
      });
      if (seen.hit) { landed = true; break; }
      if (!seen.synced) { await settled(p); continue; }
      await stepped(p, "n", ".de", "the step toward the child with a grandchild");
    }
    assert(landed, "`n' never reached the child whose body is drawn in place");
    await settled(p);
    await p.press("M-<left>");
    const said = await p.until(() => {
      const t = document.getElementById("echo").textContent;
      return /org-promote-subtree/.test(t) ? t : false;
    }, "the refusal to be spoken");
    assert(said === "M-<left> \u2192 org-promote-subtree "
      + "(nothing shallower than a child of the entry)",
      `the pill read ${JSON.stringify(said)}`);
    assert((await org()) === was, "the refused promote moved a byte");

    // ------- AND THE DEMOTE MOVES THE SUBTREE WHOLE.
    await settled(p);
    await p.press("M-<right>");
    const deeper = await p.until(async () => {
      const h = await (await fetch("/headline?id=drv-marks")).json();
      return /\n\*\*\* TODO A child whose body/.test(h.org || "") ? h.org : false;
    }, "the demote to reach the file", 15000);
    assert(/\n\*\*\*\* DONE A grandchild/.test(deeper),
      "the grandchild stayed behind: a subtree moves whole");
    assert(deeper === was
      .replace("\n** TODO A child whose body", "\n*** TODO A child whose body")
      .replace("\n*** DONE A grandchild", "\n**** DONE A grandchild"),
      "the demote moved a byte that is no headline's stars");
    assert((await pill()) === "M-<right> \u2192 org-demote-subtree (level 3)",
      `the pill read ${JSON.stringify(await pill())}`);

    // ------- POINT IS STILL ON THE CHILD, the write and its refill behind it:
    // a child's id is its POSITION among the file's headlines, which no shift
    // moves.  AND THE PROMOTE PUTS THE TREE BACK, which every later case needs.
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && at.dataset.id === docAtNow() && at.matches(".d-child")
        && /A child whose body/.test(at.textContent);
    }, "point back on the child the write moved", 15000);
    await p.press("M-<left>");
    await p.until(async (bytes) => {
      const h = await (await fetch("/headline?id=drv-marks")).json();
      return h.org === bytes;
    }, "the promote to put the bytes back", 15000, was);
    return [`the wall said ${JSON.stringify(said)}; `
      + "the child and its grandchild went 2/3 to 3/4 and back"];
  } },

// THE TYPED VALUE IS ALWAYS AN OFFER where the vocabulary is open (AGENTS.hs).
// REPORTED: `shelf' typed against a tree holding `bookshelf' narrowed to the one
// match, point sat on it, and RET wrote `bookshelf' — the reader's own word was
// never an entry, so it could not be committed.  LAST IN THE FILE ON PURPOSE:
// this is the one case that writes a TAG, and the tag column's own geometry
// cases read the table before it.
{ name: "the add-a-tag field commits the word typed, never the tag it prefixes",
  async run(p, base) {
    await p.goto(`${base}/?q=tag%3Ageometry`);
    await p.until(() => document.querySelectorAll("#app table tbody tr").length === 1,
                  "the one geometry row to mount");
    await p.press(":");
    await p.until(() => document.getElementById("tags").classList.contains("on"),
                  "the tags popup to open over the row");
    await p.press("+");
    await p.until(() => document.getElementById("pbox").classList.contains("narrow"),
                  "the add-a-tag field to raise over the popup");
    await p.type("mark");
    const shown = await p.until(() => {
      const rows = [...document.querySelectorAll("#plist .pe")].map((r) => ({
        word: (r.querySelector(".pw") || {}).textContent || "",
        hint: (r.querySelector(".pt") || {}).textContent || "",
        at: r.classList.contains("pat") }));
      return rows.length > 1 ? rows : false;
    }, "the typed line and the tag it prefixes to draw together");
    assert(shown[0].word === "mark" && shown[0].hint === "new" && shown[0].at,
      `the typed line does not lead, hinted, with point on it: `
      + JSON.stringify(shown));
    assert(shown.some((r) => r.word === "marks"),
      `the tag it prefixes is not offered beside it: ${JSON.stringify(shown)}`);
    await p.press("RET");
    const tags = await p.until(async () => {
      const a = await (await fetch("/tags?ids=drv-box")).json();
      const row = (a.rows || [])[0] || {};
      return (row.tags || []).indexOf("mark") === -1 ? false : row.tags;
    }, "the typed tag to reach the file", 15000);
    assert(tags.indexOf("marks") === -1,
      `the tag the field completed to landed instead: ${JSON.stringify(tags)}`);
    return [`typed "mark" over ${JSON.stringify(shown.map((r) => r.word))}, `
      + `the row now ${JSON.stringify(tags)}`];
  } },
// WHOLE TAGS OR NONE, AND THE CELL IS A DOOR. The renderer drops the values a
// column cannot draw ENTIRE and marks their place — a half-word is a tag no
// query spells — and this page reads a click there as the very binding `:'
// runs, over the row the click landed on.
{ name: "the tags cell ends on a whole tag, and a click there opens the tags door",
  async run(p, base) {
    await tableUp(p, base);
    const CELL = '#app tr[data-id="drv-wide"] td.tv-multi';
    await p.until((sel) => !!document.querySelector(sel),
                  "the wide row's tags cell to draw", undefined, CELL);
    // The row's WHOLE tag list is the STORE's answer and the cell is what the
    // column had room for, so the two are read apart and compared: a case
    // spelling both would only agree with itself.
    const held = await p.eval(async () => {
      const a = await (await fetch("/tags?ids=drv-wide")).json();
      return ((a.rows || [])[0] || {}).tags || [];
    });
    const cell = await p.eval((sel) => {
      const td = document.querySelector(sel);
      return { text: td.textContent,
               names: [...td.querySelectorAll(".tv-tag")].map((e) => e.textContent),
               // A cut the STYLESHEET made leaves the content wider than the
               // cell; a cut the renderer made fits inside it.
               over: td.scrollWidth - td.clientWidth };
    }, CELL);
    assert(held.length > cell.names.length,
      `the fixture no longer overruns the column: ${JSON.stringify(held)}`);
    assert(cell.names.every((n) => held.indexOf(n) !== -1),
      `a name drawn in the cell is no whole tag of the row: ${JSON.stringify(cell)}`
      + ` of ${JSON.stringify(held)}`);
    assert(cell.text === cell.names.join(" · ") + " …",
      `the cell is not whole names and the mark: ${JSON.stringify(cell.text)}`);
    assert(cell.over <= 1,
      `the cell overruns its column by ${cell.over}px, so the stylesheet cut it`);

    // Point the cursor elsewhere first, or "the click targeted the row" is a
    // claim about the row the cursor was already on.
    const before = await p.eval(() => {
      const at = document.querySelector("#app tr.tv-sel");
      return at ? at.getAttribute("data-id") : null;
    });
    if (before === "drv-wide") {
      await p.press("n");
      await p.until(() => {
        const at = document.querySelector("#app tr.tv-sel");
        return !!at && at.getAttribute("data-id") !== "drv-wide";
      }, "the cursor to step off the wide row");
    }
    // The popup's list is an ELM mount and a port round trip costs a macrotask,
    // so the rows are WAITED for rather than read the instant it raises.
    const door = () => p.until(() => {
      const rows = [...document.querySelectorAll("#ttable tbody tr[data-id]")];
      if (!rows.length) return false;
      const at = document.querySelector("#app tr.tv-sel");
      return { head: document.getElementById("thead").textContent,
               tags: rows.map((tr) => tr.children[0].textContent),
               row: at ? at.getAttribute("data-id") : null };
    }, "the popup's tag rows to draw");
    await p.eval((sel) =>
      document.querySelector(sel).querySelector(".tv-tags").click(), CELL);
    await p.until(() => document.getElementById("tags").classList.contains("on"),
                  "the tags popup to open on the click");
    const clicked = await door();
    assert(clicked.row === "drv-wide",
      `the click left the cursor on ${JSON.stringify(clicked.row)}`);
    assert(clicked.tags.length === held.length,
      `the popup is over ${JSON.stringify(clicked.tags)}, not the row's `
      + JSON.stringify(held));

    // THE SAME DOOR, asked the other way: the popup `:' opens over the very row
    // the click left the cursor on, by its id, its title and its rows.
    await p.press("ESC");
    await p.until(() => !document.getElementById("tags").classList.contains("on"),
                  "the popup to close on ESC");
    await p.press(":");
    await p.until(() => document.getElementById("tags").classList.contains("on"),
                  "the same popup to open on `:'");
    const keyed = await door();
    assert(JSON.stringify(keyed) === JSON.stringify(clicked),
      `the click's door and the key's differ: `
      + JSON.stringify([clicked, keyed]));
    return [`${JSON.stringify(held)} drew as ${JSON.stringify(cell.text)}`
      + ` (${cell.over}px past the column), and the click opened `
      + `${JSON.stringify(clicked.head)} over ${clicked.tags.length} tags`];
  } },
{ name: "the date widget stands in the value's own slot, and the phrase lands whole",
  async run(p, base) {
    const served = (id) => p.eval(async (row) => {
      const h = await (await fetch(`/headline?id=${row}`)).json();
      return { plan: h.planning || [], props: h.properties || [] };
    }, id);
    /** The planning line as the pane DREW it, or `""' where the row has none. */
    const planLine = () => p.eval(() => {
      const at = document.querySelector('#mdoc .de[data-id="PLN"]');
      return at ? at.textContent : "";
    });

    // ------- over a value that stands: the entry comes up WHOLLY SELECTED.
    await sheet(p, base, "drv-plan");
    const wasLine = await planLine();
    const stood = ((await served("drv-plan")).plan
      .find(([k]) => k === "DEADLINE") || [])[1];
    assert(!!stood, "drv-plan carries no DEADLINE for the widget to open over");
    // A SELECTION IS A THING THE READER SEES, and no engine paints one in a
    // document without the focus: a rung that skipped this would go green over
    // a screen showing nothing.
    assert(await p.eval(() => document.hasFocus()),
      "the driven page has no focus, so nothing below could prove a selection");
    await settled(p);
    await p.press("C-c");
    await p.press("C-d");
    await widgetUp(p, "the widget over the DEADLINE value");
    const open = await p.eval(() => {
      const f = document.getElementById("dwhen");
      const r = f.getBoundingClientRect();
      const at = document.querySelector("#mdoc .de.dat");
      const token = (n) => getComputedStyle(document.documentElement)
        .getPropertyValue(n).trim().toLowerCase();
      return { val: f.value, sel: [f.selectionStart, f.selectionEnd],
               ghost: document.getElementById("dghost").textContent,
               box: { x: Math.round(r.x), y: Math.round(r.y),
                      w: Math.round(r.width), h: Math.round(r.height) },
               ground: rgb(getComputedStyle(document.getElementById("ddate")).backgroundColor),
               surface: g("surface"),
               wash: token("--g-sel"),
               rowWash: getComputedStyle(at).backgroundColor };
    });
    assert(open.val === stood, `the widget opened holding ${JSON.stringify(open.val)}`
      + ` where the row carries ${JSON.stringify(stood)}`);
    assert(open.sel[0] === 0 && open.sel[1] === open.val.length,
      `it opened selecting ${open.sel[0]}..${open.sel[1]} of ${open.val.length}`);
    // THE GHOST IS SILENT AT ENTRY: the value that stands is its own answer.
    assert(open.ghost === "",
      `the ghost said ${JSON.stringify(open.ghost)} over org's own spelling`);
    // THE TWO GOLDS, AND WHY ONE GOES.  `--g-sel' is the cursor row's wash AND
    // every field's text selection; the widget stands INSIDE that row rather
    // than covering it, so the box carries the pane's own edit ground and the
    // row lifts its wash while one is open.
    assert(open.ground === open.surface,
      `the widget's ground is ${open.ground}, not the pane's ${open.surface}`);
    assert(/rgba\(0, 0, 0, 0\)|transparent/.test(open.rowWash),
      `the row at point still wears ${open.rowWash} under an open widget`);

    // …AND THE SELECTION IS PAINTED.  Read in PIXELS, because "set" and "seen"
    // are two claims: the same frame with the caret collapsed differs across the
    // value, and what differs is the palette's own selection wash.
    const on = await p.paint();
    await p.eval(() => {
      const f = document.getElementById("dwhen");
      f.setSelectionRange(f.value.length, f.value.length);
    });
    // THE COLLAPSE IS WAITED FOR, not assumed: a capture can land before the
    // engine draws it, and two identical frames would read as a selection
    // never seen.  A selection truly invisible stays identical past the cap
    // and fails below exactly as it should (the caret's return is too few
    // pixels to pass the fifth of the field the assert wants).
    let off = await p.paint();
    for (let turn = 0; turn < 25 && on.differs(open.box, off) === 0; turn += 1)
      off = await p.paint();
    const area = open.box.w * open.box.h;
    const diff = on.differs(open.box, off);
    const wash = on.count(open.box, open.wash);
    assert(area > 200, `the field's box is ${open.box.w}x${open.box.h}`);
    assert(diff > area * 0.2,
      `the selected frame differs from the collapsed one by ${diff}/${area}px `
      + "— the selection is set but not SEEN");
    assert(wash > area * 0.15,
      `only ${wash}/${area}px of the field wear ${open.wash} `
      + "— the value does not visibly carry the selection wash");

    // ESC CANCELS THE INPUT WHOLE, and the line is the bytes it was.
    await p.press("ESC");
    await p.until(() => !document.getElementById("ddate").classList.contains("on"),
                  "ESC to take the widget");
    const backTo = await planLine();
    assert(backTo === wasLine,
      `the cancelled line reads ${JSON.stringify(backTo)} against ${JSON.stringify(wasLine)}`);

    // ------- THE OTHER SUMMON KEY SWITCHES THE BOX THAT STANDS.  Two rungs in
    // one, and only a real engine has both: the entry comes up WHOLLY SELECTED
    // and a live selection is what makes `C-c' a copy, so the chord has to
    // PREFIX over a virgin widget at all; and what it opens is the ASKED
    // keyword's slot, the standing box having left exactly as ESC takes it.
    await p.press("C-c");
    await p.press("C-d");
    await widgetUp(p, "the DEADLINE widget again, to switch out of");
    const virgin = await p.eval(() => {
      const f = document.getElementById("dwhen");
      return { val: f.value, sel: [f.selectionStart, f.selectionEnd] };
    });
    assert(virgin.val === stood,
      `the box to switch out of holds ${JSON.stringify(virgin.val)}, not DEADLINE's`
      + ` ${JSON.stringify(stood)}`);
    assert(virgin.sel[0] === 0 && virgin.sel[1] === virgin.val.length,
      `it opened selecting ${virgin.sel[0]}..${virgin.sel[1]} of ${virgin.val.length}`);
    await p.press("C-c");
    await p.press("C-s");
    // THE FILL IS SYNCHRONOUS WITH THE PRESS, so this races nothing: a chord
    // that died leaves the DEADLINE box standing and is read here as its value.
    const swapped = await p.until(() => {
      const box = document.getElementById("ddate");
      const f = document.getElementById("dwhen");
      const line = document.querySelector('#mdoc .de[data-id="PLN"]');
      return box.classList.contains("on") && document.activeElement === f && line
        ? { val: f.value, line: line.textContent } : false;
    }, "a widget still standing after the second chord");
    const wantSched = ((await served("drv-plan")).plan
      .find(([k]) => k === "SCHEDULED") || [])[1];
    assert(swapped.val === wantSched,
      `the switched widget holds ${JSON.stringify(swapped.val)} where SCHEDULED `
      + `reads ${JSON.stringify(wantSched)}`);
    // AND A SWITCH WRITES NOTHING AND DRAWS NOTHING: the line is the bytes it was.
    assert(swapped.line === wasLine,
      `the switched-over line reads ${JSON.stringify(swapped.line)} against `
      + JSON.stringify(wasLine));
    // …AND IT STANDS IN THE SLOT IT NOW WRITES.  RELATIVE GEOMETRY, and WAITED
    // FOR: `placeEdit' lands a frame behind the switch, so a rect read once
    // would still measure the slot the box just left.
    const placed = await p.until(() => {
      const slot = document.querySelector('#mdoc .dpv[data-key="SCHEDULED"]');
      if (!slot) return false;
      const r = document.getElementById("dwhen").getBoundingClientRect();
      const s = slot.getBoundingClientRect();
      return r.width > 0 && s.width > 0 && Math.abs(r.left - s.left) <= 2
        ? { off: Math.round(r.left - s.left) } : false;
    }, "the switched box to land over the SCHEDULED slot");
    await p.press("ESC");
    await p.until(() => !document.getElementById("ddate").classList.contains("on"),
                  "ESC to take the switched widget");

    // ------- over a row with NO planning IN THE FILE: both settable slots stand
    // unset from the start, and the summon opens the widget over SCHEDULED.
    await sheet(p, base, "drv-prio");
    const before = await served("drv-prio");
    assert(before.plan.length === 0, "drv-prio already carries planning in the file");
    await settled(p);
    await p.press("C-c");
    await p.press("C-s");
    await widgetUp(p, "the widget over a SCHEDULED slot the file had not filled");
    // The port lands a macrotask behind the press and Elm paints a frame behind
    // that, so the line is WAITED for rather than read once.
    const drawn = await p.until(() => {
      const at = document.querySelector('#mdoc .de[data-id="PLN"]');
      return at ? at.textContent : false;
    }, "the planning line to draw its slots");
    assert(/SCHEDULED:/.test(drawn) && /DEADLINE:/.test(drawn),
      `the drawn line reads ${JSON.stringify(drawn)}`);
    // THE DRAFT JOINS NO LIST: nothing half-typed reaches the file.
    assert(JSON.stringify((await served("drv-prio")).plan) === "[]",
      "the ghosted keyword reached the file before a key was pressed");

    // THE ENGLISH PHRASE RESOLVES IN THE GHOST, before anything is written.
    await p.type("18 aug");
    const ghost = await p.until(() => {
      const s = document.getElementById("dghost");
      return s.textContent || false;
    }, "the ghost to resolve the phrase");
    // THE WEEKDAY IS COMPUTED and the year is the clock's, so the SHAPE is what
    // is read here — and the server's own answer is asserted against it below.
    assert(/^ → <\d{4}-08-18 (Mon|Tue|Wed|Thu|Fri|Sat|Sun)>$/.test(ghost),
      `the ghost reads ${JSON.stringify(ghost)}`);
    // AND IT RIDES THE FIELD'S OWN LINE, one space after what was typed, in the
    // mute ink — a SPAN and never the field's value, so no caret enters it.
    const rides = await p.eval(() => {
      const f = document.getElementById("dwhen").getBoundingClientRect();
      const s = document.getElementById("dghost");
      const r = s.getBoundingClientRect();
      return { gap: Math.round(r.left - f.right), sameLine: Math.abs(r.top - f.top) < 5,
               wide: r.width > 0, ink: rgb(getComputedStyle(s).color), mute: g("mute"),
               isField: s.tagName === "INPUT" || s.tagName === "TEXTAREA",
               typed: document.getElementById("dwhen").value };
    });
    assert(rides.sameLine && rides.wide && rides.gap >= -2 && rides.gap <= 3,
      `the ghost is not riding the field's line: ${JSON.stringify(rides)}`);
    assert(rides.ink === rides.mute, `the ghost is inked ${rides.ink}, not ${rides.mute}`);
    assert(!rides.isField, "the ghost is a field, so the caret can walk into it");
    // THE FIELD HOLDS WHAT WAS TYPED and never the resolution.
    assert(rides.typed === "18 aug", `the field holds ${JSON.stringify(rides.typed)}`);

    // RET SENDS THE RAW PHRASE; the SERVER resolves it and the pane redraws off
    // that answer.  What the ghost promised is what the file carries.
    await p.press("RET");
    const landed = await p.until(async () => {
      const h = await (await fetch("/headline?id=drv-prio")).json();
      const at = (h.planning || []).find(([k]) => k === "SCHEDULED");
      return at ? h : false;
    }, "the phrase to reach the planning line", 15000);
    const stamp = (landed.planning.find(([k]) => k === "SCHEDULED") || [])[1];
    assert(ghost === ` → ${stamp}`,
      `the server wrote ${JSON.stringify(stamp)} where the ghost promised `
      + JSON.stringify(ghost));
    // AND THE DRAWER UNDER IT IS UNTOUCHED: this write is the planning line's.
    assert(JSON.stringify(landed.properties || []) === JSON.stringify(before.props),
      `the drawer reads ${JSON.stringify(landed.properties)} `
      + `against ${JSON.stringify(before.props)}`);

    // ------- THE BRACKET IS THE ASK FOR AN ACTIVITY.  `[today]' is the clock
    // day INACTIVE — the one way this widget reaches org's other bracket — and
    // the whole grammar rides inside the pair.  READ RELATIVE TO THE DRIVEN
    // CLOCK: the server resolves against its own day, so the day is measured
    // rather than written down, and only the SHAPE is spelled here.
    await p.press("C-c");
    await p.press("C-s");
    await widgetUp(p, "the widget again, to type a bracketed phrase into");
    // The entry comes up WHOLLY SELECTED, so what is typed replaces it.
    await p.type("[today]");
    const bracket = await p.until(() => {
      const s = document.getElementById("dghost");
      return s.textContent || false;
    }, "the ghost to resolve the bracketed phrase");
    const clockDay = await p.eval(() => {
      const n = new Date(), pad = (x) => (x < 10 ? "0" : "") + x;
      return `${n.getFullYear()}-${pad(n.getMonth() + 1)}-${pad(n.getDate())}`;
    });
    assert(new RegExp(`^ → \\[${clockDay} (Mon|Tue|Wed|Thu|Fri|Sat|Sun)\\]$`).test(bracket),
      `the ghost reads ${JSON.stringify(bracket)} over "[today]" on ${clockDay}`);
    // AND THE INACTIVE STAMP IS WHAT LANDS ON DISK: the raw phrase travelled,
    // the server resolved it once, and the pane redrew off that answer.
    await p.press("RET");
    const inactive = await p.until(async () => {
      const h = await (await fetch("/headline?id=drv-prio")).json();
      const at = (h.planning || []).find(([k]) => k === "SCHEDULED");
      return at && at[1].charAt(0) === "[" ? at[1] : false;
    }, "the bracketed phrase to reach the planning line as an inactive stamp", 15000);
    assert(bracket === ` → ${inactive}`,
      `the server wrote ${JSON.stringify(inactive)} where the ghost promised `
      + JSON.stringify(bracket));
    // …AND THE PANE DREW THE BYTES: `SCHEDULED: [YYYY-MM-DD Day]' on the line.
    const inactiveLine = await p.until(() => {
      const at = document.querySelector('#mdoc .de[data-id="PLN"]');
      return at && at.textContent.indexOf("[") !== -1 ? at.textContent : false;
    }, "the pane to redraw the inactive stamp");
    assert(inactiveLine.indexOf(`SCHEDULED: ${inactive}`) !== -1,
      `the line reads ${JSON.stringify(inactiveLine)} against ${JSON.stringify(inactive)}`);

    // ------- THE WALK MOVES THE DAY AND NOTHING ELSE.  Shift+Arrow is a real
    // engine's chord and reaches this box nowhere else: the step carries the day
    // and leaves org's BRACKET standing, where a bare ISO written back would
    // drop the inactive intent the reader just spelled.  The day is COMPUTED off
    // what landed, the clock being the driven machine's.
    await p.press("C-c");
    await p.press("C-s");
    await widgetUp(p, "the widget over the inactive stamp, to walk it");
    const stoodOn = await p.eval(() => document.getElementById("dwhen").value);
    assert(stoodOn === inactive,
      `the walk opens over ${JSON.stringify(stoodOn)}, not ${JSON.stringify(inactive)}`);
    const DOW = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
    const pad = (x) => (x < 10 ? "0" : "") + x;
    const then = new Date(`${inactive.slice(1, 11)}T00:00:00Z`);
    then.setUTCDate(then.getUTCDate() + 1);
    const nextDay = `[${then.getUTCFullYear()}-${pad(then.getUTCMonth() + 1)}`
      + `-${pad(then.getUTCDate())} ${DOW[then.getUTCDay()]}]`;
    await p.press("S-<right>");
    const walked = await p.until((was) => {
      const f = document.getElementById("dwhen");
      return f.value === was ? false
        : { val: f.value, ghost: document.getElementById("dghost").textContent };
    }, "the shifted arrow to move the day", undefined, inactive);
    assert(walked.val === nextDay,
      `the step wrote ${JSON.stringify(walked.val)} where the day after `
      + `${JSON.stringify(inactive)} is ${JSON.stringify(nextDay)}`);
    // AND THE GHOST AGREES AFTER THE STEP: what the walk wrote is org's own
    // spelling, so the reader's own line has nothing added to it.
    assert(walked.ghost === "",
      `the ghost said ${JSON.stringify(walked.ghost)} over what the walk wrote`);
    // THE WALK WRITES NOTHING: ESC leaves, and the line is the bytes it was.
    await p.press("ESC");
    await p.until(() => !document.getElementById("ddate").classList.contains("on"),
                  "ESC to take the walked widget");
    const afterWalk = await planLine();
    assert(afterWalk === inactiveLine,
      `the cancelled walk left ${JSON.stringify(afterWalk)} against `
      + JSON.stringify(inactiveLine));
    return [`opened on ${JSON.stringify(open.val)} selected 0..${open.sel[1]}, `
      + `${diff}/${area}px of the field repainted and ${wash} wearing ${open.wash}; `
      + `C-c C-s over it switched to ${JSON.stringify(swapped.val)} `
      + `${placed.off}px off the SCHEDULED slot; `
      + `the slot drawn as ${JSON.stringify(drawn.trim())}, "18 aug" previewed `
      + `${JSON.stringify(ghost.trim())} and landed ${JSON.stringify(landed.planning)} `
      + `over ${JSON.stringify(landed.properties)}; "[today]" previewed `
      + `${JSON.stringify(bracket.trim())} and landed ${JSON.stringify(inactive)}; `
      + `S-<right> walked it to ${JSON.stringify(walked.val)}`];
  } },

// ORG SCHEDULES THE ENTRY AT POINT, AND THE STAMP LANDS ON THAT ENTRY.  Two
// faults met here: the summon ignored point, so `C-c C-s' on a CHILD row opened
// the ROOT's widget; and the widget's commit fired `set-planning', which
// addresses ROWS, so a child materialized by hand wrote its PARENT's planning
// line.  Both are only visible in the FILE, which is why this case reads the
// bytes on either side of the write — the pane drafted and previewed correctly
// throughout.  THE RUN PUTS THE TREE BACK: the entry is cleared through the same
// door, so the subtree is the bytes it was and the tree the other cases share is
// untouched.  READ BEHIND THE MIRROR'S OWN GUARDS: the port lands a macrotask
// behind the press and Elm paints a frame behind that (docs/bugs/…read-races-render).
{ name: "C-c C-s on a child schedules the CHILD, and the parent is untouched",
  async run(p, base) {
    /** The subtree as the server holds it, plus the two planning lists. */
    const served = () => p.eval(async () => {
      const root = await (await fetch("/headline?id=drv-marks")).json();
      const kid = await (await fetch("/headline?id=drv-marks&child=0")).json();
      return { org: root.org, plan: root.planning || [],
               kid: kid.planning || [], kidTitle: (kid.path || []).slice(-1)[0] };
    });
    await sheet(p, base, "drv-marks");
    const was = await served();
    assert(was.kid.length === 0,
      `drv-marks's first child already carries ${JSON.stringify(was.kid)}`);
    assert(was.plan.length > 0, "drv-marks carries no planning line of its own");

    // ------- THE SUMMON IS AT POINT: a child row materializes, then the box.
    await walkTo(p, ".d-child", "the first child headline");
    await settled(p);
    await p.press("C-c");
    await p.press("C-s");
    await widgetUp(p, "the widget over the child's SCHEDULED slot");
    // THE MATERIALIZE IS WAITED FOR BY ITS CRUMB, never assumed: the reread is
    // a fetch, and the summon rides its continuation.
    const trail = await p.until(() => {
      const c = [...document.querySelectorAll("#mwhere .wc")].map((e) => e.textContent);
      return c.length > 1 ? c : false;
    }, "the pane to stand in the child's own subtree");
    assert(trail.length === 2 && trail[1] === was.kidTitle,
      `the pane stands in ${JSON.stringify(trail)}, not the child's own subtree`);
    // THE CRUMB IS PLAIN DOM AND THE PANE IS ELM, so the trail says "child"
    // a frame before the rows do: the document is waited for BY ITS OWN
    // HEADLINE, or the planning line read below is still the parent's.
    await p.until((title) => {
      const head = document.querySelector('#mdoc .de[data-id="H"]');
      return !!head && head.textContent.includes(title);
    }, "the pane to redraw as the child's own document", undefined, was.kidTitle);
    const drew = await p.until(() => {
      const at = document.querySelector('#mdoc .de[data-id="PLN"]');
      return at ? at.textContent : false;
    }, "the child's planning line to draw its slots");
    assert(/SCHEDULED:/.test(drew) && /DEADLINE:/.test(drew),
      `the drawn line reads ${JSON.stringify(drew)}`);

    // ------- ESC LEAVES EVERY BYTE ALONE, the draft's own absence included.
    await p.type("18 aug");
    await p.press("ESC");
    await p.until(() => !document.getElementById("ddate").classList.contains("on"),
                  "ESC to take the widget");
    const escaped = await served();
    assert(escaped.org === was.org,
      "ESC moved bytes: the subtree is not the one the summon opened over");
    assert(JSON.stringify(escaped.kid) === "[]",
      `ESC left the child carrying ${JSON.stringify(escaped.kid)}`);
    // THE DRAFT GOES WITH THE BOX, waited for rather than read once: the undraft
    // is a port message and the pane repaints a frame behind it.  The SLOTS stay
    // -- they always draw -- but the child's SCHEDULED comes up VALUELESS again.
    await p.until(() => {
      const slot = document.querySelector('#mdoc .dpv[data-key="SCHEDULED"]');
      return !!slot && !/</.test(slot.textContent);
    }, "the ghosted value to leave with the box that drew it");

    // ------- AND THE COMMIT LANDS ON THE CHILD.  The pane is already the
    // child's, so this is the second fault on its own: the widget's own door.
    await settled(p);
    await p.press("C-c");
    await p.press("C-s");
    await widgetUp(p, "the widget again, over the child's own slot");
    await p.type("18 aug");
    const ghost = await p.until(() => {
      const s = document.getElementById("dghost");
      return s.textContent || false;
    }, "the ghost to resolve the phrase");
    await p.press("RET");
    const landed = await p.until(async () => {
      const kid = await (await fetch("/headline?id=drv-marks&child=0")).json();
      const at = (kid.planning || []).find(([k]) => k === "SCHEDULED");
      return at ? at[1] : false;
    }, "the phrase to reach the CHILD's planning line", 15000);
    assert(ghost === ` → ${landed}`,
      `the server wrote ${JSON.stringify(landed)} where the ghost promised `
      + JSON.stringify(ghost));
    const after = await served();
    // THE PARENT IS BYTE-IDENTICAL: its own planning list stands, and the whole
    // subtree differs by the ONE line the child gained and nothing else.
    assert(JSON.stringify(after.plan) === JSON.stringify(was.plan),
      `the root's own entries read ${JSON.stringify(after.plan)} `
      + `against ${JSON.stringify(was.plan)}`);
    const kidHead = was.org.split("\n").find((l) => l.includes(was.kidTitle));
    assert(!!kidHead, `no headline line spells ${JSON.stringify(was.kidTitle)}`);
    const owed = was.org.replace(`${kidHead}\n`, `${kidHead}\nSCHEDULED: ${landed}\n`);
    assert(after.org === owed,
      "the write moved a byte outside the child's own planning line");

    // ------- AND THE CLEAR RIDES THE SAME DOOR, which puts the tree back.
    await settled(p);
    await p.press("C-c");
    await p.press("C-s");
    await widgetUp(p, "the widget over the value that now stands");
    // THE ENTRY COMES UP WHOLLY SELECTED, so one DEL is the empty field the
    // shipped foot promises clears it.  The selection is WAITED for: it is
    // re-asserted while the widget is virgin, a redraw behind the open.
    await p.until(() => {
      const f = document.getElementById("dwhen");
      return f.value.length > 0 && f.selectionStart === 0
        && f.selectionEnd === f.value.length;
    }, "the standing value to come up wholly selected");
    await p.press("DEL");
    await p.until(() => document.getElementById("dwhen").value === "",
                  "the field to empty under DEL");
    await p.press("RET");
    const gone = await p.until(async () => {
      // A NON-200 IS NO ANSWER: while the store lags the write the materialize
      // refuses by digest, and an empty body would read as a cleared entry.
      const r = await fetch("/headline?id=drv-marks&child=0");
      if (!r.ok) return false;
      const kid = await r.json();
      return (kid.planning || []).length === 0 ? "cleared" : false;
    }, "the entry to leave the CHILD's planning line", 15000);
    const back = await served();
    assert(back.org === was.org,
      "the cleared subtree is not the bytes it started as");
    return [`the child row summoned into ${JSON.stringify(trail[1])} and drew `
      + `${JSON.stringify(drew.trim())}; "18 aug" previewed ${JSON.stringify(ghost.trim())} `
      + `and landed ${JSON.stringify(landed)} on the child with the root's `
      + `${JSON.stringify(was.plan)} untouched, ${gone} back to ${was.org.length} bytes`];
  } },

// THE PLANNING LINE'S OWN GRAIN.  `f' walks the entries the line draws, and the
// entry at point is the CURSOR one grain finer -- so it must wear the cursor's
// ground, and the line must lift its while it does, or the gold is drawn on gold
// and the reader sees a selection that is set and invisible (the widget's own
// `--g-sel' lesson, `page.css' "THE TWO GOLDS").  READ RELATIONALLY and behind
// the mirror's own guards: the port lands a macrotask behind the press and Elm
// paints a frame behind that (docs/bugs/…read-races-render).
{ name: "the planning line's entries walk, and RET opens the one at point",
  async run(p, base) {
    const LINE = '.de[data-id="PLN"]';
    const PLN = `#mdoc ${LINE}`;
    /** The planning line as the pane DREW it, or `""' where the row has none. */
    const planLine = () => p.eval((pln) => {
      const at = document.querySelector(pln);
      return at ? at.textContent : "";
    }, PLN);
    const entries = () => p.eval((pln) =>
      [...document.querySelectorAll(`${pln} .dpv`)].map((s) => s.dataset.key), PLN);
    /** `f' into the next entry, waited for BY THE KEYWORD point lands on: the
     * row never changes, so the row-id wait every other step uses answers yes
     * before the press is processed. */
    const into = (key) => p.until((pln, k) => {
      const at = document.querySelector(`${pln} .dpv.dat`);
      return !!at && at.dataset.key === k;
    }, `\`f' to reach the ${key} entry`, undefined, PLN, key);

    await sheet(p, base, "drv-plan");
    const wasLine = await planLine();
    const served = await p.eval(async () =>
      (await (await fetch("/headline?id=drv-plan")).json()).planning || []);
    await walkTo(p, LINE, "the planning line");
    await settled(p);
    const keys = await entries();
    assert(keys.length >= 2,
      `drv-plan's planning line carries ${JSON.stringify(keys)} — the walk needs two`);

    // TWO STEPS IN: the whole line, then the first entry, then the second.
    await p.press("f");
    await into(keys[0]);
    await p.press("f");
    await into(keys[1]);

    const dress = await p.eval((pln, k) => {
      const line = document.querySelector(pln);
      const on = line.querySelector(".dpv.dat");
      const off = [...line.querySelectorAll(".dpv")]
        .find((s) => !s.classList.contains("dat"));
      const bg = (e) => rgb(getComputedStyle(e).backgroundColor);
      const r = on.getBoundingClientRect();
      return { key: on.dataset.key, val: on.textContent, want: k,
               lit: bg(on), dark: bg(off), row: bg(line), sel: g("sel"),
               // THE PIXEL READING WANTS THE TOKEN'S OWN HEX, which is what a
               // frame carries; `rgb()' above is what a computed style reads as.
               wash: getComputedStyle(document.documentElement)
                 .getPropertyValue("--g-sel").trim().toLowerCase(),
               box: { x: Math.round(r.x), y: Math.round(r.y),
                      w: Math.round(r.width), h: Math.round(r.height) } };
    }, PLN, keys[1]);
    assert(dress.key === dress.want,
      `the dress landed on ${dress.key} where the walk stood in ${dress.want}`);
    assert(dress.lit === dress.sel,
      `the entry at point wears ${dress.lit}, not the cursor's ${dress.sel}`);
    assert(/rgba\(0, 0, 0, 0\)|transparent/.test(dress.dark),
      `an entry point is not in wears ${dress.dark}`);
    // …AND THE LINE LIFTS ITS OWN, or the entry's gold is drawn on the same gold.
    assert(/rgba\(0, 0, 0, 0\)|transparent/.test(dress.row),
      `the line still wears ${dress.row} under a picked entry — gold on gold`);
    // SEEN, not merely set: the wash is counted in PIXELS over the slot's box.
    const area = dress.box.w * dress.box.h;
    assert(area > 100, `the entry's box is ${dress.box.w}x${dress.box.h}`);
    const wash = (await p.paint()).count(dress.box, dress.wash);
    assert(wash > area * 0.15,
      `only ${wash}/${area}px of the entry wear ${dress.wash} — the dress is not SEEN`);

    // RET OVER THE ENTRY IS THE SUMMON KEY'S OWN BOX, keyed by that entry.
    await p.press("RET");
    await p.until(() => {
      const box = document.getElementById("ddate");
      const f = document.getElementById("dwhen");
      return box.classList.contains("on") && document.activeElement === f
        && f.getBoundingClientRect().width > 0 && box.style.top !== "";
    }, `the widget over the ${keys[1]} value`);
    const open = await p.eval((pln) => {
      const f = document.getElementById("dwhen");
      const slot = document.querySelector(`${pln} .dpv.dat`)
        .getBoundingClientRect();
      const r = f.getBoundingClientRect();
      return { val: f.value, sel: [f.selectionStart, f.selectionEnd],
               // THE BOX STANDS IN THE ENTRY'S OWN SLOT, not the line's head.
               off: Math.round(r.left - slot.left) };
    }, PLN);
    assert(open.val === dress.val,
      `the widget opened holding ${JSON.stringify(open.val)} where the entry `
      + `reads ${JSON.stringify(dress.val)}`);
    assert(open.sel[0] === 0 && open.sel[1] === open.val.length,
      `it opened selecting ${open.sel[0]}..${open.sel[1]} of ${open.val.length}`);
    assert(Math.abs(open.off) <= 2,
      `the field stands ${open.off}px off the entry's own slot`);

    // ESC LEAVES THE LINE THE BYTES IT WAS, on screen and on disk alike.
    await p.press("ESC");
    await p.until(() => !document.getElementById("ddate").classList.contains("on"),
                  "ESC to take the widget");
    const backTo = await planLine();
    assert(backTo === wasLine,
      `the cancelled line reads ${JSON.stringify(backTo)} against ${JSON.stringify(wasLine)}`);
    const after = await p.eval(async () =>
      (await (await fetch("/headline?id=drv-plan")).json()).planning || []);
    assert(JSON.stringify(after) === JSON.stringify(served),
      `the file reads ${JSON.stringify(after)} against ${JSON.stringify(served)}`);
    return [`walked ${JSON.stringify(keys)} to ${dress.key}, `
      + `${wash}/${area}px of its slot wearing ${dress.wash} over a line lifted to `
      + `${dress.row}; RET opened on ${JSON.stringify(open.val)} `
      + `${open.off}px off the slot, ESC left ${JSON.stringify(after)}`];
  } },

// THE CAPTURE DOC IS THE MATERIAL DOC.  `+' asks the destination, the server
// expands that tag's template into a DRAFT, and the sheet opens over it: the
// same pane and the same doors, over a subtree that does not exist yet.  AGAINST
// A REAL TREE AND A REAL STORE — the node harness serves a fabricated draft;
// here the template is a config layer ON DISK and the blob is a FILE at the end,
// so what these read is the org a reader's own keystrokes wrote.
{ name: "a tagged capture opens as a document, and C-c C-c mints the blob it drew",
  async run(p, base) {
    await tableUp(p, base);
    const before = await idsUnder(p, "tag:book");
    assert(before.length === 0,
      `the tree already holds ${JSON.stringify(before)} under :book:`);
    await draftOver(p, base, "book");

    // ------- THE TEMPLATE ARRIVED EXPANDED: a state off the tag's OWN cycle
    // (`#+TODO:' rides the very file the template does), a stamp the SERVER read
    // its clock for, and a `%^{PROMPT}' opened as the pair with nothing in it.
    const drew = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return {
        file: document.getElementById("mfile").textContent,
        note: document.getElementById("mnote").textContent.trim(),
        head: document.querySelector('#mdoc .de[data-id="H"]').textContent,
        tags: (document.querySelector('#mdoc .de[data-id="H"] .dc-tags')
                 || {}).textContent || "",
        plan: (document.querySelector('#mdoc .de[data-id="PLN"]') || {}).textContent || "",
        // THE FRAME ALONE, past the spine's `.fold' sign: the drawer's own line.
        drawer: ((d) => d ? [...d.querySelectorAll(".dg")].map((g) => g.textContent).join("") : "")(
          document.querySelector("#mdoc .d-drawer")),
        // WHERE `%?' STOOD, spent as the fill's own landing: without it point
        // would rest on row 0, the headline every other fill opens on.
        atClass: at.className, atText: at.textContent, atIndex: docAtNow(),
        // EVERY DRAFT OPENS EDITING, and this one's `%?' stood in the BODY.
        para: document.getElementById("dpara").className,
        slot: document.getElementById("dtext").value,
        focus: document.activeElement.id,
        rows: [...document.querySelectorAll("#mdoc .de.lvl-top")].length,
      };
    });
    assert(drew.file === "the capture  ·  :book:",
      `the sheet's file line reads ${JSON.stringify(drew.file)} — it names no file `
      + `and no id, there being neither yet`);
    assert(/READING/.test(drew.head) && /Book/.test(drew.head),
      `the drawn headline reads ${JSON.stringify(drew.head)}`);
    // THE DRAFT SAYS WHERE IT LANDS: the destination is a CONSTRUCTED cell, so
    // the head row wears it whether or not the org line could carry the run.
    assert(drew.tags === ":book:",
      `the head row's tag cell reads ${JSON.stringify(drew.tags)} — the reader is `
      + `composing into :book: and the pane must say so`);
    assert(/^SCHEDULED: <\d{4}-\d\d-\d\d/.test(drew.plan.trim()),
      `the planning line reads ${JSON.stringify(drew.plan)}`);
    assert(/^:PROPERTIES:/.test(drew.drawer.trim()),
      `the drawer reads ${JSON.stringify(drew.drawer)}`);
    // A DRAFT IS NEVER DIRTY: nothing about it is owed to a file, which is the
    // whole of why ESC is free.
    assert(/^synced/.test(drew.note),
      `the sheet's note reads ${JSON.stringify(drew.note)}`);
    assert(/\bd-para\b/.test(drew.atClass) && !/\bd-head\b/.test(drew.atClass),
      `point opened on ${JSON.stringify(drew.atClass)} rather than the slot after %?`);

    // ------- THE BODY, TYPED WHERE POINT OPENED — AND THE EDITOR WAS ALREADY
    // THERE.  Every draft opens EDITING: a reader who asked for a capture is
    // composing one, and a `RET' the pane makes them press first asks a question
    // `+' already answered.  A draft's cargo is HELD: the model takes the edit
    // and nothing is posted, there being no file to post to.
    assert(/^Why this one:/.test(drew.atText),
      `point opened on ${JSON.stringify(drew.atText)} rather than the %? slot`);
    assert(drew.para === "on" && drew.focus === "dtext",
      `the draft opened ${JSON.stringify([drew.para, drew.focus])} rather than in `
      + `the editor over the line %? stood on`);
    assert(/^Why this one:\s*$/.test(drew.slot),
      `the slot seeded ${JSON.stringify(drew.slot)} — %? expands to nothing at all`);
    await p.eval(() => {
      document.getElementById("dtext").value = "Why this one: the spice must flow";
    });
    await p.press("RET");
    await p.until(() => !document.getElementById("dpara").classList.contains("on"),
                  "the edit to close over the typed body");
    const held = await idsUnder(p, "tag:book");
    assert(held.length === 0,
      `an edit over a DRAFT reached the store: ${JSON.stringify(held)}`);

    // ------- THE PAIR, FILLED THROUGH THE DOOR THE PANE ALREADY HAS.  This is
    // the whole of what `%^{Author}' dissolved into.
    await walkBackTo(p, ".d-drawer", "the drawer");
    await p.press("f");
    await p.until(() => {
      const at = document.querySelector("#mdoc .de.dat");
      return !!at && at.classList.contains("d-meta") && /AUTHOR/.test(at.textContent);
    }, "f to open the drawer and land on the ask it drew");
    await settled(p);
    await p.press("RET");
    await editUp(p, "the pair's own line edit");
    const seeded = await p.eval(() => document.getElementById("dtext").value);
    assert(/^:AUTHOR:\s*$/.test(seeded),
      `the ask seeded ${JSON.stringify(seeded)} rather than an empty value`);
    await p.eval(() => { document.getElementById("dtext").value = ":AUTHOR: Herbert"; });
    await p.press("RET");
    await p.until(() => {
      const d = document.querySelector("#mdoc .d-drawer");
      return !!d && /Herbert/.test(d.textContent);
    }, "the filled pair to draw in the drawer");

    // ------- THE DATE WIDGET OVER THE DRAFT'S OWN PLANNING LINE.  A draft has
    // no row id, so the entry takes the COMMIT door the materialized child's
    // does: the model holds it and the capture command is what writes it.
    //
    // AND THE DRAFT RESOLVES IT ITSELF.  A row's phrase is posted and comes back
    // TRANSFORMED; a draft posts nothing, so the preview is made flesh in the
    // line — the ghost's own reading, drift-pinned to the wall — while the RAW
    // phrase is still what travels at `C-c C-c'.
    await settled(p);
    await p.press("C-c");
    await p.press("C-s");
    await widgetUp(p, "the widget over the draft's SCHEDULED slot");
    await p.until(() => {
      const f = document.getElementById("dwhen");
      return f.value.length > 0 && f.selectionStart === 0
        && f.selectionEnd === f.value.length;
    }, "the stamp the template expanded to, wholly selected");
    await p.type("18 aug");
    const ghost = await p.until(() => {
      const s = document.getElementById("dghost");
      return s.textContent || false;
    }, "the ghost to resolve the phrase");
    await p.press("RET");
    const drafted = await p.until(() => {
      const at = document.querySelector('#mdoc .de[data-id="PLN"]');
      return at && /<\d{4}-08-18 /.test(at.textContent) ? at.textContent.trim() : false;
    }, "the phrase to stand RESOLVED in the draft's own planning line");
    assert(!/18 aug/.test(drafted),
      `the planning line still reads the raw phrase: ${JSON.stringify(drafted)}`);
    assert(drafted.includes(ghost.replace(/^\s*→\s*/, "").trim()),
      `the line reads ${JSON.stringify(drafted)} and the ghost previewed `
      + `${JSON.stringify(ghost)} — one reader, one answer`);
    const still = await idsUnder(p, "tag:book");
    assert(still.length === 0,
      `the widget wrote to the store over a draft: ${JSON.stringify(still)}`);

    // ------- `C-c C-c' COMMITS THE WHOLE THING through the one door that mints
    // a blob: its id, its shard path, its creation drawer and its ledger line.
    await p.press("C-c");
    await p.press("C-c");
    const landed = await p.until(async () => {
      const r = await fetch("/headlines?limit=500&q=tag%3Abook");
      const j = await r.json();
      const rows = (j.view && j.view.rows) || j.rows || [];
      return rows.length === 1 ? rows[0].id : false;
    }, "the minted blob to reach the store", 20000);
    const org = await p.eval(async (id) =>
      (await (await fetch(`/headline?id=${encodeURIComponent(id)}`)).json()).org, landed);
    const lines = org.split("\n");
    // EVERY PART, AND THE MINTING'S OWN TWO BESIDE THEM.  THE DESTINATION IS
    // WORN ONCE: the draft's tag cell carried it out as the capture's `tags',
    // and the minting joins it IDEMPOTENTLY (`addTagEditsIn' folds), so the run
    // that reached the file is the run the pane drew.
    assert(/^\* READING Book :book:$/.test(lines[0]),
      `the blob opens ${JSON.stringify(lines[0])}`);
    assert((lines[0].match(/book/g) || []).length === 1,
      `the destination is worn twice: ${JSON.stringify(lines[0])}`);
    assert(/^SCHEDULED: <\d{4}-08-18 /.test(lines[1]),
      `the planning line reads ${JSON.stringify(lines[1])}`);
    assert(org.includes(`:ORG_GLANCE_ID: ${landed}`),
      `the drawer does not carry the minted id: ${JSON.stringify(org)}`);
    assert(/:ORG_GLANCE_CREATION_TIME: \[/.test(org),
      `the creation stamp is missing: ${JSON.stringify(org)}`);
    assert(org.includes(":AUTHOR: Herbert"),
      `the filled ask never reached the file: ${JSON.stringify(org)}`);
    // THE WHOLE BODY, the line point opened in and the one the template left
    // standing under it — `body' is everything UNDER the headline, children and all.
    assert(org.includes("Why this one: the spice must flow") && org.includes("Notes:"),
      `the body is not the one the pane held: ${JSON.stringify(org)}`);

    // ------- AND THE CURSOR LANDS ON THE ROW THE CAPTURE MADE.
    const sel = await p.until((id) => {
      const tr = document.querySelector("#app tr.tv-sel");
      return tr && tr.dataset.id === id ? tr.textContent.trim() : false;
    }, "point to land on the row the watch delivered", 20000, landed);
    const shut = await p.eval(() => document.getElementById("modal").className);
    assert(shut !== "on", "the sheet is still up over a capture that landed");
    return [`the ${JSON.stringify(drew.head.trim())} draft opened at row `
      + `${drew.atIndex} (${JSON.stringify(drew.atText)}), its ask empty; `
      + `"18 aug" ghosted ${JSON.stringify(ghost.trim())} and stood as `
      + `${JSON.stringify(drafted)} with the store still empty; C-c C-c minted `
      + `${landed} — ${JSON.stringify(lines.slice(0, 2))} — and point landed on `
      + `${JSON.stringify(sel.slice(0, 40))}`];
  } },

// THE BARE-DRAFT LAW: the default template is star-space and nothing else, so
// the title edit IS the capture and the four-key inbox jot is byte for byte the
// gesture it always was — `+', RET, the line, RET.
{ name: "the inbox jot is + RET the line RET, and lands the bytes it always did",
  async run(p, base) {
    await draftOver(p, base, "");
    const open = await p.eval(() => ({
      box: document.getElementById("dtitle").className,
      focus: document.activeElement.id,
      file: document.getElementById("mfile").textContent,
      head: document.querySelector('#mdoc .de[data-id="H"]').textContent,
    }));
    assert(open.box === "on" && open.focus === "dtin",
      `the bare draft opened ${JSON.stringify(open)} rather than in its title`);
    assert(/the inbox/.test(open.file),
      `the sheet's file line reads ${JSON.stringify(open.file)}`);
    assert(open.head.trim() === "*",
      `the bare draft drew ${JSON.stringify(open.head)} rather than star-space`);

    await p.type("buy milk");
    await p.press("RET");
    const landed = await p.until(() => {
      const tr = [...document.querySelectorAll("#app tbody tr")]
        .find((r) => /buy milk/.test(r.textContent));
      return tr ? tr.dataset.id : false;
    }, "the jot to reach the table", 20000);
    const org = await p.eval(async (id) =>
      (await (await fetch(`/headline?id=${encodeURIComponent(id)}`)).json()).org, landed);
    const lines = org.trimEnd().split("\n")
      .filter((l) => !/^:ORG_GLANCE_CREATION_TIME:/.test(l));
    // NO ID, NO TAG AND NO LEDGER LINE: the inbox files a jot, it mints no identity.
    assert(JSON.stringify(lines) === JSON.stringify(
        ["* buy milk", ":PROPERTIES:", ":END:"]),
      `the jot wrote ${JSON.stringify(org)}`);
    assert(!/:ORG_GLANCE_ID:/.test(org),
      `the inbox minted an identity: ${JSON.stringify(org)}`);
    assert(/:ORG_GLANCE_CREATION_TIME: \[/.test(org),
      `the creation stamp is missing: ${JSON.stringify(org)}`);
    const sel = await p.until((id) => {
      const tr = document.querySelector("#app tr.tv-sel");
      return tr && tr.dataset.id === id;
    }, "point to land on the jot", 20000, landed);
    return [`+ RET "buy milk" RET wrote ${JSON.stringify(lines)} into `
      + `${landed}, point on it: ${sel}`];
  } },

// A CAPTURE IS COMMITTED OR IT NEVER WAS.  No file is created when the draft
// opens, so there is nothing for ESC to undo: the born-at-open memory is
// trivially empty, and this is what proves it against a real store.
{ name: "ESC over a draft leaves nothing behind, on screen or on disk",
  async run(p, base) {
    await tableUp(p, base);
    const all = () => idsUnder(p, "");
    const before = await all();

    // (1) OVER A DRAWN DRAFT: the STANDING LADDER, one rung per surface — the
    // first ESC closes the box the landing opened and the second takes the
    // sheet.  No command was sent at either.
    await draftOver(p, base, "book");
    await p.press("ESC");
    await p.until(() => !document.getElementById("dpara").classList.contains("on")
                    && document.getElementById("modal").className === "on",
                  "ESC to close the landing's box and leave the sheet standing");
    await p.press("ESC");
    await p.until(() => document.getElementById("modal").className !== "on",
                  "ESC to take the sheet over the templated draft");
    const afterRich = await all();
    assert(JSON.stringify(afterRich) === JSON.stringify(before),
      `ESC over a templated draft moved the store: `
      + `${JSON.stringify([before.length, afterRich.length])}`);

    // (2) IN THE BARE DRAFT'S TITLE, where the box IS the capture: ESC drops the
    // whole thing rather than coming back to a sheet standing over no file.
    await draftOver(p, base, "");
    await p.type("never captured");
    await p.press("ESC");
    await p.until(() => document.getElementById("modal").className !== "on"
                    && document.getElementById("dtitle").className !== "on",
                  "ESC to take the title box and the sheet together");
    const afterBare = await all();
    assert(JSON.stringify(afterBare) === JSON.stringify(before),
      `ESC in the bare title moved the store: `
      + `${JSON.stringify([before.length, afterBare.length])}`);
    const gone = await p.eval(async () => {
      const r = await fetch("/headlines?limit=500&q=" + encodeURIComponent("never"));
      const j = await r.json();
      return ((j.view && j.view.rows) || j.rows || []).length;
    });
    assert(gone === 0, `the typed title reached ${gone} row(s) all the same`);

    // (3) AND AT THE TAG FIELD, before there is a draft at all.
    await captureForm(p, base);
    await p.press("ESC");
    await p.until(() => document.getElementById("capture").className !== "on"
                    && document.getElementById("modal").className !== "on",
                  "ESC to take the tag field with no sheet behind it");
    const end = await all();
    assert(JSON.stringify(end) === JSON.stringify(before),
      `the three escapes left ${end.length} rows against ${before.length}`);
    return [`three escapes — over the draft, in the bare title, at the tag `
      + `field — left the store at ${before.length} rows and nothing on screen`];
  } },

// AN EMPTY TITLE DREW NO CELL, so the title edit fell back to the whole line and
// the box swallowed the star, the state, the priority and the tag run it is
// supposed to stand among.  The capture surfaced it — a fresh draft has no title
// by definition — but the bug is the SHEET'S: any headline whose title is empty
// wore it.  The headline now always draws its title cell, and the box stands in
// that slot on both.
{ name: "an empty title still has a slot, and its edit stands tight in it",
  async run(p, base) {
    // `placeEdit' LAYS THE BOX A TURN AFTER THE RAISE: read before that and it
    // still stands where it rested, which is nobody's slot.
    const titlePlaced = (p_, why) =>
      p_.until(() => {
        const b = document.getElementById("dtitle");
        return b.classList.contains("on") && b.style.top !== ""
          && b.getBoundingClientRect().width > 0
          && !!document.querySelector('#mdoc .de[data-id="H"] .dc-title');
      }, why);
    const read = () => {
      const box = (e) => {
        if (!e || !e.getBoundingClientRect) return null;
        const r = e.getBoundingClientRect();
        return { x: Math.round(r.left), w: Math.round(r.width),
                 t: Math.round(r.top), h: Math.round(r.height) };
      };
      const head = document.querySelector('#mdoc .de[data-id="H"]');
      const t = document.getElementById("dtitle");
      return {
        head: head ? head.textContent : "",
        row: box(head),
        // WHAT STANDS LEFT OF THE SLOT: the stars and every cell org spells
        // before the title.  The tag run rides the far edge and is read apart.
        before: [...head.querySelectorAll(".ds, .dc")]
          .filter((s) => !s.classList.contains("dc-title")
                      && !s.classList.contains("dc-tags") && s.textContent)
          .map((s) => [s.className, s.textContent.trim(), box(s)]),
        title: box(head.querySelector(".dc-title")),
        tags: box(head.querySelector(".dc-tags")),
        open: t.className, field: document.activeElement.id, edit: box(t),
      };
    };
    // The box stands in the title's own slot: over the cell, and clear of every
    // decoration the row draws beside it.
    const tight = (s, what) => {
      assert(s.title !== null,
        `${what}: the head row drew no title cell — ${JSON.stringify(s.head)}`);
      assert(s.open === "on" && s.field === "dtin",
        `${what}: the title edit is ${JSON.stringify([s.open, s.field])}`);
      assert(Math.abs(s.edit.x - s.title.x) <= 2,
        `${what}: the box opens at x${s.edit.x} and the slot at x${s.title.x}`);
      // THE VERTICAL IS THE ROW'S: an empty slot's own rect sat on the
      // baseline at zero height, and the box must never follow it there.
      assert(s.edit.t - s.row.t >= 0 && s.edit.t - s.row.t <= 3,
        `${what}: the box opens at y${s.edit.t} over a row at y${s.row.t} — `
        + `it dropped to the baseline`);
      assert(s.edit.h >= 18,
        `${what}: the box is ${s.edit.h}px tall — it must cover the line`);
      assert(s.edit.w < s.row.w - 8,
        `${what}: the box is ${s.edit.w}px over a ${s.row.w}px row — it swallowed it`);
      for (const [cls, text, b] of s.before)
        assert(b.x + b.w <= s.edit.x + 2,
          `${what}: ${cls} ${JSON.stringify(text)} at x${b.x}+${b.w} is under the `
          + `box at x${s.edit.x}`);
      if (s.tags)
        assert(s.tags.x >= s.edit.x + s.edit.w - 2,
          `${what}: the tag run at x${s.tags.x} is under the box ending at `
          + `x${s.edit.x + s.edit.w}`);
    };

    // ------- ON A DRAFT, which is where a reader meets it: the filter pins a
    // priority, so the draft is RICH and the box that opens is not the jot's.
    await tableUp(p, base);
    await p.goto(`${base}/?q=${encodeURIComponent("priority:[#A]")}`);
    await p.until(() => !!document.querySelector("#app table tbody tr"),
                  "the filtered table");
    await p.press("+");
    await p.until(() => document.getElementById("capture").className === "on"
                    && document.activeElement === document.getElementById("ktag"),
                  "the tag field");
    await p.eval(() => {
      const f = document.getElementById("ktag");
      f.value = "bicycle";
      f.dispatchEvent(new Event("input", { bubbles: true }));
    });
    await p.press("RET");
    await p.until(() => !!document.querySelector("#modal.on")
                    && document.getElementById("capture").className !== "on",
                  "the sheet over the bicycle draft");
    await titlePlaced(p, "the title box over the draft's empty slot");
    const draft = await p.eval(read);
    // THE WHOLE OF WHAT THE READER LOST: the priority the filter lent and the
    // destination they typed both stand, around an editable slot.
    assert(/\[#A\]/.test(draft.head) && /:bicycle:/.test(draft.head),
      `the draft drew ${JSON.stringify(draft.head)} — the lent priority and the `
      + `destination must both be on it`);
    tight(draft, "the draft");
    await p.press("ESC");
    await p.until(() => document.getElementById("dtitle").className !== "on",
                  "ESC to close the draft's title box");
    await p.press("ESC");
    await p.until(() => document.getElementById("modal").className !== "on",
                  "ESC to drop the capture");

    // ------- AND ON A REAL DOC whose title is empty, which is the same bug and
    // was there before capture was: the row is made through the capture door and
    // then emptied through the commit door, so nothing here fabricates markup.
    const id = await p.eval(async () => {
      const made = await (await fetch("/command", {
        method: "POST", headers: { "content-type": "application/json" },
        body: JSON.stringify({ name: "capture",
                               args: { text: "TODO a title about to go" } }) })).json();
      return made.id;
    });
    const wrote = await p.until(async (row) => {
      const h = await (await fetch(`/headline?id=${encodeURIComponent(row)}`)).json();
      if (!/a title about to go/.test(h.org || "")) return false;
      const r = await fetch(`/headline?id=${encodeURIComponent(row)}`, {
        method: "POST", headers: { "content-type": "application/json" },
        body: JSON.stringify({ org: h.org.replace("* TODO a title about to go", "* TODO"),
                               digest: h.digest }) });
      return r.status === 200 || `the write answered ${r.status}`;
    }, "the row to be written with an empty title", 20000, id);
    assert(wrote === true, `the empty-title row was never written: ${wrote}`);
    // THE READ THAT PROVES IT, before the sheet is asked for one: the store lags
    // the write, and a sheet opened inside that window draws the old title.
    const bare = await p.until(async (row) => {
      const h = await (await fetch(`/headline?id=${encodeURIComponent(row)}`)).json();
      const line = String(h.org || "").split("\n")[0];
      return /^\* TODO\s*$/.test(line) ? line : false;
    }, "the emptied title to be what the door serves", 20000, id);
    assert(bare === "* TODO", `the row's headline reads ${JSON.stringify(bare)}`);
    // ENCODED, and over the whole tree: a fallback row id is a PATH carrying a
    // `#', which a bare URL would cut off as a fragment.
    await p.goto(`${base}/?q=&page=sheet&row=${encodeURIComponent(id)}`);
    await p.until(() => !!document.querySelector("#modal.on"),
                  "the sheet over the emptied row");
    await p.until(() => !!document.querySelector('#mdoc .de[data-id="H"]'),
                  "its head row to draw");
    await settled(p);
    await p.press("RET");
    await titlePlaced(p, "the title box over the real doc's empty slot");
    const real = await p.eval(read);
    assert(/TODO/.test(real.head),
      `the row drew ${JSON.stringify(real.head)} — its state must still stand`);
    tight(real, "a real doc with an empty title");
    return [`the draft drew ${JSON.stringify(draft.head)}: slot x${draft.title.x}`
      + ` w${draft.title.w}, box x${draft.edit.x} w${draft.edit.w} inside a `
      + `${draft.row.w}px row; the real doc drew ${JSON.stringify(real.head)}: `
      + `slot x${real.title.x} w${real.title.w}, box x${real.edit.x} w${real.edit.w}`];
  } },

// PROPOSAL 2026-08-26.  An org table in the doc is drawn by the table-view
// renderer inside its own block: one <glance-table> host, the composite and its
// item leaves still Elm's stops.
{ name: "an org table in the material doc mounts a table-view widget",
  async run(p, base) {
    await sheet(p, base, "drv-table");
    const t = await p.eval(() => {
      const host = document.querySelector("#mdoc glance-table");
      const tv = host && host.querySelector(".tv-table");
      const rows = tv ? [...tv.querySelectorAll("tbody tr[data-id]")] : [];
      const cells = rows.map((tr) =>
        [...tr.querySelectorAll("td")].map((td) => td.textContent.trim()));
      const link = tv ? tv.querySelector("a.tv-link") : null;
      const heads = tv
        ? [...tv.querySelectorAll("thead th")].map((th) => th.textContent.trim())
        : [];
      return { host: !!host, table: !!tv, nrows: rows.length,
               first: cells[0] || [], heads,
               link: link ? link.textContent.trim() : null };
    });
    assert(t.host, "no <glance-table> host rendered for the org table");
    assert(t.table, "the host mounted no table-view (.tv-table)");
    assert(t.nrows === 3, `the table drew ${t.nrows} data rows, not 3`);
    assert(t.first[0] === "Klarenbeekstraat",
      `the first cell reads ${JSON.stringify(t.first[0])}, not the street`);
    assert(t.heads[0] === "Street",
      `the first header reads ${JSON.stringify(t.heads[0])}, not "Street"`);
    assert(t.link === "see the plot",
      `the link cell shows ${JSON.stringify(t.link)}, not its description`);
    return [`org table mounts a table-view: ${t.nrows} rows under `
      + `[${t.heads.join(", ")}], link "${t.link}"`];
  } },
];
