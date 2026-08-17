// The stage every variant is judged on: ONE fixture, ONE keymap, ONE set of
// metrics.  A variant brings its own CSS and one `paint' hook and nothing else,
// so what differs between two tabs is the look and never the rig.
//
// A CLASSIC SCRIPT ON PURPOSE: a module over `file://' is an opaque origin and
// will not load, and every page here must open by double-clicking it.
"use strict";

var RIG = (function () {
  // ---------------------------------------------------------------- fixture
  // An item is [text, kids].  Depth is the position in this tree; the rendered
  // line carries its own two spaces per level, which is where the indent lives.
  var DOC = {
    head: "* Refer picker",
    lead: "Three rehearsals, one of which shipped. What follows is what the\n" +
          "harness had to learn before any of them could be judged.",
    list: [
      ["harness", [
        ["drive.mjs speaks CDP and depends on nothing", [
          ["an editing key needs keyDown, not rawKeyDown", []],
          ["a URL differing only in its #hash is one document", [
            ["so the runner adds a per-case query", []],
          ]],
        ]],
        ["cases.mjs mutates one tree, in order", []],
      ]],
      ["picker", [
        ["the kind stage", [
          ["a word that is not a kind IS the new kind", []],
          ["ESC walks out one rung", [
            ["and gives the kind up with it", []],
          ]],
        ]],
        ["the row stage", [
          ["/ opens the filter box", []],
          ["RET takes the row under the cursor", []],
        ]],
      ]],
      ["theme", [
        ["the hue is per theme, never per tree", []],
      ]],
    ],
    tail: "A paragraph has no depth, so nothing is drawn beside it.",
  };

  // ------------------------------------------------------------------ model
  // A ROW IS A STOP.  `kids' is what `f' descends into, `up' what `b' climbs to.
  function row(el, up, depth) {
    return { el: el, up: up, kids: [], depth: depth };
  }

  var state = { at: null, roots: [], mdoc: null, paint: null, ch: 8, lh: 16 };

  function itemEl(text, depth) {
    var d = document.createElement("div");
    d.className = "de d-item";
    d.dataset.depth = String(depth);
    var own = document.createElement("div");
    own.className = "dp";
    own.textContent = new Array(depth + 1).join("  ") + "- " + text;
    d.appendChild(own);
    return d;
  }

  function buildItems(spec, host, up, depth) {
    return spec.map(function (it) {
      var el = itemEl(it[0], depth);
      host.appendChild(el);
      var r = row(el, up, depth);
      r.kids = buildItems(it[1], el, r, depth + 1);
      return r;
    });
  }

  function plain(cls, text) {
    var d = document.createElement("div");
    d.className = "de " + cls;
    d.textContent = text;
    return d;
  }

  function headEl(text) {
    var d = document.createElement("div");
    d.className = "de d-head";
    var stars = text.slice(0, text.indexOf(" ") + 1);
    var ds = document.createElement("span");
    ds.className = "ds";
    ds.textContent = stars.trimEnd();
    var title = document.createElement("span");
    title.textContent = text.slice(stars.length);
    d.appendChild(ds);
    d.appendChild(title);
    return d;
  }

  function build(mdoc) {
    var head = headEl(DOC.head);
    var lead = plain("d-para", DOC.lead);
    var list = document.createElement("div");
    list.className = "de d-comp d-list";
    var tail = plain("d-para", DOC.tail);
    mdoc.appendChild(head);
    mdoc.appendChild(lead);
    mdoc.appendChild(list);
    mdoc.appendChild(tail);

    var rHead = row(head, null, 0);
    var rLead = row(lead, null, 0);
    var rList = row(list, null, 0);
    var rTail = row(tail, null, 0);
    rList.kids = buildItems(DOC.list, list, rList, 0);
    // THE LIST IS ONE STOP at the coarse grain: `n' skips it whole and `f' is
    // what goes in, which is why its items hang off it rather than beside it.
    return [rHead, rLead, rList, rTail];
  }

  // ----------------------------------------------------------------- moving
  function siblings(r) { return r.up ? r.up.kids : state.roots; }

  function step(d) {
    var sib = siblings(state.at);
    var i = sib.indexOf(state.at) + d;
    if (i >= 0 && i < sib.length) go(sib[i]);
  }

  function finer() { if (state.at.kids.length) go(state.at.kids[0]); }
  function broader() { if (state.at.up) go(state.at.up); }

  function go(r) {
    if (state.at) state.at.el.classList.remove("dat");
    state.at = r;
    r.el.classList.add("dat");
    r.el.scrollIntoView({ block: "nearest" });
    repaint();
  }

  // ---------------------------------------------------------- what a variant reads
  // The chain is OUTERMOST FIRST and ends at the cursor, so `chain[n-2]' is the
  // block the cursor is inside — the one question a guide has to answer.
  function chain() {
    var out = [], r = state.at;
    while (r) { out.unshift(r); r = r.up; }
    return out;
  }

  function px(el) {
    var p = state.mdoc.getBoundingClientRect(), b = el.getBoundingClientRect();
    return { top: b.top - p.top + state.mdoc.scrollTop,
             left: b.left - p.left + state.mdoc.scrollLeft,
             width: b.width, height: b.height };
  }

  // The column a row's own text starts at, counted in characters of the line
  // itself.  Two per level, org's own arithmetic.
  function col(r) { return r.depth * 2; }

  // A RAIL PER BLOCK, drawn at the tab stop LEFT of its children — which is the
  // parent's own column — and spanning the children alone.  A top-level block
  // has nothing to its left and so draws nothing, exactly as Sublime does;
  // `{outer: true}' hands it the pane's left padding and draws it anyway.
  function rails(opts) {
    var out = [];
    var outer = !!(opts && opts.outer);
    var fromOwn = !!(opts && opts.fromOwn);
    function walk(r) {
      var c = r.kids.length ? col(r.kids[0]) - 2 : null;
      if (c !== null && (c >= 0 || outer)) {
        // `fromOwn' hangs the rail off the line that OWNS it, so a row with
        // something inside it is the only kind of row a rail leaves.
        var first = px(fromOwn ? (r.el.querySelector(".dp") || r.el) : r.kids[0].el);
        var last = px(r.kids[r.kids.length - 1].el);
        out.push({
          owner: r,
          col: c,
          depth: r.kids[0].depth,
          x: state.textLeft + c * state.ch,
          top: first.top,
          height: last.top + last.height - first.top,
        });
      }
      r.kids.forEach(walk);
    }
    state.roots.forEach(walk);
    if (outer && state.roots.length) {
      // THE DOCUMENT IS A BLOCK TOO, and its rail runs the whole pane: without it
      // the outermost column is drawn beside the list and nowhere else, so it
      // breaks at every paragraph.  Owned by nothing, so it is never the block
      // point is in; first in the list, so the list's own rail sits over it.
      // THE HEADLINE IS THE DOCUMENT'S OWN LINE, so the rail starts under it --
      // that column belongs to the stars, and a rail through them is a collision.
      var body = state.roots.filter(function (r) {
        return !r.el.classList.contains("d-head");
      });
      if (!body.length) return out;
      var top = px(body[0].el);
      var end = px(body[body.length - 1].el);
      out.unshift({
        owner: null, col: -2, depth: 0,
        x: state.textLeft - 2 * state.ch,
        top: top.top, height: end.top + end.height - top.top,
      });
    }
    return out;
  }

  function measure() {
    var probe = document.createElement("span");
    probe.style.cssText = "position:absolute;visibility:hidden;white-space:pre";
    probe.textContent = new Array(101).join("0");
    state.mdoc.appendChild(probe);
    state.ch = probe.getBoundingClientRect().width / 100;
    probe.remove();
    // Every row shares one left edge; the list's own line is where the text of
    // depth 0 begins, and every rail is counted from there.
    var own = state.mdoc.querySelector(".d-list .dp");
    state.textLeft = own ? px(own).left : 0;
    state.lh = parseFloat(getComputedStyle(state.mdoc).lineHeight) || state.ch * 2;
  }

  function repaint() {
    measure();
    var c = chain();
    var truth = document.getElementById("truth");
    if (truth) {
      truth.textContent = state.at.el.classList.contains("d-comp")
        ? "the list, whole · one stop"
        : (state.at.el.classList.contains("d-item")
            ? "depth " + state.at.depth + " · inside “"
              + label(c[c.length - 2]) + "”"
            : "depth — · paragraph");
    }
    if (state.paint) state.paint({ at: state.at, chain: c, rails: rails,
                                   px: px, ch: state.ch, lh: state.lh,
                                   left: state.textLeft, mdoc: state.mdoc });
  }

  function own(r) {
    var el = r.el.querySelector(".dp") || r.el;
    return el.textContent.replace(/^\s*-\s*/, "").trim().slice(0, 28);
  }

  // A composite has no own line of its own to quote, so it answers by its name.
  function label(r) {
    return r.el.classList.contains("d-comp") ? "the list" : own(r);
  }

  // ------------------------------------------------------------------ mount
  // Three dialects for two axes: `n/p' walk siblings, `f/b' change grain, and
  // the arrows alias both the way the shipped keymap does.
  var NEXT = { n: 1, j: 1, ArrowDown: 1 }, PREV = { p: 1, k: 1, ArrowUp: 1 };
  var IN = { f: 1, l: 1, ArrowRight: 1 }, OUT = { b: 1, h: 1, ArrowLeft: 1 };

  function keys(e) {
    var k = e.key;
    if (e.ctrlKey || e.altKey || e.metaKey) return;
    if (NEXT[k]) step(1);
    else if (PREV[k]) step(-1);
    else if (IN[k]) finer();
    else if (OUT[k]) broader();
    else if (k === "t") theme();
    else return;
    e.preventDefault();
  }

  function theme() {
    var now = document.documentElement.dataset.theme === "dark" ? "light" : "dark";
    document.documentElement.dataset.theme = now;
    repaint();
  }

  function mount(opts) {
    state.mdoc = document.getElementById("mdoc");
    state.paint = opts && opts.paint;
    state.roots = build(state.mdoc);
    if (opts && opts.setup) opts.setup(state.mdoc);
    document.addEventListener("keydown", keys);
    addEventListener("resize", repaint);
    // Deep enough on load that the interesting question — depth 1 or depth 2 —
    // is already on the screen.
    go(state.roots[2].kids[0].kids[0].kids[0]);
    return state;
  }

  return { mount: mount, repaint: repaint, theme: theme,
           at: function () { return state.at; }, chain: chain, rails: rails };
})();
