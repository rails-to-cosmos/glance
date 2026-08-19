// The stage every variant is judged on: ONE fixture, ONE keymap, ONE set of
// metrics.  A variant brings its own CSS and one `paint' hook and nothing else,
// so what differs between two tabs is the look and never the rig.
//
// A CLASSIC SCRIPT ON PURPOSE: a module over `file://' is an opaque origin and
// will not load, and every page here must open by double-clicking it.
"use strict";

var RIG = (function () {
  // ---------------------------------------------------------------- fixture
  // The shipped shelf: a row at depth d puts its star at column 2d and its
  // contents at column d+2 -- the two step at different rates and CROSS at
  // depth 3, which the great-grandchild is here to show.
  // A node is {kind, text, kids, state, tags}; kinds: head, child, para,
  // drawer, list (kids are [text, kids, mark] items).
  var DOC = {
    kind: "head", text: "The shelf, six ways", state: "TODO", tags: ":spike:",
    kids: [
      { kind: "para", text: "A paragraph the root owns; every bar is judged beside it." },
      { kind: "drawer", text: ":PROPERTIES:" },
      { kind: "list", items: [
        ["a list the root owns", [
          ["with a nested item", []],
        ]],
        ["and a second entry", []],
      ] },
      { kind: "child", text: "A child whose body is drawn in place", state: "TODO", tags: ":kid:",
        kids: [
          { kind: "para", text: "A paragraph the child owns." },
          { kind: "list", items: [
            ["a list the child owns", []],
            ["with a second item", []],
          ] },
          { kind: "child", text: "A grandchild, one level further", state: "DONE",
            kids: [
              { kind: "para", text: "The grandchild's own line." },
              { kind: "child", text: "A great-grandchild, where the shelf meets the stars",
                kids: [
                  { kind: "para", text: "Its paragraph lands LEFT of its own star." },
                ] },
            ] },
        ] },
      { kind: "child", text: "A second child, so the shelf has a walk",
        kids: [
          { kind: "para", text: "Its paragraph." },
        ] },
    ],
  };

  // ------------------------------------------------------------------ model
  // A ROW IS A STOP.  `kids' is what `f' descends into, `up' what `b' climbs
  // to, `depth' the shelf (0 at the root's own contents).  A headline row
  // additionally folds: `shut' hides its subtree whole.
  function row(el, up, depth, kind) {
    return { el: el, up: up, kids: [], depth: depth, kind: kind, shut: false };
  }

  var state = { at: null, root: null, mdoc: null, paint: null,
                foldText: function () { return " …"; },
                layer: null, ch: 8, lh: 16, textLeft: 0 };

  // The shipped columns, in characters of the pane's own text edge.
  function starCol(depth) { return 2 * depth; }
  function bodyCol(depth) { return depth + 2; }
  function railCol(depth) { return depth + 0.5; }

  function rowDiv(cls, depth) {
    var d = document.createElement("div");
    d.className = "de " + cls;
    d.dataset.depth = String(depth);
    return d;
  }

  // A headline: cleaned stars in `.ds' (every star but the last a space), the
  // keyword and title as cells, tags at the far edge -- the pane's own shape.
  function headEl(node, depth, top) {
    var d = rowDiv(top ? "d-head" : "d-child", depth);
    var ds = document.createElement("span");
    ds.className = "ds";
    ds.textContent = new Array(starCol(depth) + 1).join(" ") + "* ";
    d.appendChild(ds);
    if (node.state) {
      var st = document.createElement("span");
      st.className = "dc dc-state" + (node.state === "DONE" ? " done" : "");
      st.textContent = node.state;
      d.appendChild(st);
    }
    var t = document.createElement("span");
    t.className = "dc dc-title";
    t.textContent = node.text;
    d.appendChild(t);
    if (node.tags) {
      var tg = document.createElement("span");
      tg.className = "dc dc-tags";
      tg.textContent = node.tags;
      d.appendChild(tg);
    }
    return d;
  }

  function paraEl(node, depth) {
    var d = rowDiv("d-para lvl-top", depth);
    d.style.cssText = "--g-doc-indent:" + bodyCol(depth)
      + ";--rail:calc(var(--g-doc-pad) + " + railCol(depth) + "ch)";
    d.textContent = node.text;
    return d;
  }

  // The folded drawer frame: the token on its letter, colons dimmed, org's
  // own ellipsis -- and the paragraph's bar, so the rail does not break here.
  function drawerEl(node, depth) {
    var d = rowDiv("d-meta d-drawer lvl-top", depth);
    d.style.cssText = "--g-doc-indent:" + bodyCol(depth)
      + ";--rail:calc(var(--g-doc-pad) + " + railCol(depth) + "ch)";
    var g = document.createElement("span");
    g.className = "dg";
    var c1 = document.createElement("span");
    c1.className = "dpunc";
    c1.textContent = ":";
    var word = document.createTextNode(node.text.replace(/:/g, ""));
    var c2 = document.createElement("span");
    c2.className = "dpunc";
    c2.textContent = ":";
    g.appendChild(c1); g.appendChild(word); g.appendChild(c2);
    d.appendChild(g);
    var dots = document.createElement("span");
    dots.className = "dg";
    dots.textContent = " …";
    d.appendChild(dots);
    return d;
  }

  function itemEl(text, idepth, depth, kin) {
    var d = rowDiv("d-item" + (kin ? " kin" : ""), depth);
    d.style.cssText = "--rail:calc(" + (2 * idepth) + "ch - 1.5ch)";
    var own = document.createElement("div");
    own.className = "dp";
    own.appendChild(document.createTextNode(new Array(2 * (idepth - 1) + 1).join(" ")));
    var dm = document.createElement("span");
    dm.className = "dm";
    dm.textContent = "- ";
    own.appendChild(dm);
    own.appendChild(document.createTextNode(text));
    d.appendChild(own);
    return d;
  }

  function buildItems(items, host, up, idepth, depth) {
    return items.map(function (it, i) {
      var el = itemEl(it[0], idepth, depth, i < items.length - 1);
      host.appendChild(el);
      var r = row(el, up, depth, "item");
      r.kids = buildItems(it[1], el, r, idepth + 1, depth);
      return r;
    });
  }

  function listEl(depth) {
    var d = rowDiv("d-comp d-list lvl-top", depth);
    d.style.cssText = "--g-doc-indent:" + bodyCol(depth)
      + ";--rail:calc(var(--g-doc-pad) + " + railCol(depth) + "ch)";
    return d;
  }

  // The rows are FLAT in the pane, the way the shipped view draws them: a
  // child's contents are siblings in the DOM and the nesting is in the model.
  function buildInto(node, host, up, depth, top) {
    var el, r;
    if (node.kind === "head" || node.kind === "child") {
      // A HEADLINE'S DEPTH IS ITS CONTENTS' SHELF: the star at column 2d, the
      // rows under it at d+2 -- the shipped columns.
      var d2 = top ? depth : depth + 1;
      el = headEl(node, d2, top);
      host.appendChild(el);
      r = row(el, up, d2, node.kind);
      r.kids = (node.kids || []).map(function (k) {
        return buildInto(k, host, r, d2, false);
      });
    } else if (node.kind === "para") {
      el = paraEl(node, depth);
      host.appendChild(el);
      r = row(el, up, depth, "para");
    } else if (node.kind === "drawer") {
      el = drawerEl(node, depth);
      host.appendChild(el);
      r = row(el, up, depth, "drawer");
    } else {
      el = listEl(depth);
      host.appendChild(el);
      r = row(el, up, depth, "list");
      r.kids = buildItems(node.items, el, r, 1, depth);
    }
    return r;
  }

  // ----------------------------------------------------------------- moving
  function siblings(r) { return r.up ? r.up.kids : [r]; }

  function step(d) {
    var sib = siblings(state.at);
    var i = sib.indexOf(state.at) + d;
    if (i >= 0 && i < sib.length) go(sib[i]);
  }

  function finer() {
    var r = state.at;
    if (!r.kids.length) return;
    if (r.shut) fold(r);
    go(r.kids[0]);
  }

  function broader() { if (state.at.up) go(state.at.up); }

  function go(r) {
    if (state.at) state.at.el.classList.remove("dat");
    state.at = r;
    r.el.classList.add("dat");
    r.el.scrollIntoView({ block: "nearest" });
    repaint();
  }

  // ---------------------------------------------------------------- folding
  // TAB hides the subtree WHOLE and the folded line wears org's ellipsis.
  function descendants(r) {
    return r.kids.reduce(function (acc, k) {
      return acc.concat(k, descendants(k));
    }, []);
  }

  function fold(r) {
    if (!r.kids.length || (r.kind !== "child" && r.kind !== "head")) return;
    r.shut = !r.shut;
    descendants(r).forEach(function (k) {
      k.el.style.display = r.shut ? "none" : "";
      // An open inner fold stays folded when its owner reopens.
      if (!r.shut) {
        for (var up = k.up; up !== r; up = up.up) {
          if (up.shut) { k.el.style.display = "none"; break; }
        }
      }
    });
    var dots = r.el.querySelector(".dfold");
    if (dots) dots.remove();
    // THE FOLDED LINE'S TEXT MARK IS THE VARIANT'S: `foldText' returns what
    // rides the title (" …" by default), or null for none.
    var word = r.shut ? state.foldText(r) : null;
    if (word !== null && word !== undefined) {
      dots = document.createElement("span");
      dots.className = "dg dfold";
      dots.textContent = word;
      r.el.appendChild(dots);
    }
    repaint();
  }

  // ---------------------------------------------------- what a variant reads
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

  function visible(r) { return r.el.style.display !== "none"; }

  // A SPAN PER HEADLINE: where its block starts and stops on the screen, at
  // the star's own column.  A folded block spans nothing.
  function spans() {
    var out = [];
    function walk(r) {
      if ((r.kind === "head" || r.kind === "child") && r.kids.length
          && !r.shut && visible(r)) {
        var shown = descendants(r).filter(visible);
        if (shown.length) {
          var first = px(shown[0].el);
          var lastEl = shown[shown.length - 1].el;
          var last = px(lastEl);
          out.push({
            owner: r, depth: r.depth,
            // AT THE RAIL, NOT UNDER THE STAR: the shipped shelf steps 1ch a
            // level while the stars step 2, so from shelf 2 the star's column
            // IS the text's -- the rail (d + 0.5) is the column that never
            // collides, and it is where the shipped bars already stand.
            x: state.textLeft + railCol(r.depth) * state.ch,
            headTop: px(r.el).top, headBottom: px(r.el).top + px(r.el).height,
            top: first.top, height: last.top + last.height - first.top,
          });
        }
      }
      r.kids.forEach(walk);
    }
    walk(state.root);
    return out;
  }

  function railX(depth) {
    return state.textLeft + railCol(depth) * state.ch;
  }

  // THE SHIPPED LOOK IS THE STAGE: one spine per open block at its rail, inked
  // on F's ramp -- what differs between tabs is the FOLD MARK alone.
  function inkOf(owner) {
    if (owner.el.classList.contains("dfl")) return "var(--g-bad)";
    var heads = chain().filter(function (r) {
      return (r.kind === "head" || r.kind === "child") && r.kids.length;
    });
    var rank = heads.indexOf(owner);
    if (rank < 0) return "var(--g-point-off)";
    if (rank === heads.length - 1) return "var(--g-fg)";
    var near = Math.max(25, 100 - (heads.length - 1 - rank) * 33);
    return "color-mix(in srgb, var(--g-accent) " + near + "%, var(--g-bg))";
  }

  function paintSpines(s) {
    if (!state.layer) {
      state.layer = document.createElement("div");
      state.layer.id = "rails";
      state.layer.setAttribute("aria-hidden", "true");
      state.mdoc.appendChild(state.layer);
    }
    state.layer.textContent = "";
    s.spans().forEach(function (sp) {
      var i = document.createElement("i");
      i.className = "spine";
      i.style.cssText = "left:" + railX(sp.depth) + "px;top:" + sp.top
        + "px;height:" + sp.height + "px;--bar:" + inkOf(sp.owner);
      state.layer.appendChild(i);
    });
  }

  function heads(pred) {
    var out = [];
    function walk(r) {
      if ((r.kind === "head" || r.kind === "child") && r.kids.length
          && visible(r) && pred(r)) out.push(r);
      r.kids.forEach(walk);
    }
    walk(state.root);
    return out;
  }

  function foldedHeads() { return heads(function (r) { return r.shut; }); }
  function foldableHeads() { return heads(function () { return true; }); }

  function measure() {
    var probe = document.createElement("span");
    probe.style.cssText = "position:absolute;visibility:hidden;white-space:pre";
    probe.textContent = new Array(101).join("0");
    state.mdoc.appendChild(probe);
    state.ch = probe.getBoundingClientRect().width / 100;
    probe.remove();
    var pad = getComputedStyle(state.mdoc);
    state.textLeft = state.mdoc.querySelector(".d-head")
      ? px(state.mdoc.querySelector(".d-head")).left
        + parseFloat(getComputedStyle(state.mdoc.querySelector(".d-head")).paddingLeft)
      : parseFloat(pad.paddingLeft);
    state.lh = parseFloat(pad.lineHeight) || state.ch * 2;
  }

  function label(r) {
    if (r.kind === "list") return "the list";
    if (r.kind === "drawer") return "properties";
    var t = r.el.querySelector(".dc-title");
    return (t ? t.textContent : r.el.textContent).trim().slice(0, 26);
  }

  function repaint() {
    measure();
    var c = chain();
    // The shipped tiers, kept fresh: `up' lights point's owners, `sib' the
    // branches the reader is choosing between.
    state.mdoc.querySelectorAll(".up,.sib,.car").forEach(function (el) {
      el.classList.remove("up", "sib", "car");
    });
    c.slice(0, -1).forEach(function (r) { r.el.classList.add("up"); });
    // WHAT POINT CARRIES IS POINT'S: rows are flat, so the nesting selector
    // the shipped pane uses is spelled as a class here.
    descendants(state.at).forEach(function (k) { k.el.classList.add("car"); });
    // A SIBLING'S OWN SUBTREE COMES WITH IT: rows are flat in the DOM, so the
    // descendants are marked one by one.
    siblings(state.at).forEach(function (r) {
      if (r === state.at) return;
      r.el.classList.add("sib");
      descendants(r).forEach(function (k) { k.el.classList.add("sib"); });
    });
    var truth = document.getElementById("truth");
    if (truth) {
      truth.textContent = "shelf " + state.at.depth + " · "
        + (state.at.kind === "child" || state.at.kind === "head"
            ? "headline" + (state.at.shut ? ", folded" : "")
            : "inside “" + label(c[c.length - 2]) + "”");
    }
    var ctx = {
      at: state.at, chain: c, spans: spans, px: px,
      ch: state.ch, lh: state.lh, left: state.textLeft,
      mdoc: state.mdoc, starCol: starCol, bodyCol: bodyCol, railCol: railCol,
      root: state.root, visible: visible, flagged: flagged(),
      foldedHeads: foldedHeads(), foldableHeads: foldableHeads(),
      railX: railX, inkOf: inkOf,
    };
    paintSpines(ctx);
    if (state.paint) state.paint(ctx);
  }

  // ------------------------------------------------------------------ mount
  var NEXT = { n: 1, j: 1, ArrowDown: 1 }, PREV = { p: 1, k: 1, ArrowUp: 1 };
  var IN = { f: 1, l: 1, ArrowRight: 1 }, OUT = { b: 1, h: 1, ArrowLeft: 1 };

  // A FLAG TAKES THE BRANCH: rows are flat, so the subtree is marked with it.
  function flag() {
    var on = state.at.el.classList.toggle("dfl");
    descendants(state.at).forEach(function (k) {
      k.el.classList.toggle("dfl", on);
    });
    repaint();
  }

  function flagged() {
    return [state.root].concat(descendants(state.root))
      .filter(function (r) { return r.el.classList.contains("dfl"); });
  }

  function keys(e) {
    var k = e.key;
    if (e.ctrlKey || e.altKey || e.metaKey) return;
    if (NEXT[k]) step(1);
    else if (PREV[k]) step(-1);
    else if (IN[k]) finer();
    else if (OUT[k]) broader();
    else if (k === "Tab") fold(state.at);
    else if (k === "t") theme();
    else if (k === "d") flag();
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
    if (opts && opts.foldText) state.foldText = opts.foldText;
    state.root = buildInto(DOC, state.mdoc, null, 0, true);
    if (opts && opts.setup) opts.setup(state.mdoc);
    document.addEventListener("keydown", keys);
    addEventListener("resize", repaint);
    // The grandchild's paragraph on load: three shelves already on the screen.
    go(state.root.kids[3].kids[2].kids[0]);
    return state;
  }

  return { mount: mount, repaint: repaint, theme: theme, fold: fold,
           root: function () { return state.root; },
           at: function () { return state.at; }, chain: chain, spans: spans };
})();
